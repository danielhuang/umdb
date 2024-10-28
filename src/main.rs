use std::{
    collections::{BTreeMap, BTreeSet, HashSet},
    net::SocketAddr,
    time::{Duration, Instant},
};

use crate::api::{
    available_majors, courses, sections, CourseInfo, SectionInfo, TimeRange, Timeslot,
};
use api::{add_grades, get_grades, DetailedCourseInfo, GradesEntry, Weekday};
use axum::{
    extract::{Path, Query},
    routing::{get, post},
    Json, Router,
};
use eyre::Result;
use futures::future::try_join_all;
use itertools::Itertools;
use multimap::MultiMap;
use once_cell::sync::Lazy;
use ordered_float::OrderedFloat;
use rand::{rngs::StdRng, seq::SliceRandom, SeedableRng};
use reqwest::{
    header::{ACCEPT, AUTHORIZATION, CONTENT_TYPE, REFERER},
    Client, StatusCode,
};
use rustc_hash::FxHashSet;
use serde::{Deserialize, Serialize};
use thiserror::Error;
use tokio::task::block_in_place;
use tower_http::cors::CorsLayer;

mod api;

pub const TERM: &str = "202501";

pub static CLIENT: Lazy<Client> = Lazy::new(Client::new);

use tikv_jemallocator::Jemalloc;
#[global_allocator]
static GLOBAL: Jemalloc = Jemalloc;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct CourseId {
    pub major: String,
    pub ident: String,
}

#[derive(Debug, Clone)]
struct Plan {
    required_course_groups: Vec<CourseGroup>,
    course_to_group_map: MultiMap<CourseInfo, usize>,
    available_sections: BTreeMap<CourseInfo, Vec<SectionInfo>>,
    avoid_instructors: HashSet<String>,
    avoid_time_ranges: Vec<TimeRange>,
    avoid_weekdays: HashSet<Weekday>,
    seats_required: i32,
    min_course_count: i32,
    optional_courses: BTreeSet<CourseInfo>,
}

fn timeslot_conflict(a: &Timeslot, b: &Timeslot) -> bool {
    a.days.iter().any(|x| b.days.contains(x)) && timerange_conflict(&a.time_range, &b.time_range)
}

fn timerange_conflict(a: &TimeRange, b: &TimeRange) -> bool {
    a.start_time < b.end_time && b.start_time < a.end_time
}

fn section_conflict<'a>(
    found: impl Iterator<Item = &'a SectionInfo> + 'a,
    b: &SectionInfo,
) -> bool {
    for s in found {
        for slot_a in &s.timeslots {
            for slot_b in &b.timeslots {
                if timeslot_conflict(slot_a, slot_b) {
                    return true;
                }
            }
        }
    }
    false
}

fn section_must_avoid(x: &SectionInfo, avoid: impl Iterator<Item = TimeRange>) -> bool {
    for r in avoid {
        for s in x.timeslots.iter() {
            if timerange_conflict(&r, &s.time_range) {
                return true;
            }
        }
    }
    false
}

#[derive(Error, Debug)]
#[error("constraints not satisfiable")]
struct Unsatisfiable;

fn solve(
    plan: &mut Plan,
    selected_courses: &mut BTreeMap<CourseInfo, SectionInfo>,
    unsat: &mut FxHashSet<BTreeMap<CourseInfo, SectionInfo>>,
    group_fill_count: &mut Vec<usize>,
    seed: u64,
) -> Result<(), Unsatisfiable> {
    if (selected_courses.iter().count() as i32 >= plan.min_course_count)
        && plan
            .required_course_groups
            .iter()
            .enumerate()
            .all(|(group_i, group)| group_fill_count[group_i] >= group.choose_n)
    {
        return Ok(());
    }

    if unsat.contains(selected_courses) {
        return Err(Unsatisfiable);
    }

    let mut rng = StdRng::seed_from_u64(seed);

    for available_course in plan
        .required_course_groups
        .iter()
        .enumerate()
        .sorted_by_key(|(i, x)| {
            (
                group_fill_count[*i] >= x.choose_n,
                x.choose_n,
                x.courses.len(),
            )
        })
        .flat_map(|(_, group)| {
            let mut courses = group.courses.iter().cloned().collect_vec();
            courses.shuffle(&mut rng);
            courses
                .into_iter()
                .filter(|x| !selected_courses.contains_key(x))
                .sorted_by_key(|x| {
                    !plan
                        .required_course_groups
                        .iter()
                        .flat_map(|x| &x.courses)
                        .any(|o| o == x)
                })
                .collect_vec()
        })
        .chain(plan.optional_courses.iter().cloned())
        .collect_vec()
    {
        let mut available = plan.available_sections[&available_course].clone();
        available.shuffle(&mut rng);
        available.sort_by_key(|x| x.timeslots.len());

        for section in available {
            if !section_conflict(selected_courses.values(), &section)
                && !plan.avoid_instructors.contains(&section.prof)
                && !section_must_avoid(&section, plan.avoid_time_ranges.iter().cloned())
                && (section.open_seats - section.waitlist_seats) >= plan.seats_required
                && section
                    .timeslots
                    .iter()
                    .flat_map(|x| &x.days)
                    .all(|x| !plan.avoid_weekdays.contains(x))
            {
                selected_courses.insert(available_course.clone(), section);
                for i in plan
                    .course_to_group_map
                    .get_vec(&available_course)
                    .cloned()
                    .unwrap_or_default()
                {
                    group_fill_count[i] += 1;
                }

                if let Ok(()) = solve(plan, selected_courses, unsat, group_fill_count, seed) {
                    return Ok(());
                }

                selected_courses.remove(&available_course);
                for i in plan
                    .course_to_group_map
                    .get_vec(&available_course)
                    .cloned()
                    .unwrap_or_default()
                {
                    group_fill_count[i] -= 1;
                }
            }
        }
    }

    unsat.insert(selected_courses.clone());

    Err(Unsatisfiable)
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq, Hash, Debug, PartialOrd, Ord)]
pub struct PinnedSection {
    course: CourseInfo,
    section_id: String,
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq, Hash, Debug)]
pub struct CourseGroup {
    courses: BTreeSet<CourseInfo>,
    choose_n: usize,
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq, Debug)]
pub struct ScheduleRequest {
    required_course_groups: Vec<CourseGroup>,
    optional_courses: BTreeSet<CourseInfo>,
    avoid_instructors: HashSet<String>,
    avoid_time_ranges: Vec<TimeRange>,
    #[serde(default)]
    seats_required: i32,
    #[serde(default)]
    min_course_count: i32,
    #[serde(default)]
    pinned_sections: BTreeSet<PinnedSection>,
    #[serde(default)]
    avoid_weekdays: HashSet<Weekday>,
    #[serde(default)]
    avoid_missing_grade_data: bool,
}

fn solve_plan(
    mut plan: Plan,
    mut selected_courses: BTreeMap<CourseInfo, SectionInfo>,
    unsat: &mut FxHashSet<BTreeMap<CourseInfo, SectionInfo>>,
    seed: usize,
) -> Result<BTreeMap<CourseInfo, SectionInfo>, Unsatisfiable> {
    let mut group_fill_count = vec![0; plan.required_course_groups.len()];
    for (course, _) in selected_courses.iter() {
        for (i, group) in plan.required_course_groups.iter().enumerate() {
            if group.courses.contains(course) {
                group_fill_count[i] += 1;
            }
        }
    }
    solve(
        &mut plan,
        &mut selected_courses,
        unsat,
        &mut group_fill_count,
        seed as u64,
    )?;
    Ok(selected_courses.into_iter().collect())
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq, Hash, Debug, PartialOrd, Ord)]
pub struct CourseEntry {
    #[serde(flatten)]
    pub section_info: SectionInfo,
    pub grades: BTreeMap<String, i32>,
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Debug, Eq, PartialOrd, Ord)]
pub struct Schedule {
    courses: BTreeMap<String, CourseEntry>,
    score: OrderedFloat<f64>,
}

fn calc_avg_gpa(grades: &BTreeMap<String, i32>) -> Option<f64> {
    fn grade_to_gpa(grade: &str) -> f64 {
        match grade {
            "A+" => 4.0,
            "A" => 4.0,
            "A-" => 3.7,
            "B+" => 3.3,
            "B" => 3.0,
            "B-" => 2.7,
            "C+" => 2.3,
            "C" => 2.0,
            "C-" => 1.7,
            "D+" => 1.3,
            "D" => 1.0,
            "D-" => 0.7,
            _ => 0.0,
        }
    }
    let total = grades.values().sum::<i32>() as f64;
    let sum = grades
        .iter()
        .map(|(grade, &count)| grade_to_gpa(grade) * count as f64)
        .sum::<f64>();
    if sum == 0.0 {
        None
    } else {
        Some(sum / total)
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq, Hash, Debug)]
pub struct ScheduleOptions {
    count: Option<usize>,
}

async fn build_schedules(
    Query(options): Query<ScheduleOptions>,
    Json(req): Json<ScheduleRequest>,
) -> Result<Json<BTreeSet<Schedule>>, StatusCode> {
    let ScheduleRequest {
        avoid_instructors,
        avoid_time_ranges,
        seats_required,
        optional_courses,
        min_course_count,
        pinned_sections,
        avoid_weekdays,
        required_course_groups,
        avoid_missing_grade_data,
    } = req.clone();

    let mut available_sections = BTreeMap::new();
    let mut grades = BTreeMap::new();

    for (course, s, g) in try_join_all(
        required_course_groups
            .iter()
            .cloned()
            .flat_map(|x| x.courses)
            .chain(optional_courses.iter().cloned())
            .map(|course| async move {
                let s = sections(course.title.clone())
                    .await
                    .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

                let g = get_grades(course.title.to_string())
                    .await
                    .unwrap_or_else(|e| {
                        println!("warning: {} failed to load grades: {:?}", course.title, e);
                        vec![]
                    });

                Ok((course.clone(), s, g))
                    as Result<(CourseInfo, Vec<SectionInfo>, Vec<GradesEntry>), StatusCode>
            }),
    )
    .await?
    {
        available_sections.insert(course.clone(), s);
        grades.insert(course.clone(), g);
    }

    let mut already_selected = BTreeMap::new();
    for pinned_section in pinned_sections {
        let sections = available_sections
            .get(&pinned_section.course)
            .ok_or_else(|| {
                dbg!(&pinned_section);
                StatusCode::INTERNAL_SERVER_ERROR
            })?;
        let section = sections
            .iter()
            .find(|x| x.section_id == pinned_section.section_id)
            .ok_or_else(|| {
                dbg!(&pinned_section);
                StatusCode::BAD_REQUEST
            })?;
        already_selected.insert(pinned_section.course, section.clone());
    }

    let plan = Plan {
        available_sections,
        avoid_instructors,
        avoid_time_ranges,
        seats_required,
        min_course_count,
        avoid_weekdays,
        course_to_group_map: required_course_groups
            .iter()
            .enumerate()
            .flat_map(|(i, x)| x.courses.iter().map(move |c| (c.clone(), i)))
            .collect(),
        required_course_groups,
        optional_courses,
    };

    let start = Instant::now();
    let mut all = vec![];
    let mut cache = FxHashSet::default();
    for seed in 0..options.count.unwrap_or(1000) {
        let found =
            block_in_place(|| solve_plan(plan.clone(), already_selected.clone(), &mut cache, seed));

        let found = match found {
            Ok(found) => found,
            Err(Unsatisfiable) => {
                dbg!(&already_selected, start.elapsed());
                return Ok(Json(BTreeSet::new()));
            }
        };

        let courses: BTreeMap<_, _> = found
            .into_iter()
            .map(|(course, section)| {
                (
                    course.title.to_string(),
                    CourseEntry {
                        grades: grades[&course]
                            .iter()
                            .filter(|x| x.professor == Some(section.prof.to_string()))
                            .map(|x| x.grades.clone())
                            .fold(BTreeMap::new(), |a, b| add_grades(&a, &b)),
                        section_info: section,
                    },
                )
            })
            .collect();
        let grades = if avoid_missing_grade_data {
            courses
                .values()
                .map(|x| calc_avg_gpa(&x.grades).unwrap_or(0.0))
                .collect_vec()
        } else {
            courses
                .values()
                .filter_map(|x| calc_avg_gpa(&x.grades))
                .collect_vec()
        };
        let avg_gpa = grades.iter().sum::<f64>() / grades.len() as f64;
        all.push(Schedule {
            score: OrderedFloat(avg_gpa),
            courses,
        });

        if start.elapsed() > Duration::from_secs(4) {
            break;
        }

        tokio::task::yield_now().await;
    }

    let total_len = all.len();
    let all: BTreeSet<_> = all.into_iter().collect();

    println!(
        "completed search for {:?} in {:?}, found {} ({} distinct)",
        req,
        start.elapsed(),
        total_len,
        all.len(),
    );

    Ok(Json(all))
}

async fn get_available_majors() -> Result<Json<BTreeMap<String, String>>, StatusCode> {
    Ok(Json(
        available_majors()
            .await
            .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?,
    ))
}

async fn get_available_courses(
    Path(major): Path<String>,
) -> Result<Json<BTreeMap<String, DetailedCourseInfo>>, StatusCode> {
    Ok(Json(courses(major).await.map_err(|e| {
        dbg!(&e);
        StatusCode::INTERNAL_SERVER_ERROR
    })?))
}

#[tokio::main]
async fn main() -> Result<()> {
    let app = Router::new()
        .route("/", get(|| async { "UMDB" }))
        .route("/majors", get(get_available_majors))
        .route("/courses/:major", get(get_available_courses))
        .route("/build_schedules", post(build_schedules))
        .layer(CorsLayer::permissive().allow_headers(vec![
            AUTHORIZATION,
            CONTENT_TYPE,
            ACCEPT,
            REFERER,
        ]));

    let addr = SocketAddr::from(([127, 0, 0, 1], 6007));
    axum::Server::bind(&addr)
        .serve(app.into_make_service())
        .await
        .unwrap();

    Ok(())
}
