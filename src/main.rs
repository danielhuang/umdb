use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    net::SocketAddr,
    time::Instant,
};

use crate::api::{
    available_majors, courses, sections, CourseInfo, SectionInfo, TimeRange, Timeslot,
};
use api::{add_grades, get_grades, DetailedCourseInfo, GradesEntry};
use axum::{
    extract::Path,
    routing::{get, post},
    Json, Router,
};
use eyre::Result;
use futures::future::try_join_all;
use itertools::Itertools;
use once_cell::sync::Lazy;
use rand::{rngs::StdRng, seq::SliceRandom, thread_rng, SeedableRng};
use reqwest::{
    header::{ACCEPT, AUTHORIZATION, CONTENT_TYPE, REFERER},
    Client, StatusCode,
};
use serde::{Deserialize, Serialize};
use thiserror::Error;
use tower_http::cors::CorsLayer;

mod api;

pub static CLIENT: Lazy<Client> = Lazy::new(Client::new);

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct CourseId {
    pub major: String,
    pub ident: String,
}

#[derive(Debug, Clone)]
struct Plan {
    required_courses: BTreeSet<CourseInfo>,
    available_courses: Vec<CourseInfo>,
    available_sections: BTreeMap<CourseInfo, Vec<SectionInfo>>,
    avoid_instructors: BTreeSet<String>,
    avoid_time_ranges: Vec<TimeRange>,
    seats_required: i32,
    min_credits: i32,
}

fn timeslot_conflict(a: &Timeslot, b: &Timeslot) -> bool {
    a.days.iter().any(|x| b.days.contains(x)) && timerange_conflict(&a.time_range, &b.time_range)
}

fn timerange_conflict(a: &TimeRange, b: &TimeRange) -> bool {
    a.start_time < b.end_time && b.start_time < a.end_time
}

fn section_conflict(found: impl Iterator<Item = SectionInfo>, b: &SectionInfo) -> bool {
    for s in found {
        for slot_a in s.timeslots {
            for slot_b in b.timeslots.clone() {
                if timeslot_conflict(&slot_a, &slot_b) {
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
struct Unsolvable;

fn solve(
    plan: &mut Plan,
    found: &mut BTreeMap<CourseInfo, SectionInfo>,
    seed: usize,
) -> Result<(), Unsolvable> {
    if (found.iter().map(|x| x.0.credits).sum::<i32>() >= plan.min_credits)
        && plan.required_courses.iter().all(|x| found.contains_key(x))
    {
        return Ok(());
    }

    let mut rng = StdRng::from_seed(seed.to_be_bytes().repeat(4).try_into().unwrap());

    for available_course in plan
        .available_courses
        .clone()
        .into_iter()
        .filter(|x| !found.contains_key(x))
        .sorted_by_key(|x| !plan.required_courses.contains(x))
    {
        let mut available = plan.available_sections[&available_course].clone();
        available.shuffle(&mut rng);

        for section in available {
            if !section_conflict(found.values().cloned(), &section)
                && !plan.avoid_instructors.contains(&section.prof)
                && !section_must_avoid(&section, plan.avoid_time_ranges.iter().cloned())
                && (section.open_seats - section.waitlist_seats) >= plan.seats_required
            {
                found.insert(available_course.clone(), section);

                if let Ok(()) = solve(plan, found, seed) {
                    return Ok(());
                }

                found.remove(&available_course);
            }
        }
    }

    Err(Unsolvable)
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq, Debug)]
pub struct ScheduleRequest {
    required_courses: BTreeSet<CourseInfo>,
    optional_courses: Vec<CourseInfo>,
    avoid_instructors: BTreeSet<String>,
    avoid_time_ranges: Vec<TimeRange>,
    #[serde(default)]
    seats_required: i32,
    #[serde(default)]
    min_credits: i32,
}

fn solve_plan(
    mut plan: Plan,
    seed: usize,
) -> Result<BTreeMap<CourseInfo, SectionInfo>, StatusCode> {
    let start = Instant::now();
    let mut found = BTreeMap::new();
    solve(&mut plan, &mut found, seed).map_err(|e| {
        dbg!(&e);
        StatusCode::INTERNAL_SERVER_ERROR
    })?;
    Ok(found.into_iter().collect())
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq, Hash, Debug, PartialOrd, Ord)]
pub struct ScheduleEntry {
    #[serde(flatten)]
    pub section_info: SectionInfo,
    pub grades: BTreeMap<String, i32>,
}

async fn build_schedules(
    Json(req): Json<ScheduleRequest>,
) -> Result<Json<BTreeSet<BTreeMap<String, ScheduleEntry>>>, StatusCode> {
    let ScheduleRequest {
        required_courses,
        avoid_instructors,
        avoid_time_ranges,
        seats_required,
        optional_courses,
        min_credits,
    } = req;

    let mut available_sections = BTreeMap::new();
    let mut grades = BTreeMap::new();

    for (course, s, g) in try_join_all(required_courses.iter().chain(optional_courses.iter()).map(
        |course| async move {
            let s = sections(&course.title)
                .await
                .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

            let g = get_grades(course.title.to_string()).await.map_err(|e| {
                dbg!(&e);
                StatusCode::INTERNAL_SERVER_ERROR
            })?;

            Ok((course.clone(), s, g))
                as Result<(CourseInfo, Vec<SectionInfo>, Vec<GradesEntry>), StatusCode>
        },
    ))
    .await?
    {
        available_sections.insert(course.clone(), s);
        grades.insert(course.clone(), g);
    }

    let plan = Plan {
        available_courses: required_courses
            .iter()
            .cloned()
            .chain(optional_courses.iter().cloned())
            .collect(),
        required_courses,
        available_sections,
        avoid_instructors,
        avoid_time_ranges,
        seats_required,
        min_credits,
    };

    let mut all = vec![];
    for seed in 0..100 {
        let found = solve_plan(plan.clone(), seed)?;
        let post = Instant::now();
        all.push(
            found
                .into_iter()
                .map(|x| {
                    let section_info = x.1;
                    (
                        x.0.title.to_string(),
                        ScheduleEntry {
                            grades: grades[&x.0]
                                .iter()
                                .filter(|x| x.professor == Some(section_info.prof.to_string()))
                                .map(|x| x.grades.clone())
                                .fold(BTreeMap::new(), |a, b| add_grades(&a, &b)),
                            section_info,
                        },
                    )
                })
                .collect(),
        );
        dbg!(&post.elapsed());
    }

    Ok(Json(all.into_iter().collect()))
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
    Ok(Json(courses(&major).await.map_err(|e| {
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
