use std::{
    collections::{BTreeMap, HashMap, HashSet},
    net::SocketAddr,
    time::Instant,
};

use crate::api::{
    available_majors, courses, sections, CourseInfo, SectionInfo, TimeRange, Timeslot,
};
use api::DetailedCourseInfo;
use axum::{
    extract::Path,
    routing::{get, post},
    Json, Router,
};
use eyre::Result;
use itertools::Itertools;
use once_cell::sync::Lazy;
use rand::{seq::SliceRandom, thread_rng};
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
    required_courses: HashSet<CourseInfo>,
    available_courses: Vec<CourseInfo>,
    available_sections: HashMap<CourseInfo, Vec<SectionInfo>>,
    avoid_instructors: HashSet<String>,
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

fn solve(plan: &mut Plan, found: &mut HashMap<CourseInfo, SectionInfo>) -> Result<(), Unsolvable> {
    if (found.iter().map(|x| x.0.credits).sum::<i32>() >= plan.min_credits)
        && plan.required_courses.iter().all(|x| found.contains_key(x))
    {
        return Ok(());
    }

    for available_course in plan
        .available_courses
        .clone()
        .into_iter()
        .filter(|x| !found.contains_key(x))
        .sorted_by_key(|x| !plan.required_courses.contains(x))
    {
        let mut available = plan.available_sections[&available_course].clone();
        available.shuffle(&mut thread_rng());

        for section in available {
            if !section_conflict(found.values().cloned(), &section)
                && !plan.avoid_instructors.contains(&section.prof)
                && !section_must_avoid(&section, plan.avoid_time_ranges.iter().cloned())
                && (section.open_seats - section.waitlist_seats) >= plan.seats_required
            {
                found.insert(available_course.clone(), section);

                if let Ok(()) = solve(plan, found) {
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
    required_courses: HashSet<CourseInfo>,
    optional_courses: Vec<CourseInfo>,
    avoid_instructors: HashSet<String>,
    avoid_time_ranges: Vec<TimeRange>,
    #[serde(default)]
    seats_required: i32,
    #[serde(default)]
    min_credits: i32,
}

fn solve_plan(mut plan: Plan) -> Result<HashMap<CourseInfo, SectionInfo>, StatusCode> {
    let start = Instant::now();
    let mut found = HashMap::new();
    solve(&mut plan, &mut found).map_err(|e| {
        dbg!(&e);
        dbg!(&start.elapsed());
        StatusCode::INTERNAL_SERVER_ERROR
    })?;
    dbg!(&start.elapsed());
    Ok(found.into_iter().collect())
}

async fn build_schedules(
    Json(req): Json<ScheduleRequest>,
) -> Result<Json<HashSet<BTreeMap<String, SectionInfo>>>, StatusCode> {
    let ScheduleRequest {
        required_courses,
        avoid_instructors,
        avoid_time_ranges,
        seats_required,
        optional_courses,
        min_credits,
    } = req;

    let mut available_sections = HashMap::new();

    for course in required_courses.iter().chain(optional_courses.iter()) {
        let s = sections(&course.title)
            .await
            .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;
        available_sections.insert(course.clone(), s);
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
    for _ in 0..100 {
        let found = solve_plan(plan.clone())?;
        all.push(found.into_iter().map(|x| (x.0.title, x.1)).collect());
    }

    Ok(Json(all.into_iter().collect()))
}

async fn get_available_majors() -> Result<Json<HashMap<String, String>>, StatusCode> {
    Ok(Json(
        available_majors()
            .await
            .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?,
    ))
}

async fn get_available_courses(
    Path(major): Path<String>,
) -> Result<Json<HashMap<String, DetailedCourseInfo>>, StatusCode> {
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
