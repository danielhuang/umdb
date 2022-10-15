use std::{
    collections::{HashMap, HashSet},
    net::SocketAddr,
};

use api::{sections, SectionInfo, TimeRange, Timeslot};
use axum::{
    routing::{get, post},
    Json, Router,
};
use eyre::Result;
use once_cell::sync::Lazy;
use rand::{seq::SliceRandom, thread_rng};
use reqwest::{Client, StatusCode};
use serde::{Deserialize, Serialize};
use thiserror::Error;

mod api;

pub static CLIENT: Lazy<Client> = Lazy::new(Client::new);

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct CourseId {
    pub major: String,
    pub ident: String,
}

#[derive(Debug)]
struct Plan {
    required_courses: Vec<String>,
    available_sections: HashMap<String, Vec<SectionInfo>>,
    avoid_instructors: HashSet<String>,
    avoid_time_ranges: Vec<TimeRange>,
}

fn timeslot_conflict(a: &Timeslot, b: &Timeslot) -> bool {
    a.days == b.days && timerange_conflict(&a.time_range, &b.time_range)
}

fn timerange_conflict(a: &TimeRange, b: &TimeRange) -> bool {
    a.start_time <= b.end_time && b.start_time <= a.end_time
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

fn solve(plan: &mut Plan, found: &mut Vec<(String, SectionInfo)>) -> Result<(), Unsolvable> {
    if let Some(next) = plan.required_courses.pop() {
        let mut available = plan.available_sections[&next].clone();
        available.shuffle(&mut thread_rng());

        for section in available {
            if !section_conflict(found.iter().cloned().map(|x| x.1), &section)
                && !plan.avoid_instructors.contains(&section.prof)
                && !section_must_avoid(&section, plan.avoid_time_ranges.iter().cloned())
            {
                found.push((next.clone(), section));

                if let Ok(()) = solve(plan, found) {
                    return Ok(());
                }

                found.pop();
            }
        }

        plan.required_courses.push(next);

        Err(Unsolvable)
    } else {
        Ok(())
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq, Debug)]
pub struct ScheduleRequest {
    required_courses: Vec<String>,
    avoid_instructors: HashSet<String>,
    avoid_time_ranges: Vec<TimeRange>,
}

async fn build_schedule(
    Json(req): Json<ScheduleRequest>,
) -> Result<Json<Vec<(String, SectionInfo)>>, StatusCode> {
    let ScheduleRequest {
        required_courses,
        avoid_instructors,
        avoid_time_ranges,
    } = req;

    let mut available_sections = HashMap::new();

    for course in required_courses.iter() {
        let s = sections(course)
            .await
            .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;
        available_sections.insert(course.clone(), s);
    }

    let mut plan = Plan {
        required_courses,
        available_sections,
        avoid_instructors,
        avoid_time_ranges,
    };

    let mut found = vec![];
    solve(&mut plan, &mut found).map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

    Ok(Json(found))
}

#[tokio::main]
async fn main() -> Result<()> {
    let app = Router::new()
        .route("/", get(|| async { "UMDB" }))
        .route("/build_schedule", post(build_schedule));

    let addr = SocketAddr::from(([127, 0, 0, 1], 6007));
    axum::Server::bind(&addr)
        .serve(app.into_make_service())
        .await
        .unwrap();

    Ok(())
}
