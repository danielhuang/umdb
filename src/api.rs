use std::collections::HashMap;

use eyre::Result;
use itertools::Itertools;
use scraper::{Html, Selector};
use serde::{Deserialize, Serialize};

use crate::CLIENT;

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq, Hash, Debug, Default)]
pub struct TimeRange {
    pub start_time: i32,
    pub end_time: i32,
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq, Hash, Debug, Default)]
pub struct Weekdays {
    pub monday: bool,
    pub tuesday: bool,
    pub wednesday: bool,
    pub thursday: bool,
    pub friday: bool,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct Timeslot {
    pub location: String,
    pub days: Weekdays,
    pub time_range: TimeRange,
    pub discussion: bool,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct Instructor {
    pub lname: String,
    pub fname: String,
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq, Hash, Debug)]
pub struct CourseInfo {
    pub title: String,
    pub credits: i32,
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq, Hash, Debug)]
pub struct SectionInfo {
    pub id: String,
    pub prof: String,
    pub total_seats: i32,
    pub open_seats: i32,
    pub waitlist_seats: i32,
    pub timeslots: Vec<Timeslot>,
}

pub async fn available_majors() -> Result<HashMap<String, String>> {
    let html = CLIENT
        .get("https://app.testudo.umd.edu/soc/")
        .send()
        .await?
        .text()
        .await?;

    let document = Html::parse_document(&html);

    let row_sel = Selector::parse(".course-prefix.row").unwrap();
    let rows = document
        .select(&row_sel)
        .filter_map(|x| {
            x.text()
                .filter(|x| !x.trim().is_empty())
                .map(|x| x.to_string())
                .collect_tuple()
        })
        .collect();

    Ok(rows)
}

pub async fn courses(major: &str) -> Result<HashMap<String, CourseInfo>> {
    let html = CLIENT
        .get(format!("https://app.testudo.umd.edu/soc/202301/{}", major))
        .send()
        .await?
        .text()
        .await?;

    let document = Html::parse_document(&html);

    let row_sel = Selector::parse(".row").unwrap();
    let r = document.select(&row_sel).filter_map(|row| {
        let id = row
            .select(&Selector::parse(".course-id").unwrap())
            .next()?
            .text()
            .next()?
            .to_string();

        let title = row
            .select(&Selector::parse(".course-title").unwrap())
            .next()?
            .text()
            .next()?
            .to_string();

        let credits = row
            .select(&Selector::parse(".course-min-credits").unwrap())
            .next()?
            .text()
            .next()?
            .parse()
            .ok()?;

        Some((id, CourseInfo { title, credits }))
    });

    Ok(r.collect())
}

fn parse_time(s: &str) -> Option<i32> {
    let (hr, rest) = s.split_once(':')?;
    let hr: i32 = hr.parse().ok()?;
    let min: i32 = rest.chars().take(2).collect::<String>().parse().ok()?;
    let pm = rest.ends_with("pm");

    Some(if pm && hr < 12 {
        (hr + 12) * 60 + min
    } else {
        hr * 60 + min
    })
}

pub async fn sections(id: &str) -> Result<Vec<SectionInfo>> {
    let html = CLIENT
        .get("https://app.testudo.umd.edu/soc/202301/sections")
        .query(&[("courseIds", id)])
        .send()
        .await?
        .text()
        .await?;

    let document = Html::parse_document(&html);

    let sec_sel = Selector::parse(".section").unwrap();
    let r = document.select(&sec_sel).filter_map(|section| {
        let id = section
            .select(&Selector::parse(".section-id").unwrap())
            .next()?
            .text()
            .next()?
            .trim()
            .to_string();

        let prof = section
            .select(&Selector::parse(".section-instructor").unwrap())
            .next()?
            .text()
            .next()?
            .to_string();

        let total_seats = section
            .select(&Selector::parse(".total-seats-count").unwrap())
            .next()?
            .text()
            .next()?
            .parse()
            .ok()?;

        let open_seats = section
            .select(&Selector::parse(".open-seats-count").unwrap())
            .next()?
            .text()
            .next()?
            .parse()
            .ok()?;

        let waitlist_seats = section
            .select(&Selector::parse(".waitlist-count").unwrap())
            .next()?
            .text()
            .next()?
            .parse()
            .ok()?;

        let class_days_sel = Selector::parse(".class-days-container > .row").unwrap();
        let timeslots = section
            .select(&class_days_sel)
            .filter_map(|row| {
                let days = row
                    .select(&Selector::parse(".section-days").unwrap())
                    .next()?
                    .text()
                    .next()?
                    .to_string();

                let location = row
                    .select(&Selector::parse(".class-building").unwrap())
                    .map(|x| {
                        x.text()
                            .map(|x| x.trim())
                            .filter(|x| !x.trim().is_empty())
                            .join(" ")
                    })
                    .collect::<String>();

                let start_time = row
                    .select(&Selector::parse(".class-start-time").unwrap())
                    .next()?
                    .text()
                    .next()?
                    .to_string();

                let end_time = row
                    .select(&Selector::parse(".class-end-time").unwrap())
                    .next()?
                    .text()
                    .next()?
                    .to_string();

                let discussion = row
                    .select(&Selector::parse(".class-type").unwrap())
                    .next()
                    .is_some();

                Some(Timeslot {
                    location,
                    days: Weekdays {
                        monday: days.contains('M'),
                        tuesday: days.contains("Tu"),
                        wednesday: days.contains('W'),
                        thursday: days.contains("Th"),
                        friday: days.contains('F'),
                    },
                    time_range: TimeRange {
                        start_time: parse_time(&start_time)?,
                        end_time: parse_time(&end_time)?,
                    },
                    discussion,
                })
            })
            .collect();

        Some(SectionInfo {
            id,
            prof,
            total_seats,
            open_seats,
            waitlist_seats,
            timeslots,
        })
    });

    Ok(r.collect())
}
