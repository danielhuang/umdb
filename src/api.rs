use std::collections::BTreeMap;

use cached::proc_macro::cached;
use eyre::{eyre, Result};
use futures::{
    future::{BoxFuture, Shared},
    FutureExt, TryFutureExt,
};
use itertools::Itertools;
use scraper::{Html, Selector};
use serde::{Deserialize, Serialize};

use crate::{CLIENT, TERM};

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq, Hash, Debug, Default, PartialOrd, Ord)]
pub struct TimeRange {
    pub start_time: i32,
    pub end_time: i32,
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq, Hash, Debug, PartialOrd, Ord)]
pub enum Weekday {
    Monday,
    Tuesday,
    Wednesday,
    Thursday,
    Friday,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash, PartialOrd, Ord)]
pub struct Timeslot {
    pub location: String,
    pub days: Vec<Weekday>,
    pub time_range: TimeRange,
    pub discussion: bool,
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq, Hash, Debug, PartialOrd, Ord)]
pub struct CourseInfo {
    pub id: String,
}

impl CourseInfo {
    pub async fn with_detail(&self) -> Result<DetailedCourseInfo> {
        let major: String = self.id.chars().take_while(|x| x.is_alphabetic()).collect();
        // dbg!(&major);
        let courses = courses(major).await?;
        // dbg!(&courses);
        courses
            .get(&self.id)
            .cloned()
            .ok_or_else(|| eyre!("course does not exist"))
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq, Hash, Debug, PartialOrd, Ord)]
pub struct DetailedCourseInfo {
    pub course: CourseInfo,
    pub title: String,
    pub credits: usize,
    pub desc: Option<String>,
    pub geneds: Vec<String>,
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq, Hash, Debug, PartialOrd, Ord)]
pub struct SectionInfo {
    pub section_id: String,
    pub prof: String,
    pub total_seats: i32,
    pub open_seats: i32,
    pub waitlist_seats: i32,
    pub timeslots: Vec<Timeslot>,
}

#[cached(time = 3600, result)]
pub async fn available_majors() -> Result<BTreeMap<String, String>> {
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

#[cached(time = 3600, result)]
pub async fn courses(major: String) -> Result<BTreeMap<String, DetailedCourseInfo>> {
    let html = CLIENT
        .get(format!("https://app.testudo.umd.edu/soc/{TERM}/{}", major))
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

        dbg!(&id);

        let title = row
            .select(&Selector::parse(".course-title").unwrap())
            .next()?
            .text()
            .next()?
            .to_string();

        dbg!(&title);

        let desc = row
            .select(&Selector::parse(".course-text, .approved-course-text").unwrap())
            .last()
            .and_then(|x| x.text().next())
            .map(|x| x.to_string());

        dbg!(&desc);

        let credits = row
            .select(&Selector::parse(".course-min-credits").unwrap())
            .next()?
            .text()
            .next()?
            .parse()
            .ok()?;

        dbg!(&credits);

        let geneds = row
            .select(&Selector::parse(".course-subcategory").unwrap())
            .map(|x| x.text().collect::<String>().trim().to_string())
            .collect();

        dbg!(&geneds);

        Some((
            id.clone(),
            DetailedCourseInfo {
                course: CourseInfo { id },
                title,
                credits,
                desc,
                geneds,
            },
        ))
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

#[cached(time = 180, result)]
pub async fn sections(id: String) -> Result<Vec<SectionInfo>> {
    dbg!(&id);

    let html = CLIENT
        .get(format!("https://app.testudo.umd.edu/soc/{TERM}/sections"))
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
                    days: {
                        let mut v = vec![];

                        if days.contains('M') {
                            v.push(Weekday::Monday)
                        }

                        if days.contains("Tu") {
                            v.push(Weekday::Tuesday)
                        }

                        if days.contains('W') {
                            v.push(Weekday::Wednesday)
                        }

                        if days.contains("Th") {
                            v.push(Weekday::Thursday)
                        }

                        if days.contains('F') {
                            v.push(Weekday::Friday)
                        }

                        v
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
            section_id: id,
            prof,
            total_seats,
            open_seats,
            waitlist_seats,
            timeslots,
        })
    });

    Ok(dbg!(r.collect()))
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq, Debug, Hash)]
pub struct GradesEntry {
    pub course: String,
    pub professor: Option<String>,
    pub semester: String,
    pub section: String,
    #[serde(flatten)]
    pub grades: BTreeMap<String, i32>,
}

pub fn add_grades(a: &BTreeMap<String, i32>, b: &BTreeMap<String, i32>) -> BTreeMap<String, i32> {
    let mut new: BTreeMap<_, _> = a
        .keys()
        .chain(b.keys())
        .map(|x| (x.to_string(), 0))
        .collect();
    for (k, v) in a.iter().chain(b.iter()) {
        let prev = new[k];
        new.insert(k.to_string(), prev + v);
    }
    new
}

#[cached(time = 1800)]
pub fn get_grades(course: String) -> Shared<BoxFuture<'static, Result<Vec<GradesEntry>, String>>> {
    async fn inner(course: String) -> Result<Vec<GradesEntry>> {
        println!("loading planetterp {}", course);
        let grades = CLIENT
            .get("https://planetterp.com/api/v1/grades")
            .query(&[("course", &course)])
            .send()
            .await?
            .json()
            .await?;
        println!("loaded planetterp {}", course);
        Ok(grades)
    }

    inner(course).map_err(|e| e.to_string()).boxed().shared()
}
