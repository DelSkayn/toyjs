use std::{
    collections::HashMap,
    fs::{self, File},
    io::Write,
    path::{Path, PathBuf},
};

use anyhow::Result;
use serde::{Deserialize, Serialize};
use termcolor::{StandardStream, WriteColor};

use crate::{
    commands::{write_result, Colors},
    test::TestResult,
};

#[derive(Deserialize, Serialize)]
pub struct Report {
    tests: HashMap<PathBuf, TestResult>,
}

impl Report {
    pub fn from_results(tests: HashMap<PathBuf, TestResult>) -> Self {
        Report { tests }
    }

    pub fn write(&self) -> Result<()> {
        if Path::new("report.json").exists() {
            let mut i = 1;
            let mut cur = PathBuf::from(format!("report.{}.json", i));
            while cur.exists() {
                i += 1;
                cur = PathBuf::from(format!("report.{}.json", i));
            }
            i -= 1;
            while i > 0 {
                let new = PathBuf::from(format!("report.{}.json", i));
                fs::rename(&new, cur)?;
                cur = new;
                i -= 1;
            }
            fs::rename("report.json", "report.1.json")?;
        }
        let file = File::create("report.json")?;
        serde_json::to_writer(file, self)?;

        Ok(())
    }

    pub fn from_path(path: &str) -> Result<Self> {
        let file = File::open(path)?;
        Ok(serde_json::from_reader(file)?)
    }
    pub fn compare(&self, stdout: &mut StandardStream, c: &Colors) -> Result<()> {
        self.compare_to("report.json", stdout, c)
    }

    pub fn compare_to(&self, file: &str, stdout: &mut StandardStream, c: &Colors) -> Result<()> {
        let file = File::open(file)?;
        let mut report: Report = serde_json::from_reader(file)?;

        for (k, new) in &self.tests {
            if let Some(old) = report.tests.remove(k) {
                Self::write_compare(k, &old, new, stdout, c)?;
            } else {
                stdout.set_color(&c.failure)?;
                write!(stdout, "{:<10}", "NEW")?;
                stdout.set_color(&c.base)?;
                writeln!(stdout, "{}", k.display())?;
            }
        }

        for k in report.tests.keys() {
            stdout.set_color(&c.failure)?;
            write!(stdout, "{:<10}", "MISSING")?;
            stdout.set_color(&c.base)?;
            writeln!(stdout, "{}", k.display())?;
        }

        Ok(())
    }

    fn write_compare(
        k: &Path,
        old: &TestResult,
        new: &TestResult,
        stdout: &mut StandardStream,
        c: &Colors,
    ) -> Result<()> {
        match (new, old) {
            (TestResult::Passed, TestResult::Passed)
            | (TestResult::Error(_), TestResult::Error(_))
            | (TestResult::Panic(_), TestResult::Panic(_))
            | (TestResult::Failed(_), TestResult::Failed(_)) => {}
            (TestResult::Passed, _) => {
                stdout.set_color(&c.passed)?;
                write!(stdout, "{:<10}", "IMPROVED")?;
                stdout.set_color(&c.base)?;
                writeln!(stdout, "{}", k.display())?;
            }
            (x, TestResult::Passed) => {
                stdout.set_color(&c.error)?;
                write!(stdout, "{:<10}", "REGRESSED")?;
                stdout.set_color(&c.base)?;
                writeln!(stdout, "{}", k.display())?;
                write_result(x, stdout, c)?;
            }
            (a, b) => {
                stdout.set_color(&c.failure)?;
                write!(stdout, "{:<10}", "CHANGED")?;
                stdout.set_color(&c.base)?;
                writeln!(stdout, "{}", k.display())?;
                writeln!(stdout, "PREVIOUSLY:")?;
                write_result(b, stdout, c)?;
                writeln!(stdout, "NEW:")?;
                write_result(a, stdout, c)?;
            }
        }

        Ok(())
    }
}
