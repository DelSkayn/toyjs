use std::{
    collections::HashMap,
    fs::{self, File},
    io::Write,
    panic,
    path::{Path, PathBuf},
};

use anyhow::Result;
use serde::{Deserialize, Serialize};
use termcolor::{ColorChoice, StandardStream, WriteColor};

use crate::{
    commands::{write_result, Colors},
    harness::Harness,
    test::{dir_walker, Test, TestResult},
};

#[derive(Deserialize, Serialize)]
pub struct Report {
    tests: HashMap<PathBuf, TestResult>,
}

impl Report {
    pub fn write(&self) -> Result<()> {
        if Path::new("report.json").exists() {
            let mut i = 1;
            let mut cur = PathBuf::from(format!("report.{}.json", i));
            while cur.exists() {
                i += 1;
                cur = PathBuf::from(format!("report.{}.json", i));
            }
            while i > 1 {
                let new = PathBuf::from(format!("report.{}.json", i));
                fs::rename(&new, cur)?;
                cur = new;
                i -= 1;
            }
            fs::rename("report.json", format!("report.1.json"))?;
        }
        let file = File::create("report.json")?;
        serde_json::to_writer(file, self)?;

        Ok(())
    }

    pub fn compare(&self, stdout: &mut StandardStream, c: &Colors) -> Result<()> {
        let file = File::open("report.json")?;
        let mut report: Report = serde_json::from_reader(file)?;

        for (k, new) in self.tests.iter() {
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
            (TestResult::Passed, TestResult::Passed) => {}
            (TestResult::Error(_), TestResult::Error(_)) => {}
            (TestResult::Panic(_), TestResult::Panic(_)) => {}
            (TestResult::Failed(_), TestResult::Failed(_)) => {}
            (TestResult::Passed, _) => {
                stdout.set_color(&c.passed)?;
                write!(stdout, "{:<10}", "IMPROVED")?;
                stdout.set_color(&c.base)?;
                writeln!(stdout, "{}", k.display())?;
            }
            (ref x, TestResult::Passed) => {
                stdout.set_color(&c.error)?;
                write!(stdout, "{:<10}", "REGRESSED")?;
                stdout.set_color(&c.base)?;
                writeln!(stdout, "{}", k.display())?;
                write_result(x, stdout, c)?;
            }
            (ref a, ref b) => {
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

pub fn run(p: impl AsRef<Path>, harness: &Harness) -> Result<()> {
    let p = p.as_ref().join("test");
    let mut stdout = StandardStream::stdout(ColorChoice::Auto);

    let c = Colors::new();

    let mut passed = 0;
    let mut errored = 0;
    let mut failed = 0;
    let mut panicked = 0;

    let mut tests = HashMap::new();

    dir_walker(&p, &mut |path| {
        (|| {
            write!(stdout, "{} => ", path.display())?;
            stdout.flush()?;
            let hook = panic::take_hook();
            let test = Test::from_path(path)?;
            panic::set_hook(Box::new(|_| {}));
            let res = test.run(harness);
            panic::set_hook(hook);

            write_result(&res, &mut stdout, &c)?;

            match res {
                TestResult::Panic(_) => {
                    panicked += 1;
                }
                TestResult::Passed => {
                    passed += 1;
                }
                TestResult::Failed(_) => {
                    failed += 1;
                }
                TestResult::Error(_) => {
                    errored += 1;
                }
            }
            tests.insert(path.to_path_buf(), res);
            Result::<(), anyhow::Error>::Ok(())
        })()
        .unwrap()
    })?;

    let report = Report { tests };
    if Path::new("report.json").exists() {
        writeln!(stdout, "Comparing changes")?;
        report.compare(&mut stdout, &c)?;
    }
    writeln!(stdout, "Writing report")?;
    report.write()?;

    stdout.set_color(&c.header)?;
    writeln!(stdout, "Report:")?;
    stdout.set_color(&c.passed)?;
    write!(stdout, "\tpassed: ")?;
    stdout.set_color(&c.base)?;
    writeln!(stdout, "{}", passed)?;

    stdout.set_color(&c.failure)?;
    write!(stdout, "\tfailed: ")?;
    stdout.set_color(&c.base)?;
    writeln!(stdout, "{}", failed)?;

    stdout.set_color(&c.panic)?;
    write!(stdout, "\tpanicked: ")?;
    stdout.set_color(&c.base)?;
    writeln!(stdout, "{}", panicked)?;

    stdout.set_color(&c.error)?;
    write!(stdout, "\terror: ")?;
    stdout.set_color(&c.base)?;
    writeln!(stdout, "{}", errored)?;
    writeln!(stdout)?;

    let total = passed + failed + panicked + errored;

    writeln!(
        stdout,
        " Conformance: {:.5}%",
        (passed as f64 / total as f64) * 100.0
    )?;

    Ok(())
}
