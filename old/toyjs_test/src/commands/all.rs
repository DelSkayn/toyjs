use std::{collections::HashMap, io::Write, panic, path::Path};

use anyhow::Result;
use termcolor::{ColorChoice, StandardStream, WriteColor};

use crate::{
    commands::{write_result, Colors},
    harness::Harness,
    report::Report,
    test::{dir_walker, Test, TestResult},
};

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
        .unwrap();
    })?;

    let report = Report::from_results(tests);
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
