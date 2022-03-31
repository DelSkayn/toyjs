use std::io::Write;

use anyhow::Result;
use termcolor::{Color, ColorSpec, StandardStream, WriteColor};

use crate::test::{FailureCause, TestResult};

pub mod all;
pub mod single;

pub struct Colors {
    error: ColorSpec,
    panic: ColorSpec,
    failure: ColorSpec,
    passed: ColorSpec,
    base: ColorSpec,
    header: ColorSpec,
}

impl Colors {
    pub fn new() -> Self {
        let base = ColorSpec::new().set_fg(Some(Color::White)).clone();
        let error = ColorSpec::new()
            .set_bold(true)
            .set_fg(Some(Color::Red))
            .clone();
        let failure = ColorSpec::new()
            .set_bold(true)
            .set_fg(Some(Color::Yellow))
            .clone();
        let passed = ColorSpec::new()
            .set_bold(true)
            .set_fg(Some(Color::Green))
            .clone();
        let panic = ColorSpec::new()
            .set_bold(true)
            .set_fg(Some(Color::Blue))
            .clone();
        let header = ColorSpec::new().set_bold(true).clone();

        Colors {
            error,
            panic,
            failure,
            passed,
            base,
            header,
        }
    }
}

pub fn write_result(result: &TestResult, out: &mut StandardStream, c: &Colors) -> Result<()> {
    match result {
        TestResult::Panic(payload) => {
            out.set_color(&c.panic)?;
            writeln!(out, "PANICKED")?;
            out.set_color(&c.base)?;
            writeln!(out, "\t> {}", payload)?;
        }
        TestResult::Passed => {
            out.set_color(&c.passed)?;
            writeln!(out, "PASSED")?;
            out.set_color(&c.base)?;
            write!(out, "")?;
        }
        TestResult::Failed(cause) => {
            out.set_color(&c.failure)?;
            writeln!(out, "FAILED")?;
            out.set_color(&c.base)?;
            write!(out, "\t> ")?;
            match *cause {
                FailureCause::NoError => {
                    writeln!(out, "Test did not return an error when it should have")?
                }
                FailureCause::Errored(ref e) => writeln!(out, "Test returned error: {}", e)?,
            }
        }
        TestResult::Error(e) => {
            out.set_color(&c.error)?;
            writeln!(out, "ERROR: ")?;
            out.set_color(&c.base)?;
            writeln!(out, "\t> {}", e)?;
        }
    }
    Ok(())
}
