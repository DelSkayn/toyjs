use std::{
    any::Any,
    fs,
    panic::{self, catch_unwind},
    path::Path,
};

use anyhow::{anyhow, Context as AnyhowContext, Result};
use serde::Deserialize;
use std::io::Write;
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};
use toyjs::{Context, ToyJs, Value};

use crate::harness::Harness;

#[derive(Deserialize, Debug)]
#[serde(rename_all = "snake_case")]
pub enum ErrorPhase {
    Parse,
    Resolution,
    Runtime,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub enum Flag {
    OnlyStrict,
    NoStrict,
    Module,
    Raw,
    Async,
    Generated,
    CanBlockIsFalse,
    CanBlockIsTrue,
    #[serde(rename = "non-deterministic")]
    NonDeterministic,
}

#[derive(Deserialize, Debug)]
pub struct Negative {
    phase: ErrorPhase,
    r#type: String,
}

#[derive(Deserialize, Default, Debug)]
pub struct MetaData {
    #[serde(default)]
    description: String,
    negative: Option<Negative>,
    #[serde(default)]
    includes: Vec<String>,
    #[serde(default)]
    flags: Vec<Flag>,
    #[serde(default)]
    locale: Vec<String>,
}

#[derive(Debug)]
pub struct Test {
    metadata: MetaData,
    pub source: String,
}

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

pub enum FailureCause {
    Errored(String),
    NoError,
}

pub enum TestResult {
    Failed(FailureCause),
    Passed,
    Panic(Box<dyn Any + Send + 'static>),
}

pub fn write_result(
    result: &Result<TestResult>,
    out: &mut StandardStream,
    c: &Colors,
) -> Result<()> {
    match result {
        Ok(x) => match x {
            TestResult::Panic(payload) => {
                out.set_color(&c.panic)?;
                writeln!(out, "PANICKED")?;
                out.set_color(&c.base)?;
                if let Some(x) = payload.downcast_ref::<String>() {
                    writeln!(out, "\t> {}", x)?;
                } else if let Some(x) = payload.downcast_ref::<&str>() {
                    writeln!(out, "\t> {}", x)?;
                } else {
                    writeln!(out, "\t> {:?}", payload)?;
                }
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
        },
        Err(e) => {
            out.set_color(&c.error)?;
            writeln!(out, "ERROR: ")?;
            out.set_color(&c.base)?;
            writeln!(out, "\t> {}", e)?;
        }
    }
    Ok(())
}

impl Test {
    pub fn from_path(p: impl AsRef<Path>) -> Result<Self> {
        let source = fs::read_to_string(p.as_ref())
            .with_context(|| format!("could not read test file `{}`", p.as_ref().display()))?;
        Self::from_source(source)
    }

    pub fn from_source(source: String) -> Result<Self> {
        let metadata = if let Some(start) = source.find("/*---") {
            let start = start + 5;
            let end = source[start..]
                .find("---*/")
                .ok_or_else(|| anyhow!("could not find metadata end delimiter"))?;

            serde_yaml::from_str(&source[start..start + end])
                .with_context(|| format!("could not parse test metadata"))?
        } else {
            MetaData::default()
        };
        Ok(Self { metadata, source })
    }

    pub fn run(&self, harness: &Harness) -> Result<TestResult> {
        let res = match catch_unwind(|| {
            let toyjs = ToyJs::new();
            let context = Context::new(&toyjs);
            context.with(|ctx| {
                harness.prepare(ctx, &self.metadata.includes)?;
                match ctx.eval::<Value, _>(&self.source) {
                    Ok(_) => {
                        if self.metadata.negative.is_some() {
                            Ok(TestResult::Failed(FailureCause::NoError))
                        } else {
                            Ok(TestResult::Passed)
                        }
                    }
                    Err(e) => {
                        if self.metadata.negative.is_none() {
                            Ok(TestResult::Failed(FailureCause::Errored(format!("{}", e))))
                        } else {
                            Ok(TestResult::Passed)
                        }
                    }
                }
            })
        }) {
            Ok(x) => x,
            Err(e) => Ok(TestResult::Panic(e)),
        };
        res
    }
}

pub fn dir_walker<F: FnMut(&Path)>(p: &Path, f: &mut F) -> Result<()> {
    let iter =
        fs::read_dir(p).with_context(|| format!("could not read directory: {}", p.display()))?;
    for entry in iter {
        let entry = entry?;
        if entry.file_type()?.is_file() {
            f(&entry.path());
        } else {
            dir_walker(entry.path().as_path(), f)?
        }
    }
    Ok(())
}

pub fn run_single(p: impl AsRef<Path>, harness: &Harness) -> Result<()> {
    let mut stdout = StandardStream::stdout(ColorChoice::Auto);
    let colors = Colors::new();

    write!(stdout, "{} => ", p.as_ref().display())?;

    let res = Test::from_path(p.as_ref()).and_then(|test| test.run(harness));
    write_result(&res, &mut stdout, &colors)?;
    Ok(())
}

pub fn run(p: impl AsRef<Path>, harness: &Harness) -> Result<()> {
    let p = p.as_ref().join("test");
    let mut stdout = StandardStream::stdout(ColorChoice::Auto);

    let c = Colors::new();

    let mut passed = 0;
    let mut errored = 0;
    let mut failed = 0;
    let mut panicked = 0;

    dir_walker(&p, &mut |path| {
        (|| {
            write!(stdout, "{} => ", path.display())?;
            stdout.flush()?;
            let hook = panic::take_hook();
            panic::set_hook(Box::new(|_| {}));
            let res = Test::from_path(path).and_then(|test| test.run(harness));
            panic::set_hook(hook);

            write_result(&res, &mut stdout, &c)?;

            match res {
                Ok(x) => match x {
                    TestResult::Panic(_) => {
                        panicked += 1;
                    }
                    TestResult::Passed => {
                        passed += 1;
                    }
                    TestResult::Failed(_) => {
                        failed += 1;
                    }
                },
                Err(_) => {
                    errored += 1;
                }
            }
            Result::<(), anyhow::Error>::Ok(())
        })()
        .unwrap()
    })?;

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
    writeln!(stdout, "")?;

    let total = passed + failed + panicked + errored;

    writeln!(
        stdout,
        " Conformance: {:.5}%",
        (passed as f64 / total as f64) * 100.0
    )?;

    Ok(())
}
