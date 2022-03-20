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
use toyjs::Context;

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

pub enum TestResult {
    Failed,
    Passed,
    Panic(Box<dyn Any + Send + 'static>),
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
            let context = Context::new();
            context.with(|ctx| {
                harness.prepare(ctx, &self.metadata.includes)?;
                match ctx.eval(&self.source) {
                    Ok(_) => {
                        if self.metadata.negative.is_some() {
                            Ok(TestResult::Failed)
                        } else {
                            Ok(TestResult::Passed)
                        }
                    }
                    Err(_) => {
                        if self.metadata.negative.is_none() {
                            Ok(TestResult::Failed)
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

    let base_color = ColorSpec::new().set_fg(Some(Color::White)).clone();
    let error_color = ColorSpec::new()
        .set_bold(true)
        .set_fg(Some(Color::Red))
        .clone();
    let passed_color = ColorSpec::new()
        .set_bold(true)
        .set_fg(Some(Color::Green))
        .clone();
    let panic_color = ColorSpec::new()
        .set_bold(true)
        .set_fg(Some(Color::Blue))
        .clone();

    write!(stdout, "{} => ", p.as_ref().display())?;

    let test = match Test::from_path(p.as_ref()) {
        Ok(x) => x,
        Err(e) => {
            stdout.set_color(&error_color)?;
            writeln!(stdout, "ERROR: ")?;
            stdout.set_color(&base_color)?;
            writeln!(stdout, "\t> could not load test: {}", e)?;
            return Result::<(), anyhow::Error>::Ok(());
        }
    };

    match test.run(harness) {
        Ok(x) => match x {
            TestResult::Panic(payload) => {
                stdout.set_color(&panic_color)?;
                writeln!(stdout, "PANICKED")?;
                stdout.set_color(&base_color)?;
                if let Some(x) = payload.downcast_ref::<String>() {
                    writeln!(stdout, "\t> {}", x)?;
                } else if let Some(x) = payload.downcast_ref::<&str>() {
                    writeln!(stdout, "\t> {}", x)?;
                } else {
                    writeln!(stdout, "\t> {:?}", payload)?;
                }
            }
            TestResult::Passed => {
                stdout.set_color(&passed_color)?;
                writeln!(stdout, "PASSED")?;
                stdout.set_color(&base_color)?;
                write!(stdout, "")?;
            }
            TestResult::Failed => {
                stdout.set_color(&error_color)?;
                writeln!(stdout, "FAILED")?;
                stdout.set_color(&base_color)?;
                write!(stdout, "")?;
            }
        },
        Err(e) => {
            stdout.set_color(&error_color)?;
            writeln!(stdout, "ERROR: ")?;
            stdout.set_color(&base_color)?;
            writeln!(stdout, "\t> {}", e)?;
        }
    }
    Ok(())
}

pub fn run(p: impl AsRef<Path>, harness: &Harness) -> Result<()> {
    let p = p.as_ref().join("test");
    let mut stdout = StandardStream::stdout(ColorChoice::Auto);

    let base_color = ColorSpec::new().set_fg(Some(Color::White)).clone();
    let header_color = ColorSpec::new().set_bold(true).clone();
    let error_color = ColorSpec::new()
        .set_bold(true)
        .set_fg(Some(Color::Red))
        .clone();
    let failed_color = ColorSpec::new()
        .set_bold(true)
        .set_fg(Some(Color::Yellow))
        .clone();
    let passed_color = ColorSpec::new()
        .set_bold(true)
        .set_fg(Some(Color::Green))
        .clone();
    let panic_color = ColorSpec::new()
        .set_bold(true)
        .set_fg(Some(Color::Blue))
        .clone();

    let mut passed = 0;
    let mut errored = 0;
    let mut failed = 0;
    let mut panicked = 0;

    dir_walker(&p, &mut |path| {
        (|| {
            write!(stdout, "{} => ", path.display())?;
            let test = match Test::from_path(path) {
                Ok(x) => x,
                Err(e) => {
                    errored += 1;
                    stdout.set_color(&error_color)?;
                    writeln!(stdout, "ERROR: ")?;
                    stdout.set_color(&base_color)?;
                    writeln!(stdout, "\t> could not load test: {}", e)?;
                    return Result::<(), anyhow::Error>::Ok(());
                }
            };
            let hook = panic::take_hook();
            panic::set_hook(Box::new(|_| {}));
            let res = test.run(harness);
            panic::set_hook(hook);

            match res {
                Ok(x) => match x {
                    TestResult::Panic(payload) => {
                        panicked += 1;
                        stdout.set_color(&panic_color)?;
                        writeln!(stdout, "PANICKED")?;
                        stdout.set_color(&base_color)?;
                        if let Some(x) = payload.downcast_ref::<String>() {
                            writeln!(stdout, "\t> {}", x)?;
                        } else if let Some(x) = payload.downcast_ref::<&str>() {
                            writeln!(stdout, "\t> {}", x)?;
                        } else {
                            writeln!(stdout, "\t> {:?}", payload)?;
                        }
                    }
                    TestResult::Passed => {
                        passed += 1;
                        stdout.set_color(&passed_color)?;
                        writeln!(stdout, "PASSED")?;
                        stdout.set_color(&base_color)?;
                        write!(stdout, "")?;
                    }
                    TestResult::Failed => {
                        failed += 1;
                        stdout.set_color(&failed_color)?;
                        writeln!(stdout, "FAILED")?;
                        stdout.set_color(&base_color)?;
                        write!(stdout, "")?;
                    }
                },
                Err(e) => {
                    errored += 1;
                    stdout.set_color(&error_color)?;
                    write!(stdout, "ERROR: ")?;
                    stdout.set_color(&base_color)?;
                    writeln!(stdout, "{}", e)?;
                }
            }
            Ok(())
        })()
        .unwrap()
    })?;

    stdout.set_color(&header_color)?;
    writeln!(stdout, "Report:")?;
    stdout.set_color(&base_color)?;

    stdout.set_color(&passed_color)?;
    write!(stdout, "\tpassed: ")?;
    stdout.set_color(&base_color)?;
    writeln!(stdout, "{}", passed)?;

    stdout.set_color(&failed_color)?;
    write!(stdout, "\tfailed: ")?;
    stdout.set_color(&base_color)?;
    writeln!(stdout, "{}", failed)?;

    stdout.set_color(&panic_color)?;
    write!(stdout, "\tpanicked: ")?;
    stdout.set_color(&base_color)?;
    writeln!(stdout, "{}", panicked)?;

    stdout.set_color(&error_color)?;
    write!(stdout, "\terror: ")?;
    stdout.set_color(&base_color)?;
    writeln!(stdout, "{}", errored)?;

    Ok(())
}
