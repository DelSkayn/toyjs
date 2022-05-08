use std::{fs, panic::catch_unwind, path::Path};

use anyhow::{anyhow, Context as AnyhowContext, Result};
use serde::{Deserialize, Serialize};
use toyjs::{Realm, ToyJs, Value};

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
    #[serde(rename = "CanBlockIsFalse")]
    CanBlockIsFalse,
    #[serde(rename = "CanBlockIsTrue")]
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

#[derive(Deserialize, Serialize)]
pub enum FailureCause {
    Errored(String),
    NoError,
}

#[derive(Deserialize, Serialize)]
pub enum TestResult {
    Failed(FailureCause),
    Passed,
    Panic(String),
    Error(String),
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

            let meta = &source[start..start + end];
            // A single test uses \r instead of new line which breaks the yaml parser.
            if meta.contains('\r') {
                let source = meta.replace('\r', "\n");
                serde_yaml::from_str(&source).context("could not parse test metadata")?
            } else {
                serde_yaml::from_str(meta).context("could not parse test metadata")?
            }
        } else {
            MetaData::default()
        };
        Ok(Self { metadata, source })
    }

    pub fn run(&self, harness: &Harness) -> TestResult {
        let res = match catch_unwind(|| {
            let toyjs = ToyJs::new();
            let context = Realm::new(&toyjs);
            context.with(|ctx| {
                match harness.prepare(ctx, &self.metadata.includes) {
                    Ok(()) => {}
                    Err(e) => return TestResult::Error(format!("{:?}", e)),
                }
                match ctx.eval::<_, Value>(&self.source) {
                    Ok(_) => {
                        if self.metadata.negative.is_some() {
                            TestResult::Failed(FailureCause::NoError)
                        } else {
                            TestResult::Passed
                        }
                    }
                    Err(e) => {
                        if self.metadata.negative.is_none() {
                            TestResult::Failed(FailureCause::Errored(format!("{}", e)))
                        } else {
                            TestResult::Passed
                        }
                    }
                }
            })
        }) {
            Ok(x) => x,
            Err(e) => {
                if e.is::<String>() {
                    TestResult::Panic(e.downcast_ref::<String>().unwrap().clone())
                } else if e.is::<&'static str>() {
                    TestResult::Panic((*e.downcast_ref::<&'static str>().unwrap()).to_string())
                } else {
                    TestResult::Panic("Could not format panic message".to_string())
                }
            }
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
            dir_walker(entry.path().as_path(), f)?;
        }
    }
    Ok(())
}
