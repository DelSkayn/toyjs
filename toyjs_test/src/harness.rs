use std::{collections::HashMap, fs, path::Path};

use anyhow::{anyhow, Context, Result};
use toyjs::{Ctx, Value};

use crate::test::Test;

pub struct Harness {
    sta: Test,
    assert: Test,
    other: HashMap<String, Test>,
}

impl Harness {
    pub fn load(path: impl AsRef<Path>) -> Result<Self> {
        let harness_path = path.as_ref().join("harness");
        let iterator = fs::read_dir(&harness_path).with_context(|| {
            anyhow!(
                "could not read harness directory: `{}`",
                harness_path.display()
            )
        })?;

        let mut sta = None;
        let mut assert = None;
        let mut other = HashMap::new();

        for entry in iterator {
            let entry = entry?;
            let path = entry.path().to_owned();
            let name = path.file_name().ok_or_else(|| {
                anyhow!(
                    "could not find file name for directory entry: {}",
                    path.display()
                )
            })?;
            let name = name.to_str().ok_or_else(|| {
                anyhow!(
                    "could not convert file path to utf8: {}",
                    name.to_string_lossy()
                )
            })?;
            let source = fs::read_to_string(entry.path())
                .with_context(|| format!("could not read file from path `{}`", path.display()))?;

            let test = Test::from_source(source)
                .with_context(|| format!("could not parse test at path `{}`", path.display()))?;

            if name == "sta.js" {
                sta = Some(test);
            } else if name == "assert.js" {
                assert = Some(test);
            } else {
                other.insert(name.to_owned(), test);
            }
        }
        let sta = sta.ok_or_else(|| anyhow!("`sta.js` missing from harness files"))?;
        let assert = assert.ok_or_else(|| anyhow!("`assert.js` missing from harness files"))?;
        Ok(Harness { sta, assert, other })
    }

    pub fn prepare(&self, ctx: Ctx, includes: &[String]) -> Result<()> {
        for include in includes.iter() {
            let test = self.other.get(include).ok_or_else(|| {
                anyhow!(
                    "test requires harness file `{}` but no such file was found",
                    include
                )
            })?;
            ctx.eval::<Value, _>(&test.source)
                .map_err(|e| anyhow!("failed to execute harness file `{}`: {:?}", include, e))?;
        }
        Ok(())
    }
}
