#![allow(dead_code)]

use anyhow::Result;
use clap::Parser;

mod args;
mod harness;
mod test;

fn main() -> Result<()> {
    let args = args::Args::parse();

    let path = args
        .path
        .as_ref()
        .map(|x| x.as_str())
        .unwrap_or("./test262");

    let harness = harness::Harness::load(path)?;

    test::run(path, &harness)
}
