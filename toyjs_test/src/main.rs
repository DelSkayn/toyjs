#![allow(dead_code)]

use std::path::Path;

use anyhow::Result;
use args::Sub;
use clap::Parser;

mod args;
mod harness;
mod test;

fn main() -> Result<()> {
    let args = args::Args::parse();

    let path = args.path.as_deref().unwrap_or("./test262");

    let harness = harness::Harness::load(path)?;

    match args.subcommand.unwrap_or_default() {
        Sub::All => test::run(path, &harness),
        Sub::Single { path: single_path } => {
            test::run_single(Path::new(path).join(single_path), &harness)
        }
    }
}
