#![allow(dead_code)]

use std::path::Path;

use anyhow::Result;
use args::Sub;
use clap::Parser;

mod args;
mod commands;
mod harness;
mod report;
mod test;

fn main() -> Result<()> {
    let args = args::Args::parse();

    let path = args.path.as_deref().unwrap_or("./test262");

    let harness = harness::Harness::load(path)?;

    match args.subcommand.unwrap_or_default() {
        Sub::All { filter } => commands::all::run(path, &harness, filter),
        Sub::Single { path: single_path } => {
            commands::single::run(Path::new(path).join(single_path), &harness)
        }
        Sub::Delta { from, to } => commands::delta::run(from, to),
    }?;
    Ok(())
}
