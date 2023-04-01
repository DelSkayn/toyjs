use clap::{Parser, Subcommand};

#[derive(Subcommand, Debug)]
pub enum Sub {
    All,
    Single { path: String },
    Delta { from: String, to: String },
}

impl Default for Sub {
    fn default() -> Self {
        Sub::All
    }
}

#[derive(Parser, Debug)]
#[clap(author, version, about)]
pub struct Args {
    // Path to the test262 suite
    #[clap(short, long)]
    pub path: Option<String>,
    #[clap(subcommand)]
    pub subcommand: Option<Sub>,
}
