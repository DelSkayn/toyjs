use clap::Parser;

#[derive(Parser, Debug)]
#[clap(author, version, about)]
pub struct Args {
    // Path to the test262 suite
    #[clap(short, long)]
    pub path: Option<String>,
}
