use std::{io::Write, path::Path};

use anyhow::Result;
use termcolor::{ColorChoice, StandardStream};

use crate::{
    commands::{write_result, Colors},
    harness::Harness,
    test::Test,
};

pub fn run(p: impl AsRef<Path>, harness: &Harness) -> Result<()> {
    let mut stdout = StandardStream::stdout(ColorChoice::Auto);
    let colors = Colors::new();

    write!(stdout, "{} => ", p.as_ref().display())?;

    let test = Test::from_path(p.as_ref())?;
    let res = test.run(harness);
    write_result(&res, &mut stdout, &colors)?;
    Ok(())
}
