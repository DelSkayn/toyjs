use anyhow::Result;
use termcolor::{ColorChoice, StandardStream};

use crate::report::Report;

use super::Colors;

pub fn run(from: String, to: String) -> Result<()> {
    let mut stdout = StandardStream::stdout(ColorChoice::Auto);
    let c = Colors::new();

    Report::from_path(&from)?.compare_to(&to, &mut stdout, &c)?;
    Ok(())
}
