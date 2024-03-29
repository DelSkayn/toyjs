use std::{
    env,
    fs::File,
    io::{self, Read},
    time::Instant,
};

fn get_input() -> Result<Box<dyn Read>, io::Error> {
    if let Some(x) = env::args().nth(1) {
        Ok(Box::new(File::open(x)?) as Box<dyn Read>)
    } else {
        Ok(Box::new(io::stdin()) as Box<dyn Read>)
    }
}

fn main() -> Result<(), io::Error> {
    let before = Instant::now();
    let mut read = get_input()?;
    let mut buffer = std::string::String::new();
    read.read_to_string(&mut buffer)?;
    let elapsed = before.elapsed();

    println!("{buffer}");
    println!("parsed in {:.4} seconds", elapsed.as_secs_f64());
    Ok(())
}
