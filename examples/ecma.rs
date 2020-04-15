use js::{lexer, parser, parser::ParseErrorKind};
use std::{fs::File, io::Read, path::Path};
use walkdir::WalkDir;

fn run_file(path: &Path) -> bool {
    let mut file = File::open(path).unwrap();
    let mut buffer = String::new();
    file.read_to_string(&mut buffer).unwrap();
    let l = lexer::Lexer::new(&buffer);
    let mut p = parser::Parser::new(l);
    match p.parse_script() {
        Ok(_) => true,
        Err(e) => {
            println!("in file: {}", path.display());
            println!("{}", e);
            //panic!();
            false
        }
    }
}

fn main() {
    env_logger::init();
    let mut count = 0;
    let mut finished = 0;
    for e in WalkDir::new("test262/test")
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| {
            e.path()
                .extension()
                .and_then(|e| e.to_str())
                .map(|e| e == "js")
                .unwrap_or(false)
        })
    {
        count += 1;

        if run_file(e.path()) {
            finished += 1;
        }
    }
    if count == finished {
        println!("100% COVERAGE Hurray! 🎉🎉")
    }
    println!(
        "[num tests: {} | succesfull: {}] = rate: {}",
        count,
        finished,
        finished as f32 / count as f32
    );
}
