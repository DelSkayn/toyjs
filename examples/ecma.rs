use js::{interner::Interner, lexer};
use std::{fs::File, io::Read, path::Path};
use walkdir::WalkDir;

fn run_file(path: &Path, interner: &mut Interner) -> bool {
    let mut file = File::open(path).unwrap();
    let mut buffer = String::new();
    file.read_to_string(&mut buffer).unwrap();
    let mut l = lexer::Lexer::new(buffer.as_bytes(), interner);
    let mut tokens = Vec::new();
    loop {
        match l.next_token() {
            Ok(Some(e)) => tokens.push(e),
            Err(e) => {
                println!("error in file: {}\n{:?}", path.display(), e);
                println!("source: {}", buffer);
                for b in tokens.iter() {
                    println!("{:?}", b);
                }
                panic!();
                return false;
            }
            Ok(None) => return true,
        }
        tokens.clear();
    }
}

fn main() {
    env_logger::init();
    let mut count = 0;
    let mut finished = 0;
    let mut interner = Interner::new();
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

        if run_file(e.path(), &mut interner) {
            finished += 1;
        }
    }
    if count == finished {
        println!("100% COVERAGE Hurray! 🎉🎉")
    }
    println!(
        "Parsing: \n[num tests: {} | succesfull: {}] = rate: {}",
        count,
        finished,
        finished as f32 / count as f32
    );
}
