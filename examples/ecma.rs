use js::{lexer, parser};
use std::{fs::File, io::Read, path::Path};
use walkdir::WalkDir;

fn run_file(path: &Path) {
    println!("running script: {}", path.display());
    let mut file = File::open(path).unwrap();
    let mut buffer = String::new();
    file.read_to_string(&mut buffer).unwrap();
    let l = lexer::Lexer::new(&buffer);
    let mut p = parser::Parser::new(l);
    match p.parse_script() {
        Ok(_) => {}
        Err(e) => {
            println!("{}", e);
            panic!();
        }
    };
}

fn main() {
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
        run_file(e.path())
    }
}
