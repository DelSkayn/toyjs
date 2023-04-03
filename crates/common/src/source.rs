use crate::string::String;

pub struct Source {
    name: Option<String>,
    source: String,
}

impl Source {
    pub fn new<N: Into<String>, S: Into<String>>(source: S, name: Option<N>) -> Self {
        Source {
            name: name.map(|x| x.into()),
            source: source.into(),
        }
    }
}
