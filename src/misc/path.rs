macro_rules! path {
    ($base:expr, $($segment:expr),+) => {{
        let mut base: ::std::path::PathBuf = $base.into();
        $(
            base.push($segment);
        )*
        base
    }}
}
pub(crate) use path;

#[cfg(test)]
mod tests {
    use std::ffi::OsStr;
    use std::path::{Path, PathBuf};
    use crate::misc::path::path;

    fn test() {
        let a = path!("a", "b", "c");
        println!("{:?}", a);

        let b = path!(PathBuf::from("z"), OsStr::new("x"), Path::new("y"));
        println!("{:?}", b);
    }
}