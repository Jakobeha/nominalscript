/// Concatenate path components into path
macro_rules! mk_path {
    ($base:expr, $($segment:expr),+) => {{
        let mut base: ::std::path::PathBuf = $base.into();
        $(
            base.push($segment);
        )*
        base
    }}
}
pub(crate) use mk_path;

#[cfg(test)]
mod tests {
    use std::ffi::OsStr;
    use std::path::{Path, PathBuf};
    use crate::misc::mk_path::mk_path;

    fn test() {
        let a = mk_path!("a", "b", "c");
        println!("{:?}", a);

        let b = mk_path!(PathBuf::from("z"), OsStr::new("x"), Path::new("y"));
        println!("{:?}", b);

        let c = mk_path!(&PathBuf::from("z"), mk_path!(OsStr::new("x"), "y"));
        println!("{:?}", c);
    }
}