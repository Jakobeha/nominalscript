use once_cell::{sync, unsync};

pub trait OnceCellExt<T> {
    fn with_option(option: Option<T>) -> Self;
}

impl<T> OnceCellExt<T> for unsync::OnceCell<T> {
    fn with_option(option: Option<T>) -> Self {
        match option {
            Some(value) => unsync::OnceCell::with_value(value),
            None => unsync::OnceCell::new(),
        }
    }
}

impl<T> OnceCellExt<T> for sync::OnceCell<T> {
    fn with_option(option: Option<T>) -> Self {
        match option {
            Some(value) => sync::OnceCell::with_value(value),
            None => sync::OnceCell::new(),
        }
    }
}