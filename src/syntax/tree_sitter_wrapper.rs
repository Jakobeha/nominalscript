use roaring::RoaringTreemap;
use yak_sitter::define_custom_wrapper;
use crate::diagnostics::{FileDiagnostics, FileLogger};

pub struct TreeCustom {
    deleted_nodes: RoaringTreemap,
    diagnostics: FileDiagnostics
}

define_custom_wrapper!(TreeCustom);