/// [std::collections::hash_map::Entry::insert_entry] but stable and doesn't return anything
pub trait EntryInsertExt<'a, K, V> {
    /// Sets the value of the entry, replacing the old value if it exists.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::HashMap;
    ///
    /// let mut map: HashMap<&str, String> = HashMap::new();
    /// let entry = map.entry("poneyland").insert("hoho".to_string());
    ///
    /// assert_eq!(entry.key(), &"poneyland");
    /// ```
    fn insert(self, value: V);
}

impl<'a, K, V> EntryInsertExt<'a, K, V> for std::collections::hash_map::Entry<'a, K, V> {
    fn insert(self, value: V) {
        match self {
            std::collections::hash_map::Entry::Occupied(mut entry) => { entry.insert(value); }
            std::collections::hash_map::Entry::Vacant(entry) => { entry.insert(value); }
        }
    }
}