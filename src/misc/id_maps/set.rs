use std::io;
use std::marker::PhantomData;
use std::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Sub, SubAssign};
use roaring::{MultiOps as InnerMultiOps, NonSortedIntegers, RoaringTreemap};
use crate::misc::id_maps::Id;

#[derive(Debug, Clone, Default, PartialEq)]
#[repr(transparent)]
pub struct IdSet<V: Id>(RoaringTreemap, PhantomData<V>);

// region from roaring-rs (wrapper)
/// A [`Iterator::collect`] blanket implementation that provides extra methods for [`IdSet`].
///
/// When merging multiple bitmap with the same operation it's usually faster to call the
/// method in this trait than to write your own for loop and merging the bitmaps yourself.
///
/// # Examples
/// ```
/// use roaring::{MultiOps, RoaringBitmap};
///
/// let bitmaps = [
///     RoaringBitmap::from_iter(0..10),
///     RoaringBitmap::from_iter(10..20),
///     RoaringBitmap::from_iter(20..30),
/// ];
///
/// // Stop doing this
/// let naive = bitmaps.clone().into_iter().reduce(|a, b| a | b).unwrap_or_default();
///
/// // And start doing this instead, it will be much faster!
/// let iter = bitmaps.union();
///
/// assert_eq!(naive, iter);
/// ```
pub trait MultiOps<T>: IntoIterator<Item = T> {
    /// The type of output from operations.
    type Output;

    /// The `union` between all elements.
    fn union(self) -> Self::Output;

    /// The `intersection` between all elements.
    fn intersection(self) -> Self::Output;

    /// The `difference` between all elements.
    fn difference(self) -> Self::Output;

    /// The `symmetric difference` between all elements.
    fn symmetric_difference(self) -> Self::Output;
}


//noinspection DuplicatedCode
impl<I: IntoIterator<Item = IdSet<V>>, V: Id> MultiOps<IdSet<V>> for I {
    type Output = IdSet<V>;

    fn union(self) -> Self::Output {
        IdSet(InnerMultiOps::union(self.into_iter().map(|x| x.0)), PhantomData)
    }

    fn intersection(self) -> Self::Output {
        IdSet(InnerMultiOps::intersection(self.into_iter().map(|x| x.0)), PhantomData)
    }

    fn difference(self) -> Self::Output {
        IdSet(InnerMultiOps::difference(self.into_iter().map(|x| x.0)), PhantomData)
    }

    fn symmetric_difference(self) -> Self::Output {
        IdSet(InnerMultiOps::symmetric_difference(self.into_iter().map(|x| x.0)), PhantomData)
    }
}

//noinspection DuplicatedCode
impl<I: IntoIterator<Item = Result<IdSet<V>, E>>, E, V: Id> MultiOps<Result<IdSet<V>, E>> for I  {
    type Output = Result<IdSet<V>, E>;

    fn union(self) -> Self::Output {
        Result::map(
            InnerMultiOps::union(self.into_iter().map(|res| res.map(|x| x.0))),
            |inner| IdSet(inner, PhantomData)
        )
    }

    fn intersection(self) -> Self::Output {
        Result::map(
            InnerMultiOps::intersection(self.into_iter().map(|res| res.map(|x| x.0))),
            |inner| IdSet(inner, PhantomData)
        )
    }

    fn difference(self) -> Self::Output {
        Result::map(
            InnerMultiOps::difference(self.into_iter().map(|res| res.map(|x| x.0))),
            |inner| IdSet(inner, PhantomData)
        )
    }

    fn symmetric_difference(self) -> Self::Output {
        Result::map(
            InnerMultiOps::symmetric_difference(self.into_iter().map(|res| res.map(|x| x.0))),
            |inner| IdSet(inner, PhantomData)
        )
    }
}

//noinspection DuplicatedCode
impl<'a, I: IntoIterator<Item = &'a IdSet<V>>, V: Id> MultiOps<&'a IdSet<V>> for I {
    type Output = IdSet<V>;

    fn union(self) -> Self::Output {
        IdSet(InnerMultiOps::union(self.into_iter().map(|x| &x.0)), PhantomData)
    }

    fn intersection(self) -> Self::Output {
        IdSet(InnerMultiOps::intersection(self.into_iter().map(|x| &x.0)), PhantomData)
    }

    fn difference(self) -> Self::Output {
        IdSet(InnerMultiOps::difference(self.into_iter().map(|x| &x.0)), PhantomData)
    }

    fn symmetric_difference(self) -> Self::Output {
        IdSet(InnerMultiOps::symmetric_difference(self.into_iter().map(|x| &x.0)), PhantomData)
    }
}

//noinspection DuplicatedCode
impl<'a, I: IntoIterator<Item = Result<&'a IdSet<V>, E>>, E: 'a, V: Id> MultiOps<Result<&'a IdSet<V>, E>> for I {
    type Output = Result<IdSet<V>, E>;

    fn union(self) -> Self::Output {
        Result::map(
            InnerMultiOps::union(self.into_iter().map(|res| res.map(|x| &x.0))),
            |inner| IdSet(inner, PhantomData)
        )
    }

    fn intersection(self) -> Self::Output {
        Result::map(
            InnerMultiOps::intersection(self.into_iter().map(|res| res.map(|x| &x.0))),
            |inner| IdSet(inner, PhantomData)
        )
    }

    fn difference(self) -> Self::Output {
        Result::map(
            InnerMultiOps::difference(self.into_iter().map(|res| res.map(|x| &x.0))),
            |inner| IdSet(inner, PhantomData)
        )
    }

    fn symmetric_difference(self) -> Self::Output {
        Result::map(
            InnerMultiOps::symmetric_difference(self.into_iter().map(|res| res.map(|x| &x.0))),
            |inner| IdSet(inner, PhantomData)
        )
    }
}

impl<V: Id> IdSet<V> {
    /// Returns true if the set has no elements in common with other. This is equivalent to
    /// checking for an empty intersection.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use roaring::RoaringTreemap;
    ///
    /// let mut rb1 = RoaringTreemap::new();
    /// let mut rb2 = RoaringTreemap::new();
    ///
    /// rb1.insert(1);
    ///
    /// assert_eq!(rb1.is_disjoint(&rb2), true);
    ///
    /// rb2.insert(1);
    ///
    /// assert_eq!(rb1.is_disjoint(&rb2), false);
    ///
    /// ```
    pub fn is_disjoint(&self, other: &Self) -> bool {
        self.0.is_disjoint(&other.0)
    }

    /// Returns `true` if this set is a subset of `other`.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use roaring::RoaringTreemap;
    ///
    /// let mut rb1 = RoaringTreemap::new();
    /// let mut rb2 = RoaringTreemap::new();
    ///
    /// rb1.insert(1);
    ///
    /// assert_eq!(rb1.is_subset(&rb2), false);
    ///
    /// rb2.insert(1);
    ///
    /// assert_eq!(rb1.is_subset(&rb2), true);
    ///
    /// rb1.insert(2);
    ///
    /// assert_eq!(rb1.is_subset(&rb2), false);
    /// ```
    pub fn is_subset(&self, other: &Self) -> bool {
        self.0.is_subset(&other.0)
    }

    /// Returns `true` if this set is a superset of `other`.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use roaring::RoaringTreemap;
    ///
    /// let mut rb1 = RoaringTreemap::new();
    /// let mut rb2 = RoaringTreemap::new();
    ///
    /// rb1.insert(1);
    ///
    /// assert_eq!(rb2.is_superset(&rb1), false);
    ///
    /// rb2.insert(1);
    ///
    /// assert_eq!(rb2.is_superset(&rb1), true);
    ///
    /// rb1.insert(2);
    ///
    /// assert_eq!(rb2.is_superset(&rb1), false);
    /// ```
    pub fn is_superset(&self, other: &Self) -> bool {
        self.0.is_superset(&other.0)
    }

    /// Creates an empty `RoaringTreemap`.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use roaring::RoaringTreemap;
    /// let rb = RoaringTreemap::new();
    /// ```
    pub fn new() -> Self {
        IdSet(RoaringTreemap::new(), PhantomData)
    }

    /// Creates a full `RoaringTreemap`.
    ///
    /// # Examples
    ///
    /// ```rust,ignore
    /// use roaring::RoaringTreemap;
    /// let rb = RoaringTreemap::full();
    /// ```
    pub fn full() -> Self {
        IdSet(RoaringTreemap::full(), PhantomData)
    }

    /// Adds a value to the set. Returns `true` if the value was not already present in the set.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use roaring::RoaringTreemap;
    ///
    /// let mut rb = RoaringTreemap::new();
    /// assert_eq!(rb.insert(3), true);
    /// assert_eq!(rb.insert(3), false);
    /// assert_eq!(rb.contains(3), true);
    /// ```
    pub fn insert(&mut self, value: V) -> bool {
        self.0.insert(value.into())
    }

    /// Pushes `value` in the treemap only if it is greater than the current maximum value.
    ///
    /// Returns whether the value was inserted.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use roaring::RoaringTreemap;
    ///
    /// let mut rb = RoaringTreemap::new();
    /// assert!(rb.push(1));
    /// assert!(rb.push(3));
    /// assert_eq!(rb.push(3), false);
    /// assert!(rb.push(5));
    ///
    /// assert_eq!(rb.iter().collect::<Vec<u64>>(), vec![1, 3, 5]);
    /// ```
    pub fn push(&mut self, value: V) -> bool {
        self.0.push(value.into())
    }

    /// Removes a value from the set. Returns `true` if the value was present in the set.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use roaring::RoaringTreemap;
    ///
    /// let mut rb = RoaringTreemap::new();
    /// rb.insert(3);
    /// assert_eq!(rb.remove(3), true);
    /// assert_eq!(rb.remove(3), false);
    /// assert_eq!(rb.contains(3), false);
    /// ```
    pub fn remove(&mut self, value: V) -> bool {
        self.0.remove(value.into())
    }

    /// Returns `true` if this set contains the specified integer.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use roaring::RoaringTreemap;
    ///
    /// let mut rb = RoaringTreemap::new();
    /// rb.insert(1);
    /// assert_eq!(rb.contains(0), false);
    /// assert_eq!(rb.contains(1), true);
    /// assert_eq!(rb.contains(100), false);
    /// ```
    pub fn contains(&self, value: V) -> bool {
        self.0.contains(value.into())
    }

    /// Clears all integers in this set.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use roaring::RoaringTreemap;
    ///
    /// let mut rb = RoaringTreemap::new();
    /// rb.insert(1);
    /// assert_eq!(rb.contains(1), true);
    /// rb.clear();
    /// assert_eq!(rb.contains(1), false);
    /// ```
    pub fn clear(&mut self) {
        self.0.clear();
    }

    /// Returns `true` if there are no integers in this set.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use roaring::RoaringTreemap;
    ///
    /// let mut rb = RoaringTreemap::new();
    /// assert_eq!(rb.is_empty(), true);
    ///
    /// rb.insert(3);
    /// assert_eq!(rb.is_empty(), false);
    /// ```
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Returns `true` if there are every possible integers in this set.
    ///
    /// # Examples
    ///
    /// ```rust,ignore
    /// use roaring::RoaringTreemap;
    ///
    /// let mut rb = RoaringTreemap::full();
    /// assert!(!rb.is_empty());
    /// assert!(rb.is_full());
    /// ```
    pub fn is_full(&self) -> bool {
        self.0.is_full()
    }

    /// Returns the number of distinct integers added to the set.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use roaring::RoaringTreemap;
    ///
    /// let mut rb = RoaringTreemap::new();
    /// assert_eq!(rb.len(), 0);
    ///
    /// rb.insert(3);
    /// assert_eq!(rb.len(), 1);
    ///
    /// rb.insert(3);
    /// rb.insert(4);
    /// assert_eq!(rb.len(), 2);
    /// ```
    pub fn len(&self) -> usize {
        self.0.len() as usize
    }

    /// Returns the minimum value in the set (if the set is non-empty).
    ///
    /// # Examples
    ///
    /// ```rust
    /// use roaring::RoaringTreemap;
    ///
    /// let mut rb = RoaringTreemap::new();
    /// assert_eq!(rb.min(), None);
    ///
    /// rb.insert(3);
    /// rb.insert(4);
    /// assert_eq!(rb.min(), Some(3));
    /// ```
    pub fn min(&self) -> Option<V> {
        self.0.min().map(|v| v.into())
    }

    /// Returns the maximum value in the set (if the set is non-empty).
    ///
    /// # Examples
    ///
    /// ```rust
    /// use roaring::RoaringTreemap;
    ///
    /// let mut rb = RoaringTreemap::new();
    /// assert_eq!(rb.max(), None);
    ///
    /// rb.insert(3);
    /// rb.insert(4);
    /// assert_eq!(rb.max(), Some(4));
    /// ```
    pub fn max(&self) -> Option<V> {
        self.0.max().map(|v| v.into())
    }

    /// Returns the number of integers that are <= value. rank(u64::MAX) == len()
    ///
    /// # Examples
    ///
    /// ```rust
    /// use roaring::RoaringTreemap;
    ///
    /// let mut rb = RoaringTreemap::new();
    /// assert_eq!(rb.rank(0), 0);
    ///
    /// rb.insert(3);
    /// rb.insert(4);
    /// assert_eq!(rb.rank(3), 1);
    /// assert_eq!(rb.rank(10), 2)
    /// ```
    pub fn rank(&self, value: V) -> V {
        self.0.rank(value.into()).into()
    }

    /// Returns the `n`th integer in the set or `None` if `n <= len()`
    ///
    /// # Examples
    ///
    /// ```rust
    /// use roaring::RoaringTreemap;
    ///
    /// let mut rb = RoaringTreemap::new();
    /// assert_eq!(rb.select(0), None);
    ///
    /// rb.append(vec![0, 10, 100]).unwrap();
    ///
    /// assert_eq!(rb.select(0), Some(0));
    /// assert_eq!(rb.select(1), Some(10));
    /// assert_eq!(rb.select(2), Some(100));
    /// assert_eq!(rb.select(3), None);
    /// ```
    pub fn select(&self, n: usize) -> Option<V> {
        self.0.select(n as u64).map(|v| v.into())
    }

    /// Iterator over each value stored in the RoaringTreemap, guarantees values are ordered by
    /// value.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use roaring::RoaringTreemap;
    /// use std::iter::FromIterator;
    ///
    /// let bitmap = (1..3).collect::<RoaringTreemap>();
    /// let mut iter = bitmap.iter();
    ///
    /// assert_eq!(iter.next(), Some(1));
    /// assert_eq!(iter.next(), Some(2));
    /// assert_eq!(iter.next(), None);
    /// ```
    pub fn iter(&self) -> Iter<'_, V> {
        Iter(self.0.iter(), PhantomData)
    }
}

pub struct Iter<'a, V: Id>(roaring::treemap::Iter<'a>, PhantomData<V>);
pub struct IntoIter<V: Id>(roaring::treemap::IntoIter, PhantomData<V>);

impl<'a, V: Id> Iterator for Iter<'a, V> {
    type Item = V;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|v| v.into())
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<'a, V: Id> DoubleEndedIterator for Iter<'a, V> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back().map(|v| v.into())
    }
}

#[cfg(target_pointer_width = "64")]
impl<'a, V: Id> ExactSizeIterator for Iter<'a, V> {
    fn len(&self) -> usize {
        self.0.len()
    }
}

impl<V: Id> Iterator for IntoIter<V> {
    type Item = V;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|v| v.into())
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<V: Id> DoubleEndedIterator for IntoIter<V> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back().map(|v| v.into())
    }
}

#[cfg(target_pointer_width = "64")]
impl<V: Id> ExactSizeIterator for IntoIter<V> {
    fn len(&self) -> usize {
        self.0.len()
    }
}

impl<'a, V: Id> IntoIterator for &'a IdSet<V> {
    type Item = V;
    type IntoIter = Iter<'a, V>;

    fn into_iter(self) -> Iter<'a, V> {
        self.iter()
    }
}

impl<V: Id> IntoIterator for IdSet<V> {
    type Item = V;
    type IntoIter = IntoIter<V>;

    fn into_iter(self) -> IntoIter<V> {
        IntoIter(self.0.into_iter(), PhantomData)
    }
}

impl<V: Id> FromIterator<V> for IdSet<V> {
    fn from_iter<I: IntoIterator<Item = V>>(iterator: I) -> IdSet<V> {
        IdSet(RoaringTreemap::from_iter(iterator.into_iter().map(|v| v.into())), PhantomData)
    }
}

impl<'a, V: Id> FromIterator<&'a V> for IdSet<V> {
    fn from_iter<I: IntoIterator<Item = &'a V>>(iterator: I) -> IdSet<V> {
        IdSet(RoaringTreemap::from_iter(iterator.into_iter().map(|v| (*v).into())), PhantomData)
    }
}

impl<V: Id> Extend<V> for IdSet<V> {
    fn extend<I: IntoIterator<Item = V>>(&mut self, iterator: I) {
        self.0.extend(iterator.into_iter().map(|v| v.into()))
    }
}

impl<'a, V: Id> Extend<&'a V> for IdSet<V> {
    fn extend<I: IntoIterator<Item = &'a V>>(&mut self, iterator: I) {
        self.0.extend(iterator.into_iter().map(|v| (*v).into()))
    }
}

impl<V: Id> IdSet<V> {
    /// Create the set from a sorted iterator. Values must be sorted and deduplicated.
    ///
    /// The values of the iterator must be ordered and strictly greater than the greatest value
    /// in the set. If a value in the iterator doesn't satisfy this requirement, it is not added
    /// and the append operation is stopped.
    ///
    /// Returns `Ok` with the requested `RoaringTreemap`, `Err` with the number of elements
    /// we tried to append before an error occurred.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use roaring::RoaringTreemap;
    ///
    /// let mut rb = RoaringTreemap::from_sorted_iter(0..10).unwrap();
    ///
    /// assert!(rb.iter().eq(0..10));
    /// ```
    pub fn from_sorted_iter<I: IntoIterator<Item = V>>(
        iterator: I
    ) -> Result<Self, NonSortedIntegers> {
        RoaringTreemap::from_sorted_iter(iterator.into_iter().map(|v| v.into()))
            .map(|inner| IdSet(inner, PhantomData))
    }

    /// Extend the set with a sorted iterator.
    ///
    /// The values of the iterator must be ordered and strictly greater than the greatest value
    /// in the set. If a value in the iterator doesn't satisfy this requirement, it is not added
    /// and the append operation is stopped.
    ///
    /// Returns `Ok` with the number of elements appended to the set, `Err` with
    /// the number of elements we effectively appended before an error occurred.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use roaring::RoaringTreemap;
    ///
    /// let mut rb = RoaringTreemap::new();
    /// rb.append(0..10).unwrap();
    ///
    /// assert!(rb.iter().eq(0..10));
    /// ```
    pub fn append<I: IntoIterator<Item = V>>(
        &mut self,
        iterator: I,
    ) -> Result<usize, NonSortedIntegers> {
        self.0.append(iterator.into_iter().map(|v| v.into())).map(|res| res as usize)
    }

    /// Computes the len of the union with the specified other treemap without creating a new
    /// treemap.
    ///
    /// This is faster and more space efficient when you're only interested in the cardinality of
    /// the union.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use roaring::RoaringTreemap;
    ///
    /// let rb1: RoaringTreemap = (1..4).collect();
    /// let rb2: RoaringTreemap = (3..5).collect();
    ///
    ///
    /// assert_eq!(rb1.union_len(&rb2), (rb1 | rb2).len());
    /// ```
    pub fn union_len(&self, other: &Self) -> usize {
        self.0.union_len(&other.0) as usize
    }

    /// Computes the len of the intersection with the specified other treemap without creating a
    /// new treemap.
    ///
    /// This is faster and more space efficient when you're only interested in the cardinality of
    /// the intersection.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use roaring::RoaringTreemap;
    ///
    /// let rb1: RoaringTreemap = (1..4).collect();
    /// let rb2: RoaringTreemap = (3..5).collect();
    ///
    ///
    /// assert_eq!(rb1.intersection_len(&rb2), (rb1 & rb2).len());
    /// ```
    pub fn intersection_len(&self, other: &Self) -> usize {
        self.0.intersection_len(&other.0) as usize
    }

    /// Computes the len of the difference with the specified other treemap without creating a new
    /// treemap.
    ///
    /// This is faster and more space efficient when you're only interested in the cardinality of
    /// the difference.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use roaring::RoaringTreemap;
    ///
    /// let rb1: RoaringTreemap = (1..4).collect();
    /// let rb2: RoaringTreemap = (3..5).collect();
    ///
    ///
    /// assert_eq!(rb1.difference_len(&rb2), (rb1 - rb2).len());
    /// ```
    pub fn difference_len(&self, other: &Self) -> usize {
        self.0.difference_len(&other.0) as usize
    }

    /// Computes the len of the symmetric difference with the specified other treemap without
    /// creating a new bitmap.
    ///
    /// This is faster and more space efficient when you're only interested in the cardinality of
    /// the symmetric difference.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use roaring::RoaringTreemap;
    ///
    /// let rb1: RoaringTreemap = (1..4).collect();
    /// let rb2: RoaringTreemap = (3..5).collect();
    ///
    ///
    /// assert_eq!(rb1.symmetric_difference_len(&rb2), (rb1 ^ rb2).len());
    /// ```
    pub fn symmetric_difference_len(&self, other: &Self) -> usize {
        self.0.symmetric_difference_len(&other.0) as usize
    }
}

impl<V: Id> BitOr<IdSet<V>> for IdSet<V> {
    type Output = IdSet<V>;

    /// A `union` between two sets.
    fn bitor(self, rhs: IdSet<V>) -> IdSet<V> {
        IdSet(self.0.bitor(rhs.0), PhantomData)
    }
}

impl<V: Id> BitOr<&IdSet<V>> for IdSet<V> {
    type Output = IdSet<V>;

    /// A `union` between two sets.
    fn bitor(self, rhs: &IdSet<V>) -> IdSet<V> {
        IdSet(self.0.bitor(&rhs.0), PhantomData)
    }
}

impl<V: Id> BitOr<IdSet<V>> for &IdSet<V> {
    type Output = IdSet<V>;

    /// A `union` between two sets.
    fn bitor(self, rhs: IdSet<V>) -> IdSet<V> {
        IdSet((&self.0).bitor(rhs.0), PhantomData)
    }
}

impl<V: Id> BitOr<&IdSet<V>> for &IdSet<V> {
    type Output = IdSet<V>;

    /// A `union` between two sets.
    fn bitor(self, rhs: &IdSet<V>) -> IdSet<V> {
        IdSet((&self.0).bitor(&rhs.0), PhantomData)
    }
}

impl<V: Id> BitOrAssign<IdSet<V>> for IdSet<V> {
    /// A `union` between two sets.
    fn bitor_assign(&mut self, rhs: IdSet<V>) {
        self.0.bitor_assign(rhs.0)
    }
}

impl<V: Id> BitOrAssign<&IdSet<V>> for IdSet<V> {
    /// A `union` between two sets.
    fn bitor_assign(&mut self, rhs: &IdSet<V>) {
        self.0.bitor_assign(&rhs.0)
    }
}

impl<V: Id> BitAnd<IdSet<V>> for IdSet<V> {
    type Output = IdSet<V>;

    /// An `intersection` between two sets.
    fn bitand(self, rhs: IdSet<V>) -> IdSet<V> {
        IdSet(self.0.bitand(rhs.0), PhantomData)
    }
}

impl<V: Id> BitAnd<&IdSet<V>> for IdSet<V> {
    type Output = IdSet<V>;

    /// An `intersection` between two sets.
    fn bitand(self, rhs: &IdSet<V>) -> IdSet<V> {
        IdSet(self.0.bitand(&rhs.0), PhantomData)
    }
}

impl<V: Id> BitAnd<IdSet<V>> for &IdSet<V> {
    type Output = IdSet<V>;

    /// An `intersection` between two sets.
    fn bitand(self, rhs: IdSet<V>) -> IdSet<V> {
        IdSet((&self.0).bitand(rhs.0), PhantomData)
    }
}

impl<V: Id> BitAnd<&IdSet<V>> for &IdSet<V> {
    type Output = IdSet<V>;

    /// An `intersection` between two sets.
    fn bitand(self, rhs: &IdSet<V>) -> IdSet<V> {
        IdSet((&self.0).bitand(&rhs.0), PhantomData)
    }
}

impl<V: Id> BitAndAssign<IdSet<V>> for IdSet<V> {
    /// An `intersection` between two sets.
    fn bitand_assign(&mut self, rhs: IdSet<V>) {
        self.0.bitand_assign(rhs.0)
    }
}

impl<V: Id> BitAndAssign<&IdSet<V>> for IdSet<V> {
    /// An `intersection` between two sets.
    fn bitand_assign(&mut self, rhs: &IdSet<V>) {
        self.0.bitand_assign(&rhs.0)
    }
}

impl<V: Id> Sub<IdSet<V>> for IdSet<V> {
    type Output = IdSet<V>;

    /// A `difference` between two sets.
    fn sub(self, rhs: IdSet<V>) -> IdSet<V> {
        IdSet(self.0.sub(rhs.0), PhantomData)
    }
}

impl<V: Id> Sub<&IdSet<V>> for IdSet<V> {
    type Output = IdSet<V>;

    /// A `difference` between two sets.
    fn sub(self, rhs: &IdSet<V>) -> IdSet<V> {
        IdSet(self.0.sub(&rhs.0), PhantomData)
    }
}

impl<V: Id> Sub<IdSet<V>> for &IdSet<V> {
    type Output = IdSet<V>;

    /// A `difference` between two sets.
    fn sub(self, rhs: IdSet<V>) -> IdSet<V> {
        IdSet((&self.0).sub(rhs.0), PhantomData)
    }
}

impl<V: Id> Sub<&IdSet<V>> for &IdSet<V> {
    type Output = IdSet<V>;

    /// A `difference` between two sets.
    fn sub(self, rhs: &IdSet<V>) -> IdSet<V> {
        IdSet((&self.0).sub(&rhs.0), PhantomData)
    }
}

impl<V: Id> SubAssign<IdSet<V>> for IdSet<V> {
    /// A `difference` between two sets.
    fn sub_assign(&mut self, rhs: IdSet<V>) {
        self.0.sub_assign(rhs.0)
    }
}

impl<V: Id> SubAssign<&IdSet<V>> for IdSet<V> {
    /// A `difference` between two sets.
    fn sub_assign(&mut self, rhs: &IdSet<V>) {
        self.0.sub_assign(&rhs.0)
    }
}

impl<V: Id> BitXor<IdSet<V>> for IdSet<V> {
    type Output = IdSet<V>;

    /// A `symmetric difference` between two sets.
    fn bitxor(self, rhs: IdSet<V>) -> IdSet<V> {
        IdSet(self.0.bitxor(rhs.0), PhantomData)
    }
}

impl<V: Id> BitXor<&IdSet<V>> for IdSet<V> {
    type Output = IdSet<V>;

    /// A `symmetric difference` between two sets.
    fn bitxor(self, rhs: &IdSet<V>) -> IdSet<V> {
        IdSet(self.0.bitxor(&rhs.0), PhantomData)
    }
}

impl<V: Id> BitXor<IdSet<V>> for &IdSet<V> {
    type Output = IdSet<V>;

    /// A `symmetric difference` between two sets.
    fn bitxor(self, rhs: IdSet<V>) -> IdSet<V> {
        IdSet((&self.0).bitxor(rhs.0), PhantomData)
    }
}

impl<V: Id> BitXor<&IdSet<V>> for &IdSet<V> {
    type Output = IdSet<V>;

    /// A `symmetric difference` between two sets.
    fn bitxor(self, rhs: &IdSet<V>) -> IdSet<V> {
        IdSet((&self.0).bitxor(&rhs.0), PhantomData)
    }
}

impl<V: Id> BitXorAssign<IdSet<V>> for IdSet<V> {
    /// A `symmetric difference` between two sets.
    fn bitxor_assign(&mut self, rhs: IdSet<V>) {
        self.0.bitxor_assign(rhs.0)
    }
}

impl<V: Id> BitXorAssign<&IdSet<V>> for IdSet<V> {
    /// A `symmetric difference` between two sets.
    fn bitxor_assign(&mut self, rhs: &IdSet<V>) {
        self.0.bitxor_assign(&rhs.0)
    }
}

impl<V: Id> IdSet<V> {
    /// Return the size in bytes of the serialized output.
    /// This is compatible with the official C/C++, Java and Go implementations.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use roaring::RoaringTreemap;
    ///
    /// let rb1: RoaringTreemap = (1..4).collect();
    /// let mut bytes = Vec::with_capacity(rb1.serialized_size());
    /// rb1.serialize_into(&mut bytes).unwrap();
    /// let rb2 = RoaringTreemap::deserialize_from(&bytes[..]).unwrap();
    ///
    /// assert_eq!(rb1, rb2);
    /// ```
    pub fn serialized_size(&self) -> usize {
        self.0.serialized_size()
    }

    /// Serialize this bitmap.
    /// This is compatible with the official C/C++, Java and Go implementations.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use roaring::RoaringTreemap;
    ///
    /// let rb1: RoaringTreemap = (1..4).collect();
    /// let mut bytes = vec![];
    /// rb1.serialize_into(&mut bytes).unwrap();
    /// let rb2 = RoaringTreemap::deserialize_from(&bytes[..]).unwrap();
    ///
    /// assert_eq!(rb1, rb2);
    /// ```
    pub fn serialize_into<W: io::Write>(&self, mut writer: W) -> io::Result<()> {
        self.0.serialize_into(&mut writer)
    }

    /// Deserialize a bitmap into memory.
    ///
    /// This is compatible with the official C/C++, Java and Go implementations.
    /// This method checks that all of the internal values are valid.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use roaring::RoaringTreemap;
    ///
    /// let rb1: RoaringTreemap = (1..4).collect();
    /// let mut bytes = vec![];
    /// rb1.serialize_into(&mut bytes).unwrap();
    /// let rb2 = RoaringTreemap::deserialize_from(&bytes[..]).unwrap();
    ///
    /// assert_eq!(rb1, rb2);
    /// ```
    pub fn deserialize_from<R: io::Read>(reader: R) -> io::Result<Self> {
        RoaringTreemap::deserialize_from(reader)
            .map(|inner| IdSet(inner, PhantomData))
    }

    /// Deserialize a bitmap into memory.
    ///
    /// This is compatible with the official C/C++, Java and Go implementations.
    /// This method is memory safe but will not check if the data is a valid bitmap.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use roaring::RoaringTreemap;
    ///
    /// let rb1: RoaringTreemap = (1..4).collect();
    /// let mut bytes = vec![];
    /// rb1.serialize_into(&mut bytes).unwrap();
    /// let rb2 = RoaringTreemap::deserialize_unchecked_from(&bytes[..]).unwrap();
    ///
    /// assert_eq!(rb1, rb2);
    /// ```
    pub fn deserialize_unchecked_from<R: io::Read>(reader: R) -> io::Result<Self> {
        RoaringTreemap::deserialize_unchecked_from(reader)
            .map(|inner| IdSet(inner, PhantomData))
    }
}
// endregion
