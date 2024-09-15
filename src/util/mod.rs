use std::{
    alloc::{alloc, Layout},
    fmt,
    ops::Sub,
    ptr, slice, str,
};

pub(crate) trait IntegerWrapper: Sized + Copy {
    type Wrapped: Ord
        + TryFrom<u32>
        + TryFrom<u64>
        + TryFrom<usize>
        + TryFrom<i32>
        + TryFrom<i64>
        + TryFrom<isize>
        + TryInto<u32>
        + TryInto<u64>
        + TryInto<usize>
        + TryInto<i32>
        + TryInto<i64>
        + TryInto<isize>
        + fmt::Debug
        + Copy;

    fn index(self) -> Self::Wrapped;
    fn from_index(index: Self::Wrapped) -> Self;
    fn from_u32(index: u32) -> Self {
        Self::from_index(index.try_into().unwrap_or_else(|_| panic!()))
    }
    fn from_u64(index: u64) -> Self {
        Self::from_index(index.try_into().unwrap_or_else(|_| panic!()))
    }
    fn from_usize(index: usize) -> Self {
        Self::from_index(index.try_into().unwrap_or_else(|_| panic!()))
    }
    fn from_i32(index: i32) -> Self {
        Self::from_index(index.try_into().unwrap_or_else(|_| panic!()))
    }
    fn from_i64(index: i64) -> Self {
        Self::from_index(index.try_into().unwrap_or_else(|_| panic!()))
    }
    fn from_isize(index: isize) -> Self {
        Self::from_index(index.try_into().unwrap_or_else(|_| panic!()))
    }
    fn as_u32(self) -> u32 {
        self.index().try_into().unwrap_or_else(|_| panic!())
    }
    fn as_u64(self) -> u64 {
        self.index().try_into().unwrap_or_else(|_| panic!())
    }
    fn as_usize(self) -> usize {
        self.index().try_into().unwrap_or_else(|_| panic!())
    }
    fn as_i32(self) -> i32 {
        self.index().try_into().unwrap_or_else(|_| panic!())
    }
    fn as_i64(self) -> i64 {
        self.index().try_into().unwrap_or_else(|_| panic!())
    }
    fn as_isize(self) -> isize {
        self.index().try_into().unwrap_or_else(|_| panic!())
    }
}

pub(crate) trait IndexWrapper
where
    Self: IntegerWrapper,
    Self::Wrapped: Sub<Output = Self::Wrapped>,
{
    fn distance_abs(self, other: Self) -> Self::Wrapped {
        let self_index = self.index();
        let other_index = other.index();
        self_index.max(other_index) - self_index.min(other_index)
    }
}

pub struct Arena {
    chunks: Vec<Box<[u8]>>,
    begin: *mut u8,
    end: *mut u8,
}

const PAGE_SIZE: usize = 4096;
const HUGE_PAGE_SIZE: usize = 2 * 1024 * 1024;

impl Arena {
    pub fn new() -> Self {
        Arena {
            chunks: Vec::new(),
            begin: ptr::null_mut(),
            end: ptr::null_mut(),
        }
    }
    fn new_chunk(&mut self) {
        let chunk;
        let size = self
            .chunks
            .last()
            .map_or(PAGE_SIZE, |s| (s.len() * 2).min(HUGE_PAGE_SIZE));

        let (begin, end) = unsafe {
            let ptr = alloc(Layout::from_size_align(size, 1).unwrap());
            chunk = Box::from_raw(slice::from_raw_parts_mut(ptr, size));
            (ptr, ptr.add(size))
        };
        self.chunks.push(chunk);
        self.begin = begin;
        self.end = end;
    }
    pub fn alloc_raw(&mut self, size: usize) -> *mut u8 {
        loop {
            let begin = self.begin as u64;
            let end = self.end as u64;
            if let Some(new_end) = end.checked_sub(size as u64) {
                if begin <= new_end {
                    let new_end_ptr = new_end as *mut u8;
                    self.end = new_end_ptr;
                    return new_end_ptr;
                }
            }
            self.new_chunk()
        }
    }
    pub fn alloc_slice(&mut self, slice: &[u8]) -> &mut [u8] {
        let data = self.alloc_raw(slice.len());
        let stored = unsafe { slice::from_raw_parts_mut(data, slice.len()) };
        stored.clone_from_slice(slice);
        stored
    }
    pub fn alloc_str(&mut self, string: &str) -> &mut str {
        let data = self.alloc_raw(string.len());
        unsafe {
            let stored_slice = slice::from_raw_parts_mut(data, string.len());
            stored_slice.clone_from_slice(string.as_bytes());
            str::from_utf8_unchecked_mut(stored_slice)
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    fn test_arena() {
        let mut arena = Arena::new();
        let text = "fdsjfjsd";
        let _string = arena.alloc_str(text);
        let _slice = arena.alloc_slice(text.as_bytes());
    }
}
