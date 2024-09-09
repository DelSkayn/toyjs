use mmap::AlignedMmap;
use rustix::mm::ProtFlags;

use super::*;

mod procs_map {
    use std::ops::Range;

    use bitflags::bitflags;

    bitflags! {
        #[derive(Clone,Copy,Debug,Eq, PartialEq)]
        pub struct Flags: u8{
            const READ = 0b0001;
            const WRITE = 0b0010;
            const EXECUTE = 0b0100;
            const PRESENT = 0b1000;
        }
    }

    #[derive(Debug)]
    pub struct Segment {
        pub range: Range<usize>,
        pub flags: Flags,
        pub offset: usize,
        pub major_id: usize,
        pub minor_id: usize,
        pub inode: usize,
        pub file: Option<String>,
    }

    /// Simple `/proc/id/maps` reader for verifying memory mapping works as intended.
    #[derive(Debug)]
    pub struct Reader {
        segments: Vec<Segment>,
    }

    impl Reader {
        pub fn capture() -> Self {
            let f = std::fs::read_to_string("/proc/self/maps").unwrap();

            let mut segments = Vec::new();

            for l in f.lines() {
                let mut parts = l.split_whitespace();
                let range = parts.next().unwrap();

                let (start, end) = range.split_once('-').unwrap();

                let start = usize::from_str_radix(start, 16).unwrap();
                let end = usize::from_str_radix(end, 16).unwrap();

                let flags = parts.next().unwrap();
                let mut flags_chars = flags.chars();

                let mut flags = Flags::empty();
                if let Some('r') = flags_chars.next() {
                    flags |= Flags::READ
                }
                if let Some('w') = flags_chars.next() {
                    flags |= Flags::WRITE
                }
                if let Some('x') = flags_chars.next() {
                    flags |= Flags::EXECUTE
                }
                if let Some('p') = flags_chars.next() {
                    flags |= Flags::PRESENT
                }

                let offset = usize::from_str_radix(parts.next().unwrap(), 16).unwrap();

                let dev_version = parts.next().unwrap();

                let (major_version, minor_version) = dev_version.split_once(':').unwrap();

                let major_id = major_version.parse().unwrap();
                let minor_id = minor_version.parse().unwrap();

                let inode = parts.next().unwrap().parse().unwrap();
                let file = parts.next().map(|x| x.to_string());

                segments.push(Segment {
                    range: start..end,
                    flags,
                    offset,
                    major_id,
                    minor_id,
                    inode,
                    file,
                })
            }

            Reader { segments }
        }

        #[track_caller]
        pub fn assert_mapped<T>(&self, ptr: *const T) {
            assert!(self.ptr_is_mapped(ptr))
        }

        #[track_caller]
        pub fn assert_not_mapped<T>(&self, ptr: *const T) {
            assert!(!self.ptr_is_mapped(ptr))
        }

        pub fn ptr_is_mapped<T>(&self, ptr: *const T) -> bool {
            self.segments
                .iter()
                .any(|x| x.range.contains(&(ptr as usize)))
        }

        pub fn segment<T>(&self, ptr: *const T) -> Option<&Segment> {
            self.segments
                .iter()
                .find(|x| x.range.contains(&(ptr as usize)))
        }
    }
}

#[cfg(not(miri))]
#[test]
fn test_read_mmap() {
    let map = procs_map::Reader::capture();

    let ptr = &map as *const procs_map::Reader;
    map.assert_mapped(ptr);

    let (ptr, double) = match unsafe {
        mmap::mmap_aligned(4096 * 2, ProtFlags::WRITE | ProtFlags::READ)
    }
    .unwrap()
    {
        AlignedMmap::Single(x) => (x, false),
        AlignedMmap::Double(x) => (x, true),
    };

    let map = procs_map::Reader::capture();

    if double {
        unsafe {
            map.assert_mapped(ptr.add(4096 * 2));
        }
    }

    let seg = map.segment(ptr).unwrap();
    assert!(seg.flags.contains(procs_map::Flags::READ));
    assert!(seg.flags.contains(procs_map::Flags::WRITE));

    let (ptr_b, double) = match unsafe { mmap::mmap_aligned(4096 * 2, ProtFlags::READ) }.unwrap() {
        AlignedMmap::Single(x) => (x, false),
        AlignedMmap::Double(x) => (x, true),
    };

    let map = procs_map::Reader::capture();

    if double {
        unsafe { map.assert_mapped(ptr_b.add(4096 * 2)) }
    }

    let seg = map.segment(ptr_b).unwrap();

    assert!(seg.flags.contains(procs_map::Flags::READ));
    assert!(!seg.flags.contains(procs_map::Flags::WRITE));

    unsafe { mmap::munmap(ptr.cast(), 4096 * if double { 4 } else { 2 }).unwrap() };
    unsafe { mmap::munmap(ptr_b.cast(), 4096 * if double { 4 } else { 2 }).unwrap() };

    let map = procs_map::Reader::capture();
    map.assert_not_mapped(ptr);
    map.assert_not_mapped(ptr_b);
}
