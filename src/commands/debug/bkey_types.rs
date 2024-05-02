//! Representation of the bcachefs bkey types, derived from DWARF debug info.
//!
//! This is adapted from `gimli/crates/examples/src/bin/simple.rs`.

use gimli::Reader as _;
use object::{Object, ObjectSection};
use std::collections::HashSet;
use std::{borrow, error, fs};

/// A list of the known bcachefs bkey types.
#[derive(Debug)]
pub struct BkeyTypes(Vec<BchStruct>);

impl BkeyTypes {
    pub fn new() -> Self {
        BkeyTypes(Vec::new())
    }

    /// Given a struct name and a member name, return the size and offset of
    /// the member within the struct, or None if it does not exist.
    pub fn get_member_layout(&self, outer: &str, member: &str) -> Option<(u64, u64)> {
        for bkey_type in self.0.iter() {
            if bkey_type.name == *outer {
                return bkey_type.member_layout(member);
            }
        }

        None
    }
}

#[derive(Debug)]
pub struct BchStruct {
    name: String,
    pub members: Vec<BchMember>,
}

impl BchStruct {
    pub fn member_layout(&self, name: &str) -> Option<(u64, u64)> {
        for memb in self.members.iter() {
            if memb.name == *name {
                return Some((memb.size, memb.offset));
            }
        }
        None
    }
}

#[derive(Debug)]
pub struct BchMember {
    name: String,
    size: u64,
    offset: u64,
}

// This is a simple wrapper around `object::read::RelocationMap` that implements
// `gimli::read::Relocate` for use with `gimli::RelocateReader`.
// You only need this if you are parsing relocatable object files.
#[derive(Debug, Default)]
struct RelocationMap(object::read::RelocationMap);

impl<'a> gimli::read::Relocate for &'a RelocationMap {
    fn relocate_address(&self, offset: usize, value: u64) -> gimli::Result<u64> {
        Ok(self.0.relocate(offset as u64, value))
    }

    fn relocate_offset(&self, offset: usize, value: usize) -> gimli::Result<usize> {
        <usize as gimli::ReaderOffset>::from_u64(self.0.relocate(offset as u64, value as u64))
    }
}

// The section data that will be stored in `DwarfSections` and `DwarfPackageSections`.
#[derive(Default)]
struct Section<'data> {
    data: borrow::Cow<'data, [u8]>,
    relocations: RelocationMap,
}

// The reader type that will be stored in `Dwarf` and `DwarfPackage`.
// If you don't need relocations, you can use `gimli::EndianSlice` directly.
type Reader<'data> =
    gimli::RelocateReader<gimli::EndianSlice<'data, gimli::RunTimeEndian>, &'data RelocationMap>;

fn process_file(
    object: &object::File,
    struct_list: &mut BkeyTypes,
) -> Result<(), Box<dyn error::Error>> {
    let endian = if object.is_little_endian() {
        gimli::RunTimeEndian::Little
    } else {
        gimli::RunTimeEndian::Big
    };

    // Load a `Section` that may own its data.
    fn load_section<'data>(
        object: &object::File<'data>,
        name: &str,
    ) -> Result<Section<'data>, Box<dyn error::Error>> {
        Ok(match object.section_by_name(name) {
            Some(section) => Section {
                data: section.uncompressed_data()?,
                relocations: section.relocation_map().map(RelocationMap)?,
            },
            None => Default::default(),
        })
    }

    // Borrow a `Section` to create a `Reader`.
    fn borrow_section<'data>(
        section: &'data Section<'data>,
        endian: gimli::RunTimeEndian,
    ) -> Reader<'data> {
        let slice = gimli::EndianSlice::new(borrow::Cow::as_ref(&section.data), endian);
        gimli::RelocateReader::new(slice, &section.relocations)
    }

    // Load all of the sections.
    let dwarf_sections = gimli::DwarfSections::load(|id| load_section(object, id.name()))?;

    // Create `Reader`s for all of the sections and do preliminary parsing.
    // Alternatively, we could have used `Dwarf::load` with an owned type such as `EndianRcSlice`.
    let dwarf = dwarf_sections.borrow(|section| borrow_section(section, endian));

    let mut bkey_types = HashSet::new();
    load_bkey_types(&mut bkey_types);

    let mut iter = dwarf.units();
    while let Some(header) = iter.next()? {
        let unit = dwarf.unit(header)?;
        process_unit(&dwarf, &unit, struct_list, &mut bkey_types)?;
    }

    Ok(())
}

fn load_bkey_types(bkey_types: &mut HashSet<String>) {
    let mut ptr: *const *const i8 = unsafe { bch_bindgen::c::bch2_bkey_types.as_ptr() };
    unsafe {
        while !(*ptr).is_null() {
            let mut bkey_name = String::from("bch_");
            bkey_name.push_str(std::ffi::CStr::from_ptr(*ptr).to_str().unwrap());
            bkey_types.insert(bkey_name);
            ptr = ptr.offset(1);
        }
    }

    // This key type is not included in BCH2_BKEY_TYPES.
    bkey_types.insert("bch_inode_unpacked".to_string());
}

fn process_unit(
    dwarf: &gimli::Dwarf<Reader>,
    unit: &gimli::Unit<Reader>,
    struct_list: &mut BkeyTypes,
    bkey_types: &mut HashSet<String>,
) -> Result<(), gimli::Error> {
    let mut tree = unit.entries_tree(None)?;

    process_tree(dwarf, unit, tree.root()?, struct_list, bkey_types)?;

    Ok(())
}

#[derive(Clone, Copy)]
enum CompType {
    Union,
    Struct,
}

fn process_tree(
    dwarf: &gimli::Dwarf<Reader>,
    unit: &gimli::Unit<Reader>,
    node: gimli::EntriesTreeNode<Reader>,
    struct_list: &mut BkeyTypes,
    bkey_types: &mut HashSet<String>,
) -> gimli::Result<()> {
    let entry = node.entry();
    if entry.tag() == gimli::DW_TAG_structure_type {
        if let Some(name) = entry.attr(gimli::DW_AT_name)? {
            if let Ok(name) = dwarf.attr_string(unit, name.value()) {
                let name = name.to_string_lossy()?.into_owned();
                if bkey_types.remove(&name.clone()) {
                    let mut members: Vec<BchMember> = Vec::new();
                    process_compound_type(dwarf, unit, node, &mut members, 0, CompType::Struct)?;
                    struct_list.0.push(BchStruct { name, members });
                }
            }
        }
    } else {
        let mut children = node.children();
        while let Some(child) = children.next()? {
            process_tree(dwarf, unit, child, struct_list, bkey_types)?;
        }
    }
    Ok(())
}

fn process_compound_type(
    dwarf: &gimli::Dwarf<Reader>,
    unit: &gimli::Unit<Reader>,
    node: gimli::EntriesTreeNode<Reader>,
    members: &mut Vec<BchMember>,
    starting_offset: u64,
    comp: CompType,
) -> gimli::Result<()> {
    let mut children = node.children();
    while let Some(child) = children.next()? {
        process_comp_member(dwarf, unit, child, members, starting_offset, comp)?;
    }

    Ok(())
}

// Given a DIE, checks if that DIE has a reference to a compound type (i.e., struct or union) and
// if so, returns the offset in the DIE tree for that type, and the kind of compound type it is.
fn get_comp_ref(
    unit: &gimli::Unit<Reader>,
    entry: &gimli::DebuggingInformationEntry<Reader>,
) -> Option<(gimli::UnitOffset, CompType)> {
    let ref_type = entry.attr(gimli::DW_AT_type).ok()??;
    let ref_offset = match ref_type.value() {
        gimli::AttributeValue::UnitRef(offset) => offset,
        _ => return None,
    };

    let mut ty_entry = unit.entries_at_offset(ref_offset).ok()?;
    ty_entry.next_entry().ok()??;
    let ty_entry = ty_entry.current()?;

    match ty_entry.tag() {
        gimli::DW_TAG_structure_type => Some((ty_entry.offset(), CompType::Struct)),
        gimli::DW_TAG_union_type => Some((ty_entry.offset(), CompType::Union)),
        _ => None,
    }
}

fn process_comp_member(
    dwarf: &gimli::Dwarf<Reader>,
    unit: &gimli::Unit<Reader>,
    node: gimli::EntriesTreeNode<Reader>,
    members: &mut Vec<BchMember>,
    starting_offset: u64,
    comp: CompType,
) -> gimli::Result<()> {
    let entry = node.entry().clone();

    let offset = match comp {
        CompType::Union => Some(0),
        CompType::Struct => entry
            .attr(gimli::DW_AT_data_member_location)?
            .and_then(|offset| offset.value().udata_value()),
    };
    let Some(offset) = offset else {
        return Ok(());
    };

    if let Some((ref_type, comp)) = get_comp_ref(unit, &entry) {
        let mut tree = unit.entries_tree(Some(ref_type))?;
        process_compound_type(dwarf, unit, tree.root()?, members, offset, comp)?;
    };

    let Some(size) = get_size(unit, &entry) else {
        return Ok(());
    };

    let name = entry.attr(gimli::DW_AT_name)?;
    let Some(name) = name else { return Ok(()) };
    let name = dwarf.attr_string(unit, name.value())?;
    let name = name.to_string_lossy()?.into_owned();

    members.push(BchMember {
        name,
        offset: offset + starting_offset,
        size,
    });

    Ok(())
}

fn get_size(
    unit: &gimli::Unit<Reader>,
    entry: &gimli::DebuggingInformationEntry<Reader>,
) -> Option<u64> {
    if let Some(size) = entry.attr(gimli::DW_AT_byte_size).ok()? {
        return size.udata_value();
    }

    if let Some(ref_type) = entry.attr(gimli::DW_AT_type).ok()? {
        if let gimli::AttributeValue::UnitRef(offset) = ref_type.value() {
            let mut type_entry = unit.entries_at_offset(offset).ok()?;
            type_entry.next_entry().ok()?;
            if let Some(t) = type_entry.current() {
                return get_size(unit, t);
            }
        }
    }

    None
}

/// Return a list of the known bkey types.
pub fn get_bkey_type_info() -> BkeyTypes {
    let path = fs::read_link("/proc/self/exe").unwrap();

    let file = fs::File::open(path).unwrap();
    let mmap = unsafe { memmap2::Mmap::map(&file).unwrap() };
    let object = object::File::parse(&*mmap).unwrap();
    let mut struct_list = BkeyTypes::new();
    process_file(&object, &mut struct_list).unwrap();

    /*
    for s in struct_list.0.iter() {
        for m in s.members.iter() {
            println!("{} {} {} {}", s.name, m.name, m.offset, m.size);
        }
        println!("");
    }
    */

    struct_list
}
