/*******************************************************************************
 * Copyright (c) 2020, 2020 Red Hat and others
 *
 * This program and the accompanying materials are made available under
 * the terms of the Eclipse Public License 2.0 which accompanies this
 * distribution and is available at https://www.eclipse.org/legal/epl-2.0/
 * or the Apache License, Version 2.0 which accompanies this distribution and
 * is available at https://www.apache.org/licenses/LICENSE-2.0.
 *
 * This Source Code may also be made available under the following
 * Secondary Licenses when the conditions for such availability set
 * forth in the Eclipse Public License, v. 2.0 are satisfied: GNU
 * General Public License, version 2 with the GNU Classpath
 * Exception [1] and GNU General Public License, version 2 with the
 * OpenJDK Assembly Exception [2].
 *
 * [1] https://www.gnu.org/software/classpath/license.html
 * [2] http://openjdk.java.net/legal/assembly-exception.html
 *
 * SPDX-License-Identifier: EPL-2.0 OR Apache-2.0 OR GPL-2.0 WITH Classpath-exception-2.0 OR LicenseRef-GPL-2.0 WITH Assembly-exception
 *******************************************************************************/

#include <cassert>
#include <elf.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "hashtable_api.h"
#include "omrutil.h"
#include "SnapshotImageWriter.hpp"
#include "vm_api.h"

static uintptr_t string_table_hash(void *entry, void *userData);
static uintptr_t string_table_equal(void *leftEntry, void *rightEntry, void *userData);
static void string_table_print(OMRPortLibrary *portLibrary, void *entry, void *userData); 
static void printSectionHeader(SnapshotImageSectionHeader *header);

SnapshotImageWriter::SnapshotImageWriter(const char *filename, J9PortLibrary *port_lib, bool is_little_endian)
	: _filename(filename)
	, _is_little_endian(is_little_endian)
	, _image_file(nullptr)
	, _num_program_headers(0)
	, _num_section_headers(0)
	, _index_name_section_header(0)
	, _program_header_start_offset(0)
	, _section_header_start_offset(0)
	, _file_offset(0)
	, _is_invalid(false)
	, _program_headers(nullptr)
	, _program_headers_tail(nullptr)
	, _port_lib(port_lib)
	, _section_header_name_string_table(_port_lib)
	, _section_header_string_table_header(nullptr)
	, _static_string_table(port_lib)
	, _static_string_table_header(nullptr)
	, _dynamic_string_table(port_lib)
	, _dynamic_string_table_header(nullptr)
	, _symbol_table(&_static_string_table, &_dynamic_string_table, port_lib)
{
	printf("SnapshotImageWriter\n");
	/**
	 * Create the null section header.  It's required in every file with
	 * section headers and not worth adding to a list.  Treat it as
	 * special case as it must be writen first.  We can force this by
	 * having it be first in the list
	 */
	SnapshotImageSectionHeader* zeroSectionHeader = allocateSectionHeader(0, nullptr);
	_section_headers = zeroSectionHeader;
	_section_headers_tail = zeroSectionHeader;

	/**
	 * Create the section header string table ("shstrtab") section as its needed to name
	 * the other sections
	 */
	_section_header_string_table_header = allocateSectionHeader(SHT_STRTAB, ".shstrtab");
	_index_name_section_header = _section_header_string_table_header->index;
	append_to_section_header_list(_section_header_string_table_header);
	_section_header_name_string_table.set_section_header(_section_header_string_table_header);

	/* Create section header for the static string table (".strtab") */
	_static_string_table_header = allocateSectionHeader(SHT_STRTAB, ".strtab");
	append_to_section_header_list(_static_string_table_header);
	_static_string_table.set_section_header(_static_string_table_header);

	/* Create section header for the static symbol table (".symtab") */
	SnapshotImageSectionHeader* sh_symtab = allocateSectionHeader(SHT_SYMTAB, ".symtab");
	append_to_section_header_list(sh_symtab);
	_symbol_table.set_local_section_header(sh_symtab);

	/* Create section header for the dynamic symbol table (".dynsym") */
	SnapshotImageSectionHeader* dynsym = allocateSectionHeader(SHT_DYNSYM, ".dynsym");
	/* TODO: This needs to be part of the PT_DYNAMIC section, not the segmentless section */
	append_to_section_header_list(dynsym);
	_symbol_table.set_global_section_header(dynsym);
	printSectionHeader(dynsym);

	{ //TODO: this needs to be part of the DYNAMIC section
		SnapshotImageSectionHeader* dynstr = allocateSectionHeader(SHT_STRTAB, ".dynstr");
		append_to_section_header_list(dynstr);
		_dynamic_string_table.set_section_header(dynstr);
	}
}

SnapshotImageWriter::~SnapshotImageWriter()
{
	printf("Deleting SnapshotImageWriter\n");
	if (_num_program_headers > 0) {
		SnapshotImageProgramHeader *iter = _program_headers;
		while (nullptr != iter) {
			SnapshotImageProgramHeader *prev = iter;
			iter = iter->next;
			free(prev);
		}
	}
	if (_section_headers != nullptr) {
		SnapshotImageSectionHeader *current = _section_headers;
		while (current != nullptr) {
			SnapshotImageSectionHeader *next = current->next;
			free(current);
			current = next;
		}
	}

}

/* PROGRAM HEADER
typedef struct {
               uint32_t   p_type;
               uint32_t   p_flags;
               Elf64_Off  p_offset;
               Elf64_Addr p_vaddr;
               Elf64_Addr p_paddr;
               uint64_t   p_filesz;
               uint64_t   p_memsz;
               uint64_t   p_align;
} Elf64_Phdr;
*/
static void printProgramHeader(SnapshotImageProgramHeader *header) {
	printf(">>>>>> {\n"
	"p_type = %" PRIu32 "\n"
	"p_flags = %" PRIu32 "\n"
	"p_offset = %x\n"
	"p_vaddr = %x\n"
	"p_paddr = %x\n"
	"p_filesz = %" PRIu64 "\n"
	"p_memsz = %" PRIu64 "\n"
	"p_align = %" PRIu64 "\n"
	"}\n",
	header->p_header.p_type,
	header->p_header.p_flags,
	(int)header->p_header.p_offset,
	(int)header->p_header.p_vaddr,
	(int)header->p_header.p_paddr,
	header->p_header.p_filesz,
	header->p_header.p_memsz,
	header->p_header.p_align
	);
}

SnapshotImageProgramHeader* SnapshotImageWriter::startProgramHeader(uint32_t type, uint32_t flags, Elf64_Addr vaddr, Elf64_Addr paddr, uint64_t align) {
	SnapshotImageProgramHeader *header = static_cast<SnapshotImageProgramHeader *>(malloc(sizeof(SnapshotImageProgramHeader)));
	if (header == nullptr) {
		invalidateFile();
		return nullptr;
	}
	memset(header, 0, sizeof(*header));
	header->p_header.p_type = type;
	header->p_header.p_flags = flags;
	header->p_header.p_offset = _file_offset;
	header->p_header.p_vaddr = vaddr;
	header->p_header.p_paddr = paddr;
	header->p_header.p_align = align;

	/* Set up linked list of program headers, always adding to the end of the list */
	if (_program_headers == nullptr) {
		_program_headers = header;
		_program_headers_tail = header;
	} else {
		_program_headers_tail->next = header;
		_program_headers_tail = header;
	}
	_num_program_headers += 1;
	return header;
}

void SnapshotImageWriter::endProgramHeader(SnapshotImageProgramHeader *programHeader, uint64_t extraMemSize) {
	programHeader->p_header.p_filesz = (_file_offset - programHeader->p_header.p_offset);
	/* Force memsize to be the same of the filesize, for now at least */
	programHeader->p_header.p_memsz = programHeader->p_header.p_filesz + extraMemSize;
}

void SnapshotImageWriter::writeProgramHeaders(void)
{
	if (_program_headers != nullptr) {
		/* Record the offset for the ELF header */
		_program_header_start_offset = _file_offset;
		SnapshotImageProgramHeader *iterator = _program_headers;
		while (iterator != nullptr) {
			writeBytes(reinterpret_cast<uint8_t*>(&iterator->p_header), sizeof(Elf64_Phdr));
			printProgramHeader(iterator);
			iterator = iterator->next;
		}
	}
}

#if 0
/* SECTIONS
typedef struct {
	uint32_t   sh_name;
	uint32_t   sh_type;
	uint64_t   sh_flags;
	Elf64_Addr sh_addr;
	Elf64_Off  sh_offset;
	uint64_t   sh_size;
	uint32_t   sh_link;
	uint32_t   sh_info;
	uint64_t   sh_addralign;
	uint64_t   sh_entsize;
} Elf64_Shdr;
*/
/* Sections are contained within ProgramHeaders (mostly) */
SnapshotImageSectionHeader* startSectionHeader(SnapshotImageProgramHeader *programHeader);
void endSectionHeader(SnapshotImageSectionHeader *sectionHeader;

#endif

static void printSectionHeader(SnapshotImageSectionHeader *header) {
	printf(">>>>>> Elf64_Shdr {\n"
	"sh_name = %" PRIu32 "\n"
	"sh_type = %" PRIu32 "\n"
	"sh_flags = %" PRIx64 "\n"
	"sh_addr = %" PRIx64 "\n"
	"sh_offset = %" PRIu64 "\n"
	"sh_size = %" PRIu64 " \n"
	"sh_link = %" PRIu32 " \n"
	"sh_info = %" PRIu32 "\n"
	"sh_addralign = %" PRIu64 "\n"
	"sh_entsize = %" PRIu64 "\n"
	"}\n",
	header->s_header.sh_name,
	header->s_header.sh_type,
	header->s_header.sh_flags,
	(uint64_t)header->s_header.sh_addr,
	(uint64_t)header->s_header.sh_offset,
	header->s_header.sh_size,
	header->s_header.sh_link,
	header->s_header.sh_info,
	header->s_header.sh_addralign,
	header->s_header.sh_entsize);
}

SymbolTableEntry* SnapshotImageWriter::create_global_symbol(const char *name, SymbolTable::Type type, uint16_t sectionIndex, uintptr_t value, uint64_t size)
{
	return _symbol_table.create_symbol(name,
		SymbolTable::Binding::Global,
		type,
		SymbolTable::Visibility::Default,
		sectionIndex,
		value,
		size);
}

SymbolTableEntry* SnapshotImageWriter::create_local_symbol(const char *name, SymbolTable::Type type, uint16_t sectionIndex, uintptr_t value, uint64_t size)
{
	return _symbol_table.create_symbol(name,
		SymbolTable::Binding::Local,
		type,
		SymbolTable::Visibility::Default,
		sectionIndex,
		value,
		size);
}

SnapshotImageSectionHeader* SnapshotImageWriter::allocateSectionHeader(uint32_t type, const char *section_name) {
	SnapshotImageSectionHeader *header = static_cast<SnapshotImageSectionHeader *>(malloc(sizeof(SnapshotImageSectionHeader)));
	if (header == nullptr) {
		invalidateFile();
		return nullptr;
	}
	memset(header, 0, sizeof(*header));
	header->s_header.sh_type = type;
	header->s_header.sh_name = _section_header_name_string_table.get_string_table_index(section_name);
	header->index = _num_section_headers;
	_num_section_headers += 1;
	return header;
}

void SnapshotImageWriter::append_to_section_header_list(SnapshotImageSectionHeader* header)
{
	/* List will always have th zero section header in it first so we can unconditionally
	 * add to the tail without needing to check the head
	 */
	_section_headers_tail->next = header;
	_section_headers_tail = header;
}

bool SnapshotImageWriter::writeStringTable(StringTable *table)
{
	//table->debug_print_table();
	assert(table != nullptr);
	SnapshotImageSectionHeader *header = table->get_section_header();
	assert(header != nullptr);
	header->s_header.sh_offset = _file_offset;
	header->s_header.sh_size = table->get_table_size();
	return table->write_table_segment(this);
}

#if TO_DELETE
bool SnapshotImageWriter::writeLocalSymbolTable()
{
	_symbol_table.get_local_string_table()->debug_print_table();

	SnapshotImageSectionHeader *header = _symbol_table.get_local_section_header();
	header->s_header.sh_offset = _file_offset;
	header->s_header.sh_size = _symbol_table.get_local_table_size();
	/* SymbolTables point to their StringTable section */
	header->s_header.sh_link = _symbol_table.get_local_string_table()->get_section_header_index();
	/* SymbolTables require the `sh_entsize` to indicate the size of the table entries */
	header->s_header.sh_entsize = sizeof(Elf64_Sym);
	/* SymbolTables need to put the local symbols first, then the global symbols
	 * and have the sh_info point to the first non-local symbol.  We need to + 1
	 * here to account for the mandatory SHN_UNDEF symbol that must start the table.
	 */
	header->s_header.sh_info = _symbol_table.get_number_of_local_symbols() + 1;
	return _symbol_table.write_table_segment(this, SymbolTable::Binding::Local);
}
#endif

bool SnapshotImageWriter::writeSymbolTable(SymbolTable::Binding binding)
{
	SnapshotImageSectionHeader *header = nullptr;
	uint32_t table_size = 0;
	uint32_t section_index = 0;
	uint32_t num_symbols = 1; // The mandatory SHN_UNDEF symbol
	if (binding == SymbolTable::Binding::Local) {
		header = _symbol_table.get_local_section_header();
		table_size = _symbol_table.get_local_table_size();
		section_index = _symbol_table.get_local_string_table()->get_section_header_index();
		num_symbols += _symbol_table.get_number_of_local_symbols();
		_symbol_table.get_local_string_table()->debug_print_table();
	} else {
		// Global case
		header = _symbol_table.get_global_section_header();
		table_size = _symbol_table.get_global_table_size();
		section_index = _symbol_table.get_local_string_table()->get_section_header_index();
		num_symbols += _symbol_table.get_number_of_global_symbols();
		_symbol_table.get_global_string_table()->debug_print_table();
	}

	header->s_header.sh_offset = _file_offset;
	header->s_header.sh_size = table_size;
	/* SymbolTables point to their StringTable section */
	header->s_header.sh_link = section_index;
	/* SymbolTables require the `sh_entsize` to indicate the size of the table entries */
	header->s_header.sh_entsize = sizeof(Elf64_Sym);
	/* SymbolTables need to put the local symbols first, then the global symbols
	 * and have the sh_info point to the first non-local symbol.  We need to + 1
	 * here to account for the mandatory SHN_UNDEF symbol that must start the table.
	 */
	header->s_header.sh_info = num_symbols;
	return _symbol_table.write_table_segment(this, binding);
}
// TODO: bool SnapshotImageWriter::writeGlobalSymbolTable()

bool SnapshotImageWriter::writeSectionHeader(SnapshotImageSectionHeader *header)
{
	writeBytes(reinterpret_cast<uint8_t*>(&header->s_header), sizeof(header->s_header));
	return true;
}

void SnapshotImageWriter::writeSectionHeaders(void)
{
	if (_section_headers != nullptr) {
		/* Record the offset for the ELF header */
		_section_header_start_offset = _file_offset;
		SnapshotImageSectionHeader *iterator = _section_headers;
		while (iterator != nullptr) {
			writeSectionHeader(iterator);
			iterator = iterator->next;
		}
	}

}


/* Open image file for writing.
 *
 * @return false on failure to open, true on success
 */
bool SnapshotImageWriter::openFile(void)
{
	_image_file = fopen(_filename, "wb");

	if (NULL == _image_file) {
		printf("[SNAPSHOT] Can't write to %s\n", _filename);
		return false;
	}
	return true;
}

/* Close the image file.
 *
 * @return false on failure, true on success
 */
bool SnapshotImageWriter::closeFile(void)
{
	if (nullptr != _image_file) {
		fflush(_image_file);
		if (0 == fclose(_image_file)) {
			return true;
		}
	}
	return false;
}

/**
 * Write bytes into the file.  Always use this method to write the bytes
 * as it tracks the current file offset.
 */
void SnapshotImageWriter::writeBytes(const uint8_t * buffer, size_t num_bytes, bool update_offset) {
	fwrite(buffer, sizeof(uint8_t), num_bytes, _image_file);
	if (update_offset) {
		_file_offset += num_bytes;
	}
}

void SnapshotImageWriter::reserveHeaderSpace(void)
{
	if (!isFileValid()) {
		return;
	}
	if (0 != fgetpos(_image_file, &_position_elf_header)) {
		/* File is invalid */
		invalidateFile();
		return;
	}
	Elf64_Ehdr header = {0};
	writeBytes(reinterpret_cast<uint8_t*>(&header), sizeof(Elf64_Ehdr));
}

void SnapshotImageWriter::writeHeader(void)
{
	if (!isFileValid()) {
		return;
	}
	/* create local on stack struct and initialize
	 * it before writing it to the file
	 */
	Elf64_Ehdr header;
	header.e_ident[EI_MAG0] = ELFMAG0;
	header.e_ident[EI_MAG1] = ELFMAG1;
	header.e_ident[EI_MAG2] = ELFMAG2;
	header.e_ident[EI_MAG3] = ELFMAG3;
	header.e_ident[EI_CLASS] = ELFCLASS64;
	header.e_ident[EI_VERSION] = EV_CURRENT;
	header.e_ident[EI_ABIVERSION] = 0;
	header.e_ident[EI_DATA] = _is_little_endian ? ELFDATA2LSB : ELFDATA2MSB;

	for (int32_t i = EI_PAD; i < EI_NIDENT; i++) {
		header.e_ident[i] = 0;
	}
	header.e_ident[EI_OSABI] = ELFOSABI_LINUX;
	/* TODO: Support other Linuxes like EM_PPC64, EM_S390, etc */
	header.e_type = ET_DYN;
	header.e_machine = EM_X86_64;
	header.e_version = EV_CURRENT;
	header.e_entry = 0;  //"This member gives the virtual address to which the system first transfers control, thus starting the process."
	header.e_phoff = _program_header_start_offset; // start of the program header table - offset from start of file
	header.e_shoff = _section_header_start_offset; // start of section header table
	header.e_flags = 0;
	header.e_ehsize = sizeof(Elf64_Ehdr);
	header.e_phentsize = sizeof(Elf64_Phdr);
	header.e_phnum = _num_program_headers;
	header.e_shentsize = sizeof(Elf64_Shdr);
	header.e_shnum = _num_section_headers;
	header.e_shstrndx = _index_name_section_header;	// index of section header table entry that contains section names

	if (0 != fsetpos(_image_file, &_position_elf_header)) {
		/* File is invalid */
		invalidateFile();
		return;
	}
	writeBytes(reinterpret_cast<uint8_t*>(&header), sizeof(Elf64_Ehdr), false);
}

/**
 * Look up the index for a string in the static string table (.strtab).
 *
 * The string will be added to the table if it is not yet present and the offset
 * in the table will be returned.
 *
 * The caller is responsible to keep the string alive until the SnapshotImageWriter
 * has completed writing the file.
 *
 * @param str the string to add to the static string table
 * @return the offset in the string table
 */
uint32_t SnapshotImageWriter::get_static_string_table_index(const char *str)
{
	return _static_string_table.get_string_table_index(str);
}

extern "C" void
writeSnapshotImageFile(J9JavaVM *vm)
{
	SnapshotImageWriter::writeSnapshotFile(vm);
}

void SnapshotImageWriter::writeSnapshotFile(J9JavaVM *vm)
{
	SnapshotImageWriter writer("DanTest.image", vm->portLibrary);
	if (writer.openFile()) {
		writer.reserveHeaderSpace();
		// TODO: Write segments
		
		writer.get_static_string_table_index("Foo");
		writer.get_static_string_table_index("Bar");

		SnapshotImageProgramHeader *h = writer.startProgramHeader(PT_LOAD, PF_X | PF_R, 0x1000, 0, 0x1000);
	
		writer.create_global_symbol("GlobalSymbolTest",
			SymbolTable::Type::Notype,
			1, 0x1000, 0);
		writer.create_local_symbol("LocalSymbolTest",
			SymbolTable::Type::Notype,
			1, 0x1040, 0);

		writer.endProgramHeader(h);

		// write the special sections, like the string tables and symbol tables
		// that aren't part of any existing Program Header
		writer.writeStringTable(writer.get_section_header_name_string_table());

		// write the .strtab string table
		writer.writeStringTable(&(writer._static_string_table));

		// write the .symtab static table -> must be written after the .strtab
		writer.writeSymbolTable(SymbolTable::Binding::Local);
		writer.writeStringTable(&(writer._dynamic_string_table));
		writer.writeSymbolTable(SymbolTable::Binding::Global);

		// Write program and section headers at the end of the file
		writer.writeProgramHeaders();
		writer.writeSectionHeaders();

		/* Go back and write the ELF header last so we have all the
		 * info necessary - ie: number of program and section headers -
		 * to fill in the heaader correctly.
		 */
		writer.writeHeader();
		writer.closeFile();
	}

}

StringTable::StringTable(J9PortLibrary *port_lib)
	: _table_size(1)	/* Table must have the initial `\0` in it */
	, _list_head(nullptr)
	, _list_tail(nullptr)
	, _section(nullptr)
{
		_string_table = hashTableNew(OMRPORT_FROM_J9PORT(port_lib),
			"ElfStringTable", /* tableName */
			0, /* table size */
			sizeof(StringTableEntry) /* entry size */,
			0, /* entryAlignment */
			0, /* flags */
			0, /* memoryCategory */
			string_table_hash,
			string_table_equal,
			string_table_print,
			NULL /* userData */
		);
}

StringTable::~StringTable()
{
	if (nullptr != _string_table) {
		hashTableFree(_string_table);
	}
}

uint32_t StringTable::get_string_table_index(const char *str)
{
	if (nullptr == str) {
		/* All emtpy strings will map to the 0th entry */
		return 0;
	}

	StringTableEntry examplar = {0};
	examplar.str = str;
	StringTableEntry *entry = static_cast<StringTableEntry*>(hashTableAdd(_string_table, &examplar));
	if (entry == nullptr) {
		// todo - deal with allocation failure
		printf("StringTable::get_string_table_index allocation failure!  Aborting\n");
		abort();
	}
	if (entry->offset == 0) {
		/* New entry.  Set the:
		 * 	offset to the _table_size
		 *  update the _table_size += num_bytes(entry->str) including the null
		 *  add the entry to the list so we can traverse them in order
		 */
		size_t length = strlen(str) + 1; /* +1 for the null byte */
		entry->offset = _table_size;
		_table_size += length;
		if (_list_head == nullptr) {
			_list_head = entry;
			_list_tail = entry;
		} else {
			_list_tail->next = entry;
			_list_tail = entry;
		}
	}
	return entry->offset;
}

bool StringTable::write_table_segment(SnapshotImageWriter *writer)
{
	// Write the initial null byte
	uint8_t nulByte = '\0';
	writer->writeBytes(&nulByte, 1);

	StringTableEntry *iter = _list_head;
	while (iter != nullptr) {
		writer->writeBytes(reinterpret_cast<const uint8_t*>(iter->str), strlen(iter->str) + 1);
		iter = iter->next;
	}
	return true;
}


static uintptr_t string_table_hash(void *the_entry, void *userData)
{
	StringTableEntry *entry = static_cast<StringTableEntry*>(the_entry);

	/* Implementation of the elf_hash hashing function */
	uintptr_t hash = 0;
	const char *cursor = entry->str;
	while ('\0' != *cursor) {
		hash = (hash << 4) + *cursor++;
		uintptr_t high = hash & 0xF0000000;
		if (high != 0) {
			hash ^= high >> 24;
		}
		hash &= ~high;
	}
	return hash;

}
static uintptr_t string_table_equal(void *leftEntry, void *rightEntry, void *userData)
{
	StringTableEntry *left = static_cast<StringTableEntry*>(leftEntry);
	StringTableEntry *right = static_cast<StringTableEntry*>(rightEntry);

	if (0 == strcmp(left->str, right->str)) {
		return 1;
	}
	return 0;
}

static void string_table_print(OMRPortLibrary *portLibrary, void *the_entry, void *userData)
{
	StringTableEntry *entry = static_cast<StringTableEntry*>(the_entry);
	printf("{.str: '%s', .offset: %" PRIu32 " .next: %p} \n", entry->str, entry->offset, entry->next);
}

static uintptr_t string_table_do_print(void *the_entry, void *userData)
{
	StringTableEntry *entry = static_cast<StringTableEntry*>(the_entry);
	printf("{.str: '%s', .offset: %" PRIu32 " .next: %p} \n", entry->str, entry->offset, entry->next);
	/* Don't modify the table - just print it */
	return 0;
}

void StringTable::debug_print_table()
{
	int index = -1;
	if (_section != nullptr) {
		index = (int)_section->index;
	}
	printf("[String Table: header_index=%d]\n", index);
	hashTableForEachDo(_string_table, string_table_do_print, nullptr);
	printf("[======= String Table =======]\n");
}

/**
 * Create a SymbolTable that stores its strings in `string_table` and allocates
 * its backing storage (a J9Pool) with the `port_lib`
 */
SymbolTable::SymbolTable(StringTable *local_strings, StringTable *global_strings, J9PortLibrary *port_lib)
	: _local_strings(local_strings)
	, _global_strings(global_strings)
	, _num_local_symbols(0)
	, _num_global_symbols(0)
	, _port_lib(port_lib)
	, _symbols(nullptr)
	, _local_section(nullptr)
	, _global_section(nullptr)
{
	_symbols = pool_new(
		sizeof(SymbolTableEntry),
		0, /* minNumElements */
		0, /* elementAlignment */
		0, /* flags */
		"ElfSymbolTable", /* callsite */
		0, /* memoryCategory */
		POOL_FOR_PORT(_port_lib)
	);

	/* Don't create an STN_UNDEF symbol as we'll add
	 * it when writing the table.
	 */
}
/*
typedef struct {
	uint32_t      st_name;
	unsigned char st_info;
	unsigned char st_other;
	uint16_t      st_shndx;
	Elf64_Addr    st_value;
	uint64_t      st_size;
} Elf64_Sym;
*/
SymbolTableEntry * SymbolTable::create_symbol(const char *name, Binding binding, Type type, Visibility visibility, uint16_t sectionIndex, uintptr_t value, uint64_t size)
{

	SymbolTableEntry *entry = static_cast<SymbolTableEntry *>(pool_newElement(_symbols));
	if (entry == nullptr) {
		return nullptr;
	}
	memset(entry, 0, sizeof(SymbolTableEntry));

	/* Track a string index in both the local and the global string
	 * tables as they indexes won't be the same.  We'll write the
	 * correct one into the entry when writing the table
	 */
	if ((binding == Global) || (binding == Weak)) {
		entry->globalStringIndex = _global_strings->get_string_table_index(name);
		_num_global_symbols += 1;
	} else {
		_num_local_symbols += 1;
	}
	entry->localStringIndex = _local_strings->get_string_table_index(name);

	entry->symbol.st_name = 0;	// Needs to be filled in when written and depends on the type of the symbol
	entry->symbol.st_info = st_info(binding, type);
	entry->symbol.st_other = st_other(visibility);
	entry->symbol.st_shndx = sectionIndex;
	entry->symbol.st_value = value;	// Usually the virtual address for this section
	entry->symbol.st_size = size;

	return entry;
}

typedef struct SymbolTableUserData {
	SnapshotImageWriter *writer;
	SymbolTable::Binding binding;
} SymbolTableUserData;

/* Binding must be one of {Binding::Local or Binding::Global} to determine which table
 * to write
 */
bool SymbolTable::write_table_segment(SnapshotImageWriter *writer, Binding binding)
{
	/* Write the SHN_UNDEF symbol to start the table */
	Elf64_Sym shn_undef = {0};
	shn_undef.st_shndx = SHN_UNDEF;
	writer->writeBytes(reinterpret_cast<const uint8_t*>(&shn_undef), sizeof(shn_undef));
	SymbolTableUserData userData;
	userData.writer = writer;
	switch(binding) {
	case Local:
		userData.binding = Local;
		pool_do(_symbols, writeSymbolTableEntry, &userData);
		/* Fallthrough */
	case Global:
	case Weak:
		userData.binding = Global;
		pool_do(_symbols, writeSymbolTableEntry, &userData);
		break;
	default:
		/* Unreachable */
		assert(false);
	}
	return true;
}

void SymbolTable::writeSymbolTableEntry(void *anElement, void *userData)
{
	SymbolTableUserData *data = static_cast<SymbolTableUserData *>(userData);
	SymbolTableEntry *entry = static_cast<SymbolTableEntry *>(anElement);

	if (data->binding == binding(entry->symbol.st_info)) {
		SnapshotImageWriter *writer = data->writer;
		if (data->binding == Local) {
			entry->symbol.st_name = entry->localStringIndex;
		} else {
			entry->symbol.st_name = entry->globalStringIndex;
		}
		writer->writeBytes(reinterpret_cast<const uint8_t*>(&entry->symbol), sizeof(entry->symbol));
	}
}

// TODO - API: Write the section header and connect it to the symbol table

int32_t SymbolTable::get_number_of_local_symbols(void)
{
	return _num_local_symbols;
}

int32_t SymbolTable::get_number_of_global_symbols(void)
{
	return _num_global_symbols;
}

/**
 * Destroy the SymbolTable and free the J9Pool that backs it.
 */
SymbolTable::~SymbolTable()
{
	if (nullptr != _symbols) {
		pool_kill(_symbols);
	}
}


DynamicTable::DynamicTable(J9PortLibrary *port_lib)
	: _dyamic_program_header(nullptr)
	, _dyamic_section_header(nullptr)
	, _entries(nullptr)
{
	_entries = pool_new(
		sizeof(DynamicTableEntry),
		0, /* minNumElements */
		0, /* elementAlignment */
		0, /* flags */
		"ElfDynamicTable", /* callsite */
		0, /* memoryCategory */
		POOL_FOR_PORT(port_lib)
	);
}

DynamicTable::~DynamicTable()
{
	if (nullptr != _entries) {
		pool_kill(_entries);
	}
}

DynamicTableEntry * DynamicTable::add_entry(int64_t tag, SnapshotImageSectionHeader *header)
{
	DynamicTableEntry *entry = static_cast<DynamicTableEntry*>(pool_newElement(_entries));
	if (nullptr != entry) {
		entry->entry.d_tag = tag;
		entry->definingSection = header;
	}
	return entry;
}

void DynamicTable::finalizeEntries() {
	pool_do(_entries, finalizeDynamicTableEntry, nullptr);
}

void DynamicTable::finalizeDynamicTableEntry(void *anElement, void *userData)
{
	DynamicTableEntry *entry = static_cast<DynamicTableEntry *>(anElement);

	switch(entry->entry.d_tag) {
	case DT_HASH:
	case DT_STRTAB:
	case DT_SYMTAB:
		/* point to virtual address of hash table in memory.
		 * No relocation required, loader does it for us
		 */
		entry->entry.d_un.d_ptr = entry->definingSection->s_header.sh_addr;
		break;
	case DT_SYMENT:
		/* Size of a symbol table entry */
		entry->entry.d_un.d_val = sizeof(Elf64_Sym);
		break;
	case DT_STRSZ:
		/* Size of the string table: only known after string table written */
		entry->entry.d_un.d_ptr = entry->definingSection->s_header.sh_size;
		break;
	default:
		printf("Unknown Dynamic entry type: %" PRIu64 " with header=%p\n", entry->entry.d_tag, entry->definingSection);
		printSectionHeader(entry->definingSection);
	}
}

bool DynamicTable::write_table_segment(SnapshotImageWriter *writer)
{
	pool_do(_entries, writeDynamicTableEntry, writer);
	/* Table is terminated by a null entry */
	Elf64_Dyn dt_null = {0};
	writer->writeBytes(reinterpret_cast<const uint8_t*>(&dt_null), sizeof(dt_null));
	return true;
}

void DynamicTable::writeDynamicTableEntry(void *anElement, void *userData)
{
	SnapshotImageWriter *writer = static_cast<SnapshotImageWriter *>(userData);
	DynamicTableEntry *entry = static_cast<DynamicTableEntry *>(anElement);

	writer->writeBytes(reinterpret_cast<const uint8_t*>(&entry->entry), sizeof(entry->entry));
}