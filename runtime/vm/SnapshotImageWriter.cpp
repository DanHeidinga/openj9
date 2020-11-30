/*******************************************************************************
 * Copyright (c) 2001, 2020 IBM Corp. and others
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

#include <elf.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "hashtable_api.h"
#include "SnapshotImageWriter.hpp"
#include "vm_api.h"

static uintptr_t string_table_hash(void *entry, void *userData);
static uintptr_t string_table_equal(void *leftEntry, void *rightEntry, void *userData);
static void string_table_print(OMRPortLibrary *portLibrary, void *entry, void *userData); 

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
	_num_section_headers += 1;

	/**
	 * Create the section header string table ("shstrtab") section as its needed to name
	 * the other sections
	 */
	_section_header_string_table_header = allocateSectionHeader(SHT_STRTAB, ".shstrtab");
	_index_name_section_header = _num_section_headers;
	append_to_section_header_list(_section_header_string_table_header);
	_section_header_name_string_table.set_section_header(_section_header_string_table_header);

	/* Create section header for the static string table (".strtab") */
	_static_string_table_header = allocateSectionHeader(SHT_STRTAB, ".strtab");
	append_to_section_header_list(_static_string_table_header);
	_static_string_table.set_section_header(_static_string_table_header);
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
	if (_num_section_headers > 0) {
		/* TODO: free the section headers */
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



SnapshotImageSectionHeader* SnapshotImageWriter::allocateSectionHeader(uint32_t type, const char *section_name) {
	SnapshotImageSectionHeader *header = static_cast<SnapshotImageSectionHeader *>(malloc(sizeof(SnapshotImageSectionHeader)));
	if (header == nullptr) {
		invalidateFile();
		return nullptr;
	}
	memset(header, 0, sizeof(*header));
	header->s_header.sh_type = type;
	header->s_header.sh_name = _section_header_name_string_table.get_string_table_index(section_name);
	return header;
}

void SnapshotImageWriter::append_to_section_header_list(SnapshotImageSectionHeader* header)
{
	/* List will always have th zero section header in it first so we can unconditionally
	 * add to the tail without needing to check the head
	 */
	_section_headers_tail->next = header;
	_section_headers_tail = header;
	_num_section_headers += 1;
}

/**
 * Create the SECTION HEADER string table, which will contain the names of new sections.
 */
SnapshotImageSectionHeader* SnapshotImageWriter::createSectionHeaderStringTableSection(void)
{
	if (_section_header_string_table_header == nullptr) {
		SnapshotImageSectionHeader *header = allocateSectionHeader(SHT_STRTAB, ".shstrtab");
		_index_name_section_header = _num_section_headers;
		_section_header_string_table_header = header;
		append_to_section_header_list(header);
	}
	return _section_header_string_table_header;
}

bool SnapshotImageWriter::writeStringTable(StringTable *table)
{
	SnapshotImageSectionHeader *header = table->get_section_header();
	header->s_header.sh_offset = _file_offset;
	header->s_header.sh_size = table->get_table_size();
	return table->write_table_segment(this);
}

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
uint64_t SnapshotImageWriter::get_static_string_table_index(const char *str)
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
		writer.endProgramHeader(h);
		
		// write the special sections, like the string tables and symbol tables
		// that aren't part of any existing Program Header
		writer.writeStringTable(writer.get_section_header_name_string_table());

		// write the .strtab string table
		writer.writeStringTable(&(writer._static_string_table));

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

/*
J9HashTable *
hashTableNew(
	OMRPortLibrary *portLibrary,
	const char *tableName,
	uint32_t tableSize,
	uint32_t entrySize,
	uint32_t entryAlignment,
	uint32_t flags,
	uint32_t memoryCategory,
	J9HashTableHashFn hashFn,
	J9HashTableEqualFn hashEqualFn,
	J9HashTablePrintFn printFn,
	void *functionUserData);
*/
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
			NULL);
}

StringTable::~StringTable()
{
	if (nullptr != _string_table) {
		hashTableFree(_string_table);
	}
}

uint64_t StringTable::get_string_table_index(const char *str)
{
	if (nullptr == str) {
		/* All emtpy strings will map to the 0th entry */
		return 0;
	}

	StringTableEntry examplar = {0};
	examplar.str = str;
	StringTableEntry *entry = static_cast<StringTableEntry*>(hashTableAdd(_string_table, &examplar));
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
		return 0;
	}
	return 1;
}

static void string_table_print(OMRPortLibrary *portLibrary, void *the_entry, void *userData)
{
	StringTableEntry *entry = static_cast<StringTableEntry*>(the_entry);
	printf("{.str: '%s', .offset: %" PRIu64 " .next: %p} \n", entry->str, entry->offset, entry->next);
}