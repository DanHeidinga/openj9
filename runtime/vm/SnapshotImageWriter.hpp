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

#ifndef SNAPSHOTIMAGEWRITER_HPP_
#define SNAPSHOTIMAGEWRITER_HPP_

#include <elf.h>
#include <stdio.h>

#include "j9cfg.h"
#include "vm_api.h"

typedef struct SnapshotImageSectionHeader {
		Elf64_Shdr s_header;
		char *name;
		SnapshotImageSectionHeader *next;
} SnapshotImageSectionHeader;

typedef struct SnapshotImageProgramHeader {
		Elf64_Phdr p_header;
		SnapshotImageProgramHeader *next;
		SnapshotImageSectionHeader *sections;
		int64_t numSections;
} SnapshotImageProgramHeader;

typedef struct StringTableEntry {
	const char *str;
	/* Used to chain the entries in order for writing them out */
	StringTableEntry *next;
	uint64_t offset;
} StringTableEntry;

/* Forward declaration */
class SnapshotImageWriter;

/*
 * The string table will write data into the file
 * with the following format:
 * 		\0AA\0BB\0
 * And Strings with indexes:
 * 0 - \0  "none"
 * 1 - AA
 * 5 - BB
 * 7 - null string
 *
 */
class StringTable {
	/**
	 * Data members:
	 */
private:
	int64_t _table_size;

	/* Table to map entries to their offset */
	J9HashTable *_string_table;

	/* List used to write out the entries in the table
	 * so we can traverse them in the correct order
	 */
	StringTableEntry *_list_head;
	StringTableEntry *_list_tail;

protected:
public:

	/**
	 * Function members:
	 */
private:
protected:
public:

	/**
	 * Construct a new StringTable.
	 *
	 * @param portLib a J9PortLibrary used to allocate / construct in the hashtable
	 */
	StringTable(J9PortLibrary *portLib);

	/**
	 * Descructor: free the hashtable backing the string table.
	 */
	~StringTable();

	/**
	 * Find the index for the `str`.  If `str` is
	 * already in the table, then return the existing
	 * index.  Otherwise, add the string to the table
	 * and then return its index
	 *
	 * @param str the string to get an index for
	 * @return the index (offset from the start) of the string table
	 */
	int64_t get_string_table_index(const char *str);

	/**
	 * Return the size of the table.  The size is
	 * as calculated as the segment size for the elf
	 * file.
	 *
	 * For a table with the strings "AA" && "BB", the
	 * size would be 7.
	 *
	 * @return the segment size of the table.
	 */
	int64_t get_table_size() { return _table_size; }

	/**
	 * Write the table using the provided writer
	 *
	 * @return true if successful, false on failure
	 */
	bool write_table_segment(SnapshotImageWriter *writer);
};

class SnapshotImageWriter
{

	/*
	 * Data Members
	 */
private:
	const char* _filename;
	bool _is_little_endian;

	::FILE *_image_file;

	/* Record where the ELFHeader is in the file */
	fpos_t _position_elf_header;

	/* Number of program headers */
	int32_t _num_program_headers;
	int32_t _num_section_headers;
	int32_t _index_name_section_header;
	int32_t _program_header_start_offset;
	int32_t _section_header_start_offset;
	int64_t _file_offset;

	/* Flag to indicate file writing failed or the file
	 * being generated will be invalid in some way
	 */
	bool _is_invalid;

	SnapshotImageProgramHeader *_program_headers;
	SnapshotImageProgramHeader *_program_headers_tail;

	/* Some sections are not contained in a program header, they
	 * just a default part of the file.
	 *
	 * This list is to track those sections:
	 *	* zero (NULL) section
	 *	* string table (SHT_STRTAB)
	 */
	SnapshotImageSectionHeader *_header_sections;
	SnapshotImageSectionHeader *_header_sections_tail;

	J9PortLibrary *_port_lib;

	StringTable _static_string_table;
	//StringTable _dynamic_string_table;

protected:
public:

	/*
	 * Function Members
	 */
private:
protected:
	void invalidateFile(void) { _is_invalid = true; }
	bool isFileValid(void) { return !_is_invalid; };
	void writeBytes(const uint8_t *buffer, size_t num_bytes, bool update_offset = true);

	bool writeNULLSectionHeader();
	//bool writeShstrtabSectionHeader(void);
	SnapshotImageSectionHeader* createShstrtabSectionHeader(void);
	bool writeStringTable(SnapshotImageSectionHeader *header, StringTable *table);
	bool writeSectionHeader(SnapshotImageSectionHeader *header);
	StringTable* get_static_string_table(void) { return &_static_string_table; }

public:
	SnapshotImageWriter(const char* filename, J9PortLibrary *portLib, bool isLittleEndian = true);
	~SnapshotImageWriter();

	bool openFile(void);
	bool closeFile(void);
	void reserveHeaderSpace(void);
	void writeHeader(void);

	SnapshotImageProgramHeader* startProgramHeader(uint32_t type, uint32_t flags, Elf64_Addr vaddr, Elf64_Addr paddr, uint64_t align) ;
	void endProgramHeader(SnapshotImageProgramHeader *programHeader, uint64_t extraMemSize = 0);
	void writeProgramHeaders(void);
	void writeSectionHeaders(void);

	static void writeSnapshotFile(J9JavaVM *vm);
friend StringTable;
};

#endif /* SNAPSHOTIMAGEWRITER_HPP_ */
