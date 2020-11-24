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

#include "j9cfg.h"
#include <elf.h>
#include <stdio.h>

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

protected:
public:

	/*
	 * Function Members
	 */
private:
protected:
	void invalidateFile(void) { _is_invalid = true; }
	bool isFileValid(void) { return !_is_invalid; };
	void writeBytes(uint8_t *buffer, size_t num_bytes, bool update_offset = true);

	bool writeNULLSectionHeader();
	bool writeShstrtabSectionHeader(void);

public:
	SnapshotImageWriter(const char* filename, bool isLittleEndian = true);
	~SnapshotImageWriter();

	bool openFile(void);
	bool closeFile(void);
	void reserveHeaderSpace(void);
	void writeHeader(void);

	SnapshotImageProgramHeader* startProgramHeader(uint32_t type, uint32_t flags, Elf64_Addr vaddr, Elf64_Addr paddr, uint64_t align) ;
	void endProgramHeader(SnapshotImageProgramHeader *programHeader, uint64_t extraMemSize = 0);
	void writeProgramHeaders(void);
	void writeSectionHeaders(void);

};

#endif /* SNAPSHOTIMAGEWRITER_HPP_ */
