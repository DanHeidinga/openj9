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

#include "SnapshotImageWriter.hpp"
#include "vm_api.h"

SnapshotImageWriter::SnapshotImageWriter(const char *filename, bool is_little_endian) :
	_filename(filename),
	_is_little_endian(is_little_endian),
	_image_file(nullptr),
	_num_program_headers(0),
	_num_section_headers(0),
	_index_name_section_header(0),
	_program_header_start_offset(0),
	_section_header_start_offset(0),
	_file_offset(0),
	_is_invalid(false),
	_program_headers(nullptr),
	_program_headers_tail(nullptr)
{
	printf("SnapshotImageWriter\n");
}

SnapshotImageWriter::~SnapshotImageWriter()
{
	printf("Deleting SnapshotImageWriter\n");
}

// TODO
// Need an abstraction for a Segment and for sections in that segement
#if 0
Elf64_Phdr *_programHeader;   /**< The ELFProgramHeader, required for executable ELF */

    /**< The section headers and the sectionheader names */
Elf64_Shdr *_zeroSection;
#endif


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
	SnapshotImageProgramHeader *header = (SnapshotImageProgramHeader *)malloc(sizeof(SnapshotImageProgramHeader));
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
/* Sections are contained within ProgramHeaders (mostly) */
SnapshotImageSectionHeader* startSectionHeader(SnapshotImageProgramHeader *programHeader);
void endSectionHeader(SnapshotImageSectionHeader *sectionHeader;



/* SECTIONS
typedef struct {
        Elf64_Word      sh_name;
        Elf64_Word      sh_type;
        Elf64_Xword     sh_flags;
        Elf64_Addr      sh_addr;
        Elf64_Off       sh_offset;
        Elf64_Xword     sh_size;
        Elf64_Word      sh_link;
        Elf64_Word      sh_info;
        Elf64_Xword     sh_addralign;
        Elf64_Xword     sh_entsize;
} Elf64_Shdr;
*/

#endif

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
void SnapshotImageWriter::writeBytes(uint8_t * buffer, size_t num_bytes, bool update_offset) {
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

extern "C" void
writeSnapshotImageFile(J9JavaVM *vm)
{
	SnapshotImageWriter writer("DanTest.image");
	if (writer.openFile()) {
		writer.reserveHeaderSpace();
		// TODO: Write segments
		
		SnapshotImageProgramHeader *h = writer.startProgramHeader(PT_LOAD, PF_X | PF_R, 0x1000, 0, 0x1000);
		writer.endProgramHeader(h);
		
		// Write program and section headers at the end of the file
		writer.writeProgramHeaders();
		// TODO: section headers

		/* Go back and write the ELF header last so we have all the
		 * info necessary - ie: number of program and section headers -
		 * to fill in the heaader correctly.
		 */
		writer.writeHeader();
		writer.closeFile();
	}

}