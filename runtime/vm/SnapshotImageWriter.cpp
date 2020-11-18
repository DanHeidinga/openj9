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

#include "elf.h"
#include "stdio.h"
#include "string.h"

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
	_is_invalid(false)
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

#if 0
/* PROGRAM HEADER
typedef struct {
        Elf64_Word      p_type;
        Elf64_Word      p_flags;
        Elf64_Off       p_offset;
        Elf64_Addr      p_vaddr;
        Elf64_Addr      p_paddr;
        Elf64_Xword     p_filesz;
        Elf64_Xword     p_memsz;
        Elf64_Xword     p_align;
} Elf64_Phdr;
*/
SnapshotImageProgramHeader* startProgramHeader();
void endProgramHeader(SnapshotImageProgramHeader *programHeader);

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
	fwrite(&header, sizeof(uint8_t), sizeof(Elf64_Ehdr), _image_file);
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
	fwrite(&header, sizeof(uint8_t), sizeof(Elf64_Ehdr), _image_file);
}

extern "C" void
writeSnapshotImageFile(J9JavaVM *vm)
{
	SnapshotImageWriter writer("DanTest.image");
	if (writer.openFile()) {
		writer.reserveHeaderSpace();
		// Write other parts of the file
		writer.writeHeader();
		writer.closeFile();
	}

}