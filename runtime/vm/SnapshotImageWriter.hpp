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
		/* global index in the section table header section,
		 * this applies across program section groups
		 */
		uint64_t index;
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
	uint32_t offset;
	uint32_t hash;
} StringTableEntry;

typedef struct SymbolTableEntry {
	Elf64_Sym symbol;
	uint32_t localStringIndex;
	uint32_t globalStringIndex;
	/* Index of the symbol in the symbol table */
	uint32_t globalSymbolIndex;
	/* Intended for chaining through the symbols to
	 * create the hashtable chains
	 */
	SymbolTableEntry *hash_chain;
} SymbolTableEntry;

typedef void (*symbol_do_callback)(SymbolTableEntry *, void *);

typedef struct DynamicTableEntry {
	Elf64_Dyn entry;
	SnapshotImageSectionHeader *definingSection;
} DynamicTableEntry;

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
	int32_t _table_size;

	/* Table to map entries to their offset */
	J9HashTable *_string_table;

	/* List used to write out the entries in the table
	 * so we can traverse them in the correct order
	 */
	StringTableEntry *_list_head;
	StringTableEntry *_list_tail;

	/* Section containing the StringTable */
	SnapshotImageSectionHeader *_section;

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
	 * Set the section header related to this string table
	 *
	 * @param header the SnapshotImageSectionHeader pointer representing this string table.
	 * @return void
	 */
	void set_section_header(SnapshotImageSectionHeader *header) { _section = header;}

	/**
	 * Get the section header related to this string table
	 *
	 *  @return the SnapshotImageSectionHeader pointer representing this string table.
	 */
	SnapshotImageSectionHeader * get_section_header(void) { return _section;}

	/**
	 * Get the index for the section header or 0 if no header.
	 * 
	 * @return the section header index
	 */
	uint32_t get_section_header_index() {
		if (_section != nullptr) {
			return _section->index;
		}
		return 0;
	}

	/**
	 * Find the index for the `str`.  If `str` is
	 * already in the table, then return the existing
	 * index.  Otherwise, add the string to the table
	 * and then return its index
	 *
	 * @param str the string to get an index for
	 * @return the index (offset from the start) of the string table
	 */
	uint32_t get_string_table_index(const char *str);

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
	int32_t get_table_size() { return _table_size; }

	/**
	 * Write the table using the provided writer
	 *
	 * @return true if successful, false on failure
	 */
	bool write_table_segment(SnapshotImageWriter *writer);

	/**
	 * Calculate the hash for the string at `index`.
	 *
	 * @param index the index into the string table
	 * @return the hash (or 0 for non-existant) for string at index
	 */
	uint32_t hash_for_string(uint32_t index);

	/**
	 * Debug API to print the table to stdout.
	 */
	void debug_print_table();

};

class SymbolTable;	// Forward declaration

/**
 * Implement the HashTable used for the .hash
 * section of the .dynamic section.  This speeds up the
 * lookup of global symbols by the Elf loader.
 *
 * Layout of the table in memory is:
 * -----------------
 * | num_buckets
 * | length of symbol table (chain length)
 * | bucket[0]
 * | bucket[...]
 * | bucket[n]
 * | chain[0]
 * | chain[1]
 * | chain[...]
 * | chain[n]
 * -----------------
 * bucket[i] is the start of a chain, where i = hash(sym) % num_buckets
 * then lookup symbol[i].  If not a match, next symbol will be chain[i].
 * Note, chain will be the same length as the SymbolTable.
 *
 */
class SymbolHashTable {
/**
 * Data Members
 */
private:
	uint32_t _num_buckets;
	SymbolTableEntry **_buckets;

	SymbolTable *_symbols;
	SnapshotImageSectionHeader *_header;
protected:
public:

/**
 * Function Members
 */
private:

	void calculate_buckets_and_chains();

	static void build_hash_chains(SymbolTableEntry *entry, void *data);
	uint32_t hash(SymbolTableEntry *entry);
	static void write_next_index(SymbolTableEntry *entry, void *data);

protected:
public:
	SymbolHashTable(SymbolTable *symbols, SnapshotImageSectionHeader *header);
	~SymbolHashTable();

	bool write_table_segment(SnapshotImageWriter *writer);
};


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
class SymbolTable
{
/**
 * Data Members
 */
private:
	/* There are two string tables as the global strings,
	 * those associated with the global symbols, are a
	 * strict subset of the local strings which will also
	 * include the names of local symbols.
	 *
	 * The local strings map to the ".strtab" section and
	 * the global strings to the ".dynstr" section.
	 */
	StringTable *_local_strings;
	StringTable *_global_strings;

	/* Keep two separate counts so we can correctly write
	 * out the counts when writing the separate symbol
	 * tables.
	 *
	 * Invariant: total count is always 1 great than these
	 * counts as the tables must include a "SHN_UNDEF" 
	 * symbol as the 0th entry.
	 */
	int32_t _num_local_symbols;
	int32_t _num_global_symbols;

	/* Required to allocate the Pool */
	J9PortLibrary *_port_lib;

	/* The pool of symbols in this table.  There are
	 * linked lists that thread through these symbols
	 * to track the global vs local symbols
	 */
	J9Pool *_symbols;

	/* Section containing the SymbolTable */
	SnapshotImageSectionHeader *_local_section;
	SnapshotImageSectionHeader *_global_section;

protected:
public:

enum Binding {
	Local = STB_LOCAL,
	Global = STB_GLOBAL,
	Weak = STB_WEAK
};

enum Type {
	Notype = STT_NOTYPE,
	Object = STT_OBJECT,
	Func = STT_FUNC,
	Section = STT_SECTION,
	File = STT_FILE,
	Common = STT_COMMON,
	Tls = STT_TLS
};

enum Visibility {
	Default = STV_DEFAULT,
	Internal = STV_INTERNAL,
	Hidden = STV_HIDDEN,
	Protected = STV_PROTECTED
};

/**
 * Function members
 */
private:

	/**
	 * Encode the Binding and Type so it can be stored in the
	 * `st_info` Symbol field.
	 *
	 * @param b the binding value
	 * @param t the Type of the symbol
	 * @return the encoded char
	 */
	static unsigned char st_info(Binding b, Type t) {
		return (b << 4) + (t & 0xf);
	}

	static Binding binding(unsigned char st_info) {
		switch(st_info >> 4) {
		case Local:		return Local;
		case Global:	return Global;
		case Weak:		return Weak;
		}
		return Local;
	}

	/**
	 * Encode the visibility so it can be stored in the
	 * `st_other` Symbol field.
	 *
	 * @param v The visibiilty
	 * @return the encoded char
	 */
	static unsigned char st_other(Visibility v) {
		return v & 0x3;
	}

	/* Callback used by the J9Pool pool_do() to write out
	 * each element of the SymbolTable
	 */
	static void writeSymbolTableEntry(void *anElement, void *userData);

	/**
	 * Private callback function used to drive the pool_do()
	 * iteration for the do() call.
	 * @param anElement a SymbolTableEntry pointer
	 * @param userData SymbolTableDoState pointer
	 */
	static void iterator(void *anElement, void *userData);

protected:
public:
	SymbolTable(StringTable *local_strings, StringTable *global_strings, J9PortLibrary *port_lib);
	~SymbolTable();

	/**
	 * Return the total number of symbols in the table.
	 *
	 * @return the number of symbols in the table
	 */
	int32_t get_number_of_symbols();

	/**
	 * Return the number of local symbols in the table.
	 *
	 * @return the number of local symbols
	 */
	int32_t get_number_of_local_symbols(void);

	/**
	 * Return the number of global/weak symbols in the table.
	 *
	 * @return the number of global/weak symbols
	 */
	int32_t get_number_of_global_symbols(void);

	SymbolTableEntry * create_symbol(const char *name, Binding binding, Type type, Visibility visibility, uint16_t sectionIndex, uintptr_t value, uint64_t size);

	/**
	 * Write the table using the provided writer.
	 *
	 * @param writer The image writer
	 * @param binding Binding::Global or Local to indicate which view of the table to write
	 *
	 * @return true if successful, false on failure
	 */
	bool write_table_segment(SnapshotImageWriter *writer, Binding binding);

	/**
	 * Set the section header related to this string table for local symbol table
	 *
	 * @param header the SnapshotImageSectionHeader pointer representing this string table.
	 * @return void
	 */
	void set_local_section_header(SnapshotImageSectionHeader *header) { _local_section = header; }

	/**
	 * Set the section header related to this string table for global symbol table
	 *
	 * @param header the SnapshotImageSectionHeader pointer representing this string table.
	 * @return void
	 */
	void set_global_section_header(SnapshotImageSectionHeader *header) { _global_section = header; }

	/**
	 * Get the local section header related to this string table
	 *
	 *  @return the SnapshotImageSectionHeader pointer representing this string table.
	 */
	SnapshotImageSectionHeader * get_local_section_header(void) { return _local_section; }

	/**
	 * Get the global section header related to this string table
	 *
	 *  @return the SnapshotImageSectionHeader pointer representing this string table.
	 */
	SnapshotImageSectionHeader * get_global_section_header(void) { return _global_section; }

	/**
	 * Get the index for the local section header or 0 if no header.
	 *
	 * @return the section header index
	 */
	uint32_t get_local_section_header_index() {
		if (_local_section != nullptr) {
			return _local_section->index;
		}
		return 0;
	}

	/**
	 * Get the index for the global section header or 0 if no header.
	 *
	 * @return the section header index
	 */
	uint32_t get_global_section_header_index() {
		if (_global_section != nullptr) {
			return _global_section->index;
		}
		return 0;
	}

	/**
	 * Return the size of the local table.  The size is
	 * as calculated as the segment size for the elf
	 * file.
	 *
	 * For a SymbolTable, this is sizeof(Elf64_Sym) * numSymbols
	 *
	 * @return the segment size of the table.
	 */
	int64_t get_local_table_size() { return sizeof(Elf64_Sym) * (_num_local_symbols + _num_global_symbols + 1); }

	/**
	 * Return the size of the global table.  The size is
	 * as calculated as the segment size for the elf
	 * file.
	 *
	 * For a SymbolTable, this is sizeof(Elf64_Sym) * numSymbols
	 *
	 * @return the segment size of the table.
	 */
	int64_t get_global_table_size() { return sizeof(Elf64_Sym) * (_num_global_symbols + 1); }

	/**
	 * Return the StringTable used for local strings in this SymbolTable
	 *
	 * @return a pointer to a StringTable
	 */
	StringTable *get_local_string_table(void) { return _local_strings; }

	/**
	 * Return the StringTable used for local strings in this SymbolTable
	 *
	 * @return a pointer to a StringTable
	 */
	StringTable *get_global_string_table(void) { return _global_strings; }

	/**
	 * Iterate over the SymbolTableElements with the a matching `binding`.
	 *
	 * @param callback the per-element callback function
	 * @param userData data passed to the callback
	 * @param binding the types of symbols to pass to the callback: Global or Local
	 */
	void do_symbols(symbol_do_callback callback, void *userData, Binding binding);
};

/* Represents the "PT_DYANMIC" Program Header and the ".dynamic" section
 * as well as all the required / associated data.
 */
class DynamicTable {
/**
 * Data Members
 */
private:
	/* Must be of type PT_DYNAMIC */
	SnapshotImageProgramHeader *_dyamic_program_header;
	SnapshotImageSectionHeader *_dyamic_section_header;

	/* DynamicTableEntry items to include in the dynamic section */
	J9Pool *_entries;

/* DT_NULL
 * DT_STRTAB	// string table section header
 * DT_STRSZ		// size of string table
 * DT_SYMTAB	// symbol table
 * DT_HASH		// hash for it
 * DT_SYMENT	// constant data
 */
protected:
public:

/**
 * Function Members
 */
private:
	void finalizeEntries();
	static void finalizeDynamicTableEntry(void *anElement, void *userData);
	static void writeDynamicTableEntry(void *anElement, void *userData);
protected:
public:
	DynamicTable(J9PortLibrary *port_lib);
	~DynamicTable();

	DynamicTableEntry *add_entry(int64_t tag, SnapshotImageSectionHeader *header);

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
	 *	* section header string table (SHT_STRTAB)
	 */
	SnapshotImageSectionHeader *_section_headers;
	SnapshotImageSectionHeader *_section_headers_tail;

	J9PortLibrary *_port_lib;

	/* Section Header string table (SHT_STRTAB): .shstrtab */
	StringTable _section_header_name_string_table;
	SnapshotImageSectionHeader * _section_header_string_table_header;

	/* Static string table (SHT_STRTAB): .strtab */
	StringTable _static_string_table;
	SnapshotImageSectionHeader * _static_string_table_header;

	/* Dynamic string table (SHT_STRTAB): .dynstr */
	StringTable _dynamic_string_table;
	SnapshotImageSectionHeader * _dynamic_string_table_header;

	/* Single symbol table that manages both local and global symbols */
	SymbolTable _symbol_table;

	SnapshotImageSectionHeader * _hash_table_header;

protected:
public:

	/*
	 * Function Members
	 */
private:

	/* Private API that allocates a new section but doesn't increment the section counters, etc */
	SnapshotImageSectionHeader* allocateSectionHeader(uint32_t type, const char *section_name);

	/* Adds to the section_header_list for sections that aren't contained in a program header */
	void append_to_section_header_list(SnapshotImageSectionHeader* header);

	/* Add a string to the section header string table which records the names of sections.
	 * Caller is responsible to keep the string alive for the entirity of the image write.
	 */
	uint64_t add_section_header_name(const char* str);

protected:
	void invalidateFile(void) { _is_invalid = true; }
	bool isFileValid(void) { return !_is_invalid; };
	void writeBytes(const uint8_t *buffer, size_t num_bytes, bool update_offset = true);

	bool writeStringTable(StringTable *table);
	bool writeSymbolTable(SymbolTable *table);
	bool writeSectionHeader(SnapshotImageSectionHeader *header);
	StringTable* get_section_header_name_string_table(void) { return &_section_header_name_string_table; }

public:
	SnapshotImageWriter(const char* filename, J9PortLibrary *portLib, bool isLittleEndian = true);
	~SnapshotImageWriter();

	bool openFile(void);
	bool closeFile(void);
	void reserveHeaderSpace(void);
	void writeHeader(void);

	uint32_t get_static_string_table_index(const char *str);

	SnapshotImageProgramHeader* startProgramHeader(uint32_t type, uint32_t flags, Elf64_Addr vaddr, Elf64_Addr paddr, uint64_t align) ;
	void endProgramHeader(SnapshotImageProgramHeader *programHeader, uint64_t extraMemSize = 0);
	void writeProgramHeaders(void);
	void writeSectionHeaders(void);

	SymbolTableEntry* create_local_symbol(const char *name, SymbolTable::Type type, uint16_t sectionIndex, uintptr_t value, uint64_t size);
	SymbolTableEntry* create_global_symbol(const char *name, SymbolTable::Type type, uint16_t sectionIndex, uintptr_t value, uint64_t size);

	bool writeSymbolTable(SymbolTable::Binding binding);

	/**
	 * Create the SymbolHashTable based on the SymbolTable
	 * and write the table into the file.  Update the section
	 * header with the appropriate data - file position, link
	 * to the symbol table, etc.
	 */
	bool writeHashTable();

	static void writeSnapshotFile(J9JavaVM *vm);

friend StringTable;
friend SymbolTable;
friend SymbolHashTable;
friend DynamicTable;
};

#endif /* SNAPSHOTIMAGEWRITER_HPP_ */
