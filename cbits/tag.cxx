#include <taglib/tag.h>
#include <taglib/tfile.h>
#include <taglib/fileref.h>

/**
 * TagLib::FileRef is refcounted, so the attached finalizer just needs to free
 * the opened file.
 */
extern "C" TagLib::FileRef * c_open(const char * path) {
	return new TagLib::FileRef(path);
}

/**
 * Cleanup an allocated TagLib::FileRef.
 */
extern "C" void c_freeFileRef(TagLib::FileRef *f) {
	delete f;
}

/**
 * Check the validity of an opened TagLib::FileRef
 */
extern "C" char c_isValid(TagLib::FileRef *f) {
	return static_cast<char>(f->file()->isValid());
}

extern "C" void c_freeString(TagLib::String *str) {
	delete str;
}

extern "C" const char * c_toCString(TagLib::String *str) {
	return str->toCString(true);
}

/**
 * Return the artist field of a file
 */
extern "C" TagLib::String * c_artist(TagLib::FileRef *f) {
	return new TagLib::String(f->tag()->artist());
}

/**
 * Return the artist field of a file
 */
extern "C" TagLib::String * c_title(TagLib::FileRef *f) {
	return new TagLib::String(f->tag()->title());
}

/**
 * Return the artist field of a file
 */
extern "C" TagLib::String * c_album(TagLib::FileRef *f) {
	return new TagLib::String(f->tag()->album());
}

/**
 * Return the artist field of a file
 */
extern "C" TagLib::String * c_comment(TagLib::FileRef *f) {
	return new TagLib::String(f->tag()->comment());
}

/**
 * Return the artist field of a file
 */
extern "C" TagLib::String * c_genre(TagLib::FileRef *f) {
	return new TagLib::String(f->tag()->genre());
}

extern "C" int c_year(TagLib::FileRef *f) {
	return f->tag()->year();
}

extern "C" int c_track(TagLib::FileRef *f) {
	return f->tag()->track();
}
