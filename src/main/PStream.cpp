#include <climits>
#include <stdarg.h>
#include <cstdio>
#include <cstdlib>
#include "Defn.h" // For R_size_t...
#include "Rinternals.h" // For XDREncode..., Rconnection
#include "CXXR/PStream.hpp"
#include "CXXR/RObject.h"

using namespace CXXR;
// Prototypes for functions still in serialize.cpp

// ******* class PStream ******* //

RObject* CXXR::PStream::CallHook(RObject* x, RObject* fun)
{
	SEXP val, call;
	PROTECT(call = LCONS(fun, CONS(x, R_NilValue)));
	val = eval(call, R_GlobalEnv);
	UNPROTECT(1);
	return val;
}

void CXXR::PStream::free_mem_buffer() {
	if (this->data.buf) {
		unsigned char* buf=this->data.buf;
		this->data.buf=NULL;
		free(buf);
	}
}

void CXXR::PStream::resize_buffer(size_t needed) {
	/* This used to allocate double 'needed', but that was problematic for large buffers */
	size_t newsize = needed;
	/* we need to store the result in a RAWSXP */
	if(needed > INT_MAX)
		error(_("serialization is too large to store in a raw vector"));
	if(needed < INT_MAX - ELTSIZE) needed += ELTSIZE;
	this->data.buf = static_cast<unsigned char*>(realloc(this->data.buf, newsize));
	if (this->data.buf == NULL)
		error(_("cannot allocate buffer"));
	this->data.size = newsize;
}

int CXXR::PStream::Rsnprintf(char *buf, int size, const char *format, ...)
{
	int val;
	va_list(ap);
	va_start(ap, format);
	val = vsnprintf(buf, size, format, ap);
	buf[size-1] = '\0';
	va_end(ap);
	return val;
}

// Buffered Binary Connection
void CXXR::PStream::flush_bcon_buffer() {
	if (R_WriteConnection(bconbuf.con, bconbuf.buf, bconbuf.count))
		error(_("error writing to connection"));
	bconbuf.count = 0;
}

// ******* class PStreamOut ******* //
void CXXR::PStreamOut::OutByte(Rbyte i) {
	char buf[128];
	switch (type()) {
	case ASCII:
		Rsnprintf(buf, sizeof(buf), "%02x\n", i);
		OutBytes(buf, strlen(buf));
		break;
	case BINARY:
	case XDR:
		OutBytes(&i, 1);
		break;
	default:
	error(_("unknown or inappropriate output format"));
	}
}

void CXXR::PStreamOut::OutComplex(Rcomplex c) {
	OutReal(c.r);
	OutReal(c.i);
}

void CXXR::PStreamOut::OutFormat() {
	if (type() == BINARY) {
		warning(_("binary format is deprecated; using xdr instead"));
		setType(XDR);
	}
	switch (type()) {
	case ASCII:  OutBytes("A\n", 2); break;
	case BINARY: OutBytes("B\n", 2); break;
	case XDR:    OutBytes("X\n", 2); break;
	case ANY:
		error(_("must specify ascii, binary, or xdr format"));
	default: error(_("unknown output format"));
	}
}

void CXXR::PStreamOut::OutInteger(int i) {
	char buf[128];
	switch (type()) {
	case ASCII:
		if (i == NA_INTEGER)
			Rsnprintf(buf, sizeof(buf), "NA\n");
		else
			Rsnprintf(buf, sizeof(buf), "%d\n", i);
		OutBytes(buf, strlen(buf));
		break;
	case BINARY:
		OutBytes(&i, sizeof(int));
		break;
	case XDR:
		R_XDREncodeInteger(i, buf);
		OutBytes(buf, R_XDR_INTEGER_SIZE);
		break;
	default:
		error(_("unknown or inappropriate output format"));
	}
}

void CXXR::PStreamOut::OutReal(double d) {
	char buf[128];
	switch (type()) {
	case ASCII:
		if (! R_FINITE(d)) {
			if (ISNAN(d))
				Rsnprintf(buf, sizeof(buf), "NA\n");
			else if (d < 0)
				Rsnprintf(buf, sizeof(buf), "-Inf\n");
			else
				Rsnprintf(buf, sizeof(buf), "Inf\n");
		}
		else
		/* 16: full precision; 17 gives 999, 000 &c */
		Rsnprintf(buf, sizeof(buf), "%.16g\n", d);
		OutBytes(buf, strlen(buf));
		break;
	case BINARY:
		OutBytes(&d, sizeof(double));
		break;
	case XDR:
		R_XDREncodeDouble(d, buf);
		OutBytes(buf, R_XDR_DOUBLE_SIZE);
		break;
	default:
		error(_("unknown or inappropriate output format"));
	}
}

#define REFSXP 255
#define PACK_REF_INDEX(i) (((i) << 8) | REFSXP)
#define UNPACK_REF_INDEX(i) ((i) >> 8)
#define MAX_PACKED_INDEX (INT_MAX >> 8)

void CXXR::PStreamOut::OutRefIndex(int i) {
	if (i > MAX_PACKED_INDEX) {
		OutInteger(REFSXP);
		OutInteger(i);
	}
	else OutInteger(PACK_REF_INDEX(i));
}

void CXXR::PStreamOut::OutString(const char* s, int length) {
	if (type() == ASCII) {
		int i;
		char buf[128];
		for (i = 0; i < length; i++) {
			switch(s[i]) {
			case '\n': sprintf(buf, "\\n");  break;
			case '\t': sprintf(buf, "\\t");  break;
			case '\v': sprintf(buf, "\\v");  break;
			case '\b': sprintf(buf, "\\b");  break;
			case '\r': sprintf(buf, "\\r");  break;
			case '\f': sprintf(buf, "\\f");  break;
			case '\a': sprintf(buf, "\\a");  break;
			case '\\': sprintf(buf, "\\\\"); break;
			case '\?': sprintf(buf, "\\?");  break;
			case '\'': sprintf(buf, "\\'");  break;
			case '\"': sprintf(buf, "\\\""); break;
			default  :
				/* cannot print char in octal mode -> cast to unsigned
				char first */
				/* actually, since s is signed char and '\?' == 127
				is handled above, s[i] > 126 can't happen, but
				I'm superstitious...  -pd */
				if (s[i] <= 32 || s[i] > 126)
					sprintf(buf, "\\%03o", static_cast<unsigned char>( s[i]));
				else
					sprintf(buf, "%c", s[i]);
			}
			OutBytes(buf, strlen(buf));
		}
		OutChar('\n');
	}
	else
		OutBytes(CXXRNOCAST(void *)s, length); /* FIXME: is this case right? */
}


// ******* class PStreamOutMem ******* //

CXXR::PStreamOutMem::PStreamOutMem(Format type,
                                   int version,
				   HookFunc hook,
				   RObject* pdata)
  : PStreamOut(type, version, hook, pdata) {
	data.count=0;
	data.size=0;
	data.buf=NULL;
}

RObject* CXXR::PStreamOutMem::Close() {
	SEXP val;
	/* duplicate check, for future proofing */
	if(data.count > INT_MAX)
		error(_("serialization is too large to store in a raw vector"));
	PROTECT(val = allocVector(RAWSXP, data.count));
	memcpy(RAW(val), data.buf, data.count);
	free_mem_buffer();
	UNPROTECT(1);
	return val;
}

void CXXR::PStreamOutMem::OutChar(int c) {
	if (this->data.count >= this->data.size)
		resize_buffer(this->data.count + 1);
	this->data.buf[this->data.count++]=c;
}

void CXXR::PStreamOutMem::OutBytes(const void* buf, int length) {
	size_t needed=this->data.count + size_t(length);
	if(double(this->data.count)+length > double(INT_MAX))
		error(_("serialization is too large to store in a raw vector"));
	if (needed > this->data.size)
		resize_buffer(needed);
	memcpy(this->data.buf + this->data.count, buf, length);
	this->data.count = needed;
}

// ******* class PStreamOutFile ******* //
CXXR::PStreamOutFile::PStreamOutFile(Format type,
                                     int version,
				     FILE* fp,
				     HookFunc hook,
				     RObject* pdata) :
  PStreamOut(type, version, hook, pdata) {
	s_fp=fp;
}

void CXXR::PStreamOutFile::OutChar(int c) {
	fputc(c, s_fp);
}

void CXXR::PStreamOutFile::OutBytes(const void* buf, int length) {
	size_t out=fwrite(buf, 1, length, s_fp);
	if (static_cast<int>(out)!=length)
		error(_("write failed"));
}

// ******* class PStreamOutConn ******* //

CXXR::PStreamOutConn::PStreamOutConn(Format format,
                                     int version,
				     Rconnection conn,
				     HookFunc hook,
				     RObject* pdata) :
  PStreamOut(format, version, hook, pdata) {
	s_conn=conn;
	CheckOutConn();
	if (s_conn->text && type() != PStream::ASCII)
		error(_("only ascii format can be written to text mode connections"));
}

void CXXR::PStreamOutConn::CheckOutConn() {
	if (! s_conn->isopen) 
		error(_("connection is not open")); 
	if (! s_conn->canwrite || s_conn->write == NULL) 
		error(_("cannot write to this connection")); 
}

void CXXR::PStreamOutConn::OutChar(int c) {
	CheckOutConn();
	if (s_conn->text)
		Rconn_printf(s_conn, "%c", c);
	else {
		char buf[1];
		buf[0] = char( c);
		if (1 != s_conn->write(buf, 1, 1, s_conn))
			error(_("error writing to connection"));
	}
}

void CXXR::PStreamOutConn::OutBytes(const void* buf, int length) {
	CheckOutConn();
	if (s_conn->text) {
		int i;
		CXXRCONST char *p = CXXRCONSTRUCT(static_cast<const char*>, buf);
		for (i = 0; i < length; i++)
			Rconn_printf(s_conn, "%c", p[i]);
	}
	else {
		if (length != CXXRCONSTRUCT(int, s_conn->write(buf, 1, length, s_conn)))
			error(_("error writing to connection"));
	}
}

// ******* class PStreamOutBB ******* //

CXXR::PStreamOutBB::PStreamOutBB(Format format,
                                 int version,
				 Rconnection con,
				 HookFunc hook,
				 RObject* pdata) :
	PStreamOut(format, version, hook, pdata) {
	bconbuf.con=con;
	bconbuf.count=0;
}

void CXXR::PStreamOutBB::OutChar(int c) {
	if (bconbuf.count >= BCONBUFSIZ)
		flush_bcon_buffer();
	bconbuf.buf[bconbuf.count++]=c;
}

void CXXR::PStreamOutBB::OutBytes(const void* buf, int length) {
	if (bconbuf.count + length > BCONBUFSIZ)
		flush_bcon_buffer();
	if (static_cast<unsigned int>(length) <= BCONBUFSIZ) {
		memcpy(bconbuf.buf + bconbuf.count, buf, length);
		bconbuf.count += length;
	}
	else if (R_WriteConnection(bconbuf.con, buf, length) != static_cast<unsigned int>(length))
		error(_("error writing to connection"));
}


// ******* class PStreamIn *********** //

Rcomplex CXXR::PStreamIn::InComplex() {
	Rcomplex c;
	c.r = InReal();
	c.i = InReal();
	return c;
}

void CXXR::PStreamIn::InFormat() {
	char buf[2];
	Format type;
	this->InBytes(buf, 2);
	switch (buf[0]) {
	case 'A': type = PStream::ASCII; break;
	case 'B': type = PStream::BINARY; break;
	case 'X': type = PStream::XDR; break;
	case '\n':
	/* GROSS HACK: ASCII unserialize may leave a trailing newline
	in the stream.  If the stream contains a second
	serialization, then a second unserialize will fail if such
	a newline is present.  The right fix is to make sure
	unserialize consumes exactly what serialize produces.  But
	this seems hard because of the current use of whitespace
	skipping in unserialize.  So a temporary hack to cure the
	symptom is to deal with a possible leading newline.  I
	don't think more than one is possible, but I'm not sure.
	LT */
		if (buf[1] == 'A') {
			type = PStream::ASCII;
			this->InBytes(buf, 1);
			break;
		}
	default:
		type = PStream::ANY;  /* keep compiler happy */
		error(_("unknown input format"));
	}
	if (this->format == PStream::ANY)
		this->format = type;
	else if (type != this->format)
		error(_("input format does not match specified format"));
}

int CXXR::PStreamIn::InInteger() {
	char word[128];
	char buf[128];
	int i;

	switch (type()) {
	case PStream::ASCII:
		InWord(word, sizeof(word));
		sscanf(word, "%s", buf);
		if (strcmp(buf, "NA") == 0)
			return NA_INTEGER;
		else
			sscanf(buf, "%d", &i);
		return i;
	case PStream::BINARY:
		InBytes(&i, sizeof(int));
		return i;
	case PStream::XDR:
		InBytes(buf, R_XDR_INTEGER_SIZE);
		return R_XDRDecodeInteger(buf);
	default:
		return NA_INTEGER;
	}
}

double CXXR::PStreamIn::InReal() {
	char word[128];
	char buf[128];
	double d;

	switch (type()) {
	case PStream::ASCII:
		InWord(word, sizeof(word));
		sscanf(word, "%s", buf);
		if (strcmp(buf, "NA") == 0)
			return NA_REAL;
		else if (strcmp(buf, "Inf") == 0)
			return R_PosInf;
		else if (strcmp(buf, "-Inf") == 0)
			return R_NegInf;
		else
			sscanf(buf, "%lg", &d);
		return d;
	case PStream::BINARY:
		InBytes(&d, sizeof(double));
		return d;
	case PStream::XDR:
		InBytes(buf, R_XDR_DOUBLE_SIZE);
		return R_XDRDecodeDouble(buf);
	default:
		return NA_REAL;
	}
}

int CXXR::PStreamIn::InRefIndex(int flags) {
	int i = UNPACK_REF_INDEX(flags);
	if (i == 0)
		return InInteger();
	else
		return i;
}

void CXXR::PStreamIn::InString(char* buf, int length) {
	if (type() == PStream::ASCII) {
		if (length > 0) {
			int c, d, i, j;

			StringStream iss(this);
			while(isspace(c = iss.GetChar()))
				;
			iss.UngetChar(c);
			for (i = 0; i < length; i++) {
				if ((c =  iss.GetChar()) == '\\') {
					switch(c = iss.GetChar()) {
					case 'n' : buf[i] = '\n'; break;
					case 't' : buf[i] = '\t'; break;
					case 'v' : buf[i] = '\v'; break;
					case 'b' : buf[i] = '\b'; break;
					case 'r' : buf[i] = '\r'; break;
					case 'f' : buf[i] = '\f'; break;
					case 'a' : buf[i] = '\a'; break;
					case '\\': buf[i] = '\\'; break;
					case '?' : buf[i] = '\?'; break;
					case '\'': buf[i] = '\''; break;
					case '\"': buf[i] = '\"'; break; /* closing " for emacs */
					case '0': case '1': case '2': case '3':
					case '4': case '5': case '6': case '7':
						d = 0; j = 0;
						while('0' <= c && c < '8' && j < 3) {
							d = d * 8 + (c - '0');
							c = iss.GetChar();
							j++;
						}
						buf[i] = d;
						iss.UngetChar(c);
						break;
					default  : buf[i] = c;
					}
				}
				else buf[i] = c;
			}
		}
	}
	else
		InBytes(buf, length);
}

void CXXR::PStreamIn::InWord(char* buf, int size) {
	int c, i;
	i = 0;
	do {
		c = InChar();
		if (c == EOF)
			error(_("read error"));
	} while (isspace(c));
	while (! isspace(c) && i < size) {
		buf[i++] = c;
		c = InChar();
	}
	if (i == size)
		error(_("read error"));
	buf[i] = 0;
}

// ******* class PStreamInMem ******* //
CXXR::PStreamInMem::PStreamInMem(void* buf,
                                 int length,
                                 HookFunc phook,
                                 RObject* pdata)
  : PStreamIn(PStream::ANY, phook, pdata) {
	data.count=0;
	data.size=length;
	data.buf=static_cast<unsigned char*>(buf);	
}

int CXXR::PStreamInMem::InChar() {
	if (data.count >= data.size)
		error(_("read error"));
	return data.buf[data.count++];
}

void CXXR::PStreamInMem::InBytes(void* buf, int length) {
	if (data.count + R_size_t(length) > data.size)
		error(_("read error"));
	memcpy(buf, data.buf + data.count, length);
	data.count += length;
}

// ******* class PStreamInFile ******* //
CXXR::PStreamInFile::PStreamInFile(Format format,
                                   FILE* fp,
                                   HookFunc phook,
                                   RObject* pdata)
  : PStreamIn(format, phook, pdata) {
	s_fp=fp;
}

int CXXR::PStreamInFile::InChar() {
	return fgetc(s_fp);
}

void CXXR::PStreamInFile::InBytes(void* buf, int length) {
	size_t in=fread(buf, 1, length, s_fp);
	if (CXXRCONSTRUCT(int, in) != length) error(_("read failed"));
}

// ******* class PStreamIn::PStreamInConn ******* //
CXXR::PStreamInConn::PStreamInConn(Format format,
                                   Rconnection c,
                                   HookFunc phook,
                                   RObject* pdata)
  : PStreamIn(format, phook, pdata) {
	s_conn=c;

	CheckInConn();
	if (con()->text) {
		if (type() == PStream::ANY)
			this->format=PStream::ASCII;
		else if (type() != PStream::ASCII)
			error(_("only ascii format can be read from text mode connections"));
	}
}

void CXXR::PStreamInConn::CheckInConn() {
    if (! con()->isopen)
        error(_("connection is not open"));
    if (! con()->canread || con()->read == NULL)
        error(_("cannot read from this connection"));
}

int CXXR::PStreamInConn::InChar() {
	char buf[1];
	CheckInConn();
	if (con()->text)
		return Rconn_fgetc(con());
	else {
		if (1 != con()->read(buf, 1, 1, con()))
			error(_("error reading from connection"));
		return buf[0];
	}
}

void CXXR::PStreamInConn::InBytes(void *buf, int length) {
	CheckInConn();
	if (con()->text) {
		int i;
		char *p = CXXRCONSTRUCT(static_cast<char*>, buf);
		for (i = 0; i < length; i++)
			p[i] = Rconn_fgetc(con());
	}
	else {
		if (type() == PStream::ASCII) {
			char linebuf[4];
			unsigned char *p = CXXRCONSTRUCT(static_cast<unsigned char*>, buf);
			int i, ncread;
			unsigned int res;
			for (i = 0; i < length; i++) {
				ncread = Rconn_getline(con(), linebuf, 3);
				if (ncread != 2)
					error(_("error reading from ascii connection"));
				if (!sscanf(linebuf, "%02x", &res))
					error(_("unexpected format in ascii connection"));
				*p++ = static_cast<unsigned char>(res);
			}
		} else {
			if (length != CXXRCONSTRUCT(int, con()->read(buf, 1, length, con())))
				error(_("error reading from connection"));
		}
	}
}

// ******* class PStreamIn::StringStream ******* //
int CXXR::PStreamIn::StringStream::GetChar() {
	int c;
	if (last != EOF) {
		c = last;
		last = EOF;
	}
	else c = stream->InChar();
	return c;
}

void CXXR::PStreamIn::StringStream::UngetChar(int c) {
	last = c;
}

