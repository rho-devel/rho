#ifndef PSTREAM_HPP
#define PSTREAM_HPP
#include <cstdio> // for FILE
#include "CXXR/ComplexVector.h" // for Rcomplex
#include "CXXR/RawVector.h" // for Rbyte
#include "Rconnections.h" // for Rconnection

namespace CXXR {
	class PStream {
	public:
		enum Format {ANY = 0, ASCII, BINARY, XDR};
		static const int R_DefaultSerializeVersion = 2;

		PStream(Format format) {
			this->format=format;
			this->data.size=0;
			this->data.count=0;
			this->data.buf=NULL;
		}

		RObject* CallHook(RObject*, RObject*);
		int Rsnprintf(char*, int, const char*, ...);
		void setType(Format format) { this->format=format; }
		Format type() { return format; }

		// Memory Buffer
		void free_mem_buffer();
		void resize_buffer(size_t);

		// Buffered Binary
		void flush_bcon_buffer();

		typedef RObject* (*HookFunc)(RObject*, RObject*);
	private:
	protected:
		Format format; // Also may be set by PStreamIn::InFormat
		static const unsigned int ELTSIZE = 8192;
		static const unsigned int BCONBUFSIZ = 4096;
		struct {
			size_t size;
			size_t count;
			unsigned char* buf;
		} data; // membuf_t
		struct {
			Rconnection con;
			unsigned int count;
			unsigned char buf[BCONBUFSIZ];
		} bconbuf;
	};

	class PStreamIn : public PStream {
	public:
		class StringStream {
		public:
			StringStream(PStreamIn* st)
			  : last(EOF), stream(st) { };

			int GetChar();
			void UngetChar(int);
		private:
			int last;
			PStreamIn* stream;
		};

		PStreamIn(Format format,
		          HookFunc hook, RObject* pdata)
		  : PStream(format),
		    InPersistHookFunc(hook), InPersistHookData(pdata) { }
		virtual int  InChar() { error(_("shouldn't be here")); return 0; };
		virtual void InBytes(void*, int) { };

		HookFunc InPersistHookFunc;
		RObject* InPersistHookData;

		// Input methods
		Rcomplex InComplex();
		void     InFormat();
		int      InInteger();
		double   InReal();
		int      InRefIndex(int);
		void     InString(char*, int);
		void     InWord(char*, int);
	};

	class PStreamInMem : public PStreamIn {
	public:
		PStreamInMem(void*, int,
		             HookFunc phook=NULL, RObject* pdata=NULL);
		// Virtual Methods of PStreamIn
		int  InChar();            // InCharMem
		void InBytes(void*, int); // InBytesMem
	};

	class PStreamInFile : public PStreamIn {
	public:
		PStreamInFile(Format, FILE* fp,
		             HookFunc phook=NULL, RObject* pdata=NULL);
		// Virtual Methods of PStreamIn
		int  InChar();            // InCharFile
		void InBytes(void*, int); // InBytesFile

	private:
		FILE* s_fp;
	};

	class PStreamInConn : public PStreamIn {
	public:
		PStreamInConn(Format, Rconnection, 
		             HookFunc phook=NULL, RObject* pdata=NULL);
		void CheckInConn();
		Rconnection con() { return s_conn; }
		// Virtual Methods of PStreamIn
		int  InChar();            // InCharConn
		void InBytes(void*, int); // InBytesConn

	private:
		Rconnection s_conn;
	};

	class PStreamOut : public PStream {
	public:
		PStreamOut(Format format, int version,
                           HookFunc hook, RObject* pdata)
                  : PStream(format),
		    OutPersistHookFunc(hook), OutPersistHookData(pdata),
                    s_version(version ? version : R_DefaultSerializeVersion) { }
		virtual void OutChar (int) { }
		virtual void OutBytes (const void*, int) { }

		HookFunc OutPersistHookFunc;
		RObject* OutPersistHookData;

		void OutByte(Rbyte);
		void OutComplex(Rcomplex);
		void OutFormat();
		void OutInteger(int);
		void OutReal(double);
		void OutRefIndex(int);
		void OutString(const char*, int);
		void OutStringVec(RObject*, RObject*); // FIXME subtypes
		
		int version() { return s_version; }
	private:
		int s_version;
	};

	class PStreamOutMem : public PStreamOut {
	public:
		PStreamOutMem(Format, int,
                              HookFunc hook=NULL, RObject* pdata=NULL);
		RObject* Close();
		// Virtual functions of PStreamOut:
		void OutChar(int); // OutCharMem
		void OutBytes(const void*, int); // OutBytesMem
	};

	class PStreamOutFile : public PStreamOut {
	public:
		PStreamOutFile(Format, int, FILE*,
                               HookFunc hook=NULL, RObject* pdata=NULL);
		// Virtual functions of PStreamOut:
		void OutChar(int); //OutCharFile
		void OutBytes(const void*, int); // OutBytesFile
	private:
		FILE *s_fp;
	};

	class PStreamOutConn : public PStreamOut {
	public:
		PStreamOutConn(Format, int, Rconnection,
                               HookFunc hook=NULL, RObject* pdata=NULL);
		void CheckOutConn();
		// Virtual functions of PStreamOut:
		void OutChar(int); //OutCharConn
		void OutBytes(const void*, int); // OutBytesConn
	private:
		Rconnection s_conn;
	};

	class PStreamOutBB : public PStreamOut {
	public:
		PStreamOutBB(Format, int, Rconnection,
                             HookFunc hook=NULL, RObject* pdata=NULL);
		// Virtual functions of PStreamOut:
		void OutChar(int); // OutCharBB
		void OutBytes(const void*, int); // OutBytesBB
	};
}

#endif
