PKG_CPPFLAGS = -DNTIMER
include $(MkInclude)

LIB = ../SuiteSparse_config.a

library: $(LIB)

$(LIB): SuiteSparse_config.o
	$(AR) -rucs $(LIB) SuiteSparse_config.o

mostlyclean: clean

clean:
	@-rm -rf .libs _libs $(LIB)
	@-rm -f *.o

