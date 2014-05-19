include $(MkInclude)

PKG_CPPFLAGS = -I../Include -I../../SuiteSparse_config

LIB = ../../AMD.a

lib: $(LIB)

include make_o.mk

$(LIB): $(OBJS)
	$(AR) -rucs $(LIB) $(OBJS)

mostlyclean: clean

clean:
	@-rm -rf .libs _libs $(LIB)
	@-rm -f *.o
