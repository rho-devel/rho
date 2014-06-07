include $(MkInclude)

PKG_CPPFLAGS = -I../Include -I../../SuiteSparse_config

LIB = ../../COLAMD.a

lib: $(LIB)

colamd_l.o: colamd.c
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS) -I../Include -DDLONG -c colamd.c -o $@

$(LIB): colamd_global.o colamd.o colamd_l.o
	$(AR) -rucs $(LIB) colamd_global.o colamd.o colamd_l.o

mostlyclean: clean

clean:
	@-rm -rf .libs _libs $(LIB)
	@-rm -f *.o
