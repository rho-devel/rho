include $(MkInclude)

PKG_CPPFLAGS = -I../Include -I../../UFconfig

LIB = ../../COLAMD.a

lib: $(LIB)

colamd_l.o: colamd.c
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS) -I../Include -DDLONG -c colamd.c -o $@

$(LIB): colamd_global.o colamd_l.o # colamd.o
	$(AR) -rucs $(LIB) colamd_global.o colamd_l.o # colamd.o

mostlyclean: clean

clean:
	@-rm -rf .libs _libs $(LIB)
	@-rm -f *.o
