include $(MkInclude)

PKG_CPPFLAGS = -I../Include -I../../UFconfig

# GNU make :
# AMD = amd_aat amd_1 amd_2 amd_postorder amd_post_tree amd_defaults \
# 	amd_order amd_control amd_info amd_valid amd_preprocess amd_dump
# AMDI = $(addsuffix .o, $(subst amd_,amd_i_,$(AMD)))
# AMDL = $(addsuffix .o, $(subst amd_,amd_l_,$(AMD)))
# "POSIX make":
AMDI = amd_i_1.o amd_i_2.o amd_i_aat.o amd_i_control.o amd_i_defaults.o amd_i_dump.o amd_i_info.o amd_i_order.o amd_i_post_tree.o amd_i_postorder.o amd_i_preprocess.o amd_i_valid.o

AMDL = amd_l_1.o amd_l_2.o amd_l_aat.o amd_l_control.o amd_l_defaults.o amd_l_dump.o amd_l_info.o amd_l_order.o amd_l_post_tree.o amd_l_postorder.o amd_l_preprocess.o amd_l_valid.o

UFCONFIG = ../../UFconfig/UFconfig.h

INC = ../Include/amd.h ../Include/amd_internal.h $(UFCONFIG)

LIB = ../../AMD.a

lib: $(LIB)

$(LIB): amd_global.o $(AMDI) $(AMDL)
	$(AR) -rucs $(LIB) amd_global.o $(AMDI) $(AMDL)

amd_global.o: amd_global.c $(INC)

##-- This is GNU-make-only :
#amd_i_%.o: amd_%.c $(INC)
#	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS) -I../Include -DDINT -c $< -o $@
#amd_l_%.o: amd_%.c $(INC)
#	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS) -I../Include -DDLONG -c $< -o $@

##-- Whereas this is "standard make", produced from ./make-Make.R :
include make_o.mk

mostlyclean: clean

clean:
	@-rm -rf .libs _libs $(LIB)
	@-rm -f *.o
