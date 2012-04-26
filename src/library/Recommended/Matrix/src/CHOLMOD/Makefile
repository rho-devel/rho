#-------------------------------------------------------------------------------
# CHOLMOD Makefile
#-------------------------------------------------------------------------------

.PHONY : default all library purge clean distclean ccode

default: all

# Compile the C-callable libraries and the Demo programs.
all:
	( cd Lib ; $(MAKE) )

# Compile the C-callable libraries only.
library:
	( cd Lib ; $(MAKE) )

# Remove all files not in the original distribution
purge:
	( cd Lib ; $(MAKE) purge )

# Remove all files not in the original distribution, except keep the 
# compiled libraries.
clean:
	( cd Lib ; $(MAKE) clean )

distclean: purge

ccode: all

