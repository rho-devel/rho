### configure.ac                                        -*- Autoconf -*-

AC_PREREQ(2.57)

AC_INIT([src/swap_bytes.h.in])
AC_CONFIG_FILES([src/Makevars])
AC_CONFIG_HEADERS([src/swap_bytes.h src/var.h])

: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi

CC=`"${R_HOME}/bin/R" CMD config CC`
CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`
AC_PROG_CC

AC_CACHE_CHECK([whether ${CC-cc} accepts -Wno-long-long],
[ac_cv_prog_cc_no_long_long],
[AC_LANG_PUSH(C)
save_CFLAGS="${CFLAGS}"
CFLAGS="${CFLAGS} -Wno-long-long"
AC_COMPILE_IFELSE([AC_LANG_PROGRAM()],
                  [ac_cv_prog_cc_no_long_long=yes],
                  [ac_cv_prog_cc_no_long_long=no])
CFLAGS="${save_CFLAGS}"
AC_LANG_POP(C)])
if test "${ac_cv_prog_cc_no_long_long}" = yes; then
  PKG_CFLAGS="-Wno-long-long"
else
  PKG_CFLAGS=""
fi
AC_SUBST(PKG_CFLAGS)

### check if system has a byteswap.h file defining bswap_n for n = 16,
### 32 and 64.
AC_CHECK_HEADER([byteswap.h],
[AC_TRY_LINK([#include <byteswap.h>],
[unsigned short s = 1;
unsigned int i = 1;
s = bswap_16(s);
i = bswap_32(i);],
[AC_DEFINE(HAVE_GLIBC_BSWAP)])])

AC_CHECK_SIZEOF(double, 8)
AC_CHECK_SIZEOF(int, 4)
AC_CHECK_SIZEOF(long, 4)

AC_OUTPUT()
