### boost.m4 -- macros for Boost library detection
###
### These macros contain code taken from those from the GNU Autoconf Archive.
###
### The serial numbers of the versions used were as follows:
### Base 20 Regex 22 Serialization 21

### Requires AX_BOOST_BASE to have been called
### Only exports BOOST_CPPFLAGS and BOOST_LDFLAGS if
### a path was given in --with-boost-dir= 
AC_DEFUN([CXXR_BOOST],
[
    if test -n "$ac_boost_path"; then
	BOOSTLIBDIR=`echo $BOOST_LDFLAGS | sed -e 's/@<:@^\/@:>@*//'`
	BOOST_LD_LIBRARY_PATH="${BOOSTLIBDIR}"
    else
	AC_SUBST(BOOST_CPPFLAGS,"")
	AC_SUBST(BOOST_LDFLAGS,"")
    fi
    AC_SUBST(BOOST_LD_LIBRARY_PATH)
]) #CXXR_BOOST


AC_DEFUN([AX_BOOST_BASE],
[
AC_ARG_WITH([boost-dir],
  [AS_HELP_STRING([--with-boost-dir@<:@=PATH@:>@],
    [Specify a path for the boost library.
     This may be a staged (i.e. not installed) build.])],
    [
    if test "$withval" != ""; then
        ac_boost_path="$withval"
    fi
    ],[
    ])


    boost_lib_version_req=ifelse([$1], ,1.20.0,$1)
    boost_lib_version_req_shorten=`expr $boost_lib_version_req : '\([[0-9]]*\.[[0-9]]*\)'`
    boost_lib_version_req_major=`expr $boost_lib_version_req : '\([[0-9]]*\)'`
    boost_lib_version_req_minor=`expr $boost_lib_version_req : '[[0-9]]*\.\([[0-9]]*\)'`
    boost_lib_version_req_sub_minor=`expr $boost_lib_version_req : '[[0-9]]*\.[[0-9]]*\.\([[0-9]]*\)'`
    if test "x$boost_lib_version_req_sub_minor" = "x" ; then
        boost_lib_version_req_sub_minor="0"
        fi
    WANT_BOOST_VERSION=`expr $boost_lib_version_req_major \* 100000 \+  $boost_lib_version_req_minor \* 100 \+ $boost_lib_version_req_sub_minor`
    AC_MSG_CHECKING(for boostlib >= $boost_lib_version_req)
    succeeded=no

    dnl On 64-bit systems check for system libraries in both lib64 and lib.
    dnl The former is specified by FHS, but e.g. Debian does not adhere to
    dnl this (as it rises problems for generic multi-arch support).
    dnl The last entry in the list is chosen by default when no libraries
    dnl are found, e.g. when only header-only libraries are installed!
    libsubdirs="stage/lib lib"
    ax_arch=`uname -m`
    if test $ax_arch = x86_64 -o $ax_arch = ppc64 -o $ax_arch = s390x -o $ax_arch = sparc64; then
        libsubdirs="stage/lib64 lib64 stage/lib lib lib64"
    fi

    if test "$ac_boost_path" != ""; then
    	dnl Let's see if we have what looks like a staged build, i.e. DIR/boost and DIR/stage
	dnl If we are, then don't append /include to it
	if test -d "$ac_boost_path"/"boost" && test -d "$ac_boost_path"/stage; then
	    BOOST_CPPFLAGS="-I$ac_boost_path"
	else
	    BOOST_CPPFLAGS="-I$ac_boost_path/include"
	fi
	for ac_boost_path_tmp in $libsubdirs; do
	    if test -d "$ac_boost_path"/"$ac_boost_path_tmp" ; then
		BOOST_LDFLAGS="-L$ac_boost_path/$ac_boost_path_tmp"
		break
	    fi
	done
    elif test "$cross_compiling" != yes; then
        for ac_boost_path_tmp in /usr /usr/local /opt /opt/local ; do
            if test -d "$ac_boost_path_tmp/include/boost" && test -r "$ac_boost_path_tmp/include/boost"; then
                for libsubdir in $libsubdirs ; do
                    if ls "$ac_boost_path_tmp/$libsubdir/libboost_"* >/dev/null 2>&1 ; then break; fi
                done
                BOOST_LDFLAGS="-L$ac_boost_path_tmp/$libsubdir"
                BOOST_CPPFLAGS="-I$ac_boost_path_tmp/include"
                break;
            fi
        done
    fi

    CPPFLAGS_SAVED="$CPPFLAGS"
    CPPFLAGS="$CPPFLAGS $BOOST_CPPFLAGS"
    export CPPFLAGS

    LDFLAGS_SAVED="$LDFLAGS"
    LDFLAGS="$LDFLAGS $BOOST_LDFLAGS"
    export LDFLAGS

    AC_REQUIRE([AC_PROG_CXX])
    AC_LANG_PUSH(C++)
        AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
    @%:@include <boost/version.hpp>
    ]], [[
    #if BOOST_VERSION >= $WANT_BOOST_VERSION
    // Everything is okay
    #else
    #  error Boost version is too old
    #endif
    ]])],[
        AC_MSG_RESULT(yes)
    succeeded=yes
    found_system=yes
        ],[
        ])
    AC_LANG_POP([C++])

    if test "$succeeded" != "yes" ; then
        if test "$_version" = "0" ; then
            AC_MSG_NOTICE([[We could not detect the boost libraries (version $boost_lib_version_req_shorten or higher). If you are sure you have boost installed, then check your version number looking in <boost/version.hpp>. See http://randspringer.de/boost for more documentation.]])
        else
            AC_MSG_NOTICE([Your boost libraries seems to old (version $_version).])
        fi
        # execute ACTION-IF-NOT-FOUND (if present):
        ifelse([$3], , :, [$3])
    else
        AC_DEFINE(HAVE_BOOST,,[define if the Boost library is available])
        # execute ACTION-IF-FOUND (if present):
        ifelse([$2], , :, [$2])
    fi

    CPPFLAGS="$CPPFLAGS_SAVED"
    LDFLAGS="$LDFLAGS_SAVED"

])

AC_DEFUN([AX_BOOST_REGEX],
[
        AC_REQUIRE([AC_PROG_CC])
		CPPFLAGS_SAVED="$CPPFLAGS"
		CPPFLAGS="$CPPFLAGS $BOOST_CPPFLAGS"
		export CPPFLAGS

		LDFLAGS_SAVED="$LDFLAGS"
		LDFLAGS="$LDFLAGS $BOOST_LDFLAGS"
		export LDFLAGS

        AC_CACHE_CHECK(whether the boost::regex library is available,
					   ax_cv_boost_regex,
        [AC_LANG_PUSH([C++])
			 AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[@%:@include <boost/regex.hpp>
												]],
                                   [[boost::regex r(); return 0;]])],
                   ax_cv_boost_regex=yes, ax_cv_boost_regex=no)
         AC_LANG_POP([C++])
		])
		if test "x$ax_cv_boost_regex" = "xyes"; then
			AC_DEFINE(HAVE_BOOST_REGEX,,[define if the Boost::Regex library is available])
            BOOSTLIBDIR=`echo $BOOST_LDFLAGS | sed -e 's/@<:@^\/@:>@*//'`

                for libextension in `ls $BOOSTLIBDIR/libboost_regex*.so* $BOOSTLIBDIR/libboost_regex*.dylib* $BOOSTLIBDIR/libboost_regex*.a* 2>/dev/null | sed 's,.*/,,' | sed -e 's;^lib\(boost_regex.*\)\.so.*$;\1;' -e 's;^lib\(boost_regex.*\)\.dylib.*;\1;' -e 's;^lib\(boost_regex.*\)\.a.*$;\1;'` ; do
                     ax_lib=${libextension}
				    AC_CHECK_LIB($ax_lib, exit,
                                 [BOOST_REGEX_LIB="-l$ax_lib"; AC_SUBST(BOOST_REGEX_LIB) link_regex="yes"; break],
                                 [link_regex="no"])
				done
                if test "x$link_regex" != "xyes"; then
                for libextension in `ls $BOOSTLIBDIR/boost_regex*.dll* $BOOSTLIBDIR/boost_regex*.a* 2>/dev/null | sed 's,.*/,,' | sed -e 's;^\(boost_regex.*\)\.dll.*$;\1;' -e 's;^\(boost_regex.*\)\.a.*$;\1;'` ; do
                     ax_lib=${libextension}
				    AC_CHECK_LIB($ax_lib, exit,
                                 [BOOST_REGEX_LIB="-l$ax_lib"; AC_SUBST(BOOST_REGEX_LIB) link_regex="yes"; break],
                                 [link_regex="no"])
				done
                fi

            if test "x$ax_lib" = "x"; then
                AC_MSG_ERROR(Could not find a version of the boost::regex library!)
            fi
			if test "x$link_regex" != "xyes"; then
				AC_MSG_ERROR(Could not link against $ax_lib !)
			fi
		fi

		CPPFLAGS="$CPPFLAGS_SAVED"
	LDFLAGS="$LDFLAGS_SAVED"
])

AC_DEFUN([AX_BOOST_SERIALIZATION],
[
        AC_REQUIRE([AC_PROG_CC])
		CPPFLAGS_SAVED="$CPPFLAGS"
		CPPFLAGS="$CPPFLAGS $BOOST_CPPFLAGS"
		export CPPFLAGS

		LDFLAGS_SAVED="$LDFLAGS"
		LDFLAGS="$LDFLAGS $BOOST_LDFLAGS"
		export LDFLAGS

        AC_CACHE_CHECK(whether the boost::serialization library is available,
					   ax_cv_boost_serialization,
        [AC_LANG_PUSH([C++])
			 AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[@%:@include <fstream>
												 @%:@include <boost/archive/text_oarchive.hpp>
                                                 @%:@include <boost/archive/text_iarchive.hpp>
												]],
                                   [[std::ofstream ofs("filename");
									boost::archive::text_oarchive oa(ofs);
									 return 0;
                                   ]])],
                   ax_cv_boost_serialization=yes, ax_cv_boost_serialization=no)
         AC_LANG_POP([C++])
		])
		if test "x$ax_cv_boost_serialization" = "xyes"; then
			AC_DEFINE(HAVE_BOOST_SERIALIZATION,,[define if the Boost::Serialization library is available])
            BOOSTLIBDIR=`echo $BOOST_LDFLAGS | sed -e 's/@<:@^\/@:>@*//'`
                for libextension in `ls $BOOSTLIBDIR/libboost_serialization*.so* $BOOSTLIBDIR/libboost_serialization*.dylib* $BOOSTLIBDIR/libboost_serialization*.a* 2>/dev/null | sed 's,.*/,,' | sed -e 's;^lib\(boost_serialization.*\)\.so.*$;\1;' -e 's;^lib\(boost_serialization.*\)\.dylib.*$;\1;' -e 's;^lib\(boost_serialization.*\)\.a*$;\1;'` ; do
                     ax_lib=${libextension}
				    AC_CHECK_LIB($ax_lib, exit,
                                 [BOOST_SERIALIZATION_LIB="-l$ax_lib"; AC_SUBST(BOOST_SERIALIZATION_LIB) link_serialization="yes"; break],
                                 [link_serialization="no"])
				done
                if test "x$link_serialization" != "xyes"; then
                for libextension in `ls $BOOSTLIBDIR/boost_serialization*.dll* $BOOSTLIBDIR/boost_serialization*.a* 2>/dev/null | sed 's,.*/,,' | sed -e 's;^\(boost_serialization.*\)\.dll.*$;\1;' -e 's;^\(boost_serialization.*\)\.a.*$;\1;'` ; do
                     ax_lib=${libextension}
				    AC_CHECK_LIB($ax_lib, exit,
                                 [BOOST_SERIALIZATION_LIB="-l$ax_lib"; AC_SUBST(BOOST_SERIALIZATION_LIB) link_serialization="yes"; break],
                                 [link_serialization="no"])
				done
                fi

            if test "x$ax_lib" = "x"; then
                AC_MSG_ERROR(Could not find a version of the boost::serialization library!)
            fi
			if test "x$link_serialization" != "xyes"; then
				AC_MSG_ERROR(Could not link against $ax_lib !)
			fi
		fi

		CPPFLAGS="$CPPFLAGS_SAVED"
	LDFLAGS="$LDFLAGS_SAVED"
])
