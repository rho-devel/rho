#!/bin/sh
#
R=${R:-R-patched}
if [ x$R_HOME = x ] ; then R_HOME=`$R RHOME`; fi
RINC=${R_HOME}/include
#
MatrixDir=`dirname $0`; cd $MatrixDir; MatrixDir=`pwd`
if [ ! -d $MatrixDir ]
then echo "no directory '$MatrixDir' .. exiting"; exit 3
fi
cd $MatrixDir
## CHOLMOD has one include for which gcc -MM fails below:
FIX=CHOLMOD/Include/cholmod.h
if [ -f $FIX ]
then
  sed '/^#include "UFconfig/s/\(.*\)/\/* \1 *\//' $FIX > ${FIX}_fixed
  mv $FIX ${FIX}_orig
  mv ${FIX}_fixed $FIX
fi
## __end fix__
out=DEPS.mkf_automade
gcc -I$RINC -MM *.c | perl -pe "s{$RINC/[^.]*.h( \\\\\\n)?}{}g" > $out
#           ^^^ ~~~
if [ -f ${FIX}_orig ] ; then mv ${FIX}_orig $FIX ; fi
echo ''; echo "$0 done.  Resulting file is $MatrixDir/$out"
