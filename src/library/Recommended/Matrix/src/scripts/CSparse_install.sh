#!/bin/sh
#
URL=http://www.cise.ufl.edu/research/sparse/CSparse
tarFile=CSparse.tar.gz
## subdirectory *inside* tar file:
sDir=CSparse/Source
iDir=CSparse/Include
# ----------------- a "full path" for the current directory:
MatrixDir=`dirname $0`/..; cd $MatrixDir; MatrixDir=`pwd`
if [ -d $MatrixDir ]
then echo "Downloading into directory '$MatrixDir' .."
else echo "no directory '$MatrixDir' .. exiting"; exit 3
fi
cd $MatrixDir
#
wget $URL/$tarFile
#
if [ ! -r $tarFile ]; then echo "no file $tarFile .. exiting"; exit 1 ; fi
## extract only the part we want
tar zxf $tarFile $iDir
if [ ! -d $iDir ]; then echo "no directory $iDir .. exiting"; exit 2 ; fi
echo -n "Moving file from $iDir .. "
(cd $iDir && chmod a+r cs.h && mv  cs.h   $MatrixDir)
tar zxf $tarFile $sDir
if [ ! -d $sDir ]; then echo "no directory $sDir .. exiting"; exit 2 ; fi
echo -n "cat'ing files from $sDir ..	 "
cd $sDir
cat cs_*.c | sed -e '1 p' -e '/^#include/d' > $MatrixDir/cs.c
echo '[Ok]'
echo -n "cleaning up $iDir $sDir ..			 "
## keep the [TAB]s here               ^^
cd $MatrixDir
rm -rf $tarFile $sDir $iDir
rmdir `dirname $sDir`
echo '[Ok]'
