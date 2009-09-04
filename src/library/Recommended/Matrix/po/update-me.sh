#!/bin/sh
#
## Script for updating package-specific *.pot files
## written such that it should work for any package
#
thisdir=`dirname $0` ; cd $thisdir; thisdir=`pwd`
echo 'preliminary thisdir='$thisdir
pkgDIR=`dirname $thisdir`
pkg=`basename $pkgDIR`
echo '  -->        pkgDIR='$pkgDIR' ; pkg='$pkg
cd `R-devel RHOME`/po
make pkg-update PKG=$pkg PKGDIR=$pkgDIR
echo 'end{make pkg-update}' ; echo ''
echo 'Test with (e.g.)'
echo '       LANGUAGE=de R --no-environ --no-save' ; echo ''
echo 'and then something like'
echo '       Matrix(1:6, 2,3) %*% Matrix(1:4, 2)'; echo ''
echo 'Commit with something like'
echo "       svn ci -m'translation updates'  po  inst/po"; echo ''