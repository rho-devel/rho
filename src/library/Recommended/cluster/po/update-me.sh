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
echo ''; echo '## FIXME ## use new Scheme from R 2.16.x on'
cd `R-patched RHOME`/po
make pkg-update PKG=$pkg PKGDIR=$pkgDIR
echo 'end{make pkg-update}' ; echo ''
echo 'Test with (e.g.)'
echo '       LANGUAGE=de R --no-environ --no-save' ; echo ''
echo 'and then something like'
echo '       ellipsoidPoints(diag(3)) ; ellipsoidPoints(cbind(1,1:3))'; echo ''
echo 'Commit with something like'
echo "       svn ci -m'translation updates'  po  inst/po"; echo ''
