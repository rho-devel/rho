#!/bin/sh
#
#__>> Keep in sync with ~/R/Pkgs/cluster/po/update-me.sh  <<__
#
## Script for updating package-specific *.pot files
## written such that it should work for any package
#
R=${R:-R}
thisdir=`dirname $0` ; cd $thisdir; thisdir=`pwd`
echo 'preliminary thisdir='$thisdir
pkgDIR=`dirname $thisdir`
pkg=`basename $pkgDIR`
echo '  -->        pkgDIR='$pkgDIR' ; pkg='$pkg
# echo ''; echo '## FIXME ## use new Scheme from R 3.0.x on'
# cd `$R RHOME`/po
# make pkg-update PKG=$pkg PKGDIR=$pkgDIR
echo "require('tools'); update_pkg_po('$pkgDIR')" | $R --slave
##    --------------------------------  as of R 3.0.0
echo 'end{make pkg-update}' ; echo ''
echo 'Test with (e.g.)'
echo '       LANGUAGE=de R --no-environ --no-save' ; echo ''
echo 'and then something like'
echo '       Matrix(1:6, 2,3) %*% Matrix(1:4, 2)'; echo ''
echo 'Commit with something like'
echo "       svn ci -m'translation updates'  po  inst/po"; echo ''
