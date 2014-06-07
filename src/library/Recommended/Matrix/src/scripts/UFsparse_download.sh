#!/bin/sh
## Update Libraries from Tim Davis' University of Florida (UF) collection:
## Note that cs.c , cs.h  are done in ./CSparse_install.sh
#
if [ ! -d ../src ]
then echo 'Must run in Matrix/src/ !' ; exit 1
fi
getSPQR=no
##     --- since late summer 2010, we no longer get SPQR
ufl_URL=http://www.cise.ufl.edu/research/sparse
wget -nc  $ufl_URL/amd/current/AMD.tar.gz
wget -nc  $ufl_URL/cholmod/current/CHOLMOD.tar.gz
wget -nc  $ufl_URL/colamd/current/COLAMD.tar.gz
wget -nc  $ufl_URL/SuiteSparse_config/current/SuiteSparse_config.tar.gz
if [ $getSPQR = yes ] ; then
wget -nc  $ufl_URL/SPQR/current/SPQR.tar.gz
fi

## 1) SuiteSparse_config ---------------------------------------------
  ## install SuiteSparse_config.h file (now needed by some SuiteSparse libraries)
tar zxf SuiteSparse_config.tar.gz SuiteSparse_config/SuiteSparse_config.h SuiteSparse_config/README.txt SuiteSparse_config/SuiteSparse_config.c SuiteSparse_config/Makefile
  ## Move the SuiteSparse_config/README.txt file to ../inst/doc/SuiteSparse/SuiteSparse_config.txt
mv SuiteSparse_config/README.txt ../inst/doc/SuiteSparse/SuiteSparse_config.txt
  ## touch the file SuiteSparse_config/SuiteSparse_config.mk.  We use other configuration
  ## environment variables but this name is embedded in some Makefiles
touch SuiteSparse_config/SuiteSparse_config.mk
  ## Need to add the Matrix-specific changes to SuiteSparse_config/SuiteSparse_config.h :
## 2011-04: *no longer* !!  patch -p0 < scripts/UFconfig.patch
## again for printf():
## 2012-06: *no longer* patch -p0 < scripts/SuiteSparse_config.patch

## 2) COLAMD -----------------------------------------------
   ## install COLAMD/Source and COLAMD/Include directories
tar zxf COLAMD.tar.gz COLAMD/Source/ COLAMD/Include/
Rscript --vanilla -e 'source("scripts/fixup-fn.R")' -e 'fixup("COLAMD/Source/Makefile")'
  ## install documentation for COLAMD
tar zxf COLAMD.tar.gz COLAMD/README.txt COLAMD/Doc
mv COLAMD/README.txt ../inst/doc/UFsparse/COLAMD.txt
patch -p0 < scripts/COLAMD.patch

## 3) AMD --------------------------------------------------
  ## install AMD/Source, AMD/Include and AMD/Lib directories
tar zxf AMD.tar.gz AMD/Source AMD/Include AMD/Lib AMD/README.txt
  ## restore AMD/Lib/Makefile
##svn revert AMD/Lib/Makefile
  ## install AMD documentation
mv AMD/README.txt ../inst/doc/SuiteSparse/AMD.txt
  ## remove Fortran source files and GNUMakefile
rm AMD/Source/*.f AMD/Lib/GNUmakefile
#(for f in AMD/Include/amd_internal.h AMD/Source/amd_global.c; do diff -ubBw ${f}.~1~ $f ; done ) | tee scripts/AMD-noprint.patch
patch -p0 < scripts/AMD-noprint.patch

## 4) CHOLMOD ----------------------------------------------
  ## install CHOLMOD source files
for d in Check Cholesky Core Include Lib MatrixOps Modify Partition Supernodal
do
    tar zxf ./CHOLMOD.tar.gz CHOLMOD/$d
done
  ## install CHOLMOD documentation in ../inst/doc/UFsparse
tar zxf ./CHOLMOD.tar.gz CHOLMOD/README.txt
mv CHOLMOD/README.txt ../inst/doc/UFsparse/CHOLMOD.txt

cp -p CHOLMOD/Lib/Makefile CHOLMOD/Lib/Makefile_CHOLMOD
Rscript --vanilla -e 'source("scripts/fixup-fn.R")' -e 'fixup("CHOLMOD/Lib/Makefile")'
mv    CHOLMOD/Lib/Makefile CHOLMOD/Lib/Makefile_pre
svn revert CHOLMOD/Lib/Makefile
  ##
ls -l CHOLMOD/Lib/Makefile_pre
echo 'now diff CHOLMOD/Lib/Makefile with CHOLMOD/Lib/Makefile_pre'
echo ' make changes as necessary, and then (later)'
echo ' rm CHOLMOD/Lib/Makefile_*' ; echo

## 5) SPQR -------------------------------------------------
if [ $getSPQR = yes ]
then
  ## install SPQR source files
  for d in Source Include Lib
  do
      tar zxf ./SPQR.tar.gz SPQR/$d
  done
    ## install CHOLMOD documentation in ../inst/doc/UFsparse
  tar zxf ./SPQR.tar.gz SPQR/README.txt
  mv SPQR/README.txt ../inst/doc/UFsparse/SPQR.txt
    ## patch for Matrix:
  patch -p0 < scripts/SPQR.patch

  cp -p SPQR/Lib/Makefile SPQR/Lib/Makefile_SPQR
  Rscript --vanilla -e 'source("scripts/fixup-fn.R")' -e 'fixup("SPQR/Lib/Makefile")'
  mv    SPQR/Lib/Makefile SPQR/Lib/Makefile_pre
  svn revert SPQR/Lib/Makefile
    ##
  ls -l SPQR/Lib/Makefile_pre
  echo 'now diff SPQR/Lib/Makefile with SPQR/Lib/Makefile_pre'
  echo ' make changes as necessary, and then (later)'
  echo ' rm SPQR/Lib/Makefile_*' ; echo
fi

## ----- remove the downloaded tar files -------------------
echo 'Execute this eventually:

rm CHOLMOD.tar.gz AMD.tar.gz COLAMD.tar.gz UFconfig.tar.gz SPQR.tar.gz
'
