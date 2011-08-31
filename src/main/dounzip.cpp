/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-11 Andrew R. Runnalls, subject to such other
 *CXXR copyrights and copyright restrictions as may be stated below.
 *CXXR 
 *CXXR CXXR is not part of the R project, and bugs and other issues should
 *CXXR not be reported via r-bugs or other R project channels; instead refer
 *CXXR to the CXXR website.
 *CXXR */

/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file dounzip.c
 *  first part Copyright (C) 2002-9  The R Development Core Team
 *  second part Copyright (C) 1998-2005 Gilles Vollant
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Fileio.h> /* for R_fopen */
#include "unzip.h"
#ifdef HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif
#include <errno.h>

#ifdef Win32
#include <io.h> /* for mkdir */
#endif

/* cf do_dircreate in platform.c */
static int R_mkdir(char *path)
{
#ifdef Win32
    char local[PATH_MAX];
    strcpy(local, path);
    /* need DOS paths on Win 9x */
    R_fixbackslash(local);
    return mkdir(local);
#endif
#ifdef Unix
    return mkdir(path, 0777);
#endif
}


#define BUF_SIZE 4096
static int
extract_one(unzFile uf, const char *const dest, const char * const filename,
	    SEXP names, int *nnames, int overwrite, int junk)
{
    int err = UNZ_OK;
    FILE *fout;
    char  outname[PATH_MAX], dirs[PATH_MAX], buf[BUF_SIZE], *p, *pp;
    char *fn, fn0[PATH_MAX];

    err = unzOpenCurrentFile(uf);
    if (err != UNZ_OK) return err;
    if (strlen(dest) > PATH_MAX - 1) return 1;
    strcpy(outname, dest);
    strcat(outname, FILESEP);
    if (filename) {
	if (strlen(dest) + strlen(filename) > PATH_MAX - 2) return 1;
	strncpy(fn0, filename, PATH_MAX); fn = fn0;
    } else {
	unz_file_info file_info;
	char filename_inzip[PATH_MAX];
	err = unzGetCurrentFileInfo(uf, &file_info, filename_inzip,
				    sizeof(filename_inzip), NULL, 0, NULL, 0);
	fn = filename_inzip;
    }
#ifdef Win32
    R_fixslash(fn);
#endif
    if (junk && strlen(fn) >= 2) { /* need / and basename */
	p = Rf_strrchr(fn, '/');
	if (p) fn = p+1;
    }
    strcat(outname, fn);

#ifdef Win32
    R_fixslash(outname); /* ensure path separator is / */
#endif
    p = outname + strlen(outname) - 1;
    if (*p == '/') { /* Directories are stored with trailing slash */
	if (!junk) {
	    *p = '\0';
	    if (!R_FileExists(outname)) {
		/* make parents as required: have already checked dest exists */
		pp = outname + strlen(dest) + 1;
		while((p = Rf_strchr(pp, '/'))) {
		    strcpy(dirs, outname);
		    dirs[p - outname] = '\0';
		    if (!R_FileExists(dirs)) R_mkdir(dirs);
		    pp = p + 1;
		}
		err = R_mkdir(outname);
	    }
	}
    } else {
	/* make parents as required: have already checked dest exists */
	pp = outname + strlen(dest) + 1;
	while((p = Rf_strrchr(pp, '/'))) {
	    strcpy(dirs, outname);
	    dirs[p - outname] = '\0';
	    /* Rprintf("dirs is %s\n", dirs); */
	    if (!R_FileExists(dirs)) R_mkdir(dirs);
	    pp = p + 1;
	}
	/* Rprintf("extracting %s\n", outname); */
	if (!overwrite && R_FileExists(outname)) {
	    warning(_(" not overwriting file '%s"), outname);
	}   
	fout = R_fopen(outname, "wb");
	if (!fout) {
	    unzCloseCurrentFile(uf);
	    error(_("cannot open file '%s': %s"), outname, strerror(errno));
	    return 3;		/* not reached */
	}
	while (1) {
	    err = unzReadCurrentFile(uf, buf, BUF_SIZE);
	    /* Rprintf("read %d bytes\n", err); */
	    if (err <= 0) break;
	    if (fwrite(buf, err, 1, fout) != 1) { err = -200; break; }
	    if (err < BUF_SIZE) { err = 0; break; }
	}
	fclose(fout);
	SET_STRING_ELT(names, (*nnames)++, mkChar(outname));
    }
    unzCloseCurrentFile(uf);
    return err;
}


static int
zipunzip(const char *zipname, const char *dest, int nfiles, const char **files,
	 SEXP *pnames, int *nnames, int overwrite, int junk)
{
    int   i, err = UNZ_OK;
    unzFile uf;
    SEXP names = *pnames;

    uf = unzOpen(zipname);
    if (!uf) return 1;
    if (nfiles == 0) { /* all files */
	unz_global_info gi;
	unzGetGlobalInfo(uf, &gi);
	for (i = 0; i < CXXRCONSTRUCT(int, gi.number_entry); i++) {
	    if (i > 0) if ((err = unzGoToNextFile(uf)) != UNZ_OK) break;
	    if (*nnames+1 >= LENGTH(names)) {
		SEXP onames = names;
		names = allocVector(STRSXP, 2*LENGTH(names));
		UNPROTECT(1);
		PROTECT(names);
		copyVector(names, onames);
	    }
	    if ((err = extract_one(uf, dest, NULL, names, nnames, 
				   overwrite, junk)) != UNZ_OK) break;
#ifdef Win32
	    R_ProcessEvents();
#else
	    R_CheckUserInterrupt();
#endif
	}
    } else {
	for (i = 0; i < nfiles; i++) {
	    if ((err = unzLocateFile(uf, files[i], 1)) != UNZ_OK) break;
	    if ((err = extract_one(uf, dest, files[i], names, nnames, 
				   overwrite, junk)) != UNZ_OK) break;
#ifdef Win32
	    R_ProcessEvents();
#else
	    R_CheckUserInterrupt();
#endif
	}
    }
    *pnames = names;
    unzClose(uf);
    return err;
}

static SEXP ziplist(const char *zipname)
{
    SEXP ans = R_NilValue, names, lengths, dates;
    unzFile uf;
    uLong i;
    unz_global_info gi;
    int err, nfiles;

    uf = unzOpen(zipname);
    if (!uf) error(_("zip file '%s' cannot be opened"), zipname);

    gi.number_entry = 0; /* =Wall */
    err = unzGetGlobalInfo (uf, &gi);
    if (err != UNZ_OK)
        error("error %d with zipfile in unzGetGlobalInfo", err);
    nfiles = gi.number_entry;
    /* name, length, datetime */
    PROTECT(ans = allocVector(VECSXP, 3));
    SET_VECTOR_ELT(ans, 0, names = allocVector(STRSXP, nfiles));
    SET_VECTOR_ELT(ans, 1, lengths = allocVector(INTSXP, nfiles));
    SET_VECTOR_ELT(ans, 2, dates = allocVector(STRSXP, nfiles));

    for (i = 0; CXXRCONSTRUCT(int, i) < nfiles; i++) {
        char filename_inzip[PATH_MAX], date[50];
        unz_file_info file_info;

        err = unzGetCurrentFileInfo(uf, &file_info, filename_inzip, 
				    sizeof(filename_inzip), NULL, 0, NULL, 0);
        if (err != UNZ_OK)
            error("error %d with zipfile in unzGetCurrentFileInfo\n", err);
	SET_STRING_ELT(names, i, mkChar(filename_inzip));
	INTEGER(lengths)[i] = file_info.uncompressed_size;
	snprintf(date, 50, "%d-%02d-%02d %02d:%02d",
		 file_info.tmu_date.tm_year,
		 file_info.tmu_date.tm_mon + 1,
		 file_info.tmu_date.tm_mday,
		 file_info.tmu_date.tm_hour,
		 file_info.tmu_date.tm_min);
	SET_STRING_ELT(dates, i, mkChar(date));

        if (CXXRCONSTRUCT(int, i) < nfiles - 1) {
            err = unzGoToNextFile(uf);
            if (err != UNZ_OK)
                error("error %d with zipfile in unzGoToNextFile\n",err);
        }
    }
    unzClose(uf);

    UNPROTECT(1);
    return ans;
}


SEXP attribute_hidden do_unzip(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP  fn, ans, names = R_NilValue;
    char  zipname[PATH_MAX], dest[PATH_MAX];
    const char *p, **topics = NULL;
    int   i, ntopics, list, overwrite, junk, rc, nnames = 0;

    if (!isString(CAR(args)) || LENGTH(CAR(args)) != 1)
	error(_("invalid zip name argument"));
    p = R_ExpandFileName(translateChar(STRING_ELT(CAR(args), 0)));
    if (strlen(p) > PATH_MAX - 1)
	error(_("zip path is too long"));
    strcpy(zipname, p);
    args = CDR(args);
    fn = CAR(args);
    ntopics = length(fn);
    if (ntopics > 0) {
	if (!isString(fn))
	    error(_("invalid '%s' argument"), "files");
	topics = reinterpret_cast<const char **>( R_alloc(ntopics, sizeof(char *)));
	for (i = 0; i < ntopics; i++)
	    topics[i] = translateChar(STRING_ELT(fn, i));
    }
    args = CDR(args);
    if (!isString(CAR(args)) || LENGTH(CAR(args)) != 1)
	error(_("invalid '%s' argument"), "exdir");
    p = R_ExpandFileName(translateChar(STRING_ELT(CAR(args), 0)));
    if (strlen(p) > PATH_MAX - 1)
	error(_("'exdir' is too long"));
    strcpy(dest, p);
    if (!R_FileExists(dest))
	error(_("'exdir' does not exist"));
    args = CDR(args);
    list = asLogical(CAR(args));
    if (list == NA_LOGICAL)
	error(_("invalid '%s' argument"), "list");
    if (list) return(ziplist(zipname));
    args = CDR(args);
    overwrite = asLogical(CAR(args));
    if (overwrite == NA_LOGICAL)
	error(_("invalid '%s' argument"), "overwrite");
    args = CDR(args);
    junk = asLogical(CAR(args));
    if (junk == NA_LOGICAL)
	error(_("invalid '%s' argument"), "junkpaths");

    if (ntopics > 0)
	PROTECT(names = allocVector(STRSXP, ntopics));
    else
	PROTECT(names = allocVector(STRSXP, 5000));
    rc = zipunzip(zipname, dest, ntopics, topics, &names, &nnames, 
		  overwrite, junk);
    if (rc != UNZ_OK)
	switch(rc) {
	case UNZ_END_OF_LIST_OF_FILE:
	    warning(_("requested file not found in the zip file"));
	    break;
	case UNZ_BADZIPFILE:
	    warning(_("zip file is corrupt"));
	    break;
	case UNZ_CRCERROR:
	    warning(_("CRC error in zip file"));
	    break;
	case UNZ_PARAMERROR:
	case UNZ_INTERNALERROR:
	    warning(_("internal error in unz code"));
	    break;
	case -200:
	    warning(_("write error in extracting from zip file"));
	    break;
	default:
	    warning(_("error %d in extracting from zip file"), rc);
	}
    PROTECT(ans = ScalarInteger(rc));
    PROTECT(names = lengthgets(names, nnames));
    setAttrib(ans, install("extracted"), names);
    UNPROTECT(3);
    return ans;
}

/* ------------------- unz connections --------------------- */

#include <Rconnections.h>

typedef struct unzconn {
    void *uf;
} *Runzconn;

static Rboolean unz_open(Rconnection con)
{
    unzFile uf;
    char path[2*PATH_MAX], *p;
    const char *tmp;

    if(con->mode[0] != 'r') {
	warning(_("unz connections can only be opened for reading"));
	return FALSE;
    }
    tmp = R_ExpandFileName(con->description);
    if (strlen(tmp) > PATH_MAX - 1) {
	warning(_("zip path is too long"));
	return FALSE;
    }
    strcpy(path, tmp);
    p = Rf_strrchr(path, ':');
    if(!p) {
	warning(_("invalid description of unz connection"));
	return FALSE;
    }
    *p = '\0';
    uf = unzOpen(path);
    if(!uf) {
	warning(_("cannot open zip file '%s'"), path);
	return FALSE;
    }
    if (unzLocateFile(uf, p+1, 1) != UNZ_OK) {
	warning(_("cannot locate file '%s' in zip file '%s'"), p+1, path);
	unzClose(uf);
	return FALSE;
    }
    unzOpenCurrentFile(uf);
    (static_cast<Runzconn>((con->connprivate)))->uf = uf;
    con->isopen = TRUE;
    con->canwrite = FALSE;
    con->canread = TRUE;
    if(strlen(con->mode) >= 2 && con->mode[1] == 'b') con->text = FALSE;
    else con->text = TRUE;
    /* set_iconv(); not yet */
    con->save = -1000;
    return TRUE;
}

static void unz_close(Rconnection con)
{
    unzFile uf = (static_cast<Runzconn>((con->connprivate)))->uf;
    unzCloseCurrentFile(uf);
    unzClose(uf);
    con->isopen = FALSE;
}

static int unz_fgetc_internal(Rconnection con)
{
    unzFile uf = (static_cast<Runzconn>((con->connprivate)))->uf;
    char buf[1];
    int err, p;

    err = unzReadCurrentFile(uf, buf, 1);
    p = buf[0] % 256;
    return (err < 1) ? R_EOF : p;
}

static size_t unz_read(void *ptr, size_t size, size_t nitems,
		       Rconnection con)
{
    unzFile uf = (static_cast<Runzconn>((con->connprivate)))->uf;
    return unzReadCurrentFile(uf, ptr, size*nitems)/size;
}

static int null_vfprintf(Rconnection con, const char *format, va_list ap)
{
    error(_("printing not enabled for this connection"));
    return 0; /* -Wall */
}

static size_t null_write(const void *ptr, size_t size, size_t nitems,
			 Rconnection con)
{
    error(_("write not enabled for this connection"));
    return 0; /* -Wall */
}

static double null_seek(Rconnection con, double where, int origin, int rw)
{
    error(_("seek not enabled for this connection"));
    return 0; /* -Wall */
}

static int null_fflush(Rconnection con)
{
    return 0;
}

Rconnection attribute_hidden
R_newunz(const char *description, const char *const mode)
{
    Rconnection newconn;
    newconn = static_cast<Rconnection>( malloc(sizeof(struct Rconn)));
    if(!newconn) error(_("allocation of unz connection failed"));
    newconn->connclass = static_cast<char *>( malloc(strlen("unz") + 1));
    if(!newconn->connclass) {
	free(newconn);
	error(_("allocation of unz connection failed"));
    }
    strcpy(newconn->connclass, "unz");
    newconn->description = static_cast<char *>( malloc(strlen(description) + 1));
    if(!newconn->description) {
	free(newconn->connclass); free(newconn);
	error(_("allocation of unz connection failed"));
    }
    init_con(newconn, description, CE_NATIVE, mode);

    newconn->canseek = TRUE;
    newconn->open = &unz_open;
    newconn->close = &unz_close;
    newconn->vfprintf = &null_vfprintf;
    newconn->fgetc_internal = &unz_fgetc_internal;
    newconn->fgetc = &dummy_fgetc;
    newconn->seek = &null_seek;
    newconn->fflush = &null_fflush;
    newconn->read = &unz_read;
    newconn->write = &null_write;
    newconn->connprivate = CXXRNOCAST(void *) malloc(sizeof(struct unzconn));
    if(!newconn->connprivate) {
	free(newconn->description); free(newconn->connclass); free(newconn);
	error(_("allocation of unz connection failed"));
    }
    return newconn;
}
