/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-2015   The R Core Team.
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the CXXR Project Authors.
 *
 *  CXXR is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the CXXR website.
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
 *  https://www.R-project.org/Licenses/
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Internal.h>

#include <Rconnections.h>
#include <Rdynpriv.h>
#include <R_ext/R-ftp-http.h>
#include <Rmodules/Rinternet.h>
#include "basedecl.h"

static R_InternetRoutines routines, *ptr = &routines;


/*
SEXP Rdownload(SEXP args);
Rconnection R_newurl(char *description, char *mode);
Rconnection R_newsock(char *host, int port, int server, char *mode, int timeout);


Next 6 are for use by libxml, only

void *R_HTTPOpen(const char *url);
int   R_HTTPRead(void *ctx, char *dest, int len);
void  R_HTTPClose(void *ctx);

void *R_FTPOpen(const char *url);
int   R_FTPRead(void *ctx, char *dest, int len);
void  R_FTPClose(void *ctx);

void Rsockopen(int *port)
void Rsocklisten(int *sockp, char **buf, int *len)
void Rsockconnect(int *port, char **host)
void Rsockclose(int *sockp)
void Rsockread(int *sockp, char **buf, int *maxlen)
void Rsockwrite(int *sockp, char **buf, int *start, int *end, int *len)

int Rsockselect(int nsock, int *insockfd, int *ready, int *write,
		double timeout)

int R_HTTPDCreate(const char *ip, int port);
void R_HTTPDStop(void);
 */

static int initialized = 0;

R_InternetRoutines *
R_setInternetRoutines(R_InternetRoutines *routines)
{
    R_InternetRoutines *tmp;
    tmp = ptr;
    ptr = routines;
    return(tmp);
}

static void internet_Init(void)
{
    int res;
    res = R_moduleCdynload("internet", 1, 1);
    initialized = -1;
    if(!res) return;
    if(!ptr->download)
	error(_("internet routines cannot be accessed in module"));
    initialized = 1;
    return;
}

extern "C"
SEXP Rdownload(SEXP args)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->download)(args);
    else {
	error(_("internet routines cannot be loaded"));
	return R_NilValue;
    }
}

Rconnection attribute_hidden 
R_newurl(const char *description, const char * const mode, int type)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->newurl)(description, mode, type);
    else {
	error(_("internet routines cannot be loaded"));
	return CXXRNOCAST(Rconnection)nullptr;
    }
}

Rconnection attribute_hidden
R_newsock(const char *host, int port, int server, const char * const mode,
	  int timeout)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->newsock)(host, port, server, mode, timeout);
    else {
	error(_("internet routines cannot be loaded"));
	return CXXRNOCAST(Rconnection)nullptr;
    }
}

void *R_HTTPOpen(const char *url)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->HTTPOpen)(url, nullptr, 0);
    else {
	error(_("internet routines cannot be loaded"));
	return nullptr;
    }
}

int   R_HTTPRead(void *ctx, char *dest, int len)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->HTTPRead)(ctx, dest, len);
    else {
	error(_("internet routines cannot be loaded"));
	return 0;
    }
}

void  R_HTTPClose(void *ctx)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->HTTPClose)(ctx);
    else
	error(_("internet routines cannot be loaded"));
}

void *R_FTPOpen(const char *url)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->FTPOpen)(url);
    else {
	error(_("internet routines cannot be loaded"));
	return nullptr;
    }
}

int   R_FTPRead(void *ctx, char *dest, int len)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->FTPRead)(ctx, dest, len);
    else {
	error(_("internet routines cannot be loaded"));
	return 0;
    }
}

void  R_FTPClose(void *ctx)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->FTPClose)(ctx);
    else
	error(_("internet routines cannot be loaded"));
}

extern "C"
int extR_HTTPDCreate(const char *ip, int port)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->HTTPDCreate)(ip, port);
    else
	error(_("internet routines cannot be loaded"));
    return -1;
}

extern "C"
void extR_HTTPDStop(void)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->HTTPDStop)();
    else
	error(_("internet routines cannot be loaded"));
}

extern "C"
SEXP Rsockconnect(SEXP sport, SEXP shost)
{
    if (length(sport) != 1) error("invalid 'socket' argument");
    int port = asInteger(sport);
    char *host[1];
    host[0] = const_cast<char *>( translateChar(STRING_ELT(shost, 0)));
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->sockconnect)(&port, host);
    else
	error(_("socket routines cannot be loaded"));
    return ScalarInteger(port); // The socket number
}

extern "C"
SEXP Rsockread(SEXP ssock, SEXP smaxlen)
{
    if (length(ssock) != 1) error("invalid 'socket' argument");
    int sock = asInteger(ssock), maxlen = asInteger(smaxlen);
    char buf[maxlen+1], *abuf[1];
    abuf[0] = buf;
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->sockread)(&sock, abuf, &maxlen);
    else
	error(_("socket routines cannot be loaded"));
    SEXP ans = PROTECT(allocVector(STRSXP, 1));
    SET_STRING_ELT(ans, 0, mkCharLen(buf, maxlen));
    UNPROTECT(1);
    return ans;
		       
}

extern "C"
SEXP Rsockclose(SEXP ssock)
{
    if (length(ssock) != 1) error("invalid 'socket' argument");
    int sock = asInteger(ssock);
    if (sock <= 0) error(_("attempt to close invalid socket"));
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->sockclose)(&sock);
    else
	error(_("socket routines cannot be loaded"));
    return ScalarLogical(sock);
}

extern "C"
SEXP Rsockopen(SEXP sport)
{
    if (length(sport) != 1) error("invalid 'port' argument");
    int port = asInteger(sport);
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->sockopen)(&port);
    else
	error(_("socket routines cannot be loaded"));
    return ScalarInteger(port); // The socket number
}

extern "C"
SEXP Rsocklisten(SEXP ssock)
{
    if (length(ssock) != 1) error("invalid 'socket' argument");
    int sock = asInteger(ssock), len = 256;
    char buf[257], *abuf[1];
    abuf[0] = buf;
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->socklisten)(&sock, abuf, &len);
    else
	error(_("socket routines cannot be loaded"));
    SEXP ans = PROTECT(ScalarInteger(sock)); // The socket being listened on
    SEXP host = PROTECT(allocVector(STRSXP, 1));
    SET_STRING_ELT(host, 0, mkChar(buf));
    setAttrib(ans, install("host"), host);
    UNPROTECT(2);
    return ans;
}

extern "C"
SEXP Rsockwrite(SEXP ssock, SEXP sstring)
{
    if (length(ssock) != 1) error("invalid 'socket' argument");
    int sock = asInteger(ssock), start = 0, end, len;
    char *buf = const_cast<char *>( translateChar(STRING_ELT(sstring, 0))), *abuf[1];
    end = len = int( strlen(buf));
    abuf[0] = buf;
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->sockwrite)(&sock, abuf, &start, &end, &len);
    else
	error(_("socket routines cannot be loaded"));
    return ScalarInteger(len);
}


attribute_hidden
int Rsockselect(int nsock, int *insockfd, int *ready, int *write,
		double timeout)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->sockselect)(nsock, insockfd, ready, write, timeout);
    else {
	error(_("socket routines cannot be loaded"));
	return 0;
    }
}

SEXP attribute_hidden do_curlVersion(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->curlVersion)(call, op, args, rho);
    else {
	error(_("internet routines cannot be loaded"));
	return R_NilValue;
    }
}

SEXP attribute_hidden do_curlGetHeaders(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->curlGetHeaders)(call, op, args, rho);
    else {
	error(_("internet routines cannot be loaded"));
	return R_NilValue;
    }
}

SEXP attribute_hidden do_curlDownload(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->curlDownload)(call, op, args, rho);
    else {
	error(_("internet routines cannot be loaded"));
	return R_NilValue;
    }
}

Rconnection attribute_hidden
R_newCurlUrl(const char *description, const char * const mode, int type)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->newcurlurl)(description, mode, type);
    else {
	error(_("internet routines cannot be loaded"));
	return (Rconnection)0;
    }
    return (Rconnection)0; /* -Wall */
}
