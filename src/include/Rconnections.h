/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-13 Andrew R. Runnalls, subject to such other
 *CXXR copyrights and copyright restrictions as may be stated below.
 *CXXR 
 *CXXR CXXR is not part of the R project, and bugs and other issues should
 *CXXR not be reported via r-bugs or other R project channels; instead refer
 *CXXR to the CXXR website.
 *CXXR */

/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000-2011   The R Core Team.
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

#ifndef R_CONNECTIONS_H_
#define R_CONNECTIONS_H_
#include <R_ext/Boolean.h>

/* NB: this is a private header, and not installed.  The internals of
   connections are private and subject to change without notice. */

#if defined(HAVE_OFF_T) && defined(HAVE_FSEEKO) && defined(HAVE_SYS_TYPES_H)
#include <sys/types.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* this allows the opaque pointer definition to be made available 
   in Rinternals.h */
#ifndef HAVE_RCONNECTION_TYPEDEF
typedef struct Rconn  *Rconnection;
#endif
struct Rconn {
    char* connclass;
    char* description;
    int enc; /* the encoding of 'description' */
    char mode[5];
    Rboolean text, isopen, incomplete, canread, canwrite, canseek, blocking, 
	isGzcon;
    Rboolean (*open)(struct Rconn *);
    void (*close)(struct Rconn *); /* routine closing after auto open */
    void (*destroy)(struct Rconn *); /* when closing connection */
    int (*vfprintf)(struct Rconn *, const char *, va_list);
    int (*fgetc)(struct Rconn *);
    int (*fgetc_internal)(struct Rconn *);
    double (*seek)(struct Rconn *, double, int, int);
    void (*truncate)(struct Rconn *);
    int (*fflush)(struct Rconn *);
    size_t (*read)(void *, size_t, size_t, struct Rconn *);
    size_t (*write)(const void *, size_t, size_t, struct Rconn *);
    int nPushBack, posPushBack; /* number of lines, position on top line */
    char **PushBack;
    int save, save2;
    char encname[101];
    /* will be iconv_t, which is a pointer. NULL if not in use */
    void *inconv, *outconv;
    /* The idea here is that no MBCS char will ever not fit */
    char iconvbuff[25], oconvbuff[50], *next, init_out[25];
    short navail, inavail;
    Rboolean EOF_signalled;
    Rboolean UTF8out;
    void *id;
    void *ex_ptr;
    void *connprivate;
};


typedef enum {HTTPsh, FTPsh, HTTPSsh} UrlScheme;

/* used in internet module */
typedef struct urlconn {
    void *ctxt;
    UrlScheme type;
} *Rurlconn;

/* used in internet module */
typedef struct sockconn {
    int port;
    int server;
    int fd;
    int timeout;
    char *host;
    char inbuf[4096], *pstart, *pend;
} *Rsockconn;

/* used in X11 module */
typedef struct clpconn {
    char *buff;
    int pos, len, last, sizeKB;
    Rboolean warned;
} *Rclpconn;

#define init_con	Rf_init_con
#define con_pushback	Rf_con_pushback

int Rconn_fgetc(Rconnection con);
int Rconn_ungetc(int c, Rconnection con);
int Rconn_getline(Rconnection con, char *buf, int bufsize);
int Rconn_printf(Rconnection con, const char *format, ...);
Rconnection getConnection(int n);
Rconnection getConnection_no_err(int n);
Rboolean switch_stdout(int icon, int closeOnExit);
void init_con(Rconnection newconn, const char *description, int enc,
	      const char * const mode);
Rconnection R_newurl(const char *description, const char * const mode);
Rconnection R_newsock(const char *host, int port, int server, const char * const mode, int timeout);
Rconnection in_R_newsock(const char *host, int port, int server, const char *const mode, int timeout);
Rconnection R_newunz(const char *description, const char * const mode);
int dummy_fgetc(Rconnection con);
int dummy_vfprintf(Rconnection con, const char *format, va_list ap);
int getActiveSink(int n);
void con_pushback(Rconnection con, Rboolean newLine, char *line);
size_t R_WriteConnection(Rconnection con, const void *buf, size_t n);

int Rsockselect(int nsock, int *insockfd, int *ready, int *write, double timeout);

#define set_iconv Rf_set_iconv
void set_iconv(Rconnection con);

#ifdef __cplusplus
}
#endif

#endif /* R_CONNECTIONS_H_ */

