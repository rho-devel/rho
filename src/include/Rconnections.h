/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000-2015   The R Core Team.
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
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

/* Internal header, not installed */

#ifndef R_CONNECTIONS_H_
#define R_CONNECTIONS_H_

/* the visible part of the connections */
#include <R_ext/Connections.h>

#ifdef __cplusplus
extern "C" {
#endif


/* NB: this is a private header, and not installed.  The internals of
       connections are private and subject to change without notice.
       A subset can be accessed using R_ext/Connections.h but read
       the warning in that file for details. */

typedef enum {HTTPsh, FTPsh, HTTPSsh, FTPSsh} UrlScheme;

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
Rconnection R_newurl(const char *description, const char * const mode, int type);
Rconnection R_newsock(const char *host, int port, int server, const char * const mode, int timeout);
Rconnection in_R_newsock(const char *host, int port, int server, const char *const mode, int timeout);
Rconnection R_newunz(const char *description, const char * const mode);
int dummy_fgetc(Rconnection con);
int dummy_vfprintf(Rconnection con, const char *format, va_list ap);
int getActiveSink(int n);
void con_pushback(Rconnection con, Rboolean newLine, char *line);

int Rsockselect(int nsock, int *insockfd, int *ready, int *write, double timeout);

#define set_iconv Rf_set_iconv
void set_iconv(Rconnection con);

#ifdef __cplusplus
}
#endif

#endif /* R_CONNECTIONS_H_ */

