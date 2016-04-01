/* $Id$
 *
 * This file is part of Rho, a project to refactor the R interpreter
 * into C++.  It may consist in whole or in part of program code and
 * documentation taken from the R project itself, incorporated into
 * Rho (and possibly MODIFIED) under the terms of the GNU General Public
 * Licence.
 * 
 * Rho is Copyright (C) 2008-14 Andrew R. Runnalls, subject to such other
 * copyrights and copyright restrictions as may be stated below.
 * 
 * Rho is not part of the R project, and bugs and other issues should
 * not be reported via r-bugs or other R project channels; instead refer
 * to the Rho website.
 * */

#ifndef R_INTERNET_MODULE_H
#define R_INTERNET_MODULE_H


#include <Rinternals.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef SEXP (*R_DownloadRoutine)(SEXP args);
typedef Rconnection (*R_NewUrlRoutine)(const char *description, const char * const mode, int method);
typedef Rconnection (*R_NewSockRoutine)(const char *host, int port, int server, const char *const mode, int timeout); 

typedef void * (*R_HTTPOpenRoutine)(const char *url, const char *headers, const int cacheOK);
typedef int    (*R_HTTPReadRoutine)(void *ctx, char *dest, int len);
typedef void   (*R_HTTPCloseRoutine)(void *ctx);
	      
typedef void * (*R_FTPOpenRoutine)(const char *url);
typedef int    (*R_FTPReadRoutine)(void *ctx, char *dest, int len);
typedef void   (*R_FTPCloseRoutine)(void *ctx);

typedef void   (*R_SockOpenRoutine)(int *port);
typedef void   (*R_SockListenRoutine)(int *sockp, char **buf, int *len);
typedef void   (*R_SockConnectRoutine)(int *port, char **host);
typedef void   (*R_SockCloseRoutine)(int *sockp);

typedef void   (*R_SockReadRoutine)(int *sockp, char **buf, int *maxlen);
typedef void   (*R_SockWriteRoutine)(int *sockp, char **buf, int *start, int *end, int *len);
typedef int    (*R_SockSelectRoutine)(int nsock, int *insockfd, int *ready, int *write, double timeout);

typedef int    (*R_HTTPDCreateRoutine)(const char *ip, int port);
typedef void   (*R_HTTPDStopRoutine)();

typedef SEXP (*R_CurlRoutine)(SEXP call, SEXP op, SEXP args, SEXP rho);

typedef struct {
    R_DownloadRoutine download;
    R_NewUrlRoutine   newurl;
    R_NewSockRoutine  newsock;

    R_HTTPOpenRoutine  HTTPOpen;
    R_HTTPReadRoutine  HTTPRead;
    R_HTTPCloseRoutine HTTPClose;

    R_FTPOpenRoutine   FTPOpen;
    R_FTPReadRoutine   FTPRead;
    R_FTPCloseRoutine  FTPClose;

    R_SockOpenRoutine     sockopen;
    R_SockListenRoutine   socklisten;
    R_SockConnectRoutine  sockconnect;
    R_SockCloseRoutine    sockclose;

    R_SockReadRoutine     sockread;
    R_SockWriteRoutine    sockwrite;
    R_SockSelectRoutine   sockselect;

    R_HTTPDCreateRoutine  HTTPDCreate;
    R_HTTPDStopRoutine    HTTPDStop;

    R_CurlRoutine curlVersion;
    R_CurlRoutine curlGetHeaders;
    R_CurlRoutine curlDownload;
    R_NewUrlRoutine   newcurlurl;
} R_InternetRoutines;

R_InternetRoutines *R_setInternetRoutines(R_InternetRoutines *routines);

#ifdef __cplusplus
}
#endif

#endif /* ifndef R_INTERNET_MODULE_H */
