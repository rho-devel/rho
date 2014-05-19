/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-14 Andrew R. Runnalls, subject to such other
 *CXXR copyrights and copyright restrictions as may be stated below.
 *CXXR 
 *CXXR CXXR is not part of the R project, and bugs and other issues should
 *CXXR not be reported via r-bugs or other R project channels; instead refer
 *CXXR to the CXXR website.
 *CXXR */

/* Compatibility wrapper for R */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
int trio_printf(const char *format, ...);
int trio_vprintf(const char *format, va_list args);
int trio_fprintf(FILE *file, const char *format, ...);
int trio_sprintf(char *buffer, const char *format, ...);
int trio_vsprintf(char *buffer, const char *format, va_list args);
int trio_vfprintf(FILE *file, const char *format, va_list args);
int trio_snprintf(char *buffer, size_t max, const char *format, ...);
int trio_vsnprintf(char *buffer, size_t bufferSize, const char *format,
		   va_list args);
int trio_vasprintf(char **ret, const char *format, va_list args);


int printf(const char *format, ...)
{
    int res;
    va_list(ap);
    va_start(ap, format);
    res = trio_vprintf(format, ap);
    va_end(ap);
    return res;
}

int fprintf(FILE *file, const char *format, ...)
{
    int res;
    va_list(ap);
    va_start(ap, format);
    res = trio_vfprintf(file, format, ap);
    va_end(ap);
    return res;
}

int sprintf(char *buffer, const char *format, ...)
{
    int res;
    va_list(ap);
    va_start(ap, format);
    res = trio_vsprintf(buffer, format, ap);
    va_end(ap);
    return res;
}


int vprintf(const char *format, va_list args)
{
    return trio_vprintf(format, args);
}


int vsprintf(char *buffer, const char *format, va_list args)
{
    return trio_vsprintf(buffer, format, args);
}

int vfprintf(FILE *file, const char *format, va_list args)
{
    return trio_vfprintf(file, format, args);
}

int snprintf(char *buffer, size_t max, const char *format, ...)
{
    int res;
    va_list(ap);
    va_start(ap, format);
    res = trio_vsnprintf(buffer, max, format, ap);
    va_end(ap);
    return res;
}

int vsnprintf(char *buffer, size_t bufferSize, const char *format, va_list args)
{
    return trio_vsnprintf(buffer, bufferSize, format, args);
}

#ifndef _W64
/* This is needed as MinGW's stdio.h has an inline vnsprintf mapping to
   _vsnprintf: MinGW-w64's maps to __imp__vsnprintf */
int _vsnprintf(char *buffer, size_t bufferSize, const char *format, va_list args)
{
    return trio_vsnprintf(buffer, bufferSize, format, args);
}
#endif

int vasprintf(char **ret, const char *format, va_list args)
{
    return trio_vasprintf(ret, format, args);
}
