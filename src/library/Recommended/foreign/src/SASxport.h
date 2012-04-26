/*
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

/*
 *  This file is derived from code in the SAS Technical Support
 *  document TS-140 "The Record Layout of a Data Set in SAS Transport
 *  (XPORT) Format" available as
 *       http://ftp.sas.com/techsup/download/technote/ts140.html
 */

#ifndef SASEXPORT_H
#define SASEXPORT_H

#include <string.h>		/* for memcpy and memset */
#include "foreign.h"
#include "swap_bytes.h"

/* double cnxptiee(double from, int fromtype, int totype); */



struct SAS_XPORT_header {
  char sas_symbol[2][8];	/* should be "SAS     " */
  char saslib[8];		/* should be "SASLIB  " */
  char sasver[8];
  char sas_os[8];
  char sas_create[16];
  char sas_mod[16];
};

struct SAS_XPORT_member {
  char sas_symbol[8];
  char sas_dsname[8];
  char sasdata[8];
  char sasver[8];
  char sas_osname[8];
  char sas_create[16];
  char sas_mod[16];
};

struct SAS_XPORT_namestr {
    short   ntype;              /* VARIABLE TYPE: 1=NUMERIC, 2=CHAR    */
    short   nhfun;              /* HASH OF NNAME (always 0)            */
    short   nlng;               /* LENGTH OF VARIABLE IN OBSERVATION   */
    short   nvar0;              /* VARNUM                              */
    char    nname[8];		/* NAME OF VARIABLE                    */
    char    nlabel[40];		/* LABEL OF VARIABLE                   */
    char    nform[8];		/* NAME OF FORMAT                      */
    short   nfl;                /* FORMAT FIELD LENGTH OR 0            */
    short   nfd;                /* FORMAT NUMBER OF DECIMALS           */
    short   nfj;                /* 0=LEFT JUSTIFICATION, 1=RIGHT JUST  */
    char    nfill[2];           /* (UNUSED, FOR ALIGNMENT AND FUTURE)  */
    char    niform[8];		/* NAME OF INPUT FORMAT                */
    short   nifl;               /* INFORMAT LENGTH ATTRIBUTE           */
    short   nifd;               /* INFORMAT NUMBER OF DECIMALS         */
    int     npos;               /* POSITION OF VALUE IN OBSERVATION    */
    char    rest[52];           /* remaining fields are irrelevant     */
};

#ifdef WORDS_BIGENDIAN

#define char_to_short(from, to) memcpy(&to, from, 2)
#define char_to_int(from, to) memcpy(&to, from, 4)
#define char_to_uint(from, to) memcpy(&to, from, 4)

#else

#define char_to_short(from, to)	memcpy(&to, from, 2); reverse_short(to);
#define char_to_int(from, to) memcpy(&to, from, 4); reverse_int(to);
#define char_to_uint(from, to) memcpy(&to, from, 4); reverse_uint(to);

#endif /* WORDS_BIGENDIAN */

#endif /* SASEXPORT_H */
