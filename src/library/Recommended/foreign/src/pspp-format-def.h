/* PSPP - computes sample statistics.
   Copyright (C) 1997-9, 2000 Free Software Foundation, Inc.
   Written by Ben Pfaff <blp@gnu.org>.
   Modified for R foreign library by Saikat DebRoy <saikat@stat.wisc.edu>.

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, a copy is available at
   http://www.r-project.org/Licenses/
*/

/* Numeric and string formats. */
DEFFMT (FMT_F,            "F",         2,  1,  40,  1,   40, 0001, FMT_F, 5)
DEFFMT (FMT_N,		  "N",         2,  1,  40,  1,   40, 0011, FMT_F, 16)
DEFFMT (FMT_E,		  "E",         2,  1,  40,  6,   40, 0001, FMT_E, 17)
DEFFMT (FMT_COMMA,	  "COMMA",     2,  1,  40,  1,   40, 0001, FMT_COMMA, 3)
DEFFMT (FMT_DOT,	  "DOT",       2,  1,  40,  1,   40, 0001, FMT_DOT, 32)
DEFFMT (FMT_DOLLAR,	  "DOLLAR",    2,  1,  40,  2,   40, 0001, FMT_DOLLAR, 4)
DEFFMT (FMT_PCT,	  "PCT",       2,  1,  40,  2,   40, 0001, FMT_PCT, 31)
DEFFMT (FMT_Z,		  "Z",         2,  1,  40,  1,   40, 0011, FMT_F, 15)
DEFFMT (FMT_A,		  "A",         1,  1, 255,  1,  254, 0004, FMT_A, 1)
DEFFMT (FMT_AHEX,	  "AHEX",      1,  2, 254,  2,  510, 0006, FMT_A, 2)
DEFFMT (FMT_IB,		  "IB",        2,  1,   8,  1,    8, 0010, FMT_F, 6)
DEFFMT (FMT_P,		  "P",         2,  1,  16,  1,   16, 0011, FMT_F, 8)
DEFFMT (FMT_PIB,	  "PIB",       2,  1,   8,  1,    8, 0010, FMT_F, 9)
DEFFMT (FMT_PIBHEX,	  "PIBHEX",    2,  2,  16,  2,   16, 0002, FMT_F, 7)
DEFFMT (FMT_PK,		  "PK",        2,  1,  16,  1,   16, 0010, FMT_F, 10)
DEFFMT (FMT_RB,		  "RB",        1,  2,   8,  2,    8, 0002, FMT_F, 11)
DEFFMT (FMT_RBHEX,	  "RBHEX",     1,  4,  16,  4,   16, 0002, FMT_F, 12)
			  					    
/* Custom currency. */	  					    
DEFFMT (FMT_CCA,	  "CCA",       2, -1,  -1,  1,   40, 0020, FMT_CCA, 33)
DEFFMT (FMT_CCB,	  "CCB",       2, -1,  -1,  1,   40, 0020, FMT_CCB, 34)
DEFFMT (FMT_CCC,	  "CCC",       2, -1,  -1,  1,   40, 0020, FMT_CCC, 35)
DEFFMT (FMT_CCD,	  "CCD",       2, -1,  -1,  1,   40, 0020, FMT_CCD, 36)
DEFFMT (FMT_CCE,	  "CCE",       2, -1,  -1,  1,   40, 0020, FMT_CCE, 37)
			  					     
/* Date/time formats. */  					     
DEFFMT (FMT_DATE,	  "DATE",      1,  9,  40,  9,   40, 0001, FMT_DATE, 20)
DEFFMT (FMT_EDATE,	  "EDATE",     1,  8,  40,  8,   40, 0001, FMT_EDATE, 23)
DEFFMT (FMT_SDATE,	  "SDATE",     1,  8,  40,  8,   40, 0001, FMT_SDATE, 24)
DEFFMT (FMT_ADATE,	  "ADATE",     1,  8,  40,  8,   40, 0001, FMT_ADATE, 29)
DEFFMT (FMT_JDATE,	  "JDATE",     1,  5,  40,  5,   40, 0001, FMT_JDATE, 28)
DEFFMT (FMT_QYR,	  "QYR",       1,  4,  40,  6,   40, 0001, FMT_QYR, 30)
DEFFMT (FMT_MOYR,	  "MOYR",      1,  6,  40,  6,   40, 0001, FMT_MOYR, 22)
DEFFMT (FMT_WKYR,	  "WKYR",      1,  6,  40,  8,   40, 0001, FMT_WKYR, 21)
DEFFMT (FMT_DATETIME,	  "DATETIME",  2, 17,  40, 17,   40, 0001, FMT_DATETIME, 38)
DEFFMT (FMT_TIME,	  "TIME",      2,  5,  40,  5,   40, 0001, FMT_TIME, 39)
DEFFMT (FMT_DTIME,	  "DTIME",     2, 11,  40,  8,   40, 0001, FMT_DTIME, 25)
DEFFMT (FMT_WKDAY,	  "WKDAY",     1,  2,  40,  2,   40, 0001, FMT_WKDAY, 26)
DEFFMT (FMT_MONTH,	  "MONTH",     1,  3,  40,  3,   40, 0001, FMT_MONTH, 27)
			  					     
/* These aren't real formats.  They're used by DATA LIST. */	     
DEFFMT (FMT_T,            "T",         1,  1,99999, 1,99999, 0000, FMT_T, -1)
DEFFMT (FMT_X,		  "X",         1,  1,99999, 1,99999, 0000, FMT_X, -1)
DEFFMT (FMT_DESCEND,      "***",       1,  1,99999, 1,99999, 0000, -1, -1)
DEFFMT (FMT_NEWREC,	  "***",       1,  1,99999, 1,99999, 0000, -1, -1)
