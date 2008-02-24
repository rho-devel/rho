/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008 Andrew R. Runnalls, subject to such other
 *CXXR copyrights and copyright restrictions as may be stated below.
 *CXXR 
 *CXXR CXXR is not part of the R project, and bugs and other issues should
 *CXXR not be reported via r-bugs or other R project channels; instead refer
 *CXXR to the CXXR website.
 *CXXR */

/*************************************************
*           Unicode Property Table handler       *
*************************************************/

#ifndef _UCPINTERNAL_H
#define _UCPINTERNAL_H

/* Internal header file defining the layout of the bits in each pair of 32-bit
words that form a data item in the table. */

typedef struct cnode {
  pcre_uint32 f0;
  pcre_uint32 f1;
} cnode;

/* Things for the f0 field */

#define f0_scriptmask   0xff000000  /* Mask for script field */
#define f0_scriptshift          24  /* Shift for script value */
#define f0_rangeflag    0x00f00000  /* Flag for a range item */
#define f0_charmask     0x001fffff  /* Mask for code point value */

/* Things for the f1 field */

#define f1_typemask     0xfc000000  /* Mask for char type field */
#define f1_typeshift            26  /* Shift for the type field */
#define f1_rangemask    0x0000ffff  /* Mask for a range offset */
#define f1_casemask     0x0000ffff  /* Mask for a case offset */
#define f1_caseneg      0xffff8000  /* Bits for negation */

/* The data consists of a vector of structures of type cnode. The two unsigned
32-bit integers are used as follows:

(f0) (1) The most significant byte holds the script number. The numbers are
         defined by the enum in ucp.h.

     (2) The 0x00800000 bit is set if this entry defines a range of characters.
         It is not set if this entry defines a single character

     (3) The 0x00600000 bits are spare.

     (4) The 0x001fffff bits contain the code point. No Unicode code point will
         ever be greater than 0x0010ffff, so this should be OK for ever.

(f1) (1) The 0xfc000000 bits contain the character type number. The numbers are
         defined by an enum in ucp.h.

     (2) The 0x03ff0000 bits are spare.

     (3) The 0x0000ffff bits contain EITHER the unsigned offset to the top of
         range if this entry defines a range, OR the *signed* offset to the
         character's "other case" partner if this entry defines a single
         character. There is no partner if the value is zero.

-------------------------------------------------------------------------------
| script (8) |.|.|.| codepoint (21) || type (6) |.|.| spare (8) | offset (16) |
-------------------------------------------------------------------------------
              | | |                              | |
              | | |-> spare                      | |-> spare
              | |                                |
              | |-> spare                        |-> spare
              |
              |-> range flag

The upper/lower casing information is set only for characters that come in
pairs. The non-one-to-one mappings in the Unicode data are ignored.

When searching the data, proceed as follows:

(1) Set up for a binary chop search.

(2) If the top is not greater than the bottom, the character is not in the
    table. Its type must therefore be "Cn" ("Undefined").

(3) Find the middle vector element.

(4) Extract the code point and compare. If equal, we are done.

(5) If the test character is smaller, set the top to the current point, and
    goto (2).

(6) If the current entry defines a range, compute the last character by adding
    the offset, and see if the test character is within the range. If it is,
    we are done.

(7) Otherwise, set the bottom to one element past the current point and goto
    (2).
*/

#endif /* _UCPINTERNAL_H */

/* End of ucpinternal.h */
