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

#include <R_ext/libextern.h>
#undef LibExtern
#ifdef GA_DLL_BUILD
# define LibExtern LibExport
#else
# define LibExtern extern LibImport
#endif

LibExtern image cam_image;
LibExtern image color_image;
LibExtern image console_image;
LibExtern image console1_image;
LibExtern image copy_image;
LibExtern image copy1_image;
LibExtern image copypaste_image;
LibExtern image cut_image;
LibExtern image erase_image;
LibExtern image help_image;
LibExtern image open_image;
LibExtern image open1_image;
LibExtern image paste_image;
LibExtern image paste1_image;
LibExtern image print_image;
LibExtern image save_image;
LibExtern image stop_image;
#undef LibExtern
