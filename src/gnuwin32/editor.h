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


void menueditornew(control m);
void menueditoropen(control m);
int editorchecksave(editor c);
void editorsetfont(font f);
int Rgui_Edit(char *filename, char *title, int modal);

#define EDITORMAXTITLE 128
#define MAXNEDITORS 50

struct structEditorData {
    Rboolean file; /* is the editor associated with an existing file */
    char *filename; /* corresponding file */
    char *title;    /* window title */
    Rboolean stealconsole;  /* set when using fix() or edit(), so that no events are sent to console until this editor is closed */
    menuitem mcut, mcopy, mdelete, mfind, mreplace,
	mpopcut, mpopcopy, mpopdelete;
    HelpMenuItems hmenu;
    PkgMenuItems pmenu;
};
typedef struct structEditorData *EditorData;
