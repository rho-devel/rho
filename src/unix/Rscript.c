/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2006-2014  The R Core Team
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

/* This is intended to be used in scripts like

#! /path/to/Rscript --vanilla
commandArgs(TRUE)
q(status=7)

This invokes R with a command line like
R --slave --no-restore --vanilla --file=foo [script_args]

*/

/* execv exists and can be used on Windows, but it returns immediately
   and so the exit status is lost.  HAVE_EXECV is defined under Windows.

   The main reason for using execv rather than system is to avoid
   argument quoting hell.
*/

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#ifdef _WIN32
#include <psignal.h>
/* on some systems needs to be included before <sys/types.h> */
#endif

#include <stdio.h>
#include <limits.h> /* for PATH_MAX */
#include <string.h>
#include <stdlib.h>
#include <unistd.h> /* for execv */

#include <Rversion.h>


/* Maximal length of an entire file name */
#if !defined(PATH_MAX)
# if defined(HAVE_SYS_PARAM_H)
#  include <sys/param.h>
# endif
# if !defined(PATH_MAX)
#  if defined(MAXPATHLEN)
#    define PATH_MAX MAXPATHLEN
#  elif defined(Win32)
#    define PATH_MAX 260
#  else
/* quite possibly unlimited, so we make this large, and test when used */
#    define PATH_MAX 5000
#  endif
# endif
#endif

#ifndef _WIN32
#ifndef R_ARCH /* R_ARCH should be always defined, but for safety ... */
#define R_ARCH ""
#endif

static char rhome[] = R_HOME;
static char rarch[] = R_ARCH;
#else
# ifndef BINDIR
#  define BINDIR "bin"
# endif
# define FOR_Rscript
# include "rterm.c"
#endif

#ifdef HAVE_EXECV
static int verbose = 0;
#endif

void usage(void)
{
    fprintf(stderr, "Usage: /path/to/Rscript [--options] [-e expr [-e expr2 ...] | file] [args]\n\n");
    fprintf(stderr, "--options accepted are\n");
    fprintf(stderr, "  --help              Print usage and exit\n");
    fprintf(stderr, "  --version           Print version and exit\n");
    fprintf(stderr, "  --verbose           Print information on progress\n");
    fprintf(stderr, "  --default-packages=list\n");
    fprintf(stderr, "                      Where 'list' is a comma-separated set\n");
    fprintf(stderr, "                        of package names, or 'NULL'\n");
    fprintf(stderr, "or options to R, in addition to --slave --no-restore, such as\n");
    fprintf(stderr, "  --save              Do save workspace at the end of the session\n");
    fprintf(stderr, "  --no-environ        Don't read the site and user environment files\n");
    fprintf(stderr, "  --no-site-file      Don't read the site-wide Rprofile\n");
    fprintf(stderr, "  --no-init-file      Don't read the user R profile\n");
    fprintf(stderr, "  --restore           Do restore previously saved objects at startup\n");
    fprintf(stderr, "  --vanilla           Combine --no-save, --no-restore, --no-site-file\n");
    fprintf(stderr, "                        --no-init-file and --no-environ\n");
    fprintf(stderr, "\n'file' may contain spaces but not shell metacharacters\n");
    fprintf(stderr, "Expressions (one or more '-e <expr>') may be used *instead* of 'file'\n");
    fprintf(stderr, "See also  ?Rscript  from within R\n");
}


int main(int argc, char *argv[])
{
#ifdef HAVE_EXECV
    char cmd[PATH_MAX+1], buf[PATH_MAX+8], buf2[1100], *p;
    int i, i0 = 0, ac = 0, res = 0, e_mode = 0, set_dp = 0;
    char **av;

    if(argc <= 1) {
	usage();
	exit(1);
    }
    av = (char **) malloc((size_t) (argc+4)*sizeof(char *));
    if(!av) {
	fprintf(stderr, "malloc failure\n");
	exit(1);
    }

    p = getenv("RHOME");
#ifdef _WIN32
    if(p && *p)
	snprintf(cmd, PATH_MAX+1, "%s\\%s\\Rterm.exe",  p, BINDIR);
    else {
	char rhome[MAX_PATH];
	GetModuleFileName(NULL, rhome, MAX_PATH);
	p = strrchr(rhome,'\\');
	if(!p) {fprintf(stderr, "installation problem\n"); exit(1);}
	*p = '\0';
	snprintf(cmd, PATH_MAX+1, "%s\\Rterm.exe",  rhome);
    }
#else
    if(!(p && *p)) p = rhome;
    /* avoid snprintf here */
    if(strlen(p) + 6 > PATH_MAX) {
	fprintf(stderr, "impossibly long path for RHOME\n");
	exit(1);
    }
    snprintf(cmd, PATH_MAX+1, "%s/bin/R", p);
#endif
    av[ac++] = cmd;
    av[ac++] = "--slave";
    av[ac++] = "--no-restore";

    if(argc == 2) {
	if(strcmp(argv[1], "--help") == 0) {
	    usage();
	    exit(0);
	}
	if(strcmp(argv[1], "--version") == 0) {
	    if(strlen(R_STATUS) == 0)
		fprintf(stderr, "R scripting front-end version %s.%s (%s-%s-%s)\n",
			R_MAJOR, R_MINOR, R_YEAR, R_MONTH, R_DAY);
	    else
		fprintf(stderr, "R scripting front-end version %s.%s %s (%s-%s-%s r%s)\n",
			R_MAJOR, R_MINOR, R_STATUS, R_YEAR, R_MONTH, R_DAY,
			R_GIT_REVISION);
	    exit(0);
	}
    }

    /* first copy over any -e or --foo args */
    for(i = 1; i < argc; i++) {
	if(strcmp(argv[i], "-e") == 0) {
	    e_mode = 1;
	    av[ac++] = argv[i];
	    if(!argv[++i]) {
		fprintf(stderr, "-e not followed by an expression\n");
		exit(1);
	    }
	    av[ac++] = argv[i];
	    i0 = i;
	    continue;
	}
	if(strncmp(argv[i], "--", 2) != 0) break;
	if(strcmp(argv[i], "--verbose") == 0) {
	    verbose = 1;
	    i0 = i;
	    continue;
	}
	if(strncmp(argv[i], "--default-packages=", 18) == 0) {
	    set_dp = 1;
	    if(strlen(argv[i]) > 1000) {
		fprintf(stderr, "unable to set R_DEFAULT_PACKAGES\n");
		exit(1);
	    }
	    snprintf(buf2, 1100, "R_DEFAULT_PACKAGES=%s", argv[i]+19);
	    if(verbose)
		fprintf(stderr, "setting '%s'\n", buf2);
#ifdef HAVE_PUTENV
	    if(putenv(buf2))
#endif
	    {
		fprintf(stderr, "unable to set R_DEFAULT_PACKAGES\n");
		exit(1);
	    }
	    i0 = i;
	    continue;
	}
	av[ac++] = argv[i];
	i0 = i;
    }

    if(!e_mode) {
	if(++i0 >= argc) {
	    fprintf(stderr, "file name is missing\n");
	    exit(1);
	}
	if(strlen(argv[i0]) > PATH_MAX) {
	    fprintf(stderr, "file name is too long\n");
	    exit(1);
	}
	snprintf(buf, PATH_MAX+8, "--file=%s", argv[i0]);
	av[ac++] = buf;
    }
    // copy any user arguments, preceded by "--args"
    i = i0+1;
    if (i < argc) {
	av[ac++] = "--args";
	for(; i < argc; i++)
	    av[ac++] = argv[i];
    }
    av[ac] = (char *) NULL;
#ifdef HAVE_PUTENV
    if(!set_dp && !getenv("R_DEFAULT_PACKAGES"))
	putenv("R_DEFAULT_PACKAGES=datasets,utils,grDevices,graphics,stats");

#ifndef _WIN32
    /* pass on r_arch from this binary to R as a default */
    if (!getenv("R_ARCH") && *rarch) {
	/* we have to prefix / so we may as well use putenv */
	if (strlen(rarch) + 9 > sizeof(buf2)) {
	    fprintf(stderr, "impossibly long string for R_ARCH\n");
	    exit(1);
	}
	strcpy(buf2, "R_ARCH=/");
	strcat(buf2, rarch);
	putenv(buf2);
    }
#endif
#endif
    if(verbose) {
	fprintf(stderr, "running\n  '%s", cmd);
	for(i = 1; i < ac; i++) fprintf(stderr, " %s", av[i]);
	fprintf(stderr, "'\n\n");
    }
#ifndef _WIN32
    res = execv(cmd, av); /* will not return if R is launched */
    perror("Rscript execution error");
#else
    AppMain(ac, av);
#endif
    return res;
#else /* No execv*/
    fprintf(stderr, "Rscript is not supported on this system");
    exit(1);
#endif
}
