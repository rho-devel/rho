/*
 *  Read SPSS files saved by SAVE and EXPORT commands
 *
 *  Copyright 2000-2000 Saikat DebRoy <saikat@stat.wisc.edu>
 *                      Thomas Lumley <tlumley@u.washington.edu>
 *  Copyright 2005-8 R Core Development Team
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be
 *  useful, but WITHOUT ANY WARRANTY; without even the implied
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 *  PURPOSE.  See the GNU General Public License for more
 *  details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

#include "foreign.h"
#include "file-handle.h"
#include "pfm.h"
#include "sfm.h"
#include "avl.h"
#include "var.h"

#if !PSPP && !__GCC__
#define inline
#endif

/* Divides nonnegative X by positive Y, rounding up. */
#define DIV_RND_UP(X, Y)			\
	(((X) + ((Y) - 1)) / (Y))

char *
xstrdup(const char *s)
{
    int len = strlen(s);
    char *c = Calloc(len + 1, char);
    strcpy(c, s);
    return c;
}

/* Returns a newly created empty dictionary. */
struct dictionary *
new_dictionary (int copy)
{
  struct dictionary *d = Calloc (1, struct dictionary);

  d->var = NULL;
  d->var_by_name = R_avl_create (cmp_variable, NULL);
  d->nvar = 0;

  d->N = 0;

  d->nval = 0;

  d->n_splits = 0;
  d->splits = NULL;

  d->label = NULL;

  d->n_documents = 0;
  d->documents = NULL;

  d->weight_index = -1;
  d->weight_var[0] = 0;

  d->filter_var[0] = 0;

  return d;
}

/* Find and return the variable in dictionary D having name NAME, or
   NULL if no such variable exists in D. */
struct variable *
find_dict_variable (const struct dictionary *d, const char *name)
{
  return R_avl_find (d->var_by_name, (struct variable *) name);
}

/* Initialize fields in variable V inside dictionary D with name NAME,
   type TYPE, and width WIDTH.  Initializes some other fields too. */
static inline void
common_init_stuff (struct dictionary *dict, struct variable *v,
		   const char *name, int type, int width)
{
  if (v->name != name)
    /* Avoid problems with overlap. */
    strcpy (v->name, name);

  R_avl_insert (dict->var_by_name, v);

  v->type = type;
  v->left = name[0] == '#';
  v->width = type == NUMERIC ? 0 : width;
  v->miss_type = MISSING_NONE;
  if (v->type == NUMERIC)
    {
      v->print.type = FMT_F;
      v->print.w = 8;
      v->print.d = 2;
    }
  else
    {
      v->print.type = FMT_A;
      v->print.w = v->width;
      v->print.d = 0;
    }
  v->write = v->print;
}

/* Initialize (for the first time) a variable V in dictionary DICT
   with name NAME, type TYPE, and width WIDTH.  */
void
init_variable (struct dictionary *dict, struct variable *v, const char *name,
	       int type, int width)
{
  common_init_stuff (dict, v, name, type, width);
  v->nv = type == NUMERIC ? 1 : DIV_RND_UP (width, 8);
  v->fv = dict->nval;
  dict->nval += v->nv;
  v->label = NULL;
  v->val_lab = NULL;
  v->get.fv = -1;
}

/* Creates a variable named NAME in dictionary DICT having type TYPE
   (ALPHA or NUMERIC) and, if type==ALPHA, width WIDTH.  Returns a
   pointer to the newly created variable if successful.  On failure
   (which indicates that a variable having the specified name already
   exists), returns NULL.  */
struct variable *
create_variable (struct dictionary *dict, const char *name,
		 int type, int width)
{
  if (find_dict_variable (dict, name))
    return NULL;

  {
    struct variable *new_var;

    dict->var = Realloc (dict->var, dict->nvar + 1, struct variable *);
    new_var = dict->var[dict->nvar] = Calloc (1, struct variable);

    new_var->index = dict->nvar;
    dict->nvar++;

    init_variable (dict, new_var, name, type, width);

    return new_var;
  }
}

/* Compares two value labels and returns a strcmp()-type result. */
int
val_lab_cmp (const void *a, const void *b, void *param)
{
  int width = *((int *)param);
  if (width)
    return strncmp ((char *)((struct value_label *) a)->v.s,
		    (char *)((struct value_label *) b)->v.s,
		    width);
  else
    {
      int temp = (((struct value_label *) a)->v.f
		  - ((struct value_label *) b)->v.f);
      if (temp > 0)
	return 1;
      else if (temp < 0)
	return -1;
      else
	return 0;
    }
}

/* returns an array of pointers to the data elements of an avl_tree */
/* Ugly, but the alternatives seem to be worse */

void *avlFlatten(const avl_tree *tree){
  int n;
  void **ans;


  /* T1. */
  const avl_node *an[AVL_MAX_HEIGHT];	/* Stack A: nodes. */
  const avl_node **ap = an;		/* Stack A: stack pointer. */
  const avl_node *p = tree->root.link[0];

  n=R_avl_count(tree);
  ans=Calloc(n, void * );

  for (;;)  /* from avl.c:avl_walk */
    {
      /* T2. */
      while (p != NULL)
	{
	  /* T3. */
	  *ap++ = p;
	  p = p->link[0];
	}

      /* T4. */
      if (ap == an)
	  return ans;
      p = *--ap;

      /* T5. */
      n--;
      ans[n]=p->data;
      p = p->link[1];
    }

}


static SEXP getSPSSvaluelabels(struct dictionary *dict)
{
    SEXP ans, somelabels, somevalues;
    int nlabels, nvars, i, j;
    struct value_label **flattened_labels;
    struct avl_tree *labelset;
    unsigned char tmp[MAX_SHORT_STRING+1];

    nvars = dict->nvar;
    if (nvars == 0) return R_NilValue;
    PROTECT(ans = allocVector(VECSXP, nvars));

    for(i = 0; i < nvars; i++) {
	labelset = (dict->var)[i]->val_lab;
	if (!labelset) continue;
	nlabels = R_avl_count(labelset);
	flattened_labels = avlFlatten(labelset);
	PROTECT(somelabels = allocVector(STRSXP, nlabels));
	if ((dict->var)[i]->type == NUMERIC) {
	    double *rx;
	    PROTECT(somevalues = allocVector(REALSXP, nlabels));
	    rx = REAL(somevalues);
	    for(j = 0; j < nlabels; j++) {
		SET_STRING_ELT(somelabels, j, mkChar(flattened_labels[j]->s));
		rx[j] = flattened_labels[j]->v.f;
	    }
	} else {
	    PROTECT(somevalues = allocVector(STRSXP, nlabels));
	    for(j = 0; j < nlabels; j++) {
		SET_STRING_ELT(somelabels, j, mkChar(flattened_labels[j]->s));
		memcpy(tmp,flattened_labels[j]->v.s, MAX_SHORT_STRING);
		tmp[MAX_SHORT_STRING] = '\0';
		SET_STRING_ELT(somevalues, j, mkChar((char *)tmp));
	    }
	}
	Free(flattened_labels);
	namesgets(somevalues, somelabels);
	SET_VECTOR_ELT(ans, i, somevalues);
	UNPROTECT(2); /*somevalues, somelabels*/
    }
    UNPROTECT(1);
    return ans;
}

static SEXP getSPSSmissing(struct dictionary *dict, int *have_miss)
{
    SEXP ans, elt, nm, value;
    int nvars, i;

    nvars = dict->nvar;
    if (nvars == 0) return R_NilValue;
    PROTECT(ans = allocVector(VECSXP, nvars));

    for(i = 0; i < nvars; i++) {
	struct variable *v = dict->var[i];
	int j, n = 0;
	const char *type="unknown";
	switch (v->miss_type) {
	case MISSING_NONE:
	    type = "none";
	    break;
	case MISSING_1:
	    n = 1;
	    type = "one";
	    break;
	case MISSING_2:
	    n = 2;
	    type = "two";
	    break;
	case MISSING_3:
	    n = 3;
	    type = "three";
	    break;
	case MISSING_RANGE:
	    n = 2;
	    type = "range";
	    break;
	case MISSING_LOW:
	    n = 1;
	    type = "low";
	    break;
	case MISSING_HIGH:
	    n = 1;
	    type = "high";
	    break;
	case MISSING_RANGE_1:
	    n = 3;
	    type = "range+1";
	    break;
	case MISSING_LOW_1:
	    n = 2;
	    type = "low+1";
	    break;
	case MISSING_HIGH_1:
	    n = 2;
	    type = "high+1";
	    break;
	default:
	    type = "unknown";
	}
	if (strcmp(type, "none")) (*have_miss)++;
	if (n > 0) {
	    elt = allocVector(VECSXP, 2);
	    SET_VECTOR_ELT(ans, i, elt);
	    PROTECT(nm = allocVector(STRSXP, 2));
	    SET_STRING_ELT(nm, 0, mkChar("type"));
	    SET_STRING_ELT(nm, 1, mkChar("value"));
	    setAttrib(elt, R_NamesSymbol, nm);
	    SET_VECTOR_ELT(elt, 0, mkString(type));
	    if (v->type == NUMERIC) {
		double *rx;
		PROTECT(value = allocVector(REALSXP, n));
		rx = REAL(value);
		for(j = 0; j < n; j++) rx[j] = v->missing[j].f;
	    } else {
		PROTECT(value = allocVector(STRSXP, n));
		for(j = 0; j < n; j++)
		    SET_STRING_ELT(value, j, 
				   mkChar((const char *)v->missing[j].s));
	    }
	    SET_VECTOR_ELT(elt, 1, value);	
	    UNPROTECT(2);
	} else {
	    elt = allocVector(VECSXP, 1);
	    SET_VECTOR_ELT(ans, i, elt);
	    setAttrib(elt, R_NamesSymbol, mkString("type"));
	    SET_VECTOR_ELT(elt, 0, mkString(type));
	}
    }
    UNPROTECT(1); 
    return ans;
}

static SEXP
read_SPSS_PORT(const char *filename)
{
    struct file_handle *fh = fh_get_handle_by_filename(filename);
    struct pfm_read_info inf;
    struct dictionary *dict = pfm_read_dictionary(fh, &inf);
    SEXP ans = PROTECT(allocVector(VECSXP, dict->nvar));
    SEXP ans_names = PROTECT(allocVector(STRSXP, dict->nvar));
    union value *case_vals;
    int i;
    int ncases = 0;
    int N = 10;
    int nval = 0;
    int nvar_label;
    SEXP val_labels;
    SEXP variable_labels;
    SEXP miss_labels; int have_miss = 0;

    /* Set the fv and lv elements of all variables in the
       dictionary. */
    for (i = 0; i < dict->nvar; i++) {
	struct variable *v = dict->var[i];

	v->fv = nval;
	nval += v->nv;
    }
    dict->nval = nval;
    if (!nval)
	error(_("nval is 0"));
    case_vals = (union value *) R_alloc(dict->nval, sizeof(union value));

    for (i = 0; i < dict->nvar; i++) {
	struct variable *v = dict->var[i];

	if (v->get.fv == -1)
	    continue;

	SET_STRING_ELT(ans_names, i, mkChar(dict->var[i]->name));
	if (v->type == NUMERIC) {
	    SET_VECTOR_ELT(ans, i, allocVector(REALSXP, N));
	} else {
	    SET_VECTOR_ELT(ans, i, allocVector(STRSXP, N));
	    case_vals[v->fv].c =
		(unsigned char *) R_alloc(v->width + 1, 1);
	    ((char *) &case_vals[v->fv].c[0])[v->width] = '\0';
	}
    }

    while(pfm_read_case(fh, case_vals, dict)) {
	if (ncases == N) {
	    N *= 2;
	    for (i = 0; i < dict->nvar; i++) {
		SEXP elt = VECTOR_ELT(ans, i);
		elt = lengthgets(elt, N);
		SET_VECTOR_ELT(ans, i, elt);
	    }
	}
	for (i = 0; i < dict->nvar; i++) {
	    struct variable *v = dict->var[i];

	    if (v->get.fv == -1)
		continue;

	    if (v->type == NUMERIC) {
		REAL(VECTOR_ELT(ans, i))[ncases] = case_vals[v->fv].f;
	    } else {
		SET_STRING_ELT(VECTOR_ELT(ans, i), ncases,
			       mkChar((char *)case_vals[v->fv].c));
	    }
	}
	++ncases;
    }
    if (N != ncases) {
	for (i = 0; i < dict->nvar; i++) {
	    SEXP elt = VECTOR_ELT(ans, i);
	    elt = lengthgets(elt, ncases);
	    SET_VECTOR_ELT(ans, i, elt);
	}
    }

    fh_close_handle(fh);

    /* get all the value labels */
    PROTECT(val_labels = getSPSSvaluelabels(dict));
    namesgets(val_labels, ans_names);
    setAttrib(ans, install("label.table"), val_labels);
    UNPROTECT(1);

    /* get SPSS variable labels */
    PROTECT(variable_labels = allocVector(STRSXP, dict->nvar));
    nvar_label = 0;
    for (i = 0; i < dict->nvar; i++) {
	char *lab = dict->var[i]->label;
	if (lab != NULL) {
	    nvar_label++;
	    SET_STRING_ELT(variable_labels, i, mkChar(lab));
	}
    }
    if (nvar_label > 0) {
	namesgets(variable_labels, ans_names);
	setAttrib(ans, install("variable.labels"), variable_labels);
    }
    UNPROTECT(1);

    /* report missingness */
    PROTECT(miss_labels = getSPSSmissing(dict, &have_miss));
    if(have_miss) {
	namesgets(miss_labels, duplicate(ans_names));
	setAttrib(ans, install("missings"), miss_labels);
    }
    UNPROTECT(1);
   
    free_dictionary(dict);
    setAttrib(ans, R_NamesSymbol, ans_names);
    UNPROTECT(2);
    return ans;
}

static SEXP
read_SPSS_SAVE(const char *filename)
{
    struct file_handle *fh = fh_get_handle_by_filename(filename);
    struct sfm_read_info inf;
    struct dictionary *dict;
    SEXP ans;
    SEXP ans_names;
    union value *case_vals;
    int i;
    int nvar_label;
    int nval = 0;
    SEXP val_labels;
    SEXP variable_labels;
    SEXP miss_labels; int have_miss = 0;

    /* package multcomp has an example in which this does not get
       initialized */
    inf.encoding = 0;
    dict = sfm_read_dictionary(fh, &inf);
    ans = PROTECT(allocVector(VECSXP, dict->nvar));
    ans_names = PROTECT(allocVector(STRSXP, dict->nvar));
    /* Set the fv and lv elements of all variables in the
       dictionary. */
    for (i = 0; i < dict->nvar; i++) {
	struct variable *v = dict->var[i];

	v->fv = nval;
	nval += v->nv;
    }
    dict->nval = nval;
    if (!nval)
	error(_("nval is 0"));
    case_vals = (union value *) R_alloc(dict->nval, sizeof(union value));

    for (i = 0; i < dict->nvar; i++) {
	struct variable *v = dict->var[i];

	if (v->get.fv == -1)
	    continue;

	SET_STRING_ELT(ans_names, i, mkChar(dict->var[i]->name));
	if (v->type == NUMERIC) {
	    SET_VECTOR_ELT(ans, i, allocVector(REALSXP, inf.ncases));
	} else {
	    SET_VECTOR_ELT(ans, i, allocVector(STRSXP, inf.ncases));
	    case_vals[v->fv].c =
		(unsigned char *) R_alloc(v->width + 1, 1);
	    ((char *) &case_vals[v->fv].c[0])[v->width] = '\0';
	}
    }
    for (i = 0; i < inf.ncases; i++) {
	int j;
	sfm_read_case(fh, case_vals, dict);
	for (j = 0; j < dict->nvar; j++) {
	    struct variable *v = dict->var[j];

	    if (v->get.fv == -1)
		continue;

	    if (v->type == NUMERIC) {
		REAL(VECTOR_ELT(ans, j))[i] = case_vals[v->fv].f;
	    } else {
		SET_STRING_ELT(VECTOR_ELT(ans, j), i,
			       mkChar((char *)case_vals[v->fv].c));
	    }
	}
    }
    sfm_maybe_close(fh);

    /* get all the value labels */
    PROTECT(val_labels = getSPSSvaluelabels(dict));
    namesgets(val_labels, duplicate(ans_names));
    setAttrib(ans, install("label.table"), val_labels);
    UNPROTECT(1);

    /* get SPSS variable labels */
    PROTECT(variable_labels = allocVector(STRSXP, dict->nvar));
    nvar_label = 0;
    for (i = 0; i < dict->nvar; i++) {
	char *lab = dict->var[i]->label;
	if (lab != NULL) {
	    nvar_label++;
	    SET_STRING_ELT(variable_labels, i, mkChar(lab));
	}
    }
    if (nvar_label > 0) {
	namesgets(variable_labels, ans_names);
	setAttrib(ans,install("variable.labels"), variable_labels);
    }
    UNPROTECT(1);

    /* report missingness */
    PROTECT(miss_labels = getSPSSmissing(dict, &have_miss));
    if(have_miss) {
	namesgets(miss_labels, duplicate(ans_names));
	setAttrib(ans, install("missings"), miss_labels);
    }
    UNPROTECT(1);

    free_dictionary(dict);
    setAttrib(ans, R_NamesSymbol, ans_names);
    setAttrib(ans, install("codepage"), ScalarInteger(inf.encoding));
    UNPROTECT(2);
    return ans;
}

extern int R_fgetc(FILE *stream);

static size_t
fread_pfm(void *ptr, size_t size, size_t nobj, FILE *stream)
{
    size_t nbytes = size * nobj;
    size_t i;
    char *c_ptr = ptr;

    for (i = 0; i < nbytes; i++) {
	int c = fgetc(stream);
	if (c == '\r') {
	    c = fgetc(stream);
	    if ( c != '\n') {
		ungetc(c, stream);
		c = '\r';
	    }
	}
	if (c == '\n')
	    fgetc(stream);
	if (c == EOF)
	    break;
	*c_ptr++ = c;
    }
    if (i % size != 0) {
	i -= i % size;
    }
    return i/size;
}

static int
is_PORT(FILE *fp)
{
    /* For now at least, just ignore the vanity splash strings. */
    int trans_temp[256];

    if (196 != fread_pfm(trans_temp, sizeof(char), 196, fp))
	return 0;
    {
	unsigned char src[256];
	int i;

	if (256 != fread_pfm(src, sizeof(char), 256, fp))
	    return 0;
	for (i = 0; i < 256; i++)
	    trans_temp[i] = -1;

	/* 0 is used to mark untranslatable characters, so we have to mark
	   it specially. */
	trans_temp[src[64]] = 64;
	for (i = 0; i < 256; i++) {
	    if (trans_temp[src[i]] == -1)
		trans_temp[src[i]] = i;
	}

	for (i = 0; i < 256; i++) {
	    if (trans_temp[i] == -1)
		trans_temp[i] = 0;
	}

    }

    {
	unsigned char sig[9] = {92, 89, 92, 92, 89, 88, 91, 93, '\0'};
	unsigned char buf[9];
	int i;

	buf[8] = '\0';
	if (8 != fread_pfm(buf, sizeof(char), 8, fp))
	    return 0;
	for (i = 0; i < 8; i++) {
	    if (sig[i] != trans_temp[buf[i]])
		return 0;
	}
    }
    return 1;
}

#include <string.h>
#include <errno.h>

SEXP
do_read_SPSS(SEXP file)
{
    const char *filename = CHAR(PROTECT(asChar(file)));
    FILE *fp = fopen(R_ExpandFileName(filename), "rb");
    char buf[5];
    SEXP ans;

    if(!fp)
	error(_("unable to open file: '%s'"), strerror(errno));
    if(fread_pfm(buf, sizeof(char), 4, fp) != 4) {
	fclose(fp);
	error(_("problem in reading file '%s'"), filename);
    }
    buf[4] = '\0';

    if (0 == strncmp("$FL2", buf, 4)) {
	fclose(fp);
	ans = read_SPSS_SAVE(filename);
    } else {
	if (!is_PORT(fp)) {
	    fclose(fp);
	    error(_("file '%s' is not in any supported SPSS format"),
		  filename);
	}
	fclose(fp);
	ans = read_SPSS_PORT(filename);
    }
    UNPROTECT(1);
    return ans;
}

void
spss_init(void)
{
    fh_init_files();
}
