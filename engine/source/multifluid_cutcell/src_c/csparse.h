/* CSPARSE: a Concise Sparse matrix package.
Copyright (c) 2006, Timothy A. Davis.
http://www.cise.ufl.edu/research/sparse/CSparse
https://people.math.sc.edu/Burkardt/c_src/csparse/csparse.html

CSPARSE is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option) any later version.

CSPARSE is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License along with this Module; if not, write to the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA */

#ifndef _CS_H
#define _CS_H

#ifdef MATLAB_MEX_FILE
#include "mex.h"
#endif
#define CS_VER 1		    /* CSparse Version 1.2.0 */
#define CS_SUBVER 2
#define CS_SUBSUB 0
#define CS_DATE "Mar 6, 2006"	    /* CSparse release date */
#define CS_COPYRIGHT "Copyright (c) Timothy A. Davis, 2006"

#include "my_real_c.inc"

/* --- primary CSparse routines and data structures ------------------------- */
typedef struct cs_sparse    /* matrix in compressed-column or triplet form */
{
    int nzmax ;	    /* maximum number of entries */
    int m ;	    /* number of rows */
    int n ;	    /* number of columns */
    int *p ;	    /* column pointers (size n+1) or col indices (size nzmax) */
    int *i ;	    /* row indices, size nzmax */
    my_real_c *x ;	    /* numerical values, size nzmax */
    int nz ;	    /* # of entries in triplet matrix, -1 for compressed-col */
} cs ;

cs *cs_add (const cs *A, const cs *B, my_real_c alpha, my_real_c beta) ;
int cs_cholsol (const cs *A, my_real_c *b, int order) ;
int cs_dupl (cs *A) ;
int cs_entry (cs *T, int i, int j, my_real_c x) ;
int cs_lusol (const cs *A, my_real_c *b, int order, my_real_c tol) ;
int cs_gaxpy (const cs *A, const my_real_c *x, my_real_c *y) ;
cs *cs_multiply (const cs *A, const cs *B) ;
int cs_qrsol (const cs *A, my_real_c *b, int order) ;
cs *cs_transpose (const cs *A, int values) ;
cs *cs_triplet (const cs *T) ;
my_real_c cs_norm (const cs *A) ;
int cs_print (const cs *A, int brief) ;
cs *cs_load (FILE *f) ;
/* utilities */
void *cs_calloc (int n, size_t size) ;
void *cs_free (void *p) ;
void *cs_realloc (void *p, int n, size_t size, int *ok) ;
cs *cs_spalloc (int m, int n, int nzmax, int values, int triplet) ;
cs *cs_spfree (cs *A) ;
int cs_sprealloc (cs *A, int nzmax) ;
void *cs_malloc (int n, size_t size) ;

/* --- secondary CSparse routines and data structures ----------------------- */
typedef struct cs_symbolic  /* symbolic Cholesky, LU, or QR analysis */
{
    int *Pinv ;	    /* inverse row perm. for QR, fill red. perm for Chol */
    int *Q ;	    /* fill-reducing column permutation for LU and QR */
    int *parent ;   /* elimination tree for Cholesky and QR */
    int *cp ;	    /* column pointers for Cholesky, row counts for QR */
    int m2 ;	    /* # of rows for QR, after adding fictitious rows */
    int lnz ;	    /* # entries in L for LU or Cholesky; in V for QR */
    int unz ;	    /* # entries in U for LU; in R for QR */
} css ;

typedef struct cs_numeric   /* numeric Cholesky, LU, or QR factorization */
{
    cs *L ;	    /* L for LU and Cholesky, V for QR */
    cs *U ;	    /* U for LU, R for QR, not used for Cholesky */
    int *Pinv ;	    /* partial pivoting for LU */
    my_real_c *B ;	    /* beta [0..n-1] for QR */
} csn ;

typedef struct cs_dmperm_results    /* cs_dmperm or cs_scc output */
{
    int *P ;	    /* size m, row permutation */
    int *Q ;	    /* size n, column permutation */
    int *R ;	    /* size nb+1, block k is rows R[k] to R[k+1]-1 in A(P,Q) */
    int *S ;	    /* size nb+1, block k is cols S[k] to S[k+1]-1 in A(P,Q) */
    int nb ;	    /* # of blocks in fine dmperm decomposition */
    int rr [5] ;    /* coarse row decomposition */
    int cc [5] ;    /* coarse column decomposition */
} csd ;

int *cs_amd (const cs *A, int order) ;
csn *cs_chol (const cs *A, const css *S) ;
csd *cs_dmperm (const cs *A) ;
int cs_droptol (cs *A, my_real_c tol) ;
int cs_dropzeros (cs *A) ;
int cs_happly (const cs *V, int i, my_real_c beta, my_real_c *x) ;
int cs_ipvec (int n, const int *P, const my_real_c *b, my_real_c *x) ;
int cs_lsolve (const cs *L, my_real_c *x) ;
int cs_ltsolve (const cs *L, my_real_c *x) ;
csn *cs_lu (const cs *A, const css *S, my_real_c tol) ;
cs *cs_permute (const cs *A, const int *P, const int *Q, int values) ;
int *cs_pinv (const int *P, int n) ;
int cs_pvec (int n, const int *P, const my_real_c *b, my_real_c *x) ;
csn *cs_qr (const cs *A, const css *S) ;
css *cs_schol (const cs *A, int order) ;
css *cs_sqr (const cs *A, int order, int qr) ;
cs *cs_symperm (const cs *A, const int *Pinv, int values) ;
int cs_usolve (const cs *U, my_real_c *x) ;
int cs_utsolve (const cs *U, my_real_c *x) ;
int cs_updown (cs *L, int sigma, const cs *C, const int *parent) ;
/* utilities */
css *cs_sfree (css *S) ;
csn *cs_nfree (csn *N) ;
csd *cs_dfree (csd *D) ;

/* --- tertiary CSparse routines -------------------------------------------- */
int *cs_counts (const cs *A, const int *parent, const int *post, int ata) ;
int cs_cumsum (int *p, int *c, int n) ;
int cs_dfs (int j, cs *L, int top, int *xi, int *pstack, const int *Pinv) ;
int *cs_etree (const cs *A, int ata) ;
int cs_fkeep (cs *A, int (*fkeep) (int, int, my_real_c, void *), void *other) ;
my_real_c cs_house (my_real_c *x, my_real_c *beta, int n) ;
int *cs_maxtrans (const cs *A) ;
int *cs_post (int n, const int *parent) ;
int cs_reach (cs *L, const cs *B, int k, int *xi, const int *Pinv) ;
csd *cs_scc (cs *A) ;
int cs_scatter (const cs *A, int j, my_real_c beta, int *w, my_real_c *x, int mark,
    cs *C, int nz) ;
int cs_splsolve (cs *L, const cs *B, int k, int *xi, my_real_c *x,
    const int *Pinv) ;
int cs_tdfs (int j, int k, int *head, const int *next, int *post,
    int *stack) ;
/* utilities */
csd *cs_dalloc (int m, int n) ;
cs *cs_done (cs *C, void *w, void *x, int ok) ;
int *cs_idone (int *p, cs *C, void *w, int ok) ;
csn *cs_ndone (csn *N, cs *C, void *w, void *x, int ok) ;
csd *cs_ddone (csd *D, cs *C, void *w, int ok) ;

#define CS_MAX(a,b) (((a) > (b)) ? (a) : (b))
#define CS_MIN(a,b) (((a) < (b)) ? (a) : (b))
#define CS_FLIP(i) (-(i)-2)
#define CS_UNFLIP(i) (((i) < 0) ? CS_FLIP(i) : (i))
#define CS_MARKED(Ap,j) (Ap [j] < 0)
#define CS_MARK(Ap,j) { Ap [j] = CS_FLIP (Ap [j]) ; }
#define CS_OVERFLOW(n,size) (n > INT_MAX / (int) size)
#endif
