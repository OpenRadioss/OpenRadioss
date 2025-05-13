//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2025 Altair Engineering Inc.
//Copyright>
//Copyright>    This program is free software: you can redistribute it and/or modify
//Copyright>    it under the terms of the GNU Affero General Public License as published by
//Copyright>    the Free Software Foundation, either version 3 of the License, or
//Copyright>    (at your option) any later version.
//Copyright>
//Copyright>    This program is distributed in the hope that it will be useful,
//Copyright>    but WITHOUT ANY WARRANTY; without even the implied warranty of
//Copyright>    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//Copyright>    GNU Affero General Public License for more details.
//Copyright>
//Copyright>    You should have received a copy of the GNU Affero General Public License
//Copyright>    along with this program.  If not, see <https://www.gnu.org/licenses/>.
//Copyright>
//Copyright>
//Copyright>    Commercial Alternative: Altair Radioss Software
//Copyright>
//Copyright>    As an alternative to this open-source version, Altair also offers Altair Radioss
//Copyright>    software under a commercial license.  Contact Altair to discuss further if the
//Copyright>    commercial version may interest you: https://www.altair.com/radioss/.

#ifdef _WIN64
int readr(HANDLE pipe, char *buf, int nbytes);
int writer(HANDLE pipe, char*buf, int nbytes);
#else
int readr(int pipe, char *buf, int nbytes);
int writer(int pipe   , char *buf, int nbytes);
#endif

#ifdef DNC
extern void syserr_fatal(char *msg);
extern void fatal(char *msg);
extern void catch_sig_c(int *pid);
#endif

void openfifo_c(int *iroot,int *len,int *fdw,int *fdr,int *sd,int *ispmd,int *nthr,int *ppid);
void _FCALL OPENFIFO_C(int *iroot,int *len,int *fdw,int *fdr,int *sd,int *ispmd,int *nthr,int *ppid);
void openfifo_c_(int *iroot,int *len,int *fdw,int *fdr,int *sd,int *ispmd,int *nthr,int *ppid);
void openfifo_c__(int *iroot,int *len,int *fdw,int *fdr,int *sd,int *ispmd,int *nthr,int *ppid);
// -------------------------------------------------------------
void opensem_c(int *iroot,int *len,int *ispmd,int *nthr,int *ppid);
void _FCALL OPENSEM_C(int *iroot,int *len,int *ispmd,int *nthr,int *ppid);
void opensem_c_(int *iroot,int *len,int *ispmd,int *nthr,int *ppid);
void opensem_c__(int *iroot,int *len,int *ispmd,int *nthr,int *ppid);
// -------------------------------------------------------------
void openshm_c();
void syserr();
// -------------------------------------------------------------
void _FCALL OPENSHM_C();
void openshm_c_();
void openshm_c__();
// -------------------------------------------------------------
void r2r_unlock_threads_c(int *nthr);
void _FCALL R2R_UNLOCK_THREADS_C(int *nthr);
void r2r_unlock_threads_c_(int *nthr);
void r2r_unlock_threads__(int *nthr);
// -------------------------------------------------------------
void r2r_block_c();
void _FCALL R2R_BLOCK_C();
void r2r_block_c_();
void r2r_block__();
// -------------------------------------------------------------
void r2r_sem_c();
void _FCALL R2R_SEM_C();
void r2r_sem_c_();
void r2r_sem__();
// -------------------------------------------------------------
void init_link_c(int *igd, int *nng, int *itab, int *nodbuf, my_real_c *x, int *addcnel, int *cnel, 
                 int *ixc, int *ofc, int *info, int *typ, int *cdt, int *cdr, int *print, int *rddl, 
                 int *nlink, my_real_c *dx);
void _FCALL  INIT_LINK_C (int *igd, int *nng, int *itab, int *nodbuf, my_real_c *x, int *addcnel, int *cnel,
                          int *ixc, int *ofc, int *info, int *typ, int *cdt, int *cdr, int *print, int *rddl,
                          int *nlink, my_real_c *dx);
void _FCALL init_link_c_(int *igd, int *nng, int *itab, int *nodbuf, my_real_c *x, int *addcnel, int *cnel, 
                         int *ixc, int *ofc, int *info, int *typ, int *cdt, int *cdr, int *print, int *rddl, 
                         int *nlink, my_real_c *dx);
void _FCALL init_link_c__(int *igd, int *nng, int *itab, int *nodbuf, my_real_c *x, int *addcnel, int *cnel, 
                          int *ixc, int *ofc, int *info, int *typ, int *cdt, int *cdr, int *print, int *rddl,
                          int *nlink, my_real_c *dx);
// -------------------------------------------------------------
void init_link_nl_c(int *igd, int *nng, int *itab, int *nodbuf, my_real_c *x, int *print, 
                    my_real_c *dx, int *ndof_nl, int *nb_tot_dof, int *nlnk);
void _FCALL  INIT_LINK_NL_C (int *igd, int *nng, int *itab, int *nodbuf, my_real_c *x, int *print,
                             my_real_c *dx, int *ndof_nl, int *nb_tot_dof, int *nlnk);
void _FCALL init_link_nl_c_ (int *igd, int *nng, int *itab, int *nodbuf, my_real_c *x, int *print,
                             my_real_c *dx, int *ndof_nl, int *nb_tot_dof, int *nlnk);
void _FCALL init_link_nl_c__ (int *igd, int *nng, int *itab, int *nodbuf, my_real_c *x, int *print, 
                              my_real_c *dx, int *ndof_nl, int *nb_tot_dof, int *nlnk);
void init_buf_spmd_c(int *igd, int *nng, int *itab, int *nodbuf, my_real_c *x, int *addcnel, int *cnel, int *ixc,
                     int *ofc, int *tlel, int *lel, int *lelnb, int *tleln, int *leln, int *nbelem, int *tcnel,
                     int *cnelem2, int *wgt, int *tcneldb, int *cnelemdb, int *info, int *typ, int *nglob);
void _FCALL  INIT_BUF_SPMD_C(int *igd, int *nng, int *itab, int *nodbuf, my_real_c *x, int *addcnel, int *cnel, int *ixc, 
                             int *ofc, int *tlel, int *lel, int *lelnb, int *tleln, int *leln, int *nbelem, int *tcnel, 
                             int *cnelem2, int *wgt, int *tcneldb, int *cnelemdb, int *info, int *typ, int *nglob);
void _FCALL init_buf_spmd_c_(int *igd, int *nng, int *itab, int *nodbuf, my_real_c *x, int *addcnel, int *cnel, int *ixc, 
                             int *ofc, int *tlel, int *lel, int *lelnb, int *tleln, int *leln, int *nbelem, int *tcnel, 
                             int *cnelem2, int *wgt, int *tcneldb, int *cnelemdb, int *info, int *typ, int *nglob);
void _FCALL init_buf_spmd_c__(int *igd, int *nng, int *itab, int *nodbuf, my_real_c *x, int *addcnel, int *cnel, int *ixc, 
                              int *ofc, int *tlel, int *lel, int *lelnb, int *tleln, int *leln, int *nbelem, int *tcnel,
                              int *cnelem2, int *wgt, int *tcneldb, int *cnelemdb, int *info, int *typ, int *nglob);
//-------------------------------------------------------------
void init_link_spmd_c(int *igd, int *nng, int *dbnod, int *nbproc, int *ibuf, int *dbibuf, 
                      int *dbnbuf, int *ddbuf, my_real_c *rbuf, int *dim, int *ibufnb, 
                      int *ibufcnel, int *nbel, int *dimel, int *ibufel, int *ibufelnbnod,
                      int *ibufelnod, int *dimb, int *ibufcneldb, int *ibufnbeldb, int *typ, 
                      int *bcs, int *print, int *rddl, int *nl, int *nlnk, int *iex);
void _FCALL  INIT_LINK_SPMD_C(int *igd, int *nng, int *dbnod, int *nbproc, int *ibuf, int *dbibuf, 
                              int *dbnbuf, int *ddbuf, my_real_c *rbuf, int *dim, int *ibufnb, 
                              int *ibufcnel, int *nbel, int *dimel, int *ibufel, int *ibufelnbnod, 
                              int *ibufelnod, int *dimb, int *ibufcneldb, int *ibufnbeldb, int *typ, 
                              int *bcs, int *print, int *rddl, int *nl, int *nlnk, int *iex);
void _FCALL init_link_spmd_c_(int *igd, int *nng, int *dbnod, int *nbproc, int *ibuf, int *dbibuf, 
                              int *dbnbuf, int *ddbuf, my_real_c *rbuf, int *dim, int *ibufnb, 
                              int *ibufcnel, int *nbel, int *dimel, int *ibufel, int *ibufelnbnod, 
                              int *ibufelnod, int *dimb, int *ibufcneldb, int *ibufnbeldb, int *typ, 
                              int *bcs, int *print, int *rddl, int *nl, int *nlnk, int *iex);
void _FCALL init_link_spmd_c__(int *igd, int *nng, int *dbnod, int *nbproc, int *ibuf, int *dbibuf, 
                               int *dbnbuf, int *ddbuf, my_real_c *rbuf, int *dim, int *ibufnb, 
                               int *ibufcnel, int *nbel, int *dimel, int *ibufel, int *ibufelnbnod, 
                               int *ibufelnod, int *dimb, int *ibufcneldb, int *ibufnbeldb, int *typ,
                               int *bcs, int *print, int *rddl, int *nl, int *nlnk, int *iex);
//-------------------------------------------------------------
void send_ibuf_c(int *ibuf, int *len);
void _FCALL SEND_IBUF_C(int *ibuf, int *len);
void _FCALL send_ibuf_c_(int *ibuf, int *len);
void _FCALL send_ibuf_c__(int *ibuf, int *len);
//-------------------------------------------------------------
void send_fbuf_c(my_real_c *fbuf, int *len);
void _FCALL SEND_FBUF_C(my_real_c *fbuf, int *len);
void send_fbuf_c_(my_real_c *fbuf, int *len);
void send_fbuf_c__(my_real_c *fbuf, int *len);
//-------------------------------------------------------------
void send_fbufdp_c(double *fbuf, int *len);
void _FCALL SEND_FBUFDP_C(double *fbuf, int *len);
void send_fbufdp_c_(double *fbuf, int *len);
void send_fbufdp_c__(double *fbuf, int *len);
//-------------------------------------------------------------
void get_fbuf_c(my_real_c *fbuf, int *len);
void _FCALL GET_FBUF_C(my_real_c *fbuf, int *len);
void get_fbuf_c_(my_real_c *fbuf, int *len);
void get_fbuf_c__(my_real_c *fbuf, int *len);
//-------------------------------------------------------------

void get_fbufdp_c(double *fbuf, int *len);
void _FCALL GET_FBUFDP_C(double *fbuf, int *len);
void get_fbufdp_c_(double *fbuf, int *len);
void get_fbufdp_c__(double *fbuf, int *len);
//-------------------------------------------------------------
void get_ibuf_c(int *ibuf, int *len);
void _FCALL GET_IBUF_C(int *ibuf, int *len);
void get_ibuf_c_(int *ibuf, int *len);
void get_ibuf_c__(int *ibuf, int *len);
//-------------------------------------------------------------
void send_mass_c(int *idp, int *nng, int *nodbuf, my_real_c *ms, my_real_c *in);
void _FCALL SEND_MASS_C(int *idp, int *nng, int *nodbuf, my_real_c *ms, my_real_c *in);
void send_mass_c_(int *idp, int *nng, int *nodbuf, my_real_c *ms, my_real_c *in);
void send_mass_c__(int *idp, int *nng, int *nodbuf, my_real_c *ms, my_real_c *in);
//-------------------------------------------------------------
void send_mass_nl_c(int *idp, int *nng, int *iadd_nl, my_real_c *ms);
void _FCALL SEND_MASS_NL_C(int *idp, int *nng, int *iadd_nl, my_real_c *ms);
void send_mass_nl_c_(int *idp, int *nng, int *iadd_nl, my_real_c *ms);
void send_mass_nl_c__(int *idp, int *nng, int *iadd_nl, my_real_c *ms);
void send_mass_rby_c(int *idp, int *nng, int *nodbuf, my_real_c *ms, my_real_c *in, int *npby, 
                     int *nrbody, my_real_c *rby, int *tag, int *add_rby, int *nnpby, int *nrby);
void _FCALL SEND_MASS_RBY_C(int *idp, int *nng, int *nodbuf, my_real_c *ms, my_real_c *in, int *npby,
                            int *nrbody, my_real_c *rby, int *tag, int *add_rby, int *nnpby, int *nrby);
void send_mass_rby_c_(int *idp, int *nng, int *nodbuf, my_real_c *ms, my_real_c *in, int *npby, 
                      int *nrbody, my_real_c *rby, int *tag, int *add_rby, int *nnpby, int *nrby);
void send_mass_rby_c__(int *idp, int *nng, int *nodbuf, my_real_c *ms, my_real_c *in, int *npby, 
                       int *nrbody, my_real_c *rby, int *tag, int *add_rby, int *nnpby, int *nrby);
//-------------------------------------------------------------
void init_activ_c(int *activ);
void  _FCALL INIT_ACTIV_C(int *activ);
void init_activ_c_(int *activ);
void init_activ_c__(int *activ);
//-------------------------------------------------------------
void send_mass_spmd_c(int *idp, int *nng, my_real_c *buf1, my_real_c *buf2, int *iroddl);
void _FCALL SEND_MASS_SPMD_C(int *idp, int *nng, my_real_c *buf1, my_real_c *buf2, int *iroddl);
void send_mass_spmd_c_(int *idp, int *nng, my_real_c *buf1, my_real_c *buf2, int *iroddl);
void send_mass_spmd_c__(int *idp, int *nng, my_real_c *buf1, my_real_c *buf2, int *iroddl);
//-------------------------------------------------------------
void send_mass_rby_spmd_c(int *idp,int *nng,my_real_c *buf1,my_real_c *buf2,int *buf3,
                          my_real_c *buf4,my_real_c *buf5,int *iroddl);
void _FCALL SEND_MASS_RBY_SPMD_C(int *idp,int *nng,my_real_c *buf1,my_real_c *buf2,int *buf3,
                                 my_real_c *buf4,my_real_c *buf5,int *iroddl);
void send_mass_rby_spmd_c_(int *idp,int *nng,my_real_c *buf1,my_real_c *buf2,int *buf3,
                           my_real_c *buf4,my_real_c *buf5,int *iroddl);
void send_mass_rby_spmd_c__(int *idp,int *nng,my_real_c *buf1,my_real_c *buf2,int *buf3,
                            my_real_c *buf4,my_real_c *buf5,int *iroddl);
//-------------------------------------------------------------
void get_mass_c(int *idp, int *nng, int *nodbuf, my_real_c *ms, my_real_c *in);
void _FCALL GET_MASS_C(int *idp, int *nng, int *nodbuf, my_real_c *ms, my_real_c *in);
void get_mass_c_(int *idp, int *nng, int *nodbuf, my_real_c *ms, my_real_c *in);
void get_mass_c__(int *idp, int *nng, int *nodbuf, my_real_c *ms, my_real_c *in);
//-------------------------------------------------------------
void get_mass_rby_c(int *idp, int *nng, int *nodbuf, my_real_c *ms, my_real_c *in, my_real_c *x, int *npby, 
                    int *nrbody, my_real_c *rby, int *nnpby, int *nrby);
void _FCALL GET_MASS_RBY_C(int *idp, int *nng, int *nodbuf, my_real_c *ms, my_real_c *in, my_real_c *x, int *npby, 
                           int *nrbody, my_real_c *rby, int *nnpby, int *nrby);
void get_mass_rby_c_(int *idp, int *nng, int *nodbuf, my_real_c *ms, my_real_c *in, my_real_c *x, int *npby, 
                     int *nrbody, my_real_c *rby, int *nnpby, int *nrby);
void get_mass_rby_c__(int *idp, int *nng, int *nodbuf, my_real_c *ms, my_real_c *in, my_real_c *x, int *npby, 
                      int *nrbody, my_real_c *rby, int *nnpby, int *nrby);
//-------------------------------------------------------------
void get_mass_spmd_c(int *idp, int *nng, my_real_c *buf1, my_real_c *buf2);
void _FCALL GET_MASS_SPMD_C(int *idp, int *nng, my_real_c *buf1, my_real_c *buf2);
void get_mass_spmd_c_(int *idp, int *nng, my_real_c *buf1, my_real_c *buf2);
void get_mass_spmd_c__(int *idp, int *nng, my_real_c *buf1, my_real_c *buf2);
//-------------------------------------------------------------
void get_mass_rby_spmd_c(int *idp,int *nng,my_real_c *buf1,my_real_c *buf2,my_real_c *buf3,my_real_c *buf4);
void _FCALL GET_MASS_RBY_SPMD_C(int *idp,int *nng,my_real_c *buf1,my_real_c *buf2,my_real_c *buf3,my_real_c *buf4);
void get_mass_rby_spmd_c_(int *idp,int *nng,my_real_c *buf1,my_real_c *buf2,my_real_c *buf3,my_real_c *buf4);
void get_mass_rby_spmd_c__(int *idp,int *nng,my_real_c *buf1,my_real_c *buf2,my_real_c *buf3,my_real_c *buf4);
//-------------------------------------------------------------
void check_dtnoda_c(int *i7kglo);
void check_dtnoda_c_(int *i7kglo);
void _FCALL CHECK_DTNODA_C(int *i7kglo);
void check_dtnoda_c__(int *i7kglo);
//-------------------------------------------------------------
void send_data_c(int *idp, int *nng, int *nodbuf, 
                 my_real_c *fx, my_real_c *fr, my_real_c *stx, my_real_c *str, 
                 my_real_c *vx, my_real_c *vr, my_real_c *ms, my_real_c *in, 
                 double *dx, my_real_c *x, int *typ, int *npas, my_real_c *rby, 
                 int *tag_rby, int *add_rby, int *rbylnk, int *kin, double *dr, 
                 my_real_c *dt2, int *iex, int *off_sph, int *numsph_glo, int *nrby);
void _FCALL SEND_DATA_C(int *idp, int *nng, int *nodbuf, 
                 my_real_c *fx, my_real_c *fr, my_real_c *stx, my_real_c *str, 
                 my_real_c *vx, my_real_c *vr, my_real_c *ms, my_real_c *in, 
                 double *dx, my_real_c *x, int *typ, int *npas, my_real_c *rby, 
                 int *tag_rby, int *add_rby, int *rbylnk, int *kin, double *dr, 
                 my_real_c *dt2, int *iex, int *off_sph, int *numsph_glo, int *nrby);
void send_data_c_(int *idp, int *nng, int *nodbuf, 
                 my_real_c *fx, my_real_c *fr, my_real_c *stx, my_real_c *str, 
                 my_real_c *vx, my_real_c *vr, my_real_c *ms, my_real_c *in, 
                 double *dx, my_real_c *x, int *typ, int *npas, my_real_c *rby, 
                 int *tag_rby, int *add_rby, int *rbylnk, int *kin, double *dr, 
                 my_real_c *dt2, int *iex, int *off_sph, int *numsph_glo, int *nrby);
void send_data_c__(int *idp, int *nng, int *nodbuf, 
                 my_real_c *fx, my_real_c *fr, my_real_c *stx, my_real_c *str, 
                 my_real_c *vx, my_real_c *vr, my_real_c *ms, my_real_c *in, 
                 double *dx, my_real_c *x, int *typ, int *npas, my_real_c *rby, 
                 int *tag_rby, int *add_rby, int *rbylnk, int *kin, double *dr, 
                 my_real_c *dt2, int *iex, int *off_sph, int *numsph_glo, int *nrby);
//-------------------------------------------------------------
void send_data_nl_c(int *idp, int *nng, int *iadd_nl, my_real_c *fx, my_real_c *vx, my_real_c *ms, int *npas, int *iex);
void _FCALL SEND_DATA_NL_C(int *idp, int *nng, int *iadd_nl, my_real_c *fx, my_real_c *vx, my_real_c *ms, int *npas, int *iex);
void send_data_nl_c_(int *idp, int *nng, int *iadd_nl, my_real_c *fx, my_real_c *vx, my_real_c *ms, int *npas, int *iex);
void send_data_nl_c__(int *idp, int *nng, int *iadd_nl, my_real_c *fx, my_real_c *vx, my_real_c *ms, int *npas, int *iex);
//-------------------------------------------------------------
void send_data_spmd_c(int *idp, int *nng, 
                      my_real_c *bufr1, my_real_c *bufr2, my_real_c *bufr3, 
                      my_real_c *bufr4, my_real_c *bufr5, my_real_c *bufr6, 
                      double *bufr7, my_real_c *bufr8, my_real_c *bufr9, 
                      my_real_c *buf_rby, int *flg_rby, int *typ, int *npas, int *iex);
void _FCALL SEND_DATA_SPMD_C(int *idp, int *nng, 
                      my_real_c *bufr1, my_real_c *bufr2, my_real_c *bufr3, 
                      my_real_c *bufr4, my_real_c *bufr5, my_real_c *bufr6, 
                      double *bufr7, my_real_c *bufr8, my_real_c *bufr9, 
                      my_real_c *buf_rby, int *flg_rby, int *typ, int *npas, int *iex);
void send_data_spmd_c_(int *idp, int *nng, 
                      my_real_c *bufr1, my_real_c *bufr2, my_real_c *bufr3, 
                      my_real_c *bufr4, my_real_c *bufr5, my_real_c *bufr6, 
                      double *bufr7, my_real_c *bufr8, my_real_c *bufr9, 
                      my_real_c *buf_rby, int *flg_rby, int *typ, int *npas, int *iex);
void send_data_spmd_c__(int *idp, int *nng, 
                      my_real_c *bufr1, my_real_c *bufr2, my_real_c *bufr3, 
                      my_real_c *bufr4, my_real_c *bufr5, my_real_c *bufr6, 
                      double *bufr7, my_real_c *bufr8, my_real_c *bufr9, 
                      my_real_c *buf_rby, int *flg_rby, int *typ, int *npas, int *iex);
//-------------------------------------------------------------
void get_stiff_c(int *idp, int *nng, int *nodbuf, 
                 my_real_c *ms, my_real_c *ir,
                 my_real_c *stx, my_real_c *str,
                 int *typ, int *npas, int *iex);
void _FCALL GET_STIFF_C(int *idp, int *nng, int *nodbuf, 
                 my_real_c *ms, my_real_c *ir,
                 my_real_c *stx, my_real_c *str,
                 int *typ, int *npas, int *iex);
void get_stiff_c_(int *idp, int *nng, int *nodbuf, 
                 my_real_c *ms, my_real_c *ir,
                 my_real_c *stx, my_real_c *str,
                 int *typ, int *npas, int *iex);
void get_stiff_c__(int *idp, int *nng, int *nodbuf, 
                 my_real_c *ms, my_real_c *ir,
                 my_real_c *stx, my_real_c *str,
                 int *typ, int *npas, int *iex);
//-------------------------------------------------------------
void get_stiff_spmd_c(int *idp, int *nng, 
                      my_real_c *bufr1, my_real_c *bufr2, my_real_c *bufr3, my_real_c *bufr4, 
                      int *typ, int *npas, int *iex, int *nglob);
void _FCALL GET_STIFF_SPMD_C(int *idp, int *nng, 
                      my_real_c *bufr1, my_real_c *bufr2, my_real_c *bufr3, my_real_c *bufr4, 
                      int *typ, int *npas, int *iex, int *nglob);
void get_stiff_spmd_c_(int *idp, int *nng, 
                      my_real_c *bufr1, my_real_c *bufr2, my_real_c *bufr3, my_real_c *bufr4, 
                      int *typ, int *npas, int *iex, int *nglob);
void get_stiff_spmd_c__(int *idp, int *nng, 
                      my_real_c *bufr1, my_real_c *bufr2, my_real_c *bufr3, my_real_c *bufr4, 
                      int *typ, int *npas, int *iex, int *nglob);
//-------------------------------------------------------------
void get_force_c(int *idp, int *nng, int *nodbuf, my_real_c *wf, 
                 my_real_c *wm, my_real_c *wf2, my_real_c *wm2, my_real_c *v,
                 my_real_c *vr, my_real_c *fx, my_real_c *fr, my_real_c *ms, 
                 my_real_c *in, my_real_c *x, double *xdp, my_real_c *dx, 
                 int *typ, int *kin, int *wgt, int *iex, int *iresp, double *tfext);
//-----------------------------------------------------
void _FCALL GET_FORCE_C(int *idp, int *nng, int *nodbuf, my_real_c *wf, 
                 my_real_c *wm, my_real_c *wf2, my_real_c *wm2, my_real_c *v,
                 my_real_c *vr, my_real_c *fx, my_real_c *fr, my_real_c *ms, 
                 my_real_c *in, my_real_c *x, double *xdp, my_real_c *dx, 
                 int *typ, int *kin, int *wgt, int *iex, int *iresp, double *tfext);
void get_force_c_(int *idp, int *nng, int *nodbuf, my_real_c *wf, 
                 my_real_c *wm, my_real_c *wf2, my_real_c *wm2, my_real_c *v,
                 my_real_c *vr, my_real_c *fx, my_real_c *fr, my_real_c *ms, 
                 my_real_c *in, my_real_c *x, double *xdp, my_real_c *dx, 
                 int *typ, int *kin, int *wgt, int *iex, int *iresp, double *tfext);
void get_force_c__(int *idp, int *nng, int *nodbuf, my_real_c *wf, 
                 my_real_c *wm, my_real_c *wf2, my_real_c *wm2, my_real_c *v,
                 my_real_c *vr, my_real_c *fx, my_real_c *fr, my_real_c *ms, 
                 my_real_c *in, my_real_c *x, double *xdp, my_real_c *dx, 
                 int *typ, int *kin, int *wgt, int *iex, int *iresp, double *tfext);
//-----------------------------------------------------
void get_force_nl_c(int *idp, int *nng, int *iadd_nl, my_real_c *fx, my_real_c *ms, int *iex);
void _FCALL GET_FORCE_NL_C(int *idp, int *nng, int *iadd_nl, my_real_c *fx, my_real_c *ms, int *iex);
void get_force_nl_c_(int *idp, int *nng, int *iadd_nl, my_real_c *fx, my_real_c *ms, int *iex);
void get_force_nl_c__(int *idp, int *nng, int *iadd_nl, my_real_c *fx, my_real_c *ms, int *iex);
//-----------------------------------------------------
void get_force_spmd_c(int *idp, int *nng, 
                             my_real_c *bufr1,  my_real_c *bufr2,  my_real_c *bufr3,  
                             my_real_c *bufr4, int *typ, int *iex, int *nglob);
void _FCALL GET_FORCE_SPMD_C(int *idp, int *nng, 
                             my_real_c *bufr1,  my_real_c *bufr2,  my_real_c *bufr3,  
                             my_real_c *bufr4, int *typ, int *iex, int *nglob);
void get_force_spmd_c_(int *idp, int *nng, 
                             my_real_c *bufr1,  my_real_c *bufr2,  my_real_c *bufr3,  
                             my_real_c *bufr4, int *typ, int *iex, int *nglob);
void get_force_spmd_c__(int *idp, int *nng, 
                             my_real_c *bufr1,  my_real_c *bufr2,  my_real_c *bufr3,  
                             my_real_c *bufr4, int *typ, int *iex, int *nglob);
//-----------------------------------------------------
static void do_activ_c(int *iflg);
void _FCALL DO_ACTIV_C(int *iflg);
void do_activ_c_(int *iflg);
void do_activ_c__(int *iflg);
//-----------------------------------------------------
void send_mass_kine_c(int *idp, int *nng, int *nodbuf, my_real_c *ms, my_real_c *in, int *iex, int *offset);
void _FCALL SEND_MASS_KINE_C(int *idp, int *nng, int *nodbuf, my_real_c *ms, my_real_c *in, int *iex, int *offset);
void send_mass_kine_c_(int *idp, int *nng, int *nodbuf, my_real_c *ms, my_real_c *in, int *iex, int *offset);
void send_mass_kine_c__(int *idp, int *nng, int *nodbuf, my_real_c *ms, my_real_c *in, int *iex, int *offset);
//---------------------------------------------
void get_displ_c(int *idp, int *nng, int *nodbuf, my_real_c *x);
void _FCALL GET_DISPL_C(int *idp, int *nng, int *nodbuf, my_real_c *x);
void get_displ_c_(int *idp, int *nng, int *nodbuf, my_real_c *x);
void get_displ_c__(int *idp, int *nng, int *nodbuf, my_real_c *x);
//---------------------------------------------
void get_displ_spmd_c(int *idp, int *nng, my_real_c *bufr);
void _FCALL GET_DISPL_SPMD_C(int *idp, int *nng, my_real_c *bufr);
void get_displ_spmd_c_(int *idp, int *nng, my_real_c *bufr);
void get_displ_spmd_c__(int *idp, int *nng, my_real_c *bufr);
//-------------------------------------------------------------
static void close_r2r_pipe_c();
void _FCALL CLOSE_R2R_PIPE_C();
void close_r2r_pipe_c_();
void close_r2r_pipe_c__();
/***************Routines for socket communication***********************/

void connection_sock_init_c(int *sd);
void _FCALL CONNECTION_SOCK_INIT_C(int *sd);
void connection_sock_init_c_(int *sd);
void connection_sock_init_c__(int *sd);
//-------------------------------------------------------------
void send_sock_init_c(int *iroot,int *len,int *ispmd,int *sd,int *maxproc,int *imach);
void _FCALL SEND_SOCK_INIT_C(int *iroot, int *len, int *ispmd, int *sd, int *maxproc, int *imach);
void send_sock_init_c_(int *iroot, int *len, int *ispmd, int *sd, int *maxproc, int *imach);
void send_sock_init_c__(int *iroot, int *len, int *ispmd, int *sd, int *maxproc, int *imach);
//-------------------------------------------------------------
void connection_sock_c(int *ispmd, int *sd, char *addr);
void _FCALL CONNECTION_SOCK_C(int *ispmd, int *sd, char *addr);
void connection_sock_c_(int *ispmd, int *sd, char *addr);
void connection_sock_c__(int *ispmd, int *sd, char *addr);
//-------------------------------------------------------------
void mess_sock_c(int *sd);
void _FCALL MESS_SOCK_C(int *sd);
void mess_sock_c_(int *sd);
void mess_sock_c__(int *sd);
//-------------------------------------------------------------
void send_sock_c(int *sd);
void _FCALL SEND_SOCK_C(int *sd);
void send_sock_c_(int *sd);
void send_sock_c__(int *sd);
//--------------------------------------------------------------
void get_sock_ibuf_c(int *sd, int *ibuf, int *len);
void _FCALL GET_SOCK_IBUF_C(int *sd, int *ibuf, int *len);
void get_sock_ibuf_c_(int *sd, int *ibuf, int *len);
void get_sock_ibuf_c__(int *sd, int *ibuf, int *len);
//--------------------------------------------------------------
void send_sock_ibuf_c(int *sd, int *ibuf, int *len);
void _FCALL SEND_SOCK_IBUF_C(int *sd, int *ibuf, int *len);
void send_sock_ibuf_c_(int *sd, int *ibuf, int *len);
void send_sock_ibuf_c__(int *sd, int *ibuf, int *len);
//---------------------------------------------
void send_sock_rbuf_c(int *sd, my_real_c *rbuf, int *len);
void _FCALL SEND_SOCK_RBUF_C(int *sd, my_real_c *rbuf, int *len);
void send_sock_rbuf_c_(int *sd, my_real_c *rbuf, int *len);
void send_sock_rbuf_c__(int *sd, my_real_c *rbuf, int *len);
//---------------------------------------------
void get_sock_rbuf_c(int *sd, my_real_c *rbuf, int *len);
void _FCALL GET_SOCK_RBUF_C(int *sd, my_real_c *rbuf, int *len);
void get_sock_rbuf_c_(int *sd, my_real_c *rbuf, int *len);
void get_sock_rbuf_c__(int *sd, my_real_c *rbuf, int *len);
//---------------------------------------------
void send_sock_mess_c(int *sd, char *mess, int *len);
void _FCALL SEND_SOCK_MESS_C(int *sd, char *mess, int *len);
void send_sock_mess_c_(int *sd, char *mess, int *len);
void send_sock_mess_c__(int *sd, char *mess, int *len);
//-------------------------------------
void get_sock_mess_c(int *sd, char *mess, int *len);
void _FCALL GET_SOCK_MESS_C(int *sd, char *mess, int *len);
void get_sock_mess_c_(int *sd, char *mess, int *len);
void get_sock_mess_c__(int *sd, char *mess, int *len);
//------------------------------------------------è
void close_sock_c(int *sd);
void _FCALL CLOSE_SOCK_C(int *sd);
void close_sock_c_(int *sd);
void close_sock_c__(int *sd);
//-------------------------------------------------------------
void get_name_c(char *name);
void _FCALL GET_NAME_C(char *name);
void get_name_c_(char *name);
void get_name_c__(char *name);
//-------------------------------------------------------------
void exch_itag_c(int *idp, int *nng, int *nodbuf,
                 int *itag, int *itag2, int *iex, int *offset,
                 int *flag);
void _FCALL EXCH_ITAG_C(int *idp, int *nng, int *nodbuf,
                 int *itag, int *itag2, int *iex, int *offset,
                 int *flag);
void exch_itag_c_(int *idp, int *nng, int *nodbuf,
                 int *itag, int *itag2, int *iex, int *offset,
                 int *flag);
void exch_itag_c__(int *idp, int *nng, int *nodbuf,
                 int *itag, int *itag2, int *iex, int *offset,
                 int *flag);
//-------------------------------------------------------------
void realloc_shmvs_c(int newsize);
//----------------------------------------
void realloc_shmvr_c(int newsize);
//------------------------------------------------
void exch_tagel_c(int *ntagel, int *tagel, int *flag);
void _FCALL EXCH_TAGEL_C(int *ntagel, int *tagel, int *flag);
void exch_tagel_c_(int *ntagel, int *tagel, int *flag);
void exch_tagel_c__(int *ntagel, int *tagel, int *flag);
//---------------------------------------------------------------
void get_shmbuf_c(int *val1, int *val2);
void _FCALL GET_SHMBUF_C(int *val1, int *val2);
void get_shmbuf_c_(int *val1, int *val2);
void get_shmbuf_c__(int *val1, int *val2);
void send_shmbuf_c(int *val1,int *val2);
void _FCALL SEND_SHMBUF_C(int *val1,int *val2);
void send_shmbuf_c_(int *val1,int *val2);
void send_shmbuf_c__(int *val1,int *val2);

