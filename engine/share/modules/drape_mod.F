Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
Copyright>
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>
Copyright>
Copyright>        Commercial Alternative: Altair Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
Copyright>        software under a commercial license.  Contact Altair to discuss further if the
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
      !||====================================================================
      !||    drape_mod          ../engine/share/modules/drape_mod.F
      !||--- called by ------------------------------------------------------
      !||    c3forc3            ../engine/source/elements/sh3n/coque3n/c3forc3.F
      !||    c3forc3_crk        ../engine/source/elements/xfem/c3forc3_crk.F
      !||    c3ke3              ../engine/source/elements/sh3n/coque3n/c3ke3.F
      !||    c_tf_ne            ../engine/source/output/sty/c_tf_ne.F
      !||    cbaforc3           ../engine/source/elements/shell/coqueba/cbaforc3.F
      !||    cbake3             ../engine/source/elements/shell/coqueba/cbake3.F
      !||    cdk6forc3          ../engine/source/elements/sh3n/coquedk6/cdk6forc3.F
      !||    cdkforc3           ../engine/source/elements/sh3n/coquedk/cdkforc3.F
      !||    cforc3             ../engine/source/elements/shell/coque/cforc3.F
      !||    cforc3_crk         ../engine/source/elements/xfem/cforc3_crk.F
      !||    cmain3             ../engine/source/materials/mat_share/cmain3.F
      !||    cmain3pinch        ../engine/source/elements/shell/coqueba/cmain3pinch.F
      !||    cmatc3             ../engine/source/elements/shell/coqueba/cmatc3.F
      !||    cncoefort          ../engine/source/elements/sh3n/coquedk/cncoef3.F
      !||    czforc3            ../engine/source/elements/shell/coquez/czforc3.F
      !||    czforc3_crk        ../engine/source/elements/xfem/czforc3_crk.F
      !||    czke3              ../engine/source/elements/shell/coquez/czke3.F
      !||    czkel3             ../engine/source/elements/shell/coquez/czkel3.F
      !||    drape_rrest        ../engine/source/output/restart/rdresb.F
      !||    drape_wrest        ../engine/source/output/restart/wrrest.F
      !||    dynain_c_strag     ../engine/source/output/dynain/dynain_c_strag.F
      !||    dynain_c_strsg     ../engine/source/output/dynain/dynain_c_strsg.F
      !||    eig                ../engine/stub/eig.F
      !||    eig1               ../engine/stub/eig1.F
      !||    eigcond            ../engine/stub/eigcond.F
      !||    eigp               ../engine/stub/eigp.F
      !||    forintc            ../engine/source/elements/forintc.F
      !||    gendynain          ../engine/source/output/dynain/gendynain.F
      !||    genh3d             ../engine/source/output/h3d/h3d_results/genh3d.F
      !||    genoutp            ../engine/source/output/sty/genoutp.F
      !||    genstat            ../engine/source/output/sta/genstat.F
      !||    h3d_shell_tensor   ../engine/source/output/h3d/h3d_results/h3d_shell_tensor.F
      !||    imp_buck           ../engine/source/implicit/imp_buck.F
      !||    imp_chkm           ../engine/source/implicit/imp_solv.F
      !||    imp_glob_k         ../engine/source/implicit/imp_glob_k.F
      !||    imp_glob_k0        ../engine/source/implicit/imp_glob_k.F
      !||    imp_glob_khp       ../engine/source/implicit/imp_glob_k.F
      !||    imp_k_eig          ../engine/stub/imp_k_eig.F
      !||    imp_solv           ../engine/source/implicit/imp_solv.F
      !||    layini             ../engine/source/elements/shell/coque/layini.F
      !||    outp_c_tf          ../engine/source/output/sty/outp_c_t.F
      !||    radioss2           ../engine/source/engine/radioss2.F
      !||    rdcomi             ../engine/source/output/restart/rdcomm.F
      !||    rdresb             ../engine/source/output/restart/rdresb.F
      !||    resol              ../engine/source/engine/resol.F
      !||    resol_head         ../engine/source/engine/resol_head.F
      !||    restalloc          ../engine/source/output/restart/arralloc.F
      !||    sortie_main        ../engine/source/output/sortie_main.F
      !||    stat_c_strafg      ../engine/source/output/sta/stat_c_strafg.F
      !||    stat_c_strsfg      ../engine/source/output/sta/stat_c_strsfg.F
      !||    wrcomi             ../engine/source/output/restart/wrcomm.F
      !||    wrrestp            ../engine/source/output/restart/wrrestp.F
      !||====================================================================
        MODULE DRAPE_MOD
c=======================================================================      
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
c=======================================================================      
c      
      TYPE DRAPE_PLY_
        INTEGER                           ::  NSLICE   ! number of slice
        INTEGER                           ::  IPID     ! ID PID
c-------  layer variables     
        my_real, DIMENSION(:,:) , ALLOCATABLE ::   RDRAPE
        INTEGER, DIMENSION(:,:) , ALLOCATABLE ::   IDRAPE 
c-------       
      END TYPE DRAPE_PLY_      
      !!
      TYPE DRAPE_
        INTEGER                                        ::  NPLY_DRAPE
        INTEGER                                        ::  NPLY
        INTEGER,            DIMENSION(:)   , ALLOCATABLE   ::  INDX_PLY
        TYPE (DRAPE_PLY_) , DIMENSION(:)   , ALLOCATABLE   ::  DRAPE_PLY
      END TYPE DRAPE_  
      !!
      TYPE DRAPEG_
        INTEGER        ::  NDIM_SH4N
        INTEGER        ::  NDIM_SH3N
        INTEGER        ::  NDIM_SLICE_SH4N
        INTEGER        ::  NDIM_SLICE_SH3N
        INTEGER        ::  NDIM_INDX_PLY_SH4N
        INTEGER        ::  NDIM_INDX_PLY_SH3N
        INTEGER, DIMENSION(:) , ALLOCATABLE  ::  INDX_SH3N,INDX_SH4N
      END TYPE DRAPEG_  
      INTEGER :: NUMELC_DRAPE    ,NUMELTG_DRAPE , SCDRAPE, STDRAPE 
C=======================================================================
      END MODULE DRAPE_MOD
