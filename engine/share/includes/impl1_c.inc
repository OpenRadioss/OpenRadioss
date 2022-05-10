Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
C.../IMPL1/
      COMMON /IMPL1/ 
     .       IMPL_S, ISOLV, IPREC,  ITOL, NITOL , MAXB  ,  
     .       ILINE , ISPRB ,IMCONV,ISETK ,N_LIM , L_LIM ,  NEXP,
     .       IKG   , IKPAT , NNSIZ,LPRINT,NPRINT,  MAX_L,INSOLV,
     .       IDTC  ,IDYNA  ,NL_DTP,NL_DTN,IMPDEB, IDSC,MAXB0,IMPMV,
     .       IMP_RBY,IMP_INT,ISPRN,IMUMPSD,NDDL_L,ISIGINI,IDSGAP,ILINTF,
     .       INTP_C ,L_BFGS ,IRREF,IMPL_S0,N_PAT ,MAXB1 ,IMP_CHK,
     .       IMP_IW ,IMP_IR ,IDY_DAMP,IQSTAT, NDEB0 ,NDEB1,IMP_INT7,
     .       NT_IMP1,ITTOFF ,NDDLI_G ,IBUCKL,IAUTSPC,ISCAU,ITRMAX,
     .       IMP_LR ,LMEMV  ,IKT   ,INCONV ,IREFI  ,ILINE_S,NLS_LIM ,
     .       NDIVER ,IHELAS ,NDTFIX,IDTFIX ,IKPRES ,ISOLV_D,IKPROJ ,
     .       IAL_M  ,NCY_MAX,ILAST ,NT_IMP5,IPUPD  ,IT_BCS,IT_PCG ,
     .       ISMDISP,ITER_NL,NDDL_G,NNZK_G ,NPCGPV ,IPRO_S0,INEGA ,
     .       IRWALL ,IMUMPSV,IIKGOFF,LCOKM,ISOLINFO,ISOLHIST,SOLVNFO,
     .       ISOLTPL,ISOLMNTR,PRSTIFMAT,PRSTIFMAT_NC,PRSTIFMAT_IT,
     .       M_MSG,M_ORDER,M_OCORE,IRIG_M,ISOLPGRS
      INTEGER IMPL_S, ISOLV, IPREC, ITOL,  NITOL,  MAXB ,  
     .       ILINE , ISPRB ,IMCONV,ISETK ,N_LIM , L_LIM ,  NEXP,
     .       IKG   , IKPAT , NNSIZ,LPRINT,NPRINT,  MAX_L,INSOLV,
     .       IDTC  ,IDYNA  ,NL_DTP,NL_DTN,IMPDEB, IDSC,MAXB0,IMPMV,
     .       IMP_RBY,IMP_INT,ISPRN,IMUMPSD,NDDL_L,ISIGINI,IDSGAP,ILINTF,
     .       INTP_C ,L_BFGS ,IRREF,IMPL_S0,N_PAT ,MAXB1 ,IMP_CHK,
     .       IMP_IW ,IMP_IR ,IDY_DAMP,IQSTAT, NDEB0 ,NDEB1,IMP_INT7,
     .       NT_IMP1,ITTOFF ,NDDLI_G ,IBUCKL,IAUTSPC,ISCAU,ITRMAX,
     .       IMP_LR ,LMEMV  ,IKT     ,INCONV,IREFI  ,ILINE_S,NLS_LIM,
     .       NDIVER ,IHELAS ,NDTFIX  ,IDTFIX,IKPRES ,ISOLV_D,IKPROJ ,
     .       IAL_M  ,NCY_MAX,ILAST   ,NT_IMP5,IPUPD  ,IT_BCS,IT_PCG ,
     .       ISMDISP,ITER_NL,NDDL_G,NNZK_G,NPCGPV ,IPRO_S0,INEGA  ,
     .       IRWALL ,IMUMPSV,IIKGOFF,LCOKM,ISOLINFO,ISOLHIST,SOLVNFO,
     .       ISOLTPL,ISOLMNTR,PRSTIFMAT,PRSTIFMAT_NC,PRSTIFMAT_IT , 
     .       M_MSG,M_ORDER,M_OCORE,IRIG_M,ISOLPGRS
      COMMON /IMPL1_PRIVATE/ NG_IMP
!$OMP THREADPRIVATE(/IMPL1_PRIVATE/)
      INTEGER NG_IMP 
C------L_LIM: linear iteration num limit;N_LIM : iteration num to update K
