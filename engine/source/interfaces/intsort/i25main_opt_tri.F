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
Chd|====================================================================
Chd|  I25MAIN_OPT_TRI               source/interfaces/intsort/i25main_opt_tri.F
Chd|-- called by -----------
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|-- calls ---------------
Chd|        I25IRTLM                      source/interfaces/int25/i25irtlm.F
Chd|        I25OPTCD                      source/interfaces/intsort/i25optcd.F
Chd|        I25OPTCD_E2S                  source/interfaces/intsort/i25optcd_e2s.F
Chd|        I25OPTCD_EDG                  source/interfaces/intsort/i25optcd_edg.F
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        UPGRADE_CAND_OPT              ../common_source/interf/upgrade_multimp.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25MAIN_OPT_TRI(
     1                  NIN   ,IPARI  ,INTBUF_TAB,X       ,V      ,
     2                  ITASK ,ITAB   ,KINET ,COUNT_REMSLV,
     3                  COUNT_REMSLVE, NB_CANDT, I_OPT_STOK    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD   
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr04_c.inc"
#include      "scr05_c.inc"
#include      "warn_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,NINTER),ITASK, NIN, ITAB(*), KINET(*),
     .        COUNT_REMSLV(*), NB_CANDT, I_OPT_STOK
      INTEGER :: COUNT_REMSLVE(*)
C     REAL ou REAL*8
      my_real 
     .   X(*), V(3,*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NSNR, IGAP, I_STOK_GLO, SIZOPT, IEDGE, NEDGE, IGAP0, I_STOK, L_STOK
      INTEGER N, NSNF, NSNL, NSNRF, NSNRL ,IFQ
C     REAL ou REAL*8
      my_real
     .   STARTT, STOPT, DRAD
C-----------------------------------------------------------
C     Prepare IRTLM 
C         - IRTLM/=0 <=> at the beginning of CAND_OPT
C-----------------------------------------------------------
!$OMP SINGLE
      CALL I25IRTLM(
     1  IPARI   ,INTBUF_TAB ,ITAB    ,NIN     )
!$OMP END SINGLE
C-----------------------------------------------------------
      STARTT=INTBUF_TAB%VARIABLES(3)
      STOPT =INTBUF_TAB%VARIABLES(11)
      IF(STARTT.GT.TT.OR.TT.GT.STOPT) RETURN
C-----------------------------------------------------------
      NRTM   =IPARI(4,NIN)
      NSN   =IPARI(5,NIN)
      NSNR  =IPARI(24,NIN)
      IGAP  =IPARI(21,NIN)
      IFQ   = IPARI(31,NIN)
      DRAD = ZERO
      IF(IPARI(47,NIN) > 0) DRAD =INTBUF_TAB%VARIABLES(32)
C-----------------------------------------------------------
C     Filtrage des candidats a l'impact
C     (si candidat retenu, cand_n(i) = - cand_n(i))
C-----------------------------------------------------------
       I_STOK_GLO = INTBUF_TAB%I_STOK(1)
       IF (DEBUG(3).GE.1) THEN
         IF(ITASK==0) NB_CANDT  = NB_CANDT  + I_STOK_GLO
       ENDIF
       SIZOPT = INTBUF_TAB%S_CAND_OPT_N
C
C      I_OPT_STOK modifie ds i25optcd, apres barriere !
       I_OPT_STOK = INTBUF_TAB%I_STOK(2)
       CALL I25OPTCD(
     1 INTBUF_TAB%NSV,INTBUF_TAB%CAND_E,INTBUF_TAB%CAND_N,X,I_STOK_GLO    ,
     2 INTBUF_TAB%IRECTM,INTBUF_TAB%GAP_S,INTBUF_TAB%GAP_M,V,IPARI(39,NIN),
     3 INTBUF_TAB%STFNS,ITASK     ,INTBUF_TAB%STFM,NIN      ,IPARI(5,NIN) ,
     4 INTBUF_TAB%IRTLM,INTBUF_TAB%TIME_S,INTBUF_TAB%MSEGLO,COUNT_REMSLV,ITAB,
     5 INTBUF_TAB%SECND_FR,NSNR       ,INTBUF_TAB%PENE_OLD,INTBUF_TAB%STIF_OLD,
     .                                                   INTBUF_TAB%MSEGTYP24,
     6 NRTM ,INTBUF_TAB%VARIABLES(23),I_OPT_STOK,INTBUF_TAB%CAND_OPT_E,
     .                                               INTBUF_TAB%CAND_OPT_N,
     7 SIZOPT,IGAP   ,INTBUF_TAB%GAP_SL,INTBUF_TAB%GAP_ML,DRAD)
C
C      I_OPT_STOK modifie ds i25optcd, avant barriere
       IF(I_OPT_STOK > SIZOPT)THEN
C
         IF(ITASK==0)THEN
           CALL UPGRADE_CAND_OPT(NIN,I_OPT_STOK-INTBUF_TAB%I_STOK(2),INTBUF_TAB)
         END IF
C
         CALL MY_BARRIER
C
C        I_OPT_STOK modifie ds i25optcd, apres barriere !
         SIZOPT = INTBUF_TAB%S_CAND_OPT_N
         I_OPT_STOK = INTBUF_TAB%I_STOK(2)
         CALL I25OPTCD(
     1     INTBUF_TAB%NSV,INTBUF_TAB%CAND_E,INTBUF_TAB%CAND_N,X,I_STOK_GLO    ,
     2     INTBUF_TAB%IRECTM,INTBUF_TAB%GAP_S,INTBUF_TAB%GAP_M,V,IPARI(39,NIN),
     3     INTBUF_TAB%STFNS,ITASK     ,INTBUF_TAB%STFM,NIN      ,IPARI(5,NIN) ,
     4     INTBUF_TAB%IRTLM,INTBUF_TAB%TIME_S,INTBUF_TAB%MSEGLO,COUNT_REMSLV,ITAB,
     5     INTBUF_TAB%SECND_FR,NSNR       ,INTBUF_TAB%PENE_OLD,INTBUF_TAB%STIF_OLD,
     .                                                       INTBUF_TAB%MSEGTYP24,
     6     NRTM ,INTBUF_TAB%VARIABLES(23),I_OPT_STOK,INTBUF_TAB%CAND_OPT_E,
     .                                                   INTBUF_TAB%CAND_OPT_N,
     7     SIZOPT,IGAP   ,INTBUF_TAB%GAP_SL,INTBUF_TAB%GAP_ML ,DRAD       )
       END IF
C
C      I_OPT_STOK modifie ds i25optcd, avant barriere
       INTBUF_TAB%I_STOK(2)=I_OPT_STOK
C
Cf barrier at the end of i25optcd
      NSNF = 1 + ITASK*NSN / NTHREAD
      NSNL = (ITASK+1)*NSN / NTHREAD
      DO N = NSNF,NSNL 
C       release node for future impact (at next cycles)
        IF(INTBUF_TAB%IRTLM(4*(N-1)+3) < 0) INTBUF_TAB%IRTLM(4*(N-1)+3)=0
      ENDDO
C
      NSNRF = 1 + ITASK*NSNR / NTHREAD
      NSNRL = (ITASK+1)*NSNR / NTHREAD
      DO N=NSNRF,NSNRL
        IF(IRTLM_FI(NIN)%P(3,N) < 0) IRTLM_FI(NIN)%P(3,N)=0
      ENDDO
C
      IEDGE = IPARI(58,NIN)
      IGAP0 = IPARI(53,NIN)
      IF(IEDGE/=0)THEN
        NEDGE  = IPARI(68,NIN)
        CALL I25OPTCD_EDG(INTBUF_TAB%CANDM_E2E,INTBUF_TAB%CANDS_E2E,
     .                       X      ,INTBUF_TAB%I_STOK_E(1),INTBUF_TAB%IRECTM  ,
     2                    NIN   ,V     ,INTBUF_TAB%GAPE ,IGAP  ,ITASK ,
     3                    INTBUF_TAB%STFM,INTBUF_TAB%GAP_E_L,COUNT_REMSLVE,DRAD  ,
     4                    IEDGE ,NEDGE  ,INTBUF_TAB%LEDGE   ,INTBUF_TAB%MVOISIN ,
     .                                                           INTBUF_TAB%NSV ,
     5                    IGAP0 ,INTBUF_TAB%STFE,
     6                    INTBUF_TAB%S_STFM, INTBUF_TAB%S_STFE,IFQ,INTBUF_TAB%IFPEN_E,
     7     INTBUF_TAB%FTSAVX_E,INTBUF_TAB%FTSAVY_E, INTBUF_TAB%FTSAVZ_E )



        CALL I25OPTCD_E2S(INTBUF_TAB%CANDM_E2S,INTBUF_TAB%CANDS_E2S,
     .                      X      ,INTBUF_TAB%I_STOK_E(2),INTBUF_TAB%IRECTM  ,
     2                    NIN   ,V     ,INTBUF_TAB%GAP_M ,IGAP  ,ITASK ,
     3                    INTBUF_TAB%STFM,INTBUF_TAB%GAP_ML,COUNT_REMSLVE,DRAD  ,
     4                    IEDGE ,NEDGE  ,INTBUF_TAB%LEDGE  ,INTBUF_TAB%MVOISIN ,
     .                                                          INTBUF_TAB%NSV ,
     5                    NRTM,INTBUF_TAB%GAPE ,INTBUF_TAB%GAP_E_L ,IGAP0,
     6                    INTBUF_TAB%STFE,INTBUF_TAB%S_STFE,IFQ,INTBUF_TAB%IFPEN_E2S,
     7    INTBUF_TAB%FTSAVX_E2S,INTBUF_TAB%FTSAVY_E2S, INTBUF_TAB%FTSAVZ_E2S)
      END IF

C     call my_barrier ! pour timer only

C-----------------------------------------------------------------------
      RETURN
      END
