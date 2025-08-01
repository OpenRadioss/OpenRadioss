Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||    xfeoff            ../engine/source/elements/xfem/xfeoff.F
!||--- called by ------------------------------------------------------
!||    resol             ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    activ_xfem        ../engine/source/elements/xfem/activ_xfem.F
!||    spmd_exch_iedge   ../engine/source/mpi/elements/spmd_xfem.F
!||    spmd_max_xfe_i    ../engine/source/mpi/elements/spmd_xfem.F
!||    startimeg         ../engine/source/system/timer.F
!||    stoptimeg         ../engine/source/system/timer.F
!||    upoffc            ../engine/source/elements/xfem/upoffc.F
!||    upofftg           ../engine/source/elements/xfem/upofftg.F
!||--- uses       -----------------------------------------------------
!||    crackxfem_mod     ../engine/share/modules/crackxfem_mod.F
!||    elbufdef_mod      ../common_source/modules/mat_elem/elbufdef_mod.F90
!||====================================================================
      SUBROUTINE XFEOFF(XFEM_TAB ,
     .                  IPARG    ,IXC      ,NGROUC   ,IGROUC  ,IEL_CRK ,
     .                  ELCUTC   ,IXTG     ,IADC_CRK ,IAD_ELEM,IAD_EDGE,
     .                  FR_EDGE  ,FR_NBEDGE,FR_ELEM  ,NLAY    ,INOD_CRK,
     .                  CRKEDGE  ,XEDGE4N  ,XEDGE3N  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE CRACKXFEM_MOD
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com_xfem1.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),IXC(NIXC,*),NGROUC,IGROUC(*),
     .        IEL_CRK(*),ELCUTC(2,*),IXTG(NIXTG,*),IADC_CRK(*),
     .        IAD_ELEM(2,*),IAD_EDGE(*),FR_EDGE(*),FR_NBEDGE(*),
     .        NLAY,FR_ELEM(*),INOD_CRK(*),XEDGE4N(4,*),XEDGE3N(3,*)
C
      TYPE(ELBUF_STRUCT_),  DIMENSION(NGROUP,NXEL) :: XFEM_TAB
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IG,NG,JFT,JLT,NEL,NF1,IXFEM,IADXFEM,NXLAY,OFF,
     .  STEP,ITG1,ITG2,FLAG,SIZE,LSDRC,ACTIFXFEM
C=======================================================================
C activation of new group if crack advancing or new crack initiate
c-----------------------------------------------------------------------
      IF (NSPMD > 1) THEN
        FLAG = 1
        SIZE  = NLAY
        LSDRC = FR_NBEDGE(NSPMD+1)
        CALL SPMD_EXCH_IEDGE(IAD_EDGE,FR_EDGE,SIZE  ,LSDRC,FR_NBEDGE,
     .                       FLAG    ,CRKEDGE)
      ENDIF
C---
C_tmp      IF(NUMELCRK2 == NUMELCRK)RETURN  ! check in hypethreading, SPMD
C---
C----------------------------------------
      ITG1 = 1+NUMELC
      ITG2 = 1+4*ECRKXFEC
c-------------------------------
C Boucle parallele dynamique SMP
c-------------------------------
!$OMP DO SCHEDULE(DYNAMIC,1)
c
      DO IG = 1, NGROUC
        NG  = IGROUC(IG)                    
        OFF   = IPARG(8,NG)                 
        IXFEM = IPARG(54,NG)                
        IF (IXFEM > 0 .and. OFF < 1) THEN  
          IF (IDDW > 0) CALL STARTIMEG(NG)
C---
          NEL   = IPARG(2,NG)
          NFT   = IPARG(3,NG)
          ITY   = IPARG(5,NG)
          NXLAY = IPARG(59,NG)
          LFT   = 1 
          LLT   = MIN(NVSIZ,NEL)
          JFT=LFT
          JLT=LLT
C---
          IF (ITY == 3) THEN
            CALL ACTIV_XFEM(IPARG ,NFT   ,JFT    ,JLT ,NXLAY,
     .                      NG    ,ELCUTC,IEL_CRK,ITY ,CRKEDGE)
C---
          ELSE IF (ITY == 7) THEN
            CALL ACTIV_XFEM(IPARG ,NFT   ,JFT    ,JLT ,NXLAY,
     .                      NG    ,ELCUTC(1,ITG1),IEL_CRK(ITG1),ITY,CRKEDGE)
          ENDIF
C---
          IF (IDDW > 0) CALL STOPTIMEG(NG)
        ENDIF
      ENDDO
!$OMP END DO
C-------------
C
C Boucle parallele dynamique SMP
C
!$OMP DO SCHEDULE(DYNAMIC,1)
      DO IG = 1, NGROUC
        NG = IGROUC(IG)
        OFF       = IPARG(8,NG)             
        IXFEM     = IPARG(54,NG)
        ACTIFXFEM = IPARG(70,NG)
        IF (IXFEM > 0 .and. OFF < 1 .and. ACTIFXFEM > 0) THEN  
          IF (IDDW > 0) CALL STARTIMEG(NG)
C---
          NEL   = IPARG(2,NG)
          NFT   = IPARG(3,NG)
          ITY   = IPARG(5,NG)
          NPT   = IPARG(6,NG)
          NXLAY = IPARG(59,NG)
          LFT   = 1 
          LLT   = MIN(NVSIZ,NEL)
          JFT=LFT
          JLT=LLT
C---
          IF (ITY == 3) THEN
            CALL UPOFFC(XFEM_TAB ,NG   ,
     .                  NFT      ,JFT  ,JLT   ,IXFEM    ,IEL_CRK ,
     .                  ELCUTC   ,INOD_CRK,IADC_CRK ,IXC  ,NXLAY ,
     .                  CRKEDGE  ,XEDGE4N )
C---
          ELSE IF (ITY == 7) THEN
            CALL UPOFFTG(XFEM_TAB      ,NG   ,
     .                   NFT           ,JFT  ,JLT   ,IXFEM ,IEL_CRK(ITG1),
     .                   ELCUTC(1,ITG1),INOD_CRK,IADC_CRK(ITG2),IXTG ,NXLAY ,
     .                   CRKEDGE       ,XEDGE3N )
          ENDIF
C---
          IF (IDDW > 0) CALL STOPTIMEG(NG)
        ENDIF
      ENDDO
!$OMP END DO
C-------------
      IF (NSPMD > 1) THEN
        FLAG = 0
        SIZE  = NLAY
        LSDRC = FR_NBEDGE(NSPMD+1)
        CALL SPMD_EXCH_IEDGE(IAD_EDGE,FR_EDGE,SIZE  ,LSDRC,FR_NBEDGE,
     .                       FLAG    ,CRKEDGE)
C
        FLAG = 3
        SIZE  = 6*NLAY
        LSDRC = FR_NBEDGE(NSPMD+1)
        CALL SPMD_EXCH_IEDGE(IAD_EDGE,FR_EDGE,SIZE  ,LSDRC,FR_NBEDGE,
     .                       FLAG    ,CRKEDGE)
C
        CALL SPMD_MAX_XFE_I(NUMELCRK)  ! no more used (just for anim reasons)
      ENDIF
C-------------
      RETURN
      END
