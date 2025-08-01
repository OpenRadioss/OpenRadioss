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
!||    upxfem2            ../engine/source/elements/xfem/upxfem2.F
!||--- called by ------------------------------------------------------
!||    resol              ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    spmd_exch_iedge    ../engine/source/mpi/elements/spmd_xfem.F
!||    spmd_exch_nodenr   ../engine/source/mpi/elements/spmd_xfem.F
!||    spmd_exch_redge    ../engine/source/mpi/elements/spmd_xfem.F
!||    spmd_max_xfe_i     ../engine/source/mpi/elements/spmd_xfem.F
!||    startimeg          ../engine/source/system/timer.F
!||    stoptimeg          ../engine/source/system/timer.F
!||    upenric_last       ../engine/source/elements/xfem/upenric_last.F
!||    upenritg_last      ../engine/source/elements/xfem/upenritg_last.F
!||--- uses       -----------------------------------------------------
!||    crackxfem_mod      ../engine/share/modules/crackxfem_mod.F
!||====================================================================
      SUBROUTINE UPXFEM2(IPARG    ,IXC    ,NGROUC  ,IGROUC ,IADC_CRK ,
     .                   IEL_CRK  ,ELCUTC ,IXTG    ,ENRTAG ,INOD_CRK ,
     .                   IAD_ELEM ,FR_ELEM,IAD_EDGE,FR_EDGE,FR_NBEDGE,
     .                   CRKEDGE  )
C-----------------------------------------------
      USE CRACKXFEM_MOD
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
     .        IADC_CRK(*),IEL_CRK(*),ELCUTC(2,*),IXTG(NIXTG,*),
     .        ENRTAG(NUMNOD,*),INOD_CRK(*),FR_ELEM(*),FR_EDGE(*),
     .        IAD_ELEM(2,*),IAD_EDGE(*),FR_NBEDGE(*)
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IG,NG,JFT,JLT,NEL,NF1,ITG1,ITG2,
     .        SIZE,LSDRC,FLAG,ACTIFXFEM,NXLAY
C=======================================================================
C---
C_tmp      IF(NUMELCRK2 == NUMELCRK)RETURN  ! check in hypethreading, SPMD
C---
C----------------------------------------
      ITG1 = 1+NUMELC
      ITG2 = 1 + ECRKXFEC*4
      NXLAY = NXLAYMAX
c-------------------------------
C Boucle parallele dynamique SMP
!$OMP DO SCHEDULE(DYNAMIC,1)
c-------------------------------
c     Save actual phantom enrichments at the end of cycle
c-------------------------------
      DO IG = 1, NGROUC
        NG = IGROUC(IG)
        IF (IPARG(54,NG) == 0) GOTO 100    ! IXFEM 
        IF (IPARG(8,NG)  == 1) GOTO 100    ! GROUP OFF
        IF (IDDW > 0) CALL STARTIMEG(NG)
C---
        ITY   = IPARG(5,NG)
        NEL   = IPARG(2,NG)
        NFT   = IPARG(3,NG)
        ACTIFXFEM=IPARG(70,NG)
        LFT   = 1 
        LLT   = MIN(NVSIZ,NEL)
        JFT=LFT
        JLT=LLT
        IF (ACTIFXFEM == 0) GOTO 100
C---
        IF (ITY == 3) THEN
          CALL UPENRIC_LAST(
     .        IXC    ,NFT     ,JFT    ,JLT  ,IADC_CRK, 
     .        IEL_CRK,ELCUTC,NXLAY   ,CRKEDGE )
C---
        ELSEIF (ITY == 7) THEN
          CALL UPENRITG_LAST(
     .        IXTG  ,NFT    ,JFT  ,JLT,IADC_CRK(ITG2),
     .        IEL_CRK(ITG1),ELCUTC(1,ITG1),NXLAY,CRKEDGE )
        ENDIF
C---
        IF (IDDW > 0) CALL STOPTIMEG(NG)
 100  CONTINUE
      END DO
!$OMP END DO
C-------------
      NUMELCRK2 = NUMELCRK  
c                                                anim file / crack advance)
C-------------
C
      IF(NSPMD > 1)THEN
        CALL SPMD_MAX_XFE_I(NUMELCRK2)  ! no more used
        SIZE = IENRNOD
        LSDRC = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
        FLAG = 2
        CALL SPMD_EXCH_NODENR(IAD_ELEM,FR_ELEM,SIZE,LSDRC,INOD_CRK,
     .                        ENRTAG,FLAG)
C
        FLAG = 2
        SIZE  = NXLAY
        LSDRC = FR_NBEDGE(NSPMD+1)
        CALL SPMD_EXCH_IEDGE(IAD_EDGE,FR_EDGE,SIZE  ,LSDRC,FR_NBEDGE,
     .                       FLAG    ,CRKEDGE)
C
        SIZE  = NXLAY
        LSDRC = FR_NBEDGE(NSPMD+1)
        CALL SPMD_EXCH_REDGE(IAD_EDGE,FR_EDGE,SIZE,LSDRC,FR_NBEDGE,CRKEDGE)
C
      END IF
C-------------
      RETURN
      END
