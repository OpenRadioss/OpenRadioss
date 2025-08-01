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
!||    inixfem              ../engine/source/elements/xfem/inixfem.F
!||--- called by ------------------------------------------------------
!||    resol                ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    crk_coord_ini        ../engine/source/elements/xfem/crk_coord_ini.F
!||    enrichc_ini          ../engine/source/elements/xfem/enrichc_ini.F
!||    enrichtg_ini         ../engine/source/elements/xfem/enrichtg_ini.F
!||    omp_get_thread_num   ../engine/source/engine/openmp_stub.F90
!||    spmd_exch_iedge      ../engine/source/mpi/elements/spmd_xfem.F
!||    startimeg            ../engine/source/system/timer.F
!||    stoptimeg            ../engine/source/system/timer.F
!||--- uses       -----------------------------------------------------
!||    crackxfem_mod        ../engine/share/modules/crackxfem_mod.F
!||    elbufdef_mod         ../common_source/modules/mat_elem/elbufdef_mod.F90
!||====================================================================
      SUBROUTINE INIXFEM(ELBUF_TAB,XFEM_TAB ,
     .                   IPARG    ,IXC      ,IXTG     ,NGROUC   ,IGROUC    ,
     .                   ELCUTC   ,IADC_CRK ,IEL_CRK  ,INOD_CRK ,ADDCNE_CRK,
     .                   X        ,KNOD2ELC ,NODEDGE  ,CRKNODIAD,IAD_EDGE  ,
     .                   FR_EDGE  ,FR_NBEDGE,NODLEVXF ,CRKEDGE  ,XEDGE4N   ,
     .                   XEDGE3N  )
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
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com_xfem1.inc"
#include      "task_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),IXC(NIXC,*),IXTG(NIXTG,*),NGROUC,IGROUC(*),
     .        ELCUTC(2,*),IADC_CRK(*),IEL_CRK(*),INOD_CRK(*),XEDGE4N(4,*),
     .        XEDGE3N(3,*),ADDCNE_CRK(*),KNOD2ELC(*),NODEDGE(2,*),
     .        CRKNODIAD(*),IAD_EDGE(*),FR_EDGE(*),FR_NBEDGE(*),NODLEVXF(*)
      my_real X(3,*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP)      :: ELBUF_TAB
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP,NXEL) :: XFEM_TAB
      TYPE (XFEM_EDGE_)   , DIMENSION(NXLAYMAX)    :: CRKEDGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,IG,NG,JFT,JLT,NEL,NF1,IXFEM,NLEV,N,ITG1,ITG2,FLAG,
     . SIZE,LSDRC,ICUT,IEDGE,ELCRK,ELCRKTG,ELCUT,ILEV,ILAY,NXLAY,PP1
      INTEGER ITSK,NODFTSK,NODLTSK,OMP_GET_THREAD_NUM
      my_real OFF
C=======================================================================
      NUMELCRK2 = NUMELCRK    !  pour check si crack avance dans le cycle (anim)
      ITG1 = 1+NUMELC
      ITG2 = 1+4*ECRKXFEC
C---
      NODFTSK   = 1
      NODLTSK   = NUMNOD
      CALL CRK_COORD_INI(ADDCNE_CRK,INOD_CRK,NODFTSK,NODLTSK,X,NODLEVXF)
c
      ! save nb elements connectes au noeud
      DO I=NODFTSK,NODLTSK
         N = INOD_CRK(I)
         IF (N > 0) THEN
           KNOD2ELC(N) = ADDCNE_CRK(N+1)-ADDCNE_CRK(N)
         ENDIF
      ENDDO
C---
      IF (NLEVSET == 0) RETURN   ! NUMCRACK = 0
C---
      ITG1 = 1+NUMELC
      ITG2 = 1+4*ECRKXFEC
c-----------------------------------------------------------------------------
C     initialize element enrichments
c-----------------------------------------------------------------------------
c-----
        DO IG = 1, NGROUC
          NG = IGROUC(IG)
          ITY   = IPARG(5,NG)
          OFF   = IPARG(8,NG)
          IXFEM = IPARG(54,NG)
          IF (OFF == 1)                CYCLE
          IF (IXFEM == 0)              CYCLE
          IF (ITY /= 3 .and. ITY /=7 ) CYCLE
          IF (IDDW > 0) CALL STARTIMEG(NG)
C---
          NXLAY = ELBUF_TAB(NG)%NLAY
          NEL   =IPARG(2,NG)  
          NFT   =IPARG(3,NG)  
          NPT   =IPARG(6,NG)  
          LFT   = 1 
          LLT   = MIN(NVSIZ,NEL)
          JFT   = LFT
          JLT   = LLT
C---
          IF (ITY == 3) THEN
            CALL ENRICHC_INI(ELBUF_TAB(NG)  ,XFEM_TAB(NG,1:NXEL),
     .           IXC        ,NFT       ,JFT       ,JLT      ,NXLAY   ,   
     .           IADC_CRK   ,IEL_CRK   ,INOD_CRK  ,ELCUTC   ,NODEDGE ,    
     .           CRKNODIAD  ,KNOD2ELC  ,X         ,CRKEDGE  ,XEDGE4N )         
C---
          ELSEIF (ITY == 7) THEN
            CALL ENRICHTG_INI(ELBUF_TAB(NG),
     .           IXTG          ,NFT          ,JFT     ,JLT           ,NXLAY   ,
     .           IADC_CRK(ITG2),IEL_CRK(ITG1),INOD_CRK,ELCUTC(1,ITG1),NODEDGE ,
     .           CRKNODIAD     ,KNOD2ELC     ,X       ,CRKEDGE       ,XEDGE3N )
          END IF
C---
          IF (IDDW > 0) CALL STOPTIMEG(NG)
        END DO
c-----------------------------------------------------------------------------
C     Tag cut edges : CRKEDGE(ILAY)%ICUTEDGE(IEDGE)
c-----------------------------------------------------------------------------
      DO IG = 1, NGROUC
        NG = IGROUC(IG)
        ITY   = IPARG(5,NG)
        OFF   = IPARG(8,NG)
        IXFEM = IPARG(54,NG)
        IF (OFF == 1)                GOTO 200
        IF (IXFEM == 0)              GOTO 200
        IF (ITY/=3 .AND. ITY/=7) GOTO 200
        IF (IDDW>0) CALL STARTIMEG(NG)
C---
        NEL     =IPARG(2,NG)
        NFT     =IPARG(3,NG)
        LFT  = 1 
        LLT  = MIN(NVSIZ,NEL)
        JFT = LFT
        JLT = LLT
C---
        IF (ITY == 3) THEN
          DO ILAY=1,NXLAY
            PP1 = (ILAY-1)*NXEL + 1
            DO I=JFT,JLT
              ELCRK = IEL_CRK(I+NFT)
              ELCUT = 0
              IF (ELCRK > 0) ELCUT = XFEM_PHANTOM(ILAY)%ELCUT(ELCRK)
              IF (ELCUT /= 0) THEN
                DO K=1,4
                  IEDGE = XEDGE4N(K,ELCRK)
                  ICUT = 0
                  IF (IEDGE > 0) ICUT = CRKEDGE(ILAY)%ICUTEDGE(IEDGE)
                  IF (ICUT == 2) CRKEDGE(ILAY)%ICUTEDGE(IEDGE) = 1
                ENDDO
              ENDIF
            ENDDO
          ENDDO
        ELSE IF (ITY == 7) THEN
          DO ILAY=1,NXLAY
            PP1 = (ILAY-1)*NXEL + 1
            DO I=JFT,JLT
              ELCRKTG = IEL_CRK(I+NFT+NUMELC)
              ELCRK   = ELCRKTG + ECRKXFEC
              ELCUT   = 0
              IF (ELCRK > 0) ELCUT = XFEM_PHANTOM(ILAY)%ELCUT(ELCRK)
              IF (ELCUT /= 0)THEN
                DO K=1,3
                  IEDGE = XEDGE3N(K,ELCRKTG)
                  ICUT = 0
                  IF (IEDGE > 0) ICUT = CRKEDGE(ILAY)%ICUTEDGE(IEDGE)
                  IF (ICUT == 2) CRKEDGE(ILAY)%ICUTEDGE(IEDGE) = 1
                ENDDO
              ENDIF
            ENDDO
          ENDDO
        END IF
C---
        IF (IDDW>0) CALL STOPTIMEG(NG)
 200  CONTINUE
      END DO
c----------------------
      IF (NSPMD > 1) THEN
        FLAG = 2
        SIZE  = NXLAY
        LSDRC = FR_NBEDGE(NSPMD+1)
        CALL SPMD_EXCH_IEDGE(IAD_EDGE,FR_EDGE,SIZE  ,LSDRC,FR_NBEDGE,
     .                       FLAG    ,CRKEDGE)
      END IF
C-----------
      RETURN
      END
