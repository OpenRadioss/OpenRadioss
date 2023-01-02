Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  C_CRKEDGE                     source/restart/ddsplit/c_crkedge.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|====================================================================
      SUBROUTINE C_CRKEDGE(IELC_L   ,IELTG_L ,IEDGECRK_L,NUMEDGES  ,
     .                     IEDGESH4 ,IEDGESH3,CEP       ,P         ,
     .                     IBORDEDGE,IBORDEDGE_L,NUMEDGES_L,NODEDGE,
     .                     NODEDGE_L,NODLOCAL,IEDGESH4_L,IEDGESH3_L,
     .                     IEL_CRK  ,IEDGE_L ,IEDGE     ,ECRKXFEC  ,
     .                     EDGELOCAL,NBDDEDGE_L,IEDGE_TMP,EDGEGLOBAL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------     
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL    
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IELC_L(*),IELTG_L(*),IEDGECRK_L(*),NUMEDGES,
     .        IEDGESH4(4,*),IEDGESH3(3,*),CEP(*),P,IBORDEDGE(*),
     .        IBORDEDGE_L(*),NUMEDGES_L,NODEDGE(2,*),NODEDGE_L(2,*),
     .        NODLOCAL(*),IEDGESH4_L(4,*),IEDGESH3_L(3,*),IEL_CRK(*),
     .        IEDGE_L(*),IEDGE(*),ECRKXFEC,EDGELOCAL(*),
     .        NBDDEDGE_L,IEDGE_TMP(3,*),EDGEGLOBAL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,OFFC,OFFTG,IED_L,IED_GL,NEXT,IEL_L,IC1,IC2,JCRK,
     . IEL_L1,PROC,EDGEPROC
      INTEGER,DIMENSION(:),ALLOCATABLE :: ITAG_EDGE
C=======================================================================
      OFFC = NUMELS + NUMELQ
      OFFTG = OFFC + NUMELC + NUMELT + NUMELP + NUMELR
C
      ALLOCATE(ITAG_EDGE(NUMEDGES))
      ITAG_EDGE = 0
C
      NEXT = 0
      IEL_L = 0
      IED_L = 0
      IEL_L1 = 0
C---
C  tag local edges
C---
C
C shell 4N
C
      DO I=1,NUMELC
        IF (CEP(I+OFFC) == P) THEN
          IEL_L1 = IEL_L1 + 1
          IF (IEL_CRK(I) > 0) THEN
            IEL_L = IEL_L + 1
CC            JCRK = IELC_L(IEL_L1)
            JCRK = IEL_CRK(I)
            DO K=1,4  ! edges
              IED_GL = IEDGESH4(K,JCRK)   ! Id global des edges dans element xfem
              IF (IED_GL > 0) THEN
                IF (ITAG_EDGE(IED_GL) == 0) THEN
                  IED_L = IED_L + 1
C                 local boundary edge
                  IBORDEDGE_L(IED_L) = IBORDEDGE(IED_GL)
                  EDGEGLOBAL(IED_L) = IED_GL
                  EDGELOCAL(IED_GL) = IED_L
                  ITAG_EDGE(IED_GL) = 1
C                 local edge nodes
                  IC1 = NODEDGE(1,IED_GL)
                  IC2 = NODEDGE(2,IED_GL)
                  IC1 = NODLOCAL(IC1)
                  IC2 = NODLOCAL(IC2)
                  NODEDGE_L(1,IED_L) = IC1
                  NODEDGE_L(2,IED_L) = IC2
C                  IEDGE_L(IED_L) = IED_GL
                  IEDGE_L(IED_L) = IEDGE(IED_GL)
                ENDIF
C               local element edges
                IEDGESH4_L(K,IEL_L) = EDGELOCAL(IED_GL)
C for crk ply:   comptage des edges locales pour 1 ply
                IEDGECRK_L(K + NEXT) = EDGELOCAL(IED_GL)
              ENDIF
            ENDDO
            NEXT = NEXT + 4
          ENDIF
        ENDIF
      ENDDO
C
C shell 3N
C
      IEL_L = 0
      IEL_L1 = 0
      DO I=1,NUMELTG
        IF(CEP(I+OFFTG) == P)THEN
          IEL_L1 = IEL_L1 + 1
          IF(IEL_CRK(I+NUMELC) > 0)THEN
            IEL_L = IEL_L + 1
CC           JCRK = IELTG_L(IEL_L1)
            JCRK = IEL_CRK(I+NUMELC) - ECRKXFEC
            DO K=1,3   ! edges
              IED_GL = IEDGESH3(K,JCRK)
              IF(IED_GL /= 0)THEN
                IF(ITAG_EDGE(IED_GL) == 0)THEN
                  IED_L = IED_L + 1
C local boundary edge
                  IBORDEDGE_L(IED_L) = IBORDEDGE(IED_GL)
                  EDGEGLOBAL(IED_L) = IED_GL
                  EDGELOCAL(IED_GL) = IED_L
                  ITAG_EDGE(IED_GL) = 1
C local nodal edges
                  IC1 = NODEDGE(1,IED_GL)
                  IC2 = NODEDGE(2,IED_GL)
                  IC1 = NODLOCAL(IC1)
                  IC2 = NODLOCAL(IC2)
                  NODEDGE_L(1,IED_L) = IC1
                  NODEDGE_L(2,IED_L) = IC2
C                  IEDGE_L(IED_L) = IED_GL
                  IEDGE_L(IED_L) = IEDGE(IED_GL)
                ENDIF
C local element edges
                IEDGESH3_L(K,IEL_L) = EDGELOCAL(IED_GL)
C for crk ply:
                IEDGECRK_L(K + NEXT) = EDGELOCAL(IED_GL)
              ENDIF
            ENDDO
            NEXT = NEXT + 3
          ENDIF
        ENDIF
      ENDDO
C---
c      NUMEDGES_L = IED
C---
C---
C  count front local edges  (front entre les procs) => edges d'echange
C---
      PROC = P+1
      DO IED_GL=1,NUMEDGES
cc       IF(EDGELOCAL(IED_GL) > 0.AND.IEDGE_TMP(1,IED_GL) > 1)THEN
       IF (IEDGE_TMP(3,IED_GL) < 0) THEN   ! edge interieur
        IC1 = NODEDGE(1,IED_GL)
        IC2 = NODEDGE(2,IED_GL)
c       NLOCAL = 1 => noeud sur frontiere entre des procs
        IF((NLOCAL(IC1,PROC)==1).AND.
     .     (NLOCAL(IC2,PROC)==1))THEN
          DO I = 1,NSPMD
            EDGEPROC = 0
            IF(I/=PROC)THEN
              IF(NLOCAL(IC1,I)==1.AND.
     .           NLOCAL(IC2,I)==1)THEN   ! arrete commune antre deux procs
                 NBDDEDGE_L = NBDDEDGE_L + 1
C---
                 IF(IEDGE_TMP(2,IED_GL) == 0)THEN
                   EDGEPROC = EDGEPROC + 1
                   IEDGE_TMP(1,IED_GL) = EDGEPROC   ! num local de l'arrete sur chaque proc
                   IEDGE_TMP(2,IED_GL) = 1          ! flag pour ne pas repasser cette arrete
c                  IEDGE_TMP(3,I) = IEDGE_TMP0(I)   ! flag bord libre / interieur
                 ENDIF
C---
              ENDIF
            ENDIF
          ENDDO
        END IF
       ENDIF
      ENDDO
C---
      DEALLOCATE(ITAG_EDGE)
C---
      RETURN
      END
