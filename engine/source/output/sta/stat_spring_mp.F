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
Chd|  STAT_SPRING_MP                source/output/sta/stat_spring_mp.F
Chd|-- called by -----------
Chd|        GENSTAT                       source/output/sta/genstat.F   
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE STAT_SPRING_MP(ITAB   ,IPART     ,IXR   ,IPARTR   ,IPART_STATE,
     .                          NODTAG ,STAT_INDXR,IPARG ,ELBUF_TAB,IDEL       )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
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
#include      "param_c.inc"
#include      "remesh_c.inc"
#include      "scr05_c.inc"
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "spmd_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITAB(*),IPART(LIPART1,*),IXR(NIXR,*),IPARTR(*),
     .        IPART_STATE(*),NODTAG(*),STAT_INDXR(*),IPARG(NPARG,*)
      INTEGER IDEL
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,JJ,IPRT0,IPRT,K,II
      INTEGER NG,NEL,NFT,LFT,LLT,ITY,IOFF
      INTEGER NP(5*NUMELR),WORK(70000),CLEF(2,NUMELR)
C
      TYPE(G_BUFEL_)  ,POINTER :: GBUF
C-----------------------------------------------
C     SPRING
C-----------------------------------------------
      JJ = 0
      II = 0
      IF (NUMELR /= 0) THEN
c
        DO NG=1,NGROUP
          ITY = IPARG(5,NG)
          IF (ITY == 6) THEN
            NEL = IPARG(2,NG)
            NFT = IPARG(3,NG)
            GBUF => ELBUF_TAB(NG)%GBUF
            LFT=1
            LLT=NEL
c
            DO I=LFT,LLT
              N  = I + NFT
              IPRT=IPARTR(N)
              IF (IPART_STATE(IPRT) == 0) CYCLE
c
              NP(JJ+1) = IXR(NIXR,N)
              NP(JJ+2) = ITAB(IXR(2,N))
              NP(JJ+3) = ITAB(IXR(3,N))
              NP(JJ+4) = IPRT
              NP(JJ+5) = IABS(NINT(GBUF%OFF(I)))
c
              II = II + 1
c
              JJ = JJ + 5
c
              STAT_NUMELR =STAT_NUMELR+1
              CLEF(1,STAT_NUMELR)=IPRT
              CLEF(2,STAT_NUMELR)=IXR(NIXR,N)
c
              NODTAG(IXR(2,N))=1
              NODTAG(IXR(3,N))=1
            ENDDO ! DO I=LFT,LLT
          ENDIF ! IF (ITY == 6)
        ENDDO ! DO NG=1,NGROUP
      ENDIF ! IF (NUMELR /= 0)
C----
      DO N=1,STAT_NUMELR
        STAT_INDXR(N)=N
      ENDDO
      CALL MY_ORDERS(0,WORK,CLEF,STAT_INDXR,STAT_NUMELR,2)
C----
      IPRT0=0
      DO N=1,STAT_NUMELR
        K=STAT_INDXR(N)
        JJ=5*(K-1)
        IPRT=NP(JJ+4)
        IOFF=NP(JJ+5)
        IF (IDEL==0 .OR. (IDEL==1 .AND. IOFF >= 1)) THEN
          IF (IPRT /= IPRT0) THEN
            WRITE(IUGEO,'(A,I10)')'/SPRING/',IPART(4,IPRT)
            WRITE(IUGEO,'(A)')
     .    '#SPRING_ID      NOD1      NOD2'
            IPRT0=IPRT
          ENDIF
          WRITE(IUGEO,'(3I10)') NP(JJ+1),NP(JJ+2),NP(JJ+3)
        ENDIF !IF (IDEL)
      ENDDO ! DO N=1,STAT_NUMELR
C----
C-----------------------------------------------
      RETURN
      END
