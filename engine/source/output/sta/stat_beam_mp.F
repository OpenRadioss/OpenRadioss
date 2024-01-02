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
Chd|====================================================================
Chd|  STAT_BEAM_MP                  source/output/sta/stat_beam_mp.F
Chd|-- called by -----------
Chd|        GENSTAT                       source/output/sta/genstat.F   
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE STAT_BEAM_MP(ITAB   ,IPART     ,IXP   ,IPARTP   ,IPART_STATE,
     .                        NODTAG ,STAT_INDXP,IPARG ,ELBUF_TAB,IDEL       )
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
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITAB(*),IPART(LIPART1,*),IXP(NIXP,*),IPARTP(*),
     .        IPART_STATE(*),NODTAG(*),STAT_INDXP(*),IPARG(NPARG,*)
      INTEGER IDEL
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,JJ,IPRT0,IPRT,K,II
      INTEGER NG,NEL,NFT,LFT,LLT,ITY,IOFF
      INTEGER NP(6*NUMELP),WORK(70000),CLEF(2,NUMELP)
C
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
C-----------------------------------------------
C     BEAM
C-----------------------------------------------
      JJ = 0
      II = 0
      IF (NUMELP /= 0) THEN
c
        DO NG=1,NGROUP
          ITY = IPARG(5,NG)
          IF (ITY == 5) THEN
            NEL = IPARG(2,NG)
            NFT = IPARG(3,NG)
            GBUF => ELBUF_TAB(NG)%GBUF   
            LFT=1
            LLT=NEL
c
            DO I=LFT,LLT
              N  = I + NFT
              IPRT=IPARTP(N)
              IF (IPART_STATE(IPRT) /= 0) THEN
c
                NP(JJ+1) = IXP(NIXP,N)
                NP(JJ+2) = ITAB(IXP(2,N))
                NP(JJ+3) = ITAB(IXP(3,N))
                NP(JJ+4) = ITAB(IXP(4,N))
                NP(JJ+5) = IPRT
                NP(JJ+6) = IABS(NINT(GBUF%OFF(I)))
c
                II = II + 1
c
                JJ = JJ + 6
c
                STAT_NUMELP =STAT_NUMELP+1
                CLEF(1,STAT_NUMELP)=IPRT
                CLEF(2,STAT_NUMELP)=IXP(NIXP,N)
c
                NODTAG(IXP(2,N))=1
                NODTAG(IXP(3,N))=1
                NODTAG(IXP(4,N))=1
              ENDIF ! IF (IPART_STATE(IPRT) /= 0)
            ENDDO ! DO I=LFT,LLT
          ENDIF ! IF (ITY == 5)
        ENDDO ! DO NG=1,NGROUP
      ENDIF ! IF (NUMELP /= 0)
C----
      DO N=1,STAT_NUMELP
        STAT_INDXP(N)=N
      ENDDO
      CALL MY_ORDERS(0,WORK,CLEF,STAT_INDXP,STAT_NUMELP,2)
C----
      IPRT0=0
      DO N=1,STAT_NUMELP
        K=STAT_INDXP(N)
        JJ=6*(K-1)
        IPRT=NP(JJ+5)
        IOFF=NP(JJ+6)
        IF (IDEL==0 .OR. (IDEL==1 .AND. IOFF >= 1)) THEN
          IF (IPRT /= IPRT0) THEN
            WRITE(IUGEO,'(A,I10)')'/BEAM/',IPART(4,IPRT)
            WRITE(IUGEO,'(A)')
     .    '#BEAM_ID        NOD1      NOD2      NOD3'
            IPRT0=IPRT
          ENDIF
          WRITE(IUGEO,'(4I10)') NP(JJ+1),NP(JJ+2),NP(JJ+3),NP(JJ+4)
        ENDIF !IF (IDEL)
      ENDDO ! DO N=1,STAT_NUMELP
C----
C-----------------------------------------------
      RETURN
      END
