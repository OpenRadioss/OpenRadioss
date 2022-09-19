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
Chd|  STAT_BRICK_MP                 source/output/sta/stat_brick_mp.F
Chd|-- called by -----------
Chd|        GENSTAT                       source/output/sta/genstat.F   
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE STAT_BRICK_MP(ITAB,IPART,IGEO,IXS,IPARTS,
     .                        IPART_STATE,NODTAG,STAT_INDXS,
     .                        IPARG ,IXS10,IXS16,IXS20,ELBUF_TAB,
     .                        IDEL)
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
      INTEGER ITAB(*), IPART(LIPART1,*),IPARTS(*),
     .        IGEO(NPROPGI,*), IXS(NIXS,*), IPART_STATE(*),
     .        NODTAG(*), STAT_INDXS(*),
     .        IPARG(NPARG,*),IXS10(6,*),IXS16(8,*),IXS20(12,*),
     .        IDEL
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, JJ, IPRT0, IPRT, K, STAT_NUMELS_1,N10,N20,N16
      INTEGER NG, NEL, NFT, LFT, LLT, ITY, ISOLNOD, IOFF
      INTEGER NP(24*NUMELS),
     .        WORK(70000),CLEF(2,NUMELS)
      TYPE(G_BUFEL_)  ,POINTER :: GBUF
C-----------------------------------------------
C     8 NODES BRICK
C-----------------------------------------------
      JJ = 0
      IF(NUMELS/=0)THEN

       DO NG=1,NGROUP
        ITY   =IPARG(5,NG)
        ISOLNOD = IPARG(28,NG)
        NEL   =IPARG(2,NG)
        NFT   =IPARG(3,NG)
        GBUF => ELBUF_TAB(NG)%GBUF
        LFT=1
        LLT=NEL
        IF(ITY == 1) THEN
          DO I=LFT,LLT
            N  = I + NFT
            IPRT=IPARTS(N)
            IF(IPART_STATE(IPRT)==0)CYCLE

            NP(JJ+1) = IXS(NIXS,N)
            IF (ISOLNOD == 10)THEN
	      N10 = N - NUMELS8
              NP(JJ+2) = ITAB(IXS(2,N))
              NP(JJ+3) = ITAB(IXS(4,N))
              NP(JJ+4) = ITAB(IXS(7,N))
              NP(JJ+5) = ITAB(IXS(6,N))
              IF(IXS10(1,N10) /= 0)THEN
                NP(JJ+6) = ITAB(IXS10(1,N10))
              ELSE
                NP(JJ+6) = 0
              ENDIF
              IF(IXS10(2,N10) /= 0)THEN
                NP(JJ+7) = ITAB(IXS10(2,N10))
              ELSE
                NP(JJ+7) = 0
              ENDIF
              IF(IXS10(3,N10) /= 0)THEN
                NP(JJ+8) = ITAB(IXS10(3,N10))
              ELSE
                NP(JJ+8) = 0
              ENDIF
              IF(IXS10(4,N10) /= 0)THEN
                NP(JJ+9) = ITAB(IXS10(4,N10))
              ELSE
                NP(JJ+9) = 0
              ENDIF
              IF(IXS10(5,N10) /= 0)THEN
                NP(JJ+10) = ITAB(IXS10(5,N10))
              ELSE
                NP(JJ+10) = 0
              ENDIF
              IF(IXS10(6,N10) /= 0)THEN
                NP(JJ+11) = ITAB(IXS10(6,N10))
              ELSE
                NP(JJ+11) = 0
              ENDIF
              NP(JJ+12) = 0
              NP(JJ+13) = 0
              NP(JJ+14) = 0
              NP(JJ+15) = 0
              NP(JJ+16) = 0
              NP(JJ+17) = 0
              NP(JJ+18) = 0
              NP(JJ+19) = 0
              NP(JJ+20) = 0
              NP(JJ+21) = 0
            ELSEIF (ISOLNOD == 16)THEN
	      N16 = N - (NUMELS8+NUMELS10+NUMELS20)
              NP(JJ+2) = ITAB(IXS(2,N))
              NP(JJ+3) = ITAB(IXS(3,N))
              NP(JJ+4) = ITAB(IXS(4,N))
              NP(JJ+5) = ITAB(IXS(5,N))
              NP(JJ+6) = ITAB(IXS(6,N))
              NP(JJ+7) = ITAB(IXS(7,N))
              NP(JJ+8) = ITAB(IXS(8,N))
              NP(JJ+9) = ITAB(IXS(9,N))
              IF(IXS16(1,N16) /= 0)THEN
                NP(JJ+10) = ITAB(IXS16(1,N16))
              ELSE
                NP(JJ+10) = 0
              ENDIF
              IF(IXS16(2,N16) /= 0)THEN
                NP(JJ+11) = ITAB(IXS16(2,N16))
              ELSE
                NP(JJ+11) = 0
              ENDIF
              IF(IXS16(3,N16) /= 0)THEN
                NP(JJ+12) = ITAB(IXS16(3,N16))
              ELSE
                NP(JJ+12) = 0
              ENDIF
              IF(IXS16(4,N16) /= 0)THEN
                NP(JJ+13) = ITAB(IXS16(4,N16))
              ELSE
                NP(JJ+13) = 0
              ENDIF
              IF(IXS16(5,N16) /= 0)THEN
                NP(JJ+14) = ITAB(IXS16(5,N16))
              ELSE
                NP(JJ+14) = 0
              ENDIF
              IF(IXS16(6,N16) /= 0)THEN
                NP(JJ+15) = ITAB(IXS16(6,N16))
              ELSE
                NP(JJ+15) = 0
              ENDIF
              IF(IXS16(7,N16) /= 0)THEN
                NP(JJ+16) = ITAB(IXS16(7,N16))
              ELSE
                NP(JJ+16) = 0
              ENDIF
              IF(IXS16(8,N16) /= 0)THEN
                NP(JJ+17) = ITAB(IXS16(8,N16))
              ELSE
                NP(JJ+17) = 0
              ENDIF
              NP(JJ+18) = 0
              NP(JJ+19) = 0
              NP(JJ+20) = 0
              NP(JJ+21) = 0
            ELSEIF (ISOLNOD == 20)THEN
	      N20 = N - (NUMELS8+NUMELS10)
              NP(JJ+2) = ITAB(IXS(2,N))
              NP(JJ+3) = ITAB(IXS(3,N))
              NP(JJ+4) = ITAB(IXS(4,N))
              NP(JJ+5) = ITAB(IXS(5,N))
              NP(JJ+6) = ITAB(IXS(6,N))
              NP(JJ+7) = ITAB(IXS(7,N))
              NP(JJ+8) = ITAB(IXS(8,N))
              NP(JJ+9) = ITAB(IXS(9,N))
              IF(IXS20(1,N20) /= 0)THEN
                NP(JJ+10) = ITAB(IXS20(1,N20))
              ELSE
                NP(JJ+10) = 0
              ENDIF
              IF(IXS20(2,N20) /= 0)THEN
                NP(JJ+11) = ITAB(IXS20(2,N20))
              ELSE
                NP(JJ+11) = 0
              ENDIF
              IF(IXS20(3,N20) /= 0)THEN
                NP(JJ+12) = ITAB(IXS20(3,N20))
              ELSE
                NP(JJ+12) = 0
              ENDIF
              IF(IXS20(4,N20) /= 0)THEN
                NP(JJ+13) = ITAB(IXS20(4,N20))
              ELSE
                NP(JJ+13) = 0
              ENDIF
              IF(IXS20(5,N20) /= 0)THEN
                NP(JJ+14) = ITAB(IXS20(5,N20))
              ELSE
                NP(JJ+14) = 0
              ENDIF
              IF(IXS20(6,N20) /= 0)THEN
                NP(JJ+15) = ITAB(IXS20(6,N20))
              ELSE
                NP(JJ+15) = 0
              ENDIF
              IF(IXS20(7,N20) /= 0)THEN
                NP(JJ+16) = ITAB(IXS20(7,N20))
              ELSE
                NP(JJ+16) = 0
              ENDIF
              IF(IXS20(8,N20) /= 0)THEN
                NP(JJ+17) = ITAB(IXS20(8,N20))
              ELSE
                NP(JJ+17) = 0
              ENDIF
              IF(IXS20(9,N20) /= 0)THEN
                NP(JJ+18) = ITAB(IXS20(9,N20))
              ELSE
                NP(JJ+18) = 0
              ENDIF
              IF(IXS20(10,N20) /= 0)THEN
                NP(JJ+19) = ITAB(IXS20(10,N20))
              ELSE
                NP(JJ+19) = 0
              ENDIF
              IF(IXS20(11,N20) /= 0)THEN
                NP(JJ+20) = ITAB(IXS20(11,N20))
              ELSE
                NP(JJ+20) = 0
              ENDIF
              IF(IXS20(12,N20) /= 0)THEN
                NP(JJ+21) = ITAB(IXS20(12,N20))
              ELSE
                NP(JJ+21) = 0
              ENDIF
            ELSE
              NP(JJ+2) = ITAB(IXS(2,N))
              NP(JJ+3) = ITAB(IXS(3,N))
              NP(JJ+4) = ITAB(IXS(4,N))
              NP(JJ+5) = ITAB(IXS(5,N))
              NP(JJ+6) = ITAB(IXS(6,N))
              NP(JJ+7) = ITAB(IXS(7,N))
              NP(JJ+8) = ITAB(IXS(8,N))
              NP(JJ+9) = ITAB(IXS(9,N))
              NP(JJ+10) = 0
              NP(JJ+11) = 0
              NP(JJ+12) = 0
              NP(JJ+13) = 0
              NP(JJ+14) = 0
              NP(JJ+15) = 0
              NP(JJ+16) = 0
              NP(JJ+17) = 0
              NP(JJ+18) = 0
              NP(JJ+19) = 0
              NP(JJ+20) = 0
              NP(JJ+21) = 0
            ENDIF
            NP(JJ+22) = IPRT
            NP(JJ+23) = ISOLNOD
            NP(JJ+24) = IABS(NINT(GBUF%OFF(I)))
            JJ = JJ + 24

            STAT_NUMELS =STAT_NUMELS+1
            CLEF(1,STAT_NUMELS)=IPRT
            CLEF(2,STAT_NUMELS)=IXS(NIXS,N)

            IF (ISOLNOD == 10)THEN
              NODTAG(IXS(2,N))=1
              NODTAG(IXS(4,N))=1
              NODTAG(IXS(7,N))=1
              NODTAG(IXS(6,N))=1
              IF (IXS10(1,N10) /= 0) NODTAG(IXS10(1,N10))=1
              IF (IXS10(2,N10) /= 0) NODTAG(IXS10(2,N10))=1
              IF (IXS10(3,N10) /= 0) NODTAG(IXS10(3,N10))=1
              IF (IXS10(4,N10) /= 0) NODTAG(IXS10(4,N10))=1
              IF (IXS10(5,N10) /= 0) NODTAG(IXS10(5,N10))=1
              IF (IXS10(6,N10) /= 0) NODTAG(IXS10(6,N10))=1
            ELSEIF (ISOLNOD == 16)THEN
              NODTAG(IXS(2,N))=1
              NODTAG(IXS(3,N))=1
              NODTAG(IXS(4,N))=1
              NODTAG(IXS(5,N))=1
              NODTAG(IXS(6,N))=1
              NODTAG(IXS(7,N))=1
              NODTAG(IXS(8,N))=1
              NODTAG(IXS(9,N))=1
              IF (IXS16(1,N16) /= 0) NODTAG(IXS16(1,N16))=1
              IF (IXS16(2,N16) /= 0) NODTAG(IXS16(2,N16))=1
              IF (IXS16(3,N16) /= 0) NODTAG(IXS16(3,N16))=1
              IF (IXS16(4,N16) /= 0) NODTAG(IXS16(4,N16))=1
              IF (IXS16(5,N16) /= 0) NODTAG(IXS16(5,N16))=1
              IF (IXS16(6,N16) /= 0) NODTAG(IXS16(6,N16))=1
              IF (IXS16(7,N16) /= 0) NODTAG(IXS16(7,N16))=1
              IF (IXS16(8,N16) /= 0) NODTAG(IXS16(8,N16))=1
            ELSEIF (ISOLNOD == 20)THEN
              NODTAG(IXS(2,N))=1
              NODTAG(IXS(3,N))=1
              NODTAG(IXS(4,N))=1
              NODTAG(IXS(5,N))=1
              NODTAG(IXS(6,N))=1
              NODTAG(IXS(7,N))=1
              NODTAG(IXS(8,N))=1
              NODTAG(IXS(9,N))=1
              IF (IXS20(1,N20) /= 0) NODTAG(IXS20(1,N20))=1
              IF (IXS20(2,N20) /= 0) NODTAG(IXS20(2,N20))=1
              IF (IXS20(3,N20) /= 0) NODTAG(IXS20(3,N20))=1
              IF (IXS20(4,N20) /= 0) NODTAG(IXS20(4,N20))=1
              IF (IXS20(5,N20) /= 0) NODTAG(IXS20(5,N20))=1
              IF (IXS20(6,N20) /= 0) NODTAG(IXS20(6,N20))=1
              IF (IXS20(7,N20) /= 0) NODTAG(IXS20(7,N20))=1
              IF (IXS20(8,N20) /= 0) NODTAG(IXS20(8,N20))=1
              IF (IXS20(9,N20) /= 0) NODTAG(IXS20(9,N20))=1
              IF (IXS20(10,N20) /= 0) NODTAG(IXS20(10,N20))=1
              IF (IXS20(11,N20) /= 0) NODTAG(IXS20(11,N20))=1
              IF (IXS20(12,N20) /= 0) NODTAG(IXS20(12,N20))=1
            ELSE
              NODTAG(IXS(2,N))=1
              NODTAG(IXS(3,N))=1
              NODTAG(IXS(4,N))=1
              NODTAG(IXS(5,N))=1
              NODTAG(IXS(6,N))=1
              NODTAG(IXS(7,N))=1
              NODTAG(IXS(8,N))=1
              NODTAG(IXS(9,N))=1
            ENDIF
          END DO
        END IF
      END DO
C----
      DO N=1,STAT_NUMELS
        STAT_INDXS(N)=N
      END DO
      CALL MY_ORDERS(0,WORK,CLEF,STAT_INDXS,STAT_NUMELS,2)
C----
      IPRT0=0
      DO N=1,STAT_NUMELS
        K=STAT_INDXS(N)
        JJ=24*(K-1)
        IPRT=NP(JJ+22)
        IOFF=NP(JJ+24)
       IF (NP(JJ+23) == 4) THEN
        IF(IDEL==0.OR.(IDEL==1.AND.IOFF >= 1)) THEN
         IF(IPRT /= IPRT0)THEN
          WRITE(IUGEO,'(A,I10)')'/TETRA4/',IPART(4,IPRT)
          WRITE(IUGEO,'(A)')
     .      '# TETRA4ID      NOD1      NOD2      NOD3      NOD4'
            IPRT0=IPRT
         END IF
          WRITE(IUGEO,'(5I10)') NP(JJ+1),NP(JJ+2),NP(JJ+4),
     .                          NP(JJ+8),NP(JJ+6)
        ENDIF
       ELSEIF (NP(JJ+23) == 6) THEN
        IF(IDEL==0.OR.(IDEL==1.AND.IOFF >= 1)) THEN
         IF(IPRT /= IPRT0)THEN
          WRITE(IUGEO,'(A,I10)')'/PENTA6/',IPART(4,IPRT)
          WRITE(IUGEO,'(A)')
     .      '# PENTA6ID      NOD1      NOD2      NOD3      NOD4      NOD5      NOD6'
            IPRT0=IPRT
         END IF
          WRITE(IUGEO,'(7I10)') NP(JJ+1),NP(JJ+2),NP(JJ+3),
     .                          NP(JJ+4),NP(JJ+6),NP(JJ+7),
     .                          NP(JJ+8)
        ENDIF
       ELSEIF (NP(JJ+23) == 8) THEN
        IF(IDEL==0.OR.(IDEL==1.AND.IOFF >= 1)) THEN
         IF(IPRT /= IPRT0)THEN
          WRITE(IUGEO,'(A,I10)')'/BRICK/',IPART(4,IPRT)
          WRITE(IUGEO,'(A)')
     .      '#  BRICKID      NOD1      NOD2      NOD3      NOD4      NOD5      NOD6      NOD7      NOD8'
          IPRT0=IPRT
         END IF
         WRITE(IUGEO,'(9I10)') NP(JJ+1),NP(JJ+2),NP(JJ+3),
     .                        NP(JJ+4),NP(JJ+5),NP(JJ+6),
     .                        NP(JJ+7),NP(JJ+8),NP(JJ+9)
        ENDIF
       ELSEIF (NP(JJ+23) == 10) THEN
        IF(IDEL==0.OR.(IDEL==1.AND.IOFF >= 1)) THEN
         IF(IPRT /= IPRT0)THEN
            WRITE(IUGEO,'(A,I10)')'/TETRA10/',IPART(4,IPRT)
            WRITE(IUGEO,'(A)')
     .      '#TETRA10ID'
            WRITE(IUGEO,'(A)')
     .      '#     NOD1      NOD2      NOD3      NOD4      NOD5      NOD6      NOD7      NOD8      NOD9     NOD10'
            IPRT0=IPRT
         END IF
         WRITE(IUGEO,'(I10)') NP(JJ+1)
         WRITE(IUGEO,'(10I10)') NP(JJ+2),NP(JJ+3),NP(JJ+4),
     .                        NP(JJ+5),NP(JJ+6),NP(JJ+7),
     .                        NP(JJ+8),NP(JJ+9),NP(JJ+10),
     .                        NP(JJ+11)
        ENDIF
       ELSEIF (NP(JJ+23) == 16) THEN
        IF(IDEL==0.OR.(IDEL==1.AND.IOFF >= 1)) THEN
         IF(IPRT /= IPRT0)THEN
            WRITE(IUGEO,'(A,I10)')'/SHEL16/',IPART(4,IPRT)
            WRITE(IUGEO,'(A)')
     .      '#TSHEL16ID      NOD1      NOD2      NOD3      NOD4      NOD5      NOD6      NOD7      NOD8'
            WRITE(IUGEO,'(A)')
     .      '#     NOD9     NOD10     NOD11     NOD12'
            WRITE(IUGEO,'(A)')
     .      '#    NOD13     NOD14     NOD15     NOD16'
            IPRT0=IPRT
         END IF
         WRITE(IUGEO,'(9I10)') NP(JJ+1),NP(JJ+2),NP(JJ+3),NP(JJ+4),
     .                        NP(JJ+5),NP(JJ+6),NP(JJ+7),
     .                        NP(JJ+8),NP(JJ+9)
         WRITE(IUGEO,'(4I10)') NP(JJ+10),NP(JJ+11),NP(JJ+12),
     .                        NP(JJ+13)
         WRITE(IUGEO,'(4I10)') NP(JJ+14),NP(JJ+15),NP(JJ+16),
     .                        NP(JJ+17)
        ENDIF
       ELSEIF (NP(JJ+23) == 20) THEN
        IF(IDEL==0.OR.(IDEL==1.AND.IOFF >= 1)) THEN
         IF(IPRT /= IPRT0)THEN
            WRITE(IUGEO,'(A,I10)')'/BRIC20/',IPART(4,IPRT)
            WRITE(IUGEO,'(A)')
     .      '#BRICK20ID      NOD1      NOD2      NOD3      NOD4      NOD5      NOD6      NOD7      NOD8'
            WRITE(IUGEO,'(A)')
     .      '#     NOD9     NOD10     NOD11     NOD12     NOD13     NOD14     NOD15     NOD16'
            WRITE(IUGEO,'(A)')
     .      '#    NOD17     NOD18     NOD19     NOD20'
            IPRT0=IPRT
         END IF
         WRITE(IUGEO,'(9I10)') NP(JJ+1),NP(JJ+2),NP(JJ+3),NP(JJ+4),
     .                        NP(JJ+5),NP(JJ+6),NP(JJ+7),
     .                        NP(JJ+8),NP(JJ+9)
          WRITE(IUGEO,'(8I10)') NP(JJ+10),NP(JJ+11),NP(JJ+12),
     .                        NP(JJ+13),NP(JJ+14),NP(JJ+15),
     .                        NP(JJ+16),NP(JJ+17)
         WRITE(IUGEO,'(4I10)') NP(JJ+18),NP(JJ+19),NP(JJ+20),
     .                        NP(JJ+21)
        ENDIF
       ENDIF
      END DO
      ENDIF
C-----------------------------------------------
      RETURN
      END
