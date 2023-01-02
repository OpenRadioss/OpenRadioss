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
Chd|  STAT_BRICK_SPMD               source/output/sta/stat_brick_spmd.F
Chd|-- called by -----------
Chd|        GENSTAT                       source/output/sta/genstat.F   
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        SPMD_IGET_PARTN_STA           source/mpi/output/spmd_stat.F 
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE STAT_BRICK_SPMD(ITAB,IPART,IGEO,IXS,IPARTS,
     .                     IPART_STATE,NODTAG,STAT_INDXS,
     .                     IPARG, LENGS,IXS10,IXS16,IXS20,
     .                     ELBUF_TAB,IDEL )
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
#include      "scr05_c.inc"
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "spmd_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITAB(*), IPART(LIPART1,*),
     .        IGEO(NPROPGI,*), IXS(NIXS,*),
     .        IPARTS(*), IPART_STATE(*),
     .        NODTAG(*), STAT_INDXS(*),
     .        IPARG(NPARG,*),LENGS,IXS10(6,*),IXS16(8,*),
     .        IXS20(12,*),IDEL
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, JJ, IPRT, BUF, IPRT0, K, STAT_NUMELS_1
      INTEGER NG, NEL, NFT, LFT, LLT, ITY, ISOLNOD, IOFF
      INTEGER IADD(NPART+1), IADG(NSPMD,NPART)
      INTEGER NP(24*NUMELSG),
     .        NPGLOB(24*NUMELSG),
     .        WORK(70000),CLEF(2,NUMELSG)
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
C-----------------------------------------------
C     8 NODES BRICK
C-----------------------------------------------
      IADD = 0
      NPGLOB(1:24*NUMELSG) = 0
C
C en spmd il faut envoyer l'info meme qd on a 0 elem local
      JJ = 0
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
            NP(JJ+2) = ITAB(IXS(2,N))
            NP(JJ+3) = ITAB(IXS(4,N))
            NP(JJ+4) = ITAB(IXS(7,N))
            NP(JJ+5) = ITAB(IXS(6,N))
            IF(IXS10(1,N - NUMELS8) /= 0)THEN
              NP(JJ+6) = ITAB(IXS10(1,N - NUMELS8))
            ELSE
              NP(JJ+6) = 0
            ENDIF
            IF(IXS10(2,N - NUMELS8) /= 0)THEN
              NP(JJ+7) = ITAB(IXS10(2,N - NUMELS8))
            ELSE
              NP(JJ+7) = 0
            ENDIF
            IF(IXS10(3,N - NUMELS8) /= 0)THEN
              NP(JJ+8) = ITAB(IXS10(3,N - NUMELS8))
            ELSE
              NP(JJ+8) = 0
            ENDIF
            IF(IXS10(4,N - NUMELS8) /= 0)THEN
              NP(JJ+9) = ITAB(IXS10(4,N - NUMELS8))
            ELSE
              NP(JJ+9) = 0
            ENDIF
            IF(IXS10(5,N - NUMELS8) /= 0)THEN
              NP(JJ+10) = ITAB(IXS10(5,N - NUMELS8))
            ELSE
              NP(JJ+10) = 0
            ENDIF
            IF(IXS10(6,N - NUMELS8) /= 0)THEN
              NP(JJ+11) = ITAB(IXS10(6,N - NUMELS8))
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
            NP(JJ+2) = ITAB(IXS(2,N))
            NP(JJ+3) = ITAB(IXS(3,N))
            NP(JJ+4) = ITAB(IXS(4,N))
            NP(JJ+5) = ITAB(IXS(5,N))
            NP(JJ+6) = ITAB(IXS(6,N))
            NP(JJ+7) = ITAB(IXS(7,N))
            NP(JJ+8) = ITAB(IXS(8,N))
            NP(JJ+9) = ITAB(IXS(9,N))
            IF(IXS16(1,N - (NUMELS8+NUMELS10+NUMELS20)) /= 0)THEN
              NP(JJ+10) = ITAB(IXS16(1,N - (NUMELS8+NUMELS10+NUMELS20)))
            ELSE
              NP(JJ+10) = 0
            ENDIF
            IF(IXS16(2,N - (NUMELS8+NUMELS10+NUMELS20)) /= 0)THEN
              NP(JJ+11) = ITAB(IXS16(2,N - (NUMELS8+NUMELS10+NUMELS20)))
            ELSE
              NP(JJ+11) = 0
            ENDIF
            IF(IXS16(3,N - (NUMELS8+NUMELS10+NUMELS20)) /= 0)THEN
              NP(JJ+12) = ITAB(IXS16(3,N - (NUMELS8+NUMELS10+NUMELS20)))
            ELSE
              NP(JJ+12) = 0
            ENDIF
            IF(IXS16(4,N - (NUMELS8+NUMELS10+NUMELS20)) /= 0)THEN
              NP(JJ+13) = ITAB(IXS16(4,N - (NUMELS8+NUMELS10+NUMELS20)))
            ELSE
              NP(JJ+13) = 0
            ENDIF
            IF(IXS16(5,N - (NUMELS8+NUMELS10+NUMELS20)) /= 0)THEN
              NP(JJ+14) = ITAB(IXS16(5,N - (NUMELS8+NUMELS10+NUMELS20)))
            ELSE
              NP(JJ+14) = 0
            ENDIF
            IF(IXS16(6,N - (NUMELS8+NUMELS10+NUMELS20)) /= 0)THEN
              NP(JJ+15) = ITAB(IXS16(6,N - (NUMELS8+NUMELS10+NUMELS20)))
            ELSE
              NP(JJ+15) = 0
            ENDIF
            IF(IXS16(7,N - (NUMELS8+NUMELS10+NUMELS20)) /= 0)THEN
              NP(JJ+16) = ITAB(IXS16(7,N - (NUMELS8+NUMELS10+NUMELS20)))
            ELSE
              NP(JJ+16) = 0
            ENDIF
            IF(IXS16(8,N - (NUMELS8+NUMELS10+NUMELS20)) /= 0)THEN
              NP(JJ+17) = ITAB(IXS16(8,N - (NUMELS8+NUMELS10+NUMELS20)))
            ELSE
              NP(JJ+17) = 0
            ENDIF
            NP(JJ+18) = 0
            NP(JJ+19) = 0
            NP(JJ+20) = 0
            NP(JJ+21) = 0
          ELSEIF (ISOLNOD == 20)THEN
            NP(JJ+2) = ITAB(IXS(2,N))
            NP(JJ+3) = ITAB(IXS(3,N))
            NP(JJ+4) = ITAB(IXS(4,N))
            NP(JJ+5) = ITAB(IXS(5,N))
            NP(JJ+6) = ITAB(IXS(6,N))
            NP(JJ+7) = ITAB(IXS(7,N))
            NP(JJ+8) = ITAB(IXS(8,N))
            NP(JJ+9) = ITAB(IXS(9,N))
            IF(IXS20(1,N - (NUMELS8+NUMELS10)) /= 0)THEN
              NP(JJ+10) = ITAB(IXS20(1,N - (NUMELS8+NUMELS10)))
            ELSE
              NP(JJ+10) = 0
            ENDIF
            IF(IXS20(2,N - (NUMELS8+NUMELS10)) /= 0)THEN
              NP(JJ+11) = ITAB(IXS20(2,N - (NUMELS8+NUMELS10)))
            ELSE
              NP(JJ+11) = 0
            ENDIF
            IF(IXS20(3,N - (NUMELS8+NUMELS10)) /= 0)THEN
              NP(JJ+12) = ITAB(IXS20(3,N - (NUMELS8+NUMELS10)))
            ELSE
              NP(JJ+12) = 0
            ENDIF
            IF(IXS20(4,N - (NUMELS8+NUMELS10)) /= 0)THEN
              NP(JJ+13) = ITAB(IXS20(4,N - (NUMELS8+NUMELS10)))
            ELSE
              NP(JJ+13) = 0
            ENDIF
            IF(IXS20(5,N - (NUMELS8+NUMELS10)) /= 0)THEN
              NP(JJ+14) = ITAB(IXS20(5,N - (NUMELS8+NUMELS10)))
            ELSE
              NP(JJ+14) = 0
            ENDIF
            IF(IXS20(6,N - (NUMELS8+NUMELS10)) /= 0)THEN
              NP(JJ+15) = ITAB(IXS20(6,N - (NUMELS8+NUMELS10)))
            ELSE
              NP(JJ+15) = 0
            ENDIF
            IF(IXS20(7,N - (NUMELS8+NUMELS10)) /= 0)THEN
              NP(JJ+16) = ITAB(IXS20(7,N - (NUMELS8+NUMELS10)))
            ELSE
              NP(JJ+16) = 0
            ENDIF
            IF(IXS20(8,N - (NUMELS8+NUMELS10)) /= 0)THEN
              NP(JJ+17) = ITAB(IXS20(8,N - (NUMELS8+NUMELS10)))
            ELSE
              NP(JJ+17) = 0
            ENDIF
            IF(IXS20(9,N - (NUMELS8+NUMELS10)) /= 0)THEN
              NP(JJ+18) = ITAB(IXS20(9,N - (NUMELS8+NUMELS10)))
            ELSE
              NP(JJ+18) = 0
            ENDIF
            IF(IXS20(10,N - (NUMELS8+NUMELS10)) /= 0)THEN
              NP(JJ+19) = ITAB(IXS20(10,N - (NUMELS8+NUMELS10)))
            ELSE
              NP(JJ+19) = 0
            ENDIF
            IF(IXS20(11,N - (NUMELS8+NUMELS10)) /= 0)THEN
              NP(JJ+20) = ITAB(IXS20(11,N - (NUMELS8+NUMELS10)))
            ELSE
              NP(JJ+20) = 0
            ENDIF
            IF(IXS20(12,N - (NUMELS8+NUMELS10)) /= 0)THEN
              NP(JJ+21) = ITAB(IXS20(12,N - (NUMELS8+NUMELS10)))
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

          IF (ISOLNOD == 10)THEN
            NODTAG(IXS(2,N))=1
            NODTAG(IXS(4,N))=1
            NODTAG(IXS(7,N))=1
            NODTAG(IXS(6,N))=1
            IF (IXS10(1,N - NUMELS8) /= 0) NODTAG(IXS10(1,N - NUMELS8))=1
            IF (IXS10(2,N - NUMELS8) /= 0) NODTAG(IXS10(2,N - NUMELS8))=1
            IF (IXS10(3,N - NUMELS8) /= 0) NODTAG(IXS10(3,N - NUMELS8))=1
            IF (IXS10(4,N - NUMELS8) /= 0) NODTAG(IXS10(4,N - NUMELS8))=1
            IF (IXS10(5,N - NUMELS8) /= 0) NODTAG(IXS10(5,N - NUMELS8))=1
            IF (IXS10(6,N - NUMELS8) /= 0) NODTAG(IXS10(6,N - NUMELS8))=1
          ELSEIF (ISOLNOD == 16)THEN
            NODTAG(IXS(2,N))=1
            NODTAG(IXS(3,N))=1
            NODTAG(IXS(4,N))=1
            NODTAG(IXS(5,N))=1
            NODTAG(IXS(6,N))=1
            NODTAG(IXS(7,N))=1
            NODTAG(IXS(8,N))=1
            NODTAG(IXS(9,N))=1
            IF (IXS16(1,N - (NUMELS8+NUMELS10+NUMELS20)) /= 0) NODTAG(IXS16(1,N - (NUMELS8+NUMELS10+NUMELS20)))=1
            IF (IXS16(2,N - (NUMELS8+NUMELS10+NUMELS20)) /= 0) NODTAG(IXS16(2,N - (NUMELS8+NUMELS10+NUMELS20)))=1
            IF (IXS16(3,N - (NUMELS8+NUMELS10+NUMELS20)) /= 0) NODTAG(IXS16(3,N - (NUMELS8+NUMELS10+NUMELS20)))=1
            IF (IXS16(4,N - (NUMELS8+NUMELS10+NUMELS20)) /= 0) NODTAG(IXS16(4,N - (NUMELS8+NUMELS10+NUMELS20)))=1
            IF (IXS16(5,N - (NUMELS8+NUMELS10+NUMELS20)) /= 0) NODTAG(IXS16(5,N - (NUMELS8+NUMELS10+NUMELS20)))=1
            IF (IXS16(6,N - (NUMELS8+NUMELS10+NUMELS20)) /= 0) NODTAG(IXS16(6,N - (NUMELS8+NUMELS10+NUMELS20)))=1
            IF (IXS16(7,N - (NUMELS8+NUMELS10+NUMELS20)) /= 0) NODTAG(IXS16(7,N - (NUMELS8+NUMELS10+NUMELS20)))=1
            IF (IXS16(8,N - (NUMELS8+NUMELS10+NUMELS20)) /= 0) NODTAG(IXS16(8,N - (NUMELS8+NUMELS10+NUMELS20)))=1
          ELSEIF (ISOLNOD == 20)THEN
            NODTAG(IXS(2,N))=1
            NODTAG(IXS(3,N))=1
            NODTAG(IXS(4,N))=1
            NODTAG(IXS(5,N))=1
            NODTAG(IXS(6,N))=1
            NODTAG(IXS(7,N))=1
            NODTAG(IXS(8,N))=1
            NODTAG(IXS(9,N))=1
            IF (IXS20(1,N - (NUMELS8+NUMELS10)) /= 0) NODTAG(IXS20(1,N - (NUMELS8+NUMELS10)))=1
            IF (IXS20(2,N - (NUMELS8+NUMELS10)) /= 0) NODTAG(IXS20(2,N - (NUMELS8+NUMELS10)))=1
            IF (IXS20(3,N - (NUMELS8+NUMELS10)) /= 0) NODTAG(IXS20(3,N - (NUMELS8+NUMELS10)))=1
            IF (IXS20(4,N - (NUMELS8+NUMELS10)) /= 0) NODTAG(IXS20(4,N - (NUMELS8+NUMELS10)))=1
            IF (IXS20(5,N - (NUMELS8+NUMELS10)) /= 0) NODTAG(IXS20(5,N - (NUMELS8+NUMELS10)))=1
            IF (IXS20(6,N - (NUMELS8+NUMELS10)) /= 0) NODTAG(IXS20(6,N - (NUMELS8+NUMELS10)))=1
            IF (IXS20(7,N - (NUMELS8+NUMELS10)) /= 0) NODTAG(IXS20(7,N - (NUMELS8+NUMELS10)))=1
            IF (IXS20(8,N - (NUMELS8+NUMELS10)) /= 0) NODTAG(IXS20(8,N - (NUMELS8+NUMELS10)))=1
            IF (IXS20(9,N - (NUMELS8+NUMELS10)) /= 0) NODTAG(IXS20(9,N - (NUMELS8+NUMELS10)))=1
            IF (IXS20(10,N - (NUMELS8+NUMELS10)) /= 0) NODTAG(IXS20(10,N - (NUMELS8+NUMELS10)))=1
            IF (IXS20(11,N - (NUMELS8+NUMELS10)) /= 0) NODTAG(IXS20(11,N - (NUMELS8+NUMELS10)))=1
            IF (IXS20(12,N - (NUMELS8+NUMELS10)) /= 0) NODTAG(IXS20(12,N - (NUMELS8+NUMELS10)))=1
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
C-----
      STAT_NUMELS_G=0
      CALL SPMD_IGET_PARTN_STA(24,STAT_NUMELS,STAT_NUMELS_G,LENGS,NP,
     .  		       IADG,NPGLOB,STAT_INDXS)
C-----
      IF (ISPMD==0) THEN
        DO N=1,STAT_NUMELS_G
          STAT_INDXS(N)=N
          CLEF(1,N)=NPGLOB(19*(N-1)+22)
          CLEF(2,N)=NPGLOB(19*(N-1)+1)
        END DO
        CALL MY_ORDERS(0,WORK,CLEF,STAT_INDXS,STAT_NUMELS_G,2)

        IPRT0=0
        DO N=1,STAT_NUMELS_G
          K=STAT_INDXS(N)
          JJ=24*(K-1)
          IPRT=NPGLOB(JJ+22)
          IOFF=NPGLOB(JJ+24)
         IF (NPGLOB(JJ+23) == 4) THEN
          IF(IDEL==0.OR.(IDEL==1.AND.IOFF >= 1)) THEN
           IF(IPRT /= IPRT0)THEN
           WRITE(IUGEO,'(A,I10)')'/TETRA4/',IPART(4,IPRT)
           WRITE(IUGEO,'(A)')
     .      '# TETRA4ID      NOD1      NOD2      NOD3      NOD4'
            IPRT0=IPRT
           END IF
           WRITE(IUGEO,'(5I10)') NPGLOB(JJ+1),NPGLOB(JJ+2),NPGLOB(JJ+4),
     .                          NPGLOB(JJ+8),NPGLOB(JJ+6)
          ENDIF
         ELSEIF (NPGLOB(JJ+23) == 6) THEN
          IF(IDEL==0.OR.(IDEL==1.AND.IOFF >= 1)) THEN
           IF(IPRT /= IPRT0)THEN
             WRITE(IUGEO,'(A,I10)')'/PENTA6/',IPART(4,IPRT)
             WRITE(IUGEO,'(A)')
     .      '# PENTA6ID      NOD1      NOD2      NOD3      NOD4      NOD5      NOD6'
            IPRT0=IPRT
           END IF
           WRITE(IUGEO,'(7I10)') NPGLOB(JJ+1),NPGLOB(JJ+2),NPGLOB(JJ+3),
     .                          NPGLOB(JJ+4),NPGLOB(JJ+6),NPGLOB(JJ+7),
     .                          NPGLOB(JJ+8)
          ENDIF
         ELSEIF (NPGLOB(JJ+23) == 8) THEN
          IF(IDEL==0.OR.(IDEL==1.AND.IOFF >= 1)) THEN
           IF(IPRT /= IPRT0)THEN
            WRITE(IUGEO,'(A,I10)')'/BRICK/',IPART(4,IPRT)
            WRITE(IUGEO,'(A)')
     .      '#  BRICKID      NOD1      NOD2      NOD3      NOD4      NOD5      NOD6      NOD7      NOD8'
            IPRT0=IPRT
           END IF
           WRITE(IUGEO,'(9I10)') NPGLOB(JJ+1),NPGLOB(JJ+2),NPGLOB(JJ+3),
     .                          NPGLOB(JJ+4),NPGLOB(JJ+5),NPGLOB(JJ+6),
     .                          NPGLOB(JJ+7),NPGLOB(JJ+8),NPGLOB(JJ+9)
          ENDIF
         ELSEIF (NPGLOB(JJ+23) == 10) THEN
          IF(IDEL==0.OR.(IDEL==1.AND.IOFF >= 1)) THEN
           IF(IPRT /= IPRT0)THEN
            WRITE(IUGEO,'(A,I10)')'/TETRA10/',IPART(4,IPRT)
            WRITE(IUGEO,'(A)')
     .      '#TETRA10ID'
            WRITE(IUGEO,'(A)')
     .      '#     NOD1      NOD2      NOD3      NOD4      NOD5      NOD6      NOD7      NOD8      NOD9     NOD10'
            IPRT0=IPRT
           END IF
           WRITE(IUGEO,'(I10)') NPGLOB(JJ+1)
           WRITE(IUGEO,'(10I10)') NPGLOB(JJ+2),NPGLOB(JJ+3),NPGLOB(JJ+4),
     .                        NPGLOB(JJ+5),NPGLOB(JJ+6),NPGLOB(JJ+7),
     .                        NPGLOB(JJ+8),NPGLOB(JJ+9),NPGLOB(JJ+10),
     .                        NPGLOB(JJ+11)
          ENDIF
         ELSEIF (NPGLOB(JJ+23) == 16) THEN
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
           ENDIF
           WRITE(IUGEO,'(9I10)') NPGLOB(JJ+1),NPGLOB(JJ+2),NPGLOB(JJ+3),
     .                           NPGLOB(JJ+4),NPGLOB(JJ+5),NPGLOB(JJ+6),
     .                           NPGLOB(JJ+7), NPGLOB(JJ+8),NPGLOB(JJ+9)
          WRITE(IUGEO,'(4I10)') NPGLOB(JJ+10),NPGLOB(JJ+11),
     .                          NPGLOB(JJ+12), NPGLOB(JJ+13)
          WRITE(IUGEO,'(4I10)') NPGLOB(JJ+14),NPGLOB(JJ+15),
     .                          NPGLOB(JJ+16),NPGLOB(JJ+17)
          ENDIF
         ELSEIF (NPGLOB(JJ+23) == 20) THEN
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
           WRITE(IUGEO,'(9I10)') NPGLOB(JJ+1),NPGLOB(JJ+2),NPGLOB(JJ+3),
     .                        NPGLOB(JJ+4),NPGLOB(JJ+5),NPGLOB(JJ+6),
     .                        NPGLOB(JJ+7), NPGLOB(JJ+8),NPGLOB(JJ+9)
           WRITE(IUGEO,'(8I10)')NPGLOB(JJ+10),NPGLOB(JJ+11),
     .                         NPGLOB(JJ+12),NPGLOB(JJ+13),
     .                         NPGLOB(JJ+14),NPGLOB(JJ+15),
     .                         NPGLOB(JJ+16),NPGLOB(JJ+17)
           WRITE(IUGEO,'(4I10)')NPGLOB(JJ+18),NPGLOB(JJ+19),
     .                         NPGLOB(JJ+20),NPGLOB(JJ+21)
         ENDIF
         ENDIF
        END DO
      ENDIF
C-----------------------------------------------
      RETURN
      END
