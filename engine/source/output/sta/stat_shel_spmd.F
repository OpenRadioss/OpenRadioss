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
Chd|  STAT_SHEL_SPMD                source/output/sta/stat_shel_spmd.F
Chd|-- called by -----------
Chd|        GENSTAT                       source/output/sta/genstat.F   
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        SPMD_IGET_PARTN_STA           source/mpi/output/spmd_stat.F 
Chd|        SPMD_RGATHER9_DP              source/mpi/interfaces/spmd_outp.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE STAT_SHEL_SPMD(ITAB,ITABG,LENG,IPART,IGEO,
     .            IXC,IXTG,IPARTC,IPARTTG,IPART_STATE,
     .            NODTAG,STAT_INDXC,STAT_INDXTG,LENGC,LENGTG,
     .            IPARG ,ELBUF_TAB,THKE,SIZP0,IDEL)
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
      INTEGER ITAB(*), ITABG(*), LENG, IPART(LIPART1,*),
     .        IGEO(NPROPGI,*), IXC(NIXC,*), IXTG(NIXTG,*),
     .        IPARTC(*), IPARTTG(*), IPART_STATE(*),
     .        NODTAG(*), STAT_INDXC(*), STAT_INDXTG(*),
     .        LENGC, LENGTG, IPARG(NPARG,*),SIZP0,IDEL
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      my_real
     .   THKE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, JJ, IPRT, BUF, IPRT0, K, II
      INTEGER NG, NEL, NFT, LFT, LLT, ITY, LEN, ITHK, MLW,IOFF
      INTEGER IADD(NPART+1), IADG(NSPMD,NPART)
      INTEGER NP(MAX(7*NUMELC,6*NUMELTG)),
     .        NPGLOB(MAX(7*LENGC,6*LENGTG)),
     .        WORK(70000),CLEF(2,MAX(NUMELCG,NUMELTGG))
      double precision   
     .   THK(MAX(1,MAX(NUMELC,NUMELTG))),THK0(MAX(1,MAX(NUMELCG,NUMELTGG)))
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
C-----------------------------------------------
C     4-NODE SHELLS
C-----------------------------------------------
      IADD = 0
      NPGLOB(1:MAX(7*LENGC,6*LENGTG)) = 0
C
C en spmd il faut envoyer l'info meme qd on a 0 elem local
      JJ = 0
      II = 0
      DO NG=1,NGROUP
       ITY   =IPARG(5,NG)
       IF(ITY==3) THEN
         NEL   =IPARG(2,NG)
         NFT   =IPARG(3,NG)
         GBUF => ELBUF_TAB(NG)%GBUF   
         MLW   =IPARG(1,NG)
         ITHK  =IPARG(28,NG)
         LFT=1
         LLT=NEL
         DO I=LFT,LLT
          N  = I + NFT

          IPRT=IPARTC(N)
          IF(IPART_STATE(IPRT)==0)CYCLE

          NP(JJ+1) = IXC(NIXC,N)
          NP(JJ+2) = ITAB(IXC(2,N))
          NP(JJ+3) = ITAB(IXC(3,N))
          NP(JJ+4) = ITAB(IXC(4,N))
          NP(JJ+5) = ITAB(IXC(5,N))
          NP(JJ+6) = IPRT
          NP(JJ+7) = IABS(NINT(GBUF%OFF(I)))
          II = II + 1
          IF (MLW /= 0 .AND. MLW /= 13) THEN
            IF (ITHK >0 ) THEN
                THK(II) = GBUF%THK(I)
            ELSE
                THK(II) = THKE(N)
            END IF
          ELSE
            THK(II) = ZERO
          ENDIF
          JJ = JJ + 7

          STAT_NUMELC =STAT_NUMELC+1

          NODTAG(IXC(2,N))=1
          NODTAG(IXC(3,N))=1
          NODTAG(IXC(4,N))=1
          NODTAG(IXC(5,N))=1
         END DO
       END IF
      END DO
C-----
      STAT_NUMELC_G=0
      CALL SPMD_IGET_PARTN_STA(7,STAT_NUMELC,STAT_NUMELC_G,LENGC,NP,
     .  		       IADG,NPGLOB,STAT_INDXC)
      LEN = 0
      CALL SPMD_RGATHER9_DP(THK,II,THK0,SIZP0,LEN)

C-----
      IF (ISPMD==0) THEN
        DO N=1,STAT_NUMELC_G
          STAT_INDXC(N)=N
          CLEF(1,N)=NPGLOB(7*(N-1)+7)
          CLEF(2,N)=NPGLOB(7*(N-1)+1)
        END DO
        CALL MY_ORDERS(0,WORK,CLEF,STAT_INDXC,STAT_NUMELC_G,2)

        IPRT0=0
        DO N=1,STAT_NUMELC_G
          K=STAT_INDXC(N)
          JJ=7*(K-1)
          IPRT=NPGLOB(JJ+6)
          IOFF=NPGLOB(JJ+7)
          IF(IDEL==0.OR.(IDEL==1.AND.IOFF >= 1)) THEN
           IF(IPRT /= IPRT0)THEN
            WRITE(IUGEO,'(A,I10)')'/SHELL/',IPART(4,IPRT)
            WRITE(IUGEO,'(A)')
     .      '#  SHELLID      NOD1      NOD2      NOD3      NOD4                                THK'
            IPRT0=IPRT
           END IF
           WRITE(IUGEO,'(5I10,30X,1PE20.13)')
     .     NPGLOB(JJ+1),
     .     NPGLOB(JJ+2),NPGLOB(JJ+3),NPGLOB(JJ+4),NPGLOB(JJ+5),THK0(K)
          ENDIF
        END DO
      ENDIF
C-----------------------------------------------
C     3-NODE SHELLS
C-----------------------------------------------
      IADD = 0
C
C en spmd il faut envoyer l'info meme qd on a 0 elem local
      JJ = 0
      II = 0
      DO NG=1,NGROUP
       ITY   =IPARG(5,NG)
       IF(ITY==7) THEN
         NEL   =IPARG(2,NG)
         NFT   =IPARG(3,NG)
         GBUF => ELBUF_TAB(NG)%GBUF   
         MLW   =IPARG(1,NG)
         ITHK  =IPARG(28,NG)
         LFT=1
         LLT=NEL
C
         DO I=LFT,LLT
          N  = I + NFT

          IPRT=IPARTTG(N)
          IF(IPART_STATE(IPRT)==0)CYCLE

          NP(JJ+1) = IXTG(NIXTG,N)
          NP(JJ+2) = ITAB(IXTG(2,N))
          NP(JJ+3) = ITAB(IXTG(3,N))
          NP(JJ+4) = ITAB(IXTG(4,N))
          NP(JJ+5) = IPRT
          NP(JJ+6) = IABS(NINT(GBUF%OFF(I)))
          II = II + 1
          IF (MLW /= 0 .AND. MLW /= 13) THEN
            IF (ITHK >0 ) THEN
                THK(II) = GBUF%THK(I)
            ELSE
                THK(II) = THKE(N)
            END IF
          ELSE
            THK(II) = ZERO
          ENDIF

          JJ = JJ + 6

          STAT_NUMELTG =STAT_NUMELTG+1

          NODTAG(IXTG(2,N))=1
          NODTAG(IXTG(3,N))=1
          NODTAG(IXTG(4,N))=1
         END DO
       END IF
      END DO
C-----
      STAT_NUMELTG_G=0
      CALL SPMD_IGET_PARTN_STA(6,STAT_NUMELTG,STAT_NUMELTG_G,LENGTG,NP,
     .  		       IADG,NPGLOB,STAT_INDXTG)
      LEN = 0
      CALL SPMD_RGATHER9_DP(THK,II,THK0,SIZP0,LEN)
C-----
      IF (ISPMD==0) THEN
        DO N=1,STAT_NUMELTG_G
          STAT_INDXTG(N)=N
          CLEF(1,N)=NPGLOB(6*(N-1)+6)
          CLEF(2,N)=NPGLOB(6*(N-1)+1)
        END DO
        CALL MY_ORDERS(0,WORK,CLEF,STAT_INDXTG,STAT_NUMELTG_G,2)

        IPRT0=0
        DO N=1,STAT_NUMELTG_G
          K=STAT_INDXTG(N)
          JJ=6*(K-1)
          IPRT=NPGLOB(JJ+5)
          IOFF=NPGLOB(JJ+6)
          IF(IDEL==0.OR.(IDEL==1.AND.IOFF >= 1)) THEN
           IF(IPRT /= IPRT0)THEN
            WRITE(IUGEO,'(A,I10)')'/SH3N/',IPART(4,IPRT)
            WRITE(IUGEO,'(A)')
     .      '#   SH3NID      NOD1      NOD2      NOD3                                      THK'
            IPRT0=IPRT
           END IF
           WRITE(IUGEO,'(4I10,40X,1PE20.13)')
     .     NPGLOB(JJ+1),
     .     NPGLOB(JJ+2),NPGLOB(JJ+3),NPGLOB(JJ+4),THK0(K)
          ENDIF
        END DO
      ENDIF
C-----------------------------------------------
      RETURN
      END
