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
!||    stat_shel_spmd        ../engine/source/output/sta/stat_shel_spmd.F
!||--- called by ------------------------------------------------------
!||    genstat               ../engine/source/output/sta/genstat.F
!||--- calls      -----------------------------------------------------
!||    my_orders             ../common_source/tools/sort/my_orders.c
!||    spmd_iget_partn_sta   ../engine/source/mpi/output/spmd_stat.F
!||    spmd_rgather9_dp      ../engine/source/mpi/interfaces/spmd_outp.F
!||--- uses       -----------------------------------------------------
!||    elbufdef_mod          ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    message_mod           ../engine/share/message_module/message_mod.F
!||    my_alloc_mod          ../common_source/tools/memory/my_alloc.F90
!||====================================================================
      SUBROUTINE STAT_SHEL_SPMD(ITAB,ITABG,LENG,IPART,IGEO,
     .            IXC,IXTG,IPARTC,IPARTTG,IPART_STATE,
     .            NODTAG,STAT_INDXC,STAT_INDXTG,LENGC,LENGTG,
     .            IPARG ,ELBUF_TAB,THKE,IDEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE MESSAGE_MOD
      USE MY_ALLOC_MOD
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
     .        LENGC, LENGTG, IPARG(NPARG,*),IDEL
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      my_real
     .   THKE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, JJ, IPRT, BUF, IPRT0, K, II
      INTEGER NG, NEL, NFT, LFT, LLT, ITY, LEN, ITHK, MLW,IOFF
      INTEGER WORK(70000)
      INTEGER THK_LEN,THK0_LEN
      INTEGER,DIMENSION(:),ALLOCATABLE :: IADD
      INTEGER,DIMENSION(:,:),ALLOCATABLE :: IADG
      INTEGER,DIMENSION(:),ALLOCATABLE :: NP
      INTEGER,DIMENSION(:),ALLOCATABLE :: NPGLOB
      INTEGER,DIMENSION(:,:),ALLOCATABLE :: CLEF
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: THK    ! Gather Thickness from Shell & Triangle elements
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: THK0   ! ISPMD=0 : Shell & Triangle Thicknesses after gather.
      TYPE(G_BUFEL_)  ,POINTER :: GBUF   
C-----------------------------------------------
      CALL MY_ALLOC(NP,MAX(7*NUMELC,6*NUMELTG))
      CALL MY_ALLOC(NPGLOB,MAX(7*LENGC,6*LENGTG))
      CALL MY_ALLOC(CLEF,2,MAX(NUMELCG,NUMELTGG))
      CALL MY_ALLOC(IADG,NSPMD,NPART)
      CALL MY_ALLOC(IADD,NPART+1)
C-----------------------------------------------
C INITIALIZATION
C-----------------------------------------------
      THK_LEN = MAX(1,MAX(NUMELC,NUMELTG))
      ALLOCATE(THK(THK_LEN))
      IF (ISPMD == 0) THEN
            THK0_LEN = MAX(1,MAX(NUMELCG,NUMELTGG))
      ELSE
            THK0_LEN=1
      ENDIF
      ALLOCATE(THK0(THK0_LEN))
C-----------------------------------------------
C     4-NODE SHELLS
C-----------------------------------------------
      IADD = 0
      NPGLOB(1:MAX(7*LENGC,6*LENGTG)) = 0
C
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
     .             IADG,NPGLOB,STAT_INDXC)
      LEN = 0
      CALL SPMD_RGATHER9_DP(THK,II,THK0,THK0_LEN,LEN)

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
     .             IADG,NPGLOB,STAT_INDXTG)
      LEN = 0
      CALL SPMD_RGATHER9_DP(THK,II,THK0,THK0_LEN,LEN)
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

      DEALLOCATE(THK)
      DEALLOCATE(THK0)
      DEALLOCATE(NP)
      DEALLOCATE(NPGLOB)
      DEALLOCATE(CLEF)
      DEALLOCATE(IADG)
      DEALLOCATE(IADD)
C-----------------------------------------------
      RETURN
      END
