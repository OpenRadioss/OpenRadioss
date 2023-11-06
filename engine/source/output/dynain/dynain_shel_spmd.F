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
Chd|  DYNAIN_SHEL_SPMD              source/output/dynain/dynain_shel_spmd.F
Chd|-- called by -----------
Chd|        GENDYNAIN                     source/output/dynain/gendynain.F
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        SPMD_IGET_PARTN_STA           source/mpi/output/spmd_stat.F
Chd|        SPMD_RGATHER9_DP              source/mpi/interfaces/spmd_outp.F
Chd|        STRS_TXT50                    source/output/sta/sta_txt.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        STATE_MOD                     ../common_source/modules/state_mod.F
Chd|====================================================================
      SUBROUTINE DYNAIN_SHEL_SPMD(ITAB   ,ITABG   ,LENG    ,IGEO   ,IXC    ,
     .                          IXTG   ,IPARTC  ,IPARTTG ,DYNAIN_DATA      ,
     .                          NODTAG ,DYNAIN_INDXC,DYNAIN_INDXTG,IPARG   ,
     .                          ELBUF_TAB,THKE  ,LENGC   ,LENGTG  ,IPART   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE ELBUFDEF_MOD
        USE STATE_MOD
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
        INTEGER ITAB(*), ITABG(*), LENG,
     .          IGEO(NPROPGI,*), IXC(NIXC,*), IXTG(NIXTG,*),
     .          IPARTC(*), IPARTTG(*),NODTAG(*),
     .          DYNAIN_INDXC(*), DYNAIN_INDXTG(*),
     .          LENGC, LENGTG, IPARG(NPARG,*),IPART(LIPART1,*)
        TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
        my_real
     .     THKE(*)
        TYPE (DYNAIN_DATABASE), INTENT(INOUT) :: DYNAIN_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, N, JJ, IPRT, BUF, IPRT0, K, II
        INTEGER NG, NEL, NFT, LFT, LLT, ITY, LEN, ITHK, MLW,IOFF,IPROP,
     .          ID_PROP, IERR, N4SHELL , N3SHELL ,IGTYP ,IGTYP0
        INTEGER IADD(NPART+1), IADG(NSPMD,NPART)
        INTEGER WORK(70000)
        INTEGER ,  DIMENSION(:),ALLOCATABLE :: NPC , NPTG ,NPGLOBC ,NPGLOBTG
        INTEGER ,  DIMENSION(:,:),ALLOCATABLE :: CLEF
        double precision  THKN ,BETA
        double precision ,  DIMENSION(:),ALLOCATABLE :: THKC, THKC0 , THKTG, THKTG0,
     .          BETAC, BETAC0, BETATG, BETATG0
        TYPE(G_BUFEL_)  ,POINTER :: GBUF
        CHARACTER*100 LINE
C--------------------------------------------------------

C-----------------------
C    Allocation Tabs
C-----------------------
        ALLOCATE(NPC(8*NUMELC),STAT=IERR)
        ALLOCATE(NPTG(7*NUMELTG),STAT=IERR)
        ALLOCATE(NPGLOBC(8*LENGC),STAT=IERR)
        ALLOCATE(NPGLOBTG(7*LENGTG),STAT=IERR)
        ALLOCATE(CLEF(2,MAX(NUMELCG,NUMELTGG)),STAT=IERR)
        ALLOCATE(THKC(MAX(1,NUMELC)),STAT=IERR)
        ALLOCATE(THKTG(MAX(1,NUMELTG)),STAT=IERR)
        ALLOCATE(THKC0(MAX(1,NUMELCG)),STAT=IERR)
        ALLOCATE(THKTG0(MAX(1,NUMELTGG)),STAT=IERR)
        ALLOCATE(BETAC(MAX(1,NUMELC)),STAT=IERR)
        ALLOCATE(BETATG(MAX(1,NUMELTG)),STAT=IERR)
        ALLOCATE(BETAC0(MAX(1,NUMELCG)),STAT=IERR)
        ALLOCATE(BETATG0(MAX(1,NUMELTGG)),STAT=IERR)
C-----------------------------------------------
C     4-NODE SHELLS
C-----------------------------------------------
        IADD = 0
        NPGLOBC(1:8*LENGC) = 0
        NPGLOBTG(1:7*LENGTG) = 0
C
C SPMD: Need to send infos even if 0 elems
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
            IPROP =IPARG(62,NG)
            ID_PROP=IGEO(1,IPROP)
            IGTYP= IPARG(38,NG)
            IF(IGTYP/= 1) IGTYP = 2
            LFT=1
            LLT=NEL
            DO I=LFT,LLT
              N  = I + NFT

              IPRT=IPARTC(N)
              IF(DYNAIN_DATA%IPART_DYNAIN(IPRT)==0)CYCLE

              NPC(JJ+1) = IXC(NIXC,N)
              NPC(JJ+2) = ITAB(IXC(2,N))
              NPC(JJ+3) = ITAB(IXC(3,N))
              NPC(JJ+4) = ITAB(IXC(4,N))
              NPC(JJ+5) = ITAB(IXC(5,N))
              NPC(JJ+6) = IPART(4,IPRT)
              NPC(JJ+7) = NINT(GBUF%OFF(I))
              NPC(JJ+8) = IGTYP
              II = II + 1
              IF (MLW /= 0 .AND. MLW /= 13) THEN
                IF (ITHK >0 ) THEN
                  THKC(II) = GBUF%THK(I)
                ELSE
                  THKC(II) = THKE(N)
                END IF
              ELSE
                THKC(II) = ZERO
              ENDIF
              JJ = JJ + 8

              DYNAIN_DATA%DYNAIN_NUMELC =DYNAIN_DATA%DYNAIN_NUMELC+1

              NODTAG(IXC(2,N))=1
              NODTAG(IXC(3,N))=1
              NODTAG(IXC(4,N))=1
              NODTAG(IXC(5,N))=1

              IF(IGTYP /= 1) THEN
                BETAC(II) = (HUNDRED80*ACOS(GBUF%BETAORTH(I)))/PI
              ENDIF

            END DO
          END IF
        END DO
C-----
        DYNAIN_DATA%DYNAIN_NUMELC_G=0
        CALL SPMD_IGET_PARTN_STA(8,DYNAIN_DATA%DYNAIN_NUMELC,DYNAIN_DATA%DYNAIN_NUMELC_G,LENGC,NPC,
     .    IADG,NPGLOBC,DYNAIN_INDXC)
        LEN = 0
        CALL SPMD_RGATHER9_DP(THKC,II,THKC0,DYNAIN_DATA%DYNAIN_NUMELC_G,LEN)
        LEN = 0
        CALL SPMD_RGATHER9_DP(BETAC,II,BETAC0,DYNAIN_DATA%DYNAIN_NUMELC_G,LEN)


C-----------------------------------------------
C     3-NODE SHELLS
C-----------------------------------------------
        IADD = 0
C
C SPMD: Need to send infos even if 0 elems
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
            IPROP =IPARG(62,NG)
            ID_PROP=IGEO(1,IPROP)
            IGTYP= IPARG(38,NG)
            IF(IGTYP/= 1) IGTYP = 2
            LFT=1
            LLT=NEL
C
            DO I=LFT,LLT
              N  = I + NFT

              IPRT=IPARTTG(N)
              IF(DYNAIN_DATA%IPART_DYNAIN(IPRT)==0)CYCLE

              NPTG(JJ+1) = IXTG(NIXTG,N)
              NPTG(JJ+2) = ITAB(IXTG(2,N))
              NPTG(JJ+3) = ITAB(IXTG(3,N))
              NPTG(JJ+4) = ITAB(IXTG(4,N))
              NPTG(JJ+5) = IPART(4,IPRT)
              NPTG(JJ+6) = NINT(GBUF%OFF(I))
              NPTG(JJ+7) = IGTYP
              II = II + 1
              IF (MLW /= 0 .AND. MLW /= 13) THEN
                IF (ITHK >0 ) THEN
                  THKTG(II) = GBUF%THK(I)
                ELSE
                  THKTG(II) = THKE(N)
                END IF
              ELSE
                THKTG(II) = ZERO
              ENDIF

              JJ = JJ + 7

              DYNAIN_DATA%DYNAIN_NUMELTG =DYNAIN_DATA%DYNAIN_NUMELTG+1

              NODTAG(IXTG(2,N))=1
              NODTAG(IXTG(3,N))=1
              NODTAG(IXTG(4,N))=1

              IF(IGTYP /= 1) THEN
                BETATG(II) = (HUNDRED80*ACOS(GBUF%BETAORTH(I)))/PI
              ENDIF

            END DO
          END IF
        END DO
C-----
        DYNAIN_DATA%DYNAIN_NUMELTG_G=0
        CALL SPMD_IGET_PARTN_STA(7,DYNAIN_DATA%DYNAIN_NUMELTG,DYNAIN_DATA%DYNAIN_NUMELTG_G,LENGTG,NPTG,
     .    IADG,NPGLOBTG,DYNAIN_INDXTG)
        LEN = 0
        CALL SPMD_RGATHER9_DP(THKTG,II,THKTG0,DYNAIN_DATA%DYNAIN_NUMELTG_G,LEN)
        CALL SPMD_RGATHER9_DP(BETATG,II,BETATG0,DYNAIN_DATA%DYNAIN_NUMELTG_G,LEN)


C-----------------------------------------------------------
C     Output
C------------------------------------------------------------

C---------Non Orthotropic elements ------------

C-----
        IF (ISPMD==0) THEN
C
          DO N=1,DYNAIN_DATA%DYNAIN_NUMELC_G
            DYNAIN_INDXC(N)=N
            CLEF(1,N)=NPGLOBC(8*(N-1)+8)
            CLEF(2,N)=NPGLOBC(8*(N-1)+1)
          END DO
          CALL MY_ORDERS(0,WORK,CLEF,DYNAIN_INDXC,DYNAIN_DATA%DYNAIN_NUMELC_G,2)
C
          DO N=1,DYNAIN_DATA%DYNAIN_NUMELTG_G
            DYNAIN_INDXTG(N)=N
            CLEF(1,N)=NPGLOBTG(7*(N-1)+7)
            CLEF(2,N)=NPGLOBTG(7*(N-1)+1)
          END DO
          CALL MY_ORDERS(0,WORK,CLEF,DYNAIN_INDXTG,DYNAIN_DATA%DYNAIN_NUMELTG_G,2)
C
C---------Non Orthotropic elements ------------
          IGTYP0 = 0
          DO N=1,DYNAIN_DATA%DYNAIN_NUMELC_G
            K=DYNAIN_INDXC(N)
            JJ=8*(K-1)
            IOFF=NPGLOBC(JJ+7)
            IGTYP = NPGLOBC(JJ+8)
            THKN = THKC0(K)
            IF(IOFF >= 1) THEN
              IF(IGTYP==1) THEN
                IF(IGTYP/=IGTYP0) THEN
                  IGTYP0 = IGTYP
                  IF(DYNAIN_DATA%ZIPDYNAIN==0) THEN
                    WRITE(IUDYNAIN,'(A)')'*ELEMENT_SHELL_THICKNESS'
                    WRITE(IUDYNAIN,'(A)')
     .               '$SHELLID PART_ID    NOD1    NOD2    NOD3    NOD4'
                    WRITE(IUDYNAIN,'(A)')
     .               '$          THIC1           THIC2           THIC3           THIC4'
                  ELSE
                    WRITE(LINE,'(A)') '*ELEMENT_SHELL_THICKNESS'
                    CALL STRS_TXT50(LINE,100)
                    WRITE(LINE,'(A)')
     .                '$SHELLID PART_ID    NOD1    NOD2    NOD3    NOD4'
                    CALL STRS_TXT50(LINE,100)
                    WRITE(LINE,'(A)')
     .                '$          THIC1           THIC2           THIC3           THIC4'
                    CALL STRS_TXT50(LINE,100)
                  ENDIF
                ENDIF

                IF(DYNAIN_DATA%ZIPDYNAIN==0) THEN
                  WRITE(IUDYNAIN,'(6I8)')
     .             NPGLOBC(JJ+1),NPGLOBC(JJ+6),
     .             NPGLOBC(JJ+2),NPGLOBC(JJ+3),NPGLOBC(JJ+4),NPGLOBC(JJ+5)
                  WRITE(IUDYNAIN,'(1P4G16.9)')
     .              THKN,THKN,THKN,THKN
                ELSE
                  WRITE(LINE,'(6I8)')
     .             NPGLOBC(JJ+1),NPGLOBC(JJ+6),
     .             NPGLOBC(JJ+2),NPGLOBC(JJ+3),NPGLOBC(JJ+4),NPGLOBC(JJ+5)
                  CALL STRS_TXT50(LINE,100)
                  WRITE(LINE,'(1P4G16.9)')
     .              THKN,THKN,THKN,THKN
                  CALL STRS_TXT50(LINE,100)
                ENDIF
              ELSE
                EXIT
              ENDIF
            ENDIF
          END DO

          N4SHELL = N

C-----
          DO N=1,DYNAIN_DATA%DYNAIN_NUMELTG_G
            K=DYNAIN_INDXTG(N)
            JJ=7*(K-1)
            IOFF=NPGLOBTG(JJ+6)
            IGTYP = NPGLOBTG(JJ+7)
            THKN = THKTG0(K)
            IF(IOFF >= 1) THEN
              IF(IGTYP==1) THEN
                IF(DYNAIN_DATA%ZIPDYNAIN==0) THEN
                  WRITE(IUDYNAIN,'(5I8)')
     .            NPGLOBTG(JJ+1),NPGLOBTG(JJ+5),
     .            NPGLOBTG(JJ+2),NPGLOBTG(JJ+3),NPGLOBTG(JJ+4)
                  WRITE(IUDYNAIN,'(1P3G16.9)')
     .               THKN,THKN,THKN
                ELSE
                  WRITE(LINE,'(5I8)')
     .            NPGLOBTG(JJ+1),NPGLOBTG(JJ+5),
     .            NPGLOBTG(JJ+2),NPGLOBTG(JJ+3),NPGLOBTG(JJ+4)
                  CALL STRS_TXT50(LINE,100)
                  WRITE(LINE,'(1P3G16.9)')
     .              THKN,THKN,THKN
                  CALL STRS_TXT50(LINE,100)
                ENDIF
              ELSE
                EXIT
              ENDIF
            ENDIF
          END DO

          N3SHELL = N

C--------- Orthotropic elements ------------


          IGTYP0 = 1
          DO N=N4SHELL,DYNAIN_DATA%DYNAIN_NUMELC_G
            K=DYNAIN_INDXC(N)
            JJ=8*(K-1)
            IOFF=NPGLOBC(JJ+7)
            IGTYP = NPGLOBC(JJ+8)
            THKN = THKC0(K)
            BETA = BETAC0(K)
            IF(IOFF >= 1) THEN

              IF(IGTYP/=IGTYP0) THEN
                IGTYP0 = IGTYP
                IF(DYNAIN_DATA%ZIPDYNAIN==0) THEN
                  WRITE(IUDYNAIN,'(A)')'*ELEMENT_SHELL_THICKNESS_BETA'
                  WRITE(IUDYNAIN,'(A)')
     .              '$SHELLID PART_ID    NOD1    NOD2    NOD3    NOD4'
                  WRITE(IUDYNAIN,'(A)')
     .              '$          THIC1           THIC2           THIC3           THIC4            BETA'
                ELSE
                  WRITE(LINE,'(A)') '*ELEMENT_SHELL_THICKNESS_BETA'
                  CALL STRS_TXT50(LINE,100)
                  WRITE(LINE,'(A)')
     .              '$SHELLID PART_ID    NOD1    NOD2    NOD3    NOD4'
                  CALL STRS_TXT50(LINE,100)
                  WRITE(LINE,'(A)')
     .             '$          THIC1           THIC2           THIC3           THIC4            BETA'
                  CALL STRS_TXT50(LINE,100)
                ENDIF

              ENDIF

              IF(DYNAIN_DATA%ZIPDYNAIN==0) THEN
                WRITE(IUDYNAIN,'(6I8)')
     .           NPGLOBC(JJ+1),NPGLOBC(JJ+6),
     .           NPGLOBC(JJ+2),NPGLOBC(JJ+3),NPGLOBC(JJ+4),NPGLOBC(JJ+5)
                WRITE(IUDYNAIN,'(1P5G16.9)')
     .           THKN,THKN,THKN,THKN,BETA
              ELSE
                WRITE(LINE,'(6I8)')
     .           NPGLOBC(JJ+1),NPGLOBC(JJ+6),
     .           NPGLOBC(JJ+2),NPGLOBC(JJ+3),NPGLOBC(JJ+4),NPGLOBC(JJ+5)
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(1P5G16.9)')
     .            THKN,THKN,THKN,THKN,BETA
                CALL STRS_TXT50(LINE,100)
              ENDIF

            ENDIF
          END DO

          DO N=N3SHELL,DYNAIN_DATA%DYNAIN_NUMELTG
            K=DYNAIN_INDXTG(N)
            JJ=7*(K-1)
            IOFF=NPGLOBTG(JJ+6)
            THKN = THKTG0(K)
            BETA = BETATG0(K)
            IF(IOFF >= 1) THEN
              IF(DYNAIN_DATA%ZIPDYNAIN==0) THEN
                WRITE(IUDYNAIN,'(5I8)')
     .           NPGLOBTG(JJ+1),NPGLOBTG(JJ+5),
     .           NPGLOBTG(JJ+2),NPGLOBTG(JJ+3),NPGLOBTG(JJ+4)
                WRITE(IUDYNAIN,'(1P3G16.9,16X,1PG16.9)')
     .             THKN,THKN,THKN,BETA
              ELSE
                WRITE(LINE,'(5I8)')
     .           NPGLOBTG(JJ+1),NPGLOBTG(JJ+5),
     .           NPGLOBTG(JJ+2),NPGLOBTG(JJ+3),NPGLOBTG(JJ+4)
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(1P3G16.9,16X,1PG16.9)')
     .            THKN,THKN,THKN,BETA
                CALL STRS_TXT50(LINE,100)
              ENDIF
            ENDIF
          END DO


        ENDIF

C-----------------------
C    DEAllocation Tabs
C-----------------------
        DEALLOCATE(NPC,NPTG,NPGLOBC,NPGLOBTG,CLEF,THKC,THKTG,THKC0,THKTG0,BETAC,BETATG,BETAC0,BETATG0)
C-----------------------------------------------

        RETURN
      END
