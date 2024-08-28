!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
!Copyright>
!Copyright>        This program is free software: you can redistribute it and/or modify
!Copyright>        it under the terms of the GNU Affero General Public License as published by
!Copyright>        the Free Software Foundation, either version 3 of the License, or
!Copyright>        (at your option) any later version.
!Copyright>
!Copyright>        This program is distributed in the hope that it will be useful,
!Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
!Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!Copyright>        GNU Affero General Public License for more details.
!Copyright>
!Copyright>        You should have received a copy of the GNU Affero General Public License
!Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
!Copyright>
!Copyright>
!Copyright>        Commercial Alternative: Altair Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
!Copyright>        software under a commercial license.  Contact Altair to discuss further if the
!Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
      !||====================================================================
      !||    inter7_collision_detection_mod   ../engine/source/interfaces/intsort/inter7_collision_detection.F90
      !||--- called by ------------------------------------------------------
      !||    inter_sort_07                    ../engine/source/interfaces/int07/inter_sort_07.F
      !||====================================================================
      MODULE INTER7_COLLISION_DETECTION_MOD
      CONTAINS
      !||====================================================================
      !||    inter7_collision_detection   ../engine/source/interfaces/intsort/inter7_collision_detection.F90
      !||--- called by ------------------------------------------------------
      !||    inter_sort_07                ../engine/source/interfaces/int07/inter_sort_07.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                       ../engine/source/output/message/message.F
      !||    arret                        ../engine/source/system/arret.F
      !||    inter7_candidate_pairs       ../engine/source/interfaces/intsort/inter7_candidate_pairs.F90
      !||    my_barrier                   ../engine/source/system/machine.F
      !||    spmd_oldnumcd                ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod                 ../common_source/modules/constant_mod.F
      !||    inter7_candidate_pairs_mod   ../engine/source/interfaces/intsort/inter7_candidate_pairs.F90
      !||    message_mod                  ../engine/share/message_module/message_mod.F
      !||    tri7box                      ../engine/share/modules/tri7box.F
      !||====================================================================
        SUBROUTINE INTER7_COLLISION_DETECTION(&
        &X        ,IRECT   ,NSV     ,INACTI   ,CAND_P  ,&
        &NMN      ,NRTM    ,NSN     ,CAND_E   ,CAND_N  ,&
        &GAP      ,NOINT   ,II_STOK ,NCONTACT ,BMINMA  ,&
        &TZINF    ,CAND_A,CURV_MAX, RENUM_SIZ,&
        &NB_N_B   ,ESHIFT  ,ILD     ,IFQ      ,IFPEN   ,&
        &STFN     ,STF     ,IGAP     ,GAP_S   ,&
        &NSNR     ,NCONT   ,RENUM   ,NSNROLD  ,GAP_M   ,&
        &GAPMIN   ,GAPMAX  ,NUM_IMP,GAP_S_L ,&
        &GAP_M_L  ,ITASK   ,BGAPSMX  ,I_MEM   ,&
        &KREMNOD  ,REMNOD  ,FLAGREMNODE, DRAD ,&
        &ITIED    ,CAND_F  ,DGAPLOAD,&
        &TOTAL_NB_NRTM, s_cand_a,&
        &S_KREMNOD, S_REMNOD, NSPMD, NUMNOD)

!============================================================================
!   M o d u l e s
!-----------------------------------------------
          USE TRI7BOX
          USE INTER7_CANDIDATE_PAIRS_MOD
          USE MESSAGE_MOD
          USE CONSTANT_MOD , ONLY : THREE_OVER_4, THIRD
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
          implicit none
#include "my_real.inc"
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
          INTEGER :: NMN
          INTEGER :: NSN
          INTEGER :: NOINT
          INTEGER :: INACTI
          INTEGER :: IFQ
          INTEGER :: NSNR
          INTEGER :: NSNROLD
          INTEGER :: RENUM_SIZ
          INTEGER, INTENT(IN) :: NUMNOD
          INTEGER, INTENT(in) :: NRTM !< number of segments per threads
          INTEGER, INTENT(in) :: TOTAL_NB_NRTM !< total number of segments
          INTEGER, INTENT(in) :: s_cand_a
          INTEGER :: IRECT(4,NRTM)
          INTEGER :: NSV(NSN)
          INTEGER :: CAND_A(s_cand_a)
          INTEGER :: RENUM(RENUM_SIZ)
          INTEGER :: NUM_IMP
          INTEGER :: ITASK
          INTEGER :: CAND_E(NCONTACT)
          INTEGER :: CAND_N(NCONTACT)
          INTEGER :: IFPEN(NCONTACT)
          INTEGER :: KREMNOD(*)
          INTEGER :: REMNOD(*)
          INTEGER :: NCONTACT
          INTEGER :: ESHIFT
          INTEGER :: ILD
          INTEGER :: NB_N_B
          INTEGER :: IGAP
          INTEGER :: NCONT
          INTEGER :: I_MEM
          INTEGER :: II_STOK
          INTEGER :: FLAGREMNODE
          INTEGER :: ITIED
          INTEGER, intent(in) :: S_KREMNOD
          INTEGER, intent(in) :: S_REMNOD
          INTEGER, intent(in) :: NSPMD

!     REAL
          my_real ::  GAP
          my_real ::  TZINF
          my_real ::  GAPMIN
          my_real ::  GAPMAX
          my_real ::  BMINMA(12)
          my_real ::  CURV_MAX(NRTM)
          my_real ::  BGAPSMX
          my_real , INTENT(IN) :: DRAD
          my_real , INTENT(IN) :: DGAPLOAD
          my_real :: X(3,NUMNOD)
          my_real :: CAND_P(NCONTACT)
          my_real :: STFN(NSN)
          my_real :: STF(NRTM)
          my_real :: GAP_S(NSN)
          my_real :: GAP_M(NRTM)
          my_real :: GAP_S_L(NSN)
          my_real :: GAP_M_L(NRTM)
          my_real :: CAND_F(NCONTACT)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
          INTEGER I, J,&
          &N, S_PREV_REMOTE_NUMBER,&
          &NSNFIOLD(NSPMD)
!     REAL
          my_real XYZM(6,2), MARGE, AAA
          INTEGER NBX,NBY,NBZ ! number of cells in each direction
          INTEGER (KIND=8) :: NBX8,NBY8,NBZ8,RES8,LVOXEL8

!-----------------------------------------------
!   S o u r c e  L i n e s
!-----------------------------------------------
!
!-----INITIALISATION
!----- Get bounds of the domains
!
          XYZM(1,1) = BMINMA(4)
          XYZM(2,1) = BMINMA(5)
          XYZM(3,1) = BMINMA(6)
          XYZM(4,1) = BMINMA(1)
          XYZM(5,1) = BMINMA(2)
          XYZM(6,1) = BMINMA(3)
          XYZM(1,2) = BMINMA(10)
          XYZM(2,2) = BMINMA(11)
          XYZM(3,2) = BMINMA(12)
          XYZM(4,2) = BMINMA(7)
          XYZM(5,2) = BMINMA(8)
          XYZM(6,2) = BMINMA(9)
          I_MEM = 0
!
          IF(INACTI==5.OR.INACTI==6.OR.INACTI==7.OR.&
          &IFQ>0.OR.NUM_IMP>0.OR.ITIED/=0) THEN
            S_PREV_REMOTE_NUMBER = NSNR
          ELSE
            S_PREV_REMOTE_NUMBER = 0
          END IF
          MARGE = TZINF-MAX(GAP+DGAPLOAD,DRAD) ! margin
! Work on the reduced box
          IF( NMN /= 0 ) THEN
            AAA = SQRT(NMN /&
            &((BMINMA(7)-BMINMA(10))*(BMINMA(8)-BMINMA(11))&
            &+(BMINMA(8)-BMINMA(11))*(BMINMA(9)-BMINMA(12))&
            &+(BMINMA(9)-BMINMA(12))*(BMINMA(7)-BMINMA(10))))
          ELSE
            AAA = 0
          ENDIF

          AAA = 0.75*AAA

          NBX = NINT(AAA*(BMINMA(7)-BMINMA(10)))
          NBY = NINT(AAA*(BMINMA(8)-BMINMA(11)))
          NBZ = NINT(AAA*(BMINMA(9)-BMINMA(12)))
!
          NBX = MAX(NBX,1)
          NBY = MAX(NBY,1)
          NBZ = MAX(NBZ,1)

          NBX8=NBX
          NBY8=NBY
          NBZ8=NBZ
          RES8=(NBX8+2)*(NBY8+2)*(NBZ8+2)
          LVOXEL8 = LVOXEL

          IF(RES8 > LVOXEL8) THEN
            AAA = LVOXEL
            AAA = AAA/((NBX8+2)*(NBY8+2)*(NBZ8+2))
            AAA = AAA**(THIRD)
            NBX = INT((NBX+2)*AAA)-2
            NBY = INT((NBY+2)*AAA)-2
            NBZ = INT((NBZ+2)*AAA)-2
            NBX = MAX(NBX,1)
            NBY = MAX(NBY,1)
            NBZ = MAX(NBZ,1)
          ENDIF

          NBX8=NBX
          NBY8=NBY
          NBZ8=NBZ
          RES8=(NBX8+2)*(NBY8+2)*(NBZ8+2)

          IF(RES8 > LVOXEL8) THEN
            NBX = MIN(100,MAX(NBX8,1))
            NBY = MIN(100,MAX(NBY8,1))
            NBZ = MIN(100,MAX(NBZ8,1))
          ENDIF

          ! redundancy in smp
!$OMP SINGLE
          DO I=1,(NBX+2)*(NBY+2)*(NBZ+2)
            VOXEL1(I)=0
          ENDDO
!$OMP END SINGLE NOWAIT
          IF(ITASK == 0) THEN
            ALLOCATE(NEXT_NOD(NSN+NSNR))
            ALLOCATE(PREV_REMOTE_NUMBER(S_PREV_REMOTE_NUMBER))
            ! find the old id of remote candidate nodes (inactive, ifq, itied)
            if(nspmd>1.and.(inacti==5.or.inacti==6.or.inacti==7.or.ifq>0.or.itied/=0)) then
              CALL SPMD_OLDNUMCD(RENUM,PREV_REMOTE_NUMBER,S_PREV_REMOTE_NUMBER,NSNROLD)
            end if
          endif
          call MY_BARRIER

          CALL INTER7_CANDIDATE_PAIRS(&
          &NSN     ,PREV_REMOTE_NUMBER ,NSNR     ,S_PREV_REMOTE_NUMBER  ,I_MEM   ,&
          &IRECT   ,X        ,STF      ,STFN     ,XYZM    ,&
          &NSV     ,II_STOK  ,CAND_N   ,ESHIFT   ,CAND_E  ,&
          &NCONTACT,TZINF    ,GAP_S_L  ,GAP_M_L ,&
          &VOXEL1  ,NBX      ,NBY      ,NBZ      ,&
          &INACTI  ,IFQ      ,CAND_A,CAND_P   ,IFPEN   ,&
          &NRTM    ,NSNROLD  ,IGAP     ,GAP      ,GAP_S   ,&
          &GAP_M   ,GAPMIN   ,GAPMAX   ,MARGE    ,CURV_MAX,&
          &ITASK    ,BGAPSMX  ,S_KREMNOD, KREMNOD  ,S_REMNOD, REMNOD  ,&
          &FLAGREMNODE,DRAD   ,ITIED    ,CAND_F  ,&
          &DGAPLOAD, s_cand_a,&
          &TOTAL_NB_NRTM,  NUMNOD, XREM, SIZE(XREM,1),&
          &IREM, size(irem,1), NEXT_NOD)

          IF(ITASK==0)  THEN
            IF(ALLOCATED(NEXT_NOD)) DEALLOCATE(NEXT_NOD)
            IF(ALLOCATED(PREV_REMOTE_NUMBER)) DEALLOCATE(PREV_REMOTE_NUMBER)

          ENDIF
!     I_MEM = 2 ==> Not enough memory
          IF (I_MEM ==2) RETURN
          IF(I_MEM==1)THEN
            NB_N_B = NB_N_B + 1
            IF ( NB_N_B > NCONT) THEN
              CALL ANCMSG(MSGID=85,ANMODE=ANINFO,&
              &I1=NOINT)
              CALL ARRET(2)
            ENDIF
            ILD = 1
          ELSEIF(I_MEM==2) THEN
            RETURN
            TZINF = THREE_OVER_4*TZINF
! taille de boite non diminuee
!        MINBOX= THREE_OVER_4*MINBOX
!        MAXBOX= THREE_OVER_4*MAXBOX
            IF( TZINF<=MAX(GAP+DGAPLOAD,DRAD)  ) THEN
              CALL ANCMSG(MSGID=98,ANMODE=ANINFO,&
              &I1=NOINT,C1='(I7BUCE)')
              CALL ARRET(2)
            ENDIF
            ILD = 1
          ELSEIF(I_MEM==3)THEN
            NB_N_B = NB_N_B + 1
            IF ( NB_N_B > NCONT) THEN
              CALL ANCMSG(MSGID=100,ANMODE=ANINFO,&
              &I1=NOINT)
              CALL ARRET(2)
            ENDIF
            ILD = 1
          ENDIF
!
          RETURN
        END
      END MODULE
