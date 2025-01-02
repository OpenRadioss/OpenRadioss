!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
      !||    spmd_oldnumcd                ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod                 ../common_source/modules/constant_mod.F
      !||    fill_voxel_mod               ../engine/source/interfaces/intsort/fill_voxel.F90
      !||    inter7_candidate_pairs_mod   ../engine/source/interfaces/intsort/inter7_candidate_pairs.F90
      !||    inter_struct_mod             ../engine/share/modules/inter_struct_mod.F
      !||    message_mod                  ../engine/share/message_module/message_mod.F
      !||    tri7box                      ../engine/share/modules/tri7box.F
      !||    voxel_dimensions_mod         ../engine/source/interfaces/intsort/voxel_dimensions.F90
      !||====================================================================
        SUBROUTINE INTER7_COLLISION_DETECTION(&
        &X        ,IRECT   ,NSV     ,INACTI   ,CAND_P  ,&
        &NRTM    ,NSN     ,CAND_E   ,CAND_N  ,&
        &GAP      ,NOINT   ,II_STOK ,NCONTACT ,BMINMA  ,&
        &TZINF    ,CAND_A,CURV_MAX, RENUM_SIZ,&
        &NB_N_B   ,ESHIFT  ,ILD     ,IFQ      ,IFPEN   ,&
        &STF     ,IGAP     ,GAP_S   ,&
        &NSNR     ,NCONT   ,RENUM   ,NSNROLD  ,GAP_M   ,&
        &GAPMIN   ,GAPMAX  ,NUM_IMP,GAP_S_L ,&
        &GAP_M_L  ,ITASK   ,BGAPSMX  ,I_MEM   ,&
        &KREMNOD  ,REMNOD  ,FLAGREMNODE, DRAD ,&
        &ITIED    ,CAND_F  ,DGAPLOAD,&
        &s_cand_a,&
        &S_KREMNOD, S_REMNOD, NSPMD, NUMNOD, inter_struct, &
        & intheat,idt_therm,nodadt_therm)

!============================================================================
!   M o d u l e s
!-----------------------------------------------
          USE voxel_dimensions_mod, only : compute_voxel_dimensions
          USE FILL_VOXEL_MOD
          USE INTER_STRUCT_MOD
          USE TRI7BOX
          USE INTER7_CANDIDATE_PAIRS_MOD
          USE MESSAGE_MOD
          USE CONSTANT_MOD , ONLY : THREE_OVER_4
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
          implicit none
#include "my_real.inc"
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
          TYPE(inter_struct_type), INTENT(INOUT) :: inter_struct !< structure containing the interface sorting data
          INTEGER :: NSN !< number of secondary nodes
          INTEGER :: NOINT !< number of the current interface
          INTEGER :: INACTI !< inacti option
          INTEGER :: IFQ !< friction option
          INTEGER :: NSNR !< number of remote nodes
          INTEGER :: NSNROLD !< number of old secondary nodes
          INTEGER :: RENUM_SIZ !< size of renum array
          INTEGER, INTENT(IN) :: NUMNOD !< global number of nodes
          INTEGER, INTENT(in) :: NRTM !< number of segments per threads
          INTEGER, INTENT(in) :: s_cand_a !< size of cand_a array
          INTEGER :: IRECT(4,NRTM) !< connectivity of the segments
          INTEGER :: NSV(NSN) !< global id of the secondary nodes
          INTEGER :: CAND_A(s_cand_a) !< ? 
          INTEGER :: RENUM(RENUM_SIZ) !< ?
          INTEGER :: NUM_IMP           !< 
          INTEGER :: ITASK            !< OpenMP task id
          INTEGER :: CAND_E(NCONTACT) !< segment id of the contact candidate
          INTEGER :: CAND_N(NCONTACT) !< node id of the contact candidate
          INTEGER :: IFPEN(NCONTACT)
          INTEGER :: KREMNOD(*) !< remnode option
          INTEGER :: REMNOD(*) !< remnode option
          INTEGER :: NCONTACT !< number of contact candidates
          INTEGER :: ESHIFT !< OpenMP shift
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
          integer, intent(in) :: intheat
          integer, intent(in) :: idt_therm
          integer, intent(in) :: nodadt_therm

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
          my_real :: STF(NRTM)
          my_real :: GAP_S(NSN)
          my_real :: GAP_M(NRTM)
          my_real :: GAP_S_L(NSN)
          my_real :: GAP_M_L(NRTM)
          my_real :: CAND_F(NCONTACT)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
          INTEGER S_PREV_REMOTE_NUMBER
!     REAL
          my_real XYZM(6,2), MARGE

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


        if(itask == 0) then
            allocate(prev_remote_number(s_prev_remote_number))
            ! find the old id of remote candidate nodes (inactive, ifq, itied)
            if(nspmd>1.and.(inacti==5.or.inacti==6.or.inacti==7.or.ifq>0.or.itied/=0)) then
              call spmd_oldnumcd(renum,prev_remote_number,s_prev_remote_number,nsnrold, &
                                 intheat,idt_therm,nodadt_therm)
            end if
        endif
!$OMP SINGLE
          if(nrtm>0)then
            ! finish to fill the voxel with local nondes
            !     CALL FILL_VOXEL_LOCAL_PARTIAL(nsn,nsv,nsnr,nrtm,numnod,x,stfn,INTER_STRUCT, DUMMY, 0)

!            call fill_voxel(FLAG_LOCAL,&
!       &                    nsn,&
!       &                    nsnr,&
!       &                    inter_struct%nbx,&
!       &                    inter_struct%nby,&
!       &                    inter_struct%nbz,&
!       &                    nrtm,& 
!       &                    size(XREM,1),&
!       &                    numnod,&
!       &                    nsv,&
!       &                    inter_struct%voxel,&
!       &                    inter_struct%next_nod,&
!       &                    inter_struct%size_node,&
!       &                    inter_struct%nb_voxel_on,&
!       &                    inter_struct%list_nb_voxel_on,&
!       &                    inter_struct%last_nod,&
!       &                    x,&
!       &                    stfn,&
!       &                    xrem,&
!       &                    inter_struct%box_limit_main)

 
!            call fill_voxel(FLAG_NONE,&
!       &                    1, &
!       &                    nsn,&
!       &                    nsnr,&
!       &                    inter_struct%nbx,&
!       &                    inter_struct%nby,&
!       &                    inter_struct%nbz,&
!       &                    nrtm,& 
!       &                    size(XREM,1),&
!       &                    numnod,&
!       &                    nsv,&
!       &                    inter_struct%voxel,&
!       &                    inter_struct%next_nod,&
!       &                    inter_struct%size_node,&
!       &                    inter_struct%nb_voxel_on,&
!       &                    inter_struct%list_nb_voxel_on,&
!       &                    inter_struct%last_nod,&
!       &                    x,&
!       &                    stfn,&
!       &                    xrem,&
!       &                    inter_struct%box_limit_main)
          ENDIF
!$OMP END SINGLE



            CALL INTER7_CANDIDATE_PAIRS( &
            & NSN, &
            & PREV_REMOTE_NUMBER, &
            & NSNR, &
            & S_PREV_REMOTE_NUMBER, &
            & I_MEM, &
            & IRECT, &
            & X, &
            & STF, &
            & XYZM, &
            & NSV, &
            & II_STOK, &
            & CAND_N, &
            & ESHIFT, &
            & CAND_E, &
            & NCONTACT, &
            & TZINF, &
            & GAP_S_L, &
            & GAP_M_L, &
            & inter_struct%VOXEL, &
            & inter_struct%NBX, &
            & inter_struct%NBY, &
            & inter_struct%NBZ, &
            & INACTI, &
            & IFQ, &
            & CAND_A, &
            & CAND_P, &
            & IFPEN, &
            & NRTM, &
            & NSNROLD, &
            & IGAP, &
            & GAP, &
            & GAP_S, &
            & GAP_M, &
            & GAPMIN, &
            & GAPMAX, &
            & MARGE, &
            & CURV_MAX, &
            & BGAPSMX, &
            & S_KREMNOD, &
            & KREMNOD, &
            & S_REMNOD, &
            & REMNOD, &
            & FLAGREMNODE, &
            & DRAD, &
            & ITIED, &
            & CAND_F, &
            & DGAPLOAD, &
            & s_cand_a, &
            & NUMNOD, &
            & XREM, &
            & SIZE(XREM, 1), &
            & IREM, &
            & SIZE(IREM, 1), &
            & inter_struct%NEXT_NOD)

          IF(ITASK==0)  THEN
!           IF(ALLOCATED(inter_struct%NEXT_NOD)) DEALLOCATE(inter_struct%NEXT_NOD)
            IF(ALLOCATED(PREV_REMOTE_NUMBER)) DEALLOCATE(PREV_REMOTE_NUMBER)
!           if(allocated(inter_struct%list_nb_voxel_on)) deallocate(inter_struct%list_nb_voxel_on)

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
