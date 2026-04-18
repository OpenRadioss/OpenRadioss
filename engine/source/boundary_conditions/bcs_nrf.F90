!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
! ======================================================================================================================
!||====================================================================
!||    bcs_nrf_mod   ../engine/source/boundary_conditions/bcs_nrf.F90
!||--- called by ------------------------------------------------------
!||    resol         ../engine/source/engine/resol.F
!||====================================================================
      module bcs_nrf_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief boundary condition /BCS/NRF
!! \details  lagrange FEM only
!||====================================================================
!||    bcs_nrf         ../engine/source/boundary_conditions/bcs_nrf.F90
!||--- called by ------------------------------------------------------
!||    resol           ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    ancmsg          ../engine/source/output/message/message.F
!||    arret           ../engine/source/system/arret.F
!||--- uses       -----------------------------------------------------
!||    bcs_mod         ../common_source/modules/boundary_conditions/bcs_mod.F90
!||    constant_mod    ../common_source/modules/constant_mod.F
!||    elbufdef_mod    ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    h3d_mod         ../engine/share/modules/h3d_mod.F
!||    message_mod     ../engine/share/message_module/message_mod.F
!||    precision_mod   ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine bcs_nrf(n2d      , numnod  , &
          x        , v       , a     , &
          nixs     , nixtg   , nixq  ,&
          numels   , numeltg , numelq,&
          ixs      , ixtg    , ixq   ,   &
          iparit   , lsky    , fsky  , &
          wfext    , fext    , dt1, &
          anim_v   , outp_v  , h3d_data)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use bcs_mod , only : bcs
          use precision_mod , only : WP
          use elbufdef_mod , only : elbuf_struct_
          use constant_mod , only : zero, em14, half, em20
          use h3d_mod , only : h3d_database
          use message_mod , only : aninfo, ancmsg
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
#include "units_c.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer,intent(in) :: numnod
          integer,intent(in) :: n2d
          integer,intent(in) :: iparit
          real(kind=wp),intent(in) :: x(3,numnod)
          real(kind=wp),intent(in) :: v(3,numnod)
          real(kind=wp),intent(inout) :: a(3,numnod)
          integer,intent(in) :: nixs
          integer,intent(in) :: nixtg
          integer,intent(in) :: nixq
          integer,intent(in) :: numels
          integer,intent(in) :: numeltg
          integer,intent(in) :: numelq
          integer,intent(in) :: ixs(nixs,numels)
          integer,intent(in) :: ixtg(nixtg,numeltg)
          integer,intent(in) :: ixq(nixq,numelq)
          integer,intent(in) :: lsky
          real(kind=wp),intent(inout) :: fsky(8,lsky)
          double precision,intent(inout) :: wfext
          real(kind=wp),intent(inout) :: fext(3,numnod)
          integer,intent(in) :: anim_v(10)
          integer,intent(in) :: outp_v(10)
          type(h3d_database) :: h3d_data
          real(kind=wp),intent(in) :: dt1

          !-----------------------------!
          integer icf3d(4,6), icf2d(2,4)

          data icf3d / &
            1,2,3,4, &   ! face 1
            3,7,8,4, &   ! face 2
            5,6,7,8, &   ! face 3
            1,2,6,5, &   ! face 4
            2,3,7,6, &   ! face 5
            1,4,8,5  /   ! face 6

          data icf2d / &
            1,2, &       ! edge 1  (2d face)
            2,3, &       ! edge 2  (2d face)
            3,4, &       ! edge 3  (2d face)
            4,1  /       ! edge 4  (2d face)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ii       !< loop
          integer :: jj       !< loop
          integer :: bcs_uid  !< bcs user identifier
          integer :: nface    !< number of boundary faces
          integer :: ielem    !< element id (attached to the face)
          integer :: iface    !< local face number
          integer :: nod1,nod2,nod3,nod4, nod(4), nod_id, inod, maxnod
          integer :: iad      !< index for parith/on storage (summation done later)
          real(kind=wp) :: area               !< area of the face
          real(kind=wp) :: nx,ny,nz           !< normal vector
          real(kind=wp) :: ty,tz              !< tangential vector (2d)
          real(kind=wp) :: vel_t(3)           !< tangential velocity vector (3d)
          real(kind=wp) :: L                  !< norm of normal vector
          real(kind=wp) :: fx(4),fy(4),fz(4)  !< damping forces
          real(kind=wp) :: vn                 !< normal velocity
          real(kind=wp) :: vt                 !< tangential velocity (2d)
          real(kind=wp) :: rCpN, rCsN         !< rho.ssp_p/N and rho.ssp_s/N
          real(kind=WP) :: wfextt             !< local work of external forces
          real(kind=WP) :: X12,Y12,Z12        !< [N1N2] vector
          real(kind=WP) :: X13,Y13,Z13        !< [N1N3] vector
          real(kind=WP) :: X14,Y14,Z14        !< [N1N4] vector
          real(kind=WP) :: X23,Y23,Z23        !< [N2N3] vector

          real(kind=WP) :: eps                !< tolerance for degenerated face
          INTEGER :: IOUTPUT                    !< flag for output request
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Preconditions
! ----------------------------------------------------------------------------------------------------------------------
          if(bcs%num_nrf == 0) return
          if(iparit == 1)then
            CALL ANCMSG(MSGID=3,ANMODE=ANINFO) ! /BCS/NRF not yet compatible with PARITH/ON
            CALL ARRET(2)
          end if
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          wfextt = zero
          IOUTPUT  = ANIM_V(5)+OUTP_V(5) + H3D_DATA%N_VECT_FINT + ANIM_V(6)+OUTP_V(6) + H3D_DATA%N_VECT_FEXT !in case of requested output of nodal external forces
          eps = em14

          do ii=1,bcs%num_nrf

            bcs_uid = bcs%nrf(ii)%user_id
            nface = bcs%nrf(ii)%list%size
            do jj=1,nface
              ielem = bcs%nrf(ii)%list%elem(jj)
              iface = bcs%nrf(ii)%list%face(jj)
              rCpN = bcs%nrf(ii)%list%rCp(jj)
              rCsN = bcs%nrf(ii)%list%rCs(jj)

              if(n2d == 0)then

                !2d-solid (tria & quad)
                NOD1=IXS(1+icf3d(1,iface),ielem)
                NOD2=IXS(1+icf3d(2,iface),ielem)
                NOD3=IXS(1+icf3d(3,iface),ielem)
                NOD4=IXS(1+icf3d(4,iface),ielem)
                MAXNOD=4
                IF(NOD3 == NOD4) THEN
                  NOD4 = 0 ! tria case
                END IF
                IF(NOD4==0)MAXNOD=3

                IF(NOD4 == 0)THEN
                  X12 = X(1,NOD2)-X(1,NOD1) ; Y12 = X(2, NOD2)-X(2,NOD1) ; Z12 = X(3,NOD2)-X(3,NOD1)
                  X13 = X(1,NOD3)-X(1,NOD1) ; Y13 = X(2, NOD3)-X(2,NOD1) ; Z13 = X(3,NOD3)-X(3,NOD1)
                  NX  =  Y12*Z13 - Z12*Y13
                  NY  =  Z12*X13 - X12*Z13
                  NZ  =  X12*Y13 - Y12*X13
                ELSE
                  X12 = X(1,NOD2)-X(1,NOD1) ; Y12 = X(2,NOD2)-X(2,NOD1) ; Z12 = X(3,NOD2)-X(3,NOD1)
                  X13 = X(1,NOD3)-X(1,NOD1) ; Y13 = X(2,NOD3)-X(2,NOD1) ; Z13 = X(3,NOD3)-X(3,NOD1)
                  X14 = X(1,NOD4)-X(1,NOD1) ; Y14 = X(2,NOD4)-X(2,NOD1) ; Z14 = X(3,NOD4)-X(3,NOD1)
                  X23 = X(1,NOD3)-X(1,NOD2) ; Y23 = X(2,NOD3)-X(2,NOD2) ; Z23 = X(3,NOD3)-X(3,NOD2)
                  ! normal with mean of 2 triangles
                  NX =  (Y12*Z13 - Z12*Y13) + (Y13*Z14 - Z13*Y14)
                  NY =  (Z12*X13 - X12*Z13) + (Z13*X14 - X13*Z14)
                  NZ =  (X12*Y13 - Y12*X13) + (X13*Y14 - Y13*X14)
                END IF

                L = SQRT(NX*NX + NY*NY + NZ*NZ)
                L = MAX(EM20,L) ! not supposed to be 0.0, otherwise time step is 0.0
                NX = NX / L   ! normale unitaire
                NY = NY / L   ! normale unitaire
                NZ = NZ / L   ! normale unitaire

                AREA = HALF*L

                NOD(1:4) = (/NOD1,NOD2,NOD3,NOD4/)
                fx(4) = zero
                fy(4) = zero
                fz(4) = zero
                DO INOD = 1,MAXNOD
                  NOD_ID = NOD(INOD)
                  !absorbing force contribution on NOD_ID
                  Vn = V(1,NOD_ID)*NX+V(2,NOD_ID)*NY+V(3,NOD_ID)*NZ
                  vel_t(1) = V(1,NOD_ID) - Vn*NX
                  vel_t(2) = V(2,NOD_ID) - Vn*NY
                  vel_t(3) = V(3,NOD_ID) - Vn*NZ
                  Vn = Vn*AREA
                  Vel_t(:) = Vel_t(:)*AREA
                  fx(inod) = - rCpN*vn*NX - rCsN*vel_t(1)
                  fy(inod) = - rCpN*vn*NY - rCsN*vel_t(2)
                  fz(inod) = - rCpN*vn*NZ - rCsN*vel_t(3)
                END DO
!omp critical
                IF(IPARIT == 0)THEN
                  A(1,NOD1) = A(1,NOD1) + fx(1)
                  A(2,NOD1) = A(2,NOD1) + fy(1)
                  A(3,NOD1) = A(3,NOD1) + fz(1)
                  A(1,NOD2) = A(1,NOD2) + fx(2)
                  A(2,NOD2) = A(2,NOD2) + fy(2)
                  A(3,NOD2) = A(3,NOD2) + fz(2)
                  A(1,NOD3) = A(1,NOD3) + fx(3)
                  A(2,NOD3) = A(2,NOD3) + fy(3)
                  A(3,NOD3) = A(3,NOD3) + fz(3)
                  IF(NOD4 /= 0)THEN
                    A(1,NOD4) = A(1,NOD4) + fx(4)
                    A(2,NOD4) = A(2,NOD4) + fy(4)
                    A(3,NOD4) = A(3,NOD4) + fz(4)
                  END IF
                ELSE
                  IAD = 1
                  FSKY(1,IAD) = FX(1)
                  FSKY(2,IAD) = FY(1)
                  FSKY(3,IAD) = FZ(1)
                  IAD = 1
                  FSKY(1,IAD) = FX(2)
                  FSKY(2,IAD) = FY(2)
                  FSKY(3,IAD) = FZ(2)
                  IAD = 1
                  FSKY(1,IAD) = FX(3)
                  FSKY(2,IAD) = FY(3)
                  FSKY(3,IAD) = FZ(3)
                  IF(NOD4 /= 0)THEN
                    IAD = 1
                    FSKY(1,IAD) = FX(4)
                    FSKY(2,IAD) = FY(4)
                    FSKY(3,IAD) = FZ(4)
                  END IF
                END IF
                IF(IOUTPUT > 0)THEN
                  FEXT(1,NOD1) = FEXT(1,NOD1) + fx(1)
                  FEXT(2,NOD1) = FEXT(2,NOD1) + fy(1)
                  FEXT(3,NOD1) = FEXT(3,NOD1) + fz(1)
                  FEXT(1,NOD2) = FEXT(1,NOD2) + fx(2)
                  FEXT(2,NOD2) = FEXT(2,NOD2) + fy(2)
                  FEXT(3,NOD2) = FEXT(3,NOD2) + fz(2)
                  FEXT(1,NOD3) = FEXT(1,NOD3) + fx(3)
                  FEXT(2,NOD3) = FEXT(2,NOD3) + fy(3)
                  FEXT(3,NOD3) = FEXT(3,NOD3) + fz(3)
                  IF(NOD4 /= 0)THEN
                    FEXT(1,NOD4) = FEXT(1,NOD4) + fx(4)
                    FEXT(2,NOD4) = FEXT(2,NOD4) + fy(4)
                    FEXT(3,NOD4) = FEXT(3,NOD4) + fz(4)
                  END IF
                END IF
!omp end critical

                ! External Force Work increment
                WFEXTT = WFEXTT + DT1 * (  FX(1)*V(1,NOD1) + FY(1)*V(2,NOD1) + FZ(1)*V(3,NOD1)  &
                  + FX(2)*V(1,NOD2) + FY(2)*V(2,NOD2) + FZ(2)*V(3,NOD2)  &
                  + FX(3)*V(1,NOD3) + FY(3)*V(2,NOD3) + FZ(3)*V(3,NOD3)  &
                  + FX(4)*V(1,NOD4) + FY(4)*V(2,NOD4) + FZ(4)*V(3,NOD4) )

              else

                !2d-solid (tria & quad)
                if(numelq > 0)then
                  NOD1=IXQ(1+icf2d(1,iface),ielem)
                  NOD2=IXQ(1+icf2d(2,iface),ielem)
                elseif (numeltg > 0)then
                  NOD1=IXTG(1+icf2d(1,iface),ielem)
                  NOD2=IXTG(1+icf2d(2,iface),ielem)
                else
                  cycle
                end if

                !Area.t
                TY =  X(2,NOD1)-X(2,NOD2)
                TZ =  X(3,NOD1)-X(3,NOD2)
                AREA = SQRT(TY*TY + TZ*TZ)
                TY = TY/AREA
                TZ = TZ/AREA
                !Area.n
                NY = -TZ
                NZ =  TY

                !absorbing force contribution on NOD1
                Vn = V(2,NOD1)*NY+V(3,NOD1)*NZ
                Vt = V(2,NOD1)*TY+V(3,NOD1)*TZ
                Vn = Vn*AREA
                Vt = Vt*AREA
                fy(1) = - rCpN*vn*NY - rCsN*vt*TY
                fz(1) = - rCpN*vn*NZ - rCsN*vt*TZ

                !absorbing force contribution on NOD2
                Vn = V(2,NOD2)*NY+V(3,NOD2)*NZ
                Vt = V(2,NOD2)*TY+V(3,NOD2)*TZ
                Vn = Vn*AREA
                Vt = Vt*AREA
                fy(2) = - rCpN*vn*NY - rCsN*vt*TY
                fz(2) = - rCpN*vn*NZ - rCsN*vt*TZ


!omp critical
                IF(IPARIT ==0)THEN
                  A(2,NOD1) = A(2,NOD1) + fy(1)
                  A(3,NOD1) = A(3,NOD1) + fz(1)
                  A(2,NOD2) = A(2,NOD2) + fy(2)
                  A(3,NOD2) = A(3,NOD2) + fz(2)
                ELSE
                  IAD = 1
                  FSKY(2,IAD) = FY(1)
                  FSKY(3,IAD) = FZ(1)
                  IAD = 1
                  FSKY(2,IAD) = FY(2)
                  FSKY(3,IAD) = FZ(2)
                END IF
                IF(IOUTPUT > 0)THEN
                  FEXT(2,NOD1) = FEXT(2,NOD1) + fy(1)
                  FEXT(3,NOD1) = FEXT(3,NOD1) + fz(1)
                  FEXT(2,NOD2) = FEXT(2,NOD2) + fy(2)
                  FEXT(3,NOD2) = FEXT(3,NOD2) + fz(2)
                END IF
!omp end critical

                !External Force Work
                WFEXTT = WFEXTT + DT1 * (  FY(1)*V(2,NOD1) + FZ(1)*V(3,NOD1)  &
                  + FY(2)*V(2,NOD2) + FZ(2)*V(3,NOD2) )


              end if
            enddo

!omp critical
            wfext = wfext + wfextt
!omp end critical

          end do!next ii

! ----------------------------------------------------------------------------------------------------------------------
          return

        end subroutine bcs_nrf
      end module bcs_nrf_mod
