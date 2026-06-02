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
!! \brief boundary condition /BCS/NRF (Lysmer-Kuhlemeyer absorbing boundary condition)
!! \details  lagrange FEM only
!||====================================================================
!||    bcs_nrf         ../engine/source/boundary_conditions/bcs_nrf.F90
!||--- called by ------------------------------------------------------
!||    resol           ../engine/source/engine/resol.F
!||--- uses       -----------------------------------------------------
!||    bcs_mod         ../common_source/modules/boundary_conditions/bcs_mod.F90
!||    constant_mod    ../common_source/modules/constant_mod.F
!||    h3d_mod         ../engine/share/modules/h3d_mod.F
!||    precision_mod   ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine bcs_nrf(n2d      , numnod  , &
          x        , v       , a     , &
          iparit   , lsky    , fsky  , &
          wfext    , fext    , dt1, &
          anim_v   , outp_v  , h3d_data, &
          bcs      , stifn   , ms)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use bcs_mod , only : bcs_struct_
          use precision_mod , only : WP
          use constant_mod , only : zero, em14, half, em20, TWO, one, fourth, third
          use h3d_mod , only : h3d_database
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
          integer,intent(in) :: lsky
          real(kind=wp),intent(inout) :: fsky(8,lsky)
          double precision,intent(inout) :: wfext
          real(kind=wp),intent(inout) :: fext(3,numnod)
          integer,intent(in) :: anim_v(10)
          integer,intent(in) :: outp_v(10)
          type(h3d_database) :: h3d_data
          real(kind=wp),intent(in) :: dt1
          type(bcs_struct_), intent(inout) :: bcs
          real(kind=WP),intent(inout) :: stifn(numnod)
          real(kind=WP),intent(in) :: ms(numnod)

          !-----------------------------!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ii       !< loop
          integer :: jj       !< loop
          integer :: nface    !< number of boundary faces
          integer :: nod1,nod2,nod3,nod4, nod(4), nod_id, inod, maxnod
          integer :: iad      !< index for parith/on storage (summation done later)
          real(kind=wp) :: area               !< area of the face
          real(kind=wp) :: nx,ny,nz           !< normal vector
          real(kind=wp) :: ty,tz              !< tangential vector (2d)
          real(kind=wp) :: vel_t(3)           !< tangential velocity vector (3d)
          real(kind=wp) :: L                  !< norm of normal vector
          real(kind=wp) :: fx(4),fy(4),fz(4)  !< damping forces
          real(kind=wp) :: Fface(3)           !< damping forces on face
          real(kind=wp) :: vn                 !< normal velocity
          real(kind=wp) :: vt                 !< tangential velocity (2d)
          real(kind=wp) :: rCp, rCs           !< rho.ssp_p and rho.ssp_s
          real(kind=WP) :: wfextt             !< local work of external forces
          real(kind=WP) :: X12,Y12,Z12        !< [N1N2] vector
          real(kind=WP) :: X13,Y13,Z13        !< [N1N3] vector
          real(kind=WP) :: X24,Y24,Z24        !< [N2N4] vector
          real(kind=WP) :: Fac                 !< normal velocity
          real(kind=WP) :: VF(3)
          real(kind=WP) :: norm_nrf           !< norm of equivalent nodal impedance vector
          real(kind=WP) :: HAREArCp, FACAREArCp,FACAREA !< for cpu cost
          INTEGER :: IOUTPUT                    !< flag for output request
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Preconditions
! ----------------------------------------------------------------------------------------------------------------------
          if(bcs%num_nrf == 0) return
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          IOUTPUT  = ANIM_V(5)+OUTP_V(5) + H3D_DATA%N_VECT_FINT + ANIM_V(6)+OUTP_V(6) + H3D_DATA%N_VECT_FEXT !in case of requested output of nodal external forces
          ! Reset la_nrf only for NRF boundary nodes (compact list)
          do ii = 1, bcs%nrf_num_nodes
            bcs%la_nrf(1:3, bcs%nrf_node_ids(ii)) = zero
          end do

          do ii=1,bcs%num_nrf
            wfextt = zero
            nface = bcs%nrf(ii)%list%size
            do jj=1,nface
              ! ielem = bcs%nrf(ii)%list%elem(jj)
              ! iface = bcs%nrf(ii)%list%face(jj)
              rCp = bcs%nrf(ii)%list%rCp(jj)
              rCs = bcs%nrf(ii)%list%rCs(jj)

              if(n2d == 0)then

                !2d-solid (tria & quad)
                NOD1=bcs%nrf(ii)%list%node_list(1,jj)
                NOD2=bcs%nrf(ii)%list%node_list(2,jj)
                NOD3=bcs%nrf(ii)%list%node_list(3,jj)
                NOD4=bcs%nrf(ii)%list%node_list(4,jj)
                MAXNOD=4
                FAC=FOURTH
                IF(NOD3 == NOD4) THEN
                  NOD4 = 0 ! tria case
                END IF
                IF(NOD4 == 0)THEN
                  MAXNOD=3
                  FAC=THIRD
                ENDIF

                IF(NOD4 == 0)THEN
                  X12 = X(1,NOD2)-X(1,NOD1) ; Y12 = X(2, NOD2)-X(2,NOD1) ; Z12 = X(3,NOD2)-X(3,NOD1)
                  X13 = X(1,NOD3)-X(1,NOD1) ; Y13 = X(2, NOD3)-X(2,NOD1) ; Z13 = X(3,NOD3)-X(3,NOD1)
                  NX  =  -Y12*Z13 + Z12*Y13
                  NY  =  -Z12*X13 + X12*Z13
                  NZ  =  -X12*Y13 + Y12*X13
                  VF(1)=V(1,NOD1)+V(1,NOD2)+V(1,NOD3)
                  VF(2)=V(2,NOD1)+V(2,NOD2)+V(2,NOD3)
                  VF(3)=V(3,NOD1)+V(3,NOD2)+V(3,NOD3)
                ELSE
                  X13 = X(1,NOD3)-X(1,NOD1) ; Y13 = X(2,NOD3)-X(2,NOD1) ; Z13 = X(3,NOD3)-X(3,NOD1)
                  X24 = X(1,NOD4)-X(1,NOD2) ; Y24 = X(2,NOD4)-X(2,NOD2) ; Z24 = X(3,NOD4)-X(3,NOD2)
                  ! normal from quad diagonals
                  NX = -Y13*Z24 + Z13*Y24
                  NY = -Z13*X24 + X13*Z24
                  NZ = -X13*Y24 + Y13*X24
                  VF(1)=V(1,NOD1)+V(1,NOD2)+V(1,NOD3)+V(1,NOD4)
                  VF(2)=V(2,NOD1)+V(2,NOD2)+V(2,NOD3)+V(2,NOD4)
                  VF(3)=V(3,NOD1)+V(3,NOD2)+V(3,NOD3)+V(3,NOD4)
                END IF

                L = SQRT(NX*NX + NY*NY + NZ*NZ)
                L = MAX(EM20,L) ! not supposed to be 0.0, otherwise time step is 0.0
                NX = NX / L   ! normale unitaire
                NY = NY / L   ! normale unitaire
                NZ = NZ / L   ! normale unitaire

                AREA = HALF*L

                fx(4) = zero
                fy(4) = zero
                fz(4) = zero

                VF(1:3)=FAC*VF(1:3)
                Vn = VF(1)*NX + VF(2)*NY + VF(3)*NZ
                vel_t(1) = VF(1) - Vn*NX
                vel_t(2) = VF(2) - Vn*NY
                vel_t(3) = VF(3) - Vn*NZ

                Fface(1) = - rCp*vn*NX - rCs*vel_t(1)
                Fface(2) = - rCp*vn*NY - rCs*vel_t(2)
                Fface(3) = - rCp*vn*NZ - rCs*vel_t(3)

                NOD(1)=NOD1;NOD(2)=NOD2; NOD(3)=NOD3; NOD(4)=NOD4;
                DO INOD = 1,MAXNOD
                  nod_id = nod(inod)

                  !absorbing force contribution on NOD_ID
                  ! FAC is 1/4 or 1/3 depending on quad or tria boundary face. It is also 1/MAXNOD
                  FACAREA = FAC*AREA
                  FACAREArCp = FACAREA*rCp
                  bcs%la_nrf(1,nod_id) = bcs%la_nrf(1,nod_id) + FACAREArCp*NX
                  bcs%la_nrf(2,nod_id) = bcs%la_nrf(2,nod_id) + FACAREArCp*NY
                  bcs%la_nrf(3,nod_id) = bcs%la_nrf(3,nod_id) + FACAREArCp*NZ
                  fx(inod) =  FACAREA * Fface(1)
                  fy(inod) =  FACAREA * Fface(2)
                  fz(inod) =  FACAREA * Fface(3)
                END DO

                IF(IPARIT == 0)THEN
!$omp critical
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
!$omp end critical                  
                ELSE
                  IAD = bcs%nrf(ii)%list%iadsky(1,jj)
                  FSKY(1,IAD) = FX(1)
                  FSKY(2,IAD) = FY(1)
                  FSKY(3,IAD) = FZ(1)
                  IAD = bcs%nrf(ii)%list%iadsky(2,jj)
                  FSKY(1,IAD) = FX(2)
                  FSKY(2,IAD) = FY(2)
                  FSKY(3,IAD) = FZ(2)
                  IAD = bcs%nrf(ii)%list%iadsky(3,jj)
                  FSKY(1,IAD) = FX(3)
                  FSKY(2,IAD) = FY(3)
                  FSKY(3,IAD) = FZ(3)
                  IF(NOD4 /= 0)THEN
                    IAD = bcs%nrf(ii)%list%iadsky(4,jj)
                    FSKY(1,IAD) = FX(4)
                    FSKY(2,IAD) = FY(4)
                    FSKY(3,IAD) = FZ(4)
                  END IF
                END IF
                IF(IOUTPUT > 0)THEN
!$omp critical                  
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
!$omp end critical                  
                END IF

                ! External Force Work increment
                WFEXTT = WFEXTT + DT1 * (  FX(1)*V(1,NOD1) + FY(1)*V(2,NOD1) + FZ(1)*V(3,NOD1)  &
                                         + FX(2)*V(1,NOD2) + FY(2)*V(2,NOD2) + FZ(2)*V(3,NOD2)  &
                                         + FX(3)*V(1,NOD3) + FY(3)*V(2,NOD3) + FZ(3)*V(3,NOD3) )
                IF(NOD4 /= 0)THEN
                  WFEXTT = WFEXTT + DT1 * (FX(4)*V(1,NOD4) + FY(4)*V(2,NOD4) + FZ(4)*V(3,NOD4) )
                END IF

              else

                !2d-solid (tria & quad)
                NOD1=bcs%nrf(ii)%list%node_list(1,jj)
                NOD2=bcs%nrf(ii)%list%node_list(2,jj)

                !Area.t
                TY =  X(2,NOD1)-X(2,NOD2)
                TZ =  X(3,NOD1)-X(3,NOD2)
                AREA = SQRT(TY*TY + TZ*TZ)
                TY = TY/AREA
                TZ = TZ/AREA
                !Area.n
                NY = -TZ
                NZ =  TY

                ! Axisymmetric case: multiply by R_mean
                if(n2d == 1)then
                  AREA = AREA * HALF*(X(2,NOD1)+X(2,NOD2))
                end if
                HAREArCp=HALF*AREA*rCp

                bcs%la_nrf(2,nod1) = bcs%la_nrf(2,nod1) + HAREArCp*NY
                bcs%la_nrf(3,nod1) = bcs%la_nrf(3,nod1) + HAREArCp*NZ
                bcs%la_nrf(2,nod2) = bcs%la_nrf(2,nod2) + HAREArCp*NY
                bcs%la_nrf(3,nod2) = bcs%la_nrf(3,nod2) + HAREArCp*NZ

                !absorbing force contribution on NOD1
                Vn = HALF*(V(2,NOD1)+V(2,NOD2))*NY + HALF*(V(3,NOD1)+V(3,NOD2))*NZ
                Vt = HALF*(V(2,NOD1)+V(2,NOD2))*TY + HALF*(V(3,NOD1)+V(3,NOD2))*TZ
                Fface(2) = AREA * (- rCp*vn*NY - rCs*vt*TY)
                Fface(3) = AREA * (- rCp*vn*NZ - rCs*vt*TZ)

                fy(1) = half * Fface(2)
                fz(1) = half * Fface(3)
                fy(2) = half * Fface(2)
                fz(2) = half * Fface(3)

                IF(IPARIT ==0)THEN
!$omp critical
                  A(2,NOD1) = A(2,NOD1) + fy(1)
                  A(3,NOD1) = A(3,NOD1) + fz(1)
                  A(2,NOD2) = A(2,NOD2) + fy(2)
                  A(3,NOD2) = A(3,NOD2) + fz(2)
!$omp end critical                  
                ELSE
                  IAD = bcs%nrf(ii)%list%iadsky(1,jj)
                  FSKY(2,IAD) = FY(1)
                  FSKY(3,IAD) = FZ(1)
                  IAD = bcs%nrf(ii)%list%iadsky(2,jj)              
                  FSKY(2,IAD) = FY(2)
                  FSKY(3,IAD) = FZ(2)
                END IF
                IF(IOUTPUT > 0)THEN
!$omp critical                  
                  FEXT(2,NOD1) = FEXT(2,NOD1) + fy(1)
                  FEXT(3,NOD1) = FEXT(3,NOD1) + fz(1)
                  FEXT(2,NOD2) = FEXT(2,NOD2) + fy(2)
                  FEXT(3,NOD2) = FEXT(3,NOD2) + fz(2)
!$omp end critical                  
                END IF

                !External Force Work
                WFEXTT = WFEXTT + DT1 * (  FY(1)*V(2,NOD1) + FZ(1)*V(3,NOD1)  &
                  + FY(2)*V(2,NOD2) + FZ(2)*V(3,NOD2) )


              end if
            enddo

!$omp critical
            wfext = wfext + wfextt
!$omp end critical

          end do!next ii
          ! --- CFL Stability : nodal stiffness updated consequently. Otherwise Lysmer-Kuhlemeyer absorbing boundary condition may be unstable.
          ! Iterate only over NRF boundary nodes (compact list) - avoids O(numnod) cost for large models
          do ii = 1, bcs%nrf_num_nodes
            inod = bcs%nrf_node_ids(ii)
            norm_nrf = sqrt(bcs%la_nrf(1,inod)*bcs%la_nrf(1,inod) + &
                            bcs%la_nrf(2,inod)*bcs%la_nrf(2,inod) + &
                            bcs%la_nrf(3,inod)*bcs%la_nrf(3,inod) )
            if(norm_nrf > zero .and. ms(inod) > em20)then
              stifn(inod) = stifn(inod) + (TWO*norm_nrf*norm_nrf)/ms(inod)
            end if
          end do

! ----------------------------------------------------------------------------------------------------------------------
          return

        end subroutine bcs_nrf
      end module bcs_nrf_mod
