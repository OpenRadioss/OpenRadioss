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
      module damping_vref_rby_mod
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!
!=======================================================================================================================
!!\brief This subroutine computes damping forces for /DAMP/VREL with RBODY
!=======================================================================================================================
!
        subroutine damping_vref_rby(igrnod,ngrnod,v,vr,a,                             &
                                    x,ms,dampr,nrdamp,ndamp,                          &
                                    ndamp_vrel,iparit,numnod,dt1,id_damp_vrel,        &
                                    tt,nnpby,nrbykin,npby,rby6,                       &
                                    rby6_c,tagslv_rby,weight,lskew,numskw,            &
                                    dim,damp,skew,tfext,size_rby6_c)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use GROUPDEF_MOD , only: GROUP_
          use constant_mod , only: pi,one,zero,two,half,em20
          use damping_vref_sum6_rby_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"       
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(GROUP_),                              intent(in) :: igrnod(ngrnod)              !< group od nodes structure
          integer,                                   intent(in) :: ngrnod                      !< number of groups of nodes 
          integer,                                   intent(in) :: ndamp                       !< number of /DAMP
          integer,                                   intent(in) :: nrdamp                      !< first dimension of array DAMP   
          integer,                                   intent(in) :: ndamp_vrel                  !< number of /DAMP/VREL
          integer,                                   intent(in) :: numnod                      !< number of nodes
          integer,                                   intent(in) :: id_damp_vrel(ndamp_vrel)    !< id of /DAMP/VREL in DAMP array          
          integer,                                   intent(in) :: nnpby                       !< first dimension of array NPBY
          integer,                                   intent(in) :: nrbykin                     !< number of rigid bodies
          integer,                                   intent(in) :: npby(nnpby,nrbykin)         !< main structure for rigid bodies
          integer,                                   intent(in) :: tagslv_rby(numnod)          !< tag of slaves nodes of rigid bodies
          integer,                                   intent(in) :: iparit                      !< PARITH/ON flag
          integer,                                   intent(in) :: weight(numnod)              !< weight   
          integer,                                   intent(in) :: lskew                       !< first dimension of array skew
          integer,                                   intent(in) :: numskw                      !< number of skews
          integer,                                   intent(in) :: dim                         !< first dimension of array damp
          integer,                                   intent(in) :: size_rby6_c                 !< dimension of array rby6c                                                               
          my_real,                                   intent(in) :: v(3,numnod)                 !< nodal velocity
          my_real,                                   intent(in) :: vr(3,numnod)                !< nodal rotational velocity
          my_real,                                intent(inout) :: a(3,numnod)                 !< nodal force
          my_real,                                   intent(in) :: x(3,numnod)                 !< node position
          my_real,                                   intent(in) :: ms(numnod)                  !< nodal mass                        
          my_real,                                intent(inout) :: dampr(nrdamp,ndamp)         !< main structure for option /DAMP
          my_real,                                   intent(in) :: dt1                         !< time step
          my_real,                                   intent(in) :: tt                          !< current time
          my_real,                                intent(inout) :: damp(dim,numnod)            !< damping force at previous time step 
          my_real,                                   intent(in) :: skew(lskew,numskw)          !< main structure for skews
          double precision,                       intent(inout) :: tfext                       !< external forces work
          double precision,                       intent(inout) :: rby6(8,6,nrbykin)           !< working array for rigid body assembly
          double precision,                       intent(inout) :: rby6_c(2,6,size_rby6_c)     !< working array for rigid body damping assembly  
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: nd,id,igr,isk,id_rby,id_func,im,nsn
          my_real :: freq,damp_a(3),damp_a2(3),fact,get_u_func,dxdy,dtini,t_start,t_stop
          double precision :: dw
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------
          external get_u_func
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!
          rby6_c(1:2,1:6,1:size_rby6_c) = zero 
          dw = zero          
!         
          do nd=1,ndamp_vrel
!
            id = id_damp_vrel(nd)
            id_rby = nint(dampr(25,id))
            t_start = dampr(17,id)
            t_stop  = dampr(18,id)
!            
            if ((id_rby > 0).and.(tt>=t_start).and.(tt<=t_stop)) then
              igr   = nint(dampr(2,id))
              isk   = nint(dampr(15,id))
              id_func = nint(dampr(26,id))
              freq = dampr(28,id)
              dtini = dampr(29,id)
              im = npby(1,id_rby)
              nsn = igrnod(igr)%nentity
!           
              if (id_func > 0) THEN
                fact = get_u_func(id_func,tt,dxdy)
              else
                fact = one
              endif
!            
              if (dt1 > zero) then
                if (freq > zero) then
                  damp_a(1)  = fact*dampr(3,id)*4*pi*freq
                  damp_a(2)  = fact*dampr(5,id)*4*pi*freq
                  damp_a(3)  = fact*dampr(7,id)*4*pi*freq                
                else
                  if (dtini == zero) then
!                   Initial time step is saved                    
                    dtini = dt1
                    dampr(29,id) = dt1
                  endif  
                  damp_a(1)  = fact*dampr(3,id)*(one/dtini)
                  damp_a(2)  = fact*dampr(5,id)*(one/dtini)
                  damp_a(3)  = fact*dampr(7,id)*(one/dtini)
                endif  
              else      
                damp_a(1)  = zero
                damp_a(2)  = zero
                damp_a(3)  = zero          
              endif
              damp_a2(1)  = dampr(22,id)
              damp_a2(2)  = dampr(23,id)
              damp_a2(3)  = dampr(24,id)
!
!             damping coefficient is saved for timestep computation in dtnodarayl.F
              dampr(30,id) = max(damp_a(1),damp_a(1),damp_a(3))                                         
!
              call damping_vref_sum6_rby(nsn,igr,id_rby,isk,im,                  &
                                         igrnod,ngrnod,v,vr,a,                   &
                                         x,ms,dt1,numnod,tagslv_rby,           &
                                         nrbykin,rby6,rby6_c,weight,lskew,       &
                                         numskw,skew,damp_a,dim,damp,            &
                                         dw,damp_a2,iparit,size_rby6_c)
!              
            endif
!
          enddo
!
          tfext = tfext + dw        
!
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine damping_vref_rby
      end module damping_vref_rby_mod