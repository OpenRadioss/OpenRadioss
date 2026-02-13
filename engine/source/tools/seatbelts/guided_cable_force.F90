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
!||    guided_cable_force_mod            ../engine/source/tools/seatbelts/guided_cable_force.F90
!||--- called by ------------------------------------------------------
!||    kine_seatbelt_force               ../engine/source/tools/seatbelts/kine_seatbelt_force.F
!||--- calls      -----------------------------------------------------
!||    compute_contact_force_guide       ../engine/source/tools/seatbelts/compute_contact_force_guide.F90
!||--- uses       -----------------------------------------------------
!||    compute_contact_force_guide_mod   ../engine/source/tools/seatbelts/compute_contact_force_guide.F90
!||    constant_mod                      ../common_source/modules/constant_mod.F
!||    precision_mod                     ../common_source/modules/precision_mod.F90
!||    seatbelt_mod                      ../common_source/modules/seatbelt_mod.F
!||====================================================================
      module guided_cable_force_mod
      implicit none
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!
!=======================================================================================================================
!!\brief This suborutine compute penalty forces for guided cable
!=======================================================================================================================
!
  subroutine guided_cable_force(a, stifn, iresp, numnod, x, xdp, &
                                v, dt1, ms0, n_anchor_remote_send, &
                                flag_guided_cable_update,buf_exch)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod,                         only : zero, one, em10, em20, half
          use precision_mod,                        only : WP
          use seatbelt_mod,                         only : nguided_cable, guide, anchor_remote_send
          use compute_contact_force_guide_mod,      only : compute_contact_force_guide
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer,                                   intent(in)    :: numnod                     !< number of nodes
          integer,                                   intent(in)    :: iresp                      !< simple precision flag
          integer,                                   intent(in)    :: n_anchor_remote_send       !< number of remote anchor nodes to send
          integer,                                   intent(inout) :: flag_guided_cable_update   !< flag for guided cable update
          real(kind=WP),                             intent(in)    :: dt1                        !< time step
          real(kind=WP),                             intent(in)    :: x(3,numnod)                !< nodal positions
          real(kind=WP),                             intent(in)    :: v(3,numnod)                !< nodal velocities  
          real(kind=WP),                             intent(in)    :: ms0(numnod)                !< nodal mass time 0
          real(kind=WP),                             intent(inout) :: a(3,numnod)                !< nodal accelerations
          real(kind=WP),                             intent(inout) :: stifn(numnod)              !< nodal stiffness  
          real(kind=WP),                             intent(inout) :: buf_exch(n_anchor_remote_send,4) !< buffer for exchange of remote contributions 
          double precision,                          intent(in)    :: xdp(3,numnod)              !< nodal positions double precision
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ---------------------------------------------------------------------------------------------------------------------
          integer :: i, j, k, l, node1, node2, node3,anchor_node
          integer :: main1, main2, iseg
          real(kind=WP) :: forc(3)
          real(kind=WP) :: stiff
          real(kind=WP) :: visc
          real(kind=WP) :: fric
          real(kind=WP) :: x1(3), x2(3), xa(3), x3(3)
          real(kind=WP) :: seg_vec(3), veca(3), seg_len2, alpha, alpha1, alpha2
          real(kind=WP) :: xproj(3), dist1, dist2
          real(kind=WP) :: forc_norm, forc_tan, forc_tan_ad, forc_ad(3)
          real(kind=WP) :: mass_harm, stfac, stiff_stab, adamp
          real(kind=WP) :: one_over_dt1          
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!
          l = 1           
          if (dt1 > zero) then
            one_over_dt1 = one / dt1
          else
            one_over_dt1 = zero
          endif
!
          do i=1,nguided_cable
!
            fric   = guide(i)%fric
            stfac  = guide(i)%stfac            
!            
            do j=1,guide(i)%ncont
!
!              if (abs(guide(i)%cont(j)%update)==2) flag_slipring_update = flag_slipring_update + 2
              anchor_node = guide(i)%cont(j)%anchor_node
              node1 = guide(i)%cont(j)%node(1)
              node2 = guide(i)%cont(j)%node(2)
              node3 = guide(i)%cont(j)%node(3) 
              if (iresp==1) then
                x1(1:3) = xdp(1:3,node1)
                x2(1:3) = xdp(1:3,node2)
                x3(1:3) = xdp(1:3,node3)
                xa(1:3) = xdp(1:3,anchor_node)
              else
                x1(1:3) = x(1:3,node1)
                x2(1:3) = x(1:3,node2)
                x3(1:3) = x(1:3,node3)
                xa(1:3) = x(1:3,anchor_node)
              endif 
!                
!-------------------------------------------------------------------------------------------          
!             Projection of anchor node  on the segment defined by node1 and node2
!-------------------------------------------------------------------------------------------          
              seg_vec(1:3) = x2(1:3) - x1(1:3)
              veca(1:3) = xa(1:3) - x1(1:3)
              seg_len2 = seg_vec(1)**2 + seg_vec(2)**2 + seg_vec(3)**2
              if (seg_len2 > zero) then
                alpha1 = (seg_vec(1)*veca(1)+seg_vec(2)*veca(2)+seg_vec(3)*veca(3))/seg_len2
              else
               alpha1 = zero
              endif
              xproj(1:3) = x1(1:3) + alpha1 * (x2(1:3) - x1(1:3)) - xa(1:3)
              if ((xproj(1))**2 + (xproj(2))**2 + (xproj(3))**2  > zero) then
                dist1 = sqrt( (xproj(1))**2 + (xproj(2))**2 + (xproj(3))**2 )
              else
                dist1 = zero
              endif
!          
!-------------------------------------------------------------------------------------------          
!             Projection of anchor node  on the segment defined by node2 and node3       
!-------------------------------------------------------------------------------------------       
              seg_vec(1:3) = x3(1:3) - x2(1:3)
              veca(1:3) = xa(1:3) - x2(1:3)
              seg_len2 = seg_vec(1)**2 + seg_vec(2)**2 + seg_vec(3)**2
              if (seg_len2 > zero) then
                alpha2 = (seg_vec(1)*veca(1)+seg_vec(2)*veca(2)+seg_vec(3)*veca(3))/seg_len2
              else
                alpha2 = zero
              endif
              xproj(1:3) = x2(1:3) + alpha2 * (x3(1:3) - x2(1:3)) - xa(1:3)
              if ((xproj(1))**2 + (xproj(2))**2 + (xproj(3))**2  > zero) then
                dist2 = SQRT( (xproj(1))**2 + (xproj(2))**2 + (xproj(3))**2 )
              else
                dist2 = zero
              endif 
!          
!-------------------------------------------------------------------------------------------          
!             Determination of closest segment and storage of the projection data    
!------------------------------------------------------------------------------------------- 
              if (guide(i)%cont(j)%active_segment == 1) then                        
                main1 = node1
                main2 = node2
                alpha = alpha1
                if ((alpha1 > one) .and. (alpha2 > zero)) then
                  main1 = node2
                  main2 = node3
                  alpha = alpha2
                  guide(i)%cont(j)%active_segment = 2           
                endif       
              else  
                main1 = node2
                main2 = node3
                alpha = alpha2
                if ((alpha2 < zero) .and. (alpha1 < one)) then          
                  main1 = node1
                  main2 = node2
                  alpha = alpha1
                  guide(i)%cont(j)%active_segment = 1 
                endif             
              endif          
              alpha = max(zero, min(one, alpha))   
!              
!-------------------------------------------------------------------------------------------               
!             Segment vector normalized      
!-------------------------------------------------------------------------------------------                   
              seg_vec(1:3) = X(1:3,main1) - X(1:3,main2)
              seg_len2 = seg_vec(1)**2 + seg_vec(2)**2 + seg_vec(3)**2
              if (seg_len2 > zero) then
                seg_len2 = SQRT(seg_len2)
                seg_vec(1:3) = seg_vec(1:3) / seg_len2
              else
                seg_vec(1:3) = zero
              endif
!          
!-------------------------------------------------------------------------------------------          
!             Computation of normal contact stiffness and viscosity    
!------------------------------------------------------------------------------------------- 
!
              if (guide(i)%istiff == 1) then
!               Istiff=0 - > stiffness computed from time step                
                if (ms0(anchor_node) > em10) then
                  stiff = 0.1*min(ms0(main1), ms0(main2), ms0(anchor_node))*(one_over_dt1**2)
                else
                  stiff = 0.1*min(ms0(main1), ms0(main2))*(one_over_dt1**2)
                end if      
              elseif (guide(i)%istiff == 2) then
!               Istiff=1 - > stiffness defined by stfac
                stiff = one
              elseif (guide(i)%istiff == 3) then
!               Istiff=2 - > stiffness defined by average of nodal stiffness
                stiff = half*(stifn(main1)+stifn(main2))     
              elseif (guide(i)%istiff == 4) then
!               Istiff=3 - > stiffness defined by max of nodal stiffness
                stiff = max(stifn(main1),stifn(main2))                            
              elseif (guide(i)%istiff == 5) then
!               Istiff=4 - > stiffness defined by min of nodal stiffness
                stiff = min(stifn(main1),stifn(main2))   
              elseif (guide(i)%istiff == 6) then
!               Istiff=5 - > stiffness defined by equivalent stiffness in serie of nodal stiffness
                stiff = (stifn(main1)*stifn(main2))/(em20+stifn(main1)+stifn(main2))   
              end if            
              stiff = stiff * stfac
!
              adamp = 0.05
              mass_harm = ms0(main1)*ms0(main2)/(em20+ms0(main1)+ms0(main2))
              visc  = adamp*sqrt(stiff*mass_harm)
!              
!-------------------------------------------------------------------------------------------          
!             Computation of adherence force    
!------------------------------------------------------------------------------------------- 
!
              forc_ad(1:3) = guide(i)%cont(j)%forc_ad(1:3)
              call compute_contact_force_guide(alpha, anchor_node, main1, main2, seg_vec, numnod, x, v, stiff,    &
                                               visc, fric, dt1, forc_ad, forc_norm, forc_tan_ad, "ADHE")

              forc_ad(1:3) = forc_tan_ad*seg_vec(1:3)
!               
!-------------------------------------------------------------------------------------------          
!             Computation of sliding without adherence
!-------------------------------------------------------------------------------------------                    
!          
              call compute_contact_force_guide(alpha, anchor_node, main1, main2, seg_vec, numnod, x, v, stiff,    &
                                               visc, fric, dt1, forc, forc_norm, forc_tan, "SLID")
!
              if (abs(forc_tan) > abs(forc_tan_ad)) then
                guide(i)%cont(j)%forc_ad(1:3) = forc_ad(1:3)
              elseif (abs(forc_tan_ad) > zero) then
                 guide(i)%cont(j)%forc_ad(1:3) = (abs(forc_tan)/abs(forc_tan_ad))*forc_ad(1:3)
              else
                guide(i)%cont(j)%forc_ad(1:3) = zero
              endif
              forc(1:3) =  forc(1:3) + guide(i)%cont(j)%forc_ad(1:3)
!
!-------------------------------------------------------------------------------------------          
!             Storage of data for output
!------------------------------------------------------------------------------------------- 
              guide(i)%cont(j)%forc(1:3) = -forc(1:3)
!                  
!-------------------------------------------------------------------------------------------          
!             Distribution of forces   
!-------------------------------------------------------------------------------------------                      
              a(1:3,anchor_node)=a(1:3,anchor_node)-forc(1:3)
!             
              a(1:3,main1)=a(1:3,main1)+forc(1:3)*(1.0-alpha)
              a(1:3,main2)=a(1:3,main2)+forc(1:3)*alpha
!          
              stiff_stab=stiff*(adamp+sqrt(one+adamp**2))**2
              stifn(anchor_node)=stifn(anchor_node)+stiff_stab
              stifn(main1)=stifn(main1)+stiff_stab*(1.0-alpha)
              stifn(main2)=stifn(main2)+stiff_stab*alpha 
!          
!-------------------------------------------------------------------------------------------         
!             Assembly of forces in SPMD   
!------------------------------------------------------------------------------------------- 
!  
              if (guide(i)%n_remote_proc > 0) then
                if (guide(i)%cont(j)%node_iremote(4) > 0) then
!                 contribution on anchor node stored to be exchanged to remote anchor node
                  l = guide(i)%cont(j)%node_iremote(4)
                  buf_exch(l,1) = -forc(1)
                  buf_exch(l,2) = -forc(2)
                  buf_exch(l,3) = -forc(3)
                  buf_exch(l,4) = stiff_stab
                end if  
                iseg = guide(i)%cont(j)%active_segment         
                if (guide(i)%cont(j)%node_iremote(iseg) > 0) then
!                 contribution on guided cable nodes stored to be exchanged to remote guided cable nodes
                  l = guide(i)%cont(j)%node_iremote(iseg)
                  buf_exch(l,1) = forc(1)*(1.0 - alpha)
                  buf_exch(l,2) = forc(2)*(1.0 - alpha)
                  buf_exch(l,3) = forc(3)*(1.0 - alpha)
                  buf_exch(l,4) = stiff_stab*(1.0 - alpha)
                end if
                if (guide(i)%cont(j)%node_iremote(iseg+1) > 0) then
!                 contribution on guided cable nodes stored to be exchanged to remote guided cable nodes
                  l = guide(i)%cont(j)%node_iremote(iseg+1)
                  buf_exch(l,1) = forc(1)*alpha
                  buf_exch(l,2) = forc(2)*alpha
                  buf_exch(l,3) = forc(3)*alpha
                  buf_exch(l,4) = stiff_stab*alpha
                end if
              endif  
!
!-------------------------------------------------------------------------------------------  
!             switch of nodes for next cycle
!-------------------------------------------------------------------------------------------            
!          
              if ((alpha1 < 0.1) .and. (guide(i)%cont(j)%node_next(1) > 0)) then
                guide(i)%cont(j)%node_next(2) = guide(i)%cont(j)%node(3)
                guide(i)%cont(j)%node(1) = guide(i)%cont(j)%node_next(1)
                guide(i)%cont(j)%node(2) = node1
                guide(i)%cont(j)%node(3) = node2
                guide(i)%cont(j)%active_segment = 2
                guide(i)%cont(j)%update = 1
                flag_guided_cable_update = 1
                guide(i)%cont(j)%node_iremote(3) = guide(i)%cont(j)%node_iremote(2)
                guide(i)%cont(j)%node_iremote(2) = guide(i)%cont(j)%node_iremote(1)
!               find iremote index for node node_next(1)
                do k=1,n_anchor_remote_send
                  if (guide(i)%cont(j)%node_next(1) == anchor_remote_send%node(k)) then
                    guide(i)%cont(j)%node_iremote(1) = k
                  end if
                end do 
              end if
!
              if (((one - alpha2) < 0.1) .and. (guide(i)%cont(j)%node_next(2) > 0)) then
                guide(i)%cont(j)%node_next(1) = guide(i)%cont(j)%node(1)
                guide(i)%cont(j)%node(1) = node2
                guide(i)%cont(j)%node(2) = node3
                guide(i)%cont(j)%node(3) = guide(i)%cont(j)%node_next(2)
                guide(i)%cont(j)%active_segment = 1
                guide(i)%cont(j)%update = 2
                flag_guided_cable_update = 1
                guide(i)%cont(j)%node_iremote(1) = guide(i)%cont(j)%node_iremote(2)
                guide(i)%cont(j)%node_iremote(2) = guide(i)%cont(j)%node_iremote(3)
!               find iremote index for new node_next(2)
                do k=1,n_anchor_remote_send
                  if (guide(i)%cont(j)%node_next(2) == anchor_remote_send%node(k)) then
                    guide(i)%cont(j)%node_iremote(3) = k
                  end if
                end do 
              end if
!
            end do     
          end do
!          
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine guided_cable_force
      end module guided_cable_force_mod
