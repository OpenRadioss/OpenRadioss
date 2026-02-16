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
!||====================================================================
!||    rini135_rb_mod      ../starter/source/elements/joint/rjoint/rini135_rb.F90
!||--- called by ------------------------------------------------------
!||    initia              ../starter/source/elements/initia/initia.F
!||--- calls      -----------------------------------------------------
!||    ancmsg              ../starter/source/output/message/message.F
!||--- uses       -----------------------------------------------------
!||    message_mod         ../starter/share/message_module/message_mod.F
!||    element_mod         ../common_source/modules/elements/element_mod.F90
!||====================================================================
      module rini135_rb_mod
      contains
!||====================================================================
!||
!||    rini135_rb
!||
!||====================================================================
          subroutine rini135_rb( &
          nel      ,nuvar    ,ixr      ,npby     ,      &
          lpby     ,rby      ,uvar     ,itab     ,      &
          igeo     ,ms       ,in       ,nft      ,      &
          numelr   ,nnpby    ,nrbody   ,                &
          slpby    ,numnod   ,npropgi  ,nrby     ,      &
          ltitr)  

!-------------------------------------------------------------------------------
!   M o d u l e s
!-------------------------------------------------------------------------------
          use precision_mod,    only : wp
          use constant_mod,     only : zero, third, em20
          use message_mod
          use names_and_titles_mod, only : nchartitle
          use element_mod,      only : nixr

!-------------------------------------------------------------------------------
!   I m p l i c i t   t y p e s
!-------------------------------------------------------------------------------
          implicit none

!-------------------------------------------------------------------------------
!   D u m m y   A r g u m e n t s
!-------------------------------------------------------------------------------
          integer,                          intent(in)    :: nel       !< Number of elements
          integer,                          intent(in)    :: nuvar     !< Number of user variables
          integer,                          intent(in)    :: nft
          integer,                          intent(in)    :: numelr
          integer,                          intent(in)    :: nnpby
          integer,                          intent(in)    :: nrbody
          integer,                          intent(in)    :: slpby
          integer,                          intent(in)    :: numnod
          integer,                          intent(in)    :: npropgi
          integer,                          intent(in)    :: nrby
          integer,                          intent(in)    :: ltitr                                     !< title length
!integer, dimension(nixr,*),       intent(in)    :: ixr       !< Element connectivity array
          integer, dimension(nixr,numelr),  intent(in)    :: ixr        !< Element connectivity and properties
          integer, dimension(nnpby,nrbody), intent(in)    :: npby      !< Rigid body node info
          integer, dimension(slpby),        intent(in)    :: lpby      !< Rigid body slave node list
!integer, dimension(*),            intent(in)   :: itab      !< Node ID table
          integer, dimension(numnod),       intent(in)    :: itab                !< array for user nodes Ids
          integer, dimension(npropgi),      intent(in)    :: igeo      !< Integer property info

          real(kind=wp), dimension(nrby,nrbody), intent(in) :: rby     !< Rigid body properties
          real(kind=wp), dimension(nuvar,nel),   intent(inout) :: uvar     !< User variables array
          real(kind=wp), dimension(numnod),      intent(in)    :: ms        !< Nodal mass array
          real(kind=wp), dimension(numnod),      intent(in)    :: in        !< Nodal inertia array
!-------------------------------------------------------------------------------
!   L o c a l   V a r i a b l e s
!-------------------------------------------------------------------------------
          integer :: i, iel, j, k, n, nn, nsl, id
          integer, dimension(2) :: m, nod, idrb
          real(kind=wp) :: mass, iner
          
          character(len=nchartitle) :: titr
          integer, parameter :: nodes_count = 2

!===============================================================================
!   B o d y
!===============================================================================
          id = igeo(1)
          titr = '' ! Initialize title string
          CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1),LTITR)

          do iel = 1, nel
            idrb(1) = 0
            idrb(2) = 0

            do i = 1, nodes_count
              m(i)   = 0
              k      = 0
              nod(i) = ixr(1+i, nft + iel)

              !-----------------------------------------------------------------
              ! Search of connected rigid body
              !-----------------------------------------------------------------
              search_loop: do n = 1, nrbody
                nsl = npby(2,n)
                
                ! Case A: Main node of rigid body
                if (npby(1,n) == nod(i)) then 
                  ! Tag for error message - Can't be attached to main node
                  idrb(i) = -n
                  uvar(37+i, iel) = real(n, kind=wp)
                  exit search_loop
                end if

                ! Case B: Slave nodes of rigid body
                do j = 1, nsl
                  nn = lpby(j + k)
                  if (nn == nod(i)) then
                    idrb(i) = n
                    m(i)    = npby(1,n)
                    mass    = rby(14,n)
                    ! Compute average inertia
                    iner    = (rby(10,n) + rby(11,n) + rby(12,n)) * third
                    
                    uvar(33+i, iel) = mass  !< Store rigid body mass
                    uvar(35+i, iel) = iner  !< Store rigid body inertia
                    uvar(37+i, iel) = real(n, kind=wp) !< Store rigid body ID
                    exit search_loop
                  end if
                end do
                
                k = k + nsl
              end do search_loop

              !-----------------------------------------------------------------
              ! Check Search Results and Error Handling
              !-----------------------------------------------------------------
              if (idrb(i) == 0) then      
                ! No rigid bodies found - connected to structural node
                uvar(33+i, iel) = ms(nod(i))
                uvar(35+i, iel) = in(nod(i))
                uvar(37+i, iel) = zero
                
                if (ms(nod(i)) <= em20) then
                  call ancmsg(msgid=1773, msgtype=msgerror, anmode=aninfo_blind_2, &
                              i1=id, c1=titr, i2=ixr(nixr, nft+iel), i3=itab(nod(i)))
                else if (in(nod(i)) <= em20) then
                  call ancmsg(msgid=1774, msgtype=msgwarning, anmode=aninfo_blind_2, &
                              i1=id, c1=titr, i2=ixr(nixr, nft+iel), i3=itab(nod(i)))
                end if
                
              else if (idrb(i) < 0) then
                ! Error: Connected to main node of rigid body
                call ancmsg(msgid=1768, msgtype=msgerror, anmode=aninfo_blind_2, &
                            i1=id, c1=titr, i2=ixr(nixr, nft+iel), i3=itab(nod(i)))
              end if
            end do ! End node loop (nodes_count)
          end do ! End element loop (nel)

        end subroutine rini135_rb

      end module rini135_rb_mod