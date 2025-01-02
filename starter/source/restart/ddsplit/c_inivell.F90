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
      !||    c_inivell_mod   ../starter/source/restart/ddsplit/c_inivell.F90
      !||--- called by ------------------------------------------------------
      !||    ddsplit         ../starter/source/restart/ddsplit/ddsplit.F
      !||====================================================================
      module c_inivell_mod
       contains
  !! \brief get number of splited inivel rst w/ T_start or sensor_id 
      !||====================================================================
      !||    c_inivell      ../starter/source/restart/ddsplit/c_inivell.F90
      !||--- called by ------------------------------------------------------
      !||    ddsplit        ../starter/source/restart/ddsplit/ddsplit.F
      !||--- uses       -----------------------------------------------------
      !||====================================================================
        subroutine c_inivell(                                                 &
                       ngrnod,  ngrbric,    ngrquad,       ngrsh3n,           &
                       igrnod,  igrbric,    igrquad,       igrsh3n,           &
                         proc,      cep,       scep,      nodlocal,           &
                       numnod, ninivelt,   inivel_t,     ninivel_l)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use inivel_mod 
      use groupdef_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      integer , intent(in   )                          :: ninivelt  !< number of global inivel data
      integer , intent(inout)                          :: ninivel_l !< number of local inivel data
      integer , intent(in   )                          :: proc      !< local domain number
      integer , intent(in   )                          :: numnod    !< number of node 
      integer , dimension(numnod), intent(in   )       :: nodlocal  !< global to local node number
      integer,  intent(in   )                          :: scep      !< size of cep
      integer,  dimension(scep)  , intent(in   )       :: cep       !< local domain number of each element
      integer , intent(in   )                          :: ngrnod    !< number node group
      integer , intent(in   )                          :: ngrbric   !< number solid element group
      integer , intent(in   )                          :: ngrquad   !< number quad element group
      integer , intent(in   )                          :: ngrsh3n   !< number tria element group
      type(inivel_), dimension(ninivelt), intent(in  ) :: inivel_t  !< inivel_struc 
      type (group_)  , dimension(ngrnod)               :: igrnod    !< node group array
      type (group_)  , dimension(ngrbric)              :: igrbric   !< solid element group array
      type (group_)  , dimension(ngrquad)              :: igrquad   !< quad element group array
      type (group_)  , dimension(ngrsh3n)              :: igrsh3n   !< tria element group array
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
      integer  :: i,j,id,n,itype,igr,node,nel,ilocal(ninivelt) 
      integer  :: igrs,igbric,igqd,igtria,igbric_loc,igqd_loc,igtria_loc 
!      TYPE(INIVEL_TYPE), DIMENSION(:), ALLOCATABLE :: INIVEL_L
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
       ninivel_l = 0
       do n =1,ninivelt 
         itype = inivel_t(n)%itype
         ilocal(n) = 0
         select case (itype)
           case(0,1,2,3)
             igr = inivel_t(n)%general%grnd_id
             igrs = 0
             do j=1,ngrnod
               if(igr == igrnod(j)%id) igrs=j
             enddo
             if (igrs /= 0 ) then
               do j=1,igrnod(igrs)%nentity
                  node=igrnod(igrs)%entity(j)
                  if (nodlocal(node)>0) then 
                    ilocal(n) = 1
                    cycle
                  end if
               end do 
             end if
           case(4) ! axis
             igr = inivel_t(n)%axis%grnd_id
             igrs = 0
             do j=1,ngrnod
               if(igr == igrnod(j)%id) igrs=j
             enddo
             if (igrs /= 0 ) then
               do j=1,igrnod(igrs)%nentity
                  node=igrnod(igrs)%entity(j)
                  if (nodlocal(node)>0) then 
                    ilocal(n) = 1
                    cycle
                  end if
               end do 
             end if
           case(5) ! fvm
             igbric = inivel_t(n)%fvm%grbric_id
             igqd   = inivel_t(n)%fvm%grqd_id
             igtria = inivel_t(n)%fvm%grtria_id
             if (igbric > 0) then
              igbric_loc = -HUGE(igbric_loc)
              do j = 1,ngrbric
                if (igbric == igrbric(j)%id) igbric_loc = j
              enddo
              do j=1,igrbric(igbric_loc)%nentity
                nel=igrbric(igbric_loc)%entity(j)
                if (cep(nel)==proc) then 
                  ilocal(n) = 1
                  cycle
                end if 
              end do
             endif
             if (igqd > 0) then
              igqd_loc = -HUGE(igqd_loc)
              do j = 1,ngrquad
                if (igqd == igrquad(j)%id) igqd_loc = j
              enddo    
              do j=1,igrquad(igqd_loc)%nentity
                nel=igrquad(igqd_loc)%entity(j)
                if (cep(nel)==proc) then 
                  ilocal(n) = 1
                  cycle
                end if 
              end do
             endif
             if (igtria > 0) then
              igtria_loc = -HUGE(igtria_loc)
              do j = 1,ngrsh3n
                if (igtria == igrsh3n(j)%id) igtria_loc = j
              enddo      
              do j=1,igrsh3n(igtria_loc)%nentity
                nel=igrsh3n(igtria_loc)%entity(j)
                if (cep(nel)==proc) then 
                  ilocal(n) = 1
                  cycle
                end if 
              end do
             endif
         end select
         if (ilocal(n) == 1 ) ninivel_l = ninivel_l + 1
       end do 
       end subroutine c_inivell
      end module c_inivell_mod
