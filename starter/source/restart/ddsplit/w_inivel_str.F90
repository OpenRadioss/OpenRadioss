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
      !||    w_inivel_str_mod   ../starter/source/restart/ddsplit/w_inivel_str.F90
      !||--- called by ------------------------------------------------------
      !||    ddsplit            ../starter/source/restart/ddsplit/ddsplit.F
      !||====================================================================
      module w_inivel_str_mod
       contains
  !! \brief write splited inivel rst data for engine 
      !||====================================================================
      !||    w_inivel_str   ../starter/source/restart/ddsplit/w_inivel_str.F90
      !||--- called by ------------------------------------------------------
      !||    ddsplit        ../starter/source/restart/ddsplit/ddsplit.F
      !||--- calls      -----------------------------------------------------
      !||--- uses       -----------------------------------------------------
      !||====================================================================
        subroutine w_inivel_str(                                              &
                       ngrnod,  ngrbric,    ngrquad,       ngrsh3n,           &
                       igrnod,  igrbric,    igrquad,       igrsh3n,           &
                         proc,      cep,       scep,      nodlocal,           &
                       numnod, ninivelt,   inivel_t,        len_ia,           &
                      len_am)
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
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      integer , intent(in   )                          :: ninivelt  !< number of inivel_struc
      integer , intent(inout)                          :: len_ia    !< accumulative interger length
      integer , intent(inout)                          :: len_am    !< accumulative floating length
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
      integer  :: i,j,id,n,itype,igr,node,nel,ilocal(ninivelt),itmp(10),nl,nr 
      integer  :: igrs,igbric,igqd,igtria,igbric_loc,igqd_loc,igtria_loc 
      my_real  :: rtmp(6)
!      TYPE(INIVEL_TYPE), DIMENSION(:), ALLOCATABLE :: INIVEL_L
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
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
       end do 
! write rst inivel_struc
       do n =1,ninivelt 
         if (ilocal(n) == 1) then
               nl = 0
               nr = 0
           itype = inivel_t(n)%itype 
           itmp(1) = inivel_t(n)%id
           itmp(2) = itype
           call write_i_c(itmp,2)
           select case (itype)
             case(0,1,2,3)
               itmp(1) = inivel_t(n)%general%type    
               itmp(2) = inivel_t(n)%general%skew_id 
               itmp(3) = inivel_t(n)%general%grnd_id 
               itmp(4) = inivel_t(n)%general%sensor_id 
               rtmp(1) = inivel_t(n)%general%vx 
               rtmp(2) = inivel_t(n)%general%vy 
               rtmp(3) = inivel_t(n)%general%vz 
               rtmp(4) = inivel_t(n)%general%tstart 
               nl = 4
               nr = 4
             case(4) ! axis
               itmp(1) = inivel_t(n)%axis%dir 
               itmp(2) = inivel_t(n)%axis%frame_id 
               itmp(3) = inivel_t(n)%axis%grnd_id 
               itmp(4) = inivel_t(n)%axis%sensor_id 
               rtmp(1) = inivel_t(n)%axis%vx 
               rtmp(2) = inivel_t(n)%axis%vy 
               rtmp(3) = inivel_t(n)%axis%vz 
               rtmp(4) = inivel_t(n)%axis%vr 
               rtmp(5) = inivel_t(n)%axis%tstart
               nl = 4
               nr = 5
             case(5) ! fvm
               itmp(1) = inivel_t(n)%fvm%skew_id 
               itmp(2) = inivel_t(n)%fvm%grbric_id 
               itmp(3) = inivel_t(n)%fvm%grqd_id   
               itmp(4) = inivel_t(n)%fvm%grtria_id 
               itmp(5) = inivel_t(n)%fvm%sensor_id 
               rtmp(1) = inivel_t(n)%fvm%vx 
               rtmp(2) = inivel_t(n)%fvm%vy 
               rtmp(3) = inivel_t(n)%fvm%vz 
               rtmp(4) = inivel_t(n)%fvm%tstart
               nl = 5
               nr = 4
           end select
           call write_i_c(itmp,nl)
           call write_db(rtmp,nr)
           len_ia = len_ia +2 + nl
           len_am = len_am + nr
         end if
       end do 
       end subroutine w_inivel_str
      end module w_inivel_str_mod
