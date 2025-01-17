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
      !||    inivel_init_mod   ../engine/source/loads/general/inivel/inivel_init.F90
      !||--- called by ------------------------------------------------------
      !||    resol_init        ../engine/source/engine/resol_init.F
      !||====================================================================
      module inivel_init_mod
!        
       contains
  !! \brief initialization ids of inivel data
      !||====================================================================
      !||    inivel_init    ../engine/source/loads/general/inivel/inivel_init.F90
      !||--- called by ------------------------------------------------------
      !||    resol_init     ../engine/source/engine/resol_init.F
      !||--- uses       -----------------------------------------------------
      !||    groupdef_mod   ../common_source/modules/groupdef_mod.F
      !||    inivel_mod     ../common_source/modules/inivel_mod.F90
      !||    sensor_mod     ../common_source/modules/sensor_mod.F90
      !||====================================================================
        subroutine inivel_init(                                               &
                       ngrnod,  ngrbric,    ngrquad,       ngrsh3n,           &
                       igrnod,  igrbric,    igrquad,       igrsh3n,           &
                       numskw,    liskn,      iskwn,       numfram,           &
                       iframe, ninivelt,   inivel_t,       sensors)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use inivel_mod 
      use groupdef_mod
      use sensor_mod
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
      integer , intent(inout)                          :: ninivelt  !< dimension of inivel_t
      integer , intent(inout)                          :: numskw    !< number of skew
      integer , intent(inout)                          :: numfram   !< number of frame
      integer , intent(inout)                          :: liskn     !< 1er dimension of iskwn
      integer , intent(in   )                          :: ngrnod    !< number node group
      integer , intent(in   )                          :: ngrbric   !< number solid element group
      integer , intent(in   )                          :: ngrquad   !< number quad element group
      integer , intent(in   )                          :: ngrsh3n   !< number tria element group
      integer , intent(in  ),dimension(liskn,numskw+1) :: iskwn     !< iskew skew id data
      integer , intent(in  ),dimension(liskn,numfram+1):: iframe    !< iframe frame id data
      type(inivel_), dimension(ninivelt),intent(inout) :: inivel_t  !< inivel_struc 
      type (group_)  , dimension(ngrnod)               :: igrnod    !< node group array
      type (group_)  , dimension(ngrbric)              :: igrbric   !< solid element group array
      type (group_)  , dimension(ngrquad)              :: igrquad   !< quad element group array
      type (group_)  , dimension(ngrsh3n)              :: igrsh3n   !< tria element group array
      type (sensors_) ,intent(in  )                    :: sensors
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
      integer  :: i,j,id,n,itype,igr,node,isk,isk1,ifra,ifra1 
      integer  :: igrs,igbric,igqd,igtria,igbric_loc,igqd_loc,igtria_loc,is,isens 
      my_real  :: rtmp(6)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
       do n =1,ninivelt 
         itype = inivel_t(n)%itype
         select case (itype)
           case(0,1,2,3)
             isk = inivel_t(n)%general%skew_id 
             if ( isk  >  0) then
               isk1 = 0
               do j=0,numskw
                 if(isk == iskwn(4,j+1)) isk1 = j+1
               enddo
               inivel_t(n)%general%skew_id = isk1
             endif 
!             
             igr = inivel_t(n)%general%grnd_id
             igrs = 0
             do j=1,ngrnod
               if(igr == igrnod(j)%id) igrs=j
             enddo
             inivel_t(n)%general%grnd_id = igrs 
!            
             isens = inivel_t(n)%general%sensor_id 
             is = 0
             do j=1,sensors%nsensor
               if(sensors%sensor_tab(j)%sens_id==isens) is=j
             enddo
             inivel_t(n)%general%sensor_id = is 
           case(4) ! axis
             ifra = inivel_t(n)%axis%frame_id 
             if ( ifra  >  0) then
               ifra1 = 0
               do j=1,numfram
                 if(ifra == iframe(4,j+1)) ifra1 = j+1
               enddo
               inivel_t(n)%axis%frame_id = ifra1
             endif
             igr = inivel_t(n)%axis%grnd_id
             igrs = 0
             do j=1,ngrnod
               if(igr == igrnod(j)%id) igrs=j
             enddo 
             inivel_t(n)%axis%grnd_id = igrs
             isens = inivel_t(n)%axis%sensor_id 
             is = 0
             do j=1,sensors%nsensor
               if(sensors%sensor_tab(j)%sens_id==isens) is=j
             enddo
             inivel_t(n)%axis%sensor_id = is 
          case(5) ! fvm
             isk = inivel_t(n)%fvm%skew_id 
             if ( isk  >  0) then
               isk1 = 0
               do j=0,numskw
                 if(isk == iskwn(4,j+1)) isk1 = j
               enddo
               inivel_t(n)%fvm%skew_id = isk1
             endif
             igbric = inivel_t(n)%fvm%grbric_id
             igqd   = inivel_t(n)%fvm%grqd_id
             igtria = inivel_t(n)%fvm%grtria_id
             if (igbric /= 0) then
                igbric_loc = 0
                do j = 1,ngrbric
                   if (igbric == igrbric(j)%id) igbric_loc = j
                enddo
                inivel_t(n)%fvm%grbric_id = igbric_loc
             endif
             if (igqd /= 0) then
                igqd_loc = 0
                do j = 1,ngrquad
                   if (igqd == igrquad(j)%id) igqd_loc = j
                enddo    
                inivel_t(n)%fvm%grqd_id = igqd_loc
             endif
             if (igtria /= 0) then
                igtria_loc = 0
                do j = 1,ngrsh3n
                   if (igtria == igrsh3n(j)%id) igtria_loc = j
                enddo      
                inivel_t(n)%fvm%grtria_id = igtria_loc
             endif
             isens = inivel_t(n)%fvm%sensor_id 
             is = 0
             do j=1,sensors%nsensor
               if(sensors%sensor_tab(j)%sens_id==isens) is=j
             enddo
             inivel_t(n)%fvm%sensor_id = is 
         end select
       end do 
       end subroutine inivel_init
      end module inivel_init_mod
