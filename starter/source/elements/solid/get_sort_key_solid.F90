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
!||    get_sort_key_solid_mod   ../starter/source/elements/solid/get_sort_key_solid.F90
!||--- called by ------------------------------------------------------
!||    get_element_group        ../starter/source/elements/get_element_group.F90
!||    sgrhead                  ../starter/source/elements/solid/solide/sgrhead.F
!||====================================================================
      module get_sort_key_solid_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief Initialization of the sorting keys for the solid elements based on their properties, material and part ids, etc...
!! \details
!||====================================================================
!||    get_sort_key_solid   ../starter/source/elements/solid/get_sort_key_solid.F90
!||--- called by ------------------------------------------------------
!||    get_element_group    ../starter/source/elements/get_element_group.F90
!||    sgrhead              ../starter/source/elements/solid/solide/sgrhead.F
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine get_sort_key_solid(numels,nb_key,nummat,numgeo, &
                                      npart,npropgi, &
                                      npropmi,npropm,npropg, &
                                      isms,idtgrs,npreload,trimat,numels8a, &
                                      iparts,isoloff,isolnod,iflag_bpreload,damp_range_part,tagprt_sms, &
                                      sort_key,ixs,igeo, &
                                      ipm,pm,geo, &
                                      mat_param)

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use precision_mod, only : WP
          use element_mod, only : nixs
          use matparam_def_mod , only : matparam_struct_
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
          integer, intent(in) :: numels !< number of solid elements
          integer, intent(in) :: nb_key !< number of keys for sorting
          integer, intent(in) :: nummat !< total number of materials in the model
          integer, intent(in) :: numgeo !< total number of properties in the model
          integer, intent(in) :: npart !< total number of parts in the model
    
          integer, intent(in) :: npropgi !< number of integer property parameters
          integer, intent(in) :: npropmi !< number of integer material parameters
          integer, intent(in) :: npropm !< number of real material parameters
          integer, intent(in) :: npropg !< number of real property parameters

          integer, intent(in) :: isms !< global flag for sms in the model
          integer, intent(in) :: idtgrs !< flag for using part tags for sms
          integer, intent(in) :: npreload !< number of preload steps in the model

          integer, intent(inout) :: trimat !< number of sub-materials for the law 151
          integer, intent(inout) :: numels8a !< number of hexa 8 elements with hourglass control 14 or 222 (to be used for trimat)
          integer, dimension(numels), intent(in) :: iparts !< array of part ids for each element
          integer, dimension(numels), intent(in) :: isoloff  !< deactivated solid flag
          integer, dimension(numels), intent(in) :: isolnod  !< array of element types (hexa8, hexa20, etc...)
          integer, dimension(numels), intent(in) :: iflag_bpreload !< array of flags for elements with preload conditions
          integer, dimension(npart), intent(in) :: damp_range_part !< array of damping range for each part
          integer, dimension(isms*npart), intent(in) :: tagprt_sms !< array of sms tags for each element's part

          integer, dimension(nb_key,numels), intent(inout) :: sort_key !< array to store the sorting keys for each element
          integer, dimension(nixs,numels), intent(in) :: ixs !< solid connectivity array
          integer, dimension(npropgi,numgeo), intent(inout) :: igeo !< property parameters array
          integer, dimension(npropmi,nummat), intent(in) :: ipm          
          
          
          real(kind=WP), dimension(npropg,numgeo), intent(in) :: geo !< property parameters array
          real(kind=WP), dimension(npropm,nummat), intent(in) :: pm !< material parameters array

          type(matparam_struct_), dimension(nummat), intent(in) :: mat_param !< array of material parameters structures          
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,il,mid,mln,nuvar,pid,iso,npn,jhbe,jpor,iplast,icpre,icstr,irep,istrain
          integer :: nfail0,nloc_fail,ieos,ivisc0,nlay,isvis,jclos,jale_from_mat
          integer :: igt,jcvt,itet4,itet10,ipart_sph,nfail,iint,imat
          integer :: jale_from_prop,jale,jlag,jeul,jthe,jtur,issn,irb,jsms,iboltp
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------
          integer, external :: my_shiftl
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

! ----------------------------------------------------------------------------------------------------------------------
          sort_key(1:nb_key,1:numels) = 0

          do i=1,numels
            npn = 1
            jhbe = 1
            jpor = 0
            mid = ixs(1,i) ! get the material id for the current element
            mln = nint(pm(19,abs(mid))) ! get the material law number for the current element's material
            if (mln == 51) trimat = 4
            if (mid < 0) then
              if(mln==6.and.jpor/=2) mln=17
              if(mln==46) mln=47
              mid = iabs(mid)
            endif
            if (mln == 36 .or. mln == 47) then
              nuvar = ipm(8,mid) ! get the number of variables for the current element's material
            else
              nuvar = 0
            endif
            pid = ixs(10,i) ! get the property id for the current
            iso = isolnod(i) ! get the type of element (hexa 8 nodes, hexa 20 nodes, etc...)
            iplast = 1
            icpre = 0
            icstr = 0
            irep = 0
            istrain = 0
            nfail0 = mat_param(mid)%nfail ! get the number of failure criteria
            nloc_fail = mat_param(mid)%nloc ! get the number of local failure criteria
            ieos = 0
            ivisc0 = 0
            nlay = 1
            isvis = 0
            jclos = 0      
            jtur = 0      
            if (pid /= 0) then
              npn = igeo(4,pid) ! get the number of integration points
              issn = iabs(igeo(5,pid))
              irep = igeo(6,pid)
              jhbe = igeo(10,pid)
              igt = igeo(11,pid)
              istrain = igeo(12,pid)
              icpre = iabs(igeo(13,pid))
              icstr = igeo(14,pid)
              iint = igeo(15,pid)
              jcvt = iabs(igeo(16,pid))
              itet4 = igeo(20,pid)
              itet10 = igeo(50,pid)
              if (igt == 22) then
                nlay = igeo(30,pid)
                do il=1,nlay
                  imat = igeo(100+il,pid)
                  nfail0 = max(nfail0,mat_param(imat)%nfail)
                  if (mat_param(imat)%ivisc > 0) ivisc0 = 1
                enddo
              else if (mat_param(mid)%ivisc > 0) then
                ivisc0 = 1
              endif
              igeo(34,pid) = ivisc0
              if(igt/=15) iplast = igeo(9,pid)
              if(igt==15) jpor = 2*nint(geo(28,pid))
              if (geo(130,pid)>0.) jclos = 1
              if (geo(16,pid)/=0. .or. geo(17,pid)/=0.) isvis = 1
            endif
            if((jhbe==14.or.jhbe==222).and.iso==8) numels8a = numels8a + 1
            if (jhbe == 12) jhbe = 4
            if (jhbe == 2) jhbe = 0

            jale_from_mat = nint(pm(72,mid))
            jale_from_prop = igeo(62,pid)
            jale = max(jale_from_mat, jale_from_prop) ! if inconsistent, error message was displayed in PART reader
            jlag = 0
            if (jale == 0 .and. mln /= 18) jlag = 1
            jeul = 0
            if (jale == 2) then
              jale = 0
              jeul = 1
            else if (jale == 3) then
              jlag = 1
              jale = 1
            endif
            if (mln /= 50) jtur = nint(pm(70,mid))
            jthe = nint(pm(71,mid))
            if (jlag == 0 .and. pid /= 0) issn = 4

            irb = isoloff(i)

            jsms = 0
            if(isms/=0) then
              if (idtgrs/=0) then
                if (tagprt_sms(iparts(i))/=0) jsms = 1
              else
                jsms = 1
              endif
            endif
            
            ieos = ipm(4,mid)
            ipart_sph = igeo(38,pid)
            igeo(35,pid) = isvis
            iboltp = 0
            if (npreload > 0) then
              iboltp = iflag_bpreload(i)
            endif
            if(iso==16) iso=21 ! classify shell16 after brick20
            igt = my_shiftl(igt,0)
            jsms = my_shiftl(jsms,26)
            iso = my_shiftl(iso,27) ! caution: iso must remain in the strongest weight for the sorting key
            sort_key(1,i) = iso + jsms + igt

            sort_key(2,i) = ipart_sph ! must remain alone for this key (part index)

            jtur = my_shiftl(jtur,1)
            jeul = my_shiftl(jeul,2)
            jlag = my_shiftl(jlag,3)
            jale = my_shiftl(jale,4)
            issn = my_shiftl(issn,5)
            jhbe = my_shiftl(jhbe,9)
            jpor = my_shiftl(jpor,13)
            irb = my_shiftl(irb,18)
            mln = my_shiftl(mln,22)
            sort_key(3,i) = mln + jhbe + issn + jale + jlag + jeul + jtur + jthe + jpor + irb

            npn = my_shiftl(npn,3)
            iplast = my_shiftl(iplast,13)
            icpre = my_shiftl(icpre,16)
            icstr = icstr/100 + 2*mod(icstr/10,10) + 4*mod(icstr,10)
            icstr = my_shiftl(icstr,18)
            irep = my_shiftl(irep,20)
            jcvt = my_shiftl(jcvt,22)
            iint = my_shiftl(iint,24)
            istrain = my_shiftl(istrain,26)
            itet4 = my_shiftl(itet4,27)
            nfail = my_shiftl(nfail0,29)
            sort_key(4,i) = jclos + npn + iplast + icpre + icstr + irep + iint + jcvt + istrain + itet4 + nfail

            sort_key(5,i) = mid ! must remain alone for this key (material index)
            sort_key(6,i) = pid ! must remain alone for this key (property index)

            ieos = my_shiftl(ieos,0)
            ivisc0 = my_shiftl(ivisc0,4)
            nuvar = my_shiftl(nuvar,5)
            isvis = my_shiftl(isvis,15)
            iboltp = my_shiftl(iboltp,16)
            itet10 = my_shiftl(itet10,17)
            nloc_fail = my_shiftl(nloc_fail,19)
            sort_key(7,i) = ieos + ivisc0 + nuvar + isvis + iboltp + itet10 + nloc_fail ! must remain alone for this key (flags)

            sort_key(8,i) = damp_range_part(iparts(i))
          enddo

          return

        end subroutine get_sort_key_solid
      end module get_sort_key_solid_mod
