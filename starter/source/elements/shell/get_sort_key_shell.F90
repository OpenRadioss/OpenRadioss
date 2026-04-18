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
!||    get_sort_key_shell_mod   ../starter/source/elements/shell/get_sort_key_shell.F90
!||--- called by ------------------------------------------------------
!||    cgrhead                  ../starter/source/elements/shell/coque/cgrhead.F
!||    get_element_group        ../starter/source/elements/get_element_group.F90
!||====================================================================
      module get_sort_key_shell_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief Initialization of the sorting keys for the shell elements based on their properties, material and part ids, etc...
!! \details
!||====================================================================
!||    get_sort_key_shell   ../starter/source/elements/shell/get_sort_key_shell.F90
!||--- called by ------------------------------------------------------
!||    cgrhead              ../starter/source/elements/shell/coque/cgrhead.F
!||    get_element_group    ../starter/source/elements/get_element_group.F90
!||--- uses       -----------------------------------------------------
!||    stack_mod            ../starter/share/modules1/stack_mod.F
!||====================================================================
        subroutine get_sort_key_shell(numelc,numeltg,nb_key,nummat,numgeo, &
                                      npart,nadmesh,lipart1,npropgi, &
                                      npropmi,npropm,npropg,icrack3d, &
                                      isms,idtgrs,sh4tree_dim1,sh4tree_dim2, &
                                      ipartc,isheoff,damp_range_part,tagprt_sms, &
                                      ipart,iworksh,sort_key,ixc,igeo, &
                                      ipm,sh4tree,pm,geo, &
                                      stack,mat_param)

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use precision_mod, only : WP
          use element_mod, only : nixc
          use matparam_def_mod , only : matparam_struct_
          use stack_mod
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
          integer, intent(in) :: numelc !< number of shell elements
          integer, intent(in) :: numeltg !< number of shell3node elements
          integer, intent(in) :: nb_key !< number of keys for sorting
          integer, intent(in) :: nummat !< total number of materials in the model
          integer, intent(in) :: numgeo !< total number of properties in the model
          integer, intent(in) :: npart !< total number of parts in the model
          integer, intent(in) :: nadmesh !< flag for adaptive meshing          
          integer, intent(in) :: lipart1 !< leading dimension of the ipart array
          integer, intent(in) :: npropgi !< number of integer property parameters
          integer, intent(in) :: npropmi !< number of integer material parameters
          integer, intent(in) :: npropm !< number of real material parameters
          integer, intent(in) :: npropg !< number of real property parameters
          integer, intent(inout) :: icrack3d !< global flag for 3D cracking
          integer, intent(in) :: isms !< global flag for sms in the model
          integer, intent(in) :: idtgrs !< flag for using part tags for sms

          integer, intent(in) :: sh4tree_dim1 !< first dimension of the sh4tree array for shells (adaptive meshing)
          integer, intent(in) :: sh4tree_dim2 !< second dimension of the sh4tree array for shells (adaptive meshing)

          integer, dimension(numelc), intent(in) :: ipartc !< array of part ids for each element
          integer, dimension(numelc), intent(in) :: isheoff  !< deactivated shell flag
          integer, dimension(npart), intent(in) :: damp_range_part !< array of damping range for each part
          integer, dimension(isms*npart), intent(in) :: tagprt_sms !< array of sms tags for each element's part

          integer, dimension(lipart1,npart), intent(in) :: ipart !< array of part parameters
          integer, dimension(3,numelc+numeltg), intent(in) :: iworksh !< ply array for shell element          
          integer, dimension(nb_key,numelc), intent(inout) :: sort_key !< array to store the sorting keys for each element
          integer, dimension(nixc,numelc), intent(in) :: ixc !< shell connectivity array
          integer, dimension(npropgi,numgeo), intent(in) :: igeo !< property parameters array
          integer, dimension(npropmi,nummat), intent(in) :: ipm          
          
          integer, dimension(sh4tree_dim1,sh4tree_dim2), intent(in) :: sh4tree !< tree array for shells (adaptive meshing)
          
          real(kind=WP), dimension(npropg,numgeo), intent(in) :: geo !< property parameters array
          real(kind=WP), dimension(npropm,nummat), intent(in) :: pm !< material parameters array

          type(stack_ply), intent(in) :: stack !< stack data structure for multilayer shells
          type(matparam_struct_), dimension(nummat), intent(in) :: mat_param !< array of material parameters structures          
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i, ipt, mid, pid, mln, igtyp, jthe, npn, ihbe, ithk, ipla, irep
          integer :: ishxfem_ply, nfail, ifail, ixfem, iexpan, icsen, irb, jsms
          integer :: istrain, issn, iadm, ilev, prt, imatly
          integer :: isubstack, ippid, ipid, ipmat
          integer :: nb_law58
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------
          integer, external :: my_shiftl
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

! ----------------------------------------------------------------------------------------------------------------------
          sort_key(1:nb_key,1:numelc) = 0
          do i=1,numelc
            if(nadmesh/=0) then
              prt = ipartc(i)
              iadm = ipart(10,prt)
              if(iadm==0) then
                sort_key(1,i) = 0
              else
                ilev = sh4tree(3,i)
                if(ilev<0) ilev=-ilev-1
                sort_key(1,i) = ilev+1
              end if
            endif

            mid = ixc(1,i) ! get the material id
            pid = ixc(6,i) ! get the property id
            mln  = nint(pm(19,mid)) ! get the material law number
            igtyp = igeo(11,pid) ! get the geometric type
            jthe = nint(pm(71,mid)) ! get the thermal expansion coefficient
            npn  = igeo(4,pid) ! get the number of integration points
            ihbe = nint(geo(171,pid)) ! get the bending stiffness
            ithk = nint(geo(35,pid)) ! get the shell thickness
            ipla = nint(geo(39,pid)) ! get the plasticity flag
            irep = igeo(6,pid)
            ishxfem_ply = igeo(19,pid) ! get the shell xfem ply flag
            nfail = 0 ! initialize the number of failure criteria for the material
            ifail = 0 ! initialize the failure id
            ixfem = 0 ! initialize the xfem flag

            if(igtyp==11) then
              do ipt=1,npn
                imatly  = igeo(100+ipt,pid) ! get the material id of the integration point
                nfail = max(nfail,mat_param(imatly)%nfail) ! get the maximum number of failure criteria among the integration points
              end do
              if(icrack3d > 0) then
                ixfem = mat_param(mid)%ixfem ! get the xfem flag for multilayer shells
              end if
            else if(igtyp==17) then
              npn = iworksh(1,i)
              isubstack = iworksh(3,i)
              ippid = 2
              do ipt=1,npn
                ipid = stack%igeo(ippid + ipt,isubstack) ! get the property id of the integration point from the stack
                imatly  = igeo(101,ipid) ! get the material id of the integration point from the property id
                nfail = max(nfail,mat_param(imatly)%nfail) ! get the maximum number of failure criteria among the integration points
              end do
            else if(igtyp==51.or.igtyp==52) then
              nb_law58 = 0 ! initialize the number of plys with law 58
              npn = iworksh(1,i)
              isubstack = iworksh(3,i)
              ippid = 2
              ipmat = 0
              if(igtyp==52) ipmat = ippid + npn
              do ipt=1,npn
                ipid  = stack%igeo(ippid + ipt,isubstack) ! get the property id of the integration point from the stack
                if(igtyp==51) then
                  imatly = igeo(101,ipid) ! get the material id of the integration point from the property id
                else
                  imatly = stack%igeo(ipmat + ipt,isubstack) ! get the material id of the integration point from the stack
                endif
                nfail  = max(nfail,mat_param(imatly)%nfail) ! get the maximum number of failure criteria among the integration points
                if(nint(pm(19,imatly))==58) nb_law58 = nb_law58 + 1 ! count the number of plys with law 58
              enddo
              if(nb_law58 == npn) then
                irep = 2 ! set the property id for shells with all plys having law 58
              else if(nb_law58 > 0) then
                irep = irep + 3 ! set the property id for shells with some plys having law 58
              end if
            else
              nfail = mat_param(mid)%nfail ! get the number of failure criteria for monolayer shells
              if(icrack3d > 0) then
                ixfem = mat_param(mid)%ixfem ! get the xfem flag for monolayer shells
                if(ixfem==1) then
                  ixfem=2 ! set the xfem flag to 2 for cracked monolayer shells to optimize sorting
                  icrack3d = ixfem ! set the global crack3d flag to 2 to optimize sorting
                end if
              endif              
            endif

            if(nfail>0) ifail = 1 ! set the failure id to 1 if there is at least one failure criterion for the material
            iexpan = ipm(218,mid) ! get the thermal expansion flag
            icsen = igeo(3,pid) ! get the in-plane sensitivity
            if(icsen>0) icsen=1 ! set the in-plane sensitivity flag to 1 if it is positive
            if(npn==0.and.(mln==36.or.mln==86)) then
              if(ipla==0) ipla=1 ! set the plasticity flag to 1 for shells with material law 36 or 86 and no integration points
              if(ipla==2) ipla=0 ! set the plasticity flag to 0 for shells with material law 36 or 86 and plasticity flag 2
            else if(npn==0.and.mln==2) then
              if(ipla==2) ipla=0 ! set the plasticity flag to 0 for shells with material law 2 and plasticity flag 2
            else
              if(ipla==2) ipla=0 ! set the plasticity flag to 0 for shells with plasticity flag 2
              if(ipla==3) ipla=2 ! set the plasticity flag to 2 for shells with plasticity flag 3
            endif
            if(ithk==2) then
              ithk=0 ! set the thickness flag to 0 for shells with thickness flag 2
            else if(mln==32) then
              ithk=1 ! set the thickness flag to 1 for shells with material law 32
            endif
            ipla = iabs(ipla) ! take the absolute value of the plasticity flag
            ithk = iabs(ithk) ! take the absolute value of the thickness flag
            istrain = nint(geo(11,pid)) ! get the strain flag
            if(mln==19.or.mln>=25.or.mln==15) istrain=1 ! set the strain flag to 1 for shells with material law 19, 25 or higher, or 15
            issn = iabs(nint(geo(3,pid))) ! get the absolute value of the stress sensitivity flag
            irb = isheoff(i) ! get the rigid body flag for the element
            jsms = 0 ! initialize the sms flag
            if(isms/=0) then
              if(idtgrs/=0) then
                if(tagprt_sms(ipartc(i))/=0) jsms=1 ! set the sms flag to 1 for elements with nonzero sms tag if the sms is activated in the
              else
                jsms=1 ! set the sms flag to 1 for all elements if the sms is activated in the model and not based on part tags
              end if
            end if
            sort_key(2,i) = jsms ! set the second key to the sms flag
            istrain = my_shiftl(istrain,3) ! shift the strain flag to the left by 3 bits
            issn   = my_shiftl(issn,6) ! shift the stress sensitivity flag to the left by 6 bits
            ihbe   = my_shiftl(ihbe,9) ! shift the bending stiffness flag to the left by 9 bits
            igtyp  = my_shiftl(igtyp,12) ! shift the geometric type to the left by 12 bits
            mln    = my_shiftl(mln,21) ! shift the material law number to the left by 21 bits
            sort_key(3,i) = ipla + istrain + issn + ihbe + igtyp + mln ! set the third key

            ishxfem_ply  = my_shiftl(ishxfem_ply,10) ! shift the shell xfem ply flag to the left by 10 bits
            ifail  = my_shiftl(ifail,11) ! shift the failure id to the left by 11 bits
            iexpan = my_shiftl(iexpan,14) ! shift the thermal expansion flag to the left by 14 bits
            jthe   = my_shiftl(jthe,15) ! shift the thermal expansion coefficient flag to the left by 15 bits
            icsen = my_shiftl(icsen,16) ! shift the in-plane sensitivity flag to the left by 16 bits
            npn  = my_shiftl(npn,17) ! shift the number of integration points to the left by 17 bits
            irep = my_shiftl(irep,26) ! shift the property id to the left by 26 bits
            ithk = my_shiftl(ithk,30) ! shift the thickness flag to the left by 30 bits
            if(ixfem>0) ixfem = my_shiftl(ixfem,9) ! shift the xfem flag to the left by 9 bits if it is positive
            sort_key(4,i) = ithk + irep + npn + icsen + jthe + iexpan + irb + ifail + ishxfem_ply + ixfem ! set the fourth key

            sort_key(5,i) = mid ! set the fifth key to the material id
            sort_key(6,i) = pid ! set the sixth key to the property id

            sort_key(7,i) = iworksh(2,i) ! set the seventh key to the type 17 property id for shells with type 17 geometric properties, or 0 for other shells

            sort_key(8,i) = damp_range_part(ipartc(i)) ! set the eighth key to the damping range for the part of the element           
          end do

          return

        end subroutine get_sort_key_shell
      end module get_sort_key_shell_mod
