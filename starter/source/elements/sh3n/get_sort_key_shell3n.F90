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
!||    get_sort_key_shell3n_mod   ../starter/source/elements/sh3n/get_sort_key_shell3n.F90
!||--- called by ------------------------------------------------------
!||    c3grhead                   ../starter/source/elements/sh3n/coque3n/c3grhead.F
!||    get_element_group          ../starter/source/elements/get_element_group.F90
!||====================================================================
      module get_sort_key_shell3n_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief Initialization of the sorting keys for the shell3n elements based on their properties, material and part ids, etc...
!! \details
!||====================================================================
!||    get_sort_key_shell3n   ../starter/source/elements/sh3n/get_sort_key_shell3n.F90
!||--- called by ------------------------------------------------------
!||    c3grhead               ../starter/source/elements/sh3n/coque3n/c3grhead.F
!||    get_element_group      ../starter/source/elements/get_element_group.F90
!||--- calls      -----------------------------------------------------
!||    ancmsg                 ../starter/source/output/message/message.F
!||    fretitl2               ../starter/source/starter/freform.F
!||--- uses       -----------------------------------------------------
!||    message_mod            ../starter/share/message_module/message_mod.F
!||    stack_mod              ../starter/share/modules1/stack_mod.F
!||====================================================================
        subroutine get_sort_key_shell3n(numelc,numeltg,nb_key,nummat,numgeo, &
                                      npart,nadmesh,lipart1,npropgi, &
                                      npropmi,npropm,npropg,icrack3d, &
                                      isms,idtgrs,sh3tree_dim1,sh3tree_dim2,ltitr,trimat,iwarnhb, &
                                      iparttg,ish3noff,damp_range_part,icnod,tagprt_sms, &
                                      ipart,iworksh,sort_key,ixtg,igeo, &
                                      ipm,sh3tree,pm,geo, &
                                      stack,mat_param)

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use precision_mod, only : WP
          use element_mod, only : nixtg
          use matparam_def_mod , only : matparam_struct_
          use stack_mod
          use message_mod
          use names_and_titles_mod
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
          integer, intent(in) :: numeltg !< number of shell3n elements
          integer, intent(in) :: numelc !< number of shell elements
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

          integer, intent(in) :: sh3tree_dim1 !< first dimension of the sh3tree array for shell3ns (adaptive meshing)
          integer, intent(in) :: sh3tree_dim2 !< second dimension of the sh3tree array for shell3ns (adaptive meshing)
          integer, intent(in) :: ltitr !< title size ???
          integer, intent(inout) :: trimat !< number of sub-materials for the law 151
          integer, intent(inout) :: iwarnhb !< number of Warning mesages

          integer, dimension(numeltg), intent(in) :: iparttg !< array of part ids for each element
          integer, dimension(numeltg), intent(in) :: ish3noff  !< deactivated shell3n flag
          integer, dimension(npart), intent(in) :: damp_range_part !< array of damping range for each part
          integer, dimension(numeltg), intent(in) :: icnod !< ???
          integer, dimension(isms*npart), intent(in) :: tagprt_sms !< array of sms tags for each element's part

          integer, dimension(lipart1,npart), intent(in) :: ipart !< array of part parameters
          integer, dimension(3,numelc+numeltg), intent(in) :: iworksh !< ply array for shell element          
          integer, dimension(nb_key,numeltg), intent(inout) :: sort_key !< array to store the sorting keys for each element
          integer, dimension(nixtg,numeltg), intent(in) :: ixtg !< shell3n connectivity array
          integer, dimension(npropgi,numgeo), intent(in) :: igeo !< property parameters array
          integer, dimension(npropmi,nummat), intent(in) :: ipm          
          
          integer, dimension(sh3tree_dim1,sh3tree_dim2), intent(in) :: sh3tree !< tree array for shell3ns (adaptive meshing)
          
          real(kind=WP), dimension(npropg,numgeo), intent(in) :: geo !< property parameters array
          real(kind=WP), dimension(npropm,nummat), intent(in) :: pm !< material parameters array

          type(stack_ply), intent(in) :: stack !< stack data structure for multilayer shell3ns
          type(matparam_struct_), dimension(nummat), intent(in) :: mat_param !< array of material parameters structures          
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          character(len=nchartitle) :: titr
          integer :: i, ipt, mid, pid, mln, igtyp, jthe, npn, ithk, ipla, irep
          integer :: nfail, ifail, ixfem, iexpan, icsen, irb, jsms
          integer :: istrain, issn, iadm, ilev, prt, imatly
          integer :: isubstack, ippid, ipid, ipmat
          integer :: nb_law58,ish3n,id,ico
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------
          integer, external :: my_shiftl
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

! ----------------------------------------------------------------------------------------------------------------------
          sort_key(1:nb_key,1:numeltg) = 0
          iwarnhb = 0
          do i=1,numeltg
            if(nadmesh/=0) then
              prt = iparttg(i)
              iadm = ipart(10,prt)
              if(iadm==0) then
                sort_key(1,i) = 0
              else
                ilev = sh3tree(3,i)
                if(ilev<0) ilev=-ilev-1
                sort_key(1,i) = ilev+1
              end if
            endif

            mid = ixtg(1,i) ! get the material id for the element
            pid = ixtg(5,i) ! get the property id for the element
            mln = nint(pm(19,mid)) ! get the material law number for the element
            if(mln==51) trimat=4

            jthe = nint(pm(71,mid)) ! get the material thermal expansion law number for the element
            igtyp = igeo(11,pid) ! get the property type for the element
            npn   = igeo(4,pid) ! get the number of points through the thickness for the element
            ish3n  = igeo(18,pid) ! get the shell3n flag for the element
            ixfem = 0
            nfail = mat_param(mid)%nfail ! get the number of failures for the material assigned to the element
            ifail = 0
            irep = igeo(6,pid)            

            if(igtyp==11) then
              do ipt=1,npn
                imatly = igeo(100+ipt,pid) ! get the material id for each ply of the element
                nfail = max(nfail, mat_param(imatly)%nfail) ! get the maximum number of failures among the plies
              end do
              if(icrack3d>0) then
                ! new multilayer
                ixfem = mat_param(mid)%ixfem ! get the xfem flag for the material assigned to the element
              end if
            else if(igtyp==17) then
              npn = iworksh(1,numelc+i) ! get the number of points through the thickness for the element from the ply array
              isubstack = iworksh(3,numelc+i) ! get the substack id for the element from the ply array
              ippid = 2 ! get the property id for the substack
              do ipt=1,npn
                ipid = stack%igeo(ippid+ipt,isubstack) ! get the property id for each ply of the element from the stack data structure
                imatly = igeo(101, ipid) ! get the material id for each ply of the element from the property parameters array
                nfail = max(nfail, mat_param(imatly)%nfail) ! get the maximum number of failures among the plies
              end do
            else if(igtyp==51.or.igtyp==52) then
              ! new shell property (multiple NPT through each layer)
              nb_law58 = 0
              npn = iworksh(1,numelc+i) ! get the number of points through the thickness for the element from the ply array
              isubstack = iworksh(3,numelc+i) ! get the substack id for the element from the ply array
              ipmat = 0
              ippid = 2 ! get the property id for the substack
              if(igtyp==52) ipmat = ippid + npn ! get the material id for the substack
              do ipt=1,npn
                ipid = stack%igeo(ippid+ipt,isubstack) ! get the property id for each ply of the element from the stack data structure                
                if(igtyp==51) then
                  imatly = igeo(101, ipid) ! get the material id for each ply of the element from the property parameters array
                else
                  imatly = stack%igeo(ipmat+ipt,isubstack) ! get the material id for each ply of the element from the stack data structure
                endif
                nfail = max(nfail, mat_param(imatly)%nfail) ! get the maximum number of failures among the plies
                ! PID 51 combined with LAW58
                if(nint(pm(19,imatly))==58) nb_law58 = nb_law58 + 1
              end do
              ! set IREP for tri criteria:
              if(nb_law58==npn) then
                irep = 2
              else if(nb_law58>0) then
                irep = irep + 3
              end if
            else ! igtyp == 1
              if(icrack3d>0) then
                ! new monolayer
                ixfem = mat_param(mid)%ixfem ! get the xfem flag for the material assigned to the element
                if(ixfem==1) then
                  ixfem = 2
                  icrack3d = ixfem
                end if
              end if
            end if
            if(nfail>0) ifail = 1
            iexpan = ipm(218,mid) ! get the thermal expansion flag for the material assigned to the element
            ico = icnod(i)
            if(ish3n>3.and.ish3n<=29) then
              id = igeo(1,pid) ! get the property id for the element
              call fretitl2(titr,igeo(npropgi-ltitr+1,pid),ltitr) ! get the title for the property id
              call ancmsg(msgid=435, &
                           msgtype=msgwarning, &
                           anmode=aninfo_blind_2, &
                           i1=id, &
                           c1=titr, &
                           i2=ish3n, &
                           i3=ixtg(nixtg,i)) ! print a warning message if the shell3n flag is not valid
              iwarnhb = iwarnhb + 1
              ish3n = 2 ! set the shell3n flag to 2 (default) if it is not valid
            end if

            ithk = nint(geo(35,pid)) ! get the thickness flag for the element
            ipla = nint(geo(39,pid)) ! get the planarity flag for the element
            icsen = igeo(3,pid) 
            if(icsen>0) icsen=1

            if(npn==0.and.(mln==36.or.mln==86)) then
              if(ipla==0) ipla=1
              if(ipla==2) ipla=0
            else if(npn==0.and.mln==2) then
              if(ipla==2) ipla=0
            else
              if(ipla==2) ipla=0
              if(ipla==3) ipla=2
            end if

            if(ithk==2) then
              ithk = 0
            else if(mln==32) then
              ithk = 1
            end if
            ipla = abs(ipla)
            ithk = abs(ithk)
            istrain = nint(geo(11,pid)) ! get the strain flag for the element
            if(mln==19.or.mln>=25.or.mln==15) istrain = 1
            issn = abs(nint(geo(3,pid))) 
            irb = ish3noff(i) ! get the rigid body flag for the element

            jsms = 0
            if(isms/=0) then
              if(idtgrs/=0) then
                if(tagprt_sms(iparttg(i))/=0) jsms=1
              else
                jsms=1
              end if
            end if
            jsms = my_shiftl(jsms,0)            
            sort_key(2,i) = jsms
            
            istrain = my_shiftl(istrain,3)
            issn   = my_shiftl(issn,6)
            igtyp  = my_shiftl(igtyp,9)
            mln    = my_shiftl(mln,18)
            ico   = my_shiftl(ico,29)
            sort_key(3,i) = ipla+istrain+issn+igtyp+mln+ico

            ifail = my_shiftl(ifail,4)
            iexpan = my_shiftl(iexpan,5)
            jthe =  my_shiftl(jthe,6)
            ish3n  = my_shiftl(ish3n,11)
            icsen = my_shiftl(icsen,16)
            npn   = my_shiftl(npn,17)
            irep  = my_shiftl(irep,26)
            ithk  = my_shiftl(ithk,30)
            if(ixfem > 0) ixfem  = my_shiftl(ixfem,9)
            sort_key(4,i) = ithk+irep+npn+icsen+ish3n+jthe+ifail+ixfem+irb

            sort_key(5,i) = mid
            sort_key(6,i) = pid

            sort_key(7,i) = iworksh(2,numelc+i) ! get the substack id for the element

            sort_key(8,i) = damp_range_part(iparttg(i)) ! get the damping range for the element's part
          end do

          return

        end subroutine get_sort_key_shell3n
      end module get_sort_key_shell3n_mod
