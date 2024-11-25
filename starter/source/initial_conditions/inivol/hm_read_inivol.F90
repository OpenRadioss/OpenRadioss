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
module hm_read_inivol_mod
contains
   !||====================================================================
   !||    hm_read_inivol             ../starter/source/initial_conditions/inivol/hm_read_inivol.F
   !||--- called by ------------------------------------------------------
   !||    lectur                     ../starter/source/starter/lectur.F
   !||--- calls      -----------------------------------------------------
   !||    ancmsg                     ../starter/source/output/message/message.F
   !||    hm_get_float_array_index   ../starter/source/devtools/hm_reader/hm_get_float_array_index.F
   !||    hm_get_int_array_index     ../starter/source/devtools/hm_reader/hm_get_int_array_index.F
   !||    hm_get_intv                ../starter/source/devtools/hm_reader/hm_get_intv.F
   !||    hm_option_is_encrypted     ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
   !||    hm_option_read_key         ../starter/source/devtools/hm_reader/hm_option_read_key.F
   !||    hm_option_start            ../starter/source/devtools/hm_reader/hm_option_start.F
   !||    hm_sz_r2r                  ../starter/source/coupling/rad2rad/routines_r2r.F
   !||    trace_in1                  ../starter/source/system/trace_back.F
   !||--- uses       -----------------------------------------------------
   !||    hm_option_read_mod         ../starter/share/modules1/hm_option_read_mod.F
   !||    inivol_def_mod             ../starter/share/modules1/inivol_mod.F
   !||    message_mod                ../starter/share/message_module/message_mod.F
   !||    r2r_mod                    ../starter/share/modules1/r2r_mod.F
   !||    submodel_mod               ../starter/share/modules1/submodel_mod.F
   !||====================================================================
   !! \brief Read the initial volume option
   subroutine hm_read_inivol(inivol, kvol, igrsurf, ipart, multi_fvm, bufmat, &
   &                         ipm, nbsubmat, lsubmodel, unitab,                &
   &                         n2d ,numeltg,numels,numelq,nummat,               &
   &                         npart,nsurf,lipart1,npropmi,sipart,sinivol,      &
   &                         nsubdom,sbufmat  )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use my_alloc_mod
      use unitab_mod
      use message_mod
      use multi_fvm_mod
      use groupdef_mod
      use inivol_def_mod , only : inivol_struct_, num_inivol, skvol
      use submodel_mod
      use hm_option_read_mod
      use setdef_mod
      use r2r_mod
      use names_and_titles_mod , only : nchartitle, ncharkey, ncharline
      use file_descriptor_mod
      use constant_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
      implicit none
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      integer,intent(in)::ipm(npropmi*nummat)  !< integer material properties
      type(multi_fvm_struct),intent(in) :: multi_fvm
      type (surf_), dimension(nsurf+nsets)   :: igrsurf
      type (inivol_struct_), intent(inout), dimension(:), allocatable  :: inivol
      type(submodel_data),intent(in) :: lsubmodel(nsubmod)
      my_real, dimension(:), allocatable, intent(inout) :: kvol
      type (unit_type_),intent(in) :: unitab
      integer,intent(inout) :: nbsubmat
      integer,intent(in) :: sipart         !< length of ipart
      integer,intent(in) :: sbufmat        !< length of bufmat
      integer,intent(in) :: n2d           !< 2d or 3d analysis
      integer,intent(in) :: numeltg       !< number of triangle elements
      integer,intent(in) :: numels        !< number of solid elements
      integer,intent(in) :: numelq        !< number of quad elements
      integer,intent(in) :: nummat        !< number of materials
      integer,intent(in) :: npart         !< number of parts
      integer,intent(in) :: nsurf         !< number of surfaces
      integer,intent(in) :: lipart1       !< length of ipart
      integer,intent(in) :: npropmi       !< first dimention of ipm
      integer,intent(inout) :: sinivol    !< length of inivol commputed
      integer,intent(in) :: nsubdom       !< number of subdomains (rad2rad)
      integer,intent(in) :: ipart(sipart) !< part data
      ! real arrays
      my_real,intent(in),target :: bufmat(sbufmat) !< buffer for material data
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
      integer i,j,ip,id,part_id,jrec,igs,part_user_id,idsurf
      integer submat_id,ireversed,ncont,nlin,isu,nn,jj,nintmax,ityp,numel_tot
      integer srftyp,fac,k,icumu,imat,iadbuf,nuparam,isubmat,stat,kk,n_r2r
      integer nbmat_max
      my_real vfrac,ilaw
      my_real, dimension(:),pointer :: uparam
      character(len=nchartitle) :: titr
      character mess*40
      character(len=ncharkey) :: key
      character(len=ncharline) err_msg
      data mess/'INITIAL VOLUME FRACTION                 '/
      character(len=ncharline) :: outp_msg
      logical is_encrypted,is_available
      logical detected_error
!-----------------------------------------------
!   c o m m e n t s
!-----------------------------------------------
!     inivol(igs)%id                      : initial volume fraction identifier
!     inivol(igs)%title                   : inivol title
!     inivol(igs)%num_container           : number of inivol containers (surfaces)
!     inivol(igs)%part_id                 : inivol part to be filled
!     inivol(igs)%container(kk)%surf_id   : inivol container surface
!     inivol(igs)%container(kk)%submat_id : submat identifier of the multi-material ale to fill the part
!     inivol(igs)%container(kk)%icumu     : flag for cumulate volume filling
!     inivol(igs)%container(kk)%vfrac     : filling ratio:
!                                          = 0 ! filling the side along normal direction
!                                          = 1 ! filling the side against normal direction
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

      if (multi_fvm%is_used)then
         nbsubmat = multi_fvm%nbmat
      else
         nbsubmat = 4
      endif

      !---output message & sizes
      skvol = 0
      sinivol = 0
      err_msg='INITIAL VOLUME FRACTION'
      err_category='INITIAL VOLUME FRACTION'
      call trace_in1(err_msg,len_trim(err_msg))
      if (num_inivol > 0)then
         write(istdo,'(A)')' .. INITIAL VOLUME FRACTION'
         write(iout,'(//A)')'     INITIAL VOLUME FRACTION'
         write(iout,'(A/)') '     -----------------------'
         numel_tot = max(numeltg,max(numels,numelq))
         skvol = nbsubmat*numel_tot
         sinivol = num_inivol
      endif

      !---allocation
      stat=0
      if(.not.allocated(inivol))allocate (inivol(sinivol) ,stat=stat)
      if (stat /= 0) call ancmsg(msgid=268,anmode=aninfo,msgtype=msgerror,c1='INIVOL')

      if(.not.allocated(kvol))allocate (kvol(skvol)     ,stat=stat)
      if (stat /= 0) call ancmsg(msgid=268,anmode=aninfo, msgtype=msgerror,c1='KVOL')
      if (skvol > 0) kvol  = 0

      !---read cards
      call hm_option_start('/INIVOL')
      is_encrypted= .false.
      is_available = .false.
      call hm_option_is_encrypted(is_encrypted)
      igs=0
      nintmax = 3
      n_r2r = 0
!
      do igs=1,num_inivol
!
         if (nsubdom > 0) then
            n_r2r = n_r2r + 1
            if(tag_inivol(n_r2r) == 0) call hm_sz_r2r(tag_inivol,n_r2r,lsubmodel)
         endif

         call hm_option_read_key(lsubmodel, option_id=id, option_titr=titr)
         call hm_get_intv('secondarycomponentlist', part_user_id, is_available, lsubmodel)
         call hm_get_intv('NIP', nlin, is_available, lsubmodel)

         write(iout, 1001) titr, id, part_user_id

         part_id = 0
         !search part_id
         do j=1,npart
            ip = ipart(lipart1*(j-1)+4)
            if(part_user_id == ip)then
               part_id = j
               exit
            endif
         enddo
         if(part_id == 0)call ancmsg(msgid=886,msgtype=msgerror,anmode=aninfo,i1=id,c1=titr,i2=part_user_id)  !part_id not found

         inivol(igs)%id = id
         inivol(igs)%title(1:nchartitle)=' '
         inivol(igs)%title = titr(1:len_trim(titr))
         inivol(igs)%num_container = nlin
         inivol(igs)%part_id = part_id
         allocate(inivol(igs)%container(nlin))

         write(iout, '(A)') "     surf_ID SUBMAT_ID IREVERSED     ICUMU               VFRAC"
         do kk=1,nlin
            call hm_get_int_array_index('SETSURFID_ARR', idsurf,kk, is_available, lsubmodel)
            call hm_get_int_array_index('ALE_PHASE', submat_id,kk, is_available, lsubmodel)
            call hm_get_int_array_index('fill_opt_arr', ireversed,kk, is_available, lsubmodel)
            call hm_get_int_array_index('ICUMU', icumu,kk, is_available, lsubmodel)
            call hm_get_float_array_index('FILL_RATIO', vfrac, kk, is_available, lsubmodel,unitab)

            nbmat_max = 4
            if(multi_fvm%is_used) nbmat_max = multi_fvm%nbmat
            if(submat_id <= 0 .or. submat_id > nbmat_max)then
               call ancmsg(msgid=887,msgtype=msgerror, anmode=aninfo,i1=id,c1=titr,i2=nbmat_max)
            endif
            if(ireversed < 0 .or. ireversed > 1)then
               outp_msg = ''
               outp_msg = 'FILLING OPTION MUST BE 0 OR 1 (0:DEFAULT, 1:REVERSED SURFACE)'
               call ancmsg(msgid=888,msgtype=msgerror,anmode=aninfo,i1=id,c1=titr,c2=outp_msg)
            endif
            if(vfrac < zero .or. vfrac > one)then
               call ancmsg(msgid=1596,msgtype=msgerror,anmode=aninfo,i1=id,c1=titr)
            endif

            ! vfrac \in [0,1] is optionnal. no value means that isubmat is filling 100% inside the surface.
            if(vfrac == zero)vfrac=one

            ! icumu = 1 : additive filling
            ! icumu = 0 : erase existing filling
            ! icumu =-1 : subtractive filling (substract the excess from the previous filling
            if(icumu < -1 .or. icumu > 1)then
               icumu = 0
            endif
            if(n2d == 0 .and. icumu == -1)then
               outp_msg = ''
               outp_msg = 'ICUMU=-1 NOT COMPATIBLE WITH 3D ANALYSIS (ICUMU SET TO 0)'
               call ancmsg(msgid=888,msgtype=msgwarning,anmode=aninfo,i1=id,c1=titr,c2=outp_msg)
            endif

            write(iout,'(2X,I10,I10,I9,I6,F20.0)')idsurf,submat_id,ireversed,icumu,vfrac

            imat = ipart(lipart1*(part_id-1)+1)
            ilaw = ipm((imat-1)*npropmi + 2)     !ipm(2,imat)
            if(ilaw/=51 .and. ilaw/=151)then
               call ancmsg(msgid=821, msgtype=msgerror, anmode=aninfo, i1=id, c1=titr)
            endif

            !!get bijective application to retrieve internal order of submaterial.
            !! (internally & historically submat4 is explosive submaterial)
            if(ilaw == 51)then
               iadbuf = ipm((imat-1)*npropmi + 7)!ipm(7,imat)
               nuparam= ipm((imat-1)*npropmi + 9)!ipm(9,imat)
               uparam => bufmat(iadbuf:iadbuf+nuparam)
               submat_id=uparam(276+submat_id)
            endif

            isu=0
            nn =0
            do j=1,nsurf
               if (idsurf == igrsurf(j)%id) then
                  isu=j
                  nn = igrsurf(isu)%nseg
                  exit
               end if
            enddo
            fac = 0
            if (isu > 0) then
               srftyp=igrsurf(isu)%type
               if(srftyp == 101 .or. srftyp == 200) then
                  fac = 1
               else
                  do j=1,nn
                     ityp=igrsurf(isu)%eltyp(j)
                     if(ityp==0 .or. ityp==3 .or. ityp==7) fac = fac + 1
                  enddo
               endif

               if(isu > 0 .and. nn > 1 .and. n2d /= 0)then
                  !test surface closure (closed polygon)
                  if(igrsurf(isu)%nodes(1,1) /= igrsurf(isu)%nodes(nn,2)) then
                     call ancmsg(msgid=3063,msgtype=msgerror,anmode=aninfo,i1=id,i2=igrsurf(isu)%id,c1=titr)
                  endif
                  !test polygon definition --- (last point of current segment is first point of next one : otherwise set detected_error to .true.
                  detected_error=.false.
                  do j=1,nn-1
                     if(igrsurf(isu)%nodes(j,2) /= igrsurf(isu)%nodes(j+1,1))then
                        detected_error = .true.
                        exit
                     endif
                  enddo
                  if(detected_error)then
                     call ancmsg(msgid=3064,msgtype=msgerror,anmode=aninfo,i1=id,i2=igrsurf(isu)%id,c1=titr)
                  endif
               elseif(isu > 0)then
                  if(isu > 0 .and. nn <= 2 .and. n2d /= 0 .and. srftyp == 0)then
                     call ancmsg(msgid=3064,msgtype=msgerror,anmode=aninfo,i1=id,i2=igrsurf(isu)%id,c1=titr) !not enough points to define a polygon
                  endif
               endif
               !---   discretized surface
               inivol(igs)%container(kk)%surf_id = isu
               inivol(igs)%container(kk)%submat_id = submat_id
               inivol(igs)%container(kk)%ireversed = ireversed
               inivol(igs)%container(kk)%vfrac = int(vfrac*ep9)
               inivol(igs)%container(kk)%icumu = icumu
!
               if(n2d == 0 .and. fac == 0)then
                  call ancmsg(msgid=890,msgtype=msgerror,anmode=aninfo,i1=id,c1=titr)
               elseif(n2d > 0 .and. srftyp /=0 .and. srftyp /=101 .and. srftyp /= 200)then
                  call ancmsg(msgid=2012,msgtype=msgerror,anmode=aninfo,i1=id,c1=titr)
               endif
            else  ! isu == 0
               call ancmsg(msgid=889,msgtype=msgerror,anmode=aninfo,i1=id,c1=titr,i2=idsurf)
            endif
         enddo   !next line
         write(iout,'(A//)')
      end do !next option

!-----------------------------
      return
!-----------------------------
1001  format(&
      &5X,'TITLE  . . . . . . . . . . . . . . . .=',A/,&
      &5X,'IDENTFIER (ID) . . . . . . . . . . . .=',I10/,&
      &5X,'PART IDENTIFIER. . . . . . . . . . . .=',I10)

      return
   end
end module hm_read_inivol_mod

