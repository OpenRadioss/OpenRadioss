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
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
      !||====================================================================
      !||    hm_read_ebcs_propergol   ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_propergol.F90
      !||--- called by ------------------------------------------------------
      !||    read_ebcs                ../starter/source/boundary_conditions/ebcs/read_ebcs.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                   ../starter/source/output/message/message.F
      !||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
      !||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
      !||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
      !||    ngr2usr                  ../starter/source/system/nintrr.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod              ../starter/share/message_module/message_mod.F
      !||    submodel_mod             ../starter/share/modules1/submodel_mod.F
      !||====================================================================
      subroutine hm_read_ebcs_propergol(igrsurf, multi_fvm, unitab, id, titr, uid, lsubmodel,  nsurf, numnod, ebcs)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use ebcs_mod
      use unitab_mod
      use message_mod
      use multi_fvm_mod
      use groupdef_mod
      use submodel_mod
      use names_and_titles_mod , only : nchartitle, ncharkey
      use constant_mod , only : zero, one, three100
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
      implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
#include      "units_c.inc"
#include      "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      integer,intent(in) :: nsurf, numnod !< array sizes
      type (unit_type_),intent(in) ::unitab
      integer id,uid
      type (multi_fvm_struct), intent(inout) :: multi_fvm
      type (surf_)   ,target,  dimension(nsurf)   :: igrsurf
      character(len=nchartitle), intent(in) :: titr
      type(submodel_data) lsubmodel(nsubmod)
      logical is_available,is_encrypted
      type(t_ebcs_propergol), intent(inout) :: ebcs
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
      integer i,isu,sens,monvol,surf,ipres,irho,nod,iad,j,k1,k2,nseg,iener,ivx,ivy,ivz,ialpha
      integer liste(numnod), imat,ivel_typ,u_ialpha,u_irho,u_ipres,iflagunit,flag_fmt,check_cumul_vf(2)
      integer sensor_id, submat_id
      integer ffunc_id, gfunc_id, hfunc_id
      my_real :: fscaleX,fscaleY,gscaleX,gscaleY,hscaleX,hscaleY
      my_real :: c,pres,rho,lcar,r1,r2,ener,vx,vy,vz, alpha, param_a, param_n, param_q, param_rho0s,param_t
      character mess*40,chain*9, chain1*64
      character(len=ncharkey) :: key
      logical found
      integer, dimension(:), pointer :: ingr2usr
      integer, external :: ngr2usr
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
      ebcs%title = trim(titr)

      iflagunit=0
      do j=1,unitab%nunits
        if (unitab%unit_id(j) == uid) then
          iflagunit = 1
          exit
        endif
      enddo
      if (uid/=0.and.iflagunit==0) then
        call ancmsg(msgid=659,anmode=aninfo,msgtype=msgerror,i2=uid,i1=id,c1='ebcs',c2='ebcs',c3=titr)
      endif
    
      call hm_option_is_encrypted(is_encrypted)
      call hm_get_intv('entityid' , surf     , is_available,lsubmodel)
      call hm_get_intv('sensor_id', sensor_id, is_available,lsubmodel)
      call hm_get_intv('submat_id', submat_id, is_available,lsubmodel)
      ! propergol properties
      call hm_get_floatv('rho0s', param_rho0s, is_available,lsubmodel,unitab)
      call hm_get_floatv('param_t', param_t, is_available,lsubmodel,unitab)
      ! combustion model
      call hm_get_floatv('param_a', param_a, is_available,lsubmodel,unitab)
      call hm_get_floatv('param_n', param_n, is_available,lsubmodel,unitab)
      call hm_get_intv('ffunc_id' , ffunc_id, is_available,lsubmodel)
      call hm_get_intv('gfunc_id' , gfunc_id, is_available,lsubmodel)
      call hm_get_intv('hfunc_id' , hfunc_id, is_available,lsubmodel)
      call hm_get_floatv('fscaleX', fscaleX, is_available,lsubmodel,unitab)
      call hm_get_floatv('fscaleY', fscaleY, is_available,lsubmodel,unitab)
      call hm_get_floatv('gscaleX', gscaleX, is_available,lsubmodel,unitab)
      call hm_get_floatv('gscaleY', gscaleY, is_available,lsubmodel,unitab)
      call hm_get_floatv('hscaleX', hscaleX, is_available,lsubmodel,unitab)
      call hm_get_floatv('hscaleY', hscaleY, is_available,lsubmodel,unitab)

      if(param_a == zero)param_a = one
      if(param_n < zero)param_n = zero
      !if(param_q < zero)param_q = zero
      if(param_t <= zero)param_t = three100
      if(param_rho0s == zero)param_rho0s = one

      if(fscaleX == zero) fscaleX = one
      if(fscaleY == zero) fscaleY = one
      if(gscaleX == zero) gscaleX = one
      if(gscaleY == zero) gscaleY = one
      if(hscaleX == zero) hscaleX = one
      if(hscaleY == zero) hscaleY = one

      if(submat_id == 0)submat_id = 1

      param_q = param_t
      !param_q = cv * param_t     ! -> must be done during Starter check to identify adjacent EoS (retrieve t=param_q -> param_q <- cv*t)

      ebcs%title = titr

      ebcs%sensor_id = sensor_id
      ebcs%submat_id = submat_id

      ebcs%a = param_a
      ebcs%n = param_n
      ebcs%q = param_q
      ebcs%rho0s = param_rho0s
      ebcs%has_ielem = .true.
      ebcs%ffunc_id = ffunc_id
      ebcs%gfunc_id = gfunc_id
      ebcs%hfunc_id = hfunc_id
      ebcs%fscaleX = fscaleX
      ebcs%fscaleY = fscaleY
      ebcs%gscaleX = gscaleX
      ebcs%gscaleY = gscaleY
      ebcs%hscaleX = hscaleX
      ebcs%hscaleY = hscaleY

      if(multi_fvm%is_used)then
        ebcs%is_multifluid = .true.
      endif

       ebcs%fvm_inlet_data%func_vel(1:3) = -1
       ebcs%fvm_inlet_data%val_vel(1:3) = zero
       ebcs%fvm_inlet_data%formulation = 2
       ebcs%fvm_inlet_data%vector_velocity = 1
       do imat = 1, 21 !  multi_fvm%nbmat       -> init from nbmat+1,21 to avoid uninit values transmitted to starter
          ebcs%fvm_inlet_data%func_alpha(imat) = -1
          ebcs%fvm_inlet_data%func_rho(imat)   = -1
          ebcs%fvm_inlet_data%func_pres(imat)  = -1
          ebcs%fvm_inlet_data%val_alpha(imat)  = zero
          ebcs%fvm_inlet_data%val_rho(imat)    = zero
          ebcs%fvm_inlet_data%val_pres(imat)   = zero
       enddo

         isu=0
         ingr2usr => igrsurf(1:nsurf)%id
         if (surf /= 0) isu=ngr2usr(surf,ingr2usr,nsurf)
         nseg=0
         if (isu /= 0) nseg=igrsurf(isu)%nseg
         if(surf == 0)then
            ierr=ierr+1
            write(istdo,'(6X,A)')' ** A SURFACE SHOULD BE INPUT'
            write(iout, '(6X,A)')' ** A SURFACE SHOULD BE INPUT'
         elseif(isu == 0)then
            ierr=ierr+1
            write(istdo,*)' ** ERROR SURFACE NOT FOUND, ID=',SURF
            write(iout,*) ' ** ERROR SURFACE NOT FOUND, ID=',SURF
         elseif(nseg == 0)then
            ierr=ierr+1
            write(istdo,*)' ** ERROR EMPTY SURFACE',SURF
            write(iout,*) ' ** ERROR EMPTY SURFACE',SURF
         endif

         ebcs%nb_elem = nseg

         write(iout,1001)id, trim(titr)
         write(iout,1118)surf,sensor_id,submat_id,nseg,param_rho0s, param_q
         write(iout,1201)param_a,param_n,ffunc_id,fscaleX,fscaleY,gfunc_id,gscaleX,gscaleY,hfunc_id,hscaleX,hscaleY
         
      
!-----------
      return
!-----------
 1001 format( //'PROPERGOL EBCS NUMBER. . . . :',I8,1X,A)

 1118 format(&
              '    ON SURFACE  . . . . . . . . . . . . . . . ',I8,/,&
              '    SENSOR ID   . . . . . . . . . . . . . . . ',I8,/,&
              '    SUBMAT ID   . . . . . . . . . . . . . . . ',I8,/,&
              '    NUMBER OF SEGMENTS FOUND. . . . . . . . . ',I8,/,&
              '    PROPERGOL DENSITY . . . . . . . . . . . . ',E20.12,/,&
              '    PROPERGOL HEAT OF COMBUSTION. . . . . . . ',E20.12)
 1201 format( '      --- COMBUSTION MODEL : VIEILLE''S LAW'   ,/,&
              '      VIEILLE PARAMETER A . . . . . . . . . . ',E20.12,/,&
              '      VIEILLE PARAMETER N . . . . . . . . . . ',E20.12,/,&
               '     f(t) function . . . . . . . . . . . . . ',I8,/,&
              '      f X-scale . . . . . . . . . . . . . . . ',E20.12,/,&
              '      f Y-scale . . . . . . . . . . . . . . . ',E20.12,/,&
               '     g(T) function . . . . . . . . . . . . . ',I8,/,&
              '      g X-scale . . . . . . . . . . . . . . . ',E20.12,/,&
              '      g Y-scale . . . . . . . . . . . . . . . ',E20.12,/,&
               '     h(P) function h(P)  . . . . . . . . . . ',I8,/,&
              '      h X-scale . . . . . . . . . . . . . . . ',E20.12,/,&
              '      h Y-scale . . . . . . . . . . . . . . . ',E20.12)

      end subroutine hm_read_ebcs_propergol

