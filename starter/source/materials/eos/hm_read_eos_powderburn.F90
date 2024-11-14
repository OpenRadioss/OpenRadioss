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
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief Reader for Powder Burn EoS
!! \details RHOI = PM(89)   -> provided by /MAT
!! \details RHOR = PM(01)   -> provided by /MAT
!! \details MU0 = RHOI/RHOR-1
!! \details PM(31) = P(MU0,E0) = P0 -> will be used to initialize diagonal of stress tensor SIG(1:3,*) (initial state)
!!
      !||====================================================================
      !||    hm_read_eos_powderburn   ../starter/source/materials/eos/hm_read_eos_powderburn.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_eos              ../starter/source/materials/eos/hm_read_eos.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                   ../starter/source/output/message/message.F
      !||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
      !||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
      !||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
      !||--- uses       -----------------------------------------------------
      !||    elbuftag_mod             ../starter/share/modules1/elbuftag_mod.F
      !||    message_mod              ../starter/share/message_module/message_mod.F
      !||    submodel_mod             ../starter/share/modules1/submodel_mod.F
      !||====================================================================
      subroutine hm_read_eos_powderburn(iout, pm, ipm, unitab, lsubmodel, imideos, mat_param, npropm, npropmi,mtag,&
                                        eos_tag ,ieos)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use unitab_mod
      use submodel_mod
      use message_mod
      use matparam_def_mod , only : matparam_struct_
      use constant_mod , only : zero, one, three100
      use elbuftag_mod , only : mlaw_tag_, eos_tag_, maxeos
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none      
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Include Files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      integer,intent(in) :: ieos !< EoS internal identifier
      integer,intent(in) :: npropm, npropmi !< array sizes
      type (unit_type_),intent(in) ::unitab !< units data structure
      my_real,intent(inout) :: pm(npropm)   !< material (old) buffer (real)
      integer,intent(inout) :: ipm(npropmi) !< material (old) buffer (int)
      type(submodel_data), dimension(nsubmod), intent(in) :: lsubmodel !< submodeling data structure
      integer,intent(in) :: imideos !< eos/mat identifier
      type(matparam_struct_), intent(inout) :: mat_param !material data structure
      integer,intent(in) :: iout !< output unit
      type(mlaw_tag_),intent(inout) :: mtag
      type(eos_tag_),dimension(0:maxeos) ,intent(inout) :: eos_tag
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
      my_real  bulk, p0, psh, dd, eg, gr, cc, alpha, c1, c2, scale_b, scale_p, scale_gam, scale_rho
      my_real rhor, rhoi, e0
      integer func_b, func_gam
      logical :: is_encrypted, is_available, is_available_rho0
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      IS_AVAILABLE_RHO0 = .FALSE.

      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)

      call hm_get_floatv('MAT_BULK', bulk,  is_available,lsubmodel,unitab)
      call hm_get_floatv('LAW5_P0',  p0,    is_available,lsubmodel,unitab)
      call hm_get_floatv('LAW5_PSH', psh,   is_available,lsubmodel,unitab)
      
      call hm_get_floatv('POWDER_D', dd,    is_available,lsubmodel,unitab)
      call hm_get_floatv('POWDER_EG',eg,    is_available,lsubmodel,unitab)

      call hm_get_floatv('POWDER_Gr',gr,    is_available,lsubmodel,unitab)
      call hm_get_floatv('POWDER_C', cc,    is_available,lsubmodel,unitab)
      call hm_get_floatv('Alpha',    alpha, is_available,lsubmodel,unitab)

      call hm_get_floatv('MAT_C1',   c1, is_available,lsubmodel,unitab)
      call hm_get_floatv('MAT_C2',   c2, is_available,lsubmodel,unitab)

      call hm_get_intv  ('F_id_b(P)',   func_b,  is_available,lsubmodel)
      call hm_get_floatv('SCALE_B',  scale_b, is_available,lsubmodel,unitab)
      call hm_get_floatv('SCALE_P',  scale_p, is_available,lsubmodel,unitab)

      call hm_get_intv  ('F_id_g(r)', func_gam,  is_available,lsubmodel)
      call hm_get_floatv('SCALE_GAMMA',scale_gam, is_available,lsubmodel,unitab)
      call hm_get_floatv('SCALE_RHO',scale_rho, is_available,lsubmodel,unitab)

      rhor = pm(1)
      rhoi = pm(89)
      if(rhor == zero)pm(1)=rhoi
      
      ! INPUT CHECKS  
      if(bulk <= zero)then
        call ancmsg(msgid=67,msgtype=msgerror,anmode=aninfo,&
                    I1=imideos,&
                    C1='/EOS/POWDER-BURN',&
                    C2='BULK MODULUS -B- MUST BE STRICTLY POSITIVE')
      end if
      if(DD <= zero)then
        call ancmsg(msgid=67,msgtype=msgerror,anmode=aninfo,&
                    I1=imideos,&
                    C1='/EOS/POWDER-BURN',&
                    C2='PARAMETER -D- MUST BE STRICTLY POSITIVE')
      endif
      if(eg <= zero)then
        call ancmsg(msgid=67,msgtype=msgerror,anmode=aninfo,&
                    I1=imideos,&
                    C1='/EOS/POWDER-BURN',&
                    C2='PARAMETER -eg- MUST BE STRICTLY POSITIVE')
      endif
      
      ! DEFAULT
      if(scale_b == zero) scale_b = one
      if(scale_p == zero) scale_p = one
      if(scale_gam == zero) scale_gam = one
      if(scale_rho == zero) scale_rho = one
      
      ! MATERIAL BUFFER
      pm(88) = psh
      if(pm(79)==zero)pm(79)=three100
      pm(31) = p0-psh
      pm(104)= p0-psh
      pm(27)=sqrt(bulk/rhoi) !ssp0

      e0 = eg*rhoi
      
      ! MAT_PARAM
      mat_param%eos%title = 'powder-burn'
      mat_param%eos%nuparam = 15
      allocate(mat_param%eos%uparam(15))
      mat_param%eos%uparam(1)  = bulk
      mat_param%eos%uparam(1)  = p0
      mat_param%eos%uparam(3)  = psh
      mat_param%eos%uparam(4)  = dd
      mat_param%eos%uparam(5)  = eg
      mat_param%eos%uparam(6)  = gr
      mat_param%eos%uparam(7)  = cc
      mat_param%eos%uparam(8)  = alpha
      mat_param%eos%uparam(9)  = scale_b
      mat_param%eos%uparam(10) = scale_p
      mat_param%eos%uparam(11) = scale_gam
      mat_param%eos%uparam(12) = scale_rho
      mat_param%eos%uparam(13) = c1
      mat_param%eos%uparam(14) = c2
      mat_param%eos%uparam(15) = e0
      
      mat_param%eos%nfunc = 2
      mat_param%eos%func(1) = func_b
      mat_param%eos%func(2) = func_gam
      ipm(10)= 2
      ipm(10+1) = func_b     !set internal id
      ipm(10+2) = func_gam   ! internal id

      ! engine parameters (element buffer)
      EOS_TAG(IEOS)%NVAR = 7

      WRITE(IOUT,1000)
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1500)BULK, P0, PSH, &
                        DD, eg,&
                        GR, CC, ALPHA, &
                        FUNC_B, SCALE_B, SCALE_P,&
                        FUNC_GAM, SCALE_GAM, SCALE_RHO, C1, C2
      ENDIF

      MTAG%G_TB     = 1
      MTAG%G_TEMP   = 1
      MTAG%G_BFRAC  = 1
      MTAG%G_ABURN  = 1
      MTAG%L_TB     = 1
      MTAG%L_TEMP   = 1
      MTAG%L_BFRAC  = 1
      MTAG%L_ABURN  = 1

      RETURN
! ----------------------------------------------------------------------------------------------------------------------
 1000 FORMAT(&
      5X,'  POWDER BURN EOS',/,&
      5X,'  ---------------------------------------------------     ',/)

 1500 FORMAT(&
      5X,'__ powder eos __',/&
      5X,'B         - BULK MODULUS  . . . . . . . . .  =',1PG20.13/,&
      5X,'P0        - INITIAL PRESSURE  . . . . . . .  =',1PG20.13/,&
      5X,'PSH       - PRESSURE SHIFT  . . . . . . . .  =',1PG20.13/,&
      5X,'__ gas eos __',/&
      5X,'D         - PARAMETER 1   . . . . . . . . .  =',1PG20.13/,&
      5X,'EG        - PARAMETER 2 . . . . . . . . . .  =',1PG20.13/,&
      5X,'__ growth model __',/&
      5X,'GR        - GROWTH PARAMETER  . . . . . . .  =',1PG20.13/,&
      5X,'C         - GROWTH REACTION RATIO EXPONENT.  =',1PG20.13/,&
      5X,'ALPHA     - REACTION RATIO FACTOR . . . . .  =',1PG20.13/,&
      5X,'__ burning rate __',/&
      5X,'FUNC_B    - FUNCTION IDENTIER . . . . . . .  =',1PG20.13/,&
      5X,'SCALE_B   - Y-AXIS SCALE FACTOR . . . . . .  =',1PG20.13/,&
      5X,'SCALE_P   - X-AXIS SCALE FACTOR . . . . . .  =',1PG20.13/,&
      5X,'__ speed of ignition __',/&
      5X,'FUNC_GAM  - FUNCTION IDENTIFIER . . . . . .  =',1PG20.13/,&
      5X,'SCALE_GAM - Y-AXIS SCALE FACTOR . . . . . .  =',1PG20.13/,&
      5X,'SCALE_RHO - X-AXIS SCALE FACTOR . . . . . .  =',1PG20.13/,&
      5X,'C1        - PARAMETER 1 . . . . . . . . . .  =',1PG20.13/,&
      5X,'C2        - PARAMETER 2 . . . . . . . . . .  =',1PG20.13)

      RETURN
      END SUBROUTINE HM_READ_EOS_POWDERBURN
