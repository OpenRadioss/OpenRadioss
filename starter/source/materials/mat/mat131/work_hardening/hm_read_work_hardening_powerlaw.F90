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
!||    hm_read_work_hardening_powerlaw_mod   ../starter/source/materials/mat/mat131/work_hardening/hm_read_work_hardening_powerlaw.F90
!||--- called by ------------------------------------------------------
!||    hm_read_work_hardening                ../starter/source/materials/mat/mat131/work_hardening/hm_read_work_hardening.F90
!||====================================================================
      module hm_read_work_hardening_powerlaw_mod
! \brief Read power law work hardening input data for /MAT/LAW131
! \details Read the power law (Hollomon/Ludwik) work hardening model
!          parameters for /MAT/LAW131.
      contains
!||====================================================================
!||    hm_read_work_hardening_powerlaw   ../starter/source/materials/mat/mat131/work_hardening/hm_read_work_hardening_powerlaw.F90
!||--- called by ------------------------------------------------------
!||    hm_read_work_hardening            ../starter/source/materials/mat/mat131/work_hardening/hm_read_work_hardening.F90
!||--- calls      -----------------------------------------------------
!||    ancmsg                            ../starter/source/output/message/message.F
!||    hm_get_float_array_index          ../starter/source/devtools/hm_reader/hm_get_float_array_index.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod                ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod                       ../starter/share/message_module/message_mod.F
!||    submodel_mod                      ../starter/share/modules1/submodel_mod.F
!||====================================================================
        subroutine hm_read_work_hardening_powerlaw(                            &
          ikey     ,ihard    ,nupar_hard,upar_hard,is_available,               &
          unitab   ,lsubmodel,iout     ,is_encrypted,iflag     ,               &
          titr     ,mat_id   ,matparam )
!----------------------------------------------------------------
!   M o d u l e s
!----------------------------------------------------------------
          use unitab_mod
          use submodel_mod
          use hm_option_read_mod
          use constant_mod
          use matparam_def_mod
          use precision_mod, only : WP
          use message_mod
!----------------------------------------------------------------
!   I m p l i c i t   T y p e s
!----------------------------------------------------------------
          implicit none
!----------------------------------------------------------------
!  I n p u t   A r g u m e n t s
!----------------------------------------------------------------
          integer,                 intent(in)    :: ikey                  !< Material key
          integer,                 intent(inout) :: ihard                 !< Work hardening type
          integer,                 intent(inout) :: nupar_hard            !< Number of work hardening parameters
          real(kind=WP),dimension(100),intent(inout) :: upar_hard         !< Work hardening parameters
          logical,                 intent(in)    :: is_available          !< Availability flag
          type(unit_type_),        intent(in)    :: unitab                !< Units table
          type(submodel_data),dimension(nsubmod),intent(in) :: lsubmodel  !< Submodel data structure
          integer,                 intent(in)    :: iout                  !< Output unit
          logical,                 intent(in)    :: is_encrypted          !< Encryption flag
          integer,                 intent(in)    :: iflag                 !< Flag for input format (1: simplified, 2: classic)
          character(len=nchartitle),intent(in)   :: titr                  !< Material law user title
          integer,                 intent(in)    :: mat_id                !< Material law user ID
          type(matparam_struct_),  intent(inout) :: matparam              !< Matparam data structure
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
          real(kind=WP) :: ca,cb,cn,eps0,sigmax,cb0,rm,ag,cn0,young
!===============================================================================
!            
          !=====================================================================
          !< Power law hardening parameters
          !=====================================================================
          if (iflag == 1) then 
            call hm_get_float_array_index("HARD_POWERLAW_SIGY"  ,ca,ikey,is_available,lsubmodel,unitab)
            call hm_get_float_array_index("HARD_POWERLAW_UTS"   ,cb,ikey,is_available,lsubmodel,unitab)
            call hm_get_float_array_index("HARD_POWERLAW_EPSUTS",cn,ikey,is_available,lsubmodel,unitab)
            cb0 = cb
            rm  = cb *(one+cn)
            ag  = log(one+cn)
            cn0 = cn
            cn  = rm*ag / max((rm-ca),em20)
            cb  = rm/max((cn*ag**(cn-one)),em20)
            young = matparam%young
            if (young == zero) then 
              call ancmsg(msgid=3131,                                          &
                msgtype=msgerror,                                              &
                anmode=aninfo_blind_2,                                         &
                i1=mat_id,                                                     &
                c1="ERROR",                                                    &
                c2=titr,                                                       &
                c3="HARD_POWERLAW_ENG",                                        &
                c4="ELASTIC BEHAVIOR IS NOT DEFINED (YOUNG'S MODULUS IS ZERO)" //&
                   " PLEASE DEFINE ELAS KEY TYPE PRIOR HARD KEY TYPE")
            endif
            if (cn > one) then
              cn = one
              cb = (cb0*(one+cn0)-ca)/(log(one+cn0)-cb0*(one+cn0)/young-ca/young)
              call ancmsg(msgid=3131,                                          &
                msgtype=msgwarning,                                            &
                anmode=aninfo_blind_2,                                         &
                i1=mat_id,                                                     &
                c1="WARNING",                                                  &
                c2=titr,                                                       &
                c3="HARD_POWERLAW_ENG",                                        &
                c4="INPUT DATA NOT VALID FOR AUTOMATIC PARAMETER FITTING" //   &
                   " N IS SET TO 1.0 AND B IS RECOMPUTED ACCORDINGLY" //       &
                   " RESULT WILL NOT FIT INPUT VALUES")
            endif
            if (cn < zero .and. cb < zero) then
              cn = zero
              cb = zero
              call ancmsg(msgid=3131,                                          &
                msgtype=msgwarning,                                            &
                anmode=aninfo_blind_2,                                         &
                i1=mat_id,                                                     &
                c1="WARNING",                                                  &
                c2=titr,                                                       &
                c3="HARD_POWERLAW_ENG",                                        &
                c4="INPUT DATA NOT VALID FOR AUTOMATIC PARAMETER FITTING " //  &
                   " N IS SET TO 0.0 AND B IS SET TO 0.0 " //                  &
                   " RESULT WILL NOT FIT INPUT VALUES")
            endif            
          elseif (iflag == 2) then
            call hm_get_float_array_index("HARD_POWERLAW_A",ca,ikey,is_available,lsubmodel,unitab)
            call hm_get_float_array_index("HARD_POWERLAW_B",cb,ikey,is_available,lsubmodel,unitab)
            call hm_get_float_array_index("HARD_POWERLAW_N",cn,ikey,is_available,lsubmodel,unitab)
          endif
          call hm_get_float_array_index("HARD_POWERLAW_EPS0"  ,eps0  ,ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("HARD_POWERLAW_SIGMAX",sigmax,ikey,is_available,lsubmodel,unitab)
          !< Work hardening type
          ihard = 1
          !< Default initial plastic strain
          if (eps0   == zero) eps0 = em20
          if (sigmax == zero) sigmax = infinity
          !< Number of parameters
          nupar_hard = 5
          !< Save hardening parameters
          upar_hard(1) = ca
          upar_hard(2) = cb
          upar_hard(3) = cn
          upar_hard(4) = eps0
          upar_hard(5) = sigmax
          !< Printing work hardening parameters
          if (is_encrypted)then
            write(iout,"(5X,A,//)") "CONFIDENTIAL DATA"
          else
            if (iflag == 2) then 
              write(iout,1000) ca,cb,cn,eps0,sigmax
            elseif (iflag == 1) then
              write(iout,2000) ca,cb0,cn0,eps0,sigmax,cb,cn
            endif
          endif
! ------------------------------------------------------------------------------
1000 format(/                                                                  &
          5X,"-------------------------------------------------------",/       &
          5X,"POWER LAW WORK HARDENING                               ",/,      &
          5X,"-------------------------------------------------------",/,      &
          5X,"INITIAL YIELD STRESS (A) . . . . . . . . . . . . . . .=",1PG20.13/&
          5X,"HARDENING MODULUS (B). . . . . . . . . . . . . . . . .=",1PG20.13/&
          5X,"HARDENING EXPONENT (N) . . . . . . . . . . . . . . . .=",1PG20.13/&
          5X,"INITIAL PLASTIC STRAIN (EPS0). . . . . . . . . . . . .=",1PG20.13/&
          5X,"MAXIMUM YIELD STRESS (SIGMAX). . . . . . . . . . . . .=",1PG20.13/)
2000 format(/                                                                  &
          5X,"-------------------------------------------------------",/       &
          5X,"POWER LAW WORK HARDENING                               ",/,      &
          5X,"-------------------------------------------------------",/,      &
          5X,"INITIAL YIELD STRESS (A) . . . . . . . . . . . . . . .=",1PG20.13/&
          5X,"ULTIMATE TENSILE STRENGTH (UTS). . . . . . . . . . . .=",1PG20.13/&
          5X,"ENGINEERING STRAIN AT UTS (EPS_UTS). . . . . . . . . .=",1PG20.13/&
          5X,"INITIAL PLASTIC STRAIN (EPS0). . . . . . . . . . . . .=",1PG20.13/&
          5X,"MAXIMUM YIELD STRESS (SIGMAX). . . . . . . . . . . . .=",1PG20.13/&
          5X,"COMPUTED HARDENING MODULUS (B) . . . . . . . . . . . .=",1PG20.13/&
          5X,"COMPUTED HARDENING EXPONENT (N). . . . . . . . . . . .=",1PG20.13/)
! -------------------------------------------------------------------------------
        end subroutine hm_read_work_hardening_powerlaw
      end module hm_read_work_hardening_powerlaw_mod