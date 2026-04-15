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
!||    hm_read_kinematic_hardening_chaboche_mod   ../starter/source/materials/mat/mat131/kinematic_hardening/hm_read_kinematic_hardening_chaboche.F90
!||--- called by ------------------------------------------------------
!||    hm_read_kinematic_hardening                ../starter/source/materials/mat/mat131/kinematic_hardening/hm_read_kinematic_hardening.F90
!||====================================================================
      module hm_read_kinematic_hardening_chaboche_mod
! \brief Read Chaboche kinematic hardening input data for /MAT/LAW131
! \details Read the Chaboche nonlinear kinematic hardening model parameters
!          for /MAT/LAW131.
      contains
!||====================================================================
!||    hm_read_kinematic_hardening_chaboche   ../starter/source/materials/mat/mat131/kinematic_hardening/hm_read_kinematic_hardening_chaboche.F90
!||--- called by ------------------------------------------------------
!||    hm_read_kinematic_hardening            ../starter/source/materials/mat/mat131/kinematic_hardening/hm_read_kinematic_hardening.F90
!||--- calls      -----------------------------------------------------
!||    hm_get_float_array_index               ../starter/source/devtools/hm_reader/hm_get_float_array_index.F
!||--- uses       -----------------------------------------------------
!||    elbuftag_mod                           ../starter/share/modules1/elbuftag_mod.F
!||    hm_option_read_mod                     ../starter/share/modules1/hm_option_read_mod.F
!||    submodel_mod                           ../starter/share/modules1/submodel_mod.F
!||====================================================================
        subroutine hm_read_kinematic_hardening_chaboche(                       &
          ikey     ,ikine    ,nupar_kine,upar_kine  ,chard     ,is_available,  &
          unitab   ,lsubmodel,iout     ,is_encrypted,mtag      )
!----------------------------------------------------------------
!   M o d u l e s
!----------------------------------------------------------------
          use unitab_mod
          use submodel_mod
          use hm_option_read_mod
          use constant_mod
          use matparam_def_mod
          use elbuftag_mod
          use precision_mod, only : WP
!----------------------------------------------------------------
!   I m p l i c i t   T y p e s
!----------------------------------------------------------------
          implicit none
!----------------------------------------------------------------
!  I n p u t   A r g u m e n t s
!----------------------------------------------------------------
          integer,                 intent(in)    :: ikey                  !< Material key
          integer,                 intent(inout) :: ikine                 !< Kinematic hardening type
          integer,                 intent(inout) :: nupar_kine            !< Number of kinematic hardening parameters
          real(kind=WP),dimension(100),intent(inout) :: upar_kine         !< Kinematic hardening parameters
          real(kind=WP),           intent(inout) :: chard                 !< Kinematic hardening parameter
          logical,                 intent(in)    :: is_available          !< Availability flag
          type(unit_type_),        intent(in)    :: unitab                !< Units table
          type(submodel_data),dimension(nsubmod),intent(in) :: lsubmodel  !< Submodel data structure
          integer,                 intent(in)    :: iout                  !< Output unit
          logical,                 intent(in)    :: is_encrypted          !< Encryption flag
          type(mlaw_tag_),         intent(inout) :: mtag                  !< Material tag for internal variables in element buffer
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
          real(kind=WP) :: crc1, cra1, crc2, cra2, crc3, cra3, crc4, cra4
!===============================================================================
!            
          !===================================================================
          !< Chaboche-Rousselier kinematic hardening parameters
          !===================================================================
          call hm_get_float_array_index("KINE_PRAG_CHARD",chard,ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("KINE_CHAB_CRC1" ,crc1 ,ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("KINE_CHAB_CRA1" ,cra1 ,ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("KINE_CHAB_CRC2" ,crc2 ,ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("KINE_CHAB_CRA2" ,cra2 ,ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("KINE_CHAB_CRC3" ,crc3 ,ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("KINE_CHAB_CRA3" ,cra3 ,ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("KINE_CHAB_CRC4" ,crc4 ,ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("KINE_CHAB_CRA4" ,cra4 ,ikey,is_available,lsubmodel,unitab)
          !< Kinematic hardening type
          ikine = 2
          !< Number of parameters
          nupar_kine = 8
          !< Save kinematic hardening parameters
          upar_kine(1) = crc1
          upar_kine(2) = cra1
          upar_kine(3) = crc2
          upar_kine(4) = cra2
          upar_kine(5) = crc3
          upar_kine(6) = cra3
          upar_kine(7) = crc4
          upar_kine(8) = cra4
          !< Backstresses needed in case of kinematic hardening
          mtag%l_sigb = 24
          !< Printing kinematic hardening parameters
          if (is_encrypted)then
            write(iout,"(5X,A,//)") "CONFIDENTIAL DATA"
          else
            write(iout,1000) chard,crc1,cra1,crc2,cra2,crc3,cra3,crc4,cra4
          endif
! ------------------------------------------------------------------------------
1000 format(/                                                                   &
          5X,"-------------------------------------------------------",/        &
          5X,"CHABOCHE-ROUSSELIER KINEMATIC HARDENING                ",/,       &
          5X,"-------------------------------------------------------",/,       &
          5X,"ISOTROPIC/KINEMATIC HARDENING PARAMETER (CHARD). . . .=",1PG20.13/&
          5X,"PARAMETER C1 . . . . . . . . . . . . . . . . . . . . .=",1PG20.13/&
          5X,"PARAMETER A1 . . . . . . . . . . . . . . . . . . . . .=",1PG20.13/&
          5X,"PARAMETER C2 . . . . . . . . . . . . . . . . . . . . .=",1PG20.13/&
          5X,"PARAMETER A2 . . . . . . . . . . . . . . . . . . . . .=",1PG20.13/&
          5X,"PARAMETER C3 . . . . . . . . . . . . . . . . . . . . .=",1PG20.13/&
          5X,"PARAMETER A3 . . . . . . . . . . . . . . . . . . . . .=",1PG20.13/&
          5X,"PARAMETER C4 . . . . . . . . . . . . . . . . . . . . .=",1PG20.13/&
          5X,"PARAMETER A4 . . . . . . . . . . . . . . . . . . . . .=",1PG20.13/)
! -------------------------------------------------------------------------------
        end subroutine hm_read_kinematic_hardening_chaboche
      end module hm_read_kinematic_hardening_chaboche_mod