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
!||    hm_read_elasticity_temp_isotropic_mod   ../starter/source/materials/mat/mat131/elasticity/hm_read_elasticity_temp_isotropic.F90
!||--- called by ------------------------------------------------------
!||    hm_read_elasticity                      ../starter/source/materials/mat/mat131/elasticity/hm_read_elasticity.F90
!||====================================================================
      module hm_read_elasticity_temp_isotropic_mod
        implicit none
! \brief Read temperature-dependent isotropic elasticity input data for /MAT/LAW131
! \details Read the temperature-dependent isotropic elasticity model
!          parameters for /MAT/LAW131 (elasto-plastic material law).
      contains
!||====================================================================
!||    hm_read_elasticity_temp_isotropic   ../starter/source/materials/mat/mat131/elasticity/hm_read_elasticity_temp_isotropic.F90
!||--- called by ------------------------------------------------------
!||    hm_read_elasticity                  ../starter/source/materials/mat/mat131/elasticity/hm_read_elasticity.F90
!||--- calls      -----------------------------------------------------
!||    ancmsg                              ../starter/source/output/message/message.F
!||    hm_get_float_array_index            ../starter/source/devtools/hm_reader/hm_get_float_array_index.F
!||    hm_get_int_array_index              ../starter/source/devtools/hm_reader/hm_get_int_array_index.F
!||--- uses       -----------------------------------------------------
!||    elbuftag_mod                        ../starter/share/modules1/elbuftag_mod.F
!||    hm_option_read_mod                  ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod                         ../starter/share/message_module/message_mod.F
!||    submodel_mod                        ../starter/share/modules1/submodel_mod.F
!||====================================================================
        subroutine hm_read_elasticity_temp_isotropic(                          &
          ikey     ,ielas    ,nupar_elas,is_available,                         &
          unitab   ,lsubmodel,matparam ,parmat    ,iout        ,is_encrypted,  &
          mat_id   ,titr     ,ntab_elas,itab_elas ,x2vect      ,x3vect      ,  &
          x4vect   ,fscale   ,nvartmp  ,mtag      ,nuvar_elas  )
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
          use elbuftag_mod
!----------------------------------------------------------------
!   I m p l i c i t   T y p e s
!----------------------------------------------------------------
          implicit none
!----------------------------------------------------------------
!  I n p u t   A r g u m e n t s
!----------------------------------------------------------------
          integer,                 intent(in)    :: ikey                  !< Material key
          integer,                 intent(inout) :: ielas                 !< Elastic model type
          integer,                 intent(inout) :: nupar_elas            !< Number of elastic parameters
          logical,                 intent(in)    :: is_available          !< Availability flag
          type(unit_type_),        intent(in)    :: unitab                !< Units table
          type(submodel_data),dimension(nsubmod),intent(in) :: lsubmodel  !< Submodel data structure
          type(matparam_struct_),  intent(inout) :: matparam              !< Matparam data structure
          real(kind=WP),           intent(inout) :: parmat(100)           !< Material parameter global table 1
          integer,                 intent(in)    :: iout                  !< Output unit
          logical,                 intent(in)    :: is_encrypted          !< Encryption flag
          integer,                 intent(in)    :: mat_id                !< Material law user ID
          character(len=nchartitle),intent(in)   :: titr                  !< Material law user title
          integer,                 intent(inout) :: ntab_elas             !< Number of tables used in elasticity
          integer,      dimension(100),intent(inout) :: itab_elas         !< Identifiers of tabulated elasticity functions/tables
          real(kind=WP),dimension(100),intent(inout) :: x2vect            !< x2 scale factor for tabulated elasticity
          real(kind=WP),dimension(100),intent(inout) :: x3vect            !< x3 scale factor for tabulated elasticity
          real(kind=WP),dimension(100),intent(inout) :: x4vect            !< x4 scale factor for tabulated elasticity
          real(kind=WP),dimension(100),intent(inout) :: fscale            !< y scale factor for tabulated elasticity
          integer,                 intent(inout) :: nvartmp               !< Number of temporary variables used in tabulated elasticity
          type(mlaw_tag_),         intent(inout) :: mtag                  !< Material tag for internal variables in element buffer
          integer,                 intent(inout) :: nuvar_elas            !< Number of user variables for elasticity
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
          integer :: fct_id_heat,fct_id_cool,fct_id_nu
          real(kind=WP) :: young,nu
!===============================================================================
! 
          !=====================================================================
          !< Elastic temperature-dependent isotropic parameters
          !=====================================================================
          call hm_get_float_array_index("ELAS_TEMP_E"    ,young      ,ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("ELAS_TEMP_NU"   ,nu         ,ikey,is_available,lsubmodel,unitab)
          call hm_get_int_array_index  ("ELAS_TEMP_FCTH" ,fct_id_heat,ikey,is_available,lsubmodel)
          call hm_get_int_array_index  ("ELAS_TEMP_FCTC" ,fct_id_cool,ikey,is_available,lsubmodel)
          call hm_get_int_array_index  ("ELAS_TEMP_FCTN" ,fct_id_nu  ,ikey,is_available,lsubmodel)
          !< Check parameters values
          if (nu < zero .or. nu >= half) then
            call ancmsg(msgid=3131,                                            &
                        msgtype=msgerror,                                      &
                        anmode=aninfo_blind_2,                                 &
                        i1=mat_id,                                             &
                        c1="ERROR",                                            &
                        c2=titr,                                               &
                        c3="ELAS_TEMP_ISOTROPIC",                              &
                        c4="POISSON'S RATIO MUST BE IN THE RANGE [0,0.5[.")
          endif
          !< Fill MATPARAM values
          matparam%young = young
          matparam%nu    = nu
          matparam%shear = young/(two*(one + nu))
          matparam%bulk  = young/(three*(one - two*nu))
          !< Fill PARMAT values
          parmat(1)  = matparam%bulk
          parmat(2)  = matparam%young
          parmat(3)  = matparam%nu
          parmat(16) = 2
          parmat(17) = (one - two*nu)/(one - nu)
          !< Elasticity type
          ielas = 5
          !< Number of parameters
          nupar_elas = 0
          !< Number of variables used in tabulated viscous elasticity
          nvartmp = 3
          !< Number of tables used in elasticity
          ntab_elas = 3
          !< Number of user variables for elasticity
          nuvar_elas = 3
          !< Save table id
          itab_elas(1) = fct_id_heat
          itab_elas(2) = fct_id_cool
          itab_elas(3) = fct_id_nu
          !< Save scale factors
          x2vect(1:3) = one
          x3vect(1:3) = one
          x4vect(1:3) = one
          fscale(1:2) = young
          fscale(3)   = nu 
          !< Set temperature variable tag
          if (mtag%g_temp == 0) mtag%g_temp = 1
          if (mtag%l_temp == 0) mtag%l_temp = 1
          !< Activate heat source calculation in material
          if (matparam%heat_flag == 0) matparam%heat_flag = 1
          !< Printing elastic parameters
          if (is_encrypted) then
            write(iout,"(5X,A,//)") "CONFIDENTIAL DATA"
          else
            write(iout,1000) young,nu,fct_id_heat,fct_id_cool,fct_id_nu
          endif
! ------------------------------------------------------------------------------
1000 format(/                                                                  &
          5X,"-------------------------------------------------------",/       &
          5X,"TEMPERATURE-DEPENDENT ISOTROPIC ELASTICITY             ",/,      &
          5X,"-------------------------------------------------------",/,      &
          5X,"YOUNG MODULUS (E). . . . . . . . . . . . . . . . . . .=",1PG20.13/&
          5X,"POISSON RATIO (NU) . . . . . . . . . . . . . . . . . .=",1PG20.13/&
          5X,'FUNCTION E(T) (HEATING) ID . . . . . . . . . . . . . .=',I10/    &
          5X,'FUNCTION E(T) (COOLING) ID . . . . . . . . . . . . . .=',I10/    &
          5X,'FUNCTION NU(T) ID. . . . . . . . . . . . . . . . . . .=',I10/)
! -------------------------------------------------------------------------------
        end subroutine hm_read_elasticity_temp_isotropic
      end module hm_read_elasticity_temp_isotropic_mod