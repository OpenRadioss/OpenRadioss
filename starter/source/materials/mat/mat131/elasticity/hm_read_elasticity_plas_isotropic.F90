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
      module hm_read_elasticity_plas_isotropic_mod
        implicit none
! \brief Read plastic strain dependent isotropic elasticity input data for /MAT/LAW131
! \details Read the plastic strain dependent isotropic elasticity model
!          parameters for /MAT/LAW131 (elasto-plastic material law).
      contains
        subroutine hm_read_elasticity_plas_isotropic(                          &
          ikey     ,ielas    ,nupar_elas,is_available,                         &
          unitab   ,lsubmodel,matparam ,parmat    ,iout        ,is_encrypted,  &
          mat_id   ,titr     ,ntab_elas,itab_elas ,x2vect      ,x3vect      ,  &
          x4vect   ,fscale   ,nvartmp  ,nuvar_elas,upar_elas   )
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
          integer,                 intent(inout) :: nuvar_elas            !< Number of user variables for elasticity
          real(kind=WP),dimension(100),intent(inout) :: upar_elas         !< Elastic parameters
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
          integer :: fct_id_e
          real(kind=WP) :: young,nu,einf,ce
!===============================================================================
! 
          !=====================================================================
          !< Elastic plastic straindependent isotropic parameters
          !=====================================================================
          call hm_get_float_array_index("ELAS_PLAS_E"    ,young      ,ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("ELAS_PLAS_NU"   ,nu         ,ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("ELAS_PLAS_EINF" ,einf       ,ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("ELAS_PLAS_CE"   ,ce         ,ikey,is_available,lsubmodel,unitab)
          call hm_get_int_array_index  ("ELAS_PLAS_FCTE" ,fct_id_e   ,ikey,is_available,lsubmodel)
          !< Check parameters values
          if (nu < zero .or. nu >= half) then
            call ancmsg(msgid=3131,                                            &
                        msgtype=msgerror,                                      &
                        anmode=aninfo_blind_2,                                 &
                        i1=mat_id,                                             &
                        c1="ERROR",                                            &
                        c2=titr,                                               &
                        c3="ELAS_PLAS_ISOTROPIC",                              &
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
          ielas = 7
          !< Number of parameters
          nupar_elas = 2
          !< Number of variables used in tabulated plastic strain dependent elasticity
          nvartmp = 1
          !< Number of tables used in elasticity
          ntab_elas = 1
          !< Number of user variables for elasticity
          nuvar_elas = 1
          !< Save table id
          itab_elas(1) = fct_id_e
          !< Save scale factors
          x2vect(1) = one
          x3vect(1) = one
          x4vect(1) = one
          fscale(1) = young
          !< Save elastic parameters
          upar_elas(1) = einf
          upar_elas(2) = ce
          !< Printing elastic parameters
          if (is_encrypted) then
            write(iout,"(5X,A,//)") "CONFIDENTIAL DATA"
          else
            write(iout,1000) young,nu
            if (fct_id_e > 0) then 
              write(iout,2000) fct_id_e
            else
              write(iout,2001) einf,ce
            endif
          endif
! ------------------------------------------------------------------------------
1000 format(/                                                                  &
          5X,"-------------------------------------------------------",/       &
          5X,"PLASTIC STRAIN DEPENDENT ISOTROPIC ELASTICITY          ",/,      &
          5X,"-------------------------------------------------------",/,      &
          5X,"INITIAL YOUNG MODULUS (E). . . . . . . . . . . . . . .=",1PG20.13/&
          5X,"POISSON RATIO (NU) . . . . . . . . . . . . . . . . . .=",1PG20.13)
2000 format(/                                                                  &
          5X,"TABULATED DEPENDENCY                                   ",/,      &
          5X,"--------------------                                   ",/,      &
          5X,"FUNCTION E(PLASTIC STRAIN) ID. . . . . . . . . . . . .=",I10/)
2001 format(/                                                                  &
          5X,"ANALYTICAL EXPONENTIAL DEPENDENCY                      ",/,      &
          5X,"---------------------------------                      ",/,      &
          5X,"SATURATED YOUNG MODULUS (EINF) . . . . . . . . . . . .=",1PG20.13/&
          5X,"YOUNG MODULUS SATURATION RATE (CE) . . . . . . . . . .=",1PG20.13/)
! -------------------------------------------------------------------------------
        end subroutine hm_read_elasticity_plas_isotropic
      end module hm_read_elasticity_plas_isotropic_mod