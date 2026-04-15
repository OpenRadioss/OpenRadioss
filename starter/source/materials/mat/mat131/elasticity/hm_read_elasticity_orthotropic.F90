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
!||    hm_read_elasticity_orthotropic_mod   ../starter/source/materials/mat/mat131/elasticity/hm_read_elasticity_orthotropic.F90
!||--- called by ------------------------------------------------------
!||    hm_read_elasticity                   ../starter/source/materials/mat/mat131/elasticity/hm_read_elasticity.F90
!||====================================================================
      module hm_read_elasticity_orthotropic_mod
        implicit none
! \brief Read orthotropic elasticity input data for /MAT/LAW131
! \details Read the orthotropic elasticity model parameters
!          for /MAT/LAW131 (elasto-plastic material law).
      contains
!||====================================================================
!||    hm_read_elasticity_orthotropic   ../starter/source/materials/mat/mat131/elasticity/hm_read_elasticity_orthotropic.F90
!||--- called by ------------------------------------------------------
!||    hm_read_elasticity               ../starter/source/materials/mat/mat131/elasticity/hm_read_elasticity.F90
!||--- calls      -----------------------------------------------------
!||    ancmsg                           ../starter/source/output/message/message.F
!||    hm_get_float_array_index         ../starter/source/devtools/hm_reader/hm_get_float_array_index.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod               ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod                      ../starter/share/message_module/message_mod.F
!||    submodel_mod                     ../starter/share/modules1/submodel_mod.F
!||====================================================================
        subroutine hm_read_elasticity_orthotropic(                             &
          ikey     ,ielas    ,nupar_elas,upar_elas,is_available,               &
          unitab   ,lsubmodel,matparam ,parmat    ,iout        ,is_encrypted,  &
          mat_id   ,titr     )
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
          integer,                 intent(inout) :: ielas                 !< Elastic model type
          integer,                 intent(inout) :: nupar_elas            !< Number of elastic parameters
          real(kind=WP),dimension(100),intent(inout) :: upar_elas         !< Elastic parameters
          logical,                 intent(in)    :: is_available          !< Availability flag
          type(unit_type_),        intent(in)    :: unitab                !< Units table
          type(submodel_data),dimension(nsubmod),intent(in) :: lsubmodel  !< Submodel data structure
          type(matparam_struct_),  intent(inout) :: matparam              !< Matparam data structure
          real(kind=WP),           intent(inout) :: parmat(100)           !< Material parameter global table 1
          integer,                 intent(in)    :: iout                  !< Output unit
          logical,                 intent(in)    :: is_encrypted          !< Encryption flag
          integer,                 intent(in)    :: mat_id                !< Material law user ID
          character(len=nchartitle),intent(in)   :: titr                  !< Material law user title
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
          real(kind=WP) :: e1,e2,e3,nu12,nu21,nu23,nu32,nu13,nu31,g12,g23,g31
          real(kind=WP) :: a11,a12,a22,c11,c12,c13,c22,c23,c33
          real(kind=WP) :: d11,d12,d13,d22,d23,d33,detc,dmn,dmx
!===============================================================================
! 
          !===================================================================
          !< Elastic orthotropic parameters
          !===================================================================
          ielas = 2
          call hm_get_float_array_index("ELAS_ORTH_E1"  ,e1  ,ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("ELAS_ORTH_E2"  ,e2  ,ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("ELAS_ORTH_E3"  ,e3  ,ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("ELAS_ORTH_NU12",nu12,ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("ELAS_ORTH_NU23",nu23,ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("ELAS_ORTH_NU31",nu31,ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("ELAS_ORTH_G12" ,g12 ,ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("ELAS_ORTH_G23" ,g23 ,ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("ELAS_ORTH_G31" ,g31 ,ikey,is_available,lsubmodel,unitab)
          !< Poisson's ratio
          nu21 = nu12*e2/e1 
          nu32 = nu23*e3/e2
          nu13 = nu31*e1/e3
          !< Check parameters values
          if (nu12*nu21 >= one) then 
            call ancmsg(msgid=3131,                                            &
                        msgtype=msgerror,                                      &
                        anmode=aninfo_blind_2,                                 &
                        i1=mat_id,                                             &
                        c1="ERROR",                                            &
                        c2=titr,                                               &
                        c3="ELAS_ORTHOTROPIC",                                 &
                        c4="TO REMAIN NUMERICALLY STABLE, MATERIAL CONSTANTS"//&
                     " E1, E2 AND NU21 MUST BE INPUT SUCH THAT NU12*NU21< 1,"//&
                     "  WHERE NU12 = NU21*E1/E2")
          endif
          if (nu13*nu31 >= one) then 
            call ancmsg(msgid=3131,                                            &
                        msgtype=msgerror,                                      &
                        anmode=aninfo_blind_2,                                 &
                        i1=mat_id,                                             &
                        c1="ERROR",                                            &
                        c2=titr,                                               &
                        c3="ELAS_ORTHOTROPIC",                                 &
                        c4="TO REMAIN NUMERICALLY STABLE, MATERIAL CONSTANTS"//&
                     " E1, E3 AND NU31 MUST BE INPUT SUCH THAT NU13*NU31< 1,"//&
                     "  WHERE NU13 = NU31*E1/E3")
          endif
          if (nu23*nu32 >= one) then 
            call ancmsg(msgid=3131,                                            &
                        msgtype=msgerror,                                      &
                        anmode=aninfo_blind_2,                                 &
                        i1=mat_id,                                             &
                        c1="ERROR",                                            &
                        c2=titr,                                               &
                        c3="ELAS_ORTHOTROPIC",                                 &
                        c4="TO REMAIN NUMERICALLY STABLE, MATERIAL CONSTANTS"//&
                     " E2, E3 AND NU32 MUST BE INPUT SUCH THAT NU23*NU32< 1,"//&
                     "  WHERE NU23 = NU32*E2/E3")
          endif
          !< Elasticity matrix for 2D plane stress
          a11  = e1/(one - nu12*nu21)
          a12  = nu21*a11
          a22  = e2/(one - nu12*nu21)
          !< Compliance matrix for 3D
          c11  =   one/e1
          c22  =   one/e2
          c33  =   one/e3
          c12  = -nu12/e1
          c13  = -nu31/e3
          c23  = -nu23/e2
          !< 3D Elasticity matrix
          detc =  c11*c22*c33-c11*c23*c23-c12*c12*c33+c12*c13*c23              &
                + c13*c12*c23-c13*c22*c13
          d11  =  (c22*c33-c23*c23)/detc
          d12  = -(c12*c33-c13*c23)/detc
          d13  =  (c12*c23-c13*c22)/detc
          d22  =  (c11*c33-c13*c13)/detc
          d23  = -(c11*c23-c13*c12)/detc
          d33  =  (c11*c22-c12*c12)/detc  
          dmn  =  min(d11*d22 -d12**2,d11*d33 - d13**2,d22*d33 - d23**2)      
          dmx  =  max(d11,d22,d33)
          !< Fill matparam values
          matparam%young = max(e1,e2,e3)
          matparam%nu    = max(nu12,nu23,nu31)
          matparam%shear = max(g12,g23,g31)
          matparam%bulk  = one/(c11+c22+c33+two*(c12+c23+c13))
          !< Fill PARMAT values
          parmat(1)  = matparam%bulk
          parmat(2)  = matparam%young
          parmat(3)  = matparam%nu
          parmat(16) = 2
          parmat(17) = dmn/dmx/dmx
          !< Elasticity type
          ielas = 2
          !< Number of parameters
          nupar_elas = 16
          !< Save elastic parameters
          upar_elas(1)  = d11
          upar_elas(2)  = d22
          upar_elas(3)  = d33
          upar_elas(4)  = d12
          upar_elas(5)  = d13
          upar_elas(6)  = d23
          upar_elas(7)  = a11
          upar_elas(8)  = a22
          upar_elas(9)  = a12
          upar_elas(10) = g12
          upar_elas(11) = g23
          upar_elas(12) = g31
          upar_elas(13) = e1
          upar_elas(14) = e2
          upar_elas(15) = nu13
          upar_elas(16) = nu23
          !< Printing elastic parameters
          if (is_encrypted)then
            write(iout,"(5X,A,//)") "CONFIDENTIAL DATA"
          else
            write(iout,1000) e1,e2,e3,nu12,nu23,nu31,g12,g23,g31
          endif
! ------------------------------------------------------------------------------
1000 format(/                                                                  &
          5X,"-------------------------------------------------------",/       &
          5X,"ORTHOTROPIC ELASTICITY                                 ",/,      &
          5X,"-------------------------------------------------------",/,      &
          5X,"YOUNG MODULUS IN DIRECTION 1 (E1). . . . . . . . . . .=",1PG20.13/&
          5X,"YOUNG MODULUS IN DIRECTION 2 (E2). . . . . . . . . . .=",1PG20.13/&
          5X,"YOUNG MODULUS IN DIRECTION 3 (E3). . . . . . . . . . .=",1PG20.13/&
          5X,"POISSON RATIO IN PLANE 12 (NU12) . . . . . . . . . . .=",1PG20.13/&
          5X,"POISSON RATIO IN PLANE 23 (NU23) . . . . . . . . . . .=",1PG20.13/&
          5X,"POISSON RATIO IN PLANE 31 (NU31) . . . . . . . . . . .=",1PG20.13/&
          5X,"SHEAR MODULUS IN PLANE 12 (G12). . . . . . . . . . . .=",1PG20.13/&
          5X,"SHEAR MODULUS IN PLANE 23 (G23). . . . . . . . . . . .=",1PG20.13/&
          5X,"SHEAR MODULUS IN PLANE 31 (G31). . . . . . . . . . . .=",1PG20.13/)
! -------------------------------------------------------------------------------
        end subroutine hm_read_elasticity_orthotropic
      end module hm_read_elasticity_orthotropic_mod