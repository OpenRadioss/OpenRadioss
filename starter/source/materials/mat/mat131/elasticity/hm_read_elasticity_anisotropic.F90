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
!||    hm_read_elasticity_anisotropic_mod   ../starter/source/materials/mat/mat131/elasticity/hm_read_elasticity_anisotropic.F90
!||--- called by ------------------------------------------------------
!||    hm_read_elasticity                   ../starter/source/materials/mat/mat131/elasticity/hm_read_elasticity.F90
!||====================================================================
      module hm_read_elasticity_anisotropic_mod
        implicit none
! \brief Read anisotropic elasticity input data for /MAT/LAW131
! \details Read the anisotropic elasticity model parameters
!          for /MAT/LAW131 (elasto-plastic material law).
      contains
!||====================================================================
!||    hm_read_elasticity_anisotropic   ../starter/source/materials/mat/mat131/elasticity/hm_read_elasticity_anisotropic.F90
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
        subroutine hm_read_elasticity_anisotropic(                             &
          ikey     ,ielas    ,nupar_elas,upar_elas,is_available,               &
          unitab   ,lsubmodel,matparam ,parmat    ,iout     ,is_encrypted,     &
          mat_id   ,titr     ,iresp    )
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
          integer,                 intent(in)    :: ikey                  !< material key
          integer,                 intent(inout) :: ielas                 !< elastic model type
          integer,                 intent(inout) :: nupar_elas            !< number of elastic parameters
          real(kind=WP),dimension(100),intent(inout) :: upar_elas         !< elastic parameters
          logical,                 intent(in)    :: is_available          !< availability flag
          type(unit_type_),        intent(in)    :: unitab                !< units table
          type(submodel_data),dimension(nsubmod),intent(in) :: lsubmodel  !< submodel data structure
          type(matparam_struct_),  intent(inout) :: matparam              !< matparam data structure
          real(kind=WP),           intent(inout) :: parmat(100)           !< material parameter global table 1
          integer,                 intent(in)    :: iout                  !< output unit
          logical,                 intent(in)    :: is_encrypted          !< encryption flag
          integer, intent(in)                    :: mat_id                !< Material law user ID
          character(len=nchartitle),intent(in)   :: titr                  !< Material law user title
          integer, intent(in)                    :: iresp                 !< Flag for single precision
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
          integer :: i,j,ipiv(6),ipiv2d(3),info
          real(kind=WP) :: dmn,dmx,c(6,6),c2(6,6),s(6,6),s2d(3,3),c2d(3,3)
          real(kind=WP) :: sbd(2,2),cbd(2,2),detsbd
          real(kind=WP) :: wr(6),wi(6),vl(6,6),vr(6,6),work(102)
!===============================================================================
! 
          !=====================================================================
          !< Elastic anisotropic parameters
          !=====================================================================
          c(1:6,1:6) = zero
          call hm_get_float_array_index("ELAS_ANIS_C11",c(1,1),ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("ELAS_ANIS_C12",c(1,2),ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("ELAS_ANIS_C13",c(1,3),ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("ELAS_ANIS_C14",c(1,4),ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("ELAS_ANIS_C15",c(1,5),ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("ELAS_ANIS_C16",c(1,6),ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("ELAS_ANIS_C22",c(2,2),ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("ELAS_ANIS_C23",c(2,3),ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("ELAS_ANIS_C24",c(2,4),ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("ELAS_ANIS_C25",c(2,5),ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("ELAS_ANIS_C26",c(2,6),ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("ELAS_ANIS_C33",c(3,3),ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("ELAS_ANIS_C34",c(3,4),ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("ELAS_ANIS_C35",c(3,5),ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("ELAS_ANIS_C36",c(3,6),ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("ELAS_ANIS_C44",c(4,4),ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("ELAS_ANIS_C45",c(4,5),ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("ELAS_ANIS_C46",c(4,6),ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("ELAS_ANIS_C55",c(5,5),ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("ELAS_ANIS_C56",c(5,6),ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("ELAS_ANIS_C66",c(6,6),ikey,is_available,lsubmodel,unitab)
          !< Apply symmetry of elasticity matrix
          c(2,1) = c(1,2)
          c(3,1) = c(1,3)
          c(4,1) = c(1,4)
          c(5,1) = c(1,5)
          c(6,1) = c(1,6)
          c(3,2) = c(2,3)
          c(4,2) = c(2,4)
          c(5,2) = c(2,5)
          c(6,2) = c(2,6)
          c(4,3) = c(3,4)
          c(5,3) = c(3,5)
          c(6,3) = c(3,6)
          c(5,4) = c(4,5)
          c(6,4) = c(4,6)
          c(6,5) = c(5,6)
          !< Check that elasticity matrix is positive definite
          c2  = c
          info = 0
          if (iresp == 0) then 
            call dgeev('N','N',6,c2,6,wr,wi,vl,6,vr,6,work,102,info)  
          else
            call sgeev('N','N',6,c2,6,wr,wi,vl,6,vr,6,work,102,info)
          endif
          if (minval(wr) < -1.0d-6) then
            call ancmsg(msgid=3131,                                            &
                        msgtype=msgerror,                                      &
                        anmode=aninfo_blind_2,                                 &
                        i1=mat_id,                                             &
                        c1="ERROR",                                            &
                        c2=titr,                                               &
                        c3="ELAS_ANISOTROPIC",                                 &
                        c4="ELASTIC STIFFNESS MATRIX IS NOT POSITIVE DEFINITE.")
          endif
          !< Fill matparam values
          matparam%bulk  = (one/nine)*(c(1,1) + c(2,2) + c(3,3) +              & 
                                  two*(c(1,2) + c(1,3) + c(2,3)))
          matparam%shear = (one/fifteen)*((c(1,1) + c(2,2) + c(3,3)) -         &
                                          (c(1,2) + c(1,3) + c(2,3)) +         &
                                    three*(c(4,4) + c(5,5) + c(6,6)))
          matparam%young = nine*matparam%bulk*matparam%shear/                  &
                           (three*matparam%bulk + matparam%shear)
          matparam%nu    = (three*matparam%bulk - two*matparam%shear)/         &
                           (two*(three*matparam%bulk + matparam%shear))
          !< 3D Compliance matrix
          c2  = c
          s = zero
          do i = 1,6
            s(i,i) = one
          enddo
          info = 0
          if (iresp == 0) then 
            call dgesv(6, 6, c2, 6, ipiv, s, 6, info)  
          else
            call sgesv(6, 6, c2, 6, ipiv, s, 6, info)
          endif
          !< 2D Compliance matrix for plane stress
          s2d(1,1) = s(1,1)
          s2d(1,2) = s(1,2)
          s2d(1,3) = s(1,4)
          s2d(2,1) = s(2,1)
          s2d(2,2) = s(2,2)
          s2d(2,3) = s(2,4)
          s2d(3,1) = s(4,1)
          s2d(3,2) = s(4,2)
          s2d(3,3) = s(4,4)
          !< Invert 2D compliance matrix to get 2D elasticity matrix for plane stress
          c2d = zero
          do i = 1,3
            c2d(i,i) = one
          enddo
          info = 0
          if (iresp == 0) then 
            call dgesv(3, 3, s2d, 3, ipiv2d, c2d, 3, info)
          else
            call sgesv(3, 3, s2d, 3, ipiv2d, c2d, 3, info)
          endif
          !< Compliance matrix for bending of shells
          sbd(1,1) = s(5,5)
          sbd(1,2) = s(5,6)
          sbd(2,1) = s(6,5)
          sbd(2,2) = s(6,6)
          !< Invert compliance matrix for bending of shells to get bending stiffness matrix
          detsbd = sbd(1,1)*sbd(2,2) - sbd(1,2)*sbd(2,1)
          cbd(1,1) = sbd(2,2)/detsbd
          cbd(1,2) = -sbd(1,2)/detsbd
          cbd(2,2) = sbd(1,1)/detsbd
          !< Fill PARMAT values
          parmat(1)  = matparam%bulk
          parmat(2)  = matparam%young
          parmat(3)  = matparam%nu
          parmat(16) = 2
          dmn = minval(wr)
          dmx = maxval(wr)
          parmat(17) = dmn/dmx/dmx
          !< Elasticity type
          ielas = 3
          !< Number of parameters
          nupar_elas = 34
          !< Save elastic parameters
          upar_elas(1)  = c(1,1)
          upar_elas(2)  = c(1,2)
          upar_elas(3)  = c(1,3)
          upar_elas(4)  = c(1,4)
          upar_elas(5)  = c(1,5)
          upar_elas(6)  = c(1,6)
          upar_elas(7)  = c(2,2)
          upar_elas(8)  = c(2,3)
          upar_elas(9)  = c(2,4)
          upar_elas(10) = c(2,5)
          upar_elas(11) = c(2,6)
          upar_elas(12) = c(3,3)
          upar_elas(13) = c(3,4)
          upar_elas(14) = c(3,5)
          upar_elas(15) = c(3,6)
          upar_elas(16) = c(4,4)
          upar_elas(17) = c(4,5)
          upar_elas(18) = c(4,6)
          upar_elas(19) = c(5,5)
          upar_elas(20) = c(5,6)
          upar_elas(21) = c(6,6)
          upar_elas(22) = c2d(1,1)
          upar_elas(23) = c2d(1,2)
          upar_elas(24) = c2d(1,3)
          upar_elas(25) = c2d(2,2)
          upar_elas(26) = c2d(2,3)
          upar_elas(27) = c2d(3,3)
          upar_elas(28) = cbd(1,1)
          upar_elas(29) = cbd(1,2)
          upar_elas(30) = cbd(2,2)
          upar_elas(31) = s(3,1)
          upar_elas(32) = s(3,2)
          upar_elas(33) = s(3,4)
          upar_elas(34) = maxval(wr)
          !< Printing elastic parameters
          if (is_encrypted)then
            write(iout,"(5X,A,//)") "CONFIDENTIAL DATA"
          else
            write(iout,1000) c(1,1),c(1,2),c(1,3),c(1,4),c(1,5),c(1,6),c(2,2), &
               c(2,3),c(2,4),c(2,5),c(2,6),c(3,3),c(3,4),c(3,5),c(3,6),c(4,4), &
               c(4,5),c(4,6),c(5,5),c(5,6),c(6,6)
          endif
! ------------------------------------------------------------------------------
1000 format(/                                                                  &
          5X,"-------------------------------------------------------",/       &
          5X,"ANISOTROPIC ELASTICITY                                 ",/,      &
          5X,"-------------------------------------------------------",/,      &
          5X,"ELASTICITY MATRIX COMPONENT C11. . . . . . . . . . . .=",1PG20.13/&
          5X,"ELASTICITY MATRIX COMPONENT C12. . . . . . . . . . . .=",1PG20.13/&
          5X,"ELASTICITY MATRIX COMPONENT C13. . . . . . . . . . . .=",1PG20.13/& 
          5X,"ELASTICITY MATRIX COMPONENT C14. . . . . . . . . . . .=",1PG20.13/&
          5X,"ELASTICITY MATRIX COMPONENT C15. . . . . . . . . . . .=",1PG20.13/&
          5X,"ELASTICITY MATRIX COMPONENT C16. . . . . . . . . . . .=",1PG20.13/&
          5X,"ELASTICITY MATRIX COMPONENT C22. . . . . . . . . . . .=",1PG20.13/&
          5X,"ELASTICITY MATRIX COMPONENT C23. . . . . . . . . . . .=",1PG20.13/&
          5X,"ELASTICITY MATRIX COMPONENT C24. . . . . . . . . . . .=",1PG20.13/&
          5X,"ELASTICITY MATRIX COMPONENT C25. . . . . . . . . . . .=",1PG20.13/&
          5X,"ELASTICITY MATRIX COMPONENT C26. . . . . . . . . . . .=",1PG20.13/&
          5X,"ELASTICITY MATRIX COMPONENT C33. . . . . . . . . . . .=",1PG20.13/&
          5X,"ELASTICITY MATRIX COMPONENT C34. . . . . . . . . . . .=",1PG20.13/&
          5X,"ELASTICITY MATRIX COMPONENT C35. . . . . . . . . . . .=",1PG20.13/&
          5X,"ELASTICITY MATRIX COMPONENT C36. . . . . . . . . . . .=",1PG20.13/&
          5X,"ELASTICITY MATRIX COMPONENT C44. . . . . . . . . . . .=",1PG20.13/&
          5X,"ELASTICITY MATRIX COMPONENT C45. . . . . . . . . . . .=",1PG20.13/&
          5X,"ELASTICITY MATRIX COMPONENT C46. . . . . . . . . . . .=",1PG20.13/&
          5X,"ELASTICITY MATRIX COMPONENT C55. . . . . . . . . . . .=",1PG20.13/&
          5X,"ELASTICITY MATRIX COMPONENT C56. . . . . . . . . . . .=",1PG20.13/&
          5X,"ELASTICITY MATRIX COMPONENT C66. . . . . . . . . . . .=",1PG20.13/)
! -------------------------------------------------------------------------------
        end subroutine hm_read_elasticity_anisotropic
      end module hm_read_elasticity_anisotropic_mod