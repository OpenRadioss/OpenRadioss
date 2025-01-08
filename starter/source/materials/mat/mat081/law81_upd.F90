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
      !||====================================================================
      !||    law81_upd_mod   ../starter/source/materials/mat/mat081/law81_upd.F90
      !||--- called by ------------------------------------------------------
      !||    updmat          ../starter/source/materials/updmat.F
      !||====================================================================
     module law81_upd_mod
       contains
! \brief Update material law 81 to take into account tabulated stiffness
      !||====================================================================
      !||    law81_upd          ../starter/source/materials/mat/mat081/law81_upd.F90
      !||--- called by ------------------------------------------------------
      !||    updmat             ../starter/source/materials/updmat.F
      !||--- calls      -----------------------------------------------------
      !||    finter             ../starter/source/tools/curve/finter.F
      !||--- uses       -----------------------------------------------------
      !||====================================================================
       subroutine law81_upd(                                                   &
         matparam,nfunc   ,ifunc   ,npc     ,snpc    ,pld     ,stf     ,       &
         pm      ,npropm  ,iout    ,mat_id  ,titr    )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
         use constant_mod
         use matparam_def_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
         implicit none
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
         type(matparam_struct_), intent(inout)     :: matparam
         integer, intent(in)                       :: nfunc
         integer, dimension(nfunc), intent(in)     :: ifunc
         integer, intent(in)                       :: snpc
         integer, dimension(snpc), intent(in)      :: npc
         integer, intent(in)                       :: stf
         my_real, dimension(stf), intent(in)       :: pld
         my_real, dimension(npropm), intent(inout) :: pm
         integer, intent(in)                       :: npropm
         integer, intent(in)                       :: iout
         integer, intent(in)                       :: mat_id
         character(len=nchartitle), intent(in)     :: titr
         my_real :: finter
         external finter
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
         my_real :: deri,kini,kscale,gini,gscale
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!
         !< Bulk modulus function (if exists)
         if (ifunc(1) > 0) then 
           !< Initial bulk modulus (or scale factor)
           kscale = matparam%uparam(1)
           !< Update accounting for the tabulated bulk modulus function
           kini = kscale*finter(ifunc(1),zero,npc,pld,deri)
           !< Save the new value of the bulk modulus
           matparam%bulk = kini
           !< Update PM table
           pm(32)  = matparam%bulk
           pm(100) = matparam%bulk
           pm(107) = two*pm(32)
         endif
!
         !< Shear modulus function (if exists)
         if (ifunc(2) > 0) then
           !< Initial shear modulus (or scale factor)
           gscale = matparam%uparam(2)
           !< Update accounting for the tabulated bulk modulus function
           gini = gscale*finter(ifunc(2),zero,npc,pld,deri)
           !< Save the new value of the bulk modulus
           matparam%shear = gini
           !< Update PM table
           pm(22) = matparam%shear
         endif
!
         !< Update elastic parameters in the material parameters structures
         if ((ifunc(1) > 0).or.(ifunc(2) > 0)) then
           kini = matparam%bulk
           gini = matparam%shear
           matparam%young = nine*kini*gini/(three*kini + gini)
           matparam%nu = (three*kini - two*gini)/(six*kini + two*gini)
           pm(20) = matparam%young
           pm(21) = matparam%nu
           pm(24) = matparam%young/(one - (matparam%nu)**2)
         endif
!
         !< Print the updated material parameters
         write(iout,1000) titr,mat_id,81
         write(iout,1100)
         write(iout,1200) matparam%bulk,matparam%shear,matparam%young,matparam%nu
!
 1000 format(/                                                                 &
       5X,A,/,                                                                 &
       5X,'MATERIAL NUMBER. . . . . . . . . . . . . . .=',I10/,                &
       5X,'MATERIAL LAW . . . . . . . . . . . . . . . .=',I10/)
 1100 format(/                                                                 &
       5X,'-----------------------------------------------------',/,           &
       5X,'  ADDITIONAL DATA DRUCKER-PRAGER WITH CAP HARDENING  ',/,           &
       5X,'-----------------------------------------------------',/)
 1200 FORMAT(/                                                                 &
       5X,'INITIAL BULK MODULUS. . . . . . . . . . . . =',1PG20.13/            &
       5X,'INITIAL SHEAR MODULUS . . . . . . . . . . . =',1PG20.13/            &
       5X,'INITIAL YOUNG MODULUS (COMPUTED). . . . . . =',1PG20.13/            &
       5X,'INITIAL POISSON RATIO (COMPUTED). . . . . . =',1PG20.13/)
!
       end subroutine law81_upd
     end module law81_upd_mod

