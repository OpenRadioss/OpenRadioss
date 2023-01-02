Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
Copyright>
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>
Copyright>
Copyright>        Commercial Alternative: Altair Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
Copyright>        software under a commercial license.  Contact Altair to discuss further if the
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
Chd|====================================================================
Chd|  SET_DD_MAT_WEIGHT             source/user_interface/set_dd_mat_weight.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        DDWEIGHTS_MOD                 share/modules1/ddweights_mod.F
Chd|====================================================================
       SUBROUTINE SET_DD_MAT_WEIGHT(MAT_WEIGHT1PT,MAT_WEIGHT5PT,ELEM_TYPE)
C----------------------------------------------- 
      USE DDWEIGHTS_MOD
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "r4r8_p.inc"
#include      "userlib.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       my_real MAT_WEIGHT1PT,MAT_WEIGHT5PT
       INTEGER ELEM_TYPE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C
C       print*,'MAT_NUMBER', MAT_NUMBER ,'WEIGHT:',MAT_WEIGHT1PT,MAT_WEIGHT5PT
C
       
       DDWEIGHTS(1,ELEM_TYPE,MAT_NUMBER)=MAT_WEIGHT1PT
       DDWEIGHTS(2,ELEM_TYPE,MAT_NUMBER)=MAT_WEIGHT5PT

!       DDWEIGHTS(1,2,MAT_NUMBER)=0.9
!       DDWEIGHTS(2,2,MAT_NUMBER)=3.7
       END

Chd|====================================================================
Chd|  INIT_MAT_WEIGHT               source/user_interface/set_dd_mat_weight.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|        R2R_VOID                      source/coupling/rad2rad/r2r_void.F
Chd|-- calls ---------------
Chd|        DDWEIGHTS_MOD                 share/modules1/ddweights_mod.F
Chd|====================================================================
       SUBROUTINE INIT_MAT_WEIGHT(NUMMAT)
C----------------------------------------------- 
      USE DDWEIGHTS_MOD
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "r4r8_p.inc"
#include      "userlib.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       INTEGER NUMMAT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C-----------------------------------------------
         ALLOCATE (DDWEIGHTS(1:2,1:2,1:NUMMAT+1))
         DDWEIGHTS(1:2,1:2,1:NUMMAT+1)=ZERO
       END
