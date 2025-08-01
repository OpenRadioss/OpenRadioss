Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    my_isnan        ../engine/source/system/my_isnan.F
!||--- called by ------------------------------------------------------
!||    check_nan_acc   ../engine/source/output/outfile/check_nan_acc.F
!||    nlocal_acc      ../engine/source/assembly/nlocal_acc.F
!||====================================================================
        FUNCTION MY_ISNAN(X) RESULT(BOOL)
#ifdef COMP_ARMFLANG
        USE, INTRINSIC :: IEEE_ARITHMETIC 
#endif
#include "implicit_f.inc"
        my_real, INTENT(IN) :: X
        LOGICAL :: BOOL

#ifdef COMP_ARMFLANG      
        BOOL=IEEE_IS_NAN(X)
#else
        BOOL=.NOT.(X.EQ.X)
#endif      
        END FUNCTION MY_ISNAN
