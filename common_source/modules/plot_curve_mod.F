Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  PLOT_CURVE_MOD                modules/plot_curve_mod.F      
Chd|-- called by -----------
Chd|        ADD_MASS_STAT                 starter/source/tools/admas/add_mass_stat.F
Chd|        OUTRIN                        starter/source/materials/time_step/outri.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE PLOT_CURVE_MOD
      INTERFACE
      SUBROUTINE PLOT_CURVE(X, Y, NB_POINTS, INPUT_SIZE_X, INPUT_SIZE_Y, 
     .                      INPUT_X_MINVALUE, INPUT_Y_MINVALUE, INPUT_X_MAXVALUE, 
     .                      INPUT_Y_MAXVALUE, INPUT_SYMBOL, INPUT_CURVE_TYPE,
     .                      INPUT_TXT_X, INPUT_TXT_Y)   
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "nchar_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     . X(NB_POINTS),Y(NB_POINTS)
      INTEGER NB_POINTS,NB_GRADUATION,INPUT_SIZE_X, INPUT_SIZE_Y, INPUT_CURVE_TYPE
      my_real
     . INPUT_X_MINVALUE, INPUT_Y_MINVALUE, 
     . INPUT_X_MAXVALUE, INPUT_Y_MAXVALUE
      CHARACTER*1 INPUT_SYMBOL
      CHARACTER(*) INPUT_TXT_X, INPUT_TXT_Y
C-----------------------------------------------
      OPTIONAL ::
     .   INPUT_SIZE_X, INPUT_SIZE_Y, INPUT_X_MINVALUE, INPUT_Y_MINVALUE, 
     .   INPUT_X_MAXVALUE, INPUT_Y_MAXVALUE, INPUT_SYMBOL, 
     .   INPUT_CURVE_TYPE, INPUT_TXT_X, INPUT_TXT_Y
      END SUBROUTINE
      END INTERFACE
C===============================================
      END MODULE 
