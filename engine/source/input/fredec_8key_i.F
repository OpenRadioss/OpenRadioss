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
C-----------------------------------------------
Chd|====================================================================
Chd|  FREDEC_8KEY_I                 source/input/fredec_8key_i.F  
Chd|-- called by -----------
Chd|        FREANIM                       source/output/anim/reader/freanim.F
Chd|        FREDYNAIN                     source/input/fredynain.F      
Chd|        FRESTAT                       source/input/frestat.F        
Chd|        H3D_READ                      source/output/h3d/h3d_build_fortran/h3d_read.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FREDEC_8KEY_I(CART, KEY2, KEY3, KEY4, KEY5, KEY6, KEY7, KEY8, NBC)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "nchara_c.inc"
#include      "chara_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   G l o b a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER CART*(*), KEY2*(*), KEY3*(*), KEY4*(*), KEY5*(*), KEY6*(*), KEY7*(*), KEY8*(*)
      CHARACTER MOT1*ncharkey
      INTEGER   I1,I2,NBC
C======================================================================|
C     /Key1/KEY2/KEY3/KEY4/KEY5/KEY6/KEY7/KEY8/NBC
C-----------------------------------------------
      I1=2
      I2=I1
      DO WHILE(CART(I2:I2)/='/'.AND.I2<120)
        I2=I2+1
      ENDDO
C KEY2
      I1 =I2+1
      I1=MIN(I1,120)
      I2 =I1
      DO WHILE(CART(I2:I2)/='/'.AND.I2<120)
        I2=I2+1
      ENDDO
      I2=I2-1
      KEY2 = CART(I1:I2)
C KEY3
      I1 =I2+2
      I1=MIN(I1,120)
      I2 =I1
      DO WHILE(CART(I2:I2)/='/'.AND.I2<120)
        I2=I2+1
      ENDDO
      I2=I2-1
      KEY3 = CART(I1:I2)
C KEY4
      I1=I2+2
      I1=MIN(I1,120)      
      I2=I1
      DO WHILE(CART(I2:I2)/='/' .AND. I2 < 120)
        I2=I2+1
      ENDDO
      I2=I2-1
      KEY4 = CART(I1:I2)
C KEY5
      I1=I2+2
      I1=MIN(I1,120)      
      I2=I1
      DO WHILE(CART(I2:I2)/='/' .AND. I2 < 120)
        I2=I2+1
      ENDDO
      I2=I2-1
      KEY5 = CART(I1:I2)
C KEY6
      I1=I2+2
      I1=MIN(I1,120)      
      I2=I1
      DO WHILE(CART(I2:I2)/='/' .AND. I2 < 120)
        I2=I2+1
      ENDDO
      I2=I2-1
      KEY6 = CART(I1:I2)
C KEY7
      I1=I2+2
      I1=MIN(I1,120)      
      I2=I1
      DO WHILE(CART(I2:I2)/='/' .AND. I2 < 120)
        I2=I2+1
      ENDDO
      I2=I2-1
      KEY7 = CART(I1:I2)
C KEY8
      I1=I2+2
      I1=MIN(I1,120)      
      I2=I1
      DO WHILE(CART(I2:I2)/='/' .AND. I2 < 120)
        I2=I2+1
      ENDDO
      I2=I2-1
      KEY8 = CART(I1:I2)
C NBC
      I1=101   
      I2=110
      MOT1= CART(I1:I2)
      READ(MOT1,FMT='(I10)')NBC
C---
      RETURN
      END
