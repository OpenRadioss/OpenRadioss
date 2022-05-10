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
Chd|  CEVEC3                        source/elements/shell/coque/cevec3.F
Chd|-- called by -----------
Chd|        CFORC3                        source/elements/shell/coque/cforc3.F
Chd|-- calls ---------------
Chd|        CORTDIR3                      source/elements/shell/coque/cortdir3.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE CEVEC3(ELBUF_STR,DIR_A  ,DIR_B  ,
     1    JFT,JLT,X1 ,X2 ,X3 ,X4 ,Y1 ,Y2 ,Y3 ,Y4  ,
     2    Z1 ,Z2 ,Z3 ,Z4 ,E1X,E1Y,E1Z,E2X,E2Y, E2Z, 
     3    E3X,E3Y,E3Z,IREP,NLAY,NEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT,J,IREP,NLAY,NEL
      my_real
     .   X1(*), X2(*), X3(*), X4(*), Y1(*), Y2(*), Y3(*), Y4(*),
     .   Z1(*), Z2(*), Z3(*), Z4(*), E1X(*), E1Y(*), E1Z(*), E2X(*),
     .   E2Y(*), E2Z(*), E3X(*), E3Y(*), E3Z(*), DIR_A(*),DIR_B(*)
      TYPE (ELBUF_STRUCT_) :: ELBUF_STR
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL ou REAL*8
      my_real
     .   X31(MVSIZ), Y31(MVSIZ), Z31(MVSIZ), X42(MVSIZ), Y42(MVSIZ),
     .   Z42(MVSIZ), X21(MVSIZ), Y21(MVSIZ), Z21(MVSIZ),
     .   SUM(MVSIZ), SUMA
C=======================================================================
      DO I=JFT,JLT                                      
        X21(I)=X2(I)-X1(I)                              
        Y21(I)=Y2(I)-Y1(I)                              
        Z21(I)=Z2(I)-Z1(I)                              
        X31(I)=X3(I)-X1(I)                              
        Y31(I)=Y3(I)-Y1(I)                              
        Z31(I)=Z3(I)-Z1(I)                              
        X42(I)=X4(I)-X2(I)                              
        Y42(I)=Y4(I)-Y2(I)                              
        Z42(I)=Z4(I)-Z2(I)                              
C
        E3X(I)=Y31(I)*Z42(I)-Z31(I)*Y42(I)              
        E3Y(I)=Z31(I)*X42(I)-X31(I)*Z42(I)              
        E3Z(I)=X31(I)*Y42(I)-Y31(I)*X42(I)              
        SUMA=E3X(I)*E3X(I)+E3Y(I)*E3Y(I)+E3Z(I)*E3Z(I)  
        SUMA=MAX(SQRT(SUMA),EM20)                       
        E3X(I)=E3X(I)/SUMA                              
        E3Y(I)=E3Y(I)/SUMA                              
        E3Z(I)=E3Z(I)/SUMA                              
      ENDDO                                             
C
      DO I=JFT,JLT                                       
        SUMA= X21(I)*E3X(I)+Y21(I)*E3Y(I)+Z21(I)*E3Z(I)  
        E1X(I)= X21(I)-E3X(I)*SUMA                       
        E1Y(I)= Y21(I)-E3Y(I)*SUMA                       
        E1Z(I)= Z21(I)-E3Z(I)*SUMA                       
      ENDDO                                              
C
      DO I=JFT,JLT                                      
        SUMA=E1X(I)*E1X(I)+E1Y(I)*E1Y(I)+E1Z(I)*E1Z(I)  
        SUMA=MAX(SQRT(SUMA),EM20)                       
        E1X(I)=E1X(I)/SUMA                              
        E1Y(I)=E1Y(I)/SUMA                              
        E1Z(I)=E1Z(I)/SUMA                              
      ENDDO                                             
C
      DO I=JFT,JLT                                        
        E2X(I)=E3Y(I)*E1Z(I)-E3Z(I)*E1Y(I)                
        E2Y(I)=E3Z(I)*E1X(I)-E3X(I)*E1Z(I)                
        E2Z(I)=E3X(I)*E1Y(I)-E3Y(I)*E1X(I)                
        SUMA  =E2X(I)*E2X(I)+E2Y(I)*E2Y(I)+E2Z(I)*E2Z(I)  
        SUMA  =MAX(SQRT(SUMA),EM20)                       
        E2X(I)=E2X(I)/SUMA                                
        E2Y(I)=E2Y(I)/SUMA                                
        E2Z(I)=E2Z(I)/SUMA                                
      ENDDO                                               
C
C---  directions orthotropie / anisotropie convectes
C
      CALL CORTDIR3(ELBUF_STR,DIR_A,DIR_B  ,JFT    ,JLT    ,
     .              NLAY   ,IREP   ,X21    ,Y21    ,Z21    ,
     .              X31    ,Y31    ,Z31    ,E1X    ,E1Y    ,   
     .              E1Z    ,E2X    ,E2Y    ,E2Z    ,NEL    )
C-----------
      RETURN
      END
