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
C     routine reectite en 50c
Chd|====================================================================
Chd|  CNVEC3                        source/elements/shell/coque/cnvec3.F
Chd|-- called by -----------
Chd|        CFORC3                        source/elements/shell/coque/cforc3.F
Chd|        CFORC3_CRK                    source/elements/xfem/cforc3_crk.F
Chd|-- calls ---------------
Chd|        CORTDIR3                      source/elements/shell/coque/cortdir3.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE CNVEC3(ELBUF_STR,DIR_A  ,DIR_B  ,
     1           JFT     ,JLT    ,IREP   ,IGTYP  ,NLAY   ,       
     2           X1      ,X2     ,X3     ,X4     ,Y1     ,Y2     ,       
     3           Y3      ,Y4     ,Z1     ,Z2     ,Z3     ,Z4     ,       
     4           E1X     ,E1Y    ,E1Z    ,E2X    ,E2Y    ,E2Z    ,       
     5           E3X     ,E3Y    ,E3Z    ,NEL    )                               
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
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: JFT,JLT,NLAY,IREP,IGTYP,NEL
      my_real
     .   X1(*), X2(*), X3(*), X4(*), Y1(*), Y2(*), Y3(*), Y4(*),
     .   Z1(*), Z2(*), Z3(*), Z4(*), E1X(*), E1Y(*), E1Z(*), E2X(*),
     .   E2Y(*), E2Z(*), E3X(*), E3Y(*), E3Z(*),DIR_A(*),DIR_B(*)
      TYPE (ELBUF_STRUCT_) :: ELBUF_STR
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   SUMA,S1,S2,X21,X32,X34,X41,Y21,Y32,Y34,Y41,Z21,Z32,Z34,Z41
      my_real, DIMENSION(MVSIZ) :: 
     .   E11,E12,E13,E21,E22,E23,RX,RY,RZ,SX,SY,SZ
C=======================================================================
      DO I=JFT,JLT                          
        X21 = X2(I)-X1(I)                   
        X32 = X3(I)-X2(I)                   
        X34 = X3(I)-X4(I)                   
        X41 = X4(I)-X1(I)                   

        Y21 = Y2(I)-Y1(I)                   
        Y32 = Y3(I)-Y2(I)                   
        Y34 = Y3(I)-Y4(I)                   
        Y41 = Y4(I)-Y1(I)                   

        Z21 = Z2(I)-Z1(I)                   
        Z32 = Z3(I)-Z2(I)                   
        Z34 = Z3(I)-Z4(I)                   
        Z41 = Z4(I)-Z1(I)                   
                                            
        E1X(I) = (X21+X34 )                  
        E1Y(I) = (Y21+Y34 )                   
        E1Z(I) = (Z21+Z34 )                   

        E2X(I) = (X32+X41 )                   
        E2Y(I) = (Y32+Y41 )                   
        E2Z(I) = (Z32+Z41 )                   

        E3X(I) = E1Y(I)*E2Z(I)-E1Z(I)*E2Y(I)  
        E3Y(I) = E1Z(I)*E2X(I)-E1X(I)*E2Z(I)  
        E3Z(I) = E1X(I)*E2Y(I)-E1Y(I)*E2X(I)  
      ENDDO                                 
      IF (IREP > 0) THEN
        DO I=JFT,JLT
          RX(I) = E1X(I)
          RY(I) = E1Y(I)
          RZ(I) = E1Z(I)
          SX(I) = E2X(I)
          SY(I) = E2Y(I)
          SZ(I) = E2Z(I)
        ENDDO
      ENDIF
C
C---  orthogonalisation du repere local
C
      IF (ISHFRAM == 0 .OR. IGTYP == 16 ) THEN
C---    Repere convecte symetrique - version 5 (default)
        DO I=JFT,JLT                         
          SUMA   = E3X(I)*E3X(I)+E3Y(I)*E3Y(I)+E3Z(I)*E3Z(I)
          SUMA   = ONE / MAX(SQRT(SUMA),EM20)                  
          E3X(I) = E3X(I) * SUMA                            
          E3Y(I) = E3Y(I) * SUMA                            
          E3Z(I) = E3Z(I) * SUMA                            
C
          S1     = E1X(I)*E1X(I)+E1Y(I)*E1Y(I)+E1Z(I)*E1Z(I) 
          S2     = E2X(I)*E2X(I)+E2Y(I)*E2Y(I)+E2Z(I)*E2Z(I) 
          SUMA   = SQRT(S1/S2)                
          E1X(I) = E1X(I) + (E2Y(I)*E3Z(I)-E2Z(I)*E3Y(I))*SUMA
          E1Y(I) = E1Y(I) + (E2Z(I)*E3X(I)-E2X(I)*E3Z(I))*SUMA
          E1Z(I) = E1Z(I) + (E2X(I)*E3Y(I)-E2Y(I)*E3X(I))*SUMA
C
          SUMA   = E1X(I)*E1X(I)+E1Y(I)*E1Y(I)+E1Z(I)*E1Z(I)  
          SUMA   = ONE / MAX(SQRT(SUMA),EM20)                  
          E1X(I) = E1X(I) * SUMA
          E1Y(I) = E1Y(I) * SUMA
          E1Z(I) = E1Z(I) * SUMA
C
          E2X(I) = E3Y(I) * E1Z(I) - E3Z(I) * E1Y(I)
          E2Y(I) = E3Z(I) * E1X(I) - E3X(I) * E1Z(I)
          E2Z(I) = E3X(I) * E1Y(I) - E3Y(I) * E1X(I)
        ENDDO
      ELSEIF (ISHFRAM == 2) THEN
C---    Repere convecte nonsymetrique - version 4
        DO I=JFT,JLT
          SUMA   = E2X(I)*E2X(I)+E2Y(I)*E2Y(I)+E2Z(I)*E2Z(I)
          E1X(I) = E1X(I)*SUMA + E2Y(I)*E3Z(I)-E2Z(I)*E3Y(I)
          E1Y(I) = E1Y(I)*SUMA + E2Z(I)*E3X(I)-E2X(I)*E3Z(I)
          E1Z(I) = E1Z(I)*SUMA + E2X(I)*E3Y(I)-E2Y(I)*E3X(I)
          SUMA   = E1X(I)*E1X(I)+E1Y(I)*E1Y(I)+E1Z(I)*E1Z(I)
          SUMA   = ONE/MAX(SQRT(SUMA),EM20)
          E1X(I) = E1X(I)*SUMA
          E1Y(I) = E1Y(I)*SUMA
          E1Z(I) = E1Z(I)*SUMA
C
          SUMA   = E3X(I)*E3X(I)+E3Y(I)*E3Y(I)+E3Z(I)*E3Z(I)
          SUMA   = ONE / MAX(SQRT(SUMA),EM20)                  
          E3X(I) = E3X(I) * SUMA                            
          E3Y(I) = E3Y(I) * SUMA                            
          E3Z(I) = E3Z(I) * SUMA                            
C
          E2X(I) = E3Y(I)*E1Z(I)-E3Z(I)*E1Y(I)
          E2Y(I) = E3Z(I)*E1X(I)-E3X(I)*E1Z(I)
          E2Z(I) = E3X(I)*E1Y(I)-E3Y(I)*E1X(I)
          SUMA   = E2X(I)*E2X(I)+E2Y(I)*E2Y(I)+E2Z(I)*E2Z(I)
          SUMA   = ONE/MAX(SQRT(SUMA),EM20)
          E2X(I) = E2X(I)*SUMA
          E2Y(I) = E2Y(I)*SUMA
          E2Z(I) = E2Z(I)*SUMA
        ENDDO
      ENDIF
C
C---  directions orthotropie / anisotropie convectes
C
      CALL CORTDIR3(ELBUF_STR,DIR_A  ,DIR_B  ,JFT    ,JLT    ,
     .              NLAY     ,IREP   ,RX     ,RY     ,RZ     ,
     .              SX       ,SY     ,SZ     ,E1X    ,E1Y    ,   
     .              E1Z      ,E2X    ,E2Y    ,E2Z    ,NEL    )   
C-----------
      RETURN
      END SUBROUTINE CNVEC3
