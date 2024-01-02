Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  CGSHELL4                      source/implicit/cgshell.F     
Chd|-- called by -----------
Chd|        SPBRM_PRE                     source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE CGSHELL4(ELBUF_STR,JFT,JLT ,PM   ,IXC ,
     +                    X   ,MAS,XC ,YC ,ZC )
C-----------------------------------------------
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXC(NIXC,*), JFT, JLT
C     REAL
      my_real
     .   PM(NPROPM,*),X(3,*),MAS,XC,YC,ZC
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,MAT
C     REAL
      my_real
     .   XX,YY,ZZ,XMAS25,XMAS(MVSIZ),AREA(MVSIZ),
     .   RX, RY, RZ,SX,SY,SZ,E3X,E3Y,E3Z
      TYPE(G_BUFEL_) ,POINTER :: GBUF
C-----------------------------------------------
      GBUF => ELBUF_STR%GBUF
      DO I=JFT,JLT
       RX=X(1,IXC(3,I))+X(1,IXC(4,I))-X(1,IXC(2,I))-X(1,IXC(5,I))
       SX=X(1,IXC(4,I))+X(1,IXC(5,I))-X(1,IXC(2,I))-X(1,IXC(3,I))
       RY=X(2,IXC(3,I))+X(2,IXC(4,I))-X(2,IXC(2,I))-X(2,IXC(5,I))
       SY=X(2,IXC(4,I))+X(2,IXC(5,I))-X(2,IXC(2,I))-X(2,IXC(3,I))
       RZ=X(3,IXC(3,I))+X(3,IXC(4,I))-X(3,IXC(2,I))-X(3,IXC(5,I))
       SZ=X(3,IXC(4,I))+X(3,IXC(5,I))-X(3,IXC(2,I))-X(3,IXC(3,I))
       E3X = RY * SZ - RZ * SY 
       E3Y = RZ * SX - RX * SZ 
       E3Z = RX * SY - RY * SX 
       AREA(I) =FOURTH*SQRT(E3X*E3X + E3Y*E3Y + E3Z*E3Z)
      END DO
      DO I=JFT,JLT
       MAT = IXC(1,I)
       XMAS(I)=PM(1,MAT)*AREA(I)*GBUF%THK(I)
      END DO
C
C
      DO I=JFT,JLT
         IF (GBUF%OFF(I) ==ZERO) CYCLE
         XX= X(1,IXC(2,I))+X(1,IXC(3,I))+X(1,IXC(4,I))+X(1,IXC(5,I))
         YY= X(2,IXC(2,I))+X(2,IXC(3,I))+X(2,IXC(4,I))+X(2,IXC(5,I))
         ZZ= X(3,IXC(2,I))+X(3,IXC(3,I))+X(3,IXC(4,I))+X(3,IXC(5,I))
         XMAS25 = FOURTH*XMAS(I)
         MAS = MAS+XMAS(I)
         XC = XC + XMAS25*XX
         YC = YC + XMAS25*YY
         ZC = ZC + XMAS25*ZZ
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  CGSHELL3                      source/implicit/cgshell.F     
Chd|-- called by -----------
Chd|        SPBRM_PRE                     source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE CGSHELL3(ELBUF_STR,JFT,JLT ,PM   ,IXTG ,
     +                    X   ,MAS,XC ,YC ,ZC )
C-----------------------------------------------
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXTG(NIXTG,*), JFT, JLT
C     REAL
      my_real
     .   PM(NPROPM,*),X(3,*),MAS,XC,YC,ZC
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,MAT
C     REAL
      my_real
     .   XX,YY,ZZ,XMAS25,XMAS(MVSIZ),AREA(MVSIZ),
     .   RX, RY, RZ,SX,SY,SZ,E3X,E3Y,E3Z
      TYPE(G_BUFEL_) ,POINTER :: GBUF
C-----------------------------------------------
      GBUF => ELBUF_STR%GBUF
      DO I=JFT,JLT
       RX=X(1,IXTG(3,I))-X(1,IXTG(2,I))
       SX=X(1,IXTG(4,I))-X(1,IXTG(2,I))
       RY=X(2,IXTG(3,I))-X(2,IXTG(2,I))
       SY=X(2,IXTG(4,I))-X(2,IXTG(2,I))
       RZ=X(3,IXTG(3,I))-X(3,IXTG(2,I))
       SZ=X(3,IXTG(4,I))-X(3,IXTG(2,I))
       E3X = RY * SZ - RZ * SY 
       E3Y = RZ * SX - RX * SZ 
       E3Z = RX * SY - RY * SX 
       AREA(I) =HALF*SQRT(E3X*E3X + E3Y*E3Y + E3Z*E3Z)
      END DO
      DO I=JFT,JLT
       MAT = IXTG(1,I)
       XMAS(I)=PM(1,MAT)*AREA(I)*GBUF%THK(I)
      END DO
C
C
      DO I=JFT,JLT
         IF (GBUF%OFF(I) ==ZERO) CYCLE
         XX= X(1,IXTG(2,I))+X(1,IXTG(3,I))+X(1,IXTG(4,I))
         YY= X(2,IXTG(2,I))+X(2,IXTG(3,I))+X(2,IXTG(4,I))
         ZZ= X(3,IXTG(2,I))+X(3,IXTG(3,I))+X(3,IXTG(4,I))
         XMAS25 = THIRD*XMAS(I)
         MAS = MAS+XMAS(I)
         XC = XC + XMAS25*XX
         YC = YC + XMAS25*YY
         ZC = ZC + XMAS25*ZZ
      ENDDO
C
      RETURN
      END
