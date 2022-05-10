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
Chd|  S8COOR3                       source/elements/solid/solide8/s8coor3.F
Chd|-- called by -----------
Chd|        S8FORC3                       source/elements/solid/solide8/s8forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8COOR3(OFFG,OFF,X,V,IXS,
     .                   XLOC,YLOC,ZLOC,VXLOC,VYLOC,VZLOC,
     .                   MXT,NC,NGL,PID)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(11,*)
C     REAL ou REAL*8
      my_real
     .   X(3,*),V(3,*),OFFG(*),OFF(*)
      my_real
     .    XLOC(MVSIZ,8), YLOC(MVSIZ,8), ZLOC(MVSIZ,8),
     .   VXLOC(MVSIZ,8),VYLOC(MVSIZ,8),VZLOC(MVSIZ,8)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NC(8,MVSIZ), MXT(MVSIZ), PID(MVSIZ), NGL(MVSIZ), I, 
     .        MXT_1 
C     REAL ou REAL*8
      my_real
     .    VIS(MVSIZ)
      my_real
     .   OFF_L
C-----------------------------------------------
      OFF_L  = ZERO
      MXT_1 = IXS(1,LFT)
      DO 10 I=LFT,LLT
      NGL(I)=IXS(11,I)
      MXT(I)=MXT_1
      PID(I)=IXS(10,I)
      NC(1,I)=IXS(2,I)
      NC(2,I)=IXS(3,I)
      NC(3,I)=IXS(4,I)
      NC(4,I)=IXS(5,I)
      NC(5,I)=IXS(6,I)
      NC(6,I)=IXS(7,I)
      NC(7,I)=IXS(8,I)
   10 NC(8,I)=IXS(9,I)
C
C----------------------------
C     NODAL COORDINATES     |
C----------------------------
      DO 20 I=LFT,LLT
      XLOC(I,1)=X(1,NC(1,I))
      YLOC(I,1)=X(2,NC(1,I))
      ZLOC(I,1)=X(3,NC(1,I))
      XLOC(I,2)=X(1,NC(2,I))
      YLOC(I,2)=X(2,NC(2,I))
      ZLOC(I,2)=X(3,NC(2,I))
      XLOC(I,3)=X(1,NC(3,I))
      YLOC(I,3)=X(2,NC(3,I))
      ZLOC(I,3)=X(3,NC(3,I))
      XLOC(I,4)=X(1,NC(4,I))
      YLOC(I,4)=X(2,NC(4,I))
      ZLOC(I,4)=X(3,NC(4,I))
      XLOC(I,5)=X(1,NC(5,I))
      YLOC(I,5)=X(2,NC(5,I))
      ZLOC(I,5)=X(3,NC(5,I))
      XLOC(I,6)=X(1,NC(6,I))
      YLOC(I,6)=X(2,NC(6,I))
      ZLOC(I,6)=X(3,NC(6,I))
      XLOC(I,7)=X(1,NC(7,I))
      YLOC(I,7)=X(2,NC(7,I))
      ZLOC(I,7)=X(3,NC(7,I))
      XLOC(I,8)=X(1,NC(8,I))
      YLOC(I,8)=X(2,NC(8,I))
      ZLOC(I,8)=X(3,NC(8,I))
C
      VXLOC(I,1)=V(1,NC(1,I))
      VYLOC(I,1)=V(2,NC(1,I))
      VZLOC(I,1)=V(3,NC(1,I))
      VXLOC(I,2)=V(1,NC(2,I))
      VYLOC(I,2)=V(2,NC(2,I))
      VZLOC(I,2)=V(3,NC(2,I))
      VXLOC(I,3)=V(1,NC(3,I))
      VYLOC(I,3)=V(2,NC(3,I))
      VZLOC(I,3)=V(3,NC(3,I))
      VXLOC(I,4)=V(1,NC(4,I))
      VYLOC(I,4)=V(2,NC(4,I))
      VZLOC(I,4)=V(3,NC(4,I))
      VXLOC(I,5)=V(1,NC(5,I))
      VYLOC(I,5)=V(2,NC(5,I))
      VZLOC(I,5)=V(3,NC(5,I))
      VXLOC(I,6)=V(1,NC(6,I))
      VYLOC(I,6)=V(2,NC(6,I))
      VZLOC(I,6)=V(3,NC(6,I))
      VXLOC(I,7)=V(1,NC(7,I))
      VYLOC(I,7)=V(2,NC(7,I))
      VZLOC(I,7)=V(3,NC(7,I))
      VXLOC(I,8)=V(1,NC(8,I))
      VYLOC(I,8)=V(2,NC(8,I))
      VZLOC(I,8)=V(3,NC(8,I))
      OFF(I) = MIN(UN,ABS(OFFG(I)))
      OFF_L  = MIN(OFF_L,OFFG(I))
   20 CONTINUE
      IF(OFF_L.LT.ZERO)THEN
        DO I=LFT,LLT
          IF(OFFG(I).LT.0.)THEN
            VXLOC(I,1)=0.
            VYLOC(I,1)=0.
            VZLOC(I,1)=0.
            VXLOC(I,2)=0.
            VYLOC(I,2)=0.
            VZLOC(I,2)=0.
            VXLOC(I,3)=0.
            VYLOC(I,3)=0.
            VZLOC(I,3)=0.
            VXLOC(I,4)=0.
            VYLOC(I,4)=0.
            VZLOC(I,4)=0.
            VXLOC(I,5)=0.
            VYLOC(I,5)=0.
            VZLOC(I,5)=0.
            VXLOC(I,6)=0.
            VYLOC(I,6)=0.
            VZLOC(I,6)=0.
            VXLOC(I,7)=0.
            VYLOC(I,7)=0.
            VZLOC(I,7)=0.
            VXLOC(I,8)=0.
            VYLOC(I,8)=0.
            VZLOC(I,8)=0.
          ENDIF
        ENDDO
      ENDIF
C
      RETURN
      END
