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
Chd|  SPSGSYM                       source/elements/sph/spsgsym.F 
Chd|-- called by -----------
Chd|        FORINTP                       source/elements/forintp.F     
Chd|-- calls ---------------
Chd|        SPHBOX                        share/modules/sphbox.F        
Chd|====================================================================
      SUBROUTINE SPSGSYM(
     1    ISPCOND   ,XFRAME    ,ISPSYM    ,XSPSYM    ,VSPSYM    ,
     2    WA        ,WASIGSM   ,WASPACT   ,WAR       )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SPHBOX
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "sphcom.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISPCOND(NISPCOND,*), ISPSYM(NSPCOND,*), WASPACT(*)
C     REAL ou REAL*8
      my_real
     .   XFRAME(NXFRAME,*) ,XSPSYM(3,*) ,VSPSYM(3,*), WA(KWASPH,*), 
     .   WASIGSM(6,*), WAR(10,*) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IC,NC,IS,SM,JS,ISLIDE,SS
      my_real
     .       SXX,SXY,SXZ,SYY,SYZ,SZZ,
     .       TXX,TXY,TXZ,TYY,TYZ,TZZ,
     .       OX,OY,OZ,UX,UY,UZ,VX,VY,VZ,WX,WY,WZ,
     .       UXX,UXY,UXZ,UYX,UYY,UYZ,UZX,UZY,UZZ,
     .       VXX,VXY,VXZ,VYY,VYZ,VZZ
C-----------------------------------------------
C       Prepare les contraintes sur les particules symetriques.
C-----------------------------------------------
      DO NC=1,NSPCOND
        IC=ISPCOND(2,NC)
        IS=ISPCOND(3,NC)
        ISLIDE=ISPCOND(5,NC)
        IF (IC.EQ.1) THEN
            OX=XFRAME(10,IS)
            OY=XFRAME(11,IS)
            OZ=XFRAME(12,IS)
            UX=XFRAME(1,IS)
            UY=XFRAME(2,IS)
            UZ=XFRAME(3,IS)
            VX=XFRAME(4,IS)
            VY=XFRAME(5,IS)
            VZ=XFRAME(6,IS)
            WX=XFRAME(7,IS)
            WY=XFRAME(8,IS)
            WZ=XFRAME(9,IS)
        ELSEIF (IC.EQ.2) THEN
            OX=XFRAME(10,IS)
            OY=XFRAME(11,IS)
            OZ=XFRAME(12,IS)
            UX=XFRAME(4,IS)
            UY=XFRAME(5,IS)
            UZ=XFRAME(6,IS)
            VX=XFRAME(7,IS)
            VY=XFRAME(8,IS)
            VZ=XFRAME(9,IS)
            WX=XFRAME(1,IS)
            WY=XFRAME(2,IS)
            WZ=XFRAME(3,IS)
        ELSEIF (IC.EQ.3) THEN
            OX=XFRAME(10,IS)
            OY=XFRAME(11,IS)
            OZ=XFRAME(12,IS)
            UX=XFRAME(7,IS)
            UY=XFRAME(8,IS)
            UZ=XFRAME(9,IS)
            VX=XFRAME(1,IS)
            VY=XFRAME(2,IS)
            VZ=XFRAME(3,IS)
            WX=XFRAME(4,IS)
            WY=XFRAME(5,IS)
            WZ=XFRAME(6,IS)
        ENDIF
        DO SS=1,NSPHACT
         SM=WASPACT(SS)
         JS=ISPSYM(NC,SM)
         IF(JS.GT.0)THEN
          TXX=WA(1,SM)
          TYY=WA(2,SM)
          TZZ=WA(3,SM)
          TXY=WA(4,SM)
          TYZ=WA(5,SM)
          TXZ=WA(6,SM)
          IF(ISLIDE.EQ.0)THEN
C----------
           WASIGSM(1,JS)=TXX
           WASIGSM(2,JS)=TYY
           WASIGSM(3,JS)=TZZ
           WASIGSM(4,JS)=TXY
           WASIGSM(5,JS)=TYZ
           WASIGSM(6,JS)=TXZ
          ELSE
C----------
C          Changmnt de repere.
           UXX=TXX*UX+TXY*UY+TXZ*UZ
           UXY=TXX*VX+TXY*VY+TXZ*VZ
           UXZ=TXX*WX+TXY*WY+TXZ*WZ
           UYX=TXY*UX+TYY*UY+TYZ*UZ
           UYY=TXY*VX+TYY*VY+TYZ*VZ
           UYZ=TXY*WX+TYY*WY+TYZ*WZ
           UZX=TXZ*UX+TYZ*UY+TZZ*UZ
           UZY=TXZ*VX+TYZ*VY+TZZ*VZ
           UZZ=TXZ*WX+TYZ*WY+TZZ*WZ
           VXX=UX*UXX+UY*UYX+UZ*UZX
           VXY=UX*UXY+UY*UYY+UZ*UZY
           VXZ=UX*UXZ+UY*UYZ+UZ*UZZ
           VYY=VX*UXY+VY*UYY+VZ*UZY
           VYZ=VX*UXZ+VY*UYZ+VZ*UZZ
           VZZ=WX*UXZ+WY*UYZ+WZ*UZZ
C----------
C          Symetrie.
           VXY=-VXY
           VXZ=-VXZ
C----------
C          Back to global system.
           UXX=VXX*UX+VXY*VX+VXZ*WX
           UXY=VXX*UY+VXY*VY+VXZ*WY
           UXZ=VXX*UZ+VXY*VZ+VXZ*WZ
           UYX=VXY*UX+VYY*VX+VYZ*WX
           UYY=VXY*UY+VYY*VY+VYZ*WY
           UYZ=VXY*UZ+VYY*VZ+VYZ*WZ
           UZX=VXZ*UX+VYZ*VX+VZZ*WX
           UZY=VXZ*UY+VYZ*VY+VZZ*WY
           UZZ=VXZ*UZ+VYZ*VZ+VZZ*WZ
           TXX=UX*UXX+VX*UYX+WX*UZX
           TXY=UX*UXY+VX*UYY+WX*UZY
           TXZ=UX*UXZ+VX*UYZ+WX*UZZ
           TYY=UY*UXY+VY*UYY+WY*UZY
           TYZ=UY*UXZ+VY*UYZ+WY*UZZ
           TZZ=UZ*UXZ+VZ*UYZ+WZ*UZZ
C
           WASIGSM(1,JS)=TXX
           WASIGSM(2,JS)=TYY
           WASIGSM(3,JS)=TZZ
           WASIGSM(4,JS)=TXY
           WASIGSM(5,JS)=TYZ
           WASIGSM(6,JS)=TXZ
          ENDIF
         ENDIF
        ENDDO
C
C Particules symetriques de particules remotes
C
        DO SS=1,NSPHR
         JS=ISPSYMR(NC,SS)
         IF(JS.GT.0)THEN
          TXX=WAR(1,SS)
          TYY=WAR(2,SS)
          TZZ=WAR(3,SS)
          TXY=WAR(4,SS)
          TYZ=WAR(5,SS)
          TXZ=WAR(6,SS)
          IF(ISLIDE.EQ.0)THEN
C----------
           WASIGSM(1,JS)=TXX
           WASIGSM(2,JS)=TYY
           WASIGSM(3,JS)=TZZ
           WASIGSM(4,JS)=TXY
           WASIGSM(5,JS)=TYZ
           WASIGSM(6,JS)=TXZ
          ELSE
C----------
C          Changmnt de repere.
           UXX=TXX*UX+TXY*UY+TXZ*UZ
           UXY=TXX*VX+TXY*VY+TXZ*VZ
           UXZ=TXX*WX+TXY*WY+TXZ*WZ
           UYX=TXY*UX+TYY*UY+TYZ*UZ
           UYY=TXY*VX+TYY*VY+TYZ*VZ
           UYZ=TXY*WX+TYY*WY+TYZ*WZ
           UZX=TXZ*UX+TYZ*UY+TZZ*UZ
           UZY=TXZ*VX+TYZ*VY+TZZ*VZ
           UZZ=TXZ*WX+TYZ*WY+TZZ*WZ
           VXX=UX*UXX+UY*UYX+UZ*UZX
           VXY=UX*UXY+UY*UYY+UZ*UZY
           VXZ=UX*UXZ+UY*UYZ+UZ*UZZ
           VYY=VX*UXY+VY*UYY+VZ*UZY
           VYZ=VX*UXZ+VY*UYZ+VZ*UZZ
           VZZ=WX*UXZ+WY*UYZ+WZ*UZZ
C----------
C          Symetrie.
           VXY=-VXY
           VXZ=-VXZ
C----------
C          Back to global system.
           UXX=VXX*UX+VXY*VX+VXZ*WX
           UXY=VXX*UY+VXY*VY+VXZ*WY
           UXZ=VXX*UZ+VXY*VZ+VXZ*WZ
           UYX=VXY*UX+VYY*VX+VYZ*WX
           UYY=VXY*UY+VYY*VY+VYZ*WY
           UYZ=VXY*UZ+VYY*VZ+VYZ*WZ
           UZX=VXZ*UX+VYZ*VX+VZZ*WX
           UZY=VXZ*UY+VYZ*VY+VZZ*WY
           UZZ=VXZ*UZ+VYZ*VZ+VZZ*WZ
           TXX=UX*UXX+VX*UYX+WX*UZX
           TXY=UX*UXY+VX*UYY+WX*UZY
           TXZ=UX*UXZ+VX*UYZ+WX*UZZ
           TYY=UY*UXY+VY*UYY+WY*UZY
           TYZ=UY*UXZ+VY*UYZ+WY*UZZ
           TZZ=UZ*UXZ+VZ*UYZ+WZ*UZZ
C
           WASIGSM(1,JS)=TXX
           WASIGSM(2,JS)=TYY
           WASIGSM(3,JS)=TZZ
           WASIGSM(4,JS)=TXY
           WASIGSM(5,JS)=TYZ
           WASIGSM(6,JS)=TXZ
          END IF
         END IF
        END DO
C
      ENDDO
C----------------------------------
      RETURN
      END
