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
Chd|  SROTORTH                      source/elements/solid/srotorth.F
Chd|-- called by -----------
Chd|        DFUNCS                        source/output/anim/generate/dfunc6.F
Chd|        H3D_SOLID_SCALAR_1            source/output/h3d/h3d_results/h3d_solid_scalar_1.F
Chd|        STAT_S_ORTHO                  source/output/sta/stat_s_ortho.F
Chd|-- calls ---------------
Chd|        SCORTHO31                     source/elements/thickshell/solidec/scortho31.F
Chd|        SORTHO31                      source/elements/solid/solide/sortho31.F
Chd|====================================================================
      SUBROUTINE SROTORTH(X     ,IXS   ,GAMA  , KHBE  ,ITYP  ,
     .                    ICSIG )
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
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "scr18_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   X(3,*),GAMA(6)
      INTEGER IXS(NIXS), KHBE, ITYP,ICSIG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   X1, X2, X3, X4,
     .   X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4,
     .   Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4,
     .   Z5, Z6, Z7, Z8,
     .   L11,L12,L13,L22,L23,L33,
     .   R11,R12,R13,R21,R22,R23,R31,R32,R33,
     .   G11,G22,G33,G12,G21,G23,G32,G13,G31,
     .   T11,T22,T33,T12,T21,T23,T32,T13,T31,
     .   S11,S12,S21,S13,S31,S22,S23,S32,S33,
     .   CP ,SP
      INTEGER NC1, NC2, NC3, NC4,
     .        NC5, NC6, NC7, NC8
C-----------------------------------------------
        NC1=IXS(2)
        NC2=IXS(3)
        NC3=IXS(4)
        NC4=IXS(5)
        NC5=IXS(6)
        NC6=IXS(7)
        NC7=IXS(8)
        NC8=IXS(9)
C----------------------------
C     NODAL COORDINATES     |
C----------------------------
        X1=X(1,NC1)
        Y1=X(2,NC1)
        Z1=X(3,NC1)
        X2=X(1,NC2)
        Y2=X(2,NC2)
        Z2=X(3,NC2)
        X3=X(1,NC3)
        Y3=X(2,NC3)
        Z3=X(3,NC3)
        X4=X(1,NC4)
        Y4=X(2,NC4)
        Z4=X(3,NC4)
        X5=X(1,NC5)
        Y5=X(2,NC5)
        Z5=X(3,NC5)
        X6=X(1,NC6)
        Y6=X(2,NC6)
        Z6=X(3,NC6)
        X7=X(1,NC7)
        Y7=X(2,NC7)
        Z7=X(3,NC7)
        X8=X(1,NC8)
        Y8=X(2,NC8)
        Z8=X(3,NC8)
C-----------
       IF(KHBE/=15)THEN
C        REPERE CONVECTE
         IF (KHBE==24.OR.KHBE==14) THEN
           CALL SORTHO31(
     .          X1, X2, X3, X4, X5, X6, X7, X8,
     .          Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .          Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .          R12, R13, R11, R22, R23, R21, R32, R33, R31)
         ELSE
           CALL SORTHO31(
     .          X1, X2, X3, X4, X5, X6, X7, X8,
     .          Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .          Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .          R11, R12, R13, R21, R22, R23, R31, R32, R33)
         ENDIF
       ELSE
         CALL SCORTHO31(
     .               X1, X2, X3, X4, X5, X6, X7, X8,
     .               Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .               Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .               R11, R12, R13, R21, R22, R23, R31, R32, R33)
       ENDIF
C-----------
C     REPERE ORTHOTROPE.
C-----------
       IF(ITYP == 21 .OR. ITYP == 22) THEN
          CP = GAMA(1) ! COS(PHI)
          SP = GAMA(2) ! SIN(PHI)
          IF(ICSIG == 10.OR.KHBE == 15)THEN
             T11=R11*CP+R12*SP
             T12=R21*CP+R22*SP
             T13=R31*CP+R32*SP
             T21=R12*CP-R11*SP
             T22=R22*CP-R21*SP
             T23=R32*CP-R31*SP
          ELSEIF(ICSIG == 100)THEN
             T11=R13*CP+R11*SP
             T12=R23*CP+R21*SP
             T13=R33*CP+R31*SP
             T21=R11*CP-R13*SP
             T22=R21*CP-R23*SP
             T23=R31*CP-R33*SP
          ELSEIF(ICSIG == 1 )THEN
             T11=R12*CP+R13*SP
             T12=R22*CP+R23*SP
             T13=R32*CP+R33*SP
             T21=R13*CP-R12*SP
             T22=R23*CP-R22*SP
             T23=R33*CP-R32*SP
          ENDIF
          GAMA(1)=T11
          GAMA(2)=T12
          GAMA(3)=T13
          GAMA(4)=T21
          GAMA(5)=T22
          GAMA(6)=T23
       ELSE
          G11=GAMA(1)
          G21=GAMA(2)
          G31=GAMA(3)
          G12=GAMA(4)
          G22=GAMA(5)
          G32=GAMA(6)
          G13=G21*G32-G31*G22
          G23=G31*G12-G11*G32
          G33=G11*G22-G21*G12
C       MATRICE DE PASSAGE GLOBAL -> ORTHOTROPE.
          T11=R11*G11+R12*G21+R13*G31
          T12=R11*G12+R12*G22+R13*G32
          T13=R11*G13+R12*G23+R13*G33
          T21=R21*G11+R22*G21+R23*G31
          T22=R21*G12+R22*G22+R23*G32
          T23=R21*G13+R22*G23+R23*G33
          T31=R31*G11+R32*G21+R33*G31
          T32=R31*G12+R32*G22+R33*G32
          T33=R31*G13+R32*G23+R33*G33
          GAMA(1)=T11
          GAMA(2)=T21
          GAMA(3)=T31
          GAMA(4)=T12
          GAMA(5)=T22
          GAMA(6)=T32
        ENDIF
C-----------
      RETURN
      END
