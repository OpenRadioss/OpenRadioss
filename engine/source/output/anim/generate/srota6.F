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
Chd|  SROTA6                        source/output/anim/generate/srota6.F
Chd|-- called by -----------
Chd|        FORINT                        source/elements/forint.F      
Chd|        GPSSTRAIN_SKIN                source/output/anim/generate/tensgpstrain.F
Chd|        H3D_SOLID_TENSOR_1            source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|        STAT_S_STRAF                  source/output/sta/stat_s_straf.F
Chd|        STAT_S_STRSF                  source/output/sta/stat_s_strsf.F
Chd|        STRN_TENSCOR3                 source/output/h3d/h3d_results/h3d_strn_tenscor3.F
Chd|        STRS_TENSCOR3                 source/output/h3d/h3d_results/strs_tenscor3.F
Chd|        TENSGPS1                      source/output/anim/generate/tensor6.F
Chd|        TENSGPS2                      source/output/anim/generate/tensor6.F
Chd|        TENSGPS3                      source/output/anim/generate/tensor6.F
Chd|        TENSGPSTRAIN                  source/output/anim/generate/tensgpstrain.F
Chd|        TENSGPS_SKIN                  source/output/anim/generate/tensor6.F
Chd|        TENSORS                       source/output/anim/generate/tensor6.F
Chd|        THSOL                         source/output/th/thsol.F      
Chd|-- calls ---------------
Chd|        SCOOR431                      source/elements/solid/sconnect/scoor431.F
Chd|        SCORTHO31                     source/elements/thickshell/solidec/scortho31.F
Chd|        SORTHO31                      source/elements/solid/solide/sortho31.F
Chd|====================================================================
      SUBROUTINE SROTA6(X     ,IXS   ,KCVT  ,TENS  ,GAMA  ,
     .                  KHBE  ,ITYP  )
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
     .   X(3,*),TENS(6),GAMA(6)
      INTEGER IXS(NIXS), KCVT, KHBE, ITYP
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
     .   S11,S12,S21,S13,S31,S22,S23,S32,S33
      INTEGER NC1, NC2, NC3, NC4,
     .        NC5, NC6, NC7, NC8
C-----------------------------------------------
C---- should not rotate w/ global iso system
       IF (KCVT==0.AND.ISORTH==0) RETURN
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
       IF (ITYP == 43) THEN
C        Corotational frame - hexa spotwelds
         CALL SCOOR431(
     .     X1, X2, X3, X4, X5, X6, X7, X8,
     .     Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .     Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .     R11, R12, R13, R21, R22, R23, R31, R32, R33)
       ELSEIF(KHBE/=15)THEN
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
C     ORTHOTROPIC ELEMENTS : Rotation from ORHTOROPIC FRAME TO LOCAL FRAME
C-----------
      IF (KCVT==2) THEN
        IF (ISORTH > 0) THEN
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
        R11=T11
        R12=T12
        R13=T13
        R21=T21
        R22=T22
        R23=T23
        R31=T31
        R32=T32
        R33=T33
       ENDIF
      ENDIF
C-----------
C      Rotation from LOCAL FRAME TO GLOBAL FRAME
C-----------
C     TENSOR ROTATION.
C        R passage Global -> Local.
C        R S Transpose(R)
C    1     SX
C    2     SY
C    3     SZ
C    4     SXY
C    5     SYZ
C    6     SXZ
C-----------
        L11    =TENS(1)
        L22    =TENS(2)
        L33    =TENS(3)
        L12    =TENS(4)
        L23    =TENS(5)
        L13    =TENS(6)
        S11    =L11*R11+L12*R12+L13*R13
        S12    =L11*R21+L12*R22+L13*R23
        S13    =L11*R31+L12*R32+L13*R33
        S21    =L12*R11+L22*R12+L23*R13
        S22    =L12*R21+L22*R22+L23*R23
        S23    =L12*R31+L22*R32+L23*R33
        S31    =L13*R11+L23*R12+L33*R13
        S32    =L13*R21+L23*R22+L33*R23
        S33    =L13*R31+L23*R32+L33*R33
        TENS(1)=R11*S11+R12*S21+R13*S31
        TENS(2)=R21*S12+R22*S22+R23*S32
        TENS(3)=R31*S13+R32*S23+R33*S33
        TENS(4)=R11*S12+R12*S22+R13*S32
        TENS(5)=R21*S13+R22*S23+R23*S33
        TENS(6)=R11*S13+R12*S23+R13*S33
C-----------
      RETURN
      END
