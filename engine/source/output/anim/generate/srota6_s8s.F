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
Chd|  SROTA6_S8S                    source/output/anim/generate/srota6_s8s.F
Chd|-- called by -----------
Chd|        TENSORS                       source/output/anim/generate/tensor6.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SROTA6_S8S(KCVT  ,TENS  ,GAMA  ,KHBE  ,ITYP  ,FRAME)
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
C     REAL ou REAL*8
      my_real
     .   TENS(6),GAMA(6),FRAME(3,3)
      INTEGER IXS(NIXS), KCVT, KHBE, ITYP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL ou REAL*8
      my_real
     .   L11,L12,L13,L22,L23,L33,
     .   R11,R12,R13,R21,R22,R23,R31,R32,R33,
     .   G11,G22,G33,G12,G21,G23,G32,G13,G31,
     .   T11,T22,T33,T12,T21,T23,T32,T13,T31,
     .   S11,S12,S21,S13,S31,S22,S23,S32,S33
C-----------------------------------------------
C---- should not rotate w/ global iso system
!       IF (KCVT==0.AND.ISORTH==0) RETURN
C----------------------------
      IF (KHBE /= 17 .OR. IINT /= 3) RETURN
         R11 = FRAME(1,1)
	   R21 = FRAME(2,1)
	   R31 = FRAME(3,1)
	   R12 = FRAME(1,2)
	   R22 = FRAME(2,2)
	   R32 = FRAME(3,2)
	   R13 = FRAME(1,3)
	   R23 = FRAME(2,3)
	   R33 = FRAME(3,3)
!       ENDIF
C-----------
C     ORTHOTROPIC ELEMENTS : Rotation from ORHTOROPIC FRAME TO LOCAL FRAME
C-----------
      IF (KCVT.EQ.2) THEN
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
