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
Chd|  S8FINT3                       source/elements/solid/solide8/s8fint3.F
Chd|-- called by -----------
Chd|        S8FORC3                       source/elements/solid/solide8/s8forc3.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE S8FINT3(BUFLY,VOLGP,QVIS,
     .                   PX1,PX2,PX3,PX4,PX5,PX6,PX7,PX8,
     .                   PY1,PY2,PY3,PY4,PY5,PY6,PY7,PY8,
     .                   PZ1,PZ2,PZ3,PZ4,PZ5,PZ6,PZ7,PZ8,NEL)

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
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "nsvis_c.inc"
#include      "vecfx_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
C     REAL ou REAL*8
      my_real VOLGP(MVSIZ,8),QVIS(NEL),
     .   PX1(MVSIZ,8),PX2(MVSIZ,8),PX3(MVSIZ,8),PX4(MVSIZ,8),      
     .   PX5(MVSIZ,8),PX6(MVSIZ,8),PX7(MVSIZ,8),PX8(MVSIZ,8),      
     .   PY1(MVSIZ,8),PY2(MVSIZ,8),PY3(MVSIZ,8),PY4(MVSIZ,8),      
     .   PY5(MVSIZ,8),PY6(MVSIZ,8),PY7(MVSIZ,8),PY8(MVSIZ,8),      
     .   PZ1(MVSIZ,8),PZ2(MVSIZ,8),PZ3(MVSIZ,8),PZ4(MVSIZ,8),      
     .   PZ5(MVSIZ,8),PZ6(MVSIZ,8),PZ7(MVSIZ,8),PZ8(MVSIZ,8)
      TYPE (BUF_LAY_)  ,TARGET :: BUFLY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IPT,II(6)
C     REAL ou REAL*8
      my_real
     .    S1, S2, S3,
     .    S4, S5, S6
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
C-----------------------------------------------
!
      DO I=1,6
        II(I) = NEL*(I-1)
      ENDDO
!
      DO I=LFT,LLT
        F11(I)=ZERO
        F21(I)=ZERO
        F31(I)=ZERO
        F12(I)=ZERO
        F22(I)=ZERO
        F32(I)=ZERO
        F13(I)=ZERO
        F23(I)=ZERO
        F33(I)=ZERO
        F14(I)=ZERO
        F24(I)=ZERO
        F34(I)=ZERO
        F15(I)=ZERO
        F25(I)=ZERO
        F35(I)=ZERO
        F16(I)=ZERO
        F26(I)=ZERO
        F36(I)=ZERO
        F17(I)=ZERO
        F27(I)=ZERO
        F37(I)=ZERO
        F18(I)=ZERO
        F28(I)=ZERO
        F38(I)=ZERO
      ENDDO
C
      DO IPT=1,8
        LBUF => BUFLY%LBUF(1,1,IPT)
        DO I=LFT,LLT
          J = 6*(I-1)
          S1=(LBUF%SIG(II(1)+I)+SVIS(I,1)-QVIS(I))*VOLGP(I,IPT)
          S2=(LBUF%SIG(II(2)+I)+SVIS(I,2)-QVIS(I))*VOLGP(I,IPT)
          S3=(LBUF%SIG(II(3)+I)+SVIS(I,3)-QVIS(I))*VOLGP(I,IPT)
          S4=(LBUF%SIG(II(4)+I)+SVIS(I,4))*VOLGP(I,IPT)
          S5=(LBUF%SIG(II(5)+I)+SVIS(I,5))*VOLGP(I,IPT)
          S6=(LBUF%SIG(II(6)+I)+SVIS(I,6))*VOLGP(I,IPT)
C
          F11(I)=F11(I)
     -          -(S1*PX1(I,IPT)+S4*PY1(I,IPT)+S6*PZ1(I,IPT))
          F21(I)=F21(I)
     -          -(S2*PY1(I,IPT)+S4*PX1(I,IPT)+S5*PZ1(I,IPT))
          F31(I)=F31(I)
     -          -(S3*PZ1(I,IPT)+S6*PX1(I,IPT)+S5*PY1(I,IPT))
          F12(I)=F12(I)
     -          -(S1*PX2(I,IPT)+S4*PY2(I,IPT)+S6*PZ2(I,IPT))
          F22(I)=F22(I)
     -          -(S2*PY2(I,IPT)+S4*PX2(I,IPT)+S5*PZ2(I,IPT))
          F32(I)=F32(I)
     -          -(S3*PZ2(I,IPT)+S6*PX2(I,IPT)+S5*PY2(I,IPT))
          F13(I)=F13(I)
     -          -(S1*PX3(I,IPT)+S4*PY3(I,IPT)+S6*PZ3(I,IPT))
          F23(I)=F23(I)
     -          -(S2*PY3(I,IPT)+S4*PX3(I,IPT)+S5*PZ3(I,IPT))
          F33(I)=F33(I)
     -          -(S3*PZ3(I,IPT)+S6*PX3(I,IPT)+S5*PY3(I,IPT))
          F14(I)=F14(I)
     -          -(S1*PX4(I,IPT)+S4*PY4(I,IPT)+S6*PZ4(I,IPT))
          F24(I)=F24(I)
     -          -(S2*PY4(I,IPT)+S4*PX4(I,IPT)+S5*PZ4(I,IPT))
          F34(I)=F34(I)
     -          -(S3*PZ4(I,IPT)+S6*PX4(I,IPT)+S5*PY4(I,IPT))
          F15(I)=F15(I)
     -          -(S1*PX5(I,IPT)+S4*PY5(I,IPT)+S6*PZ5(I,IPT))
          F25(I)=F25(I)
     -          -(S2*PY5(I,IPT)+S4*PX5(I,IPT)+S5*PZ5(I,IPT))
          F35(I)=F35(I)
     -          -(S3*PZ5(I,IPT)+S6*PX5(I,IPT)+S5*PY5(I,IPT))
          F16(I)=F16(I)
     -          -(S1*PX6(I,IPT)+S4*PY6(I,IPT)+S6*PZ6(I,IPT))
          F26(I)=F26(I)
     -          -(S2*PY6(I,IPT)+S4*PX6(I,IPT)+S5*PZ6(I,IPT))
          F36(I)=F36(I)
     -          -(S3*PZ6(I,IPT)+S6*PX6(I,IPT)+S5*PY6(I,IPT))
          F17(I)=F17(I)
     -          -(S1*PX7(I,IPT)+S4*PY7(I,IPT)+S6*PZ7(I,IPT))
          F27(I)=F27(I)
     -          -(S2*PY7(I,IPT)+S4*PX7(I,IPT)+S5*PZ7(I,IPT))
          F37(I)=F37(I)
     -          -(S3*PZ7(I,IPT)+S6*PX7(I,IPT)+S5*PY7(I,IPT))
          F18(I)=F18(I)
     -          -(S1*PX8(I,IPT)+S4*PY8(I,IPT)+S6*PZ8(I,IPT))
          F28(I)=F28(I)
     -          -(S2*PY8(I,IPT)+S4*PX8(I,IPT)+S5*PZ8(I,IPT))
          F38(I)=F38(I)
     -          -(S3*PZ8(I,IPT)+S6*PX8(I,IPT)+S5*PY8(I,IPT))
        ENDDO
C
      ENDDO
c-----------
      RETURN
      END
