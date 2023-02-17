Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  S8SFINT3                      source/elements/solid/solide8s/s8sfint3.F
Chd|-- called by -----------
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8SFINT3(SIG,
     .   F11,F21,F31,F12,F22,F32,F13,F23,F33,F14,F24,F34,
     .   F15,F25,F35,F16,F26,F36,F17,F27,F37,F18,F28,F38,
     .   DN_X, DN_Y, DN_Z, BB,
     .   VOL,QVIS,ICP,JFAC,NEL ,I_SH,IDEGE)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICP,NEL,I_SH,IDEGE(*)
C     REAL
      my_real
     .   SIG(NEL,6),
     .   DN_X(MVSIZ,8),DN_Y(MVSIZ,8),DN_Z(MVSIZ,8),
     .   F11(*),F21(*),F31(*),F12(*),F22(*),F32(*),
     .   F13(*),F23(*),F33(*),F14(*),F24(*),F34(*),
     .   F15(*),F25(*),F35(*),F16(*),F26(*),F36(*),
     .   F17(*),F27(*),F37(*),F18(*),F28(*),F38(*),
     .   VOL(*),QVIS(*),JFAC(*),BB(6,24,MVSIZ)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,IB
C     REAL
      my_real
     .   S1(MVSIZ), S2(MVSIZ), S3(MVSIZ),
     .   S4(MVSIZ), S5(MVSIZ), S6(MVSIZ),
     .   P(MVSIZ),COEF,FVOL
C-----------------------------------------------
      
      IF (ICP==1.AND.(ISMSTR==10.OR.ISMSTR==12)) THEN
       DO I=LFT,LLT
        FVOL=JFAC(I)*VOL(I)
        S1(I)=(SIG(I,1)+SVIS(I,1)-QVIS(I))*FVOL
        S2(I)=(SIG(I,2)+SVIS(I,2)-QVIS(I))*FVOL
        S3(I)=(SIG(I,3)+SVIS(I,3)-QVIS(I))*FVOL
        S4(I)=(SIG(I,4)+SVIS(I,4))*FVOL
        S5(I)=(SIG(I,5)+SVIS(I,5))*FVOL
        S6(I)=(SIG(I,6)+SVIS(I,6))*FVOL
       ENDDO
      ELSEIF (ICP==1) THEN
       COEF=ZEP3 
       DO I=LFT,LLT
        P(I) =COEF*(SIG(I,1)+SIG(I,2)+SIG(I,3)
     .            +SVIS(I,1)+SVIS(I,2)+SVIS(I,3))
        IF (IDEGE(I)>10) P(I) =QVIS(I)
        S1(I)=(SIG(I,1)+SVIS(I,1)-P(I))*VOL(I)
        S2(I)=(SIG(I,2)+SVIS(I,2)-P(I))*VOL(I)
        S3(I)=(SIG(I,3)+SVIS(I,3)-P(I))*VOL(I)
        S4(I)=(SIG(I,4)+SVIS(I,4))*VOL(I)
        S5(I)=(SIG(I,5)+SVIS(I,5))*VOL(I)
        S6(I)=(SIG(I,6)+SVIS(I,6))*VOL(I)
       ENDDO
      ELSE
      DO I=LFT,LLT
        S1(I)=(SIG(I,1)+SVIS(I,1)-QVIS(I))*VOL(I)
        S2(I)=(SIG(I,2)+SVIS(I,2)-QVIS(I))*VOL(I)
        S3(I)=(SIG(I,3)+SVIS(I,3)-QVIS(I))*VOL(I)
        S4(I)=(SIG(I,4)+SVIS(I,4))*VOL(I)
        S5(I)=(SIG(I,5)+SVIS(I,5))*VOL(I)
        S6(I)=(SIG(I,6)+SVIS(I,6))*VOL(I)
       ENDDO
      ENDIF
      ! Cas ANS
        DO I=LFT,LLT
        F11(I)=F11(I)-(S1(I)*BB(1,1,I)+S2(I)*BB(2,1,I)+S3(I)*BB(3,1,I)
     .                +S4(I)*BB(4,1,I)+S6(I)*BB(5,1,I)+S5(I)*BB(6,1,I))
        F21(I)=F21(I)-(S1(I)*BB(1,2,I)+S2(I)*BB(2,2,I)+S3(I)*BB(3,2,I)
     .                +S4(I)*BB(4,2,I)+S6(I)*BB(5,2,I)+S5(I)*BB(6,2,I))
        F31(I)=F31(I)-(S1(I)*BB(1,3,I)+S2(I)*BB(2,3,I)+S3(I)*BB(3,3,I)
     .                +S4(I)*BB(4,3,I)+S6(I)*BB(5,3,I)+S5(I)*BB(6,3,I))

        F12(I)=F12(I)-(S1(I)*BB(1,4,I)+S2(I)*BB(2,4,I)+S3(I)*BB(3,4,I)
     .                +S4(I)*BB(4,4,I)+S6(I)*BB(5,4,I)+S5(I)*BB(6,4,I))
        F22(I)=F22(I)-(S1(I)*BB(1,5,I)+S2(I)*BB(2,5,I)+S3(I)*BB(3,5,I)
     .                +S4(I)*BB(4,5,I)+S6(I)*BB(5,5,I)+S5(I)*BB(6,5,I))
        F32(I)=F32(I)-(S1(I)*BB(1,6,I)+S2(I)*BB(2,6,I)+S3(I)*BB(3,6,I)
     .                +S4(I)*BB(4,6,I)+S6(I)*BB(5,6,I)+S5(I)*BB(6,6,I))

        F13(I)=F13(I)-(S1(I)*BB(1,7,I)+S2(I)*BB(2,7,I)+S3(I)*BB(3,7,I)
     .                +S4(I)*BB(4,7,I)+S6(I)*BB(5,7,I)+S5(I)*BB(6,7,I))
        F23(I)=F23(I)-(S1(I)*BB(1,8,I)+S2(I)*BB(2,8,I)+S3(I)*BB(3,8,I)
     .                +S4(I)*BB(4,8,I)+S6(I)*BB(5,8,I)+S5(I)*BB(6,8,I))
        F33(I)=F33(I)-(S1(I)*BB(1,9,I)+S2(I)*BB(2,9,I)+S3(I)*BB(3,9,I)
     .                +S4(I)*BB(4,9,I)+S6(I)*BB(5,9,I)+S5(I)*BB(6,9,I))

        F14(I)=F14(I)-(S1(I)*BB(1,10,I)+S2(I)*BB(2,10,I)+S3(I)*BB(3,10,I)
     .                +S4(I)*BB(4,10,I)+S6(I)*BB(5,10,I)+S5(I)*BB(6,10,I))
        F24(I)=F24(I)-(S1(I)*BB(1,11,I)+S2(I)*BB(2,11,I)+S3(I)*BB(3,11,I)
     .                +S4(I)*BB(4,11,I)+S6(I)*BB(5,11,I)+S5(I)*BB(6,11,I))
        F34(I)=F34(I)-(S1(I)*BB(1,12,I)+S2(I)*BB(2,12,I)+S3(I)*BB(3,12,I)
     .                +S4(I)*BB(4,12,I)+S6(I)*BB(5,12,I)+S5(I)*BB(6,12,I))

        F15(I)=F15(I)-(S1(I)*BB(1,13,I)+S2(I)*BB(2,13,I)+S3(I)*BB(3,13,I)
     .                +S4(I)*BB(4,13,I)+S6(I)*BB(5,13,I)+S5(I)*BB(6,13,I))
        F25(I)=F25(I)-(S1(I)*BB(1,14,I)+S2(I)*BB(2,14,I)+S3(I)*BB(3,14,I)
     .                +S4(I)*BB(4,14,I)+S6(I)*BB(5,14,I)+S5(I)*BB(6,14,I))
        F35(I)=F35(I)-(S1(I)*BB(1,15,I)+S2(I)*BB(2,15,I)+S3(I)*BB(3,15,I)
     .                +S4(I)*BB(4,15,I)+S6(I)*BB(5,15,I)+S5(I)*BB(6,15,I))

        F16(I)=F16(I)-(S1(I)*BB(1,16,I)+S2(I)*BB(2,16,I)+S3(I)*BB(3,16,I)
     .                +S4(I)*BB(4,16,I)+S6(I)*BB(5,16,I)+S5(I)*BB(6,16,I))
        F26(I)=F26(I)-(S1(I)*BB(1,17,I)+S2(I)*BB(2,17,I)+S3(I)*BB(3,17,I)
     .                +S4(I)*BB(4,17,I)+S6(I)*BB(5,17,I)+S5(I)*BB(6,17,I))
        F36(I)=F36(I)-(S1(I)*BB(1,18,I)+S2(I)*BB(2,18,I)+S3(I)*BB(3,18,I)
     .                +S4(I)*BB(4,18,I)+S6(I)*BB(5,18,I)+S5(I)*BB(6,18,I))

        F17(I)=F17(I)-(S1(I)*BB(1,19,I)+S2(I)*BB(2,19,I)+S3(I)*BB(3,19,I)
     .                +S4(I)*BB(4,19,I)+S6(I)*BB(5,19,I)+S5(I)*BB(6,19,I))
        F27(I)=F27(I)-(S1(I)*BB(1,20,I)+S2(I)*BB(2,20,I)+S3(I)*BB(3,20,I)
     .                +S4(I)*BB(4,20,I)+S6(I)*BB(5,20,I)+S5(I)*BB(6,20,I))
        F37(I)=F37(I)-(S1(I)*BB(1,21,I)+S2(I)*BB(2,21,I)+S3(I)*BB(3,21,I)
     .                +S4(I)*BB(4,21,I)+S6(I)*BB(5,21,I)+S5(I)*BB(6,21,I))

        F18(I)=F18(I)-(S1(I)*BB(1,22,I)+S2(I)*BB(2,22,I)+S3(I)*BB(3,22,I)
     .                +S4(I)*BB(4,22,I)+S6(I)*BB(5,22,I)+S5(I)*BB(6,22,I))
        F28(I)=F28(I)-(S1(I)*BB(1,23,I)+S2(I)*BB(2,23,I)+S3(I)*BB(3,23,I)
     .                +S4(I)*BB(4,23,I)+S6(I)*BB(5,23,I)+S5(I)*BB(6,23,I))
        F38(I)=F38(I)-(S1(I)*BB(1,24,I)+S2(I)*BB(2,24,I)+S3(I)*BB(3,24,I)
     .                +S4(I)*BB(4,24,I)+S6(I)*BB(5,24,I)+S5(I)*BB(6,24,I))

        ENDDO
        ! Cas de l'integration complete standard
!        DO I=LFT,LLT
!        F11(I)=F11(I)-(S1(I)*DN_X(I,1)+S4(I)*DN_Y(I,1)+S6(I)*DN_Z(I,1))
!        F21(I)=F21(I)-(S2(I)*DN_Y(I,1)+S4(I)*DN_X(I,1)+S5(I)*DN_Z(I,1))
!        F31(I)=F31(I)-(S3(I)*DN_Z(I,1)+S5(I)*DN_Y(I,1)+S6(I)*DN_X(I,1))
!
!        F12(I)=F12(I)-(S1(I)*DN_X(I,2)+S4(I)*DN_Y(I,2)+S6(I)*DN_Z(I,2))
!        F22(I)=F22(I)-(S2(I)*DN_Y(I,2)+S4(I)*DN_X(I,2)+S5(I)*DN_Z(I,2))
!        F32(I)=F32(I)-(S3(I)*DN_Z(I,2)+S5(I)*DN_Y(I,2)+S6(I)*DN_X(I,2))
!        
!        F13(I)=F13(I)-(S1(I)*DN_X(I,3)+S4(I)*DN_Y(I,3)+S6(I)*DN_Z(I,3))
!        F23(I)=F23(I)-(S2(I)*DN_Y(I,3)+S4(I)*DN_X(I,3)+S5(I)*DN_Z(I,3))
!        F33(I)=F33(I)-(S3(I)*DN_Z(I,3)+S5(I)*DN_Y(I,3)+S6(I)*DN_X(I,3))
!        
!        F14(I)=F14(I)-(S1(I)*DN_X(I,4)+S4(I)*DN_Y(I,4)+S6(I)*DN_Z(I,4))
!        F24(I)=F24(I)-(S2(I)*DN_Y(I,4)+S4(I)*DN_X(I,4)+S5(I)*DN_Z(I,4))
!        F34(I)=F34(I)-(S3(I)*DN_Z(I,4)+S5(I)*DN_Y(I,4)+S6(I)*DN_X(I,4))
!        
!        F15(I)=F15(I)-(S1(I)*DN_X(I,5)+S4(I)*DN_Y(I,5)+S6(I)*DN_Z(I,5))
!        F25(I)=F25(I)-(S2(I)*DN_Y(I,5)+S4(I)*DN_X(I,5)+S5(I)*DN_Z(I,5))
!        F35(I)=F35(I)-(S3(I)*DN_Z(I,5)+S5(I)*DN_Y(I,5)+S6(I)*DN_X(I,5))
!        
!        F16(I)=F16(I)-(S1(I)*DN_X(I,6)+S4(I)*DN_Y(I,6)+S6(I)*DN_Z(I,6))
!        F26(I)=F26(I)-(S2(I)*DN_Y(I,6)+S4(I)*DN_X(I,6)+S5(I)*DN_Z(I,6))
!        F36(I)=F36(I)-(S3(I)*DN_Z(I,6)+S5(I)*DN_Y(I,6)+S6(I)*DN_X(I,6))
!        
!        F17(I)=F17(I)-(S1(I)*DN_X(I,7)+S4(I)*DN_Y(I,7)+S6(I)*DN_Z(I,7))
!        F27(I)=F27(I)-(S2(I)*DN_Y(I,7)+S4(I)*DN_X(I,7)+S5(I)*DN_Z(I,7))
!        F37(I)=F37(I)-(S3(I)*DN_Z(I,7)+S5(I)*DN_Y(I,7)+S6(I)*DN_X(I,7))
!        
!        F18(I)=F18(I)-(S1(I)*DN_X(I,8)+S4(I)*DN_Y(I,8)+S6(I)*DN_Z(I,8))
!        F28(I)=F28(I)-(S2(I)*DN_Y(I,8)+S4(I)*DN_X(I,8)+S5(I)*DN_Z(I,8))
!        F38(I)=F38(I)-(S3(I)*DN_Z(I,8)+S5(I)*DN_Y(I,8)+S6(I)*DN_X(I,8))
!        ENDDO
!
!!c      

      RETURN
      END
