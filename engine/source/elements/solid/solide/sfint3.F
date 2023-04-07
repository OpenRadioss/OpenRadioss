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
Chd|  SFINT3                        source/elements/solid/solide/sfint3.F
Chd|-- called by -----------
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|        ALEFVM_MOD                    ../common_source/modules/ale/alefvm_mod.F
Chd|        ALE_MOD                       ../common_source/modules/ale/ale_mod.F
Chd|====================================================================
      SUBROUTINE SFINT3(
     1   SIG,     PX1,     PX2,     PX3,
     2   PX4,     PY1,     PY2,     PY3,
     3   PY4,     PZ1,     PZ2,     PZ3,
     4   PZ4,     PX5,     PX6,     PX7,
     5   PX8,     PY5,     PY6,     PY7,
     6   PY8,     PZ5,     PZ6,     PZ7,
     7   PZ8,     F11,     F21,     F31,
     8   F12,     F22,     F32,     F13,
     9   F23,     F33,     F14,     F24,
     A   F34,     F15,     F25,     F35,
     B   F16,     F26,     F36,     F17,
     C   F27,     F37,     F18,     F28,
     D   F38,     VOL,     QVIS,    N1X,
     E   N2X,     N3X,     N4X,     N5X,
     F   N6X,     N1Y,     N2Y,     N3Y,
     G   N4Y,     N5Y,     N6Y,     N1Z,
     H   N2Z,     N3Z,     N4Z,     N5Z,
     I   N6Z,     DFE,     Pbak,    IXS,
     J   NEL,     NFT,     JALE,    JEUL)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
      !-------------------------------------------------!
      ! ISFINT=1 : volume integration for SIG_total     !
      ! ISFINT=2 : volume integration for SIG_dev only  !
      ! ISFINT=3 : surface integration for SIG_total    !
      !-------------------------------------------------!  
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ALEFVM_MOD , only:ALEFVM_Param
      USE ALE_MOD
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
#include      "com01_c.inc"
#include      "nsvis_c.inc"
#include      "inter22.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NFT
      INTEGER, INTENT(IN) :: JALE
      INTEGER, INTENT(IN) :: JEUL
      INTEGER IXS(NIXS,*),NEL
      my_real
     .   SIG(NEL,6),
     .   PX1(*), PX2(*), PX3(*), PX4(*),  
     .   PY1(*), PY2(*), PY3(*), PY4(*),  
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*),  
     .   PX5(*), PX6(*), PX7(*), PX8(*),  
     .   PY5(*), PY6(*), PY7(*), PY8(*),  
     .   PZ5(*), PZ6(*), PZ7(*), PZ8(*),  
     .   F11(*),F21(*),F31(*),F12(*),F22(*),F32(*),
     .   F13(*),F23(*),F33(*),F14(*),F24(*),F34(*),
     .   F15(*),F25(*),F35(*),F16(*),F26(*),F36(*),
     .   F17(*),F27(*),F37(*),F18(*),F28(*),F38(*),
     .   VOL(*),QVIS(*)
      my_real
     .   N1X(*), N2X(*), N3X(*), N4X(*), N5X(*), N6X(*),
     .   N1Y(*), N2Y(*), N3Y(*), N4Y(*), N5Y(*), N6Y(*),
     .   N1Z(*), N2Z(*), N3Z(*), N4Z(*), N5Z(*), N6Z(*),
     .   DFE(MVSIZ,3),Pbak(MVSIZ),
     .   TX,TY,TZ     
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
      my_real S1(MVSIZ), S2(MVSIZ), S3(MVSIZ),S4(MVSIZ), S5(MVSIZ), S6(MVSIZ),FINT,FINTD,QVIS_LOC,VOL_LOC,P(MVSIZ)
C-----------------------------------------------

      !-------------------------------------------------!
      ! ISFINT=3 : surface integration for SIG_total    !
      !-------------------------------------------------!  
      IF(ALE%GLOBAL%ISFINT==3 .AND. JALE+JEUL/=0)THEN
        !------------------------------!
        !    TOTAL STRESS TENSOR       ! 
        !------------------------------!  
        DO I=1,NEL                         
          QVIS_LOC = QVIS(I)                 
          S1(I)=SIG(I,1)+SVIS(I,1)-QVIS_LOC  
          S2(I)=SIG(I,2)+SVIS(I,2)-QVIS_LOC  
          S3(I)=SIG(I,3)+SVIS(I,3)-QVIS_LOC  
          S4(I)=SIG(I,4)+SVIS(I,4)           
          S5(I)=SIG(I,5)+SVIS(I,5)           
          S6(I)=SIG(I,6)+SVIS(I,6)           
        ENDDO  
        !------------------------------!
        !          PRESSURE            ! 
        !------------------------------!  
        DO I=1,NEL                         
          P(I)=THIRD*(S1(I)+S2(I)+S3(I))    
          Pbak(I)=ONE_OVER_8*P(I)                       
        ENDDO          
        !------------------------------!
        !          DIV(SIGMA)          ! 
        !------------------------------!  
        ! Integral ( div(sigma) ) dV = Integral (sigma:n) dS  
        DO I=1,NEL 
          ! node-1 : faces 1,4,6
          TX=N1X(I)+N4X(I)+N6X(I)
          TY=N1Y(I)+N4Y(I)+N6Y(I)
          TZ=N1Z(I)+N4Z(I)+N6Z(I)                    
          F11(I)=F11(I)-ONE_OVER_8*(S1(I)*TX + S4(I)*TY + S6(I)*TZ)
             !print *, "ID,FINT(11)=", IXS(11,I+NFT), -ONE_OVER_8*(S1(I)*TX + S4(I)*TY + S6(I)*TZ)
          F21(I)=F21(I)-ONE_OVER_8*(S4(I)*TX + S2(I)*TY + S5(I)*TZ)
          F31(I)=F31(I)-ONE_OVER_8*(S6(I)*TX + S5(I)*TY + S3(I)*TZ)                    
          ! node-2 : faces 1,4,5
          TX=N1X(I)+N4X(I)+N5X(I)
          TY=N1Y(I)+N4Y(I)+N5Y(I)
          TZ=N1Z(I)+N4Z(I)+N5Z(I)                    
          F12(I)=F12(I)-ONE_OVER_8*(S1(I)*TX + S4(I)*TY + S6(I)*TZ)
          F22(I)=F22(I)-ONE_OVER_8*(S4(I)*TX + S2(I)*TY + S5(I)*TZ)
          F32(I)=F32(I)-ONE_OVER_8*(S6(I)*TX + S5(I)*TY + S3(I)*TZ)          
          ! node-3 : faces 1,2,5
          TX=N1X(I)+N2X(I)+N5X(I)
          TY=N1Y(I)+N2Y(I)+N5Y(I)
          TZ=N1Z(I)+N2Z(I)+N5Z(I)                    
          F13(I)=F13(I)-ONE_OVER_8*(S1(I)*TX + S4(I)*TY + S6(I)*TZ)
          F23(I)=F23(I)-ONE_OVER_8*(S4(I)*TX + S2(I)*TY + S5(I)*TZ)
          F33(I)=F33(I)-ONE_OVER_8*(S6(I)*TX + S5(I)*TY + S3(I)*TZ)         
          ! node-4 : faces 1,2,6
          TX=N1X(I)+N2X(I)+N6X(I)
          TY=N1Y(I)+N2Y(I)+N6Y(I)
          TZ=N1Z(I)+N2Z(I)+N6Z(I)                    
          F14(I)=F14(I)-ONE_OVER_8*(S1(I)*TX + S4(I)*TY + S6(I)*TZ)
          F24(I)=F24(I)-ONE_OVER_8*(S4(I)*TX + S2(I)*TY + S5(I)*TZ)
          F34(I)=F34(I)-ONE_OVER_8*(S6(I)*TX + S5(I)*TY + S3(I)*TZ)   
          ! node-5 : faces 3,4,6
          TX=N3X(I)+N4X(I)+N6X(I)
          TY=N3Y(I)+N4Y(I)+N6Y(I)
          TZ=N3Z(I)+N4Z(I)+N6Z(I)                    
          F15(I)=F15(I)-ONE_OVER_8*(S1(I)*TX + S4(I)*TY + S6(I)*TZ)
          F25(I)=F25(I)-ONE_OVER_8*(S4(I)*TX + S2(I)*TY + S5(I)*TZ)
          F35(I)=F35(I)-ONE_OVER_8*(S6(I)*TX + S5(I)*TY + S3(I)*TZ) 
          ! node-6 : faces 3,4,5
          TX=N3X(I)+N4X(I)+N5X(I)
          TY=N3Y(I)+N4Y(I)+N5Y(I)
          TZ=N3Z(I)+N4Z(I)+N5Z(I)                    
          F16(I)=F16(I)-ONE_OVER_8*(S1(I)*TX + S4(I)*TY + S6(I)*TZ)
          F26(I)=F26(I)-ONE_OVER_8*(S4(I)*TX + S2(I)*TY + S5(I)*TZ)
          F36(I)=F36(I)-ONE_OVER_8*(S6(I)*TX + S5(I)*TY + S3(I)*TZ)                           
          ! node-7 : faces 2,3,5
          TX=N2X(I)+N3X(I)+N5X(I)
          TY=N2Y(I)+N3Y(I)+N5Y(I)
          TZ=N2Z(I)+N3Z(I)+N5Z(I)                    
          F17(I)=F17(I)-ONE_OVER_8*(S1(I)*TX + S4(I)*TY + S6(I)*TZ)
          F27(I)=F27(I)-ONE_OVER_8*(S4(I)*TX + S2(I)*TY + S5(I)*TZ)
          F37(I)=F37(I)-ONE_OVER_8*(S6(I)*TX + S5(I)*TY + S3(I)*TZ)     
          ! node-8 : faces 2,3,6
          TX=N2X(I)+N3X(I)+N6X(I)
          TY=N2Y(I)+N3Y(I)+N6Y(I)
          TZ=N2Z(I)+N3Z(I)+N6Z(I)                    
          F18(I)=F18(I)-ONE_OVER_8*(S1(I)*TX + S4(I)*TY + S6(I)*TZ)
          F28(I)=F28(I)-ONE_OVER_8*(S4(I)*TX + S2(I)*TY + S5(I)*TZ)
          F38(I)=F38(I)-ONE_OVER_8*(S6(I)*TX + S5(I)*TY + S3(I)*TZ)                              
        ENDDO  
        RETURN                          
      ENDIF

      !-------------------------------------------------!
      ! ISFINT=1 : volume integration for SIG_total     !
      ! ISFINT=2 : volume integration for SIG_dev only  !
      !-------------------------------------------------!       
      IF((ALE%GLOBAL%ICAA==1.OR.ALE%GLOBAL%ISFINT==2) .AND. JALE+JEUL/=0)THEN
        DO I=1,NEL
         QVIS_LOC = QVIS(I)
         VOL_LOC = VOL(I) 
         S1(I)=SIG(I,1)+SVIS(I,1)-QVIS_LOC
         S2(I)=SIG(I,2)+SVIS(I,2)-QVIS_LOC
         S3(I)=SIG(I,3)+SVIS(I,3)-QVIS_LOC
         P(I)=(S1(I)+S2(I)+S3(I))/THREE
         S1(I)=(S1(I)-P(I))*VOL_LOC
         S2(I)=(S2(I)-P(I))*VOL_LOC
         S3(I)=(S3(I)-P(I))*VOL_LOC
         S4(I)=(SIG(I,4)+SVIS(I,4))*VOL_LOC
         S5(I)=(SIG(I,5)+SVIS(I,5))*VOL_LOC
         S6(I)=(SIG(I,6)+SVIS(I,6))*VOL_LOC
         P(I)=P(I)*0.125 
        ENDDO
        DO I=1,NEL
          Pbak(I) = P(I)
        ENDDO
        !--------------------------------------------------------------!
        ! fint from spherical tensor : FVM                             !
        !--------------------------------------------------------------!
        DO I=1,NEL
          F11(I)=F11(I)-P(I)*(N1X(I)              +N4X(I)       +N6X(I))
          F21(I)=F21(I)-P(I)*(N1Y(I)              +N4Y(I)       +N6Y(I))
          F31(I)=F31(I)-P(I)*(N1Z(I)              +N4Z(I)       +N6Z(I))
          F12(I)=F12(I)-P(I)*(N1X(I)              +N4X(I)+N5X(I)       )
          F22(I)=F22(I)-P(I)*(N1Y(I)              +N4Y(I)+N5Y(I)       )
          F32(I)=F32(I)-P(I)*(N1Z(I)              +N4Z(I)+N5Z(I)       )
          F13(I)=F13(I)-P(I)*(N1X(I)+N2X(I)              +N5X(I)       )
          F23(I)=F23(I)-P(I)*(N1Y(I)+N2Y(I)              +N5Y(I)       )
          F33(I)=F33(I)-P(I)*(N1Z(I)+N2Z(I)              +N5Z(I)       )
          F14(I)=F14(I)-P(I)*(N1X(I)+N2X(I)                     +N6X(I))
          F24(I)=F24(I)-P(I)*(N1Y(I)+N2Y(I)                     +N6Y(I))
          F34(I)=F34(I)-P(I)*(N1Z(I)+N2Z(I)                     +N6Z(I))
          F15(I)=F15(I)-P(I)*(             +N3X(I)+N4X(I)       +N6X(I))
          F25(I)=F25(I)-P(I)*(             +N3Y(I)+N4Y(I)       +N6Y(I))
          F35(I)=F35(I)-P(I)*(             +N3Z(I)+N4Z(I)       +N6Z(I))
          F16(I)=F16(I)-P(I)*(             +N3X(I)+N4X(I)+N5X(I)       )
          F26(I)=F26(I)-P(I)*(             +N3Y(I)+N4Y(I)+N5Y(I)       )
          F36(I)=F36(I)-P(I)*(             +N3Z(I)+N4Z(I)+N5Z(I)       )
          F17(I)=F17(I)-P(I)*(      +N2X(I)+N3X(I)       +N5X(I)       )
          F27(I)=F27(I)-P(I)*(      +N2Y(I)+N3Y(I)       +N5Y(I)       )
          F37(I)=F37(I)-P(I)*(      +N2Z(I)+N3Z(I)       +N5Z(I)       )
          F18(I)=F18(I)-P(I)*(      +N2X(I)+N3X(I)              +N6X(I))
          F28(I)=F28(I)-P(I)*(      +N2Y(I)+N3Y(I)              +N6Y(I))
          F38(I)=F38(I)-P(I)*(      +N2Z(I)+N3Z(I)              +N6Z(I)) 
        ENDDO
      ELSE !IF(ICAA == 0 .OR. JALE+JEUL==0 )
        DO I=1,NEL
         QVIS_LOC = QVIS(I)
         VOL_LOC = VOL(I) 
         S1(I)=(SIG(I,1)+SVIS(I,1)-QVIS_LOC)*VOL_LOC
         S2(I)=(SIG(I,2)+SVIS(I,2)-QVIS_LOC)*VOL_LOC
         S3(I)=(SIG(I,3)+SVIS(I,3)-QVIS_LOC)*VOL_LOC
         S4(I)=(SIG(I,4)+SVIS(I,4))*VOL_LOC
         S5(I)=(SIG(I,5)+SVIS(I,5))*VOL_LOC
         S6(I)=(SIG(I,6)+SVIS(I,6))*VOL_LOC 
        ENDDO
        IF(ALEFVM_Param%IEnabled/=0)THEN
          DO I=1,NEL
            Pbak(I) =0.125 * (S1(I)+S2(I)+S3(I))/THREE/VOL(I)
          ENDDO
        ENDIF
      ENDIF      
      IF(INT22==0)THEN !no yet deviatoric stress contribution with inter22 to simplify (priority : ditching)
        !--------------------------------------------------------------!
        ! FEM internal forces                                          !
        !  /CAA or /INTER/TYPE22 : deviatoric stress tensor            !
        !  otherwise             : total stress tensor                 !      
        !--------------------------------------------------------------!
        IF(JEUL==0 .OR. (JEUL==1.AND.INTEG8==0))THEN
         DO I=1,NEL
          !shape functions hypothesis : phi1=-phi7, phi2=-phi8, phi3=-phi5, phi4=-phi6
          !---nodes 1-7 ---!
          FINT=S1(I)*PX1(I)+S4(I)*PY1(I)+S6(I)*PZ1(I)
          F11(I)=F11(I)-FINT
          F17(I)=F17(I)+FINT
          FINT=S2(I)*PY1(I)+S4(I)*PX1(I)+S5(I)*PZ1(I)
          F21(I)=F21(I)-FINT
          F27(I)=F27(I)+FINT
          FINT=S3(I)*PZ1(I)+S6(I)*PX1(I)+S5(I)*PY1(I)
          F31(I)=F31(I)-FINT
          F37(I)=F37(I)+FINT
          !---nodes 2-8 ---!          
          FINT=S1(I)*PX2(I)+S4(I)*PY2(I)+S6(I)*PZ2(I)
          F12(I)=F12(I)-FINT
          F18(I)=F18(I)+FINT
          FINT=S2(I)*PY2(I)+S4(I)*PX2(I)+S5(I)*PZ2(I)
          F22(I)=F22(I)-FINT
          F28(I)=F28(I)+FINT
          FINT=S3(I)*PZ2(I)+S6(I)*PX2(I)+S5(I)*PY2(I)
          F32(I)=F32(I)-FINT
          F38(I)=F38(I)+FINT
          !---nodes 3-5 ---!
          FINT=S1(I)*PX3(I)+S4(I)*PY3(I)+S6(I)*PZ3(I)
          F13(I)=F13(I)-FINT
          F15(I)=F15(I)+FINT
          FINT=S2(I)*PY3(I)+S4(I)*PX3(I)+S5(I)*PZ3(I)
          F23(I)=F23(I)-FINT
          F25(I)=F25(I)+FINT
          FINT=S3(I)*PZ3(I)+S6(I)*PX3(I)+S5(I)*PY3(I)
          F33(I)=F33(I)-FINT
          F35(I)=F35(I)+FINT
          !---nodes 4-6 ---!
          FINT=S1(I)*PX4(I)+S4(I)*PY4(I)+S6(I)*PZ4(I)
          F14(I)=F14(I)-FINT
          F16(I)=F16(I)+FINT
          FINT=S2(I)*PY4(I)+S4(I)*PX4(I)+S5(I)*PZ4(I)
          F24(I)=F24(I)-FINT
          F26(I)=F26(I)+FINT
          FINT=S3(I)*PZ4(I)+S6(I)*PX4(I)+S5(I)*PY4(I)
          F34(I)=F34(I)-FINT
          F36(I)=F36(I)+FINT
         ENDDO
        ELSE
        !--------------------------------------------------------------!
        ! FEM internal forces  + INTEG8                                !
        !  /CAA or /INTER/TYPE22 : deviatoric stress tensor            !
        !  otherwise             : total stress tensor                 !   
        !--------------------------------------------------------------!
        !shape functions are different with hidden flag INTEG8 in /ANALY control card     
         DO I=1,NEL
          F11(I)=F11(I)-(S1(I)*PX1(I)+S4(I)*PY1(I)+S6(I)*PZ1(I))
          F21(I)=F21(I)-(S2(I)*PY1(I)+S4(I)*PX1(I)+S5(I)*PZ1(I))
          F31(I)=F31(I)-(S3(I)*PZ1(I)+S6(I)*PX1(I)+S5(I)*PY1(I))
          F12(I)=F12(I)-(S1(I)*PX2(I)+S4(I)*PY2(I)+S6(I)*PZ2(I))
          F22(I)=F22(I)-(S2(I)*PY2(I)+S4(I)*PX2(I)+S5(I)*PZ2(I))
          F32(I)=F32(I)-(S3(I)*PZ2(I)+S6(I)*PX2(I)+S5(I)*PY2(I))
          F13(I)=F13(I)-(S1(I)*PX3(I)+S4(I)*PY3(I)+S6(I)*PZ3(I))
          F23(I)=F23(I)-(S2(I)*PY3(I)+S4(I)*PX3(I)+S5(I)*PZ3(I))
          F33(I)=F33(I)-(S3(I)*PZ3(I)+S6(I)*PX3(I)+S5(I)*PY3(I))
          F14(I)=F14(I)-(S1(I)*PX4(I)+S4(I)*PY4(I)+S6(I)*PZ4(I))
          F24(I)=F24(I)-(S2(I)*PY4(I)+S4(I)*PX4(I)+S5(I)*PZ4(I))
          F34(I)=F34(I)-(S3(I)*PZ4(I)+S6(I)*PX4(I)+S5(I)*PY4(I))
          F15(I)=F15(I)-(S1(I)*PX5(I)+S4(I)*PY5(I)+S6(I)*PZ5(I))
          F25(I)=F25(I)-(S2(I)*PY5(I)+S4(I)*PX5(I)+S5(I)*PZ5(I))
          F35(I)=F35(I)-(S3(I)*PZ5(I)+S6(I)*PX5(I)+S5(I)*PY5(I))
          F16(I)=F16(I)-(S1(I)*PX6(I)+S4(I)*PY6(I)+S6(I)*PZ6(I))
          F26(I)=F26(I)-(S2(I)*PY6(I)+S4(I)*PX6(I)+S5(I)*PZ6(I))
          F36(I)=F36(I)-(S3(I)*PZ6(I)+S6(I)*PX6(I)+S5(I)*PY6(I))
          F17(I)=F17(I)-(S1(I)*PX7(I)+S4(I)*PY7(I)+S6(I)*PZ7(I))
          F27(I)=F27(I)-(S2(I)*PY7(I)+S4(I)*PX7(I)+S5(I)*PZ7(I))
          F37(I)=F37(I)-(S3(I)*PZ7(I)+S6(I)*PX7(I)+S5(I)*PY7(I))
          F18(I)=F18(I)-(S1(I)*PX8(I)+S4(I)*PY8(I)+S6(I)*PZ8(I))
          F28(I)=F28(I)-(S2(I)*PY8(I)+S4(I)*PX8(I)+S5(I)*PZ8(I))
          F38(I)=F38(I)-(S3(I)*PZ8(I)+S6(I)*PX8(I)+S5(I)*PY8(I))
         ENDDO
        ENDIF
      ENDIF  
      
      RETURN
      END
