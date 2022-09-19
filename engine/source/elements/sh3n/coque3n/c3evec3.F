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
Chd|  C3EVEC3                       source/elements/sh3n/coque3n/c3evec3.F
Chd|-- called by -----------
Chd|        C3FORC3                       source/elements/sh3n/coque3n/c3forc3.F
Chd|        C3FORC3_CRK                   source/elements/xfem/c3forc3_crk.F
Chd|-- calls ---------------
Chd|        CLSKEW3                       source/elements/sh3n/coquedk/cdkcoor3.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE C3EVEC3(ELBUF_STR,DIR_A    ,DIR_B   ,JFT      ,JLT      ,
     .                   IREP     ,E1X0     ,E1Y0    ,E1Z0     ,E2X0     ,
     .                   E2Y0     ,E2Z0     ,E3X0    ,E3Y0     ,E3Z0     ,
     .                   E1X      ,E1Y      ,E1Z     ,E2X      ,
     .                   E2Y      ,E2Z      ,E3X     ,E3Y      ,E3Z      ,
     .                   NLAY     ,OFFG     ,ECOS    ,ESIN     ,ISH3NFRAM,
     .                   NEL      ,AREA     ,X21     ,Y21      ,Z21      ,
     .                   X31      ,Y31      ,Z31     ,
     .                   X1       ,X2       ,X3      ,Y1       ,Y2       ,
     .                   Y3       ,Z1       ,Z2      ,Z3       )
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
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT,IREP,NLAY,ISH3NFRAM,NEL
      my_real
     .   DIR_A(NEL,*), DIR_B(NEL,*),E1X0(*), E1Y0(*), E1Z0(*),
     .   E2X0(*), E2Y0(*), E2Z0(*),E3X0(*), E3Y0(*), E3Z0(*),OFFG(*),
     .   E1X(MVSIZ), E1Y(MVSIZ), E1Z(MVSIZ),
     .   E2X(MVSIZ), E2Y(MVSIZ), E2Z(MVSIZ),
     .   E3X(MVSIZ), E3Y(MVSIZ), E3Z(MVSIZ),
     .   X21(MVSIZ), Y21(MVSIZ), Z21(MVSIZ),
     .   X31(MVSIZ), Y31(MVSIZ), Z31(MVSIZ),
     .   ECOS(*) ,ESIN(*), AREA(MVSIZ)
!       SP issue :
      REAL(kind=8), DIMENSION(MVSIZ), INTENT(in) ::X1,X2,X3
      REAL(kind=8), DIMENSION(MVSIZ), INTENT(in) ::Y1,Y2,Y3
      REAL(kind=8), DIMENSION(MVSIZ), INTENT(in) ::Z1,Z2,Z3


      TYPE (ELBUF_STRUCT_) :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,J,N,ILAW
C     REAL
      my_real X32(MVSIZ), Y32(MVSIZ), Z32(MVSIZ),SUM(MVSIZ),
     .     E11(MVSIZ),E12(MVSIZ),E13(MVSIZ),
     .     E21(MVSIZ),E22(MVSIZ),E23(MVSIZ)
      my_real V1,V2,V3,VR,VS,AA,BB,SUMA
      my_real, DIMENSION(:) ,POINTER :: DIR1, DIR2
C=======================================================================
      DO I=JFT,JLT
        X21(I)=X2(I)-X1(I)
        Y21(I)=Y2(I)-Y1(I)
        Z21(I)=Z2(I)-Z1(I)
        X31(I)=X3(I)-X1(I)
        Y31(I)=Y3(I)-Y1(I)
        Z31(I)=Z3(I)-Z1(I)
        X32(I)=X3(I)-X2(I)
        Y32(I)=Y3(I)-Y2(I)
        Z32(I)=Z3(I)-Z2(I)
      ENDDO
C
      IF (IREP > 0) THEN
        DO I=JFT,JLT
          E11(I) = X21(I)
          E12(I) = Y21(I)
          E13(I) = Z21(I)
          E21(I) = X31(I)
          E22(I) = Y31(I)
          E23(I) = Z31(I)
        ENDDO
      ENDIF
C
      DO I=JFT,JLT
        E1X(I)= X21(I)
        E1Y(I)= Y21(I)
        E1Z(I)= Z21(I)
        SUM(I) = SQRT(E1X(I)*E1X(I)+E1Y(I)*E1Y(I)+E1Z(I)*E1Z(I))
        E1X(I)=E1X(I)/SUM(I)
        E1Y(I)=E1Y(I)/SUM(I)
        E1Z(I)=E1Z(I)/SUM(I)
      ENDDO
C
      DO I=JFT,JLT
        E3X(I)=Y31(I)*Z32(I)-Z31(I)*Y32(I)
        E3Y(I)=Z31(I)*X32(I)-X31(I)*Z32(I)
        E3Z(I)=X31(I)*Y32(I)-Y31(I)*X32(I)
        SUM(I) = SQRT(E3X(I)*E3X(I)+E3Y(I)*E3Y(I)+E3Z(I)*E3Z(I))
        E3X(I)=E3X(I)/SUM(I)
        E3Y(I)=E3Y(I)/SUM(I)
        E3Z(I)=E3Z(I)/SUM(I)
        AREA(I) = HALF * SUM(I)
      ENDDO
C
      DO I=JFT,JLT
        E2X(I)=E3Y(I)*E1Z(I)-E3Z(I)*E1Y(I)
        E2Y(I)=E3Z(I)*E1X(I)-E3X(I)*E1Z(I)
        E2Z(I)=E3X(I)*E1Y(I)-E3Y(I)*E1X(I)
        SUM(I) = SQRT(E2X(I)*E2X(I)+E2Y(I)*E2Y(I)+E2Z(I)*E2Z(I))
        E2X(I)=E2X(I)/SUM(I)
        E2Y(I)=E2Y(I)/SUM(I)
        E2Z(I)=E2Z(I)/SUM(I)
      ENDDO
C---
      DO I=JFT,JLT
        E1X0(I) = E1X(I)
        E1Y0(I) = E1Y(I)
        E1Z0(I) = E1Z(I)
        E2X0(I) = E2X(I)
        E2Y0(I) = E2Y(I)
        E2Z0(I) = E2Z(I)
        E3X0(I) = E3X(I)
        E3Y0(I) = E3Y(I)
        E3Z0(I) = E3Z(I)
      ENDDO
      IF(ISH3NFRAM ==0 ) THEN
       II = 0
       CALL CLSKEW3(JFT,JLT,II,
     .   X21, Y21, Z21,
     .   X31, Y31, Z31,
     .   E1X,E2X,E3X,E1Y,E2Y,E3Y,E1Z,E2Z,E3Z,SUM,OFFG)
       DO I=JFT,JLT
        ECOS(I) = E1X(I)*E1X0(I)+E1Y(I)*E1Y0(I)+E1Z(I)*E1Z0(I)
        AA = MAX(ZERO,ONE-ECOS(I)*ECOS(I))
        ESIN(I) = SQRT(AA)
        BB = E2X(I)*E1X0(I)+E2Y(I)*E1Y0(I)+E2Z(I)*E1Z0(I)
        IF (BB >ZERO) ESIN(I) = -ESIN(I)
       ENDDO
      END IF
C-----------------------------------------
      IF (IREP == 1) THEN
        DO N=1,NLAY
          DIR1 => ELBUF_STR%BUFLY(N)%DIRA
!          J = (N-1)*NEL
          J = 2*(N-1)
          DO I=JFT,JLT
            AA = DIR1(I)
            BB = DIR1(I + NEL)
            V1 = AA*E11(I) + BB*E21(I)
            V2 = AA*E12(I) + BB*E22(I)
            V3 = AA*E13(I) + BB*E23(I)
            VR = V1*E1X(I) + V2*E1Y(I) + V3*E1Z(I)
            VS = V1*E2X(I) + V2*E2Y(I) + V3*E2Z(I)
            SUMA=SQRT(VR*VR + VS*VS)
            DIR_A(I,J+1) = VR/SUMA
            DIR_A(I,J+2) = VS/SUMA
          ENDDO
        ENDDO
      ELSEIF (IREP == 2) THEN
        DO N=1,NLAY
          DIR1 => ELBUF_STR%BUFLY(N)%DIRA
          DIR2 => ELBUF_STR%BUFLY(N)%DIRB
!          J = (N-1)*NEL
          J = 2*(N-1)
          DO I=JFT,JLT
C---        Axe I
            AA = DIR1(I)
            BB = DIR1(I + NEL)
            V1 = AA*E11(I) + BB*E21(I)
            V2 = AA*E12(I) + BB*E22(I)
            V3 = AA*E13(I) + BB*E23(I)
            VR = V1*E1X(I) + V2*E1Y(I) + V3*E1Z(I)
            VS = V1*E2X(I) + V2*E2Y(I) + V3*E2Z(I)
            SUMA = ONE / MAX( SQRT(VR*VR + VS*VS), EM20)
            DIR_A(I,J+1) = VR*SUMA
            DIR_A(I,J+2) = VS*SUMA
C---        Axe II
            AA = DIR2(I)
            BB = DIR2(I + NEL)
            V1 = AA*E11(I) + BB*E21(I)
            V2 = AA*E12(I) + BB*E22(I)
            V3 = AA*E13(I) + BB*E23(I)
            VR = V1*E1X(I) + V2*E1Y(I) + V3*E1Z(I)
            VS = V1*E2X(I) + V2*E2Y(I) + V3*E2Z(I)
            SUMA = ONE / MAX( SQRT(VR*VR + VS*VS), EM20)
            DIR_B(I,J+1) = VR*SUMA
            DIR_B(I,J+2) = VS*SUMA
          ENDDO
        ENDDO
      ELSEIF (IREP == 3) THEN
C   mixing law58 with other user laws with IREP = 0 within PID51
        DO N=1,NLAY
          ILAW = ELBUF_STR%BUFLY(N)%ILAW
!          J = (N-1)*NEL
          J = 2*(N-1)
          IF (ILAW == 58) THEN
            DIR1 => ELBUF_STR%BUFLY(N)%DIRA
            DIR2 => ELBUF_STR%BUFLY(N)%DIRB
            DO I=JFT,JLT
C---        Axe I                                   
              AA = DIR1(I)
              BB = DIR1(I + NEL)
              V1 = AA*E11(I) + BB*E21(I)		
              V2 = AA*E12(I) + BB*E22(I)		
              V3 = AA*E13(I) + BB*E23(I)		
              VR = V1*E1X(I)+ V2*E1Y(I) + V3*E1Z(I)
              VS = V1*E2X(I)+ V2*E2Y(I) + V3*E2Z(I)
              SUMA = ONE / MAX( SQRT(VR*VR + VS*VS), EM20)
              DIR_A(I,J+1) = VR*SUMA
              DIR_A(I,J+2) = VS*SUMA
C---        Axe II                                  
              AA = DIR2(I)
              BB = DIR2(I + NEL)
              V1 = AA*E11(I) + BB*E21(I)		
              V2 = AA*E12(I) + BB*E22(I)		
              V3 = AA*E13(I) + BB*E23(I)		
              VR = V1*E1X(I)+ V2*E1Y(I) + V3*E1Z(I)
              VS = V1*E2X(I)+ V2*E2Y(I) + V3*E2Z(I)
              SUMA = ONE / MAX( SQRT(VR*VR + VS*VS), EM20)
              DIR_B(I,J+1) = VR*SUMA
              DIR_B(I,J+2) = VS*SUMA
            ENDDO
          ELSE  ! IREP = 0 within PID51
            DIR1 => ELBUF_STR%BUFLY(N)%DIRA
            DO I=JFT,JLT
              DIR_A(I,J+1) = DIR1(I)
              DIR_A(I,J+2) = DIR1(I+NEL)
            ENDDO
          ENDIF
        ENDDO
      ELSEIF (IREP == 4) THEN
C   mixing law58 with other user laws with IREP = 1 within PID51
        DO N=1,NLAY
          ILAW = ELBUF_STR%BUFLY(N)%ILAW
!          J = (N-1)*NEL
          J = 2*(N-1)
          IF (ILAW == 58) THEN
            DIR1 => ELBUF_STR%BUFLY(N)%DIRA
            DIR2 => ELBUF_STR%BUFLY(N)%DIRB
            DO I=JFT,JLT
C---        Axe I                                   
              AA = DIR1(I)
              BB = DIR1(I + NEL)
              V1 = AA*E11(I) + BB*E21(I)		
              V2 = AA*E12(I) + BB*E22(I)		
              V3 = AA*E13(I) + BB*E23(I)		
              VR = V1*E1X(I)+ V2*E1Y(I) + V3*E1Z(I)
              VS = V1*E2X(I)+ V2*E2Y(I) + V3*E2Z(I)
              SUMA = ONE / MAX( SQRT(VR*VR + VS*VS), EM20)
              DIR_A(I,J+1) = VR*SUMA
              DIR_A(I,J+2) = VS*SUMA
C---        Axe II                                  
              AA = DIR2(I)
              BB = DIR2(I + NEL)
              V1 = AA*E11(I) + BB*E21(I)		
              V2 = AA*E12(I) + BB*E22(I)		
              V3 = AA*E13(I) + BB*E23(I)		
              VR = V1*E1X(I)+ V2*E1Y(I) + V3*E1Z(I)
              VS = V1*E2X(I)+ V2*E2Y(I) + V3*E2Z(I)
              SUMA = ONE / MAX( SQRT(VR*VR + VS*VS), EM20)
              DIR_B(I,J+1) = VR*SUMA
              DIR_B(I,J+2) = VS*SUMA
            ENDDO
          ELSE  ! IREP = 1 within PID51
            DIR1 => ELBUF_STR%BUFLY(N)%DIRA
            DO I=JFT,JLT
              AA = DIR1(I)
              BB = DIR1(I + NEL)
              V1 = AA*E11(I) + BB*E21(I)		
              V2 = AA*E12(I) + BB*E22(I)		
              V3 = AA*E13(I) + BB*E23(I)		
              VR = V1*E1X(I)+ V2*E1Y(I) + V3*E1Z(I)
              VS = V1*E2X(I)+ V2*E2Y(I) + V3*E2Z(I)
              SUMA=SQRT(VR*VR + VS*VS)
              DIR_A(I,J+1) = VR/SUMA
              DIR_A(I,J+2) = VS/SUMA
            ENDDO
          ENDIF
        ENDDO
      ENDIF
C------ restore old local sys compute cos, sin relative to old      
      IF(ISH3NFRAM ==0 ) THEN
       DO I=JFT,JLT
        E11(I) = E1X(I)
        E12(I) = E1Y(I)
        E13(I) = E1Z(I)
        E21(I) = E2X(I)
        E22(I) = E2Y(I)
        E23(I) = E2Z(I)
       ENDDO
       DO I=JFT,JLT
        E1X(I) = E1X0(I)
        E1Y(I) = E1Y0(I)
        E1Z(I) = E1Z0(I)
        E2X(I) = E2X0(I)
        E2Y(I) = E2Y0(I)
        E2Z(I) = E2Z0(I)
        E3X(I) = E3X0(I)
        E3Y(I) = E3Y0(I)
        E3Z(I) = E3Z0(I)
       ENDDO
C-------------Ej0 is used in mulawc for user'laws (99)       
       DO I=JFT,JLT
        E1X0(I) = E11(I)
        E1Y0(I) = E12(I)
        E1Z0(I) = E13(I)
        E2X0(I) = E21(I)
        E2Y0(I) = E22(I)
        E2Z0(I) = E23(I)
       ENDDO
      END IF
c-----------
      RETURN
      END
Chd|====================================================================
Chd|  C3NEWVE3                      source/elements/sh3n/coque3n/c3evec3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE C3NEWVE3(JFT  ,JLT  ,TG3  ,ECOS   ,ESIN   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT
      my_real
     .   ECOS(MVSIZ),ESIN(MVSIZ),TG3(MVSIZ)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II
C     REAL
      my_real
     .   THETA
C=======================================================================
      DO I=JFT,JLT
        THETA  = HALF*(ATAN(TG3(I)))
        ECOS(I)= COS(THETA)
        ESIN(I)= SIN(THETA)
      ENDDO
c-----------
      RETURN
      END
Chd|====================================================================
Chd|  SHROTO3                       source/elements/sh3n/coque3n/c3evec3.F
Chd|-- called by -----------
Chd|        C3FORC3                       source/elements/sh3n/coque3n/c3forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SHROTO3(JFT,JLT,ECOS,ESIN,EXX,
     .                     EYY,EXY,EXZ,EYZ,KXX,
     .                     KYY,KXY)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT
C     REAL
      my_real
     .   EXX(MVSIZ), EYY(MVSIZ), EXY(MVSIZ), EXZ(MVSIZ), EYZ(MVSIZ),
     .   KXX(MVSIZ), KYY(MVSIZ), KXY(MVSIZ),ECOS(MVSIZ),ESIN(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
C     REAL
      my_real
     .  M2(MVSIZ),N2(MVSIZ),MN(MVSIZ),CM(4),MN2(MVSIZ),T1,TXY,TYX
C-----------------------------------------------
      DO I=JFT,JLT
       M2(I)= ECOS(I)*ECOS(I)
       N2(I)= ESIN(I)*ESIN(I)
       MN(I)= ECOS(I)*ESIN(I)
       MN2(I)= TWO*MN(I)
      ENDDO
C------Exy :2*exy      
      DO I=JFT,JLT
       T1 = EXY(I)*MN(I)
       CM(1)=M2(I)*EXX(I)+N2(I)*EYY(I)-T1
       CM(2)=N2(I)*EXX(I)+M2(I)*EYY(I)+T1
       CM(3)=(EXX(I)-EYY(I))*MN2(I)+EXY(I)*(M2(I)-N2(I))
       EXX(I)=CM(1)
       EYY(I)=CM(2)
       EXY(I)=CM(3)
      ENDDO
      DO I=JFT,JLT
        T1 = KXY(I)*MN(I)
        CM(1)=M2(I)*KXX(I)+N2(I)*KYY(I)-T1
        CM(2)=N2(I)*KXX(I)+M2(I)*KYY(I)+T1
        CM(3)=(KXX(I)-KYY(I))*MN2(I)+KXY(I)*(M2(I)-N2(I))
        KXX(I)=CM(1)
        KYY(I)=CM(2)
        KXY(I)=CM(3)
      ENDDO
C
      DO I=JFT,JLT
       CM(1)= EXZ(I)*ECOS(I) - EYZ(I)*ESIN(I)
       CM(2)= EXZ(I)*ESIN(I) + EYZ(I)*ECOS(I)
       EXZ(I) = CM(1)
       EYZ(I) = CM(2)
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  SHTROTO3                      source/elements/sh3n/coque3n/c3evec3.F
Chd|-- called by -----------
Chd|        C3FORC3                       source/elements/sh3n/coque3n/c3forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SHTROTO3(JFT,JLT,ECOS,ESIN,GSTR,
     .                    F_DEF,ISMSTR,NEL)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT,ISMSTR,NEL
C     REAL
      my_real
     .   GSTR(NEL,8), F_DEF(MVSIZ,8),ECOS(MVSIZ),ESIN(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
C     REAL
      my_real
     .  M2(MVSIZ),N2(MVSIZ),MN(MVSIZ),CM(4),MN2(MVSIZ),T1,T2,T3
C-----------------------------------------------
      DO I=JFT,JLT
       M2(I)= ECOS(I)*ECOS(I)
       N2(I)= ESIN(I)*ESIN(I)
       MN(I)= ECOS(I)*ESIN(I)
       MN2(I)= TWO*MN(I)
      ENDDO
C------Exy :2*exy 
      IF (ISMSTR==10) THEN
       DO I=JFT,JLT
        T1 = (F_DEF(I,3)+F_DEF(I,4))*MN(I)
        CM(1)=M2(I)*F_DEF(I,1)+N2(I)*F_DEF(I,2)-T1
        CM(2)=N2(I)*F_DEF(I,1)+M2(I)*F_DEF(I,2)+T1
        T2 = (F_DEF(I,1)-F_DEF(I,2))*MN2(I)
        CM(3)=T2+F_DEF(I,3)*M2(I)-F_DEF(I,4)*N2(I)
        CM(4)=T2+F_DEF(I,4)*M2(I)-F_DEF(I,3)*N2(I)
        F_DEF(I,1)=CM(1)
        F_DEF(I,2)=CM(2)
        F_DEF(I,3)=CM(3)
        F_DEF(I,4)=CM(4)
       ENDDO
      ELSE
       DO I=JFT,JLT
        T1 = GSTR(I,3)*MN(I)
        CM(1)=M2(I)*GSTR(I,1)+N2(I)*GSTR(I,2)-T1
        CM(2)=N2(I)*GSTR(I,1)+M2(I)*GSTR(I,2)+T1
        CM(3)=(GSTR(I,1)-GSTR(I,2))*MN2(I)+GSTR(I,3)*(M2(I)-N2(I))
        GSTR(I,1)=CM(1)
        GSTR(I,2)=CM(2)
        GSTR(I,3)=CM(3)
       ENDDO
      END IF
C
      RETURN
      END
Chd|====================================================================
Chd|  C3SROTO3                      source/elements/sh3n/coque3n/c3evec3.F
Chd|-- called by -----------
Chd|        C3FORC3                       source/elements/sh3n/coque3n/c3forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE C3SROTO3(JFT  ,JLT  ,ECOS ,ESIN      ,FOR,
     .                    MOM  ,NFOR ,NMOM ,IFRAM_OLD ,NEL)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT,IFRAM_OLD,NEL
C     REAL
      my_real
     .   FOR(NEL,5), MOM(NEL,3),NFOR(NEL,5), NMOM(NEL,3),
     .   ECOS(MVSIZ),ESIN(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
C     REAL
      my_real
     .  M2(MVSIZ),N2(MVSIZ),MN(MVSIZ),CM(5),MN2(MVSIZ),T1,T2,T3
C-----------------------------------------------
      IF(IFRAM_OLD ==0 ) THEN
       DO I=JFT,JLT
        M2(I)= ECOS(I)*ECOS(I)
        N2(I)= ESIN(I)*ESIN(I)
        MN(I)= ECOS(I)*ESIN(I)
        MN2(I)= TWO*MN(I)
       ENDDO
C------
       DO I=JFT,JLT
        T1 = FOR(I,3)*MN2(I)
        NFOR(I,1)=M2(I)*FOR(I,1)+N2(I)*FOR(I,2)+T1
        NFOR(I,2)=N2(I)*FOR(I,1)+M2(I)*FOR(I,2)-T1
        NFOR(I,3)=(-FOR(I,1)+FOR(I,2))*MN(I)+FOR(I,3)*(M2(I)-N2(I))
        NFOR(I,4)=ECOS(I)*FOR(I,4)-ESIN(I)*FOR(I,5)
        NFOR(I,5)=ECOS(I)*FOR(I,5)+ESIN(I)*FOR(I,4)
       ENDDO
       DO I=JFT,JLT
        T1 = MOM(I,3)*MN2(I)
        NMOM(I,1)=M2(I)*MOM(I,1)+N2(I)*MOM(I,2)+T1
        NMOM(I,2)=N2(I)*MOM(I,1)+M2(I)*MOM(I,2)-T1
        NMOM(I,3)=(-MOM(I,1)+MOM(I,2))*MN(I)+MOM(I,3)*(M2(I)-N2(I))
       ENDDO
      ELSE
       DO I=JFT,JLT
        NFOR(I,1) = FOR(I,1)
        NFOR(I,2) = FOR(I,2)
        NFOR(I,3) = FOR(I,3)
        NFOR(I,4) = FOR(I,4)
        NFOR(I,5) = FOR(I,5)
        NMOM(I,1) = MOM(I,1)
        NMOM(I,2) = MOM(I,2)
        NMOM(I,3) = MOM(I,3)
       ENDDO
      END IF
C
      RETURN
      END
