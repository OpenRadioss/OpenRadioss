Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    m4law   ../engine/source/materials/mat/mat004/m4law.F
!||--- called by ------------------------------------------------------
!||    mmain   ../engine/source/materials/mat_share/mmain.F90
!||====================================================================
      SUBROUTINE M4LAW(
     1   PM,      OFF,     SIG,     EPXE,
     2   MAT,     SSP,     VOL,     D1,      
     3   D2,      D3,      D4,      D5,
     4   D6,      RHO0,    DPDM,    EPD,
     5   IPLA,    SIGY,    DEFP,    DPLA,
     6   EPSP,    TSTAR,   TEMPEL,  NEL,
     7   JTHE,    FHEAT ,  JLAG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: JTHE
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: IPLA
      INTEGER, INTENT(IN) :: MAT(NEL)
      INTEGER, INTENT(IN) :: JLAG
      my_real, INTENT(IN) :: PM(NPROPM,*)
      my_real, INTENT(IN) :: VOL(NEL)
      my_real :: SIG(NEL,6)
      my_real ,DIMENSION(NEL) :: OFF,EPXE,SSP,D1,D2,D3,D4,D5,D6,RHO0,DPDM,EPD,SIGY,DEFP,DPLA,EPSP
      my_real ,DIMENSION(NEL) ,INTENT(IN)    :: TSTAR
      my_real ,DIMENSION(NEl) ,INTENT(INOUT) :: TEMPEL
      my_real, DIMENSION(NEL) ,INTENT(INOUT) :: FHEAT ! Heat due to plastic work for Heat Equation with lagrangian framework
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,MX
      my_real :: RHOCP,TMAX,CT, CE, CH
      my_real G(NEL)    ,G1(NEL)   ,G2(NEL)  ,QS(NEL) ,AK(NEL),
     .        QH(NEL)   ,TMELT(NEL),AJ2(NEL) ,DAV(NEL),P(NEL) ,
     .        EPMX(NEL) ,CA(NEL)   ,CB(NEL)  ,CC(NEL) ,
     .        CN(NEL)   ,EPXO(NEL) ,EPDR(NEL),CMX(NEL),SIGMX(NEL),
     .        SCALE(NEL),T0(NEL)
C-----------------------------------------------
C   B o d y
C-----------------------------------------------
      MX = MAT(1)
      TMAX  = PM(47,MX)
      RHOCP =  PM(69,MX)  
      DO I=1,NEL
        G(I)    =PM(22,MX)
        CA(I)   =PM(38,MX)
        CB(I)   =PM(39,MX)
        CN(I)   =PM(40,MX)
        EPMX(I) =PM(41,MX)
        SIGMX(I)=PM(42,MX)
        CC(I)   =PM(43,MX)
        EPDR(I) =PM(44,MX)
        CMX(I)  =PM(45,MX)
        TMELT(I)=PM(46,MX)
        T0(I)   =PM(79,MX)
      ENDDO
C
      DO I=1,NEL
        P(I)  =-THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3))
        DAV(I)=-THIRD*(D1(I)+D2(I)+D3(I))
        G1(I)=DT1*G(I)*OFF(I)
        G2(I)=TWO*G1(I)*OFF(I)
      ENDDO
C-----------------------
C     SOUND SPEED
C-----------------------
      DO I=1,NEL
        DPDM(I) = DPDM(I) + ONEP333*G(I)
        SSP(I)=SQRT(ABS(DPDM(I))/RHO0(I))
      ENDDO
C-------------------------------
C     CONTRAINTES DEVIATORIQUES
C-------------------------------
      DO I=1,NEL
        SIG(I,1)=SIG(I,1)+P(I)+G2(I)*(D1(I)+DAV(I))
        SIG(I,2)=SIG(I,2)+P(I)+G2(I)*(D2(I)+DAV(I))
        SIG(I,3)=SIG(I,3)+P(I)+G2(I)*(D3(I)+DAV(I))
        SIG(I,4)=SIG(I,4)+G1(I)*D4(I)
        SIG(I,5)=SIG(I,5)+G1(I)*D5(I)
        SIG(I,6)=SIG(I,6)+G1(I)*D6(I)
      ENDDO
C
      DO I=1,NEL
        EPD(I)=OFF(I)*MAX(   ABS(D1(I)),   ABS(D2(I)),   ABS(D3(I)),
     .         HALF*ABS(D4(I)),HALF*ABS(D5(I)),HALF*ABS(D6(I)))
C
        EPSP(I) = EPD(I)     
        AJ2(I)=HALF*(SIG(I,1)**2+SIG(I,2)**2+SIG(I,3)**2)
     .                +SIG(I,4)**2+SIG(I,5)**2+SIG(I,6)**2
        AJ2(I)=SQRT(THREE*AJ2(I))
      ENDDO
C-------------------------------
C     CRITERE
C-------------------------------
      DO I=1,NEL
       CT = ONE
       IF (TEMPEL(I) >= TMELT(I)) THEN
        AJ2(I)=ZERO
        QH(I)=ZERO
        AK(I)=ZERO
        SCALE(I)=ZERO
        CYCLE
       ELSEIF (TEMPEL(I) > T0(I)) THEN
         IF (TEMPEL(I) > TMAX) CMX(I)=ONE
         CT = ONE - TSTAR(I)**CMX(I)
       ENDIF
C
       IF(EPD(I)<=EPDR(I)) THEN
        CE=ONE
       ELSE
        CE=ONE + CC(I) * LOG(EPD(I)/EPDR(I))
       ENDIF
C
       IF(EPXE(I)<=ZERO) THEN
        CH=CA(I)
       ELSEIF(EPXE(I)>EPMX(I)) THEN
        CH=ZERO
       ELSE
        CH=CA(I)+CB(I)*EPXE(I)**CN(I)
       ENDIF
C
       AK(I) = MIN(SIGMX(I),CH)*CE*CT
C-----------------------
C     MODULE ECROUISSAGE
C-----------------------
       IF(CN(I)>=ONE) THEN
        QH(I)= (CB(I)*CN(I)*EPXE(I)**(CN(I) - ONE))*CE*CT
       ELSEIF(EPXE(I)>ZERO)THEN
        QH(I)= (CB(I)*CN(I)/EPXE(I)**(ONE -CN(I)))*CE*CT
       ELSE
        QH(I)=ZERO
       ENDIF
C-------------------------------
C     PROJECTION
C-------------------------------
       IF(AJ2(I)<=AK(I)) THEN
        SCALE(I)=1.
       ELSEIF(AJ2(I)/=ZERO) THEN
        SCALE(I)=AK(I)/AJ2(I)
       ELSE
        SCALE(I)=ZERO
       ENDIF
      ENDDO
C
      IF(IPLA==0)THEN
       DO I=1,NEL
       SIG(I,1)=SCALE(I)*SIG(I,1)
       SIG(I,2)=SCALE(I)*SIG(I,2)
       SIG(I,3)=SCALE(I)*SIG(I,3)
       SIG(I,4)=SCALE(I)*SIG(I,4)
       SIG(I,5)=SCALE(I)*SIG(I,5)
       SIG(I,6)=SCALE(I)*SIG(I,6)
       DPLA(I) =(ONE -SCALE(I))*AJ2(I)/(THREE*G(I)+QH(I))      
       EPXE(I)=EPXE(I)+ DPLA(I)
       EPXE(I)=EPXE(I)*OFF(I)
      ENDDO
C
      ELSEIF(IPLA==2)THEN
       DO I=1,NEL
       SIG(I,1)=SCALE(I)*SIG(I,1)
       SIG(I,2)=SCALE(I)*SIG(I,2)
       SIG(I,3)=SCALE(I)*SIG(I,3)
       SIG(I,4)=SCALE(I)*SIG(I,4)
       SIG(I,5)=SCALE(I)*SIG(I,5)
       SIG(I,6)=SCALE(I)*SIG(I,6)
       DPLA(I) =(ONE -SCALE(I))*AJ2(I)/(THREE*G(I))      
       EPXE(I)=EPXE(I)+DPLA(I)
       EPXE(I)=EPXE(I)*OFF(I)
       ENDDO
C
      ELSEIF(IPLA==1)THEN
       DO I=1,NEL
C      plastic strain increment.
       DPLA(I)=(ONE -SCALE(I))*AJ2(I)/(THREE*G(I)+QH(I))
C      actual yield stress.
       AK(I)=AK(I)+DPLA(I)*QH(I)
       SCALE(I)= MIN(ONE,AK(I)/ MAX(AJ2(I),EM15))
       SIG(I,1)=SCALE(I)*SIG(I,1)
       SIG(I,2)=SCALE(I)*SIG(I,2)
       SIG(I,3)=SCALE(I)*SIG(I,3)
       SIG(I,4)=SCALE(I)*SIG(I,4)
       SIG(I,5)=SCALE(I)*SIG(I,5)
       SIG(I,6)=SCALE(I)*SIG(I,6)
       EPXE(I)=EPXE(I)+DPLA(I)
       EPXE(I)=EPXE(I)*OFF(I)
       ENDDO
      ENDIF
C
      DO I=1,NEL
       SIGY(I)=AK(I)
       DEFP(I)=EPXE(I)
      ENDDO 
C----------------------------------------------
C     TEMPERATURE (Heating due to plastic work)
C----------------------------------------------
      IF (JTHE /= 0 .AND. JLAG /= 0) THEN
        DO I=1,NEL       
          FHEAT(I) = FHEAT(I) + SIGY(I)*DPLA(I)*VOL(I)
        ENDDO
      ELSEIF(RHOCP > ZERO)THEN
        DO I=1,NEL
          TEMPEL(I) = TEMPEL(I) + SIGY(I)*DPLA(I) / RHOCP
          ! temperature and internal energy must be incremented consistantly
          ! internal energy is incremented later in parent subroutine (mmain)
          ! with total energy deformation which already includes plastic work
          !   Edef = 0.5 * VOL * sum ( sig.eps_dot , i=1..6)
          !   so internal energy and temperature remain consistant
        ENDDO
      END IF
c----------
      RETURN
      END
