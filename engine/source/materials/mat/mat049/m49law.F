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
      !||    m49law   ../engine/source/materials/mat/mat049/m49law.F
      !||--- called by ------------------------------------------------------
      !||    mmain    ../engine/source/materials/mat_share/mmain.F90
      !||====================================================================
      SUBROUTINE M49LAW(MAT    ,PM     ,OFF    ,SIG    ,EPXE ,
     2                  THETA  ,EPD    ,CXX    ,DF     ,D1   ,
     3                  D2     ,D3     ,D4     ,D5     ,D6   ,
     4                  RHO0   ,DPDM   ,SIGY   ,DEFP   ,DPLA ,
     5                  ESPE   ,ECOLD  ,NEL    ,JLAG   ,JTHE ,
     6                  FHEAT  ,VOL)
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
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MAT(MVSIZ),NEL
      INTEGER, INTENT(IN) :: JLAG !< flag for lagrangian framework
      INTEGER, INTENT(IN) :: JTHE !< flag for heat equation
      my_real PM(NPROPM,*),OFF(MVSIZ) ,SIG(NEL,6),EPXE(MVSIZ),
     .        THETA(MVSIZ),EPD(MVSIZ) ,CXX(MVSIZ)  ,DF(MVSIZ)  , D1(MVSIZ)  ,
     .        D2(MVSIZ)   ,D3(MVSIZ)  ,D4(MVSIZ)   ,D5(MVSIZ)  , D6(MVSIZ)  ,
     .        RHO0(MVSIZ) ,DPDM(MVSIZ),SIGY(MVSIZ) ,DEFP(MVSIZ), DPLA(MVSIZ),
     .        ESPE(MVSIZ) ,ECOLD(MVSIZ)
      my_real, DIMENSION(NEL) ,INTENT(INOUT) :: FHEAT ! Heat due to plastic work for Heat Equation with lagrangian framework
      my_real, DIMENSION(NEL) ,INTENT(IN) :: VOL ! Element Volume
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX,ITB, K, J, IFRAC(MVSIZ),IQUAD(MVSIZ), IQUADI, IFLAGR(MVSIZ), NBE
      my_real G(MVSIZ)  ,G0   ,G1(MVSIZ)    ,G2(MVSIZ)   ,SIG0,
     .        CB ,CN   ,CB1   ,CB2  ,CH  ,
     .        CF ,EPMX ,SIGMX ,SPH  ,T0  ,
     .        P(MVSIZ)  ,DAV(MVSIZ)  ,TMELT ,EMELT(MVSIZ),QH(MVSIZ)  ,
     .        QD(MVSIZ) ,AJ2(MVSIZ)  ,SCALE(MVSIZ) ,YLD(MVSIZ)
      my_real QA, QB, QC, QE, RHOCP
C-----------------------------------------------
C   B o d y
C-----------------------------------------------
      MX   = MAT(1)
      G0   = PM(22,MX)
      SIG0 = PM(38,MX)
      CB   = PM(39,MX)
      CN   = PM(40,MX)
      EPMX = PM(41,MX)
      SIGMX= PM(42,MX)
      CB1  = PM(43,MX)
      CB2  = PM(44,MX)
      CH   = PM(45,MX)
      SPH  = PM(69,MX)
      CF   = PM(77,MX)
      T0   = PM(78,MX)
      TMELT= PM(46,MX)
      RHOCP= PM(69,MX)
C
      DO I=1,NEL
        P(I)  =-(SIG(I,1)+SIG(I,2)+SIG(I,3))*THIRD
        DAV(I)=-(D1(I)+D2(I)+D3(I))*THIRD
        EMELT(I) = ECOLD(I) + SPH*TMELT
      ENDDO
C----------------------------
C     SHEAR MODULUS & YIELD
C----------------------------
      DO I=1,NEL
        QA = P(I)*DF(I)**THIRD
        QB = ONE - CH*(THETA(I)-T0)
        IF(EMELT(I)<=ZERO .OR. CF<=ZERO) THEN
           QC = ONE
        ELSEIF(ESPE(I)>=EMELT(I)) THEN
           QC = ZERO
        ELSE
           QC = EXP(CF * ESPE(I) / (ESPE(I)-EMELT(I)))
        ENDIF
        G(I)  = G0 * (CB1*QA + QB) * QC
        QD(I) = (CB2*QA + QB) * QC
        IF(EPXE(I)<=ZERO) THEN
          QE = SIG0
        ELSEIF(EPXE(I)>EPMX) THEN
          QE = SIG0 * ((ONE + CB*EPMX)**CN)
        ELSE
          QE = SIG0 * ((ONE + CB*EPXE(I))**CN)
        ENDIF       
        YLD(I) = MIN(SIGMX, QE) * QD(I)       
      ENDDO
C--------------------------------      
C     DEVIATORIC STRESS ESTIMATE
C--------------------------------
      DO I=1,NEL
        G1(I) = DT1*G(I)*OFF(I)
        G2(I) = TWO*G1(I)*OFF(I)
        SIG(I,1)=SIG(I,1)+P(I)+G2(I)*(D1(I)+DAV(I))
        SIG(I,2)=SIG(I,2)+P(I)+G2(I)*(D2(I)+DAV(I))
        SIG(I,3)=SIG(I,3)+P(I)+G2(I)*(D3(I)+DAV(I))
        SIG(I,4)=SIG(I,4)+G1(I)*D4(I)
        SIG(I,5)=SIG(I,5)+G1(I)*D5(I)
        SIG(I,6)=SIG(I,6)+G1(I)*D6(I)
      ENDDO
C---     dP/dRHO
      DO I=1,NEL
        DPDM(I) = DPDM(I) + FOUR_OVER_3*G(I)
        CXX(I)=SQRT(ABS(DPDM(I))/RHO0(I))
      ENDDO
C
      DO I=1,NEL
        EPD(I)=OFF(I)*MAX(ABS(D1(I)), ABS(D2(I)), ABS(D3(I)), HALF*ABS(D4(I)),HALF*ABS(D5(I)),HALF*ABS(D6(I)))
        AJ2(I)=HALF*(SIG(I,1)**2+SIG(I,2)**2+SIG(I,3)**2)+SIG(I,4)**2+SIG(I,5)**2+SIG(I,6)**2
        AJ2(I)=SQRT(THREE*AJ2(I))
      ENDDO
C---
      DO I=1,NEL
C---      Melting
        IF(THETA(I)>=TMELT) THEN
          QH(I) =ZERO
          G(I)  =ZERO
          YLD(I)=ZERO
          AJ2(I)=ZERO
          SCALE(I)=ZERO    
        ELSE
C---      Hardening rule
          IF(CN>=1) THEN
            QH(I)= QD(I)*SIG0*CB*CN * ((ONE + CB*EPXE(I))**(CN-ONE))
          ELSEIF(EPXE(I)>ZERO)THEN
            QH(I)= QD(I)*SIG0*CB*CN / ((ONE + CB*EPXE(I))**(ONE-CN))
          ELSE
            QH(I)=ZERO
          ENDIF
C---      Radial return
          IF(AJ2(I)<=YLD(I)) THEN
            SCALE(I)=ONE
          ELSEIF(AJ2(I)/=ZERO) THEN
            SCALE(I)=YLD(I)/AJ2(I)
          ELSE
            SCALE(I)=ZERO
          ENDIF
        ENDIF
      ENDDO
C---    Radial return
      DO I=1,NEL
C---    plastic strain increment.
        DPLA(I)=(ONE -SCALE(I))*AJ2(I)/MAX((THREE*G(I)+QH(I)),EM15)
C---    actual yield stress.
        YLD(I)  =YLD(I)+DPLA(I)*QH(I)
        SIG(I,1)=SCALE(I)*SIG(I,1)
        SIG(I,2)=SCALE(I)*SIG(I,2)
        SIG(I,3)=SCALE(I)*SIG(I,3)
        SIG(I,4)=SCALE(I)*SIG(I,4)
        SIG(I,5)=SCALE(I)*SIG(I,5)
        SIG(I,6)=SCALE(I)*SIG(I,6)
        EPXE(I) =EPXE(I)+DPLA(I)
        EPXE(I) =EPXE(I)*OFF(I)
      ENDDO
C
      DO I=1,NEL
       SIGY(I)=YLD(I)
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
          THETA(I) = THETA(I) + SIGY(I)*DPLA(I) / RHOCP
          ! temperature and internal energy must be incremented consistantly
          ! internal energy is incremented later in parent subroutine (mmain)
          ! with total energy deformation which already includes plastic work
          !   Edef = 0.5 * VOL * sum ( sig.eps_dot , i=1..6)
          !   so internal energy and temperature remain consistant
        ENDDO
      END IF


      RETURN
      END
