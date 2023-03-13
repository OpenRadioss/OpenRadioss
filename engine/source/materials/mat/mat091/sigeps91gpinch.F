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
Chd|  SIGEPS91GPINCH                source/materials/mat/mat091/sigeps91gpinch.F
Chd|-- called by -----------
Chd|        MULAWGLCPINCH                 source/elements/shell/coqueba/mulawglcpinch.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SIGEPS91GPINCH(
     1                     JFT     ,JLT     ,NUVAR   ,UPARAM  ,RHO0    ,
     2                     THK     ,THK0    ,NEL     ,SSP     ,AREA    ,
     3                     DEPSXX  ,DEPSYY  ,DEPSZZ  ,
     4                     DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5                     DEPBXX  ,DEPBYY  ,DEPBXY  ,
     6                     DEPPXZ  ,DEPPYZ  ,
     7                     SIGOXX  ,SIGOYY  ,SIGOZZ  ,
     8                     SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     9                     MOMOXX  ,MOMOYY  ,MOMOXY  ,
     A                     MOMOPXZ ,MOMOPYZ ,
     B                     SIGNXX  ,SIGNYY  ,SIGNZZ  ,
     C                     SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     D                     MOMNXX  ,MOMNYY  ,MOMNXY  ,
     E                     MOMNPXZ ,MOMNPYZ ,TIME    ,UVAR    ,DT_INV   ,
     F                     VISCMX  ,ALDT    ,VOL0    ,IPM     ,MAT      ,
     G                     PLA     ,DEGMB   ,DEGFX   ,
     H                     NGL     ,EZZAVG  ,AREAPINCH)
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
#include      "param_c.inc"
C-----------------------------------------------
C   I N P U T   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,NEL,ISMSTR,NUVAR, IPM(NPROPMI,*),MAT(NEL),NGL(*)
C     REAL
      my_real
     .  UPARAM(*),UVAR(NEL,NUVAR)
      my_real
     .  TIME
      my_real
     .   DEPSXX(NEL),DEPSYY(NEL),DEPSZZ(NEL),
     .   DEPSXY(NEL),DEPSYZ(NEL),DEPSZX(NEL),
     .   DEPBXX(NEL),DEPBYY(NEL),DEPBXY(NEL),
     .   DEPPXZ(NEL),DEPPYZ(NEL),
     .   SIGOXX(NEL),SIGOYY(NEL),SIGOZZ(NEL),
     .   SIGOXY(NEL),SIGOYZ(NEL),SIGOZX(NEL),
     .   MOMOXX(NEL),MOMOYY(NEL),MOMOXY(NEL),
     .   MOMOPXZ(NEL),MOMOPYZ(NEL),
     .   AREA(NEL),THK(NEL),THK0(NEL),RHO0(NEL),DT_INV(NEL),ALDT(MVSIZ),
     .   VOL0(MVSIZ),PLA(NEL),DEGMB(MVSIZ),DEGFX(MVSIZ),EZZAVG(MVSIZ),
     .   AREAPINCH(MVSIZ)
C-----------------------------------------------
C   O U T P U T   A R G U M E N T S
C-----------------------------------------------
      my_real
     .    SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .    SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .    MOMNXX(NEL),MOMNYY(NEL),MOMNXY(NEL),
     .    MOMNPXZ(NEL),MOMNPYZ(NEL),
     .    SSP(NEL),RHO(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,MX,J,IADBUF
C     REAL
      my_real
     .   PA1,PA2,PA3,PA4,PA5,
     .   E,NU,G,GS,SIGY,HM,SIGY0,
     .   MS,FS,D1,D2,D3,C1,C2,C3, 
     .   AAA,BBB,CCC,P,BULK
      my_real
     .   VOL(MVSIZ),SPHEPS(MVSIZ), 
     .   DEVDEPSXX(MVSIZ),DEVDEPSYY(MVSIZ),DEVDEPSZZ(MVSIZ),
     .   DEVDEPSXY(MVSIZ),DEVDEPSYZ(MVSIZ),DEVDEPSXZ(MVSIZ),
     .   B1(MVSIZ),B2(MVSIZ),B3(MVSIZ),B4(MVSIZ),B5(MVSIZ),
     .   THK08(MVSIZ),VISCMX(MVSIZ),THKX(MVSIZ),MU(MVSIZ),
     .   SVONM(MVSIZ),CRIT(MVSIZ),SPHSIG(MVSIZ),MAGDEV(MVSIZ),INVMAGDEV(MVSIZ),
     .   DEVSXX(MVSIZ),DEVSYY(MVSIZ),DEVSZZ(MVSIZ),
     .   DEVSXY(MVSIZ),DEVSYZ(MVSIZ),DEVSZX(MVSIZ),
     .   N1(MVSIZ),N2(MVSIZ),N3(MVSIZ),N4(MVSIZ),N5(MVSIZ),N6(MVSIZ),
     .   RR(MVSIZ),UNSYEQ(MVSIZ),HH(MVSIZ),ETSE(MVSIZ),
     .   DEGSH_LOC(MVSIZ),DEGMB_LOC(MVSIZ),DEGFX_LOC(MVSIZ),DWELM(MVSIZ),
     .   DWELF(MVSIZ),DWPLA(MVSIZ),DPLA(MVSIZ),
     .   SDEVXX(MVSIZ),SDEVYY(MVSIZ),SDEVZZ(MVSIZ),
     .   SIGNDEVXX(MVSIZ),SIGNDEVYY(MVSIZ),SIGNDEVZZ(MVSIZ),PNEW(MVSIZ),PTRIAL(MVSIZ),
     .   POLD(MVSIZ),SIGODEVXX(MVSIZ),SIGODEVYY(MVSIZ),SIGODEVZZ(MVSIZ),DD(MVSIZ)
C-----------------------------------------------
C
C initialize
      IF (TIME == ZERO) THEN
        DO I=1,NEL
          UVAR(I,1) = AREAPINCH(I)*THK(I) 
          UVAR(I,2) = THK(I)
        ENDDO
      ENDIF
C retrieve material parameters from the buffer       
        MX = MAT(1)
        IADBUF = IPM(7,MX)
        E = UPARAM(IADBUF)
        NU  = UPARAM(IADBUF+1)
        SIGY0    = UPARAM(IADBUF+2)
        HM      = UPARAM(IADBUF+3)
        G    = HALF*E/(ONE+NU)
        GS   = 5.D0/6.D0*G
        BULK = E/(THREE*(ONE-TWO*NU))
C other derived material parameters
        PA1  = E*(ONE-NU)/(ONE+NU)/(ONE-TWO*NU)
        PA2  = E*NU/(ONE+NU)/(ONE-TWO*NU)
        PA3  = G
        PA4  = (ONE+NU)*(ONE-TWO*NU)/(ONE-NU**2)/(ONE-NU)*PA1
        PA5  = (ONE+NU)*(ONE-TWO*NU)/(ONE-NU**2)*PA2 
C C1/C2/C3 are PM(28:30) [ref:sigeps02g]
        C1 = ONE/E
        C2 = -NU*C1
        C3 = ONE/G
C
      DO I=1,NEL
        THK08(I)= THK0(I)*ONE_OVER_12
        B1(I)   = PA1*THK08(I)
        B2(I)   = PA2*THK08(I)
        B3(I)   = PA3*THK08(I)
        B4(I)   = PA4*THK08(I)
        B5(I)   = PA5*THK08(I)
      ENDDO
C
      DO I=1,NEL
C        THKX(I) = UVAR(I,2)*(1+HALF*DEPSZZ(I))/(1-HALF*DEPSZZ(I))
         THKX(I) = UVAR(I,2)*(1+HALF*EZZAVG(I))/(1-HALF*EZZAVG(I))

        UVAR(I,2) = THKX(I)
        VOL(I) = AREAPINCH(I)*THKX(I)
        RHO(I) = UVAR(I,1)*RHO0(I)/VOL(I)
        MU(I) = RHO(I)/RHO0(I)-ONE
        PNEW(I) = BULK*MU(I)
C        print*, NGL(I),PNEW(I)
      ENDDO

        
      DO I=JFT,JLT
C       energy of transverse shear + pinching
        DEGSH_LOC(I) = SIGOYZ(I)*DEPSYZ(I)+SIGOZX(I)*DEPSZX(I)
C       energy of only in plane components, xx,yy,xy
        DEGMB_LOC(I) = DEGMB(I) - DEGSH_LOC(I) 
C       bending energy
        DEGFX_LOC(I) = DEGFX(I) 
C       calculate old pressure and old derivatoric stress
        POLD(I) = -THIRD*(SIGOXX(I)+SIGOYY(I)+SIGOZZ(I))
        SIGODEVXX(I) = SIGOXX(I)+POLD(I)
        SIGODEVYY(I) = SIGOYY(I)+POLD(I)
        SIGODEVZZ(I) = SIGOZZ(I)+POLD(I)
C       calculate change in spherical strain = DD
        DD(I) = THIRD*(DEPSXX(I)+DEPSYY(I)+DEPSZZ(I))
C       update trial deviatoric stress to calculate trial deviatoric stress
        SIGNDEVXX(I)=SIGODEVXX(I)+TWO*G*(DEPSXX(I)-DD(I))
        SIGNDEVYY(I)=SIGODEVYY(I)+TWO*G*(DEPSYY(I)-DD(I))
        SIGNDEVZZ(I)=SIGODEVZZ(I)+TWO*G*(DEPSZZ(I)-DD(I))
C       shear stresses
        SIGNXY(I)=SIGOXY(I)+G*DEPSXY(I)
        SIGNYZ(I)=SIGOYZ(I)+GS*(DEPSYZ(I)+ZERO*DEPPYZ(I))
        SIGNZX(I)=SIGOZX(I)+GS*(DEPSZX(I)+ZERO*DEPPXZ(I))
C       moments
        MOMNXX(I)=MOMOXX(I)+B4(I)*DEPBXX(I)+B5(I)*DEPBYY(I)
        MOMNYY(I)=MOMOYY(I)+B5(I)*DEPBXX(I)+B4(I)*DEPBYY(I)
        MOMNXY(I)=MOMOXY(I)+B3(I)*DEPBXY(I)
        MOMNPXZ(I)=MOMOPXZ(I)+B3(I)*DEPPXZ(I)
        MOMNPYZ(I)=MOMOPYZ(I)+B3(I)*DEPPYZ(I)
C       Sound speed like solids
        SSP(I) = SQRT(PA1/RHO0(I))






      ENDDO
C     calculate equivalent stress 2D
      DO I=JFT,JLT
        MS = MOMNXX(I)+MOMNYY(I)
C     theory manual (global plasticity algorithm)
        UNSYEQ(I) = ONE/
     .    SQRT(MAX(SIXTEEN*(MS*MS + THREE*(MOMNXY(I)*MOMNXY(I) - MOMNXX(I)*MOMNYY(I)))
     .         +THREE*HALF*(SIGNDEVXX(I)**2+SIGNDEVYY(I)**2+SIGNDEVZZ(I)**2+TWO*SIGNXY(I)**2),EM20))
        SIGY = SIGY0 + HM*PLA(I)
        RR(I)  = MIN(ONE,SIGY*UNSYEQ(I))
      ENDDO
C
      DO I=JFT,JLT
        IF (RR(I) < ONE) THEN
          HH(I) = ZERO
          ETSE(I) = ZERO
        ENDIF
      ENDDO
C     energies calculation
      DO I=JFT,JLT
C
          SIGNXX(I) = SIGNDEVXX(I)*RR(I)-PNEW(I)
          SIGNYY(I) = SIGNDEVYY(I)*RR(I)-PNEW(I)
          SIGNXY(I) = SIGNXY(I)*RR(I)
          SIGNZZ(I) = SIGNDEVZZ(I)*RR(I)-PNEW(I)
C
          D1 = SIGNXX(I)-SIGOXX(I)
          D2 = SIGNYY(I)-SIGOYY(I)
          D3 = SIGNZZ(I)-SIGOZZ(I)
C
          DWELM(I) = (SIGNXX(I)+SIGOXX(I))*(C1*D1+C2*D2+C2*D3)+
     .               (SIGNYY(I)+SIGOYY(I))*(C2*D1+C1*D2+C2*D3)+
     .               (SIGNZZ(I)+SIGOZZ(I))*(C2*D1+C2*D2+C1*D3)+
     .               (SIGNXY(I)+SIGOXY(I))*(C3*(SIGNXY(I)-SIGOXY(I)))
          DEGMB_LOC(I) = DEGMB_LOC(I)+SIGNXX(I)*DEPSXX(I)+SIGNYY(I)*DEPSYY(I)
     .                               +SIGNZZ(I)*DEPSZZ(I)
     .                               +SIGNXY(I)*DEPSXY(I)
!
          MOMNXX(I) = MOMNXX(I)*RR(I)
          MOMNYY(I) = MOMNYY(I)*RR(I)
          MOMNXY(I) = MOMNXY(I)*RR(I)
          D1 = MOMNXX(I)-MOMOXX(I)
          D2 = MOMNYY(I)-MOMOYY(I)
          DWELF(I) = TWELVE*(
     .              (MOMNXX(I)+MOMOXX(I))*(C1*D1+C2*D2)
     .             +(MOMNYY(I)+MOMOYY(I))*(C2*D1+C1*D2)
     .             +(MOMNXY(I)+MOMOXY(I))*(C3*(MOMNXY(I)-MOMOXY(I))) )
          DEGFX_LOC(I) = DEGFX_LOC(I)+ MOMNXX(I)*DEPBXX(I)+MOMNYY(I)*DEPBYY(I)
     .                                +MOMNXY(I)*DEPBXY(I)

      ENDDO
C     delta of plastic work  
      DO I=JFT,JLT
          DWPLA(I) = DEGMB_LOC(I)+DEGFX_LOC(I)*THK0(I)-DWELM(I)-DWELF(I)
      ENDDO
C     equivalent plastic strain 
      DO I=JFT,JLT
          DPLA(I) = MAX(ZERO,HALF*DWPLA(I)/SIGY)
          PLA(I) = PLA(I) + DPLA(I)
      ENDDO  
      RETURN
      END
