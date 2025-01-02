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
      !||    m2law            ../engine/source/materials/mat/mat002/m2law.F
      !||--- called by ------------------------------------------------------
      !||    mmain            ../engine/source/materials/mat_share/mmain.F90
      !||--- calls      -----------------------------------------------------
      !||    m2iter_imp       ../engine/source/materials/mat/mat002/m2iter_imp.F
      !||    mdtsph           ../engine/source/materials/mat_share/mdtsph.F
      !||    mqviscb          ../engine/source/materials/mat_share/mqviscb.F
      !||    mstrain_rate     ../engine/source/materials/mat_share/mstrain_rate.F
      !||--- uses       -----------------------------------------------------
      !||    glob_therm_mod   ../common_source/modules/mat_elem/glob_therm_mod.F90
      !||====================================================================
      SUBROUTINE M2LAW(
     1   PM,      OFF,     SIG,     EINT,
     2   RHO,     QOLD,    EPXE,    EPSD,
     3   VOL,     STIFN,   DT2T,    NELTST,
     4   ITYPTST, OFFG,    GEO,     PID,
     5   AMU,     VOL_AVG, MUMAX,   MAT,
     6   NGL,     SSP,     DVOL,    AIRE,
     7   VNEW,    VD2,     DELTAX,  VIS,
     8   D1,      D2,      D3,      D4,
     9   D5,      D6,      PNEW,    PSH,
     A   QNEW,    SSP_EQ,  SOLD1,   SOLD2,
     B   SOLD3,   SOLD4,   SOLD5,   SOLD6,
     C   SIGY,    DEFP,    DPLA,
     D   EPSP,    TSTAR,   ETSE,    MSSA,
     E   DMELS,   TEMPEL,  SIGBAK,  AL_IMP,
     F   SIGNOR,  CONDE,   DTEL,    G_DT,
     G   NEL,     IPM,     RHOREF,  RHOSP,
     H   IPG,     DMG,     ITY,     JTUR,
     I   JTHE,    JSPH,    ISMSTR,  JSMS,
     J   PLAP,    NPG ,    IEOS  ,  DPDM ,    FHEAT ,   glob_therm)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      use glob_therm_mod
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "impl1_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NPG
      INTEGER, INTENT(IN) :: ISMSTR
      INTEGER, INTENT(IN) :: JSMS
      INTEGER, INTENT(IN) :: ITY
      INTEGER, INTENT(IN) :: JTUR
      INTEGER, INTENT(IN) :: JTHE
      INTEGER, INTENT(IN) :: JSPH
      INTEGER, INTENT(IN) :: IEOS
C
      INTEGER NELTST,ITYPTST,PID(NEL),G_DT,NEL,IPG
      INTEGER MAT(NEL),NGL(NEL), IPM(NPROPMI,*)
      my_real, DIMENSION(NEL) ,INTENT(INOUT) :: PLAP
      my_real :: DT2T
C
      my_real
     .   PM(NPROPM,*), OFF(NEL), SIG(NEL,6), EINT(NEL), RHO(NEL), QOLD(NEL),
     .   EPXE(NEL), EPSD(NEL), VOL(NEL), STIFN(NEL), OFFG(NEL),GEO(NPROPG,*),
     .   MUMAX(NEL),AMU(NEL),VOL_AVG(NEL)
      my_real
     .   VNEW(NEL), VD2(NEL), DELTAX(NEL), SSP(NEL), AIRE(NEL), VIS(NEL), 
     .   PSH(NEL), PNEW(NEL),QNEW(NEL) ,SSP_EQ(NEL), DVOL(NEL), 
     .   D1(NEL), D2(NEL), D3(NEL), D4(NEL), D5(NEL), D6(NEL), 
     .   SOLD1(MVSIZ), SOLD2(MVSIZ), SOLD3(MVSIZ),
     .   SOLD4(MVSIZ), SOLD5(MVSIZ), SOLD6(MVSIZ),
     .   TSTAR(MVSIZ), DPLA(MVSIZ), EPSP(MVSIZ), DMG(NEL)
      my_real, DIMENSION(MVSIZ) ,INTENT(IN) :: DPDM
      my_real, DIMENSION(MVSIZ) ,INTENT(INOUT) :: FHEAT
C
      my_real
     .   SIGY(NEL) ,DEFP(NEL),ETSE(NEL), MSSA(NEL), DMELS(NEL), TEMPEL(NEL),
     .   SIGBAK(NEL,6),AL_IMP(NEL),SIGNOR(MVSIZ,6),CONDE(NEL),DTEL(NEL),
     .   RHOREF(NEL)  ,RHOSP(NEL)  
      type (glob_therm_) ,intent(inout) :: glob_therm
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I, J, ICC,IRTY,ISRATE,VP,IDEV,MX,IBID,IKFLG,NINDX
      INTEGER :: INDX(NEL)
      my_real
     .   RHO0, PLAP1,POLD,PSHIFT,BULK,P, EPMX,CC,CN,EPDR, CMX, 
     .   Z3,Z4,RHOCPI,T_REF,T_MELT,ASRATE,
     .   E1, E2, E3, E4, E5,E6, EINC, G0,G1,G2,G3,G3H,
     .   SCALE, BID1, BID2, BID3, DTA,MT,
     .   DSXX,DSYY,DSZZ,DSXY,DSYZ,DSZX,ALPHA,HKIN,
     .   FACQ0,FISOKIN,BETA,VM,VM_1,NORM_1,CA0,CB,SIGM0
      my_real
     .   EPD(MVSIZ),G(MVSIZ), AK(MVSIZ),PC(MVSIZ), QH(MVSIZ), 
     .   AJ2(MVSIZ), DAV(MVSIZ),CA(MVSIZ),SIGMX(MVSIZ),
     .   SIGEXX(MVSIZ),SIGEYY(MVSIZ),SIGEZZ(MVSIZ),
     .   SIGEXY(MVSIZ),SIGEYZ(MVSIZ),SIGEZX(MVSIZ)
      my_real, DIMENSION(MVSIZ) :: BIDMVSIZ
C-----------------------------------------------
      FACQ0   = ONE
C
      MX      = MAT(1)
      RHO0    = PM( 1,MX)
      BULK    = PM(32,MX)
      CN      = PM(40,MX)
      CC      = PM(43,MX)
      ICC     = NINT(PM(49,MX))
      EPMX    = PM(41,MX)
      EPDR    = PM(44,MX)
      IRTY    = NINT(PM(50,MX))
      Z3      = PM(51,MX)
      CMX     = PM(45,MX)
      ISRATE  = IPM(3,MX)
      ASRATE  = MIN(ONE,PM(9,MX)*DT1)
      FISOKIN = PM(55,MX)
      G0      = PM(22,MX)
      CA0     = PM(38,MX)
      CB      = PM(39,MX)
      SIGM0   = PM(42,MX)
      VP      = IPM(255,MX)
      PSHIFT  = PM(88,MX)
!
      IF (IRTY == 1) THEN        ! Zerilli
        RHOCPI  = PM(53,MX)       
        T_REF   = PM(54,MX)
        Z4      = PM(52,MX)
      ELSE                       ! Johnson-Cook
        RHOCPI = PM(69,MX) 
        IF (RHOCPI > ZERO) RHOCPI = ONE / RHOCPI
        T_REF  = PM(79,MX)
        T_MELT = PM(80,MX)
      END IF
!
      DO I=1,NEL
        G(I)    = G0*OFF(I)
        CA(I)   = CA0
        SIGMX(I)= SIGM0
        ETSE(I) = ONE
      ENDDO
C
C------------------------------------------
C     ECROUISSAGE CINE
C------------------------------------------
      IKFLG=0
      DO I=1,NEL
        IF (FISOKIN /= ZERO ) THEN
         SIG(I,1)=SIG(I,1)-SIGBAK(I,1)
         SIG(I,2)=SIG(I,2)-SIGBAK(I,2)
         SIG(I,3)=SIG(I,3)-SIGBAK(I,3)
         SIG(I,4)=SIG(I,4)-SIGBAK(I,4)
         SIG(I,5)=SIG(I,5)-SIGBAK(I,5)
         SIG(I,6)=SIG(I,6)-SIGBAK(I,6)
         IKFLG = IKFLG + 1
        ENDIF           
      ENDDO
C      
      DO I=1,NEL
        P  =-THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3))
        DAV(I)=-THIRD*(D1(I)+D2(I)+D3(I))
        G1=DT1*G(I)
        G2=TWO*G1
        SSP(I)=SQRT((ONEP333*G(I)+BULK)/RHO0)
c      -- deviatoric elastic stress predictor
        SIG(I,1)=SIG(I,1)+P+G2*(D1(I)+DAV(I))
        SIG(I,2)=SIG(I,2)+P+G2*(D2(I)+DAV(I))
        SIG(I,3)=SIG(I,3)+P+G2*(D3(I)+DAV(I))
        SIG(I,4)=SIG(I,4)+G1*D4(I)
        SIG(I,5)=SIG(I,5)+G1*D5(I)
        SIG(I,6)=SIG(I,6)+G1*D6(I)
C
c      -- von Mises stress at elastic predictor
        AJ2(I)=HALF*(SIG(I,1)**2+SIG(I,2)**2+SIG(I,3)**2)
     .             + SIG(I,4)**2+SIG(I,5)**2+SIG(I,6)**2
        AJ2(I)=SQRT(THREE*AJ2(I))
      ENDDO
C
c     --  storing elastic predictors for stiffness computation
      IF (IMPL_S>0.OR.IKFLG>0) THEN
        DO I=1,NEL
         SIGEXX(I) = SIG(I,1)
         SIGEYY(I) = SIG(I,2)
         SIGEZZ(I) = SIG(I,3)        
         SIGEXY(I) = SIG(I,4)
         SIGEYZ(I) = SIG(I,5)
         SIGEZX(I) = SIG(I,6)
        ENDDO
      ENDIF
C-------------
C     STRAIN RATE (JOHNSON-COOK, ZERILLI-ARMSTRONG)
C-------------
      IDEV = VP - 2
      CALL MSTRAIN_RATE(NEL    ,ISRATE ,ASRATE ,EPSD   ,IDEV   ,
     .                  D1,      D2,      D3,      D4,       D5,      D6)

      EPSP(1:NEL) = EPSD(1:NEL)                   
c

      IF (CC /= ZERO) THEN
        IF(VP == 1)THEN
          DO I=1,NEL
            EPD(I)= MAX(PLAP(I),EPDR)
            EPD(I)= LOG(EPD(I)/EPDR)  
          ENDDO
        ELSE
          DO I=1,NEL
            EPD(I)= MAX(EPSD(I),EM15)
            EPD(I)= LOG(EPD(I)/EPDR)  
          ENDDO
        ENDIF ! VP
!  
        IF (IRTY == 0) THEN   ! J-C
          DO I=1,NEL
             MT=MAX(EM15,Z3)
             EPD(I)= MAX(ZERO,EPD(I))
             EPD(I)= (ONE +CC * EPD(I))*(ONE -TSTAR(I)**MT)
             IF(ICC==1) SIGMX(I)= SIGM0*EPD(I)
          ENDDO
        ELSEIF (IRTY==1) THEN
          DO I=1,NEL
             EPD(I)= CC*EXP((-Z3+Z4 * EPD(I))*TEMPEL(I))
             IF (ICC==1) SIGMX(I) = SIGM0 + EPD(I)
             CA(I) = CA0 + EPD(I)
             EPD(I)=ONE
          ENDDO
        END IF
      ELSE
        DO I=1,NEL
          EPD(I)=ONE
        ENDDO
      ENDIF
C-------------
C     CRITERE
C-------------
      IF (IKFLG == 0 ) THEN  ! isotropic hardening
        IF(CN==ONE) THEN
          AK(1:NEL)= CA(1:NEL)+CB*EPXE(1:NEL)
          QH(1:NEL)= CB*EPD(1:NEL)
        ELSE
          DO  I=1,NEL
            IF(EPXE(I)>ZERO) THEN
              AK(I)=CA(I)+CB*EPXE(I)**CN
              IF(CN>ONE) THEN
                QH(I) = (CB*CN*EPXE(I)**(CN - ONE))*EPD(I)
              ELSE
                QH(I) = (CB*CN/EPXE(I)**(ONE -CN))*EPD(I)
              ENDIF
            ELSE
              AK(I)=CA(I)
              QH(I)=ZERO
            ENDIF
          ENDDO
        ENDIF
        DO  I=1,NEL
          AK(I)=AK(I)*EPD(I)
          IF(SIGMX(I)<AK(I))THEN
            AK(I)=SIGMX(I)
            QH(I)=ZERO
          ENDIF
          SIGY(I) = AK(I)
          IF(EPXE(I)>EPMX)THEN
            AK(I)=ZERO
            QH(I)=ZERO
          ENDIF
        ENDDO
!
      ELSE     ! IKFLG > 0 => kinematic hardening
!
        DO I=1,NEL
          BETA = ONE-FISOKIN
          ! SIGY is used for hourglass stress compute--        
          IF(CN==ONE) THEN
            SIGY(I) = CA(I)+CB*EPXE(I)
            AK(I)= CA(I)+BETA*CB*EPXE(I)
            QH(I)= CB*EPD(I)
          ELSE
            IF(EPXE(I)>ZERO) THEN
              SIGY(I)=CA(I)+CB*EPXE(I)**CN
              AK(I)=CA(I)+BETA*CB*EPXE(I)**CN
              IF(CN>ONE) THEN
                QH(I)= (CB*CN*EPXE(I)**(CN - ONE))*EPD(I)
              ELSE
                QH(I)= (CB*CN/EPXE(I)**(ONE -CN))*EPD(I)
              ENDIF
            ELSE
             AK(I)=CA(I)
             SIGY(I)=CA(I)
             QH(I)=ZERO
            ENDIF
          ENDIF
          AK(I)=AK(I)*EPD(I)
          SIGY(I)=SIGY(I)*EPD(I)
          IF(SIGMX(I)<AK(I))THEN
            AK(I)=SIGMX(I)
            QH(I)=ZERO
          ENDIF
          SIGY(I)=MIN(SIGY(I),SIGMX(I))
          IF(EPXE(I)>EPMX)THEN
            AK(I)=ZERO
            QH(I)=ZERO
          ENDIF
        END DO !I=1,NEL
      END IF !(IKFLG == 0 ) THEN
!---------------------------------------------------------
      IF (IMPL_S==0) THEN
        DO I=1,NEL
          SCALE= MIN(ONE,AK(I)/ MAX(AJ2(I),EM15))
C         plastic strain increment.
          DPLA(I)=(ONE-SCALE)*AJ2(I)/MAX(THREE*G(I)+QH(I),EM15)
C         actual yield stress.
          AK(I)=AK(I)+(ONE - FISOKIN)*DPLA(I)*QH(I)
          SCALE= MIN(ONE,AK(I)/ MAX(AJ2(I),EM15))
          SIG(I,1)=SCALE*SIG(I,1)
          SIG(I,2)=SCALE*SIG(I,2)
          SIG(I,3)=SCALE*SIG(I,3)
          SIG(I,4)=SCALE*SIG(I,4)
          SIG(I,5)=SCALE*SIG(I,5)
          SIG(I,6)=SCALE*SIG(I,6)
          EPXE(I)=EPXE(I)+DPLA(I)
        ENDDO
!
      ELSE    ! implicit : nonlinear hardening requires iterations in radial return
        CALL  M2ITER_IMP(SIG,     EPXE,    AJ2,     G,
     1                   CA,      CB,      CN,      EPD,
     2                   SIGMX,   EPMX,    DPLA,   AK,
     3                   QH,      SIGY,    FISOKIN, NEL)
!
      END IF ! IMPL_S
c-----------------------------------------
      IF (IKFLG > 0 ) THEN
       DO I=1,NEL
          DSXX = SIGEXX(I) - SIG(I,1) 
          DSYY = SIGEYY(I) - SIG(I,2)
          DSZZ = SIGEZZ(I) - SIG(I,3)
          DSXY = SIGEXY(I) - SIG(I,4)
          DSYZ = SIGEYZ(I) - SIG(I,5)
          DSZX = SIGEZX(I) - SIG(I,6)
C 
          HKIN = TWO_THIRD*FISOKIN*QH(I)
          ALPHA = HKIN/MAX(TWO*G(I)+HKIN,EM15)  
C       ..updates back stresses
          SIGBAK(I,1) = SIGBAK(I,1) + ALPHA*DSXX 
          SIGBAK(I,2) = SIGBAK(I,2) + ALPHA*DSYY 
          SIGBAK(I,3) = SIGBAK(I,3) + ALPHA*DSZZ 
          SIGBAK(I,4) = SIGBAK(I,4) + ALPHA*DSXY 
          SIGBAK(I,5) = SIGBAK(I,5) + ALPHA*DSYZ 
          SIGBAK(I,6) = SIGBAK(I,6) + ALPHA*DSZX 
C       ..gets stresses from shifted stresses and back stresses
          SIG(I,1)=SIG(I,1) + SIGBAK(I,1)
          SIG(I,2)=SIG(I,2) + SIGBAK(I,2)
          SIG(I,3)=SIG(I,3) + SIGBAK(I,3)
          SIG(I,4)=SIG(I,4) + SIGBAK(I,4)
          SIG(I,5)=SIG(I,5) + SIGBAK(I,5)
          SIG(I,6)=SIG(I,6) + SIGBAK(I,6)
       ENDDO
      END IF !(IKFLG > 0 ) THEN
c-----------------------------------------
!
      BIDMVSIZ(1:MVSIZ) = ZERO
      IF (JSPH==0) THEN
       CALL MQVISCB(
     1   PM,      OFF,     RHO,     BIDMVSIZ,
     2   BIDMVSIZ,SSP,     BIDMVSIZ,STIFN,
     3   DT2T,    NELTST,  ITYPTST, AIRE,
     4   OFFG,    GEO,     PID,     VNEW,
     5   VD2,     DELTAX,  VIS,     D1,
     6   D2,      D3,      PNEW,    PSH,
     7   MAT,     NGL,     QNEW,    SSP_EQ,
     8   VOL,     MSSA,    DMELS,   IBID,
     9   FACQ0,   CONDE,   DTEL,    G_DT,
     A   IPM,     RHOREF,  RHOSP,   NEL,
     B   ITY,     ISMSTR,  JTUR,    JTHE,
     C   JSMS,    NPG   ,   glob_therm)
      ELSE
       CALL MDTSPH(
     1   PM,      OFF,     RHO,     BIDMVSIZ,
     2   BIDMVSIZ,BIDMVSIZ,STIFN,   DT2T,
     3   NELTST,  ITYPTST, OFFG,    GEO,
     4   PID,     MUMAX,   SSP,     VNEW,
     5   VD2,     DELTAX,  VIS,     D1,
     6   D2,      D3,      PNEW,    PSH,
     7   MAT,     NGL,     QNEW,    SSP_EQ,
     8   G_DT,    DTEL,    NEL,     ITY,
     9   JTUR,    JTHE)
      ENDIF
C
      DTA = HALF*DT1
C
      NINDX = 0
      INDX(1:NEL) = 0
      DO I=1,NEL
        IF ((EPXE(I) > EPMX).AND.(DMG(I)==ZERO)) THEN 
          NINDX = NINDX + 1
          INDX(NINDX) = I
          DMG(I) = ONE
        ENDIF
      ENDDO
 
      IF (IEOS == 0) THEN !  add pressure to the deviatoric stress
        DO I=1,NEL
          PNEW(I)  = BULK*AMU(I)
          SIG(I,1) =(SIG(I,1) - PNEW(I))*OFF(I)
          SIG(I,2) =(SIG(I,2) - PNEW(I))*OFF(I)
          SIG(I,3) =(SIG(I,3) - PNEW(I))*OFF(I)
          SIG(I,4) = SIG(I,4) * OFF(I)
          SIG(I,5) = SIG(I,5) * OFF(I)
          SIG(I,6) = SIG(I,6) * OFF(I)
        ENDDO
!
        DO I=1,NEL
          E1 = D1(I)*(SOLD1(I)+SIG(I,1))
          E2 = D2(I)*(SOLD2(I)+SIG(I,2))
          E3 = D3(I)*(SOLD3(I)+SIG(I,3))
          E4 = D4(I)*(SOLD4(I)+SIG(I,4))
          E5 = D5(I)*(SOLD5(I)+SIG(I,5))
          E6 = D6(I)*(SOLD6(I)+SIG(I,6))
          EINC = VOL_AVG(I)*(E1+E2+E3+E4+E5+E6)*DTA - HALF*DVOL(I)*(QOLD(I)+QNEW(I))
          EINT(I) = (EINT(I)+EINC*OFF(I)) / MAX(EM15,VOL(I))
        ENDDO

      ELSE
        ! if EOS is used, material law calculates only deviatoric stress tensor
        !                 sound speed depends on pressure derivative over volume change
        !                 calculated in EOS
        DO I = 1, NEL
          SSP(I) = SQRT((DPDM(I) + FOUR*G(I)/THREE)/RHO0)
!
          POLD     = (SOLD1(I)+SOLD2(I)+SOLD3(I)) * THIRD
          SOLD1(I) = SOLD1(I) - POLD
          SOLD2(I) = SOLD2(I) - POLD
          SOLD3(I) = SOLD3(I) - POLD
          E1 = D1(I) * (SOLD1(I)+SIG(I,1))
          E2 = D2(I) * (SOLD2(I)+SIG(I,2))
          E3 = D3(I) * (SOLD3(I)+SIG(I,3))
          E4 = D4(I) * (SOLD4(I)+SIG(I,4))
          E5 = D5(I) * (SOLD5(I)+SIG(I,5))
          E6 = D6(I) * (SOLD6(I)+SIG(I,6))
          EINC    = VOL_AVG(I) * (E1+E2+E3+E4+E5+E6) * DTA
          EINT(I) = EINT(I) + (EINC+HALF*DVOL(I)*(POLD-PSHIFT-QOLD(I)-QNEW(I)))*OFF(I)
        ENDDO       
      END IF       
C
      DO I=1,NEL
        QOLD(I) = QNEW(I)
        DEFP(I) = EPXE(I)
        SIGY(I) = MAX(SIGY(I),AK(I))
      ENDDO
      DO I=1,NEL
        IF (DPLA(I) > 0) ETSE(I)= HALF*QH(I)*OFF(I)/MAX(G(I),EM15)
      ENDDO
!----------------------------------------------------------------     
!     implicit
!----------------------------------------------------------------     
      IF (IMPL_S>0) THEN
        IF (IKT==0) RETURN
        IF (IKFLG==0)THEN
          DO I=1,NEL
           IF (DPLA(I)>0)THEN
            IF(CN==ONE) THEN
             QH(I)= CB*EPD(I)
            ELSEIF(CN>ONE) THEN
             QH(I)= (CB*CN*EPXE(I)**(CN - ONE))*EPD(I)
            ELSE
             QH(I)= (CB*CN/EPXE(I)**(ONE -CN))*EPD(I)
            ENDIF
           ENDIF
          ENDDO
        ENDIF
C      -----IKT=4------------
        DO I = 1,NEL
         IF (DPLA(I)>ZERO) THEN

c .....   Von Mises stress at the elastic predictor (point B)
c .....   SIGEXX, etc. are deviatoric stresses
          VM =HALF*(SIGEXX(I)**2+SIGEYY(I)**2+SIGEZZ(I)**2)
     1               +SIGEXY(I)**2+SIGEYZ(I)**2+SIGEZX(I)**2
          VM_1 =ONE/SQRT(THREE*VM)
          G3 = THREE*G(I)
          G3H = MAX(G3+QH(I),EM15)
          SCALE = MAX(ZERO,ONE-G3H*DPLA(I)*VM_1)
c .....   NORM_1 normalizes deviatoric stresses, includes consistent
c .....   stiffness matrix parameter beta, von Mises at B, and two_pmi
          NORM_1=G3*VM_1*SQRT(SCALE/G3H)
c .....   Deviatoric stresses "normalized"
          SIGNOR(I,1)=SIGEXX(I)*NORM_1
          SIGNOR(I,2)=SIGEYY(I)*NORM_1
          SIGNOR(I,3)=SIGEZZ(I)*NORM_1
          SIGNOR(I,4)=SIGEXY(I)*NORM_1
          SIGNOR(I,5)=SIGEYZ(I)*NORM_1
          SIGNOR(I,6)=SIGEZX(I)*NORM_1

c .....   Parameter alpha of consistent matrix
          AL_IMP(I)= ONE - G3*DPLA(I)*VM_1
         ELSE
           AL_IMP(I)=ONE
         ENDIF
        ENDDO
      ENDIF
!---------------------------------------------------------
      ! end implicit
!---------------------------------------------------------
!     update and filter plastic strain rate for VP=1
!---------------------------------------------------------
!
      IF (VP== 1) THEN
        DO I=1,NEL       
          PLAP1   = DPLA(I)/MAX(EM20,DT1)
          PLAP(I) = ASRATE * PLAP1 + (ONE - ASRATE) * PLAP(I)
        ENDDO
      ENDIF
!---------------------------------------------------------
!     calculate thermal load due to plastic work
!---------------------------------------------------------
      IF (JTHE == 0 .and. RHOCPI > ZERO) THEN  ! update temperature in adiabatic conditions
        DO I=1,NEL       
          TEMPEL(I) = TEMPEL(I) + SIGY(I)*DPLA(I) * RHOCPI
        ENDDO
      ELSE IF (JTHE < 0) THEN  ! cumulate thermal load due to plastic work for /heat/mat
        DO I=1,NEL       
          FHEAT(I) = FHEAT(I) + SIGY(I)*DPLA(I)*VOL(I)
        ENDDO
      END IF
!---------------------------------------------------------
      ! Printout element deletion
      IF (NINDX > 0) THEN
        DO J=1,NINDX
#include "lockon.inc"
          WRITE(IOUT, 1000) NGL(INDX(J)),IPG
          WRITE(ISTDO,1100) NGL(INDX(J)),IPG,TT
#include "lockoff.inc"
        ENDDO
      ENDIF
!---------------------------------------------------------------
 1000 FORMAT(1X,'EXCEEDED EPS_MAX ON SOLID ELEMENT NUMBER ',I10,
     . ': DEVIATORIC STRESS SET TO 0 ON INTEGRATION POINT ',I5 )
 1100 FORMAT(1X,'EXCEEDED EPS_MAX ON SOLID ELEMENT NUMBER ',I10,
     . ': DEVIATORIC STRESS SET TO 0 ON INTEGRATION POINT ',I5 ,
     .          ' AT TIME :',G11.4)  
!-----------
      RETURN
      END
