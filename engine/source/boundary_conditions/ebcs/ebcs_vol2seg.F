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
!||    ebcs_vol2seg   ../engine/source/boundary_conditions/ebcs/ebcs_vol2seg.F
!||--- called by ------------------------------------------------------
!||    ebcs_main      ../engine/source/boundary_conditions/ebcs/ebcs_main.F
!||--- uses       -----------------------------------------------------
!||    ale_mod        ../common_source/modules/ale/ale_mod.F
!||    segvar_mod     ../engine/share/modules/segvar_mod.F
!||====================================================================
      SUBROUTINE EBCS_VOL2SEG(NSEG,SURF_NODES,ISEG,SEGVAR,
     .                   A,V,X,VOLMON,FSAV)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SEGVAR_MOD
      USE ALE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSEG,ISEG(NSEG),SURF_NODES(NSEG,4)
      my_real
     .        A(3,*),X(3,*),V(3,*),VOLMON(*),FSAV(*)
      TYPE(t_segvar) :: SEGVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IS,K,KSEG,SEGAD,IRECT(4)
      my_real ORIENT,
     .        PEXT,PRES,VOL,RO,EN,
     .        FAC,X13,Y13,Z13,X24,Y24,Z24,NX,NY,NZ,S,
     .        VN1,VN2,VN3,VN4,VEL,VN,VPLUS,VMOINS,
     .        GAM1,GAMRP,VCRT2,FLUXM,FLUXE,FLUX,
     .        GV,RV,RSR,PN,ENIN,ROIN,ENOU,ROOU,PNS,FX,FY,FZ,
     .        GAM,NORM,DMIN,DMOU,HIN,HOU,PNA
      my_real PNA1,PNA2,PNA3,PNA4,RSR1,RSR2,RSR3,RSR4,AIRE
C=======================================================================
C VOLUME MONITORE
      GAM   =VOLMON(1)
      PEXT  =VOLMON(3)
      PRES  =VOLMON(12)
      VOL   =VOLMON(16)
      RO    =VOLMON(20)/VOL
C volumetric Energy
      EN    =VOLMON(13)/VOL
C massic Enthalpy
      HIN   = (EN+PRES)/RO

      GAM1 = ONE/(GAM-ONE)
      GAMRP= HALF*(GAM-ONE)/GAM
      VCRT2= TWO*GAM*PRES/RO/(GAM+ONE)

      FLUX =ZERO
      FLUXM=ZERO
      FLUXE=ZERO
      AIRE =ZERO

      DO IS=1,NSEG
        DO K=1,4
          IRECT(K)=SURF_NODES(IS,K)
        ENDDO
        IF(IRECT(4) == 0) THEN
          IRECT(4)=IRECT(3)
          FAC=THIRD
        ELSE
          FAC=FOURTH
        ENDIF
C
C INDICES FOR SEGMENTS, AND OREINTATIONS
C
        KSEG=ABS(ISEG(IS))
        ORIENT=FLOAT(ISEG(IS)/KSEG)

C NORMAL VECTORS
        X13=X(1,IRECT(3))-X(1,IRECT(1))
        Y13=X(2,IRECT(3))-X(2,IRECT(1))
        Z13=X(3,IRECT(3))-X(3,IRECT(1))
        X24=X(1,IRECT(4))-X(1,IRECT(2))
        Y24=X(2,IRECT(4))-X(2,IRECT(2))
        Z24=X(3,IRECT(4))-X(3,IRECT(2))
        NX=Y13*Z24-Z13*Y24
        NY=Z13*X24-X13*Z24
        NZ=X13*Y24-Y13*X24
        NORM=SQRT(NX*NX+NY*NY+NZ*NZ)
        S=HALF*NORM
        IF(NORM>EM20)THEN
          NORM=ORIENT/NORM
          NX=NX*NORM
          NY=NY*NORM
          NZ=NZ*NORM
        ENDIF
        

C NORMAL VELOCITIES
c        write(6,*)iseg(is),nx,ny,nz
        VN1=V(1,IRECT(1))*NX+V(2,IRECT(1))*NY+V(3,IRECT(1))*NZ
        VN2=V(1,IRECT(2))*NX+V(2,IRECT(2))*NY+V(3,IRECT(2))*NZ
        VN3=V(1,IRECT(3))*NX+V(2,IRECT(3))*NY+V(3,IRECT(3))*NZ
        VN4=V(1,IRECT(4))*NX+V(2,IRECT(4))*NY+V(3,IRECT(4))*NZ
        IF(IRECT(4) == IRECT(3))THEN
          VN=FAC*(VN1+VN2+VN3)
        ELSE
          VN=FAC*(VN1+VN2+VN3+VN4)
        ENDIF
        VPLUS =MAX(VN,ZERO)
        VMOINS=MIN(VN,ZERO)
C
C STAGNATION POINT
C
        VEL=MIN(VN1,ZERO)**2
        VEL=MIN(VEL,VCRT2)
        GV=GAMRP*RO*VEL/PRES
        IF(GV>EM4)THEN
         RSR    = (ONE-GV)**GAM1
         PNA1     = PRES*RSR**GAM
        ELSE
         RV     = HALF*RO*VEL
         RSR    = ONE - RV/(PRES*GAM)
         PNA1     = PRES - RV
        ENDIF
        RSR1=RSR
C
        VEL=MIN(VN2,ZERO)**2
        VEL=MIN(VEL,VCRT2)
        GV=GAMRP*RO*VEL/PRES
        IF(GV>EM4)THEN
         RSR    = (ONE-GV)**GAM1
         PNA2     = PRES*RSR**GAM
        ELSE
         RV     = HALF*RO*VEL
         RSR    = ONE - RV/(PRES*GAM)
         PNA2     = PRES - RV
        ENDIF
        RSR2=RSR
C
        VEL=MIN(VN3,ZERO)**2
        VEL=MIN(VEL,VCRT2)
        GV=GAMRP*RO*VEL/PRES
        IF(GV>EM4)THEN
         RSR    = (ONE-GV)**GAM1
         PNA3     = PRES*RSR**GAM
        ELSE
         RV     = HALF*RO*VEL
         RSR    = ONE - RV/(PRES*GAM)
         PNA3     = PRES - RV
        ENDIF
        RSR3=RSR
C
        VEL=MIN(VN4,ZERO)**2
        VEL=MIN(VEL,VCRT2)
        GV=GAMRP*RO*VEL/PRES
        IF(GV>EM4)THEN
         RSR    = (ONE-GV)**GAM1
         PNA4     = PRES*RSR**GAM
        ELSE
         RV     = HALF*RO*VEL
         RSR    = ONE - RV/(PRES*GAM)
         PNA4     = PRES - RV
        ENDIF
        RSR4=RSR
C
        IF(IRECT(4) == IRECT(3))THEN
          PNA=FAC*(PNA1+PNA2+PNA3)
          RSR=FAC*(RSR1+RSR2+RSR3)
        ELSE
          PNA=FAC*(PNA1+PNA2+PNA3+PNA4)
          RSR=FAC*(RSR1+RSR2+RSR3+RSR4)
        ENDIF
C INCOMING FLOW
        ENIN = PNA*GAM1
        ROIN = RSR*RO
C        ROIN =RO
C        ENIN =EN
C OUTGOING FLOW
        SEGAD=ALE%GLOBAL%NVCONV*(KSEG-1)
        ROOU = SEGVAR%RHO(KSEG)
        ENOU = SEGVAR%EINT(KSEG)

        SEGVAR%RHO(KSEG)=ROIN
        SEGVAR%EINT(KSEG)=ENIN
C
C MASS FLUX AND ENTHALPY TO AIRBAGS (>0 si rentrant)
C airbag -> fluid 
C hin  =  (Ea+Pa)/roa 
C fluide -> airbag 
C hou = (Ef+P)/rof +1/2 v+^2
C        HOU=(ENOU+PNA)/ROOU+0.5*VPLUS**2
c
        HOU=GAM*ENOU/ROOU+HALF*VPLUS**2
        DMIN=ROIN*VMOINS
        DMOU=ROOU*VPLUS        
        AIRE =AIRE +S
        FLUX =FLUX +S*VN
        FLUXM=FLUXM+S*(DMOU+DMIN)
        FLUXE=FLUXE+S*(HIN*DMIN+HOU*DMOU)

C RELATIVE PRESSURE
        PN  = PNA1-PEXT
        PNS=-PN*S*FAC
        FX=PNS*NX
        FY=PNS*NY
        FZ=PNS*NZ

        A(1,IRECT(1))=A(1,IRECT(1))+FX
        A(2,IRECT(1))=A(2,IRECT(1))+FY
        A(3,IRECT(1))=A(3,IRECT(1))+FZ

        PN  = PNA2-PEXT
        PNS=-PN*S*FAC
        FX=PNS*NX
        FY=PNS*NY
        FZ=PNS*NZ
        A(1,IRECT(2))=A(1,IRECT(2))+FX
        A(2,IRECT(2))=A(2,IRECT(2))+FY
        A(3,IRECT(2))=A(3,IRECT(2))+FZ

        PN  = PNA3-PEXT
        PNS=-PN*S*FAC
        FX=PNS*NX
        FY=PNS*NY
        FZ=PNS*NZ
        A(1,IRECT(3))=A(1,IRECT(3))+FX
        A(2,IRECT(3))=A(2,IRECT(3))+FY
        A(3,IRECT(3))=A(3,IRECT(3))+FZ

        IF(IRECT(4)/=IRECT(3))THEN
        PN  = PNA4-PEXT
        PNS=-PN*S*FAC
        FX=PNS*NX
        FY=PNS*NY
        FZ=PNS*NZ
          A(1,IRECT(4))=A(1,IRECT(4))+FX
          A(2,IRECT(4))=A(2,IRECT(4))+FY
          A(3,IRECT(4))=A(3,IRECT(4))+FZ
        ENDIF

      ENDDO

       VOLMON(22)=VOLMON(22) - FLUXE
       VOLMON(24)=VOLMON(24) - FLUXM
       FLUX=-FLUX+FSAV(9)*FSAV(8)
       FSAV(8)=FSAV(8)      + AIRE
       FSAV(9)=FLUX/FSAV(8)
C-----------
      RETURN
      END
