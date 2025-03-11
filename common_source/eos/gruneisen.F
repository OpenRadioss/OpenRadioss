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
      !||    gruneisen   ../common_source/eos/gruneisen.F
      !||--- called by ------------------------------------------------------
      !||    eosmain     ../common_source/eos/eosmain.F
      !||====================================================================
      SUBROUTINE GRUNEISEN(IFLAG , NEL    ,PM   ,OFF   ,EINT  ,MU   ,MU2 ,
     2                     ESPE  , DVOL   ,DF   ,VNEW  ,MAT   ,RHO0,
     3                     PNEW  , DPDM   ,DPDE ,PSH   ,
     4                     NUMMAT, NPROPM )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
!----------------------------------------------------------------------------
!! \details STAGGERED SCHEME IS EXECUTED IN TWO PASSES IN EOSMAIN : IFLG=0 THEN IFLG=1
!! \details COLLOCATED SCHEME IS DOING A SINGLE PASS : IFLG=2
!! \details
!! \details  STAGGERED SCHEME
!! \details     EOSMAIN / IFLG = 0 : DERIVATIVE CALCULATION FOR SOUND SPEED ESTIMATION c[n+1] REQUIRED FOR PSEUDO-VISCOSITY (DPDE:partial derivative, DPDM:total derivative)
!! \details     MQVISCB            : PSEUDO-VISCOSITY Q[n+1]
!! \details     MEINT              : INTERNAL ENERGY INTEGRATION FOR E[n+1] : FIRST PART USING P[n], Q[n], and Q[n+1] CONTRIBUTIONS
!! \details     EOSMAIN / IFLG = 1 : UPDATE P[n+1], T[N+1]
!! \details                          INTERNAL ENERGY INTEGRATION FOR E[n+1] : LAST PART USING P[n+1] CONTRIBUTION
!! \details                            (second order integration dE = -P.dV where P = 0.5(P[n+1] + P[n]) )
!! \details  COLLOCATED SCHEME
!! \details     EOSMAIN / IFLG = 2 : SINGLE PASS FOR P[n+1] AND DERIVATIVES
!----------------------------------------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: NUMMAT            !< number of material laws  (size for PM array)
      INTEGER,INTENT(IN) :: NPROPM            !< size for PM array
      INTEGER,INTENT(IN) :: IFLAG             !< flag for the operation (staggered scheme IFLAG=0,1 ; collocated scheme IFLAG=2)
      INTEGER,INTENT(IN) :: NEL               !< number of elements in the current group
      INTEGER,INTENT(IN) :: MAT(NEL)          !< material identifier of each element
      my_real,INTENT(IN) :: PM(NPROPM,NUMMAT) !< material buffer
      my_real,INTENT(IN) :: OFF(NEL)          !< OFF=1 if element is active, OFF=0 if element is inactive
      my_real,INTENT(IN) :: MU(NEL)           !< volumetric strain : rho/rho0-1
      my_real,INTENT(IN) :: MU2(NEL)          !< MU**2 IF MU > 0 , 0 otherwise
      my_real,INTENT(IN) :: ESPE(NEL)         !< specific internal energy
      my_real,INTENT(IN)  :: DVOL(NEL)        !< volume change
      my_real,INTENT(IN) :: VNEW(NEL)         !< current volume
      my_real,INTENT(IN) :: RHO0(NEL)         !< reference density
      my_real,INTENT(IN) :: DF(NEL)           !< relative volume (v/v0 = rho0/rho)
      my_real,INTENT(INOUT) :: EINT(NEL)      !< internal energy
      my_real,INTENT(INOUT) :: PSH(NEL)       !< pressure shift (for relative pressure modeling)
      my_real,INTENT(INOUT) :: PNEW(NEL)      !< current pressure
      my_real,INTENT(INOUT) :: DPDE(NEL)      !< partial derivative dP/dE where E=Eint/V0
      my_real,INTENT(INOUT) :: DPDM(NEL)      !< total derivative : DP/Dmu = dPdmu + dPdE* P/(1+mu)**2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX
      my_real AA, BB, DVV, FF, FG, FAC, XX, DFF, DFG, FAC1, PP
      my_real CC(NEL),S1(NEL),S2(NEL),S3(NEL),G0(NEL),GA(NEL),PC(NEL)
C-----------------------------------------------
C   B o d y
C-----------------------------------------------
      IF(IFLAG == 0) THEN !--- SOUND SPEED DERIVATIVE (DPDE is partial derivative, DPDM is total derivative)
       ! user parameters
       DO I=1,NEL
        MX = MAT(I)
        CC(I) = PM(33,MX)
        S1(I) = PM(34,MX)
        S2(I) = PM(160,MX)
        S3(I) = PM(161,MX)
        G0(I) = PM(35,MX)
        GA(I) = PM(36,MX)
        PC(I) = PM(37,MX)
        PSH(I) = PM(88,MX)
       ENDDO

       ! Derivative
       DO I=1,NEL
        FAC=ONE
        FAC1=ONE
        IF(MU(I) > ZERO) THEN
         XX= MU(I)/(ONE+MU(I))
         FF=ONE+(ONE-HALF*G0(I))*MU(I)-HALF*GA(I)*MU2(I)
         FG=ONE-(S1(I)-ONE+S2(I)*XX+S3(I)*XX*XX)*MU(I)
         FAC=FF/(FG*FG)
         DFF=ONE-HALF*G0(I)-GA(I)*MU(I)
         DFG=ONE-S1(I)+XX*(-TWO*S2(I)+XX*(S2(I)-THREE*S3(I))+TWO*S3(I)*XX*XX)
         FAC1=FAC*(ONE+MU(I)*(DFF/FF-TWO*DFG/FG))
        ENDIF
        AA=FAC*RHO0(I)*CC(I)*CC(I)*MU(I)
        BB=G0(I)+GA(I)*MU(I)
        PP=MAX(AA+BB*ESPE(I),PC(I))*OFF(I) ! total pressure need for sound speed calculation (=> no pressure shift)
        DPDM(I)=FAC1*RHO0(I)*CC(I)*CC(I)+PP*DF(I)*DF(I)*BB+GA(I)*ESPE(I)
        DPDE(I)=BB
       ENDDO

      ELSEIF(IFLAG == 1) THEN  !--- P[n+1] and EINT[n+1]
       ! user parameters
       DO I=1,NEL
        MX = MAT(I)
        CC(I) = PM(33,MX)
        S1(I) = PM(34,MX)
        S2(I) = PM(160,MX)
        S3(I) = PM(161,MX)
        G0(I) = PM(35,MX)
        GA(I) = PM(36,MX)
        PC(I) = PM(37,MX)
        PSH(I) = PM(88,MX)
       ENDDO

       ! Pressure P[n+1]
       ! Internal Energy Eint[n+1] (2nd order integration requires P[n+1])
       DO I=1,NEL
        FAC=ONE
        IF(MU(I) > ZERO) THEN
         XX= MU(I)/(ONE+MU(I))
         FF=ONE+(ONE-HALF*G0(I))*MU(I)-HALF*GA(I)*MU2(I)
         FG=ONE-(S1(I)-ONE+S2(I)*XX+S3(I)*XX*XX)*MU(I)
         FAC=FF/(FG*FG)
        ENDIF
        AA=FAC*RHO0(I)*CC(I)*CC(I)*MU(I)
        BB=G0(I)+GA(I)*MU(I)
        DPDE(I) = BB
        DVV=HALF*DVOL(I)*DF(I) / MAX(EM15,VNEW(I))
        PNEW(I)=(AA+BB*ESPE(I))/(ONE+BB*DVV)
        PNEW(I)= MAX(PNEW(I),PC(I))*OFF(I)
        EINT(I)=EINT(I) - HALF*DVOL(I)*PNEW(I) !total pressure for energy integration
        PNEW(I) = PNEW(I) - PSH(I)
       ENDDO

      ELSEIF(IFLAG == 2) THEN  !--- collocated scheme (law151)
        ! user parameters
        DO I=1, NEL
          MX = MAT(I)
          CC(I) = PM(33,MX)
          S1(I) = PM(34,MX)
          S2(I) = PM(160,MX)
          S3(I) = PM(161,MX)
          G0(I) = PM(35,MX)
          GA(I) = PM(36,MX)
          PC(I) = PM(37,MX)
          PSH(I) = PM(88,MX)
        ENDDO
        ! pressure and sound speed derivatives
        DO I=1,NEL
          IF (VNEW(I) > ZERO) THEN
            FAC=ONE
            FAC1=ONE
            IF(MU(I) > ZERO) THEN
               XX= MU(I)/(ONE+MU(I))
               FF=ONE+(ONE-HALF*G0(I))*MU(I)-HALF*GA(I)*MU2(I)
               FG=ONE-(S1(I)-ONE+S2(I)*XX+S3(I)*XX*XX)*MU(I)
               FAC=FF/(FG*FG)
               DFF=ONE-HALF*G0(I)-GA(I)*MU(I)
               DFG=ONE-S1(I)+XX*(-TWO*S2(I)+XX*(S2(I)-THREE*S3(I))+TWO*S3(I)*XX*XX)
               FAC1=FAC*(ONE+MU(I)*(DFF/FF-TWO*DFG/FG))
            ENDIF
            AA=FAC*RHO0(I)*CC(I)*CC(I)*MU(I)
            BB=G0(I)+GA(I)*MU(I)
            PNEW(I)=MAX(AA+BB*ESPE(I),PC(I))*OFF(I)
            DPDM(I)=FAC1*RHO0(I)*CC(I)*CC(I)+PNEW(I)*DF(I)*DF(I)*BB+GA(I)*ESPE(I)
            DPDE(I)=BB
            PNEW(I)=PNEW(I)-PSH(I)
          ENDIF
        ENDDO

      ENDIF

      RETURN
      END
