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
Chd|  DT37LAW                       source/materials/time_step/dt37law.F
Chd|-- called by -----------
Chd|        DTMAIN                        source/materials/time_step/dtmain.F
Chd|-- calls ---------------
Chd|        DTEL                          source/materials/time_step/dtel.F
Chd|        DTSPH                         source/materials/time_step/dtsph.F
Chd|====================================================================
      SUBROUTINE DT37LAW (PM,GEO,PID,MAT, BUFMAT, IPM, DELTAX, AIRE, VOL, DTX)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C     ELEMENTARY TIME STEPS FOR ALE MULTI MATERIAL LAW 37 (/MAT/BIPHAS)
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
#include      "param_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real, INTENT(IN) :: PM(NPROPM, *), GEO(NPROPG, *), AIRE(*), VOL(*), DTX(*)
      my_real, INTENT(IN), DIMENSION(:), TARGET :: BUFMAT(*), DELTAX(*)
      INTEGER, INTENT(IN) :: PID(*),MAT(*),IPM(NPROPMI, *)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER                                :: I, MX, IADBUF,IFLG(MVSIZ)
      my_real,DIMENSION(:),POINTER           :: UPARAM
      
      my_real
     .   SSP(MVSIZ)  , DPDM(MVSIZ) , RHO0(MVSIZ)    , 
     .   BULK , C1   ,  P  ,        
     .   MAS(MVSIZ), MU1P1(MVSIZ),MU1P2(MVSIZ),RHO1(MVSIZ),RHO2(MVSIZ),
     .   GAM,RHO10,RHO20,P0,VFRAC1,VFRAC2,UVAR1,A1,R1,VISA1,VISB1,VISA2,VISB2,
     .   B1,B2,MAS1,MAS2,SSP1,SSP2,VIS(MVSIZ),A
     
      INTEGER ISOLVER

     
c-----------------------------------------------
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

      IADBUF   = IPM(7,MAT(1))
      UPARAM   =>BUFMAT(IADBUF:IADBUF+17)
      ISOLVER  = UPARAM(17)
      A1       = UPARAM(10) !mfrac_liq
      RHO10    = UPARAM(11)
      RHO20    = UPARAM(12)
      R1       = UPARAM(6)
      GAM      = UPARAM(5)
      P0       = UPARAM(9)
      C1       = UPARAM(4)
      VISA1    = UPARAM(1)
      VISB1    = UPARAM(3)
      VISA2    = UPARAM(13)
      VISB2    = UPARAM(15)
    
       DO I=LFT,LLT
        RHO0(I)= RHO10 * A1 + (ONE-A1)*RHO20
        MAS(I)= RHO0(I)*VOL(I)
      ENDDO

      DO I=LFT,LLT
       IF(GAM*C1>=EM30)THEN !if Liquid and gas correctly defined
         RHO1(I) = RHO10
         RHO2(I) = RHO20
       ENDIF 
      ENDDO
                
      IF (ISOLVER==2) THEN
        DO I=LFT,LLT
          MAS1 = A1 * VOL(I)
          IF (A1 < EM10)THEN
            MAS1=ZERO
            VFRAC1=ZERO
            VFRAC2=ONE
          ENDIF
          IF (ONE-A1 < EM10)THEN
            MAS2=ZERO
            RHO1=MAS(I)/VOL(I)
            VFRAC1=ONE
            VFRAC2=ZERO
          ENDIF
          A = (RHO0(I)-RHO2(I))/(RHO1(I)-RHO2(I))
          VFRAC1=A
          IF(VFRAC1<EM20)VFRAC1=ZERO
          VFRAC2 = ONE-VFRAC1
          SSP1=C1
          SSP2=GAM*P0*(RHO2(I)/RHO20)**GAM
          UVAR1=A*RHO1(I)
          IF(SSP1>ZERO)THEN
            SSP1 = VFRAC1 / SSP1
          ELSE
            SSP1=ZERO
          ENDIF
          IF(SSP2>ZERO)THEN
            SSP2 = VFRAC2 / SSP2
          ELSE
            SSP2=ZERO
          ENDIF
          SSP(I) = SSP1 + SSP2
          SSP(I) = SQRT(ONE / SSP(I) / RHO0(I))
          B1=UVAR1
          B2=RHO0(I)-B1
          VIS(I)=(B1*RHO1(I)*VISA1 + B2*RHO2(I)*VISA2)/RHO0(I)
        
        ENDDO
      ELSE
        DO I=LFT,LLT
          SSP(I)=SQRT(R1)
          VIS(I)=ZERO
        ENDDO
      ENDIF 

      
      !----------------------------------------------!
      !  ELEMENTARY TIME STEP (ARTIFICIAL VISCOSITY) !
      !----------------------------------------------!
      IF(JSPH==0)THEN
       CALL DTEL(SSP,PM,GEO,PID,MAT, RHO0, VIS, DELTAX, AIRE, VOL, DTX)
      ELSE
       CALL DTSPH(SSP,PM,GEO,PID,MAT, RHO0, VIS, DELTAX, VOL, DTX)
      ENDIF
C-----------
      RETURN
      END
