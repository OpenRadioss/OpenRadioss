Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
C|============================================================
C|  GRUNEISEN                                /eos/gruneisen.F
C|------------------------------------------------------------
C|-- appelee par -----------
C|      EOSMAIN                               /eos/eosmain.F
C|-- appelle ---------------
C|============================================================
Chd|====================================================================
Chd|  GRUNEISEN                     eos/gruneisen.F               
Chd|-- called by -----------
Chd|        EOSMAIN                       common_source/eos/eosmain.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE GRUNEISEN(IFLAG,NEL,PM   ,OFF  ,EINT ,MU   ,MU2 , 
     2                     ESPE ,DVOL ,DF   ,VNEW ,MAT  ,RHO0,
     3                     PNEW ,DPDM ,DPDE ,THETA,ECOLD)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "scr06_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MAT(NEL), IFLAG, NEL
      my_real PM(NPROPM,NUMMAT), 
     .        OFF(NEL)  ,EINT(NEL) ,MU(NEL)   , 
     .        MU2(NEL)  ,ESPE(NEL) ,DVOL(NEL) ,DF(NEL)  , 
     .        VNEW(NEL) ,RHO0(NEL) ,PNEW(NEL) ,DPDM(NEL),
     .        DPDE(NEL) ,THETA(NEL),ECOLD(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX
      my_real
     .   AA, BB, DVV, FF, FG, FAC, XX, DFF, DFG, FAC1, PP, ETA
      my_real
     .   CC(MVSIZ),S1(MVSIZ),S2(MVSIZ),S3(MVSIZ),G0(MVSIZ),GA(MVSIZ),
     .   PC(MVSIZ),SPH(MVSIZ)
C--------------------------------------------------------------------
      IF(IFLAG == 0) THEN
C-----------------------------------------
C     COMPUTE BULK MODULUS FOR SOUND SPEED
C     COMPUTE COLD COMPRESSION ENERGY
C-----------------------------------------
       DO I=1,NEL
        MX    = MAT(I)
        CC(I) = PM(33,MX)
        S1(I) = PM(34,MX)
        S2(I) = PM(160,MX)
        S3(I) = PM(161,MX)
        G0(I) = PM(35,MX)
        GA(I) = PM(36,MX)
        PC(I) = PM(37,MX)
        SPH(I)= PM(69,MX)
       ENDDO
C
       DO I=1,NEL
        FAC=ONE
        FAC1=ONE
        IF(MU(I) > ZERO) THEN
         XX= MU(I)/(ONE+MU(I))
         FF=ONE+(ONE-HALF*G0(I))*MU(I)-HALF*GA(I)*MU2(I)
         FG=ONE-(S1(I)-ONE+S2(I)*XX+S3(I)*XX*XX)*MU(I)
         FAC=FF/(FG*FG)
         DFF=ONE-HALF*G0(I)-GA(I)*MU(I)
         DFG=ONE-S1(I)+XX*(-TWO*S2(I)+XX*(S2(I)-THREE*S3(I))
     .      +TWO*S3(I)*XX*XX)
         FAC1=FAC*(ONE+MU(I)*(DFF/FF-TWO*DFG/FG))
        ENDIF
C
        AA=FAC*RHO0(I)*CC(I)*CC(I)*MU(I)
        BB=G0(I)+GA(I)*MU(I)
        PP=MAX(AA+BB*ESPE(I),PC(I))*OFF(I)
        DPDM(I)=FAC1*RHO0(I)*CC(I)*CC(I)+PP*DF(I)*DF(I)*BB+GA(I)*ESPE(I)
        DPDE(I)=BB
       ENDDO
C
       DO I=1,NEL
        ECOLD(I)=-THREE100*SPH(I)
        IF(MU(I)>ZERO) THEN
          XX  = MU(I)/(ONE+MU(I))
          ETA=ONE+MU(I)
          ECOLD(I)=ECOLD(I)*EXP((G0(I)-GA(I))*XX)*ETA**GA(I)+
     .             HALF*RHO0(I)*CC(I)*CC(I)*MU2(I)
        ENDIF
       ENDDO
C
      ELSEIF(IFLAG == 1) THEN
C----------------------------------------
C     UPDATE PRESSURE AND INTERNAL ENERGY
C----------------------------------------
       DO I=1,NEL
        MX    = MAT(I)
        CC(I) = PM(33,MX)
        S1(I) = PM(34,MX)
        S2(I) = PM(160,MX)
        S3(I) = PM(161,MX)
        G0(I) = PM(35,MX)
        GA(I) = PM(36,MX)
        PC(I) = PM(37,MX)
       ENDDO
C
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
        DVV=HALF*DVOL(I)*DF(I) / MAX(EM15,VNEW(I))
        PNEW(I)=(AA+BB*ESPE(I))/(ONE+BB*DVV)
        PNEW(I)= MAX(PNEW(I),PC(I))*OFF(I)
        EINT(I)=EINT(I) - HALF*DVOL(I)*PNEW(I)
       ENDDO
C------------------------
C     COMPUTE TEMPERATURE
C------------------------
       DO I=1,NEL
        MX     =MAT(I)
        SPH(I) =PM(69,MX)
       ENDDO
       DO I=1,NEL
        XX=-ECOLD(I)
        IF(VNEW(I)>EM15)XX=DF(I)*EINT(I)/VNEW(I)-ECOLD(I)
        IF(OFF(I)<ONE.OR.SPH(I)<=ZERO.OR.XX<ZERO) CYCLE
        THETA(I)=XX/SPH(I)
       ENDDO
       ELSEIF(IFLAG == 2) THEN
          DO I=1, NEL
             MX    =MAT(I)
             CC(I) =PM(33,MX)
             S1(I) =PM(34,MX)
             S2(I) =PM(160,MX)
             S3(I) =PM(161,MX)
             G0(I) =PM(35,MX)
             GA(I) =PM(36,MX)
             PC(I) =PM(37,MX)
             SPH(I)=PM(69,MX)
          ENDDO
C     
          DO I=1, NEL
             IF (VNEW(I) > ZERO) THEN
                FAC=ONE
                FAC1=ONE
                IF(MU(I) > ZERO) THEN
                   XX= MU(I)/(ONE+MU(I))
                   FF=ONE+(ONE-HALF*G0(I))*MU(I)-HALF*GA(I)*MU2(I)
                   FG=ONE-(S1(I)-ONE+S2(I)*XX+S3(I)*XX*XX)*MU(I)
                   FAC=FF/(FG*FG)
                   DFF=ONE-HALF*G0(I)-GA(I)*MU(I)
                   DFG=ONE-S1(I)+XX*(-TWO*S2(I)+XX*(S2(I)-THREE*S3(I))
     .                  +TWO*S3(I)*XX*XX)
                   FAC1=FAC*(ONE+MU(I)*(DFF/FF-TWO*DFG/FG))
                ENDIF
C     
                AA=FAC*RHO0(I)*CC(I)*CC(I)*MU(I)
                BB=G0(I)+GA(I)*MU(I)
                PNEW(I)=MAX(AA+BB*ESPE(I),PC(I))*OFF(I)
                DPDM(I)=FAC1*RHO0(I)*CC(I)*CC(I)+PNEW(I)*DF(I)*DF(I)*BB+GA(I)*ESPE(I)
                DPDE(I)=BB
             ENDIF
          ENDDO
      ENDIF
      RETURN
      END
