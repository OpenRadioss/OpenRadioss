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
C|============================================================
C|  TILLOTSON                              /dfs/tillotson.F
C|------------------------------------------------------------
C|-- appelee par -----------
C|       EOSMAIN                           /matera/mmain.F
C|-- appelle ---------------
C| 
C|============================================================
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
Chd|====================================================================
Chd|  TILLOTSON                     eos/tillotson.F               
Chd|-- called by -----------
Chd|        EOSMAIN                       common_source/eos/eosmain.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TILLOTSON(IFLAG,NEL, PM   ,OFF  ,EINT ,MU   ,MU2, 
     2                     ESPE ,DVOL ,DF   ,VNEW ,MAT  ,
     3                     PNEW ,DPDM ,DPDE ,THETA, ECOLD)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MAT(MVSIZ), IFLAG, NEL
      my_real
     .   PM(NPROPM,*), 
     .   OFF(MVSIZ) , EINT(MVSIZ), VOL0(MVSIZ), MU(MVSIZ)   , 
     .   MU2(MVSIZ) , ESPE(MVSIZ), DVOL(MVSIZ), DF(MVSIZ)   , 
     .   VNEW(MVSIZ), PNEW(MVSIZ), DPDM(MVSIZ), DPDE(MVSIZ) ,
     .   THETA(MVSIZ),ECOLD(MVSIZ)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "scr06_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX
      my_real
     .   AA, BB, DVV, ETA, ENEW, OMEGA, XX, EXPA, EXPB,
     .   PP, FACC1, FACC2, FACPB, GA
      my_real
     .   C1(MVSIZ),C2(MVSIZ),PTIA(MVSIZ),PTIB(MVSIZ),EZERO(MVSIZ),
     .   ALPHA(MVSIZ),BETA(MVSIZ),ESUBL(MVSIZ),VSUBL(MVSIZ),
     .   PC(MVSIZ), SPH(MVSIZ)
C--------------------------------------------------------------------    
      IF(IFLAG == 0) THEN
C-----------------------------------------
C     COMPUTE BULK MODULUS FOR SOUND SPEED
C     COMPUTE COLD ENERGY
C-----------------------------------------
       DO I=LFT,LLT
        MX      = MAT(I)
        C1(I)   = PM(32,MX)
        C2(I)   = PM(33,MX)
        PTIA(I) = PM(34,MX)
        PTIB(I) = PM(35,MX)
        PC(I)   = PM(37,MX)
        SPH(I)  = PM(69,MX)
        EZERO(I)= PM(36,MX)
        ESUBL(I)= PM(160,MX)
        VSUBL(I)= PM(161,MX)
        ALPHA(I)= PM(162,MX)
        BETA(I) = PM(163,MX)
       ENDDO
C
       DO I=LFT,LLT
        FACC1=ONE
        FACC2=ONE
        FACPB=ONE
        IF(MU(I)<ZERO) THEN
          FACC2=ZERO
          IF(DF(I)> VSUBL(I).OR.
     .      (DF(I)<=VSUBL(I).AND.ESPE(I)>=ESUBL(I))) THEN
           XX  = MU(I)/(ONE+MU(I))
           EXPA= EXP(-ALPHA(I)*XX*XX)
           EXPB= EXP(BETA(I)*XX)
           FACC1=EXPA*EXPB
           FACPB=EXPA
          ENDIF
        ENDIF
C        
        ETA=ONE+MU(I)        
        OMEGA= ONE+ESPE(I)/(EZERO(I)*ETA**2)
        AA=FACC1*C1(I)*MU(I)+FACC2*C2(I)*MU2(I)
        BB=PTIA(I)+FACPB*PTIB(I)/OMEGA
        PP=MAX(AA+BB*ETA*ESPE(I),PC(I))*OFF(I)
        DPDM(I)=FACC1*C1(I)+TWO*FACC2*C2(I)*MU(I)+BB*ETA*PP*DF(I)*DF(I)
     .         +ESPE(I)*( BB+(TWO*ESPE(I)/ETA-PP*DF(I)*DF(I))
     .         *PTIB(I)*FACPB/(EZERO(I)*ETA*OMEGA**2) )
        DPDE(I)=BB*ETA
       ENDDO
C
       DO I=LFT,LLT
        ECOLD(I)=-THREE100*SPH(I)
        IF(MU(I)>ZERO) THEN
           XX  = MU(I)/(ONE+MU(I))
           ETA=ONE+MU(I)
           GA=PTIA(I)+PTIB(I)-TWO_THIRD
           ECOLD(I)=ECOLD(I)*EXP(GA*XX)*ETA**TWO_THIRD+
     .              HALF*C1(I)*MU2(I)
        ENDIF
       ENDDO
C
      ELSEIF(IFLAG == 1) THEN
C----------------------------------------
C     UPDATE PRESSURE AND INTERNAL ENERGY
C----------------------------------------
       DO I=LFT,LLT
        MX      = MAT(I)
        C1(I)   = PM(32,MX)
        C2(I)   = PM(33,MX)
        PTIA(I) = PM(34,MX)
        PTIB(I) = PM(35,MX)
        PC(I)   = PM(37,MX)
        EZERO(I)= PM(36,MX)
        ESUBL(I)= PM(160,MX)
        VSUBL(I)= PM(161,MX)
        ALPHA(I)= PM(162,MX)
        BETA(I) = PM(163,MX)
       ENDDO
C
       DO I=LFT,LLT
        DVV=HALF*DVOL(I)*DF(I) / MAX(EM15,VNEW(I))
        ETA=ONE+MU(I)        
        OMEGA= ONE+ESPE(I)/(EZERO(I)*ETA**2)
C
        FACC1=ONE
        FACC2=ONE
        FACPB=ONE
        IF(MU(I)<ZERO) THEN
         FACC2=ZERO
         IF(DF(I)>VSUBL(I).OR.
     .     (DF(I)<=VSUBL(I).AND.ESPE(I)>=ESUBL(I))) THEN       
           XX  = MU(I)/(ONE+MU(I))
           EXPA= EXP(-ALPHA(I)*XX*XX)
           EXPB= EXP(BETA(I)*XX)
           FACC1=EXPA*EXPB
           FACPB=EXPA
         ENDIF
        ENDIF
C
        AA=FACC1*C1(I)*MU(I)+FACC2*C2(I)*MU2(I)
        BB=(PTIA(I)+FACPB*PTIB(I)/OMEGA)*ETA 
        PNEW(I) = (AA +BB*ESPE(I))/(ONE+ BB*DVV)
        ENEW = ESPE(I) - PNEW(I)*DVV
C       ONE ITERATION
        OMEGA= ONE+ENEW/(EZERO(I)*ETA**2)
        BB=(PTIA(I)+FACPB*PTIB(I)/OMEGA)*ETA        
        PNEW(I) = (AA +BB*ESPE(I))/(ONE+ BB*DVV)
        PNEW(I)= MAX(PNEW(I),PC(I))*OFF(I)
        EINT(I)= EINT(I) - HALF*DVOL(I)*PNEW(I)
       ENDDO
C------------------------
C     COMPUTE TEMPERATURE
C------------------------
       DO I=LFT,LLT
        MX     =MAT(I)
        SPH(I) =PM(69,MX)
       ENDDO
       DO I=LFT,LLT
        XX=-ECOLD(I)
        IF(VNEW(I)>EM15)XX=DF(I)*EINT(I)/VNEW(I)-ECOLD(I)
        IF(OFF(I)<ONE.OR.SPH(I)<=ZERO.OR.XX<ZERO) CYCLE
        THETA(I)=XX/SPH(I)
       ENDDO
C
      ELSEIF(IFLAG == 2) THEN
         DO I=1, NEL
            MX      = MAT(I)
            C1(I)   = PM(32,MX)
            C2(I)   = PM(33,MX)
            PTIA(I) = PM(34,MX)
            PTIB(I) = PM(35,MX)
            PC(I)   = PM(37,MX)
            SPH(I)  = PM(69,MX)
            EZERO(I)= PM(36,MX)
            ESUBL(I)= PM(160,MX)
            VSUBL(I)= PM(161,MX)
            ALPHA(I)= PM(162,MX)
            BETA(I) = PM(163,MX)
         ENDDO
C     
         DO I=1, NEL
            IF (VNEW(I) > ZERO) THEN
               FACC1=ONE
               FACC2=ONE
               FACPB=ONE
               IF(MU(I)<ZERO) THEN
                  FACC2=ZERO
                  IF(DF(I)> VSUBL(I).OR.
     .                 (DF(I)<=VSUBL(I).AND.ESPE(I)>=ESUBL(I))) THEN
                     XX  = MU(I)/(ONE+MU(I))
                     EXPA= EXP(-ALPHA(I)*XX*XX)
                     EXPB= EXP(BETA(I)*XX)
                     FACC1=EXPA*EXPB
                     FACPB=EXPA
                  ENDIF
               ENDIF
C     
               ETA=ONE+MU(I)        
               OMEGA= ONE+ESPE(I)/(EZERO(I)*ETA**2)
               AA=FACC1*C1(I)*MU(I)+FACC2*C2(I)*MU2(I)
               BB=PTIA(I)+FACPB*PTIB(I)/OMEGA
               PP=MAX(AA+BB*ETA*ESPE(I),PC(I))*OFF(I)
               DPDM(I)=FACC1*C1(I)+TWO*FACC2*C2(I)*MU(I)+BB*ETA*PP*DF(I)*DF(I)
     .              +ESPE(I)*( BB+(TWO*ESPE(I)/ETA-PP*DF(I)*DF(I))
     .              *PTIB(I)*FACPB/(EZERO(I)*ETA*OMEGA**2) )
               DPDE(I)=BB*ETA
            ENDIF
         ENDDO        
      ENDIF
      RETURN
      END
