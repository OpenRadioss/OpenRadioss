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
C|  PUFF                                   /dfs/puff.F
C|------------------------------------------------------------
C|-- appelee par -----------
C|       EOSMAIN                           /matera/mmain.F
C|-- appelle ---------------
C| 
C|============================================================
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
Chd|====================================================================
Chd|  PUFF                          eos/puff.F                    
Chd|-- called by -----------
Chd|        EOSMAIN                       common_source/eos/eosmain.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PUFF(IFLAG,NEL, PM   ,OFF  ,EINT ,MU   ,MU2, 
     2                ESPE ,DVOL ,DF   ,VNEW ,MAT  ,
     3                PNEW ,DPDM ,DPDE ,THETA, ECOLD)
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
     .   OFF(MVSIZ) , EINT(MVSIZ), MU(MVSIZ)  , 
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
     .   AA, BB, DVV, ETA, XX, GX, PRES, CC, EXPA, EE
      my_real
     .   C1(MVSIZ),C2(MVSIZ),C3(MVSIZ),T1(MVSIZ),T2(MVSIZ),
     .   G0(MVSIZ),ESUBL(MVSIZ),HH(MVSIZ),
     .   PC(MVSIZ),SPH(MVSIZ)
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
        C3(I)   = PM(34,MX)
        T1(I)   = PM(35,MX)
        T2(I)   = PM(36,MX)
        PC(I)   = PM(37,MX)
        SPH(I)  = PM(69,MX)
        ESUBL(I)= PM(160,MX)
        G0(I)   = PM(161,MX)
        HH(I)   = PM(162,MX)
       ENDDO
C
       DO I=LFT,LLT
        XX =MU(I)/(ONE+MU(I))
        IF(MU(I)>=ZERO) THEN
          AA=(C1(I)+C3(I)*MU2(I))*MU(I)+C2(I)*MU2(I)
          GX=ONE-G0(I)*HALF*XX
          BB=G0(I)
          PRES=MAX(AA*GX+BB*ESPE(I),PC(I))*OFF(I)
          DPDM(I)=(C1(I)+TWO*C2(I)*MU(I)+THREE*C3(I)*MU2(I))*GX
     .            +G0(I)*DF(I)*DF(I)*(PRES-HALF*AA)
          DPDE(I)=BB
        ELSEIF(ESPE(I)<ESUBL(I))THEN
          AA=(T1(I)+T2(I)*MU(I))*MU(I)
          GX=ONE-G0(I)*HALF*XX
          BB=G0(I)
          PRES=MAX(AA*GX+BB*ESPE(I),PC(I))*OFF(I)
          DPDM(I)=(T1(I)+TWO*T2(I)*MU(I))*GX +
     .            G0(I)*DF(I)*DF(I)*(PRES-HALF*AA)
          DPDE(I)=BB
        ELSE
          ETA=ONE+MU(I)        
          EE=SQRT(ETA)
          BB=(HH(I)+(G0(I)-HH(I))*EE)*ETA
          CC= C1(I)/(G0(I)*ESUBL(I))
          EXPA=EXP(CC*XX)
          AA= BB*ESUBL(I)*(EXPA-ONE)
          PRES=MAX(AA+BB*ESPE(I),PC(I))*OFF(I)
          DPDM(I)=BB*DF(I)*DF(I)*(PRES+ESUBL(I)*EXPA*CC) +
     .       (ESPE(I)+ESUBL(I)*(EXPA-ONE))*(HH(I)+THREE_HALF*EE*(G0(I)-HH(I)))
          DPDE(I)=BB
        ENDIF
       ENDDO
C
       DO I=LFT,LLT
        ECOLD(I)=-THREE100*SPH(I)
        IF(MU(I)>ZERO) THEN
           XX  = MU(I)/(ONE+MU(I))
           ECOLD(I)=ECOLD(I)*EXP(G0(I)*XX)+HALF*C1(I)*MU2(I) 
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
        C3(I)   = PM(34,MX)
        T1(I)   = PM(35,MX)
        T2(I)   = PM(36,MX)
        PC(I)   = PM(37,MX)
        ESUBL(I)= PM(160,MX)
        G0(I)   = PM(161,MX)
        HH(I)   = PM(162,MX)
       ENDDO
C
       DO I=LFT,LLT
        DVV=HALF*DVOL(I)*DF(I) / MAX(EM15,VNEW(I))
        XX =MU(I)/(ONE+MU(I))
C
        IF(MU(I)>=ZERO) THEN
          AA=(C1(I)+C3(I)*MU2(I))*MU(I)+C2(I)*MU2(I)
          AA=AA*(ONE-G0(I)*HALF*XX)
          BB=G0(I)
        ELSEIF(ESPE(I)<ESUBL(I))THEN
          AA=(T1(I)+T2(I)*MU(I))*MU(I)
          AA=AA*(ONE-G0(I)*HALF*XX)
          BB=G0(I)
        ELSE
          ETA=ONE+MU(I)        
          BB=(HH(I)+(G0(I)-HH(I))*SQRT(ETA))*ETA
          CC= C1(I)/(G0(I)*ESUBL(I))
          EXPA=EXP(CC*XX)
          AA= BB*ESUBL(I)*(EXPA-ONE)
        ENDIF
C
        PNEW(I)= (AA +BB*ESPE(I))/(ONE+ BB*DVV)
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
        XX=DF(I)*EINT(I)/VNEW(I)-ECOLD(I)
        IF(OFF(I)<1..OR.SPH(I)<=0..OR.XX<0.) CYCLE
        THETA(I)=XX/SPH(I)
       ENDDO
C
      ELSEIF(IFLAG == 2) THEN
         DO I=1, NEL
            MX      = MAT(I)
            C1(I)   = PM(32,MX)
            C2(I)   = PM(33,MX)
            C3(I)   = PM(34,MX)
            T1(I)   = PM(35,MX)
            T2(I)   = PM(36,MX)
            PC(I)   = PM(37,MX)
            SPH(I)  = PM(69,MX)
            ESUBL(I)= PM(160,MX)
            G0(I)   = PM(161,MX)
            HH(I)   = PM(162,MX)
         ENDDO
C     
         DO I=1, NEL
            IF (VNEW(I) > ZERO) THEN
               XX =MU(I)/(ONE+MU(I))
               IF(MU(I)>=ZERO) THEN
                  AA=(C1(I)+C3(I)*MU2(I))*MU(I)+C2(I)*MU2(I)
                  GX=ONE-G0(I)*HALF*XX
                  BB=G0(I)
                  PRES=MAX(AA*GX+BB*ESPE(I),PC(I))*OFF(I)
                  DPDM(I)=(C1(I)+TWO*C2(I)*MU(I)+THREE*C3(I)*MU2(I))*GX
     .                 +G0(I)*DF(I)*DF(I)*(PRES-HALF*AA)
                  DPDE(I)=BB
               ELSEIF(ESPE(I)<ESUBL(I))THEN
                  AA=(T1(I)+T2(I)*MU(I))*MU(I)
                  GX=ONE-G0(I)*HALF*XX
                  BB=G0(I)
                  PRES=MAX(AA*GX+BB*ESPE(I),PC(I))*OFF(I)
                  DPDM(I)=(T1(I)+TWO*T2(I)*MU(I))*GX +
     .                 G0(I)*DF(I)*DF(I)*(PRES-HALF*AA)
                  DPDE(I)=BB
               ELSE
                  ETA=ONE+MU(I)        
                  EE=SQRT(ETA)
                  BB=(HH(I)+(G0(I)-HH(I))*EE)*ETA
                  CC= C1(I)/(G0(I)*ESUBL(I))
                  EXPA=EXP(CC*XX)
                  AA= BB*ESUBL(I)*(EXPA-ONE)
                  PRES=MAX(AA+BB*ESPE(I),PC(I))*OFF(I)
                  DPDM(I)=BB*DF(I)*DF(I)*(PRES+ESUBL(I)*EXPA*CC) +
     .                 (ESPE(I)+ESUBL(I)*(EXPA-ONE))*(HH(I)+THREE_HALF*EE*(G0(I)-HH(I)))
                  DPDE(I)=BB
               ENDIF
               PNEW(I) = PRES
            ENDIF
         ENDDO
      ENDIF
      RETURN
      END
