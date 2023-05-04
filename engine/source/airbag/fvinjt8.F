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
Chd|  FVINJT8                       source/airbag/fvinjt8.F       
Chd|-- called by -----------
Chd|        FVBAG1                        source/airbag/fvbag1.F        
Chd|        FV_UP_SWITCH                  source/airbag/fv_up_switch.F  
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE FVINJT8(NJET   , IBAGJET , RBAGJET , NPC    , TF,
     2                   NSENSOR,SENSOR_TAB, SCALT,    IGEO,     GEO,     
     3                   PM     , IVOLU,   DATAINJ )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------  
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NSENSOR
      INTEGER NJET, IBAGJET(NIBJET,*), NPC(*), IGEO(NPROPGI,*), IVOLU(*)
C     REAL
      my_real
     .   RBAGJET(NRBJET,*), TF(*),SCALT, 
     .   GEO(NPROPG,*), PM(NPROPM,*), DATAINJ(6,NJET)
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I_INJ, I_TYPINJ, NGASES, ISENS, IFLU, IMASS, ITEMP, IMOL, 
     .   I_GAS, IJ, IK, IL, IMASS1, I_GAS1, IMOL1, KK
      my_real
     .   TSTART, ASTIME, DGMASSI, GMTOT_OLD, GMASS_OLD, GMOL_OLD, FMASS, 
     .   FTEMP,  MOLFR,  GMASS,   GMASS_DT,  GMTOT,     DGMASS,   EFAC , 
     .   TSG,    DYDX,  RMWG,  TEMP,  
     .   CPA, CPB, CPC, CPD, CPE, CPF, MW, 
     .   CPA_NEW, CPB_NEW, CPC_NEW, CPD_NEW, CPE_NEW, CPF_NEW,MWMIX_NEW,
     .   DMASS_MIX, DMOL_MIX, DMASSG_MIX, DTEPS, MASS_MIX, MASS_MIX_DT, 
     .   MWMIX, FMASS1, MW1, MOLFR1, R_IGC1, DGMOUT
C
      my_real
     .         FINTER
      EXTERNAL FINTER
C-----------------------------------------------
      R_IGC1   =PM(27,IVOLU(66))
C---------------------
C     INJECTEURS
C---------------------
       DO IJ=1,NJET
        I_INJ    = IBAGJET(13,IJ)
        I_TYPINJ = IGEO(22,I_INJ)
        NGASES   = IGEO(23,I_INJ)
C
        ISENS    = IBAGJET(4,IJ)
        IF(ISENS==0)THEN
         TSTART=ZERO
        ELSE
         TSTART=SENSOR_TAB(ISENS)%TSTART
        ENDIF
        IFLU =IGEO(24,I_INJ)
        ASTIME = GEO(201,I_INJ)
        DGMASSI=ZERO
        CPA_NEW=ZERO
        CPB_NEW=ZERO
        CPC_NEW=ZERO
        CPD_NEW=ZERO
        CPE_NEW=ZERO
        CPF_NEW=ZERO
        GMTOT_OLD=RBAGJET(8,IJ)
        GMASS_OLD=RBAGJET(7,IJ)
        GMOL_OLD=RBAGJET(20,IJ)
C
        DO IK=1,NGASES
         IF (I_TYPINJ==1) THEN
           FMASS=GEO(208+(IK-1)*2+1,I_INJ)
           IF (IFLU==1)FMASS=FMASS/ASTIME
           FTEMP=GEO(208+(IK-1)*2+2,I_INJ)
           IMASS=IGEO(100+(IK-1)*3+2,I_INJ)
           ITEMP=IGEO(100+(IK-1)*3+3,I_INJ)
         ELSE IF (I_TYPINJ==2) THEN
           FMASS=GEO(209,I_INJ)
           IF (IFLU==1)FMASS=FMASS/ASTIME
           FTEMP=GEO(210,I_INJ)
           MOLFR=GEO(211+(IK-1)+1,I_INJ)
           IMASS=IGEO(25,I_INJ)
           ITEMP=IGEO(26,I_INJ)
           IMOL =IGEO(100+(IK-1)*2+2,I_INJ)
         END IF
C
         IF (I_TYPINJ==1) THEN
           I_GAS=IGEO(100+(IK-1)*3+1,I_INJ)
         ELSE IF (I_TYPINJ==2) THEN
           I_GAS=IGEO(100+(IK-1)*2+1,I_INJ)
         END IF
         MW   =PM(20,I_GAS)
         CPA  =PM(21,I_GAS)
         CPB  =PM(22,I_GAS)
         CPC  =PM(23,I_GAS)
         CPD  =PM(24,I_GAS)
         CPE  =PM(25,I_GAS)
         CPF  =PM(26,I_GAS)
         IF(TT>=TSTART)THEN
          TSG = (TT-TSTART)/ASTIME
C
          IF(ITEMP>0) THEN
            TEMP=FTEMP*FINTER(ITEMP,TSG,NPC,TF,DYDX)
          ELSE
            TEMP=FTEMP
          ENDIF
          EFAC= TEMP*(CPA+HALF*CPB*TEMP+THIRD*CPC*TEMP*TEMP
     .                +FOURTH*CPD*TEMP*TEMP*TEMP
     .                -CPE/(TEMP*TEMP)
     .                +ONE_FIFTH*CPF*TEMP*TEMP*TEMP*TEMP)
         ELSE
          EFAC=ZERO
         ENDIF
C
         DMASS_MIX=ZERO 
         DMOL_MIX=ZERO
         DMASSG_MIX=ZERO

         IF(TT>=TSTART)THEN
            DTEPS=DT1/ASTIME
            IF (DT1==ZERO) DTEPS=EM15
C
            IF (I_TYPINJ==1) THEN
C --- calcul masse  Cette boucle devrait etre faite en dehors de la boucle IK
              DO IL=1,NGASES
C
               I_GAS1=IGEO(100+(IL-1)*3+1,I_INJ)
               IMASS1=IGEO(100+(IL-1)*3+2,I_INJ)
               FMASS1=GEO(208+(IL-1)*2+1,I_INJ)
               IF (IFLU==1) FMASS1=FMASS1/ASTIME
               MW1=PM(20,I_GAS1)
C
               IF(IMASS1>0) THEN
                 GMASS=FMASS1*FINTER(IMASS1,TSG,NPC,TF,DYDX)
               ELSE
                 GMASS=FMASS1
               END IF
C
               IF (IFLU==1) THEN
                DMASS_MIX=DMASS_MIX+GMASS
                DMOL_MIX=DMOL_MIX+GMASS/MW1
               ELSEIF (IFLU==0) THEN
                IF(IMASS1>0) THEN
                  GMASS_DT=FMASS1*FINTER(IMASS1,TSG+DTEPS,NPC,TF,DYDX)
                ELSE
                  GMASS_DT=FMASS1
                END IF
                DMASS_MIX=DMASS_MIX+(GMASS_DT-GMASS)
                DMOL_MIX=DMOL_MIX+(GMASS_DT-GMASS)/MW1
               END IF
              END DO ! end boucle sur IL
C
              IF(IMASS>0) THEN
                GMASS=FMASS*FINTER(IMASS,TSG,NPC,TF,DYDX)
              ELSE
                GMASS=FMASS
              END IF
              DMASSG_MIX=GMASS
              IF (IFLU==0) THEN
                IF(IMASS>0) THEN
                  GMASS_DT=FMASS*FINTER(IMASS,TSG+DTEPS,NPC,TF,DYDX)
                ELSE
                  GMASS_DT=FMASS
                END IF
                DMASSG_MIX=GMASS_DT-GMASS
              END IF
            ELSE IF (I_TYPINJ==2) THEN
C --- MASS_MIX correspond sert uniquement au calcul de GMASS
C --- c est un cumul pour retrouver la fonction d input de masse
C
C --- MASS_MIX ne peut etre nulle car somme des fractions molaire = 1
C --- et les MWs sont strictement positifs
C
C --- DMASS_MIX qui sert uniquement au calcul de CP[A-F]
              MASS_MIX=ZERO
              MASS_MIX_DT=ZERO
              DO IL=1,NGASES
C
               I_GAS1=IGEO(100+(IL-1)*2+1,I_INJ)
               IMASS1=IGEO(25,I_INJ)
               FMASS1=GEO(209,I_INJ)
               IF (IFLU==1) FMASS1=FMASS1/ASTIME
               MW1=PM(20,I_GAS1)
C
               IMOL1=IGEO(100+(IL-1)*2+2,I_INJ)
               MOLFR1=GEO(211+(IL-1)+1,I_INJ)
C
               IF (IMOL1==0) THEN
                GMASS=MOLFR1
     .                *MW1
               ELSE
                GMASS=MOLFR1
     .                *FINTER(IMOL1,TSG,NPC,TF,DYDX)
     .                *MW1
               END IF
               IF (IFLU==1) THEN
                 DMASS_MIX=DMASS_MIX+GMASS
                 DMOL_MIX=DMOL_MIX+GMASS/MW1
C
                 MASS_MIX=MASS_MIX+GMASS
C
               ELSEIF (IFLU==0) THEN
                 IF (IMOL1==0) THEN
                  GMASS_DT=MOLFR1
     .                     *MW1
                 ELSE
                  GMASS_DT=MOLFR1
     .                     *FINTER(IMOL1,TSG+DTEPS,NPC,TF,DYDX)
     .                     *MW1
                 END IF
C --- la fraction molaire n est pas un debit
C --- pas de recalcul de DMASS_MIX avec DTEPS
                 DMASS_MIX=DMASS_MIX+GMASS
                 DMOL_MIX=DMOL_MIX+GMASS/MW1
                 MASS_MIX=MASS_MIX+GMASS
                 MASS_MIX_DT=MASS_MIX_DT+GMASS_DT
C
               END IF
              END DO
C --- Somme de masse (masse x masse_molaire)/masse_mixture
              IF (IMOL==0) THEN
                GMASS=FMASS*MOLFR
     .                     *MW
              ELSE
                GMASS=FMASS*MOLFR
     .                     *FINTER(IMOL,TSG,NPC,TF,DYDX)
     .                     *MW
              END IF
              GMASS=GMASS
     .              *FINTER(IMASS,TSG,NPC,TF,DYDX)/MASS_MIX
              DMASS_MIX=FMASS*DMASS_MIX
     .              *FINTER(IMASS,TSG,NPC,TF,DYDX)/MASS_MIX
              DMOL_MIX =FMASS*DMOL_MIX
     .              *FINTER(IMASS,TSG,NPC,TF,DYDX)/MASS_MIX
              DMASSG_MIX=GMASS
              IF (IFLU==0) THEN
               IF (IMOL==0) THEN
                 GMASS_DT=FMASS*MOLFR
     .                         *MW
               ELSE
                 GMASS_DT=FMASS*MOLFR
     .                         *FINTER(IMOL,TSG+DTEPS,NPC,TF,DYDX)
     .                         *MW
               END IF
C --- la fraction molaire n est pas un debit
C --- pas de recalcul de DMASSG_MIX avec DTEPS
               GMASS_DT=GMASS_DT*FINTER(IMASS,TSG+DTEPS,NPC,TF,DYDX)/MASS_MIX_DT
              END IF
            END IF
C
            IF (DMASS_MIX/=ZERO)THEN
             MWMIX_NEW     =DMASS_MIX/DMOL_MIX
             CPA_NEW =CPA_NEW +CPA*DMASSG_MIX/DMASS_MIX
             CPB_NEW =CPB_NEW +CPB*DMASSG_MIX/DMASS_MIX
             CPC_NEW =CPC_NEW +CPC*DMASSG_MIX/DMASS_MIX
             CPD_NEW =CPD_NEW +CPD*DMASSG_MIX/DMASS_MIX
             CPE_NEW =CPE_NEW +CPE*DMASSG_MIX/DMASS_MIX
             CPF_NEW =CPF_NEW +CPF*DMASSG_MIX/DMASS_MIX
            END IF
C
            KK=20+(IK-1)*4
            IF(IFLU==1) THEN
              GMASS_OLD = RBAGJET(KK+1,IJ)
              GMASS =GMASS*DT1 + GMASS_OLD
              DGMASS=MAX(ZERO,GMASS-GMASS_OLD)
            ELSE
              DGMASS=MAX(ZERO,GMASS_DT-GMASS)
            END IF
            RBAGJET(KK+1,IJ) = RBAGJET(KK+1,IJ) + DGMASS
            RBAGJET(KK+2,IJ) = RBAGJET(KK+2,IJ) + DGMASS
            DGMASSI=DGMASSI+DGMASS
         ELSE
          DGMASS=ZERO
          GMASS =ZERO
         ENDIF !TT >= TSTART
C
         IF(DMASS_MIX/=ZERO) THEN
           DATAINJ(4,IJ)=DATAINJ(4,IJ)+EFAC*DMASSG_MIX/DMASS_MIX
         ENDIF
C
        END DO
C Fin de boucle sur les gas inject s
C
        IF (DT1>ZERO) THEN
           DATAINJ(2,IJ)=DGMASSI/DT1
        ELSE
           DATAINJ(2,IJ)=ZERO
        ENDIF
C
        IF (DMASS_MIX/=ZERO)THEN
         MWMIX=MWMIX_NEW
         RBAGJET(2 ,IJ)=CPA_NEW
         RBAGJET(3 ,IJ)=CPB_NEW
         RBAGJET(4 ,IJ)=CPC_NEW
         RBAGJET(16,IJ)=CPD_NEW
         RBAGJET(17,IJ)=CPE_NEW
         RBAGJET(18,IJ)=CPF_NEW
        END IF
C
        CPA = RBAGJET(2,IJ)
        CPB = RBAGJET(3,IJ)
        CPC = RBAGJET(4,IJ)
        CPD = RBAGJET(16,IJ)
        CPE = RBAGJET(17,IJ)
        CPF = RBAGJET(18,IJ)
        IF (DMASS_MIX/=ZERO)THEN
          RMWG=R_IGC1/MWMIX
          RBAGJET(1,IJ)=RMWG
        ELSE
          RMWG=RBAGJET(1,IJ)
        END IF
C
        GMTOT =GMTOT_OLD+DGMASSI
C
        RBAGJET(1,IJ)=RMWG
        RBAGJET(8,IJ)=GMTOT
        RBAGJET(7,IJ)=RBAGJET(7,IJ)+DGMASSI
        RBAGJET(20,IJ)=DMOL_MIX
       ENDDO
C fin de boucle sur injecteurs
C------
        RETURN
        END
