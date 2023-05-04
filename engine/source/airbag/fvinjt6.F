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
Chd|  FVINJT6                       source/airbag/fvinjt6.F       
Chd|-- called by -----------
Chd|        FVBAG1                        source/airbag/fvbag1.F        
Chd|        FV_UP_SWITCH                  source/airbag/fv_up_switch.F  
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE FVINJT6(NJET   , IBAGJET , RBAGJET , NPC    , TF      ,
     2                   NSENSOR,SENSOR_TAB, SCALT,    DATAINJ )
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
      INTEGER NJET, IBAGJET(NIBJET,*), NPC(*)
C     REAL
      my_real
     .   RBAGJET(NRBJET,*), TF(*),SCALT,DATAINJ(6,NJET)
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) ,INTENT(IN) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IINJ, IMASS, IFLU, ISENS, ITEMP
      my_real
     .   TSTART, FMASS, GMASS, GMASS_OLD, GMTOT, GMTOT_OLD, DGMASS,
     .   TSG,    DYDX,  RMWG,  FTEMP,  TEMP,  EFAC , 
     .   CPA,    CPB,   CPC,   CPG,    CVG
C
      my_real
     .         FINTER
      EXTERNAL FINTER
C-----------------------------------------------
      DO IINJ=1,NJET
         FMASS=RBAGJET(5,IINJ)
         GMASS_OLD=RBAGJET(7,IINJ)
         GMTOT_OLD=RBAGJET(8,IINJ)
         IMASS=IBAGJET(1,IINJ)
         IFLU =IBAGJET(2,IINJ)
         ISENS=IBAGJET(4,IINJ)
         IF(ISENS==0)THEN
          TSTART=ZERO
         ELSE
          TSTART=SENSOR_TAB(ISENS)%TSTART
         ENDIF
         IF (TT>=TSTART.AND.DT1>ZERO)THEN
            TSG=(TT-TSTART)*SCALT
            IF (IMASS>0) THEN
               GMASS=FMASS*FINTER(IMASS,TSG,NPC,TF,DYDX)
               IF(IFLU==1)GMASS = GMASS*SCALT*DT1 + GMASS_OLD
            ELSE
               GMASS=FMASS
            ENDIF
            DGMASS=MAX(ZERO,GMASS-GMASS_OLD)
         ELSE
            DGMASS=ZERO
            GMASS=ZERO
         ENDIF
         GMTOT=GMTOT_OLD+DGMASS
         IF (DT1>ZERO) THEN
            DATAINJ(2,IINJ)=DGMASS/DT1
         ELSE
            DATAINJ(2,IINJ)=ZERO
         ENDIF
C------------
C Temperature
C------------
         RMWG =RBAGJET(1,IINJ)
         CPA  =RBAGJET(2,IINJ)
         CPB  =RBAGJET(3,IINJ)
         CPC  =RBAGJET(4,IINJ)
         FTEMP=RBAGJET(6,IINJ)
         ITEMP=IBAGJET(3,IINJ)
         EFAC =ZERO
         TEMP =ZERO
         IF(TT>=TSTART)THEN
            TSG = (TT-TSTART)*SCALT
            IF(ITEMP>0) THEN
              TEMP=FTEMP*FINTER(ITEMP,TSG,NPC,TF,DYDX)
            ELSE
              TEMP=FTEMP
            ENDIF
            EFAC= TEMP*(CPA+HALF*CPB*TEMP+THIRD*CPC*TEMP*TEMP)
         ENDIF
C 
         IF (IBAGJET(12,IINJ)==2) EFAC=EFAC-RMWG*TEMP
C
         DATAINJ(4,IINJ)=EFAC
         CPG=CPA+CPB*TEMP+CPC*TEMP*TEMP
         CVG=CPG-RMWG
         DATAINJ(5,IINJ)=CPG/CVG
C
         RBAGJET(7,IINJ)=GMASS
         RBAGJET(8,IINJ)=GMTOT
      ENDDO
      RETURN
      END
