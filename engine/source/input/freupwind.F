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
Chd|  FREUPWIND                     source/input/freupwind.F      
Chd|-- called by -----------
Chd|        FREFORM                       source/input/freform.F        
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        WRIUSC2                       source/input/wriusc2.F        
Chd|        ALE_MOD                       ../common_source/modules/ale/ale_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE FREUPWIND(IKAD,KEY0,KUPWM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE ALE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IKAD(0:*),KUPWM
      CHARACTER KEY0(*)*5
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IKEY,N,MID
      CHARACTER KEY2*5
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C  This subrouting is reading updated coefficient eta1 eta2 eta3
C  initially defined with starter card /UPWIND or from previous run
C  with engine card /UPWIND
C  Additional details
         ! ALE%UPWIND%UPWSM2 : READ HERE BUT UPDATED IN LECTUR.F FOR ALE PRINTOUT UPWSM=UPWSM2
         ! ALE%UPWIND%UPWMG  : READ HERE BUT UPDATED IN LECTUR.F FOR ALE PRINTOUT PM(15,IMAT)=UPWMG2                                 
         ! ALE%UPWIND%UPWOG  : READ HERE BUT UPDATED IN LECTUR.F FOR ALE PRINTOUT PM(16,IMAT)=UPWOG2
C
         ! ALE%UPWIND%UPW_UPDATE == 1 : ENGINE /UPWIND CARD DETECTED
         ! ALE%UPWIND%UPW_UPDATE == 2 : /UPWIND CARD IS CHANGING AT LEAST ONE PARAMETER
         ! ALE%UPWIND%UPW_UPDATE modified in lectur.F (checking change for 1st and 2nd parameter)
C-----------------------------------------------
      ALE%UPWIND%UPW_UPDATE = 0  !now initialized in freform.F because SUPG is default option
      ALE%UPWIND%UPWMG2     = 0  !now initialized in freform.F because SUPG is default option
      ALE%UPWIND%UPWOG2     = 0
      ALE%UPWIND%UPWSM2     = 0
      IKEY=KUPWM
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
       CALL WRIUSC2(IKAD(IKEY)+1,1,KEY0(IKEY))
       READ(IUSC2,*,ERR=9990)ALE%UPWIND%UPWMG2, ALE%UPWIND%UPWOG2, ALE%UPWIND%UPWSM2

       ALE%UPWIND%UPW_UPDATE = 1       

       IF(ALE%UPWIND%UPWMG2==ZERO)ALE%UPWIND%UPWMG2=ONE 
       IF(ALE%UPWIND%UPWOG2==ZERO)ALE%UPWIND%UPWOG2=ONE 
       IF(ALE%UPWIND%UPWSM2==ZERO)ALE%UPWIND%UPWSM2=ONE 

       !Upwind coefficient for Momentum Advection
       IF(ALE%UPWIND%UPWMG2<=ZERO .OR. ALE%UPWIND%UPWMG2>ONE)THEN
         WRITE(ISTDO,*)' ** ERROR IN CARD /UPWIND'
         WRITE(ISTDO,*)'     MOMENTUM COEFFICIENT IS OUT OF BOUNDS [0,1]'
         CALL ARRET(2)          
       ENDIF

       !Upwind coefficient for Mass and Energy Advection
       IF(ALE%UPWIND%UPWOG2<ZERO.OR.ALE%UPWIND%UPWOG2>ONE)THEN
         WRITE(ISTDO,*)' ** ERROR IN CARD /UPWIND'
         WRITE(ISTDO,*)'     MASS & ENERGY COEFFICIENT IS OUT OF BOUNDS [0,1]'
         CALL ARRET(2)          
       ENDIF
       
       !Upwind coefficient for Wet Surface (Mulimaterial specific advection)
       IF(ALE%UPWIND%UPWSM2<-ONE.OR.ALE%UPWIND%UPWSM2>ONE)THEN
         WRITE(ISTDO,*)' ** ERROR IN CARD /UPWIND'
         WRITE(ISTDO,*)'     WET SURFACE COEFFICIENT IS OUT OF BOUNDS [-1,1]'          
         CALL ARRET(2) 
       ENDIF

      ENDIF

      RETURN

 9990 CONTINUE
      CALL ANCMSG(MSGID=73,ANMODE=ANINFO,C1=KEY0(IKEY))
      CALL ARRET(0)
      END
