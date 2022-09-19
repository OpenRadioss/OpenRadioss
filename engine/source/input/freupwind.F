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
Chd|====================================================================
Chd|  FREUPWIND                     source/input/freupwind.F      
Chd|-- called by -----------
Chd|        FREFORM                       source/input/freform.F        
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        WRIUSC2                       source/input/wriusc2.F        
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE FREUPWIND(IKAD,KEY0,KUPWM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
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
#include      "scr17_c.inc"
#include      "com07_c.inc"
#include      "com04_c.inc"
#include      "warn_c.inc"
#include      "scrupw_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
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
C  Additionnal details
         ! UPWSM2 : READ HERE BUT UPDATED IN LECTUR.F FOR ALE PRINTOUT UPWSM=UPWSM2
         ! UPWMG  : READ HERE BUT UPDATED IN LECTUR.F FOR ALE PRINTOUT PM(15,IMAT)=UPWMG2
         ! UPWOG  : READ HERE BUT UPDATED IN LECTUR.F FOR ALE PRINTOUT PM(16,IMAT)=UPWOG2
C
         ! UPW_UPDATE == 1 : ENGINE /UPWIND CARD DETECTED
         ! UPW_UPDATE == 2 : /UPWIND CARD IS CHANGING AT LEAST ONE PARAMETER
         ! UPW_UPDATE modified in lectur.F (checking change for 1st and 2nd parameter)
C-----------------------------------------------
      UPW_UPDATE = 0  !now initialized in freform.F because SUPG is default option
      UPWMG2     = 0  !now initialized in freform.F because SUPG is default option
      UPWOG2     = 0
      UPWSM2     = 0
      IKEY=KUPWM
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
!       READ(IUSC1,REC=IKAD(IKEY),FMT='(7X,A)',ERR=9990)KEY2
       CALL WRIUSC2(IKAD(IKEY)+1,1,KEY0(IKEY))
       READ(IUSC2,*,ERR=9990)UPWMG2, UPWOG2, UPWSM2

       UPW_UPDATE = 1

       IF(UPWMG2==ZERO)UPWMG2=ONE
       IF(UPWOG2==ZERO)UPWOG2=ONE
       IF(UPWSM2==ZERO)UPWSM2=ONE

       !Upwind coefficient for Momentum Advection
       IF(UPWMG2<=ZERO.OR.UPWMG2>ONE)THEN
         WRITE(ISTDO,*)
     .    ' ** ERROR IN CARD /UPWIND'
         WRITE(ISTDO,*)
     .    '     MOMENTUM COEFFICIENT IS OUT OF BOUNDS [0,1]'
         CALL ARRET(2)
       ENDIF

       !Upwind coefficient for Mass and Energy Advection
       IF(UPWOG2<ZERO.OR.UPWOG2>ONE)THEN
         WRITE(ISTDO,*)
     .    ' ** ERROR IN CARD /UPWIND'
         WRITE(ISTDO,*)
     .    '     MASS & ENERGY COEFFICIENT IS OUT OF BOUNDS [0,1]'
         CALL ARRET(2)
       ENDIF

       !Upwind coefficient for Wet Surface (Mulimaterial specific advection)
       IF(UPWSM2<-ONE.OR.UPWSM2>ONE)THEN
         WRITE(ISTDO,*)
     .   ' ** ERROR IN CARD /UPWIND'
         WRITE(ISTDO,*)
     .   '     WET SURFACE COEFFICIENT IS OUT OF BOUNDS [-1,1]'
         CALL ARRET(2)
       ENDIF

      ENDIF

      RETURN

 9990 CONTINUE
      CALL ANCMSG(MSGID=73,ANMODE=ANINFO,
     .            C1=KEY0(IKEY))
      CALL ARRET(0)
      END
