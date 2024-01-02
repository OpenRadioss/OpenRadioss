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
Chd|====================================================================
Chd|  BOLTST                        source/elements/solid/solide/boltst.F
Chd|-- called by -----------
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|        S8CFORC3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE BOLTST(IP      ,BPRELD   ,SIG       ,TT        ,        
     .                  NEL     ,NPT      ,NSENSOR   ,SENSOR_TAB)
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
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NPT,NSENSOR,NEL,IP
      my_real
     .   TT, DT1
      my_real
     .   BPRELD(NEL,*),SIG(NEL,6)
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) ,INTENT(IN) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,ISENS 
      my_real
     .   TS,TE
C-----------------------------------------------
C
!     Retrieve sysid of the sensors at cycle=0
      IF (NCYCLE == 0 .AND. IP == 1) THEN
       DO I=1,NEL
        ISENS = INT(BPRELD(I,10))
        IF (ISENS > 0)THEN            
         DO K=1,NSENSOR
          IF(ISENS==SENSOR_TAB(K)%SENS_ID) GOTO 100  ! do it in staerter !!!
         ENDDO
100      BPRELD(I,10)=K
        ENDIF
       ENDDO
      ENDIF
      
       DO I=1,NEL
        TS = BPRELD(I,1)
        TE = BPRELD(I,2)
        IF(BPRELD(I,3) == ZERO)THEN
          ISENS = INT(BPRELD(I,10))
        
          IF (ISENS > 0) THEN
           TS = TS+SENSOR_TAB(ISENS)%TSTART
           TE = TE+SENSOR_TAB(ISENS)%TSTART
          ENDIF
        ENDIF

        IF(BPRELD(I,3) < ONE)THEN
          IF(TT >= TS )THEN
           SIG(I,1) = BPRELD(I,4) !SIG(I,1)+BPRELD(I,4)
           SIG(I,2) = BPRELD(I,5) !SIG(I,2)+BPRELD(I,5)
           SIG(I,3) = BPRELD(I,6) !SIG(I,3)+BPRELD(I,6)
           SIG(I,4) = BPRELD(I,7) !SIG(I,4)+BPRELD(I,7)
           SIG(I,5) = BPRELD(I,8) !SIG(I,5)+BPRELD(I,8)
           SIG(I,6) = BPRELD(I,9) !SIG(I,6)+BPRELD(I,9)
           BPRELD(I,3) = REAL(IP)/REAL(NPT)
           
!          Added to shift all the preloading duration in case of sensor use        
           BPRELD(I,1) = TS
           BPRELD(I,2) = TE
          ENDIF
        ENDIF
       ENDDO
      
! Added to compensate loss of stress when filling the gap
       IF(BPRELD(1,3) == ONE)THEN
         TS = BPRELD(1,1)
         TE = BPRELD(1,2)
!         IF (TT <= (TE+EM01*(TE-TS))) THEN       ! sb : seems to be an error 
         IF (TT <= (TS+EM01*(TE-TS))) THEN
          DO I=1,NEL
            SIG(I,1) = BPRELD(I,4)
            SIG(I,2) = BPRELD(I,5)
            SIG(I,3) = BPRELD(I,6)
            SIG(I,4) = BPRELD(I,7)
            SIG(I,5) = BPRELD(I,8)
            SIG(I,6) = BPRELD(I,9)
          ENDDO
         ELSE IF (TT >= TE) THEN
          DO I=1,NEL
            BPRELD(I,3) = TWO
          ENDDO
         ENDIF
       ENDIF

       
      RETURN
      END

