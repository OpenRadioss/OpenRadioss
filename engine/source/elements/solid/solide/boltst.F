Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||    boltst       ../engine/source/elements/solid/solide/boltst.F
!||--- called by ------------------------------------------------------
!||    s10forc3     ../engine/source/elements/solid/solide10/s10forc3.F
!||    s4forc3      ../engine/source/elements/solid/solide4/s4forc3.F
!||    s8cforc3     ../engine/source/elements/thickshell/solide8c/s8cforc3.F
!||    s8eforc3     ../engine/source/elements/solid/solide8e/s8eforc3.F
!||    s8zforc3     ../engine/source/elements/solid/solide8z/s8zforc3.F
!||    sforc3       ../engine/source/elements/solid/solide/sforc3.F
!||    szforc3      ../engine/source/elements/solid/solidez/szforc3.F
!||--- uses       -----------------------------------------------------
!||    sensor_mod   ../common_source/modules/sensor_mod.F90
!||====================================================================
      SUBROUTINE BOLTST(IP      ,BPRELD   ,SIG       ,TT        ,        
     .                  NEL     ,NPT      ,NSENSOR   ,SENSOR_TAB,
     .                  FUN_ID  ,SENS_ID  )
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
      INTEGER, INTENT(IN) :: NPT,NSENSOR,NEL,IP,FUN_ID,SENS_ID
      my_real
     .   TT
      my_real
     .   BPRELD(NEL,*),SIG(NEL,6)
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) ,INTENT(IN) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ISENS 
      my_real
     .   TS,TE,SFAC
C-----------------------------------------------
!!! see preload_solid_ini.F90
      
       DO I=1,NEL
        TS = BPRELD(I,1)
        TE = BPRELD(I,2)
        IF(BPRELD(I,3) == ZERO)THEN
          ISENS = SENS_ID
        
          IF (ISENS > 0) THEN
           TS = TS+SENSOR_TAB(ISENS)%TSTART
           TE = TE+SENSOR_TAB(ISENS)%TSTART
          ENDIF
        ENDIF

        IF(BPRELD(I,3) < ONE)THEN
          IF(TT >= TS )THEN
           SFAC = BPRELD(I,10)
           SIG(I,1) = SFAC*BPRELD(I,4) !SIG(I,1)+BPRELD(I,4)
           SIG(I,2) = SFAC*BPRELD(I,5) !SIG(I,2)+BPRELD(I,5)
           SIG(I,3) = SFAC*BPRELD(I,6) !SIG(I,3)+BPRELD(I,6)
           SIG(I,4) = SFAC*BPRELD(I,7) !SIG(I,4)+BPRELD(I,7)
           SIG(I,5) = SFAC*BPRELD(I,8) !SIG(I,5)+BPRELD(I,8)
           SIG(I,6) = SFAC*BPRELD(I,9) !SIG(I,6)+BPRELD(I,9)
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
         IF(FUN_ID == 0)THEN
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
         ELSE ! ramping inital stress need more time to reach the pick
           IF (TT < TE) THEN
             SFAC = BPRELD(1,10)
             SIG(1:NEL,1:6) = SFAC*BPRELD(1:NEL,4:9)
           ELSE 
             BPRELD(1:NEL,3) = TWO
           ENDIF
         ENDIF
       END IF !(BPRELD(1,3) == ONE)THEN

       
      RETURN
      END

