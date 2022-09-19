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
Chd|====================================================================
      SUBROUTINE BOLTST (IBOLTP, IP, BPRELD, SIG, TT, NEL, SENSOR, LSENSOR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C
      INTEGER IBOLTP,NEL,IP
      INTEGER LSENSOR(NSENSI,*)
      my_real
     .   TT, DT1
      my_real
     .   BPRELD(NEL,*),SIG(NEL,6),SENSOR(NSENSR,*)
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
       DO I=LFT,LLT
        ISENS = INT(BPRELD(I,10))
        IF (ISENS > 0)THEN
         DO K=1,NSENSOR
          IF(ISENS==LSENSOR(1,K))GOTO 100
         ENDDO
100      BPRELD(I,10)=K
        ENDIF
       ENDDO
      ENDIF

       DO I=LFT,LLT
        TS = BPRELD(I,1)
        TE = BPRELD(I,2)
        IF(BPRELD(I,3) == ZERO)THEN
          ISENS = INT(BPRELD(I,10))

          IF (ISENS > 0) THEN
           TS = TS+SENSOR(2,ISENS)
           TE = TE+SENSOR(2,ISENS)
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
       IF(BPRELD(LFT,3) == ONE)THEN
         TS = BPRELD(LFT,1)
         TE = BPRELD(LFT,2)
!         IF (TT <= (TE+EM01*(TE-TS))) THEN       ! sb : seems to be an error
         IF (TT <= (TS+EM01*(TE-TS))) THEN
          DO I=LFT,LLT
            SIG(I,1) = BPRELD(I,4)
            SIG(I,2) = BPRELD(I,5)
            SIG(I,3) = BPRELD(I,6)
            SIG(I,4) = BPRELD(I,7)
            SIG(I,5) = BPRELD(I,8)
            SIG(I,6) = BPRELD(I,9)
          ENDDO
         ELSE IF (TT >= TE) THEN
          DO I=LFT,LLT
            BPRELD(I,3) = TWO
          ENDDO
         ENDIF
       ENDIF


      RETURN
      END

