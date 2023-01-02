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
Chd|  FVTEMP                        source/airbag/fvtemp.F        
Chd|-- called by -----------
Chd|        FVBAG1                        source/airbag/fvbag1.F        
Chd|        FVUPD1                        source/airbag/fvupd.F         
Chd|        FV_UP_SWITCH                  source/airbag/fv_up_switch.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FVTEMP(ITYP  , EFAC , CPA  , CPB  , CPC  ,
     .                  CPD   , CPE  , CPF  , RMW  , TEMP0,
     .                  TEMP  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITYP
      my_real
     .        EFAC, CPA, CPB, CPC, CPD, CPE, CPF, RMW, TEMP0, TEMP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K
      my_real CVA, HH, TEMP2
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IF(ITYP == 6) THEN
         CVA=CPA-RMW
         IF(CPC == ZERO) THEN
            IF(CPB == ZERO) THEN
               TEMP=EFAC/CVA
            ELSE
               TEMP=(SQRT(CVA*CVA+TWO*CPB*EFAC)-CVA)/CPB
            ENDIF
         ELSE
            TEMP=TEMP0
            DO K=1,5
               HH=CPA+HALF*CPB*TEMP+THIRD*CPC*TEMP*TEMP
               TEMP=EFAC/(HH-RMW)
            ENDDO
         ENDIF
      ELSEIF(ITYP == 8) THEN
         TEMP=TEMP0
         DO K=1,5
            TEMP2=TEMP*TEMP
            HH=CPA+HALF*CPB*TEMP+THIRD*CPC*TEMP2+
     .         FOURTH*CPD*TEMP*TEMP2-CPE/TEMP2+ONE_FIFTH*CPF*TEMP2*TEMP2   
            IF( (HH-RMW) > ZERO) THEN
              TEMP=EFAC/(HH-RMW)
            ELSE
              TEMP=ZERO
              RETURN
            ENDIF
         ENDDO
      ENDIF
      RETURN
      END 