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
C
Chd|====================================================================
Chd|  S8ZTEMPEL                     source/elements/solid/solide8z/s8ztempel.F
Chd|-- called by -----------
Chd|        S8CFORC3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8ZTEMPEL(LFT,LLT,NI,KSI,ETA,ZETA,
     1                     NC1,NC2,NC3,NC4,NC5,NC6,NC7,NC8,
     2                     TEMP,TEMPEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT,LLT,NC1(*), NC2(*), NC3(*), NC4(*), 
     .        NC5(*), NC6(*), NC7(*), NC8(*)
C     REAL
      my_real
     .   KSI,ETA,ZETA,NI(*),TEMP(*),TEMPEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   RP,SP,TP,SM,RM,TM
C-----------------------------------------------
C
C isoparametric shape function
C
      RP = ONE + KSI            
      SP = ONE + ETA            
      TP = ONE + ZETA           
      RM = ONE - KSI            
      SM = ONE - ETA            
      TM = ONE - ZETA
      NI(1) = ONE_OVER_8*RM*SM*TM
      NI(2) = ONE_OVER_8*RP*SM*TM
      NI(3) = ONE_OVER_8*RP*SP*TM
      NI(4) = ONE_OVER_8*RM*SP*TM
      NI(5) = ONE_OVER_8*RM*SM*TP
      NI(6) = ONE_OVER_8*RP*SM*TP
      NI(7) = ONE_OVER_8*RP*SP*TP
      NI(8) = ONE_OVER_8*RM*SP*TP
      DO I=LFT,LLT
         TEMPEL(I)=   NI(1)*TEMP(NC1(I)) +  NI(2)*TEMP(NC2(I)) +
     .                NI(3)*TEMP(NC3(I)) +  NI(4)*TEMP(NC4(I)) +
     .                NI(5)*TEMP(NC5(I)) +  NI(6)*TEMP(NC6(I)) +
     .                NI(7)*TEMP(NC7(I)) +  NI(8)*TEMP(NC8(I))  
      ENDDO
C-----------
      RETURN
      END
