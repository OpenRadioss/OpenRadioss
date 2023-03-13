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
Chd|  I15MARQ                       source/interfaces/int15/i15marq.F
Chd|-- called by -----------
Chd|        I15CMP                        source/interfaces/int15/i15cmp.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I15MARQ(KSI,NSC,KSC,NTC,KTC,
     2                   IACTIV,NSN,KSN,IMARQND,NNC,
     3                   KNC)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KSI(4,*),NSC,KSC(*),NTC,KTC(*),
     .        IACTIV(4,*),IMARQND(*),NSN,KSN(*),NNC,
     .        KNC(*)
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IL, IN, IN1, IN2, IN3, IN4
C-----------------------------------------------
      DO I=1,NSN
       IN=KSN(I)
       IMARQND(IN)=0
      ENDDO
C-----
      DO I=1,NSC
       IL=KSC(I)
       IN1=KSI(1,IL)
       IN2=KSI(2,IL)
       IN3=KSI(3,IL)
       IN4=KSI(4,IL)
       IF (IACTIV(1,IL)>0
     . .OR.IACTIV(2,IL)>0    
     . .OR.IACTIV(3,IL)>0    
     . .OR.IACTIV(4,IL)>0) THEN
        IMARQND(IN1)=1
        IMARQND(IN2)=1
        IMARQND(IN3)=1
        IMARQND(IN4)=1
       ENDIF
      ENDDO
      DO I=1,NTC
       IL=KTC(I)
       IN1=KSI(1,IL)
       IN2=KSI(2,IL)
       IN3=KSI(3,IL)
       IF (IACTIV(1,IL)>0) THEN
        IMARQND(IN1)=1
        IMARQND(IN2)=1
        IMARQND(IN3)=1
       ENDIF   
      ENDDO
C-----
      NNC=0
      DO I=1,NSN
       IN=KSN(I)
       IF (IMARQND(IN)/=0) THEN
        NNC=NNC+1
        KNC(NNC)=IN
       ENDIF
      ENDDO
C----------------------------------
      RETURN
      END
