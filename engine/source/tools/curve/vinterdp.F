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
Chd|  VINTERDP                      source/tools/curve/vinterdp.F 
Chd|-- called by -----------
Chd|        FIXVEL                        source/constraints/general/impvel/fixvel.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE VINTERDP(TF,IAD,IPOS ,ILEN,NEL0,X,DYDX,Y)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C
      INTEGER ILEN(*),IPOS(*),IAD(*),NEL0
      my_real TF(2,*)
      INTEGER I,J1,J,ICONT,J2
      DOUBLE PRECISION :: X(*),DYDX(*),Y(*)
      DOUBLE PRECISION YDP, TF2J2,TF2J1,TF1J2,TF1J1,
     .  XDP,YYDP, DYDXDP
C
C
      J = 0
 100  CONTINUE
C 
       J = J+1
       ICONT = 0
       DO I=1,NEL0
         J1 = IPOS(I)+IAD(I)+1
         IF(J.LE.ILEN(I)-1.AND.X(I).GT.TF(1,J1))THEN
           IPOS(I)=IPOS(I)+1
           ICONT = 1
         ENDIF
       ENDDO
C
      IF(ICONT.EQ.1)GOTO 100
C
      DO I=1,NEL0
        J1   =IPOS(I)+IAD(I)
        J2   = J1+1

        TF2J2 = TF(2,J2)
        TF2J1 = TF(2,J1)
        TF1J2 = TF(1,J2)
        TF1J1 = TF(1,J1)
        XDP = X(I)

        DYDXDP = (TF2J2-TF2J1)/(TF1J2-TF1J1)
        DYDX(I) = DYDXDP

        YYDP = TF2J1 + DYDXDP * (XDP - TF1J1)
        Y(I) = YYDP

!        DYDX(I)=(TF(2,J2)-TF(2,J1))/(TF(1,J2)-TF(1,J1))
!        Y(I)   = TF(2,J1) + DYDX(I)*(X(I)-TF(1,J1))
      ENDDO
C
      RETURN
      END
