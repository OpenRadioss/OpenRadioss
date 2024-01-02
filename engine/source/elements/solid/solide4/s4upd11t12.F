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
Chd|  S4UPD11T12                    source/elements/solid/solide4/s4upd11t12.F
Chd|-- called by -----------
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|-- calls ---------------
Chd|        S4JAC_I                       source/elements/solid/solide4/s4jac_i.F
Chd|====================================================================
      SUBROUTINE S4UPD11T12(
     1   OFFG,    OFFG0,   XD1,     XD2,
     2   XD3,     XD4,     YD1,     YD2,
     3   YD3,     YD4,     ZD1,     ZD2,
     4   ZD3,     ZD4,     JAC_1,   SIG,
     5   SIGL,    NEL,     JCVT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: JCVT
      INTEGER NEL
C     REAL
      my_real
     .   OFFG(*),OFFG0(*),JAC_1(10,NEL),SIG(NEL,6),SIGL(NEL,6)
      DOUBLE PRECISION 
     .   XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ),
     .   YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ),
     .   ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,NCH,INDEX(MVSIZ),J    
      my_real
     .   JAC_I(10,MVSIZ)
      DOUBLE PRECISION 
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ)
C=======================================================================
C-----Always in global system and fixed then (not like Ismstr=11
C-----for JCVT>0 Fixed SAV is rotated w/ local system not like Ismstr=1
        NCH=0
        DO I=1,NEL
         IF(OFFG(I)/=OFFG0(I).AND.ABS(OFFG(I)) > ONE ) THEN
           NCH = NCH +1
           INDEX(NCH) = I
         END IF
          END DO
          IF (NCH==0) RETURN
        DO J=1,NCH
          I= INDEX(J)
          X1(J)=XD1(I)
          X2(J)=XD2(I)
          X3(J)=XD3(I)
          X4(J)=XD4(I)
          Y1(J)=YD1(I)
          Y2(J)=YD2(I)
          Y3(J)=YD3(I)
          Y4(J)=YD4(I)
          Z1(J)=ZD1(I)
          Z2(J)=ZD2(I)
          Z3(J)=ZD3(I)
          Z4(J)=ZD4(I)
        END DO
C
       CALL S4JAC_I(
     .              X1   ,X2   ,X3   ,X4   ,
     .              Y1   ,Y2   ,Y3   ,Y4   ,
     .              Z1   ,Z2   ,Z3   ,Z4   ,
     .              JAC_I ,NCH  )
#include "vectorize.inc"
        DO J=1,NCH
          I = INDEX(J)
          JAC_1(1:10,I) = JAC_I(1:10,J)
          SIGL(I,1:6) = SIG(I,1:6)
        ENDDO      
C-----------
      RETURN
      END
