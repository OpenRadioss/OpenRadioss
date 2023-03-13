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
Chd|  SORTI25                       source/mpi/interfaces/sorti25.F
Chd|-- called by -----------
Chd|        SPMD_I7FCOM_PON               source/mpi/forces/spmd_i7fcom_pon.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SORTI25(N,ISKY,FSKYI,NFSKYI)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N, NFSKYI, ISKY(*)
      my_real FSKYI(2*NFSKYI,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, L, IR, IIA
      my_real RRA1, RRA2, RRA3, RRA4, RRA5, RRA6, RRA7, RRA8, RRA9, RRA10
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
Cel test sur n=0
      IF (N==0) RETURN
      L=N/2+1
      IR=N
      IF(NFSKYI==4)THEN
10     CONTINUE
        IF(L>1)THEN
          L=L-1
          IIA =ISKY(L)
          RRA1=FSKYI(1,L)
          RRA2=FSKYI(2,L)
          RRA3=FSKYI(3,L)
          RRA4=FSKYI(4,L)
          RRA5=FSKYI(5,L)
          RRA6=FSKYI(6,L)
          RRA7=FSKYI(7,L)
          RRA8=FSKYI(8,L)
        ELSE
          IIA =ISKY(IR)
          RRA1=FSKYI(1,IR)
          RRA2=FSKYI(2,IR)
          RRA3=FSKYI(3,IR)
          RRA4=FSKYI(4,IR)
          RRA5=FSKYI(5,IR)
          RRA6=FSKYI(6,IR)
          RRA7=FSKYI(7,IR)
          RRA8=FSKYI(8,IR)
          ISKY(IR)=ISKY(1)
          FSKYI(1,IR)=FSKYI(1,1)
          FSKYI(2,IR)=FSKYI(2,1)
          FSKYI(3,IR)=FSKYI(3,1)
          FSKYI(4,IR)=FSKYI(4,1)
          FSKYI(5,IR)=FSKYI(5,1)
          FSKYI(6,IR)=FSKYI(6,1)
          FSKYI(7,IR)=FSKYI(7,1)
          FSKYI(8,IR)=FSKYI(8,1)
          IR=IR-1
C         IF(IR==1)THEN
          IF(IR<=1)THEN
            ISKY(1)=IIA
            FSKYI(1,1)=RRA1
            FSKYI(2,1)=RRA2
            FSKYI(3,1)=RRA3
            FSKYI(4,1)=RRA4
            FSKYI(5,1)=RRA5
            FSKYI(6,1)=RRA6
            FSKYI(7,1)=RRA7
            FSKYI(8,1)=RRA8
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
20      IF(J<=IR)THEN
          IF(J<IR)THEN
            IF(ISKY(J)<ISKY(J+1))J=J+1
          ENDIF
          IF(IIA<ISKY(J))THEN
            ISKY(I)=ISKY(J)
            FSKYI(1,I)=FSKYI(1,J)
            FSKYI(2,I)=FSKYI(2,J)
            FSKYI(3,I)=FSKYI(3,J)
            FSKYI(4,I)=FSKYI(4,J)
            FSKYI(5,I)=FSKYI(5,J)
            FSKYI(6,I)=FSKYI(6,J)
            FSKYI(7,I)=FSKYI(7,J)
            FSKYI(8,I)=FSKYI(8,J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 20
        ENDIF
        ISKY(I)=IIA
        FSKYI(1,I)=RRA1
        FSKYI(2,I)=RRA2
        FSKYI(3,I)=RRA3
        FSKYI(4,I)=RRA4
        FSKYI(5,I)=RRA5
        FSKYI(6,I)=RRA6
        FSKYI(7,I)=RRA7
        FSKYI(8,I)=RRA8
       GO TO 10
      ELSE
100    CONTINUE
        IF(L>1)THEN
          L=L-1
          IIA =ISKY(L)
          RRA1=FSKYI(1,L)
          RRA2=FSKYI(2,L)
          RRA3=FSKYI(3,L)
          RRA4=FSKYI(4,L)
          RRA5=FSKYI(5,L)
          RRA6=FSKYI(6,L)
          RRA7=FSKYI(7,L)
          RRA8=FSKYI(8,L)
          RRA9=FSKYI(9,L)
          RRA10=FSKYI(10,L)
        ELSE
          IIA =ISKY(IR)
          RRA1=FSKYI(1,IR)
          RRA2=FSKYI(2,IR)
          RRA3=FSKYI(3,IR)
          RRA4=FSKYI(4,IR)
          RRA5=FSKYI(5,IR)
          RRA6=FSKYI(6,IR)
          RRA7=FSKYI(7,IR)
          RRA8=FSKYI(8,IR)
          RRA9=FSKYI(9,IR)
          RRA10=FSKYI(10,IR)
          ISKY(IR)=ISKY(1)
          FSKYI(1,IR)=FSKYI(1,1)
          FSKYI(2,IR)=FSKYI(2,1)
          FSKYI(3,IR)=FSKYI(3,1)
          FSKYI(4,IR)=FSKYI(4,1)
          FSKYI(5,IR)=FSKYI(5,1)
          FSKYI(6,IR)=FSKYI(6,1)
          FSKYI(7,IR)=FSKYI(7,1)
          FSKYI(8,IR)=FSKYI(8,1)
          FSKYI(9,IR)=FSKYI(9,1)
          FSKYI(10,IR)=FSKYI(10,1)
          IR=IR-1
C         IF(IR==1)THEN
          IF(IR<=1)THEN
            ISKY(1)=IIA
            FSKYI(1,1)=RRA1
            FSKYI(2,1)=RRA2
            FSKYI(3,1)=RRA3
            FSKYI(4,1)=RRA4
            FSKYI(5,1)=RRA5
            FSKYI(6,1)=RRA6
            FSKYI(7,1)=RRA7
            FSKYI(8,1)=RRA8
            FSKYI(9,1)=RRA9
            FSKYI(10,1)=RRA10
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
200     IF(J<=IR)THEN
          IF(J<IR)THEN
            IF(ISKY(J)<ISKY(J+1))J=J+1
          ENDIF
          IF(IIA<ISKY(J))THEN
            ISKY(I)=ISKY(J)
            FSKYI(1,I)=FSKYI(1,J)
            FSKYI(2,I)=FSKYI(2,J)
            FSKYI(3,I)=FSKYI(3,J)
            FSKYI(4,I)=FSKYI(4,J)
            FSKYI(5,I)=FSKYI(5,J)
            FSKYI(6,I)=FSKYI(6,J)
            FSKYI(7,I)=FSKYI(7,J)
            FSKYI(8,I)=FSKYI(8,J)
            FSKYI(9,I)=FSKYI(9,J)
            FSKYI(10,I)=FSKYI(10,J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 200
        ENDIF
        ISKY(I)=IIA
        FSKYI(1,I)=RRA1
        FSKYI(2,I)=RRA2
        FSKYI(3,I)=RRA3
        FSKYI(4,I)=RRA4
        FSKYI(5,I)=RRA5
        FSKYI(6,I)=RRA6
        FSKYI(7,I)=RRA7
        FSKYI(8,I)=RRA8
        FSKYI(9,I)=RRA9
        FSKYI(10,I)=RRA10
       GO TO 100
      ENDIF
C
      RETURN
      END
