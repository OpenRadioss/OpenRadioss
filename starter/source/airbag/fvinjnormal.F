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
Chd|  FVINJNORMAL                   source/airbag/fvinjnormal.F   
Chd|-- called by -----------
Chd|        FVMESH0                       source/airbag/fvmesh0.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FVINJNORMAL(IBUF, ELEM, IXC, IXTG, ITAGEL, ELTG,
     .                       NEL, ILVOUT, MINUS_SIGN_REVERSE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXC(NIXC,*), IXTG(NIXTG,*)
      INTEGER IBUF(*), ELEM(3,*), ELTG(*), ITAGEL(*)
      INTEGER NEL, ILVOUT
      INTEGER MINUS_SIGN_REVERSE(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, I1, I2, I3
      INTEGER N1, N2, N3, N4, IEL, IREVERSE
      CHARACTER*6 TITL
C-----------------------------------------------------------------------
C REDEFINE INTERNAL TRIANGLE FOR NORMAL CONSISTENCY FOR INJECTORS
C-----------------------------------------------------------------------
      IF (ILVOUT >= 2) WRITE(IOUT,1000)
      DO I=1,NEL
         I1=ELEM(1,I)
         I2=ELEM(2,I)
         I3=ELEM(3,I)
         I1=IBUF(I1)
         I2=IBUF(I2)
         I3=IBUF(I3)
         IEL=ELTG(I)
         IREVERSE=0
         IF (IEL<=NUMELC) THEN
            N1  =IXC(2,IEL)
            N2  =IXC(3,IEL)
            N3  =IXC(4,IEL)
            N4  =IXC(5,IEL)
            K   =IXC(NIXC,IEL)
            TITL='SHELL:'
            IF(I1==N1) THEN
               IF(I2==N3) THEN
                   IF(I3==N2) IREVERSE=1
               ELSEIF(I2==N4) THEN
                   IREVERSE=1
               ENDIF
            ELSEIF(I1==N2) THEN
               IF(I2==N4) THEN
                   IF(I3==N3) IREVERSE=1
               ELSEIF(I2==N1) THEN
                   IREVERSE=1
               ENDIF
            ELSEIF(I1==N3) THEN
               IF(I2==N1) THEN
                   IF(I3==N4) IREVERSE=1
               ELSEIF(I2==N2) THEN
                   IREVERSE=1
               ENDIF
            ELSEIF(I1==N4) THEN
               IF(I2==N2) THEN
                   IF(I3==N1) IREVERSE=1
               ELSEIF(I2==N3) THEN
                   IREVERSE=1
               ENDIF
            ENDIF
          ELSEIF (IEL>NUMELC) THEN
               J=IEL-NUMELC
               N1  =IXTG(2,J)
               N2  =IXTG(3,J)
               N3  =IXTG(4,J)
               K   =IXTG(NIXTG,J)
               TITL='SH3N: '
               IF(I1==N1.AND.I2==N3) IREVERSE=1
               IF(I1==N2.AND.I2==N1) IREVERSE=1
               IF(I1==N3.AND.I2==N2) IREVERSE=1
          ENDIF
C
           IF(IREVERSE==1 .AND. MINUS_SIGN_REVERSE(I)==0) THEN
               I2=ELEM(2,I)
               I3=ELEM(3,I)
               ELEM(2,I)=I3
               ELEM(3,I)=I2
              IF(ILVOUT >= 2) THEN
              WRITE(IOUT,1100) I,TITL,K
              ENDIF
           ENDIF
C
      ENDDO
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--     
 1000 FORMAT(/5X,'FVMBAG   INTERNAL SURFACE ELEMENTS RENUMBERED'
     .       /5X,'---------------------------------------------')
 1100 FORMAT( 5X,'TRIANGLE',I8,5X,A,I8)
      RETURN
      END
