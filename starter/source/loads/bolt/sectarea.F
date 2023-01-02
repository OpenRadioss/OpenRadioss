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

!      SUBROUTINE SECTAREA(IXS,X,IBUF,NELE,ISOLNOD,AREA)  
Chd|====================================================================
Chd|  SECTAREA                      source/loads/bolt/sectarea.F  
Chd|-- called by -----------
Chd|        HM_READ_PRELOAD               source/loads/general/preload/hm_read_preload.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SECTAREA(IXS,IXS10,X,IBUF,NELE,ISOLNOD,AREA,ITAB)  
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),IXS10(6,*),IBUF(2,*),ISOLNOD(*)  ,ITAB(*)
      INTEGER NELE
      my_real
     .   AREA,X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------     
      INTEGER IE,II,I,J,IN1,IN2,IN3,IJ,I10
      INTEGER NODE(10)
      my_real
     .   X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,N3
C=======================================================================
      
      AREA = ZERO
      DO IE=1,NELE
        J=0
        II=IBUF(1,IE)
        NODE(1:10) = 0
        IF(II<=NUMELS8)THEN
         IF(ISOLNOD(II) == 4) THEN     ! Cas du tetra4
           DO I=0,6,2
            IF(BTEST(IBUF(2,IE),I)) THEN
              J=J+1
              NODE(J)=IXS(I+2,II)
            ENDIF
           ENDDO
         ELSE IF (ISOLNOD(II) == 8) THEN
           DO I=0,7
            IF(BTEST(IBUF(2,IE),I)) THEN
              J=J+1
              NODE(J)=IXS(I+2,II)
            ENDIF
           ENDDO
         ENDIF
           
         IF(J>=3)THEN
           IN1 = NODE(1)
           IN2 = NODE(2)
           IN3 = NODE(3)
           X1=X(1,IN1)-X(1,IN2)
           Y1=X(2,IN1)-X(2,IN2)
           Z1=X(3,IN1)-X(3,IN2)
           X2=X(1,IN3)-X(1,IN2)
           Y2=X(2,IN3)-X(2,IN2)
           Z2=X(3,IN3)-X(3,IN2)
           X3=Y1*Z2-Z1*Y2
           Y3=Z1*X2-Z2*X1
           Z3=X1*Y2-X2*Y1
           N3=X3*X3+Y3*Y3+Z3*Z3
           AREA=AREA+HALF*SQRT(N3)
           IF(J==4)THEN
             IN2 = NODE(4)
             X1=X(1,IN1)-X(1,IN2)
             Y1=X(2,IN1)-X(2,IN2)
             Z1=X(3,IN1)-X(3,IN2)
             X2=X(1,IN3)-X(1,IN2)
             Y2=X(2,IN3)-X(2,IN2)
             Z2=X(3,IN3)-X(3,IN2)
             X3=Y1*Z2-Z1*Y2
             Y3=Z1*X2-Z2*X1
             Z3=X1*Y2-X2*Y1
             N3=X3*X3+Y3*Y3+Z3*Z3
             AREA=AREA+HALF*SQRT(N3)
           ENDIF
         ENDIF
         
        ELSE
         I10=II-NUMELS8
         IF(I10<=NUMELS10) THEN
         !IF(ISOLNOD(II) == 10) THEN     ! Cas du tetra10
          DO I=0,6,2
           IF(BTEST(IBUF(2,IE),I)) THEN
            J=J+1
            NODE(J)=IXS(I+2,II)
           ENDIF
          ENDDO
          DO I=8,13
           IF(BTEST(IBUF(2,IE),I)) THEN
            J=J+1
            NODE(J)=IXS10(I-7,I10)
           ENDIF
          ENDDO
         
         IF(J==6)THEN
          IN1 = NODE(1)
          IN2 = NODE(2)
          IN3 = NODE(3)
          X1=X(1,IN2)-X(1,IN1)
          Y1=X(2,IN2)-X(2,IN1)
          Z1=X(3,IN2)-X(3,IN1)
          X2=X(1,IN3)-X(1,IN1)
          Y2=X(2,IN3)-X(2,IN1)
          Z2=X(3,IN3)-X(3,IN1)
          X3=Y1*Z2-Z1*Y2
          Y3=Z1*X2-Z2*X1
          Z3=X1*Y2-X2*Y1
          N3=X3*X3+Y3*Y3+Z3*Z3
          AREA=AREA+HALF*SQRT(N3)
         ENDIF
        ENDIF
        ENDIF
      ENDDO
         
      RETURN
      END
