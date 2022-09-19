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
Chd|  I2SURFS                       source/interfaces/inter3d1/i2surfs.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        NORMA1                        source/interfaces/inter3d1/norma1.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I2SURFS(
     .      X        ,NSV      ,AREA     ,NSN      ,ITAB     ,
     .      IXC      ,IXTG     ,IXS      ,KNOD2ELS ,NOD2ELS  ,
     .      KNOD2ELC ,NOD2ELC  ,KNOD2ELTG,NOD2ELTG ,ILEV     ,
     .      ID,TITR  ,INTTH    )
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "param_c.inc"
#include      "scr07_c.2.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN,ILEV,INTTH
      INTEGER NSV(*),IXC(NIXC,*),IXTG(NIXTG,*),IXS(NIXS,*),ITAB(*),
     .   KNOD2ELC(*),KNOD2ELTG(*),KNOD2ELS(*),NOD2ELC(*),
     .   NOD2ELTG(*),NOD2ELS(*)
C     REAL
      my_real
     .   X(3,*),AREA(*)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, N, IAD, IS, IEL, ICOQ, ISOL,
     .   N1,N2,N3,N4,N5,N6,N7,N8
C     REAL
      my_real
     .   EX,EY,EZ,X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,X4,Y4,Z4,
     .   X5,Y5,Z5,X6,Y6,Z6,X7,Y7,Z7,X8,Y8,Z8,
     .   X12,Y12,Z12,X13,Y13,Z13,X24,Y24,Z24,I1,I2,I3,AREA0
      my_real
     .   FACE(6)
C=======================================================================
      ICOQ = 1
      ISOL = 1
      IF (ILEV == 11 .OR. ILEV == 21) ISOL = 0
      IF (ILEV == 12 .OR. ILEV == 22) ICOQ = 0
      DO I=1,NSN
        AREA0   = AREA(I)
        AREA(I) = ZERO
        IS = NSV(I)
        IF (ICOQ == 1) THEN
C--       Cooques 4n
          DO IAD = KNOD2ELC(IS)+1,KNOD2ELC(IS+1)
            IEL  = NOD2ELC(IAD)
            N1   = IXC(2,IEL)
            N2   = IXC(3,IEL)
            N3   = IXC(4,IEL)
            N4   = IXC(5,IEL)
            X1  = X(1,N1)
            Y1  = X(2,N1)
            Z1  = X(3,N1)
            X2  = X(1,N2)
            Y2  = X(2,N2)
            Z2  = X(3,N2)
            X3  = X(1,N3)
            Y3  = X(2,N3)
            Z3  = X(3,N3)
            X4  = X(1,N4)
            Y4  = X(2,N4)
            Z4  = X(3,N4)
            X12 = X2 - X1
            Y12 = Y2 - Y1
            Z12 = Z2 - Z1
            X13 = X3 - X1
            Y13 = Y3 - Y1
            Z13 = Z3 - Z1
            X24 = X4 - X2
            Y24 = Y4 - Y2
            Z24 = Z4 - Z2
            EX  = Y13*Z24 - Z13*Y24
            EY  = Z13*X24 - X13*Z24
            EZ  = X13*Y24 - Y13*X24
            AREA(I) = AREA(I) + SQRT(EX*EX+EY*EY+EZ*EZ)*HALF*FOURTH
          ENDDO
C--       Cooques 3n
          DO IAD = KNOD2ELTG(IS)+1,KNOD2ELTG(IS+1)
            IEL  = NOD2ELTG(IAD)
            N1  = IXTG(2,IEL)
            N2  = IXTG(3,IEL)
            N3  = IXTG(4,IEL)
            X1  = X(1,N1)
            Y1  = X(2,N1)
            Z1  = X(3,N1)
            X2  = X(1,N2)
            Y2  = X(2,N2)
            Z2  = X(3,N2)
            X3  = X(1,N3)
            Y3  = X(2,N3)
            Z3  = X(3,N3)
            X13 = X3 - X1
            Y13 = Y3 - Y1
            Z13 = Z3 - Z1
            X12 = X2 - X1
            Y12 = Y2 - Y1
            Z12 = Z2 - Z1
            EX  = Y12*Z13 - Z12*Y13
            EY  = Z12*X13 - X12*Z13
            EZ  = X12*Y13 - Y12*X13
            AREA(I) = AREA(I) + SQRT(EX*EX+EY*EY+EZ*EZ)*HALF*THIRD
          ENDDO
        ENDIF
        IF (ISOL == 1) THEN
C--       Solides 8n
          DO IAD = KNOD2ELS(IS)+1,KNOD2ELS(IS+1)
            IEL  = NOD2ELS(IAD)
            N1 = IXS(2,IEL)
            N2 = IXS(3,IEL)
            N3 = IXS(4,IEL)
            N4 = IXS(5,IEL)
            N5 = IXS(6,IEL)
            N6 = IXS(7,IEL)
            N7 = IXS(8,IEL)
            N8 = IXS(9,IEL)
            X1=X(1,N1)
            Y1=X(2,N1)
            Z1=X(3,N1)
            X2=X(1,N2)
            Y2=X(2,N2)
            Z2=X(3,N2)
            X3=X(1,N3)
            Y3=X(2,N3)
            Z3=X(3,N3)
            X4=X(1,N4)
            Y4=X(2,N4)
            Z4=X(3,N4)
            X5=X(1,N5)
            Y5=X(2,N5)
            Z5=X(3,N5)
            X6=X(1,N6)
            Y6=X(2,N6)
            Z6=X(3,N6)
            X7=X(1,N7)
            Y7=X(2,N7)
            Z7=X(3,N7)
            X8=X(1,N8)
            Y8=X(2,N8)
            Z8=X(3,N8)
C----
c           face 1234
            XX1(1)=X1
            XX2(1)=Y1
            XX3(1)=Z1
            XX1(2)=X2
            XX2(2)=Y2
            XX3(2)=Z2
            XX1(3)=X3
            XX2(3)=Y3
            XX3(3)=Z3
            XX1(4)=X4
            XX2(4)=Y4
            XX3(4)=Z4
            CALL NORMA1(I1,I2,I3,FACE(1))
c           face 5678
            XX1(1)=X5
            XX2(1)=Y5
            XX3(1)=Z5
            XX1(2)=X6
            XX2(2)=Y6
            XX3(2)=Z6
            XX1(3)=X7
            XX2(3)=Y7
            XX3(3)=Z7
            XX1(4)=X8
            XX2(4)=Y8
            XX3(4)=Z8
            CALL NORMA1(I1,I2,I3,FACE(2))
c           face 2376
            XX1(1)=X2
            XX2(1)=Y2
            XX3(1)=Z2
            XX1(2)=X3
            XX2(2)=Y3
            XX3(2)=Z3
            XX1(3)=X7
            XX2(3)=Y7
            XX3(3)=Z7
            XX1(4)=X6
            XX2(4)=Y6
            XX3(4)=Z6
            CALL NORMA1(I1,I2,I3,FACE(3))
c           face 1485
            XX1(1)=X1
            XX2(1)=Y1
            XX3(1)=Z1
            XX1(2)=X4
            XX2(2)=Y4
            XX3(2)=Z4
            XX1(3)=X8
            XX2(3)=Y8
            XX3(3)=Z8
            XX1(4)=X5
            XX2(4)=Y5
            XX3(4)=Z5
            CALL NORMA1(I1,I2,I3,FACE(4))
c           face 1265
            XX1(1)=X1
            XX2(1)=Y1
            XX3(1)=Z1
            XX1(2)=X2
            XX2(2)=Y2
            XX3(2)=Z2
            XX1(3)=X6
            XX2(3)=Y6
            XX3(3)=Z6
            XX1(4)=X5
            XX2(4)=Y5
            XX3(4)=Z5
            CALL NORMA1(I1,I2,I3,FACE(5))
c           face 4378
            XX1(1)=X4
            XX2(1)=Y4
            XX3(1)=Z4
            XX1(2)=X3
            XX2(2)=Y3
            XX3(2)=Z3
            XX1(3)=X7
            XX2(3)=Y7
            XX3(3)=Z7
            XX1(4)=X8
            XX2(4)=Y8
            XX3(4)=Z8
            CALL NORMA1(I1,I2,I3,FACE(6))
C----
            DO K=1,8
              N = IXS(K+1,IEL)
              IF (N == IS) THEN
                IF (K == 1) THEN
                  AREA(I) = AREA(I) + (FACE(1)+FACE(4)+FACE(5))*ONE_OVER_12
                ELSEIF  (K == 2) THEN
                  AREA(I) = AREA(I) + (FACE(1)+FACE(3)+FACE(5))*ONE_OVER_12
                ELSEIF  (K == 3) THEN
                  AREA(I) = AREA(I) + (FACE(1)+FACE(3)+FACE(6))*ONE_OVER_12
                ELSEIF  (K == 4) THEN
                  AREA(I) = AREA(I) + (FACE(1)+FACE(4)+FACE(6))*ONE_OVER_12
                ELSEIF  (K == 5) THEN
                  AREA(I) = AREA(I) + (FACE(2)+FACE(4)+FACE(5))*ONE_OVER_12
                ELSEIF  (K == 6) THEN
                  AREA(I) = AREA(I) + (FACE(2)+FACE(3)+FACE(5))*ONE_OVER_12
                ELSEIF  (K == 7) THEN
                  AREA(I) = AREA(I) + (FACE(2)+FACE(3)+FACE(6))*ONE_OVER_12
                ELSEIF  (K == 8) THEN
                  AREA(I) = AREA(I) + (FACE(2)+FACE(4)+FACE(6))*ONE_OVER_12
                ENDIF
              ENDIF
            ENDDO
          ENDDO
        ENDIF
C----
        IF (AREA(I) == ZERO) AREA(I) = AREA0
        IF (AREA(I) == ZERO) THEN
          CALL ANCMSG(MSGID=670,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=ID,
     .                C1=TITR,
     .                I2=ITAB(IS))
        ENDIF
C----
      ENDDO
C----------
      RETURN
      END SUBROUTINE I2SURFS
