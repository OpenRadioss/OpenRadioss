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
Chd|  SDLEN_SMS                     source/elements/solid/solidez/sdlen_sms.F
Chd|-- called by -----------
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|        BASISF                        source/elements/solid/solide8/basisf.F
Chd|        DEGENES8                      source/elements/solid/solide/degenes8.F
Chd|        DEGES4V                       source/elements/solid/solide/deges4v.F
Chd|        IDEGE8                        source/elements/solid/solide/idege8.F
Chd|====================================================================
      SUBROUTINE SDLEN_SMS(
     1   DELTAX,     VOLG,       IXS,        X1,
     2   X2,         X3,         X4,         X5,
     3   X6,         X7,         X8,         Y1,
     4   Y2,         Y3,         Y4,         Y5,
     5   Y6,         Y7,         Y8,         Z1,
     6   Z2,         Z3,         Z4,         Z5,
     7   Z6,         Z7,         Z8,         IPARTS,
     8   TAGPRT_SMS, TAGELEM_SMS,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER  IXS(NIXS,*), IPARTS(*), TAGPRT_SMS(*), TAGELEM_SMS(*)
C     REAL
      my_real
     .   DELTAX(*),VOLG(*),
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*),  
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, J, IT, IPT, IDEGE(MVSIZ), NINDX, NJNDX, 
     .         INDEX(MVSIZ), JNDEX(MVSIZ)
C     REAL
      my_real
     .   AJ11, AJ12, AJ13, AJ21,
     .   AJ22, AJ23, AJ31, AJ32,
     .   AJ33, AI11, AI12, AI13,
     .   AI21, AI22, AI23, AI31,
     .   AI32, AI33
C     REAL
      my_real
     .   X12(MVSIZ), X34(MVSIZ), X56(MVSIZ),
     .   X78(MVSIZ), Y12(MVSIZ), Y34(MVSIZ), Y56(MVSIZ), Y78(MVSIZ),
     .   Z12(MVSIZ), Z34(MVSIZ), Z56(MVSIZ), Z78(MVSIZ), X14(MVSIZ),
     .   X23(MVSIZ), X58(MVSIZ), X67(MVSIZ), Y14(MVSIZ), Y23(MVSIZ),
     .   Y58(MVSIZ), Y67(MVSIZ), Z14(MVSIZ), Z23(MVSIZ), Z58(MVSIZ),
     .   Z67(MVSIZ), X15(MVSIZ), X26(MVSIZ), X37(MVSIZ), X48(MVSIZ),
     .   Y15(MVSIZ), Y26(MVSIZ), Y37(MVSIZ), Y48(MVSIZ), Z15(MVSIZ),
     .   Z26(MVSIZ), Z37(MVSIZ), Z48(MVSIZ), H(8), VLINV,
     .   XX1,YY1,ZZ1,XX2,YY2,ZZ2,XX3,YY3,ZZ3,SMAX(MVSIZ),VMIN,
     .   P1(8), P2(8), P3(8),VLINC(MVSIZ,8)
      my_real
     .   AREAM(MVSIZ), FAC, V_G
C-----------------------------------------------
C  
      NINDX=0    
      NJNDX=0
C
      IF (ISMS_SELEC==1) THEN
C-- Full AMS
        DO I=1,NEL
          NJNDX=NJNDX+1
          JNDEX(NJNDX)=I
        END DO
      ELSEIF (ISMS_SELEC==2) THEN
C-- AMS by parts    
        DO I=1,NEL
          IF(TAGPRT_SMS(IPARTS(I))==0)THEN
            NINDX=NINDX+1
            INDEX(NINDX)=I
          ELSE
            NJNDX=NJNDX+1
            JNDEX(NJNDX)=I
          END IF
        END DO
      ELSEIF (ISMS_SELEC==3) THEN
C-- AMS auto - defined by elements
        DO I=1,NEL
          IF(TAGELEM_SMS(I)==0)THEN
            NINDX=NINDX+1
            INDEX(NINDX)=I
          ELSE
            NJNDX=NJNDX+1
            JNDEX(NJNDX)=I
          END IF
        END DO
      ELSEIF (ISMS_SELEC==4) THEN
C-- AMS auto + parts
        DO I=1,NEL
          IF ((TAGELEM_SMS(I)==0).AND.(TAGPRT_SMS(IPARTS(I))==0)) THEN
            NINDX=NINDX+1
            INDEX(NINDX)=I
          ELSE
            NJNDX=NJNDX+1
            JNDEX(NJNDX)=I
          END IF
        END DO
      ENDIF
C      
      IF(IDTS6 > 0)THEN
        CALL DEGENES8(
     1   IXS,     IDEGE,   NEL)
      ELSE
        IDEGE(1:NEL)=0
      END IF
C-----------------------------------------------------------
C     Non AMS elements ~ sdlen.F
C-----------------------------------------------------------
      DO J=1,NINDX
       I=INDEX(J)
C
       AREAM(I) =EM20
C
       CALL IDEGE8(X1(I),X2(I),X3(I),X4(I),Y1(I),Y2(I),Y3(I),Y4(I),
     .            Z1(I),Z2(I),Z3(I),Z4(I),AREAM(I),FAC,IT)
       CALL IDEGE8(X5(I),X6(I),X7(I),X8(I),Y5(I),Y6(I),Y7(I),Y8(I),
     .            Z5(I),Z6(I),Z7(I),Z8(I),AREAM(I),FAC,IT)
       CALL IDEGE8(X1(I),X2(I),X6(I),X5(I),Y1(I),Y2(I),Y6(I),Y5(I),
     .            Z1(I),Z2(I),Z6(I),Z5(I),AREAM(I),FAC,IT)
       CALL IDEGE8(X2(I),X3(I),X7(I),X6(I),Y2(I),Y3(I),Y7(I),Y6(I),
     .            Z2(I),Z3(I),Z7(I),Z6(I),AREAM(I),FAC,IT)
       CALL IDEGE8(X3(I),X4(I),X8(I),X7(I),Y3(I),Y4(I),Y8(I),Y7(I),
     .            Z3(I),Z4(I),Z8(I),Z7(I),AREAM(I),FAC,IT)
       CALL IDEGE8(X4(I),X1(I),X5(I),X8(I),Y4(I),Y1(I),Y5(I),Y8(I),
     .            Z4(I),Z1(I),Z5(I),Z8(I),AREAM(I),FAC,IT)
C----tetra 4  ,pyrami    
       IF(IDEGE(I) > 0)THEN
C----tetra 4  ,pyrami     
         IF (IDEGE(I) > 2) THEN
           FAC=ONE_OVER_9
         ELSEIF (IDEGE(I) > 1) THEN
           FAC=FOURTH
         ELSE
           FAC=ONE
         END IF
C--------suposse here V=0.5*A_max*L for penta  =0.333A_max*L for Pyram 
         IF (IT ==0  ) AREAM(I)=FAC*AREAM(I) 
C--------add special treat for tetra4, as V is not right values
         IF (IDEGE(I) > 3 ) THEN
             AREAM(I)=AREAM(I)*FAC
           CALL DEGES4V(V_G,
     .    X1(I), X2(I), X3(I), X4(I), X5(I), X6(I), X7(I), X8(I),
     .    Y1(I), Y2(I), Y3(I), Y4(I), Y5(I), Y6(I), Y7(I), Y8(I),
     .    Z1(I), Z2(I), Z3(I), Z4(I), Z5(I), Z6(I), Z7(I), Z8(I))
           ELSE
             V_G=VOLG(I)
           END IF
       ENDIF
C------
       DELTAX(I)=FOUR*VOLG(I)/SQRT(AREAM(I))
      END DO
C-----------------------------------------------------------
C     AMS elements ~ sdlen8
C-----------------------------------------------------------
      DO J=1,NJNDX
       I=JNDEX(J)
       IF(IDEGE(I)/=0) THEN
C------- due to the fact that AREA_Max*L is far from V for Dege---
         AREAM(I) =EM20
C----tetra 4  ,pyrami     
         IF (IDEGE(I) > 2) THEN
           FAC=ONE_OVER_9
         ELSEIF (IDEGE(I) > 1) THEN
           FAC=FOURTH
         ELSE
           FAC=ONE
         END IF
           IT = 0
         CALL IDEGE8(X1(I),X2(I),X3(I),X4(I),Y1(I),Y2(I),Y3(I),Y4(I),
     .              Z1(I),Z2(I),Z3(I),Z4(I),AREAM(I),FAC,IT)
         CALL IDEGE8(X5(I),X6(I),X7(I),X8(I),Y5(I),Y6(I),Y7(I),Y8(I),
     .              Z5(I),Z6(I),Z7(I),Z8(I),AREAM(I),FAC,IT)
         CALL IDEGE8(X1(I),X2(I),X6(I),X5(I),Y1(I),Y2(I),Y6(I),Y5(I),
     .              Z1(I),Z2(I),Z6(I),Z5(I),AREAM(I),FAC,IT)
         CALL IDEGE8(X2(I),X3(I),X7(I),X6(I),Y2(I),Y3(I),Y7(I),Y6(I),
     .              Z2(I),Z3(I),Z7(I),Z6(I),AREAM(I),FAC,IT)
         CALL IDEGE8(X3(I),X4(I),X8(I),X7(I),Y3(I),Y4(I),Y8(I),Y7(I),
     .              Z3(I),Z4(I),Z8(I),Z7(I),AREAM(I),FAC,IT)
         CALL IDEGE8(X4(I),X1(I),X5(I),X8(I),Y4(I),Y1(I),Y5(I),Y8(I),
     .              Z4(I),Z1(I),Z5(I),Z8(I),AREAM(I),FAC,IT)
C----tetra 4       
         IF (IT ==0  ) AREAM(I)=FAC*AREAM(I)
         IF (IDEGE(I) > 3 ) THEN
             AREAM(I)=AREAM(I)*FAC
           CALL DEGES4V(V_G,
     .    X1(I), X2(I), X3(I), X4(I), X5(I), X6(I), X7(I), X8(I),
     .    Y1(I), Y2(I), Y3(I), Y4(I), Y5(I), Y6(I), Y7(I), Y8(I),
     .    Z1(I), Z2(I), Z3(I), Z4(I), Z5(I), Z6(I), Z7(I), Z8(I))
           ELSE
             V_G=VOLG(I)
           END IF
         DELTAX(I) = FOUR*V_G/SQRT(AREAM(I))
       END IF
      END DO

      DO I=1,NEL
         X12(I)=X1(I)-X2(I)
         Y12(I)=Y1(I)-Y2(I)
         Z12(I)=Z1(I)-Z2(I)
         X34(I)=X3(I)-X4(I)
         Y34(I)=Y3(I)-Y4(I)
         Z34(I)=Z3(I)-Z4(I)
         X56(I)=X5(I)-X6(I)
         Y56(I)=Y5(I)-Y6(I)
         Z56(I)=Z5(I)-Z6(I)
         X78(I)=X7(I)-X8(I)
         Y78(I)=Y7(I)-Y8(I)
         Z78(I)=Z7(I)-Z8(I)
         X14(I)=X1(I)-X4(I)
         Y14(I)=Y1(I)-Y4(I)
         Z14(I)=Z1(I)-Z4(I)
         X23(I)=X2(I)-X3(I)
         Y23(I)=Y2(I)-Y3(I)
         Z23(I)=Z2(I)-Z3(I)
         X58(I)=X5(I)-X8(I)
         Y58(I)=Y5(I)-Y8(I)
         Z58(I)=Z5(I)-Z8(I)
         X67(I)=X6(I)-X7(I)
         Y67(I)=Y6(I)-Y7(I)
         Z67(I)=Z6(I)-Z7(I)
         X15(I)=X1(I)-X5(I)
         Y15(I)=Y1(I)-Y5(I)
         Z15(I)=Z1(I)-Z5(I)
         X26(I)=X2(I)-X6(I)
         Y26(I)=Y2(I)-Y6(I)
         Z26(I)=Z2(I)-Z6(I)
         X37(I)=X3(I)-X7(I)
         Y37(I)=Y3(I)-Y7(I)
         Z37(I)=Z3(I)-Z7(I)
         X48(I)=X4(I)-X8(I)
         Y48(I)=Y4(I)-Y8(I)
         Z48(I)=Z4(I)-Z8(I)
       ENDDO
C      
      DO IPT=1,8
       CALL BASISF (H,P1,P2,P3,IPT)
C
       DO I=1,NEL
          AJ11=P1(1)*X12(I)+P1(3)*X34(I)+P1(5)*X56(I)+P1(7)*X78(I)
          AJ12=P1(1)*Y12(I)+P1(3)*Y34(I)+P1(5)*Y56(I)+P1(7)*Y78(I)
          AJ13=P1(1)*Z12(I)+P1(3)*Z34(I)+P1(5)*Z56(I)+P1(7)*Z78(I)
          AJ21=P2(1)*X14(I)+P2(2)*X23(I)+P2(5)*X58(I)+P2(6)*X67(I)
          AJ22=P2(1)*Y14(I)+P2(2)*Y23(I)+P2(5)*Y58(I)+P2(6)*Y67(I)
          AJ23=P2(1)*Z14(I)+P2(2)*Z23(I)+P2(5)*Z58(I)+P2(6)*Z67(I)
          AJ31=P3(1)*X15(I)+P3(2)*X26(I)+P3(3)*X37(I)+P3(4)*X48(I)
          AJ32=P3(1)*Y15(I)+P3(2)*Y26(I)+P3(3)*Y37(I)+P3(4)*Y48(I)
          AJ33=P3(1)*Z15(I)+P3(2)*Z26(I)+P3(3)*Z37(I)+P3(4)*Z48(I)
          AI11= AJ22*AJ33-AJ23*AJ32
          AI21=-AJ21*AJ33+AJ23*AJ31
          AI31= AJ21*AJ32-AJ22*AJ31
          VLINC(I,IPT)=AJ11*AI11+AJ12*AI21+AJ13*AI31
       ENDDO
      END DO ! IPT=1,8
C      
      DO I=1,NEL
c mediane * 4
        XX1 = X1(I) + X2(I) + X3(I) + X4(I)
     .      - X5(I) - X6(I) - X7(I) - X8(I)
        YY1 = Y1(I) + Y2(I) + Y3(I) + Y4(I)
     .      - Y5(I) - Y6(I) - Y7(I) - Y8(I)
        ZZ1 = Z1(I) + Z2(I) + Z3(I) + Z4(I)
     .      - Z5(I) - Z6(I) - Z7(I) - Z8(I)
        XX2 = X1(I) + X2(I) + X5(I) + X6(I)
     .      - X3(I) - X4(I) - X7(I) - X8(I)
        YY2 = Y1(I) + Y2(I) + Y5(I) + Y6(I)
     .      - Y3(I) - Y4(I) - Y7(I) - Y8(I)
        ZZ2 = Z1(I) + Z2(I) + Z5(I) + Z6(I)
     .      - Z3(I) - Z4(I) - Z7(I) - Z8(I)
        XX3 = X1(I) + X4(I) + X5(I) + X8(I)
     .      - X3(I) - X2(I) - X7(I) - X6(I)
        YY3 = Y1(I) + Y4(I) + Y5(I) + Y8(I)
     .      - Y3(I) - Y2(I) - Y7(I) - Y6(I)
        ZZ3 = Z1(I) + Z4(I) + Z5(I) + Z8(I)
     .      - Z3(I) - Z2(I) - Z7(I) - Z6(I)
C surface * 16
                SMAX(I) =       (YY1 * ZZ2 - YY2 * ZZ1)**2
     .                + (ZZ1 * XX2 - ZZ2 * XX1)**2
     .                + (XX1 * YY2 - XX2 * YY1)**2
                SMAX(I) =    MAX(SMAX(I),(YY1 * ZZ3 - YY3 * ZZ1)**2
     .                + (ZZ1 * XX3 - ZZ3 * XX1)**2
     .                + (XX1 * YY3 - XX3 * YY1)**2)
                SMAX(I) =    MAX(SMAX(I),(YY3 * ZZ2 - YY2 * ZZ3)**2
     .                + (ZZ3 * XX2 - ZZ2 * XX3)**2
     .                + (XX3 * YY2 - XX2 * YY3)**2)
        ENDDO

C volume / 8
      IF (IDTS6>0) THEN
                DO I=1,NEL
                        IF(IDEGE(I)==0)THEN
                                VMIN = MIN(VLINC(I,1),VLINC(I,2),VLINC(I,3),VLINC(I,4),
     .                                     VLINC(I,5),VLINC(I,6),VLINC(I,7),VLINC(I,8))
                                DELTAX(I)=HUNDRED28*VMIN/SQRT(SMAX(I))
                        ENDIF
                ENDDO
      ELSE
                DO I=1,NEL
        VMIN = MIN(VLINC(I,1),VLINC(I,2),VLINC(I,3),VLINC(I,4),
     .             VLINC(I,5),VLINC(I,6),VLINC(I,7),VLINC(I,8))
                        DELTAX(I)=HUNDRED28*VMIN/SQRT(SMAX(I))
      ENDDO
      ENDIF
C-----------
      RETURN
      END
