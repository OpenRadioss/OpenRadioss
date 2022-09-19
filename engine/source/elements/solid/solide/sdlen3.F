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
Chd|  SDLEN3                        source/elements/solid/solide/sdlen3.F
Chd|-- called by -----------
Chd|        S6CFORC3                      source/elements/thickshell/solide6c/s6cforc3.F
Chd|        SCFORC3                       source/elements/thickshell/solidec/scforc3.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|        SLDEGE                        source/elements/solid/solide/sdlen3.F
Chd|        SLEN                          source/elements/solid/solide/slen.F
Chd|        SLENA                         source/elements/solid/solide/slen.F
Chd|====================================================================
      SUBROUTINE SDLEN3(VOLN,DELTAX,
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .   N1X, N2X, N3X, N4X, N5X, N6X,
     .   N1Y, N2Y, N3Y, N4Y, N5Y, N6Y,
     .   N1Z, N2Z, N3Z, N4Z, N5Z, N6Z)
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
      my_real
     .   VOLN(*),DELTAX(*),
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*),
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*),
     .   N1X(*), N2X(*), N3X(*), N4X(*), N5X(*), N6X(*),
     .   N1Y(*), N2Y(*), N3Y(*), N4Y(*), N5Y(*), N6Y(*),
     .   N1Z(*), N2Z(*), N3Z(*), N4Z(*), N5Z(*), N6Z(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "fluid_scr_c.inc"
#include      "scr17_c.inc"
#include      "alesolver.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NFAC
      my_real
     .   XIOFF(MVSIZ), AREAM(MVSIZ), ATEST(MVSIZ), AREA(MVSIZ,6)
C-----------------------------------------------
      DO I=LFT,LLT
        XIOFF(I) = ONE
        AREAM(I) = EM20
      ENDDO

      IF((ICAA==1 .OR. ISFINT>=2) .AND. JALE+JEUL/=0)THEN
        CALL SLENA(
     .     N1X, N2X, N3X, N4X, N5X, N6X,
     .     N1Y, N2Y, N3Y, N4Y, N5Y, N6Y,
     .     N1Z, N2Z, N3Z, N4Z, N5Z, N6Z,
     .     X1, X2, X3, X4, X5, X6, X7, X8,
     .     Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .     Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,AREAM)
        DO I=LFT,LLT
          DELTAX(I)=TWO*VOLN(I)*XIOFF(I)/SQRT(AREAM(I))
        ENDDO
      ELSE
        CALL SLEN(X1,X2,X3,X4,Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,1,AREA,AREAM)
        CALL SLEN(X5,X6,X7,X8,Y5,Y6,Y7,Y8,Z5,Z6,Z7,Z8,2,AREA,AREAM)
        CALL SLEN(X1,X2,X6,X5,Y1,Y2,Y6,Y5,Z1,Z2,Z6,Z5,3,AREA,AREAM)
        CALL SLEN(X2,X3,X7,X6,Y2,Y3,Y7,Y6,Z2,Z3,Z7,Z6,4,AREA,AREAM)
        CALL SLEN(X3,X4,X8,X7,Y3,Y4,Y8,Y7,Z3,Z4,Z8,Z7,5,AREA,AREAM)
        CALL SLEN(X4,X1,X5,X8,Y4,Y1,Y5,Y8,Z4,Z1,Z5,Z8,6,AREA,AREAM)
        IF (IDTS6>0) THEN
          CALL SLDEGE(
     .     X1, X2, X3, X4, X5, X6, X7, X8,
     .     Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .     Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .     AREA,AREAM, VOLN)
        ENDIF
        IF(MTN==5.OR.MTN==41)THEN
          DO I=LFT,LLT
            ATEST(I)=EM4*AREAM(I)
          ENDDO
          DO I=LFT,LLT
            NFAC=0
            IF(AREA(I,1)<ATEST(I)) NFAC=NFAC+1
            IF(AREA(I,2)<ATEST(I)) NFAC=NFAC+1
            IF(AREA(I,3)<ATEST(I)) NFAC=NFAC+1
            IF(AREA(I,4)<ATEST(I)) NFAC=NFAC+1
            IF(AREA(I,5)<ATEST(I)) NFAC=NFAC+1
            IF(AREA(I,6)<ATEST(I)) NFAC=NFAC+1
            IF(NFAC>=3) XIOFF(I)=EP03
          ENDDO
       END IF
       DO  I=LFT,LLT
         DELTAX(I)=FOUR*VOLN(I)*XIOFF(I)/SQRT(AREAM(I))
       ENDDO
      ENDIF
C-----------------------------------------------      
      RETURN
      END

Chd|====================================================================
Chd|  SLDEGE                        source/elements/solid/solide/sdlen3.F
Chd|-- called by -----------
Chd|        SDLEN3                        source/elements/solid/solide/sdlen3.F
Chd|-- calls ---------------
Chd|        DEGES4V                       source/elements/solid/solide/sdlen3.F
Chd|        IDEGE                         source/elements/solid/solide/sdlen3.F
Chd|====================================================================
      SUBROUTINE SLDEGE(
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .   AREA,AREAM,VOLG)
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
      my_real
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*),
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*),
     .   AREA(MVSIZ,6),AREAM(*),VOLG(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IDEG(MVSIZ),J,IDET4(MVSIZ),IT(MVSIZ)
      INTEGER :: N_INDX
      INTEGER, DIMENSION(MVSIZ) :: INDX
      my_real
     .   FAC(MVSIZ),V_G
C-----------------------------------------------
      IDEG(1:MVSIZ)=0
      DO J=1,6
       DO I=LFT,LLT
        IF(AREA(I,J)<EM30) IDEG(I)=IDEG(I)+1
       ENDDO
      ENDDO
C
      N_INDX = 0
      DO I=LFT,LLT
C-------due to the fact that AREA_Max*L is far from V for Dege---
       IF(IDEG(I) > 0) THEN
                AREAM(I) =EM20
C----tetra 4  ,pyrami    
                IF (IDEG(I)>=2) THEN
                 FAC(I)=ONE_OVER_9
                ELSE
                 FAC(I)=FOURTH
                END IF
                N_INDX = N_INDX + 1
                INDX(N_INDX) = I
        ENDIF
      ENDDO
      IDET4(1:MVSIZ) = 1
      IT(1:MVSIZ) = 0
      IF(N_INDX>0) THEN
        CALL IDEGE(X1,X2,X3,X4,Y1,Y2,Y3,Y4,
     .             Z1,Z2,Z3,Z4,AREA(1,1),AREAM,FAC,IDET4,IT,INDX,N_INDX)
        CALL IDEGE(X5,X6,X7,X8,Y5,Y6,Y7,Y8,
     .             Z5,Z6,Z7,Z8,AREA(1,2),AREAM,FAC,IDET4,IT,INDX,N_INDX)
        CALL IDEGE(X1,X2,X6,X5,Y1,Y2,Y6,Y5,
     .             Z1,Z2,Z6,Z5,AREA(1,3),AREAM,FAC,IDET4,IT,INDX,N_INDX)
        CALL IDEGE(X2,X3,X7,X6,Y2,Y3,Y7,Y6,
     .             Z2,Z3,Z7,Z6,AREA(1,4),AREAM,FAC,IDET4,IT,INDX,N_INDX)
        CALL IDEGE(X3,X4,X8,X7,Y3,Y4,Y8,Y7,
     .             Z3,Z4,Z8,Z7,AREA(1,5),AREAM,FAC,IDET4,IT,INDX,N_INDX)
        CALL IDEGE(X4,X1,X5,X8,Y4,Y1,Y5,Y8,
     .             Z4,Z1,Z5,Z8,AREA(1,6),AREAM,FAC,IDET4,IT,INDX,N_INDX)

#include      "vectorize.inc"
          DO J=1,N_INDX
                I = INDX(J)
C--------suposse here V=0.5*A_max*L for penta	=0.333A_max*L for Pyram 
                IF (IT(I) ==0  ) AREAM(I)=FAC(I)*AREAM(I)
C--------add special treat for tetra4, as V is not right values		
                IF (IDET4(I) ==1 ) THEN
                        CALL DEGES4V(V_G,
     .                  X1(I), X2(I), X3(I), X4(I), X5(I), X6(I), X7(I), X8(I),
     .                  Y1(I), Y2(I), Y3(I), Y4(I), Y5(I), Y6(I), Y7(I), Y8(I),
     .                  Z1(I), Z2(I), Z3(I), Z4(I), Z5(I), Z6(I), Z7(I), Z8(I))
	                FAC(I)=THIRD*VOLG(I)/V_G
                        AREAM(I)=FAC(I)*FAC(I)*AREAM(I)
	        END IF
        ENDDO
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  IDEGE                         source/elements/solid/solide/sdlen3.F
Chd|-- called by -----------
Chd|        SLDEGE                        source/elements/solid/solide/sdlen3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE IDEGE(X1,X2,X3,X4,Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,A,AMAX,FAC,
     .                IT4,IT,INDX,N_INDX)
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
      my_real
     .   X1(*), X2(*), X3(*), X4(*), Y1(*), Y2(*), Y3(*), Y4(*),
     .   Z1(*), Z2(*), Z3(*), Z4(*),A(MVSIZ),AMAX(*),FAC(MVSIZ)
      INTEGER, INTENT(IN) ::N_INDX
      INTEGER, DIMENSION(MVSIZ), INTENT(IN) :: INDX
      INTEGER IT4(MVSIZ),IT(MVSIZ)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IDE(MVSIZ),I,J
      my_real
     .   X12,Y12,Z12,X23,Y23,Z23,X34,Y34,Z34,X41,Y41,Z41
C-----------------------------------------------
        IDE(1:MVSIZ) = 0
#include      "vectorize.inc"
        DO J=1,N_INDX
                I = INDX(J)
                X12 = X2(I) - X1(I)
                Y12 = Y2(I) - Y1(I)
                Z12 = Z2(I) - Z1(I)
                IF (X12==ZERO.AND.Y12==ZERO.AND.Z12==ZERO) IDE(I) = 1
C       
                X23 = X2(I) - X3(I)
                Y23 = Y2(I) - Y3(I)
                Z23 = Z2(I) - Z3(I)
                IF (X23==ZERO.AND.Y23==ZERO.AND.Z23==ZERO) IDE(I) = 1
C       
                X34 = X4(I) - X3(I)
                Y34 = Y4(I) - Y3(I)
                Z34 = Z4(I) - Z3(I)
                IF (X34==ZERO.AND.Y34==ZERO.AND.Z34==ZERO) IDE(I) = 1
C       
                X41 = X4(I) - X1(I)
                Y41 = Y4(I) - Y1(I)
                Z41 = Z4(I) - Z1(I)
                IF (X41==ZERO.AND.Y41==ZERO.AND.Z41==ZERO) IDE(I) = 1
                IF (IDE(I)==0) IT4(I) =0
                IF (A(I) > AMAX(I) )THEN
                        IT(I)=IDE(I)
                        AMAX(I) = A(I)
                END IF
        ENDDO
C       
      RETURN
      END
Chd|====================================================================
Chd|  DEGES4V                       source/elements/solid/solide/sdlen3.F
Chd|-- called by -----------
Chd|        SDLEN_DEGE                    source/elements/solid/solide/sdlen3.F
Chd|        SDLEN_SMS                     source/elements/solid/solidez/szforc3.F
Chd|        SLDEGE                        source/elements/solid/solide/sdlen3.F
Chd|-- calls ---------------
Chd|        NODEDEGE                      source/elements/solid/solide/sdlen3.F
Chd|        TETRA4V                       source/elements/solid/solide/sdlen3.F
Chd|====================================================================
      SUBROUTINE DEGES4V(DET,
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   DET,X1, X2, X3, X4,X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,NNOD
C                                                                     12
      my_real
     .   XYZ(3,4),XIJ,YIJ,ZIJ
C-----------------------------------------------
C
	   NNOD=1
	   XYZ(1,NNOD)=X1
	   XYZ(2,NNOD)=Y1
	   XYZ(3,NNOD)=Z1
       CALL NODEDEGE(X2,Y2,Z2,XYZ,NNOD)
       CALL NODEDEGE(X3,Y3,Z3,XYZ,NNOD)
       CALL NODEDEGE(X4,Y4,Z4,XYZ,NNOD)
       IF (NNOD==4) GOTO 100	
       CALL NODEDEGE(X5,Y5,Z5,XYZ,NNOD)	
       IF (NNOD==4) GOTO 100	
       CALL NODEDEGE(X6,Y6,Z6,XYZ,NNOD)	
       IF (NNOD==4) GOTO 100	
       CALL NODEDEGE(X7,Y7,Z7,XYZ,NNOD)	
       IF (NNOD==4) GOTO 100	
       CALL NODEDEGE(X8,Y8,Z8,XYZ,NNOD)	
 100   CONTINUE
       CALL TETRA4V(DET,
     .   XYZ(1,1), XYZ(1,2), XYZ(1,3), XYZ(1,4),
     .   XYZ(2,1), XYZ(2,2), XYZ(2,3), XYZ(2,4),
     .   XYZ(3,1), XYZ(3,2), XYZ(3,3), XYZ(3,4))
C      
      RETURN
      END
Chd|====================================================================
Chd|  NODEDEGE                      source/elements/solid/solide/sdlen3.F
Chd|-- called by -----------
Chd|        DEGES4V                       source/elements/solid/solide/sdlen3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE NODEDEGE(X,Y,Z,XYZ,NN)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NN
      my_real
     .   XYZ(3,*),X,Y,Z
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
      my_real
     .   XIJ,YIJ,ZIJ
C----6---------------------------------------------------------------7---------8
       DO J=1,NN
	    XIJ=XYZ(1,J)-X
	    YIJ=XYZ(2,J)-Y
	    ZIJ=XYZ(3,J)-Z
        IF (XIJ==ZERO.AND.YIJ==ZERO.AND.ZIJ==ZERO) RETURN
       END DO !J=1,NN
	     NN = NN + 1
	     XYZ(1,NN)=X
	     XYZ(2,NN)=Y
	     XYZ(3,NN)=Z
C
      RETURN
      END
Chd|====================================================================
Chd|  TETRA4V                       source/elements/solid/solide/sdlen3.F
Chd|-- called by -----------
Chd|        DEGES4V                       source/elements/solid/solide/sdlen3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TETRA4V(DET,
     .   X1, X2, X3, X4,
     .   Y1, Y2, Y3, Y4,
     .   Z1, Z2, Z3, Z4)
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
      my_real
     .   DET,
     .   X1, X2, X3, X4, Y1, Y2, Y3, Y4,Z1, Z2, Z3, Z4
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C                                                                     12
      my_real
     .   X41, Y41, Z41, X42, Y42, Z42, X43, Y43, Z43
      my_real
     .   RX, RY, RZ, SX, SY, SZ,TX, TY, TZ,
     .   TSX, TSY,TSZ
C-----------------------------------------------
       X43 = X4 - X3
       Y43 = Y4 - Y3
       Z43 = Z4 - Z3
       X41 = X4 - X1
       Y41 = Y4 - Y1
       Z41 = Z4 - Z1
       X42 = X4 - X2
       Y42 = Y4 - Y2
       Z42 = Z4 - Z2
C
       RX =  -X41
       RY =  -Y41
       RZ =  -Z41
C
       SX =  -X42
       SY =  -Y42
       SZ =  -Z42
C
       TX =  -X43
       TY =  -Y43
       TZ =  -Z43
C
       TSX =  Y43*Z42 - Y42*Z43
       TSY =  Z43*X42 - Z42*X43
       TSZ =  X43*Y42 - X42*Y43
C------V=r*(s^t)/6
       DET = ABS(RX*TSX + RY*TSY + RZ*TSZ)*ONE_OVER_6
C      
      RETURN
      END
Chd|====================================================================
Chd|  SDLEN_DEGE                    source/elements/solid/solide/sdlen3.F
Chd|-- called by -----------
Chd|        S8CFORC3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        SDLEN8                        source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|        DEGENES8                      source/elements/solid/solide/sdlen3.F
Chd|        DEGES4V                       source/elements/solid/solide/sdlen3.F
Chd|        IDEGE8                        source/elements/solid/solide/sdlen3.F
Chd|====================================================================
      SUBROUTINE SDLEN_DEGE(VOLG,LAT,
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .   IXS  ,IDEGE)
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
#include      "vect01_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*), IDEGE(*)
      my_real
     .   VOLG(*),LAT(*),
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*),
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C----------DELTAX crrection for degenerated element
      INTEGER I,IT
      my_real
     .   AREAM(MVSIZ), ATAX,FAC,V_G
C-----------------------------------------------
      CALL DEGENES8(IXS,IDEGE)
C
      DO I=LFT,LLT
C-------due to the fact that AREA_Max*L is far from V for Dege---
       IF(IDEGE(I) > 0) THEN
        AREAM(I) =EM20
C----tetra 4  ,pyrami     
        IF (IDEGE(I)>2) THEN
         FAC=ONE_OVER_9
        ELSEIF (IDEGE(I)>1) THEN
         FAC=FOURTH
        ELSE
         FAC=ONE
        END IF
		IT = 0
        CALL IDEGE8(X1(I),X2(I),X3(I),X4(I),Y1(I),Y2(I),Y3(I),Y4(I),
     .             Z1(I),Z2(I),Z3(I),Z4(I),AREAM(I),FAC,IT)
        CALL IDEGE8(X5(I),X6(I),X7(I),X8(I),Y5(I),Y6(I),Y7(I),Y8(I),
     .             Z5(I),Z6(I),Z7(I),Z8(I),AREAM(I),FAC,IT)
        CALL IDEGE8(X1(I),X2(I),X6(I),X5(I),Y1(I),Y2(I),Y6(I),Y5(I),
     .             Z1(I),Z2(I),Z6(I),Z5(I),AREAM(I),FAC,IT)
        CALL IDEGE8(X2(I),X3(I),X7(I),X6(I),Y2(I),Y3(I),Y7(I),Y6(I),
     .             Z2(I),Z3(I),Z7(I),Z6(I),AREAM(I),FAC,IT)
        CALL IDEGE8(X3(I),X4(I),X8(I),X7(I),Y3(I),Y4(I),Y8(I),Y7(I),
     .             Z3(I),Z4(I),Z8(I),Z7(I),AREAM(I),FAC,IT)
        CALL IDEGE8(X4(I),X1(I),X5(I),X8(I),Y4(I),Y1(I),Y5(I),Y8(I),
     .             Z4(I),Z1(I),Z5(I),Z8(I),AREAM(I),FAC,IT)
C----tetra 4       
        IF (IT ==0  ) AREAM(I)=FAC*AREAM(I)
        IF (IDEGE(I)>3 ) THEN
		 AREAM(I)=AREAM(I)*FAC
         CALL DEGES4V(V_G,
     .   X1(I), X2(I), X3(I), X4(I), X5(I), X6(I), X7(I), X8(I),
     .   Y1(I), Y2(I), Y3(I), Y4(I), Y5(I), Y6(I), Y7(I), Y8(I),
     .   Z1(I), Z2(I), Z3(I), Z4(I), Z5(I), Z6(I), Z7(I), Z8(I))
		ELSE
		 V_G=VOLG(I)
		END IF
        LAT(I) = FOUR*V_G/SQRT(AREAM(I))

       ENDIF
      ENDDO
C      
      RETURN
      END
Chd|====================================================================
Chd|  IDEGE8                        source/elements/solid/solide/sdlen3.F
Chd|-- called by -----------
Chd|        SDLEN_DEGE                    source/elements/solid/solide/sdlen3.F
Chd|        SDLEN_SMS                     source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE IDEGE8(X1,X2,X3,X4,Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,AMAX,FAC,IT)
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
      my_real
     .   X1, X2, X3, X4, Y1, Y2, Y3, Y4,
     .   Z1, Z2, Z3, Z4,AMAX,FAC
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IDE,IT
      my_real
     .   X12,Y12,Z12,X23,Y23,Z23,X34,Y34,Z34,X41,Y41,Z41,A,
     .   RX,RY,RZ,SX,SY,SZ,NX,NY,NZ
C-----------------------------------------------
       RX = X2+X3-X1-X4
       RY = Y2+Y3-Y1-Y4
       RZ = Z2+Z3-Z1-Z4
       SX = X3+X4-X1-X2
       SY = Y3+Y4-Y1-Y2
       SZ = Z3+Z4-Z1-Z2
       NX = RY * SZ - RZ * SY
       NY = RZ * SX - RX * SZ
       NZ = RX * SY - RY * SX
C--------A: (2*AREA)^2-----       
       A = NX*NX+NY*NY+NZ*NZ
       IDE = 0
       X12 = X2 - X1
       Y12 = Y2 - Y1
       Z12 = Z2 - Z1
       IF (X12==ZERO.AND.Y12==ZERO.AND.Z12==ZERO) THEN
        IDE = 1
	GOTO 10
       ENDIF
C       
       X23 = X2 - X3
       Y23 = Y2 - Y3
       Z23 = Z2 - Z3
       IF (X23==ZERO.AND.Y23==ZERO.AND.Z23==ZERO) THEN
        IDE = 1
	GOTO 10
       ENDIF
C       
       X34 = X4 - X3
       Y34 = Y4 - Y3
       Z34 = Z4 - Z3
       IF (X34==ZERO.AND.Y34==ZERO.AND.Z34==ZERO) THEN
        IDE = 1
	GOTO 10
       ENDIF
C       
       X41 = X4 - X1
       Y41 = Y4 - Y1
       Z41 = Z4 - Z1
       IF (X41==ZERO.AND.Y41==ZERO.AND.Z41==ZERO) THEN
        IDE = 1
	GOTO 10
       ENDIF
C       
   10   CONTINUE
C     
       IF (A > AMAX )THEN
	    IT=IDE
		AMAX = A
	   END IF
      RETURN
      END
Chd|====================================================================
Chd|  DEGENES8                      source/elements/solid/solide/sdlen3.F
Chd|-- called by -----------
Chd|        S8CFORC3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        SDLEN_DEGE                    source/elements/solid/solide/sdlen3.F
Chd|        SDLEN_SMS                     source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|        INTAB                         source/implicit/ind_glob_k.F  
Chd|====================================================================
      SUBROUTINE DEGENES8(IXS,IDEGE)
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
#include      "vect01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*), IDEGE(*)
C-----------------------------------------------
C   External function
C-----------------------------------------------
      LOGICAL INTAB
      EXTERNAL INTAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,NC(8),NJ,J
C-----------------------------------------------
      DO I=LFT,LLT
       IDEGE(I)=0
       DO J=1,8
        NC(J)=IXS(J+1,I)
       END DO
       DO J=1,8
        NJ=NC(J)
        NC(J)=0
        IF (INTAB(8,NC,NJ)) IDEGE(I)=IDEGE(I)+1
        NC(J)=NJ
       END DO
C------number of degene nodes       
       IDEGE(I)=IDEGE(I)/2
      ENDDO
C      
      RETURN
      END
Chd|====================================================================
Chd|  SDLEN_SM11                    source/elements/solid/solide/sdlen3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SDLEN_SM11(VOLG,DELTAX,SMAX,IDEGE)
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
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IDEGE(*)
      my_real
     .   VOLG(*),DELTAX(*),SMAX(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C----------DELTAX crrection for degenerated element
      INTEGER I,IT
C-----------------------------------------------
      DO I=LFT,LLT
       DELTAX(I)=MIN(DELTAX(I),SIXTEEN*VOLG(I)*SMAX(I))
       IDEGE(I) = 1
      ENDDO
C      
      RETURN
      END
