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
Cqz-------linear only----------------------------
Chd|====================================================================
Chd|  R4MAT3                        source/elements/spring/r4mat3.F
Chd|-- called by -----------
Chd|        R4KE3                         source/elements/spring/r4ke3.F
Chd|-- calls ---------------
Chd|        RKENONL                       source/elements/spring/r4mat3.F
Chd|====================================================================
      SUBROUTINE R4MAT3(JFT    ,JLT    ,GEO    ,KX     ,MGN    ,
     1                  AL0    ,FX     ,DX     ,TF     ,NPF    ,
     2                  POS    ,IGEO   )
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
#include      "param_c.inc"
#include      "impl2_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT    ,JLT, MGN(*),NPF(*),IGEO(NPROPGI,*)
C     REAL ou REAL*8
      my_real
     .   GEO(NPROPG,*), KX(*),AL0(*),FX(*),DX(*),TF(*),POS(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ILEN,ILENG
      INTEGER IECROU(MVSIZ), IFUNC(MVSIZ),IFUNC2(MVSIZ)
C     REAL ou REAL*8
      my_real
     .   A(MVSIZ)
C-----------------------------------------------
C
      DO I=JFT,JLT
       KX(I)=GEO(2,MGN(I))
      ENDDO
      ILEN = 0
      DO I=JFT,JLT
       A(I)   =GEO(10,MGN(I))
C       KX(I)=A(I)*KX(I)
       ILENG=NINT(GEO(93,MGN(I)))
       IF(ILENG.NE.0) ILEN = 1
      ENDDO
      IF(ILEN.NE.0) THEN
       DO I=JFT,JLT
        ILENG=NINT(GEO(93,MGN(I)))
        IF(ILENG.NE.0)THEN
          KX(I)=KX(I)/AL0(I)
        ENDIF
       ENDDO
      ENDIF
      IF (ISMDISP.GT.0.OR.ISPRN.EQ.1) THEN
       DO I=JFT,JLT
        IECROU(I)=NINT(GEO(7,MGN(I)))
C        IFUNC(I) =NINT(GEO(4,MGN(I)))
C        IFUNC2(I) =NINT(GEO(18,MGN(I)))
        IFUNC(I) =IGEO(101,MGN(I))
        IFUNC2(I)=IGEO(103,MGN(I))
       ENDDO
       CALL RKENONL(JFT    ,JLT    ,KX     ,FX     ,DX     ,
     .              IECROU ,IFUNC  ,IFUNC2 ,A      ,TF     ,
     .              NPF    ,POS    )
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  RKENONL                       source/elements/spring/r4mat3.F
Chd|-- called by -----------
Chd|        R12MAT3                       source/elements/spring/r12mat3.F
Chd|        R13MAT3                       source/elements/spring/r13mat3.F
Chd|        R4MAT3                        source/elements/spring/r4mat3.F
Chd|-- calls ---------------
Chd|        ES_FUNC                       source/elements/spring/r4mat3.F
Chd|        VINTER2                       source/tools/curve/vinter.F   
Chd|        VINTER2DP                     source/tools/curve/vinter.F   
Chd|====================================================================
      SUBROUTINE RKENONL(JFT    ,JLT    ,KX     ,FX     ,DX     ,
     .                   IECROU ,IFUNC  ,IFUNC2 ,A      ,TF     ,
     .                   NPF    ,POS    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "scr05_c.inc"
#include      "impl1_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT,NPF(*),IECROU(*), IFUNC(*),IFUNC2(*)
C     REAL ou REAL*8
      my_real
     .   FX(*), KX(*), DX(*), TF(*), POS(4,*),A(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C----s---1----+----2----+----3----+----4----+----5----+----6----+----7--
      INTEGER 
     . I, J,JPOS(MVSIZ), JLEN(MVSIZ),JAD(MVSIZ),
     . JPOS0(MVSIZ),JFUNC,JFUNC2,INDEX(MVSIZ),NC
      my_real
     .   XX(MVSIZ) ,DYDX(MVSIZ) ,YY(MVSIZ),E_SEG,E_T,E_MIN,
     .   X0(MVSIZ),F0(MVSIZ)
C----s---1----+----2----+----3----+----4----+----5----+----6----+----7--
C-c'est fait deja--
C      DO I=JFT,JLT
C        DX(I)=DX(I)/XL0(I)
C      ENDDO
C-------------------------------------
C        VECTOR INTERPOLATION (ADRESS)
C-------------------------------------
      NC = 0
      IF (ILINE.EQ.1.OR.
     .   (ISMDISP.GT.0.AND.NCYCLE.EQ.1.AND.IMCONV.EQ.1)) THEN
       DO I=JFT,JLT
       IF (IFUNC(I).GT.0) THEN
        NC = NC + 1
        INDEX(NC) = I
       ENDIF 
       ENDDO
       DO J=1,NC
        I = INDEX(J)
        CALL ES_FUNC(IFUNC(I),TF,NPF,E_SEG)
        KX(I)= A(I)*MAX(EM6,ABS(E_SEG))
       ENDDO
      ELSE
      DO I=JFT,JLT
       IF (IFUNC(I).GT.0.AND.IECROU(I).EQ.0.AND.ABS(DX(I)).GT.EM20) THEN
        NC = NC + 1
        INDEX(NC) = I
       ENDIF 
      ENDDO
      IF (NC.EQ.0) RETURN
C
      DO J=1,NC
       I = INDEX(J)
       JPOS(J) = 0
C       JPOS(J) = NINT(POS(1,I))
       JFUNC=MAX(1,IFUNC(I))
       JAD(J)  = NPF(JFUNC) / 2  + 1
       JLEN(J) = NPF(JFUNC+1) / 2  - JAD(J)  - JPOS(J)
       XX(J)   = DX(I)
       JPOS0(J) = JPOS(J)
       X0(J)   = ZERO
      ENDDO
C
      IF (IRESP.EQ.1) THEN
        CALL VINTER2DP(TF,JAD ,JPOS0,JLEN ,NC,X0 ,DYDX ,F0)
        CALL VINTER2DP(TF,JAD ,JPOS ,JLEN ,NC,XX ,DYDX ,YY)
      ELSE
        CALL VINTER2(TF,JAD ,JPOS0,JLEN ,NC,X0 ,DYDX ,F0 )
        CALL VINTER2(TF,JAD ,JPOS ,JLEN ,NC,XX ,DYDX ,YY )
      ENDIF 
C
      DO J=1,NC
       I = INDEX(J)
       E_SEG = ABS((FX(I)-F0(J))/XX(J))
       E_T = ABS(DYDX(J))
       E_MIN = EM06*KX(I)
       KX(I)= A(I)*MAX(E_MIN,E_SEG,E_T)
      ENDDO
      ENDIF 
C
      RETURN
      END
Chd|====================================================================
Chd|  ES_FUNC                       source/elements/spring/r4mat3.F
Chd|-- called by -----------
Chd|        RKENONL                       source/elements/spring/r4mat3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ES_FUNC(IFUNCT  ,TF     ,NPF    ,E_S    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPF(*),IFUNCT
C     REAL ou REAL*8
      my_real
     .   TF(*),E_S
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C----s---1----+----2----+----3----+----4----+----5----+----6----+----7--
      INTEGER 
     . J,J1,NPOINT,N0
      my_real
     .   X,Y,Y0,E1,E2,XN,YN
C----s---1----+----2----+----3----+----4----+----5----+----6----+----7--
      E_S = ZERO 
       NPOINT=(NPF(IFUNCT+1)-NPF(IFUNCT))/2+1
       Y0 = ZERO
       N0 = 0
       IF (TF(NPF(IFUNCT)).LT.ZERO) N0 = NPOINT+1
       DO J=2,NPOINT
        J1=2*(J-2)
        X=TF(NPF(IFUNCT)+J1)
        IF (X.GE.ZERO) THEN
         IF (X.EQ.ZERO) Y0=TF(NPF(IFUNCT)+J1+1)
         N0=J
         GOTO 10
        ENDIF
       ENDDO
 10    CONTINUE
C------only traction--------
       IF (N0.LE.2) THEN
        X=TF(NPF(IFUNCT))
        Y=TF(NPF(IFUNCT)+1)-Y0
        IF (X.EQ.ZERO) THEN
         X=TF(NPF(IFUNCT)+2)
         Y=TF(NPF(IFUNCT)+3)-Y0
	ELSE
         X=TF(NPF(IFUNCT)+2)-X
         Y=TF(NPF(IFUNCT)+3)-Y
        ENDIF
        IF (ABS(X).GT.ZERO) E_S= Y/X
C------only compression--------
       ELSEIF (N0.GE.NPOINT) THEN
        J1=2*(N0-3)
        X=TF(NPF(IFUNCT)+J1)
        Y=TF(NPF(IFUNCT)+J1+1)-Y0
C------w/o zero point--------------	
        IF (N0.EQ.(NPOINT+1)) THEN
         J1=2*(N0-4)
         X=TF(NPF(IFUNCT)+J1)-X
         Y=TF(NPF(IFUNCT)+J1+1)-Y
        ENDIF
        IF (ABS(X).GT.ZERO) E_S= Y/X
       ELSE
C------compression 1er point
        J1=2*(N0-3)
        XN=TF(NPF(IFUNCT)+J1)
        YN=TF(NPF(IFUNCT)+J1+1)-Y0
        E1 = ZERO
        IF (ABS(XN).GT.ZERO) E1= YN/XN
C------traction 1er point
        J1=2*(N0-2)
        X=TF(NPF(IFUNCT)+J1)
        IF (X.EQ.ZERO) THEN
         J1=J1+2
         X=TF(NPF(IFUNCT)+J1)
         Y=TF(NPF(IFUNCT)+J1+1)-Y0
         E2 = ZERO
         IF (ABS(X).GT.ZERO) E2= Y/X
         E_S= HALF*(E1+E2)
C------w/o point (0,0)-----
	ELSE
         Y=TF(NPF(IFUNCT)+J1+1)-Y0
         E_S= ZERO
         IF (ABS(X-XN).GT.ZERO) E_S= (Y-YN)/(X-XN)
        ENDIF
       ENDIF
C
      RETURN
      END
