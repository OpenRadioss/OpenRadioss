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
Chd|  RINI33                        source/elements/joint/rjoint/rini33.F
Chd|-- called by -----------
Chd|        RINIT3                        source/elements/spring/rinit3.F
Chd|-- calls ---------------
Chd|        GET_SKEW                      source/elements/joint/rjoint/rini33.F
Chd|        GET_U_FUNC                    source/user_interface/uaccess.F
Chd|        GET_U_GEO                     source/user_interface/uaccess.F
Chd|        GET_U_PNU                     source/user_interface/uaccess.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE RINI33(NEL   ,IOUT   ,IPROP , IX    ,XL    ,
     3                  MASS   ,XINER ,STIFN  ,
     4                  STIFR ,VISCM  ,VISCR ,UVAR   ,NUVAR)
      USE MESSAGE_MOD
C-------------------------------------------------------------------------
C     This subroutine initialize springs using user properties.
C-------------------------------------------------------------------------
C----------+---------+---+---+--------------------------------------------
C VAR      | SIZE    |TYP| RW| DEFINITION
C----------+---------+---+---+--------------------------------------------
C IOUT     |  1      | I | R | OUTPUT FILE UNIT (L00 file)
C IPROP    |  1      | I | R | PROPERTY NUMBER
C----------+---------+---+---+--------------------------------------------
C IX       | 4*NEL   | I | R | SPRING CONNECTIVITY
C                            | IX(1,I) NODE 1 ID
C                            | IX(2,I) NODE 2 ID
C                            | IX(3,I) OPTIONAL NODE 3 ID
C                            | IX(4,I) SPRING ID
C----------+---------+---+---+--------------------------------------------
C MASS     |   NEL   | F | W | ELEMENT MASS
C XINER    |   NEL   | F | W | ELEMENT INERTIA (SPHERICAL)
C STIFM    |   NEL   | F | W | ELEMENT STIFNESS (TIME STEP)
C STIFR    |   NEL   | F | W | ELEMENT ROTATION STIFNESS (TIME STEP)
C VISCM    |   NEL   | F | W | ELEMENT VISCOSITY (TIME STEP)
C VISCR    |   NEL   | F | W | ELEMENT ROTATION VISCOSITY (TIME STEP)
C----------+---------+---+---+--------------------------------------------
C UVAR     |NUVAR*NEL| F | W | USER ELEMENT VARIABLES
C NUVAR    |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C----------+---------+---+---+--------------------------------------------
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
C----------------------------------------------------------
C   D u m m y   A r g u m e n t s   a n d   F u n c t i o n
C----------------------------------------------------------
      INTEGER NEL,IOUT,IPROP,NUVAR,IX(4,MVSIZ)
      my_real 
     .        MASS(NEL) ,XINER(NEL) ,STIFN(NEL),XL(MVSIZ,3) ,
     .        STIFR(NEL),VISCM(NEL) ,VISCR(NEL),UVAR(NUVAR,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IDSK1,IDSK2,JTYP,SKFLG,IFKNX,IFKNY,IFKNZ,
     .        IFKRX,IFKRY,IFKRZ,IFCNX,IFCNY,IFCNZ,IFCRX,IFCRY,IFCRZ,
     .        GET_U_PNU,GET_SKEW,KFUNC,KMAT,KPROP
      my_real KXX,KYY,KZZ,KRX,KRY,KRZ,KNN,KR,X1,Y1,Z1,LEN2,
     .        K1,K2,K3,K4,K5,K6,C1,C2,C3,C4,C5,C6,KTT,KRR,CTT,CRR,
     .        CXX,CYY,CZZ,CRX,CRY,CRZ, DERI,XF,GET_U_FUNC,
     .        U(LSKEW),V(LSKEW),A(LSKEW),B(LSKEW),EX(LSKEW),GET_U_GEO
C-----------------------------------------------
      EXTERNAL  GET_U_GEO,GET_SKEW
      PARAMETER (KFUNC=29)
      PARAMETER (KMAT=31)
      PARAMETER (KPROP=33)
C=======================================================================
      JTYP = NINT(GET_U_GEO(1,IPROP))
      IDSK1= NINT(GET_U_GEO(2,IPROP))
      IDSK2= NINT(GET_U_GEO(3,IPROP))
      SKFLG= NINT(GET_U_GEO(14,IPROP))
      KXX  = GET_U_GEO(4,IPROP)
      KYY  = GET_U_GEO(5,IPROP)
      KZZ  = GET_U_GEO(6,IPROP)
      KRX  = GET_U_GEO(7,IPROP)
      KRY  = GET_U_GEO(8,IPROP)
      KRZ  = GET_U_GEO(9,IPROP)
      KNN  = GET_U_GEO(10,IPROP)
      IFKNX = GET_U_PNU(1,IPROP,KFUNC) 
      IFKNY = GET_U_PNU(2,IPROP,KFUNC) 
      IFKNZ = GET_U_PNU(3,IPROP,KFUNC) 
      IFKRX = GET_U_PNU(4,IPROP,KFUNC) 
      IFKRY = GET_U_PNU(5,IPROP,KFUNC) 
      IFKRZ = GET_U_PNU(6,IPROP,KFUNC)
C----
      K1 = KXX
      K2 = KYY
      K3 = KZZ
      K4 = KRX
      K5 = KRY
      K6 = KRZ
      IF (IFKNX/=0) THEN
        XF = GET_U_FUNC(IFKNX,ZERO,DERI)
        K1  = MAX(KXX*DERI, EM20)
      ENDIF
      IF (IFKNY/=0) THEN
        XF = GET_U_FUNC(IFKNY,ZERO,DERI)
        K2  = MAX(KYY*DERI, EM20)
      ENDIF
      IF (IFKNZ/=0) THEN
        XF = GET_U_FUNC(IFKNZ,ZERO,DERI)
        K3  = MAX(KZZ*DERI, EM20)
      ENDIF
      IF (IFKRX/=0) THEN
        XF = GET_U_FUNC(IFKRX,ZERO,DERI)
        K4  = MAX(KRX*DERI, EM20)
      ENDIF
      IF (IFKRY/=0) THEN
        XF = GET_U_FUNC(IFKRY,ZERO,DERI)
        K5  = MAX(KRY*DERI, EM20) 
      ENDIF
      IF (IFKRZ/=0) THEN
        XF = GET_U_FUNC(IFKRZ,ZERO,DERI)
        K6  = MAX(KRZ*DERI, EM20)
      ENDIF
      CXX = GET_U_GEO(21,IPROP)
      CYY = GET_U_GEO(22,IPROP)
      CZZ = GET_U_GEO(23,IPROP)
      CRX = GET_U_GEO(24,IPROP)
      CRY = GET_U_GEO(25,IPROP)
      CRZ = GET_U_GEO(26,IPROP)
C
      IFCNX = GET_U_PNU(7,IPROP,KFUNC) 
      IFCNY = GET_U_PNU(8,IPROP,KFUNC) 
      IFCNZ = GET_U_PNU(9,IPROP,KFUNC) 
      IFCRX = GET_U_PNU(10,IPROP,KFUNC) 
      IFCRY = GET_U_PNU(11,IPROP,KFUNC) 
      IFCRZ = GET_U_PNU(12,IPROP,KFUNC)
C
      C1 = CXX
      C2 = CYY
      C3 = CZZ
      C4 = CRX
      C5 = CRY
      C6 = CRZ
      IF (IFCNX/=0) THEN
        XF = GET_U_FUNC(IFCNX,ZERO,DERI)
        C1  = MAX(CXX*DERI, EM20)
      ENDIF
      IF (IFCNY/=0) THEN
        XF = GET_U_FUNC(IFCNY,ZERO,DERI)
        C2  = MAX(CYY*DERI, EM20)
      ENDIF
      IF (IFCNZ/=0) THEN
        XF = GET_U_FUNC(IFCNZ,ZERO,DERI)
        C3  = MAX(CZZ*DERI, EM20)
      ENDIF
      IF (IFCRX/=0) THEN
        XF = GET_U_FUNC(IFCRX,ZERO,DERI)
        C4  = MAX(CRX*DERI, EM20)
      ENDIF
      IF (IFCRY/=0) THEN
        XF = GET_U_FUNC(IFCRY,ZERO,DERI)
        C5  = MAX(CRY*DERI, EM20) 
      ENDIF
      IF (IFCRZ/=0) THEN
        XF = GET_U_FUNC(IFCRZ,ZERO,DERI)
        C6  = MAX(CRZ*DERI, EM20)
      ENDIF
          KTT = MAX(K1,K2,K3)
          KRR = MAX(K4,K5,K6)
          CTT = MAX(C1,C2,C3)
          CRR = MAX(C4,C5,C6)
C-------   local frame 
      IERR=IERR+GET_SKEW(IOUT,JTYP,SKFLG,IDSK1,IDSK2,U,V,EX,A,B)
      DO I=1,NEL
        X1 = XL(I,1)
        Y1 = XL(I,2)
        Z1 = XL(I,3)
        XL(I,1)=EX(1)*X1+EX(2)*Y1+EX(3)*Z1
        XL(I,2)=EX(4)*X1+EX(5)*Y1+EX(6)*Z1
        XL(I,3)=EX(7)*X1+EX(8)*Y1+EX(9)*Z1
      ENDDO
C--------------------------------------
C       ELEMENT INITIALIZATION
C--------------------------------------
        DO I=1,NEL
          MASS(I)   = ZERO
          XINER(I)  = ZERO
          UVAR(1,I) = XL(I,1)
          UVAR(2,I) = XL(I,2)
          UVAR(3,I) = XL(I,3)
          LEN2=XL(I,1)*XL(I,1)+XL(I,2)*XL(I,2)+XL(I,3)*XL(I,3)
          UVAR(4,I) = A(1)
          UVAR(5,I) = A(2)
          UVAR(6,I) = A(3)
          UVAR(7,I) = A(4)
          UVAR(8,I) = A(5)
          UVAR(9,I) = A(6)
          UVAR(10,I)= A(7)
          UVAR(11,I)= A(8)
          UVAR(12,I)= A(9)
          UVAR(22,I)= EX(1)
          UVAR(23,I)= EX(2)
          UVAR(24,I)= EX(3)
          UVAR(25,I)= EX(4)
          UVAR(26,I)= EX(5)
          UVAR(27,I)= EX(6)
          UVAR(28,I)= EX(7)
          UVAR(29,I)= EX(8)
          UVAR(30,I)= EX(9)	  
C
          KR = KNN*MAX(ONE,LEN2)
          UVAR(19,I)= KXX
          UVAR(20,I)= KYY
          UVAR(21,I)= KZZ
	  
          IF(JTYP>=2.AND.JTYP<=4) THEN
            UVAR(31,I)= KRX
            UVAR(32,I)= KR
            UVAR(33,I)= KR
          ELSEIF(JTYP==5) THEN
            UVAR(31,I)= KR
            UVAR(32,I)= KRY
            UVAR(33,I)= KRZ
          ELSEIF(JTYP>=6.AND.JTYP<=8) THEN
            UVAR(31,I)= KR
            UVAR(32,I)= KR
            UVAR(33,I)= KR
          ELSE
            UVAR(31,I)= KRX
            UVAR(32,I)= KRY
            UVAR(33,I)= KRZ
          ENDIF
C
          UVAR(34,I)= ZERO
          UVAR(35,I)= ZERO
          UVAR(36,I)= ZERO
          UVAR(37,I)= ZERO
          UVAR(38,I)= ZERO
          UVAR(39,I)= ZERO	  
C
          STIFN(I) = KTT
          STIFR(I) = KRR+KTT*LEN2
          VISCM(I) = CTT
          VISCR(I) = CRR
        ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  GET_SKEW                      source/elements/joint/rjoint/rini33.F
Chd|-- called by -----------
Chd|        RINI33                        source/elements/joint/rjoint/rini33.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        PROD_AB                       source/elements/joint/rjoint/rini33.F
Chd|        PROD_ABT                      source/elements/joint/rjoint/rini33.F
Chd|        PROD_ATB                      source/elements/joint/rjoint/rini33.F
Chd|        QROT33                        source/elements/joint/rjoint/rini33.F
Chd|        ROT12                         source/elements/joint/rjoint/rini33.F
Chd|        GET_U_SKEW                    source/user_interface/uaccess.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      INTEGER FUNCTION GET_SKEW(IOUT,JTYP,SKFLG,IDSK1,IDSK2,U,V,X,A,B)
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "param_c.inc"
C----------------------------------------------------------
C   D u m m y   A r g u m e n t s   a n d   F u n c t i o n
C----------------------------------------------------------
      INTEGER IOUT,JTYP,IDSK1,IDSK2,SKFLG
      my_real U(LSKEW),V(LSKEW),X(LSKEW),A(LSKEW),B(LSKEW)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IERR1,ISK1,ISK2,IP1,IP2, GET_U_SKEW,N1,N2,N3
      my_real 
     .        NX,NY,NZ,CO,SI,KSI,    
     .        T(3),Q(LSKEW),Q1(LSKEW),Q2(LSKEW),X1(LSKEW),X2(LSKEW)
C-----------------------------
      EXTERNAL GET_U_SKEW
C=======================================================================
C
      IERR1 = 0
      ISK1 = GET_U_SKEW(IDSK1,N1,N2,N3,U)
      ISK2 = GET_U_SKEW(IDSK2,N1,N2,N3,V)
C
      IF (JTYP==5) THEN
C---     universal joint
        IF ((U(1)*V(1)+U(2)*V(2)+U(3)*V(3))<=EM10) THEN
          X(1) = U(2)*V(3) - U(3)*V(2)
          X(2) = U(3)*V(1) - U(1)*V(3)
          X(3) = U(1)*V(2) - U(2)*V(1)
          NX = SQRT(X(1)*X(1)+X(2)*X(2)+X(3)*X(3))
          X(1) = X(1) / NX
          X(2) = X(2) / NX
          X(3) = X(3) / NX
          X(4) = U(1) 
          X(5) = U(2)
          X(6) = U(3) 
          X(7) = V(1)
          X(8) = V(2)
          X(9) = V(3)
C
          CALL PROD_ATB(V,U,A)
          CALL PROD_ATB(U,V,B)
        ELSE
C          IERR1 = 1
C          WRITE(*,*)' ** ERROR/NON ORTHOGONAL UNIVERSAL JOINT AXES'
C          WRITE(IOUT,'(//A,A//)')' ** ERROR PROPERTY SET INPUT',
C     .     ' NON ORTHOGONAL UNIVERSAL JOINT AXES'
C         IERR1 = IERR1 + 1
           CALL ANCMSG(MSGID=389,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1)
c     .                   I1=ID,
c     .                   C1=TITR)
        ENDIF
      ELSE
C---------------------------
        IF (SKFLG==1) THEN
c----   first skew is used
          IF (ISK2==0) THEN
            DO I=1,9
              X(I) = U(I)
              A(I) = U(I)
              B(I) = ZERO
            ENDDO
          ELSE
            DO I=1,9
              X(I) = U(I)
              B(I) = ZERO
            END DO
            CALL PROD_ATB(V,U,A)
          ENDIF
C------
        ELSE
c----   mean skew is calculated
          DO I=1,9
            B(I) = 0.
          END DO
          CALL PROD_ATB(U,V,Q)
          CALL ROT12(Q, T, CO, SI)
          CALL QROT33(Q1, T, CO, SI)
C
          CALL PROD_ATB(V,U,Q)
          CALL ROT12(Q, T, CO, SI)
          CALL QROT33(Q2, T, CO, SI)
          A(1) = HALF * (Q1(1) + Q2(1))
          A(2) = HALF * (Q1(2) + Q2(4))
          A(3) = HALF * (Q1(3) + Q2(7))
          A(4) = HALF * (Q1(4) + Q2(2))
          A(5) = HALF * (Q1(5) + Q2(5))
          A(6) = HALF * (Q1(6) + Q2(8))
          A(7) = HALF * (Q1(7) + Q2(3))
          A(8) = HALF * (Q1(8) + Q2(6))
          A(9) = HALF * (Q1(9) + Q2(9))	  
C
          CALL PROD_AB(U,A,X1)
          CALL PROD_ABT(V,A,X2)
          DO I=1,9
            X(I) = HALF * (X1(I) + X2(I))
          END DO
C-------------------------------------
        ENDIF
      ENDIF
C-----------------------------------------------
      GET_SKEW = IERR1
      RETURN
      END
C
Chd|====================================================================
Chd|  QROT33                        source/elements/joint/rjoint/rini33.F
Chd|-- called by -----------
Chd|        GET_SKEW                      source/elements/joint/rjoint/rini33.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE QROT33(SKEW, T, C, S)
C-------------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C----------------------------------------------------------
C   D u m m y   A r g u m e n t s   a n d   F u n c t i o n
C----------------------------------------------------------
      my_real T(3), SKEW(LSKEW)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real E11,E22,E33,E12,E21,E13,E31,E23,E32,
     .        U1,U2,U3, S,C,CI,SI2
C=======================================================================
      CI = ONE - C
      U1 = T(1)
      U2 = T(2)
      U3 = T(3)

      E11 = U1 * U1 *CI + C
      E22 = U2 * U2 *CI + C
      E33 = U3 * U3 *CI + C

      E12 = U1 * U2 * CI   
      E21 = E12 + U3 * S
      E12 = E12 - U3 * S

      E13 = U1 * U3 * CI
      E31 = E13 - U2 * S
      E13 = E13 + U2 * S

      E23 = U2 * U3 * CI
      E32 = E23 + U1 * S
      E23 = E23 - U1 * S
C
      SKEW(1) =  E11  
      SKEW(4) =  E12    
      SKEW(7) =  E13    
      SKEW(2) =  E21    
      SKEW(5) =  E22   
      SKEW(8) =  E23  
      SKEW(3) =  E31  
      SKEW(6	) =  E32  
      SKEW(9) =  E33  
      RETURN
      END
Chd|====================================================================
Chd|  ROT12                         source/elements/joint/rjoint/rini33.F
Chd|-- called by -----------
Chd|        GET_SKEW                      source/elements/joint/rjoint/rini33.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ROT12(SKEW, T, C, S)
C-------------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C----------------------------------------------------------
C   D u m m y   A r g u m e n t s   a n d   F u n c t i o n
C----------------------------------------------------------
      my_real ROT(3), T(3), SKEW(LSKEW), KSI, S, C
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real E,E11,E22,E33,E12,E21,E13,E31,E23,E32,SI2,NR,
     .        UMI 
      DATA  UMI/-1.0/
C=======================================================================
C
       PI = 2.*ATAN2(ONE,ZERO)
C---   first skew rotation
       E11 = SKEW(1)   
       E12 = SKEW(4)   
       E13 = SKEW(7)   
       E21 = SKEW(2)   
       E22 = SKEW(5)   
       E23 = SKEW(8)   
       E31 = SKEW(3)   
       E32 = SKEW(6)   
       E33 = SKEW(9)  
       E = E11+E22+E33
       C = HALF * (E - 1.)
       C = MIN(C,ONE)
       C = MAX(C,UMI)
       KSI = ACOS(C)
       S   = SIN(KSI)
       IF(S==ZERO) THEN
         SI2 = ZERO
       ELSE
         SI2  = HALF / S
       ENDIF
       T(1) = (E32 - E23) * SI2
       T(2) = (E13 - E31) * SI2
       T(3) = (E21 - E12) * SI2
       NR = SQRT(T(1)*T(1)+T(2)*T(2)+T(3)*T(3))
       IF (NR/=ZERO) NR = ONE/NR
       T(1) = T(1)*NR
       T(2) = T(2)*NR 
       T(3) = T(3)*NR 
C
       C = HALF*(C+ ONE)
       S = SQRT(ONE-C)
       C = SQRT(C)
C
      RETURN
      END

C==================================================== 
Chd|====================================================================
Chd|  PROD_ABT                      source/elements/joint/rjoint/rini33.F
Chd|-- called by -----------
Chd|        GET_SKEW                      source/elements/joint/rjoint/rini33.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PROD_ABT(A,B,X)
#include      "implicit_f.inc"
#include      "param_c.inc"
      INTEGER I,J
      my_real A(LSKEW),B(LSKEW),X(LSKEW)
C
      X(1)=A(1)*B(1)+A(4)*B(4)+A(7)*B(7) 
      X(4)=A(1)*B(2)+A(4)*B(5)+A(7)*B(8) 
      X(7)=A(1)*B(3)+A(4)*B(6)+A(7)*B(9) 
      X(2)=A(2)*B(1)+A(5)*B(4)+A(8)*B(7) 
      X(5)=A(2)*B(2)+A(5)*B(5)+A(8)*B(8) 
      X(8)=A(2)*B(3)+A(5)*B(6)+A(8)*B(9) 
      X(3)=A(3)*B(1)+A(6)*B(4)+A(9)*B(7) 
      X(6)=A(3)*B(2)+A(6)*B(5)+A(9)*B(8) 
      X(9)=A(3)*B(3)+A(6)*B(6)+A(9)*B(9) 
      RETURN
      END
C==================================================== 
Chd|====================================================================
Chd|  PROD_ATB                      source/elements/joint/rjoint/rini33.F
Chd|-- called by -----------
Chd|        INIT_SKEW45                   source/elements/joint/rjoint/rini45.F
Chd|        RINI33_RB                     source/elements/joint/rjoint/rini33_rb.F
Chd|        GET_SKEW                      source/elements/joint/rjoint/rini33.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PROD_ATB(A,B,X)
#include      "implicit_f.inc"
#include      "param_c.inc"
      my_real A(LSKEW),B(LSKEW),X(LSKEW)
C
      X(1)=A(1)*B(1)+A(2)*B(2)+A(3)*B(3) 
      X(2)=A(4)*B(1)+A(5)*B(2)+A(6)*B(3) 
      X(3)=A(7)*B(1)+A(8)*B(2)+A(9)*B(3) 
      X(4)=A(1)*B(4)+A(2)*B(5)+A(3)*B(6) 
      X(5)=A(4)*B(4)+A(5)*B(5)+A(6)*B(6) 
      X(6)=A(7)*B(4)+A(8)*B(5)+A(9)*B(6) 
      X(7)=A(1)*B(7)+A(2)*B(8)+A(3)*B(9) 
      X(8)=A(4)*B(7)+A(5)*B(8)+A(6)*B(9) 
      X(9)=A(7)*B(7)+A(8)*B(8)+A(9)*B(9) 
      RETURN
      END
C==================================================== 
Chd|====================================================================
Chd|  PROD_AB                       source/elements/joint/rjoint/rini33.F
Chd|-- called by -----------
Chd|        GET_SKEW                      source/elements/joint/rjoint/rini33.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PROD_AB(A,B,X)
#include      "implicit_f.inc"
#include      "param_c.inc"
      my_real A(LSKEW),B(LSKEW),X(LSKEW)
C
      X(1)=A(1)*B(1)+A(4)*B(2)+A(7)*B(3) 
      X(2)=A(2)*B(1)+A(5)*B(2)+A(8)*B(3) 
      X(3)=A(3)*B(1)+A(6)*B(2)+A(9)*B(3) 
      X(4)=A(1)*B(4)+A(4)*B(5)+A(7)*B(6) 
      X(5)=A(2)*B(4)+A(5)*B(5)+A(8)*B(6) 
      X(6)=A(3)*B(4)+A(6)*B(5)+A(9)*B(6) 
      X(7)=A(1)*B(7)+A(4)*B(8)+A(7)*B(9) 
      X(8)=A(2)*B(7)+A(5)*B(8)+A(8)*B(9) 
      X(9)=A(3)*B(7)+A(6)*B(8)+A(9)*B(9) 
      RETURN
      END
