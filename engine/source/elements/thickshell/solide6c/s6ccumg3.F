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
Chd|  S6CCUMG3                      source/elements/thickshell/solide6c/s6ccumg3.F
Chd|-- called by -----------
Chd|        S6CKE3                        source/elements/thickshell/solide6c/s6cke3.F
Chd|-- calls ---------------
Chd|        SETPROJKS6                    source/elements/thickshell/solide6c/setprojks6.F
Chd|        SET_RI33                      source/elements/shell/coqueba/cbasumg3.F
Chd|        SYSTRAN3                      source/elements/solid/solide8z/systran3.F
Chd|====================================================================
      SUBROUTINE S6CCUMG3(
     1   R11,     R21,     R31,     R12,
     2   R22,     R32,     R13,     R23,
     3   R33,     K11,     K12,     K13,
     4   K14,     K15,     K16,     K22,
     5   K23,     K24,     K25,     K26,
     6   K33,     K34,     K35,     K36,
     7   K44,     K45,     K46,     K55,
     8   K56,     K66,     X1,      X2,
     9   X3,      X4,      X5,      X6,
     A   Y1,      Y2,      Y3,      Y4,
     B   Y5,      Y6,      Z1,      Z2,
     C   Z3,      Z4,      Z5,      Z6,
     D   NEL)
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
C     REAL
      my_real
     .   R11(*), R12(*), R13(*),
     .   R21(*), R22(*), R23(*),
     .   R31(*), R32(*), R33(*),
     .   K11(9,*)   ,K12(9,*)   ,K13(9,*)    ,K14(9,*)  ,K15(9,*)   ,
     .   K16(9,*)   ,K22(9,*)   ,K23(9,*)   ,K24(9,*)   ,K25(9,*)   ,
     .   K26(9,*)   ,K33(9,*)   ,K34(9,*)   ,K35(9,*)   ,K36(9,*)    ,
     .   K44(9,*)   ,K45(9,*)   ,K46(9,*)  ,K55(9,*)   ,K56(9,*)    ,
     .   K66(9,*)  
      my_real
     .   X1(*), X2(*), X3(*), X4(*),X5(*), X6(*), 
     .   Y1(*), Y2(*), Y3(*), Y4(*),Y5(*), Y6(*), 
     .   Z1(*), Z2(*), Z3(*), Z4(*),Z5(*), Z6(*) 
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ISYM,L,J
C     REAL
      my_real
     .   Q(3,3,MVSIZ)
      my_real
     .    DR(3,3,MVSIZ),
     .    R1(3,3,MVSIZ),R2(3,3,MVSIZ),R3(3,3,MVSIZ),R4(3,3,MVSIZ),
     .    R5(3,3,MVSIZ),R6(3,3,MVSIZ),DI(6),XX,YY,ZZ,XY,XZ,YZ,RTR(6),
     .    ABC,XXYZ2,ZZXY2,YYXZ2,DETA
C-----------------------------------------------
C-----PROJECTION----
       DO I=1,NEL
        XX = X1(I)*X1(I)+X2(I)*X2(I)+X3(I)*X3(I)
     1      +X4(I)*X4(I)+X5(I)*X5(I)+X6(I)*X6(I)
        YY = Y1(I)*Y1(I)+Y2(I)*Y2(I)+Y3(I)*Y3(I)
     1      +Y4(I)*Y4(I)+Y5(I)*Y5(I)+Y6(I)*Y6(I)
        XY = X1(I)*Y1(I)+X2(I)*Y2(I)+X3(I)*Y3(I)
     1      +X4(I)*Y4(I)+X5(I)*Y5(I)+X6(I)*Y6(I)
        XZ = X1(I)*Z1(I)+X2(I)*Z2(I)+X3(I)*Z3(I)
     1      +X4(I)*Z4(I)+X5(I)*Z5(I)+X6(I)*Z6(I)
        YZ = Y1(I)*Z1(I)+Y2(I)*Z2(I)+Y3(I)*Z3(I)
     1      +Y4(I)*Z4(I)+Y5(I)*Z5(I)+Y6(I)*Z6(I)
        ZZ = Z1(I)*Z1(I)+Z2(I)*Z2(I)+Z3(I)*Z3(I)
     1      +Z4(I)*Z4(I)+Z5(I)*Z5(I)+Z6(I)*Z6(I)
        RTR(1)= YY+ZZ
        RTR(2)= XX+ZZ
        RTR(3)= XX+YY
        RTR(4)= -XY
        RTR(5)= -XZ
        RTR(6)= -YZ
C       
        ABC = RTR(1)*RTR(2)*RTR(3)
        XXYZ2 = RTR(1)*RTR(6)*RTR(6)
        YYXZ2 = RTR(2)*RTR(5)*RTR(5)
        ZZXY2 = RTR(3)*RTR(4)*RTR(4)
        DETA = ABC + TWO*RTR(4)*RTR(5)*RTR(6)-XXYZ2-YYXZ2-ZZXY2
        IF (DETA<EM20) THEN
         DETA=ONE
        ELSE
         DETA=ONE/DETA
        ENDIF
        DI(1) = (ABC-XXYZ2)*DETA/RTR(1)
        DI(2) = (ABC-YYXZ2)*DETA/RTR(2)
        DI(3) = (ABC-ZZXY2)*DETA/RTR(3)
        DI(4) = (RTR(5)*RTR(6)-RTR(4)*RTR(3))*DETA
        DI(5) = (RTR(6)*RTR(4)-RTR(5)*RTR(2))*DETA
        DI(6) = (RTR(4)*RTR(5)-RTR(6)*RTR(1))*DETA
        DR(1,1,I)=  DI(1)
        DR(2,2,I)=  DI(2)
        DR(3,3,I)=  DI(3)
        DR(1,2,I)=  DI(4)
        DR(1,3,I)=  DI(5)
        DR(2,3,I)=  DI(6)
        DR(2,1,I)=  DR(1,2,I)
        DR(3,1,I)=  DR(1,3,I)
        DR(3,2,I)=  DR(2,3,I)
       ENDDO
C--------ini-RJ---------      
       DO I=1,NEL
         DO L=1,3
          DO J=1,3
           R1(L,J,I)=ZERO
           R2(L,J,I)=ZERO
           R3(L,J,I)=ZERO
           R4(L,J,I)=ZERO
           R5(L,J,I)=ZERO
           R6(L,J,I)=ZERO
          ENDDO
         ENDDO
       END DO
C-------------Q<-Q^t---------------       
       DO I=1,NEL
        Q(1,1,I)=R11(I)
        Q(1,2,I)=R21(I)
        Q(1,3,I)=R31(I)
        Q(2,1,I)=R12(I)
        Q(2,2,I)=R22(I)
        Q(2,3,I)=R32(I)
        Q(3,1,I)=R13(I)
        Q(3,2,I)=R23(I)
        Q(3,3,I)=R33(I)
       ENDDO
C
       CALL SET_RI33(X1 ,Y1, Z1  ,R1  ,1,NEL)
       CALL SET_RI33(X2 ,Y2, Z2  ,R2  ,1,NEL)
       CALL SET_RI33(X3 ,Y3, Z3  ,R3  ,1,NEL)
       CALL SET_RI33(X4 ,Y4, Z4  ,R4  ,1,NEL)
       CALL SET_RI33(X5 ,Y5, Z5  ,R5  ,1,NEL)
       CALL SET_RI33(X6 ,Y6, Z6  ,R6  ,1,NEL)
       CALL SETPROJKS6(DR    ,R1     ,R2     ,R3     ,R4    ,
     1                 R5    ,R6     ,  
     2                 K11,K12,K13,K14,K15,K16,K22,K23,
     3                 K24,K25,K26,K33,K34,K35,K36,K44,
     4                 K45,K46,K55,K56,K66,
     5                 Q  ,1,NEL  )
C
      RETURN
C----------no projection  ---------    
      DO I=1,NEL
       Q(1,1,I)=R11(I)
       Q(2,1,I)=R21(I)
       Q(3,1,I)=R31(I)
       Q(1,2,I)=R12(I)
       Q(2,2,I)=R22(I)
       Q(3,2,I)=R32(I)
       Q(1,3,I)=R13(I)
       Q(2,3,I)=R23(I)
       Q(3,3,I)=R33(I)
      ENDDO
      ISYM=1
C------------QKQ^t
       CALL SYSTRAN3(1,NEL,Q,K11,ISYM) 
       CALL SYSTRAN3(1,NEL,Q,K22,ISYM) 
       CALL SYSTRAN3(1,NEL,Q,K33,ISYM) 
       CALL SYSTRAN3(1,NEL,Q,K44,ISYM) 
       CALL SYSTRAN3(1,NEL,Q,K55,ISYM) 
       CALL SYSTRAN3(1,NEL,Q,K66,ISYM) 
      ISYM=0
       CALL SYSTRAN3(1,NEL,Q,K12,ISYM) 
       CALL SYSTRAN3(1,NEL,Q,K13,ISYM) 
       CALL SYSTRAN3(1,NEL,Q,K14,ISYM) 
       CALL SYSTRAN3(1,NEL,Q,K15,ISYM) 
       CALL SYSTRAN3(1,NEL,Q,K16,ISYM) 
       CALL SYSTRAN3(1,NEL,Q,K23,ISYM) 
       CALL SYSTRAN3(1,NEL,Q,K24,ISYM) 
       CALL SYSTRAN3(1,NEL,Q,K25,ISYM) 
       CALL SYSTRAN3(1,NEL,Q,K26,ISYM) 
       CALL SYSTRAN3(1,NEL,Q,K34,ISYM) 
       CALL SYSTRAN3(1,NEL,Q,K35,ISYM) 
       CALL SYSTRAN3(1,NEL,Q,K36,ISYM) 
       CALL SYSTRAN3(1,NEL,Q,K45,ISYM) 
       CALL SYSTRAN3(1,NEL,Q,K46,ISYM) 
       CALL SYSTRAN3(1,NEL,Q,K56,ISYM) 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      RETURN
      END
