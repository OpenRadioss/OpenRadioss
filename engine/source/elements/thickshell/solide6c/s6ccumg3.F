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
Chd|  S6CCUMG3                      source/elements/thickshell/solide6c/s6ccumg3.F
Chd|-- called by -----------
Chd|        S6CKE3                        source/elements/thickshell/solide6c/s6cke3.F
Chd|        S6CKEL3                       source/elements/thickshell/solide6c/s6cke3.F
Chd|-- calls ---------------
Chd|        SETPROJKS6                    source/elements/thickshell/solide6c/s6ccumg3.F
Chd|        SET_RI33                      source/elements/shell/coqueba/cbasumg3.F
Chd|        SYSTRAN3                      source/elements/solid/solide8z/s8zcumg3.F
Chd|====================================================================
      SUBROUTINE S6CCUMG3(
     .   R11, R21, R31, R12, R22, R32, R13, R23, R33,
     .   K11,K12,K13,K14,K15,K16,K22,K23,
     .   K24,K25,K26,K33,K34,K35,K36,K44,
     .   K45,K46,K55,K56,K66,
     .   X1, X2, X3, X4, X5, X6, 
     .   Y1, Y2, Y3, Y4, Y5, Y6, 
     .   Z1, Z2, Z3, Z4, Z5, Z6) 
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
C     REAL ou REAL*8
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
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ISYM,L,J
C     REAL ou REAL*8
      my_real
     .   Q(3,3,MVSIZ)
      my_real
     .    DR(3,3,MVSIZ),
     .    R1(3,3,MVSIZ),R2(3,3,MVSIZ),R3(3,3,MVSIZ),R4(3,3,MVSIZ),
     .    R5(3,3,MVSIZ),R6(3,3,MVSIZ),DI(6),XX,YY,ZZ,XY,XZ,YZ,RTR(6),
     .    ABC,XXYZ2,ZZXY2,YYXZ2,DETA
C-----------------------------------------------
C-----PROJECTION----
       DO I=LFT,LLT
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
        IF (DETA.LT.EM20) THEN
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       CALL SET_RI33(X1 ,Y1, Z1  ,R1  ,LFT,LLT)
       CALL SET_RI33(X2 ,Y2, Z2  ,R2  ,LFT,LLT)
       CALL SET_RI33(X3 ,Y3, Z3  ,R3  ,LFT,LLT)
       CALL SET_RI33(X4 ,Y4, Z4  ,R4  ,LFT,LLT)
       CALL SET_RI33(X5 ,Y5, Z5  ,R5  ,LFT,LLT)
       CALL SET_RI33(X6 ,Y6, Z6  ,R6  ,LFT,LLT)
       CALL SETPROJKS6(DR    ,R1     ,R2     ,R3     ,R4    ,
     1                 R5    ,R6     ,  
     2                 K11,K12,K13,K14,K15,K16,K22,K23,
     3                 K24,K25,K26,K33,K34,K35,K36,K44,
     4                 K45,K46,K55,K56,K66,
     5                 Q  ,LFT,LLT  )
C
      RETURN
C----------no projection  ---------    
      DO I=LFT,LLT
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
       CALL SYSTRAN3(LFT,LLT,Q,K11,ISYM) 
       CALL SYSTRAN3(LFT,LLT,Q,K22,ISYM) 
       CALL SYSTRAN3(LFT,LLT,Q,K33,ISYM) 
       CALL SYSTRAN3(LFT,LLT,Q,K44,ISYM) 
       CALL SYSTRAN3(LFT,LLT,Q,K55,ISYM) 
       CALL SYSTRAN3(LFT,LLT,Q,K66,ISYM) 
      ISYM=0
       CALL SYSTRAN3(LFT,LLT,Q,K12,ISYM) 
       CALL SYSTRAN3(LFT,LLT,Q,K13,ISYM) 
       CALL SYSTRAN3(LFT,LLT,Q,K14,ISYM) 
       CALL SYSTRAN3(LFT,LLT,Q,K15,ISYM) 
       CALL SYSTRAN3(LFT,LLT,Q,K16,ISYM) 
       CALL SYSTRAN3(LFT,LLT,Q,K23,ISYM) 
       CALL SYSTRAN3(LFT,LLT,Q,K24,ISYM) 
       CALL SYSTRAN3(LFT,LLT,Q,K25,ISYM) 
       CALL SYSTRAN3(LFT,LLT,Q,K26,ISYM) 
       CALL SYSTRAN3(LFT,LLT,Q,K34,ISYM) 
       CALL SYSTRAN3(LFT,LLT,Q,K35,ISYM) 
       CALL SYSTRAN3(LFT,LLT,Q,K36,ISYM) 
       CALL SYSTRAN3(LFT,LLT,Q,K45,ISYM) 
       CALL SYSTRAN3(LFT,LLT,Q,K46,ISYM) 
       CALL SYSTRAN3(LFT,LLT,Q,K56,ISYM) 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      RETURN
      END
Chd|====================================================================
Chd|  SETPROJKS6                    source/elements/thickshell/solide6c/s6ccumg3.F
Chd|-- called by -----------
Chd|        S6CCUMG3                      source/elements/thickshell/solide6c/s6ccumg3.F
Chd|-- calls ---------------
Chd|        TRANPVQ33                     source/elements/thickshell/solide6c/s6ccumg3.F
Chd|        TRANQIKQJ                     source/elements/shell/coquez/czsumg3.F
Chd|        TRANRIDRRJ33                  source/elements/thickshell/solide6c/s6ccumg3.F
Chd|====================================================================
      SUBROUTINE SETPROJKS6(
     1                 DR    ,R1     ,R2     ,R3     ,R4    ,
     1                 R5    ,R6     ,  
     2                 K11,K12,K13,K14,K15,K16,K22,K23,
     3                 K24,K25,K26,K33,K34,K35,K36,K44,
     4                 K45,K46,K55,K56,K66,
     5                 VQ ,JFT ,JLT  ) 
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
      INTEGER JFT,JLT
C     REAL ou REAL*8
      my_real
     .    DR(3,3,*),VQ(3,3,*),
     .    R1(3,3,*),R2(3,3,*),R3(3,3,*),R4(3,3,*),R5(3,3,*),R6(3,3,*),
     .   K11(3,3,*),K12(3,3,*),K13(3,3,*),K14(3,3,*)  ,K15(3,3,*) ,
     .   K16(3,3,*),K22(3,3,*),K23(3,3,*),K24(3,3,*)  ,K25(3,3,*) ,
     .   K26(3,3,*),K33(3,3,*),K34(3,3,*),K35(3,3,*)  ,K36(3,3,*) ,
     .   K44(3,3,*),K45(3,3,*),K46(3,3,*),K55(3,3,*)  ,K56(3,3,*) ,
     .   K66(3,3,*)  
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,EP,IS,IAS,IT,IAT,SHI,SHJ
      my_real
     .    KL(3,3,MVSIZ),KQ(3,3,MVSIZ)
         DATA IS/1/,IAS/0/,IT/1/,IAT/0/
      my_real,
     .  DIMENSION(:,:,:), ALLOCATABLE:: P,KE
C-------------------------------------------------------------
        ALLOCATE(P(18,18,MVSIZ))
        ALLOCATE(KE(18,18,MVSIZ))
C-----------P11=Pr11Q
       CALL TRANRIDRRJ33(JFT   ,JLT    ,R1  ,DR , R1 ,KL  ,IS   )
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IS ,IT)
       SHI=0
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I,J,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
        DO I=1,3 
        DO J=I,3 
         DO EP=JFT,JLT 
          KE(I,J,EP)= K11(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P22=Pr22Q
       CALL TRANRIDRRJ33(JFT   ,JLT    ,R2  ,DR , R2 ,KL  ,IS   )
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IS ,IT)
       SHI=SHI+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHI,J+SHI,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
        DO I=1,3 
        DO J=I,3 
         DO EP=JFT,JLT 
          KE(I+SHI,J+SHI,EP)= K22(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P33=Pr33Q
       CALL TRANRIDRRJ33(JFT   ,JLT    ,R3  ,DR , R3 ,KL  ,IS   )
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IS ,IT)
       SHI=SHI+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHI,J+SHI,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
        DO I=1,3 
        DO J=I,3 
         DO EP=JFT,JLT 
          KE(I+SHI,J+SHI,EP)= K33(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P44=Pr44Q
       CALL TRANRIDRRJ33(JFT   ,JLT    ,R4  ,DR , R4 ,KL  ,IS   )
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IS ,IT)
       SHI=SHI+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHI,J+SHI,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
        DO I=1,3 
        DO J=I,3 
         DO EP=JFT,JLT 
          KE(I+SHI,J+SHI,EP)= K44(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P55=Pr55Q
       CALL TRANRIDRRJ33(JFT   ,JLT    ,R5  ,DR , R5 ,KL  ,IS   )
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IS ,IT)
       SHI=SHI+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHI,J+SHI,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
        DO I=1,3 
        DO J=I,3 
         DO EP=JFT,JLT 
          KE(I+SHI,J+SHI,EP)= K55(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P66=Pr66Q
       CALL TRANRIDRRJ33(JFT   ,JLT    ,R6  ,DR , R6 ,KL  ,IS   )
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IS ,IT)
       SHI=SHI+3
       if (SHI/=15) print *,'error,SHI=',SHI
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHI,J+SHI,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
        DO I=1,3 
        DO J=I,3 
         DO EP=JFT,JLT 
          KE(I+SHI,J+SHI,EP)= K66(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P12=Pr12Q
       CALL TRANRIDRRJ33(JFT   ,JLT    ,R1  ,DR , R2 ,KL  ,IAS   )
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IT)
       SHJ=3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I,J+SHJ,EP)= KQ(I,J,EP)
          KE(I,J+SHJ,EP)= K12(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P21=Pr21Q
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IAT)
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHJ,J,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P13=Q1Pr13Q
       CALL TRANRIDRRJ33(JFT   ,JLT    ,R1  ,DR , R3 ,KL  ,IAS   )
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IT)
       SHJ=SHJ+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I,J+SHJ,EP)= KQ(I,J,EP)
          KE(I,J+SHJ,EP)= K13(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P31=Pr31Q
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IAT)
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHJ,J,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P14=Pr14Q
       CALL TRANRIDRRJ33(JFT   ,JLT    ,R1  ,DR , R4 ,KL  ,IAS   )
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IT)
       SHJ=SHJ+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I,J+SHJ,EP)= KQ(I,J,EP)
          KE(I,J+SHJ,EP)= K14(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P41=P41Q
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IAT)
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHJ,J,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P15=Pr15Q
       CALL TRANRIDRRJ33(JFT   ,JLT    ,R1  ,DR , R5 ,KL  ,IAS   )
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IT)
       SHJ=SHJ+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I,J+SHJ,EP)= KQ(I,J,EP)
          KE(I,J+SHJ,EP)= K15(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P51=P51Q
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IAT)
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHJ,J,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P16=Pr16Q
       CALL TRANRIDRRJ33(JFT   ,JLT    ,R1  ,DR , R6 ,KL  ,IAS   )
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IT)
       SHJ=SHJ+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I,J+SHJ,EP)= KQ(I,J,EP)
          KE(I,J+SHJ,EP)= K16(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P61=P61Q
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IAT)
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHJ,J,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P23=Pr23Q
       CALL TRANRIDRRJ33(JFT   ,JLT    ,R2  ,DR , R3 ,KL  ,IAS   )
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IT)
       SHI=3
       SHJ=3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHI,J+SHJ,EP)= KQ(I,J,EP)
          KE(I+SHI,J+SHJ,EP)= K23(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P32=Pr32Q
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IAT)
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHJ,J+SHI,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P24=Pr24Q
       CALL TRANRIDRRJ33(JFT   ,JLT    ,R2  ,DR , R4 ,KL  ,IAS   )
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IT)
       SHI=3
       SHJ=3+3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHI,J+SHJ,EP)= KQ(I,J,EP)
          KE(I+SHI,J+SHJ,EP)= K24(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P42=Pr42Q
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IAT)
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHJ,J+SHI,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P25=Pr25Q
       CALL TRANRIDRRJ33(JFT   ,JLT    ,R2  ,DR , R5 ,KL  ,IAS   )
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IT)
       SHI=3
       SHJ=3+3+3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHI,J+SHJ,EP)= KQ(I,J,EP)
          KE(I+SHI,J+SHJ,EP)= K25(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P52=Pr52Q
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IAT)
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHJ,J+SHI,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P26=Pr26Q
       CALL TRANRIDRRJ33(JFT   ,JLT    ,R2  ,DR , R6 ,KL  ,IAS   )
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IT)
       SHI=3
       SHJ=3+3+3+3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHI,J+SHJ,EP)= KQ(I,J,EP)
          KE(I+SHI,J+SHJ,EP)= K26(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P62=Pr62Q
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IAT)
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHJ,J+SHI,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P34=Pr34Q
       CALL TRANRIDRRJ33(JFT   ,JLT    ,R3  ,DR , R4 ,KL  ,IAS   )
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IT)
       SHI=3+3
       SHJ=3+3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHI,J+SHJ,EP)= KQ(I,J,EP)
          KE(I+SHI,J+SHJ,EP)= K34(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P43=Pr43Q
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IAT)
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHJ,J+SHI,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P35=Pr35Q
       CALL TRANRIDRRJ33(JFT   ,JLT    ,R3  ,DR , R5 ,KL  ,IAS   )
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IT)
       SHI=3+3
       SHJ=3+3+3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHI,J+SHJ,EP)= KQ(I,J,EP)
          KE(I+SHI,J+SHJ,EP)= K35(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P53=Pr53Q
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IAT)
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHJ,J+SHI,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P36=Pr36Q
       CALL TRANRIDRRJ33(JFT   ,JLT    ,R3  ,DR , R6 ,KL  ,IAS   )
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IT)
       SHI=3+3
       SHJ=3+3+3+3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHI,J+SHJ,EP)= KQ(I,J,EP)
          KE(I+SHI,J+SHJ,EP)= K36(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P63=Pr63Q
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IAT)
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHJ,J+SHI,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P45=Pr45Q
       CALL TRANRIDRRJ33(JFT   ,JLT    ,R4  ,DR , R5 ,KL  ,IAS   )
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IT)
       SHI=3+3+3
       SHJ=3+3+3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHI,J+SHJ,EP)= KQ(I,J,EP)
          KE(I+SHI,J+SHJ,EP)= K45(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P54=Pr54Q
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IAT)
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHJ,J+SHI,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P46=Pr46Q
       CALL TRANRIDRRJ33(JFT   ,JLT    ,R4  ,DR , R6 ,KL  ,IAS   )
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IT)
       SHI=3+3+3
       SHJ=3+3+3+3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHI,J+SHJ,EP)= KQ(I,J,EP)
          KE(I+SHI,J+SHJ,EP)= K46(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P64=Pr64Q
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IAT)
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHJ,J+SHI,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P56=Pr56Q
       CALL TRANRIDRRJ33(JFT   ,JLT    ,R5  ,DR , R6 ,KL  ,IAS   )
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IT)
       SHI=3+3+3+3
       SHJ=3+3+3+3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHI,J+SHJ,EP)= KQ(I,J,EP)
          KE(I+SHI,J+SHJ,EP)= K56(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P65=Pr65Q
       CALL TRANPVQ33(JFT   ,JLT    ,KL ,KQ ,VQ ,IAS ,IAT)
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          P(I+SHJ,J+SHI,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------
        DO I=1,18 
        DO J=I+1,18 
         DO EP=JFT,JLT 
          KE(J,I,EP)= KE(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------
        CALL TRANQIKQJ(JFT   ,JLT    ,P  ,KE,P ,18 ,IS ) 
C-----------after projection----
       SHI=0
       SHJ=0
C-----------K11
        DO I=1,3 
        DO J=I,3 
         DO EP=JFT,JLT 
          K11(I,J,EP) =KE(I,J,EP)
          K11(J,I,EP) =KE(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K22
       SHI=SHI+3
       SHJ=SHJ+3
        DO I=1,3 
        DO J=I,3 
         DO EP=JFT,JLT 
          K22(I,J,EP) = KE(I+SHI,J+SHJ,EP)
          K22(J,I,EP) = KE(I+SHI,J+SHJ,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K33
       SHI=SHI+3
       SHJ=SHJ+3
        DO I=1,3 
        DO J=I,3 
         DO EP=JFT,JLT 
          K33(I,J,EP) = KE(I+SHI,J+SHJ,EP)
          K33(J,I,EP) = KE(I+SHI,J+SHJ,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K44
       SHI=SHI+3
       SHJ=SHJ+3
        DO I=1,3 
        DO J=I,3 
         DO EP=JFT,JLT 
          K44(I,J,EP) = KE(I+SHI,J+SHJ,EP)
          K44(J,I,EP) = KE(I+SHI,J+SHJ,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K55
       SHI=SHI+3
       SHJ=SHJ+3
        DO I=1,3 
        DO J=I,3 
         DO EP=JFT,JLT 
          K55(I,J,EP) = KE(I+SHI,J+SHJ,EP)
          K55(J,I,EP) = KE(I+SHI,J+SHJ,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K66
       SHI=SHI+3
       SHJ=SHJ+3
        DO I=1,3 
        DO J=I,3 
         DO EP=JFT,JLT 
          K66(I,J,EP) = KE(I+SHI,J+SHJ,EP)
          K66(J,I,EP) = KE(I+SHI,J+SHJ,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K12
       SHI=0
       SHJ=3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K12(I,J,EP) = KE(I+SHI,J+SHJ,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K13
       SHI=0
       SHJ=3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K13(I,J,EP) = KE(I+SHI,J+SHJ,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K14
       SHI=0
       SHJ=3+3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K14(I,J,EP) = KE(I+SHI,J+SHJ,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K15
       SHI=0
       SHJ=3+3+3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K15(I,J,EP) = KE(I+SHI,J+SHJ,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K16
       SHI=0
       SHJ=3+3+3+3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K16(I,J,EP) = KE(I+SHI,J+SHJ,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K23
       SHI=3
       SHJ=3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K23(I,J,EP) = KE(I+SHI,J+SHJ,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K24
       SHI=3
       SHJ=3+3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K24(I,J,EP) = KE(I+SHI,J+SHJ,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K25
       SHI=3
       SHJ=3+3+3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K25(I,J,EP) = KE(I+SHI,J+SHJ,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K26
       SHI=3
       SHJ=3+3+3+3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K26(I,J,EP) = KE(I+SHI,J+SHJ,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K34
       SHI=3+3
       SHJ=3+3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K34(I,J,EP) = KE(I+SHI,J+SHJ,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K35
       SHI=3+3
       SHJ=3+3+3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K35(I,J,EP) = KE(I+SHI,J+SHJ,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K36
       SHI=3+3
       SHJ=3+3+3+3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K36(I,J,EP) = KE(I+SHI,J+SHJ,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K45
       SHI=3+3+3
       SHJ=3+3+3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K45(I,J,EP) = KE(I+SHI,J+SHJ,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K46
       SHI=3+3+3
       SHJ=3+3+3+3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K46(I,J,EP) = KE(I+SHI,J+SHJ,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K56
       SHI=3+3+3+3
       SHJ=3+3+3+3+3
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K56(I,J,EP) = KE(I+SHI,J+SHJ,EP)
         ENDDO
        ENDDO
        ENDDO
C        
        DEALLOCATE(P)
        DEALLOCATE(KE)
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      RETURN
      END
Chd|====================================================================
Chd|  TRANRIDRRJ33                  source/elements/thickshell/solide6c/s6ccumg3.F
Chd|-- called by -----------
Chd|        SETPROJKS6                    source/elements/thickshell/solide6c/s6ccumg3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TRANRIDRRJ33(JFT   ,JLT    ,RI  ,RD , RJ,KD  ,IS   )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,IS
      my_real
     .   RI(3,3,*), RJ(3,3,*),RD(3,3,*),KD(3,3,*)
C-----------------------------------------------
C   LOCAL  A R G U M E N T S
C-----------------------------------------------
      INTEGER I,J,EP,L,I1,J1
      my_real
     .    K(3,3)
C--------------[KD]=|RI| |RD| |RJ|^t---------------------------------
       IF (IS==1) THEN
        DO I=1,3
        DO J=I,3
         DO EP=JFT,JLT
          K(I,J)=ZERO
          DO L=1,3
           K(I,J)=K(I,J)+RI(I,1,EP)*RD(1,L,EP)*RJ(J,L,EP)+
     1                   RI(I,2,EP)*RD(2,L,EP)*RJ(J,L,EP)+
     1                   RI(I,3,EP)*RD(3,L,EP)*RJ(J,L,EP)
          ENDDO
          KD(I,J,EP)= K(I,J)
          KD(J,I,EP)= K(I,J)
         ENDDO
        ENDDO
        ENDDO
C
       ELSE
        DO I=1,3
        DO J=1,3
         DO EP=JFT,JLT
          K(I,J)=ZERO
          DO L=1,3
           K(I,J)=K(I,J)+RI(I,1,EP)*RD(1,L,EP)*RJ(J,L,EP)+
     1                   RI(I,2,EP)*RD(2,L,EP)*RJ(J,L,EP)+
     1                   RI(I,3,EP)*RD(3,L,EP)*RJ(J,L,EP)
          ENDDO
          KD(I,J,EP)= K(I,J)
         ENDDO
        ENDDO
        ENDDO
       END IF
C
      RETURN
      END
Chd|====================================================================
Chd|  TRANPVQ33                     source/elements/thickshell/solide6c/s6ccumg3.F
Chd|-- called by -----------
Chd|        SETPROJKS6                    source/elements/thickshell/solide6c/s6ccumg3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TRANPVQ33(JFT   ,JLT    ,KD  ,PVQ ,VQ ,IS ,IT   )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,IS,IT
      my_real
     .   KD(3,3,*),PVQ(3,3,*),VQ(3,3,*)
C-----------------------------------------------
C   LOCAL  A R G U M E N T S
C-----------------------------------------------
      INTEGER I,J,EP,L,I1,J1
      my_real
     .    K(3,3)
C--------------PVQIJ=[VQ]gij-[KD][VQ]--;PVQJI=[VQ]gij-[KD]^t[VQ]-----------------------------
       IF (IT==1) THEN
        DO I=1,3
        DO J=1,3
         DO EP=JFT,JLT
          K(I,J)=ZERO
          DO L=1,3
           K(I,J)=K(I,J)+KD(I,L,EP)*VQ(L,J,EP)
          ENDDO
          PVQ(I,J,EP)=-K(I,J)
         ENDDO
        ENDDO
        ENDDO
       ELSE
        DO I=1,3
        DO J=1,3
         DO EP=JFT,JLT
          K(I,J)=ZERO
          DO L=1,3
           K(I,J)=K(I,J)+KD(L,I,EP)*VQ(L,J,EP)
          ENDDO
          PVQ(I,J,EP)=-K(I,J)
         ENDDO
        ENDDO
        ENDDO
       END IF !(IT==1) THEN
C       
       IF (IS==1) THEN
        DO I=1,3
        DO J=1,3
         DO EP=JFT,JLT
          PVQ(I,J,EP)=PVQ(I,J,EP)+VQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
       END IF
C       
      RETURN
      END
Chd|====================================================================
Chd|  MATVPV                        source/elements/thickshell/solide6c/s6ccumg3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MATVPV(JFT   ,JLT    ,P  ,VL ,ND )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,ND
      my_real
     .   P(ND,ND,*),VL(ND,*)
C-----------------------------------------------
C   LOCAL  A R G U M E N T S
C-----------------------------------------------
      INTEGER I,J,EP,L,I1,J1
      my_real
     .   V(ND)
C-----------------------------------------
        DO EP=JFT,JLT
         DO I=1,ND
          V(I)=ZERO
          DO L=1,ND
           V(I)=V(I)+P(I,L,EP)*VL(L,EP)
          ENDDO
         ENDDO
         DO I=1,ND
          VL(I,EP)=V(I)
         ENDDO
        ENDDO
      RETURN
      END
