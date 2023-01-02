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
Chd|  I6FRI3                        source/interfaces/inter3d/i6fri3.F
Chd|-- called by -----------
Chd|        I6MAIN                        source/interfaces/inter3d/i6main.F
Chd|-- calls ---------------
Chd|        NINTERP                       source/interfaces/int14/ninterp.F
Chd|====================================================================
      SUBROUTINE I6FRI3(X       ,IRECT  ,MSR   ,NSV   ,IRTL  ,
     1                  CST     ,IRTLO  ,FRIC0 ,FRIC  ,ES    ,
     2                  EM      ,SFRIC  ,IFRICF,NPC   ,TF    ,
     3                  LOLD    ,IFRICV ,VNT   ,ASCALF,ASCALV,
     4                  STIFF   ,
     5                  N1     ,N2    ,N3    ,SSC   ,TTC ,
     6                  XFACE  ,XP    ,YP    ,ZP    ,H1  ,
     7                  H2     ,H3    ,H4    ,FNI   ,FXI ,
     8                  FYI    ,FZI   ,FX1   ,FX2   ,FX3 ,
     9                  FX4    ,FY1   ,FY2   ,FY3   ,FY4 ,
     1                  FZ1    ,FZ2   ,FZ3   ,FZ4)
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
      INTEGER IRECT(4,*),MSR(*),NSV(*),IRTL(*),IRTLO(*),NPC(*),LOLD(*)
      INTEGER IFRICF,IFRICV
c
      my_real
     .   X(3,*),CST(2,*),FRIC0(3,*),ES(*),EM(*),TF(*),VNT(*)
      my_real
     .   FRIC,SFRIC,ASCALF,ASCALV,STIFF
      my_real, DIMENSION(MVSIZ), INTENT(IN) ::  N1,N2,N3,SSC,TTC,XFACE
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: XP,YP,ZP
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: H1,H2,H3,H4,FNI
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: FXI,FYI,FZI
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: FX1,FX2,FX3,FX4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: FY1,FY2,FY3,FY4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: FZ1,FZ2,FZ3,FZ4
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "vect07_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IL, JJ, NN, L, J3, J2, J1, I3, I2, I1
      my_real
     .   H(4), XX1(4), XX2(4), XX3(4)
      my_real
     .   SS0,TT0,XC0,YC0,ZC0,SP,SM,TP,TM,FT,FTX,FTY,FTZ,
     .   ANSX,ANSY,ANSZ,FMAX,FTI,FN,TN1,TN2,TN3,TN
      my_real
     .   KFRIC(LLT),KFRICV(LLT),XX(LLT)
C-----------------------------------------------
!      EQUIVALENCE (XP,BV(1,16)),(YP,BV(1,17)),(ZP,BV(1,18))
!      EQUIVALENCE (N1,BV(1,19)) ,(N2,BV(1,20)) ,(N3,BV(1,21))
!      EQUIVALENCE (SSC,BV(1,23)),(TTC,BV(1,24))
!      EQUIVALENCE (H1,BV(1,25)) ,(H2,BV(1,26)) ,(H3,BV(1,27))
!      EQUIVALENCE (H4,BV(1,28)) ,(XFACE,BV(1,29))
!      EQUIVALENCE (FNI,BV(1,40))
!      EQUIVALENCE (FXI,BV(1,41)),(FYI,BV(1,42)),(FZI,BV(1,43))
!      EQUIVALENCE (FX1,BV(1,44)),(FY1,BV(1,45)),(FZ1,BV(1,46))
!      EQUIVALENCE (FX2,BV(1,47)),(FY2,BV(1,48)),(FZ2,BV(1,49))
!      EQUIVALENCE (FX3,BV(1,50)),(FY3,BV(1,51)),(FZ3,BV(1,52))
!      EQUIVALENCE (FX4,BV(1,53)),(FY4,BV(1,54)),(FZ4,BV(1,55))
C=======================================================================
C     Coefficient de Frottement : MU = FROT*F1(FN)*F2(VT)
C----------------------------------------------------------
      IF (IFRICF == 0) THEN
        DO I=LFT,LLT
          KFRIC(I) = FRIC
        ENDDO
      ELSE
        DO I=LFT,LLT
          XX(I) = FNI(I)*ASCALF
        ENDDO 
        CALL NINTERP(IFRICF,NPC,TF,LLT,XX,KFRIC)
        DO I=LFT,LLT
          KFRIC(I) = FRIC*KFRIC(I)
        ENDDO
      ENDIF
      IF (IFRICV /= 0) THEN
        DO I=LFT,LLT
          XX(I) = VNT(I)*ASCALV
        ENDDO 
        CALL NINTERP(IFRICV,NPC,TF,LLT,XX,KFRICV)
          KFRIC(I) = KFRIC(I)*KFRICV(I)
      ENDIF      
c----------------------------------------------------------
      DO I=LFT,LLT
        IL = I+NFT
          IF (LOLD(I) == 0) THEN
            FXI(I) = ZERO
            FYI(I) = ZERO
            FZI(I) = ZERO
            FRIC0(1,IL) = ZERO
            FRIC0(2,IL) = ZERO
            FRIC0(3,IL) = ZERO
          ELSE
C-------------------------------
C           POINT IMPACTE PRECEDEMENT
C-------------------------------
            SS0 = CST(1,IL)
            TT0 = CST(2,IL)
c
            DO JJ=1,4
              NN=MSR(IRECT(JJ,IABS(IRTLO(IL))))
              XX1(JJ) = X(1,NN)
              XX2(JJ) = X(2,NN)
              XX3(JJ) = X(3,NN)
            ENDDO
c
            SP = ONE + SS0
            SM = ONE - SS0
            TP = FOURTH*(ONE + TT0)
            TM = FOURTH*(ONE - TT0)
            H(1)=TM*SM
            H(2)=TM*SP
            H(3)=TP*SP
            H(4)=TP*SM
c
            XC0 = ZERO
            YC0 = ZERO
            ZC0 = ZERO
            DO JJ=1,4
              XC0 = XC0 + H(JJ)*XX1(JJ)
              YC0 = YC0 + H(JJ)*XX2(JJ)
              ZC0 = ZC0 + H(JJ)*XX3(JJ)
            ENDDO
c
            ANSX = XP(I) - XC0
            ANSY = YP(I) - YC0
            ANSZ = ZP(I) - ZC0
C
            FMAX = SFRIC - MIN(KFRIC(I)*FNI(I),ZERO)
C
            FXI(I) = FRIC0(1,IL) + ANSX*STIFF
            FYI(I) = FRIC0(2,IL) + ANSY*STIFF
            FZI(I) = FRIC0(3,IL) + ANSZ*STIFF
C
            FN  = FXI(I)*N1(I)+FYI(I)*N2(I)+FZI(I)*N3(I)
            FTX = FXI(I) - N1(I)*FN
            FTY = FYI(I) - N2(I)*FN
            FTZ = FZI(I) - N3(I)*FN
            FT  = SQRT(FTX*FTX + FTY*FTY + FTZ*FTZ)
            IF (FT /= ZERO) THEN
              TN1 = FTX/FT
              TN2 = FTY/FT
              TN3 = FTZ/FT
            ELSE
              TN3 = ZERO
              TN  = SQRT(N1(I)*N1(I)+N2(I)*N2(I))
              IF(TN/=ZERO)THEN
                TN2 =-N1(I)/TN
                TN1 = N2(I)/TN
              ELSE
                TN2 = ZERO
                TN1 = ONE
              ENDIF
            ENDIF
c
            IF (FT > FMAX) THEN
C-------------------------------
C             POINT GLISSANT
C-------------------------------
              FXI(I) = TN1*FMAX
              FYI(I) = TN2*FMAX
              FZI(I) = TN3*FMAX
              IRTLO(IL) = IRTL(IL)*XFACE(I)
              CST(1,IL) = SSC(I)
              CST(2,IL) = TTC(I)
            ELSE
C-------------------------------
C             POINT NON GLISSANT
C-------------------------------
              FXI(I) = FTX
              FYI(I) = FTY
              FZI(I) = FTZ
            ENDIF
            FRIC0(1,IL) = FXI(I) 
            FRIC0(2,IL) = FYI(I) 
            FRIC0(3,IL) = FZI(I) 
C
          ENDIF  ! LOLD
      ENDDO      ! I=LFT,LLT
c
c----------------------------------------------------------
c
      DO I=LFT,LLT
        IL=I+NFT
        L =IRTL(IL)
        FX1(I)=FXI(I)*H1(I)
        FY1(I)=FYI(I)*H1(I)
        FZ1(I)=FZI(I)*H1(I)
C
        FX2(I)=FXI(I)*H2(I)
        FY2(I)=FYI(I)*H2(I)
        FZ2(I)=FZI(I)*H2(I)
C
        FX3(I)=FXI(I)*H3(I)
        FY3(I)=FYI(I)*H3(I)
        FZ3(I)=FZI(I)*H3(I)
C
        FX4(I)=FXI(I)*H4(I)
        FY4(I)=FYI(I)*H4(I)
        FZ4(I)=FZI(I)*H4(I)
C---------------------------------
C       FRICTION MAIN
C---------------------------------
        J3=3*IRECT(1,L)
        J2=J3-1
        J1=J2-1
        EM(J1)=EM(J1)+FX1(I)
        EM(J2)=EM(J2)+FY1(I)
        EM(J3)=EM(J3)+FZ1(I)
C
        J3=3*IRECT(2,L)
        J2=J3-1
        J1=J2-1
        EM(J1)=EM(J1)+FX2(I)
        EM(J2)=EM(J2)+FY2(I)
        EM(J3)=EM(J3)+FZ2(I)
C
        J3=3*IRECT(3,L)
        J2=J3-1
        J1=J2-1
        EM(J1)=EM(J1)+FX3(I)
        EM(J2)=EM(J2)+FY3(I)
        EM(J3)=EM(J3)+FZ3(I)
C
        J3=3*IRECT(4,L)
        J2=J3-1
        J1=J2-1
        EM(J1)=EM(J1)+FX4(I)
        EM(J2)=EM(J2)+FY4(I)
        EM(J3)=EM(J3)+FZ4(I)
C---------------------------------
C       FRICTION SECND
C---------------------------------
        I3=3*IL
        I2=I3-1
        I1=I2-1
        ES(I1)=ES(I1)-FXI(I)
        ES(I2)=ES(I2)-FYI(I)
        ES(I3)=ES(I3)-FZI(I)
      ENDDO      ! I=LFT,LLT
C
C-----------
      RETURN
      END
