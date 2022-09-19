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
Chd|  CM58_REFSTA                   source/materials/mat/mat058/cm58_refsta.F
Chd|-- called by -----------
Chd|        CMLAWI                        source/elements/shell/coque/cepsini.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE CM58_REFSTA(
     1                   JFT    ,JLT    ,FOR    ,EINT   ,GSTR   ,
     2                   THK    ,DIR1   ,DIR2   ,NUPARAM,NUVAR  ,
     3                   UVAR   ,UPARAM ,NEL    ,ISMSTR ,
     4                   NBFUNC ,KFUNC  ,NPF    ,TF     ,AREA   ,
     5                   EXX    ,EYY    ,EXY    )
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT, NUPARAM,NUVAR,NEL,ISMSTR,NBFUNC,KFUNC(NBFUNC)
      my_real
     .   FOR(NEL,5),UPARAM(*),THK(*),UVAR(NEL,NUVAR),AREA(NEL),
     .   EINT(NEL,2),GSTR(NEL,8),DIR1(NEL,*),DIR2(NEL,*),
     .   EXX(NEL),EYY(NEL),EXY(NEL)
C-----------------------------------------------
      INTEGER  NPF(*)
      my_real  FINTER ,TF(*)
      EXTERNAL FINTER
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,MX,NC,NT,ITER,NITER,NBFCT,FLAGUL
      my_real
     .        KC,KT,KFC,KFT,G0,GT,GB,TAN_LOCK,VISCE,VISCG,LC0,LT0,
     .        KBC,KBT,DC0,DT0,HC0,HT0,ETC,ETT,DC,DT,TRACE,R3R3,S3S3,
     .        AC,AT,BC,BT,BC2,BT2,CC,TT,FUNC,FUNT,DERIC,DERIT,
     .        Y,EC2,ET2,SIGC,SIGT,UDC,UDT,FC,FT,FACT,SIG0,
     .        FPC,FPT,CCL,TTL,DEC,DET,HC,HT,KF,KMAX,HDC,HDT,
     .        TAN_PHI,RFAC,RFAT,R1,R2,R3,S1,S2,S3,T1,T2,T3,RS1,RS2,RS3,
     .        R12,S12,R22,S22,ZEROSTRESS,FLEX1,FLEX2,YFAC(6),PHI,GXY,DDEC,DDET,
     .        DCC1,dtt1,DCC,DTT,KKC,KKT
      my_real
     .        EC(MVSIZ),ET(MVSIZ),LC(MVSIZ),LT(MVSIZ),FN(MVSIZ),
     .        YC(MVSIZ),YT(MVSIZ),DEGMB(MVSIZ),
     .        DEPSXX(MVSIZ),DEPSYY(MVSIZ),DEPSXY(MVSIZ),
     .        SIGNXX(MVSIZ),SIGNYY(MVSIZ),SIGNXY(MVSIZ),
     .        TENS(3,MVSIZ),EMINCRL(MVSIZ),EMINTRL(MVSIZ),EMINSRL(MVSIZ),
     .        EPSNC(MVSIZ),EPSNT(MVSIZ),EPSNS(MVSIZ),EMAXSL(MVSIZ),EMAXSRL(MVSIZ),
     .        EMAXCL(MVSIZ),EMAXCRL(MVSIZ),EMAXTL(MVSIZ),EMAXTRL(MVSIZ),
     .        EPSMAXC(MVSIZ),EPSMAXRLC(MVSIZ),EPSNRLC (MVSIZ),
     .        SLMAXRLC(MVSIZ),SLMAXRLT(MVSIZ),SLMAXS(MVSIZ),SLMINS(MVSIZ),
     .        EPSMAXS(MVSIZ),EPSMAXRLS(MVSIZ),EPSNRLS (MVSIZ),SLMAXRLS(MVSIZ) ,
     .        SLMAXC(MVSIZ),EPSMAXT(MVSIZ),EPSMAXRLT(MVSIZ),EPSNRLT(MVSIZ),
     .        SLMAXT(MVSIZ),PHIO(MVSIZ),SLMINC(MVSIZ),SLMINT(MVSIZ)
C=======================================================================
C---  Initialisations
C----------------------------------------------------------------------
      NITER      = 5
      LC0        = UPARAM( 1)
      LT0        = UPARAM( 2)
      DC0        = UPARAM( 3)
      DT0        = UPARAM( 4)
      HC0        = UPARAM( 5)
      HT0        = UPARAM( 6)
      NC         = UPARAM( 7)
      NT         = UPARAM( 8)
      KC         = UPARAM( 9)
      KT         = UPARAM(10)
      KFC        = UPARAM(11)
      KFT        = UPARAM(12)
      G0         = UPARAM(13)
      GT         = UPARAM(14)
      GB         = UPARAM(15)
      TAN_LOCK   = UPARAM(16)
      VISCE      = UPARAM(17)
      VISCG      = UPARAM(18)
      KBC        = UPARAM(19)
      KBT        = UPARAM(20)
      ZEROSTRESS = UPARAM(24)
      FLAGUL     = NINT(UPARAM(35))
      KKC        = UPARAM(40)
      KKT        = UPARAM(41)
c
      FLEX1 = UPARAM(26)
      FLEX2 = UPARAM(27)
      DO I=1,3
        YFAC(I)  = UPARAM(27 + I )
      ENDDO
      NBFCT    = NINT(UPARAM(31))
C
      IF (KBC == ZERO) THEN
       CCL = EP30
      ELSE
       CCL = KC / KBC
      ENDIF
      IF (KBT == ZERO) THEN
       TTL = EP30
      ELSE
       TTL = KT / KBT
      ENDIF
C      
      DO I=JFT,JLT
        DEGMB(I) = FOR(I,1)*EXX(I)+FOR(I,2)*EYY(I)+FOR(I,3)*EXY(I)
        FOR(I,1)=ZERO
        FOR(I,2)=ZERO
        FOR(I,3)=ZERO
      ENDDO
C----------------------------------------------------------------------
      IF (ISMSTR == 11) THEN
c       total strain in fiber coord sys
        DO I=JFT,JLT
          R1 = DIR1(I,1)
          S1 = DIR1(I,2)
          R2 = DIR2(I,1)
          S2 = DIR2(I,2)
c
          T1 = GSTR(I,1)
          T2 = GSTR(I,2)
          T3 = GSTR(I,3)*HALF
          EC(I)     = R1*R1*T1 + S1*S1*T2 + TWO*R1*S1*T3  ! eps_x dir1
          ET(I)     = R2*R2*T1 + S2*S2*T2 + TWO*R2*S2*T3 ! eps_y dir2
          DEPSXY(I) = (R1*R2 + S1*S2) / (R1*S2 - R2*S1)    ! Tan(alpha_totale)
        ENDDO
      ELSE
c       strain rate in fiber coord sys
        DO I=JFT,JLT
          R1 = DIR1(I,1)
          S1 = DIR1(I,2)
          R2 = DIR2(I,1)
          S2 = DIR2(I,2)
c
          T1 = EXX(I)
          T2 = EYY(I)
          T3 = HALF*EXY(I)
          DEPSXX(I) = R1*R1*T1 + S1*S1*T2 + TWO*R1*S1*T3 ! delta_eps_x
          DEPSYY(I) = R2*R2*T1 + S2*S2*T2 + TWO*R2*S2*T3 ! delta_eps_y
          DEPSXY(I) = (R1*R2 + S1*S2) / (R1*S2 - R2*S1)   ! Tan(alpha_totale)
C---      integration des deformations
          ETC = UVAR(I,4) + DEPSXX(I)
          ETT = UVAR(I,5) + DEPSYY(I)
          UVAR(I,4) = ETC
          UVAR(I,5) = ETT
          EC(I) = EXP(ETC) - ONE ! eng strain
          ET(I) = EXP(ETT) - ONE
        ENDDO
      ENDIF
c---
      IF(FLAGUL /= 1)THEN!no hysteresis
c---
      DO I=JFT,JLT
        LC(I) = LC0 * (ONE + EC(I))
        LT(I) = LT0 * (ONE + ET(I))
      ENDDO
C---  Calcul YC, YT
      AC = KC*DC0
      AC = AC*AC
      AT = KT*DT0
      AT = AT*AT
      DO I=JFT,JLT
        YC(I) = UVAR(I,7)
        YT(I) = UVAR(I,8)
        FN(I) = ZERO
C---    non coupled model  
        DO ITER = 1, NITER
          HC  = HC0 + YC(I)
          HT  = HT0 + YT(I)
          DC = SQRT(LC(I)*LC(I) + HC*HC)
          DT = SQRT(LT(I)*LT(I) + HT*HT)
          UDC= ONE / DC
          UDT= ONE / DT
          HDC= HC * UDC
          HDT= HT * UDT
          CC = DC - DC0
          TT = DT - DT0
          IF(KFUNC(1) /= 0)THEN
            FC = YFAC(1)*FINTER(KFUNC(1),CC,NPF,TF,FPC)
            FPC = FPC *YFAC(1)
            KC = FPC
            KFC = FLEX1*FPC*HC0/DC0
            FPC = FPC*HDC
          ELSEIF (CC >= CCL ) THEN
            FC  = HALF * KC*CCL
            FPC = ZERO
          ELSE
            FC  = (KC - HALF*KBC * CC) * CC
            FPC = (KC - KBC*CC) * HDC
          ENDIF
          IF(KFUNC(2) /= 0)THEN
            FT = YFAC(2)*FINTER(KFUNC(2),TT,NPF,TF,FPT)
            FPT = FPT *YFAC(2)
            KT = FPT
            KFT = FLEX2*FPT*HT0/DT0
            FPT = FPT*HDT
          ELSEIF (TT >= TTL ) THEN
            FT  = HALF * KT*TTL
            FPT = ZERO
          ELSE
            FT  = (KT - HALF*KBT * TT) * TT
            FPT = (KT - KBT*TT) * HDT
          ENDIF
          FUNC = KFC*YC(I) + FC*HC/DC
          FUNT = KFT*YT(I) + FT*HT/DT
          DERIC= KFC + FPC*HDC + FC*UDC*(ONE - HDC*HDC)
          DERIT= KFT + FPT*HDT + FT*UDT*(ONE - HDT*HDT)
C-------------------------------------
          YC(I) = YC(I) - FUNC / DERIC
          YT(I) = YT(I) - FUNT / DERIT
          FN(I) = ZERO
        ENDDO
C---    Coupled model    (traction fibre)
        IF ((YC(I) + YT(I)) < ZERO) THEN
          Y = HALF * (UVAR(I,7) - UVAR(I,8))
          DO ITER = 1, NITER
            HC = HC0 + Y
            HT = HT0 - Y
            DC = SQRT(LC(I)*LC(I) + HC*HC)
            DT = SQRT(LT(I)*LT(I) + HT*HT)
            CC = DC - DC0
            TT = DT - DT0
            UDC= ONE / DC
            UDT= ONE / DT
            HDC= HC * UDC
            HDT= HT * UDT
            IF(KFUNC(1) /= 0)THEN
             FC =YFAC(1)* FINTER(KFUNC(1),CC,NPF,TF,FPC)
             FPC = FPC *YFAC(1)
             KC = FPC
             KFC = FLEX1*FPC*HC0/DC0
             FPC = FPC*HDC
            ELSEIF (CC >= CCL ) THEN
              FC  = HALF * KC*CCL
              FPC = ZERO
            ELSE
              FC  = (KC - HALF*KBC * CC) * CC
              FPC = (KC - KBC*CC) * HDC
            ENDIF
            IF(KFUNC(2) /= 0)THEN
              FT = YFAC(2)*FINTER(KFUNC(2),TT,NPF,TF,FPT)
              FPT = FPT *YFAC(2)
              KT = FPT
              KFT = FLEX2*FPT*HT0/DT0
              FPT = FPT*HDT
            ELSEIF (TT >= TTL ) THEN
              FT  = HALF * KT*TTL
              FPT = ZERO
            ELSE
              FT  = (KT - HALF*KBT * TT) * TT
              FPT = (KT - KBT*TT) * HDT
            ENDIF
C
            KF = KFC + KFT
            FUNC  = KF*Y + FC * HDC - FT * HDT
            DERIC = KF + FPC*HDC + FC*UDC*(ONE - HDC*HDC)
     .                 + FPT*HDT + FT*UDT*(ONE - HDT*HDT)
            Y = Y - FUNC / DERIC
C
            IF (Y > 0) THEN
              Y = MIN(Y, HT0)
            ELSE
              Y = MAX(Y,-HC0)
            ENDIF
          ENDDO
          YC(I) = Y
          YT(I) =-Y
          FN(I) = FC*HC/DC + FT*HT/DT
        ENDIF
        HC = HC0 + YC(I)
        HT = HT0 + YT(I)
        DC = SQRT(LC(I)*LC(I) + HC*HC)
        DT = SQRT(LT(I)*LT(I) + HT*HT)
      ENDDO
C---      
      DO I=JFT,JLT
        HC = HC0 + YC(I)
        HT = HT0 + YT(I)
        DC = SQRT(LC(I)*LC(I) + HC*HC)
        DT = SQRT(LT(I)*LT(I) + HT*HT)
        CC = DC - DC0
        TT = DT - DT0
        IF(KFUNC(1) /= 0)THEN
          FC =YFAC(1)* FINTER(KFUNC(1),CC,NPF,TF,FPC)
          FPC = FPC *YFAC(1)
          KC = FPC
        ELSEIF (CC >= CCL ) THEN
          FC  = HALF * KC*CCL
        ELSE
          FC = (KC - HALF*KBC * CC) * CC
        ENDIF
        IF(KFUNC(2) /= 0)THEN
          FT = YFAC(2)*FINTER(KFUNC(2),TT,NPF,TF,FPT)
          FPT = FPT *YFAC(2)
          KT = FPT
        ELSEIF (TT >= TTL ) THEN
          FT  = HALF * KT*TTL
        ELSE
          FT = (KT - HALF*KBT * TT) * TT
        ENDIF
C
        IF (ISMSTR == 11) THEN
          TRACE = (GSTR(I,1) + ONE) * (GSTR(I,2) + ONE)
          EC2 = ONE
          ET2 = ONE
        ELSE
          TRACE = EXP(GSTR(I,1) + GSTR(I,2))
          IF (TRACE /= ZERO) THEN
            EC2 = TRACE / MAX((EC(I) + ONE), EM6)
            ET2 = TRACE / MAX((ET(I) + ONE), EM6)
            RFAC = NC / EC2
            RFAT = NT / ET2
          ELSE
            RFAC = ZERO
            RFAT = ZERO
          ENDIF
        ENDIF
C---    contraintes membrane       
        SIGC = RFAC * FC * LC(I) / DC
        SIGT = RFAT * FT * LT(I) / DT
        SIGNXX(I) = SIGC
        SIGNYY(I) = SIGT
C---    contraintes cisaillement      
        SIG0    = UVAR(I,10)
        TAN_PHI = DEPSXY(I)
        PHI = atan (TAN_PHI)*HUNDRED80/PI
        IF (KFUNC(3) /= 0)THEN
          SIGNXY(I)  =YFAC(3)* FINTER(KFUNC(3),PHI,NPF,TF,GXY)
          GXY = GXY *YFAC(3)
        ELSEIF (TAN_PHI > TAN_LOCK) THEN
          SIGNXY(I) = GT*TAN_PHI + GB - SIG0
        ELSEIF (TAN_PHI < -TAN_LOCK) THEN
          SIGNXY(I) = GT*TAN_PHI - GB - SIG0
        ELSE
          SIGNXY(I) = G0*TAN_PHI - SIG0
        ENDIF
      ENDDO
C---
      DO I=JFT,JLT
        UVAR(I,6) = DEPSXY(I)
        UVAR(I,7) = YC(I)
        UVAR(I,8) = YT(I)
      ENDDO
C----------------  
C----------------  FIN LAW 58 sans hysteresis
C----------------  
      ELSE
      DO I=JFT,JLT
        LC(I) = ONE + EC(I)
        LT(I) = ONE + ET(I)
        EPSMAXC(I)   = UVAR(I,17)
        EMINCRL(I)   = UVAR(I,18)
        EPSMAXRLC(I) = UVAR(I,19)
        SLMAXC(I)    = UVAR(I,20)
        EPSMAXT(I)   = UVAR(I,21)
        EMINTRL(I)   = UVAR(I,22)
        EPSMAXRLT(I) = UVAR(I,23)
        SLMAXT(I)    = UVAR(I,24)
        SLMINC(I)    = UVAR(I,25)
        SLMINT(I)    = UVAR(I,26)
        SLMAXRLC(I)  = UVAR(I,27)
        SLMAXRLT(I)  = UVAR(I,28)
        EPSMAXS(I)   = UVAR(I,30)
        EMINSRL(I)   = UVAR(I,31)
        SLMAXS(I)    = UVAR(I,32)
        SLMINS(I)    = UVAR(I,33)
        EPSMAXRLS(I) = UVAR(I,34)
        SLMAXRLS(I)  = UVAR(I,35)
      ENDDO
      DO I=JFT,JLT
        FN(I) = ZERO
c---  uncoupled model  (compression of fiber)
        YC(I)    = UVAR(I,7)
        YT(I)    = UVAR(I,8)
        HC  = HC0 + YC(I)  !longueur ressort
        HT  = HT0 + YT(I)
        DC  = SQRT(LC(I)*LC(I) + HC*HC) ! long fibre
        DT  = SQRT(LT(I)*LT(I) + HT*HT)
        DCC = UVAR(I,15) - DC0
        DTT = UVAR(I,16) - DT0

        DDEC  = DC - UVAR(I,15)
        DDET  = DT - UVAR(I,16)
        IF(DCC >=ZERO ) THEN
           !----------------------LOAD
           DO ITER = 1, NITER
            HC  = HC0 + YC(I)  !longueur ressort
            DC  = SQRT(LC(I)*LC(I) + HC*HC) ! long fibre
            UDC = ONE / DC
            HDC = HC * UDC      !sinus(alpha)
            DCC = DC - DC0
            DCC = MAX (UVAR(I,18),DCC)
            DC  = DCC +  DC0
            UDC = ONE / DC
            HDC = HC * UDC      !sinus(alpha)
            FC          = YFAC(1) * FINTER(KFUNC(1),DCC,NPF,TF,FPC)
            FPC         = FPC * YFAC(1)
            KC = FPC
            FPC         = FPC  * HDC
            EPSMAXC(I)  = DCC
            SLMAXC(I)   = FC
            SLMAXRLC(I) = SLMAXC(I)
            EPSMAXRLC(I)= EPSMAXC(I)! UVAR(I,19))
            FUNC = KFC*YC(I) + FC*HC/DC
            DERIC= KFC + FPC*HDC + FC*UDC*(ONE - HDC*HDC)
            YC(I) = YC(I) - FUNC / DERIC
           ENDDO !iter
         ELSE ! DCC < ZERO domaine de compression
          DO ITER = 1, NITER
           HC  = HC0 + YC(I)  !longueur ressort
           DC  = SQRT(LC(I)*LC(I) + HC*HC) ! long fibre
           UDC = ONE / DC
           HDC = HC * UDC      !sinus(alpha)
           DCC = DC - DC0
           ! compression  fibre
           FC  = KKC  * DCC
           KC  = KKC
           FPC = KKC  * HDC! derivee % yc
           FUNC = KFC*YC(I) + FC*HC/DC
           DERIC= KFC + FPC*HDC + FC*UDC*(ONE - HDC*HDC)
           YC(I) = YC(I) - FUNC / DERIC
           FN(I) = ZERO
          ENDDO !iter
          EPSMAXC(I)   = ZERO
          EMINCRL(I)   = ZERO
          EPSMAXRLC(I) = ZERO
          SLMAXC(I)    = ZERO
          SLMINC(I)    = ZERO
          SLMAXRLC(I)  = ZERO
         ENDIF ! COMPRESSION OR TENSION FOR DIRECTION CHAINE
c----------------------------------
c----------------direction trame---
c----------------------------------
         IF(DTT >=ZERO ) THEN
           !----------------------LOAD
           DO ITER = 1, NITER
            HT  = HT0 + YT(I)  !longueur ressort
            DT  = SQRT(LT(I)*LT(I) + HT*HT) ! long fibre
            UDT = ONE / DT
            HDT = HT * UDT      !sinus(alpha)
            DTT = DT - DT0
            DTT = MAX (UVAR(I,22),DTT)
            DT  = DTT +  DT0
            UDT = ONE / DT
            HDT = HT * UDT      !sinus(alpha)
            FT = YFAC(2)*FINTER(KFUNC(2),DTT,NPF,TF,FPT)
            FPT = FPT * YFAC(2)
            KT  =  FPT
            FPT = FPT *HDT
            EPSMAXT(I)  = DTT
            SLMAXT(I)   = FT
            SLMAXRLT(I) = SLMAXT(I)
            EPSMAXRLT(I)= EPSMAXT(I)!
            EMINTRL(I)= ZERO
            SLMINT(I) = ZERO
            FUNT = KFT*YT(I) + FT*HT/DT
            DERIT= KFT + FPT*HDT + FT*UDT*(ONE - HDT*HDT)
            YT(I) = YT(I) - FUNT / DERIT
           ENDDO !iter
         ELSE ! DTT < ZERO domaine de compression
          DO ITER = 1, NITER
           HT  = HT0 + YT(I)  !longueur ressort
           DT  = SQRT(LT(I)*LT(I) + HT*HT) ! long fibre
           UDT = ONE / DT
           HDT = HT * UDT      !sinus(alpha)
           DTT = DT - DT0
           FT  = KKT  * DTT
           FPT = KKT  * HDT! derivee % yT
           KT  = KKT
           FUNT = KFT*YT(I) + FT*HT/DT
           DERIT= KFT + FPT*HDT + FT*UDT*(ONE - HDT*HDT)
           YT(I) = YT(I) - FUNT / DERIT
          ENDDO !iter
          EPSMAXT(I)   = ZERO
          EMINTRL(I)   = ZERO
          EPSMAXRLT(I) = ZERO
          SLMAXT(I)    = ZERO
          SLMINT(I)    = ZERO
          SLMAXRLT(I)  = ZERO
         ENDIF ! COMPRESSION OR TENSION FOR DIRECTION CHAINE
         FN(I) = ZERO

C  ========================================================
C---    Coupled model (traction fibre)
C  ========================================================
        IF ((YC(I) + YT(I)) < ZERO) THEN !eps
          HC = HC0 + UVAR(I,7)
          HT = HT0 + UVAR(I,8)
          DC = SQRT(LC(I)*LC(I) + HC*HC)
          DT = SQRT(LT(I)*LT(I) + HT*HT)
          DDEC  = DC - UVAR(I,15)
          DDET  = DT - UVAR(I,16)

          Y = HALF * (UVAR(I,7) - UVAR(I,8))
          HC = HC0 + Y
          HT = HT0 - Y
          DC = SQRT(LC(I)*LC(I) + HC*HC)
          DT = SQRT(LT(I)*LT(I) + HT*HT)
          DCC1 = UVAR(I,15) - DC0
          DTT1 = UVAR(I,16) - DT0
          KF = KFC + KFT
          DO ITER = 1, NITER
           HC = HC0 + Y
           HT = HT0 - Y
           DC = SQRT(LC(I)*LC(I) + HC*HC)
           DT = SQRT(LT(I)*LT(I) + HT*HT)
           DCC = DC - DC0
           DTT = DT - DT0
           UDC= ONE / DC
           UDT= ONE / DT
           HDC= HC * UDC
           HDT= HT * UDT
           IF(DCC1 >=ZERO  ) THEN
c----------------------------------LOAD
              FC          = YFAC(1) * FINTER(KFUNC(1),DCC,NPF,TF,FPC)
              FPC         = FPC * YFAC(1)
              KC = FPC
              FPC         = FPC  * HDC
              EPSMAXC(I)  = DCC
              SLMAXC(I)   = FC
              SLMAXRLC(I) = SLMAXC(I)
              EPSMAXRLC(I)= EPSMAXC(I)
              EMINCRL(I)= ZERO
              SLMINC(I) = ZERO
           ELSE ! DOMAINE POSITIF
            FC  = KKC  * DCC
            FPC = KKC  * HDC! derivee % yc
            KC  = KKC
            EPSMAXC(I)   = ZERO
            EMINCRL(I)   = ZERO
            EPSMAXRLC(I) = ZERO
            SLMAXC(I)    = ZERO
            SLMINC(I)    = ZERO
            SLMAXRLC(I)  = ZERO
           ENDIF ! (DCC1 >=ZERO  )
c----------------------------------
c----------------direction trame---
c----------------------------------

           IF(DTT1 >=ZERO  ) THEN
               FT = YFAC(2)*FINTER(KFUNC(2),DTT,NPF,TF,FPT)
               FPT = FPT * YFAC(2)
               KT  =  FPT
               FPT = FPT * HDT
               EPSMAXT(I)  = DTT
               SLMAXT(I)   = FT
               SLMAXRLT(I) = SLMAXT(I)
               EPSMAXRLT(I)= EPSMAXT(I)!
               EMINTRL(I)= ZERO
               SLMINT(I) = ZERO
           ELSE ! DOMAINE POSITIF
            ! compression  fibre
             FT  = KKT  * DTT
             FPT = KKT  * HDT! derivee % yT
             KT  = KKT
             EPSMAXT(I)   = ZERO
             EMINTRL(I)   = ZERO
             EPSMAXRLT(I) = ZERO
             SLMAXT(I)    = ZERO
             SLMINT(I)    = ZERO
             SLMAXRLT(I)  = ZERO
           ENDIF
C
           FUNC  = KF*Y + FC * HDC - FT * HDT
           DERIC = KF + FPC*HDC + FC*UDC*(ONE - HDC*HDC)
     .                  + FPT*HDT + FT*UDT*(ONE - HDT*HDT)
           Y = Y - FUNC / DERIC
C
           IF (Y > 0) THEN
              Y = MIN(Y, HT0)
           ELSE
              Y = MAX(Y,-HC0)
           ENDIF
          ENDDO !iter
          YC(I) = Y
          YT(I) =-Y
          FN(I) = FC*HC/DC + FT*HT/DT
        ENDIF
        HC = HC0 + YC(I)
        HT = HT0 + YT(I)
        DC = SQRT(LC(I)*LC(I) + HC*HC)
        DT = SQRT(LT(I)*LT(I) + HT*HT)
C------------------------------------------------------------------
C                             SHEAR
C------------------------------------------------------------------
        IF (ISMSTR == 11) THEN
          TRACE = (GSTR(I,1) + ONE) * (GSTR(I,2) + ONE)
          EC2 = ONE
          ET2 = ONE
        ELSE
          TRACE = EXP(GSTR(I,1) + GSTR(I,2))
          IF (TRACE /= ZERO) THEN
            EC2 = TRACE / MAX((EC(I) + ONE), EM6)
            ET2 = TRACE / MAX((ET(I) + ONE), EM6)
            RFAC = NC / EC2
            RFAT = NT / ET2
          ELSE
            RFAC = ZERO
            RFAT = ZERO
          ENDIF
        ENDIF
C---    contraintes membrane       
        SIGC = RFAC * FC * LC(I) / DC
        SIGT = RFAT * FT * LT(I) / DT
        SIGNXX(I) = SIGC
        SIGNYY(I) = SIGT
C---    contraintes cisaillement      
        SIG0    = UVAR(I,10)
        TAN_PHI = DEPSXY(I)
        PHI   = ATAN(TAN_PHI)*HUNDRED80/PI
        SIGNXY(I)  =YFAC(3)* FINTER(KFUNC(3),PHI,NPF,TF,GXY)
        UVAR(I,15) = DC
        UVAR(I,16) = DT
      ENDDO
C---
      DO I=JFT,JLT
        UVAR(I,6) = DEPSXY(I)
        UVAR(I,7) = YC(I)
        UVAR(I,8) = YT(I)
c
        UVAR(I,17) = EPSMAXC(I)
        UVAR(I,18) = EMINCRL(I)
        UVAR(I,19) = EPSMAXRLC(I)
        UVAR(I,20) = SLMAXC(I)
        UVAR(I,21) = EPSMAXT(I)
        UVAR(I,22) = EMINTRL(I)
        UVAR(I,23) = EPSMAXRLT(I)
        UVAR(I,24) = SLMAXT(I)
        UVAR(I,25) = SLMINC(I)
        UVAR(I,26) = SLMINT(I)
        UVAR(I,27) = SLMAXRLC(I)
        UVAR(I,28) = SLMAXRLT(I)
        UVAR(I,30) = EPSMAXS(I)
        UVAR(I,31) = EMINSRL(I)
        UVAR(I,32) = SLMAXS(I)
        UVAR(I,33) = SLMINS(I)
        UVAR(I,34) = EPSMAXRLS(I)
        UVAR(I,35) = SLMAXRLS(I)
      ENDDO
C----------------
      ENDIF
C----------------  FIN hysteresis
c
      DO I=JFT,JLT
        TENS(1,I) = SIGNXX(I)
        TENS(2,I) = SIGNYY(I)
        TENS(3,I) = SIGNXY(I)
      ENDDO
C----------------  RETOUR REPERE COQUE
      DO I=JFT,JLT
        R1 = DIR1(I,1)
        S1 = DIR1(I,2)
        R2 = DIR2(I,1)
        S2 = DIR2(I,2)
        RS1= R1*S1
        RS2= R2*S2
        R12= R1*R1
        R22= R2*R2
        S12= S1*S1
        S22= S2*S2
        RS3 = S1*S2-R1*R2
        R3R3= ONE+S1*R2+R1*S2
        R3R3= HALF*R3R3
        S3S3= ONE-S1*R2-R1*S2
        S3S3= HALF*S3S3
        T1 = TENS(1,I)
        T2 = TENS(2,I)
        T3 = TENS(3,I)
        TENS(1,I) = R12*T1 + R22*T2 - RS3*T3
        TENS(2,I) = S12*T1 + S22*T2 + RS3*T3
        TENS(3,I) = RS1*T1 + RS2*T2 + (R3R3 - S3S3)*T3
      ENDDO
C-----------------------
C      FORCES ET MOMENTS
C-----------------------
      DO I=JFT,JLT
        FOR(I,1)=FOR(I,1) + THK(I)*TENS(1,I)
        FOR(I,2)=FOR(I,2) + THK(I)*TENS(2,I)
        FOR(I,3)=FOR(I,3) + THK(I)*TENS(3,I)
      ENDDO
C
      DO I=JFT,JLT
         DEGMB(I) = DEGMB(I)+
     +              FOR(I,1)*EXX(I)+FOR(I,2)*EYY(I)+FOR(I,3)*EXY(I)
         EINT(I,1) = EINT(I,1) + DEGMB(I)*HALF*THK(I)*AREA(I)
      ENDDO
C-----------------------------------------------------------
C     REF-STATE ZEROSTRESS OPTION
C-----------------------------------------------------------
      IF(ZEROSTRESS /= ZERO)THEN
        DO I=JFT,JLT
          UVAR(I,11) = SIGNXX(I)
          UVAR(I,12) = SIGNYY(I)
          UVAR(I,13) = SIGNXY(I)
        ENDDO
c      ELSE
c        DO I=JFT,JLT
c         DEGMB(I) = DEGMB(I)+
c     +              FOR(I,1)*EXX(I)+FOR(I,2)*EYY(I)+FOR(I,3)*EXY(I)
c         EINT(I,1) = EINT(I,1) + DEGMB(I)*HALF*THK(I)*AREA(I)
c        ENDDO
      ENDIF
C-----------
      RETURN
      END
