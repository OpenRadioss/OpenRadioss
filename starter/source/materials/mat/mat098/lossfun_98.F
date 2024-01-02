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
Chd|  LOSSFUN_98                    source/materials/mat/mat098/lossfun_98.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        CALC_UNIAX                    source/materials/mat/mat098/lossfun_98.F
Chd|        CALC_UNIAX_2                  source/materials/mat/mat098/lossfun_98.F
Chd|        FCT_FIBER                     source/materials/mat/mat098/lossfun_98.F
Chd|        FCT_FIBER_2                   source/materials/mat/mat098/lossfun_98.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE LOSSFUN_98(NVAR,NDC,X,FUN,DFUN,CONS,DCONS,IGOTO,
     .                    NPC,PLD ,IFUNC,NFUNC,UPARAM,LENC,LENT,
     .                    XBIA,NPTBI,ISYM )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      !CHARACTER*nchartitle :: TITR
      !INTEGER  , DIMENSION(NFUNC)      :: IFUNC,FUNC_ID
      INTEGER     IGOTO ,SIZEPN,NPTBI
      INTEGER     NVAR,NDC
      my_real  
     .     CONS(*),DCONS(*),X(NVAR),UPARAM(*),FUN(NVAR),DFUN(NVAR),
     .     XBIA(NPTBI)
!      TYPE(TTABLE) TABLE(*)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPC(*), NFUNC, IFUNC(NFUNC)
      my_real FINTER ,PLD(*)
      EXTERNAL FINTER
C-----------------------------------------------
       INTENT(IN)    :: NPC,PLD,IGOTO ,XBIA,NPTBI
       INTENT(INOUT) :: FUN,DFUN,CONS,DCONS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,J,ID,NP1,NP2,J1,K,K1,NCON, ITER,NITER,
     .      PN1,PN2,ISYM,LENC,LENT
      my_real 
     .      EMBC,EMBT,Y,FMINS,FMINF,DEMBC,EMBCP,FLEXP,
     .      FLEX1,FLEX2,LC0,LT0,DC0,DT0,HC0,HT0,KC,KT,KFC,KFT,
     .      HC,HT,DCC,DC,DT,UDC,UDT,HDC,HDT,FC,FT,FPC,FPT,KF,
     .      DERIC,DTT,FUNC,DCP,DTP, T1,T2,A1,A2,DFLEX,FLEX2P,FLEX1P,
     .      FMINC,FMINT,FMINC2,FMINS2,FMINF2,FMINF1,YP,EMBTP,DEMBT,YFAC(8)

      my_real
     .      EC(LENC),FCU(LENC),LC(LENC),YC(LENC),SIGC(LENC), 
     .      SIGCP(LENC),
     .      XFIB(LENC),YFIB(LENC),
     .      XCFIB(NPTBI),YCFIB(NPTBI),XTFIB(NPTBI),YTFIB(NPTBI)
      my_real
     .      ET(LENT),FTU(LENT),LT(LENT),YT(LENT),SIGT(LENT),
     .      SIGTP(LENT)

c      my_real, DIMENSION(:)   ,ALLOCATABLE ::  
c---------------------------- 

      IF (ISYM == 1)THEN
      FMINF = ZERO

      FMINF1 = ZERO
      FMINF2 = ZERO
      FMINC = ZERO
      FMINT  = ZERO
      FMINS = ZERO
      FMINS2 = ZERO

c---------------------------- 
      EMBC = X(1)
      EMBT = X(3)
      FLEX1= X(2)
      FLEX2= X(4)
      DEMBC = MAX(EM03 * EMBC, EM10)
      DFLEX = MAX(EM03 * FLEX1, EM10)
      DEMBT = MAX(EM03 * EMBT, EM10)

      NITER= 5
      DO I=1,8! NFUNC
       YFAC(I)= UPARAM(8+I)  
      ENDDO
      DC0 = ONE+EMBC
      HC0 = SQRT(DC0*DC0 - ONE)

      DT0 = ONE+EMBT 
      HT0 = SQRT(DT0*DT0 - ONE)


      ! TRANSFORMER CONTRAINTE DEF TISSU EN FORCE DEP FIBRE
      CALL FCT_FIBER_2(NPC,PLD ,IFUNC(7),IFUNC(8),YFAC(7),YFAC(8),XBIA,NPTBI,
     .       FLEX1,FLEX2,DC0,HC0,DT0,HT0,XCFIB,YCFIB,XTFIB,YTFIB )
C---------------------------------------------------------------------
      PN1  = NPC(IFUNC(1))
      PN2  = NPC(IFUNC(1)+1)
      LENC = HALF*(PN2 - PN1) ! 
      !DIRECTION CHAINE  = FMINC
      DO I = 1,LENC
       II = 2*(I-1)
       ! for each x(i) we have sigc(i) OR sigt(i) as response        
       EC(I) = PLD(NPC(IFUNC(1))+ II )
       FCU(I)= PLD(NPC(IFUNC(1))+ II+1 )*YFAC(1)
        CALL  CALC_UNIAX_2(EC(I),XCFIB,YCFIB,LENC,DC0,HC0,
     .                   YFAC(1),FLEX1,FLEX2,EMBC ,SIGC(I))   !SIGC = SIGMA CALCULATED
        A1 = (FCU(I) - SIGC(I))/MAX(EM20,FCU(I))
        FMINC = FMINC + A1**2   !somme de la difference des carres
      ENDDO
      !DIRECTION TRAME = FMINT
      PN1  = NPC(IFUNC(2))
      PN2  = NPC(IFUNC(2)+1)
      LENT = HALF*(PN2 - PN1) ! 
      DO I = 1,LENT
       II = 2*(I-1)  
       ET(I) = PLD(NPC(IFUNC(2))+ II ) 
       FTU(I)= PLD(NPC(IFUNC(2))+ II+1 )*YFAC(2)
        CALL  CALC_UNIAX_2(ET(I),XTFIB,YTFIB,LENT,DT0,HT0,
     .                   YFAC(2),FLEX1,FLEX2,EMBT,SIGT(I))   !SIGT = SIGMA CALCULATED
        A2 = (FTU(I) - SIGT(I))/MAX(EM20,FTU(I))
        FMINT = FMINT + A2**2   !somme de la difference des carres
      ENDDO
C----------------------------------------------------------- 
      IF (IGOTO == 5 .or. IGOTO == 8) FUN =  (FMINC + FMINT )/TWO                                   
C----------------------------------------------------------
c     DERIVATIVES - CHAINE
C----------------------------------------------------------
      !Deriv over stretch ( a voir si besoin de recalculer fonctions fibres)
      IF (IGOTO == 3 .or. IGOTO == 8) THEN
      !PERTURBATION STRETCH DIRECTION CHAINE  = FMINS
         EMBCP  = X(1) + DEMBC 
         FLEX1  = X(2)
         DC0 = ONE + EMBCP
         HC0 = SQRT(DC0*DC0 - ONE)
         DO I = 1,LENC
           II = 2*(I-1)
           EC(I) = PLD(NPC(IFUNC(1))+ II )
           FCU(I)= PLD(NPC(IFUNC(1))+ II +1)*YFAC(1)
           CALL  CALC_UNIAX_2(EC(I),XCFIB,YCFIB,LENC,DC0,HC0,
     .                     YFAC(1),FLEX1,FLEX2,EMBCP,SIGC(I))
c
           A1 = (FCU(I) - SIGC(I))/MAX(EM20,FCU(I))
           FMINS = FMINS + A1**2 !somme de la difference des carres
         ENDDO  !LENC    
c        FMINS   = SQRT(FMINS)                                        
         DFUN(1) = (FMINS - FMINC) / DEMBC 
C----------------------------------------------------------
         ! Deriv over flex                          
C----------------------------------------------------------
         !PERTURBATION FLEX DIRECTION CHAINE  = FMINF1 ET FMINF2
         EMBC  = X(1)
         EMBT  = X(3)
         FLEXP = X(2) + DFLEX
         FLEX2 = X(4)
         DC0 = ONE + EMBC
         HC0 = SQRT(DC0*DC0 - ONE)
         DO I = 1,LENC
           II = 2*(I-1)
           EC(I) = PLD(NPC(IFUNC(1))+ II )
           FCU(I)= PLD(NPC(IFUNC(1))+ II +1)*YFAC(1)
           CALL  CALC_UNIAX_2(EC(I),XCFIB,YCFIB,LENC,DC0,HC0,
     .                     YFAC(1),FLEXP,FLEX2,EMBC,SIGC(I))
c
           A1 = (FCU(I) - SIGC(I))/MAX(EM20,FCU(I))
           !A1 = FCU(I) - SIGC(I)
           FMINF1 = FMINF1 + A1**2 !somme de la difference des carres
         ENDDO !LENC  

         DT0 = ONE + EMBT
         HT0 = SQRT(DT0*DT0 - ONE)
         DO I = 1,LENT
           II = 2*(I-1)
           ET(I) = PLD(NPC(IFUNC(2))+ II )
           FTU(I)= PLD(NPC(IFUNC(2))+ II +1)*YFAC(2)
           CALL  CALC_UNIAX_2(ET(I),XTFIB,YTFIB,LENT,DT0,HT0,
     .                     YFAC(2),FLEXP,FLEX2,EMBT,SIGT(I))
c
           A1 = (FTU(I) - SIGT(I))/MAX(EM20,FTU(I))
           FMINF2 = FMINF2 + A1**2 !somme de la difference des carres
         ENDDO !LENT   
c        FMINF   = SQRT(FMINF)                                        
         DFUN(2) = (FMINF1 + FMINF2 - FMINC - FMINT) / DFLEX                                     
C----------------------------------------------------------
c     DERIVATIVES - TRAME
C----------------------------------------------------------
         FMINF1 = ZERO
         FMINF2 = ZERO
         EMBC = X(1)
         FLEX1= X(2)
         !PERTURBATION STRETCH DIRECTION TRAME  = FMINS2
         EMBTP  = X(3) + DEMBT 
         FLEX2  = X(4)
         DT0 = ONE + EMBTP
         HT0 = SQRT(DT0*DT0 - ONE)
         DO I = 1,LENT
           II = 2*(I-1)
           ET(I) = PLD(NPC(IFUNC(2))+ II )
           FTU(I)= PLD(NPC(IFUNC(2))+ II +1)*YFAC(2)
           CALL  CALC_UNIAX_2(ET(I),XTFIB,YTFIB,LENT,DT0,HT0,
     .                     YFAC(2),FLEX1,FLEX2,EMBTP,SIGT(I))
c
           A1 = (FTU(I) - SIGT(I))/MAX(EM20,FTU(I))
           FMINS2 = FMINS2 + A1**2 !somme de la difference des carres
         ENDDO  !LENT    
c        FMINS   = SQRT(FMINS)                                        
         DFUN(3) = (FMINS2 - FMINT) / DEMBT      
C----------------------------------------------------------
        ! Deriv over flex                          
        !PERTURBATION FLEX DIRECTION TRAME  = FMINF1 ET FMINF2
         EMBC = X(1)
         EMBT = X(3)
         FLEX1= X(2)
         FLEXP = X(4) + DFLEX

         DC0 = ONE + EMBC
         HC0 = SQRT(DC0*DC0 - ONE)
         DO I = 1,LENC
           II = 2*(I-1)
           EC(I) = PLD(NPC(IFUNC(1))+ II )
           FCU(I)= PLD(NPC(IFUNC(1))+ II +1)*YFAC(1)
           CALL  CALC_UNIAX_2(EC(I),XCFIB,YCFIB,LENC,DC0,HC0,
     .                     YFAC(1),FLEX1,FLEXP,EMBC,SIGC(I))
c
           A1 = (FCU(I) - SIGC(I))/MAX(EM20,FCU(I))
           !A1 = FCU(I) - SIGC(I)
           FMINF1 = FMINF1 + A1**2 !somme de la difference des carres
         ENDDO !LENC  
         DT0 = ONE + EMBT
         HT0 = SQRT(DT0*DT0 - ONE)
         DO I = 1,LENT
           II = 2*(I-1)
           ET(I) = PLD(NPC(IFUNC(2))+ II )
           FTU(I)= PLD(NPC(IFUNC(2))+ II +1)*YFAC(2)
           CALL  CALC_UNIAX_2(ET(I),XTFIB,YTFIB,LENT,DT0,HT0,
     .                     YFAC(2),FLEX1,FLEXP,EMBT,SIGT(I))
c
           A1 = (FTU(I) - SIGT(I))/MAX(EM20,FTU(I))
           !A1 = FCU(I) - SIGC(I)
           FMINF2 = FMINF2 + A1**2 !somme de la difference des carres
         ENDDO !LENT   
         DFUN(4) = (FMINF1 + FMINF2 - FMINC - FMINT) / DFLEX                                     
      ENDIF !IGOTO
C----------------------------------------------------------
C------------------ISYM------------------------
C----------------------------------------------------------
      ELSE
      FMINC = ZERO
      FMINS = ZERO
      FMINF = ZERO

      EMBC = X(1)
      EMBT = X(1)
      FLEX1= X(2)
      FLEX2= X(2)
      DEMBC = MAX(EM03 * EMBC, EM10)
      DFLEX = MAX(EM03 * FLEX1, EM10)
      NITER= 5
      DO I=1,8! NFUNC
       YFAC(I)= UPARAM(8+I)  
      ENDDO
      DC0 = ONE+EMBC
      HC0 = SQRT(DC0*DC0 - ONE)

      DT0 = ONE+EMBT 
      HT0 = SQRT(DT0*DT0 - ONE)
      ! IL FAUT PRENDRE LA LONGUEUR DE LA FONCTION CR  EE
      PN1 = NPC(IFUNC(7))
      PN2 = NPC(IFUNC(7)+1)
      SIZEPN = HALF*(PN2 - PN1) ! 


      ! TRANSFORMER CONTRAINTE DEF TISSU EN FORCE DEP FIBRE
      CALL FCT_FIBER(NPC,PLD ,IFUNC(7),SIZEPN,DC0,HC0,XFIB,YFIB)

      ! STOP

      DO I = 1,SIZEPN
       II = 2*(I-1)
       ! for each x(i) we have sigc(i) and sigt(i) as response        
       EC(I) = PLD(NPC(IFUNC(1))+ II )
       FCU(I)= PLD(NPC(IFUNC(1))+ II+1 )*YFAC(1)

        CALL  CALC_UNIAX(EC(I),XFIB,YFIB,SIZEPN,DC0,HC0,
     .                  YFAC(1),FLEX1,EMBC,SIGC(I)) !SIGC = SIGMA CALCULATED
        A1 = (FCU(I) - SIGC(I))/MAX(EM20,FCU(I))
        FMINC = FMINC + A1**2   !somme de la difference des carres
      ENDDO
      !STOP
      IF (IGOTO == 5 .or. IGOTO == 8) FUN =  FMINC                                      
C----------------------------------------------------------
c     DERIVATIVES
C----------------------------------------------------------
      !Deriv over stretch ( a voir si besoin de recalculer fonctions fibres)
      IF (IGOTO == 3 .or. IGOTO == 8) THEN
         EMBCP  = X(1) + DEMBC 
         FLEX1  = X(2)
         DC0 = ONE + EMBCP
         HC0 = SQRT(DC0*DC0 - ONE)
         DT0 = DC0
         HT0 = HC0  
         DO I = 1,SIZEPN
           II = 2*(I-1)
           EC(I) = PLD(NPC(IFUNC(1))+ II )
           FCU(I)= PLD(NPC(IFUNC(1))+ II +1)*YFAC(1)
           CALL  CALC_UNIAX(EC(I),XFIB,YFIB,SIZEPN,DC0,HC0,
     .                     YFAC(1),FLEX1,EMBCP,SIGC(I))
           A1 = (FCU(I) - SIGC(I))/MAX(EM20,FCU(I))
           FMINS = FMINS + A1**2 !somme de la difference des carres
         ENDDO  !SIZEPN    
c        FMINS   = SQRT(FMINS)                                        
         DFUN(1) = (FMINS - FMINC) / DEMBC      
C----------------------------------------------------------
        ! Deriv over flex                          
         EMBC  = X(1)
         FLEXP = X(2) + DFLEX
         DC0 = ONE + EMBC
         HC0 = SQRT(DC0*DC0 - ONE)
         DT0 = DC0
         HT0 = HC0  
         DO I = 1,SIZEPN
           II = 2*(I-1)
           EC(I) = PLD(NPC(IFUNC(1))+ II )
           FCU(I)= PLD(NPC(IFUNC(1))+ II +1)*YFAC(1)
           CALL  CALC_UNIAX(EC(I),XFIB,YFIB,SIZEPN,DC0,HC0,
     .                     YFAC(1),FLEXP,EMBC,SIGC(I))
c
           A1 = (FCU(I) - SIGC(I))/MAX(EM20,FCU(I))
           !A1 = FCU(I) - SIGC(I)
           FMINF = FMINF + A1**2 !somme de la difference des carres
         ENDDO !SIZEPN                                                 
c        FMINF   = SQRT(FMINF)                                        
         DFUN(2) = (FMINF - FMINC) / DFLEX                                     
C----------------------------------------------------------
      ENDIF !IGOTO
      ENDIF !ISYM
C----------------------------------------------------------
      !write(*,'(A,3G20.13)')'FUN , DF1,DF2 ', FUN,DFUN(1),DFUN(2)
c-----------
      RETURN
      END


Chd|====================================================================
Chd|  CALC_UNIAX                    source/materials/mat/mat098/lossfun_98.F
Chd|-- called by -----------
Chd|        LOSSFUN_98                    source/materials/mat/mat098/lossfun_98.F
Chd|-- calls ---------------
Chd|        FINTER58                      source/materials/mat/mat098/lossfun_98.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE CALC_UNIAX(EC,XFIB,YFIB,SIZEPN,DC0,HC0,
     .           YFAC,FLEX,STRETCH,SIGC)
!           CALL  CALC_UNIAX(EC,XFIB,YFIB,NPT,DC0,HC0,
!     .                     YFAC(7),FLEX,EMBCP,SIGC(I))
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
c----------------------------------------------- 
      INTEGER  SIZEPN
      my_real 
     .   XFIB(SIZEPN),YFIB(SIZEPN)
      my_real 
     .    EC,DC0,HC0,YFAC,FLEX,STRETCH,SIGC,FUNC
c---------------------------- 
      INTEGER  ITER,NITER
      my_real 
     .    HC,HT,YC,DC,DCC,UDC,HDC,LC,LT,DERIC,DT0,
     .  FC,EC2,FPC
      my_real 
     .    FINTER58
      EXTERNAL FINTER58
c----------------------------------------------- 

      NITER = 5
      YC  = ZERO      ! initialization
      LC  = ONE + EC
c----------------------
      DO ITER = 1, NITER
        HC   = HC0 + YC 
        DC   = SQRT(LC*LC + HC*HC) 
        DCC  = DC - DC0
        UDC  = ONE / DC
        HDC  = HC * UDC
        FC   = YFAC * FINTER58(DCC,XFIB,YFIB,FPC,SIZEPN)
        FPC  = FPC*HDC*YFAC
        FUNC = TWO*FLEX*YC + FC*HDC
        DERIC= TWO*FLEX + FPC*HDC + FC*UDC*(ONE - HDC*HDC)
        YC   = YC - FUNC / MAX(DERIC,EM20)
      ENDDO !iter
c----------------------
      HC   = HC0 + YC  
      HT   = HC0 - YC 
      DC   = SQRT(LC*LC + HC*HC) 
      DCC  = DC - DC0    
      FC   = YFAC * FINTER58(DCC,XFIB,YFIB,FPC,SIZEPN)
      DT0 = DC0
      LT   = SQRT(MAX(EM20, DT0**2 - HT**2))
      EC2  = LT - ONE 
      SIGC = FC * LC / DC ! ENGINEERING

c      SIGC = FC * LC / DC /(EC2+ONE)!TRUE
c
c-----------
      RETURN
      END 
      
C----------------------------------------------------------
      my_real 
Chd|====================================================================
Chd|  FINTER58                      source/materials/mat/mat098/lossfun_98.F
Chd|-- called by -----------
Chd|        CALC_UNIAX                    source/materials/mat/mat098/lossfun_98.F
Chd|        CALC_UNIAX_2                  source/materials/mat/mat098/lossfun_98.F
Chd|-- calls ---------------
Chd|====================================================================
     .     FUNCTION FINTER58(XX,XC,YC,DERI,NPT)
!                     FINTER58(DCC,XFIB,YFIB,FPC,SIZEPN)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------

      INTEGER IFUNC, I,NPT
      my_real 
     .    ABC,DERI,XX,DX1,DX2,XC(NPT),YC(NPT)
C-----------------------------------------------
C
        DX2 = XC(1) - XX
        DO 100 I=2,NPT
        DX1 = -DX2
        DX2 = XC(I) - XX
        IF(DX2>=0.0.OR.I==NPT)THEN
          DERI = (YC(I) - YC(I-1)) / (XC(I) - XC(I-1))
          IF(DX1<=DX2)THEN
            FINTER58 = YC(I-1) + DX1 * DERI
          ELSE
            FINTER58 = YC(I) - DX2 * DERI
          ENDIF
          RETURN
        ENDIF
 100    CONTINUE

C
      RETURN
      END      

Chd|====================================================================
Chd|  FCT_FIBER_2                   source/materials/mat/mat098/lossfun_98.F
Chd|-- called by -----------
Chd|        LOSSFUN_98                    source/materials/mat/mat098/lossfun_98.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE FCT_FIBER_2(NPC,PLD ,IDN1,IDN2,YFAC1,YFAC2,
     .           XBIA,NPTBI,KFC,KFT,DC0,HC0,DT0,HT0,XCFIB,YCFIB,XTFIB,YTFIB )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPC(*) 
      my_real FINTER ,PLD(*),XBIA(NPTBI)
      EXTERNAL FINTER
      INTEGER I,J,IDN1,IDN2,NPTBI,NP1, NITER,ITER
      my_real 
     .   DC0,HC0,DT0,HT0,LC,LT,DC,UDC,UDT,HDC,HDT,YFAC1,YFAC2,YC,YT,
     .   FPC,FPT, Y,HC,HT,DT, FC,FT,KF,KFT,KFC,FUNC,DERIC,
     .   XX(NPTBI),YB(NPTBI),XFIB(NPTBI),YFIB(NPTBI),XCFIB(NPTBI),
     .   YCFIB(NPTBI),XTFIB(NPTBI),YTFIB(NPTBI),DCC(NPTBI),
     .   DTT(NPTBI),STISSUC(NPTBI),STISSUT(NPTBI)
C-----------------------------------------------
       INTENT(IN)    :: NPC,PLD ,NPTBI,YFAC1,YFAC2
c---------------------------------------------- 
      !IDN = IFUNC(7)
      NITER = 5
      DO I = 1, NPTBI-1       
        J = 2*(I-1)
        STISSUC(I) =  YFAC1 * FINTER(IDN1,XBIA(I),NPC,PLD,FPC)!IFUNC7
        STISSUT(I) =  YFAC2 * FINTER(IDN2,XBIA(I),NPC,PLD,FPT)!IFUNC8
        LC = ONE + XBIA(I)  
        LT = ONE + XBIA(I) 

c        FTISS(I)= YB(I)*(1.0 + XX(I))
         Y = ZERO
         DO ITER = 1, NITER
            HC = HC0 + Y 
            HT = HT0 - Y 
            DC = SQRT(LC *LC  + HC*HC) 
            DT = SQRT(LT *LT  + HT*HT)
            UDC= ONE / DC
            UDT= ONE / DT
            HDC= HC * UDC
            HDT= HT * UDT
            FC  = STISSUC(I) * LC/DC
            FPC = -FC * HDC/DC
            !FPC = STISSUC(I) * HDC
            FT  = STISSUT(I) * LT/DT
            FPT = -FT * HDT/DT
            !FPT = STISSUT(I) * HDT
            KF  = KFC + KFT
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
          ENDDO !iter

          YC = Y
          YT =-Y
          HC = HC0 + YC   
          HT = HT0 + YT   
          DC = SQRT(LC *LC  + HC*HC) 
          DT = SQRT(LT *LT  + HT*HT) 
          XCFIB(I) = DC - DC0
          XTFIB(I) = DT - DT0
          YCFIB(I) = STISSUC(I)  *LC/DC
          YTFIB(I) = STISSUT(I)  *LT/DT
          !PRINT*, 'C,XFIB, YFIB ', I, XCFIB(I), YCFIB(I)
          !PRINT*, 'T,XFIB, YFIB ', I, XTFIB(I), YTFIB(I)
      ENDDO
         ! STOP
c-----------
      RETURN
      END 

Chd|====================================================================
Chd|  FCT_FIBER                     source/materials/mat/mat098/lossfun_98.F
Chd|-- called by -----------
Chd|        LOSSFUN_98                    source/materials/mat/mat098/lossfun_98.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE FCT_FIBER(NPC,PLD ,IDN,SIZEPN,DC0,HC0,XFIB,YFIB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPC(*) 
      my_real 
     .    PLD(*)
      INTEGER I,J,IDN,SIZEPN,NP1
      my_real 
     .   XX(SIZEPN),YB(SIZEPN),XFIB(SIZEPN),YFIB(SIZEPN),DC0,HC0,
     .   LC,DC
C-----------------------------------------------
       INTENT(IN)    :: NPC,PLD 
c---------------------------- 
      !IDN = IFUNC(7)
      NP1  = (NPC(IDN+1)-NPC(IDN))*HALF  

      DO I = 1, NP1        
        J = 2*(I-1)
        XX(I) = PLD(NPC(IDN)+J)
        LC = ONE + XX(I)
        DC = SQRT(HC0*HC0 + LC*LC)
        XFIB(I) = DC - DC0
        YFIB(I) = PLD(NPC(IDN)+J+1)*LC/DC
c        FTISS(I)= YB(I)*(1.0 + XX(I))
      ENDDO
c-----------
      RETURN
      END 

Chd|====================================================================
Chd|  CALC_UNIAX_2                  source/materials/mat/mat098/lossfun_98.F
Chd|-- called by -----------
Chd|        LOSSFUN_98                    source/materials/mat/mat098/lossfun_98.F
Chd|-- calls ---------------
Chd|        FINTER58                      source/materials/mat/mat098/lossfun_98.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE CALC_UNIAX_2(EC,XFIB,YFIB,SIZEPN,DC0,HC0,
     .           YFAC,FLEX1,FLEX2,EMBC ,SIGC)
!        CALL  CALC_UNIAX_2(EC(I),XCFIB,YCFIB,LENC,DC0,HC0,
!     .                   YFAC(1),FLEX1,FLEX2,EMBC ,EMBT,SIGC(I))   !SIGC = SIGMA CALCULATED
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
c----------------------------------------------- 
      INTEGER  SIZEPN
      my_real 
     .   XFIB(SIZEPN),YFIB(SIZEPN)
      my_real 
     .    EC,DC0,HC0,YFAC,FLEX1,FLEX2,EMBC ,SIGC,FUNC
c---------------------------- 
      INTEGER  ITER,NITER
      my_real 
     .    HC,HT,YC,DC,DCC,UDC,HDC,LC,LT,DERIC,DT0,
     .  FC,EC2,FPC
      my_real 
     .    FINTER58
      EXTERNAL FINTER58
c----------------------------------------------- 

      NITER = 5
      YC  = ZERO      ! initialization
      LC  = ONE + EC
c----------------------
      DO ITER = 1, NITER
        HC   = HC0 + YC 
        DC   = SQRT(LC*LC + HC*HC) 
        DCC  = DC - DC0
        UDC  = ONE / DC
        HDC  = HC * UDC
        FC   = YFAC * FINTER58(DCC,XFIB,YFIB,FPC,SIZEPN)
        FPC  = FPC*HDC*YFAC
        FUNC = (FLEX1+FLEX2)*YC + FC*HDC
        DERIC= (FLEX1+FLEX2) + FPC*HDC + FC*UDC*(ONE - HDC*HDC)
        YC   = YC - FUNC / MAX(DERIC,EM20)
      ENDDO !iter
c----------------------
      HC   = HC0 + YC  
      DC   = SQRT(LC*LC + HC*HC) 
      DCC  = DC - DC0    
      FC   = YFAC * FINTER58(DCC,XFIB,YFIB,FPC,SIZEPN)
      SIGC = FC * LC / DC ! ENGINEERING (ON PREND L INITIAL EC2 = 0)

c      SIGC = FC * LC / DC /(EC2+ONE)
c
c-----------
      RETURN
      END 

