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
Chd|  SIGEPS158C                    source/materials/mat/mat158/sigeps158c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|        VINTER2                       source/tools/curve/vinter.F   
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE SIGEPS158C(
     1           NEL       ,NUPARAM   ,NUVAR     ,NFUNC     ,IFUNC     ,
     2           NPF       ,TF        ,TIME      ,TIMESTEP  ,UPARAM    ,
     3           AREA      ,THKLY     ,SOUNDSP   ,VISCMAX   ,UVAR      ,
     4           DEPSXX    ,DEPSYY    ,DEPSXY    ,DEPSYZ    ,DEPSZX    ,
     5           EPSXX     ,EPSYY     ,EPSXY     ,EPSYZ     ,EPSZX     ,
     6           SIGOXX    ,SIGOYY    ,SIGOXY    ,SIGOYZ    ,SIGOZX    ,
     7           SIGNXX    ,SIGNYY    ,SIGNXY    ,SIGNYZ    ,SIGNZX    ,
     8           SIGVXX    ,SIGVYY    ,SIGVXY    ,TAN_PHI   ,OFFGG     ,
     9           RHO0      ,ETSE      ,SHF       ,ALDT      ,NSENSOR   ,
     A           SENSOR_TAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL    |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C NFUNC   |  1      | I | R | NUMBER FUNCTION USED FOR THIS USER LAW
C IFUNC   | NFUNC   | I | R | FUNCTION INDEX 
C NPF     |  *      | I | R | FUNCTION ARRAY   
C TF      |  *      | F | R | FUNCTION ARRAY 
C---------+---------+---+---+--------------------------------------------
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C RHO0    | NEL    | F | R | INITIAL DENSITY
C AREA    | NEL    | F | R | AREA
C THKLY   | NEL    | F | R | LAYER THICKNESS
C ...     |         |   |   |
C DEPSXX  | NEL    | F | R | STRAIN INCREMENT XX
C DEPSYY  | NEL    | F | R | STRAIN INCREMENT YY
C ...     |         |   |   |
C EPSXX   | NEL    | F | R | STRAIN XX  TRUE
C EPSYY   | NEL    | F | R | STRAIN YY  TRUE
C ...     |         |   |   |
C SIGOXX  | NEL    | F | R | OLD ELASTO PLASTIC STRESS XX 
C SIGOYY  | NEL    | F | R | OLD ELASTO PLASTIC STRESS YY
C ...     |         |   |   |    
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL    | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL    | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C SIGVXX  | NEL    | F | W | VISCOUS STRESS XX
C SIGVYY  | NEL    | F | W | VISCOUS STRESS YY
C ...     |         |   |   |
C SOUNDSP | NEL    | F | W | SOUND SPEED (NEEDED FOR TIME STEP)
C VISCMAX | NEL    | F | W | MAXIMUM DAMPING MODULUS(NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFFG    | NEL    | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NEL,NFUNC,NUPARAM,NUVAR,NSENSOR
      my_real ,INTENT(IN) :: TIME,TIMESTEP
      INTEGER ,DIMENSION(NFUNC),INTENT(IN) :: IFUNC
      INTEGER ,DIMENSION(SNPC) ,INTENT(IN) :: NPF
      my_real ,DIMENSION(STF)  ,INTENT(IN) :: TF
      my_real ,DIMENSION(NUPARAM) ,INTENT(IN) :: UPARAM
      my_real ,DIMENSION(NEL) ,INTENT(IN) :: AREA,RHO0,THKLY,OFFGG,
     .   DEPSXX,DEPSYY,DEPSXY,DEPSYZ,DEPSZX,EPSXX,EPSYY,EPSXY,EPSYZ,EPSZX,
     .   SIGOXX,SIGOYY,SIGOXY,SIGOYZ,SIGOZX,SHF,ALDT
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) ,INTENT(IN) :: SENSOR_TAB
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real ,DIMENSION(NEL) ,INTENT(OUT) :: TAN_PHI,ETSE,
     .    SIGNXX,SIGNYY,SIGNXY,SIGNYZ,SIGNZX,
     .    SIGVXX,SIGVYY,SIGVXY,SOUNDSP,VISCMAX
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real ,DIMENSION(NEL,NUVAR) ,INTENT(INOUT) :: UVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,ITER,NITER,ISENS,IFUNCC,IFUNCT,IFUNCS,IFUNF1,IFUNF2
      INTEGER ,DIMENSION(NEL) :: IADF1,IADF2,IAD1,IAD2,IAD3,ILEN1,ILEN2,ILEN3,
     .   ILENF1,ILENF2,IPOS,IPOS1,IPOS2
      my_real :: KMAX,GMAX,GFROT,GSH,STIFF,LOGC,LOGT,KFLEX,KFLEX1,KFLEX2,
     .   MASS,DYC,DYT,DC0,DT0,H0,HC0,HT0,VISCE,VISCG,LC0,LT0,HDC,HDT,
     .   TFROT,SIGG,EC2,ET2,SINN,TAN2,DAMP,V1,V2,DTINV,ETC,ETT,TRACE,
     .   ZEROSTRESS,DSIG,LMIN,TSTART,PHI1,PHI2,J11,J12,J21,J22,DET
      my_real ,DIMENSION(3)   :: YFAC
      my_real ,DIMENSION(NEL) :: SIGC,SIGT,EC,ET,LC,LT,FN,ANGLE,KG,GXY,
     .   DTANG,TFOLD,CVISC,CVIST,FLEXC,FLEXT,FLEXF,HC,HT,DC,DT,DCC,DTT,
     .   FC,FT,FPC,FPT,DFLXC,DFLXT,EPSF,EPSFC,EPSFT,XC,XT,BETA,PH01,PH02
C-----------------------------------------------
C   S t a t e    V a r i a b l e s  (U V A R)
C-----------------------------------------------
c     UVAR(1)  = SIGNXX + SIGVXX  ! total stress in 1st direction (elastic + viscous)
c     UVAR(2)  = SIGNYY + SIGVYY  ! total stress in 2nd direction (elastic + viscous)
c     UVAR(3)  = SIGNXY + SIGVXY  ! total shear stress (elastic + friction)
c     UVAR(4)  = EC               ! total element elongation in 1st direction
c     UVAR(5)  = ET               ! total element elongation in 2nd direction
c     UVAR(6)  = DEPSXY           ! tan(alpha) of shear angle
c     UVAR(7)  = YC               ! total flex element elongation in 1st direction
c     UVAR(8)  = YT               ! total flex element elongation in 2nd direction
c     UVAR(9)  = SIGVXY           ! Friction stress (shear)
c     UVAR(10) = SIG0             ! Initial shear stress (if initial shear angle > 0)
c     UVAR(11) = SIGNXX           ! elastic stress in 1st direction (used for zerostress relaxation)
c     UVAR(12) = SIGNYY           ! elastic stress in 2nd direction (used for zerostress relaxation)
c     UVAR(13) = SIGNXY           ! elastic stress in shear (used for zerostress relaxation)
c     UVAR(14) = ALDT             ! initial characteristic length of the element
c     UVAR(15) = DC               ! total fiber elongation in 1st direction           
c     UVAR(16) = DT               ! total fiber elongation in 2nd direction
C======================================================================|
C---  Initialisations
C----------------------------------------------------------------------
      LC0      = ONE
      LT0      = ONE
      DC0      = UPARAM(1)  
      DT0      = UPARAM(2)  
      HC0      = UPARAM(3)  
      HT0      = UPARAM(4)  
      KFLEX    = UPARAM(5)
      KFLEX1   = UPARAM(6)       ! stiffness factor for log function in dir1
      KFLEX2   = UPARAM(7)       ! stiffness factor for log function in dir1
      ZEROSTRESS = UPARAM(8)
      ISENS    = NINT(UPARAM(9))
      KMAX     = UPARAM(10)
      GMAX     = UPARAM(11)
      YFAC(1)  = UPARAM(12)
      YFAC(2)  = UPARAM(13)
      YFAC(3)  = UPARAM(14)
      
      IFUNCC   = IFUNC(1)
      IFUNCT   = IFUNC(2)
      IFUNCS   = IFUNC(3)
      IFUNF1   = IFUNC(4)
      IFUNF2   = IFUNC(5)
      
      GFROT    = GMAX
      GSH      = SHF(1)
      VISCE    = EM02       ! fiber visc damping coefficient
      VISCG    = EM02       ! shear damping coefficient
c
      IF (ZEROSTRESS > ZERO .and. ISENS > 0) THEN
        TSTART = SENSOR_TAB(ISENS)%TSTART
      ELSE
        TSTART = ZERO
      ENDIF  
      DTINV = TIMESTEP/MAX(TIMESTEP**2,EM20)
c
      H0    = HC0 + HT0
      NITER = 10     
C-----------------------------------------------------------
#include "vectorize.inc"
      DO I = 1,NEL
        TFOLD(I) = UVAR(I,9)
        MASS     = RHO0(I)*AREA(I)*THKLY(I)*FOURTH  ! 2m = rho*V/4
        CVISC(I) = SQRT(MASS*KFLEX1)*DTINV*THIRD
        CVIST(I) = SQRT(MASS*KFLEX2)*DTINV*THIRD
      ENDDO

c---  strain integration -> total engineering strains
#include "vectorize.inc"
      DO I = 1,NEL
        ETC = UVAR(I,4) + DEPSXX(I)     
        ETT = UVAR(I,5) + DEPSYY(I)     
        UVAR(I,4) = ETC
        UVAR(I,5) = ETT
        EC(I) = EXP(ETC) - ONE         ! eng strain dir 1
        ET(I) = EXP(ETT) - ONE         ! eng strain dir 2
        LC(I) = LC0 * (ONE + EC(I))    ! element length dir 1
        LT(I) = LT0 * (ONE + ET(I))    ! element length dir 2 
      ENDDO

      DO I = 1,NEL
        BETA(I) = ONE             ! increment scale factor for Newton iterations
        PH01(I) = EP10
        PH02(I) = EP10
      ENDDO
c
#include "vectorize.inc"
      DO I = 1,NEL
        IPOS(I)  = 1
        IAD1(I)  = NPF(IFUNCC)   / 2 + 1
        IAD2(I)  = NPF(IFUNCT)   / 2 + 1
        IAD3(I)  = NPF(IFUNCS)   / 2 + 1
        ILEN1(I) = NPF(IFUNCC+1) / 2 - IAD1(I) - IPOS(I)
        ILEN2(I) = NPF(IFUNCT+1) / 2 - IAD2(I) - IPOS(I)
        ILEN3(I) = NPF(IFUNCS+1) / 2 - IAD3(I) - IPOS(I)
      END DO
      IF (IFUNF1 > 0) THEN
        DO I = 1,NEL
          IADF1(I)  = NPF(IFUNF1)   / 2 + 1
          ILENF1(I) = NPF(IFUNF1+1) / 2 - IADF1(I) - IPOS(I)
        END DO
      END IF       
      IF (IFUNF2 > 0) THEN
        DO I = 1,NEL
          IADF2(I)  = NPF(IFUNF2)   / 2 + 1
          ILENF2(I) = NPF(IFUNF2+1) / 2 - IADF2(I) - IPOS(I)
        END DO
      END IF       
c                  
c---  resolve nonlinear fiber equilibrium equations using Newton iterations
c
      EPSFC(1:NEL) = UVAR(1:NEL,7)       ! eng strain flex dir 1
      EPSFT(1:NEL) = UVAR(1:NEL,8)       ! eng strain flex dir 2

      DO ITER = 1,NITER

        EPSF (1:NEL) =(HC0 * EPSFC(1:NEL) + HT0 * EPSFT(1:NEL)) / H0 ! eng strain coupling flex spring
        FLEXF(1:NEL) = KFLEX * EPSF(1:NEL)
        DO I = 1,NEL
          XC(I)  = EPSFC(I)
          XT(I)  = EPSFT(I)
          HC(I)  = HC0 * (EPSFC(I) + ONE)      ! length of flex spring dir 1
          HT(I)  = HT0 * (EPSFT(I) + ONE)      ! length of flex spring dir 2
          DC(I)  = SQRT(LC(I)**2 + HC(I)**2)   ! fiber length in dir 1
          DT(I)  = SQRT(LT(I)**2 + HT(I)**2)   ! fiber length in dir 2
          DCC(I) = DC(I) - DC0
          DTT(I) = DT(I) - DT0
        END DO
        IPOS1(1:NEL) = 1
        IPOS2(1:NEL) = 1

        CALL VINTER2(TF,IAD1,IPOS1,ILEN1,NEL,DCC,FPC,FC)   ! fiber force dir 1
        CALL VINTER2(TF,IAD2,IPOS2,ILEN2,NEL,DTT,FPT,FT)   ! fiber force dir 2
c
       ! flex force dir 1 
        IF (IFUNF1 > 0) THEN
          IPOS(1:NEL) = 1
          CALL VINTER2(TF,IADF1,IPOS,ILENF1,NEL,EPSFC,DFLXC,FLEXC)       
          FLEXC(1:NEL) = FLEXC(1:NEL) * KFLEX1
          DFLXC(1:NEL) = DFLXC(1:NEL) * KFLEX1          
        ELSE        
          DO I = 1,NEL
            FLEXC(I) = KFLEX1 * LOG(EPSFC(I) + ONE)
            DFLXC(I) = KFLEX1 / (EPSFC(I) + ONE)
          END DO
        END IF
c       flex force dir 2 
        IF (IFUNF2 > 0) THEN
          IPOS(1:NEL) = 1
          CALL VINTER2(TF,IADF2,IPOS,ILENF2,NEL,EPSFT,DFLXT,FLEXT)       
          FLEXT(1:NEL) = FLEXT(1:NEL) * KFLEX2
          DFLXT(1:NEL) = DFLXT(1:NEL) * KFLEX2          
        ELSE        
          DO I = 1,NEL
            FLEXT(I) = KFLEX2 * LOG(EPSFT(I) + ONE)
            DFLXT(I) = KFLEX2 / (EPSFT(I) + ONE)
          END DO
        END IF
c
#include "vectorize.inc"
        DO I = 1,NEL
          DYC  = EPSFC(I) - UVAR(I,7)
          DYT  = EPSFT(I) - UVAR(I,8)
          HDC  = HC(I) / DC(I)
          HDT  = HT(I) / DT(I)          
          PHI1 = FC(I) * HDC + FLEXC(I) + FLEXF(I)  + CVISC(I)*DYC
          PHI2 = FT(I) * HDT + FLEXT(I) + FLEXF(I)  + CVIST(I)*DYT
          J12  = KFLEX * HT0 / H0
          J21  = KFLEX * HC0 / H0
          J11  = J12 + FPC(I)*HDC*HC0 + DFLXC(I) + CVISC(I)
          J22  = J21 + FPT(I)*HDT*HT0 + DFLXT(I) + CVIST(I)
          DET  = J11 * J22 - J12 * J21

          EPSFC(I) = EPSFC(I) - BETA(I) * (J22 * PHI1 - J12 * PHI2) / DET          
          EPSFT(I) = EPSFT(I) + BETA(I) * (J12 * PHI1 - J11 * PHI2) / DET
          
          EPSFC(I) = MAX(EPSFC(I), EM04 - ONE)       
          EPSFT(I) = MAX(EPSFT(I), EM04 - ONE)  

          IF (ABS(PHI1) > PH01(I) .and.  ABS(PHI2) > PH02(I)) THEN
            EPSFC(I) = XC(I)
            EPSFT(I) = XT(I)
            BETA(I)  = BETA(I) * HALF
            BETA(I)  = MAX(BETA(I), EM02)
          END IF
          PH01(I) = ABS(PHI1)
          PH02(I) = ABS(PHI2) 
        END DO
c
      END DO    ! ITER 
c
#include "vectorize.inc"
      DO I = 1,NEL
        SIGC(I)    = FC(I) * LC(I) / DC(I)
        SIGT(I)    = FT(I) * LT(I) / DT(I)
        UVAR(I,7)  = EPSFC(I)
        UVAR(I,8)  = EPSFT(I)
        UVAR(I,1)  = SIGC(I)
        UVAR(I,2)  = SIGT(I)
        UVAR(I,15) = DC(I)
        UVAR(I,16) = DT(I)
        FN(I)      = FLEXF(I)   ! normal force for friction in shear 
      ENDDO !i,nel
c
C------------------------------------------------------------------
C     Trace = Eps1 + Eps2  (Trace of true principal strain tensor)
C     Trace = ec_true + ec2_true
C     ec2_eng  = exp(Tr) / (ec_eng + 1) - 1
C     rfac =  2*Nc / (ec2_eng+1)
C     ec2_eng+1 > 0
C------------------------------------------------------------------
#include "vectorize.inc"
      DO I = 1,NEL
        TRACE = EXP(EPSXX(I) + EPSYY(I))! =exp(tr)
        EC2 = MAX(TRACE / (EC(I) + ONE), EM6)
        ET2 = MAX(TRACE / (ET(I) + ONE), EM6)
C---    true stress membrane       
        SIGNXX(I) = SIGC(I) / EC2 
        SIGNYY(I) = SIGT(I) / ET2 
      ENDDO !i,nel
c------------------------------------------------------------------
c---  SHEAR    
c------------------------------------------------------------------
#include "vectorize.inc"
      DO I = 1,NEL
        TAN_PHI(I)= DEPSXY(I)
        ANGLE(I)  = ATAN(TAN_PHI(I))*HUNDRED80/PI
        IPOS(I) = 1
      ENDDO
c
      CALL VINTER2(TF,IAD3,IPOS,ILEN3,NEL,ANGLE,GXY,SIGNXY)   ! shear stress =f(angle)
c
#include "vectorize.inc"
      DO I = 1,NEL
        TAN2  = TAN_PHI(I)**2
        KG(I) = GXY(I) * TAN2 * YFAC(3)        
c---    fiber visc damping
        DAMP = SQRT(RHO0(I)*AREA(I)*THKLY(I)*HALF)
        V1   = VISCE*DAMP*SQRT(KMAX)
        V2   = VISCE*DAMP*SQRT(KMAX)
        SIGVXX(I) = DTINV*(DEPSXX(I))*V1
        SIGVYY(I) = DTINV*(DEPSYY(I))*V2
c---    friction in shear  
        IF (FN(I) > ZERO) THEN
          TFROT = TWO_THIRD*VISCG*FN(I)*(HC0+HT0)/(LC(I)+LT(I))
          DTANG(I) = DEPSXY(I) - TAN_PHI(I)
          SIGG =  TFOLD(I) + GFROT*DTANG(I)
          IF (ABS(SIGG) > TFROT) THEN
            SIGVXY(I) = SIGN(TFROT,SIGG)
          ELSE
            SIGVXY(I) = SIGG
          ENDIF
        ENDIF  
C---
        SINN  = TAN_PHI(I) / SQRT(ONE + TAN2)
        STIFF = KMAX*(ONE+SINN) + GMAX
        LMIN  = MIN(DC(I)/DC0,DT(I)/DT0)*UVAR(I,14)
        SOUNDSP(I) = SQRT(STIFF/(RHO0(I)))*ALDT(I)/LMIN
        VISCMAX(I) = MAX(V1,V2)
        ETSE(I)    = ONE
      ENDDO
C---
#include "vectorize.inc"
      DO I = 1,NEL
        UVAR(I,3) = SIGNXY(I) + SIGVXY(I)
        TAN_PHI(I)= DEPSXY(I)
        UVAR(I,6) = DEPSXY(I)
        UVAR(I,9) = SIGVXY(I)
c
        SIGNYZ(I) = SIGOYZ(I) + GSH * DEPSYZ(I)
        SIGNZX(I) = SIGOZX(I) + GSH * DEPSZX(I)
      ENDDO
C-----------------------------------------------------------
C     REF-STATE ZEROSTRESS OPTION
C-----------------------------------------------------------
      IF (ZEROSTRESS > ZERO)THEN
        IF (TIME <= TSTART) THEN
#include "vectorize.inc"
         DO I = 1,NEL
            UVAR(I,11) = SIGNXX(I)
            UVAR(I,12) = SIGNYY(I)
            UVAR(I,13) = SIGNXY(I)
            SIGNXX(I)  = ZERO
            SIGNYY(I)  = ZERO
            SIGNXY(I)  = ZERO
          ENDDO
        ELSE
#include "vectorize.inc"
          DO I = 1,NEL
            DSIG = SIGNXX(I) - SIGOXX(I) - UVAR(I,11)
            IF((UVAR(I,11) > ZERO).AND.(DSIG < ZERO))THEN
              UVAR(I,11) = MAX(ZERO,UVAR(I,11)+ZEROSTRESS*DSIG)
            ELSEIF((UVAR(I,11) < ZERO).AND.(DSIG > ZERO))THEN
              UVAR(I,11) = MIN(ZERO,UVAR(I,11)+ZEROSTRESS*DSIG)
            ENDIF
            DSIG = SIGNYY(I) - SIGOYY(I) - UVAR(I,12)
            IF((UVAR(I,12) > ZERO).AND.(DSIG < ZERO))THEN
              UVAR(I,12) = MAX(ZERO,UVAR(I,12)+ZEROSTRESS*DSIG)
            ELSEIF((UVAR(I,12) < ZERO).AND.(DSIG > ZERO))THEN
              UVAR(I,12) = MIN(ZERO,UVAR(I,12)+ZEROSTRESS*DSIG)
            ENDIF
            DSIG = SIGNXY(I) - SIGOXY(I) - UVAR(I,13)
            IF((UVAR(I,13) > ZERO).AND.(DSIG < ZERO))THEN
              UVAR(I,13) = MAX(ZERO,UVAR(I,13)+ZEROSTRESS*DSIG)
            ELSEIF((UVAR(I,13) < ZERO).AND.(DSIG > ZERO))THEN
              UVAR(I,13) = MIN(ZERO,UVAR(I,13)+ZEROSTRESS*DSIG)
            ENDIF
            SIGNXX(I) = SIGNXX(I) - UVAR(I,11)
            SIGNYY(I) = SIGNYY(I) - UVAR(I,12)
            SIGNXY(I) = SIGNXY(I) - UVAR(I,13)
          ENDDO
        ENDIF
      ENDIF
c     
      DO I = 1,NEL
        SOUNDSP(I) = SQRT(KMAX*TWO/(RHO0(I)))
      ENDDO
C-----------
      RETURN
      END SUBROUTINE SIGEPS158C
