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
Chd|  SIGEPS58C                     source/materials/mat/mat058/sigeps58c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        INTERFACE_TABLE_MOD           share/modules/table_mod.F     
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE SIGEPS58C(
     1     NEL0   ,NUPARAM,NUVAR   ,MFUNC   ,KFUNC  ,
     2     NPF    ,NPT0    ,IPT    ,IFLAG   ,NSENSOR,
     2     TF     ,TIME   ,TIMESTEP,UPARAM  ,RHO0   ,
     3     AREA   ,EINT   ,THKLY   ,
     4     EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
     5     DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ  ,DEPSZX ,
     6     EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ   ,EPSZX  ,
     7     SIGOXX ,SIGOYY ,SIGOXY  ,SIGOYZ  ,SIGOZX ,
     8     SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     9     SIGVXX ,SIGVYY ,SIGVXY  ,SIGVYZ  ,SIGVZX ,
     A     SOUNDSP,VISCMAX,THK     ,PLA     ,UVAR   ,
     B     OFF    ,NGL    ,PM      ,MAT     ,ETSE   ,
     C     SHF    ,YLD    ,EPSP    ,TAN_PHI ,
     D     ALDT   ,SENSOR_TAB,ISMSTR,TABLE  ,OFFGG   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE_MOD
      USE INTERFACE_TABLE_MOD
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL0    |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL0 
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C NFUNC   |  1      | I | R | NUMBER FUNCTION USED FOR THIS USER LAW
C IFUNC   | NFUNC   | I | R | FUNCTION INDEX 
C NPF     |  *      | I | R | FUNCTION ARRAY   
C NPT0    |  1      | I | R | NUMBER OF LAYERS OR INTEGRATION POINTS   
C IPT     |  1      | I | R | LAYER OR INTEGRATION POINT NUMBER   
C IFLAG   |  *      | I | R | GEOMETRICAL FLAGS   
C TF      |  *      | F | R | FUNCTION ARRAY 
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C RHO0    | NEL0    | F | R | INITIAL DENSITY
C AREA    | NEL0    | F | R | AREA
C EINT    | 2*NEL0  | F | R | INTERNAL ENERGY(MEMBRANE,BENDING)
C THKLY   | NEL0    | F | R | LAYER THICKNESS
C EPSPXX  | NEL0    | F | R | STRAIN RATE XX
C EPSPYY  | NEL0    | F | R | STRAIN RATE YY
C ...     |         |   |   |
C DEPSXX  | NEL0    | F | R | STRAIN INCREMENT XX
C DEPSYY  | NEL0    | F | R | STRAIN INCREMENT YY
C ...     |         |   |   |
C EPSXX   | NEL0    | F | R | STRAIN XX  TRUE
C EPSYY   | NEL0    | F | R | STRAIN YY  TRUE
C ...     |         |   |   |
C SIGOXX  | NEL0    | F | R | OLD ELASTO PLASTIC STRESS XX 
C SIGOYY  | NEL0    | F | R | OLD ELASTO PLASTIC STRESS YY
C ...     |         |   |   |    
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL0    | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL0    | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C SIGVXX  | NEL0    | F | W | VISCOUS STRESS XX
C SIGVYY  | NEL0    | F | W | VISCOUS STRESS YY
C ...     |         |   |   |
C SOUNDSP | NEL0    | F | W | SOUND SPEED (NEEDED FOR TIME STEP)
C VISCMAX | NEL0    | F | W | MAXIMUM DAMPING MODULUS(NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C THK     | NEL0    | F |R/W| THICKNESS
C PLA     | NEL0    | F |R/W| PLASTIC STRAIN
C UVAR    |NEL0*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL0    | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "com01_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NSENSOR
      INTEGER NEL0, NUPARAM, NUVAR, NPT0, IPT,ISMSTR,IFLAG(*),
     .   NGL(NEL0),MAT(NEL0)
      my_real TIME,TIMESTEP(NEL0),UPARAM(*),
     .   AREA(NEL0),RHO0(NEL0),EINT(NEL0,2),
     .   THKLY(NEL0),PLA(NEL0),OFFGG(NEL0),TAN_PHI(NEL0),
     .   EPSPXX(NEL0),EPSPYY(NEL0),
     .   EPSPXY(NEL0),EPSPYZ(NEL0),EPSPZX(NEL0),
     .   DEPSXX(NEL0),DEPSYY(NEL0),
     .   DEPSXY(NEL0),DEPSYZ(NEL0),DEPSZX(NEL0),
     .   EPSXX(NEL0) ,EPSYY(NEL0) ,
     .   EPSXY(NEL0) ,EPSYZ(NEL0) ,EPSZX(NEL0) ,
     .   SIGOXX(NEL0),SIGOYY(NEL0),
     .   SIGOXY(NEL0),SIGOYZ(NEL0),SIGOZX(NEL0),
     .   PM(NPROPM,*),SHF(*),EPSP(NEL0),ALDT(NEL0)
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real
     .    SIGNXX(NEL0),SIGNYY(NEL0),
     .    SIGNXY(NEL0),SIGNYZ(NEL0),SIGNZX(NEL0),
     .    SIGVXX(NEL0),SIGVYY(NEL0),
     .    SIGVXY(NEL0),SIGVYZ(NEL0),SIGVZX(NEL0),
     .    SOUNDSP(NEL0),VISCMAX(NEL0),ETSE(NEL0)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real UVAR(NEL0,NUVAR), OFF(NEL0),THK(NEL0),YLD(NEL0)
      TYPE(TTABLE) TABLE(*)
      my_real, DIMENSION(1) :: XX
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*), MFUNC, KFUNC(MFUNC)
      my_real FINTER ,TF(*)
      EXTERNAL FINTER
C        Y = FINTER(IFUNC(J),X,NPF,TF,DYDX)
C        Y       : y = f(x)
C        X       : x
C        DYDX    : f'(x) = dy/dx
C        IFUNC(J): FUNCTION INDEX
C              J : FIRST(J=1), SECOND(J=2) .. FUNCTION USED FOR THIS LAW
C        NPF,TF  : FUNCTION PARAMETER
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NC,NT,ISENS,ITER,NITER,NBFCT,NINDX,NUMOFF,UNLOAD,IERR,
     .        IFUNCC,IFUNCT,IDXCNT,IDX1,IDX2,IERRT,IERRC,INDEX(MVSIZ),
     .        INDXOFF(MVSIZ),TAGOLDC,TAGOLDT,TAGC(0:1),TAGT(0:1)
      my_real
     .        KC,KT,KFC,KFT,G0,GT,GB,TAN_LOCK,VISCE,VISCG,LC0,LT0,
     .        KBC,KBT,DC0,DT0,HC0,HT0,EC0,ET0,DC,DT,TRACE,GFROT,GSH,
     .        AC,AT,BC,BT,BC2,BT2,DCC,DTT,FUNC,FUNT,DERIC,DERIT,DTINV,
     .        TFROT,SIGG,Y,EC2,ET2,SIGC,SIGT,SIG0,UDC,UDT,FC,FT,
     .        FPC,FPT,CCL,TTL,DEC,DET,HC,HT,KF,KG,KMAX,HDC,HDT,
     .        SINN,TAN2,DAMP,RFAC,RFAT,V1,V2,AREAMIN,DAREAMIN,
     .        AA,VV,ZEROSTRESS,DSIG,LMIN,TSTART,YFAC(6),YY,PHI,GXY,
     .        FLEX1,FLEX2,FACC,FACT,KFC0,KFT0,ETC,ETT,PHIN,PHII,SXYI,
     .        DDEC,DDET,COEFUL,COEFRL,DCC1,dtt1,DCC0,dtt0,PHII2,SXYI2,
     .        EPSI1,SIGI1,EPSI2,SIGI2,KKC,KKT,EMINC,EMAXC,SMINC,SMAXC,
     .        EMINT,EMAXT,SMINT,SMAXT,FACE_C,FACE_T,FACS_C,FACS_T,
     .        FACE,FACF,FATE,FATF,FCL,FTL,FCLP,FTLP,TEST,DPHI,PHIO,KC0,KT0,
     .        DYC,DYT,MASS
      my_real
     .        EC(MVSIZ),ET(MVSIZ),LC(MVSIZ),LT(MVSIZ),FN(MVSIZ),SIGG0(MVSIZ),
     .        DTANG(MVSIZ),TFOLD(MVSIZ),YC(MVSIZ),YT(MVSIZ),
     .        EMINRLC(MVSIZ),EMAXRLC(MVSIZ),EPSMAXC(MVSIZ),
     .        EMINRLT(MVSIZ),EMAXRLT(MVSIZ),EPSMAXT(MVSIZ),
     .        EPSNC(MVSIZ),EPSNT(MVSIZ),EPSNS(MVSIZ),
     .        EPSNRLC(MVSIZ),EPSNRLT(MVSIZ),SMAXRLC(MVSIZ),SMAXRLT(MVSIZ),
     .        SMINRLC(MVSIZ),SIGMAXC(MVSIZ),SMINRLT(MVSIZ),SIGMAXT(MVSIZ),
     .        PHIMIN(MVSIZ),PHIMAX(MVSIZ),PHIRLMAX(MVSIZ),SXYMAX(MVSIZ),
     .        SXYMIN(MVSIZ),SXYMAXRL(MVSIZ),CVISC(MVSIZ),CVIST(MVSIZ)
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
      NITER    = 3
      LC0      = UPARAM( 1)  
      LT0      = UPARAM( 2)  
      DC0      = UPARAM( 3)  
      DT0      = UPARAM( 4)  
      HC0      = UPARAM( 5)  
      HT0      = UPARAM( 6)  
      NC       = UPARAM( 7)  
      NT       = UPARAM( 8)  
      KC0      = UPARAM( 9)  ! young dir1
      KT0      = UPARAM(10)  ! young dir2
      KKC      = UPARAM(40)  ! initial slope dir1
      KKT      = UPARAM(41)  ! initial slope dir2
      KFC0     = UPARAM(11)  
      KFT0     = UPARAM(12)  
      G0       = UPARAM(13)  
      GT       = UPARAM(14)  
      GB       = UPARAM(15)  
      TAN_LOCK = UPARAM(16)  
      VISCE    = UPARAM(17)  
      VISCG    = UPARAM(18)  
      KBC      = UPARAM(19)  
      KBT      = UPARAM(20)  
      GFROT    = UPARAM(21)  
      AREAMIN  = UPARAM(22)  
      DAREAMIN = UPARAM(23)  
      ZEROSTRESS = UPARAM(24)
      ISENS    = NINT(UPARAM(25))
      FLEX1    = UPARAM(26)
      FLEX2    = UPARAM(27)
      NBFCT    = NINT(UPARAM(31))
      GSH      = UPARAM(32)*SHF(1)
      IF (ZEROSTRESS > ZERO .and. ISENS > 0) THEN
        TSTART = SENSOR_TAB(ISENS)%TSTART
      ELSE
        TSTART   = ZERO
      ENDIF  
c
      YFAC(1) = UPARAM(27 + 1 )
      YFAC(2) = UPARAM(27 + 2 )
      YFAC(3) = UPARAM(27 + 3 )
      YFAC(4) = UPARAM(33)
      YFAC(5) = UPARAM(34)
      YFAC(6) = UPARAM(42)
      UNLOAD  = NINT(UPARAM(35))
c
      KFC = KFC0  
      KFT = KFT0
      KC  = KC0
      KT  = KT0
      NINDX  = 0
      NUMOFF = 0
C-----------------------------------------------------------
C     DESACTIVATION FOR VERY WARPED ELEMENT (QBAT)
C-----------------------------------------------------------
      IF (NPT0 ==1) THEN
        DO I=1,NEL0
          IF (UVAR(I,40)==ONE) CYCLE
          DEPSXX(I) = DEPSXX(I)*UVAR(I,40)
          DEPSYY(I) = DEPSYY(I)*UVAR(I,40)
          AA =UVAR(I,10)/G0
          DEPSXY(I) = AA*(ONE-UVAR(I,40))+DEPSXY(I)*UVAR(I,40)
        ENDDO
      ENDIF
C
      IF (UNLOAD == 0) THEN !no hysteresis
c      
        ! KF  = KFC + KFT
        ! CCL :  eps value when derivative = 0
        IF (KBC == ZERO) THEN 
          CCL = EP30 
        ELSE
          CCL = KC / KBC ! condition pour derivee nulle 
        ENDIF

        IF (KBT == ZERO) THEN
         TTL = EP30
        ELSE
         TTL = KT / KBT             
        ENDIF
        DO I=1,NEL0
          IF(OFFGG(I) > ZERO) THEN
            NINDX = NINDX+1
            INDEX(NINDX)=I
          ELSE
            NUMOFF = NUMOFF+1
            INDXOFF(NUMOFF)=I
          ENDIF
        ENDDO
#include "vectorize.inc"
        DO  J=1,NINDX
          I=INDEX(J)
          YC(I)    = UVAR(I,7) 
          YT(I)    = UVAR(I,8) 
          TFOLD(I) = UVAR(I,9)
          MASS     = RHO0(I)*AREA(I)*THKLY(I)*FOURTH  ! 2m = rho*V/4
          DTINV    = TIMESTEP(I)/MAX(TIMESTEP(I)*TIMESTEP(I),EM20)
          CVISC(I) = SQRT(MASS*KFC0)*DTINV*THIRD
          CVIST(I) = SQRT(MASS*KFT0)*DTINV*THIRD
        ENDDO

C---  integration des deformations
#include "vectorize.inc"
        DO  J=1,NINDX
          I=INDEX(J)
          ETC =UVAR(I,4)+DEPSXX(I)     
          ETT =UVAR(I,5)+DEPSYY(I)     
          UVAR(I,4) = ETC
          UVAR(I,5) = ETT
          EC(I) = EXP(ETC) - ONE ! eng strain
          ET(I) = EXP(ETT) - ONE
        ENDDO
#include "vectorize.inc"
        DO  J=1,NINDX
          I=INDEX(J)                                   
          LC(I) = LC0 * (ONE + EC(I)) 
          LT(I) = LT0 * (ONE + ET(I)) 
        ENDDO
c                    
c---    Calcul YC, YT
        AC = KC*DC0
        AC = AC*AC 
        AT = KT*DT0
        AT = AT*AT
#include "vectorize.inc"
        DO  J=1,NINDX
          I=INDEX(J)
          FN(I) = ZERO
c---      uncoupled model  (compression of fiber)
          DYC = ZERO
          DYT = ZERO
          DO ITER = 1, NITER
            HC  = HC0 + YC(I)  !longueur ressort
            HT  = HT0 + YT(I)     
            DC  = SQRT(LC(I)*LC(I) + HC*HC) ! long fibre  
            DT  = SQRT(LT(I)*LT(I) + HT*HT)   
            UDC = ONE / DC                     
            UDT = ONE / DT                     
            HDC = HC * UDC      !sinus(alpha)              
            HDT = HT * UDT                    
            DCC = DC - DC0                    
            DTT = DT - DT0     
            IF (KFUNC(1) /= 0) THEN            
              FC = YFAC(1)*FINTER(KFUNC(1),DCC,NPF,TF,FPC)
              FPC = FPC *YFAC(1)
              KC = FPC
              KFC = FLEX1*FPC*HC0/DC0  ! rigidite de flexion
              IF(KFC == ZERO)KFC=KFC0
              FPC = FPC*HDC
            ELSEIF (DCC >= CCL ) THEN 
              FC  = HALF * KC*CCL
              FPC = ZERO
            ELSE    
              FC  = (KC - HALF*KBC * DCC) * DCC        
              FPC = (KC - KBC*DCC) * HDC! derivee % yc
            ENDIF 
            IF (KFUNC(2) /= 0) THEN
              FT = YFAC(2)*FINTER(KFUNC(2),DTT,NPF,TF,FPT)
              FPT = FPT * YFAC(2)
              KT  = FPT
              KFT = FLEX2*FPT*HT0/DT0
              IF(KFT == ZERO)KFT=KFT0
              FPT = FPT*HDT
            ELSEIF (DTT >= TTL ) THEN
              FT  = HALF * KT*TTL
              FPT = ZERO       
            ELSE           
              FT  = (KT - HALF*KBT * DTT) * DTT        
              FPT = (KT - KBT*DTT) * HDT   
            ENDIF
            FUNC = KFC*YC(I) + FC*HC/DC + CVISC(I)*DYC
            FUNT = KFT*YT(I) + FT*HT/DT + CVIST(I)*DYT          
            DERIC= KFC + FPC*HDC + FC*UDC*(ONE - HDC*HDC) + CVISC(I)
            DERIT= KFT + FPT*HDT + FT*UDT*(ONE - HDT*HDT) + CVIST(I)
C
            YC(I) = YC(I) - FUNC / DERIC
            YT(I) = YT(I) - FUNT / DERIT
            DYC   = YC(I) - UVAR(I,7)
            DYT   = YT(I) - UVAR(I,8)
            FN(I) = ZERO
          ENDDO !iter   
C---      Coupled model (traction fibre)
          DYC = ZERO
          DYT = ZERO
          IF ((YC(I) + YT(I)) < ZERO) THEN !eps
            Y = HALF * (UVAR(I,7) - UVAR(I,8))
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
             IF (KFUNC(1) /= 0) THEN            
               FC =YFAC(1)* FINTER(KFUNC(1),DCC,NPF,TF,FPC)
               FPC = FPC *YFAC(1)
               KC  = FPC
               KFC = FLEX1*FPC*HC0/DC0
               IF(KFC == ZERO)KFC=KFC0
               FPC = FPC*HDC
              ELSEIF (DCC >= CCL ) THEN
                FC  = HALF * KC*CCL
                FPC = ZERO
              ELSE 
                FC  = (KC - HALF*KBC * DCC) * DCC      
                FPC = (KC - KBC*DCC) * HDC
              ENDIF 
              IF (KFUNC(2) /= 0) THEN
                FT = YFAC(2)*FINTER(KFUNC(2),DTT,NPF,TF,FPT)
                FPT = FPT * YFAC(2)
                KT = FPT
                KFT = FLEX2*FPT*HT0/DT0
                IF(KFT == ZERO)KFT=KFT0
                FPT = FPT*HDT
             ELSEIF (DTT >= TTL ) THEN
                FT  = HALF * KT*TTL
                FPT = ZERO  
              ELSE     
                FT  = (KT - HALF*KBT * DTT) * DTT      
                FPT = (KT - KBT*DTT) * HDT 
              ENDIF
C
              KF = KFC + KFT
              FUNC  = KF*Y + FC * HDC - FT * HDT
     .              + (CVISC(I) + CVIST(I))*DYC
              DERIC = KF + FPC*HDC + FC*UDC*(ONE - HDC*HDC)
     .                   + FPT*HDT + FT*UDT*(ONE - HDT*HDT) 
     .                   + CVISC(I) + CVIST(I)
              Y = Y - FUNC / DERIC
              DYC = Y - HALF * (UVAR(I,7) - UVAR(I,8))
C
              IF (Y > 0) THEN
                Y = MIN(Y, HT0)
              ELSE
                Y = MAX(Y,-HC0)
              ENDIF
            ENDDO !iter
c
            YC(I) = Y
            YT(I) =-Y
            FN(I) = FC*HC/DC + FT*HT/DT
          ENDIF
          HC = HC0 + YC(I)   
          HT = HT0 + YT(I)    
          DC = SQRT(LC(I)*LC(I) + HC*HC) 
          DT = SQRT(LT(I)*LT(I) + HT*HT) 
        ENDDO !i,nel
C-------------------------------------      
#include "vectorize.inc"
        DO  J=1,NINDX
          I=INDEX(J)
          HC = HC0 + YC(I)   
          HT = HT0 + YT(I)    
          DC = SQRT(LC(I)*LC(I) + HC*HC) 
          DT = SQRT(LT(I)*LT(I) + HT*HT) 
          DCC = DC - DC0    
          DTT = DT - DT0  
          IF (KFUNC(1) /= 0) THEN            
            FC =YFAC(1)* FINTER(KFUNC(1),DCC,NPF,TF,FPC)
            FPC = FPC *YFAC(1)
            KC = FPC
          ELSEIF (DCC >= CCL ) THEN            
            FC  = HALF * KC*CCL           
          ELSE                              
            FC  = (KC - HALF*KBC * DCC) * DCC          
          ENDIF                             
          IF(KFUNC(2) /= 0)THEN
            FT = YFAC(2)*FINTER(KFUNC(2),DTT,NPF,TF,FPT)
            FPT = FPT * YFAC(2)
            KT = FPT
          ELSEIF (DTT >= TTL ) THEN            
            FT  = HALF * KT*TTL           
          ELSE                              
            FT  = (KT - HALF*KBT * DTT) * DTT          
          ENDIF                             
C------------------------------------------------------------------
C         Trace = Eps1 + Eps2  (Trace of true principal strain tensor)
C         Trace = ec_true + ec2_true
C         ec2_eng  = exp(Tr) / (ec_eng + 1) - 1
C         rfac =  2*Nc / (ec2_eng+1)
C         ec2_eng+1 > 0
C------------------------------------------------------------------
          TRACE = EXP(EPSXX(I) + EPSYY(I))! =exp(tr)
          EC2 = MAX(TRACE / (EC(I) + ONE), EM6)
          ET2 = MAX(TRACE / (ET(I) + ONE), EM6)
          RFAC = NC / EC2
          RFAT = NT / ET2
C---      contraintes membrane       
          SIGC = FC * LC(I) / DC
          SIGT = FT * LT(I) / DT
          SIGNXX(I) = SIGC * RFAC
          SIGNYY(I) = SIGT * RFAT
c
          UVAR(I,1)  = SIGC
          UVAR(I,2)  = SIGT
          UVAR(I,15) = DC
          UVAR(I,16) = DT
C---      contraintes cisaillement      
C------------------------------------------------------------------
C------------------------------------------------------------------
c++++++++++
c         G0 = GT / (ONE + TAN_LOCK*TAN_LOCK)
c         SIGG = G0*TAN_PHI - SIGG0
c         KG   = G0*TAN_PHI*TAN_PHI
c++++++++++
          SIG0      = UVAR(I,10)
          TAN_PHI(I)= DEPSXY(I)
          TAN2      = TAN_PHI(I)*TAN_PHI(I)
          IF (KFUNC(3) /= 0)THEN
            PHI = atan (TAN_PHI(I))*HUNDRED80/PI
            SIGNXY(I)  = YFAC(3)* FINTER(KFUNC(3),PHI,NPF,TF,GXY)
            KG         = GXY*YFAC(3)*TAN2
          ELSEIF (TAN_PHI(I) > TAN_LOCK) THEN
            SIGNXY(I) = GT*TAN_PHI(I) + GB - SIG0
            KG        = GT*TAN2
          ELSEIF (TAN_PHI(I) < -TAN_LOCK) THEN
            SIGNXY(I) = GT*TAN_PHI(I) - GB - SIG0
            KG        = GT*TAN2
          ELSE
            SIGNXY(I) = G0*TAN_PHI(I) - SIG0
            KG        = G0*TAN2
          ENDIF
C---      contraintes visc fibres   
          DTINV = TIMESTEP(I)/MAX(TIMESTEP(I)*TIMESTEP(I),EM20)
          DAMP  = SQRT(RHO0(I)*AREA(I)*THKLY(I)*HALF)
          V1 = VISCE*DAMP*SQRT(NC*KC)
          V2 = VISCE*DAMP*SQRT(NT*KT)
          SIGVXX(I) = DTINV*(DEPSXX(I))*V1
          SIGVYY(I) = DTINV*(DEPSYY(I))*V2
C---      contraintes frottement cisaillement  
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
          SINN = TAN_PHI(I) / SQRT(ONE + TAN2)
          KMAX = MAX(KC0*RFAC,KT0*RFAT)*(ONE+SINN) + KG
          LMIN = MIN(DC/DC0,DT/DT0)*UVAR(I,14)
c          if (RHO0(I)==ZERO.OR.LMIN==ZERO)
          SOUNDSP(I) = SQRT(KMAX/(RHO0(I)))*ALDT(I)/LMIN
          VISCMAX(I) = MAX(V1,V2)
          ETSE(I)    = ONE
        ENDDO
C---
#include "vectorize.inc"
        DO  J=1,NINDX
          I=INDEX(J)
          UVAR(I,3) = SIGNXY(I) + SIGVXY(I)
          TAN_PHI(I)= DEPSXY(I)
          UVAR(I,6) = DEPSXY(I)
          UVAR(I,7) = YC(I)
          UVAR(I,8) = YT(I)
          UVAR(I,9) = SIGVXY(I)
c
          SIGNYZ(I) = SIGOYZ(I) + GSH *DEPSYZ(I)
          SIGNZX(I) = SIGOZX(I) + GSH *DEPSZX(I)
        ENDDO
!=========================================================
!=========================================================
      ELSE  !  LOADING/UNLOADING WITH HYSTERESIS  (UNLOAD = 1)
!=========================================================
!=========================================================
       NITER  = 5
       EPSI1  = UPARAM(36)            
       SIGI1  = UPARAM(37)            
       EPSI2  = UPARAM(38)
       SIGI2  = UPARAM(39)
       PHII   = UPARAM(43)
       SXYI   = UPARAM(44)
       PHII2  = UPARAM(45)
       SXYI2  = UPARAM(46)
       IF (ISIGI == 0) THEN   
         IF (TIME == ZERO)THEN 
            DO I=1,NEL0
             UVAR(I,15)= DC0 
             UVAR(I,16)= DT0 
            ENDDO
          ENDIF
       ENDIF
       DO I=1,NEL0
         IF(OFFGG(I) > ZERO) THEN
           NINDX = NINDX+1
           INDEX(NINDX)=I
         ELSE
           NUMOFF = NUMOFF+1
           INDXOFF(NUMOFF)=I
         ENDIF
       ENDDO
#include "vectorize.inc"
       DO  J=1,NINDX
        I=INDEX(J)            
        YC(I)    = UVAR(I,7)  
        YT(I)    = UVAR(I,8)  
        TFOLD(I) = UVAR(I,9)  
       ENDDO
C---  integration des deformations
#include "vectorize.inc"
      DO  J=1,NINDX
        I=INDEX(J)
        ETC =UVAR(I,4)+DEPSXX(I)     
        ETT =UVAR(I,5)+DEPSYY(I)     
        UVAR(I,4) = ETC
        UVAR(I,5) = ETT
        EC(I) = EXP(ETC) - ONE ! eng strain
        ET(I) = EXP(ETT) - ONE
      ENDDO
#include "vectorize.inc"
      DO  J=1,NINDX
        I=INDEX(J)                                     
        LC(I) = ONE + EC(I) 
        LT(I) = ONE + ET(I)
        EPSMAXC(I) = UVAR(I,17)
        EMINRLC(I) = UVAR(I,18)
        EMAXRLC(I) = UVAR(I,19)
        SIGMAXC(I) = UVAR(I,20)
        SMINRLC(I) = UVAR(I,25)
        SMAXRLC(I) = UVAR(I,27)
        EPSMAXT(I) = UVAR(I,21)
        EMINRLT(I) = UVAR(I,22)
        EMAXRLT(I) = UVAR(I,23)
        SIGMAXT(I) = UVAR(I,24)
        SMINRLT(I) = UVAR(I,26)
        SMAXRLT(I) = UVAR(I,28)
      ENDDO
#include "vectorize.inc"
      DO  J=1,NINDX
        I=INDEX(J)
        FN(I) = ZERO
c---    uncoupled model  (compression of fiber)         
        HC  = HC0 + YC(I)  !longueur ressort           
        HT  = HT0 + YT(I)                              
        DC  = SQRT(LC(I)*LC(I) + HC*HC) ! long fibre   
        DT  = SQRT(LT(I)*LT(I) + HT*HT)                
        DCC = UVAR(I,15) - DC0                                 
        DTT = UVAR(I,16) - DT0 
        DDEC  = DC - UVAR(I,15)                        
        DDET  = DT - UVAR(I,16)                        
        IF (DCC >ZERO ) THEN   
         IF ((DDEC >=ZERO .AND. DCC> = UVAR(I,17)) .OR.
     .       (DDEC >=ZERO .AND. UVAR(I,17)==UVAR(I,18)) ) THEN     
           !----------------------LOAD
           DO ITER = 1, NITER           
            HC  = HC0 + YC(I)  !longueur ressort   
            DC  = SQRT(LC(I)*LC(I) + HC*HC) ! long fibre    
            DCC = DC - DC0 
            DCC = MAX (UVAR(I,18),DCC)           
            DC  = DCC +  DC0     
            UDC = ONE / DC                                         
            HDC = HC * UDC      !sinus(alpha)                                
            IF (KFUNC(1) == 0) THEN      
              FC  = KC0 * DCC            
              FPC = KC0 * HDC            
            ELSE                         
              FC  = YFAC(1) * FINTER(KFUNC(1),DCC,NPF,TF,FPC)
              FPC = FPC * YFAC(1) 
              KC  = FPC
              FPC = FPC  * HDC
            ENDIF
            EPSMAXC(I)  = DCC
            SIGMAXC(I)  = FC
            SMAXRLC(I)  = SIGMAXC(I)
            EMAXRLC(I)  = EPSMAXC(I)! UVAR(I,19))
            FUNC = KFC*YC(I) + FC*HC/DC
            DERIC= KFC + FPC*HDC + FC*UDC*(ONE - HDC*HDC)
            YC(I) = YC(I) - FUNC / DERIC
            EMINRLC(I) = DCC                                                           
            SMINRLC(I) = FC                                                           
           ENDDO !iter
           !TAGOLDC = 1
          ELSEIF(DDEC >=ZERO    ) THEN     
           !----------------------RELOAD
           DO ITER = 1, NITER
            HC  = HC0 + YC(I)  !longueur ressort   
            DC  = SQRT(LC(I)*LC(I) + HC*HC) ! long fibre    
            DCC = DC - DC0    
            DCC = MAX (UVAR(I,18),DCC)                             
            DC  = DCC +  DC0     
            UDC = ONE / DC                                         
            HDC = HC * UDC      !sinus(alpha)                                
            EPSNRLC(I) = UVAR(I,17)*(DCC - UVAR(I,18)) /(UVAR(I,17)-UVAR(I,18))
            IF (KFUNC(1) == 0) THEN      
              FC  = KC0 * DCC            
              FPC = KC0 * HDC            
            ELSE                         
              FC     = YFAC(1) * FINTER(KFUNC(1),EPSNRLC(I),NPF,TF,FPC)
              COEFRL = (UVAR(I,20) - UVAR(I,25))/UVAR(I,20)
              FC     =  UVAR(I,25) + COEFRL * FC
              FPC    = FPC * YFAC(1) 
              FPC    = FPC * UVAR(I,17)* (UVAR(I,20) - UVAR(I,25)) 
     .                     /(UVAR(I,17)-UVAR(I,18))/UVAR(I,20)
              KC  = FPC
              FPC = FPC * HDC 
            ENDIF                     
            EMAXRLC(I)= DCC   ! UVAR(I,19))            
            SMAXRLC(I) = FC                             
            FUNC = KFC*YC(I) + FC*HC/DC                  
            DERIC= KFC + FPC*HDC + FC*UDC*(ONE - HDC*HDC) 
            YC(I) = YC(I) - FUNC / DERIC        
           ENDDO !iter
           IF(DCC>UVAR(I,17))THEN!emax
            EPSMAXC(I)  = DCC
            SIGMAXC(I)   = FC              
           ENDIF                            
           !TAGOLDC = 2
          ELSE
c
           IF (UVAR(I,19) > ZERO) THEN  !emaxrl
           DO ITER = 1, NITER
            HC  = HC0 + YC(I)  !longueur ressort   
            DC  = SQRT(LC(I)*LC(I) + HC*HC) ! long fibre    
            DCC = DC - DC0                    
            DCC = MAX (ZERO,DCC)                  
            DC  = DCC +   DC0         
            UDC = ONE / DC                                         
            HDC = HC * UDC      !sinus(alpha)                               
            EPSNC(I) = DCC*EPSI1/UVAR(I,19)  
            IF (KFUNC(1) == 0) THEN      
              FC  = KC0 * DCC            
              FPC = KC0 * HDC            
            ELSE                         
              FC     = YFAC(4) * FINTER(KFUNC(4),EPSNC(I),NPF,TF,FPC)
              FPC    = FPC*YFAC(4)
              COEFUL = FC/SIGI1
              FC     = COEFUL *UVAR(I,27) !SMAXRLC(I)
              FPC    = FPC*UVAR(I,27)*EPSI1/UVAR(I,19)/SIGI1
              KC     = FPC
              FPC    = FPC * HDC
            ENDIF
            EMINRLC(I)= DCC 
            SMINRLC(I) = FC
            FUNC = KFC*YC(I) + FC*HC/DC
            DERIC= KFC + FPC*HDC + FC*UDC*(ONE - HDC*HDC)
            YC(I) = YC(I) - FUNC / DERIC
           ENDDO !iter
          ELSE!(UVAR(I,19) >ZERO
           DO ITER = 1, NITER
            HC  = HC0 + YC(I)  !longueur ressort
            DC  = SQRT(LC(I)*LC(I) + HC*HC) ! long fibre  
            UDC = ONE / DC                                        
            HDC = HC * UDC      !sinus(alpha)                                
            DCC = DC - DC0   
            FC  = KKC  * DCC        
            FPC = KKC  * HDC! derivee % yc
            KC  = KKC             
            FUNC = KFC*YC(I) + FC*HC/DC
            DERIC= KFC + FPC*HDC + FC*UDC*(ONE - HDC*HDC)
            YC(I) = YC(I) - FUNC / DERIC
            FN(I) = ZERO
           ENDDO !iter           
            EPSMAXC(I)  = ZERO
            SIGMAXC(I)   = ZERO
            SMAXRLC(I) = ZERO
            EMAXRLC(I)= ZERO
            EMINRLC(I)  = ZERO 
            SMINRLC(I)   = ZERO
          ENDIF!(UVAR(I,19) >ZERO)
          !TAGOLDC = 3
          ENDIF !LOAD RELOD OR UNLOAD
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
          EPSMAXC(I) = ZERO
          EMINRLC(I) = ZERO 
          EMAXRLC(I) = ZERO 
          SIGMAXC(I) = ZERO 
          SMINRLC(I) = ZERO 
          SMAXRLC(I) = ZERO 
         ENDIF ! COMPRESSION OR TENSION FOR DIRECTION CHAINE
c----------------------------------
c----------------direction trame---
c----------------------------------
         IF(DTT >ZERO ) THEN !(DDET >=ZERO .AND. UVAR(I,22)==ZERO).OR.
          IF((DDET >=ZERO .AND.DTT>UVAR(I,21))
     .          .OR.(DDET >=ZERO .AND.UVAR(I,21)==UVAR(I,22))) THEN     
           !----------------------LOAD
           DO ITER = 1, NITER           
            HT  = HT0 + YT(I)  !longueur ressort   
            DT  = SQRT(LT(I)*LT(I) + HT*HT) ! long fibre    
            DTT = DT - DT0     
            DTT = MAX (UVAR(I,22),DTT)     
            DT  = DTT +  DT0           
            UDT = ONE / DT                                         
            HDT = HT * UDT      !sinus(alpha)                               
            IF (KFUNC(2) == 0) THEN
              FT  = KT0 * DTT
              FPT = KT0 * HDT
            ELSE
              FT = YFAC(2)*FINTER(KFUNC(2),DTT,NPF,TF,FPT)
              FPT = FPT * YFAC(2)   
              KT  =  FPT         
              FPT = FPT *HDT
            ENDIF
            EPSMAXT(I) = DTT
            SIGMAXT(I) = FT
            SMAXRLT(I) = SIGMAXT(I)
            EMAXRLT(I)= EPSMAXT(I)! 
            EMINRLT(I)= ZERO 
            SMINRLT(I) = ZERO  
            FUNT = KFT*YT(I) + FT*HT/DT          
            DERIT= KFT + FPT*HDT + FT*UDT*(ONE - HDT*HDT)
            YT(I) = YT(I) - FUNT / DERIT
            EMINRLT(I)= DTT 
            SMINRLT(I) = FT  
           ENDDO !iter
           !TAGOLDT = 1
          ELSEIF(DDET >=ZERO) THEN     
           !----------------------RELOAD
           DO ITER = 1, NITER
            HT  = HT0 + YT(I)  !longueur ressort   
            DT  = SQRT(LT(I)*LT(I) + HT*HT) ! long fibre    
            DTT = DT - DT0                    
            DTT = MAX (UVAR(I,22),DTT)                  
            DT  = DTT +  DT0           
            UDT = ONE / DT                                         
            HDT = HT * UDT      !sinus(alpha)                               
            EPSNRLT(I) = UVAR(I,21)*(DTT - UVAR(I,22)) /(UVAR(I,21)-UVAR(I,22))                
            IF (KFUNC(2) == 0) THEN
              FT  = KT0 * DTT
              FPT = KT0 * HDT
            ELSE
              FT     = YFAC(2)*FINTER(KFUNC(2),EPSNRLT(I),NPF,TF,FPT)
              COEFRL =  (UVAR(I,24) - UVAR(I,26))/ UVAR(I,24)
              FT     = UVAR(I,26) + COEFRL * FT 
       
              FPT    = FPT * YFAC(2)   
              FPT    = FPT * UVAR(I,21) * (UVAR(I,24) - UVAR(I,26))
     .                    /(UVAR(I,21)-UVAR(I,22))/UVAR(I,24)
              KT  =  FPT         
              FPT = FPT * HDT
            ENDIF
            FUNT = KFT*YT(I) + FT*HT/DT          
            DERIT= KFT + FPT*HDT + FT*UDT*(ONE - HDT*HDT)
            YT(I) = YT(I) - FUNT / DERIT           
           ENDDO !iter
           EMAXRLT(I) = DTT
           SMAXRLT(I) = FT
           IF(DTT>UVAR(I,21))THEN
             EPSMAXT(I)  = DTT
             SIGMAXT(I)   = FT              
             !EMINRLT(I)= ZERO 
             !SMINRLT(I) = ZERO      
           ENDIF                            
           !TAGOLDT = 2
          ELSE !UNLOAd
            !-----------------------UNLOAD
           IF (UVAR(I,23)>ZERO)THEN
              DO ITER = 1, NITER
               HT  = HT0 + YT(I)  !longueur ressort   
               DT  = SQRT(LT(I)*LT(I) + HT*HT) ! long fibre    
               DTT = DT - DT0                    
               DTT = MAX (zero,DTT)                  
               DT  = DTT +  DT0           
               UDT = ONE / DT                                         
               HDT = HT * UDT      !sinus(alpha)                               
               EPSNT(I)   = EPSI2*DTT/UVAR(I,23)
               IF (KFUNC(2) == 0) THEN
                 FT  = KT0 * DTT
                 FPT = KT0 * HDT
               ELSE
                 FT         = YFAC(5)*FINTER(KFUNC(5),EPSNT(I),NPF,TF,FPT)
                 FPT        = FPT * YFAC(5)   
                 COEFUL    = FT /SIGI2
                 FT       = COEFUL * UVAR(I,28)
                 FPT      = FPT * EPSI2*UVAR(I,28)/UVAR(I,23)/SIGI2        
                 KT  =  FPT                                                          
                 FPT = FPT * HDT
               ENDIF
               EMINRLT(I)= DTT 
               SMINRLT(I) = FT  
               FUNT = KFT*YT(I) + FT*HT/DT          
               DERIT= KFT + FPT*HDT + FT*UDT*(ONE - HDT*HDT)
               YT(I) = YT(I) - FUNT / DERIT
              ENDDO !iter
           ELSE !(UVAR(I,23)>ZERO)
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
            EPSMAXT(I)  = ZERO
            SIGMAXT(I)   = ZERO
            SMAXRLT(I) = ZERO
            EMAXRLT(I)= ZERO
            EMINRLT(I)= ZERO 
            SMINRLT(I) = ZERO      
           ENDIF !(UVAR(I,23)>ZERO)
           !TAGOLDT = 3
          ENDIF !LOAD RELOD OR UNLOAD
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
          !TAGOLDT = 5
          EPSMAXT(I)   = ZERO 
          EMINRLT(I)   = ZERO 
          EMAXRLT(I) = ZERO   
          SIGMAXT(I)    = ZERO 
          SMINRLT(I)    = ZERO 
          SMAXRLT(I)  = ZERO 
         ENDIF ! COMPRESSION OR TENSION FOR DIRECTION CHAINE
         FN(I) = ZERO

C  ========================================================
C---    Coupled model (traction fibre)
C  ========================================================
        IF ((YC(I) + YT(I)) < ZERO) THEN !eps
          EPSMAXC(I)   = UVAR(I,17)
          EMINRLC(I)   = UVAR(I,18)
          EMAXRLC(I) = UVAR(I,19)
          SIGMAXC(I)    = UVAR(I,20)
          SMINRLC(I)    = UVAR(I,25)
          SMAXRLC(I)  = UVAR(I,27)
          EPSMAXT(I)   = UVAR(I,21)
          EMINRLT(I)   = UVAR(I,22)
          EMAXRLT(I) = UVAR(I,23)
          SIGMAXT(I)    = UVAR(I,24)
          SMINRLT(I)    = UVAR(I,26)
          SMAXRLT(I)  = UVAR(I,28)
          TAGOLDC      = NINT(UVAR(I,30))
          TAGOLDT      = NINT(UVAR(I,31))
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
          DCC0 = DC - DC0
          DTT0 = DT - DT0
          DCC1 = UVAR(I,15) - DC0
          DTT1 = UVAR(I,16) - DT0
          KF = KFC + KFT

          IDXCNT = 1                  
          IDX1 = MOD(IDXCNT,2)            
          IDX2 = MOD(IDXCNT+1,2)      
c---------------initialization                            
          IF(DCC1 >=ZERO  ) THEN   
c----------------------------------LOAD
            IF((DDEC >=ZERO .AND.DCC0>UVAR(I,17))
     .                                   .OR.(DDEC >=ZERO .AND.UVAR(I,17)==UVAR(I,18)) ) THEN 
              TAGC(IDX1) = 1                                                              
              TAGC(IDX2) = 1                                                              
              IFUNCC  = KFUNC(1)  
              FACC    = YFAC(1)   
              EMINC = ZERO                                    
              SMINC = ZERO    
              FACE = ONE
              FACF = ONE 
c----------------------------------RELOAD
            ELSEIF(DDEC >=ZERO)THEN !RELOAD
              TAGC(IDX1) = 2             
              TAGC(IDX2) = 2             
              IFUNCC= KFUNC(1)                                                   
              FACC  = YFAC(1)
              EMINC = UVAR(I,18)                                    
              SMINC = UVAR(I,25)                          
              FACE  = UVAR(I,17)/(UVAR(I,17)-UVAR(I,18))
              FACF  = (UVAR(I,20) - UVAR(I,25))/UVAR(I,20)                  
              IF (FACE <= ZERO) THEN
c                !print*,'ERROR1: CYCLE,ELEM,FACE=',NCYCLE,NGL(I),FACE 
c                !print*,'a   EMAXC,EMINRLC=',UVAR(I,17),UVAR(I,18)
                FACE = ABS(FACE) 
              ENDIF                
              IF (FACF <= ZERO) THEN
c                !print*,'ERROR1: CYCLE,ELEM,FACF=',NCYCLE,NGL(I),FACF 
c                !print*,'a   SMAXC,SMINRLC=',UVAR(I,20),UVAR(I,25)
                FACF = ABS(FACF) 
              ENDIF                
            ELSE !UNLOAd
c----------------------------------UNLOAD
              IF(UVAR(I,19) >ZERO)THEN
               TAGC(IDX1) = 3             
               TAGC(IDX2) = 3             
               IFUNCC= KFUNC(4)                                                   
               FACC  = YFAC(4)
               EMINC =  ZERO                 
               SMINC =  ZERO                                 
               FACE =  EPSI1 /UVAR(I,19)
               FACF = UVAR(I,27)/SIGI1                                         
              ELSE!UNLOAd UVAR(I,19) <=ZERO
                TAGC(IDX1) = 5            
                TAGC(IDX2) = 5            
              ENDIF!UNLOAd UVAR(I,19) <=ZERO
            ENDIF!UNLOAd RELOAD LOAD
           ELSE ! DOMAINE POSITIF
              TAGC(IDX1) = 5            
              TAGC(IDX2) = 5            
           ENDIF ! (DCC1 >=ZERO  )            
c ---------------direction trame ---------------------
           IF(DTT1 >=ZERO  ) THEN 
             IF((DDET >=ZERO .AND.DTT0>UVAR(I,21))
     .              .OR.(DDET >=ZERO .AND.UVAR(I,21)==UVAR(I,22))   ) THEN      
c----------------------------------LOAD
               TAGT(IDX1) = 1                                                             
               TAGT(IDX2) = 1                                                             
               IFUNCT  = KFUNC(2)           
               FACT    = YFAC(2)              
               EMINT = ZERO                                   
               SMINT = ZERO                 
               FATE = ONE                    
               FATF = ONE                    
             ELSEIF(DDET >=ZERO)THEN         
c--------------------------------RELOAD
               TAGT(IDX1) = 2             
               TAGT(IDX2) = 2                                
               IFUNCT= KFUNC(2)                                                     
               FACT  = YFAC(2)                               
               EMINT =  UVAR(I,22)                                     
               SMINT =  UVAR(I,26)                           
               FATE =  UVAR(I,21) /(UVAR(I,21)-UVAR(I,22))        
               FATF = (UVAR(I,24) - UVAR(I,26))/UVAR(I,24)                             
               IF (FATE <= ZERO) THEN
c                 !print*,'ERROR1: CYCLE,ELEM,FATE=',NCYCLE,NGL(I),FATE 
c                 !print*,'a   EMAXT,EMINRLT=',UVAR(I,21),UVAR(I,22)
                 FATE = ABS(FATE) 
               ENDIF                
               IF (FATF <= ZERO) THEN
c                 !print*,'ERROR1: CYCLE,ELEM,FATF=',NCYCLE,NGL(I),FATF 
c                 !print*,'a   SMAXT,SMINRLT=',UVAR(I,24),UVAR(I,26)
                 FATF = ABS(FATF) 
               ENDIF                
             ELSE ! 
c--------------------------------UNLOAD
               IF(UVAR(I,23) >ZERO)THEN
                TAGT(IDX1) = 3             
                TAGT(IDX2) = 3             
                IFUNCT  = KFUNC(5)  
                FACT    = YFAC(5)   
                EMINT =  ZERO                                 
                SMINT =  ZERO
                FATE =  EPSI2 /UVAR(I,23)
                FATF = UVAR(I,28)/SIGI2                            
               ELSE !unload (UVAR(I,23) >ZERO) 
                TAGT(IDX1) = 5            
                TAGT(IDX2) = 5            
               ENDIF
             ENDIF
           ELSE ! DOMAINE POSITIF 
            ! compression  fibre 
             TAGT(IDX1) = 5            
             TAGT(IDX2) = 5            
           ENDIF             
c------------------------------------------- 
          IERR = 1  
          DO WHILE (IERR == 1)        
c-------------------------------------------          
            DO ITER = 1, NITER
c---         chaine       
              HC = HC0 + Y                    
              HT = HT0 - Y                    
              DC = SQRT(LC(I)*LC(I) + HC*HC)  
              DT = SQRT(LT(I)*LT(I) + HT*HT)        
              DCC = DC - DC0                   
              DTT = DT - DT0     
c            
              SELECT CASE (TAGC(IDX1))
                CASE(1)  ! tension load or reload
                  DCC = MAX (UVAR(I,18),DCC)                  
                  DC  = DCC + DC0         
                  UDC = ONE / DC                                         
                  HDC = HC * UDC                       
                  EPSNRLC(I) = (DCC -EMINC) *FACE                                                 
                  IF (IFUNCC == 0) THEN
                    FC  = KC0 * DCC
                    FPC = KC0 * HDC
                  ELSE
                    FC  = FACC * FINTER(IFUNCC,EPSNRLC(I),NPF,TF,FPC)                
                    FC  = SMINC + FC *FACF                                                
                    FPC = FPC * FACC                                                   
                    FPC = FPC *FACF * FACE                                                 
                    KC  = FPC                                                                       
                    FPC = FPC * HDC                                                       
                  ENDIF                                     
                  IF ( ITER==NITER) THEN! DCC > UVAR(I,19) .AND.                               
                   EPSMAXC(I) = DCC
                   SIGMAXC(I) = FC
                   EMAXRLC(I) = DCC                                        
                   SMAXRLC(I) = FC
                   EMINRLC(I) = DCC                                                           
                   SMINRLC(I) = FC                                                           
                  ENDIF                                     
                  !IF (DCC > UVAR(I,17) .AND. ITER==NITER) THEN    
                  ! EPSMAXC(I) = DCC                                                              
                  ! SIGMAXC(I) = FC                                                               
                   !EMINRLC(I) = ZERO                                                               
                   !SMINRLC(I) = ZERO                                                               
                  !ENDIF                 
                CASE(2)  ! tension load or reload
                  DCC = MAX (UVAR(I,18),DCC)                  
                  DC  = DCC + DC0         
                  UDC = ONE / DC                                         
                  HDC = HC * UDC                       
                  EPSNRLC(I) = (DCC -EMINC) *FACE                                                 
                  IF (IFUNCC == 0) THEN
                    FC  = KC0 * DCC
                    FPC = KC0 * HDC
                  ELSE
                    FC  = FACC * FINTER(IFUNCC,EPSNRLC(I),NPF,TF,FPC)                
                    FC  = SMINC + FC *FACF                                                
                    FPC = FPC * FACC                                                   
                    FPC = FPC *FACF * FACE                                                 
                    KC  = FPC                                                                       
                    FPC = FPC * HDC                                                       
                  ENDIF                                     
                  IF ( ITER==NITER) THEN  ! DCC > UVAR(I,19) .AND.                             
                    EMAXRLC(I) = DCC                                        
                    SMAXRLC(I) = FC
                  ENDIF                                     
                  IF (DCC > UVAR(I,17) .AND. ITER==NITER) THEN    
                   EPSMAXC(I) = DCC                                                              
                   SIGMAXC(I) = FC                                                               
                   !EMINRLC(I) = ZERO                                                               
                   !SMINRLC(I) = ZERO                                                               
                  ENDIF                 
                CASE(3)  ! tension unload
                  DCC = MAX (ZERO,DCC)                  
                  DC  = DCC + DC0         
                  UDC = ONE / DC                                         
                  HDC = HC * UDC                       
                  EPSNRLC(I) = (DCC -EMINC) *FACE                                                 
                  IF (IFUNCC == 0) THEN
                    FC  = KC0 * DCC
                    FPC = KC0 * HDC
                  ELSE
                    FC  = FACC * FINTER(IFUNCC,EPSNRLC(I),NPF,TF,FPC)
                    FC  = SMINC + FC *FACF                                                
                    FPC = FPC * FACC                                                   
                    FPC = FPC *FACF * FACE                                                 
                    KC  = FPC                                                                       
                    FPC = FPC * HDC                                                       
                  ENDIF 
                  IF (DCC < UVAR(I,18)) THEN                                
                    EMINRLC(I) = DCC                                                           
                    SMINRLC(I) = FC                                                           
                  ENDIF 
                CASE(5)  ! compression
                  UDC= ONE / DC                         
                  HDC= HC * UDC                        
                  FC  = KKC  * DCC                     
                  FPC = KKC  * HDC! derivee % yc       
                  KC  = KKC                            
                  EPSMAXC(I) = ZERO                  
                  EMINRLC(I) = ZERO                  
                  EMAXRLC(I) = ZERO                  
                  SIGMAXC(I) = ZERO                  
                  SMINRLC(I) = ZERO                  
                  SMAXRLC(I) = ZERO                  
              END SELECT
c
c -----------------------direction trame 
              SELECT CASE (TAGT(IDX1))
                CASE(1)  ! tension load or reload
                  DTT = MAX (UVAR(I,22),DTT)                  
                  DT  = DTT +   DT0         
                  UDT = ONE / DT                                         
                  HDT = HT * UDT      !sinus(alpha)                               
                  EPSNRLT(I) = (DTT - EMINT)*FATE  
                  IF (IFUNCT == 0) THEN
                    FT  = KT0 * DTT   
                    FPT = KT0 * HDT 
                  ELSE
                    FT  = FACT*FINTER(IFUNCT,EPSNRLT(I),NPF,TF,FPT)
                    FT  = SMINT + FT * FATF       
                    FPT = FPT * FACT  
                    FPT = FPT *FATF * FATE
                    KT  = FPT         
                    FPT = FPT * HDT
                  ENDIF
                  IF (ITER==NITER) THEN   !DTT > UVAR(I,23) .AND.                  
                    EMAXRLT(I) = DTT             
                    SMAXRLT(I) = FT
                    EPSMAXT(I) = DTT
                    SIGMAXT(I) = FT              
                    EMINRLT(I) = DTT 
                    SMINRLT(I) = FT   
                  ENDIF                                    
                  !IF(DTT > UVAR(I,21).AND.ITER==NITER)THEN
                  !  EPSMAXT(I) = DTT
                  !  SIGMAXT(I) = FT              
                  !  EMINRLT(I) = ZERO 
                  !  SMINRLT(I) = ZERO      
                  !ENDIF           

                CASE(2)  ! tension load or reload
                  DTT = MAX (UVAR(I,22),DTT)                  
                  DT  = DTT +   DT0         
                  UDT = ONE / DT                                         
                  HDT = HT * UDT      !sinus(alpha)                               
                  EPSNRLT(I) = (DTT - EMINT)*FATE  
                  IF (IFUNCT == 0) THEN
                    FT  = KT0 * DTT   
                    FPT = KT0 * HDT 
                  ELSE
                    FT  = FACT*FINTER(IFUNCT,EPSNRLT(I),NPF,TF,FPT)
                    FT  = SMINT + FT * FATF       
                    FPT = FPT * FACT  
                    FPT = FPT *FATF * FATE
                    KT  = FPT         
                    FPT = FPT * HDT
                  ENDIF                                    
                  IF (ITER==NITER) THEN !DTT > UVAR(I,23) .AND.                    
                    EMAXRLT(I) = DTT             
                    SMAXRLT(I) = FT
                  ENDIF                                    
                  IF(DTT > UVAR(I,21).AND.ITER==NITER)THEN
                    EPSMAXT(I) = DTT
                    SIGMAXT(I) = FT              
                    !EMINRLT(I) = ZERO 
                    !SMINRLT(I) = ZERO      
                  ENDIF           
                 
                CASE(3)  ! tension unload
                  DTT = MAX (ZERO,DTT)                  
                  DT  = DTT +   DT0         
                  UDT = ONE / DT                                         
                  HDT = HT * UDT      !sinus(alpha)                               
                  EPSNRLT(I) = (DTT - EMINT)*FATE  
                  IF (IFUNCT == 0) THEN
                    FT  = KT0 * DTT   
                    FPT = KT0 * HDT 
                  ELSE
                    FT  = FACT*FINTER(IFUNCT,EPSNRLT(I),NPF,TF,FPT)
                    FT  = SMINT + FT * FATF       
                    FPT = FPT * FACT  
                    FPT = FPT *FATF * FATE
                    KT  = FPT         
                    FPT = FPT * HDT
                  ENDIF                                     
                  IF (DTT < UVAR(I,22)) THEN                
                    EMINRLT(I) = DTT                      
                    SMINRLT(I) = FT                      
                  ENDIF                                     
                CASE(5)  ! compression
                  UDT= ONE / DT                    
                  HDT= HT * UDT                   
                  FT  = KKT  * DTT        
                  FPT = KKT  * HDT! derivee % yT
                  KT  = KKT     
                  EPSMAXT(I) = ZERO 
                  EMINRLT(I) = ZERO 
                  EMAXRLT(I) = ZERO 
                  SIGMAXT(I) = ZERO 
                  SMINRLT(I) = ZERO 
                  SMAXRLT(I) = ZERO 
              END SELECT
c----
              FUNC  = KF*Y + FC * HDC - FT * HDT
              DERIC = KF + FPC*HDC + FC*UDC*(ONE - HDC*HDC) 
     .                     + FPT*HDT + FT*UDT*(ONE - HDT*HDT)
              Y = Y - FUNC / DERIC

              IF (Y > 0) THEN
                Y = MIN(Y, HT0)
              ELSE
                Y = MAX(Y,-HC0)
              ENDIF
                
            ENDDO !   ITER = 1, NITER              
            YC(I) = Y
            YT(I) =-Y
            FN(I) = FC*HC/DC + FT*HT/DT
c----------------------------------              
            HC = HC0 + YC(I)   
            HT = HT0 + YT(I)    
            DC = SQRT(LC(I)*LC(I) + HC*HC)
            DT = SQRT(LT(I)*LT(I) + HT*HT)
C------------------------------------------------------------------
c           POST testing
C------------------------------------------------------------------
            IERR  = 0
            IERRT = 0
            IERRC = 0
            IF (TAGC(IDX1) == 3 .and. ((EMINRLC(I) > UVAR(I,18) .and. TAGOLDC == 3)
     .                              .OR. EMINRLC(I) >UVAR(I,17) .OR. EMINRLC(I) >UVAR(I,19))) THEN
              IERR = 1
              IERRC= 1
              EMINRLC(I)= UVAR(I,18)
              SMINRLC(I) = UVAR(I,25)
              DC = UVAR(I,15)
              Y = HALF * (UVAR(I,7) - UVAR(I,8))
              IFUNCC = KFUNC(1)
              FACC   = YFAC(1)
              IF (UVAR(I,18)==ZERO .OR. UVAR(I,17)==UVAR(I,18))THEN 
               TAGC(IDX2) = 1
               EMINC = ZERO                                    
               SMINC = ZERO    
               FACE  = ONE
               FACF  = ONE                      
              ELSE
               TAGC(IDX2) = 2
               EMINC = UVAR(I,18)                                     
               SMINC = UVAR(I,25)                          
               FACE  = UVAR(I,17) /(UVAR(I,17)-UVAR(I,18))
               FACF  = (UVAR(I,20) - UVAR(I,25))/UVAR(I,20)   
               IF (FACE <= ZERO) THEN
c                 !print*,'ERROR: CYCLE,ELEM,FACE=',NCYCLE,NGL(I),FACE 
c                 !print*,'   EMAXC,EMINRLC=',UVAR(I,17),UVAR(I,18)
                 FACE = ABS(FACE) 
               ENDIF                
               IF (FACF <= ZERO) THEN
c                 !print*,'ERROR: CYCLE,ELEM,FACF=',NCYCLE,NGL(I),FACF 
c                     !print*,'   SMAXC,SMINRLC=',UVAR(I,20),UVAR(I,25)
                 FACF = ABS(FACF) 
               ENDIF                
              ENDIF 
            ELSEIF ((TAGC(IDX1) == 1.or.TAGC(IDX1) == 2) .and. (DCC < UVAR(I,19).and. TAGOLDC /= 3 )) THEN
              IERR = 1
              IERRC= 1
              EPSMAXC(I) = UVAR(I,17)
              EMINRLC(I) = UVAR(I,18)
              EMAXRLC(I) = UVAR(I,19)
              SIGMAXC(I) = UVAR(I,20)
              SMINRLC(I) = UVAR(I,25)
              SMAXRLC(I) = UVAR(I,27)
              DC         = UVAR(I,15)
              Y = HALF * (UVAR(I,7) - UVAR(I,8))
              TAGC(IDX2) = 3
              IFUNCC= KFUNC(4)                                                   
              FACC  = YFAC(4)
              EMINC = ZERO                 
              SMINC = ZERO                                 
              FACE  = EPSI1 /UVAR(I,19)
              FACF  = UVAR(I,27)/SIGI1                                         
            ENDIF !IF (TAGC(IDX1) 
C---------------------------TRAME------------------------------------
            IF (TAGT(IDX1) == 3 .and. ((EMINRLT(I) > UVAR(I,22) .and.TAGOLDT == 3)
     .         .OR. EMINRLT(I) >UVAR(I,21).OR. EMINRLT(I) >UVAR(I,23))) THEN
              IERR = 1
              IERRT= 1
              EMINRLT(I)= UVAR(I,22)
              SMINRLT(I) = UVAR(I,26)
              DT = UVAR(I,16)
              Y = HALF * (UVAR(I,7) - UVAR(I,8))
              IFUNCT = KFUNC(2)
              FACT   = YFAC(2)
              IF(UVAR(I,22)==ZERO.OR.UVAR(I,21)==UVAR(I,22))THEN 
               TAGT(IDX2) = 1
               EMINT = ZERO                                    
               SMINT = ZERO    
               FATE = ONE
               FATF = ONE                      
              ELSE
               TAGT(IDX2) = 2
               EMINT = UVAR(I,22)                                     
               SMINT = UVAR(I,26)                          
               FATE =  UVAR(I,21)/(UVAR(I,21)-UVAR(I,22))
               FATF = (UVAR(I,24) - UVAR(I,26))/UVAR(I,24)                              
                   IF (FATE <= ZERO) THEN
c                     !print*,'ERROR: CYCLE,ELEM,FATE=',NCYCLE,NGL(I),FATE 
c                     !print*,'   EMAXT,EMINRLT=',UVAR(I,21),UVAR(I,22)
                     FATE = ABS(FATE) 
                   ENDIF                
                   IF (FATF <= ZERO) THEN
c                     !print*,'ERROR: CYCLE,ELEM,FATF=',NCYCLE,NGL(I),FATF 
c                     !print*,'   SMAXT,SMINRLT=',UVAR(I,24),UVAR(I,26)
                     FATF = ABS(FATF) 
                   ENDIF                
              ENDIF 

            ELSEIF ((TAGT(IDX1) == 1.or.TAGT(IDX1) == 2).and.(DTT < UVAR(I,23).and. TAGOLDT /= 3 ))THEN
              IERR = 1
              IERRT= 1
              EPSMAXT(I) = UVAR(I,21)
              EMINRLT(I) = UVAR(I,22)
              EMAXRLT(I) = UVAR(I,23)
              SIGMAXT(I) = UVAR(I,24)
              SMINRLT(I) = UVAR(I,26)
              SMAXRLT(I) = UVAR(I,28)
              DT         = UVAR(I,16)
              Y = HALF * (UVAR(I,7) - UVAR(I,8))
              TAGT(IDX2) = 3
              IFUNCT= KFUNC(5)                                                   
              FACT  = YFAC(5)
              EMINT = ZERO                                 
              SMINT = ZERO
              FATE  =  EPSI2 /UVAR(I,23)
              FATF  = UVAR(I,28)/SIGI2                            
            ENDIF ! IF (TAGT(IDX1)
            IDXCNT = IDXCNT + 1

            IDX1 = MOD(IDXCNT,2) 
         
            IDX2 = MOD(IDXCNT+1,2)

            IF (IDXCNT > 3) THEN
              !!print*, 'IDXCNT > 3'
              IF (IERRC == 1)THEN
                EMINRLC(I) = UVAR(I,18)                                  
                SMINRLC(I) = UVAR(I,25)                                  
                EMAXRLC(I) = UVAR(I,19)                             
                SMAXRLC(I) = UVAR(I,27)
                EPSMAXC(I) = UVAR(I,17)                              
                SIGMAXC(I) = UVAR(I,20)                              
                IF (DCC > UVAR(I,19)) THEN                                  
                  EMAXRLC(I) = DCC                                          
                  SMAXRLC(I) = FC
                ENDIF                                       
                IF (DCC > UVAR(I,17)) THEN                                  
                  EPSMAXC(I) = DCC
                  SIGMAXC(I) = FC                                                               
                  EMINRLC(I) = ZERO                                                               
                  SMINRLC(I) = ZERO                                      
                ELSEIF (DCC < UVAR(I,18)) THEN                                  
                  EMINRLC(I) = DCC                                                             
                  SMINRLC(I) = FC                                                             
                ENDIF 
              ENDIF !IF (IERRC == 1) 
              IF (IERRT==1) THEN                            
                EMINRLT(I) = UVAR(I,22)                    
                SMINRLT(I) = UVAR(I,26)                    
                EPSMAXT(I) = UVAR(I,21)               
                SIGMAXT(I) = UVAR(I,24)                
                EPSMAXT(I) = UVAR(I,21)                 
                EMAXRLT(I) = DTT                           
                SMAXRLT(I) = FT                             
                IF (DTT > UVAR(I,23)) THEN                       
                  EMAXRLT(I) = DTT             
                  SMAXRLT(I) = FT
                ENDIF                                      
                IF (DTT > UVAR(I,21)) THEN                       
                  EPSMAXT(I) = DTT                          
                  SIGMAXT(I) = FT                           
                  EMINRLT(I) = ZERO                           
                  SMINRLT(I) = ZERO                           
                ELSEIF (DTT < UVAR(I,22)) THEN                
                  EMINRLT(I) = DTT                      
                  SMINRLT(I) = FT                      
                ENDIF                                     
              ENDIF !IF (IERRT==1)                                
              EXIT            
            ENDIF             
        
         ENDDO   !  WHILE (IERR == 1) 
         UVAR(I,30) = TAGC(IDX2)
         UVAR(I,31) = TAGT(IDX2)
c
        ENDIF   ! unload option
C=======================================================================
c       SHEAR
C=======================================================================
        TRACE = EXP(EPSXX(I) + EPSYY(I))! =exp(tr)
        EC2 = MAX(TRACE / (EC(I) + ONE), EM6)
        ET2 = MAX(TRACE / (ET(I) + ONE), EM6)
c
        RFAC = ONE / EC2
        RFAT = ONE / ET2
C---    contraintes membrane       
c
        SIGC = FC * LC(I) / DC
        SIGT = FT * LT(I) / DT
        SIGNXX(I) = SIGC * RFAC
        SIGNYY(I) = SIGT * RFAT
c
        UVAR(I,1)  = SIGC
        UVAR(I,2)  = SIGT
        UVAR(I,15) = DC
        UVAR(I,16) = DT
C------------------------------------------------------------------
C---    contraintes cisaillement      
C------------------------------------------------------------------
        SIG0    = UVAR(I,10)
        TAN_PHI(I)= DEPSXY(I)
        UVAR(I,6)= DEPSXY(I)
        TAN2    = TAN_PHI(I)*TAN_PHI(I)
        PHIO    = UVAR(I,29)
        PHI     = ATAN(TAN_PHI(I))*HUNDRED80/PI
        DPHI    = PHI - PHIO
        UVAR(I,29) =    PHI 

        PHIMIN(I)    = UVAR(I,32)      
        PHIMAX(I)    = UVAR(I,33)   
        PHIRLMAX(I)  = UVAR(I,34)
        SXYMAX(I)    = UVAR(I,35)      
        SXYMIN(I)    = UVAR(I,36)   
        SXYMAXRL(I)  = UVAR(I,37)      

        IF (KFUNC(3) == 0) THEN ! analytic shear without UNLOAD
          IF (TAN_PHI(I) > TAN_LOCK) THEN
          SIGNXY(I) = GT*TAN_PHI(I) + GB - SIG0
            KG        = GT*TAN2
          ELSEIF (TAN_PHI(I) < -TAN_LOCK) THEN
            SIGNXY(I) = GT*TAN_PHI(I) - GB - SIG0
            KG        = GT*TAN2
          ELSE
            SIGNXY(I) = G0*TAN_PHI(I) - SIG0
            KG        = G0*TAN2
          ENDIF
c-----        
        ELSE  ! KFUNC(3) > 0  => load/UNLOAD
c-----
          IF (PHI >=ZERO)THEN
            IF ((DPHI >=ZERO .AND. UVAR(I,32) ==ZERO).OR.(DPHI >=ZERO .AND. 
     .           PHI >UVAR(I,33)).OR.(DPHI >=ZERO .AND. UVAR(I,33)==UVAR(I,32)) .OR.
     .          (DPHI >=ZERO .AND. UVAR(I,35)==ZERO)) THEN
             !----------------------LOAD
              SIGNXY(I)   = YFAC(3)*FINTER(KFUNC(3),PHI,NPF,TF,GXY)
              KG          = GXY*YFAC(3)*TAN2
              PHIMAX(I)      = PHI
              PHIRLMAX(I)    = PHI
              SXYMAX(I)      = SIGNXY(I)
              SXYMAXRL(I)    = SIGNXY(I)  
            ELSEIF(DPHI>=ZERO)THEN 
             !----------------------RELOAD
             PHIN      = UVAR(I,33)*(PHI - UVAR(I,32))/(UVAR(I,33) - UVAR(I,32))
             SIGNXY(I)  = YFAC(3)*FINTER(KFUNC(3),PHIN,NPF,TF,GXY)
             SIGNXY(I)  = UVAR(I,36) + SIGNXY(I) *(UVAR(I,35) - UVAR(I,36))/UVAR(I,35)
             KG          = GXY*YFAC(3)*TAN2
             PHIRLMAX(I)    = PHI
             SXYMAXRL(I)    = SIGNXY(I)  
             IF (PHI >UVAR(I,33))THEN
              PHIMAX(I)      = PHI
              SXYMAX(I)      = SIGNXY(I)
             ENDIF
            ELSE
             !----------------------UNLOAD
             PHIN      = PHI * PHII/UVAR(I,34)
             SIGNXY(I) = YFAC(6)*FINTER(KFUNC(6),PHIN,NPF,TF,GXY)
             SIGNXY(I) = SIGNXY(I)*UVAR(I,37) / SXYI 
             KG        = GXY*YFAC(6)*TAN2
             PHIMIN(I)    = PHI     
             SXYMIN(I)    = SIGNXY(I)         
            ENDIF!DPHI LOAD - RELOAD - UNLOAD
          ELSE ! PHI <ZERO 
            IF((DPHI <=ZERO .AND. UVAR(I,32) ==ZERO).OR.(DPHI <=ZERO .AND. PHI <UVAR(I,33))
     .     .OR.(DPHI <=ZERO .AND. UVAR(I,33)==UVAR(I,32)) ) THEN
             !----------------------LOAD
              SIGNXY(I)   = YFAC(3)*FINTER(KFUNC(3),PHI,NPF,TF,GXY)
              KG          = GXY*YFAC(3)*TAN2
              PHIMAX(I)      = PHI
              PHIRLMAX(I)    = PHI
              SXYMAX(I)      = SIGNXY(I)
              SXYMAXRL(I)    = SIGNXY(I)  
            ELSEIF(DPHI<=ZERO)THEN 
             !----------------------RELOAD
             PHIN      = UVAR(I,33)*(PHI - UVAR(I,32))/(UVAR(I,33) - UVAR(I,32))
             SIGNXY(I)  = YFAC(3)*FINTER(KFUNC(3),PHIN,NPF,TF,GXY)
             SIGNXY(I)  = UVAR(I,36) + SIGNXY(I) *(UVAR(I,35) - UVAR(I,36))/UVAR(I,35)
             KG          = GXY*YFAC(3)*TAN2
             PHIRLMAX(I)    = PHI
             SXYMAXRL(I)    = SIGNXY(I)  
             IF (PHI <UVAR(I,33))THEN
              PHIMAX(I)      = PHI
              SXYMAX(I)      = SIGNXY(I)
             ENDIF
            ELSE
             !----------------------UNLOAD
             PHIN      = PHI * PHII2/UVAR(I,34)
             SIGNXY(I) = YFAC(6)*FINTER(KFUNC(6),PHIN,NPF,TF,GXY)
             SIGNXY(I) = SIGNXY(I)*UVAR(I,37) / SXYI2 
             KG        = GXY*YFAC(6)*TAN2
             PHIMIN(I)    = PHI     
             SXYMIN(I)    = SIGNXY(I)                   
            ENDIF!DPHI LOAD - RELOAD - UNLOAD
          ENDIF
c
        ENDIF ! KFUNC(3) > 0
c---------------------------------------------------------------------
C---    contraintes visc fibres   
c
        DTINV = TIMESTEP(I)/MAX(TIMESTEP(I)*TIMESTEP(I),EM20)
        DAMP  = SQRT(RHO0(I)*AREA(I)*THKLY(I)*HALF)        
        IF (KC < ZERO) THEN
          KC = ONE
        ENDIF
        IF (KT < ZERO) THEN
          KT = ONE
        ENDIF
        V1 = VISCE*DAMP*SQRT(KC)
        V2 = VISCE*DAMP*SQRT(KT)
        SIGVXX(I) = DTINV*(DEPSXX(I))*V1
        SIGVYY(I) = DTINV*(DEPSYY(I))*V2
C---    contraintes frottement cisaillement  
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
        SINN = TAN_PHI(I) / SQRT(ONE + TAN2)
        KMAX = MAX(KC0*RFAC,KT0*RFAT)*(ONE+SINN) + KG
        LMIN = MIN(DC/DC0,DT/DT0)*UVAR(I,14)
        SOUNDSP(I) = SQRT(KMAX/(RHO0(I)))*ALDT(I)/LMIN
        VISCMAX(I) = MAX(V1,V2)
        ETSE(I)    = ONE
      ENDDO
C---
#include "vectorize.inc"
      DO  J=1,NINDX
        I=INDEX(J)
        UVAR(I,3) = SIGNXY(I) + SIGVXY(I)
        TAN_PHI(I) = DEPSXY(I)
        UVAR(I,6)  = DEPSXY(I)
        UVAR(I,7) = YC(I)
        UVAR(I,8) = YT(I)
        UVAR(I,9) = SIGVXY(I)
c
        UVAR(I,17) = EPSMAXC(I)   
        UVAR(I,18) = EMINRLC(I)   
        UVAR(I,19) = EMAXRLC(I)  
        UVAR(I,20) = SIGMAXC(I)    
        UVAR(I,21) = EPSMAXT(I)   
        UVAR(I,22) = EMINRLT(I)   
        UVAR(I,23) = EMAXRLT(I) 
        UVAR(I,24) = SIGMAXT(I)  
        UVAR(I,25) = SMINRLC(I)
        UVAR(I,26) = SMINRLT(I)
        UVAR(I,27) = SMAXRLC(I) 
        UVAR(I,28) = SMAXRLT(I)
        UVAR(I,32) = PHIMIN(I)         
        UVAR(I,33) = PHIMAX(I)      
        UVAR(I,34) = PHIRLMAX(I)  
        UVAR(I,35) = SXYMAX(I)         
        UVAR(I,36) = SXYMIN(I)      
        UVAR(I,37) = SXYMAXRL(I)       
c
        SIGNYZ(I) = SIGOYZ(I) + GSH *DEPSYZ(I)
        SIGNZX(I) = SIGOZX(I) + GSH *DEPSZX(I)
      ENDDO
     
      ENDIF ! UNLOAD
C-----------------------------------------------------------
C     REF-STATE ZEROSTRESS OPTION
C-----------------------------------------------------------
      IF (ZEROSTRESS /= ZERO)THEN
        IF (TT <= TSTART) THEN
#include "vectorize.inc"
         DO  J=1,NINDX
            I=INDEX(J)
            UVAR(I,11) = SIGNXX(I)
            UVAR(I,12) = SIGNYY(I)
            UVAR(I,13) = SIGNXY(I)
            SIGNXX(I)  = ZERO
            SIGNYY(I)  = ZERO
            SIGNXY(I)  = ZERO
          ENDDO
        ELSE
#include "vectorize.inc"
         DO  J=1,NINDX
            I=INDEX(J)
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
      DO J=1,NUMOFF
        I = INDXOFF(J)
        KMAX = MAX(KC0,KT0)*TWO
        SOUNDSP(I) = SQRT(KMAX/(RHO0(I)))
      ENDDO
C-----------------------------------------------------------
C     DESACTIVATION SI SURFACE < SURFACE MIN
C-----------------------------------------------------------
      IF (AREAMIN > ZERO) THEN
#include "vectorize.inc"
       DO  J=1,NINDX
          I=INDEX(J)
          AA = (ONE+EC(I)+ET(I) - AREAMIN) * DAREAMIN
          AA = MIN(MAX(AA,ZERO),ONE)
          SIGNXX(I) = SIGNXX(I)*AA
          SIGNYY(I) = SIGNYY(I)*AA
          SIGNXY(I) = SIGNXY(I)*AA
          SIGNYZ(I) = SIGNYZ(I)*AA
          SIGNZX(I) = SIGNZX(I)*AA
        ENDDO
      ENDIF
      !IF(ncycle== 2)!stop
C-----------------------------------------------------------
C     DESACTIVATION FOR VERY WARPED ELEMENT (QBAT)
C-----------------------------------------------------------
      IF (NPT0 ==1) THEN
#include "vectorize.inc"
        DO  J=1,NINDX
          I=INDEX(J)
          IF (UVAR(I,40)==ONE) CYCLE
          AA =UVAR(I,40)
          SIGNXX(I) = SIGNXX(I)*AA
          SIGNYY(I) = SIGNYY(I)*AA
          SIGNXY(I) = SIGNXY(I)*AA
          IF (UVAR(I,40)<EM02) THEN
           KMAX = MAX(KC0,KT0)*TWO
           SOUNDSP(I) = SQRT(KMAX/(RHO0(I)))
          END IF
        ENDDO
      ENDIF
C-----------
      RETURN
      END SUBROUTINE SIGEPS58C
