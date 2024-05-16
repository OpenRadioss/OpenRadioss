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
Chd|  SIGEPS83                      source/materials/mat/mat083/sigeps83.F
Chd|-- called by -----------
Chd|        SUSER43                       source/elements/solid/sconnect/suser43.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE SIGEPS83(
     1     NEL     ,TIME    ,TIMESTEP,UPARAM  ,OFF     ,
     2     EPSD    ,STIFM   ,IFUNC   ,MAXFUNC ,NPF     ,TF      ,
     3     AREA    ,DEPSZZ  ,DEPSYZ  ,DEPSZX  ,NUPARAM ,EPSZZ   ,
     4     SIGOZZ  ,SIGOYZ  ,SIGOZX  ,SIGNZZ  ,SIGNYZ  ,SIGNZX  ,
     5     PLA     ,JSMS    ,DMELS   ,SYM,     UVAR    ,NUVAR   ,
     6     DMG     ,ASRATE  )    
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C JSMS    |  1      | I | R | 0/1 (=1 IF /DT/AMS APPLIES TO THIS ELEMENT GROUP)
C---------+---------+---+---+--------------------------------------------
C MFUNC   |  1      | I | R | NUMBER FUNCTION USED FOR THIS USER LAW not used
C KFUNC   | NFUNC   | I | R | FUNCTION INDEX not used
C NPF     |  *      | I | R | FUNCTION ARRAY   
C TF      |  *      | F | R | FUNCTION ARRAY 
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C ...     |         |   |   |
C DEPSXX  | NEL     | F | R | STRAIN INCREMENT XX
C DEPSYY  | NEL     | F | R | STRAIN INCREMENT YY
C ...     |         |   |   |
C SIGOXX  | NEL     | F | R | OLD ELASTO PLASTIC STRESS XX 
C SIGOYY  | NEL     | F | R | OLD ELASTO PLASTIC STRESS YY
C ...     |         |   |   |    
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C DMELS   | NEL     | F | W | NON DIAGONAL TERM FOR AMS
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
#include      "sms_c.inc"
C----------------------------------------------------------
C   D u m m y   A r g u m e n t s  
C----------------------------------------------------------
      INTEGER NEL,NUPARAM,NUVAR,MAXFUNC,JSMS
      INTEGER IFUNC(*),NPF(*)
      my_real
     .   TIME,TIMESTEP,ASRATE
      my_real
     .   UPARAM(NUPARAM),OFF(NEL),TF(*),PLA(NEL),AREA(NEL),
     .   EPSD(NEL),DEPSZZ(NEL),DEPSYZ(NEL),DEPSZX(NEL),
     .   SIGOZZ(NEL),SIGOYZ(NEL),SIGOZX(NEL),EPSZZ(NEL),
     .   STIFM(NEL),SIGNZZ(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .   DMELS(*),UVAR(NEL,NUVAR), SYM(NEL),DMG(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: II,IEL,ITER,IRATE,NRATE,IDYIELD,IPLAS,IFUNN,IFUNT,
     .           ICOMP,NTRAC,NCOMP,VP
      INTEGER ,DIMENSION(NEL) :: ELTRAC,ELCOMP
      my_real :: YOUNGT,YOUNGC,SHEAR,DYDX,
     .   SVMN,SVMT,DTB,ALPHA,BETA,SVM,TS,TN,XSCALE,RNC,RSC,XFAC,YFAC,
     .   AA,AN,AS,NORMEF,NZZ,NYZ,NZX,FACN,FACT,SZZ,SYZ,SZX,
     .   FPX,FPY,FPRIM,SIGN,SIGT,SIG_EFF,PHI,DTINV,DSZZ,DSYZ,DSZX,DYLD,YLD0
      my_real ,DIMENSION(NEL) :: DSIGX,DSIGY,DSIGZ,DEP,DEPST,STF,EPSP,HT, 
     .   FYIELD,HYIELD, RN,HN,RS,SIGTRZZ,SIGTRYZ,SIGTRZX,YOUNG
C----------------------------------------------------------
C   E x t e r n a l  F u n c t i o n
C----------------------------------------------------------
       my_real :: FINTER
c-----------------------------------------------------
c     UVAR(1) = strain rate
C=======================================================================
C     INPUT PARAMETERS INITIALIZATION
C-----------------------------------------------
      IFUNN   = IFUNC(1)
      IFUNT   = IFUNC(2)
      IDYIELD = IFUNC(3)
      YOUNGT  = UPARAM(1) 
      ALPHA   = UPARAM(2)
      BETA    = UPARAM(3)
      YFAC    = UPARAM(4)
      XSCALE  = UPARAM(5) 
      RNC     = UPARAM(6)
      RSC     = UPARAM(7)
      XFAC    = UPARAM(8)
      IPLAS   = UPARAM(10)
      SHEAR   = UPARAM(11)
      ICOMP   = NINT(UPARAM(12))
      YOUNGC  = UPARAM(13)
      VP      = NINT(UPARAM(14))
      DTINV   = ONE / MAX(EM20,TIMESTEP)
c
      IF (VP == 0) THEN              ! total displacement rate
        EPSP(1:NEL)   = SQRT(DEPSZZ(1:NEL)**2 + DEPSYZ(1:NEL)**2 + DEPSZX(1:NEL)**2) *DTINV                                                      
        UVAR(1:NEL,1) = ASRATE*EPSP(1:NEL) + (ONE - ASRATE)*UVAR(:NEL,1)
        EPSD(1:NEL)   = UVAR(1:NEL,1)
      ENDIF
      EPSP(:NEL) = UVAR(:NEL,1)
      DEP(:NEL)  = ZERO
c----------------------------------
      NTRAC = 0
      NCOMP = 0
      IF (ICOMP == 0) THEN            ! elasto-plastic in tension & compression
        IF (YOUNGC == YOUNGT) THEN    ! common Young modulus
          YOUNG(:NEL)  = YOUNGT
        ELSE
          DO IEL=1,NEL
            IF (SIGOZZ(IEL) > ZERO) THEN   ! element in tension
              YOUNG(IEL)  = YOUNGT
            ELSE                           ! element in compression
              YOUNG(IEL)  = YOUNGC
            ENDIF
          ENDDO
        END IF
      ELSE       ! ICOMP = 1  (linear in compression, elasto-plastic in tension)
        ELTRAC(1:NEL) = 0
        ELCOMP(1:NEL) = 0
        DO IEL=1,NEL
          IF (SIGOZZ(IEL) > ZERO) THEN   ! element in tension
            YOUNG(IEL)  = YOUNGT
            NTRAC = NTRAC + 1
            ELTRAC(NTRAC) = IEL
          ELSE                          ! element in compression
            YOUNG(IEL)  = YOUNGC
            NCOMP = NCOMP + 1
            ELCOMP(NCOMP) = IEL
          ENDIF
        ENDDO
      ENDIF
c
      STF(1:NEL)     = YOUNG(1:NEL) * AREA(1:NEL)                                            
      DSIGZ(1:NEL)   = YOUNG(1:NEL) * DEPSZZ(1:NEL)
      DSIGY(1:NEL)   = SHEAR*DEPSYZ(1:NEL)                              
      DSIGX(1:NEL)   = SHEAR*DEPSZX(1:NEL)                              
      SIGNZZ(1:NEL)  = SIGOZZ(1:NEL) + DSIGZ(1:NEL) * OFF(1:NEL)             
      SIGNYZ(1:NEL)  = SIGOYZ(1:NEL) + DSIGY(1:NEL) * OFF(1:NEL)              
      SIGNZX(1:NEL)  = SIGOZX(1:NEL) + DSIGX(1:NEL) * OFF(1:NEL)              
      STIFM(1:NEL)   = STIFM(1:NEL)  + STF(1:NEL)*OFF(1:NEL)                                           
      SIGTRZZ(1:NEL) = SIGNZZ(1:NEL)                   
      SIGTRYZ(1:NEL) = SIGNYZ(1:NEL)                     
      SIGTRZX(1:NEL) = SIGNZX(1:NEL)            
c-----------------------------------------------------      
c omega = sqrt(2k/2*dmels), dt=2/omega, 2*dmels=dt**2 * 2k / 4
      IF (IDTMINS==2 .AND. JSMS/=0) THEN
        DTB = (DTMINS/DTFACS)**2
        DO IEL=1,NEL                                                 
          DMELS(IEL)=MAX(DMELS(IEL),HALF*DTB*STF(IEL)*OFF(IEL))
        ENDDO                                                        
      END IF
c          
c----------------------Interpolation
c
      IF (IFUNN /= 0) THEN
        DO IEL=1,NEL               
          RN(IEL) = FINTER(IFUNN,EPSP(IEL)*XSCALE,NPF,TF,DYDX) * RNC  !/ AREA(IEL)
        ENDDO
      ELSE
        DO IEL=1,NEL               
          RN(IEL) = RNC  !/ AREA(IEL)
        ENDDO
      ENDIF
 
      IF (IFUNT /=0)THEN
        DO IEL=1,NEL       
          RS(IEL) = FINTER(IFUNT,EPSP(IEL)*XSCALE,NPF,TF,DYDX) * RSC  !/ AREA(IEL)
        ENDDO
      ELSE
        DO IEL=1,NEL               
          RS(IEL) = RSC   !  / AREA(IEL)
        ENDDO
      ENDIF
      DO IEL=1,NEL          
        FYIELD(IEL) = MAX(ZERO, FINTER(IDYIELD,PLA(IEL)*XFAC,NPF,TF,DYDX))
        FYIELD(IEL) = FYIELD(IEL)*(ONE-DMG(IEL))*YFAC
        HYIELD(IEL) = DYDX*YFAC
      ENDDO
c-----------------------------------------------------------------------------
c     Plasticity projection (radial return from sigma trial) 
c-----------------------------------------------------------------------------
      IF (BETA == TWO) THEN
        IF (ICOMP == 0) THEN    ! elasto-plastic in tension and compression
          DO IEL = 1,NEL
            AA = RN(IEL)*(ONE-ALPHA*SIN(SYM(IEL)))
            AN = ONE/MAX(EM20,AA)**2
            AS = ONE/MAX(EM20,RS(IEL))**2
            SIG_EFF = AN * SIGTRZZ(IEL)** 2 + AS * (SIGTRYZ(IEL)**2 + SIGTRZX(IEL)**2)
            YLD0 = FYIELD(IEL)
            PHI  = SIG_EFF - YLD0**2
            IF (SIG_EFF > ZERO .and. PHI > ZERO) THEN ! plastic
              NORMEF = SQRT (SIGTRZZ(IEL)**2 + SIGTRYZ(IEL)**2 + SIGTRZX(IEL)**2)
              NZZ  = SIGTRZZ(IEL) / NORMEF
              NYZ  = SIGTRYZ(IEL) / NORMEF
              NZX  = SIGTRZX(IEL) / NORMEF
              FACN = TWO*AN*YOUNG(IEL)
              FACT = TWO*AS*SHEAR
              DO ITER= 1,3
                FPRIM = -FACN*SIGNZZ(IEL)*NZZ - FACT*(SIGNYZ(IEL)*NYZ + SIGNZX(IEL)*NZX)
                FPRIM = FPRIM -TWO*FYIELD(IEL)
                              
                DSZZ = -FACN*SIGNZZ(IEL)*NZZ
                DSYZ = -FACT*SIGNYZ(IEL)*NYZ
                DSZX = -FACT*SIGNZX(IEL)*NZX
                DYLD = -TWO*FYIELD(IEL)*HYIELD(IEL)
                FPRIM = DSZZ + DSYZ + DSZX + DYLD

                DEP(IEL) = DEP(IEL) - PHI / FPRIM

                SIGNZZ(IEL) = SIGTRZZ(IEL) - YOUNG(IEL)*DEP(IEL) * NZZ
                SIGNYZ(IEL) = SIGTRYZ(IEL) - SHEAR*DEP(IEL) * NYZ
                SIGNZX(IEL) = SIGTRZX(IEL) - SHEAR*DEP(IEL) * NZX

                FYIELD(IEL) = YLD0 + HYIELD(IEL)*DEP(IEL)
                SIG_EFF = AN * SIGNZZ(IEL)** 2 + AS * (SIGNYZ(IEL)**2 + SIGNZX(IEL)**2)
                PHI     = SIG_EFF - FYIELD(IEL)**2    ! yield criterion function
              ENDDO  
              DEP(IEL) = MAX(ZERO, DEP(IEL))*OFF(IEL)
              PLA(IEL) = PLA(IEL) + DEP(IEL)
            ENDIF 
          ENDDO
c--------------
        ELSE  !  ICOMP = 1 (linear in compression, elasto-plastic in tension)
c--------------
          DO II = 1,NTRAC                                                    
            IEL= ELTRAC(II)                                                   
            AA = RN(IEL)*(ONE-ALPHA*SIN(SYM(IEL)))
            AN = ONE/MAX(EM20,AA)**2
            AS = ONE/MAX(EM20,RS(IEL))**2
            SIG_EFF = AN * SIGTRZZ(IEL)** 2 + AS * (SIGTRYZ(IEL)**2 + SIGTRZX(IEL)**2)
            YLD0 = FYIELD(IEL)
            PHI  = SIG_EFF - YLD0**2
            IF (SIG_EFF > ZERO .and. PHI > ZERO) THEN ! plastic
              NORMEF = SQRT (SIGTRZZ(IEL)**2 + SIGTRYZ(IEL)**2 + SIGTRZX(IEL)**2)
              NZZ  = SIGTRZZ(IEL) / NORMEF
              NYZ  = SIGTRYZ(IEL) / NORMEF
              NZX  = SIGTRZX(IEL) / NORMEF
              FACN = TWO*AN*YOUNG(IEL)
              FACT = TWO*AS*SHEAR
              DO ITER= 1,3
                DSZZ = -FACN*SIGNZZ(IEL)*NZZ
                DSYZ = -FACT*SIGNYZ(IEL)*NYZ
                DSZX = -FACT*SIGNZX(IEL)*NZX
                DYLD = -TWO*FYIELD(IEL)*HYIELD(IEL)
                FPRIM = DSZZ + DSYZ + DSZX + DYLD

                DEP(IEL) = DEP(IEL) - PHI / FPRIM

                SIGNZZ(IEL) = SIGTRZZ(IEL) - YOUNG(IEL)*DEP(IEL) * NZZ
                SIGNYZ(IEL) = SIGTRYZ(IEL) - SHEAR*DEP(IEL) * NYZ
                SIGNZX(IEL) = SIGTRZX(IEL) - SHEAR*DEP(IEL) * NZX

                FYIELD(IEL) = YLD0 + HYIELD(IEL)*DEP(IEL)
                SIG_EFF = AN * SIGNZZ(IEL)** 2 + AS * (SIGNYZ(IEL)**2 + SIGNZX(IEL)**2)
                PHI     = SIG_EFF - FYIELD(IEL)**2    ! yield criterion function
              ENDDO  
              DEP(IEL) = MAX(ZERO, DEP(IEL))*OFF(IEL)
              PLA(IEL) = PLA(IEL) + DEP(IEL)
            ENDIF 
          ENDDO
c
          DO II = 1,NCOMP                                                  
            IEL= ELCOMP(II)                                                   
            AS = ONE/MAX(EM20,RS(IEL))**2
            SIG_EFF = AS * (SIGTRYZ(IEL)**2 + SIGTRZX(IEL)**2)
            YLD0 = FYIELD(IEL)
            PHI  = SIG_EFF - YLD0**2
            IF (SIG_EFF > ZERO .and. PHI > ZERO) THEN ! plastic
              NORMEF = SQRT (SIGTRYZ(IEL)**2 + SIGTRZX(IEL)**2)
              NYZ  = SIGTRYZ(IEL) / NORMEF
              NZX  = SIGTRZX(IEL) / NORMEF
              FACT = TWO*AS*SHEAR
              DO ITER= 1,3
                DSYZ = -FACT*SIGNYZ(IEL)*NYZ
                DSZX = -FACT*SIGNZX(IEL)*NZX
                DYLD = -TWO*FYIELD(IEL)*HYIELD(IEL)
                FPRIM = DSYZ + DSZX + DYLD

                DEP(IEL) = DEP(IEL) - PHI / FPRIM
                SIGNYZ(IEL) = SIGTRYZ(IEL) - SHEAR*DEP(IEL) * NYZ
                SIGNZX(IEL) = SIGTRZX(IEL) - SHEAR*DEP(IEL) * NZX

                FYIELD(IEL) = YLD0 + HYIELD(IEL)*DEP(IEL)
                SIG_EFF = AS * (SIGNYZ(IEL)**2 + SIGNZX(IEL)**2)
                PHI     = SIG_EFF - FYIELD(IEL)**2    ! yield criterion function
              ENDDO  
              DEP(IEL) = MAX(ZERO, DEP(IEL))*OFF(IEL)
              PLA(IEL) = PLA(IEL) + DEP(IEL)
            ENDIF 
          ENDDO
c
        END IF
c------------------------
      ELSE   ! BETA /= 2                                                                
c------------------------
        IF (ICOMP == 0) THEN    ! elasto-plastic in tension and compression
          DO IEL=1,NEL                                                      
            AA = RN(IEL)*(ONE-ALPHA*SIN(SYM(IEL)))
            AN = ONE/MAX(EM20,AA)**BETA
            AS = ONE/MAX(EM20,RS(IEL))**BETA
            SVMN = ABS(SIGNZZ(IEL))                      ! plast - normal stress
            SVMT = SQRT(SIGNYZ(IEL)**2 + SIGNZX(IEL)**2) ! plast - shear stress 
            SIG_EFF = AN * SVMN**BETA + AS * SVMT**BETA 
            YLD0 = FYIELD(IEL)
            PHI  = SIG_EFF - YLD0**BETA
            IF (SIG_EFF > ZERO .and. PHI > ZERO) THEN ! plastic
              NORMEF = SQRT (SIGTRZZ(IEL)**2+SIGTRYZ(IEL)**2 + SIGTRZX(IEL)**2)
              NZZ  = SIGTRZZ(IEL) / NORMEF
              NYZ  = SIGTRYZ(IEL) / NORMEF
              NZX  = SIGTRZX(IEL) / NORMEF

              FACN = BETA*AN*YOUNG(IEL)
              FACT = BETA*AS*SHEAR

              DO ITER=1,3
                SZZ = ABS(SIGNZZ(IEL))
                SYZ = ABS(SIGNYZ(IEL))
                SZX = ABS(SIGNZX(IEL))
                DSZZ = -FACN*NZZ*SZZ**(BETA-ONE)
                DSYZ = -FACT*NYZ*SYZ*SVMT**(BETA-TWO)
                DSZX = -FACT*NZX*SZX*SVMT**(BETA-TWO)
                DYLD = -BETA*HYIELD(IEL)*FYIELD(IEL)**(BETA-ONE)
                FPRIM = DSZZ + DSYZ + DSZX + DYLD

                DEP(IEL) = DEP(IEL) - PHI / FPRIM

                SIGNZZ(IEL) = SIGTRZZ(IEL) - YOUNG(IEL)*DEP(IEL) * NZZ
                SIGNYZ(IEL) = SIGTRYZ(IEL) - SHEAR*DEP(IEL) * NYZ
                SIGNZX(IEL) = SIGTRZX(IEL) - SHEAR*DEP(IEL) * NZX
                FYIELD(IEL) = YLD0 + HYIELD(IEL)*DEP(IEL)

                SVMT = SQRT(SIGNYZ(IEL)**2 + SIGNZX(IEL)**2)
                SIG_EFF = AN * SZZ**BETA + AS * SVMT**BETA 
                PHI     = SIG_EFF - FYIELD(IEL)**BETA
              ENDDO  
              DEP(IEL) = MAX(ZERO, DEP(IEL))*OFF(IEL)
              PLA(IEL) = PLA(IEL) + DEP(IEL)
            ENDIF 
          ENDDO
c
        ELSE   ! ICOMP = 1   ! elastic in compression
c
          DO II = 1,NTRAC                                                    
            IEL= ELTRAC(II)                                                   
            AA = RN(IEL)*(ONE-ALPHA*SIN(SYM(IEL)))
            AN = ONE/MAX(EM20,AA)**BETA
            AS = ONE/MAX(EM20,RS(IEL))**BETA
            SVMN = ABS(SIGNZZ(IEL))                      ! plast - normal stress
            SVMT = SQRT(SIGNYZ(IEL)**2 + SIGNZX(IEL)**2) ! plast - shear stress 
            SIG_EFF = AN * SVMN**BETA + AS * SVMT**BETA 
            YLD0 = FYIELD(IEL)
            PHI  = SIG_EFF - YLD0**BETA
            IF (SIG_EFF > ZERO .and. PHI > ZERO) THEN ! plastic
              NORMEF = SQRT (SIGTRZZ(IEL)**2+SIGTRYZ(IEL)**2 + SIGTRZX(IEL)**2)
              NZZ  = SIGTRZZ(IEL) / NORMEF
              NYZ  = SIGTRYZ(IEL) / NORMEF
              NZX  = SIGTRZX(IEL) / NORMEF

              FACN = BETA*AN*YOUNG(IEL)
              FACT = BETA*AS*SHEAR

              DO ITER=1,3
                SZZ = ABS(SIGNZZ(IEL))
                SYZ = ABS(SIGNYZ(IEL))
                SZX = ABS(SIGNZX(IEL))
                DSZZ = -FACN*NZZ*SZZ**(BETA-ONE)
                DSYZ = -FACT*NYZ*SYZ*SVMT**(BETA-TWO)
                DSZX = -FACT*NZX*SZX*SVMT**(BETA-TWO)
                DYLD = -BETA*HYIELD(IEL)*FYIELD(IEL)**(BETA-ONE)
                FPRIM = DSZZ + DSYZ + DSZX + DYLD

                DEP(IEL) = DEP(IEL) - PHI / FPRIM

                SIGNZZ(IEL) = SIGTRZZ(IEL) - YOUNG(IEL)*DEP(IEL) * NZZ
                SIGNYZ(IEL) = SIGTRYZ(IEL) - SHEAR*DEP(IEL) * NYZ
                SIGNZX(IEL) = SIGTRZX(IEL) - SHEAR*DEP(IEL) * NZX
                FYIELD(IEL) = YLD0 + HYIELD(IEL)*DEP(IEL)

                SVMT = SQRT(SIGNYZ(IEL)**2 + SIGNZX(IEL)**2)
                SIG_EFF = AN * SZZ**BETA + AS * SVMT**BETA 
                PHI     = SIG_EFF - FYIELD(IEL)**BETA
              ENDDO  
              DEP(IEL) = MAX(ZERO, DEP(IEL))*OFF(IEL)
              PLA(IEL) = PLA(IEL) + DEP(IEL)
            ENDIF 
          ENDDO
c
          DO II = 1,NCOMP                                                  
            IEL= ELCOMP(II)                                                   
            AS = ONE/MAX(EM20,RS(IEL))**BETA
            SVMT = SQRT(SIGNYZ(IEL)**2 + SIGNZX(IEL)**2) ! plast - shear stress 
            SIG_EFF = AS * SVMT**BETA 
            YLD0 = FYIELD(IEL)
            PHI  = SIG_EFF - YLD0**BETA
            IF (SIG_EFF > ZERO .and. PHI > ZERO) THEN ! plastic
              NORMEF = SVMT
              NYZ  = SIGTRYZ(IEL) / NORMEF
              NZX  = SIGTRZX(IEL) / NORMEF
              FACT = BETA*AS*SHEAR

              DO ITER=1,3
                SYZ = ABS(SIGNYZ(IEL))
                SZX = ABS(SIGNZX(IEL))
                DSYZ = -FACT*NYZ*SYZ*SVMT**(BETA-TWO)
                DSZX = -FACT*NZX*SZX*SVMT**(BETA-TWO)
                DYLD = -BETA*HYIELD(IEL)*FYIELD(IEL)**(BETA-ONE)
                FPRIM = DSYZ + DSZX + DYLD

                DEP(IEL) = DEP(IEL) - PHI / FPRIM

                SIGNYZ(IEL) = SIGTRYZ(IEL) - SHEAR*DEP(IEL) * NYZ
                SIGNZX(IEL) = SIGTRZX(IEL) - SHEAR*DEP(IEL) * NZX
                FYIELD(IEL) = YLD0 + HYIELD(IEL)*DEP(IEL)

                SVMT = SQRT(SIGNYZ(IEL)**2 + SIGNZX(IEL)**2)
                SIG_EFF = AS * SVMT**BETA 
                PHI     = SIG_EFF - FYIELD(IEL)**BETA
              ENDDO  
              DEP(IEL) = MAX(ZERO, DEP(IEL))*OFF(IEL)
              PLA(IEL) = PLA(IEL) + DEP(IEL)
            ENDIF 
          ENDDO
c
        END IF    ! ICOMP
c
      ENDIF  ! BETA                                                                   
c----------------------------end iplas 2--------------------------------------
c     normal projection option (hidden: iplas = 1) deleted in august 2021
c------------------
      IF (VP == 1) THEN
        DO IEL=1,NEL
          EPSP(IEL)   = DEP(IEL) * DTINV  ! plastic strain rate
          UVAR(IEL,1) = ASRATE*EPSP(IEL) + (ONE - ASRATE)*UVAR(IEL,1)
          EPSD(IEL)   = UVAR(IEL,1)
        ENDDO
      ENDIF
C-----------
      RETURN
      END
