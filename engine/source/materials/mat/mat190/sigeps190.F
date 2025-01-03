Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
      !||====================================================================
      !||    sigeps190          ../engine/source/materials/mat/mat190/sigeps190.F
      !||--- called by ------------------------------------------------------
      !||    mulaw              ../engine/source/materials/mat_share/mulaw.F90
      !||--- calls      -----------------------------------------------------
      !||    condamage          ../engine/source/materials/mat/mat190/condamage.F
      !||    conversion         ../engine/source/materials/mat/mat190/conversion.F
      !||    prodata            ../engine/source/materials/tools/prodATA.F
      !||    valpvec_v          ../engine/source/materials/mat/mat033/sigeps33.F
      !||    valpvecdp_v        ../engine/source/materials/mat/mat033/sigeps33.F
      !||--- uses       -----------------------------------------------------
      !||    condamage_mod      ../engine/source/materials/mat/mat190/condamage.F
      !||    conversion_mod     ../engine/source/materials/mat/mat190/conversion.F
      !||    matparam_def_mod   ../common_source/modules/mat_elem/matparam_def_mod.F90
      !||====================================================================
      SUBROUTINE SIGEPS190(
     1     NEL    ,NUVAR   ,RHO     ,ET      ,
     2     EPSPXX ,EPSPYY  ,EPSPZZ  ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
     3     SIGNXX ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     4     MFXX   ,MFXY    ,MFXZ    ,MFYX    ,MFYY    ,MFYZ   ,
     5     MFZX   ,MFZY    ,MFZZ    ,SOUNDSP ,VISCMAX ,UVAR   ,                          
     7     NUMTABL,MATPARAM,NVARTMP ,VARTMP  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE CONVERSION_MOD
      USE CONDAMAGE_MOD
      USE MATPARAM_DEF_MOD
C-----------------------------------------------
C Material law for isotropic path dependent recoverable foam
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
#include      "comlock.inc"
#include      "scr05_c.inc"
#include      "impl1_c.inc"
#include      "com01_c.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C RHO0    | NEL     | F | R | INITIAL DENSITY
C RHO     | NEL     | F | R | DENSITY
C VOLUME  | NEL     | F | R | VOLUME
C EINT    | NEL     | F | R | TOTAL INTERNAL ENERGY
C EPSPXX  | NEL     | F | R | STRAIN RATE XX
C EPSPYY  | NEL     | F | R | STRAIN RATE YY
C ...     |         |   |   |
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C SIGVXX  | NEL     | F | W | VISCOUS STRESS XX
C SIGVYY  | NEL     | F | W | VISCOUS STRESS YY
C ...     |         |   |   |
C SOUNDSP | NEL     | F | W | SOUND SPEED (NEEDED FOR TIME STEP)
C VISCMAX | NEL     | F | W | MAXIMUM DAMPING MODULUS(NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
C-------------------------------------------------------------------------
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NEL,NUVAR,NVARTMP,NUMTABL
      !INTEGER, DIMENSION(NEL),INTENT(IN) :: NGL
      my_real ,DIMENSION(NEL) ,INTENT(IN) :: RHO,
     .         EPSPXX,EPSPYY,EPSPZZ,EPSPXY,EPSPYZ,EPSPZX,
     .         MFXX   ,MFXY   ,MFXZ ,MFYX, MFYY  , MFYZ  ,  
     .         MFZX   ,MFZY   ,MFZZ                  
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real ,DIMENSION(NEL) ,INTENT(OUT) ::
     .      SIGNXX ,SIGNYY ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX,VISCMAX
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real, DIMENSION(NEL) ,INTENT(INOUT) :: SOUNDSP, ET
      my_real ,DIMENSION(NEL,NUVAR)   ,INTENT(INOUT) :: UVAR
      INTEGER ,DIMENSION(NEL,NVARTMP) ,INTENT(INOUT) :: VARTMP
      TYPE(MATPARAM_STRUCT_)  :: MATPARAM
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER I,IK,IJ 
      my_real, DIMENSION(NEL) :: 
     .   JAC ,JEQ ,B11    ,B22 ,B33 ,B12 ,
     .   B23 ,B13 ,BSQR11 ,BSQR22 ,I1BAR ,I2BAR ,
     .   BSQR33 ,BSQR12 ,BSQR23 ,BSQR13 ,H1 ,H2 ,H3 ,
     .   ZXX   ,ZXY ,ZYZ ,ZXZ , ZYX , ZZY ,
     .   ZZX   ,ZYY ,ZZZ ,WHYSMAX ,
     .   ZXXOLD  ,ZXYOLD  ,ZYZOLD ,ZXZOLD , 
     .   ZYXOLD  , ZZYOLD ,ZZXOLD ,ZYYOLD ,ZZZOLD ,  
     .   ZXXMID ,ZXYMID ,ZYZMID ,ZXZMID , 
     .   ZYXMID , ZZYMID ,ZZXMID ,ZYYMID ,ZZZMID ,
     .   DZXX ,DZXY ,DZYZ ,DZXZ , 
     .   DZYX , DZZY ,DZZX ,DZYY ,DZZZ ,  
     .   S1   ,S2 ,S3 ,S4 ,S5 ,S6 ,
     .   FS11 ,FS12 ,FS13 ,FS21 ,FS22 ,FS23 ,
     .   FS31 ,FS32 ,FS33 ,SLOPEMAX ,  DAMAGE ,   
     .   ZXXD ,ZYYD ,ZZZD ,ZXYD ,ZYZD ,ZZXD ,EPST ,YLD
c       
      my_real
     .   G,K,NU,HU, SHAPE ,GS,KS,STIFFMAX,AA, DE,E_NEW,EPSS, E_OLD,
     .   EP2,EP3,EP4,EP5,EP6,ERT11,ERT12,ERT13,ERT21,E,EMOD,EP1,
     .   ERT22,ERT23,ERT31,ERT32,ERT33,SCAL,
     .   EPSP(3),XSCAL,
C      
     .   STRAIN(NEL,3),SLOPE(NEL,3),DRATE(NEL,3),EV(NEL,3),
     .   F(NEL,3,3),FO(NEL,3,3),C(NEL,3,3), EGL(NEL,6),
C
     .   SPKNODAM(NEL,6),SPKNORATE(NEL,6),SPK(NEL,6),SIG(NEL,6) ,
     .   DFIRST(NEL,3,6),CIJKL(NEL,6,6),
c
     .   EVV(MVSIZ,3), VAL(MVSIZ,3),AV(MVSIZ,6),DIRPRV(MVSIZ,3,3) 
c     
c=========================================================================
c      Isotropic path dependent recoverable foam law
c-------------------------------------------------------------------------
!      
      !=======================================================================
      ! - INITIALISATION OF COMPUTATION ON TIME STEP
      !=======================================================================
      ! Recovering model parameters
      EMOD     = MATPARAM%UPARAM(1)  
      K        = MATPARAM%UPARAM(2)  
      G        = MATPARAM%UPARAM(3)  
      XSCAL    = MATPARAM%UPARAM(4)
      SCAL     = MATPARAM%UPARAM(5)
      HU       = MATPARAM%UPARAM(6)       
      SHAPE    = MATPARAM%UPARAM(7) 
      STIFFMAX = EMOD*HUNDRED  
      ! Recovering user variables
      WHYSMAX(1:NEL) = UVAR(1:NEL,13)
!
      !=======================================================================
      ! - COMPUTATION OF THE DEFORMATION GRADIENT
      !=======================================================================                         
      DO I=1,NEL
        F(I,1,1) = ONE+MFXX(I)
        F(I,2,2) = ONE+MFYY(I)
        F(I,3,3) = ONE+MFZZ(I)
        F(I,1,2) = MFXY(I)
        F(I,2,3) = MFYZ(I)
        F(I,1,3) = MFXZ(I)
        F(I,2,1) = MFYX(I)
        F(I,3,2) = MFZY(I)
        F(I,3,1) = MFZX(I)
      ENDDO                               
!
      !=======================================================================
      ! - COMPUTATION OF THE GREEN-LAGRANGE TENSORS (FT * F - I)/2
      !=======================================================================  
      CALL PRODATA(F, C, EGL, NEL)
      DO I=1,NEL
        ! Current Green-Lagrange tensor
        ZXX(I) = EGL(I,1)
        ZYY(I) = EGL(I,2)
        ZZZ(I) = EGL(I,3)
        ZXY(I) = EGL(I,4)
        ZYZ(I) = EGL(I,5)
        ZZX(I) = EGL(I,6)
C
        ! Recover old Green-Lagrange tensor
        ZXXOLD(I) = UVAR(I,1)  
        ZYYOLD(I) = UVAR(I,2) 
        ZZZOLD(I) = UVAR(I,3) 
        ZXYOLD(I) = UVAR(I,4) 
        ZYZOLD(I) = UVAR(I,5) 
        ZZXOLD(I) = UVAR(I,6) 
c
        ! Save current Green-Lagrange tensor
        UVAR(I,1) = ZXX(I)              
        UVAR(I,2) = ZYY(I)              
        UVAR(I,3) = ZZZ(I)              
        UVAR(I,4) = ZXY(I)              
        UVAR(I,5) = ZYZ(I)              
        UVAR(I,6) = ZZX(I)   
c        
        ! Midpoint Green-Lagrange tensor at t = n + 1/2
        ZXXMID(I)  =  (ZXX(I) + ZXXOLD(I))/TWO
        ZYYMID(I)  =  (ZYY(I) + ZYYOLD(I))/TWO
        ZZZMID(I)  =  (ZZZ(I) + ZZZOLD(I))/TWO
        ZXYMID(I)  =  (ZXY(I) + ZXYOLD(I))/TWO
        ZYZMID(I)  =  (ZYZ(I) + ZYZOLD(I))/TWO
        ZZXMID(I)  =  (ZZX(I) + ZZXOLD(I))/TWO
c
        ! Deformation Jacobian
        JAC(I) = F(I,1,1)*F(I,2,2)*F(I,3,3) - F(I,1,1)*F(I,2,3)*F(I,3,2) -
     .           F(I,3,3)*F(I,1,2)*F(I,2,1) + F(I,1,2)*F(I,2,3)*F(I,3,1) +
     .           F(I,2,1)*F(I,3,2)*F(I,1,3) - F(I,2,2)*F(I,3,1)*F(I,1,3)
        JAC(I) = MAX(JAC(I),ONE)
c
      ENDDO     
c
      !=======================================================================
      ! - COMPUTATION OF THE GREEN-LAGRANGE EIGENVALUES AND VECTORS
      !=======================================================================
      DO I=1,NEL                       
        AV(I,1) = ZXXMID(I)
        AV(I,2) = ZYYMID(I)
        AV(I,3) = ZZZMID(I)
        AV(I,4) = ZXYMID(I)
        AV(I,5) = ZYZMID(I)
        AV(I,6) = ZZXMID(I)
      ENDDO          
      IF (IRESP == 1) THEN
        CALL VALPVECDP_V(AV,EVV,DIRPRV,NEL)
      ELSE
        CALL VALPVEC_V(AV,EVV,DIRPRV,NEL)
      ENDIF
c
      !=======================================================================
      ! - COMPUTATION OF THE STRAIN RATES IN PRINCIPAL DIRECTIONS
      !=======================================================================
      DO I=1,NEL
c
        ! True strain rates
        EP1 = EPSPXX(I)
        EP2 = EPSPYY(I)      
        EP3 = EPSPZZ(I) 
        EP4 = HALF*EPSPXY(I)        
        EP5 = HALF*EPSPYZ(I)
        EP6 = HALF*EPSPZX(I)
c
        ! Rotation in principal directions
        ERT11 =  DIRPRV(I,1,1)*EP1 + DIRPRV(I,2,1)*EP4 + DIRPRV(I,3,1)*EP6
        ERT12 =  DIRPRV(I,1,2)*EP1 + DIRPRV(I,2,2)*EP4 + DIRPRV(I,3,2)*EP6
        ERT13 =  DIRPRV(I,1,3)*EP1 + DIRPRV(I,2,3)*EP4 + DIRPRV(I,3,3)*EP6
        ERT21 =  DIRPRV(I,1,1)*EP4 + DIRPRV(I,2,1)*EP2 + DIRPRV(I,3,1)*EP5
        ERT22 =  DIRPRV(I,1,2)*EP4 + DIRPRV(I,2,2)*EP2 + DIRPRV(I,3,2)*EP5
        ERT23 =  DIRPRV(I,1,3)*EP4 + DIRPRV(I,2,3)*EP2 + DIRPRV(I,3,3)*EP5  
        ERT31 =  DIRPRV(I,1,1)*EP6 + DIRPRV(I,2,1)*EP5 + DIRPRV(I,3,1)*EP3
        ERT32 =  DIRPRV(I,1,2)*EP6 + DIRPRV(I,2,2)*EP5 + DIRPRV(I,3,2)*EP3
        ERT33 =  DIRPRV(I,1,3)*EP6 + DIRPRV(I,2,3)*EP5 + DIRPRV(I,3,3)*EP3       
        EPSP(1) = DIRPRV(I,1,1)*ERT11 + DIRPRV(I,2,1)*ERT21 
     .                                + DIRPRV(I,3,1)*ERT31 
        EPSP(2) = DIRPRV(I,1,2)*ERT12 + DIRPRV(I,2,2)*ERT22 
     .                                + DIRPRV(I,3,2)*ERT32 
        EPSP(3) = DIRPRV(I,1,3)*ERT13 + DIRPRV(I,2,3)*ERT23 
     .                                + DIRPRV(I,3,3)*ERT33
c
        !(e > 0 compression and e < 0 traction)
        EV(I,1) = SQRT(TWO*EVV(I,1) + ONE)
        EV(I,2) = SQRT(TWO*EVV(I,2) + ONE)
        EV(I,3) = SQRT(TWO*EVV(I,3) + ONE)
        STRAIN(I,1) = ONE - EV(I,1)        
        STRAIN(I,2) = ONE - EV(I,2)        
        STRAIN(I,3) = ONE - EV(I,3) 
        EPST(I) = SQRT(STRAIN(I,1)**2 + STRAIN(I,2)**2 + STRAIN(I,3)**2) 
        DRATE(I,1) = EPSP(1)*(ONE - STRAIN(I,1)) ! eng
        DRATE(I,2) = EPSP(2)*(ONE - STRAIN(I,2))
        DRATE(I,3) = EPSP(3)*(ONE - STRAIN(I,3))
      ENDDO
c
      !=======================================================================
      ! - COMPUTATION OF THE GLOBAL INCREMENTAL STIFFNESS MATRIX
      ! (Vérifier si effet de Poisson purement numérique)
      !=======================================================================
      CIJKL = ZERO
      CALL CONVERSION(ZXXMID  ,ZYYMID  ,ZZZMID  ,ZXYMID  ,ZYZMID  ,ZZXMID  ,  
     .                CIJKL   ,DFIRST  ,DRATE   ,XSCAL   ,SCAL    ,NEL     ,
     .                JAC     ,SLOPE   ,NUMTABL ,MATPARAM%TABLE   ,NVARTMP ,
     .                VARTMP  ,YLD     ,MATPARAM%TABLE(1)%NDIM    )
C
      DO I = 1,NEL
C
C       increment of Green Lagrange strains
C
        DZXX(I) = ZXX(I)-ZXXOLD(I)
        DZYY(I) = ZYY(I)-ZYYOLD(I)
        DZZZ(I) = ZZZ(I)-ZZZOLD(I)
        DZXY(I) = ZXY(I)-ZXYOLD(I)
        DZYZ(I) = ZYZ(I)-ZYZOLD(I)
        DZZX(I) = ZZX(I)-ZZXOLD(I)
C
C       USE GAMMAS INSTEAD OF EPSILONS FOR INCREMENTS
C
        DZXY(I) = DZXY(I)*TWO
        DZYZ(I) = DZYZ(I)*TWO
        DZZX(I) = DZZX(I)*TWO
C
C       INCREMENT HYPERELASTIC 2PK STRESSES   DS=C*DE
C
        DO IK=1,6
          SPKNORATE(I,IK)= UVAR(I,6+IK)
     .        + CIJKL(I,1,IK)*DZXX(I)+CIJKL(I,2,IK)*DZYY(I)
     .        + CIJKL(I,3,IK)*DZZZ(I)+CIJKL(I,4,IK)*DZXY(I)
     .        + CIJKL(I,5,IK)*DZYZ(I)+CIJKL(I,6,IK)*DZZX(I)
        ENDDO
        UVAR(I, 7)  =  SPKNORATE(I,1)         
        UVAR(I, 8)  =  SPKNORATE(I,2)         
        UVAR(I, 9)  =  SPKNORATE(I,3)         
        UVAR(I,10)  =  SPKNORATE(I,4)         
        UVAR(I,11)  =  SPKNORATE(I,5)         
        UVAR(I,12)  =  SPKNORATE(I,6)         

c       add the viscous 2PK stresses ( dynamic overstress )
c       drate = dynamic principal 2PK - static principal 2PK
C

        DO IK=1,6
          IF (IK < 4) THEN
            SPKNODAM(I,IK)=SPKNORATE(I,IK)
     .                      +DFIRST(I,1,IK)*DRATE(I,1)
     .                      +DFIRST(I,2,IK)*DRATE(I,2)
     .                      +DFIRST(I,3,IK)*DRATE(I,3)
          ELSE
            SPKNODAM(I,IK)=SPKNORATE(I,IK)
     .                      + (DFIRST(I,1,IK)*DRATE(I,1)
     .                      +  DFIRST(I,2,IK)*DRATE(I,2)
     .                      +  DFIRST(I,3,IK)*DRATE(I,3) )*HALF
          ENDIF
        ENDDO
c
      ENDDO !nel
C
C     compute damage at t(n+1)
      CALL CONDAMAGE(ZXX     ,ZYY     ,ZZZ     ,ZXY     ,ZZX     ,ZYZ     ,
     .               DAMAGE  ,NEL     ,HU      ,SHAPE   ,WHYSMAX ,
     .               NUMTABL ,MATPARAM%TABLE   ,NVARTMP ,VARTMP  )     
C
C     apply damage
C 
      DO I=1,NEL
        UVAR(I,13) =  WHYSMAX(I) 
        DO IK=1,6
          SPK(I,IK)=SPKNODAM(I,IK)*(ONE-DAMAGE(I))
        ENDDO
C
C       transform 2PK to Cauchy stresses  
C
        S1(I) = SPK(I,1)                    
        S2(I) = SPK(I,2)                    
        S3(I) = SPK(I,3)                    
        S4(I) = SPK(I,4)                    
        S5(I) = SPK(I,5)                    
        S6(I) = SPK(I,6)                    
C
        FS11(I) = F(I,1,1)*S1(I) + F(I,1,2)*S4(I) + F(I,1,3)*S6(I)
        FS12(I) = F(I,1,1)*S4(I) + F(I,1,2)*S2(I) + F(I,1,3)*S5(I)
        FS13(I) = F(I,1,1)*S6(I) + F(I,1,2)*S5(I) + F(I,1,3)*S3(I)
        FS21(I) = F(I,2,1)*S1(I) + F(I,2,2)*S4(I) + F(I,2,3)*S6(I)
        FS22(I) = F(I,2,1)*S4(I) + F(I,2,2)*S2(I) + F(I,2,3)*S5(I)
        FS23(I) = F(I,2,1)*S6(I) + F(I,2,2)*S5(I) + F(I,2,3)*S3(I)
        FS31(I) = F(I,3,1)*S1(I) + F(I,3,2)*S4(I) + F(I,3,3)*S6(I)
        FS32(I) = F(I,3,1)*S4(I) + F(I,3,2)*S2(I) + F(I,3,3)*S5(I)
        FS33(I) = F(I,3,1)*S6(I) + F(I,3,2)*S5(I) + F(I,3,3)*S3(I)
C
        SIGNXX(I) = FS11(I)*F(I,1,1)+FS12(I)*F(I,1,2)+FS13(I)*F(I,1,3)
        SIGNYY(I) = FS21(I)*F(I,2,1)+FS22(I)*F(I,2,2)+FS23(I)*F(I,2,3)
        SIGNZZ(I) = FS31(I)*F(I,3,1)+FS32(I)*F(I,3,2)+FS33(I)*F(I,3,3)
        SIGNXY(I) = FS11(I)*F(I,2,1)+FS12(I)*F(I,2,2)+FS13(I)*F(I,2,3)
        SIGNYZ(I) = FS21(I)*F(I,3,1)+FS22(I)*F(I,3,2)+FS23(I)*F(I,3,3)
        SIGNZX(I) = FS11(I)*F(I,3,1)+FS12(I)*F(I,3,2)+FS13(I)*F(I,3,3)
C
        SIGNXX(I) = SIGNXX(I) / JAC(I)   
        SIGNYY(I) = SIGNYY(I) / JAC(I)   
        SIGNZZ(I) = SIGNZZ(I) / JAC(I)   
        SIGNXY(I) = SIGNXY(I) / JAC(I)   
        SIGNYZ(I) = SIGNYZ(I) / JAC(I)   
        SIGNZX(I) = SIGNZX(I) / JAC(I)  
C
C       timestep if the slope of the stress/strain curve
C       exceeds the user specified modulus
C
        GS=G
        KS=K
        SLOPEMAX(I)=MAX(SLOPE(I,1) , SLOPE(I,2))
        SLOPEMAX(I)=MAX(SLOPEMAX(I), SLOPE(I,3))
        SLOPEMAX(I)=MAX(SLOPEMAX(I), EMOD)

        GS = SLOPEMAX(I)*HALF
        KS = SLOPEMAX(I)*THIRD
        !
        SOUNDSP(I) = SQRT(SLOPEMAX(I)/RHO(I))
        VISCMAX(I) = ZERO
        !
        ET(I) = MAX(SLOPE(I,1),SLOPE(I,2),SLOPE(I,3))/EMOD
      ENDDO
C-------------------------------------------------------------------------
      RETURN
      END
