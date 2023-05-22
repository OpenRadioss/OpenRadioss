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
Chd|  SIGEPS190                      source/materials/mat/mat190/sigeps190.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE SIGEPS190(
     1     NEL    ,NUVAR   ,TIME   ,RHO   ,
     2     EPSPXX ,EPSPYY ,EPSPZZ  ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
     3     SIGNXX ,SIGNYY ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     4     MFXX   ,MFXY    ,MFXZ   ,MFYX    , MFYY   , MFYZ  ,
     5     MFZX   ,MFZY    ,MFZZ   ,                            
     6     SOUNDSP,VISCMAX,UVAR    ,                          
     7     NUMTABL,MATPARAM,NVARTMP,VARTMP)
C Material law for isotropic path dependent recoverable foam
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE CONVERSION_MOD
      USE CONDAMAGE_MOD
      USE TABLE_MOD
      USE INTERFACE_TABLE_MOD
      USE MATPARAM_DEF_MOD
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
      my_real ,INTENT(IN) :: TIME
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
      my_real, DIMENSION(NEL) ,INTENT(INOUT) :: SOUNDSP 
      my_real ,DIMENSION(NEL,NUVAR)   ,INTENT(INOUT) :: UVAR
      INTEGER ,DIMENSION(NEL,NVARTMP) ,INTENT(INOUT) :: VARTMP
      TYPE(MATPARAM_STRUCT_)  :: MATPARAM
C----------------------------------------------------------------
c    VARIABLES FOR FUNCTION INTERPOLATION 
C----------------------------------------------------------------
      EXTERNAL FINTER
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER I,IK,IJ 
      my_real, DIMENSION(NEL) :: 
        ! Deformation GRADIENT
     .   FPSXX,FPSYY,FPSZZ,
     .   FPSXY,FPSYX,FPSXZ,
     .   FPSZX,FPSYZ,FPSZY,
         ! Old Deformation GRADIENT
     .   FPOSXX,FPOSYY,FPOSZZ,
     .   FPOSXY,FPOSYX,FPOSXZ,
     .   FPOSZX,FPOSYZ,FPOSZY,
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
     .   ZXXD ,ZYYD ,ZZZD ,ZXYD ,ZYZD ,ZZXD 
c       
      my_real
     .   G,K,NU,HU, SHAPE ,GS,KS,
     .   EP2,EP3,EP4,EP5,EP6,ERT11,ERT12,ERT13,ERT21,E,EMOD,EP1,
     .   ERT22,ERT23,ERT31,ERT32,ERT33,   SCAL,SCALINT,
     .   EPSP(3),
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
C      
      EMOD = MATPARAM%UPARAM(1)  
      E    = MATPARAM%UPARAM(2)     
      K    = MATPARAM%UPARAM(3)     
      NU   = MATPARAM%UPARAM(4)     
      G    = MATPARAM%UPARAM(5)     
      SCAL = MATPARAM%UPARAM(6)     
  
      HU      = MATPARAM%UPARAM(8)       
      SHAPE   = MATPARAM%UPARAM(9)    
c===================================================================================
c     FPxx = Deformation Gradient
      DO I=1,NEL                  
        FPSXX(I) = ONE+MFXX(I)     
        FPSYY(I) = ONE+MFYY(I)     
        FPSZZ(I) = ONE+MFZZ(I)     
      END DO                      
      FPSXY(1:NEL) = MFXY(1:NEL)       
      FPSYX(1:NEL) = MFYX(1:NEL)       
      FPSXZ(1:NEL) = MFXZ(1:NEL)       
      FPSZX(1:NEL) = MFZX(1:NEL)       
      FPSYZ(1:NEL) = MFYZ(1:NEL)       
      FPSZY(1:NEL) = MFZY(1:NEL)  
      IF ( TIME == ZERO ) THEN             
        DO I=1,NEL                        
          UVAR(I,1) = ONE               
          UVAR(I,2) = ONE                
          UVAR(I,3) = ONE                         
        ENDDO                             
      ENDIF                               
c                              
      DO I=1,NEL                          
        FPOSXX(I) = UVAR(I,1)  
        FPOSYY(I) = UVAR(I,2)             
        FPOSZZ(I) = UVAR(I,3)             
        FPOSXY(I) = UVAR(I,4)             
        FPOSYX(I) = UVAR(I,5)             
        FPOSXZ(I) = UVAR(I,6)             
        FPOSZX(I) = UVAR(I,7)             
        FPOSYZ(I) = UVAR(I,8)             
        FPOSZY(I) = UVAR(I,9)             
c
        UVAR(I,1) = FPSXX(I)              
        UVAR(I,2) = FPSYY(I)              
        UVAR(I,3) = FPSZZ(I)              
        UVAR(I,4) = FPSXY(I)              
        UVAR(I,5) = FPSYX(I)              
        UVAR(I,6) = FPSXZ(I)              
        UVAR(I,7) = FPSZX(I)              
        UVAR(I,8) = FPSYZ(I)              
        UVAR(I,9) = FPSZY(I)              
        WHYSMAX(I)= UVAR(I,17)        
      ENDDO                               
                             
      DO I=1,NEL
C
c     deformation gradient at t(n+1)
C
        F(I,1,1) = FPSXX(I)
        F(I,2,2) = FPSYY(I)
        F(I,3,3) = FPSZZ(I)
        F(I,1,2) = FPSXY(I)
        F(I,2,3) = FPSYZ(I)
        F(I,1,3) = FPSXZ(I)
        F(I,2,1) = FPSYX(I)
        F(I,3,2) = FPSZY(I)
        F(I,3,1) = FPSZX(I)
      ENDDO                               
c
c       Green-Lagrange strains at EGL = Zt(n+1) = (FT * F - I)/2
c
      CALL PRODATA(F, C, EGL, NEL)

      DO I=1,NEL
        ZXX(I) = EGL(I,1)
        ZYY(I) = EGL(I,2)
        ZZZ(I) = EGL(I,3)
        ZXY(I) = EGL(I,4)
        ZYZ(I) = EGL(I,5)
        ZZX(I) = EGL(I,6)
C
c       deformation gradient at t(n)

        ZXXOLD(I) = UVAR(I,19)  
        ZYYOLD(I) = UVAR(I,20) 
        ZZZOLD(I) = UVAR(I,21) 
        ZXYOLD(I) = UVAR(I,22) 
        ZYZOLD(I) = UVAR(I,23) 
        ZZXOLD(I) = UVAR(I,24) 

        UVAR(I,19) = ZXX(I)              
        UVAR(I,20) = ZYY(I)              
        UVAR(I,21) = ZZZ(I)              
        UVAR(I,22) = ZXY(I)              
        UVAR(I,23) = ZYZ(I)              
        UVAR(I,24) = ZZX(I)              
      ENDDO                               
      DO I=1,NEL
C
c       midpoint Green lagrange strains
C 
        ZXXMID(I)  =  (ZXX(I)+ ZXXOLD(I))/TWO
        ZYYMID(I)  =  (ZYY(I)+ ZYYOLD(I))/TWO
        ZZZMID(I)  =  (ZZZ(I)+ ZZZOLD(I))/TWO
        ZXYMID(I)  =  (ZXY(I)+ ZXYOLD(I))/TWO
        ZYZMID(I)  =  (ZYZ(I)+ ZYZOLD(I))/TWO
        ZZXMID(I)  =  (ZZX(I)+ ZZXOLD(I))/TWO
C
        JAC(I) = F(I,1,1)*F(I,2,2)*F(I,3,3) - F(I,1,1)*F(I,2,3)*F(I,3,2) -
     .           F(I,3,3)*F(I,1,2)*F(I,2,1) + F(I,1,2)*F(I,2,3)*F(I,3,1) +
     .           F(I,2,1)*F(I,3,2)*F(I,1,3) - F(I,2,2)*F(I,3,1)*F(I,1,3)
        IF(JAC(I)==ZERO) JAC(I) = ONE
C
      ENDDO
c     compute eigenvalues and eigenvectors of the midpoint GL strain tensor    
c     crucial : need to make sure that the order of eigenvalues
c     is the same as what is done in subroutine conversion
      DO I=1,NEL                       
        AV(I,1) = ZXXMID(I)
        AV(I,2) = ZYYMID(I)
        AV(I,3) = ZZZMID(I)
        AV(I,4) = ZXYMID(I)
        AV(I,5) = ZYZMID(I)
        AV(I,6) = ZZXMID(I)
      ENDDO                     
      CALL VALPVEC_V  (AV,EVV,DIRPRV,NEL)

c     compute the strain rates in each one
c     of the principal directions of the midpoint GL tensor
c
c     just for the record : the eigenvectors of the midpoint GL
c     are of course not the eigenvectors of the strain rate tensor
c     this is a flaw of the formulation, kind of inevitable
c

      DO I=1,NEL
        !calculation of D (true strain rate )
        EP1 = EPSPXX(I)
        EP2 = EPSPYY(I)      
        EP3 = EPSPZZ(I) 
        EP4 = HALF*EPSPXY(I)        
        EP5 = HALF*EPSPYZ(I)
        EP6 = HALF*EPSPZX(I)
c c      phi_trans*L*phi_t    
c c      VEC-transposed * EPSP
        ERT11 =DIRPRV(I,1,1)*EP1 + DIRPRV(I,2,1)*EP4 + DIRPRV(I,3,1)*EP6
        ERT12 =DIRPRV(I,1,2)*EP1 + DIRPRV(I,2,2)*EP4 + DIRPRV(I,3,2)*EP6
        ERT13 =DIRPRV(I,1,3)*EP1 + DIRPRV(I,2,3)*EP4 + DIRPRV(I,3,3)*EP6
     
        ERT21 =DIRPRV(I,1,1)*EP4 + DIRPRV(I,2,1)*EP2 + DIRPRV(I,3,1)*EP5
        ERT22 =DIRPRV(I,1,2)*EP4 + DIRPRV(I,2,2)*EP2 + DIRPRV(I,3,2)*EP5
        ERT23 =DIRPRV(I,1,3)*EP4 + DIRPRV(I,2,3)*EP2 + DIRPRV(I,3,3)*EP5  
     
        ERT31 =DIRPRV(I,1,1)*EP6 + DIRPRV(I,2,1)*EP5 + DIRPRV(I,3,1)*EP3
        ERT32 =DIRPRV(I,1,2)*EP6 + DIRPRV(I,2,2)*EP5 + DIRPRV(I,3,2)*EP3
        ERT33 =DIRPRV(I,1,3)*EP6 + DIRPRV(I,2,3)*EP5 + DIRPRV(I,3,3)*EP3       
c C
c c     VEC-transposed * EPSP * VEC    diagonal components only
c C
        EPSP(1) = DIRPRV(I,1,1)*ERT11 + DIRPRV(I,2,1)*ERT21 
     .                                + DIRPRV(I,3,1)*ERT31 
        EPSP(2) = DIRPRV(I,1,2)*ERT12 + DIRPRV(I,2,2)*ERT22 
     .                                + DIRPRV(I,3,2)*ERT32 
        EPSP(3) = DIRPRV(I,1,3)*ERT13 + DIRPRV(I,2,3)*ERT23 
     .                                + DIRPRV(I,3,3)*ERT33
         !(e > 0 compression and e < 0 traction)

        EV(I,1) = SQRT(TWO * EVV(I,1) + ONE )
        EV(I,2) = SQRT(TWO * EVV(I,2) + ONE )
        EV(I,3) = SQRT(TWO * EVV(I,3) + ONE )

        STRAIN(I,1) = ONE - EV(I,1)        
        STRAIN(I,2) = ONE - EV(I,2)        
        STRAIN(I,3) = ONE - EV(I,3) 
c c     abs(eps) not necessary  
        DRATE(I,1) = EPSP(1)*(ONE - STRAIN(I,1)) ! eng
        DRATE(I,2) = EPSP(2)*(ONE - STRAIN(I,2))
        DRATE(I,3) = EPSP(3)*(ONE - STRAIN(I,3))
      ENDDO
c c
c c     compute global incremental stiffness matrix
c c     essentially a conversion from principal midpoint GL system
c c     to the global reference system
c c
c c     drate goes in to subroutine conversion as true strain rates
c c     in the principal directions of the midpoint GL strain tensor
c c
c      drate comes out of subroutine conversion as principal 2PK
c      dynamic overstress ( principal directions of GL and 2PK are same )
c
      CALL CONVERSION(ZXXMID, ZYYMID  ,ZZZMID, ZXYMID, ZYZMID, ZZXMID,  
     .                   CIJKL, DFIRST  ,DRATE , SCAL  , NEL   , JAC , 
     .                   SLOPE, NUMTABL ,MATPARAM%TABLE ,NVARTMP,VARTMP)
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
C       USE GAMMA'S INSTEAD OF EPSILONS FOR INCREMENTS
C
        DZXY(I) =DZXY(I)*TWO
        DZYZ(I) =DZYZ(I)*TWO
        DZZX(I) =DZZX(I)*TWO
C
C       INCREMENT HYPERELASTIC 2PK STRESSES   DS=C*DE
C
        DO IK=1,6
          SPKNORATE(I,IK)= UVAR(I,10+IK)
     .        + CIJKL(I,1,IK)*DZXX(I)+CIJKL(I,2,IK)*DZYY(I)
     .        + CIJKL(I,3,IK)*DZZZ(I)+CIJKL(I,4,IK)*DZXY(I)
     .        + CIJKL(I,5,IK)*DZYZ(I)+CIJKL(I,6,IK)*DZZX(I)
        ENDDO
        UVAR(I,11)  =  SPKNORATE(I,1)         
        UVAR(I,12)  =  SPKNORATE(I,2)         
        UVAR(I,13)  =  SPKNORATE(I,3)         
        UVAR(I,14)  =  SPKNORATE(I,4)         
        UVAR(I,15)  =  SPKNORATE(I,5)         
        UVAR(I,16)  =  SPKNORATE(I,6)         

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
c     computes the global incremental stiffness matrix
c     at t(n+1/2) ( midpoint GL strains are used )

      CALL CONDAMAGE(ZXX ,ZYY  ,ZZZ ,ZXY      ,ZZX     ,ZYZ,
     .               DAMAGE ,NEL  ,HU  ,SHAPE   ,WHYSMAX,
     .               NUMTABL ,MATPARAM%TABLE,  NVARTMP, VARTMP)     
C
C     apply damage
C 
      DO I=1,NEL
        UVAR(I,17)    =  WHYSMAX(I) 
        UVAR(I,18)    =  ONE - DAMAGE(I)     
        DO IK=1,6
          SPK(I,IK)=SPKNODAM(I,IK)*(ONE-DAMAGE(I))
        ENDDO
C
C       transform 2PK to Cauchy stresses  
C
        S1(I) =SPK(I,1)                    
        S2(I) =SPK(I,2)                    
        S3(I) =SPK(I,3)                    
        S4(I) =SPK(I,4)                    
        S5(I) =SPK(I,5)                    
        S6(I) =SPK(I,6)                    
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
        SLOPEMAX(I)=MAX(SLOPE(I,1), SLOPE(I,2))
        SLOPEMAX(I)=MAX(SLOPEMAX(I), SLOPE(I,3))
        !SLOPEMAX(I)=MIN(SLOPEMAX(I), HUNDRED * EMOD)
        IF (SLOPEMAX(I) > EMOD) THEN
          GS =SLOPEMAX(I)*HALF
          KS =SLOPEMAX(I)*THIRD
        ENDIF
        SOUNDSP(I) = SQRT((GS*FOUR_OVER_3 + KS)/RHO(I))
        VISCMAX(I) = ZERO
      ENDDO
C-------------------------------------------------------------------------
      RETURN
      END
