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
Chd|  SIGEPS50                      source/materials/mat/mat050/sigeps50.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        MULAW8                        source/materials/mat_share/mulaw8.F
Chd|-- calls ---------------
Chd|        TABLE_MAT_VINTERP             source/materials/tools/table_mat_vinterp.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        TABLE_MAT_VINTERP_MOD         source/materials/tools/table_mat_vinterp.F
Chd|====================================================================
      SUBROUTINE SIGEPS50 (
     1     NEL    ,NUPARAM,NUVAR   ,NFUNC   ,IFUNC   ,NPF    ,
     2     TF     ,TIME   ,TIMESTEP,UPARAM  ,RHO0    ,RHO    ,
     3     VOLUME ,EINT   ,NVARTMP ,VARTMP  ,
     4     EPSPXX ,EPSPYY ,EPSPZZ  ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
     5     DEPSXX ,DEPSYY ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX ,
     6     EPSXX  ,EPSYY  ,EPSZZ   ,EPSXY   ,EPSYZ   ,EPSZX  ,
     7     SIGOXX ,SIGOYY ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX ,
     8     SIGNXX ,SIGNYY ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     9     SIGVXX ,SIGVYY ,SIGVZZ  ,SIGVXY  ,SIGVYZ  ,SIGVZX ,
     A     SOUNDSP,VISCMAX,UVAR    ,OFF     ,AMU     ,MAT_PARAM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MATPARAM_DEF_MOD
      USE TABLE_MAT_VINTERP_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
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
C DEPSXX  | NEL     | F | R | STRAIN INCREMENT XX
C DEPSYY  | NEL     | F | R | STRAIN INCREMENT YY
C ...     |         |   |   |
C EPSXX   | NEL     | F | R | STRAIN XX
C EPSYY   | NEL     | F | R | STRAIN YY
C ...     |         |   |   |
C SIGOXX  | NEL     | F | R | OLD ELASTO PLASTIC STRESS XX 
C SIGOYY  | NEL     | F | R | OLD ELASTO PLASTIC STRESS YY
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
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NVARTMP
      INTEGER NEL, NUPARAM, NUVAR
      my_real TIME,TIMESTEP,UPARAM(*),
     .   RHO(NEL),RHO0(NEL),VOLUME(NEL),EINT(NEL),
     .   EPSPXX(NEL),EPSPYY(NEL),EPSPZZ(NEL),
     .   EPSPXY(NEL),EPSPYZ(NEL),EPSPZX(NEL),
     .   DEPSXX(NEL),DEPSYY(NEL),DEPSZZ(NEL),
     .   DEPSXY(NEL),DEPSYZ(NEL),DEPSZX(NEL),
     .   EPSXX(NEL) ,EPSYY(NEL) ,EPSZZ(NEL) ,
     .   EPSXY(NEL) ,EPSYZ(NEL) ,EPSZX(NEL) ,
     .   SIGOXX(NEL),SIGOYY(NEL),SIGOZZ(NEL),
     .   SIGOXY(NEL),SIGOYZ(NEL),SIGOZX(NEL),
     .   AMU(NEL)
      TYPE(MATPARAM_STRUCT_) ,INTENT(IN) :: MAT_PARAM
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real
     .    SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .    SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .    SIGVXX(NEL),SIGVYY(NEL),SIGVZZ(NEL),
     .    SIGVXY(NEL),SIGVYZ(NEL),SIGVZX(NEL),
     .    SOUNDSP(NEL),VISCMAX(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      INTEGER ,INTENT(INOUT) :: VARTMP(NEL,NVARTMP) ! last interpolation positions in function tables
      my_real UVAR(NEL,NUVAR), OFF(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*), NFUNC, IFUNC(NFUNC)
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
      INTEGER I,IF1,IF2
      INTEGER N11,N22,N33,N12,N23,N31
      INTEGER, DIMENSION(NEL,2) :: IPOS2
      my_real E11,E22,E33,G12,G23,G31,EMX11,EMX22,EMX33,EMX12,EMX23,EMX31,
     .        MUOLD,DMUDT,ASRATE,DMUDTF
      my_real ,DIMENSION(NEL) :: DEP1,DEP2,DEP3,DEP4,DEP5,DEP6,DYDX,
     .                           EP1,EP2,EP3,EP4,EP5,EP6,SIG
      my_real, DIMENSION(NEL,1) :: XVEC1
      my_real, DIMENSION(NEL,2) :: XVEC2
C=======================================================================
      E11 = UPARAM(1)
      E22 = UPARAM(2)
      E33 = UPARAM(3)
      G12 = UPARAM(4)
      G23 = UPARAM(5)
      G31 = UPARAM(6)
      IF1 = NINT(UPARAM(7))
      IF2 = NINT(UPARAM(8))
C
      N11 = NINT(UPARAM(45))
      N22 = NINT(UPARAM(46))
      N33 = NINT(UPARAM(47))
      N12 = NINT(UPARAM(48))
      N23 = NINT(UPARAM(49))
      N31 = NINT(UPARAM(50))
      ASRATE=MIN(ONE,UPARAM(51)*TIMESTEP)
C
      EMX11 = UPARAM(9)
      EMX22 = UPARAM(10)
      EMX33 = UPARAM(11)
      EMX12 = UPARAM(12)
      EMX23 = UPARAM(13)
      EMX31 = UPARAM(14)
      DO I=1,NEL
        SIGNXX(I) = SIGOXX(I) + E11 * DEPSXX(I)
        SIGNYY(I) = SIGOYY(I) + E22 * DEPSYY(I)
        SIGNZZ(I) = SIGOZZ(I) + E33 * DEPSZZ(I)
        SIGNXY(I) = SIGOXY(I) + G12 * DEPSXY(I)
        SIGNYZ(I) = SIGOYZ(I) + G23 * DEPSYZ(I)
        SIGNZX(I) = SIGOZX(I) + G31 * DEPSZX(I)
        SOUNDSP(I) = SQRT(MAX(E11,E22,E33,G12,G23,G31)/RHO(I))
        VISCMAX(I) = ZERO
      ENDDO
C
      DO I=1,NEL
          IF(EPSXX(I)>EMX11 .OR. EPSYY(I)>EMX22 .OR. EPSZZ(I)>EMX33.OR.
     .       ABS(EPSXY(I)/TWO)>EMX12.OR.
     .       ABS(EPSYZ(I)/TWO)>EMX23.OR.
     .       ABS(EPSZX(I)/TWO)>EMX31) OFF(I) = ZERO
      ENDDO
c
      DO I=1,NEL
       MUOLD=UVAR(I,1)
       DMUDTF=UVAR(I,2)
c       AMU = RHO(I)/RHO0(I) - ONE
       DMUDT=(AMU(I)-MUOLD)/MAX(TIMESTEP,EM20)
       DMUDTF=ASRATE*DMUDT+(1.-ASRATE)*DMUDTF
       UVAR(I,1)=AMU(I)
       UVAR(I,2)=DMUDTF
       DMUDT=ABS(DMUDTF)
       EP1(I) = AMU(I)
       EP2(I) = AMU(I)
       EP3(I) = AMU(I)
       EP4(I) = AMU(I)
       EP5(I) = AMU(I)
       EP6(I) = AMU(I)
      ENDDO
c
      IF(IF1==1)THEN
       DO I=1,NEL
           EP1(I) = EPSXX(I)
           EP2(I) = EPSYY(I)
           EP3(I) = EPSZZ(I)
       ENDDO
      ELSEIF(IF1==-1)THEN
       DO I=1,NEL
           EP1(I) = -EPSXX(I)
           EP2(I) = -EPSYY(I)
           EP3(I) = -EPSZZ(I)
       ENDDO
      ENDIF
      IF(IF2==1)THEN
       DO I=1,NEL
           EP4(I) = EPSXY(I)
           EP5(I) = EPSYZ(I)
           EP6(I) = EPSZX(I)
        ENDDO
      ELSEIF(IF2==-1)THEN
       DO I=1,NEL
           EP4(I) = -EPSXY(I)
           EP5(I) = -EPSYZ(I)
           EP6(I) = -EPSZX(I)
       ENDDO
      ENDIF
      DO I=1,NEL
           UVAR(I,5) = ASRATE*EPSPXX(I)+(ONE -ASRATE)*UVAR(I,5)
           DEP1(I) = ABS(UVAR(I,5))
           UVAR(I,6) = ASRATE*EPSPYY(I)+(ONE -ASRATE)*UVAR(I,6)
           DEP2(I) = ABS(UVAR(I,6))
           UVAR(I,7) = ASRATE*EPSPZZ(I)+(ONE -ASRATE)*UVAR(I,7)
           DEP3(I) = ABS(UVAR(I,7))
           UVAR(I,8) = ASRATE*EPSPXY(I)+(ONE -ASRATE)*UVAR(I,8)
           DEP4(I) = ABS( UVAR(I,8))
           UVAR(I,9) = ASRATE*EPSPYZ(I)+(ONE -ASRATE)*UVAR(I,9)
           DEP5(I) = ABS(UVAR(I,9))
           UVAR(I,10) = ASRATE*EPSPZX(I)+(ONE -ASRATE)*UVAR(I,10)
           DEP6(I) = ABS(UVAR(I,10))
      ENDDO
C
      DO I=1,NEL
       UVAR(I,3) = HALF*(EPSXX(I)**2 + EPSYY(I)**2 + EPSZZ(I)**2)
     .           +       EPSXY(I)**2 + EPSYZ(I)**2 + EPSZX(I)**2
       UVAR(I,3) = SQRT(THREE*UVAR(I,3))
       UVAR(I,4) = HALF*(UVAR(I,5)**2+UVAR(I,6)**2+UVAR(I,7)**2) +
     .                 UVAR(I,8)**2+UVAR(I,9)**2+UVAR(I,10)**2
       UVAR(I,4) = SQRT(THREE*UVAR(I,4))/THREE_HALF
      ENDDO
!
!     table interpolations in each direction
!      
      !  direction xx : table(1)    
      IF (N11 > 0) THEN
        IF (MAT_PARAM%TABLE(1)%NDIM == 1) THEN
          XVEC1(1:NEL,1)   = EP1(1:NEL)
          CALL TABLE_MAT_VINTERP(MAT_PARAM%TABLE(1),NEL,NEL,VARTMP(1,1),XVEC1,SIG,DYDX) 
        ELSE   ! NDIM = 2
          XVEC2(1:NEL,1)  = EP1(1:NEL)
          XVEC2(1:NEL,2)  = DEP1(1:NEL)
          IPOS2(1:NEL,1)  = VARTMP(1:NEL,1)
          IPOS2(1:NEL,2)  = VARTMP(1:NEL,2)         
          CALL TABLE_MAT_VINTERP(MAT_PARAM%TABLE(1),NEL,NEL,IPOS2,XVEC2,SIG,DYDX)
          VARTMP(1:NEL,1) = IPOS2(1:NEL,1)
          VARTMP(1:NEL,2) = IPOS2(1:NEL,2)
        END IF
        SIGNXX(1:NEL) = SIGN(MIN(ABS(SIGNXX(1:NEL)),SIG(1:NEL)),SIGNXX(1:NEL))
      ENDIF
      !  direction yy : table(2)  
      IF (N22 > 0) THEN
        IF (MAT_PARAM%TABLE(2)%NDIM == 1) THEN
          XVEC1(1:NEL,1)   = EP2(1:NEL)
          CALL TABLE_MAT_VINTERP(MAT_PARAM%TABLE(2),NEL,NEL,VARTMP(1,3),XVEC1,SIG,DYDX) 
        ELSE   ! NDIM = 2
          XVEC2(1:NEL,1)  = EP2(1:NEL)
          XVEC2(1:NEL,2)  = DEP2(1:NEL)
          IPOS2(1:NEL,1)  = VARTMP(1:NEL,3)
          IPOS2(1:NEL,2)  = VARTMP(1:NEL,4)         
          CALL TABLE_MAT_VINTERP(MAT_PARAM%TABLE(2),NEL,NEL,IPOS2,XVEC2,SIG,DYDX)
          VARTMP(1:NEL,3) = IPOS2(1:NEL,1)
          VARTMP(1:NEL,4) = IPOS2(1:NEL,2)
        END IF
        SIGNYY(1:NEL) = SIGN(MIN(ABS(SIGNYY(1:NEL)),SIG(1:NEL)),SIGNYY(1:NEL))
      ENDIF
      !  direction zz : table(3)  
      IF (N33 > 0) THEN
        IF (MAT_PARAM%TABLE(3)%NDIM == 1) THEN
          XVEC1(1:NEL,1)   = EP3(1:NEL)
          CALL TABLE_MAT_VINTERP(MAT_PARAM%TABLE(3),NEL,NEL,VARTMP(1,5),XVEC1,SIG,DYDX) 
        ELSE   ! NDIM = 2
          XVEC2(1:NEL,1)  = EP3(1:NEL)
          XVEC2(1:NEL,2)  = DEP3(1:NEL)
          IPOS2(1:NEL,1)  = VARTMP(1:NEL,5)
          IPOS2(1:NEL,2)  = VARTMP(1:NEL,6)         
          CALL TABLE_MAT_VINTERP(MAT_PARAM%TABLE(3),NEL,NEL,IPOS2,XVEC2,SIG,DYDX)
          VARTMP(1:NEL,5) = IPOS2(1:NEL,1)
          VARTMP(1:NEL,6) = IPOS2(1:NEL,2)
        END IF
        SIGNZZ(1:NEL) = SIGN(MIN(ABS(SIGNZZ(1:NEL)),SIG(1:NEL)),SIGNZZ(1:NEL))
      ENDIF
      !  direction xy : table(4)  
      IF (N12 > 0) THEN
        IF (MAT_PARAM%TABLE(3)%NDIM == 1) THEN
          XVEC1(1:NEL,1)   = EP4(1:NEL)
          CALL TABLE_MAT_VINTERP(MAT_PARAM%TABLE(4),NEL,NEL,VARTMP(1,7),XVEC1,SIG,DYDX) 
        ELSE   ! NDIM = 2
          XVEC2(1:NEL,1)  = EP4(1:NEL)
          XVEC2(1:NEL,2)  = DEP4(1:NEL)
          IPOS2(1:NEL,1)  = VARTMP(1:NEL,7)
          IPOS2(1:NEL,2)  = VARTMP(1:NEL,8)         
          CALL TABLE_MAT_VINTERP(MAT_PARAM%TABLE(4),NEL,NEL,IPOS2,XVEC2,SIG,DYDX)
          VARTMP(1:NEL,7) = IPOS2(1:NEL,1)
          VARTMP(1:NEL,8) = IPOS2(1:NEL,2)
        END IF
        SIGNXY(1:NEL) = SIGN(MIN(ABS(SIGNXY(1:NEL)),SIG(1:NEL)),SIGNXY(1:NEL))
      ENDIF
      !  direction yz : table(5)  
      IF (N23 > 0) THEN
        IF (MAT_PARAM%TABLE(5)%NDIM == 1) THEN
          XVEC1(1:NEL,1)   = EP5(1:NEL)
          CALL TABLE_MAT_VINTERP(MAT_PARAM%TABLE(5),NEL,NEL,VARTMP(1,9),XVEC1,SIG,DYDX) 
        ELSE   ! NDIM = 2
          XVEC2(1:NEL,1)  = EP5(1:NEL)
          XVEC2(1:NEL,2)  = DEP5(1:NEL)
          IPOS2(1:NEL,1)  = VARTMP(1:NEL,9)
          IPOS2(1:NEL,2)  = VARTMP(1:NEL,10)         
          CALL TABLE_MAT_VINTERP(MAT_PARAM%TABLE(5),NEL,NEL,IPOS2,XVEC2,SIG,DYDX)
          VARTMP(1:NEL,9)  = IPOS2(1:NEL,1)
          VARTMP(1:NEL,10) = IPOS2(1:NEL,2)
        END IF
        SIGNYZ(1:NEL) = SIGN(MIN(ABS(SIGNYZ(1:NEL)),SIG(1:NEL)),SIGNYZ(1:NEL))
      ENDIF
      !  direction zz : TABLE(6)  
      IF (N31 > 0) THEN
        IF (MAT_PARAM%TABLE(6)%NDIM == 1) THEN
          XVEC1(1:NEL,1)   = EP6(1:NEL)
          CALL TABLE_MAT_VINTERP(MAT_PARAM%TABLE(6),NEL,NEL,VARTMP(1,11),XVEC1,SIG,DYDX) 
        ELSE   ! NDIM = 2
          XVEC2(1:NEL,1)  = EP6(1:NEL)
          XVEC2(1:NEL,2)  = DEP6(1:NEL)
          IPOS2(1:NEL,1)  = VARTMP(1:NEL,11)
          IPOS2(1:NEL,2)  = VARTMP(1:NEL,12)         
          CALL TABLE_MAT_VINTERP(MAT_PARAM%TABLE(6),NEL,NEL,IPOS2,XVEC2,SIG,DYDX)
          VARTMP(1:NEL,11) = IPOS2(1:NEL,1)
          VARTMP(1:NEL,12) = IPOS2(1:NEL,2)
        END IF
        SIGNZX(1:NEL) = SIGN(MIN(ABS(SIGNZX(1:NEL)),SIG(1:NEL)),SIGNZX(1:NEL))
      ENDIF
C-----------
      RETURN
      END
