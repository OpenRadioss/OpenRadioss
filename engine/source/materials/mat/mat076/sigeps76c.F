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
Chd|  SIGEPS76C                     source/materials/mat/mat076/sigeps76c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|        ASSO_QPLAS76C                 source/materials/mat/mat076/asso_qplas76c.F
Chd|        NO_ASSO_LPLAS76C              source/materials/mat/mat076/no_asso_lplas76c.F
Chd|        NO_ASSO_QPLAS76C              source/materials/mat/mat076/no_asso_qplas76c.F
Chd|        VINTER                        source/tools/curve/vinter.F   
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|====================================================================
      SUBROUTINE SIGEPS76C(
     1     NEL     ,NUPARAM ,NUVAR   ,NFUNC   ,IFUNC   ,
     2     NPF     ,TF      ,MATPARAM,TIME    ,TIMESTEP,
     3     UPARAM  ,UVAR    ,RHO0    ,OFF     ,NGL     ,
     4     DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5     SIGOXX  ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     6     SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     7     SOUNDSP ,THK     ,THKLY   ,PLA     ,EPSP    ,
     8     ETSE    ,GS      ,YLD     ,INLOC   ,L_PLANL ,
     9     PLANL   ,DPLANL  ,DMG     ,IDEL7NOK,
     A     NVARTMP ,VARTMP  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MATPARAM_DEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "tabsiz_c.inc"
#include      "comlock.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN)    :: NEL,NFUNC,NUPARAM,NUVAR,INLOC,L_PLANL
      INTEGER ,INTENT(INOUT) :: IDEL7NOK
      INTEGER ,INTENT(IN)    :: NVARTMP
      INTEGER ,DIMENSION(NEL,NVARTMP) ,INTENT(INOUT) :: VARTMP
      INTEGER ,DIMENSION(NFUNC), INTENT(IN) :: IFUNC
      INTEGER ,DIMENSION(NEL)  , INTENT(IN) :: NGL
      my_real ,INTENT(IN) :: TIME,TIMESTEP
      my_real ,DIMENSION(NUPARAM), INTENT(IN) :: UPARAM
      my_real ,DIMENSION(NEL), INTENT(IN) :: RHO0,THKLY,GS,
     .   DEPSXX,DEPSYY,DEPSXY,DEPSYZ,DEPSZX,
     .   SIGOXX,SIGOYY,SIGOXY,SIGOYZ,SIGOZX,DPLANL
      my_real, DIMENSION(L_PLANL*NEL), INTENT(IN) :: PLANL
      my_real, DIMENSION(NEL), INTENT(INOUT) :: 
     .   EPSP,PLA,SIGNXX,SIGNYY,SIGNXY,SIGNYZ,SIGNZX,SOUNDSP,
     .   ETSE,OFF,YLD,THK,DMG
      my_real, DIMENSION(NEL,NUVAR), INTENT(INOUT) :: UVAR
      TYPE(MATPARAM_STRUCT_), INTENT(IN), TARGET :: MATPARAM
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER :: NPF(SNPC)
      my_real :: TF(STF)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,IFORM,IQUAD,NUMTABL,NINDX
      INTEGER ,DIMENSION(NEL)   :: INDX,IAD,ILEN
      my_real :: DF,EPSPF,EPSPR,YFAC2
      my_real ,DIMENSION(NEL) :: DEZZ,DPLA,PLA_DAM,
     .  SIG0XX,SIG0YY,SIG0XY,SIG0YZ,SIG0ZX
      TYPE(TABLE_4D_), DIMENSION(:), POINTER :: TABLE
C=======================================================================
c
      !====================================================================
      ! - PARAMETERS INITIALIZATION
      !====================================================================
      EPSPF = UPARAM(10)       !   Failure plastic strain           
      EPSPR = UPARAM(11)       !   Rupture plastic strain                  
      IFORM = NINT(UPARAM(13)) ! Flag for plasticity 
                               !   = 0 associated (with quadratic yield criterion only)
                               !   = 1 not associated     
      IQUAD = NINT(UPARAM(14)) ! Flag for quadratic yield criterion
      YFAC2 = UPARAM(29)       ! Damage function scale factor
      DPLA(1:NEL) = ZERO       ! Initialization of the plastic strain increment
      ETSE(1:NEL) = ONE
      DO I = 1,NEL
        IF (OFF(I) < EM01) OFF(I) = ZERO
        IF (OFF(I) <  ONE) OFF(I) = OFF(I)*FOUR_OVER_5  
      ENDDO
c
      ! Table parameters
      NUMTABL =  MATPARAM%NTABLE
      TABLE   => MATPARAM%TABLE(1:NUMTABL)
c
      !====================================================================
      ! - COMPUTE INITIAL UNDAMAGED STRESSES
      !====================================================================
      DO I = 1,NEL 
        SIG0XX(I) = SIGOXX(I)/MAX(ONE-DMG(I),EM20)
        SIG0YY(I) = SIGOYY(I)/MAX(ONE-DMG(I),EM20)
        SIG0XY(I) = SIGOXY(I)/MAX(ONE-DMG(I),EM20)
        SIG0YZ(I) = SIGOYZ(I)/MAX(ONE-DMG(I),EM20)
        SIG0ZX(I) = SIGOZX(I)/MAX(ONE-DMG(I),EM20)
      ENDDO
c
      !====================================================================
      ! - ELASTO-PLASTIC BEHAVIOR
      !====================================================================  
      IF (IFORM == 1) THEN  ! Associated plastic flow, Quadratic yield criterion
        CALL ASSO_QPLAS76C(
     .       NEL     ,NUPARAM ,NUVAR   ,NFUNC   ,IFUNC   ,
     .       NPF     ,TF      ,NUMTABL ,TABLE   ,
     .       TIME    ,TIMESTEP,UPARAM  ,UVAR    ,RHO0    ,
     .       DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     .       SIG0XX  ,SIG0YY  ,SIG0XY  ,SIG0YZ  ,SIG0ZX  ,
     .       SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     .       PLA     ,DPLA    ,EPSP    ,OFF     ,GS      ,
     .       YLD     ,SOUNDSP ,DEZZ    ,INLOC   ,DPLANL  ,
     .       NVARTMP ,VARTMP  )
c
      ELSE   ! Non-associated plastic flow
        IF (IQUAD == 1) THEN ! Quadratic yield criterion
          CALL NO_ASSO_QPLAS76C(
     .       NEL     ,NUPARAM ,NUVAR   ,NFUNC   ,IFUNC   ,
     .       NPF     ,TF      ,NUMTABL ,TABLE   ,
     .       TIME    ,TIMESTEP,UPARAM  ,UVAR    ,RHO0    ,
     .       DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     .       SIG0XX  ,SIG0YY  ,SIG0XY  ,SIG0YZ  ,SIG0ZX  ,
     .       SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     .       PLA     ,DPLA    ,EPSP    ,OFF     ,GS      ,
     .       YLD     ,SOUNDSP ,DEZZ    ,INLOC   ,DPLANL  ,
     .       NVARTMP ,VARTMP  )
        ELSE                 ! Non-quadratic yield criterion
          CALL NO_ASSO_LPLAS76C(
     .       NEL     ,NUPARAM ,NUVAR   ,NFUNC   ,IFUNC   ,
     .       NPF     ,TF      ,NUMTABL ,TABLE   ,
     .       TIME    ,TIMESTEP,UPARAM  ,UVAR    ,RHO0    ,
     .       DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     .       SIG0XX  ,SIG0YY  ,SIG0XY  ,SIG0YZ  ,SIG0ZX  ,
     .       SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     .       PLA     ,DPLA    ,EPSP    ,OFF     ,GS      ,
     .       YLD     ,SOUNDSP ,DEZZ    ,INLOC   ,DPLANL  ,
     .       NVARTMP ,VARTMP  )
        ENDIF
      ENDIF
c
      !====================================================================
      ! - UPDATE DAMAGE VARIABLE
      !====================================================================
      NINDX = 0      
      IF (INLOC > 0) THEN 
        PLA_DAM(1:NEL) = PLANL(1:NEL)
      ELSE
        PLA_DAM(1:NEL) = PLA(1:NEL)
      ENDIF
!
      IF (IFUNC(2) > 0) THEN
        ! Tabulated damage
        IAD(1:NEL)  = NPF(IFUNC(2))   / 2 + 1
        ILEN(1:NEL) = NPF(IFUNC(2)+1) / 2 - IAD(1:NEL) - VARTMP(1:NEL,7)
!
        CALL VINTER(TF,IAD,VARTMP(1:NEL,7),ILEN,NEL,PLA_DAM,DF,DMG)
!        
        DMG(1:NEL) = ABS(YFAC2)*DMG(1:NEL)
        DO I=1,NEL
          IF (DMG(I) >= ONE) THEN 
            DMG(I) = ONE
            IF (OFF(I) == ONE) THEN 
              OFF(I) = FOUR_OVER_5
              IDEL7NOK    = 1
              NINDX       = NINDX+1
              INDX(NINDX) = I
            ENDIF
          ENDIF 
        ENDDO 
      ! Analytical damage
      ELSE
        DO I=1,NEL
          IF (PLA_DAM(I) >= EPSPF) THEN 
            DMG(I) = (PLA_DAM(I) - EPSPF)/ (EPSPR - EPSPF)
            DMG(I) = MIN(DMG(I),ONE)
          ENDIF
          IF (DMG(I) >= ONE) THEN 
            IF (OFF(I) == ONE) THEN 
              OFF(I) = FOUR_OVER_5
              IDEL7NOK    = 1
              NINDX       = NINDX+1
              INDX(NINDX) = I
            ENDIF
          ENDIF
        ENDDO
      ENDIF
c
      !====================================================================
      ! - COMPUTE DAMAGED STRESSES
      !====================================================================
      DO I = 1,NEL 
        SIGNXX(I) = SIGNXX(I)*(ONE-DMG(I))
        SIGNYY(I) = SIGNYY(I)*(ONE-DMG(I))
        SIGNXY(I) = SIGNXY(I)*(ONE-DMG(I))
        SIGNYZ(I) = SIGNYZ(I)*(ONE-DMG(I))
        SIGNZX(I) = SIGNZX(I)*(ONE-DMG(I))
      ENDDO
c  
      !====================================================================
      ! - HOURGLASS COEFFICIENT AND THICKNESS UPDATE
      !====================================================================  
      DO I=1,NEL
        IF (DPLA(I) > ZERO) THEN 
          ETSE(I) = HALF
        ENDIF
        THK(I) = THK(I) + DEZZ(I)*THKLY(I)*OFF(I) 
      ENDDO  
c  
      !====================================================================
      ! - PRINTOUT ELEMENT DELETION
      !====================================================================    
      IF (NINDX > 0) THEN
        DO I=1,NINDX
#include "lockon.inc"
          WRITE(IOUT, 1000) NGL(INDX(I))
          WRITE(ISTDO,1100) NGL(INDX(I)),TIME
#include "lockoff.inc"
        ENDDO
      ENDIF
c------------------------------------------------------      
 1000 FORMAT(1X,'RUPTURE (SAMP) OF SHELL ELEMENT NUMBER ',I10)
 1100 FORMAT(1X,'RUPTURE (SAMP) OF SHELL ELEMENT NUMBER ',I10,'AT TIME :',G11.4)  
c------------------------------------------------------      
      END
