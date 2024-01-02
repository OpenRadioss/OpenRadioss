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
Chd|        FINTER                        source/tools/curve/finter.F   
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
     8     ETSE    ,GS      ,YLD     ,INLOC   ,DPLANL  )
C-----------------------------------------------
      USE MATPARAM_DEF_MOD
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
C---------+---------+---+---+--------------------------------------------
C NFUNC   |  1      | I | R | NUMBER FUNCTION USED FOR THIS USER LAW
C IFUNC   | NFUNC   | I | R | FUNCTION INDEX 
C NPF     |  *      | I | R | FUNCTION ARRAY   
C TF      |  *      | F | R | FUNCTION ARRAY 
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C RHO0    | NEL     | F | R | INITIAL DENSITY
C THKLY   | NEL     | F | R | LAYER THICKNESS
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
C SOUNDSP | NEL     | F | W | SOUND SPEED (NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C THK     | NEL     | F |R/W| THICKNESS
C PLA     | NEL     | F |R/W| PLASTIC STRAIN
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: NEL,NFUNC,NUPARAM,NUVAR,INLOC
      INTEGER ,DIMENSION(NFUNC)  ,INTENT(IN)  :: IFUNC
      INTEGER :: NGL(NEL)
      my_real :: TIME,TIMESTEP
      my_real :: UPARAM(NUPARAM)
      my_real ,DIMENSION(NEL) :: RHO0,THKLY,GS,EPSP,
     .   DEPSXX,DEPSYY,DEPSXY,DEPSYZ,DEPSZX,
     .   SIGOXX,SIGOYY,SIGOXY,SIGOYZ,SIGOZX,DPLANL
      TYPE (MATPARAM_STRUCT_) , TARGET  :: MATPARAM
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real ,DIMENSION(NEL) ,INTENT(OUT) :: SOUNDSP,ETSE,PLA,
     .    SIGNXX,SIGNYY,SIGNXY,SIGNYZ,SIGNZX
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real :: UVAR(NEL,NUVAR), OFF(NEL),THK(NEL),YLD(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER  NPF(*)
      my_real  FINTER, TF(*)
      EXTERNAL FINTER
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IFORM,IQUAD,ICONV,NDIM,NPT,NINDX,NUMTABL,INDX(NEL)
      my_real :: E,DF,EPSPF,EPSPR,YFAC2
      my_real ,DIMENSION(NEL) :: DEZZ,DPLA,H,PLA_DAM
      TYPE(TABLE_4D_), DIMENSION(:) , POINTER :: TABLE
c-----------------------------------------
c     UVAR(1)  : EPSPT
c     UVAR(2)  : EPSPC
c     UVAR(3)  : EPSPS
c     UVAR(4)  : EPDT
c     UVAR(5)  : EPDC
c     UVAR(6)  : EPDS
c     UVAR(7)  : DAM
c     UVAR(8)  : DPLANL
C=======================================================================
c     PARAMETERS INITIALIZATION
c-----------------------------------------
      E      = UPARAM(1)                       
      EPSPF  = UPARAM(10)       !   Failure plastic strain           
      EPSPR  = UPARAM(11)       !   rupture plastic strain                  
      IFORM  = NINT(UPARAM(13))
      IQUAD  = NINT(UPARAM(14))
      YFAC2  = UPARAM(29)
      DPLA(1:NEL) = ZERO ! Initialization of the plastic strain increment
      ETSE(1:NEL) = ONE
      DO I = 1,NEL
        IF(OFF(I) < EM01) OFF(I)=ZERO
        IF(OFF(I) < ONE)   OFF(I)=OFF(I)*FOUR_OVER_5  
      ENDDO
c
      NUMTABL =  MATPARAM%NTABLE
      TABLE   => MATPARAM%TABLE(1:NUMTABL)
c--------------------------------------------------------------      
      IF (IFORM == 1) THEN  ! associated plastic flow, quadratic yld
        CALL ASSO_QPLAS76C(
     .       NEL     ,NUPARAM ,NUVAR   ,NFUNC   ,IFUNC   ,
     .       NPF     ,TF      ,NUMTABL ,TABLE   ,
     .       TIME    ,TIMESTEP,UPARAM  ,UVAR    ,RHO0    ,
     .       DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     .       SIGOXX  ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     .       SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     .       PLA     ,DPLA    ,EPSP    ,OFF     ,GS      ,
     .       YLD     ,SOUNDSP ,DEZZ    ,INLOC   ,DPLANL  )
c
      ELSE   ! IFORM = 0 => non associated plastic flow
        IF (IQUAD == 1) THEN    ! quadratic yld form
          CALL NO_ASSO_QPLAS76C(
     .       NEL     ,NUPARAM ,NUVAR   ,NFUNC   ,IFUNC   ,
     .       NPF     ,TF      ,NUMTABL ,TABLE   ,
     .       TIME    ,TIMESTEP,UPARAM  ,UVAR    ,RHO0    ,
     .       DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     .       SIGOXX  ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     .       SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     .       PLA     ,DPLA    ,EPSP    ,OFF     ,GS      ,
     .       YLD     ,SOUNDSP ,DEZZ    ,INLOC   ,DPLANL  )
        ELSE                 ! associated plastic flow
          CALL NO_ASSO_LPLAS76C(
     .       NEL     ,NUPARAM ,NUVAR   ,NFUNC   ,IFUNC   ,
     .       NPF     ,TF      ,NUMTABL ,TABLE   ,
     .       TIME    ,TIMESTEP,UPARAM  ,UVAR    ,RHO0    ,
     .       DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     .       SIGOXX  ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     .       SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     .       PLA     ,DPLA    ,EPSP    ,OFF     ,GS      ,
     .       YLD     ,SOUNDSP ,DEZZ    ,INLOC   ,DPLANL  )
        ENDIF
      ENDIF    ! IFORM
c--------------------------------------------------------------      
c     update damage
c--------------------------------------------------------------
      NINDX = 0      
      IF (INLOC > 0) THEN 
        PLA_DAM(1:NEL) = UVAR(1:NEL,8)
      ELSE
        PLA_DAM(1:NEL) = PLA(1:NEL)
      ENDIF
      IF (IFUNC(2) > 0) THEN
        DO I=1,NEL
          UVAR(I,7) = ABS(YFAC2)* FINTER(IFUNC(2),PLA_DAM(I),NPF,TF,DF)
          UVAR(I,7) = MIN(UVAR(I,7),ONE)
          IF (UVAR(I,7) >= ONE) THEN 
            IF (OFF(I) == ONE) THEN 
              OFF(I) = FOUR_OVER_5
              IDEL7NOK    = 1
              NINDX       = NINDX+1
              INDX(NINDX) = I
            ENDIF
            UVAR(I,7) = ONE
            SIGNXX(I) = ZERO
            SIGNYY(I) = ZERO
            SIGNXY(I) = ZERO
            SIGNYZ(I) = ZERO
            SIGNZX(I) = ZERO
          ENDIF 
        ENDDO 
      ELSE
        DO I=1,NEL
          IF (PLA_DAM(I) >= EPSPR) THEN
            IF (OFF(I) == ONE) THEN 
              OFF(I)      = FOUR_OVER_5
              IDEL7NOK    = 1
              NINDX       = NINDX+1
              INDX(NINDX) = I
            ENDIF
            UVAR(I,7)   = ONE
            SIGNXX(I)   = ZERO
            SIGNYY(I)   = ZERO
            SIGNXY(I)   = ZERO
            SIGNYZ(I)   = ZERO
            SIGNZX(I)   = ZERO
          ELSEIF (PLA_DAM(I) >= EPSPF) THEN  
            UVAR(I,7) = (PLA_DAM(I) - EPSPF)/ (EPSPR - EPSPF)
          ENDIF
        ENDDO
      ENDIF
C-------------------------------------------------------------------------------
      DO I=1,NEL
c       IF(DPLA(I) > ZERO) ETSE(I)= AA1(1) / (AA1(I) + E)
        IF (DPLA(I) > ZERO) THEN 
          ETSE(I) = HALF   !H(I) / (H(I)+E)!HALF   ! ?????
        ENDIF
        THK(I) = THK(I) + DEZZ(I)*THKLY(I)*OFF(I) 
      ENDDO  
c-----------     
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
