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
!||    sigeps110c               ../engine/source/materials/mat/mat110/sigeps110c.F
!||--- called by ------------------------------------------------------
!||    mulawc                   ../engine/source/materials/mat_share/mulawc.F90
!||--- calls      -----------------------------------------------------
!||    sigeps110c_lite_newton   ../engine/source/materials/mat/mat110/sigeps110c_lite_newton.F
!||    sigeps110c_lite_nice     ../engine/source/materials/mat/mat110/sigeps110c_lite_nice.F
!||    sigeps110c_newton        ../engine/source/materials/mat/mat110/sigeps110c_newton.F
!||    sigeps110c_nice          ../engine/source/materials/mat/mat110/sigeps110c_nice.F
!||--- uses       -----------------------------------------------------
!||    interface_table_mod      ../engine/share/modules/table_mod.F
!||    table_mod                ../engine/share/modules/table_mod.F
!||====================================================================
      SUBROUTINE SIGEPS110C(
     1     NEL     ,NGL     ,NUPARAM ,NUVAR   ,NPF     , 
     2     TIME    ,TIMESTEP,UPARAM  ,UVAR    ,JTHE    ,OFF     ,
     3     GS      ,RHO     ,PLA     ,DPLA    ,EPSP    ,SOUNDSP ,
     4     DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,ASRATE  ,
     5     EPSPXX  ,EPSPYY  ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
     5     SIGOXX  ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     6     SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,THKLY   ,
     7     THK     ,SIGY    ,ET      ,TEMPEL  ,TEMP    ,SEQ     ,
     8     TF      ,NUMTABL ,ITABLE  ,TABLE   ,NVARTMP ,VARTMP  ,
     9     SIGA    ,INLOC   ,DPLANL  ,LOFF    ,IOFF_DUCT)
      !=======================================================================
      !      Modules
      !=======================================================================
      USE TABLE_MOD
      USE INTERFACE_TABLE_MOD
      !=======================================================================
      !      Implicit types
      !=======================================================================
#include      "implicit_f.inc"
      !=======================================================================
      !      Common
      !=======================================================================
#include      "com04_c.inc"
      !=======================================================================
      !      Dummy arguments
      !=======================================================================
      INTEGER NEL,NUPARAM,NUVAR,JTHE,NUMTABL,ITABLE(NUMTABL),NVARTMP,NPF(*),INLOC
      INTEGER,DIMENSION(NEL), INTENT(IN)    :: NGL
      INTEGER,DIMENSION(NEL), INTENT(INOUT) :: IOFF_DUCT
      my_real 
     .   TIME,TIMESTEP,ASRATE,TF(*)
      INTEGER :: VARTMP(NEL,NVARTMP)
      my_real,DIMENSION(NUPARAM), INTENT(IN) :: 
     .   UPARAM
      my_real,DIMENSION(NEL), INTENT(IN)     :: 
     .   RHO,TEMPEL,
     .   DEPSXX,DEPSYY,DEPSXY,DEPSYZ,DEPSZX,
     .   EPSPXX,EPSPYY,EPSPXY,EPSPYZ,EPSPZX ,
     .   SIGOXX,SIGOYY,SIGOXY,SIGOYZ,SIGOZX,
     .   GS,THKLY,DPLANL,LOFF
c
      my_real ,DIMENSION(NEL), INTENT(OUT)   :: 
     .   SOUNDSP,SIGY,ET,EPSP,
     .   SIGNXX,SIGNYY,SIGNXY,SIGNYZ,SIGNZX
c
      my_real ,DIMENSION(NEL), INTENT(INOUT)       :: 
     .   PLA,DPLA,OFF,THK,TEMP,SEQ
      my_real ,DIMENSION(NEL,NUVAR), INTENT(INOUT) :: 
     .   UVAR
      my_real ,DIMENSION(NEL,3)     ,INTENT(INOUT) :: 
     .   SIGA
      TYPE(TTABLE), DIMENSION(NTABLE) ::  TABLE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ICRIT,IRES
C=======================================================================
c      UVAR(1)     
c      UVAR(2)     
C      DEPIJ = PLASTIC STRAIN TENSOR COMPONENT
C      DEPSIJ = TOTAL STRAIN  TENSOR COMPONENT (EL+PL)
C=======================================================================
c  
       ICRIT   = NINT(UPARAM(27))! Vegter formulation flag
                                 !  = 1 => Vegter classical
                                 !  = 2 => Vegter standard
                                 !  = 3 => Vegter 2017
                                 !  = 4 => Vegter-Lite
       IRES    = NINT(UPARAM(29))! Plastic projection method
                                 !  = 1 => Nice method
                                 !  = 2 => Newton-iteration method
c--------------------------
      ! Choice of return mapping algorithm
      SELECT CASE (IRES)
        ! Nice explicit return mapping      
        CASE(1)
          ! Choice of Vegter formulation
          SELECT CASE (ICRIT)
            CASE(1,2,3)
              CALL SIGEPS110C_NICE(
     1           NEL     ,NGL     ,NUPARAM ,NUVAR   ,NPF      ,
     2           TIME    ,TIMESTEP,UPARAM  ,UVAR    ,JTHE     ,OFF     ,
     3           GS      ,RHO     ,PLA     ,DPLA    ,EPSP     ,SOUNDSP ,
     4           DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX   ,ASRATE  ,
     5           EPSPXX  ,EPSPYY  ,EPSPXY  ,EPSPYZ  ,EPSPZX   ,
     6           SIGOXX  ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX   ,
     7           SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX   ,THKLY   ,
     8           THK     ,SIGY    ,ET      ,TEMPEL  ,TEMP     ,SEQ     ,
     9           TF      ,NUMTABL ,ITABLE  ,TABLE   ,NVARTMP  ,VARTMP  ,
     A           SIGA    ,INLOC   ,DPLANL  ,LOFF    )
            CASE(4)
              CALL SIGEPS110C_LITE_NICE(
     1           NEL     ,NGL     ,NUPARAM ,NUVAR   ,NPF      ,
     2           TIME    ,TIMESTEP,UPARAM  ,UVAR    ,JTHE     ,OFF     ,
     3           GS      ,RHO     ,PLA     ,DPLA    ,EPSP     ,SOUNDSP ,
     4           DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX   ,ASRATE  ,
     5           EPSPXX  ,EPSPYY  ,EPSPXY  ,EPSPYZ  ,EPSPZX   ,
     6           SIGOXX  ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX   ,
     7           SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX   ,THKLY  ,
     8           THK     ,SIGY    ,ET      ,TEMPEL  ,TEMP     ,SEQ    ,
     9           TF      ,NUMTABL ,ITABLE  ,TABLE   ,NVARTMP  ,VARTMP ,
     A           SIGA    ,INLOC   ,DPLANL  ,LOFF    )  
          END SELECT   
        ! Newton cutting plane return mapping
        CASE(2)
          ! Choice of Vegter formulation
          SELECT CASE (ICRIT)
            CASE(1,2,3)
              CALL SIGEPS110C_NEWTON(
     1           NEL     ,NGL     ,NUPARAM ,NUVAR   ,NPF      ,
     2           TIME    ,TIMESTEP,UPARAM  ,UVAR    ,JTHE     ,OFF     ,
     3           GS      ,RHO     ,PLA     ,DPLA    ,EPSP     ,SOUNDSP ,
     4           DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX   ,ASRATE  ,
     5           EPSPXX  ,EPSPYY  ,EPSPXY  ,EPSPYZ  ,EPSPZX   ,
     6           SIGOXX  ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX   ,
     7           SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX   ,THKLY   ,
     8           THK     ,SIGY    ,ET      ,TEMPEL  ,TEMP     ,SEQ     ,
     9           TF      ,NUMTABL ,ITABLE  ,TABLE   ,NVARTMP  ,VARTMP  ,
     A           SIGA    ,INLOC   ,DPLANL  ,LOFF    )
            CASE(4)
              CALL SIGEPS110C_LITE_NEWTON(
     1           NEL     ,NGL     ,NUPARAM ,NUVAR   ,NPF      ,
     2           TIME    ,TIMESTEP,UPARAM  ,UVAR    ,JTHE     ,OFF     ,
     3           GS      ,RHO     ,PLA     ,DPLA    ,EPSP     ,SOUNDSP ,
     4           DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX   ,ASRATE  ,
     5           EPSPXX  ,EPSPYY  ,EPSPXY  ,EPSPYZ  ,EPSPZX   ,
     6           SIGOXX  ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX   ,
     7           SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX   ,THKLY  ,
     8           THK     ,SIGY    ,ET      ,TEMPEL  ,TEMP     ,SEQ    ,
     9           TF      ,NUMTABL ,ITABLE  ,TABLE   ,NVARTMP  ,VARTMP ,
     A           SIGA    ,INLOC   ,DPLANL  ,LOFF    )
          END SELECT    
        END SELECT
        ! Ductile failure activation
        IOFF_DUCT(1:NEL) = 1
c--------------------------
      END
