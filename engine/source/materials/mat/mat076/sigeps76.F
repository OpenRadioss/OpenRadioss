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
Chd|  SIGEPS76                      source/materials/mat/mat076/sigeps76.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|        ASSO_PLAS76                   source/materials/mat/mat076/asso_plas76.F
Chd|        NO_ASSO_PLAS76                source/materials/mat/mat076/no_asso_plas76.F
Chd|        INTERFACE_TABLE_MOD           share/modules/table_mod.F     
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE SIGEPS76(
     1           NEL    ,NUPARAM,NUVAR   ,NFUNC   ,IFUNC   ,NGL     ,
     2           NPF    ,TF     ,TIME    ,TIMESTEP,UPARAM  ,MATPARAM,
     3           RHO0   ,RHO    ,DPLA    ,ET      ,SOUNDSP ,YLD     ,
     3           DEPSXX ,DEPSYY ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     4           SIGOXX ,SIGOYY ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     5           SIGNXX ,SIGNYY ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     6           UVAR   ,OFF    ,EPSP    ,PLA     ,INLOC   ,VARNL   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE_MOD
      USE INTERFACE_TABLE_MOD
      USE MATPARAM_DEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C=======================================================================
      INTEGER :: NEL,NUPARAM,NUVAR,NGL(NEL),INLOC
      my_real :: TIME,TIMESTEP
      my_real :: UPARAM(NUPARAM)
      my_real :: RHO(NEL),RHO0(NEL),
     .   DEPSXX(NEL),DEPSYY(NEL),DEPSZZ(NEL),
     .   DEPSXY(NEL),DEPSYZ(NEL),DEPSZX(NEL),
     .   SIGOXX(NEL),SIGOYY(NEL),SIGOZZ(NEL),
     .   SIGOXY(NEL),SIGOYZ(NEL),SIGOZX(NEL),
     .   EPSP(NEL),DPLA(NEL),VARNL(NEL),
     .   SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .   SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .   SOUNDSP(NEL),DPLA1(NEL),ET(NEL),OFF(NEL),YLD(NEL),PLA(NEL)
      my_real :: UVAR(NEL,NUVAR)
      TYPE(MATPARAM_STRUCT_) , TARGET :: MATPARAM
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*),NFUNC,IFUNC(NFUNC),NDIM,NPT
      my_real :: TF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IFORM,NUMTABL
      TYPE(TTABLE), DIMENSION(:) , POINTER :: TABLE
C=======================================================================
c
      IFORM = NINT(UPARAM(13)) ! Flag for plasticity 
                               ! = 0 associated (with quadratic yield criterion only)
                               ! = 1 not associated
c
      NUMTABL =  MATPARAM%NTABLE
      TABLE   => MATPARAM%TABLE(1:NUMTABL)
c--------------------------                              
      SELECT CASE (IFORM)
c
        CASE(0)
          CALL NO_ASSO_PLAS76(
     1      NEL    ,NUPARAM,NUVAR   ,NFUNC   ,IFUNC   ,
     2      NPF    ,TF     ,TIME    ,TIMESTEP,UPARAM  ,PLA    ,
     3      RHO0   ,RHO    ,DPLA    ,ET      ,NUMTABL ,TABLE  ,
     3      DEPSXX ,DEPSYY ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX ,
     4      SIGOXX ,SIGOYY ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX ,
     5      SIGNXX ,SIGNYY ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     6      SOUNDSP,UVAR   ,OFF     ,NGL     ,EPSP    ,YLD    ,
     7      INLOC   ,VARNL  )
c
        CASE(1) 
          CALL ASSO_PLAS76(     
     1      NEL    ,NUPARAM,NUVAR   ,NFUNC   ,IFUNC   ,
     2      NPF    ,TF     ,TIME    ,TIMESTEP,UPARAM  ,PLA    ,
     3      RHO0   ,RHO    ,DPLA    ,ET      ,NUMTABL ,TABLE  ,
     3      DEPSXX ,DEPSYY ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX ,
     4      SIGOXX ,SIGOYY ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX ,
     5      SIGNXX ,SIGNYY ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     6      SOUNDSP,UVAR   ,OFF     ,NGL     ,EPSP    ,YLD    ,
     7      INLOC   ,VARNL  )
c
      END SELECT
c-----------
      RETURN
      END
