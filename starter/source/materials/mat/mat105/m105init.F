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
Chd|  M105INIT                      source/materials/mat/mat105/m105init.F
Chd|-- called by -----------
Chd|        MATINI                        source/materials/mat_share/matini.F
Chd|-- calls ---------------
Chd|        M5IN2                         source/initial_conditions/detonation/m5in2.F
Chd|        M5IN3                         source/initial_conditions/detonation/m5in3.F
Chd|        DETONATORS_MOD                share/modules1/detonators_mod.F
Chd|====================================================================
      SUBROUTINE M105INIT(
     .               IPM     ,DETONATORS ,PM      ,
     .               NGL     ,NUVAR      ,UVAR    ,UPARAM  ,X       ,
     .               MAT     ,IPARG      ,IFORM   ,IX      ,NIX     ,
     .               BUFMAT     ,RHO0    ,TB)
      USE DETONATORS_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPM(NPROPMI,*),NGL(*),MAT(*), IPARG(NPARG),IFORM,NIX,IX(NIX,*)
      my_real  
     .   PM(NPROPM,*),UVAR(*),
     .   UPARAM(*), X(3,*), BUFMAT(*), RHO0, TB(*)
      TYPE(DETONATOR_STRUCT_)::DETONATORS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NUVAR, GG1, GG2, GG3, ISFLUID
      INTEGER NPH,IFLG,NV46
C-----------------------------------------------

      !---------------------------------!
      ! Compute Burning Time            !
      !---------------------------------!
      NPH = 1
      IF(N2D==0)THEN
        CALL M5IN3 (PM,MAT,0,DETONATORS,TB,NGL,IPARG,X,IX,NIX)
      ELSE
        CALL M5IN2 (PM,MAT,0,DETONATORS,TB,NGL,X,IX,NIX)
      ENDIF

C-----------------------------------------------
      RETURN
      END SUBROUTINE M105INIT
