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
! wrapper for implicit calls , cannot be the included in the module
! because the calls does not respect Fortran standard and will cause
! compiler errors
      SUBROUTINE FORCE_IMP(IB      ,FAC       ,NPC     ,TF      ,A      ,
     2                  V       ,X         ,SKEW    ,AR      ,VR     ,
     3                  NSENSOR ,SENSOR_TAB,TFEXC   ,IADC   ,
     4                  FSKY    ,FSKYV     ,FEXT    ,H3D_DATA,
     5                  CPTREAC ,FTHREAC   ,NODREAC ,TH_SURF ,FSAVSURF,
     6                  NSEG_LOADP) 
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE H3D_MOD
      USE PINCHTYPE_MOD 
      USE SENSOR_MOD
      USE TH_SURF_MOD , ONLY : TH_SURF_
      USE FORCE_MOD   , ONLY : FORCE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "impl1_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------,
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NSENSOR
      INTEGER NPC(*),CPTREAC,NODREAC(*)
      INTEGER IB(NIBCLD,*)
      INTEGER IADC(4,*)
      my_real
     .   FAC(LFACCLD,*), TF(*), A(3,*), V(3,*), AR(3,*), VR(3,*),
     .   X(3,*), SKEW(LSKEW,*), TFEXC,
     .   FSKY(8,LSKY), FSKYV(LSKY,8),FEXT(3,*),FTHREAC(6,*)
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
      TYPE (TH_SURF_) , INTENT(IN) :: TH_SURF
      my_real, INTENT(INOUT) :: FSAVSURF(5,NSURF)
      INTEGER, INTENT(INOUT) :: NSEG_LOADP(NSURF)

      call FORCE(IB      ,FAC       ,NPC     ,TF      ,A      ,
     2                  V       ,X         ,SKEW    ,AR      ,VR     ,
     3                  NSENSOR ,SENSOR_TAB,TFEXC   ,IADC   ,
     4                  FSKY    ,FSKYV     ,FEXT    ,H3D_DATA,
     5                  CPTREAC ,FTHREAC   ,NODREAC ,TH_SURF ,FSAVSURF,
     6                  NSEG_LOADP) 

      END SUBROUTINE