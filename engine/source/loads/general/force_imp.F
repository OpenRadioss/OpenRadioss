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
! wrapper for implicit calls , cannot be the included in the module
! because the calls does not respect Fortran standard and will cause
! compiler errors
!||====================================================================
!||    force_imp       ../engine/source/loads/general/force_imp.F
!||--- called by ------------------------------------------------------
!||    dyna_ina        ../engine/source/implicit/imp_dyna.F
!||    dyna_wex        ../engine/source/implicit/imp_dyna.F
!||    imp_chkm        ../engine/source/implicit/imp_solv.F
!||    imp_solv        ../engine/source/implicit/imp_solv.F
!||--- calls      -----------------------------------------------------
!||    force           ../engine/source/loads/general/force.F90
!||--- uses       -----------------------------------------------------
!||    force_mod       ../engine/source/loads/general/force.F90
!||    h3d_mod         ../engine/share/modules/h3d_mod.F
!||    pinchtype_mod   ../common_source/modules/pinchtype_mod.F
!||    sensor_mod      ../common_source/modules/sensor_mod.F90
!||    skew_mod        ../common_source/modules/skew_mod.F90
!||    th_surf_mod     ../common_source/modules/interfaces/th_surf_mod.F
!||====================================================================
      SUBROUTINE FORCE_IMP(IB      ,FAC     ,SNPC    ,NPC        ,TF         ,
     2                     A       ,V       ,X       ,SKEWS      ,
     3                     AR      ,VR      ,NSENSOR ,SENSOR_TAB ,WFEXC      ,
     4                     IADC    ,FSKY    ,FEXT    ,H3D_DATA   ,CPTREAC    ,
     5                     FTHREAC ,NODREAC ,TH_SURF ,
     6                     DPL0CLD ,VEL0CLD ,D       ,DR         ,NCONLD     ,
     7                     NUMNOD  ,NFUNCT  ,STF     ,WFEXT)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE H3D_MOD
      USE PINCHTYPE_MOD 
      USE SENSOR_MOD
      USE TH_SURF_MOD , ONLY : TH_SURF_
      USE FORCE_MOD   , ONLY : FORCE
      USE SKEW_MOD
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
!#include      "com04_c.inc"
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
      INTEGER, INTENT(IN) :: SNPC
      INTEGER ,INTENT(IN) :: NSENSOR,NCONLD,NUMNOD,NFUNCT
      INTEGER ,INTENT(IN) :: STF
      INTEGER NPC(SNPC),CPTREAC,NODREAC(*)
      INTEGER IB(NIBCLD,*)
      INTEGER IADC(4,*)
      my_real FAC(LFACCLD,*),TF(*),A(3,*),V(3,*),AR(3,*),VR(3,*),X(3,*),WFEXC,FSKY(8,LSKY),FEXT(3,*),FTHREAC(6,*)
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
      TYPE(SKEW_),INTENT(INOUT) :: SKEWS
      TYPE (TH_SURF_) , INTENT(INOUT) :: TH_SURF
      my_real, INTENT(IN) ::  DPL0CLD(6,NCONLD),VEL0CLD(6,NCONLD)
      my_real, INTENT(IN) ::  D(3,NUMNOD), DR(3,NUMNOD)
      DOUBLE PRECISION,INTENT(INOUT) :: WFEXT
C-----------------------------------------------
      call FORCE( NIBCLD  ,IB      ,LFACCLD    ,FAC      ,SNPC      ,
     2            NPC     ,STF     ,TF         ,A        ,V         ,
     3            X       ,SKEWS   ,AR         ,
     4            VR      ,NSENSOR ,SENSOR_TAB ,WFEXC    ,IADC      ,
     5            LSKY    ,FSKY    ,FEXT       ,H3D_DATA ,CPTREAC   ,
     6            FTHREAC ,NODREAC ,TH_SURF    ,
     7            DPL0CLD ,VEL0CLD ,D          ,DR       ,NCONLD    ,
     8            NUMNOD  ,NFUNCT  , ANIM_V    ,OUTP_V   ,
     9            IPARIT  ,TT      ,DT1        ,N2D      ,WFEXT     ,
     A            IMPL_S) 

      END SUBROUTINE
