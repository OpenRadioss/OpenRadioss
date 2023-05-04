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
Chd|  ANIM_SET2ZERO_STRUCT          source/output/anim/reader/anim_set2zero_struct.F
Chd|-- called by -----------
Chd|        FREANIM                       source/output/anim/reader/freanim.F
Chd|-- calls ---------------
Chd|        ANIM_MOD                      ../common_source/modules/anim_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE ANIM_SET2ZERO_STRUCT(SENSORS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE ANIM_MOD
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr14_c.inc"
#include      "scr25_c.inc"
#include      "scrcut_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      TYPE (SENSORS_) ,INTENT(OUT) :: SENSORS
C-----------------------------------------------
      TANIM0            = ZERO
      TANIM_STOP0       = ZERO
      DTANIM0           = ZERO
      TANIMSENS         = ZERO
      SENSORS%ANIM_DT   = ZERO
      SENSORS%ANIM_ID   = 0
      ANIM_M            = 0
      ANIM_K            = 0
      ANIM_U            = 0
      ANIM_MAT          = 0
      NCUTS             = 0
      IEPSDOT           = 0
      FMT_ANI           = 3
      ANIM_VERS         = 44
      IZIP              = 0
      IAD_GPS           = 100
      ISTRESALL         = 0
      ISTRAIALL         = 0
      IEPSDOALL         = 0
      IEPSPALL          = 0
      IEPSPNLALL        = 0
      IEPSDNLALL        = 0
      IPHIALL           = 0
      IORTHDALL         = 0 
      IPLYALL           = 0
      ANIM_PLY          = 0
      ANIM_CRK          = 0
      ISTRESFULL        = 0
      IEPSPFULL         = 0
      ISTRAINFULL       = 0
      IEPSDOFULL        = 0
      ! anim per ply
      IPLYALL           = 0
      ISTRESALL_PLY     = 0
      IPHIALL_PLY       = 0       
      IEPSPALL_PLY      = 0
      ISTRAINALL_PLY    = 0
      IEPSDOTALL_PLY    = 0
      IDAMAALL_PLY      = 0
      IBRICK_STRESSALL  = 0
      IBRICK_STRAINALL  = 0
      IBRICK_EPSPALL    = 0
      IWPLAFULL         = 0
      IWPLAALL          = 0
      IBEAM_EPSPALL     = 0
      ITSAIWUALL        = 0
      ITSAIWUFULL       = 0

      DO I = 1,MX_ANI
        ANIM_V(I)       = 0
        ANIM_T(I)       = 0
        ANIM_E(I)       = 0
        ANIM_CE(I)      = 0
        ANIM_SE(I)      = 0
        ANIM_FE(I)      = 0
        ANIM_CT(I)      = 0
        ANIM_ST(I)      = 0
        ANIM_FT(I)      = 0
        NTITLETAB(I)    = 0
      ENDDO
      ANIM_WPLA         = 0
      IFVANI            = 0
      NLTITLE           = 0
      NBPLY_ANIM        = 0
      NBPLY_ANIM_STRESS = 0
      NBPLY_ANIM_STRAIN = 0
      NBPLY_ANIM_EPSDOT = 0
      NBPLY_ANIM_PHI    = 0
      NBPLY_ANIM_EPSP   = 0
      NBPLY_ANIM_DAMA   = 0

      RETURN
      END
