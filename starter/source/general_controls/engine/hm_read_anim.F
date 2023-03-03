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
Chd|  HM_READ_ANIM                  source/general_controls/engine/hm_read_anim.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ANIM_MOD                      ../common_source/modules/anim_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        STACK_MOD                     share/modules1/stack_mod.F    
Chd|====================================================================
      SUBROUTINE HM_READ_ANIM()
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE STACK_MOD
      USE ANIM_MOD
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
      INTEGER I, 
     .        NBPLY_ANIM,NBPLY_ANIM_STRESS,IPT,NBPLY_ANIM_PHI,
     .        NBPLY_ANIM_EPSP,NBPLY_ANIM_STRAIN,NBPLY_ANIM_EPSDOT,
     .        NBPLY_ANIM_DAMA
C-----------------------------------------------
C     Initializations ../..
C-----------------------------------------------
      TANIM0            = ZERO
      DTANIM0           = ZERO
      TANIM_STOP0       = EP20
      TANIMSENS         = ZERO
      DTANISENS         = ZERO
      IANISENS          = 0
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
        LANISENS(I)     = 0
        NTITLETAB(I)    = 0
      ENDDO
      ANIM_WPLA         = 0
      NLSENS            = 0
      IFVANI            = 0
      NLTITLE           = 0
      NBPLY_ANIM        = 0
      NBPLY_ANIM_STRESS = 0
      NBPLY_ANIM_STRAIN = 0
      NBPLY_ANIM_EPSDOT = 0
      NBPLY_ANIM_PHI    = 0
      NBPLY_ANIM_EPSP   = 0
      NBPLY_ANIM_DAMA   = 0
C-----------------------------------------------
C     Read options ../..
C-----------------------------------------------
C     Counters
C-----------------------------------------------
      NV_ANI = 0
      NT_ANI = 0
      NE_ANI = 0
      NN_ANI = 0
      NCT_ANI = 0
      NCE_ANI = 0
      NST_ANI = 0
      NSE_ANI = 0
      NFT_ANI = 0
      NFE_ANI = 0
      DO I = 1,MX_ANI
        NV_ANI = NV_ANI + ANIM_V(I)
        NT_ANI = NT_ANI + ANIM_T(I)
        NE_ANI = NE_ANI + ANIM_E(I)
        NN_ANI = NN_ANI + ANIM_N(I)
        NST_ANI = NST_ANI + ANIM_ST(I)
        NSE_ANI = NSE_ANI + ANIM_SE(I)
        NCT_ANI = NCT_ANI + ANIM_CT(I)
        NCE_ANI = NCE_ANI + ANIM_CE(I)
        NFT_ANI = NFT_ANI + ANIM_FT(I)
        NFE_ANI = NFE_ANI + ANIM_FE(I)
      ENDDO
      IF(ANIM_V(12)==1)NV_ANI=NV_ANI+1
C
      RETURN
      END
