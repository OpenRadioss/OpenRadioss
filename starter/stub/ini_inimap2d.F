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
#ifndef DNC
Chd|====================================================================
Chd|  INI_INIMAP2D                  stub/ini_inimap2d.F           
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        FUNC2D_MOD                    share/modules1/func2d_mod.F   
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INIMAP2D_MOD                  share/modules1/inimap2d_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE INI_INIMAP2D(INIMAP2D ,ELBUF_TAB ,IPART   ,IPARG   ,IPARTS   , 
     .                        IPARTQ   ,XGRID     ,VEL     ,IXS     ,IXQ      ,
     .                        IXTG     ,PM        ,IPM     ,BUFMAT  ,MULTI_FVM, 
     .                        FUNC2D   ,IGRBRIC   ,IGRQUAD ,IGRSH3N )
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
      USE INIMAP2D_MOD  
      USE FUNC2D_MOD
      USE MULTI_FVM_MOD
      USE GROUPDEF_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C     I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc" 
C-----------------------------------------------
C     C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr17_c.inc"
!     NGROUP
#include      "com01_c.inc"
!     NFUNCT
#include      "com04_c.inc"
!     MVSIZ
#include      "mvsiz_p.inc"
!     IOUT
#include      "units_c.inc"
!     IALEFVM
#include      "alefvm.inc"
!     N0PHAS, NVPHAS
#include      "mmale51_c.inc"
C-----------------------------------------------
C     D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(INIMAP2D_STRUCT), DIMENSION(NINIMAP2D), INTENT(INOUT) :: INIMAP2D
      TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP), INTENT(INOUT), TARGET :: ELBUF_TAB
      INTEGER, INTENT(IN) :: IPART(LIPART1, *)
      INTEGER, INTENT(IN) :: IPARTS(*), IPARTQ(*),IPM(NPROPMI, *), 
     .     IPARG(NPARG, NGROUP), IXS(NIXS, *), IXQ(NIXQ, *), IXTG(NIXTG, *)
      my_real, INTENT(IN) :: XGRID(3, *), PM(NPROPM, *), BUFMAT(*)
      my_real, INTENT(INOUT) :: VEL(3, *)
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
      TYPE(FUNC2D_STRUCT), DIMENSION(NFUNC2D), INTENT(IN) :: FUNC2D
      my_real, POINTER, DIMENSION(:) :: THETA
      CHARACTER*2 :: Str1, Str2
C      TYPE(KDTREE_TAB) :: KDTREES(4 * NINIMAP2D)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE (GROUP_)  , DIMENSION(NGRQUAD) :: IGRQUAD
      TYPE (GROUP_)  , DIMENSION(NGRSH3N) :: IGRSH3N
C-----------------------------------------------
C     L o c a l   V a r i a b l e s
C-----------------------------------------------
      
      END SUBROUTINE INI_INIMAP2D

#endif
