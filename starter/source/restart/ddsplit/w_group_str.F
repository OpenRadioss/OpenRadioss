Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  W_GROUP_STR                   source/restart/ddsplit/w_group_str.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        W_GR_ENTITY_E                 source/restart/ddsplit/w_gr_entity.F
Chd|        W_GR_ENTITY_N                 source/restart/ddsplit/w_gr_entity.F
Chd|        W_GR_ENTITY_P                 source/restart/ddsplit/w_gr_entity.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE W_GROUP_STR(LEN_IA  ,IGRNOD   ,IGRBRIC ,IGRQUAD   ,IGRSH4N   ,
     .                       IGRSH3N ,IGRTRUSS ,IGRBEAM ,IGRSPRING ,IGRPART   ,
     .                       CEP     ,CEL      ,NODLOCAL,PROC      ,FRONTB_R2R,
     .                       NUMNOD_L)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "r2r_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LEN_IA,CEP(*),CEL(*),NODLOCAL(*),PROC,FRONTB_R2R(*),NUMNOD_L
!
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE (GROUP_)  , DIMENSION(NGRQUAD) :: IGRQUAD
      TYPE (GROUP_)  , DIMENSION(NGRSHEL) :: IGRSH4N
      TYPE (GROUP_)  , DIMENSION(NGRSH3N) :: IGRSH3N
      TYPE (GROUP_)  , DIMENSION(NGRTRUS) :: IGRTRUSS
      TYPE (GROUP_)  , DIMENSION(NGRBEAM) :: IGRBEAM
      TYPE (GROUP_)  , DIMENSION(NGRSPRI) :: IGRSPRING
      TYPE (GROUP_)  , DIMENSION(NGRPART) :: IGRPART
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ESHIFT
C-----------------------------------------------
!  --- WRITE IGRNOD ---
      IF (NGRNOD > 0)  CALL W_GR_ENTITY_N(IGRNOD ,NGRNOD ,LEN_IA,LENIGRNOD,NODLOCAL,
     .                                    PROC   ,FRONTB_R2R,NUMNOD_L)
!  --- WRITE IGRBRIC ---
      ESHIFT = 0
      IF (NGRBRIC > 0) CALL W_GR_ENTITY_E(IGRBRIC,NGRBRIC,LEN_IA,LENIGRBRIC,CEP,
     .                                    CEL    ,PROC   ,ESHIFT)
!  --- WRITE IGRQUAD ---
      ESHIFT = ESHIFT + NUMELS
      IF (NGRQUAD > 0) CALL W_GR_ENTITY_E(IGRQUAD,NGRQUAD,LEN_IA,LENIGRQUAD,CEP,
     .                                    CEL    ,PROC   ,ESHIFT)
!  --- WRITE IGRSH4N ---
      ESHIFT = ESHIFT + NUMELQ
      IF (NGRSHEL > 0) CALL W_GR_ENTITY_E(IGRSH4N,NGRSHEL,LEN_IA,LENIGRSH4N,CEP,
     .                                    CEL    ,PROC   ,ESHIFT)
!  --- WRITE IGRTRUSS ---
      ESHIFT = ESHIFT + NUMELC
      IF (NGRTRUS > 0) CALL W_GR_ENTITY_E(IGRTRUSS,NGRTRUS,LEN_IA,LENIGRTRUS,CEP,
     .                                    CEL     ,PROC   ,ESHIFT)
!  --- WRITE IGRBEAM ---
      ESHIFT = ESHIFT + NUMELT
      IF (NGRBEAM > 0) CALL W_GR_ENTITY_E(IGRBEAM,NGRBEAM,LEN_IA,LENIGRBEAM,CEP,
     .                                    CEL    ,PROC   ,ESHIFT)
!  --- WRITE IGRSPRING ---
      ESHIFT = ESHIFT + NUMELP
      IF (NGRSPRI > 0) CALL W_GR_ENTITY_E(IGRSPRING,NGRSPRI,LEN_IA,LENIGRSPRI,CEP,
     .                                    CEL      ,PROC   ,ESHIFT)
!  --- WRITE IGRSH3N ---
      ESHIFT = ESHIFT + NUMELR
      IF (NGRSH3N > 0) CALL W_GR_ENTITY_E(IGRSH3N,NGRSH3N,LEN_IA,LENIGRSH3N,CEP,
     .                                    CEL    ,PROC   ,ESHIFT)
!!      ESHIFT = ESHIFT + NUMELTG
!  --- WRITE IGRPART ---
      IF (NGRPART > 0) CALL W_GR_ENTITY_P(IGRPART,NGRPART,LEN_IA,LENIGRPART,CEP,
     .                                  CEL    ,PROC   )
!---------
      RETURN
      END
