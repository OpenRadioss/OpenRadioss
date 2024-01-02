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
Chd|  WRITE_NLOC_STRUCT             source/output/restart/write_nloc_struct.F
Chd|-- called by -----------
Chd|        WRRESTP                       source/output/restart/wrrestp.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/output/tools/write_db.F
Chd|        WRITE_I_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|====================================================================
      SUBROUTINE WRITE_NLOC_STRUCT(NLOC_DMG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE NLOCAL_REG_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (NLOCAL_STR_),TARGET :: NLOC_DMG 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ILOC,NNOD,L_NLOC,NUMELS_NL,NUMELC_NL,NDDMAX,
     .        NUMELTG_NL,LCNE_NL
      my_real, DIMENSION(:), POINTER :: FNL
      INTEGER, DIMENSION(8)  :: 
     .  HEAD
C=======================================================================
      ILOC       = NLOC_DMG%IMOD
      NNOD       = NLOC_DMG%NNOD
      L_NLOC     = NLOC_DMG%L_NLOC
      NUMELS_NL  = NLOC_DMG%NUMELS_NL
      NUMELC_NL  = NLOC_DMG%NUMELC_NL
      NUMELTG_NL = NLOC_DMG%NUMELTG_NL
      NDDMAX     = NLOC_DMG%NDDMAX
      LCNE_NL    = NLOC_DMG%LCNE_NL
      HEAD(1)    = ILOC
      HEAD(2)    = NNOD 
      HEAD(3)    = L_NLOC
      HEAD(4)    = NUMELS_NL
      HEAD(5)    = NUMELC_NL
      HEAD(6)    = NUMELTG_NL
      HEAD(7)    = NDDMAX
      HEAD(8)    = LCNE_NL
      CALL WRITE_I_C(HEAD,8)
c
      IF (ILOC > 0) THEN
c
        CALL WRITE_DB(NLOC_DMG%DENS,NUMMAT)
c
        CALL WRITE_DB(NLOC_DMG%DAMP,NUMMAT)
c
        CALL WRITE_DB(NLOC_DMG%LEN,NUMMAT)
c
        CALL WRITE_DB(NLOC_DMG%LE_MAX,NUMMAT)
c
        CALL WRITE_DB(NLOC_DMG%SSPNL,NUMMAT)
c
        CALL WRITE_I_C(NLOC_DMG%INDX,NNOD)
c
        CALL WRITE_I_C(NLOC_DMG%POSI,NNOD+1)
c
        CALL WRITE_I_C(NLOC_DMG%IDXI,NUMNOD)
c
        ! If PARITH/ON
        IF (IPARI0 == 1) THEN
c
          CALL WRITE_I_C(NLOC_DMG%ADDCNE,NNOD+1)
c
          CALL WRITE_I_C(NLOC_DMG%PROCNE,LCNE_NL)
c
          CALL WRITE_I_C(NLOC_DMG%IADS,8*NUMELS_NL)
c
          CALL WRITE_I_C(NLOC_DMG%IADC,4*NUMELC_NL)
c
          CALL WRITE_I_C(NLOC_DMG%IADTG,3*NUMELTG_NL)
c
        ENDIF
c
        CALL WRITE_DB(NLOC_DMG%MASS,L_NLOC)
c
        CALL WRITE_DB(NLOC_DMG%MASS0,L_NLOC)
c
        FNL => NLOC_DMG%FNL(1:L_NLOC,1)
        CALL WRITE_DB(FNL,L_NLOC)
c
        CALL WRITE_DB(NLOC_DMG%VNL,L_NLOC)
c
        CALL WRITE_DB(NLOC_DMG%VNL_OLD,L_NLOC)
c
        CALL WRITE_DB(NLOC_DMG%DNL,L_NLOC)
c
        CALL WRITE_DB(NLOC_DMG%UNL,L_NLOC)
c---
      ENDIF
c--------------------------------
      RETURN
      END
