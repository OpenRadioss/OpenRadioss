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
Chd|  READ_NLOC_STRUCT              source/output/restart/read_nloc_struct.F
Chd|-- called by -----------
Chd|        RDRESB                        source/output/restart/rdresb.F
Chd|-- calls ---------------
Chd|        READ_DB                       source/output/tools/read_db.F 
Chd|        READ_I_C                      ../common_source/tools/input_output/write_routtines.c
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|====================================================================
      SUBROUTINE READ_NLOC_STRUCT(NLOC_DMG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE NLOCAL_REG_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "parit_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr02_c.inc"
#include      "rad2r_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (NLOCAL_STR_),TARGET :: NLOC_DMG 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ILOC,NNOD,L_NLOC,NUMELS_NL,NUMELC_NL,NDDMAX,
     .        NUMELTG_NL,LCNE_NL,MATSIZE
      my_real, DIMENSION(:), POINTER :: FNL,STIFNL
      INTEGER, DIMENSION(8)  :: 
     .  HEAD
C=======================================================================
c
c-------  read NLOC_DMG data
c
      CALL READ_I_C(HEAD,8)
      ILOC       = HEAD(1) 
      NNOD       = HEAD(2) 
      L_NLOC     = HEAD(3)
      NUMELS_NL  = HEAD(4)  
      NUMELC_NL  = HEAD(5)  
      NUMELTG_NL = HEAD(6) 
      NDDMAX     = HEAD(7) 
      LCNE_NL    = HEAD(8)
      NLOC_DMG%IMOD       = ILOC
      NLOC_DMG%NNOD       = NNOD
      NLOC_DMG%L_NLOC     = L_NLOC
      NLOC_DMG%NUMELS_NL  = NUMELS_NL
      NLOC_DMG%NUMELC_NL  = NUMELC_NL
      NLOC_DMG%NUMELTG_NL = NUMELTG_NL
      NLOC_DMG%NDDMAX     = NDDMAX
      NLOC_DMG%LCNE_NL    = LCNE_NL   
c
      IF (ILOC > 0) THEN
c
        IF (R2R_SIU > 0) THEN
C--       multidomains - origianl nummat is used
          MATSIZE = NUMMAT0 
        ELSE
          MATSIZE = NUMMAT 
        ENDIF
c
        ALLOCATE (NLOC_DMG%LEN(MATSIZE))
        ALLOCATE (NLOC_DMG%LE_MAX(MATSIZE))
        ALLOCATE (NLOC_DMG%DENS(MATSIZE))
        ALLOCATE (NLOC_DMG%DAMP(MATSIZE))
        ALLOCATE (NLOC_DMG%SSPNL(MATSIZE))
        ALLOCATE (NLOC_DMG%INDX(NNOD))
        ALLOCATE (NLOC_DMG%POSI(NNOD+1))
        ALLOCATE (NLOC_DMG%IDXI(NUMNOD))
        ALLOCATE (NLOC_DMG%MASS(L_NLOC))
        ALLOCATE (NLOC_DMG%MASS0(L_NLOC))
        ALLOCATE (NLOC_DMG%VNL(L_NLOC))
        ALLOCATE (NLOC_DMG%VNL_OLD(L_NLOC))
        ALLOCATE (NLOC_DMG%DNL(L_NLOC))
        ALLOCATE (NLOC_DMG%UNL(L_NLOC))
        ALLOCATE (NLOC_DMG%CNE(0))
c
        ! If starter PARITH/ON
        IF (IPARI0 == 1) THEN
          ALLOCATE (NLOC_DMG%ADDCNE(NNOD+1))
          ALLOCATE (NLOC_DMG%PROCNE(LCNE_NL))
          ALLOCATE (NLOC_DMG%IADS(8,NUMELS_NL))
          ALLOCATE (NLOC_DMG%IADC(4,NUMELC_NL))
          ALLOCATE (NLOC_DMG%IADTG(3,NUMELTG_NL))     
c
        ! If starter PARITH/OFF
        ELSE
          ALLOCATE (NLOC_DMG%ADDCNE(0))
          ALLOCATE (NLOC_DMG%PROCNE(0))
          ALLOCATE (NLOC_DMG%IADS(0,0))
          ALLOCATE (NLOC_DMG%IADC(0,0))
          ALLOCATE (NLOC_DMG%IADTG(0,0))
          ALLOCATE (NLOC_DMG%FSKY(0,0))
          ALLOCATE (NLOC_DMG%STSKY(0,0))
        ENDIF
c
        ! If engine PARITH/ON
        IF (IPARIT == 1) THEN 
          ALLOCATE (NLOC_DMG%FNL(L_NLOC,1))
          NLOC_DMG%FNL(1:L_NLOC,1)    = ZERO
          ! Non-local nodal stiffness
          IF (NODADT > 0) THEN
            ALLOCATE (NLOC_DMG%STIFNL(L_NLOC,1))
            NLOC_DMG%STIFNL(1:L_NLOC,1) = ZERO
          ELSE
            ALLOCATE (NLOC_DMG%STIFNL(0,0))
          ENDIF
        ! If engine PARITH/OFF
        ELSE
          ALLOCATE (NLOC_DMG%FNL(L_NLOC,NTHREAD))
          NLOC_DMG%FNL(1:L_NLOC,1:NTHREAD) = ZERO
          ! Non-local nodal stiffness
          IF (NODADT > 0) THEN
            ALLOCATE (NLOC_DMG%STIFNL(L_NLOC,NTHREAD))
            NLOC_DMG%STIFNL(1:L_NLOC,1:NTHREAD) = ZERO
          ELSE
            ALLOCATE (NLOC_DMG%STIFNL(0,0))
          ENDIF
        ENDIF
c
        CALL READ_DB(NLOC_DMG%DENS,MATSIZE)
c
        CALL READ_DB(NLOC_DMG%DAMP,MATSIZE)
c
        CALL READ_DB(NLOC_DMG%LEN,MATSIZE)
c
        CALL READ_DB(NLOC_DMG%LE_MAX,MATSIZE)
c
        CALL READ_DB(NLOC_DMG%SSPNL,MATSIZE)
c        
        CALL READ_I_C(NLOC_DMG%INDX,NNOD)
c        
        CALL READ_I_C(NLOC_DMG%POSI,NNOD+1)
c
        CALL READ_I_C(NLOC_DMG%IDXI,NUMNOD)
c
        ! If starter PARITH/ON, reading pointers and allocation of the FSKY vector
        IF (IPARI0 == 1) THEN 
c
          ! ADDCNE table
          CALL READ_I_C(NLOC_DMG%ADDCNE,NNOD+1)
c
          ! PROCNE table
          CALL READ_I_C(NLOC_DMG%PROCNE,LCNE_NL)
c
          ! IADS table
          CALL READ_I_C(NLOC_DMG%IADS,8*NUMELS_NL)
c
          ! IADC table
          CALL READ_I_C(NLOC_DMG%IADC,4*NUMELC_NL)
c
          ! IADTG table
          CALL READ_I_C(NLOC_DMG%IADTG,3*NUMELTG_NL)
c
          ! FSKY vector
          ALLOCATE (NLOC_DMG%FSKY(NLOC_DMG%ADDCNE(NNOD+1),NDDMAX))
          NLOC_DMG%FSKY(1:NLOC_DMG%ADDCNE(NNOD+1),1:NDDMAX) = ZERO
c
          ! STSKY vector
          IF (NODADT > 0) THEN
            ALLOCATE (NLOC_DMG%STSKY(NLOC_DMG%ADDCNE(NNOD+1),NDDMAX))
            NLOC_DMG%STSKY(1:NLOC_DMG%ADDCNE(NNOD+1),1:NDDMAX) = ZERO
          ELSE
            ALLOCATE (NLOC_DMG%STSKY(0,0))
          ENDIF
c
        ENDIF
c        
        CALL READ_DB(NLOC_DMG%MASS,L_NLOC)
c
        CALL READ_DB(NLOC_DMG%MASS0,L_NLOC)
c
        FNL => NLOC_DMG%FNL(1:L_NLOC,1)
        CALL READ_DB(FNL,L_NLOC)
c
        CALL READ_DB(NLOC_DMG%VNL,L_NLOC)
c
        CALL READ_DB(NLOC_DMG%VNL_OLD,L_NLOC)
c
        CALL READ_DB(NLOC_DMG%DNL,L_NLOC)
c
        CALL READ_DB(NLOC_DMG%UNL,L_NLOC)
c
      ELSE
        NLOC_DMG%NNOD       = 0
        NLOC_DMG%L_NLOC     = 0
        NLOC_DMG%NUMELS_NL  = 0
        NLOC_DMG%NUMELC_NL  = 0
        NLOC_DMG%NUMELTG_NL = 0
        NLOC_DMG%NDDMAX     = 0
        NLOC_DMG%LCNE_NL    = 0
        ALLOCATE (NLOC_DMG%DENS(0))
        ALLOCATE (NLOC_DMG%DAMP(0))
        ALLOCATE (NLOC_DMG%LEN(0))
        ALLOCATE (NLOC_DMG%LE_MAX(0))
        ALLOCATE (NLOC_DMG%SSPNL(0))
        ALLOCATE (NLOC_DMG%INDX(0))
        ALLOCATE (NLOC_DMG%POSI(0))
        ALLOCATE (NLOC_DMG%IDXI(0))
        ALLOCATE (NLOC_DMG%ADDCNE(0))
        ALLOCATE (NLOC_DMG%CNE(0))
        ALLOCATE (NLOC_DMG%PROCNE(0))
        ALLOCATE (NLOC_DMG%IADS(0,0))
        ALLOCATE (NLOC_DMG%IADC(0,0))
        ALLOCATE (NLOC_DMG%IADTG(0,0))
        ALLOCATE (NLOC_DMG%MASS(0))
        ALLOCATE (NLOC_DMG%MASS0(0))
        ALLOCATE (NLOC_DMG%FNL(0,0))
        ALLOCATE (NLOC_DMG%VNL(0))
        ALLOCATE (NLOC_DMG%VNL_OLD(0))
        ALLOCATE (NLOC_DMG%DNL(0))
        ALLOCATE (NLOC_DMG%UNL(0))
        ALLOCATE (NLOC_DMG%STIFNL(0,0))
        ALLOCATE (NLOC_DMG%FSKY(0,0))
        ALLOCATE (NLOC_DMG%STSKY(0,0))
        ALLOCATE (NLOC_DMG%IAD_ELEM(0))
        ALLOCATE (NLOC_DMG%IAD_SIZE(0))
        ALLOCATE (NLOC_DMG%FR_ELEM(0))
      ENDIF
c-----------
      RETURN
      END
