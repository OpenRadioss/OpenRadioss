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
Chd|  DYNAIN_SIZE_C                 source/output/dynain/dynain_size.F
Chd|-- called by -----------
Chd|        GENDYNAIN                     source/output/dynain/gendynain.F
Chd|-- calls ---------------
Chd|        SPMD_GLOB_ISUM9               source/mpi/interfaces/spmd_th.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE DYNAIN_SIZE_C(IPARG ,ELBUF_TAB, P0ARS   ,WASZ  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "scr05_c.inc"
#include      "scr16_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),
     .        WASZ,P0ARS
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JJ,NN,ITY,IAD,NFT,LFT,LLT,NPT,IHBE,
     .        MLW,NG,NEL,I,NPG,MPT,NLAY,NPTR,NPTS,
     .        NPTT,IGTYP,IL,NPT_ALL,
     .        RWASZ,WASZ2,P0ARSZ2,RWASZ2
C======================================================================|
      P0ARS = 0
      WASZ = 0
C
      P0ARSZ2 = 0
      WASZ2   = 0
C
      IF (DYNAIN_C(4) == 1) THEN
C------------------------------
C       4: *INITIAL_STRESS_SHELL
C------------------------------
        DO NG=1,NGROUP
          ITY   = IPARG(5,NG)
          NEL   = IPARG(2,NG)
          IGTYP = IPARG(38,NG)
          IF (ITY == 3 .OR. ITY == 7) THEN
            NPT =IPARG(6,NG)
            MPT =IABS(NPT)
            MLW =IPARG(1,NG)
            IHBE  = IPARG(23,NG)
            NLAY  = ELBUF_TAB(NG)%NLAY
            NPTR  = ELBUF_TAB(NG)%NPTR
            NPTS  = ELBUF_TAB(NG)%NPTS
c            NPTT  = ELBUF_TAB(NG)%NPTT
            NPG   = NPTR*NPTS
            IF (ITY==3.AND.IHBE==23) NPG=4
C
C pre counting of all NPTT (especialy for PID_51)
C
            IF (IGTYP == 51 .OR. IGTYP == 52) THEN
              NPT_ALL = 0
              DO IL=1,NLAY
                NPT_ALL = NPT_ALL + ELBUF_TAB(NG)%BUFLY(IL)%NPTT
              ENDDO
              MPT = MAX(1,NPT_ALL)
            ENDIF
            IF (MLW == 1 .OR. MLW == 3 .OR. MLW == 23) MPT=0
            IF(MPT == 0) MPT = 3
C
            WASZ2 = WASZ2 + 5*NEL
            IF (MPT == 0) THEN
              WASZ2 = WASZ2 + 8*NPG*NEL
            ELSE
              WASZ2 = WASZ2 + 8*NPG*MPT*NEL
            ENDIF
          ENDIF
        ENDDO
C
        RWASZ2= WASZ2
        IF (NSPMD > 1) CALL SPMD_GLOB_ISUM9(RWASZ2,1)
        IF (ISPMD == 0) P0ARSZ2 = RWASZ2
      ENDIF  !  IF (STAT_C(4) == 1)
C
      P0ARS= MAX (P0ARS,P0ARSZ2)+6
      WASZ = MAX (WASZ,WASZ2)
C
      P0ARSZ2 = 0
      WASZ2   = 0
C
      IF (DYNAIN_C(5) == 1) THEN
C------------------------------
C       5: *INITIAL_STRAIN_SHELL
C------------------------------
        DO NG=1,NGROUP
         ITY = IPARG(5,NG)
         NEL = IPARG(2,NG)
         IF (ITY == 3.OR.ITY == 7) THEN
            NPT =IPARG(6,NG)
            MPT =IABS(NPT)
            MLW =IPARG(1,NG)
            IHBE  = IPARG(23,NG)
            NLAY  = ELBUF_TAB(NG)%NLAY
            NPTR  = ELBUF_TAB(NG)%NPTR
            NPTS  = ELBUF_TAB(NG)%NPTS
c            NPTT  = ELBUF_TAB(NG)%NPTT
            NPG   = NPTR*NPTS
            IF (ITY==3.AND.IHBE==23) NPG=4
C
C pre counting of all NPTT (especialy for PID_51)
C
            IF (IGTYP == 51 .OR. IGTYP == 52) THEN
              NPT_ALL = 0
              DO IL=1,NLAY
                NPT_ALL = NPT_ALL + ELBUF_TAB(NG)%BUFLY(IL)%NPTT
              ENDDO
              MPT = MAX(1,NPT_ALL)
            ENDIF
            IF (MLW == 1 .OR. MLW == 3 .OR. MLW == 23) MPT=0

            IF(MPT == 0) MPT = 3
C
            WASZ2 = WASZ2 + 5*NEL
            IF (MPT == 0) THEN
              WASZ2 = WASZ2 + 7*NPG*NEL
            ELSE
              WASZ2 = WASZ2 + 7*NPG*MPT*NEL
            ENDIF
         ENDIF
        ENDDO
cC
        RWASZ2= WASZ2
        IF (NSPMD > 1) CALL SPMD_GLOB_ISUM9(RWASZ2,1)
        IF (ISPMD == 0) P0ARSZ2 = RWASZ2
      ENDIF
C
      P0ARS= MAX (P0ARS,P0ARSZ2)+6
      WASZ = MAX (WASZ,WASZ2)
C
!-----------
      RETURN
      END
