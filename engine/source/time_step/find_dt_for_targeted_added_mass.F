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
Chd|  FIND_DT_FOR_TARGETED_ADDED_MASSsource/time_step/find_dt_for_targeted_added_mass.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        FIND_DT_TARGET                ../common_source/tools/time_step/find_dt_target.F
Chd|        MYQSORT                       ../common_source/tools/sort/myqsort.F
Chd|        SPMD_GATHER_DTNODA            source/mpi/generic/spmd_gather_dtnoda.F
Chd|        SPMD_GLOB_IMAX9               source/mpi/generic/spmd_glob_imax9.F
Chd|        SPMD_GLOB_ISUM9               source/mpi/interfaces/spmd_th.F
Chd|        SPMD_RBCAST                   source/mpi/generic/spmd_rbcast.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE FIND_DT_FOR_TARGETED_ADDED_MASS(MS,STIFN,DTSCA,IGRP_USR,TARGET_DT,
     .                                           PERCENT_ADDMASS,PERCENT_ADDMASS_OLD,TOTMAS,WEIGHT,IGRNOD,
     .                                           ICNDS10)
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
      USE GROUPDEF_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C----------------------------------------------- 
      INTEGER WEIGHT(*),IGRP_USR,ICNDS10(3,*)
      my_real
     .   MS(*),STIFN(*),TARGET_DT,DTSCA,PERCENT_ADDMASS,PERCENT_ADDMASS_OLD,TOTMAS
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD) :: IGRNOD
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "my_allocate.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,COMPT,K,NVAL,SIZ,SIZG,SIZ_MAX,ND,IGROUP
      INTEGER, DIMENSION(:),ALLOCATABLE :: TAGN
      my_real, DIMENSION(:),ALLOCATABLE,TARGET :: 
     .   DT2_L
      my_real, DIMENSION(:),ALLOCATABLE :: 
     .   STF_L,MS_L
      my_real, DIMENSION(:),POINTER ::
     .   TMP
      my_real SUMK,SUMM,TARGET_PERCENT
      INTEGER :: IERROR
      INTEGER, DIMENSION(:), ALLOCATABLE :: PERM
C=======================================================================
C
C--------------------------------------------------------------------------------------
C     DERIVED FROM ADD_MS_L_STAT in starter - computation of time step according to requested % of added mass
C--------------------------------------------------------------------------------------
C 
      SIZ = 0
      ALLOCATE(TAGN(NUMNOD))
      TAGN(1:NUMNOD) = 0
C
C---  Condensed nodes of TETRA10 are excluded
      DO I=1,NS10E
        ND = IABS(ICNDS10(1,I))
        TAGN(ND) = -1
      ENDDO 
C
      IF (IGRP_USR < 0) THEN
        DO I=1,NGRNOD
          IF (-IGRP_USR==IGRNOD(I)%ID) IGROUP = I
        ENDDO
      ELSE
        IGROUP = IGRP_USR
      ENDIF     
C
C---  Count and tag nodes to be taken into account for target_dt computation
      IF (IGROUP > 0) THEN
        DO I=1,IGRNOD(IGROUP)%NENTITY
          N = IGRNOD(IGROUP)%ENTITY(I)
          IF ((WEIGHT(N)==1).AND.(MS(N)/=ZERO).AND.(STIFN(N)>EM20).AND.(TAGN(N)==0)) THEN
            TAGN(N) = 1
            SIZ = SIZ + 1
          ENDIF
        ENDDO
      ELSE
        DO I=1,NUMNOD
          IF ((WEIGHT(I)==1).AND.(MS(I)/=ZERO).AND.(STIFN(I)>EM20).AND.(TAGN(I)==0)) THEN
            TAGN(I) = 1
            SIZ = SIZ + 1
          ENDIF
        ENDDO
      ENDIF
      SIZG = SIZ
      SIZ_MAX = SIZ
C
C---  Counstruction of arrays
      IF (NSPMD > 1) THEN
        CALL SPMD_GLOB_IMAX9(SIZ_MAX,1)
        CALL SPMD_GLOB_ISUM9(SIZG,1)
        IF (ISPMD == 0) THEN
          ALLOCATE(DT2_L(2*SIZG),STF_L(SIZG),MS_L(SIZG))
          SIZ = SIZ_MAX
        ENDIF
        CALL SPMD_GATHER_DTNODA(TAGN,STIFN,MS,WEIGHT,SIZ,DT2_L,STF_L,MS_L)
      ELSE
        ALLOCATE(DT2_L(2*SIZG),STF_L(SIZG),MS_L(SIZG))
        COMPT = 0
        DO I=1,NUMNOD
          IF (TAGN(I) > 0) THEN
            COMPT = COMPT + 1
            DT2_L(COMPT) = MS(I)/STIFN(I)
            MS_L(COMPT) = MS(I)
            STF_L(COMPT) = STIFN(I)
          ENDIF
        ENDDO
      ENDIF
C
      DEALLOCATE(TAGN)
C
      IF (ISPMD == 0) THEN     
C
        TMP => DT2_L(SIZG+1:SIZG*2)
        ALLOCATE( PERM(SIZG ))
C
C  --- Sorting
C
        SUMM = ZERO
        SUMK = ZERO
        DO I=1,SIZG
          TMP(I)=I
          PERM(I) = I
          SUMM = SUMM + MS_L(I)
          SUMK = SUMK + STF_L(I)
        ENDDO

        CALL MYQSORT(SIZG,DT2_L,PERM,IERROR)
        TMP(1:SIZG) = PERM(1:SIZG)

        DEALLOCATE( PERM )
C
C----- determination of target time step
C
C       in case of rst
C
        TARGET_PERCENT = MAX(ZERO,PERCENT_ADDMASS - PERCENT_ADDMASS_OLD)
        PERCENT_ADDMASS_OLD = PERCENT_ADDMASS
C
        NVAL = 1
        CALL FIND_DT_TARGET(MS_L,STF_L,TARGET_DT,TARGET_PERCENT,DT2_L,TMP,DTSCA,TOTMAS,NVAL,SIZG)
C
        DEALLOCATE(DT2_L,STF_L,MS_L)
C
      ENDIF
C
      IF (NSPMD > 1) CALL SPMD_RBCAST(TARGET_DT,TARGET_DT,1,1,0,2)
C
      RETURN
      END
