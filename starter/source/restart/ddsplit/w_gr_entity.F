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
Chd|  W_GR_ENTITY_E                 source/restart/ddsplit/w_gr_entity.F
Chd|-- called by -----------
Chd|        W_GROUP_STR                   source/restart/ddsplit/w_group_str.F
Chd|-- calls ---------------
Chd|        FRETITL                       source/starter/freform.F      
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE W_GR_ENTITY_E(IGR  ,NGR  ,LEN_IA ,LENIGR ,CEP,
     .                       CEL    ,PROC ,ESHIFT )
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
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NGR,LEN_IA,LENIGR,CEP(*),CEL(*),PROC,ESHIFT
!
      TYPE (GROUP_) , DIMENSION(NGR) :: IGR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ID,IGU,NENTITY,GRTYPE,TRI,GRPGRP,LEVEL,R2R_ALL,
     .        R2R_SHARE,L_GROUP,ERR,ITITLE1(LTITR),ENTITY,
     .       NENTITY_L(NGR)
      CHARACTER(LEN=nchartitle) :: TITR
!      CHARACTER TITR*nchartitle
      INTEGER, ALLOCATABLE, DIMENSION (:)  ::  IGROUP_L
C-----------------------------------------------
      DO IGU=1,NGR
        TITR    = IGR(IGU)%TITLE
        CALL FRETITL(TITR,ITITLE1,LTITR)
        CALL WRITE_I_C(ITITLE1,LTITR)
      ENDDO ! DO IGU=1,NGR
      LEN_IA = LEN_IA + NGR
!
      ERR = 0
      ALLOCATE (IGROUP_L(LENIGR), STAT=ERR)
      IGROUP_L(1:LENIGR) = 0  
!
      DO IGU=1,NGR
        NENTITY  = IGR(IGU)%NENTITY
        NENTITY_L(IGU) = 0
        DO J=1,NENTITY
          ENTITY = IGR(IGU)%ENTITY(J)
          IF (ENTITY > 0) THEN
            ENTITY = ENTITY + ESHIFT
            IF (CEP(ENTITY) == PROC)  NENTITY_L(IGU) = NENTITY_L(IGU) + 1
          ENDIF
        ENDDO
      ENDDO ! DO IGU=1,NGR
!
      L_GROUP = 0
!
      DO IGU=1,NGR
        ID       = IGR(IGU)%ID
        NENTITY  = IGR(IGU)%NENTITY
        GRTYPE   = IGR(IGU)%GRTYPE
        TRI      = IGR(IGU)%SORTED
        GRPGRP   = IGR(IGU)%GRPGRP
        LEVEL    = IGR(IGU)%LEVEL
        TITR     = IGR(IGU)%TITLE
        R2R_ALL  = IGR(IGU)%R2R_ALL
        R2R_SHARE= IGR(IGU)%R2R_SHARE
        IGROUP_L(L_GROUP+1) = ID
          L_GROUP = L_GROUP+1
        IGROUP_L(L_GROUP+1) = NENTITY_L(IGU)
          L_GROUP = L_GROUP+1
        IGROUP_L(L_GROUP+1) = GRTYPE
          L_GROUP = L_GROUP+1
        IGROUP_L(L_GROUP+1) = TRI
          L_GROUP = L_GROUP+1
        IGROUP_L(L_GROUP+1) = GRPGRP
          L_GROUP = L_GROUP+1
        IGROUP_L(L_GROUP+1) = LEVEL
          L_GROUP = L_GROUP+1
!        IGROUP_L(L_GROUP+1) = TITR  !!! written above for all groups
!          L_GROUP = L_GROUP+1
        IGROUP_L(L_GROUP+1) = R2R_ALL
          L_GROUP = L_GROUP+1
        IGROUP_L(L_GROUP+1) = R2R_SHARE
          L_GROUP = L_GROUP+1
!
! GROUP ENTITIES
!
        DO J=1,NENTITY
          ENTITY = IGR(IGU)%ENTITY(J)
          IF (ENTITY > 0) THEN
            ENTITY = ENTITY + ESHIFT
            IF (CEP(ENTITY) == PROC) THEN
               IGROUP_L(L_GROUP+1) = CEL(ENTITY)
               L_GROUP = L_GROUP+1
            ENDIF
          ENDIF
        ENDDO
      ENDDO ! DO IGU=1,NGR
!---------
      CALL WRITE_I_C(IGROUP_L,L_GROUP)
!---------
      DEALLOCATE (IGROUP_L)
!---------
      LEN_IA = LEN_IA + L_GROUP
!---------
      RETURN
      END
Chd|====================================================================
Chd|  W_GR_ENTITY_P                 source/restart/ddsplit/w_gr_entity.F
Chd|-- called by -----------
Chd|        W_GROUP_STR                   source/restart/ddsplit/w_group_str.F
Chd|-- calls ---------------
Chd|        FRETITL                       source/starter/freform.F      
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE W_GR_ENTITY_P(IGR  ,NGR  ,LEN_IA ,LENIGR ,CEP,
     .                         CEL  ,PROC )
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
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NGR,LEN_IA,LENIGR,CEP(*),CEL(*),PROC
!
      TYPE (GROUP_) , DIMENSION(NGR) :: IGR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ID,IGU,NENTITY,GRTYPE,TRI,GRPGRP,LEVEL,R2R_ALL,
     .        R2R_SHARE,L_GROUP,ERR,ITITLE2(LTITR),ENTITY,NENTITY_L
      CHARACTER(LEN=nchartitle) :: TITR
!      CHARACTER TITR*nchartitle
      INTEGER, ALLOCATABLE, DIMENSION (:)  ::  IGROUP_L
C-----------------------------------------------
      DO IGU=1,NGR
        TITR    = IGR(IGU)%TITLE
        CALL FRETITL(TITR,ITITLE2,LTITR)
        CALL WRITE_I_C(ITITLE2,LTITR)
      ENDDO ! DO IGU=1,NGR
      LEN_IA = LEN_IA + NGR
!
      ERR = 0
      ALLOCATE (IGROUP_L(LENIGR), STAT=ERR)
      IGROUP_L(1:LENIGR) = 0  
!
      L_GROUP = 0
!
      DO IGU=1,NGR
        ID       = IGR(IGU)%ID
        NENTITY  = IGR(IGU)%NENTITY
        GRTYPE   = IGR(IGU)%GRTYPE
        TRI      = IGR(IGU)%SORTED
        GRPGRP   = IGR(IGU)%GRPGRP
        LEVEL    = IGR(IGU)%LEVEL
        TITR     = IGR(IGU)%TITLE
        R2R_ALL  = IGR(IGU)%R2R_ALL
        R2R_SHARE= IGR(IGU)%R2R_SHARE
!
! GROUP ENTITIES
!
!
        IGROUP_L(L_GROUP+1) = ID
          L_GROUP = L_GROUP+1
        IGROUP_L(L_GROUP+1) = NENTITY
          L_GROUP = L_GROUP+1
        IGROUP_L(L_GROUP+1) = GRTYPE
          L_GROUP = L_GROUP+1
        IGROUP_L(L_GROUP+1) = TRI
          L_GROUP = L_GROUP+1
        IGROUP_L(L_GROUP+1) = GRPGRP
          L_GROUP = L_GROUP+1
        IGROUP_L(L_GROUP+1) = LEVEL
          L_GROUP = L_GROUP+1
!        IGROUP_L(L_GROUP+1) = TITR  !!! written above for all groups
!          L_GROUP = L_GROUP+1
        IGROUP_L(L_GROUP+1) = R2R_ALL
          L_GROUP = L_GROUP+1
        IGROUP_L(L_GROUP+1) = R2R_SHARE
          L_GROUP = L_GROUP+1
!
! GROUP ENTITIES
!
        DO J=1,NENTITY
          ENTITY = IGR(IGU)%ENTITY(J)
          IF (ENTITY > 0) THEN
            IGROUP_L(L_GROUP+1) = ENTITY
            L_GROUP = L_GROUP+1
          ENDIF
        ENDDO
      ENDDO ! DO IGU=1,NGR
!---------
      CALL WRITE_I_C(IGROUP_L,L_GROUP)
!---------
      DEALLOCATE (IGROUP_L)
!---------
      LEN_IA = LEN_IA + L_GROUP
!---------
      RETURN
      END
Chd|====================================================================
Chd|  W_GR_ENTITY_N                 source/restart/ddsplit/w_gr_entity.F
Chd|-- called by -----------
Chd|        W_GROUP_STR                   source/restart/ddsplit/w_group_str.F
Chd|-- calls ---------------
Chd|        FRETITL                       source/starter/freform.F      
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE W_GR_ENTITY_N(IGR  ,NGR       ,LEN_IA ,LENIGR ,NODLOCAL,
     .                         PROC ,FRONTB_R2R,NUMNOD_L)
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
#include      "scr17_c.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NGR,LEN_IA,LENIGR,PROC,
     .        FRONTB_R2R(SFRONTB_R2R,*),NUMNOD_L
      INTEGER, DIMENSION(*), INTENT(IN) :: NODLOCAL
!
      TYPE (GROUP_) , DIMENSION(NGR) :: IGR
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
!       NODLOCAL  : integer, dimension=NUMNOD
!                   gives the local ID of a global element
!                   --> used here to avoid NLOCAL call (the NLOCAL perf is bad)
!                       NODLOCAL /= 0 if the element is on the current domain/processor
!                       and =0 if the element is not on the current domain
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
!      INTEGER  NLOCAL
!      EXTERNAL NLOCAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ID,IGU,NOD,NNOD,GRTYPE,TRI,GRPGRP,LEVEL,R2R_ALL,
     .        R2R_SHARE,L_GROUP,ERR,ITITLE3(LTITR),NNOD_LOC(NGR)
      CHARACTER TITR*nchartitle
      INTEGER, ALLOCATABLE, DIMENSION (:)  ::  IGROUP_L
C-----------------------------------------------
      DO IGU=1,NGR
        TITR    = IGR(IGU)%TITLE
        CALL FRETITL(TITR,ITITLE3,LTITR)
        CALL WRITE_I_C(ITITLE3,LTITR)
      ENDDO ! DO IGU=1,NGR
      LEN_IA = LEN_IA + NGR
!
      ERR = 0
      ALLOCATE (IGROUP_L(LENIGR), STAT=ERR)
      IGROUP_L(1:LENIGR) = 0  
!
      DO IGU=1,NGR
        NNOD  = IGR(IGU)%NENTITY
        NNOD_LOC(IGU) = 0
        DO J=1,NNOD
          NOD = IGR(IGU)%ENTITY(J)
          IF (NOD > 0) THEN
!---------multidomaines -> modif domdec
            IF ((NSUBDOM.GT.0).AND.(IDDOM.EQ.0)) THEN
              IF (FRONTB_R2R(NOD,PROC+1).EQ.IGU) CYCLE
            ENDIF
!---------
            IF( NODLOCAL(NOD)/=0.AND.NODLOCAL(NOD)<=NUMNOD_L ) NNOD_LOC(IGU) = NNOD_LOC(IGU) + 1
          ENDIF
        ENDDO
      ENDDO ! DO IGU=1,NGR
!
      L_GROUP = 0
!
      DO IGU=1,NGR
        ID       = IGR(IGU)%ID
        NNOD     = IGR(IGU)%NENTITY
        GRTYPE   = IGR(IGU)%GRTYPE
        TRI      = IGR(IGU)%SORTED
        GRPGRP   = IGR(IGU)%GRPGRP
        LEVEL    = IGR(IGU)%LEVEL
        TITR     = IGR(IGU)%TITLE
        R2R_ALL  = IGR(IGU)%R2R_ALL
        R2R_SHARE= IGR(IGU)%R2R_SHARE
        IGROUP_L(L_GROUP+1) = ID
          L_GROUP = L_GROUP+1
        IGROUP_L(L_GROUP+1) = NNOD_LOC(IGU)
          L_GROUP = L_GROUP+1
        IGROUP_L(L_GROUP+1) = GRTYPE
          L_GROUP = L_GROUP+1
        IGROUP_L(L_GROUP+1) = TRI
          L_GROUP = L_GROUP+1
        IGROUP_L(L_GROUP+1) = GRPGRP
          L_GROUP = L_GROUP+1
        IGROUP_L(L_GROUP+1) = LEVEL
          L_GROUP = L_GROUP+1
!        IGROUP_L(L_GROUP+1) = TITR  !!! written above for all groups
!          L_GROUP = L_GROUP+1
        IGROUP_L(L_GROUP+1) = R2R_ALL
          L_GROUP = L_GROUP+1
        IGROUP_L(L_GROUP+1) = R2R_SHARE
          L_GROUP = L_GROUP+1
!
! GROUP ENTITIES (--- NODES ---)
!
        DO J=1,NNOD
          NOD = IGR(IGU)%ENTITY(J)
          IF (NOD > 0) THEN
!---------multidomaines -> modif domdec
            IF ((NSUBDOM.GT.0).AND.(IDDOM.EQ.0)) THEN
              IF (FRONTB_R2R(NOD,PROC+1).EQ.IGU) CYCLE
            ENDIF
!---------
            IF( NODLOCAL(NOD)/=0.AND.NODLOCAL(NOD)<=NUMNOD_L ) THEN
              IGROUP_L(L_GROUP+1) = NODLOCAL(NOD)
              L_GROUP = L_GROUP+1
            ENDIF
          ENDIF
        ENDDO ! DO J=1,NNOD
      ENDDO ! DO IGU=1,NGR
!---------
      CALL WRITE_I_C(IGROUP_L,L_GROUP)
!---------
      DEALLOCATE (IGROUP_L)
!---------
      LEN_IA = LEN_IA + L_GROUP
!---------
      RETURN
      END
