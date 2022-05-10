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
Chd|  ST_QAPRINT_SET                source/output/qaprint/st_qaprint_set.F
Chd|-- called by -----------
Chd|        ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|-- calls ---------------
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_2INDEXES     source/devtools/hm_reader/hm_get_int_array_2indexes.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_GET_STRING                 source/devtools/hm_reader/hm_get_string.F
Chd|        HM_GET_STRING_INDEX           source/devtools/hm_reader/hm_get_string_index.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE ST_QAPRINT_SET(SET    ,LSUBMODEL,ITAB     ,IGRNOD ,IGRPART,
     .                          IPART  ,IGRBRIC  ,IGRSH4N  ,IGRSH3N,IGRQUAD,
     .                          IGRBEAM,IGRTRUSS ,IGRSPRING,IGRSURF,IGRSLIN,
     .                          IXC    ,IXTG     ,IXQ      ,IXP    ,IXT    ,
     .                          IXR    ,IXS      )
C=======================================================================
C   M o d u l e s
C-----------------------------------------------
      USE QA_OUT_MOD
      USE SETDEF_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "tabsiz_c.inc"
#include      "units_c.inc"
#include      "submod_c.inc"
#include      "scr03_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: ITAB(*),IPART(LIPART1,*),IXC(NIXC,*),
     .   IXTG(NIXTG,*),IXQ(NIXQ,*),IXP(NIXP,*),IXT(NIXT,*),IXR(NIXR,*),
     .   IXS(NIXS,*)
!
      TYPE (SET_) , DIMENSION(NSETS) :: SET
      TYPE (SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
      TYPE (GROUP_)  , INTENT(IN), DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (GROUP_)  , INTENT(IN), DIMENSION(NGRPART) :: IGRPART
      TYPE (GROUP_)  , INTENT(IN), DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE (GROUP_)  , INTENT(IN), DIMENSION(NGRSHEL) :: IGRSH4N
      TYPE (GROUP_)  , INTENT(IN), DIMENSION(NGRSH3N) :: IGRSH3N
      TYPE (GROUP_)  , INTENT(IN), DIMENSION(NGRQUAD) :: IGRQUAD
      TYPE (GROUP_)  , INTENT(IN), DIMENSION(NGRBEAM) :: IGRBEAM
      TYPE (GROUP_)  , INTENT(IN), DIMENSION(NGRTRUS) :: IGRTRUSS
      TYPE (GROUP_)  , INTENT(IN), DIMENSION(NGRSPRI) :: IGRSPRING
      TYPE (SURF_)   , INTENT(IN), DIMENSION(NSURF)   :: IGRSURF
      TYPE (SURF_)   , INTENT(IN), DIMENSION(NSLIN)   :: IGRSLIN
C--------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,ID,IGS,CLAUSES_MAX,ISET_TYPE,ITMP,ICODE,IDS_MAX,IDS,
     .        OPT_,OPT_D,OPT_O,OPT_G,OPT_B,OPT_A,OPT_E,OPT_I,OPT_C,
     .        IGR,NOD,NB_NODE,NB_PART,IP,IE,NB_SOLID,NB_SH4N,NB_SH3N,
     .        NB_QUAD,NB_TRIA,NB_BEAM,NB_TRUSS,NB_SPRING,NB_SURF_SEG,
     .        NB_LINE_SEG,ISEG
      CHARACTER(LEN = nchartitle) :: TITLE
      CHARACTER(LEN = ncharfield) :: KEYSET,SET_TYPE
      CHARACTER(LEN = ncharkey)   :: KEY
      CHARACTER (LEN=255) :: VARNAME
      DOUBLE PRECISION TEMP_DOUBLE
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
      IS_AVAILABLE = .FALSE.
!
!     not calling regularly
!!      CALL HM_DEBUG_PRINT_OPTION('/SET')
!
      CALL HM_OPTION_START('/SET')
!
      IF (MYQAKEY('/SET')) THEN
!
        IF (NSETS > 0) THEN
!
          DO IGS = 1, NSETS
!---
            CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                              OPTION_ID   = ID,
     .                              OPTION_TITR = TITLE,
     .                              KEYWORD2    = KEY)
            WRITE(VARNAME,'(A)') TRIM(TITLE)
            CALL QAPRINT(TITLE(1:LEN_TRIM(VARNAME)), ID, 0.0_8)
!---
            CALL HM_GET_STRING('set_Type', SET_TYPE ,ncharfield, IS_AVAILABLE)
!-----------------------
! issue 'SET_TYPE'  ---> read one more character than the SET_TYPE
! ===> workarround
            ITMP = LEN(TRIM(SET_TYPE))
            IF (ITMP > 0) THEN
              ICODE = IACHAR(SET_TYPE(ITMP:ITMP))
              IF (ICODE == 0)  SET_TYPE(ITMP:ITMP)=' '
            ENDIF
!-----------------------
            WRITE(VARNAME,'(A,I0,A)') 'SET_',ID,'_'//TRIM(SET_TYPE)
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), ID, 0.0_8)
!---
            CALL HM_GET_INTV('iset_Type',  ISET_TYPE,IS_AVAILABLE,LSUBMODEL)
            WRITE(VARNAME,'(A,I0,A)') 'SET_',ID,'_'//'iset_Type'
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), ISET_TYPE, 0.0_8)
!---
            CALL HM_GET_INTV('clausesmax',CLAUSES_MAX,IS_AVAILABLE,LSUBMODEL)
!
            DO J=1,CLAUSES_MAX  ! max KEY's of the current /SET
              CALL HM_GET_STRING_INDEX('KEY_type', KEYSET, J, ncharline, IS_AVAILABLE)
!-----------------------
! issue 'KEYSET'  ---> read one more character than the KEYSET
! ===> workarround
              ITMP = LEN(TRIM(KEYSET))
              ICODE = IACHAR(KEYSET(ITMP:ITMP))
              IF (ICODE == 0)  KEYSET(ITMP:ITMP)=' '
!-----------------------
!---
!!              CALL HM_GET_INT_ARRAY_INDEX('opt_' ,OPT_ ,J,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('opt_D',OPT_D,J,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('opt_O',OPT_O,J,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('opt_G',OPT_G,J,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('opt_B',OPT_B,J,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('opt_A',OPT_A,J,IS_AVAILABLE,LSUBMODEL) 
              CALL HM_GET_INT_ARRAY_INDEX('opt_E',OPT_E,J,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('opt_I',OPT_I,J,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('opt_C',OPT_C,J,IS_AVAILABLE,LSUBMODEL)
!---
!!              IF (OPT_ == 1) THEN
!!                WRITE(VARNAME,'(A,I0,A)') 'SET_',ID,'_'//'opt_'
!!                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),OPT_,0.0_8)
!!              ENDIF ! IF (OPT_ == 1)
              IF (OPT_D == 1) THEN
                WRITE(VARNAME,'(A,I0,A)') 'SET_',ID,'_'//'opt_D'
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),OPT_D,0.0_8)
              ENDIF ! IF (OPT_D == 1)
              IF (OPT_O == 1) THEN
                WRITE(VARNAME,'(A,I0,A)') 'SET_',ID,'_'//'opt_O'
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),OPT_O,0.0_8)
              ENDIF ! IF (OPT_O == 1)
              IF (OPT_G == 1) THEN
                WRITE(VARNAME,'(A,I0,A)') 'SET_',ID,'_'//'opt_G'
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),OPT_G,0.0_8)
              ENDIF ! IF (OPT_G == 1)
              IF (OPT_B == 1) THEN
                WRITE(VARNAME,'(A,I0,A)') 'SET_',ID,'_'//'opt_B'
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),OPT_B,0.0_8)
              ENDIF ! IF (OPT_B == 1)
              IF (opt_A == 1) THEN
                WRITE(VARNAME,'(A,I0,A)') 'SET_',ID,'_'//'opt_A'
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),OPT_A,0.0_8)
              ENDIF ! IF (opt_A == 1)
              IF (OPT_E == 1) THEN
                WRITE(VARNAME,'(A,I0,A)') 'SET_',ID,'_'//'opt_E'
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),OPT_E,0.0_8)
              ENDIF ! IF (OPT_E == 1)
              IF (OPT_I == 1) THEN
                WRITE(VARNAME,'(A,I0,A)') 'SET_',ID,'_'//'opt_I'
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),OPT_I,0.0_8)
              ENDIF ! IF (OPT_I == 1)
              IF (OPT_C == 1) THEN
                WRITE(VARNAME,'(A,I0,A)') 'SET_',ID,'_'//'opt_C'
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),OPT_C,0.0_8)
              ENDIF ! IF (OPT_C == 1)
!---
              CALL HM_GET_INT_ARRAY_INDEX('idsmax' ,IDS_MAX ,J,IS_AVAILABLE,LSUBMODEL)
              DO K=1,IDS_MAX
                CALL HM_GET_INT_ARRAY_2INDEXES('ids',IDS,J,K,IS_AVAILABLE,LSUBMODEL)
                WRITE(VARNAME,'(A,I0,A,I0)') 'SET_',ID,'_'//TRIM(KEYSET)//'_',K
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IDS,0.0_8)
              ENDDO ! DO K=1,IDS_MAX
!---
            ENDDO ! DO J=1,CLAUSES_MAX
!---
!   printout new groupes (grnod, grpart, grelem, ...) generated by /SET
!---
!
!---
!           --- New /SET grnod --
!---
            IF( SET(IGS)%SET_ACTIV == 0 ) CYCLE

            NB_NODE = SET(IGS)%NB_NODE

            IF (NB_NODE > 0) THEN
              IGR = SET(IGS)%SET_GRNOD_ID
              WRITE(VARNAME,'(A,I0,A)') 'SET_',ID,'_'//'GRNOD'
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IGRNOD(IGR)%ID,0.0_8)
              WRITE(VARNAME,'(A,I0,A)') 'SET_',ID,'_'//'GRNOD_NB_NODE'
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NB_NODE,0.0_8)
              DO N = 1,NB_NODE
                NOD = IGRNOD(IGR)%ENTITY(N)
                WRITE(VARNAME,'(A,I0,A,I0)') 'SET_',ID,'_'//'NODE'//'_',N
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ITAB(NOD),0.0_8)
              ENDDO
            ENDIF ! IF (NB_NODE > 0)
!---
!           --- New /SET grpart --
!---
            NB_PART = SET(IGS)%NB_PART
            IF (NB_PART > 0) THEN
              IGR = SET(IGS)%SET_GRPART_ID
              WRITE(VARNAME,'(A,I0,A)') 'SET_',ID,'_'//'GRPART'
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IGRPART(IGR)%ID,0.0_8)
              WRITE(VARNAME,'(A,I0,A)') 'SET_',ID,'_'//'GRPART_NB_PART'
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NB_PART,0.0_8)
              DO N = 1,NB_PART
                IP = IGRPART(IGR)%ENTITY(N)
                WRITE(VARNAME,'(A,I0,A,I0)') 'SET_',ID,'_'//'PART'//'_',N
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IPART(4,IP),0.0_8)
              ENDDO
            ENDIF ! IF (NB_PART > 0)
!---
!           --- New /SET grelem --
!---
            ! solid
            NB_SOLID = SET(IGS)%NB_SOLID
            IF (NB_SOLID > 0) THEN
              IGR = SET(IGS)%SET_GRSOLID_ID
              WRITE(VARNAME,'(A,I0,A)') 'SET_',ID,'_'//'GRBRIC'
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IGRBRIC(IGR)%ID,0.0_8)
              WRITE(VARNAME,'(A,I0,A)') 'SET_',ID,'_'//'GRBRIC_NB_SOLID'
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NB_SOLID,0.0_8)
              DO N = 1,NB_SOLID
                IE = IGRBRIC(IGR)%ENTITY(N)
                WRITE(VARNAME,'(A,I0,A,I0)') 'SET_',ID,'_'//'SOLID'//'_',N
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IXS(NIXS,IE),0.0_8)
              ENDDO
            ENDIF ! IF (NB_SOLID > 0)
!
            ! sh4n
            NB_SH4N = SET(IGS)%NB_SH4N
            IF (NB_SH4N > 0) THEN
              IGR = SET(IGS)%SET_GRSH4N_ID
              WRITE(VARNAME,'(A,I0,A)') 'SET_',ID,'_'//'GRSH4N'
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IGRSH4N(IGR)%ID,0.0_8)
              WRITE(VARNAME,'(A,I0,A)') 'SET_',ID,'_'//'GRSH4N_NB_SH4N'
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NB_SH4N,0.0_8)
              DO N = 1,NB_SH4N
                IE = IGRSH4N(IGR)%ENTITY(N)
                WRITE(VARNAME,'(A,I0,A,I0)') 'SET_',ID,'_'//'SHELL'//'_',N
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IXC(NIXC,IE),0.0_8)
              ENDDO
            ENDIF ! IF (NB_SH4N > 0)
!
            ! sh3n
            NB_SH3N = SET(IGS)%NB_SH3N
            IF (NB_SH3N > 0) THEN
              IGR = SET(IGS)%SET_GRSH3N_ID
              WRITE(VARNAME,'(A,I0,A)') 'SET_',ID,'_'//'GRSH3N'
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IGRSH3N(IGR)%ID,0.0_8)
              WRITE(VARNAME,'(A,I0,A)') 'SET_',ID,'_'//'GRSH3N_NB_SH3N'
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NB_SH3N,0.0_8)
              DO N = 1,NB_SH3N
                IE = IGRSH3N(IGR)%ENTITY(N)
                WRITE(VARNAME,'(A,I0,A,I0)') 'SET_',ID,'_'//'SH3N'//'_',N
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IXTG(NIXTG,IE),0.0_8)
              ENDDO
            ENDIF ! IF (NB_SH3N > 0)
!
            ! quad
            NB_QUAD = SET(IGS)%NB_QUAD
            IF (NB_QUAD > 0) THEN
              IGR = SET(IGS)%SET_GRQUAD_ID
              WRITE(VARNAME,'(A,I0,A)') 'SET_',ID,'_'//'GRQUAD'
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IGRQUAD(IGR)%ID,0.0_8)
              WRITE(VARNAME,'(A,I0,A)') 'SET_',ID,'_'//'GRQUAD_NB_QUAD'
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NB_QUAD,0.0_8)
              DO N = 1,NB_QUAD
                IE = IGRQUAD(IGR)%ENTITY(N)
                WRITE(VARNAME,'(A,I0,A,I0)') 'SET_',ID,'_'//'QUAD'//'_',N
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IXQ(NIXQ,IE),0.0_8)
              ENDDO
            ENDIF ! IF (NB_QUAD > 0)
!
            ! tria
            NB_TRIA = SET(IGS)%NB_TRIA
            IF (NB_TRIA > 0) THEN
              IGR = SET(IGS)%SET_GRTRIA_ID
              WRITE(VARNAME,'(A,I0,A)') 'SET_',ID,'_'//'GRTRIA'
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IGRSH3N(IGR)%ID,0.0_8)
              WRITE(VARNAME,'(A,I0,A)') 'SET_',ID,'_'//'GRTRIA_NB_TRIA'
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NB_TRIA,0.0_8)
              DO N = 1,NB_TRIA
                IE = IGRSH3N(IGR)%ENTITY(N)
                WRITE(VARNAME,'(A,I0,A,I0)') 'SET_',ID,'_'//'TRIA'//'_',N
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IXTG(NIXTG,IE),0.0_8)
              ENDDO
            ENDIF ! IF (NB_TRIA > 0)
!
            ! beam
            NB_BEAM = SET(IGS)%NB_BEAM
            IF (NB_BEAM > 0) THEN
              IGR = SET(IGS)%SET_GRBEAM_ID
              WRITE(VARNAME,'(A,I0,A)') 'SET_',ID,'_'//'GRBEAM'
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IGRBEAM(IGR)%ID,0.0_8)
              WRITE(VARNAME,'(A,I0,A)') 'SET_',ID,'_'//'GRBEAM_NB_BEAM'
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NB_BEAM,0.0_8)
              DO N = 1,NB_BEAM
                IE = IGRBEAM(IGR)%ENTITY(N)
                WRITE(VARNAME,'(A,I0,A,I0)') 'SET_',ID,'_'//'BEAM'//'_',N
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IXP(NIXP,IE),0.0_8)
              ENDDO
            ENDIF ! IF (NB_BEAM > 0)
!
            ! truss
            NB_TRUSS = SET(IGS)%NB_TRUSS
            IF (NB_TRUSS > 0) THEN
              IGR = SET(IGS)%SET_GRTRUSS_ID
              WRITE(VARNAME,'(A,I0,A)') 'SET_',ID,'_'//'GRTRUSS'
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IGRTRUSS(IGR)%ID,0.0_8)
              WRITE(VARNAME,'(A,I0,A)') 'SET_',ID,'_'//'GRTRUSS_NB_TRUSS'
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NB_TRUSS,0.0_8)
              DO N = 1,NB_TRUSS
                IE = IGRTRUSS(IGR)%ENTITY(N)
                WRITE(VARNAME,'(A,I0,A,I0)') 'SET_',ID,'_'//'TRUSS'//'_',N
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IXT(NIXT,IE),0.0_8)
              ENDDO
            ENDIF ! IF (NB_TRUSS > 0)
!
            ! spring
            NB_SPRING = SET(IGS)%NB_SPRING
            IF (NB_SPRING > 0) THEN
              IGR = SET(IGS)%SET_GRSPRING_ID
              WRITE(VARNAME,'(A,I0,A)') 'SET_',ID,'_'//'GRSPRING'
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IGRSPRING(IGR)%ID,0.0_8)
              WRITE(VARNAME,'(A,I0,A)') 'SET_',ID,'_'//'GRSPRING_NB_SPRING'
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NB_SPRING,0.0_8)
              DO N = 1,NB_SPRING
                IE = IGRSPRING(IGR)%ENTITY(N)
                WRITE(VARNAME,'(A,I0,A,I0)') 'SET_',ID,'_'//'SPRING'//'_',N
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IXR(NIXR,IE),0.0_8)
              ENDDO
            ENDIF ! IF (NB_SPRING > 0)
!---
!           --- New /SET grsurf --
!---
            ! surface segments
            NB_SURF_SEG = SET(IGS)%HAS_SURF_SEG
            IF (NB_SURF_SEG > 0) THEN
              IGR = SET(IGS)%SET_NSURF_ID
              WRITE(VARNAME,'(A,I0,A)') 'SET_',ID,'_'//'SURFACE'
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IGRSURF(IGR)%ID,0.0_8)
              WRITE(VARNAME,'(A,I0,A)') 'SET_',ID,'_'//'SURFACE_NB_SEG'
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NB_SURF_SEG,0.0_8)
              DO N = 1,NB_SURF_SEG
                WRITE(VARNAME,'(A,I0,A,I0,A)') 'SET_',ID,'_'//'SURFACE_SEG_',N,'_NODE_1'
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ITAB(IGRSURF(IGR)%NODES(N,1)),0.0_8)
                WRITE(VARNAME,'(A,I0,A,I0,A)') 'SET_',ID,'_'//'SURFACE_SEG_',N,'_NODE_2'
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ITAB(IGRSURF(IGR)%NODES(N,2)),0.0_8)
                WRITE(VARNAME,'(A,I0,A,I0,A)') 'SET_',ID,'_'//'SURFACE_SEG_',N,'_NODE_3'
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ITAB(IGRSURF(IGR)%NODES(N,3)),0.0_8)
                WRITE(VARNAME,'(A,I0,A,I0,A)') 'SET_',ID,'_'//'SURFACE_SEG_',N,'_NODE_4'
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ITAB(IGRSURF(IGR)%NODES(N,4)),0.0_8)
                WRITE(VARNAME,'(A,I0,A,I0,A)') 'SET_',ID,'_'//'SURFACE_SEG_',N,'_ELTYP'
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IGRSURF(IGR)%ELTYP(N),0.0_8)
                WRITE(VARNAME,'(A,I0,A,I0,A)') 'SET_',ID,'_'//'SURFACE_SEG_',N,'_ELEM'
                IF (IGRSURF(IGR)%ELTYP(N) == 3 ) THEN ! SH4N
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IXC(NIXC,IGRSURF(IGR)%ELEM(N)),0.0_8)
                ELSEIF (IGRSURF(IGR)%ELTYP(N) == 7 ) THEN ! SH3N
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IXTG(NIXTG,IGRSURF(IGR)%ELEM(N)),0.0_8)
                ELSEIF (IGRSURF(IGR)%ELTYP(N) == 1 ) THEN ! SOLID
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IXS(NIXS,IGRSURF(IGR)%ELEM(N)),0.0_8)
                ENDIF ! IF (IGRSURF(IGR)%ELTYP(N) == 3 )
              ENDDO
            ENDIF ! IF (NB_SURF_SEG > 0)
!---
!           --- New /SET grline --
!---
            ! line segments
            NB_LINE_SEG = SET(IGS)%HAS_LINE_SEG
            IF (NB_LINE_SEG > 0) THEN
              IGR = SET(IGS)%SET_NSLIN_ID
              WRITE(VARNAME,'(A,I0,A)') 'SET_',ID,'_'//'LINE'
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IGRSLIN(IGR)%ID,0.0_8)
              WRITE(VARNAME,'(A,I0,A)') 'SET_',ID,'_'//'LINE_NB_SEG'
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NB_LINE_SEG,0.0_8)
              DO N = 1,NB_LINE_SEG
                WRITE(VARNAME,'(A,I0,A,I0,A)') 'SET_',ID,'_'//'LINE_SEG_',N,'_NODE_1'
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ITAB(IGRSLIN(IGR)%NODES(N,1)),0.0_8)
                WRITE(VARNAME,'(A,I0,A,I0,A)') 'SET_',ID,'_'//'LINE_SEG_',N,'_NODE_2'
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ITAB(IGRSLIN(IGR)%NODES(N,2)),0.0_8)
                WRITE(VARNAME,'(A,I0,A,I0,A)') 'SET_',ID,'_'//'LINE_SEG_',N,'_ELTYP'
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IGRSLIN(IGR)%ELTYP(N),0.0_8)
                WRITE(VARNAME,'(A,I0,A,I0,A)') 'SET_',ID,'_'//'LINE_SEG_',N,'_ELEM'
                IF (IGRSLIN(IGR)%ELTYP(N) == 3 ) THEN ! SH4N
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IXC(NIXC,IGRSLIN(IGR)%ELEM(N)),0.0_8)
                ELSEIF (IGRSLIN(IGR)%ELTYP(N) == 7 ) THEN ! SH3N
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IXTG(NIXTG,IGRSLIN(IGR)%ELEM(N)),0.0_8)
                ELSEIF (IGRSLIN(IGR)%ELTYP(N) == 1 ) THEN ! SOLID
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IXS(NIXS,IGRSLIN(IGR)%ELEM(N)),0.0_8)
                ELSEIF (IGRSLIN(IGR)%ELTYP(N) == 2 ) THEN ! QUAD
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IXQ(NIXQ,IGRSLIN(IGR)%ELEM(N)),0.0_8)
                ENDIF ! IF (IGRSLIN(IGR)%ELTYP(N) == 3 )
              ENDDO
            ENDIF ! IF (NB_LINE_SEG > 0)
!---
          ENDDO ! DO KK = 1, NSETS
        ENDIF ! IF (NSETS > 0)
      ENDIF ! IF (MYQAKEY('/SET'))
C-----------------------------------------------------------------------
      RETURN
      END
