Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    hm_prelecgrns            ../starter/source/groups/hm_prelecgrns.F
!||--- called by ------------------------------------------------------
!||    lectur                   ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||    hm_get_int_array_index   ../starter/source/devtools/hm_reader/hm_get_int_array_index.F
!||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_read_key       ../starter/source/devtools/hm_reader/hm_option_read_key.F
!||    hm_option_start          ../starter/source/devtools/hm_reader/hm_option_start.F
!||    usr2sys                  ../starter/source/system/sysfus.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod       ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_PRELECGRNS(ITABM1 ,IGRNOD ,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE, NCHARKEY
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITABM1(*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD) :: IGRNOD
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J10(10)
      INTEGER I,J,K,ID,NNOD,NL,IGS,KK,JJ,NENTITY,UID,NN
      CHARACTER(LEN=NCHARTITLE) :: TITR,TITR1
      CHARACTER(LEN=NCHARKEY) :: KEY,KEY2
      CHARACTER MESS*40
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
      DATA MESS/'NODENS GROUP DEFINITION                 '/
C-----------------------------------------------
C    IGRNOD(IGS)%ID : GROUP identifier
C    IGRNOD(IGS)%TITLE : GROUP title
C    IGRNOD(IGS)%NENTITY : Entities (nodes) number of the GROUP
C    IGRNOD(IGS)%GRTYPE : TYPE ( 0-NOEUDS, 1-BRIC, 2-QUAD,   3-SHELL_4N,
!                                4-TRUSS,  5-BEAM, 6-SPRINGS,7-SHELL_3N )
!!                   GRTYPE --- > OBSOLETE
C    IGRNOD(IGS)%SORTED  : FLAG for sorted/unsorted nodes
!                    = 0 -> sorted
!                    = 1 -> unsorted
C    IGRNOD(IGS)%GRPGRP : TYPE of nodal GROUP
!                                 = 1 FOR /GRNOD/NOD
!                                 = 2 FOR /GRNOD/GNROD
!                                 = 0 ALL THE REST
C    IGRNOD(IGS)%LEVEL  : Hierarchy level
!                             (FLAG 'SUBLEVEL DONE' FOR GROUPS OF GROUPS)
!                            = 0 ---> not yet initialized
!                            = 1 ---> done
!  - R2R -
!                              ! R2R_ALL <--- IGROUP2(8,I) = IGROUP2(2,I) -- before splitting
!                              ! R2R_SHARE <--- IGROUP2(9,I)   (shared on boundary subdomain)
C    IGRNOD(IGS)%R2R_ALL   ! Multidomaines -> number of node(elems, parts) before split
C    IGRNOD(IGS)%R2R_SHARE ! shared on boundary subdomain
C-----------------------------------------------
      IS_AVAILABLE = .FALSE.
C=======================================================================
C groupes de noeuds(type NODE,NODENS) + init IGRNOD(IGS)%ID
C=======================================================================
      IGS=0
      TITR1='NODENS GROUP'
!
      CALL HM_OPTION_START('/GRNOD')
!
C      
      ! Loop over all GRNOD
      DO I=1,NGRNOD
C
        ! Read keys and uid
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                          OPTION_ID   = ID,
     .                          OPTION_TITR = TITR  ,
     .                          UNIT_ID     = UID,
     .                          KEYWORD2    = KEY   ,
     .                          KEYWORD3    = KEY2)   
C
        ! Increasing the counter
        IGS=IGS+1
C
        ! Initialization of data structure
        IGRNOD(IGS)%ID=ID
        IGRNOD(IGS)%GRTYPE=0  !!! obsolete
        IGRNOD(IGS)%SORTED=0
        IGRNOD(IGS)%LEVEL=1
        IGRNOD(IGS)%TITLE=TITR
        NN=0
        NNOD=0
        !-----------------------------------------------------------------
        ! GRNOD from NODENS        
        IF(KEY(1:6) == 'NODENS') THEN
!         groupe de noeuds (non sorted)
          CALL HM_GET_INTV('idsmax' ,NENTITY,IS_AVAILABLE,LSUBMODEL)
          DO KK = 1,NENTITY
            CALL HM_GET_INT_ARRAY_INDEX ('ids',JJ  ,KK,IS_AVAILABLE,LSUBMODEL)
            IF (JJ /= 0) THEN
              NNOD = NNOD + 1
            ENDIF
          ENDDO
          IGRNOD(IGS)%NENTITY = NNOD
          IGRNOD(IGS)%GRPGRP  = 1
          CALL MY_ALLOC(IGRNOD(IGS)%ENTITY,NNOD)
          IGRNOD(IGS)%SORTED  = 1
          DO KK = 1,NENTITY
            CALL HM_GET_INT_ARRAY_INDEX ('ids',JJ  ,KK,IS_AVAILABLE,LSUBMODEL)
            IF (JJ /= 0) THEN
              NN = NN + 1
              IGRNOD(IGS)%ENTITY(NN)=USR2SYS(JJ,ITABM1,MESS,ID)
            ENDIF
          ENDDO          
        ENDIF ! IF(LINE(1:1) == '/')
      ENDDO
!-----
      RETURN
      END
