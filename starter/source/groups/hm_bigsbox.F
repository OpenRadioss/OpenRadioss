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
Chd|  HM_BIGSBOX                    source/groups/hm_bigsbox.F    
Chd|-- called by -----------
Chd|        HM_READ_LINES                 source/groups/hm_read_lines.F 
Chd|        HM_READ_SURF                  source/groups/hm_read_surf.F  
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        BOXBOXS                       source/model/box/boxbox.F     
Chd|        BOX_SURF_SH                   source/model/box/bigbox.F     
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_BIGSBOX(NUMEL ,IX     ,NIX    ,NIX1 ,NIX2,IELTYP,
     .                   X     ,        NSEG ,FLAG,SKEW  ,
     .                   ISKN  ,ISURF0 ,ITABM1 ,IBOX,
     .                   ID    ,IBUFBOX,ISURFLIN,IADB,KEY   ,
     .                   SBUFBOX,TITR  ,MESS   ,TAGSHELLBOX,
     .                   NN    ,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE OPTIONDEF_MOD
      USE HM_OPTION_READ_MOD
      USE SUBMODEL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIX,IX(NIX,*),NIX1,NIX2,NUMEL,IELTYP,
     .        NSEG,FLAG,ISKN(LISKN,*),ISURF0,
     .        ITABM1(*),IBUFBOX(*),
     .        IADB,SBUFBOX,TAGSHELLBOX(*),NN
      MY_REAL
     .        X(3,*),SKEW(LSKEW,*)
      CHARACTER KEY*4,MESS*40
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------
      TYPE (SURF_) :: ISURFLIN
      TYPE (BOX_)  , DIMENSION(NBBOX)  :: IBOX
      TYPE(SUBMODEL_DATA) LSUBMODEL(NSUBMOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,JJ,K,K1,J,JREC,ISK,BOXTYPE,ISU,TAGN(NUMEL),
     .    ITYPE,IADBOX,IDB,NBOX,ID,IDBX,BOXSEG,IADISU,
     .    ICOUNT,ITER,FLAGG,NIXEL
      MY_REAL
     .    DIAM,XP1,YP1,ZP1,XP2,YP2,ZP2,NODINB(3)
      CHARACTER BOX*3
      LOGICAL BOOL,IS_AVAILABLE, IS_ENCRYPTED
C=======================================================================
       DO I=1,NBBOX
         IBOX(I)%NBLEVELS = 0
         IBOX(I)%LEVEL = 1
         IBOX(I)%ACTIBOX = 0
         IF(IBOX(I)%NBOXBOX > 0)THEN
           IBOX(I)%NBLEVELS = -1
           IBOX(I)%LEVEL =  0
         END IF
         IBOX(I)%BOXIAD = 0
       END DO
C-------
      CALL HM_GET_INT_ARRAY_INDEX('ids',IDB,1,IS_AVAILABLE,LSUBMODEL)
      IF(KEY == 'BOX')THEN
        BOXTYPE = 1
      ELSE IF(KEY == 'BOX2')THEN
        BOXTYPE = 2
      END IF
C-------
C get box de box ID'S dans LINE :
C-------
      ISU = 0
      DO I=1,NBBOX
        IF(IDB == IBOX(I)%ID) ISU=I
      END DO
C---
      IF(ISU > 0)THEN
       NBOX   = IBOX(ISU)%NBOXBOX
       !super box enabled:
       IBOX(ISU)%ACTIBOX = 1
      ELSE
       IF(FLAG == 0)THEN
        IF(ISURF0 == 0)THEN
          CALL ANCMSG(MSGID=799, MSGTYPE=MSGERROR, ANMODE=ANINFO,I1=ID, C1=TITR,I2=IDB)
        ELSE IF(ISURF0 == 1)THEN
          CALL ANCMSG(MSGID=800,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=ID,C1=TITR,I2=IDB)
        END IF
       END IF
      END IF
C---
C   simple box dans /LINE :
C---
      BOOL=.FALSE.
      IF(ISU>0)THEN
        IF(IBOX(ISU)%NBLEVELS == 0 .AND. IBOX(ISU)%LEVEL == 1) THEN
          IF(NBOX == 0)THEN
            CALL BOX_SURF_SH(X          ,IBUFBOX,SKEW  ,IADB   ,BOXTYPE,
     .                       IBOX       ,ISU    ,NUMEL ,NIX    ,IX     ,
     .                       NIX1       ,NIX2   ,ISURF0,IELTYP ,FLAG   ,
     .                       TAGSHELLBOX,0      )
            BOOL=.TRUE.
          END IF
        END IF
      ENDIF
C---
C READ LEVELS OF BOXES ==> "SUBLEVEL DONE"
C---
      IF(.NOT.BOOL)THEN
        ICOUNT = 1
        ITER   = 0
        DO WHILE (ICOUNT == 1)
          ITER  = ITER  + 1
          FLAGG = 0
C---      count next level
          CALL BOXBOXS(IBOX       ,SKEW    ,FLAGG  ,ICOUNT ,ITER   ,
     .                 BOXTYPE    ,IBUFBOX ,X      ,IADB   ,IX     ,
     .                 NIX        ,NIX1    ,NIX2   ,NUMEL  ,ISURF0 ,
     .                 IELTYP     ,ID      ,TITR   ,MESS   ,FLAG   ,
     .                 TAGSHELLBOX,0       )
          IF (IADB>SBUFBOX .OR. IADB<0) 
     .      CALL ANCMSG(MSGID=1007, MSGTYPE=MSGERROR,ANMODE=ANSTOP)
C---      fill next level
          FLAGG = 1
          CALL BOXBOXS(IBOX       ,SKEW    ,FLAGG  ,ICOUNT ,ITER   ,
     .                 BOXTYPE    ,IBUFBOX ,X      ,IADB   ,IX     ,
     .                 NIX        ,NIX1    ,NIX2   ,NUMEL  ,ISURF0 ,
     .                 IELTYP     ,ID      ,TITR   ,MESS   ,FLAG   ,
     .                 TAGSHELLBOX,0       )
C---
        ENDDO
      ENDIF
C---
C tag lines (ou surfaces) in main-box:
C---
C---count lines within BOX
      IF(ISU > 0)THEN
C
        IF(FLAG == 0)THEN
          BOXSEG = IBOX(ISU)%NENTITY  ! segments of main box
          NSEG = NSEG + BOXSEG
        ELSE IF(FLAG == 1)THEN
          BOXSEG = IBOX(ISU)%NENTITY  ! segments of main box
          IADISU = IBOX(ISU)%BOXIAD  ! addresses of segments in main box
          NSEG = NSEG + BOXSEG
          DO I=1,BOXSEG
           NN = NN + 1
           IF(ISURF0 == 1)THEN ! surfaces
             DO K=NIX1,NIX2
               J=IBUFBOX(IADISU+K-2)
                ISURFLIN%NODES(NN,K-1) = J
             ENDDO
             IADISU = IADISU + NIX2 - 1
           ELSE ! lines
C--------------------
             J=IBUFBOX(IADISU)
             ISURFLIN%NODES(NN,1) = J
             IADISU = IADISU + 1
C--------------------
            J=IBUFBOX(IADISU)
            ISURFLIN%NODES(NN,2) = J
            IADISU = IADISU + 1
           END IF
C--------------------
           IF(IELTYP == 7)THEN
               J=IBUFBOX(IADISU)
               ISURFLIN%NODES(NN,4) = 
     .                         ISURFLIN%NODES(NN,3)
               IADISU = IADISU + 1
           END IF
C--------------------
               J=IBUFBOX(IADISU)
               ISURFLIN%ELTYP(NN)= J
               IADISU = IADISU + 1
C--------------------
               J=IBUFBOX(IADISU)
               ISURFLIN%ELEM(NN) = J
               IADISU = IADISU + 1
C--------------------
          END DO
        END IF ! IF(FLAG == 0)
      END IF ! IF(ISU > 0)
C-----------
      RETURN
      END
