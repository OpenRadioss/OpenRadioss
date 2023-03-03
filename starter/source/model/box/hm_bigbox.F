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
Chd|  HM_BIGBOX                     source/model/box/hm_bigbox.F  
Chd|-- called by -----------
Chd|        HM_LECGRN                     source/groups/hm_lecgrn.F     
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        BOXBOX                        source/model/box/boxbox.F     
Chd|        BOXTAGN                       source/model/box/bigbox.F     
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|====================================================================
      SUBROUTINE HM_BIGBOX(X   ,FLAG,NNOD ,
     .                  SKEW,IGS    ,ISKN ,ITABM1,IBOX   ,
     .                  ID  ,IBUFBOX,IADB ,TITR,KEY,NN,
     .                  IBOXMAX,IGRNOD,IDB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
      USE OPTIONDEF_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER FLAG,NNOD,
     .        IGS,ISKN(LISKN,*),ITABM1(*),
     .        ID,IBUFBOX(*),IADB,NN,IBOXMAX
      INTEGER,INTENT(IN) :: IDB
      my_real
     .        X(3,*),SKEW(LSKEW,*)
      CHARACTER*nchartitle,
     .   TITR
      CHARACTER*ncharfield,
     .   KEY
      TYPE (GROUP_)  , DIMENSION(NGRNOD) :: IGRNOD
      TYPE (BOX_)    , DIMENSION(NBBOX)  :: IBOX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,ISU,JREC,NBOX,BOXTYPE,IADBOX,
     .        ICOUNT,ITER,FLAGG,INBOX,BOXNODS,IADISU
      CHARACTER BOX*3
      LOGICAL BOOL
C-----------------------------------------------
       DO I=1,NBBOX
         IBOX(I)%NBLEVELS = 0
         IBOX(I)%LEVEL = 1
         IBOX(I)%ACTIBOX = 0
         IF(IBOX(I)%NBOXBOX > 0)THEN
           IBOX(I)%NBLEVELS = -1
           IBOX(I)%LEVEL =  0
         END IF
C
         IBOX(I)%BOXIAD = 0
       END DO
C-------
C get box de box ID'S dans grnod:
C-------
      ISU = 0
      DO I=1,NBBOX
        IF(IDB == IBOX(I)%ID) THEN 
          ISU=I
          EXIT
        ENDIF
      END DO
C---
      IF(ISU > 0)THEN
       NBOX   = IBOX(ISU)%NBOXBOX
C super box activated:
       IBOX(ISU)%ACTIBOX = 1
      ELSE
       IF(FLAG == 0)THEN
        CALL ANCMSG(MSGID=794,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=ID,
     .              C1=TITR,
     .              I2=IDB)
       END IF
      END IF
C---
C   simple box dans grnod:
C---
      BOOL = .FALSE.
      IF(ISU>0)THEN
        IF(IBOX(ISU)%NBLEVELS == 0 .AND. IBOX(ISU)%LEVEL == 1) THEN
          IF(NBOX == 0)THEN
            CALL BOXTAGN(X   ,IBUFBOX,SKEW,IADB,IBOX,ISU ,FLAG,IBOXMAX)
            BOOL =.TRUE.
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
          CALL BOXBOX(IBOX  ,SKEW   ,
     .                FLAGG  ,ICOUNT,ITER  ,IBUFBOX,
     .                X      ,IADB  ,ID    ,TITR   ,
     .                KEY    ,FLAG  ,IBOXMAX)
C---      fill next level
          FLAGG = 1
          CALL BOXBOX(IBOX  ,SKEW   ,
     .                FLAGG  ,ICOUNT,ITER  ,IBUFBOX,
     .                X      ,IADB  ,ID    ,TITR   ,
     .                KEY    ,FLAG  ,IBOXMAX)
C---
        ENDDO
      ENDIF
C---
C tag group nodes in main-box:
C---
      IF(ISU > 0)THEN
        IF(FLAG == 0)THEN
          BOXNODS = IBOX(ISU)%NENTITY  ! nodes of main box
          NNOD = BOXNODS
        ELSE IF(FLAG == 1)THEN
          BOXNODS = IBOX(ISU)%NENTITY  ! nodes of main box
          IADISU  = IBOX(ISU)%BOXIAD  ! addresses of nodes in main box
          NNOD = BOXNODS
          DO I=1,BOXNODS
            N=IBUFBOX(IADISU+I-1)
            NN = NN + 1
            IGRNOD(IGS)%ENTITY(NN) = N
          END DO
        END IF
      END IF
C--------------
      RETURN
      END
