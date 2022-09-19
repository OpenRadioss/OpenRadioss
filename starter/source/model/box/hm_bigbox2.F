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
Chd|  HM_BIGBOX2                    source/model/box/hm_bigbox2.F 
Chd|-- called by -----------
Chd|        HM_LECGRE                     source/groups/hm_lecgre.F     
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        BOXBOX2                       source/model/box/boxbox.F     
Chd|        BOXTAGE                       source/model/box/bigbox.F     
Chd|        FREERR                        source/starter/freform.F      
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|====================================================================
      SUBROUTINE HM_BIGBOX2(X      ,FLAG   ,NEL     ,
     .                      SKEW   ,IGS    ,ISKN    ,ITABM1,IBOX   ,
     .                      ID     ,NADMESH,NIX     ,IX    ,NIX1   ,NUMEL ,
     .                      IPARTE ,IPART  ,KLEVTREE,ELTREE,KELTREE,BUFTMP,
     .                      KEY     ,TITR  ,MES     ,IGRELEM,NGRELE ,NN   ,
     .                      IADB   ,IBOXMAX,IBUFBOX ,IDB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE OPTIONDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JREC,FLAG,NEL,IGS,
     .        ISKN(LISKN,*),ITABM1(*),ID,NADMESH,
     .        NIX,IX(NIX,*),NIX1,NUMEL,IPARTE(*),IPART(LIPART1,*),
     .        KLEVTREE,KELTREE,ELTREE(KELTREE,*),
     .        BUFTMP(NUMEL*5),NGRELE,NN,IBOXMAX,IADB,IBUFBOX(*)
      INTEGER,INTENT(IN) :: IDB
      my_real X(3,*),SKEW(LSKEW,*)
      CHARACTER KEY*4,MES*40
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------
      TYPE (GROUP_), DIMENSION(NGRELE) :: IGRELEM
      TYPE (BOX_)  , DIMENSION(NBBOX)  :: IBOX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ISU,ISK,TAGN(NUMEL),BOXTYPE,
     . NEGBOX,TAGNEG(NUMEL),TAGPOS(NUMEL),
     . NBOX,BOXELE,ICOUNT,ITER,FLAGG,IADISU
      my_real XP1,YP1,ZP1,XP2,YP2,ZP2,DIAM,NODINB(3)
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
      IF(KEY(1:4) == 'BOX2')THEN
        BOXTYPE = 2
      ELSE IF(KEY(1:3) == 'BOX')THEN
        BOXTYPE = 1
      END IF
C-------
C get box de box ID'S dans grshel:
C-------
      ISU = 0
      DO I=1,NBBOX
        IF(IDB == IBOX(I)%ID)THEN
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
        CALL ANCMSG(MSGID=798,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=ID,
     .              C1=TITR,
     .              I2=IDB)
       END IF
      END IF
C---
C   simple box dans grshel:
C---
      BOOL =.FALSE.
      IF(ISU>0)THEN
        IF(IBOX(ISU)%NBLEVELS == 0 .AND. IBOX(ISU)%LEVEL == 1) THEN
          IF (NBOX == 0) THEN ! simple box (no sub box)
            CALL BOXTAGE(X      ,SKEW   ,IBOX   ,
     .                   ISU    ,BOXTYPE,IX     ,NIX   ,
     .                   NIX1   ,IPARTE ,IPART  ,KLEVTREE,ELTREE,
     .                   KELTREE,NUMEL  ,NADMESH,FLAG  ,IBOXMAX,
     .                   IADB   ,IBUFBOX)
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
          CALL BOXBOX2(IBOX    ,SKEW  ,
     .                 FLAGG   ,ICOUNT,ITER    ,BOXTYPE,
     .                 X       ,IX    ,FLAG    ,IBOXMAX,
     .                 NIX     ,NIX1  ,IPARTE  ,IPART  ,
     .                 KLEVTREE,ELTREE,KELTREE ,NUMEL  ,
     .                 NADMESH ,ID     ,TITR   ,MES    ,
     .                 IADB    ,IBUFBOX)
C---      fill next level
          FLAGG = 1
          CALL BOXBOX2(IBOX    ,SKEW   ,
     .                 FLAGG   ,ICOUNT ,ITER    ,BOXTYPE,
     .                 X       ,IX     ,FLAG    ,IBOXMAX,
     .                 NIX     ,NIX1   ,IPARTE  ,IPART  ,
     .                 KLEVTREE,ELTREE ,KELTREE ,NUMEL  ,
     .                 NADMESH ,ID     ,TITR    ,MES    ,
     .                 IADB    ,IBUFBOX)
C---
        ENDDO
      ENDIF
C---
C tag group elements in main-box:
C---
      IF(ISU > 0)THEN
        IF(FLAG == 0)THEN
          BOXELE = IBOX(ISU)%NENTITY  ! elements of main box
          NEL = BOXELE
        ELSE IF(FLAG == 1)THEN
          BOXELE = IBOX(ISU)%NENTITY  ! elements of main box
          IADISU = IBOX(ISU)%BOXIAD  ! addresses of elements in main box
          NEL = BOXELE
          DO I=1,BOXELE
            J=IBUFBOX(IADISU+I-1)
            NN = NN + 1
            IGRELEM(IGS)%ENTITY(NN) = J
          END DO
        END IF
      END IF
C----------
      RETURN
 999  CALL FREERR(1)
      RETURN
      END
