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
Chd|  BOXBOX                        source/model/box/boxbox.F     
Chd|-- called by -----------
Chd|        BIGBOX                        source/model/box/bigbox.F     
Chd|        HM_BIGBOX                     source/model/box/hm_bigbox.F  
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        BOXASSEM1                     source/model/box/bigbox.F     
Chd|        BOXTAGN                       source/model/box/bigbox.F     
Chd|        FREERR                        source/starter/freform.F      
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|====================================================================
      SUBROUTINE BOXBOX(IBOX  ,SKEW   ,
     .                  FLAGG  ,ICOUNT,ITER  ,IBUFBOX,
     .                  X      ,IADB  ,ID    ,TITR   ,
     .                  KEY    ,FLAG  ,IBOXMAX)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE OPTIONDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBUFBOX(*),
     .        IAD,FLAGG,ICOUNT,ITER,IADB,FLAG,IBOXMAX
      my_real
     .      X(3,*),SKEW(LSKEW,*)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
      CHARACTER*ncharfield,
     .   KEY
      TYPE (BOX_)  , DIMENSION(NBBOX)  :: IBOX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,K,L,J,JJ,IB,IBS,IDBS,NBOX,
     .         SKIPFLAG,IADX,CNT
C-----------------------------------------------
      IF (FLAGG == 0) ICOUNT=0
C---
C     boucle sur les boxes
C---
      DO IB=1,NBBOX
        SKIPFLAG = 0
        CNT = 0
        NBOX = IBOX(IB)%NBOXBOX
        IF(FLAGG == 0 .AND. IBOX(IB)%NBLEVELS == -1)THEN
C---
         IF(SKIPFLAG == 0)THEN
           DO K=1,NBOX
             J    = IBOX(IB)%IBOXBOX(K)
             IBS  = ABS(J)
             IDBS = IBOX(IBS)%ID
C            activate sublevel boxes of main box:
             IF(IDBS == 0)THEN
               CALL ANCMSG(MSGID=796,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO,
     .                     I1=ID,
     .                     C1=TITR,
     .                     I2=IDBS)
             ELSEIF(IBOX(IBS)%LEVEL == 0)THEN
C            reference a un box non initialise
               IF(ITER > NBBOX)GOTO 900
               IBOX(IB)%NBLEVELS=-1
               IBOX(IB)%LEVEL=0
               ICOUNT=1
               SKIPFLAG = 1
               GOTO 10
             ELSE
C            reference a un box initialise
               CNT = CNT + 1
             ENDIF
           END DO
C---
 10        CONTINUE
C---
         END IF
C---
        IF(SKIPFLAG == 0)THEN
          IBOX(IB)%NBLEVELS= CNT
        ENDIF
C---
        ELSEIF(FLAGG == 1 .AND. IBOX(IB)%LEVEL == 0 .AND.
     .                          IBOX(IB)%NBLEVELS > -1)THEN
C---
           DO K=1,NBOX
             J = IBOX(IB)%IBOXBOX(K)
             IBS  = ABS(J)
             IDBS = IBOX(IBS)%ID
             IF(IBOX(IBS)%NBLEVELS == -1)THEN
               GOTO 20
             ELSE
C
C   fill each sub-box only one time:
C
              IF(IBOX(IBS)%ACTIBOX == 0)THEN
                CALL BOXTAGN(X   ,IBUFBOX,SKEW,IADB,
     .                       IBOX,IBS ,FLAG,IBOXMAX)
                 IBOX(IBS)%ACTIBOX = 1
              END IF
C---
             ENDIF
           END DO
C---
           IBOX(IB)%LEVEL=1
           CALL BOXASSEM1(IBOX,IBUFBOX,IB,IADB,FLAG)
C-----
 20        CONTINUE
C---
        END IF
      END DO
C---
C-----------
      RETURN
 900  CONTINUE
      CALL ANCMSG(MSGID=797,
     .            MSGTYPE=MSGERROR,
     .            ANMODE=ANINFO,
     .            C1=KEY,
     .            I1=ID,
     .            C2=KEY,
     .            C3=TITR,
     .            I2=IBOX(IB)%ID,C4='BOXBOX')
      RETURN
 999  CALL FREERR(1)
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  BOXBOX2                       source/model/box/boxbox.F     
Chd|-- called by -----------
Chd|        BIGBOX2                       source/model/box/bigbox.F     
Chd|        HM_BIGBOX2                    source/model/box/hm_bigbox2.F 
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        BOXASSEM2                     source/model/box/bigbox.F     
Chd|        BOXTAGE                       source/model/box/bigbox.F     
Chd|        FREERR                        source/starter/freform.F      
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|====================================================================
      SUBROUTINE BOXBOX2(IBOX    ,SKEW  ,
     .                   FLAGG   ,ICOUNT,ITER    ,BOXTYPE,
     .                   X       ,IX    ,FLAG    ,IBOXMAX,
     .                   NIX     ,NIX1  ,IPARTE  ,IPART  ,
     .                   KLEVTREE,ELTREE,KELTREE ,NUMEL  ,
     .                   NADMESH ,ID    ,TITR    ,MES    ,
     .                   IADB,IBUFBOX)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE OPTIONDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER 
     .        FLAGG,ICOUNT,ITER,BOXTYPE,NIX,
     .        IX(NIX,*),NIX1,IPARTE(*),IPART(LIPART1,*),
     .        KLEVTREE,KELTREE,ELTREE(KELTREE,*),NUMEL,
     .        NADMESH,FLAG,IBOXMAX,IADB,IBUFBOX(*)
      my_real
     .      X(3,*),SKEW(LSKEW,*)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
      CHARACTER MES*40
      TYPE (BOX_)  , DIMENSION(NBBOX)  :: IBOX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,K,L,J,JJ,IB,IBS,IDBS,NBOX,
     .         SKIPFLAG,CNT
C-----------------------------------------------
      IF (FLAGG == 0) ICOUNT=0
C---
C     boucle sur les boxes
C---
      DO IB=1,NBBOX
        SKIPFLAG = 0
        CNT = 0
        NBOX = IBOX(IB)%NBOXBOX
        IF(FLAGG == 0 .AND. IBOX(IB)%NBLEVELS == -1)THEN
C---
         IF(SKIPFLAG == 0)THEN
           DO K=1,NBOX
             J    = IBOX(IB)%IBOXBOX(K)
             IBS  = ABS(J)
             IDBS = IBOX(IBS)%ID
C            activate sublevel boxes of main box:
             IF(IDBS == 0)THEN
               CALL ANCMSG(MSGID=796,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO,
     .                     I1=ID,
     .                     C1=TITR,
     .                     I2=IDBS)
             ELSEIF(IBOX(IBS)%LEVEL == 0)THEN
C            reference a un box non initialise
               IF(ITER > NBBOX)GOTO 900
               IBOX(IB)%NBLEVELS=-1
               IBOX(IB)%LEVEL=0
               ICOUNT=1
               SKIPFLAG = 1
               GOTO 10
             ELSE
C            reference a un box initialise
               CNT = CNT + 1
             ENDIF
           END DO
C---
 10        CONTINUE
C---
         END IF
C---
        IF(SKIPFLAG == 0)THEN
          IBOX(IB)%NBLEVELS= CNT
        ENDIF
C---
        ELSEIF(FLAGG == 1 .AND. IBOX(IB)%LEVEL == 0 .AND.
     .                          IBOX(IB)%NBLEVELS > -1)THEN
C---
           DO K=1,NBOX
             J = IBOX(IB)%IBOXBOX(K)
             IBS = ABS(J)
             IDBS = IBOX(IBS)%ID
             IF(IBOX(IBS)%NBLEVELS == -1)THEN
               GOTO 20
             ELSE
C
C   fill each sub-box only one time:
C
              IF(IBOX(IBS)%ACTIBOX == 0)THEN
                CALL BOXTAGE(X      ,SKEW   ,IBOX   ,
     .                       IBS    ,BOXTYPE,IX     ,NIX   ,
     .                       NIX1   ,IPARTE ,IPART  ,KLEVTREE,ELTREE,
     .                       KELTREE,NUMEL  ,NADMESH,FLAG    ,IBOXMAX,
     .                       IADB,IBUFBOX)
                 IBOX(IBS)%ACTIBOX = 1
              END IF
C---
             ENDIF
           END DO
C---
           IBOX(IB)%LEVEL=1
           CALL BOXASSEM2(IBOX,IBUFBOX,IB,IADB,NUMEL,FLAG,IBOXMAX)
C-----
 20        CONTINUE
C---
        END IF
      END DO
C-----------
      RETURN
 900  CONTINUE
      CALL ANCMSG(MSGID=797,
     .            MSGTYPE=MSGERROR,
     .            ANMODE=ANINFO,
     .            C1=MES,
     .            I1=ID,
     .            C2=MES,
     .            C3=TITR,
     .            I2=IBOX(IB)%ID,C4='BOXBOX2')
      RETURN
 999  CALL FREERR(1)
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  BOXBOXS                       source/model/box/boxbox.F     
Chd|-- called by -----------
Chd|        BIGSBOX                       source/model/box/bigbox.F     
Chd|        HM_BIGSBOX                    source/groups/hm_bigsbox.F    
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        BOXASSEM3                     source/model/box/bigbox.F     
Chd|        BOX_SURF_SH                   source/model/box/bigbox.F     
Chd|        FREERR                        source/starter/freform.F      
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|====================================================================
      SUBROUTINE BOXBOXS(IBOX       ,SKEW    ,FLAGG  ,ICOUNT  ,ITER   ,
     .                   BOXTYPE    ,IBUFBOX ,X      ,IADB    ,IX     ,
     .                   NIX        ,NIX1    ,NIX2   ,NUMEL   ,ISURF0 ,
     .                   IELTYP     ,ID      ,TITR   ,MESS    ,FLAG   ,
     .                   TAGSHELLBOX,IEXT    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE OPTIONDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBUFBOX(*),
     .        IAD,FLAGG,ICOUNT,ITER,BOXTYPE,IADB,NIX,
     .        IX(NIX,*),NIX1,NIX2,ISURF0,IELTYP,NUMEL,FLAG,
     .        TAGSHELLBOX(*),IEXT
      my_real
     .        X(3,*),SKEW(LSKEW,*)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
      CHARACTER MESS*40
      TYPE (BOX_)  , DIMENSION(NBBOX)  :: IBOX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,L,J,JJ,IB,IBS,IDBS,NBOX,SKIPFLAG,
     .        IADX,CNT
C-----------------------------------------------
      IF (FLAGG == 0) ICOUNT=0
C---
C     boucle sur les boxes
C---
      DO IB=1,NBBOX
        SKIPFLAG = 0
        CNT = 0
        NBOX = IBOX(IB)%NBOXBOX
        IF(FLAGG == 0 .AND. IBOX(IB)%NBLEVELS == -1)THEN
C---
         IF(SKIPFLAG == 0)THEN
           DO K=1,NBOX
             J    = IBOX(IB)%IBOXBOX(K)
             IBS  = ABS(J)
             IDBS = IBOX(IBS)%ID
C activate sublevel boxes of main box:
             IF (IDBS == 0)THEN
               CALL ANCMSG(MSGID=796,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO,
     .                     I1=ID,
     .                     C1=TITR,
     .                     I2=IDBS)
             ELSEIF(IBOX(IBS)%LEVEL == 0)THEN
C            reference a un box non initialise
               IF (ITER > NBBOX) GOTO 900
               IBOX(IB)%NBLEVELS=-1
               IBOX(IB)%LEVEL=0
               ICOUNT=1
               SKIPFLAG = 1
               GOTO 10
             ELSE
C            reference a un box initialise
               CNT = CNT + 1
             ENDIF
           END DO
C---
 10        CONTINUE
C---
         END IF
C---
        IF(SKIPFLAG == 0)THEN
          IBOX(IB)%NBLEVELS= CNT
        ENDIF
C---
        ELSEIF(FLAGG == 1 .AND. IBOX(IB)%LEVEL == 0 .AND.
     .                          IBOX(IB)%NBLEVELS > -1)THEN
C---
           DO K=1,NBOX
             J = IBOX(IB)%IBOXBOX(K)
             IBS  = ABS(J)
             IDBS = IBOX(IBS)%ID
             IF(IBOX(IBS)%NBLEVELS == -1)THEN
               GOTO 20
             ELSE
C   fill each sub-box only once time:
              IF(IBOX(IBS)%ACTIBOX == 0)THEN
                CALL BOX_SURF_SH(X          ,IBUFBOX,SKEW   ,IADB   ,BOXTYPE,
     .                           IBOX       ,IBS    ,NUMEL  ,NIX    ,IX     ,
     .                           NIX1       ,NIX2   ,ISURF0,IELTYP  ,FLAG   ,
     .                           TAGSHELLBOX,IEXT   )
                 IBOX(IBS)%ACTIBOX = 1
              END IF
C---
             ENDIF
           END DO
C---
           IBOX(IB)%LEVEL=1
           CALL BOXASSEM3(IBOX,IBUFBOX,IB  ,IADB  ,NUMEL ,
     .                    NIX    ,IX  ,NIX1   ,NIX2,ISURF0,IELTYP,
     .                    FLAG   ,IEXT)
C-----
 20        CONTINUE
C---
        END IF
      END DO
C---
C-----------
      RETURN
 900  CONTINUE
      CALL ANCMSG(MSGID=797,
     .            MSGTYPE=MSGERROR,
     .            ANMODE=ANINFO,
     .            C1=MESS,
     .            I1=ID,
     .            C2=MESS,
     .            C3=TITR,
     .            I2=IBOX(IB)%ID,C4='BOXBOX3')
      RETURN
 999  CALL FREERR(1)
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  ELSTAGBOXBOX                  source/model/box/boxbox.F     
Chd|-- called by -----------
Chd|        SBOXBOXSURF                   source/model/box/bigbox.F     
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        BOXASSEM4                     source/model/box/bigbox.F     
Chd|        ELSTAGBOX                     source/model/box/bigbox.F     
Chd|        FACEBOX                       source/model/box/bigbox.F     
Chd|        FREERR                        source/starter/freform.F      
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|====================================================================
      SUBROUTINE ELSTAGBOXBOX(IBOX   ,SKEW   ,
     .                        FLAGG  ,ICOUNT ,ITER    ,BOXTYPE,
     .                        IBUFBOX,X      ,IADB    ,IXS    ,
     .                        KNOD2ELS,NOD2ELS,
     .                        IEXT    ,FLAG    ,IXS10  ,
     .                        IXS16   ,IXS20 ,ELSTAG  ,ID,TITR,
     .                        KNOD2ELC,NOD2ELC,IXC    ,TAGSHELLBOXC,
     .                        KNOD2ELTG,NOD2ELTG,IXTG  ,TAGSHELLBOXG,IEXT_SET)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE OPTIONDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBUFBOX(*),
     .        FLAGG,ICOUNT,ITER,BOXTYPE,IADB,
     .        IXS(NIXS,*),KNOD2ELS(*),NOD2ELS(*),IEXT,
     .        FLAG,IXS10(6,*),IXS16(8,*),IXS20(12,*),
     .        ELSTAG(*),KNOD2ELC(*),NOD2ELC(*),IXC(NIXC,*),
     .        TAGSHELLBOXC(*),KNOD2ELTG(*),NOD2ELTG(*),IXTG(NIXTG,*) ,
     .        TAGSHELLBOXG(*),IEXT_SET
      my_real
     .      X(3,*),SKEW(LSKEW,*)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
      TYPE (BOX_)  , DIMENSION(NBBOX)  :: IBOX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,K,L,J,JJ,IB,IBS,IDBS,NBOX,
     .         SKIPFLAG,IADX,CNT
C-----------------------------------------------
      IF(FLAGG == 0)ICOUNT=0
C---
C     boucle sur les boxes
C---
      DO IB=1,NBBOX
        SKIPFLAG = 0
        CNT = 0
        NBOX = IBOX(IB)%NBOXBOX
        IF(FLAGG == 0 .AND. IBOX(IB)%NBLEVELS == -1)THEN
C---
         IF (SKIPFLAG == 0) THEN
           DO K=1,NBOX
             J    = IBOX(IB)%IBOXBOX(K)
             IBS  = ABS(J)
             IDBS = IBOX(IBS)%ID
             IF(IDBS == 0)THEN
               CALL ANCMSG(MSGID=796,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO,
     .                     I1=ID,
     .                     C1=TITR,
     .                     I2=IDBS)
             ELSEIF(IBOX(IBS)%LEVEL == 0)THEN
C            reference a un box non initialise
               IF(ITER > NBBOX)GOTO 900
               IBOX(IB)%NBLEVELS=-1
               IBOX(IB)%LEVEL=0
               ICOUNT=1
               SKIPFLAG = 1
               GOTO 10
             ELSE
C            reference a un box initialise
               CNT = CNT + 1
             ENDIF
           END DO
C---
 10        CONTINUE
C---
         END IF
C---
        IF(SKIPFLAG == 0)THEN
          IBOX(IB)%NBLEVELS= CNT
        ENDIF
C---
        ELSEIF(FLAGG == 1 .AND. IBOX(IB)%LEVEL == 0 .AND.
     .                          IBOX(IB)%NBLEVELS > -1)THEN
C---
           DO K=1,NBOX
             J = IBOX(IB)%IBOXBOX(K)
             IBS  = ABS(J)
             IDBS = IBOX(IBS)%ID
             IF(IBOX(IBS)%NBLEVELS == -1)THEN
               GOTO 20
             ELSE
C   fill each sub-box only one time:
              IF(IBOX(IBS)%ACTIBOX == 0)THEN
C---
C    tag faces of solids within box
C---
          CALL ELSTAGBOX(IXS    ,ELSTAG ,X    ,SKEW ,BOXTYPE,
     .                   IBS    ,IBOX   )
C---
C    fill tmp "IBUFBOX" for taged faces
C---
          CALL FACEBOX(IXS          ,X         ,KNOD2ELS ,NOD2ELS ,IEXT        ,
     .                 FLAG         ,IXS10     ,IXS16    ,IXS20   ,SKEW        ,
     .                 IBOX         ,ELSTAG    ,IBUFBOX  ,IADB    ,IBS         ,
     .                 ID           ,TITR      ,KNOD2ELC ,NOD2ELC ,IXC         ,
     .                 TAGSHELLBOXC ,KNOD2ELTG ,NOD2ELTG ,IXTG    ,TAGSHELLBOXG,
     .                 IEXT_SET     )
                 IBOX(IBS)%ACTIBOX = 1
              END IF
C---
             ENDIF
           END DO
C---
           IBOX(IB)%LEVEL=1
           CALL BOXASSEM4(IBOX,IBUFBOX,IB,IADB,FLAG,IEXT_SET)
C-----
 20        CONTINUE
C---
        END IF
      END DO
C---
C-----------
      RETURN
 900  CONTINUE
      CALL ANCMSG(MSGID=797,
     .            MSGTYPE=MSGERROR,
     .            ANMODE=ANINFO,
     .            C1='SURFACE',
     .            I1=ID,
     .            C2='SURFACE',
     .            C3=TITR,
     .            I2=IBOX(IB)%ID,C4='BOXBOX4')
      RETURN
 999  CALL FREERR(1)
C-----------
      RETURN
      END
