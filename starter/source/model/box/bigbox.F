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
Chd|  BIGBOX                        source/model/box/bigbox.F     
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        BOXBOX                        source/model/box/boxbox.F     
Chd|        BOXTAGN                       source/model/box/bigbox.F     
Chd|        FREERR                        source/starter/freform.F      
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|====================================================================
      SUBROUTINE BIGBOX(X   ,FLAG,NNOD ,
     .                  SKEW,IGS    ,ISKN ,ITABM1,IBOX   ,
     .                  ID  ,IBUFBOX,IADB ,TITR,KEY,NN,
     .                  IBOXMAX,IGRNOD)
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
C   A n a l y s e   M o d u l e
C-----------------------------------------------
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
      INTEGER FLAG,NNOD,
     .        IGS,ISKN(LISKN,*),ITABM1(*),
     .        ID,IBUFBOX(*),IADB,NN,IBOXMAX
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
      INTEGER I,J,K,N,ISU,JREC,IDB,NBOX,BOXTYPE,IADBOX,
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
      JREC=IREC+1
      READ(IIN,REC=JREC,ERR=999,FMT='(A)')LINE
      READ(LINE,ERR=999,FMT=FMT_I) IDB
C-------
C get box de box ID'S dans grnod:
C-------
      ISU = 0
      DO I=1,NBBOX
        IF(IDB == IBOX(I)%ID) ISU=I
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
 999  CALL FREERR(1)
      RETURN
      END
Chd|====================================================================
Chd|  BOXTAGN                       source/model/box/bigbox.F     
Chd|-- called by -----------
Chd|        BIGBOX                        source/model/box/bigbox.F     
Chd|        BOXBOX                        source/model/box/boxbox.F     
Chd|        HM_BIGBOX                     source/model/box/hm_bigbox.F  
Chd|-- calls ---------------
Chd|        CHECKCYL                      source/model/box/rdbox.F      
Chd|        CHECKPARA                     source/model/box/rdbox.F      
Chd|        CHECKSPHERE                   source/model/box/rdbox.F      
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|====================================================================
      SUBROUTINE BOXTAGN(X   ,IBUFBOX,SKEW,IADB,
     .                   IBOX,ISU ,FLAG,IBOXMAX)
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
C   A n a l y s e   M o d u l e
C-----------------------------------------------
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
      INTEGER IBUFBOX(*),IADB,ISU,FLAG,IBOXMAX
      my_real
     .        X(3,*),SKEW(LSKEW,*)
      TYPE (BOX_)  , DIMENSION(NBBOX)  :: IBOX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,OK,ISK,IDBX,BOXNOD,TAGN(NUMNOD),
     .        IADB0,JAD,ITYPE
      my_real
     .    XP1,YP1,ZP1,XP2,YP2,ZP2,DIAM,NODINB(3)
C-----------------------------------------------
      OK  = 0
      BOXNOD = 0
      IADB0 = IADB
      TAGN(1:NUMNOD) = 0
C-------
      IDBX = IBOX(ISU)%ID
      ISK  = IBOX(ISU)%ISKBOX
      ITYPE= IBOX(ISU)%TYPE
      DIAM = IBOX(ISU)%DIAM
      XP1  = IBOX(ISU)%X1
      YP1  = IBOX(ISU)%Y1
      ZP1  = IBOX(ISU)%Z1
      XP2  = IBOX(ISU)%X2
      YP2  = IBOX(ISU)%Y2
      ZP2  = IBOX(ISU)%Z2
C
      IF(IDBX.NE.0)THEN
        DO I=1,NUMNOD
          OK = 0
          NODINB(1) = X(1,I)
          NODINB(2) = X(2,I)
          NODINB(3) = X(3,I)
          IF(ITYPE == 1)THEN       ! 'RECTA'
            CALL CHECKPARA(XP1,YP1,ZP1,XP2,YP2,ZP2,
     .                     ISK,NODINB,SKEW,OK)
          ELSE IF(ITYPE == 2)THEN  ! 'CYLIN'
            CALL CHECKCYL(XP1, YP1, ZP1 , XP2, YP2, ZP2,
     .                    NODINB  , DIAM, OK )
          ELSE IF(ITYPE == 3)THEN  ! 'SPHER'
            CALL CHECKSPHERE(XP1, YP1, ZP1, NODINB, DIAM, OK)
          END IF
C
C tag nodes dans box:
C
          IF(OK == 1)THEN
            IF(TAGN(I) == 0)THEN
              BOXNOD=BOXNOD+1
              TAGN(I) = 1
            END IF
          END IF
        END DO
C
        IBOX(ISU)%NENTITY = BOXNOD
        IBOX(ISU)%BOXIAD = IADB0
      END IF
C---------------
      IF (FLAG == 0) THEN
        DO I=1,NUMNOD
          IF(TAGN(I) == 1)THEN
            IADB = IADB + 1
          END IF
        END DO
      ELSEIF (FLAG == 1) THEN
        DO I=1,NUMNOD
          IF(TAGN(I) == 1)THEN
            IBUFBOX(IADB) = I
            IADB = IADB + 1
          END IF
        END DO
      ENDIF
C---------------
      RETURN
      END
Chd|====================================================================
Chd|  BOXASSEM1                     source/model/box/bigbox.F     
Chd|-- called by -----------
Chd|        BOXBOX                        source/model/box/boxbox.F     
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|====================================================================
      SUBROUTINE BOXASSEM1(IBOX,IBUFBOX,IB,IADB,FLAG)
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
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBUFBOX(*),IB,IADB,FLAG
      TYPE (BOX_)  , DIMENSION(NBBOX)  :: IBOX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,TAGPOS(NUMNOD),TAGNEG(NUMNOD),
     . TAGN(NUMNOD),BOXNOD,IADB0,IADBOX,IDBX,NBOX,
     . JAD,IBS,BOXNODS
C-----------------------------------------------
      TAGPOS(1:NUMNOD) = 0
      TAGNEG(1:NUMNOD) = 0
      TAGN(1:NUMNOD) = 0
C
C assembly of sub-lelevs:
C
C---------------
      BOXNOD = 0
      IADB0 = IADB
      NBOX   = IBOX(IB)%NBOXBOX
      BOXNODS = 0
C
      IF (FLAG == 0) THEN
        DO K=1,NBOX
          J    = IBOX(IB)%IBOXBOX(K)
          IBS  = ABS(J)
          IDBX = IBOX(IBS)%ID
          IF (IDBX /= 0) BOXNODS = BOXNODS + IBOX(IBS)%NENTITY  ! nodes of sub-box
        ENDDO
        IADB = IADB + BOXNODS
C
        IBOX(IB)%NENTITY=BOXNODS
        IBOX(IB)%BOXIAD=IADB0
        IF (IBOX(IB)%ACTIBOX == 0) IBOX(IB)%ACTIBOX = 1
      ELSEIF (FLAG == 1) THEN
C---
C tag nodes of positive boxes:
C---
        DO K=1,NBOX
          J    = IBOX(IB)%IBOXBOX(K)
          IBS  = ABS(J)
          IDBX = IBOX(IBS)%ID
          BOXNODS = IBOX(IBS)%NENTITY  ! nodes of sub-box
          JAD     = IBOX(IBS)%BOXIAD  ! address of nodes
C---
          IF(IDBX.NE.0 .and. J > 0)THEN
            DO I=1,BOXNODS
              N = IBUFBOX(JAD+I-1)
              IF(TAGPOS(N) == 0)THEN
               TAGPOS(N) = 1
              END IF
            END DO
          END IF
        END DO
C---
C tag nodes of negative boxes:
C---
        DO K=1,NBOX
          J    = IBOX(IB)%IBOXBOX(K)
          IBS  = ABS(J)
          IDBX = IBOX(IBS)%ID
          BOXNODS = IBOX(IBS)%NENTITY  ! nodes of sub-box
          JAD     = IBOX(IBS)%BOXIAD  ! address of nodes
C---
          IF(IDBX.NE.0 .and. J < 0)THEN
            DO I=1,BOXNODS
              N = IBUFBOX(JAD+I-1)
              IF(TAGNEG(N) == 0)THEN
               TAGNEG(N) = 1
              END IF
            END DO
          END IF
        END DO
C----------------
C final combination (+/-) assembly in sublevel:
C----------------
        DO I=1,NUMNOD
          IF(TAGPOS(I) > 0 .and. TAGNEG(I) == 0)TAGN(I) = 1
        END DO
C
        DO I=1,NUMNOD
          IF(TAGN(I) == 1)THEN
            BOXNOD=BOXNOD+1
          END IF
        END DO
        IBOX(IB)%NENTITY=BOXNOD
        IBOX(IB)%BOXIAD=IADB0
        IF(IBOX(IB)%ACTIBOX == 0) IBOX(IB)%ACTIBOX = 1
C
C final tag of nodes within box:
C
        DO I=1,NUMNOD
          IF(TAGN(I) == 1)THEN
            IBUFBOX(IADB) = I
            IADB = IADB + 1
          END IF
        END DO
      ENDIF ! IF(FLAG == 0)
C------------
      RETURN
      END
Chd|====================================================================
Chd|  BOXASSEM2                     source/model/box/bigbox.F     
Chd|-- called by -----------
Chd|        BOXBOX2                       source/model/box/boxbox.F     
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|====================================================================
      SUBROUTINE BOXASSEM2(IBOX,IBUFBOX,IB,IADB,NUMEL,FLAG,IBOXMAX)
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
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IB,NUMEL,FLAG,IBOXMAX,IADB,IBUFBOX(*)
      TYPE (BOX_)  , DIMENSION(NBBOX)  :: IBOX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,JJ,K,N,TAGPOS(NUMEL),TAGNEG(NUMEL),
     . TAGN(NUMEL),IADB0,IADBOX,IDBX,NBOX,JAD,IBS,
     . BOXELE0,BOXELE
C-----------------------------------------------
      TAGPOS(1:NUMEL) = 0
      TAGNEG(1:NUMEL) = 0
      TAGN(1:NUMEL) = 0
C
C assembly of sub-lelevs:
C
C---------------
      BOXELE = 0
      IADB0 = IADB
      NBOX   = IBOX(IB)%NBOXBOX
C
      BOXELE0 = 0
      IF (FLAG == 0) THEN
        DO K=1,NBOX
          J    = IBOX(IB)%IBOXBOX(K)
          IBS  = ABS(J)
          IDBX = IBOX(IBS)%ID
          IF (IDBX /= 0) BOXELE0 = BOXELE0 + IBOX(IBS)%NENTITY  ! elements of sub-box
        ENDDO
        IADB = IADB + BOXELE0
C
        IBOX(IB)%NENTITY=BOXELE0
        IBOX(IB)%BOXIAD=IADB0  ! address of elements
        IF (IBOX(IB)%ACTIBOX == 0) IBOX(IB)%ACTIBOX = 1
      ELSEIF (FLAG == 1) THEN
C---
C tag elements of positive boxes:
C---
        DO K=1,NBOX
          J    = IBOX(IB)%IBOXBOX(K)
          IBS  = ABS(J)
          IDBX = IBOX(IBS)%ID
          BOXELE0 = IBOX(IBS)%NENTITY  ! elements of sub-box
          JAD     = IBOX(IBS)%BOXIAD  ! address of nodes
C---
          IF(IDBX.NE.0 .and. J > 0)THEN
            DO I=1,BOXELE0
              JJ = IBUFBOX(JAD+I-1)
              IF(TAGPOS(JJ) == 0)THEN
               TAGPOS(JJ) = 1
              END IF
            END DO
          END IF
        END DO
C---
C tag elements of negative boxes:
C---
        DO K=1,NBOX
          J    = IBOX(IB)%IBOXBOX(K)
          IBS  = ABS(J)
          IDBX = IBOX(IBS)%ID
          BOXELE0 = IBOX(IBS)%NENTITY  ! elements of sub-box
          JAD     = IBOX(IBS)%BOXIAD  ! address of nodes
C---
          IF(IDBX.NE.0 .and. J < 0)THEN
            DO I=1,BOXELE0
              JJ = IBUFBOX(JAD+I-1)
              IF(TAGNEG(JJ) == 0)THEN
               TAGNEG(JJ) = 1
              END IF
            END DO
          END IF
        END DO
C----------------
C final combination (+/-) assembly in sublevel:
C----------------
        DO I=1,NUMEL
          IF(TAGPOS(I) > 0 .and. TAGNEG(I) == 0)TAGN(I) = 1
        END DO
C
        DO I=1,NUMEL
          IF(TAGN(I) == 1)THEN
            BOXELE=BOXELE+1
          END IF
        END DO
        IBOX(IB)%NENTITY=BOXELE
          IBOX(IB)%BOXIAD=IADB0
        IF(IBOX(IB)%ACTIBOX == 0) IBOX(IB)%ACTIBOX = 1
C
C final tag of elements within box:
C
        DO I=1,NUMEL
          IF(TAGN(I) == 1)THEN
            IBUFBOX(IADB) = I
            IADB = IADB + 1
          END IF
        END DO
      ENDIF ! IF (FLAG == 0) THEN
C------------
      RETURN
      END
Chd|====================================================================
Chd|  BOXASSEM3                     source/model/box/bigbox.F     
Chd|-- called by -----------
Chd|        BOXBOXS                       source/model/box/boxbox.F     
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|====================================================================
      SUBROUTINE BOXASSEM3(IBOX,IBUFBOX,IB  ,IADB  ,NUMEL ,
     .                     NIX    ,IX  ,NIX1   ,NIX2,ISURF0,IELTYP,
     .                     FLAG   ,IEXT)
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
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBUFBOX(*),IB,IADB,
     .        NUMEL,NIX,IX(NIX,*),NIX1,NIX2,ISURF0,IELTYP,
     .        FLAG,IEXT
      TYPE (BOX_)  , DIMENSION(NBBOX)  :: IBOX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,JJ,K,N,TAGPOS(NUMEL),TAGNEG(NUMEL),
     . TAGN(NUMEL),IADB0,IADBOX,IDBX,NBOX,
     . JAD,IBS,BOXSEG0,BOXSEG,DIF_NIX,KAD,POS_IEXT
C-----------------------------------------------
      DIF_NIX = 6
      IF (ISURF0 == 0) DIF_NIX = 4
!
      POS_IEXT = 0
      IF (IEXT > 0) THEN
        DIF_NIX = DIF_NIX + 1
        POS_IEXT = 1
      ENDIF
C
      TAGPOS(1:NUMEL) = 0
      TAGNEG(1:NUMEL) = 0
      TAGN(1:NUMEL) = 0
C
C assembly of sub-lelevs:
C
C---------------
      BOXSEG = 0
      IADB0 = IADB
      NBOX   = IBOX(IB)%NBOXBOX
C
      BOXSEG0 = 0
      IF (FLAG == 0) THEN
        DO K=1,NBOX
          J    = IBOX(IB)%IBOXBOX(K)
          IBS  = ABS(J)
          IDBX = IBOX(IBS)%ID
          IF (IDBX /= 0)
     .       BOXSEG0 = BOXSEG0 + IBOX(IBS)%NENTITY  ! seg of sub-box
        ENDDO
C
        IF (ISURF0 == 0) THEN  ! seg line
           IADB = IADB + BOXSEG0*4
           IF(IEXT > 0) IADB = IADB + BOXSEG0
        ELSEIF (ISURF0 == 1) THEN  ! seg surf
           IADB = IADB + BOXSEG0*6
           IF(IEXT > 0) IADB = IADB + BOXSEG0
        ENDIF
C
        IBOX(IB)%NENTITY=BOXSEG0
        IBOX(IB)%BOXIAD=IADB0
        IF (IBOX(IB)%ACTIBOX == 0) IBOX(IB)%ACTIBOX = 1
      ELSEIF (FLAG == 1) THEN
C---
C tag elements (for line defining) of positive boxes:
C---
        DO K=1,NBOX
          J    = IBOX(IB)%IBOXBOX(K)
          IBS  = ABS(J)
          IDBX = IBOX(IBS)%ID
          BOXSEG0 = IBOX(IBS)%NENTITY  ! elements of sub-box
          JAD     = IBOX(IBS)%BOXIAD  ! address of elements
C---
          IF(IDBX.NE.0 .and. J > 0)THEN
            DO I=1,BOXSEG0
!!              JJ = IBUFBOX(JAD+DIF_NIX-1) ! tag element
              KAD = JAD - 1 + DIF_NIX - POS_IEXT
              JJ = IBUFBOX(KAD) ! tag element
!              JJ = IBOX(IBS)%ENTITY(I)
              JAD = JAD + DIF_NIX
              IF(TAGPOS(JJ) == 0)THEN
                TAGPOS(JJ) = 1
              END IF
            END DO
          END IF
        END DO
C---
C tag elements (for line defining) of negative boxes:
C---
        DO K=1,NBOX
          J    = IBOX(IB)%IBOXBOX(K)
          IBS  = ABS(J)
          IDBX = IBOX(IBS)%ID
          BOXSEG0 = IBOX(IBS)%NENTITY  ! elements of sub-box
          JAD     = IBOX(IBS)%BOXIAD  ! address of elements
C---
          IF(IDBX.NE.0 .and. J < 0)THEN
            DO I=1,BOXSEG0
!!              JJ = IBUFBOX(JAD+DIF_NIX-1) ! tag element
              KAD = JAD - 1 + DIF_NIX - POS_IEXT
              JJ = IBUFBOX(KAD) ! tag element
              JAD = JAD + DIF_NIX
              IF(TAGNEG(JJ) == 0)THEN
                TAGNEG(JJ) = 1
              END IF
            END DO
          END IF
        END DO
C----------------
C final combination (+/-) assembly in sublevel:
C----------------
        DO I=1,NUMEL
          IF(TAGPOS(I) > 0 .and. TAGNEG(I) == 0)TAGN(I) = 1
        END DO
C
        DO I=1,NUMEL
          IF(TAGN(I) == 1)THEN
            BOXSEG=BOXSEG+1
          END IF
        END DO
        IBOX(IB)%NENTITY=BOXSEG
        IBOX(IB)%BOXIAD=IADB0
        IF(IBOX(IB)%ACTIBOX == 0) IBOX(IB)%ACTIBOX = 1
C
C final tag of lines within box:
C 
        DO I=1,NUMEL
          IF(TAGN(I) == 1)THEN
            IF(ISURF0 == 1)THEN
              DO K=NIX1,NIX2
                IBUFBOX(IADB) = IX(K,I)
                IADB = IADB + 1
              ENDDO
            ELSE
                IBUFBOX(IADB) = IX(NIX1,I)
                IADB = IADB + 1
                IBUFBOX(IADB) = IX(NIX2,I)
                IADB = IADB + 1
            END IF
            IF(IELTYP == 7)THEN
                IBUFBOX(IADB) = IBUFBOX(IADB-1)
                IADB = IADB + 1
            END IF
            IBUFBOX(IADB)=IELTYP
            IADB = IADB + 1
            IBUFBOX(IADB)=I
            IADB = IADB + 1
!
            IF (IEXT > 0) THEN
              IBUFBOX(IADB)=IEXT
              IADB = IADB + 1
            ENDIF
          END IF
        END DO
      ENDIF ! IF (FLAG == 0) THEN
C------------
      RETURN
      END
Chd|====================================================================
Chd|  BOXASSEM4                     source/model/box/bigbox.F     
Chd|-- called by -----------
Chd|        ELSTAGBOXBOX                  source/model/box/boxbox.F     
Chd|-- calls ---------------
Chd|        IFACE                         source/ale/ale3d/iface.F      
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|====================================================================
      SUBROUTINE BOXASSEM4(IBOX,IBUFBOX,IB,IADB,FLAG,IEXT_SET)
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
C   A n a l y s e   M o d u l e
C-----------------------------------------------
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
      INTEGER IBUFBOX(*),IB,IADB,FLAG,IEXT_SET
      TYPE (BOX_)  , DIMENSION(NBBOX)  :: IBOX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,JJ,K,N,JJ_OLD,IADB0,IADBOX,IDBX,NBOX,IS,IIS,r,
     . JAD,IBS,BOXSEG0,BOXSEG,DIF_NIX,ELFACE(4,16,NUMELS),
     . IFACE(NUMELS),TAGFACES(16,NUMELS),POSFACE(16,NUMELS),
     . NEGFACE(16,NUMELS),NOD(4),NOLD(4),FAC,NFAC,IDEL,JFACE,
     . TAG8(NUMELS),TAG10(NUMELS),NFACE,POS_IEXT,KAD
C-----------------------------------------------
      DIF_NIX = NISX
      POS_IEXT = 0
      IF (IEXT_SET > 0) THEN
        DIF_NIX = DIF_NIX + 1 ! for IEXT_SET
        POS_IEXT = 1
      ENDIF
C
      DO I=1,NUMELS
       IFACE(I) = 0
       DO J=1,16
        ELFACE(1,J,I) = 0
        ELFACE(2,J,I) = 0
        ELFACE(3,J,I) = 0
        ELFACE(4,J,I) = 0
C
        TAGFACES(J,I) = 0
        POSFACE(J,I)  = 0
        NEGFACE(J,I)  = 0
       END DO
      END DO
C
      DO I=1,NUMELS8
       TAG8(I) = 1
      END DO
C
      DO I=1,NUMELS10
       J = I+NUMELS8
       TAG10(J) = 1
      END DO
C-----------------------
C assembly of sub-lelevs:
C-----------------------
      BOXSEG = 0
      IADB0 = IADB
      NBOX   = IBOX(IB)%NBOXBOX
C
      BOXSEG0 = 0
      IF (FLAG == 0) THEN
        DO K=1,NBOX
          J    = IBOX(IB)%IBOXBOX(K)
          IBS  = ABS(J)
          IDBX = IBOX(IBS)%ID
          IF (IDBX /= 0) BOXSEG0 = BOXSEG0  + IBOX(IBS)%NENTITY  ! faces of sub-box
          IADB = IADB + BOXSEG0
C
          DO J=1,NUMELS
            NFACE = 4
            IF(TAG10(J) == 1) NFACE = 16
            DO IS=1,NFACE
              IADB = IADB + 6
              IF (IEXT_SET > 0) IADB = IADB + 1 ! for IEXT_SET
            ENDDO
          ENDDO
        ENDDO ! DO K=1,NBOX
C
        IBOX(IB)%NENTITY=BOXSEG0
        IBOX(IB)%SURFIAD=IADB0
        IF (IBOX(IB)%ACTIBOX == 0) IBOX(IB)%ACTIBOX = 1
      ELSEIF (FLAG == 1) THEN
C---------------
C  solide 8 + 10
C---------------
C---
C tag elements of positive boxes:
C---
        DO K=1,NBOX
          J    = IBOX(IB)%IBOXBOX(K)
          IBS  = ABS(J)
          IDBX = IBOX(IBS)%ID
          BOXSEG0 = IBOX(IBS)%NENTITY  ! faces of sub-box
          JAD     = IBOX(IBS)%SURFIAD  ! address of faces
C---
          IF(IDBX.NE.0 .and. J > 0)THEN
            DO I=1,BOXSEG0
!!              JJ = IBUFBOX(JAD+DIF_NIX-1) ! tag element
              KAD = JAD - 1 + DIF_NIX - POS_IEXT
              JJ = IBUFBOX(KAD) ! tag element
              NOD(1) = IBUFBOX(JAD)
              NOD(2) = IBUFBOX(JAD+1)
              NOD(3) = IBUFBOX(JAD+2)
              NOD(4) = IBUFBOX(JAD+3)
C
              NFACE = 4
              IF(TAG10(JJ) == 1) NFACE = 16
              FAC = 0
              IDEL = 0
              DO r=1,4
               IF(NOD(r) > 0) FAC = FAC + 1
              END DO
C---
              IF(K > 1)THEN
                DO IIS=1,NFACE  ! loop over double faces
                  NFAC = 0
                  NOLD(1) = ELFACE(1,IIS,JJ)
                  NOLD(2) = ELFACE(2,IIS,JJ)
                  NOLD(3) = ELFACE(3,IIS,JJ)
                  NOLD(4) = ELFACE(4,IIS,JJ)
C
                  DO r=1,4
                   IF(NOLD(r) > 0 .and. NOLD(r)==NOD(r))
     .              NFAC = NFAC + 1
                  END DO
C
                  IF(FAC == 4 .and. NFAC == 4)THEN
                    IDEL = 1
                    EXIT
                  END IF
                 END DO
                END IF
C---
                IF(IDEL /= 1)THEN
C---
C fill work box faces
C
                 IFACE(JJ) = IFACE(JJ) + 1
                 IS = IFACE(JJ)
                 POSFACE(IS,JJ) = 1
C
                 IF(ELFACE(1,IS,JJ) == 0)
     .              ELFACE(1,IS,JJ) = IBUFBOX(JAD)
                 IF(ELFACE(2,IS,JJ) == 0)
     .              ELFACE(2,IS,JJ) = IBUFBOX(JAD+1)
                 IF(ELFACE(3,IS,JJ) == 0)
     .              ELFACE(3,IS,JJ) = IBUFBOX(JAD+2)
                 IF(ELFACE(4,IS,JJ) == 0)
     .              ELFACE(4,IS,JJ) = IBUFBOX(JAD+3)
C---
               ENDIF
               JAD = JAD + DIF_NIX
            END DO
          END IF
        END DO
C---
C tag elements of negative boxes:
C---
        DO K=1,NBOX
          J    = IBOX(IB)%IBOXBOX(K)
          IBS  = ABS(J)
          IDBX = IBOX(IBS)%ID
          BOXSEG0 = IBOX(IBS)%NENTITY  ! faces of sub-box
          JAD     = IBOX(IBS)%SURFIAD  ! address of faces
C---
          IF(IDBX.NE.0 .and. J < 0)THEN
            DO I=1,BOXSEG0
!!              JJ = IBUFBOX(JAD+DIF_NIX-1) ! tag element
              KAD = JAD - 1 + DIF_NIX - POS_IEXT
              JJ = IBUFBOX(KAD) ! tag element
              NOD(1) = IBUFBOX(JAD)
              NOD(2) = IBUFBOX(JAD+1)
              NOD(3) = IBUFBOX(JAD+2)
              NOD(4) = IBUFBOX(JAD+3)
C
              NFACE = 4
              IF(TAG10(JJ) == 1) NFACE = 16
              FAC = 0
              IDEL = 1
              JFACE = 0
C
              DO r=1,4
               IF(NOD(r) > 0) FAC = FAC + 1
              END DO
C---
C                IF(K > 1)THEN
                DO IIS=1,NFACE  ! loop over double faces
                  NFAC = 0
                  NOLD(1) = ELFACE(1,IIS,JJ)
                  NOLD(2) = ELFACE(2,IIS,JJ)
                  NOLD(3) = ELFACE(3,IIS,JJ)
                  NOLD(4) = ELFACE(4,IIS,JJ)
C
                  DO r=1,4
                   IF(NOLD(r) > 0 .and. NOLD(r)==NOD(r))
     .              NFAC = NFAC + 1
                  END DO
C
                  IF(FAC == 4 .and. NFAC == 4)THEN
                    IDEL = 0
                    JFACE = IIS
                    EXIT
                  END IF
                 END DO
C                END IF
C---
                IF(IDEL /= 1)THEN
C---
C
C fill work box faces
C
                 IS = JFACE
                 NEGFACE(IS,JJ) = 1   ! negative faces
C---
               ENDIF
               JAD = JAD + DIF_NIX
            END DO
          END IF
        END DO
C----------------
C final combination (+/-) assembly in sublevel:
C----------------
       DO J=1,NUMELS
         NFACE = 4
         IF(TAG10(JJ) == 1) NFACE = 16
         DO IS=1,NFACE
          IF(POSFACE(IS,J)>0 .and. NEGFACE(IS,J)==0)THEN
            TAGFACES(IS,J)=1
            BOXSEG = BOXSEG + 1
          END IF
         END DO
       END DO
C
C---count lines within BOX
C
       DO J=1,NUMELS
         NFACE = 4
         IF(TAG10(JJ) == 1) NFACE = 16
         DO IS=1,NFACE
          IF(TAGFACES(IS,J) == 1)THEN
            IBUFBOX(IADB) = ELFACE(1,IS,J)
            IADB = IADB + 1
            IBUFBOX(IADB) = ELFACE(2,IS,J)
            IADB = IADB + 1
            IBUFBOX(IADB) = ELFACE(3,IS,J)
            IADB = IADB + 1
            IBUFBOX(IADB) = ELFACE(4,IS,J)
            IADB = IADB + 1
C
            IBUFBOX(IADB)=1  !  IELTYP
            IADB = IADB + 1
            IBUFBOX(IADB)=J
            IADB = IADB + 1
            IF (IEXT_SET > 0) THEN
              IBUFBOX(IADB) = IEXT_SET
              IADB = IADB + 1
            ENDIF
          END IF
         END DO
       END DO
C---------------
  40   CONTINUE
C---
       IBOX(IB)%NENTITY=BOXSEG
       IBOX(IB)%SURFIAD=IADB0
       IF(IBOX(IB)%ACTIBOX == 0) IBOX(IB)%ACTIBOX = 1
C---
      ENDIF ! IF (FLAG == 0)
C---------------
      RETURN
      END
Chd|====================================================================
Chd|  BOXTAGE                       source/model/box/bigbox.F     
Chd|-- called by -----------
Chd|        BIGBOX2                       source/model/box/bigbox.F     
Chd|        BOXBOX2                       source/model/box/boxbox.F     
Chd|        HM_BIGBOX2                    source/model/box/hm_bigbox2.F 
Chd|-- calls ---------------
Chd|        CHECKCYL                      source/model/box/rdbox.F      
Chd|        CHECKPARA                     source/model/box/rdbox.F      
Chd|        CHECKSPHERE                   source/model/box/rdbox.F      
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|====================================================================
      SUBROUTINE BOXTAGE(X      ,SKEW   ,IBOX   ,
     .                   ISU    ,BOXTYPE,IX     ,NIX   ,
     .                   NIX1   ,IPARTE ,IPART  ,KLEVTREE,ELTREE,
     .                   KELTREE,NUMEL  ,NADMESH,FLAG   ,IBOXMAX,
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
C   A n a l y s e   M o d u l e
C-----------------------------------------------
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
      INTEGER ISU,BOXTYPE,
     .        NIX,IX(NIX,*),NIX1,IPARTE(*),IPART(LIPART1,*),
     .        KLEVTREE,KELTREE,ELTREE(KELTREE,*),NUMEL,
     .        NADMESH,FLAG,IBOXMAX,IADB,IBUFBOX(*)
      my_real
     .        X(3,*),SKEW(LSKEW,*)
      TYPE (BOX_)  , DIMENSION(NBBOX)  :: IBOX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,JJ,K,OK,OK1,ISK,IDBX,NELBOX,TAGELEM(NUMEL),
     .    JAD,IP,NLEV,MY_LEV,ITYPE,IE,IADB0
C
      my_real
     .    XP1,YP1,ZP1,XP2,YP2,ZP2,DIAM,NODINB(3)
C-----------------------------------------------
!            IE = 0
            OK  = 0
            OK1 = 0
            NELBOX = 0
            IADB0 = IADB
            TAGELEM(1:NUMEL) = 0
C-------
            IDBX = IBOX(ISU)%ID
            ISK  = IBOX(ISU)%ISKBOX
            ITYPE= IBOX(ISU)%TYPE
            DIAM = IBOX(ISU)%DIAM
            XP1  = IBOX(ISU)%X1
            YP1  = IBOX(ISU)%Y1
            ZP1  = IBOX(ISU)%Z1
            XP2  = IBOX(ISU)%X2
            YP2  = IBOX(ISU)%Y2
            ZP2  = IBOX(ISU)%Z2
C--------------------------------
            IF(IDBX.NE.0)THEN
C---
              IF(NADMESH==0)THEN
                IF (BOXTYPE == 2) THEN
                  DO JJ=1,NUMEL
                    OK=0
                    DO K=2,NIX1+1
                     I=IX(K,JJ)
                     NODINB(1) = X(1,I)
                     NODINB(2) = X(2,I)
                     NODINB(3) = X(3,I)
                     IF(ITYPE == 1)THEN       ! 'RECTA'
                      CALL CHECKPARA(XP1,YP1,ZP1,XP2,YP2,ZP2,
     .                               ISK,NODINB,SKEW,OK)
                     ELSE IF(ITYPE == 2)THEN  ! 'CYLIN'
                      CALL CHECKCYL(XP1, YP1, ZP1 , XP2, YP2, ZP2,
     .                              NODINB  , DIAM, OK )
                     ELSE IF(ITYPE == 3)THEN  ! 'SPHER'
                      CALL CHECKSPHERE(XP1, YP1, ZP1, NODINB, DIAM, OK)
                     END IF
                    ENDDO
                    IF (OK == 1) THEN
                     IF(TAGELEM(JJ) == 0)THEN
                      NELBOX=NELBOX+1
                      TAGELEM(JJ) = 1
                     END IF
                    ENDIF
                  ENDDO
                ELSE IF(BOXTYPE == 1)THEN
                  DO JJ=1,NUMEL
                    OK1=0
                    DO K=2,NIX1+1
                     OK=0
                     I=IX(K,JJ)
                     NODINB(1) = X(1,I)
                     NODINB(2) = X(2,I)
                     NODINB(3) = X(3,I)
                     IF(ITYPE == 1)THEN       ! 'RECTA'
                      CALL CHECKPARA(XP1,YP1,ZP1,XP2,YP2,ZP2,
     .                               ISK,NODINB,SKEW,OK)
                     ELSE IF(ITYPE == 2)THEN  ! 'CYLIN'
                      CALL CHECKCYL(XP1, YP1, ZP1 , XP2, YP2, ZP2,
     .                              NODINB  , DIAM, OK )
                     ELSE IF(ITYPE == 3)THEN  ! 'SPHER'
                      CALL CHECKSPHERE(XP1, YP1, ZP1, NODINB, DIAM, OK)
                     END IF
                     IF(OK == 1) OK1 = OK1 + 1
                    ENDDO
                    IF (OK1 == NIX1) THEN
                     IF(TAGELEM(JJ) == 0)THEN
                      NELBOX=NELBOX+1
                      TAGELEM(JJ) = 1
                     END IF
                    ENDIF
                  ENDDO
                ENDIF
              ELSE   ! NADMESH /=0
                IF (BOXTYPE == 2) THEN
                  DO JJ=1,NUMEL
                    OK=0
                    DO K=2,NIX1+1
                     I=IX(K,JJ)
                     NODINB(1) = X(1,I)
                     NODINB(2) = X(2,I)
                     NODINB(3) = X(3,I)
                     IF(ITYPE == 1)THEN       ! 'RECTA'
                      CALL CHECKPARA(XP1,YP1,ZP1,XP2,YP2,ZP2,
     .                               ISK,NODINB,SKEW,OK)
                     ELSE IF(ITYPE == 2)THEN  ! 'CYLIN'
                      CALL CHECKCYL(XP1, YP1, ZP1 , XP2, YP2, ZP2,
     .                              NODINB  , DIAM, OK )
                     ELSE IF(ITYPE == 3)THEN  ! 'SPHER'
                      CALL CHECKSPHERE(XP1, YP1, ZP1, NODINB, DIAM, OK)
                     END IF
                    ENDDO
                    IF (OK == 1) THEN
                      IP=IPARTE(JJ)
                      NLEV  =IPART(10,IP)
                      MY_LEV=ELTREE(KLEVTREE,JJ)
                      IF(MY_LEV < 0) MY_LEV=-(MY_LEV+1)
                      IF(MY_LEV==NLEV)THEN
                       IF(TAGELEM(JJ) == 0)THEN
                        NELBOX=NELBOX+1
                        TAGELEM(JJ) = 1
                       END IF
                      ENDIF
                    ENDIF
                  ENDDO
                ELSE IF(BOXTYPE == 1)THEN
                  DO JJ=1,NUMEL
                    OK1=0
                    DO K=2,NIX1+1
                     OK=0
                     I=IX(K,JJ)
                     NODINB(1) = X(1,I)
                     NODINB(2) = X(2,I)
                     NODINB(3) = X(3,I)
                     IF(ITYPE == 1)THEN       ! 'RECTA'
                      CALL CHECKPARA(XP1,YP1,ZP1,XP2,YP2,ZP2,
     .                               ISK,NODINB,SKEW,OK)
                     ELSE IF(ITYPE == 2)THEN  ! 'CYLIN'
                      CALL CHECKCYL(XP1, YP1, ZP1 , XP2, YP2, ZP2,
     .                              NODINB  , DIAM, OK )
                     ELSE IF(ITYPE == 3)THEN  ! 'SPHER'
                      CALL CHECKSPHERE(XP1, YP1, ZP1, NODINB, DIAM, OK)
                     END IF
                     IF(OK == 1) OK1 = OK1 + 1
                    ENDDO
                    IF (OK1 == NIX1) THEN
                      IP=IPARTE(JJ)
                      NLEV  =IPART(10,IP)
                      MY_LEV=ELTREE(KLEVTREE,JJ)
                      IF(MY_LEV < 0) MY_LEV=-(MY_LEV+1)
                      IF(MY_LEV==NLEV)THEN
                       IF(TAGELEM(JJ) == 0)THEN
                        NELBOX=NELBOX+1
                        TAGELEM(JJ) = 1
                       END IF
                      ENDIF
                    ENDIF
                  ENDDO
                ENDIF
              END IF
              IBOX(ISU)%NENTITY = NELBOX
              IBOX(ISU)%BOXIAD=IADB0
            END IF
C--------------------------------
      IF (FLAG == 0) THEN
        DO I=1,NUMEL
          IF(TAGELEM(I) == 1)THEN
            IADB = IADB + 1
          END IF
        END DO
      ELSEIF (FLAG == 1 .AND. NELBOX > 0) THEN
        DO I=1,NUMEL
          IF(TAGELEM(I) == 1)THEN
            IBUFBOX(IADB) = I
            IADB = IADB + 1
          END IF
        END DO
      ENDIF
C----------------
      RETURN
      END
Chd|====================================================================
Chd|  BIGBOX2                       source/model/box/bigbox.F     
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        BOXBOX2                       source/model/box/boxbox.F     
Chd|        BOXTAGE                       source/model/box/bigbox.F     
Chd|        FREERR                        source/starter/freform.F      
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|====================================================================
      SUBROUTINE BIGBOX2(X      ,FLAG   ,NEL     ,
     .                   SKEW   ,IGS    ,ISKN    ,ITABM1,IBOX   ,
     .                   ID     ,NADMESH,NIX     ,IX    ,NIX1   ,NUMEL ,
     .                   IPARTE ,IPART  ,KLEVTREE,ELTREE,KELTREE,BUFTMP,
     .                   KEY     ,TITR  ,MES     ,IGRELEM,NGRELE ,NN   ,
     .                   IADB   ,IBOXMAX,IBUFBOX)
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
C   A n a l y s e   M o d u l e
C-----------------------------------------------
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
      my_real
     .        X(3,*),SKEW(LSKEW,*)
      CHARACTER KEY*4,MES*40
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------
      TYPE (GROUP_), DIMENSION(NGRELE) :: IGRELEM
      TYPE (BOX_)  , DIMENSION(NBBOX)  :: IBOX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ISU,IDB,ISK,TAGN(NUMEL),BOXTYPE,
     . NEGBOX,TAGNEG(NUMEL),TAGPOS(NUMEL),
     . NBOX,BOXELE,ICOUNT,ITER,FLAGG,IADISU
      my_real
     .    XP1,YP1,ZP1,XP2,YP2,ZP2,DIAM,NODINB(3)
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
      JREC=IREC+1
      READ(IIN,REC=JREC,ERR=999,FMT='(A)')LINE
      READ(LINE,ERR=999,FMT=FMT_I) IDB
      IF(KEY == 'BOX')THEN
        BOXTYPE = 1
      ELSE IF(KEY == 'BOX2')THEN
        BOXTYPE = 2
      END IF
C-------
C get box de box ID'S dans grshel:
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
      BOOL = .FALSE.
      IF(ISU>0)THEN
        IF(IBOX(ISU)%NBLEVELS == 0 .AND. IBOX(ISU)%LEVEL == 1) THEN
          IF (NBOX == 0) THEN ! simple box (no sub box)
            CALL BOXTAGE(X      ,SKEW   ,IBOX   ,
     .                   ISU    ,BOXTYPE,IX     ,NIX   ,
     .                   NIX1   ,IPARTE ,IPART  ,KLEVTREE,ELTREE,
     .                   KELTREE,NUMEL  ,NADMESH,FLAG  ,IBOXMAX,
     .                   IADB   ,IBUFBOX)
            BOOL = .TRUE.
          END IF
        END IF
      ENDIF
C---
C READ LEVELS OF BOXES ==> "SUBLEVEL DONE"
C---
       IF(.NOT. BOOL)THEN
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
Chd|====================================================================
Chd|  BOX_SURF_SH                   source/model/box/bigbox.F     
Chd|-- called by -----------
Chd|        BIGSBOX                       source/model/box/bigbox.F     
Chd|        BOXBOXS                       source/model/box/boxbox.F     
Chd|        HM_BIGSBOX                    source/groups/hm_bigsbox.F    
Chd|-- calls ---------------
Chd|        CHECKCYL                      source/model/box/rdbox.F      
Chd|        CHECKPARA                     source/model/box/rdbox.F      
Chd|        CHECKSPHERE                   source/model/box/rdbox.F      
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|====================================================================
      SUBROUTINE BOX_SURF_SH(X            ,IBUFBOX,SKEW  ,IADB   ,BOXTYPE,
     .                       IBOX         ,ISU    ,NUMEL ,NIX    ,IX     ,
     .                       NIX1         ,NIX2   ,ISURF0,IELTYP ,FLAG   ,
     .                       TAGSHELLBOX  ,IEXT   )
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
C   A n a l y s e   M o d u l e
C-----------------------------------------------
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
      INTEGER IBUFBOX(*),IADB,NUMEL,BOXTYPE,ISURF0,IELTYP,
     .        ISU,NIX,IX(NIX,*),NIX1,NIX2,
     .        FLAG , TAGSHELLBOX(*),IEXT
      my_real
     .        X(3,*),SKEW(LSKEW,*)
      TYPE (BOX_)  , DIMENSION(NBBOX)  :: IBOX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,JJ,K,OK,OK1,ISK,IDBX,BOXSEG,
     .        IADB0,JAD,ITYPE,DIF_NIX
C
      my_real
     .    XP1,YP1,ZP1,XP2,YP2,ZP2,DIAM,NODINB(3)
C-----------------------------------------------
            DIF_NIX = NIX2 - NIX1 + 1
            IF(ISURF0 == 0) DIF_NIX = NIX1
C
            OK  = 0
            BOXSEG = 0
            IADB0 = IADB
            TAGSHELLBOX(1:NUMEL) = 0
C-------
            IDBX = IBOX(ISU)%ID
            ISK  = IBOX(ISU)%ISKBOX
            ITYPE= IBOX(ISU)%TYPE
            DIAM = IBOX(ISU)%DIAM
            XP1  = IBOX(ISU)%X1
            YP1  = IBOX(ISU)%Y1
            ZP1  = IBOX(ISU)%Z1
            XP2  = IBOX(ISU)%X2
            YP2  = IBOX(ISU)%Y2
            ZP2  = IBOX(ISU)%Z2
C
            IF(IDBX.NE.0)THEN
              IF (BOXTYPE == 2) THEN
                DO JJ=1,NUMEL
                  OK=0
                  DO K=NIX1,NIX2
                    I=IX(K,JJ)
                    NODINB(1) = X(1,I)
                    NODINB(2) = X(2,I)
                    NODINB(3) = X(3,I)
                    IF(ITYPE == 1)THEN       ! 'RECTA'
                     CALL CHECKPARA(XP1,YP1,ZP1,XP2,YP2,ZP2,
     .                              ISK,NODINB,SKEW,OK)
                    ELSE IF(ITYPE == 2)THEN  ! 'CYLIN'
                     CALL CHECKCYL(XP1, YP1, ZP1 , XP2, YP2, ZP2,
     .                             NODINB  , DIAM, OK )
                    ELSE IF(ITYPE == 3)THEN  ! 'SPHER'
                     CALL CHECKSPHERE(XP1,YP1,ZP1,NODINB,DIAM,OK)
                    END IF
                  ENDDO
                  IF (OK == 1) THEN
                   IF(TAGSHELLBOX(JJ) == 0)THEN
                     BOXSEG=BOXSEG+1
                     TAGSHELLBOX(JJ) = 1
                   END IF
                  ENDIF
                ENDDO
              ELSE IF (BOXTYPE == 1) THEN
                DO JJ=1,NUMEL
                  OK1=0
                  DO K=NIX1,NIX2
                    OK=0
                    I=IX(K,JJ)
                    NODINB(1) = X(1,I)
                    NODINB(2) = X(2,I)
                    NODINB(3) = X(3,I)
                    IF(ITYPE == 1)THEN       ! 'RECTA'
                     CALL CHECKPARA(XP1,YP1,ZP1,XP2,YP2,ZP2,
     .                              ISK,NODINB,SKEW,OK)
                    ELSE IF(ITYPE == 2)THEN  ! 'CYLIN'
                     CALL CHECKCYL(XP1, YP1, ZP1 , XP2, YP2, ZP2,
     .                             NODINB  , DIAM, OK )
                    ELSE IF(ITYPE == 3)THEN  ! 'SPHER'
                     CALL CHECKSPHERE(XP1,YP1,ZP1,NODINB,DIAM,OK)
                    END IF
                    IF(OK == 1) OK1 = OK1 + 1
                  ENDDO
                  IF (OK1 == DIF_NIX) THEN
                   IF(TAGSHELLBOX(JJ) == 0)THEN
                     BOXSEG=BOXSEG+1
                     TAGSHELLBOX(JJ) = 1
                   END IF
                  ENDIF
                ENDDO
              ENDIF
              IBOX(ISU)%NENTITY = BOXSEG
              IBOX(ISU)%BOXIAD = IADB0
            END IF
C
C---count lines within BOX
C
      IF (FLAG == 0) THEN
        IF(IDBX.NE.0)THEN
          DO I=1,NUMEL
            IF(TAGSHELLBOX(I) == 1)THEN
              IF(ISURF0 == 1)THEN
                DO K=NIX1,NIX2
cc               IBUFBOX(IADB) = IX(K,I)
                  IADB = IADB + 1
                ENDDO
              ELSE
cc               IBUFBOX(IADB) = IX(NIX1,I)
                  IADB = IADB + 1
cc               IBUFBOX(IADB) = IX(NIX2,I)
                  IADB = IADB + 1
C
              END IF
              IF(IELTYP == 7)THEN
cc               IBUFBOX(IADB) = IBUFBOX(IADB-1)
                  IADB = IADB + 1
              END IF
cc           IBUFBOX(IADB)=IELTYP
              IADB = IADB + 1
cc           IBUFBOX(IADB)=I
              IADB = IADB + 1
cc
              IF (IEXT > 0) THEN
cc           IBUFBOX(IADB) = IEXT
              IADB = IADB + 1
              ENDIF
            END IF
          END DO
C
        ENDIF
      ELSEIF (FLAG == 1 .AND. BOXSEG > 0) THEN
       DO I=1,NUMEL
         IF(TAGSHELLBOX(I) == 1)THEN
           IF(ISURF0 == 1)THEN
             DO K=NIX1,NIX2
               IBUFBOX(IADB) = IX(K,I)
               IADB = IADB + 1
             ENDDO
           ELSE
               IBUFBOX(IADB) = IX(NIX1,I)
               IADB = IADB + 1
               IBUFBOX(IADB) = IX(NIX2,I)
               IADB = IADB + 1
C
           END IF
           IF(IELTYP == 7)THEN
               IBUFBOX(IADB) = IBUFBOX(IADB-1)
               IADB = IADB + 1
           END IF
           IBUFBOX(IADB)=IELTYP
           IADB = IADB + 1
           IBUFBOX(IADB)=I
           IADB = IADB + 1
           IF (IEXT > 0) THEN
             IBUFBOX(IADB) = IEXT
             IADB = IADB + 1
           ENDIF
         END IF
       END DO
      ENDIF ! IF (FLAG == 0)
C---------------
      RETURN
      END
Chd|====================================================================
Chd|  BIGSBOX                       source/model/box/bigbox.F     
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        BOXBOXS                       source/model/box/boxbox.F     
Chd|        BOX_SURF_SH                   source/model/box/bigbox.F     
Chd|        FREERR                        source/starter/freform.F      
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|====================================================================
      SUBROUTINE BIGSBOX(NUMEL ,IX     ,NIX    ,NIX1 ,NIX2,IELTYP,
     .                   X     ,        NSEG ,FLAG,SKEW  ,
     .                   ISKN  ,ISURF0 ,ITABM1 ,IBOX,
     .                   ID    ,IBUFBOX,ISURFLIN,IADB,KEY   ,
     .                   SBUFBOX,TITR  ,MESS   ,TAGSHELLBOX,
     .                   NN    )
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
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
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
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,JJ,K,K1,J,JREC,ISK,BOXTYPE,ISU,TAGN(NUMEL),
     .    ITYPE,IADBOX,IDB,NBOX,ID,IDBX,BOXSEG,IADISU,
     .    ICOUNT,ITER,FLAGG,NIXEL
      MY_REAL
     .    DIAM,XP1,YP1,ZP1,XP2,YP2,ZP2,NODINB(3)
      CHARACTER BOX*3
      LOGICAL BOOL
C=======================================================================
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
      JREC=IREC+1
      READ(IIN,REC=JREC,ERR=999,FMT='(A)')LINE
      READ(LINE,ERR=999,FMT=FMT_I) IDB
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
C super box activated:
       IBOX(ISU)%ACTIBOX = 1
      ELSE
       IF(FLAG == 0)THEN
        IF(ISURF0 == 0)THEN
          CALL ANCMSG(MSGID=799,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=ID,
     .                C1=TITR,
     .                I2=IDB)
        ELSE IF(ISURF0 == 1)THEN
          CALL ANCMSG(MSGID=800,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=ID,
     .                C1=TITR,
     .                I2=IDB)
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
 999  CALL FREERR(1)
      RETURN
      END
C
Chd|====================================================================
Chd|  ELSTAGBOX                     source/model/box/bigbox.F     
Chd|-- called by -----------
Chd|        ELSTAGBOXBOX                  source/model/box/boxbox.F     
Chd|        SBOXBOXSURF                   source/model/box/bigbox.F     
Chd|-- calls ---------------
Chd|        CHECKCYL                      source/model/box/rdbox.F      
Chd|        CHECKPARA                     source/model/box/rdbox.F      
Chd|        CHECKSPHERE                   source/model/box/rdbox.F      
Chd|        FREERR                        source/starter/freform.F      
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|====================================================================
      SUBROUTINE ELSTAGBOX(IXS    ,ELSTAG ,X   ,SKEW ,BOXTYPE,
     .                     ISU    ,IBOX   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
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
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),ELSTAG(*),BOXTYPE,ISU
      MY_REAL
     .        X(3,*),SKEW(LSKEW,*)
      TYPE (BOX_)  , DIMENSION(NBBOX)  :: IBOX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JJ,JS,K,J,OK,OK1,IDBX,ITYPE,ISK,
     .    FACES(4,6),PWR(7)
      MY_REAL
     .    XP1,YP1,ZP1,XP2,YP2,ZP2,DIAM,NODINB(3)
      DATA FACES/4,3,2,1,
     .           5,6,7,8,
     .           1,2,6,5,
     .           3,4,8,7,
     .           2,3,7,6,
     .           1,5,8,4/
      DATA PWR/1,2,4,8,16,32,64/
C=======================================================================
      OK  = 0
      OK1 = 0
C-------
      IDBX = IBOX(ISU)%ID
      ISK  = IBOX(ISU)%ISKBOX
      ITYPE= IBOX(ISU)%TYPE
      DIAM = IBOX(ISU)%DIAM
      XP1  = IBOX(ISU)%X1
      YP1  = IBOX(ISU)%Y1
      ZP1  = IBOX(ISU)%Z1
      XP2  = IBOX(ISU)%X2
      YP2  = IBOX(ISU)%Y2
      ZP2  = IBOX(ISU)%Z2
C-------
       IF(IDBX.NE.0)THEN
C---
         IF (BOXTYPE == 2) THEN
          DO JS=1,NUMELS
            ELSTAG(JS)=0
            DO JJ=1,6
              DO K=1,4
                OK=0
                J=IXS(FACES(K,JJ)+1,JS)
                NODINB(1) = X(1,J)
                NODINB(2) = X(2,J)
                NODINB(3) = X(3,J)
                IF(ITYPE == 1)THEN       ! 'RECTA'
                  CALL CHECKPARA(XP1,YP1,ZP1,XP2,YP2,ZP2,
     .                           ISK,NODINB,SKEW,OK)
                ELSE IF(ITYPE == 2)THEN  ! 'CYLIN'
                  CALL CHECKCYL(XP1, YP1, ZP1 , XP2, YP2, ZP2,
     .                          NODINB  , DIAM, OK )
                ELSE IF(ITYPE == 3)THEN  ! 'SPHER'
                  CALL CHECKSPHERE(XP1,YP1,ZP1,NODINB,DIAM,OK)
                END IF
                 IF (OK == 1) THEN
               	   ELSTAG(JS)=ELSTAG(JS)+PWR(JJ)
            	 EXIT
                END IF
              ENDDO
            ENDDO
          ENDDO
C---
         ELSE IF (BOXTYPE == 1) THEN
          DO JS=1,NUMELS
            ELSTAG(JS)=0
            DO JJ=1,6
              OK1=0
              DO K=1,4
                OK=0
                J=IXS(FACES(K,JJ)+1,JS)
                NODINB(1) = X(1,J)
                NODINB(2) = X(2,J)
                NODINB(3) = X(3,J)
                IF(ITYPE == 1)THEN       ! 'RECTA'
                  CALL CHECKPARA(XP1,YP1,ZP1,XP2,YP2,ZP2,
     .                           ISK,NODINB,SKEW,OK)
                ELSE IF(ITYPE == 2)THEN  ! 'CYLIN'
                  CALL CHECKCYL(XP1, YP1, ZP1 , XP2, YP2, ZP2,
     .                          NODINB  , DIAM, OK )
                ELSE IF(ITYPE == 3)THEN  ! 'SPHER'
                  CALL CHECKSPHERE(XP1,YP1,ZP1,NODINB,DIAM,OK)
                END IF
                IF(OK == 1) OK1 = OK1 + 1
              ENDDO
              IF (OK1 == 4) THEN
                 ELSTAG(JS)=ELSTAG(JS)+PWR(JJ)
              ENDIF
            ENDDO
          ENDDO
         ENDIF
       ENDIF
C-----------
      RETURN
 999  CALL FREERR(1)
      RETURN
      END
Chd|====================================================================
Chd|  SBOXBOXSURF                   source/model/box/bigbox.F     
Chd|-- called by -----------
Chd|        HM_READ_SURF                  source/groups/hm_read_surf.F  
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        BOXBUFILL                     source/model/box/bigbox.F     
Chd|        ELSTAGBOX                     source/model/box/bigbox.F     
Chd|        ELSTAGBOXBOX                  source/model/box/boxbox.F     
Chd|        FACEBOX                       source/model/box/bigbox.F     
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE SBOXBOXSURF(IXS     ,X      ,NSEG,
     .                       KNOD2ELS,NOD2ELS,IEXT ,FLAG,
     .                       IXS10   ,IXS16  ,IXS20,SKEW   ,IBOX,
     .                       ID      ,IBUFBOX,IADB ,KEY ,
     .                       SBUFBOX ,TITR   ,KNOD2ELC,NOD2ELC,IXC,
     .                       TAGSHELLBOXC,KNOD2ELTG,NOD2ELTG,IXTG ,
     .                       TAGSHELLBOXG,IGRSURF,NN,NSEG0,LSUBMODEL)
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
C   A n a l y s e   M o d u l e
C-----------------------------------------------
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
      INTEGER IXS(NIXS,*),NSEG,KNOD2ELS(*),
     .        NOD2ELS(*),IEXT,FLAG,IXS10(6,*),
     .        IXS16(8,*),IXS20(12,*),ID,IBUFBOX(*),
     .        KNOD2ELC(*),NOD2ELC(*),IXC(NIXC,*),TAGSHELLBOXC(*),
     .        KNOD2ELTG(*),NOD2ELTG(*),IXTG(NIXTG,*) ,TAGSHELLBOXG(*),
     .        IADB,SBUFBOX,NN,NSEG0
      MY_REAL
     .        X(3,*),SKEW(LSKEW,*)
      CHARACTER KEY*4
      CHARACTER*nchartitle,
     .   TITR
      TYPE (SURF_) :: IGRSURF
      TYPE (BOX_)  , DIMENSION(NBBOX)  :: IBOX
      TYPE(SUBMODEL_DATA) LSUBMODEL(NSUBMOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,JREC,IDB,BOXTYPE,ISU,ICOUNT,ITER,FLAGG,
     .        BOXSEG,IADISU,ELSTAG(NUMELS)
      CHARACTER BOX*3
      LOGICAL BOOL,IS_AVAILABLE,IS_ENCRYPTED
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
         IBOX(I)%SURFIAD = 0 ! used for temporary storage
       END DO
C-------
      CALL HM_GET_INT_ARRAY_INDEX('ids',IDB,1,IS_AVAILABLE,LSUBMODEL)
      IF(KEY == 'BOX')THEN
        BOXTYPE = 1
      ELSE IF(KEY == 'BOX2')THEN
        BOXTYPE = 2
      END IF
C-------
C get box de box ID'S dans SURF:
C-------
      ISU = 0
      DO I=1,NBBOX
        IF(IDB == IBOX(I)%ID) ISU=I
      END DO
C---
      IF(ISU .LE. 0)THEN
       IF(FLAG == 0)THEN
        CALL ANCMSG(MSGID=800,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=ID,
     .              C1=TITR,
     .              I2=IDB)
       END IF
      END IF
C---
C   tag surfaces from solids within a simple box:
C---
      BOOL = .FALSE.
      IF(ISU>0)THEN
        IF(IBOX(ISU)%NBLEVELS == 0 .AND. IBOX(ISU)%LEVEL == 1)THEN
          IF(IBOX(ISU)%NBOXBOX == 0)THEN
C---
C    tag faces of solids within box
C---
            CALL ELSTAGBOX(IXS    ,ELSTAG ,X    ,SKEW  ,BOXTYPE,ISU    ,IBOX   )
C---
C    fill tmp "IBUFBOX" for taged faces
C---
            CALL FACEBOX(IXS          ,X         ,KNOD2ELS ,NOD2ELS,IEXT        ,
     .                   FLAG         ,IXS10     ,IXS16    ,IXS20  ,SKEW        ,
     .                   IBOX         ,ELSTAG    ,IBUFBOX  ,IADB   ,ISU         ,
     .                   ID           ,TITR      ,KNOD2ELC ,NOD2ELC,IXC         ,
     .                   TAGSHELLBOXC ,KNOD2ELTG ,NOD2ELTG ,IXTG   ,TAGSHELLBOXG,
     .                   0            )
            IF (IADB>SBUFBOX .OR. IADB<0) CALL ANCMSG(MSGID=1007, MSGTYPE=MSGERROR,ANMODE=ANSTOP)
            BOOL = .TRUE.
          END IF
        END IF
       ENDIF
C---
C READ LEVELS OF BOXES ==> "SUBLEVEL DONE"
C---
C---
C    fill tmp "IBUFBOX" for taged faces
C---
      IF(.NOT. BOOL)THEN
        ICOUNT = 1
        ITER   = 0
        DO WHILE (ICOUNT == 1)
          ITER  = ITER  + 1
          FLAGG = 0
C---      count next level
          CALL ELSTAGBOXBOX(
     .             IBOX     ,SKEW    ,FLAGG       ,ICOUNT      ,ITER     ,
     .             BOXTYPE  ,IBUFBOX ,X           ,IADB        ,IXS      ,
     .             KNOD2ELS ,NOD2ELS ,IEXT        ,FLAG        ,IXS10    ,
     .             IXS16    ,IXS20   ,ELSTAG      ,ID          ,TITR     ,
     .             KNOD2ELC ,NOD2ELC ,IXC         ,TAGSHELLBOXC,KNOD2ELTG,
     .             NOD2ELTG ,IXTG    ,TAGSHELLBOXG,0           )
          IF (IADB>SBUFBOX .OR. IADB<0) 
     .      CALL ANCMSG(MSGID=1007, MSGTYPE=MSGERROR,ANMODE=ANSTOP)
C---      fill next level
          FLAGG = 1
          CALL ELSTAGBOXBOX(
     .             IBOX     ,SKEW    ,FLAGG       ,ICOUNT      ,ITER     ,
     .             BOXTYPE  ,IBUFBOX ,X           ,IADB        ,IXS      ,
     .             KNOD2ELS ,NOD2ELS ,IEXT        ,FLAG        ,IXS10    ,
     .             IXS16    ,IXS20   ,ELSTAG      ,ID          ,TITR     ,
     .             KNOD2ELC ,NOD2ELC ,IXC         ,TAGSHELLBOXC,KNOD2ELTG,
     .             NOD2ELTG ,IXTG    ,TAGSHELLBOXG,0           )
        ENDDO
      ENDIF

C--------------------------
C
C  fill final  - IBUFSSG - for taged faces
C
      IF(ISU > 0)THEN
        BOXSEG = IBOX(ISU)%NENTITY  ! nb of surfaces in main box (/box/box)
        IADISU = IBOX(ISU)%SURFIAD  ! addresses of surfaces in main box
        IF(FLAG == 0)THEN
          NSEG=NSEG+BOXSEG
        ELSE IF(FLAG == 1)THEN
          NSEG=NSEG+BOXSEG
          DO I=1,BOXSEG
            NN = NN + 1
            CALL BOXBUFILL(IADISU,IBUFBOX,IGRSURF%NODES,NN,NSEG0,
     .                     IGRSURF%ELTYP,IGRSURF%ELEM)
          END DO
        END IF
      END IF
C--------------------------
      RETURN
 999  CALL FREERR(1)
      RETURN
      END
Chd|====================================================================
Chd|  FACEBOX                       source/model/box/bigbox.F     
Chd|-- called by -----------
Chd|        ELSTAGBOXBOX                  source/model/box/boxbox.F     
Chd|        SBOXBOXSURF                   source/model/box/bigbox.F     
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        SSURF10TMP                    source/model/box/bigbox.F     
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|====================================================================
      SUBROUTINE FACEBOX(IXS         ,X        ,KNOD2ELS ,NOD2ELS,IEXT        ,
     .                   FLAG        ,IXS10    ,IXS16    ,IXS20  ,SKEW        ,
     .                   IBOX        ,ELSTAG   ,IBUFBOX  ,IADB   ,ISU         ,
     .                   ID          ,TITR     ,KNOD2ELC ,NOD2ELC,IXC         ,
     .                   TAGSHELLBOXC,KNOD2ELTG,NOD2ELTG ,IXTG   ,TAGSHELLBOXG,
     .                   IEXT_SET    )
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
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ID,IEXT_SET
      INTEGER IXS(NIXS,*),KNOD2ELS(*),
     .        NOD2ELS(*),IEXT,FLAG,IXS10(6,*),IXS16(8,*),
     .        IXS20(12,*),IBUFBOX(*),IADB,ISU,ELSTAG(*),
     .        KNOD2ELC(*),NOD2ELC(*),IXC(NIXC,*),TAGSHELLBOXC(*),KNOD2ELTG(*),
     .        NOD2ELTG(*),IXTG(NIXTG,*),TAGSHELLBOXG(*)
      MY_REAL
     .        X(3,*),SKEW(LSKEW,*)
      CHARACTER TITR*nchartitle
C-----------------------------------------------
      TYPE (BOX_)  , DIMENSION(NBBOX)  :: IBOX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,J,K,JS,KS,II,JJ,K1,K2,LL,FACE(4),FC10(3),NN,KK,I1,
     .        NI(4),NS(4),MI(4),MS(4),NMIN,MMIN,NF,MF,IPERM,N1,N2,
     .        BOXSEG,IADB0,NNS,ISHEL
      INTEGER NODTAG(NUMNOD),FASTAG(NUMELS)
      INTEGER FACES(4,6),PWR(7),FACES10(3,6)
      DATA FACES/4,3,2,1,
     .           5,6,7,8,
     .           1,2,6,5,
     .           3,4,8,7,
     .           2,3,7,6,
     .           1,5,8,4/
      DATA FACES10/0,0,0,
     .             0,0,0,
     .             3,6,4,
     .             5,6,2,
     .             1,2,3,
     .             4,5,1/
      DATA PWR/1,2,4,8,16,32,64/
      CHARACTER BOX*3
C-----------------------------------------------
      BOXSEG = 0
      IADB0  = IADB
C---
C    fill tmp "IBUFBOX" for taged faces
C---
C-------------------------
      IF(IEXT==0)THEN
        DO JS=1,NUMELS
          IF(ELSTAG(JS)/=0)THEN
            CALL ANCMSG(MSGID=802,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=ID,
     .                  C1=TITR)
          END IF
        ENDDO
      ENDIF
C-------------------------
C
      FASTAG=0
C
      IF(IEXT==1)THEN
C--------------------------
C       External surface only
C--------------------------
        DO JS=1,NUMELS8+NUMELS10
          DO JJ=1,6
            IF(MOD(ELSTAG(JS),PWR(JJ+1))/PWR(JJ)==0)CYCLE
            DO II=1,4
              NS(II)=IXS(FACES(II,JJ)+1,JS)
            END DO
C
C           keep only 1 occurence of each node (triangles, degenerated cases...)
C
            DO K1=1,3
            DO K2=K1+1,4
              IF(NS(K2)==NS(K1))NS(K2)=0
            END DO
            END DO
            NF=0
            DO K1=1,4
              N1=NS(K1)
              IF(N1/=0)THEN
        	NF=NF+1
        	NS(NF)=N1
              END IF
            END DO
            IF(NF < 3)CYCLE
C
C           permute
C
            NMIN=NS(1)
            DO II=2,NF
              NMIN=MIN(NMIN,NS(II))
            END DO
            DO IPERM=1,NF
              IF(NMIN==NS(IPERM).AND.
     .           NS(MOD(IPERM,NF)+1)/=NS(IPERM))THEN
                DO II=1,NF
                  NI(II)=NS(MOD(II+IPERM-2,NF)+1)
                END DO
                EXIT
              END IF
            END DO
C
C           looks for an elt sharing the face.
C
            DO K=KNOD2ELS(NI(1))+1,KNOD2ELS(NI(1)+1)
              KS=NOD2ELS(K)
              IF(KS==JS .OR. KS > NUMELS8+NUMELS10 .OR.
     .           ELSTAG(KS)==0) CYCLE
              DO II=1,NF
                NODTAG(NI(II))=0
              END DO
              DO II=1,8
                NODTAG(IXS(II+1,KS))=1
              END DO
              NN=0
              DO II=1,NF
                NN=NN+NODTAG(NI(II))
              END DO
              IF(NN==NF)THEN
                DO KK=1,6
            	  DO II=1,4
            	    MS(II)=IXS(FACES(II,KK)+1,KS)
            	  END DO
C
C                 keep only 1 occurence of each node (triangles, degenerated cases...)
C
            	  DO K1=1,3
            	  DO K2=K1+1,4
            	    IF(MS(K2)==MS(K1))MS(K2)=0
            	  END DO
            	  END DO
            	  MF=0
            	  DO K1=1,4
            	    N1=MS(K1)
            	    IF(N1/=0)THEN
              	      MF=MF+1
              	      MS(MF)=N1
            	    END IF
            	  END DO
            	  IF(MF /= NF)CYCLE
C
C                 permute
C
           	  MMIN=MS(1)
           	  DO II=2,MF
           	    MMIN=MIN(MMIN,MS(II))
           	  END DO
            	  DO IPERM=1,MF
            	    IF(MMIN==MS(IPERM).AND.
     .                 MS(MOD(IPERM,MF)+1)/=MS(IPERM))THEN
            	      DO II=1,MF
            	  	MI(II)=MS(MOD(II+IPERM-2,MF)+1)
            	      END DO
            	      EXIT
            	    END IF
            	  END DO
                  IF(MI(1)==NI(1).AND.MI(NF)==NI(2))THEN
C                    FACTAG(JS) moins face jj
                     FASTAG(JS)=FASTAG(JS)+PWR(JJ)
                     GO TO 100
                  END IF
                END DO
              END IF
            END DO
 100        CONTINUE
          END DO
        END DO
      END IF
C--------------------------
C
C--------------------------
      DO JS=1,NUMELS8
        IF(ELSTAG(JS).GT.0)THEN
          DO JJ=1,6
            IF(MOD(ELSTAG(JS),PWR(JJ+1))/PWR(JJ)/=0 .AND.
     .         MOD(FASTAG(JS),PWR(JJ+1))/PWR(JJ)==0)THEN
C
C           still needs to filter degenerated faces
C
            DO K1=1,4
              I1      =FACES(K1,JJ)+1
              FACE(K1)=IXS(I1,JS)
            END DO
            DO K1=1,4
              N1=FACE(K1)
              DO K2=1,4
        	IF(K2/=K1)THEN
        	  N2=FACE(K2)
        	  IF(N2==N1)FACE(K2)=0
        	END IF
              END DO
            END DO
            NN=0
            DO K1=1,4
              N1=FACE(K1)
              IF(N1/=0)THEN
        	NN=NN+1
        	FACE(NN)=N1
              END IF
            END DO


C---   find shells SURF/BOX/BOX/EXT

C
C count faces within the box:
C
             IF(FLAG == 0 .and. NN == 3) THEN 
               KS = 0 
               ISHEL = 0 
               DO K=KNOD2ELTG(FACE(1))+1,KNOD2ELTG(FACE(1)+1)
                 KS=NOD2ELTG(K)
                 ISHEL = 0
                 DO I=1,3
                   DO J=1,3
                     IF(FACE(I) == IXTG(J+1,KS)) ISHEL = ISHEL + 1
                   ENDDO
                 ENDDO
                 IF (ISHEL == 3)EXIT
                 KS = 0
               ENDDO
               IF(KS == 0)THEN
                 BOXSEG=BOXSEG+1
                 IADB = IADB + 6
                 IF (IEXT_SET > 0) IADB = IADB + 1
               ELSEIF(TAGSHELLBOXG(KS)==0) THEN
                 BOXSEG=BOXSEG+1
                 IADB = IADB + 6
                 IF (IEXT_SET > 0) IADB = IADB + 1
               ENDIF
             ELSEIF(FLAG == 0 .and. NN == 4) THEN 
               KS = 0
               ISHEL = 0
               DO K=KNOD2ELC(FACE(1))+1,KNOD2ELC(FACE(1)+1)
                 KS=NOD2ELC(K)
                 ISHEL = 0
                 DO I=1,4
                   DO J=1,4
                     IF(FACE(I) == IXC(J+1,KS)) ISHEL = ISHEL + 1
                   ENDDO
                 ENDDO
                 IF (ISHEL == 4)EXIT
                 KS = 0
               ENDDO
               IF(KS == 0)THEN
                 BOXSEG=BOXSEG+1
                 IADB = IADB + 6
                 IF (IEXT_SET > 0) IADB = IADB + 1
               ELSEIF(TAGSHELLBOXC(KS)==0) THEN
                 BOXSEG=BOXSEG+1
                 IADB = IADB + 6
                 IF (IEXT_SET > 0) IADB = IADB + 1
               ENDIF
             ELSEIF(NN==3)THEN
               KS = 0
               ISHEL = 0
               DO K=KNOD2ELTG(FACE(1))+1,KNOD2ELTG(FACE(1)+1)
                 KS=NOD2ELTG(K)
                 ISHEL = 0
                 DO I=1,3
                   DO J=1,3
                     IF(FACE(I) == IXTG(J+1,KS)) ISHEL = ISHEL + 1
                   ENDDO
                 ENDDO
                 IF (ISHEL == 3)EXIT
                 KS = 0
               ENDDO
               IF(KS == 0)THEN
                 BOXSEG=BOXSEG+1
                 CALL SSURF10TMP(FACE(1),FACE(2),FACE(3),FACE(3),
     .                        IADB,JS,IBUFBOX,IEXT_SET)                   
               ELSEIF(TAGSHELLBOXG(KS)==0) THEN
                   BOXSEG=BOXSEG+1
                   CALL SSURF10TMP(FACE(1),FACE(2),FACE(3),FACE(3),
     .                       IADB,JS,IBUFBOX,IEXT_SET) 
               ENDIF               
             ELSEIF(NN==4)THEN
               KS = 0
               ISHEL = 0
               DO K=KNOD2ELC(FACE(1))+1,KNOD2ELC(FACE(1)+1)
                 KS=NOD2ELC(K)
                 ISHEL = 0
                 DO I=1,4
                   DO J=1,4
                     IF(FACE(I) == IXC(J+1,KS)) ISHEL = ISHEL + 1
                   ENDDO
                 ENDDO
                 IF (ISHEL == 4)EXIT
                 KS = 0
               ENDDO
               IF(KS == 0)THEN
                 BOXSEG=BOXSEG+1
                CALL SSURF10TMP(FACE(1),FACE(2),FACE(3),FACE(4),
     .                       IADB,JS,IBUFBOX,IEXT_SET)  
               ELSEIF(TAGSHELLBOXC(KS)==0) THEN
                 BOXSEG=BOXSEG+1
                CALL SSURF10TMP(FACE(1),FACE(2),FACE(3),FACE(4),
     .                       IADB,JS,IBUFBOX,IEXT_SET) 
  
               ENDIF                
             END IF
C---
            END IF
          END DO
        ENDIF
      ENDDO
C--------------------------
C
C--------------------------
      DO J=1,NUMELS10
        JS = J+NUMELS8
        IF(ELSTAG(JS).GT.0)THEN
          DO JJ=3,6
            IF(MOD(ELSTAG(JS),PWR(JJ+1))/PWR(JJ)/=0 .AND.
     .         MOD(FASTAG(JS),PWR(JJ+1))/PWR(JJ)==0)THEN
C
C           still needs to filter degenerated faces
C
            DO K1=1,4
              FACE(K1)=IXS(FACES(K1,JJ)+1,JS)
            END DO
            DO K1=1,3
              DO K2=K1+1,4
        	IF(FACE(K2) == FACE(K1)) FACE(K2)=0
              END DO
            END DO
            NN=0
            DO K1=1,4
              IF(FACE(K1) /= 0)THEN
        	NN=NN+1
        	FACE(NN)=FACE(K1)
              END IF
            END DO
C
             IF(NN == 3)THEN
               NNS=1
               FC10(1)=IXS10(FACES10(1,JJ),J)
               FC10(2)=IXS10(FACES10(2,JJ),J)
               FC10(3)=IXS10(FACES10(3,JJ),J)
               IF(FC10(1) /= 0)NNS=NNS+1
               IF(FC10(2) /= 0)NNS=NNS+1
               IF(FC10(3) /= 0)NNS=NNS+1
C
C count faces within the box:
C
                IF(NNS == 3)NNS=2
                BOXSEG=BOXSEG+NNS
                IF(NNS == 4)THEN
c                 4 triangles
C
                  IF (FLAG == 0)THEN
                    IADB = IADB + 24
                    IF (IEXT_SET > 0) IADB = IADB + 4
                  ELSEIF (FLAG == 1) THEN
                    CALL SSURF10TMP(FACE(1),FC10(1),FC10(3),FC10(3),
     .                           IADB,JS,IBUFBOX,IEXT_SET)
                    CALL SSURF10TMP(FACE(2),FC10(2),FC10(1),FC10(1),
     .                           IADB,JS,IBUFBOX,IEXT_SET)
                    CALL SSURF10TMP(FACE(3),FC10(3),FC10(2),FC10(2),
     .                           IADB,JS,IBUFBOX,IEXT_SET)
                    CALL SSURF10TMP(FC10(1),FC10(2),FC10(3),FC10(3),
     .                           IADB,JS,IBUFBOX,IEXT_SET)
                  ENDIF ! IF (FLAG == 0)
                ELSEIF(NNS == 3)THEN
c               1 quadrangle, 1 triangle
C
                  IF (FLAG == 0)THEN
                    IADB = IADB + 12
                    IF (IEXT_SET > 0) IADB = IADB + 2
                  ELSEIF (FLAG == 1) THEN
                    IF(FC10(1) == 0)THEN
                      CALL SSURF10TMP(FACE(1),FACE(2),FC10(2),FC10(3),
     .                             IADB,JS,IBUFBOX,IEXT_SET)
                      CALL SSURF10TMP(FACE(3),FC10(3),FC10(2),FC10(2),
     .                             IADB,JS,IBUFBOX,IEXT_SET)
                    ELSEIF(FC10(2) == 0)THEN
                      CALL SSURF10TMP(FACE(2),FACE(3),FC10(3),FC10(1),
     .                             IADB,JS,IBUFBOX,IEXT_SET)
                      CALL SSURF10TMP(FACE(1),FC10(1),FC10(3),FC10(3),
     .                             IADB,JS,IBUFBOX,IEXT_SET)
                    ELSEIF(FC10(3) == 0)THEN
                      CALL SSURF10TMP(FACE(3),FACE(1),FC10(1),FC10(2),
     .                             IADB,JS,IBUFBOX,IEXT_SET)
                      CALL SSURF10TMP(FACE(2),FC10(2),FC10(1),FC10(1),
     .                             IADB,JS,IBUFBOX,IEXT_SET)
                    ENDIF
                  ENDIF ! IF (FLAG == 0)
                ELSEIF(NNS == 2)THEN
c               2 triangles
C
                  IF (FLAG == 0)THEN
                    IADB = IADB + 12
                    IF (IEXT_SET > 0) IADB = IADB + 2
                  ELSEIF (FLAG == 1) THEN
                    IF(FC10(1) /= 0)THEN
                      CALL SSURF10TMP(FACE(3),FACE(1),FC10(1),FC10(1),
     .                             IADB,JS,IBUFBOX,IEXT_SET)
                      CALL SSURF10TMP(FACE(2),FACE(3),FC10(1),FC10(1),
     .                             IADB,JS,IBUFBOX,IEXT_SET)
                    ELSEIF(FC10(2) /= 0)THEN
                      CALL SSURF10TMP(FACE(1),FACE(2),FC10(2),FC10(2),
     .                             IADB,JS,IBUFBOX,IEXT_SET)
                      CALL SSURF10TMP(FACE(3),FACE(1),FC10(2),FC10(2),
     .                             IADB,JS,IBUFBOX,IEXT_SET)
                    ELSEIF(FC10(3) /= 0)THEN
                      CALL SSURF10TMP(FACE(2),FACE(3),FC10(3),FC10(3),
     .                             IADB,JS,IBUFBOX,IEXT_SET)
                      CALL SSURF10TMP(FACE(1),FACE(2),FC10(3),FC10(3),
     .                             IADB,JS,IBUFBOX,IEXT_SET)
                    ENDIF
                  ENDIF ! IF (FLAG == 0)
                ELSEIF(NNS == 1)THEN
c                 1 triangle
C
                  IF (FLAG == 0)THEN
                    IADB = IADB + 6
                    IF (IEXT_SET > 0) IADB = IADB + 1
                  ELSEIF (FLAG == 1) THEN
                    CALL SSURF10TMP(FACE(1),FACE(2),FACE(3),FACE(3),
     .                           IADB,JS,IBUFBOX,IEXT_SET)
                  ENDIF ! IF (FLAG == 0)
                END IF
             END IF
            END IF
          END DO
        ENDIF
      ENDDO
C
cc      IF (FLAG == 0) IADB = IADB + BOXSEG
C--------------------------
      IF(ISU>0)THEN
        IF(IBOX(ISU)%ID > 0)THEN
          IBOX(ISU)%NENTITY=BOXSEG
          IBOX(ISU)%SURFIAD=IADB0
        END IF
      ENDIF
C--------------------------
C
      RETURN
 999  CALL FREERR(1)
      RETURN
      END
Chd|====================================================================
Chd|  BOXBUFILL                     source/model/box/bigbox.F     
Chd|-- called by -----------
Chd|        SBOXBOXSURF                   source/model/box/bigbox.F     
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE BOXBUFILL(IADISU,IBUFBOX,SURF_NODES,NN,NSEG0,
     .                     SURF_ELTYP,SURF_ELEM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
      INTEGER J,IADISU,IBUFBOX(*),NN,NSEG0,SURF_NODES(NSEG0,4),
     .        SURF_ELTYP(NSEG0),SURF_ELEM(NSEG0)
C---------------------------------
      J=IBUFBOX(IADISU)   ! N1
      IADISU=IADISU+1
      SURF_NODES(NN,1) = J
C
      J=IBUFBOX(IADISU)   ! N2
      IADISU=IADISU+1
      SURF_NODES(NN,2) = J
C
      J=IBUFBOX(IADISU)   ! N3
      IADISU=IADISU+1
      SURF_NODES(NN,3) = J
C
      J=IBUFBOX(IADISU)   ! N4
      IADISU=IADISU+1
      SURF_NODES(NN,4) = J
C
      J=IBUFBOX(IADISU)   ! IELTYP
      IADISU=IADISU+1
      SURF_ELTYP(NN) = J
C
      J=IBUFBOX(IADISU)   ! JS - element id
      IADISU=IADISU+1
      SURF_ELEM(NN) = J
C
      RETURN
      END
Chd|====================================================================
Chd|  SSURF10TMP                    source/model/box/bigbox.F     
Chd|-- called by -----------
Chd|        FACEBOX                       source/model/box/bigbox.F     
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SSURF10TMP(N1,N2,N3,N4,IAD,JS,IBUFBOX,IEXT_SET) 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N1,N2,N3,N4,IAD,JS,IBUFBOX(*),IEXT_SET
C-----------------------------------------------
      IBUFBOX(IAD)=N1
      IAD=IAD+1
      IBUFBOX(IAD)=N2
      IAD=IAD+1
      IBUFBOX(IAD)=N3
      IAD=IAD+1
      IBUFBOX(IAD)=N4
      IAD=IAD+1
      IBUFBOX(IAD)=1
      IAD=IAD+1
      IBUFBOX(IAD)=JS
      IAD=IAD+1
!
      IF (IEXT_SET > 0) THEN
        IBUFBOX(IAD) = 2  ! IEXT = 2 (all - need for /LINE/SURF)
        IAD=IAD+1
      ENDIF
!---
      RETURN
      END
