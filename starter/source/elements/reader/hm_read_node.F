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
C
Chd|====================================================================
Chd|  HM_READ_NODE                  source/elements/reader/hm_read_node.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        CONSTIT                       source/elements/nodes/constit.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        USRTOS                        source/system/sysfus.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F
Chd|====================================================================
      SUBROUTINE HM_READ_NODE(X       ,ITAB,ITABM1,CMERGE,UNITAB,
     .                        WIGE    ,LSUBMODEL, IS_DYNA)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ /NODE ELEMENTS USING HM_READER
C   READ /CNODE ELEMENTS USING 'READ IN FILE'
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME            DESCRIPTION
C
C     X               NODE COORDS
C     ITAB            USER ID OF NODES
C     ITABM1          REVERSE TAB ITA
C     CMERGE          CNODE MERGING DISTANCE
C     UNITAB          UNITS ARRAY
C     WIGE            WEIGHT FOR IGEO ANALYSIS
C     LSUBMODEL       SUBMODEL STRUCTURE
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      INTEGER, INTENT(IN) :: IS_DYNA
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C OUTPUT ARGUMENTS
      INTEGER,INTENT(OUT)::ITAB(*)
      INTEGER,INTENT(OUT)::ITABM1(*)
      my_real,
     .  INTENT(OUT)::X(3,*)
      my_real,
     .  INTENT(OUT)::CMERGE(*)
      my_real,
     .  INTENT(OUT)::WIGE(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "hash_id.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com10_c.inc"
#include      "com_xfem1.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr06_c.inc"
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "titr_c.inc"
#include      "sphcom.inc"
#include      "remesh_c.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,M,I,J,J1,NN, IOUTN, IERROR, STAT
      INTEGER NUMNUSR,NUMNUSR1,NUMNAUX,KSPHRES,NUMNUSR2
      INTEGER CHID, CNT1, CNT2, UID, IFLAGUNIT, ID
      INTEGER FLAG_FMT,FLAG_FMT_TMP,IFIX_TMP
      my_real
     .    X1,X2,X3,XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,FAC_L,
     .    W
      CHARACTER LLINE*ncharline, KEY*ncharfield
      CHARACTER MOT1*ncharfield
      INTEGER, DIMENSION(:), ALLOCATABLE :: SUB_NOD,UID_NOD,ITAB_TMP,ITABM1_TMP
      REAL*8, DIMENSION(:,:), ALLOCATABLE :: HM_X
      my_real, DIMENSION(:,:), ALLOCATABLE ::
     .                                        X_TMP
      LOGICAL IS_AVAILABLE
      REAL*8, DIMENSION(:), ALLOCATABLE :: DMERGE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USRTOS
C=======================================================================
      FAC_L = ONE

      XMIN = EP20
      YMIN = EP20
      ZMIN = EP20
      XMAX =-EP20
      YMAX =-EP20
      ZMAX =-EP20
C--------------------------------------------------
C      ALLOCS & INITS
C--------------------------------------------------
      CALL CPP_NODES_COUNT(NUMNUSR1,NUMNUSR2)
      ALLOCATE (SUB_NOD(NUMNUSR1),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='SUB_NOD')
      ALLOCATE (UID_NOD(NUMNUSR1),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='UID_NOD')
      ALLOCATE (HM_X(3,NUMNUSR1),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='HM_X')
      ALLOCATE (DMERGE(NUMNUSR2),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='DMERGE')
      SUB_NOD(1:NUMNUSR1) = 0
      UID_NOD(1:NUMNUSR1) = 0
      HM_X(1:3,1:NUMNUSR1) = 0
      DMERGE(1:NUMNUSR2) = ZERO

      IF(IS_DYNA==0)THEN
C--------------------------------------------------
C        READING NODES INPUTS IN HM STRUCTURE
C----------------------------------------------------
        W = ZERO
        CALL CPP_NODE_READ(ITAB,HM_X,W,SUB_NOD,UID_NOD)
C----------------------------------------------------
C        FILL OTHER STRUCTURES + CHECKS
C----------------------------------------------------
        UID = -1
        N   = 0
        DO I=1,NUMNUSR1
          N=N+1
          X(1,N) = HM_X(1,N)
          X(2,N) = HM_X(2,N)
          X(3,N) = HM_X(3,N)
C---------------------------------------------------
C        SUBMODEL OFFSET
C---------------------------------------------------
          IF(SUB_NOD(N) /= 0)THEN
            IF(UID_NOD(N) == 0 .AND. LSUBMODEL(SUB_NOD(N))%UID /= 0)
     .        UID_NOD(N) = LSUBMODEL(SUB_NOD(N))%UID
          ENDIF
C---------------------------------------------------
C        UNITS
C---------------------------------------------------
          IF(UID_NOD(N) /= UID )THEN
            UID = UID_NOD(N)
            IFLAGUNIT = 0
            DO J=1,NUNITS
              IF (UNITAB%UNIT_ID(J) == UID) THEN
                FAC_L = UNITAB%FAC_L(J)
                IFLAGUNIT = 1
                EXIT
              ENDIF
            ENDDO
            IF (UID/=0 .AND. IFLAGUNIT==0)THEN
              CALL ANCMSG(MSGID=643,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                    I1=UID,C1='/NODE')
            ENDIF
          ENDIF
          X(1,N) = X(1,N)*FAC_L
          X(2,N) = X(2,N)*FAC_L
          X(3,N) = X(3,N)*FAC_L
C----------------------------------------------------
          IF(NUMELIG3D > 0)  WIGE(N) = W
          XMIN=MIN(XMIN,X(1,N))
          YMIN=MIN(YMIN,X(2,N))
          ZMIN=MIN(ZMIN,X(3,N))
          XMAX=MAX(XMAX,X(1,N))
          YMAX=MAX(YMAX,X(2,N))
          ZMAX=MAX(ZMAX,X(3,N))
        ENDDO
      ELSE ! IF(IS_DYNA==0)THEN
C
C       Possible nodes merging !
        ALLOCATE (ITAB_TMP(NUMNUSR1),STAT=stat)
        IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='ITAB_TMP')
        ALLOCATE (ITABM1_TMP(2*NUMNUSR1),STAT=stat)
        IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='ITABM1_TMP')
        ALLOCATE (X_TMP(3,NUMNUSR1),STAT=stat)
        IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='X_TMP')
C--------------------------------------------------
C        READING NODES INPUTS IN HM STRUCTURE
C----------------------------------------------------
        W = ZERO
        CALL CPP_NODE_READ(ITAB_TMP,HM_X,W,SUB_NOD,UID_NOD)
C----------------------------------------------------
C        FILL OTHER STRUCTURES + CHECKS
C----------------------------------------------------
        UID = -1
        N   = 0
        DO I=1,NUMNUSR1
          N=N+1
          X_TMP(1,N) = HM_X(1,N)
          X_TMP(2,N) = HM_X(2,N)
          X_TMP(3,N) = HM_X(3,N)
C---------------------------------------------------
C        SUBMODEL OFFSET
C---------------------------------------------------
          IF(SUB_NOD(N) /= 0)THEN
            IF(UID_NOD(N) == 0 .AND. LSUBMODEL(SUB_NOD(N))%UID /= 0)
     .        UID_NOD(N) = LSUBMODEL(SUB_NOD(N))%UID
          ENDIF
C---------------------------------------------------
C        UNITS
C---------------------------------------------------
          IF(UID_NOD(N) /= UID )THEN
            UID = UID_NOD(N)
            IFLAGUNIT = 0
            DO J=1,NUNITS
              IF (UNITAB%UNIT_ID(J) == UID) THEN
                FAC_L = UNITAB%FAC_L(J)
                IFLAGUNIT = 1
                EXIT
              ENDIF
            ENDDO
            IF (UID/=0 .AND. IFLAGUNIT==0)THEN
              CALL ANCMSG(MSGID=643,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                    I1=UID,C1='/NODE')
            ENDIF
          ENDIF
          X_TMP(1,N) = X_TMP(1,N)*FAC_L
          X_TMP(2,N) = X_TMP(2,N)*FAC_L
          X_TMP(3,N) = X_TMP(3,N)*FAC_L
C----------------------------------------------------
          IF(NUMELIG3D > 0)  WIGE(N) = W
          XMIN=MIN(XMIN,X_TMP(1,N))
          YMIN=MIN(YMIN,X_TMP(2,N))
          ZMIN=MIN(ZMIN,X_TMP(3,N))
          XMAX=MAX(XMAX,X_TMP(1,N))
          YMAX=MAX(YMAX,X_TMP(2,N))
          ZMAX=MAX(ZMAX,X_TMP(3,N))
        ENDDO
C--------------------------------------------------
C      USER NODE: INVERSE ARRAY
C--------------------------------------------------
        CALL CONSTIT(ITAB_TMP,ITABM1_TMP,NUMNUSR1)
C
C       Compaction of ITAB and X (cf /MERGE/NODE) - Nodes are sorted by ID
        NUMNAUX = 1
        ITAB(1)  = ITABM1_TMP(1)
        X(1:3,1) = X_TMP(1:3,ITABM1_TMP(NUMNUSR1+1))
        DO I=2,NUMNUSR1
          IF(ITABM1_TMP(NUMNUSR1+I) == ITABM1_TMP(NUMNUSR1+I-1)) CYCLE ! Twice the same ID
          NUMNAUX        = NUMNAUX + 1
          ITAB(NUMNAUX)  = ITABM1_TMP(I)
          X(1:3,NUMNAUX) = X_TMP(1:3,ITABM1_TMP(NUMNUSR1+I))
        ENDDO
        NUMNUSR1 = NUMNAUX
C--------------------------------------------------
        IF(ALLOCATED(ITAB_TMP))   DEALLOCATE(ITAB_TMP)
        IF(ALLOCATED(ITABM1_TMP)) DEALLOCATE(ITABM1_TMP)
        IF(ALLOCATED(X_TMP))      DEALLOCATE(X_TMP)
      END IF ! IF(IS_DYNA==0)THEN
C--------------------------------------------------
      IF(ALLOCATED(SUB_NOD)) DEALLOCATE(SUB_NOD)
      IF(ALLOCATED(UID_NOD)) DEALLOCATE(UID_NOD)
      IF(ALLOCATED(HM_X))    DEALLOCATE(HM_X)
C--------------------------------------------------
C        READING CNODES INPUTS IN HM STRUCTURE
C--------------------------------------------------
C
      IF(NUMNUSR2 /= 0) THEN
        ALLOCATE (ITAB_TMP(NUMNUSR2),STAT=stat)
        IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='ITAB_TMP')
        ALLOCATE (SUB_NOD(NUMNUSR2),STAT=stat)
        IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='SUB_NOD')
        ALLOCATE (UID_NOD(NUMNUSR2),STAT=stat)
        IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='UID_NOD')
        ALLOCATE (HM_X(3,NUMNUSR2),STAT=stat)
        IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='HM_X')

        ITAB_TMP(1:NUMNUSR2) = 0
        SUB_NOD(1:NUMNUSR2) = 0
        UID_NOD(1:NUMNUSR2) = 0
        HM_X(1:3,1:NUMNUSR2) = ZERO

        CALL CPP_CNODE_READ(ITAB_TMP,HM_X,DMERGE,SUB_NOD,UID_NOD)
        N = NUMNUSR1
        DO I=1,NUMNUSR2
          N = N + 1
C---------------------------------------------------
C        SUBMODEL OFFSET
C---------------------------------------------------
          IF(SUB_NOD(I) /= 0)THEN
            IF(UID_NOD(I) == 0 .AND. LSUBMODEL(SUB_NOD(I))%UID /= 0)
     .        UID_NOD(I) = LSUBMODEL(SUB_NOD(I))%UID
          ENDIF
          ITAB(N) = ITAB_TMP(I)
C---------------------------------------------------
C        UNITS
C---------------------------------------------------
          IF(UID_NOD(I) /= UID )THEN
            UID = UID_NOD(I)
            IFLAGUNIT = 0
            DO J=1,NUNITS
              IF (UNITAB%UNIT_ID(J) == UID) THEN
                FAC_L = UNITAB%FAC_L(J)
                IFLAGUNIT = 1
                EXIT
              ENDIF
            ENDDO
            IF (UID/=0 .AND. IFLAGUNIT==0)THEN
              CALL ANCMSG(MSGID=643,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                    I1=UID,C1='/CNODE')
            ENDIF
          ENDIF
          X(1,N) = HM_X(1,I)*FAC_L
          X(2,N) = HM_X(2,I)*FAC_L
          X(3,N) = HM_X(3,I)*FAC_L
          CMERGE(I) = DMERGE(I) * FAC_L
          XMIN=MIN(XMIN,X(1,N))
          YMIN=MIN(YMIN,X(2,N))
          ZMIN=MIN(ZMIN,X(3,N))
          XMAX=MAX(XMAX,X(1,N))
          YMAX=MAX(YMAX,X(2,N))
          ZMAX=MAX(ZMAX,X(3,N))
        ENDDO
        IF(ALLOCATED(ITAB_TMP))   DEALLOCATE(ITAB_TMP)
        IF(ALLOCATED(SUB_NOD)) DEALLOCATE(SUB_NOD)
        IF(ALLOCATED(UID_NOD)) DEALLOCATE(UID_NOD)
        IF(ALLOCATED(HM_X))    DEALLOCATE(HM_X)
      END IF
      NUMNUSR = N
C     Here : NUMNUSR == NUMNOD0 !
C--------------------------------------------------
C     SPH RESERVE
C--------------------------------------------------
      X1=XMIN-FOURTH*(XMAX-XMIN)
      X2=YMIN-FOURTH*(YMAX-YMIN)
      X3=ZMIN-FOURTH*(ZMAX-ZMIN)

      CALL HM_OPTION_START('/SPH/RESERVE')
      I      =NUMNUSR
      ISPHRES=I
      DO N=1,NBPARTINLET
        CALL HM_OPTION_READ_KEY(LSUBMODEL,OPTION_ID = ID)
        CALL HM_GET_INTV('Np',KSPHRES,IS_AVAILABLE,LSUBMODEL)
c KSPHRES by proc
        DO J=1,KSPHRES*NSPMD
          I    =I+1
          ITAB(I)=IDLT_NODE_AUTO
          IDLT_NODE_AUTO=IDLT_NODE_AUTO+1
          X(1,I)=X1
          X(2,I)=X2
          X(3,I)=X3
        ENDDO
      ENDDO
C--------------------------------------------------
C     Solids TO SPH
      FIRSTNOD_SPHSOL=I+1
      DO N=1,NSPHSOL
        I=I+1
        ITAB(I)=IDLT_NODE_AUTO
        IDLT_NODE_AUTO=IDLT_NODE_AUTO+1
        X(1,I)  =ZERO
        X(2,I)  =ZERO
        X(3,I)  =ZERO
      END DO
C--------------------------------------------------
C     TABN for additional nodes
      IF(NADMESH/=0)THEN
        IF(NUMNOD-NUMNOD0>100000000)THEN
          CALL ANCMSG(MSGID=645,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO)
        ENDIF
        DO N=NUMNOD0+1,NUMNOD
          ITAB(N)=ID_LIMIT_ADMESH+N-NUMNOD0-NUMCNOD
        END DO
      END IF
C--------------------------------------------------
C     IGE: deprecated
C--------------------------------------------------

      IF(NADIGEMESH/=0)THEN
        IF(NUMNOD-NUMNODIGE0>100000000)THEN
          CALL ANCMSG(MSGID=645,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO)
        ENDIF
        DO N=NUMNODIGE0+1,NUMNOD
          ITAB(N)=ID_LIMIT_ADMESH+N-NUMNODIGE0
        END DO
      END IF
C--------------------------------------------------
      DO N=1,64*NUMELIG3D
        IDLT_NODE_AUTO=IDLT_NODE_AUTO+1
      END DO
C--------------------------------------------------
      CALL CONSTIT(ITAB,ITABM1,NUMNOD)

      CALL C_NEW_HASH(H_NODE,NUMNOD)
      DO I=1,NUMNOD
        CALL C_HASH_INSERT(H_NODE,ITAB(I),I)
      ENDDO

C--------------------------------------------------
C     X from file Ynn
C--------------------------------------------------
      IF (ISIGI==3.OR.ISIGI==4.OR.ISIGI==5) THEN
C
  120   READ(IIN4,FMT='(A)',END=199,ERR=199)LLINE
        IF(LLINE(1:33)/='/NODAL     /VECTOR    /COORDINATE')GOTO 120
        READ(IIN4,FMT='(A)',END=199,ERR=199)LLINE
C
        IF (IOUTP_FMT==2) THEN
  125     READ(IIN4,FMT='(A)',END=130,ERR=199)LLINE
          IF(LLINE(1:1)=='#')GOTO 125
          IF(LLINE(1:1)=='/')GOTO 130
          READ(LLINE,'(I8,3F16.0)')N,X1,X2,X3
          I = USRTOS(N,ITABM1)
          IF(I/=0)THEN
            X(1,I) = X1
            X(2,I) = X2
            X(3,I) = X3
          ENDIF
          GOTO 125
        ELSE
  126     READ(IIN4,FMT='(A)',END=130,ERR=199)LLINE
          IF(LLINE(1:1)=='#')GOTO 126
          IF(LLINE(1:1)=='/')GOTO 130
          READ(LLINE,FMT=FMT_I_3F) N,X1,X2,X3
          I = USRTOS(N,ITABM1)
          IF(I/=0)THEN
            X(1,I) = X1
            X(2,I) = X2
            X(3,I) = X3
          ENDIF
          GOTO 126
        ENDIF
  130   CONTINUE
  199   CONTINUE
        REWIND(IIN4)
C
      ENDIF
C--------------------------------
      J=0
      IF (IPRI > 3)THEN
        DO N=1,NUMNUSR1,50

          J=J+50
          J=MIN(J,NUMNUSR1)
          WRITE(IOUT,'(//A/A//A/)')TITRE(70),TITRE(71),TITRE(72)
          DO I=N,J
            WRITE(IOUT,'(5X,I10,8X,1P3G20.13)')
     .            ITAB(I),X(1,I),X(2,I),X(3,I)
          ENDDO
        ENDDO
C
        DO N=NUMNUSR1+1,NUMNUSR,50
          J=J+50
          J=MIN(J,NUMNUSR)
          WRITE(IOUT,'(A)')TITRE(117)
          DO I=N,J
            WRITE(IOUT,'(5X,I10,8X,1P3G20.13)')
     .           ITAB(I),X(1,I),X(2,I),X(3,I)
          ENDDO
        ENDDO
      ENDIF
C--------------------------------
      IF (IPRI > 6)THEN
        IF(NUMNOD > NUMNUSR)THEN
          DO 31 N=NUMNUSR+1,NUMNOD,50
            J=J+50
            J=MIN(J,NUMNOD)
            WRITE(IOUT,'(//A)')
     .       '            COORDINATES OF NODES FOR SPH RESERVES'
            WRITE(IOUT,'(A)')
     .       '            -------------------------------------'
            WRITE(IOUT,'(A/)')TITRE(72)
            DO 21 I=N,J
   21       WRITE(IOUT,'(5X,I10,8X,1P3G20.13)') ITAB(I),X(1,I),X(2,I),X(3,I)
   31     CONTINUE
        ENDIF
      ENDIF
C--------------------------------------------------
      RETURN
      END
