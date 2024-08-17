Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
      !||    lecextlnk                ../starter/source/coupling/rad2rad/lecextlnk.F
      !||--- called by ------------------------------------------------------
      !||    lectur                   ../starter/source/starter/lectur.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                   ../starter/source/output/message/message.F
      !||    grfind                   ../starter/source/starter/freform.F
      !||    hm_get_int_array_index   ../starter/source/devtools/hm_reader/hm_get_int_array_index.F
      !||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
      !||    hm_option_read_key       ../starter/source/devtools/hm_reader/hm_option_read_key.F
      !||    hm_option_start          ../starter/source/devtools/hm_reader/hm_option_start.F
      !||--- uses       -----------------------------------------------------
      !||    hm_option_read_mod       ../starter/share/modules1/hm_option_read_mod.F
      !||    message_mod              ../starter/share/message_module/message_mod.F
      !||    r2r_mod                  ../starter/share/modules1/r2r_mod.F
      !||    reader_old_mod           ../starter/share/modules1/reader_old_mod.F90
      !||    submodel_mod             ../starter/share/modules1/submodel_mod.F
      !||====================================================================
      SUBROUTINE LECEXTLNK(IEXTER,IPART,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE MESSAGE_MOD
        USE R2R_MOD
        USE SUBMODEL_MOD
        USE HM_OPTION_READ_MOD
        USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE, NCHARKEY
        USE READER_OLD_MOD , ONLY : IREC
C-------------------------------------
C     Read radioss link for external process coupling.
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IEXTER(NR2R,*),IPART(LIPART1,*)
        TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER N, NUSER, IGR, IGRS, STAT,I,COMPT,SET
        CHARACTER MESS*40
        CHARACTER(LEN=NCHARKEY)::KEY
        CHARACTER(LEN=NCHARTITLE)::TITR
        INTEGER J,ADD,K
        INTEGER FLAG_OK,NUM_LINK,FOUND,NEL,ID,NELN
        INTEGER, DIMENSION(:), ALLOCATABLE :: ID_SUB_TEMP
        LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
        INTEGER GRFIND
C
        DATA MESS/' ** ERROR EXTERNAL COUPLING DEFINITION '/
        WRITE(IOUT,1000)
        IF (NR2RLNK>0) WRITE(IOUT,1200)
C
C      IEXTER(1,M) ............... Id of GRNOD of the Link
C      IEXTER(2,M) ............... Link ser Id
C      IEXTER(3,M) ............... Id of 1st domain
C      IEXTER(4,M) ............... Id of 2nd domain
C      IEXTER(5,M) ............... Type du Link (4 ou 5)
C        -> pour fulldomain IEXTER(4,M) = 0
C        -> pour link classique IEXTER(4,M) = IEXTER(4,M) = -1
C
C
C      ISUBDOM(1,N) .............. Id of subdomain
C      ISUBDOM(2,N) .............. Nb of parts of the subdomain
C      ISUBDOM(3,N) .............. Adress of the parts of the subdomain in ISUBDOM_PART
C      ISUBDOM(4,N) .............. Id of principal link of subdomain
C      ISUBDOM(5,N) .............. Nb of nodes of subdomain
C      ISUBDOM(6,N) .............. Nb of elements of subdomain
C      ISUBDOM(7,N) .............. ROOTLEN
C
C      ISUBDOM_PART() ............ List of parts of the subdomain
C-----------------------------------------------
C
        IF (NR2RLNK > 0) THEN
C------------------------------------------------------------------
C------------------------READING OF LINKS--------------------------
C------------------------------------------------------------------
C
          IS_AVAILABLE = .FALSE.
          CALL HM_OPTION_START('/EXTERN/LINK')

          DO I=1,NR2RLNK
            CALL HM_OPTION_READ_KEY(LSUBMODEL,OPTION_ID = NUSER,OPTION_TITR = TITR)
            CALL HM_GET_INTV('grnod_id',IGR,IS_AVAILABLE,LSUBMODEL)
C-------- CHECK NODE GROUP
            IEXTER(1,I) = IGR
            IEXTER(2,I) = NUSER
            IEXTER(3,I) = -1
            IEXTER(4,I) = -1
C-----    PRINTOUT
            WRITE(IOUT,1100) NUSER,IGR
          ENDDO

        ENDIF

        IF (NSUBDOM>0) THEN
C------------------------------------------------------------------
C------------------------READING OF SUBDOMAINS---------------------
C------------------------------------------------------------------
C
          IS_AVAILABLE = .FALSE.
          CALL HM_OPTION_START('/SUBDOMAIN')
          ALLOCATE (ISUBDOM_PART(NB_PART_SUB),STAT=stat)
          ALLOCATE (ID_SUB_TEMP(NB_PART_SUB),STAT=stat)
          SET = 0
          R2R_FLAG_ERR_OFF = 0

          DO I=1,NSUBDOM
            CALL HM_OPTION_READ_KEY(LSUBMODEL,OPTION_ID = NUSER,OPTION_TITR = TITR,KEYWORD2 = KEY)
            CALL HM_GET_INTV('idsmax',NEL,IS_AVAILABLE,LSUBMODEL)
            CALL HM_GET_INTV('negativeIdsmax',NELN,IS_AVAILABLE,LSUBMODEL)
            IF (I>1) SET = SET+ISUBDOM(1,I-1)
            ISUBDOM(3,I)=SET
            COMPT = 0
            DO J=1,NEL
              CALL HM_GET_INT_ARRAY_INDEX('ids',ID,J,IS_AVAILABLE,LSUBMODEL)
              COMPT=COMPT+1
              ID_SUB_TEMP(COMPT+SET)=ID
C----- CHECK PART ID
              FLAG_OK = 0
              DO K=1,NPART
                IF(ID==IPART(4,K)) THEN
                  FLAG_OK=1
                  ISUBDOM_PART(COMPT+SET)=K
                END IF
              END DO
              IF (FLAG_OK==0) THEN
                CALL ANCMSG(MSGID=783,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=NUSER,C1=TITR,I2=ID)
              ENDIF
            END DO
C--       if ID < 0 in list of part -> error message for size of interface is deactivated
            IF (NELN > 0) R2R_FLAG_ERR_OFF = 1
C
C----- STORAGE OF DATA FOR SUBDOMAINS
            ISUBDOM(1,I) = COMPT
            ISUBDOM(2,I) = NUSER
C
          ENDDO

          DO I=1,NSUBDOM
            WRITE(IOUT,1300) ISUBDOM(2,I),ISUBDOM(1,I)
            WRITE(IOUT,1301)
            ADD = ISUBDOM(3,I)
            WRITE(IOUT,1302) (ID_SUB_TEMP(J+ADD),J=1,ISUBDOM(1,I))
          END DO

          IF (FLG_SWALE==1) THEN
C------------------------------------------------------------------
C-------------SWITCH OF FULL/SUB PARTS FOR ALE---------------------
C------------------------------------------------------------------
            NB_PART_SUB = NPART - ISUBDOM(1,1)
            DO I=1,ISUBDOM(1,1)
              ID_SUB_TEMP(I) = ISUBDOM_PART(I)
            END DO

            DEALLOCATE (ISUBDOM_PART)
            ALLOCATE (ISUBDOM_PART(NB_PART_SUB),STAT=stat)
            COMPT = 0
            DO I=1,NPART
              FOUND = 0
              DO J=1,ISUBDOM(1,1)
                IF (ID_SUB_TEMP(J)==I) FOUND = 1
              END DO
              IF (FOUND==1) CYCLE
              COMPT = COMPT + 1
              ISUBDOM_PART(COMPT) = I
            END DO
            ISUBDOM(1,1) = COMPT

          ENDIF

          DEALLOCATE (ID_SUB_TEMP)

        ENDIF

C------------------------------------------------------------------
        IREC=IREC+1

        RETURN
C------------------------------------------------------------------
 1000   FORMAT(
     .   //'      MULTIDOMAINS COUPLING DEFINITIONS '/
     .   '      --------------------------------- '/)
 1100   FORMAT(/10X,'EXTERNAL LINK IDENTIFIER . . . .',I10,
     .         /10X,'RADIOSS NODE GROUP ID . . . . . ',I10)
 1300   FORMAT(/10X,'SUBDOMAIN IDENTIFIER . . . . . .',I10,
     .         /10X,'NUMBER OF PARTS . . . . . . . . ',I10)
 1301   FORMAT( 10X,'LIST OF PARTS : ')
 1302   FORMAT(  9X,10I9)
 1200   FORMAT(' ** INFO : DATA RELATED TO EXTERNAL',
     .         ' COUPLING WILL BE CHECKED IN RADIOSS ENGINE.')
      END SUBROUTINE LECEXTLNK
