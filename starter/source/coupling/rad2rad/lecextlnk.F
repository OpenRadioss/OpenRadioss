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
Chd|  LECEXTLNK                     source/coupling/rad2rad/lecextlnk.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE LECEXTLNK(IEXTER,IPART,LSUBMODEL)
      USE MESSAGE_MOD
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE R2R_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD   
C-------------------------------------
C     Read radioss link for external process coupling.
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IEXTER(NR2R,*),IPART(LIPART1,*)
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, NUSER, IGR, IGRS, STAT,I,COMPT,SET
      CHARACTER MESS*40,KEY*ncharkey,TITR*nchartitle
      INTEGER J,ADD,K
      INTEGER FLAG_OK,NUM_LINK,FOUND,NEL,ID
      INTEGER, DIMENSION(:), ALLOCATABLE :: ID_SUB_TEMP
      LOGICAL IS_AVAILABLE      
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER GRFIND
C
      DATA MESS/' ** ERROR EXTERNAL COUPLING DEFINITION '/
      WRITE(IOUT,1000)
      IF (NR2RLNK.GT.0) WRITE(IOUT,1200)
C      
C      IEXTER(1,M) ............... Id du GRNOD du Link
C      IEXTER(2,M) ............... User Id du Link
C      IEXTER(3,M) ............... Id du 1er domaine
C      IEXTER(4,M) ............... Id du 2e domaine
C      IEXTER(5,M) ............... Type du Link (4 ou 5)
C        -> pour fulldomain IEXTER(4,M) = 0
C        -> pour link classique IEXTER(4,M) = IEXTER(4,M) = -1
C        
C
C      ISUBDOM(1,N) .............. Id du subdomaine
C      ISUBDOM(2,N) .............. Nb de parts du subdomaine
C      ISUBDOM(3,N) .............. Adresse des parts du subdomaine dans ISUBDOM_PART
C      ISUBDOM(4,N) .............. Id du link principal du subdomaine
C      ISUBDOM(5,N) .............. Nb de noeuds du subdomaine
C      ISUBDOM(6,N) .............. Nb d'elements du subdomaine
C      ISUBDOM(7,N) .............. ROOTLEN
C
C      ISUBDOM_PART() ............ Liste des parts du subdomaine  
C-----------------------------------------------      
C
      IF (NR2RLNK > 0) THEN
C------------------------------------------------------------------
C------------------------LECTURE DES LINKS------------------------- 
C------------------------------------------------------------------
C
        IS_AVAILABLE = .FALSE.
        CALL HM_OPTION_START('/EXTERN/LINK')
C        
        DO I=1,NR2RLNK
C
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                            OPTION_ID = NUSER,
     .                            OPTION_TITR = TITR)
C
          CALL HM_GET_INTV('grnod_id',IGR,IS_AVAILABLE,LSUBMODEL) 
C-------- CHECK NODE GROUP
C         IGRS = GRFIND (IGR,IGRN,MESS)
C         IF (IGRS.EQ.0) IERR=IERR+1
          IEXTER(1,I) = IGR
          IEXTER(2,I) = NUSER
	  IEXTER(3,I) = -1
	  IEXTER(4,I) = -1
C-----    IMPRESSIONS
          WRITE(IOUT,1100) NUSER,IGR	  
        ENDDO
      
      ENDIF
      
      IF (NSUBDOM.GT.0) THEN      
C------------------------------------------------------------------
C---------------------LECTURE DES SUBDOMAINES---------------------- 
C------------------------------------------------------------------
C
        IS_AVAILABLE = .FALSE.
        CALL HM_OPTION_START('/SUBDOMAIN')
C      
        ALLOCATE (ISUBDOM_PART(NB_PART_SUB),STAT=stat)     
        ALLOCATE (ID_SUB_TEMP(NB_PART_SUB),STAT=stat)
C
        SET = 0
        R2R_FLAG_ERR_OFF = 0
C
        DO I=1,NSUBDOM
C
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                            OPTION_ID = NUSER,
     .                            OPTION_TITR = TITR,
     .                            KEYWORD2 = KEY)
C
          CALL HM_GET_INTV('idsmax',NEL,IS_AVAILABLE,LSUBMODEL) 
C
	  IF (I.GT.1) SET = SET+ISUBDOM(1,I-1)
	  ISUBDOM(3,I)=SET
          COMPT = 0
C
          DO J=1,NEL
C
            CALL HM_GET_INT_ARRAY_INDEX('ids',ID,J,IS_AVAILABLE,LSUBMODEL)
C
	    IF (ID.GT.0) THEN	      
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
            ELSEIF (ID.EQ.-1) THEN
                FLAG_OK = 1
                R2R_FLAG_ERR_OFF = 1
            ENDIF
	    IF (FLAG_OK==0) THEN
              CALL ANCMSG(MSGID=783,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    I1=NUSER,
     .                    C1=TITR,
     .                    I2=ID)
	    ENDIF
	  END DO
C----- STOCKAGE DES INFOS DU SUBDOMAINE
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

      IF (FLG_SWALE.EQ.1) THEN
C------------------------------------------------------------------
C-------------SWITCH OF FULL/SUB PARTS FOR ALE--------------------- 
C------------------------------------------------------------------
        NB_PART_SUB = NPART - ISUBDOM(1,1)
        DO I=1,ISUBDOM(1,1)
          ID_SUB_TEMP(I) = ISUBDOM_PART(I)
        END DO
C
        DEALLOCATE (ISUBDOM_PART)
        ALLOCATE (ISUBDOM_PART(NB_PART_SUB),STAT=stat)
C
        COMPT = 0
        DO I=1,NPART
          FOUND = 0
          DO J=1,ISUBDOM(1,1)
            IF (ID_SUB_TEMP(J).EQ.I) FOUND = 1 
          END DO
          IF (FOUND.EQ.1) CYCLE
          COMPT = COMPT + 1
          ISUBDOM_PART(COMPT) = I
        END DO
        ISUBDOM(1,1) = COMPT
C       
      ENDIF

      DEALLOCATE (ID_SUB_TEMP)
      
      ENDIF
      
C------------------------------------------------------------------
      IREC=IREC+1
 800  CONTINUE
      RETURN
C------------------------------------------------------------------
 999  CALL FREERR(3)
      RETURN
C------------------------------------------------------------------
 1000 FORMAT(
     . //'      MULTIDOMAINS COUPLING DEFINITIONS '/
     . '      --------------------------------- '/)
 1100 FORMAT(/10X,'EXTERNAL LINK IDENTIFIER . . . .',I10,
     .       /10X,'RADIOSS NODE GROUP ID . . . . . ',I10)
 1300 FORMAT(/10X,'SUBDOMAIN IDENTIFIER . . . . . .',I10,
     .       /10X,'NUMBER OF PARTS . . . . . . . . ',I10)    
 1301 FORMAT( 10X,'LIST OF PARTS : ')
 1302 FORMAT(  9X,10I9)  
 1200 FORMAT(' ** INFO : DATA RELATED TO EXTERNAL',
     .       ' COUPLING WILL BE CHECKED IN RADIOSS ENGINE.') 
      END
