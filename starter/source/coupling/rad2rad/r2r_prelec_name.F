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
Chd|  R2R_PRELEC_NAME               source/coupling/rad2rad/r2r_prelec_name.F
Chd|-- called by -----------
Chd|        R2R_FORK                      source/coupling/rad2rad/r2r_fork.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        RESTMOD                       share/modules1/restart_mod.F  
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE R2R_PRELEC_NAME(KDOM,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE RESTMOD
      USE R2R_MOD
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
      INTEGER KDOM
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NUSER,N,RLEN,STAT,DEB,K,FLAG_FMT,ID,NEL
      CHARACTER KEY*ncharkey,LOCNAME*nchartitle,TITR*nchartitle
      LOGICAL IS_AVAILABLE
C=======================================================================  	        
C
      IS_AVAILABLE = .FALSE.
      CALL HM_OPTION_START('/SUBDOMAIN')
      NB_PART_SUB = 0
C
      IF (IPID.EQ.0) THEN
C--------------------------------------------------------------------C
C------Prereading of domain names --> Child process -----------------C
C--------------------------------------------------------------------C
      	     
        FLAG_FMT = 0

        DO N=1,NSUBDOM
C       
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                            OPTION_ID = ID,
     .                            OPTION_TITR = TITR,
     .                            KEYWORD2 = KEY) 
C---
          IF(N == KDOM)THEN
            DOM_NAME = ''//TRIM(TITR)
	  ENDIF
C-----    Computation of total number of parts
          CALL HM_GET_INTV('idsmax',NEL,IS_AVAILABLE,LSUBMODEL)
          NB_PART_SUB = NB_PART_SUB + NEL
        END DO
      
      ELSE
C--------------------------------------------------------------------C
C------Prereading of domain names --> father process ----------------C
C--------------------------------------------------------------------C
      	     
        N = 0
        DEB = 1

C      DO WHILE(N < NSUBDOM)
        DO N=1,NSUBDOM
C---
C       
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                            OPTION_ID = ID,
     .                            OPTION_TITR = TITR,
     .                            KEYWORD2 = KEY)

	  LOCNAME = ''//TRIM(TITR)
	  RLEN = LEN_TRIM(LOCNAME)
	  DOM_NAME(DEB:DEB+RLEN-1)=LOCNAME(1:RLEN)	
	  ISUBDOM(7,N)=RLEN
	  ISUBDOM(8,N)=DEB
	  DEB=DEB+RLEN	    	    
C-----    Computation of total number of parts
          CALL HM_GET_INTV('idsmax',NEL,IS_AVAILABLE,LSUBMODEL)
          NB_PART_SUB = NB_PART_SUB + NEL
C 
        END DO	  

C------Check of domain names ----------------C
       
        DO N=1,NSUBDOM
          DO K=N+1,NSUBDOM
	    IF (ISUBDOM(7,N)==ISUBDOM(7,K)) THEN
	      RLEN = ISUBDOM(7,N)
	      KEY(1:RLEN)=DOM_NAME(ISUBDOM(8,N):ISUBDOM(8,N)+RLEN-1)
	      TITR(1:RLEN)=DOM_NAME(ISUBDOM(8,K):ISUBDOM(8,K)+RLEN-1)
	      IF (KEY(1:RLEN) == TITR(1:RLEN)) THEN
                CALL ANCMSG(MSGID=826,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,I1=ISUBDOM(1,N),
     .                    C1=KEY(1:RLEN))
		 IERR=IERR+1
              ENDIF
	    ENDIF
	  ENDDO	 
        ENDDO
      
      ENDIF
      
      RETURN 
            
C------------------------------------------------------------------
 999  CALL FREERR(3)
      RETURN          
      
      END
      
