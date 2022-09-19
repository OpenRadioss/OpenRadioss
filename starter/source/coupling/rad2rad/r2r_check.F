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
Chd|  R2R_CHECK                     source/coupling/rad2rad/r2r_check.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        GRFIND                        source/starter/freform.F      
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        RESTMOD                       share/modules1/restart_mod.F  
Chd|====================================================================
      SUBROUTINE R2R_CHECK(IEXTER ,IGRNOD   ,IPARTL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE RESTMOD
      USE R2R_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
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
      INTEGER IEXTER(NR2R,*),IPARTL(LIPART1,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER STAT,I,IGR,IGRS,N,K,ADD
      CHARACTER MESS*40
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAGPART
C-----------------------------------------------
      TYPE (GROUP_)  ,DIMENSION(NGRNOD)  :: IGRNOD
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER GRFIND
C
      DATA MESS/' ** ERROR EXTERNAL COUPLING DEFINITION '/      	

C----- Check if Id of subdomains-----------------------------------C

        IF (NSUBDOM>0) THEN
         DO N=1,NSUBDOM-1
           IGR = ISUBDOM(2,N)
	       DO I=N+1,NSUBDOM
	         IF (IGR==ISUBDOM(2,I)) THEN
                CALL ANCMSG(MSGID=792,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    I1=IGR)
                IERR=IERR+1	
             ENDIF
           END DO
         END DO
        ENDIF

C----- Check of parts of subdomains-------------------------------C

        ALLOCATE(TAGPART(NPART))
        TAGPART = 0
	
        DO N=1,NSUBDOM
        ADD = ISUBDOM(3,N)
        DO K=1,NPART
            DO I=1,ISUBDOM(1,N)
              IF(K == ISUBDOM_PART(I+ADD))THEN
                  TAGPART(K)=TAGPART(K)+1
		    IF (TAGPART(K)>1) THEN
                   CALL ANCMSG(MSGID=827,
     .                         MSGTYPE=MSGERROR,
     .                         ANMODE=ANINFO,I1=ISUBDOM(1,N),
     .                         I2=IPART(LIPART1*(K-1)+4))
		      IERR=IERR+1
		    ENDIF  	     		    	  		
              ENDIF
            ENDDO	
	  END DO
	END DO

C----- Check of common nodes between interfaces---------------------C

!        DO N=1,NR2RLNK-1
!	   DO I=N+1,NR2RLNK
C                 CALL ANSTCKI(22)	   
C                 CALL ANSTCKI(9)
C                 CALL ANSTCKI(8)
C    	         CALL ANCERR(792,ANINFO)
C		 IERR=IERR+1	     
!	   END DO
!	END DO

C----- Check of multidomains node groups-----------------------------C

        DO N=1,NR2RLNK
           IGR = IEXTER(1,N)
           IGRS = GRFIND(IGR,IGRNOD,MESS)
           IEXTER(1,N) = IGRS	
           IF (IGRS==0) IERR=IERR+1
	END DO

C--------------------------------------------------------------------C

      RETURN
      END
