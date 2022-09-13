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
Chd|  HM_READ_SPRING                source/elements/reader/hm_read_spring.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ANODSET                       source/output/analyse/analyse_node.c
Chd|        FREERR                        source/starter/freform.F      
Chd|        VDOUBLE                       source/system/sysfus.F        
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_SPRING(IXR   ,ITAB ,ITABM1,IPART,IPARTR,
     .                  IGEO  ,IXR_KJ  ,LSUBMODEL,ISKN,R_SKEW,IPM)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ /SPRING ELEMENTS USING HM_READER
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME            DESCRIPTION                         
C
C     IXR             SPRING ELEM ARRAY : CONNECTIVITY, ID, PID
C     ITAB            USER ID OF NODES         
C     ITABM1          REVERSE TAB ITAB
C     IPART           PART ARRAY      
C     IPARTR          INTERNAL PART ID OF A GIVEN SPRING ELEMENT 
C     IGEO            PROP ARRAY (INTEGER)
C     IXR_KJ          KJOINT ADDITIONAL CONNECTIVITY 
C     LSUBMODEL       SUBMODEL STRUCTURE     
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
C----------------------------------------------------------
C     LECTURE ELEMENT RESSORT
C     VERSION NUMEROTATION DES NOEUDS LIBRE/MARS 90/DIM
C----------------------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "analyse_name.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "scr03_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com10_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "submod_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      INTEGER,INTENT(IN)::ITAB(*)
      INTEGER,INTENT(IN)::ITABM1(*)
      INTEGER,INTENT(IN)::IPART(LIPART1,*)
      INTEGER,INTENT(IN)::IGEO(NPROPGI,*)
      INTEGER,INTENT(IN)::ISKN(LISKN,*)
      INTEGER,INTENT(IN)::IPM(NPROPMI,*)
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C OUTPUT ARGUMENTS
      INTEGER,INTENT(OUT)::IXR(NIXR,*)
      INTEGER,INTENT(OUT)::IXR_KJ(5,*)
      INTEGER,INTENT(OUT)::IPARTR(*)
      INTEGER,INTENT(OUT)::R_SKEW(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, I1, I2,PID,N,ID,IDS,J,IPID,JC,STAT,IMID,IGTYP,MID
      INTEGER FLAG_FMT,FLAG_FMT_TMP,IFIX_TMP
      INTEGER FLAG_KJ(NUMELR),IKJ_TMP(3,NUMELR),NUMEL_KJ,CPT,
     .        INDEX_PART,OFF_DEF(NSUBMOD),OFF_NOD(NSUBMOD),OFF_EL(NSUBMOD),
     .        OFF_PART(NSUBMOD)
      CHARACTER MESS*40, MESS2*40, CHAR_MAT*11, CHAR_SKEW*11
      my_real
     .   BID 
      INTEGER, DIMENSION(:), ALLOCATABLE :: SUB_SPRING,SKEWID
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NINTRN
      INTEGER USR2SYS
      DATA MESS /'3D SPRING ELEMENTS DEFINITION           '/
      DATA MESS2/'3D SPRING ELEMENTS SELECTION FOR TH PLOT'/
C=======================================================================
C--------------------------------------------------
C      HM OFFSETS IDs
C--------------------------------------------------
      DO I=1,NSUBMOD
        OFF_DEF(I) = LSUBMODEL(I)%OFF_DEF
        OFF_NOD(I) = LSUBMODEL(I)%OFF_NOD
        OFF_EL(I) = LSUBMODEL(I)%OFF_EL
        OFF_PART(I) = LSUBMODEL(I)%OFF_PART
      ENDDO
C--------------------------------------------------
C      ALLOCS & INITS
C--------------------------------------------------
      ALLOCATE (SUB_SPRING(NUMELR),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='SUB_SPRING') 
      SUB_SPRING(1:NUMELR) = 0
      ALLOCATE (SKEWID(NUMELR),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='SKEWID') 
      SKEWID(1:NUMELR) = 0
      INDEX_PART = 1
C--------------------------------------------------
C      READING SPRING INPUTS IN HM STRUCTURE
C-------------------------------------------------- 
      CALL CPP_SPRING_READ(IXR,NIXR,IXR_KJ,5,IPARTR,SUB_SPRING,SKEWID)
C--------------------------------------------------
C      FILL OTHER STRUCTURES + CHECKS
C--------------------------------------------------
        I=0
	NUMEL_KJ = 0
C	
        DO N=1,NUMELR
          I = I + 1
C--------------------------------------------------
C      SUBMODEL OFFSET
C--------------------------------------------------
          IF(SUB_SPRING(I) /= 0)THEN
            IF(IPARTR(I) /= 0) IPARTR(I) = IPARTR(I) + OFF_PART(SUB_SPRING(I)) 
            IF(IXR(NIXR,I) /= 0) IXR(NIXR,I) = IXR(NIXR,I) + OFF_EL(SUB_SPRING(I))
            DO J=2,4
              IF(IXR(J,I) /= 0) IXR(J,I) = IXR(J,I) + OFF_NOD(SUB_SPRING(I))
            ENDDO
            DO J=1,3
              IF(IXR_KJ(J,I) /= 0) IXR_KJ(J,I) = IXR_KJ(J,I) + OFF_NOD(SUB_SPRING(I))
            ENDDO
            IF(SKEWID(I) > 0) SKEWID(I)=SKEWID(I)+OFF_DEF(SUB_SPRING(I))
          ENDIF
C--------------------------------------------------
C      INTERNAL PART ID
C--------------------------------------------------
          IF( IPART(4,INDEX_PART) /= IPARTR(I) )THEN  
            DO J=1,NPART                            
              IF(IPART(4,J)== IPARTR(I) ) INDEX_PART = J 
            ENDDO  
          ENDIF
          IF( IPART(4,INDEX_PART) /= IPARTR(I) ) THEN
            CALL ANCMSG(MSGID=402,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  C1="SPRING",
     .                  I1=IPARTR(I),
     .                  I2=IPARTR(I),
     .                  PRMOD=MSG_CUMU)
          ENDIF   
          IPID=IPART(2,INDEX_PART)   
          IMID=IPART(1,INDEX_PART)  
          IGTYP=IGEO(11,IPID)
          IXR(5,I)=0
C
          IF(IGTYP == 23) IXR(5,I)=IMID
          IPARTR(I) = INDEX_PART
C-------------------------------------------------- 
c
          FLAG_KJ(I) = 0
          DO J=1,3
            IF (IXR_KJ(J,I)/=0) FLAG_KJ(I) = FLAG_KJ(I) + 1
          END DO   
c	    
          IF (IXR(NIXR,I)>ID_LIMIT) THEN
            CALL ANCMSG(MSGID=509,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                  I1=IXR(NIXR,I),C1=LINE,C2='/SPRING')
          ENDIF
          IXR(1,I)=IPID   
          IXR(2,I)=USR2SYS(IXR(2,I),ITABM1,MESS,IXR(NIXR,I))
          IXR(3,I)=USR2SYS(IXR(3,I),ITABM1,MESS,IXR(NIXR,I))
          CALL ANODSET(IXR(2,I), CHECK_SPRING)
          CALL ANODSET(IXR(3,I), CHECK_SPRING)
          IF(IXR(4,I)/=0) THEN
             IXR(4,I)=USR2SYS(IXR(4,I),ITABM1,MESS,IXR(NIXR,I))
             CALL ANODSET(IXR(4,I), CHECK_USED)
          ENDIF
C         Noeuds additionels pour joints	  
	  IF (FLAG_KJ(I)>0) THEN
            DO J=1,3
             IF(IXR_KJ(J,I)/=0) THEN
                IXR_KJ(J,I)=USR2SYS(IXR_KJ(J,I),ITABM1,MESS,IXR(NIXR,I))
                CALL ANODSET(IXR_KJ(J,I), CHECK_USED)
             ENDIF
            END DO  	  		
          ENDIF
C         Skews per element - PROP type23 and mat law 108 or PROP type8 - only
          IF (SKEWID(I) > 0) THEN  
            DO J = 0,NUMSKW+MIN(1,NSPCOND)*NUMSPH+NSUBMOD
              IF (SKEWID(I) == ISKN(4,J+1)) THEN
                R_SKEW(I) = J+1
                GO TO 500 
              ENDIF
            ENDDO
            CALL ANCMSG(MSGID=137,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .            C1='SPRING',
     .            C2='SPRING',
     .            I1=IXR(NIXR,I),I2=SKEWID(I))
500         CONTINUE
          ENDIF
        ENDDO
C
      IF(ALLOCATED(SUB_SPRING)) DEALLOCATE(SUB_SPRING)
C-----------
      CALL ANCMSG(MSGID=402,                 
     .            MSGTYPE=MSGERROR,         
     .            ANMODE=ANINFO_BLIND_1,    
     .            PRMOD=MSG_PRINT) 
C-------------------------------------
C Recherche des ID doubles
C-------------------------------------
      IDS = 79
      I = 0
      J = 0
      CALL VDOUBLE(IXR(NIXR,1),NIXR,NUMELR,MESS,0,BID)
      IDS = 35
C
      I1=1
      I2=MIN0(50,NUMELR)
C
   90 WRITE (IOUT,300)
      DO 100 I=I1,I2
        PID = IGEO(1,IXR(1,I))
C
        IF (IXR(5,I) > 0) THEN
          MID = IPM(1,IXR(5,I))
          WRITE (CHAR_MAT,'(I10,1X)') MID
        ELSE
          CHAR_MAT=''
        ENDIF
C
        IF (SKEWID(I) > 0) THEN
          WRITE (CHAR_SKEW,'(I10)') SKEWID(I)
        ELSE
          CHAR_SKEW=''
        ENDIF
C
	IF (IGEO(11,IXR(1,I))==45) NUMEL_KJ = NUMEL_KJ + 1
        IF(IXR(4,I)==0) THEN
          WRITE (IOUT,'(5(I10,1X),44X,A,A)') I,IXR(NIXR,I),PID,
     .                  ITAB(IXR(2,I)),ITAB(IXR(3,I)),CHAR_MAT,CHAR_SKEW
        ELSEIF (FLAG_KJ(I)>0) THEN
          IF (FLAG_KJ(I) == 1) THEN
            WRITE (IOUT,'(7(I10,1X),A,A)') I,IXR(NIXR,I),PID,
     .                   ITAB(IXR(2,I)),ITAB(IXR(3,I)),ITAB(IXR(4,I)),
     .                  (ITAB(IXR_KJ(J,I)),J=1,FLAG_KJ(I)),CHAR_MAT,CHAR_SKEW  	
          ELSEIF  (FLAG_KJ(I) == 2) THEN
            WRITE (IOUT,'(8(I10,1X),A,A)') I,IXR(NIXR,I),PID,
     .                   ITAB(IXR(2,I)),ITAB(IXR(3,I)),ITAB(IXR(4,I)),
     .                  (ITAB(IXR_KJ(J,I)),J=1,FLAG_KJ(I)),CHAR_MAT,CHAR_SKEW 
          ELSEIF  (FLAG_KJ(I) == 3) THEN
            WRITE (IOUT,'(9(I10,1X),A,A)') I,IXR(NIXR,I),PID,
     .                   ITAB(IXR(2,I)),ITAB(IXR(3,I)),ITAB(IXR(4,I)),
     .                  (ITAB(IXR_KJ(J,I)),J=1,FLAG_KJ(I)),CHAR_MAT,CHAR_SKEW 
          ENDIF
	ELSE
          WRITE (IOUT,'(6(I10,1X),33X,A,A)') I,IXR(NIXR,I),PID,
     .           ITAB(IXR(2,I)),ITAB(IXR(3,I)),ITAB(IXR(4,I)),CHAR_MAT,CHAR_SKEW
        ENDIF
C
  100 CONTINUE
      IF(I2==NUMELR)GOTO 200
      I1=I1+50
      I2=MIN0(I2+50,NUMELR)
      GOTO 90
C
C
 200  CONTINUE
C--------------------------------------------------
C Reorganisation du tableau additionel pour kjoints
C--------------------------------------------------

      IF (NUMEL_KJ>0) THEN
        DO I=1,NUMELR
	  DO J=1,3
	    IKJ_TMP(J,I)=IXR_KJ(J,I)
	  END DO
	END DO
	CPT = 0
	IXR_KJ(1,NUMELR+1)=NUMEL_KJ
        DO I=1,NUMELR
	  IF (IGEO(11,IXR(1,I))==45) THEN
	    CPT = CPT+1
	    DO J=1,3
	      IXR_KJ(J,CPT)=IKJ_TMP(J,I)
	    END DO
	    IXR_KJ(4,CPT)=IXR(NIXR,I)
	    IXR_KJ(5,CPT)=I
	  ENDIF  
	END DO	       
      ENDIF
C
C----
      RETURN
  300 FORMAT(/' SPRING ELEMENTS'/
     +        ' ---------------'/
     + '    LOC-EL     GLO-EL       GEOM      NODE1      NODE2'
     + '    (NODE3)                                    (MAT_ID)     (SKEW)')
  310 FORMAT(' SPRING ELEMENT TH SELECTION'/
     +       ' ---------------------------'/)   
  999 CALL FREERR(3)
      RETURN
      END
