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
Chd|  HM_READ_TRIA                  source/elements/reader/hm_read_tria.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ANODSET                       source/output/analyse/analyse_node.c
Chd|        VDOUBLE                       source/system/sysfus.F        
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_TRIA(IXTG  ,ITAB   ,ITABM1 ,IPART  ,IPARTTG ,
     .                  PM    ,GEO    ,ICNOD  ,IGEO   ,IPM     ,
     .                  UNITAB ,LSUBMODEL)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ /TRIA ELEMENTS USING HM_READER
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME            DESCRIPTION                         
C
C     IXTG            /TRIA ARRAY : CONNECTIVITY, ID, MID PID
C     ITAB            USER ID OF NODES         
C     ITABM1          REVERSE TAB ITAB
C     IPART           PART ARRAY      
C     IPARTTG         INTERNAL PART ID OF A GIVEN TRIA (INTERNAL ID)   
C     PM              MATERIAL ARRAY     
C     GEO             PROP ARRAY (REAL)  
C     ICNOD           FLAG FOR TRIA WITH ISH3N = 31  
C     IGEO            PROP ARRAY (INTEGER)   
C     IPM             MATERIAL ARRAY (INTEGER)   
C     UNITAB          UNIT ARRAY 
C     LSUBMODEL       SUBMODEL STRUCTURE     
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
C--------------------------------------------------------
C     LECTURE DES ELEMENTS 2D TRIANGULAIRES
C--------------------------------------------------------
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
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "remesh_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER,INTENT(IN)::ITAB(*)
      INTEGER,INTENT(IN)::ITABM1(*)
      INTEGER,INTENT(IN)::IPART(LIPART1,*)
      INTEGER,INTENT(IN)::IGEO(NPROPGI,NUMGEO)
      INTEGER,INTENT(IN)::IPM(NPROPMI,*)
      my_real,
     .  INTENT(IN)::GEO(NPROPG,*)
      my_real,
     .  INTENT(IN)::PM(NPROPM,*)
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C OUTPUT ARGUMENTS
      INTEGER,INTENT(OUT)::IXTG(NIXTG,*)
      INTEGER,INTENT(OUT)::IPARTTG(*)
      INTEGER,INTENT(OUT)::ICNOD(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   BID,FAC_L
      INTEGER I, J, I1, I2, ID,IDS,IPID,MT,N,MID,PID,UID,JC,STAT,IPARTTG_TMP
      INTEGER INDEX_PART
      CHARACTER*40 MESS
      DATA MESS /'2D TRIANGULAR ELEMENT DEFINITION '/
      INTEGER ISH3N,KK,IFLAGUNIT
      INTEGER, DIMENSION(:), ALLOCATABLE :: SUB_TRIA,UID_TRIA,TMP_IPARTTG
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: TMP_IXTG
C-----------------------------------------------
C   FUNCTION
C-----------------------------------------------
      INTEGER USR2SYS
      INTEGER NINTRN
C=======================================================================
C--------------------------------------------------
C      ALLOCS & INITS
c      use NUMELTG0 IN PLACE OF NUMELTG ( NBADMESH routine is modifying NUMELTG )
C--------------------------------------------------
      ALLOCATE (SUB_TRIA(NUMELTG0),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='SUB_TRIA') 
      ALLOCATE (UID_TRIA(NUMELTG0),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='UID_TRIA')
      ALLOCATE (TMP_IXTG(NIXTG,NUMELTG0),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='TMP_IXTG') 
      ALLOCATE (TMP_IPARTTG(NUMELTG0),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='TMP_IPARTTG')
      SUB_TRIA(1:NUMELTG0) = 0
      UID_TRIA(1:NUMELTG0) = 0
      TMP_IXTG(1:NIXTG,1:NUMELTG0) = 0
      TMP_IPARTTG(1:NUMELTG0) = 0
      INDEX_PART = 1
      UID = -1 
      KK=3
      I = 0
C--------------------------------------------------
C      READING TRIAS INPUTS IN HM STRUCTURE
C-------------------------------------------------- 
      CALL CPP_TRIA_READ(TMP_IXTG,NIXTG,TMP_IPARTTG,SUB_TRIA,UID_TRIA)
C--------------------------------------------------
C      FILL OTHER STRUCTURES + CHECKS
C--------------------------------------------------
      NUMELTG6 = 0
      DO WHILE (KK <= 6)       
        DO N=1,NUMELTG0 
          IPARTTG_TMP = TMP_IPARTTG(N)

          IF( IPART(4,INDEX_PART) /= IPARTTG_TMP)THEN  
            DO J=1,NPART                            
              IF(IPART(4,J)== IPARTTG_TMP )INDEX_PART = J
            ENDDO  
          ENDIF           
          ISH3N = IGEO(18,IPART(2,INDEX_PART)) 
          IF(KK == 6 .AND. ISH3N==31) NUMELTG6 = NUMELTG6 + 1

          IF((KK==3.AND.ISH3N/=31).OR.(KK==6.AND.ISH3N==31))THEN
            I = I + 1
            ICNOD(I)=KK
            DO J=1,NIXTG
              IXTG(J,I) = TMP_IXTG(J,N) 
            ENDDO
            IPARTTG(I) = TMP_IPARTTG(N)
C--------------------------------------------------
            IF(SUB_TRIA(N) /= 0)THEN
              IF(UID_TRIA(N) == 0 .AND. LSUBMODEL(SUB_TRIA(N))%UID /= 0) 
     .             UID_TRIA(N) = LSUBMODEL(SUB_TRIA(N))%UID
            ENDIF
C--------------------------------------------------
C      UNITS
C--------------------------------------------------
            IF(UID_TRIA(N) /= UID )THEN
              UID = UID_TRIA(N)
              IFLAGUNIT = 0          
              DO J=1,NUNITS                         
                IF (UNITAB%UNIT_ID(J) == UID) THEN  
                  FAC_L = UNITAB%FAC_L(J)                 
                  IFLAGUNIT = 1                     
                ENDIF                               
              ENDDO                             
              IF (UID/=0.AND.IFLAGUNIT==0) THEN
                CALL ANCMSG(MSGID=643,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                      I1=UID,C1='/TRIA')
              ENDIF 
            ENDIF   
C--------------------------------------------------
C      INTERNAL PART ID
C--------------------------------------------------
            IF( IPART(4,INDEX_PART) /= IPARTTG(I) )THEN  
              DO J=1,NPART                            
                IF(IPART(4,J)== IPARTTG(I) ) INDEX_PART = J
              ENDDO  
            ENDIF
            IF( IPART(4,INDEX_PART) /= IPARTTG(I) ) THEN
              CALL ANCMSG(MSGID=402,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    C1="TRIA",
     .                    I1=IPARTTG(I),
     .                    I2=IPARTTG(I),
     .                    PRMOD=MSG_CUMU)
            ENDIF             
            IPARTTG(I) = INDEX_PART
C--------------------------------------------------           
            MT=IPART(1,INDEX_PART)                         
            IPID=IPART(2,INDEX_PART)                                
            IXTG(1,I)=MT                                
            IXTG(5,I)=IPID
            IF (IXTG(NIXTG,I)>ID_LIMIT)THEN
                CALL ANCMSG(MSGID=509,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                      I1=IXTG(NIXTG,I),C1=LINE,C2='/TRIA')
            ELSEIF (NADMESH/=0.AND.IXTG(NIXTG,I)>ID_LIMIT_ADMESH)THEN
                CALL ANCMSG(MSGID=1069,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                      I1=IXTG(NIXTG,I),C1=LINE,C2='/TRIA')
            ENDIF       
                                       
            DO J=2,4                                                 
              IXTG(J,I)=USR2SYS(IXTG(J,I),ITABM1,MESS,ID)  
              CALL ANODSET(IXTG(J,I), CHECK_SHELL)     
            ENDDO
          ENDIF
          IF (I  == NUMELTG0) KK = 7 
        ENDDO
        IF (I  < NUMELTG0) THEN 
          KK = 6
        ELSE
c exit from DOWHILE (kk <=6)
          KK = 7
        ENDIF
      ENDDO
      IF(ALLOCATED(SUB_TRIA)) DEALLOCATE(SUB_TRIA)
      IF(ALLOCATED(UID_TRIA)) DEALLOCATE(UID_TRIA)

      IF(ALLOCATED(TMP_IXTG)) DEALLOCATE(TMP_IXTG)
      IF(ALLOCATED(TMP_IPARTTG)) DEALLOCATE(TMP_IPARTTG)
C
      I1=1
      I2=MIN0(50,NUMELTG0)
C
      IF(IPRI>=5)THEN
  90   WRITE (IOUT,'(//A/A//A/)')' 2D TRIANGULAR ELEMENTS ',
     & ' ELEMENT INTERNAL   MATER   PRSET   NODE1   NODE2   NODE3'
       DO I=I1,I2
         MID = IPM (1,IXTG(1,I))
         PID = IGEO(1,IXTG(5,I))
         WRITE (IOUT,'(7(I10,1X))') IXTG(NIXTG,I),I,MID,PID,
     .                      (ITAB(IXTG(J,I)),J=2,4)
       ENDDO
       IF(I2==NUMELTG0)GOTO 200
       I1=I1+50
       I2=MIN0(I2+50,NUMELTG0)
       GOTO 90
      ENDIF
C
 200  CONTINUE
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
c
      CALL VDOUBLE(IXTG(NIXTG,1),NIXTG,NUMELTG0,MESS,0,BID)
c      CALL ANCNTG(IDS,I,J)
      IDS = 44
c      CALL ANCHECK(IDS)
C
      RETURN
      END
