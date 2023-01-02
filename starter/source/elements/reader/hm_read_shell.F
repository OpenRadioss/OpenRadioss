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
Chd|  HM_READ_SHELL                 source/elements/reader/hm_read_shell.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ANODSET                       source/output/analyse/analyse_node.c
Chd|        APARTSET                      source/output/analyse/analyse_part.c
Chd|        VDOUBLE                       source/system/sysfus.F        
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_SHELL(IXC   ,ITAB  ,ITABM1,IPART,IPARTC,
     .                  THK   ,IPM   ,IGEO  ,UNITAB,ITAG ,ANGLE ,LSUBMODEL)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ /SHELL ELEMENTS USING HM_READER
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME            DESCRIPTION                         
C
C     IXC             /SHELL ARRAY : CONNECTIVITY, ID, MID PID
C     ITAB            USER ID OF NODES         
C     ITABM1          REVERSE TAB ITAB
C     IPART           PART ARRAY      
C     IPARTC          INTERNAL PART ID OF A GIVEN SHELL (INTERNAL ID)   
C     THK             THICKNESS OF A GIVEN SHELL (INTERNAL ID) 
C     IPM             MATERIAL ARRAY (INTEGER) 
C     IGEO            PROP ARRAY (INTEGER)     
C     ITAG            XFEM TAG     
C     UNITAB          UNIT ARRAY 
C     ANGLE           ANGLE OF A GIVEN SHELL (INTERNAL ID) 
C     LSUBMODEL       SUBMODEL STRUCTURE     
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
C--------------------------------------------------------
C     LECTURE DES ELEMENTS COQUES 4 NOEUDS
C     VERSIION NUMEROTATION DES NOEUDS LIBRE/MARS 90/DIM
C--------------------------------------------------------
C
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com10_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "param_c.inc"
#include      "titr_c.inc"
#include      "remesh_c.inc"
#include      "sysunit.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER,INTENT(IN)::ITAB(*)
      INTEGER,INTENT(IN)::ITABM1(*)
      INTEGER,INTENT(IN)::IPART(LIPART1,*)
      INTEGER,INTENT(IN)::IGEO(NPROPGI,*)
      INTEGER,INTENT(IN)::IPM(NPROPMI,*)
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C OUTPUT ARGUMENTS
      INTEGER,INTENT(OUT)::IXC(NIXC,*)
      INTEGER,INTENT(OUT)::IPARTC(*)
      INTEGER,INTENT(OUT)::ITAG(*)
      my_real,
     .  INTENT(OUT)::ANGLE(*)
      my_real,
     .  INTENT(OUT)::THK(*) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, I1, I2, ID,IDS,IPID,MT,N,MID,PID,UID,NDEGEN,JC,STAT,
     .   IFLAGUNIT,FLAG_FMT,FLAG_FMT_TMP,IFIX_TMP,ISHXFEM,IOUTN,IERROR,INDEX_PART
      CHARACTER MESS*40, MESS2*40,TITR*nchartitle
      my_real
     .   BID,FAC_L
      INTEGER, DIMENSION(:), ALLOCATABLE :: SUB_SHELL,UID_SHELL
      REAL*8, DIMENSION(:), ALLOCATABLE :: HM_THK,HM_ANGLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NINTRN
      INTEGER USR2SYS
      DATA MESS/'3D SHELL ELEMENTS DEFINITION            '/
      DATA MESS2/'3D SHELL ELEMENTS SELECTION FOR TH PLOT '/
C=======================================================================
C--------------------------------------------------
C      ALLOCS & INITS
c      use NUMELC0 IN PLACE OF NUMELC ( NBADMESH routine is modifying NUMELC )
C--------------------------------------------------
      ALLOCATE (SUB_SHELL(NUMELC0),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='SUB_SHELL') 
      ALLOCATE (UID_SHELL(NUMELC0),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='UID_SHELL') 
      ALLOCATE (HM_THK(NUMELC0),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='HM_THK') 
      ALLOCATE (HM_ANGLE(NUMELC0),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='HM_ANGLE') 
      SUB_SHELL(1:NUMELC0) = 0
      UID_SHELL(1:NUMELC0) = 0
      HM_THK(1:NUMELC0) = ZERO
      HM_ANGLE(1:NUMELC0) = ZERO
      NDEGEN = 0
      INDEX_PART = 1
      UID = -1 
C--------------------------------------------------
C      READING SHELLS INPUTS IN HM STRUCTURE
C-------------------------------------------------- 
      CALL CPP_SHELL_READ(IXC,NIXC,IPARTC,HM_ANGLE,HM_THK,SUB_SHELL,UID_SHELL)
C--------------------------------------------------
C      FILL OTHER STRUCTURES + CHECKS
C--------------------------------------------------
      DO I=1,NUMELC0
C--------------------------------------------------
C      FOR _SP _DP PURPOSE
C--------------------------------------------------
       ANGLE(I) = HM_ANGLE(I) * PI / HUNDRED80
       THK(I) = HM_THK(I)
C--------------------------------------------------
C      SUBMODEL OFFSET
C--------------------------------------------------
        IF(SUB_SHELL(I) /= 0)THEN
          IF(UID_SHELL(I) == 0 .AND. LSUBMODEL(SUB_SHELL(I))%UID /= 0) 
     .         UID_SHELL(I) = LSUBMODEL(SUB_SHELL(I))%UID
        ENDIF
C--------------------------------------------------
C      UNITS
C--------------------------------------------------
        FAC_L = ONE
        IF(UID_SHELL(I) /= UID )THEN
          UID = UID_SHELL(I)
          IFLAGUNIT = 0          
          DO J=1,NUNITS                         
            IF (UNITAB%UNIT_ID(J) == UID) THEN  
              FAC_L = UNITAB%FAC_L(J)                 
              IFLAGUNIT = 1                     
            ENDIF                               
          ENDDO                                  
          IF (UID/=0.AND.IFLAGUNIT==0) THEN
            CALL ANCMSG(MSGID=643,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                  I1=UID,C1='/SHELL')
          ENDIF 
        ENDIF 
        THK(I) = THK(I) * FAC_L
C--------------------------------------------------
C      INTERNAL PART ID
C--------------------------------------------------
        IF( IPART(4,INDEX_PART) /= IPARTC(I) )THEN  
          DO J=1,NPART                            
            IF(IPART(4,J)== IPARTC(I) ) INDEX_PART = J 
          ENDDO  
        ENDIF
        IF(IPART(4,INDEX_PART) /= IPARTC(I)) THEN
          CALL ANCMSG(MSGID=402,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                C1="SHELL",
     .                I1=IPARTC(I),
     .                I2=IPARTC(I),
     .                PRMOD=MSG_CUMU)
        ENDIF 
        IPARTC(I) = INDEX_PART
C--------------------------------------------------
        MT=IPART(1,INDEX_PART)                         
        IPID=IPART(2,INDEX_PART)                                 
        IXC(1,I)=MT                                
        IXC(6,I)=IPID 
        IF (IXC(NIXC,I)>ID_LIMIT)THEN
            CALL ANCMSG(MSGID=509,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                  I1=IXC(NIXC,I),C1=LINE,C2='/SHELL')
        ELSEIF (NADMESH/=0.AND.IXC(NIXC,I)>ID_LIMIT_ADMESH)THEN
            CALL ANCMSG(MSGID=1069,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                  I1=IXC(NIXC,I),C1=LINE,C2='/SHELL')
        ENDIF                            
        IF(  ( IXC(4,I) == IXC(5,I)) .OR.        
     .       ( IXC(5,I) == 0 )) THEN             
           NDEGEN = NDEGEN + 1                     
           CALL ANCMSG(MSGID=430,
     .                 MSGTYPE=MSGWARNING,
     .                 I1=IXC(NIXC,I),
     .                 ANMODE=ANINFO_BLIND_2,
     .                 PRMOD=MSG_CUMU)
        ENDIF                                                                  
        IF(THK(I)>0) THEN                       
           CALL APARTSET(INDEX_PART, CHECK_THICK_SHELL)   
        ENDIF 
                               
        DO J=2,5                                                 
          IXC(J,I)=USR2SYS(IXC(J,I),ITABM1,MESS,ID) 
          CALL ANODSET(IXC(J,I), CHECK_SHELL)     
        ENDDO

        ISHXFEM = IGEO(19,IPID)

        IF(ISHXFEM > 0) THEN
          DO J=2,5                                                 
             ITAG(IXC(J,I)) = 1
          ENDDO
        ENDIF
      ENDDO
c
C      
      IF(ALLOCATED(SUB_SHELL)) DEALLOCATE(SUB_SHELL)
      IF(ALLOCATED(UID_SHELL)) DEALLOCATE(UID_SHELL)
      IF(ALLOCATED(HM_THK)) DEALLOCATE(HM_THK)
      IF(ALLOCATED(HM_ANGLE)) DEALLOCATE(HM_ANGLE)
C
      I1=1
      I2=MIN0(50,NUMELC0)
C
      IF(IPRI>=5)THEN
   90  WRITE (IOUT,'(//A/A//A,A/)')TITRE(110),TITRE(111),TITRE(102),TITRE(105)
       DO I=I1,I2
         MID = IPM(1,IXC(1,I))
         PID = IGEO(1,IXC(6,I)) 
         WRITE (IOUT,FMT='(8(I10,1X),1PG20.13,1X,1PG20.13)') IXC(NIXC,I),I,MID,PID,
     .                      (ITAB(IXC(J,I)),J=2,5),ANGLE(I),THK(I)
       ENDDO
       IF(I2==NUMELC0)GOTO 200
       I1=I1+50
       I2=MIN0(I2+50,NUMELC0)
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
C Search Duplicated Ids
C-------------------------------------
      IDS = 79
      I = 0
      J = 0
      CALL VDOUBLE(IXC(NIXC,1),NIXC,NUMELC0,MESS,0,BID)
      IDS = 17
      
      RETURN

      END
