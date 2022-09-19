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
Chd|  HM_READ_QUAD                  source/elements/reader/hm_read_quad.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ANODSET                       source/output/analyse/analyse_node.c
Chd|        VDOUBLE                       source/system/sysfus.F        
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_QUAD(IXQ   ,ITAB  ,ITABM1,IPART,IPARTQ,
     .                  IPM   ,IGEO  ,UNITAB ,LSUBMODEL)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ /QUAD ELEMENTS USING HM_READER
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME            DESCRIPTION                         
C
C     IXQ             /QUAD ARRAY : CONNECTIVITY, ID, MID PID
C     ITAB            USER ID OF NODES         
C     ITABM1          REVERSE TAB ITAB
C     IPART           PART ARRAY      
C     IPARTQ          INTERNAL PART ID OF A GIVEN QUAD (INTERNAL ID)   
C     IPM             MATERIAL ARRAY (INTEGER) 
C     IGEO            PROP ARRAY (INTEGER)  
C     UNITAB          UNIT ARRAY 
C     LSUBMODEL       SUBMODEL STRUCTURE     
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
C--------------------------------------------------------
C     LECTURE DES ELEMENTS QUAD 4 NOEUDS
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
      INTEGER,INTENT(OUT)::IXQ(NIXQ,*)
      INTEGER,INTENT(OUT)::IPARTQ(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, I1, I2, ID,IDS,IPID,MT,N,MID,PID,UID,NDEGEN,JC,STAT,
     .   IFLAGUNIT,FLAG_FMT,FLAG_FMT_TMP,IFIX_TMP,ISHXFEM,IOUTN,IERROR,INDEX_PART,
     .   OFF_DEF(NSUBMOD),OFF_NOD(NSUBMOD),OFF_EL(NSUBMOD),
     .   OFF_PART(NSUBMOD)
      CHARACTER MESS*40, MESS2*40,TITR*nchartitle
      my_real
     .   BID,FAC_L
      INTEGER, DIMENSION(:), ALLOCATABLE :: SUB_QUAD,UID_QUAD
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NINTRN
      INTEGER USR2SYS
      DATA MESS/'2D QUAD ELEMENTS DEFINITION            '/
      DATA MESS2/'2D QUAD ELEMENTS SELECTION FOR TH PLOT '/
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
c      use NUMELQ IN PLACE OF NUMELC ( NBADMESH routine is modifying NUMELC )
C--------------------------------------------------
      ALLOCATE (SUB_QUAD(NUMELQ),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='SUB_QUAD')
      ALLOCATE (UID_QUAD(NUMELQ),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='UID_QUAD')
      SUB_QUAD(1:NUMELQ) = 0
      UID_QUAD(1:NUMELQ) = 0
      NDEGEN = 0
      INDEX_PART = 1
      UID = -1
C--------------------------------------------------
C      READING QUADS INPUTS IN HM STRUCTURE
C-------------------------------------------------- 
      CALL CPP_QUAD_READ(IXQ,NIXQ,IPARTQ,SUB_QUAD,UID_QUAD)
C--------------------------------------------------
C      FILL OTHER STRUCTURES + CHECKS
C--------------------------------------------------
      DO I=1,NUMELQ
C--------------------------------------------------
C      SUBMODEL OFFSET
C--------------------------------------------------
        IF(SUB_QUAD(I) /= 0)THEN
          IF(IPARTQ(I) /= 0) IPARTQ(I) = IPARTQ(I) + OFF_PART(SUB_QUAD(I))
          IF(IXQ(NIXQ,I) /= 0) IXQ(NIXQ,I) = IXQ(NIXQ,I) + OFF_EL(SUB_QUAD(I))
          DO J=2,5
            IF(IXQ(J,I) /= 0) IXQ(J,I) = IXQ(J,I) + OFF_NOD(SUB_QUAD(I))
          ENDDO
          IF(UID_QUAD(I) /= 0) UID_QUAD(I) = UID_QUAD(I) + OFF_DEF(SUB_QUAD(I))
          IF(UID_QUAD(I) == 0 .AND. LSUBMODEL(SUB_QUAD(I))%UID /= 0)
     .         UID_QUAD(I) = LSUBMODEL(SUB_QUAD(I))%UID
        ENDIF
C--------------------------------------------------
C      UNITS
C--------------------------------------------------
        IF(UID_QUAD(I) /= UID )THEN
          UID = UID_QUAD(I)
          IFLAGUNIT = 0
          DO J=1,NUNITS
            IF (UNITAB%UNIT_ID(J) == UID) THEN
              FAC_L = UNITAB%FAC_L(J)
              IFLAGUNIT = 1
            ENDIF
          ENDDO
          IF (UID/=0.AND.IFLAGUNIT==0) THEN
            CALL ANCMSG(MSGID=643,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                  I1=UID,C1='/QUAD')
          ENDIF
        ENDIF
C--------------------------------------------------
C      INTERNAL PART ID
C--------------------------------------------------
        IF( IPART(4,INDEX_PART) /= IPARTQ(I) )THEN
          DO J=1,NPART
            IF(IPART(4,J)== IPARTQ(I) ) INDEX_PART = J
          ENDDO
        ENDIF
        IF(IPART(4,INDEX_PART) /= IPARTQ(I)) THEN
          CALL ANCMSG(MSGID=402,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                C1="QUAD",
     .                I1=IPARTQ(I),
     .                I2=IPARTQ(I),
     .                PRMOD=MSG_CUMU)
        ENDIF
        IPARTQ(I) = INDEX_PART
C--------------------------------------------------
        MT=IPART(1,INDEX_PART)
        IPID=IPART(2,INDEX_PART)
        IXQ(1,I)=MT
        IXQ(6,I)=IPID
        IF (IXQ(NIXQ,I)>ID_LIMIT)THEN
            CALL ANCMSG(MSGID=509,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                  I1=IXQ(NIXQ,I),C1=LINE,C2='/QUAD')
        ELSEIF (NADMESH/=0.AND.IXQ(NIXQ,I)>ID_LIMIT_ADMESH)THEN
            CALL ANCMSG(MSGID=1069,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                  I1=IXQ(NIXQ,I),C1=LINE,C2='/QUAD')
        ENDIF
        IF(  ( IXQ(4,I) == IXQ(5,I)) .OR.
     .       ( IXQ(5,I) == 0 )) THEN
           NDEGEN = NDEGEN + 1
           CALL ANCMSG(MSGID=430,
     .                 MSGTYPE=MSGWARNING,
     .                 I1=IXQ(NIXQ,I),
     .                 ANMODE=ANINFO_BLIND_2,
     .                 PRMOD=MSG_CUMU)
        ENDIF

        DO J=2,5
          IXQ(J,I)=USR2SYS(IXQ(J,I),ITABM1,MESS,ID)
          CALL ANODSET(IXQ(J,I), CHECK_SHELL)
        ENDDO

      ENDDO
c
      IF(ALLOCATED(SUB_QUAD)) DEALLOCATE(SUB_QUAD)
      IF(ALLOCATED(UID_QUAD)) DEALLOCATE(UID_QUAD)
C
      I1=1
      I2=MIN0(50,NUMELQ)
C
      IF(IPRI>=5)THEN
   90  WRITE (IOUT,'(//A/A//A,A/)')TITRE(110),TITRE(111),TITRE(102),TITRE(105)
       DO I=I1,I2
         MID = IPM(1,IXQ(1,I))
         PID = IGEO(1,IXQ(6,I))
         WRITE (IOUT,FMT='(8(I10,1X),1PG20.13,1X,1PG20.13)') IXQ(NIXQ,I),I,MID,PID,
     .                      (ITAB(IXQ(J,I)),J=2,5)
       ENDDO
       IF(I2==NUMELQ)GOTO 200
       I1=I1+50
       I2=MIN0(I2+50,NUMELQ)
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
      CALL VDOUBLE(IXQ(NIXQ,1),NIXQ,NUMELQ,MESS,0,BID)
      IDS = 17

      RETURN

      END
