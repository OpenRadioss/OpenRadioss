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
Chd|====================================================================
Chd|  HM_READ_RIVET                 source/elements/reader/hm_read_rivet.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ANODSET                       source/output/analyse/analyse_node.c
Chd|        IFRONTPLUS                    source/spmd/node/frontplus.F  
Chd|        RIVET0                        source/elements/reader/rivet0.F
Chd|        VDOUBLE                       source/system/sysfus.F        
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_RIVET(IXRI  ,V    ,VR   ,MS    ,IN    ,
     .                         RIVET ,GEO  ,ITAB ,ITABM1,IKINE ,
     .                         IPART ,IGEO ,LSUBMODEL)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ /RIVET ELEMENTS USING HM_READER
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME            DESCRIPTION                         
C
C     IXRI            /RIVET ARRAY : CONNECTIVITY, ID, PID
C     V               NODAL VELOCITIES
C     VR              NODAL ROTATIONEAL VELOCITIES
C     MS              NODAL MASSES
C     IN              NODAL INERTIA
C     RIVET           RIVET ARRAY (FLOAT)
C     GEO             PROP ARRAY (FLOAT)
C     ITAB            USER ID OF NODES         
C     ITABM1          REVERSE TAB ITAB       
C     IKINE           KINEMATIC CONDITION ARRAY
C     IPART           PART ARRAY      
C     IGEO            PROP ARRAY (INTEGER)
C     LSUBMODEL       SUBMODEL STRUCTURE     
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
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
#include      "param_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      my_real,INTENT(INOUT)::MS(*)
      my_real,INTENT(INOUT)::IN(*)
      my_real,INTENT(IN)::GEO(NPROPG,*)
      INTEGER,INTENT(IN)::ITAB(*)
      INTEGER,INTENT(IN)::ITABM1(*)
      INTEGER,INTENT(IN)::IPART(LIPART1,*)
      INTEGER,INTENT(IN)::IGEO(NPROPGI,*)
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C OUTPUT ARGUMENTS
      INTEGER,INTENT(OUT)::IXRI(4,*)
C INPUT/OUTPUT ARGUMENTS
      my_real,INTENT(INOUT)::V(3,*)
      my_real,INTENT(INOUT)::VR(3,*)
      my_real,INTENT(INOUT)::RIVET(*)
      INTEGER,INTENT(IN)::IKINE(*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL      
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, I1, I2, PID,MT,IPID,J,N,STAT,P,IF1,IF2
      INTEGER CPT,INDEX_PART
      CHARACTER MESS*40, MESS2*40
      my_real
     .   BID 
      INTEGER, DIMENSION(:), ALLOCATABLE :: SUB_RIVET,IPART_RIVET
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
      DATA MESS/'RIVET OR SPOTWELD DEFINITION            '/
C=======================================================================
C--------------------------------------------------
C      ALLOCS & INITS
C--------------------------------------------------
      ALLOCATE (SUB_RIVET(NRIVET),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='SUB_RIVET') 
      SUB_RIVET(1:NRIVET) = 0
      ALLOCATE (IPART_RIVET(NRIVET),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='IPART_RIVET') 
      IPART_RIVET(1:NRIVET) = 0
      INDEX_PART = 1
C--------------------------------------------------
C      READING BEAM INPUTS IN HM STRUCTURE
C-------------------------------------------------- 
      CALL CPP_RIVET_READ(IXRI,4,IPART_RIVET,SUB_RIVET)
C--------------------------------------------------
C      FILL OTHER STRUCTURES + CHECKS
C--------------------------------------------------
      DO I=1,NRIVET
C--------------------------------------------------
C      INTERNAL PART ID
C--------------------------------------------------
        IF( IPART(4,INDEX_PART) /= IPART_RIVET(I) )THEN  
          DO J=1,NPART                            
            IF(IPART(4,J)== IPART_RIVET(I) ) INDEX_PART = J            
          ENDDO  
        ENDIF
        IF(IPART(4,INDEX_PART) /= IPART_RIVET(I)) THEN
          CALL ANCMSG(MSGID=402,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                C1="RIVET",
     .                I1=IPART_RIVET(I),
     .                I2=IPART_RIVET(I),
     .                PRMOD=MSG_CUMU)
        ENDIF             
        IPART_RIVET(I) = INDEX_PART
        MT=IPART(1,INDEX_PART)                         
        IPID=IPART(2,INDEX_PART)  
        IXRI(1,I)=IPID   
        IF (IXRI(4,I)>ID_LIMIT) THEN               
          CALL ANCMSG(MSGID=509,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I1=IXRI(4,I),C1=LINE,C2='/RIVET')
        ENDIF
      ENDDO
C--------------------------------------------------
      CALL ANCMSG(MSGID=402,                 
     .            MSGTYPE=MSGERROR,         
     .            ANMODE=ANINFO_BLIND_1,    
     .            PRMOD=MSG_PRINT)
C-------------------------------------------------- 

      DO J=1,NRIVET
        IXRI(2,J)=USR2SYS(IXRI(2,J),ITABM1,MESS,IXRI(4,NRIVET))
        IXRI(3,J)=USR2SYS(IXRI(3,J),ITABM1,MESS,IXRI(4,NRIVET))
        CALL ANODSET(IXRI(2,J), CHECK_2N)  
        CALL ANODSET(IXRI(3,J), CHECK_2N)  
      ENDDO
      DO P = 1, NSPMD
        DO J=1,NRIVET
          IF1 = NLOCAL(IXRI(2,J),P)
          IF2 = NLOCAL(IXRI(3,J),P)
          IF (IF1==1.OR.IF2==1) THEN
            CALL IFRONTPLUS(IXRI(2,J),P)
            CALL IFRONTPLUS(IXRI(3,J),P)
          ENDIF
        ENDDO
      ENDDO
C-------------------------------------
C Recherche des ID doubles
C-------------------------------------
      CALL VDOUBLE(IXRI(4,1),4,NRIVET,MESS,0,BID)
C-------------------------------------
      DO J=1,NRIVET
        IF1 = 0
        DO P = 1, NSPMD
          IF1 = IF1 + NLOCAL(IXRI(2,J),P)
        ENDDO
        IF (IF1==0) THEN
          CALL IFRONTPLUS(IXRI(2,J),1)
          CALL IFRONTPLUS(IXRI(3,J),1)    
        ENDIF      
      ENDDO
      CALL RIVET0(V,VR,MS,IN,IXRI,RIVET,GEO,ITAB,IKINE)
C
      I1=1
      I2=MIN0(50,NRIVET)
C
   90 WRITE (IOUT,300)
      DO I=I1,I2
         PID=IGEO(1,IXRI(1,I))
         WRITE (IOUT,270) I,IXRI(4,I),PID,
     .                 ITAB(IXRI(2,I)),ITAB(IXRI(3,I))
      ENDDO
      IF(I2==NRIVET)RETURN
      I1=I1+50
      I2=MIN0(I2+50,NRIVET)
      GOTO 90
c
      IF(ALLOCATED(SUB_RIVET)) DEALLOCATE(SUB_RIVET)
      IF(ALLOCATED(IPART_RIVET)) DEALLOCATE(IPART_RIVET)
C-------------------------------------
C
 270  FORMAT(6I10)
 300  FORMAT(/' RIVET '            /
     +       ,' -------'/
     + '    LOC-EL    GLO-EL      GEOM     NODE1     NODE2')
      RETURN
      END
