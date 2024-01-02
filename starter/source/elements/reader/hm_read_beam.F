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
Chd|  HM_READ_BEAM                  source/elements/reader/hm_read_beam.F
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
      SUBROUTINE HM_READ_BEAM(IXP    ,ITAB ,ITABM1,IPART,IPARTP,
     .                  IPM    ,IGEO   ,LSUBMODEL)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ /BEAM ELEMENTS USING HM_READER
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME            DESCRIPTION                         
C
C     IXP             /BEAM ARRAY : CONNECTIVITY, ID, PID
C     ITAB            USER ID OF NODES         
C     ITABM1          REVERSE TAB ITAB
C     IPART           PART ARRAY      
C     IPARTP          INTERNAL PART ID OF A GIVEN BEAM (INTERNAL ID) 
C     IPM             MATERIAL ARRAY (INTEGER) 
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
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      INTEGER,INTENT(IN)::ITAB(*)
      INTEGER,INTENT(IN)::ITABM1(*)
      INTEGER,INTENT(IN)::IPART(LIPART1,*)
      INTEGER,INTENT(IN)::IGEO(NPROPGI,*)
      INTEGER,INTENT(IN)::IPM(NPROPMI,*)
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C OUTPUT ARGUMENTS
      INTEGER,INTENT(OUT)::IXP(NIXP,*)
      INTEGER,INTENT(OUT)::IPARTP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, I1, I2, MID, PID,MT,IPID,ID,IDS,J,N,JC,STAT
      INTEGER CPT,INDEX_PART
      CHARACTER MESS*40, MESS2*40
      my_real
     .   BID 
      INTEGER, DIMENSION(:), ALLOCATABLE :: SUB_BEAM
      INTEGER N2,N3,N4
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
      INTEGER NINTRN
      DATA MESS /'3D BEAM ELEMENTS DEFINITION             '/
      DATA MESS2/'3D BEAM ELEMENTS SELECTION FOR TH PLOT  '/
C=======================================================================
C--------------------------------------------------
C      ALLOCS & INITS
C--------------------------------------------------
      ALLOCATE (SUB_BEAM(NUMELP),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='SUB_BEAM') 
      SUB_BEAM(1:NUMELP) = 0
      INDEX_PART = 1
C--------------------------------------------------
C      READING BEAM INPUTS IN HM STRUCTURE
C-------------------------------------------------- 
      CALL CPP_BEAM_READ(IXP,NIXP,IPARTP,SUB_BEAM)
C--------------------------------------------------
C      FILL OTHER STRUCTURES + CHECKS
C--------------------------------------------------
      DO I=1,NUMELP 
C--------------------------------------------------
C      INTERNAL PART ID
C--------------------------------------------------
        IF( IPART(4,INDEX_PART) /= IPARTP(I) )THEN  
          DO J=1,NPART                            
            IF(IPART(4,J)== IPARTP(I) ) INDEX_PART = J            
          ENDDO  
        ENDIF
        IF(IPART(4,INDEX_PART) /= IPARTP(I)) THEN
          CALL ANCMSG(MSGID=402,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                C1="BEAM",
     .                I1=IPARTP(I),
     .                I2=IPARTP(I),
     .                PRMOD=MSG_CUMU)
        ENDIF             
        IPARTP(I) = INDEX_PART
C--------------------------------------------------
        MT=IPART(1,INDEX_PART)                         
        IPID=IPART(2,INDEX_PART)  
        IXP(1,I)=MT                               
        IXP(5,I)=IPID 
        IF (IXP(6,I)>ID_LIMIT) THEN               
          CALL ANCMSG(MSGID=509,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I1=IXP(6,I),C1=LINE,C2='/BEAM')
        ENDIF
C optional Node 3 
        IF (IXP(4,I)==0 .OR. IXP(4,I)==IXP(2,I)
     .      .OR. IXP(4,I)==IXP(3,I)) THEN
          CALL ANCMSG(MSGID=2093,
     .                MSGTYPE=MSGINFO,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=IPART(4,INDEX_PART),
     .                I2=IXP(6,I),
     .                PRMOD=MSG_CUMU)
          IXP(4,I) = IXP(3,I)                    
        ENDIF
        DO J=2,4                                            
          IXP(J,I)=USR2SYS(IXP(J,I),ITABM1,MESS,IXP(6,I))  
        ENDDO
C Node 1 and 2 Must be connected to something (CHECK_BEAM)
C Node 3 is just a used node, to define directions (CHECK_USED)
        CALL ANODSET(IXP(2,I), CHECK_BEAM)  
        CALL ANODSET(IXP(3,I), CHECK_BEAM)  
        CALL ANODSET(IXP(4,I), CHECK_USED)
      ENDDO
      IF(ALLOCATED(SUB_BEAM)) DEALLOCATE(SUB_BEAM)
C-----------
      CALL ANCMSG(MSGID=402,                 
     .            MSGTYPE=MSGERROR,         
     .            ANMODE=ANINFO_BLIND_1,    
     .            PRMOD=MSG_PRINT) 
C     
      CALL ANCMSG(MSGID=2093,                 
     .            MSGTYPE=MSGINFO,         
     .            ANMODE=ANINFO_BLIND_1,    
     .            PRMOD=MSG_PRINT) 
C-------------------------------------
C Recherche des ID doubles
C-------------------------------------
      IDS = 79
      I = 0
      J = 0
c      CALL ANCNTS(IDS,I)
      CALL VDOUBLE(IXP(NIXP,1),NIXP,NUMELP,MESS,0,BID)
c      CALL ANCNTG(IDS,I,J)
      IDS = 28
c      CALL ANCHECK(IDS)
C
      I1=1
      I2=MIN0(50,NUMELP)
C-------------------------------------
   90 WRITE (IOUT,300)
      DO I=I1,I2
        MID=IPM (1,IXP(1,I))   
        PID=IGEO(1,IXP(5,I))  
        N2=IXP(2,I)
        N3=IXP(3,I)
        N4=IXP(4,I) 
        IF(N2>0)N2=ITAB(N2)
        IF(N3>0)N3=ITAB(N3)
        IF(N4>0)N4=ITAB(N4)                
        WRITE (IOUT,'(7(I10,1X))')I,IXP(6,I),MID,PID,N2,N3,N4
      ENDDO
      IF(I2==NUMELP)GOTO 200
      I1=I1+50
      I2=MIN0(I2+50,NUMELP)
      GOTO 90
C-------------------------------------
 200  CONTINUE
      RETURN
 300  FORMAT(/' BEAM ELEMENTS'/
     +        ' -------------'/
     + '  LOC-EL  GLO-EL   MATER     GEOM   NODE1   NODE2   NODE3')
 310  FORMAT(' BEAM ELEMENT TH SELECTION'/
     +       ' -------------------------'/)
      END
