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
Chd|  HM_READ_TRUSS                 source/elements/reader/hm_read_truss.F
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
      SUBROUTINE HM_READ_TRUSS(IXT ,ITAB,ITABM1,IPART,IPARTT,
     .                  IPM    ,IGEO  ,LSUBMODEL)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ /TRUSS ELEMENTS USING HM_READER
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME            DESCRIPTION                         
C
C     IXT             TRUSS ELEM ARRAY : CONNECTIVITY, ID, PID
C     ITAB            USER ID OF NODES         
C     ITABM1          REVERSE TAB ITAB
C     IPART           PART ARRAY      
C     IPARTT          INTERNAL PART ID OF A GIVEN TRUSS ELEMENT 
C     IPM             MATERIAL ARRAY (INTEGER)
C     IGEO            PROP ARRAY (INTEGER)
C     LSUBMODEL       SUBMODEL STRUCTURE     
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
C---------------------------------------------------------
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
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com10_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "submod_c.inc"
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
      INTEGER,INTENT(OUT)::IXT(NIXT,*)
      INTEGER,INTENT(OUT)::IPARTT(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, I1, I2, MID, PID,MT,IPID,ID,IDS,J,N,JC,STAT
      INTEGER FLAG_FMT,FLAG_FMT_TMP,IFIX_TMP,CPT,
     .        INDEX_PART,OFF_DEF(NSUBMOD),OFF_NOD(NSUBMOD),
     .        OFF_EL(NSUBMOD),OFF_PART(NSUBMOD)
      CHARACTER MESS*40, MESS2*40
      my_real
     .   BID 
      INTEGER, DIMENSION(:), ALLOCATABLE :: SUB_TRUSS
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NINTRN
      INTEGER USR2SYS
C
      DATA MESS/'3D TRUSS ELEMENTS DEFINITION            '/
      DATA MESS2/'3D TRUSS ELEMENTS SELECTION FOR TH PLOT '/
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
      ALLOCATE (SUB_TRUSS(NUMELT),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='SUB_TRUSS') 
      SUB_TRUSS(1:NUMELT) = 0
      INDEX_PART = 1
C--------------------------------------------------
C      READING TRUSS INPUTS IN HM STRUCTURE
C-------------------------------------------------- 
      CALL CPP_TRUSS_READ(IXT,NIXT,IPARTT,SUB_TRUSS)
C--------------------------------------------------
C      FILL OTHER STRUCTURES + CHECKS
C--------------------------------------------------
        I=0
        DO N=1,NUMELT
          I = I + 1 
C--------------------------------------------------
C      SUBMODEL OFFSET
C--------------------------------------------------
          IF(SUB_TRUSS(I) /= 0)THEN
            IF(IPARTT(I) /= 0) IPARTT(I) = IPARTT(I) + OFF_PART(SUB_TRUSS(I)) 
            IF(IXT(NIXT,I) /= 0) IXT(NIXT,I) = IXT(NIXT,I) + OFF_EL(SUB_TRUSS(I))
            DO J=2,3
              IF(IXT(J,I) /= 0) IXT(J,I) = IXT(J,I) + OFF_NOD(SUB_TRUSS(I))
            ENDDO
          ENDIF
C--------------------------------------------------
C      INTERNAL PART ID
C--------------------------------------------------
          IF( IPART(4,INDEX_PART) /= IPARTT(I) )THEN  
            DO J=1,NPART                            
              IF(IPART(4,J)== IPARTT(I) ) INDEX_PART = J
            ENDDO  
          ENDIF
          IF( IPART(4,INDEX_PART) /= IPARTT(I) ) THEN
            CALL ANCMSG(MSGID=402,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  C1="TRUSS",
     .                  I1=IPARTT(I),
     .                  I2=IPARTT(I),
     .                  PRMOD=MSG_CUMU)
          ENDIF 
          IPARTT(I) = INDEX_PART
C--------------------------------------------------
          MT=IPART(1,INDEX_PART)                         
          IPID=IPART(2,INDEX_PART) 
          IXT(1,I)=MT
          IXT(4,I)=IPID 

          IF (IXT(5,I)>ID_LIMIT) THEN
            CALL ANCMSG(MSGID=509,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                  I1=IXT(5,I),C1=LINE,C2='/TRUSS')
          ENDIF

          DO J=2,3           
            IXT(J,I)=USR2SYS(IXT(J,I),ITABM1,MESS,IXT(5,I))
            CALL ANODSET(IXT(J,I), CHECK_TRUSS)             
          ENDDO
        ENDDO
      IF(ALLOCATED(SUB_TRUSS)) DEALLOCATE(SUB_TRUSS)
C-----------
      CALL ANCMSG(MSGID=402,                 
     .            MSGTYPE=MSGERROR,         
     .            ANMODE=ANINFO_BLIND_1,    
     .            PRMOD=MSG_PRINT) 
C
      I1=1
      I2=MIN0(50,NUMELT)
C-------------------------------------
C Recherche des ID doubles
C-------------------------------------
      IDS = 79
      I = 0
      J = 0
c      CALL ANCNTS(IDS,I)
      CALL VDOUBLE(IXT(NIXT,1),NIXT,NUMELT,MESS,0,BID)
c      CALL ANCNTG(IDS,I,J)
      IDS = 21
c      CALL ANCHECK(IDS)
C
   90 WRITE (IOUT,300)
      DO I=I1,I2
           MID=IPM(1,IXT(1,I))
           PID=IGEO(1,IXT(4,I))
         WRITE (IOUT,'(6(I10,1X))') I,IXT(5,I),MID,PID,
     .                    ITAB(IXT(2,I)),ITAB(IXT(3,I))
      ENDDO
      IF(I2==NUMELT)GOTO 200
      I1=I1+50
      I2=MIN0(I2+50,NUMELT)
      GOTO 90
C
 200  CONTINUE
      RETURN
C----
 300  FORMAT(/' TRUSS ELEMENTS'            /
     +        ' --------------'            /
     + '  LOC-EL  GLO-EL   MATER    GEOM   NODE1   NODE2')
 310  FORMAT(' TRUSS ELEMENT TH SELECTION'/
     +       ' --------------------------'/)
      RETURN
      END
