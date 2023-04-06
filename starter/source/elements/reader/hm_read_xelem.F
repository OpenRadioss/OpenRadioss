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
Chd|  HM_READ_XELEM                 source/elements/reader/hm_read_xelem.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ANODSET                       source/output/analyse/analyse_node.c
Chd|        UDOUBLE                       source/system/sysfus.F        
Chd|        NODGRNR5                      source/starter/freform.F      
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_XELEM(IGRNOD   ,ITAB ,ITABM1,IPART,IPARTX,
     .                  IPM,IGEO ,KXX ,IXX, LSUBMODEL)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ /XELEM ELEMENTS USING HM_READER
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME            DESCRIPTION                         
C
C     IGRNOD          NODE GROUP ARRAY
C     ITAB            USER ID OF NODES         
C     ITABM1          REVERSE TAB ITAB
C     IPART           PART ARRAY      
C     IPARTX          INTERNAL PART ID OF A GIVEN XELEM ELEMENT 
C     IPM             MAT ARRAY (INTEGER)
C     IGEO            PROP ARRAY (INTEGER)
C     KXX             XELEM CONNECTIVITY NODES 
C     IXX             XELEM ARRAY (INTEGER)
C     LSUBMODEL       SUBMODEL STRUCTURE   
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE GROUPDEF_MOD
C----------------------------------------------------------
C      XELEM ELEMENT READ
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
#include      "scr23_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      TYPE(GROUP_),INTENT(IN)::IGRNOD(NGRNOD)
      INTEGER,INTENT(IN)::ITAB(*)
      INTEGER,INTENT(IN)::ITABM1(*)
      INTEGER,INTENT(IN)::IPART(LIPART1,*)
      INTEGER,INTENT(IN)::IGEO(NPROPGI,*)
      INTEGER,INTENT(IN)::IPM(NPROPMI,*)
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C OUTPUT ARGUMENTS
      INTEGER,INTENT(OUT)::KXX(NIXX,*)
      INTEGER,INTENT(OUT)::IXX(*)
      INTEGER,INTENT(OUT)::IPARTX(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, I1, I2,PID,N,ID,IDS,J,STAT,MID,IAD,NNOD,IGS
      INTEGER INDEX_PART
      INTEGER TABIDS(NUMELX)
      CHARACTER MESS*40
      my_real
     .   BID 
      INTEGER, DIMENSION(:), ALLOCATABLE :: SUB_XELEM,IDEX,IDGU
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NODGRNR5
      DATA MESS /'MULTI-PURPOSE ELEMENTS DEFINITION       '/
C=======================================================================
C--------------------------------------------------
C      ALLOCS & INITS
C--------------------------------------------------
      ALLOCATE (SUB_XELEM(NUMELX),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='SUB_XELEM') 
      SUB_XELEM(1:NUMELX) = 0
      ALLOCATE (IDEX(NUMELX),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='IDEX') 
      IDEX(1:NUMELX) = 0
      ALLOCATE (IDGU(NUMELX),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='IDGU') 
      IDGU(1:NUMELX) = 0
      INDEX_PART = 1
C--------------------------------------------------
C      READING SPRING INPUTS IN HM STRUCTURE
C-------------------------------------------------- 
      CALL CPP_XELEM_READ(IDEX,IDGU,IPARTX,SUB_XELEM)

      IAD =1
      DO N=1,NUMELX

C--------------------------------------------------
C      INTERNAL PART ID
C--------------------------------------------------
        IF( IPART(4,INDEX_PART) /= IPARTX(N) )THEN  
          DO J=1,NPART                            
            IF(IPART(4,J)== IPARTX(N) ) INDEX_PART = J            
          ENDDO  
        ENDIF
        IF(IPART(4,INDEX_PART) /= IPARTX(N)) THEN
          CALL ANCMSG(MSGID=402,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                C1='XELEM',
     .                I1=IPARTX(N),
     .                I2=IPARTX(N),
     .                PRMOD=MSG_CUMU)
        ENDIF             
        IPARTX(N) = INDEX_PART


        KXX(1,N) =IPART(1,INDEX_PART)
        KXX(2,N) =IPART(2,INDEX_PART)
        KXX(4,N) =IAD
C
        KXX(5,N)=IDEX(N)
C
        NNOD = NODGRNR5(IDGU(N)   ,IGS     ,IXX(IAD),IGRNOD   ,
     .                    ITABM1 ,MESS    )
C       check non sorted nodes group type.
        IF (IGS/=0.AND.IGRNOD(IGS)%SORTED/=1) THEN
           CALL ANCMSG(MSGID=411,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=KXX(5,N),
     .                 I2=IGRNOD(IGS)%ID)
        ENDIF
        IF (NNOD < 1) THEN
           CALL ANCMSG(MSGID=412,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=KXX(5,N),
     .                 I2=NNOD)
        ENDIF
C
        CALL ANODSET(IXX(IAD), CHECK_2N)
        CALL ANODSET(IXX(IAD+NNOD-1), CHECK_2N)
        DO 10 I=2,NNOD-2
           CALL ANODSET(IXX(IAD+I), CHECK_USED)
 10     CONTINUE
        KXX(3,N)=NNOD
        IF (NNOD>MAXNX) MAXNX=NNOD
        ISUMNX  =ISUMNX+NNOD
C
        IAD     =IAD+NNOD
      ENDDO
C-----------
      CALL ANCMSG(MSGID=402,                 
     .            MSGTYPE=MSGERROR,         
     .            ANMODE=ANINFO_BLIND_1,    
     .            PRMOD=MSG_PRINT)
C-------------------------------------
C Recherche des ID doubles
C-------------------------------------
      DO I=1,NUMELX
         TABIDS(I)= KXX(NIXX,I)
      ENDDO
      CALL UDOUBLE(TABIDS,1,NUMELX,MESS,0,BID)
C-------------------------------------
C Print
C-------------------------------------
      I1=1
      I2=MIN0(50,NUMELX)
C
   90 WRITE (IOUT,300)
      DO 100 I=I1,I2
       MID=IPM(1,KXX(1,I))
       PID=IGEO(1,KXX(2,I))
       WRITE (IOUT,'(4(I10,1X))') I,KXX(NIXX,I),MID,PID
       WRITE (IOUT,'(10(I10,1X))')
     .       (ITAB(IXX(IAD)),IAD=KXX(4,I),KXX(4,I)+KXX(3,I)-1)
       WRITE (IOUT,'(A)') 'END OF ELEMENT TRACEBACK'
c      call flush(IOUT)
  100 CONTINUE
      IF(I2==NUMELX)GOTO 200
      I1=I1+50
      I2=MIN0(I2+50,NUMELX)
      GOTO 90
C
 200  CONTINUE
C
 300  FORMAT(/' MULTI-PURPOSE ELEMENTS'/
     +        ' ----------------------'/
     + '  LOC-EL  GLO-EL   MATER    GEOM'/
     + '  NODES LIST')
      RETURN
      END
