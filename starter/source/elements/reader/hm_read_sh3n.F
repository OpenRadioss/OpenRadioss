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
Chd|  HM_READ_SH3N                  source/elements/reader/hm_read_sh3n.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ANODSET                       source/output/analyse/analyse_node.c
Chd|        APARTSET                      source/output/analyse/analyse_part.c
Chd|        FREERR                        source/starter/freform.F      
Chd|        VDOUBLE                       source/system/sysfus.F        
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_SH3N(IXTG  ,ITAB   ,ITABM1 ,IPART  ,IPARTTG ,
     .                  THK    ,PM    ,GEO    ,ICNOD  ,IGEO   ,IPM     ,
     .                  UNITAB ,ANGLE ,LSUBMODEL)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ /SH3N ELEMENTS USING HM_READER
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME            DESCRIPTION                         
C
C     IXTG            /SH3N ARRAY : CONNECTIVITY, ID, MID PID
C     ITAB            USER ID OF NODES         
C     ITABM1          REVERSE TAB ITAB
C     IPART           PART ARRAY      
C     IPARTTG         INTERNAL PART ID OF A GIVEN SH3N (INTERNAL ID)   
C     THK             THICKNESS OF A GIVEN SH3N (INTERNAL ID) 
C     PM              MATERIAL ARRAY     
C     GEO             PROP ARRAY (REAL)  
C     ICNOD           FLAG FOR SH3N WITH ISH3N = 31  
C     IGEO            PROP ARRAY (INTEGER)   
C     IPM             MATERIAL ARRAY (INTEGER)   
C     UNITAB          UNIT ARRAY 
C     ANGLE           ANGLE OF A GIVEN SH3N (INTERNAL ID) 
C     LSUBMODEL       SUBMODEL STRUCTURE     
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
C--------------------------------------------------------
C     LECTURE DES ELEMENTS COQUES TRIANGULAIRE
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
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com10_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
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
      my_real,
     .  INTENT(OUT)::ANGLE(*)
      my_real,
     .  INTENT(OUT)::THK(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   BID,FAC_L
      INTEGER I, J, I1, I2, ID,IDS,IPID,MT,N,MID,PID,UID,JC,STAT,IPARTTG_TMP
      INTEGER INDEX_PART,OFF_DEF(NSUBMOD),OFF_NOD(NSUBMOD),OFF_EL(NSUBMOD),
     .        OFF_PART(NSUBMOD)
      CHARACTER*40 MESS
      DATA MESS /'3D TRIANGULAR SHELL ELEMENT DEFINITION '/
      INTEGER ISH3N,KK,IFLAGUNIT
      INTEGER, DIMENSION(:), ALLOCATABLE :: SUB_SH3N,UID_SH3N,TMP_IPARTTG
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: TMP_IXTG
      REAL*8, DIMENSION(:), ALLOCATABLE :: HM_THK,HM_ANGLE
C-----------------------------------------------
C   FUNCTION
C-----------------------------------------------
      INTEGER USR2SYS
      INTEGER NINTRN
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
c      use NUMELTG0 IN PLACE OF NUMELTG ( NBADMESH routine is modifying NUMELTG )
C--------------------------------------------------
      ALLOCATE (SUB_SH3N(NUMELTG0),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='SUB_SH3N') 
      ALLOCATE (UID_SH3N(NUMELTG0),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='UID_SH3N') 
      ALLOCATE (HM_THK(NUMELTG0),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='HM_THK') 
      ALLOCATE (HM_ANGLE(NUMELTG0),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='HM_ANGLE') 
      ALLOCATE (TMP_IXTG(NIXTG,NUMELTG0),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='TMP_IXTG') 
      ALLOCATE (TMP_IPARTTG(NUMELTG0),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='TMP_IPARTTG')
      SUB_SH3N(1:NUMELTG0) = 0
      UID_SH3N(1:NUMELTG0) = 0
      TMP_IXTG(1:NIXTG,1:NUMELTG0) = 0
      TMP_IPARTTG(1:NUMELTG0) = 0
      HM_THK(1:NUMELTG0) = ZERO
      HM_ANGLE(1:NUMELTG0) = ZERO
      INDEX_PART = 1
      UID = -1 
      KK=3
      I = 0
C--------------------------------------------------
C      READING SH3NS INPUTS IN HM STRUCTURE
C-------------------------------------------------- 
      CALL CPP_SH3N_READ(TMP_IXTG,NIXTG,TMP_IPARTTG,HM_ANGLE,HM_THK,SUB_SH3N,UID_SH3N)
C--------------------------------------------------
C      FILL OTHER STRUCTURES + CHECKS
C--------------------------------------------------
      NUMELTG6 = 0
      DO WHILE (KK <= 6)       
        DO N=1,NUMELTG0 
          IPARTTG_TMP = TMP_IPARTTG(N)
          IF(SUB_SH3N(N) .NE. 0)THEN
            IF(TMP_IPARTTG(N) .NE. 0) IPARTTG_TMP = TMP_IPARTTG(N) + OFF_PART(SUB_SH3N(N)) 
          ENDIF

          IF( IPART(4,INDEX_PART) .NE. IPARTTG_TMP)THEN  
            DO J=1,NPART                            
              IF(IPART(4,J).EQ. IPARTTG_TMP )INDEX_PART = J
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
C      FOR _SP _DP PURPOSE
C--------------------------------------------------
            ANGLE(I) = HM_ANGLE(N) * PI / CENT80
            THK(I) = HM_THK(N)
C--------------------------------------------------
C      SUBMODEL OFFSET
C--------------------------------------------------
            IF(SUB_SH3N(N) .NE. 0)THEN
              IF(IPARTTG(I) .NE. 0) IPARTTG(I) = IPARTTG(I) + OFF_PART(SUB_SH3N(N)) 
              IF(IXTG(NIXTG,I) .NE. 0) IXTG(NIXTG,I) = IXTG(NIXTG,I) + OFF_EL(SUB_SH3N(N))
              DO J=2,4
                IF(IXTG(J,I) .NE. 0) IXTG(J,I) = IXTG(J,I) + OFF_NOD(SUB_SH3N(N))
              ENDDO
              IF(UID_SH3N(N) .NE. 0) UID_SH3N(N) = UID_SH3N(N) + OFF_DEF(SUB_SH3N(N))
              IF(UID_SH3N(N) == 0 .AND. LSUBMODEL(SUB_SH3N(N))%UID .NE. 0) 
     .             UID_SH3N(N) = LSUBMODEL(SUB_SH3N(N))%UID
            ENDIF
C--------------------------------------------------
C      UNITS
C--------------------------------------------------
            IF(UID_SH3N(N) .NE. UID )THEN
              UID = UID_SH3N(N)
              IFLAGUNIT = 0          
              DO J=1,NUNITS                         
                IF (UNITAB%UNIT_ID(J) == UID) THEN  
                  FAC_L = UNITAB%FAC_L(J)                 
                  IFLAGUNIT = 1                     
                ENDIF                               
              ENDDO                             
              IF (UID.NE.0.AND.IFLAGUNIT.EQ.0) THEN
                CALL ANCMSG(MSGID=643,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                      I1=UID,C1='/SH3N')
              ENDIF 
            ENDIF   
            THK(I) = THK(I) * FAC_L
C--------------------------------------------------
C      INTERNAL PART ID
C--------------------------------------------------
            IF( IPART(4,INDEX_PART) .NE. IPARTTG(I) )THEN  
              DO J=1,NPART                            
                IF(IPART(4,J).EQ. IPARTTG(I) ) INDEX_PART = J
              ENDDO  
            ENDIF
            IF( IPART(4,INDEX_PART) .NE. IPARTTG(I) ) THEN
              CALL ANCMSG(MSGID=402,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    C1="SH3N",
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
            IF (IXTG(NIXTG,I).GT.ID_LIMIT)THEN
                CALL ANCMSG(MSGID=509,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                      I1=IXTG(NIXTG,I),C1=LINE,C2='/SH3N')
            ELSEIF (NADMESH.NE.0.AND.IXTG(NIXTG,I).GT.ID_LIMIT_ADMESH)THEN
                CALL ANCMSG(MSGID=1069,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                      I1=IXTG(NIXTG,I),C1=LINE,C2='/SH3N')
            ENDIF                                                                   
            IF(THK(I).GT.0) THEN                       
               CALL APARTSET(INDEX_PART, CHECK_THICK_SHELL)   
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
      IF(ALLOCATED(SUB_SH3N)) DEALLOCATE(SUB_SH3N)
      IF(ALLOCATED(UID_SH3N)) DEALLOCATE(UID_SH3N)
      IF(ALLOCATED(HM_THK)) DEALLOCATE(HM_THK)
      IF(ALLOCATED(HM_ANGLE)) DEALLOCATE(HM_ANGLE)

      IF(ALLOCATED(TMP_IXTG)) DEALLOCATE(TMP_IXTG)
      IF(ALLOCATED(TMP_IPARTTG)) DEALLOCATE(TMP_IPARTTG)
C
      I1=1
      I2=MIN0(50,NUMELTG0)
C
      IF(IPRI.GE.5)THEN
  90   WRITE (IOUT,'(//A/A//A/)')' TRIANGULAR SHELL ELEMENTS ',
     & ' ELEMENT INTERNAL   MATER   PRSET   NODE1   NODE2   NODE3'
       DO I=I1,I2
         MID = IPM (1,IXTG(1,I))
         PID = IGEO(1,IXTG(5,I))
         WRITE (IOUT,'(7(I10,1X),1PG20.13,1PG20.13)') IXTG(NIXTG,I),I,MID,PID,
     .                      (ITAB(IXTG(J,I)),J=2,4),ANGLE(I),THK(I)
       ENDDO
       IF(I2.EQ.NUMELTG0)GOTO 200
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
 999  CALL FREERR(1)
      RETURN
      END
