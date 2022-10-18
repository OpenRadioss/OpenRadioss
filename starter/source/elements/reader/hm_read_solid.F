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
Chd|  HM_READ_SOLID                 source/elements/reader/hm_read_solid.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ANODSET                       source/output/analyse/analyse_node.c
Chd|        UDOUBLE                       source/system/sysfus.F        
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_SOLID(IXS   ,PM     ,ITAB ,ITABM1,
     .                  IPART,IPARTS,ISOLNOD,IXS10 ,IXS20,IXS16 ,
     .                  IGEO ,LSUBMODEL,IS_DYNA)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ /BRICK /TETRA4 /PENTA6 /TETRA10  /BRICK20 ELEMENTS USING HM_READER
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME            DESCRIPTION                         
C
C     IXS             SOLID ELEM ARRAY : CONNECTIVITY, ID, PID, MID
C     PM              MATERIAL ARRAY (REAL) 
C     ITAB            USER ID OF NODES         
C     ITABM1          REVERSE TAB ITAB
C     IPART           PART ARRAY      
C     IPARTS          INTERNAL PART ID OF A GIVEN SOLID ELEMENT 
C     ISOLNOD         NUMBER OF NODES OF SOLID ELEMENT 
C     IXS10           TETRA10 CONNECTIVITY NODES 5->10
C     IXS20           BRICK20 CONNECTIVITY NODES 9->20
C     IXS16           BRICK16 CONNECTIVITY NODES 9->16
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
#include      "com10_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "param_c.inc"
#include      "titr_c.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      INTEGER,INTENT(IN)::ITAB(*)
      INTEGER,INTENT(IN)::ITABM1(*)
      INTEGER,INTENT(IN)::IPART(LIPART1,*)
      INTEGER,INTENT(IN)::IGEO(NPROPGI,*)
      INTEGER,INTENT(IN)::IS_DYNA
      my_real,
     .  INTENT(IN)::PM(NPROPM,*)
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C OUTPUT ARGUMENTS
      INTEGER,INTENT(OUT)::ISOLNOD(*)
      INTEGER,INTENT(OUT)::IXS(NIXS,*)
      INTEGER,INTENT(OUT)::IXS10(6,*)
      INTEGER,INTENT(OUT)::IXS16(8,*)
      INTEGER,INTENT(OUT)::IXS20(12,*)
      INTEGER,INTENT(OUT)::IPARTS(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, MT, MLAW, JTUR, I1, I2, INEW,I10,I20,I16
      INTEGER IC,IC1,IC2,IC3,IC4,IPID,ID,IDS,N,JC,NSPHDIR,STAT
      INTEGER FLAG_FMT,FLAG_FMT_TMP,IFIX_TMP,NUMELS_READ,
     .        IOUTN, IERROR, INDEX_PART,OFF_DEF(NSUBMOD),
     .        OFF_NOD(NSUBMOD),OFF_EL(NSUBMOD),
     .        OFF_PART(NSUBMOD)
      my_real
     .   BID 
      CHARACTER MESS*40, MESS2*40
      INTEGER, DIMENSION(:), ALLOCATABLE :: SUB_SOL
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
C
      DATA MESS/'3D SOLID ELEMENTS DEFINITION            '/
      DATA MESS2/'3D SOLID ELEMENTS SELECTION FOR TH PLOT '/
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
      ALLOCATE (SUB_SOL(NUMELS),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='SUB_SOL') 
      SUB_SOL(1:NUMELS) = 0
      INDEX_PART = 1
C--------------------------------------------------
C      READING BRICK INPUTS IN HM STRUCTURE
C-------------------------------------------------- 
      CALL CPP_BRICK_READ(IXS,NIXS,IPARTS,SUB_SOL)
C--------------------------------------------------
C      FILL OTHER STRUCTURES + CHECKS
C--------------------------------------------------
      I = 0
      DO N=1,NUMBRICK
        I = I + 1
        IF(IXS(6,I)+IXS(7,I)+IXS(8,I)+IXS(9,I)==0)THEN
          IXS(9,I)=IXS(5,I)
          IXS(8,I)=IXS(4,I)
          IXS(7,I)=IXS(4,I)
          IXS(6,I)=IXS(5,I)
          IXS(5,I)=IXS(3,I)
          IXS(4,I)=IXS(3,I)
          IXS(3,I)=IXS(2,I)
          ISOLNOD(I)=4
        ELSEIF(IXS(8,I)+IXS(9,I)==0)THEN
          IXS(9,I)=IXS(5,I)
          IXS(8,I)=IXS(7,I)
          IXS(7,I)=IXS(6,I)
          IXS(6,I)=IXS(5,I)
          IXS(5,I)=IXS(2,I)
          ISOLNOD(I)=6
        ELSE
          ISOLNOD(I)=8
        ENDIF
C--------------------------------------------------
C      SUBMODEL OFFSET
C--------------------------------------------------
        IF(SUB_SOL(I) /= 0)THEN
          IF(IPARTS(I) /= 0) IPARTS(I) = IPARTS(I) + OFF_PART(SUB_SOL(I)) 
          IF(IXS(NIXS,I) /= 0) IXS(NIXS,I) = IXS(NIXS,I) + OFF_EL(SUB_SOL(I))
          DO J=2,9
            IF(IXS(J,I) /= 0) IXS(J,I) = IXS(J,I) + OFF_NOD(SUB_SOL(I))
          ENDDO
        ENDIF
C--------------------------------------------------
C      INTERNAL PART ID
C--------------------------------------------------
        IF( IPART(4,INDEX_PART) /= IPARTS(I) )THEN  
          DO J=1,NPART 
            IF(IPART(4,J)== IPARTS(I) ) INDEX_PART = J
          ENDDO  
        ENDIF
        IF( IPART(4,INDEX_PART) /= IPARTS(I) ) THEN
          CALL ANCMSG(MSGID=402,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                C1="BRICK",
     .                I1=IPARTS(I),
     .                I2=IPARTS(I),
     .                PRMOD=MSG_CUMU)
        ENDIF 
        IPARTS(I) = INDEX_PART       
        MT=IPART(1,INDEX_PART)                         
        IPID=IPART(2,INDEX_PART)               
        IXS(1,I)=MT                                
        IXS(10,I)=IPID 
C--------------------------------------------------
        DO J=2,9  
          IXS(J,I)=USR2SYS(IXS(J,I),ITABM1,MESS,IXS(11,I))
          CALL ANODSET(IXS(J,I), CHECK_VOLU)
        ENDDO  
      ENDDO            
C--------------------------------------------------
C      READING TETRA4 INPUTS IN HM STRUCTURE
C--------------------------------------------------
      CALL CPP_TETRA4_READ(IXS,NIXS,NUMBRICK,IPARTS,SUB_SOL)
C--------------------------------------------------
C      FILL OTHER STRUCTURES + CHECKS
C--------------------------------------------------
      INDEX_PART = 1
      DO N=1,NUMTETRA4
        I = I + 1
C--------------------------------------------------
C      SUBMODEL OFFSET
C--------------------------------------------------
        IF(SUB_SOL(I) /= 0)THEN
          IF(IPARTS(I) /= 0) IPARTS(I) = IPARTS(I) + OFF_PART(SUB_SOL(I)) 
          IF(IXS(NIXS,I) /= 0) IXS(NIXS,I) = IXS(NIXS,I) + OFF_EL(SUB_SOL(I))
          DO J=2,5
            IF(IXS(J,I) /= 0) IXS(J,I) = IXS(J,I) + OFF_NOD(SUB_SOL(I))
          ENDDO
        ENDIF
C--------------------------------------------------
C      INTERNAL PART ID
C--------------------------------------------------
        IF( IPART(4,INDEX_PART) /= IPARTS(I) )THEN  
          DO J=1,NPART                            
            IF(IPART(4,J)== IPARTS(I) ) INDEX_PART = J  
          ENDDO  
        ENDIF 
        IF( IPART(4,INDEX_PART) /= IPARTS(I) ) THEN
          CALL ANCMSG(MSGID=402,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                C1="TETRA4",
     .                I1=IPARTS(I),
     .                I2=IPARTS(I),
     .                PRMOD=MSG_CUMU)
        ENDIF
        IPARTS(I) = INDEX_PART 
C--------------------------------------------------      
        MT=IPART(1,INDEX_PART)                         
        IPID=IPART(2,INDEX_PART)                        
        IXS(1,I)=MT                                
        IXS(10,I)=IPID 
        DO J=2,5
          IXS(J,I)=USR2SYS(IXS(J,I),ITABM1,MESS,IXS(11,I))
          CALL ANODSET(IXS(J,I), CHECK_VOLU)
        ENDDO
        IXS(9,I)=IXS(5,I)
        IXS(8,I)=IXS(4,I)
        IXS(7,I)=IXS(4,I)
        IXS(6,I)=IXS(5,I)
        IXS(5,I)=IXS(3,I)
        IXS(4,I)=IXS(3,I)
        IXS(3,I)=IXS(2,I)
        ISOLNOD(I)=4
      ENDDO
C--------------------------------------------------
C      READING PENTA6 INPUTS IN HM STRUCTURE
C-------------------------------------------------- 
      IF (IS_DYNA ==0) CALL CPP_PENTA6_READ(IXS,NIXS,NUMBRICK+NUMTETRA4,IPARTS,SUB_SOL)
C--------------------------------------------------
C      FILL OTHER STRUCTURES + CHECKS
C--------------------------------------------------
      INDEX_PART = 1
      DO N=1,NUMPENTA6
        I = I + 1
C--------------------------------------------------
C      SUBMODEL OFFSET
C--------------------------------------------------
        IF(SUB_SOL(I) /= 0)THEN
          IF(IPARTS(I) /= 0) IPARTS(I) = IPARTS(I) + OFF_PART(SUB_SOL(I)) 
          IF(IXS(NIXS,I) /= 0) IXS(NIXS,I) = IXS(NIXS,I) + OFF_EL(SUB_SOL(I))
          DO J=2,7
            IF(IXS(J,I) /= 0) IXS(J,I) = IXS(J,I) + OFF_NOD(SUB_SOL(I))
          ENDDO
        ENDIF
C--------------------------------------------------
C      INTERNAL PART ID
C--------------------------------------------------
        IF( IPART(4,INDEX_PART) /= IPARTS(I) )THEN  
          DO J=1,NPART                            
            IF(IPART(4,J)== IPARTS(I) ) INDEX_PART = J             
          ENDDO  
        ENDIF 
        IF( IPART(4,INDEX_PART) /= IPARTS(I) ) THEN
          CALL ANCMSG(MSGID=402,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                C1="PENTA6",
     .                I1=IPARTS(I),
     .                I2=IPARTS(I),
     .                PRMOD=MSG_CUMU)
        ENDIF
        IPARTS(I) = INDEX_PART 
C--------------------------------------------------      
        MT=IPART(1,INDEX_PART)                         
        IPID=IPART(2,INDEX_PART)                   
        IXS(1,I)=MT                                
        IXS(10,I)=IPID 
        DO J=2,7
          IXS(J,I)=USR2SYS(IXS(J,I),ITABM1,MESS,IXS(11,I))
          CALL ANODSET(IXS(J,I), CHECK_VOLU)
        ENDDO
        IXS(9,I)=IXS(5,I)
        IXS(8,I)=IXS(7,I)
        IXS(7,I)=IXS(6,I)
        IXS(6,I)=IXS(5,I)
        IXS(5,I)=IXS(2,I)
        ISOLNOD(I)=6
      ENDDO
C--------------------------------------------------
C      READING TETRA10 INPUTS IN HM STRUCTURE
C-------------------------------------------------- 
      CALL CPP_TETRA10_READ(IXS,NIXS,IXS10,6,NUMBRICK+NUMTETRA4+NUMPENTA6,IPARTS,SUB_SOL)
C--------------------------------------------------
C      FILL OTHER STRUCTURES + CHECKS
C--------------------------------------------------
      INDEX_PART = 1
      I10=0
      DO N=1,NUMELS10
        I = I + 1
        I10 = I10 + 1
C--------------------------------------------------
C      SUBMODEL OFFSET
C--------------------------------------------------
        IF(SUB_SOL(I) /= 0)THEN
          IF(IPARTS(I) /= 0) IPARTS(I) = IPARTS(I) + OFF_PART(SUB_SOL(I)) 
          IF(IXS(NIXS,I) /= 0) IXS(NIXS,I) = IXS(NIXS,I) + OFF_EL(SUB_SOL(I))
          DO J=2,5
            IF(IXS(J,I) /= 0) IXS(J,I) = IXS(J,I) + OFF_NOD(SUB_SOL(I))
          ENDDO
          DO J=1,6
            IF(IXS10(J,I10) /= 0) IXS10(J,I10) = IXS10(J,I10) + OFF_NOD(SUB_SOL(I))
          ENDDO
        ENDIF
C--------------------------------------------------
C      INTERNAL PART ID
C--------------------------------------------------
        IF( IPART(4,INDEX_PART) /= IPARTS(I) )THEN  
          DO J=1,NPART                            
            IF(IPART(4,J)== IPARTS(I) ) INDEX_PART = J
          ENDDO  
        ENDIF
        IF( IPART(4,INDEX_PART) /= IPARTS(I) ) THEN
          CALL ANCMSG(MSGID=402,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                C1="TETRA10",
     .                I1=IPARTS(I),
     .                I2=IPARTS(I),
     .                PRMOD=MSG_CUMU)
        ENDIF 
        IPARTS(I) = INDEX_PART 
C--------------------------------------------------      
        MT=IPART(1,INDEX_PART)                         
        IPID=IPART(2,INDEX_PART)              
        IXS(1,I)=MT                                
        IXS(10,I)=IPID  
        DO J=2,5             
          IXS(J,I)=USR2SYS(IXS(J,I),ITABM1,MESS,IXS(11,I))
          CALL ANODSET(IXS(J,I), CHECK_VOLU)
        ENDDO
C
        DO J=1,6
          IF(IXS10(J,I10)/=0)THEN
            IXS10(J,I10)=USR2SYS(IXS10(J,I10),ITABM1,MESS,IXS(11,I))
            CALL ANODSET(IXS10(J,I10), CHECK_VOLU) 
          ENDIF
        ENDDO
C
        IXS(9,I)=IXS(5,I)
        IXS(8,I)=IXS(4,I)
        IXS(7,I)=IXS(4,I)
        IXS(6,I)=IXS(5,I)
        IXS(5,I)=IXS(3,I)
        IXS(4,I)=IXS(3,I)
        IXS(3,I)=IXS(2,I)
        ISOLNOD(I)=10   
      ENDDO
C--------------------------------------------------
C      READING BRIC20 INPUTS IN HM STRUCTURE
C--------------------------------------------------
      IF (IS_DYNA ==0) CALL CPP_BRICK20_READ(IXS,NIXS,IXS20,12,NUMBRICK+NUMTETRA4+NUMPENTA6+NUMELS10,IPARTS,SUB_SOL)
C--------------------------------------------------
C      FILL OTHER STRUCTURES + CHECKS
C--------------------------------------------------
      INDEX_PART = 1
      I20=0
      DO N=1,NUMELS20
        I = I + 1
        I20 = I20 + 1
C--------------------------------------------------
C      SUBMODEL OFFSET
C--------------------------------------------------
        IF(SUB_SOL(I) /= 0)THEN
          IF(IPARTS(I) /= 0) IPARTS(I) = IPARTS(I) + OFF_PART(SUB_SOL(I)) 
          IF(IXS(NIXS,I) /= 0) IXS(NIXS,I) = IXS(NIXS,I) + OFF_EL(SUB_SOL(I))
          DO J=2,9
            IF(IXS(J,I) /= 0) IXS(J,I) = IXS(J,I) + OFF_NOD(SUB_SOL(I))
          ENDDO
          DO J=1,12
            IF(IXS20(J,I20) /= 0) IXS20(J,I20) = IXS20(J,I20) + OFF_NOD(SUB_SOL(I))
          ENDDO
        ENDIF
C--------------------------------------------------
C      INTERNAL PART ID
C--------------------------------------------------
        IF( IPART(4,INDEX_PART) /= IPARTS(I) )THEN  
          DO J=1,NPART                            
            IF(IPART(4,J)== IPARTS(I) ) INDEX_PART = J 
          ENDDO  
        ENDIF 
        IF( IPART(4,INDEX_PART) /= IPARTS(I) ) THEN
          CALL ANCMSG(MSGID=402,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                C1="BRIC20",
     .                I1=IPARTS(I),
     .                I2=IPARTS(I),
     .                PRMOD=MSG_CUMU)
        ENDIF
        IPARTS(I) = INDEX_PART 
C--------------------------------------------------      
        MT=IPART(1,INDEX_PART)                         
        IPID=IPART(2,INDEX_PART)               
        IXS(1,I)=MT                                
        IXS(10,I)=IPID  
        DO J=2,9 
          IXS(J,I)=USR2SYS(IXS(J,I),ITABM1,MESS,IXS(11,I))
          CALL ANODSET(IXS(J,I), CHECK_VOLU)
        ENDDO
C
        DO J=1,12
          IF(IXS20(J,I20)/=0)THEN
             IXS20(J,I20)=USR2SYS(IXS20(J,I20),ITABM1,MESS,IXS(11,I))
          CALL ANODSET(IXS20(J,I20), CHECK_VOLU)
          ENDIF
        ENDDO
        ISOLNOD(I)=20
      ENDDO
C--------------------------------------------------
C      READING SHEL16 INPUTS IN HM STRUCTURE
C--------------------------------------------------
      IF (IS_DYNA ==0) CALL CPP_SHEL16_READ(IXS,NIXS,IXS16,8,NUMBRICK+NUMTETRA4+NUMPENTA6+NUMELS10+NUMELS20,IPARTS,SUB_SOL)
C--------------------------------------------------
C      FILL OTHER STRUCTURES + CHECKS
C--------------------------------------------------
      INDEX_PART = 1
      I16=0
      DO N=1,NUMELS16
        I = I + 1
        I16 = I16 + 1
C--------------------------------------------------
C      SUBMODEL OFFSET
C--------------------------------------------------
        IF(SUB_SOL(I) /= 0)THEN
          IF(IPARTS(I) /= 0) IPARTS(I) = IPARTS(I) + OFF_PART(SUB_SOL(I)) 
          IF(IXS(NIXS,I) /= 0) IXS(NIXS,I) = IXS(NIXS,I) + OFF_EL(SUB_SOL(I))
          DO J=2,9
            IF(IXS(J,I) /= 0) IXS(J,I) = IXS(J,I) + OFF_NOD(SUB_SOL(I))
          ENDDO
          DO J=1,8
            IF(IXS16(J,I16) /= 0) IXS16(J,I16) = IXS16(J,I16) + OFF_NOD(SUB_SOL(I))
          ENDDO
        ENDIF
C--------------------------------------------------
C      INTERNAL PART ID
C--------------------------------------------------
        IF( IPART(4,INDEX_PART) /= IPARTS(I) )THEN  
          DO J=1,NPART                            
            IF(IPART(4,J)== IPARTS(I) ) INDEX_PART = J 
          ENDDO  
        ENDIF 
        IF( IPART(4,INDEX_PART) /= IPARTS(I) ) THEN
          CALL ANCMSG(MSGID=402,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                C1="SHEL16",
     .                I1=IPARTS(I),
     .                I2=IPARTS(I),
     .                PRMOD=MSG_CUMU)
        ENDIF
        IPARTS(I) = INDEX_PART 
C--------------------------------------------------      
        MT=IPART(1,INDEX_PART)                         
        IPID=IPART(2,INDEX_PART)               
        IXS(1,I)=MT                                
        IXS(10,I)=IPID  
        DO J=2,9 
          IXS(J,I)=USR2SYS(IXS(J,I),ITABM1,MESS,IXS(11,I))
          CALL ANODSET(IXS(J,I), CHECK_VOLU)
        ENDDO
C
        DO J=1,8
          IF(IXS16(J,I16)/=0)THEN
             IXS16(J,I16)=USR2SYS(IXS16(J,I16),ITABM1,MESS,IXS(11,I))
          CALL ANODSET(IXS16(J,I16), CHECK_VOLU)
          ENDIF
        ENDDO
        ISOLNOD(I)=16
      ENDDO
C-----------
      CALL ANCMSG(MSGID=402,                 
     .             MSGTYPE=MSGERROR,         
     .             ANMODE=ANINFO_BLIND_1,    
     .             PRMOD=MSG_PRINT) 
C-------------------------------------
      IF (ALLOCATED(SUB_SOL)) DEALLOCATE(SUB_SOL)
C-------------------------------------
C Recherche des ID doubles
C-------------------------------------
      CALL UDOUBLE(IXS(NIXS,1),NIXS,NUMELS,MESS,0,BID)
      RETURN
      END
C
Chd|====================================================================
Chd|  LCE16S3                       source/elements/reader/hm_read_solid.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        REORDR                        source/elements/solid_2d/quad/reordr.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE LCE16S3(IXS    ,ISEL    ,PM      ,IPOINT  ,ITAB     ,ITABM1 ,
     .                   ICODE  ,IPARTS  ,IGRBRIC ,GEO     ,ISOLNOD  ,
     .                   IXS10  ,IPART   ,IXS20   ,IXS16   ,KNOD2ELS ,NOD2ELS,
     .                   IGRSURF,SPH2SOL ,SOL2SPH )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com10_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "sphcom.inc"
#include      "param_c.inc"
#include      "titr_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*), ISEL(*), IPOINT(2,*), ITAB(*), ITABM1(*),
     .   ICODE(*),IPARTS(*),ISOLNOD(*),
     .   IXS10(6,*),IPART(LIPART1,*),IXS20(12,*),IXS16(8,*),
     .   KNOD2ELS(*),NOD2ELS(*),SPH2SOL(*),SOL2SPH(2,*)
C     REAL
      my_real
     .   PM(NPROPM,*),GEO(NPROPG,*)
C-----------------------------------------------
      TYPE (GROUP_)  ,DIMENSION(NGRBRIC)  :: IGRBRIC
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, MT, MLAW, JTUR, I1, I2, INEW, K, N, IAD, NN
      INTEGER IC,IC1,IC2,IC3,IC4,MID,PID
      CHARACTER MESS*40, MESS2*40
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
C
      DATA MESS/'3D SOLID ELEMENTS DEFINITION            '/
      DATA MESS2/'3D SOLID ELEMENTS SELECTION FOR TH PLOT '/
C
C----------------------------------------------------
C     DETECTION DES ELEMENTS LOIS 6 PAROI---->LOI 17
C----------------------------------------------------
C      LOI 17 SI LA BCS EST  111 (3D)
      DO I=1,NUMELS8
        MT=IXS(1,I)
        MLAW=NINT(PM(19,MT))
        JTUR=NINT(PM(70,MT))
        DO J=2,9
          IF((MLAW==6.AND.JTUR/=0) .OR. MLAW==46) THEN
           IC=ICODE(IXS(J,I))
           IC1=IC/512
           IC2=(IC-512*IC1)/64
           IC3=(IC-512*IC1-64*IC2)/8
           IC4=(IC-512*IC1-64*IC2-8*IC3)
           IF(IC1==7 .OR. IC4==7)IXS(1,I)=-IABS(IXS(1,I))
          ENDIF
        ENDDO
      ENDDO
C----------------------------------------------------
C     CLASSEMENT DES ELEMENTS PAR LOI DE MATERIAU
C----------------------------------------------------
      CALL REORDR(IXS   ,NIXS   ,NUMELS8,PM      ,IPOINT ,
     .            IPARTS,NGRBRIC,IGRBRIC)
C------------------------------------------
C RENUMEROTATION POUR SURFACES
C------------------------------------------
      DO I=1,NSURF
        NN =IGRSURF(I)%NSEG
        DO J=1,NN
          IF (IGRSURF(I)%ELTYP(J) == 1) THEN
            IF (IGRSURF(I)%ELEM(J) <= NUMELS8)
     .        IGRSURF(I)%ELEM(J)=IPOINT(1,IGRSURF(I)%ELEM(J))
          END IF
        ENDDO
      ENDDO
C------------------------------------------
C     RENUMEROTATION DES PARTICULES SPH 
C------------------------------------------
      IF(NSPHSOL/=0)THEN
        DO I=1,NUMSPH
          IF(SPH2SOL(I)/=0)SPH2SOL(I)=IPOINT(1,SPH2SOL(I))
        ENDDO
C
C Rebuild SOL2SPH, SOL2SPH(1,N)+1<=I<=SOLSPH(2,N) <=> N==SPH2SOL(I)
        DO N=1,NUMELS8
          SOL2SPH(1,N)=0
          SOL2SPH(2,N)=0
        END DO
        N=SPH2SOL(FIRST_SPHSOL)
        SOL2SPH(1,N)=0
        SOL2SPH(2,N)=SOL2SPH(1,N)+1
        DO I=FIRST_SPHSOL+1,FIRST_SPHSOL+NSPHSOL-1
          IF(SPH2SOL(I)==N)THEN
            SOL2SPH(2,N)=SOL2SPH(2,N)+1
          ELSE
            N=SPH2SOL(I)
            SOL2SPH(1,N)=I-1
            SOL2SPH(2,N)=SOL2SPH(1,N)+1
          END IF
        END DO          
      END IF
C------------------------------------------
C     PERMUTATION DE ISOLNOD
C------------------------------------------
      DO I=1,NUMELS8
        IPOINT(2,I)=ISOLNOD(I)
      ENDDO
      DO I=1,NUMELS8
        ISOLNOD(IPOINT(1,I))=IPOINT(2,I)
      ENDDO
C------------------------------------------
C Reconstruction de la matrice Nod -> Solid elt
C------------------------------------------
      DO  K=2,9
        DO  I=1,NUMELS
          N = IXS(K,I)
          KNOD2ELS(N) = KNOD2ELS(N) + 1
          IF(N/=0) NOD2ELS(KNOD2ELS(N)) = I
        END DO
      END DO
C
      DO K=1,6
        DO I=1,NUMELS10
          N = IXS10(K,I)
          IF (N/=0) THEN
            KNOD2ELS(N) = KNOD2ELS(N) + 1
            NOD2ELS(KNOD2ELS(N)) = NUMELS8+I
          END IF
        END DO
      END DO
C
      DO K=1,12
        DO I=1,NUMELS20
          N = IXS20(K,I)
          IF (N/=0) THEN
            KNOD2ELS(N) = KNOD2ELS(N) + 1
            NOD2ELS(KNOD2ELS(N)) = NUMELS10+NUMELS8+I
          END IF
        END DO
      END DO
C
      DO K=1,8
        DO I=1,NUMELS16
          N = IXS16(K,I)
          IF (N/=0) THEN
            KNOD2ELS(N) = KNOD2ELS(N) + 1
            NOD2ELS(KNOD2ELS(N)) = NUMELS20+NUMELS10+NUMELS8+I
          END IF
        END DO
      END DO
C
      DO N=NUMNOD,1,-1
        KNOD2ELS(N+1)=KNOD2ELS(N)
      END DO
      KNOD2ELS(1)=0
C------------------------------------------
C     PRINT
C------------------------------------------
      I1=1
      I2=50
C
      IF(IPRI>=5)THEN
        WRITE (IOUT,'(//A//)') TITRE(206)
   90   CONTINUE
        I2=MIN0(I2,NUMELS8)
         WRITE (IOUT,'(//A/A//A/A,A/)')
     .        TITRE(90),TITRE(91),
     .        '   ELEMENT   INTERNAL     PART     MATER     PRSET',
     .        '     NODE1     NODE2     NODE3     NODE4     NODE5',
     .        '     NODE6     NODE7     NODE8'
         DO I=I1,I2
          INEW=IPOINT(1,I)
          WRITE (IOUT,'(5I10)')
     .      IXS(11,INEW),INEW,IPART(4,IPARTS(INEW)),
     .      IPART(5,IPARTS(INEW)),IPART(6,IPARTS(INEW))
          IF(ISOLNOD(INEW)==4)THEN
            WRITE (IOUT,'(8I10)')
     .        ITAB(IXS(2,INEW)),ITAB(IXS(4,INEW)),
     .        ITAB(IXS(7,INEW)),ITAB(IXS(6,INEW))
          ELSEIF(ISOLNOD(INEW)==6)THEN
            WRITE (IOUT,'(6I10)')
     .        ITAB(IXS(5,INEW)),ITAB(IXS(3,INEW)),ITAB(IXS(4,INEW)),
     .        ITAB(IXS(6,INEW)),ITAB(IXS(7,INEW)),ITAB(IXS(8,INEW))
          ELSE
            WRITE (IOUT,'(8I10)')
     .        (ITAB(IXS(J,INEW)),J=2,9)
          ENDIF
         ENDDO
        IF(I2==NUMELS8)GOTO 200
        I1=I1+50
        I2=I2+50
        GOTO 90
C
 200    CONTINUE
        I1=1
        I2=50
C
  290   CONTINUE
        WRITE (IOUT,'(//A/A//A/A,A/)')
     .        '            TEN NODE TETRA ELEMENTS',
     .        '            -----------------------',
     .        '   ELEMENT   INTERNAL     PART     MATER     PRSET',
     .        '     NODE1     NODE2     NODE3     NODE4     NODE5',
     .        '     NODE6     NODE7     NODE8     NODE9    NODE10'
        I2=MIN0(I2,NUMELS10)
        DO I=I1,I2
          INEW=I+NUMELS8
          WRITE (IOUT,'(5I10)')
     .      IXS(11,INEW),INEW,IPART(4,IPARTS(INEW)),
     .      IPART(5,IPARTS(INEW)),IPART(6,IPARTS(INEW))
          WRITE (IOUT,'(10I10)')
     .        ITAB(IXS(2,INEW)),ITAB(IXS(4,INEW)),
     .        ITAB(IXS(7,INEW)),ITAB(IXS(6,INEW)),
     .        (ITAB(IXS10(J,I)),J=1,6)
        ENDDO
        IF(I2==NUMELS10)GOTO 300
        I1=I1+50
        I2=I2+50
        GOTO 290
C
 300    CONTINUE
        I1=1
        I2=50
C
        DOWHILE(I1<=NUMELS20)
         WRITE (IOUT,'(//A/A//A/A,A/A/A)')
     .        '            TWENTY NODE BRICK ELEMENTS',
     .        '            --------------------------',
     .        ' ELEMENT INTERNAL   PART   MATER   PRSET',
     .        '   NODE1   NODE2   NODE3   NODE4   NODE5',
     .        '   NODE6   NODE7   NODE8',
     .        '   NODE9  NODE10  NODE11  NODE12  NODE13  NODE14',
     .        '  NODE15  NODE16  NODE17  NODE18  NODE19  NODE20'
         I2=MIN0(I2,NUMELS20)
         DO I=I1,I2
          INEW=I+NUMELS8+NUMELS10
          WRITE (IOUT,'(5I10)')
     .      IXS(11,INEW),INEW,IPART(4,IPARTS(INEW)),
     .      IPART(5,IPARTS(INEW)),IPART(6,IPARTS(INEW))
          WRITE (IOUT,'(8I10/6I10/6I10)')
     .        (ITAB(IXS(J,INEW)),J=2,9),
     .        (ITAB(IXS20(J,I)),J=1,12)
         ENDDO
         I1=I1+50
         I2=I2+50
        ENDDO
        I1=1
        I2=50
C
        DOWHILE(I1<=NUMELS16)
         WRITE (IOUT,'(//A/A//A/A,A/A,A)')
     .        '            SIXTEEN NODE SHELL ELEMENTS',
     .        '            ---------------------------',
     .        ' ELEMENT INTERNAL   PART   MATER   PRSET',
     .        '   NODE1   NODE2   NODE3   NODE4   NODE5',
     .        '   NODE6   NODE7   NODE8',
     .        '   NODE9  NODE10  NODE11  NODE12  NODE13  NODE14',
     .        '  NODE15  NODE16'
         I2=MIN0(I2,NUMELS16)
         DO I=I1,I2
          INEW=I+NUMELS8+NUMELS10+NUMELS20
          WRITE (IOUT,'(5I10)')
     .      IXS(11,INEW),INEW,IPART(4,IPARTS(INEW)),
     .      IPART(5,IPARTS(INEW)),IPART(6,IPARTS(INEW))
          WRITE (IOUT,'(8I10/8I10)')
     .        (ITAB(IXS(J,INEW)),J=2,9),
     .        (ITAB(IXS16(J,I)),J=1,8)
         ENDDO
         I1=I1+50
         I2=I2+50
        ENDDO
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  LCE16S4                       source/elements/reader/hm_read_solid.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE LCE16S4(IXS,PM,ICODE)
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com10_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "param_c.inc"
#include      "titr_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*), ICODE(*)
C     REAL
      my_real
     .   PM(NPROPM,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, MT, MLAW, JTUR
      INTEGER IC,IC1,IC2,IC3,IC4
C-----------------------------------------------
C     DETECTION DES ELEMENTS LOIS 6 PAROI---->LOI 17
C----------------------------------------------------
C     LOI 17 SI LA BCS EST  111 (3D)
      DO I=1,NUMELS8
        MT=IXS(1,I)
        IF(MT < 1)CYCLE
        MLAW=NINT(PM(19,MT))
        JTUR=NINT(PM(70,MT))
        DO J=2,9
          IF((MLAW==6.AND.JTUR/=0) .OR. MLAW==46) THEN
           IC=ICODE(IXS(J,I))
           IC1=IC/512
           IC2=(IC-512*IC1)/64
           IC3=(IC-512*IC1-64*IC2)/8
           IC4=(IC-512*IC1-64*IC2-8*IC3)
           IF(IC1==7 .OR. IC4==7)IXS(1,I)=-IABS(IXS(1,I))
          ENDIF
        ENDDO
      ENDDO
C
      RETURN
      END
C
Chd|====================================================================
Chd|  HM_PRELCE16S                  source/elements/reader/hm_read_solid.F
Chd|-- called by -----------
Chd|        CONTRL                        source/starter/contrl.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_PRELCE16S(IPART   ,IGEO    ,IXS     ,NSPHSOL ,LSUBMODEL,
     .                        IS_DYNA )
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
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPART(LIPART1,*), IGEO(NPROPGI,*), IXS(NIXS,*), NSPHSOL
C     REAL
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
      INTEGER,INTENT(IN)::IS_DYNA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, IT, NT
      INTEGER IPID,ID,IDS,N,NSPHDIR,STAT
      INTEGER FLAG_FMT,FLAG_FMT_TMP,IFIX_TMP,INDEX_PART,OFF_DEF(NSUBMOD),
     .        OFF_NOD(NSUBMOD),OFF_EL(NSUBMOD),OFF_PART(NSUBMOD)
      my_real
     .   BID 
      CHARACTER MESS*40
      INTEGER, DIMENSION(:), ALLOCATABLE :: IPARTS
      INTEGER, DIMENSION(:), ALLOCATABLE :: SUB_SOL
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
C
      DATA MESS/'3D SOLID ELEMENTS DEFINITION            '/
C=======================================================================
        NSPHSOL=0
C
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
      ALLOCATE (SUB_SOL(NUMELS),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='SUB_SOL')
      SUB_SOL(1:NUMELS) = 0
      INDEX_PART = 1
C--------------------------------------------------
C      READING BRICKS INPUTS IN HM STRUCTURE
C--------------------------------------------------  
      ALLOCATE(IPARTS(NUMELS))
      CALL CPP_BRICK_READ(IXS,NIXS,IPARTS,SUB_SOL) 
C--------------------------------------------------
C      NSPHSOL NSPHDIR CALCULATION + CHECKS
C--------------------------------------------------
      DO I=1,NUMBRICK
C--------------------------------------------------
C      SUBMODEL OFFSET
C--------------------------------------------------
        IF(SUB_SOL(I) /= 0)THEN
          IF(IPARTS(I) /= 0) IPARTS(I) = IPARTS(I) + OFF_PART(SUB_SOL(I)) 
          IF(IXS(NIXS,I) /= 0) IXS(NIXS,I) = IXS(NIXS,I) + OFF_EL(SUB_SOL(I))
          DO J=2,9
            IF(IXS(J,I) /= 0) IXS(J,I) = IXS(J,I) + OFF_NOD(SUB_SOL(I))
          ENDDO
        ENDIF
C--------------------------------------------------
C      INTERNAL PART ID
C--------------------------------------------------
        IF( IPART(4,INDEX_PART) /= IPARTS(I) )THEN  
          DO J=1,NPART                            
            IF(IPART(4,J)== IPARTS(I) )INDEX_PART = J             
          ENDDO
        ENDIF 
        IPARTS(I) = INDEX_PART 
C--------------------------------------------------
        IF (IXS(11,I)>ID_LIMIT) THEN
          CALL ANCMSG(MSGID=509,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I1=IXS(11,I),C1=LINE,C2='/SOLID')
        ENDIF
        NSPHDIR=IGEO(37,IPART(2,IPARTS(I)))
        IF(IXS(6,I)+IXS(7,I)+IXS(8,I)+IXS(9,I)==0)THEN
          NT=0
          DO IT=1,NSPHDIR
            NT=NT+IT
            NSPHSOL=NSPHSOL+NT
          END DO
        ELSEIF(IXS(8,I)+IXS(9,I)==0)THEN
          NT=0
          DO IT=1,NSPHDIR
            NT=NT+IT
          END DO
          NSPHSOL=NSPHSOL+NSPHDIR*NT
        ELSE
          NSPHSOL=NSPHSOL+NSPHDIR*NSPHDIR*NSPHDIR
        ENDIF
      ENDDO
C
        FLAG_FMT = 0
        I=0
C--------------------------------------------------
C      READING TETRA4s INPUTS IN HM STRUCTURE
C-------------------------------------------------- 
      CALL CPP_TETRA4_READ(IXS,NIXS,NUMBRICK,IPARTS,SUB_SOL)
C--------------------------------------------------
C      NSPHSOL NSPHDIR CALCULATION + CHECKS
C--------------------------------------------------
        DO I=NUMBRICK+1,NUMBRICK+NUMTETRA4
C--------------------------------------------------
C      SUBMODEL OFFSET
C--------------------------------------------------
          IF(SUB_SOL(I) /= 0)THEN
            IF(IPARTS(I) /= 0) IPARTS(I) = IPARTS(I) + OFF_PART(SUB_SOL(I)) 
            IF(IXS(NIXS,I) /= 0) IXS(NIXS,I) = IXS(NIXS,I) + OFF_EL(SUB_SOL(I))
            DO J=2,5
              IF(IXS(J,I) /= 0) IXS(J,I) = IXS(J,I) + OFF_NOD(SUB_SOL(I))
            ENDDO
          ENDIF
C--------------------------------------------------
C      INTERNAL PART ID
C--------------------------------------------------
          IF( IPART(4,INDEX_PART) /= IPARTS(I) )THEN  
            DO J=1,NPART                            
              IF(IPART(4,J)== IPARTS(I) )INDEX_PART = J             
            ENDDO  
          ENDIF 
          IPARTS(I) = INDEX_PART 
C--------------------------------------------------
C
          NSPHDIR=IGEO(37,IPART(2,IPARTS(I)))
          IF (IXS(11,I)>ID_LIMIT) THEN
            CALL ANCMSG(MSGID=509,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                  I1=IXS(11,I),C1=LINE,C2='/SOLID')
          ENDIF
          NT=0
          DO IT=1,NSPHDIR
            NT=NT+IT
            NSPHSOL=NSPHSOL+NT
          END DO
        ENDDO
C--------------------------------------------------
C      READING PENTA6s INPUTS IN HM STRUCTURE
C-------------------------------------------------- 
      IF (IS_DYNA ==0) CALL CPP_PENTA6_READ(IXS,NIXS,NUMBRICK+NUMTETRA4,IPARTS,SUB_SOL)
C--------------------------------------------------
C      NSPHSOL NSPHDIR CALCULATION + CHECKS
C--------------------------------------------------
        DO I=NUMBRICK+NUMTETRA4+1,NUMBRICK+NUMTETRA4+NUMPENTA6
C--------------------------------------------------
C      SUBMODEL OFFSET
C--------------------------------------------------
          IF(SUB_SOL(I) /= 0)THEN
            IF(IPARTS(I) /= 0) IPARTS(I) = IPARTS(I) + OFF_PART(SUB_SOL(I)) 
            IF(IXS(NIXS,I) /= 0) IXS(NIXS,I) = IXS(NIXS,I) + OFF_EL(SUB_SOL(I))
            DO J=2,7
              IF(IXS(J,I) /= 0) IXS(J,I) = IXS(J,I) + OFF_NOD(SUB_SOL(I))
            ENDDO
          ENDIF
C--------------------------------------------------
C      INTERNAL PART ID
C--------------------------------------------------
          IF( IPART(4,INDEX_PART) /= IPARTS(I) )THEN  
            DO J=1,NPART                            
              IF(IPART(4,J)== IPARTS(I) )INDEX_PART = J             
            ENDDO  
          ENDIF 
          IPARTS(I) = INDEX_PART 
C--------------------------------------------------
          NSPHDIR=IGEO(37,IPARTS(I-NUMBRICK))
          IF (IXS(11,I)>ID_LIMIT) THEN
            CALL ANCMSG(MSGID=509,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                  I1=IXS(11,I),C1=LINE,C2='/SOLID')
          ENDIF
          NT=0
          DO IT=1,NSPHDIR
            NT=NT+IT
            NSPHSOL=NSPHSOL+NT
          END DO
        ENDDO
      IF (ALLOCATED(IPARTS)) DEALLOCATE(IPARTS)
      IF (ALLOCATED(SUB_SOL)) DEALLOCATE(SUB_SOL)
C--------------------------
      RETURN
      END
