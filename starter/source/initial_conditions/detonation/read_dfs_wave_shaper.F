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
Chd|  READ_DFS_WAVE_SHAPER          source/initial_conditions/detonation/read_dfs_wave_shaper.F
Chd|-- called by -----------
Chd|        READ_DETONATORS               source/initial_conditions/detonation/read_detonators.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_CRYPTED          source/devtools/hm_reader/hm_option_is_crypted.F
Chd|        IFRONTPLUS                    source/spmd/node/frontplus.F  
Chd|        REMOVE_DET                    source/initial_conditions/detonation/remove_det.F
Chd|        NODGRNR5                      source/starter/freform.F      
Chd|        DETONATORS_MOD                share/modules1/detonators_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE READ_DFS_WAVE_SHAPER(DET_ID,DETONATORS,X,IGRNOD,IPM,ITABM1,UNITAB,LSUBMODEL,KEY,KEY2,I1,I2,I3,I4,I5,
     .                                N1,NPEM,ITAB)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE DETONATORS_MOD
      USE GROUPDEF_MOD
      USE HM_OPTION_READ_MOD
      USE SUBMODEL_MOD
      USE DETONATORS_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "sysunit.inc"
#include      "tabsiz_c.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN)      :: UNITAB 
      INTEGER                           :: ITABM1(SITABM1),ITAB(NUMNOD)
      INTEGER                           :: IPM(NPROPMI,NUMMAT)    
      my_real                           :: X(3,NUMNOD)
      CHARACTER*ncharkey                :: KEY, KEY2      
      TYPE(SUBMODEL_DATA),INTENT(IN)    :: LSUBMODEL(NSUBMOD)
      INTEGER            ,INTENT(INOUT) :: I1,I2,I3,I4,I5 
      INTEGER,INTENT(IN)                :: DET_ID 
      INTEGER,INTENT(INOUT)             :: N1   ,NPEM 
      TYPE(DETONATOR_STRUCT_),TARGET    :: DETONATORS      
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER              :: I, MAT, J, K,IGU,IGS,JJ,MDET
      INTEGER              :: IBID,  NODE_ID1, NODE_ID2,uID1,uID2, IOPT, IUNIT, UID
      INTEGER              :: FLAG_FMT,IMAT,IFLAGUNIT,UNUSED
      INTEGER              :: STAT,NPE
      my_real              :: XC, YC, ZC, ALT, XC1, YC1, ZC1, XC2, YC2, ZC2, NX, NY, NZ, BID, VCJ
      CHARACTER*40         :: MESS
      CHARACTER*64         :: chain1,chain2
      CHARACTER*nchartitle :: TITR
      LOGICAL              :: IS_ENCRYPTED, IS_AVAILABLE
      INTEGER,POINTER,DIMENSION(:)  :: IECRAN
      my_real,POINTER,DIMENSION(:)  :: DECRAN     
      INTEGER              :: NDETPS,NDETSG,NECRAN,NDETPL,NDETCORD
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER,EXTERNAL     :: NODGRNR5, USR2SYS, NINTRI
      INTEGER              :: UNUSED_MAT_DETONATOR
      DATA MESS/'DETONATORS DEFINITION                   '/      
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------      
      NDETPS = DETONATORS%NDETPS
      NDETSG = DETONATORS%NDETSG
      NECRAN = DETONATORS%NECRAN
      NDETPL = DETONATORS%NDETPL
      NDETCORD = DETONATORS%NDETCORD

      IECRAN => DETONATORS%IECRAN(1:)
      DECRAN => DETONATORS%DECRAN(1:)

      IS_ENCRYPTED= .FALSE.                                                                  
      IS_AVAILABLE = .FALSE. 
      CALL HM_OPTION_IS_CRYPTED(IS_ENCRYPTED)  
      !---------------------------------!                                                    
      !             READING             !                                                    
      !---------------------------------!                                                    
      CALL HM_GET_FLOATV('rad_det_locationA_X', XC1, IS_AVAILABLE, LSUBMODEL, UNITAB)       
      CALL HM_GET_FLOATV('rad_det_locationA_Y', YC1, IS_AVAILABLE, LSUBMODEL, UNITAB)       
      CALL HM_GET_FLOATV('rad_det_locationA_Z', ZC1, IS_AVAILABLE, LSUBMODEL, UNITAB)       
      CALL HM_GET_FLOATV('rad_det_time', ALT, IS_AVAILABLE, LSUBMODEL,UNITAB)              
      CALL HM_GET_INTV('rad_det_materialid', MAT, IS_AVAILABLE, LSUBMODEL)                 
      CALL HM_GET_INTV('entityid', IGU, IS_AVAILABLE, LSUBMODEL)                 
      !---------------------------------!                                                    
      !      CHECKING USER FLAGS        !                                                    
      !      +INTERNAL ID               !                                                    
      !---------------------------------!                                                    
      MDET=MAT !bak                                                                          
      IF (ALT.GT.INFINITY) ALT=INFINITY                                                      
      IF(MAT.GT.0)UNUSED=UNUSED_MAT_DETONATOR(MAT,NUMMAT,IPM) 
      UNUSED=0                               
      IF (MAT.LT.0) THEN                                                                     
        CALL REMOVE_DET(KEY(1:7),I1,I2,I3,I4,I5,DETONATORS)      
        CALL ANCMSG(MSGID=102,                                                               
     .              MSGTYPE=MSGERROR,                                                        
     .              ANMODE=ANINFO,                                                           
     .              I1=DET_ID,                                                                   
     .              C1='DETONATOR IS REFERRING TO A NEGATIVE MATERIAL ID',                   
     .              C2='/DFS/WAV_SHAPER',                                                    
     .              I2=MDET)                                                                 
      ELSEIF (UNUSED==1) THEN                                                                
        CALL REMOVE_DET(KEY(1:7),I1,I2,I3,I4,I5,DETONATORS)      
        CALL ANCMSG(MSGID=102,                                                               
     .              MSGTYPE=MSGERROR,                                                        
     .              ANMODE=ANINFO,                                                           
     .              I1=DET_ID,                                                                   
     .              C1='DETONATOR IS REFERRING TO AN UNKNOWN MATERIAL ID',                   
     .              C2='/DFS/WAV_SHAPER',                                                    
     .              I2=MDET)                                                                 
      ELSEIF (UNUSED==2) THEN                                                                
        CALL REMOVE_DET(KEY(1:7),I1,I2,I3,I4,I5,DETONATORS)      
        CALL ANCMSG(MSGID=102,                                                               
     .              MSGTYPE=MSGERROR,                                                        
     .              ANMODE=ANINFO,                                                           
     .              I1=DET_ID,                                                                   
     .              C1='DETONATOR MUST REFER TO A JWL MATERIAL LAW (LAWS 5 OR 51)',          
     .              C2='/DFS/WAV_SHAPER',                                                    
     .              I2=MDET)                                                                 
      ELSE                                                                                   
      !---------------------------------!                                                    
      !        LISTING PRINTOUT         !                                                    
      !---------------------------------!                                                    
        I3=I3+1                                                                              
        NPE = NODGRNR5(IGU ,IGS,IECRAN(N1),IGRNOD ,ITABM1 ,MESS) 
              !Nodes in group are ordered from 1 to NPE. last point is the nearest from the detonation origin. Points are composing the screen lines (guard lines). Screen lines is the boundary of the obstacle (shadow area)
        DETONATORS%NPE=NPE   
        IF(IS_ENCRYPTED) WRITE(IOUT,1001)                    
        IF(.NOT.IS_ENCRYPTED)WRITE(IOUT,1550) DET_ID,XC1,YC1,ZC1,ALT,MDET,IGU,NPE                                     
        NPEM=MAX0(NPE,NPEM)                                                                  
        IF(.NOT.IS_ENCRYPTED)WRITE(IOUT,FMT=FMW_10I) (ITAB(IECRAN(I)),I=N1,N1+NPE-1)                              
        DO I=N1,N1+NPE-1                                                                     
          CALL IFRONTPLUS(IECRAN(I),1)                                                       
        END DO                                                                               
        DETONATORS%BURN(1,I3) = ALT                                                                        
        DETONATORS%BURN(2,I3) = MAT                                                                        
        DETONATORS%BURN(3,I3) = XC1                                                                        
        DETONATORS%BURN(4,I3) = YC1                                                                        
        DETONATORS%BURN(5,I3) = ZC1                                                                        
        DETONATORS%BURN(6,I3) = NPE                                                                        
        N1      = N1+NPE                                                                     
        DETONATORS%BURN(7,I3) = N1                                                                         
        DETONATORS%BURN(8,I3) = 0 !unused                                                                  
      END IF                                                                                 
            
C-----------------------------------------------
C   O u t p u t   F o r m a t
C-----------------------------------------------
 1001 FORMAT(///'SHADOW LINE DETONATION    ',I10,/5X,
     &          '----------------------    ',/5X,
     &          'CONFIDENTIAL DATA')    
 1550 FORMAT(///'SHADOW LINE DETONATION    =',I10,/5X,
     &          '----------------------     ',/5X,
     &          'X-COORDINATE              =',1PG20.13,/5X,
     &          'Y-COORDINATE              =',1PG20.13,/5X,
     &          'Z-COORDINATE              =',1PG20.13,/5X,
     &          'LIGHTING TIME             =',1PG20.13,/5X,
     &          'EXPLOSIVE MATERIAL NUMBER =',I10/
     &          'SHADOW LINE NODE GROUP ID =',I10/
     &          'NUMBER OF POINTS(SHADOW)  =',I10/
     &          'SHADOW LINE DEFINITION :   ')
           
C-----------------------------------------------
      END SUBROUTINE
