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
Chd|  READ_DFS_DETCORD              source/initial_conditions/detonation/read_dfs_detcord.F
Chd|-- called by -----------
Chd|        READ_DETONATORS               source/initial_conditions/detonation/read_detonators.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        DETCORD0                      source/initial_conditions/detonation/detcord0.F
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
      SUBROUTINE READ_DFS_DETCORD(DET_ID,DETONATORS,X,IGRNOD,IPM,ITABM1,UNITAB,LSUBMODEL,KEY,KEY2,I1,I2,I3,I4,I5,N2,NPCM,ITAB)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE DETONATORS_MOD
      USE GROUPDEF_MOD
      USE HM_OPTION_READ_MOD
      USE SUBMODEL_MOD
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
      INTEGER,INTENT(IN)                :: ITABM1(SITABM1),ITAB(NUMNOD)
      INTEGER,INTENT(IN)                :: IPM(NPROPMI,NUMMAT)
      my_real                           :: X(3,NUMNOD)
      CHARACTER*ncharkey                :: KEY, KEY2      
      TYPE(SUBMODEL_DATA),INTENT(IN)    :: LSUBMODEL(NSUBMOD)
      INTEGER            ,INTENT(INOUT) :: I1,I2,I3,I4,I5
      INTEGER,INTENT(IN)                :: DET_ID
      INTEGER,INTENT(INOUT)             :: N2,NPCM
      TYPE(DETONATOR_STRUCT_)           :: DETONATORS
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER              :: I, MAT, J, NPEM, K,IGU,IGS,JJ,MDET
      INTEGER              :: IBID,  NODE_ID1, NODE_ID2,uID1,uID2, IOPT, IUNIT, UID
      INTEGER              :: FLAG_FMT,IMAT,IFLAGUNIT,UNUSED
      INTEGER              :: STAT,NPE,NPE2
      my_real              :: XC, YC, ZC, ALT, XC1, YC1, ZC1, XC2, YC2, ZC2, NX, NY, NZ, BID, VCJ
      CHARACTER*40         :: MESS
      CHARACTER*64         :: chain1,chain2
      CHARACTER*nchartitle :: TITR
      LOGICAL              :: IS_ENCRYPTED, IS_AVAILABLE
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

      IS_ENCRYPTED= .FALSE.
      IS_AVAILABLE = .FALSE.     
      CALL HM_OPTION_IS_CRYPTED(IS_ENCRYPTED)   
      !---------------------------------!                                                     
      !             READING             !                                                     
      !---------------------------------!                                                     
      CALL HM_GET_FLOATV('magnitude', VCJ, IS_AVAILABLE, LSUBMODEL, UNITAB)        
      CALL HM_GET_FLOATV('rad_det_time', ALT, IS_AVAILABLE, LSUBMODEL, UNITAB)        
      CALL HM_GET_INTV('rad_det_iopt', IOPT, IS_AVAILABLE, LSUBMODEL)        
      CALL HM_GET_INTV('rad_det_materialid', MAT, IS_AVAILABLE, LSUBMODEL)        
      CALL HM_GET_INTV('entityid', IGU, IS_AVAILABLE, LSUBMODEL)        
                                                                                              
      !---------------------------------!                                                             
      !      CHECKING USER FLAGS        !                                                             
      !      +INTERNAL ID               !                                                             
      !---------------------------------!                                                             
      MDET=MAT !bak                                                                                   
      IF (ALT.GT.INFINITY) ALT=INFINITY                                                               
      NPE2 = NODGRNR5(IGU ,IGS,DETONATORS%ICORD(N2),IGRNOD ,ITABM1 ,MESS)   
      DETONATORS%NPE2 = NPE2                                     
      IF(IGRNOD(IGS)%SORTED /= 1)THEN                                                                 
          CALL ANCMSG(MSGID   = 104,                                                                  
     .                MSGTYPE = MSGERROR,                                                             
     .                ANMODE  = ANINFO,                                                               
     .                C1      = '/DFS/DETCORD',                                                       
     .                I1      = DET_ID,                                                                   
     .                C2      = 'ORDERED GROUP OF NODES IS REQUIRED')                                 
      ENDIF   
      UNUSED = 0                                                                                        
      IF(MAT.GT.0)UNUSED=UNUSED_MAT_DETONATOR(MAT,NUMMAT,IPM)                                         
      IF (MAT.LT.0) THEN                                                                              
        CALL REMOVE_DET(KEY(1:7),I1,I2,I3,I4,I5,DETONATORS)               
        CALL ANCMSG(MSGID=102,                                                                        
     .              MSGTYPE=MSGERROR,                                                                 
     .              ANMODE=ANINFO,                                                                    
     .              I1=DET_ID,                                                                            
     .              C1='DETONATOR IS REFERRING TO A NEGATIVE MATERIAL ID',                            
     .              C2='/DFS/DETCORD',                                                                
     .              I2=MDET)                                                                          
      ELSEIF (UNUSED==1) THEN                                                                         
        CALL REMOVE_DET(KEY(1:7),I1,I2,I3,I4,I5,DETONATORS)               
        CALL ANCMSG(MSGID=102,                                                                        
     .              MSGTYPE=MSGERROR,                                                                 
     .              ANMODE=ANINFO,                                                                    
     .              I1=DET_ID,                                                                            
     .              C1='DETONATOR IS REFERRING TO AN UNKNOWN MATERIAL ID',                            
     .              C2='/DFS/DETCORD',                                                                
     .              I2=MDET)                                                                          
      ELSEIF (UNUSED==2) THEN                                                                         
        CALL REMOVE_DET(KEY(1:7),I1,I2,I3,I4,I5,DETONATORS)               
        CALL ANCMSG(MSGID=102,                                                                        
     .              MSGTYPE=MSGERROR,                                                                 
     .              ANMODE=ANINFO,                                                                    
     .              I1=DET_ID,                                                                            
     .              C1='DETONATOR MUST REFER TO A JWL MATERIAL LAW (LAWS 5 OR 51)',                   
     .              C2='/DFS/DETCORD',                                                                
     .              I2=MDET)                                                                          
      ELSEIF (NPE2.EQ.0) THEN                                                                         
        CALL REMOVE_DET(KEY(1:7),I1,I2,I3,I4,I5,DETONATORS)               
      ELSE                                                                                            
        !---------------------------------!                                                           
        !        LISTING PRINTOUT         !                                                           
        !---------------------------------!                                                           
        !prevoir le cas NNOD=0 ou NNOD =1                                                             
        I5   = I5+1                                                                                   
        !IOPT=0  : def=3                                                                              
        !IOPT=1  : Vdet  - multiple segments (experimental / osbslete)                                
        !IOPT=2  : instantaneous - multiple segments (experimental / obsolete)                        
        !IOPT=3  : Centripetal-Catmull-Rom SPLINE interpolation + projection along neutral fiber
        IF(IS_ENCRYPTED) WRITE(IOUT,1001)
        IF(IOPT==0)IOPT=3                                                                             
        IF(IOPT==2)THEN                                                                               
          IF(.NOT.IS_ENCRYPTED)WRITE(IOUT,1700) DET_ID,IGU,NPE2,ALT,MDET                                                       
        ELSEIF(IOPT==1)THEN                                                                           
          IF(.NOT.IS_ENCRYPTED)WRITE(IOUT,1701) DET_ID,IGU,NPE2,ALT,VCJ,MDET                                                   
        ELSEIF(IOPT==3)THEN                                                                           
          IF(.NOT.IS_ENCRYPTED)WRITE(IOUT,1700) DET_ID,IGU,NPE2,ALT,MDET                                                       
        ENDIF                                                                                         
        NPCM=MAX0(NPE2,NPCM)                                                                          
        IF(.NOT.IS_ENCRYPTED)WRITE(IOUT,FMT=FMW_10I) (ITAB(DETONATORS%ICORD(I)),I=N2,N2+NPE2-1)   
        DO I=N2,N2+NPE2-1                                                                             
          CALL IFRONTPLUS(DETONATORS%ICORD(I),1)                                                                 
        END DO                                                                                        
        IF(IOPT==1 .AND. VCJ.LE.ZERO)IOPT=2                                                           
        CALL DETCORD0(DETONATORS,ITAB,N2,ALT,X,VCJ,IOPT)                                                         
        if(IOPT/=3 .AND. .NOT.IS_ENCRYPTED)WRITE(IOUT,FMT='(100F30.16)') ((DETONATORS%DCORD(I)),I=N2,N2+NPE2-1)                          
        !---------------------------------!                                                           
        DETONATORS%BURN(1,I5) = ALT                                                                                 
        DETONATORS%BURN(2,I5) = MAT                                                                                 
        DETONATORS%BURN(3,I5) = VCJ                                                                                 
        DETONATORS%BURN(4,I5) = IOPT                                                                                
        DETONATORS%BURN(5,I5) = ZERO                                                                                
        DETONATORS%BURN(6,I5) = NPE2                                                                                
        N2      = N2+NPE2                                                                             
        DETONATORS%BURN(7,I5) = N2                                                                                  
        DETONATORS%BURN(8,I5) = 0 !unused                                                                           
      END IF                                                                                          
C-----------------------------------------------
C   O u t p u t   F o r m a t
C-----------------------------------------------      
 1001 FORMAT(///'DETONATING CORD           ',I10,/5X,
     &          '---------------           ',/5X,
     &          'CONFIDENTIAL DATA')    
 1700 FORMAT(///'DETONATING CORD            ',I10,/5X,
     &          '----------------           ',/5X,
     &          'NODE GROUP IDENTIFIER     =',I10     ,/5X,
     &          'NUMBER OF POINTS(CORD)    =',I10     ,/5X,         
     &          'LIGHTING TIME             =',1PG20.13,/5X,
     &          'EXPLOSIVE MATERIAL NUMBER =',I10     ,/5X,
     &          'CORD DEFINITION :          ')     
 1701 FORMAT(///'DETONATING CORD            ',I10,/5X,
     &          '----------------           ',/5X,
     &          'NODE GROUP IDENTIFIER     =',I10     ,/5X,
     &          'NUMBER OF POINTS(CORD)    =',I10     ,/5X,         
     &          'LIGHTING TIME             =',1PG20.13,/5X,
     &          'DETONATION VELOCITY       =',1PG20.13,/5X,     
     &          'EXPLOSIVE MATERIAL NUMBER =',I10     ,/5X,
     &          'CORD DEFINITION :          ')     
C-----------------------------------------------      
                 
      END SUBROUTINE
