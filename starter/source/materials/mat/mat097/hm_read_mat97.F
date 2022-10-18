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
Chd|  HM_READ_MAT97                 source/materials/mat/mat097/hm_read_mat97.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT97(UPARAM ,MAXUPARAM,NUPARAM  ,ISRATE   , IMATVIS  ,
     .                         NUVAR  ,IFUNC    ,MAXFUNC  ,NFUNC    , PARMAT   , 
     .                         UNITAB ,MAT_ID   ,TITR     ,MTAG     , LSUBMODEL,
     .                         PM     ,IPM      )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C   READ MAT LAW97 WITH HM READER ( TO BE COMPLETED )
C
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME            DESCRIPTION                         
C
C     PM              MATERIAL ARRAY(REAL)
C     UNITAB          UNITS ARRAY
C     MAT_ID          MATERIAL ID(INTEGER)
C     TITR            MATERIAL TITLE
C     LSUBMODEL       SUBMODEL STRUCTURE   
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD            
      USE MESSAGE_MOD      
      USE SUBMODEL_MOD
      USE MATPARAM_DEF_MOD          
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      my_real, INTENT(INOUT)                :: PM(NPROPM),PARMAT(100),UPARAM(MAXUPARAM)
      INTEGER, INTENT(INOUT)                :: IPM(NPROPMI),ISRATE,IFUNC(MAXFUNC),NFUNC,MAXFUNC,MAXUPARAM,NUPARAM, NUVAR,IMATVIS
      TYPE(MLAW_TAG_),INTENT(INOUT)         :: MTAG
      INTEGER,INTENT(IN)                    :: MAT_ID
      CHARACTER*nchartitle,INTENT(IN)       :: TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)        :: LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .     W, D, PCJ, E0, P0, C1, VCJ,C,
     .     EADD, TBEGIN, TEND, PSH,RHOR,RHO0,
     .     A(5),
     .     R(5),
     .     AL(5),
     .     BL(5),
     .     RL(5)
      INTEGER :: IBFRAC, I  
      LOGICAL IS_AVAILABLE,IS_ENCRYPTED         
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      IMATVIS = 0
      ISRATE=0

      NUVAR         = 5

      !======== BUFFER ALLOCATION SIZES
      MTAG%G_TB     = 1  
      MTAG%G_TEMP   = 1  
      MTAG%G_BFRAC  = 1
      MTAG%G_ABURN  = 1
      MTAG%L_TB     = 1
      MTAG%L_TEMP   = 1
      MTAG%L_BFRAC  = 1 
      MTAG%L_ABURN  = 1  
      MTAG%L_SSP   = 1
      MTAG%L_EINT  = 1
      MTAG%G_EINT  = 1
      MTAG%L_VK    = 1

      !======== INITIALIZATION
      IBFRAC        = 0
      PSH           = ZERO
      P0            = ZERO
      D             = ZERO
      PCJ           = ZERO
      E0            = ZERO
      W             = ZERO
      C             = ZERO
      A(1:5)        = ZERO
      R(1:5)        = ZERO
      AL(1:5)       = ZERO
      BL(1:5)       = ZERO
      RL(1:5)       = ZERO
                   
      !======== READING INPUT FILE ===========!

      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
      !line-1
      CALL HM_GET_FLOATV('MAT_RHO'   ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho' ,RHOR     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-2
      CALL HM_GET_FLOATV('MAT_P0'    , P0      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_PSH'   , PSH     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV('MAT_IBFRAC'  ,IBFRAC   ,IS_AVAILABLE, LSUBMODEL)
      !line-3
      CALL HM_GET_FLOATV('MLAW97_D'    , D     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MLAW97_PCJ'  , PCJ   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MLAW97_E0'   , E0    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Omega'       , W     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MLAW97_C'    , C     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-4
      CALL HM_GET_FLOATV('MLAW97_A1'   ,A(1)   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MLAW97_A2'   ,A(2)   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MLAW97_A3'   ,A(3)   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MLAW97_A4'   ,A(4)   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MLAW97_A5'   ,A(5)   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-5
      CALL HM_GET_FLOATV('MLAW97_R1'   ,R(1)   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MLAW97_R2'   ,R(2)   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MLAW97_R3'   ,R(3)   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MLAW97_R4'   ,R(4)   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MLAW97_R5'   ,R(5)   ,IS_AVAILABLE, LSUBMODEL, UNITAB)      
      !line-6
      CALL HM_GET_FLOATV('MLAW97_AL1'  ,AL(1)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MLAW97_AL2'  ,AL(2)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MLAW97_AL3'  ,AL(3)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MLAW97_AL4'  ,AL(4)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MLAW97_AL5'  ,AL(5)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)      
      !line-7
      CALL HM_GET_FLOATV('MLAW97_BL1'  ,BL(1)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MLAW97_BL2'  ,BL(2)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MLAW97_BL3'  ,BL(3)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MLAW97_BL4'  ,BL(4)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MLAW97_BL5'  ,BL(5)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)      
      !line-8
      CALL HM_GET_FLOATV('MLAW97_RL1'  ,RL(1)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MLAW97_RL2'  ,RL(2)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MLAW97_RL3'  ,RL(3)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MLAW97_RL4'  ,RL(4)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MLAW97_RL5'  ,RL(5)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)      

      !========== DEFAULT VALUES=============!              
      IF(RHOR==ZERO)RHOR=RHO0
      PM(1) =RHOR
      PM(89)=RHO0

      !========== PARAMETER CHECK============!              
      IF(W==ZERO)THEN
          CALL ANCMSG(
     .                MSGID   = 77          ,
     .                MSGTYPE = MSGERROR    ,
     .                ANMODE  = ANINFO_BLIND,
     .                I1      = MAT_ID          
     .                )        
      ENDIF  
      
      DO I=1,5
       IF (A(I)==ZERO)THEN
         R(I)=ONE
       ENDIF
       IF(R(I)==ZERO)THEN
          CALL ANCMSG(
     .                MSGID   = 76          ,
     .                MSGTYPE = MSGERROR    ,
     .                ANMODE  = ANINFO_BLIND,
     .                I1      = MAT_ID          
     .                )        
       ENDIF
      ENDDO   
      
      !========== PARAMETER BACKUP===========! 
      UPARAM(01)    = P0
      UPARAM(02)    = PSH
      UPARAM(03)    = IBFRAC
      UPARAM(04)    = D
      UPARAM(05)    = PCJ
      UPARAM(06)    = E0
      UPARAM(07)    = W
      UPARAM(08)    = C
      UPARAM(09:13) = A(1:5)
      UPARAM(14:18) = R(1:5)
      UPARAM(19:23) = AL(1:5)
      UPARAM(24:28) = BL(1:5)
      UPARAM(29:33) = RL(1:5)      
      UPARAM(34)    = RHO0*D**2/PCJ
      VCJ           = ONE-ONE/UPARAM(34)
      UPARAM(35)    = VCJ
      NUPARAM       = 35
      NFUNC         = 0
      PARMAT(1)     = W*(PCJ+E0)  !RHO0*D**2
      PM(38)        = D      
      
      !P0
      PM(31)=P0
      PM(104)=P0
      
      !SSP0
      PM(27)=D                  

      !======== LISTING OUTPUT
      WRITE(IOUT,1100) TRIM(TITR),MAT_ID,97 
      WRITE(IOUT,1000)    
      IF(IS_ENCRYPTED)THEN
         WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
         WRITE(IOUT,1200) RHO0
         WRITE(IOUT,1400) W,C,D,PCJ,VCJ,E0,P0,PSH,IBFRAC 
         WRITE(IOUT,1300) A(1:5)   
         WRITE(IOUT,1301) R(1:5)
         WRITE(IOUT,1302)AL(1:5)
         WRITE(IOUT,1303)BL(1:5)         
         WRITE(IOUT,1304)RL(1:5)  
      ENDIF
     

C-----------------------------------------------
      RETURN 
C-----------------------------------------------


 1000 FORMAT(
     &     5X,'  J.W.L.B. EXPLOSIVE                  ',/,
     &     5X,'  ------------------                  ',//)
 1100 FORMAT(/
     &     5X,A,/,
     &     5X,'MATERIAL NUMBER . . . . . . . . . . . .=',I10/,
     &     5X,'MATERIAL LAW. . . . . . . . . . . . . .=',I10/)
 1200 FORMAT(
     &     5X,'INITIAL DENSITY . . . . . . . . . . . .=',1PG20.13/)
 1300 FORMAT(
     &     5X,'A1. . . . . . . . . . . . . . . . . . .=',E12.4/,
     &     5X,'A2. . . . . . . . . . . . . . . . . . .=',E12.4/,
     &     5X,'A3. . . . . . . . . . . . . . . . . . .=',E12.4/,
     &     5X,'A4. . . . . . . . . . . . . . . . . . .=',E12.4/,
     &     5X,'A5. . . . . . . . . . . . . . . . . . .=',E12.4//)
 1301 FORMAT(
     &     5X,'R1. . . . . . . . . . . . . . . . . . .=',E12.4/,
     &     5X,'R2. . . . . . . . . . . . . . . . . . .=',E12.4/,
     &     5X,'R3. . . . . . . . . . . . . . . . . . .=',E12.4/,
     &     5X,'R4. . . . . . . . . . . . . . . . . . .=',E12.4/,
     &     5X,'R5. . . . . . . . . . . . . . . . . . .=',E12.4//)
 1302 FORMAT(
     &     5X,'AL1 . . . . . . . . . . . . . . . . . .=',E12.4/,
     &     5X,'AL2 . . . . . . . . . . . . . . . . . .=',E12.4/,
     &     5X,'AL3 . . . . . . . . . . . . . . . . . .=',E12.4/,
     &     5X,'AL4 . . . . . . . . . . . . . . . . . .=',E12.4/,
     &     5X,'AL5 . . . . . . . . . . . . . . . . . .=',E12.4//)
 1303 FORMAT(
     &     5X,'BL1 . . . . . . . . . . . . . . . . . .=',E12.4/,
     &     5X,'BL2 . . . . . . . . . . . . . . . . . .=',E12.4/,
     &     5X,'BL3 . . . . . . . . . . . . . . . . . .=',E12.4/,
     &     5X,'BL4 . . . . . . . . . . . . . . . . . .=',E12.4/,
     &     5X,'BL5 . . . . . . . . . . . . . . . . . .=',E12.4//)
 1304 FORMAT(
     &     5X,'RL1 . . . . . . . . . . . . . . . . . .=',E12.4/,
     &     5X,'RL2 . . . . . . . . . . . . . . . . . .=',E12.4/,
     &     5X,'RL3 . . . . . . . . . . . . . . . . . .=',E12.4/,
     &     5X,'RL4 . . . . . . . . . . . . . . . . . .=',E12.4/,
     &     5X,'RL5 . . . . . . . . . . . . . . . . . .=',E12.4//)     
 1400 FORMAT(
     &     5X,'OMEGA . . . . . . . . . . . . . . . . .=',E12.4/,
     &     5X,'C PARAMETER . . . . . . . . . . . . . .=',E12.4/,
     &     5X,'DETONATION VELOCITY . . . . . . . . . .=',E12.4/,
     &     5X,'CHAPMAN JOUGUET PRESSURE. . . . . . . .=',E12.4/,
     &     5X,'CHAPMAN JOUGUET VOLUME. . . . . . . . .=',E12.4/,
     &     5X,'INITIAL ENERGY PER UNIT VOLUME. . . . .=',E12.4/,  
     &     5X,'INITIAL PRESSURE. . . . . . . . . . . .=',E12.4/,
     &     5X,'PRESSURE SHIFT. . . . . . . . . . . . .=',E12.4/,  
     &     5X,'BURN FRACTION METHOD. . . . . . . . . .=',I10/)



      END SUBROUTINE HM_READ_MAT97
