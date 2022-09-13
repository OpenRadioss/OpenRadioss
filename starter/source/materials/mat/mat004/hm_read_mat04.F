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
Chd|  HM_READ_MAT04                 source/materials/mat/mat004/hm_read_mat04.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_CRYPTED          source/devtools/hm_reader/hm_option_is_crypted.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT04(
     .           LSUBMODEL ,MTAG     ,UNITAB   ,IPM    ,PM,
     .           MAT_ID    ,TITR     ,EOS_TAG  ,MAXEOS ,ISRATE      )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFTAG_MOD            
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ MAT LAW04 WITH HM READER
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C     UNITAB          UNITS ARRAY
C     MAT_ID          MATERIAL ID(INTEGER)
C     TITR            MATERIAL TITLE
C     LSUBMODEL       SUBMODEL STRUCTURE    
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "scr03_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "sysunit.inc"
#include      "submod_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(INOUT)                       :: MAT_ID
      INTEGER, INTENT(IN)                          :: MAXEOS
      INTEGER, INTENT(INOUT)                       :: ISRATE
      INTEGER, DIMENSION(NPROPMI) ,INTENT(INOUT)   :: IPM
      CHARACTER*nchartitle ,INTENT(IN)             :: TITR
      my_real, DIMENSION(NPROPM)  ,INTENT(INOUT)   :: PM
      my_real, DIMENSION(LUNIT,NUNITS) ,INTENT(IN) :: UNITAB
      TYPE(SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL
      TYPE(MLAW_TAG_), INTENT(INOUT)               :: MTAG
      TYPE(EOS_TAG_) , TARGET, DIMENSION(0:MAXEOS) ,INTENT(OUT) :: EOS_TAG    
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: IS_AVAILABLE,IS_CRYPTED,IS_EOS, EOS_EMBEDDED
      INTEGER JTUR, I, EMPTY_LINE,COUNT,IEOS,LAW6_OPT
      INTEGER JTHE, IFLAGR
      my_real
     .   YOUNG, ANU, CA, CB, CN, EPSM, SIGM, BULK, 
     .   PMIN, CC, EPS0, CM, TMELT, TMAX, CS, T0, SPH, AS, BS,
     .   AL, BL, G, TBA, TBK, PCO, FAC_L, FAC_T, FAC_M, FAC_C,
     .   BID2 ,BID3 ,BID4 ,BID5 ,
     .   BID2_,BID3_,BID4_,BID5_,
     .   C0,C1,C2,C3,C4,C5,E0,PSH,TI,RHO0,RHOR
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      COUNT = 0
      IS_EOS=.FALSE.
      IEOS=0
      EOS_EMBEDDED=.FALSE.
      BULK=ZERO
      ISRATE = 0
!---
      CALL HM_OPTION_IS_CRYPTED(IS_CRYPTED)

      CALL HM_GET_FLOATV('MAT_RHO'        ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho'      ,RHOR     ,IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_FLOATV('MAT_E'          ,YOUNG    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'         ,ANU      ,IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_FLOATV('MAT_SIGY'       ,CA       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_BETA'       ,CB       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_HARD'       ,CN       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS'        ,EPSM     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SIG'        ,SIGM     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      
      CALL HM_GET_FLOATV('MAT_PC'         ,PMIN     ,IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_FLOATV('MAT_SRC'        ,CC       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SRP'        ,EPS0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_M'          ,CM       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_TMELT'      ,TMELT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_TMAX'       ,TMAX     ,IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_FLOATV('MAT_SPHEAT'     ,CS       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_T0'         ,T0       ,IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_INTV  ('Line_count'     ,COUNT    ,IS_AVAILABLE, LSUBMODEL)

      IF(INVERS_INIT>=2018)THEN
        EOS_EMBEDDED=.FALSE.
      ELSEIF(INVERS_INIT>=110)THEN
        IF(COUNT==3)EOS_EMBEDDED=.TRUE.
      ELSE
        EOS_EMBEDDED=.TRUE.
      ENDIF
        
        !----------------------------------------------------                                                          
        !CHECK IF OLD CARD FORMAT 100 IS USED (OBSOLETE)                                                               
        ! MAKES IT  COMPATIBLE + WARNING                                                                               
        !----------------------------------------------------                                                          
      IF(EOS_EMBEDDED)THEN                                                                          
        IF(INVERS_INIT>=140) THEN                                                                 
          CALL ANCMSG(MSGID=1072,                                                                   
     .                MSGTYPE=MSGERROR,                                                             
     .                ANMODE=ANINFO,                                                                
     .                I1 = MAT_ID,                                                                  
     .                C1 = TITR)                                                                    
          RETURN                                                                                    
        ENDIF                                                                                       
        CALL HM_GET_FLOATV('MAT_C0'       ,C0       ,IS_AVAILABLE, LSUBMODEL, UNITAB)               
        CALL HM_GET_FLOATV('MAT_C1'       ,C1       ,IS_AVAILABLE, LSUBMODEL, UNITAB)               
        CALL HM_GET_FLOATV('MAT_C2'       ,C2       ,IS_AVAILABLE, LSUBMODEL, UNITAB)               
        CALL HM_GET_FLOATV('MAT_C3'       ,C3       ,IS_AVAILABLE, LSUBMODEL, UNITAB)               
        CALL HM_GET_FLOATV('MAT_C4'       ,C4       ,IS_AVAILABLE, LSUBMODEL, UNITAB)               
        CALL HM_GET_FLOATV('MAT_C5'       ,C5       ,IS_AVAILABLE, LSUBMODEL, UNITAB)               
        CALL HM_GET_FLOATV('MAT_E0'       ,E0       ,IS_AVAILABLE, LSUBMODEL, UNITAB)               
        CALL HM_GET_FLOATV('MAT_PSH'      ,PSH      ,IS_AVAILABLE, LSUBMODEL, UNITAB)               
        BULK  = C1                                                                                  
        PM(23)= E0                                                                                  
        PM(31)= C0-PSH                                                                              
        PM(32)= C1                                                                                  
        PM(33)= C2                                                                                  
        PM(34)= C3                                                                                  
        PM(35)= C4                                                                                  
        PM(36)= C5                                                                                  
        PM(88)= PSH                                                                                 
        IEOS = 1 ! POLYNOMIAL EOS    
        IPM(4) = IEOS
       ENDIF                                                                                        
     
      IF (PMIN==ZERO) PMIN=-EP20
      IF (RHOR==ZERO) RHOR=RHO0
      PM(1) = RHOR
      PM(89)= RHO0      
 
   
      IF(CS == ZERO.AND.TMELT == ZERO)THEN
         CALL ANCMSG(MSGID=593,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=MAT_ID,
     .               C1=TITR)
         CS=EP20
      ELSEIF(CS == ZERO)THEN
         CALL ANCMSG(MSGID=594,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=MAT_ID,
     .               C1=TITR)
      END IF
      SPH=CS
      IF(PMIN == ZERO) PMIN  =-EP20
      IF(CN == ZERO.OR.CN == ONE)   CN = ONEP0001
      IF(EPSM == ZERO) EPSM  = EP20
      IF(SIGM == ZERO) SIGM  = EP20
      IF(CC == ZERO)   EPS0  = ONE
      IF(CM == ZERO)   CM    = ONE
      IF(TMELT == ZERO)TMELT = EP20
      IF(TMAX == ZERO) TMAX  = EP20
      IF(T0<=ZERO) T0    = THREE100
      G=YOUNG/(TWO*(ONE + ANU))
      IF(.NOT.EOS_EMBEDDED)BULK=YOUNG/(THREE*(ONE - TWO*ANU))
C
      PM(20)=YOUNG
      PM(21)=ANU
      PM(22)=G
      PM(32)=BULK
      PM(37)=PMIN
      PM(38)=CA
      PM(39)=CB
      PM(40)=CN
      PM(41)=EPSM
      PM(42)=SIGM
      PM(43)=CC
      PM(44)=EPS0
      PM(45)=CM
      PM(46)=TMELT
      PM(47)=TMAX
      PM(48)=CS
      PM(69)=SPH
      PM(79)=T0
      PM(80)=TMELT
CC-----------
C     Formulation for solid elements time step computation.
      IPM(252)= 2
      PM(105) =  (ONE -TWO*ANU)/(ONE - ANU) ! TWO*G/(BULK + FOUR_OVER_3*G)       
C----- Definition des variables internes (stockage elementaire)
      MTAG%G_PLA  = 1
      MTAG%G_TEMP = 1
      MTAG%G_EPSD = 1
c
      MTAG%L_PLA  = 1
      MTAG%L_TEMP = 1
      MTAG%L_EPSD = 1   
      
C--------------------------------
      WRITE(IOUT,1000)

      IF(IS_CRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,2001) TITR,MAT_ID,4
        WRITE(IOUT,2002) RHO0,RHOR           
        WRITE(IOUT,1300)YOUNG,ANU,G,BULK
        WRITE(IOUT,1400)CA,CB,CN,EPSM,SIGM
        WRITE(IOUT,1500)PMIN
        WRITE(IOUT,1600)CC,EPS0,CM,TMELT,TMAX,CS,T0
      ENDIF
      
      !--------------------------------------------------!
      ! OLD FORMAT 100 (OBSOLETE)                        !
      !--------------------------------------------------!
      IF(EOS_EMBEDDED)THEN                               !
        WRITE(IOUT,2000)                                 !      
        IF(IS_CRYPTED)THEN                              !
          WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'     !
        ELSE                                             !
          WRITE(IOUT,2500)C0,C1,C2,C3,C4,C5,E0,PM(1),PSH !
        ENDIF                                            !
      ENDIF                                              !
      !--------------------------------------------------!
C
      IF(EPS0 == ZERO) THEN
         CALL ANCMSG(MSGID=298,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=4,
     .               I2=MAT_ID,
     .               C1=TITR)
      ENDIF
      RETURN
 1000 FORMAT(
     & 5X,'  JOHNSON COOK LAW    ',/,
     & 5X,'  ----------------    ',//)
 1300 FORMAT(
     & 5X,'YOUNG',1H','S MODULUS . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'POISSON',1H','S RATIO . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SHEAR MODULUS . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'BULK  MODULUS . . . . . . . . . . . . .=',1PG20.13//)
 1400 FORMAT(
     & 5X,'YIELD COEFFICIENT CA. . . . . . . . . .=',1PG20.13/,
     & 5X,'YIELD COEFFICIENT CB. . . . . . . . . .=',1PG20.13/,
     & 5X,'YIELD COEFFICIENT CN. . . . . . . . . .=',1PG20.13/,
     & 5X,'EPS-MAX . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SIG-MAX . . . . . . . . . . . . . . . .=',1PG20.13//)
 1500 FORMAT(
     & 5X,'PRESSURE CUTOFF . . . . . . . . . . . .=',1PG20.13/)
 1600 FORMAT(
     & 5X,'STRAIN RATE COEFFICIENT CC. . . . . . .=',1PG20.13/,
     & 5X,'REFERENCE STRAIN RATE . . . . . . . . .=',1PG20.13/,
     & 5X,'TEMPERATURE EXPONENT. . . . . . . . . .=',1PG20.13/,
     & 5X,'MELTING TEMPERATURE DEGREE K. . . . . .=',1PG20.13/,
     & 5X,'THETA-MAX . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SPECIFIC HEAT . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'ROOM TEMPERATURE . . . . . . . . . . . =',1PG20.13//)
 2000 FORMAT(
     & 5X,'  POLYNOMIAL EOS                       ',/,
     & 5X,'  --------------                       ',/,   
     & 5X,'    INPUT 100 IS OBSOLETE              ',/,         
     & 5X,'    IT CAN BE ERASED BY /EOS CARD      ',/)
 2001 FORMAT(/
     & 5X,A,/,
     & 5X,'   MATERIAL NUMBER . . . . . . . . . . . .=',I10/,
     & 5X,'   MATERIAL LAW. . . . . . . . . . . . . .=',I10/)
 2002 FORMAT(
     &  5X,'  INITIAL DENSITY . . . . . . . . . . . .=',1PG20.13/,
     &  5X,'  REFERENCE DENSITY . . . . . . . . . . .=',1PG20.13/)
 2500 FORMAT(
     & 5X,'C0. . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'C1. . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'C2. . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'C3. . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'C4. . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'C5. . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'INITIAL INTERNAL ENERGY PER UNIT VOLUME .=',1PG20.13/,
     & 5X,'REFERENCE DENSITY . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'PRESSURE SHIFT. . . . . . . . . . . . . .=',1PG20.13//)     
 999  CALL FREERR(3)
      RETURN
      END



    

