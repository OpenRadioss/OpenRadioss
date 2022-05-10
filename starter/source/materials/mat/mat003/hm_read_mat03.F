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
Chd|  HM_READ_MAT03                 source/materials/mat/mat003/hm_read_mat03.F
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
      SUBROUTINE HM_READ_MAT03(
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
C   READ MAT LAW03 WITH HM READER
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
      my_real
     .   YOUNG, ANU, CA, CB, CN, EPSM, SIGM, BULK,
     .   PMIN, E0, G, FAC_L, FAC_T, FAC_M, FAC_C,
     .   BID2 ,BID3 ,BID4 ,BID5 ,
     .   BID2_,BID3_,BID4_,BID5_,
     .   C0,C1,C2,C3,C4,C5,PSH,RHO0,RHOR
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      COUNT = 0
      IS_EOS=.FALSE.
      IEOS=0
      EOS_EMBEDDED=.FALSE.
      BULK=ZERO
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

      CALL HM_GET_INTV  ('Line_count'     ,COUNT    ,IS_AVAILABLE, LSUBMODEL)
      
      IF(INVERS_INIT.GE.2018)THEN
        EOS_EMBEDDED=.FALSE.
      ELSEIF(INVERS_INIT.GE.110)THEN
        IF(COUNT==3)EOS_EMBEDDED=.TRUE.
      ELSE
        EOS_EMBEDDED=.TRUE.
      ENDIF
        
        !----------------------------------------------------                                                          
        !CHECK IF OLD CARD FORMAT 100 IS USED (OBSOLETE)                                                               
        ! MAKES IT  COMPATIBLE + WARNING                                                                               
        !----------------------------------------------------                                                          
      IF(EOS_EMBEDDED)THEN                                                                          
        IF(INVERS_INIT.GE.140) THEN                                                                 
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
       ENDIF                                                                                        
     
      IF (PMIN.EQ.ZERO) PMIN=-EP20
      IF (RHOR.EQ.ZERO) RHOR=RHO0
      PM(1) = RHOR
      PM(89)= RHO0      
      IF(CN.EQ.ZERO.OR.CN.EQ.UN)   CN = UNP0001
      IF(EPSM.EQ.ZERO) EPSM  = EP20
      IF(SIGM.EQ.ZERO) SIGM  = EP20
      G    = YOUNG/(DEUX*(UN +ANU))
      IF(.NOT.EOS_EMBEDDED)BULK = YOUNG/(TROIS*(UN - DEUX*ANU))
      IF(.NOT.EOS_EMBEDDED)E0   = ZERO

      PM(20)=YOUNG
      PM(21)=ANU
      PM(22)=G
      PM(23)=E0
      PM(32)=BULK
      PM(37)=PMIN
      PM(38)=CA
      PM(39)=CB
      PM(40)=CN
      PM(41)=EPSM
      PM(42)=SIGM
      PM(79)=TROIS100
      !Formulation for solid elements time step computation.
      IPM(252)= 2
      PM(105) =  (UN -DEUX*ANU)/(UN - ANU) ! DEUX*G/(BULK + QTHIRD*G)

      MTAG%G_PLA   = 1   
      MTAG%L_PLA   = 1   
      MTAG%G_TEMP  = 1
      MTAG%L_TEMP  = 1

      WRITE(IOUT,1000)          

      IF(IS_CRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,2001) TITR,MAT_ID,3
        WRITE(IOUT,2002) RHO0,RHOR 
        WRITE(IOUT,1300)YOUNG,ANU,G,BULK
        WRITE(IOUT,1400)CA,CB,CN,EPSM,SIGM
        WRITE(IOUT,1500)PMIN
      ENDIF
      !--------------------------------------------------!
      ! OLD FORMAT 100 (OBSOLETE)                        !
      !--------------------------------------------------!
      IF(EOS_EMBEDDED)THEN                    !
        WRITE(IOUT,2000)                                 !      
        IF(IS_CRYPTED)THEN 
              WRITE(IOUT,2001) TITR,MAT_ID,6
          WRITE(IOUT,2002) RHO0,RHOR           
                             !
          WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'     !
        ELSE      
                                                 !
          WRITE(IOUT,2500)C0,C1,C2,C3,C4,C5,E0,PM(1),PSH !
        ENDIF                                            !
      ENDIF                                              !
      !--------------------------------------------------!      
C-----------
      RETURN
 1000 FORMAT(
     & 5X,'  ELASTIC PLASTIC HYDRODYNAMIC    ',/,
     & 5X,'  ----------------------------    ',//)
 1300 FORMAT(
     & 5X,'YOUNG',1H','S MODULUS . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'POISSON',1H','S RATIO . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SHEAR MODULUS . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'BULK  MODULUS . . . . . . . . . . . . . .=',1PG20.13//)
 1400 FORMAT(
     & 5X,'YIELD COEFFICIENT CA. . . . . . . . . .=',1PG20.13/,
     & 5X,'YIELD COEFFICIENT CB. . . . . . . . . .=',1PG20.13/,
     & 5X,'YIELD COEFFICIENT CN. . . . . . . . . .=',1PG20.13/,
     & 5X,'EPS-MAX . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SIG-MAX . . . . . . . . . . . . . . . .=',1PG20.13//)
 1500 FORMAT(
     & 5X,'PRESSURE CUTOFF . . . . . . . . . . . .=',1PG20.13//)
 2000 FORMAT(
     & 5X,'  POLYNOMIAL EOS                       ',/,
     & 5X,'  --------------                       ',/,   
     & 5X,'    INPUT 100 IS OBSOLETE              ',/,         
     & 5X,'    IT CAN BE ERASED BY /EOS CARD      ',/)
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
 2001 FORMAT(/
     & 5X,A,/,
     & 5X,'   MATERIAL NUMBER . . . . . . . . . . . .=',I10/,
     & 5X,'   MATERIAL LAW. . . . . . . . . . . . . .=',I10/)
 2002 FORMAT(
     &  5X,'  INITIAL DENSITY . . . . . . . . . . . .=',1PG20.13/,
     &  5X,'  REFERENCE DENSITY . . . . . . . . . . .=',1PG20.13/)

 999  CALL FREERR(3)
      RETURN
      END




