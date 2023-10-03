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
Chd|  HM_READ_MAT49                 source/materials/mat/mat049/hm_read_mat49.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT49(
     .           LSUBMODEL ,MTAG     ,UNITAB   ,IPM    ,PM,
     .           MAT_ID    ,TITR     ,EOS_TAG  ,ISRATE )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFTAG_MOD            
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ MAT LAW49 WITH HM READER
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
#include      "sysunit.inc"
#include      "submod_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(INOUT)                       :: MAT_ID
      INTEGER, INTENT(INOUT)                       :: ISRATE
      INTEGER, DIMENSION(NPROPMI) ,INTENT(INOUT)   :: IPM
      CHARACTER*nchartitle ,INTENT(IN)             :: TITR
      my_real, DIMENSION(NPROPM)  ,INTENT(INOUT)   :: PM
      my_real, DIMENSION(LUNIT,NUNITS) ,INTENT(IN) :: UNITAB
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD),INTENT(IN) :: LSUBMODEL
      TYPE(MLAW_TAG_), INTENT(INOUT)               :: MTAG
      TYPE(EOS_TAG_) , TARGET, DIMENSION(0:MAXEOS) ,INTENT(INOUT) :: EOS_TAG   
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED,IS_EOS, EOS_EMBEDDED
      INTEGER JTUR, I, EMPTY_LINE,COUNT,IEOS,LAW6_OPT
      INTEGER JTHE, IFLAGR
      my_real
     .    YOUNG,NU,SIG0,CB,CN,SIGM,EPSM,T0,TMELT,PMIN,
     .    SPH,CB1,CB2,CH,CF,G0,BULK,
     .    FAC_L,FAC_T,FAC_M,FAC_C,
     .   BID2 ,BID3 ,BID4 ,BID5 ,
     .   BID2_,BID3_,BID4_,BID5_,
     .   C0,C1,C2,C3,C4,C5,E0,PSH,RHO0,RHOR
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      COUNT = 0
      IS_EOS=.FALSE.
      IEOS=0
      EOS_EMBEDDED=.FALSE.
      BULK=ZERO
      
      BULK = ZERO
      PSH = ZERO
      
      IS_ENCRYPTED=.FALSE.
      IS_AVAILABLE=.FALSE.
            
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)

      CALL HM_GET_FLOATV('MAT_RHO'        ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho'      ,RHOR     ,IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_FLOATV('MAT_E0'         ,YOUNG    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'         ,NU       ,IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_FLOATV('MAT_SIGY'       ,SIG0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_BETA'       ,CB       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_HARD'       ,CN       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS'        ,EPSM     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SIG'        ,SIGM     ,IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_FLOATV('MAT_T0'         ,T0       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_TMELT'      ,TMELT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SPHEAT'     ,SPH      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_PC'         ,PMIN     ,IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_FLOATV('MAT_B1'         ,CB1      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_B2'         ,CB2      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('h'              ,CH       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_F'          ,CF       ,IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_INTV  ('Line_count'     ,COUNT    ,IS_AVAILABLE, LSUBMODEL)

      IF(INVERS_INIT<=51)THEN
        EOS_EMBEDDED=.TRUE.
      ENDIF          


        IF(EOS_EMBEDDED)THEN
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
            PM(88)= ZERO 
            IEOS = 1 ! POLYNOMIAL EOS         
        ENDIF

      IF (PMIN==ZERO) PMIN=-EP20
      IF (RHOR==ZERO) RHOR=RHO0
      PM(1) = RHOR
      PM(89)= RHO0     

      IF(CF < ZERO) THEN
            CALL ANCMSG(MSGID=1513,MSGTYPE=MSGERROR,ANMODE=ANINFO,
     .                  I1 = MAT_ID,C1 = TITR)
      ENDIF
      IF(NU<ZERO.OR.NU>=HALF)THEN
            CALL ANCMSG(MSGID=1514,MSGTYPE=MSGERROR,ANMODE=ANINFO,
     .                  I1 = MAT_ID,C1 = TITR)
      ENDIF
      IF(EPSM==ZERO) EPSM  = EP20
      IF(SIGM==ZERO) SIGM  = EP20
      IF(TMELT==ZERO)TMELT = EP20
      IF(PMIN==ZERO) PMIN  =-EP20
      IF(T0==ZERO)   T0    = THREE100
      G0 = YOUNG /TWO / (ONE+NU)
      IF(.NOT.EOS_EMBEDDED)BULK=YOUNG/(THREE*(ONE - TWO*NU))
C
      PM(20) = YOUNG 
      PM(21) = NU
      PM(22) = G0
      PM(24) = NU * YOUNG / (ONE-NU*NU)
      PM(32) = BULK
      PM(37) = PMIN
      PM(38) = SIG0
      PM(39) = CB
      PM(40) = CN
      PM(41) = EPSM
      PM(42) = SIGM
      PM(43) = CB1
      PM(44) = CB2
      PM(45) = CH
      PM(46) = TMELT
      PM(69) = SPH
      PM(77) = CF
      PM(78) = T0
      PM(79) = T0
      PM(80) = TMELT
C-----------
C     Formulation for solid elements time step computation.
      IPM(252)= 2
      PM(105) = TWO*G0/(BULK+FOUR_OVER_3*G0) ! =(1-2*Nu)/(1-Nu)
C
      WRITE(IOUT,1000)          
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,2001) TITR,MAT_ID,49
        WRITE(IOUT,2002) RHO0,RHOR  
        WRITE(IOUT,1100)YOUNG,SIG0,NU,CB,CN,SIGM,EPSM,
     .                T0,TMELT,SPH,PMIN,CB1,CB2,CH,CF
      ENDIF
      !--------------------------------------------------!
      ! OLD FORMAT 100 (OBSOLETE)                        !
      !--------------------------------------------------!
      IF(EOS_EMBEDDED)THEN                    !
        WRITE(IOUT,2000)                                 !      
        IF(IS_ENCRYPTED)THEN                              !
          WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'     !
        ELSE                                             !
          WRITE(IOUT,2500)C0,C1,C2,C3,C4,C5,E0,PM(1),PSH !
        ENDIF                                            !
      ENDIF                                              !
      !--------------------------------------------------!      
c
C---- Definition des variables internes (stockage elementaire)
c
      MTAG%G_PLA   = 1    
      MTAG%G_TEMP  = 1    
c
      MTAG%L_PLA   = 1    ! NB10
      MTAG%L_TEMP  = 1    ! NB11
      MTAG%L_EPSD  = 1    ! NB12
C--------------------------------
 1000 FORMAT(
     & 5X,'   STEINBERG-GUINAN CONSTITUTIVE LAW   ',/,
     & 5X,'  ----------------------------------   ',//)
 1100 FORMAT(
     & 5X,'INITIAL YOUNG MODULUS . . . . . . . . .=',1PG20.13/
     & 5X,'INITIAL YIELD STRESS. . . . . . . . . .=',1PG20.13/
     & 5X,'POISSON RATIO . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'HARDENING COEFFICIENT . . . . . . . . .=',1PG20.13/
     & 5X,'HARDENING EXPONENT. . . . . . . . . . .=',1PG20.13/
     & 5X,'MAXIMUM STRESS. . . . . . . . . . . . .=',1PG20.13/
     & 5X,'MAXIMUM STRAIN. . . . . . . . . . . . .=',1PG20.13/
     & 5X,'INITIAL TEMPERATURE . . . . . . . . . .=',1PG20.13/
     & 5X,'MELTING TEMPERATURE . . . . . . . . . .=',1PG20.13/
     & 5X,'SPECIFIC HEAT . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'MINIMAL PRESSURE. . . . . . . . . . . .=',1PG20.13/
     & 5X,'B1 COEFFICIENT. . . . . . . . . . . . .=',1PG20.13/
     & 5X,'B2 COEFFICIENT. . . . . . . . . . . . .=',1PG20.13/
     & 5X,'H COEFFICIENT . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'F COEFFICIENT . . . . . . . . . . . . .=',1PG20.13//)
C-----------
 2000 FORMAT(
     & 5X,'  POLYNOMIAL EOS                       ',/,
     & 5X,'  --------------                       ',/,   
     & 5X,'    INPUT 100 IS OBSOLETE              ',/,         
     & 5X,'    IT CAN BE REPLACED BY /EOS CARD    ',/)
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
      RETURN
      END
