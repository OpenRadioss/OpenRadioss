Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
      !||====================================================================
      !||    hm_read_mat49            ../starter/source/materials/mat/mat049/hm_read_mat49.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                   ../starter/source/output/message/message.F
      !||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
      !||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
      !||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
      !||    init_mat_keyword         ../starter/source/materials/mat/init_mat_keyword.F
      !||--- uses       -----------------------------------------------------
      !||    elbuftag_mod             ../starter/share/modules1/elbuftag_mod.F
      !||    message_mod              ../starter/share/message_module/message_mod.F
      !||    submodel_mod             ../starter/share/modules1/submodel_mod.F
      !||====================================================================
      SUBROUTINE HM_READ_MAT49(
     .           LSUBMODEL ,MTAG     ,UNITAB   ,IPM    ,PM       ,
     .           MAT_ID    ,TITR     ,ISRATE ,MATPARAM )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFTAG_MOD            
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE MATPARAM_DEF_MOD          
      USE UNITAB_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
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
#include      "units_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(INOUT)                       :: MAT_ID
      INTEGER, INTENT(INOUT)                       :: ISRATE
      INTEGER, DIMENSION(NPROPMI) ,INTENT(INOUT)   :: IPM
      CHARACTER(LEN=NCHARTITLE) ,INTENT(IN)             :: TITR
      my_real, DIMENSION(NPROPM)  ,INTENT(INOUT)   :: PM
      TYPE(UNIT_TYPE_), INTENT(in) :: UNITAB
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD),INTENT(IN) :: LSUBMODEL
      TYPE(MLAW_TAG_), INTENT(INOUT)               :: MTAG
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
      INTEGER JTUR, I, IEOS,EMPTY_LINE,COUNT
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
      IEOS = 0
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

      !Default EoS
      IEOS   = 18 ! Linear EOS is used by default
      MATPARAM%IEOS = IEOS
      IPM(4)        = IEOS    ! keep this temporarily for output and fluid sections

      IF (PMIN==ZERO) PMIN=-EP20
      IF (RHOR==ZERO) RHOR=RHO0
      PM(1) = RHOR
      PM(89)= RHO0     

      IF(CF < ZERO) THEN
        CALL ANCMSG(MSGID=1513,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1 = MAT_ID,C1 = TITR)
      ENDIF
      
      IF(NU<ZERO.OR.NU>=HALF)THEN
        CALL ANCMSG(MSGID=1514,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1 = MAT_ID,C1 = TITR)
      ENDIF
      
      IF(EPSM==ZERO) EPSM  = EP20
      IF(SIGM==ZERO) SIGM  = EP20
      IF(TMELT==ZERO)TMELT = EP20
      IF(PMIN==ZERO) PMIN  =-EP20
      IF(T0==ZERO)   T0    = THREE100
      G0 = YOUNG /TWO / (ONE+NU)
      BULK=YOUNG/(THREE*(ONE - TWO*NU))

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
                
      !Formulation for solid elements time step computation.
      IPM(252)= 2
      PM(105) = TWO*G0/(BULK+FOUR_OVER_3*G0) ! =(1-2*Nu)/(1-Nu)

      ! MATPARAM keywords
      CALL INIT_MAT_KEYWORD(MATPARAM,"INCOMPRESSIBLE")

      ! Material compatibility with /EOS option
      CALL INIT_MAT_KEYWORD(MATPARAM,"EOS")

      ! EOS/Thermo keyword for pressure treatment in elements
      CALL INIT_MAT_KEYWORD(MATPARAM,"HYDRO_EOS")

      ! Properties compatibility 
      CALL INIT_MAT_KEYWORD(MATPARAM,"SOLID_ISOTROPIC")    
      CALL INIT_MAT_KEYWORD(MATPARAM,"SPH")         

      WRITE(IOUT,1000)          
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,2001) TITR,MAT_ID,49
        WRITE(IOUT,2002) RHO0,RHOR  
        WRITE(IOUT,1100)YOUNG,SIG0,NU,CB,CN,SIGM,EPSM,T0,TMELT,SPH,PMIN,CB1,CB2,CH,CF
      ENDIF

      MTAG%G_PLA   = 1    
      MTAG%G_TEMP  = 1    

      MTAG%L_PLA   = 1
      MTAG%L_TEMP  = 1
      MTAG%L_EPSD  = 1
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
      RETURN
      END
