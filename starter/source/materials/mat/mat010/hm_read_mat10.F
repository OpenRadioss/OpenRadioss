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
Chd|  HM_READ_MAT10                 source/materials/mat/mat010/hm_read_mat10.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT10(
     .           LSUBMODEL ,MTAG     ,UNITAB   ,IPM     ,PM      ,
     .           MAT_ID    ,TITR     ,EOS_TAG  ,ISRATE  ,MATPARAM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD !MAXEOS
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE MATPARAM_DEF_MOD          
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ MAT LAW21 WITH HM READER
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
#include      "units_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER, INTENT(IN)                          :: MAT_ID
      INTEGER, INTENT(INOUT)                       :: ISRATE
      INTEGER, DIMENSION(NPROPMI) ,INTENT(INOUT)   :: IPM
      CHARACTER*nchartitle ,INTENT(IN)             :: TITR
      my_real, DIMENSION(NPROPM)  ,INTENT(INOUT)   :: PM
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD),INTENT(IN) :: LSUBMODEL
      TYPE(MLAW_TAG_), INTENT(INOUT)               :: MTAG
      TYPE(EOS_TAG_) , TARGET, DIMENSION(0:MAXEOS) ,INTENT(INOUT) :: EOS_TAG  
      TYPE(MATPARAM_STRUCT_),INTENT(INOUT)         :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
      INTEGER :: I,IFUNC,COUNT,ID,IFORM,IEOS
      my_real :: RHO0,RHOR,E,NU,A0,A1,A2,AMX,C0,C1,C2,C3,FAC_Y
      my_real :: PMIN,PEXT,XMUMX,BUNL,E0,G,PSTAR,PSH,DELTA,DET,YOUNG,ANU
      CHARACTER*64 :: chain,chain2  
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      PSTAR   = -INFINITY      
      C0      = ZERO
      C1      = ZERO
      C2      = ZERO
      C3      = ZERO
      PEXT    = ZERO
      PSH     = ZERO
      PMIN    = ZERO
      BUNL    = ZERO
      XMUMX   = ZERO
      ISRATE  = 0
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
!---
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
!
      CALL HM_GET_INTV  ('Line_count'      ,COUNT ,IS_AVAILABLE,LSUBMODEL)
      IF(COUNT==0)COUNT=3 ! format 4.* and older
      COUNT=COUNT+2

      CALL HM_GET_FLOATV('MAT_RHO'         ,RHO0  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho'       ,RHOR  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_E'           ,YOUNG     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'          ,ANU    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_A0'          ,A0    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_A1'          ,A1    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_A2'          ,A2    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_AMAX'        ,AMX   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('EOS_COM_C0'      ,C0    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('EOS_COM_C1'      ,C1    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('EOS_COM_C2'      ,C2    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('EOS_COM_C3'      ,C3    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('EOS_COM_B'       ,BUNL  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('EOS_COM_Mue_max' ,XMUMX ,IS_AVAILABLE, LSUBMODEL, UNITAB)      

      CALL HM_GET_FLOATV('MAT_PC'          ,PMIN  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('PEXT'            ,PEXT  ,IS_AVAILABLE, LSUBMODEL, UNITAB)

!---
      IF (RHOR == ZERO) RHOR=RHO0
      PM(1) = RHOR
      PM(89)= RHO0
!
      IF(COUNT == 5) THEN
        IFORM= 1 !old format (embedded EoS : C0,C1,C2, ... parameters)
        IEOS = 13
        IPM(4)=IEOS
        EOS_TAG(IEOS)%L_MU = 1
      ELSE
        IFORM=2 !new format (Eos defined from /EOS option)
        IREC = IREC+1
      ENDIF

      E0 = ZERO

      IF(C1 == ZERO)C1=THIRD*YOUNG/(ONE-TWO*ANU)   !bulk modulus

      IF(YOUNG <= ZERO)THEN
        chain='YOUNG MODULUS MUST BE DEFINED                                   '
        chain2=''
        CALL ANCMSG(MSGID=829,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=10,
     .                I2=MAT_ID,
     .                C1='ERROR',
     .                C2=TITR,
     .                C3=chain,
     .                C4=chain2)   
      ENDIF 

      IF(ANU <= ZERO)THEN
        chain='POISSON RATIO MUST BE DEFINED                                   '
        chain2=''
        CALL ANCMSG(MSGID=829,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=10,
     .                I2=MAT_ID,
     .                C1='ERROR',
     .                C2=TITR,
     .                C3=chain,
     .                C4=chain2)   
      ENDIF 
      
      IF(A1 < ZERO .AND. A2 == ZERO)THEN  
        chain ='INVERTED YIELD SURFACE BECAUSE A1 IS NEGATIVE.                  '
        chain2='CHECK A0,A1,A2 YIELD PARAMETERS                                 '
        CALL ANCMSG(MSGID=829,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO,
     .                I1=10,
     .                I2=MAT_ID,
     .                C1='WARNING',
     .                C2=TITR,
     .                C3=chain,
     .                C4=chain2)   
      ENDIF 
    
      IF(A2 < ZERO)THEN
        chain ='UNEXPECTED YIELD SURFACE : A2 IS NEGATIVE                       '
        chain2='CHECK A0,A1,A2 YIELD PARAMETERS                                 '
        CALL ANCMSG(MSGID=829,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO,
     .                I1=10,
     .                I2=MAT_ID,
     .                C1='WARNING',
     .                C2=TITR,
     .                C3=chain,
     .                C4=chain2)     
      ENDIF           
      IF(IFORM == 1 .AND. C1 <= ZERO) THEN
        chain='TENSILE BULK MODULUS C1 IS LOWER OR EQUAL TO 0.                 '
        chain2=''
        CALL ANCMSG(MSGID=829,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=10,
     .                I2=MAT_ID,
     .                C1='ERROR',
     .                C2=TITR,
     .                C3=chain,
     .                C4=chain2)
      END IF  

      IF(A2==ZERO.AND.A1/=ZERO)THEN   !(A2=A1=ZERO => error message)
        PSTAR=-A0/A1 
      ELSEIF(A2 /= ZERO)THEN
        DELTA = A1*A1-FOUR*A0*A2
        !Si intersection avec l'axe
        IF(DELTA  >=  ZERO)THEN
          DELTA=SQRT(DELTA) 
          PSTAR = (-A1+DELTA)/TWO/A2
        
        ELSE
          PSTAR = -A1/TWO/A2 !extremum
          chain ='YIELD SURFACE J2(P)=A0+A1.P+A2.P^2 HAS NO INTERSECTION WITH     '
          chain2='PRESSURE AXIS. ASSUMING SURFACE CLOSURE AT P=                   '
          WRITE(chain2(46:59),FMT=('(E13.6)'))   PSTAR   
          CALL ANCMSG(  MSGID=829,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO,
     .                  I1=10,
     .                  I2=MAT_ID,
     .                  C1='WARNING',
     .                  C2=TITR,
     .                  C3=chain,
     .                  C4=chain2)               
        ENDIF
      ELSE
        !do nothing let user do what he wants
      ENDIF
C
      IF(AMX == ZERO)  AMX   = EP20
      IF(PMIN == ZERO) PMIN  =-INFINITY
      !-----------------------------------------------------------------------------!
      ! THESE FOLLOWING CHECKS ARE RELEVANT ONLY IN CASE OF EMBEDDED COMPACTION EOS !
      !-----------------------------------------------------------------------------!

      IF(IFORM == 1)THEN
        IF(XMUMX == ZERO.AND.BUNL /= ZERO)THEN
         chain= 'MISSING MUMAX VALUE IS AUTOMATICALLY ESTIMATED FROM BUNL VALUE. '
         chain2=''
         CALL ANCMSG(MSGID=829,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO,
     .               I1=10,
     .               I2=MAT_ID,
     .               C1='WARNING',
     .               C2=TITR,
     .               C3=chain,
     .               C4=chain2)         
         IF(C3 == ZERO)THEN
          IF(C2 == ZERO)THEN
           XMUMX=EP20 
          ELSE
           XMUMX=(BUNL-C1)/(TWO*C2)
          ENDIF
         ELSE
          DET=SQRT(C2**2 + THREE*C3*(BUNL-C1))
           XMUMX=(DET-C2)/(THREE*C3)
         ENDIF
        ENDIF

        IF(XMUMX /= ZERO.AND.BUNL == ZERO)THEN
         chain= 'MISSING BUNL VALUE IS AUTOMATICALLY ESTIMATED FROM MUMAX        '     
         chain2='' 
         CALL ANCMSG(MSGID=829,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO,
     .               I1=10,
     .               I2=MAT_ID,
     .               C1='WARNING',
     .               C2=TITR,
     .               C3=chain,
     .               C4=chain2)
         IF(C3 == ZERO)THEN
          IF(C2 == ZERO)THEN
           BUNL = C1 
          ELSE
           BUNL = C1 + TWO*C2*XMUMX
          ENDIF
         ELSE
           BUNL = C1 + TWO*C2*XMUMX + THREE*C3*C3*XMUMX**TWO
         ENDIF
        ENDIF  
        IF(BUNL == ZERO) BUNL  = C1           
      ENDIF !IFORM == 1

      G=YOUNG/(TWO*(ONE + ANU))

      !--- STORAGE OF MATERIAL PARAMETERS IN MATERIAL BUFFER ---!
      PM(20)=YOUNG
      PM(21)=ANU
      PM(22)=G
      PM(23)=E0     
      PM(31)=C0
      PM(32)=C1
      PM(33)=C2
      PM(34)=C3
      PM(49)=C0
      PM(35)=ZERO
      PM(36)=ZERO
      PM(37)=PMIN
      PM(38)=A0
      PM(39)=A1
      PM(40)=A2
      PM(41)=AMX
      PM(43)=PEXT      
      PM(44)=PSTAR 
      PM(45)=BUNL
      PM(46)=XMUMX
      PM(47)=ZERO ! MUMIN
      PM(48)=IFORM
      PM(79)=THREE100
      PM(88)=PSH 
      PM(104)=C0     
      PM(105) = TWO*G/(C1+FOUR_OVER_3*G) ! =(1-2*Nu)/(1-Nu)
      
      !Formulation for solid elements time step computation.
      IPM(252)= 2

C---- Buffer Size for specific element buffer allocations

      MTAG%G_PLA   = 1
      MTAG%G_EPSQ  = 1

      MTAG%L_PLA   = 1     ! /TH(VPLA)
      MTAG%L_EPSQ  = 1     ! /TH(EPSP) 
      
      ! EOS/Thermo keyword
      CALL INIT_MAT_KEYWORD(MATPARAM,"ELASTO_PLASTIC")
      CALL INIT_MAT_KEYWORD(MATPARAM,"HYDRO_EOS")

      ! Properties compatibility
      CALL INIT_MAT_KEYWORD(MATPARAM,"SOLID_ISOTROPIC")
      CALL INIT_MAT_KEYWORD(MATPARAM,"SPH")

      !--- OUTPUT IN STARTER LISTING FILE ---!
      WRITE(IOUT,1100) TRIM(TITR),MAT_ID,10
      WRITE(IOUT,1000)
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1200)RHO0,RHOR
        WRITE(IOUT,1300)YOUNG,ANU,G
        WRITE(IOUT,1400)A0,A1,A2,AMX
        IF(IFORM == 1)THEN
          WRITE(IOUT,1500)C0,C1,C2,C3,BUNL,XMUMX,PMIN,PSTAR
        ELSE
          WRITE(IOUT,1501)BUNL,XMUMX,PMIN,PSTAR
        ENDIF
      ENDIF
C-----------
      RETURN
 1000 FORMAT(
     & 5X,40H  DRUCKER-PRAGER (LAW10)                    ,/,
     & 5X,40H  ----------------------                   ,//)
 1100 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW. . . . . . . . . . . . . .=',I10/)
 1200 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'REFERENCE DENSITY . . . . . . . . . . .=',1PG20.13/)
 1300 FORMAT(
     & 5X,40HYOUNG'S MODULUS . . . . . . . . . . . .=,E12.4/,
     & 5X,40HPOISSON'S RATIO . . . . . . . . . . . .=,E12.4/,
     & 5X,40HSHEAR MODULUS . . . . . . . . . . . . .=,E12.4//)
 1400 FORMAT(
     & 5X,40HYIELD COEFFICIENT A0. . . . . . . . . .=,E12.4/,
     & 5X,40HYIELD COEFFICIENT A1. . . . . . . . . .=,E12.4/,
     & 5X,40HYIELD COEFFICIENT A2. . . . . . . . . .=,E12.4/,
     & 5X,40HA-MAX . . . . . . . . . . . . . . . . .=,E12.4//)
 1500 FORMAT(
     & 5X,40HC0. . . . . . . . . . . . . . . . . . .=,E12.4/,
     & 5X,40HC1. . . . . . . . . . . . . . . . . . .=,E12.4/,
     & 5X,40HC2. . . . . . . . . . . . . . . . . . .=,E12.4/,
     & 5X,40HC3. . . . . . . . . . . . . . . . . . .=,E12.4/,
     & 5X,40HUNLOADING BULK. . . . . . . . . . . . .=,E12.4/,
     & 5X,40HMAX VOLUMIC COMPRESSION . . . . . . . .=,E12.4/,
     & 5X,40HFRACTURE PRESSURE . . . . . . . . . . .=,E12.4/,
     & 5X,40HYIELD SURFACE PRESSURE ROOT . . . . . .=,E12.4//)
 1501 FORMAT(
     & 5X,40HUNLOADING BULK. . . . . . . . . . . . .=,E12.4/,
     & 5X,40HMAX VOLUMIC COMPRESSION . . . . . . . .=,E12.4/,
     & 5X,40HFRACTURE PRESSURE . . . . . . . . . . .=,E12.4/,
     & 5X,40HYIELD SURFACE PRESSURE ROOT . . . . . .=,E12.4//)
C-----------
      RETURN
      END