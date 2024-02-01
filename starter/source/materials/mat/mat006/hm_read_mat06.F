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
Chd|====================================================================
Chd|  HM_READ_MAT06                 source/materials/mat/mat006/hm_read_mat06.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        HM_GET_BOOLV                  source/devtools/hm_reader/hm_get_boolv.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT06(
     .           LSUBMODEL ,MTAG6    ,UNITAB   ,IPM     ,PM,
     .           MAT_ID    ,TITR     ,EOS_TAG  ,ISRATE,
     .           ITYP      ,MTAG17   ,MATPARAM )
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
#include      "scr03_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER, INTENT(INOUT)                       :: MAT_ID
      INTEGER, INTENT(INOUT)                       :: ISRATE
      INTEGER, DIMENSION(NPROPMI) ,INTENT(INOUT)   :: IPM
      CHARACTER*nchartitle ,INTENT(IN)             :: TITR
      my_real, DIMENSION(NPROPM)  ,INTENT(INOUT)   :: PM
      TYPE(SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL
      TYPE(MLAW_TAG_), INTENT(INOUT)               :: MTAG6,MTAG17
      TYPE(EOS_TAG_) , TARGET, DIMENSION(0:MAXEOS) ,INTENT(INOUT) :: EOS_TAG  
      INTEGER,INTENT(IN)                           :: ITYP 
      TYPE(MATPARAM_STRUCT_),INTENT(INOUT)         :: MATPARAM   
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED,IS_EOS
      INTEGER JTUR, I, EMPTY_LINE,COUNT,IEOS,LAW6_OPT
      my_real
     .   RHO0, VIS, BID, PSH, C0, C1, C2, C3, C4, C5, PMIN, E0, RK0,
     .   SSL, CTM, SK, SE, RPR, CT1, CT2, CT3, AK, E, A, B, YP0, YP1,
     .   MU,RHOR
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      COUNT = 0
      IS_EOS=.FALSE.
      IEOS=0
      PMIN=ZERO
      RHOR=ZERO
      IS_ENCRYPTED=.false.
!---
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
!
      CALL HM_GET_INTV  ('Line_count'     ,COUNT    ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_RHO'        ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho'      ,RHOR     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_BOOLV ('EOS_DATA_EXIST' ,IS_EOS   ,IS_AVAILABLE)  
      CALL HM_GET_INTV  ('Law6_opt'       ,LAW6_OPT ,IS_AVAILABLE, LSUBMODEL)              !1:HYD_VISC,2:HYDRO,3:K-EPS
      CALL HM_GET_FLOATV('DAMP1'          ,VIS      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_PC'         ,PMIN     ,IS_AVAILABLE, LSUBMODEL, UNITAB)

      IF(COUNT==3)IS_EOS=.TRUE.             !EoS no longer embedded since format 2018, but still compatible
      IF(INVERS_INIT<2018)IS_EOS=.TRUE.  !no embedded EoS before up to format 2017
      
      IF (IS_EOS) THEN
         IEOS = 1 !polynomial EOS
         CALL HM_GET_FLOATV('MAT_C0'      ,C0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
         CALL HM_GET_FLOATV('MAT_C1'      ,C1     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
         CALL HM_GET_FLOATV('MAT_C2'      ,C2     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
         CALL HM_GET_FLOATV('MAT_C3'      ,C3     ,IS_AVAILABLE, LSUBMODEL, UNITAB)                           
         CALL HM_GET_FLOATV('MAT_C4'      ,C4     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
         CALL HM_GET_FLOATV('MAT_C5'      ,C5     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
         CALL HM_GET_FLOATV('MAT_EA'      ,E0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)  
         CALL HM_GET_FLOATV('MAT_PSH'     ,PSH    ,IS_AVAILABLE, LSUBMODEL, UNITAB)                                             
      ELSE
         E0 = ZERO
         C0 = ZERO
         C1 = ZERO
         C2 = ZERO
         C3 = ZERO
         C4 = ZERO
         C5 = ZERO
         PSH =ZERO
      ENDIF
      
      IF (PMIN == ZERO) PMIN=-INFINITY
      IF (RHOR == ZERO) RHOR=RHO0
      PM(1) = RHOR
      PM(89)= RHO0

      PM(23)=E0
      PM(24)=VIS

      IF(RHOR /= ZERO) THEN
        MU = RHO0/RHOR-ONE  
      ELSE
        MU = ZERO  ! RHOR=0.0 => error 683 will be displayed in hm_read_mat.F
      ENDIf
      IF(MU < ZERO)THEN
        PM(31) = -PSH + C0+C1*MU+         C3*MU**3+(C4+C5*MU)*E0
      ELSE
        PM(31) = -PSH + C0+C1*MU+C2*MU**2+C3*MU**3+(C4+C5*MU)*E0
      ENDIF

      PM(32)=C1
      PM(33)=C2
      PM(34)=C3
      PM(35)=C4
      PM(36)=C5
      PM(37)=PMIN
      PM(80)=INFINITY
      PM(88)=PSH      
      PM(104)=C0 - PSH
      
      IPM(04)=IEOS

      ! Material compatibility with /EOS option
      CALL INIT_MAT_KEYWORD(MATPARAM,"EOS")

      ! EOS/Thermo keyword for pressure treatment in elements
      CALL INIT_MAT_KEYWORD(MATPARAM,"HYDRO_EOS")

      CALL INIT_MAT_KEYWORD(MATPARAM,"INCOMPRESSIBLE")

      ! Properties compatibility
      CALL INIT_MAT_KEYWORD(MATPARAM,"SOLID_POROUS")
      CALL INIT_MAT_KEYWORD(MATPARAM,"SPH")
C-----

      WRITE(IOUT,2001) TRIM(TITR),MAT_ID,6   
      WRITE(IOUT,1000)
      
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSEIF (IS_EOS) THEN
        WRITE(IOUT,2002)RHO0,RHOR
        WRITE(IOUT,1300)VIS
        WRITE(IOUT,1400)PSH
        WRITE(IOUT,1500)C0,C1,C2,C3,C4,C5,PMIN,PM(31),E0  
      ELSE
         WRITE(IOUT,2002)RHO0,RHOR
         WRITE(IOUT,1300)VIS
         WRITE(IOUT,1501)PMIN 
      ENDIF


c
C---- Definition des variables internes (stockage elementaire)
c
      MTAG6%G_RK    = 1
      MTAG6%G_RE    = 1
      MTAG6%L_RK    = 1 
      MTAG6%L_RE    = 1 
      MTAG6%L_VK    = 1 
      MTAG6%G_TEMP  = 1
      MTAG6%L_TEMP  = 1
c
      MTAG17%G_RK   = 1
      MTAG17%G_RE   = 1
      MTAG17%L_RK   = 1 
      MTAG17%L_RE   = 1 
      MTAG17%L_VK   = 1
      MTAG17%G_TEMP = 1
      MTAG17%L_TEMP = 1 
C--------------------------------
 1000 FORMAT(
     & 5X,'  VISCOUS FLUID   ',/,
     & 5X,'  --------------  ',//)
 1300 FORMAT(
     & 5X,'VISCOSITY . . . . . . . . . . . . . . .=',1PG20.13)
 1400 FORMAT(
     & 5X,'PSH PRESSURE SHIFT. . . . . . . . . . .=',1PG20.13/)
 1500 FORMAT(
     & 5X,'C0. . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'C1. . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'C2. . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'C3. . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'C4. . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'C5. . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'PRESSURE CUTOFF . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'INITIAL PRESSURE. . . . . . . . . . . .=',1PG20.13/,
     & 5X,'INITIAL INTERNAL ENERGY PER UNIT VOLUME=',1PG20.13//)
 1501 FORMAT(
     & 5X,'PRESSURE CUTOFF . . . . . . . . . . . .=',1PG20.13//)
 2001 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW. . . . . . . . . . . . . .=',I10/)
 2002 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'REFERENCE DENSITY . . . . . . . . . . .=',1PG20.13)

C--------------------------------
C-----------
      RETURN
      END
