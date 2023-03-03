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
Chd|  HM_READ_MAT13                 source/materials/mat/mat013/hm_read_mat13.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT13(LSUBMODEL, MTAG, UNITAB, IPM, PM, 
     .                         MAT_ID   , TITR,MATPARAM)
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
#include      "units_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER, INTENT(IN)                          :: MAT_ID
      CHARACTER*nchartitle ,INTENT(IN)             :: TITR
      TYPE(SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL
      TYPE(MLAW_TAG_), INTENT(INOUT)               :: MTAG
      INTEGER, DIMENSION(NPROPMI) ,INTENT(INOUT)   :: IPM
      my_real, DIMENSION(NPROPM)  ,INTENT(INOUT)   :: PM
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT)        :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .     YOUNG, ANU, G, C0, C1, E0, E1MN2, EN1N2, SDSP, 
     .     RHO0, RHOR
      LOGICAL :: IS_ENCRYPTED, IS_AVAILABLE
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.

C     Check input encryption
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C     Initial and reference density
      CALL HM_GET_FLOATV('MAT_RHO'    ,RHO0        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho'  ,RHOR        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      IF (RHOR == ZERO) THEN
         RHOR = RHO0
      ENDIF
      PM(1) = RHOR
      PM(89) = RHO0 
C     Get input values
      CALL HM_GET_FLOATV('MAT_E', YOUNG, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU', ANU, IS_AVAILABLE, LSUBMODEL, UNITAB)

      IF (YOUNG <= ZERO) THEN
         CALL ANCMSG(MSGID=683,
     .        MSGTYPE=MSGERROR,
     .        ANMODE=ANINFO,
     .        I1=MAT_ID,
     .        C1=TITR)
      ENDIF
      IF(ANU==HALF)ANU=ZEP499
      
      G=YOUNG/(TWO*(ONE+ANU))
      C1=YOUNG/(THREE*(ONE-TWO*ANU))
      E1MN2=YOUNG/(ONE-ANU**2)
      EN1N2=ANU*E1MN2
      SDSP =SQRT(YOUNG/MAX(PM(1),EM20))
C
      PM(20)=YOUNG
      PM(21)=ANU
      PM(22)=G
      PM(24)=E1MN2
      PM(27)=SDSP
      PM(32)=C1
c-----------------
      CALL INIT_MAT_KEYWORD(MATPARAM,"TOTAL")
      IF (ANU > 0.49) THEN
        CALL INIT_MAT_KEYWORD(MATPARAM,"INCOMPRESSIBLE")
      ELSE
        CALL INIT_MAT_KEYWORD(MATPARAM,"COMPRESSIBLE")
      END IF
c-----------------
C--------------------------------
C
      WRITE(IOUT, 800) TRIM(TITR),MAT_ID,13
      WRITE(IOUT,1000)
      IF(IS_ENCRYPTED)THEN
         WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
         WRITE(IOUT, 850) RHO0
         WRITE(IOUT,1300)YOUNG,ANU,G
      ENDIF
C-----------
      RETURN
  800 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . =',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . =',I10/)
  850 FORMAT(
     & 5X,'INITIAL DENSITY    . . . . . . . . . . =',1PG20.13/)
 1000 FORMAT(
     & 5X,'  RIGID LAW                           ',/,
     & 5X,'  ---------                           ')
 1300 FORMAT(
     & 5X,'YOUNG MODULUS . . . . . . . . . . . . .=',E12.4/,
     & 5X,'POISSON RATIO . . . . . . . . . . . . .=',E12.4/,
     & 5X,'SHEAR MODULUS . . . . . . . . . . . . .=',E12.4//)
C-----------
      RETURN
      END
