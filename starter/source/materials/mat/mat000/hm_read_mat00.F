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
Chd|  HM_READ_MAT00                 source/materials/mat/mat000/hm_read_mat00.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT00(MTAG,IPM,PM,UNITAB,ID,TITR,LSUBMODEL,ISRATE,
     .                         MATPARAM)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ MAT LAW00 WITH HM READER
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME            DESCRIPTION                         
C
C     IPM             MATERIAL ARRAY(INTEGER)
C     PM              MATERIAL ARRAY(REAL)
C     UNITAB          UNITS ARRAY
C     ID              MATERIAL ID(INTEGER)
C     TITR            MATERIAL TITLE
C     LSUBMODEL       SUBMODEL STRUCTURE    
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD 
      USE ELBUFTAG_MOD            
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
C INPUT ARGUMENTS
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER,INTENT(IN)::ID
      CHARACTER*nchartitle,INTENT(IN)::TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C MODIFIED ARGUMENT
      INTEGER,INTENT(INOUT)::IPM(NPROPMI),ISRATE
      my_real,INTENT(INOUT)::PM(NPROPM)
      TYPE(MLAW_TAG_),INTENT(INOUT) :: MTAG
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   YOUNG, ANU, G, C0, C1, E0, E1MN2, EN1N2, SDSP, RHO0, RHOR
      LOGICAL IS_AVAILABLE,IS_ENCRYPTED
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.

      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)

      CALL HM_GET_FLOATV('MAT_RHO',  RHO0  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho',RHOR  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_E',    YOUNG ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('nu',       ANU   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
C--------------------------------------------------
      IF (RHOR == ZERO ) RHOR=RHO0

      G=YOUNG/(TWO*(ONE+ANU))
      C0=ZERO
      C1=YOUNG/(THREE*(ONE-TWO*ANU))
      E0=ZERO
      E1MN2=YOUNG/(ONE-ANU**2)
      EN1N2=ANU*E1MN2
      SDSP =SQRT(YOUNG/MAX(RHOR,EM20))
      ISRATE = 0
C
      PM(1) =RHOR
      PM(20)=YOUNG
      PM(21)=ANU
      PM(70)=ZERO
      PM(72)=ZERO
      pm(32)=C1
      PM(89) =RHO0
c-------------------------------
      MTAG%G_EINT = 0
      MTAG%L_EINT = 0
      MTAG%L_DP_DRHO = 0
c-------------------------------
      ! Properties compatibility
      CALL INIT_MAT_KEYWORD(MATPARAM,"SOLID_ISOTROPIC")
      CALL INIT_MAT_KEYWORD(MATPARAM,"SHELL_ISOTROPIC")
      CALL INIT_MAT_KEYWORD(MATPARAM,"SPRING_MATERIAL")
      CALL INIT_MAT_KEYWORD(MATPARAM,"BEAM_ALL")
      CALL INIT_MAT_KEYWORD(MATPARAM,"TRUSS")
c-------------------------------
      WRITE(IOUT,1001) TRIM(TITR),ID,0
      WRITE(IOUT,1000)
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1100)RHO0,YOUNG,ANU
      ENDIF
C
C-----------
      RETURN
 1000 FORMAT(
     & 5X,'  VOID MATERIAL                         ',/,
     & 5X,'  -------------                         ',//)
 1001 FORMAT(
     & 5X,A,/,
     & 5X,'   MATERIAL NUMBER . . . . . . . . . . . .=',I10/,
     & 5X,'   MATERIAL LAW. . . . . . . . . . . . . .=',I10/)
 1100 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'YOUNG MODULUS . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'POISSON''S RATIO . . . . . . . . . . . .=',1PG20.13/)
C-----------
      RETURN
      END
