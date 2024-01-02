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
Chd|  HM_READ_MAT20                 source/materials/mat/mat020/hm_read_mat20.F
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
      SUBROUTINE HM_READ_MAT20(
     .             IPM      ,PM       ,UNITAB   ,MAT_ID   ,TITR    ,
     .             LSUBMODEL,MTAG     ,MATPARAM )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C   READ MAT LAW20 WITH HM READER 
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
#include      "com01_c.inc"
#include      "mmale20_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER, INTENT(INOUT) :: IPM(NPROPMI)
      INTEGER, INTENT(IN) :: MAT_ID
      my_real, INTENT(INOUT) :: PM(NPROPM)
      TYPE(MLAW_TAG_), INTENT(INOUT) :: MTAG
      CHARACTER*nchartitle, INTENT(IN) :: TITR
      TYPE(SUBMODEL_DATA),INTENT(IN) :: LSUBMODEL(*)
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER MAT(5), I, NMAT
      my_real AMAT(5)
      INTEGER CMAT(5)
      DATA CMAT/0,0,0,0,0/
      LOGICAL :: IS_ENCRYPTED, IS_AVAILABLE
C-----------------------------------------------
C     S o u r c e 
C-----------------------------------------------
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)

C     Number of material (it's always 2...)
      NMAT = 2      
C
      CALL HM_GET_INTV('MAT1', MAT(1), IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('MAT2', MAT(2), IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('IC1', CMAT(1), IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('IC2', CMAT(2), IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('IDISCRETE', CMAT(3), IS_AVAILABLE, LSUBMODEL)
C     Material proportions
      CALL HM_GET_FLOATV('MAT_ALPHA1', AMAT(1), IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_ALPHA2', AMAT(2), IS_AVAILABLE, LSUBMODEL, UNITAB)
      
      DO I=1,2
         PM(20 + I) = MAT(I) + EM01
         PM(30 + I) = AMAT(I)
         PM(185 + I) = CMAT(I)
      ENDDO
      PM(20)=NMAT+EM01
      NMULT=MAX(NMULT,NMAT)
C
      WRITE(IOUT, 900) TRIM(TITR),MAT_ID,20
      WRITE(IOUT,1000)
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1300)(MAT(I),AMAT(I),CMAT(I),I=1,NMAT)
      ENDIF
C
      IF(CMAT(3)>=ONE)DISCRETE_FILL=.TRUE.

      IF(NSPMD > 1) THEN
        CALL ANCMSG(MSGID=755,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              C1='LAW20/2D MULTIMATERIAL')
      ENDIF
C----- Definition des variables internes (stockage elementaire)
      MTAG%L_FRAC = 1   ! pourcentage de phase
C-----------
      ! MATPARAM keywords
      CALL INIT_MAT_KEYWORD(MATPARAM,"INCOMPRESSIBLE")
      ! Properties compatibility
      CALL INIT_MAT_KEYWORD(MATPARAM,"SOLID_ORTHOTROPIC")      
C-----------
      RETURN
C--------------------------------
  900 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . . . .=',I10/)
 1000 FORMAT(
     & 5X,40H  /MAT/LAW20 : 2D BI-MATERIAL LAW       ,/,
     & 5X,40H  -------------------------------       ,//)
 1300 FORMAT(
     & 5X,40HREFERENCED MATERIAL NUMBER. . . . . . .=,I5/,
     & 5X,40HFILLING RATIO . . . . . . . . . . . . .=,E12.4/,
     & 5X,40HINCOMING PHASIS CONTROL FLAG. . . . . .=,I5)
C-----------
      RETURN
      END
