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
Chd|  HM_READ_MAT19                 source/materials/mat/mat019/hm_read_mat19.F
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
      SUBROUTINE HM_READ_MAT19(MTAG     ,UPARAM   ,MAXUPARAM,NUPARAM  ,PM       ,
     .                         MATPARAM ,PARMAT   ,NUVAR    ,MAT_ID   ,TITR     ,
     .                         UNITAB   ,LSUBMODEL,ISRATE   )
C-----------------------------------------------
C   D e s c r i p t i o n
C   =====================
C   READ MAT LAW19 WITH HM READER
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
      USE SUBMODEL_MOD
      USE MATPARAM_DEF_MOD          
      USE ELBUFTAG_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(INOUT)  :: NUPARAM,NUVAR,ISRATE
      INTEGER ,INTENT(IN)   :: MAT_ID,MAXUPARAM
      CHARACTER*nchartitle,INTENT(IN)::TITR
      my_real ,INTENT(INOUT):: PM(NPROPM),PARMAT(100),UPARAM(MAXUPARAM)
      TYPE (UNIT_TYPE_)      ,INTENT(IN)    :: UNITAB 
      TYPE(SUBMODEL_DATA)    ,INTENT(IN)    :: LSUBMODEL((NSUBMOD))
      TYPE(MLAW_TAG_)        ,INTENT(INOUT) :: MTAG
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real :: N12,N21,E11,E22,G12,G23,G31,RCOMP,DETC,AMU,C1,SSP,
     .   A11,A22,A12,ZEROSTRESS,POROSITY,AREAMIN1,AREAMIN2,RHOR,RHO0
      INTEGER ISENS
      LOGICAL IS_AVAILABLE,IS_ENCRYPTED
C-----------------------------------------------
C   S o u r c e   L i n e s
c=======================================================================
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.

      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)

      CALL HM_GET_FLOATV('MAT_RHO'  ,RHO0  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho',RHOR  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      
      CALL HM_GET_FLOATV('MAT_EA'   ,E11   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_EB'   ,E22   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_PRAB' ,N12   ,IS_AVAILABLE,LSUBMODEL,UNITAB)

      CALL HM_GET_FLOATV('MAT_GAB'  ,G12   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_GBC'  ,G23   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_GCA'  ,G31   ,IS_AVAILABLE,LSUBMODEL,UNITAB)

      CALL HM_GET_FLOATV('MAT_REDFACT'   ,RCOMP      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('M58_Zerostress',ZEROSTRESS ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_POROS'     ,POROSITY   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV  ('ISENSOR'       ,ISENS      ,IS_AVAILABLE,LSUBMODEL) 
C-----------------------------------------------------
c
      ISRATE = -1
      N21    = N12*E22/E11
      DETC   = ONE - N12*N21
      A11 = E11 / DETC
      A22 = E22 / DETC
      A12 = A11 * N21
      C1  = MAX(E11,E22) / DETC
      SSP = SQRT(C1/RHO0)
C-----------------------------------------------------
      IF(E11 == ZERO.OR. E22 == ZERO .OR. G12 == ZERO .OR. G23 == ZERO .OR. G31 == ZERO) THEN
         CALL ANCMSG(MSGID=306, MSGTYPE=MSGERROR, ANMODE=ANINFO,
     .               I1=19,
     .               I2=MAT_ID,
     .               C1=TITR,
     .               C2='E11, E22, G12, G23, or G31')
      ENDIF
C--------------------------------------------------------
      IF (DETC <= ZERO) THEN
         CALL ANCMSG(MSGID=307, MSGTYPE=MSGERROR, ANMODE=ANINFO,
     .               I1=MAT_ID,
     .               C1=TITR)
      ENDIF
C-----------------------------------------
      IF (RCOMP == ZERO) RCOMP=ONE
        IF (RCOMP < EM03) THEN
          CALL ANCMSG(MSGID=1572, MSGTYPE=MSGWARNING, ANMODE=ANINFO,
     .                I1=MAT_ID,
     .                C1=TITR)
         RCOMP = EM03
       ENDIF
C-------------------------------------
      IF (POROSITY < ZERO .OR. POROSITY > ONE) THEN
         CALL ANCMSG(MSGID=775, MSGTYPE=MSGERROR, ANMODE=ANINFO,
     .               I1=MAT_ID,
     .               C1=TITR)
      ENDIF
C-----------------------------------------
      IF (RHOR == ZERO ) RHOR=RHO0
      PM(1) = RHOR
      PM(89)= RHO0
      AMU = PM(17)
      IF (AMU == ZERO) AMU = FOURTH

      PM(17) = AMU
      PM(20) = MAX(E11,E22)/DETC
      PM(21) = SQRT(N12*N21)
      PM(22) = MAX(G12,G23,G31)    
      PM(24) = PM(20)
      PM(25) = PM(21)*PM(24)
      PM(26) = FIVE_OVER_6
      PM(27) = SSP
      PM(32) = C1
      PM(33) = E11
      PM(34) = E22
      PM(35) = N12
      PM(36) = N21
      PM(37) = G12
      PM(38) = G23
      PM(39) = G31
      PM(44) = RCOMP
c
      AREAMIN1 = ZERO
      PM(53)   = AREAMIN1
      AREAMIN2 = ONE + HALF*(AREAMIN1-ONE)
      IF (AREAMIN2 > AREAMIN1) THEN
        PM(54) = ONE / (AREAMIN2-AREAMIN1)
      ELSE
        PM(54) = ZERO
      ENDIF
      PM(56) = POROSITY 
c-----------------------------------------
      UPARAM(1)  = E11
      UPARAM(2)  = E22
      UPARAM(3)  = N12
      UPARAM(4)  = N21
      UPARAM(5)  = G12
      UPARAM(6)  = G23
      UPARAM(7)  = G31
      UPARAM(8)  = RCOMP
      UPARAM(9)  = SSP
      UPARAM(10) = A11                         
      UPARAM(11) = A22                         
      UPARAM(12) = A12
      UPARAM(13) = ISENS
      UPARAM(14) = ZEROSTRESS
      UPARAM(15) = POROSITY
c
      NUPARAM = 15
      NUVAR   = 0
c                
c --------------------------
      PARMAT(1)  = C1
      PARMAT(2)  = MAX(E11,E22)/DETC
      PARMAT(3)  = SQRT(N12*N21)
      PARMAT(4)  = ISRATE
      PARMAT(5)  = ZERO ! FCUT
c
c---- Definition des variables internes (stockage elementaire)
c
      MTAG%G_SIGI = 3
c
      ! MATPARAM keywords
      CALL INIT_MAT_KEYWORD(MATPARAM,"TOTAL")
      CALL INIT_MAT_KEYWORD(MATPARAM,"INCOMPRESSIBLE")
      CALL INIT_MAT_KEYWORD(MATPARAM,"ORTHOTROPIC")

      ! Properties compatibility
      CALL INIT_MAT_KEYWORD(MATPARAM,"SHELL_ORTHOTROPIC")
C--------------------------------------
      WRITE(IOUT,1000) TRIM(TITR),MAT_ID,19
      WRITE(IOUT,1100)      
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1200) RHO0
        WRITE(IOUT,1300) E11,E22,N12,G12,G23,G31,
     .                   RCOMP,ZEROSTRESS,ISENS,POROSITY
      ENDIF
C----------------------------------------
 1000 FORMAT(
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . . . . . . =',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . . . . . . =',I10/)
 1100 FORMAT
     &(5X,'ORTHOTROPIC MATERIAL FOR SHELLS (LAW19)     ',/,
     & 5X,'--------------------------------------------',/)
 1200 FORMAT(
     & 5X,'INITIAL DENSITY . . . .  .  . . . . . . . . . . .=',1PG20.13/)  
 1300 FORMAT(
     & 5X,'YOUNG MODULUS E11 (TENSION) . . . . . . . . . . .=',1PG20.13/
     & 5X,'YOUNG MODULUS E22 (TENSION) . . . . . . . . . . .=',1PG20.13/
     & 5X,'POISSON RATIO NU12. . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'SHEAR MODULUS G12 . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'SHEAR MODULUS G23 . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'SHEAR MODULUS G31 . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'COMPRESSION REDUCTION FACTOR. . . . . . . . . . . ',/
     & 5X,'     RCOMP=E11C/E11= E22C/E22 . . . . . . . . . .=',1PG20.13/
     & 5X,'REF-STATE STRESS RELAXATION FACTOR. . . . . . . .=',1PG20.13/
     & 5X,'REF-STATE ACTIVATION SENSOR ID. . . . . . . . . .=',1PG20.13/
     & 5X,'POROSITY. . . . . . . . . . . . . . . . . . . . .=',1PG20.13/)
C-----------
      RETURN
      END
