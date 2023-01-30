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
Chd|  HM_READ_FAIL_ORTHSTRAIN       source/materials/fail/orthstrain/hm_read_fail_orthstrain.F
Chd|-- called by -----------
Chd|        HM_READ_FAIL                  source/materials/fail/hm_read_fail.F
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
       SUBROUTINE HM_READ_FAIL_ORTHSTRAIN(
     .            UPARAM  ,MAXUPARAM ,NUPARAM ,NUVAR   ,IFUNC    ,
     .            MAXFUNC ,NFUNC     ,UNITAB  ,FAIL_ID ,LSUBMODEL)
C-----------------------------------------------
c    Orthotropic strain  failure model
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ FAILURE ORTHSTRAIN
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME            DESCRIPTION                         
C
C     UNITAB          UNITS ARRAY
C     FAIL_ID         FAILURE ID(INTEGER)
C     TITR            MATERIAL TITLE
C     LSUBMODEL       SUBMODEL STRUCTURE   
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C----------+---------+---+---+--------------------------------------------
C VAR      | SIZE    |TYP| RW| DEFINITION
C----------+---------+---+---+--------------------------------------------
C UPARAM   | NUPARAM | F | W | USER FAILURE MODEL PARAMETER ARRAY
C MAXUPARAM|  1      | I | R | MAXIMUM SIZE OF UPARAM 
C NUPARAM  |  1      | I | W | SIZE OF UPARAM =< MAXUPARAM
C NUVAR    |  1      | I | W | NUMBER OF USER  VARIABLES
C----------+---------+---+---+--------------------------------------------
C IFUNC    | NFUNC   | I | W | FUNCTION NUMBER ARRAY
C MAXFUNC  |  1      | I | R | MAXIMUM SIZE OF IFUNC
C NFUNC    |  1      | I | W | SIZE OF IFUNC =< MAXFUNC
C----------+---------+---+---+--------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER,INTENT(IN)             ::MAXUPARAM,MAXFUNC,FAIL_ID
      TYPE(SUBMODEL_DATA),INTENT(IN) ::LSUBMODEL(*)
C MODIFIED ARGUMENT
      INTEGER,INTENT(INOUT)          ::IFUNC(MAXFUNC),NFUNC
      my_real,INTENT(INOUT)          ::UPARAM(MAXUPARAM)
      INTEGER,INTENT(INOUT)            ::NUPARAM,NUVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IDAM,ISOLID,FT1,FT2,FT12,FC1,FC2,FC12,STRDEF
      my_real 
     .    EPSPREF,ET1,ET2,ET12,EC1,EC2,EC12,ET3,EC3,EC23,ET23,EC31,ET31,
     .    ET1M,ET2M,ET12M,EC1M,EC2M,EC12M,ET3M,EC3M,EC23M,ET23M,EC31M,ET31M,
     .    FAC_M,FAC_L,FAC_T,FAC_E,PTHK,ALPHA,FSCALE_SIZ,REF_SIZ,
     .    EPSPREF_UNIT,FSCAL_UNIT,REF_SIZ_UNIT,FCUT
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C=======================================================================
      IS_ENCRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
      CALL HM_GET_FLOATV         ('Pthk'             ,PTHK          ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV           ('Strdef'           ,STRDEF        ,IS_AVAILABLE,LSUBMODEL)
c
      CALL HM_GET_FLOATV         ('Epsilon_Dot_ref'  ,EPSPREF       ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('Fcut'             ,FCUT          ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV           ('fct_IDel'         ,IFUNC(13)     ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV         ('Fscale_el'        ,FSCALE_SIZ    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('EI_ref'           ,REF_SIZ       ,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
      CALL HM_GET_FLOATV         ('Epsilon_11tf'     ,ET1           ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('Epsilon_11tm'     ,ET1M          ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV           ('Fct_ID_11t'       ,IFUNC(1)      ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV         ('Epsilon_11cf'     ,EC1           ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('Epsilon_11cm'     ,EC1M          ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV           ('Fct_ID_11c'       ,IFUNC(4)      ,IS_AVAILABLE,LSUBMODEL)
!
      CALL HM_GET_FLOATV         ('Epsilon_22tf'     ,ET2           ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('Epsilon_22tm'     ,ET2M          ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV           ('Fct_ID_22t'       ,IFUNC(2)      ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV         ('Epsilon_22cf'     ,EC2           ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('Epsilon_22cm'     ,EC2M          ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV           ('Fct_ID_22c'       ,IFUNC(5)      ,IS_AVAILABLE,LSUBMODEL)
!
      CALL HM_GET_FLOATV         ('Epsilon_33tf'     ,ET3           ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('Epsilon_33tm'     ,ET3M          ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV           ('Fct_ID_33t'       ,IFUNC(7)      ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV         ('Epsilon_33cf'     ,EC3           ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('Epsilon_33cm'     ,EC3M          ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV           ('Fct_ID_33c'       ,IFUNC(8)      ,IS_AVAILABLE,LSUBMODEL)
!
      CALL HM_GET_FLOATV         ('Epsilon_12tf'     ,ET12          ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('Epsilon_12tm'     ,ET12M         ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV           ('Fct_ID_12t'       ,IFUNC(3)      ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV         ('Epsilon_12cf'     ,EC12          ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('Epsilon_12cm'     ,EC12M         ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV           ('Fct_ID_12c'       ,IFUNC(6)      ,IS_AVAILABLE,LSUBMODEL)
!
      CALL HM_GET_FLOATV         ('Epsilon_23tf'     ,ET23          ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('Epsilon_23tm'     ,ET23M         ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV           ('Fct_ID_23t'       ,IFUNC(9)      ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV         ('Epsilon_23cf'     ,EC23          ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('Epsilon_23cm'     ,EC23M         ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV           ('Fct_ID_23c'       ,IFUNC(10)     ,IS_AVAILABLE,LSUBMODEL)
!
      CALL HM_GET_FLOATV         ('Epsilon_31tf'     ,ET31          ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('Epsilon_31tm'     ,ET31M         ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV           ('Fct_ID_31t'       ,IFUNC(11)     ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV         ('Epsilon_31cf'     ,EC31          ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('Epsilon_31cm'     ,EC31M         ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV           ('Fct_ID_31c'       ,IFUNC(12)     ,IS_AVAILABLE,LSUBMODEL)
!
        !units 
      CALL HM_GET_FLOATV_DIM('Epsilon_Dot_ref'      ,EPSPREF_UNIT  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      IF (EPSPREF == ZERO) EPSPREF = ONE*EPSPREF_UNIT
!
      CALL HM_GET_FLOATV_DIM('Fscale_el'            ,FSCAL_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      IF (FSCALE_SIZ == ZERO) FSCALE_SIZ = ONE*FSCAL_UNIT
!
      CALL HM_GET_FLOATV_DIM('EI_ref'               ,REF_SIZ_UNIT  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      IF (REF_SIZ == ZERO) REF_SIZ = ONE *REF_SIZ_UNIT
c-----------------------------------------------------
c     default values
c-----------------------------------------------------
      IF (ET1  == ZERO) ET1 = EP20
      IF (ET1M == ZERO) ET1M = 1.2*ET1
      IF (ET2  == ZERO) ET2 = EP20
      IF (ET2M == ZERO) ET2M = 1.2*ET2
      IF (EC1  == ZERO) EC1 = EP20
      IF (EC1M == ZERO) EC1M = 1.2*EC1
      IF (EC2  == ZERO) EC2 = EP20
      IF (EC2M == ZERO) EC2M = 1.2*EC2
      IF (ET12 == ZERO) ET12 = EP20
      IF (ET12M == ZERO) ET12M = 1.2*ET12
      IF (EC12  == ZERO) EC12 = ET12
      IF (EC12M == ZERO) EC12M = 1.2*EC12
      IF (EC23  == ZERO) EC23 = ET23
      IF (EC23M == ZERO) EC23M = 1.2*EC23
      IF (EC31  == ZERO) EC31 = ET31
      IF (EC31M == ZERO) EC31M = 1.2*EC31
      IF (EC3   == ZERO) EC3 = ET3
      IF (EC3M  == ZERO) EC3M = 1.2*EC3
      IF (PTHK  == ZERO) PTHK = ONE
      IF (FCUT  == ZERO) FCUT = 10000.0D0*FAC_T_WORK
!
      IDAM = 0
      ISOLID = 0
      IF (STRDEF == 0) STRDEF = 1
      IF (IDAM == 0) IDAM = 1 ! => linear damage, default value for shells
c                    IDAM = 2   => linear stress softening, the only choice for solids
c-----------------------------------------------------
      UPARAM(1)  = IDAM    
      UPARAM(2)  = PTHK
      UPARAM(3)  = ISOLID   ! not used 
C
      UPARAM(4)   = ET1
      UPARAM(5)   = ET1M
      UPARAM(6)   = ET2
      UPARAM(7)   = ET2M
      UPARAM(8)   = EC1
      UPARAM(9)   = EC1M
      UPARAM(10)  = EC2
      UPARAM(11)  = EC2M
      UPARAM(12)  = ET12
      UPARAM(13)  = ET12M
      UPARAM(14)  = EC12
      UPARAM(15)  = EC12M
      UPARAM(16)  = FCUT
      UPARAM(17)  = EPSPREF
      UPARAM(18)  = ET3
      UPARAM(19)  = ET3M
      UPARAM(20)  = EC3
      UPARAM(21)  = EC3M
      UPARAM(22)  = ET23
      UPARAM(23)  = ET23M
      UPARAM(24)  = EC23
      UPARAM(25)  = EC23M
      UPARAM(22)  = ET31
      UPARAM(23)  = ET31M
      UPARAM(24)  = EC31
      UPARAM(25)  = EC31M
      UPARAM(26)  = FSCALE_SIZ
      UPARAM(27)  = REF_SIZ
      UPARAM(28)  = STRDEF
c-----------------
      NUPARAM = 28
      NUVAR   = 32
      NFUNC   = 13
c-----------------------------------------------------
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT, 1000)
       ELSE
        WRITE(IOUT, 1000) FAIL_ID,PTHK,STRDEF,EPSPREF,FCUT,
     .     IFUNC(13),FSCALE_SIZ,REF_SIZ,
     .     ET1,ET1M,IFUNC(1)   ,EC1,EC1M,IFUNC(4),
     .     ET2,ET2M,IFUNC(2)   ,EC2,EC2M,IFUNC(5),
     .     ET3,ET3M,IFUNC(7)   ,EC3,EC3M,IFUNC(8),
     .     ET12,ET12M,IFUNC(3) ,EC12,EC12M,IFUNC(6),
     .     ET23,ET23M,IFUNC(9) ,EC23,EC23M,IFUNC(10),
     .     ET31,ET31M,IFUNC(11),EC31,EC31M,IFUNC(12)
      ENDIF
c-----------------------------------------------------
1000  FORMAT(
     & 5X,'STRAIN RATE DEPENDANT ORTHOTROPIC FAILURE MODEL  '/,
     & 5X,'-----------------------------------------------  '/,
     & 5X,'FAILURE MODEL ID. . . . . . . . . . . . =',I10/,
     & 5X,'PTHK (SHELLS) . . . . . . . . . . . . . =',E12.4/,
     & 5X,'FAILURE STRAIN INPUT TYPE (STRDEF). . . =',I10/
     & 5X,' =1 (STRAIN MEASURE DEFINED IN PROPERTY) ',/                
     & 5X,' =2 (ENGINEERING STRAIN) . . . . . . . . ',/                
     & 5X,' =3 (TRUE STRAIN). . . . . . . . . . . . ',/                 
     & 5X,'REFERENCE STRAIN RATE . . . . . . . . . =',E12.4/,
     & 5X,'STRAIN RATE FILTERING FREQUENCY . . . . =',E12.4/,
     & 5X,'ELEMENT SIZE FACTOR FUNCTION. . . . . . =',I10/, 
     & 5X,'SCALE FACTOR OF ELEMENT SIZE FUNCTION . =',E12.4/,
     & 5X,'REFERENCE ELEMENT SIZE. . . . . . . . . =',E12.4/,
     & 5X,'FAILURE STRAIN TENSION 1  . . . . . . . =',E12.4/,
     & 5X,'MAXIMUM STRAIN TENSION 1  . . . . . . . =',E12.4/,
     & 5X,'STRAIN RATE FUNCTION TENSION 1  . . . . =',I10/,
     & 5X,'FAILURE STRAIN COMPRESSION 1  . . . . . =',E12.4/,
     & 5X,'MAXIMUM STRAIN COMPRESSION 1  . . . . . =',E12.4/,
     & 5X,'STRAIN RATE FUNCTION COMPRESSION 1  . . =',I10/,
     & 5X,'FAILURE STRAIN TENSION 2  . . . . . . . =',E12.4/,
     & 5X,'MAXIMUM STRAIN TENSION 2  . . . . . . . =',E12.4/,
     & 5X,'STRAIN RATE FUNCTION TENSION 2  . . . . =',I10/,
     & 5X,'FAILURE STRAIN COMPRESSION 2  . . . . . =',E12.4/,
     & 5X,'MAXIMUM STRAIN COMPRESSION 2  . . . . . =',E12.4/,
     & 5X,'STRAIN RATE FUNCTION COMPRESSION 2  . . =',I10/,
     & 5X,'FAILURE STRAIN TENSION 3  . . . . . . . =',E12.4/,
     & 5X,'MAXIMUM STRAIN TENSION 3  . . . . . . . =',E12.4/,
     & 5X,'STRAIN RATE FUNCTION TENSION 3  . . . . =',I10/,
     & 5X,'FAILURE STRAIN COMPRESSION 3  . . . . . =',E12.4/,
     & 5X,'MAXIMUM STRAIN COMPRESSION 3  . . . . . =',E12.4/,
     & 5X,'STRAIN RATE FUNCTION COMPRESSION 3  . . =',I10/,
     & 5X,'FAILURE STRAIN SHEAR TENSION 12 . . . . =',E12.4/,
     & 5X,'MAXIMUM STRAIN SHEAR TENSION 12 . . . . =',E12.4/,
     & 5X,'STRAIN RATE FUNCTION SHEAR TENSION 12 . =',I10/,
     & 5X,'FAILURE STRAIN SHEAR COMPRESSION 12 . . =',E12.4/,
     & 5X,'MAXIMUM STRAIN SHEAR COMPRESSION 12 . . =',E12.4/,
     & 5X,'STRAIN RATE FUNCTION SHEAR COMPRESSION 12=',I10/,
     & 5X,'FAILURE STRAIN SHEAR TENSION 23 . . . . =',E12.4/,
     & 5X,'MAXIMUM STRAIN SHEAR TENSION 23 . . . . =',E12.4/,
     & 5X,'STRAIN RATE FUNCTION SHEAR TENSION 23 . =',I10/,
     & 5X,'FAILURE STRAIN SHEAR COMPRESSION 23 . . =',E12.4/,
     & 5X,'MAXIMUM STRAIN SHEAR COMPRESSION 23 . . =',E12.4/,
     & 5X,'STRAIN RATE FUNCTION SHEAR COMPRESSION 23=',I10/,
     & 5X,'FAILURE STRAIN SHEAR TENSION 31 . . . . =',E12.4/,
     & 5X,'MAXIMUM STRAIN SHEAR TENSION 31 . . . . =',E12.4/,
     & 5X,'STRAIN RATE FUNCTION SHEAR TENSION 31 . =',I10/,
     & 5X,'FAILURE STRAIN SHEAR COMPRESSION 31 . . =',E12.4/,
     & 5X,'MAXIMUM STRAIN SHEAR COMPRESSION 31 . . =',E12.4/,
     & 5X,'STRAIN RATE FUNCTION SHEAR COMPRESSION 31=',I10/,
     & 5X,' -----------------------------------              ')
c----------- 
      RETURN
      END
