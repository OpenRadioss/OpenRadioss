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
Chd|  HM_READ_FAIL_SNCONNECT        source/materials/fail/snconnect/hm_read_fail_snconnect.F
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
      SUBROUTINE HM_READ_FAIL_SNCONNECT(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR  ,IFUNC    ,
     .           MAXFUNC  ,NFUNC    ,UNITAB   ,ID     ,FAIL_ID  ,
     .           LSUBMODEL)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ FAILURE SNCONNECT
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
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER,INTENT(IN)             ::ID,FAIL_ID
       TYPE(SUBMODEL_DATA),INTENT(IN) ::LSUBMODEL(*)
C MODIFIED ARGUMENT
      INTEGER,INTENT(INOUT)          ::IFUNC(MAXFUNC),NFUNC,MAXFUNC,MAXUPARAM
      my_real,INTENT(INOUT)          ::UPARAM(MAXUPARAM)
      INTEGER,INTENT(OUT)            ::NUPARAM,NUVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      INTEGER    ::     ID_0N,ID_0S,ID_FN,ID_FS,ISOLID,NFAIL,ISYM 
      my_real    ::     A2, B2 , A3, B3,XSCALE0,XSCALEF,AREASCALE,
     .                  XSCALE0_UNIT,XSCALEF_UNIT
C-----------------------------------------------
      LOGICAL    ::     IS_AVAILABLE,IS_ENCRYPTED
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
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
Card1
      CALL HM_GET_FLOATV         ('Alpha_0' ,A2    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('Beta_0'  ,B2    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('Alpha_f' ,A3    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('Beta_f'  ,B3    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV           ('Ifail_so',ISOLID,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV           ('ISYM'    ,ISYM  ,IS_AVAILABLE,LSUBMODEL)
Card2     
      CALL HM_GET_INTV           ('fct_IDON'  ,ID_0N     ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV           ('fct_IDOS'  ,ID_0S     ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV           ('fct_IDFN'  ,ID_FN     ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV           ('fct_IDFS'  ,ID_FS     ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV         ('XSCALE_0'  ,XSCALE0   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('XSCALE_F'  ,XSCALEF   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('AREA_scale',AREASCALE ,IS_AVAILABLE,LSUBMODEL,UNITAB)
C-------------------------------------------- 
      IF (XSCALE0 == ZERO) THEN 
         CALL HM_GET_FLOATV_DIM('XSCALE0' ,XSCALE0_UNIT  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
         XSCALE0 = ONE * XSCALE0_UNIT
      ENDIF

      IF (XSCALEF == ZERO)THEN 
         CALL HM_GET_FLOATV_DIM('XSCALEF' ,XSCALEF_UNIT  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
         XSCALEF = ONE * XSCALEF_UNIT
      ENDIF 
C-------------------------------------------- 
      NFAIL  = 1
      IF (ISOLID == 0) THEN
        ISOLID = 1
      ELSE IF (ISOLID == 2) THEN
        NFAIL = 4                ! all Gauss points must fail
      END IF  
      IF (B2 == ZERO) B2 = ONE
      IF (B3 == ZERO) B3 = ONE
c-----------------------------------------------------------------------
      UPARAM(1)  = A2
      UPARAM(2)  = B2
      UPARAM(3)  = A3
      UPARAM(4)  = B3
      UPARAM(5)  = NFAIL
      UPARAM(6)  = XSCALE0
      UPARAM(7)  = XSCALEF
      UPARAM(8)  = ISYM
      UPARAM(9)  = AREASCALE
C    
      IFUNC(1) = ID_0N
      IFUNC(2) = ID_0S
      IFUNC(3) = ID_FN
      IFUNC(4) = ID_FS
C    
      NUPARAM = 9
      NFUNC   = 4
      NUVAR   = 3
c-----------------------------------------------------------------------
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT, 1000)
      ELSE
        WRITE(IOUT, 1100) A2,B2,A3,B3,ISOLID,ISYM,
     .                    ID_0N,ID_0S,ID_FN,ID_FS,XSCALE0,XSCALEF                  
      ENDIF
C-----------
      RETURN
C-----------
 1000 FORMAT(
     & 5X,'    CRYPTED DATA IN FAILURE MODEL        '/,
     & 5X,'    -----------------------------        '/)
 1100 FORMAT(
     & 5X,'    SNCONNECT                            '/,
     & 5X,'    ------------------------             '/,
     & 5X,'CONSTANT ALPHA INITIAL . . . . . . . . .=',1PG20.13/,
     & 5X,'CONSTANT BETA INITIAL. . . . . . . . . .=',1PG20.13/,
     & 5X,'CONSTANT ALPHA FINAL . . . . . . . . . .=',1PG20.13/,
     & 5X,'CONSTANT BETA FINAL . .. . . . . . . . .=',1PG20.13/,
     & 5X,'ISOLID FLAG  . . . . . . . . . . . . . .=',I10/,
     & 5X,'RUPTURE DEACTIVATION FLAG IN COMPRESSION=',I10/,
     & 5X,'FUCNCTION ID INITIAL NORMAL. . . . . . .=',I10/,
     & 5X,'FUCNCTION ID INITIAL TANGENTIAL  . . . .=',I10/,
     & 5X,'FUCNCTION ID FINAL NORMAL .  . . . . . .=',I10/,
     & 5X,'FUCNCTION ID FINAL TANGENTIAL  . . . . .=',I10/,
     & 5X,'ABSCISSA SCALE FOR INITIAL CURVES  . . .=',1PG20.13/,
     & 5X,'ABSCISSA SCALE FOR FINAL CURVES  . . . .=',1PG20.13)
C-----------       
      RETURN
      END
