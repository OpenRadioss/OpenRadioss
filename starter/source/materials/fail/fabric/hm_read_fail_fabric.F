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
Chd|  HM_READ_FAIL_FABRIC           source/materials/fail/fabric/hm_read_fail_fabric.F
Chd|-- called by -----------
Chd|        HM_READ_FAIL                  source/materials/fail/hm_read_fail.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_FAIL_FABRIC(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR  ,IFUNC    ,
     .           MAXFUNC  ,NFUNC    ,UNITAB   ,MAT_ID ,FAIL_ID  ,
     .           LSUBMODEL,UNIT_ID)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ ANISOTROPIC FABRIC FAILURE MODEL
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
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER,INTENT(IN)             ::MAT_ID,FAIL_ID,UNIT_ID
      TYPE(SUBMODEL_DATA),INTENT(IN) ::LSUBMODEL(*)
C MODIFIED ARGUMENT
      INTEGER,INTENT(INOUT)          ::IFUNC(MAXFUNC),NFUNC,MAXFUNC,MAXUPARAM
      my_real,INTENT(INOUT)          ::UPARAM(MAXUPARAM)
      INTEGER,INTENT(INOUT)            ::NUPARAM,NUVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  :: IRFUN,NDIR
      my_real  :: EPSF1,EPSR1,EPSF2,EPSR2,FAC_T
      LOGICAL  :: IS_AVAILABLE,IS_ENCRYPTED
C=======================================================================
      IS_ENCRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.
c
      IF (UNIT_ID > 0) THEN
        FAC_T = UNITAB%FAC_T(UNIT_ID)
      ELSE
        FAC_T = ONE
      ENDIF     
C--------------------------------------------------
C     check crypting
C--------------------------------------------------
c
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
c
C--------------------------------------------------
C     Read DATA
C--------------------------------------------------
      CALL HM_GET_FLOATV ('Epsilon_f1'  ,EPSF1     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('Epsilon_r1'  ,EPSR1     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('Epsilon_f2'  ,EPSF2     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('Epsilon_r2'  ,EPSR2     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV   ('NDIR'        ,NDIR      ,IS_AVAILABLE,LSUBMODEL)
c
      CALL HM_GET_INTV   ('fct_ID'      ,IRFUN     ,IS_AVAILABLE,LSUBMODEL)
c--------------------------------------------------
c     Default values
c--------------------------------------------------
      IF (EPSF1 <= ZERO) EPSF1 = EP20
      IF (EPSR1 <= ZERO) EPSR1 = EP20*TWO
      IF (EPSF2 <= ZERO) EPSF2 = EP20
      IF (EPSR2 <= ZERO) EPSR2 = EP20*TWO
      IF (NDIR == 0) NDIR = 2
      NDIR = MIN(NDIR,2)
      NDIR = MAX(NDIR,1)
c          
      IF (EPSF1 > EPSR1 .or. EPSF2 > EPSR2) THEN
        CALL ANCMSG(MSGID=617,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=MAT_ID)
      ENDIF     
c--------------------------------------------------
      UPARAM(1) = EPSF1 
      UPARAM(2) = EPSR1
      UPARAM(3) = EPSF2
      UPARAM(4) = EPSR2
      UPARAM(5) = FAC_T   ! abscissa unit scaling factor (strain rate)
      UPARAM(6) = NDIR
c-----------------------------------------------------
      IFUNC(1) = IRFUN
c-----------------------------------------------------
      NUPARAM = 6
      NUVAR   = 2
      NFUNC   = 1
c-----------------------------------------------------
      IF (IS_ENCRYPTED)THEN
        WRITE(IOUT, 1000)
      ELSE
        WRITE(IOUT, 1100) EPSF1,EPSR1,EPSF2,EPSR2,NDIR,IRFUN
      ENDIF
c-----------
      RETURN
c-------------------------------------------------- 
 1000 FORMAT(
     & 5X,'    CRYPTED DATA IN FAILURE MODEL             '/,
     & 5X,'    -----------------------------             '/)
 1100 FORMAT(
     & 5X,'    ANISOTROPIC FABRIC FAILURE MODEL          ',/
     & 5X,'    ------------------------                  ',/
     & 5X,'FAILURE TENSION STRAIN DIRECTION 1 . . =',E12.4/
     & 5X,'RUPTURE TENSION STRAIN DIRECTION 1 . . =',E12.4/
     & 5X,'FAILURE TENSION STRAIN DIRECTION 2 . . =',E12.4/
     & 5X,'RUPTURE TENSION STRAIN DIRECTION 2 . . =',E12.4/
     & 5X,'NUMBER OF BROKEN DIR. FOR EL. DELETION =',I10/
     & 5X,'STRAIN RATE SCALING FUNCTION . . . . . =',I10)
c-------------------------------------------------- 
      END
