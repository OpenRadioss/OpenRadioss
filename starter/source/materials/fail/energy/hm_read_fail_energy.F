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
Chd|  HM_READ_FAIL_ENERGY           source/materials/fail/energy/hm_read_fail_energy.F
Chd|-- called by -----------
Chd|        HM_READ_FAIL                  source/materials/fail/hm_read_fail.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_FAIL_ENERGY(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR  ,IFUNC    ,
     .           MAXFUNC  ,NFUNC    ,ID     ,FAIL_ID  ,
     .           LSUBMODEL,UNITAB   )
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ FAILURE ENERGY (REPLACE LECR11)
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
C---s----1---------2---------3---------4---------5---------6---------7->
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
      INTEGER,INTENT(IN)             ::ID,FAIL_ID
      TYPE(SUBMODEL_DATA),INTENT(IN) ::LSUBMODEL(*)
C MODIFIED ARGUMENT
      INTEGER,INTENT(INOUT)          ::IFUNC(MAXFUNC),NFUNC,MAXFUNC,MAXUPARAM
      my_real,INTENT(INOUT)          ::UPARAM(MAXUPARAM)
      INTEGER,INTENT(INOUT)            ::NUPARAM,NUVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER     ::     IRFUN,DMG_FLAG
      my_real     ::     RF1,RF2,RIEF1,RIEF2,SCALE_EPSP,PTHKF
C-----------------------------------------------
       LOGICAL    ::     IS_AVAILABLE,IS_ENCRYPTED
C=======================================================================
      IS_ENCRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C=======================================================================
c     ENERGY Rupture Criterion
C=======================================================================
CARD1     2F_I_F_I) RIEF1,RIEF2,IRFUN,SCALE_EPSP,DMG_FLAG
      CALL HM_GET_FLOATV         ('E1'         ,RIEF1      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('E2'         ,RIEF2      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV           ('fct_ID'     ,IRFUN      ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV         ('xscale'     ,SCALE_EPSP ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV           ('I_Dam'      ,DMG_FLAG   ,IS_AVAILABLE,LSUBMODEL)
C=======================================================================
      IF (RIEF1 <= ZERO) RIEF1 = EP30
      IF (RIEF2 <= ZERO) RIEF2 = TWO*EP30      
      IF (DMG_FLAG == 0) DMG_FLAG = 1  ! DMG_FLAG = 1    => damage reduces forces after criteria is met
      IF (DMG_FLAG == 2) DMG_FLAG = 0  ! DMG_FLAG = 2->0 => damage reduces saved stress (old method)
C=======================================================================
      IF (RIEF1 > RIEF2) THEN
        CALL ANCMSG(MSGID=618,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=ID)
      ENDIF 
C
      PTHKF = EM06
      IF(SCALE_EPSP == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('xscale' ,SCALE_EPSP   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        SCALE_EPSP = ONE * SCALE_EPSP
      ENDIF
C=============================================  
      UPARAM(1) = RIEF1
      UPARAM(2) = RIEF2
      UPARAM(3) = PTHKF

      UPARAM(4) = ONE/SCALE_EPSP
      UPARAM(5) = DMG_FLAG   
      IFUNC(1)  = IRFUN
C
C=============================================  
      NUPARAM = 5
      NUVAR   = 0
      NFUNC   = 1
C=============================================  
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT, 1000)
      ELSE       
       WRITE(IOUT, 2000) RIEF1,RIEF2,IRFUN,SCALE_EPSP
      ENDIF
      RETURN
C=============================================  
 1000 FORMAT(
     & 5X,40H    CRYPTED DATA IN FAILURE MODEL             /,
     & 5X,40H    -----------------------------             /)
 2000 FORMAT(
     & 5X,40H    ENERGY   FAILURE MODEL                  /,
     & 5X,40H    ------------------------                  /,
     & 5X,'MAXIMUM SPECIFIC ENERGY 1. . . . . . . =',E12.4/
     & 5X,'MAXIMUM SPECIFIC ENERGY 2. . . . . . . =',E12.4/
     & 5X,'MAXIMUM ENERGIES SCALING FUNCTION. . . =',I8/
     & 5X,'ABSCISSA SCALE FACTOR FOR FUNCTION . . =',E12.4/)       
c----------- 
      RETURN
      END
