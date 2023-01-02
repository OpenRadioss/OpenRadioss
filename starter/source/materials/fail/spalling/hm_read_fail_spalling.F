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
Chd|  HM_READ_FAIL_SPALLING         source/materials/fail/spalling/hm_read_fail_spalling.F
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
      SUBROUTINE HM_READ_FAIL_SPALLING(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR  ,IFUNC    ,
     .           MAXFUNC  ,NFUNC    ,UNITAB   ,ID     ,FAIL_ID  ,
     .           LSUBMODEL)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ FAILURE SPALLING (REPLACE LECR08)
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
#include      "scr17_c.inc"
#include      "scr03_c.inc"
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
      INTEGER,INTENT(INOUT)          ::NUPARAM,NUVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ISHELL, ISOLID,nrate
      my_real D1,D2,D3,D4,D5,EPSP0,PMIN
      LOGICAL    ::     IS_AVAILABLE,IS_ENCRYPTED
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_ENCRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.

      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)

      CALL HM_GET_FLOATV         ('D1'           ,D1     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('D2'           ,D2     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('D3'           ,D3     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('D4'           ,D4     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('D5'           ,D5     ,IS_AVAILABLE,LSUBMODEL,UNITAB)

      CALL HM_GET_FLOATV         ('Epsilon_Dot_0',EPSP0  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('P_min'        ,PMIN   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV           ('Ifail_so'     ,ISOLID ,IS_AVAILABLE,LSUBMODEL)

      IF(EPSP0==0)EPSP0=EM20
      IF(PMIN==0)PMIN=-EP30
      IF(ISOLID==0)ISOLID=1
      UPARAM(1) = D1
      UPARAM(2) = D2
      UPARAM(3) = D3
      UPARAM(4) = D4
      UPARAM(5) = D5
      UPARAM(6) = EPSP0
      UPARAM(7) = PMIN
      UPARAM(8) = ISOLID
      NUPARAM = 8
      NUVAR   = 1
      NFUNC   = 0

      IF(IS_ENCRYPTED)THEN
       WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE       
       WRITE(IOUT, 1000)D1,D2,D3,D4,D5,PMIN         
       IF(ISOLID==1)THEN
         WRITE(IOUT, 1100)
       ELSEIF(ISOLID==2)THEN
         WRITE(IOUT, 1200)
       ELSEIF(ISOLID==3)THEN
         WRITE(IOUT, 1300)
       ELSEIF(ISOLID==4)THEN
         WRITE(IOUT, 1400)  
       ENDIF

       IF(D3>0.)THEN
          CALL ANCMSG(MSGID=831,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO,
     .                I1=ID)
       ENDIF
C
      ENDIF
C               
 1000 FORMAT(
     & 5X, ' JOHNSON COOK FAILURE MODEL AND SPALLING  ',/,
     & 5X, ' --------------------------------------- ' ,/,
     & 5X,'FIRST FAILURE PARAMETER (D1).. . . . . =',E12.4/
     & 5X,'SECOND FAILURE PARAMETER(D2). . . . . .=',E12.4/
     & 5X,'THIRD FAILURE PARAMETER (D3). . . . . .=',E12.4/
     & 5X,'FORTH FAILURE PARAMETER (D4). . . . . .=',E12.4/
     & 5X,'FIFTH FAILURE PARAMETER (D5).   . . . .=',E12.4/
     & 5X,'LIMIT PRESSURE . . . . . . ..   . . . .=',E12.4//)
  
 1100 FORMAT(
     & 5X,'    NO JOHNSON COOK DAMAGE AND ALLOWS  SPALL ')      
 1200 FORMAT(
     & 5X,'    JOHNSON COOK DAMAGE WITH ELEMENT DELETE',/ 
     & 5X, '           AND ALLOWS  SPALL              ') 
 1300 FORMAT(
     & 5X,'  JOHNSON COOK DAMAGE WITHOUT ELEMENT DELETE ',/
     & 5X,'          AND ALLOWS  SPALL           '    )
 1400 FORMAT(
     & 5X,'    JOHNSON COOK DAMAGE AND ALLOWS  SPALL',/ 
     & 5X, '           WITH ELEMENT DELETE          ')   

      RETURN
      END
