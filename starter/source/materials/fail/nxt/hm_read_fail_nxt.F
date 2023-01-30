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
Chd|  HM_READ_FAIL_NXT              source/materials/fail/nxt/hm_read_fail_nxt.F
Chd|-- called by -----------
Chd|        HM_READ_FAIL                  source/materials/fail/hm_read_fail.F
Chd|-- calls ---------------
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_FAIL_NXT(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR  ,IFUNC    ,
     .           MAXFUNC  ,NFUNC    ,ID     ,FAIL_ID  ,
     .           LSUBMODEL,UNITAB   )
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ FAILURE TENSSTRAIN (REPLACE LECR10)
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
      INTEGER,INTENT(INOUT)            ::NUPARAM,NUVAR
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      INTEGER    ::     ISHELL
C-----------------------------------------------
      LOGICAL    ::     IS_AVAILABLE,IS_ENCRYPTED
C=======================================================================
      IS_ENCRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C--------------------------------------------------
C EXTRACT DATAS  
C--------------------------------------------------
      ISHELL = 0
Card1      
      CALL HM_GET_INTV           ('fct_ID1'     ,IFUNC(1) ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV           ('fct_ID2'     ,IFUNC(2) ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV           ('Ifail_sh'    ,ISHELL   ,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
      UPARAM(1) = ISHELL
C
      NUPARAM = 1
      NUVAR   = 2
      NFUNC   = 2
C
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT, 1000)
      ELSE    
       IF(ISHELL == 0) THEN   
        WRITE(IOUT, 1001)IFUNC(1),IFUNC(2)
       ELSE
        WRITE(IOUT, 1100)IFUNC(1),IFUNC(2)
       ENDIF
      ENDIF       
C-----------
 1000 FORMAT(
     & 5X,40H    CRYPTED DATA IN FAILURE MODEL             /,
     & 5X,40H    -----------------------------             /)
 1001 FORMAT(
     & 5X,40H     NXT FAILURE  CRITERIA                     /,
     & 5X,40H    ------------------------                  /,
     & 5X,'  NO SHELL ELEMENT DELETION AFTER FAILURE' /,
     & 5X,'S-R CURVE FORMING LIMIT DIAGRAM . . . . . . . =',I10/,
     & 5X,'3D  CURVE FORMING LIMIT DIAGRAM . . . . . . . =',I10//)
 1100 FORMAT(
     & 5X,40H     NXT FAILURE  CRITERIA                     /,
     & 5X,40H    ------------------------                  /,
     & 5X,'   SHELL ELEMENT DELETION AFTER FAILURE' /,
     & 5X,'S-R CURVE FORMING LIMIT DIAGRAM . . . . . . . =',I10/,
     & 5X,'3D  CURVE FORMING LIMIT DIAGRAM . . . . . . . =',I10//)
C-----------
      RETURN
      END








