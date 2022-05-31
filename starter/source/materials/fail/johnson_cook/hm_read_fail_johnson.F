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
Chd|  HM_READ_FAIL_JOHNSON          source/materials/fail/johnson_cook/hm_read_fail_johnson.F
Chd|-- called by -----------
Chd|        HM_READ_FAIL                  source/materials/fail/hm_read_fail.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_CRYPTED          source/devtools/hm_reader/hm_option_is_crypted.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_FAIL_JOHNSON(
     .           UPARAM    ,MAXUPARAM ,NUPARAM ,NUVAR  ,IFUNC  ,
     .           MAXFUNC   ,NFUNC     ,UNITAB  ,IMID_F ,FAIL_ID,
     .           LSUBMODEL ,IXFEM     )
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ FAILURE JOHNSON
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
C FAIL_ID  |  1      | I | W | ID OF FAILURE CRITERIA
C----------+---------+---+---+--------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER,INTENT(IN)             ::MAXUPARAM,MAXFUNC,FAIL_ID,IMID_F
      TYPE(SUBMODEL_DATA),INTENT(IN) ::LSUBMODEL(*)
C MODIFIED ARGUMENT
      INTEGER,INTENT(INOUT)          ::IFUNC(MAXFUNC),NFUNC,IXFEM
      my_real,INTENT(INOUT)          ::UPARAM(MAXUPARAM)
      INTEGER,INTENT(OUT)            ::NUPARAM,NUVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ISHELL,ISOLID
      my_real D1,D2,D3,D4,D5,EPSP0,UNIT_T,P_THICKFAIL,DADV
      LOGICAL :: IS_AVAILABLE,IS_CRYPTED
C-----------------------------------------------    
!!      FAC_T = UNITAB(4) 
      IS_CRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_CRYPTED(IS_CRYPTED)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
      CALL HM_GET_FLOATV         ('D1'           ,D1      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('D2'           ,D2      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('D3'           ,D3      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('D4'           ,D4      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('D5'           ,D5      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('Epsilon_Dot_0',EPSP0   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV           ('Ifail_sh'     ,ISHELL  ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV           ('Ifail_so'     ,ISOLID  ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV         ('Dadv         ',DADV    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV           ('Ixfem'        ,IXFEM   ,IS_AVAILABLE,LSUBMODEL)
!
      P_THICKFAIL = ZERO
!!      EPSP0 =  EPSP0 / FAC_T
      IF (EPSP0 == ZERO) EPSP0=EM20
      IF (ISHELL == 0)   ISHELL=1
      IF (ISOLID == 0)   ISOLID=1
      IF (IXFEM /= 1 .AND. IXFEM /= 2) IXFEM = 0
      IF (IXFEM > 0)     ISOLID = 0
      IF (DADV == ZERO .OR. DADV > ONE) DADV = ONE
!
      UPARAM(1) = D1
      UPARAM(2) = D2
      UPARAM(3) = D3
      UPARAM(4) = D4
      UPARAM(5) = D5
      UPARAM(6) = EPSP0
      UPARAM(7) = ISHELL
      UPARAM(8) = ISOLID
      UPARAM(9) = P_THICKFAIL
      UPARAM(10)= DADV
      UPARAM(11)= IXFEM
      NUPARAM = 11
      NUVAR = 0
      NFUNC = 0
C
      IF (IS_CRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE 
C---
C for shell      
C---
        IF(IXFEM == 0)THEN
          WRITE(IOUT, 1000)D1,D2,D3,D4,D5
          IF(ISHELL == 1)THEN
            WRITE(IOUT, 1100)
          ELSEIF(ISHELL == 2)THEN
            WRITE(IOUT, 1200)
          ENDIF
        ELSE
          WRITE(IOUT, 1010)D1,D2,D3,D4,D5,IXFEM,DADV
          WRITE(IOUT, 1400)
        END IF
C---
C for solid      
C---
        IF(IXFEM == 0)THEN ! XFEM yet not available for solids
          IF(ISOLID == 1)THEN
            WRITE(IOUT, 2100)
          ELSEIF(ISOLID == 2)THEN
            WRITE(IOUT, 2200)
          ENDIF
        ENDIF
C
        IF(D3 > ZERO)THEN
          CALL ANCMSG(MSGID=831,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO,
     .                I1=IMID_F)
        ENDIF
      ENDIF ! IF (IS_CRYPTED)
C
 1000 FORMAT(
     & 5X,40H  JOHNSON COOK DAMAGE PARAMETER         ,/,
     & 5X,40H  -----------------------------         ,/,
     & 5X,'FIRST FAILURE PARAMETER (D1).. . . . . =',E12.4/
     & 5X,'SECOND FAILURE PARAMETER(D2). . . . . .=',E12.4/
     & 5X,'THIRD FAILURE PARAMETER (D3). . . . . .=',E12.4/
     & 5X,'FORTH FAILURE PARAMETER (D4). . . . . .=',E12.4/
     & 5X,'FIFTH FAILURE PARAMETER (D5).   . . . .=',E12.4//)
 1100 FORMAT(
     & 5X,'   SHELL ELEMENT DELETION AFTER FAILURE') 
 2100 FORMAT(
     & 5X,'   SOLID ELEMENT DELETION AFTER FAILURE') 

 1200 FORMAT(
     & 5X,'   STRESS TENSOR IN SHELL LAYER SET TO ZERO AFTER FAILURE')   
 1400 FORMAT(
     & 5X,'   SHELL ELEMENT CRACKING AFTER FAILURE')   
 2200 FORMAT(
     & 5X,'   DEVIATORIC STRESS IN SOLID WILL VANISH AFTER FAILURE')    
C
 1010 FORMAT(
     & 5X,40H  XFEM JOHNSON COOK DAMAGE PARAMETER    ,/,
     & 5X,40H  -----------------------------         ,/,
     & 5X,'FIRST FAILURE PARAMETER (D1). . . . . .=',E12.4/
     & 5X,'SECOND FAILURE PARAMETER(D2). . . . . .=',E12.4/
     & 5X,'THIRD FAILURE PARAMETER (D3). . . . . .=',E12.4/
     & 5X,'FORTH FAILURE PARAMETER (D4). . . . . .=',E12.4/
     & 5X,'FIFTH FAILURE PARAMETER (D5). . . . . .=',E12.4/
     & 5X,'FLAG XFEM . . . . . . . . . . . . . . .=',I10/,
     & 5X,'CRITICAL ADVANCEMENT VALUE. . . . . . .=',E12.4//)
C-----------
      RETURN
      END

