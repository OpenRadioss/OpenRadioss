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
Chd|  HM_READ_FAIL_TBUTCHER         source/materials/fail/tuler_butcher/hm_read_fail_tbutcher.F
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
      SUBROUTINE HM_READ_FAIL_TBUTCHER(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR  ,IFUNC    ,
     .           MAXFUNC  ,NFUNC    ,UNITAB   ,ID     ,FAIL_ID  ,
     .           LSUBMODEL,IXFEM)
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
      INTEGER,INTENT(INOUT)          ::NUPARAM,NUVAR,IXFEM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ISHELL, ISOLID,IBRIT
      my_real
     .  TBA,TBK,SIGR,FAC_L,FAC_T,FAC_M,FAC_C,BRIT_B,BRIT_C,DADV
      LOGICAL    ::     IS_AVAILABLE,IS_CRYPTED
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_CRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.

      CALL HM_OPTION_IS_CRYPTED(IS_CRYPTED)
C
      CALL HM_GET_FLOATV         ('Lambda'           ,TBA     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('K'           ,TBK     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('Sigma_r'           ,SIGR     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV           ('Ifail_sh'     ,ISHELL ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV           ('Ifail_so'     ,ISOLID ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV           ('Iduct'     ,IBRIT ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV           ('Ixfem'     ,IXFEM ,IS_AVAILABLE,LSUBMODEL)
C
      CALL HM_GET_FLOATV         ('a_TBUTCHER_XFEMTBUTC',BRIT_B  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('b_TBUTCHER_XFEMTBUTC'        ,BRIT_C   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('Dadv'        ,DADV   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF (DADV == ZERO)DADV=ZEP85
      IF (DADV > ONE)THEN
         DADV = ONE
         CALL ANCMSG(MSGID=1049,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO,
     .               I1=ID)
      ENDIF
C
      IF(ISHELL.EQ.0)ISHELL=1
      IF(ISOLID.EQ.0)ISOLID= 1
      IF(IBRIT.EQ.0)IBRIT= 1
      IF(IXFEM /= 1 .AND. IXFEM /= 2)IXFEM = 0
      IF(IXFEM > 0)ISOLID = 0
      IF(IXFEM == 0)IBRIT = 0
      IF(SIGR <= ZERO)SIGR=EP30
      IF(TBK <= ZERO)TBK=EP30
      UPARAM(1) = TBA
      UPARAM(2) = TBK
      UPARAM(3) = SIGR
      UPARAM(4) = ISHELL
      UPARAM(5) = ISOLID
      UPARAM(6) = IBRIT
      UPARAM(7) = IXFEM
      UPARAM(8) = BRIT_B
      UPARAM(9) = BRIT_C
      UPARAM(10)= DADV
C
      NUPARAM = 10
C
      NUVAR = 2
      NFUNC = 0
C
C
      IF(IS_CRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE 
C---
C for shell      
C---
        IF(IXFEM == 0)THEN
          WRITE(IOUT, 1000)TBA,TBK,SIGR,IXFEM
          IF(ISHELL.EQ.1)THEN
            WRITE(IOUT, 1100)
          ELSEIF(ISHELL.EQ.2)THEN
            WRITE(IOUT, 1200)
          ENDIF
        ELSE
          WRITE(IOUT, 1010)TBA,TBK,SIGR,IXFEM,IBRIT,BRIT_B,BRIT_C,DADV
          WRITE(IOUT, 1400)
        END IF
C---
C for solid      
C---
        IF(IXFEM == 0)THEN ! XFEM yet not available for solids
          IF(ISOLID.EQ.1)THEN
            WRITE(IOUT, 2100)
          ELSEIF(ISOLID.EQ.2)THEN
            WRITE(IOUT, 2200)
          ENDIF
        END IF
      ENDIF      
C
 1000 FORMAT(
     & 5X,40H    TULER BUTCHER DAMAGE PARAMETER            /,
     & 5X,40H    -----------------------------             /,
     & 5X,40HEXPONENT LAMBDA . . . . . . . . . . . .=,E12.4/,
     & 5X,40HDAMAGE INTEGRAL  K. . . . . . . . . . .=,E12.4/,
     & 5X,40HFRACTURE STRESS . . . . . . . . . . . .=,E12.4/,
     & 5X,40HXFEM FAILURE FLAG . . . . . . . . . . .=,I10//)
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
 1010 FORMAT(
     & 5X,40H    XFEM TULER BUTCHER DAMAGE PARAMETER       /,
     & 5X,40H    -----------------------------             /,
     & 5X,40HEXPONENT LAMBDA . . . . . . . . . . . .=,E12.4/,
     & 5X,40HDAMAGE INTEGRAL  K. . . . . . . . . . .=,E12.4/,
     & 5X,40HFRACTURE STRESS . . . . . . . . . . . .=,E12.4/,
     & 5X,40HXFEM FAILURE FLAG . . . . . . . . . . .=,I10/,
     & 5X,40HDUCTILE/BRITTLE FAILURE FLAG  . . . . .=,I10/,
     & 5X,40HBRITTLE FRACTURE EXPONENT A  . . . . . =,E12.4/,
     & 5X,40HBRITTLE FRACTURE EXPONENT B  . . . . . =,E12.4/,
     & 5X,40HCRITICAL ADVANCEMENT COEFFICIENT . . . =,E12.4//)
C-----------


      RETURN
      END
