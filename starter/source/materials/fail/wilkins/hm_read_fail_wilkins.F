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
Chd|  HM_READ_FAIL_WILKINS          source/materials/fail/wilkins/hm_read_fail_wilkins.F
Chd|-- called by -----------
Chd|        HM_READ_FAIL                  source/materials/fail/hm_read_fail.F
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_FAIL_WILKINS(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR  ,IFUNC    ,
     .           MAXFUNC  ,NFUNC    ,IXFEM    ,ID     ,FAIL_ID  ,
     .           LSUBMODEL,UNITAB   )
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ FAILURE WILKINS (replace LECR03)
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
      INTEGER,INTENT(INOUT)          ::IFUNC(MAXFUNC),NFUNC,MAXFUNC,MAXUPARAM,IXFEM
      my_real,INTENT(INOUT)          ::UPARAM(MAXUPARAM)
      INTEGER,INTENT(OUT)            ::NUPARAM,NUVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER    ::     IMARGIN,IENG
      my_real    ::     RANI,DADV,FACT_MARGIN,FACT_LOOSEMETAL
      INTEGER    ::     ISHELL, ISOLID
      my_real    ::     PC,ALPHA,BETA,DF
      LOGICAL    ::     IS_AVAILABLE,IS_ENCRYPTED
C--------------------------------------------------
C   S o u r c e   L i n e s
C--------------------------------------------------
      IS_ENCRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.
      
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)

      CALL HM_GET_FLOATV    ('Alpha'         ,ALPHA     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV    ('Beta_WILKINS'  ,BETA      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV    ('Plim'          ,PC        ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV    ('Df'            ,DF        ,IS_AVAILABLE,LSUBMODEL,UNITAB)

      CALL HM_GET_INTV      ('Ifail_sh'      ,ISHELL    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV      ('Ifail_so'      ,ISOLID    ,IS_AVAILABLE,LSUBMODEL)
            
      IF(ISHELL==0)ISHELL=1
      IF(ISOLID==0)ISOLID=1
      IF(PC == ZERO)PC=EP30
      IF(DF <= ZERO) DF=EP30
      UPARAM(1) = ALPHA
      UPARAM(2) = BETA
      UPARAM(3) = PC
      UPARAM(4) = DF
      UPARAM(5) = ISHELL
      UPARAM(6) = ISOLID
      NUPARAM = 6
      NUVAR = 1
      NFUNC = 0
      
      IF(IS_ENCRYPTED)THEN
       WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE       
       WRITE(IOUT, 1000)ALPHA,BETA,PC,DF     
       IF(ISHELL==1)THEN
        WRITE(IOUT, 1100)
       ELSEIF(ISHELL==2)THEN
        WRITE(IOUT, 1200)
       ENDIF   
       IF(ISOLID==1)THEN
        WRITE(IOUT, 2100)
       ELSEIF(ISOLID==2)THEN
        WRITE(IOUT, 2200)
       ENDIF
      ENDIF         
 1000 FORMAT(
     & 5X,40H    WILKINS DAMAGE MODEL                  /,
     & 5X,40H    --------------------                  /,
     & 5X,40HHYDROSTATIC PRESSURE COEFFICIENT. . . .=,1PE20.13/,
     & 5X,40HDEVIATORC COEFFICIENT   . . . . . . . .=,1PE20.13/,
     & 5X,40HLIMITE PRESSION     . . . . . . . . . .=,1PE20.13/,
     & 5X,40HCRITICAL DAMAGE     . . . . . . . . . .=,1PE20.13/     )
 1100 FORMAT(
     & 5X,'   SHELL ELEMENT DELETION ') 
 2100 FORMAT(
     & 5X,'   SOLID ELEMENT  DELETION ') 

 1200 FORMAT(
     & 5X,'    SHELL TENSOR STRESS OF LAYER WILL BE VANISHED ')    
 2200 FORMAT(
     & 5X,'    DEVIATORIC STRESS WILL BE VANISHED ')    
      RETURN
      END
