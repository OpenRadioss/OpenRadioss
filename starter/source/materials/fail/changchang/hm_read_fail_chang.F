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
Chd|  HM_READ_FAIL_CHANG            source/materials/fail/changchang/hm_read_fail_chang.F
Chd|-- called by -----------
Chd|        HM_READ_FAIL                  source/materials/fail/hm_read_fail.F
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_CRYPTED          source/devtools/hm_reader/hm_option_is_crypted.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_FAIL_CHANG(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR  ,IFUNC    ,
     .           MAXFUNC  ,NFUNC    ,IXFEM    ,ID     ,FAIL_ID  ,
     .           LSUBMODEL,UNITAB   )
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ FAILURE CHANG (replace LECR13)
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
      INTEGER ISHELL 
      my_real 
     .    SIGT1,SIGT2,SIGT12,SIGC1,SIGC2,BETA,TMAX
      LOGICAL    ::     IS_AVAILABLE,IS_CRYPTED
C--------------------------------------------------
C   S o u r c e   L i n e s
C--------------------------------------------------
      IS_CRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.
      
      CALL HM_OPTION_IS_CRYPTED(IS_CRYPTED)

      CALL HM_GET_FLOATV    ('Sigma_1t'   ,SIGT1      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV    ('Sigma_2t'   ,SIGT2      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV    ('Sigma_12'   ,SIGT12     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV    ('Sigma_1c'   ,SIGC1      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV    ('Sigma_2c'   ,SIGC2      ,IS_AVAILABLE,LSUBMODEL,UNITAB)

      CALL HM_GET_FLOATV    ('Beta'       ,BETA       ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV    ('Tau_max'    ,TMAX       ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV      ('Ifail_sh'   ,ISHELL     ,IS_AVAILABLE,LSUBMODEL)
            
      IF (SIGT1==ZERO) SIGT1 = EP30
      IF (SIGT2==ZERO) SIGT2 = EP30
      IF (SIGT12==ZERO) SIGT12 = EP30
      IF (SIGC1==ZERO) SIGC1 = EP30
      IF (SIGC2==ZERO) SIGC2 = EP30
      IF(TMAX==ZERO) TMAX=EP30
      IF(ISHELL==0) ISHELL = 1               

      UPARAM(1) = SIGT1
      UPARAM(2) = SIGT2
      UPARAM(3) = SIGT12
      UPARAM(4) = SIGC1
      UPARAM(5) = SIGC2
      UPARAM(6) = BETA
      UPARAM(7) = TMAX
      UPARAM(8) = ISHELL

      NUPARAM = 8
      NUVAR   = 1
      NFUNC   = 0

      IF(IS_CRYPTED)THEN
      
      ELSE
       WRITE(IOUT, 1000)SIGT1,SIGT2,SIGT12,SIGC1,SIGC2,BETA,TMAX    
       IF(ISHELL==1)THEN
        WRITE(IOUT, 1100)
       ELSEIF(ISHELL==2)THEN
        WRITE(IOUT, 1200)
       ENDIF       
      ENDIF
c----------- 
 1000 FORMAT(
     & 5X,40H    CHANG CHANG FAILURE MODEL                  /,
     & 5X,40H    ------------------------                  /,
     & 5X,'Longitudinal tensile strength . . . . . . . =',E12.4/
     & 5X,'Transversal tensile strength  . . . . . . . =',E12.4/
     & 5X,'Shear strength . . . . . . . . . . . . . .  =',E12.4/
     & 5X,'Longitudinal compressive strength . . . . . =',E12.4/
     & 5X,'Transversal compressive  strength  . . . .  =',E12.4/
     & 5X,'Shear scaling factor . . . . . . . . . . .  =',E12.4/
     & 5X,' Relaxation time . . . . . . . . . . . . .  =',E12.4/)  
 1100 FORMAT(
     & 5X,'  SHELL IS DELETED IF CRITERIA IS REACHED FOR ONE LAYER ') 
 1200 FORMAT(
     & 5X,'  SHELL TENSOR STRESS IS SET TO ZERO AND SHELL IS DELETED',/  
     & 5X,'  IF CRITERIA IS REACHED FOR ALL LAYERS' )     
c----------- 
      RETURN
      END
