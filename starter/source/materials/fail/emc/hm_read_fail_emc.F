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
Chd|  HM_READ_FAIL_EMC              source/materials/fail/emc/hm_read_fail_emc.F
Chd|-- called by -----------
Chd|        HM_READ_FAIL                  source/materials/fail/hm_read_fail.F
Chd|-- calls ---------------
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_FAIL_EMC(
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
      INTEGER,INTENT(OUT)            ::NUPARAM,NUVAR
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real    ::     AFRAC,NFRAC,BFRAC,CFRAC,GAMA,DEPS0
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
Card1   
      CALL HM_GET_FLOATV    ('a_EMC'  ,AFRAC     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV    ('n_EMC'  ,NFRAC     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV    ('b0'     ,BFRAC     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV    ('c'      ,CFRAC     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
Card2   
      CALL HM_GET_FLOATV    ('Gamma'        ,GAMA   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV    ('Epsilon_Dot_0',DEPS0  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
c-----------------------------------------------------------------------
      IF (AFRAC == ZERO) AFRAC = ONE
      IF (BFRAC == ZERO) BFRAC = ONE
      IF (DEPS0 == ZERO) DEPS0 = EM30
c-----------------------------------------------------------------------
      UPARAM(1)  = AFRAC
      UPARAM(2)  = BFRAC
      UPARAM(3)  = GAMA
      UPARAM(4)  = CFRAC
      UPARAM(5)  = NFRAC
      UPARAM(6)  = DEPS0
c-----------------------------------------------------------------------
      NUPARAM = 6
      NUVAR   = 2
c-----------------------------------------------------------------------
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT, 1300)
      ELSE
        WRITE(IOUT, 1100) AFRAC,NFRAC,BFRAC,CFRAC,GAMA,DEPS0
      ENDIF
C-----------
      RETURN
C-----------
 1300 FORMAT(
     & 5X,40H    CRYPTED DATA IN FAILURE MODEL             /,
     & 5X,40H    -----------------------------             /)
 1100 FORMAT(
     & 5X,40H    EXTENDED MOHR COULOMB RUPTURE MODEL /,
     & 5X,40H    ----------------------------------- /,
     & 5X,'HOSFORD EXPONENT. . . . . . . . . .  . . . . . . . =',E16.9/
     & 5X,'STRESS STATE SENSITIVITY. . . . . .  . . . . . . . =',E16.9/
     & 5X,'STRAIN TO FRACTURE FOR UNIAXIAL TENSION. . . . . . =',E16.9/
     & 5X,'FRICTION COEFFICIENT FOR TRIAXIALITY . . . . . . . =',E16.9/
     & 5X,'STRAIN RATE SENSITIVITY . . . . . .  . . . . . . . =',E16.9/
     & 5X,'REFERENCE STRAIN RATE. . . . . . . . . . . . . . . =',E16.9)
C-----------       
      RETURN
      END









