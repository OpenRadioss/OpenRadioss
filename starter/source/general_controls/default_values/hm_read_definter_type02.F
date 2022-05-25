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
Chd|  HM_READ_DEFINTER_TYPE02       source/general_controls/default_values/hm_read_definter_type02.F
Chd|-- called by -----------
Chd|        HM_READ_DEFINTER              source/general_controls/default_values/hm_read_definter.F
Chd|-- calls ---------------
Chd|        DEFINTER                      source/interfaces/interf1/definter.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_DEFINTER_TYPE02(NTYP , DEF_INTER, LSUBMODEL)
C============================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NTYP
      INTEGER DEF_INTER(*)
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com09_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  IGNORE,ILEV,IBUC,IDEL2,
     .   IGSTI,IPRINT,IASSIGN
      CHARACTER OPT*ncharkey,KEY*ncharkey,KEY1*ncharkey
      LOGICAL IS_AVAILABLE
C============================================================================
C       READING DEFAULT VALUES INTERFACE TYPE02 (/DEFAULT/INTER/TYPE02)
C============================================================================
      IBUC=0
      ILEV = 0
      IDEL2 = 0
      IGSTI = 0        
      IASSIGN = 0    
C---------------------------------------------------------------
C
      IPRINT=0
      CALL HM_GET_INTV('Ignore',IGNORE,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('WFLAG',ILEV,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Isearch',IBUC,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('NodDel2',IDEL2,IS_AVAILABLE,LSUBMODEL)
C
      KEY1='IGNORE'
      CALL DEFINTER(KEY1     ,IGNORE   ,IASSIGN   ,IPRINT   ,
     .              NTYP     ,DEF_INTER)
C
      KEY1='ILEV'
      CALL DEFINTER(KEY1     ,ILEV     ,IASSIGN   ,IPRINT   ,
     .              NTYP     ,DEF_INTER)
C
      KEY1='IBUC'
      CALL DEFINTER(KEY1     ,IBUC     ,IASSIGN   ,IPRINT   ,
     .              NTYP     ,DEF_INTER)
C
      KEY1='IDEL2'
      CALL DEFINTER(KEY1     ,IDEL2    ,IASSIGN   ,IPRINT   ,
     .              NTYP     ,DEF_INTER)
C
      IF (ILEV == 25 .or. ILEV == 26 .or. ILEV == 27 .or. ILEV == 28) THEN
C
        CALL HM_GET_INTV('Istf',IGSTI,IS_AVAILABLE,LSUBMODEL)
C
        KEY1='IGSTI'
        CALL DEFINTER(KEY1     ,IGSTI    ,IASSIGN   ,IPRINT   ,
     .              NTYP     ,DEF_INTER)
      ENDIF
C
C----------------------------------------------------
C DEFAULT VALUES PRINT
C----------------------------------------------------
      IPRINT = 1
c
      KEY1='IGNORE'
      CALL DEFINTER(KEY1     ,IGNORE   ,IASSIGN   ,IPRINT   ,
     .              NTYP     ,DEF_INTER)
C
      KEY1='ILEV'
      CALL DEFINTER(KEY1     ,ILEV     ,IASSIGN   ,IPRINT   ,
     .              NTYP     ,DEF_INTER)
C
      KEY1='IBUC'
      CALL DEFINTER(KEY1     ,IBUC     ,IASSIGN   ,IPRINT   ,
     .              NTYP     ,DEF_INTER)
C
      KEY1='IDEL2'
      CALL DEFINTER(KEY1     ,IDEL2    ,IASSIGN   ,IPRINT   ,
     .              NTYP     ,DEF_INTER)
C
      IF (ILEV == 25 .or. ILEV == 26 .or. ILEV == 27 .or. ILEV == 28) THEN
        KEY1='IGSTI'
        CALL DEFINTER(KEY1     ,IGSTI    ,IASSIGN   ,IPRINT   ,
     .              NTYP     ,DEF_INTER)
        WRITE(IOUT,5102) IGNORE,ILEV,IBUC,IDEL2,IGSTI
      ELSE
        WRITE(IOUT,5002) IGNORE,ILEV,IBUC,IDEL2
      ENDIF

C--------------------------------------------------------------
      RETURN
 999  CALL FREERR(3)
      RETURN
C--------------------------------------------------------------

 5002 FORMAT(/
     .       '    INTERFACE TYPE 2                          ' //,
     .       '    IGNORE FLAG . . . . . . . . . . . . . . . ',I5/
     .       '    FORMULATION LEVEL . . . . . . . . . . . . ',I5/,
     .       '    SEARCH FORMULATION. . . . . . . . . . . . ',I5/,
     .       '    DELETION FLAG CASE FAILURE                '/,
     .       '    OF MAIN ELEMENT . . . . . . . . . . . . ..',I5/)
C
 5102 FORMAT(/
     .       '    INTERFACE TYPE 2                          ' //,
     .       '    IGNORE FLAG . . . . . . . . . . . . . . . ',I5/
     .       '    FORMULATION LEVEL . . . . . . . . . . . . ',I5/,
     .       '    SEARCH FORMULATION. . . . . . . . . . . . ',I5/,
     .       '    DELETION FLAG CASE FAILURE                '/,
     .       '    OF MAIN ELEMENT . . . . . . . . . . . .. .',I5/,
     .       '    INTERFACE STIFFNESS DEFINITION FLAG . . . ',I5/)
C
C--------------------------------------------------------------
      END 
