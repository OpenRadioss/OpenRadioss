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
Chd|  HM_READ_DEFINTER_TYPE19       source/general_controls/default_values/hm_read_definter_type19.F
Chd|-- called by -----------
Chd|        HM_READ_DEFINTER              source/general_controls/default_values/hm_read_definter.F
Chd|-- calls ---------------
Chd|        DEFINTER                      source/interfaces/interf1/definter.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_DEFINTER_TYPE19( NTYP , DEF_INTER,LSUBMODEL)
C============================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
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
      INTEGER  IGSTI,IGAP,IBAG,IDEL7N,FLAGREMNOD,IREM7I2,INACTI,MODFR,
     .         IPRINT,IASSIGN,IEDGE
      CHARACTER KEY1*ncharkey
      LOGICAL IS_AVAILABLE

CC============================================================================
C       READING DEFAULT VALUES INTERFACE TYPE19 (/DEFAULT/INTER/TYPE19)
C============================================================================

      IGSTI = 0   
      IBAG=0
      IDEL7N = 0 
      INACTI = 0
      IEDGE = 0
C
      IASSIGN = 0  
C
      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
      CALL HM_GET_INTV('Istf',IGSTI,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Igap',IGAP,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Iedge_Type19',IEDGE,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ibag',IBAG,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Idel7',IDEL7N,IS_AVAILABLE,LSUBMODEL)
C
      CALL HM_GET_INTV('IKREM',FLAGREMNOD,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('ICOG',IREM7I2,IS_AVAILABLE,LSUBMODEL)

C
      CALL HM_GET_INTV('INACTIV',INACTI,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('IFORM',MODFR,IS_AVAILABLE,LSUBMODEL)
C
C----------------------------------------------------
C READING DEFAULT VALUES
C----------------------------------------------------
      IPRINT = 0
C
          KEY1='IGSTI'
          CALL DEFINTER(KEY1     ,IGSTI    ,IASSIGN   ,IPRINT   ,
     .                  NTYP     ,DEF_INTER)
C
          KEY1='IGAP'
          CALL DEFINTER(KEY1     ,IGAP     ,IASSIGN   ,IPRINT   ,
     .                  NTYP     ,DEF_INTER)
C
          KEY1='IEDGE'
          CALL DEFINTER(KEY1     ,IEDGE   ,IASSIGN   ,IPRINT   ,
     .                  NTYP     ,DEF_INTER)
C
          KEY1='IBAG'
          CALL DEFINTER(KEY1     ,IBAG     ,IASSIGN   ,IPRINT   ,
     .                  NTYP     ,DEF_INTER)
C
          KEY1='IDEL'
          CALL DEFINTER(KEY1     ,IDEL7N   ,IASSIGN   ,IPRINT   ,
     .                  NTYP     ,DEF_INTER)
C
          KEY1='FLAGREMNOD'
          CALL DEFINTER(KEY1     ,FLAGREMNOD,IASSIGN  ,IPRINT   ,
     .                  NTYP     ,DEF_INTER)
C
          KEY1='IREM7I2'
          CALL DEFINTER(KEY1     ,IREM7I2  ,IASSIGN   ,IPRINT   ,
     .                  NTYP     ,DEF_INTER)
C
          KEY1='INACTI'
          CALL DEFINTER(KEY1     ,INACTI   ,IASSIGN   ,IPRINT   ,
     .                  NTYP     ,DEF_INTER)
C
          KEY1='MODFR'
          CALL DEFINTER(KEY1     ,MODFR    ,IASSIGN   ,IPRINT   ,
     .                  NTYP     ,DEF_INTER)
C
C----------------------------------------------------
C DEFAULT VALUES PRINT
C----------------------------------------------------
       IPRINT = 1
c
          KEY1='IGSTI'
          CALL DEFINTER(KEY1     ,IGSTI    ,IASSIGN   ,IPRINT   ,
     .                  NTYP     ,DEF_INTER)
C
          KEY1='IGAP'
          CALL DEFINTER(KEY1     ,IGAP     ,IASSIGN   ,IPRINT   ,
     .                  NTYP     ,DEF_INTER)
C
          KEY1='IEDGE'
          CALL DEFINTER(KEY1     ,IEDGE   ,IASSIGN   ,IPRINT   ,
     .                  NTYP     ,DEF_INTER)
C
          KEY1='IBAG'
          CALL DEFINTER(KEY1     ,IBAG     ,IASSIGN   ,IPRINT   ,
     .                  NTYP     ,DEF_INTER)
C
          KEY1='IDEL'
          CALL DEFINTER(KEY1     ,IDEL7N   ,IASSIGN   ,IPRINT   ,
     .                  NTYP     ,DEF_INTER)
C
          KEY1='INACTI'
          CALL DEFINTER(KEY1     ,INACTI   ,IASSIGN   ,IPRINT   ,
     .                  NTYP     ,DEF_INTER)
C
          KEY1='MODFR'
          CALL DEFINTER(KEY1     ,MODFR    ,IASSIGN   ,IPRINT   ,
     .                  NTYP     ,DEF_INTER)
C
          KEY1='FLAGREMNOD'
          CALL DEFINTER(KEY1     ,FLAGREMNOD,IASSIGN  ,IPRINT   ,
     .                  NTYP     ,DEF_INTER)
C
          KEY1='IREM7I2'
          CALL DEFINTER(KEY1     ,IREM7I2  ,IASSIGN   ,IPRINT   ,
     .                  NTYP     ,DEF_INTER)
C
          WRITE(IOUT,5019) IGSTI,IGAP,IBAG,IDEL7N,INACTI,
     .                     MODFR,IEDGE,FLAGREMNOD,IREM7I2
C--------------------------------------------------------------
      RETURN
C--------------------------------------------------------------
 5019 FORMAT(/
     .       '    INTERFACE TYPE 19                         ' //,
     .       '    STIFFNESS FORMULATION. .  . . . . . . . . ',I5/,
     .       '    VARIABLE GAP FLAG . . . . . . . . . . . . ',I5/,
     .       '    AIRBAG VENT HOLES CLOSURE FLAG            ' /,
     .       '    IN CASE OF CONTACT. . . . . . . . . . . . ',I5/,
     .       '    DELETION FLAG ON FAILURE OF MAIN ELEMENT',I5/,
     .       '    DE-ACTIVATION OF INITIAL PENETRATIONS . . ',I5/,
     .       '    TYPE OF FRICTION PENALTY FORMULATION. . . ',I5/,
     .       '    EDGES SELECTION FLAG . . . . . . . . . . .',I5/,
     .       '    FLAGREMNODE : NOT CONSIDER NEIGHBORING    '/,
     .       '    NODES IN CASE OF ELEMENT SIZE < GAP VALUE ',I5/,
     .       '    DE-ACTIVATION FLAG IF CONFLITING W/ TYPE2 ',I5/)
C
C--------------------------------------------------------------
      END 
