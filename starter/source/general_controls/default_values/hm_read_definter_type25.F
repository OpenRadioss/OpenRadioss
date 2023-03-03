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
Chd|  HM_READ_DEFINTER_TYPE25       source/general_controls/default_values/hm_read_definter_type25.F
Chd|-- called by -----------
Chd|        HM_READ_DEFINTER              source/general_controls/default_values/hm_read_definter.F
Chd|-- calls ---------------
Chd|        DEFINTER                      source/interfaces/interf1/definter.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_DEFINTER_TYPE25( NTYP , DEF_INTER, LSUBMODEL)
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
#include      "units_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  
     .   IGSTI,IBID,IGAP,IREM25I2,IBAG,IDEL25,IEDGE,
     .   IGAP0,ISHARP,IVIS2,INACTI,MFROT,IFQ,MODFR,IDSENS,INTFRIC,
     .   IPRINT,IASSIGN,ITHK
      my_real
     .   GAPSCALE,PERCENT_SIZE,
     .   PENMIN,FRIC,GAP,STARTT,STOPT,VISC,VISCF,
     .   BUMULT,ALPHA,PMAX
      CHARACTER 
     .  BCFLAG*ncharfield, BCFLAGM*ncharfield,KEY*ncharkey,KEY1*ncharkey
      LOGICAL IS_AVAILABLE

C============================================================================
C       READING DEFAULT VALUES INTERFACE TYPE25 (/DEFAULT/INTER/TYPE25)
C============================================================================
C
      IGSTI = 0 
      IBAG=0
      INACTI = 0
      IDSENS = 0
      MFROT=0
      IFQ=0
      INTFRIC = 0
      ITHK = 0
      MODFR = 0 
C
      GAPSCALE = ZERO
      PERCENT_SIZE = ZERO
      FRIC = ZERO
      GAP = ZERO
      STARTT = ZERO
      STOPT=EP30
      VISCF = ZERO
      VISC = ZERO
      PMAX = EP30
C
      IASSIGN = 0 
C---------------------------------------------------------------
      IPRINT = 0
C
      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
      CALL HM_GET_INTV('Istf',IGSTI,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Igap',IGAP,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Irem_i2',IREM25I2,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('TYPE24_Idel',IDEL25,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Iedge',IEDGE,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Itied',IGAP0,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ishape',ISHARP,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Irs',INTFRIC,IS_AVAILABLE,LSUBMODEL)
C
      KEY1='IGSTI'
      CALL DEFINTER(KEY1     ,IGSTI    ,IASSIGN   ,IPRINT   ,
     .              NTYP     ,DEF_INTER)
C
      KEY1='IGAP'
      CALL DEFINTER(KEY1     ,IGAP     ,IASSIGN   ,IPRINT   ,
     .              NTYP     ,DEF_INTER)
C
      KEY1='IREM25I2'
      CALL DEFINTER(KEY1     ,IREM25I2 ,IASSIGN   ,IPRINT   ,
     .              NTYP     ,DEF_INTER)
C
      KEY1='IEDGE'
      CALL DEFINTER(KEY1     ,IEDGE    ,IASSIGN   ,IPRINT   ,
     .              NTYP     ,DEF_INTER)
C
      KEY1='IBAG'
      CALL DEFINTER(KEY1     ,IBAG     ,IASSIGN   ,IPRINT   ,
     .              NTYP     ,DEF_INTER)
C
      KEY1='IDEL25'
      CALL DEFINTER(KEY1     ,IDEL25   ,IASSIGN   ,IPRINT   ,
     .              NTYP     ,DEF_INTER)
C
      KEY1='IGAP0'
      CALL DEFINTER(KEY1     ,IGAP0    ,IASSIGN   ,IPRINT   ,
     .              NTYP     ,DEF_INTER)
C
      KEY1='ISHARP'
      CALL DEFINTER(KEY1     ,ISHARP   ,IASSIGN   ,IPRINT   ,
     .              NTYP     ,DEF_INTER)

      CALL HM_GET_INTV('Irs',INACTI,IS_AVAILABLE,LSUBMODEL)

      KEY1='INACTI'
      CALL DEFINTER(KEY1     ,INACTI   ,IASSIGN   ,IPRINT   ,
     .              NTYP     ,DEF_INTER)
C
      KEY1='ITHK'
      CALL DEFINTER(KEY1     ,ITHK     ,IASSIGN   ,IPRINT   ,
     .              NTYP     ,DEF_INTER)
C
C----------------------------------------------------
C DEFAULT VALUES PRINT
C----------------------------------------------------
      IPRINT = 1
c
      KEY1='IGSTI'
      CALL DEFINTER(KEY1     ,IGSTI    ,IASSIGN   ,IPRINT   ,
     .              NTYP     ,DEF_INTER)
C
      KEY1='IGAP'
      CALL DEFINTER(KEY1     ,IGAP     ,IASSIGN   ,IPRINT   ,
     .              NTYP     ,DEF_INTER)
C
      KEY1='IREM25I2'
      CALL DEFINTER(KEY1     ,IREM25I2 ,IASSIGN   ,IPRINT   ,
     .              NTYP     ,DEF_INTER)
C
      KEY1='IEDGE'
      CALL DEFINTER(KEY1     ,IEDGE    ,IASSIGN   ,IPRINT   ,
     .              NTYP     ,DEF_INTER)
C
      KEY1='IBAG'
      CALL DEFINTER(KEY1     ,IBAG     ,IASSIGN   ,IPRINT   ,
     .              NTYP     ,DEF_INTER)
C
      KEY1='IDEL25'
      CALL DEFINTER(KEY1     ,IDEL25   ,IASSIGN   ,IPRINT   ,
     .              NTYP     ,DEF_INTER)
C
      KEY1='IGAP0'
      CALL DEFINTER(KEY1     ,IGAP0    ,IASSIGN   ,IPRINT   ,
     .              NTYP     ,DEF_INTER)
C
      KEY1='ISHARP'
      CALL DEFINTER(KEY1     ,ISHARP   ,IASSIGN   ,IPRINT   ,
     .              NTYP     ,DEF_INTER)
C
      KEY1='INACTI'
      CALL DEFINTER(KEY1     ,INACTI   ,IASSIGN   ,IPRINT   ,
     .              NTYP     ,DEF_INTER)
C
      KEY1='ITHK'
      CALL DEFINTER(KEY1     ,ITHK     ,IASSIGN   ,IPRINT   ,
     .              NTYP     ,DEF_INTER)
C
      WRITE(IOUT,5025) IGSTI,IGAP,IGAP0,ISHARP,IEDGE,IDEL25,IREM25I2,INACTI,MODFR
C
C--------------------------------------------------------------
      RETURN
C--------------------------------------------------------------
 
 5025 FORMAT(/
     .       '    INTERFACE TYPE 25                         ' //,
     .       '    STIFFNESS FORMULATION. .  . . . . . . . . ',I5/,
     .       '    GAP FLAG. . . . . . . . . . . . . . . . . ',I5/,
     .       '    ZERO GAP FOR SECONDARY SHELL ON FREE EDGE ',I5/,
     .       '    GAP SHAPE ALONG MAIN FREE EDGES . . . . ',/,
     .       '            (1:SQUARE,2:CYLINDRICAL) . . . . .',I5/,
     .       '    EDGE FLAG . . . . . . . . . . . . . . . . ',I5/,
     .       '    DELETION FLAG ON FAILURE OF MAIN ELEMENT',I5/,
     .       '    DE-ACTIVATION FLAG IF CONFLICTING W/ TYPE2',I10/,
     .       '    DE-ACTIVATION OF INITIAL PENETRATIONS . . ',I5/,
     .       '    FRICTION FORMULATION. . . . . . . . . . . ',I5/)
C--------------------------------------------------------------
      END 
