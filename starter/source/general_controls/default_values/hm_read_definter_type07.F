Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    hm_read_definter_type07   ../starter/source/general_controls/default_values/hm_read_definter_type07.F
!||--- called by ------------------------------------------------------
!||    hm_read_definter          ../starter/source/general_controls/default_values/hm_read_definter.F
!||--- calls      -----------------------------------------------------
!||    definter                  ../starter/source/interfaces/interf1/definter.F
!||    hm_get_intv               ../starter/source/devtools/hm_reader/hm_get_intv.F
!||--- uses       -----------------------------------------------------
!||    message_mod               ../starter/share/message_module/message_mod.F
!||    submodel_mod              ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_DEFINTER_TYPE07( NTYP , DEF_INTER,LSUBMODEL)
C============================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE SUBMODEL_MOD , ONLY : SUBMODEL_DATA, NSUBMOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARKEY
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NTYP
      INTEGER DEF_INTER(100)
      TYPE(SUBMODEL_DATA) LSUBMODEL(NSUBMOD)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  IGSTI,IGAP,IBAG,IDEL7N,FLAGREMNOD,IREM7I2,INACTI,MODFR,IPRINT,IASSIGN
      CHARACTER(LEN=NCHARKEY) :: KEY1
      LOGICAL IS_AVAILABLE

C============================================================================
C       READING DEFAULT VALUES INTERFACE TYPE07 (/DEFAULT/INTER/TYPE07)
C============================================================================
      
      IGSTI = 0        
      IBAG=0
      IDEL7N = 0 
      INACTI = 0
C
      IASSIGN = 0    

C
      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
      CALL HM_GET_INTV('Istf',IGSTI,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Igap',IGAP,IS_AVAILABLE,LSUBMODEL)
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
          WRITE(IOUT,5007) IGSTI,IGAP,IBAG,IDEL7N,
     .                     FLAGREMNOD,IREM7I2,INACTI,MODFR
C--------------------------------------------------------------
      RETURN

C--------------------------------------------------------------

C
 5007 FORMAT(/
     .       '    INTERFACE TYPE 7                          ' //,
     .       '    STIFFNESS FORMULATION. .  . . . . . . . . ',I5/,
     .       '    VARIABLE GAP FLAG . . . . . . . . . . . . ',I5/,
     .       '    AIRBAG VENT HOLES CLOSURE FLAG            ' /,
     .       '    IN CASE OF CONTACT. . . . . . . . . . . . ',I5/,
     .       '    DELETION FLAG ON FAILURE OF MAIN ELEMENT',I5/,
     .       '    FLAGREMNODE : NOT CONSIDER NEIGHBORING    '/,
     .       '    NODES IN CASE OF ELEMENT SIZE < GAP VALUE ',I5/,
     .       '    DE-ACTIVATION FLAG IF CONFLICTING W/ TYPE2 ',I5/,
     .       '    DE-ACTIVATION OF INITIAL PENETRATIONS . . ',I5/,
     .       '    TYPE OF FRICTION PENALTY FORMULATION. . . ',I5/)
C
C--------------------------------------------------------------
      END 
