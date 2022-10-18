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
Chd|  HM_READ_DEFINTER_TYPE24       source/general_controls/default_values/hm_read_definter_type24.F
Chd|-- called by -----------
Chd|        HM_READ_DEFINTER              source/general_controls/default_values/hm_read_definter.F
Chd|-- calls ---------------
Chd|        DEFINTER                      source/interfaces/interf1/definter.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_DEFINTER_TYPE24( NTYP , DEF_INTER, LSUBMODEL)
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
#include      "sphcom.inc"
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
      INTEGER  
     .   IGSTI,IREM24I2,ILEV,IDEL24,INTPLY,
     .   IEDGE,IGAP0,IPEN0,INACTI,MFROT,IFQ,
     .   IDSENS,INTFRIC,IPRINT,IASSIGN
      my_real
     .   ETEN_M,EDG_ANGL,GAPMAX_S,GAPMAX_M,STMIN,STMAX,PENMAX,PENMIN,
     .   VISC,VISCF,BUMULT,ALPHA,GAP,STARTT,STOPT,STFAC,FRIC
      CHARACTER 
     .   BCFLAG*ncharfield, BCFLAGM*ncharfield,KEY*ncharkey,KEY1*ncharkey
      LOGICAL IS_AVAILABLE
!
C============================================================================
C       READING DEFAULT VALUES INTERFACE TYPE24 (/DEFAULT/INTER/TYPE24)
C============================================================================
      
      ILEV = 0
      IGSTI = 0 
      IDEL24= 0   
      INTPLY = 0     
      INACTI = 0
      IDSENS = 0
      MFROT=0
      IFQ=0
      INTFRIC = 0
C
      FRIC = ZERO
      GAP = ZERO
      STARTT = ZERO
      STOPT=EP30
      VISCF = ZERO
      VISC = ZERO

      IASSIGN = 0 

C---------------------------------------------------------------
      IPRINT = 0
      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
      CALL HM_GET_INTV('Istf',IGSTI,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Irem_i2',IREM24I2,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('TYPE24_Idel',IDEL24,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('TYPE24_Iedge',IEDGE,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Itied',IGAP0,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ipen',IPEN0,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Irs',INACTI,IS_AVAILABLE,LSUBMODEL)
C
      KEY1='IGSTI'
      CALL DEFINTER(KEY1     ,IGSTI    ,IASSIGN   ,IPRINT   ,
     .              NTYP     ,DEF_INTER)
      KEY1='IDEL24'
      CALL DEFINTER(KEY1     ,IDEL24   ,IASSIGN   ,IPRINT   ,
     .              NTYP     ,DEF_INTER)
      KEY1='IREM24I2'
      CALL DEFINTER(KEY1     ,IREM24I2   ,IASSIGN   ,IPRINT   ,
     .              NTYP     ,DEF_INTER)
C
      KEY1='IEDGE'
      CALL DEFINTER(KEY1     ,IEDGE    ,IASSIGN   ,IPRINT   ,
     .              NTYP     ,DEF_INTER)
C
      KEY1='IGAP0'
      CALL DEFINTER(KEY1     ,IGAP0    ,IASSIGN   ,IPRINT   ,
     .              NTYP     ,DEF_INTER)
C
      KEY1='IPEN0'
      CALL DEFINTER(KEY1     ,IPEN0    ,IASSIGN   ,IPRINT   ,
     .              NTYP     ,DEF_INTER)
C
      KEY1='INACTI'
      CALL DEFINTER(KEY1     ,INACTI   ,IASSIGN   ,IPRINT   ,
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
      KEY1='IDEL24'
      CALL DEFINTER(KEY1     ,IDEL24   ,IASSIGN   ,IPRINT   ,
     .              NTYP     ,DEF_INTER)
C
      KEY1='IEDGE'
      CALL DEFINTER(KEY1     ,IEDGE    ,IASSIGN   ,IPRINT   ,
     .              NTYP     ,DEF_INTER)
C
      KEY1='IGAP0'
      CALL DEFINTER(KEY1     ,IGAP0    ,IASSIGN   ,IPRINT   ,
     .              NTYP     ,DEF_INTER)
C
      KEY1='IPEN0'
      CALL DEFINTER(KEY1     ,IPEN0    ,IASSIGN   ,IPRINT   ,
     .              NTYP     ,DEF_INTER)
C
      KEY1='INACTI'
      CALL DEFINTER(KEY1     ,INACTI   ,IASSIGN   ,IPRINT   ,
     .              NTYP     ,DEF_INTER)
C
      KEY1='IREM24I2'
      CALL DEFINTER(KEY1     ,IREM24I2   ,IASSIGN   ,IPRINT   ,
     .              NTYP     ,DEF_INTER)
C
      WRITE(IOUT,5024) IGSTI,IDEL24,IEDGE,IGAP0,IREM24I2,INACTI

C--------------------------------------------------------------
      RETURN
C--------------------------------------------------------------
 5024 FORMAT(/
     .       '    INTERFACE TYPE 24                         ' //,
     .       '    STIFFNESS FORMULATION. .  . . . . . . . . ',I5/,
     .       '    DELETION FLAG ON FAILURE OF MAIN ELEMENT',I5/,
     .       '    EDGE FLAG . . . . . . . . . . . . . . . . ',I5/,
     .       '    GAP MODIFICATION FLAG FOR SECONDARY SHELL     ',/,
     .       '    NODES ON THE FREE EDGES . . . . . . . . . ',I5/,
     .       '    DE-ACTIVATION FLAG IF CONFLICTING W/ TYPE2',I5/,
     .       '    DE-ACTIVATION OF INITIAL PENETRATIONS . . ',I5/,
     .       '    FRICTION FORMULATION. . . . . . . . . . . ',I5/)
   
C--------------------------------------------------------------
      END 
