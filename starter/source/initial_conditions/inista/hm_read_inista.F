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
Chd|  HM_READ_INISTA                source/initial_conditions/inista/hm_read_inista.F
Chd|-- called by -----------
Chd|        CONTRL                        source/starter/contrl.F       
Chd|-- calls ---------------
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_STRING                 source/devtools/hm_reader/hm_get_string.F
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_INISTA(S0FILE   ,ISIGI    ,IOUTP_FMT,
     .                          IROOTYY_R,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr03_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER S0FILE*ncharline
      INTEGER ISIGI,IOUTP_FMT,IROOTYY_R
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NINISTA,IB,SIZ
      LOGICAL IS_AVAILABLE
C=======================================================================
C
      IS_AVAILABLE = .FALSE.
C
      ! Count number of Inista card
      CALL HM_OPTION_COUNT('/INISTA',NINISTA)
C
      IF (NINISTA > 0) THEN
C
        ! Start reading /INIGRAV card 
        CALL HM_OPTION_START('/INISTA')
C
        ! Read title, ID and Unit ID
        CALL HM_OPTION_READ_KEY(LSUBMODEL) 
C
        ! Read card
        CALL HM_GET_STRING('ISRTY',S0FILE   ,ncharline   ,IS_AVAILABLE)
        SIZ = LEN_TRIM(S0FILE)
        CALL HM_GET_INTV('IBAL'   ,IB       ,IS_AVAILABLE,LSUBMODEL)  
        CALL HM_GET_INTV('IOUTYY' ,IOUTP_FMT,IS_AVAILABLE,LSUBMODEL)  
        CALL HM_GET_INTV('IOUTYNN',IROOTYY_R,IS_AVAILABLE,LSUBMODEL) 
C
        IF (INVERS < 90 .AND. IROOTYY_R == 0) IROOTYY_R = 2
C
        ! Setting ISIGI values
        IF (IB <= 1) ISIGI = 3
        IF (IB == 2) ISIGI = 4
        IF (IB == 3) ISIGI = 5
C
      ELSE
        ! Default value
        ISIGI     = 0
        IOUTP_FMT = 0
        IROOTYY_R = 0        
      ENDIF       
C-----------
      END
