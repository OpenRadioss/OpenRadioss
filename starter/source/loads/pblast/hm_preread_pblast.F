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
Chd|  HM_PREREAD_PBLAST             source/loads/pblast/hm_preread_pblast.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        NGR2USR                       source/system/nintrr.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        PBLAST_MOD                    ../common_source/modules/loads/pblast_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_PREREAD_PBLAST(NUMLOADP , IGRSURF, LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE PBLAST_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(INOUT) :: NUMLOADP
C-----------------------------------------------
      TYPE (SURF_)   ,TARGET, DIMENSION(NSURF)   :: IGRSURF
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I, ID, ISU, IS, IERR1
      CHARACTER :: TITR*nchartitle
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
      LOGICAL :: IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER,EXTERNAL ::  NGR2USR
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IERR1 = 0

      CALL HM_OPTION_START('/LOAD/PBLAST')

      DO I = 1, NLOADP_B
         CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_TITR = TITR, OPTION_ID = ID)
         CALL HM_GET_INTV('surf_ID', ISU, IS_AVAILABLE, LSUBMODEL)
         INGR2USR => IGRSURF(1:NSURF)%ID
         IS = NGR2USR(ISU,INGR2USR,NSURF)
         IF(IS > 0)THEN
           NUMLOADP = NUMLOADP + IGRSURF(IS)%NSEG*4
         ENDIF
      ENDDO

      IF(NLOADP_B > 0 ) ALLOCATE (   PBLAST_TAB(NLOADP_B),STAT=IERR1); 
       IF (IERR1 /= 0) THEN
         WRITE(IOUT,*)' ** ERROR IN MEMORY ALLOCATION'
         WRITE(ISTDO,*)' ** ERROR IN MEMORY ALLOCATION'
         CALL ARRET(2)
       ENDIF                         

      END SUBROUTINE HM_PREREAD_PBLAST
