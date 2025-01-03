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
      !||    hm_preread_pblast      ../starter/source/loads/pblast/hm_preread_pblast.F
      !||--- called by ------------------------------------------------------
      !||    lectur                 ../starter/source/starter/lectur.F
      !||--- calls      -----------------------------------------------------
      !||    arret                  ../starter/source/system/arret.F
      !||    hm_get_intv            ../starter/source/devtools/hm_reader/hm_get_intv.F
      !||    hm_option_read_key     ../starter/source/devtools/hm_reader/hm_option_read_key.F
      !||    hm_option_start        ../starter/source/devtools/hm_reader/hm_option_start.F
      !||    ngr2usr                ../starter/source/system/nintrr.F
      !||--- uses       -----------------------------------------------------
      !||    hm_option_read_mod     ../starter/share/modules1/hm_option_read_mod.F
      !||    message_mod            ../starter/share/message_module/message_mod.F
      !||    submodel_mod           ../starter/share/modules1/submodel_mod.F
      !||====================================================================
      SUBROUTINE HM_PREREAD_PBLAST(PBLAST,NUMLOADP , IGRSURF, LSUBMODEL,NSURF)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE PBLAST_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE NAMES_AND_TITLES_MOD, ONLY : NCHARTITLE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(INOUT) :: NUMLOADP
C-----------------------------------------------
      TYPE (PBLAST_)  , INTENT(INOUT) :: PBLAST
      TYPE (SURF_)   ,TARGET, DIMENSION(NSURF)   :: IGRSURF
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
      INTEGER, INTENT(IN) :: NSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I, ID, ISU, IS, IERR1
      CHARACTER(LEN=NCHARTITLE) :: TITR
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

      DO I = 1, PBLAST%NLOADP_B
         CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_TITR = TITR, OPTION_ID = ID)
         CALL HM_GET_INTV('surf_ID', ISU, IS_AVAILABLE, LSUBMODEL)
         INGR2USR => IGRSURF(1:NSURF)%ID
         IS = NGR2USR(ISU,INGR2USR,NSURF)
         IF(IS > 0)THEN
           NUMLOADP = NUMLOADP + IGRSURF(IS)%NSEG*4
         ENDIF
      ENDDO

      IF(PBLAST%NLOADP_B > 0 ) ALLOCATE (   PBLAST%PBLAST_TAB(PBLAST%NLOADP_B),STAT=IERR1); 
       IF (IERR1 /= 0) THEN
         WRITE(IOUT,*)' ** ERROR IN MEMORY ALLOCATION'
         WRITE(ISTDO,*)' ** ERROR IN MEMORY ALLOCATION'
         CALL ARRET(2)
       ENDIF                         

      END SUBROUTINE HM_PREREAD_PBLAST
