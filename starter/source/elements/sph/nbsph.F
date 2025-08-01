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
!||    nbsph                  ../starter/source/elements/sph/nbsph.F
!||--- called by ------------------------------------------------------
!||    contrl                 ../starter/source/starter/contrl.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                 ../starter/source/output/message/message.F
!||    hm_get_intv            ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_count        ../starter/source/devtools/hm_reader/hm_option_count.F
!||    hm_option_read_key     ../starter/source/devtools/hm_reader/hm_option_read_key.F
!||    hm_option_start        ../starter/source/devtools/hm_reader/hm_option_start.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod     ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod            ../starter/share/message_module/message_mod.F
!||    submodel_mod           ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE NBSPH(LSUBMODEL) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE HM_OPTION_READ_MOD
      USE SUBMODEL_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARKEY
      USE MESSAGE_MOD
C----------------------------------------------------------
C     Format par BLOCS.
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: N,ID,KRESERV
      CHARACTER :: MESS*40
      LOGICAL :: IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      DATA MESS /'SPH OPTIONS DEFINITION                  '/
C-----------------------------------------------
      NSPHIO =0
      NSPHRES=0
C
      CALL HM_OPTION_COUNT('/SPH/RESERVE',NBPARTINLET)
      CALL HM_OPTION_START('/SPH/RESERVE')
      DO N=1,NBPARTINLET
        CALL HM_OPTION_READ_KEY(LSUBMODEL,OPTION_ID = ID)
        CALL HM_GET_INTV('Np',KRESERV,IS_AVAILABLE,LSUBMODEL)
        NSPHRES=NSPHRES+KRESERV
      ENDDO
C
      CALL HM_OPTION_COUNT('/SPH/INOUT',NSPHIO)
c
c NSPHRES (number of SPH reserve for inlet) for each proc
c in order to assure an suffisant number of reserve for inlet in SPMD
      NSPHRES = NSPHRES*MAX(1,NSPMD)

      IF (NBPARTINLET > 0 .AND. NSPHRES == 0) THEN
          WRITE(MESS,*)'SPH/RESERVE option is missing'
          CALL ANCMSG(MSGID=3094,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=ID)
      ENDIF
C-------------------------------------
      RETURN
C-------------------------------------
      END
