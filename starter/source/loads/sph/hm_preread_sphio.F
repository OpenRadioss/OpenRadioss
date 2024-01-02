Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  HM_PREREAD_SPHIO              source/loads/sph/hm_preread_sphio.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        FRETITL                       source/starter/freform.F      
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_PREREAD_SPHIO(IGRSURF   ,SVSPHIO,NOM_OPT, LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE R2R_MOD
      USE GROUPDEF_MOD
      USE HM_OPTION_READ_MOD
      USE SUBMODEL_MOD      
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "sphcom.inc"
#include      "scr17_c.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER SVSPHIO
      INTEGER NOM_OPT(LNOPT1,*)
      TYPE (SURF_), DIMENSION(NSURF) :: IGRSURF
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,ID,IDS,NSEG,ITYPE,IDPRT,IDSURF,SKIP,IN1,IN2,IN3,LVAD
      my_real
     .   DIST,FCUT
      CHARACTER TITR*nchartitle
      LOGICAL IS_AVAILABLE
C=======================================================================
      SVSPHIO = 0
      IS_AVAILABLE = .FALSE.
C
      ! Start reading the option
      CALL HM_OPTION_START('/SPH/INOUT')
C
      ! Loop over /SPH/INOUT
      I = 0
      DO N = 1,NSPHIO
        SKIP = 0        
CC----------Multidomaines --> on ignore les inlets non tages------------
        IF (NSUBDOM > 0) THEN
         IF(TAGSPHIO(N) == 0) SKIP = 1
        ENDIF
C----------------------------------------------------------------------
        IF (SKIP == 0) THEN
          I = I+1
C
          ! Title and ID
          TITR = ''   
          CALL HM_OPTION_READ_KEY(LSUBMODEL, 
     .                            OPTION_ID   = ID, 
     .                            OPTION_TITR = TITR)  
          NOM_OPT(1,I)=ID
          CALL FRETITL(TITR,NOM_OPT(LNOPT1-LTITR+1,I),LTITR)
C         
          ! Reading type dans surface ID
          CALL HM_GET_INTV('Itype'   ,ITYPE  ,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('SURF_ID' ,IDSURF ,IS_AVAILABLE,LSUBMODEL)
C
          IF ((IDSURF > 0).AND.(ITYPE == 1)) THEN
            IDS=0
            DO J=1,NSURF
              IF(IGRSURF(J)%ID == IDSURF) THEN
                IDS=J
                EXIT
              ENDIF
            ENDDO
            NSEG = IGRSURF(IDS)%NSEG
            LVAD = 4 + 2*NSEG
          ELSE
            LVAD = 22        
          ENDIF
          SVSPHIO = SVSPHIO + LVAD          
        ENDIF
      ENDDO
C-------------------------------------
C
      RETURN
      END
