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
Chd|  HM_PRE_READ_PRELOAD           source/loads/general/preload/hm_pre_read_preload.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_PRE_READ_PRELOAD(NSTRF,LSUBMODEL) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE R2R_MOD   
      USE HM_OPTION_READ_MOD    
      USE SUBMODEL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "boltpr_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      !INTEGER NUMPRELOAD
      INTEGER NSTRF(*) !IGRN(LIGRN1,*) !,ISURF(LISURF1,*)
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NLD,ISEC,NN,ID,UID,NDEB,NFIN,IS,K0  !,ISU,ISS
      INTEGER FLAG_FMT,FLAG_FMT_TMP,IFIX_TMP
      CHARACTER TITR*nchartitle,KEY*ncharline
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
!      INTEGER  NGR2USRN
C-----------------------------------------------
      LOGICAL IS_AVAILABLE
C=======================================================================
C     
      IS_AVAILABLE = .FALSE.
C-----------------------------------------------
      CALL HM_OPTION_START('/PRELOAD')       
C-----------------------------------------------
      NLD  = NUMPRELOAD
      NUMPRELOAD = 0
                                        
      DO I=1,NLD
        ! Read title, ID and Unit ID
        TITR = ''   
        CALL HM_OPTION_READ_KEY(LSUBMODEL, 
     .                          OPTION_ID      = ID, 
     .                          UNIT_ID        = UID,
     .                          OPTION_TITR    = TITR,           
     .                          KEYWORD2       = KEY)
C
         IF (KEY(1:LEN_TRIM(KEY))=='AXIAL') CYCLE

        CALL HM_GET_INTV('sect_ID'  ,ISEC  ,IS_AVAILABLE,LSUBMODEL)                                             

             NN = 0
        IF (ISEC > 0) THEN
          K0=31
          DO IS=1,NSECT
              IF (ISEC==NSTRF(K0+23)) THEN
                NN = NSTRF(K0+7)
                EXIT
              ENDIF
              K0=NSTRF(K0+24)
           ENDDO 
        ENDIF
        NUMPRELOAD = NUMPRELOAD + NN   

      ENDDO                                            
C---
      RETURN

      RETURN
      END SUBROUTINE HM_PRE_READ_PRELOAD
