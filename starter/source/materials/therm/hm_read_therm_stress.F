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
Chd|  HM_READ_THERM_STRESS          source/materials/therm/hm_read_therm_stress.F
Chd|-- called by -----------
Chd|        READ_MATERIAL_MODELS          source/materials/read_material_models.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_THERM_STRESS(MLAW_TAG ,IPM     ,PM     ,UNITAB  ,LSUBMODEL  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE ELBUFTAG_MOD            
C============================================================================
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr06_c.inc"
#include      "scr17_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,DIMENSION(NPROPMI,NUMMAT), INTENT(OUT)   :: IPM
      my_real ,DIMENSION(NPROPM ,NUMMAT), INTENT(OUT)   :: PM
      TYPE(MLAW_TAG_) , DIMENSION(NUMMAT  ) , INTENT(INOUT)     :: MLAW_TAG
      TYPE (UNIT_TYPE_) ,INTENT(IN) :: UNITAB 
      TYPE(SUBMODEL_DATA) ,DIMENSION(*) , INTENT(IN)    :: LSUBMODEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: ITH,IMAT,ILAW, JTHE,NTHERM_ST,IFUNC_ALPHA,MAT_ID,I
      INTEGER ,DIMENSION(NUMMAT) :: ITHERM_FOR
      my_real ::  FSCAL_ALPHA
      CHARACTER TITR*nchartitle,KEY*80
      LOGICAL IS_AVAILABLE
c=======================================================================

c--------------------------------------------------
c     COUNT EOS MODELS USING CFG FILES
c--------------------------------------------------
c      
      CALL HM_OPTION_COUNT('/THERM_STRESS',NTHERM_ST)
c
c--------------------------------------------------
c     START BROWSING /THERM_STRESS MODELS
c--------------------------------------------------
c
      CALL HM_OPTION_START('/THERM_STRESS')
c
c--------------------------------------------------
      DO ITH = 1,NTHERM_ST
c
         CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                          OPTION_ID   = MAT_ID ,
     .                          OPTION_TITR = TITR   ,
     .                          KEYWORD2    = KEY    )


         IF(KEY(1:3) == 'MAT')THEN   
            CALL HM_GET_INTV  ('FUNCT_ID'      ,IFUNC_ALPHA    ,IS_AVAILABLE, LSUBMODEL) 
            CALL HM_GET_FLOATV('CLOAD_SCALE_Y' ,FSCAL_ALPHA    ,IS_AVAILABLE, LSUBMODEL, UNITAB)

            IF (FSCAL_ALPHA == ZERO) FSCAL_ALPHA=ONE
            DO I=1,NUMMAT-1
              IF(IPM(1,I) == MAT_ID) THEN
                 ILAW = IPM(2,I)
                 JTHE = NINT(PM(71,I))
                 IF(JTHE==0) THEN
                    IMAT = IPM(1,I)
                    CALL FRETITL2(TITR,IPM(NPROPMI-LTITR+1,I),LTITR)
                    CALL ANCMSG(MSGID=1129,
     .                          MSGTYPE=MSGERROR,
     .                          ANMODE=ANINFO,
     .                          I1=IMAT,
     .                          C1=TITR)
                 ENDIF
                 IPM(218,I)   = 1
                 IPM(219,I)   = IFUNC_ALPHA
                 PM(191,I)   = FSCAL_ALPHA
                 MLAW_TAG(I)%G_TEMP  = 1
                 MLAW_TAG(I)%L_TEMP  = 1
                 WRITE(IOUT,4000)MAT_ID,IFUNC_ALPHA,FSCAL_ALPHA
                 IF (PM(72, I) > ONE) THEN
                    ! Euler or ALE material
                    CALL ANCMSG(MSGID=1723,
     .                          MSGTYPE=MSGERROR,
     .                          ANMODE=ANINFO,
     .                          I1=IMAT,
     .                          C1=TITR)
                 ENDIF
              ENDIF  
            ENDDO
          ENDIF

       ENDDO


c-----------------------------------------      
 4000 FORMAT(
     & 5X,'    THERMAL MATERIAL EXPANSION  ',/,
     & 5X,'    --------------------------  ',/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . . .=',I10/,
     & 5X,'CURVE ID DEFINING THERMAL EXPANSION COEFFICIENT '/,
     & 5X,'   AS A FUNCTION OF TEMPERATURE .=',1PG20.13/,
     & 5X,'THERMAL EXPANSION FUNCTION SCALE FACTOR .=',1PG20.13//)
c-----------------------------------------      
      RETURN
      END
