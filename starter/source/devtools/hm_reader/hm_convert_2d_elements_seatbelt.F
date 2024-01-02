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
Chd|  HM_CONVERT_2D_ELEMENTS_SEATBELTsource/devtools/hm_reader/hm_convert_2d_elements_seatbelt.F
Chd|-- called by -----------
Chd|        STARTER0                      source/starter/starter0.F     
Chd|-- calls ---------------
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_NEXT                source/devtools/hm_reader/hm_option_next.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_CONVERT_2D_ELEMENTS_SEATBELT(IDMAX_PART,IDMAX_PROP,IDMAX_MAT,IDMAX_ELEM,IDMAX_TH,
     .                                          SEATBELT_CONVERTED_ELEMENTS,NB_SEATBELT_SHELLS,LSUBMODEL)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   ROUTINE TO CONVERT /SHELL & /MAT/LAW119 -> /SPRING /PART /PROP/TYPE23 & /MAT/LAW114
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME          DESCRIPTION                         
C    
C     IDMAX_PART                    MAX ID PARTS
C     IDMAX_PROP                    MAX ID PROPS
C     IDMAX_MAT                     MAX ID MATS
C     IDMAX_ELEM                    MAX ID ELEMS
C     IDMAX_TH                      MAX ID TH
C     SEATBELT_CONVERTED_ELEMENTS   CORRESPONDANCE SHELL SPRING FOR SEATBLETS
C     NB_SEATBELT_SHELLS            NUMBER OF SEATELT SHELL
C     LSUBMODEL                     SUBMODEL STRUCTURE    
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD       
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "nchar_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      INTEGER,INTENT(IN)::IDMAX_PART,IDMAX_PROP,IDMAX_MAT,IDMAX_ELEM,IDMAX_TH,
     .                    NB_SEATBELT_SHELLS
      INTEGER,INTENT(INOUT)::SEATBELT_CONVERTED_ELEMENTS(3,NB_SEATBELT_SHELLS)
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(NSUBMOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ID,SUBMODEL_INDEX,OFFSET,NB_PART,
     .        NB_MAT,NB_MAT_119,MAT_ID,ELEM_INDEX,CPT,NB_THSHELL
      INTEGER, DIMENSION(:), ALLOCATABLE :: PART_SUB,PART_MAT119,MAT119_IDS
      CHARACTER KEY*ncharline
      LOGICAL IS_AVAILABLE
      INTEGER :: WORK(70000)
      INTEGER :: INDEX(2*NB_SEATBELT_SHELLS)
C-----------------------------------------------
      NB_MAT_119 = 0
C-----------------------------------------------
C   Check existence of Mat laws 119 in the model
C-----------------------------------------------
      CALL HM_OPTION_COUNT('/MAT',NB_MAT)
c
      CALL HM_OPTION_START('/MAT')
      DO I=1,NB_MAT
        KEY  = ''
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                          KEYWORD2    = KEY)
        IF(KEY(1:6) == 'LAW119' .OR. KEY(1:11) == 'SH_SEATBELT') THEN
          NB_MAT_119 = NB_MAT_119 + 1
        ENDIF
      ENDDO
C-----------------------------------------------
C   Search Parts with Mat 119 
C-----------------------------------------------
      IF (NB_MAT_119 /= 0) THEN
        ALLOCATE(MAT119_IDS(NB_MAT_119))
        MAT119_IDS(1:NB_MAT_119) = 0
        J = 0
        CALL HM_OPTION_START('/MAT')
        DO I=1,NB_MAT
          KEY  = ''
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                          OPTION_ID   = ID,
     .                          KEYWORD2    = KEY,
     .                          SUBMODEL_INDEX = SUBMODEL_INDEX)
          IF(KEY(1:6) == 'LAW119' .OR. KEY(1:11) == 'SH_SEATBELT') THEN 
            J = J + 1
            MAT119_IDS(J) = ID
          ENDIF
        ENDDO
C-----------------------------------------------
        CALL HM_OPTION_COUNT('/PART',NB_PART)
        ALLOCATE(PART_SUB(NB_PART))
        ALLOCATE(PART_MAT119(NB_PART))
        PART_SUB(1:NB_PART) = 0
        PART_MAT119(1:NB_PART) = 0
c
        CALL HM_OPTION_START('/PART')
        DO I=1,NB_PART
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = ID,
     .                       SUBMODEL_INDEX = SUBMODEL_INDEX)
          CALL HM_GET_INTV('materialid',MAT_ID,IS_AVAILABLE,LSUBMODEL)
          DO J=1,NB_MAT_119
            IF(MAT_ID == MAT119_IDS(J)) PART_MAT119(I) = MAT119_IDS(J)
          ENDDO
          PART_SUB(I) = SUBMODEL_INDEX
        ENDDO
C-----------------------------------------------
C   Translate /SHELL -> /SPRING /PART /PROP/TYPE23 & /MAT/LAW114
C-----------------------------------------------
c
        ELEM_INDEX = 0
        CALL HM_OPTION_START('/PART')
        DO I=1,NB_PART
          CALL HM_OPTION_NEXT()
          IF(PART_MAT119(I) /= 0) THEN
            CALL CPP_GET_SUBMODEL_INDEX(SUBMODEL_INDEX)
            OFFSET = 0
            CALL CPP_CONVERT_2D_ELEMENTS_SEATBELT(PART_MAT119,IDMAX_PART,IDMAX_PROP,IDMAX_MAT,
     .                                           IDMAX_ELEM,OFFSET,SEATBELT_CONVERTED_ELEMENTS,ELEM_INDEX)
          ENDIF
        ENDDO
c
        IF (ALLOCATED(PART_SUB)) DEALLOCATE(PART_SUB)
        IF (ALLOCATED(PART_MAT119)) DEALLOCATE(PART_MAT119)
        IF (ALLOCATED(MAT119_IDS)) DEALLOCATE(MAT119_IDS)
C-----------------------------------------------
C   Translate /TH containing /SHELLs THAT ARE TRANSLATED TO /SPRINGs
C-----------------------------------------------
c
        CALL HM_OPTION_COUNT('/TH/SHEL',NB_THSHELL)
        CALL HM_OPTION_START('/TH/SHEL')
        DO I=1,NB_THSHELL
          CALL HM_OPTION_NEXT()
          CALL CPP_GET_SUBMODEL_INDEX(SUBMODEL_INDEX)
          OFFSET = 0
          CALL CPP_CONVERT_TH_2D_ELEMENT_SEATBELT(IDMAX_TH,OFFSET,SEATBELT_CONVERTED_ELEMENTS,NB_SEATBELT_SHELLS)
        ENDDO
c
C--------------------------------------------------
      ENDIF
      RETURN
C
      END

