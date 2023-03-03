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
Chd|  HM_OPTION_NEXT                source/devtools/hm_reader/hm_option_next.F
Chd|-- called by -----------
Chd|        CONTRL                        source/starter/contrl.F       
Chd|        HM_CONVERT_2D_ELEMENTS_SEATBELTsource/devtools/hm_reader/hm_convert_2d_elements_seatbelt.F
Chd|        HM_CONVERT_INTER_TYPE19       source/devtools/hm_reader/hm_convert_inter_type19.F
Chd|        HM_COUNT_2D_ELEMENT_SEATBELT  source/devtools/hm_reader/hm_count_2d_element_seatbelt.F
Chd|        HM_READ_ALE_MUSCL             source/general_controls/ale_cfd/hm_read_ale_muscl.F
Chd|        HM_READ_ALE_SOLVER            source/general_controls/ale_cfd/hm_read_ale_solver.F
Chd|        HM_READ_UPWIND                source/general_controls/computation/hm_read_upwind.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_OPTION_NEXT()
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   GENERIC ROUTINE TO GO TO NEXT /OPTION AFTER HM_OPTION_START
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME          DESCRIPTION                         
C 
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD       
      USE SUBMODEL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
      CALL CPP_OPTION_NEXT() 
C--------------------------------------------------
      RETURN
C
      END

