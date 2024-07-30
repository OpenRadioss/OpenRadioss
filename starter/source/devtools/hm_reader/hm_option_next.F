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
      !||====================================================================
      !||    hm_option_next                    ../starter/source/devtools/hm_reader/hm_option_next.F
      !||--- called by ------------------------------------------------------
      !||    contrl                            ../starter/source/starter/contrl.F
      !||    hm_convert_2d_elements_seatbelt   ../starter/source/devtools/hm_reader/hm_convert_2d_elements_seatbelt.F
      !||    hm_convert_inter_type19           ../starter/source/devtools/hm_reader/hm_convert_inter_type19.F
      !||    hm_count_2d_element_seatbelt      ../starter/source/devtools/hm_reader/hm_count_2d_element_seatbelt.F
      !||    hm_read_ale_muscl                 ../starter/source/general_controls/ale_cfd/hm_read_ale_muscl.F
      !||    hm_read_ale_solver                ../starter/source/general_controls/ale_cfd/hm_read_ale_solver.F
      !||    hm_read_upwind                    ../starter/source/general_controls/computation/hm_read_upwind.F
      !||--- calls      -----------------------------------------------------
      !||--- uses       -----------------------------------------------------
      !||    message_mod                       ../starter/share/message_module/message_mod.F
      !||    submodel_mod                      ../starter/share/modules1/submodel_mod.F
      !||====================================================================
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

