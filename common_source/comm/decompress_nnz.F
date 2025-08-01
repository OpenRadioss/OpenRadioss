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
!||    decompress_i_nnz   ../common_source/comm/decompress_nnz.F
!||--- called by ------------------------------------------------------
!||    intbuf_ini         ../common_source/interf/intbuf_ini.F
!||    rdresb             ../engine/source/output/restart/rdresb.F
!||--- calls      -----------------------------------------------------
!||    read_i_c           ../common_source/tools/input_output/write_routtines.c
!||====================================================================
       SUBROUTINE DECOMPRESS_I_NNZ(ARRAY,LEN)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C---------------------------------------------
C         Arguments
C---------------------------------------------
       INTEGER, INTENT(IN) :: LEN
       INTEGER, INTENT(INOUT) :: ARRAY(LEN)
C---------------------------------------------
C        Local Variables
C---------------------------------------------

       INTEGER I,J,IBEGIN, NNZ
       
       CALL READ_I_C(IBEGIN,1)
       J = 2
       DO WHILE(IBEGIN > 0)
         CALL READ_I_C(NNZ,1)
         J = J +1
         CALL READ_I_C(ARRAY(IBEGIN:IBEGIN+NNZ-1),NNZ)
         J = J + NNZ
         CALL READ_I_C(IBEGIN,1)
         J = J + 1
       END DO

       END SUBROUTINE


!||====================================================================
!||    decompress_r_nnz   ../common_source/comm/decompress_nnz.F
!||--- called by ------------------------------------------------------
!||    intbuf_ini         ../common_source/interf/intbuf_ini.F
!||    rdresb             ../engine/source/output/restart/rdresb.F
!||--- calls      -----------------------------------------------------
!||    read_db            ../common_source/tools/input_output/read_db.F
!||    read_i_c           ../common_source/tools/input_output/write_routtines.c
!||====================================================================
       SUBROUTINE DECOMPRESS_R_NNZ(ARRAY,LEN)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C---------------------------------------------
C         Arguments
C---------------------------------------------
       INTEGER, INTENT(IN) :: LEN
       my_real, INTENT(INOUT) :: ARRAY(LEN)
C---------------------------------------------
C        Local Variables
C---------------------------------------------

       INTEGER I,J,IBEGIN, NNZ
       
       CALL READ_I_C(IBEGIN,1)
       J = 2
       DO WHILE(IBEGIN > 0)
         CALL READ_I_C(NNZ,1)
         J = J +1
         CALL READ_DB(ARRAY(IBEGIN:IBEGIN+NNZ-1),NNZ)
         J = J + NNZ
         CALL READ_I_C(IBEGIN,1)
         J = J + 1
       END DO

       END SUBROUTINE

