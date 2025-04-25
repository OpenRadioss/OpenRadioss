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
      !||    compress_c_nnz   ../common_source/comm/compress_nnz.F
      !||--- calls      -----------------------------------------------------
      !||    write_i_c        ../common_source/tools/input_output/write_routtines.c
      !||====================================================================
       SUBROUTINE COMPRESS_C_NNZ(ARRAY,LEN)
       IMPLICIT NONE
C---------------------------------------------
C         Arguments
C---------------------------------------------
       INTEGER, INTENT(IN) :: LEN
       INTEGER, INTENT(IN) :: ARRAY(LEN)
C      INTEGER, INTENT(INOUT) :: COMPR(*)
C---------------------------------------------
C        Local Variables
C---------------------------------------------
       INTEGER I,J,IBEGIN, NNZ
       LOGICAL continue_loop 
        

         IBEGIN = 1
         J = 1
         DO WHILE (IBEGIN <= LEN)
           I = IBEGIN
           NNZ = 0
           continue_loop = (ARRAY(I) /= ICHAR(' '))
           DO WHILE(continue_loop)
              IF(I <= LEN) THEN
                 IF(ARRAY(I) /= ICHAR(' ')) THEN
                   continue_loop = .true.
                   NNZ = NNZ + 1
                   I = I + 1
                 ELSE
                   continue_loop = .false.
                 ENDIF
              ELSE
                continue_loop = .false.
              ENDIF
           ENDDO
           IF(NNZ > 0) THEN
C            COMPR(J) = IBEGIN
C            COMPR(J+1) = NNZ
             CALL WRITE_I_C(IBEGIN,1)
             CALL WRITE_I_C(NNZ,1)
             J = J + 2
             CALL WRITE_I_C(ARRAY(IBEGIN:IBEGIN+NNZ-1),NNZ)
             J = J + NNZ
           ENDIF
           IBEGIN = I + 1
         ENDDO
C        COMPR(J) = -1
         I = -1
         CALL WRITE_I_C(I,1) 
C        WRITE(6,*) LEN,"compressed into ",J, dble(J)/dble(LEN)
       END SUBROUTINE 


      !||====================================================================
      !||    compress_i_nnz    ../common_source/comm/compress_nnz.F
      !||--- called by ------------------------------------------------------
      !||    copy_ival_dummy   ../starter/source/restart/ddsplit/inter_tools.F
      !||    ddsplit           ../starter/source/restart/ddsplit/ddsplit.F
      !||    write_intbuf      ../engine/source/output/restart/write_intbuf.F
      !||    wrrestp           ../engine/source/output/restart/wrrestp.F
      !||--- calls      -----------------------------------------------------
      !||    write_i_c         ../common_source/tools/input_output/write_routtines.c
      !||====================================================================
       SUBROUTINE COMPRESS_I_NNZ(ARRAY,LEN)
       IMPLICIT NONE
C---------------------------------------------
C         Arguments
C---------------------------------------------
       INTEGER, INTENT(IN) :: LEN
       INTEGER, INTENT(IN) :: ARRAY(LEN)
C      INTEGER, INTENT(INOUT) :: COMPR(*)
C---------------------------------------------
C        Local Variables
C---------------------------------------------
       INTEGER I,J,IBEGIN, NNZ
       LOGICAL continue_loop 

         IBEGIN = 1
         J = 1
         DO WHILE (IBEGIN <= LEN)
           I = IBEGIN
           NNZ = 0
           continue_loop = (ARRAY(I) /= 0)
           DO WHILE(continue_loop)
              IF(I <= LEN) THEN
                 IF(ARRAY(I) /= 0) THEN
                   continue_loop = .true.
                   NNZ = NNZ + 1
                   I = I + 1
                 ELSE
                   continue_loop = .false.
                 ENDIF
              ELSE
                continue_loop = .false.
              ENDIF
           ENDDO
           IF(NNZ > 0) THEN
C            COMPR(J) = IBEGIN
C            COMPR(J+1) = NNZ
             CALL WRITE_I_C(IBEGIN,1)
             CALL WRITE_I_C(NNZ,1)
             J = J + 2
             CALL WRITE_I_C(ARRAY(IBEGIN:IBEGIN+NNZ-1),NNZ)
             J = J + NNZ
           ENDIF
           IBEGIN = I + 1
         ENDDO
C        COMPR(J) = -1
         I = -1
         CALL WRITE_I_C(I,1) 
C        WRITE(6,*) LEN,"I compressed into ",J, dble(J)/dble(LEN)
       END SUBROUTINE 

      !||====================================================================
      !||    compress_r_nnz          ../common_source/comm/compress_nnz.F
      !||--- called by ------------------------------------------------------
      !||    ddsplit                 ../starter/source/restart/ddsplit/ddsplit.F
      !||    split_cand_rval_dummy   ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_node_rval_dummy   ../starter/source/restart/ddsplit/inter_tools.F
      !||    w_geo                   ../starter/source/restart/ddsplit/w_geo.F
      !||    write_intbuf            ../engine/source/output/restart/write_intbuf.F
      !||    wrrestp                 ../engine/source/output/restart/wrrestp.F
      !||--- calls      -----------------------------------------------------
      !||    write_db                ../common_source/tools/input_output/write_db.F
      !||    write_i_c               ../common_source/tools/input_output/write_routtines.c
      !||====================================================================
       SUBROUTINE COMPRESS_R_NNZ(ARRAY,LEN)
#include      "implicit_f.inc"
C---------------------------------------------
C         Arguments
C---------------------------------------------
       INTEGER, INTENT(IN) :: LEN
       my_real , INTENT(IN) :: ARRAY(LEN)
C      INTEGER, INTENT(INOUT) :: COMPR(*)
C---------------------------------------------
C        Local Variables
C---------------------------------------------
       INTEGER I,J,IBEGIN, NNZ
       LOGICAL continue_loop 

         IBEGIN = 1
         J = 1
         DO WHILE (IBEGIN <= LEN)
           I = IBEGIN
           NNZ = 0
           continue_loop = (ARRAY(I) /= 0)
           DO WHILE(continue_loop)
              IF(I <= LEN) THEN
                 IF(ARRAY(I) /= 0) THEN
                   continue_loop = .true.
                   NNZ = NNZ + 1
                   I = I + 1
                 ELSE
                   continue_loop = .false.
                 ENDIF
              ELSE
                continue_loop = .false.
              ENDIF
           ENDDO
           IF(NNZ > 0) THEN
C            COMPR(J) = IBEGIN
C            COMPR(J+1) = NNZ
             CALL WRITE_I_C(IBEGIN,1)
             CALL WRITE_I_C(NNZ,1)
             J = J + 2
             CALL WRITE_DB(ARRAY(IBEGIN:IBEGIN+NNZ-1),NNZ)
             J = J + NNZ
           ENDIF
           IBEGIN = I + 1
         ENDDO
C        COMPR(J) = -1
         I = -1
         CALL WRITE_I_C(I,1) 
C        WRITE(6,*) LEN,"R compressed into ",J, dble(J)/dble(LEN)
       END SUBROUTINE 
