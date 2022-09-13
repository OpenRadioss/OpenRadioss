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
Chd|  COMPRESS_C_NNZ                comm/compress_nnz.F           
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        WRITE_I_C                     starter/source/output/tools/write_routines.c
Chd|        WRITE_I_C                     engine/source/output/tools/sortie_c.c
Chd|====================================================================
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


Chd|====================================================================
Chd|  COMPRESS_I_NNZ                comm/compress_nnz.F           
Chd|-- called by -----------
Chd|        DDSPLIT                       starter/source/restart/ddsplit/ddsplit.F
Chd|        WRRESTP                       engine/source/output/restart/wrrestp.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     starter/source/output/tools/write_routines.c
Chd|        WRITE_I_C                     engine/source/output/tools/sortie_c.c
Chd|====================================================================
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

Chd|====================================================================
Chd|  COMPRESS_R_NNZ                comm/compress_nnz.F           
Chd|-- called by -----------
Chd|        DDSPLIT                       starter/source/restart/ddsplit/ddsplit.F
Chd|        W_GEO                         starter/source/restart/ddsplit/w_geo.F
Chd|        WRRESTP                       engine/source/output/restart/wrrestp.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      starter/source/restart/ddsplit/wrrest.F
Chd|        WRITE_DB                      engine/source/output/tools/write_db.F
Chd|        WRITE_I_C                     starter/source/output/tools/write_routines.c
Chd|        WRITE_I_C                     engine/source/output/tools/sortie_c.c
Chd|====================================================================
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
