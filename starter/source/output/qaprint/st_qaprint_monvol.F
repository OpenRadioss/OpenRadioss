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
Chd|  ST_QAPRINT_MONVOL             source/output/qaprint/st_qaprint_monvol.F
Chd|-- called by -----------
Chd|        ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|-- calls ---------------
Chd|        MONVOL_STRUCT_MOD             share/modules1/monvol_struct_mod.F
Chd|====================================================================
      SUBROUTINE ST_QAPRINT_MONVOL(T_MONVOL, T_MONVOL_METADATA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE QA_OUT_MOD
      USE MONVOL_STRUCT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
!     NVOLU
#include "com04_c.inc"
!     NIMV
#include "param_c.inc"
!     nchartitle
#include "nchar_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(MONVOL_STRUCT_), DIMENSION(NVOLU), INTENT(IN) :: T_MONVOL
      TYPE(MONVOL_METADATA_), INTENT(IN) :: T_MONVOL_METADATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: NN, II, JJ, KK, ID
      CHARACTER(LEN = nchartitle) :: TITLE
      CHARACTER(LEN = 255) :: VARNAME
      INTEGER :: NJET, NVENT
      DOUBLE PRECISION :: FVALUE
      INTEGER, DIMENSION(NVOLU) :: IDX, IDS
      LOGICAL :: OK_QA
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      OK_QA = MYQAKEY('/MONVOL')
      IF (OK_QA) THEN
         IF (NVOLU > 0) THEN
!     Sort by ID to ensure internal order independnat output
            DO II = 1, NVOLU
               IDS(II) = T_MONVOL(II)%ID
               IDX(II) = II
            ENDDO
            CALL QUICKSORT_I2(IDS, IDX, 1, NVOLU)
!     ICBAG
            DO II = 1, NICBAG
               DO JJ = 1, NVOLU * NVOLU
                  WRITE(VARNAME, '(A, I0, A,  I0)') 'ICBAG_', II, '_', JJ
                  IF (T_MONVOL_METADATA%ICBAG(II, JJ) /= 0) THEN
                     CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),
     .                    T_MONVOL_METADATA%ICBAG(II, JJ), 0.0_8)
                  ENDIF
               ENDDO
            ENDDO
!     RCBAG
            DO II = 1, NICBAG
               DO JJ = 1, NVOLU * NVOLU
                  WRITE(VARNAME, '(A, I0, A, I0)') 'RCBAG_', II, '_', JJ
                  IF (T_MONVOL_METADATA%RCBAG(II, JJ) /= ZERO) THEN
                     FVALUE = T_MONVOL_METADATA%RCBAG(II, JJ)
                     CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),
     .                    0, FVALUE)
                  ENDIF
               ENDDO
            ENDDO
         ENDIF
         DO KK = 1, NVOLU
            NN = IDX(KK)
            ID = T_MONVOL(NN)%ID
            TITLE = T_MONVOL(NN)%TITLE
            IF (LEN_TRIM(TITLE) == 0) THEN
               TITLE = "MONVOL_FAKE_TITLE"
            ENDIF
            CALL QAPRINT(TITLE(1:LEN_TRIM(TITLE)), ID, 0.0_8)
!     IVOLU
            DO II = 1, NIMV
               WRITE(VARNAME, '(A, I0)') 'IVOLU_', II
               IF (T_MONVOL(NN)%IVOLU(II) /= 0) THEN
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), T_MONVOL(NN)%IVOLU(II), 0.0_8)
               ENDIF
            ENDDO
!     IBAGJET
            NJET = T_MONVOL(NN)%NJET
            IF (NJET > 0) THEN
               DO II = 1, NIBJET
                  DO JJ = 1, NJET
                     WRITE(VARNAME, '(A, I0, A, I0)') 'IBAGJET_', II, '_',  JJ
                     IF (T_MONVOL(NN)%IBAGJET(II, JJ) /= 0) THEN
                        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), T_MONVOL(NN)%IBAGJET(II, JJ), 0.0_8)
                     ENDIF
                  ENDDO
               ENDDO
            ENDIF
!     IBAGHOL
            NVENT = T_MONVOL(NN)%NVENT
            IF (NVENT > 0) THEN
               DO II = 1, NIBHOL
                  DO JJ = 1, NVENT
                     WRITE(VARNAME, '(A, I0, A, I0)') 'IBAGHOL_', II, '_', JJ
                     IF (T_MONVOL(NN)%IBAGHOL(II, JJ) /= 0) THEN
                        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), T_MONVOL(NN)%IBAGHOL(II, JJ), 0.0_8)
                     ENDIF
                  ENDDO
               ENDDO
            ENDIF
!     RVOLU
            DO II = 1, NRVOLU
               WRITE(VARNAME, '(A, I0)') 'RVOLU_', II
               IF (T_MONVOL(NN)%RVOLU(II) /= ZERO) THEN
                  FVALUE = T_MONVOL(NN)%RVOLU(II)
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), 0, FVALUE)
               ENDIF
            ENDDO
!     RBAGJET
            IF (NJET > 0) THEN
               DO II = 1, NRBJET
                  DO JJ = 1, NJET
                     WRITE(VARNAME, '(A, I0, A, I0)') 'RBAGJET_', II, '_', JJ
                     IF (T_MONVOL(NN)%RBAGJET(II, JJ) /= ZERO) THEN
                        FVALUE = T_MONVOL(NN)%RBAGJET(II, JJ)
                        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), 0, FVALUE)
                     ENDIF
                  ENDDO
               ENDDO
            ENDIF
!     RBAGHOL
            IF (NVENT > 0) THEN
               DO II = 1, NRBHOL
                  DO JJ = 1, NVENT
                     WRITE(VARNAME, '(A, I0, A, I0)') 'RBAGHOL_', II, '_', JJ
                     IF (T_MONVOL(NN)%RBAGHOL(II, JJ) /= ZERO) THEN
                        FVALUE = T_MONVOL(NN)%RBAGHOL(II, JJ)
                        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), 0, FVALUE)
                     ENDIF
                  ENDDO
               ENDDO
            ENDIF
         ENDDO
      ENDIF
C-----------------------------------------------
C     E n d   o f   S u b r o u t i n e
C-----------------------------------------------
      END SUBROUTINE ST_QAPRINT_MONVOL
