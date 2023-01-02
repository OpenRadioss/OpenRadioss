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
C
Chd|====================================================================
Chd|  C_MAD                         source/restart/ddsplit/c_mad.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|====================================================================
      SUBROUTINE C_MAD(IEXMAD,NMADSH4_L,NMADSH3_L,NMADSOL_L,NMADNOD_L,
     +                 MADCL_NMADNOD_L,CEP      ,PROC)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------     

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       INTEGER  PROC, NMADSH4_L, NMADSH3_L, NMADSOL_L, NMADNOD_L,
     .          MADCL_NMADNOD_L,IEXMAD(*),CEP(*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL    
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IDEB, K, ESHIFT, P
C
C Couplage etendu
C
      IF(NEXMAD/=0) THEN
C
C ELEM shell4
C
        IDEB = 1 + 7*NCONX + NMADPRT
        ESHIFT = NUMELS+NUMELQ
        DO I = 1, NMADSH4
          K = IEXMAD(IDEB+I-1)
          IF(CEP(K+ESHIFT)==PROC) THEN
            NMADSH4_L = NMADSH4_L+1
          END IF
        END DO
C
C Elem shell3
C
        IDEB = IDEB + NMADSH4
        ESHIFT = NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+NUMELR
        DO I = 1, NMADSH3
          K = IEXMAD(IDEB+I-1)
          IF(CEP(K+ESHIFT)==PROC) THEN
            NMADSH3_L = NMADSH3_L+1
          END IF
        END DO
C
C Elem solides
C
        IDEB = IDEB + NMADSH3
        ESHIFT = 0
        DO I = 1, NMADSOL
          K = IEXMAD(IDEB+I-1)
          IF(CEP(K+ESHIFT)==PROC) THEN
            NMADSOL_L = NMADSOL_L+1
          END IF
        END DO
C
C Noeuds
C
        IDEB = IDEB + NMADSOL
        DO I = 1, NMADNOD
          K = IEXMAD(IDEB+I-1)
          IF(NLOCAL(K,PROC+1)==1) THEN
            DO P = 1, PROC
              IF(NLOCAL(K,P)==1) GOTO 100
            END DO
            NMADNOD_L = NMADNOD_L+1
          END IF
 100      CONTINUE
        END DO
C
        MADCL_NMADNOD_L=0
        DO I = 1, NMADNOD
          K = IEXMAD(IDEB+I-1)
          IF(NLOCAL(K,PROC+1)==1) THEN
            MADCL_NMADNOD_L = MADCL_NMADNOD_L + 1
          END IF
        END DO
      END IF
C
      RETURN
      END
