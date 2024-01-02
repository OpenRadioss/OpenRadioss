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
Chd|  I7CMP3B                       source/interfaces/inter3d1/i7cmp3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I7CMP3B(I_STOK,CAND_E  ,CAND_N, PROV_E, PROV_N,
     1                   PENE)
C============================================================================
C  cette routine est appelee par : I7BUC1(/inter3d1/i7buc1.F)
C                                  I7TRI(/inter3d1/i7tri.F)
C----------------------------------------------------------------------------
C  cette routine appelle : -
C============================================================================
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect07_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I_STOK
      INTEGER CAND_N(*),CAND_E(*),PROV_N(MVSIZ),PROV_E(MVSIZ),PENE(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
C-----------------------------------------------
C     CAS DECHARGEMENT
      DO I=LFT,LLT
        IF(PENE(I)/=0.0)THEN
          I_STOK = I_STOK + 1
          CAND_N(I_STOK) = PROV_N(I)
          CAND_E(I_STOK) = PROV_E(I)
        ENDIF
      ENDDO     
      RETURN
      END
      
Chd|====================================================================
Chd|  I7CMP3                        source/interfaces/inter3d1/i7cmp3.F
Chd|-- called by -----------
Chd|        I21BUC1                       source/interfaces/inter3d1/i21buc3.F
Chd|        I21TRI                        source/interfaces/inter3d1/i21tri.F
Chd|        I23BUC1                       source/interfaces/inter3d1/i23buc3.F
Chd|        I23TRI                        source/interfaces/inter3d1/i23tri.F
Chd|        I24BUC1                       source/interfaces/inter3d1/i24buc1.F
Chd|        I24TRI                        source/interfaces/inter3d1/i24tri.F
Chd|        I25TRI                        source/interfaces/inter3d1/i25tri.F
Chd|        I7BUC1                        source/interfaces/inter3d1/i7buc1.F
Chd|        I7BUC_VOX1                    source/interfaces/inter3d1/i7buc_vox1.F
Chd|        I7TRI                         source/interfaces/inter3d1/i7tri.F
Chd|        I7TRIVOX1                     source/interfaces/inter3d1/i7trivox1.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I7CMP3(I_STOK,CAND_E  ,CAND_N, IFLAG,PENE,
     1                  PROV_N,PROV_E)
C============================================================================
C  cette routine est appelee par : I7BUC1(/inter3d1/i7buc1.F)
C                                  I7TRI(/inter3d1/i7tri.F)
C----------------------------------------------------------------------------
C  cette routine appelle : -
C============================================================================
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect07_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I_STOK,J_STOK,IFLAG
      INTEGER CAND_N(*),CAND_E(*)
      INTEGER, DIMENSION(MVSIZ), INTENT(IN) :: PROV_N,PROV_E    
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: PENE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
C-----------------------------------------------
C
C
      IF(IFLAG==0) THEN
C     CAS DENOMBREMENT SEULEMENT
      DO 100 I=LFT,LLT
      IF(PENE(I)/=ZERO)THEN
       I_STOK = I_STOK + 1
      ENDIF
 100  CONTINUE
      ELSE
C     CAS DECHARGEMENT
      DO 200 I=LFT,LLT
      IF(PENE(I)/=0.0)THEN
       I_STOK = I_STOK + 1
       CAND_N(I_STOK) = PROV_N(I)
       CAND_E(I_STOK) = PROV_E(I)
      ENDIF
 200  CONTINUE
      ENDIF
C
      RETURN
      END
