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
C
Chd|====================================================================
Chd|  C_ISPH                        source/restart/ddsplit/c_isph.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE C_ISPH(KXSP,CEPSP,PROC,NUMSPH_L,
     .                  SLONFSPH_L,SLPRTSPH,SLPRTSPH_L,IPARTSP,
     .      SSPHVELN_L,NSPHSOL_L,FIRST_SPHSOL_L)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER PROC, NUMSPH_L, NSPHSOL_L, FIRST_SPHSOL_L,
     .        KXSP(NISP,*), CEPSP(*)

c inlet outlet spmd
      INTEGER SLONFSPH_L, IPRT, N, IPARTSP(*),SLPRTSPH_L,SLPRTSPH,
     .SSPHVELN_L

C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NB_L, IUN
      my_real
     .        AUX
      DATA IUN/1/
C
      NB_L = 0
C
      DO I = 1, NUMSPH
        IF(CEPSP(I)==PROC) THEN
          NB_L = NB_L + 1
        ENDIF
      ENDDO
C
      NUMSPH_L = NB_L
C
c inlet spmd calcul SLONFSPH_L
      SLONFSPH_L = 0

      DO IPRT=1,NPART
       DO N=1,NUMSPH
        IF(CEPSP(N)==PROC.and.IPARTSP(N)==IPRT.
     .     AND.KXSP(2,N)/=0)THEN
         SLONFSPH_L=SLONFSPH_L+1
        ENDIF
       ENDDO
      ENDDO

      SLPRTSPH_L=SLPRTSPH
      SSPHVELN_L=NSPHIO*NUMSPH_L*2
C
      NB_L = 0
      DO I = 1, FIRST_SPHSOL-1
        IF(CEPSP(I)==PROC) THEN
          NB_L = NB_L + 1
        ENDIF
      ENDDO
      FIRST_SPHSOL_L = NB_L+1
C
      NB_L = 0
      DO I = 1, NSPHSOL
        IF(CEPSP(FIRST_SPHSOL+I-1)==PROC) THEN
          NB_L = NB_L + 1
        ENDIF
      ENDDO
      NSPHSOL_L = NB_L

      RETURN
      END
