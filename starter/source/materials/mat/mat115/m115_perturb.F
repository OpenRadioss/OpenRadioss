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
Chd|  M115_PERTURB                  source/materials/mat/mat115/m115_perturb.F
Chd|-- called by -----------
Chd|        S4INIT3                       source/elements/solid/solide4/s4init3.F
Chd|        SINIT3                        source/elements/solid/solide/sinit3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE M115_PERTURB(PM       ,MAT      ,RHO      ,PERTURB  ,RNOISE   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER PERTURB(NPERTURB),MAT(*)
      my_real  
     .   RHO(2*LLT),RNOISE(NPERTURB,*),PM(NPROPM,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
      my_real
     .        RHO0
C-----------------------------------------------
      ! Loop over elements
      DO I = LFT,LLT
        ! Initial constant density
        RHO0   = PM(1,MAT(I))
        RHO(I) = RHO0
        ! If /PERTURB/SOLID/PART card is set
        IF (NPERTURB /= 0) THEN
          DO J=1,NPERTURB
            IF (PERTURB(J) == 3 .AND. RNOISE(J,NUMELC+NUMELTG+I+NFT) /= ZERO) THEN
              RHO(I)  = RHO(I) * RNOISE(J,NUMELC+NUMELTG+I+NFT)
            ENDIF
          ENDDO
        ENDIF
        ! Copy of initial density on the second length of RHO
        RHO(I+LLT) = RHO(I)
      ENDDO
C-----------------------------------------------
      END SUBROUTINE M115_PERTURB