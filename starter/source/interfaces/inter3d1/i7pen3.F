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
Chd|  I7PEN3                        source/interfaces/inter3d1/i7pen3.F
Chd|-- called by -----------
Chd|        I20INI3                       source/interfaces/inter3d1/i20ini3.F
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
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I7PEN3(MARGE,GAPV,N1 ,N2 ,N3 ,
     1                  PENE ,NX1 ,NY1,NZ1,NX2,
     2                  NY2  ,NZ2 ,NX3,NY3,NZ3,
     3                  NX4  ,NY4 ,NZ4,P1 ,P2 ,
     4                  P3   ,P4)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .        MARGE, GAPV(*)
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: N1,N2,N3,PENE
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: NX1,NY1,NZ1
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: NX2,NY2,NZ2
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: NX3,NY3,NZ3
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: NX4,NY4,NZ4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: P1,P2,P3,P4
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect07_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .     D1,D2,D3,D4,S2
C-----------------------------------------------
       DO I=LFT,LLT
        D1 = SQRT(P1(I))
        P1(I) = MAX(ZERO, GAPV(I) + MARGE - D1)
C
        D2 = SQRT(P2(I))
        P2(I) = MAX(ZERO, GAPV(I) + MARGE - D2)
C
        D3 = SQRT(P3(I))
        P3(I) = MAX(ZERO, GAPV(I) + MARGE - D3)
C
        D4 = SQRT(P4(I))
        P4(I) = MAX(ZERO, GAPV(I) + MARGE - D4)
       ENDDO
C
       DO I=LFT,LLT
        PENE(I) = MAX(P1(I),P2(I),P3(I),P4(I))
        if(P1(I)==PENE(I))then
         N1(I) = NX1(I)
         N2(I) = NY1(I) 
         N3(I) = NZ1(I) 
        elseif(P2(I)==PENE(I))then
         N1(I) = NX2(I)
         N2(I) = NY2(I) 
         N3(I) = NZ2(I) 
        elseif(P3(I)==PENE(I))then
         N1(I) = NX3(I)
         N2(I) = NY3(I) 
         N3(I) = NZ3(I) 
        elseif(P4(I)==PENE(I))then
         N1(I) = NX4(I)
         N2(I) = NY4(I) 
         N3(I) = NZ4(I) 
        endif
       ENDDO
       DO I=LFT,LLT
         S2 = ONE/MAX(EM30,SQRT(N1(I)**2 + N2(I)**2 + N3(I)**2))
         N1(I) = N1(I)*S2
         N2(I) = N2(I)*S2
         N3(I) = N3(I)*S2
       ENDDO
C
      RETURN
      END
