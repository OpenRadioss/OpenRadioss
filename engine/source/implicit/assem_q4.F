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
Chd|  ASSEM_Q4                      source/implicit/assem_q4.F    
Chd|-- called by -----------
Chd|        Q4KE2                         source/elements/solid_2d/quad4/q4ke2.F
Chd|-- calls ---------------
Chd|        ASSEM_KII                     source/implicit/imp_glob_k.F  
Chd|        ASSEM_KIJ                     source/implicit/imp_glob_k.F  
Chd|====================================================================
      SUBROUTINE ASSEM_Q4(
     1                   IXQ   ,NEL   ,IDDL  ,NDOF  ,K_DIAG,
     2                   K_LT  ,IADK  ,JDIK  ,K11   ,K12   ,
     3                   K13   ,K14   ,K22   ,K23   ,K24   ,
     4                   K33   ,K34   ,K44   ,OFF   )
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
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXQ(NIXQ,*), NEL, IDDL(*), NDOF(*), IADK(*), JDIK(*)
      my_real
     .   K_DIAG(*), K_LT(*),
     .   K11(2,2,*), K12(2,2,*), K13(2,2,*), K14(2,2,*), K22(2,2,*),
     .   K23(2,2,*), K24(2,2,*), K33(2,2,*), K34(2,2,*), K44(2,2,*),
     .   OFF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, EP, NN(MVSIZ,4)
      my_real
     .   KTP(3,3,MVSIZ)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      DO I=1,4
        DO EP=1,NEL
          NN(EP,I) = IXQ(I+1,EP)
        ENDDO
      ENDDO
C
      DO EP=1,NEL
        DO J=1,3
          KTP(1,J,EP) = ZERO
        ENDDO
        DO I=2,3
          KTP(I,1,EP) = ZERO
        ENDDO
      ENDDO
C-----block diagonal kii
      DO EP=1,NEL
        DO J=1,2
        DO I=1,2
          KTP(I+1,J+1,EP) = K11(I,J,EP)
        ENDDO
        ENDDO
      ENDDO
      CALL ASSEM_KII(NN(1,1),NEL,IDDL,IADK,K_DIAG,K_LT,KTP,3,OFF)
      DO EP=1,NEL
        DO J=1,2
        DO I=1,2
          KTP(I+1,J+1,EP) = K22(I,J,EP)
        ENDDO
        ENDDO
      ENDDO
      CALL ASSEM_KII(NN(1,2),NEL,IDDL,IADK,K_DIAG,K_LT,KTP,3,OFF)
      DO EP=1,NEL
        DO J=1,2
        DO I=1,2
          KTP(I+1,J+1,EP) = K33(I,J,EP)
        ENDDO
        ENDDO
      ENDDO
      CALL ASSEM_KII(NN(1,3),NEL,IDDL,IADK,K_DIAG,K_LT,KTP,3,OFF)
      DO EP=1,NEL
        DO J=1,2
        DO I=1,2
          KTP(I+1,J+1,EP) = K44(I,J,EP)
        ENDDO
        ENDDO
      ENDDO
      CALL ASSEM_KII(NN(1,4),NEL,IDDL,IADK,K_DIAG,K_LT,KTP,3,OFF)
C-----non diag kij
      DO EP=1,NEL
        DO J=1,2
        DO I=1,2
          KTP(I+1,J+1,EP) = K12(I,J,EP)
        ENDDO
        ENDDO
      ENDDO
      CALL ASSEM_KIJ(NN(1,1),NN(1,2),NEL,IDDL,IADK,JDIK,
     .               K_DIAG,K_LT,KTP,3,OFF)
      DO EP=1,NEL
        DO J=1,2
        DO I=1,2
          KTP(I+1,J+1,EP) = K13(I,J,EP)
        ENDDO
        ENDDO
      ENDDO
      CALL ASSEM_KIJ(NN(1,1),NN(1,3),NEL,IDDL,IADK,JDIK,
     .               K_DIAG,K_LT,KTP,3,OFF)
      DO EP=1,NEL
        DO J=1,2
        DO I=1,2
          KTP(I+1,J+1,EP) = K14(I,J,EP)
        ENDDO
        ENDDO
      ENDDO
      CALL ASSEM_KIJ(NN(1,1),NN(1,4),NEL,IDDL,IADK,JDIK,
     .               K_DIAG,K_LT,KTP,3,OFF)
      DO EP=1,NEL
        DO J=1,2
        DO I=1,2
          KTP(I+1,J+1,EP) = K23(I,J,EP)
        ENDDO
        ENDDO
      ENDDO
      CALL ASSEM_KIJ(NN(1,2),NN(1,3),NEL,IDDL,IADK,JDIK,
     .               K_DIAG,K_LT,KTP,3,OFF)
      DO EP=1,NEL
        DO J=1,2
        DO I=1,2
          KTP(I+1,J+1,EP) = K24(I,J,EP)
        ENDDO
        ENDDO
      ENDDO
      CALL ASSEM_KIJ(NN(1,2),NN(1,4),NEL,IDDL,IADK,JDIK,
     .               K_DIAG,K_LT,KTP,3,OFF)
      DO EP=1,NEL
        DO J=1,2
        DO I=1,2
          KTP(I+1,J+1,EP) = K34(I,J,EP)
        ENDDO
        ENDDO
      ENDDO
      CALL ASSEM_KIJ(NN(1,3),NN(1,4),NEL,IDDL,IADK,JDIK,
     .               K_DIAG,K_LT,KTP,3,OFF)
C
      RETURN
      END
