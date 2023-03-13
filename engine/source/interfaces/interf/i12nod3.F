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
Chd|  I12NOD3                       source/interfaces/interf/i12nod3.F
Chd|-- called by -----------
Chd|        INTTI12A                      source/interfaces/interf/intti12.F
Chd|-- calls ---------------
Chd|        SEGVAR_MOD                    share/modules/segvar_mod.F    
Chd|====================================================================
      SUBROUTINE I12NOD3(NRT,ISEG,IRECT,SEGVAR,JCODV,
     .                   NODVAR,FCOUNT,NSN)
      USE SEGVAR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "conv_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRT,IRECT(4,*),JCODV(*),ISEG(*),FCOUNT(*),NSN
      my_real NODVAR(*)
      TYPE(t_segvar),TARGET :: SEGVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,KK,KVAR,NODAD,SEGAD
      my_real,DIMENSION(:),POINTER :: ptr
C-----------------------------------------------
      DO I=1,NSN*NVCONV
        NODVAR(I)=ZERO
      ENDDO


      DO KVAR=1,NVCONV
        DO I=1,NRT
          KK=ISEG(I)                     
          SELECT CASE(KVAR)              
            CASE(1)                      
              ptr(1:) => SEGVAR%RHO(1:)  
            CASE(2)                      
              ptr(1:) => SEGVAR%EINT(1:) 
            CASE(3)                      
              ptr(1:) => SEGVAR%RK(1:)   
            CASE(4)                      
              ptr(1:) => SEGVAR%RE(1:)   
            CASE(5)                      
              ptr(1:) => SEGVAR%UVAR(1:) 
          END SELECT                     
               
          DO J=1,4
            NODAD=NVCONV*(IRECT(J,I)-1)+KVAR
            NODVAR(NODAD)=NODVAR(NODAD)+ptr(KK)
          ENDDO
        ENDDO
      ENDDO

      DO I=1,NSN
        DO KVAR=1,NVCONV
          NODAD=NVCONV*(I-1)+KVAR
          NODVAR(NODAD)=NODVAR(NODAD)/FLOAT(ABS(FCOUNT(I)))
        ENDDO
      ENDDO

      RETURN
      END

