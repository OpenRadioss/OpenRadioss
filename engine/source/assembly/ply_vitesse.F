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
Chd|  PLY_VITESSE                   source/assembly/ply_vitesse.F 
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        PLYV                          source/assembly/ply_vitesse.F 
Chd|        PLYXFEM_MOD                   share/modules/plyxfem_mod.F   
Chd|====================================================================
      SUBROUTINE PLY_VITESSE(NODFT,NODLT,NPLYMAX,INOD,NDDIM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE PLYXFEM_MOD
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODFT,NODLT,NDDIM, NPLYMAX,INOD(*)
C     REAL
cc        TYPE(LAYER_DATA), DIMENSION(:), ALLOCATABLE :: PLY 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J,K,N,NN
      my_real
     .   VDT
C
       DO J=1,NPLYMAX                                                 
          CALL PLYV(NODFT,NODLT, INOD, PLY(J)%A(1,1),
     .                                 PLY(J)%V(1,1),PLY(J)%U(1,1))  
       ENDDO                                                          
C
      RETURN
      END
Chd|====================================================================
Chd|  PLYV                          source/assembly/ply_vitesse.F 
Chd|-- called by -----------
Chd|        PLY_VITESSE                   source/assembly/ply_vitesse.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PLYV(NODFT,NODLT,INOD,A,V,U)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODFT,NODLT,NDDIM,INOD(*)
C     REAL
      my_real
     .  A(4,*), V(3,*),U(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J,K,N,NN
      my_real
     .   VDT
C
#include   "vectorize.inc"
            DO NN=NODFT,NODLT
              N = INOD(NN)
              IF(N > 0) THEN
               V(1,N)= V(1,N) + DT12*A(1,N)
               V(2,N)= V(2,N) + DT12*A(2,N)
               V(3,N)= V(3,N) + DT12*A(3,N)  
C  displacement
               U(1,N)= U(1,N) + DT2*V(1,N)
               U(2,N)= U(2,N) + DT2*V(2,N)
               U(3,N)= U(3,N) + DT2*V(3,N)
               A(1,N)=ZERO
               A(2,N)=ZERO
               A(3,N)=ZERO
               A(4,N)=ZERO
              ENDIF
            ENDDO                    
C
      RETURN
      END

