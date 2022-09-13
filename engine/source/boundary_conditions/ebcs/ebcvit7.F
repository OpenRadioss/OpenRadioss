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
Chd|  EBCVIT7                       source/boundary_conditions/ebcs/ebcvit7.F
Chd|-- called by -----------
Chd|        EBCCLAP                       source/boundary_conditions/ebcs/ebcclap.F
Chd|-- calls ---------------
Chd|        EBCS_MOD                      ../common_source/modules/ebcs_mod.F
Chd|====================================================================
      SUBROUTINE EBCVIT7(V, A, LISTE, NOD, V0, RESO, EBCS)
      USE EBCS_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LISTE(*),NOD
      my_real
     .       V(3,*),A(3,*),V0(3,NOD),RESO(3,NOD)
      TYPE(t_ebcs_iniv), INTENT(IN) :: EBCS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N
      my_real
     .     VN,LCAR,ALP,ALPDT,AX,AY,AZ,C
C
      IF(TT==ZERO)THEN
          DO I=1,NOD
            N=LISTE(I)
            RESO(1,I)=A(1,N)
            RESO(2,I)=A(2,N)
            RESO(3,I)=A(3,N)
          ENDDO
      ENDIF
C
      C=EBCS%c
      LCAR=EBCS%lcar
      ALP=ZERO
      IF(LCAR>ZERO)ALP=C/LCAR
      ALPDT=ALP*DT1
C
      IF(ALP>ZERO)THEN
        DO I=1,NOD
          N=LISTE(I)
          AX=RESO(1,I)+ALPDT*(A(1,N)-RESO(1,I))
          AY=RESO(2,I)+ALPDT*(A(2,N)-RESO(2,I))
          AZ=RESO(3,I)+ALPDT*(A(3,N)-RESO(3,I))
C memorisation de la reaction
          RESO(1,I)=AX
          RESO(2,I)=AY
          RESO(3,I)=AZ
c          write(6,*)i,'R',A(3,N),'A',AZ,'av',
c       .     ALP*(V0(3,I)-V(3,N)),'V0',V0(3,I),'V',V(3,N)
c          A(1,N)=A(1,N)-AX
c          A(2,N)=A(2,N)-AY
c          A(3,N)=A(3,N)-AZ
c          AX=0
c          AY=0
c          AZ=0
          A(1,N)=A(1,N)-AX+ALP*(V0(1,I)-V(1,N))
          A(2,N)=A(2,N)-AY+ALP*(V0(2,I)-V(2,N))
          A(3,N)=A(3,N)-AZ+ALP*(V0(3,I)-V(3,N))
        ENDDO
      ELSE
        DO I=1,NOD
          N=LISTE(I)
          A(1,N)=ZERO
          A(2,N)=ZERO
          A(3,N)=ZERO
          V(1,N)=V0(1,I)
          V(2,N)=V0(2,I)
          V(3,N)=V0(3,I)
        ENDDO      
      ENDIF
      RETURN
      END       
