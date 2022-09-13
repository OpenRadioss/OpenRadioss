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
Chd|  EBCCLAP                       source/boundary_conditions/ebcs/ebcclap.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        EBCCLAP1                      source/boundary_conditions/ebcs/ebcclap.F
Chd|        EBCVIT4                       source/boundary_conditions/ebcs/ebcvit4.F
Chd|        EBCVIT5                       source/boundary_conditions/ebcs/ebcvit5.F
Chd|        EBCVIT7                       source/boundary_conditions/ebcs/ebcvit7.F
Chd|        EBCS_MOD                      ../common_source/modules/ebcs_mod.F
Chd|====================================================================
      SUBROUTINE EBCCLAP(V, A, FV, EBCS_TAB)
      USE EBCS_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
c#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .       V(*),A(*),FV(*)
      TYPE(t_ebcs_tab), TARGET, INTENT(INOUT) :: EBCS_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,TYP,
     .        NOD,J
      CLASS(t_ebcs), POINTER :: EBCS

      DO I=1,NEBCS
         TYP = EBCS_TAB%tab(i)%poly%type
         NOD = EBCS_TAB%tab(i)%poly%nb_node
         
         IF(TYP == 2 .OR. TYP == 3)THEN
            EBCS => EBCS_TAB%tab(i)%poly
            IF(TYP == 2) CALL EBCCLAP1(V, A, EBCS%node_list, NOD, EBCS%la, -ONE)
            IF(TYP == 3) CALL EBCCLAP1(V, A, EBCS%node_list, NOD, EBCS%la, ONE)
         ELSE IF(TYP == 4)THEN
            select type (twf => EBCS_TAB%tab(i)%poly)
            type is(t_ebcs_vel)
            CALL EBCVIT4(V, A, twf%node_list, NOD, FV, twf%reso,twf%la, twf)
            end select
         ELSE IF (TYP == 5)THEN
            select type (twf => EBCS_TAB%tab(i)%poly)
            type is(t_ebcs_normv)
            CALL EBCVIT5(V, A, twf%node_list, NOD, FV, twf%reso, twf%la, twf )
            end select
         ELSE IF (TYP == 7)THEN
            select type (twf => EBCS_TAB%tab(i)%poly)
            type is(t_ebcs_iniv)
            CALL EBCVIT7(V, A, twf%node_list, NOD, twf%v0, twf%reso,twf)
            end select
         ENDIF
C
      ENDDO
      RETURN
      END
Chd|====================================================================
Chd|  EBCCLAP1                      source/boundary_conditions/ebcs/ebcclap.F
Chd|-- called by -----------
Chd|        EBCCLAP                       source/boundary_conditions/ebcs/ebcclap.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE EBCCLAP1(V,A,LISTE,NOD,NORM,FAC)
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
     .       V(3,*),A(3,*),NORM(3,*),FAC,VX,VY,VZ
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N
      my_real
     .     VN  
C
      DO I=1,NOD
        N=LISTE(I)
c        if(FAC>0.)write(6,*)'ebcclap',I,N,V(1,N)
        VX=V(1,N)+A(1,N)*DT12
        VY=V(2,N)+A(2,N)*DT12
        VZ=V(3,N)+A(3,N)*DT12
        VN=FAC*(VX*NORM(1,I)+VY*NORM(2,I)+VZ*NORM(3,I))
        IF(VN<0)THEN
          V(1,N)=ZERO
          V(2,N)=ZERO
          V(3,N)=ZERO
          A(1,N)=ZERO
          A(2,N)=ZERO
          A(3,N)=ZERO
        ENDIF
c      write(6,*)'apres',I,N,V(1,N)
      ENDDO
      RETURN
      END       
