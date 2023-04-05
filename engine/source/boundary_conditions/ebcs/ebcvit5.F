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
Chd|  EBCVIT5                       source/boundary_conditions/ebcs/ebcvit5.F
Chd|-- called by -----------
Chd|        EBCCLAP                       source/boundary_conditions/ebcs/ebcclap.F
Chd|-- calls ---------------
Chd|        EBCS_MOD                      ../common_source/modules/boundary_conditions/ebcs_mod.F
Chd|====================================================================
      SUBROUTINE EBCVIT5(V, A, LISTE, NOD, FV, RESO, LA, EBCS)
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
     .       V(3,*),A(3,*),LA(3,NOD),RESO(3,NOD),FV(*)
      TYPE(t_ebcs_normv), INTENT(IN) :: EBCS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,IVIMP
      my_real
     .     C,LCAR,ALP,ALPDT,AX,AY,AZ,VIMP,V0(3),FAC
C
      IVIMP=EBCS%ivimp
      IF(IVIMP>0)THEN
        VIMP=EBCS%vimp*FV(IVIMP)
      ELSE
        VIMP=EBCS%vimp
      ENDIF
      C=EBCS%c
      LCAR=EBCS%lcar
      ALP=ZERO
      IF(LCAR>ZERO)ALP=C/LCAR
      ALPDT=ALP*DT1
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
      IF(ALP>ZERO)THEN
        DO I=1,NOD
          N=LISTE(I)
c          write(6,*)'ebcvit5 N',N,' RESO',RESO(3,I),'A',A(3,N)
          AX=RESO(1,I)+ALPDT*(A(1,N)-RESO(1,I))
          AY=RESO(2,I)+ALPDT*(A(2,N)-RESO(2,I))
          AZ=RESO(3,I)+ALPDT*(A(3,N)-RESO(3,I))
C memorisation de la reaction
          RESO(1,I)=AX
          RESO(2,I)=AY
          RESO(3,I)=AZ
C
          FAC=VIMP/SQRT(LA(1,I)**2+LA(2,I)**2+LA(3,I)**2)
          V0(1)=FAC*LA(1,I)
          V0(2)=FAC*LA(2,I)
          V0(3)=FAC*LA(3,I)
C
          A(1,N)=A(1,N)-AX+ALP*(V0(1)-V(1,N))
          A(2,N)=A(2,N)-AY+ALP*(V0(2)-V(2,N))
          A(3,N)=A(3,N)-AZ+ALP*(V0(3)-V(3,N))
        ENDDO
      ELSE
        DO I=1,NOD
          N=LISTE(I)
          FAC=VIMP/SQRT(LA(1,I)**2+LA(2,I)**2+LA(3,I)**2)
          V0(1)=FAC*LA(1,I)
          V0(2)=FAC*LA(2,I)
          V0(3)=FAC*LA(3,I)
          A(1,N)=ZERO
          A(2,N)=ZERO
          A(3,N)=ZERO
          V(1,N)=V0(1)
          V(2,N)=V0(2)
          V(3,N)=V0(3)
        ENDDO      
      ENDIF
      RETURN
      END       
