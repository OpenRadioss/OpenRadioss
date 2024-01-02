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
Chd|  ROTOS4                        source/materials/mat_share/rotos4.F
Chd|-- called by -----------
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ROTOS4(JFT,JLT,SIGXX,SIGYY,SIGXY,SIGYX,DIR,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: JFT, JLT,NEL
      my_real,INTENT(INOUT) :: SIGXX(NEL),SIGYY(NEL),SIGXY(NEL),SIGYX(NEL)
      my_real,INTENT(IN) :: DIR(NEL,2)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real S1, S2, S3, S4,S5
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------
      DO I=JFT,JLT
         S1 =    DIR(I,1)*DIR(I,1)*SIGXX(I) + DIR(I,2)*DIR(I,2)*SIGYY(I) + DIR(I,1)*DIR(I,2)*(SIGXY(I)+SIGYX(I))
         S2 =    DIR(I,2)*DIR(I,2)*SIGXX(I) + DIR(I,1)*DIR(I,1)*SIGYY(I) - DIR(I,2)*DIR(I,1)*(SIGXY(I)+SIGYX(I))
         S5 =   -DIR(I,1)*DIR(I,2)*SIGXX(I) + DIR(I,2)*DIR(I,1)*SIGYY(I)
         S3 = S5+DIR(I,1)*DIR(I,1)*SIGXY(I) - DIR(I,2)*DIR(I,2)*SIGYX(I)
         S4 = S5+DIR(I,1)*DIR(I,1)*SIGYX(I) - DIR(I,2)*DIR(I,2)*SIGXY(I)
         SIGXX(I)=S1
         SIGYY(I)=S2
         SIGXY(I)=S3
         SIGYX(I)=S4
      ENDDO
c-----------
      RETURN
      END
