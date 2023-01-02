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
Chd|  FEQUILIBRE                    source/elements/shell/fequilibre.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FEQUILIBRE(A,FZERO,IXC,IXTG)
C------------------------------------------------------
C   I m p l i c i t   T y p e s
C------------------------------------------------------
#include      "implicit_f.inc"
C------------------------------------------------------
C   C o m m o n   B l o c k s
C------------------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C------------------------------------------------------
C   D u m m y   A r g u m e n t s
C------------------------------------------------------
      INTEGER  IXC(NIXC,*),IXTG(NIXTG,*)
C     REAL
      my_real
     .         A(3,*),FZERO(3,4,*)
C------------------------------------------------------
C   L o c a l   V a r i a b l e s
C------------------------------------------------------
      INTEGER  I,J,K,L, WA(NUMNOD)
C------------------------------------------------------
C   S o u r c e   L i n e s
C------------------------------------------------------
      DO I=1,NUMNOD
        WA(I)=0
      ENDDO
C------------------------------------------------------
C   I m p l i c a t i o n   d e s   n o e u d s
C------------------------------------------------------
      DO J=1,4
        DO I=1,NUMELC
          K=IXC(J+1,I)
          WA(K)=WA(K)+1
        ENDDO
      ENDDO
      DO J=1,3
        DO I=1,NUMELTG
          K=IXTG(J+1,I)
          WA(K)=WA(K)+1
        ENDDO
      ENDDO
C------------------------------------------------------
C   M i s e   a   0   d e   F Z E R O
C------------------------------------------------------
      DO J=1,4
        DO L=1,3
          DO I=1,(NUMELC+NUMELTG)
            FZERO(L,J,I) = ZERO
          ENDDO
        ENDDO
      ENDDO
C------------------------------------------------------
C   S h e l l   4 N
C------------------------------------------------------
      DO J=1,4
        DO L=1,3
          DO I=1,NUMELC 
            K=IXC(J+1,I)
            FZERO(L,J,I)=A(L,K)/WA(K)
          ENDDO
        ENDDO
      ENDDO
C------------------------------------------------------
C   S h e l l   3 N
C------------------------------------------------------
      DO J=1,3
        DO L=1,3
          DO I=1,NUMELTG
            K=IXTG(J+1,I)
            FZERO(L,J,(I+NUMELC))=A(L,K)/WA(K)
          ENDDO
        ENDDO
      ENDDO
C------------------------------------------------------
C   M i s e   a   0   d e s   a c c e l e r a t i o n s
C------------------------------------------------------
      DO J=1,4
        DO L=1,3
          DO I=1,NUMELC 
            K=IXC(J+1,I)
            A(L,K) = ZERO
          ENDDO
        ENDDO
      ENDDO
C
      DO J=1,3
        DO L=1,3
          DO I=1,NUMELTG
            K=IXTG(J+1,I)
            A(L,K) = ZERO
          ENDDO
        ENDDO
      ENDDO
C
      RETURN
      END            
