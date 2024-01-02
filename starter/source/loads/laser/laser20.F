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
Chd|  LASER20                       source/loads/laser/laser20.F  
Chd|-- called by -----------
Chd|        LASER10                       source/loads/laser/laser10.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE LASER20(NL ,NS1 ,NS2 ,LAS ,XLAS ,X,IXQ,IPARG )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NL, NS1, NS2 
      my_real
     .   XLAS(SXLAS),X(3,NUMNOD)
      INTEGER IPARG(NPARG,NGROUP),LAS(2,SILAS/2),IXQ(7,NUMELQ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IL, NG, ITY, NFT, NEL, I, NE1, NE2, NFOUND, N1, J, N2 , II, NEXT
      LOGICAL lFOUND
C--------------------------------------------------
C RECHERCHE DU GROUPE ET  DU NUMERO DANS LE GROUPE
C--------------------------------------------------
      DO IL=1,NL+1
        NEXT = LAS(1,IL)
        lFOUND = .FALSE.
        DO NG=1,NGROUP
         ITY=IPARG(5,NG)
         IF(ITY/=2) CYCLE !NG
         NFT=IPARG(3,NG)
         NEL=IPARG(2,NG)
         DO I=1,NEL
          II = I + NFT
          IF(IXQ(7,II) /= NEXT) CYCLE !I
           LAS(1,IL) = NG
           LAS(2,IL) = I
           lFOUND = .TRUE.
           EXIT
         END DO !I=1,NEL
         IF(lFOUND)EXIT
        END DO !NG=1,NGROUP
      END DO !IL=1,NL+1
C
C
C---------------------------------
C RECHERCHE DES NOEUDS DE SURFACE 
C---------------------------------
      NE1  = LAS(2,NL) + IPARG(3,LAS(1,NL))
      NE2  = LAS(2,NL+1) + IPARG(3,LAS(1,NL+1))
      NFOUND = 0
      DO I=1,4
        N1 = IXQ(I+1,NE1)
        DO J=1,4
          N2 = IXQ(J+1,NE2)
          IF(N1==N2)THEN
            IF(NFOUND==0)THEN
              NS1 = N1
              NFOUND = 1
              EXIT !J
            ELSEIF(NFOUND==1)THEN
              NS2 = N1
              GOTO 160
            ENDIF
          ENDIF
        END DO !J=1,4
      END DO !I=1,4
C
 160  CONTINUE
 
      RETURN
      END
