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
Chd|  C33STIF2EL                    source/elements/solid/solidez/szhour3.F
Chd|-- called by -----------
Chd|        MMODUL                        source/elements/solid/solidez/szhour3.F
Chd|-- calls ---------------
Chd|        CBATRAN3V                     source/elements/solid/solidez/szhour3.F
Chd|====================================================================
      SUBROUTINE C33STIF2EL(NEL   ,QC   ,QCG   ,QGC    ,QG     ,
     .                      C33   ,CC   ,G33   ,CG     ) 
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
      INTEGER NEL  
C     REAL
      my_real
     .   QC(MVSIZ,3,3),QCG(MVSIZ,3,3),QGC(MVSIZ,3,3),QG(MVSIZ,3,3),
     .   G3(MVSIZ,3),C33(MVSIZ,3),CC(MVSIZ,3,3),CG(MVSIZ,3,3),G33(MVSIZ,3,3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,ISYM
C     REAL
      my_real
     .   B(MVSIZ,3,3)
C-----------------------------------------------
       ISYM = 0
C      
       DO I=1,NEL
        B(I,1:3,1:3)=ZERO
       ENDDO
       DO J= 1,3
        DO I=1,NEL
          B(I,J,J)=C33(I,J)
        ENDDO
       ENDDO
      CALL CBATRAN3V(1,NEL    ,QC   ,B ,QCG,ISYM) 
C      
       DO J= 1,3
       DO K= 1,3
        DO I=1,NEL
          CG(I,J,K)=HALF*B(I,J,K)
        ENDDO
       ENDDO
       ENDDO
C---------------[G']->[G33]----
       ISYM = 1
       DO I=1,NEL
        B(I,1:3,1:3)=ZERO
       ENDDO
       DO J= 1,3
        DO I=1,NEL
          B(I,J,J)=C33(I,J)
        ENDDO
       ENDDO
       CALL CBATRAN3V(1,NEL    ,QCG   ,B ,QCG,ISYM) 
C      
       DO J= 1,3
       DO K= J,3
        DO I=1,NEL
          G33(I,J,K)=FOURTH*B(I,J,K)
        ENDDO
       ENDDO
       ENDDO
C---------------[C']->[CC]----
       DO I=1,NEL
        B(I,1:3,1:3)=ZERO
       ENDDO
       DO J= 1,3
        DO I=1,NEL
          B(I,J,J)=C33(I,J)
        ENDDO
       ENDDO
       CALL CBATRAN3V(1,NEL   ,QC   ,B ,QC,ISYM) 
C      
       DO J= 1,3
       DO K= J,3
        DO I=1,NEL
          CC(I,J,K)=B(I,J,K)
        ENDDO
       ENDDO
       ENDDO
C       
       DO J= 1,3
       DO K= J+1,3
        DO I=1,NEL
          CC(I,K,J)=CC(I,J,K)
          G33(I,K,J)=G33(I,J,K)
        ENDDO
       ENDDO
       ENDDO
C      
      RETURN
      END
