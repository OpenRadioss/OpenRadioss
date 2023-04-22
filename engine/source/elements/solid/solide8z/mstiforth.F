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
Chd|  MSTIFORTH                     source/elements/solid/solide8z/mstiforth.F
Chd|-- called by -----------
Chd|        MMATS                         source/elements/solid/solide8z/mmats.F
Chd|-- calls ---------------
Chd|        CBATRAN3                      source/elements/shell/coqueba/cbasumg3.F
Chd|====================================================================
      SUBROUTINE MSTIFORTH(JFT    ,JLT   ,QC  ,QCG  ,QGC    ,
     .                     QG     ,CC    ,G3  ,G33  ,CG     ) 
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
      INTEGER JFT, JLT  
C     REAL
      my_real
     .   QC(3,3,*),QCG(3,3,*),QGC(3,3,*),QG(3,3,*),
     .   G3(3,*),CC(3,3,*),CG(3,3,*),G33(3,3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,ISYM
C     REAL
      my_real
     .   A(3,3,MVSIZ),B(3,3,MVSIZ)
C-----------------------------------------------
       ISYM = 0
       DO J= 1,3
       DO K= J,3
        DO I=JFT,JLT
          A(J,K,I)=CC(J,K,I)
          B(J,K,I)=ZERO
        ENDDO
       ENDDO
       ENDDO
C      
       DO J= 1,3
       DO K= J+1,3
        DO I=JFT,JLT
          A(K,J,I)=CC(J,K,I)
          B(K,J,I)=ZERO
        ENDDO
       ENDDO
       ENDDO
C      
       DO J= 1,3
        DO I=JFT,JLT
          B(J,J,I)=G3(J,I)
        ENDDO
       ENDDO
      CALL CBATRAN3(JFT   ,JLT    ,QC   ,A ,QCG,ISYM) 
      CALL CBATRAN3(JFT   ,JLT    ,QGC  ,B ,QG ,ISYM) 
C      
       DO J= 1,3
       DO K= 1,3
        DO I=JFT,JLT
          CG(J,K,I)=HALF*A(J,K,I)+TWO*B(J,K,I)
        ENDDO
       ENDDO
       ENDDO
C---------------[G']->[G33]----
       ISYM = 1
       DO J= 1,3
       DO K= J,3
        DO I=JFT,JLT
          A(J,K,I)=CC(J,K,I)
          B(J,K,I)=ZERO
        ENDDO
       ENDDO
       ENDDO
C      
       DO J= 1,3
        DO I=JFT,JLT
          B(J,J,I)=G3(J,I)
        ENDDO
       ENDDO
      CALL CBATRAN3(JFT   ,JLT    ,QCG  ,A ,QCG,ISYM) 
      CALL CBATRAN3(JFT   ,JLT    ,QG   ,B ,QG ,ISYM) 
C      
       DO J= 1,3
       DO K= J,3
        DO I=JFT,JLT
          G33(J,K,I)=FOURTH*A(J,K,I)+B(J,K,I)
        ENDDO
       ENDDO
       ENDDO
C---------------[C']->[CC]----
       DO J= 1,3
       DO K= J,3
        DO I=JFT,JLT
          A(J,K,I)=CC(J,K,I)
          B(J,K,I)=ZERO
        ENDDO
       ENDDO
       ENDDO
C      
       DO J= 1,3
        DO I=JFT,JLT
          B(J,J,I)=G3(J,I)
        ENDDO
       ENDDO
      CALL CBATRAN3(JFT   ,JLT    ,QC   ,A ,QC ,ISYM) 
      CALL CBATRAN3(JFT   ,JLT    ,QGC  ,B ,QGC,ISYM) 
C      
       DO J= 1,3
       DO K= J,3
        DO I=JFT,JLT
          CC(J,K,I)=A(J,K,I)+FOUR*B(J,K,I)
        ENDDO
       ENDDO
       ENDDO
C       
       DO J= 1,3
       DO K= J+1,3
        DO I=JFT,JLT
          CC(K,J,I)=CC(J,K,I)
          G33(K,J,I)=G33(J,K,I)
        ENDDO
       ENDDO
       ENDDO
C      
      RETURN
      END
