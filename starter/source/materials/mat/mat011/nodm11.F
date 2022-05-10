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
	SUBROUTINE NODM11(PM,IXS,IXQ,ICODE)
C     BLOCAGE DES NOEUDS MATERIAU 11 HORS DOMAINE CALCULE
C     FP 12/97
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),IXQ(NIXQ,*),ICODE(*)
C     REAL ou REAL*8
      my_real
     .   PM(NPROPM,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER, DIMENSION(NUMNOD) :: ITAG
      INTEGER I,J,IC,IC1,IC2,IC3,IC4
C=======================================================================
      DO I=1,NUMNOD
       ITAG(I) =0
      ENDDO
C TAGAGE MAT 11
       IF(N2D.EQ.0)THEN
        DO I=1,NUMELS
         IF(INT(PM(19,IXS(1,I))).EQ.11) THEN
          DO  J=2,9
           ITAG(IXS(J,I))=1
          ENDDO
         ENDIF
        ENDDO
       ELSE
        DO I=1,NUMELQ
         IF(INT(PM(19,IXQ(1,I))).EQ.11) THEN
          DO J=2,5
           ITAG(IXQ(J,I))=1
          ENDDO
         ENDIF
        ENDDO
       ENDIF
       IF(N2D.EQ.0)THEN
        DO I=1,NUMELS
         IF(INT(PM(19,IXS(1,I))).NE.11) THEN
          DO  J=2,9
           ITAG(IXS(J,I))=0
          ENDDO
         ENDIF
        ENDDO
       ELSE
        DO I=1,NUMELQ
         IF(INT(PM(19,IXQ(1,I))).NE.11) THEN
          DO J=2,5
           ITAG(IXQ(J,I))=0
          ENDDO
         ENDIF
        ENDDO
       ENDIF
C
       DO I=1,NUMNOD
        IF(ITAG(I).EQ.1)THEN
         IC=ICODE(I)
         IC1=IC/512
         IC2=(IC-512*IC1)/64
         IC3=(IC-512*IC1-64*IC2)/8
         IC4=IC-512*IC1-64*IC2-8*IC3
         IC1=7
         ICODE(I)=IC1*512+IC2*64+IC3*8+IC4
        ENDIF
       ENDDO
      RETURN
      END
