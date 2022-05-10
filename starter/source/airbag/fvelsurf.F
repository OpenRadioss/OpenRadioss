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
Chd|  FVELSURF                      source/airbag/fvelsurf.F      
Chd|-- called by -----------
Chd|        FVBAG_VERTEX                  source/spmd/domain_decomposition/grid2mat.F
Chd|        FVMESH0                       source/airbag/fvmesh0.F       
Chd|        INIT_MONVOL                   source/airbag/init_monvol.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FVELSURF(IBUF, ELEM, ELEM_ID, IXC, IXTG, NEL,
     .     ELTG, MATTG, NB_NODE, FLAG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXC(NIXC,*), IXTG(NIXTG,*)
      INTEGER IBUF(*), ELEM(3,*), ELEM_ID(*)
      INTEGER, DIMENSION(NEL), INTENT(INOUT) :: ELTG, MATTG
      INTEGER NEL
      INTEGER NB_NODE
      LOGICAL :: FLAG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, ITAG(NB_NODE), J, JJ, ICMAX, NC, I1, I2, I3, IFOUND
      INTEGER K, KK, ITY
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: CNS
C-------------------------------------------------------------------
C SEARCH SHELL ELEMENT TO WHICH BAG TRIANGLE BELONGS
C-------------------------------------------------------------------
      DO I=1,NB_NODE
         ITAG(I)=0
      ENDDO
      DO I=1,NUMELC
         DO J=1,4
            JJ=IXC(1+J,I)
            ITAG(JJ)=ITAG(JJ)+1
         ENDDO
      ENDDO
      DO I=1,NUMELTG
         DO J=1,3
            JJ=IXTG(1+J,I)
            ITAG(JJ)=ITAG(JJ)+1
         ENDDO
      ENDDO
      ICMAX=0
      DO I=1,NB_NODE
         ICMAX=MAX(ICMAX,ITAG(I))
      ENDDO
C
      ALLOCATE(CNS(NB_NODE,1+ICMAX*2))
      DO I=1,NB_NODE
         CNS(I,1)=0
      ENDDO
      DO I=1,NUMELC
         DO J=1,4
            JJ=IXC(1+J,I)
            NC=CNS(JJ,1)
            NC=NC+1
            CNS(JJ,1)=NC
            CNS(JJ,1+2*(NC-1)+1)=1
            CNS(JJ,1+2*(NC-1)+2)=I
         ENDDO
      ENDDO   
      DO I=1,NUMELTG
         DO J=1,3
            JJ=IXTG(1+J,I)
            NC=CNS(JJ,1)
            NC=NC+1
            CNS(JJ,1)=NC
            CNS(JJ,1+2*(NC-1)+1)=2
            CNS(JJ,1+2*(NC-1)+2)=I
         ENDDO
      ENDDO   
C
      DO I=1,NB_NODE
         ITAG(I)=ZERO
      ENDDO
      DO I=1,NEL
         I1=ELEM(1,I)
         I2=ELEM(2,I)
         I3=ELEM(3,I)
         I1=IBUF(I1)
         I2=IBUF(I2)
         I3=IBUF(I3)
         IFOUND=0
         DO J=1,CNS(I1,1)
            ITY=CNS(I1,1+2*(J-1)+1)
            JJ=CNS(I1,1+2*(J-1)+2)
            IF (ITY.EQ.1) THEN
               DO K=1,4
                  KK=IXC(1+K,JJ)
                  ITAG(KK)=1
               ENDDO
               IF (ITAG(I1).EQ.1.AND.ITAG(I2).EQ.1.AND.ITAG(I3).EQ.1) THEN
                  IF (.NOT. FLAG) IFOUND=NUMELQ+JJ
                  IF (FLAG .AND. (JJ .EQ. ELEM_ID(I))) THEN
                     IFOUND=NUMELQ+JJ
                  ENDIF
               ENDIF
               DO K=1,4
                  KK=IXC(1+K,JJ)
                  ITAG(KK)=0
               ENDDO
            ELSEIF (ITY.EQ.2) THEN
               DO K=1,3
                  KK=IXTG(1+K,JJ)
                  ITAG(KK)=1
               ENDDO
               IF (ITAG(I1).EQ.1.AND.ITAG(I2).EQ.1.AND.ITAG(I3).EQ.1) THEN
                  IF (.NOT. FLAG) IFOUND=NUMELC+JJ
                  IF (FLAG .AND. (JJ .EQ. ELEM_ID(I))) THEN
                     IFOUND=NUMELQ+NUMELC+JJ
                  ENDIF
               ENDIF
               DO K=1,3
                  KK=IXTG(1+K,JJ)
                  ITAG(KK)=0
               ENDDO
            ENDIF
         ENDDO
         IF (IFOUND.NE.0) GOTO 100
         DO J=1,CNS(I2,1)
            ITY=CNS(I2,1+2*(J-1)+1)
            JJ=CNS(I2,1+2*(J-1)+2)
            IF (ITY.EQ.1) THEN
               DO K=1,4
                  KK=IXC(1+K,JJ)
                  ITAG(KK)=1
               ENDDO
               IF (ITAG(I1).EQ.1.AND.ITAG(I2).EQ.1.AND.ITAG(I3).EQ.1) THEN
                  IF (.NOT. FLAG) IFOUND=NUMELQ+JJ
                  IF (FLAG .AND. (JJ .EQ. ELEM_ID(I))) THEN
                     IFOUND=NUMELQ+JJ
                  ENDIF
               ENDIF
               DO K=1,4
                  KK=IXC(1+K,JJ)
                  ITAG(KK)=0
               ENDDO
            ELSEIF (ITY.EQ.2) THEN
               DO K=1,3
                  KK=IXTG(1+K,JJ)
                  ITAG(KK)=1
               ENDDO
               IF (ITAG(I1).EQ.1.AND.ITAG(I2).EQ.1.AND.ITAG(I3).EQ.1) THEN
                  IF (.NOT. FLAG) IFOUND=NUMELC+JJ
                  IF (FLAG .AND. JJ .EQ. ELEM_ID(I)) THEN
                     IFOUND=NUMELQ+NUMELC+JJ
                  ENDIF
               ENDIF
               DO K=1,3
                  KK=IXTG(1+K,JJ)
                  ITAG(KK)=0
               ENDDO
            ENDIF
         ENDDO
         IF (IFOUND.NE.0) GOTO 100
         DO J=1,CNS(I3,1)
            ITY=CNS(I3,1+2*(J-1)+1)
            JJ=CNS(I3,1+2*(J-1)+2)
            IF (ITY.EQ.1) THEN
               DO K=1,4
                  KK=IXC(1+K,JJ)
                  ITAG(KK)=1
               ENDDO
               IF (ITAG(I1).EQ.1.AND.ITAG(I2).EQ.1.AND.ITAG(I3).EQ.1) THEN
                  IF (.NOT. FLAG) IFOUND=NUMELQ+JJ
                  IF (FLAG .AND. (JJ .EQ. ELEM_ID(I))) THEN
                     IFOUND=NUMELQ+JJ
                  ENDIF
               ENDIF
               DO K=1,4
                  KK=IXC(1+K,JJ)
                  ITAG(KK)=0
               ENDDO
            ELSEIF (ITY.EQ.2) THEN
               DO K=1,3
                  KK=IXTG(1+K,JJ)
                  ITAG(KK)=1
               ENDDO
               IF (ITAG(I1).EQ.1.AND.ITAG(I2).EQ.1.AND.ITAG(I3).EQ.1) THEN
                  IF (.NOT. FLAG) IFOUND=NUMELC+JJ
                  IF (FLAG .AND. (JJ .EQ. ELEM_ID(I))) THEN
                     IFOUND=NUMELQ+NUMELC+JJ
                  ENDIF
               ENDIF
               DO K=1,3
                  KK=IXTG(1+K,JJ)
                  ITAG(KK)=0
               ENDDO
            ENDIF
         ENDDO
C
  100    CONTINUE
         ELTG(I)=IFOUND
      ENDDO
C
      DEALLOCATE(CNS)
C-----------------------
C STORE MATERIAL NUMBER
C-----------------------
      DO I=1,NEL
         J=ELTG(I)
         IF (J.LE.NUMELC) THEN
           MATTG(I) =IXC(1,J)
         ELSEIF (J.GT.NUMELC) THEN
           MATTG(I) =IXTG(1,J-NUMELC)
         ENDIF
      ENDDO
C     
      RETURN
      END
