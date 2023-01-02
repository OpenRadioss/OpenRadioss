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
Chd|  M_FLOW                        source/restart/ddsplit/m_flow.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE M_FLOW(IFLOW, IFLOW_L, NODLOCAL, P, MEMFLOW)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "flowcom.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IFLOW(*), IFLOW_L(*), NODLOCAL(*), P
      INTEGER (KIND=8) :: MEMFLOW(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IADI, IADJ, NINOUT, NNO, NEL, NNN, IADL, NNO_L, J,
     .        JJ, J1, J2, J3, PJ1, PJ2, PJ3, IADMATI, IADMATR, ITYP,
     .        NR, NC, NPROW, NPCOL, NNR_P, NNC_P, NNE_P, NBLOC,
     .        NNN_L
      INTEGER JFORM
C
      DO I=1,LIFLOW
         IFLOW_L(I)=IFLOW(I)
      ENDDO
C
      IADI=0
      IADMATI=1
      IADMATR=1
      DO I=1,NFLOW
         ITYP =IFLOW(IADI+2)
         NNO  =IFLOW(IADI+5)
         NEL  =IFLOW(IADI+6)
         NBLOC=IFLOW(IADI+12)
         NPROW=IFLOW(IADI+18)
         NPCOL=IFLOW(IADI+19)
		 
         IF (ITYP==1) THEN
            NINOUT=IFLOW(IADI+4)
            NNN=IFLOW(IADI+7)
C
            IADJ=IADI+NIFLOW+NNO+3*NEL+NINOUT*NIIOFLOW
            IADL=IADJ+NNN+NEL
            NNO_L=0
            DO J=1,NNO
               JJ=IFLOW(IADI+NIFLOW+J)
               IFLOW_L(IADI+NIFLOW+J)=NODLOCAL(JJ)   ! IBUF
               IF (NODLOCAL(JJ)/=0) THEN
                  NNO_L=NNO_L+1
                  IF(NSPMD == 1) THEN
                    IFLOW_L(IADL+NNO_L)=J  ! IBUFL
                  ELSE
                    IFLOW_L(IADL+2*NNO+NNO_L)=J  
                  ENDIF
               ENDIF
            ENDDO
            IFLOW_L(IADI+16)=NNO_L
            NNN_L=0
            DO J=1,NNN
               JJ=IFLOW(IADJ+J)
               IFLOW_L(IADJ+J)=NODLOCAL(JJ) ! IBUFI
               IF (NODLOCAL(JJ)>0) THEN
                  NNN_L=NNN_L+1
                  IF(NSPMD == 1) THEN
                    IFLOW_L(IADL+NNO+NNN_L)=J  ! IBUFIL
                  ELSE
                    IFLOW_L(IADL+4*NNO+NNN_L)=J  
                  ENDIF
               ENDIF
            ENDDO
            IFLOW_L(IADI+22)=NNN_L
C
            IF(NSPMD > 1) THEN
              NR=(P-1)/NPCOL+1
              NC=P-(NR-1)*NPCOL
              NNR_P=0
              DO J=1,NNO
                 IF (IFLOW(IADL+J)==NR) THEN
                    NNR_P=NNR_P+1
                    IFLOW_L(IADL+J)=1   ! IBUFR
                 ELSE
                    IFLOW_L(IADL+J)=0
                 ENDIF
              ENDDO
              NNC_P=0
              DO J=1,NNO
                 IF (IFLOW(IADL+NNO+J)==NC) THEN
                    NNC_P=NNC_P+1
                    IFLOW_L(IADL+NNO+J)=1  ! IBUFC
                 ELSE
                    IFLOW_L(IADL+NNO+J)=0
                 ENDIF
              ENDDO
              IADL=IADL+4*NNO+2*NNN
              DO J=1,NEL
                 IF (IFLOW(IADL+J)==NR) THEN
                    IFLOW_L(IADL+J)=1   ! IBUFELR
                 ELSE
                    IFLOW_L(IADL+J)=0
                 ENDIF
              ENDDO
              NNE_P=0
              DO J=1,NEL
                 IF (IFLOW(IADL+NEL+J)==NC) THEN
                    NNE_P=NNE_P+1
                    IFLOW_L(IADL+NEL+J)=1   ! IBUFELC
                 ELSE
                    IFLOW_L(IADL+NEL+J)=0
                 ENDIF
              ENDDO
C
              IFLOW_L(IADI+8)=NNR_P+NBLOC
              IFLOW_L(IADI+9)=NNR_P*NNC_P+NNR_P*NNE_P
              IFLOW_L(IADI+10)=IADMATI
              IFLOW_L(IADI+11)=IADMATR
              IFLOW_L(IADI+20)=IADMATR+NNR_P*NNC_P
C
              MEMFLOW(1)=MEMFLOW(1)+IFLOW_L(IADI+8)
              MEMFLOW(2)=MEMFLOW(2)+IFLOW_L(IADI+9)
C
              IADMATI=IADMATI+NNR_P+NBLOC
              IADMATR=IADMATR+NNR_P*NNC_P+NNR_P*NNE_P
            ENDIF

         ELSEIF(ITYP == 3) THEN
            JFORM =IFLOW(IADI+4)
            IF(JFORM == 1) IADL=IADI+NIFLOW+NNO+3*NEL
            IF(JFORM == 2) IADL=IADI+NIFLOW+NNO+5*NEL
            NNO_L=0
            DO J=1,NNO
               IFLOW_L(IADL+J)=0  ! IBUFL
            ENDDO
            DO J=1,NNO
               JJ=IFLOW(IADI+NIFLOW+J)
               IFLOW_L(IADI+NIFLOW+J)=NODLOCAL(JJ)  ! IBUF
               IF (NODLOCAL(JJ)/=0) THEN
                  NNO_L=NNO_L+1
                  IFLOW_L(IADL+NNO_L)=J  ! IBUFL
               ENDIF
            ENDDO
            IFLOW_L(IADI+16)=NNO_L

            IF(NSPMD > 1) THEN
              NR=(P-1)/NPCOL+1
              NC=P-(NR-1)*NPCOL
              IF(JFORM == 1) IADL=IADL+NNO+NNO+NEL
              IF(JFORM == 2) IADL=IADL+NNO+NBGAUGE+NNO+NEL
              NNR_P=0
              DO J=1,NEL
                 IF (IFLOW(IADL+J)==NR) THEN
                    NNR_P=NNR_P+1
                    IFLOW_L(IADL+J)=1   ! IBUFELR
                 ELSE
                    IFLOW_L(IADL+J)=0
                 ENDIF
              ENDDO
              IADL=IADL+NEL
              NNC_P=0
              DO J=1,NEL
                 IF (IFLOW(IADL+J)==NC) THEN
                    NNC_P=NNC_P+1
                    IFLOW_L(IADL+J)=1   ! IBUFELC
                 ELSE
                    IFLOW_L(IADL+J)=0
                 ENDIF
              ENDDO
C
              IF(P == 1) THEN
                 WRITE(IOUT,1000) NPROW, NPCOL, NBLOC
                 WRITE(IOUT,'(4X,A)') 'PROC  NB OF ROWS  NB OF COLUMNS'
              ENDIF
              WRITE(IOUT,'(3I10)') P,NNR_P,NNC_P
			  
              IFLOW_L(IADI+8) = NNR_P+NBLOC
              IFLOW_L(IADI+9) = NNR_P*NNC_P
              IFLOW_L(IADI+10)= IADMATI
              IFLOW_L(IADI+11)= IADMATR
              IFLOW_L(IADI+20)= NNR_P
C
              MEMFLOW(1)=MEMFLOW(1)+IFLOW_L(IADI+8)
              MEMFLOW(2)=MEMFLOW(2)+IFLOW_L(IADI+9)
C
              IADMATI=IADMATI+IFLOW_L(IADI+8)
              IADMATR=IADMATR+IFLOW_L(IADI+9)
            ENDIF
         ENDIF
         IADI=IADI+IFLOW(IADI+14)
      ENDDO 
C
 1000 FORMAT(/4X,'PARALLEL SOLVER PARAMETERS (SCALAPACK)'
     .       /4X,'--------------------------------------'
     .       /4X,'NUMBER OF ROW OF PROCESS GRID           ',I10
     .       /4X,'NUMBER OF COLUMNS OF PROCESS GRID       ',I10
     .       /4X,'2D-CYCLIC DECOMPOSITION BLOCK-SIZE      ',I10)
      RETURN
      END
