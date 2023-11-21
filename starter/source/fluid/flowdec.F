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
Chd|  FLOWDEC                       source/fluid/flowdec.F        
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|====================================================================
      SUBROUTINE FLOWDEC(IFLOW )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------

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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IFLOW(*)
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL   
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IADI, I, ITYP, NINOUT, NNO, NEL, NNN, NBLOC, IADL, PP,
     .        JJ, J, NPROW, NPCOL, K, P, JFORM
      INTEGER PGRID(NSPMD,NSPMD)
C
      IADI=0
      DO I=1,NFLOW
         ITYP =IFLOW(IADI+2)
         NNO  =IFLOW(IADI+5)
         NEL  =IFLOW(IADI+6)
         NBLOC=IFLOW(IADI+12)
         NPROW=IFLOW(IADI+18)
         NPCOL=IFLOW(IADI+19)
         PP=0
         DO J=1,NPROW
            DO K=1,NPCOL
               PP=PP+1
               PGRID(J,K)=PP
            ENDDO
         ENDDO

         IF (ITYP==1) THEN
C
            NINOUT=IFLOW(IADI+4)
            NNN=IFLOW(IADI+7)
            IADL=IADI+NIFLOW+NNO+3*NEL+NINOUT*NIIOFLOW+NNN+NEL
            PP=1
            JJ=0
            DO J=1,NNO
               JJ=JJ+1
               IF (JJ==NBLOC+1) THEN
                  PP=PP+1
                  IF (PP==NPROW+1) PP=1
                  JJ=1
               ENDIF
               IFLOW(IADL+J)=PP  !IBUFR
            ENDDO
            PP=1
            JJ=0
            DO J=1,NNO
               JJ=JJ+1
               IF (JJ==NBLOC+1) THEN
                  PP=PP+1
                  IF (PP==NPCOL+1) PP=1
                  JJ=1
               ENDIF
               IFLOW(IADL+NNO+J)=PP  !IBUFC
            ENDDO
C
            IADL=IADL+4*NNO+2*NNN
            PP=1
            JJ=0
            DO J=1,NEL
               JJ=JJ+1
               IF (JJ==NBLOC+1) THEN
                  PP=PP+1
                  IF (PP==NPROW+1) PP=1
                  JJ=1
               ENDIF
               IFLOW(IADL+J)=PP   !IBUFELR
            ENDDO
            PP=1
            JJ=0
            DO J=1,NEL
               JJ=JJ+1
               IF (JJ==NBLOC+1) THEN
                  PP=PP+1
                  IF (PP==NPCOL+1) PP=1
                  JJ=1
               ENDIF
               IFLOW(IADL+NEL+J)=PP  !IBUFELC
            ENDDO
C CNP nombre de processeurs possedant chaque noeud
            IADL=IADI+NIFLOW+NNO+3*NEL+NINOUT*NIIOFLOW+NNN+NEL+3*NNO
            DO J=1,NNO
               IFLOW(IADL+J)=0
            ENDDO
            DO P=1,NSPMD
               DO J=1,NNO
                  JJ=IFLOW(IADI+NIFLOW+J)
                  IF (NLOCAL(JJ,P)==1) IFLOW(IADL+J)=IFLOW(IADL+J)+1
               ENDDO
            ENDDO
C CNPI nombre de processeurs possedant chaque noeud auxiliaire
            IADL=IADL+NNO+NNN
            DO J=1,NNN
               IFLOW(IADL+J)=0
            ENDDO
            DO P=1,NSPMD
               DO J=1,NNN
                  JJ=IFLOW(IADI+NIFLOW+NNO+3*NEL+NINOUT*NIIOFLOW+J)           
                  IF (NLOCAL(JJ,P)==1) IFLOW(IADL+J)=IFLOW(IADL+J)+1
               ENDDO
            ENDDO
C            
         ELSEIF(ITYP == 3) THEN
            JFORM=IFLOW(IADI+4)
C CNP nombre de processeurs possedant chaque noeud
            IADL = 0 
            IF(JFORM==1) IADL=IADI+NIFLOW+NNO+3*NEL+NNO
            IF(JFORM==2) IADL=IADI+NIFLOW+NNO+5*NEL+NNO+NBGAUGE
            DO J=1,NNO
               IFLOW(IADL+J)=0
            ENDDO
            DO P=1,NSPMD
               DO J=1,NNO
                  JJ=IFLOW(IADI+NIFLOW+J)
                  IF (NLOCAL(JJ,P)==1) IFLOW(IADL+J)=IFLOW(IADL+J)+1
               ENDDO
            ENDDO
C IBUFELR ligne des elements dans la Process Grid
            IADL=IADL+NNO+NEL
            PP=1
            JJ=0
            DO J=1,NEL
               JJ=JJ+1
               IF (JJ==NBLOC+1) THEN
                  PP=PP+1
                  IF (PP==NPROW+1) PP=1
                  JJ=1
               ENDIF
               IFLOW(IADL+J)=PP   !IBUFELR
            ENDDO
C IBUFELC colonne des elements dans la Process Grid
            IADL=IADL+NEL
            PP=1
            JJ=0
            DO J=1,NEL
               JJ=JJ+1
               IF (JJ==NBLOC+1) THEN
                  PP=PP+1
                  IF (PP==NPCOL+1) PP=1
                  JJ=1
               ENDIF
               IFLOW(IADL+J)=PP  !IBUFELC
            ENDDO
         ENDIF
         IADI=IADI+IFLOW(IADI+14)
      ENDDO
C
      RETURN
      END
