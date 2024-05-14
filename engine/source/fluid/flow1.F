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
Chd|  FLOW1                         source/fluid/flow1.F          
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        DAAACC                        source/fluid/daaacc.F         
Chd|====================================================================
      SUBROUTINE FLOW1(IFLOW, RFLOW, NBGAUGE, A)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "flowcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IFLOW(*), NBGAUGE
      my_real RFLOW(*), A(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  N, NEL, NNO, II1, II2, II3, II4, II5, II6
      INTEGER  IR1, IR2, IR3, IR4, IR5, IR6, IR7, IR8
      INTEGER  IADI, IADR, ITYP, JFORM, NDIM, FREESURF
C
      IADI=0
      IADR=0
      DO N=1,NFLOW
         ITYP=IFLOW(IADI+2)
         IF (ITYP==3) THEN
            JFORM    = IFLOW(IADI+4)
            NNO      = IFLOW(IADI+5)
            NEL      = IFLOW(IADI+6)
            FREESURF = IFLOW(IADI+25)
            II1=IADI+1
            II2=II1+NIFLOW
            II3=II2+NNO
            IF(JFORM==1) THEN
               II4=II3+3*NEL
               II5=II4+NNO
               II6=II5
               NDIM=3
            ELSEIF(JFORM==2) THEN
               II4=II3+5*NEL
               II5=II4+NNO
               II6=II5+NBGAUGE
               NDIM=5
            ENDIF
            IR1=IADR+1
            IR2=IR1+NRFLOW
            IR3=IR2+NEL*3
            IR4=IR3+NEL*FREESURF
            IR5=IR4+NEL
            IR6=IR5+NEL*FREESURF
            IR7=IR6+NEL*FREESURF
            IR8=IR7+NEL*NEL
            CALL DAAACC(NDIM, NNO, NEL, IFLOW(II1), IFLOW(II2), IFLOW(II3), IFLOW(II4), IFLOW(II6), 
     .                  A,  RFLOW(IR2), RFLOW(IR8))
         ENDIF
         IF(N < NFLOW) THEN
           IADI=IADI+IFLOW(IADI+14)  
           IADR=IADR+IFLOW(IADI+15)
         ENDIF
      ENDDO
C
      RETURN
      END
      
