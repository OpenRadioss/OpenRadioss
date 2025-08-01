Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    aninflow   ../engine/source/output/anim/generate/aninflow.F
!||--- called by ------------------------------------------------------
!||    nodalp     ../engine/source/output/anim/generate/nodalp.F
!||====================================================================
      SUBROUTINE ANINFLOW(IFUNC, WA4, IFLOW, RFLOW)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "flowcom.inc"
* debug SPMD
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IFUNC, IFLOW(*)
      my_real
     .        RFLOW(*)
      REAL WA4(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IADI, IADR, I, ITYP, NINOUT, NNO, NEL, II1, II2,
     .        IR1, IR2, J, JJ, NNO_L, NNI_L, II3, II4, JJJ, NNI

         IADI=0
         IADR=0
         DO I=1,NFLOW
            ITYP=IFLOW(IADI+2)
            IF (ITYP==1) THEN
               NINOUT=IFLOW(IADI+4)
               NNO=IFLOW(IADI+5)
               NEL=IFLOW(IADI+6)
               NNI=IFLOW(IADI+7)
               NNO_L=IFLOW(IADI+16)
               NNI_L=IFLOW(IADI+22)
               II1=IADI+NIFLOW
               II2=II1+NNO+3*NEL+NINOUT*NIIOFLOW
               IF(NSPMD == 1) THEN
                  II3=II2+NNI+NEL
                  II4=II3+NNO
               ELSE
                  II3=II2+NNI+NEL+2*NNO
                  II4=II3+2*NNO
               ENDIF
               IR1=IADR+NRFLOW
               IR2=IR1+NNO+NNI
               IF (IFUNC==3) THEN
                  DO J=1,NNO_L
                     JJ=IFLOW(II3+J)
                     JJJ=IFLOW(II1+JJ)
                     WA4(JJJ)=RFLOW(IR2+JJ)
                  ENDDO
                  DO J=1,NNI_L
                     JJ=IFLOW(II4+J)
                     JJJ=IFLOW(II2+JJ)
                     WA4(JJJ)=RFLOW(IR2+NNO+JJ)
                  ENDDO
               ELSEIF (IFUNC==13) THEN
                  DO J=1,NNO_L
                     JJ=IFLOW(II3+J)
                     JJJ=IFLOW(II1+JJ)
                     WA4(JJJ)=RFLOW(IR1+JJ)
                  ENDDO
                  DO J=1,NNI_L
                     JJ=IFLOW(II4+J)
                     JJJ=IFLOW(II2+JJ)
                     WA4(JJJ)=RFLOW(IR1+NNO+JJ)
                  ENDDO
               ENDIF
            ENDIF
            IADR=IADR+IFLOW(IADI+15)
            IADI=IADI+IFLOW(IADI+14)
         ENDDO

      RETURN
      END
