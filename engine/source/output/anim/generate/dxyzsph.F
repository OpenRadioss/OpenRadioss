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
Chd|  DXYZSPH                       source/output/anim/generate/dxyzsph.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        SPMD_GATHER_SPH               source/mpi/anim/spmd_gather_sph.F
Chd|        WRITE_R_C                     source/output/tools/sortie_c.c
Chd|====================================================================
      SUBROUTINE DXYZSPH (NESPH,KXSP,X,SPBUF,SNNSPHG,NNSPH)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "sphcom.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NESPH, KXSP(NISP,*),SNNSPHG,NNSPH
      my_real
     .  X(3,*),SPBUF(NSPBUF,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, J, INOD,CNT,BUF
      my_real
     3   XI,YI,ZI,R,
     4   XQ(3),FACT
      REAL R4,R4NP(3*NNSPH)
C-----------------------------------------------
      IF (NUMSPH+MAXPJET.EQ.0) GOTO 100
C-----------------------------------------------
C-----------------------------------------------
      CNT = 0
      FACT =(TROIS/(QUATRE*SQRT(DEUX)))**THIRD
      DO 200 N=1,NUMSPH+MAXPJET
         INOD=KXSP(3,N)
         XI  =X(1,INOD)
         YI  =X(2,INOD)
         ZI  =X(3,INOD)
           R   = SPBUF(1,N)*FACT
           XQ(1) =XI+R
           XQ(2) =YI-R
           XQ(3) =ZI-R
C           XQ(1) =XI+R*SQRT(2.)
C           XQ(2) =YI+R*SQRT(2.)/SQRT(3.)
C           XQ(3) =ZI-R/SQRT(3.)
           IF (NSPMD == 1) THEN
            DO J=1,3
             R4 = XQ(J)
             CALL WRITE_R_C(R4,1)
            ENDDO
           ELSE
             CNT = CNT + 1
             R4NP(CNT) = XQ(1)
             CNT = CNT + 1
             R4NP(CNT) = XQ(2)
             CNT = CNT + 1
             R4NP(CNT) = XQ(3)
           ENDIF
           XQ(1) =XI-R
           XQ(2) =YI+R
           XQ(3) =ZI-R
C           XQ(1) =XI-R*SQRT(2.)
C           XQ(2) =YI+R*SQRT(2.)/SQRT(3.)
C           XQ(3) =ZI-R/SQRT(3.)
           IF (NSPMD == 1) THEN
            DO J=1,3
              R4 = XQ(J)
            CALL WRITE_R_C(R4,1)
            ENDDO
           ELSE
             CNT = CNT + 1
             R4NP(CNT) = XQ(1)
             CNT = CNT + 1
             R4NP(CNT) = XQ(2)
             CNT = CNT + 1
             R4NP(CNT) = XQ(3)
           ENDIF
           XQ(1) =XI-R
           XQ(2) =YI-R
           XQ(3) =ZI+R
C           XQ(1) =XI
C           XQ(2) =YI-R*2.*SQRT(2.)/SQRT(3.)
C           XQ(3) =ZI-R/SQRT(3.)
           IF (NSPMD == 1) THEN
            DO J=1,3
             R4 = XQ(J)
             CALL WRITE_R_C(R4,1)
            ENDDO
           ELSE
             CNT = CNT + 1
             R4NP(CNT) = XQ(1)
             CNT = CNT + 1
             R4NP(CNT) = XQ(2)
             CNT = CNT + 1
             R4NP(CNT) = XQ(3)
           ENDIF
          XQ(1) =XI+R
          XQ(2) =YI+R
          XQ(3) =ZI+R
C           XQ(1) =XI
C           XQ(2) =YI
C           XQ(3) =ZI+SQRT(3.)*R
           IF (NSPMD == 1) THEN
            DO J=1,3
             R4 = XQ(J)
             CALL WRITE_R_C(R4,1)
            ENDDO
           ELSE
             CNT = CNT + 1
             R4NP(CNT) = XQ(1)
             CNT = CNT + 1
             R4NP(CNT) = XQ(2)
             CNT = CNT + 1
             R4NP(CNT) = XQ(3)
           ENDIF
 200   CONTINUE
 100  CONTINUE
C-------------------------------------------------------
      IF (NSPMD > 1) THEN
        IF (ISPMD.EQ.0) THEN
          BUF = 3*SNNSPHG
        ELSE
          BUF=1
        ENDIF
        CALL SPMD_GATHER_SPH(R4NP,NUMSPH*4,BUF)
      ENDIF
C
      RETURN
      END
