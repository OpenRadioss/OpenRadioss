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
!||    dsphnor      ../engine/source/output/anim/generate/dsphnor.F
!||--- called by ------------------------------------------------------
!||    genani       ../engine/source/output/anim/generate/genani.F
!||--- calls      -----------------------------------------------------
!||    spmd_igath   ../engine/source/mpi/anim/spmd_igath.F
!||    write_s_c    ../common_source/tools/input_output/write_routtines.c
!||====================================================================
      SUBROUTINE DSPHNOR(KXSP,X,SPBUF,NNSPH)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "sphcom.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXSP(NISP,*),NNSPH
      my_real
     .  X(3,*),SPBUF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I3000,NXX,NYY,NZZ
      INTEGER N, J, INOD
      INTEGER INP(NNSPH*3),CNT,LRECV
      my_real
     3   XI,YI,ZI,R,NX,NY,NZ,NORMN,
     4   XQ(3)
C-----------------------------------------------
      IF (NUMSPH+MAXPJET==0) GOTO 100
C-----------------------------------------------
      I3000 = 3000
      CNT = 0
C
      DO 200 N=1,NUMSPH+MAXPJET
        INOD=KXSP(3,N)
        XI  =X(1,INOD)
        YI  =X(2,INOD)
        ZI  =X(3,INOD)
        NX= ONE
        NY=-ONE
        NZ=-ONE         
C        NX= SQRT(2.)
C        NY= SQRT(2.)/SQRT(3.)
C        NZ=-1./SQRT(3.)
        NORMN=SQRT(NX**2+NY**2+NZ**2)
        IF (NORMN==ZERO) THEN
         NX=THREE1000
         NY=THREE1000
         NZ=THREE1000          
        ELSE
         NX=THREE1000*NX/NORMN
         NY=THREE1000*NY/NORMN
         NZ=THREE1000*NZ/NORMN          
        ENDIF
        CNT = CNT + 1
        INP(CNT) = NINT(NX)
        CNT = CNT + 1
        INP(CNT) = NINT(NY)
        CNT = CNT + 1
        INP(CNT) = NINT(NZ)
        NX=-ONE
        NY= ONE
        NZ=-ONE         
        NORMN=SQRT(NX**2+NY**2+NZ**2)
        IF (NORMN==ZERO) THEN
          NX=THREE1000
          NY=THREE1000
          NZ=THREE1000          
         ELSE
          NX=THREE1000*NX/NORMN
          NY=THREE1000*NY/NORMN
          NZ=THREE1000*NZ/NORMN          
         ENDIF
         CNT = CNT + 1
         INP(CNT) = NINT(NX)
         CNT = CNT + 1
         INP(CNT) = NINT(NY)
         CNT = CNT + 1
         INP(CNT) = NINT(NZ)
         NX=-ONE
         NY=-ONE
         NZ= ONE         
         NORMN=SQRT(NX**2+NY**2+NZ**2)
         IF (NORMN==ZERO) THEN
          NX=THREE1000
          NY=THREE1000
          NZ=THREE1000          
         ELSE
          NX=THREE1000*NX/NORMN
          NY=THREE1000*NY/NORMN
          NZ=THREE1000*NZ/NORMN          
         ENDIF
         CNT = CNT + 1
         INP(CNT) = NINT(NX)
         CNT = CNT + 1
         INP(CNT) = NINT(NY)
         CNT = CNT + 1
         INP(CNT) = NINT(NZ)
         NX= ONE
         NY= ONE
         NZ= ONE         
         NORMN=SQRT(NX**2+NY**2+NZ**2)
        IF (NORMN==ZERO) THEN
          NX=THREE1000
          NY=THREE1000
          NZ=THREE1000          
         ELSE
          NX=THREE1000*NX/NORMN
          NY=THREE1000*NY/NORMN
          NZ=THREE1000*NZ/NORMN          
         ENDIF
         CNT = CNT + 1
         INP(CNT) = NINT(NX)
         CNT = CNT + 1
         INP(CNT) = NINT(NY)
         CNT = CNT + 1
         INP(CNT) = NINT(NZ)
 200   CONTINUE
 100  CONTINUE
      IF (NSPMD > 1) THEN
        CALL SPMD_IGATH(INP,CNT,LRECV)
      ELSE
        LRECV = CNT
      END IF
      IF (ISPMD==0) THEN
         CALL WRITE_S_C(INP,LRECV)
      ENDIF

      RETURN
      END
