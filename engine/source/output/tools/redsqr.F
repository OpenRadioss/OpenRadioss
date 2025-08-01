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
!||    redsqr         ../engine/source/output/tools/redsqr.F
!||--- called by ------------------------------------------------------
!||    convrrest      ../engine/source/output/restart/rdresb.F
!||    dsrrest        ../engine/source/output/restart/rdresb.F
!||    eigrrest       ../engine/source/output/restart/rdresb.F
!||    fxbresb        ../engine/source/output/restart/rdresb.F
!||    fxfluxrrest    ../engine/source/constraints/thermic/fxfluxrrest.F
!||    fxtemprrest    ../engine/source/output/restart/rdresb.F
!||    nfrrest        ../engine/source/output/restart/rdresb.F
!||    nitscherrest   ../engine/source/output/restart/rdresb.F
!||    radiarrest     ../engine/source/output/restart/rdresb.F
!||    rmatrrest      ../engine/source/output/restart/rdresb.F
!||    thcrrest       ../engine/source/output/restart/rdresb.F
!||====================================================================
      SUBROUTINE REDSQR(A,L,IFORM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER L, IFORM
C     REAL
      my_real
     .   A(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr13_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LREC, L1, L2, I
      CHARACTER*8 STR(10),OneSTR
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER IC
C     REAL
      my_real
     .   RSTR
      EXTERNAL RSTR
C-----------------------------------------------
      IF (L==0)RETURN
      IF (MOD(IFORM,5)==1)THEN
       LREC=10
      ELSE
       LREC=2000
      ENDIF
C
      L1=1
      L2=LREC
   1  L2=MIN(L,L2)
   
      IF (MOD(IFORM,5)==1)THEN
         READ(IUNIT,'(10A8)') (STR(I),I=1,10)
         DO I=L1,L2
           OneStr=STR(I-L1+1)
           A(I)=RSTR(OneStr)
         ENDDO
      ELSE
         READ(IUNIT) (A(I),I=L1,L2)
      ENDIF
      
      IF(L2==L)RETURN
      L1=L2+1
      L2=L1+LREC-1
      GOTO 1
      END
!||====================================================================
!||    redsqrdp   ../engine/source/output/tools/redsqr.F
!||====================================================================
      SUBROUTINE REDSQRDP(A,L,IFORM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER L, IFORM
C     REAL
      DOUBLE PRECISION
     .   A(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr13_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LREC, L1, L2, I
      CHARACTER*8 STR(10),OneSTR
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C     REAL
      my_real 
     .   RSTR
      my_real
     .   VAL
C-----------------------------------------------
      EXTERNAL RSTR
      IF (L==0)RETURN
      IF (MOD(IFORM,5)==1)THEN
       LREC=10
      ELSE
       LREC=2000
      ENDIF
C
      L1=1
      L2=LREC
   1  L2=MIN(L,L2)
      IF (MOD(IFORM,5)==1)THEN
       READ(IUNIT,'(10A8)') (STR(I),I=1,10)
       DO  I=L1,L2
          OneSTR=STR(I-L1+1)
          VAL=RSTR(OneSTR)
          A(I)=VAL
       ENDDO
      ELSE
       READ(IUNIT) (A(I),I=L1,L2)
      ENDIF
      IF(L2==L)RETURN
      L1=L2+1
      L2=L1+LREC-1
      GOTO 1
      END
