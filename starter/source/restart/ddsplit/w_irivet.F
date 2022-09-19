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
C
c      SUBROUTINE C_RIVET(IXRI,FRONT,PROC,NRIVET_L)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
c#include      "implicit_f.inc"
c#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
c#include      "com04_c.inc"
c#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
c       INTEGER  PROC, NRIVET_L,
c     .          IXRI(4,*), FRONT(NUMNOD,*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
c      INTEGER N, N1, N2
C
c      NRIVET_L = 0
c      DO N = 1, NRIVET
c        N1=IXRI(2,N)
c        N2=IXRI(3,N)
c        IF(MOD(FRONT(N1,PROC+1),10)==1.AND.
c     +     MOD(FRONT(N2,PROC+1),10)==1)THEN
c          NRIVET_L = NRIVET_L + 1
c        ENDIF
c      ENDDO
C
c      RETURN
c      END
C
Chd|====================================================================
Chd|  W_IRIVET                      source/restart/ddsplit/w_irivet.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|====================================================================
      SUBROUTINE W_IRIVET(IXRI,NODLOCAL,PROC,LEN_IA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------  

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       INTEGER  PROC, LEN_IA,
     .          IXRI(4,*), NODLOCAL(*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N_L, N, N1, N2, IXRI_L(4,NRIVET)
C
      N_L = 0
      DO N = 1, NRIVET
        N1=IXRI(2,N)
        N2=IXRI(3,N)
        IF(NLOCAL(N1,PROC+1)==1.AND.
     +     NLOCAL(N2,PROC+1)==1)THEN
          IXRI_L(1,N) = IXRI(1,N)
          IXRI_L(2,N) = NODLOCAL(N1)
          IXRI_L(3,N) = NODLOCAL(N2)
          IXRI_L(4,N) = IXRI(4,N)
        ELSE
          IXRI_L(1,N) = IXRI(1,N)
          IXRI_L(2,N) = -N1
          IXRI_L(3,N) = N2
          IXRI_L(4,N) = IXRI(4,N)
        ENDIF
      ENDDO
C
      CALL WRITE_I_C(IXRI_L,NRIVET*4)
      LEN_IA = LEN_IA + NRIVET*4
C
      RETURN
      END
