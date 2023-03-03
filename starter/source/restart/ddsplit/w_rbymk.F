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
Chd|  W_RBYMK                       source/restart/ddsplit/w_rbymk.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|====================================================================
      SUBROUTINE W_RBYMK(NPBY,LPBY,FRONT_RM,NODLOCAL,PROC,
     .                          NSLARB_L,LEN_IA)
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
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       INTEGER  PROC, LEN_IA, NSLARB_L, NODLOCAL(*),
     .          NPBY(NIRBYM,*), LPBY(*),
     .          FRONT_RM(NRBYM,*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NSL, K, KK, N, NN, NSL_L, J, M, ILP,
     .        NPBY_L(NIRBYM,NRBYM), LPBY_L(NSLARB_L)
      INTEGER, DIMENSION(:), ALLOCATABLE :: SECNDNG,INDEX,IWORK
C
      ILP = 0
      K = 0
      DO N = 1, NRBYM
        NSL=NPBY(2,N)
        NSL_L = 0
        DO KK = 1, NSL
          NN = LPBY(K+KK)
          IF(NLOCAL(NN,PROC+1)==1)THEN
            NSL_L = NSL_L + 1
            LPBY_L(NSL_L+ILP) = NODLOCAL(NN)
          ENDIF
        ENDDO
        NPBY_L(2,N) = NSL_L
        M = NPBY(1,N)
C optimisation pour ne pas traiter les rigid material  inutiles
        IF(NSL_L==0.AND.MOD(FRONT_RM(M,PROC+1),10)==0)THEN
          NPBY_L(1,N) = -M
        ELSE
          NPBY_L(1,N) = M
        ENDIF
        K = K + NSL
        ILP = ILP + NSL_L
      ENDDO
C
      CALL WRITE_I_C(NPBY_L,NIRBYM*NRBYM)
      CALL WRITE_I_C(LPBY_L,NSLARB_L)
      LEN_IA = LEN_IA + NIRBYM*NRBYM + NSLARB_L
C
      RETURN
      END
