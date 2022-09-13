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
Chd|  W_ICNDS10                     source/restart/ddsplit/w_icnds10.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|====================================================================
      SUBROUTINE W_ICNDS10(ICNDS10,ITAGND,PROC,NODLOCAL,NS10E_L,
     .                     ITABCNDM,NBDDCNDM,NUMNOD_L,LEN_IA,itab)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICNDS10(3,*),PROC,NS10E_L,ITAGND(*),NODLOCAL(*),LEN_IA,
     .        ITABCNDM(*),NBDDCNDM,NUMNOD_L,itab(*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, NN, K, J, L,P, N_L,N1,N2, ILP,PROCI,NG
      INTEGER ICNDS10_L(3,NS10E_L),IFRONT(NUMNOD_L),NF
C
      PROCI = PROC + 1
C-----only edge in front (>0) is tagged      
      IFRONT(1:NUMNOD_L)=0
      N_L = 0
      DO N = 1, NS10E
       NN = ICNDS10(1,N)
       IF(NLOCAL(NN,PROCI)==1.AND.ITAGND(NN)<=NS10E)THEN
         N1 = ICNDS10(2,N)
         N2 = ICNDS10(3,N)
         N_L = N_L + 1
         ICNDS10_L(1,N_L) = NODLOCAL(NN)
         ICNDS10_L(2,N_L) = NODLOCAL(N1)
         ICNDS10_L(3,N_L) = NODLOCAL(N2)
         NF = 0
         DO P = 1, NSPMD
          NF = NF +NLOCAL(NN,P)           
         ENDDO
         IF (NF >1) THEN
          IFRONT(NODLOCAL(N1))=1
          IFRONT(NODLOCAL(N2))=1
         END IF
         IF (ITAGND(NN) <0) ICNDS10_L(1,N_L)= -ICNDS10_L(1,N_L)
 100    CONTINUE         
       ENDIF
      ENDDO
      IF (N_L /= NS10E_L) print *,'**error in W_ICNDS10,N_L,NS10E_L=',N_L,NS10E_L
C
      L = 3*N_L
      CALL WRITE_I_C(ICNDS10_L,L)
      LEN_IA = LEN_IA + L
C      
      IF(NBDDCNDM>0) THEN
          ITABCNDM(1:NUMNOD_L) = 0
          ILP=0
          DO N = 1, N_L
            N1 = ICNDS10_L(2,N)
            N2 = ICNDS10_L(3,N)
            IF(ITABCNDM(N1)==0.AND.IFRONT(N1)>0) THEN
              ILP = ILP + 1
              ITABCNDM(N1) = ILP
            ENDIF
            IF(ITABCNDM(N2)==0.AND.IFRONT(N2)>0) THEN
              ILP = ILP + 1
              ITABCNDM(N2) = ILP
            ENDIF
          END DO
      ENDIF
C
      RETURN
      END
