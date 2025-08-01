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
!||    donesec           ../engine/source/output/anim/generate/donesec.F
!||--- called by ------------------------------------------------------
!||    genani            ../engine/source/output/anim/generate/genani.F
!||--- calls      -----------------------------------------------------
!||    spmd_glob_isum9   ../engine/source/mpi/interfaces/spmd_th.F
!||====================================================================
      SUBROUTINE DONESEC(ISECT,NESCT,NSTRF,IXS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISECT,NESCT,
     .        NSTRF(*),IXS(NIXS,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J, I, KK, K0, K1, K5, K9, NSEG, NSEGC, NSEGTG
      INTEGER NSEGS, POWER2(8),IPACK,
     .         II(8), N1, N2, N3, N4, N5, N6, N7, N8, K3, N
      DATA POWER2/1,2,4,8,16,32,64,128/
      SAVE K1
      SAVE K0
C-----------------------------------------------
        IF (ISECT==1) K0 = NSTRF(25)
        K5=K0+30+NSTRF(K0+14)+NSTRF(K0+6)
     1          +2*NSTRF(K0+7) +NSTRF(K0+8)*2
        NSEGC = NSTRF(K0+9)
        DO J=1,NSEGC
          KK = K5+2*(J-1)+1
          IF(NSTRF(KK)/=0) NESCT = NESCT + 1
        ENDDO
        K9=K5+2*NSTRF(K0+9) +2*NSTRF(K0+10)
     1       +2*NSTRF(K0+11)+2*NSTRF(K0+12)
        NSEGTG = NSTRF(K0+13)
        DO J=1,NSEGTG
          KK = K9+2*(J-1)+1
          IF(NSTRF(KK)/=0) NESCT = NESCT + 1
        ENDDO
        K3=K0+30+NSTRF(K0+14)+NSTRF(K0+6)
        NSEGS=NSTRF(K0+7)

        IF(NSEGS/=0) NESCT = NESCT + 1

        DO J=1,NSEGS
          KK=K3+2*(J-1)
          IPACK=NSTRF(KK+1)
          IF(IPACK/=0)THEN
            N =NSTRF(KK)
            II(1)=IXS(2,N)
            II(2)=IXS(3,N)
            II(3)=IXS(4,N)
            II(4)=IXS(5,N)
            II(5)=IXS(6,N)
            II(6)=IXS(7,N)
            II(7)=IXS(8,N)
            II(8)=IXS(9,N)
            IF(     II(2)==II(1).AND.II(4)==II(3)
     .         .AND.II(8)==II(5).AND.II(7)==II(6))THEN
C tetra4, tetra10
                N1=MOD(IPACK/POWER2(1),2)
                N2=MOD(IPACK/POWER2(3),2)
                N3=MOD(IPACK/POWER2(5),2)
                N4=MOD(IPACK/POWER2(6),2)
                IF(N1/=0.AND.N2/=0.AND.N3/=0)
     .             NESCT = NESCT + 1
                IF(N1/=0.AND.N2/=0.AND.N4/=0)
     .             NESCT = NESCT + 1
                IF(N2/=0.AND.N3/=0.AND.N4/=0)
     .             NESCT = NESCT + 1
                IF(N3/=0.AND.N1/=0.AND.N4/=0)
     .             NESCT = NESCT + 1
            ELSE
C brick, shell16, brick20
                N1=MOD(IPACK/POWER2(1),2)
                N2=MOD(IPACK/POWER2(2),2)
                N3=MOD(IPACK/POWER2(3),2)
                N4=MOD(IPACK/POWER2(4),2)
                N5=MOD(IPACK/POWER2(5),2)
                N6=MOD(IPACK/POWER2(6),2)
                N7=MOD(IPACK/POWER2(7),2)
                N8=MOD(IPACK/POWER2(8),2)
                IF(N1/=0.AND.N2/=0.AND.N3/=0.AND.N4/=0)
     .             NESCT = NESCT + 1
                IF(N5/=0.AND.N6/=0.AND.N7/=0.AND.N8/=0)
     .             NESCT = NESCT + 1
                IF(N1/=0.AND.N5/=0.AND.N6/=0.AND.N2/=0)
     .             NESCT = NESCT + 1
                IF(N4/=0.AND.N8/=0.AND.N7/=0.AND.N3/=0)
     .             NESCT = NESCT + 1
                IF(N1/=0.AND.N4/=0.AND.N8/=0.AND.N5/=0)
     .             NESCT = NESCT + 1
                IF(N2/=0.AND.N3/=0.AND.N7/=0.AND.N6/=0)
     .             NESCT = NESCT + 1
            END IF
          END IF
        END DO
        K0=NSTRF(K0+24)
      IF (NSPMD > 1) THEN
        CALL SPMD_GLOB_ISUM9(NESCT,1)
        IF (ISPMD/=0) NESCT = 0
      ENDIF
C
      RETURN
      END
