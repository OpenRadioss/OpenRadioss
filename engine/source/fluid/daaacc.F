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
Chd|  DAAACC                        source/fluid/daaacc.F
Chd|-- called by -----------
Chd|        FLOW1                         source/fluid/flow1.F
Chd|-- calls ---------------
Chd|        SPMD_FL_SUM                   source/mpi/generic/spmd_fl_sum.F
Chd|====================================================================
      SUBROUTINE DAAACC(NDIM, NNO, NEL, IFLOW, IBUF, ELEM, IBUFL, CNP, A, NORMAL, ACCF)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NDIM, NNO, NEL, IFLOW(*), IBUF(*), ELEM(NDIM,*), IBUFL(*), CNP(*)
        my_real A(3,*), NORMAL(3,*), ACCF(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER  I, K, N, N1, N2, N3, N4
        INTEGER  II, JJ, KK, JFORM, NNO_L, LENBUF
        my_real  AX, AY, AZ, WI(4,2), PM1
        my_real  AL(3,NNO)
        my_real, ALLOCATABLE :: SBUF(:), RBUF(:)
C
        JFORM = IFLOW(4)
C Accelerations locales SPMD
        IF (NSPMD == 1) THEN
          DO I=1,NNO
            II=IBUF(I)
            AL(1,I)=A(1,II)
            AL(2,I)=A(2,II)
            AL(3,I)=A(3,II)
          ENDDO
        ELSE
          NNO_L = IFLOW(16)
          LENBUF=3*NNO
          ALLOCATE(SBUF(LENBUF), RBUF(LENBUF))
          SBUF(1:LENBUF)=ZERO
          RBUF(1:LENBUF)=ZERO
          DO I=1,NNO_L
            II=IBUFL(I)
            JJ=IBUF(II)
            KK=3*(II-1)
            SBUF(KK+1)=A(1,JJ)/CNP(II)
            SBUF(KK+2)=A(2,JJ)/CNP(II)
            SBUF(KK+3)=A(3,JJ)/CNP(II)
          ENDDO

          CALL SPMD_FL_SUM(SBUF, LENBUF, RBUF)

          DO I=1,NNO
            K=3*(I-1)
            AL(1,I)=RBUF(K+1)
            AL(2,I)=RBUF(K+2)
            AL(3,I)=RBUF(K+3)
          ENDDO
          DEALLOCATE(SBUF, RBUF)
        ENDIF

        IF(JFORM == 1) THEN
          DO I = 1,NEL
            N1 = ELEM(1,I)
            N2 = ELEM(2,I)
            N3 = ELEM(3,I)
            AX = THIRD * (AL(1,N1) + AL(1,N2) + AL(1,N3))
            AY = THIRD * (AL(2,N1) + AL(2,N2) + AL(2,N3))
            AZ = THIRD * (AL(3,N1) + AL(3,N2) + AL(3,N3))
            ACCF(I) = AX*NORMAL(1,I)+AY*NORMAL(2,I)+AZ*NORMAL(3,I)
          ENDDO
        ELSEIF(JFORM == 2) THEN
          WI(1,1)=FOURTH
          WI(2,1)=FOURTH
          WI(3,1)=FOURTH
          WI(4,1)=FOURTH
          WI(1,2)=THIRD
          WI(2,2)=THIRD
          WI(3,2)=ONE_OVER_6
          WI(4,2)=ONE_OVER_6
          DO I = 1,NEL
            N1 = ELEM(1,I)
            N2 = ELEM(2,I)
            N3 = ELEM(3,I)
            N4 = ELEM(4,I)
            K  = ELEM(5,I)
            AX = WI(1,K)*AL(1,N1)+WI(2,K)*AL(1,N2)+WI(3,K)*AL(1,N3)+WI(4,K)*AL(1,N4)
            AY = WI(1,K)*AL(2,N1)+WI(2,K)*AL(2,N2)+WI(3,K)*AL(2,N3)+WI(4,K)*AL(2,N4)
            AZ = WI(1,K)*AL(3,N1)+WI(2,K)*AL(3,N2)+WI(3,K)*AL(3,N3)+WI(4,K)*AL(3,N4)
            ACCF(I) = AX*NORMAL(1,I)+AY*NORMAL(2,I)+AZ*NORMAL(3,I)
          ENDDO
        ENDIF
C
        RETURN
      END

