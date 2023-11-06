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
Chd|  ALELIN                        source/ale/grid/alelin.F
Chd|-- called by -----------
Chd|        ALEWDX                        source/ale/grid/alewdx.F
Chd|-- calls ---------------
Chd|        SPMD_GLOB_DSUM9               source/mpi/interfaces/spmd_th.F
Chd|        SPMD_RBCAST                   source/mpi/generic/spmd_rbcast.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE ALELIN(NALELK ,LINALE ,W ,WEIGHT ,IGRNOD , ITAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER,INTENT(IN)   :: NALELK
        INTEGER,INTENT(IN)   :: LINALE(*), WEIGHT(*),ITAB(NUMNOD)
        my_real, INTENT(INOUT) :: W(3,NUMNOD)
        TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: ID(3), K, JJ, M1, M2, N, IC, IM, N1, J, I, NI, GR_ID
        INTEGER :: uID, II
        my_real :: WW, WM1M2(6)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C
C This subroutines is handling links on grid velocities.
C It can be defined using /ALE/LINK/VEL
C /VEL/ALE
C
C   NALELK    : number of ALE LINKS
C   LINALE[:] : definition array
C         [1] : user ID         (uID)
C         [2] : main node     (M1)
C         [3] : main node     (M2)
C         [4] : number of nodes (N)
C         [5] : direction XYZ   (IC)
C         [6] : formulation     (IM)
C
C                  |ALE LINK 1                                       |2              |NALELK
C             +----+----+----+----+----+----+----+----+----+...-+----+----+----+--...+----+----+--...
C   LINALE    |uID | M1 | M2 | N  | IC | IM |id1 |id2 |id3 |... |idn | ...           |
C             +----+----+----+----+----+----+----+----+----+...-+----+----+----+--...+----+----+--...
C (1:SLINALE)   1    2    3    4    5    6   6+1                 6+N
C    +LLINAL               |
C                         (N=1 if grnod_id is used)
C
C    First subarrays are used to define ale links from starter (1:SLINALE). Engine links are in (SLINALE+1:SLINALE+LLINAL)
C    Only Starter part is written in restart files.
C
C-----------------------------------------------
C   P r e - C o n d i t i o n s
C-----------------------------------------------
C
C None : NALELK=0 => Nothing is done.
C
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

        K=0
        N=0
        DO JJ=1,NALELK

          WM1M2(1) = ZERO
          WM1M2(2) = ZERO
          WM1M2(3) = ZERO
          WM1M2(4) = ZERO
          WM1M2(5) = ZERO
          WM1M2(6) = ZERO
          K        = K+IABS(N)+6
          uID      = LINALE(K-5)
          M1       = LINALE(K-4)
          M2       = LINALE(K-3)
          N        = LINALE(K-2)

          IF(uID<0)CYCLE !OFF
          IF(M1 > 0)THEN                ! test if node is on current domain
            IF(WEIGHT(M1) ==  1)THEN
              WM1M2(1) = W(1,M1)
              WM1M2(2) = W(2,M1)
              WM1M2(3) = W(3,M1)
            END IF
          END IF

          IF(M2 > 0)THEN                ! test if node is on current domain
            IF(WEIGHT(M2) ==  1)THEN
              WM1M2(4) = W(1,M2)
              WM1M2(5) = W(2,M2)
              WM1M2(6) = W(3,M2)
            END IF
          END IF

          IF(NSPMD > 1) THEN
            ! Exchange in order to manage the alelink hierarchy
            CALL SPMD_GLOB_DSUM9(WM1M2,6)
            CALL SPMD_RBCAST(WM1M2,WM1M2,1,6,0,2)
          END IF

          IC=LINALE(K-1)
          IM=LINALE(K)
          ID(1)=IC/4
          IC=IC-4*ID(1)
          ID(2)=IC/2
          ID(3)=IC-2*ID(2)

          IF(N>0)THEN
            N1=N+1
          ELSE
            GR_ID=LINALE(K+1)
            N1=IGRNOD(GR_ID)%NENTITY+1
            N=1!IABS(N)
          ENDIF

          DO J=1,3
            IF(ID(J) /=  0) THEN
              IF(IM ==  0) THEN
                IF(LINALE(K-2)>0)THEN !---NODE LIST IF NUMNOD>0
                  DO I=1,N
                    NI=LINALE(K+I)
                    IF(NI > 0)THEN                ! test if node is on current domain
                      W(J,NI)=WM1M2(J)+(WM1M2(3+J)-WM1M2(J))*I/N1
                    ENDIF
                  ENDDO
                ELSE !---GRNOD IF NUMNOD =-1
                  II=0
                  DO I=1,IGRNOD(GR_ID)%NENTITY
                    NI=IGRNOD(GR_ID)%ENTITY(I)
                    II=II+1
                    W(J,NI)=WM1M2(J)+(WM1M2(3+J)-WM1M2(J))*II/N1
                  ENDDO
                ENDIF!(LINALE(K-2)>0)

              ELSE

                IF(IM*ABS(WM1M2(J)) > IM*ABS(WM1M2(3+J)))THEN
                  WW=WM1M2(J)
                ELSE
                  WW=WM1M2(3+J)
                ENDIF

                IF(LINALE(K-2)>0)THEN !---NODE LIST IF NUMNOD>0
                  DO I=1,N
                    NI=LINALE(K+I)
                    IF(NI > 0)THEN              ! test if node is on current domain
                      W(J,NI)=WW
                    ENDIF
                  ENDDO
                ELSE !---GRNOD IF NUMNOD =-1
                  GR_ID=LINALE(K+1)
                  DO I=1,IGRNOD(GR_ID)%NENTITY
                    NI=IGRNOD(GR_ID)%ENTITY(I)
                    W(J,NI)=WW
                  ENDDO
                ENDIF!(LINALE(K-2)>0)

              ENDIF!(IM ==  0)
            ENDIF!(ID(J) /=  0)
          ENDDO !next J
        ENDDO
C-----------------------------------------------
        RETURN
      END
