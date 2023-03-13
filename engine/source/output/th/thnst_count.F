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
Chd|  THNST_COUNT                   source/output/th/thnst_count.F
Chd|-- called by -----------
Chd|        INIT_TH                       source/output/th/init_th.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE THNST_COUNT(NTHGRP2, ITHGRP, WA_SIZE, INDEX_WA_NST, 
     .                      IPARG,ITHBUF)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),ITHBUF(*)
      INTEGER, INTENT(in) :: NTHGRP2
      INTEGER, INTENT(inout) :: WA_SIZE
      INTEGER, DIMENSION(2*NTHGRP2+1), INTENT(inout) :: INDEX_WA_NST
      INTEGER, DIMENSION(NITHGR,*), INTENT(in) :: ITHGRP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: BOOL
      INTEGER II, I, J, N, IH, NG, ITY, MTE, NB0, NB1, NB2, NB3,
     .   NB4, NB5, NNB3, MB1, MB2, MB3, MB4, MB5, K, IST, IP, L,
     .   LWA, IMAT, IPROP, NX, IGTYP, NUVAR, NUVARN,NEL,NFT,
     .   KVAR,KVARN
      my_real
     .   WWA(100)
      INTEGER :: J_FIRST,NITER,IAD,NN,IADV,NVAR,ITYP,IJK
      INTEGER, DIMENSION(NTHGRP2) :: INDEX_NST
C-------------------------
C           ELEMENTS NSTRANDS
C-------------------------

        IJK = 0
        WA_SIZE = 0
        INDEX_NST(1:NTHGRP2) = 0
        DO NITER=1,NTHGRP2
            ITYP=ITHGRP(2,NITER)
            NN  =ITHGRP(4,NITER)
            IAD =ITHGRP(5,NITER)
            NVAR=ITHGRP(6,NITER)
            IADV=ITHGRP(7,NITER)
            II=0
            IF(ITYP==100)THEN
!   -------------------------------
                II=0
                IH=IAD

                DO WHILE (ITHBUF(IH+NN) /= ISPMD .AND. IH < IAD+NN)
                    IH = IH + 1
                ENDDO
                IF (IH >= IAD+NN) GOTO 666 

                DO NG=1,NGROUP
                    ITY=IPARG(5,NG)
                    IF (ITY == 100) THEN
C       multi-purpose elements are nstrands elements only (to be modified).
                        NEL=IPARG(2,NG)
                        NFT=IPARG(3,NG)

                        DO I=1,NEL
                            N  =I+NFT
                            K  =ITHBUF(IH)
                            IF (K == N) THEN
                                DO J=1,NN
C                                   loop over strands into the ONE NSTRAND TH group.
C                                   strand to save:              
                                    WA_SIZE = WA_SIZE + NVAR
                                    IH=IH+1
                                ENDDO ! DO J=1,NN
                                WA_SIZE = WA_SIZE + 1
                            ENDIF ! IF (K == N)
                        ENDDO ! DO I=1,NEL
                    ENDIF ! IF (ITY == 100)
                ENDDO ! DO NG=1,NGROUP
            ENDIF
 666        continue
                INDEX_NST(NITER) = WA_SIZE
        ENDDO


        J_FIRST = 0
        BOOL = .TRUE.
        DO I=1,NTHGRP2
            IF(BOOL.EQV..TRUE.) THEN
                IF( INDEX_NST(I)/=0 ) THEN
                    BOOL = .FALSE.
                    J_FIRST = I
                ENDIF
            ENDIF
        ENDDO

        J = 0
        IF(J_FIRST>0) THEN
            J=J+1
            INDEX_WA_NST(J) = INDEX_NST(J_FIRST)
            J=J+1
            INDEX_WA_NST(J) = J_FIRST
            DO I=J_FIRST+1,NTHGRP2
                IF( INDEX_NST(I)-INDEX_NST(I-1)>0 ) THEN
                    J=J+1
                    INDEX_WA_NST(J) = INDEX_NST(I)
                    J=J+1
                    INDEX_WA_NST(J) = I
                ENDIF
            ENDDO
        ENDIF
        INDEX_WA_NST(2*NTHGRP2+1) = J  !   number of non-zero index
C-----------
      RETURN
      END SUBROUTINE THNST_COUNT
