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
Chd|  THNOD_COUNT                   source/output/th/thnod_count.F
Chd|-- called by -----------
Chd|        INIT_TH                       source/output/th/init_th.F    
Chd|-- calls ---------------
Chd|        PLYXFEM_MOD                   share/modules/plyxfem_mod.F   
Chd|====================================================================
      SUBROUTINE THNOD_COUNT(ITHGRP, NTHGRP2, WA_SIZE, INDEX_WA_NOD, ITHBUF, WEIGHT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE PLYXFEM_MOD
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
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr16_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
#include      "submodel.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITHBUF(*),WEIGHT(NUMNOD)
      INTEGER, INTENT(inout) :: WA_SIZE,NTHGRP2
      INTEGER, DIMENSION(2*NTHGRP2+1), INTENT(inout) :: INDEX_WA_NOD
      INTEGER, DIMENSION(NITHGR,*), INTENT(in) :: ITHGRP

C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: BOOL,CONDITION
      INTEGER :: N, I, J, ISK, II, L, K, IUN, IFRA, N1,IPLY,IDIR
      INTEGER :: NN,IAD,IADV,NVAR,ITYP,NITER,J_FIRST
      INTEGER, DIMENSION(NTHGRP2) :: INDEX_NOD

      my_real
     .   XL(3),DL(3),VL(3),AL(3),VRL(3),ARL(3),OD(3),VO(3),AO(3),
     .   VRG(3),ARG(3)
      DATA IUN/1/
C-------------------------
C     NODES
C          DEPLACEMENT, VITESSE, ACCELERATION,
C          VITESSE ANGULAIRE, ACCELERATION ANGULAIRE,
C          & POSITION
C-------------------------
        WA_SIZE = 0
        INDEX_NOD(1:NTHGRP2) = 0

        DO N=1,NTHGRP2
            ITYP=ITHGRP(2,N)
            NN  =ITHGRP(4,N)
            IAD =ITHGRP(5,N)
            NVAR=ITHGRP(6,N)
            IADV=ITHGRP(7,N)
            IF(ITYP==0)THEN
                IF(IRODDL.NE.0)THEN
                    II=0
                    DO J=IAD,IAD+NN-1
                        I=ITHBUF(J)
                        ISK = 1 + ITHBUF(J+NN)
                        CONDITION = (I <= 0)
                        IF(.NOT. CONDITION) CONDITION = (WEIGHT(I) == 0)
                        IF (CONDITION) THEN
                        !   not for me!
                        ELSEIF(ISK.EQ.1)THEN
C---------
C               output with respect to the global SKEW.
                          WA_SIZE = WA_SIZE + NVAR + 1
                        ELSEIF(ISK.LE.NUMSKW+1+NSUBMOD)THEN
!                       output with respect to a (non global) SKEW.
                          WA_SIZE = WA_SIZE + NVAR + 1
                        ELSE    ! ISK==
C---------
C               output with respect to a REFERENCE FRAME.
                            WA_SIZE = WA_SIZE + NVAR + 1
                        ENDIF ! ISK==
                    ENDDO ! J=IAD,IAD+NN-1
                ELSE ! IRODDL/=0
C
                    II=0
                    DO J=IAD,IAD+NN-1
                        I=ITHBUF(J)
                        ISK = 1 + ITHBUF(J+NN)
                        CONDITION = (I <= 0)
                        IF(.NOT. CONDITION) CONDITION = (WEIGHT(I) == 0)
                        IF (CONDITION) THEN
                       !   not for me!
                        ELSEIF(ISK.EQ.1)THEN
C               output with respect to the global SKEW.
                            WA_SIZE = WA_SIZE + NVAR + 1
                        ELSEIF(ISK.LE.NUMSKW+1+NSUBMOD)THEN
C---------
C               output with respect to a (non global) SKEW.
                            WA_SIZE = WA_SIZE + NVAR + 1
                        ELSE
C---------
C                   output with respect to a REFERENCE FRAME.
                            WA_SIZE = WA_SIZE + NVAR + 1
                        ENDIF
                    ENDDO
                ENDIF
            INDEX_NOD(N) = WA_SIZE
            ENDIF
        ENDDO

        J_FIRST = 0
        BOOL = .TRUE.
        DO I=1,NTHGRP2
            IF(BOOL.EQV..TRUE.) THEN
                IF( INDEX_NOD(I)/=0 ) THEN
                    BOOL = .FALSE.
                    J_FIRST = I
                ENDIF
            ENDIF
        ENDDO

        J = 0
        IF(J_FIRST>0) THEN
            J=J+1
            INDEX_WA_NOD(J) = INDEX_NOD(J_FIRST)
            J=J+1
            INDEX_WA_NOD(J) = J_FIRST
            DO I=J_FIRST+1,NTHGRP2
                IF( INDEX_NOD(I)-INDEX_NOD(I-1)>0 ) THEN
                    J=J+1
                    INDEX_WA_NOD(J) = INDEX_NOD(I)
                    J=J+1
                    INDEX_WA_NOD(J) = I
                ENDIF
            ENDDO
        ENDIF
        INDEX_WA_NOD(2*NTHGRP2+1) = J  !   number of non-zero index
C-----------


      RETURN
      END SUBROUTINE THNOD_COUNT
