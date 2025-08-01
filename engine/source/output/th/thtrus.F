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
!||    thtrus         ../engine/source/output/th/thtrus.F
!||--- called by ------------------------------------------------------
!||    hist2          ../engine/source/output/th/hist2.F
!||--- uses       -----------------------------------------------------
!||    elbufdef_mod   ../common_source/modules/mat_elem/elbufdef_mod.F90
!||====================================================================
      SUBROUTINE THTRUS(IPARG,NTHGRP2 , ITHGRP ,
     .                  ITHBUF ,ELBUF_TAB,WA   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
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
      INTEGER, DIMENSION(NITHGR,*), INTENT(in) :: ITHGRP
      my_real
     .   WA(*)
C
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II,I,N,IH,NG,ITY,MTE,K,L,LWA,NEL,NFT,J,IP
      INTEGER :: NITER,IAD,NN,IADV,NVAR,ITYP,IJK
C
      TYPE(G_BUFEL_)  ,POINTER :: GBUF
C-----------------------------------------------
C-------------------------
C           ELEMENTS BARRES
C-------------------------
        IJK = 0
        DO NITER=1,NTHGRP2
            ITYP=ITHGRP(2,NITER)
            NN  =ITHGRP(4,NITER)
            IAD =ITHGRP(5,NITER)
            NVAR=ITHGRP(6,NITER)
            IADV=ITHGRP(7,NITER)
            II=0
            IF(ITYP==4)THEN
!   -------------------------------
                II=0
                IH=IAD
                DO WHILE (ITHBUF(IH+NN) /= ISPMD .AND. IH < IAD+NN)
                    IH = IH + 1
                ENDDO
                IF (IH >= IAD+NN) GOTO 666 
                DO NG=1,NGROUP
                    ITY=IPARG(5,NG)
                    GBUF => ELBUF_TAB(NG)%GBUF
                    IF (ITY == 4) THEN
                        MTE=IPARG(1,NG)
                        NEL=IPARG(2,NG)
                        NFT=IPARG(3,NG)
                        DO I=1,NEL
                            N=I+NFT
                            K=ITHBUF(IH)
                            IP=ITHBUF(IH+NN)
                            IF (K == N) THEN
                                IH=IH+1
                                II = ((IH-1) - IAD)*NVAR
                                DO WHILE (ITHBUF(IH+NN) /= ISPMD .AND. IH < IAD+NN)
                                  IH = IH + 1
                                ENDDO
                                IF (IH > IAD+NN) GOTO 666
                                DO L=IADV,IADV+NVAR-1
                                    K=ITHBUF(L)
                                    IJK=IJK+1
                                    IF (ITHBUF(L) == 1) THEN
                                        WA(IJK)=GBUF%OFF(I)
                                    ELSEIF (ITHBUF(L) == 2) THEN
                                        WA(IJK)=GBUF%FOR(I)
                                    ELSEIF (ITHBUF(L) == 3) THEN
                                        WA(IJK)=GBUF%EINT(I)
                                    ELSEIF (ITHBUF(L) == 4) THEN
                                        WA(IJK)=GBUF%AREA(I)
                                    ELSEIF (ITHBUF(L) == 5) THEN
                                        WA(IJK)=GBUF%LENGTH(I)
                                    ELSEIF (ITHBUF(L) == 6) THEN
                                        IF (MTE == 1) THEN
                                            WA(IJK)=ZERO
                                        ELSE
                                            WA(IJK)=GBUF%PLA(I)
                                        ENDIF
                                    ENDIF ! IF (K == 1)
                                ENDDO ! DO L=IADV,IADV+NVAR-1
                                IJK = IJK + 1
                                WA(IJK) = II
                            ENDIF ! IF (K == N)
                        ENDDO ! DO I=1,NEL
                    ENDIF ! IF (ITY == 4)
                ENDDO ! DO NG=1,NGROUP
 666    continue
!   -------------------------------
            ENDIF
        ENDDO
C---
      RETURN
      END
