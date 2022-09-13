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
Chd|  THNOD                         source/output/th/thnod.F      
Chd|-- called by -----------
Chd|        ABFHIST2                      source/output/abf/abfhist2.F  
Chd|        HIST2                         source/output/th/hist2.F      
Chd|-- calls ---------------
Chd|        RELFRAM                       source/tools/skew/relfram.F   
Chd|        PINCHTYPE_MOD                 ../common_source/modules/pinchtype_mod.F
Chd|        PLYXFEM_MOD                   share/modules/plyxfem_mod.F   
Chd|====================================================================
      SUBROUTINE THNOD(ITHBUF ,
     2             WA      ,X       ,D       ,V       ,A      ,
     3             VR      ,AR      ,ISKWN   ,IFRAME  ,SKEW   ,
     4             XFRAME  ,WEIGHT  ,TEMP    ,INOD    ,FTHREAC,
     5             NODREAC, CPTREAC ,DR      ,IFORM   ,NTHGRP2,
     6             ITHGRP ,PINCH_DATA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE PLYXFEM_MOD
      USE PINCHTYPE_MOD
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
      INTEGER CPTREAC,ITHBUF(*),
     .        ISKWN(LISKN,*),IFRAME(LISKN,*),WEIGHT(NUMNOD),INOD(*),
     .        NODREAC(*),IFORM,NTHGRP2
      INTEGER, DIMENSION(NITHGR,*), INTENT(in) :: ITHGRP
      my_real
     .   WA(*),X(3,*),D(3,*),V(3,*),A(3,*),VR(3,*),AR(3,*),
     .   SKEW(LSKEW,*),XFRAME(NXFRAME,*),TEMP(*),FTHREAC(6,*),
     .   DR(3,*)
      TYPE(PINCH) :: PINCH_DATA
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
!       NTHGRP2 : integer ; number of TH group 
!       WA_SIZE : integer ; size of working array for node element
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: CONDITION
      INTEGER I, J, ISK, II, L, K, IUN, IFRA, N1,IPLY,IDIR,N
      INTEGER :: II_SAVE,IJK, ITYP
      INTEGER :: IAD,NN,IADV,NVAR
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
        IJK = 0
        DO N=1,NTHGRP2
            ITYP=ITHGRP(2,N)
            NN  =ITHGRP(4,N)
            IAD =ITHGRP(5,N)
            NVAR=ITHGRP(6,N)
            IADV=ITHGRP(7,N)
            II=0
            IF(ITYP==0)THEN
                IF(IRODDL/=0)THEN
                    II=0
                    DO J=IAD,IAD+NN-1
                        I=ITHBUF(J)
                        ISK = 1 + ITHBUF(J+NN)
                        CONDITION = (I <= 0)
                        IF(.NOT. CONDITION) CONDITION = (WEIGHT(I) == 0)
                        IF (CONDITION) THEN
                            DO L=IADV,IADV+NVAR-1
                                II=II+1
                            ENDDO
                        ELSEIF(ISK==1)THEN
C---------
C               output with respect to the global SKEW.
                          II_SAVE = II
                          DO L=IADV,IADV+NVAR-1
                            K=ITHBUF(L)
                            II=II+1
                            IJK=IJK+1
                            IF (K==1)THEN
                                WA(IJK)=D(1,I)
                            ELSEIF(K==2)THEN
                                WA(IJK)=D(2,I)
                            ELSEIF(K==3)THEN
                                WA(IJK)=D(3,I)
                            ELSEIF(K==4)THEN
                                WA(IJK)=V(1,I)
                            ELSEIF(K==5)THEN
                                WA(IJK)=V(2,I)
                            ELSEIF(K==6)THEN
                                WA(IJK)=V(3,I)
                            ELSEIF(K==7)THEN
                                WA(IJK)=A(1,I)
                            ELSEIF(K==8)THEN
                                WA(IJK)=A(2,I)
                            ELSEIF(K==9)THEN
                                WA(IJK)=A(3,I)
                            ELSEIF(K==10)THEN
                                WA(IJK)=VR(1,I)
                            ELSEIF(K==11)THEN
                                WA(IJK)=VR(2,I)
                            ELSEIF(K==12)THEN
                                WA(IJK)=VR(3,I)
                            ELSEIF(K==13)THEN
                                WA(IJK)=AR(1,I)
                            ELSEIF(K==14)THEN
                                WA(IJK)=AR(2,I)
                            ELSEIF(K==15)THEN
                                WA(IJK)=AR(3,I)
                            ELSEIF(K==16)THEN
                                WA(IJK)=X(1,I)
                            ELSEIF(K==17)THEN
                                WA(IJK)=X(2,I)
                            ELSEIF(K==18)THEN
                                WA(IJK)=X(3,I)
                            ELSEIF(K==19)THEN
C instruction pour contourner bug PGI si thermique
		                        call sync_data(I)
                                IF (ITHERM_FE /= 0) THEN
                                    WA(IJK) = TEMP(I)
                                ELSE
                                    WA(IJK) = ZERO
                                ENDIF
                            ELSEIF(K > 19 .AND. K <= 619) THEN
                                IF(IPLYXFEM > 0) THEN
                                    IDIR = MOD((K - 19),3)
                                    IF(IDIR == 0) IDIR = 3
                                    IPLY = (K - 19)/3
                                    IF(MOD((K - 19),3) /= 0) IPLY = IPLY + 1
                                    WA(IJK) = PLY(IPLY)%U(IDIR,INOD(I))
                                ENDIF
                            ELSEIF(K == 620) THEN
                                IF (NODREAC(I) /= 0) THEN
                                    WA(IJK) = FTHREAC(1,NODREAC(I))
                                ELSE
                                    WA(IJK) = ZERO
                                ENDIF
                            ELSEIF(K == 621) THEN
                                IF (NODREAC(I) /= 0) THEN
                                    WA(IJK) = FTHREAC(2,NODREAC(I))
                                ELSE
                                    WA(IJK) = ZERO
                                ENDIF
                            ELSEIF(K == 622) THEN
                                IF (NODREAC(I) /= 0) THEN
                                    WA(IJK) = FTHREAC(3,NODREAC(I))
                                ELSE
                                    WA(IJK) = ZERO
                                ENDIF
                            ELSEIF(K == 623) THEN
                                IF (NODREAC(I) /= 0) THEN
                                    WA(IJK) = FTHREAC(4,NODREAC(I))
                                ELSE
                                    WA(IJK) = ZERO
                                ENDIF
                            ELSEIF(K == 624) THEN
                                IF (NODREAC(I) /= 0) THEN
                                    WA(IJK) = FTHREAC(5,NODREAC(I))
                                ELSE
                                    WA(IJK) = ZERO
                                ENDIF
                            ELSEIF(K == 625) THEN
                                IF (NODREAC(I) /= 0) THEN
                                    WA(IJK) = FTHREAC(6,NODREAC(I))
                                ELSE
                                    WA(IJK) = ZERO
                                ENDIF
                            ELSEIF(K == 626) THEN
                                IF ((IDROT == 1 .OR. ISECUT>0 .OR. ISUB>0 .OR.
     .                              MSHSUB>0 .OR. IISROT>0 .OR. IMPOSE_DR>0) .AND.
     .                              IRODDL/=0 )THEN
                                    WA(IJK) = DR(1,I)
                                ELSE
                                    WA(IJK) = ZERO
                                ENDIF
                            ELSEIF(K == 627) THEN
                                IF ((IDROT == 1 .OR. ISECUT>0 .OR. ISUB>0 .OR.
     .                               MSHSUB>0 .OR. IISROT>0 .OR. IMPOSE_DR>0) .AND.
     .                               IRODDL/=0 )THEN
                                    WA(IJK) = DR(2,I)
                                ELSE
                                    WA(IJK) = ZERO
                                ENDIF
                            ELSEIF(K == 628) THEN
                                IF ((IDROT == 1 .OR. ISECUT>0 .OR. ISUB>0 .OR.
     .                               MSHSUB>0 .OR. IISROT>0 .OR. IMPOSE_DR>0) .AND.
     .                               IRODDL/=0 )THEN
                                    WA(IJK) = DR(3,I)
                                ELSE
                                    WA(IJK) = ZERO
                                ENDIF
C start of pinching information
                            ELSEIF(K == 629) THEN
                              IF (NPINCH > 0 )THEN
                                WA(IJK) = PINCH_DATA%APINCH(1,I)
                              ELSE
                                WA(IJK) = ZERO
                              ENDIF
                            ELSEIF(K == 630) THEN
                              IF (NPINCH > 0 )THEN
                                WA(IJK) = PINCH_DATA%APINCH(2,I)
                              ELSE
                                WA(IJK) = ZERO
                              ENDIF
                            ELSEIF(K == 631) THEN
                              IF (NPINCH > 0 )THEN
                                WA(IJK) = PINCH_DATA%APINCH(3,I)
                              ELSE
                                WA(IJK) = ZERO
                              ENDIF
                            ELSEIF(K == 632) THEN
                              IF (NPINCH > 0 )THEN
                                WA(IJK) = PINCH_DATA%VPINCH(1,I)
                              ELSE
                                WA(IJK) = ZERO
                              ENDIF
                            ELSEIF(K == 633) THEN
                              IF (NPINCH > 0 )THEN
                                WA(IJK) = PINCH_DATA%VPINCH(2,I)
                              ELSE
                               WA(IJK) = ZERO
                              ENDIF
                            ELSEIF(K == 634) THEN
                              IF (NPINCH > 0 )THEN
                                WA(IJK) = PINCH_DATA%VPINCH(3,I)
                              ELSE
                                WA(IJK) = ZERO
                              ENDIF
                            ELSEIF(K == 635) THEN
                              IF (NPINCH > 0 )THEN
                                WA(IJK) = PINCH_DATA%DPINCH(1,I)
                              ELSE
                                WA(IJK) = ZERO
                              ENDIF
                            ELSEIF(K == 636) THEN
                              IF (NPINCH > 0 )THEN
                                WA(IJK) = PINCH_DATA%DPINCH(2,I)
                              ELSE
                                WA(IJK) = ZERO
                              ENDIF
                            ELSEIF(K == 637) THEN
                              IF (NPINCH > 0 )THEN
                                WA(IJK) = PINCH_DATA%DPINCH(3,I)
                             ELSE
                               WA(IJK) = ZERO
                             ENDIF
C end of pinching information
                            ENDIF   !   K==...
                          ENDDO   ! L=IADV,IADV+NVAR-1
                          IJK=IJK+1
                          WA(IJK) = II_SAVE
                        ELSEIF(ISK<=NUMSKW+1+NSUBMOD)THEN
!                       output with respect to a (non global) SKEW.
                          II_SAVE = II
                          DO L=IADV,IADV+NVAR-1
                            K=ITHBUF(L)
                            II=II+1
                            IJK=IJK+1
                            IF(K==1)THEN
                                WA(IJK) = D(1,I)*SKEW(1,ISK)
     .                                 + D(2,I)*SKEW(2,ISK)
     .                                 + D(3,I)*SKEW(3,ISK)
                            ELSEIF(K==2)THEN
                                WA(IJK) = D(1,I)*SKEW(4,ISK)
     .                                 + D(2,I)*SKEW(5,ISK)
     .                                 + D(3,I)*SKEW(6,ISK)
                            ELSEIF(K==3)THEN
                                WA(IJK) = D(1,I)*SKEW(7,ISK)
     .                                 + D(2,I)*SKEW(8,ISK)
     .                                 + D(3,I)*SKEW(9,ISK)
                            ELSEIF(K==4)THEN
                                WA(IJK) = V(1,I)*SKEW(1,ISK)
     .                                 + V(2,I)*SKEW(2,ISK)
     .                                 + V(3,I)*SKEW(3,ISK)
                            ELSEIF(K==5)THEN
                                WA(IJK) = V(1,I)*SKEW(4,ISK)
     .                                 + V(2,I)*SKEW(5,ISK)
     .                                 + V(3,I)*SKEW(6,ISK)
                            ELSEIF(K==6)THEN
                                WA(IJK) = V(1,I)*SKEW(7,ISK)
     .                                 + V(2,I)*SKEW(8,ISK)
     .                                 + V(3,I)*SKEW(9,ISK)
                            ELSEIF(K==7)THEN
                                WA(IJK) = A(1,I)*SKEW(1,ISK)
     .                                 + A(2,I)*SKEW(2,ISK)
     .                                 + A(3,I)*SKEW(3,ISK)
                            ELSEIF(K==8)THEN
                                WA(IJK) = A(1,I)*SKEW(4,ISK)
     .                                 + A(2,I)*SKEW(5,ISK)
     .                                 + A(3,I)*SKEW(6,ISK)
                            ELSEIF(K==9)THEN
                                WA(IJK) = A(1,I)*SKEW(7,ISK)
     .                                 + A(2,I)*SKEW(8,ISK)
     .                                 + A(3,I)*SKEW(9,ISK)
                            ELSEIF(K==10)THEN
                                WA(IJK) = VR(1,I)*SKEW(1,ISK)
     .                                 + VR(2,I)*SKEW(2,ISK)
     .                                 + VR(3,I)*SKEW(3,ISK)
                            ELSEIF(K==11)THEN
                                WA(IJK) = VR(1,I)*SKEW(4,ISK)
     .                                 + VR(2,I)*SKEW(5,ISK)
     .                                 + VR(3,I)*SKEW(6,ISK)
                            ELSEIF(K==12)THEN
                                WA(IJK) = VR(1,I)*SKEW(7,ISK)
     .                                 + VR(2,I)*SKEW(8,ISK)
     .                                 + VR(3,I)*SKEW(9,ISK)
                            ELSEIF(K==13)THEN
                                WA(IJK) = AR(1,I)*SKEW(1,ISK)
     .                                 + AR(2,I)*SKEW(2,ISK)
     .                                 + AR(3,I)*SKEW(3,ISK)
                            ELSEIF(K==14)THEN
                                WA(IJK) = AR(1,I)*SKEW(4,ISK)
     .                                 + AR(2,I)*SKEW(5,ISK)
     .                                 + AR(3,I)*SKEW(6,ISK)
                            ELSEIF(K==15)THEN
                                WA(IJK) = AR(1,I)*SKEW(7,ISK)
     .                                 + AR(2,I)*SKEW(8,ISK)
     .                                 + AR(3,I)*SKEW(9,ISK)
                            ELSEIF(K==16)THEN
                                WA(IJK) = X(1,I)*SKEW(1,ISK)
     .                                 + X(2,I)*SKEW(2,ISK)
     .                                 + X(3,I)*SKEW(3,ISK)
                            ELSEIF(K==17)THEN
                                WA(IJK) = X(1,I)*SKEW(4,ISK)
     .                                 + X(2,I)*SKEW(5,ISK)
     .                                 + X(3,I)*SKEW(6,ISK)
                            ELSEIF(K==18)THEN
                                WA(IJK) = X(1,I)*SKEW(7,ISK)
     .                                 + X(2,I)*SKEW(8,ISK)
     .                                 + X(3,I)*SKEW(9,ISK)
                            ELSEIF(K==19)THEN
C instruction pour contourner bug PGI si thermique
		                        call sync_data(I)
                            ELSEIF(K == 620) THEN
                                IF (NODREAC(I) /= 0) THEN
                                  WA(IJK) = FTHREAC(1,NODREAC(I))*SKEW(1,ISK)
     .                                   + FTHREAC(2,NODREAC(I))*SKEW(2,ISK)
     .                                   + FTHREAC(3,NODREAC(I))*SKEW(3,ISK)
                                ELSE
                                  WA(IJK) = ZERO
                                ENDIF
                            ELSEIF(K == 621) THEN
                                IF (NODREAC(I) /= 0) THEN
                                  WA(IJK) = FTHREAC(1,NODREAC(I))*SKEW(4,ISK)
     .                                   + FTHREAC(2,NODREAC(I))*SKEW(5,ISK)
     .                                   + FTHREAC(3,NODREAC(I))*SKEW(6,ISK)
                                ELSE
                                  WA(IJK) = ZERO
                                ENDIF
                            ELSEIF(K == 622) THEN
                                IF (NODREAC(I) /= 0) THEN
                                  WA(IJK) = FTHREAC(1,NODREAC(I))*SKEW(7,ISK)
     .                                   + FTHREAC(2,NODREAC(I))*SKEW(8,ISK)
     .                                   + FTHREAC(3,NODREAC(I))*SKEW(9,ISK)
                                ELSE
                                  WA(IJK) = ZERO
                                ENDIF
                            ELSEIF(K == 623) THEN
                                IF (NODREAC(I) /= 0) THEN
                                  WA(IJK) = FTHREAC(4,NODREAC(I))*SKEW(1,ISK)
     .                                   + FTHREAC(5,NODREAC(I))*SKEW(2,ISK)
     .                                   + FTHREAC(6,NODREAC(I))*SKEW(3,ISK)
                                ELSE
                                  WA(IJK) = ZERO
                                ENDIF
                            ELSEIF(K == 624) THEN
                                IF (NODREAC(I) /= 0) THEN
                                  WA(IJK) = FTHREAC(4,NODREAC(I))*SKEW(4,ISK)
     .                                   + FTHREAC(5,NODREAC(I))*SKEW(5,ISK)
     .                                   + FTHREAC(6,NODREAC(I))*SKEW(6,ISK)
                                ELSE
                                  WA(IJK) = ZERO
                                ENDIF
                            ELSEIF(K == 625) THEN
                                IF (NODREAC(I) /= 0) THEN
                                  WA(IJK) = FTHREAC(4,NODREAC(I))*SKEW(7,ISK)
     .                                   + FTHREAC(5,NODREAC(I))*SKEW(8,ISK)
     .                                   + FTHREAC(6,NODREAC(I))*SKEW(9,ISK)
                                ELSE
                                  WA(IJK) = ZERO
                                ENDIF
                            ELSEIF(K == 626) THEN
                                IF ((IDROT == 1 .OR. ISECUT>0 .OR. ISUB>0 .OR.
     .                               MSHSUB>0 .OR. IISROT>0 .OR. IMPOSE_DR>0) .AND.
     .                               IRODDL/=0 )THEN
                                  WA(IJK) = DR(1,I)*SKEW(1,ISK)
     .                                   + DR(2,I)*SKEW(2,ISK)
     .                                   + DR(3,I)*SKEW(3,ISK)
                                ELSE
                                  WA(IJK) = ZERO
                                ENDIF
                            ELSEIF(K == 627) THEN
                                IF ((IDROT == 1 .OR. ISECUT>0 .OR. ISUB>0 .OR.
     .                               MSHSUB>0 .OR. IISROT>0 .OR. IMPOSE_DR>0) .AND.
     .                               IRODDL/=0 )THEN
                                  WA(IJK) = DR(1,I)*SKEW(4,ISK)
     .                                   + DR(2,I)*SKEW(5,ISK)
     .                                   + DR(3,I)*SKEW(6,ISK)
                                ELSE
                                  WA(IJK) = ZERO
                                ENDIF
                            ELSEIF(K == 628) THEN
                                IF ((IDROT == 1 .OR. ISECUT>0 .OR. ISUB>0 .OR.
     .                               MSHSUB>0 .OR. IISROT>0 .OR. IMPOSE_DR>0) .AND.
     .                               IRODDL/=0 )THEN
                                  WA(IJK) = DR(1,I)*SKEW(7,ISK)
     .                                   + DR(2,I)*SKEW(8,ISK)
     .                                   + DR(3,I)*SKEW(9,ISK)
                                ELSE
                                  WA(IJK) = ZERO
                                ENDIF
C start of pinching information
                            ELSEIF(K == 629) THEN
                              IF (NPINCH > 0 )THEN
                                WA(IJK) = PINCH_DATA%APINCH(1,I)*SKEW(1,ISK)
     .                                   +PINCH_DATA%APINCH(2,I)*SKEW(2,ISK)
     .                                   +PINCH_DATA%APINCH(3,I)*SKEW(3,ISK)
                              ELSE
                                WA(IJK) = ZERO
                              ENDIF
                            ELSEIF(K == 630) THEN
                              IF (NPINCH > 0 )THEN
                                WA(IJK) = PINCH_DATA%APINCH(1,I)*SKEW(4,ISK)
     .                                   +PINCH_DATA%APINCH(2,I)*SKEW(5,ISK)
     .                                   +PINCH_DATA%APINCH(3,I)*SKEW(6,ISK)
                              ELSE
                                WA(IJK) = ZERO
                              ENDIF
                            ELSEIF(K == 631) THEN
                              IF (NPINCH > 0 )THEN
                                WA(IJK) = PINCH_DATA%APINCH(1,I)*SKEW(7,ISK)
     .                                   +PINCH_DATA%APINCH(2,I)*SKEW(8,ISK)
     .                                   +PINCH_DATA%APINCH(3,I)*SKEW(9,ISK)
                              ELSE
                                WA(IJK) = ZERO
                              ENDIF
                            ELSEIF(K == 632) THEN
                              IF (NPINCH > 0 )THEN
                                WA(IJK) = PINCH_DATA%VPINCH(1,I)*SKEW(1,ISK)
     .                                   +PINCH_DATA%VPINCH(2,I)*SKEW(2,ISK)
     .                                   +PINCH_DATA%VPINCH(3,I)*SKEW(3,ISK)
                              ELSE
                                WA(IJK) = ZERO
                              ENDIF
                            ELSEIF(K == 633) THEN
                              IF (NPINCH > 0 )THEN
                                WA(IJK) = PINCH_DATA%VPINCH(1,I)*SKEW(4,ISK)
     .                                   +PINCH_DATA%VPINCH(2,I)*SKEW(5,ISK)
     .                                   +PINCH_DATA%VPINCH(3,I)*SKEW(6,ISK)
                              ELSE
                                WA(IJK) = ZERO
                              ENDIF
                            ELSEIF(K == 634) THEN
                              IF (NPINCH > 0 )THEN
                                WA(IJK) = PINCH_DATA%VPINCH(1,I)*SKEW(7,ISK)
     .                                   +PINCH_DATA%VPINCH(2,I)*SKEW(8,ISK)
     .                                   +PINCH_DATA%VPINCH(3,I)*SKEW(9,ISK)
                              ELSE
                                WA(IJK) = ZERO
                              ENDIF
                            ELSEIF(K == 635) THEN
                              IF (NPINCH > 0 )THEN
                                WA(IJK) = PINCH_DATA%DPINCH(1,I)*SKEW(1,ISK)
     .                                   +PINCH_DATA%DPINCH(2,I)*SKEW(2,ISK)
     .                                   +PINCH_DATA%DPINCH(3,I)*SKEW(3,ISK)
                              ELSE
                                WA(IJK) = ZERO
                              ENDIF
                            ELSEIF(K == 636) THEN
                              IF (NPINCH > 0 )THEN
                                WA(IJK) = PINCH_DATA%DPINCH(1,I)*SKEW(4,ISK)
     .                                   +PINCH_DATA%DPINCH(2,I)*SKEW(5,ISK)
     .                                   +PINCH_DATA%DPINCH(3,I)*SKEW(6,ISK)
                              ELSE
                                WA(IJK) = ZERO
                              ENDIF
                            ELSEIF(K == 637) THEN
                              IF (NPINCH > 0 )THEN
                                WA(IJK) = PINCH_DATA%DPINCH(1,I)*SKEW(7,ISK)
     .                                   +PINCH_DATA%DPINCH(2,I)*SKEW(8,ISK)
     .                                   +PINCH_DATA%DPINCH(3,I)*SKEW(9,ISK)
                              ELSE
                                WA(IJK) = ZERO
                              ENDIF
C end of pinching information
                            ENDIF
                          ENDDO
                          IJK=IJK+1
                          WA(IJK) = II_SAVE
                        ELSE    ! ISK==
C---------
C               output with respect to a REFERENCE FRAME.
                            IFRA=ISK-(NUMSKW+1+NSUBMOD)-MIN(IUN,NSPCOND)*NUMSPH
                            CALL RELFRAM(
     1                          X(1,I) ,D(1,I) ,V(1,I) ,A(1,I) ,VR(1,I) ,
     2                          AR(1,I) ,XFRAME(1,IFRA),XFRAME(10,IFRA),
     .                          XFRAME(34,IFRA) ,XFRAME(31,IFRA) ,
     3                          XFRAME(28,IFRA) ,XL ,DL ,VL ,AL ,
     4                          VRL ,ARL )
                            II_SAVE = II
                            DO L=IADV,IADV+NVAR-1
                                K=ITHBUF(L)
                                II=II+1
                                IJK=IJK+1
                                IF (K==1)THEN
                                    WA(IJK)=DL(1)
                                ELSEIF(K==2)THEN
                                    WA(IJK)=DL(2)
                                ELSEIF(K==3)THEN
                                    WA(IJK)=DL(3)
                                ELSEIF(K==4)THEN
                                    WA(IJK)=VL(1)
                                ELSEIF(K==5)THEN
                                    WA(IJK)=VL(2)
                                ELSEIF(K==6)THEN
                                    WA(IJK)=VL(3)
                                ELSEIF(K==7)THEN
                                    WA(IJK)=AL(1)
                                ELSEIF(K==8)THEN
                                    WA(IJK)=AL(2)
                                ELSEIF(K==9)THEN
                                    WA(IJK)=AL(3)
                                ELSEIF(K==10)THEN
                                    WA(IJK)=VRL(1)
                                ELSEIF(K==11)THEN
                                    WA(IJK)=VRL(2)
                                ELSEIF(K==12)THEN
                                    WA(IJK)=VRL(3)
                                ELSEIF(K==13)THEN
                                    WA(IJK)=ARL(1)
                                ELSEIF(K==14)THEN
                                    WA(IJK)=ARL(2)
                                ELSEIF(K==15)THEN
                                    WA(IJK)=ARL(3)
                                ELSEIF(K==16)THEN
                                    WA(IJK)=XL(1)
                                ELSEIF(K==17)THEN
                                    WA(IJK)=XL(2)
                                ELSEIF(K==18)THEN
                                    WA(IJK)=XL(3)
                                ELSEIF(K==19)THEN
C instruction pour contourner bug PGI si thermique
		                            call sync_data(I)
                                    IF (ITHERM_FE /= 0) THEN
                                        WA(IJK) = TEMP(I)
                                    ELSE
                                        WA(IJK) = ZERO
                                    ENDIF
                                ENDIF
                            ENDDO
                            IJK=IJK+1
                            WA(IJK) = II_SAVE
                        ENDIF ! ISK==
                    ENDDO ! J=IAD,IAD+NN-1
                ELSE ! IRODDL/=0
                    VRG(1)=ZERO
                    VRG(2)=ZERO
                    VRG(3)=ZERO
                    ARG(1)=ZERO
                    ARG(2)=ZERO
                    ARG(3)=ZERO
C
                    II=0                    
                    DO J=IAD,IAD+NN-1
                        I=ITHBUF(J)
                        ISK = 1 + ITHBUF(J+NN)
                        CONDITION = (I <= 0)
                        IF(.NOT. CONDITION) CONDITION = (WEIGHT(I) == 0)
                        IF (CONDITION) THEN
                            DO L=IADV,IADV+NVAR-1
                                II=II+1
                            ENDDO
                        ELSEIF(ISK==1)THEN
C               output with respect to the global SKEW.
                            II_SAVE = II
                            DO L=IADV,IADV+NVAR-1
                                K=ITHBUF(L)
                                II=II+1
                                IJK=IJK+1
                                IF (K==1)THEN
                                    WA(IJK)=D(1,I)
                                ELSEIF(K==2)THEN
                                    WA(IJK)=D(2,I)
                                ELSEIF(K==3)THEN
                                    WA(IJK)=D(3,I)
                                ELSEIF(K==4)THEN
                                    WA(IJK)=V(1,I)
                                ELSEIF(K==5)THEN
                                    WA(IJK)=V(2,I)
                                ELSEIF(K==6)THEN
                                    WA(IJK)=V(3,I)
                                ELSEIF(K==7)THEN
                                    WA(IJK)=A(1,I)
                                ELSEIF(K==8)THEN
                                    WA(IJK)=A(2,I)
                                ELSEIF(K==9)THEN
                                    WA(IJK)=A(3,I)
                                ELSEIF(K==16)THEN
                                    WA(IJK)=X(1,I)
                                ELSEIF(K==17)THEN
                                    WA(IJK)=X(2,I)
                                ELSEIF(K==18)THEN
                                    WA(IJK)=X(3,I)
                                ELSEIF(K==19)THEN
C instruction pour contourner bug PGI si thermique
		                            call sync_data(I)
                                    IF (ITHERM_FE /= 0) THEN
                                        WA(IJK) = TEMP(I)
                                    ELSE
                                        WA(IJK) = ZERO
                                    ENDIF
                                ELSEIF(K == 620) THEN
                                    IF (NODREAC(I) /= 0) THEN
                                        WA(IJK) = FTHREAC(1,NODREAC(I))
                                    ELSE
                                        WA(IJK) = ZERO
                                    ENDIF
                                ELSEIF(K == 621) THEN
                                    IF (NODREAC(I) /= 0) THEN
                                        WA(IJK) = FTHREAC(2,NODREAC(I))
                                    ELSE
                                        WA(IJK) = ZERO
                                    ENDIF
                                ELSEIF(K == 622) THEN
                                    IF (NODREAC(I) /= 0) THEN
                                        WA(IJK) = FTHREAC(3,NODREAC(I))
                                    ELSE
                                        WA(IJK) = ZERO
                                    ENDIF
                                ELSEIF(K == 623) THEN
                                    IF (NODREAC(I) /= 0) THEN
                                        WA(IJK) = FTHREAC(4,NODREAC(I))
                                    ELSE
                                        WA(IJK) = ZERO
                                    ENDIF
                                ELSEIF(K == 624) THEN
                                    IF (NODREAC(I) /= 0) THEN
                                        WA(IJK) = FTHREAC(5,NODREAC(I))
                                    ELSE
                                        WA(IJK) = ZERO
                                    ENDIF
                                ELSEIF(K == 625) THEN
                                    IF (NODREAC(I) /= 0) THEN
                                        WA(IJK) = FTHREAC(6,NODREAC(I))
                                    ELSE
                                        WA(IJK) = ZERO
                                    ENDIF
                                ELSEIF(K == 626) THEN
                                    IF ((IDROT == 1 .OR. ISECUT>0 .OR. ISUB>0 .OR.
     .                                   MSHSUB>0 .OR. IISROT>0 .OR. IMPOSE_DR>0) .AND.
     .                                   IRODDL/=0 )THEN
                                        WA(IJK) = DR(1,I)
                                    ELSE
                                        WA(IJK) = ZERO
                                    ENDIF
                                ELSEIF(K == 627) THEN
                                    IF ((IDROT == 1 .OR. ISECUT>0 .OR. ISUB>0 .OR.
     .                                   MSHSUB>0 .OR. IISROT>0 .OR. IMPOSE_DR>0) .AND.
     .                                   IRODDL/=0 )THEN
                                        WA(IJK) = DR(2,I)
                                    ELSE
                                        WA(IJK) = ZERO
                                    ENDIF
                                ELSEIF(K == 628) THEN
                                    IF ((IDROT == 1 .OR. ISECUT>0 .OR. ISUB>0 .OR.
     .                                   MSHSUB>0 .OR. IISROT>0 .OR. IMPOSE_DR>0) .AND.
     .                                   IRODDL/=0 )THEN
                                        WA(IJK) = DR(3,I)
                                    ELSE
                                        WA(IJK) = ZERO
                                    ENDIF
                                ELSE
                                    WA(IJK)=0.
                                ENDIF
                            ENDDO ! L=IADV,IADV+NVAR-1
                            IJK=IJK+1
                            WA(IJK) = II_SAVE
                        ELSEIF(ISK<=NUMSKW+1+NSUBMOD)THEN
C---------
C               output with respect to a (non global) SKEW.
                            II_SAVE=II
                            DO L=IADV,IADV+NVAR-1
                                K=ITHBUF(L)
                                II=II+1
                                IJK=IJK+1
                                IF(K==1)THEN
                                    WA(IJK) = D(1,I)*SKEW(1,ISK)
     .                                     + D(2,I)*SKEW(2,ISK)
     .                                     + D(3,I)*SKEW(3,ISK)
                                ELSEIF(K==2)THEN
                                    WA(IJK) = D(1,I)*SKEW(4,ISK)
     .                                     + D(2,I)*SKEW(5,ISK)
     .                                     + D(3,I)*SKEW(6,ISK)
                                ELSEIF(K==3)THEN
                                    WA(IJK) = D(1,I)*SKEW(7,ISK)
     .                                     + D(2,I)*SKEW(8,ISK)
     .                                     + D(3,I)*SKEW(9,ISK)
                                ELSEIF(K==4)THEN
                                    WA(IJK) = V(1,I)*SKEW(1,ISK)
     .                                     + V(2,I)*SKEW(2,ISK)
     .                                     + V(3,I)*SKEW(3,ISK)
                                ELSEIF(K==5)THEN
                                    WA(IJK) = V(1,I)*SKEW(4,ISK)
     .                                     + V(2,I)*SKEW(5,ISK)
     .                                     + V(3,I)*SKEW(6,ISK)
                                ELSEIF(K==6)THEN
                                    WA(IJK) = V(1,I)*SKEW(7,ISK)
     .                                     + V(2,I)*SKEW(8,ISK)
     .                                     + V(3,I)*SKEW(9,ISK)
                                ELSEIF(K==7)THEN
                                    WA(IJK) = A(1,I)*SKEW(1,ISK)
     .                                     + A(2,I)*SKEW(2,ISK)
     .                                     + A(3,I)*SKEW(3,ISK)
                                ELSEIF(K==8)THEN
                                    WA(IJK) = A(1,I)*SKEW(4,ISK)
     .                                     + A(2,I)*SKEW(5,ISK)
     .                                     + A(3,I)*SKEW(6,ISK)
                                ELSEIF(K==9)THEN
                                    WA(IJK) = A(1,I)*SKEW(7,ISK)
     .                                     + A(2,I)*SKEW(8,ISK)
     .                                     + A(3,I)*SKEW(9,ISK)
                                ELSEIF(K==16)THEN
                                    WA(IJK) = X(1,I)*SKEW(1,ISK)
     .                                     + X(2,I)*SKEW(2,ISK)
     .                                     + X(3,I)*SKEW(3,ISK)
                                ELSEIF(K==17)THEN
                                    WA(IJK) = X(1,I)*SKEW(4,ISK)
     .                                     + X(2,I)*SKEW(5,ISK)
     .                                     + X(3,I)*SKEW(6,ISK)
                                ELSEIF(K==18)THEN
                                    WA(IJK) = X(1,I)*SKEW(7,ISK)
     .                                     + X(2,I)*SKEW(8,ISK)
     .                                     + X(3,I)*SKEW(9,ISK)
                                ELSEIF(K==19)THEN
C instruction pour contourner bug PGI si thermique
		                            call sync_data(I)
                                    IF (ITHERM_FE /= 0) THEN
                                        WA(IJK) = TEMP(I)
                                    ELSE
                                        WA(IJK) = ZERO
                                    ENDIF
                                ELSEIF(K == 620) THEN
                                    IF (NODREAC(I) /= 0) THEN
                                      WA(IJK) = FTHREAC(1,NODREAC(I))*SKEW(1,ISK)
     .                                       + FTHREAC(2,NODREAC(I))*SKEW(2,ISK)
     .                                       + FTHREAC(3,NODREAC(I))*SKEW(3,ISK)
                                    ELSE
                                      WA(IJK) = ZERO
                                    ENDIF
                                ELSEIF(K == 621) THEN
                                    IF (NODREAC(I) /= 0) THEN
                                      WA(IJK) = FTHREAC(1,NODREAC(I))*SKEW(4,ISK)
     .                                       + FTHREAC(2,NODREAC(I))*SKEW(5,ISK)
     .                                       + FTHREAC(3,NODREAC(I))*SKEW(6,ISK)
                                    ELSE
                                      WA(IJK) = ZERO
                                    ENDIF
                                ELSEIF(K == 622) THEN
                                    IF (NODREAC(I) /= 0) THEN
                                      WA(IJK) = FTHREAC(1,NODREAC(I))*SKEW(7,ISK)
     .                                       + FTHREAC(2,NODREAC(I))*SKEW(8,ISK)
     .                                       + FTHREAC(3,NODREAC(I))*SKEW(9,ISK)
                                    ELSE
                                      WA(IJK) = ZERO
                                    ENDIF
                                ELSEIF(K == 623) THEN
                                    IF (NODREAC(I) /= 0) THEN
                                      WA(IJK) = FTHREAC(4,NODREAC(I))*SKEW(1,ISK)
     .                                       + FTHREAC(5,NODREAC(I))*SKEW(2,ISK)
     .                                       + FTHREAC(6,NODREAC(I))*SKEW(3,ISK)
                                    ELSE
                                      WA(IJK) = ZERO
                                    ENDIF
                                ELSEIF(K == 624) THEN
                                    IF (NODREAC(I) /= 0) THEN
                                      WA(IJK) = FTHREAC(4,NODREAC(I))*SKEW(4,ISK)
     .                                       + FTHREAC(5,NODREAC(I))*SKEW(5,ISK)
     .                                       + FTHREAC(6,NODREAC(I))*SKEW(6,ISK)
                                    ELSE
                                      WA(IJK) = ZERO
                                    ENDIF
                                ELSEIF(K == 625) THEN
                                    IF (NODREAC(I) /= 0) THEN
                                        WA(IJK) = FTHREAC(4,NODREAC(I))*SKEW(7,ISK)
     .                                       + FTHREAC(5,NODREAC(I))*SKEW(8,ISK)
     .                                       + FTHREAC(6,NODREAC(I))*SKEW(9,ISK)
                                    ELSE
                                        WA(IJK) = ZERO
                                    ENDIF
                                ELSEIF(K == 626) THEN
                                    IF ((IDROT == 1 .OR. ISECUT>0 .OR. ISUB>0 .OR.
     .                                  MSHSUB>0 .OR. IISROT>0 .OR. IMPOSE_DR>0) .AND.
     .                                  IRODDL/=0 )THEN
                                        WA(IJK) = DR(1,I)*SKEW(1,ISK)
     .                                  + DR(2,I)*SKEW(2,ISK)
     .                                  + DR(3,I)*SKEW(3,ISK)
                                    ELSE
                                        WA(IJK) = ZERO
                                    ENDIF
                                ELSEIF(K == 627) THEN
                                    IF ((IDROT == 1 .OR. ISECUT>0 .OR. ISUB>0 .OR.
     .                                   MSHSUB>0 .OR. IISROT>0 .OR. IMPOSE_DR>0) .AND.
     .                                   IRODDL/=0 )THEN
                                      WA(IJK) = DR(1,I)*SKEW(4,ISK)
     .                                       + DR(2,I)*SKEW(5,ISK)
     .                                       + DR(3,I)*SKEW(6,ISK)
                                    ELSE
                                      WA(IJK) = ZERO
                                    ENDIF
                                ELSEIF(K == 628) THEN
                                    IF ((IDROT == 1 .OR. ISECUT>0 .OR. ISUB>0 .OR.
     .                                  MSHSUB>0 .OR. IISROT>0 .OR. IMPOSE_DR>0) .AND.
     .                                  IRODDL/=0 )THEN
                                      WA(IJK) = DR(1,I)*SKEW(7,ISK)
     .                                       + DR(2,I)*SKEW(8,ISK)
     .                                     + DR(3,I)*SKEW(9,ISK)
                                    ELSE
                                      WA(IJK) = ZERO
                                    ENDIF
                                ELSE
                                    WA(IJK)=0.
                                ENDIF
                            ENDDO
                            IJK=IJK+1
                            WA(IJK)=II_SAVE
                        ELSE
C---------
C                   output with respect to a REFERENCE FRAME.
                    IFRA=ISK-(NUMSKW+1+NSUBMOD)-MIN(IUN,NSPCOND)*NUMSPH
                    CALL RELFRAM(
     1              X(1,I) ,D(1,I) ,V(1,I) ,A(1,I) ,VRG  ,
     2              ARG ,  XFRAME(1,IFRA),XFRAME(10,IFRA),
     .              XFRAME(34,IFRA) ,XFRAME(31,IFRA) ,
     3              XFRAME(28,IFRA) ,XL ,DL ,VL ,AL ,
     4              VRL ,ARL )
                    II_SAVE = II 
                    DO L=IADV,IADV+NVAR-1
                        K=ITHBUF(L)
                        II=II+1
                        IJK=IJK+1
                        IF (K==1)THEN
                            WA(IJK)=DL(1)
                        ELSEIF(K==2)THEN
                            WA(IJK)=DL(2)
                        ELSEIF(K==3)THEN
                            WA(IJK)=DL(3)
                        ELSEIF(K==4)THEN
                            WA(IJK)=VL(1)
                        ELSEIF(K==5)THEN
                            WA(IJK)=VL(2)
                        ELSEIF(K==6)THEN
                                WA(IJK)=VL(3)
                        ELSEIF(K==7)THEN
                                WA(IJK)=AL(1)
                        ELSEIF(K==8)THEN
                                WA(IJK)=AL(2)
                        ELSEIF(K==9)THEN
                                WA(IJK)=AL(3)
                        ELSEIF(K==16)THEN
                                WA(IJK)=XL(1)
                        ELSEIF(K==17)THEN
                                WA(IJK)=XL(2)
                        ELSEIF(K==18)THEN
                                WA(IJK)=XL(3)
                        ELSEIF(K==19)THEN
C instruction pour contourner bug PGI si thermique
		                    call sync_data(I)
                            IF (ITHERM_FE /= 0) THEN
                                WA(IJK) = TEMP(I)
                            ELSE
                                WA(IJK) = ZERO
                            ENDIF
                        ELSE
                            WA(IJK)=0.
                        ENDIF
                    ENDDO
                    IJK=IJK+1
                    WA(IJK) = II_SAVE

              ENDIF
            ENDDO
       ENDIF
       ENDIF
       ENDDO
C-------------------------
      RETURN
      END SUBROUTINE THNOD
