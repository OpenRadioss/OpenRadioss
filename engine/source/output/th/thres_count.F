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
Chd|  THRES_COUNT                   source/output/th/thres_count.F
Chd|-- called by -----------
Chd|        INIT_TH                       source/output/th/init_th.F    
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE THRES_COUNT(IPARG,ITHBUF,ELBUF_TAB,IGEO,IXR, 
     .                       ITHGRP,NTHGRP2,WA_SIZE,INDEX_WA_SPRING)
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
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "scr03_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),ITHBUF(*),IXR(NIXR,*),
     .   IGEO(NPROPGI,*),ITHGRP(NITHGR,*),NTHGRP2
      INTEGER, INTENT(inout) :: WA_SIZE
      INTEGER, DIMENSION(2*NTHGRP2+1), INTENT(inout) :: INDEX_WA_SPRING
C
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
!       NTHGRP2 : integer ; number of TH group 
!       WA_SIZE : integer ; size of working array for spring element
!       INDEX_WA_SPRING : integer ; dimension=NTHGRP2
!               local index of WA array, sent to PROC0 
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: BOOL
      INTEGER :: II,I,N,IH,NG,ITY,MTE,K,IP,L,
     .   LWA,NEL,NFT,IPROP,IGTYP,J,JJ(6)
      INTEGER :: NN,IAD,IADV,NVAR,ITYP,NITER,J_FIRST
      INTEGER, DIMENSION(NTHGRP2) :: INDEX_RESSORT

      my_real
     . WWA(100)
      my_real
     . V1,V2,V3,E1X,E1Y,E1Z,E2X,E2Y,E2Z,E3X,E3Y,E3Z
C
      TYPE(G_BUFEL_)  ,POINTER :: GBUF
!$COMMENT
!       THRES_COUNT description
!       count the size of working array for 
!       spring element
!       
!       THRES_COUNT organization :
!       loop over the NTHGRP2 TH group and
!       if a group is a spring group, then :
!           - add the size of the group NVAR to the 
!             global size WA_SIZE (WA_SIZE=WA_SIZE+NVAR)
!           - add another case for the local position
!             WA_SIZE = WA_SIZE + 1
!             the local position is usefull to build 
!             the glaobl index SPRING_STRUCT(I)%TH_SPRING on PROC0
!$ENDCOMMENT

C-----------------------------------------------
C           ELEMENTS RESSORTS
C-----------------------------------------------
        WA_SIZE = 0
        INDEX_RESSORT(1:NTHGRP2) = 0

        DO NITER=1,NTHGRP2

          ITYP=ITHGRP(2,NITER)
          NN  =ITHGRP(4,NITER)
          IAD =ITHGRP(5,NITER)
          NVAR=ITHGRP(6,NITER)
          IADV=ITHGRP(7,NITER)

          IH=IAD
          IF(ITYP==6) THEN
Cel specifique spmd
            IF (IMACH == 3) THEN
C decalage IH
                DO WHILE (ITHBUF(IH+NN) /= ISPMD .AND. IH < IAD+NN)
                    IH = IH + 1
                ENDDO
                IF (IH >= IAD+NN) GOTO 666
            ENDIF
C
            DO NG=1,NGROUP
                ITY=IPARG(5,NG)
                GBUF => ELBUF_TAB(NG)%GBUF
                IF (ITY == 6) THEN
                    NFT=IPARG(3,NG)
                    NFT=IPARG(3,NG)
                    IPROP = IXR(1,NFT+1)
                    IGTYP = IGEO(11,IPROP)
                    MTE=IPARG(1,NG)
                    NEL=IPARG(2,NG)
C
                    DO K=1,6
                        JJ(K) = (K-1)*NEL + 1
                    ENDDO
C
                    IF (IGTYP == 4) THEN
                        DO I=1,NEL
                            N=I+NFT
                            K=ITHBUF(IH)
                            IP=ITHBUF(IH+NN)
C
                            IF (K == N) THEN
                                IH=IH+1
Cel traitement specifique spmd
                                IF (IMACH == 3) THEN
C recherche du ii correct
                                    II = ((IH-1) - IAD)*NVAR
                                    DO WHILE (ITHBUF(IH+NN) /= ISPMD .AND. IH < IAD+NN)
                                        IH = IH + 1
                                    ENDDO
                                ENDIF
C
                                IF (IH > IAD+NN) GOTO 666
                                WA_SIZE = WA_SIZE + NVAR + 1 
                            ENDIF
                        ENDDO
                    ELSEIF (IGTYP == 26) THEN
                        DO I=1,NEL
                            N=I+NFT
                            K=ITHBUF(IH)
                            IP=ITHBUF(IH+NN)
C
                            IF (K == N) THEN
                                IH=IH+1
                                IF (IMACH == 3) THEN
C recherche du ii correct
                                    II = ((IH-1) - IAD)*NVAR
                                    DO WHILE (ITHBUF(IH+NN) /= ISPMD .AND. IH < IAD+NN)
                                        IH = IH + 1
                                    ENDDO
                                ENDIF
C
                                IF (IH > IAD+NN) GOTO 666
                                WA_SIZE = WA_SIZE + NVAR + 1
                            ENDIF
                        ENDDO
                    ELSEIF (IGTYP == 27) THEN
                        DO I=1,NEL
                            N=I+NFT
                            K=ITHBUF(IH)
                            IP=ITHBUF(IH+NN)
C
                            IF (K == N) THEN
                                IH=IH+1
                                IF (IMACH == 3) THEN
C recherche du ii correct
                                    II = ((IH-1) - IAD)*NVAR
                                    DO WHILE (ITHBUF(IH+NN) /= ISPMD .AND. IH < IAD+NN)
                                        IH = IH + 1
                                    ENDDO
                                ENDIF
C
                                IF (IH > IAD+NN) GOTO 666
                                WA_SIZE = WA_SIZE + NVAR + 1
                            ENDIF
                        ENDDO
                    ELSEIF( IGTYP == 12) THEN
                        DO I=1,NEL
                            N=I+NFT
                            K=ITHBUF(IH)
                            IP=ITHBUF(IH+NN)
C
                            IF (K == N) THEN
                                IH=IH+1
                                IF (IMACH == 3) THEN
                                    II = ((IH-1) - IAD)*NVAR
                                    DO WHILE (ITHBUF(IH+NN) /= ISPMD .AND. IH < IAD+NN)
                                        IH = IH + 1
                                    ENDDO
                                ENDIF
C
                                IF (IH > IAD+NN) GOTO 666
                                WA_SIZE = WA_SIZE + NVAR + 1 
                            ENDIF
                        ENDDO
                    ELSEIF (IGTYP == 8 .OR. IGTYP == 13 .OR. IGTYP == 25 
     .                                .OR. IGTYP == 23 ) THEN
                        DO I=1,NEL
                            N=I+NFT
                            K=ITHBUF(IH)
                            IP=ITHBUF(IH+NN)
C
                            IF (K == N) THEN
                                IH=IH+1
Cel traitement specifique spmd
                                IF (IMACH == 3) THEN
C recherche du ii correct
                                    II = ((IH-1) - IAD)*NVAR
                                    DO WHILE (ITHBUF(IH+NN) /= ISPMD .AND. IH < IAD+NN)
                                        IH = IH + 1
                                    ENDDO
                                ENDIF
C
                                IF (IH > IAD+NN) GOTO 666
                                WA_SIZE = WA_SIZE + NVAR + 1
                            ENDIF
                        ENDDO
                    ELSEIF (IGTYP >= 29) THEN
                            IF (IGTYP <= 31 .OR. IGTYP == 35 .OR. IGTYP == 36. OR. 
     .                      IGTYP == 44) THEN
                                DO I=1,NEL
                                    N=I+NFT
                                    K=ITHBUF(IH)
                                    IP=ITHBUF(IH+NN)
C
                                    IF (K == N) THEN
                                        IH=IH+1
Cel traitement specifique spmd
                                        IF (IMACH == 3) THEN
C recherche du ii correct
                                            II = ((IH-1) - IAD)*NVAR
                                            DO WHILE (ITHBUF(IH+NN) /= ISPMD .AND. IH.LT.IAD+NN)
                                                IH = IH + 1
                                            ENDDO
                                        ENDIF
C
                                        IF (IH > IAD+NN) GOTO 666
                                        WA_SIZE = WA_SIZE + NVAR + 1
                                    ENDIF
                                ENDDO
                            ELSEIF (IGTYP == 32) THEN
                                DO I=1,NEL
                                    N=I+NFT
                                    K=ITHBUF(IH)
                                    IP=ITHBUF(IH+NN)
C
                                    IF (K == N) THEN
                                        IH=IH+1
Cel traitement specifique spmd
                                        IF (IMACH == 3) THEN
C recherche du ii correct
                                            II = ((IH-1) - IAD)*NVAR
                                            DO WHILE (ITHBUF(IH+NN) /= ISPMD .AND. IH < IAD+NN)
                                                IH = IH + 1
                                            ENDDO
                                        ENDIF
C
                                        IF (IH > IAD+NN) GOTO 666
                                        WA_SIZE = WA_SIZE + NVAR + 1
                                    ENDIF
                                ENDDO
                            ELSEIF (IGTYP == 33 .OR. IGTYP == 45) THEN
                                DO I=1,NEL
                                    N=I+NFT
                                    K=ITHBUF(IH)
                                    IP=ITHBUF(IH+NN)
C
                                    IF (K == N) THEN
                                        IH=IH+1
Cel traitement specifique spmd
                                        IF (IMACH == 3) THEN
C recherche du ii correct
                                            II = ((IH-1) - IAD)*NVAR
                                            DO WHILE (ITHBUF(IH+NN) /= ISPMD .AND. IH < IAD+NN)
                                                IH = IH + 1
                                            ENDDO
                                        ENDIF
C
                                        IF (IH > IAD+NN) GOTO 666
                                        WA_SIZE = WA_SIZE + NVAR + 1
                                    ENDIF
                                ENDDO ! DO I=1,NEL
                            ENDIF
                    ENDIF ! IF (IGTYP)
                ENDIF ! IF (ITY)
            ENDDO ! DO NG=1,NGROUP

 666        continue
            INDEX_RESSORT(NITER) = WA_SIZE
          ENDIF
        ENDDO ! DO N=1,NTHGRP2


        J_FIRST = 0
        BOOL = .TRUE.
        DO I=1,NTHGRP2
            IF(BOOL.EQV..TRUE.) THEN
                IF( INDEX_RESSORT(I)/=0 ) THEN
                    BOOL = .FALSE.
                    J_FIRST = I
                ENDIF
            ENDIF
        ENDDO

        J = 0
        IF(J_FIRST>0) THEN
            J=J+1
            INDEX_WA_SPRING(J) = INDEX_RESSORT(J_FIRST)
            J=J+1
            INDEX_WA_SPRING(J) = J_FIRST
            DO I=J_FIRST+1,NTHGRP2
                IF( INDEX_RESSORT(I)-INDEX_RESSORT(I-1)>0 ) THEN
                    J=J+1
                    INDEX_WA_SPRING(J) = INDEX_RESSORT(I)
                    J=J+1
                    INDEX_WA_SPRING(J) = I
                ENDIF
            ENDDO
        ENDIF
        INDEX_WA_SPRING(2*NTHGRP2+1) = J  !   number of non-zero index
C-----------
      RETURN
      END
