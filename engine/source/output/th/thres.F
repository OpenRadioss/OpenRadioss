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
Chd|  THRES                         source/output/th/thres.F      
Chd|-- called by -----------
Chd|        ABFHIST2                      source/output/abf/abfhist2.F  
Chd|        HIST2                         source/output/th/hist2.F      
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE THRES(IPARG,ITHBUF,ELBUF_TAB,WA,IGEO,
     .                      IXR,NTHGRP2,ITHGRP,X)
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
      INTEGER, INTENT(in) :: NTHGRP2
      INTEGER, DIMENSION(NITHGR,*), INTENT(in) :: ITHGRP
      INTEGER IPARG(NPARG,*),ITHBUF(*),IXR(NIXR,*),
     .   IGEO(NPROPGI,*)
      my_real
     .   WA(*),X(3,NUMNOD)
C
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
!       NTHGRP2 : integer ; number of TH group
!       WA_SIZE : integer ; size of working array for spring element
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: II,I,N,IH,NG,ITY,MTE,K,IP,L
      INTEGER :: IJK,LWA,NEL,NFT,IPROP,IGTYP,J,JJ(6)
      INTEGER :: NITER,IAD,NN,IADV,NVAR,ITYP,NODE1,NODE2,NODE3
      my_real
     . WWA(100)
      my_real
     . V1,V2,V3,E1X,E1Y,E1Z,E2X,E2Y,E2Z,E3X,E3Y,E3Z
C
      TYPE(G_BUFEL_)  ,POINTER :: GBUF
!$COMMENT
!       THRES description
!       initialization of WA array for spring element
!
!       THRES organization :
!       loop over the NTHGRP2 TH group and
!       if a group is a spring group, then :
!           - initialization of the NVAR value
!           - add the position II at the end of the chunk (NVAR+1 value)
!$ENDCOMMENT
C-----------------------------------------------
C           ELEMENTS RESSORTS
C-----------------------------------------------
        IJK = 0
        DO NITER=1,NTHGRP2
          II=0
          ITYP=ITHGRP(2,NITER)
          NN  =ITHGRP(4,NITER)
          IAD =ITHGRP(5,NITER)
          NVAR=ITHGRP(6,NITER)
          IADV=ITHGRP(7,NITER)

          IF(ITYP==6) THEN
            IH=IAD
Cel specifique spmd
            IF (IMACH == 3) THEN
C decalage IH
                DO WHILE (ITHBUF(IH+NN) /= ISPMD .AND. IH < IAD+NN)
                    IH = IH + 1
                ENDDO
                IF (IH >= IAD+NN) CYCLE
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
                            NODE1 = IXR(2,N)
                            NODE2 = IXR(3,N)
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
C
                                WWA(1)=GBUF%OFF(I)
                                WWA(2)=GBUF%FOR(I)
                                WWA(3)=ZERO
                                WWA(4)=ZERO
                                WWA(5)=ZERO
                                WWA(6)=ZERO
                                WWA(7)=ZERO
                                WWA(8)=GBUF%TOTDEPL(I)
                                WWA(9)=ZERO
                                WWA(10)=ZERO
                                WWA(11)=ZERO
                                WWA(12)=ZERO
                                WWA(13)=ZERO
                                WWA(14)=GBUF%EINT(I)
                                WWA(15)=ZERO
                                WWA(16)=ZERO
                                DO L=17,64
                                    WWA(L)= ZERO
                                ENDDO
                                ! Absolute spring length
                                WWA(65)= SQRT((X(1,NODE2)-X(1,NODE1))**2 +
     .                                        (X(2,NODE2)-X(2,NODE1))**2 +
     .                                        (X(3,NODE2)-X(3,NODE1))**2)
                                DO L=IADV,IADV+NVAR-1
                                    K=ITHBUF(L)
                                    IJK=IJK+1
                                    WA(IJK)=WWA(K)
                                ENDDO
                                IJK=IJK+1
                                WA(IJK) = II
                            ENDIF
                        ENDDO
                    ELSEIF (IGTYP == 26) THEN
                        DO I=1,NEL
                            N=I+NFT
                            K=ITHBUF(IH)
                            IP=ITHBUF(IH+NN)
                            NODE1 = IXR(2,N)
                            NODE2 = IXR(3,N)
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
C
                                WWA(1)=GBUF%OFF(I)
                                WWA(2)=GBUF%FOR(I)
                                WWA(3)=ZERO
                                WWA(4)=ZERO
                                WWA(5)=ZERO
                                WWA(6)=ZERO
                                WWA(7)=ZERO
                                WWA(8)=GBUF%TOTDEPL(I)
                                WWA(9)=ZERO
                                WWA(10)=ZERO
                                WWA(11)=ZERO
                                WWA(12)=ZERO
                                WWA(13)=ZERO
                                WWA(14)=GBUF%EINT(I)
                                WWA(15)=ZERO
                                WWA(16)=ZERO
                                DO L=17,64
                                    WWA(L)= ZERO
                                ENDDO
                                ! Absolute spring length
                                WWA(65)= SQRT((X(1,NODE2)-X(1,NODE1))**2 +
     .                                        (X(2,NODE2)-X(2,NODE1))**2 +
     .                                        (X(3,NODE2)-X(3,NODE1))**2)
                                DO L=IADV,IADV+NVAR-1
                                    K=ITHBUF(L)
                                    IJK=IJK+1
                                    WA(IJK)=WWA(K)
                                ENDDO
                                IJK=IJK+1
                                WA(IJK) = II
                            ENDIF
                        ENDDO
                    ELSEIF (IGTYP == 27) THEN
                        DO I=1,NEL
                            N=I+NFT
                            K=ITHBUF(IH)
                            IP=ITHBUF(IH+NN)
                            NODE1 = IXR(2,N)
                            NODE2 = IXR(3,N)
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
C
                                WWA(1)=GBUF%OFF(I)
                                WWA(2)=GBUF%FOR(I)
                                WWA(3)=ZERO
                                WWA(4)=ZERO
                                WWA(5)=ZERO
                                WWA(6)=ZERO
                                WWA(7)=ZERO
                                WWA(8)=GBUF%TOTDEPL(I)
                                WWA(9)=ZERO
                                WWA(10)=ZERO
                                WWA(11)=ZERO
                                WWA(12)=ZERO
                                WWA(13)=ZERO
                                WWA(14)=GBUF%EINT(I)
                                WWA(15)=ZERO
                                WWA(16)=ZERO
                                DO L=17,64
                                    WWA(L)= ZERO
                                ENDDO
                                ! Absolute spring length
                                WWA(65)= SQRT((X(1,NODE2)-X(1,NODE1))**2 +
     .                                        (X(2,NODE2)-X(2,NODE1))**2 +
     .                                        (X(3,NODE2)-X(3,NODE1))**2)
                                DO L=IADV,IADV+NVAR-1
                                    K=ITHBUF(L)
                                    IJK=IJK+1
                                    WA(IJK)=WWA(K)
                                ENDDO
                                IJK=IJK+1
                                WA(IJK) = II
                            ENDIF
                        ENDDO
                    ELSEIF( IGTYP == 12) THEN
                        DO I=1,NEL
                            N=I+NFT
                            K=ITHBUF(IH)
                            IP=ITHBUF(IH+NN)
                            NODE1 = IXR(2,N)
                            NODE2 = IXR(3,N)
                            NODE3 = IXR(4,N)
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
C
                                WWA(1)=GBUF%OFF(I)
                                WWA(2)=GBUF%FOR(I)
                                WWA(3)=ZERO
                                WWA(4)=ZERO
                                WWA(5)=ZERO
                                WWA(6)=ZERO
                                WWA(7)=ZERO
                                WWA(8)=GBUF%TOTDEPL(I)
                                WWA(9)=ZERO
                                WWA(10)=ZERO
                                WWA(11)=ZERO
                                WWA(12)=ZERO
                                WWA(13)=ZERO
                                WWA(14)=GBUF%EINT(I)
                                WWA(15)=GBUF%FOR(I) + GBUF%DFS(I)
                                WWA(16)=GBUF%FOR(I) - GBUF%DFS(I)
                                DO L=17,64
                                    WWA(L)= ZERO
                                ENDDO
                                ! Absolute spring length
                                WWA(65)= SQRT((X(1,NODE2)-X(1,NODE1))**2 +
     .                                        (X(2,NODE2)-X(2,NODE1))**2 +
     .                                        (X(3,NODE2)-X(3,NODE1))**2)
     .                                 + SQRT((X(1,NODE3)-X(1,NODE2))**2 +
     .                                        (X(2,NODE3)-X(2,NODE2))**2 +
     .                                        (X(3,NODE3)-X(3,NODE2))**2)
                                DO L=IADV,IADV+NVAR-1
                                    K=ITHBUF(L)
                                    IJK=IJK+1
                                    WA(IJK)=WWA(K)
                                ENDDO
                                IJK=IJK+1
                                WA(IJK) = II
                            ENDIF
                        ENDDO
                    ELSEIF (IGTYP == 8 .OR. IGTYP == 13 .OR. IGTYP == 25
     .                                .OR. IGTYP == 23 ) THEN
                        DO I=1,NEL
                            N=I+NFT
                            K=ITHBUF(IH)
                            IP=ITHBUF(IH+NN)
                            NODE1 = IXR(2,N)
                            NODE2 = IXR(3,N)
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
C
                                WWA(1)=GBUF%OFF(I)
                                WWA(2)=GBUF%FOR(JJ(1)+I-1)
                                WWA(3)=GBUF%FOR(JJ(2)+I-1)
                                WWA(4)=GBUF%FOR(JJ(3)+I-1)
                                WWA(5)=GBUF%MOM(JJ(1)+I-1)
                                WWA(6)=GBUF%MOM(JJ(2)+I-1)
                                WWA(7)=GBUF%MOM(JJ(3)+I-1)
                                WWA(8)=GBUF%TOTDEPL(JJ(1)+I-1)
                                WWA(9)=GBUF%TOTDEPL(JJ(2)+I-1)
                                WWA(10)=GBUF%TOTDEPL(JJ(3)+I-1)
                                WWA(11)=GBUF%TOTROT(JJ(1)+I-1)
                                WWA(12)=GBUF%TOTROT(JJ(2)+I-1)
                                WWA(13)=GBUF%TOTROT(JJ(3)+I-1)
                                WWA(14)=GBUF%EINT(I)
                                WWA(15)=ZERO
                                WWA(16)=ZERO
                                DO L=17,64
                                    WWA(L)= ZERO
                                ENDDO
                                ! Absolute spring length
                                WWA(65)= SQRT((X(1,NODE2)-X(1,NODE1))**2 +
     .                                        (X(2,NODE2)-X(2,NODE1))**2 +
     .                                        (X(3,NODE2)-X(3,NODE1))**2)
                                DO L=IADV,IADV+NVAR-1
                                    K=ITHBUF(L)
                                    IJK=IJK+1
                                    WA(IJK)=WWA(K)
                                ENDDO
                                IJK=IJK+1
                                WA(IJK) = II
                            ENDIF
                        ENDDO
                    ELSEIF (IGTYP >= 29) THEN
                            IF (IGTYP <= 31 .OR. IGTYP == 35 .OR. IGTYP == 36. OR.
     .                      IGTYP == 44) THEN
                                DO I=1,NEL
                                    N=I+NFT
                                    K=ITHBUF(IH)
                                    IP=ITHBUF(IH+NN)
                                    NODE1 = IXR(2,N)
                                    NODE2 = IXR(3,N)
C
                                    IF (K == N) THEN
                                        IH=IH+1
Cel traitement specifique spmd
                                        IF (IMACH == 3) THEN
C recherche du ii correct
                                            II = ((IH-1) - IAD)*NVAR
                                            DO WHILE (ITHBUF(IH+NN) /= ISPMD .AND. IH<IAD+NN)
                                                IH = IH + 1
                                            ENDDO
                                        ENDIF
C
                                        IF (IH > IAD+NN) GOTO 666
C
                                        WWA(1)=GBUF%OFF(I)
                                        WWA(2)=GBUF%FOR(JJ(1)+I-1)
                                        WWA(3)=GBUF%FOR(JJ(2)+I-1)
                                        WWA(4)=GBUF%FOR(JJ(3)+I-1)
                                        WWA(5)=GBUF%MOM(JJ(1)+I-1)
                                        WWA(6)=GBUF%MOM(JJ(2)+I-1)
                                        WWA(7)=GBUF%MOM(JJ(3)+I-1)
                                        WWA(8) =GBUF%V_REPCVT(JJ(1)+I-1)
                                        WWA(9) =GBUF%V_REPCVT(JJ(2)+I-1)
                                        WWA(10)=GBUF%V_REPCVT(JJ(3)+I-1)
                                        WWA(11)=GBUF%VR_REPCVT(JJ(1)+I-1)
                                        WWA(12)=GBUF%VR_REPCVT(JJ(2)+I-1)
                                        WWA(13)=GBUF%VR_REPCVT(JJ(3)+I-1)
                                        WWA(14)=GBUF%EINT(I)
!
                                        WWA(15)=ZERO
                                        WWA(16)=ZERO
C---            repere                                 
                                        E1X = GBUF%SKEW(6*(I-1) + 1)
                                        E1Y = GBUF%SKEW(6*(I-1) + 2)
                                        E1Z = GBUF%SKEW(6*(I-1) + 3)
                                        E2X = GBUF%SKEW(6*(I-1) + 4)
                                        E2Y = GBUF%SKEW(6*(I-1) + 5)
                                        E2Z = GBUF%SKEW(6*(I-1) + 6)
                                        E3X = E1Y*E2Z - E1Z*E2Y
                                        E3Y = E1Z*E2X - E1X*E2Z
                                        E3Z = E1X*E2Y - E1Y*E2X
C---            force locale                           
                                        V1 = GBUF%FOR(JJ(1)+I-1)
                                        V2 = GBUF%FOR(JJ(2)+I-1)
                                        V3 = GBUF%FOR(JJ(3)+I-1)
                                        WWA(23)=-V1
                                        WWA(24)=-V2
                                        WWA(25)=-V3
                                        WWA(26)= V1
                                        WWA(27)= V2
                                        WWA(28)= V3
C---            force globale                          
                                        WWA(20)= V1*E1X+V2*E1Y+V3*E1Z
                                        WWA(21)= V1*E2X+V2*E2Y+V3*E2Z
                                        WWA(22)= V1*E3X+V2*E3Y+V3*E3Z
                                        WWA(17)=-WWA(20)
                                        WWA(18)=-WWA(21)
                                        WWA(19)=-WWA(22)
C---            moment local                           
                                        V1 = GBUF%MOM(JJ(1)+I-1)
                                        V2 = GBUF%MOM(JJ(4)+I-1)
                                        V3 = GBUF%MOM(JJ(5)+I-1)
                                        WWA(35)= V1
                                        WWA(36)= V2
                                        WWA(37)= V3
                                        WWA(38)=-V1
                                        WWA(39)= V2 + TWO*GBUF%MOM(JJ(2)+I-1)
                                        WWA(40)= V3 + TWO*GBUF%MOM(JJ(3)+I-1)
C---            moment global                          
                                        WWA(29)= V1*E1X+V2*E1Y+V3*E1Z
                                        WWA(30)= V1*E2X+V2*E2Y+V3*E2Z
                                        WWA(31)= V1*E3X+V2*E3Y+V3*E3Z
                                        WWA(32)= WWA(38)*E1X+WWA(39)*E1Y+WWA(40)*E1Z
                                        WWA(33)= WWA(38)*E2X+WWA(39)*E2Y+WWA(40)*E2Z
                                        WWA(34)= WWA(38)*E3X+WWA(39)*E3Y+WWA(40)*E3Z
C---            deformation locale                         
                                        V1 = -GBUF%V_REPCVT(JJ(1)+I-1)
                                        WWA(47)= V1
                                        WWA(48)= ZERO
                                        WWA(49)= ZERO
                                        WWA(50)=-V1
                                        WWA(51)= ZERO
                                        WWA(52)= ZERO
C---            deformation globale                        
                                        WWA(41)= V1*E1X
                                        WWA(42)= V1*E2X
                                        WWA(43)= V1*E3X
                                        WWA(44)=-WWA(41)
                                        WWA(45)=-WWA(42)
                                        WWA(46)=-WWA(43)
C---            rotation locale Noeud1                     
                                        V1 = -GBUF%VR_REPCVT(JJ(1)+I-1)
                                        V2 = GBUF%V_REPCVT(JJ(2)+I-1)
                                        V3 = GBUF%V_REPCVT(JJ(3)+I-1)
                                        WWA(59)= V1
                                        WWA(60)= V2
                                        WWA(61)= V3
C---            rotation globale Noeud1                    
                                        WWA(53)= V1*E1X+V2*E1Y+V3*E1Z
                                        WWA(54)= V1*E2X+V2*E2Y+V3*E2Z
                                        WWA(55)= V1*E3X+V2*E3Y+V3*E3Z
C---            rotation locale Noeud2                     
                                        V2 = GBUF%VR_REPCVT(JJ(2)+I-1)
                                        V3 = GBUF%VR_REPCVT(JJ(3)+I-1)
                                        WWA(62)=-V1
                                        WWA(63)= V2
                                        WWA(64)= V3
C---            rotation globale Noeud2                    
                                        WWA(56)=-V1*E1X+V2*E1Y+V3*E1Z
                                        WWA(57)=-V1*E2X+V2*E2Y+V3*E2Z
                                        WWA(58)=-V1*E3X+V2*E3Y+V3*E3Z
C---            absolute spring length
                                        WWA(65)= SQRT((X(1,NODE2)-X(1,NODE1))**2 +
     .                                                (X(2,NODE2)-X(2,NODE1))**2 +
     .                                                (X(3,NODE2)-X(3,NODE1))**2)
c
                                        DO L=IADV,IADV+NVAR-1
                                            K=ITHBUF(L)
                                            IJK=IJK+1
                                            WA(IJK)=WWA(K)
                                        ENDDO
                                        IJK=IJK+1
                                        WA(IJK) = II
                                    ENDIF
                                ENDDO
                            ELSEIF (IGTYP == 32) THEN
                                DO I=1,NEL
                                    N=I+NFT
                                    K=ITHBUF(IH)
                                    IP=ITHBUF(IH+NN)
                                    NODE1 = IXR(2,N)
                                    NODE2 = IXR(3,N)
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
C
                                        WWA(1)=GBUF%OFF(I)
                                        WWA(2)=GBUF%FOR(JJ(1)+I-1)
                                        WWA(3)=GBUF%FOR(JJ(2)+I-1)
                                        WWA(4)=GBUF%FOR(JJ(3)+I-1)
                                        WWA(5)=GBUF%MOM(JJ(1)+I-1)
                                        WWA(6)=GBUF%MOM(JJ(2)+I-1)
                                        WWA(7)=GBUF%MOM(JJ(3)+I-1)
                                        WWA(8)=GBUF%V_REPCVT(JJ(1)+I-1)
                                        WWA(9)=GBUF%V_REPCVT(JJ(2)+I-1)
                                        WWA(10)=GBUF%V_REPCVT(JJ(3)+I-1)
                                        WWA(11)=GBUF%VR_REPCVT(JJ(1)+I-1)
                                        WWA(12)=GBUF%VR_REPCVT(JJ(2)+I-1)
                                        WWA(13)=GBUF%VR_REPCVT(JJ(3)+I-1)
                                        WWA(14)=GBUF%EINT(I)
                                        WWA(15)=ZERO
                                        WWA(16)=ZERO
                                        DO L=17,64
                                            WWA(L)= ZERO
                                        ENDDO
                                        ! Absolute spring length
                                        WWA(65)= SQRT((X(1,NODE2)-X(1,NODE1))**2 +
     .                                                (X(2,NODE2)-X(2,NODE1))**2 +
     .                                                (X(3,NODE2)-X(3,NODE1))**2)
                                        DO L=IADV,IADV+NVAR-1
                                            K=ITHBUF(L)
                                            IJK=IJK+1
                                            WA(IJK)=WWA(K)
                                        ENDDO
                                        IJK=IJK+1
                                        WA(IJK) = II
                                    ENDIF
                                ENDDO
                            ELSEIF (IGTYP == 33 .OR. IGTYP == 45) THEN
                                DO I=1,NEL
                                    N=I+NFT
                                    K=ITHBUF(IH)
                                    IP=ITHBUF(IH+NN)
                                    NODE1 = IXR(2,N)
                                    NODE2 = IXR(3,N)
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
C
                                        WWA(1)=GBUF%OFF(I)
                                        WWA(2)=GBUF%FOR(JJ(1)+I-1)
                                        WWA(3)=GBUF%FOR(JJ(2)+I-1)
                                        WWA(4)=GBUF%FOR(JJ(3)+I-1)
                                        WWA(5)=GBUF%MOM(JJ(1)+I-1)
                                        WWA(6)=GBUF%MOM(JJ(2)+I-1)
                                        WWA(7)=GBUF%MOM(JJ(3)+I-1)
                                        WWA(8)=GBUF%TOTDEPL(JJ(1)+I-1)
                                        WWA(9)=GBUF%TOTDEPL(JJ(2)+I-1)
                                        WWA(10)=GBUF%TOTDEPL(JJ(3)+I-1)
                                        WWA(11)=GBUF%TOTROT(JJ(1)+I-1)
                                        WWA(12)=GBUF%TOTROT(JJ(2)+I-1)
                                        WWA(13)=GBUF%TOTROT(JJ(3)+I-1)
                                        WWA(14)=GBUF%EINT(I)
                                        WWA(15)=ZERO
                                        WWA(16)=ZERO
                                        DO L=17,64
                                            WWA(L)= ZERO
                                        ENDDO
                                        ! Absolute spring length
                                        WWA(65)= SQRT((X(1,NODE2)-X(1,NODE1))**2 +
     .                                                (X(2,NODE2)-X(2,NODE1))**2 +
     .                                                (X(3,NODE2)-X(3,NODE1))**2)
                                        DO L=IADV,IADV+NVAR-1
                                            K=ITHBUF(L)
                                            IJK=IJK+1
                                            WA(IJK)=WWA(K)
                                        ENDDO
                                        IJK=IJK+1
                                        WA(IJK) = II
                                    ENDIF
                                ENDDO ! DO I=1,NEL
                            ENDIF
                    ENDIF ! IF (IGTYP)
                ENDIF ! IF (ITY)
            ENDDO ! DO NG=1,NGROUP

        ENDIF ! if(ITYP==6)

 666  ENDDO ! DO N=1,NTHGRP2
C-----------
      RETURN
      END
