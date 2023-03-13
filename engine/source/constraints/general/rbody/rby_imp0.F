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
Chd|  RBY_IMP0                      source/constraints/general/rbody/rby_imp0.F
Chd|-- called by -----------
Chd|        UPD_GLOB_K                    source/implicit/upd_glob_k.F  
Chd|-- calls ---------------
Chd|        RBY_IMP1                      source/constraints/general/rbody/rby_imp0.F
Chd|        RBY_IMPM                      source/constraints/general/rbody/rby_imp0.F
Chd|====================================================================
      SUBROUTINE RBY_IMP0(X     ,RBY   ,LPBY  ,NPBY  ,SKEW  ,
     1                    NRBYAC,IRBYAC,NSC   ,ISIJ  ,NMC   ,
     2                    IMIJ  ,NSS   ,ISS   ,ISKEW ,ITAB  ,
     3                    WEIGHT,MS    ,IN    ,
     4                    NDDL  ,IADK  ,JDIK  ,DIAG_K ,
     5                    LT_K  ,NDOF  ,IDDL  ,IKC    ,B     )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER WEIGHT(*),LPBY(*),NPBY(NNPBY,*),ISKEW(*),ITAB(*),
     .        NRBYAC,IRBYAC(*),NDDL,IADK(*),JDIK(*),NDOF(*),
     .        IDDL(*),IKC(*),NSC(*),ISIJ(*),NSS(*) ,ISS(*),
     .        NMC,IMIJ(*)
      my_real
     .   X(3,*), RBY(NRBY,*), SKEW(LSKEW,*),
     .   IN(*),MS(*),DIAG_K(*),LT_K(*),B(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N,K,NK,NN,J,K1
C-----------------------------------------------
        K=1
        NK=1
        NN=1
        DO I=1,NRBYAC
          N=IRBYAC(I)
          K1=IRBYAC(I+NRBYKIN)+1
          CALL RBY_IMP1(X, RBY(1,N),LPBY(K1),NPBY(1,N),
     1                  NSC(I),ISIJ(NK),NSS(K),ISS(NN),
     2                  SKEW,ISKEW,ITAB,WEIGHT,MS,IN,
     3                  NDDL  ,IADK   ,JDIK   ,DIAG_K ,
     4                  LT_K  ,NDOF  ,IDDL   ,IKC    ,B      )
          DO J=1,NPBY(2,N)
           NN  = NN  + NSS(K+J-1)
          ENDDO
          K  = K  + NPBY(2,N)
          NK  = NK  + 2*NSC(I)
        ENDDO
C
        IF (NMC>0)
     .  CALL RBY_IMPM(X    ,NMC   ,IMIJ  ,ISIJ(NK),SKEW ,
     1                ISKEW,ITAB  ,WEIGHT,MS      ,IN   ,
     2                IADK ,JDIK  ,LT_K  ,NDOF    ,IDDL )
C
      RETURN
      END
Chd|====================================================================
Chd|  RBY_IMPI                      source/constraints/general/rbody/rby_imp0.F
Chd|-- called by -----------
Chd|        UPD_INT_K                     source/implicit/upd_glob_k.F  
Chd|-- calls ---------------
Chd|        RBY_IMP1                      source/constraints/general/rbody/rby_imp0.F
Chd|====================================================================
      SUBROUTINE RBY_IMPI(X     ,RBY   ,LPBY  ,NPBY  ,SKEW  ,
     1                    NRBYAC,IRBYAC,NSS   ,ISS   ,ISKEW ,
     2                    ITAB  ,WEIGHT,MS    ,IN    ,
     3                    NDDL  ,IADK  ,JDIK  ,DIAG_K ,
     4                    LT_K  ,NDOF  ,IDDL  ,IKC    ,B     )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER WEIGHT(*),LPBY(*),NPBY(NNPBY,*),ISKEW(*),ITAB(*),
     .        NRBYAC,IRBYAC(*),NDDL,IADK(*),JDIK(*),NDOF(*),
     .        IDDL(*),IKC(*),NSS(*) ,ISS(*)
      my_real
     .   X(3,*), RBY(NRBY,*), SKEW(LSKEW,*),
     .   IN(*),MS(*),DIAG_K(*),LT_K(*),B(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N,K,NN,J,NSC,ISIJ,K1
C-----------------------------------------------
        NSC=0
        K=1
        NN=1
        DO I=1,NRBYAC
          N=IRBYAC(I)
          K1=IRBYAC(I+NRBYKIN)+1
          CALL RBY_IMP1(X, RBY(1,N),LPBY(K1),NPBY(1,N),
     1                  NSC ,ISIJ ,NSS(K),ISS(NN),
     2                  SKEW,ISKEW,ITAB,WEIGHT,MS,IN,
     3                  NDDL  ,IADK   ,JDIK   ,DIAG_K ,
     4                  LT_K  ,NDOF  ,IDDL   ,IKC    ,B   )
          DO J=1,NPBY(2,N)
           NN  = NN  + NSS(K+J-1)
          ENDDO
          K  = K  + NPBY(2,N)
        ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  RBY_IMP1                      source/constraints/general/rbody/rby_imp0.F
Chd|-- called by -----------
Chd|        RBY_IMP0                      source/constraints/general/rbody/rby_imp0.F
Chd|        RBY_IMPI                      source/constraints/general/rbody/rby_imp0.F
Chd|-- calls ---------------
Chd|        GET_KII                       source/implicit/imp_glob_k.F  
Chd|        GET_KIJ                       source/implicit/imp_glob_k.F  
Chd|        PRINT_WKIJ                    source/implicit/imp_glob_k.F  
Chd|        PUT_KII                       source/implicit/imp_glob_k.F  
Chd|        PUT_KIJ                       source/implicit/imp_glob_k.F  
Chd|        UPDKB_RB                      source/constraints/general/rbody/rby_imp0.F
Chd|        UPDKB_RB1                     source/constraints/general/rbody/rby_imp0.F
Chd|        UPDKB_RB2                     source/constraints/general/rbody/rby_imp0.F
Chd|====================================================================
      SUBROUTINE RBY_IMP1(X    ,RBY,NOD ,NBY,
     1                   NSC, ISI ,NS ,NODS,
     2                  SKEW,ISKEW,ITAB,WEIGHT,MS ,IN  ,
     3                  NDDL  ,IADK   ,JDIK   ,DIAG_K ,
     4                  LT_K  ,NDOF  ,IDDL   ,IKC    ,B      )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NOD(*), NBY(*), ISKEW(*),ITAB(*), WEIGHT(*),
     .        NSC,ISI(2,NSC) ,NS(*),NODS(*)
      INTEGER NDDL,IADK(*),JDIK(*),NDOF(*),IDDL(*),IKC(*)
      my_real
     .   X(3,*), RBY(*), SKEW(LSKEW,*),
     .   IN(*),MS(*),DIAG_K(*),LT_K(*),B(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C  ds010   21/2/00  +1
      INTEGER M, NSN, IJD, ISK, I, N, J,NI,NJ,J1,
     .        K,L,ID,JD,ND,IMD,NIDOF,IR
C     REAL
      my_real
     .   XS,YS,ZS,XS1,YS1,ZS1, KDD(6,6),BD(6)
C-----------------------------------------------
      M    =NBY(1)
C -------main utilise place de premier secnd node (just like change node number)
      IF (IMACH==3.AND.M<0) RETURN
       NSN  =NBY(2)
       IMD = IDDL(M)+1
        ND = 6
C--------boucle secnd nodes--
       J1=0
       DO I=1,NSN
C--------block diagonal Kmm--
        N = NOD(I)
        IF (NDOF(N)>0) THEN
         XS=X(1,N)-X(1,M)
         YS=X(2,N)-X(2,M)
         ZS=X(3,N)-X(3,M)
         DO K=1,NDOF(N)
          ID = IDDL(N)+K
          IKC(ID)=7
          BD(K)=B(ID)
         ENDDO
         DO K=NDOF(N)+1,ND
          BD(K)=ZERO
         ENDDO
         CALL GET_KII(N ,IDDL ,IADK,DIAG_K,LT_K ,KDD,NDOF(N))
         CALL UPDKB_RB(NDOF(N),XS,YS,ZS,KDD,BD)
C-------Update K,B---
         CALL PUT_KII(M ,IDDL ,IADK,DIAG_K,LT_K ,KDD,ND)
         DO K=1,ND
          ID = IMD+K-1
          B(ID) = B(ID) + BD(K)
         ENDDO
C--------no diag--Kjm=sum(KjsCsm)--
         DO J = 1,NS(I)
          NI=NODS(J1+J)
          NIDOF=NDOF(NI)
          CALL GET_KIJ(NI,N,IDDL,IADK,JDIK,LT_K,KDD,NIDOF,NDOF(N),IR)
          IF (IR==1) CALL PRINT_WKIJ(ITAB(NI) ,ITAB(N) ,1 )
          CALL UPDKB_RB1(NIDOF,NDOF(N),XS,YS,ZS,KDD)
C-------  Update ---
          CALL PUT_KIJ(NI,M,IDDL,IADK,JDIK,LT_K,KDD,NIDOF,ND,IR)
          IF (IR==1) CALL PRINT_WKIJ(ITAB(NI) ,ITAB(M) ,1 )
         ENDDO
         J1=J1+NS(I)
        ENDIF
       ENDDO
C-------fin -boucle secnd nodes--
C--------due au coupled block KIJ--
       DO I=1,NSC
        NI =ISI(1,I)
        NJ =ISI(2,I)
        CALL GET_KIJ(NI,NJ,IDDL,IADK,JDIK,LT_K,KDD,NDOF(NI),NDOF(NJ),IR)
        IF (IR==1) CALL PRINT_WKIJ(ITAB(NI) ,ITAB(NJ) ,1 )
        XS=X(1,NI)-X(1,M)
        YS=X(2,NI)-X(2,M)
        ZS=X(3,NI)-X(3,M)
        XS1=X(1,NJ)-X(1,M)
        YS1=X(2,NJ)-X(2,M)
        ZS1=X(3,NJ)-X(3,M)
        CALL UPDKB_RB2(NDOF(NI),NDOF(NJ),XS,YS,ZS,XS1,YS1,ZS1,KDD,1)
C--------update --
       CALL PUT_KII(M ,IDDL ,IADK,DIAG_K,LT_K ,KDD,ND)
c        write(*,*)'2 lt_k(2)=',lt_k(2),kdd(1,3),i
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  RBY_IMPM                      source/constraints/general/rbody/rby_imp0.F
Chd|-- called by -----------
Chd|        RBY_IMP0                      source/constraints/general/rbody/rby_imp0.F
Chd|-- calls ---------------
Chd|        GET_KIJ                       source/implicit/imp_glob_k.F  
Chd|        PRINT_WKIJ                    source/implicit/imp_glob_k.F  
Chd|        PUT_KIJ                       source/implicit/imp_glob_k.F  
Chd|        UPDKB_RB2                     source/constraints/general/rbody/rby_imp0.F
Chd|====================================================================
      SUBROUTINE RBY_IMPM(X    ,NMC   ,IMI   ,ISI  ,SKEW ,
     1                    ISKEW,ITAB  ,WEIGHT,MS   ,IN   ,
     2                    IADK ,JDIK  ,LT_K  ,NDOF ,IDDL )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISKEW(*),ITAB(*), WEIGHT(*),
     .        NMC,IMI(2,NMC) ,ISI(2,NMC)
      INTEGER IADK(*),JDIK(*),NDOF(*),IDDL(*)
C     REAL
      my_real
     .   SKEW(LSKEW,*),X(3,*), IN(*),MS(*),LT_K(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C  ds010   21/2/00  +1
      INTEGER M, I, NI,NJ,ND,NM,IR
C     REAL
      my_real
     .   XS,YS,ZS,XS1,YS1,ZS1, KDD(6,6)
C-----------------------------------------------
       ND=6
       DO I=1,NMC
        NI =ISI(1,I)
        NJ =ISI(2,I)
        M  =IMI(1,I)
        NM =IMI(2,I)
        CALL GET_KIJ(NI,NJ,IDDL,IADK,JDIK,LT_K,KDD,NDOF(NI),NDOF(NJ),IR)
        IF (IR==1) CALL PRINT_WKIJ(ITAB(NI) ,ITAB(NJ) ,1 )
        XS=X(1,NI)-X(1,M)
        YS=X(2,NI)-X(2,M)
        ZS=X(3,NI)-X(3,M)
        XS1=X(1,NJ)-X(1,NM)
        YS1=X(2,NJ)-X(2,NM)
        ZS1=X(3,NJ)-X(3,NM)
        CALL UPDKB_RB2(NDOF(NI),NDOF(NJ),XS,YS,ZS,XS1,YS1,ZS1,KDD,0)
C--------update --
        CALL PUT_KIJ(M ,NM ,IDDL ,IADK,JDIK,LT_K,KDD,ND,ND,IR)
        IF (IR==1) CALL PRINT_WKIJ(ITAB(M) ,ITAB(NM) ,1 )
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  UPDKB_RB                      source/constraints/general/rbody/rby_imp0.F
Chd|-- called by -----------
Chd|        I2UPDK0                       source/interfaces/interf/i2_imp1.F
Chd|        I2_FRK0                       source/interfaces/interf/i2_imp1.F
Chd|        RBE2_IMP1                     source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBY_FRK                       source/constraints/general/rbody/rby_imp0.F
Chd|        RBY_IMP1                      source/constraints/general/rbody/rby_imp0.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE UPDKB_RB(NDL,XS,YS,ZS,KDD,BD)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDL
C     REAL
      my_real
     .    XS,YS,ZS, BD(6),KDD(6,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, MI,MJ
C     REAL
      my_real
     .   B(3),K(6,6),KR(3,3),RKR(3,3),RMF(3,3)
C------------------------------------
C-------------produit {K'}=[CDI]^t[K][CDI] B'=[CDI]^tB
c-----with [CDI]=-[[I] [R]]-----
c----             [[0] [I]]-----
        KDD(2,1)=KDD(1,2)
        KDD(3,1)=KDD(1,3)
        KDD(3,2)=KDD(2,3)
        DO I=1,3
         KR(I,1)=-KDD(I,2)*ZS+KDD(I,3)*YS
         KR(I,2)= KDD(I,1)*ZS-KDD(I,3)*XS
         KR(I,3)=-KDD(I,1)*YS+KDD(I,2)*XS
        ENDDO
        DO I=1,3
         RKR(1,I)=-KR(2,I)*ZS+KR(3,I)*YS
         RKR(2,I)= KR(1,I)*ZS-KR(3,I)*XS
         RKR(3,I)=-KR(1,I)*YS+KR(2,I)*XS
        ENDDO
C
        DO I=1,3
        DO J=1,3
          MJ=J+3
          K(I,MJ)=KR(I,J)
        ENDDO
        ENDDO
        DO I=1,3
          MI=I+3
        DO J=1,3
          MJ=J+3
          K(MI,MJ)=RKR(I,J)
        ENDDO
        ENDDO
C
       IF (NDL==6) THEN
        DO I=1,3
        DO J=4,6
          K(I,J)=K(I,J)+KDD(I,J)
        ENDDO
        ENDDO
        DO I=1,3
         J=I+3
         RMF(1,I)=-KDD(2,J)*ZS+KDD(3,J)*YS
         RMF(2,I)= KDD(1,J)*ZS-KDD(3,J)*XS
         RMF(3,I)=-KDD(1,J)*YS+KDD(2,J)*XS
        ENDDO
        DO I=1,3
         MI=I+3
        DO J=I,3
         MJ=J+3
          K(MI,MJ)=K(MI,MJ)+RMF(I,J)+RMF(J,I)+KDD(MI,MJ)
        ENDDO
        ENDDO
        B(1)=-BD(2)*ZS+BD(3)*YS
        B(2)= BD(1)*ZS-BD(3)*XS
        B(3)=-BD(1)*YS+BD(2)*XS
        DO I=1,3
         MI=I+3
         BD(MI)= BD(MI)+B(I)
        ENDDO
       ENDIF
C
       DO I=1,3
        DO J=4,6
          KDD(I,J)=K(I,J)
        ENDDO
       ENDDO
       DO I=4,6
        DO J=I,6
          KDD(I,J)=K(I,J)
        ENDDO
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  UPDKB_RB1                     source/constraints/general/rbody/rby_imp0.F
Chd|-- called by -----------
Chd|        I2UPDK0                       source/interfaces/interf/i2_imp1.F
Chd|        I2_FRK0                       source/interfaces/interf/i2_imp1.F
Chd|        RBE2_IMP1                     source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBY_FRK                       source/constraints/general/rbody/rby_imp0.F
Chd|        RBY_IMP1                      source/constraints/general/rbody/rby_imp0.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE UPDKB_RB1(NI,NJ,XS,YS,ZS,KDD)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NI,NJ
C     REAL
      my_real
     .    XS,YS,ZS, KDD(6,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
C     REAL
      my_real
     .   K(6,6)
C------------------------------------
C-------------produit {K'}=-[K][CDI]
c-----with [CDI]=-[[I] [R]]-----
c----             [[0] [I]]-----
C
        DO I=1,6
        DO J=1,6
          K(I,J)=ZERO
        ENDDO
        ENDDO
        DO I=1,NI
        DO J=1,NJ
          K(I,J)=KDD(I,J)
        ENDDO
        ENDDO
        DO I=1,3
         K(I,4)=K(I,4)-KDD(I,2)*ZS+KDD(I,3)*YS
         K(I,5)=K(I,5)+KDD(I,1)*ZS-KDD(I,3)*XS
         K(I,6)=K(I,6)-KDD(I,1)*YS+KDD(I,2)*XS
        ENDDO
C
       IF (NI==6) THEN
        DO I=4,6
         K(I,4)=K(I,4)-KDD(I,2)*ZS+KDD(I,3)*YS
         K(I,5)=K(I,5)+KDD(I,1)*ZS-KDD(I,3)*XS
         K(I,6)=K(I,6)-KDD(I,1)*YS+KDD(I,2)*XS
        ENDDO
       ENDIF
C
        DO I=1,6
        DO J=1,6
          KDD(I,J)=K(I,J)
        ENDDO
        ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  UPDKB_RB2                     source/constraints/general/rbody/rby_imp0.F
Chd|-- called by -----------
Chd|        I2UPDK0                       source/interfaces/interf/i2_imp1.F
Chd|        I2UPDKM0                      source/interfaces/interf/i2_imp1.F
Chd|        RBY_IMP1                      source/constraints/general/rbody/rby_imp0.F
Chd|        RBY_IMPM                      source/constraints/general/rbody/rby_imp0.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE UPDKB_RB2(NI,NJ,XS,YS,ZS,XS1,YS1,ZS1,KDD,ISYM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NI,NJ,ISYM
C     REAL
      my_real
     .    XS,YS,ZS,XS1,YS1,ZS1, KDD(6,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, MI,MJ
C     REAL
      my_real
     .   K(6,6),KR(3,3),RKR(3,3),RMF(3,3),RK(3,3)
C------------------------------------
C-------------produit {K'}=[CDI]^t[K][CDJ] +()^t
c-----with [CDI]=-[[I] [R]]-----
c----             [[0] [I]]-----
C
        DO I=1,3
         KR(I,1)=-KDD(I,2)*ZS1+KDD(I,3)*YS1
         KR(I,2)= KDD(I,1)*ZS1-KDD(I,3)*XS1
         KR(I,3)=-KDD(I,1)*YS1+KDD(I,2)*XS1
        ENDDO
        DO I=1,3
         RKR(1,I)=-KR(2,I)*ZS+KR(3,I)*YS
         RKR(2,I)= KR(1,I)*ZS-KR(3,I)*XS
         RKR(3,I)=-KR(1,I)*YS+KR(2,I)*XS
         RK(1,I)=-KDD(2,I)*ZS+KDD(3,I)*YS
         RK(2,I)= KDD(1,I)*ZS-KDD(3,I)*XS
         RK(3,I)=-KDD(1,I)*YS+KDD(2,I)*XS
        ENDDO
C
        DO I=1,3
          MI=I+3
        DO J=1,3
          MJ=J+3
          K(I,J)=KDD(I,J)
          K(I,MJ)=KR(I,J)
          K(MI,J)=RK(I,J)
          K(MI,MJ)=RKR(I,J)
        ENDDO
        ENDDO
C
       IF (NI==6) THEN
        DO I=4,6
        DO J=1,3
          K(I,J)=K(I,J)+KDD(I,J)
        ENDDO
        ENDDO
C---------FM Rj------------
        DO I=1,3
         J=I+3
         RMF(I,1)=-KDD(J,2)*ZS1+KDD(J,3)*YS1
         RMF(I,2)= KDD(J,1)*ZS1-KDD(J,3)*XS1
         RMF(I,3)=-KDD(J,1)*YS1+KDD(J,2)*XS1
        ENDDO
        DO I=1,3
         MI=I+3
        DO J=1,3
         MJ=J+3
          K(MI,MJ)=K(MI,MJ)+RMF(I,J)
        ENDDO
        ENDDO
       ENDIF
       IF (NJ==6) THEN
        DO I=1,3
        DO J=4,6
          K(I,J)=K(I,J)+KDD(I,J)
        ENDDO
        ENDDO
C---------Ri^tMF------------
        DO I=1,3
         J=I+3
         RMF(1,I)=-KDD(2,J)*ZS+KDD(3,J)*YS
         RMF(2,I)= KDD(1,J)*ZS-KDD(3,J)*XS
         RMF(3,I)=-KDD(1,J)*YS+KDD(2,J)*XS
        ENDDO
        DO I=1,3
         MI=I+3
        DO J=1,3
         MJ=J+3
          K(MI,MJ)=K(MI,MJ)+RMF(I,J)
        ENDDO
        ENDDO
       ENDIF
       IF (NI==6.AND.NJ==6) THEN
        DO I=1,3
         MI=I+3
        DO J=1,3
         MJ=J+3
          K(MI,MJ)=K(MI,MJ)+KDD(MI,MJ)
        ENDDO
        ENDDO
       ENDIF
C
       IF (ISYM==1) THEN
        DO I=1,6
         DO J=1,6
          KDD(I,J)=K(I,J)+K(J,I)
         ENDDO
        ENDDO
       ELSE
        DO I=1,6
         DO J=1,6
          KDD(I,J)=K(I,J)
         ENDDO
        ENDDO
       ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  RBY_IMPF                      source/constraints/general/rbody/rby_imp0.F
Chd|-- called by -----------
Chd|        IMP3_A2B                      source/airbag/monv_imp0.F     
Chd|        UPD_FR                        source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE RBY_IMPF(X  ,M  ,N   ,NDOF ,A     ,AR  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N, M,NDOF(*)
C     REAL
      my_real
     .   X(3,*), A(3,*),AR(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   XS,YS,ZS
C-----------------------------------------------
       IF (M<0) RETURN
        A(1,M)=A(1,M)+A(1,N)
        A(2,M)=A(2,M)+A(2,N)
        A(3,M)=A(3,M)+A(3,N)
       IF (NDOF(M)==6) THEN
        XS=X(1,N)-X(1,M)
        YS=X(2,N)-X(2,M)
        ZS=X(3,N)-X(3,M)
        AR(1,M)=AR(1,M)-A(2,N)*ZS+A(3,N)*YS
        AR(2,M)=AR(2,M)+A(1,N)*ZS-A(3,N)*XS
        AR(3,M)=AR(3,M)-A(1,N)*YS+A(2,N)*XS
       ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  UPDFR_RB                      source/constraints/general/rbody/rby_imp0.F
Chd|-- called by -----------
Chd|        DIAG_INT                      source/mpi/implicit/imp_fri.F 
Chd|        I2_FRUP0                      source/interfaces/interf/i2_imp1.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE UPDFR_RB(XS,YS,ZS,KII,K)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDL
C     REAL
      my_real
     .    XS,YS,ZS, K(6),KII(6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   KDD(3,3),KR(3,3)
C------------------------------------
C-------------produit {K'}=[CDI]^t[K][CDI]
c-----with [CDI]=-[[I] [R]]-----
        DO I=1,3
         KDD(I,I)=KII(I)
         K(I)=K(I)+KII(I)
        ENDDO
        KDD(1,2)=KII(4)
        KDD(1,3)=KII(5)
        KDD(2,3)=KII(6)
        KDD(2,1)=KDD(1,2)
        KDD(3,1)=KDD(1,3)
        KDD(3,2)=KDD(2,3)
        DO I=1,3
         KR(I,1)=-KDD(I,2)*ZS+KDD(I,3)*YS
         KR(I,2)= KDD(I,1)*ZS-KDD(I,3)*XS
         KR(I,3)=-KDD(I,1)*YS+KDD(I,2)*XS
        ENDDO
C
        K(4)=K(4)-KR(2,1)*ZS+KR(3,1)*YS
        K(5)=K(5)+KR(1,2)*ZS-KR(3,2)*XS
        K(6)=K(6)-KR(1,3)*YS+KR(2,3)*XS
C
      RETURN
      END
Chd|====================================================================
Chd|  RBY_IMPR1                     source/constraints/general/rbody/rby_imp0.F
Chd|-- called by -----------
Chd|        IMP_DYKV                      source/implicit/imp_dyna.F    
Chd|        IMP_DYKV0                     source/implicit/imp_dyna.F    
Chd|        UPD_RHS                       source/implicit/upd_glob_k.F  
Chd|        UPD_RHS_FR                    source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        UPDB_RB                       source/constraints/general/rbody/rby_imp0.F
Chd|====================================================================
      SUBROUTINE RBY_IMPR1(X    ,RBY,NOD ,NBY,NDOF  ,
     1                   IDDL    ,B      )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NOD(*), NBY(*),NDOF(*),IDDL(*)
C     REAL
      my_real
     .   X(3,*), RBY(*), B(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C  ds010   21/2/00  +1
      INTEGER M, NSN, IJD, ISK, I, N, J,NI,NJ,J1,
     .        K,L,ID,JD,ND,IMD,NIDOF
C     REAL
      my_real
     .   XS,YS,ZS,BD(6)
C-----------------------------------------------
      M    =NBY(1)
C -------main utilise place de premier secnd node (just like change node number)
      IF ((IMACH==3.AND.M<0).OR.NDOF(M)==0) RETURN
       NSN  =NBY(2)
        ND = 6
C--------boucle secnd nodes--
       J1=0
       DO I=1,NSN
C--------block diagonal Kmm--
        N = NOD(I)
        IF (NDOF(N)>0) THEN
         XS=X(1,N)-X(1,M)
         YS=X(2,N)-X(2,M)
         ZS=X(3,N)-X(3,M)
         DO K=1,NDOF(N)
          ID = IDDL(N)+K
          BD(K)=B(ID)
         ENDDO
         DO K=NDOF(N)+1,ND
          BD(K)=ZERO
         ENDDO
         CALL UPDB_RB(NDOF(N),XS,YS,ZS,BD)
C-------Update B---
         DO K=1,ND
          ID = IDDL(M)+K
          B(ID) = B(ID) + BD(K)
         ENDDO
        ENDIF
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  UPDB_RB                       source/constraints/general/rbody/rby_imp0.F
Chd|-- called by -----------
Chd|        I2UPDB0                       source/interfaces/interf/i2_imp1.F
Chd|        I2UPDB02                      source/interfaces/interf/i2_imp1.F
Chd|        RBE2_IMPB0                    source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBY_IMPR1                     source/constraints/general/rbody/rby_imp0.F
Chd|        RBY_IMPR2                     source/constraints/general/rbody/rby_imp0.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE UPDB_RB(NDL,XS,YS,ZS,BD)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDL
C     REAL
      my_real
     .    XS,YS,ZS, BD(6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, MI,MJ
C     REAL
      my_real
     .   B(3)
C------------------------------------
C-------------produit B'=[CDI]^tB
c-----with [CDI]=-[[I] [R]]-----
c----             [[0] [I]]-----
C
       IF (NDL==6) THEN
        B(1)=-BD(2)*ZS+BD(3)*YS
        B(2)= BD(1)*ZS-BD(3)*XS
        B(3)=-BD(1)*YS+BD(2)*XS
        DO I=1,3
         MI=I+3
         BD(MI)= BD(MI)+B(I)
        ENDDO
       ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  RBY_IMPR2                     source/constraints/general/rbody/rby_imp0.F
Chd|-- called by -----------
Chd|        IMP_DYKV                      source/implicit/imp_dyna.F    
Chd|        IMP_DYKV0                     source/implicit/imp_dyna.F    
Chd|        UPD_RHS                       source/implicit/upd_glob_k.F  
Chd|        UPD_RHS_FR                    source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        UPDB_RB                       source/constraints/general/rbody/rby_imp0.F
Chd|====================================================================
      SUBROUTINE RBY_IMPR2(X    ,RBY,NOD ,NBY,NDOF  ,
     1                   IDDL   ,B  ,AC  ,ACR    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NOD(*), NBY(*),NDOF(*),IDDL(*)
C     REAL
      my_real
     .   X(3,*), RBY(*), B(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C  ds010   21/2/00  +1
      INTEGER M, NSN, IJD, ISK, I, N, J,NI,NJ,J1,
     .        K,L,ID,JD,ND,IMD,NIDOF
C     REAL
      my_real
     .   XS,YS,ZS,BD(6),AC(3,*) ,ACR(3,*)
C-----------------------------------------------
      M    =NBY(1)
C -------main utilise place de premier secnd node (just like change node number)
      IF (IMACH==3.AND.M<0) RETURN
       NSN  =NBY(2)
        ND = 6
C--------boucle secnd nodes--
       J1=0
       DO I=1,NSN
C--------block diagonal Kmm--
        N = NOD(I)
        IF (NDOF(N)==0) THEN
         XS=X(1,N)-X(1,M)
         YS=X(2,N)-X(2,M)
         ZS=X(3,N)-X(3,M)
         DO K=1,3
          BD(K)=AC(K,N)
          BD(K+3)=ACR(K,N)
         ENDDO
         CALL UPDB_RB(ND,XS,YS,ZS,BD)
C-------Update B---
         IF (NDOF(M)==0) THEN
          DO K=1,3
           AC(K,M)=AC(K,M)+BD(K)
           ACR(K,M)=ACR(K,M)+BD(K+3)
          ENDDO
         ELSE
          DO K=1,ND
           ID = IDDL(M)+K
           B(ID) = B(ID) + BD(K)
          ENDDO
         ENDIF
        ENDIF
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  RBY_FRK                       source/constraints/general/rbody/rby_imp0.F
Chd|-- called by -----------
Chd|        UPD_KML                       source/mpi/implicit/imp_fri.F 
Chd|        UPD_KSL                       source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        PUT_KMII                      source/implicit/imp_glob_k.F  
Chd|        UPDKB_RB                      source/constraints/general/rbody/rby_imp0.F
Chd|        UPDKB_RB1                     source/constraints/general/rbody/rby_imp0.F
Chd|====================================================================
      SUBROUTINE RBY_FRK(NS   ,M    ,X     ,ITAB ,IKC  ,
     1                    NDOF  ,IDDL ,IDDLM,IADK ,JDIK ,
     2                    DIAG_K,LT_K ,B    ,A    ,KSS  ,
     3                    KSM  ,KNM   ,KRM  ,IDLM ,ISS,ISM )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IADK(*),JDIK(*),NDOF(*),IDDL(*),IDDLM(*),IKC(*),
     .        M, NS,ITAB(*),IDLM ,ISS,ISM
      my_real
     .   X(3,*),DIAG_K(*),LT_K(*),B(*),A(3,*),
     .   KSS(6),KSM(3,3),KNM(3,3),KRM(3,3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K,ID,NL,NI,NJ,NDOFI,ND,IR,IDM
      my_real KDD(6,6),BD(6),XS,YS,ZS
C------------------------------------
C     VITESSES DES NOEUDS SECONDS
C------------------------------------
        I=NS
        NDOFI = 3
        ND = 6
C-----
         XS=X(1,I)-X(1,M)
         YS=X(2,I)-X(2,M)
         ZS=X(3,I)-X(3,M)
        IF (ISS>0) THEN
         DO K=1,NDOFI
          BD(K) = A(K,I)
          KDD(K,K) = KSS(K)
         ENDDO
         DO K=NDOFI+1,6
          BD(K)=ZERO
         ENDDO
         KDD(1,2) = KSS(4)
         KDD(1,3) = KSS(5)
         KDD(2,3) = KSS(6)
         CALL UPDKB_RB(NDOFI,XS,YS,ZS,KDD,BD)
         CALL PUT_KMII(IDLM,IADK,DIAG_K,LT_K,KDD,ND)
        ENDIF
        IF (ISM>0) THEN
C--------no diag--Kjm=sum(KjsCsm)--
          DO K=1,NDOFI
          DO J=1,NDOFI
           KDD(K,J) = KSM(K,J)
          ENDDO
          ENDDO
C------- Update ---
          CALL UPDKB_RB1(NDOFI,NDOFI,XS,YS,ZS,KDD)
           DO K=1,NDOFI
           DO J=1,NDOFI
            KNM(K,J)=KDD(J,K)
            KRM(K,J)=KDD(J,K+NDOFI)
           ENDDO
           ENDDO
        ENDIF
C
      RETURN
      END
