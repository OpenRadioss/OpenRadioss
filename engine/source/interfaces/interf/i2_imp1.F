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
Chd|  I2_IMP1                       source/interfaces/interf/i2_imp1.F
Chd|-- called by -----------
Chd|        I2_IMP0                       source/interfaces/interf/i2_imp0.F
Chd|        I2_IMPI                       source/interfaces/interf/i2_imp0.F
Chd|-- calls ---------------
Chd|        I2UPDK0                       source/interfaces/interf/i2_imp1.F
Chd|        I2UPDK1                       source/interfaces/interf/i2_imp1.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I2_IMP1(IPARI,INTBUF_TAB,ITAB  ,
     .                   NSC2 ,ISIJ2,NSS2,ISS2 ,
     .                   X    ,MS  ,IN   ,WEIGHT  ,
     .                  IKC ,NDOF ,NDDL,IDDL    ,IADK  ,
     .                  JDIK ,DIAG_K ,LT_K  ,B  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IPARI(*), WEIGHT(*),
     .           NSC2,ISIJ2(*),NSS2(*),ISS2(*),ITAB(*)
        INTEGER NDDL,IADK(*),JDIK(*),NDOF(*),IDDL(*),IKC(*)
C     REAL
        my_real
     .     X(*),MS(*),IN(*),DIAG_K(*),LT_K(*),B(*)

        TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER
     .     K10, K11, K12, K13, K14, KFI, J10, J11, J12, J21, J22,
     .     JFI,NSN,NMN,NRTS,NRTM,ILEV
C-----------------------------------------------
        NRTS  =IPARI(3)
        NRTM  =IPARI(4)
        NSN   =IPARI(5)
        NMN   =IPARI(6)
        ILEV  =IPARI(20)
C
        K10=1
        K11=K10+4*NRTS
        K12=K11+4*NRTM
        K13=K12+NSN
        K14=K13+NMN
        KFI=K14+NSN
        J10=1
        J11=J10+1
        J12=J11+NPARIR
        J21=J12+2*NSN
        J22=J21+7*NSN
        JFI=J22+NMN
C  version spmd avec plus d'un proc nsn = nsn_loc & nmn = nmn_loc
c      IF (IMACH==3.AND.NSPMD>1) THEN
c        NSN = IPARI(16)
c        NMN = IPARI(18)
c      ENDIF
C
        IF(ILEV==1)THEN
          CALL I2UPDK1(NSN       ,NMN       ,INTBUF_TAB%IRECTM,
     1      INTBUF_TAB%DPARA,INTBUF_TAB%MSR,INTBUF_TAB%NSV,INTBUF_TAB%IRTLM,
     2      MS        ,X         ,WEIGHT    ,ITAB      ,
     3      NSC2      ,ISIJ2     ,NSS2      ,ISS2      ,
     4                    IKC ,NDOF ,NDDL,IDDL    ,IADK  ,
     5                    JDIK ,DIAG_K ,LT_K  ,B)
        ELSE
          CALL I2UPDK0(NSN       ,NMN       ,INTBUF_TAB%IRECTM,
     1      INTBUF_TAB%CSTS,INTBUF_TAB%MSR,INTBUF_TAB%NSV,INTBUF_TAB%IRTLM,
     2      MS        ,X         ,WEIGHT    ,ITAB      ,
     3      NSC2      ,ISIJ2     ,NSS2      ,ISS2      ,
     4                    IKC ,NDOF ,NDDL,IDDL    ,IADK  ,
     5                    JDIK ,DIAG_K ,LT_K  ,B)
        ENDIF
C
        RETURN
      END
Chd|====================================================================
Chd|  I2_IMPM                       source/interfaces/interf/i2_imp1.F
Chd|-- called by -----------
Chd|        I2_IMP0                       source/interfaces/interf/i2_imp0.F
Chd|-- calls ---------------
Chd|        GET_KIJ                       source/implicit/imp_glob_k.F
Chd|        I2UPDKM0                      source/interfaces/interf/i2_imp1.F
Chd|        I2UPDKM1                      source/interfaces/interf/i2_imp1.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I2_IMPM(IPARI,INTBUF_TAB,NMC2 ,IMIJ2,
     .                   X    ,MS  ,IN   ,WEIGHT  ,
     .                   NDOF ,NDDL,IDDL    ,IADK  ,JDIK ,
     .                   LT_K ,DIAG_K)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE INTBUFDEF_MOD
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
        INTEGER IPARI(NPARI,*), WEIGHT(*),
     .          NMC2,IMIJ2(4,*)
        INTEGER NDDL,IADK(*),JDIK(*),NDOF(*),IDDL(*)
C     REAL
        my_real
     .     X(*),MS(*),IN(*),LT_K(*),DIAG_K(*)

        TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER
     .     K10, K11, K12, K13, K14, KFI, J10, J11, J12, J21, J22,
     .     L10, L11, L12, L13, L14, LFI, M10, M11, M12, M21, M22,
     .     JI,JFI,JI1,NSN,NSN1,ILEV,N1,N2,NS1,NS2,NI,NJ,I,IR
        my_real
     .     KDD(6,6)
C-----------------------------------------------
        DO I=1,NMC2
          N1=IMIJ2(1,I)
          N2=IMIJ2(2,I)
          NS1=IMIJ2(3,I)
          NS2=IMIJ2(4,I)
          NSN = IPARI(5,N1)
          JI=IPARI(1,N1)
          K10=JI
          K11=K10+4*IPARI(3,N1)
          K12=K11+4*IPARI(4,N1)
          K13=K12+NSN
          K14=K13+IPARI(6,N1)
          NI=INTBUF_TAB(N1)%NSV(NS1)
          J10=IPARI(2,N1)
          J11=J10+1
          J12=J11+NPARIR
          J21=J12+2*NSN
          NSN1 = IPARI(5,N2)
          JI1=IPARI(1,N2)
          L10=JI1
          L11=L10+4*IPARI(3,N2)
          L12=L11+4*IPARI(4,N2)
          L13=L12+NSN1
          L14=L13+IPARI(6,N2)
          NJ=INTBUF_TAB(N2)%IRECTM(NS2)
          M10=IPARI(2,N2)
          M11=M10+1
          M12=M11+NPARIR
          M21=M12+2*NSN1
C------supposant ILEV est le meme pour NI,NJ---
          ILEV  =IPARI(20,N1)
          IF (NDOF(NI)>0.AND.NDOF(NJ)>0) THEN
            CALL GET_KIJ(NI,NJ,IDDL,IADK,JDIK,LT_K,KDD,NDOF(NI),NDOF(NJ),IR)
            IF(ILEV==1)THEN
              CALL I2UPDKM1(NS1,INTBUF_TAB(N1)%IRECTM,INTBUF_TAB(N1)%DPARA,INTBUF_TAB(N1)%NSV,INTBUF_TAB(N1)%IRTLM,
     .                      NS2,INTBUF_TAB(N2)%IRECTM,INTBUF_TAB(N2)%DPARA,INTBUF_TAB(N2)%NSV,INTBUF_TAB(N2)%IRTLM,
     .                      X  ,KDD       ,NDOF      ,IDDL      ,IADK      ,
     .                      JDIK,LT_K     ,DIAG_K    )
            ELSE
              CALL I2UPDKM0(NS1,INTBUF_TAB(N1)%IRECTM,INTBUF_TAB(N1)%CSTS,INTBUF_TAB(N1)%NSV,INTBUF_TAB(N1)%IRTLM,
     .                      NS2,INTBUF_TAB(N2)%IRECTM,INTBUF_TAB(N2)%CSTS,INTBUF_TAB(N2)%NSV,INTBUF_TAB(N2)%IRTLM,
     .                      X  ,KDD       ,NDOF      ,IDDL      ,IADK      ,
     .                      JDIK,LT_K     ,DIAG_K    )
            ENDIF
          ENDIF
        ENDDO
C
        RETURN
      END
Chd|====================================================================
Chd|  I2UPDK0                       source/interfaces/interf/i2_imp1.F
Chd|-- called by -----------
Chd|        I2_IMP1                       source/interfaces/interf/i2_imp1.F
Chd|-- calls ---------------
Chd|        GET_KII                       source/implicit/imp_glob_k.F
Chd|        GET_KIJ                       source/implicit/imp_glob_k.F
Chd|        PRINT_WKIJ                    source/implicit/imp_glob_k.F
Chd|        PUT_KII                       source/implicit/imp_glob_k.F
Chd|        PUT_KIJ                       source/implicit/imp_glob_k.F
Chd|        UPDKB_RB                      source/constraints/general/rbody/rby_imp0.F
Chd|        UPDKB_RB1                     source/constraints/general/rbody/rby_imp0.F
Chd|        UPDKB_RB2                     source/constraints/general/rbody/rby_imp0.F
Chd|        UPDKDD                        source/interfaces/interf/i2_imp1.F
Chd|        UPDKDD1                       source/interfaces/interf/i2_imp1.F
Chd|        UPDKDD2                       source/interfaces/interf/i2_imp1.F
Chd|====================================================================
      SUBROUTINE I2UPDK0(NSN,NMN,IRECT,CRST,MSR ,
     1                   NSV,IRTL,MS    ,X  ,WEIGHT,
     2                   ITAB,NSC, ISI ,NS ,NODS,
     3                  IKC ,NDOF ,NDDL,IDDL    ,IADK  ,
     4                  JDIK ,DIAG_K ,LT_K  ,B)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NSN, NMN,
     .     IRECT(4,*), MSR(*), NSV(*), IRTL(*),WEIGHT(*),
     .          NSC,ISI(2,NSC) ,NS(*),NODS(*),ITAB(*)
        INTEGER NDDL,IADK(*),JDIK(*),NDOF(*),IDDL(*),IKC(*)
C     REAL
        my_real
     .     CRST(2,*),X(3,*),MS(*),DIAG_K(*),LT_K(*),B(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER NIR, I, J, J1, J2, J3, J4, K, JD, II, L, JJ,
     .          I1,ID,NL,NI,NJ,NIDOF,ND,NDI,NDJ,NDM,NM,L1,NM1,
     .          NIR1,IR
C     REAL
        my_real
     .     H(4,NSN),H2(4), SS, TT, SP,SM,TP,TM,KDD(6,6),BD(6),
     .     KII(6,6),BI(6),XS0(NSN),YS0(NSN),ZS0(NSN),
     .     XS,YS,ZS,XS1,YS1,ZS1,FACM,NUN
C------------------------------------
C     VITESSES DES NOEUDS SECONDS
C------------------------------------
C-------pour simplement etre coherent avec INTTI2, on ne distinque pas 3n,4n
C-------on est oblige de distinque quand meme, sinon la matrice n'est plus positive
C      FACM = ONE / NIR
        NUN=-ONE
        J1=0
        DO II=1,NSN
          I=NSV(II)
          L=IRTL(II)
          IF (NDOF(I)>0) THEN
            DO K=1,NDOF(I)
              ID = IDDL(I)+K
              IKC(ID)=5
              BD(K)=B(ID)
            ENDDO
            DO K=NDOF(I)+1,6
              BD(K)=ZERO
            ENDDO
            CALL GET_KII(I ,IDDL ,IADK,DIAG_K,LT_K ,KDD,NDOF(I))
C
            SS=CRST(1,II)
            TT=CRST(2,II)
            SS = MIN(ONE,SS)
            TT = MIN(ONE,TT)
            SS = MAX(NUN,SS)
            TT = MAX(NUN,TT)
            SP=ONE + SS
            SM=ONE - SS
            IF (IRECT(3,L)==IRECT(4,L)) THEN
              NIR=3
              TP=FOURTH*(ONE + TT)
              TM=FOURTH*(ONE - TT)
              H(1,II)=TM*SM
              H(2,II)=TM*SP
              H(3,II)=ONE-H(1,II)-H(2,II)
              H2(1)=H(1,II)*H(1,II)
              H2(2)=H(2,II)*H(2,II)
              H2(3)=H(3,II)*H(3,II)
            ELSE
              NIR=4
              TP=FOURTH*(ONE + TT)
              TM=FOURTH*(ONE - TT)
              H(1,II)=TM*SM
              H(2,II)=TM*SP
              H(3,II)=TP*SP
              H(4,II)=TP*SM
              H2(1)=H(1,II)*H(1,II)
              H2(2)=H(2,II)*H(2,II)
              H2(3)=H(3,II)*H(3,II)
              H2(4)=H(4,II)*H(4,II)
            ENDIF
            NDM = 0
            DO J=1,NIR
              NJ=IRECT(J,L)
              NDM = MAX(NDM,NDOF(NJ))
            ENDDO
C-------NDOF(M)> 3 comme rigid body---
            IF (NDM==6) THEN
              XS0(II)=ZERO
              YS0(II)=ZERO
              ZS0(II)=ZERO
              DO J=1,NIR
                NJ=IRECT(J,L)
                XS0(II)=XS0(II)+X(1,NJ)*H(J,II)
                YS0(II)=YS0(II)+X(2,NJ)*H(J,II)
                ZS0(II)=ZS0(II)+X(3,NJ)*H(J,II)
              ENDDO
              XS=X(1,I)-XS0(II)
              YS=X(2,I)-YS0(II)
              ZS=X(3,I)-ZS0(II)
              CALL UPDKB_RB(NDOF(I),XS,YS,ZS,KDD,BD)
            ENDIF
CC-------Update K(main node),B---
            DO J=1,NIR
              NJ=IRECT(J,L)
              ND = MIN(NDM,NDOF(NJ))
              CALL UPDKDD(ND,KDD,KII,H2(J),1)
              CALL PUT_KII(NJ,IDDL ,IADK,DIAG_K,LT_K ,KII,ND)
              DO K=1,ND
                ID = IDDL(NJ)+K
                B(ID) = B(ID) + H(J,II)*BD(K)
              ENDDO
              DO I1=J+1,NIR
                NM=IRECT(I1,L)
                TM=H(J,II)*H(I1,II)
                ND = MIN(ND,NDOF(NM))
                CALL UPDKDD(ND,KDD,KII,TM,0)
                CALL PUT_KIJ(NJ,NM,IDDL,IADK,JDIK,LT_K,KII,ND,ND,IR)
                IF (IR==1) CALL PRINT_WKIJ(ITAB(NJ) ,ITAB(NM) ,2 )
              ENDDO
            ENDDO
C--------no diag--Kjm=sum(KjsCsm)--
            DO I1 = 1,NS(II)
              NI=NODS(J1+I1)
              NIDOF=NDOF(NI)
              CALL GET_KIJ(NI,I,IDDL,IADK,JDIK,LT_K,KDD,NIDOF,NDOF(I),IR)
              IF (IR==1) CALL PRINT_WKIJ(ITAB(NI) ,ITAB(I) ,2 )
C------- Update ---
              IF (NDM==6) CALL UPDKB_RB1(NIDOF,NDOF(I),XS,YS,ZS,KDD)
              DO J=1,NIR
                NJ=IRECT(J,L)
                NDI = MIN(NDM,NIDOF)
                NDJ = MIN(NDM,NDOF(NJ))
                IF (NI==NJ.AND.NDJ>0) THEN
                  CALL UPDKDD1(NDI,NDOF(I),KDD,KII,H(J,II),1)
                  CALL PUT_KII(NJ,IDDL ,IADK,DIAG_K,LT_K ,KII,NDJ)
                ELSEIF (NDJ>0) THEN
                  CALL UPDKDD1(NDI,NDOF(I),KDD,KII,H(J,II),0)
                  CALL PUT_KIJ(NI,NJ,IDDL,IADK,JDIK,LT_K,KII,NDI,NDJ,IR)
                  IF (IR==1) CALL PRINT_WKIJ(ITAB(NI) ,ITAB(NJ) ,2 )
                ENDIF
              ENDDO
            ENDDO
            J1=J1+NS(II)
          ENDIF
        ENDDO
C--------due au coupled block KIJ--
        DO I=1,NSC
          II =ISI(1,I)
          JJ =ISI(2,I)
          NI =NSV(II)
          NJ =NSV(JJ)
          L=IRTL(II)
          L1=IRTL(JJ)
          CALL GET_KIJ(NI,NJ,IDDL,IADK,JDIK,LT_K,KDD,NDOF(NI),NDOF(NJ),IR)
          IF (IR==1) CALL PRINT_WKIJ(ITAB(NI) ,ITAB(NJ) ,2 )
          IF (IRECT(3,L)==IRECT(4,L)) THEN
            NIR=3
          ELSE
            NIR=4
          ENDIF
          IF (L==L1) THEN
            NDM = 0
            DO J=1,NIR
              NM=IRECT(J,L)
              NDM = MAX(NDM,NDOF(NM))
            ENDDO
            IF (NDM==6) THEN
              XS=X(1,NI)-XS0(II)
              YS=X(2,NI)-YS0(II)
              ZS=X(3,NI)-ZS0(II)
              XS1=X(1,NJ)-XS0(JJ)
              YS1=X(2,NJ)-YS0(JJ)
              ZS1=X(3,NJ)-ZS0(JJ)
              CALL UPDKB_RB2(NDOF(NI),NDOF(NJ),XS,YS,ZS,XS1,YS1,ZS1,KDD,0)
            ENDIF
            DO J=1,NIR
              NM=IRECT(J,L)
              TM=H(J,II)*H(J,JJ)
              CALL UPDKDD2(NDM,KDD,KII,TM,TM)
              CALL PUT_KII(NM ,IDDL ,IADK,DIAG_K,LT_K,KII,NDOF(NM))
              DO J1=J+1,NIR
                NM1=IRECT(J1,L)
                NDM = MIN(NDOF(NM),NDOF(NM1))
                IF (NDM>0) THEN
                  TM=H(J,II)*H(J1,JJ)
                  TP=H(J,JJ)*H(J1,II)
                  CALL UPDKDD2(NDM,KDD,KII,TM,TP)
C--------update --
                  CALL PUT_KIJ(NM,NM1,IDDL,IADK,JDIK,LT_K,KII,
     .                        NDOF(NM),NDOF(NM1),IR)
                  IF (IR==1) CALL PRINT_WKIJ(ITAB(NM) ,ITAB(NM1) ,2 )
                ENDIF
              ENDDO
            ENDDO
C----- (L/=L1)-----
          ELSE
            NDM = 0
            IF (IRECT(3,L1)==IRECT(4,L1)) THEN
              NIR1=3
            ELSE
              NIR1=4
            ENDIF
            DO J=1,MAX(NIR,NIR1)
              NM=IRECT(J,L)
              NM1=IRECT(J,L1)
              NDM = MAX(NDM,NDOF(NM),NDOF(NM1))
            ENDDO
            IF (NDM==6) THEN
              XS=X(1,NI)-XS0(II)
              YS=X(2,NI)-YS0(II)
              ZS=X(3,NI)-ZS0(II)
              XS1=X(1,NJ)-XS0(JJ)
              YS1=X(2,NJ)-YS0(JJ)
              ZS1=X(3,NJ)-ZS0(JJ)
              CALL UPDKB_RB2(NDOF(NI),NDOF(NJ),XS,YS,ZS,XS1,YS1,ZS1,KDD,0)
            ENDIF
            DO J=1,NIR
              NM=IRECT(J,L)
              DO J1=1,NIR1
                NM1=IRECT(J1,L1)
                TM=H(J,II)*H(J1,JJ)
C--------update --
                NDM = MIN(NDOF(NM),NDOF(NM1))
                IF (NM==NM1.AND.NDM>0) THEN
                  CALL UPDKDD1(NDOF(NI),NDOF(NJ),KDD,KII,TM,1)
                  CALL PUT_KII(NM ,IDDL ,IADK,DIAG_K,LT_K,KII,NDOF(NM))
                ELSEIF (NDM>0) THEN
                  CALL UPDKDD1(NDOF(NI),NDOF(NJ),KDD,KII,TM,0)
                  CALL PUT_KIJ(NM,NM1,IDDL,IADK,JDIK,LT_K,KII,
     .                         NDOF(NM),NDOF(NM1),IR)
                  IF (IR==1) CALL PRINT_WKIJ(ITAB(NM) ,ITAB(NM1) ,2 )
                ENDIF
              ENDDO
            ENDDO
          ENDIF
        ENDDO
C
        RETURN
      END
Chd|====================================================================
Chd|  I2UPDKM0                      source/interfaces/interf/i2_imp1.F
Chd|-- called by -----------
Chd|        I2_IMPM                       source/interfaces/interf/i2_imp1.F
Chd|-- calls ---------------
Chd|        PUT_KII                       source/implicit/imp_glob_k.F
Chd|        PUT_KIJ                       source/implicit/imp_glob_k.F
Chd|        UPDKB_RB2                     source/constraints/general/rbody/rby_imp0.F
Chd|        UPDKDD1                       source/interfaces/interf/i2_imp1.F
Chd|====================================================================
      SUBROUTINE I2UPDKM0(NS1,IRECT,CRST,NSV,IRTL,
     .                    NS2,IRECT1,CRST1,NSV1,IRTL1,
     .                    X  ,KDD   ,NDOF ,IDDL ,IADK    ,
     .                    JDIK,LT_K ,DIAG_K)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NS1,IRECT(4,*), NSV(*), IRTL(*),
     .          NS2,IRECT1(4,*), NSV1(*), IRTL1(*),
     .          NDOF(*) ,IDDL(*) ,IADK(*),JDIK(*)
C     REAL
        my_real
     .     CRST(2,*),CRST1(2,*),X(3,*),KDD(6,6),LT_K(*),DIAG_K(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER  J, L, J1,L1,NI,NJ,K,NIR,NIR1,NM,NM1,NDM,IR
C     REAL
        my_real
     .     H(4),H1(4),SS, TT, SP,SM,TP,TM,NUN,
     .     KII(6,6),XS,YS,ZS,XS1,YS1,ZS1,XM0,YM0,ZM0,XM1,YM1,ZM1
C------------------------------------
        NUN=-ONE
        L=IRTL(NS1)
        L1=IRTL1(NS2)
        SS=CRST(1,NS1)
        TT=CRST(2,NS1)
        SS = MIN(ONE,SS)
        TT = MIN(ONE,TT)
        SS = MAX(NUN,SS)
        TT = MAX(NUN,TT)
        SP=ONE + SS
        SM=ONE - SS
        IF (IRECT(3,L)==IRECT(4,L)) THEN
          NIR=3
          TP=FOURTH*(ONE + TT)
          TM=FOURTH*(ONE - TT)
          H(1)=TM*SM
          H(2)=TM*SP
          H(3)=ONE-H(1)-H(2)
        ELSE
          NIR=4
          TP=FOURTH*(ONE + TT)
          TM=FOURTH*(ONE - TT)
          H(1)=TM*SM
          H(2)=TM*SP
          H(3)=TP*SP
          H(4)=TP*SM
        ENDIF
        XM0=ZERO
        YM0=ZERO
        ZM0=ZERO
        DO J=1,NIR
          NJ=IRECT(J,L)
          XM0=XM0+X(1,NJ)*H(J)
          YM0=YM0+X(2,NJ)*H(J)
          ZM0=ZM0+X(3,NJ)*H(J)
        ENDDO
C---------NJ------
        SS=CRST1(1,NS2)
        TT=CRST1(2,NS2)
        SS = MIN(ONE,SS)
        TT = MIN(ONE,TT)
        SS = MAX(NUN,SS)
        TT = MAX(NUN,TT)
        SP=ONE + SS
        SM=ONE - SS
        IF (IRECT1(3,L1)==IRECT1(4,L1)) THEN
          NIR1=3
          TP=FOURTH*(ONE + TT)
          TM=FOURTH*(ONE - TT)
          H1(1)=TM*SM
          H1(2)=TM*SP
          H1(3)=ONE-H1(1)-H1(2)
        ELSE
          NIR1=4
          TP=FOURTH*(ONE + TT)
          TM=FOURTH*(ONE - TT)
          H1(1)=TM*SM
          H1(2)=TM*SP
          H1(3)=TP*SP
          H1(4)=TP*SM
        ENDIF
        XM1=ZERO
        YM1=ZERO
        ZM1=ZERO
        DO J=1,NIR1
          NJ=IRECT1(J,L1)
          XM1=XM1+X(1,NJ)*H1(J)
          YM1=YM1+X(2,NJ)*H1(J)
          ZM1=ZM1+X(3,NJ)*H1(J)
        ENDDO
        NI = NSV(NS1)
        NJ = NSV1(NS2)
        NDM = MAX(NDOF(NI),NDOF(NJ))
        DO J=1,MAX(NIR,NIR1)
          NM=IRECT(J,L)
          NM1=IRECT1(J,L1)
          NDM = MAX(NDM,NDOF(NM),NDOF(NM1))
        ENDDO
C-------NDOF(M)> 3 comme rigid body---
        IF (NDM==6) THEN
          XS=X(1,NI)-XM0
          YS=X(2,NI)-YM0
          ZS=X(3,NI)-ZM0
          XS1=X(1,NJ)-XM1
          YS1=X(2,NJ)-YM1
          ZS1=X(3,NJ)-ZM1
          CALL UPDKB_RB2(NDOF(NI),NDOF(NJ),XS,YS,ZS,XS1,YS1,ZS1,KDD,0)
        ENDIF
        DO J=1,NIR
          NM=IRECT(J,L)
          DO J1=1,NIR1
            NM1=IRECT1(J1,L1)
            TM=H(J)*H1(J1)
C--------update --
            IF (NM==NM1) THEN
              CALL UPDKDD1(NDOF(NI),NDOF(NJ),KDD,KII,TM,1)
              CALL PUT_KII(NM ,IDDL ,IADK,DIAG_K,LT_K,KII,NDOF(NM))
            ELSE
              CALL UPDKDD1(NDOF(NI),NDOF(NJ),KDD,KII,TM,0)
              CALL PUT_KIJ(NM,NM1,IDDL,IADK,JDIK,LT_K,KII,
     .                      NDOF(NM),NDOF(NM1),IR)
            ENDIF
          ENDDO
        ENDDO
C
        RETURN
      END
Chd|====================================================================
Chd|  I2UPDK1                       source/interfaces/interf/i2_imp1.F
Chd|-- called by -----------
Chd|        I2_IMP1                       source/interfaces/interf/i2_imp1.F
Chd|-- calls ---------------
Chd|        GET_KII                       source/implicit/imp_glob_k.F
Chd|        GET_KIJ                       source/implicit/imp_glob_k.F
Chd|        I2MATC                        source/interfaces/interf/i2_imp1.F
Chd|        PRINT_WKIJ                    source/implicit/imp_glob_k.F
Chd|        PUT_KII                       source/implicit/imp_glob_k.F
Chd|        PUT_KIJ                       source/implicit/imp_glob_k.F
Chd|        UPDK1_II                      source/interfaces/interf/i2_imp1.F
Chd|        UPDK1_IJ                      source/interfaces/interf/i2_imp1.F
Chd|        UPDK1_JJ                      source/interfaces/interf/i2_imp1.F
Chd|====================================================================
      SUBROUTINE I2UPDK1(NSN,NMN,IRECT,DPARA,MSR ,
     1                  NSV,IRTL,MS    ,X  ,WEIGHT,
     2                  ITAB,NSC, ISI ,NS ,NODS,
     3                  IKC ,NDOF ,NDDL,IDDL    ,IADK  ,
     4                  JDIK ,DIAG_K ,LT_K  ,B)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NSN, NMN,
     .     IRECT(4,*), MSR(*), NSV(*), IRTL(*),WEIGHT(*),
     .          NSC,ISI(2,NSC) ,NS(*),NODS(*),ITAB(*)
        INTEGER NDDL,IADK(*),JDIK(*),NDOF(*),IDDL(*),IKC(*)
C     REAL
        my_real
     .     DPARA(7,*),X(*),MS(*),DIAG_K(*),LT_K(*),B(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER NIR(NSN),I, J, K, JD, II, L, JJ,I1,ID,NL,
     .          NI,NJ,J1,NIDOF,ND,NM,IDOF,L1,NM1,NDM,ND1,IR
C     REAL
        my_real
     .     RJ(9,4,NSN),RJT(9,4,NSN)
        my_real
     .     KDD(6,6),BD(6),KII(6,6),BI(6),XS,YS,ZS,XS1,YS1,ZS1
C------------------------------------
C     VITESSES DES NOEUDS SECONDS
C------------------------------------
        CALL I2MATC(NSN,IRECT,DPARA,NSV,IRTL,X,NIR,RJ ,RJT  )
        J1=0
        ND = 3
        NDM = 3
        DO II=1,NSN
          I=NSV(II)
          IDOF=NDOF(I)
          IF (IDOF>0) THEN
            L=IRTL(II)
            DO K=1,IDOF
              ID = IDDL(I)+K
              IKC(ID)=5
              BD(K)=B(ID)
            ENDDO
            DO K=IDOF+1,6
              BD(K)=ZERO
            ENDDO
            CALL GET_KII(I ,IDDL ,IADK,DIAG_K,LT_K ,KDD,IDOF)
            DO J=1,IDOF
              DO K=J+1,IDOF
                KDD(K,J)=KDD(J,K)
              ENDDO
            ENDDO
C-------Update K(main node),B---
            DO J=1,NIR(II)
              NJ=IRECT(J,L)
              ND=MIN(NDM,NDOF(NJ))
              CALL UPDK1_II(IDOF,RJ(1,J,II),RJT(1,J,II),KDD,KII,BD,BI)
              CALL PUT_KII(NJ ,IDDL ,IADK,DIAG_K,LT_K ,KII,ND)
              DO K=1,ND
                ID = IDDL(NJ)+K
                B(ID) = B(ID) + BI(K)
              ENDDO
              DO I1=J+1,NIR(II)
                NM=IRECT(I1,L)
                ND1=MIN(NDM,NDOF(NJ))
                CALL UPDK1_IJ(IDOF,IDOF,RJ(1,J,II),RJT(1,J,II),
     1                        RJ(1,I1,II),RJT(1,I1,II),KDD,KII,0)
                CALL PUT_KIJ(NJ,NM,IDDL,IADK,JDIK,LT_K,KII,ND,ND1,IR)
                IF (IR==1) CALL PRINT_WKIJ(ITAB(NJ) ,ITAB(NM) ,2 )
              ENDDO
            ENDDO
C--------no diag--Kmj=sum(KjsCsm)--
            DO I1 = 1,NS(II)
              NI=NODS(J1+I1)
              NIDOF=NDOF(NI)
              CALL GET_KIJ(NI,I,IDDL,IADK,JDIK,LT_K,KDD,NIDOF,IDOF,IR)
              IF (IR==1) CALL PRINT_WKIJ(ITAB(NI) ,ITAB(I) ,2 )
C------- Update ---
              DO J=1,NIR(II)
                NJ=IRECT(J,L)
                ND=MIN(NDM,NDOF(NJ))
                IF (NI==NJ.AND.ND>0) THEN
                  CALL UPDK1_JJ(NIDOF,IDOF,RJ(1,J,II),RJT(1,J,II),KDD,KII)
                  DO K=1,3
                    DO L1=1,3
                      KII(K,L1)=KII(K,L1)+KII(L1,K)
                    ENDDO
                  ENDDO
                  CALL PUT_KII(NJ ,IDDL ,IADK,DIAG_K,LT_K ,KII,ND)
                ELSEIF (ND>0) THEN
                  CALL UPDK1_JJ(NIDOF,IDOF,RJ(1,J,II),RJT(1,J,II),KDD,KII)
                  CALL PUT_KIJ(NI,NJ,IDDL,IADK,JDIK,LT_K,KII,NIDOF,ND,IR)
                  IF (IR==1) CALL PRINT_WKIJ(ITAB(NI) ,ITAB(NJ) ,2 )
                ENDIF
              ENDDO
            ENDDO
            J1=J1+NS(II)
          ENDIF
        ENDDO
C--------due au coupled block KIJ-attension ISI different que rigid body-
        DO I=1,NSC
          II =ISI(1,I)
          JJ =ISI(2,I)
          NI =NSV(II)
          NJ =NSV(JJ)
          L=IRTL(II)
          L1=IRTL(JJ)
          NIDOF=NDOF(NI)
          IDOF=NDOF(NJ)
          CALL GET_KIJ(NI,NJ,IDDL,IADK,JDIK,LT_K,KDD,NIDOF,IDOF,IR)
          IF (IR==1) CALL PRINT_WKIJ(ITAB(NI) ,ITAB(NJ) ,2 )
          IF (L==L1) THEN
            DO J=1,NIR(II)
              NM=IRECT(J,L)
C--------update --
              ND=MIN(NDM,NDOF(NM))
              CALL UPDK1_IJ(NIDOF,IDOF,RJ(1,J,II),RJT(1,J,II),
     1                       RJ(1,J,JJ),RJT(1,J,JJ),KDD,KII,1)
              CALL PUT_KII(NM ,IDDL ,IADK,DIAG_K,LT_K ,KII,ND)
              DO J1=J+1,NIR(JJ)
                NM1=IRECT(J1,L)
                ND1=MIN(NDM,NDOF(NM1))
                IF (ND1>0) THEN
                  CALL UPDK1_IJ(NIDOF,IDOF,RJ(1,J,II),RJT(1,J,II),
     1                          RJ(1,J1,JJ),RJT(1,J1,JJ),KDD,KII,0)
                  CALL PUT_KIJ(NM,NM1,IDDL,IADK,JDIK,LT_K,KII,ND,ND1,IR)
                  IF (IR==1) CALL PRINT_WKIJ(ITAB(NM) ,ITAB(NM1) ,2 )
                  CALL UPDK1_IJ(NIDOF,IDOF,RJ(1,J1,II),RJT(1,J1,II),
     1                          RJ(1,J,JJ),RJT(1,J,JJ),KDD,KII,0)
                  CALL PUT_KIJ(NM1,NM,IDDL,IADK,JDIK,LT_K,KII,ND1,ND,IR)
                  IF (IR==1) CALL PRINT_WKIJ(ITAB(NM1) ,ITAB(NM) ,2 )
                ENDIF
              ENDDO
            ENDDO
          ELSE
            DO J=1,NIR(II)
              NM=IRECT(J,L)
              ND=MIN(NDM,NDOF(NM))
              DO J1=1,NIR(JJ)
                NM1=IRECT(J1,L1)
                ND1=MIN(NDM,NDOF(NM1))
                IF (NM==NM1.AND.ND1>0) THEN
                  CALL UPDK1_IJ(NIDOF,IDOF,RJ(1,J,II),RJT(1,J,II),
     1                         RJ(1,J1,JJ),RJT(1,J1,JJ),KDD,KII,1)
                  CALL PUT_KII(NM ,IDDL ,IADK,DIAG_K,LT_K ,KII,ND)
                ELSEIF (ND1>0) THEN
                  CALL UPDK1_IJ(NIDOF,IDOF,RJ(1,J,II),RJT(1,J,II),
     1                         RJ(1,J1,JJ),RJT(1,J1,JJ),KDD,KII,0)
                  CALL PUT_KIJ(NM,NM1,IDDL,IADK,JDIK,LT_K,KII,ND,ND1,IR)
                  IF (IR==1) CALL PRINT_WKIJ(ITAB(NM) ,ITAB(NM1) ,2 )
                ENDIF
              ENDDO
            ENDDO
          ENDIF
        ENDDO
C
        RETURN
      END
C-------------produit {K'}=[CDI]^t[K][CDI] B'=[CDI]^tB with [CDI]=-[RJT RJ]^t
Chd|====================================================================
Chd|  UPDK1_II                      source/interfaces/interf/i2_imp1.F
Chd|-- called by -----------
Chd|        I2UPDK1                       source/interfaces/interf/i2_imp1.F
Chd|        I2_FRK1                       source/interfaces/interf/i2_imp1.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE UPDK1_II(NDL,RJ,RJT,KDD,K,BD,B)
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
     .      B(3),K(6,6),RJ(3,3), RJT(3,3), BD(6),KDD(6,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, J
C     REAL
        my_real
     .     K1(3,3)
C------------------------------------
        DO I=1,3
          DO J=I,3
            K(I,J)=RJT(1,I)*(KDD(1,1)*RJT(1,J)+
     1                       KDD(1,2)*RJT(2,J)+KDD(1,3)*RJT(3,J))+
     2             RJT(2,I)*(KDD(1,2)*RJT(1,J)+
     3                       KDD(2,2)*RJT(2,J)+KDD(2,3)*RJT(3,J))+
     4             RJT(3,I)*(KDD(1,3)*RJT(1,J)+
     5                       KDD(2,3)*RJT(2,J)+KDD(3,3)*RJT(3,J))
          ENDDO
        ENDDO
        DO I=1,3
          B(I)=RJT(1,I)*BD(1)+RJT(2,I)*BD(2)+RJT(3,I)*BD(3)
        ENDDO
C
        IF (NDL==6) THEN
          DO I=1,3
            DO J=1,3
              K1(I,J)= RJT(1,I)*(KDD(1,4)*RJ(1,J)+
     1                      KDD(1,5)*RJ(2,J)+KDD(1,6)*RJ(3,J))+
     2                 RJT(2,I)*(KDD(2,4)*RJ(1,J)+
     3                      KDD(2,5)*RJ(2,J)+KDD(2,6)*RJ(3,J))+
     4                 RJT(3,I)*(KDD(3,4)*RJ(1,J)+
     5                      KDD(3,5)*RJ(2,J)+KDD(3,6)*RJ(3,J))
            ENDDO
          ENDDO
          DO I=1,3
            DO J=I,3
              K(I,J)= K(I,J)+K1(I,J)+K1(J,I)+
     1               RJ(1,I)*(KDD(4,4)*RJ(1,J)+KDD(4,5)*RJ(2,J)+
     2                        KDD(4,6)*RJ(3,J) ) +
     3               RJ(2,I)*(KDD(4,5)*RJ(1,J)+KDD(5,5)*RJ(2,J)+
     4                        KDD(5,6)*RJ(3,J) ) +
     5               RJ(3,I)*(KDD(4,6)*RJ(1,J)+KDD(5,6)*RJ(2,J)+
     6                        KDD(6,6)*RJ(3,J) )
            ENDDO
          ENDDO
          DO I=1,3
            B(I)=B(I)+RJ(1,I)*BD(4)+RJ(2,I)*BD(5)+RJ(3,I)*BD(6)
          ENDDO
        ENDIF
C
        RETURN
      END
C-------------produit {K'}=[CDI]^t[K][CDI] with [CDI]=-[RJT RJ]^t
Chd|====================================================================
Chd|  UPFR1_II                      source/interfaces/interf/i2_imp1.F
Chd|-- called by -----------
Chd|        I2_FRUP1                      source/interfaces/interf/i2_imp1.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE UPFR1_II(RJ,RJT,KII,K)
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
     .      K(6),RJ(3,3), RJT(3,3), KII(6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, J
C     REAL
        my_real
     .     KDD(3,3)
C------------------------------------
        DO I=1,3
          KDD(I,I)=KII(I)
        ENDDO
        KDD(1,2)=KII(4)
        KDD(1,3)=KII(5)
        KDD(2,3)=KII(6)
        KDD(2,1)=KDD(1,2)
        KDD(3,1)=KDD(1,3)
        KDD(3,2)=KDD(2,3)
C
        DO I=1,3
          J = I
          K(I)=K(I)+RJT(1,I)*(KDD(1,1)*RJT(1,J)+
     1                      KDD(1,2)*RJT(2,J)+KDD(1,3)*RJT(3,J))+
     2            RJT(2,I)*(KDD(1,2)*RJT(1,J)+
     3                      KDD(2,2)*RJT(2,J)+KDD(2,3)*RJT(3,J))+
     4            RJT(3,I)*(KDD(1,3)*RJT(1,J)+
     5                      KDD(2,3)*RJT(2,J)+KDD(3,3)*RJT(3,J))
        ENDDO
C
        RETURN
      END
C-------------produit {K'}=[CDI(I)]^t[KIJ][CDI(J)] with [CDI]=-[RJT RJ]^t
Chd|====================================================================
Chd|  UPDK1_IJ                      source/interfaces/interf/i2_imp1.F
Chd|-- called by -----------
Chd|        I2UPDK1                       source/interfaces/interf/i2_imp1.F
Chd|        I2UPDKM1                      source/interfaces/interf/i2_imp1.F
Chd|        I2_FRK1                       source/interfaces/interf/i2_imp1.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE UPDK1_IJ(NDI,NDJ,R1J,R1JT,R2J,R2JT,KDD,KII,ISYM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NDI,NDJ,ISYM
C     REAL
        my_real
     .      R1J(3,3), R1JT(3,3),R2J(3,3), R2JT(3,3), KDD(6,6),
     .      KII(6,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, J
C     REAL
        my_real
     .     K(3,3)
C------------------------------------
        DO I=1,3
          DO J=1,3
            K(I,J)=R1JT(1,I)*(KDD(1,1)*R2JT(1,J)+
     1                       KDD(1,2)*R2JT(2,J)+KDD(1,3)*R2JT(3,J))+
     2             R1JT(2,I)*(KDD(2,1)*R2JT(1,J)+
     3                       KDD(2,2)*R2JT(2,J)+KDD(2,3)*R2JT(3,J))+
     4             R1JT(3,I)*(KDD(3,1)*R2JT(1,J)+
     5                       KDD(3,2)*R2JT(2,J)+KDD(3,3)*R2JT(3,J))
          ENDDO
        ENDDO
C
        IF (NDI==6) THEN
          DO I=1,3
            DO J=1,3
              K(I,J)=K(I,J)+ R1J(1,I)*(KDD(4,1)*R2JT(1,J)+
     1                      KDD(4,2)*R2JT(2,J)+KDD(4,3)*R2JT(3,J))+
     2                 R1J(2,I)*(KDD(5,1)*R2JT(1,J)+
     3                      KDD(5,2)*R2JT(2,J)+KDD(5,3)*R2JT(3,J))+
     4                 R1J(3,I)*(KDD(6,1)*R2JT(1,J)+
     5                      KDD(6,2)*R2JT(2,J)+KDD(6,3)*R2JT(3,J))
            ENDDO
          ENDDO
        ENDIF
        IF (NDJ==6) THEN
          DO I=1,3
            DO J=1,3
              K(I,J)=K(I,J)+ R1JT(1,I)*(KDD(1,4)*R2J(1,J)+
     1                      KDD(1,5)*R2J(2,J)+KDD(1,6)*R2J(3,J))+
     2                 R1JT(2,I)*(KDD(2,4)*R2J(1,J)+
     3                      KDD(2,5)*R2J(2,J)+KDD(2,6)*R2J(3,J))+
     4                 R1JT(3,I)*(KDD(3,4)*R2J(1,J)+
     5                      KDD(3,5)*R2J(2,J)+KDD(3,6)*R2J(3,J))
            ENDDO
          ENDDO
        ENDIF
        IF (NDI==6.AND.NDJ==6) THEN
          DO I=1,3
            DO J=1,3
              K(I,J)= K(I,J)+
     1               R1J(1,I)*(KDD(4,4)*R2J(1,J)+KDD(4,5)*R2J(2,J)+
     2                        KDD(4,6)*R2J(3,J) ) +
     3               R1J(2,I)*(KDD(5,4)*R2J(1,J)+KDD(5,5)*R2J(2,J)+
     4                        KDD(5,6)*R2J(3,J) ) +
     5               R1J(3,I)*(KDD(6,4)*R2J(1,J)+KDD(6,5)*R2J(2,J)+
     6                        KDD(6,6)*R2J(3,J) )
            ENDDO
          ENDDO
        ENDIF
C
        IF (ISYM==1) THEN
          DO I=1,3
            DO J=1,3
              KII(I,J)=K(I,J)+K(J,I)
            ENDDO
          ENDDO
        ELSE
          DO I=1,3
            DO J=1,3
              KII(I,J)=K(I,J)
            ENDDO
          ENDDO
        ENDIF
C
        RETURN
      END
C-------------produit {K'}=[K][CDI] [CDI]=-[RJT RJ]^t
Chd|====================================================================
Chd|  UPDK1_JJ                      source/interfaces/interf/i2_imp1.F
Chd|-- called by -----------
Chd|        I2UPDK1                       source/interfaces/interf/i2_imp1.F
Chd|        I2_FRK1                       source/interfaces/interf/i2_imp1.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE UPDK1_JJ(NDI,NDJ,RJ,RJT,KDD,KII)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NDI,NDJ
C     REAL
        my_real
     .      RJ(3,3), RJT(3,3), KDD(6,6), KII(6,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, J,MI
C     REAL
        my_real
     .     K(6,3)
C----------K RJT--------------------------
        DO I=1,3
          DO J=1,3
            K(I,J)=KDD(I,1)*RJT(1,J)+
     1             KDD(I,2)*RJT(2,J)+KDD(I,3)*RJT(3,J)
          ENDDO
        ENDDO
C
        IF (NDJ==6) THEN
          DO I=1,3
            DO J=1,3
              K(I,J)=K(I,J)+KDD(I,4)*RJ(1,J)+
     1               KDD(I,5)*RJ(2,J)+KDD(I,6)*RJ(3,J)
            ENDDO
          ENDDO
        ENDIF
        IF (NDI==6) THEN
          DO I=1,3
            MI=I+3
            DO J=1,3
              K(MI,J)= KDD(MI,1)*RJT(1,J)+
     1                 KDD(MI,2)*RJT(2,J)+KDD(MI,3)*RJT(3,J)
            ENDDO
          ENDDO
        ENDIF
        IF (NDI==6.AND.NDJ==6) THEN
          DO I=1,3
            MI=I+3
            DO J=1,3
              K(MI,J)= K(MI,J)+ KDD(MI,4)*RJ(1,J)+
     1                 KDD(MI,5)*RJ(2,J)+KDD(MI,6)*RJ(3,J)
            ENDDO
          ENDDO
        ENDIF
C
        DO I=1,NDI
          DO J=1,3
            KII(I,J)=K(I,J)
          ENDDO
        ENDDO
C
        RETURN
      END
C-------------produit {K'}=[CDI]^t[K][CDI]  with [CDI]=-H*[I]
Chd|====================================================================
Chd|  UPDKDD                        source/interfaces/interf/i2_imp1.F
Chd|-- called by -----------
Chd|        I2UPDK0                       source/interfaces/interf/i2_imp1.F
Chd|        I2_FRK0                       source/interfaces/interf/i2_imp1.F
Chd|        RM_IMP1                       source/model/remesh/rm_imp0.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE UPDKDD(NDL,KDD,KII,H2,ISYM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NDL,ISYM
C     REAL
        my_real
     .      KDD(6,6),KII(6,6),H2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, J
C     REAL
C------------------------------------
        DO I=1,6
          DO J=1,6
            KII(I,J) = ZERO
          ENDDO
        ENDDO
        DO I=1,NDL
          DO J=I,NDL
            KII(I,J)=H2*KDD(I,J)
          ENDDO
        ENDDO
        IF(ISYM/=1) THEN
          DO I=1,NDL
            DO J=I,NDL
              KII(J,I)=KII(I,J)
            ENDDO
          ENDDO
        ENDIF
C
        RETURN
      END
C-------------produit {K'}=[K][CDI]  with [CDI]=-H*[I]
Chd|====================================================================
Chd|  UPDKDD1                       source/interfaces/interf/i2_imp1.F
Chd|-- called by -----------
Chd|        I2UPDK0                       source/interfaces/interf/i2_imp1.F
Chd|        I2UPDKM0                      source/interfaces/interf/i2_imp1.F
Chd|        I2_FRK0                       source/interfaces/interf/i2_imp1.F
Chd|        RM_IMP1                       source/model/remesh/rm_imp0.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE UPDKDD1(NDI,NDJ,KDD,KII,H,ISYM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NDI,NDJ,ISYM
C     REAL
        my_real
     .      KDD(6,6),KII(6,6),H
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, J
C     REAL
C------------------------------------
        DO I=1,6
          DO J=1,6
            KII(I,J) = ZERO
          ENDDO
        ENDDO
        IF(ISYM==1) THEN
          DO I=1,NDI
            DO J=1,NDJ
              KII(I,J)=H*(KDD(I,J)+KDD(J,I))
            ENDDO
          ENDDO
        ELSE
          DO I=1,NDI
            DO J=1,NDJ
              KII(I,J)=H*KDD(I,J)
            ENDDO
          ENDDO
        ENDIF
C
        RETURN
      END
C-------------produit {K'}=[CDI]^t[KDD][CDJ]  with [CDI]=-HI*[KDD]
Chd|====================================================================
Chd|  UPDKDD2                       source/interfaces/interf/i2_imp1.F
Chd|-- called by -----------
Chd|        I2UPDK0                       source/interfaces/interf/i2_imp1.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE UPDKDD2(NDL,KDD,KII,H1,H2)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NDL,ISYM
C     REAL
        my_real
     .      KDD(6,6),KII(6,6),H1,H2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, J
C     REAL
C------------------------------------
        DO I=1,NDL
          DO J=1,NDL
            KII(I,J)=H1*KDD(I,J)+H2*KDD(J,I)
          ENDDO
        ENDDO
C
        RETURN
      END
Chd|====================================================================
Chd|  I2MATC                        source/interfaces/interf/i2_imp1.F
Chd|-- called by -----------
Chd|        I2RECU1                       source/interfaces/interf/i2_imp2.F
Chd|        I2RECU2                       source/interfaces/interf/i2_imp2.F
Chd|        I2UPDB1                       source/interfaces/interf/i2_imp1.F
Chd|        I2UPDB12                      source/interfaces/interf/i2_imp1.F
Chd|        I2UPDK1                       source/interfaces/interf/i2_imp1.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I2MATC(NSN,IRECT,DPARA,NSV,IRTL,X ,
     1                  NIRI,RJ ,RJT  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NSN, IRECT(4,*), NSV(*), IRTL(*),NIRI(*)
C     REAL
        my_real
     .     DPARA(7,*),X(3,*),RJ(3,3,4,NSN),RJT(3,3,4,NSN)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER  I, J, II, L, JJ,NJ,K,NIR
C     REAL
        my_real
     .     XX,YY,ZZ,XXX,YYY,ZZZ,XY,YZ,ZX,XY2,YZ2,ZX2,
     .     B1,B2,B3,C1,C2,C3,FACM,
     .     X22,Y22,Z22,DET,XM(4),YM(4),ZM(4),X0,Y0,Z0
        my_real
     .     XS,YS,ZS
C------------------------------------
C     MATRICE DE JACOBIEN [C]
C------------------------------------
        DO II=1,NSN
          I=NSV(II)
          L=IRTL(II)
          NIR=4
          DO J=1,NIR
            NJ=IRECT(J,L)
            XM(J)=X(1,NJ)
            YM(J)=X(2,NJ)
            ZM(J)=X(3,NJ)
          ENDDO
          IF(IRECT(3,L)==IRECT(4,L)) THEN
            NIR=3
            XM(4)=ZERO
            YM(4)=ZERO
            ZM(4)=ZERO
          ENDIF
          FACM = ONE / NIR
C----------------------------------------------------
C       VITESSE DE ROTATION MOYENNE DU SEGMENT MAIN
C----------------------------------------------------
          X0=FACM*(XM(1)+XM(2)+XM(3)+XM(4))
          Y0=FACM*(YM(1)+YM(2)+YM(3)+YM(4))
          Z0=FACM*(ZM(1)+ZM(2)+ZM(3)+ZM(4))
          DO J=1,NIR
            XM(J)=XM(J)-X0
            YM(J)=YM(J)-Y0
            ZM(J)=ZM(J)-Z0
          ENDDO
          XS=X(1,I)-X0
          YS=X(2,I)-Y0
          ZS=X(3,I)-Z0
C--------cette partie est une double travail que INTTI1
          XX=0
          YY=0
          ZZ=0
          XY=0
          YZ=0
          ZX=0
          DO J=1,NIR
            XX=XX+ XM(J)*XM(J)
            YY=YY+ YM(J)*YM(J)
            ZZ=ZZ+ ZM(J)*ZM(J)
            XY=XY+ XM(J)*YM(J)
            YZ=YZ+ YM(J)*ZM(J)
            ZX=ZX+ ZM(J)*XM(J)
          ENDDO
          ZZZ=XX+YY
          XXX=YY+ZZ
          YYY=ZZ+XX
          XY2=XY*XY
          YZ2=YZ*YZ
          ZX2=ZX*ZX
          DET= XXX*YYY*ZZZ -XXX*YZ2 -YYY*ZX2 -ZZZ*XY2 -TWO*XY*YZ*ZX
          DET=ONE/DET
          B1=(ZZZ*YYY-YZ2)*DET
          B2=(XXX*ZZZ-ZX2)*DET
          B3=(YYY*XXX-XY2)*DET
          C3=(ZZZ*XY+YZ*ZX)*DET
          C1=(XXX*YZ+ZX*XY)*DET
          C2=(YYY*ZX+XY*YZ)*DET
c        DET= DPARA(1,II)
c        B1=DPARA(2,II)
c        B2=DPARA(3,II)
c        B3=DPARA(4,II)
c        C1=DPARA(5,II)
c        C2=DPARA(6,II)
c        C3=DPARA(7,II)
          DO J=1,NIR
            X22 = C1*XM(J)
            Y22 = C2*YM(J)
            Z22 = C3*ZM(J)
C-------RJ=(R^tR)^-1Rj^t-(j=1,ndir)---
            RJ(1,1,J,II)=Z22-Y22
            RJ(2,1,J,II)=B2*ZM(J)-C1*YM(J)
            RJ(3,1,J,II)=C1*ZM(J)-B3*YM(J)
            RJ(1,2,J,II)=-B1*ZM(J)+C2*XM(J)
            RJ(2,2,J,II)=-Z22+X22
            RJ(3,2,J,II)=-C2*ZM(J)+B3*XM(J)
            RJ(1,3,J,II)=B1*YM(J)-C3*XM(J)
            RJ(2,3,J,II)=C3*YM(J)-B2*XM(J)
            RJ(3,3,J,II)=Y22-X22
C-------RJT=1/4[I]+(Rs)RJ---
            DO K=1,3
              RJT(1,K,J,II)=RJ(2,K,J,II)*ZS-RJ(3,K,J,II)*YS
              RJT(2,K,J,II)=-RJ(1,K,J,II)*ZS+RJ(3,K,J,II)*XS
              RJT(3,K,J,II)=RJ(1,K,J,II)*YS-RJ(2,K,J,II)*XS
            ENDDO
            DO K=1,3
              RJT(K,K,J,II)=RJT(K,K,J,II)+FACM
            ENDDO
          ENDDO
          NIRI(II)=NIR
        ENDDO
C
        RETURN
      END
Chd|====================================================================
Chd|  I2UPDKM1                      source/interfaces/interf/i2_imp1.F
Chd|-- called by -----------
Chd|        I2_IMPM                       source/interfaces/interf/i2_imp1.F
Chd|-- calls ---------------
Chd|        I2MATCM                       source/interfaces/interf/i2_imp1.F
Chd|        PUT_KII                       source/implicit/imp_glob_k.F
Chd|        PUT_KIJ                       source/implicit/imp_glob_k.F
Chd|        UPDK1_IJ                      source/interfaces/interf/i2_imp1.F
Chd|====================================================================
      SUBROUTINE I2UPDKM1(NS1,IRECT,DPARA,NSV,IRTL,
     .                    NS2,IRECT1,DPARA1,NSV1,IRTL1,
     .                    X  ,KDD   ,NDOF ,IDDL ,IADK    ,
     .                    JDIK,LT_K ,DIAG_K)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NS1,IRECT(4,*), NSV(*), IRTL(*),
     .          NS2,IRECT1(4,*), NSV1(*), IRTL1(*),
     .          NDOF(*) ,IDDL(*) ,IADK(*),JDIK(*)
C     REAL
        my_real
     .     DPARA(7,*),DPARA1(7,*),X(3,*),KDD(6,6),LT_K(*),DIAG_K(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER  J, L, J1,L1,NI,NJ,K,NIR,NIR1,NM,NM1,IR
C     REAL
        my_real
     .     KII(6,6),RJ(3,3,4),RJT(3,3,4),RJ1(3,3,4),RJT1(3,3,4)
C------------------------------------
        CALL I2MATCM(NS1,IRECT,DPARA,NSV,IRTL,
     .               X  ,NIR   ,RJ   ,RJT   )
        CALL I2MATCM(NS2,IRECT1,DPARA1,NSV1,IRTL1,
     .               X  ,NIR1  ,RJ1  ,RJT1  )
        NI=NSV(NS1)
        NJ=NSV1(NS2)
        L=IRTL(NS1)
        L1=IRTL1(NS2)
        DO J=1,NIR
          NM=IRECT(J,L)
          DO J1=1,NIR1
            NM1=IRECT1(J1,L1)
            IF (NM==NM1) THEN
              CALL UPDK1_IJ(NDOF(NI),NDOF(NJ),RJ(1,1,J),RJT(1,1,J),
     1                     RJ1(1,1,J1),RJT1(1,1,J1),KDD,KII,1)
              CALL PUT_KII(NM,IDDL,IADK,DIAG_K,LT_K,KII,3)
            ELSE
              CALL UPDK1_IJ(NDOF(NI),NDOF(NJ),RJ(1,1,J),RJT(1,1,J),
     1                     RJ1(1,1,J1),RJT1(1,1,J1),KDD,KII,0)
              CALL PUT_KIJ(NM1,NM,IDDL,IADK,JDIK,LT_K,KII,3,3,IR)
            ENDIF
          ENDDO
        ENDDO
C
        RETURN
      END
Chd|====================================================================
Chd|  I2MATCM                       source/interfaces/interf/i2_imp1.F
Chd|-- called by -----------
Chd|        I2UPDKM1                      source/interfaces/interf/i2_imp1.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I2MATCM(II,IRECT,DPARA,NSV,IRTL,X ,
     1                   NIRI,RJ ,RJT  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IRECT(4,*), NSV(*), IRTL(*),NIRI
C     REAL
        my_real
     .     DPARA(7,*),X(3,*),RJ(3,3,4),RJT(3,3,4)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER  I, J, II, L, JJ,NJ,K,NIR
C     REAL
        my_real
     .     XX,YY,ZZ,XXX,YYY,ZZZ,XY,YZ,ZX,XY2,YZ2,ZX2,
     .     B1,B2,B3,C1,C2,C3,FACM,
     .     X22,Y22,Z22,DET,XM(4),YM(4),ZM(4),X0,Y0,Z0
        my_real
     .     XS,YS,ZS
C------------------------------------
C     MATRICE DE JACOBIEN [C]
C------------------------------------
        I=NSV(II)
        L=IRTL(II)
        NIR=4
        DO J=1,NIR
          NJ=IRECT(J,L)
          XM(J)=X(1,NJ)
          YM(J)=X(2,NJ)
          ZM(J)=X(3,NJ)
        ENDDO
        IF(IRECT(3,L)==IRECT(4,L)) THEN
          NIR=3
          XM(4)=ZERO
          YM(4)=ZERO
          ZM(4)=ZERO
        ENDIF
        FACM = ONE / NIR
C----------------------------------------------------
C       VITESSE DE ROTATION MOYENNE DU SEGMENT MAIN
C----------------------------------------------------
        X0=FACM*(XM(1)+XM(2)+XM(3)+XM(4))
        Y0=FACM*(YM(1)+YM(2)+YM(3)+YM(4))
        Z0=FACM*(ZM(1)+ZM(2)+ZM(3)+ZM(4))
        DO J=1,NIR
          XM(J)=XM(J)-X0
          YM(J)=YM(J)-Y0
          ZM(J)=ZM(J)-Z0
        ENDDO
        XS=X(1,I)-X0
        YS=X(2,I)-Y0
        ZS=X(3,I)-Z0
C--------cette partie est une double travail que INTTI1
        XX=0
        YY=0
        ZZ=0
        XY=0
        YZ=0
        ZX=0
        DO J=1,NIR
          XX=XX+ XM(J)*XM(J)
          YY=YY+ YM(J)*YM(J)
          ZZ=ZZ+ ZM(J)*ZM(J)
          XY=XY+ XM(J)*YM(J)
          YZ=YZ+ YM(J)*ZM(J)
          ZX=ZX+ ZM(J)*XM(J)
        ENDDO
        ZZZ=XX+YY
        XXX=YY+ZZ
        YYY=ZZ+XX
        XY2=XY*XY
        YZ2=YZ*YZ
        ZX2=ZX*ZX
        DET= XXX*YYY*ZZZ -XXX*YZ2 -YYY*ZX2 -ZZZ*XY2 -TWO*XY*YZ*ZX
        DET=ONE/DET
        B1=(ZZZ*YYY-YZ2)*DET
        B2=(XXX*ZZZ-ZX2)*DET
        B3=(YYY*XXX-XY2)*DET
        C3=(ZZZ*XY+YZ*ZX)*DET
        C1=(XXX*YZ+ZX*XY)*DET
        C2=(YYY*ZX+XY*YZ)*DET
        DO J=1,NIR
          X22 = C1*XM(J)
          Y22 = C2*YM(J)
          Z22 = C3*ZM(J)
C-------RJ=(R^tR)^-1Rj^t-(j=1,ndir)---
          RJ(1,1,J)=Z22-Y22
          RJ(2,1,J)=B2*ZM(J)-C1*YM(J)
          RJ(3,1,J)=C1*ZM(J)-B3*YM(J)
          RJ(1,2,J)=-B1*ZM(J)+C2*XM(J)
          RJ(2,2,J)=-Z22+X22
          RJ(3,2,J)=-C2*ZM(J)+B3*XM(J)
          RJ(1,3,J)=B1*YM(J)-C3*XM(J)
          RJ(2,3,J)=C3*YM(J)-B2*XM(J)
          RJ(3,3,J)=Y22-X22
C-------RJT=1/4[I]+(Rs)RJ---
          DO K=1,3
            RJT(1,K,J)=RJ(2,K,J)*ZS-RJ(3,K,J)*YS
            RJT(2,K,J)=-RJ(1,K,J)*ZS+RJ(3,K,J)*XS
            RJT(3,K,J)=RJ(1,K,J)*YS-RJ(2,K,J)*XS
          ENDDO
          DO K=1,3
            RJT(K,K,J)=RJT(K,K,J)+FACM
          ENDDO
        ENDDO
        NIRI=NIR
C
        RETURN
      END
Chd|====================================================================
Chd|  I2_FRFM1                      source/interfaces/interf/i2_imp1.F
Chd|-- called by -----------
Chd|        IMP3_A2B                      source/airbag/monv_imp0.F
Chd|        UPD_FR                        source/mpi/implicit/imp_fri.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I2_FRFM1(X   ,IRECT,DPARA ,NSV ,IRTL ,
     1                    A   ,II   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER
     .     IRECT(4,*), NSV(*), IRTL(*),II
C     REAL
        my_real
     .     A(3,*),X(3,*),DPARA(7,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, J, J1,J2,J3,J4,  L, JJ,NIR
C     REAL
        my_real
     .     FXS, FYS, FZS,MX,MY,MZ,DET,FX0,FY0,FZ0,
     .     X0,X1,X2,X3,X4,XS,Y0,Y1,Y2,Y3,Y4,YS,Z0,Z1,Z2,Z3,Z4,ZS,
     .     A1,A2,A3,B1,B2,B3,C1,C2,C3,FACM,XM(4),YM(4),ZM(4)
C-----------------------------------------------
        I=NSV(II)
        L=IRTL(II)
        NIR=4
        DO JJ=1,NIR
          J=IRECT(JJ,L)
          XM(JJ)=X(1,J)
          YM(JJ)=X(2,J)
          ZM(JJ)=X(3,J)
        ENDDO
        IF(IRECT(3,L)==IRECT(4,L)) THEN
          NIR=3
          XM(4)=ZERO
          YM(4)=ZERO
          ZM(4)=ZERO
        ENDIF
        FACM = ONE / NIR
        X0=FACM*(XM(1)+XM(2)+XM(3)+XM(4))
        Y0=FACM*(YM(1)+YM(2)+YM(3)+YM(4))
        Z0=FACM*(ZM(1)+ZM(2)+ZM(3)+ZM(4))
        DO J=1,NIR
          XM(J)=XM(J)-X0
          YM(J)=YM(J)-Y0
          ZM(J)=ZM(J)-Z0
        ENDDO
        XS=X(1,I)-X0
        YS=X(2,I)-Y0
        ZS=X(3,I)-Z0
C
        DET=DPARA(1,II)
        B1=DPARA(2,II)
        B2=DPARA(3,II)
        B3=DPARA(4,II)
        C1=DPARA(5,II)
        C2=DPARA(6,II)
        C3=DPARA(7,II)
C
        FXS=A(1,I)
        FYS=A(2,I)
        FZS=A(3,I)
        MX=  YS*FZS - ZS*FYS
        MY=  ZS*FXS - XS*FZS
        MZ=  XS*FYS - YS*FXS
C
        A1=DET*(MX*B1+MY*C3+MZ*C2)
        A2=DET*(MY*B2+MZ*C1+MX*C3)
        A3=DET*(MZ*B3+MX*C2+MY*C1)
C
        FX0=FXS*FACM
        FY0=FYS*FACM
        FZ0=FZS*FACM
C------------------------------------------------------
C     FORCES TRANSMISES AUX NOEUDS MAINS
C------------------------------------------------------
        DO JJ=1,NIR
          J=IRECT(JJ,L)
          A(1,J)=A(1,J) + FX0 + A2*ZM(JJ) - A3*YM(JJ)
          A(2,J)=A(2,J) + FY0 + A3*XM(JJ) - A1*ZM(JJ)
          A(3,J)=A(3,J) + FZ0 + A1*YM(JJ) - A2*XM(JJ)
        ENDDO
C
        RETURN
      END
Chd|====================================================================
Chd|  I2_FRFM0                      source/interfaces/interf/i2_imp1.F
Chd|-- called by -----------
Chd|        IMP3_A2B                      source/airbag/monv_imp0.F
Chd|        UPD_FR                        source/mpi/implicit/imp_fri.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I2_FRFM0(X   ,IRECT,CRST  ,NSV  ,IRTL,
     1                    A   ,AR   ,II    ,NDOF )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER
     .     IRECT(4,*), NSV(*), IRTL(*), II,NDOF(*)
C     REAL
        my_real
     .     X(3,*),A(3,*), AR(3,*), CRST(2,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER NIR, I, J, I1, J1,  L, NJ,NDM
C     REAL
        my_real
     .     H(4), SS, TT, FXI, FYI, FZI,SP,SM,TP,TM,NUN,
     .     MXI, MYI, MZI,XS,YS,ZS,XS0,YS0,ZS0
C-----------------------------------------------
        NUN=-ONE
C
C
        I=NSV(II)
        L=IRTL(II)
C
        SS=CRST(1,II)
        TT=CRST(2,II)
        SS = MIN(ONE,SS)
        TT = MIN(ONE,TT)
        SS = MAX(NUN,SS)
        TT = MAX(NUN,TT)
C
        FXI=A(1,I)
        FYI=A(2,I)
        FZI=A(3,I)
C
        SP=ONE + SS
        SM=ONE - SS
        TP=FOURTH*(ONE + TT)
        TM=FOURTH*(ONE - TT)
        IF (IRECT(3,L)==IRECT(4,L)) THEN
          NIR=3
          TP=FOURTH*(ONE + TT)
          TM=FOURTH*(ONE - TT)
          H(1)=TM*SM
          H(2)=TM*SP
          H(3)=ONE-H(1)-H(2)
        ELSE
          NIR=4
          TP=FOURTH*(ONE + TT)
          TM=FOURTH*(ONE - TT)
          H(1)=TM*SM
          H(2)=TM*SP
          H(3)=TP*SP
          H(4)=TP*SM
        ENDIF
        NDM = 0
        DO J=1,NIR
          NJ=IRECT(J,L)
          A(1,NJ)=A(1,NJ)+FXI*H(J)
          A(2,NJ)=A(2,NJ)+FYI*H(J)
          A(3,NJ)=A(3,NJ)+FZI*H(J)
          NDM = MAX(NDM,NDOF(J))
        ENDDO
        IF(NDM==6)THEN
          XS0=ZERO
          YS0=ZERO
          ZS0=ZERO
          DO J=1,NIR
            NJ=IRECT(J,L)
            XS0=XS0+X(1,NJ)*H(J)
            YS0=YS0+X(2,NJ)*H(J)
            ZS0=ZS0+X(3,NJ)*H(J)
          ENDDO
          XS=X(1,I)-XS0
          YS=X(2,I)-YS0
          ZS=X(3,I)-ZS0
          MXI = YS * FZI - ZS * FYI
          MYI = ZS * FXI - XS * FZI
          MZI = XS * FYI - YS * FXI
          DO J=1,NIR
            NJ=IRECT(J,L)
            AR(1,NJ)=AR(1,NJ)-MXI*H(J)
            AR(2,NJ)=AR(2,NJ)-MYI*H(J)
            AR(3,NJ)=AR(3,NJ)-MZI*H(J)
          ENDDO
        ENDIF
C
        RETURN
      END
Chd|====================================================================
Chd|  I2_FRUP0                      source/interfaces/interf/i2_imp1.F
Chd|-- called by -----------
Chd|        DIAG_INT                      source/mpi/implicit/imp_fri.F
Chd|        UPDK_MV                       source/airbag/monv_imp0.F
Chd|-- calls ---------------
Chd|        UPDFR_RB                      source/constraints/general/rbody/rby_imp0.F
Chd|====================================================================
      SUBROUTINE I2_FRUP0(X   ,IRECT,CRST  ,NSV  ,IRTL ,
     1                    II  ,NDOF ,KSS   ,K    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER
     .     IRECT(4,*), NSV(*), IRTL(*), II,NDOF(*)
C     REAL
        my_real
     .     X(3,*),KSS(6),K(6,4),  CRST(2,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER NIR, I, J,  JD,  L, JJ,NJ,ND
C     REAL
        my_real
     .     H(4), SS, TT, SP,SM,TP,TM,K0(6),XS,YS,ZS,
     .     XS0,YS0,ZS0,H2
C-----------------------------------------------
        NIR=4
        I=NSV(II)
        L=IRTL(II)
C
        SS=CRST(1,II)
        TT=CRST(2,II)
        SP=ONE + SS
        SM=ONE - SS
        IF(IRECT(3,L)==IRECT(4,L)) THEN
          NIR = 3
          TP=FOURTH*(ONE + TT)
          TM=FOURTH*(ONE - TT)
          H(1)=TM*SM
          H(2)=TM*SP
          H(3)=ONE-H(1)-H(2)
        ELSE
          TP=FOURTH*(ONE + TT)
          TM=FOURTH*(ONE - TT)
          H(1)=TM*SM
          H(2)=TM*SP
          H(3)=TP*SP
          H(4)=TP*SM
        ENDIF
        ND = 0
        DO J=1,NIR
          NJ=IRECT(J,L)
          ND = MAX(ND,NDOF(NJ))
        ENDDO
C-------NDOF(M)> 3 comme rigid body---
        IF (ND==6) THEN
          XS0=ZERO
          YS0=ZERO
          ZS0=ZERO
          DO J=1,NIR
            NJ=IRECT(J,L)
            XS0=XS0+X(1,NJ)*H(J)
            YS0=YS0+X(2,NJ)*H(J)
            ZS0=ZS0+X(3,NJ)*H(J)
          ENDDO
          XS=X(1,I)-XS0
          YS=X(2,I)-YS0
          ZS=X(3,I)-ZS0
          CALL UPDFR_RB(XS,YS,ZS,KSS,K0)
        ELSE
          DO JJ =1,3
            K0(JJ)=KSS(JJ)
          ENDDO
        ENDIF
C-------Update K(main node),B---
        DO J=1,NIR
          NJ=IRECT(J,L)
          H2=H(J)*H(J)
          DO JJ =1,NDOF(NJ)
            K(JJ,J)=K(JJ,J)+H2*K0(JJ)
          ENDDO
        ENDDO
C
        RETURN
      END
Chd|====================================================================
Chd|  I2_FRUP1                      source/interfaces/interf/i2_imp1.F
Chd|-- called by -----------
Chd|        DIAG_INT                      source/mpi/implicit/imp_fri.F
Chd|        UPDK_MV                       source/airbag/monv_imp0.F
Chd|-- calls ---------------
Chd|        UPFR1_II                      source/interfaces/interf/i2_imp1.F
Chd|====================================================================
      SUBROUTINE I2_FRUP1(X   ,IRECT,DPARA ,NSV ,IRTL ,
     1                    II  ,KII  ,KJJ   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER
     .     IRECT(4,*), NSV(*), IRTL(*),II
C     REAL
        my_real
     .    X(3,*),KII(6),KJJ(6,4),DPARA(7,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, J,  L, JJ,NJ,K,NIR
C     REAL
        my_real
     .     RJ(3,3,4),RJT(3,3,4),
     .     B1,B2,B3,C1,C2,C3,FACM,
     .     X22,Y22,Z22,DET,XM(4),YM(4),ZM(4),X0,Y0,Z0
        my_real
     .     XS,YS,ZS
C-----------------------------------------------
C
        I=NSV(II)
        L=IRTL(II)
        NIR=4
        DO J=1,NIR
          NJ=IRECT(J,L)
          XM(J)=X(1,NJ)
          YM(J)=X(2,NJ)
          ZM(J)=X(3,NJ)
        ENDDO
        IF(IRECT(3,L)==IRECT(4,L)) THEN
          NIR=3
          XM(4)=ZERO
          YM(4)=ZERO
          ZM(4)=ZERO
        ENDIF
        FACM = ONE / NIR
C----------------------------------------------------
C       VITESSE DE ROTATION MOYENNE DU SEGMENT MAIN
C----------------------------------------------------
        X0=FACM*(XM(1)+XM(2)+XM(3)+XM(4))
        Y0=FACM*(YM(1)+YM(2)+YM(3)+YM(4))
        Z0=FACM*(ZM(1)+ZM(2)+ZM(3)+ZM(4))
        DO J=1,NIR
          XM(J)=XM(J)-X0
          YM(J)=YM(J)-Y0
          ZM(J)=ZM(J)-Z0
        ENDDO
        XS=X(1,I)-X0
        YS=X(2,I)-Y0
        ZS=X(3,I)-Z0
        DET= DPARA(1,II)
        B1=DPARA(2,II)
        B2=DPARA(3,II)
        B3=DPARA(4,II)
        C1=DPARA(5,II)
        C2=DPARA(6,II)
        C3=DPARA(7,II)
        DO J=1,NIR
          X22 = C1*XM(J)
          Y22 = C2*YM(J)
          Z22 = C3*ZM(J)
C-------RJ=(R^tR)^-1Rj^t-(j=1,ndir)---
          RJ(1,1,J)=Z22-Y22
          RJ(2,1,J)=B2*ZM(J)-C1*YM(J)
          RJ(3,1,J)=C1*ZM(J)-B3*YM(J)
          RJ(1,2,J)=-B1*ZM(J)+C2*XM(J)
          RJ(2,2,J)=-Z22+X22
          RJ(3,2,J)=-C2*ZM(J)+B3*XM(J)
          RJ(1,3,J)=B1*YM(J)-C3*XM(J)
          RJ(2,3,J)=C3*YM(J)-B2*XM(J)
          RJ(3,3,J)=Y22-X22
C-------RJT=1/4[I]+(Rs)RJ---
          DO K=1,3
            RJT(1,K,J)=RJ(2,K,J)*ZS-RJ(3,K,J)*YS
            RJT(2,K,J)=-RJ(1,K,J)*ZS+RJ(3,K,J)*XS
            RJT(3,K,J)=RJ(1,K,J)*YS-RJ(2,K,J)*XS
          ENDDO
          DO K=1,3
            RJT(K,K,J)=RJT(K,K,J)+FACM
          ENDDO
        ENDDO
C
        DO J=1,NIR
          NJ=IRECT(J,L)
          CALL UPFR1_II(RJ(1,1,J),RJT(1,1,J),KII,KJJ(1,J))
        ENDDO
C
        RETURN
      END
Chd|====================================================================
Chd|  I2_IMPR1                      source/interfaces/interf/i2_imp1.F
Chd|-- called by -----------
Chd|        IMP_DYKV                      source/implicit/imp_dyna.F
Chd|        IMP_DYKV0                     source/implicit/imp_dyna.F
Chd|        UPD_RHS                       source/implicit/upd_glob_k.F
Chd|        UPD_RHS_FR                    source/implicit/imp_solv.F
Chd|-- calls ---------------
Chd|        I2UPDB0                       source/interfaces/interf/i2_imp1.F
Chd|        I2UPDB1                       source/interfaces/interf/i2_imp1.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I2_IMPR1(IPARI,INTBUF_TAB,
     .                    X    ,NDOF ,IDDL    ,B     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IPARI(*)
        INTEGER NDOF(*),IDDL(*)
C     REAL
        my_real
     .     X(*),B(*)
        TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER
     .     K10, K11, K12, K13, K14, KFI, J10, J11, J12, J21, J22,
     .     JFI,NSN,NMN,NRTS,NRTM,ILEV
C-----------------------------------------------
        NRTS  =IPARI(3)
        NRTM  =IPARI(4)
        NSN   =IPARI(5)
        NMN   =IPARI(6)
        ILEV  =IPARI(20)
C
        K10=1
        K11=K10+4*NRTS
        K12=K11+4*NRTM
        K13=K12+NSN
        K14=K13+NMN
        KFI=K14+NSN
        J10=1
        J11=J10+1
        J12=J11+NPARIR
        J21=J12+2*NSN
        J22=J21+7*NSN
        JFI=J22+NMN
C
        IF(ILEV==1)THEN
          CALL I2UPDB1(NSN       ,INTBUF_TAB%IRECTM,INTBUF_TAB%DPARA,
     1      INTBUF_TAB%NSV,INTBUF_TAB%IRTLM,X         ,NDOF      ,IDDL      ,
     2      B        )
        ELSE
          CALL I2UPDB0(NSN       ,INTBUF_TAB%IRECTM,INTBUF_TAB%CSTS,
     1      INTBUF_TAB%NSV,INTBUF_TAB%IRTLM,X         ,NDOF      ,IDDL      ,
     2      B        )
        ENDIF
C
        RETURN
      END
Chd|====================================================================
Chd|  I2UPDB0                       source/interfaces/interf/i2_imp1.F
Chd|-- called by -----------
Chd|        I2_IMPR1                      source/interfaces/interf/i2_imp1.F
Chd|-- calls ---------------
Chd|        UPDB_RB                       source/constraints/general/rbody/rby_imp0.F
Chd|====================================================================
      SUBROUTINE I2UPDB0(NSN,IRECT,CRST,NSV,IRTL,
     1                   X  ,NDOF ,IDDL,B  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NSN, IRECT(4,*), NSV(*), IRTL(*)
        INTEGER NDOF(*),IDDL(*)
C     REAL
        my_real
     .     CRST(2,*),X(3,*),B(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER NIR, I, J, J1, J2, J3, J4, K, JD, II, L, JJ,
     .          I1,ID,NL,NI,NJ,NIDOF,ND,NDI,NDJ,NDM,NM
C     REAL
        my_real
     .     H(4),SS, TT, SP,SM,TP,TM,BD(6),
     .     BI(6),XS0,YS0,ZS0,XS,YS,ZS,NUN
C------------------------------------
C     VITESSES DES NOEUDS SECONDS
C------------------------------------
C-------pour simplement etre coherent avec INTTI2, on ne distinque pas 3n,4n
C-------on est oblige de distinque quand meme, sinon la matrice n'est plus positive
        NUN=-ONE
        J1=0
        DO II=1,NSN
          I=NSV(II)
          L=IRTL(II)
          IF (NDOF(I)>0) THEN
            DO K=1,NDOF(I)
              ID = IDDL(I)+K
              BD(K)=B(ID)
            ENDDO
            DO K=NDOF(I)+1,6
              BD(K)=ZERO
            ENDDO
C
            SS=CRST(1,II)
            TT=CRST(2,II)
            SS = MIN(ONE,SS)
            TT = MIN(ONE,TT)
            SS = MAX(NUN,SS)
            TT = MAX(NUN,TT)
            SP=ONE + SS
            SM=ONE - SS
            IF (IRECT(3,L)==IRECT(4,L)) THEN
              NIR=3
              TP=FOURTH*(ONE + TT)
              TM=FOURTH*(ONE - TT)
              H(1)=TM*SM
              H(2)=TM*SP
              H(3)=ONE-H(1)-H(2)
            ELSE
              NIR=4
              TP=FOURTH*(ONE + TT)
              TM=FOURTH*(ONE - TT)
              H(1)=TM*SM
              H(2)=TM*SP
              H(3)=TP*SP
              H(4)=TP*SM
            ENDIF
            NDM = 0
            DO J=1,NIR
              NJ=IRECT(J,L)
              NDM = MAX(NDM,NDOF(NJ))
            ENDDO
C-------NDOF(M)> 3 comme rigid body---
            IF (NDM==6) THEN
              XS0=ZERO
              YS0=ZERO
              ZS0=ZERO
              DO J=1,NIR
                NJ=IRECT(J,L)
                XS0=XS0+X(1,NJ)*H(J)
                YS0=YS0+X(2,NJ)*H(J)
                ZS0=ZS0+X(3,NJ)*H(J)
              ENDDO
              XS=X(1,I)-XS0
              YS=X(2,I)-YS0
              ZS=X(3,I)-ZS0
              CALL UPDB_RB(NDOF(I),XS,YS,ZS,BD)
            ENDIF
CC-------Update B---
            DO J=1,NIR
              NJ=IRECT(J,L)
              ND = MIN(NDM,NDOF(NJ))
              DO K=1,ND
                ID = IDDL(NJ)+K
                B(ID) = B(ID) + H(J)*BD(K)
              ENDDO
            ENDDO
          ENDIF
        ENDDO
C
        RETURN
      END
Chd|====================================================================
Chd|  I2UPDB1                       source/interfaces/interf/i2_imp1.F
Chd|-- called by -----------
Chd|        I2_IMPR1                      source/interfaces/interf/i2_imp1.F
Chd|-- calls ---------------
Chd|        I2MATC                        source/interfaces/interf/i2_imp1.F
Chd|        UPDB1_II                      source/interfaces/interf/i2_imp1.F
Chd|====================================================================
      SUBROUTINE I2UPDB1(NSN,IRECT,DPARA,NSV,IRTL,
     1                   X ,NDOF ,IDDL ,B  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NSN, IRECT(4,*), NSV(*), IRTL(*)
        INTEGER NDOF(*),IDDL(*)
C     REAL
        my_real
     .     DPARA(7,*),X(*),B(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER NIR(NSN),I, J, K, JD, II, L, JJ,I1,ID,NL,
     .          NI,NJ,J1,NIDOF,ND,NM,IDOF,L1,NM1
C     REAL
        my_real
     .     RJ(9,4,NSN),RJT(9,4,NSN)
        my_real
     .     BD(6),BI(6),XS,YS,ZS
C------------------------------------
C     VITESSES DES NOEUDS SECONDS
C------------------------------------
        CALL I2MATC(NSN,IRECT,DPARA,NSV,IRTL,X,NIR,RJ ,RJT  )
        ND = 3
        DO II=1,NSN
          I=NSV(II)
          IDOF=NDOF(I)
          IF (IDOF>0) THEN
            L=IRTL(II)
            DO K=1,IDOF
              ID = IDDL(I)+K
              BD(K)=B(ID)
            ENDDO
            DO K=IDOF+1,6
              BD(K)=ZERO
            ENDDO
C-------Update B---
            DO J=1,NIR(II)
              NJ=IRECT(J,L)
              CALL UPDB1_II(IDOF,RJ(1,J,II),RJT(1,J,II),BD,BI)
              DO K=1,ND
                ID = IDDL(NJ)+K
                B(ID) = B(ID) + BI(K)
              ENDDO
            ENDDO
          ENDIF
        ENDDO
C
        RETURN
      END
C-------------produit B'=[CDI]^tB with [CDI]=-[RJT RJ]^t
Chd|====================================================================
Chd|  UPDB1_II                      source/interfaces/interf/i2_imp1.F
Chd|-- called by -----------
Chd|        I2UPDB1                       source/interfaces/interf/i2_imp1.F
Chd|        I2UPDB12                      source/interfaces/interf/i2_imp1.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE UPDB1_II(NDL,RJ,RJT,BD,B)
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
     .      B(3),RJ(3,3), RJT(3,3), BD(6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, J
C     REAL
C------------------------------------
        DO I=1,3
          B(I)=RJT(1,I)*BD(1)+RJT(2,I)*BD(2)+RJT(3,I)*BD(3)
        ENDDO
C
        IF (NDL==6) THEN
          DO I=1,3
            B(I)=B(I)+RJ(1,I)*BD(4)+RJ(2,I)*BD(5)+RJ(3,I)*BD(6)
          ENDDO
        ENDIF
C
        RETURN
      END
Chd|====================================================================
Chd|  I2_IMPR2                      source/interfaces/interf/i2_imp1.F
Chd|-- called by -----------
Chd|        IMP_DYKV                      source/implicit/imp_dyna.F
Chd|        IMP_DYKV0                     source/implicit/imp_dyna.F
Chd|        UPD_RHS                       source/implicit/upd_glob_k.F
Chd|        UPD_RHS_FR                    source/implicit/imp_solv.F
Chd|-- calls ---------------
Chd|        I2UPDB02                      source/interfaces/interf/i2_imp1.F
Chd|        I2UPDB12                      source/interfaces/interf/i2_imp1.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I2_IMPR2(IPARI,INTBUF_TAB,A    ,AR      ,
     .                    X    ,NDOF ,IDDL    ,B     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IPARI(*)
        INTEGER NDOF(*),IDDL(*)
C     REAL
        my_real
     .     X(*),B(*),A(*),AR(*)
        TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER
     .     K10, K11, K12, K13, K14, KFI, J10, J11, J12, J21, J22,
     .     JFI,NSN,NMN,NRTS,NRTM,ILEV
C-----------------------------------------------
        NRTS  =IPARI(3)
        NRTM  =IPARI(4)
        NSN   =IPARI(5)
        NMN   =IPARI(6)
        ILEV  =IPARI(20)
C
        K10=1
        K11=K10+4*NRTS
        K12=K11+4*NRTM
        K13=K12+NSN
        K14=K13+NMN
        KFI=K14+NSN
        J10=1
        J11=J10+1
        J12=J11+NPARIR
        J21=J12+2*NSN
        J22=J21+7*NSN
        JFI=J22+NMN
C
        IF(ILEV==1)THEN
          CALL I2UPDB12(NSN       ,INTBUF_TAB%IRECTM,INTBUF_TAB%DPARA,
     1      INTBUF_TAB%NSV,INTBUF_TAB%IRTLM,X         ,NDOF      ,IDDL      ,
     2      B         ,A         ,AR        )
        ELSE
          CALL I2UPDB02(NSN       ,INTBUF_TAB%IRECTM,INTBUF_TAB%CSTS,
     1      INTBUF_TAB%NSV,INTBUF_TAB%IRTLM,X         ,NDOF      ,IDDL      ,
     2      B        ,A         ,AR        )
        ENDIF
C
        RETURN
      END
Chd|====================================================================
Chd|  I2UPDB02                      source/interfaces/interf/i2_imp1.F
Chd|-- called by -----------
Chd|        I2_IMPR2                      source/interfaces/interf/i2_imp1.F
Chd|-- calls ---------------
Chd|        UPDB_RB                       source/constraints/general/rbody/rby_imp0.F
Chd|====================================================================
      SUBROUTINE I2UPDB02(NSN,IRECT,CRST,NSV,IRTL,
     1                   X  ,NDOF ,IDDL,B   ,A   ,AR   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NSN, IRECT(4,*), NSV(*), IRTL(*)
        INTEGER NDOF(*),IDDL(*)
C     REAL
        my_real
     .     CRST(2,*),X(3,*),B(*),A(3,*),AR(3,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER NIR, I, J, J1, J2, J3, J4, K, JD, II, L, JJ,
     .          I1,ID,NL,NI,NJ,NIDOF,ND,NDI,NDJ,NDM,NM
C     REAL
        my_real
     .     H(4),SS, TT, SP,SM,TP,TM,BD(6),
     .     BI(6),XS0,YS0,ZS0,XS,YS,ZS,NUN
C------------------------------------
C     VITESSES DES NOEUDS SECONDS
C------------------------------------
C-------pour simplement etre coherent avec INTTI2, on ne distinque pas 3n,4n
C-------on est oblige de distinque quand meme, sinon la matrice n'est plus positive
        NUN=-ONE
        J1=0
        IF (IRODDL/=0) THEN
          ND = 6
        ELSE
          ND = 3
        ENDIF
        DO II=1,NSN
          I=NSV(II)
          L=IRTL(II)
          IF (NDOF(I)==0) THEN
            DO K=1,3
              BD(K)=A(K,I)
            ENDDO
            IF (ND==3) THEN
              DO K=ND+1,6
                BD(K)=ZERO
              ENDDO
            ELSE
              DO K=1,3
                BD(K+3)=AR(K,I)
              ENDDO
            ENDIF
C
            SS=CRST(1,II)
            TT=CRST(2,II)
            SS = MIN(ONE,SS)
            TT = MIN(ONE,TT)
            SS = MAX(NUN,SS)
            TT = MAX(NUN,TT)
            SP=ONE + SS
            SM=ONE - SS
            IF (IRECT(3,L)==IRECT(4,L)) THEN
              NIR=3
              TP=FOURTH*(ONE + TT)
              TM=FOURTH*(ONE - TT)
              H(1)=TM*SM
              H(2)=TM*SP
              H(3)=ONE-H(1)-H(2)
            ELSE
              NIR=4
              TP=FOURTH*(ONE + TT)
              TM=FOURTH*(ONE - TT)
              H(1)=TM*SM
              H(2)=TM*SP
              H(3)=TP*SP
              H(4)=TP*SM
            ENDIF
C-------comme rigid body---
            IF (ND==6) THEN
              XS0=ZERO
              YS0=ZERO
              ZS0=ZERO
              DO J=1,NIR
                NJ=IRECT(J,L)
                XS0=XS0+X(1,NJ)*H(J)
                YS0=YS0+X(2,NJ)*H(J)
                ZS0=ZS0+X(3,NJ)*H(J)
              ENDDO
              XS=X(1,I)-XS0
              YS=X(2,I)-YS0
              ZS=X(3,I)-ZS0
              CALL UPDB_RB(ND,XS,YS,ZS,BD)
            ENDIF
CC-------Update B---
            DO J=1,NIR
              NJ=IRECT(J,L)
              IF (NDOF(NJ)==0) THEN
                DO K=1,3
                  A(K,NJ)=A(K,NJ)+BD(K)
                ENDDO
                IF (ND==6) THEN
                  DO K=1,3
                    AR(K,NJ)=AR(K,NJ)+BD(K+3)
                  ENDDO
                ENDIF
              ELSE
                DO K=1,ND
                  ID = IDDL(NJ)+K
                  B(ID) = B(ID) + H(J)*BD(K)
                ENDDO
              ENDIF
            ENDDO
          ENDIF
        ENDDO
C
        RETURN
      END
Chd|====================================================================
Chd|  I2UPDB12                      source/interfaces/interf/i2_imp1.F
Chd|-- called by -----------
Chd|        I2_IMPR2                      source/interfaces/interf/i2_imp1.F
Chd|-- calls ---------------
Chd|        I2MATC                        source/interfaces/interf/i2_imp1.F
Chd|        UPDB1_II                      source/interfaces/interf/i2_imp1.F
Chd|====================================================================
      SUBROUTINE I2UPDB12(NSN,IRECT,DPARA,NSV,IRTL,
     1                   X ,NDOF ,IDDL ,B   ,A    ,AR )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NSN, IRECT(4,*), NSV(*), IRTL(*)
        INTEGER NDOF(*),IDDL(*)
C     REAL
        my_real
     .     DPARA(7,*),X(*),B(*),A(3,*),AR(3,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER NIR(NSN),I, J, K, JD, II, L, JJ,I1,ID,NL,
     .          NI,NJ,J1,NIDOF,ND,NM,IDOF,L1,NM1
C     REAL
        my_real
     .     RJ(9,4,NSN),RJT(9,4,NSN)
        my_real
     .     BD(6),BI(6),XS,YS,ZS
C------------------------------------
C     VITESSES DES NOEUDS SECONDS
C------------------------------------
        CALL I2MATC(NSN,IRECT,DPARA,NSV,IRTL,X,NIR,RJ ,RJT  )
        IF (IRODDL/=0) THEN
          ND = 6
        ELSE
          ND = 3
        ENDIF
        DO II=1,NSN
          I=NSV(II)
          IDOF=NDOF(I)
          IF (IDOF==0) THEN
            L=IRTL(II)
            DO K=1,3
              BD(K)=A(K,I)
            ENDDO
            IF (ND==3) THEN
              DO K=ND+1,6
                BD(K)=ZERO
              ENDDO
            ELSE
              DO K=1,3
                BD(K+3)=AR(K,I)
              ENDDO
            ENDIF
C-------Update B---
            DO J=1,NIR(II)
              NJ=IRECT(J,L)
              CALL UPDB1_II(ND,RJ(1,J,II),RJT(1,J,II),BD,BI)
              IF (NDOF(NJ)==0) THEN
                DO K=1,3
                  A(K,NJ)=A(K,NJ)+BI(K)
                ENDDO
              ELSE
                DO K=1,3
                  ID = IDDL(NJ)+K
                  B(ID) = B(ID) + BI(K)
                ENDDO
              ENDIF
            ENDDO
          ENDIF
        ENDDO
C
        RETURN
      END
Chd|====================================================================
Chd|  I2_FRK0                       source/interfaces/interf/i2_imp1.F
Chd|-- called by -----------
Chd|        UPD_KML                       source/mpi/implicit/imp_fri.F
Chd|        UPD_KSL                       source/mpi/implicit/imp_fri.F
Chd|-- calls ---------------
Chd|        PRINT_WKIJ                    source/implicit/imp_glob_k.F
Chd|        PUT_KMII                      source/implicit/imp_glob_k.F
Chd|        PUT_KMIJ                      source/implicit/imp_glob_k.F
Chd|        UPDKB_RB                      source/constraints/general/rbody/rby_imp0.F
Chd|        UPDKB_RB1                     source/constraints/general/rbody/rby_imp0.F
Chd|        UPDKDD                        source/interfaces/interf/i2_imp1.F
Chd|        UPDKDD1                       source/interfaces/interf/i2_imp1.F
Chd|====================================================================
      SUBROUTINE I2_FRK0(IRECT,CRST  ,X    ,ITAB ,NSV  ,
     1                   IRTL ,IKC  ,NDOF  ,IDDL ,IDDLM,
     2                   IADK ,JDIK ,DIAG_K,LT_K ,B    ,
     3                   A    ,KSS  ,KSM  ,KNM   ,KRM  ,
     4                   II   ,IDLM ,ISS  ,ISM  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IADK(*),JDIK(*),NDOF(*),IDDL(*),IDDLM(*),IKC(*),
     .          IRECT(4,*), NSV(*),IRTL(*),ITAB(*),II,
     .          IDLM(*)  ,ISS  ,ISM
C     REAL
        my_real
     .     CRST(2,*),X(3,*),DIAG_K(*),LT_K(*),B(*),A(3,*),
     .     KSS(6),KSM(3,3),KNM(3,3,*),KRM(3,3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER NIR, I, J, J1, J2, J3, J4, K, JD, L, JJ,
     .          I1,ID,NL,NI,NJ,NIDOF,ND,NDI,NDJ,NDM,NM,L1,NM1,
     .          IR,IDM,NDOFI
C     REAL
        my_real
     .     H(4),H2(4), SS, TT, SP,SM,TP,TM,KDD(6,6),BD(6),
     .     KII(6,6),BI(6),XS0,YS0,ZS0,XS,YS,ZS,XS1,YS1,ZS1,NUN
C------------------------------------
C     VITESSES DES NOEUDS SECONDS
C------------------------------------
        NUN=-ONE
        I=NSV(II)
        L=IRTL(II)
        NDOFI = 3
C
        SS=CRST(1,II)
        TT=CRST(2,II)
        SS = MIN(ONE,SS)
        TT = MIN(ONE,TT)
        SS = MAX(NUN,SS)
        TT = MAX(NUN,TT)
        SP=ONE + SS
        SM=ONE - SS
        IF (IRECT(3,L)==IRECT(4,L)) THEN
          NIR=3
          TP=THIRD*(ONE + TT)
          TM=THIRD*(ONE - TT)
          H(1)=TM*SM
          H(2)=TM*SP
          H(3)=ONE-H(1)-H(2)
        ELSE
          NIR=4
          TP=FOURTH*(ONE + TT)
          TM=FOURTH*(ONE - TT)
          H(1)=TM*SM
          H(2)=TM*SP
          H(3)=TP*SP
          H(4)=TP*SM
        ENDIF
        NDM = 0
        DO J=1,NIR
          NJ=IRECT(J,L)
          NDM = MAX(NDM,NDOF(NJ))
        ENDDO
C-------NDOF(M)> 3 comme rigid body---
        IF (NDM==6) THEN
          XS0=ZERO
          YS0=ZERO
          ZS0=ZERO
          DO J=1,NIR
            NJ=IRECT(J,L)
            XS0=XS0+X(1,NJ)*H(J)
            YS0=YS0+X(2,NJ)*H(J)
            ZS0=ZS0+X(3,NJ)*H(J)
          ENDDO
          XS=X(1,I)-XS0
          YS=X(2,I)-YS0
          ZS=X(3,I)-ZS0
        ENDIF
        IF (ISS>0) THEN
C-------Update KSS(main node),B---
          IF (IRECT(3,L)==IRECT(4,L)) THEN
            H2(1)=H(1)*H(1)
            H2(2)=H(2)*H(2)
            H2(3)=H(3)*H(3)
          ELSE
            H2(1)=H(1)*H(1)
            H2(2)=H(2)*H(2)
            H2(3)=H(3)*H(3)
            H2(4)=H(4)*H(4)
          ENDIF
          DO K=1,NDOFI
            BD(K)=A(K,I)
            KDD(K,K) = KSS(K)
          ENDDO
          DO K=NDOFI+1,6
            BD(K)=ZERO
          ENDDO
          KDD(1,2) = KSS(4)
          KDD(1,3) = KSS(5)
          KDD(2,3) = KSS(6)
          IF (NDM==6) CALL UPDKB_RB(NDOFI,XS,YS,ZS,KDD,BD)
          DO J=1,NIR
            NJ=IRECT(J,L)
            ND = MIN(NDM,NDOF(NJ))
            CALL UPDKDD(ND,KDD,KII,H2(J),1)
            CALL PUT_KMII(IDLM(J),IADK,DIAG_K,LT_K ,KII,ND)
            DO I1=J+1,NIR
              NM=IRECT(I1,L)
              TM=H(J)*H(I1)
              ND = MIN(ND,NDOF(NM))
              CALL UPDKDD(ND,KDD,KII,TM,0)
              CALL PUT_KMIJ(IDLM(J) ,IDLM(I1) ,IADK,JDIK,LT_K,
     .                      KII,ND ,ND  ,IR )
              IF (IR==1) CALL PRINT_WKIJ(ITAB(NJ) ,ITAB(NM) ,2 )
            ENDDO
          ENDDO
        ENDIF
C
        IF (ISM>0) THEN
C--------no diag--Kjm=sum(KjsCsm)--
          DO K=1,NDOFI
            DO J=1,NDOFI
              KDD(K,J) = KSM(K,J)
            ENDDO
          ENDDO
C------- Update ---
          IF (NDM==6) CALL UPDKB_RB1(NDOFI,NDOFI,XS,YS,ZS,KDD)
          DO J=1,NIR
            NJ=IRECT(J,L)
            NDI = MIN(NDM,NDOFI)
            NDJ = MIN(NDM,NDOF(NJ))
            IF (NDJ>0)CALL UPDKDD1(NDI,NDJ,KDD,KII,H(J),0)
            DO K=1,NDOFI
              DO J1=1,NDOFI
                KNM(K,J1,J)=KII(J1,K)
                KRM(K,J1,J)=KII(J1,K+NDOFI)
              ENDDO
            ENDDO
          ENDDO
        ENDIF
C
        RETURN
      END
Chd|====================================================================
Chd|  I2_FRK1                       source/interfaces/interf/i2_imp1.F
Chd|-- called by -----------
Chd|        UPD_KML                       source/mpi/implicit/imp_fri.F
Chd|        UPD_KSL                       source/mpi/implicit/imp_fri.F
Chd|-- calls ---------------
Chd|        PRINT_WKIJ                    source/implicit/imp_glob_k.F
Chd|        PUT_KMII                      source/implicit/imp_glob_k.F
Chd|        PUT_KMIJ                      source/implicit/imp_glob_k.F
Chd|        UPDK1_II                      source/interfaces/interf/i2_imp1.F
Chd|        UPDK1_IJ                      source/interfaces/interf/i2_imp1.F
Chd|        UPDK1_JJ                      source/interfaces/interf/i2_imp1.F
Chd|====================================================================
      SUBROUTINE I2_FRK1(IRECT,DPARA ,X    ,ITAB ,NSV  ,
     1                   IRTL ,IKC  ,NDOF  ,IDDL ,IDDLM,
     2                   IADK ,JDIK ,DIAG_K,LT_K ,B    ,
     3                   A    ,KSS  ,KSM  ,KNM   ,KRM  ,
     4                   II   ,IDLM ,ISS  ,ISM  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IADK(*),JDIK(*),NDOF(*),IDDL(*),IDDLM(*),IKC(*),
     .          IRECT(4,*), NSV(*),IRTL(*),ITAB(*),II,
     .          IDLM(*)  ,ISS  ,ISM
C     REAL
        my_real
     .     DPARA(7,*),X(3,*),DIAG_K(*),LT_K(*),B(*),A(3,*),
     .     KSS(6),KSM(3,3),KNM(3,3,*),KRM(3,3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER NIR, I, J, J1, J2, J3, J4, K,L,JD, JJ,
     .          I1,ID,NL,NI,NJ,NIDOF,ND,NDI,NDJ,NDM,NM,L1,NM1,
     .          NIR1,IR,IDM,ND1,NDOFI
C     REAL
        my_real
     .     XX,YY,ZZ,XXX,YYY,ZZZ,XY,YZ,ZX,XY2,YZ2,ZX2,
     .     B1,B2,B3,C1,C2,C3,FACM,
     .     X22,Y22,Z22,DET,XM(4),YM(4),ZM(4),KDD(6,6),BD(6),
     .     KII(6,6),BI(6),X0,Y0,Z0,XS,YS,ZS,XS1,YS1,ZS1,NUN,
     .     RJ(3,3,4),RJT(3,3,4)
C------------------------------------
C     VITESSES DES NOEUDS SECONDS
C------------------------------------
        I=NSV(II)
        L=IRTL(II)
        NDOFI = 3
C
        NIR=4
        DO J=1,NIR
          NJ=IRECT(J,L)
          XM(J)=X(1,NJ)
          YM(J)=X(2,NJ)
          ZM(J)=X(3,NJ)
        ENDDO
        IF (IRECT(3,L)==IRECT(4,L)) THEN
          NIR=3
          XM(4)=ZERO
          YM(4)=ZERO
          ZM(4)=ZERO
        ENDIF
        FACM = ONE / NIR
        X0=FACM*(XM(1)+XM(2)+XM(3)+XM(4))
        Y0=FACM*(YM(1)+YM(2)+YM(3)+YM(4))
        Z0=FACM*(ZM(1)+ZM(2)+ZM(3)+ZM(4))
        DO J=1,NIR
          XM(J)=XM(J)-X0
          YM(J)=YM(J)-Y0
          ZM(J)=ZM(J)-Z0
        ENDDO
        XS=X(1,I)-X0
        YS=X(2,I)-Y0
        ZS=X(3,I)-Z0
C--------cette partie est une double travail que INTTI1
        XX=0
        YY=0
        ZZ=0
        XY=0
        YZ=0
        ZX=0
        DO J=1,NIR
          XX=XX+ XM(J)*XM(J)
          YY=YY+ YM(J)*YM(J)
          ZZ=ZZ+ ZM(J)*ZM(J)
          XY=XY+ XM(J)*YM(J)
          YZ=YZ+ YM(J)*ZM(J)
          ZX=ZX+ ZM(J)*XM(J)
        ENDDO
        ZZZ=XX+YY
        XXX=YY+ZZ
        YYY=ZZ+XX
        XY2=XY*XY
        YZ2=YZ*YZ
        ZX2=ZX*ZX
        DET= XXX*YYY*ZZZ -XXX*YZ2 -YYY*ZX2 -ZZZ*XY2 -TWO*XY*YZ*ZX
        DET=ONE/DET
        B1=(ZZZ*YYY-YZ2)*DET
        B2=(XXX*ZZZ-ZX2)*DET
        B3=(YYY*XXX-XY2)*DET
        C3=(ZZZ*XY+YZ*ZX)*DET
        C1=(XXX*YZ+ZX*XY)*DET
        C2=(YYY*ZX+XY*YZ)*DET
        DO J=1,NIR
          X22 = C1*XM(J)
          Y22 = C2*YM(J)
          Z22 = C3*ZM(J)
C-------RJ=(R^tR)^-1Rj^t-(j=1,ndir)---
          RJ(1,1,J)=Z22-Y22
          RJ(2,1,J)=B2*ZM(J)-C1*YM(J)
          RJ(3,1,J)=C1*ZM(J)-B3*YM(J)
          RJ(1,2,J)=-B1*ZM(J)+C2*XM(J)
          RJ(2,2,J)=-Z22+X22
          RJ(3,2,J)=-C2*ZM(J)+B3*XM(J)
          RJ(1,3,J)=B1*YM(J)-C3*XM(J)
          RJ(2,3,J)=C3*YM(J)-B2*XM(J)
          RJ(3,3,J)=Y22-X22
C-------RJT=1/4[I]+(Rs)RJ---
          DO K=1,3
            RJT(1,K,J)=RJ(2,K,J)*ZS-RJ(3,K,J)*YS
            RJT(2,K,J)=-RJ(1,K,J)*ZS+RJ(3,K,J)*XS
            RJT(3,K,J)=RJ(1,K,J)*YS-RJ(2,K,J)*XS
          ENDDO
          DO K=1,3
            RJT(K,K,J)=RJT(K,K,J)+FACM
          ENDDO
        ENDDO
C
        NDM = 3
        IF (ISS>0) THEN
CC-------Update KSS(main node),B---
          DO K=1,NDOFI
            BD(K)=A(K,I)
            KDD(K,K) = KSS(K)
          ENDDO
          DO K=NDOFI+1,6
            BD(K)=ZERO
          ENDDO
          KDD(1,2) = KSS(4)
          KDD(1,3) = KSS(5)
          KDD(2,3) = KSS(6)
          DO J=1,NIR
            NJ=IRECT(J,L)
            ND = MIN(NDM,NDOF(NJ))
            CALL UPDK1_II(NDOFI,RJ(1,1,J),RJT(1,1,J),KDD,KII,BD,BI)
            CALL PUT_KMII(IDLM(J),IADK,DIAG_K,LT_K ,KII,ND)
            DO I1=J+1,NIR
              NM=IRECT(I1,L)
              ND1 = MIN(ND,NDOF(NM))
              CALL UPDK1_IJ(NDOFI,NDOFI,RJ(1,1,J),RJT(1,1,J),
     1                      RJ(1,1,I1),RJT(1,1,I1),KDD,KII,0)
              CALL PUT_KMIJ(IDLM(J) ,IDLM(I1) ,IADK,JDIK,LT_K,
     .                      KII,ND ,ND1 ,IR )
              IF (IR==1) CALL PRINT_WKIJ(ITAB(NJ) ,ITAB(NM) ,2 )
            ENDDO
          ENDDO
        ENDIF
C
        IF (ISM>0) THEN
C--------no diag--Kjm=sum(KjsCsm)--
          DO K=1,NDOFI
            DO J=1,NDOFI
              KDD(K,J) = KSM(K,J)
            ENDDO
          ENDDO
C------- Update ---
          DO J=1,NIR
            NJ=IRECT(J,L)
            NDJ = MIN(NDM,NDOF(NJ))
            IF (NDJ>0)THEN
              CALL UPDK1_JJ(NDOFI,NDOFI,RJ(1,1,J),RJT(1,1,J),KDD,KII)
              DO K=1,NDOFI
                DO J1=1,NDOFI
                  KNM(K,J1,J)=KII(J1,K)
                  KRM(K,J1,J)=KII(J1,K+NDOFI)
                ENDDO
              ENDDO
            ENDIF
          ENDDO
        ENDIF
C
        RETURN
      END

