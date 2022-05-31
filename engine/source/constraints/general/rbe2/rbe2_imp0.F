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
Chd|  RBE2_IMP0                     source/constraints/general/rbe2/rbe2_imp0.F
Chd|-- called by -----------
Chd|        UPD_GLOB_K                    source/implicit/upd_glob_k.F  
Chd|-- calls ---------------
Chd|        PRERBE2                       source/constraints/general/rbe2/rbe2f.F
Chd|        RBE2_IMP1                     source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE2_IMPL                     source/constraints/general/rbe2/rbe2_imp0.F
Chd|====================================================================
      SUBROUTINE RBE2_IMP0(
     1                    IRBE2  ,LRBE2  ,X      ,NSRB2  ,ISB2   ,
     2                    IKC    ,NDOF   ,IDDL   ,IADK   ,JDIK   ,
     3                    DIAG_K ,LT_K   ,B      ,WEIGHT ,ITAB   ,
     4                    SKEW   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER WEIGHT(*),IRBE2(NRBE2L,*),LRBE2(*),
     .        IADK(*),JDIK(*),NDOF(*),ITAB(*),
     .        IDDL(*),IKC(*),ISB2(*),NSRB2(*)
C     REAL ou REAL*8
      my_real
     .   X(3,*), SKEW(LSKEW,*),DIAG_K(*),LT_K(*),B(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, M, NS ,NML, IAD,JJ,IROT,IADS,MAX_M,
     .        JT(3,NRBE2),JR(3,NRBE2),NM,NN,NSJ,IADJ,NSN,ISK,IC,IRAD
C     REAL ou REAL*8
C-----------------------------------------------
      CALL PRERBE2(IRBE2 ,JT  ,JR   )
      IADJ=1
      DO N=1,NRBE2
        IAD = IRBE2(1,N)
        M   = IRBE2(3,N)
        NSN = IRBE2(5,N)
            ISK  =IRBE2(7,N)
            NSJ  =IRBE2(8,N)
            IRAD =IRBE2(11,N)
        IC =JT(1,N)+JT(2,N)+JT(3,N)+JR(1,N)+JR(2,N)+JR(3,N)
       IF (ISK.GT.1.AND.IC.LT.6) THEN
         CALL RBE2_IMPL(M     ,NSN   ,LRBE2(IAD+1) ,X   ,NSRB2(IAD+1),
     2                  ISB2(IADJ),JT(1,N)  ,JR(1,N),IKC   ,NDOF  ,
     3                  IDDL  ,IADK   ,JDIK  ,DIAG_K,LT_K  ,B     ,
     4                  SKEW(1,ISK),ITAB    ,IRAD   )
       ELSE
         CALL RBE2_IMP1(M     ,NSN   ,LRBE2(IAD+1) ,X   ,NSRB2(IAD+1),
     2                  ISB2(IADJ),JT(1,N)  ,JR(1,N),IKC   ,NDOF  ,
     3                  IDDL  ,IADK   ,JDIK  ,DIAG_K,LT_K  ,B     ,
     4                  ITAB  ,IRAD   )
       END IF
       IADJ=IADJ+NSJ
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  RBE2_IMPI                     source/constraints/general/rbe2/rbe2_imp0.F
Chd|-- called by -----------
Chd|        UPD_INT_K                     source/implicit/upd_glob_k.F  
Chd|-- calls ---------------
Chd|        PRERBE2                       source/constraints/general/rbe2/rbe2f.F
Chd|        RBE2_IMP1                     source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE2_IMPL                     source/constraints/general/rbe2/rbe2_imp0.F
Chd|====================================================================
      SUBROUTINE RBE2_IMPI(
     1                    IRBE2  ,LRBE2 ,X     ,SKEW   ,
     2                    NSB2   ,ISB2  ,IKC   ,NDOF   ,IDDL   ,
     3                    IADK   ,JDIK  ,DIAG_K ,LT_K  ,B     ,
     4                    WEIGHT ,ITAB  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER WEIGHT(*),IRBE2(NRBE2L,*),LRBE2(*),
     .        IADK(*),JDIK(*),NDOF(*),ITAB(*),
     .        IDDL(*),IKC(*),NSB2(*),ISB2(*)
C     REAL ou REAL*8
      my_real
     .   X(3,*), SKEW(LSKEW,*), DIAG_K(*),LT_K(*),B(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, M, NS ,NML, IAD,JJ,IROT,IADS,IC,
     .        JT(3,NRBE2),JR(3,NRBE2),NM,NN,NSN,IADJ,ISK,IRAD
C     REAL ou REAL*8
C-----------------------------------------------
      CALL PRERBE2(IRBE2 ,JT  ,JR   )
      IADJ=1
      DO N=1,NRBE2
        IAD = IRBE2(1,N)
        M   = IRBE2(3,N)
        NSN = IRBE2(5,N)
        ISK = IRBE2(7,N)
            IRAD =IRBE2(11,N)
C----------debug: only for the lowest hierarchy
           IF (IRBE2(9,N)==0) THEN
        DO J=1,3
         JR(J,N)=0
        ENDDO
       END IF
         IC =JT(1,N)+JT(2,N)+JT(3,N)
       IF (NDOF(M).GT.0) THEN
        IF (ISK.GT.1.AND.IC.NE.3) THEN
            CALL RBE2_IMPL(M     ,NSN   ,LRBE2(IAD+1) ,X ,NSB2(IAD+1),
     2                     ISB2(IADJ),JT(1,N)  ,JR(1,N),IKC   ,NDOF  ,
     3                     IDDL  ,IADK   ,JDIK  ,DIAG_K,LT_K  ,B     ,
     4                     SKEW(1,ISK),ITAB  ,IRAD   )
        ELSE
          IF (IRBE2(9,N)==0) JR(1,N)=-1
           CALL RBE2_IMP1(M     ,NSN   ,LRBE2(IAD+1) ,X ,NSB2(IAD+1),
     2                     ISB2(IADJ),JT(1,N)  ,JR(1,N),IKC   ,NDOF  ,
     3                     IDDL  ,IADK   ,JDIK  ,DIAG_K,LT_K  ,B     ,
     4                     ITAB  ,IRAD   )
        ENDIF
       END IF
       DO J =1,NSN
        IADJ=IADJ+NSB2(IAD+J)
       ENDDO
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  RBE2_IMP1                     source/constraints/general/rbe2/rbe2_imp0.F
Chd|-- called by -----------
Chd|        RBE2_IMP0                     source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE2_IMPI                     source/constraints/general/rbe2/rbe2_imp0.F
Chd|-- calls ---------------
Chd|        GET_KII                       source/implicit/imp_glob_k.F  
Chd|        GET_KIJ                       source/implicit/imp_glob_k.F  
Chd|        PRINT_WKIJ                    source/implicit/imp_glob_k.F  
Chd|        PUT_KII                       source/implicit/imp_glob_k.F  
Chd|        PUT_KIJ                       source/implicit/imp_glob_k.F  
Chd|        SYM_KDD                       source/constraints/general/rbe2/rbe2_imp0.F
Chd|        UPDKB_RB                      source/constraints/general/rbody/rby_imp0.F
Chd|        UPDKB_RB1                     source/constraints/general/rbody/rby_imp0.F
Chd|        UPDK_BC                       source/constraints/general/rbe3/rbe3_imp0.F
Chd|        UPDK_BC2                      source/constraints/general/rbe3/rbe3_imp0.F
Chd|====================================================================
      SUBROUTINE RBE2_IMP1(M     ,NSN   ,ISL   ,X      ,NSJ    ,
     2                    ISJ   ,JT    ,JR    ,IKC    ,NDOF   ,
     3                    IDDL  ,IADK  ,JDIK  ,DIAG_K,LT_K    ,
     4                    B     ,ITAB  ,IRAD   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER M, NSN,ISL(*),NSJ(*),ISJ(*) ,JT(3),JR(3),IRAD
      INTEGER IADK(*),JDIK(*),NDOF(*),IDDL(*),IKC(*),ITAB(*)
C     REAL ou REAL*8
      my_real
     .   X(3,*),DIAG_K(*),LT_K(*),B(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIR, I, J, J1, J2, J3, J4, K, JD, II, L, JJ,
     .        I1,ID,NL,NI,NJ,NIDOF,ND,NDI,NDJ,NDM,NM,L1,NM1,
     .        NIR1,IR,IP,ISTIF,IMD,N,NTDOF
C     REAL ou REAL*8
      my_real
     .   KDD(6,6),BD(6),XS,YS,ZS,TMP
C------------------------------------
C     VITESSES DES NOEUDS SECONDS
C------------------------------------
        IP =5
        IMD = IDDL(M)+1
        ND = NDOF(M)
C--------boucle secnd nodes--
       NTDOF=JT(1)+JT(2)+JT(3)+JR(1)+JR(2)+JR(3)
C-----cas exception: e.g.:contact-----
       IF ((JT(1)+JT(2)+JT(3)).EQ.3.AND.JR(1).LT.0) THEN
        NTDOF=6
        JR(1)=0
       ENDIF
       J1=0
       DO I=1,NSN
C--------block diagonal Kmm--
        N = ISL(I)
        IF (NDOF(N).GT.0) THEN
         XS=X(1,N)-X(1,M)
         YS=X(2,N)-X(2,M)
         ZS=X(3,N)-X(3,M)
         DO K=1,6
          BD(K)=ZERO
          DO J=1,6
           KDD(K,J)=ZERO
          ENDDO
         ENDDO
         DO K=1,MIN(3,NDOF(N))
          ID = IDDL(N)+K
          IKC(ID)=16*JT(K)
          BD(K)=B(ID)*JT(K)
         ENDDO
         DO K=MIN(3,NDOF(N))+1,NDOF(N)
          ID = IDDL(N)+K
          IKC(ID)=16*JR(K-3)
          BD(K)=B(ID)*JR(K-3)
         ENDDO
         CALL GET_KII(N ,IDDL ,IADK,DIAG_K,LT_K ,KDD,NDOF(N))
         IF (IRAD.EQ.0) CALL UPDK_BC(JT,JR,KDD ,ISTIF)
         CALL UPDKB_RB(NDOF(N),XS,YS,ZS,KDD,BD)
         IF (IRAD.GT.0)CALL UPDK_BC(JT,JR,KDD ,ISTIF)
C-------Update K,B---
         CALL PUT_KII(M ,IDDL ,IADK,DIAG_K,LT_K ,KDD,ND)
         DO K=1,ND
          ID = IMD+K-1
          B(ID) = B(ID) + BD(K)
         ENDDO
C--------no diag--Kjm=sum(KjsCsm)--
         NDM=0
         DO I1 = 1,NSJ(I)
          NI=ISJ(I1+J1)
          NIDOF=NDOF(NI)
C          NDM = MAX(NDM,NIDOF)
          DO K=1,6
           DO J=1,6
            KDD(K,J)=ZERO
           ENDDO
          ENDDO
          CALL GET_KIJ(NI,N,IDDL,IADK,JDIK,LT_K,KDD,NIDOF,NDOF(N),IR)
          IF (IR.EQ.1) CALL PRINT_WKIJ(ITAB(NI) ,ITAB(N) ,IP )
          IF (IRAD.EQ.0) CALL UPDK_BC2(JT,JR,KDD,ISTIF)
C-------  Update ---
           CALL UPDKB_RB1(NIDOF,NDOF(N),XS,YS,ZS,KDD)
           IF (IRAD.GT.0) CALL UPDK_BC2(JT,JR,KDD ,ISTIF)
           IF (NI.EQ.M) THEN
            CALL SYM_KDD(6,KDD)
            CALL PUT_KII(M ,IDDL ,IADK,DIAG_K,LT_K ,KDD,ND)
           ELSE
            CALL PUT_KIJ(NI,M,IDDL,IADK,JDIK,LT_K,KDD,NIDOF,ND,IR)
            IF (IR.EQ.1) CALL PRINT_WKIJ(ITAB(NI) ,ITAB(M) ,IP )
           ENDIF
         ENDDO
         J1=J1+NSJ(I)
             IF (NTDOF.LT.6) THEN
           DO K=1,6
            DO J=K,6
             KDD(K,J)=ZERO
            ENDDO
           ENDDO
           CALL GET_KII(N ,IDDL ,IADK,DIAG_K,LT_K ,KDD,NDOF(N))
           DO K=1,6
            DO J=K+1,6
             KDD(J,K)=KDD(K,J)
            ENDDO
           ENDDO
           IF (IRAD.EQ.0) CALL UPDK_BC2(JT,JR,KDD ,ISTIF)
           CALL UPDKB_RB1(NDOF(N),NDOF(N),XS,YS,ZS,KDD)
           IF (IRAD.GT.0) CALL UPDK_BC2(JT,JR,KDD ,ISTIF)
           CALL PUT_KIJ(N,M,IDDL,IADK,JDIK,LT_K,KDD,NDOF(N),ND,IR)
           IF (IR.EQ.1) CALL PRINT_WKIJ(ITAB(N) ,ITAB(M) ,IP )
             ENDIF
        ENDIF
C-------fin -boucle secnd nodes--
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  RBE2_IMPL                     source/constraints/general/rbe2/rbe2_imp0.F
Chd|-- called by -----------
Chd|        RBE2_IMP0                     source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE2_IMPI                     source/constraints/general/rbe2/rbe2_imp0.F
Chd|-- calls ---------------
Chd|        CDI_SKEW                      source/constraints/general/rbe2/rbe2_imp0.F
Chd|        GET_KII                       source/implicit/imp_glob_k.F  
Chd|        GET_KIJ                       source/implicit/imp_glob_k.F  
Chd|        PRINT_WKIJ                    source/implicit/imp_glob_k.F  
Chd|        PUT_KII                       source/implicit/imp_glob_k.F  
Chd|        PUT_KIJ                       source/implicit/imp_glob_k.F  
Chd|        RBE2_BCL                      source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE2_IMPBL                    source/constraints/general/rbe2/rbe2_imp0.F
Chd|        SYM_KDD                       source/constraints/general/rbe2/rbe2_imp0.F
Chd|        UPDCDIK2_CDI                  source/constraints/general/rbe2/rbe2_imp0.F
Chd|        UPDK2_CDI                     source/constraints/general/rbe2/rbe2_imp0.F
Chd|====================================================================
      SUBROUTINE RBE2_IMPL(M     ,NSN   ,ISL   ,X      ,NSJ    ,
     2                    ISJ    ,JT    ,JR    ,IKC    ,NDOF   ,
     3                    IDDL   ,IADK  ,JDIK  ,DIAG_K,LT_K    ,
     4                    B      ,SKEW  ,ITAB  ,IRAD   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER M, NSN,ISL(*),NSJ(*),ISJ(*) ,JT(3),JR(3),IRAD
      INTEGER IADK(*),JDIK(*),NDOF(*),IDDL(*),IKC(*),ITAB(*)
C     REAL ou REAL*8
      my_real
     .   X(3,*),DIAG_K(*),LT_K(*),B(*),SKEW(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIR, I, J, J1, J2, J3, J4, K, JD, II, L, JJ,
     .        I1,ID,NL,NI,NJ,NIDOF,ND,NDI,NDJ,NDM,NM,L1,NM1,
     .        NIR1,IR,IP,ISTIF,IMD,N,NT,NR,IC,JT1(3),JR1(3)
C     REAL ou REAL*8
      my_real
     .   KDD(6,6),BD(6),XS,YS,ZS,TMP,CDT(3,3),CDTR(3,3),CDR(3,3)
C------------------------------------
C     VITESSES DES NOEUDS SECONDS
C------------------------------------
        IP =5
        IMD = IDDL(M)+1
        ND = NDOF(M)
        NT=JT(1)+JT(2)+JT(3)
        NR=JR(1)+JR(2)+JR(3)
C--------upd B--
         CALL RBE2_IMPBL(M      ,NSN    ,ISL   ,X   ,JT       ,
     1                   JR     ,NDOF   ,IDDL  ,B   ,SKEW     ,
     2                   IRAD   )
C--------boucle secnd nodes--
       J1=0
       DO I=1,NSN
C--------block diagonal Kmm--
        N = ISL(I)
        IF (NDOF(N).GT.0) THEN
         XS=X(1,N)-X(1,M)
         YS=X(2,N)-X(2,M)
         ZS=X(3,N)-X(3,M)
         DO K=1,6
          DO J=1,6
           KDD(K,J)=ZERO
          ENDDO
         ENDDO
         DO K=1,MIN(3,NDOF(N))
          ID = IDDL(N)+K
          IF (NT.EQ.3)IKC(ID)=16*JT(K)
         ENDDO
         DO K=MIN(3,NDOF(N))+1,NDOF(N)
          ID = IDDL(N)+K
c          IKC(ID)=16*JR(K-3)
          IF (NR.EQ.3) IKC(ID)=16*JR(K-3)
         ENDDO
         CALL CDI_SKEW(XS,YS,ZS,JT,JR,SKEW,CDT,CDR,CDTR,JT1,JR1,IRAD)
         CALL GET_KII(N ,IDDL ,IADK,DIAG_K,LT_K ,KDD,NDOF(N))
c         CALL UPDK_BC(JT1,JR1,KDD ,ISTIF)
C-------Update K,B---
         DO K=1,6
          DO J=K+1,6
           KDD(J,K)=KDD(K,J)
          ENDDO
         ENDDO
         CALL UPDCDIK2_CDI(ND,CDT,CDR,CDTR,KDD)
         CALL PUT_KII(M ,IDDL ,IADK,DIAG_K,LT_K ,KDD,ND)
C--------no diag--Kjm=sum(KjsCsm)--
         DO I1 = 1,NSJ(I)
          NI=ISJ(I1+J1)
          NIDOF=NDOF(NI)
C          NDM = MAX(NDM,NIDOF)
          DO K=1,6
           DO J=1,6
            KDD(K,J)=ZERO
           ENDDO
          ENDDO
          CALL GET_KIJ(NI,N,IDDL,IADK,JDIK,LT_K,KDD,NIDOF,NDOF(N),IR)
          IF (IR.EQ.1) CALL PRINT_WKIJ(ITAB(NI) ,ITAB(N) ,IP )
c          CALL UPDK_BC2(JT1,JR1,KDD ,ISTIF)
C-------  Update ---
           CALL UPDK2_CDI(NIDOF,NDOF(N),CDT,CDR,CDTR,KDD)
           IF (NI.EQ.M) THEN
            CALL SYM_KDD(6,KDD)
            CALL PUT_KII(M ,IDDL ,IADK,DIAG_K,LT_K ,KDD,ND)
           ELSE
            CALL PUT_KIJ(NI,M,IDDL,IADK,JDIK,LT_K,KDD,NIDOF,ND,IR)
            IF (IR.EQ.1) CALL PRINT_WKIJ(ITAB(NI) ,ITAB(M) ,IP )
           ENDIF
         ENDDO
         J1=J1+NSJ(I)
C        IF ((NT+NR).LT.6) THEN
           DO K=1,6
            DO J=K,6
             KDD(K,J)=ZERO
            ENDDO
           ENDDO
           CALL GET_KII(N ,IDDL ,IADK,DIAG_K,LT_K ,KDD,NDOF(N))
           DO K=1,6
            DO J=K+1,6
             KDD(J,K)=KDD(K,J)
            ENDDO
           ENDDO
           CALL UPDK2_CDI(NDOF(N),NDOF(N),CDT,CDR,CDTR,KDD)
           CALL PUT_KIJ(N,M,IDDL,IADK,JDIK,LT_K,KDD,NDOF(N),ND,IR)
           IF (IR.EQ.1) CALL PRINT_WKIJ(ITAB(N) ,ITAB(M) ,IP )
C        ENDIF
          IF (NT.GT.0.AND.NT.LT.3) THEN
            IC = JT(1)*100+JT(2)*10+JT(3)
            IR =0
            CALL RBE2_BCL(IC   ,SKEW  ,IDDL  ,IKC   ,IADK ,
     1                    JDIK ,DIAG_K,LT_K  ,N     ,IR    )
          ENDIF
          IF (NR.GT.0.AND.NR.LT.3) THEN
            IC = JR(1)*100+JR(2)*10+JR(3)
            IR =1
            CALL RBE2_BCL(IC   ,SKEW  ,IDDL  ,IKC   ,IADK ,
     1                    JDIK ,DIAG_K,LT_K  ,N     ,IR    )
          ENDIF
        ENDIF
C-------fin -boucle secnd nodes--
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  RBE2_IMPR1                    source/constraints/general/rbe2/rbe2_imp0.F
Chd|-- called by -----------
Chd|        IMP_DYKV                      source/implicit/imp_dyna.F    
Chd|        IMP_DYKV0                     source/implicit/imp_dyna.F    
Chd|        UPD_RHS                       source/implicit/upd_glob_k.F  
Chd|        UPD_RHS_FR                    source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        PRERBE2                       source/constraints/general/rbe2/rbe2f.F
Chd|        RBE2_IMPB0                    source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE2_IMPBL                    source/constraints/general/rbe2/rbe2_imp0.F
Chd|====================================================================
      SUBROUTINE RBE2_IMPR1(
     1                    IRBE2  ,LRBE2 ,X     ,SKEW   ,NDOF   ,
     2                    IDDL   ,B     ,WEIGHT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER WEIGHT(*),IRBE2(NRBE2L,*),LRBE2(*),
     .        NDOF(*),IDDL(*),IRAD
C     REAL ou REAL*8
      my_real
     .   X(3,*), SKEW(LSKEW,*), B(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, M, NS ,NML, IAD,JJ,IROT,IADS,
     .        JT(3,NRBE2),JR(3,NRBE2),NSN,NSJ,ISK
C     REAL ou REAL*8
C-----------------------------------------------
      CALL PRERBE2(IRBE2 ,JT  ,JR   )
      DO N=1,NRBE2
        IAD = IRBE2(1,N)
        M  = IRBE2(3,N)
        NSN = IRBE2(5,N)
        ISK = IRBE2(7,N)
        IRAD = IRBE2(11,N)
        IF (ISK.GT.1) THEN
         CALL RBE2_IMPBL(M      ,NSN    ,LRBE2(IAD+1),X   ,JT(1,N),
     1                   JR(1,N),NDOF   ,IDDL  ,B     ,SKEW(1,ISK),
     2                   IRAD   )
        ELSE
         CALL RBE2_IMPB0(M      ,NSN    ,LRBE2(IAD+1),X   ,JT(1,N),
     1                   JR(1,N),NDOF   ,IDDL  ,B     ,IRAD  )
        END IF
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  RBE2_IMPB0                    source/constraints/general/rbe2/rbe2_imp0.F
Chd|-- called by -----------
Chd|        RBE2_IMPR1                    source/constraints/general/rbe2/rbe2_imp0.F
Chd|-- calls ---------------
Chd|        UPDB_RB                       source/constraints/general/rbody/rby_imp0.F
Chd|====================================================================
      SUBROUTINE RBE2_IMPB0(M     ,NSN   ,ISL   ,X      ,JT    ,
     2                      JR    ,NDOF  ,IDDL  ,B      ,IRAD  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER M, NSN,ISL(*),JT(3),JR(3),NDOF(*),IDDL(*),IRAD
C     REAL ou REAL*8
      my_real
     .   X(3,*),B(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ISK, I, N, J,NI,NJ,J1,K,L,ID,JD,ND,IMD,NIDOF
C     REAL ou REAL*8
      my_real
     .   XS,YS,ZS,BD(6)
C------------------------------------
C     VITESSES DES NOEUDS SECONDS
C------------------------------------
       IF (NDOF(M).LE.0) RETURN
C
        ND = NDOF(M)
       DO I=1,NSN
C--------block diagonal Kmm--
        N = ISL(I)
       IF (NDOF(N).GT.0) THEN
         XS=X(1,N)-X(1,M)
         YS=X(2,N)-X(2,M)
         ZS=X(3,N)-X(3,M)
         DO K=1,6
          BD(K)=ZERO
         ENDDO
        IF (IRAD.GT.0) THEN
         DO K=1,MIN(3,NDOF(N))
          ID = IDDL(N)+K
          BD(K)=B(ID)
         ENDDO
         CALL UPDB_RB(NDOF(N),XS,YS,ZS,BD)
         DO K=1,MIN(3,NDOF(N))
          ID = IDDL(N)+K
          BD(K)=B(ID)*JT(K)
         ENDDO
         DO K=MIN(3,NDOF(N))+1,NDOF(N)
          ID = IDDL(N)+K
          BD(K)=(BD(K)+B(ID))*JR(K-3)
         ENDDO
        ELSE
         DO K=1,MIN(3,NDOF(N))
          ID = IDDL(N)+K
          BD(K)=B(ID)*JT(K)
         ENDDO
         DO K=MIN(3,NDOF(N))+1,NDOF(N)
          ID = IDDL(N)+K
          BD(K)=B(ID)*JR(K-3)
         ENDDO
         CALL UPDB_RB(NDOF(N),XS,YS,ZS,BD)
        END IF !(IRAD.GT.0) THEN
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
Chd|  RBE2_IMPBL                    source/constraints/general/rbe2/rbe2_imp0.F
Chd|-- called by -----------
Chd|        RBE2_IMPL                     source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE2_IMPR1                    source/constraints/general/rbe2/rbe2_imp0.F
Chd|-- calls ---------------
Chd|        CDI_SKEW                      source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE2IMPBSN                    source/constraints/general/rbe2/rbe2_imp0.F
Chd|====================================================================
      SUBROUTINE RBE2_IMPBL(M     ,NSN   ,ISL   ,X      ,JT    ,
     2                      JR    ,NDOF  ,IDDL  ,B      ,SKEW  ,
     3                      IRAD  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER M, NSN,ISL(*),JT(3),JR(3),NDOF(*),IDDL(*),IRAD
C     REAL ou REAL*8
      my_real
     .   X(3,*),B(*),SKEW(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ISK, I, N, J,NI,NJ,J1,K,L,ID,JD,ND,IMD,NDS,
     .        IT,IR,IC,JT1(3),JR1(3)
C     REAL ou REAL*8
      my_real
     .   XS,YS,ZS,LXS,LYS,LZS,BD(6),BDL(6),BB(3),BM(3),
     .   CDT(3,3),CDTR(3,3),CDR(3,3)
C------------------------------------
C     VITESSES DES NOEUDS SECONDS
C------------------------------------
       IF (NDOF(M).LE.0) RETURN
C
        ND = NDOF(M)
C-------translation-------
       DO I=1,NSN
C-------
        N = ISL(I)
       IF (NDOF(N).GT.0) THEN
         XS=X(1,N)-X(1,M)
         YS=X(2,N)-X(2,M)
         ZS=X(3,N)-X(3,M)
         DO K=1,3
          BB(K)=ZERO
          BM(K)=ZERO
          BD(K)=ZERO
          BD(K+3)=ZERO
         ENDDO
         CALL CDI_SKEW(XS,YS,ZS,JT,JR,SKEW,CDT,CDR,CDTR,JT1,JR1,IRAD)
         DO K=1,MIN(3,NDOF(N))
          ID = IDDL(N)+K
          BB(K)=B(ID)
C          BB(K)=B(ID)*JT1(K)
         ENDDO
         DO K=4,NDOF(N)
          ID = IDDL(N)+K
          BM(K-3)=B(ID)
C          BM(K-3)=B(ID)*JR1(K-3)
         ENDDO
         DO K=1,3
          DO L=1,3
           BD(K)=BD(K)+CDT(L,K)*BB(L)
           BD(K+3)=BD(K+3)+CDR(L,K)*BM(L)
           BD(K+3)=BD(K+3)+CDTR(L,K)*BB(L)
          ENDDO
         ENDDO
C-------Update B---
         DO K=1,ND
          ID = IDDL(M)+K
          B(ID) = B(ID) + BD(K)
         ENDDO
        ENDIF
       ENDDO
      IC = JT(1)*100+JT(2)*10+JT(3)
      IF (IC.GT.0.AND.IC.LT.111) THEN
       IR = 0
       CALL RBE2IMPBSN(NSN   ,ISL   ,B     ,IC     ,NDOF   ,
     1                 IDDL  ,SKEW  ,IR    )
      END IF
      IC = JR(1)*100+JR(2)*10+JR(3)
      IF (IC.GT.0.AND.IC.LT.111) THEN
       IR = 1
       CALL RBE2IMPBSN(NSN   ,ISL   ,B     ,IC     ,NDOF   ,
     1                 IDDL  ,SKEW  ,IR    )
      END IF
C
      RETURN
      END
Chd|====================================================================
Chd|  SYM_KDD                       source/constraints/general/rbe2/rbe2_imp0.F
Chd|-- called by -----------
Chd|        RBE2_IMP1                     source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE2_IMPL                     source/constraints/general/rbe2/rbe2_imp0.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SYM_KDD(ND,KDD)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ND
C     REAL ou REAL*8
      my_real
     .    KDD(ND,ND)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,K,L
        DO I=1,ND
         DO J=1,ND
          KDD(I,J)=KDD(I,J)+KDD(J,I)
         ENDDO
        ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  CDI_SKEW                      source/constraints/general/rbe2/rbe2_imp0.F
Chd|-- called by -----------
Chd|        RBE2_FRK                      source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE2_IMPBL                    source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE2_IMPKD                    source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE2_IMPL                     source/constraints/general/rbe2/rbe2_imp0.F
Chd|-- calls ---------------
Chd|        CDI_BCN                       source/constraints/general/rbe2/rbe2_imp0.F
Chd|        CDI_BCN1                      source/constraints/general/rbe2/rbe2_imp0.F
Chd|====================================================================
      SUBROUTINE CDI_SKEW(XS,YS,ZS,JT,JR,SKEW,KT,KR,KTR,JT1,JR1,IRAD)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JT(3),JR(3),JT1(3),JR1(3),IRAD
C     REAL ou REAL*8
      my_real
     .    XS,YS,ZS,SKEW(*),KT(3,3),KR(3,3),KTR(3,3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,ICT
C------------------------KT=QtJTQ-----------------------
      ICT = JT(1)*100+JT(2)*10+JT(3)
      CALL CDI_BCN(ICT   ,SKEW  ,JT   ,KT  ,JT1 )
      ICT = JR(1)*100+JR(2)*10+JR(3)
      CALL CDI_BCN(ICT   ,SKEW  ,JR   ,KR  ,JR1 )
      CALL CDI_BCN1(XS,YS,ZS,JT,JR,SKEW,KTR,IRAD)
C
      RETURN
      END
Chd|====================================================================
Chd|  UPDK2_CDI                     source/constraints/general/rbe2/rbe2_imp0.F
Chd|-- called by -----------
Chd|        RBE2_FRK                      source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE2_IMPL                     source/constraints/general/rbe2/rbe2_imp0.F
Chd|        UPDCDIK2_CDI                  source/constraints/general/rbe2/rbe2_imp0.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE UPDK2_CDI(NI,NJ,CDT,CDR,CDTR,KDD)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NI,NJ
C     REAL ou REAL*8
      my_real
     .    XS,YS,ZS, KDD(6,6),CDT(3,3),CDR(3,3),CDTR(3,3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,L
C     REAL ou REAL*8
      my_real
     .   K(6,6)
C------------------------------------
C-------------produit {K'}=-[K][CDI]
c-----with [CDI]=-[[CDT] [CDTR]]-----
c----             [[0]   [CDR] ]-----
C
        DO I=1,6
        DO J=1,6
          K(I,J)=ZERO
        ENDDO
        ENDDO
        DO I=1,3
        DO J=1,3
         DO L=1,3
          K(I,J)=K(I,J)+KDD(I,L)*CDT(L,J)
          K(I+3,J)=K(I+3,J)+KDD(I+3,L)*CDT(L,J)
          K(I,J+3)=K(I,J+3)+KDD(I,L)*CDTR(L,J)+
     .                      KDD(I,L+3)*CDR(L,J)
          K(I+3,J+3)=K(I+3,J+3)+KDD(I+3,L)*CDTR(L,J)+
     .                          KDD(I+3,L+3)*CDR(L,J)
         ENDDO
        ENDDO
        ENDDO
C
        DO I=1,NI
        DO J=1,NJ
          KDD(I,J)=K(I,J)
        ENDDO
        ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  UPDCDIK2_CDI                  source/constraints/general/rbe2/rbe2_imp0.F
Chd|-- called by -----------
Chd|        RBE2_FRK                      source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE2_IMPKD                    source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE2_IMPL                     source/constraints/general/rbe2/rbe2_imp0.F
Chd|-- calls ---------------
Chd|        UPDK2_CDI                     source/constraints/general/rbe2/rbe2_imp0.F
Chd|====================================================================
      SUBROUTINE UPDCDIK2_CDI(ND,CDT,CDR,CDTR,KDD)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ND
C     REAL ou REAL*8
      my_real
     .    KDD(6,6),CDT(3,3),CDR(3,3),CDTR(3,3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,L
C     REAL ou REAL*8
      my_real
     .   K(6,6)
C------------------------------------
C-------------produit {K'}=[CDI]^t[K][CDI]
c-----with [CDI]=-[[CDT] [CDTR]]-----
c----             [[0]   [CDR]]-----
C------------------KDD-> [K][CDI]
        CALL UPDK2_CDI(ND,ND,CDT,CDR,CDTR,KDD)
        DO I=1,6
        DO J=1,6
          K(I,J)=ZERO
        ENDDO
        ENDDO
        DO I=1,ND
        DO J=1,ND
          K(I,J)=KDD(J,I)
        ENDDO
        ENDDO
        CALL UPDK2_CDI(ND,ND,CDT,CDR,CDTR,K)
C
        DO I=1,ND
        DO J=1,ND
          KDD(I,J)=K(I,J)
        ENDDO
        ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  RBE2_BCL                      source/constraints/general/rbe2/rbe2_imp0.F
Chd|-- called by -----------
Chd|        RBE2_IMPL                     source/constraints/general/rbe2/rbe2_imp0.F
Chd|-- calls ---------------
Chd|        BC_UPDK                       source/constraints/general/bcs/bc_imp0.F
Chd|        BC_UPDK2                      source/constraints/general/rbe2/rbe2_imp0.F
Chd|        DIR_RBE2                      source/constraints/general/rbe2/rbe2v.F
Chd|        L_DIR                         source/constraints/general/bcs/bc_imp0.F
Chd|====================================================================
      SUBROUTINE RBE2_BCL(ICT  ,SKEW  ,IADN  ,IFIX  ,IADK ,
     1                    JDIK ,DIAG_K,LT_K  ,I     ,IR    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICT,IADN(*),IFIX(*),IADK(*) ,JDIK(*),I,IR
      my_real
     .   SKEW(*),DIAG_K(*),LT_K(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J,K,J1,L,ND
      my_real
     .   EJ(3),EJ1(3),S,EA,EB
C----------------100-------------------------
        ND = IADN(I)
          IF (IR.GT.0) ND = IADN(I) + 3
C-----IF J,J1 change during the deformation---    
         DO J=1,3
          IF (IFIX(ND +J) == 17) IFIX(ND +J)=0
         END DO
C
        SELECT CASE (ICT)
         CASE(100)
          EJ(1)=SKEW(1)
          EJ(2)=SKEW(2)
          EJ(3)=SKEW(3)
          CALL L_DIR(EJ,J)
          CALL BC_UPDK(I     ,IADN  ,EJ    ,J     ,IR    ,
     1                 IADK  ,JDIK  ,DIAG_K,LT_K  )
          IFIX(ND +J) = 17
C----------------010-------------------------
         CASE(10)
          EJ(1)=SKEW(4)
          EJ(2)=SKEW(5)
          EJ(3)=SKEW(6)
          CALL L_DIR(EJ,J)
          CALL BC_UPDK(I     ,IADN  ,EJ    ,J     ,IR    ,
     1                 IADK  ,JDIK  ,DIAG_K,LT_K  )
          IFIX(ND +J) = 17
C----------------001-------------------------
         CASE(1)
          EJ(1)=SKEW(7)
          EJ(2)=SKEW(8)
          EJ(3)=SKEW(9)
          CALL L_DIR(EJ,J)
          CALL BC_UPDK(I     ,IADN  ,EJ    ,J     ,IR    ,
     1                 IADK  ,JDIK  ,DIAG_K,LT_K  )
          IFIX(ND +J) = 17
C----------------011-------------------------
         CASE(11)
          EJ(1)=SKEW(7)
          EJ(2)=SKEW(8)
          EJ(3)=SKEW(9)
          CALL L_DIR(EJ,J)
          IFIX(ND +J) = 17
          EJ1(1)=SKEW(4)
          EJ1(2)=SKEW(5)
          EJ1(3)=SKEW(6)
          CALL L_DIR(EJ1,J1)
          IF (J1.EQ.J) THEN
           EJ1(J)=ZERO
           CALL L_DIR(EJ1,J1)
           EJ1(1)=SKEW(4)/SKEW(3+J1)
           EJ1(2)=SKEW(5)/SKEW(3+J1)
           EJ1(3)=SKEW(6)/SKEW(3+J1)
          ENDIF
          CALL DIR_RBE2(J    ,J1    ,K     )
              S=ONE/(ONE-EJ(J1)*EJ1(J))
              EA=-S*(EJ(J1)*EJ1(K)-EJ(K))
              EB=-S*(EJ1(J)*EJ(K)-EJ1(K))
          CALL BC_UPDK2(I     ,IADN  ,J    ,J1    ,K     ,
     1                  IR    ,EA    ,EB   ,IADK  ,JDIK  ,
     2                  DIAG_K,LT_K  )
          IFIX(ND +J1) = 17
C----------------101-------------------------
         CASE(101)
          EJ(1)=SKEW(7)
          EJ(2)=SKEW(8)
          EJ(3)=SKEW(9)
          CALL L_DIR(EJ,J)
          IFIX(ND +J) = 17
          EJ1(1)=SKEW(1)
          EJ1(2)=SKEW(2)
          EJ1(3)=SKEW(3)
          CALL L_DIR(EJ1,J1)
          IF (J1.EQ.J) THEN
           EJ1(J)=ZERO
           CALL L_DIR(EJ1,J1)
           EJ1(1)=SKEW(1)/SKEW(J1)
           EJ1(2)=SKEW(2)/SKEW(J1)
           EJ1(3)=SKEW(3)/SKEW(J1)
          ENDIF
          CALL DIR_RBE2(J    ,J1    ,K     )
              S=ONE/(ONE-EJ(J1)*EJ1(J))
              EA=-S*(EJ(J1)*EJ1(K)-EJ(K))
              EB=-S*(EJ1(J)*EJ(K)-EJ1(K))
          CALL BC_UPDK2(I     ,IADN  ,J    ,J1    ,K     ,
     1                  IR    ,EA    ,EB   ,IADK  ,JDIK  ,
     2                  DIAG_K,LT_K  )
          IFIX(ND +J1) = 17
C----------------110-------------------------
         CASE(110)
          EJ(1)=SKEW(4)
          EJ(2)=SKEW(5)
          EJ(3)=SKEW(6)
          CALL L_DIR(EJ,J)
          IFIX(ND +J) = 17
          EJ1(1)=SKEW(1)
          EJ1(2)=SKEW(2)
          EJ1(3)=SKEW(3)
          CALL L_DIR(EJ1,J1)
          IF (J1.EQ.J) THEN
           EJ1(J)=ZERO
           CALL L_DIR(EJ1,J1)
           EJ1(1)=SKEW(1)/SKEW(J1)
           EJ1(2)=SKEW(2)/SKEW(J1)
           EJ1(3)=SKEW(3)/SKEW(J1)
          ENDIF
          CALL DIR_RBE2(J    ,J1    ,K     )
              S=ONE/(ONE-EJ(J1)*EJ1(J))
              EA=-S*(EJ(J1)*EJ1(K)-EJ(K))
              EB=-S*(EJ1(J)*EJ(K)-EJ1(K))
          CALL BC_UPDK2(I     ,IADN  ,J    ,J1    ,K     ,
     1                  IR    ,EA    ,EB   ,IADK  ,JDIK  ,
     2                  DIAG_K,LT_K  )
          IFIX(ND +J1) = 17
       END SELECT
C
      RETURN
      END
Chd|====================================================================
Chd|  CDI_BCN                       source/constraints/general/rbe2/rbe2_imp0.F
Chd|-- called by -----------
Chd|        CDI_SKEW                      source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE2FL                        source/constraints/general/rbe2/rbe2f.F
Chd|        RBE2FRF                       source/constraints/general/rbe2/rbe2f.F
Chd|        SMS_RBE_1                     source/ams/sms_rbe2.F         
Chd|        SMS_RBE_5                     source/ams/sms_rbe2.F         
Chd|-- calls ---------------
Chd|        DIR_RBE2                      source/constraints/general/rbe2/rbe2v.F
Chd|        L_DIR                         source/constraints/general/bcs/bc_imp0.F
Chd|====================================================================
      SUBROUTINE CDI_BCN(ICT   ,SKEW  ,JT   ,KT  ,JT1 )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JT(3)   ,ICT,JT1(3)
      my_real
     .   SKEW(*),KT(3,3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,J1,L,ND
      my_real
     .   EJ(3),EJ1(3),S,EA,EB
C-------JT1 presente the real consentration dof----------------------
        DO I=1,3
         DO J=1,3
          KT(I,J)=ZERO
         ENDDO
         JT1(I) = 0
        ENDDO
C----------------100-------------------------
        SELECT CASE (ICT)
         CASE(100)
          EJ(1)=SKEW(1)
          EJ(2)=SKEW(2)
          EJ(3)=SKEW(3)
          CALL L_DIR(EJ,J)
          DO I=1,3
           KT(J,I)=EJ(I)
          ENDDO
          JT1(J) = 1
C----------------010-------------------------
         CASE(10)
          EJ(1)=SKEW(4)
          EJ(2)=SKEW(5)
          EJ(3)=SKEW(6)
          CALL L_DIR(EJ,J)
          DO I=1,3
           KT(J,I)=EJ(I)
          ENDDO
          JT1(J) = 1
C----------------001-------------------------
         CASE(1)
          EJ(1)=SKEW(7)
          EJ(2)=SKEW(8)
          EJ(3)=SKEW(9)
          CALL L_DIR(EJ,J)
          DO I=1,3
           KT(J,I)=EJ(I)
          ENDDO
          JT1(J) = 1
C----------------011-------------------------
         CASE(11)
          EJ(1)=SKEW(7)
          EJ(2)=SKEW(8)
          EJ(3)=SKEW(9)
          CALL L_DIR(EJ,J)
          EJ1(1)=SKEW(4)
          EJ1(2)=SKEW(5)
          EJ1(3)=SKEW(6)
          CALL L_DIR(EJ1,J1)
          IF (J1.EQ.J) THEN
           EJ1(J)=ZERO
           CALL L_DIR(EJ1,J1)
           EJ1(1)=SKEW(4)/SKEW(3+J1)
           EJ1(2)=SKEW(5)/SKEW(3+J1)
           EJ1(3)=SKEW(6)/SKEW(3+J1)
          ENDIF
          CALL DIR_RBE2(J    ,J1    ,K     )
          S=ONE/(ONE-EJ(J1)*EJ1(J))
          EA=S*(EJ(J1)*EJ1(K)-EJ(K))
          EB=S*(EJ1(J)*EJ(K)-EJ1(K))
          KT(J,J)=EJ(J)
          KT(J,K)=-EA
          KT(J1,J1)=EJ1(J1)
          KT(J1,K)=-EB
          JT1(J) = 1
          JT1(J1) = 1
C----------------101-------------------------
         CASE(101)
          EJ(1)=SKEW(7)
          EJ(2)=SKEW(8)
          EJ(3)=SKEW(9)
          CALL L_DIR(EJ,J)
          EJ1(1)=SKEW(1)
          EJ1(2)=SKEW(2)
          EJ1(3)=SKEW(3)
          CALL L_DIR(EJ1,J1)
          IF (J1.EQ.J) THEN
           EJ1(J)=ZERO
           CALL L_DIR(EJ1,J1)
           EJ1(1)=SKEW(1)/SKEW(J1)
           EJ1(2)=SKEW(2)/SKEW(J1)
           EJ1(3)=SKEW(3)/SKEW(J1)
          ENDIF
          CALL DIR_RBE2(J    ,J1    ,K     )
          S=ONE/(ONE-EJ(J1)*EJ1(J))
          EA=S*(EJ(J1)*EJ1(K)-EJ(K))
          EB=S*(EJ1(J)*EJ(K)-EJ1(K))
          KT(J,J)=EJ(J)
          KT(J,K)=-EA
          KT(J1,J1)=EJ1(J1)
          KT(J1,K)=-EB
          JT1(J) = 1
          JT1(J1) = 1
C----------------110-------------------------
         CASE(110)
          EJ(1)=SKEW(4)
          EJ(2)=SKEW(5)
          EJ(3)=SKEW(6)
          CALL L_DIR(EJ,J)
          EJ1(1)=SKEW(1)
          EJ1(2)=SKEW(2)
          EJ1(3)=SKEW(3)
          CALL L_DIR(EJ1,J1)
          IF (J1.EQ.J) THEN
           EJ1(J)=ZERO
           CALL L_DIR(EJ1,J1)
           EJ1(1)=SKEW(1)/SKEW(J1)
           EJ1(2)=SKEW(2)/SKEW(J1)
           EJ1(3)=SKEW(3)/SKEW(J1)
          ENDIF
          CALL DIR_RBE2(J    ,J1    ,K     )
          S=ONE/(ONE-EJ(J1)*EJ1(J))
          EA=S*(EJ(J1)*EJ1(K)-EJ(K))
          EB=S*(EJ1(J)*EJ(K)-EJ1(K))
          KT(J,J)=EJ(J)
          KT(J,K)=-EA
          KT(J1,J1)=EJ1(J1)
          KT(J1,K)=-EB
          JT1(J) = 1
          JT1(J1) = 1
C----------------111-------------------------
         CASE(111)
          DO I=1,3
           KT(I,I)=ONE
           JT1(I) = 1
          ENDDO
       END SELECT
C
      RETURN
      END
Chd|====================================================================
Chd|  RBE2IMPBSN                    source/constraints/general/rbe2/rbe2_imp0.F
Chd|-- called by -----------
Chd|        RBE2_IMPBL                    source/constraints/general/rbe2/rbe2_imp0.F
Chd|-- calls ---------------
Chd|        DIR_RBE2                      source/constraints/general/rbe2/rbe2v.F
Chd|        L_DIR                         source/constraints/general/bcs/bc_imp0.F
Chd|====================================================================
      SUBROUTINE RBE2IMPBSN(NSL   ,ISL   ,B     ,ICT    ,NDOF   ,
     2                     IDDL   ,SKEW  ,IR    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSL   ,ISL(*)   ,ICT, NDOF(*),IDDL(*),IR
      my_real
     .   SKEW(*),B(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,J1,L,NS,ND,NR
      my_real
     .   EJ(3),EJ1(3),S,EA,EB
C----------------100-------------------------
        IF (IR.EQ.0) THEN
         NR = 0
        ELSE
         NR = 3
        ENDIF
        SELECT CASE (ICT)
         CASE(100)
          EJ(1)=SKEW(1)
          EJ(2)=SKEW(2)
          EJ(3)=SKEW(3)
          CALL L_DIR(EJ,J)
          J1=0
          CALL DIR_RBE2(J    ,J1    ,K     )
C----------------010-------------------------
         CASE(10)
          EJ(1)=SKEW(4)
          EJ(2)=SKEW(5)
          EJ(3)=SKEW(6)
          CALL L_DIR(EJ,J)
          J1=0
          CALL DIR_RBE2(J    ,J1    ,K     )
C----------------001-------------------------
         CASE(1)
          EJ(1)=SKEW(7)
          EJ(2)=SKEW(8)
          EJ(3)=SKEW(9)
          CALL L_DIR(EJ,J)
          J1=0
          CALL DIR_RBE2(J    ,J1    ,K     )
C----------------011-------------------------
         CASE(11)
          EJ(1)=SKEW(7)
          EJ(2)=SKEW(8)
          EJ(3)=SKEW(9)
          CALL L_DIR(EJ,J)
          EJ1(1)=SKEW(4)
          EJ1(2)=SKEW(5)
          EJ1(3)=SKEW(6)
          CALL L_DIR(EJ1,J1)
          IF (J1.EQ.J) THEN
           EJ1(J)=ZERO
           CALL L_DIR(EJ1,J1)
           EJ1(1)=SKEW(4)/SKEW(3+J1)
           EJ1(2)=SKEW(5)/SKEW(3+J1)
           EJ1(3)=SKEW(6)/SKEW(3+J1)
          ENDIF
          CALL DIR_RBE2(J    ,J1    ,K     )
          S=ONE/(ONE-EJ(J1)*EJ1(J))
          EA=S*(EJ(J1)*EJ1(K)-EJ(K))
          EB=S*(EJ1(J)*EJ(K)-EJ1(K))
C----------------101-------------------------
         CASE(101)
          EJ(1)=SKEW(7)
          EJ(2)=SKEW(8)
          EJ(3)=SKEW(9)
          CALL L_DIR(EJ,J)
          EJ1(1)=SKEW(1)
          EJ1(2)=SKEW(2)
          EJ1(3)=SKEW(3)
          CALL L_DIR(EJ1,J1)
          IF (J1.EQ.J) THEN
           EJ1(J)=ZERO
           CALL L_DIR(EJ1,J1)
           EJ1(1)=SKEW(1)/SKEW(J1)
           EJ1(2)=SKEW(2)/SKEW(J1)
           EJ1(3)=SKEW(3)/SKEW(J1)
          ENDIF
          CALL DIR_RBE2(J    ,J1    ,K     )
          S=ONE/(ONE-EJ(J1)*EJ1(J))
          EA=S*(EJ(J1)*EJ1(K)-EJ(K))
          EB=S*(EJ1(J)*EJ(K)-EJ1(K))
C----------------110-------------------------
         CASE(110)
          EJ(1)=SKEW(4)
          EJ(2)=SKEW(5)
          EJ(3)=SKEW(6)
          CALL L_DIR(EJ,J)
          EJ1(1)=SKEW(1)
          EJ1(2)=SKEW(2)
          EJ1(3)=SKEW(3)
          CALL L_DIR(EJ1,J1)
          IF (J1.EQ.J) THEN
           EJ1(J)=ZERO
           CALL L_DIR(EJ1,J1)
           EJ1(1)=SKEW(1)/SKEW(J1)
           EJ1(2)=SKEW(2)/SKEW(J1)
           EJ1(3)=SKEW(3)/SKEW(J1)
          ENDIF
          CALL DIR_RBE2(J    ,J1    ,K     )
          S=ONE/(ONE-EJ(J1)*EJ1(J))
          EA=S*(EJ(J1)*EJ1(K)-EJ(K))
          EB=S*(EJ1(J)*EJ(K)-EJ1(K))
       END SELECT
C
       DO I=1,NSL
        NS = ISL(I)
        IF (NDOF(NS).EQ.0) CYCLE
         ND =IDDL(NS)+NR
C-------------------100---------------------
         IF (ICT .EQ. 100 ) THEN
          B(ND+J1)=B(ND+J1)-EJ(J1)*B(ND+J)
          B(ND+K)=B(ND+K)-EJ(K)*B(ND+J)
C-------------------010---------------------
         ELSEIF (ICT .EQ. 10) THEN
          B(ND+J1)=B(ND+J1)-EJ(J1)*B(ND+J)
          B(ND+K)=B(ND+K)-EJ(K)*B(ND+J)
C-------------------001---------------------
         ELSEIF (ICT .EQ. 1) THEN
          B(ND+J1)=B(ND+J1)-EJ(J1)*B(ND+J)
          B(ND+K)=B(ND+K)-EJ(K)*B(ND+J)
C-------------------011---------------------
         ELSEIF (ICT .EQ. 11) THEN
          B(ND+K)=B(ND+K)+EA*B(ND+J)+EB*B(ND+J1)
C-------------------101---------------------
         ELSEIF (ICT .EQ. 101) THEN
          B(ND+K)=B(ND+K)+EA*B(ND+J)+EB*B(ND+J1)
C-------------------110---------------------
         ELSEIF (ICT .EQ. 110 ) THEN
          B(ND+K)=B(ND+K)+EA*B(ND+J)+EB*B(ND+J1)
         ENDIF
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  BC_UPDK2                      source/constraints/general/rbe2/rbe2_imp0.F
Chd|-- called by -----------
Chd|        BC_UPDK2D                     source/constraints/general/bcs/bc_imp0.F
Chd|        RBE2_BCL                      source/constraints/general/rbe2/rbe2_imp0.F
Chd|-- calls ---------------
Chd|        GET_KII                       source/implicit/imp_glob_k.F  
Chd|        PUT_KII                       source/implicit/imp_glob_k.F  
Chd|====================================================================
      SUBROUTINE BC_UPDK2(N     ,IDDL  ,J    ,L     ,K     ,
     1                    IR    ,EJ    ,EL   ,IADK  ,JDIK  ,
     2                    DIAG_K,LT_K  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N,J ,L   ,K,IDDL(*),IR,IADK(*) ,JDIK(*)
      my_real
     .   EJ,EL,DIAG_K(*),LT_K(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ND,J1,K1,L1,ID,SHF,JFT,KFT,LFT,NL,NJ,
     .        IT(6),KK,IDJ,IDL,JJ,SHL
      my_real
     .   KDD(6,6),KII(6,6)
C------------K :free; J,L to be condentrated----------------------
      IF (IR.EQ.0) THEN
       ND = 3
      ELSE
       ND = 6
      ENDIF
      IF (EJ.EQ.ZERO.AND.EL.EQ.ZERO) RETURN
      DO I=1,ND
      DO JJ=1,ND
       KII(I,JJ)=ZERO
      ENDDO
      ENDDO
      IF (IR.EQ.0) THEN
       J1 = J
       K1 = K
       L1 = L
      ELSE
       J1 = J + 3
       K1 = K + 3
       L1 = L + 3
      ENDIF
C-------------first KNN------------
      CALL GET_KII(N ,IDDL ,IADK,DIAG_K,LT_K ,KDD,ND)
      DO I=1,ND
      DO JJ=I+1,ND
       KDD(JJ,I)=KDD(I,JJ)
      ENDDO
      ENDDO
      KII(K1,K1)=EJ*(KDD(J1,J1)*EJ+TWO*EL*KDD(J1,L1)-TWO*KDD(J1,K1))
     .           +EL*(KDD(L1,L1)*EL-TWO*KDD(L1,K1))
C +++ couplage t,r-----
      IF (IR.NE.0) THEN
       KII(K,K1)=-KDD(K,J1)*EJ-KDD(K,L1)*EL
       KII(J,K1)=-KDD(J,J1)*EJ-KDD(J,L1)*EL
       KII(L,K1)=-KDD(L,J1)*EJ-KDD(L,L1)*EL
      ENDIF
C ---
      CALL PUT_KII(N ,IDDL ,IADK,DIAG_K,LT_K ,KII,ND)
C  -----------KIJ-----
      IDJ = IDDL(N)+ J1
      IDL = IDDL(N)+ L1
      IF (IKPAT.EQ.0) THEN
        SHF = IABS(J-3)
        NJ = IADK(IDJ+1)-IADK(IDJ)-SHF
        JFT = IADK(IDJ)+SHF-1
        SHL = IABS(L-3)
        NL = IADK(IDL+1)-IADK(IDL)-SHL
        LFT = IADK(IDL)+SHL-1
        KFT = IADK(IDDL(N)+ K1)+IABS(K-3)-1
        DO JJ = 1, NJ
         LT_K(KFT+JJ) = LT_K(KFT+JJ)-EJ*LT_K(JFT+JJ)
        ENDDO
        DO JJ = 1, NL
         LT_K(KFT+JJ) = LT_K(KFT+JJ)-EL*LT_K(LFT+JJ)
        ENDDO
       DO I = 1, IDDL(N)
        NJ =0
        NL =0
        DO JJ = IADK(I), IADK(I+1)-1
         IF (JDIK(JJ).EQ.IDJ) NJ = JJ
         IF (JDIK(JJ).EQ.IDL) NL = JJ
        ENDDO
        IF (NJ.GT.0) LT_K(NJ+K1-J1) = LT_K(NJ+K1-J1)-EJ*LT_K(NJ)
        IF (NL.GT.0) LT_K(NJ+K1-J1) = LT_K(NJ+K1-J1)-EL*LT_K(NL)
       ENDDO
      ELSE
        SHF = J1-1
        SHL = L1-1
        NJ = IADK(IDJ+1)-IADK(IDJ)-SHF
        JFT = IADK(IDJ)-1
        NL = IADK(IDL+1)-IADK(IDL)-SHL
        LFT = IADK(IDL)-1
        KFT = IADK(IDDL(N)+K1)-1
        DO JJ = 1, NJ
         LT_K(KFT+JJ) = LT_K(KFT+JJ)-EJ*LT_K(JFT+JJ)
        ENDDO
        DO JJ = 1, NL
         LT_K(KFT+JJ) = LT_K(KFT+JJ)-EL*LT_K(LFT+JJ)
        ENDDO
C---------
       DO I = IDDL(N)+ND+1, NDDL_L
        NJ =0
        NL =0
        DO JJ = IADK(I), IADK(I+1)-1
         IF (JDIK(JJ).EQ.IDJ) NJ = JJ
         IF (JDIK(JJ).EQ.IDL) NL = JJ
        ENDDO
        IF (NJ.GT.0) LT_K(NJ+K1-J1) = LT_K(NJ+K1-J1)-EJ*LT_K(NJ)
        IF (NL.GT.0) LT_K(NJ+K1-J1) = LT_K(NJ+K1-J1)-EL*LT_K(NL)
       ENDDO
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  CDI_BCN1                      source/constraints/general/rbe2/rbe2_imp0.F
Chd|-- called by -----------
Chd|        CDI_SKEW                      source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE2DL2                       source/constraints/general/rbe2/rbe2v.F
Chd|        RBE2FL                        source/constraints/general/rbe2/rbe2f.F
Chd|        RBE2FRF                       source/constraints/general/rbe2/rbe2f.F
Chd|        RBE2VL1                       source/constraints/general/rbe2/rbe2v.F
Chd|        RBE2_FRD                      source/constraints/general/rbe2/rbe2v.F
Chd|-- calls ---------------
Chd|        DIR_RBE2                      source/constraints/general/rbe2/rbe2v.F
Chd|        L_DIR                         source/constraints/general/bcs/bc_imp0.F
Chd|====================================================================
      SUBROUTINE CDI_BCN1(XS,YS,ZS,JT,JR,SKEW,KTR,IRAD)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JT(3)   ,JR(3),IRAD
      my_real
     .   XS,YS,ZS,SKEW(*),KTR(3,3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,J1,L,ND,ICT
      my_real
     .   EJ(3),EJ1(3),S,XL,YL,ZL,VQ(3,3),KK(3,3),SA,SB
C-------JT1 presente the real consentration dof----------------------
        DO I=1,3
         VQ(1,I)= SKEW(I)
         VQ(2,I)= SKEW(I+3)
         VQ(3,I)= SKEW(I+6)
         KK(I,I)=ZERO
        ENDDO
        XL = VQ(1,1)*XS+ VQ(1,2)*YS+VQ(1,3)*ZS
        YL = VQ(2,1)*XS+ VQ(2,2)*YS+VQ(2,3)*ZS
        ZL = VQ(3,1)*XS+ VQ(3,2)*YS+VQ(3,3)*ZS
       IF (IRAD.GT.0) THEN
        KK(1,2) =ZL*JR(2)
        KK(1,3) =-YL*JR(3)
        KK(2,1) =-ZL*JR(1)
        KK(2,3) =XL*JR(3)
        KK(3,1) =YL*JR(1)
        KK(3,2) =-XL*JR(2)
        ICT = JR(1)*100+JR(2)*10+JR(3)
C--------Nastran's formulation
       ELSE
        KK(1,2) =ZL*JT(1)
        KK(1,3) =-YL*JT(1)
        KK(2,1) =-ZL*JT(2)
        KK(2,3) =XL*JT(2)
        KK(3,1) =YL*JT(3)
        KK(3,2) =-XL*JT(3)
        ICT = JT(1)*100+JT(2)*10+JT(3)
       END IF !(IR.GT.0) THEN
C--------[Q]^t[Rs][Q]--------
        DO J=1,3
          KTR(1,J)=KK(1,2)*VQ(2,J)+KK(1,3)*VQ(3,J)
          KTR(2,J)=KK(2,1)*VQ(1,J)+KK(2,3)*VQ(3,J)
          KTR(3,J)=KK(3,1)*VQ(1,J)+KK(3,2)*VQ(2,J)
        ENDDO
C
          DO I=1,3
          DO J=1,3
           KK(I,J)=KTR(I,J)
           KTR(I,J)=ZERO
          ENDDO
          ENDDO
C----------------100-------------------------
        SELECT CASE (ICT)
         CASE(100)
          EJ(1)=SKEW(1)
          EJ(2)=SKEW(2)
          EJ(3)=SKEW(3)
          CALL L_DIR(EJ,J)
          DO I=1,3
           KTR(J,I)=KK(1,I)/EJ(J)
          ENDDO
C----------------010-------------------------
         CASE(10)
          EJ(1)=SKEW(4)
          EJ(2)=SKEW(5)
          EJ(3)=SKEW(6)
          CALL L_DIR(EJ,J)
          DO I=1,3
           KTR(J,I)=KK(2,I)/EJ(J)
          ENDDO
C----------------001-------------------------
         CASE(1)
          EJ(1)=SKEW(7)
          EJ(2)=SKEW(8)
          EJ(3)=SKEW(9)
          CALL L_DIR(EJ,J)
          DO I=1,3
           KTR(J,I)=KK(3,I)/EJ(J)
          ENDDO
C----------------011-------------------------
         CASE(11)
          EJ(1)=SKEW(7)
          EJ(2)=SKEW(8)
          EJ(3)=SKEW(9)
          CALL L_DIR(EJ,J)
          EJ1(1)=SKEW(4)
          EJ1(2)=SKEW(5)
          EJ1(3)=SKEW(6)
          CALL L_DIR(EJ1,J1)
          IF (J1.EQ.J) THEN
           EJ1(J)=ZERO
           CALL L_DIR(EJ1,J1)
           EJ1(1)=SKEW(4)/SKEW(3+J1)
           EJ1(2)=SKEW(5)/SKEW(3+J1)
           EJ1(3)=SKEW(6)/SKEW(3+J1)
          ENDIF
          CALL DIR_RBE2(J    ,J1    ,K     )
          S=ONE/(ONE-EJ(J1)*EJ1(J))
          SA = S/SKEW(6+J)
          SB = S/SKEW(3+J1)
          DO I=1,3
           KTR(J,I)=SA*KK(3,I)-SB*EJ(J1)*KK(2,I)
           KTR(J1,I)=SB*KK(2,I)-SA*EJ1(J)*KK(3,I)
          ENDDO
C----------------101-------------------------
         CASE(101)
          EJ(1)=SKEW(7)
          EJ(2)=SKEW(8)
          EJ(3)=SKEW(9)
          CALL L_DIR(EJ,J)
          EJ1(1)=SKEW(1)
          EJ1(2)=SKEW(2)
          EJ1(3)=SKEW(3)
          CALL L_DIR(EJ1,J1)
          IF (J1.EQ.J) THEN
           EJ1(J)=ZERO
           CALL L_DIR(EJ1,J1)
           EJ1(1)=SKEW(1)/SKEW(J1)
           EJ1(2)=SKEW(2)/SKEW(J1)
           EJ1(3)=SKEW(3)/SKEW(J1)
          ENDIF
          CALL DIR_RBE2(J    ,J1    ,K     )
          S=ONE/(ONE-EJ(J1)*EJ1(J))
          SA = S/SKEW(6+J)
          SB = S/SKEW(J1)
          DO I=1,3
           KTR(J,I)=SA*KK(3,I)-SB*EJ(J1)*KK(1,I)
           KTR(J1,I)=SB*KK(1,I)-SA*EJ1(J)*KK(3,I)
          ENDDO
C----------------110-------------------------
         CASE(110)
          EJ(1)=SKEW(4)
          EJ(2)=SKEW(5)
          EJ(3)=SKEW(6)
          CALL L_DIR(EJ,J)
          EJ1(1)=SKEW(1)
          EJ1(2)=SKEW(2)
          EJ1(3)=SKEW(3)
          CALL L_DIR(EJ1,J1)
          IF (J1.EQ.J) THEN
           EJ1(J)=ZERO
           CALL L_DIR(EJ1,J1)
           EJ1(1)=SKEW(1)/SKEW(J1)
           EJ1(2)=SKEW(2)/SKEW(J1)
           EJ1(3)=SKEW(3)/SKEW(J1)
          ENDIF
          CALL DIR_RBE2(J    ,J1    ,K     )
          S=ONE/(ONE-EJ(J1)*EJ1(J))
          SA = S/SKEW(3+J)
          SB = S/SKEW(J1)
          DO I=1,3
           KTR(J,I)=SA*KK(2,I)-SB*EJ(J1)*KK(1,I)
           KTR(J1,I)=SB*KK(1,I)-SA*EJ1(J)*KK(2,I)
          ENDDO
C----------------111-------------------------
         CASE(111)
          DO I=1,3
          DO J=1,3
           KTR(I,J)=KK(I,J)
          ENDDO
          ENDDO
       END SELECT
C
      RETURN
      END
Chd|====================================================================
Chd|  RBE2_IMPKD                    source/constraints/general/rbe2/rbe2_imp0.F
Chd|-- called by -----------
Chd|        DIAG_INT                      source/mpi/implicit/imp_fri.F 
Chd|        UPDK_MV                       source/airbag/monv_imp0.F     
Chd|-- calls ---------------
Chd|        BCL_IMPKD                     source/constraints/general/bcs/bc_imp0.F
Chd|        CDI_SKEW                      source/constraints/general/rbe2/rbe2_imp0.F
Chd|        UPDCDIK2_CDI                  source/constraints/general/rbe2/rbe2_imp0.F
Chd|====================================================================
      SUBROUTINE RBE2_IMPKD(M     ,NS    ,X     ,ISK    ,JT     ,
     2                     JR     ,NDOF  ,SKEW0 ,KDD    ,DIAG_KM,
     3                     DIAG_KN,IRAD  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER M, NS,JT(3),JR(3),IRAD,NDOF(*),ISK
C     REAL ou REAL*8
      my_real
     .   X(3,*),DIAG_KM(6),DIAG_KN(6),KDD(6,6),SKEW0(9)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIR, I, J, J1, J2, J3, J4, K, JD, II, L, JJ,
     .        I1,ID,NL,NI,NJ,NIDOF,ND,NDI,NDJ,NDM,NM,L1,NM1,
     .        NIR1,IR,IP,ISTIF,IMD,N,NT,NR,IC,JT1(3),JR1(3)
C     REAL ou REAL*8
      my_real
     .   XS,YS,ZS,TMP,CDT(3,3),CDTR(3,3),CDR(3,3),SKEW(LSKEW),
     .   KII(6,6)
C------------------------------------
C     VITESSES DES NOEUDS SECONDS
C------------------------------------
        ND = NDOF(M)
        NT=JT(1)+JT(2)+JT(3)
        NR=JR(1)+JR(2)+JR(3)
        IF (ISK.GT.1) THEN
         DO K=1,LSKEW
          SKEW(K)=SKEW0(K)
         ENDDO
        ELSE
         DO K=1,LSKEW
          SKEW(K)=ZERO
         ENDDO
          SKEW(1)=ONE
          SKEW(5)=ONE
          SKEW(9)=ONE
        ENDIF
C
        DO K=1,6
         DO J=1,6
           KII(K,J)=KDD(K,J)
         ENDDO
        ENDDO
C--------block diagonal Kmm--
        N = NS
        IF (NDOF(N).GT.0) THEN
         XS=X(1,N)-X(1,M)
         YS=X(2,N)-X(2,M)
         ZS=X(3,N)-X(3,M)
         CALL CDI_SKEW(XS,YS,ZS,JT,JR,SKEW,CDT,CDR,CDTR,JT1,JR1,IRAD)
C-------Update K,
         CALL UPDCDIK2_CDI(ND,CDT,CDR,CDTR,KII)
         DO K=1,6
          DIAG_KM(K)=DIAG_KM(K)+KII(K,K)
         ENDDO
          IF (NT.GT.0.AND.NT.LT.3) THEN
            IC = JT(1)*4+JT(2)*2+JT(3)
            CALL BCL_IMPKD(IC  ,I1   ,SKEW  ,KII   ,DIAG_KN )
          ENDIF
          IF (NR.GT.0.AND.NR.LT.3) THEN
           IC = JR(1)*4+JR(2)*2+JR(3)
           DO K=1,3
           DO J=1,3
            KII(K,J)=KDD(K+3,J+3)
           ENDDO
           ENDDO
           CALL BCL_IMPKD(IC  ,I1   ,SKEW  ,KII   ,DIAG_KN(4))
          ENDIF
        ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  RBE2_FRK                      source/constraints/general/rbe2/rbe2_imp0.F
Chd|-- called by -----------
Chd|        UPD_KML                       source/mpi/implicit/imp_fri.F 
Chd|        UPD_KSL                       source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        CDI_SKEW                      source/constraints/general/rbe2/rbe2_imp0.F
Chd|        PUT_KMII                      source/implicit/imp_glob_k.F  
Chd|        UPDCDIK2_CDI                  source/constraints/general/rbe2/rbe2_imp0.F
Chd|        UPDK2_CDI                     source/constraints/general/rbe2/rbe2_imp0.F
Chd|====================================================================
      SUBROUTINE RBE2_FRK(NS    ,M    ,X     ,ISK  ,SKEW0 ,
     1                    IRAD  ,NDOF  ,IDDL ,JT   ,JR    ,
     2                    IADK  ,JDIK ,DIAG_K,LT_K ,B     ,
     3                    A     ,KSS  ,KSM  ,KNM   ,KRM   ,
     4                    IDLM ,ISS,ISM )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IADK(*),JDIK(*),NDOF(*),IDDL(*),IRAD,
     .        M, NS,IDLM ,ISS,ISM,ISK,JT(3),JR(3)
C     REAL ou REAL*8
      my_real
     .   X(3,*),DIAG_K(*),LT_K(*),B(*),A(3,*),
     .   KSS(6),KSM(3,3),KNM(3,3),KRM(3,3),SKEW0(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, J1, J2, J3, J4, K, JD, II, L, JJ,
     .        ND,NDI,NDJ,NDM,NM,L1,NM1,
     .        IMD,N,NT,NR,IC,JT1(3),JR1(3),NDOFI
C     REAL ou REAL*8
      my_real
     .   XS,YS,ZS,CDT(3,3),CDTR(3,3),CDR(3,3),SKEW(LSKEW),
     .   KDD(6,6)
C-------------------B is not modified-----------------
        ND = NDOF(M)
        NDOFI = 3
        IF (ISK.GT.1) THEN
         DO K=1,LSKEW
          SKEW(K)=SKEW0(K)
         ENDDO
        ELSE
         DO K=1,LSKEW
          SKEW(K)=ZERO
         ENDDO
          SKEW(1)=ONE
          SKEW(5)=ONE
          SKEW(9)=ONE
        ENDIF
        N = NS
C
        DO K=1,6
         DO J=1,6
           KDD(K,J)=ZERO
         ENDDO
        ENDDO
       IF (ISS.GT.0) THEN
         DO K=1,NDOFI
           KDD(K,K) = KSS(K)
         ENDDO
         KDD(1,2) = KSS(4)
         KDD(1,3) = KSS(5)
         KDD(2,3) = KSS(6)
         XS=X(1,N)-X(1,M)
         YS=X(2,N)-X(2,M)
         ZS=X(3,N)-X(3,M)
         CALL CDI_SKEW(XS,YS,ZS,JT,JR,SKEW,CDT,CDR,CDTR,JT1,JR1,IRAD)
C-------Update K,
         CALL UPDCDIK2_CDI(ND,CDT,CDR,CDTR,KDD)
         CALL PUT_KMII(IDLM,IADK,DIAG_K,LT_K,KDD,ND)
       ENDIF
       IF (ISM.GT.0) THEN
C--------no diag--Kjm=sum(KjsCsm)--
          DO K=1,NDOFI
          DO J=1,NDOFI
           KDD(K,J) = KSM(K,J)
          ENDDO
          ENDDO
         XS=X(1,N)-X(1,M)
         YS=X(2,N)-X(2,M)
         ZS=X(3,N)-X(3,M)
         CALL CDI_SKEW(XS,YS,ZS,JT,JR,SKEW,CDT,CDR,CDTR,JT1,JR1,IRAD)
C------- Update ---
          CALL UPDK2_CDI(NDOFI,NDOFI,CDT,CDR,CDTR,KDD)
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

