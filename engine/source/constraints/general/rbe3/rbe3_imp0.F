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
Chd|  RBE3_IMP0                     source/constraints/general/rbe3/rbe3_imp0.F
Chd|-- called by -----------
Chd|        UPD_GLOB_K                    source/implicit/upd_glob_k.F  
Chd|-- calls ---------------
Chd|        PRERBE3                       source/constraints/general/rbe3/rbe3f.F
Chd|        RBE3CL                        source/constraints/general/rbe3/rbe3f.F
Chd|        RBE3_IMP1                     source/constraints/general/rbe3/rbe3_imp0.F
Chd|====================================================================
      SUBROUTINE RBE3_IMP0(
     1                    IRBE3  ,LRBE3 ,FRBE3  ,X     ,SKEW   ,
     2                    ISS3   ,IKC   ,NDOF   ,IDDL  ,IADK   ,
     3                    JDIK  ,DIAG_K ,LT_K   ,B     ,WEIGHT ,
     4                    ITAB  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER WEIGHT(*),IRBE3(NRBE3L,*),LRBE3(*),
     .        IADK(*),JDIK(*),NDOF(*),ITAB(*),
     .        IDDL(*),IKC(*),ISS3(*)
C     REAL
      my_real
     .   X(3,*), SKEW(LSKEW,*), FRBE3(*),
     .   DIAG_K(*),LT_K(*),B(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, M, NS ,NML, IAD,JJ,IROT,IADS,MAX_M,IROTG,
     .        JT(3,NRBE3),JR(3,NRBE3),NM,NN,NSJ,IADJ
C     REAL
      my_real,
     .         DIMENSION(:),ALLOCATABLE :: FDSTNB ,MDSTNB

C-----------------------------------------------
      IADS = SLRBE3/2
      CALL PRERBE3(IRBE3 ,MAX_M , IROTG,JT  ,JR   )
      ALLOCATE(FDSTNB(18*MAX_M))
      IF (IROTG>0) ALLOCATE(MDSTNB(18*MAX_M))
      IADJ=1
      DO N=1,NRBE3
        IAD = IRBE3(1,N)
        NS  = IRBE3(3,N)
        NML = IRBE3(5,N)
          IROT =IRBE3(6,N)
          NSJ  =IRBE3(8,N)
       IF (NS==0.OR.NDOF(NS)==0) CYCLE
       IF (WEIGHT(NS)/=0) THEN
        CALL RBE3CL(LRBE3(IAD+1),LRBE3(IADS+IAD+1),NS     ,X    ,
     .              FRBE3(6*IAD+1),SKEW    ,NML     ,IROT   ,FDSTNB ,
     .              MDSTNB  ,IRBE3(2,N))
            CALL RBE3_IMP1(NS    ,NML   ,LRBE3(IAD+1) ,X      ,IROT   ,
     2                     NSJ   ,ISS3(IADJ),JT(1,N)  ,JR(1,N),FDSTNB ,
     3                     MDSTNB,IKC   ,NDOF  ,IDDL   ,IADK   ,
     4                     JDIK  ,DIAG_K,LT_K  ,B     ,ITAB   )
       END IF
       IADJ=IADJ+NSJ
      ENDDO
C
      DEALLOCATE(FDSTNB)
      IF (IROTG>0) DEALLOCATE(MDSTNB)
C
      RETURN
      END
Chd|====================================================================
Chd|  RBE3_IMPI                     source/constraints/general/rbe3/rbe3_imp0.F
Chd|-- called by -----------
Chd|        UPD_INT_K                     source/implicit/upd_glob_k.F  
Chd|-- calls ---------------
Chd|        PRERBE3                       source/constraints/general/rbe3/rbe3f.F
Chd|        RBE3CL                        source/constraints/general/rbe3/rbe3f.F
Chd|        RBE3_IMP1                     source/constraints/general/rbe3/rbe3_imp0.F
Chd|====================================================================
      SUBROUTINE RBE3_IMPI(
     1                    IRBE3  ,LRBE3 ,FRBE3  ,X     ,SKEW   ,
     2                    NSS3   ,ISS3  ,IKC   ,NDOF   ,IDDL   ,
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
#include      "param_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER WEIGHT(*),IRBE3(NRBE3L,*),LRBE3(*),
     .        IADK(*),JDIK(*),NDOF(*),ITAB(*),
     .        IDDL(*),IKC(*),NSS3(*),ISS3(*)
C     REAL
      my_real
     .   X(3,*), SKEW(LSKEW,*), FRBE3(*),
     .   DIAG_K(*),LT_K(*),B(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, M, NS ,NML, IAD,JJ,IROT,IADS,MAX_M,IROTG,
     .        JT(3,NRBE3),JR(3,NRBE3),NM,NN,NSJ,IADJ
C     REAL
      my_real,
     .         DIMENSION(:),ALLOCATABLE :: FDSTNB ,MDSTNB

C-----------------------------------------------
      IADS = SLRBE3/2
      CALL PRERBE3(IRBE3 ,MAX_M , IROTG,JT  ,JR   )
      ALLOCATE(FDSTNB(18*MAX_M))
      IF (IROTG>0) ALLOCATE(MDSTNB(18*MAX_M))
      IADJ=1
      DO N=1,NRBE3
        IAD = IRBE3(1,N)
        NS  = IRBE3(3,N)
        IF (NS==0) CYCLE
        NML = IRBE3(5,N)
          IROT =IRBE3(6,N)
          DO J =1,3
           JR(J,N)=0
          ENDDO
       IF (WEIGHT(NS)/=0.AND.NDOF(NS)>0) THEN
        CALL RBE3CL(LRBE3(IAD+1),LRBE3(IADS+IAD+1),NS     ,X    ,
     .              FRBE3(6*IAD+1),SKEW    ,NML     ,IROT   ,FDSTNB ,
     .              MDSTNB  ,IRBE3(2,N))
            CALL RBE3_IMP1(NS    ,NML   ,LRBE3(IAD+1) ,X      ,IROT   ,
     2                     NSS3(N),ISS3(IADJ),JT(1,N) ,JR(1,N),FDSTNB ,
     3                     MDSTNB,IKC   ,NDOF  ,IDDL  ,IADK   ,
     4                     JDIK  ,DIAG_K,LT_K  ,B     ,ITAB   )
        IADJ=IADJ+NSS3(N)
       END IF
      ENDDO
C
      DEALLOCATE(FDSTNB)
      IF (IROTG>0) DEALLOCATE(MDSTNB)
C
      RETURN
      END
Chd|====================================================================
Chd|  RBE3_IMP1                     source/constraints/general/rbe3/rbe3_imp0.F
Chd|-- called by -----------
Chd|        RBE3_IMP0                     source/constraints/general/rbe3/rbe3_imp0.F
Chd|        RBE3_IMPI                     source/constraints/general/rbe3/rbe3_imp0.F
Chd|-- calls ---------------
Chd|        GET_KII                       source/implicit/imp_glob_k.F  
Chd|        GET_KIJ                       source/implicit/imp_glob_k.F  
Chd|        PRINT_WKIJ                    source/implicit/imp_glob_k.F  
Chd|        PUT_KII                       source/implicit/imp_glob_k.F  
Chd|        PUT_KIJ                       source/implicit/imp_glob_k.F  
Chd|        UPDB_CDI                      source/constraints/general/rbe3/rbe3_imp0.F
Chd|        UPDK_CDI                      source/constraints/general/rbe3/rbe3_imp0.F
Chd|        UPDK_CDII                     source/constraints/general/rbe3/rbe3_imp0.F
Chd|        UPDK_CDIJ                     source/constraints/general/rbe3/rbe3_imp0.F
Chd|====================================================================
      SUBROUTINE RBE3_IMP1(NS    ,NML   ,IML   ,X      ,IROT   ,
     2                     NSJ   ,ISJ   ,JT    ,JR     ,FDSTNB ,
     3                     MDSTNB,IKC   ,NDOF  ,IDDL   ,IADK   ,
     4                     JDIK  ,DIAG_K,LT_K  ,B      ,ITAB   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NS, NML,IML(*),NSJ,ISJ(*) ,JT(3),JR(3),IROT
      INTEGER IADK(*),JDIK(*),NDOF(*),IDDL(*),IKC(*),ITAB(*)
      my_real
     .   X(3,*),DIAG_K(*),LT_K(*),B(*),FDSTNB(18,*),MDSTNB(18,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIR, I, J, J1, J2, J3, J4, K, JD, II, L, JJ,
     .        I1,ID,NL,NI,NJ,NIDOF,ND,NDI,NDJ,NDM,NM,L1,NM1,
     .        NIR1,IR,IP,ISTIF
C     REAL
      my_real
     .   KDD(6,6),BD(6),KII(6,6),KIJ(6,6),BI(6)
C------------------------------------
C     VITESSES DES NOEUDS SECONDS
C------------------------------------
       IF (NDOF(NS)<=0) RETURN
C
       IP=4
       I = NS
       NDM = NDOF(NS)
        DO K=1,6
        DO J=K,6
         KDD(K,J)=ZERO
        ENDDO
        ENDDO
        DO K=1,NDOF(I)
         ID = IDDL(I)+K
         IKC(ID)=13
         BD(K)=B(ID)
        ENDDO
        DO K=NDOF(I)+1,6
         BD(K)=ZERO
        ENDDO
        DO K=1,3
         BD(K)=BD(K)*JT(K)
         BD(K+3)=BD(K+3)*JR(K)
        ENDDO
        CALL GET_KII(I ,IDDL ,IADK,DIAG_K,LT_K ,KDD,NDOF(I))
        DO K=1,6
        DO J=K,6
         KDD(J,K)=KDD(K,J)
        ENDDO
        ENDDO
C        CALL UPDK_BC(JT,JR,KDD,ISTIF)
C
C-------Update K(main node)---
C  IF (ISTIF>0) THEN
         DO J=1,NML
          NJ=IML(J)
          ND = NDOF(NJ)
C-------Update CDI^t[KDD]CDI---
          CALL UPDK_CDII(FDSTNB(1,J),MDSTNB(1,J),KDD,KII,IROT,NDM)
          CALL PUT_KII(NJ,IDDL ,IADK,DIAG_K,LT_K ,KII,ND)
          DO I1=J+1,NML
           NM=IML(I1)
C-------Update CDI^t[KDD]CDJ---
           CALL UPDK_CDIJ(FDSTNB(1,J),MDSTNB(1,J),FDSTNB(1,I1),
     .                    MDSTNB(1,I1),KDD,KIJ,IROT,NDM )
           CALL PUT_KIJ(NJ,NM,IDDL,IADK,JDIK,LT_K,KIJ,ND,NDOF(NM),IR)
           IF (IR==1) CALL PRINT_WKIJ(ITAB(NJ) ,ITAB(NM) ,IP )
          ENDDO
         ENDDO
C  END IF
        DO J=1,NML
          NJ=IML(J)
          ND = NDOF(NJ)
          CALL UPDB_CDI(FDSTNB(1,J),MDSTNB(1,J),BD,BI,IROT)
          DO K=1,ND
           ID = IDDL(NJ)+K
           B(ID) = B(ID) + BI(K)
          ENDDO
        ENDDO
C--------no diag--Kjm=sum(KjsCsm)--
        DO I1 = 1,NSJ
          NI=ISJ(I1)
          NIDOF=NDOF(NI)
          NDM = MAX(NDM,NIDOF)
          DO K=1,6
          DO J=1,6
           KDD(K,J)=ZERO
          ENDDO
          ENDDO
          CALL GET_KIJ(NI,I,IDDL,IADK,JDIK,LT_K,KDD,NIDOF,NDOF(I),IR)
          IF (IR==1) CALL PRINT_WKIJ(ITAB(NI) ,ITAB(I) ,IP )
C------- Update ---
C          CALL UPDK_BC2(JT,JR,KDD,ISTIF)
C    IF (ISTIF>0) THEN
           DO J=1,NML
            NJ=IML(J)
            NDJ = NDOF(NJ)
            IF (NI==NJ) THEN
             CALL UPDK_CDI(FDSTNB(1,J),MDSTNB(1,J),KDD,KII,IROT,NDM,1)
             CALL PUT_KII(NJ,IDDL ,IADK,DIAG_K,LT_K ,KII,NDJ)
              ELSE
             CALL UPDK_CDI(FDSTNB(1,J),MDSTNB(1,J),KDD,KII,IROT,NDM,0)
             CALL PUT_KIJ(NI,NJ,IDDL,IADK,JDIK,LT_K,KII,NIDOF,NDJ,IR)
             IF (IR==1) CALL PRINT_WKIJ(ITAB(NI) ,ITAB(NJ) ,IP )
            ENDIF
          ENDDO
C          ENDIF
        ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  RBE3_IMPR1                    source/constraints/general/rbe3/rbe3_imp0.F
Chd|-- called by -----------
Chd|        IMP_DYKV                      source/implicit/imp_dyna.F    
Chd|        IMP_DYKV0                     source/implicit/imp_dyna.F    
Chd|        UPD_RHS                       source/implicit/upd_glob_k.F  
Chd|        UPD_RHS_FR                    source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        PRERBE3                       source/constraints/general/rbe3/rbe3f.F
Chd|        RBE3CL                        source/constraints/general/rbe3/rbe3f.F
Chd|        RBE3_IMPB0                    source/constraints/general/rbe3/rbe3_imp0.F
Chd|====================================================================
      SUBROUTINE RBE3_IMPR1(
     1                    IRBE3  ,LRBE3 ,FRBE3  ,X     ,SKEW   ,
     2                    NDOF   ,IDDL  ,B      ,WEIGHT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER WEIGHT(*),IRBE3(NRBE3L,*),LRBE3(*),
     .        NDOF(*),IDDL(*)
C     REAL
      my_real
     .   X(3,*), SKEW(LSKEW,*), FRBE3(*),B(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, M, NS ,NML, IAD,JJ,IROT,IADS,MAX_M,IROTG,
     .        JT(3,NRBE3),JR(3,NRBE3),NM,NN,NSJ
C     REAL
      my_real,
     .         DIMENSION(:),ALLOCATABLE :: FDSTNB ,MDSTNB

C-----------------------------------------------
      IADS = SLRBE3/2
      CALL PRERBE3(IRBE3 ,MAX_M , IROTG,JT  ,JR   )
      ALLOCATE(FDSTNB(18*MAX_M))
      IF (IROTG>0) ALLOCATE(MDSTNB(18*MAX_M))
      DO N=1,NRBE3
        IAD = IRBE3(1,N)
        NS  = IRBE3(3,N)
        IF (NS==0) CYCLE
        NML = IRBE3(5,N)
          IROT =IRBE3(6,N)
       IF (WEIGHT(NS)/=0) THEN
        CALL RBE3CL(LRBE3(IAD+1),LRBE3(IADS+IAD+1),NS     ,X    ,
     .              FRBE3(6*IAD+1),SKEW    ,NML     ,IROT   ,FDSTNB ,
     .              MDSTNB ,IRBE3(2,N) )
            CALL RBE3_IMPB0(NS    ,NML   ,LRBE3(IAD+1),X      ,IROT   ,
     2                      JT(1,N),JR(1,N),FDSTNB   ,MDSTNB  ,NDOF   ,
     4                      IDDL  ,B     )
       END IF
      ENDDO
C
      DEALLOCATE(FDSTNB)
      IF (IROTG>0) DEALLOCATE(MDSTNB)
C
      RETURN
      END
Chd|====================================================================
Chd|  RBE3_IMPB0                    source/constraints/general/rbe3/rbe3_imp0.F
Chd|-- called by -----------
Chd|        RBE3_IMPR1                    source/constraints/general/rbe3/rbe3_imp0.F
Chd|-- calls ---------------
Chd|        UPDB_CDI                      source/constraints/general/rbe3/rbe3_imp0.F
Chd|====================================================================
      SUBROUTINE RBE3_IMPB0(NS    ,NML   ,IML   ,X      ,IROT   ,
     2                      JT    ,JR     ,FDSTNB ,MDSTNB,NDOF  ,
     3                      IDDL   ,B      )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NS, NML,IML(*),JT(3),JR(3),NDOF(*),IDDL(*),IROT
      my_real
     .   X(3,*),B(*),FDSTNB(18,*),MDSTNB(18,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIR, I, J, J1, J2, J3, J4, K, JD, II, L, JJ,
     .        I1,ID,NL,NI,NJ,NIDOF,ND,NDI,NDJ,NDM,NM,L1,NM1,
     .        NIR1,IR,IP
C     REAL
      my_real
     .   BD(6),BI(6)
C------------------------------------
C     VITESSES DES NOEUDS SECONDS
C------------------------------------
       IF (NDOF(NS)<=0) RETURN
C
       I = NS
        DO K=1,NDOF(I)
         ID = IDDL(I)+K
         BD(K)=B(ID)
        ENDDO
        DO K=NDOF(I)+1,6
         BD(K)=ZERO
        ENDDO
        DO K=1,3
         BD(K)=BD(K)*JT(K)
         BD(K+3)=BD(K+3)*JR(K)
        ENDDO
C-------Update K(main node)---
        DO J=1,NML
          NJ=IML(J)
          CALL UPDB_CDI(FDSTNB(1,J),MDSTNB(1,J),BD,BI,IROT)
          DO K=1,NDOF(NJ)
           ID = IDDL(NJ)+K
           B(ID) = B(ID) + BI(K)
          ENDDO
        ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  RBE3_IMPR2                    source/constraints/general/rbe3/rbe3_imp0.F
Chd|-- called by -----------
Chd|        IMP_DYKV                      source/implicit/imp_dyna.F    
Chd|        IMP_DYKV0                     source/implicit/imp_dyna.F    
Chd|        UPD_RHS                       source/implicit/upd_glob_k.F  
Chd|        UPD_RHS_FR                    source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        PRERBE3                       source/constraints/general/rbe3/rbe3f.F
Chd|        RBE3CL                        source/constraints/general/rbe3/rbe3f.F
Chd|        RBE3_IMPB2                    source/constraints/general/rbe3/rbe3_imp0.F
Chd|====================================================================
      SUBROUTINE RBE3_IMPR2(
     1                    IRBE3  ,LRBE3 ,FRBE3  ,X     ,SKEW   ,
     2                    NDOF   ,IDDL  ,B      ,WEIGHT,A      ,
     3                    AR     )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER WEIGHT(*),IRBE3(NRBE3L,*),LRBE3(*),
     .        NDOF(*),IDDL(*)
C     REAL
      my_real
     .   X(3,*), SKEW(LSKEW,*), FRBE3(*),B(*),A(*),AR(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, M, NS ,NML, IAD,JJ,IROT,IADS,MAX_M,IROTG,
     .        JT(3,NRBE3),JR(3,NRBE3),NM,NN,NSJ
C     REAL
      my_real,
     .         DIMENSION(:),ALLOCATABLE :: FDSTNB ,MDSTNB

C-----------------------------------------------
      IADS = SLRBE3/2
      CALL PRERBE3(IRBE3 ,MAX_M , IROTG,JT  ,JR   )
      ALLOCATE(FDSTNB(18*MAX_M))
      IF (IROTG>0) ALLOCATE(MDSTNB(18*MAX_M))
      DO N=1,NRBE3
        IAD = IRBE3(1,N)
        NS  = IRBE3(3,N)
        IF (NS==0) CYCLE
        NML = IRBE3(5,N)
          IROT =IRBE3(6,N)
       IF (WEIGHT(NS)/=0) THEN
        CALL RBE3CL(LRBE3(IAD+1),LRBE3(IADS+IAD+1),NS     ,X    ,
     .              FRBE3(6*IAD+1),SKEW    ,NML     ,IROT   ,FDSTNB ,
     .              MDSTNB  ,IRBE3(2,N))
            CALL RBE3_IMPB2(NS    ,NML   ,LRBE3(IAD+1),X      ,IROT   ,
     2                      JT(1,N),JR(1,N),FDSTNB   ,MDSTNB  ,NDOF   ,
     4                      IDDL  ,B     ,A      ,AR      )
       END IF
      ENDDO
C
      DEALLOCATE(FDSTNB)
      IF (IROTG>0) DEALLOCATE(MDSTNB)
C
      RETURN
      END
Chd|====================================================================
Chd|  RBE3_IMPB2                    source/constraints/general/rbe3/rbe3_imp0.F
Chd|-- called by -----------
Chd|        RBE3_IMPR2                    source/constraints/general/rbe3/rbe3_imp0.F
Chd|-- calls ---------------
Chd|        UPDB_CDI                      source/constraints/general/rbe3/rbe3_imp0.F
Chd|====================================================================
      SUBROUTINE RBE3_IMPB2(NS    ,NML   ,IML   ,X      ,IROT   ,
     2                      JT    ,JR     ,FDSTNB ,MDSTNB,NDOF  ,
     3                      IDDL   ,B     ,A    ,AR      )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NS, NML,IML(*),JT(3),JR(3),NDOF(*),IDDL(*),IROT
C     REAL
      my_real
     .   X(3,*),B(*),FDSTNB(18,*),MDSTNB(18,*),A(3,*),AR(3,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIR, I, J, J1, J2, J3, J4, K, JD, II, L, JJ,
     .        I1,ID,NL,NI,NJ,NIDOF,ND,NDI,NDJ,NDM,NM,L1,NM1,
     .        NIR1,IR,IP
C     REAL
      my_real
     .   BD(6),BI(6)
C------------------------------------
C     VITESSES DES NOEUDS SECONDS
C------------------------------------
C
       I = NS
      IF (IRODDL/=0) THEN
       ND = 6
      ELSE
       ND = 3
      ENDIF
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
        DO K=1,3
         BD(K)=BD(K)*JT(K) 
         BD(K+3)=BD(K+3)*JR(K) 
        ENDDO 
C-------Update K(main node)---
        DO J=1,NML
          NJ=IML(J)
         IF (NDOF(NJ)> 0) THEN
          CALL UPDB_CDI(FDSTNB(1,J),MDSTNB(1,J),BD,BI,IROT)
            DO K=1,3
             ID = IDDL(NJ)+K
             B(ID)=BI(K) 
            ENDDO 
           IF (IROT>0) THEN
            DO K=4,6
             ID = IDDL(NJ)+K
             B(ID)=BI(K) 
            ENDDO 
           ENDIF
         END IF 
        ENDDO 
C
       ELSE
        DO K=1,NDOF(I)
         ID = IDDL(I)+K
         BD(K)=B(ID)
        ENDDO
        DO K=NDOF(I)+1,6
         BD(K)=ZERO
        ENDDO
       ENDIF
C
       DO K=1,3
         BD(K)=BD(K)*JT(K)
         BD(K+3)=BD(K+3)*JR(K)
       ENDDO
C-------Update K(main node)---
        DO J=1,NML
          NJ=IML(J)
         IF (NDOF(NJ)==0) THEN
          CALL UPDB_CDI(FDSTNB(1,J),MDSTNB(1,J),BD,BI,IROT)
            DO K=1,3
             A(K,NJ)=A(K,NJ)+BI(K)
            ENDDO
           IF (IROT>0) THEN
            DO K=1,3
             AR(K,NJ)=AR(K,NJ)+BI(K+3)
            ENDDO
           ENDIF
         END IF
        ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  UPDK_BC                       source/constraints/general/rbe3/rbe3_imp0.F
Chd|-- called by -----------
Chd|        RBE2_IMP1                     source/constraints/general/rbe2/rbe2_imp0.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE UPDK_BC(JT,JR,K ,ISTIF)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JT(3),JR(3),ISTIF
C     REAL
      my_real
     .    K(6,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
C     REAL
      my_real
     .    R
C------------------------------------
C
        DO I=1,3
         DO J=1,3
          K(I,J)= K(I,J)*JT(I)*JT(J)
          K(I,J+3)= K(I,J+3)*JT(I)*JR(J)
          K(I+3,J)= K(I+3,J)*JR(I)*JT(J)
          K(I+3,J+3)= K(I+3,J+3)*JR(I)*JR(J)
         ENDDO
        ENDDO
          R = ZERO
        DO I=1,6
         DO J=1,6
          R=R+ABS(K(I,J))
         ENDDO
        ENDDO
          IF (R<EM30) THEN
           ISTIF = 0
          ELSE
           ISTIF = 1
          ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  UPDK_BC2                      source/constraints/general/rbe3/rbe3_imp0.F
Chd|-- called by -----------
Chd|        RBE2_IMP1                     source/constraints/general/rbe2/rbe2_imp0.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE UPDK_BC2(JT,JR,K ,ISTIF)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JT(3),JR(3),ISTIF
C     REAL
      my_real
     .    K(6,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
C     REAL
      my_real
     .    R
C------------------------------------
C
        DO I=1,3
         DO J=1,3
          K(I,J)= K(I,J)*JT(J)
          K(I,J+3)= K(I,J+3)*JR(J)
          K(I+3,J)= K(I+3,J)*JT(J)
          K(I+3,J+3)= K(I+3,J+3)*JR(J)
         ENDDO
        ENDDO
        R=ZERO
        DO I=1,6
         DO J=1,6
          R=R+ABS(K(I,J))
         ENDDO
        ENDDO
          IF (R<EM30) THEN
           ISTIF = 0
          ELSE
           ISTIF = 1
          ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  UPDB_CDI                      source/constraints/general/rbe3/rbe3_imp0.F
Chd|-- called by -----------
Chd|        RBE3_IMP1                     source/constraints/general/rbe3/rbe3_imp0.F
Chd|        RBE3_IMPB0                    source/constraints/general/rbe3/rbe3_imp0.F
Chd|        RBE3_IMPB2                    source/constraints/general/rbe3/rbe3_imp0.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE UPDB_CDI(FDI,MDI,BD,BI,IROT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IROT
C     REAL
      my_real
     .    FDI(3,6),MDI(3,6),BD(6),BI(6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
C     REAL
C-------Update =CDI^t[BD]---
         DO J=1,6
            BI(J) = ZERO
         ENDDO
C
        DO I=1,3
         DO J=1,6
            BI(I)=BI(I)+FDI(I,J)*BD(J)
         ENDDO
        ENDDO
       IF (IROT>0) THEN
        DO I=4,6
         DO J=1,6
            BI(I)=BI(I)+MDI(I-3,J)*BD(J)
         ENDDO
        ENDDO
       END IF
C
      RETURN
      END
Chd|====================================================================
Chd|  UPDK_CDII                     source/constraints/general/rbe3/rbe3_imp0.F
Chd|-- called by -----------
Chd|        RBE3_FR1                      source/constraints/general/rbe3/rbe3_imp0.F
Chd|        RBE3_FRUPD                    source/constraints/general/rbe3/rbe3_imp0.F
Chd|        RBE3_IMP1                     source/constraints/general/rbe3/rbe3_imp0.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE UPDK_CDII(FDI,MDI,KDD,KII,IROT,ND)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IROT,ND
C     REAL
      my_real
     .    FDI(3,6),MDI(3,6),KDD(6,6),KII(6,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,K,L
C     REAL
C-------Update KII=CDI^t[KDD]CDI----FDI=CDI^t
        DO I=1,6
         DO J=I,6
            KII(I,J)=ZERO
         ENDDO
        ENDDO
C----   FDI[KDD]FDI^t------
        DO I=1,3
         DO J=I,3
          DO K=1,ND
           DO L=1,ND
               KII(I,J)=KII(I,J)+FDI(I,K)*KDD(K,L)*FDI(J,L)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
        DO I=1,3
         DO J=I,3
             KII(J,I)=KII(I,J)
         ENDDO
        ENDDO
       IF (IROT>0) THEN
C----   MDI[KDD]MDI^t------
        DO I=1,3
         DO J=I,3
          DO K=1,ND
           DO L=1,ND
               KII(I+3,J+3)=KII(I+3,J+3)+MDI(I,K)*KDD(K,L)*MDI(J,L)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
C----   FDI[KDD]MDI^t------
        DO I=1,3
         DO J=1,3
          DO K=1,ND
           DO L=1,ND
               KII(I,J+3)=KII(I,J+3)+FDI(I,K)*KDD(K,L)*MDI(J,L)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
        DO I=1,3
         DO J=1,3
              KII(I+3,J)=KII(J,I+3)
         ENDDO
        ENDDO
        DO I=1,3
         DO J=I,3
              KII(J+3,I+3)=KII(I+3,J+3)
         ENDDO
        ENDDO
       ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  UPDK_CDIJ                     source/constraints/general/rbe3/rbe3_imp0.F
Chd|-- called by -----------
Chd|        RBE3_FR1                      source/constraints/general/rbe3/rbe3_imp0.F
Chd|        RBE3_IMP1                     source/constraints/general/rbe3/rbe3_imp0.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE UPDK_CDIJ(FDI,MDI,FDJ,MDJ,KDD,KIJ,IROT,ND)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IROT,ND
C     REAL
      my_real
     .    FDI(3,6),MDI(3,6),FDJ(3,6),MDJ(3,6),KDD(6,6),KIJ(6,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,K,L
C     REAL
C-------Update KII=CDI^t[KDD]CDJ---
        DO I=1,6
         DO J=1,6
            KIJ(I,J)=ZERO
         ENDDO
        ENDDO
C----   FDI[KDD]FDJ^t------
        DO I=1,3
         DO J=1,3
          DO K=1,ND
           DO L=1,ND
               KIJ(I,J)=KIJ(I,J)+FDI(I,K)*KDD(K,L)*FDJ(J,L)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       IF (IROT>0) THEN
C----   MDI[KDD]MDJ^t------
        DO I=1,3
         DO J=1,3
          DO K=1,ND
           DO L=1,ND
               KIJ(I+3,J+3)=KIJ(I+3,J+3)+MDI(I,K)*KDD(K,L)*MDJ(J,L)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
C----   FDI[KDD]MDI^t------
        DO I=1,3
         DO J=1,3
          DO K=1,ND
           DO L=1,ND
               KIJ(I,J+3)=KIJ(I,J+3)+FDI(I,K)*KDD(K,L)*MDJ(J,L)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
C----   MDI[KDD]MDJ^t------
        DO I=1,3
         DO J=1,3
          DO K=1,ND
           DO L=1,ND
               KIJ(I+3,J)=KIJ(I+3,J)+MDI(I,K)*KDD(K,L)*FDJ(J,L)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  UPDK_CDI                      source/constraints/general/rbe3/rbe3_imp0.F
Chd|-- called by -----------
Chd|        RBE3_FR1                      source/constraints/general/rbe3/rbe3_imp0.F
Chd|        RBE3_IMP1                     source/constraints/general/rbe3/rbe3_imp0.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE UPDK_CDI(FDI,MDI,KDD,KIJ,IROT,ND,ISYM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IROT,ND,ISYM
C     REAL
      my_real
     .    FDI(3,6),MDI(3,6),KDD(6,6),KIJ(6,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,K,L
C     REAL
C-------Update KIJ=[KDD]CDI---
        DO I=1,6
         DO J=1,6
            KIJ(I,J)=ZERO
         ENDDO
        ENDDO
C----   FDI[KDD]------
        DO I=1,ND
         DO J=1,3
          DO K=1,ND
               KIJ(I,J)=KIJ(I,J)+KDD(I,K)*FDI(J,K)
          ENDDO
         ENDDO
        ENDDO
       IF (IROT>0) THEN
C----   MDI[KDD]------
        DO I=1,ND
         DO J=1,3
          DO K=1,ND
               KIJ(I,J+3)=KIJ(I,J+3)+KDD(I,K)*MDI(J,K)
          ENDDO
         ENDDO
        ENDDO
       ENDIF
C
       IF (ISYM==1) THEN
        DO I=1,6
         DO J=1,6
          KIJ(I,J)=KIJ(I,J)+KIJ(J,I)
         ENDDO
        ENDDO
       ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  UPDFRK_BC                     source/constraints/general/rbe3/rbe3_imp0.F
Chd|-- called by -----------
Chd|        RBE3_FRUPD                    source/constraints/general/rbe3/rbe3_imp0.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE UPDFRK_BC(JT, K ,ISTIF)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JT(3),ISTIF
C     REAL
      my_real
     .    K(6,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
C     REAL
      my_real
     .    R
C------------------------------------
C
        DO I=1,3
         DO J=1,3
          K(I,J)= K(I,J)*JT(I)*JT(J)
         ENDDO
        ENDDO
          R = ZERO
        DO I=1,3
         DO J=1,3
          R=R+ABS(K(I,J))
         ENDDO
        ENDDO
          IF (R<EM30) THEN
           ISTIF = 0
          ELSE
           ISTIF = 1
          ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  RBE3_FRUPD                    source/constraints/general/rbe3/rbe3_imp0.F
Chd|-- called by -----------
Chd|        DIAG_INT                      source/mpi/implicit/imp_fri.F 
Chd|        UPDK_MV                       source/airbag/monv_imp0.F     
Chd|-- calls ---------------
Chd|        UPDFRK_BC                     source/constraints/general/rbe3/rbe3_imp0.F
Chd|        UPDK_CDII                     source/constraints/general/rbe3/rbe3_imp0.F
Chd|====================================================================
      SUBROUTINE RBE3_FRUPD(NIR   ,IML   ,FDSTNB ,MDSTNB ,NDOF  ,
     1                      JT    ,IROT  ,KSS    ,DIAG_M3)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER
     .   NIR   ,IML(*)   ,NDOF(*),JT(*)
C     REAL
      my_real
     .   KSS(6),DIAG_M3(6,NIR),FDSTNB(18,NIR),MDSTNB(18,NIR)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,  JD,  L, JJ,NJ,ND,IROT,NS,ISTIF
C     REAL
      my_real
     .   K0(6,6),KIJ(6,6)
C-----------------------------------------------
          ND = 3
       DO J = 1,3
        K0(J,J)=KSS(J)
       ENDDO
       K0(1,2)=KSS(4)
       K0(1,3)=KSS(5)
       K0(2,3)=KSS(6)
       K0(2,1) = K0(1,2)
       K0(3,1) = K0(1,3)
       K0(3,2) = K0(2,3)
       CALL UPDFRK_BC(JT,K0 ,ISTIF)
       IF (ISTIF>0) THEN
        DO J=1,NIR
         CALL UPDK_CDII(FDSTNB(1,J),MDSTNB(1,J),
     .                  K0 ,KIJ ,IROT ,ND   )
         DO JJ=1,6
            DIAG_M3(JJ,J)=KIJ(JJ,JJ)
           ENDDO
        ENDDO
       ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  RBE3_FR0                      source/constraints/general/rbe3/rbe3_imp0.F
Chd|-- called by -----------
Chd|        UPD_KML                       source/mpi/implicit/imp_fri.F 
Chd|        UPD_KSL                       source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        RBE3CL                        source/constraints/general/rbe3/rbe3f.F
Chd|        RBE3_FR1                      source/constraints/general/rbe3/rbe3_imp0.F
Chd|====================================================================
      SUBROUTINE RBE3_FR0(NS    ,NML   ,IML    ,X     ,IROT   ,
     2                     JT    ,JR    ,FRBE3 ,SKEW  ,IKC    ,
     3                     NDOF  ,IADK  ,JDIK   ,DIAG_K ,LT_K ,
     2                     KSS   ,KSM    ,KNM   ,KRM    ,IDLM  ,
     3                     ISS   ,ISM    ,ITAB  ,ISK    ,ID  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NS, NML,IML(*),JT(3),JR(3),IROT
      INTEGER IADK(*),JDIK(*),NDOF(*),ITAB(*),
     .        IDLM(*)  ,ISS  ,ISM,ISK(*),IKC(*),ID
C     REAL
      my_real
     .   X(3,*),DIAG_K(*),LT_K(*),FRBE3(*),SKEW(*),
     .   KSS(6),KSM(3,3),KNM(3,3,*),KRM(3,3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
C     REAL
      my_real
     .   FDSTNB(18,NML),MDSTNB(18,NML)
C------------------------------------
C     VITESSES DES NOEUDS SECONDS
C------------------------------------
       IF (NDOF(NS)<=0) RETURN
        CALL RBE3CL(IML  ,ISK   ,NS      ,X      ,FRBE3  ,
     .              SKEW ,NML   ,IROT    ,FDSTNB ,MDSTNB ,ID)
        CALL RBE3_FR1(NS    ,NML   ,IML    ,X     ,IROT   ,
     2                JT    ,JR    ,FDSTNB ,MDSTNB,IKC    ,
     3                NDOF  ,IADK  ,JDIK   ,DIAG_K ,LT_K ,
     2                KSS   ,KSM    ,KNM   ,KRM    ,IDLM  ,
     3                ISS   ,ISM    ,ITAB  )
C
      RETURN
      END
Chd|====================================================================
Chd|  RBE3_FR1                      source/constraints/general/rbe3/rbe3_imp0.F
Chd|-- called by -----------
Chd|        RBE3_FR0                      source/constraints/general/rbe3/rbe3_imp0.F
Chd|-- calls ---------------
Chd|        PRINT_WKIJ                    source/implicit/imp_glob_k.F  
Chd|        PUT_KMII                      source/implicit/imp_glob_k.F  
Chd|        PUT_KMIJ                      source/implicit/imp_glob_k.F  
Chd|        UPDK_CDI                      source/constraints/general/rbe3/rbe3_imp0.F
Chd|        UPDK_CDII                     source/constraints/general/rbe3/rbe3_imp0.F
Chd|        UPDK_CDIJ                     source/constraints/general/rbe3/rbe3_imp0.F
Chd|====================================================================
      SUBROUTINE RBE3_FR1(NS    ,NML   ,IML    ,X     ,IROT   ,
     2                     JT    ,JR    ,FDSTNB ,MDSTNB,IKC    ,
     3                     NDOF  ,IADK  ,JDIK   ,DIAG_K ,LT_K ,
     2                     KSS   ,KSM    ,KNM   ,KRM    ,IDLM  ,
     3                     ISS   ,ISM    ,ITAB  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NS, NML,IML(*),JT(3),JR(3),IROT
      INTEGER IADK(*),JDIK(*),NDOF(*),ITAB(*),
     .        IDLM(*)  ,ISS  ,ISM,IKC(*)
      my_real
     .   X(3,*),DIAG_K(*),LT_K(*),FDSTNB(18,*),MDSTNB(18,*),
     .   KSS(6),KSM(3,3),KNM(3,3,*),KRM(3,3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIR, I, J, J1, J2, J3, J4, K, JD, II, L, JJ,
     .        I1,ID,NL,NI,NJ,NIDOF,ND,NDI,NDJ,NDM,NM,L1,NM1,
     .        NIR1,IR,IP,ISTIF,NDOFI
      my_real
     .   KDD(6,6),KII(6,6)
C------------------------------------
C     VITESSES DES NOEUDS SECONDS
C------------------------------------
       IF (NDOF(NS)<=0) RETURN
C
       NDOFI = 3
       IP=4
       I = NS
        DO K=1,6
        DO J=1,6
         KDD(K,J)=ZERO
        ENDDO
        ENDDO
       IF (ISS>0) THEN
        DO K=1,NDOFI
         KDD(K,K) = KSS(K)
        ENDDO
        KDD(1,2) = KSS(4)
        KDD(1,3) = KSS(5)
        KDD(2,3) = KSS(6)
        KDD(2,1) = KDD(1,2)
        KDD(3,1) = KDD(1,3)
        KDD(3,2) = KDD(2,3)
C        CALL UPDFRK_BC(JT,KDD,ISTIF)
C
C-------Update K(main node)---
C        IF (ISTIF>0) THEN
         DO J=1,NML
          NJ=IML(J)
          ND = NDOF(NJ)
C-------Update CDI^t[KDD]CDI---
          CALL UPDK_CDII(FDSTNB(1,J),MDSTNB(1,J),KDD,KII,IROT,NDOFI)
          CALL PUT_KMII(IDLM(J),IADK,DIAG_K,LT_K ,KII,ND)
          DO I1=J+1,NML
           NM=IML(I1)
C-------Update CDI^t[KDD]CDJ---
           CALL UPDK_CDIJ(FDSTNB(1,J),MDSTNB(1,J),FDSTNB(1,I1),
     .                    MDSTNB(1,I1),KDD,KII,IROT,NDOFI)
           CALL PUT_KMIJ(IDLM(J) ,IDLM(I1) ,IADK,JDIK,LT_K,
     .                   KII,ND ,ND  ,IR )
           IF (IR==1) CALL PRINT_WKIJ(ITAB(NJ) ,ITAB(NM) ,IP )
          ENDDO
         ENDDO
        ELSE
        END IF
C       END IF !(ISS>0) THEN
C--------no diag--Kjm=sum(KjsCsm)--
       IF (ISM>0) THEN
C--------no diag--Kjm=sum(KjsCsm)--
          DO K=1,NDOFI
          DO J=1,NDOFI
           KDD(K,J) = KSM(K,J)
          ENDDO
          ENDDO
C------- Update ---
C          CALL UPDFRK_BC(JT,KDD,ISTIF)
C    IF (ISTIF>0) THEN
           DO J=1,NML
            NJ=IML(J)
            CALL UPDK_CDI(FDSTNB(1,J),MDSTNB(1,J),KDD,KII,IROT,NDOFI,0)
            DO K=1,NDOFI
            DO J1=1,NDOFI
             KNM(K,J1,J)=KII(J1,K)
             KRM(K,J1,J)=KII(J1,K+NDOFI)
            ENDDO
            ENDDO
           ENDDO
          ENDIF
C       ENDIF
C
      RETURN
      END
