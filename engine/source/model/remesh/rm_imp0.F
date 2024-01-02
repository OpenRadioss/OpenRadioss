Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  RMDIM_IMP                     source/model/remesh/rm_imp0.F 
Chd|-- called by -----------
Chd|        DIM_KINMAX                    source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|        CP_INT                        source/implicit/produt_v.F    
Chd|        REORDER_A                     source/implicit/ind_glob_k.F  
Chd|        REMESH_MOD                    share/modules/remesh_mod.F    
Chd|====================================================================
      SUBROUTINE RMDIM_IMP(IXC ,IXTG  ,NDOF   ,NNMAX ,NKINE,
     1                     INLOC,NROW ,ITAB   ,SH4TREE,SH3TREE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE REMESH_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "remesh_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXC(NIXC,*),IXTG(NIXTG,*),NDOF(*),NNMAX,
     1         NKINE,INLOC(*),NROW(*),ITAB(*), 
     2         SH4TREE(KSH4TREE,*), SH3TREE(KSH3TREE,*)
C-----------------------------------------------
C   External function
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, NN, LEVEL, IP, NLEV,I,J,K,L,M1,M2,MK1,MK2
      INTEGER SON,M(4),MC,NI(5),MN,NS,NZ,NR,NK,NKS,IS
      INTEGER, DIMENSION(:),ALLOCATABLE :: NROWK
      INTEGER, DIMENSION(:,:),ALLOCATABLE :: ICOK
C-----------------------------------------------
      TAGNOD=0
      NSH_KIN=0
      DO LEVEL=LEVELMAX-1,0,-1

        DO NN=PSH4KIN(LEVEL)+1,PSH4KIN(LEVEL+1)
          N    =LSH4KIN(NN)

          SON=SH4TREE(2,N)
C
          MC=IXC(4,SON)

          TAGNOD(MC)=1
C
          M(1)=IXC(3,SON  )
          M(2)=IXC(4,SON+1)
          M(3)=IXC(5,SON+2)
          M(4)=IXC(2,SON+3)
            DO J = 1 , 4
             MN = M(J)
           IF(TAGNOD(MN)==0.AND.NDOF(MN)>0) NSH_KIN= NSH_KIN + 1
           TAGNOD(MN)=1
            END DO 
        END DO

        DO NN=PSH3KIN(LEVEL)+1,PSH3KIN(LEVEL+1)
          N    =LSH3KIN(NN)

          SON=SH3TREE(2,N)
C
          M(1)=IXTG(4,SON+3)
          M(2)=IXTG(2,SON+3)
          M(3)=IXTG(3,SON+3)
            DO J = 1 , 3
             MN = M(J)
           IF(TAGNOD(MN)==0.AND.NDOF(MN)>0) NSH_KIN= NSH_KIN + 1
           TAGNOD(MN)=1
            END DO 
        END DO

      END DO
      
      IF (NSH_KIN==0) RETURN

      ALLOCATE(ISH_NS(NSH_KIN),ISH_MS(2,NSH_KIN))
      
      NS = 0
      TAGNOD=0
      DO LEVEL=LEVELMAX-1,0,-1

        DO NN=PSH4KIN(LEVEL)+1,PSH4KIN(LEVEL+1)
          N    =LSH4KIN(NN)

          SON=SH4TREE(2,N)

            DO J = 1 , 4
             NI(J) = IXC(J+1,N)
            END DO 
            NI(5) = NI(1)
C
          MC=IXC(4,SON)

          TAGNOD(MC)=1
C
          M(1)=IXC(3,SON  )
          M(2)=IXC(4,SON+1)
          M(3)=IXC(5,SON+2)
          M(4)=IXC(2,SON+3)
            DO J = 1 , 4
             MN = M(J)
           IF(TAGNOD(MN)==0.AND.NDOF(MN)>0) THEN
              NS= NS + 1
              ISH_NS(NS) = MN
              ISH_MS(1,NS) = NI(J)
              ISH_MS(2,NS) = NI(J+1)
           END IF
           TAGNOD(MN)=1
            END DO 
        END DO


        DO NN=PSH3KIN(LEVEL)+1,PSH3KIN(LEVEL+1)
          N    =LSH3KIN(NN)

          SON=SH3TREE(2,N)

            DO J = 1 , 3
             NI(J) = IXTG(J+1,N)
            END DO 
            NI(4) = NI(1)
C
          M(1)=IXTG(4,SON+3)
          M(2)=IXTG(2,SON+3)
          M(3)=IXTG(3,SON+3)
            DO J = 1 , 3
             MN = M(J)
           IF(TAGNOD(MN)==0.AND.NDOF(MN)>0) THEN
              NS= NS + 1
              ISH_NS(NS) = MN
              ISH_MS(1,NS) = NI(J)
              ISH_MS(2,NS) = NI(J+1)
           END IF
           TAGNOD(MN)=1
            END DO 
        END DO

      END DO

      TAGNOD=0
      NK = 0
      DO I = 1, NSH_KIN
       NS = ISH_NS(I)
       IF (TAGNOD(NS)==0) THEN
        NK=NK+1
        TAGNOD(NS)=NK
       END IF
      END DO 
      NKS = NK
C-------i,j<->j,i
      DO NN=1,NSH4ACT
        N    =LSH4ACT(NN)
           IS = 0
         DO J=1,4
          M(J)=IXC(J+1,N)
          MN = TAGNOD(M(J))
            IF (MN>0.AND.MN<=NKS) IS = MN 
         ENDDO 
         IF (IS > 0) THEN
          DO J=1,4
           NS=M(J)
           IF (TAGNOD(NS)==0) THEN
            NK=NK+1
            TAGNOD(NS)=NK
           END IF
          END DO
         END IF 
      END DO 
      DO NN=1,NSH3ACT
        N    =LSH3ACT(NN)
           IS = 0
         DO J=1,3
          M(J)=IXTG(J+1,N)
          MN = TAGNOD(M(J))
            IF (MN>0.AND.MN<=NKS) IS = MN 
         ENDDO 
         IF (IS > 0) THEN
          DO J=1,3
           NS=M(J)
           IF (TAGNOD(NS)==0) THEN
            NK=NK+1
            TAGNOD(NS)=NK
           END IF
          END DO
         END IF 
      END DO 
C
      L=4**LEVELMAX
      ALLOCATE(NROWK(NK),ICOK(NNMAX+L,NK))
      NROWK=0
C-----------------elementary connectivity -----
      DO NN=1,NSH4ACT
        N    =LSH4ACT(NN)
         DO J=1,4
          M(J)=IXC(J+1,N)
         ENDDO 
         DO J=1,4
          NS=M(J)
          K=TAGNOD(NS)
          IF (K > 0) THEN
           DO L=1,4
            IF (NS/=M(L)) THEN
             CALL REORDER_A(NROWK(K),ICOK(1,K),M(L)) 
            ENDIF
           ENDDO
          ENDIF 
         ENDDO
      END DO 
      DO NN=1,NSH3ACT
        N    =LSH3ACT(NN)
         DO J=1,3
          M(J)=IXTG(J+1,N)
         ENDDO 
         DO J=1,3
          NS=M(J)
          K=TAGNOD(NS)
          IF (K > 0) THEN
           DO L=1,3
            IF (NS/=M(L)) THEN
             CALL REORDER_A(NROWK(K),ICOK(1,K),M(L)) 
            ENDIF
           ENDDO
          ENDIF 
         ENDDO
      END DO 
C-----------------new connectivity due to kin-----
      DO I = 1, NSH_KIN
       NS = ISH_NS(I)
       M1 = ISH_MS(1,I)
       M2 = ISH_MS(2,I)
       K=TAGNOD(NS)
       MK1 = TAGNOD(M1)
       MK2 = TAGNOD(M2)
       DO J=1,NROWK(K)
          NN = ICOK(J,K)
          MN = TAGNOD(NN)
        IF (NN/=M1) THEN
         CALL REORDER_A(NROWK(MK1),ICOK(1,MK1),NN) 
         CALL REORDER_A(NROWK(MN),ICOK(1,MN),M1) 
        END IF
        IF (NN/=M2) THEN
         CALL REORDER_A(NROWK(MK2),ICOK(1,MK2),NN) 
         CALL REORDER_A(NROWK(MN),ICOK(1,MN),M2) 
        END IF
       END DO 
      END DO 
C
      NZ = 0
      DO I = 1, NSH_KIN
       NS = ISH_NS(I)
       K=TAGNOD(NS)
       IF (INLOC(NS)==0) THEN
        NKINE=NKINE+1
        INLOC(NS)=NKINE
       END IF
       NROW(NS)=MAX(NROW(NS),NROWK(K))
       DO J=1,NROWK(K)
          NN = ICOK(J,K)
          MN = TAGNOD(NN)
        NNMAX=MAX(NNMAX,NROWK(MN))
        IF (INLOC(NN)==0) THEN
         NKINE=NKINE+1
         INLOC(NN)=NKINE
        END IF
        NROW(NN)=MAX(NROW(NN),NROWK(MN))
       END DO 
       NZ = NZ + NROWK(K)
      END DO 
C
      
      ALLOCATE(IAD_NJ(NSH_KIN+1),JDI_NJ(NZ))
      
      IAD_NJ(1) = 1
      
      DO I = 1, NSH_KIN
       NS = ISH_NS(I)
       K=TAGNOD(NS)
       IAD_NJ(I+1) = IAD_NJ(I) + NROWK(K)
       CALL CP_INT(NROWK(K),ICOK(1,K),JDI_NJ(IAD_NJ(I)))
      END DO
      
C
      DEALLOCATE(NROWK,ICOK)
C
      RETURN
      END     

Chd|====================================================================
Chd|  RMIND_IMP                     source/model/remesh/rm_imp0.F 
Chd|-- called by -----------
Chd|        IND_KINE_K                    source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|        REORDER_A                     source/implicit/ind_glob_k.F  
Chd|        REMESH_MOD                    share/modules/remesh_mod.F    
Chd|====================================================================
      SUBROUTINE RMIND_IMP(NNMAX,INLOC,NROWK,ICOK )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE REMESH_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNMAX
      INTEGER INLOC(*),NROWK(*),ICOK(NNMAX,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, NN, LEVEL, IP, NLEV,I,J,K,M1,M2,MK1,MK2
      INTEGER MN,NS,NZ,NR,NK
C-----------------------------------------------
      DO I = 1, NSH_KIN
       NS = ISH_NS(I)
       M1 = ISH_MS(1,I)
       M2 = ISH_MS(2,I)
       MK1 = INLOC(M1)
       MK2 = INLOC(M2)
       DO J=IAD_NJ(I),IAD_NJ(I+1)-1
        NN = JDI_NJ(J)
        MN = INLOC(NN)
        IF (NN/=M1) THEN
         CALL REORDER_A(NROWK(MK1),ICOK(1,MK1),NN) 
         CALL REORDER_A(NROWK(MN),ICOK(1,MN),M1) 
        END IF 
        IF (NN/=M2) THEN
         CALL REORDER_A(NROWK(MK2),ICOK(1,MK2),NN) 
         CALL REORDER_A(NROWK(MN),ICOK(1,MN),M2) 
        END IF 
       END DO 
      END DO 
C
      RETURN
      END     

Chd|====================================================================
Chd|  RM_IMP0                       source/model/remesh/rm_imp0.F 
Chd|-- called by -----------
Chd|        UPD_GLOB_K                    source/implicit/upd_glob_k.F  
Chd|-- calls ---------------
Chd|        RM_IMP1                       source/model/remesh/rm_imp0.F 
Chd|        REMESH_MOD                    share/modules/remesh_mod.F    
Chd|====================================================================
      SUBROUTINE RM_IMP0(NDDL  ,IADK  ,JDIK  ,DIAG_K ,LT_K  ,
     1                   NDOF  ,IDDL  ,IKC   ,B      ,ITAB  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE REMESH_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDDL,
     .        IADK(*),JDIK(*),NDOF(*),IDDL(*),IKC(*),ITAB(*)
       my_real
     .        DIAG_K(*),LT_K(*),B(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,NS,NR
C-----------------------------------------------
      DO I = 1, NSH_KIN
       NS = ISH_NS(I)
       NR = IAD_NJ(I+1)-IAD_NJ(I)
            CALL RM_IMP1(2    ,ISH_MS(1,I),NS    ,NR   ,
     2                   JDI_NJ(IAD_NJ(I)) ,
     3                   ITAB ,IKC  ,NDOF  ,NDDL ,IDDL ,
     4                   IADK ,JDIK ,DIAG_K,LT_K ,B    )
      END DO
C
      RETURN
      END     

Chd|====================================================================
Chd|  RM_IMP1                       source/model/remesh/rm_imp0.F 
Chd|-- called by -----------
Chd|        RM_IMP0                       source/model/remesh/rm_imp0.F 
Chd|-- calls ---------------
Chd|        GET_KII                       source/implicit/imp_glob_k.F  
Chd|        GET_KIJ                       source/implicit/imp_glob_k.F  
Chd|        PRINT_WKIJ                    source/implicit/imp_glob_k.F  
Chd|        PUT_KII                       source/implicit/imp_glob_k.F  
Chd|        PUT_KIJ                       source/implicit/imp_glob_k.F  
Chd|        UPDKDD                        source/interfaces/interf/i2_imp1.F
Chd|        UPDKDD1                       source/interfaces/interf/i2_imp1.F
Chd|====================================================================
      SUBROUTINE RM_IMP1(NIR  ,IRECT ,I    ,NR   ,NODS ,
     3                   ITAB ,IKC  ,NDOF  ,NDDL ,IDDL ,
     4                   IADK ,JDIK ,DIAG_K,LT_K ,B    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER 
     .        NIR,IRECT(*),I,NR,NODS(*),ITAB(*)
      INTEGER NDDL,IADK(*),JDIK(*),NDOF(*),IDDL(*),IKC(*)
C     REAL
      my_real
     .   DIAG_K(*),LT_K(*),B(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  J, J1, J2, J3, J4, K, JD, II, L, JJ,
     .        I1,ID,NL,NI,NJ,NIDOF,ND,NDI,NDJ,NDM,NM,L1,NM1,
     .        NIR1,IR
C     REAL
      my_real
     .   KDD(6,6),BD(6),KII(6,6),BI(6),FACM,FACM2
C --------------------------------------------------    
        NDM = 0
        DO J=1,NIR
         NJ=IRECT(J)
         NDM = MAX(NDM,NDOF(NJ))
        ENDDO
        IF (NDM==0) RETURN
      FACM = ONE / NIR
      FACM2 = FACM*FACM
        DO K=1,NDOF(I)
         ID = IDDL(I)+K
         IKC(ID)=12 
         BD(K)=B(ID) 
        ENDDO 
        DO K=NDOF(I)+1,6
         BD(K)=ZERO 
        ENDDO 
        CALL GET_KII(I ,IDDL ,IADK,DIAG_K,LT_K ,KDD,NDOF(I))
C-------Update K(main node),B---
        DO J=1,NIR
          NJ=IRECT(J)
          ND = MIN(NDM,NDOF(NJ))
          CALL UPDKDD(ND,KDD,KII,FACM2,1)
          CALL PUT_KII(NJ,IDDL ,IADK,DIAG_K,LT_K ,KII,ND)
          DO K=1,ND
           ID = IDDL(NJ)+K
           B(ID) = B(ID) + FACM*BD(K)
          ENDDO 
          DO I1=J+1,NIR
           NM=IRECT(I1)
           ND = MIN(ND,NDOF(NM))
           CALL UPDKDD(ND,KDD,KII,FACM2,0)
           CALL PUT_KIJ(NJ,NM,IDDL,IADK,JDIK,LT_K,KII,ND,ND,IR)
           IF (IR==1) CALL PRINT_WKIJ(ITAB(NJ) ,ITAB(NM) ,3 )
          ENDDO 
        ENDDO 
C--------no diag--Kjm=sum(KjsCsm)--
        DO I1 = 1,NR
          NI=NODS(I1)
          NIDOF=NDOF(NI)
          IF (NIDOF==0) CYCLE
          CALL GET_KIJ(NI,I,IDDL,IADK,JDIK,LT_K,KDD,NIDOF,NDOF(I),IR)
          IF (IR==1) CALL PRINT_WKIJ(ITAB(NI) ,ITAB(I) ,3 )
C------- Update ---
          NDI = MIN(NDM,NIDOF)
          DO J=1,NIR
           NJ=IRECT(J)
           NDJ = MIN(NDM,NDOF(NJ))
           IF (NDJ>0) THEN
            IF (NJ==NI) THEN
             CALL UPDKDD1(NIDOF,NDJ,KDD,KII,FACM,1)
             CALL PUT_KII(NJ ,IDDL ,IADK,DIAG_K,LT_K,KII,NDJ)
            ELSE
             CALL UPDKDD1(NDI,NDOF(I),KDD,KII,FACM,0)
             CALL PUT_KIJ(NI,NJ,IDDL,IADK,JDIK,LT_K,KII,NDI,NDJ,IR)
             IF (IR==1) CALL PRINT_WKIJ(ITAB(NI) ,ITAB(NJ) ,3 )
            ENDIF 
           ENDIF 
          ENDDO 
        ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  RM_IMP2                       source/model/remesh/rm_imp0.F 
Chd|-- called by -----------
Chd|        RECUKIN                       source/implicit/recudis.F     
Chd|-- calls ---------------
Chd|        REMESH_MOD                    share/modules/remesh_mod.F    
Chd|====================================================================
      SUBROUTINE RM_IMP2(IXC,IXTG,V ,VR ,SH4TREE,SH3TREE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE REMESH_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "param_c.inc"
#include      "remesh_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXC(NIXC,*),IXTG(NIXTG,*),
     2         SH4TREE(KSH4TREE,*), SH3TREE(KSH3TREE,*)
       my_real
     .        V(3,*),VR(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, NN, LEVEL, IP, NLEV,  IERR
      INTEGER SON,M(4),MC,N1,N2,N3,N4,J
C-----------------------------------------------
        TAGNOD=0
C-------
      DO LEVEL=0,LEVELMAX-1

        DO NN=PSH4KIN(LEVEL)+1,PSH4KIN(LEVEL+1)
          N    =LSH4KIN(NN)
C
          N1=IXC(2,N)
          N2=IXC(3,N)
          N3=IXC(4,N)
          N4=IXC(5,N)
C
          SON=SH4TREE(2,N)
C
          MC=IXC(3,SON+3)

          IF(TAGNOD(MC)==0)THEN
            TAGNOD(MC)=1
            DO J=1,3
              V(J,MC)= FOURTH*(V(J,N1)+V(J,N2)+V(J,N3)+V(J,N4))
            END DO
            DO J=1,3
              VR(J,MC)= FOURTH*(VR(J,N1)+VR(J,N2)+VR(J,N3)+VR(J,N4))
            END DO
          ELSE
          END IF
C
          M(1)=IXC(3,SON  )
          M(2)=IXC(4,SON+1)
          M(3)=IXC(5,SON+2)
          M(4)=IXC(2,SON+3)

          IF(TAGNOD(M(1))==0)THEN
            TAGNOD(M(1))=1
            DO J=1,3
              V(J,M(1))= HALF*(V(J,N1)+V(J,N2))
            END DO
            DO J=1,3
              VR(J,M(1))= HALF*(VR(J,N1)+VR(J,N2))
            END DO
          ELSE
          END IF

          IF(TAGNOD(M(2))==0)THEN
            TAGNOD(M(2))=1
            DO J=1,3
              V(J,M(2))= HALF*(V(J,N2)+V(J,N3))
            END DO
            DO J=1,3
              VR(J,M(2))= HALF*(VR(J,N2)+VR(J,N3))
            END DO
          ELSE
          END IF

          IF(TAGNOD(M(3))==0)THEN
            TAGNOD(M(3))=1
            DO J=1,3
              V(J,M(3))= HALF*(V(J,N3)+V(J,N4))
            END DO
            DO J=1,3
              VR(J,M(3))= HALF*(VR(J,N3)+VR(J,N4))
            END DO
          ELSE
          END IF

          IF(TAGNOD(M(4))==0)THEN
            TAGNOD(M(4))=1
            DO J=1,3
              V(J,M(4))= HALF*(V(J,N4)+V(J,N1))
            END DO
            DO J=1,3
              VR(J,M(4))= HALF*(VR(J,N4)+VR(J,N1))
            END DO
          ELSE
          END IF
        END DO

        DO NN=PSH3KIN(LEVEL)+1,PSH3KIN(LEVEL+1)
          N    =LSH3KIN(NN)
C
          N1=IXTG(2,N)
          N2=IXTG(3,N)
          N3=IXTG(4,N)
C
          SON=SH3TREE(2,N)
C
          M(1)=IXTG(4,SON+3)
          M(2)=IXTG(2,SON+3)
          M(3)=IXTG(3,SON+3)

          IF(TAGNOD(M(1))==0)THEN
            TAGNOD(M(1))=1
            DO J=1,3
              V(J,M(1))= HALF*(V(J,N1)+V(J,N2))
            END DO
            DO J=1,3
              VR(J,M(1))= HALF*(VR(J,N1)+VR(J,N2))
            END DO
          ELSE
          END IF

          IF(TAGNOD(M(2))==0)THEN
            TAGNOD(M(2))=1
            DO J=1,3
              V(J,M(2))= HALF*(V(J,N2)+V(J,N3))
            END DO
            DO J=1,3
              VR(J,M(2))= HALF*(VR(J,N2)+VR(J,N3))
            END DO
          ELSE
          END IF

          IF(TAGNOD(M(3))==0)THEN
            TAGNOD(M(3))=1
            DO J=1,3
              V(J,M(3))= HALF*(V(J,N3)+V(J,N1))
            END DO
            DO J=1,3
              VR(J,M(3))= HALF*(VR(J,N3)+VR(J,N1))
            END DO
          ELSE
          END IF
        END DO
C
      END DO
C
      RETURN
      END     
