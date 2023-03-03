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
Chd|  FVBRIC1                       source/airbag/fvbric1.F       
Chd|-- called by -----------
Chd|        INIT_MONVOL                   source/airbag/init_monvol.F   
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FVNORMAL                      source/airbag/fvmbag1.F       
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MONVOL_STRUCT_MOD             share/modules1/monvol_struct_mod.F
Chd|====================================================================
      SUBROUTINE FVBRIC1(T_MONVOLN, IBUF , ELEM  , IXS    , IBRIC,
     .                   TBRIC, NEL   , NELA   , NBRIC, 
     .                   TFAC , TAGELS, MONVID , NELI,
     .                   NNA   , ILVOUT ,
     .                   ELTG , X     , TITR, NB_NODE)
      USE MESSAGE_MOD
      USE MONVOL_STRUCT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(MONVOL_STRUCT_), INTENT(INOUT) :: T_MONVOLN
      INTEGER IXS(NIXS,*),
     .        IBRIC, NEL, NELA, NBRIC, 
     .        MONVID, NNA, ILVOUT,
     .        NELI, NB_NODE
      INTEGER, DIMENSION(T_MONVOLN%NNS + T_MONVOLN%NNI), INTENT(IN) :: IBUF
      INTEGER, DIMENSION(3, NEL + NELI), INTENT(INOUT) :: ELEM
      INTEGER, DIMENSION(NEL + 2 * NELI), INTENT(INOUT) :: TAGELS
      INTEGER, DIMENSION(2, NBRIC), INTENT(IN) :: TBRIC
      INTEGER, DIMENSION(12, NBRIC), INTENT(INOUT) :: TFAC
      INTEGER, DIMENSION(NEL + NELI), INTENT(IN) :: ELTG
      my_real
     .        X(3,*)
      CHARACTER*nchartitle,
     .        TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, ITAG(NB_NODE), IAD, J, NG, NNO, NFAC, NV, NG2, NALL, 
     .        NC, NNO2, K, KK, L, JJ, II,
     .        ITAG2(NB_NODE), NALL2, LL, N1, N2, N3, N4,
     .        INFO, NFAC2, NN1, NN2, NN3, NN4,
     .        IERROR, ISPLIT, IFOUND, NTYPE, NTYPE2, NELT
      my_real
     .        NSX, NSY, NSZ, NEX, NEY, NEZ, SS, NSX2, NSY2, NSZ2
C
      INTEGER, TARGET :: FAC4(3,4), FAC8(4,6), FAC6(4,5), NOD6(5)
      INTEGER, TARGET :: FAC5(4,5), NOD5(5), NFACE(4), NOD8(6), NOD3(4)
      DATA FAC4 /1,5,3,
     .           3,5,6,
     .           6,5,1,
     .           1,3,6/
      DATA FAC8 /1,4,3,2,
     .           5,6,7,8,
     .           1,2,6,5,
     .           2,3,7,6,
     .           3,4,8,7,
     .           4,1,5,8/
      DATA FAC6 /1,3,2,0,
     .           5,6,7,0,
     .           1,2,6,5,
     .           2,3,7,6,
     .           3,4,8,7/
      DATA NOD6 /3,3,4,4,4/
      DATA NOD8 /4,4,4,4,4,4/
      DATA NOD3 /3,3,3,3/
      DATA FAC5 /1,2,5,0,
     .           2,3,5,0,
     .           3,4,5,0,
     .           4,1,5,0,
     .           1,4,3,2/
      DATA NOD5 /3,3,3,3,4/
      DATA NFACE/6,4,5,5/
      INTEGER, DIMENSION(:), ALLOCATABLE :: ADSKY, ADDCNET, CNT
      INTEGER :: IAD1, IAD2, IP1(4), IP2(4), NODEID, TRIID, NOD1(4), NOD2(4), IS
      LOGICAL :: FOUND
      INTEGER :: FNOD1(4), NID(4), NID2(3)
      INTEGER, DIMENSION(:, :), POINTER :: FAC
      INTEGER, DIMENSION(:), POINTER :: NOD

      ALLOCATE(ADDCNET(NB_NODE + 1), ADSKY(NB_NODE + 1))
      DO I=1,NB_NODE
         ADDCNET(I) = 0
         ITAG(I)=0
         ITAG2(I)=0
      ENDDO
      ADDCNET(NB_NODE + 1)  = 0
      NELT=NEL+NELI             ! total number of triangles (surface + internal)
      DO I=1,NELT
         DO J=1,3
            JJ=IBUF(ELEM(J,I))  ! ELEM(J, I) = Node of triangle I, IBUF = Id of the node
            ITAG(JJ)=1          ! Tag the nodes of the triangles
            ADDCNET(JJ + 1) = ADDCNET(JJ + 1) + 1
         ENDDO
      ENDDO

      ADDCNET(1) = 1
      DO I = 2, NB_NODE + 1
         ADDCNET(I) = ADDCNET(I) + ADDCNET(I - 1)
      ENDDO
      DO I =  1, NB_NODE
         ADSKY(I) = ADDCNET(I)
      ENDDO
C     ============================
C     Node -> triangle connectivity
C     ============================
      ALLOCATE(CNT(ADDCNET(NB_NODE + 1)))
      DO I=1,NELT
         DO J=1,3
            JJ=IBUF(ELEM(J,I))  ! ELEM(J, I) = Node of triangle I, IBUF = Id of the node
            CNT(ADSKY(JJ)) = I
            ADSKY(JJ) = ADSKY(JJ) + 1
         ENDDO
      ENDDO

C
      ISPLIT=0
  10  IERROR=0  
      INFO=0
      NELA=0
      DO I=1,NBRIC
         II=TBRIC(1,I)
         NTYPE=TBRIC(2,I)
         NFAC=NFACE(NTYPE)
         SELECT CASE (NTYPE)
         CASE (1) 
            FAC => FAC8(1:4, 1:6)
            NOD => NOD8(1:6)
         CASE (2)
            FAC => FAC4(1:3, 1:4)
            NOD => NOD3(1:4)
         CASE (3)
            FAC => FAC6(1:4, 1:5)
            NOD => NOD6(1:5)
         CASE (4)
            FAC => FAC5(1:4, 1:5)
            NOD => NOD5(1:5)
         CASE DEFAULT
C     ERROR
         END SELECT
         DO J=1,NFAC
            NALL = 1
            NID(1:4) = 0
            DO K = 1, NOD(J)
               KK = FAC(K, J)
               NALL = NALL * ITAG(IXS(1+KK,II))
               NID(K) = IXS(1+KK,II)
            ENDDO
C
            IF (TFAC(2*(J-1)+1,I)==0) THEN
               IF (NALL==1) THEN
                  TFAC(2*(J-1)+1,I)=2
                  DO K = 1, NOD(J)
                     KK = FAC(K, J)
                     ITAG2(IXS(1+KK,II))=1
                  ENDDO
                  CALL FVNORMAL(X, NID(1), NID(2), NID(3), NID(4), NSX, NSY, NSZ)
C     Now check among all the triangles those who share their nodes with the considered face
C     Trick : useless to wander through the whole list, only elements connected to the nodes of 
C     the face have to be considered
                  IFOUND = 0
                  DO K = 1, NOD(J)
                     NODEID = NID(K)
                     IAD1 = ADDCNET(NODEID)
                     IAD2 = ADDCNET(NODEID + 1) - 1
                     DO IAD = IAD1, IAD2
                        TRIID = CNT(IAD)
                        NALL2 = 1
                        DO KK = 1, 3
                           NID2(KK) = IBUF(ELEM(KK, TRIID))
                           NALL2 = NALL2 * ITAG2(NID2(KK))
                        ENDDO
                        IF (NALL2 == 1) THEN
                           IFOUND = IFOUND + 1
                           TAGELS(TRIID) = I
                           CALL FVNORMAL(X,NID2(1),NID2(2),NID2(3),0,NEX,NEY,NEZ)
                           SS=NSX*NEX+NSY*NEY+NSZ*NEZ
                           IF (SS<=ZERO) THEN
                              CALL ANCMSG(MSGID=634,
     .                             MSGTYPE=MSGERROR,
     .                             ANMODE=ANINFO_BLIND_1,
     .                             I1=MONVID,
     .                             C1=TITR,
     .                             I2=IXS(NIXS,II))
                              INFO=1
                           ENDIF
                        ENDIF
                     ENDDO
                  ENDDO
C
                  IF(IFOUND == 0) THEN
                    IERROR=1
                    IF(ILVOUT >= 2) WRITE(IOUT,'(A,I10)') 
     .                'WARNING : CANNOT FIND AIRBAG TRIANGLE FOR BRICK',
     .                IXS(NIXS,II)
                  ENDIF
C
                  DO K = 1, NOD(J)
                     KK = FAC(K, J)
                     ITAG2(IXS(1+KK,II))=0
                  ENDDO

               ELSE   ! NALL=0
                  TFAC(2*(J-1)+1,I)=3
                  IF (NTYPE==2) THEN
                     NELA=NELA+1
                  ELSEIF (NTYPE==3) THEN
                     IF(NOD6(J)==4) THEN
                        NELA=NELA+2
                     ELSE
                        NELA=NELA+1
                     ENDIF
                  ELSEIF (NTYPE==4) THEN
                     IF(NOD5(J)==4) THEN
                        NELA=NELA+2
                     ELSE
                        NELA=NELA+1
                     ENDIF
                  ELSEIF (NTYPE==1) THEN
                     NELA=NELA+2
                  ENDIF
               ENDIF
C
            ELSEIF (TFAC(2*(J-1)+1,I)==-2) THEN
               IF (NALL==1) THEN
                  DO K = 1, NOD(J)
                     KK = FAC(K, J)
                     ITAG2(IXS(1+KK,II))=1
                  ENDDO

                  DO K=NEL+1,NELT
                     NALL2=1
                     DO L=1,3
                        LL=IBUF(ELEM(L,K))
                        NALL2=NALL2*ITAG2(LL)
                     ENDDO
                     IF (NALL2==1) THEN
                        IF (TAGELS(2*K-NEL-1) == 0) THEN
                           TAGELS(2*K-NEL-1)=I
                        ELSE 
                           TAGELS(2*K-NEL)=I
                        ENDIF
                     ENDIF
                  ENDDO

                  DO K = 1, NOD(J)
                     KK = FAC(K, J)
                     ITAG2(IXS(1+KK,II))=0
                  ENDDO
C
               ENDIF
            ENDIF
         ENDDO  ! boucle J=1,NFAC
      ENDDO     ! boucle I=1,NBRIC
      IF(IERROR==1.AND.ISPLIT==0) THEN
        ISPLIT=1
        DO K=1,NELT-1
           IF(TAGELS(K) /= 0) CYCLE
           IF(ELTG(K+1) /= ELTG(K)) CYCLE
C Split quad element along diagonal 13
           TAGELS(K+1)=1
           N1=ELEM(1,K)
           N3=ELEM(2,K+1)
           ELEM(3,K)=N3
           ELEM(1,K+1)=N1
        ENDDO
        DO I=1,NBRIC
         NFAC=NFACE(TBRIC(2,I))
         DO J=1,NFAC
            IF (TFAC(2*(J-1)+1,I)==2) TFAC(2*(J-1)+1,I)=0
         ENDDO
        ENDDO
        DO K=1,NELT
           TAGELS(K)=0
        ENDDO
      GO TO 10
      ELSEIF(IERROR==1.AND.ISPLIT==1) THEN 
           CALL ANCMSG(MSGID=1048,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=MONVID,C1=TITR)
      ENDIF
C

 
C
      IF(ILVOUT >= 3) THEN
        WRITE(IOUT,'(A)')'SOLID ELEMENT'
        WRITE(IOUT,'(A,A)')'     LOC    GLOB TYPE    6*(FLAG FACE,',
     .                     'NEIGHBOUR SOLID ELEMENT)'
        DO I=1,NBRIC
         WRITE(IOUT,'(2I8,I5,6(I5,I8))')I,TBRIC(1,I),TBRIC(2,I),
     .               (TFAC(2*(J-1)+1,I),TFAC(2*(J-1)+2,I),J=1,6)
        ENDDO
      ENDIF
C
C Verification de l'orientation des briques totalement internes
C
      IF (INFO==0) THEN
         DO I=1,NB_NODE
            ITAG(I)=0
         ENDDO
C
         DO I=1,NBRIC
CFA Une brique appuye sur l'airbag a des faces internes
            II=TBRIC(1,I)
            NTYPE=TBRIC(2,I)
            NFAC=NFACE(NTYPE)
            DO J=1,NFAC
               IF (TFAC(2*(J-1)+1,I)/=1) CYCLE
               NV=TFAC(2*(J-1)+2,I)
C
               IF (NTYPE==2) THEN
                  DO K=1,3
                     KK=FAC4(K,J)
                     ITAG(IXS(1+KK,II))=1
                  ENDDO
C Normale a la facette solide
                  N1=IXS(1+FAC4(1,J),II)
                  N2=IXS(1+FAC4(2,J),II)
                  N3=IXS(1+FAC4(3,J),II)
                  N4=0
                  CALL FVNORMAL(X,N1,N2,N3,N4,NSX,NSY,NSZ)
               ELSEIF (NTYPE==3) THEN
                  DO K=1,NOD6(J)
                     KK=FAC6(K,J)
                     ITAG(IXS(1+KK,II))=1
                  ENDDO
C Normale a la facette solide
                  N1=IXS(1+FAC6(1,J),II)
                  N2=IXS(1+FAC6(2,J),II)
                  N3=IXS(1+FAC6(3,J),II)
                  IF(NOD6(J)==4) THEN
                     N4=IXS(1+FAC6(4,J),II)
                  ELSE
                     N4=0
                  ENDIF
                  CALL FVNORMAL(X,N1,N2,N3,N4,NSX,NSY,NSZ)
               ELSEIF (NTYPE==4) THEN
                  DO K=1,NOD5(J)
                     KK=FAC5(K,J)
                     ITAG(IXS(1+KK,II))=1
                  ENDDO
C Normale a la facette solide
                  N1=IXS(1+FAC5(1,J),II)
                  N2=IXS(1+FAC5(2,J),II)
                  N3=IXS(1+FAC5(3,J),II)
                  IF(NOD5(J)==4) THEN
                     N4=IXS(1+FAC5(4,J),II)
                  ELSE
                     N4=0
                  ENDIF
                  CALL FVNORMAL(X,N1,N2,N3,N4,NSX,NSY,NSZ)
               ELSEIF (NTYPE==1) THEN
                  DO K=1,4
                     KK=FAC8(K,J)
                     ITAG(IXS(1+KK,II))=1
                  ENDDO
C Normale a la facette solide
                  N1=IXS(1+FAC8(1,J),II)
                  N2=IXS(1+FAC8(2,J),II)
                  N3=IXS(1+FAC8(3,J),II)
                  N4=IXS(1+FAC8(4,J),II)
                  CALL FVNORMAL(X,N1,N2,N3,N4,NSX,NSY,NSZ)
               ENDIF
C
               NTYPE2=TBRIC(2,NV)
               NFAC2=NFACE(NTYPE2)
               DO K=1,NFAC2
                  IF (NTYPE2==2) THEN
                     NALL=1
                     DO L=1,3
                        LL=FAC4(L,K)
                        NALL=NALL*ITAG(IXS(1+LL,TBRIC(1,NV)))
                     ENDDO
                  ELSEIF (NTYPE2==3) THEN
                     NALL=1
                     DO L=1,NOD6(K)
                        LL=FAC6(L,K)
                        NALL=NALL*ITAG(IXS(1+LL,TBRIC(1,NV)))
                     ENDDO
                  ELSEIF (NTYPE2==4) THEN
                     NALL=1
                     DO L=1,NOD5(K)
                        LL=FAC5(L,K)
                        NALL=NALL*ITAG(IXS(1+LL,TBRIC(1,NV)))
                     ENDDO
                  ELSEIF (NTYPE2==1) THEN
                     NALL=1
                     DO L=1,4
                        LL=FAC8(L,K)
                        NALL=NALL*ITAG(IXS(1+LL,TBRIC(1,NV)))
                     ENDDO
                  ENDIF
                  IF (NALL==0) CYCLE
C Normale a la facette du voisin
                  IF (NTYPE2==2) THEN
                     NN1=IXS(1+FAC4(1,K),TBRIC(1,NV))
                     NN2=IXS(1+FAC4(2,K),TBRIC(1,NV))
                     NN3=IXS(1+FAC4(3,K),TBRIC(1,NV))
                     NN4=0
                     CALL FVNORMAL(X,NN1,NN2,NN3,NN4,NSX2,NSY2,NSZ2)
                  ELSEIF (NTYPE2==3) THEN
                     NN1=IXS(1+FAC6(1,K),TBRIC(1,NV))
                     NN2=IXS(1+FAC6(2,K),TBRIC(1,NV))
                     NN3=IXS(1+FAC6(3,K),TBRIC(1,NV))
                     IF(NOD6(K)==4) THEN
                        NN4=IXS(1+FAC6(4,K),TBRIC(1,NV))
                     ELSE
                        NN4=0
                     ENDIF
                     CALL FVNORMAL(X,NN1,NN2,NN3,NN4,NSX2,NSY2,NSZ2)
                  ELSEIF (NTYPE2==4) THEN
                     NN1=IXS(1+FAC5(1,K),TBRIC(1,NV))
                     NN2=IXS(1+FAC5(2,K),TBRIC(1,NV))
                     NN3=IXS(1+FAC5(3,K),TBRIC(1,NV))
                     IF(NOD5(K)==4) THEN
                        NN4=IXS(1+FAC5(4,K),TBRIC(1,NV))
                     ELSE
                        NN4=0
                     ENDIF
                     CALL FVNORMAL(X,NN1,NN2,NN3,NN4,NSX2,NSY2,NSZ2)
                  ELSEIF (NTYPE2==1) THEN
                     NN1=IXS(1+FAC8(1,K),TBRIC(1,NV))
                     NN2=IXS(1+FAC8(2,K),TBRIC(1,NV))
                     NN3=IXS(1+FAC8(3,K),TBRIC(1,NV))
                     NN4=IXS(1+FAC8(4,K),TBRIC(1,NV))
                     CALL FVNORMAL(X,NN1,NN2,NN3,NN4,NSX2,NSY2,NSZ2)
                  ENDIF
C
                  SS=NSX*NSX2+NSY*NSY2+NSZ*NSZ2
                  IF (SS>=ZERO) THEN
                     CALL ANCMSG(MSGID=634,
     .                           MSGTYPE=MSGERROR,
     .                           ANMODE=ANINFO_BLIND_1,
     .                           I1=MONVID,
     .                           C1=TITR,
     .                           I2=IXS(NIXS,II))
                  ENDIF
                  GOTO 100
               ENDDO
 100        CONTINUE
C
               IF (NTYPE==2) THEN
                  DO K=1,3
                     KK=FAC4(K,J)
                     ITAG(IXS(1+KK,II))=0
                  ENDDO
               ELSEIF (NTYPE==3) THEN
                  DO K=1,NOD6(J)
                     KK=FAC6(K,J)
                     ITAG(IXS(1+KK,II))=0
                  ENDDO
               ELSEIF (NTYPE==4) THEN
                  DO K=1,NOD5(J)
                     KK=FAC5(K,J)
                     ITAG(IXS(1+KK,II))=0
                  ENDDO
               ELSEIF (NTYPE==1) THEN
                  DO K=1,4
                     KK=FAC8(K,J)
                     ITAG(IXS(1+KK,II))=0
                  ENDDO
               ENDIF
            ENDDO
         ENDDO
      ENDIF
C
C Ajout des triangles airbag et interne non appuye sur une brique
C
      DO I=1,NELT
         IF (TAGELS(I)==0) NELA=NELA+1
      ENDDO
C
      IF(NELA > 0) THEN
        WRITE(IOUT,'(/5X,A,I10/)') 'NUMBER OF AIRBAG TRIANGLES NOT CONNECTED TO A SOLID ELEMENT . .=',NELA
      ENDIF	  
C
C Noeuds auxiliaires
C
      DO I=1,NB_NODE
         ITAG(I)=0
      ENDDO
      DO I=1,NBRIC
         II=TBRIC(1,I)
         NTYPE=TBRIC(2,I)
         IF (NTYPE==2) THEN
            ITAG(IXS(1+1,II))=1
            ITAG(IXS(1+3,II))=1
            ITAG(IXS(1+6,II))=1
            ITAG(IXS(1+5,II))=1
         ELSEIF (NTYPE==3) THEN
            ITAG(IXS(1+1,II))=1
            ITAG(IXS(1+2,II))=1
            ITAG(IXS(1+3,II))=1
            ITAG(IXS(1+5,II))=1
            ITAG(IXS(1+6,II))=1
            ITAG(IXS(1+7,II))=1
         ELSEIF (NTYPE==4) THEN
            ITAG(IXS(1+1,II))=1
            ITAG(IXS(1+2,II))=1
            ITAG(IXS(1+3,II))=1
            ITAG(IXS(1+4,II))=1
            ITAG(IXS(1+5,II))=1
         ELSEIF (NTYPE==1) THEN
            DO J=1,8
               ITAG(IXS(1+J,II))=1
            ENDDO
         ENDIF
      ENDDO
      NNA=0
      DO I=1,NB_NODE
         IF (ITAG(I)==1) THEN
            NNA=NNA+1
         ENDIF
      ENDDO
      T_MONVOLN%NNA = NNA
      T_MONVOLN%NTGA = NELA
      ALLOCATE(T_MONVOLN%IBUFA(NNA))
      IF (NELA > 0) THEN
         ALLOCATE(T_MONVOLN%ELEMA(3, NELA))
         T_MONVOLN%ELEMA(:, :) = 0
         ALLOCATE(T_MONVOLN%TAGELA(NELA))
         T_MONVOLN%TAGELA(:) = 0
      ENDIF
      ALLOCATE(T_MONVOLN%BRNA(8, T_MONVOLN%NBRIC))
      T_MONVOLN%BRNA(:, :) = 0
      IF (NNA > 0) THEN
         ALLOCATE(T_MONVOLN%NCONA(16, NNA))
         T_MONVOLN%NCONA(:, :) = 0
      ENDIF

      NNA=0
      DO I=1,NB_NODE
         IF (ITAG(I)==1) THEN
            NNA=NNA+1
            T_MONVOLN%IBUFA(NNA)=I
         ENDIF
      ENDDO
C
      IF (ALLOCATED(ADDCNET)) DEALLOCATE(ADDCNET)
      IF (ALLOCATED(ADSKY)) DEALLOCATE(ADSKY)
      IF (ALLOCATED(CNT)) DEALLOCATE(CNT)
      RETURN
      END

