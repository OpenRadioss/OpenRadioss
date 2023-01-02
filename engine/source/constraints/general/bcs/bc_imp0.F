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
C   ------- only BC dans system global sont traites---------
Chd|====================================================================
Chd|  BC_IMP0                       source/constraints/general/bcs/bc_imp0.F
Chd|-- called by -----------
Chd|        UPD_GLOB_K                    source/implicit/upd_glob_k.F  
Chd|        UPD_INT_K                     source/implicit/upd_glob_k.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE BC_IMP0(ICODT ,ICODR,ISKEW,IFIX,NDOF,IADN )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICODT(*),ICODR(*),ISKEW(*),IFIX(*),
     .        NDOF(*),IADN(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, ISK,  ICT,ICR,J,K,NFIX,ND
C----------------BC-------------------------
Ccw   FIX X-DOF FOR 2D CASE
      IF (N2D/=0) THEN
        DO I = 1,NUMNOD
C         IF (ICODT(I)<4) THEN
C           ICODT(I) = ICODT(I) + 4
C         ENDIF
          IF (NDOF(I)>0) THEN
            ND = IADN(I)
            IFIX(ND+1) = 1
          ENDIF
        ENDDO
      ENDIF
      IF (IRODDL==0) THEN
      DO I = 1,NUMNOD
       ISK = ISKEW(I)
       IF (ISK==1) THEN
        ICT = ICODT(I)
        K = NDOF(I)
        ND = IADN(I)
        IF (ICT > 0 .AND. K> 0) THEN
         IF (ICT == 4 .AND. K>2) THEN
          IFIX(ND +1) = 1
         ELSEIF (ICT == 2) THEN
          IFIX(ND +2) = 1
         ELSEIF (ICT == 1) THEN
          IFIX(ND +3) = 1
         ELSEIF (ICT == 3) THEN
          IFIX(ND +2) = 1
          IFIX(ND +3) = 1
         ELSEIF (ICT == 5) THEN
          IF (K>2) IFIX(ND +1) = 1
          IFIX(ND +3) = 1
         ELSEIF (ICT == 6) THEN
          IF (K>2) IFIX(ND +1) = 1
          IFIX(ND +2) = 1
         ELSEIF (ICT == 7) THEN
          IF (K>2) IFIX(ND +1) = 1
          IFIX(ND +2) = 1
          IFIX(ND +3) = 1
         ENDIF
        ENDIF
       ENDIF
      ENDDO
      ELSE
      DO I = 1,NUMNOD
       ISK = ISKEW(I)
       IF (ISK==1) THEN
        ICT = ICODT(I)
        ICR = ICODR(I)
        K = NDOF(I)
        ND = IADN(I)
        IF (ICT > 0 .AND. K> 0) THEN
         IF (ICT == 4 .AND. K>2) THEN
          IFIX(ND +1) = 1
         ELSEIF (ICT == 2) THEN
          IFIX(ND +2) = 1
         ELSEIF (ICT == 1) THEN
          IFIX(ND +3) = 1
         ELSEIF (ICT == 3) THEN
          IFIX(ND +2) = 1
          IFIX(ND +3) = 1
         ELSEIF (ICT == 5) THEN
          IF (K>2) IFIX(ND +1) = 1
          IFIX(ND +3) = 1
         ELSEIF (ICT == 6) THEN
          IF (K>2) IFIX(ND +1) = 1
          IFIX(ND +2) = 1
         ELSEIF (ICT == 7) THEN
          IF (K>2) IFIX(ND +1) = 1
          IFIX(ND +2) = 1
          IFIX(ND +3) = 1
         ENDIF
        ENDIF
        IF (ICR > 0 .AND. K==6) THEN
         IF (ICR == 1) THEN
          IFIX(ND +6) = 1
         ELSEIF (ICR == 2) THEN
          IFIX(ND +5) = 1
         ELSEIF (ICR == 3) THEN
          IFIX(ND +5) = 1
          IFIX(ND +6) = 1
         ELSEIF (ICR == 4) THEN
          IFIX(ND +4) = 1
         ELSEIF (ICR == 5) THEN
          IFIX(ND +4) = 1
          IFIX(ND +6) = 1
         ELSEIF (ICR == 6) THEN
          IFIX(ND +4) = 1
          IFIX(ND +5) = 1
         ELSEIF (ICR == 7) THEN
          IFIX(ND +4) = 1
          IFIX(ND +5) = 1
          IFIX(ND +6) = 1
         ENDIF
        ENDIF
       ENDIF
      ENDDO
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  BC_IMP1                       source/constraints/general/bcs/bc_imp0.F
Chd|-- called by -----------
Chd|        UPD_GLOB_K                    source/implicit/upd_glob_k.F  
Chd|        UPD_INT_K                     source/implicit/upd_glob_k.F  
Chd|-- calls ---------------
Chd|        BCL_IMPK                      source/constraints/general/bcs/bc_imp0.F
Chd|====================================================================
      SUBROUTINE BC_IMP1(ICODT ,ICODR ,ISKEW ,SKEW  ,IFIX  ,
     1                   NDOF  ,IADN  ,IADK  ,JDIK  ,DIAG_K,
     2                   LT_K  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICODT(*),ICODR(*),ISKEW(*),IFIX(*),
     .        NDOF(*),IADN(*),IADK(*) ,JDIK(*)
      my_real
     .   SKEW(LSKEW,*),DIAG_K(*),LT_K(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, ISK,  ICT,ICR,J,K,ND,IR,IT,IFIX_CP(6)
C----------------BC-------------------------
         IT = 0
         IR = 1
      IF (IRODDL==0) THEN
      DO I = 1,NUMNOD
       ISK = ISKEW(I)
       IF (ISK>1) THEN
        ICT = IABS(ICODT(I))
        K = NDOF(I)
        ND = IADN(I)
C---------<0 with FV coupling, not to change IFIX		
        IF (ICODT(I)<0) THEN
         DO J =1,3
          IFIX_CP(J)=IFIX(ND+J)
         END DO
        END IF
        IF (ICT > 0 .AND. K> 0) THEN
         CALL BCL_IMPK(ICT  ,ISK   ,SKEW  ,IFIX  ,IADN  ,
     1                IADK  ,JDIK  ,DIAG_K,LT_K  ,
     2                I     ,ND    ,IT    )
        ENDIF
        IF (ICODT(I)<0) THEN
         DO J =1,3
          IFIX(ND+J)=IFIX_CP(J)
         END DO
        END IF
       ENDIF
      ENDDO
      ELSE
      DO I = 1,NUMNOD
       ISK = ISKEW(I)
       IF (ISK>1) THEN
        ICT = IABS(ICODT(I))
        ICR = IABS(ICODR(I))
        K = NDOF(I)
        ND = IADN(I)
        IF (ICODT(I)<0) THEN
         DO J =1,3
          IFIX_CP(J)=IFIX(ND+J)
         END DO
        END IF
        IF (ICT > 0 .AND. K> 0) THEN
         CALL BCL_IMPK(ICT  ,ISK   ,SKEW  ,IFIX  ,IADN  ,
     1                IADK  ,JDIK  ,DIAG_K,LT_K  ,
     2                I     ,ND    ,IT    )
        ENDIF
        IF (ICODT(I)<0) THEN
         DO J =1,3
          IFIX(ND+J)=IFIX_CP(J)
         END DO
        END IF
        IF (ICODR(I)<0) THEN
         DO J =4,6
          IFIX_CP(J)=IFIX(ND+J)
         END DO
        END IF
        IF (ICR > 0 .AND. K==6) THEN
         ND = ND + 3
         CALL BCL_IMPK(ICR  ,ISK   ,SKEW  ,IFIX  ,IADN  ,
     1                IADK  ,JDIK  ,DIAG_K,LT_K  ,
     2                I     ,ND    ,IR    )
        ENDIF
        IF (ICODR(I)<0) THEN
         DO J =4,6
          IFIX(ND+J) = IFIX_CP(J)
         END DO
        END IF
       ENDIF
      ENDDO
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  BCL_IMPK                      source/constraints/general/bcs/bc_imp0.F
Chd|-- called by -----------
Chd|        BC_IMP1                       source/constraints/general/bcs/bc_imp0.F
Chd|-- calls ---------------
Chd|        BC_UPDK                       source/constraints/general/bcs/bc_imp0.F
Chd|        BC_UPDK2D                     source/constraints/general/bcs/bc_imp0.F
Chd|        L_DIR2                        source/constraints/general/bcs/bc_imp0.F
Chd|====================================================================
      SUBROUTINE BCL_IMPK(ICT  ,ISK   ,SKEW  ,IFIX  ,IADN  ,
     1                   IADK  ,JDIK  ,DIAG_K,LT_K  ,
     2                   I     ,ND    ,IR    )
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
      INTEGER ICT,IFIX(*),IADN(*),IADK(*) ,JDIK(*),
     .        I, ND,ISK,IR
      my_real
     .   SKEW(LSKEW,*),DIAG_K(*),LT_K(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J,K,J1,L,KC
      my_real
     .   EJ(3)
C----------------BC-------------------------
         KC = 8
         IF (ICT == 4 ) THEN
          EJ(1)=SKEW(1,ISK)
          EJ(2)=SKEW(2,ISK)
          EJ(3)=SKEW(3,ISK)
          CALL L_DIR2(EJ,J,1)
          CALL BC_UPDK(I     ,IADN  ,EJ    ,J     ,IR    ,
     1                 IADK  ,JDIK  ,DIAG_K,LT_K  )
          IFIX(ND +J) = KC
         ELSEIF (ICT == 2) THEN
          EJ(1)=SKEW(4,ISK)
          EJ(2)=SKEW(5,ISK)
          EJ(3)=SKEW(6,ISK)
          CALL L_DIR2(EJ,J,2)
          CALL BC_UPDK(I     ,IADN  ,EJ    ,J     ,IR    ,
     1                 IADK  ,JDIK  ,DIAG_K,LT_K  )
          IFIX(ND +J) = KC
         ELSEIF (ICT == 1) THEN
          EJ(1)=SKEW(7,ISK)
          EJ(2)=SKEW(8,ISK)
          EJ(3)=SKEW(9,ISK)
          CALL L_DIR2(EJ,J,3)
          CALL BC_UPDK(I     ,IADN  ,EJ    ,J     ,IR    ,
     1                 IADK  ,JDIK  ,DIAG_K,LT_K  )
          IFIX(ND +J) = KC
         ELSEIF (ICT == 3) THEN
C
          CALL BC_UPDK2D(IADN  ,IFIX(ND+1),SKEW(7,ISK),SKEW(4,ISK),
     1                   I     ,IR    ,KC    ,IADK  ,JDIK  ,DIAG_K,
     2                   LT_K  )
         ELSEIF (ICT == 5) THEN
          CALL BC_UPDK2D(IADN  ,IFIX(ND+1),SKEW(7,ISK),SKEW(1,ISK),
     1                   I     ,IR    ,KC    ,IADK  ,JDIK  ,DIAG_K,
     2                   LT_K  )
         ELSEIF (ICT == 6) THEN
C
          CALL BC_UPDK2D(IADN  ,IFIX(ND+1),SKEW(4,ISK),SKEW(1,ISK),
     1                   I     ,IR    ,KC    ,IADK  ,JDIK  ,DIAG_K,
     2                   LT_K  )
         ELSEIF (ICT == 7) THEN
          IFIX(ND +1) = 1
          IFIX(ND +2) = 1
          IFIX(ND +3) = 1
         ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  L_DIR0                        source/constraints/general/bcs/bc_imp0.F
Chd|-- called by -----------
Chd|        FV_RWL                        source/constraints/general/rwall/srw_imp.F
Chd|        L_DIR                         source/constraints/general/bcs/bc_imp0.F
Chd|        L_DIR02                       source/constraints/general/impvel/fv_imp0.F
Chd|        L_DIR2                        source/constraints/general/bcs/bc_imp0.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE L_DIR0(EJ ,J)
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
      INTEGER J
      my_real
     .   EJ(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   EJ1,EJ2,EJ3
C-----------------------------------------------
      EJ1 = ABS(EJ(1))
      EJ2 = ABS(EJ(2))
      EJ3 = ABS(EJ(3))
      IF (EJ1>=MAX(EJ2,EJ3)) THEN
       J = 1
      ELSEIF (EJ2>=MAX(EJ1,EJ3)) THEN
       J = 2
      ELSE
       J = 3
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  L_DIR                         source/constraints/general/bcs/bc_imp0.F
Chd|-- called by -----------
Chd|        BC_IMP2                       source/constraints/general/bcs/bc_imp0.F
Chd|        BC_IMPA                       source/constraints/general/bcs/bc_imp0.F
Chd|        BC_IMPR1                      source/constraints/general/bcs/bc_imp0.F
Chd|        CDI_BCN                       source/constraints/general/rbe2/rbe2_imp0.F
Chd|        CDI_BCN1                      source/constraints/general/rbe2/rbe2_imp0.F
Chd|        DIAG_INT                      source/mpi/implicit/imp_fri.F 
Chd|        FR_U2DD                       source/mpi/implicit/imp_fri.F 
Chd|        FV_RWL0                       source/constraints/general/rwall/srw_imp.F
Chd|        FV_RWLR0                      source/constraints/general/rwall/srw_imp.F
Chd|        IMP3_A2B                      source/airbag/monv_imp0.F     
Chd|        IMP3_U2X                      source/airbag/monv_imp0.F     
Chd|        RBE2D_BCL                     source/constraints/general/rbe2/rbe2v.F
Chd|        RBE2FLSN                      source/constraints/general/rbe2/rbe2f.F
Chd|        RBE2FLSNFR                    source/constraints/general/rbe2/rbe2f.F
Chd|        RBE2IMPBSN                    source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE2_BCL                      source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RWL_IMPD                      source/constraints/general/rwall/srw_imp.F
Chd|        SELECT_DOF                    source/constraints/general/rbe2/rbe2v.F
Chd|        UPDK_MV                       source/airbag/monv_imp0.F     
Chd|        UPD_FR                        source/mpi/implicit/imp_fri.F 
Chd|        UPD_KML                       source/mpi/implicit/imp_fri.F 
Chd|        UPD_KSL                       source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        L_DIR0                        source/constraints/general/bcs/bc_imp0.F
Chd|====================================================================
      SUBROUTINE L_DIR(EJ ,J)
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
      INTEGER J
      my_real
     .   EJ(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   MAX_E
C-----------------------------------------------
      CALL L_DIR0(EJ ,J)
      MAX_E = ONE/EJ(J)
      DO I = 1, 3
       EJ(I) = MAX_E*EJ(I)
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  L_DIR2                        source/constraints/general/bcs/bc_imp0.F
Chd|-- called by -----------
Chd|        BCL_FRK                       source/constraints/general/bcs/bc_imp0.F
Chd|        BCL_IMPB                      source/constraints/general/bcs/bc_imp0.F
Chd|        BCL_IMPD                      source/constraints/general/bcs/bc_imp0.F
Chd|        BCL_IMPK                      source/constraints/general/bcs/bc_imp0.F
Chd|        BCL_IMPKD                     source/constraints/general/bcs/bc_imp0.F
Chd|        BC_UPDF                       source/constraints/general/bcs/bc_imp0.F
Chd|        GETBCL_J                      source/constraints/general/impvel/fv_imp0.F
Chd|-- calls ---------------
Chd|        L_DIR0                        source/constraints/general/bcs/bc_imp0.F
Chd|====================================================================
      SUBROUTINE L_DIR2(EJ ,J   ,J0)
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
      INTEGER J,J0
      my_real
     .   EJ(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   MAX_E
C-----------------------------------------------
C------- will be same than L_DIR, just not to change so much lines--
      IF (J0<0 )THEN
       IF (ABS(EJ(J0))>EM6) THEN
        J=J0
       ELSE
        CALL L_DIR0(EJ ,J)
       ENDIF
      ELSE
       CALL L_DIR0(EJ ,J)
      ENDIF
      MAX_E = ONE/EJ(J)
      DO I = 1, 3
       EJ(I) = MAX_E*EJ(I)
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  BC_UPDK                       source/constraints/general/bcs/bc_imp0.F
Chd|-- called by -----------
Chd|        BCL_IMPK                      source/constraints/general/bcs/bc_imp0.F
Chd|        BC_IMPA                       source/constraints/general/bcs/bc_imp0.F
Chd|        FV_UPDK                       source/constraints/general/impvel/fv_imp0.F
Chd|        RBE2_BCL                      source/constraints/general/rbe2/rbe2_imp0.F
Chd|-- calls ---------------
Chd|        GET_KII                       source/implicit/imp_glob_k.F  
Chd|        PUT_KII                       source/implicit/imp_glob_k.F  
Chd|====================================================================
      SUBROUTINE BC_UPDK(N     ,IDDL  ,EJ    ,JJ    ,IR    ,
     1                   IADK  ,JDIK  ,DIAG_K,LT_K  )
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
      INTEGER N,JJ,IDDL(*),IR,IADK(*) ,JDIK(*)
      my_real
     .   EJ(*),DIAG_K(*),LT_K(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ND,K,L,J1,K1,L1,ID,SHF,JFT,KFT,LFT,NL,NJ,
     .        IT(6),KK
      my_real
     .   KDD(6,6),KII(6,6)
C-----------------------------------------------
      IF (IR==0) THEN
       ND = 3
      ELSE
       ND = 6
      ENDIF
      K = JJ + 1
      L = JJ + 2
      IF (K>3) K = K - 3
      IF (L>3) L = L - 3
      IF (EJ(K)==ZERO.AND.EJ(L)==ZERO) RETURN
      DO I=1,ND
      DO J=1,ND
       KII(I,J)=ZERO
      ENDDO
      ENDDO
      IF (IR==0) THEN
       J1 = JJ
       K1 = K
       L1 = L
      ELSE
       J1 = JJ + 3
       K1 = K + 3
       L1 = L + 3
      ENDIF
      CALL GET_KII(N ,IDDL ,IADK,DIAG_K,LT_K ,KDD,ND)
      DO I=1,ND
      DO J=I+1,ND
       KDD(J,I)=KDD(I,J)
      ENDDO
      ENDDO
      KII(K1,K1)=-(TWO*KDD(K1,J1)-KDD(J1,J1)*EJ(K))*EJ(K)
      KII(L1,L1)=-(TWO*KDD(L1,J1)-KDD(J1,J1)*EJ(L))*EJ(L)
      KII(L1,K1)=-KDD(L1,J1)*EJ(K)-KDD(K1,J1)*EJ(L)
     1           +KDD(J1,J1)*EJ(L)*EJ(K)
      KII(K1,L1)=KII(L1,K1)
      IF (IR/=0) THEN
       J = JJ
       KII(J,K1)=-KDD(J,J1)*EJ(K)
       KII(K,K1)=-KDD(K,J1)*EJ(K)
       KII(L,K1)=-KDD(L,J1)*EJ(K)
       KII(J,L1)=-KDD(J,J1)*EJ(L)
       KII(K,L1)=-KDD(K,J1)*EJ(L)
       KII(L,L1)=-KDD(L,J1)*EJ(L)
      ENDIF
      CALL PUT_KII(N ,IDDL ,IADK,DIAG_K,LT_K ,KII,ND)
      ID = IDDL(N)+ J1
      IF (IKPAT==0) THEN
        SHF = IABS(JJ-3)
        NL = IADK(ID+1)-IADK(ID)-SHF
        JFT = IADK(ID)+SHF-1
        KFT = IADK(IDDL(N)+ K1)+IABS(K-3)-1
        LFT = IADK(IDDL(N)+ L1)+IABS(L-3)-1
        DO J = 1, NL
         LT_K(KFT+J) = LT_K(KFT+J)-EJ(K)*LT_K(JFT+J)
         LT_K(LFT+J) = LT_K(LFT+J)-EJ(L)*LT_K(JFT+J)
        ENDDO
       DO I = 1, IDDL(N)
        NJ =0
        DO J = IADK(I), IADK(I+1)-1
         IF (JDIK(J)==ID) NJ = J
        ENDDO
        IF (NJ>0) THEN
         LT_K(NJ+K1-J1) = LT_K(NJ+K1-J1)-EJ(K)*LT_K(NJ)
         LT_K(NJ+L1-J1) = LT_K(NJ+L1-J1)-EJ(L)*LT_K(NJ)
        ENDIF
       ENDDO
      ELSE
        SHF = J1-1
        NL = IADK(ID+1)-IADK(ID)-SHF
        JFT = IADK(ID)-1
        KFT = IADK(IDDL(N)+K1)-1
        LFT = IADK(IDDL(N)+L1)-1
        DO J = 1, NL
         LT_K(KFT+J) = LT_K(KFT+J)-EJ(K)*LT_K(JFT+J)
         LT_K(LFT+J) = LT_K(LFT+J)-EJ(L)*LT_K(JFT+J)
        ENDDO
C---------ajoute NDDL_L dans impl1_c.inc
       DO I = IDDL(N)+ND+1, NDDL_L
        NJ =0
        DO J = IADK(I), IADK(I+1)-1
         IF (JDIK(J)==ID) NJ = J
        ENDDO
        IF (NJ>0) THEN
         LT_K(NJ+K1-J1) = LT_K(NJ+K1-J1)-EJ(K)*LT_K(NJ)
         LT_K(NJ+L1-J1) = LT_K(NJ+L1-J1)-EJ(L)*LT_K(NJ)
        ENDIF
       ENDDO
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  BC_IMP2                       source/constraints/general/bcs/bc_imp0.F
Chd|-- called by -----------
Chd|        RECUKIN                       source/implicit/recudis.F     
Chd|-- calls ---------------
Chd|        BCL_IMPD                      source/constraints/general/bcs/bc_imp0.F
Chd|        BC_UPD2D                      source/constraints/general/bcs/bc_imp0.F
Chd|        BC_UPDD                       source/constraints/general/bcs/bc_imp0.F
Chd|        L_DIR                         source/constraints/general/bcs/bc_imp0.F
Chd|        IMP_ASPC                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE BC_IMP2(ICODT ,ICODR ,ISKEW ,SKEW  ,NDOF   ,
     1                   D     ,DR    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_ASPC
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICODT(*),ICODR(*),ISKEW(*),NDOF(*)
      my_real
     .   SKEW(LSKEW,*),D(3,*),DR(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, ISK,  ICT,ICR,J,K,IAD,IR,N,NN
      my_real
     .   EJ(3)
C----------------BC-------------------------
      IF (IRODDL==0) THEN
      DO I = 1,NUMNOD
       ISK = ISKEW(I)
       IF (ISK>1) THEN
        ICT = ICODT(I)
        K = NDOF(I)
        IF (ICT /= 0 .AND. K> 0) THEN
         CALL BCL_IMPD(ICT  ,ISK   ,SKEW  ,I     ,D     )
        ENDIF
       ENDIF
      ENDDO
      ELSE
      DO I = 1,NUMNOD
       ISK = ISKEW(I)
       IF (ISK>1) THEN
        ICT = ICODT(I)
        ICR = ICODR(I)
        K = NDOF(I)
        IF (ICT /= 0 .AND. K> 0) THEN
         CALL BCL_IMPD(ICT  ,ISK   ,SKEW  ,I     ,D     )
        ENDIF
        IF (ICR /= 0 .AND. K==6) THEN
         CALL BCL_IMPD(ICR  ,ISK   ,SKEW  ,I     ,DR    )
        ENDIF
       ENDIF
      ENDDO
      ENDIF
C--------AUTOSPC---------------------------------------
       DO N = NSPCL,1 ,-1
        I = IN_SPC(N)
        IF (NDOF(I)==0) CYCLE
        IR = 0
        IAD = 6*(N-1)+1
        NN = IC_SPC(N)
        IF (NN>3) THEN
         NN= NN-3
         IR = 1
        ENDIF
        IF (NN==1) THEN
         EJ(1)=SKEW_SPC(IAD)
         EJ(2)=SKEW_SPC(IAD+1)
         EJ(3)=SKEW_SPC(IAD+2)
             CALL L_DIR(EJ,J)
        END IF
        IF (IR==0) THEN
         IF (NN==1) THEN
              D(J,I) = ZERO
              CALL BC_UPDD(I     ,EJ   ,J     ,D     )
         ELSEIF (NN==2) THEN
              CALL BC_UPD2D(I     ,SKEW_SPC(IAD),SKEW_SPC(IAD+3),D    )
         END IF
        ELSE
         IF (NN==1) THEN
              DR(J,I) = ZERO
              CALL BC_UPDD(I     ,EJ   ,J     ,DR    )
         ELSEIF (NN==2) THEN
              CALL BC_UPD2D(I     ,SKEW_SPC(IAD),SKEW_SPC(IAD+3),DR   )
         END IF
        ENDIF
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  BCL_IMPD                      source/constraints/general/bcs/bc_imp0.F
Chd|-- called by -----------
Chd|        BC_IMP2                       source/constraints/general/bcs/bc_imp0.F
Chd|        FR_U2DD                       source/mpi/implicit/imp_fri.F 
Chd|        IMP3_U2X                      source/airbag/monv_imp0.F     
Chd|-- calls ---------------
Chd|        BC_C2D                        source/constraints/general/bcs/bc_imp0.F
Chd|        BC_UPDD                       source/constraints/general/bcs/bc_imp0.F
Chd|        DIR_RBE2                      source/constraints/general/rbe2/rbe2v.F
Chd|        L_DIR2                        source/constraints/general/bcs/bc_imp0.F
Chd|====================================================================
      SUBROUTINE BCL_IMPD(ICT  ,ISK   ,SKEW  ,I     ,D     )
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
      INTEGER ICT,I, ISK
      my_real
     .   SKEW(LSKEW,*),D(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J,K,J1,L
      my_real
     .   EJ(3),EJ1(3),MAX_E,EA,EB
C----------------BC-------negative ICT only possible with 2 dirs------------------
         IF (ICT == 4 ) THEN
          EJ(1)=SKEW(1,ISK)
          EJ(2)=SKEW(2,ISK)
          EJ(3)=SKEW(3,ISK)
          CALL L_DIR2(EJ,J,1)
          D(J,I) = ZERO
          CALL BC_UPDD(I     ,EJ    ,J     ,D     )
         ELSEIF (ICT == 2) THEN
          EJ(1)=SKEW(4,ISK)
          EJ(2)=SKEW(5,ISK)
          EJ(3)=SKEW(6,ISK)
          CALL L_DIR2(EJ,J,2)
          D(J,I) = ZERO
          CALL BC_UPDD(I     ,EJ    ,J     ,D     )
         ELSEIF (ICT == 1) THEN
          EJ(1)=SKEW(7,ISK)
          EJ(2)=SKEW(8,ISK)
          EJ(3)=SKEW(9,ISK)
          CALL L_DIR2(EJ,J,3)
          D(J,I) = ZERO
          CALL BC_UPDD(I     ,EJ    ,J     ,D     )
         ELSEIF (IABS(ICT) == 3) THEN
          EJ(1)=SKEW(7,ISK)
          EJ(2)=SKEW(8,ISK)
          EJ(3)=SKEW(9,ISK)
C          CALL L_DIR2(EJ,J,3)
          EJ1(1)=SKEW(4,ISK)
          EJ1(2)=SKEW(5,ISK)
          EJ1(3)=SKEW(6,ISK)
          CALL BC_C2D(EJ,EJ1, J, J1  ,EA, EB  )
          CALL DIR_RBE2(J, J1  ,K)
C-----FV-BCS coupling		  
          IF (ICT<0) THEN
           D(J,I) =D(J,I)- EA*D(K,I)
           D(J1,I) =D(J1,I)- EB*D(K,I)
          ELSE
           D(J,I) = -EA*D(K,I)
           D(J1,I) = -EB*D(K,I)
          END IF !(ICT>0) THEN
C          CALL L_DIR2(EJ1,J1,2)
c          IF (J1==J) THEN
c           EJ1(J)=ZERO
c           CALL L_DIR(EJ1,J1)
c           MAX_E=ONE/SKEW(J1+3,ISK)
c           DO K = 1, 3
c            EJ1(K) = MAX_E*SKEW(K+3,ISK)
c           ENDDO
c          ENDIF
c          D(J,I) = ZERO
c          D(J1,I) = ZERO
c          CALL BC_UPDD2(I     ,EJ    ,J     ,EJ1    ,J1     ,D     )
         ELSEIF (IABS(ICT) == 5) THEN
          EJ(1)=SKEW(7,ISK)
          EJ(2)=SKEW(8,ISK)
          EJ(3)=SKEW(9,ISK)
c          CALL L_DIR2(EJ,J,3)
          EJ1(1)=SKEW(1,ISK)
          EJ1(2)=SKEW(2,ISK)
          EJ1(3)=SKEW(3,ISK)
c          CALL L_DIR2(EJ1,J1,1)
          CALL BC_C2D(EJ,EJ1, J, J1  ,EA, EB  )
          CALL DIR_RBE2(J, J1  ,K)
          IF (ICT<0) THEN
           D(J,I) =D(J,I)- EA*D(K,I)
           D(J1,I) =D(J1,I)- EB*D(K,I)
          ELSE
           D(J,I) = -EA*D(K,I)
           D(J1,I) = -EB*D(K,I)
          END IF !(ICT>0) THEN
         ELSEIF (IABS(ICT) == 6) THEN
          EJ(1)=SKEW(4,ISK)
          EJ(2)=SKEW(5,ISK)
          EJ(3)=SKEW(6,ISK)
c          CALL L_DIR2(EJ,J,2)
          EJ1(1)=SKEW(1,ISK)
          EJ1(2)=SKEW(2,ISK)
          EJ1(3)=SKEW(3,ISK)
          CALL BC_C2D(EJ,EJ1, J, J1  ,EA, EB  )
          CALL DIR_RBE2(J, J1  ,K)
          IF (ICT<0) THEN
           D(J,I) =D(J,I)- EA*D(K,I)
           D(J1,I) =D(J1,I)- EB*D(K,I)
          ELSE
           D(J,I) = -EA*D(K,I)
           D(J1,I) = -EB*D(K,I)
          END IF !(ICT>0) THEN
         ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  BC_UPDD                       source/constraints/general/bcs/bc_imp0.F
Chd|-- called by -----------
Chd|        BCL_IMPD                      source/constraints/general/bcs/bc_imp0.F
Chd|        BC_IMP2                       source/constraints/general/bcs/bc_imp0.F
Chd|        FR_U2DD                       source/mpi/implicit/imp_fri.F 
Chd|        FV_IMPD                       source/constraints/general/impvel/fv_imp0.F
Chd|        IMP3_U2X                      source/airbag/monv_imp0.F     
Chd|        RWL_IMPD                      source/constraints/general/rwall/srw_imp.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE BC_UPDD(N     ,EJ    ,J     ,D     )
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
      INTEGER N,J
      my_real
     .   EJ(*),D(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ND,K,L
C-----------------------------------------------
      K = J + 1
      L = J + 2
      IF (K>3) K = K - 3
      IF (L>3) L = L - 3
      D(J,N) = D(J,N)- EJ(K)* D(K,N)-EJ(L)* D(L,N)
C
      RETURN
      END
Chd|====================================================================
Chd|  BC_UPDD2                      source/constraints/general/bcs/bc_imp0.F
Chd|-- called by -----------
Chd|        FV_IMPD                       source/constraints/general/impvel/fv_imp0.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE BC_UPDD2(N     ,EJ    ,J    ,EJ1    ,J1    ,D     )
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
      INTEGER N,J,J1
      my_real
     .   EJ(*),EJ1(*),D(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,L
      my_real
     .   S
C-----------------------------------------------
      K = J + 1
      IF (K>3) K = K - 3
      IF (K==J1) THEN
       K = J + 2
       IF (K>3) K = K - 3
      ENDIF
      S =ONE-EJ1(J)*EJ(J1)
      D(J1,N) = ( D(J1,N)-EJ1(J)*D(J,N)+
     .            (EJ1(J)*EJ(K)-EJ1(K))*D(K,N) )/S
      D(J,N) = D(J,N)- EJ(K)* D(K,N)-EJ(J1)* D(J1,N)
C
      RETURN
      END
Chd|====================================================================
Chd|  BCL_IMPKD                     source/constraints/general/bcs/bc_imp0.F
Chd|-- called by -----------
Chd|        DIAG_INT                      source/mpi/implicit/imp_fri.F 
Chd|        RBE2_IMPKD                    source/constraints/general/rbe2/rbe2_imp0.F
Chd|        UPDK_MV                       source/airbag/monv_imp0.F     
Chd|-- calls ---------------
Chd|        FV_UPDKD                      source/constraints/general/impvel/fv_imp0.F
Chd|        FV_UPDKD2                     source/constraints/general/bcs/bc_imp0.F
Chd|        L_DIR2                        source/constraints/general/bcs/bc_imp0.F
Chd|====================================================================
      SUBROUTINE BCL_IMPKD(ICT  ,ISK   ,SKEW  ,KDD   ,DIAG_K  )
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
      INTEGER ICT,ISK
      my_real
     .   SKEW(LSKEW,*),DIAG_K(*),KDD(3,3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J,K,J1,L
      my_real
     .   EJ(3)
C----------------BC-------------------------
         IF (ICT == 4 ) THEN
          EJ(1)=SKEW(1,ISK)
          EJ(2)=SKEW(2,ISK)
          EJ(3)=SKEW(3,ISK)
          CALL L_DIR2(EJ,J,1)
          CALL FV_UPDKD(EJ    ,J    ,KDD   ,DIAG_K)
         ELSEIF (ICT == 2) THEN
          EJ(1)=SKEW(4,ISK)
          EJ(2)=SKEW(5,ISK)
          EJ(3)=SKEW(6,ISK)
          CALL L_DIR2(EJ,J,2)
          CALL FV_UPDKD(EJ    ,J    ,KDD   ,DIAG_K)
         ELSEIF (ICT == 1) THEN
          EJ(1)=SKEW(7,ISK)
          EJ(2)=SKEW(8,ISK)
          EJ(3)=SKEW(9,ISK)
          CALL L_DIR2(EJ,J,3)
          CALL FV_UPDKD(EJ    ,J    ,KDD   ,DIAG_K)
         ELSEIF (ICT == 3) THEN
          CALL FV_UPDKD2(SKEW(7,ISK),SKEW(4,ISK),KDD   ,DIAG_K)
         ELSEIF (ICT == 5) THEN
          CALL FV_UPDKD2(SKEW(7,ISK),SKEW(1,ISK),KDD   ,DIAG_K)
         ELSEIF (ICT == 6) THEN
          CALL FV_UPDKD2(SKEW(4,ISK),SKEW(1,ISK),KDD   ,DIAG_K)
         ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  BC_UPDF                       source/constraints/general/bcs/bc_imp0.F
Chd|-- called by -----------
Chd|        IMP3_A2B                      source/airbag/monv_imp0.F     
Chd|        UPD_FR                        source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        BC_FI                         source/constraints/general/bcs/bc_imp0.F
Chd|        BC_FI2                        source/constraints/general/bcs/bc_imp0.F
Chd|        L_DIR2                        source/constraints/general/bcs/bc_imp0.F
Chd|====================================================================
      SUBROUTINE BC_UPDF(NBC   ,IBC   ,SKEW  ,A     )
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
      INTEGER NBC   ,IBC(3,*)
      my_real
     .  A(3,*),SKEW(LSKEW,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,K,L,J1,K1,L1,K2,K3,II,ISK,ICT
      my_real
     .   EJ(3)
C-----------------------------------------------
        DO II=1,NBC
         N = IBC(1,II)
         ISK = IBC(2,II)
         ICT = IBC(3,II)
         IF (ICT == 4 ) THEN
          EJ(1)=SKEW(1,ISK)
          EJ(2)=SKEW(2,ISK)
          EJ(3)=SKEW(3,ISK)
          CALL L_DIR2(EJ,J,1)
          CALL BC_FI(N    ,EJ    ,J    ,A     )
         ELSEIF (ICT == 2) THEN
          EJ(1)=SKEW(4,ISK)
          EJ(2)=SKEW(5,ISK)
          EJ(3)=SKEW(6,ISK)
          CALL L_DIR2(EJ,J,2)
          CALL BC_FI(N    ,EJ    ,J    ,A     )
         ELSEIF (ICT == 1) THEN
          EJ(1)=SKEW(7,ISK)
          EJ(2)=SKEW(8,ISK)
          EJ(3)=SKEW(9,ISK)
          CALL L_DIR2(EJ,J,3)
          CALL BC_FI(N    ,EJ    ,J    ,A     )
         ELSEIF (ICT == 3) THEN
          CALL BC_FI2(N    ,SKEW(7,ISK),SKEW(4,ISK),A     )
         ELSEIF (ICT == 5) THEN
          CALL BC_FI2(N    ,SKEW(7,ISK),SKEW(1,ISK),A     )
         ELSEIF (ICT == 6) THEN
          CALL BC_FI2(N    ,SKEW(4,ISK),SKEW(1,ISK),A     )
         ENDIF
        ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  BC_FI                         source/constraints/general/bcs/bc_imp0.F
Chd|-- called by -----------
Chd|        BC_UPDF                       source/constraints/general/bcs/bc_imp0.F
Chd|        IMP3_A2B                      source/airbag/monv_imp0.F     
Chd|        UPD_FR                        source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE BC_FI(N    ,EJ    ,J1    ,A     )
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
      INTEGER J1,N
      my_real
     .  A(3,*),EJ(3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K,L
C-----------------------------------------------
          K = J1 + 1
          IF (K>3) K = K - 3
          L = J1 + 2
          IF (L>3) L = L - 3
          A(K,N)=A(K,N)-EJ(K)*A(J1,N)
          A(L,N)=A(L,N)-EJ(L)*A(J1,N)
C
      RETURN
      END
Chd|====================================================================
Chd|  BC_UPDB                       source/constraints/general/bcs/bc_imp0.F
Chd|-- called by -----------
Chd|        BCL_IMPB                      source/constraints/general/bcs/bc_imp0.F
Chd|        BC_IMPR1                      source/constraints/general/bcs/bc_imp0.F
Chd|        FV_IMPRL                      source/constraints/general/impvel/fv_imp0.F
Chd|        FV_RWLR0                      source/constraints/general/rwall/srw_imp.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE BC_UPDB(ID    ,EJ    ,JJ    ,IR    ,LB    )
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
      INTEGER ID,JJ,IR
      my_real
     .   EJ(*),LB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ND,K,L,J1,K1,L1
C-----------------------------------------------
      IF (IR==0) THEN
       ND = 3
      ELSE
       ND = 6
      ENDIF
      K = JJ + 1
      L = JJ + 2
      IF (K>3) K = K - 3
      IF (L>3) L = L - 3
      IF (EJ(K)==ZERO.AND.EJ(L)==ZERO) RETURN
      IF (IR==0) THEN
       J1 = JJ
       K1 = K
       L1 = L
      ELSE
       J1 = JJ + 3
       K1 = K + 3
       L1 = L + 3
      ENDIF
      LB(ID+K1)=LB(ID+K1)-EJ(K)*LB(ID+J1)
      LB(ID+L1)=LB(ID+L1)-EJ(L)*LB(ID+J1)
C
      RETURN
      END
Chd|====================================================================
Chd|  BC_IMPR1                      source/constraints/general/bcs/bc_imp0.F
Chd|-- called by -----------
Chd|        EXT_RHS                       source/implicit/upd_glob_k.F  
Chd|        UPD_RHS                       source/implicit/upd_glob_k.F  
Chd|-- calls ---------------
Chd|        BCL_IMPB                      source/constraints/general/bcs/bc_imp0.F
Chd|        BC_UPDB                       source/constraints/general/bcs/bc_imp0.F
Chd|        BC_UPDF2D                     source/constraints/general/bcs/bc_imp0.F
Chd|        L_DIR                         source/constraints/general/bcs/bc_imp0.F
Chd|        IMP_ASPC                      share/modules/impbufdef_mod.F 
Chd|        IMP_FVBCL                     share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE BC_IMPR1(ICODT ,ICODR ,ISKEW ,SKEW  ,NDOF  ,
     1                    IADN  ,LB    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_ASPC
      USE IMP_FVBCL
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICODT(*),ICODR(*),ISKEW(*),NDOF(*),IADN(*)
      my_real
     .   SKEW(LSKEW,*),LB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, ISK,  ICT,ICR,J,K,ND,IR,IT,IAD,NN,N
      my_real
     .   EJ(3)
C----------------BC-------------------------
         IT = 0
         IR = 1
C-----case FV,BCS coupling---
      IF (NFVBCL > 0 )THEN
       IF (IRODDL==0) THEN
        DO I = 1,NUMNOD
         ISK = ISKEW(I)
         IF (ISK>1) THEN
          ICT = IABS(ICT_1(I))
          K = NDOF(I)
          ND = IADN(I)
          IF (ICT > 0 .AND. K> 0) THEN
          CALL BCL_IMPB(ICT  ,ISK   ,SKEW  ,ND    ,LB   ,
     1                 IT    )
          ENDIF
         ENDIF
        ENDDO
C
       ELSE
C
        DO I = 1,NUMNOD
         ISK = ISKEW(I)
         IF (ISK>1) THEN
          ICT = IABS(ICT_1(I))
          ICR = IABS(ICR_1(I))
          K = NDOF(I)
          ND = IADN(I)
          IF (ICT > 0 .AND. K> 0) THEN
          CALL BCL_IMPB(ICT  ,ISK   ,SKEW  ,ND    ,LB   ,
     1                 IT    )
          ENDIF
          IF (ICR > 0 .AND. K==6) THEN
           CALL BCL_IMPB(ICR  ,ISK   ,SKEW  ,ND    ,LB   ,
     1                 IR    )
          ENDIF
         ENDIF
        ENDDO
C
       END IF !(IRODDL==0) THEN
      ELSE
C
      IF (IRODDL==0) THEN
      DO I = 1,NUMNOD
       ISK = ISKEW(I)
       IF (ISK>1) THEN
        ICT = ICODT(I)
        K = NDOF(I)
        ND = IADN(I)
        IF (ICT > 0 .AND. K> 0) THEN
         CALL BCL_IMPB(ICT  ,ISK   ,SKEW  ,ND    ,LB   ,
     1                 IT    )
        ENDIF
       ENDIF
      ENDDO
C
      ELSE
C
      DO I = 1,NUMNOD
       ISK = ISKEW(I)
       IF (ISK>1) THEN
        ICT = ICODT(I)
        ICR = ICODR(I)
        K = NDOF(I)
        ND = IADN(I)
        IF (ICT > 0 .AND. K> 0) THEN
         CALL BCL_IMPB(ICT  ,ISK   ,SKEW  ,ND    ,LB   ,
     1                 IT    )
        ENDIF
        IF (ICR > 0 .AND. K==6) THEN
C
         CALL BCL_IMPB(ICR  ,ISK   ,SKEW  ,ND    ,LB   ,
     1                 IR    )
        ENDIF
       ENDIF
      ENDDO
C
      ENDIF
      END IF !(NFVBCL > 0 )THEN
C
       DO N = 1, NSPCL
        I = IN_SPC(N)
        IF (NDOF(I)==0) CYCLE
        IAD = 6*(N-1)+1
        NN = IC_SPC(N)
            ND = IADN(I)
        IR=0
        IF (NN>3) THEN
         NN=NN-3
         IR = 1
        END IF
        IF (NN==1) THEN
         EJ(1)=SKEW_SPC(IAD)
         EJ(2)=SKEW_SPC(IAD+1)
         EJ(3)=SKEW_SPC(IAD+2)
             CALL L_DIR(EJ,J)
             CALL BC_UPDB(ND    ,EJ    ,J     ,IR   ,LB    )
        ELSEIF (NN==2) THEN
             CALL BC_UPDF2D(ND   ,SKEW_SPC(IAD),SKEW_SPC(IAD+3),IR,LB )
        END IF
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  BCL_IMPB                      source/constraints/general/bcs/bc_imp0.F
Chd|-- called by -----------
Chd|        BC_IMPR1                      source/constraints/general/bcs/bc_imp0.F
Chd|-- calls ---------------
Chd|        BC_UPDB                       source/constraints/general/bcs/bc_imp0.F
Chd|        BC_UPDF2D                     source/constraints/general/bcs/bc_imp0.F
Chd|        L_DIR2                        source/constraints/general/bcs/bc_imp0.F
Chd|====================================================================
      SUBROUTINE BCL_IMPB(ICT  ,ISK   ,SKEW  ,ND    ,LB    ,
     1                    IR   )
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
      INTEGER ICT,ND,ISK,IR
      my_real
     .   SKEW(LSKEW,*),LB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J,K,J1,L
      my_real
     .   EJ(3)
C----------------BC-------------------------
         IF (ICT == 4 ) THEN
          EJ(1)=SKEW(1,ISK)
          EJ(2)=SKEW(2,ISK)
          EJ(3)=SKEW(3,ISK)
          CALL L_DIR2(EJ,J,1)
          CALL BC_UPDB(ND    ,EJ    ,J     ,IR    ,LB    )
         ELSEIF (ICT == 2) THEN
          EJ(1)=SKEW(4,ISK)
          EJ(2)=SKEW(5,ISK)
          EJ(3)=SKEW(6,ISK)
          CALL L_DIR2(EJ,J,2)
          CALL BC_UPDB(ND    ,EJ    ,J     ,IR    ,LB    )
         ELSEIF (ICT == 1) THEN
          EJ(1)=SKEW(7,ISK)
          EJ(2)=SKEW(8,ISK)
          EJ(3)=SKEW(9,ISK)
          CALL L_DIR2(EJ,J,3)
          CALL BC_UPDB(ND    ,EJ    ,J     ,IR    ,LB    )
         ELSEIF (ICT == 3) THEN
C
          CALL BC_UPDF2D(ND    ,SKEW(7,ISK),SKEW(4,ISK),IR    ,LB     )
         ELSEIF (ICT == 5) THEN
C
          CALL BC_UPDF2D(ND    ,SKEW(7,ISK),SKEW(1,ISK),IR    ,LB     )
         ELSEIF (ICT == 6) THEN
C
          CALL BC_UPDF2D(ND    ,SKEW(4,ISK),SKEW(1,ISK),IR    ,LB     )
         ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  BC_UPDFR                      source/constraints/general/bcs/bc_imp0.F
Chd|-- called by -----------
Chd|        BCL_FRK                       source/constraints/general/bcs/bc_imp0.F
Chd|        FV_UPDFR                      source/constraints/general/impvel/fv_imp0.F
Chd|        UPD_KML                       source/mpi/implicit/imp_fri.F 
Chd|        UPD_KSL                       source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        PUT_KMII                      source/implicit/imp_glob_k.F  
Chd|====================================================================
      SUBROUTINE BC_UPDFR(N     ,IDDL  ,EJ    ,JJ    ,IDDLM  ,
     1                   IKC    ,IADK  ,JDIK  ,DIAG_K,LT_K  ,
     2                   LB     ,A     ,KSS   ,KSM   ,IDLM   ,
     3                   IFSS   ,IFSM )
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
      INTEGER N,JJ,IDDL(*),IDDLM(*),IKC(*),IADK(*),JDIK(*),
     .        IDLM,IFSS   ,IFSM
      my_real
     .   EJ(*),DIAG_K(*),LT_K(*),LB(*),A(3,*),
     .   KSS(6),KSM(3,3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ND,K,L,J1,K1,L1,ID,IDM
      my_real
     .   KDD(3,3),KII(6,6)
C-----------------------------------------------
      ND = 3
      K = JJ + 1
      L = JJ + 2
      IF (K>3) K = K - 3
      IF (L>3) L = L - 3
      IF (EJ(K)==ZERO.AND.EJ(L)==ZERO) RETURN
       J1 = JJ
       K1 = K
       L1 = L
       DO I=1,ND
       DO J=1,ND
        KII(I,J)=ZERO
       ENDDO
       ENDDO
      IF (IFSS>0) THEN
       DO I=1,ND
        KDD(I,I)=KSS(I)
       ENDDO
       KDD(1,2) = KSS(4)
       KDD(1,3) = KSS(5)
       KDD(2,3) = KSS(6)
       KDD(2,1) = KDD(1,2)
       KDD(3,1) = KDD(1,3)
       KDD(3,2) = KDD(2,3)
C
       KII(K1,K1)=KDD(K1,K1)-(TWO*KDD(K1,J1)-KDD(J1,J1)*EJ(K))*EJ(K)
       KII(L1,L1)=KDD(L1,L1)-(TWO*KDD(L1,J1)-KDD(J1,J1)*EJ(L))*EJ(L)
       KII(L1,K1)=KDD(L1,K1)-KDD(L1,J1)*EJ(K)-KDD(K1,J1)*EJ(L)
     1           +KDD(J1,J1)*EJ(L)*EJ(K)
       KII(K1,L1)=KII(L1,K1)
       CALL PUT_KMII(IDLM ,IADK,DIAG_K,LT_K ,KII,ND)
       ID = IDDL(N)
       IDM = IDDLM(N)
C--------debug test door_rd8 (GPCG+contact)       
C       IF(IKC(ID+K1)==0) LB(IDM+K1)=LB(IDM+K1)-EJ(K)*A(J1,N)
C       IF(IKC(ID+L1)==0) LB(IDM+L1)=LB(IDM+L1)-EJ(L)*A(J1,N)
      ENDIF
C
      IF (IFSM>0) THEN
       KSM(K1,J1)=KSM(K1,J1)-EJ(K)*KSM(J1,J1)
       KSM(K1,K1)=KSM(K1,K1)-EJ(K)*KSM(J1,K1)
       KSM(K1,L1)=KSM(K1,L1)-EJ(K)*KSM(J1,L1)
       KSM(L1,J1)=KSM(L1,J1)-EJ(L)*KSM(J1,J1)
       KSM(L1,K1)=KSM(L1,K1)-EJ(L)*KSM(J1,K1)
       KSM(L1,L1)=KSM(L1,L1)-EJ(L)*KSM(J1,L1)
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  BCL_FRK                       source/constraints/general/bcs/bc_imp0.F
Chd|-- called by -----------
Chd|        UPD_KML                       source/mpi/implicit/imp_fri.F 
Chd|        UPD_KSL                       source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        BC_UPDFR                      source/constraints/general/bcs/bc_imp0.F
Chd|        BC_UPDFR2                     source/constraints/general/bcs/bc_imp0.F
Chd|        L_DIR2                        source/constraints/general/bcs/bc_imp0.F
Chd|====================================================================
      SUBROUTINE BCL_FRK(N      ,IDDL  ,IDDLM  ,ICT  ,ISK   ,
     1                   SKEW   ,IKC   ,IADK  ,JDIK  ,DIAG_K,
     2                   LT_K   ,LB    ,A     ,KSS   ,KSM   ,
     3                   IDLM   ,IFSS  ,IFSM  )
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
      INTEGER ICT,IKC(*),IDDL(*),IDDLM(*),IADK(*) ,JDIK(*),
     .        N,ISK,IDLM   ,IFSS  ,IFSM
      my_real
     .   SKEW(LSKEW,*),DIAG_K(*),LT_K(*),LB(*),A(3,*),KSS(*),KSM(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J,K,J1,L
      my_real
     .   EJ(3)
C----------------BC-------------------------
         IF (ICT == 4 ) THEN
          EJ(1)=SKEW(1,ISK)
          EJ(2)=SKEW(2,ISK)
          EJ(3)=SKEW(3,ISK)
          CALL L_DIR2(EJ,J,1)
          CALL BC_UPDFR(N     ,IDDL  ,EJ    ,J     ,IDDLM  ,
     1                  IKC   ,IADK  ,JDIK  ,DIAG_K,LT_K   ,
     2                  LB    ,A     ,KSS   ,KSM   ,IDLM   ,
     3                  IFSS  ,IFSM  )
         ELSEIF (ICT == 2) THEN
          EJ(1)=SKEW(4,ISK)
          EJ(2)=SKEW(5,ISK)
          EJ(3)=SKEW(6,ISK)
          CALL L_DIR2(EJ,J,2)
          CALL BC_UPDFR(N     ,IDDL  ,EJ    ,J     ,IDDLM  ,
     1                  IKC   ,IADK  ,JDIK  ,DIAG_K,LT_K   ,
     2                  LB    ,A     ,KSS   ,KSM   ,IDLM   ,
     3                  IFSS  ,IFSM  )
         ELSEIF (ICT == 1) THEN
          EJ(1)=SKEW(7,ISK)
          EJ(2)=SKEW(8,ISK)
          EJ(3)=SKEW(9,ISK)
          CALL L_DIR2(EJ,J,3)
          CALL BC_UPDFR(N     ,IDDL  ,EJ    ,J     ,IDDLM  ,
     1                  IKC   ,IADK  ,JDIK  ,DIAG_K,LT_K   ,
     2                  LB    ,A     ,KSS   ,KSM   ,IDLM   ,
     3                  IFSS  ,IFSM  )
C---------------------
         ELSEIF (ICT == 3) THEN
          CALL BC_UPDFR2(N     ,IDDL  ,SKEW(7,ISK),SKEW(4,ISK),IDDLM ,
     1                  IKC    ,IADK  ,JDIK  ,DIAG_K,LT_K   ,
     2                  LB     ,A     ,KSS   ,KSM   ,IDLM   ,
     3                  IFSS   ,IFSM )
         ELSEIF (ICT == 5) THEN
          CALL BC_UPDFR2(N     ,IDDL  ,SKEW(7,ISK),SKEW(1,ISK),IDDLM ,
     1                  IKC    ,IADK  ,JDIK  ,DIAG_K,LT_K   ,
     2                  LB     ,A     ,KSS   ,KSM   ,IDLM   ,
     3                  IFSS   ,IFSM )
         ELSEIF (ICT == 6) THEN
          CALL BC_UPDFR2(N     ,IDDL  ,SKEW(4,ISK),SKEW(1,ISK),IDDLM ,
     1                  IKC    ,IADK  ,JDIK  ,DIAG_K,LT_K   ,
     2                  LB     ,A     ,KSS   ,KSM   ,IDLM   ,
     3                  IFSS   ,IFSM )
         ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  BC_IMPA                       source/constraints/general/bcs/bc_imp0.F
Chd|-- called by -----------
Chd|        UPD_GLOB_K                    source/implicit/upd_glob_k.F  
Chd|        UPD_INT_K                     source/implicit/upd_glob_k.F  
Chd|-- calls ---------------
Chd|        BC_UPDK                       source/constraints/general/bcs/bc_imp0.F
Chd|        BC_UPDK2D                     source/constraints/general/bcs/bc_imp0.F
Chd|        L_DIR                         source/constraints/general/bcs/bc_imp0.F
Chd|        IMP_ASPC                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE BC_IMPA(IADK  ,JDIK   ,DIAG_K,LT_K  ,NDOF  ,
     1                   IDDL  ,IKC    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_ASPC
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
      INTEGER IADK(*),JDIK(*),NDOF(*),IDDL(*),IKC(*)
      my_real
     .  DIAG_K(*),LT_K(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,IER1,IR,IAD,NN,ND,KC
C
      my_real
     .  EJ(3)
C-----------------------------------------------
       KC=15
       DO N = 1, NSPCL
        I = IN_SPC(N)
        IF (NDOF(I)==0) CYCLE
        IR = 0
        IAD = 6*(N-1)+1
        NN = IC_SPC(N)
        ND = IDDl(I)
        IF (NN>3) THEN
         NN= NN-3
         IR = 1
        ENDIF
        IF (NN==1) THEN
         EJ(1)=SKEW_SPC(IAD)
         EJ(2)=SKEW_SPC(IAD+1)
         EJ(3)=SKEW_SPC(IAD+2)
             CALL L_DIR(EJ,J)
             CALL BC_UPDK(I     ,IDDL  ,EJ    ,J     ,IR    ,
     1                    IADK  ,JDIK  ,DIAG_K ,LT_K )
             IKC(ND +J) = KC
        ELSEIF (NN==2) THEN
         CALL BC_UPDK2D(IDDL  ,IKC(ND+1),SKEW_SPC(IAD),SKEW_SPC(IAD+3),
     1                  I     ,IR    ,KC    ,IADK  ,JDIK  ,DIAG_K,
     2                  LT_K  )
        END IF
       ENDDO
C
       RETURN
      END
Chd|====================================================================
Chd|  CLCEIG                        source/constraints/general/bcs/bc_imp0.F
Chd|-- called by -----------
Chd|        AUTSPC                        source/constraints/general/bcs/bc_imp0.F
Chd|-- calls ---------------
Chd|        CP_REAL                       source/implicit/produt_v.F    
Chd|        ZERO1                         source/system/zero.F          
Chd|====================================================================
      SUBROUTINE CLCEIG(AMTX,EIGVAL,EIGVEC,SMALL,NMTX,IERR)
C     PURPOSE:
C
C        CALCULATE THE EIGENVALUES AND EIGEN VECTORS OF A SYMMETRIC
C        (N X N) MATRIX USING JACOBI'S METHOD (REF:  COMPUTER
C        APPLICATIONS OF NUMERICAL METHODS - SHAN S. KUO)
C
C     INPUT:
C
C        AMTX(NMTX,NMTX)    - MATRIX WHOSE EIGENVALUES AND EIGEN VECTORS
C                             ARE TO BE CALCULATED
C        SMALL              - IF THE RATIO OF MAXIMUM OFF-DIAGONAL
C                             TERM TO CORRESPONDING DIAGONAL TERMS IS
C                             BELOW THIS VALUE, JACOBI METHOD HAS
C                             CONVERGED
C        NMTX               - DIMENSION OF AMTX
C
C     OUTPUT:
C
C        EIGVAL(NMTX)       - THE EIGENVALUES OF AMTX
C        EIGVEC(NMTX,NMTX)  - THE EIGEN VECTORS OF AMTX
C        IERR               - ERROR SWITCH
C                             = 0 - NO ERROR ENCOUNTERED
C                             > 0 - ERROR ENCOUNTERED
C
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
      INTEGER NMTX, IERR
      my_real
     *       SMALL
      my_real
     *       AMTX(NMTX,NMTX), EIGVAL(NMTX), EIGVEC(NMTX,NMTX)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, IB, JB, ITN, ITMAX
      my_real
     *        ROTN(NMTX,NMTX), WORK(NMTX,NMTX)
      my_real
     *       BIG, BRATIO, BSAVE, DEN, TT, CT, ST
      DATA ITMAX /50/
C
C     IF AMTX IS A 1 X 1 MATRIX
C
      IF (NMTX == 1) THEN
         EIGVAL(1) = AMTX(1,1)
         EIGVEC(1,1) = ONE
         GOTO 999
      ENDIF
C
C     INITIALIZE EIGEN VECTOR TO IDENTITY MATRIX
C
      CALL ZERO1(EIGVEC,NMTX*NMTX)
      DO 10 I = 1, NMTX
         EIGVEC(I,I) = ONE
 10   CONTINUE
C
C     FIND LARGEST OFF-DIAGONAL TERM IN AMTX THAT NEEDS TO BE ZEROED OUT
C
      ITN = 0
 40   BIG = ZERO
      BSAVE = ZERO
      DO 60 I = 1, (NMTX - 1)
         DO 50 J = (I+1), NMTX
C
C           CALCULATE MAXIMUM NONZERO RATIO BETWEEN OFF-DIAGONAL AND
C           DIAGONAL TERMS OF THE AMTX IN ROW I
C
            IF (ABS(AMTX(I,I)) >= ABS(AMTX(J,J))) THEN
               IF (ABS(AMTX(I,I)) > SMALL) THEN
                  BRATIO = ABS(AMTX(I,J)/AMTX(I,I))
               ELSE
                  BRATIO = ABS(AMTX(I,J))/SMALL
               ENDIF
            ELSE
               IF (ABS(AMTX(J,J)) > SMALL) THEN
                  BRATIO = ABS(AMTX(I,J)/AMTX(J,J))
               ELSE
                  BRATIO = ABS(AMTX(I,J))/SMALL
               ENDIF
            ENDIF
C
            IF (BRATIO > BSAVE) THEN
               BSAVE = BRATIO
            ENDIF
C
C           LARGEST OFF-DIAGONAL ELEMENT TO BE ZEROED OUT
C
            IF (ABS(AMTX(I,J)) > BIG) THEN
               BIG = ABS(AMTX(I,J))
               IB = I
               JB = J
            ENDIF
 50      CONTINUE
 60   CONTINUE
C
      IF (BSAVE <= SMALL) THEN
C
C        JACOBI METHOD HAS CONVERGED; UPDATE EIGENVALUES AND RETURN
C
         DO 70 I = 1, NMTX
            EIGVAL(I) = AMTX(I,I)
 70      CONTINUE
      ELSE
C
C        PERFORM JACOBI ITERATION IF AMTX IS NOT DIAGONALIZED OUT
C
         ITN = ITN + 1
C
C        CHECK LIMIT ON NUMBER OF JACOBI ITERATIONS
C
         IF (ITN > ITMAX) THEN
            IERR = 277
C            CALL OSERR3(IERR,ITMAX,BIG,'CLCEIG',' ',LUNOUT)
            GOTO 999
         ENDIF
C
C        CALCULATE TANGENT, COSINE AND SINE OF ROTATION ANGLE (THETA)
C
         DEN = ABS(AMTX(IB,IB) - AMTX(JB,JB)) +
     *         SQRT( (AMTX(IB,IB) - AMTX(JB,JB))**2 +
     *           FOUR*AMTX(IB,JB)**2 )
         IF (DEN > ZERO) THEN
            IF (AMTX(IB,IB) >= AMTX(JB,JB)) THEN
               TT = TWO*AMTX(IB,JB)/DEN
            ELSE
               TT = -TWO*AMTX(IB,JB)/DEN
            ENDIF
C
            CT = ONE/SQRT(ONE + TT**2)
            ST = CT*TT
         ELSE
            IF (AMTX(IB,IB) >= AMTX(JB,JB)) THEN
               CT = ZERO
               ST = ONE
            ELSE
               CT = ZERO
               ST = -ONE
            ENDIF
         ENDIF
C
C        CONSTRUCT ROTATION MATRIX
C
         CALL ZERO1(ROTN,NMTX*NMTX)
         DO 90 I = 1, NMTX
            ROTN(I,I) = ONE
 90      CONTINUE
C
         ROTN(IB,IB) = CT
         ROTN(IB,JB) = -ST
         ROTN(JB,IB) = ST
         ROTN(JB,JB) = CT
C
C        CALCULATE TRIPLE PRODUCT, TRANSPOSE(ROTN) X AMTX X ROTN
C
         DO 130 I = 1, NMTX
            DO 120 J = 1, NMTX
               IF (J == IB) THEN
                  WORK(I,J) = AMTX(I,IB)*CT + AMTX(I,JB)*ST
               ELSEIF (J == JB) THEN
                  WORK(I,J) = -AMTX(I,IB)*ST + AMTX(I,JB)*CT
               ELSE
                  WORK(I,J) = AMTX(I,J)
               ENDIF
 120        CONTINUE
 130     CONTINUE
C
         DO 150 I = 1, NMTX
            DO 140 J = 1, NMTX
               IF (I == IB) THEN
                  AMTX(I,J) = WORK(IB,J)*CT + WORK(JB,J)*ST
               ELSEIF (I == JB) THEN
                  AMTX(I,J) = -WORK(IB,J)*ST + WORK(JB,J)*CT
               ELSE
                  AMTX(I,J) = WORK(I,J)
               ENDIF
 140        CONTINUE
 150     CONTINUE
C
C        MAKE AMTX SYMMETRIX BY AVERAGING OFF-DIAGONAL TERMS - TO
C        TAKE CARE OF ROUND-OFF ERRORS IN CALCULATION
C
         DO 170 I = 1, NMTX
            DO 160 J = (I+1), NMTX
               AMTX(I,J) = HALF*(AMTX(I,J) + AMTX(J,I))
               AMTX(J,I) = AMTX(I,J)
 160        CONTINUE
 170     CONTINUE
C
C        CALCULATE CONTRIBUTION TO EIGEN VECTOR MATRIX
C
         DO 190 I = 1, NMTX
            DO 180 J = 1, NMTX
               IF (J == IB) THEN
                  WORK(I,J) = EIGVEC(I,IB)*CT + EIGVEC(I,JB)*ST
               ELSEIF (J == JB) THEN
                  WORK(I,J) = -EIGVEC(I,IB)*ST + EIGVEC(I,JB)*CT
               ELSE
                  WORK(I,J) = EIGVEC(I,J)
               ENDIF
 180        CONTINUE
 190     CONTINUE
         CALL CP_REAL(NMTX*NMTX,WORK,EIGVEC)
C
         GOTO 40
      ENDIF
C
 999  RETURN
      END
Chd|====================================================================
Chd|  AUTSPC                        source/constraints/general/bcs/bc_imp0.F
Chd|-- called by -----------
Chd|        UPD_ASPC0                     source/constraints/general/bcs/bc_imp0.F
Chd|-- calls ---------------
Chd|        CLCEIG                        source/constraints/general/bcs/bc_imp0.F
Chd|        NRMLZAUSPC                    source/constraints/general/bcs/bc_imp0.F
Chd|        ZERO1                         source/system/zero.F          
Chd|====================================================================
      SUBROUTINE AUTSPC(KII,IDEG,RASPCC,SMLEIG,NDDL,
     *                  IKC,NASPCC,LAUSPC,IERR )
C-----------------------------------------------
C --- based on AUTSPC of OS----
C
C     PURPOSE:
C
C        RETRIEVE THE STIFFNESS MATRIX (MAX. DIMENSION OF 3X3)
C        CORRESPONDING TO THE SPECIFIED D.O.F.S OF CURRENT GRID POINT,
C        CALCULATE ITS EIGENVALUES TO IDENTIFY AUTO-SPC D.O.F.
C        CANDIDATES
C
C     INPUT:
C
C         KII(3,3)    - NODAL GLOBAL STIFFNESS MATRIX (TRA. OR ROT.)
C        IDEG(3)        - IDDL ARRAY
C        SMLEIG         - VALUE BELOW WHICH EIGENVALUE IS CONSIDERED = 0
C        NDDL           - NUMBER OF EQUATIONS
C
C     OUTPUT:
C
C        NASPCC  (0-2) - DIMENSION OF AUTO-SPC WITH LOCAL DIRECTION
C        RASPCC(6) -  EIGENVECTORS OF FREE D.O.F.S WITH RIGID MODES
C        LAUSPC  (0-3)  - TOTAL NUMBER OF D.O.F.S THAT HAVE AUTO-SPC
C        IERR           - ERROR SWITCH
C                         = 0 - NO ERROR ENCOUNTERED
C                         > 0 - ERROR ENCOUNTERED
C
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
      INTEGER NDDL, NASPCC, LAUSPC,N,IERR
      INTEGER IDEG(3), IKC(3)
      my_real
     *       KII(3,3), RASPCC(*), SMLEIG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     *   AMTX(9), COPY(9)
      INTEGER IDMTX,JDMTX,KDMTX,NZERO
      INTEGER I, J, IND, IHIGH, NMTX, IEQ, ID(3)
      my_real
     *   EIGVEC(9), EIGVAL(3), HIGH,NOM
C
C     INITIALIZE GRID MATRIX DATA TO ZERO
C
      NMTX = 0
      LAUSPC = 0
      NASPCC = 0
      IERR = 0
C
C     GET THE FREE DEGREES OF FREEDOM
C
      DO I = 1,3
        IF (IDEG(I) <= NDDL .AND. IKC(I) == 0 ) THEN
          NMTX = NMTX + 1
          ID(NMTX) = I
        ENDIF
      ENDDO
C
      IF (NMTX == 0) GOTO 999
C

      CALL ZERO1(EIGVAL,3)

C================================== ONE DOF
      IF (NMTX == 1) THEN
C
C        IF GRID POINT HAS ONE FREE D.O.F., GET THE 1X1 MATRIX
C
          IDMTX=ID(NMTX)
          AMTX(1) = KII(IDMTX,IDMTX)
C
C        IF THERE IS A RIGID BODY MODE, UPDATE AUTO-SPC CANDIDATE ARRAY
C
        IF (ABS(AMTX(1)) <= EM10 ) THEN
          LAUSPC = LAUSPC + 1
          IKC(IDMTX) = 14
        ENDIF
C
C================================== TWO DOF
      ELSEIF (NMTX == 2) THEN
C
C        IF GRID POINT HAS TWO FREE D.O.F.S, GET THE 2X2 MATRIX
C
        CALL ZERO1(AMTX,4)
        CALL ZERO1(EIGVEC,4)
        IDMTX=ID(1)
        JDMTX=ID(2)
        AMTX(1) = KII(IDMTX,IDMTX)
        AMTX(3) = KII(IDMTX,JDMTX)
        AMTX(4) = KII(JDMTX,JDMTX)
        AMTX(2) = AMTX(3)
C
C        CALCULATE EIGENVALUES AND IF THERE ARE RIGID BODY MODES, UPDATE
C        AUTO-SPC CANDIDATE ARRAY - THE D.O.F IN EACH RIGID BODY MODE
C        WHICH HAS THE HIGHEST COMPONENT WITHIN ITS EIGEN VECTOR IS THE
C        ONE TO BE UPDATED
C
        CALL CLCEIG(AMTX,EIGVAL,EIGVEC,SMLEIG,NMTX,IERR)
        IF (IERR /= 0)  GOTO 999
C
C        NORMALIZE EIGENVALUES BASED ON MAXIMUM EIGENVALUE
C
        CALL NRMLZAUSPC(EIGVAL,SMLEIG,2,NZERO)
C
C        IF BOTH EIGENVALUES ARE ZERO, IKC=14
C
        IF (NZERO == 2) THEN
	 DO I = 1 , NMTX
          LAUSPC = LAUSPC + 1
          IKC(ID(I)) = 14
	 ENDDO
        ELSEIF (NZERO == 1) THEN
         DO 100 I = 1, NMTX
          IF ( EIGVAL(I) < SMLEIG ) THEN
            LAUSPC = LAUSPC + 1
            IND = 2*I-1
            HIGH = MAX(ABS(EIGVEC(IND)),EIGVEC(ABS(IND+1)))
	    IF (ABS(HIGH-ONE)<SMLEIG) THEN
             IKC(ID(I)) = 14
	    ELSE
             IKC(ID(I)) = 15
	     NASPCC =NASPCC +1
             RASPCC(ID(1)) = EIGVEC(IND)
             RASPCC(ID(2)) = EIGVEC(IND+1)
            ENDIF
          ENDIF
  100    CONTINUE
        ENDIF
C
C================================== THREE DOF
      ELSEIF (NMTX == 3) THEN
C
C        IF GRID POINT HAS THREE FREE D.O.F.S, FILL THE AMTX MATRIX
C
        CALL ZERO1(EIGVEC,9)
C
C        CALCULATE EIGENVALUES AND IF THERE ARE RIGID BODY MODES, UPDATE
C        AUTO-SPC CANDIDATE ARRAY - THE D.O.F IN EACH RIGID BODY MODE
C        WHICH HAS THE HIGHEST COMPONENT WITHIN ITS EIGEN VECTOR IS THE
C        ONE TO BE UPDATED
C
         CALL CLCEIG(KII,EIGVAL,EIGVEC,SMLEIG,NMTX,IERR)
         IF (IERR /= 0) GOTO 999
c
          DO I = 1, NMTX
           COPY(I)=EIGVAL(I)
	  ENDDO
C
C        NORMALIZE EIGENVALUES BASED ON MAXIMUM EIGENVALUE
C
         CALL NRMLZAUSPC(EIGVAL,SMLEIG,3,NZERO)
C
        IF (NZERO == 3) THEN
	 DO I = 1 , NMTX
          LAUSPC = LAUSPC + 1
          IKC(I) = 14
	 ENDDO
        ELSEIF (NZERO == 2) THEN
	 HIGH = ZERO
         DO I = 1, NMTX
          IF ( EIGVAL(I) < SMLEIG ) THEN
            LAUSPC = LAUSPC + 1
            IND = 3*I-2
	    DO J=0,2
             HIGH = MAX(HIGH,ABS(EIGVEC(IND+J)))
	    ENDDO
          ENDIF
	 ENDDO
C        STILL GLOBAL SYSTEM
	 IF (ABS(HIGH-ONE)<SMLEIG) THEN
          DO I = 1, NMTX
           IF ( EIGVAL(I) < SMLEIG ) IKC(I) = 14
	  ENDDO
	 ELSE
          DO I = 1, NMTX
           IF ( EIGVAL(I) < SMLEIG ) THEN
	     NASPCC =NASPCC +1
             IKC(I) = 15
             IND = 3*I-2
             IEQ = 3*(NASPCC-1)
             RASPCC(IEQ+1) = EIGVEC(IND)
             RASPCC(IEQ+2) = EIGVEC(IND+1)
             RASPCC(IEQ+3) = EIGVEC(IND+2)
           ENDIF
	  ENDDO
         ENDIF
        ELSEIF (NZERO == 1) THEN
         DO I = 1, NMTX
          IF ( EIGVAL(I) < SMLEIG ) THEN
            LAUSPC = LAUSPC + 1
            IND = 3*I-2
            HIGH = MAX(EIGVEC(IND),EIGVEC(IND+1),EIGVEC(IND+2))
	    IF (ABS(HIGH-ONE)<SMLEIG) THEN
             IKC(I) = 14
	    ELSE
	     NASPCC =NASPCC +1
             IKC(I) = 15
             RASPCC(1) = EIGVEC(IND)
             RASPCC(2) = EIGVEC(IND+1)
             RASPCC(3) = EIGVEC(IND+2)
            ENDIF
          ENDIF
	 ENDDO
        ENDIF
      ENDIF
C
 999  CONTINUE
C
      RETURN
      END
Chd|====================================================================
Chd|  NRMLZAUSPC                    source/constraints/general/bcs/bc_imp0.F
Chd|-- called by -----------
Chd|        AUTSPC                        source/constraints/general/bcs/bc_imp0.F
Chd|-- calls ---------------
Chd|        ZERO1                         source/system/zero.F          
Chd|====================================================================
      SUBROUTINE NRMLZAUSPC(VECTOR,SMLEIG,LENGTH,NZERO)

C     PURPOSE:
C
C        NORMALIZE A VECTOR WITH THE ELEMENT OF LARGEST MAGNITUDE
C
C     DIFFERS FROM NRMLZE THAT IT ZEROS 'SMALL' VECTOR ENTRIES

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
      INTEGER LENGTH,NZERO
      my_real
     *       SMLEIG, VECTOR(LENGTH)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     *       HIGH

      DO I = 1, LENGTH
        VECTOR(I) = ABS(VECTOR(I))
      ENDDO
      NZERO = 0

      HIGH = MAX(VECTOR(1),VECTOR(2))
      IF ( LENGTH == 3 ) THEN
        HIGH = MAX(HIGH,VECTOR(3))
      ENDIF

      IF ( HIGH < EM10 ) THEN
        CALL ZERO1(VECTOR,LENGTH)
         NZERO = LENGTH
      ELSE
        DO I = 1,LENGTH
          VECTOR(I) = VECTOR(I)/HIGH
          IF ( VECTOR(I) < SMLEIG ) THEN
            VECTOR(I) = ZERO
            NZERO = NZERO + 1
          ENDIF
        ENDDO
      ENDIF

      RETURN
      END
C
Chd|====================================================================
Chd|  SPC_DIR                       source/constraints/general/bcs/bc_imp0.F
Chd|-- called by -----------
Chd|        UPD_ASPC0                     source/constraints/general/bcs/bc_imp0.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPC_DIR(IKC,J,J1)
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
      INTEGER IKC(3),J,J1
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,II(3)
      J = 0
      J1 =0
      N =0
      DO I = 1, 3
        IF (IKC(I) ==15) THEN
	 N = N+1
	 II(N)=I
C---------reset in BC_IMPA ---
         IKC(I)=0
	ENDIF
      ENDDO
      IF (N>0) J =II(1)
      IF (N>1) J1 =II(2)

      RETURN
      END
C
Chd|====================================================================
Chd|  UPD_ASPC0                     source/constraints/general/bcs/bc_imp0.F
Chd|-- called by -----------
Chd|        UPD_ASPC                      source/constraints/general/bcs/bc_imp0.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        AUTSPC                        source/constraints/general/bcs/bc_imp0.F
Chd|        GET_KII                       source/implicit/imp_glob_k.F  
Chd|        SPC_DIR                       source/constraints/general/bcs/bc_imp0.F
Chd|        IMP_ASPC                      share/modules/impbufdef_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE UPD_ASPC0(NDDL  ,NDOF  ,IDDL  ,IKC    ,ITAB  ,
     .                    IADK  ,JDIK  ,DIAG_K,LT_K   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_ASPC
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "impl1_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDDL,NDOF(*),IDDL(*),IKC(*),ITAB(*),
     .        IADK(*) ,JDIK(*)
      my_real
     .   DIAG_K(*),LT_K(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NSPCLI,NSPCT,NSPCTI,I,J,K,N,
     .        ID(3),IK,IDK,IER1,IER2,IERR,NSPC,IR,NDOFT,J1,NSPCR,
     .        NSPCN,IKC3
C
      my_real
     *       KII(3,3),KDD(6,6),SMLEIG,KDIV3,RS
      INTEGER, DIMENSION(:),ALLOCATABLE :: ILT,ILR
      my_real, DIMENSION(:,:),ALLOCATABLE ::
     .                                     SKEWT,SKEWR
C-----------------------------------------------
      NSPCL = 0
      NSPCT = 0
      NSPCR = 0
      NSPCN = 0
      IERR = 0
      SMLEIG = EM8
      IF(IAUTSPC>1) THEN
        ALLOCATE(ILT(NUMNOD),SKEWT(6,NUMNOD),STAT=IER1)
        ILT=0
        SKEWT=ZERO
       IF (IRODDL/=0) THEN
        ALLOCATE(ILR(NUMNOD),SKEWR(6,NUMNOD),STAT=IER2)
        ILR=0
        SKEWR=ZERO
       ENDIF
      ENDIF
      DO I = 1, NUMNOD
       IF (NDOF(I)==0) CYCLE
       CALL GET_KII(I ,IDDL ,IADK,DIAG_K,LT_K ,KDD,NDOF(I))
       NDOFT = MIN(3,NDOF(I))
       KDIV3=KDD(1,1)+KDD(2,2)+KDD(3,3)
       IK = IDDL(I)+1
       IKC3=IKC(IK)+IKC(IK+1)+IKC(IK+2)
       IF (KDIV3<=EM10.AND.IKC3==0) THEN
        DO J=1,NDOFT
         K = IDDL(I)+J
	 IKC(K) = 14
        ENDDO
        NSPCT = NSPCT+3
        NSPCN = NSPCN + 1
       ELSEIF(IAUTSPC>1) THEN
        DO J=1,NDOFT
        DO K=J,NDOFT
         KII(J,K) = KDD(J,K)
        ENDDO
        ENDDO
        DO J=1,NDOFT
        DO K=J+1,NDOFT
         KII(K,J) = KII(J,K)
        ENDDO
        ENDDO
        DO J=1,NDOFT
         ID(J) = IDDL(I)+J
        ENDDO
        IDK = ID(1)
        CALL AUTSPC(KII,ID  ,SKEWT(1,I),SMLEIG,NDDL  ,
     *              IKC(IDK),ILT(I),NSPCTI ,IERR  )
        IF (IERR > 0) THEN
         IERR = I
         GO TO 900
        ENDIF
        NSPCT = NSPCT+NSPCTI
        NSPCL = NSPCL + MIN(1,ILT(I))
        IF ((ILT(I)+NSPCTI)>0) NSPCN = NSPCN + 1
       ENDIF !((KDD(1,1)+KDD(2,2)+KDD(3,3))<=EM10) THEN
       IF (NDOF(I)==6) THEN
        IK = IDDL(I)+4
        IKC3=IKC(IK)+IKC(IK+1)+IKC(IK+2)
        IF ((KDD(4,4)+KDD(5,5)+KDD(6,6))<=EM10.AND.IKC3==0) THEN
         DO J=1,NDOFT
          K = IDDL(I)+J + 3
	  IKC(K) = 14
         ENDDO
         NSPCR = NSPCR+3
         IF (KDIV3>EM10) NSPCN = NSPCN + 1
        ELSEIF(IAUTSPC>1) THEN
         DO J=1,NDOFT
         DO K=J,NDOFT
          KII(J,K) = KDD(J+3,K+3)
         ENDDO
         ENDDO
         DO J=1,NDOFT
         DO K=J+1,NDOFT
          KII(K,J) = KII(J,K)
         ENDDO
         ENDDO
         DO J=1,NDOFT
          ID(J) = IDDL(I)+J+3
         ENDDO
         IDK = ID(1)
         CALL AUTSPC(KII,ID  ,SKEWR(1,I),SMLEIG,NDDL  ,
     *             IKC(IDK) ,ILR(I),NSPCTI ,IERR  )
         IF (IERR > 0) THEN
          IERR = I
          GO TO 900
         ENDIF
         NSPCR = NSPCR+NSPCTI
         NSPCL = NSPCL + MIN(1,ILR(I))
         IF ((ILR(I)+NSPCTI)>0) NSPCN = NSPCN + 1
        END IF !((KDD(4,4)+KDD(5,5)+KDD(6,6))<=EM10) THEN
       ENDIF
      ENDDO
C
      NSPCNT = NSPCN
      IF (NSPCN>0.AND.ILINE/=0) THEN
           IF(IMACH/=3.OR.ISPMD==0)THEN
             WRITE(ISTDO,'(I10,A)')NSPCN,
     .        ' NODES TREATED BY AUTOSPC FOR :'
             WRITE(ISTDO,'(I10,A)')NSPCT,
     .        ' TRANSLATIONAL DOFS'
             WRITE(ISTDO,'(I10,A)')NSPCR,
     .        ' ROTATIONAL DOFS'
             WRITE(IOUT,'(I10,A)')NSPCN,
     .        ' NODES TREATED BY AUTOSPC FOR :'
             WRITE(IOUT,'(I10,A)')NSPCT,
     .        ' TRANSLATIONAL DOFS'
             WRITE(IOUT,'(I10,A)')NSPCR,
     .        ' ROTATIONAL DOFS'
           ENDIF
      ENDIF
C
      IF (NSPCL>0) THEN
        IF(ALLOCATED(IN_SPC)) DEALLOCATE(IN_SPC)
        IF(ALLOCATED(IC_SPC)) DEALLOCATE(IC_SPC)
        ALLOCATE(IN_SPC(NSPCL),IC_SPC(NSPCL),STAT=IER1)
        IF(ALLOCATED(SKEW_SPC)) DEALLOCATE(SKEW_SPC)
        ALLOCATE(SKEW_SPC(6*NSPCL),STAT=IER2)
       IC_SPC =0
       IN_SPC =0
       SKEW_SPC=ZERO
       NSPC = 0
       DO I = 1, NUMNOD
        IF (ILT(I)>0) THEN
         NSPC = NSPC +1
         IR = 0
         IN_SPC(NSPC) = I
         CALL SPC_DIR(IKC(IDDL(I)+1),J   ,J1    )
         IC_SPC(NSPC) = ILT(I)
         J = 6*(NSPC-1)+1
         SKEW_SPC(J)=SKEWT(1,I)
         SKEW_SPC(J+1)=SKEWT(2,I)
         SKEW_SPC(J+2)=SKEWT(3,I)
         IF (ILT(I)==2) THEN
          SKEW_SPC(J+3)=SKEWT(4,I)
          SKEW_SPC(J+4)=SKEWT(5,I)
          SKEW_SPC(J+5)=SKEWT(6,I)
         ENDIF
        ENDIF
        IF (ILR(I)>0) THEN
         IR = 1
         NSPC = NSPC +1
         IN_SPC(NSPC) = I
         CALL SPC_DIR(IKC(IDDL(I)+4),J   ,J1    )
         IC_SPC(NSPC) = ILR(I) + 3
         J = 6*(NSPC-1)+1
         SKEW_SPC(J)=SKEWR(1,I)
         SKEW_SPC(J+1)=SKEWR(2,I)
         SKEW_SPC(J+2)=SKEWR(3,I)
         IF (ILR(I)==2) THEN
          SKEW_SPC(J+3)=SKEWR(4,I)
          SKEW_SPC(J+4)=SKEWR(5,I)
          SKEW_SPC(J+5)=SKEWR(6,I)
         ENDIF
        ENDIF
       ENDDO
      ENDIF
C
 900  CONTINUE
      IF(IAUTSPC>1) THEN
       DEALLOCATE(ILT,SKEWT)
       IF (IRODDL/=0) DEALLOCATE(ILR,SKEWR)
      ENDIF
      IF (IERR>0) THEN
           IF(IMACH/=3.OR.ISPMD==0)THEN
             CALL ANCMSG(MSGID=102,ANMODE=ANINFO,
     .            I1=ITAB(IERR))
           ENDIF
           CALL ARRET(2)
      ENDIF
      RETURN
      END
Chd|====================================================================
Chd|  UPD_ASPC                      source/constraints/general/bcs/bc_imp0.F
Chd|-- called by -----------
Chd|        UPD_GLOB_K                    source/implicit/upd_glob_k.F  
Chd|-- calls ---------------
Chd|        CP_REAL                       source/implicit/produt_v.F    
Chd|        SPMD_SUMF_K                   source/mpi/implicit/imp_spmd.F
Chd|        UPD_ASPC0                     source/constraints/general/bcs/bc_imp0.F
Chd|====================================================================
      SUBROUTINE UPD_ASPC(NDDL  ,NDOF  ,IDDL  ,IKC    ,ITAB     ,
     .                    IADK  ,JDIK  ,DIAG_K,LT_K   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDDL,NDOF(*),IDDL(*),IKC(*),ITAB(*),
     .        IADK(*) ,JDIK(*)
      my_real
     .   DIAG_K(*),LT_K(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IERR,NZ
      my_real,
     .         DIMENSION(:),ALLOCATABLE :: DIAG_KP,LT_KP
C-----------------------------------------------
       IF (IMACH==3.AND.NSPMD>1) THEN
         NZ = IADK(NDDL+1)-IADK(1)
         ALLOCATE(DIAG_KP(NDDL),LT_KP(NZ),STAT=IERR)
         CALL CP_REAL(NDDL,DIAG_K,DIAG_KP)
         CALL CP_REAL(NZ,LT_K,LT_KP)
         CALL SPMD_SUMF_K(DIAG_KP  ,LT_KP     )
         CALL UPD_ASPC0(NDDL  ,NDOF  ,IDDL  ,IKC    ,ITAB  ,
     .                  IADK  ,JDIK  ,DIAG_KP,LT_KP   )
         DEALLOCATE(DIAG_KP,LT_KP)
       ELSE
           CALL UPD_ASPC0(NDDL  ,NDOF  ,IDDL  ,IKC    ,ITAB  ,
     .                    IADK  ,JDIK  ,DIAG_K,LT_K   )
       ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  BC_UPDK2D                     source/constraints/general/bcs/bc_imp0.F
Chd|-- called by -----------
Chd|        BCL_IMPK                      source/constraints/general/bcs/bc_imp0.F
Chd|        BC_IMPA                       source/constraints/general/bcs/bc_imp0.F
Chd|-- calls ---------------
Chd|        BC_C2D                        source/constraints/general/bcs/bc_imp0.F
Chd|        BC_UPDK2                      source/constraints/general/rbe2/rbe2_imp0.F
Chd|        DIR_RBE2                      source/constraints/general/rbe2/rbe2v.F
Chd|====================================================================
      SUBROUTINE BC_UPDK2D(IADN  ,IFIX  ,SKEW  ,SKEW1 ,I     ,
     1                     IR    ,KC    ,IADK  ,JDIK  ,DIAG_K,
     2                     LT_K  )
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
      INTEGER IADN(*),IFIX(*),IADK(*) ,JDIK(*),I,IR,KC
      my_real
     .   SKEW(3),SKEW1(3),DIAG_K(*),LT_K(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J,K,J1,L,ND
      my_real
     .   EJ(3),EJ1(3),S,EA,EB
C-----------------------------------------------
          EJ(1)=SKEW(1)
          EJ(2)=SKEW(2)
          EJ(3)=SKEW(3)
c          CALL L_DIR(EJ,J)
          EJ1(1)=SKEW1(1)
          EJ1(2)=SKEW1(2)
          EJ1(3)=SKEW1(3)
         CALL BC_C2D(EJ,EJ1, J, J1  ,EA, EB  )
          CALL DIR_RBE2(J    ,J1    ,K     )
c	  S=ONE/(ONE-EJ(J1)*EJ1(J))
C------------signe due to the subroutine BC_UPDK2
c	  EA=-S*(EJ(J1)*EJ1(K)-EJ(K))
c	  EB=-S*(EJ1(J)*EJ(K)-EJ1(K))
          CALL BC_UPDK2(I     ,IADN  ,J    ,J1    ,K     ,
     1                  IR    ,EA    ,EB   ,IADK  ,JDIK  ,
     2                  DIAG_K,LT_K  )
          IFIX(J) = KC
          IFIX(J1) = KC
C
      RETURN
      END
Chd|====================================================================
Chd|  BC_UPDF2D                     source/constraints/general/bcs/bc_imp0.F
Chd|-- called by -----------
Chd|        BCL_IMPB                      source/constraints/general/bcs/bc_imp0.F
Chd|        BC_IMPR1                      source/constraints/general/bcs/bc_imp0.F
Chd|-- calls ---------------
Chd|        BC_C2D                        source/constraints/general/bcs/bc_imp0.F
Chd|        DIR_RBE2                      source/constraints/general/rbe2/rbe2v.F
Chd|====================================================================
      SUBROUTINE BC_UPDF2D(ND    ,SKEW  ,SKEW1 ,IR    ,B     )
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
      INTEGER ND,IR
      my_real
     .   SKEW(3),SKEW1(3),B(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J,K,J1,L
      my_real
     .   EJ(3),EJ1(3),S,EA,EB
C-----------------------------------------------
          EJ(1)=SKEW(1)
          EJ(2)=SKEW(2)
          EJ(3)=SKEW(3)
C          CALL L_DIR(EJ,J)
          EJ1(1)=SKEW1(1)
          EJ1(2)=SKEW1(2)
          EJ1(3)=SKEW1(3)
          CALL BC_C2D(EJ,EJ1, J, J1  ,EA, EB  )
          CALL DIR_RBE2(J    ,J1    ,K     )
c          S=ONE/(ONE-EJ(J1)*EJ1(J))
c          EA=S*(EJ(J1)*EJ1(K)-EJ(K))
c          EB=S*(EJ1(J)*EJ(K)-EJ1(K))
C          
          IF (IR>0) ND=ND+3
          B(ND+K)=B(ND+K)-EA*B(ND+J)-EB*B(ND+J1)
C
      RETURN
      END
Chd|====================================================================
Chd|  BC_UPD2D                      source/constraints/general/bcs/bc_imp0.F
Chd|-- called by -----------
Chd|        BC_IMP2                       source/constraints/general/bcs/bc_imp0.F
Chd|        FR_U2DD                       source/mpi/implicit/imp_fri.F 
Chd|        IMP3_U2X                      source/airbag/monv_imp0.F     
Chd|-- calls ---------------
Chd|        BC_C2D                        source/constraints/general/bcs/bc_imp0.F
Chd|        DIR_RBE2                      source/constraints/general/rbe2/rbe2v.F
Chd|====================================================================
      SUBROUTINE BC_UPD2D(N     ,SKEW    ,SKEW1    ,D     )
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
      INTEGER N
      my_real
     .   SKEW(3),SKEW1(3),D(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,L,J,J1
      my_real
     .   EJ(3),EJ1(3),S,EA,EB
C-----------------------------------------------
          EJ(1)=SKEW(1)
          EJ(2)=SKEW(2)
          EJ(3)=SKEW(3)
c          CALL L_DIR(EJ,J)
          EJ1(1)=SKEW1(1)
          EJ1(2)=SKEW1(2)
          EJ1(3)=SKEW1(3)
          CALL BC_C2D(EJ,EJ1, J, J1  ,EA, EB  )
          CALL DIR_RBE2(J    ,J1    ,K     )
c	  S=ONE/(ONE-EJ(J1)*EJ1(J))
c	  EA=S*(EJ(J1)*EJ1(K)-EJ(K))
c	  EB=S*(EJ1(J)*EJ(K)-EJ1(K))
C
          D(J,N) = -EA* D(K,N)
          D(J1,N)= -EB* D(K,N)
C
      RETURN
      END
Chd|====================================================================
Chd|  FV_UPDKD2                     source/constraints/general/bcs/bc_imp0.F
Chd|-- called by -----------
Chd|        BCL_IMPKD                     source/constraints/general/bcs/bc_imp0.F
Chd|        DIAG_INT                      source/mpi/implicit/imp_fri.F 
Chd|        UPDK_MV                       source/airbag/monv_imp0.F     
Chd|-- calls ---------------
Chd|        BC_C2D                        source/constraints/general/bcs/bc_imp0.F
Chd|        DIR_RBE2                      source/constraints/general/rbe2/rbe2v.F
Chd|====================================================================
      SUBROUTINE FV_UPDKD2(SKEW    ,SKEW1   ,KDD   ,DIAG_K)
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
      my_real
     .   SKEW(3),SKEW1(3),DIAG_K(3),KDD(3,3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ND,K,L,J1,K1,L1,J
      my_real
     .   EJ(3),EJ1(3),S,EA,EB
C-----------------------------------------------
          EJ(1)=SKEW(1)
          EJ(2)=SKEW(2)
          EJ(3)=SKEW(3)
c          CALL L_DIR(EJ,J)
          EJ1(1)=SKEW1(1)
          EJ1(2)=SKEW1(2)
          EJ1(3)=SKEW1(3)
c	  S=ONE/(ONE-EJ(J1)*EJ1(J))
c	  EA=S*(EJ(J1)*EJ1(K)-EJ(K))
c	  EB=S*(EJ1(J)*EJ(K)-EJ1(K))
          CALL BC_C2D(EJ,EJ1, J, J1  ,EA, EB  )
          CALL DIR_RBE2(J    ,J1    ,K     )
          EA = -EA
          EB = -EB
        DIAG_K(K)=DIAG_K(K)+
     .           EA*(KDD(J,J)*EA+TWO*EB*KDD(J,J1)-TWO*KDD(J,K))
     .          +EB*(KDD(J1,J1)*EB-TWO*KDD(J1,K))
C
      RETURN
      END
Chd|====================================================================
Chd|  BC_FI2                        source/constraints/general/bcs/bc_imp0.F
Chd|-- called by -----------
Chd|        BC_UPDF                       source/constraints/general/bcs/bc_imp0.F
Chd|        IMP3_A2B                      source/airbag/monv_imp0.F     
Chd|        UPD_FR                        source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        BC_C2D                        source/constraints/general/bcs/bc_imp0.F
Chd|        DIR_RBE2                      source/constraints/general/rbe2/rbe2v.F
Chd|====================================================================
      SUBROUTINE BC_FI2(N    ,SKEW    ,SKEW1   ,A     )
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
      INTEGER N
      my_real
     .  A(3,*),SKEW(3),SKEW1(3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K,L,J,J1
      my_real
     .   EJ(3),EJ1(3),S,EA,EB
C-----------------------------------------------
          EJ(1)=SKEW(1)
          EJ(2)=SKEW(2)
          EJ(3)=SKEW(3)
c          CALL L_DIR(EJ,J)
          EJ1(1)=SKEW1(1)
          EJ1(2)=SKEW1(2)
          EJ1(3)=SKEW1(3)
          CALL BC_C2D(EJ,EJ1, J, J1  ,EA, EB  )
          CALL DIR_RBE2(J    ,J1    ,K     )
          EA = -EA
          EB = -EB
c	  S=ONE/(ONE-EJ(J1)*EJ1(J))
c	  EA=S*(EJ(J1)*EJ1(K)-EJ(K))
c	  EB=S*(EJ1(J)*EJ(K)-EJ1(K))
C
          A(K,N)=A(K,N)+EA*A(J,N)+EB*A(J1,N)
C
      RETURN
      END
Chd|====================================================================
Chd|  BC_UPDFR2                     source/constraints/general/bcs/bc_imp0.F
Chd|-- called by -----------
Chd|        BCL_FRK                       source/constraints/general/bcs/bc_imp0.F
Chd|        UPD_KML                       source/mpi/implicit/imp_fri.F 
Chd|        UPD_KSL                       source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        BC_C2D                        source/constraints/general/bcs/bc_imp0.F
Chd|        DIR_RBE2                      source/constraints/general/rbe2/rbe2v.F
Chd|        PUT_KMII                      source/implicit/imp_glob_k.F  
Chd|====================================================================
      SUBROUTINE BC_UPDFR2(N     ,IDDL  ,SKEW  ,SKEW1 ,IDDLM  ,
     1                    IKC    ,IADK  ,JDIK  ,DIAG_K,LT_K   ,
     2                    LB     ,A     ,KSS   ,KSM   ,IDLM   ,
     3                    IFSS   ,IFSM )
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
      INTEGER N,IDDL(*),IDDLM(*),IKC(*),IADK(*),JDIK(*),
     .        IDLM,IFSS   ,IFSM
      my_real
     .   DIAG_K(*),LT_K(*),LB(*),A(3,*),
     .   KSS(6),KSM(3,3),SKEW(3),SKEW1(3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ND,K,L,J1,ID,IDM
      my_real
     .   EJ(3),EJ1(3),S,EA,EB,KDD(6,6)
C-----------------------------------------------
          EJ(1)=SKEW(1)
          EJ(2)=SKEW(2)
          EJ(3)=SKEW(3)
c          CALL L_DIR(EJ,J)
          EJ1(1)=SKEW1(1)
          EJ1(2)=SKEW1(2)
          EJ1(3)=SKEW1(3)
          CALL BC_C2D(EJ,EJ1, J, J1  ,EA, EB  )
          CALL DIR_RBE2(J    ,J1    ,K     )
          EA = -EA
          EB = -EB
c	  S=ONE/(ONE-EJ(J1)*EJ1(J))
c	  EA=S*(EJ(J1)*EJ1(K)-EJ(K))
c	  EB=S*(EJ1(J)*EJ(K)-EJ1(K))
C
       ND = 3
      IF (IFSS>0) THEN
       DO I=1,ND
        KDD(I,I)=KSS(I)
       ENDDO
       KDD(1,2) = KSS(4)
       KDD(1,3) = KSS(5)
       KDD(2,3) = KSS(6)
       KDD(2,1) = KDD(1,2)
       KDD(3,1) = KDD(1,3)
       KDD(3,2) = KDD(2,3)
C
       KDD(K,K)= KDD(K,K)
     .          +EA*(KDD(J,J)*EA+TWO*EB*KDD(J,J1)-TWO*KDD(J,K))
     .          +EB*(KDD(J1,J1)*EB-TWO*KDD(J1,K))
       CALL PUT_KMII(IDLM ,IADK,DIAG_K,LT_K ,KDD,ND)
       ID = IDDL(N)
       IDM = IDDLM(N)
       IF(IKC(ID+K)==0) LB(IDM+K)=LB(IDM+K)+EA*A(J,N)+EB*A(J1,N)
      ENDIF
C
      IF (IFSM>0) THEN
       KSM(K,K)= EA*(KSM(J,J)*EA+TWO*EB*KSM(J,J1)-TWO*KSM(J,K))
     .          +EB*(KSM(J1,J1)*EB-TWO*KSM(J1,K))
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  GET_NSPC                      source/constraints/general/bcs/bc_imp0.F
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        IMP_ASPC                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE GET_NSPC(NSPC  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_ASPC
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
      INTEGER NSPC
C
       NSPC=NSPCNT
C
      RETURN
      END
Chd|====================================================================
Chd|  PUT_NSPC                      source/constraints/general/bcs/bc_imp0.F
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        IMP_ASPC                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE PUT_NSPC(NSPC  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_ASPC
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
      INTEGER NSPC
C
       NSPCNT=NSPC
C
      RETURN
      END
Chd|====================================================================
Chd|  BC_C2D                        source/constraints/general/bcs/bc_imp0.F
Chd|-- called by -----------
Chd|        BCL_IMPD                      source/constraints/general/bcs/bc_imp0.F
Chd|        BC_FI2                        source/constraints/general/bcs/bc_imp0.F
Chd|        BC_UPD2D                      source/constraints/general/bcs/bc_imp0.F
Chd|        BC_UPDF2D                     source/constraints/general/bcs/bc_imp0.F
Chd|        BC_UPDFR2                     source/constraints/general/bcs/bc_imp0.F
Chd|        BC_UPDK2D                     source/constraints/general/bcs/bc_imp0.F
Chd|        FV_UPDKD2                     source/constraints/general/bcs/bc_imp0.F
Chd|        GETBCL_J                      source/constraints/general/impvel/fv_imp0.F
Chd|-- calls ---------------
Chd|        GDIR2_IND                     source/constraints/general/impvel/fv_imp0.F
Chd|====================================================================
      SUBROUTINE BC_C2D(EJ,EJ1, J, J1  ,EA, EB  )
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
      INTEGER J, J1  
      my_real
     .   EJ(3),EJ1(3),EA,EB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K
      my_real
     .   DET
C---------set up Matrix C=-(EA,EB) for BC local 2 dir
C-----determine ind dir K
      CALL GDIR2_IND(EJ,EJ1,K)
	  J  = K +1
	  IF (J >3) J= J-3
      J1  = K +2
	  IF (J1 >3) J1= J1-3
      DET = ONE/(EJ(J)*EJ1(J1)-EJ(J1)*EJ1(J))
	  EA = DET*(EJ1(J1)*EJ(K)-EJ(J1)*EJ1(K))
	  EB = DET*(EJ(J)*EJ1(K)-EJ1(J)*EJ(K))
      RETURN
      END
