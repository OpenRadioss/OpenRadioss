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
Chd|  SP_STAT0                      source/implicit/imp_fsa_inv.F 
Chd|-- called by -----------
Chd|        IMP_FSA_INV2                  source/implicit/imp_fsa_inv.F 
Chd|        IMP_FSA_INVH                  source/implicit/imp_fsa_inv.F 
Chd|        IMP_FSA_INVH2                 source/implicit/imp_fsa_inv.F 
Chd|        IMP_FSA_INVP                  source/implicit/imp_fsa_inv.F 
Chd|        IMP_FSA_INVP2                 source/implicit/imp_fsa_inv.F 
Chd|        M_LNZ                         source/implicit/imp_solv.F    
Chd|        SMS_FSA_INVH                  source/ams/sms_fsa_inv.F      
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE SP_STAT0(IL ,IADK  ,JDIK  ,NC     ,JM    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  IL, IADK(*)  ,JDIK(*),NC,JM(*)
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J,K
C-----------------------------------------------
      NC=0
      DO K =IADK(IL),IADK(IL+1)-1
       NC=NC+1
       JM(NC)=JDIK(K)
      ENDDO 
      NC=NC+1
      JM(NC)=IL
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  SP_STATIC                     source/implicit/imp_fsa_inv.F 
Chd|-- called by -----------
Chd|        IMP_PC_INV                    source/implicit/imp_pc_inv.F  
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        SP_A2                         source/implicit/imp_fsa_inv.F 
Chd|====================================================================
      SUBROUTINE SP_STATIC(NDDL  ,IADK  ,JDIK  ,DIAG_K ,LT_K  ,
     .                     IADM  ,JDIM  ,NNZM  ,NC     ,JM    ,
     .                     MAXC  ,PSI   ,IP    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C--IP:0 standard; unpaire:avec pre-filtre; 2,3 level 2; >4 fsai(ip-4 : 6,7:level 2)
      INTEGER  NDDL  ,MAXC,IADK(*)  ,JDIK(*)
      INTEGER  NNZM,IADM(*) ,JDIM(*),NC(*),JM(MAXC,*),IP
C     REAL ou REAL*8
      my_real
     .  LT_K(*),DIAG_K(*),PSI
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J,N,K,I1,IFSAI,IOPT
      my_real
     .  PSR
C-----------------------------------------------
      IF (IP.GE.4) THEN
       IFSAI=1
       IOPT=IP-4
      ELSE
       IFSAI=0
       IOPT=IP
      ENDIF
C------define sparse static pattern:----
      NNZM = 0
      IADM(1)=1
C-------symetry firstly---
      IF (MOD(IOPT,2).GT.0) THEN
C-------pre-filtrage----
       DO I=1,NDDL
        DO K =IADK(I),IADK(I+1)-1
         J=JDIK(K)
         PSR = PSI*SQRT(DIAG_K(I)*DIAG_K(J))
         IF (ABS(LT_K(K)).GE.PSR) THEN
          NNZM = NNZM+1
          JDIM(NNZM)=J
         ENDIF
        ENDDO 
        IADM(I+1)=NNZM+1
       ENDDO 
      ELSE 
       DO I=2,NDDL+1
        IADM(I)=IADK(I)
       ENDDO
       NNZM= IADK(NDDL+1)-1
       DO I=1,NNZM
        JDIM(I)=JDIK(I)
       ENDDO
      ENDIF
C
      DO I=1,NDDL
       NC(I)=0
      ENDDO 
C
      IF (IOPT.GT.1) THEN
       DO I=1,NDDL
C------------avec diag----
        NC(I) = NC(I)+1
        JM(NC(I),I)=I
        DO K =IADM(I),IADM(I+1)-1
         J=JDIM(K)
         NC(J) = NC(J)+1
         JM(NC(J),J)=I
         NC(I) = NC(I)+1
         JM(NC(I),I)=J
        ENDDO 
       ENDDO 
       CALL SP_A2(NDDL,NC,JM,MAXC,IFSAI)
      ELSE
       IF (IFSAI.EQ.1) THEN
        DO I=1,NDDL
         NC(I) = NC(I)+1
         JM(NC(I),I)=I
         DO K =IADM(I),IADM(I+1)-1
          J=JDIM(K)
          NC(J) = NC(J)+1
          JM(NC(J),J)=I
         ENDDO 
         IF (NC(I).GT.MAXC) THEN
          WRITE(*,*)'N>MAXB',NC(I),MAXC,I
          CALL ARRET(2)
         ENDIF 
        ENDDO 
       ELSE
        DO I=1,NDDL
         NC(I) = NC(I)+1
         JM(NC(I),I)=I
         DO K =IADM(I),IADM(I+1)-1
          J=JDIM(K)
          NC(J) = NC(J)+1
          JM(NC(J),J)=I
          NC(I) = NC(I)+1
          JM(NC(I),I)=J
         ENDDO 
         IF (NC(I).GT.MAXC) THEN
          WRITE(*,*)'N>MAXB',NC(I),MAXC,I
          CALL ARRET(2)
         ENDIF 
        ENDDO 
       ENDIF
      ENDIF
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  SP_A2                         source/implicit/imp_fsa_inv.F 
Chd|-- called by -----------
Chd|        SP_STATIC                     source/implicit/imp_fsa_inv.F 
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        INTAB2                        source/implicit/imp_fsa_inv.F 
Chd|====================================================================
      SUBROUTINE SP_A2(NDDL,NC,JM,MAXC,IFSAI)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL,NC(*),MAXC,IFSAI
      INTEGER  JM(MAXC,*)
C-----------------------------------------------
C   External function
C-----------------------------------------------
      INTEGER INTAB2
      EXTERNAL INTAB2
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J,NN(NDDL),JN(MAXC,NDDL)
C-----------------------------------------------
      DO I=1,NDDL
       NN(I)=NC(I)
       NC(I)=0
       DO J=1,NN(I)
        JN(J,I)=JM(J,I)
       ENDDO 
      ENDDO 
      IF (IFSAI.EQ.1) THEN
       DO I=1,NDDL
        NC(I) = NC(I)+1
        JM(NC(I),I)=I
        DO J=I+1,NDDL
         IF(INTAB2(NN(I),JN(1,I),NN(J),JN(1,J)).GT.0) THEN
          NC(J) = NC(J)+1
          JM(NC(J),J)=I
          IF (NC(J).GT.MAXC) THEN
           WRITE(*,*)'N>MAXB',NC(J),MAXC,J
           CALL ARRET(2)
          ENDIF 
         ENDIF
        ENDDO 
       ENDDO 
      ELSE
       DO I=1,NDDL
        NC(I) = NC(I)+1
        JM(NC(I),I)=I
        DO J=I+1,NDDL
         IF(INTAB2(NN(I),JN(1,I),NN(J),JN(1,J)).GT.0) THEN
          NC(I) = NC(I)+1
          JM(NC(I),I)=J
          NC(J) = NC(J)+1
          JM(NC(J),J)=I
          IF (NC(I).GT.MAXC) THEN
           WRITE(*,*)'N>MAXB',NC(I),MAXC,I
           CALL ARRET(2)
          ENDIF 
         ENDIF
        ENDDO 
       ENDDO 
      ENDIF 
C--------------------------------------------
      RETURN
      END
C-------------resol A(N,N).MJ=B  ---B=MJ(input)-
Chd|====================================================================
Chd|  IMP_FSAI                      source/implicit/imp_fsa_inv.F 
Chd|-- called by -----------
Chd|        FSA_SOLV                      source/implicit/imp_fsa_inv.F 
Chd|        IMP_FSA_INV2                  source/implicit/imp_fsa_inv.F 
Chd|        IMP_FSA_INVH                  source/implicit/imp_fsa_inv.F 
Chd|        IMP_FSA_INVH2                 source/implicit/imp_fsa_inv.F 
Chd|        IMP_FSA_INVP                  source/implicit/imp_fsa_inv.F 
Chd|        IMP_FSA_INVP2                 source/implicit/imp_fsa_inv.F 
Chd|        SMS_FSA_INVH                  source/ams/sms_fsa_inv.F      
Chd|-- calls ---------------
Chd|        IMP_FAC_ICJ                   source/implicit/imp_fac_ic.F  
Chd|        PREC0_SOLV                    source/implicit/prec_solv.F   
Chd|====================================================================
      SUBROUTINE IMP_FSAI(N     ,IADA  ,JDIA  ,DIAG_A ,LT_A,
     .                    MAXA  ,MJ    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  N ,IADA(*)  ,JDIA(*),MAXA
C     REAL ou REAL*8
      my_real
     . DIAG_A(*),LT_A(*),MJ(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,IADL(N+1),JDIL(MAXA),NNZL,NNE,IWA1(N)
      my_real
     . LT_L(MAXA),WA1(N),DIAG_L(N)
C-----------------------------------------------
        CALL IMP_FAC_ICJ(
     1                    N     ,MAXA  ,IADA  ,JDIA  ,DIAG_A ,   
     2                    LT_A  ,IADL  ,JDIL  ,DIAG_L,LT_L   ,
     3                    ZERO  ,NNZL  ,MAXA  ,IWA1  ,WA1    ,
     4                    NNE   )
C 
#ifdef MUMPS5
        CALL PREC0_SOLV(N     ,NNZL  ,IADL  ,JDIL  ,DIAG_L ,   
     1                  LT_L  ,MJ    ,WA1   )
#endif 
        DO I=1,N
         MJ(I)=WA1(I)
        ENDDO
C--------------------------------------------
      RETURN
      END
C-------------set submatrix A(N,N) Format m.c.c.s. for FSAI ----
Chd|====================================================================
Chd|  GET_SUBS0                     source/implicit/imp_fsa_inv.F 
Chd|-- called by -----------
Chd|        IMP_FSA_INV2                  source/implicit/imp_fsa_inv.F 
Chd|-- calls ---------------
Chd|        IND_LT2LN                     source/implicit/imp_fsa_inv.F 
Chd|        INTAB0                        source/implicit/imp_fsa_inv.F 
Chd|====================================================================
      SUBROUTINE GET_SUBS0(NDDL  ,IADK  ,JDIK  ,DIAG_K ,LT_K  ,
     .                     NC    ,IADA  ,JDIA  ,DIAG_A ,LT_A  ,   
     .                     JM    ,MAXA  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL  ,IADK(*)  ,JDIK(*),IADA(*)  ,JDIA(*)
      INTEGER  NC    ,JM(*),MAXA
C     REAL ou REAL*8
      my_real
     .  LT_K(*),DIAG_K(*),LT_A(*),DIAG_A(*)
C-----------------------------------------------
C   External function
C-----------------------------------------------
      INTEGER INTAB0
      EXTERNAL INTAB0
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,JJ,NNZA,N
C--------------------------------------------
      NNZA=0
      IADA(1)=1
      DO I=1,NC
       J=JM(I)
        DIAG_A(I)=DIAG_K(J)
        DO K=IADK(J),IADK(J+1)-1
         JJ=JDIK(K)
         N=INTAB0(NC,JM,JJ)
         IF (N.GT.0) THEN
          NNZA=NNZA+1
          JDIA(NNZA)=N
          LT_A(NNZA)=LT_K(K)
         ENDIF
        ENDDO
       IADA(I+1)=NNZA+1
      ENDDO
      CALL IND_LT2LN(NC,IADA  ,JDIA  ,LT_A, NNZA   )
C
      RETURN
      END
C----------version spmd---set submatrix A(N,N) Format m.c.c.s. for FSAI ----
Chd|====================================================================
Chd|  GET_SUBSP                     source/implicit/imp_fsa_inv.F 
Chd|-- called by -----------
Chd|        FSA_SOLV                      source/implicit/imp_fsa_inv.F 
Chd|        IMP_FSA_INVH                  source/implicit/imp_fsa_inv.F 
Chd|        IMP_FSA_INVH2                 source/implicit/imp_fsa_inv.F 
Chd|        IMP_FSA_INVP                  source/implicit/imp_fsa_inv.F 
Chd|        IMP_FSA_INVP2                 source/implicit/imp_fsa_inv.F 
Chd|-- calls ---------------
Chd|        IND_LT2LN                     source/implicit/imp_fsa_inv.F 
Chd|        INTAB0                        source/implicit/imp_fsa_inv.F 
Chd|====================================================================
      SUBROUTINE GET_SUBSP(NDDL  ,IADK  ,JDIK  ,DIAG_K ,LT_K  ,
     .                     NC    ,IADA  ,JDIA  ,DIAG_A ,LT_A  ,   
     .                     JM    ,MAXA  ,IDLFT0,IDLFT1 ,DIAG_C,
     .                     LT_C  ,DIAG_M ,LT_M  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL  ,IADK(*)  ,JDIK(*),IADA(*)  ,JDIA(*)
      INTEGER  NC    ,JM(*),MAXA,IDLFT0,IDLFT1
C     REAL ou REAL*8
      my_real
     .  LT_K(*),DIAG_K(*),LT_A(*),DIAG_A(*),LT_C(*),DIAG_C(*),
     .  DIAG_M(*) ,LT_M(*)
C-----------------------------------------------
C   External function
C-----------------------------------------------
      INTEGER INTAB0
      EXTERNAL INTAB0
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,JJ,NNZA,N,K0
C--------------------------------------------
      NNZA=0
      IADA(1)=1
      K0=IADK(IDLFT1+1)-1
#include      "vectorize.inc"
      DO I=1,NC
       J=JM(I)
       IF (J.LE.IDLFT0) THEN
        DIAG_A(I)=DIAG_M(J)
        DO K=IADK(J),IADK(J+1)-1
         JJ=JDIK(K)
         N=INTAB0(NC,JM,JJ)
         IF (N.GT.0) THEN
          NNZA=NNZA+1
          JDIA(NNZA)=N
          LT_A(NNZA)=LT_M(K)
         ENDIF
        ENDDO
       ELSEIF (J.GT.IDLFT1) THEN
        DIAG_A(I)=DIAG_C(J-IDLFT1)
        DO K=IADK(J),IADK(J+1)-1
         JJ=JDIK(K)
C         IF (JJ.GT.IDLFT1) THEN
          N=INTAB0(NC,JM,JJ)
          IF (N.GT.0) THEN
           NNZA=NNZA+1
           JDIA(NNZA)=N
           LT_A(NNZA)=LT_C(K-K0)
          ENDIF
C         ENDIF
        ENDDO
       ELSE
        DIAG_A(I)=DIAG_K(J)
        DO K=IADK(J),IADK(J+1)-1
         JJ=JDIK(K)
         N=INTAB0(NC,JM,JJ)
         IF (N.GT.0) THEN
          NNZA=NNZA+1
          JDIA(NNZA)=N
          LT_A(NNZA)=LT_K(K)
         ENDIF
        ENDDO
       ENDIF
       IADA(I+1)=NNZA+1
      ENDDO
C      CALL IND_LT2L(NC,IADA  ,JDIA  ,LT_A, NNZA   )
      CALL IND_LT2LN(NC,IADA  ,JDIA  ,LT_A, NNZA   )
C
      RETURN
      END
C-------------set submatrix A(N,N) Format m.c.r.s. for FSAI ----
Chd|====================================================================
Chd|  GET_SUBSA                     source/implicit/imp_fsa_inv.F 
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        INTAB0                        source/implicit/imp_fsa_inv.F 
Chd|====================================================================
      SUBROUTINE GET_SUBSA(NDDL  ,IADK  ,JDIK  ,DIAG_K ,LT_K  ,
     .                     NC    ,IADA  ,JDIA  ,DIAG_A ,LT_A  ,   
     .                     JM    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL  ,IADK(*)  ,JDIK(*),IADA(*)  ,JDIA(*)
      INTEGER  NC    ,JM(*)
C     REAL ou REAL*8
      my_real
     .  LT_K(*),DIAG_K(*),LT_A(*),DIAG_A(*)
C-----------------------------------------------
C   External function
C-----------------------------------------------
      INTEGER INTAB0
      EXTERNAL INTAB0
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,JJ,NNZA,N
C--------------------------------------------
      NNZA=0
      IADA(1)=1
      DO I=1,NC
       J=JM(I)
        DIAG_A(I)=DIAG_K(J)
        DO K=IADK(J),IADK(J+1)-1
         JJ=JDIK(K)
         N=INTAB0(NC,JM,JJ)
         IF (N.GT.0) THEN
          NNZA=NNZA+1
          JDIA(NNZA)=N
          LT_A(NNZA)=LT_K(K)
         ENDIF
        ENDDO
       IADA(I+1)=NNZA+1
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  INTAB0                        source/implicit/imp_fsa_inv.F 
Chd|-- called by -----------
Chd|        DIM_SUBNZ                     source/implicit/imp_solv.F    
Chd|        GET_KIJS                      source/implicit/imp_pc_inv.F  
Chd|        GET_SUBS0                     source/implicit/imp_fsa_inv.F 
Chd|        GET_SUBSA                     source/implicit/imp_fsa_inv.F 
Chd|        GET_SUBSN                     source/implicit/imp_fsa_inv.F 
Chd|        GET_SUBSP                     source/implicit/imp_fsa_inv.F 
Chd|        GET_SUBSP_SMS                 source/ams/sms_fsa_inv.F      
Chd|        SPC_FR_K                      source/mpi/implicit/imp_fri.F 
Chd|        UPD_FR_K                      source/mpi/implicit/imp_fri.F 
Chd|        INTAB2                        source/implicit/imp_fsa_inv.F 
Chd|-- calls ---------------
Chd|====================================================================
      INTEGER FUNCTION INTAB0(NIC,IC,N)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N ,NIC,IC(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
C----6-----IC est deja en ordre croissante---------------------------7---------8
      INTAB0=0
      IF (N.LT.IC(1).OR.N.GT.IC(NIC)) RETURN
      IF (N.LT.NIC/2) THEN
       DO I =1,NIC
        IF (N.EQ.IC(I)) THEN
         INTAB0=I
         RETURN
        ENDIF
       ENDDO 
      ELSE
       DO I =NIC,1,-1
        IF (N.EQ.IC(I)) THEN
         INTAB0=I
         RETURN
        ENDIF
       ENDDO 
      ENDIF 
C
      RETURN
      END
Chd|====================================================================
Chd|  INTAB2                        source/implicit/imp_fsa_inv.F 
Chd|-- called by -----------
Chd|        GET_SUBA                      source/implicit/imp_pc_inv.F  
Chd|        SP_A2                         source/implicit/imp_fsa_inv.F 
Chd|-- calls ---------------
Chd|        INTAB0                        source/implicit/imp_fsa_inv.F 
Chd|====================================================================
      INTEGER FUNCTION INTAB2(NIC1,IC1,NIC2,IC2)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIC1,IC1(*),NIC2,IC2(*)
C-----------------------------------------------
C   External function
C-----------------------------------------------
      INTEGER INTAB0
      EXTERNAL INTAB0
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
C----6-----ICi est deja en ordre croissante--return indice dans IC1--7---------8
      INTAB2=0
      IF (IC1(NIC1).LT.IC2(1).OR.IC2(NIC2).LT.IC1(1)) RETURN
C      IF (NIC1.GT.NIC2) THEN
       DO I =1,NIC2
        INTAB2=INTAB0(NIC1,IC1,IC2(I))
        IF (INTAB2.GT.0) RETURN
       ENDDO 
      RETURN
      END
Cqz------factorized sparse approximate inverse version spmd------- 
Chd|====================================================================
Chd|  IMP_FSA_INVP                  source/implicit/imp_fsa_inv.F 
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        GET_SUBSP                     source/implicit/imp_fsa_inv.F 
Chd|        IMP_FSAI                      source/implicit/imp_fsa_inv.F 
Chd|        IMP_PCG1                      source/implicit/imp_fsa_inv.F 
Chd|        SP_STAT0                      source/implicit/imp_fsa_inv.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE IMP_FSA_INVP(
     1                    NDDL  ,NNZ   ,IADK  ,JDIK  ,DIAG_K ,   
     2                    LT_K  ,DIAG_M ,LT_M  ,MAXC  ,MAX_A ,
     3                    NNE   ,IDLFT0 ,IDLFT1,MAX_D )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc" 
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL ,NNZ ,IADK(*),JDIK(*),MAXC ,MAX_A ,NNE,
     .         IDLFT0 ,IDLFT1,MAX_D
C     REAL ou REAL*8
      my_real
     .  DIAG_K(*), LT_K(*),DIAG_M(*), LT_M(*)   
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C--- M->A^-1 LT_M  stricly lower in m.c.r.s. format
      INTEGER I,J,K,M,N,NC,IADA(MAXC+1),JDIA(MAX_A),JM(MAXC+1)
      INTEGER MAX_L,IERR,I_CHK
      my_real
     .   DIAG_A(MAXC),MJ(MAXC),
     .   DIAG_C(NDDL-IDLFT1+1),LT_C(MAX_D+1)
      my_real, DIMENSION(:),ALLOCATABLE :: LT_A
C-----------------------------
       ALLOCATE(LT_A(MAX_A),STAT=IERR)
        IF (IERR.NE.0) THEN
          CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .                C1='FOR IMPLICIT PRECONDITION')
          CALL ARRET(2)
        ENDIF 
C--------------copy la partie utile------
       I_CHK = NNE
       NNE = 0
       K=IADK(IDLFT1+1)-1
       DO I=IDLFT1+1,NDDL
         DIAG_C(I-IDLFT1) = DIAG_M(I)
         DO J=IADK(I),IADK(I+1)-1
          LT_C(J-K)=LT_M(J)
         ENDDO 
       ENDDO 
       DO I=IDLFT0+1,NDDL
        CALL SP_STAT0(I  ,IADK  ,JDIK  ,NC    ,JM    )
        CALL GET_SUBSP(NDDL  ,IADK  ,JDIK  ,DIAG_K ,LT_K  ,
     .                 NC    ,IADA  ,JDIA  ,DIAG_A ,LT_A  ,   
     .                 JM    ,MAX_A ,IDLFT0,IDLFT1 ,DIAG_C,
     .                 LT_C  ,DIAG_M ,LT_M  )
        DO J=1,NC-1
         MJ(J)=ZERO
        ENDDO
        MJ(NC)=UN
        IF (NC.GT.10000) THEN
          MAX_L=MAX_A
          CALL IMP_PCG1( 
     1                    NC    ,MAX_L  ,IADA  ,JDIA  ,DIAG_A ,   
     2                    LT_A  ,MJ     ,IERR  )
         IF (I_CHK.GT.0.AND.IERR.LT.0) NNE = I
        ELSE
        MAX_L=1+(NC*(NC-1))/2
        CALL IMP_FSAI(NC    ,IADA  ,JDIA  ,DIAG_A ,LT_A  ,
     .               MAX_L ,MJ    )
        ENDIF 
C------------filtrage----Diagonal est dans LT_M (last one)--
         DIAG_M(I)=MJ(NC)
         IF (DIAG_M(I).LT.EM20) THEN
          IF (NNE.EQ.0.AND.I_CHK.EQ.0) NNE = I
          DIAG_M(I)=ABS(DIAG_M(I))
          DIAG_M(I)=MAX(EM20,DIAG_M(I))
         ENDIF
         DO K =1,NC-1
          M=IADK(I)+K-1
          LT_M(M)=MJ(K)/DIAG_M(I)
         ENDDO
         IF (I_CHK.GT.0.AND.MJ(NC).LT.EM20) DIAG_M(I)=MJ(NC)
       ENDDO
       DEALLOCATE(LT_A)
C 
      RETURN
      END
Chd|====================================================================
Chd|  IMP_PCG1                      source/implicit/imp_fsa_inv.F 
Chd|-- called by -----------
Chd|        FSA_SOLV                      source/implicit/imp_fsa_inv.F 
Chd|        IMP_FSA_INV2                  source/implicit/imp_fsa_inv.F 
Chd|        IMP_FSA_INVH                  source/implicit/imp_fsa_inv.F 
Chd|        IMP_FSA_INVH2                 source/implicit/imp_fsa_inv.F 
Chd|        IMP_FSA_INVP                  source/implicit/imp_fsa_inv.F 
Chd|        IMP_FSA_INVP2                 source/implicit/imp_fsa_inv.F 
Chd|-- calls ---------------
Chd|        MAV_LT                        source/implicit/produt_v.F    
Chd|        PRODUT_V0                     source/implicit/produt_v.F    
Chd|        CRIT_STOP                     source/implicit/imp_pcg.F     
Chd|====================================================================
      SUBROUTINE IMP_PCG1( 
     1                    NDDL  ,NNZ   ,IADK  ,JDIK  ,DIAG_K ,   
     2                    LT_K  ,R     ,ISP   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "impl2_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C----------resol [K]{X}={F}---------
      INTEGER  NDDL  ,NNZ   ,IADK(*)  ,JDIK(*)
C     REAL ou REAL*8
      my_real
     .  DIAG_K(*), LT_K(*) ,R(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IT,IP,NLIM,ND,IPRE,NNZM,ISTOP,ITOL,ISP
      my_real
     .   S , R2, R02,ALPHA,BETA,G0,G1,RR,TOLS,TOLN,TOLS2
      my_real
     .  X(NDDL) ,P(NDDL) ,Z(NDDL)  ,Y(NDDL),DIAG_M(NDDL) 
      INTEGER CRIT_STOP
      EXTERNAL CRIT_STOP
      my_real
     .  ANORM2,XNORM2,L_A,L_B2,L_B,A_OLD,B_OLD,TMP,EPS_M
      my_real
     .  CS,DBAR, DELTA, DENOM, KCOND,SNPROD,QRNORM,
     .  GAMMA, GBAR, GMAX, GMIN, EPSLN,LQNORM,DIAG,CGNORM,
     .  OLDB, RHS1, RHS2,SN, ZBAR, ZL ,OLDB2,TNORM2,EPS(4)
C--------------INITIALISATION--------------------------
      ITOL = 1
      IPRE = 2
      NNZM = 0
      NLIM=MAX(NDDL,2)
      TOLS=SQRT(P_MACH)
      EPS_M = P_MACH
      IT=0
      ND = NDDL
C-------------IT=0--------
C------X(I)=ZERO--------
       DO I=1,NDDL
        X(I) = ZERO
        DIAG_M(I)=UN/MAX(EM20,DIAG_K(I))
       ENDDO 
       CALL MAV_LT(
     1            NDDL  ,NNZ   ,IADK  ,JDIK  ,DIAG_K,   
     2            LT_K  ,X     ,Z     )
       DO I=1,NDDL
        R(I) = R(I)-Z(I)
       ENDDO 
        DO I=1,NDDL
         Z(I) = R(I) *DIAG_M(I)
        ENDDO 
       DO I=1,NDDL
        P(I) = Z(I)
       ENDDO 
       CALL PRODUT_V0(NDDL,R,Z,G0)
       CALL MAV_LT(
     1            NDDL  ,NNZ   ,IADK  ,JDIK  ,DIAG_K,   
     2            LT_K  ,P     ,Y     )
       CALL PRODUT_V0(NDDL,P,Y,S)
       ALPHA = G0/S
       TOLS2=TOLS*TOLS
       IF (ITOL.EQ.1) THEN
        CALL PRODUT_V0(NDDL,R,R,R02)
        R2 =R02
       ELSEIF (ITOL.EQ.3) THEN
C------ R2<TOL*TOL*ANORM2*XNORM2------
        R02=ABS(G0)
        R2 =R02
        L_A=UN/ALPHA
C        L_B2=R02
        TNORM2=L_A*L_A
        ANORM2=ZERO
        XNORM2=ZERO
        A_OLD=L_A
        B_OLD=ZERO
        OLDB   = SQRT(R02)
       ELSEIF (ITOL.EQ.4) THEN
        R02=ALPHA*ALPHA*ABS(G0)
        EPS(1)=R02
        R2=R02
       ELSE
        R02=ABS(G0)
        R2 =R02
       ENDIF
       IF (R02.EQ.ZERO) GOTO 200
       TOLN=R02*TOLS2
C-------pour etre coherent avec lanzos for linear
       IT=1
       DO I=1,NDDL
         X(I) = X(I) + ALPHA*P(I)
         R(I) = R(I) - ALPHA*Y(I)
       ENDDO 
        DO I=1,NDDL
         Z(I) = R(I) *DIAG_M(I)
        ENDDO 
       CALL PRODUT_V0(NDDL,R,Z,G1)
       BETA=G1/G0
       IF (ITOL.EQ.1) THEN
        CALL PRODUT_V0(NDDL,R,R,R2)
       ELSEIF (ITOL.EQ.3) THEN
C------ R2<TOL*TOL*ANORM2*XNORM2------
        R2=ABS(G1)
        L_B2=ABS(BETA)*A_OLD*A_OLD
        L_B=SQRT(L_B2)
        TNORM2=TNORM2+L_B2
        B_OLD=BETA
C*     INITIALIZE OTHER QUANTITIES.
        GBAR   = L_A
        DBAR   = L_B
        RHS1   = OLDB
        RHS2   = ZERO
        GMAX   = ABS( L_A ) + EPS_M
        GMIN   = GMAX
        OLDB2   = L_B2
        OLDB   = L_B
       ELSEIF (ITOL.EQ.4) THEN
        R2=R02
       ELSE
        R2=ABS(G1)
       ENDIF
       G0 = G1
       IF (ITOL.EQ.3) TOLN=TOLN*ANORM2
       ISTOP=CRIT_STOP(IT,R2,NLIM,TOLN)
       DO WHILE (ISTOP.EQ.1)
        DO I=1,NDDL
         P(I) = Z(I) + BETA*P(I)
        ENDDO 
       CALL MAV_LT(
     1            NDDL  ,NNZ   ,IADK  ,JDIK  ,DIAG_K,   
     2            LT_K  ,P     ,Y     )
        CALL PRODUT_V0(NDDL,P,Y,S)
        ALPHA=G0/S
        DO I=1,NDDL
         X(I) = X(I) + ALPHA*P(I)
         R(I) = R(I) - ALPHA*Y(I)
        ENDDO 
        DO I=1,NDDL
         Z(I) = R(I) *DIAG_M(I)
        ENDDO 
        CALL PRODUT_V0(NDDL,R,Z,G1)
        BETA=G1/G0
        G0 = G1
        IF (ITOL.EQ.1) THEN
         CALL PRODUT_V0(NDDL,R,R,R2)
        ELSEIF (ITOL.EQ.3) THEN
         R2 =ABS(G1)
         S=UN/ALPHA
         L_A=S+B_OLD*A_OLD
C----------L_B2 : beta(j-1)^2-A_OLD :1/alpha(j-1)-------
         L_B2=ABS(BETA)*S*S
         L_B=SQRT(L_B2)
         A_OLD=S
         B_OLD=BETA
         ANORM2=TNORM2
         TNORM2=TNORM2+L_A*L_A+OLDB2+L_B2
         GAMMA  = SQRT( GBAR*GBAR + OLDB2 )
         CS     = GBAR / GAMMA
         SN     = OLDB / GAMMA
         DELTA  = CS * DBAR  +  SN * L_A
         GBAR   = SN * DBAR  -  CS * L_A
         EPSLN  = SN * L_B
         DBAR   =            -  CS * L_B
         ZL     = RHS1 / GAMMA
         XNORM2 = XNORM2+ZL*ZL
         GMAX   = MAX( GMAX, GAMMA )
         GMIN   = MIN( GMIN, GAMMA )
         RHS1   = RHS2  -  DELTA * ZL
         RHS2   =       -  EPSLN * ZL
         TOLN=TOLS2*ANORM2*XNORM2
         OLDB2   = L_B2
         OLDB   = L_B
        ELSEIF (ITOL.EQ.4) THEN
         TMP=ALPHA*ALPHA*ABS(G1)
         IF (IT.GE.ND) THEN
          DO J=1,ND-1
           EPS(J)=EPS(J+1)
          ENDDO
          EPS(ND)=TMP
          R2=ZERO
          DO J=1,ND
           R2=R2+EPS(J)
          ENDDO
         ELSE
          EPS(IT+1)=TMP
          R2=R2+TMP
         ENDIF
        ELSE
         R2 =ABS(G1)
        ENDIF
        ISTOP=CRIT_STOP(IT,R2,NLIM,TOLN)
        IT = IT +1
       ENDDO
 200   CONTINUE
       IF(IT.GE.NLIM)THEN
        ISP =-1
       ELSE
        ISP = 0
       ENDIF
C        RR = SQRT(R2/R02)
C        WRITE(*,1002)IT,RR
C--------X->R--------
       DO I=1,NDDL
        R(I) = X(I) 
       ENDDO 
C--------------------------------------------
 1002 FORMAT(3X,'TOTAL C.G. ITERATION=',I8,5X,
     .          ' RELATIVE RESIDUAL NORM=',E11.4)
 1003 FORMAT(5X,
     . '---WARNING : THE ITERATION LIMIT NUMBER WAS REACHED',
     . 1X,'IN PRECONDITIONER')
       RETURN
      END
Chd|====================================================================
Chd|  IMP_FSA_INV2                  source/implicit/imp_fsa_inv.F 
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        GET_SUBS0                     source/implicit/imp_fsa_inv.F 
Chd|        IMP_FSAI                      source/implicit/imp_fsa_inv.F 
Chd|        IMP_KFILTR                    source/implicit/imp_fsa_inv.F 
Chd|        IMP_PCG1                      source/implicit/imp_fsa_inv.F 
Chd|        SP_STAT0                      source/implicit/imp_fsa_inv.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE IMP_FSA_INV2(
     1                    NDDL   ,IADK   ,JDIK   ,DIAG_K ,LT_K  ,   
     2                    IADM   ,JDIM   ,DIAG_M ,LT_M   ,MAXC  ,
     3                    MAX_A  ,NNE    ,D_TOL  ,P_MACH)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc" 
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL ,IADK(*),JDIK(*),MAXC ,MAX_A ,NNE,
     .         IADM(*),JDIM(*)   
C     REAL ou REAL*8
      my_real
     .  DIAG_K(*), LT_K(*),DIAG_M(*), LT_M(*) ,D_TOL  ,P_MACH  
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C--- M->A^-1 LT_M  stricly lower in m.c.r.s. format
      INTEGER I,J,K,M,N,NC,IERR
      INTEGER MAX_L,I_CHK
      INTEGER, DIMENSION(:),ALLOCATABLE :: IADA,JDIA,JM
      my_real, DIMENSION(:),ALLOCATABLE :: DIAG_A,LT_A,MJ
C-----------------------------
      I_CHK = NNE
      NNE = 0
       ALLOCATE(IADA(MAXC+1))
       ALLOCATE(JDIA(MAX_A))
       ALLOCATE(JM(MAXC+1))
       ALLOCATE(DIAG_A(MAXC))
       ALLOCATE(MJ(MAXC))
       ALLOCATE(LT_A(MAX_A),STAT=IERR)
         IF (IERR.NE.0) THEN
           CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .                 C1='FOR IMPLICIT PRECONDITION')
           CALL ARRET(2)
         ENDIF
       DO I=1,NDDL
        CALL SP_STAT0(I  ,IADM  ,JDIM  ,NC    ,JM    )
        CALL GET_SUBS0(NDDL  ,IADK  ,JDIK  ,DIAG_K ,LT_K  ,
     .                 NC    ,IADA  ,JDIA  ,DIAG_A ,LT_A  ,   
     .                 JM    ,MAX_A )
        DO J=1,NC-1
         MJ(J)=ZERO
        ENDDO
        MJ(NC)=UN
        IF (NC.GT.10000) THEN
          MAX_L=MAX_A
          CALL IMP_PCG1( 
     1                    NC    ,MAX_L  ,IADA  ,JDIA  ,DIAG_A ,   
     2                    LT_A  ,MJ     ,IERR  )
         IF (I_CHK.GT.0.AND.IERR.LT.0) NNE = I
        ELSE
         MAX_L=1+(NC*(NC-1))/2
         CALL IMP_FSAI(NC    ,IADA  ,JDIA  ,DIAG_A ,LT_A  ,
     .               MAX_L ,MJ    )
        ENDIF
C-----------------
         DIAG_M(I)=MJ(NC)
         IF (DIAG_M(I).LT.EM20) THEN
          IF (NNE.EQ.0.AND.I_CHK.EQ.0) NNE = I
          DIAG_M(I)=ABS(DIAG_M(I))
          DIAG_M(I)=MAX(EM20,DIAG_M(I))
         ENDIF
         DO K =1,NC-1
          M=IADM(I)+K-1
          LT_M(M)=MJ(K)/DIAG_M(I)
         ENDDO
         IF (I_CHK.GT.0.AND.MJ(NC).LT.EM20) DIAG_M(I)=MJ(NC)
       ENDDO
C
       DEALLOCATE(IADA,JDIA)
       DEALLOCATE(JM)
       DEALLOCATE(DIAG_A,LT_A)
       DEALLOCATE(MJ)
       K = 1
       IF (D_TOL.GT.ZERO)
     .  CALL IMP_KFILTR(K     ,NDDL  ,IADM  ,JDIM  ,DIAG_M ,
     .                  LT_M  ,D_TOL ,P_MACH,DIAG_K)
C
      RETURN
      END
Chd|====================================================================
Chd|  IMP_FSA_INVP2                 source/implicit/imp_fsa_inv.F 
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        GET_SUBSP                     source/implicit/imp_fsa_inv.F 
Chd|        IMP_FSAI                      source/implicit/imp_fsa_inv.F 
Chd|        IMP_KFILTR                    source/implicit/imp_fsa_inv.F 
Chd|        IMP_PCG1                      source/implicit/imp_fsa_inv.F 
Chd|        SP_STAT0                      source/implicit/imp_fsa_inv.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE IMP_FSA_INVP2(
     1                    NDDL   ,IADK   ,JDIK   ,DIAG_K ,LT_K   ,   
     2                    IADM   ,JDIM   ,DIAG_M ,LT_M   ,MAXC   ,
     3                    MAX_A  ,NNE    ,IDLFT0 ,IDLFT1 ,MAX_D  ,
     4                    D_TOL  ,P_MACH)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc" 
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL ,NNZ ,IADK(*),JDIK(*),MAXC ,MAX_A ,NNE,
     .         IDLFT0 ,IDLFT1,MAX_D,IADM(*),JDIM(*)
C     REAL ou REAL*8
      my_real
     .  DIAG_K(*), LT_K(*),DIAG_M(*), LT_M(*),D_TOL  ,P_MACH   
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C--- M->A^-1 LT_M  stricly lower in m.c.r.s. format
      INTEGER I,J,K,M,N,NC,IADA(MAXC+1),JDIA(MAX_A),JM(MAXC+1)
      INTEGER MAX_L,IERR,I_CHK
      my_real
     .   DIAG_A(MAXC),MJ(MAXC),
     .   DIAG_C(NDDL-IDLFT1+1),LT_C(MAX_D+1)
      my_real, DIMENSION(:),ALLOCATABLE :: LT_A
C-----------------------------
        ALLOCATE(LT_A(MAX_A),STAT=IERR)
        IF (IERR.NE.0) THEN
          CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .                 C1='FOR IMPLICIT PRECONDITION')
          CALL ARRET(2)
        ENDIF 
C--------------copy la partie utile------
       I_CHK = NNE
       NNE = 0
       K=IADK(IDLFT1+1)-1
       DO I=IDLFT1+1,NDDL
         DIAG_C(I-IDLFT1) = DIAG_M(I)
         DO J=IADK(I),IADK(I+1)-1
          LT_C(J-K)=LT_M(J)
         ENDDO 
       ENDDO 
       DO I=IDLFT0+1,NDDL
        CALL SP_STAT0(I  ,IADM  ,JDIM  ,NC    ,JM    )
        CALL GET_SUBSP(NDDL  ,IADK  ,JDIK  ,DIAG_K ,LT_K  ,
     .                 NC    ,IADA  ,JDIA  ,DIAG_A ,LT_A  ,   
     .                 JM    ,MAX_A ,IDLFT0,IDLFT1 ,DIAG_C,
     .                 LT_C  ,DIAG_M,LT_M  )
        DO J=1,NC-1
         MJ(J)=ZERO
        ENDDO
        MJ(NC)=UN
        IF (NC.GT.10000) THEN
          MAX_L=MAX_A
          CALL IMP_PCG1( 
     1                    NC    ,MAX_L  ,IADA  ,JDIA  ,DIAG_A ,   
     2                    LT_A  ,MJ     ,IERR  )
         IF (I_CHK.GT.0.AND.IERR.LT.0) NNE = I
        ELSE
         MAX_L=1+(NC*(NC-1))/2
         CALL IMP_FSAI(NC    ,IADA  ,JDIA  ,DIAG_A ,LT_A  ,
     .               MAX_L ,MJ    )
        ENDIF 
C------------Diagonal est dans LT_M (last one)--
         DIAG_M(I)=MJ(NC)
         IF (DIAG_M(I).LT.EM20) THEN
          IF (NNE.EQ.0.AND.I_CHK.EQ.0) NNE = I
          DIAG_M(I)=ABS(DIAG_M(I))
          DIAG_M(I)=MAX(EM20,DIAG_M(I))
         ENDIF
         DO K =1,NC-1
          M=IADM(I)+K-1
          LT_M(M)=MJ(K)/DIAG_M(I)
         ENDDO
         IF (I_CHK.GT.0.AND.MJ(NC).LT.EM20) DIAG_M(I)=MJ(NC)
       ENDDO
C
       DEALLOCATE(LT_A)
       K = IDLFT0+1
       IF (D_TOL.GT.ZERO)
     .  CALL IMP_KFILTR(K     ,NDDL  ,IADM  ,JDIM  ,DIAG_M ,
     .                  LT_M  ,D_TOL ,P_MACH,DIAG_K)
C 
      RETURN
      END
Chd|====================================================================
Chd|  IMP_KFILTR                    source/implicit/imp_fsa_inv.F 
Chd|-- called by -----------
Chd|        IMP_FSA_INV2                  source/implicit/imp_fsa_inv.F 
Chd|        IMP_FSA_INV2HP                source/implicit/imp_fsa_inv.F 
Chd|        IMP_FSA_INVH2                 source/implicit/imp_fsa_inv.F 
Chd|        IMP_FSA_INVP2                 source/implicit/imp_fsa_inv.F 
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        CP_INT                        source/implicit/produt_v.F    
Chd|        CP_REAL                       source/implicit/produt_v.F    
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE IMP_KFILTR(NDF   ,ND    ,IADA  ,JDIA  ,DIAG_A ,
     .                      LT_A  ,TOL   ,E_PS  ,DIAG_K )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc" 
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDF,ND ,IADA(*)  ,JDIA(*)
C     REAL ou REAL*8
      my_real
     . DIAG_A(*),LT_A(*),TOL,E_PS,DIAG_K(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J,K,NZ,IERR,MNZ,INORM
      INTEGER, DIMENSION(:),ALLOCATABLE :: IADL,JDIL
      my_real
     .   MIN_D,MAX_D,MTOL,DD,TAUX
      my_real, DIMENSION(:),ALLOCATABLE :: LT_L
C-----------------------------
       print *,'D_tol,p_mach=',tol,e_ps
       NZ = IADA(ND+1)-IADA(1)
       DO I  = 1, NDF-1
	    DIAG_A(I) = ZERO
        DO J  = IADA(I), IADA(I+1)-1
	     LT_A(J) = ZERO
        ENDDO
        IADA(I+1) = 1
       ENDDO
      IF (NZ.GT.0.AND.TOL.GT.ZERO) THEN
       ALLOCATE(IADL(ND+1))
       ALLOCATE(JDIL(NZ),LT_L(NZ),STAT=IERR)
         IF (IERR.NE.0) THEN
           CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .                 C1='FOR IMPLICIT PRECONDITION')
           CALL ARRET(2)
         ENDIF
C-----------------------------------------------
       CALL CP_INT(ND+1,IADA,IADL)
       CALL CP_INT(NZ,JDIA,JDIL)
       CALL CP_REAL(NZ,LT_A,LT_L)
       MAX_D = ZERO
       MIN_D = EP20
       DO I  = NDF, ND
        MAX_D = MAX(MAX_D,DIAG_A(I))
        MIN_D = MIN(MIN_D,DIAG_A(I))
       ENDDO
	print *,'max_d,min_d=',max_d,min_d
c       MTOL = TOL*MIN_D
c       MTOL = MAX(E_PS,MTOL)
C------post-filtration------
       MNZ = 0
       DO I  = NDF, ND
        DO J  = IADL(I), IADL(I+1)-1
         MTOL = TOL*MIN(DIAG_A(JDIL(J)),DIAG_A(I))
         MTOL = MAX(E_PS,MTOL)
         IF (ABS(LT_L(J)).GT.MTOL) THEN
	      MNZ = MNZ + 1
	      JDIA(MNZ) = JDIL(J)
	      LT_A(MNZ) = LT_L(J)
	     ENDIF
        ENDDO
	    IADA(I+1) = MNZ + 1
       ENDDO
       DEALLOCATE(IADL,JDIL)
       DEALLOCATE(LT_L)
       TAUX = UN*MNZ/NZ
c	print *,'mnz,nz=',mnz,nz,mtol
        print *,'filtrage factor=',TAUX
C------retrun from M normalized------
      ENDIF 
      INORM = 0
      IF (INORM.GT.ZERO) THEN
       DO I  = NDF, ND
	    DIAG_A(I) = DIAG_A(I)/DIAG_K(I)
        DO J  = IADA(I), IADA(I+1)-1
	     DD = SQRT(DIAG_K(I)/DIAG_K(JDIA(J)))
	     LT_A(J) = DD*LT_A(J)
        ENDDO
       ENDDO
      ENDIF 
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  GET_SUBSN                     source/implicit/imp_fsa_inv.F 
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        IND_LT2LN                     source/implicit/imp_fsa_inv.F 
Chd|        INTAB0                        source/implicit/imp_fsa_inv.F 
Chd|====================================================================
      SUBROUTINE GET_SUBSN(NDDL  ,IADK  ,JDIK  ,DIAG_K ,LT_K  ,
     .                     NC    ,IADA  ,JDIA  ,DIAG_A ,LT_A  ,   
     .                     JM    ,MAXA  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL  ,IADK(*)  ,JDIK(*),IADA(*)  ,JDIA(*)
      INTEGER  NC    ,JM(*),MAXA
C     REAL ou REAL*8
      my_real
     .  LT_K(*),DIAG_K(*),LT_A(*),DIAG_A(*)
C-----------------------------------------------
C   External function
C-----------------------------------------------
      INTEGER INTAB0
      EXTERNAL INTAB0
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,JJ,NNZA,N
      my_real
     .  DD
C--------------------------------------------
      NNZA=0
      IADA(1)=1
      DO I=1,NC
       J=JM(I)
        DIAG_A(I)=UN
        DO K=IADK(J),IADK(J+1)-1
         JJ=JDIK(K)
         N=INTAB0(NC,JM,JJ)
         IF (N.GT.0) THEN
	  DD = SQRT(DIAG_K(J)*DIAG_K(JJ))
          NNZA=NNZA+1
          JDIA(NNZA)=N
          LT_A(NNZA)=LT_K(K)/DD
         ENDIF
        ENDDO
       IADA(I+1)=NNZA+1
      ENDDO
      CALL IND_LT2LN(NC,IADA  ,JDIA  ,LT_A, NNZA   )
C
      RETURN
      END
Chd|====================================================================
Chd|  IND_LT2LN                     source/implicit/imp_fsa_inv.F 
Chd|-- called by -----------
Chd|        GET_SUBS0                     source/implicit/imp_fsa_inv.F 
Chd|        GET_SUBSN                     source/implicit/imp_fsa_inv.F 
Chd|        GET_SUBSP                     source/implicit/imp_fsa_inv.F 
Chd|        GET_SUBSP_SMS                 source/ams/sms_fsa_inv.F      
Chd|-- calls ---------------
Chd|        CP_INT                        source/implicit/produt_v.F    
Chd|        CP_REAL                       source/implicit/produt_v.F    
Chd|====================================================================
       SUBROUTINE IND_LT2LN(NDDL,IADK  ,JDIK  ,LT_K, MAXL   )
C-------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL, IADK(*),JDIK(*),MAXL
      my_real
     .  LT_K(*)
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  IADM(NDDL+1),JDIM(MAXL),ICOL(NDDL)
      INTEGER  I,JD,J,K,N,NM
      my_real
     .  LT_M(MAXL)
C-----------------------------------------------
        CALL CP_INT(NDDL+1,IADK,IADM)
        CALL CP_INT(MAXL,JDIK,JDIM)
        CALL CP_REAL(MAXL,LT_K,LT_M)
C
       DO I = 1, NDDL
        ICOL(I) = 0
        DO J = IADM(I),IADM(I+1)-1
         JD = JDIM(J)
         ICOL(JD) = ICOL(JD) + 1
        ENDDO
       ENDDO
C
       IADK(1) = 1
       DO I = 1,NDDL
        IADK(I+1) = IADK(I)+ICOL(I)
       ENDDO
C
       DO I = 1,NDDL
        ICOL(I) = 0
        DO J=IADM(I),IADM(I+1)-1
         JD = JDIM(J)
         K = IADK(JD) + ICOL(JD)
         JDIK(K) = I
         LT_K(K) = LT_M(J)
         ICOL(JD) = ICOL(JD) + 1
        ENDDO 
       ENDDO
C--------------------------------------------
      RETURN
      END
Cqz------factorized sparse approximate inverse version hybrid------- 
Chd|====================================================================
Chd|  IMP_FSA_INVH                  source/implicit/imp_fsa_inv.F 
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        GET_SUBSP                     source/implicit/imp_fsa_inv.F 
Chd|        IMP_FSAI                      source/implicit/imp_fsa_inv.F 
Chd|        IMP_PCG1                      source/implicit/imp_fsa_inv.F 
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SP_STAT0                      source/implicit/imp_fsa_inv.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE IMP_FSA_INVH(
     1                    NDDL  ,NNZ   ,IADK  ,JDIK  ,DIAG_K ,   
     2                    LT_K  ,DIAG_M ,LT_M  ,MAXC  ,MAX_A ,
     3                    NNE   ,IDLFT0 ,IDLFT1,MAX_D ,ITASK )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc" 
#include      "task_c.inc" 
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL ,NNZ ,IADK(*),JDIK(*),MAXC ,MAX_A ,NNE,
     .         IDLFT0 ,IDLFT1,MAX_D,ITASK
C     REAL ou REAL*8
      my_real
     .  DIAG_K(*), LT_K(*),DIAG_M(*), LT_M(*)   
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C--- M->A^-1 LT_M  stricly lower in m.c.r.s. format
      INTEGER I,J,K,M,N,NC,MAX_L,IERR,I_CHK,IER1,
     .        JM(MAXC+1)
      my_real
     .   DIAG_C(NDDL-IDLFT1+1),LT_C(MAX_D+1)
      INTEGER, DIMENSION(:),ALLOCATABLE :: IADA,JDIA
      my_real, DIMENSION(:),ALLOCATABLE :: DIAG_A,LT_A,MJ
C-----------------------------
       I_CHK = NNE
       NNE = 0
       IF ((IDLFT0+1).GT.NDDL) RETURN
C
       ALLOCATE(IADA(MAXC+1),DIAG_A(MAXC),MJ(MAXC),STAT=IER1)
       ALLOCATE(LT_A(MAX_A),JDIA(MAX_A),STAT=IERR)
 
        IF ((IERR+IER1).NE.0) THEN
         IF (ITASK == 0 ) THEN
           CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .                 C1='FOR IMPLICIT PRECONDITION')
           CALL ARRET(2)
         END IF !(ITASK == 0 ) THEN
        ENDIF 

C--------------copy la partie utile------
       K=IADK(IDLFT1+1)-1
       DO I=IDLFT1+1,NDDL
         DIAG_C(I-IDLFT1) = DIAG_M(I)
         DO J=IADK(I),IADK(I+1)-1
          LT_C(J-K)=LT_M(J)
         ENDDO 
       ENDDO 
C----------------------
      CALL MY_BARRIER
C---------------------
C
C Boucle parallele dynamique SMP
C
!$OMP DO SCHEDULE(DYNAMIC,1)
      DO I=IDLFT0+1,NDDL
        CALL SP_STAT0(I  ,IADK  ,JDIK  ,NC    ,JM    )
        CALL GET_SUBSP(NDDL  ,IADK  ,JDIK  ,DIAG_K ,LT_K  ,
     .                 NC    ,IADA  ,JDIA  ,DIAG_A ,LT_A  ,   
     .                 JM    ,MAX_A ,IDLFT0,IDLFT1 ,DIAG_C,
     .                 LT_C  ,DIAG_M ,LT_M  )
        DO J=1,NC-1
         MJ(J)=ZERO
        ENDDO
        MJ(NC)=UN
C
        IF (NC.GT.10000) THEN
          MAX_L=MAX_A
          CALL IMP_PCG1( 
     1                    NC    ,MAX_L  ,IADA  ,JDIA  ,DIAG_A ,   
     2                    LT_A  ,MJ     ,IERR  )
C
         IF (I_CHK.GT.0.AND.IERR.LT.0) NNE = I
        ELSE
C
         MAX_L=1+(NC*(NC-1))/2
         CALL IMP_FSAI(NC    ,IADA  ,JDIA  ,DIAG_A ,LT_A  ,
     .                 MAX_L ,MJ    )
C
        ENDIF 
C------------filtrage----Diagonal est dans LT_M (last one)--
         DIAG_M(I)=MJ(NC)
         IF (DIAG_M(I).LT.EM20) THEN
          IF (NNE.EQ.0.AND.I_CHK.EQ.0) NNE = I
          DIAG_M(I)=ABS(DIAG_M(I))
          DIAG_M(I)=MAX(EM20,DIAG_M(I))
         ENDIF
         DO K =1,NC-1
          M=IADK(I)+K-1
          LT_M(M)=MJ(K)/DIAG_M(I)
         ENDDO
C
         IF (I_CHK.GT.0.AND.MJ(NC).LT.EM20) DIAG_M(I)=MJ(NC)
       ENDDO

!$OMP END DO
C
       DEALLOCATE(IADA,DIAG_A,MJ)
       DEALLOCATE(LT_A,JDIA)
C 
      RETURN
      END
Chd|====================================================================
Chd|  IMP_FSA_INVH2                 source/implicit/imp_fsa_inv.F 
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        GET_SUBSP                     source/implicit/imp_fsa_inv.F 
Chd|        IMP_FSAI                      source/implicit/imp_fsa_inv.F 
Chd|        IMP_KFILTR                    source/implicit/imp_fsa_inv.F 
Chd|        IMP_PCG1                      source/implicit/imp_fsa_inv.F 
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SP_STAT0                      source/implicit/imp_fsa_inv.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE IMP_FSA_INVH2(
     1                    NDDL   ,IADK   ,JDIK   ,DIAG_K ,LT_K   ,   
     2                    IADM   ,JDIM   ,DIAG_M ,LT_M   ,MAXC   ,
     3                    MAX_A  ,NNE    ,IDLFT0 ,IDLFT1 ,MAX_D  ,
     4                    D_TOL  ,P_MACH ,ITASK  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc" 
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL ,NNZ ,IADK(*),JDIK(*),MAXC ,MAX_A ,NNE,
     .         IDLFT0 ,IDLFT1,MAX_D,IADM(*),JDIM(*),ITASK
C     REAL ou REAL*8
      my_real
     .  DIAG_K(*), LT_K(*),DIAG_M(*), LT_M(*),D_TOL  ,P_MACH   
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C--- M->A^-1 LT_M  stricly lower in m.c.r.s. format
      INTEGER I,J,K,M,N,NC,MAX_L,IERR,I_CHK,IER1,
     .        JM(MAXC+1)
      my_real
     .   DIAG_C(NDDL-IDLFT1+1),LT_C(MAX_D+1)
      INTEGER, DIMENSION(:),ALLOCATABLE :: IADA,JDIA
      my_real, DIMENSION(:),ALLOCATABLE :: DIAG_A,LT_A,MJ
C-----------------------------
       I_CHK = NNE
       NNE = 0
       IF ((IDLFT0+1).GT.NDDL) RETURN
C       
       ALLOCATE(IADA(MAXC+1),DIAG_A(MAXC),MJ(MAXC),STAT=IER1)
       ALLOCATE(LT_A(MAX_A),JDIA(MAX_A),STAT=IERR)
C-------- 
        IF ((IERR+IER1).NE.0) THEN
         IF (ITASK == 0 ) THEN
           CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .                 C1='FOR IMPLICIT PRECONDITION')
           CALL ARRET(2)
         END IF !(ITASK == 0 ) THEN
        ENDIF 
C--------------copy la partie utile------
       K=IADK(IDLFT1+1)-1
       DO I=IDLFT1+1,NDDL
         DIAG_C(I-IDLFT1) = DIAG_M(I)
         DO J=IADK(I),IADK(I+1)-1
          LT_C(J-K)=LT_M(J)
         ENDDO 
       ENDDO 
C----------------------
      CALL MY_BARRIER
C---------------------
C Boucle parallele dynamique SMP
C
!$OMP DO SCHEDULE(DYNAMIC,1)
       DO I=IDLFT0+1,NDDL
        CALL SP_STAT0(I  ,IADM  ,JDIM  ,NC    ,JM    )
        CALL GET_SUBSP(NDDL  ,IADK  ,JDIK  ,DIAG_K ,LT_K  ,
     .                 NC    ,IADA  ,JDIA  ,DIAG_A ,LT_A  ,   
     .                 JM    ,MAX_A ,IDLFT0,IDLFT1 ,DIAG_C,
     .                 LT_C  ,DIAG_M,LT_M  )
        DO J=1,NC-1
         MJ(J)=ZERO
        ENDDO
        MJ(NC)=UN
        IF (NC.GT.10000) THEN
          MAX_L=MAX_A
          CALL IMP_PCG1( 
     1                    NC    ,MAX_L  ,IADA  ,JDIA  ,DIAG_A ,   
     2                    LT_A  ,MJ     ,IERR  )
         IF (I_CHK.GT.0.AND.IERR.LT.0) NNE = I
        ELSE
         MAX_L=1+(NC*(NC-1))/2
         CALL IMP_FSAI(NC    ,IADA  ,JDIA  ,DIAG_A ,LT_A  ,
     .                MAX_L ,MJ    )
        ENDIF 
C------------Diagonal est dans LT_M (last one)--
         DIAG_M(I)=MJ(NC)
         IF (DIAG_M(I).LT.EM20) THEN
          IF (NNE.EQ.0.AND.I_CHK.EQ.0) NNE = I
          DIAG_M(I)=ABS(DIAG_M(I))
          DIAG_M(I)=MAX(EM20,DIAG_M(I))
         ENDIF
         DO K =1,NC-1
          M=IADM(I)+K-1
          LT_M(M)=MJ(K)/DIAG_M(I)
         ENDDO
C
         IF (I_CHK.GT.0.AND.MJ(NC).LT.EM20) DIAG_M(I)=MJ(NC)
       ENDDO
C
!$OMP END DO
C
       DEALLOCATE(IADA,DIAG_A,MJ)
       DEALLOCATE(LT_A,JDIA)
C
      IF (ITASK == 0 ) THEN
        K = IDLFT0+1
       IF (D_TOL.GT.ZERO)
     .  CALL IMP_KFILTR(K     ,NDDL  ,IADM  ,JDIM  ,DIAG_M ,
     .                  LT_M  ,D_TOL ,P_MACH,DIAG_K)
      END IF 
C 
      RETURN
      END
Chd|====================================================================
Chd|  SP_DIM                        source/implicit/imp_fsa_inv.F 
Chd|-- called by -----------
Chd|        IMP_FSA_INV2HP                source/implicit/imp_fsa_inv.F 
Chd|        IMP_FSA_INVHP                 source/implicit/imp_fsa_inv.F 
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE SP_DIM(IL ,IADK  ,JDIK  ,NC  ,MAX_A ,MAX_L )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  IL, IADK(*)  ,JDIK(*),NC,MAX_A ,MAX_L
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J,K
C-----------------------------------------------
      NC=0
      DO K =IADK(IL),IADK(IL+1)-1
       NC=NC+1
      ENDDO 
      NC=NC+1
C------MAX_L<- MAX_A      
        IF (NC <= 10000) THEN
         MAX_L = 1+(NC*(NC-1))/2
        ELSE
         MAX_L = MAX_A
        END IF
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  FSA_SOLV                      source/implicit/imp_fsa_inv.F 
Chd|-- called by -----------
Chd|        IMP_FSA_INV2HP                source/implicit/imp_fsa_inv.F 
Chd|        IMP_FSA_INVHP                 source/implicit/imp_fsa_inv.F 
Chd|-- calls ---------------
Chd|        GET_SUBSP                     source/implicit/imp_fsa_inv.F 
Chd|        IMP_FSAI                      source/implicit/imp_fsa_inv.F 
Chd|        IMP_PCG1                      source/implicit/imp_fsa_inv.F 
Chd|====================================================================
      SUBROUTINE FSA_SOLV(
     1                    NDDL  ,NC    ,IADK  ,JDIK  ,DIAG_K ,   
     2                    LT_K  ,DIAG_M,LT_M  ,DIAG_C,LT_C   ,
     3                    MAX_A ,IDLFT0,IDLFT1,NNE   ,I_CHK  ,
     4                    IADM  ,JDIM  ,I     )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc" 
#include      "task_c.inc" 
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  I,NDDL ,NC ,IADK(*),JDIK(*),MAX_A ,NNE,
     .         IDLFT0,IDLFT1 ,I_CHK,IADM(*),JDIM(*)
C     REAL ou REAL*8
      my_real
     .  DIAG_K(*), LT_K(*),DIAG_M(*), LT_M(*) ,DIAG_C(*),LT_C(*) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C--- M->A^-1 LT_M  stricly lower in m.c.r.s. format
      INTEGER J,K,M,N,MAX_L,IERR,IER1,IADA(NC+1),JM(NC)
c     .        IADA(NC+1),JDIA(MAX_A),JM(NC)
c      my_real
c     .      DIAG_A(NC),LT_A(MAX_A),MJ(NC)
      INTEGER, DIMENSION(:),ALLOCATABLE :: JDIA
      my_real, DIMENSION(:),ALLOCATABLE :: DIAG_A,LT_A,MJ
C-----------------------------
C       
       ALLOCATE(DIAG_A(NC),MJ(NC),STAT=IER1)
       ALLOCATE(LT_A(MAX_A),JDIA(MAX_A),STAT=IERR)
C-----------------------------
      J=0
      DO K =IADM(I),IADM(I+1)-1
       J=J+1
       JM(J)=JDIM(K)
      ENDDO 
      J=J+1
      JM(J)=I
C
        CALL GET_SUBSP(NDDL  ,IADK  ,JDIK  ,DIAG_K ,LT_K  ,
     .                 NC    ,IADA  ,JDIA  ,DIAG_A ,LT_A  ,   
     .                 JM    ,MAX_A ,IDLFT0,IDLFT1 ,DIAG_C,
     .                 LT_C  ,DIAG_M ,LT_M  )
        DO J=1,NC-1
         MJ(J)=ZERO
        ENDDO
        MJ(NC)=UN
C
        IF (NC > 10000) THEN
          MAX_L=MAX_A
          CALL IMP_PCG1( 
     1                    NC    ,MAX_L  ,IADA  ,JDIA  ,DIAG_A ,   
     2                    LT_A  ,MJ     ,IERR  )
C
         IF (I_CHK.GT.0.AND.IERR.LT.0) NNE = I
        ELSE
C
         MAX_L=1+(NC*(NC-1))/2
         CALL IMP_FSAI(NC    ,IADA  ,JDIA  ,DIAG_A ,LT_A  ,
     .                 MAX_L ,MJ    )
C
        ENDIF 
C------------filtrage----Diagonal est dans LT_M (last one)--
         DIAG_M(I)=MJ(NC)
         IF (DIAG_M(I).LT.EM20) THEN
          IF (NNE.EQ.0.AND.I_CHK.EQ.0) NNE = I
          DIAG_M(I)=ABS(DIAG_M(I))
          DIAG_M(I)=MAX(EM20,DIAG_M(I))
         ENDIF
         DO K =1,NC-1
          M=IADM(I)+K-1
          LT_M(M)=MJ(K)/DIAG_M(I)
         ENDDO
         IF (I_CHK.GT.0.AND.MJ(NC).LT.EM20) DIAG_M(I)=MJ(NC)
C 
       DEALLOCATE(DIAG_A,MJ)
       DEALLOCATE(LT_A,JDIA)
      RETURN
      END
Cqz------factorized sparse approximate inverse version hybrid SMP inside
Chd|====================================================================
Chd|  IMP_FSA_INVHP                 source/implicit/imp_fsa_inv.F 
Chd|-- called by -----------
Chd|        LIN_SOLVH1                    source/implicit/lin_solv.F    
Chd|-- calls ---------------
Chd|        FSA_SOLV                      source/implicit/imp_fsa_inv.F 
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SP_DIM                        source/implicit/imp_fsa_inv.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE IMP_FSA_INVHP(
     1                    NDDL  ,NNZ   ,IADK  ,JDIK  ,DIAG_K ,   
     2                    LT_K  ,DIAG_M ,LT_M  ,MAXC  ,MAX_A ,
     3                    NNE   ,IDLFT0 ,IDLFT1,MAX_D )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc" 
#include      "task_c.inc" 
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL ,NNZ ,IADK(*),JDIK(*),MAXC ,MAX_A ,NNE,
     .         IDLFT0 ,IDLFT1,MAX_D
C     REAL ou REAL*8
      my_real
     .  DIAG_K(*), LT_K(*),DIAG_M(*), LT_M(*)   
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C--- M->A^-1 LT_M  stricly lower in m.c.r.s. format
      INTEGER I,J,K,M,N,NC,MAX_L,IERR,I_CHK,IER1,
     .        ITSK,F_DDL,L_DDL,N1
      my_real
     .   DIAG_C(NDDL-IDLFT1+1),LT_C(MAX_D+1)
      INTEGER OMP_GET_THREAD_NUM
      EXTERNAL OMP_GET_THREAD_NUM
C-----------------------------
       I_CHK = NNE
       NNE = 0
       IF ((IDLFT0+1).GT.NDDL) RETURN
C
C--------------copy la partie utile------
       K=IADK(IDLFT1+1)-1
!$OMP PARALLEL PRIVATE(ITSK,F_DDL,L_DDL,NC,MAX_L,I,J,N1)
        ITSK = OMP_GET_THREAD_NUM()
        N1 = NDDL-IDLFT1
        F_DDL = IDLFT1+1+ITSK*N1/ NTHREAD
        L_DDL = IDLFT1+(ITSK+1)*N1/ NTHREAD
C
C----------------------
      CALL MY_BARRIER
C---------------------
       DO I=F_DDL,L_DDL
         DIAG_C(I-IDLFT1) = DIAG_M(I)
         DO J=IADK(I),IADK(I+1)-1
          LT_C(J-K)=LT_M(J)
         ENDDO 
       ENDDO 
C----------------------
      CALL MY_BARRIER
C---------------------
C
C Boucle parallele dynamique SMP
C
!$OMP DO SCHEDULE(DYNAMIC,1)
      DO I=IDLFT0+1,NDDL
        CALL SP_DIM(I  ,IADK  ,JDIK  ,NC   ,MAX_A ,MAX_L )
        CALL FSA_SOLV(
     1                NDDL  ,NC    ,IADK  ,JDIK  ,DIAG_K ,   
     2                LT_K  ,DIAG_M,LT_M  ,DIAG_C,LT_C   ,
     3                MAX_L ,IDLFT0,IDLFT1,NNE   ,I_CHK  ,
     4                IADK  ,JDIK  ,I   )
      ENDDO

!$OMP END DO
!$OMP END PARALLEL
C 
      RETURN
      END
Chd|====================================================================
Chd|  IMP_FSA_INV2HP                source/implicit/imp_fsa_inv.F 
Chd|-- called by -----------
Chd|        LIN_SOLVIH2                   source/implicit/lin_solv.F    
Chd|-- calls ---------------
Chd|        FSA_SOLV                      source/implicit/imp_fsa_inv.F 
Chd|        IMP_KFILTR                    source/implicit/imp_fsa_inv.F 
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SP_DIM                        source/implicit/imp_fsa_inv.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE IMP_FSA_INV2HP(
     1                    NDDL   ,IADK   ,JDIK   ,DIAG_K ,LT_K   ,   
     2                    IADM   ,JDIM   ,DIAG_M ,LT_M   ,MAXC   ,
     3                    MAX_A  ,NNE    ,IDLFT0 ,IDLFT1 ,MAX_D  ,
     4                    D_TOL  ,P_MACH )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc" 
#include      "task_c.inc" 
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL ,NNZ ,IADK(*),JDIK(*),MAXC ,MAX_A ,NNE,
     .         IDLFT0 ,IDLFT1,MAX_D,IADM(*),JDIM(*)
C     REAL ou REAL*8
      my_real
     .  DIAG_K(*), LT_K(*),DIAG_M(*), LT_M(*),D_TOL  ,P_MACH   
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C--- M->A^-1 LT_M  stricly lower in m.c.r.s. format
      INTEGER I,J,K,M,N,NC,MAX_L,IERR,I_CHK,IER1,
     .        ITSK,F_DDL,L_DDL,N1
      my_real
     .   DIAG_C(NDDL-IDLFT1+1),LT_C(MAX_D+1)
      INTEGER OMP_GET_THREAD_NUM
      EXTERNAL OMP_GET_THREAD_NUM
C-----------------------------
       I_CHK = NNE
       NNE = 0
       IF ((IDLFT0+1).GT.NDDL) RETURN
C       
C--------------copy la partie utile------
       K=IADK(IDLFT1+1)-1
!$OMP PARALLEL PRIVATE(ITSK,F_DDL,L_DDL,NC,MAX_L,N1,J,I)
        ITSK = OMP_GET_THREAD_NUM()
        N1 = NDDL-IDLFT1
        F_DDL = IDLFT1+1+ITSK*N1/ NTHREAD
        L_DDL = IDLFT1+(ITSK+1)*N1/ NTHREAD
       DO I=IDLFT1+1,NDDL
         DIAG_C(I-IDLFT1) = DIAG_M(I)
         DO J=IADK(I),IADK(I+1)-1
          LT_C(J-K)=LT_M(J)
         ENDDO 
       ENDDO 
C----------------------
      CALL MY_BARRIER
C---------------------
C Boucle parallele dynamique SMP
C
!$OMP DO SCHEDULE(DYNAMIC,1)
       DO I=IDLFT0+1,NDDL
        CALL SP_DIM(I  ,IADM  ,JDIM  ,NC   ,MAX_A ,MAX_L )
        CALL FSA_SOLV(
     1                NDDL  ,NC    ,IADK  ,JDIK  ,DIAG_K ,   
     2                LT_K  ,DIAG_M,LT_M  ,DIAG_C,LT_C   ,
     3                MAX_L ,IDLFT0,IDLFT1,NNE   ,I_CHK  ,
     4                IADM  ,JDIM  ,I   )
       ENDDO
C
!$OMP END DO
!$OMP END PARALLEL
C
Citask0      IF (ITASK == 0 ) THEN
        K = IDLFT0+1
       IF (D_TOL.GT.ZERO)
     .  CALL IMP_KFILTR(K     ,NDDL  ,IADM  ,JDIM  ,DIAG_M ,
     .                  LT_M  ,D_TOL ,P_MACH,DIAG_K)
Citask0      END IF 
C 
      RETURN
      END
     
      
