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
      module MOD_SMS_FSA
#include      "my_real.inc"
C-----
      INTEGER, DIMENSION(:), ALLOCATABLE :: IADM, JADM, KADM, 
     .                                      JDIM, JDIM2,
     .                                      ISORTND, INVND
      my_real
     .       , DIMENSION(:), ALLOCATABLE :: DIAG_M, LT_M, LT_M2,
     .                                      DIAG_INV
      INTEGER NNDFT0, NNDFT1, NNZM
      END MODULE MOD_SMS_FSA
Chd|====================================================================
Chd|  SMS_CHECK                     source/ams/sms_fsa_inv.F      
Chd|-- called by -----------
Chd|        SMS_PCG                       source/ams/sms_pcg.F          
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        FLOATMIN                      ../common_source/tools/math/precision.c
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SMS_FSA_INVH                  source/ams/sms_fsa_inv.F      
Chd|        SPMD_EXCHM_SMS                source/mpi/ams/spmd_sms.F     
Chd|        SPMD_GLOB_LMIN                source/mpi/ams/spmd_sms.F     
Chd|        SPMD_MAX_S                    source/mpi/implicit/imp_spmd.F
Chd|        SPMD_NNDFT_SMS                source/mpi/ams/spmd_sms.F     
Chd|        SPMD_NNZ_SMS                  source/mpi/ams/spmd_sms.F     
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SMS_CHECK(NODFT ,NODLT ,IADK  ,JDIK  ,DIAG_K ,  
     2                     LT_K  ,IADI  , JDII ,LT_I  ,ITASK  ,
     3                     ITAB  ,IAD_ELEM,FR_ELEM,FR_SMS,FR_RMS,
     4                     LIST_SMS,LIST_RMS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MOD_SMS_FSA
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
#include "com01_c.inc"
#include "com04_c.inc"
#include "sms_c.inc"
#include "task_c.inc" 
#include "timeri_c.inc"
#include "units_c.inc" 
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NODFT, NODLT, 
     .         IADK(*), JDIK(*), IADI(*), JDII(*),
     .         ITASK, ITAB(*), IAD_ELEM(2,*), FR_ELEM(*),
     .         FR_SMS(NSPMD+1), FR_RMS(NSPMD+1), 
     .         LIST_SMS(*), LIST_RMS(*)
C     REAL ou REAL*8
      my_real
     .  DIAG_K(*), LT_K(*), LT_I(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ITAG(NUMNOD)   
C
      INTEGER I, J, K, L, NOD, IBID, IERR, IMIN,
     .        NNZMFT, NNZMLT
      my_real
     .        LMIN
      my_real
     .   CS1(2)
      REAL FLMIN
C-----------------------------------------------
      IF(ISPMD==0.AND.ITASK==0)THEN
        WRITE(ISTDO,2001)
        WRITE(IOUT,2001) 
      END IF
C machine  precision minimum -simple
      IF(ITASK==0)THEN
       CALL FLOATMIN(CS1(1),CS1(2),FLMIN)
       P_MACH_SMS = TWO*SQRT(FLMIN)
       IF (NSPMD > 1)CALL SPMD_MAX_S(P_MACH_SMS)
      END IF
C-----------------------------------------------
C     [K]:matrice de masse complete 
C     [M]:factorized sparse approximated inverse Lt D L
C------------------------------------------
      NNZM  = NNZ_SMS
C
      IF (IMON.GT.0.AND.ITASK.EQ.0) CALL STARTIME(32,1)
C
      IF(ITASK==0)THEN
        ALLOCATE(IADM(NUMNOD+1),JADM(NUMNOD+1),KADM(NUMNOD),
     .           ISORTND(NUMNOD),INVND(NUMNOD),STAT=IERR)
        IF (IERR.NE.0) THEN
          CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .                C1='FOR FSAI')
          CALL ARRET(2)
        ENDIF 
      ENDIF 
C     ----- approx. (by each colonne of L_T) inverse ---------
C     ----- utilise d'abord la place de [M] pour la matrice assemblee ---------
      IF (NSPMD.EQ.1) THEN
        NNDFT0=0
        NNDFT1=NUMNOD
C----------------------
        CALL MY_BARRIER
C---------------------
        DO NOD=NODFT,NODLT
          ISORTND(NOD)=NOD
        END DO
      ELSEIF(ITASK.EQ.0)THEN
        CALL SPMD_NNDFT_SMS(
     1             FR_SMS ,FR_RMS,LIST_SMS,LIST_RMS,IAD_ELEM,
     2             FR_ELEM,NNDFT0,NNDFT1,ISORTND)
      ENDIF
C----------------------
      CALL MY_BARRIER
C---------------------
      DO K=NODFT,NODLT
        NOD = ISORTND(K)
        INVND(NOD) = K
      END DO
C-----
      DO NOD=NODFT,NODLT
        KADM(NOD)=IADK(NOD+1)-IADK(NOD)
      END DO
C----------------------
      CALL MY_BARRIER
C---------------------
      IF(ITASK.EQ.0)THEN
C 
        IF (NSPMD > 1) THEN
          CALL SPMD_NNZ_SMS(
     1               FR_SMS ,FR_RMS,LIST_SMS,LIST_RMS,IAD_ELEM,
     2               FR_ELEM,NNZM  ,IADK    ,KADM    )
        END IF
C 
        ALLOCATE(DIAG_M(NUMNOD),LT_M(NNZM),JDIM(NNZM),
     .           DIAG_INV(NUMNOD),STAT=IERR)
        IF (IERR.NE.0) THEN
          CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .                C1='FOR FSAI')
          CALL ARRET(2)
        ENDIF
      ENDIF
C---------------------
      IF(ITASK==0)THEN
        IADM(1)=1
        DO I=1,NUMNOD
          IADM(I+1)=IADM(I)+KADM(ISORTND(I))
        END DO
      END IF
C----------------------
      CALL MY_BARRIER
C---------------------
      NNZMFT=ITASK*NNZM/NTHREAD+1
      NNZMLT=(ITASK+1)*NNZM/NTHREAD
      JDIM(NNZMFT:NNZMLT)=0
C----------------------
      CALL MY_BARRIER
C---------------------
C M triee 1 ... NNDFT0 ... NNDFT1 ... NUMNOD
      DO I=NODFT,NODLT
       NOD=ISORTND(I)
       DIAG_M(I) = DIAG_K(NOD)
       L=IADM(I)
       DO J=IADK(NOD),IADK(NOD+1)-1
        K=INVND(JDIK(J))
        IF(K < I) THEN
          JDIM(L)=K
          LT_M(L)=LT_K(J)
          L = L + 1
        END IF
       ENDDO
       KADM(I)=L
      ENDDO 
C----------------------
      CALL MY_BARRIER
C---------------------
C M <- termes des processeurs voisins
      IF (ITASK == 0 .AND. NSPMD > 1) THEN
        CALL SPMD_EXCHM_SMS(
     1           FR_SMS ,FR_RMS,LIST_SMS,LIST_RMS,IAD_ELEM,
     2           FR_ELEM,IADK  ,JDIK    ,LT_K    ,KADM    ,
     3           JDIM   ,LT_M  ,INVND   )
      END IF
C----------------------
C M compactage
      IF(ITASK.EQ.0)THEN
        ALLOCATE(LT_M2(NNZM),JDIM2(NNZM),
     .           STAT=IERR)
        IF (IERR.NE.0) THEN
          CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .                C1='FOR FSAI')
          CALL ARRET(2)
        ENDIF 
      ENDIF
C----------------------
      CALL MY_BARRIER
C---------------------
      DO I=NODFT,NODLT
        KADM(I)=0
        DO K=IADM(I),IADM(I+1)-1
         J = JDIM(K)
         IF(J/=0) ITAG(J) = 0
        END DO
        DO K=IADM(I),IADM(I+1)-1
         J = JDIM(K)
         IF(J/=0) THEN
           IF(ITAG(J)==0)THEN
             KADM(I) = KADM(I) + 1
             ITAG(J) = K
           END IF
         END IF
        END DO
      END DO
C----------------------
      CALL MY_BARRIER
C---------------------
      IF(ITASK==0)THEN
        JADM(1)=1
        DO I=1,NUMNOD
          JADM(I+1)=JADM(I)+KADM(I)
        END DO
      END IF
C----------------------
      CALL MY_BARRIER
C---------------------
      DO I=NODFT,NODLT
        KADM(I)=JADM(I)
        DO K=IADM(I),IADM(I+1)-1
         J = JDIM(K)
         IF(J/=0) ITAG(J) = 0
        END DO
        DO K=IADM(I),IADM(I+1)-1
         J = JDIM(K)
         IF(J/=0) THEN
           IF(ITAG(J)==0)THEN
             JDIM2(KADM(I)) = J
             LT_M2(KADM(I)) = LT_M(K)
             ITAG(J) = KADM(I)
             KADM(I) = KADM(I) + 1
           ELSE
             LT_M2(ITAG(J)) = LT_M2(ITAG(J)) + LT_M(K)
           END IF
         END IF
        END DO
      END DO
C----------------------
      CALL MY_BARRIER
C---------------------
      CALL SMS_FSA_INVH(NNZM   ,JADM  ,JDIM2 , DIAG_M, LT_M2, 
     1  	        NNDFT0,NNDFT1,ITASK  ,DIAG_INV)
C----------------------
      CALL MY_BARRIER
C---------------------
      IF (ITASK == 0) THEN
C-------
        DO I=1,NNDFT0
         DIAG_INV(I) = ZERO
        ENDDO 
C-------
        LMIN=EP20
        DO I=NNDFT0+1,NUMNOD
          IF(DIAG_INV(I) < LMIN)THEN
            LMIN=DIAG_INV(I)
            IMIN=ITAB(ISORTND(I))
          END IF
        END DO
        IF (NSPMD > 1) THEN
          CALL SPMD_GLOB_LMIN(LMIN,IMIN)
        END IF
C-------
        IF(ISPMD==0.AND.ITASK==0)THEN
          IF(LMIN.LT.EM06)THEN
            WRITE(ISTDO,3001) IMIN,LMIN
            WRITE(IOUT,3001)  IMIN,LMIN
          ELSE
            WRITE(ISTDO,4001) 
            WRITE(IOUT,4001)  
          END IF
        END IF
      END IF
C
      IF (IMON.GT.0.AND.ITASK.EQ.0) CALL STOPTIME(32,1)
C----------------------
      CALL MY_BARRIER
C---------------------
      IF(ITASK==0)THEN
        DEALLOCATE(IADM,JADM,KADM,ISORTND,INVND,
     .             DIAG_M,LT_M,JDIM,LT_M2,JDIM2,DIAG_INV)
      END IF
C--------------------------------------------
 2001 FORMAT(' ... RUNNING DIAGNOSIS')
 3001 FORMAT(
     .' **  WARNING : RADIOSS DETECTED A SEVERE ISSUE',/
     .'     PLEASE CHECK THE MODEL, ESPECIALLY KINEMATIC CONDITIONS',/
     .'     ISSUE MAY OCCUR NEARBY OR ON ENTITY LINKED ',/
     .' 				   TO NODE ID =',I10/
     .'     (MINIMUM DIAGONAL TERM OF FSAI = ',1PG20.14,')')
 4001 FORMAT(' **  INFO : COULD NOT IDENTIFY THE ISSUE')
      RETURN
      END
C--------factorized sparse approximate inverse version hybrid------- 
Chd|====================================================================
Chd|  SMS_FSA_INVH                  source/ams/sms_fsa_inv.F      
Chd|-- called by -----------
Chd|        SMS_CHECK                     source/ams/sms_fsa_inv.F      
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        GET_SUBSP_SMS                 source/ams/sms_fsa_inv.F      
Chd|        IMP_FSAI                      source/implicit/imp_fsa_inv.F 
Chd|        SMS_PCG1                      source/ams/sms_fsa_inv.F      
Chd|        SP_STAT0                      source/implicit/imp_fsa_inv.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SMS_FSA_INVH(NNZM   ,IADM  ,JDIM  , DIAG_M, LT_M, 
     1  	              NNDFT0 ,NNDFT1,ITASK ,DIAG_INV)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
#include "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc" 
#include      "task_c.inc" 
#include      "units_c.inc" 
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NNZM , IADM(*), JDIM(*),
     .         NNDFT0 ,NNDFT1,ITASK
C     REAL ou REAL*8
      my_real
     .  DIAG_M(*), LT_M(*), DIAG_INV(*)   
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C--- M->A^-1 LT_M  stricly lower in m.c.r.s. format
      INTEGER I,J,K,M,N,NC,MAX_L,IERR,I_CHK,IER1,
     .        JM(NUMNOD+1)
      INTEGER, DIMENSION(:),ALLOCATABLE :: IADA, JDIA
      my_real, 
     .        DIMENSION(:),ALLOCATABLE :: DIAG_A, LT_A, MJ
C-----------------------------
       IF ((NNDFT0+1).GT.NUMNOD) RETURN
C
       ALLOCATE(IADA(NUMNOD+1),DIAG_A(NUMNOD),MJ(NUMNOD),STAT=IER1)
       ALLOCATE(LT_A(NNZM),JDIA(NNZM),STAT=IERR)
 
       IF ((IERR+IER1).NE.0) THEN
         CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .               C1='FOR FSAI')
         CALL ARRET(2)
       ENDIF 
C
C Boucle parallele dynamique SMP
C
!$OMP DO SCHEDULE(DYNAMIC,1)
      DO I=NNDFT0+1,NUMNOD

        IF(DIAG_M(I)==ZERO) THEN
          DIAG_INV(I)=EP20
          CYCLE
        END IF

        CALL SP_STAT0(I  ,IADM  ,JDIM  ,NC    ,JM    )
        CALL GET_SUBSP_SMS(IADM  ,JDIM  ,DIAG_M ,LT_M  ,NC    ,
     .                     IADA  ,JDIA  ,DIAG_A ,LT_A  ,JM    ,   
     .                     NNDFT0,NNDFT1)
        DO J=1,NC-1
         MJ(J)=ZERO
        ENDDO
        MJ(NC)=ONE
C
        IF (NC.GT.10000) THEN
          CALL SMS_PCG1(NC    ,IADA  ,JDIA  ,DIAG_A ,LT_A  ,   
     2                  MJ    ,IERR  )
C
        ELSE
C
         MAX_L=1+(NC*(NC-1))/2
         CALL IMP_FSAI(NC    ,IADA  ,JDIA  ,DIAG_A ,LT_A  ,
     .                 MAX_L ,MJ    )
C
        ENDIF 
C------------filtrage----Diagonal est dans LT_M (last one)--
        DIAG_INV(I)=MJ(NC)
       ENDDO

!$OMP END DO
C
       DEALLOCATE(IADA,DIAG_A,MJ)
       DEALLOCATE(LT_A,JDIA)
C 
      RETURN
      END
C----------version spmd---set submatrix A(N,N) Format m.c.c.s. for FSAI ----
Chd|====================================================================
Chd|  GET_SUBSP_SMS                 source/ams/sms_fsa_inv.F      
Chd|-- called by -----------
Chd|        SMS_FSA_INVH                  source/ams/sms_fsa_inv.F      
Chd|-- calls ---------------
Chd|        IND_LT2LN                     source/implicit/imp_fsa_inv.F 
Chd|        INTAB0                        source/implicit/imp_fsa_inv.F 
Chd|====================================================================
      SUBROUTINE GET_SUBSP_SMS(IADM  ,JDIM  ,DIAG_M ,LT_M  ,NC    ,
     .                         IADA  ,JDIA  ,DIAG_A ,LT_A  ,JM    ,   
     .                         NNDFT0,NNDFT1 )
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
      INTEGER  IADM(*), JDIM(*), IADA(*), JDIA(*)
      INTEGER  NC    , JM(*), NNDFT0, NNDFT1
C     REAL ou REAL*8
      my_real
     .  LT_A(*),DIAG_A(*),
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
#include      "vectorize.inc"
      DO I=1,NC
       J=JM(I)
C
       DIAG_A(I)=DIAG_M(J)
       DO K=IADM(J),IADM(J+1)-1
        JJ=JDIM(K)
        N=INTAB0(NC,JM,JJ)
        IF (N.GT.0) THEN
         NNZA=NNZA+1
         JDIA(NNZA)=N
         LT_A(NNZA)=LT_M(K)
        ENDIF
       ENDDO
C
       IADA(I+1)=NNZA+1
      ENDDO
      CALL IND_LT2LN(NC,IADA  ,JDIA  ,LT_A, NNZA   )
C
      RETURN
      END
Chd|====================================================================
Chd|  SMS_PCG1                      source/ams/sms_fsa_inv.F      
Chd|-- called by -----------
Chd|        SMS_FSA_INVH                  source/ams/sms_fsa_inv.F      
Chd|-- calls ---------------
Chd|        MAV_LT                        source/implicit/produt_v.F    
Chd|        PRODUT_V0                     source/implicit/produt_v.F    
Chd|====================================================================
      SUBROUTINE SMS_PCG1( 
     1                    NDDL  ,IADK  ,JDIK  ,DIAG_K ,LT_K  ,   
     2                    R     ,ISP   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "sms_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C----------resol [K]{X}={F}, K stored as diagonal + lt ---------
      INTEGER  NDDL  ,IADK(*)  ,JDIK(*)
C     REAL ou REAL*8
      my_real
     .  DIAG_K(*), LT_K(*) , R(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IT,IP,NLIM,ND,ISTOP,ISP,IBID
      my_real
     .   S , R2, R02, ALPHA, BETA, G0, G1, RR, TOLS, TOLN, TOLS2
      my_real
     .  X(NDDL) ,P(NDDL) ,Z(NDDL)  ,Y(NDDL),DIAG_M(NDDL) 
      my_real
     .  EPS_M
C--------------INITIALISATION--------------------------
      NLIM=MAX(NDDL,2)

      TOLS=SQRT(P_MACH_SMS)
      EPS_M = P_MACH_SMS

      IT=0
C-------------IT=0--------
C------X(I)=ZERO--------
      DO I=1,NDDL
       X(I) = ZERO
       DIAG_M(I)=ONE/MAX(EM20,DIAG_K(I))
      ENDDO 
      CALL MAV_LT(
     1  	 NDDL  ,IBID  ,IADK  ,JDIK  ,DIAG_K,   
     2  	 LT_K  ,X     ,Z     )
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
     1  	 NDDL  ,IBID ,IADK  ,JDIK  ,DIAG_K,   
     2  	 LT_K  ,P     ,Y     )
      CALL PRODUT_V0(NDDL,P,Y,S)
      ALPHA = G0/S
      TOLS2=TOLS*TOLS

      CALL PRODUT_V0(NDDL,R,R,R02)
      R2 =R02
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
       CALL PRODUT_V0(NDDL,R,R,R2)
C
       G0 = G1

       IF (IT.GE.NLIM) THEN
         ISTOP = 0
       ELSEIF (R2.LE.TOLN) THEN
         ISTOP = 0
       ELSE
         ISTOP = 1
       ENDIF

       DO WHILE (ISTOP.EQ.1)
        DO I=1,NDDL
         P(I) = Z(I) + BETA*P(I)
        ENDDO 
        CALL MAV_LT(
     1             NDDL  ,IBID  ,IADK  ,JDIK  ,DIAG_K,   
     2             LT_K  ,P     ,Y     )
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
        CALL PRODUT_V0(NDDL,R,R,R2)

        IF (IT.GE.NLIM) THEN
          ISTOP = 0
        ELSEIF (R2.LE.TOLN) THEN
          ISTOP = 0
        ELSE
          ISTOP = 1
        ENDIF

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
     . 1X,'IN FSAI')
       RETURN
      END
