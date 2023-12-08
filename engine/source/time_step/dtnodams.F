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
Chd|  DTNODAMS                      source/time_step/dtnodams.F   
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE DTNODAMS(NODFT ,NODLT ,NELTST,ITYPTST,ITAB   ,
     2                    MS    ,IN    ,STIFN ,STIFR  ,DT2T   ,
     3                    DMAST ,DINERT,ADT   ,ADM    ,IMSCH  ,
     4                    WEIGHT,A     ,AR    ,IGRNOD ,
     5                    ADI   ,RBYM  ,ARBY  ,ARRBY  ,ISMSCH ,
     6                    NODNX_SMS,DIAG_SMS  ,NPBY   ,TAGMSR_RBY_SMS,
     7                    H3D_DATA )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE H3D_MOD
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "scr02_c.inc"
#include      "scr07_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr18_c.inc"
#include      "units_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODFT, NODLT,NELTST,ITYPTST,ITAB(*),
     .        WEIGHT(*),IMSCH,
     .        ISMSCH, NODNX_SMS(*), NPBY(NNPBY,*), TAGMSR_RBY_SMS(*)
C     REAL
      my_real DT2T, DMAST, DINERT, 
     .   MS(*) ,IN(*) ,STIFN(*), STIFR(*),ADT(*) ,ADM(*) ,
     .   A(3,*) ,AR(3,*) ,ADI(*) ,RBYM(NFRBYM,*),ARBY(3,*),
     .   ARRBY(3,*), DIAG_SMS(*)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD) :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, NN, K1, KMAX1, K2, KMAX2, ISTOP,I,J,
     .   INDTN1(1024), INDTN2(1024), TAG(NUMNOD), TAGR(NUMNOD), IOK,
     .   K, M, MSR, NSN, IAD
      my_real DTN1(1024), DTN2(1024),
     .   DTNOD1, DTNOD2, DT2P, MAS, INER, MAS0, DMM, MASS, STI,
     .   DT2X, DT2S
C----------------------------------------------------------
C     TAG NODAL GROUP 
C----------------------------------------------------------
      IF(IDTGR(11)<0)THEN
        IOK = 0
        DO N=1,NGRNOD
          IF (-IDTGR(11)==IGRNOD(N)%ID) THEN
            IDTGR(11)=N
            IOK = 1
          ENDIF
        ENDDO
        IF (IOK == 0) THEN
          CALL ANCMSG(MSGID=237,ANMODE=ANINFO,
     .                I1=-IDTGR(11))
          CALL ARRET(2)
        ENDIF
      ENDIF
      IF(IDTGR(11)/=0)THEN
       DO N=NODFT,NODLT
         TAG (N) = 0
         TAGR(N) = 0
       ENDDO
       DO N=1,IGRNOD(IDTGR(11))%NENTITY
         IF(NODNX_SMS(IGRNOD(IDTGR(11))%ENTITY(N))==0)THEN
           TAG(IGRNOD(IDTGR(11))%ENTITY(N))  = 1
           TAGR(IGRNOD(IDTGR(11))%ENTITY(N)) = 1
         END IF
       ENDDO
      ELSE
       DO N=NODFT,NODLT
         IF(NODNX_SMS(N)==0)THEN
           TAG(N)  = 1
           TAGR(N) = 1
         ELSE
           TAG(N)  = 0
           TAGR(N) = 0
         END IF
       ENDDO
      ENDIF
C----------------------------------------------------------
C     ANIM TIME STEP 
C----------------------------------------------------------
!$OMP SINGLE
      IMSCH = 0
!$OMP END SINGLE
#ifdef __INTEL_LLVM_COMPILER
!$omp barrier
#endif
      IF(ANIM_N(1)+OUTP_N(1)+H3D_DATA%N_SCAL_DT >0)THEN
#include      "vectorize.inc"
        DO NN=NODFT,NODLT
          ADT(NN) = EP06
        ENDDO
      ENDIF
C----------------------------------------------------------
C     SCALE TIME STEP 
C----------------------------------------------------------
      ISTOP = 0
C----------------------------------------------------------
C     NODAL TIME STEP VS TRANSLATION (NON AMS NODES)
C----------------------------------------------------------
      DT2S=DT2T

      IF(I7KGLO > 0 .OR. NODADT /= 0)THEN
        DO 370 I=NODFT,NODLT,1024
         K1 = 0
         DTNOD1 = EP20
#include      "vectorize.inc"
         DO N=I,MIN(NODLT,I+1023)
          IF(STIFN(N)<=ZERO)THEN
             ISTOP = -ITAB(N)
          ELSEIF(NODNX_SMS(N)==0)THEN
           IF(MS(N)>ZERO)THEN
             K1 = K1 + 1
C MS si non compense
             DTN1(K1) = DTFAC1(11)*SQRT(TWO * MS(N) / STIFN(N))
             DTNOD1 = MIN(DTNOD1,DTN1(K1))
             INDTN1(K1) = N
           ENDIF
          ENDIF
         ENDDO
         KMAX1 = K1
C
         IF(DTNOD1<DTMIN1(11))THEN
          IF(IDTMIN(11)==1.OR.IDTMIN(11)==5)THEN
            DO K1=1,KMAX1
C              IF(DTN1(K1)<DTMIN1(11))THEN
C                  N = INDTN1(K1) 
C                  ISTOP = ITAB(N)
C              ENDIF
              DTN1(K1)=DTMIN1(11)
            ENDDO
          ELSEIF(IDTMIN(11)==3.OR.IDTMIN(11)==8)THEN
C----------------------------------------------------------
            IF (ANIM_N(2)+OUTP_N(2)+H3D_DATA%N_SCAL_DMAS >0) THEN
#include      "vectorize.inc"
              DO K1=1,KMAX1
                IF(DTN1(K1)<DTMIN1(11))THEN
                  N = INDTN1(K1) 
                  DT2P = DTMIN1(11)/DTFAC1(11)
                  MAS = HALF * STIFN(N) * DT2P * DT2P * ONEP00001
                  IF(TAG(N)==1)THEN
                    IF(WEIGHT(N)==1) THEN
                      DMAST =  DMAST + MAS - MS(N)
                      ADM(N) = MAS*(ONE+ADM(N))/MS(N)  - ONE
                    ENDIF
                    MS(N) = MAS
                    IMSCH = 1
                  ENDIF
                  DTN1(K1)=DTMIN1(11)
                ENDIF
              ENDDO
            ELSE
#include      "vectorize.inc"
              DO K1=1,KMAX1
                IF(DTN1(K1)<DTMIN1(11))THEN
                  N = INDTN1(K1) 
                  DT2P = DTMIN1(11)/DTFAC1(11)
                  MAS = HALF * STIFN(N) * DT2P * DT2P * ONEP00001
                  IF(TAG(N)==1)THEN
                    DMAST =  DMAST + (MAS - MS(N))*WEIGHT(N)
                    MS(N) = MAS
                    IMSCH = 1
                  ENDIF
                  DTN1(K1)=DTMIN1(11)
                ENDIF
              ENDDO
            ENDIF
          ELSEIF(IDTMIN(11)==4)THEN
#include      "vectorize.inc"
            DO K1=1,KMAX1
              IF(DTN1(K1)<DTMIN1(11))THEN
                  N = INDTN1(K1) 
                  DT2P = DTMIN1(11)/DTFAC1(11)
                  MAS = HALF * STIFN(N) * DT2P * DT2P 
                  MAS=MS(N)/MAS
                  DTN1(K1)=DTMIN1(11)
                  A(1,N)=A(1,N)*MAS
                  A(2,N)=A(2,N)*MAS
                  A(3,N)=A(3,N)*MAS
              ENDIF
            ENDDO
          ENDIF
         ENDIF
C--------
         IF(DTNOD1<DT2T)THEN
          DO K1=1,KMAX1
              IF(DTN1(K1)<DT2T)THEN
                N = INDTN1(K1) 
                NELTST  = ITAB(N)
                ITYPTST = 11
                DT2T = DTN1(K1)
              ENDIF
          ENDDO
         ENDIF
#include      "vectorize.inc"
           DO N=I,MIN(NODLT,I+1023)
              STIFN(N) = EM20
           ENDDO
C----------------------------------------------------------
C     ANIM TIME STEP 
C----------------------------------------------------------
         IF(ANIM_N(1)+OUTP_N(1)+H3D_DATA%N_SCAL_DT >0)THEN
#include      "vectorize.inc"
           DO K1=1,KMAX1
             ADT(INDTN1(K1))=DTN1(K1)
           ENDDO
         ENDIF
 370    CONTINUE
       END IF
C----------------------------------------------------------
C      AMS TIME STEP & NODAL TIME STEP (ROTATION) 
C----------------------------------------------------------
       IF (IRODDL/=0) THEN
          DO 450 I=NODFT,NODLT,1024
           DTNOD1 = EP20
           DTNOD2 = EP20
           K1 = 0
           K2 = 0
#include      "vectorize.inc"
            DO N=I,MIN(NODLT,I+1023)
             IF(STIFR(N)<=ZERO)THEN
               ISTOP=-ITAB(N)
             ELSEIF(NODADT/=0 .AND. NODNX_SMS(N)==0)THEN
              IF(IN(N)>ZERO)THEN
               K1 = K1 + 1
               INDTN1(K1) = N
               DTN1(K1) = DTFAC1(11)*SQRT(TWO * IN(N) / STIFR(N))
               DTNOD1 = MIN(DTNOD1,DTN1(K1))
              ENDIF
             ELSEIF(NODNX_SMS(N)/=0)THEN
               IF(IN(N)>ZERO)THEN
                 K2 = K2 + 1
                 INDTN2(K2) = N
                 DTN2(K2) = DTFACS*SQRT(TWO * IN(N) / STIFR(N))
                 DTNOD2 = MIN(DTNOD2,DTN2(K2))
               ENDIF
             ENDIF
            ENDDO
           KMAX1 = K1
           KMAX2 = K2
           IF(DTNOD1<DTMIN1(11))THEN
            IF(IDTMIN(11)==1.OR.IDTMIN(11)==5)THEN
              DO K1=1,KMAX1
                DTN1(K1)=DTMIN1(11)
C                IF(DTN1(K1)<DTMIN1(11))THEN
C                  N = INDTN1(K1) 
C                  ISTOP = ITAB(N)
C                ENDIF
              ENDDO
            ELSEIF(IDTMIN(11)==3.OR.IDTMIN(11)==8)THEN
              IF (ANIM_N(12)+OUTP_N(3)+H3D_DATA%N_SCAL_DINER >0) THEN
#include      "vectorize.inc"
                DO K1=1,KMAX1
                  IF(DTN1(K1)<DTMIN1(11))THEN
                    N = INDTN1(K1) 
                    INER = IN(N)
                    DT2P = DTMIN1(11)/DTFAC1(11)
                    INER = HALF * STIFR(N) * DT2P * DT2P * ONEP00001
                    IF(TAGR(N)==1)THEN
                      IF(WEIGHT(N)==1)THEN
                       DINERT = DINERT + ( INER - IN(N))
                       ADI(N) = INER*(ONE+ADI(N))/IN(N)  - ONE
                      END IF
                      IN(N) = MAX(INER,IN(N))
                      IMSCH = 1
                    ENDIF
                    DTN1(K1)=DTMIN1(11)
                  ENDIF
                ENDDO
C
              ELSE
#include      "vectorize.inc"
                DO K1=1,KMAX1
                  IF(DTN1(K1)<DTMIN1(11))THEN
                    N = INDTN1(K1) 
                    INER = IN(N)
                    DT2P = DTMIN1(11)/DTFAC1(11)
                    INER = HALF * STIFR(N) * DT2P * DT2P * ONEP00001
                    IF(TAGR(N)==1)THEN
                      DINERT = DINERT + ( INER - IN(N))*WEIGHT(N)
                      IN(N) = MAX(INER,IN(N))
                      IMSCH = 1
                    ENDIF
                    DTN1(K1)=DTMIN1(11)
                  ENDIF
                ENDDO
              ENDIF
            ELSEIF(IDTMIN(11)==4)THEN
#include      "vectorize.inc"
             DO K1=1,KMAX1
              IF(DTN1(K1)<DTMIN1(11))THEN
                  N = INDTN1(K1) 
                  DT2P = DTMIN1(11)/DTFAC1(11)
                  MAS = HALF * STIFR(N) * DT2P * DT2P 
                  MAS=IN(N)/MAS
                  DTN1(K1)=DTMIN1(11)
                  AR(1,N)=AR(1,N)*MAS
                  AR(2,N)=AR(2,N)*MAS
                  AR(3,N)=AR(3,N)*MAS
              ENDIF
             ENDDO
            ENDIF
           ENDIF
C-----------
           IF(DTNOD1<DT2T)THEN
            DO K1=1,KMAX1
              IF(DTN1(K1)<DT2T)THEN
                N = INDTN1(K1) 
                NELTST  = ITAB(N)
                ITYPTST = 11
                DT2T = DTN1(K1)
              ENDIF
            ENDDO
           ENDIF
C-----------
           IF(DTNOD2<DTMINS)THEN
            IF (ANIM_N(12)+OUTP_N(3)+H3D_DATA%N_SCAL_DINER >0) THEN
#include    "vectorize.inc"
              DO K2=1,KMAX2
                IF(DTN2(K2)<DTMINS)THEN
                  N = INDTN2(K2) 
                  INER = IN(N)
                  DT2P = DTMINS/DTFACS
                  INER = HALF * STIFR(N) * DT2P * DT2P * ONEP00001
                  IF(NODNX_SMS(N)/=0)THEN
                    IF(WEIGHT(N)==1)THEN
                     DINERT = DINERT + ( INER - IN(N))
                     ADI(N) = INER*(ONE+ADI(N))/IN(N)  - ONE
                    END IF
                    IN(N) = MAX(INER,IN(N))
                    IMSCH = 1
                    DTN2(K2)=DTMINS
                  ENDIF
                ENDIF
              ENDDO
C
            ELSE
#include    "vectorize.inc"
              DO K2=1,KMAX2
                IF(DTN2(K2)<DTMINS)THEN
                  N = INDTN2(K2) 
                  INER = IN(N)
                  DT2P = DTMINS/DTFACS
                  INER = HALF * STIFR(N) * DT2P * DT2P * ONEP00001
                  IF(NODNX_SMS(N)/=0)THEN
                    DINERT = DINERT + ( INER - IN(N))*WEIGHT(N)
                    IN(N) = MAX(INER,IN(N))
                    IMSCH = 1
                    DTN2(K2)=DTMINS
                  ENDIF
                ENDIF
              ENDDO
            ENDIF
           ENDIF
C-----------
           IF(DTNOD2<DT2T)THEN
            DO K2=1,KMAX2
              IF(DTN2(K2)<DT2T)THEN
                N = INDTN2(K2) 
                NELTST  = ITAB(N)
                ITYPTST = 11
                DT2T = DTN2(K2)
              ENDIF
            ENDDO
           ENDIF
#include      "vectorize.inc"
            DO N=I,MIN(NODLT,I+1023)
              STIFN(N) = EM20
            ENDDO
#include      "vectorize.inc"
            DO N=I,MIN(NODLT,I+1023)
              STIFR(N) = EM20
            ENDDO
C----------------------------------------------------------
C     ANIM TIME STEP 
C----------------------------------------------------------
           IF(ANIM_N(1)+OUTP_N(1)+H3D_DATA%N_SCAL_DT >0)THEN
#include      "vectorize.inc"
             DO K1=1,KMAX1
               N = INDTN1(K1) 
               ADT(N)=MIN(ADT(N),DTN1(K1))
             ENDDO
#include      "vectorize.inc"
             DO K2=1,KMAX2
               N = INDTN2(K2) 
               ADT(N)=MIN(ADT(N),DTN2(K2))
             ENDDO
           ENDIF
 450      CONTINUE
C
      ENDIF

      IF(IDTMIN(11)==8) THEN
        IF(DT2T < DTMIN1(11)) DT2T=MIN(DT2S,DTMIN1(11))
      END IF

C--------------------------------
       IF (IDTMIN(11)/=5)THEN
        IF(ISTOP>0)THEN
          TSTOP = TT
#include "lockon.inc"
          WRITE(IOUT,*)
     .     ' **ERROR : NODAL TIME STEP LESS OR EQUAL DTMIN N=',ISTOP
          WRITE(ISTDO,*)
     .     ' **ERROR : NODAL TIME STEP LESS OR EQUAL DTMIN N=',ISTOP
#include "lockoff.inc"
        ELSEIF(ISTOP<0)THEN
          TSTOP = TT
#include "lockon.inc"
          WRITE(IOUT,*)
     .     ' **ERROR : NEGATIVE STIFFNESS NODE',-ISTOP
          WRITE(ISTDO,*)
     .     ' **ERROR : NEGATIVE STIFFNESS NODE',-ISTOP
             IF ( ISTAMPING == 1) THEN
                 WRITE(ISTDO,'(A)')'The run encountered a problem in an in
     .terface Type 7.'
               WRITE(ISTDO,'(A)')'You may need to check if there is enou 
     .gh clearance between the tools,'
               WRITE(ISTDO,'(A)')'and that they do not penetrate each ot
     .her during their travel'
                 WRITE(IOUT, '(A)')'The run encountered a problem in an in
     .terface Type 7.'
               WRITE(IOUT, '(A)')'You may need to check if there is enou
     .gh clearance between the tools,'
               WRITE(IOUT, '(A)')'and that they do not penetrate each ot
     .her during their travel'
               ENDIF
#include "lockoff.inc"
        ENDIF
       ELSE
        IF(ISTOP>0)THEN
          MSTOP = 2
#include "lockon.inc"
          WRITE(IOUT,*)
     .     ' **ERROR : NODAL TIME STEP LESS OR EQUAL DTMIN N=',ISTOP
          WRITE(ISTDO,*)
     .     ' **ERROR : NODAL TIME STEP LESS OR EQUAL DTMIN N=',ISTOP
#include "lockoff.inc"
        ELSEIF(ISTOP<0)THEN
          MSTOP = 2
#include "lockon.inc"
          WRITE(IOUT,*)
     .     ' **ERROR : NEGATIVE STIFFNESS NODE',-ISTOP
          WRITE(ISTDO,*)
     .     ' **ERROR : NEGATIVE STIFFNESS NODE',-ISTOP
             IF ( ISTAMPING == 1) THEN
                 WRITE(ISTDO,'(A)')'The run encountered a problem in an in
     .terface Type 7.'
               WRITE(ISTDO,'(A)')'You may need to check if there is enou 
     .gh clearance between the tools,'
               WRITE(ISTDO,'(A)')'and that they do not penetrate each ot
     .her during their travel'
                 WRITE(IOUT, '(A)')'The run encountered a problem in an in
     .terface Type 7.'
               WRITE(IOUT, '(A)')'You may need to check if there is enou
     .gh clearance between the tools,'
               WRITE(IOUT, '(A)')'and that they do not penetrate each ot
     .her during their travel'
               ENDIF
#include "lockoff.inc"
        ENDIF
       ENDIF
       IF(IPARIT==0) THEN
         IF(IRODDL==0) THEN
           DO N = NODFT, NODLT
             STIFN(N) = STIFN(N)*WEIGHT(N)
           ENDDO
         ELSE
           DO N = NODFT, NODLT
             STIFN(N) = STIFN(N)*WEIGHT(N)
             STIFR(N) = STIFR(N)*WEIGHT(N)
           ENDDO
         ENDIF
       ENDIF
C
      RETURN
      END
