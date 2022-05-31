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
Chd|  DTNODA                        source/time_step/dtnoda.F     
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE DTNODA(NODFT ,NODLT ,NELTST,ITYPTST,ITAB   ,
     2                  MS    ,IN    ,STIFN ,STIFR  ,DT2T   ,
     3                  DMAST ,DINERT,ADT   ,ADM    ,IMSCH  ,
     4                  WEIGHT,A     ,AR    ,IGRNOD ,
     5                  ADI   ,RBYM  ,ARBY  ,ARRBY  ,WEIGHT_MD,
     6                  MCP   ,MCP_OFF,CONDN ,NALE  ,H3D_DATA)
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
#include      "rad2r_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "scr02_c.inc"
#include      "scr05_c.inc"
#include      "scr07_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "units_c.inc"
#include      "com08_c.inc"
#include      "couple_c.inc"
#include      "parit_c.inc"
#include      "impl1_c.inc"
#include      "vect01_c.inc"
#include      "alesolver.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODFT, NODLT,NELTST,ITYPTST,ITAB(*),
     .        WEIGHT(*),IMSCH,WEIGHT_MD(*)
      INTEGER,INTENT(IN) :: NALE(*)
      my_real DT2T, DMAST, DINERT,
     .   MS(*) ,IN(*) ,STIFN(*), STIFR(*),ADT(*) ,ADM(*) ,
     .   A(3,*) ,AR(3,*) ,ADI(*) ,RBYM(NFRBYM,*),ARBY(3,*),
     .   ARRBY(3,*),CONDN(*),MCP(*),MCP_OFF(*)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD) :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, NN, K, KMAX, ISTOP,I,J,
     .   INDTN(1024), TAG(NUMNOD),IOK, NOD_LAG_COUNT
      my_real DTN(1024),
     .   DTNOD, DT2P, MAS, INER, MAS0, DMM, MASS, STI, DT2S
C----------------------------------------------------------
C     TAG NODAL GROUP 
C----------------------------------------------------------
      IF (NCYCLE == 0) THEN
!$OMP SINGLE
        IF(IDTGR(11).LT.0)THEN
          IOK = 0
          DO N=1,NGRNOD
            IF (-IDTGR(11).EQ.IGRNOD(N)%ID) THEN
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
!$OMP END SINGLE
      ENDIF
      
      IF(IDTGR(11).NE.0)THEN   
        DO N=NODFT,NODLT                                             
          TAG(N) = 0                                                 
        ENDDO                                                        
        DO N=1,IGRNOD(IDTGR(11))%NENTITY
          TAG(IGRNOD(IDTGR(11))%ENTITY(N)) = 1
        ENDDO                                                        
      ELSE                                                          
        DO N=NODFT,NODLT                                             
          TAG(N) = 1                                                 
        ENDDO                                                        
      ENDIF
      
      !ALE or EULER grid points are not concerned about /DT/NODA & /DT/NODA/CST options
      IF(IALE+IEULER.NE.0 .AND. I_DT_NODA_ALE_ON.EQ.0 .AND. NODADT.NE.0)THEN
        !NOD_LAG_COUNT = 0
        DO N=NODFT,NODLT
          IF(IABS(NALE(N)).EQ.1 .OR. IABS(NALE(N)).EQ.2)THEN
            TAG(N) = 0     
         ! ELSE
         !   NOD_LAG_COUNT = NOD_LAG_COUNT + 1
          ENDIF                                                        
        ENDDO        
        !IF(NOD_LAG_COUNT == 0)THEN
        !  NODADT = 0
        !  IDTMIN(1) = 0
        !  IDTMIN(2) = 0
        !  IDTMIN(3) = 0
        !  IDTMIN(7) = 0
        !ENDIF 
      ENDIF      
                                                      
C----------------------------------------------------------
C     ANIM TIME STEP 
C----------------------------------------------------------
cc      IMSCH = 0
      IF(ANIM_N(1)+OUTP_N(1)+H3D_DATA%N_SCAL_DT .GT.0)THEN
#include      "vectorize.inc"
        DO NN=NODFT,NODLT
          ADT(NN) = EP06
        ENDDO
      ENDIF

C----------------------------------------------------------
C     SCALE TIME STEP 
C----------------------------------------------------------
       ISTOP = 0
C
      IF(NODADT_THERM == 1)THEN
C-----------------------------
C       NODAL THERMAL TIME STEP 
C----------------------------------------------------------
        DO I=NODFT,NODLT,1024
         K = 0
         DTNOD = EP20
C true dependency
         DO N=I,MIN(NODLT,I+1023)
          MCP(N) = MCP(N) * MCP_OFF(N)
          IF(MCP(N) .LT. EM10) MCP(N) = ZERO
          IF(CONDN(N).LE.ZERO)THEN
             ISTOP = -ITAB(N)
          ELSEIF(MCP(N).GT.ZERO)THEN
             K = K + 1
             INDTN(K) = N
          ENDIF
         ENDDO
         KMAX = K
C vectorize sqrt
#include      "simd.inc"
         DO K=1,KMAX
           N = INDTN(K)
           DTN(K) = DTFAC1(11)*TWO * MCP(N) / CONDN(N)
           DTNOD = MIN(DTNOD,DTN(K))   
         END DO
C
         IF(DTNOD.LT.DTMIN1(11))THEN
#include      "vectorize.inc"
            DO K=1,KMAX
              IF(DTN(K).LT.DTMIN1(11))THEN
                  N = INDTN(K) 
                  ISTOP = ITAB(N)
              ENDIF
            ENDDO
         ENDIF
C
         IF(DTNOD.LT.DT2T)THEN
#include      "vectorize.inc"
          DO K=1,KMAX
              IF(DTN(K).LT.DT2T)THEN
                N = INDTN(K) 
                NELTST  = ITAB(N)
                ITYPTST = 11
                DT2T = DTN(K)
              ENDIF
          ENDDO
         ENDIF
C      
           DO N=I,MIN(NODLT,I+1023)
              CONDN(N) = EM20
           ENDDO
C----------------------------------------------------------
C     ANIM TIME STEP 
C----------------------------------------------------------
         IF(ANIM_N(1)+OUTP_N(1)+H3D_DATA%N_SCAL_DT .GT.0)THEN
#include      "vectorize.inc"
           DO K=1,KMAX
             ADT(INDTN(K))=DTN(K)
           ENDDO
         ENDIF
        ENDDO
C--------------------------------
        DO N = NODFT, NODLT
          CONDN(N) = EM20
        ENDDO

      ELSE ! Mechanical Time step

       IF(NODADT.GT.0)THEN

        DT2S=DT2T
C----------------------------------------------------------
C       NODAL TIME STEP (TRANSLATION)
C----------------------------------------------------------
        DO 340 I=NODFT,NODLT,1024
         K = 0
         DTNOD = EP20
C true dependency
         IF(IALE+IEULER .EQ. 0 .OR. I_DT_NODA_ALE_ON.EQ.1)THEN
           DO N=I,MIN(NODLT,I+1023)
             IF(STIFN(N).LE.ZERO)THEN                             
                ISTOP = -ITAB(N)                                  
             ELSEIF(MS(N).GT.ZERO)THEN                            
                K = K + 1                                         
                INDTN(K) = N                                      
             ENDIF                                                
           ENDDO
         ELSE
           DO N=I,MIN(NODLT,I+1023)
             IF(STIFN(N).LE.ZERO)THEN                             
                ISTOP = -ITAB(N)                                  
             ELSEIF(MS(N).GT.ZERO)THEN
               IF(NALE(N).EQ.0.OR.NALE(N).EQ.3)THEN                            
                K        = K + 1                                         
                INDTN(K) = N 
               ENDIF                                     
             ENDIF                                                
           ENDDO         
         ENDIF
         
         KMAX = K

C vectorize sqrt
#include      "simd.inc"
         DO K=1,KMAX
           N      = INDTN(K)
           DTN(K) = DTFAC1(11)*SQRT(TWO * MS(N) / STIFN(N))
           DTNOD  = MIN(DTNOD,DTN(K))
         END DO
C
         IF(DTNOD.LT.DTMIN1(11))THEN
          IF(IDTMIN(11).EQ.1.OR.IDTMIN(11).EQ.5)THEN
#include      "vectorize.inc"
            DO K=1,KMAX
              IF(DTN(K).LT.DTMIN1(11))THEN
                  N     = INDTN(K) 
                  ISTOP = ITAB(N)
              ENDIF
            ENDDO
          ELSEIF(IDTMIN(11)==3.OR.IDTMIN(11)==8)THEN
C----------------------------------------------------------
            IF (ANIM_N(2)+OUTP_N(2)+H3D_DATA%N_SCAL_DMAS .GT.0) THEN
#include      "vectorize.inc"
              DO K=1,KMAX
                IF(DTN(K).LT.DTMIN1(11))THEN
                  N = INDTN(K) 
                  DT2P = DTMIN1(11)/DTFAC1(11)
                  MAS = HALF * STIFN(N) * DT2P * DT2P * ONEP00001
                  IF(TAG(N).EQ.1)THEN
                    IF(WEIGHT_MD(N).EQ.1) THEN
                      DMAST =  DMAST + MAS - MS(N)
                      ! In 2D, MS(N) is re-written to its initial value at each timestep
                      IF (N2D /= 0) THEN
                        ADM(N) = (MAS-MS(N))/MS(N)
                      ELSE
                        ADM(N) = MAS*(ONE+ADM(N))/MS(N)  - ONE
                      ENDIF
                    ENDIF
                    MS(N) = MAS
                    IMSCH = 1
                  ENDIF
                ENDIF
              ENDDO
            ELSE
#include      "vectorize.inc"
              DO K=1,KMAX
                IF(DTN(K).LT.DTMIN1(11))THEN
                  N = INDTN(K) 
                  DT2P = DTMIN1(11)/DTFAC1(11)
                  MAS = HALF * STIFN(N) * DT2P * DT2P * ONEP00001
                  IF(TAG(N).EQ.1)THEN
                    DMAST =  DMAST + (MAS - MS(N))*WEIGHT_MD(N)
                    MS(N) = MAS
                    IMSCH = 1
                  ENDIF
                ENDIF
              ENDDO
            ENDIF
          ELSEIF(IDTMIN(11).EQ.4)THEN
#include      "vectorize.inc"
            DO K=1,KMAX
              IF(DTN(K).LT.DTMIN1(11))THEN
                  N = INDTN(K) 
                  DT2P = DTMIN1(11)/DTFAC1(11)
                  MAS = HALF * STIFN(N) * DT2P * DT2P 
                  MAS=MS(N)/MAS
                  DTN(K)=DTMIN1(11)
                  A(1,N)=A(1,N)*MAS
                  A(2,N)=A(2,N)*MAS
                  A(3,N)=A(3,N)*MAS
              ENDIF
            ENDDO
          ENDIF
         ENDIF
         IF(DTNOD.LT.DT2T)THEN
#include      "vectorize.inc"
          DO K=1,KMAX
              IF(DTN(K).LT.DT2T)THEN
                N = INDTN(K) 
                NELTST  = ITAB(N)
                ITYPTST = 11
                DT2T = DTN(K)
              ENDIF
          ENDDO
         ENDIF
C
           DO N=I,MIN(NODLT,I+1023)
              STIFN(N) = EM20
           ENDDO
C----------------------------------------------------------
C     ANIM TIME STEP 
C----------------------------------------------------------
         IF(ANIM_N(1)+OUTP_N(1)+H3D_DATA%N_SCAL_DT .GT.0)THEN
#include      "vectorize.inc"
           DO K=1,KMAX
             ADT(INDTN(K))=DTN(K)
           ENDDO
         ENDIF
 340    CONTINUE
C
C  rigid material
C 
C----------------------------------------------------------
C       NODAL TIME STEP (TRANSLATION)
C----------------------------------------------------------
       IF(IRIGID_MAT > 0) THEN
         K = 0
C true dependency
         DO  I=1,NRBYM
           DTNOD = EP20
           MASS = RBYM(1,I)
           STI  = RBYM(27,I)
           IF(STI.LE.ZERO)THEN
c le rigi materiau
              ISTOP = -(I+ NUMNOD)
           ELSEIF(MASS.GT. ZERO)THEN
              K = K + 1
C              DTN(K) = DTFAC1(11)*SQRT(2. * MASS / STI)
C              DTNOD = MIN(DTNOD,DTN(K))
              INDTN(K) = I
           ENDIF
          ENDDO
         KMAX = K
C vectorize sqrt
#include      "simd.inc"
         DO K=1,KMAX
            N = INDTN(K)
            DTN(K) = DTFAC1(11)*SQRT(TWO * MASS / STI)
            DTNOD = MIN(DTNOD,DTN(K))
         END DO
C
         IF(DTNOD.LT.DTMIN1(11))THEN
          IF(IDTMIN(11).EQ.1.OR.IDTMIN(11).EQ.5)THEN
#include      "vectorize.inc"
            DO K=1,KMAX
              IF(DTN(K) < DTMIN1(11))THEN
                  N = INDTN(K) 
                  ISTOP = N + NUMNOD
              ENDIF
            ENDDO
          ELSEIF(IDTMIN(11)==3.OR.IDTMIN(11)==8)THEN
C----------------------------------------------------------
#include      "vectorize.inc"
            DO K=1,KMAX
              IF(DTN(K).LT.DTMIN1(11))THEN
                N = INDTN(K) 
                MASS = RBYM(1,N)
                STI = RBYM(27,N)                   
                DT2P = DTMIN1(11)/DTFAC1(11)
                MAS = HALF * STI * DT2P * DT2P * ONEP00001
c                IF(TAG(N).EQ.1)THEN
c                  IF(WEIGHT(N).EQ.1) THEN
c                    DMAST =  DMAST + MAS - MS(N)
c                    ADM(N) = MAS*(ONE+ADM(N))/MS(N)  - ONE
c                  ENDIF
                   RBYM(1,N) = MAS
c                  IMSCH = 1
c                ENDIF
              ENDIF
            ENDDO
          ELSEIF(IDTMIN(11).EQ.4)THEN
#include      "vectorize.inc"
            DO K=1,KMAX
              IF(DTN(K).LT.DTMIN1(11))THEN
                  N = INDTN(K) 
                  MASS = RBYM(1,N)
                  STI = RBYM(27,N)
                  DT2P = DTMIN1(11)/DTFAC1(11)
                  MAS = HALF * STI * DT2P * DT2P 
                  MAS=MASS/MAS
                  DTN(K)=DTMIN1(11)
                  ARBY(1,N)=ARBY(1,N)*MAS
                  ARBY(2,N)=ARBY(2,N)*MAS
                  ARBY(3,N)=ARBY(3,N)*MAS
              ENDIF
            ENDDO
          ENDIF
         ENDIF
         IF(DTNOD.LT.DT2T)THEN
#include      "vectorize.inc"
          DO K=1,KMAX
              IF(DTN(K).LT.DT2T)THEN
                N = INDTN(K) 
                NELTST  = N + NUMNOD
                ITYPTST = 11
                DT2T = DTN(K)
              ENDIF
          ENDDO
         ENDIF
C----------------------------------------------------------
C     ANIM TIME STEP 
C----------------------------------------------------------
         IF(ANIM_N(1)+OUTP_N(1)+H3D_DATA%N_SCAL_DT .GT.0)THEN
#include      "vectorize.inc"
           DO K=1,KMAX
             ADT(INDTN(K))=DTN(K)
           ENDDO
         ENDIF
C
       ENDIF
C end rigid material       
C----------------------------------------------------------
C       NODAL TIME STEP (ROTATION)
C----------------------------------------------------------
        IF (IRODDL.NE.0) THEN
          DO 350 I=NODFT,NODLT,1024
           DTNOD = 1.E20
           K = 0
C true dependency
           DO N=I,MIN(NODLT,I+1023)
             IF(STIFR(N).LE.ZERO)THEN
               ISTOP=-ITAB(N)
             ELSEIF(IN(N).GT.ZERO)THEN
               K = K + 1
               INDTN(K) = N
             ENDIF
           ENDDO
           KMAX = K
C vectorize sqrt
#include      "simd.inc"
           DO K=1,KMAX
              N = INDTN(K)
              DTN(K) = DTFAC1(11)*SQRT(TWO * IN(N) / STIFR(N))
              DTNOD = MIN(DTNOD,DTN(K))            
           END DO
C
           IF(DTNOD.LT.DTMIN1(11))THEN
            IF(IDTMIN(11).EQ.1.OR.IDTMIN(11).EQ.5)THEN
#include      "vectorize.inc"
              DO K=1,KMAX
                IF(DTN(K).LT.DTMIN1(11))THEN
                  N = INDTN(K) 
                  ISTOP = ITAB(N)
                ENDIF
              ENDDO
            ELSEIF(IDTMIN(11)==3.OR.IDTMIN(11)==8)THEN
              IF (ANIM_N(12)+OUTP_N(3)+H3D_DATA%N_SCAL_DINER .GT.0) THEN
#include      "vectorize.inc"
                DO K=1,KMAX
                  IF(DTN(K).LT.DTMIN1(11))THEN
                    N = INDTN(K) 
                    INER = IN(N)
                    DT2P = DTMIN1(11)/DTFAC1(11)
                    INER = HALF * STIFR(N) * DT2P * DT2P * ONEP00001
                    IF(TAG(N).EQ.1)THEN
                      IF(WEIGHT_MD(N).EQ.1)THEN
                       DINERT = DINERT + ( INER - IN(N))
                       ADI(N) = INER*(ONE+ADI(N))/IN(N)  - ONE
                      END IF
                      IN(N) = MAX(INER,IN(N))
                      IMSCH = 1
                    ENDIF
                  ENDIF
                ENDDO
              ELSE
#include      "vectorize.inc"
                DO K=1,KMAX
                  IF(DTN(K).LT.DTMIN1(11))THEN
                    N = INDTN(K) 
                    INER = IN(N)
                    DT2P = DTMIN1(11)/DTFAC1(11)
                    INER = HALF * STIFR(N) * DT2P * DT2P * ONEP00001
                    IF(TAG(N).EQ.1)THEN
                      DINERT = DINERT + ( INER - IN(N))*WEIGHT_MD(N)
                      IN(N) = MAX(INER,IN(N))
                      IMSCH = 1
                    ENDIF
                  ENDIF
                ENDDO
              ENDIF
            ELSEIF(IDTMIN(11).EQ.4)THEN
#include      "vectorize.inc"
             DO K=1,KMAX
              IF(DTN(K).LT.DTMIN1(11))THEN
                  N = INDTN(K) 
                  DT2P = DTMIN1(11)/DTFAC1(11)
                  MAS = HALF * STIFR(N) * DT2P * DT2P 
                  MAS=IN(N)/MAS
                  DTN(K)=DTMIN1(11)
                  AR(1,N)=AR(1,N)*MAS
                  AR(2,N)=AR(2,N)*MAS
                  AR(3,N)=AR(3,N)*MAS
              ENDIF
             ENDDO
            ENDIF
           ENDIF
           IF(DTNOD.LT.DT2T)THEN
#include      "vectorize.inc"
            DO K=1,KMAX
              IF(DTN(K).LT.DT2T)THEN
                N = INDTN(K) 
                NELTST  = ITAB(N)
                ITYPTST = 11
                DT2T = DTN(K)
              ENDIF
            ENDDO
           ENDIF
C
            DO N=I,MIN(NODLT,I+1023)
              STIFR(N) = EM20
            ENDDO
C----------------------------------------------------------
C     ANIM TIME STEP 
C----------------------------------------------------------
           IF(ANIM_N(1)+OUTP_N(1)+H3D_DATA%N_SCAL_DT .GT.0)THEN
#include      "vectorize.inc"
             DO K=1,KMAX
               N = INDTN(K) 
               ADT(N)=MIN(ADT(N),DTN(K))
             ENDDO
           ENDIF
 350      CONTINUE
C
C rigid material
C 
        IF(IRIGID_MAT > 0) THEN
           K = 0
C true dependency
          DO  I=1, NRBYM
             DTNOD = 1.E20
             INER  = RBYM(17, I)
             STI = RBYM(28,I)
               IF(STI.LE.ZERO)THEN
                 ISTOP=-(I + NUMNOD)
               ELSEIF(IN(N).GT.ZERO)THEN
                 K = K + 1
                 INDTN(K) = I 
                 DTN(K) = DTFAC1(11)*SQRT(TWO * INER / STI)
                 DTNOD = MIN(DTNOD,DTN(K))
               ENDIF
           ENDDO
           KMAX = K
C vectorize sqrt
#include      "simd.inc"
           DO K=1,KMAX
              N = INDTN(K)
              DTN(K) = DTFAC1(11)*SQRT(TWO * IN(N) / STIFR(N))
              DTNOD = MIN(DTNOD,DTN(K))            
           END DO
C
           IF(DTNOD.LT.DTMIN1(11))THEN
            IF(IDTMIN(11).EQ.1.OR.IDTMIN(11).EQ.5)THEN
#include      "vectorize.inc"
              DO K=1,KMAX
                IF(DTN(K).LT.DTMIN1(11))THEN
                  N = INDTN(K) 
                  ISTOP = NODFT + N
                ENDIF
              ENDDO
            ELSEIF(IDTMIN(11)==3.OR.IDTMIN(11)==8)THEN
#include      "vectorize.inc"
              DO K=1,KMAX
                IF(DTN(K).LT.DTMIN1(11))THEN
                  N = INDTN(K) 
                  STI = RBYM(28,N)
                  DT2P = DTMIN1(11)/DTFAC1(11)
                  INER = HALF * STI * DT2P * DT2P * ONEP00001
c                    IF(WEIGHT(N).EQ.1)THEN
c                     DINERT = DINERT + ( INER - IN(N))
c                    ADI(N) = INER*(ONE+ADI(N))/IN(N)  - ONE
c                    END IF
                  RBYM(17,N) = MAX(INER,RBYM(17,N))
                  IMSCH = 1
                ENDIF
              ENDDO
C
            ELSEIF(IDTMIN(11).EQ.4)THEN
#include      "vectorize.inc"
             DO K=1,KMAX
              IF(DTN(K).LT.DTMIN1(11))THEN
                  N = INDTN(K)
                  STI = RBYM(28,N)  
                  DT2P = DTMIN1(11)/DTFAC1(11)
                  MAS = HALF * STI * DT2P * DT2P 
                  MAS=RBYM(17,N)/MAS
                  DTN(K)=DTMIN1(11)
                  ARRBY(1,N)=ARRBY(1,N)*MAS
                  ARRBY(2,N)=ARRBY(2,N)*MAS
                  ARRBY(3,N)=ARRBY(3,N)*MAS
              ENDIF
             ENDDO
            ENDIF
           ENDIF
           IF(DTNOD.LT.DT2T)THEN
#include "vectorize.inc"
            DO K=1,KMAX
              IF(DTN(K).LT.DT2T)THEN
                N = INDTN(K) 
                NELTST  = N + NUMNOD
                ITYPTST = 11
                DT2T = DTN(K)
              ENDIF
            ENDDO
           ENDIF
C----------------------------------------------------------
C     ANIM TIME STEP 
C----------------------------------------------------------
           IF(ANIM_N(1)+OUTP_N(1)+H3D_DATA%N_SCAL_DT .GT.0)THEN
#include      "vectorize.inc"
             DO K=1,KMAX
               N = INDTN(K) 
               ADT(N)=MIN(ADT(N),DTN(K))
             ENDDO
           ENDIF
       ENDIF 
C --end rigid material  
      ENDIF

      IF(IDTMIN(11)==8) THEN
        IF(DT2T < DTMIN1(11)) DT2T=MIN(DT2S,DTMIN1(11))
      END IF

C----------------------------------------------------------
C       INTERFACES 7
C----------------------------------------------------------
      ELSEIF(I7KGLO.GT.0)THEN

        DT2S=DT2T

        DO 370 I=NODFT,NODLT,1024
         K = 0
         DTNOD = EP20
C true dependency
         DO N=I,MIN(NODLT,I+1023)
           IF(STIFN(N).LE.ZERO)THEN   
              ISTOP=-ITAB(N)          
           ELSEIF(MS(N).GT.ZERO)THEN  
              K = K + 1               
              INDTN(K) = N            
           ENDIF                      
         ENDDO
         KMAX = K
C vectorize sqrt
#include      "simd.inc"
         DO K=1,KMAX
           N = INDTN(K)
           DTN(K) = DTFAC1(11)*SQRT(TWO * MS(N) / STIFN(N))
           DTNOD = MIN(DTNOD,DTN(K))
         END DO

C
         IF(DTNOD.LT.DTMIN1(11))THEN
           IF(IDTMIN(11).EQ.1.OR.IDTMIN(11).EQ.5)THEN
#include      "vectorize.inc"
              DO K=1,KMAX
                IF(DTN(K).LT.DTMIN1(11))THEN
                  N = INDTN(K) 
                  ISTOP = ITAB(N)
                ENDIF
              ENDDO
           ELSEIF(IDTMIN(11)==3.OR.IDTMIN(11)==8)THEN
C----------------------------------------------------------
             IF (ANIM_N(2)+OUTP_N(2)+H3D_DATA%N_SCAL_DMAS .GT.0) THEN
#include      "vectorize.inc"
               DO K=1,KMAX
                IF(DTN(K).LT.DTMIN1(11))THEN
                  N = INDTN(K) 
                  DT2P = DTMIN1(11)/DTFAC1(11)
                  MAS = HALF * STIFN(N) * DT2P * DT2P * ONEP00001
                  IF(TAG(N).EQ.1)THEN
                    IF(WEIGHT_MD(N).EQ.1) THEN
                      DMAST = DMAST + MAS - MS(N)
                      ADM(N) = MAS*(ONE+ADM(N))/MS(N)  - ONE
                    ENDIF
                    MS(N) = MAS
                    IMSCH = 1
                  ENDIF
                ENDIF
               ENDDO
             ELSE
#include      "vectorize.inc"
               DO K=1,KMAX
                IF(DTN(K).LT.DTMIN1(11))THEN
                  N = INDTN(K) 
                  DT2P = DTMIN1(11)/DTFAC1(11)
                  MAS = HALF * STIFN(N) * DT2P * DT2P * ONEP00001
                  IF(TAG(N).EQ.1)THEN
                    DMAST = DMAST + (MAS - MS(N))*WEIGHT_MD(N)
                    MS(N) = MAS
                    IMSCH = 1
                  ENDIF
                ENDIF
               ENDDO
             ENDIF
           ENDIF
         ENDIF
C----------------------------------------------------------
         IF(DTNOD.LT.DT2T)THEN
#include      "vectorize.inc"
            DO K=1,KMAX
              IF(DTN(K).LT.DT2T)THEN
                N = INDTN(K) 
                NELTST  = ITAB(N)
                ITYPTST = 11
                DT2T = DTN(K)
              ENDIF
            ENDDO
         ENDIF
C
           DO N=I,MIN(NODLT,I+1023)
             STIFN(N) = EM20
           ENDDO
           IF (IRODDL.NE.0) THEN
             DO N=I,MIN(NODLT,I+1023)
               STIFR(N) = EM20
             ENDDO
           ENDIF
C----------------------------------------------------------
C     ANIM TIME STEP 
C----------------------------------------------------------
        IF(ANIM_N(1)+OUTP_N(1)+H3D_DATA%N_SCAL_DT .GT.0)THEN
#include      "vectorize.inc"
          DO K=1,KMAX
            N = INDTN(K) 
            ADT(N)=MIN(ADT(N),DTN(K))
          ENDDO
        ENDIF
 370    CONTINUE

        IF(IDTMIN(11)==8) THEN
          IF(DT2T < DTMIN1(11)) DT2T=MIN(DT2S,DTMIN1(11))
        END IF
  
C--------------------------------

        DO N = NODFT, NODLT
          STIFN(N) = EM20
        ENDDO
        IF (IRODDL.NE.0) THEN
          DO N = NODFT, NODLT
            STIFR(N) = EM20
          ENDDO
        ENDIF
C--------------------------------
      ENDIF
C--------------------------------
       IF (IDTMIN(11).NE.5.AND.INCONV.EQ.1)THEN
        IF(ISTOP.GT.0)THEN
          TSTOP = TT
#include "lockon.inc"
          WRITE(IOUT,*)
     .     ' **ERROR : NODAL TIME STEP LESS OR EQUAL DTMIN N=',ISTOP
          WRITE(ISTDO,*)
     .     ' **ERROR : NODAL TIME STEP LESS OR EQUAL DTMIN N=',ISTOP
#include "lockoff.inc"
        ELSEIF(ISTOP.LT.0)THEN
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
       ELSEIF (INCONV.EQ.1)THEN
        IF(ISTOP.GT.0)THEN
          MSTOP = 2
#include "lockon.inc"
          WRITE(IOUT,*)
     .     ' **ERROR : NODAL TIME STEP LESS OR EQUAL DTMIN N=',ISTOP
          WRITE(ISTDO,*)
     .     ' **ERROR : NODAL TIME STEP LESS OR EQUAL DTMIN N=',ISTOP
#include "lockoff.inc"
        ELSEIF(ISTOP.LT.0)THEN
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
C
       IF(IPARIT.EQ.0) THEN
         IF(IRODDL.EQ.0) THEN
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

      ENDIF
C
      RETURN
      END
