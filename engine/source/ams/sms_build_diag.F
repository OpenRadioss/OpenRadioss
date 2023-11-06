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
Chd|  SMS_BUILD_DIAG                source/ams/sms_build_diag.F
Chd|-- called by -----------
Chd|        SMS_BUILD_MAT_2               source/ams/sms_build_mat_2.F
Chd|-- calls ---------------
Chd|        FOAT_TO_6_FLOAT               source/system/parit.F
Chd|        MY_BARRIER                    source/system/machine.F
Chd|        SMS_RBE2_NODXI                source/ams/sms_rbe2.F
Chd|        SMS_RBE3_NODXI                source/ams/sms_rbe3.F
Chd|        SPMD_ALLGLOB_ISUM9            source/mpi/generic/spmd_allglob_isum9.F
Chd|        SPMD_EXCH_NODNX               source/mpi/ams/spmd_exch_nodnx.F
Chd|        SPMD_EXCH_SMS                 source/mpi/ams/spmd_exch_sms.F
Chd|        SPMD_EXCH_SMS6                source/mpi/ams/spmd_exch_sms6.F
Chd|        SPMD_FRWALL_NN                source/mpi/kinematic_conditions/spmd_frwall_nn.F
Chd|        SPMD_IBCAST                   source/mpi/generic/spmd_ibcast.F
Chd|        SPMD_MIJ_SMS                  source/mpi/ams/spmd_sms.F
Chd|        SPMD_SD_CJ_2                  source/mpi/kinematic_conditions/spmd_sd_cj_2.F
Chd|====================================================================
      SUBROUTINE SMS_BUILD_DIAG(
     1             ITASK    ,NODFT   ,NODLT  ,MS      ,NODII_SMS   ,
     2             JAD_SMS ,JDI_SMS  ,LT_SMS ,DIAG_SMS,INDX1_SMS   ,
     3             INDX2_SMS,IAD_ELEM,FR_ELEM  ,NPBY   ,LPBY,
     4             LAD_SMS ,KAD_SMS  ,JRB_SMS ,MSKYI_SMS,ISKYI_SMS ,
     5             JADI_SMS,JDII_SMS ,LTI_SMS  ,NODXI_SMS,FR_SMS   ,
     6             FR_RMS  ,LIST_SMS ,LIST_RMS ,MSKYI_FI_SMS,ILINK ,
     7             RLINK   ,NNLINK   ,LNLINK   ,TAG_LNK_SMS ,LJOINT,
     8             IADCJ   ,FR_CJ    ,ITAB     ,WEIGHT      ,IMV   ,
     9             MV      ,MV6      ,W6       ,NPRW        ,LPRW  ,
     A             FR_WALL ,NRWL_SMS ,TAGMSR_RBY_SMS,RBY    ,AWORK ,
     B             X       ,A        ,AR       ,IN          ,V     ,
     C             VR      ,TAGSLV_RBY_SMS,IRBE2,LRBE2      ,IRBE3 ,
     D             LRBE3   ,IAD_RBE3M,FR_RBE3M )
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
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "sms_c.inc"
#include      "scr03_c.inc"
#include      "task_c.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER ITASK, NODFT, NODLT,
     .          JAD_SMS(*), JDI_SMS(*),
     .          INDX1_SMS(*), INDX2_SMS(*),
     .          IAD_ELEM(2,NSPMD+1) ,FR_ELEM(*),
     .          NPBY(NNPBY,*), LPBY(*),
     .          LAD_SMS(*), KAD_SMS(*), JRB_SMS(*),
     .          ISKYI_SMS(LSKYI_SMS,*),
     .          JADI_SMS(*), JDII_SMS(*), NODXI_SMS(*), NODII_SMS(*),
     .        FR_SMS(NSPMD+1), FR_RMS(NSPMD+1), LIST_SMS(*), LIST_RMS(*),
     .          ILINK(*), RLINK(*), NNLINK(10,*), LNLINK(*),
     .          TAG_LNK_SMS(*), LJOINT(*), FR_CJ(*),IADCJ(NSPMD+1,*),
     .          ITAB(*), WEIGHT(*), IMV(*),
     .          NPRW(*), LPRW(*), FR_WALL(NSPMD+2,*), NRWL_SMS(*),
     .          TAGMSR_RBY_SMS(*), TAGSLV_RBY_SMS(*),
     .          IRBE2(*)  ,LRBE2(*),
     .          IRBE3(*), LRBE3(*), IAD_RBE3M(*),FR_RBE3M(*)
        my_real
     .          MS(*), LT_SMS(*), DIAG_SMS(*),
     .          MSKYI_SMS(*), LTI_SMS(*), MSKYI_FI_SMS(*), MV(*),
     .          RBY(NRBY,*), AWORK(3,*), X(3,*), A(3,*), AR(3,*), IN(*),
     .          V(3,*), VR(3,*)
        DOUBLE PRECISION MV6(6,*), W6(6,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, J, K, KN, IKN, JJ, KK, II, IJ, N, M, IX, KMV
        INTEGER NG, ITY, NEL, NFT, ISOLNOD,ILOC4(4)
        INTEGER MSR, NSN, KI, KJ, KL, NSR, LOC_PROC, NN, MAIN
        INTEGER K1, IC, ISMS,
     .          NOD2ADD(NUMNOD), ICSIZE, IMOV, ITYP, ILAGM, ICOUNT,
     .          N2, N3, N4, N5, N6, N7
        INTEGER SIZE, LENR, IAD, L, LLT, KAD, JI,
     .          NODFT1_SMS, NODLT1_SMS, NODFT2_SMS, NODLT2_SMS,
     .          KADI_SMS(NUMNOD), NADI_SMS(NUMNOD), NINDXT
        my_real
     .          MELE4, MELE12, XN, LTIJ, MSLV,
     .          IXX, IYY, IZZ, XX, YY, ZZ, MAS,
     .          VRX, VRY, VRZ, V1, V2, V3, GX, GY, GZ
        DATA ILOC4/2,4,6,8/
C-----------------------------------------------
        NODII_SMS(NODFT:NODLT)=0
        DO N=NODFT,NODLT
          IF(JADI_SMS(N+1) > JADI_SMS(N))THEN
            NODII_SMS(N)=1
          END IF
        END DO
C
        IF(NSPMD > 1)THEN
C
          CALL MY_BARRIER()
C
          IF(ITASK==0) THEN   ! comm sur 1er thread
            DO K=1,FR_RMS(NSPMD+1)-1
              I=LIST_RMS(K)
              IF(I==0)CYCLE
              NODII_SMS(I)=1
            END DO
            LOC_PROC=ISPMD+1
            M = 1
            DO K=1,NSPMD
              IF(K/=LOC_PROC)THEN
                DO J=FR_SMS(K),FR_SMS(K+1)-1
                  I=LIST_SMS(M)
                  M = M + 1
                  IF(I==0)CYCLE
                  NODII_SMS(I)=1
                END DO
              END IF
            END DO

            LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
C
C Echange NODII_SMS
C
            CALL SPMD_EXCH_NODNX(NODII_SMS,IAD_ELEM ,FR_ELEM,LENR)
C
          END IF
C
          CALL MY_BARRIER()
C
        END IF
C
        DO N=NODFT,NODLT
          IF(NODII_SMS(N)/=0)THEN
            NODXI_SMS(N)=1
          END IF
        END DO
C
        IF(NRBE2/=0)THEN
C
          CALL MY_BARRIER()
C
          IF (ITASK==0)THEN
            CALL SMS_RBE2_NODXI(
     1        IRBE2 ,LRBE2 ,NODXI_SMS)
          END IF
        END IF
C
        IF (NRBE3/=0)THEN
C
          CALL MY_BARRIER()
C
          IF (ITASK==0)THEN
            CALL SMS_RBE3_NODXI(
     1        IRBE3 ,LRBE3 ,NODXI_SMS,IAD_RBE3M,FR_RBE3M)
          END IF
        END IF
C
        NINDX1_SMS=0
        NINDX2_SMS=0
C
        CALL MY_BARRIER()
C
        IF(ITASK==0)THEN
          DO N=1,NUMNOD
            IF(NODXI_SMS(N)/=0)THEN
              NINDX1_SMS=NINDX1_SMS+1
              INDX1_SMS(NINDX1_SMS)=N
              NODXI_SMS(N)=NINDX1_SMS
            END IF
            IF(NODII_SMS(N)/=0)THEN
              NINDX2_SMS=NINDX2_SMS+1
              INDX2_SMS(NINDX2_SMS)=N
              NODII_SMS(N)=NINDX2_SMS
            END IF
          END DO
        END IF
C
C-----------------------------------------------
        IF(NLINK+NRLINK+NJOINT/=0)THEN
C
          CALL MY_BARRIER()
C
          IF(ITASK==0)THEN
            NOD2ADD(1:NUMNOD)=0
C---
            IF(NRLINK/=0)THEN
              K = 1
              DO I=1,NRLINK
                K1=4*I-3
                IC=ILINK(K1+1)
                IF(IC==0) CYCLE
                NSN = ILINK(K1)
                ISMS=0
                DO J=1,NSN
                  N=RLINK(K+J-1)
                  IF(NODXI_SMS(N)/=0)THEN
                    ISMS=1
                    EXIT
                  END IF
                END DO

                IF(NSPMD > 1) CALL SPMD_ALLGLOB_ISUM9(ISMS,1)

                IF(ISMS==0)THEN
                  TAG_LNK_SMS(I)=-ABS(TAG_LNK_SMS(I))
                ELSE
                  TAG_LNK_SMS(I)= ABS(TAG_LNK_SMS(I))
                END IF

                IF(ISMS/=0)THEN
C
C propagate AMS to all nodes of the rlink
                  DO J=1,NSN
                    N=RLINK(K+J-1)
                    IF(NODXI_SMS(N)==0.AND.NOD2ADD(N)==0)THEN
                      NINDX1_SMS=NINDX1_SMS+1
                      INDX1_SMS(NINDX1_SMS)=N
                      NODXI_SMS(N)=NINDX1_SMS
                      NOD2ADD(N)=1
                    END IF
                  END DO
C
                END IF
                K = K + NSN
              END DO
            END IF
C---
            IF(NLINK/=0)THEN
              K = 1
              DO I=1,NLINK
                IC=NNLINK(3,I)
                IF(IC==0) CYCLE
                NSN = NNLINK(1,I)
                ISMS=0
                DO J=1,NSN
                  N=LNLINK(K+J-1)
                  IF(NODXI_SMS(N)/=0)THEN
                    ISMS=1
                    EXIT
                  END IF
                END DO

                IF(NSPMD > 1) CALL SPMD_ALLGLOB_ISUM9(ISMS,1)


                IF(ISMS==0)THEN
                  TAG_LNK_SMS(NRLINK+I)=-ABS(TAG_LNK_SMS(NRLINK+I))
                ELSE
                  TAG_LNK_SMS(NRLINK+I)= ABS(TAG_LNK_SMS(NRLINK+I))
                END IF

                IF(ISMS/=0)THEN
C
C propagate AMS to all nodes of the rlink
                  DO J=1,NSN
                    N=LNLINK(K+J-1)
                    IF(NODXI_SMS(N)==0.AND.NOD2ADD(N)==0)THEN
                      NINDX1_SMS=NINDX1_SMS+1
                      INDX1_SMS(NINDX1_SMS)=N
                      NODXI_SMS(N)=NINDX1_SMS
                      NOD2ADD(N)=1
                    END IF
                  END DO
C
                END IF
                K = K + NSN
              END DO
            END IF
C-----------------------------------------------
            IF(NJOINT/=0)THEN
              IF(ISPMD==0)THEN
                K=1
                DO J=1,NJOINT
                  NSN=LJOINT(K)
                  ISMS=0
                  DO I=1,NSN
                    N=LJOINT(K+I)
                    IF(NODXI_SMS(N)/=0)THEN
                      ISMS=1
                      EXIT
                    END IF
                  END DO

                  TAG_LNK_SMS(NRLINK+NLINK+J)=ISMS

                  K=K+NSN+1
                END DO
              END IF
C
              IF(NSPMD > 1)
     .          CALL SPMD_IBCAST(TAG_LNK_SMS(NRLINK+NLINK+1),
     .        TAG_LNK_SMS(NRLINK+NLINK+1),NJOINT,1,0,2)
C

              IF(NSPMD==1)THEN
                K=1
                DO J=1,NJOINT
                  ISMS=TAG_LNK_SMS(NRLINK+NLINK+J)
                  IF(ISMS/=0)THEN
                    NSN=LJOINT(K)
                    DO I=1,NSN
                      N=LJOINT(K+I)
                      IF(NODXI_SMS(N)==0.AND.NOD2ADD(N)==0)THEN
                        NINDX1_SMS=NINDX1_SMS+1
                        INDX1_SMS(NINDX1_SMS)=N
                        NODXI_SMS(N)=NINDX1_SMS
                        NOD2ADD(N)=1
                      END IF
                    END DO
                  END IF
                  K=K+NSN+1
                END DO
              ELSE
                IF(ISPMD==0)THEN
                  K=1
                  DO J=1,NJOINT
                    ISMS=TAG_LNK_SMS(NRLINK+NLINK+J)
                    IF(ISMS/=0)THEN
                      NSN=LJOINT(K)
                      DO I=1,NSN
                        N=LJOINT(K+I)
                        IF(NODXI_SMS(N)==0.AND.NOD2ADD(N)==0)THEN
                          NINDX1_SMS=NINDX1_SMS+1
                          INDX1_SMS(NINDX1_SMS)=N
                          NODXI_SMS(N)=NINDX1_SMS
                          NOD2ADD(N)=1
                        END IF
                      END DO
                    END IF
                    K=K+NSN+1
                  END DO
                END IF
                ICSIZE=0
                DO N=1,NJOINT
                  IF(TAG_LNK_SMS(NRLINK+NLINK+N)/=0)
     .            ICSIZE=ICSIZE+IADCJ(NSPMD+1,N)-IADCJ(1,N)
                END DO
                CALL SPMD_SD_CJ_2(NOD2ADD,LJOINT,FR_CJ,IADCJ,ICSIZE,
     .                            TAG_LNK_SMS(NRLINK+NLINK+1),NODXI_SMS,
     .                                                        INDX1_SMS)
              END IF
            END IF
          END IF
        END IF
C-----------------------------------------------
        IF(NRWALL > 0)THEN
          IF(ITASK==0)THEN
            K = 1
            DO N=1,NRWALL
              N2=N +NRWALL
              N3=N2+NRWALL
              N4=N3+NRWALL
              N5=N4+NRWALL
              N6=N5+NRWALL
              N7=N6+NRWALL
              NSN  =NPRW(N)
              IMOV =NPRW(N3)
              ITYP =NPRW(N4)
              ILAGM=NPRW(N6)
              ICOUNT   =K
              IF(ILAGM==0)THEN
                DO J=1,NSN
                  I=LPRW(K+J-1)
                  IF(NODXI_SMS(I)/=0)THEN
                    NRWL_SMS(ICOUNT)=J
                    ICOUNT=ICOUNT+1
                  END IF
                END DO
              END IF
C nb of ams nodes in the wall
              NPRW(N7)=ICOUNT-K
C for sms_fixvel, etc
              IF(IMOV /= 0)THEN
                NOD2ADD(IMOV)=0
                IF(ICOUNT > K.AND.NODXI_SMS(IMOV)==0)NOD2ADD(IMOV)=1
                IF(NSPMD > 1)
     .            CALL SPMD_FRWALL_NN(FR_WALL(1,N),NOD2ADD(IMOV))
                IF(NOD2ADD(IMOV)/=0)THEN
                  NINDX1_SMS=NINDX1_SMS+1
                  INDX1_SMS(NINDX1_SMS)=IMOV
                  NODXI_SMS(IMOV)=NINDX1_SMS
                END IF
              END IF
              K  =K+NSN
            END DO
          END IF
        END IF
C-----------------------------------------------
C
        KMV=0
C
        IF(IDTMINS/=0)THEN
          IF(IPARIT==0.OR.DEBUG(9)==0)THEN
            DO I=NODFT,NODLT
C reset du passe
              DIAG_SMS(I)= ZERO
              DO IJ=JAD_SMS(I),JAD_SMS(I+1)-1
                DIAG_SMS(I)=DIAG_SMS(I)-LT_SMS(IJ)
              END DO
            END DO
          ELSE
            DO I=NODFT,NODLT
C reset du passe
              DIAG_SMS(I)= ZERO
            END DO
C
            CALL MY_BARRIER
C
            NODFT1_SMS=1+ITASK*NINDX1_SMS/NTHREAD
            NODLT1_SMS=(ITASK+1)*NINDX1_SMS/NTHREAD
C
            DO N=NODFT1_SMS,NODLT1_SMS
              I=INDX1_SMS(N)
              DO IJ=JAD_SMS(I),JAD_SMS(I+1)-1
                KMV=KMV+1
                IMV(KMV)=I
                MV(KMV)=-LT_SMS(IJ)
              END DO
            END DO
          END IF
        ELSE
C
C      /DT/INTER/AMS
          DO I=NODFT,NODLT
C reset du passe
            DIAG_SMS(I)= ZERO
          END DO
        END IF
C-----------------------------------------------
        CALL MY_BARRIER ! barriere avt NODFT2_SMS,NODLT2_SMS
C-----------------------------------------------
        NODFT2_SMS=1+ITASK*NINDX2_SMS/NTHREAD
        NODLT2_SMS=(ITASK+1)*NINDX2_SMS/NTHREAD
C
        IF(IPARIT==0)THEN
C
          DO N=NODFT2_SMS,NODLT2_SMS
            I=INDX2_SMS(N)
            DO IJ=JADI_SMS(I),JADI_SMS(I+1)-1
              DIAG_SMS(I)=DIAG_SMS(I)-LTI_SMS(IJ)
            END DO
          END DO
C
          IF(NSPMD > 1)THEN
C
            CALL MY_BARRIER()
C
            IF(ITASK==0) THEN   ! comm sur 1er thread

              LOC_PROC = ISPMD+1
              M = 1
              DO K=1,FR_SMS(LOC_PROC)-1
                I=LIST_SMS(M)
                M = M + 1
                IF(I==0)CYCLE
                DIAG_SMS(I)=DIAG_SMS(I)+MSKYI_SMS(K)
              END DO

              DO K=FR_SMS(LOC_PROC+1),FR_SMS(NSPMD+1)-1
                I=LIST_SMS(M)
                M = M + 1
                IF(I==0)CYCLE
                DIAG_SMS(I)=DIAG_SMS(I)+MSKYI_SMS(K)
              END DO

              CALL SPMD_MIJ_SMS(
     1        ISKYI_SMS,FR_SMS,FR_RMS,LIST_RMS,MSKYI_SMS,
     2        MSKYI_FI_SMS)

              DO K=1,FR_RMS(NSPMD+1)-1
                I=LIST_RMS(K)
                IF(I==0)CYCLE
                DIAG_SMS(I)=DIAG_SMS(I)+MSKYI_FI_SMS(K)
              END DO

            END IF
C
            CALL MY_BARRIER
C
            IF(ITASK==0) THEN   ! comm sur 1er thread
              SIZE = 1
              LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
C
C Echange DIAG_SMS
C
              CALL SPMD_EXCH_SMS(
     .             DIAG_SMS,NODXI_SMS,IAD_ELEM ,FR_ELEM,SIZE,
     .             LENR)
            END IF
          END IF
C
        ELSEIF(DEBUG(9)==0)THEN
C---------------------------------------------------------------------
C Parith/ON is ensured when changing n  of threads, not n  of domains
C---------------------------------------------------------------------
          DO N=NODFT2_SMS,NODLT2_SMS
            I=INDX2_SMS(N)
            DO IJ=JADI_SMS(I),JADI_SMS(I+1)-1
              KMV=KMV+1
              IMV(KMV)=I
              MV(KMV)=-LTI_SMS(IJ)
            END DO
          END DO
C
          IF(NSPMD > 1)THEN
            LOC_PROC = ISPMD+1
            M = 1
            DO K=1,FR_SMS(LOC_PROC)-1
              I=LIST_SMS(M)
              M = M + 1
              IF(I == 0 .OR. NODII_SMS(I) < NODFT2_SMS .OR.
     .                       NODLT2_SMS < NODII_SMS(I))CYCLE
              KMV=KMV+1
              IMV(KMV)=I
              MV(KMV)=MSKYI_SMS(K)
            END DO

            DO K=FR_SMS(LOC_PROC+1),FR_SMS(NSPMD+1)-1
              I=LIST_SMS(M)
              M = M + 1
              IF(I == 0 .OR. NODII_SMS(I) < NODFT2_SMS .OR.
     .                       NODLT2_SMS < NODII_SMS(I))CYCLE
              KMV=KMV+1
              IMV(KMV)=I
              MV(KMV)=MSKYI_SMS(K)
            END DO

            IF(ITASK==0) THEN   ! comm sur 1er thread
              CALL SPMD_MIJ_SMS(
     1        ISKYI_SMS,FR_SMS,FR_RMS,LIST_RMS,MSKYI_SMS,
     2        MSKYI_FI_SMS)
            END IF
C
            CALL MY_BARRIER()
C
            DO K=1,FR_RMS(NSPMD+1)-1
              I=LIST_RMS(K)
              IF(I == 0 .OR. NODII_SMS(I) < NODFT2_SMS .OR.
     .                       NODLT2_SMS < NODII_SMS(I))CYCLE
              KMV=KMV+1
              IMV(KMV)=I
              MV(KMV)=MSKYI_FI_SMS(K)
            END DO
C
          END IF
C
          DO N=NODFT2_SMS,NODLT2_SMS
            I=INDX2_SMS(N)
            DO J=1,6
              W6(J,I)=ZERO
            END DO
          END DO
C
          CALL FOAT_TO_6_FLOAT(1,KMV,MV,MV6)
C
          DO K=1,KMV
            I=IMV(K)
            DO J=1,6
              W6(J,I) = W6(J,I)+MV6(J,K)
            END DO
          END DO
C
          CALL MY_BARRIER()
C
          DO N=NODFT2_SMS,NODLT2_SMS
            I=INDX2_SMS(N)
            DIAG_SMS(I) = DIAG_SMS(I)
     .                   +W6(1,I)+W6(2,I)+W6(3,I)
     .      +W6(4,I)+W6(5,I)+W6(6,I)
          END DO
C
          IF(NSPMD > 1) THEN
C
            CALL MY_BARRIER()
C
            IF(ITASK==0) THEN   ! comm sur 1er thread
              SIZE = 1
              LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
C
C Echange DIAG_SMS
C
              CALL SPMD_EXCH_SMS(
     .             DIAG_SMS,NODXI_SMS,IAD_ELEM ,FR_ELEM,SIZE,
     .             LENR)
            END IF
C
          END IF
C
        ELSE ! IF(IPARIT==1.AND.DEBUG(9)==1)
C---------------------------------------------------------------------
C Parith/ON is ensured when changing n  of threads and/or n  of domains
C---------------------------------------------------------------------
C
          CALL MY_BARRIER()
C
          NODFT1_SMS=1+ITASK*NINDX1_SMS/NTHREAD
          NODLT1_SMS=(ITASK+1)*NINDX1_SMS/NTHREAD
C
          DO N=NODFT1_SMS,NODLT1_SMS
            I=INDX1_SMS(N)
            DO IJ=JADI_SMS(I),JADI_SMS(I+1)-1
              KMV=KMV+1
              IMV(KMV)=I
              MV(KMV)=-LTI_SMS(IJ)
            END DO
          END DO
C
          IF(NSPMD > 1)THEN
            LOC_PROC = ISPMD+1
            M = 1
            DO K=1,FR_SMS(LOC_PROC)-1
              I=LIST_SMS(M)
              M = M + 1
              IF(I == 0 .OR. NODXI_SMS(I) < NODFT1_SMS .OR.
     .                       NODLT1_SMS < NODXI_SMS(I))CYCLE
              KMV=KMV+1
              IMV(KMV)=I
              MV(KMV)=MSKYI_SMS(K)
            END DO

            DO K=FR_SMS(LOC_PROC+1),FR_SMS(NSPMD+1)-1
              I=LIST_SMS(M)
              M = M + 1
              IF(I == 0 .OR. NODXI_SMS(I) < NODFT1_SMS .OR.
     .                       NODLT1_SMS < NODXI_SMS(I))CYCLE
              KMV=KMV+1
              IMV(KMV)=I
              MV(KMV)=MSKYI_SMS(K)
            END DO

            IF(ITASK==0) THEN   ! comm sur 1er thread
              CALL SPMD_MIJ_SMS(
     1        ISKYI_SMS,FR_SMS,FR_RMS,LIST_RMS,MSKYI_SMS,
     2        MSKYI_FI_SMS)
            END IF
C
            CALL MY_BARRIER()
C
            DO K=1,FR_RMS(NSPMD+1)-1
              I=LIST_RMS(K)
              IF(I == 0 .OR. NODXI_SMS(I) < NODFT1_SMS .OR.
     .                       NODLT1_SMS < NODXI_SMS(I))CYCLE
              KMV=KMV+1
              IMV(KMV)=I
              MV(KMV)=MSKYI_FI_SMS(K)
            END DO
C
          END IF
C
          DO N=NODFT1_SMS,NODLT1_SMS
            I=INDX1_SMS(N)
            DO J=1,6
              W6(J,I)=ZERO
            END DO
          END DO
C
          CALL FOAT_TO_6_FLOAT(1,KMV,MV,MV6)
C
          DO K=1,KMV
            I=IMV(K)
            DO J=1,6
              W6(J,I) = W6(J,I)+MV6(J,K)
            END DO
          END DO
C
          IF(NSPMD > 1) THEN
C
            CALL MY_BARRIER()
C
            IF(ITASK==0) THEN   ! comm sur 1er thread
              SIZE = 1
              LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
C
C Echange DIAG_SMS
C
              CALL SPMD_EXCH_SMS6(
     .             W6,NODXI_SMS,IAD_ELEM ,FR_ELEM,SIZE,
     .             LENR)
            END IF
C
          END IF
C
          CALL MY_BARRIER()
C
          DO N=NODFT1_SMS,NODLT1_SMS
            I=INDX1_SMS(N)
            DIAG_SMS(I) = W6(1,I)+W6(2,I)+W6(3,I)
     .      +W6(4,I)+W6(5,I)+W6(6,I)
          END DO
C
        END IF
C-----------------------------------------------
C
        CALL MY_BARRIER
C
        DO N=NODFT,NODLT
          IF(TAGSLV_RBY_SMS(N)==0) DIAG_SMS(N) = MS(N)+DIAG_SMS(N)
        END DO
C
        CALL MY_BARRIER
C
        RETURN
      END
