Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    sensor_spmd       ../engine/source/tools/sensor/sensor_spmd.F
!||--- called by ------------------------------------------------------
!||    resol             ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    spmd_glob_dsum9   ../engine/source/mpi/interfaces/spmd_th.F
!||    spmd_glob_isum9   ../engine/source/mpi/interfaces/spmd_th.F
!||    spmd_ibcast       ../engine/source/mpi/generic/spmd_ibcast.F
!||    spmd_rbcast       ../engine/source/mpi/generic/spmd_rbcast.F
!||    spmd_sd_acc       ../engine/source/mpi/output/spmd_sd_acc.F
!||    spmd_sd_sens      ../engine/source/mpi/output/spmd_sd_sens.F
!||--- uses       -----------------------------------------------------
!||    groupdef_mod      ../common_source/modules/groupdef_mod.F
!||    sensor_mod        ../common_source/modules/sensor_mod.F90
!||    spmd_mod          ../engine/source/mpi/spmd_mod.F90
!||====================================================================
      SUBROUTINE SENSOR_SPMD(SENSOR_TAB,IPARI  ,NPRW   ,ISENSP  ,NSENSP    ,
     .                   XSENS     ,X      ,ACCELM ,IACCP   ,NACCP     ,
     .                   GAUGE     ,IGAUP  ,NGAUP  ,PARTSAV2,NSENSOR,
     .                   COMM_SENS14,SENSOR_STRUCT   )
C-----------------------------------------------
C   SPMD sensor update routine
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SENSOR_MOD
      USE GROUPDEF_MOD
      USE SPMD_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NSENSOR
      INTEGER IPARI(NPARI,NINTER),
     .        NPRW(*), ISENSP(2,*), NSENSP(*), IACCP(*), NACCP(*),
     .        IGAUP(*), NGAUP(*)
      my_real XSENS(12,*), X(3,*),
     .        ACCELM(LLACCELM,*), GAUGE(LLGAUGE,*),PARTSAV2(2,*)
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
      type(sensor_comm),INTENT(INOUT) :: COMM_SENS14
      type(sensor_type),DIMENSION(NSENSOR),INTENT(INOUT) :: SENSOR_STRUCT(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K, TYP, LEN, IN, N5, ISENSUSR, I, LEN1,LOC_PROC,
     .        ISENST1, ISENST2, KK, N1, N2,M,
     .        IBUF(NSENSOR), ISENST10,ISENST13,J
      my_real RBUF(NSENSOR*LSENBUF), RXBUF(5,2*NSENSOR)

      INTEGER :: MY_SIZE
      CHARACTER(len=4) :: MY_OPERATION
      REAL(kind=8), DIMENSION(:), ALLOCATABLE :: SBUF_DOUBLE,RBUF_DOUBLE
      INTEGER :: I2,I3,I4,IDX
     
C=======================================================================
C
       ISENST1 = 0
       ISENST2 = 0
       ISENST10= 0
       ISENST13= 0
       LOC_PROC = ISPMD+1
       KK = 0
       LEN = 0
       ISENSUSR = 0
       DO K=1,NSENSOR
         TYP = SENSOR_TAB(K)%TYPE
         IF (TYP == 1) THEN
           ISENST1 = 1
         ELSEIF (TYP == 2) THEN
           ISENST2 = 1
           IF(ISENSP(1,K) == LOC_PROC)THEN
            N1 = SENSOR_TAB(K)%IPARAM(1)
            IF(LOC_PROC /= 1)THEN
             KK = KK+1
             RXBUF(1,KK) = K
             RXBUF(2,KK) = 1
             RXBUF(3,KK) = X(1,N1)
             RXBUF(4,KK) = X(2,N1)
             RXBUF(5,KK) = X(3,N1)
            ELSE
C stockage directe sur p0
             XSENS(1,K) = X(1,N1)
             XSENS(2,K) = X(2,N1)
             XSENS(3,K) = X(3,N1)
            END IF
           END IF
           IF(ISENSP(2,K) == LOC_PROC)THEN
            N2 = SENSOR_TAB(K)%IPARAM(2)
            IF(LOC_PROC /= 1)THEN
             KK = KK+1
             RXBUF(1,KK) = K
             RXBUF(2,KK) = 2
             RXBUF(3,KK) = X(1,N2)
             RXBUF(4,KK) = X(2,N2)
             RXBUF(5,KK) = X(3,N2)
            ELSE
C stockage directe sur p0
             XSENS(4,K) = X(1,N2)
             XSENS(5,K) = X(2,N2)
             XSENS(6,K) = X(3,N2)
            END IF
           END IF
         ELSEIF(TYP == 6)THEN
           LEN = LEN + 1
           IN = SENSOR_TAB(K)%IPARAM(1)
           IF (IN > NINTER) IN = SENSOR_TAB(K)%IPARAM(2)
           IBUF(LEN) = IPARI(29,IN)
         ELSEIF(TYP == 7)THEN
           LEN = LEN + 1
           IN = SENSOR_TAB(K)%IPARAM(1)
           N5 = IN + 4*NRWALL
           IBUF(LEN) = NPRW(N5)
         ELSEIF(TYP == 10)THEN
           ISENST10 = 1
c-------------------------------
         ELSEIF (TYP == 13) THEN     ! sensor WORK
c-------------------------------
           ISENST13 = 1
           IF (ISENSP(1,K) == LOC_PROC) THEN
             N1 = SENSOR_TAB(K)%IPARAM(1)
             IF (LOC_PROC /= 1) THEN
               KK = KK+1
               RXBUF(1,KK) = K
               RXBUF(2,KK) = 1
               RXBUF(3,KK) = X(1,N1)
               RXBUF(4,KK) = X(2,N1)
               RXBUF(5,KK) = X(3,N1)
             ELSE    ! stockage directe sur p0
               XSENS(1,K) = X(1,N1)
               XSENS(2,K) = X(2,N1)
               XSENS(3,K) = X(3,N1)
             END IF
           END IF
           IF (ISENSP(2,K) == LOC_PROC) THEN
             N2 = SENSOR_TAB(K)%IPARAM(2)
             IF (N2 > 0 ) THEN
               IF (LOC_PROC /= 1) THEN
                 KK = KK+1
                 RXBUF(1,KK) = K
                 RXBUF(2,KK) = 2
                 RXBUF(3,KK) = X(1,N2)
                 RXBUF(4,KK) = X(2,N2)
                 RXBUF(5,KK) = X(3,N2)
               ELSE                    ! stockage directe sur p0
                 XSENS(4,K) = X(1,N2)
                 XSENS(5,K) = X(2,N2)
                 XSENS(6,K) = X(3,N2)
               END IF
             ELSE
               IF (LOC_PROC /= 1) THEN
                 KK = KK+1
                 RXBUF(1,KK) = K
                 RXBUF(2,KK) = 2
                 RXBUF(3,KK) = ZERO
                 RXBUF(4,KK) = ZERO
                 RXBUF(5,KK) = ZERO
               ELSE                    ! stockage directe sur p0
                 XSENS(4,K) = ZERO
                 XSENS(5,K) = ZERO
                 XSENS(6,K) = ZERO
               END IF
             END IF
           END IF
c-------------
         ELSEIF(TYP==16)THEN
         ! -------------------
         ! sensor HIC : need to communicate ACC
            ISENST1 = 1
         ! -------------------
         ELSEIF(TYP == 29.OR.TYP == 30.OR.TYP == 31)THEN
c-------------
           ISENSUSR = 1
           IF(NACCELM>0) ISENST1 = 1
         ENDIF
       ENDDO
c-----------------------------------------------------------------------
       IF (ISENST1 == 1) THEN
c        Extra communication pour sensor type 1 (accelerometre associe)
C
         CALL SPMD_SD_ACC(ACCELM,IACCP,NACCP)
         CALL SPMD_RBCAST(ACCELM,ACCELM,LLACCELM,NACCELM,0,2)
       END IF
C
       IF (ISENST10 == 1) THEN
c         Extra communication pour sensor type 1 (gauge associee)
C
         CALL SPMD_SD_ACC(GAUGE,IGAUP,NGAUP)
         CALL SPMD_RBCAST(GAUGE,GAUGE,LLGAUGE,NBGAUGE,0,2)
       END IF
C
       IF (ISENST2 == 1) THEN
c        Extra communication pour sensor type 2
C
         CALL SPMD_SD_SENS(XSENS,RXBUF,NSENSP)
         CALL SPMD_RBCAST(XSENS,XSENS,12,NSENSOR,0,2)
       END IF
c
       IF (ISENST13 == 1) THEN
c        Extra communication pour sensor type 13
C
         CALL SPMD_SD_SENS(XSENS,RXBUF,NSENSP)
         CALL SPMD_RBCAST(XSENS,XSENS,12,NSENSOR,0,2)
         IF (TT == ZERO) THEN
           DO K=1,NSENSOR
             TYP = SENSOR_TAB(K)%TYPE
             IF (TYP == 13) THEN
               XSENS(7,K)  = XSENS(1,K)
               XSENS(8,K)  = XSENS(2,K)
               XSENS(9,K)  = XSENS(3,K)
               N2  = SENSOR_TAB(K)%IPARAM(2)
               IF (N2 > 0) THEN
                 XSENS(10,K) = XSENS(4,K)
                 XSENS(11,K) = XSENS(5,K)
                 XSENS(12,K) = XSENS(6,K)
               ENDIF
             ENDIF
           ENDDO
         ENDIF
       END IF
       ! -------------------------------
       ! mpi communication for energy sensor (type 14)
       IF(COMM_SENS14%BOOL) THEN
            IF(IPARIT>0) THEN 
              MY_SIZE = COMM_SENS14%NUM_SENS*2*6*NTHREAD
              ALLOCATE( SBUF_DOUBLE(MY_SIZE) )
              ALLOCATE( RBUF_DOUBLE(MY_SIZE) )
              DO K=1,COMM_SENS14%NUM_SENS
                J = COMM_SENS14%ID_SENS(K)
                DO I2=1,2
                  DO I3=1,6
                    DO I4=1,NTHREAD
                      IDX = ((K-1)*2*6*NTHREAD) + ((I2-1)*6*NTHREAD) + ((I3-1)*NTHREAD) + I4
                      SBUF_DOUBLE(IDX) = SENSOR_STRUCT(J)%FBSAV6_SENS(I2,I3,I4)
                    END DO
                  END DO
                END DO
              ENDDO

              CALL SPMD_ALLREDUCE(SBUF_DOUBLE,RBUF_DOUBLE,MY_SIZE,SPMD_SUM)

              DO K=1,COMM_SENS14%NUM_SENS
                J = COMM_SENS14%ID_SENS(K)
                DO I2=1,2
                  DO I3=1,6
                    DO I4=1,NTHREAD
                      IDX = ((K-1)*2*6*NTHREAD) + ((I2-1)*6*NTHREAD) + ((I3-1)*NTHREAD) + I4
                      SENSOR_STRUCT(J)%FBSAV6_SENS(I2,I3,I4) = RBUF_DOUBLE(IDX)
                    END DO
                  END DO
                END DO
              ENDDO
              DEALLOCATE( SBUF_DOUBLE )
              DEALLOCATE( RBUF_DOUBLE )
            ELSE
                CALL SPMD_GLOB_DSUM9(PARTSAV2,2*NPART)
                CALL SPMD_RBCAST(PARTSAV2,PARTSAV2,2,NPART,0,2)
            ENDIF        
       ENDIF
       ! -------------------------------
c-----------------------------------------------------------------------
       IF (LEN > 0) THEN
C
        CALL SPMD_GLOB_ISUM9(IBUF,LEN)
        CALL SPMD_IBCAST(IBUF,IBUF,LEN,1,0,2)
C
C   il faut corrige la reduction et l'affecter au ipari/nprw
        LEN = 0
        DO K=1,NSENSOR
         TYP = SENSOR_TAB(K)%TYPE
         IF (TYP == 6)THEN
           LEN = LEN + 1
           IN = SENSOR_TAB(K)%IPARAM(1)
           IF (IN > NINTER) IN = SENSOR_TAB(K)%IPARAM(2)
           IPARI(29,IN)=MIN(IBUF(LEN),1)
         ELSEIF (TYP == 7)THEN
           LEN = LEN + 1
           IN = SENSOR_TAB(K)%IPARAM(1)
           N5 = IN + 4*NRWALL
           NPRW(N5) = MIN(IBUF(LEN),1)
         ENDIF
        ENDDO

       ENDIF
C------------
       RETURN
       END
