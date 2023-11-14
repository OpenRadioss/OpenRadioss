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
Chd|  SMS_RBE3_NODXI                source/ams/sms_rbe3.F         
Chd|-- called by -----------
Chd|        SMS_BUILD_DIAG                source/ams/sms_build_diag.F   
Chd|-- calls ---------------
Chd|        PRERBE3                       source/constraints/general/rbe3/rbe3f.F
Chd|        SPMD_EXCH_RBE3_NODNX          source/mpi/ams/spmd_sms.F     
Chd|        SPMD_MAX_II                   source/mpi/implicit/imp_spmd.F
Chd|====================================================================
      SUBROUTINE SMS_RBE3_NODXI(
     1   IRBE3 ,LRBE3 ,NODXI_SMS,IAD_M ,FR_M   )
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE3(NRBE3L,*),LRBE3(*),NODXI_SMS(*), IAD_M(*), FR_M(*)   
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, I, M, JT(3,NRBE3), JR(3,NRBE3), IAD, NS, NML, FIN, 
     .        FINFIN, ICOM, IROTG, MAX_M
C-----------------------------------------------
      CALL PRERBE3(IRBE3 ,MAX_M , IROTG,JT  ,JR   )
      ICOM = IAD_M(NSPMD+1)-IAD_M(1)
c      IF (NSPMD>1)CALL SPMD_MAX_II(IROTG,IAD_M,ICOM)
C----- 
C     when a secnd node belongs to a domain, all main nodes also belong to the domain !!!
C----- 
      FINFIN=0
      DO WHILE(FINFIN==0)
        FINFIN=1
C
C going up
        FIN=0
        DO WHILE(FIN==0)
         FIN=1
         DO N=1,NRBE3
          IAD = IRBE3(1,N)
          NS  = IRBE3(3,N)
          IF (NS==0) CYCLE
          NML = IRBE3(5,N)
          IF(JT(1,N)+JT(2,N)+JT(3,N)/=0)THEN
           DO I=1,NML
            M = LRBE3(IAD+I)
            IF(NODXI_SMS(NS)/=0.AND.NODXI_SMS(M)==0) THEN
              NODXI_SMS(M)=1
              FIN=0
            END IF
           ENDDO
          END IF
         END DO
        END DO
C
        IF (ICOM>0) THEN
          CALL SPMD_EXCH_RBE3_NODNX(
     1      NODXI_SMS,FR_M   ,IAD_M ,IAD_M(NSPMD+1) )
        END IF
C
C going down
        FIN=0
        DO WHILE(FIN==0)
         FIN=1
         DO N=1,NRBE3
          IAD = IRBE3(1,N)
          NS  = IRBE3(3,N)
          IF (NS==0) CYCLE
          NML = IRBE3(5,N)
          IF(JT(1,N)+JT(2,N)+JT(3,N)/=0)THEN
           DO I=1,NML
            M = LRBE3(IAD+I)
            IF(NODXI_SMS(M)/=0.AND.NODXI_SMS(NS)==0) THEN
              NODXI_SMS(NS)=1
              FIN=0
C
C             a climb-up is still needed
              FINFIN=0
              EXIT
            END IF
           ENDDO
          END IF
         END DO
        END DO
C
        IF (NSPMD>1)CALL SPMD_MAX_II(FINFIN,IAD_M,ICOM)
      END DO ! DO WHILE(FINFIN==0)
C
      RETURN
      END

Chd|====================================================================
Chd|  SMS_RBE3T1                    source/ams/sms_rbe3.F         
Chd|-- called by -----------
Chd|        SMS_ENCIN_2                   source/ams/sms_encin_2.F      
Chd|        SMS_MASS_SCALE_2              source/ams/sms_mass_scale_2.F 
Chd|        SMS_PCG                       source/ams/sms_pcg.F          
Chd|-- calls ---------------
Chd|        FOAT_TO_6_FLOAT               source/system/parit.F         
Chd|        PRERBE3                       source/constraints/general/rbe3/rbe3f.F
Chd|        PRERBE3P                      source/constraints/general/rbe3/rbe3f.F
Chd|        SMS_RBE3_1                    source/ams/sms_rbe3.F         
Chd|        SMS_RBE3_2                    source/ams/sms_rbe3.F         
Chd|        SPMD_EXCH_RBE3_A_PON          source/mpi/kinematic_conditions/spmd_exch_rbe3_a_pon.F
Chd|        ZERO1                         source/system/zero.F          
Chd|====================================================================
      SUBROUTINE SMS_RBE3T1(
     1                  IRBE3 ,LRBE3  ,X     ,A      ,FRBE3  ,
     2                  SKEW  ,WEIGHT ,IAD_M ,FR_M   ,FR_MPON,
     3                  RSUM  ,RSUM_PON ,R3SIZE)
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
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE3(NRBE3L,*), LRBE3(*), WEIGHT(*), IAD_M(*), FR_M(*),
     .        FR_MPON(*),R3SIZE
      my_real
     .   X(3,*), A(3,*), FRBE3(*), SKEW(*), RSUM(*)
      DOUBLE PRECISION
     .   RSUM_PON(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, MAX_M,IROTG,JT(3,NRBE3),JR(3,NRBE3),IERR,NMT,
     .        IADA,IADMS,IADFN,IADAR,IADIN,IADFR,IADM0,IADI0,IADL,
     .        IPA,IPMS,IPFN,IPAR,IPIN,IPFR,NMP,IADLP,NS,NML,ICOM,
     .        IADLP1,IADM1,IADI1,NMT0,IADMP(SLRBE3/2),IML(SLRBE3/2),
     .        ISIZE
C     REAL
C------------allacation will be removed to ini_ uniforming smp spmd in v11
C      my_real
C     .      , DIMENSION(:), ALLOCATABLE :: RSUM
C      DOUBLE PRECISION
C     .      , DIMENSION(:), ALLOCATABLE :: RSUM_PON
C======================================================================|
      NMT0 = SLRBE3/2
      CALL PRERBE3(IRBE3 ,MAX_M , IROTG,JT  ,JR   )
      ICOM = IAD_M(NSPMD+1)-IAD_M(1)
c      IF (NSPMD>1)CALL SPMD_MAX_II(IROTG,IAD_M,ICOM)
      IF (R3SIZE>5)IROTG = 1
C
      IF (NMT0>0) THEN
       CALL PRERBE3P(IRBE3 ,LRBE3 ,IADMP ,IML   , NMT   )
       IADA=1
       IADMS=IADA+3*NMT
       IADFN=IADMS+NMT
       IF (IROTG>0) THEN
        IADAR=IADFN+NMT
        IADIN=IADAR+3*NMT
        IADFR=IADIN+NMT
       ELSE
        IADAR=IADFN
        IADIN=IADAR
        IADFR=IADIN
       ENDIF
       IADL=IADFR+NMT
C
C      ALLOCATE(RSUM(IADL),STAT=IERR)
       CALL ZERO1(RSUM,IADL)
       CALL SMS_RBE3_1(
     1           IRBE3     ,LRBE3 ,X     ,A     ,FRBE3 ,
     2           SKEW      ,WEIGHT,JT    ,IROTG ,MAX_M ,
     3           RSUM(IADA),NMT0  ,IADMP )
C
       NMP = 6*NMT
       IPA=1
       IPMS=IPA+3*NMP
       IPFN=IPMS+NMP
       IF (IROTG>0) THEN
        IPAR=IPFN+NMP
        IPIN=IPAR+3*NMP
        IPFR=IPIN+NMP
       ELSE
        IPAR=IPFN
        IPIN=IPAR
        IPFR=IPIN
       ENDIF
       IADLP=IPFR+NMP
C      version spmd p/on
C      ALLOCATE(RSUM_PON(IADLP),STAT=IERR)
C      RSUM_PON=ZERO
       CALL FOAT_TO_6_FLOAT(1  ,NMT*3  ,RSUM(IADA) ,RSUM_PON(IPA) )
       IF (ICOM>0) THEN
           ISIZE=3
           CALL SPMD_EXCH_RBE3_A_PON(
     .      RSUM_PON(IPA),FR_MPON,IAD_M  ,IAD_M(NSPMD+1),ISIZE)
       ENDIF
C
C Routine assemblage parith/ON
C
       CALL SMS_RBE3_2(IRBE3 ,LRBE3 ,A     ,WEIGHT,RSUM_PON(IPA),
     2                 NMT   ,IML   ,JT    )
C      DEALLOCATE(RSUM_PON)
C
C      DEALLOCATE(RSUM)
      END IF ! IF (NMT>0)
C---
      RETURN
      END

Chd|====================================================================
Chd|  SMS_RBE3T2                    source/ams/sms_rbe3.F         
Chd|-- called by -----------
Chd|        SMS_PCG                       source/ams/sms_pcg.F          
Chd|-- calls ---------------
Chd|        PRERBE3                       source/constraints/general/rbe3/rbe3f.F
Chd|        PRERBE3P                      source/constraints/general/rbe3/rbe3f.F
Chd|        RBE3CL                        source/constraints/general/rbe3/rbe3f.F
Chd|====================================================================
      SUBROUTINE SMS_RBE3T2(IRBE3 ,LRBE3 ,X    ,A     ,FRBE3 ,
     2                      SKEW  ,R     ,PREC_SMS3)
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
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE3(NRBE3L,*),LRBE3(*)
C     REAL
      my_real
     .   X(3,*), A(3,*), FRBE3(*), SKEW(*), R(3,*), PREC_SMS3(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, M, NS ,NML, IAD,JJ,IROT,IADS,MAX_M,IROTG,
     .        JT(3,NRBE3),JR(3,NRBE3),NM,NN,K,
     .        NMT,NMT0,IADMP(SLRBE3/2),IML(SLRBE3/2)
C     REAL
      my_real 
     .        AS(3)
      my_real, 
     .         DIMENSION(:,:,:),ALLOCATABLE :: FDSTNB ,MDSTNB
C======================================================================|
      IADS = SLRBE3/2
      CALL PRERBE3(IRBE3 ,MAX_M , IROTG,JT  ,JR   )
      ALLOCATE(FDSTNB(3,6,MAX_M))
      IF (IROTG>0) ALLOCATE(MDSTNB(3,6,MAX_M))
C
      NMT0 = SLRBE3/2
      IF (NMT0>0) THEN
       CALL PRERBE3P(IRBE3 ,LRBE3 ,IADMP ,IML   , NMT   )
       DO I=1,NMT
         M = IML(I)
         A(1,M)=R(1,M)*PREC_SMS3(1,M)
         A(2,M)=R(2,M)*PREC_SMS3(2,M)
         A(3,M)=R(3,M)*PREC_SMS3(3,M)
       END DO
      END IF
C
      DO N=1,NRBE3
        IAD = IRBE3(1,N)
        NS  = IRBE3(3,N)
        IF (NS==0) CYCLE
        NML = IRBE3(5,N)
          IROT =MIN(IRBE3(6,N),IRODDL)
        CALL RBE3CL(LRBE3(IAD+1),LRBE3(IADS+IAD+1),NS     ,X      ,
     .              FRBE3(6*IAD+1),SKEW    ,NML     ,IROT   ,FDSTNB ,
     .              MDSTNB  ,IRBE3(2,N))
        DO J = 1,3
           AS(J) = ZERO
          ENDDO
        DO I=1,NML
         M = LRBE3(IAD+I)
         DO J = 1,3
          DO K = 1,3
             AS(J) = AS(J)+FDSTNB(K,J,I)*A(K,M)
            ENDDO
           ENDDO
        ENDDO
        DO J = 1,3
           A(J,NS) = AS(J) *JT(J,N)
          ENDDO
      ENDDO
C      
      DEALLOCATE(FDSTNB)
      IF (IROTG>0) DEALLOCATE(MDSTNB)
C---
      RETURN
      END


Chd|====================================================================
Chd|  SMS_RBE3_PREC                 source/ams/sms_rbe3.F         
Chd|-- called by -----------
Chd|        SMS_PCG                       source/ams/sms_pcg.F          
Chd|-- calls ---------------
Chd|        FOAT_TO_6_FLOAT               source/system/parit.F         
Chd|        PRERBE3                       source/constraints/general/rbe3/rbe3f.F
Chd|        PRERBE3P                      source/constraints/general/rbe3/rbe3f.F
Chd|        SMS_RBE3_1                    source/ams/sms_rbe3.F         
Chd|        SMS_RBE3_3                    source/ams/sms_rbe3.F         
Chd|        SPMD_EXCH_RBE3_A_PON          source/mpi/kinematic_conditions/spmd_exch_rbe3_a_pon.F
Chd|        ZERO1                         source/system/zero.F          
Chd|====================================================================
      SUBROUTINE SMS_RBE3_PREC(
     1                  IRBE3 ,LRBE3  ,X     ,DIAG_SMS,DIAG_SMS3,
     2                  FRBE3 ,SKEW  ,WEIGHT ,IAD_M   ,FR_M     ,
     3                  FR_MPON,RSUM ,RSUM_PON ,R3SIZE)
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
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE3(NRBE3L,*), LRBE3(*), WEIGHT(*), IAD_M(*), FR_M(*),
     .        FR_MPON(*),R3SIZE
C     REAL
      my_real
     .   X(3,*), DIAG_SMS(*), DIAG_SMS3(*), FRBE3(*), SKEW(*), RSUM(*)
      DOUBLE PRECISION
     .   RSUM_PON(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, MAX_M,IROTG,JT(3,NRBE3),JR(3,NRBE3),IERR,NMT,
     .        IADA,IADMS,IADFN,IADAR,IADIN,IADFR,IADM0,IADI0,IADL,
     .        IPA,IPMS,IPFN,IPAR,IPIN,IPFR,NMP,IADLP,NS,NML,ICOM,
     .        IADLP1,IADM1,IADI1,NMT0,IADMP(SLRBE3/2),IML(SLRBE3/2),
     .        ISIZE
C     REAL
C------------allacation will be removed to ini_ uniforming smp spmd in v11
C      my_real
C     .      , DIMENSION(:), ALLOCATABLE :: RSUM
C      DOUBLE PRECISION
C     .      , DIMENSION(:), ALLOCATABLE :: RSUM_PON
C======================================================================|
      NMT0 = SLRBE3/2
      CALL PRERBE3(IRBE3 ,MAX_M , IROTG,JT  ,JR   )
      ICOM = IAD_M(NSPMD+1)-IAD_M(1)
c      IF (NSPMD>1)CALL SPMD_MAX_II(IROTG,IAD_M,ICOM)
       IF (R3SIZE>5)IROTG = 1
C
      IF (NMT0>0) THEN
       CALL PRERBE3P(IRBE3 ,LRBE3 ,IADMP ,IML   , NMT   )
       IADA=1
       IADMS=IADA+3*NMT
       IADFN=IADMS+NMT
       IF (IROTG>0) THEN
        IADAR=IADFN+NMT
        IADIN=IADAR+3*NMT
        IADFR=IADIN+NMT
       ELSE
        IADAR=IADFN
        IADIN=IADAR
        IADFR=IADIN
       ENDIF
       IADL=IADFR+NMT
C
C      ALLOCATE(RSUM(IADL),STAT=IERR)
       CALL ZERO1(RSUM,IADL)
       CALL SMS_RBE3_1(
     1           IRBE3     ,LRBE3 ,X     ,DIAG_SMS3,FRBE3 ,
     2           SKEW      ,WEIGHT,JT    ,IROTG    ,MAX_M ,
     3           RSUM(IADA),NMT0  ,IADMP )
C
       NMP = 6*NMT
       IPA=1
       IPMS=IPA+3*NMP
       IPFN=IPMS+NMP
       IF (IROTG>0) THEN
        IPAR=IPFN+NMP
        IPIN=IPAR+3*NMP
        IPFR=IPIN+NMP
       ELSE
        IPAR=IPFN
        IPIN=IPAR
        IPFR=IPIN
       ENDIF
       IADLP=IPFR+NMP
C      version spmd p/on
C      ALLOCATE(RSUM_PON(IADLP),STAT=IERR)
C      RSUM_PON=ZERO
       CALL FOAT_TO_6_FLOAT(1  ,NMT*3  ,RSUM(IADA) ,RSUM_PON(IPA) )
       IF (ICOM>0) THEN
           ISIZE=3
           CALL SPMD_EXCH_RBE3_A_PON(
     .      RSUM_PON(IPA),FR_MPON,IAD_M  ,IAD_M(NSPMD+1),ISIZE)
       ENDIF
C
C Routine assemblage parith/ON
C
       CALL SMS_RBE3_3(IRBE3 ,LRBE3 ,DIAG_SMS3,WEIGHT,RSUM_PON(IPA),
     2                 NMT   ,IML   ,JT    )
C      DEALLOCATE(RSUM_PON)
C
C      DEALLOCATE(RSUM)
      END IF ! IF (NMT>0)
C---
      RETURN
      END

Chd|====================================================================
Chd|  SMS_RBE3_1                    source/ams/sms_rbe3.F         
Chd|-- called by -----------
Chd|        SMS_RBE3T1                    source/ams/sms_rbe3.F         
Chd|        SMS_RBE3_PREC                 source/ams/sms_rbe3.F         
Chd|-- calls ---------------
Chd|        MFAC_RBE3                     source/constraints/general/rbe3/rbe3f.F
Chd|        RBE3CL                        source/constraints/general/rbe3/rbe3f.F
Chd|====================================================================
      SUBROUTINE SMS_RBE3_1(
     1                 IRBE3 ,LRBE3 ,X     ,A     ,FRBE3 ,
     2                 SKEW  ,WEIGHT,JT    ,IROTG ,MAX_M ,
     3                 AM    ,NMT0  ,IADMP )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE3(NRBE3L,*),LRBE3(*),WEIGHT(*)
      INTEGER MAX_M,IROTG,JT(3,*),NMT0,IADMP(*)
C     REAL
      my_real
     .   X(3,*), A(3,*), FRBE3(*),SKEW(*), AM(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, NS ,NML, IAD, IROT, IADS, NN, K
C     REAL
      my_real
     .     FNS(3), SFD, SMD
C     REAL
      my_real,
     .         DIMENSION(:,:,:),ALLOCATABLE :: FDSTNB ,MDSTNB
C-----------------------------------------------
      IADS = NMT0
      ALLOCATE(FDSTNB(3,6,MAX_M))
      IF (IROTG>0) ALLOCATE(MDSTNB(3,6,MAX_M))
C---
      DO N=1,NRBE3
        IAD = IRBE3(1,N)
        NS  = IRBE3(3,N)
        NML = IRBE3(5,N)
          IROT =IRBE3(6,N)
        IF (NS>0) THEN
         CALL RBE3CL(LRBE3(IAD+1),LRBE3(IADS+IAD+1),NS     ,X      ,
     .               FRBE3(6*IAD+1),SKEW    ,NML     ,IROT   ,FDSTNB ,
     .               MDSTNB  ,IRBE3(2,N))

        DO J = 1,3
            NN = JT(J,N)*WEIGHT(NS)
            FNS(J) = A(J,NS)*NN
          ENDDO
C---not to add supplementary mass globally
        CALL MFAC_RBE3(FDSTNB,MDSTNB,NML  ,IROTG,SFD ,SMD)
        DO I=1,NML
         K = IADMP(IAD+I)
         DO J = 1,3
             AM(1,K) = AM(1,K)+FDSTNB(1,J,I)*FNS(J)
             AM(2,K) = AM(2,K)+FDSTNB(2,J,I)*FNS(J)
             AM(3,K) = AM(3,K)+FDSTNB(3,J,I)*FNS(J)
           ENDDO
        ENDDO
C---
       END IF ! IF (NS>0) THEN
      ENDDO
C
      DEALLOCATE(FDSTNB)
      IF (IROTG>0) DEALLOCATE(MDSTNB)
C
      RETURN
      END

Chd|====================================================================
Chd|  SMS_RBE3_2                    source/ams/sms_rbe3.F         
Chd|-- called by -----------
Chd|        SMS_RBE3T1                    source/ams/sms_rbe3.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SMS_RBE3_2(IRBE3 ,LRBE3 ,A     ,WEIGHT,DA    ,
     2                      NMT   ,IML   ,JT    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE3(NRBE3L,*),LRBE3(*),WEIGHT(*),IML(*) ,NMT,
     .        JT(3,NRBE3)
C     REAL
      my_real
     .   A(3,*)
      DOUBLE PRECISION
     .   DA(6,3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, M, N, NS
C     REAL
      my_real
     .   AX,AY,AZ
C======================================================================|
#include    "vectorize.inc"
      DO I=1,NMT
        M = IML(I)
        AX = ZERO
        AY = ZERO
        AZ = ZERO
        DO J=1,6
         AX = AX + DA(J,1,I)
         AY = AY + DA(J,2,I)
         AZ = AZ + DA(J,3,I)
        END DO
        A(1,M) = A(1,M)+ AX
        A(2,M) = A(2,M)+ AY
        A(3,M) = A(3,M)+ AZ
      END DO
C---
      DO N=1,NRBE3
        NS  = IRBE3(3,N)
        IF(NS/=0)THEN
C         Reset residu for secnd node
          DO J = 1,3
            IF(JT(J,N)/=0)A(J,NS)=ZERO
          END DO
        END IF
      END DO
C---
      RETURN
      END

Chd|====================================================================
Chd|  SMS_RBE3_3                    source/ams/sms_rbe3.F         
Chd|-- called by -----------
Chd|        SMS_RBE3_PREC                 source/ams/sms_rbe3.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SMS_RBE3_3(IRBE3 ,LRBE3 ,DIAG_SMS3,WEIGHT,DA    ,
     2                      NMT   ,IML   ,JT    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE3(NRBE3L,*),LRBE3(*),WEIGHT(*),IML(*) ,NMT, JT(3,NRBE3)
      my_real DIAG_SMS3(3,*)
      DOUBLE PRECISION DA(6,3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, M, N, NS
C     REAL
      my_real
     .   DD
C======================================================================|
#include    "vectorize.inc"
      DO I=1,NMT
        M = IML(I)
        DO J=1,3
          DD=DIAG_SMS3(J,M)
          DO K=1,6
           DD = DD + DA(K,J,I)
          END DO
          DIAG_SMS3(J,M) = DD
        END DO
      END DO
C---
      RETURN
      END
