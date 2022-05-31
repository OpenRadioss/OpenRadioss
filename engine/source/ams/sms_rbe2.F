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
Chd|  SMS_DIAG_RBE2                 source/ams/sms_rbe2.F         
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        PRERBE2                       source/constraints/general/rbe2/rbe2f.F
Chd|        SMS_RBE_5                     source/ams/sms_rbe2.F         
Chd|        SPMD_EXCH_RBE2_SMS            source/mpi/kinematic_conditions/spmd_exch_rbe2_sms.F
Chd|        SPMD_MAX_I                    source/mpi/implicit/imp_spmd.F
Chd|====================================================================
      SUBROUTINE SMS_DIAG_RBE2(
     1   IRBE2 ,LRBE2 ,NODXI_SMS,JAD_SMS,JDI_SMS,LT_SMS,
     2   NMRBE2,MS,DIAG_SMS,PREC_SMS3,IAD_RBE2,FR_RBE2M,
     3   WEIGHT,SKEW)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE2(NRBE2L,*),LRBE2(*),NODXI_SMS(*),
     .        JAD_SMS(*),JDI_SMS(*),NMRBE2, IAD_RBE2(*),
     .        FR_RBE2M(*), WEIGHT(*)
C     REAL ou REAL*8
      my_real
     .        LT_SMS(*), MS(*), DIAG_SMS(*), PREC_SMS3(3,*), SKEW(LSKEW,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K, N, ISK, I, J, M, JT(3,NRBE2),JR(3,NRBE2),
     .        IAD, NS, NSN, MID, NHI, IRAD, IJ, NN, TAG(3,NUMNOD), 
     .        ICOM, ISIZE
      my_real
     .        DIAG_RBE2(3,NUMNOD), DD
      DOUBLE PRECISION
     .   FRBE2M6(3,6,NMRBE2)
C-----------------------------------------------
      CALL PRERBE2(IRBE2 ,JT  ,JR   )
      ICOM = IAD_RBE2(NSPMD+1)-IAD_RBE2(1)
      IF (NSPMD.GT.1)CALL SPMD_MAX_I(ICOM)
C
      TAG(1:3,1:NUMNOD)=0
C
      DO NHI=NHRBE2,0,-1
       DO N=1,NRBE2
        IF (IRBE2(9,N).NE.NHI) CYCLE
	IAD = IRBE2(1,N)
        NSN = IRBE2(5,N)
        M   = IRBE2(3,N)
        DO I=1,NSN
          NS=LRBE2(IAD+I)
          IF(JT(1,N)/=0)THEN
            IF(TAG(1,M)==0)THEN
              TAG(1,NS)=M
            ELSE
              TAG(1,NS)=TAG(1,M)
            END IF
          END IF
          IF(JT(2,N)/=0)THEN
            IF(TAG(2,M)==0)THEN
              TAG(2,NS)=M
            ELSE
              TAG(2,NS)=TAG(2,M)
            END IF
          END IF
          IF(JT(3,N)/=0)THEN
            IF(TAG(3,M)==0)THEN
              TAG(3,NS)=M
            ELSE
              TAG(3,NS)=TAG(3,M)
            END IF
          END IF
        END DO
       END DO
      END DO
C
C
      DO N=1,NUMNOD
       DIAG_RBE2(1,N)=DIAG_SMS(N)
       DIAG_RBE2(2,N)=DIAG_SMS(N)
       DIAG_RBE2(3,N)=DIAG_SMS(N)
      END DO
C
      DO NHI=0,NHRBE2
       DO N=1,NMRBE2
        DO K=1,6
         FRBE2M6(1,K,N) = ZERO
         FRBE2M6(2,K,N) = ZERO
         FRBE2M6(3,K,N) = ZERO
        END DO
       END DO
       DO N=1,NRBE2
        IF (IRBE2(9,N).NE.NHI) CYCLE
	IAD = IRBE2(1,N)
        NSN = IRBE2(5,N)
        M   = IRBE2(3,N)
        ISK = IRBE2(7,N)
	MID = IABS(IRBE2(6,N))
	IRAD = IRBE2(11,N)
        CALL SMS_RBE_5(NSN   ,LRBE2(IAD+1),DIAG_RBE2,MS    ,WEIGHT,
     1                 JT    ,FRBE2M6(1,1,MID),M      ,IRAD  ,ISK   ,
     2                 SKEW  )

       END DO
C-----------------
       IF (ICOM.GT.0) THEN
         ISIZE=3
         CALL SPMD_EXCH_RBE2_SMS(
     .    FRBE2M6 ,IAD_RBE2,FR_RBE2M,IAD_RBE2(NSPMD+1),ISIZE)
       ENDIF
C
C assemblage parith/ON
#include    "vectorize.inc"
       DO N=1,NRBE2
         IF (IRBE2(9,N).NE.NHI) CYCLE
         M  = IRBE2(3,N)
         MID = IRBE2(6,N)
         IRAD = IRBE2(11,N)
         IF (MID.LT.0) CYCLE
         DO J=1,3
           DD=DIAG_RBE2(J,M)
           DO K=1,6
             DD = DD + FRBE2M6(J,K,MID)
           ENDDO
           DIAG_RBE2(J,M)=DD
         END DO
       ENDDO
C
      END DO
C-----------------
C
      DO N=1,NRBE2
       IAD = IRBE2(1,N)
       NSN = IRBE2(5,N)
       M   = IRBE2(3,N)
       ISK = IRBE2(7,N)
       MID = IABS(IRBE2(6,N))
       IRAD = IRBE2(11,N)
       IF(JT(1,N)+JT(2,N)+JT(3,N)/=0.AND.NODXI_SMS(M)==0)THEN
         DO I=1,NSN
          NS = LRBE2(IAD+I)
          DO IJ=JAD_SMS(NS),JAD_SMS(NS+1)-1
            NN=JDI_SMS(IJ)
            IF(TAG(1,NN)==TAG(1,NS))
     .        DIAG_RBE2(1,M)=MAX(MS(M),DIAG_RBE2(1,M)+LT_SMS(IJ))
            IF(TAG(2,NN)==TAG(2,NS))
     .        DIAG_RBE2(2,M)=MAX(MS(M),DIAG_RBE2(2,M)+LT_SMS(IJ))
            IF(TAG(3,NN)==TAG(3,NS))
     .        DIAG_RBE2(3,M)=MAX(MS(M),DIAG_RBE2(3,M)+LT_SMS(IJ))
          END DO
         ENDDO
       END IF
      END DO
C
      DO N=1,NRBE2
        M  = IRBE2(3,N)
        MID = IRBE2(6,N)
        IRAD = IRBE2(11,N)
        IF (MID.LT.0) CYCLE
        DO J=1,3
          IF(DIAG_RBE2(J,M)==ZERO)THEN
            PREC_SMS3(J,M)=ZERO
          ELSE
            PREC_SMS3(J,M)=ONE/DIAG_RBE2(J,M)
          END IF
        END DO
      ENDDO
C
      RETURN
      END

Chd|====================================================================
Chd|  SMS_RBE2_NODXI                source/ams/sms_rbe2.F         
Chd|-- called by -----------
Chd|        SMS_BUILD_DIAG                source/ams/sms_build_diag.F   
Chd|-- calls ---------------
Chd|        PRERBE2                       source/constraints/general/rbe2/rbe2f.F
Chd|====================================================================
      SUBROUTINE SMS_RBE2_NODXI(
     1   IRBE2 ,LRBE2 ,NODXI_SMS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE2(NRBE2L,*),LRBE2(*),NODXI_SMS(*)
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K, N, ISK, I, J, M, JT(3,NRBE2),JR(3,NRBE2),
     .        IAD, NS, NSN, MID, NHI, IRAD
C-----------------------------------------------
      CALL PRERBE2(IRBE2 ,JT  ,JR   )
C
      DO NHI=0,NHRBE2
       DO N=1,NRBE2
        IF (IRBE2(9,N).NE.NHI) CYCLE
	IAD = IRBE2(1,N)
        NSN = IRBE2(5,N)
        M   = IRBE2(3,N)
        ISK = IRBE2(7,N)
	MID = IABS(IRBE2(6,N))
	IRAD = IRBE2(11,N)
        IF(JT(1,N)+JT(2,N)+JT(3,N)/=0.AND.NODXI_SMS(M)==0)THEN
          DO I=1,NSN
           NS = LRBE2(IAD+I)
           IF(NODXI_SMS(NS)/=0) THEN
             NODXI_SMS(M)=1
             EXIT
           END IF
          ENDDO
        END IF
       END DO
      END DO
C
      RETURN
      END

Chd|====================================================================
Chd|  SMS_RBE_CNDS                  source/ams/sms_rbe2.F         
Chd|-- called by -----------
Chd|        SMS_ENCIN_2                   source/ams/sms_encin_2.F      
Chd|        SMS_MASS_SCALE_2              source/ams/sms_mass_scale_2.F 
Chd|        SMS_PCG                       source/ams/sms_pcg.F          
Chd|-- calls ---------------
Chd|        PRERBE2                       source/constraints/general/rbe2/rbe2f.F
Chd|        SMS_RBE2_S                    source/ams/sms_rbe2.F         
Chd|        SMS_RBE_1                     source/ams/sms_rbe2.F         
Chd|        SMS_RBE_2                     source/ams/sms_rbe2.F         
Chd|        SPMD_EXCH_RBE2_SMS            source/mpi/kinematic_conditions/spmd_exch_rbe2_sms.F
Chd|        SPMD_MAX_I                    source/mpi/implicit/imp_spmd.F
Chd|====================================================================
      SUBROUTINE SMS_RBE_CNDS(
     1   IRBE2 ,LRBE2 ,X      ,A      ,AR     ,
     1   MS    ,IN    ,SKEW   ,WEIGHT ,IAD_RBE2,
     2   FR_RBE2M,NMRBE2)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE2(NRBE2L,*),LRBE2(*),WEIGHT(*),IAD_RBE2(*),
     .        FR_RBE2M(*) ,NMRBE2
C     REAL ou REAL*8
      my_real
     .   X(3,*), A(3,*), AR(3,*), MS(*), IN(*), 
     .   SKEW(LSKEW,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K, N, ISK, I, J, M, ISIZE, JT(3,NRBE2),JR(3,NRBE2),
     .        IAD, NS, ICOM, NSN, MID, NHI, IRAD
      DOUBLE PRECISION
     .   FRBE2M6(3,6,NMRBE2)
C-----------------------------------------------
      CALL PRERBE2(IRBE2 ,JT  ,JR   )
      ICOM = IAD_RBE2(NSPMD+1)-IAD_RBE2(1)
      IF (NSPMD.GT.1)CALL SPMD_MAX_I(ICOM)
C
      DO NHI=0,NHRBE2
       DO N=1,NMRBE2
        DO J=1,3
        DO K=1,6
         FRBE2M6(J,K,N) = ZERO
        END DO
        END DO
       END DO
       DO N=1,NRBE2
        IF (IRBE2(9,N).NE.NHI) CYCLE
	IAD = IRBE2(1,N)
        NSN = IRBE2(5,N)
        M   = IRBE2(3,N)
        ISK = IRBE2(7,N)
	MID = IABS(IRBE2(6,N))
	IRAD = IRBE2(11,N)
        CALL SMS_RBE_1(NSN   ,LRBE2(IAD+1),X       ,A     ,AR       ,
     1                 MS    ,IN    ,WEIGHT,JT(1,N),FRBE2M6(1,1,MID),
     2                 M     ,IRAD  ,ISK   ,SKEW   )
       END DO
C-----------------
       IF (ICOM.GT.0) THEN
         ISIZE=3
         CALL SPMD_EXCH_RBE2_SMS(
     .    FRBE2M6 ,IAD_RBE2,FR_RBE2M,IAD_RBE2(NSPMD+1),ISIZE)
       ENDIF
C
C Routine assemblage parith/ON
C
       ISIZE=3
       CALL SMS_RBE2_S(IRBE2  ,ISIZE,A      ,WEIGHT ,FRBE2M6,
     1                 NMRBE2 ,NHI  )
C


      END DO
C
      DO N=1,NRBE2
        IAD = IRBE2(1,N)
        M  = IRBE2(3,N)
        NSN = IRBE2(5,N)
        ISK = IRBE2(7,N)
        IRAD = IRBE2(11,N)
        CALL SMS_RBE_2(NSN   ,LRBE2(IAD+1),X     ,A     ,AR    ,
     1                 JT(1,N),M  ,IRAD   ,ISK   ,SKEW  )
      ENDDO
C
      RETURN
      END

Chd|====================================================================
Chd|  SMS_RBE_ACCL                  source/ams/sms_rbe2.F         
Chd|-- called by -----------
Chd|        SMS_PCG                       source/ams/sms_pcg.F          
Chd|-- calls ---------------
Chd|        PRERBE2                       source/constraints/general/rbe2/rbe2f.F
Chd|        SMS_RBE_3                     source/ams/sms_rbe2.F         
Chd|====================================================================
      SUBROUTINE SMS_RBE_ACCL(
     1   IRBE2 ,LRBE2  ,R        ,A       ,PREC_SMS3,
     1   SKEW  ,WEIGHT ,IAD_RBE2 ,FR_RBE2M,NMRBE2)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE2(NRBE2L,*),LRBE2(*),WEIGHT(*),IAD_RBE2(*),
     .        FR_RBE2M(*) ,NMRBE2
C     REAL ou REAL*8
      my_real
     .   R(3,*), A(3,*), PREC_SMS3(*), SKEW(LSKEW,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K, N, ISK, I, J, M, ISIZE, JT(3,NRBE2),JR(3,NRBE2),
     .        IAD, NS, ICOM, NSN, MID, NHI, IRAD
C-----------------------------------------------
      CALL PRERBE2(IRBE2 ,JT  ,JR   )
C
      DO N=NRBE2,1,-1
        IAD = IRBE2(1,N)
        M  = IRBE2(3,N)
        NSN = IRBE2(5,N)
        ISK = IRBE2(7,N)
        IRAD = IRBE2(11,N)
        CALL SMS_RBE_3(NSN   ,LRBE2(IAD+1),R     ,A     ,PREC_SMS3,
     1                 JT(1,N),M  ,IRAD   ,ISK   ,SKEW  )
      ENDDO
C
      RETURN
      END

Chd|====================================================================
Chd|  SMS_RBE_CORR                  source/ams/sms_rbe2.F         
Chd|-- called by -----------
Chd|        SMS_ENCIN_2                   source/ams/sms_encin_2.F      
Chd|        SMS_PCG                       source/ams/sms_pcg.F          
Chd|-- calls ---------------
Chd|        PRERBE2                       source/constraints/general/rbe2/rbe2f.F
Chd|        SMS_RBE_4                     source/ams/sms_rbe2.F         
Chd|====================================================================
      SUBROUTINE SMS_RBE_CORR(
     1   IRBE2 ,LRBE2 ,V      ,W      ,MS    ,
     1   SKEW   ,WEIGHT ,IAD_RBE2,FR_RBE2M,NMRBE2)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE2(NRBE2L,*),LRBE2(*),WEIGHT(*),IAD_RBE2(*),
     .        FR_RBE2M(*) ,NMRBE2
C     REAL ou REAL*8
      my_real
     .   V(3,*), W(3,*), MS(*), SKEW(LSKEW,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K, N, ISK, I, J, M, ISIZE, JT(3,NRBE2),JR(3,NRBE2),
     .        IAD, NS, ICOM, NSN, MID, NHI, IRAD
C-----------------------------------------------
      CALL PRERBE2(IRBE2 ,JT  ,JR   )
C
      DO N=1,NRBE2
        IAD = IRBE2(1,N)
        M  = IRBE2(3,N)
        NSN = IRBE2(5,N)
        ISK = IRBE2(7,N)
        IRAD = IRBE2(11,N)
        CALL SMS_RBE_4(NSN   ,LRBE2(IAD+1),V     ,W     ,MS    ,
     1                 JT(1,N),M  ,IRAD   ,ISK   ,SKEW  )
      ENDDO
C
      RETURN
      END

Chd|====================================================================
Chd|  SMS_RBE_PREC                  source/ams/sms_rbe2.F         
Chd|-- called by -----------
Chd|        SMS_PCG                       source/ams/sms_pcg.F          
Chd|-- calls ---------------
Chd|        PRERBE2                       source/constraints/general/rbe2/rbe2f.F
Chd|        SMS_RBE_5                     source/ams/sms_rbe2.F         
Chd|        SPMD_EXCH_RBE2_SMS            source/mpi/kinematic_conditions/spmd_exch_rbe2_sms.F
Chd|        SPMD_MAX_I                    source/mpi/implicit/imp_spmd.F
Chd|====================================================================
      SUBROUTINE SMS_RBE_PREC(
     1   IRBE2 ,LRBE2  ,DIAG_SMS ,MS   ,DIAG_SMS3,
     1   SKEW  ,WEIGHT ,IAD_RBE2 ,FR_RBE2M,NMRBE2)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE2(NRBE2L,*),LRBE2(*),WEIGHT(*),IAD_RBE2(*),
     .        FR_RBE2M(*) ,NMRBE2
C     REAL ou REAL*8
      my_real
     .   DIAG_SMS(*), MS(*), DIAG_SMS3(3,*), SKEW(LSKEW,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K, N, ISK, I, J, M, ISIZE, JT(3,NRBE2),JR(3,NRBE2),
     .        IAD, NS, ICOM, NSN, MID, NHI, IRAD
      my_real
     .        DD
      DOUBLE PRECISION
     .   FRBE2M6(3,6,NMRBE2)
C-----------------------------------------------
      CALL PRERBE2(IRBE2 ,JT  ,JR   )
      ICOM = IAD_RBE2(NSPMD+1)-IAD_RBE2(1)
      IF (NSPMD.GT.1)CALL SPMD_MAX_I(ICOM)
C
      DO NHI=0,NHRBE2
       DO N=1,NMRBE2
        DO K=1,6
         FRBE2M6(1,K,N) = ZERO
         FRBE2M6(2,K,N) = ZERO
         FRBE2M6(3,K,N) = ZERO
        END DO
       END DO
       DO N=1,NRBE2
        IF (IRBE2(9,N).NE.NHI) CYCLE
	IAD = IRBE2(1,N)
        NSN = IRBE2(5,N)
        M   = IRBE2(3,N)
        ISK = IRBE2(7,N)
	MID = IABS(IRBE2(6,N))
	IRAD = IRBE2(11,N)
        CALL SMS_RBE_5(NSN   ,LRBE2(IAD+1),DIAG_SMS3,MS    ,WEIGHT,
     1                 JT    ,FRBE2M6(1,1,MID),M      ,IRAD  ,ISK   ,
     2                 SKEW  )

       END DO
C-----------------
       IF (ICOM.GT.0) THEN
         ISIZE=3
         CALL SPMD_EXCH_RBE2_SMS(
     .    FRBE2M6 ,IAD_RBE2,FR_RBE2M,IAD_RBE2(NSPMD+1),ISIZE)
       ENDIF
C
C assemblage parith/ON
#include    "vectorize.inc"
       DO N=1,NRBE2
         IF (IRBE2(9,N).NE.NHI) CYCLE
         M  = IRBE2(3,N)
         MID = IRBE2(6,N)
         IRAD = IRBE2(11,N)
         IF (MID.LT.0) CYCLE
         DO J=1,3
           DD=DIAG_SMS3(J,M)
           DO K=1,6
             DD = DD + FRBE2M6(J,K,MID)
           ENDDO
           DIAG_SMS3(J,M)=DD
         END DO
       ENDDO
C
      END DO
C-----------------
      RETURN
      END

Chd|====================================================================
Chd|  SMS_RBE_1                     source/ams/sms_rbe2.F         
Chd|-- called by -----------
Chd|        SMS_RBE_CNDS                  source/ams/sms_rbe2.F         
Chd|-- calls ---------------
Chd|        CDI_BCN                       source/constraints/general/rbe2/rbe2_imp0.F
Chd|        SUM_6_FLOAT                   source/system/parit.F         
Chd|====================================================================
      SUBROUTINE SMS_RBE_1(NSL   ,ISL   ,X     ,A     ,AR    ,
     1                     MS    ,IN    ,WEIGHT,JT    ,FS6   ,
     2                     M     ,IRAD  ,ISK   ,SKEW  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSL,ISL(*),WEIGHT(*),JT(3),M,IRAD,ISK
C     REAL ou REAL*8
      my_real
     .   X(3,*), A(3,*), AR(3,*), MS(*), IN(*), SKEW(LSKEW,*)
      DOUBLE PRECISION
     .   FS6(3,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, K, IJT, JT1(3), IC
C     REAL ou REAL*8
      my_real
     .   F1(NSL), F2(NSL), F3(NSL), RX, RY, RZ, CDT(9)
C-----------------------------------------------
      IF ((JT(1)+JT(2)+JT(3)).GT.0) THEN
       IJT=1
      ELSE
       IJT=0
      ENDIF

      IF(ISK<=1)THEN
C
C Remontee des forces
        DO K = 1, 6
          FS6(1,K) = ZERO
          FS6(2,K) = ZERO
          FS6(3,K) = ZERO
        END DO
C
        DO I=1,NSL
         N = ISL(I)
         IF(WEIGHT(N).EQ.1) THEN
           F1(I)=JT(1)*A(1,N)
           F2(I)=JT(2)*A(2,N)
           F3(I)=JT(3)*A(3,N)
         ELSE
           F1(I)=ZERO
           F2(I)=ZERO
           F3(I)=ZERO
         ENDIF
        ENDDO
C
      ELSE ! IF(ISK<=1)THEN
        IC = JT(1)*100+JT(2)*10+JT(3)
        CALL CDI_BCN(IC   ,SKEW(1,ISK)  ,JT   ,CDT  ,JT1  )
        DO I=1,NSL
          N  = ISL(I)
          RX = A(1,N)*WEIGHT(N)
          RY = A(2,N)*WEIGHT(N)
          RZ = A(3,N)*WEIGHT(N)
          F1(I) = CDT(1)*RX+CDT(2)*RY+CDT(3)*RZ
          F2(I) = CDT(4)*RX+CDT(5)*RY+CDT(6)*RZ
          F3(I) = CDT(7)*RX+CDT(8)*RY+CDT(9)*RZ
        ENDDO
      END IF
C
C Traitement Parith/ON avant echange
C
      CALL SUM_6_FLOAT(1  ,NSL  ,F1, FS6(1,1), 3)
      CALL SUM_6_FLOAT(1  ,NSL  ,F2, FS6(2,1), 3)
      CALL SUM_6_FLOAT(1  ,NSL  ,F3, FS6(3,1), 3)

      RETURN
      END

Chd|====================================================================
Chd|  SMS_RBE_2                     source/ams/sms_rbe2.F         
Chd|-- called by -----------
Chd|        SMS_RBE_CNDS                  source/ams/sms_rbe2.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SMS_RBE_2(NSL   ,ISL   ,X     ,A     ,AR    ,
     1                     JT    ,M     ,IRAD  ,ISK   ,SKEW  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSL, ISL(*), JT(3), M, IRAD, ISK
C     REAL ou REAL*8
      my_real
     .   X(3,*), A(3,*), AR(3,*), SKEW(LSKEW,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, IJT
C     REAL ou REAL*8
      my_real
     .   AAX, AAY, AAZ
C-----------------------------------------------
      IF ((JT(1)+JT(2)+JT(3)).GT.0) THEN
       IJT=1
      ELSE
       IJT=0
      ENDIF
C
C Reset 2nd membre
      IF(ISK<=1)THEN
        DO I=1,NSL
          N = ISL(I)
          IF(JT(3).GT.0)THEN
            A(3,N) =ZERO
          ENDIF
          IF(JT(2).GT.0)THEN
            A(2,N) =ZERO
          ENDIF
          IF(JT(1).GT.0)THEN
            A(1,N) =ZERO
          ENDIF
        END DO
      ELSE
        DO I=1,NSL
          N  = ISL(I)
          AAX  =JT(1)*(SKEW(1,ISK)*A(1,N)+SKEW(2,ISK)*A(2,N)+SKEW(3,ISK)*A(3,N))
          AAY  =JT(2)*(SKEW(4,ISK)*A(1,N)+SKEW(5,ISK)*A(2,N)+SKEW(6,ISK)*A(3,N))
          AAZ  =JT(3)*(SKEW(7,ISK)*A(1,N)+SKEW(8,ISK)*A(2,N)+SKEW(9,ISK)*A(3,N))
          A(1,N) =A(1,N)-AAX*SKEW(1,ISK)-AAY*SKEW(4,ISK)-AAZ*SKEW(7,ISK)
          A(2,N) =A(2,N)-AAX*SKEW(2,ISK)-AAY*SKEW(5,ISK)-AAZ*SKEW(8,ISK)
          A(3,N) =A(3,N)-AAX*SKEW(3,ISK)-AAY*SKEW(6,ISK)-AAZ*SKEW(9,ISK)
        ENDDO
      END IF
C
      RETURN
      END

Chd|====================================================================
Chd|  SMS_RBE_3                     source/ams/sms_rbe2.F         
Chd|-- called by -----------
Chd|        SMS_RBE_ACCL                  source/ams/sms_rbe2.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SMS_RBE_3(NSL   ,ISL   ,R     ,A     ,PREC_SMS3,
     1                     JT    ,M     ,IRAD  ,ISK   ,SKEW  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSL,ISL(*),JT(3),M,IRAD, ISK
C     REAL ou REAL*8
      my_real
     .   R(3,*), A(3,*), SKEW(LSKEW,*), PREC_SMS3(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, IJT
C     REAL ou REAL*8
      my_real
     .     AAX, AAY, AAZ, DAX, DAY, DAZ
C-----------------------------------------------
      IF ((JT(1)+JT(2)+JT(3)).GT.0) THEN
       IJT=1
      ELSE
       IJT=0
      ENDIF
C
C retablit accelerations secnds == main 
C (le terme de rotation AR x MN est dj pass au 2nd membre)
      IF(ISK<=1)THEN
        IF(JT(3).GT.0)A(3,M)=R(3,M)*PREC_SMS3(3,M)
        IF(JT(2).GT.0)A(2,M)=R(2,M)*PREC_SMS3(2,M)
        IF(JT(1).GT.0)A(1,M)=R(1,M)*PREC_SMS3(1,M)
        DO I=1,NSL
          N = ISL(I)
          IF(JT(3).GT.0)THEN
            A(3,N) =A(3,M)
          ENDIF
          IF(JT(2).GT.0)THEN
            A(2,N) =A(2,M)
          ENDIF
          IF(JT(1).GT.0)THEN
            A(1,N) =A(1,M)
          ENDIF
        END DO
      ELSE
        DO I=1,NSL
          N  = ISL(I)
          DAX  =A(1,N)-A(1,M)
          DAY  =A(2,N)-A(2,M)
          DAZ  =A(3,N)-A(3,M)
          AAX  =JT(1)*(SKEW(1,ISK)*DAX+SKEW(2,ISK)*DAY+SKEW(3,ISK)*DAZ)
          AAY  =JT(2)*(SKEW(4,ISK)*DAX+SKEW(5,ISK)*DAY+SKEW(6,ISK)*DAZ)
          AAZ  =JT(3)*(SKEW(7,ISK)*DAX+SKEW(8,ISK)*DAY+SKEW(9,ISK)*DAZ)
          A(1,N) =A(1,N)-AAX*SKEW(1,ISK)-AAY*SKEW(4,ISK)-AAZ*SKEW(7,ISK)
          A(2,N) =A(2,N)-AAX*SKEW(2,ISK)-AAY*SKEW(5,ISK)-AAZ*SKEW(8,ISK)
          A(3,N) =A(3,N)-AAX*SKEW(3,ISK)-AAY*SKEW(6,ISK)-AAZ*SKEW(9,ISK)
        ENDDO
      END IF

      RETURN
      END


Chd|====================================================================
Chd|  SMS_RBE_4                     source/ams/sms_rbe2.F         
Chd|-- called by -----------
Chd|        SMS_RBE_CORR                  source/ams/sms_rbe2.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SMS_RBE_4(NSL   ,ISL   ,V     ,W     ,MS    ,
     1                     JT    ,M     ,IRAD  ,ISK   ,SKEW  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSL,ISL(*),JT(3),M,IRAD, ISK
C     REAL ou REAL*8
      my_real
     .   V(3,*), W(3,*), MS(*), SKEW(LSKEW,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, IJT
C     REAL ou REAL*8
      my_real
     .     AAX, AAY, AAZ, DAX, DAY, DAZ
C-----------------------------------------------
      IF ((JT(1)+JT(2)+JT(3)).GT.0) THEN
       IJT=1
      ELSE
       IJT=0
      ENDIF
C
C Corrige W=[M]V (MS(N)*V(..,N) already counted into MS(M)*V(..,M))
      IF(ISK<=1)THEN
        DO I=1,NSL
          N = ISL(I)
          IF(JT(3).GT.0)THEN
            W(3,N) =W(3,N)-MS(N)*V(3,N)
          ENDIF
          IF(JT(2).GT.0)THEN
            W(2,N) =W(2,N)-MS(N)*V(2,N)
          ENDIF
          IF(JT(1).GT.0)THEN
            W(1,N) =W(1,N)-MS(N)*V(1,N)
          ENDIF
        END DO
      ELSE
        DO I=1,NSL
          N  = ISL(I)
          DAX  =MS(N)*V(1,N)
          DAY  =MS(N)*V(2,N)
          DAZ  =MS(N)*V(3,N)
          AAX  =JT(1)*(SKEW(1,ISK)*DAX+SKEW(2,ISK)*DAY+SKEW(3,ISK)*DAZ)
          AAY  =JT(2)*(SKEW(4,ISK)*DAX+SKEW(5,ISK)*DAY+SKEW(6,ISK)*DAZ)
          AAZ  =JT(3)*(SKEW(7,ISK)*DAX+SKEW(8,ISK)*DAY+SKEW(9,ISK)*DAZ)
          W(1,N) =W(1,N)-AAX*SKEW(1,ISK)-AAY*SKEW(4,ISK)-AAZ*SKEW(7,ISK)
          W(2,N) =W(2,N)-AAX*SKEW(2,ISK)-AAY*SKEW(5,ISK)-AAZ*SKEW(8,ISK)
          W(3,N) =W(3,N)-AAX*SKEW(3,ISK)-AAY*SKEW(6,ISK)-AAZ*SKEW(9,ISK)
        ENDDO
      END IF

      RETURN
      END

Chd|====================================================================
Chd|  SMS_RBE_5                     source/ams/sms_rbe2.F         
Chd|-- called by -----------
Chd|        SMS_DIAG_RBE2                 source/ams/sms_rbe2.F         
Chd|        SMS_RBE_PREC                  source/ams/sms_rbe2.F         
Chd|-- calls ---------------
Chd|        CDI_BCN                       source/constraints/general/rbe2/rbe2_imp0.F
Chd|        SUM_6_FLOAT                   source/system/parit.F         
Chd|====================================================================
      SUBROUTINE SMS_RBE_5(NSL   ,ISL   ,DIAG_SMS3,MS    ,WEIGHT,
     1                     JT    ,FS6   ,M        ,IRAD  ,ISK   ,
     2                     SKEW  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSL,ISL(*),WEIGHT(*),JT(3),M,IRAD, ISK
C     REAL ou REAL*8
      my_real
     .   DIAG_SMS3(3,*), MS(*), SKEW(LSKEW,*)
      DOUBLE PRECISION
     .   FS6(3,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, K, IJT, JT1(3), IC
C     REAL ou REAL*8
      my_real
     .   F1(NSL), F2(NSL), F3(NSL), RX, RY, RZ, CDT(9)
C-----------------------------------------------
      IF ((JT(1)+JT(2)+JT(3)).GT.0) THEN
       IJT=1
      ELSE
       IJT=0
      ENDIF

C
C Remontee des diagonales
      DO K = 1, 6
        FS6(1,K) = ZERO
        FS6(2,K) = ZERO
        FS6(3,K) = ZERO
      END DO
C
      IF(ISK<=1)THEN
        DO I=1,NSL
         N = ISL(I)
         IF(WEIGHT(N).EQ.1) THEN
           F1(I)=JT(1)*(DIAG_SMS3(1,N)-MS(N))
           F2(I)=JT(2)*(DIAG_SMS3(2,N)-MS(N))
           F3(I)=JT(3)*(DIAG_SMS3(3,N)-MS(N))
         ELSE
           F1(I)=ZERO
           F2(I)=ZERO
           F3(I)=ZERO
         ENDIF
        ENDDO
C
      ELSE ! IF(ISK<=1)THEN
        IC = JT(1)*100+JT(2)*10+JT(3)
        CALL CDI_BCN(IC   ,SKEW(1,ISK)  ,JT   ,CDT  ,JT1  )
        DO I=1,NSL
          N  = ISL(I)
          RX = (DIAG_SMS3(1,N)-MS(N))*WEIGHT(N)
          RY = (DIAG_SMS3(2,N)-MS(N))*WEIGHT(N)
          RZ = (DIAG_SMS3(3,N)-MS(N))*WEIGHT(N)
          F1(I) = CDT(1)*RX+CDT(2)*RY+CDT(3)*RZ
          F2(I) = CDT(4)*RX+CDT(5)*RY+CDT(6)*RZ
          F3(I) = CDT(7)*RX+CDT(8)*RY+CDT(9)*RZ
        ENDDO
      END IF
C
C
C Traitement Parith/ON avant echange
C
      CALL SUM_6_FLOAT(1  ,NSL  ,F1, FS6(1,1), 3)
      CALL SUM_6_FLOAT(1  ,NSL  ,F2, FS6(2,1), 3)
      CALL SUM_6_FLOAT(1  ,NSL  ,F3, FS6(3,1), 3)

      RETURN
      END

Chd|====================================================================
Chd|  SMS_RBE2_S                    source/ams/sms_rbe2.F         
Chd|-- called by -----------
Chd|        SMS_RBE_CNDS                  source/ams/sms_rbe2.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SMS_RBE2_S(IRBE2 ,ISIZE,A     ,WEIGHT,F6    ,
     1                      NMRBE2,IH    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE2(NRBE2L,*),ISIZE, WEIGHT(*),NMRBE2,IH
C     REAL ou REAL*8
      my_real
     .   A(ISIZE,*)
      DOUBLE PRECISION
     .   F6(ISIZE,6,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, N, NS ,NML, IAD,JJ,M,MID,IROT,IRAD
C     REAL ou REAL*8
C======================================================================|
#include    "vectorize.inc"
      DO N=1,NRBE2
        IF (IH.NE.IRBE2(9,N)) CYCLE
        M  = IRBE2(3,N)
        MID = IRBE2(6,N)
        IRAD = IRBE2(11,N)
        IF (MID.LT.0) CYCLE
        DO K=1,6
         DO J=1,ISIZE
	  A(J,M) = A(J,M)+ F6(J,K,MID)
         ENDDO
        ENDDO
      ENDDO
C---
      RETURN
      END

