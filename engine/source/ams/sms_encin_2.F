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
Chd|  SMS_ENCIN_2                   source/ams/sms_encin_2.F      
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SMS_BCS                       source/ams/sms_bcs.F          
Chd|        SMS_BCSCYC                    source/ams/sms_bcscyc.F       
Chd|        SMS_FIXVEL                    source/ams/sms_fixvel.F       
Chd|        SMS_MAV_LT                    source/ams/sms_pcg.F          
Chd|        SMS_RBE3T1                    source/ams/sms_rbe3.F         
Chd|        SMS_RBE_CNDS                  source/ams/sms_rbe2.F         
Chd|        SMS_RBE_CORR                  source/ams/sms_rbe2.F         
Chd|        SPMD_EXCH_A_RB6               source/mpi/kinematic_conditions/spmd_exch_a_rb6.F
Chd|        SPMD_LIST_SMS                 source/mpi/ams/spmd_sms.F     
Chd|        SPMD_MIJ_SMS                  source/mpi/ams/spmd_sms.F     
Chd|        INTSTAMP_MOD                  share/modules/intstamp_mod.F  
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SMS_ENCIN_2(
     1        ITASK    ,NODFT    ,NODLT    ,NODXI_SMS,
     2        MS       ,JAD_SMS  ,JDI_SMS  ,LT_SMS    ,INDX1_SMS,
     3        DIAG_SMS ,IAD_ELEM ,FR_ELEM  ,WEIGHT    ,V        ,
     4        A        ,WV       ,WMV      ,WDG       ,XMOM_SMS ,
     5        ICODT    ,ICODR    ,ISKEW    ,SKEW      ,IBFV     ,
     6        VEL      ,NPC      ,TF       ,X         ,D        ,
     7        SENSOR   ,LSENSOR  ,IFRAME   ,XFRAME    ,JADI_SMS ,
     8        JDII_SMS ,LTI_SMS  ,ISKYI_SMS ,MSKYI_SMS ,FR_SMS  ,
     9        FR_RMS   ,NPBY     ,TAGSLV_RBY_SMS,INTSTAMP,CPTREAC,
     A        NODREAC  ,FTHREAC  ,AR        ,VR        ,
     B        DR       ,IN       ,RBY      ,IRBE2     ,LRBE2     ,
     C        IAD_RBE2 ,FR_RBE2M ,NMRBE2   ,R2SIZE    ,IRBE3     ,
     D        LRBE3    ,FRBE3    ,IAD_RBE3M,FR_RBE3M  ,FR_RBE3MP ,
     E        RRBE3    ,RRBE3_PON,IAD_RBY  ,FR_RBY6   ,RBY6      ,
     F        LPBY     ,TAGMSR_RBY_SMS,R3SIZE,NODII_SMS,INDX2_SMS,
     G        IBCSCYC  ,LBCSCYC  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MOD_SMS_WORK
      USE INTSTAMP_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "intstamp_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "scr07_c.inc"
#include      "sms_c.inc"
#include      "stati_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  ITASK, NODFT, NODLT, NODXI_SMS(*),
     .         JAD_SMS(*), JDI_SMS(*), INDX1_SMS(*),
     .         IAD_ELEM(2,NSPMD+1) ,FR_ELEM(*), WEIGHT(*),
     .         ICODT(*), ICODR(*), ISKEW(*),
     .         NPC(*), IBFV(NIFV,*), LSENSOR(NSENSI,*), IFRAME(LISKN,*),
     .         JADI_SMS(*), JDII_SMS(*),CPTREAC,NODREAC(*),
     .         FR_SMS(NSPMD+1), FR_RMS(NSPMD+1), ISKYI_SMS(*),
     .         NPBY(NNPBY,*), TAGSLV_RBY_SMS(*),
     .         IRBE2(NRBE2L,*), LRBE2(*), IAD_RBE2(*),
     .         FR_RBE2M(*), NMRBE2, R2SIZE, IRBE3(NRBE3L,*), LRBE3(*),
     .         IAD_RBE3M(*) ,FR_RBE3M(*) ,FR_RBE3MP(*),
     .         FR_RBY6(*) ,IAD_RBY(*) ,LPBY(*) ,TAGMSR_RBY_SMS(*),R3SIZE,
     .         NODII_SMS(*),INDX2_SMS(*),IBCSCYC(*),LBCSCYC(*)
      my_real
     .    MS(*), DIAG_SMS(*), LT_SMS(*),
     .    V(3,*), A(3,*), WV(3,*), WMV(3,*), WDG(*),  XMOM_SMS(3,*),
     .    SKEW(*), X(3,*), D(3,*), TF(*), VEL(LFXVELR,*),
     .    SENSOR(NSENSR,*), XFRAME(NXFRAME,*),
     .    LTI_SMS(*), MSKYI_SMS(*),FTHREAC(6,*),
     .    AR(3,*), VR(3,*), DR(3,*), IN(*), RBY(NRBY,*),
     .    FRBE3(*), RRBE3(*)
      DOUBLE PRECISION RRBE3_PON(*)
      DOUBLE PRECISION RBY6(8,6,NRBYKIN)
      TYPE(INTSTAMP_DATA) INTSTAMP(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER NODFT1_SMS, NODLT1_SMS
      INTEGER NODFT2_SMS, NODLT2_SMS
      INTEGER N, IBID, IPRI, INFO, ITHIS, IT, M, MSR, NN, IERROR,
     .        I, IAD, NSN, K, KI, NRBDIM
      my_real
     .        RBID, DT05, MAS, P1, P2, P3
C-----
      INTEGER, DIMENSION(:), ALLOCATABLE :: IMV
      my_real
     .       , DIMENSION(:), ALLOCATABLE :: MV
      DOUBLE PRECISION
     .       , DIMENSION(:,:), ALLOCATABLE :: MV6
C-----
      DATA IT/0/
C-----------------------------------------------
      IPRI=1
      IF(T1S==TT)IPRI=MOD(NCYCLE,IABS(NCPRI))
      INFO=MDESS-MANIM
      ITHIS=0
      IF(TT<THIS)ITHIS=1
      IF(IPRI/=0.AND.ITHIS/=0.AND.
     .   INFO<=0.AND.ISTAT==0
     .   .AND.NTH==0.AND.NANIM==0) RETURN
C
      IF(IPARIT/=0)THEN
        IF(DEBUG(9)==0)THEN
          ALLOCATE(IMV(2*NISKY_SMS+FR_RMS(NSPMD+1)),
     .           MV (3*(2*NISKY_SMS+FR_RMS(NSPMD+1))),
     .           MV6(6,3*(2*NISKY_SMS+FR_RMS(NSPMD+1))),
     .           STAT=IERROR)
        ELSE
          ALLOCATE(IMV(NUMNOD+NNZ_SMS+2*NISKY_SMS+FR_RMS(NSPMD+1)),
     .           MV (3*(NUMNOD+NNZ_SMS+2*NISKY_SMS+FR_RMS(NSPMD+1))),
     .           MV6(6,3*(NUMNOD+NNZ_SMS+2*NISKY_SMS+FR_RMS(NSPMD+1))),
     .           STAT=IERROR)
        END IF
        IF(IERROR/=0) THEN
          CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .                C1='(/DT/.../AMS)')
          CALL ARRET(2)
        ENDIF
        IF(ITASK==0)THEN
          ALLOCATE(MW6(6,3*NUMNOD),STAT=IERROR)
          IF(IERROR/=0) THEN
            CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .                  C1='(/DT/.../AMS)')
            CALL ARRET(2)
          ENDIF
        END IF
      END IF
      IF(ITASK==0)THEN
        ALLOCATE(LIST_SMS(FR_SMS(NSPMD+1)),LIST_RMS(FR_RMS(NSPMD+1)),
     .        MSKYI_FI_SMS(FR_RMS(NSPMD+1)),
     .        VFI(3,FR_RMS(NSPMD+1)+FR_SMS(NSPMD+1)),STAT=IERROR)
        IF(IERROR/=0) THEN
          CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .                C1='(/DT/.../AMS)')
          CALL ARRET(2)
        ENDIF
      ENDIF
C
      IF(NSPMD > 1)THEN
        IF(ITASK==0)THEN
          CALL SPMD_LIST_SMS(ISKYI_SMS,FR_SMS,FR_RMS,LIST_SMS,LIST_RMS,
     .                       NPBY     ,TAGSLV_RBY_SMS)
        END IF
C
        CALL MY_BARRIER
C
      END IF
C
      NODFT1_SMS=1+ITASK*NINDX1_SMS/NTHREAD
      NODLT1_SMS=(ITASK+1)*NINDX1_SMS/NTHREAD
C
      NODFT2_SMS=1+ITASK*NINDX2_SMS/NTHREAD
      NODLT2_SMS=(ITASK+1)*NINDX2_SMS/NTHREAD
C
      DT05=HALF*DT1
      DO N=NODFT,NODLT
C
       WV(1,N) = V(1,N)+DT05*A(1,N)
       WV(2,N) = V(2,N)+DT05*A(2,N)
       WV(3,N) = V(3,N)+DT05*A(3,N)
C
       MAS=MS(N)
       XMOM_SMS(1,N)=MAS*WV(1,N)
       XMOM_SMS(2,N)=MAS*WV(2,N)
       XMOM_SMS(3,N)=MAS*WV(3,N)
C
       IF(NODXI_SMS(N)/=0.AND.TAGSLV_RBY_SMS(N)==0) THEN
         WDG(N)=MAX(ZERO,DIAG_SMS(N)-MS(N))
       ELSEIF(TAGSLV_RBY_SMS(N)/=0)THEN
         WDG(N)=DIAG_SMS(N)
       END IF
C
      ENDDO
C
C-----------------------------------
      IF(NRBODY/=0)THEN
C
        CALL MY_BARRIER()
C 
        DO N=NODFT1_SMS,NODLT1_SMS
         I=INDX1_SMS(N)
         M=TAGSLV_RBY_SMS(I)
         IF(M /= 0)THEN
           MSR=NPBY(1,M)
           WV(1,I)=WV(1,MSR)
           WV(2,I)=WV(2,MSR)
           WV(3,I)=WV(3,MSR)
         END IF
        END DO
C 
      END IF
C
      CALL MY_BARRIER
C
C----
C
      IF(NSPMD > 1)THEN
C
        CALL MY_BARRIER()
C
        IF(ITASK==0) THEN   ! comm sur 1er thread
          CALL SPMD_MIJ_SMS(
     1           ISKYI_SMS,FR_SMS,FR_RMS,LIST_RMS,MSKYI_SMS,
     2           MSKYI_FI_SMS)
        END IF
      END IF
C----
      CALL SMS_MAV_LT(
     1        NODFT   ,NODLT   ,NUMNOD ,JAD_SMS  ,JDI_SMS  ,
     2        ITASK   ,WDG     ,LT_SMS ,WV       ,WMV      ,
     3        NODFT1_SMS,NODLT1_SMS,INDX1_SMS,NODXI_SMS,IAD_ELEM ,
     4        FR_ELEM   ,WEIGHT    ,JADI_SMS ,JDII_SMS ,LTI_SMS  ,
     5        ISKYI_SMS ,MSKYI_SMS ,FR_SMS   ,FR_RMS   ,LIST_SMS ,
     6        LIST_RMS  ,MSKYI_FI_SMS ,VFI   ,IMV      ,MV       ,
     7        MV6       ,MW6       ,NODFT2_SMS,NODLT2_SMS,INDX2_SMS,
     8        NODII_SMS )
C
      CALL MY_BARRIER
C
C-----------------------------------
      IF(NRBODY/=0)THEN
C
        CALL MY_BARRIER()
C 
!$OMP DO SCHEDULE(DYNAMIC,1)
        DO M =1,NRBODY
          DO K = 1, 6
            RBY6(1,K,M) = ZERO
            RBY6(2,K,M) = ZERO
            RBY6(3,K,M) = ZERO
          END DO
C
          MSR=NPBY(1,M)
          IF(MSR < 0) CYCLE
C
          IF(TAGMSR_RBY_SMS(MSR) /= 0) THEN
            RBY6(1,1,M)=WMV(1,MSR)*WEIGHT(MSR)
            RBY6(2,1,M)=WMV(2,MSR)*WEIGHT(MSR)
            RBY6(3,1,M)=WMV(3,MSR)*WEIGHT(MSR)
          END IF
C
        END DO
!$OMP  END DO

!$OMP SINGLE
       DO N=1,NINDX1_SMS
        I=INDX1_SMS(N)
        M=TAGSLV_RBY_SMS(I)
        IF(M /= 0)THEN
          IF(WEIGHT(I) /= 0)THEN
            RBY6(1,1,M)=RBY6(1,1,M)+WMV(1,I)
            RBY6(2,1,M)=RBY6(2,1,M)+WMV(2,I)
            RBY6(3,1,M)=RBY6(3,1,M)+WMV(3,I)
          END IF
        END IF
       END DO
!$OMP END SINGLE

       IF (NSPMD > 1) THEN
!$OMP SINGLE
         NRBDIM=3
         CALL SPMD_EXCH_A_RB6(
     1     NRBDIM,IAD_RBY,FR_RBY6,IAD_RBY(NSPMD+1),RBY6)
!$OMP END SINGLE
       END IF

!$OMP DO SCHEDULE(DYNAMIC,1)
        DO M =1,NRBODY
          MSR=NPBY(1,M)
          IF(MSR < 0) CYCLE
C
C         IF(TAGMSR_RBY_SMS(MSR) /= 0) THEN
          WMV(1,MSR)=RBY6(1,1,M)
          WMV(2,MSR)=RBY6(2,1,M)
          WMV(3,MSR)=RBY6(3,1,M)
C         END IF
        END DO
!$OMP  END DO
      END IF
C-----------------------------------
C     reimp wmv=0
      CALL SMS_BCS(NODFT1_SMS,NODLT1_SMS,INDX1_SMS,ICODT  ,ISKEW ,
     2             SKEW      ,WMV       ,NODLT1_SMS )
C     
      IF (NBCSCYC>0) CALL SMS_BCSCYC(IBCSCYC,LBCSCYC,SKEW,X,WMV)
C
      IF(NFXVEL > 0)THEN
C
C       reimp wmv=0
        CALL MY_BARRIER
C
        IF(ITASK==0)
     .  CALL SMS_FIXVEL(IBFV   ,WMV     ,V     ,NPC    ,TF     ,
     2                  VEL    ,DIAG_SMS,X     ,SKEW   ,SENSOR ,
     3                  LSENSOR,WEIGHT  ,D     ,IFRAME ,XFRAME ,
     4                  -(IT+1),DIAG_SMS,NODXI_SMS,CPTREAC,
     5                  NODREAC,FTHREAC ,AR     ,VR       ,DR  ,
     6                  IN     ,RBY     )
      END IF
C
      CALL MY_BARRIER
C
C----
      DO N=NODFT1_SMS,NODLT1_SMS
       I=INDX1_SMS(N)
       IF(TAGSLV_RBY_SMS(I)==0)THEN
         XMOM_SMS(1,I)=XMOM_SMS(1,I)+WMV(1,I)
         XMOM_SMS(2,I)=XMOM_SMS(2,I)+WMV(2,I)
         XMOM_SMS(3,I)=XMOM_SMS(3,I)+WMV(3,I)
       END IF
      ENDDO
C-----------------------------------
C RBE2
C-----------------------------------
      IF (NRBE2>0.OR.R2SIZE>0) THEN
C
        CALL MY_BARRIER
C
        IF(ITASK==0)THEN
C
          CALL SMS_RBE_CORR(
     1     IRBE2  ,LRBE2  ,WV      ,XMOM_SMS  ,MS    ,
     1     SKEW   ,WEIGHT ,IAD_RBE2,FR_RBE2M,NMRBE2)
C
          CALL SMS_RBE_CNDS(
     1     IRBE2 ,LRBE2 ,X      ,XMOM_SMS,AR     ,
     1     MS    ,IN    ,SKEW   ,WEIGHT ,IAD_RBE2,
     2     FR_RBE2M,NMRBE2)
C
        END IF
C
        CALL MY_BARRIER
C
      END IF
C-----------------------------------
C RBE3
C-----------------------------------
      IF (NRBE3>0)THEN
        IF(ITASK==0)THEN
          CALL SMS_RBE3T1(
     1      IRBE3 ,LRBE3  ,X        ,XMOM_SMS,FRBE3    ,
     2      SKEW  ,WEIGHT ,IAD_RBE3M,FR_RBE3M,FR_RBE3MP,
     3      RRBE3 ,RRBE3_PON,R3SIZE )
        END IF
C
        CALL MY_BARRIER
C
      END IF
C
      IF(ITASK==0)DEALLOCATE(LIST_SMS,LIST_RMS,
     .        MSKYI_FI_SMS,VFI)
      IF(IPARIT/=0)THEN
        DEALLOCATE(IMV, MV, MV6)
        IF(ITASK==0)DEALLOCATE(MW6)
      END IF
C
C     fin section //
      RETURN
      END
