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
      module MOD_SMS_WORK
      implicit none
#include      "my_real.inc"
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE :: CJWORK, FREA, 
     .         ARWORK,PR,ZR,YR
      INTEGER, DIMENSION(:), ALLOCATABLE :: IRWL_WORK
C-----
      INTEGER, DIMENSION(:), ALLOCATABLE :: LIST_SMS,LIST_RMS
      my_real
     .       , DIMENSION(:), ALLOCATABLE :: MSKYI_FI_SMS
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE :: VFI
      DOUBLE PRECISION
     .       , DIMENSION(:,:), ALLOCATABLE :: MW6
      END MODULE MOD_SMS_WORK
Chd|====================================================================
Chd|  SMS_MASS_SCALE_2              source/ams/sms_mass_scale_2.F 
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SMS_ADMESH_0                  source/ams/sms_admesh.F       
Chd|        SMS_BCS                       source/ams/sms_bcs.F          
Chd|        SMS_BCSCYC                    source/ams/sms_bcscyc.F       
Chd|        SMS_CJOINT_0                  source/ams/sms_cjoint.F       
Chd|        SMS_CJOINT_2                  source/ams/sms_cjoint.F       
Chd|        SMS_FIXVEL                    source/ams/sms_fixvel.F       
Chd|        SMS_GRAVIT                    source/ams/sms_gravit.F       
Chd|        SMS_MAV_LT                    source/ams/sms_pcg.F          
Chd|        SMS_PCG                       source/ams/sms_pcg.F          
Chd|        SMS_RBE3T1                    source/ams/sms_rbe3.F         
Chd|        SMS_RBE_CNDS                  source/ams/sms_rbe2.F         
Chd|        SMS_THBCS                     source/ams/sms_thbcs.F        
Chd|        SPMD_EXCH_A_RB6               source/mpi/kinematic_conditions/spmd_exch_a_rb6.F
Chd|        SPMD_LIST_SMS                 source/mpi/ams/spmd_sms.F     
Chd|        SPMD_MIJ_SMS                  source/mpi/ams/spmd_sms.F     
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        NGR2USR                       source/input/freform.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INTSTAMP_MOD                  share/modules/intstamp_mod.F  
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE SMS_MASS_SCALE_2(
     1        ITASK    ,NODFT    ,NODLT    ,NODII_SMS ,INDX2_SMS ,
     2        NODXI_SMS,MS       ,MS0      ,A         ,ICODT     ,
     3        ICODR    ,ISKEW    ,SKEW     ,JAD_SMS   ,JDI_SMS   ,
     4        LT_SMS   ,X_SMS    ,P_SMS    ,Z_SMS     ,Y_SMS     ,
     5        PREC_SMS ,INDX1_SMS,DIAG_SMS ,IAD_ELEM  ,FR_ELEM   ,
     6        WEIGHT   ,NPBY      ,LPBY      ,
     7        TAGSLV_RBY_SMS,LAD_SMS ,KAD_SMS ,JRB_SMS,IBFV      ,
     8        VEL      ,NPC      ,TF       ,V         ,X         ,
     9        D        ,SENSOR_TAB,NSENSOR ,IFRAME    ,XFRAME    ,
     A        JADI_SMS ,JDII_SMS ,LTI_SMS  ,FR_SMS    ,FR_RMS    ,
     B        ISKYI_SMS,MSKYI_SMS,RES_SMS  ,IGRV      ,AGRV      ,
     C        LGRAV    ,ILINK    ,RLINK    ,FR_RL     ,FRL6      ,
     D        NNLINK   ,LNLINK   ,FR_LL    ,FNL6      ,TAG_LNK_SMS,
     E        ITAB     ,FSAV     ,LJOINT   ,IADCJ     ,FR_CJ      ,
     F        AM       ,VR       ,IN       ,FRL       ,FNL        ,
     G        NPRW     ,LPRW     ,RWBUF    ,RWSAV     ,FOPT       ,
     H        FR_WALL  ,NRWL_SMS ,INTSTAMP ,KINET     ,IXC        ,
     I        IXTG     ,SH4TREE  ,SH3TREE  ,CPTREAC   ,NODREAC    ,
     J        FTHREAC  ,FRWL6    ,DIM      ,TAGSLV_RBY,DAMPR      ,
     K        DAMP     ,IGRNOD   ,DR       ,RBY       ,
     L        TAGMSR_RBY_SMS,JSM_SMS,IRBE2  ,LRBE2      ,
     N        IAD_RBE2 ,FR_RBE2M ,NMRBE2   ,R2SIZE    ,IRBE3      ,
     O        LRBE3      ,FRBE3    ,IAD_RBE3M ,FR_RBE3M ,FR_RBE3MP,
     P        RRBE3      ,RRBE3_PON,PREC_SMS3 ,DIAG_SMS3,IAD_RBY  ,
     Q        FR_RBY6    ,RBY6     ,R3SIZE ,BETATE      ,IBCSCYC  ,
     R        LBCSCYC  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MOD_SMS_WORK
      USE INTSTAMP_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SENSOR_MOD
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
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "remesh_c.inc"
#include      "scr03_c.inc"
#include      "sms_c.inc"
#include      "tabsiz_c.inc"
#include      "task_c.inc"
#include      "timeri_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "stati_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  ITASK, NODFT,NSENSOR,NODLT, NODII_SMS(*), INDX2_SMS(*),
     .         NODXI_SMS(*), ICODT(*), ICODR(*),
     .         ISKEW(*), JAD_SMS(*), JDI_SMS(*), INDX1_SMS(*),
     .         IAD_ELEM(2,NSPMD+1) ,FR_ELEM(*), WEIGHT(*),
     .         NPBY(NNPBY,*), LPBY(*), TAGSLV_RBY_SMS(*), TAGSLV_RBY(*),
     .         LAD_SMS(*), KAD_SMS(*), JRB_SMS(*),
     .         NPC(*), IBFV(NIFV,*),IFRAME(LISKN,*),
     .         JADI_SMS(*), JDII_SMS(*),
     .         FR_RMS(NSPMD+1), FR_SMS(NSPMD+1), ISKYI_SMS(LSKYI_SMS,*),
     .         IGRV(*),CPTREAC,NODREAC(*),
     .         ILINK(*),RLINK(*), FR_RL(NSPMD+2,*), NNLINK(10,*),
     .         LNLINK(*), FR_LL(NSPMD+2,*), TAG_LNK_SMS(*), ITAB(*),
     .         LJOINT(*), FR_CJ(*), IADCJ(*),
     .         NPRW(*), LPRW(*), FR_WALL(*), NRWL_SMS(*),
     .         KK, MAIN, KINET(*),
     .         IXC(NIXC,*), IXTG(NIXTG,*),
     .         SH4TREE(KSH4TREE,*), SH3TREE(KSH3TREE,*), DIM,
     .         TAGMSR_RBY_SMS(*), JSM_SMS(*),
     .         IRBE2(NRBE2L,*), LRBE2(*), IAD_RBE2(*),
     .         FR_RBE2M(*), NMRBE2, R2SIZE, IRBE3(NRBE3L,*), LRBE3(*),
     .         IAD_RBE3M(*) ,FR_RBE3M(*) ,FR_RBE3MP(*),
     .         FR_RBY6(*),IAD_RBY(*),R3SIZE,IBCSCYC(*),LBCSCYC(*)
      my_real
     .    MS(*), MS0(*), A(3,*), DIAG_SMS(*),
     .    SKEW(LSKEW,*), LT_SMS(*),
     .    X_SMS(3,*), P_SMS(3,*), Y_SMS(3,*), Z_SMS(3,*), PREC_SMS(*),
     .    V(3,*), X(3,*), D(3,*), TF(*), VEL(LFXVELR,*),
     .    XFRAME(NXFRAME,*), LTI_SMS(*), MSKYI_SMS(*),
     .    RES_SMS(3,*), AGRV(*),LGRAV(*),
     .    FSAV(NTHVKI,*), AM(3,*), VR(3,*), IN(*), FRL(*), FNL(*),
     .    RWBUF(*), RWSAV(*), FOPT(*),FTHREAC(6,*),
     .    DAMPR(NRDAMP,*), DAMP(DIM,*), DR(3,*), RBY(NRBY,*), 
     .    FRBE3(*), RRBE3(*), PREC_SMS3(3,*), DIAG_SMS3(3,*),BETATE
      DOUBLE PRECISION FRL6(*), FNL6(*), FRWL6(*), RRBE3_PON(*)
      DOUBLE PRECISION RBY6(8,6,NRBYKIN)
      TYPE(INTSTAMP_DATA) INTSTAMP(*)
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) ,INTENT(IN) :: SENSOR_TAB
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD) :: IGRNOD
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, ISP, IT, IX, IERROR
      INTEGER ICOUNT, J, K, L, NSN, IMOV, ITYP, ILAGM, IFLAG,
     .        N2, N3, N4, N5, N6, N7, ND, IGR, ISK,
     .        M, IAD, MSR, KAD, KI, KJ, JI, NSR,
     .        LOC_PROC, P, NN, LENR, SIZE, NRBDIM
      INTEGER NODFT1_SMS, NODLT1_SMS
      INTEGER NODFT2_SMS, NODLT2_SMS,NGR2USR
      my_real
     .   VX,VY,VZ, MVX, MVY, MVZ,
     .   VXJ, VYJ, VZJ, MAS,TFEXTT, ERRTET, DW, DT15, DT25, RBID,
     .   OMEGA, BETASDT, DAMPT, FACTB, D_TSTART, D_TSTOP, DA, ADT,
     .   P1, P2, P3, UOMEGA, DOMEGA
C-----
      INTEGER, DIMENSION(:), ALLOCATABLE :: IMV
      my_real
     .       , DIMENSION(:), ALLOCATABLE :: MV
      my_real,
     .   DIMENSION(3,NUMNOD) :: MVSKW,VSKW,RSKW,DAMPSKW
      DOUBLE PRECISION
     .       , DIMENSION(:,:), ALLOCATABLE :: MV6
      EXTERNAL NGR2USR
C-----------------------------------------------
      IF(ITASK==0)THEN
        ALLOCATE(CJWORK(18,NJOINT),
     .     FREA(3,NUMNOD), IRWL_WORK(SLPRW),
     .           STAT=IERROR)
        IF(IERROR/=0) THEN
          CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .          C1='(/DT/.../AMS)')
          CALL ARRET(2)
        ENDIF
      END IF
C
      CALL MY_BARRIER
C
      FREA(1:3,NODFT:NODLT)=ZERO
C
      IF(IPARIT/=0)THEN
        IF(DEBUG(9)==0)THEN
          ALLOCATE(IMV(2*NISKY_SMS+FR_RMS(NSPMD+1)),
     .           MV (3*(2*NISKY_SMS+FR_RMS(NSPMD+1))),
     .           MV6(6,3*(2*NISKY_SMS+FR_RMS(NSPMD+1))),STAT=IERROR)
        ELSE
          ALLOCATE(IMV(NUMNOD+NNZ_SMS+2*NISKY_SMS+FR_RMS(NSPMD+1)),
     .           MV (3*(NUMNOD+NNZ_SMS+2*NISKY_SMS+FR_RMS(NSPMD+1))),
     .           MV6(6,3*(NUMNOD+NNZ_SMS+2*NISKY_SMS+FR_RMS(NSPMD+1))),
     .           STAT=IERROR)
        END IF
        IF(IERROR/=0) THEN
          WRITE(ISTDO,*)
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
C
      CALL SMS_GRAVIT(IGRV  ,AGRV   ,NPC   ,TF   ,A     ,
     2                V     ,X      ,SKEW  ,MS   ,SENSOR_TAB,
     3                WEIGHT,LGRAV ,ITASK,TAGSLV_RBY_SMS,NSENSOR)
C
      CALL MY_BARRIER
C
      NODFT1_SMS=1+ITASK*NINDX1_SMS/NTHREAD
      NODLT1_SMS=(ITASK+1)*NINDX1_SMS/NTHREAD
C
      NODFT2_SMS=1+ITASK*NINDX2_SMS/NTHREAD
      NODLT2_SMS=(ITASK+1)*NINDX2_SMS/NTHREAD
C----
C
      DO N=NODFT,NODLT

        A(1,N)=A(1,N)+RES_SMS(1,N)
        A(2,N)=A(2,N)+RES_SMS(2,N)
        A(3,N)=A(3,N)+RES_SMS(3,N)

        RES_SMS(1,N)=ZERO
        RES_SMS(2,N)=ZERO
        RES_SMS(3,N)=ZERO

      END DO
C
      CALL MY_BARRIER
C
C--------------------------------------------
C     RAILEIGH DAMPING
C--------------------------------------------
      IF(NDAMP/=0.OR.ISTAT==1.OR.ISTAT==3)THEN
C
        DO N=NODFT,NODLT
         IF(NODXI_SMS(N)==0)THEN
           Z_SMS(1,N)=MS(N)*V(1,N)
           Z_SMS(2,N)=MS(N)*V(2,N)
           Z_SMS(3,N)=MS(N)*V(3,N)
         ELSE
           X_SMS(1,N)=V(1,N)
           X_SMS(2,N)=V(2,N)
           X_SMS(3,N)=V(3,N)
         END IF
        ENDDO
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
             X_SMS(1,I)=X_SMS(1,MSR)
             X_SMS(2,I)=X_SMS(2,MSR)
             X_SMS(3,I)=X_SMS(3,MSR)
           END IF
          END DO
C 
        END IF
C
        CALL MY_BARRIER
C
C       Z_SMS utilise temporairement pour [M]V
        CALL SMS_MAV_LT(
     1           NODFT   ,NODLT  ,NUMNOD ,JAD_SMS  ,JDI_SMS  ,
     2           ITASK   ,DIAG_SMS,LT_SMS,X_SMS ,Z_SMS ,
     3           NODFT1_SMS,NODLT1_SMS,INDX1_SMS,NODXI_SMS,IAD_ELEM ,
     4           FR_ELEM   ,WEIGHT    ,JADI_SMS ,JDII_SMS ,LTI_SMS  ,
     5           ISKYI_SMS ,MSKYI_SMS ,FR_SMS   ,FR_RMS   ,LIST_SMS ,
     6           LIST_RMS  ,MSKYI_FI_SMS ,VFI   ,IMV      ,MV      ,
     7           MV6       ,MW6       ,NODFT2_SMS,NODLT2_SMS,INDX2_SMS,
     8           NODII_SMS )
C
        CALL MY_BARRIER
C
C-----------------------------------
C     remontee Yi => Ym
C-----------------------------------
        IF(NRBODY/=0)THEN
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
              RBY6(1,1,M)=Z_SMS(1,MSR)*WEIGHT(MSR)
              RBY6(2,1,M)=Z_SMS(2,MSR)*WEIGHT(MSR)
              RBY6(3,1,M)=Z_SMS(3,MSR)*WEIGHT(MSR)
            END IF
C
          END DO
!$OMP END DO

!$OMP SINGLE
          DO N=1,NINDX1_SMS
           I=INDX1_SMS(N)
           M=TAGSLV_RBY_SMS(I)
           IF(M /= 0)THEN
             IF(WEIGHT(I) /= 0)THEN
               RBY6(1,1,M)=RBY6(1,1,M)+Z_SMS(1,I)
               RBY6(2,1,M)=RBY6(2,1,M)+Z_SMS(2,I)
               RBY6(3,1,M)=RBY6(3,1,M)+Z_SMS(3,I)
             END IF
             Z_SMS(1,I)=ZERO
             Z_SMS(2,I)=ZERO
             Z_SMS(3,I)=ZERO
           END IF
          END DO
!$OMP END SINGLE

         IF (NSPMD > 1) THEN
!$OMP SINGLE
           NRBDIM=3
           CALL SPMD_EXCH_A_RB6(
     1       NRBDIM,IAD_RBY,FR_RBY6,IAD_RBY(NSPMD+1),RBY6)
!$OMP END SINGLE
         END IF

!$OMP DO SCHEDULE(DYNAMIC,1)
          DO M =1,NRBODY
            MSR=NPBY(1,M)
            IF(MSR < 0) CYCLE
            IF(TAGMSR_RBY_SMS(MSR) /= 0) THEN
              Z_SMS(1,MSR)=RBY6(1,1,M)
              Z_SMS(2,MSR)=RBY6(2,1,M)
              Z_SMS(3,MSR)=RBY6(3,1,M)
            END IF
          END DO
!$OMP END DO
C
        END IF
C
        CALL MY_BARRIER
C
C-----------------------------------
        IF(ITASK==0)THEN
         IF (IMON>0) CALL STARTIME(5,1)
         DW = ZERO
         DO ND=1,NDAMP
          IGR   = NINT(DAMPR(2,ND))
          ISK   = NINT(DAMPR(15,ND))
          FACTB = DAMPR(16,ND)
          DAMPT  = MIN(DT1,DT2)*FACTB
          D_TSTART = DAMPR(17,ND)
          D_TSTOP  = DAMPR(18,ND)
          IF (TT>=D_TSTART .AND. TT<=D_TSTOP) THEN
            IF(ISK<=1)THEN
C----- Damping sur dof rotation et seulement -----
             IF (DAMPR(19,ND)>0) CYCLE
             DAMPA = DAMPR(3,ND)
             DAMPB = DAMPR(4,ND)
             BETASDT= -MIN(DAMPB,DAMPT)*DT1/MAX(DT1*DT1,EM30)
             OMEGA  = ONE/ (ONE + HALF * DAMPA * DT1)
             DO N=1,IGRNOD(IGR)%NENTITY
               I=IGRNOD(IGR)%ENTITY(N)
               IF(TAGSLV_RBY(I)/=0) CYCLE
               DA=A(1,I)-DAMPA*Z_SMS(1,I)-BETASDT *(A(1,I) - DAMP(1,I))
               DA = DA * OMEGA - A(1,I)
               DAMP(1,I) = A(1,I)
               A(1,I)    = A(1,I) + DA
C              DW =DW+DA*(V(1,I)+HALF*ACC(1,I)*DT1)*DT12*WEIGHT(I)
C              2nd order error
               DW =DW+DA*V(1,I)*DT12*WEIGHT(I)
             END DO
             DAMPA = DAMPR(5,ND)
             DAMPB = DAMPR(6,ND)
             BETASDT= -MIN(DAMPB,DAMPT)*DT1/MAX(DT1*DT1,EM30)
             OMEGA  = ONE/ (ONE + HALF * DAMPA * DT1)
             DO N=1,IGRNOD(IGR)%NENTITY
               I=IGRNOD(IGR)%ENTITY(N)
               IF(TAGSLV_RBY(I)/=0) CYCLE
               DA=A(2,I)-DAMPA*Z_SMS(2,I)-BETASDT *(A(2,I) - DAMP(2,I))
               DA = DA * OMEGA - A(2,I)
               DAMP(2,I) = A(2,I)
               A(2,I)    = A(2,I) + DA
C              2nd order error
               DW =DW+DA*V(2,I)*DT12*WEIGHT(I)
             END DO
             DAMPA = DAMPR(7,ND)
             DAMPB = DAMPR(8,ND)
             BETASDT= -MIN(DAMPB,DAMPT)*DT1/MAX(DT1*DT1,EM30)
             OMEGA  = ONE/ (ONE + HALF * DAMPA * DT1)
             DO N=1,IGRNOD(IGR)%NENTITY
               I=IGRNOD(IGR)%ENTITY(N)
               IF(TAGSLV_RBY(I)/=0) CYCLE
               DA=A(3,I)-DAMPA*Z_SMS(3,I)-BETASDT *(A(3,I) - DAMP(3,I))
               DA = DA * OMEGA - A(3,I)
               DAMP(3,I) = A(3,I)
               A(3,I)    = A(3,I) + DA
C              2nd order error
               DW =DW+DA*V(3,I)*DT12*WEIGHT(I)
             END DO
            ELSE
#include "vectorize.inc"
             DO N=1,IGRNOD(IGR)%NENTITY
               I=IGRNOD(IGR)%ENTITY(N)
               IF(TAGSLV_RBY(I)/=0) CYCLE
               MVSKW(1,I)= SKEW(1,ISK)*Z_SMS(1,I)
     .                    +SKEW(2,ISK)*Z_SMS(2,I)
     .                    +SKEW(3,ISK)*Z_SMS(3,I)
               MVSKW(2,I)= SKEW(4,ISK)*Z_SMS(1,I)
     .                    +SKEW(5,ISK)*Z_SMS(2,I)
     .                    +SKEW(6,ISK)*Z_SMS(3,I)
               MVSKW(3,I)= SKEW(7,ISK)*Z_SMS(1,I)
     .                    +SKEW(8,ISK)*Z_SMS(2,I)
     .                    +SKEW(9,ISK)*Z_SMS(3,I)
               VSKW(1,I)= SKEW(1,ISK)*V(1,I)
     .                   +SKEW(2,ISK)*V(2,I)
     .                   +SKEW(3,ISK)*V(3,I)
               VSKW(2,I)= SKEW(4,ISK)*V(1,I)
     .                   +SKEW(5,ISK)*V(2,I)
     .                   +SKEW(6,ISK)*V(3,I)
               VSKW(3,I)= SKEW(7,ISK)*V(1,I)
     .                   +SKEW(8,ISK)*V(2,I)
     .                   +SKEW(9,ISK)*V(3,I)
               RSKW(1,I)= SKEW(1,ISK)*A(1,I)
     .                   +SKEW(2,ISK)*A(2,I)
     .                   +SKEW(3,ISK)*A(3,I)
               RSKW(2,I)= SKEW(4,ISK)*A(1,I)
     .                   +SKEW(5,ISK)*A(2,I)
     .                   +SKEW(6,ISK)*A(3,I)
               RSKW(3,I)= SKEW(7,ISK)*A(1,I)
     .                   +SKEW(8,ISK)*A(2,I)
     .                   +SKEW(9,ISK)*A(3,I)
               DAMPSKW(1,I)= SKEW(1,ISK)*DAMP(1,I)
     .                      +SKEW(2,ISK)*DAMP(2,I)
     .                      +SKEW(3,ISK)*DAMP(3,I)
               DAMPSKW(2,I)= SKEW(4,ISK)*DAMP(1,I)
     .                      +SKEW(5,ISK)*DAMP(2,I)
     .                      +SKEW(6,ISK)*DAMP(3,I)
               DAMPSKW(3,I)= SKEW(7,ISK)*DAMP(1,I)
     .                      +SKEW(8,ISK)*DAMP(2,I)
     .                      +SKEW(9,ISK)*DAMP(3,I)
             END DO
             DAMPA = DAMPR(3,ND)
             DAMPB = DAMPR(4,ND)
             BETASDT= -MIN(DAMPB,DAMPT)*DT1/MAX(DT1*DT1,EM30)
             OMEGA  = ONE/ (ONE + HALF * DAMPA * DT1)
#include "vectorize.inc"
             DO N=1,IGRNOD(IGR)%NENTITY
              I=IGRNOD(IGR)%ENTITY(N)
              IF(TAGSLV_RBY(I)/=0) CYCLE
              DA = RSKW(1,I) - DAMPA*MVSKW(1,I)
     .                      - BETASDT *(RSKW(1,I) - DAMPSKW(1,I))
              DA = DA * OMEGA - RSKW(1,I)
              DAMPSKW(1,I) = RSKW(1,I)
              RSKW(1,I)    = RSKW(1,I) + DA
C             2nd order error
              DW =DW+DA*VSKW(1,I)*DT12*WEIGHT(I)
             ENDDO
             DAMPA = DAMPR(5,ND)
             DAMPB = DAMPR(6,ND)
             BETASDT= -MIN(DAMPB,DAMPT)*DT1/MAX(DT1*DT1,EM30)
             OMEGA  = ONE/ (ONE + HALF * DAMPA * DT1)
#include "vectorize.inc"
             DO N=1,IGRNOD(IGR)%NENTITY
              I=IGRNOD(IGR)%ENTITY(N)
              IF(TAGSLV_RBY(I)/=0) CYCLE
              DA = RSKW(2,I) - DAMPA*MVSKW(2,I)
     .                      - BETASDT *(RSKW(2,I) - DAMPSKW(2,I))
              DA = DA * OMEGA - RSKW(2,I)
              DAMPSKW(2,I) = RSKW(2,I)
              RSKW(2,I)    = RSKW(2,I) + DA
C             2nd order error
              DW =DW+DA*VSKW(2,I)*DT12*WEIGHT(I)
             ENDDO
             DAMPA = DAMPR(7,ND)
             DAMPB = DAMPR(8,ND)
             BETASDT= -MIN(DAMPB,DAMPT)*DT1/MAX(DT1*DT1,EM30)
             OMEGA  = ONE/ (ONE + HALF * DAMPA * DT1)
#include "vectorize.inc"
             DO N=1,IGRNOD(IGR)%NENTITY
              I=IGRNOD(IGR)%ENTITY(N)
              IF(TAGSLV_RBY(I)/=0) CYCLE
              DA = RSKW(3,I) - DAMPA*MVSKW(3,I)
     .                      - BETASDT *(RSKW(3,I) - DAMPSKW(3,I))
              DA = DA * OMEGA - RSKW(3,I)
              DAMPSKW(3,I) = RSKW(3,I)
              RSKW(3,I)    = RSKW(3,I) + DA
C             2nd order error
              DW =DW+DA*VSKW(3,I)*DT12*WEIGHT(I)
             ENDDO
#include "vectorize.inc"
             DO N=1,IGRNOD(IGR)%NENTITY
               I=IGRNOD(IGR)%ENTITY(N)
               IF(TAGSLV_RBY(I)/=0) CYCLE
               A(1,I)= SKEW(1,ISK)*RSKW(1,I)
     .                +SKEW(4,ISK)*RSKW(2,I)
     .                +SKEW(7,ISK)*RSKW(3,I)
               A(2,I)= SKEW(2,ISK)*RSKW(1,I)
     .                +SKEW(5,ISK)*RSKW(2,I)
     .                +SKEW(8,ISK)*RSKW(3,I)
               A(3,I)= SKEW(3,ISK)*RSKW(1,I)
     .                +SKEW(6,ISK)*RSKW(2,I)
     .                +SKEW(9,ISK)*RSKW(3,I)
               DAMP(1,I)= SKEW(1,ISK)*DAMPSKW(1,I)
     .                   +SKEW(4,ISK)*DAMPSKW(2,I)
     .                   +SKEW(7,ISK)*DAMPSKW(3,I)
               DAMP(2,I)= SKEW(2,ISK)*DAMPSKW(1,I)
     .                   +SKEW(5,ISK)*DAMPSKW(2,I)
     .                   +SKEW(8,ISK)*DAMPSKW(3,I)
               DAMP(3,I)= SKEW(3,ISK)*DAMPSKW(1,I)
     .                   +SKEW(6,ISK)*DAMPSKW(2,I)
     .                   +SKEW(9,ISK)*DAMPSKW(3,I)
             END DO
            END IF
          END IF
         END DO
#include "lockon.inc"
         TFEXT = TFEXT + DW
#include "lockoff.inc"
         IF (IMON>0) CALL STOPTIME(5,1)
        END IF
C
        CALL MY_BARRIER
C-----------------------------------
       IF (ISTAT==1.OR.ISTAT==3) THEN
!$OMP SINGLE
         OMEGA  = BETATE * DT12
         UOMEGA = ONE - OMEGA
         DOMEGA = TWO*BETATE
         DW = ZERO
        IF(ISTATG==0)THEN
          DO J= 1,3
             DO I=1,NUMNOD
               IF(TAGSLV_RBY(I)/=0) CYCLE
               DA = A(J,I)
               A(J,I)  = UOMEGA*A(J,I) -DOMEGA*Z_SMS(J,I)  
               DA = A(J,I) -DA
               DW =DW+DA*V(J,I)*DT12*WEIGHT(I)
             END DO
          END DO
          ELSE
         IF(ISTATG<0)THEN
          ISTATG=NGR2USR(-ISTATG,IGRNOD,NGRNOD)
         ENDIF
         DO J= 1,3
#include "vectorize.inc"
             DO N=1,IGRNOD(ISTATG)%NENTITY
               I=IGRNOD(ISTATG)%ENTITY(N)
               IF(TAGSLV_RBY(I)/=0) CYCLE
               DA = A(J,I)
               A(J,I)  = UOMEGA*A(J,I) -DOMEGA*Z_SMS(J,I)  
               DA = A(J,I) -DA
               DW =DW+DA*V(J,I)*DT12*WEIGHT(I)
             END DO
         END DO
        END IF !(ISTATG==0)THEN
#include "lockon.inc"
         TFEXT = TFEXT + DW
#include "lockoff.inc"
!$OMP END SINGLE
C
      CALL MY_BARRIER
       END IF !(ISTAT==3) THEN
C
      END IF

C-----------------------------------
C RBE2
C-----------------------------------
      IF (NRBE2>0.OR.R2SIZE>0) THEN      
        IF(ITASK==0)THEN
          CALL SMS_RBE_CNDS(
     1     IRBE2 ,LRBE2 ,X      ,A      ,AM     ,
     1     MS    ,IN    ,SKEW   ,WEIGHT ,IAD_RBE2,
     2     FR_RBE2M,NMRBE2)
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
     1      IRBE3 ,LRBE3  ,X        ,A       ,FRBE3    ,
     2      SKEW  ,WEIGHT ,IAD_RBE3M,FR_RBE3M,FR_RBE3MP,
     3      RRBE3 ,RRBE3_PON ,R3SIZE)
        END IF
C
        CALL MY_BARRIER
C
      END IF
C-----------------------------------
C     CONDITIONS AUX LIMITES
      CALL SMS_THBCS(NODFT1_SMS,NODLT1_SMS,INDX1_SMS,ICODT  ,ICODR    ,
     2               ISKEW  ,SKEW      ,A         ,AM       ,FTHREAC  ,
     3               NODREAC,CPTREAC)
C
      CALL SMS_BCS(NODFT1_SMS,NODLT1_SMS,INDX1_SMS,ICODT  ,ISKEW ,
     2             SKEW      ,A         ,NODLT1_SMS)
C
      IF(IRODDL/=0)
     1  CALL SMS_BCS(NODFT1_SMS,NODLT1_SMS,INDX1_SMS,ICODR  ,ISKEW ,
     2               SKEW      ,AM        ,NODLT1_SMS)
C
      IF (NBCSCYC>0) CALL SMS_BCSCYC(IBCSCYC,LBCSCYC,SKEW,X,A)
C      
      CALL MY_BARRIER
C
C-----------------------------------
C
C     PREC_SMS utilise pour stocker la diagonale vraie (cf rbodies)
      PREC_SMS(NODFT:NODLT)=DIAG_SMS(NODFT:NODLT)
C
      CALL MY_BARRIER()
C 
      IF(NRBODY/=0)THEN
C
!$OMP DO SCHEDULE(DYNAMIC,1)
        DO M =1,NRBODY
          DO K = 1, 6
            RBY6(1,K,M) = ZERO
          END DO
C
          MSR=NPBY(1,M)
          IF(MSR < 0) CYCLE
C
          IF(TAGMSR_RBY_SMS(MSR) /= 0) THEN
            RBY6(1,1,M)=DIAG_SMS(MSR)*WEIGHT(MSR)
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
            RBY6(1,1,M)=RBY6(1,1,M)+DIAG_SMS(I)
          END IF
        END IF
       END DO
!$OMP END SINGLE

       IF (NSPMD > 1) THEN
!$OMP SINGLE
         NRBDIM=1
         CALL SPMD_EXCH_A_RB6(
     1     NRBDIM,IAD_RBY,FR_RBY6,IAD_RBY(NSPMD+1),RBY6)
!$OMP END SINGLE
       END IF

!$OMP DO SCHEDULE(DYNAMIC,1)
        DO M =1,NRBODY
C
          MSR=NPBY(1,M)
C
          IF(MSR < 0) CYCLE
C
          IF(TAGMSR_RBY_SMS(MSR) /= 0) THEN
            PREC_SMS(MSR)=RBY6(1,1,M)
          END IF
C
        END DO
!$OMP  END DO
C 
        DO N=NODFT1_SMS,NODLT1_SMS
         I=INDX1_SMS(N)
         M=TAGSLV_RBY_SMS(I)
         IF(M /= 0)THEN
           MSR=NPBY(1,M)
           PREC_SMS(I)=PREC_SMS(MSR)
         END IF
        END DO
C
        CALL MY_BARRIER()
C 
      END IF
C-----------------------------------
C
      IF(NFXVEL > 0)THEN
        IF(ITASK==0)THEN
          IT=0
          CALL SMS_FIXVEL(IBFV   ,A       ,V        ,NPC    ,TF     ,
     2                    VEL    ,MS      ,X        ,SKEW   ,SENSOR_TAB,
     3                    WEIGHT  ,D        ,IFRAME ,XFRAME ,NSENSOR ,
     4                    IT     ,PREC_SMS,NODXI_SMS,CPTREAC,NODREAC,
     5                    FTHREAC,AM      ,VR       ,DR     ,IN     ,
     6                    RBY    )
        END IF
C
        CALL MY_BARRIER
C
      END IF
C-----------------------------------
      IF(NJOINT > 0)THEN
        CALL SMS_CJOINT_0(A    ,AM    ,V ,VR,X    ,
     2                    FSAV ,LJOINT,MS,IN,IADCJ,
     3                    FR_CJ,CJWORK,TAG_LNK_SMS(NRLINK+NLINK+1),
     .                          PREC_SMS,ITASK)
C
        CALL MY_BARRIER
C
      END IF
C-----------------------------------
      IF(NADMESH/=0)THEN
        IF(ITASK==0)THEN
          CALL SMS_ADMESH_0(A, PREC_SMS, IXC, IXTG,SH4TREE  ,
     .                        SH3TREE  )
        END IF
C
        CALL MY_BARRIER
C
      END IF
C-----------------------------------
      CALL SMS_PCG(NODFT  ,NODLT    ,NNZ_SMS,JAD_SMS  ,
     2            JDI_SMS ,DIAG_SMS ,LT_SMS ,A      ,ISP      ,
     3            X_SMS   ,P_SMS    ,Z_SMS  ,Y_SMS  ,PREC_SMS ,
     4            NODFT1_SMS,NODLT1_SMS,INDX1_SMS,ICODT ,ICODR ,
     5            ISKEW   ,SKEW     ,ITASK  ,NODXI_SMS,IAD_ELEM,
     6            FR_ELEM ,WEIGHT   ,IBFV   ,VEL      ,NPC     ,
     7            TF       ,V      ,X       ,D        ,SENSOR_TAB,
     8            IFRAME ,XFRAME  ,JADI_SMS ,JDII_SMS ,NSENSOR   ,
     9            LTI_SMS  ,FR_SMS ,FR_RMS  ,LIST_SMS ,LIST_RMS,
     A            MSKYI_FI_SMS,VFI ,ISKYI_SMS,MSKYI_SMS,
     B            RES_SMS  ,ILINK  ,RLINK   ,FR_RL    ,FRL6    ,
     C            NNLINK   ,LNLINK ,FR_LL   ,FNL6     ,MS      ,
     D            TAG_LNK_SMS,ITAB ,FSAV    ,LJOINT   ,IADCJ   ,
     E            FR_CJ    ,CJWORK ,FRL     ,FNL      ,NPRW    ,
     F            LPRW     ,RWBUF  ,RWSAV   ,FOPT     ,FR_WALL ,
     G            IRWL_WORK,NRWL_SMS,FREA   ,INTSTAMP ,IMV     ,
     H            MV       ,MV6     ,MW6    ,KINET    ,IXC     ,
     I            IXTG     ,SH4TREE ,SH3TREE,CPTREAC  ,NODREAC ,
     J            FTHREAC  ,FRWL6  ,AM       ,VR      ,
     K            DR       ,IN      ,RBY    ,NPBY     ,LPBY    ,
     L      TAGMSR_RBY_SMS ,IRBE2   ,LRBE2  ,IAD_RBE2 ,FR_RBE2M,
     M            NMRBE2   ,R2SIZE  ,IRBE3  ,LRBE3    ,FRBE3   ,
     N            IAD_RBE3M ,FR_RBE3M ,FR_RBE3MP,RRBE3,RRBE3_PON,
     O            PREC_SMS3,DIAG_SMS3,IAD_RBY  ,FR_RBY6    ,RBY6,
     P            TAGSLV_RBY_SMS,R3SIZE,NODFT2_SMS,NODLT2_SMS,INDX2_SMS,
     Q            NODII_SMS,IBCSCYC ,LBCSCYC  )
C
      CALL MY_BARRIER
C
c      DT15=HALF*DT1
c      DT25=HALF*DT2
c      TFEXTT=ERRTE_SMS
c      ERRTET =ZERO
c      DO N=NODFT1_SMS,NODLT1_SMS
c        I = INDX1_SMS(N)
c        VX = V(1,I)+DT05*A(1,I)
c        VY = V(2,I)+DT05*A(2,I)
c        VZ = V(3,I)+DT05*A(3,I)
c        MVX = RES_SMS(1,I)
c        MVY = RES_SMS(2,I)
c        MVZ = RES_SMS(3,I)
c        DW = (VX*MVX + VY*MVY + VZ*MVZ)*WEIGHT(I)
c        TFEXTT =TFEXTT + DT15*DW
c        ERRTET =ERRTET  + DT25*DW
c      END DO
c#include "lockon.inc"
c      TFEXT=TFEXT+TFEXTT
c      ERRTE_SMS=ERRTE_SMS+ERRTET
c#include "lockoff.inc"
c
c      CALL MY_BARRIER
c
C
      IF(NJOINT > 0)THEN
        CALL SMS_CJOINT_2(A    ,AM    ,V ,VR,X    ,
     2                    LJOINT,MS,IN,IADCJ,FR_CJ,
     3                    CJWORK,TAG_LNK_SMS(NRLINK+NLINK+1),ITASK)
C
        CALL MY_BARRIER
C
      END IF
C-----------------------------------
C
      DO N=NODFT1_SMS,NODLT1_SMS
        I=INDX1_SMS(N)
        A(1,I) = A(1,I)*MS(I)
        A(2,I) = A(2,I)*MS(I)
        A(3,I) = A(3,I)*MS(I)
      ENDDO
C
      CALL MY_BARRIER
C
      IF(ITASK==0) DEALLOCATE(CJWORK, FREA, IRWL_WORK, 
     .      LIST_SMS,LIST_RMS,MSKYI_FI_SMS,VFI)
      IF(IPARIT/=0)THEN
        DEALLOCATE(IMV, MV, MV6)
        IF(ITASK==0)DEALLOCATE(MW6)
      END IF
C
      RETURN
      END
