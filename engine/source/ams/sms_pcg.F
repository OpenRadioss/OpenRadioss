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
Chd|  SMS_PCG                       source/ams/sms_pcg.F          
Chd|-- called by -----------
Chd|        SMS_MASS_SCALE_2              source/ams/sms_mass_scale_2.F 
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SMS_ADMESH_1                  source/ams/sms_admesh.F       
Chd|        SMS_ADMESH_2                  source/ams/sms_admesh.F       
Chd|        SMS_BCS                       source/ams/sms_bcs.F          
Chd|        SMS_BCSCYC                    source/ams/sms_bcscyc.F       
Chd|        SMS_CHECK                     source/ams/sms_fsa_inv.F      
Chd|        SMS_CJOINT_1                  source/ams/sms_cjoint.F       
Chd|        SMS_FIXVEL                    source/ams/sms_fixvel.F       
Chd|        SMS_INISI                     source/ams/sms_proj.F         
Chd|        SMS_INIST                     source/ams/sms_proj.F         
Chd|        SMS_INIX                      source/ams/sms_proj.F         
Chd|        SMS_MAV_LT                    source/ams/sms_pcg.F          
Chd|        SMS_PRO_P                     source/ams/sms_proj.F         
Chd|        SMS_RBE3T1                    source/ams/sms_rbe3.F         
Chd|        SMS_RBE3T2                    source/ams/sms_rbe3.F         
Chd|        SMS_RBE3_PREC                 source/ams/sms_rbe3.F         
Chd|        SMS_RBE_ACCL                  source/ams/sms_rbe2.F         
Chd|        SMS_RBE_CNDS                  source/ams/sms_rbe2.F         
Chd|        SMS_RBE_CORR                  source/ams/sms_rbe2.F         
Chd|        SMS_RBE_PREC                  source/ams/sms_rbe2.F         
Chd|        SMS_RGWAL_0                   source/ams/sms_rgwal0.F       
Chd|        SMS_RLINK10                   source/ams/sms_rlink.F        
Chd|        SMS_RLINK11                   source/ams/sms_rlink.F        
Chd|        SMS_UPDST                     source/ams/sms_proj.F         
Chd|        SPMD_EXCH_A_RB6               source/mpi/kinematic_conditions/spmd_exch_a_rb6.F
Chd|        SPMD_GLOB_DPSUM9              source/mpi/interfaces/spmd_th.F
Chd|        SPMD_GLOB_DSUM9               source/mpi/interfaces/spmd_th.F
Chd|        SPMD_RBCAST                   source/mpi/generic/spmd_rbcast.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        SUM_6_FLOAT                   source/system/parit.F         
Chd|        INTSTAMP_MOD                  share/modules/intstamp_mod.F  
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        SMS_PCG_PROJ                  share/modules/sms_mod.F       
Chd|====================================================================
      SUBROUTINE SMS_PCG (NODFT ,NODLT    ,NNZ      ,IADK     ,
     2                JDIK      ,DIAG_SMS ,LT_K     ,R        ,ISP     ,
     3                X_SMS     ,P_SMS    ,Z_SMS    ,Y_SMS    ,PREC_SMS,
     4                NODFT1_SMS,NODLT1_SMS,INDX1_SMS,ICODT   ,ICODR   ,
     5                ISKEW     ,SKEW     ,ITASK    ,NODNX_SMS,IAD_ELEM,
     6                FR_ELEM   ,WEIGHT   ,IBFV     ,VEL      ,NPC     ,
     7                TF        ,V        ,X        ,D        ,SENSOR_TAB,
     8                IFRAME    ,XFRAME   ,JADI_SMS ,JDII_SMS ,NSENSOR   ,
     9                LTI_SMS   ,FR_SMS   ,FR_RMS   ,LIST_SMS ,LIST_RMS,
     A                MSKYI_FI_SMS,VFI    ,ISKYI_SMS,MSKYI_SMS         ,
     B                RES_SMS   ,ILINK    ,LLINK    ,FR_RL    ,FRL6    ,
     C                NNLINK    ,LNLINK   ,FR_LL    ,FNL6     ,MS      ,
     D                TAG_LNK_SMS,ITAB    ,FSAV     ,LJOINT   ,IADCJ   ,
     E                FR_CJ     ,CJWORK   ,FRL      ,FNL      ,NPRW    ,
     F                LPRW      ,RWBUF    ,RWSAV    ,FOPT     ,FR_WALL ,
     G                IRWL_WORK ,NRWL_SMS ,FREA     ,INTSTAMP ,IMV     ,
     H                MV        ,MV6      ,MW6      ,KINET    ,IXC     ,
     I                IXTG      ,SH4TREE  ,SH3TREE  ,CPTREAC  ,NODREAC ,
     J                FTHREAC   ,FRWL6    ,AM       ,VR      ,
     K                DR        ,IN       ,RBY      ,NPBY     ,LPBY    ,
     L             TAGMSR_RBY_SMS,IRBE2   ,LRBE2    ,IAD_RBE2 ,FR_RBE2M,
     M                NMRBE2    ,R2SIZE   ,IRBE3  ,LRBE3    ,FRBE3     ,
     N                IAD_RBE3M,FR_RBE3M ,FR_RBE3MP,RRBE3   ,RRBE3_PON ,
     O                PREC_SMS3,DIAG_SMS3,IAD_RBY  ,FR_RBY6 ,RBY6      ,
     P                TAGSLV_RBY_SMS,R3SIZE,NODFT2_SMS,NODLT2_SMS,INDX2_SMS,
     Q                NODII_SMS ,IBCSCYC ,LBCSCYC )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE INTSTAMP_MOD
        USE SENSOR_MOD
        USE SMS_PCG_PROJ
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
#include "comlock.inc"
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
#include      "parit_c.inc"
#include      "remesh_c.inc"
#include      "scr03_c.inc"
#include      "scr07_c.inc"
#include      "sms_c.inc"
#include      "task_c.inc"
#include      "timeri_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C----------resol [M]{X}={F}---------
        INTEGER  NODFT, NODLT, IADK(*), JDIK(*), NNZ, ISP,NSENSOR,
     .           NODFT1_SMS,NODLT1_SMS,INDX1_SMS(*),
     .           ICODT(*), ICODR(*), ISKEW(*), ITASK, NODNX_SMS(*),
     .           IAD_ELEM(2,NSPMD+1) ,FR_ELEM(*), WEIGHT(*),
     .           NPC(*), IBFV(NIFV,*),IFRAME(LISKN,*),
     .           JADI_SMS(*), JDII_SMS(*),
     .           FR_SMS(NSPMD+1), FR_RMS(NSPMD+1),
     .           LIST_SMS(*), LIST_RMS(*),ISKYI_SMS(*),
     .           ILINK(*), LLINK(*), FR_RL(NSPMD+2,*), NNLINK(10,*),
     .           LNLINK(*), FR_LL(NSPMD+2,*), TAG_LNK_SMS(*), ITAB(*),
     .           LJOINT(*), FR_CJ(*), IADCJ(*),
     .           NPRW(*), LPRW(*), FR_WALL(*), IRWL_WORK(*), NRWL_SMS(*),
     .           IMV(*), KINET(*),CPTREAC,NODREAC(*),
     .           IXC(NIXC,*), IXTG(NIXTG,*),
     .           SH4TREE(KSH4TREE,*), SH3TREE(KSH3TREE,*),
     .           NPBY(NNPBY,*), LPBY(*), TAGMSR_RBY_SMS(*),
     .           IRBE2(NRBE2L,*), LRBE2(*), IAD_RBE2(*),
     .           FR_RBE2M(*), NMRBE2, R2SIZE, IRBE3(NRBE3L,*), LRBE3(*),
     .           IAD_RBE3M(*) ,FR_RBE3M(*) ,FR_RBE3MP(*),
     .           FR_RBY6(*),IAD_RBY(*), TAGSLV_RBY_SMS(*),R3SIZE,
     .           NODFT2_SMS,NODLT2_SMS,INDX2_SMS(*),NODII_SMS(*),
     .           IBCSCYC(*) ,LBCSCYC(*)
C     REAL
        my_real
     .    DIAG_SMS(*), LT_K(*) ,R(3,*),
     .    X_SMS(3,*), P_SMS(3,*), Y_SMS(3,*), Z_SMS(3,*), PREC_SMS(*),
     .    SKEW(*), V(3,*), X(3,*), D(3,*), TF(*), VEL(LFXVELR,*),
     .    XFRAME(NXFRAME,*), LTI_SMS(*), RES_SMS(3,*),
     .    MS(*), FSAV(NTHVKI,*), CJWORK(*), FRL(*), FNL(*),
     .    RWBUF(*), RWSAV(*), FOPT(*), FREA(3,*),RBID,
     .    MSKYI_FI_SMS(*), MSKYI_SMS(*), VFI(*), MV(*),FTHREAC(6,*),
     .    AM(3,*), VR(3,*), DR(3,*), IN(*), RBY(NRBY,*),
     .    FRBE3(*), RRBE3(*),
     .    PREC_SMS3(3,NUMNOD), DIAG_SMS3(3,NUMNOD)
        DOUBLE PRECISION FRL6(*), FNL6(*), MV6(*), MW6(*), FRWL6(*),
     .                   RRBE3_PON(*)
        DOUBLE PRECISION RBY6(8,6,NRBYKIN)
        TYPE(INTSTAMP_DATA) INTSTAMP(*)
        TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) ,INTENT(IN) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, IT, TOTIT, NLIM, N, L, K, LLT, IDOWN, J, IFLAG, IACT,
     .          NCPRIA, M, MSR, IAD, NSN, KI, NRBDIM
        my_real
     .     ALPHA, BETA, TOLN,
     .     ST , R2T, R02T, G0T, G1T, RES_OLD,
     .     P1, P2, P3, DT05,
     .     XX, YY, ZZ, VRX, VRY, VRZ, V1, V2, V3, GX, GY, GZ, A1, A2, A3
        my_real
     .     R2(MVSIZ), G(MVSIZ), S(MVSIZ), R02(MVSIZ)
        my_real
     .     RBUF(2)
        DOUBLE PRECISION R6T(6), G6T(6), S6T(6), DBUF(12)
C--------------INITIALISATION--------------------------
        IF(IMONM>0.AND.ITASK==0)CALL STARTIME(61,1)

        NCPRIA=ABS(NCPRISMS)
        NLIM  =MAX(NSMSPCG,2)
!$OMP SINGLE
        NUPDTL_SMS=-1
!$OMP END SINGLE
#ifdef __INTEL_LLVM_COMPILER
!$omp barrier
#endif

        IACT=0
        IT  =0
        TOTIT=0
C-------------IT=0--------
C------X(I)=ZERO--------
C
C warning : PREC_SMS == DIAG_SMS at THIS STAGE
C
C comment faire rbody secnd de rbe (cf diag) ?
        IF(NRBE2+R2SIZE+NRBE3/=0)THEN
          DO N=NODFT1_SMS,NODLT1_SMS
            I=INDX1_SMS(N)
            DIAG_SMS3(1,I)=PREC_SMS(I)
            DIAG_SMS3(2,I)=PREC_SMS(I)
            DIAG_SMS3(3,I)=PREC_SMS(I)
          END DO
        END IF
C
C warning : PREC_SMS == 1/DIAG_SMS after THIS STAGE
        DO N=NODFT1_SMS,NODLT1_SMS
          I=INDX1_SMS(N)
          IF(PREC_SMS(I)==ZERO)THEN
C reset (spotflag=1 forces non remises a zero)
C        PREC_SMS(I)=ZERO
            R(1,I)=ZERO
            R(2,I)=ZERO
            R(3,I)=ZERO
          ELSE
            PREC_SMS(I)=ONE/PREC_SMS(I)
          END IF
        ENDDO
C-----------------------------------
C RBE2
C-----------------------------------
        IF(NRBE2+R2SIZE+NRBE3/=0)THEN
          IF (NRBE2>0.OR.R2SIZE>0) THEN
C
            CALL MY_BARRIER
C
            IF(ITASK==0)THEN
              CALL SMS_RBE_PREC(
     1         IRBE2 ,LRBE2   ,DIAG_SMS,MS    ,DIAG_SMS3,
     1         SKEW  ,WEIGHT  ,IAD_RBE2,FR_RBE2M ,NMRBE2)
            END IF
          END IF
C-----------------------------------
C RBE3
C-----------------------------------
          IF (NRBE3>0)THEN
C
            CALL MY_BARRIER
C
            IF(ITASK==0)THEN
              CALL SMS_RBE3_PREC(
     1          IRBE3 ,LRBE3  ,X      ,DIAG_SMS ,DIAG_SMS3,
     2          FRBE3 ,SKEW   ,WEIGHT ,IAD_RBE3M,FR_RBE3M ,
     3          FR_RBE3MP,RRBE3 ,RRBE3_PON ,R3SIZE)
            END IF
          END IF
C
          CALL MY_BARRIER
C
          DO N=NODFT1_SMS,NODLT1_SMS
            I=INDX1_SMS(N)
            IF(DIAG_SMS3(1,I)==ZERO)THEN
              PREC_SMS3(1,I)=ZERO
            ELSE
              PREC_SMS3(1,I)=ONE/DIAG_SMS3(1,I)
            END IF
            IF(DIAG_SMS3(2,I)==ZERO)THEN
              PREC_SMS3(2,I)=ZERO
            ELSE
              PREC_SMS3(2,I)=ONE/DIAG_SMS3(2,I)
            END IF
            IF(DIAG_SMS3(3,I)==ZERO)THEN
              PREC_SMS3(3,I)=ZERO
            ELSE
              PREC_SMS3(3,I)=ONE/DIAG_SMS3(3,I)
            END IF
          END DO
C
        END IF ! IF(NRBE2+NRBE3/=0)THEN
C-----------------------------------
C     LIENS RIGIDES ENTRE NOEUDS : REMONTEE FORCES
C---- // ----------------------------
        IF(NRLINK+NLINK+NJOINT+NADMESH > 0)THEN
C
          CALL MY_BARRIER
C
          IDOWN=0
          IF(NRLINK>0)CALL SMS_RLINK10(
     1     MS    ,R ,ILINK ,LLINK,SKEW,
     2     FR_RL ,WEIGHT,FRL6  ,IDOWN,TAG_LNK_SMS,
     3     ITAB  ,FRL   )
C
          IF(NLINK>0) CALL SMS_RLINK11(
     1     MS    ,R ,NNLINK,LNLINK,SKEW  ,
     2     FR_LL ,WEIGHT,FNL6  ,X     ,XFRAME,
     3     V     ,IDOWN ,TAG_LNK_SMS,ITAB,FNL)
C
          IF(NJOINT > 0)
     .    CALL SMS_CJOINT_1(R     ,DIAG_SMS,LJOINT,IADCJ,FR_CJ,
     .               CJWORK,IDOWN ,TAG_LNK_SMS(NRLINK+NLINK+1),ITASK)
C
C        IF(NADMESH/=0)THEN
C          IF(ITASK==0)THEN
C            CALL SMS_ADMESH_1(R, DIAG_SMS, IXC, IXTG,SH4TREE  ,
C    .                        SH3TREE  ,NODNX_SMS)
C          END IF
C        END IF
C
          CALL MY_BARRIER
C
        END IF
C
C------PCG(PROJECTION)----place here to have the same reference value
        IF (M_VS_SMS > 0 ) THEN
          IF(IMONM>0.AND.ITASK==0)CALL STOPTIME(61,1)
          IF(IMONM>0.AND.ITASK==0)CALL STARTIME(70,1)
C
          CALL SMS_INISI(
     1             IADK  ,JDIK  ,DIAG_SMS,LT_K  ,ITASK ,
     2             NODFT1_SMS,NODLT1_SMS,INDX1_SMS,NODNX_SMS,IAD_ELEM ,
     3             FR_ELEM   ,WEIGHT    ,JADI_SMS ,JDII_SMS ,LTI_SMS  ,
     4             ISKYI_SMS ,MSKYI_SMS ,FR_SMS   ,FR_RMS   ,LIST_SMS ,
     5             LIST_RMS  ,MSKYI_FI_SMS ,VFI   ,IMV      ,MV       ,
     6             MV6       ,MW6       ,MS       ,NODFT    ,NODLT    ,
     7             PREC_SMS  ,KINET     )
C      /---------------/
          CALL MY_BARRIER
C      /---------------/
          CALL SMS_INIST(
     1             IADK  ,JDIK  ,DIAG_SMS,LT_K  ,ITASK ,
     2             NODFT1_SMS,NODLT1_SMS,INDX1_SMS,NODNX_SMS,IAD_ELEM ,
     3             FR_ELEM   ,WEIGHT    ,JADI_SMS ,JDII_SMS ,LTI_SMS  ,
     4             ISKYI_SMS ,MSKYI_SMS ,FR_SMS   ,FR_RMS   ,LIST_SMS ,
     5             LIST_RMS  ,MSKYI_FI_SMS ,VFI   ,IMV      ,MV       ,
     6             MV6       ,MW6       ,MS       ,NODFT    ,NODLT    )
C      /---------------/
          CALL MY_BARRIER
C      /---------------/
          CALL SMS_INIX(NODFT,NODLT,NUMNOD,X_SMS,R   ,WEIGHT,ITASK ,
     .                  DIAG_SMS   )
C
          IF(IMONM>0.AND.ITASK==0)CALL STOPTIME(70,1)
          IF(IMONM>0.AND.ITASK==0)CALL STARTIME(61,1)
C
        ELSE
C
          DO N=NODFT1_SMS,NODLT1_SMS
            I=INDX1_SMS(N)
C
            X_SMS(1,I) = R(1,I)*PREC_SMS(I)
            X_SMS(2,I) = R(2,I)*PREC_SMS(I)
            X_SMS(3,I) = R(3,I)*PREC_SMS(I)
          ENDDO
        END IF
C-----------------------------------
C RBE3
C-----------------------------------
        IF (NRBE3>0)THEN
C
          CALL MY_BARRIER
C
          IF(ITASK==0)THEN
            CALL SMS_RBE3T2(IRBE3 ,LRBE3 ,X    ,X_SMS ,FRBE3 ,
     2                      SKEW  ,R     ,PREC_SMS3   )
          END IF
        END IF
C-----------------------------------
C RBE2
C-----------------------------------
        IF (NRBE2>0) THEN
C
          CALL MY_BARRIER
C
          IF(ITASK==0)THEN
            CALL SMS_RBE_ACCL(
     1       IRBE2  ,LRBE2  ,R      ,X_SMS  ,PREC_SMS3 ,
     1       SKEW   ,WEIGHT ,IAD_RBE2  ,FR_RBE2M,NMRBE2)
          END IF
C
        END IF
C-----------------------------------
C      LIENS RIGIDES ENTRE NOEUDS : PROJETTE X_SMS
C---- // ----------------------------
        IF(NRLINK+NLINK+NJOINT+NADMESH > 0)THEN
C
          CALL MY_BARRIER
C
          IDOWN=1
          IF(NRLINK>0)CALL SMS_RLINK10(
     1     MS    ,X_SMS ,ILINK ,LLINK,SKEW,
     2     FR_RL ,WEIGHT,FRL6  ,IDOWN,TAG_LNK_SMS,
     3     ITAB  ,FRL   )
C
          IF(NLINK>0) CALL SMS_RLINK11(
     1     MS    ,X_SMS ,NNLINK,LNLINK,SKEW  ,
     2     FR_LL ,WEIGHT,FNL6  ,X     ,XFRAME,
     3     V     ,IDOWN ,TAG_LNK_SMS,ITAB,FNL)
C
          IF(NJOINT > 0)
     .    CALL SMS_CJOINT_1(X_SMS ,DIAG_SMS,LJOINT,IADCJ,FR_CJ,
     .               CJWORK,IDOWN ,TAG_LNK_SMS(NRLINK+NLINK+1),ITASK)
C
          IF(NADMESH/=0)THEN
            CALL SMS_ADMESH_2(X_SMS, DIAG_SMS, IXC, IXTG,SH4TREE  ,
     .                          SH3TREE  ,ITASK)
          END IF
        END IF
C
        IF(NRWALL > 0)THEN
C
          CALL MY_BARRIER
C
C detect impacts
          IFLAG=0
          CALL SMS_RGWAL_0(IFLAG ,X    ,V      ,RWBUF   ,LPRW  ,
     2    NPRW     ,MS   ,FSAV(1,NINTER+1),FR_WALL ,FOPT ,
     3    RWSAV    ,WEIGHT ,IRWL_WORK     ,NRWL_SMS,FRWL6,
     4    X_SMS    ,RBID   ,RBID   ,RBID   )
C
          CALL MY_BARRIER
C
C project x_sms
          IFLAG=1
          CALL SMS_RGWAL_0(IFLAG ,X    ,V      ,RWBUF   ,LPRW  ,
     2    NPRW     ,MS   ,FSAV(1,NINTER+1),FR_WALL ,FOPT ,
     3    RWSAV    ,WEIGHT ,IRWL_WORK     ,NRWL_SMS,FRWL6,
     4    X_SMS    ,RBID   ,RBID   ,RBID   )
        END IF
C
        IF(NADMESH/=0)THEN
C
          Y_SMS(1:3,NODFT:NODLT)=ZERO
          Z_SMS(1:3,NODFT:NODLT)=ZERO
C
          CALL MY_BARRIER
C
        END IF
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
              X_SMS(1,I)=X_SMS(1,MSR)
              X_SMS(2,I)=X_SMS(2,MSR)
              X_SMS(3,I)=X_SMS(3,MSR)
            END IF
          END DO
C
          CALL MY_BARRIER()
C
        END IF
C-----------------------------------
 10     CONTINUE
C-----------------------------------
C
        CALL MY_BARRIER
C
C-----------------------------------
        IF(IMONM>0.AND.ITASK==0)CALL STOPTIME(61,1)
        CALL SMS_MAV_LT(
     1             NODFT   ,NODLT  ,NUMNOD ,IADK  ,JDIK  ,
     2             ITASK   ,DIAG_SMS,LT_K   ,X_SMS  ,Z_SMS ,
     3             NODFT1_SMS,NODLT1_SMS,INDX1_SMS,NODNX_SMS,IAD_ELEM ,
     4             FR_ELEM   ,WEIGHT    ,JADI_SMS ,JDII_SMS ,LTI_SMS  ,
     5             ISKYI_SMS ,MSKYI_SMS ,FR_SMS   ,FR_RMS   ,LIST_SMS ,
     6             LIST_RMS  ,MSKYI_FI_SMS ,VFI   ,IMV      ,MV      ,
     7             MV6       ,MW6       ,NODFT2_SMS,NODLT2_SMS,INDX2_SMS,
     8             NODII_SMS )
C
        IF(IMONM>0.AND.ITASK==0)CALL STARTIME(61,1)
C
        IF(IPARIT==0)THEN
          RES0_SMS = ZERO
          G0_SMS   = ZERO
        ELSE
!$OMP SINGLE
          DO K=1,6
            R6SMS(K)=ZERO
            G6SMS(K)=ZERO
          ENDDO
!$OMP END SINGLE
#ifdef __INTEL_LLVM_COMPILER
!$omp barrier
#endif
        END IF
C
        CALL MY_BARRIER
C
        IF(NADMESH/=0)THEN
          IF(ITASK==0)THEN
            CALL SMS_ADMESH_1(Z_SMS, DIAG_SMS, IXC, IXTG,SH4TREE  ,
     .      SH3TREE  ,NODNX_SMS)
          END IF
C
          CALL MY_BARRIER
C
        END IF
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
     1       IRBE2 ,LRBE2 ,X_SMS  ,Z_SMS  ,MS    ,
     1       SKEW   ,WEIGHT ,IAD_RBE2,FR_RBE2M,NMRBE2)
C
            CALL SMS_RBE_CNDS(
     1       IRBE2 ,LRBE2 ,X      ,Z_SMS  ,AM     ,
     1       MS    ,IN    ,SKEW   ,WEIGHT ,IAD_RBE2,
     2       FR_RBE2M,NMRBE2)
C
          END IF
C
        END IF
C-----------------------------------
C RBE3
C-----------------------------------
        IF (NRBE3>0)THEN
C
          CALL MY_BARRIER
C
          IF(ITASK==0)THEN
            CALL SMS_RBE3T1(
     1        IRBE3 ,LRBE3  ,X        ,Z_SMS   ,FRBE3    ,
     2        SKEW  ,WEIGHT ,IAD_RBE3M,FR_RBE3M,FR_RBE3MP,
     3        RRBE3 ,RRBE3_PON ,R3SIZE)
          END IF
        END IF
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
              RBY6(1,1,M)=Z_SMS(1,MSR)*WEIGHT(MSR)
              RBY6(2,1,M)=Z_SMS(2,MSR)*WEIGHT(MSR)
              RBY6(3,1,M)=Z_SMS(3,MSR)*WEIGHT(MSR)
            END IF

          END DO
!$OMP  END DO

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
            END IF
          END DO
!$OMP END SINGLE
#ifdef __INTEL_LLVM_COMPILER
!$omp barrier
#endif

          IF (NSPMD > 1) THEN
!$OMP SINGLE
            NRBDIM=3
            CALL SPMD_EXCH_A_RB6(
     1        NRBDIM,IAD_RBY,FR_RBY6,IAD_RBY(NSPMD+1),RBY6)
!$OMP END SINGLE
#ifdef __INTEL_LLVM_COMPILER
!$omp barrier
#endif
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
!$OMP  END DO
        END IF
C-----------------------------------
        CALL SMS_BCS(NODFT1_SMS,NODLT1_SMS,INDX1_SMS,ICODT  ,ISKEW ,
     2               SKEW      ,Z_SMS     ,NODLT1_SMS )
C-----------------------------------
C /BCS/CYCLIC
C-----------------------------------
        IF (NBCSCYC>0) CALL SMS_BCSCYC(IBCSCYC,LBCSCYC,SKEW,X,Z_SMS)
C-----------------------------------
C      LIENS RIGIDES ENTRE NOEUDS : REMONTEE Z_SMS
C---- // ----------------------------
        IF(NRLINK+NLINK+NJOINT > 0)THEN
C
          CALL MY_BARRIER
C
          IDOWN=0
          IF(NRLINK>0)CALL SMS_RLINK10(
     1     MS    ,Z_SMS ,ILINK ,LLINK,SKEW,
     2     FR_RL ,WEIGHT,FRL6  ,IDOWN,TAG_LNK_SMS,
     3     ITAB  ,FRL   )
C
          IF(NLINK>0) CALL SMS_RLINK11(
     1     MS    ,Z_SMS ,NNLINK,LNLINK,SKEW  ,
     2     FR_LL ,WEIGHT,FNL6  ,X     ,XFRAME,
     3     V     ,IDOWN ,TAG_LNK_SMS,ITAB,FNL)
C
          IF(NJOINT > 0)
     .    CALL SMS_CJOINT_1(Z_SMS ,DIAG_SMS,LJOINT,IADCJ,FR_CJ,
     .               CJWORK,IDOWN ,TAG_LNK_SMS(NRLINK+NLINK+1),ITASK)
        END IF
C
        CALL MY_BARRIER
C
        DO N=NODFT1_SMS,NODLT1_SMS
          I=INDX1_SMS(N)
          RES_SMS(1,I) = R(1,I)-Z_SMS(1,I)
          RES_SMS(2,I) = R(2,I)-Z_SMS(2,I)
          RES_SMS(3,I) = R(3,I)-Z_SMS(3,I)
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
              RES_SMS(1,I)=ZERO
              RES_SMS(2,I)=ZERO
              RES_SMS(3,I)=ZERO
            END IF
          END DO
C
          CALL MY_BARRIER
C
        END IF
C-----------------------------------
        IF(NFXVEL > 0)THEN
C
          CALL MY_BARRIER
C
          IF(ITASK==0)
     .    CALL SMS_FIXVEL(IBFV   ,RES_SMS ,V        ,NPC    ,TF     ,
     2                    VEL    ,DIAG_SMS,X        ,SKEW   ,SENSOR_TAB,
     3                    WEIGHT  ,D        ,IFRAME ,XFRAME ,NSENSOR   ,
     4                    IT+1   ,DIAG_SMS,NODNX_SMS,CPTREAC,NODREAC,
     5                    FTHREAC,AM       ,VR     ,DR      ,IN     ,
     6                    RBY     )
C
          CALL MY_BARRIER
C
        END IF
C
        IF(NRWALL > 0)THEN
C
          CALL MY_BARRIER
C
C project res
          IFLAG=2
          CALL SMS_RGWAL_0(IFLAG ,X    ,V      ,RWBUF   ,LPRW  ,
     2    NPRW     ,MS   ,FSAV(1,NINTER+1),FR_WALL ,FOPT ,
     3    RWSAV    ,WEIGHT ,IRWL_WORK     ,NRWL_SMS,FRWL6,
     4    RBID     ,RES_SMS,RBID   ,RBID   )
C
          CALL MY_BARRIER
C
        END IF
C-----------------------------------
        DO N=NODFT1_SMS,NODLT1_SMS
          I=INDX1_SMS(N)
          Z_SMS(1,I) = RES_SMS(1,I) *PREC_SMS(I)
          Z_SMS(2,I) = RES_SMS(2,I) *PREC_SMS(I)
          Z_SMS(3,I) = RES_SMS(3,I) *PREC_SMS(I)
        ENDDO
C-----------------------------------
C RBE3
C-----------------------------------
        IF (NRBE3>0)THEN
C
          CALL MY_BARRIER
C
          IF(ITASK==0)THEN
            CALL SMS_RBE3T2(IRBE3 ,LRBE3 ,X    ,Z_SMS ,FRBE3 ,
     2                      SKEW  ,RES_SMS     ,PREC_SMS3   )
          END IF
        END IF
C-----------------------------------
C RBE2
C-----------------------------------
        IF (NRBE2>0) THEN
C
          CALL MY_BARRIER
C
          IF(ITASK==0)THEN
            CALL SMS_RBE_ACCL(
     1       IRBE2 ,LRBE2 ,RES_SMS,Z_SMS  ,PREC_SMS3,
     1       SKEW   ,WEIGHT ,IAD_RBE2 ,FR_RBE2M,NMRBE2)
          END IF
C
        END IF
C-----------------------------------
C      LIENS RIGIDES ENTRE NOEUDS : PROJETTE
C---- // ----------------------------
        IF(NRLINK+NLINK+NJOINT+NADMESH > 0)THEN
C
          CALL MY_BARRIER
C
          IDOWN=1
          IF(NRLINK>0)CALL SMS_RLINK10(
     1     MS    ,Z_SMS ,ILINK ,LLINK,SKEW,
     2     FR_RL ,WEIGHT,FRL6  ,IDOWN,TAG_LNK_SMS,
     3     ITAB  ,FRL   )
C
          IF(NLINK>0) CALL SMS_RLINK11(
     1     MS    ,Z_SMS ,NNLINK,LNLINK,SKEW  ,
     2     FR_LL ,WEIGHT,FNL6  ,X     ,XFRAME,
     3     V     ,IDOWN ,TAG_LNK_SMS,ITAB,FNL)
C
          IF(NJOINT > 0)
     .    CALL SMS_CJOINT_1(Z_SMS ,DIAG_SMS,LJOINT,IADCJ,FR_CJ,
     .               CJWORK,IDOWN ,TAG_LNK_SMS(NRLINK+NLINK+1),ITASK)
C
          IF(NADMESH/=0)THEN
            CALL SMS_ADMESH_2(Z_SMS, DIAG_SMS, IXC, IXTG,SH4TREE  ,
     .                          SH3TREE  ,ITASK)
          END IF
C
          CALL MY_BARRIER
C
        END IF
C-----------------------------------
C
        DO N=NODFT1_SMS,NODLT1_SMS,MVSIZ
C
          LLT=MIN(NODLT1_SMS-N+1,MVSIZ)
C
          DO L=1,LLT
            I=INDX1_SMS(N+L-1)
            P_SMS(1,I) = Z_SMS(1,I)
            P_SMS(2,I) = Z_SMS(2,I)
            P_SMS(3,I) = Z_SMS(3,I)
            G(L)  =  (  Z_SMS(1,I)*RES_SMS(1,I)
     .                + Z_SMS(2,I)*RES_SMS(2,I)
     .                + Z_SMS(3,I)*RES_SMS(3,I))
     .                   * WEIGHT(I)
C
C Tolerance wrt RES, not to R (due to kinematic conditions, like RWALLs)
            R2(L) = (  RES_SMS(1,I)*RES_SMS(1,I)
     .               + RES_SMS(2,I)*RES_SMS(2,I)
     .               + RES_SMS(3,I)*RES_SMS(3,I))
     .                   * WEIGHT(I)
          ENDDO
C
          IF(IPARIT==0)THEN
            R02T = ZERO
            G0T  = ZERO
            DO L=1,LLT
              R02T = R02T + R2(L)
              G0T  = G0T  + G(L)
            ENDDO
#include "lockon.inc"
            RES0_SMS=RES0_SMS+R02T
            G0_SMS  =G0_SMS +G0T
#include "lockoff.inc"
          ELSE
            DO K=1,6
              R6T(K) = ZERO
              G6T(K) = ZERO
            ENDDO
            IF(IMONM>0.AND.ITASK==0)CALL STARTIME(62,1)
            CALL SUM_6_FLOAT(1,LLT,R2,R6T,1)
            CALL SUM_6_FLOAT(1,LLT,G,G6T,1)
            IF(IMONM>0.AND.ITASK==0)CALL STOPTIME(62,1)
#include "lockon.inc"
            DO K=1,6
              R6SMS(K)=R6SMS(K)+R6T(K)
              G6SMS(K)=G6SMS(K)+G6T(K)
            ENDDO
#include "lockoff.inc"
          END IF
        ENDDO
C-----------------------------------
C
        CALL MY_BARRIER
C
        IF(NSPMD <= 1)THEN
          IF(IPARIT/=0.AND.ITASK==0)THEN
            RES0_SMS=R6SMS(1)+R6SMS(2)+R6SMS(3)+
     .               R6SMS(4)+R6SMS(5)+R6SMS(6)
            G0_SMS  =G6SMS(1)+G6SMS(2)+G6SMS(3)+
     .               G6SMS(4)+G6SMS(5)+G6SMS(6)
          END IF
        ELSEIF(ITASK==0)THEN     ! communication sur un seul thread
          IF(IPARIT==0)THEN
            IF(IMONM>0) CALL STARTIME(63,1)
            RBUF(1)=RES0_SMS
            RBUF(2)=G0_SMS
            CALL SPMD_GLOB_DSUM9(RBUF,2)
            CALL SPMD_RBCAST(RBUF,RBUF,2,1,0,2)
            RES0_SMS=RBUF(1)
            G0_SMS  =RBUF(2)
            IF(IMONM>0) CALL STOPTIME(63,1)
          ELSE
            IF(IMONM>0) CALL STARTIME(63,1)
            DO K=1,6
              DBUF(K)  =R6SMS(K)
              DBUF(K+6)=G6SMS(K)
            END DO
            CALL SPMD_GLOB_DPSUM9(DBUF,12)
            RBUF(1)  = DBUF(1)+DBUF(2)+DBUF(3)+
     .                 DBUF(4)+DBUF(5)+DBUF(6)
            RBUF(2)  = DBUF(7) +DBUF(8) +DBUF(9)+
     .                 DBUF(10)+DBUF(11)+DBUF(12)
            CALL SPMD_RBCAST(RBUF,RBUF,2,1,0,2)
            RES0_SMS=RBUF(1)
            G0_SMS  =RBUF(2)
            IF(IMONM>0) CALL STOPTIME(63,1)
          END IF
        END IF
C-----------------------------------
C     redescente Pm => Pi
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
              P_SMS(1,I)=P_SMS(1,MSR)
              P_SMS(2,I)=P_SMS(2,MSR)
              P_SMS(3,I)=P_SMS(3,MSR)
            END IF
          END DO
C
          CALL MY_BARRIER()
C
        END IF
C-----------------------------------
C
        CALL MY_BARRIER
C
        IF(IMONM>0.AND.ITASK==0)CALL STOPTIME(61,1)
        IF (RES0_SMS<EM10) GOTO 200
        TOLN=RES0_SMS*TOL_SMS

 100    CONTINUE

        IT = IT +1
        TOTIT = TOTIT + 1

C
C------PCG(PROJECTION)----
        IF (M_VS_SMS > 0 ) THEN
          IF(IMONM>0.AND.ITASK==0)CALL STARTIME(70,1)
C
          CALL SMS_PRO_P(NODFT ,NODLT ,NUMNOD ,P_SMS,WEIGHT,ITASK ,
C z as work array
     .                   Z_SMS ,DIAG_SMS)
C       /---------------/
          CALL MY_BARRIER
C       /---------------/
C
          IF(IMONM>0.AND.ITASK==0)CALL STOPTIME(70,1)
        END IF
C
c      CALL MY_BARRIER
C
        CALL SMS_MAV_LT(
     1            NODFT   ,NODLT  ,NUMNOD ,IADK  ,JDIK  ,
     2            ITASK   ,DIAG_SMS,LT_K   ,P_SMS  ,Y_SMS ,
     3            NODFT1_SMS,NODLT1_SMS,INDX1_SMS,NODNX_SMS,IAD_ELEM ,
     4            FR_ELEM   ,WEIGHT    ,JADI_SMS ,JDII_SMS ,LTI_SMS  ,
     5            ISKYI_SMS ,MSKYI_SMS ,FR_SMS   ,FR_RMS   ,LIST_SMS ,
     6            LIST_RMS  ,MSKYI_FI_SMS ,VFI   ,IMV      ,MV      ,
     7            MV6       ,MW6      ,NODFT2_SMS,NODLT2_SMS,INDX2_SMS,
     8            NODII_SMS )
C
        IF(IMONM>0.AND.ITASK==0)CALL STARTIME(61,1)
        IF(IPARIT==0)THEN
          RES1_SMS= ZERO
          G1_SMS  = ZERO
          S_SMS   = ZERO
        ELSE
!$OMP SINGLE
          DO K=1,6
            R6SMS(K)=ZERO
            G6SMS(K)=ZERO
            S6SMS(K)=ZERO
          ENDDO
!$OMP END SINGLE
#ifdef __INTEL_LLVM_COMPILER
!$omp barrier
#endif
        END IF
C
        CALL MY_BARRIER
C
        IF(NADMESH/=0)THEN
C
          CALL MY_BARRIER
C
          IF(ITASK==0)THEN
            CALL SMS_ADMESH_1(Y_SMS, DIAG_SMS, IXC, IXTG,SH4TREE  ,
     .      SH3TREE  ,NODNX_SMS)
          END IF
C
          CALL MY_BARRIER
C
        END IF
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
     1       IRBE2 ,LRBE2 ,P_SMS  ,Y_SMS  ,MS    ,
     1       SKEW   ,WEIGHT ,IAD_RBE2,FR_RBE2M,NMRBE2)
C
            CALL SMS_RBE_CNDS(
     1       IRBE2 ,LRBE2 ,X      ,Y_SMS  ,AM     ,
     1       MS    ,IN    ,SKEW   ,WEIGHT ,IAD_RBE2,
     2       FR_RBE2M,NMRBE2)
C
          END IF
C
        END IF
C-----------------------------------
C RBE3
C-----------------------------------
        IF (NRBE3>0)THEN
C
          CALL MY_BARRIER
C
          IF(ITASK==0)THEN
            CALL SMS_RBE3T1(
     1        IRBE3 ,LRBE3  ,X        ,Y_SMS   ,FRBE3    ,
     2        SKEW  ,WEIGHT ,IAD_RBE3M,FR_RBE3M,FR_RBE3MP,
     3        RRBE3 ,RRBE3_PON ,R3SIZE)
          END IF
        END IF
C-----------------------------------
C     remontee Yi => Ym
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
              RBY6(1,1,M)=Y_SMS(1,MSR)*WEIGHT(MSR)
              RBY6(2,1,M)=Y_SMS(2,MSR)*WEIGHT(MSR)
              RBY6(3,1,M)=Y_SMS(3,MSR)*WEIGHT(MSR)
            END IF
C
          END DO
!$OMP  END DO

!$OMP SINGLE
          DO N=1,NINDX1_SMS
            I=INDX1_SMS(N)
            M=TAGSLV_RBY_SMS(I)
            IF(M /= 0 )THEN
              IF(WEIGHT(I) /= 0)THEN
                RBY6(1,1,M)=RBY6(1,1,M)+Y_SMS(1,I)
                RBY6(2,1,M)=RBY6(2,1,M)+Y_SMS(2,I)
                RBY6(3,1,M)=RBY6(3,1,M)+Y_SMS(3,I)
              END IF
              Y_SMS(1,I)=ZERO
              Y_SMS(2,I)=ZERO
              Y_SMS(3,I)=ZERO
            END IF
          END DO
!$OMP END SINGLE
#ifdef __INTEL_LLVM_COMPILER
!$omp barrier
#endif

          IF (NSPMD > 1) THEN
!$OMP SINGLE
            NRBDIM=3
            CALL SPMD_EXCH_A_RB6(
     1        NRBDIM,IAD_RBY,FR_RBY6,IAD_RBY(NSPMD+1),RBY6)
!$OMP END SINGLE
#ifdef __INTEL_LLVM_COMPILER
!$omp barrier
#endif
          END IF

!$OMP DO SCHEDULE(DYNAMIC,1)
          DO M =1,NRBODY
            MSR=NPBY(1,M)
            IF(MSR < 0) CYCLE

            IF(TAGMSR_RBY_SMS(MSR) /= 0) THEN
              Y_SMS(1,MSR)=RBY6(1,1,M)
              Y_SMS(2,MSR)=RBY6(2,1,M)
              Y_SMS(3,MSR)=RBY6(3,1,M)
            END IF

          END DO
!$OMP  END DO
        END IF
C-----------------------------------
        CALL SMS_BCS(NODFT1_SMS,NODLT1_SMS,INDX1_SMS,ICODT  ,ISKEW ,
     2               SKEW      ,Y_SMS     ,NODLT1_SMS )
C-----------------------------------
        IF (NBCSCYC>0) CALL SMS_BCSCYC(IBCSCYC,LBCSCYC,SKEW,X,Y_SMS)
C-----------------------------------
C      LIENS RIGIDES ENTRE NOEUDS : REMONTEE
C---- // ----------------------------
        IF(NRLINK+NLINK+NJOINT > 0)THEN
C
          CALL MY_BARRIER
C
          IDOWN=0
          IF(NRLINK>0)CALL SMS_RLINK10(
     1     MS    ,Y_SMS ,ILINK ,LLINK,SKEW,
     2     FR_RL ,WEIGHT,FRL6  ,IDOWN,TAG_LNK_SMS,
     3     ITAB  ,FRL   )
C
          IF(NLINK>0) CALL SMS_RLINK11(
     1     MS    ,Y_SMS ,NNLINK,LNLINK,SKEW  ,
     2     FR_LL ,WEIGHT,FNL6  ,X     ,XFRAME,
     3     V     ,IDOWN ,TAG_LNK_SMS,ITAB,FNL)
C
          IF(NJOINT > 0)
     .    CALL SMS_CJOINT_1(Y_SMS ,DIAG_SMS,LJOINT,IADCJ,FR_CJ,
     .               CJWORK,IDOWN ,TAG_LNK_SMS(NRLINK+NLINK+1),ITASK)
        END IF
C
        IF(NRWALL > 0)THEN
C
          CALL MY_BARRIER
C
C project y_sms
          IFLAG=2
          CALL SMS_RGWAL_0(IFLAG ,X    ,V      ,RWBUF   ,LPRW  ,
     2    NPRW     ,MS   ,FSAV(1,NINTER+1),FR_WALL ,FOPT ,
     3    RWSAV    ,WEIGHT ,IRWL_WORK     ,NRWL_SMS,FRWL6,
     4    RBID     ,Y_SMS  ,RBID   ,RBID   )
        END IF
C
        CALL MY_BARRIER
C
C-----------------------------------
        DO N=NODFT1_SMS,NODLT1_SMS,MVSIZ
C
          LLT=MIN(NODLT1_SMS-N+1,MVSIZ)
C
          DO L=1,LLT
            I=INDX1_SMS(N+L-1)
            S(L) = (P_SMS(1,I)*Y_SMS(1,I)
     .      + P_SMS(2,I)*Y_SMS(2,I)
     .      + P_SMS(3,I)*Y_SMS(3,I))*WEIGHT(I)
          ENDDO
C
          IF(IPARIT==0)THEN
            ST = ZERO
            DO L=1,LLT
              ST=ST+S(L)
            END DO
#include "lockon.inc"
            S_SMS=S_SMS+ST
#include "lockoff.inc"
          ELSE
            DO K=1,6
              S6T(K) = ZERO
            ENDDO
            IF(IMONM>0.AND.ITASK==0)CALL STARTIME(62,1)
            CALL SUM_6_FLOAT(1,LLT,S,S6T,1)
            IF(IMONM>0.AND.ITASK==0)CALL STOPTIME(62,1)
#include "lockon.inc"
            DO K=1,6
              S6SMS(K)=S6SMS(K)+S6T(K)
            ENDDO
#include "lockoff.inc"
          END IF
        ENDDO
C-----------------------------------
C
        CALL MY_BARRIER
C
        IF(NSPMD <= 1)THEN
          IF(IPARIT/=0.AND.ITASK==0)THEN
            S_SMS=S6SMS(1)+S6SMS(2)+S6SMS(3)+
     .            S6SMS(4)+S6SMS(5)+S6SMS(6)
          END IF
        ELSEIF(ITASK==0)THEN     ! communication sur un seul thread
          IF(IPARIT==0)THEN
            IF(IMONM>0.AND.ITASK==0)CALL STARTIME(63,1)
            CALL SPMD_GLOB_DSUM9(S_SMS,1)
            CALL SPMD_RBCAST(S_SMS,S_SMS,1,1,0,2)
            IF(IMONM>0.AND.ITASK==0)CALL STOPTIME(63,1)
          ELSE
            IF(IMONM>0.AND.ITASK==0)CALL STARTIME(63,1)
            DO K=1,6
              DBUF(K)  =S6SMS(K)
            END DO
            CALL SPMD_GLOB_DPSUM9(DBUF,6)
            S_SMS  = DBUF(1)+DBUF(2)+DBUF(3)+
     .               DBUF(4)+DBUF(5)+DBUF(6)
            CALL SPMD_RBCAST(S_SMS,S_SMS,1,1,0,2)
            IF(IMONM>0.AND.ITASK==0)CALL STOPTIME(63,1)
          END IF
        END IF
C
        CALL MY_BARRIER
C
        ALPHA=G0_SMS/MAX(EM30,S_SMS)
c       print *,'alpha',it,alpha,g0_sms,s_sms
C
        DO N=NODFT1_SMS,NODLT1_SMS
          I=INDX1_SMS(N)
          X_SMS(1,I) = X_SMS(1,I) + ALPHA*P_SMS(1,I)
          X_SMS(2,I) = X_SMS(2,I) + ALPHA*P_SMS(2,I)
          X_SMS(3,I) = X_SMS(3,I) + ALPHA*P_SMS(3,I)
          RES_SMS(1,I) = RES_SMS(1,I) - ALPHA*Y_SMS(1,I)
          RES_SMS(2,I) = RES_SMS(2,I) - ALPHA*Y_SMS(2,I)
          RES_SMS(3,I) = RES_SMS(3,I) - ALPHA*Y_SMS(3,I)
        ENDDO
C-----------------------------------
        IF(NFXVEL > 0)THEN
C
          CALL MY_BARRIER
C
          IF(ITASK==0)
     .    CALL SMS_FIXVEL(IBFV   ,RES_SMS ,V        ,NPC    ,TF     ,
     2                    VEL    ,DIAG_SMS,X        ,SKEW   ,SENSOR_TAB,
     3                    WEIGHT  ,D         ,IFRAME,XFRAME ,NSENSOR   ,
     4                    IT+1   ,DIAG_SMS,NODNX_SMS,CPTREAC,NODREAC,
     5                    FTHREAC,AM       ,VR     ,DR      ,IN     ,
     6                    RBY     )
C
          CALL MY_BARRIER
C
        END IF
C-----------------------------------
        DO N=NODFT1_SMS,NODLT1_SMS
          I=INDX1_SMS(N)
          Z_SMS(1,I) = RES_SMS(1,I) *PREC_SMS(I)
          Z_SMS(2,I) = RES_SMS(2,I) *PREC_SMS(I)
          Z_SMS(3,I) = RES_SMS(3,I) *PREC_SMS(I)
        END DO
C-----------------------------------
C RBE3
C-----------------------------------
        IF (NRBE3>0)THEN
C
          CALL MY_BARRIER
C
          IF(ITASK==0)THEN
            CALL SMS_RBE3T2(IRBE3 ,LRBE3 ,X    ,Z_SMS ,FRBE3 ,
     2                      SKEW  ,RES_SMS     ,PREC_SMS3   )
          END IF
        END IF
C-----------------------------------
C RBE2
C-----------------------------------
        IF (NRBE2>0) THEN
C
          CALL MY_BARRIER
C
          IF(ITASK==0)THEN
            CALL SMS_RBE_ACCL(
     1       IRBE2 ,LRBE2 ,RES_SMS,Z_SMS  ,PREC_SMS3,
     1       SKEW   ,WEIGHT ,IAD_RBE2 ,FR_RBE2M,NMRBE2)
          END IF
C
        END IF
C-----------------------------------
C      LIENS RIGIDES ENTRE NOEUDS : PROJETTE
C---- // ----------------------------
        IF(NRLINK+NLINK+NJOINT+NADMESH > 0)THEN
C
          CALL MY_BARRIER
C
          IDOWN=1
          IF(NRLINK>0)CALL SMS_RLINK10(
     1     MS    ,Z_SMS ,ILINK ,LLINK,SKEW,
     2     FR_RL ,WEIGHT,FRL6  ,IDOWN,TAG_LNK_SMS,
     3     ITAB  ,FRL   )
C
          IF(NLINK>0) CALL SMS_RLINK11(
     1     MS    ,Z_SMS ,NNLINK,LNLINK,SKEW  ,
     2     FR_LL ,WEIGHT,FNL6  ,X     ,XFRAME,
     3     V     ,IDOWN ,TAG_LNK_SMS,ITAB,FNL)
C
          IF(NJOINT > 0)
     .    CALL SMS_CJOINT_1(Z_SMS ,DIAG_SMS,LJOINT,IADCJ,FR_CJ,
     .               CJWORK,IDOWN ,TAG_LNK_SMS(NRLINK+NLINK+1),ITASK)
C
          IF(NADMESH/=0)THEN
            CALL SMS_ADMESH_2(Z_SMS, DIAG_SMS, IXC, IXTG,SH4TREE  ,
     .                          SH3TREE  ,ITASK)
          END IF
C
          CALL MY_BARRIER
C
        END IF
C-----------------------------------
        DO N=NODFT1_SMS,NODLT1_SMS,MVSIZ
C
          LLT=MIN(NODLT1_SMS-N+1,MVSIZ)
C
          DO L=1,LLT
            I=INDX1_SMS(N+L-1)
            R2(L) = (  RES_SMS(1,I)*RES_SMS(1,I)
     .      + RES_SMS(2,I)*RES_SMS(2,I)
     .      + RES_SMS(3,I)*RES_SMS(3,I))
     .      * WEIGHT(I)
            G(L)  = (  Z_SMS(1,I)*RES_SMS(1,I)
     .               + Z_SMS(2,I)*RES_SMS(2,I)
     .               + Z_SMS(3,I)*RES_SMS(3,I))
     .                   * WEIGHT(I)
          ENDDO
C
          IF(IPARIT==0)THEN
            R2T = ZERO
            G1T = ZERO
            DO L=1,LLT
              R2T = R2T + R2(L)
              G1T = G1T + G(L)
            ENDDO
#include "lockon.inc"
            RES1_SMS= RES1_SMS+ R2T
            G1_SMS  = G1_SMS + G1T
#include "lockoff.inc"
          ELSE
            DO K=1,6
              R6T(K) = ZERO
              G6T(K) = ZERO
            ENDDO
            IF(IMONM>0.AND.ITASK==0)CALL STARTIME(62,1)
            CALL SUM_6_FLOAT(1,LLT,R2,R6T,1)
            CALL SUM_6_FLOAT(1,LLT,G,G6T,1)
            IF(IMONM>0.AND.ITASK==0)CALL STOPTIME(62,1)
#include "lockon.inc"
            DO K=1,6
              R6SMS(K)=R6SMS(K)+R6T(K)
              G6SMS(K)=G6SMS(K)+G6T(K)
            ENDDO
#include "lockoff.inc"
          END IF
        ENDDO
C-----------------------------------
C
        CALL MY_BARRIER
C
        IF(NSPMD <= 1)THEN
          IF(IPARIT/=0.AND.ITASK==0)THEN
            RES1_SMS=R6SMS(1)+R6SMS(2)+R6SMS(3)+
     .               R6SMS(4)+R6SMS(5)+R6SMS(6)
            G1_SMS =G6SMS(1)+G6SMS(2)+G6SMS(3)+
     .              G6SMS(4)+G6SMS(5)+G6SMS(6)
          END IF
        ELSEIF(ITASK==0)THEN     ! communication sur un seul thread
          IF(IPARIT==0)THEN
            IF(IMONM>0) CALL STARTIME(63,1)
            RBUF(1)=RES1_SMS
            RBUF(2)=G1_SMS
            CALL SPMD_GLOB_DSUM9(RBUF,2)
            CALL SPMD_RBCAST(RBUF,RBUF,2,1,0,2)
            RES1_SMS =RBUF(1)
            G1_SMS  =RBUF(2)
            IF(IMONM>0) CALL STOPTIME(63,1)
          ELSE
            IF(IMONM>0) CALL STARTIME(63,1)
            DO K=1,6
              DBUF(K)  =R6SMS(K)
              DBUF(K+6)=G6SMS(K)
            END DO
            CALL SPMD_GLOB_DPSUM9(DBUF,12)
            RBUF(1)  = DBUF(1)+DBUF(2)+DBUF(3)+
     .                 DBUF(4)+DBUF(5)+DBUF(6)
            RBUF(2)  = DBUF(7) +DBUF(8) +DBUF(9)+
     .                 DBUF(10)+DBUF(11)+DBUF(12)
            CALL SPMD_RBCAST(RBUF,RBUF,2,1,0,2)
            RES1_SMS=RBUF(1)
            G1_SMS =RBUF(2)
            IF(IMONM>0) CALL STOPTIME(63,1)
          END IF
        END IF
C
        CALL MY_BARRIER
C

        if(ncpria > 0) then
          if(itask==0.and.ispmd==0
     .       .and.(ncprisms < 0 .and.
     .        mod(ncycle,ncpria)==0))then
            write(iout,1002) ncycle,totit,res1_sms,toln
          end if
        endif
C
        IF(IMONM>0.AND.ITASK==0)CALL STOPTIME(61,1)
        IF(IT>=NLIM.OR.RES1_SMS<=TOLN) GO TO 200
        IF(IMONM>0.AND.ITASK==0)CALL STARTIME(61,1)

        BETA=G1_SMS/MAX(EM30,G0_SMS)
C
        CALL MY_BARRIER
C
!$OMP SINGLE
        G0_SMS = G1_SMS
!$OMP END SINGLE
#ifdef __INTEL_LLVM_COMPILER
!$omp barrier
#endif

        DO N=NODFT1_SMS,NODLT1_SMS
          I=INDX1_SMS(N)
          P_SMS(1,I) = Z_SMS(1,I) + BETA*P_SMS(1,I)
          P_SMS(2,I) = Z_SMS(2,I) + BETA*P_SMS(2,I)
          P_SMS(3,I) = Z_SMS(3,I) + BETA*P_SMS(3,I)
        ENDDO
C-----------------------------------
C     redescente Pm => Pi
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
              P_SMS(1,I)=P_SMS(1,MSR)
              P_SMS(2,I)=P_SMS(2,MSR)
              P_SMS(3,I)=P_SMS(3,MSR)
            END IF
          END DO
C
          CALL MY_BARRIER()
C
        END IF
C-----------------------------------
C
        CALL MY_BARRIER
C
        IF(IMONM>0.AND.ITASK==0)CALL STOPTIME(61,1)
        GO TO 100
 200    CONTINUE
c      if(itask==0.and.ispmd==0)then
c     .           .and.mod(ncycle,npri_sms)==0)then
c        print *,ncycle,'nit=',it,nlim,res1_sms,toln
c      end if

        IF(IMONM>0.AND.ITASK==0)CALL STARTIME(61,1)
        IF(IT>=NLIM)THEN
          MSTOP = 2
          IF(ISPMD==0.AND.ITASK==0)THEN
#include "lockon.inc"
            WRITE(ISTDO,*)
     .      ' ** ERROR : AMS IS LIKELY DIVERGING '
            WRITE(IOUT,1100) NLIM,NCYCLE
#include "lockoff.inc"
          ENDIF
C
          IF(IDTMINS/=0)THEN
C
            CALL MY_BARRIER
C
            CALL SMS_CHECK(NODFT ,NODLT ,IADK  ,JDIK  ,DIAG_SMS,
     2                     LT_K  ,JADI_SMS ,JDII_SMS ,LTI_SMS  ,ITASK  ,
     3                     ITAB  ,IAD_ELEM ,FR_ELEM  ,FR_SMS   ,FR_RMS ,
     4                     LIST_SMS,LIST_RMS)
C
          END IF
C
          GO TO 300
        ENDIF
C-----------------------------------
C     Reaction force and work
C-----------------------------------
        IF(NRWALL/=0)THEN
C
          CALL MY_BARRIER
C
          IF(IMONM>0.AND.ITASK==0)CALL STOPTIME(61,1)
          CALL SMS_MAV_LT(
     1             NODFT   ,NODLT  ,NUMNOD ,IADK  ,JDIK  ,
     2             ITASK   ,DIAG_SMS,LT_K   ,X_SMS  ,Z_SMS ,
     3             NODFT1_SMS,NODLT1_SMS,INDX1_SMS,NODNX_SMS,IAD_ELEM ,
     4             FR_ELEM   ,WEIGHT    ,JADI_SMS ,JDII_SMS ,LTI_SMS  ,
     5             ISKYI_SMS ,MSKYI_SMS ,FR_SMS   ,FR_RMS   ,LIST_SMS ,
     6             LIST_RMS  ,MSKYI_FI_SMS ,VFI   ,IMV      ,MV      ,
     7             MV6       ,MW6       ,NODFT2_SMS,NODLT2_SMS,INDX2_SMS,
     8             NODII_SMS )
C
          IF(IMONM>0.AND.ITASK==0)CALL STARTIME(61,1)
C
          CALL MY_BARRIER
C
          IF(NADMESH/=0)THEN
            IF(ITASK==0)THEN
              CALL SMS_ADMESH_1(Z_SMS, DIAG_SMS, IXC, IXTG,SH4TREE  ,
     .        SH3TREE  ,NODNX_SMS)
            END IF
C
            CALL MY_BARRIER
C
          END IF
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
     1         IRBE2 ,LRBE2 ,X_SMS  ,Z_SMS  ,MS    ,
     1         SKEW   ,WEIGHT ,IAD_RBE2,FR_RBE2M,NMRBE2)
C
              CALL SMS_RBE_CNDS(
     1         IRBE2 ,LRBE2 ,X      ,Z_SMS  ,AM     ,
     1         MS    ,IN    ,SKEW   ,WEIGHT ,IAD_RBE2,
     2         FR_RBE2M,NMRBE2)
C
            END IF
C
          END IF
C-----------------------------------
C RBE3
C-----------------------------------
          IF (NRBE3>0)THEN
C
            CALL MY_BARRIER
C
            IF(ITASK==0)THEN
              CALL SMS_RBE3T1(
     1        IRBE3 ,LRBE3  ,X        ,Z_SMS   ,FRBE3    ,
     2        SKEW  ,WEIGHT ,IAD_RBE3M,FR_RBE3M,FR_RBE3MP,
     3        RRBE3 ,RRBE3_PON ,R3SIZE)
            END IF
          END IF
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
              IF(M /= 0 )THEN
                IF(WEIGHT(I) /= 0)THEN
                  RBY6(1,1,M)=RBY6(1,1,M)+Z_SMS(1,I)
                  RBY6(2,1,M)=RBY6(2,1,M)+Z_SMS(2,I)
                  RBY6(3,1,M)=RBY6(3,1,M)+Z_SMS(3,I)
                END IF
              END IF
            END DO
!$OMP END SINGLE
#ifdef __INTEL_LLVM_COMPILER
!$omp barrier
#endif

            IF (NSPMD > 1) THEN
!$OMP SINGLE
              NRBDIM=3
              CALL SPMD_EXCH_A_RB6(
     1          NRBDIM,IAD_RBY,FR_RBY6,IAD_RBY(NSPMD+1),RBY6)
!$OMP END SINGLE
#ifdef __INTEL_LLVM_COMPILER
!$omp barrier
#endif
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
!$OMP    END DO
          END IF
C
          CALL MY_BARRIER
C
          CALL SMS_BCS(NODFT1_SMS,NODLT1_SMS,INDX1_SMS,ICODT  ,ISKEW ,
     2                 SKEW      ,Z_SMS     ,NODLT1_SMS )
C-----------------------------------
C /BCS/CYCLIC
C-----------------------------------
          IF (NBCSCYC>0) CALL SMS_BCSCYC(IBCSCYC,LBCSCYC,SKEW,X,Z_SMS)
C-----------------------------------
C      LIENS RIGIDES ENTRE NOEUDS : REMONTEE
C---- // ----------------------------
          IF(NRLINK+NLINK+NJOINT > 0)THEN
C
            CALL MY_BARRIER
C
            IDOWN=0
            IF(NRLINK>0)CALL SMS_RLINK10(
     1       MS    ,Z_SMS ,ILINK ,LLINK,SKEW,
     2       FR_RL ,WEIGHT,FRL6  ,IDOWN,TAG_LNK_SMS,
     3       ITAB  ,FRL   )
C
            IF(NLINK>0) CALL SMS_RLINK11(
     1       MS    ,Z_SMS ,NNLINK,LNLINK,SKEW  ,
     2       FR_LL ,WEIGHT,FNL6  ,X     ,XFRAME,
     3       V     ,IDOWN ,TAG_LNK_SMS,ITAB,FNL)
C
            IF(NJOINT > 0)
     .      CALL SMS_CJOINT_1(Z_SMS ,DIAG_SMS,LJOINT,IADCJ,FR_CJ,
     .                 CJWORK,IDOWN ,TAG_LNK_SMS(NRLINK+NLINK+1),ITASK)
          END IF
C
          CALL MY_BARRIER
C
          IF(IFRICW/=0.AND.IACT==0)THEN
C
            IACT=IACT+1
C
            DO N=NODFT1_SMS,NODLT1_SMS
              I=INDX1_SMS(N)
C
              RES_SMS(1,I) = R(1,I)-Z_SMS(1,I)
              RES_SMS(2,I) = R(2,I)-Z_SMS(2,I)
              RES_SMS(3,I) = R(3,I)-Z_SMS(3,I)
            ENDDO
C--------
            IF(NRBODY/=0)THEN
C
              CALL MY_BARRIER()
C
              DO N=NODFT1_SMS,NODLT1_SMS
                I=INDX1_SMS(N)
                M=TAGSLV_RBY_SMS(I)
                IF(M /= 0)THEN
                  RES_SMS(1,I)=ZERO
                  RES_SMS(2,I)=ZERO
                  RES_SMS(3,I)=ZERO
                END IF
              END DO
            END IF
C
            CALL MY_BARRIER
C
C store Ft
            IFLAG=3
            CALL SMS_RGWAL_0(IFLAG ,X    ,V      ,RWBUF   ,LPRW  ,
     2                    NPRW     ,MS   ,FSAV(1,NINTER+1),FR_WALL ,FOPT ,
     3                    RWSAV    ,WEIGHT ,IRWL_WORK     ,NRWL_SMS,FRWL6,
     4                    X_SMS    ,RES_SMS,R    ,FREA    )
            IT  =0
            GO TO 10
          ELSE
C
            DO N=NODFT1_SMS,NODLT1_SMS
              I=INDX1_SMS(N)
C
C retrieve Frea == 0 or Ft if sliding
              FREA(1,I) = FREA(1,I)+R(1,I)-Z_SMS(1,I)
              FREA(2,I) = FREA(2,I)+R(2,I)-Z_SMS(2,I)
              FREA(3,I) = FREA(3,I)+R(3,I)-Z_SMS(3,I)
            ENDDO
C
            CALL MY_BARRIER
C
C--------
            IF(NRBODY/=0)THEN
C
              CALL MY_BARRIER()
C
              DO N=NODFT1_SMS,NODLT1_SMS
                I=INDX1_SMS(N)
                M=TAGSLV_RBY_SMS(I)
                IF(M /= 0)THEN
                  FREA(1,I)=ZERO
                  FREA(2,I)=ZERO
                  FREA(3,I)=ZERO
                END IF
              END DO
C
              CALL MY_BARRIER()
C
            END IF
C
            IFLAG=4
            CALL SMS_RGWAL_0(IFLAG ,X    ,V      ,RWBUF   ,LPRW  ,
     2                    NPRW     ,MS   ,FSAV(1,NINTER+1),FR_WALL ,FOPT ,
     3                    RWSAV    ,WEIGHT ,IRWL_WORK     ,NRWL_SMS,FRWL6,
     4                    X_SMS    ,RES_SMS,R    ,FREA    )
C
            CALL MY_BARRIER
C
          END IF
        END IF
C
C-------X->R--------
 300    CONTINUE
        DO N=NODFT1_SMS,NODLT1_SMS
          I=INDX1_SMS(N)
          R(1,I) = X_SMS(1,I)
          R(2,I) = X_SMS(2,I)
          R(3,I) = X_SMS(3,I)
        ENDDO
        IF(IMONM>0.AND.ITASK==0)CALL STOPTIME(61,1)
C--------PCG (PROJECTION)
        IF (M_VS_SMS > 0 .AND. IT > 0) THEN
          IF(IMONM>0.AND.ITASK==0)CALL STARTIME(70,1)
          CALL SMS_UPDST(
     1             IADK  ,JDIK  ,DIAG_SMS,LT_K  ,ITASK ,
     2             NODFT1_SMS,NODLT1_SMS,INDX1_SMS,NODNX_SMS,IAD_ELEM ,
     3             FR_ELEM   ,WEIGHT    ,JADI_SMS ,JDII_SMS ,LTI_SMS  ,
     4             ISKYI_SMS ,MSKYI_SMS ,FR_SMS   ,FR_RMS   ,LIST_SMS ,
     5             LIST_RMS  ,MSKYI_FI_SMS ,VFI   ,IMV      ,MV       ,
     6             MV6       ,MW6       ,MS       ,X_SMS    ,P_SMS    ,
     7             Y_SMS     ,NODFT     ,NODLT    ,KINET    )
C----------------------
          CALL MY_BARRIER
C----------------------
          IF (ITASK == 0) NCG_RUN_SMS = NCG_RUN_SMS + 1
          IF(IMONM>0.AND.ITASK==0)CALL STOPTIME(70,1)
        END IF
C
        if(ncpria > 0) then
          if(itask==0.and.ispmd==0
     .       .and.(ncprisms/=0.and.mod(ncycle,ncpria)==0))then
            IF(TOTIT==0)THEN
              write(iout,1000) ncycle,totit
            ELSE
              write(iout,1001) ncycle,totit,res1_sms,toln
            END IF
          end if
        endif
C--------------------------------------------
 1000   FORMAT(3X,'CYCLE NUMBER',I5,
     .            ' TOTAL C.G. ITERATION NUMBER=',I5)
 1001   FORMAT(3X,'CYCLE NUMBER',I5,
     .            ' TOTAL C.G. ITERATION NUMBER=',I5,
     .            ' RELATIVE RESIDUAL NORM=',E11.4,
     .            ' REFERENCE RESIDUAL NORM',E11.4)
 1002   FORMAT(3X,'CYCLE NUMBER',I5,
     .            ' ITERATION NUMBER=',I5,
     .            ' RELATIVE RESIDUAL NORM=',E11.4,
     .            ' REFERENCE RESIDUAL NORM',E11.4)
 1100   FORMAT(
     .   ' ** ERROR : AMS IS LIKELY DIVERGING:',/,
     .   '    TOTAL C.G. ITERATION NUMBER = ',I8,' AT CYCLE NUMBER ',I8)
        RETURN
      END
C-------------produit {w}=[K]{v} using full matrix K ----
Chd|====================================================================
Chd|  SMS_MAV_LT                    source/ams/sms_pcg.F          
Chd|-- called by -----------
Chd|        SMS_ENCIN_2                   source/ams/sms_encin_2.F      
Chd|        SMS_MASS_SCALE_2              source/ams/sms_mass_scale_2.F 
Chd|        SMS_PCG                       source/ams/sms_pcg.F          
Chd|-- calls ---------------
Chd|        FOAT_TO_6_FLOAT               source/system/parit.F         
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_EXCH_SMS                 source/mpi/ams/spmd_exch_sms.F
Chd|        SPMD_EXCH_SMS6                source/mpi/ams/spmd_exch_sms6.F
Chd|        SPMD_VFI_SMS                  source/mpi/ams/spmd_vfi_sms.F 
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|====================================================================
      SUBROUTINE SMS_MAV_LT(NODFT  ,NODLT ,NUMNOD ,IADL  ,JDIL  ,
     2                      ITASK  ,DIAG_K ,LT_K  ,V      ,W     ,
     3              NODFT1_SMS,NODLT1_SMS,INDX1_SMS,NODNX_SMS,IAD_ELEM ,
     4              FR_ELEM   ,WEIGHT    ,JADI_SMS ,JDII_SMS ,LTI_SMS  ,
     5              ISKYI_SMS ,MSKYI_SMS ,FR_SMS   ,FR_RMS   ,LIST_SMS ,
     6              LIST_RMS  ,MSKYI_FI_SMS,VFI    ,IMV      ,MV      ,
     7              MV6       ,MW6         ,NODFT2_SMS,NODLT2_SMS,INDX2_SMS,
     8              NODII_SMS )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "com01_c.inc"
#include "parit_c.inc"
#include "sms_c.inc"
#include "task_c.inc"
#include "timeri_c.inc"
#include "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER  NODFT, NODLT, ITASK, NUMNOD, IADL(*)  ,JDIL(*),
     .           NODFT1_SMS,NODLT1_SMS,INDX1_SMS(*), NODNX_SMS(*),
     .           NODFT2_SMS,NODLT2_SMS,INDX2_SMS(*), NODII_SMS(*),
     .           IAD_ELEM(2,NSPMD+1) ,FR_ELEM(*),WEIGHT(*),
     .           JADI_SMS(*),JDII_SMS(*),
     .           ISKYI_SMS(LSKYI_SMS,*),FR_SMS(NSPMD+1),FR_RMS(NSPMD+1),
     .           LIST_SMS(*), LIST_RMS(*), IMV(*)
C     REAL
        my_real
     .    DIAG_K(*), W(*), LT_K(*)  ,V(*), LTI_SMS(*), MSKYI_SMS(*),
     .    MSKYI_FI_SMS(*), VFI(*), MV(*)
        DOUBLE PRECISION MV6(6,3,*), MW6(6,3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,K,I3,I2,I1,K3,K2,K1,N, LOC_PROC, M, KK,
     .          KMV,KMV3,KMV2,KMV1
        INTEGER SIZE, LENR, JAD, DIR, L, LLT,
     .          IAD_SEND(NSPMD+1),IAD_RECV(NSPMD+1),
     .          REQ_R(NSPMD),REQ_S(NSPMD)
        my_real
     .     L_K
        my_real
     .          RBUF(3*(FR_RMS(NSPMD+1)+FR_SMS(NSPMD+1))),
     .          SBUF(3*(FR_RMS(NSPMD+1)+FR_SMS(NSPMD+1)))
C-----------------------------

C
        IF(IDTMINS==2.OR.IDTMINS_INT/=0)THEN
C
          IF(NSPMD>1) THEN
C
            CALL MY_BARRIER
C
            IF(ITASK==0)THEN   ! comm sur 1er thread
C
              SIZE = 3
              IF(IMONM>0) CALL STARTIME(65,1)
              CALL SPMD_VFI_SMS(V,SIZE,VFI,FR_RMS,
     .                         FR_SMS,LIST_RMS,LIST_SMS,1,
     .                         IAD_SEND,IAD_RECV,REQ_R,REQ_S,RBUF,SBUF)
              IF(IMONM>0) CALL STOPTIME(65,1)

            END IF
          END IF
        END IF
C
        IF(IMONM>0.AND.ITASK==0)CALL STARTIME(64,1)
        IF(IMONM>0.AND.ITASK==0)CALL STARTIME(74,1)
C
        KMV= 0
        IF(IPARIT==0.OR.DEBUG(9)==0)THEN
C
          DO N=NODFT1_SMS,NODLT1_SMS
            I=INDX1_SMS(N)
            I3=3*I
            I2=I3-1
            I1=I2-1
            W(I3)=DIAG_K(I)*V(I3)*WEIGHT(I)
            W(I2)=DIAG_K(I)*V(I2)*WEIGHT(I)
            W(I1)=DIAG_K(I)*V(I1)*WEIGHT(I)
          ENDDO
C
          IF(IDTMINS/=0)THEN
            DO N=NODFT1_SMS,NODLT1_SMS
              I=INDX1_SMS(N)
              I3=3*I
              I2=I3-1
              I1=I2-1
              DO J =IADL(I),IADL(I+1)-1
                K =ABS(JDIL(J))
                K3=3*K
                K2=K3-1
                K1=K2-1
                L_K = LT_K(J)
                W(I3) = W(I3) + L_K*V(K3)
                W(I2) = W(I2) + L_K*V(K2)
                W(I1) = W(I1) + L_K*V(K1)
c           W(K3) = W(K3) + L_K*V(I3)
c           W(K2) = W(K2) + L_K*V(I2)
c           W(K1) = W(K1) + L_K*V(I1)
              ENDDO
            ENDDO
          END IF
C
        ELSE
C---------------------------------------------------------------------
C Parith/ON is ensured when changing n of threads and/or n of domains
C---------------------------------------------------------------------
          DO N=NODFT1_SMS,NODLT1_SMS
            I=INDX1_SMS(N)
            I3=3*I
            I2=I3-1
            I1=I2-1
            W(I3)=ZERO
            W(I2)=ZERO
            W(I1)=ZERO
          ENDDO
C
          IF(IDTMINS/=0)THEN
            DO N=NODFT1_SMS,NODLT1_SMS
              I=INDX1_SMS(N)
              I3=3*I
              I2=I3-1
              I1=I2-1
              KMV =KMV + 1
              KMV3=3*KMV
              KMV2=KMV3-1
              KMV1=KMV2-1
              IMV(KMV)=I
              MV(KMV3)=DIAG_K(I)*V(I3)*WEIGHT(I)
              MV(KMV2)=DIAG_K(I)*V(I2)*WEIGHT(I)
              MV(KMV1)=DIAG_K(I)*V(I1)*WEIGHT(I)
              DO J =IADL(I),IADL(I+1)-1
                K =ABS(JDIL(J))
                L_K = LT_K(J)
                K3=3*K
                K2=K3-1
                K1=K2-1
                KMV =KMV + 1
                KMV3=3*KMV
                KMV2=KMV3-1
                KMV1=KMV2-1
                IMV(KMV)=I
                MV(KMV3)=L_K*V(K3)
                MV(KMV2)=L_K*V(K2)
                MV(KMV1)=L_K*V(K1)
              END DO
            END DO
          END IF
        END IF
C
        CALL MY_BARRIER ! barriere avt NODFT2_SMS,NODLT2_SMS
C
        IF(ITASK==0)THEN
          IF(IMONM>0)CALL STOPTIME(74,1)
        END IF
C
        IF(IDTMINS==2.OR.IDTMINS_INT/=0)THEN
C
          IF(IPARIT==0)THEN
            DO N=NODFT2_SMS,NODLT2_SMS
              I=INDX2_SMS(N)
              I3=3*I
              I2=I3-1
              I1=I2-1
              DO J =JADI_SMS(I),JADI_SMS(I+1)-1
                K =JDII_SMS(J)
                K3=3*K
                K2=K3-1
                K1=K2-1
                L_K = LTI_SMS(J)
                W(I3) = W(I3) +L_K*V(K3)
                W(I2) = W(I2) +L_K*V(K2)
                W(I1) = W(I1) +L_K*V(K1)
c           W(K3) = W(K3) +L_K*V(I3)
c           W(K2) = W(K2) +L_K*V(I2)
c           W(K1) = W(K1) +L_K*V(I1)
              END DO
            END DO
          END IF
C
          IF(NSPMD>1) THEN
C
            IF(ITASK==0)THEN   ! comm sur 1er thread
              IF(IMONM>0)CALL STOPTIME(64,1)
C
              SIZE = 3
              IF(IMONM>0) CALL STARTIME(65,1)
              CALL SPMD_VFI_SMS(V,SIZE,VFI,FR_RMS,
     .                         FR_SMS,LIST_RMS,LIST_SMS,2,
     .                         IAD_SEND,IAD_RECV,REQ_R,REQ_S,RBUF,SBUF)
              IF(IMONM>0) CALL STOPTIME(65,1)

            END IF
C
            CALL MY_BARRIER
C
          ELSE
C
            CALL MY_BARRIER
C
            IF(IMONM>0.AND.ITASK==0)CALL STOPTIME(64,1)
          END IF
C
          IF(IMONM>0.AND.ITASK==0)CALL STARTIME(64,1)
C
          IF(IPARIT==0)THEN
            IF(NSPMD>1) THEN
              IF(ITASK==0)THEN
                KK = 0
                LOC_PROC=ISPMD+1
                M = 1
                DO L = 1, NSPMD
C
                  DO K=FR_RMS(L),FR_RMS(L+1)-1
                    I=LIST_RMS(K)
                    KK = KK + 1
                    IF(I==0)CYCLE
                    I3=3*I
                    I2=I3-1
                    I1=I2-1
                    K3=3*KK
                    K2=K3-1
                    K1=K2-1
                    W(I3) = W(I3) -MSKYI_FI_SMS(K)*VFI(K3)
                    W(I2) = W(I2) -MSKYI_FI_SMS(K)*VFI(K2)
                    W(I1) = W(I1) -MSKYI_FI_SMS(K)*VFI(K1)
                  END DO
C
                  IF(L/=LOC_PROC)THEN
                    DO K=FR_SMS(L),FR_SMS(L+1)-1
                      I=LIST_SMS(M)
                      KK= KK + 1
                      M = M + 1
                      IF(I==0)CYCLE
                      I3=3*I
                      I2=I3-1
                      I1=I2-1
                      K3=3*KK
                      K2=K3-1
                      K1=K2-1
                      W(I3) = W(I3) -MSKYI_SMS(K)*VFI(K3)
                      W(I2) = W(I2) -MSKYI_SMS(K)*VFI(K2)
                      W(I1) = W(I1) -MSKYI_SMS(K)*VFI(K1)
                    END DO
                  END IF
C
                END DO
              END IF
C
              CALL MY_BARRIER
C
              IF(ITASK==0)THEN   ! comm sur 1er thread
                IF(IMONM>0)CALL STOPTIME(64,1)
C
                SIZE = 3
                LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
                IF(IMONM>0) CALL STARTIME(80,1)
                CALL SPMD_EXCH_SMS(W,NODNX_SMS,IAD_ELEM,FR_ELEM,SIZE,
     .                             LENR)
                IF(IMONM>0) CALL STOPTIME(80,1)
              END IF
C
C BARRIER before RETURN
              CALL MY_BARRIER
C
            ELSE
C
C BARRIER before RETURN
              CALL MY_BARRIER
C
              IF(IMONM>0.AND.ITASK==0)CALL STOPTIME(64,1)
C
            END IF
C
          ELSEIF(DEBUG(9)==0)THEN ! IPARIT==1.AND.DEBUG(9)==0) !
C---------------------------------------------------------------------
C Parith/ON is ensured when changing n of threads, not n of domains
C---------------------------------------------------------------------
            DO N=NODFT2_SMS,NODLT2_SMS
              I=INDX2_SMS(N)
              DO J =JADI_SMS(I),JADI_SMS(I+1)-1
                K =JDII_SMS(J)
                L_K = LTI_SMS(J)
                K3=3*K
                K2=K3-1
                K1=K2-1
                KMV =KMV + 1
                KMV3=3*KMV
                KMV2=KMV3-1
                KMV1=KMV2-1
                IMV(KMV)=I
                MV(KMV3)=L_K*V(K3)
                MV(KMV2)=L_K*V(K2)
                MV(KMV1)=L_K*V(K1)
              END DO
            END DO
C
            IF(NSPMD>1) THEN
C
Cafter gather VFI
              CALL MY_BARRIER
C
              KK = 0
              LOC_PROC=ISPMD+1
              M = 1
              DO L = 1, NSPMD
C
                DO K=FR_RMS(L),FR_RMS(L+1)-1
                  I=LIST_RMS(K)
                  KK = KK + 1
                  IF(I == 0 .OR. NODII_SMS(I) < NODFT2_SMS .OR.
     .            NODLT2_SMS < NODII_SMS(I))CYCLE
                  K3=3*KK
                  K2=K3-1
                  K1=K2-1
                  KMV =KMV + 1
                  KMV3=3*KMV
                  KMV2=KMV3-1
                  KMV1=KMV2-1
                  IMV(KMV)=I
                  MV(KMV3) = -MSKYI_FI_SMS(K)*VFI(K3)
                  MV(KMV2) = -MSKYI_FI_SMS(K)*VFI(K2)
                  MV(KMV1) = -MSKYI_FI_SMS(K)*VFI(K1)
                END DO
C
                IF(L/=LOC_PROC)THEN
                  DO K=FR_SMS(L),FR_SMS(L+1)-1
                    I=LIST_SMS(M)
                    KK= KK + 1
                    M = M + 1
                    IF(I == 0 .OR. NODII_SMS(I) < NODFT2_SMS .OR.
     .              NODLT2_SMS < NODII_SMS(I))CYCLE
                    K3=3*KK
                    K2=K3-1
                    K1=K2-1
                    KMV =KMV + 1
                    KMV3=3*KMV
                    KMV2=KMV3-1
                    KMV1=KMV2-1
                    IMV(KMV)=I
                    MV(KMV3) = -MSKYI_SMS(K)*VFI(K3)
                    MV(KMV2) = -MSKYI_SMS(K)*VFI(K2)
                    MV(KMV1) = -MSKYI_SMS(K)*VFI(K1)
                  END DO
                END IF
C
              END DO
C
            END IF
C
            CALL FOAT_TO_6_FLOAT(1,3*KMV,MV,MV6)
C
            DO N=NODFT2_SMS,NODLT2_SMS
              I=INDX2_SMS(N)
              DO J=1,6
                MW6(J,1,I)=ZERO
                MW6(J,2,I)=ZERO
                MW6(J,3,I)=ZERO
              END DO
            END DO
C
            DO K=1,KMV
              I=IMV(K)
              DO J=1,6
                MW6(J,1,I) = MW6(J,1,I)+MV6(J,1,K)
                MW6(J,2,I) = MW6(J,2,I)+MV6(J,2,K)
                MW6(J,3,I) = MW6(J,3,I)+MV6(J,3,K)
              END DO
            END DO
C
            DO N=NODFT2_SMS,NODLT2_SMS
              I=INDX2_SMS(N)
              I3=3*I
              I2=I3-1
              I1=I2-1
              W(I3) = W(I3)
     .               +MW6(1,3,I)+MW6(2,3,I)+MW6(3,3,I)
     .        +MW6(4,3,I)+MW6(5,3,I)+MW6(6,3,I)
              W(I2) = W(I2)
     .               +MW6(1,2,I)+MW6(2,2,I)+MW6(3,2,I)
     .        +MW6(4,2,I)+MW6(5,2,I)+MW6(6,2,I)
              W(I1) = W(I1)
     .               +MW6(1,1,I)+MW6(2,1,I)+MW6(3,1,I)
     .        +MW6(4,1,I)+MW6(5,1,I)+MW6(6,1,I)
            END DO
C
            IF(NSPMD>1) THEN
C
              CALL MY_BARRIER
C
              IF(ITASK==0)THEN   ! comm sur 1er thread
                IF(IMONM>0)CALL STOPTIME(64,1)
C
                SIZE = 3
                LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
                IF(IMONM>0) CALL STARTIME(80,1)
                CALL SPMD_EXCH_SMS(W,NODNX_SMS,IAD_ELEM,FR_ELEM,SIZE,
     .                            LENR)
                IF(IMONM>0) CALL STOPTIME(80,1)
              END IF
C
C BARRIER before RETURN
              CALL MY_BARRIER
C
            ELSE
C
C BARRIER before RETURN
              CALL MY_BARRIER
C
              IF(IMONM>0.AND.ITASK==0)CALL STOPTIME(64,1)
C
            END IF
C
          ELSE ! IF(IPARIT==1.AND.DEBUG(9)==1)
C---------------------------------------------------------------------
C Parith/ON is ensured when changing n of threads and/or n of domains
C---------------------------------------------------------------------
            DO N=NODFT1_SMS,NODLT1_SMS
              I=INDX1_SMS(N)
              DO J =JADI_SMS(I),JADI_SMS(I+1)-1
                K =JDII_SMS(J)
                L_K = LTI_SMS(J)
                K3=3*K
                K2=K3-1
                K1=K2-1
                KMV =KMV + 1
                KMV3=3*KMV
                KMV2=KMV3-1
                KMV1=KMV2-1
                IMV(KMV)=I
                MV(KMV3)=L_K*V(K3)
                MV(KMV2)=L_K*V(K2)
                MV(KMV1)=L_K*V(K1)
              END DO
            END DO
C
            IF(NSPMD>1) THEN
C
Cafter gather VFI
              CALL MY_BARRIER
C
              KK = 0
              LOC_PROC=ISPMD+1
              M = 1
              DO L = 1, NSPMD
C
                DO K=FR_RMS(L),FR_RMS(L+1)-1
                  I=LIST_RMS(K)
                  KK = KK + 1
                  IF(I == 0 .OR. NODNX_SMS(I) < NODFT1_SMS .OR.
     .            NODLT1_SMS < NODNX_SMS(I))CYCLE
                  K3=3*KK
                  K2=K3-1
                  K1=K2-1
                  KMV =KMV + 1
                  KMV3=3*KMV
                  KMV2=KMV3-1
                  KMV1=KMV2-1
                  IMV(KMV)=I
                  MV(KMV3) = -MSKYI_FI_SMS(K)*VFI(K3)
                  MV(KMV2) = -MSKYI_FI_SMS(K)*VFI(K2)
                  MV(KMV1) = -MSKYI_FI_SMS(K)*VFI(K1)
                END DO
C
                IF(L/=LOC_PROC)THEN
                  DO K=FR_SMS(L),FR_SMS(L+1)-1
                    I=LIST_SMS(M)
                    KK= KK + 1
                    M = M + 1
                    IF(I == 0 .OR. NODNX_SMS(I) < NODFT1_SMS .OR.
     .              NODLT1_SMS < NODNX_SMS(I))CYCLE
                    K3=3*KK
                    K2=K3-1
                    K1=K2-1
                    KMV =KMV + 1
                    KMV3=3*KMV
                    KMV2=KMV3-1
                    KMV1=KMV2-1
                    IMV(KMV)=I
                    MV(KMV3) = -MSKYI_SMS(K)*VFI(K3)
                    MV(KMV2) = -MSKYI_SMS(K)*VFI(K2)
                    MV(KMV1) = -MSKYI_SMS(K)*VFI(K1)
                  END DO
                END IF
C
              END DO
C
            END IF
C
            CALL FOAT_TO_6_FLOAT(1,3*KMV,MV,MV6)
C
            DO N=NODFT1_SMS,NODLT1_SMS
              I=INDX1_SMS(N)
              DO J=1,6
                MW6(J,1,I)=ZERO
                MW6(J,2,I)=ZERO
                MW6(J,3,I)=ZERO
              END DO
            END DO
C
            DO K=1,KMV
              I=IMV(K)
              DO J=1,6
                MW6(J,1,I) = MW6(J,1,I)+MV6(J,1,K)
                MW6(J,2,I) = MW6(J,2,I)+MV6(J,2,K)
                MW6(J,3,I) = MW6(J,3,I)+MV6(J,3,K)
              END DO
            END DO
C
            IF(NSPMD>1) THEN
C
              CALL MY_BARRIER
C
              IF(ITASK==0)THEN   ! comm sur 1er thread
                IF(IMONM>0)CALL STOPTIME(64,1)
C
                SIZE = 3
                LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
                IF(IMONM>0) CALL STARTIME(80,1)
                CALL SPMD_EXCH_SMS6(MW6,NODNX_SMS,IAD_ELEM,FR_ELEM,SIZE,
     .                             LENR)
                IF(IMONM>0) CALL STOPTIME(80,1)
              END IF
C
              CALL MY_BARRIER
C
            END IF
C
            IF(IMONM>0.AND.ITASK==0)CALL STARTIME(64,1)
C
            DO N=NODFT1_SMS,NODLT1_SMS
              I=INDX1_SMS(N)
              I3=3*I
              I2=I3-1
              I1=I2-1
              W(I3) = MW6(1,3,I)+MW6(2,3,I)+MW6(3,3,I)
     .        +MW6(4,3,I)+MW6(5,3,I)+MW6(6,3,I)
              W(I2) = MW6(1,2,I)+MW6(2,2,I)+MW6(3,2,I)
     .        +MW6(4,2,I)+MW6(5,2,I)+MW6(6,2,I)
              W(I1) = MW6(1,1,I)+MW6(2,1,I)+MW6(3,1,I)
     .        +MW6(4,1,I)+MW6(5,1,I)+MW6(6,1,I)
            END DO
C
C BARRIER before RETURN
            CALL MY_BARRIER
C
            IF(IMONM>0.AND.ITASK==0)CALL STOPTIME(64,1)
C
          END IF

        ELSE ! IF(IDTMINS==2.OR.IDTMINS_INT/=0) <=> IDMINS==1
C
          CALL MY_BARRIER
C
          IF(ITASK==0)THEN   ! comm sur 1er thread
            IF(IMONM>0)CALL STOPTIME(64,1)
C
            IF(NSPMD > 1)THEN
              SIZE = 3
              LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
              IF(IMONM>0) CALL STARTIME(65,1)
              CALL SPMD_EXCH_SMS(W,NODNX_SMS,IAD_ELEM,FR_ELEM,SIZE,
     .                           LENR)
              IF(IMONM>0) CALL STOPTIME(65,1)
            END IF
          END IF
C BARRIER before RETURN
          CALL MY_BARRIER
C
        END IF
C--------------------------------------------
        RETURN
      END
C-------------product {w}=[K]{v}, v(numnod) using full matrix K ----
Chd|====================================================================
Chd|  SMS_MAV_LT1                   source/ams/sms_pcg.F          
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        FOAT_TO_6_FLOAT               source/system/parit.F         
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_EXCH_SMS                 source/mpi/ams/spmd_exch_sms.F
Chd|        SPMD_FI_SMS                   source/mpi/ams/spmd_fi_sms.F  
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|====================================================================
      SUBROUTINE SMS_MAV_LT1(NODFT  ,NODLT ,NUMNOD ,IADL  ,JDIL  ,
     2                      ITASK  ,DIAG_K ,LT_K  ,V      ,W     ,
     3              NODFT1_SMS,NODLT1_SMS,INDX1_SMS,NODNX_SMS,IAD_ELEM ,
     4              FR_ELEM   ,WEIGHT    ,JADI_SMS ,JDII_SMS ,LTI_SMS  ,
     5              ISKYI_SMS ,MSKYI_SMS ,FR_SMS   ,FR_RMS   ,LIST_SMS ,
     6              LIST_RMS  ,MSKYI_FI_SMS,VFI    ,IMV      ,MV      ,
     7              MV6       ,MW6       )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "com01_c.inc"
#include "parit_c.inc"
#include "sms_c.inc"
#include "task_c.inc"
#include "timeri_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER  NODFT, NODLT, ITASK, NUMNOD, IADL(*)  ,JDIL(*),
     .           NODFT1_SMS,NODLT1_SMS,INDX1_SMS(*), NODNX_SMS(*),
     .           IAD_ELEM(2,NSPMD+1) ,FR_ELEM(*),WEIGHT(*),
     .           JADI_SMS(*),JDII_SMS(*),
     .           ISKYI_SMS(LSKYI_SMS,*),FR_SMS(NSPMD+1),FR_RMS(NSPMD+1),
     .           LIST_SMS(*), LIST_RMS(*), IMV(*)
C     REAL
        my_real
     .    DIAG_K(*), W(*), LT_K(*)  ,V(*), LTI_SMS(*), MSKYI_SMS(*),
     .    MSKYI_FI_SMS(*), VFI(*), MV(*)
        DOUBLE PRECISION MV6(6,*), MW6(6,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,K,N, LOC_PROC, M, KK, KMV
        INTEGER SIZE, LENR, JAD, DIR, L, LLT
        my_real
     .     L_K
C-----------------------------
        DO N=NODFT1_SMS,NODLT1_SMS
          I=INDX1_SMS(N)
          W(I)=DIAG_K(I)*V(I)*WEIGHT(I)
        ENDDO
C
        IF(IDTMINS/=0)THEN
          DO N=NODFT1_SMS,NODLT1_SMS
            I=INDX1_SMS(N)
            DO J =IADL(I),IADL(I+1)-1
              K =ABS(JDIL(J))
              L_K = LT_K(J)
              W(I) = W(I) + L_K*V(K)
c         W(K) = W(K) + L_K*V(I)
            ENDDO
          ENDDO
        END IF
C
        IF(IDTMINS==2.OR.IDTMINS_INT/=0)THEN
C
          IF(IPARIT==0)THEN
            DO N=NODFT1_SMS,NODLT1_SMS
              I=INDX1_SMS(N)
              DO J =JADI_SMS(I),JADI_SMS(I+1)-1
                K =JDII_SMS(J)
                L_K = LTI_SMS(J)
                W(I) = W(I) +L_K*V(K)
c           W(K) = W(K) +L_K*V(I)
              END DO
            END DO
          END IF
C
          IF(NSPMD>1) THEN
C
            CALL MY_BARRIER
C
            IF(ITASK==0)THEN   ! comm sur 1er thread
C
              SIZE = 1
              IF(IMONM>0) CALL STARTIME(65,1)
              CALL SPMD_FI_SMS(V,NODNX_SMS,SIZE,VFI,FR_RMS,
     .                         FR_SMS,LIST_RMS,LIST_SMS)
              IF(IMONM>0) CALL STOPTIME(65,1)

            END IF
          END IF
C
          IF(IPARIT==0)THEN
            IF(NSPMD>1) THEN
              IF(ITASK==0)THEN
                KK = 0
                LOC_PROC=ISPMD+1
                M = 1
                DO L = 1, NSPMD
C
                  DO K=FR_RMS(L),FR_RMS(L+1)-1
                    I=LIST_RMS(K)
                    KK = KK + 1
                    IF(I==0)CYCLE
                    W(I) = W(I) -MSKYI_FI_SMS(K)*VFI(KK)
                  END DO
C
                  IF(L/=LOC_PROC)THEN
                    DO K=FR_SMS(L),FR_SMS(L+1)-1
                      I=LIST_SMS(M)
                      KK= KK + 1
                      M = M + 1
                      IF(I==0)CYCLE
                      W(I) = W(I) -MSKYI_SMS(K)*VFI(KK)
                    END DO
                  END IF
C
                END DO
              END IF
            END IF
C
          ELSE
C
            KMV= 0
            DO N=NODFT1_SMS,NODLT1_SMS
              I=INDX1_SMS(N)
              DO J =JADI_SMS(I),JADI_SMS(I+1)-1
                K =JDII_SMS(J)
                L_K = LTI_SMS(J)
                KMV =KMV + 1
                IMV(KMV)=I
                MV(KMV) =L_K*V(K)
              END DO
            END DO
C
            IF(NSPMD>1) THEN
C
Cafter gather VFI
              CALL MY_BARRIER
C
              KK = 0
              LOC_PROC=ISPMD+1
              M = 1
              DO L = 1, NSPMD
C
                DO K=FR_RMS(L),FR_RMS(L+1)-1
                  I=LIST_RMS(K)
                  KK = KK + 1
                  IF(I == 0 .OR. NODNX_SMS(I) < NODFT1_SMS .OR.
     .            NODLT1_SMS < NODNX_SMS(I))CYCLE
                  KMV =KMV + 1
                  IMV(KMV)=I
                  MV(KMV) = -MSKYI_FI_SMS(K)*VFI(KK)
                END DO
C
                IF(L/=LOC_PROC)THEN
                  DO K=FR_SMS(L),FR_SMS(L+1)-1
                    I=LIST_SMS(M)
                    KK= KK + 1
                    M = M + 1
                    IF(I == 0 .OR. NODNX_SMS(I) < NODFT1_SMS .OR.
     .                NODLT1_SMS < NODNX_SMS(I))CYCLE
                    KMV =KMV + 1
                    IMV(KMV)=I
                    MV(KMV) = -MSKYI_SMS(K)*VFI(KK)
                  END DO
                END IF
C
              END DO
C
            END IF
C
            CALL FOAT_TO_6_FLOAT(1,KMV,MV,MV6)
C
            DO N=NODFT1_SMS,NODLT1_SMS
              I=INDX1_SMS(N)
              DO J=1,6
                MW6(J,I)=ZERO
              END DO
            END DO
C
            DO K=1,KMV
              I=IMV(K)
              DO J=1,6
                MW6(J,I) = MW6(J,I)+MV6(J,K)
              END DO
            END DO
C
            DO N=NODFT1_SMS,NODLT1_SMS
              I=INDX1_SMS(N)
              W(I) = W(I)
     .        +MW6(1,I)+MW6(2,I)+MW6(3,I)
     .        +MW6(4,I)+MW6(5,I)+MW6(6,I)
            END DO
C
          END IF

        END IF
C
        IF(NSPMD>1) THEN
C
          CALL MY_BARRIER
C
          IF(ITASK==0)THEN   ! comm sur 1er thread
C
            SIZE = 1
            LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
            IF(IMONM>0) CALL STARTIME(65,1)
            CALL SPMD_EXCH_SMS(W,NODNX_SMS,IAD_ELEM,FR_ELEM,SIZE,
     .                         LENR)
            IF(IMONM>0) CALL STOPTIME(65,1)
          END IF
        END IF
C
        CALL MY_BARRIER
C
C--------------------------------------------
        RETURN
      END
C-------------product {w}=[K]{v}, v(numnod) using full matrix K ----
Chd|====================================================================
Chd|  SMS_MAV_LT2                   source/ams/sms_pcg.F          
Chd|-- called by -----------
Chd|        SMS_INIST                     source/ams/sms_proj.F         
Chd|-- calls ---------------
Chd|        FOAT_TO_6_FLOAT               source/system/parit.F         
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_EXCH_SMS                 source/mpi/ams/spmd_exch_sms.F
Chd|        SPMD_FI_SMS                   source/mpi/ams/spmd_fi_sms.F  
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|====================================================================
      SUBROUTINE SMS_MAV_LT2(NODFT  ,NODLT ,NUMNOD ,IADL  ,JDIL  ,
     2                      ITASK  ,DIAG_K ,LT_K  ,V      ,W     ,
     3              NODFT1_SMS,NODLT1_SMS,INDX1_SMS,NODNX_SMS,IAD_ELEM ,
     4              FR_ELEM   ,WEIGHT    ,JADI_SMS ,JDII_SMS ,LTI_SMS  ,
     5              ISKYI_SMS ,MSKYI_SMS ,FR_SMS   ,FR_RMS   ,LIST_SMS ,
     6              LIST_RMS  ,MSKYI_FI_SMS,VFI    ,IMV      ,MV      ,
     7              MV6       ,MW6       )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "com01_c.inc"
#include "parit_c.inc"
#include "sms_c.inc"
#include "task_c.inc"
#include "timeri_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER  NODFT, NODLT, ITASK, NUMNOD, IADL(*)  ,JDIL(*),
     .           NODFT1_SMS,NODLT1_SMS,INDX1_SMS(*), NODNX_SMS(*),
     .           IAD_ELEM(2,NSPMD+1) ,FR_ELEM(*),WEIGHT(*),
     .           JADI_SMS(*),JDII_SMS(*),
     .           ISKYI_SMS(LSKYI_SMS,*),FR_SMS(NSPMD+1),FR_RMS(NSPMD+1),
     .           LIST_SMS(*), LIST_RMS(*), IMV(*)
C     REAL
        my_real
     .    DIAG_K(*), W(*), LT_K(*)  ,V(*), LTI_SMS(*), MSKYI_SMS(*),
     .    MSKYI_FI_SMS(*), VFI(*), MV(*)
        DOUBLE PRECISION MV6(6,*), MW6(6,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,K,N, LOC_PROC, M, KK, KMV
        INTEGER SIZE, LENR, JAD, DIR, L, LLT
        my_real
     .     L_K
C-----------------------------
C
        DO N=NODFT1_SMS,NODLT1_SMS
          I=INDX1_SMS(N)
          W(I)=V(I)*WEIGHT(I)
        ENDDO
C
        IF(IDTMINS/=0)THEN
          DO N=NODFT1_SMS,NODLT1_SMS
            I=INDX1_SMS(N)
            IF(DIAG_K(I)/=ZERO)THEN
              DO J =IADL(I),IADL(I+1)-1
                K =ABS(JDIL(J))
                IF(DIAG_K(K)/=ZERO)THEN
                  L_K = LT_K(J)/SQRT(DIAG_K(I)*DIAG_K(K))
                  W(I) = W(I) + L_K*V(K)
                END IF
              ENDDO
            END IF
          ENDDO
        END IF
C
        IF(IDTMINS==2.OR.IDTMINS_INT/=0)THEN
C
          IF(IPARIT==0)THEN
            DO N=NODFT1_SMS,NODLT1_SMS
              I=INDX1_SMS(N)
              IF(DIAG_K(I)/=ZERO)THEN
                DO J =JADI_SMS(I),JADI_SMS(I+1)-1
                  K =JDII_SMS(J)
                  IF(DIAG_K(K)/=ZERO)THEN
                    L_K = LTI_SMS(J)/SQRT(DIAG_K(I)*DIAG_K(K))
                    W(I) = W(I) +L_K*V(K)
                  END IF
c             W(K) = W(K) +L_K*V(I)
                END DO
              END IF
            END DO
          END IF
C
          IF(NSPMD>1) THEN
C
            CALL MY_BARRIER
C
            IF(ITASK==0)THEN   ! comm sur 1er thread
C
              SIZE = 1
              IF(IMONM>0) CALL STARTIME(65,1)
              CALL SPMD_FI_SMS(V,NODNX_SMS,SIZE,VFI,FR_RMS,
     .                         FR_SMS,LIST_RMS,LIST_SMS)
              IF(IMONM>0) CALL STOPTIME(65,1)

            END IF
          END IF
C
          IF(IPARIT==0)THEN
            IF(NSPMD>1) THEN
              IF(ITASK==0)THEN
                KK = 0
                LOC_PROC=ISPMD+1
                M = 1
                DO L = 1, NSPMD
C
                  DO K=FR_RMS(L),FR_RMS(L+1)-1
                    I=LIST_RMS(K)
                    KK = KK + 1
                    IF(I==0)CYCLE
                    W(I) = W(I) -MSKYI_FI_SMS(K)*VFI(KK)
                  END DO
C
                  IF(L/=LOC_PROC)THEN
                    DO K=FR_SMS(L),FR_SMS(L+1)-1
                      I=LIST_SMS(M)
                      KK= KK + 1
                      M = M + 1
                      IF(I==0)CYCLE
                      W(I) = W(I) -MSKYI_SMS(K)*VFI(KK)
                    END DO
                  END IF
C
                END DO
              END IF
            END IF
C
          ELSE
C
            KMV= 0
            DO N=NODFT1_SMS,NODLT1_SMS
              I=INDX1_SMS(N)
              DO J =JADI_SMS(I),JADI_SMS(I+1)-1
                K =JDII_SMS(J)
                IF(DIAG_K(I)/=ZERO.AND.DIAG_K(K)/=ZERO)THEN
                  L_K = LTI_SMS(J)/SQRT(DIAG_K(I)*DIAG_K(K))
                ELSE
                  L_K = ZERO
                END IF
                KMV =KMV + 1
                IMV(KMV)=I
                MV(KMV) =L_K*V(K)
              END DO
            END DO
C
            IF(NSPMD>1) THEN
C
Cafter gather VFI
              CALL MY_BARRIER
C
              KK = 0
              LOC_PROC=ISPMD+1
              M = 1
              DO L = 1, NSPMD
C
                DO K=FR_RMS(L),FR_RMS(L+1)-1
                  I=LIST_RMS(K)
                  KK = KK + 1
                  IF(I == 0 .OR. NODNX_SMS(I) < NODFT1_SMS .OR.
     .            NODLT1_SMS < NODNX_SMS(I))CYCLE
                  KMV =KMV + 1
                  IMV(KMV)=I
                  MV(KMV) = -MSKYI_FI_SMS(K)*VFI(KK)
                END DO
C
                IF(L/=LOC_PROC)THEN
                  DO K=FR_SMS(L),FR_SMS(L+1)-1
                    I=LIST_SMS(M)
                    KK= KK + 1
                    M = M + 1
                    IF(I == 0 .OR. NODNX_SMS(I) < NODFT1_SMS .OR.
     .              NODLT1_SMS < NODNX_SMS(I))CYCLE
                    KMV =KMV + 1
                    IMV(KMV)=I
                    MV(KMV) = -MSKYI_SMS(K)*VFI(KK)
                  END DO
                END IF
C
              END DO
C
            END IF
C
            CALL FOAT_TO_6_FLOAT(1,KMV,MV,MV6)
C
            DO N=NODFT1_SMS,NODLT1_SMS
              I=INDX1_SMS(N)
              DO J=1,6
                MW6(J,I)=ZERO
              END DO
            END DO
C
            DO K=1,KMV
              I=IMV(K)
              DO J=1,6
                MW6(J,I) = MW6(J,I)+MV6(J,K)
              END DO
            END DO
C
            DO N=NODFT1_SMS,NODLT1_SMS
              I=INDX1_SMS(N)
              W(I) = W(I)
     .        +MW6(1,I)+MW6(2,I)+MW6(3,I)
     .        +MW6(4,I)+MW6(5,I)+MW6(6,I)
            END DO
C
          END IF

        END IF
C
        IF(NSPMD>1) THEN
C
          CALL MY_BARRIER
C
          IF(ITASK==0)THEN   ! comm sur 1er thread
C
            SIZE = 1
            LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
            IF(IMONM>0) CALL STARTIME(65,1)
            CALL SPMD_EXCH_SMS(W,NODNX_SMS,IAD_ELEM,FR_ELEM,SIZE,
     .                         LENR)
            IF(IMONM>0) CALL STOPTIME(65,1)
          END IF
        END IF
C
        CALL MY_BARRIER
C
C--------------------------------------------
        RETURN
      END






