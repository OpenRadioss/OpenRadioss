Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  SMS_BUILD_MAT_2               source/ams/sms_build_mat_2.F  
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SMS_BUILD_DIAG                source/ams/sms_build_diag.F   
Chd|        SPMD_LIST_SMS                 source/mpi/ams/spmd_sms.F     
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTSTAMP_MOD                  share/modules/intstamp_mod.F  
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SMS_BUILD_MAT_2(
     1             ITASK    ,NODFT   ,NODLT  ,
     2             IXC      ,IPARG   ,IXS      ,IXT      ,IXP     ,
     3             IXR      ,IXTG    ,NODNX_SMS,MS      ,MS0      ,
     4             INDX1_SMS,INDX2_SMS,JAD_SMS ,JDI_SMS  ,LT_SMS   ,
     .             KAD_SMS ,KDI_SMS  ,LTK_SMS  ,PK_SMS   ,NODII_SMS,
     5             JADC_SMS ,JADS_SMS,JADT_SMS,JADP_SMS ,JADR_SMS ,
     6             JADTG_SMS,DIAG_SMS,TAGPRT_SMS,TAGREL_SMS,
     7             IPARTS   ,IPARTQ  ,IPARTC   ,IPARTT   ,IPARTP   ,
     8             IPARTR   ,IPARTUR  ,IPARTTG ,IPARTX   ,IAD_ELEM ,
     9             FR_ELEM  ,NPBY    ,LPBY,TAGSLV_RBY_SMS,LAD_SMS  ,
     A             JSM_SMS ,DMELTG   ,DMELC    ,MSKYI_SMS,
     B             ISKYI_SMS,JADI_SMS,JDII_SMS ,LTI_SMS  ,NODXI_SMS,
     C             DMELS    ,DMELTR  ,DMELP    ,DMELRT   ,IGEO     ,
     D             FR_SMS   ,FR_RMS  ,EV       ,IPARI    ,INTBUF_TAB,
     E             KINET ,TAGSLV_I21_SMS,JADI21_SMS,INTSTAMP,
     F             IXS10 ,JADS10_SMS,ILINK     ,RLINK    ,NNLINK   ,
     G             LNLINK   ,TAG_LNK_SMS,LJOINT,IADCJ    ,FR_CJ    ,
     H             ITAB ,WEIGHT  ,DMINT2   ,ELBUF_TAB,TAGMSR_RBY_SMS,
     I             NPRW ,LPRW    ,FR_WALL  ,NRWL_SMS ,RBY           ,
     J             X    ,A       ,AR       ,IN       ,V             ,
     K             VR   ,IRBE2   ,LRBE2    ,IRBE3    ,LRBE3         ,
     L             IAD_RBE3M ,FR_RBE3M,NATIV_SMS,T2MAIN_SMS,T2FAC_SMS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MOD_SMS_WORK
      USE INTSTAMP_MOD
      USE MESSAGE_MOD
      USE ELBUFDEF_MOD 
      USE INTBUFDEF_MOD 
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
#include      "kincod_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "sms_c.inc"
#include      "scr17_c.inc"
#include      "task_c.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITASK, NODFT, NODLT,
     .        IPARG(NPARG,*), IXC(NIXC,*), IXS(NIXS,*), IXT(NIXT,*), 
     .        IXP(NIXP,*), IXR(NIXR,*), IXTG(NIXTG,*), 
     .        NODNX_SMS(*), JAD_SMS(*), JDI_SMS(*), 
     .        KAD_SMS(*), KDI_SMS(*), PK_SMS(*),
     .        JADC_SMS(4,*), JADS_SMS(8,*), 
     .        JADT_SMS(2,*), JADP_SMS(2,*),
     .        JADR_SMS(3,*), JADTG_SMS(3,*),
     .        INDX1_SMS(*), INDX2_SMS(*), TAGPRT_SMS(*), TAGREL_SMS(*), 
     .        IPARTS(*), IPARTQ(*), IPARTC(*), IPARTT(*),
     .        IPARTP(*), IPARTR(*), IPARTUR(*), IPARTTG(*), IPARTX(*),
     .        IAD_ELEM(2,NSPMD+1) ,FR_ELEM(*),
     .        NPBY(NNPBY,*), LPBY(*), TAGSLV_RBY_SMS(*),
     .        LAD_SMS(*), JSM_SMS(*), 
     .        ISKYI_SMS(LSKYI_SMS,*),
     .        JADI_SMS(*), JDII_SMS(*), NODXI_SMS(*), NODII_SMS(*), 
     .        IGEO(NPROPGI,*), 
     .        FR_RMS(NSPMD+1), FR_SMS(NSPMD+1),
     .        IPARI(NPARI,*), KINET(*),
     .        TAGSLV_I21_SMS(*), JADI21_SMS(*), 
     .        IXS10(6,*), JADS10_SMS(6,*),
     .        ILINK(*), RLINK(*), NNLINK(10,*), LNLINK(*), 
     .        TAG_LNK_SMS(*), LJOINT(*), FR_CJ(*),IADCJ(NSPMD+1,*),
     .        ITAB(*), WEIGHT(*), TAGMSR_RBY_SMS(*),
     .        NPRW(*), LPRW(*), FR_WALL(*), NRWL_SMS(*),
     .        IRBE2(*), LRBE2(*), 
     .        IRBE3(*), LRBE3(*), IAD_RBE3M(*),FR_RBE3M(*), NATIV_SMS(*),
     .        T2MAIN_SMS(6,*)
      my_real
     .        MS(*), MS0(*), LT_SMS(*), LTK_SMS(*), DIAG_SMS(*),
     .        DMELTG(*), DMELC(*), MSKYI_SMS(*), LTI_SMS(*),
     .        DMELS(*), DMELTR(*), DMELP(*), DMELRT(*), EV(*),
     .        DMINT2(4,*), RBY(NRBY,*), X(3,*), A(3,*), AR(3,*), IN(*),
     .        V(3,*), VR(3,*),T2FAC_SMS(*)
      TYPE(INTSTAMP_DATA) INTSTAMP(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, KN, IKN, JJ, KK, II, IJ, IK, N, M, NN, P, LOC_PROC
      INTEGER NG, ITY, NEL, NFT, ISOLNOD,MLW,LFT, LLT,
     .        KAD, NPT, IHBE, ICNOD, ISTRA, IEXPAN, IE, J1,
     .        ILOC4(4), IG, IGTYP, IERROR, IPERM1(6), IPERM2(6),IPENTA6(6)
      INTEGER MSR, NSN, KI, KJ, KL, NSR
      INTEGER SIZE, LENR, IAD, L, JI,TAGA(NUMNOD),NAD_SMS(NUMNOD),
     .        KADI_SMS(NUMNOD+1), NADI_SMS(NUMNOD), TAG8(NUMNOD), 
     .        NAD_SMS_0(NUMNOD)
      INTEGER NTY, ILAGM, N1, N2, N3, N4,
     .        NMN,ILEV, KSN, KMULT
      my_real
     .        MELE4, MELE12, XN, LTIJ, MSLV,
     .        IXX, IYY, IZZ, XX, YY, ZZ, MAS, AWORK(3,NUMNOD),
     .        VRX, VRY, VRZ, V1, V2, V3, GX, GY, GZ, XNOD,
     .        FAC_SCAL_I,FAC_SCAL_J
C-----
      INTEGER, DIMENSION(:), ALLOCATABLE :: IMV
      my_real
     .       , DIMENSION(:), ALLOCATABLE :: MV
      DOUBLE PRECISION
     .       , DIMENSION(:,:), ALLOCATABLE :: MV6
      my_real,
     .   DIMENSION(:), POINTER :: OFFG
C-----
      DATA ILOC4/1,3,6,5/
      DATA IPERM1/1,2,3,1,2,3/
      DATA IPERM2/2,3,1,4,4,4/
      DATA IPENTA6/1,2,3,5,6,7/
C-----------------------------------------------
C reset enforcement of contact sorting
!$OMP SINGLE
      KFORSMS=0
!$OMP END SINGLE
C
      IF(IPARIT/=0)THEN
        IF(DEBUG(9)==0)THEN
          ALLOCATE(IMV(2*NISKY_SMS+FR_RMS(NSPMD+1)),
     .           MV (2*NISKY_SMS+FR_RMS(NSPMD+1)),
     .           MV6(6,2*NISKY_SMS+FR_RMS(NSPMD+1)),STAT=IERROR)
        ELSE
          ALLOCATE(IMV(NNZ_SMS+2*NISKY_SMS+FR_RMS(NSPMD+1)),
     .           MV (NNZ_SMS+2*NISKY_SMS+FR_RMS(NSPMD+1)),
     .           MV6(6,NNZ_SMS+2*NISKY_SMS+FR_RMS(NSPMD+1)),STAT=IERROR)
        END IF
        IF(IERROR/=0) THEN
          CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .                C1='(/DT/.../AMS)')
          CALL ARRET(2)
        ENDIF
        IF(ITASK==0)THEN
          ALLOCATE(MW6(6,NUMNOD),STAT=IERROR)
          IF(IERROR/=0) THEN
            CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .                  C1='(/DT/.../AMS)')
            CALL ARRET(2)
          ENDIF
        END IF
      END IF
      IF(ITASK==0)THEN
        ALLOCATE(LIST_SMS(FR_SMS(NSPMD+1)),LIST_RMS(FR_RMS(NSPMD+1)),
     .        MSKYI_FI_SMS(FR_RMS(NSPMD+1)),STAT=IERROR)
        IF(IERROR/=0) THEN
          CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .                C1='(/DT/.../AMS)')
          CALL ARRET(2)
        ENDIF
      ENDIF
C
      NODXI_SMS(NODFT:NODLT)=NODNX_SMS(NODFT:NODLT)
C
      CALL MY_BARRIER()
C
C     si /DT/INTER/AMS sans /DT/AMS
      IF(IDTMINS/=2)GO TO 100
C
!$OMP DO SCHEDULE(DYNAMIC,1)
      DO NG = 1, NGROUP
C
      IF(TAGREL_SMS(NG)==0)GOTO 250
C
      ITY     = IPARG(5,NG)
      MLW     = IPARG(1,NG)
      NEL     = IPARG(2,NG)
      NFT     = IPARG(3,NG)
      KAD     = IPARG(4,NG)
      NPT     = IPARG(6,NG)
      ICNOD   = IPARG(11,NG)
      ISTRA   = IPARG(44,NG)
      IHBE    = IPARG(23,NG)
      ISOLNOD = IPARG(28,NG)
      IEXPAN  = IPARG(49,NG)
      IF (IHBE==101) THEN
        IHBE=1
      ELSEIF(IHBE==102) THEN
        IHBE=0
      ELSEIF(IHBE==112) THEN
        IHBE=0
      ENDIF
      LFT   = 1
      LLT   = NEL
      IF (ITY==1.AND.ISOLNOD==4) THEN
        OFFG => ELBUF_TAB(NG)%GBUF%OFF
        DO J=LFT,LLT
         IE=NFT+J

         MELE4=ZERO
         IF(MLW/=0)THEN
          IF (OFFG(J) > ZERO) THEN
            MELE4=HALF*DMELS(IE)
          END IF
         END IF
C
C  Me=[ 3*dmels    -dmels ...        -dmels ]
C     [  -dmels   3*dmels ...        -dmels ]
C     [...]
C  w^2 < 2k / (m+4*dmels)
C  but dt = 2/w =sqrt( 2*(m+dmels) /k) => 4*dmels=dmels(mqviscb)/2
C                                     <=> mele12=dmels(mqviscb)/2/4
         MELE12=FOURTH*MELE4
         DO K=1,4
           I=IXS(1+ILOC4(K),IE)

           IJ=JADS_SMS(K,IE)
           DO KK=1,4
             JJ = IXS(1+ILOC4(KK),IE)
             IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
                LTK_SMS (IJ)=-MELE12
                IJ=IJ+1
             END IF
           END DO
         END DO
        END DO
      ELSEIF (ITY==1.AND.ISOLNOD==6) THEN
        OFFG => ELBUF_TAB(NG)%GBUF%OFF
        DO J=LFT,LLT
         IE=NFT+J

         MELE4=ZERO
         IF(MLW/=0)THEN
          IF (OFFG(J) > ZERO) THEN
            MELE4=HALF*DMELS(IE)
          END IF
         END IF
C
C  Me=[ 3*dmels    -dmels ...        -dmels ]
C     [  -dmels   3*dmels ...        -dmels ]
C     [...]
C  w^2 < 2k / (m+4*dmels)
C  but dt = 2/w =sqrt( 2*(m+dmels) /k) => 4*dmels=dmels(mqviscb)/2
C                                     <=> mele12=dmels(mqviscb)/2/4
         MELE12=ONE_OVER_6*MELE4
         DO K=1,6
           I=IXS(1+IPENTA6(K),IE)

           IJ=JADS_SMS(K,IE)
           DO KK=1,6
             JJ = IXS(1+IPENTA6(KK),IE)
             IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
                LTK_SMS (IJ)=-MELE12
                IJ=IJ+1
             END IF
           END DO
         END DO
        END DO
      ELSEIF(ITY==1.AND.ISOLNOD==8)THEN
        OFFG => ELBUF_TAB(NG)%GBUF%OFF
        DO J=LFT,LLT
         IE=NFT+J

         KMULT=1

         MELE4=ZERO
         IF(MLW/=0)THEN
          IF (OFFG(J) > ZERO) THEN
            KMULT=0
            XNOD=ZERO
            DO K=1,8
              I=IXS(1+K,IE)
              TAGA(I)=0
            END DO
            DO K=1,8
              I=IXS(1+K,IE)
              IF(TAGA(I)==0)XNOD=XNOD+ONE
              TAGA(I)=TAGA(I)+1
              KMULT=MAX(KMULT,TAGA(I))
            END DO
C peut etre instable pour les prismes
C           MELE4 =HALF*DMELS(IE)
            MELE4 =KMULT*HALF*DMELS(IE)
C
C  Me=[ 7*dmels    -dmels ...        -dmels ]
C     [  -dmels   7*dmels ...        -dmels ]
C     [...]
C  w^2 < 2k / (m+8*dmels)
C  but dt = 2/w =sqrt( 2*(m+dmels) /k) => 8*dmels=dmels(mqviscb)/2
C                                     <=> mele12=dmels=dmels(mqviscb)/2/8
C
C  Pentas (note : 2 x plus de masse que necessaire sur les nds non doubles)
C  Me=[ 5*dmels    -dmels   ...        -dmels ]
C     [  -dmels   5*dmels   ...        -dmels ]
C     [  -dmels    -dmels  5*dmels     -dmels ]
C     [...]
C  nds doubles w^2 < 4k / (2*m+6*dmels) , nds simples w^2 < 2k / (m+6*dmels)
C  but dt = 2/w =sqrt( 2*(m+dmels) /k) => 3*dmels=dmels(mqviscb)/2
C                                     <=> mele12=dmels=dmels(mqviscb)/2/3
C                                                     =kmult*dmels(mqviscb)/2/6
            MELE12=(ONE/XNOD)*MELE4
          ELSE
            MELE12=ZERO
          END IF
         ELSE
C
C void elements may be into contact
           MELE12=ZERO
         END IF

         DO K=1,8
           I=IXS(1+K,IE)
           TAGA(I)=0
           TAG8(K)=0
         END DO

         DO K=1,8
           I=IXS(1+K,IE)
           IF(TAGA(I)/=0)THEN
             TAG8(K)=1
           ELSE
             TAGA(I)=1
           END IF
         END DO
         DO K=1,8
           I=IXS(1+K,IE)
           IF(TAG8(K)/=0)CYCLE

           IJ=JADS_SMS(K,IE)
           DO KK=1,8
             JJ = IXS(1+KK,IE)
             IF(TAG8(KK)/=0) CYCLE

             IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
                LTK_SMS (IJ)=-MELE12
                IJ=IJ+1
             END IF
           END DO
         END DO
        END DO
      ELSEIF(ITY==1.AND.ISOLNOD==10)THEN
        IF(IDT1TET10/=0)THEN
          OFFG => ELBUF_TAB(NG)%GBUF%OFF
          DO J=LFT,LLT
           IE=NFT+J
           J1=IE-NUMELS8

           MELE4=ZERO
           IF(MLW/=0)THEN
            IF (OFFG(J) > ZERO) THEN
             MELE4 = HALF*DMELS(IE)
C
C             Q : Quelles VP pour M-1K ?  M=[ Mvettex+9dm, -dm      ,.....    ]
C                     [  -dm   , Medge+9dm, -dm, ....]
C                      .........
C             Mvertex = Mass/32, Medge=7*Mass/48
C
C             A: Supposed lambda(M) > Mvertex+10dm
C
             MELE4 = MELE4/THIRTY2
            END IF
           END IF

           MELE12=MELE4/TEN

           DO K=1,4
             I=IXS(1+ILOC4(K),IE)

             IJ=JADS_SMS(K,IE)
             DO KK=1,4
               JJ = IXS(1+ILOC4(KK),IE)
               IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
            LTK_SMS (IJ)=-MELE12
            IJ=IJ+1
               END IF
             END DO

             DO KK=1,6
               JJ = IXS10(KK,J1)
               IF(JJ==0) CYCLE

               IF(.NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
            LTK_SMS (IJ)=-MELE12
            IJ=IJ+1
               END IF
             END DO
           END DO

           DO K=1,6

             I =IXS10(K,J1)
             IF(I==0)CYCLE

             IJ=JADS10_SMS(K,J1)

             DO KK=1,4
               JJ = IXS(1+ILOC4(KK),IE)
               IF(.NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
            LTK_SMS (IJ)=-MELE12
            IJ=IJ+1
               END IF
             END DO

             DO KK=1,6
               JJ = IXS10(KK,J1)
               IF(JJ==0) CYCLE

               IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
            LTK_SMS (IJ)=-MELE12
            IJ=IJ+1
               END IF
             END DO

           END DO

C          nd milieu inexistant, transfert aux sommets (symetrie dans JSM_SMS...)
           DO K=1,6

             I =IXS10(K,J1)
             IF(I/=0)CYCLE

             I=IXS(1+ILOC4(IPERM1(K)),IE)
             IJ=JADS_SMS(IPERM1(K),IE)

             DO KK=1,4
               JJ = IXS(1+ILOC4(KK),IE)
               IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
            LTK_SMS (IJ)=LTK_SMS (IJ)-HALF*MELE12
            IJ=IJ+1
               END IF
             END DO

             DO KK=1,6
               JJ = IXS10(KK,J1)
               IF(JJ==0) CYCLE

               IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
            LTK_SMS (IJ)=LTK_SMS (IJ)-HALF*MELE12
            IJ=IJ+1
               END IF
             END DO

             I=IXS(1+ILOC4(IPERM2(K)),IE)
             IJ=JADS_SMS(IPERM2(K),IE)

             DO KK=1,4
               JJ = IXS(1+ILOC4(KK),IE)
               IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
            LTK_SMS (IJ)=LTK_SMS (IJ)-HALF*MELE12
            IJ=IJ+1
               END IF
             END DO

             DO KK=1,6
               JJ = IXS10(KK,J1)
               IF(JJ==0) CYCLE

               IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
            LTK_SMS (IJ)=LTK_SMS (IJ)-HALF*MELE12
            IJ=IJ+1
               END IF
             END DO
           END DO
          END DO
        ELSE ! IF(IDT1TET10/=0)THEN (old way for ascending compatibility)
          OFFG => ELBUF_TAB(NG)%GBUF%OFF
          DO J=LFT,LLT
           IE=NFT+J
           J1=IE-NUMELS8

           MELE4=ZERO
           IF(MLW/=0)THEN
            IF (OFFG(J) > ZERO) THEN
             MELE4 = HALF*DMELS(IE)
C
C             Q : Quelles VP pour M-1K ?  M=[ Mvettex+9dm, -dm      ,.....    ]
C                     [  -dm   , Medge+9dm, -dm, ....]
C                      .........
C             Mvertex = Mass/32, Medge=7*Mass/48
C
C             A: Supposed lambda(M) > Mvertex+10dm
C
             MELE4 = MELE4*SEVEN/FOURTY8
            END IF
           END IF

           MELE12=MELE4/NINE

           DO K=1,4
             I=IXS(1+ILOC4(K),IE)

             IJ=JADS_SMS(K,IE)
             DO KK=1,4
               JJ = IXS(1+ILOC4(KK),IE)
               IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
            LTK_SMS (IJ)=-MELE12
            IJ=IJ+1
               END IF
             END DO

             DO KK=1,6
               JJ = IXS10(KK,J1)
               IF(JJ==0) CYCLE

               IF(.NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
            LTK_SMS (IJ)=-MELE12
            IJ=IJ+1
               END IF
             END DO
           END DO

           DO K=1,6

             I =IXS10(K,J1)
             IF(I==0)CYCLE

             IJ=JADS10_SMS(K,J1)

             DO KK=1,4
               JJ = IXS(1+ILOC4(KK),IE)
               IF(.NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
            LTK_SMS (IJ)=-MELE12
            IJ=IJ+1
               END IF
             END DO

             DO KK=1,6
               JJ = IXS10(KK,J1)
               IF(JJ==0) CYCLE

               IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
            LTK_SMS (IJ)=-MELE12
            IJ=IJ+1
               END IF
             END DO

           END DO

C          nd milieu inexistant, transfert aux sommets (symetrie dans JSM_SMS...)
           DO K=1,6

             I =IXS10(K,J1)
             IF(I/=0)CYCLE

             I=IXS(1+ILOC4(IPERM1(K)),IE)
             IJ=JADS_SMS(IPERM1(K),IE)

             DO KK=1,4
               JJ = IXS(1+ILOC4(KK),IE)
               IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
            LTK_SMS (IJ)=LTK_SMS (IJ)-HALF*MELE12
            IJ=IJ+1
               END IF
             END DO

             DO KK=1,6
               JJ = IXS10(KK,J1)
               IF(JJ==0) CYCLE

               IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
            LTK_SMS (IJ)=LTK_SMS (IJ)-HALF*MELE12
            IJ=IJ+1
               END IF
             END DO

             I=IXS(1+ILOC4(IPERM2(K)),IE)
             IJ=JADS_SMS(IPERM2(K),IE)

             DO KK=1,4
               JJ = IXS(1+ILOC4(KK),IE)
               IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
            LTK_SMS (IJ)=LTK_SMS (IJ)-HALF*MELE12
            IJ=IJ+1
               END IF
             END DO

             DO KK=1,6
               JJ = IXS10(KK,J1)
               IF(JJ==0) CYCLE

               IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
            LTK_SMS (IJ)=LTK_SMS (IJ)-HALF*MELE12
            IJ=IJ+1
               END IF
             END DO
           END DO
          END DO
        END IF
      ELSEIF(ITY==3)THEN
        OFFG => ELBUF_TAB(NG)%GBUF%OFF
        DO J=LFT,LLT
         IE=NFT+J

         MELE4=ZERO
         IF(MLW/=0)THEN
          IF (OFFG(J) > ZERO) THEN
            MELE4 =HALF*DMELC(IE)
          END IF
         END IF
         MELE12=THIRD*MELE4
         DO K=1,4
           I=IXC(1+K,IE)

           IJ=JADC_SMS(K,IE)
           DO KK=1,4
             JJ = IXC(1+KK,IE)
             IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
                LTK_SMS (IJ)=-MELE12
                IJ=IJ+1
             END IF
           END DO
         END DO
        END DO
      ELSEIF(ITY==4)THEN
       OFFG => ELBUF_TAB(NG)%GBUF%OFF
       DO J=LFT,LLT
         IE = NFT+J

         MELE4=ZERO
         IF(MLW/=0)THEN
          IF (OFFG(J) > ZERO) THEN
            MELE4 =HALF*DMELTR(IE)
          END IF
         END IF
         MELE12=MELE4
         DO K=1,2
           I=IXT(1+K,IE)
           IJ=JADT_SMS(K,IE)
           DO KK=1,2
             JJ = IXT(1+KK,IE)
             IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
                LTK_SMS (IJ)=-MELE12
                IJ=IJ+1
             END IF
           END DO
         END DO
       END DO
      ELSEIF(ITY==5)THEN
       OFFG => ELBUF_TAB(NG)%GBUF%OFF
       DO J=LFT,LLT
         IE = NFT+J

         MELE4=ZERO
         IF(MLW/=0)THEN
          IF (OFFG(J) > ZERO) THEN
            MELE4 =HALF*DMELP(IE)
          END IF
         END IF
         MELE12=MELE4
         DO K=1,2
           I=IXP(1+K,IE)

           IJ=JADP_SMS(K,IE)
           DO KK=1,2
             JJ = IXP(1+KK,IE)
             IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
                LTK_SMS (IJ)=-MELE12
                IJ=IJ+1
             END IF
           END DO
         END DO
       END DO
      ELSEIF(ITY==6)THEN
       IG = IXR(1,NFT+1)
       IGTYP =  IGEO(11,IG)
       OFFG => ELBUF_TAB(NG)%GBUF%OFF
       IF(IGTYP/=12)THEN
        DO J=LFT,LLT
          IE = NFT+J

          MELE4=ZERO
          IF(MLW/=0)THEN
           IF (OFFG(J) > ZERO) THEN
             MELE4=HALF*DMELRT(IE)
           END IF
          END IF
          MELE12=MELE4
          DO K=1,2
            I=IXR(1+K,IE)

            IJ=JADR_SMS(K,IE)
            DO KK=1,2
              JJ = IXR(1+KK,IE)
              IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
                 LTK_SMS (IJ)=-MELE12
                 IJ=IJ+1
              END IF
            END DO
          END DO
        END DO
       ELSE
        DO J=LFT,LLT
          IE = NFT+J

            MELE12=ZERO
            IF(MLW/=0)THEN
             IF (OFFG(J) > ZERO) THEN
               MELE12=HALF*DMELRT(IE)
             END IF
            END IF

            K=1
            I=IXR(1+K,IE)

            IJ=JADR_SMS(K,IE)
            KK=2
              JJ = IXR(1+KK,IE)
              IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
                 LTK_SMS (IJ)=-MELE12
                 IJ=IJ+1
              END IF

            K=2
            I=IXR(1+K,IE)

            IJ=JADR_SMS(K,IE)
            KK=1
              JJ = IXR(1+KK,IE)
              IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
                 LTK_SMS (IJ)=-MELE12
                 IJ=IJ+1
              END IF
            KK=3
              JJ = IXR(1+KK,IE)
              IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
                 LTK_SMS (IJ)=-MELE12
                 IJ=IJ+1
              END IF

            K=3
            I=IXR(1+K,IE)

            IJ=JADR_SMS(K,IE)
            KK=2
              JJ = IXR(1+KK,IE)
              IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
                 LTK_SMS (IJ)=-MELE12
                 IJ=IJ+1
              END IF

        END DO
       END IF
      ELSEIF(ITY==7)THEN
        OFFG => ELBUF_TAB(NG)%GBUF%OFF
        DO J=LFT,LLT
         IE = NFT+J

         MELE4=ZERO
         IF(MLW/=0)THEN
          IF (OFFG(J) > ZERO) THEN
            MELE4=HALF*DMELTG(IE)
          END IF
         END IF
C
C  Me=[ 2*dmeltg    -dmeltg   -dmeltg ]
C     [  -dmeltg   2*dmeltg   -dmeltg ]
C     [  -dmeltg    -dmeltg  2*dmeltg
C  w^2 < 2k / (m+3*dmeltg)
C  but dt = 2/w =sqrt( 2*(m+dmelc) /k) => 3*dmeltg=dmeltg calculated/2
C                                     <=> mele12=dmeltg/3=dmeltg calculated/2/3
         MELE12=THIRD*MELE4
         DO K=1,3
           I=IXTG(1+K,IE)

           IJ=JADTG_SMS(K,IE)
           DO KK=1,3
             JJ = IXTG(1+KK,IE)
             IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
                LTK_SMS (IJ)=-MELE12
                IJ=IJ+1
             END IF
           END DO
         END DO
        END DO
      END IF
 250  CONTINUE
      END DO
!$OMP END DO
C
      CALL MY_BARRIER()
C 
C-------------------------------------------------------------------------
C     KOMPACTING ELEMENTARY MATRIX
C-------------------------------------------------------------------------
      DO I=NODFT, NODLT
        DO IK=JAD_SMS(I),LAD_SMS(I)
          LT_SMS(IK)=ZERO
        END DO

        DO IJ=KAD_SMS(I),KAD_SMS(I+1)-1
          IK         =JAD_SMS(I)+PK_SMS(IJ)-1
          LT_SMS(IK) = LT_SMS(IK) + LTK_SMS(IJ)
        END DO
      END DO
C
      CALL MY_BARRIER()
C 
C------------
C inter/type2
C------------
      IF(ITASK==0)THEN

        DO I=1,NUMNOD
          NAD_SMS(I)=LAD_SMS(I)+1
        END DO
C
C---    T2MAIN_SMS(6) must be updated if element failure
        IF (ISMSNOK==1) THEN
          DO N=1,NINTER
            NTY   = IPARI(7,N)
            ILAGM = IPARI(33,N)
            ILEV  = IPARI(20,N)
            IF(NTY==2 .AND. ILAGM==0 .AND.ILEV/=25.AND.ILEV/=26)THEN
              NSN=IPARI(5,N)
              DO II=1,NSN
                I=INTBUF_TAB(N)%NSV(II)
                IF (I < 0) T2MAIN_SMS(6,-I)=-1
              ENDDO
            ENDIF
          END DO
        ENDIF
C
        KSN=0
        DO N=1,NINTER
          NTY   = IPARI(7,N)
          ILAGM = IPARI(33,N)
          ILEV  = IPARI(20,N)
          IF(NTY==2 .AND. ILAGM==0 .AND.ILEV/=25.AND.ILEV/=26.AND.ILEV/=27.AND.ILEV/=28)THEN
C
            KAD=IPARI(1,N)
            NSN=IPARI(5,N)
            DO II=1,NSN
              I=INTBUF_TAB(N)%NSV(II)
              L=INTBUF_TAB(N)%IRTLM(II)
              N1 = INTBUF_TAB(N)%IRECTM(4*(L-1)+1)
              N2 = INTBUF_TAB(N)%IRECTM(4*(L-1)+2)
              N3 = INTBUF_TAB(N)%IRECTM(4*(L-1)+3)
              N4 = INTBUF_TAB(N)%IRECTM(4*(L-1)+4)
    
              IF(NATIV_SMS(I)==0.AND.NATIV_SMS(N1)==0
     .                          .AND.NATIV_SMS(N2)==0
     .                          .AND.NATIV_SMS(N3)==0
     .                          .AND.NATIV_SMS(N4)==0) CYCLE

C
              IF(I > 0)THEN
                DO KJ=JAD_SMS(I),LAD_SMS(I)
                  J =JDI_SMS(KJ)
                  LTIJ = LT_SMS(KJ)
                  LT_SMS(KJ)=ZERO

                  IF (T2MAIN_SMS(1,J) == 1) THEN
C-- No Type2 + AMS on J
                    LT_SMS(NAD_SMS(J)) = LTIJ
                    LT_SMS(NAD_SMS(N1))= LTIJ
                    NAD_SMS(J) =NAD_SMS(J)+1
                    NAD_SMS(N1)=NAD_SMS(N1)+1

                    LT_SMS(NAD_SMS(J)) = LTIJ
                    LT_SMS(NAD_SMS(N2))= LTIJ
                    NAD_SMS(J) =NAD_SMS(J)+1
                    NAD_SMS(N2)=NAD_SMS(N2)+1

                    LT_SMS(NAD_SMS(J)) = LTIJ
                    LT_SMS(NAD_SMS(N3))= LTIJ
                    NAD_SMS(J) =NAD_SMS(J)+1
                    NAD_SMS(N3)=NAD_SMS(N3)+1

                    LT_SMS(NAD_SMS(J)) = LTIJ
                    LT_SMS(NAD_SMS(N4))= LTIJ
                    NAD_SMS(J) =NAD_SMS(J)+1
                    NAD_SMS(N4)=NAD_SMS(N4)+1
C
                  ELSEIF(T2MAIN_SMS(6,J)==0) THEN
C-- Type2 crossed connection between main nodes - no failure on J
C
                    LT_SMS(NAD_SMS(J)) = ZERO
                    LT_SMS(NAD_SMS(N1))= ZERO
                    NAD_SMS(J) =NAD_SMS(J)+1
                    NAD_SMS(N1)=NAD_SMS(N1)+1

                    LT_SMS(NAD_SMS(J)) = ZERO
                    LT_SMS(NAD_SMS(N2))= ZERO
                    NAD_SMS(J) =NAD_SMS(J)+1
                    NAD_SMS(N2)=NAD_SMS(N2)+1

                    LT_SMS(NAD_SMS(J)) = ZERO
                    LT_SMS(NAD_SMS(N3))= ZERO
                    NAD_SMS(J) =NAD_SMS(J)+1
                    NAD_SMS(N3)=NAD_SMS(N3)+1

                    LT_SMS(NAD_SMS(J)) = ZERO
                    LT_SMS(NAD_SMS(N4))= ZERO
                    NAD_SMS(J) =NAD_SMS(J)+1
                    NAD_SMS(N4)=NAD_SMS(N4)+1

                    IF (I>J) THEN
                      DO K =2,5
                        DO KK =2,5
                          IF (T2MAIN_SMS(K,I)/=T2MAIN_SMS(KK,J)) THEN
                            LT_SMS(NAD_SMS(T2MAIN_SMS(K,I))) = HALF*LTIJ
                            LT_SMS(NAD_SMS(T2MAIN_SMS(KK,J)))= HALF*LTIJ
                            NAD_SMS(T2MAIN_SMS(K,I)) =NAD_SMS(T2MAIN_SMS(K,I))+1
                            NAD_SMS(T2MAIN_SMS(KK,J))=NAD_SMS(T2MAIN_SMS(KK,J))+1
                          ENDIF
                        ENDDO
                      ENDDO
                    ENDIF
C
                  ELSE
C-- Type2 crossed connection between main nodes - failure of main element of j
C
                    LT_SMS(NAD_SMS(J)) = LTIJ
                    LT_SMS(NAD_SMS(N1))= LTIJ
                    NAD_SMS(J) =NAD_SMS(J)+1
                    NAD_SMS(N1)=NAD_SMS(N1)+1

                    LT_SMS(NAD_SMS(J)) = LTIJ
                    LT_SMS(NAD_SMS(N2))= LTIJ
                    NAD_SMS(J) =NAD_SMS(J)+1
                    NAD_SMS(N2)=NAD_SMS(N2)+1

                    LT_SMS(NAD_SMS(J)) = LTIJ
                    LT_SMS(NAD_SMS(N3))= LTIJ
                    NAD_SMS(J) =NAD_SMS(J)+1
                    NAD_SMS(N3)=NAD_SMS(N3)+1

                    LT_SMS(NAD_SMS(J)) = LTIJ
                    LT_SMS(NAD_SMS(N4))= LTIJ
                    NAD_SMS(J) =NAD_SMS(J)+1
                    NAD_SMS(N4)=NAD_SMS(N4)+1

                    IF (I>J) THEN
                      DO K =2,5
                        DO KK =2,5
                          IF (T2MAIN_SMS(K,I)/=T2MAIN_SMS(KK,J)) THEN
                            LT_SMS(NAD_SMS(T2MAIN_SMS(K,I))) = ZERO
                            LT_SMS(NAD_SMS(T2MAIN_SMS(KK,J)))= ZERO
                            NAD_SMS(T2MAIN_SMS(K,I)) =NAD_SMS(T2MAIN_SMS(K,I))+1
                            NAD_SMS(T2MAIN_SMS(KK,J))=NAD_SMS(T2MAIN_SMS(KK,J))+1
                          ENDIF
                        ENDDO
                      ENDDO
                    ENDIF
C
                  ENDIF
C
                END DO
              ELSE
                I=-I
                DO KJ=JAD_SMS(I),LAD_SMS(I)
                  J =JDI_SMS(KJ)
                  LTIJ = ZERO

                  IF (T2MAIN_SMS(1,J) == 1) THEN
C-- No Type2 + AMS on J
                    LT_SMS(NAD_SMS(J)) = LTIJ
                    LT_SMS(NAD_SMS(N1))= LTIJ
                    NAD_SMS(J) =NAD_SMS(J)+1
                    NAD_SMS(N1)=NAD_SMS(N1)+1

                    LT_SMS(NAD_SMS(J)) = LTIJ
                    LT_SMS(NAD_SMS(N2))= LTIJ
                    NAD_SMS(J) =NAD_SMS(J)+1
                    NAD_SMS(N2)=NAD_SMS(N2)+1

                    LT_SMS(NAD_SMS(J)) = LTIJ
                    LT_SMS(NAD_SMS(N3))= LTIJ
                    NAD_SMS(J) =NAD_SMS(J)+1
                    NAD_SMS(N3)=NAD_SMS(N3)+1

                    LT_SMS(NAD_SMS(J)) = LTIJ
                    LT_SMS(NAD_SMS(N4))= LTIJ
                    NAD_SMS(J) =NAD_SMS(J)+1
                    NAD_SMS(N4)=NAD_SMS(N4)+1

                  ELSE
C-- Type2 crossed connection between main nodes
C
                    LT_SMS(NAD_SMS(J)) = ZERO
                    LT_SMS(NAD_SMS(N1))= ZERO
                    NAD_SMS(J) =NAD_SMS(J)+1
                    NAD_SMS(N1)=NAD_SMS(N1)+1

                    LT_SMS(NAD_SMS(J)) = ZERO
                    LT_SMS(NAD_SMS(N2))= ZERO
                    NAD_SMS(J) =NAD_SMS(J)+1
                    NAD_SMS(N2)=NAD_SMS(N2)+1

                    LT_SMS(NAD_SMS(J)) = ZERO
                    LT_SMS(NAD_SMS(N3))= ZERO
                    NAD_SMS(J) =NAD_SMS(J)+1
                    NAD_SMS(N3)=NAD_SMS(N3)+1

                    LT_SMS(NAD_SMS(J)) = ZERO
                    LT_SMS(NAD_SMS(N4))= ZERO
                    NAD_SMS(J) =NAD_SMS(J)+1
                    NAD_SMS(N4)=NAD_SMS(N4)+1

                    IF (I>J) THEN
                    DO K =2,5
                      DO KK =2,5
                        IF (T2MAIN_SMS(K,I)/=T2MAIN_SMS(KK,J)) THEN
                          LT_SMS(NAD_SMS(T2MAIN_SMS(K,I))) = HALF*LTIJ
                          LT_SMS(NAD_SMS(T2MAIN_SMS(KK,J)))= HALF*LTIJ
                          NAD_SMS(T2MAIN_SMS(K,I)) =NAD_SMS(T2MAIN_SMS(K,I))+1
                          NAD_SMS(T2MAIN_SMS(KK,J))=NAD_SMS(T2MAIN_SMS(KK,J))+1
                        ENDIF
                      ENDDO
                    ENDDO
                    ENDIF
C
                  ENDIF
C
                END DO
              END IF
            END DO
C
          ELSEIF(NTY==2.AND.ILAGM==0.AND.(ILEV==25.or.ILEV==26))THEN
C
            KAD=IPARI(1,N)
            NSN=IPARI(5,N)
            DO II=1,NSN
              I=INTBUF_TAB(N)%NSV(II)
              KSN=KSN+1

              IF(WEIGHT(ABS(I))/=1)CYCLE

              L=INTBUF_TAB(N)%IRTLM(II)
              N1 = INTBUF_TAB(N)%IRECTM(4*(L-1)+1)
              N2 = INTBUF_TAB(N)%IRECTM(4*(L-1)+2)
              N3 = INTBUF_TAB(N)%IRECTM(4*(L-1)+3)
              N4 = INTBUF_TAB(N)%IRECTM(4*(L-1)+4)
    
              IF(NATIV_SMS(I)==0.AND.NATIV_SMS(N1)==0
     .                          .AND.NATIV_SMS(N2)==0
     .                          .AND.NATIV_SMS(N3)==0
     .                          .AND.NATIV_SMS(N4)==0) CYCLE

              IF(I > 0)THEN

                LT_SMS(NAD_SMS(I)) = -DMINT2(1,KSN)
                LT_SMS(NAD_SMS(N1))= -DMINT2(1,KSN)
                NAD_SMS(I) =NAD_SMS(I)+1
                NAD_SMS(N1)=NAD_SMS(N1)+1


                LT_SMS(NAD_SMS(I)) = -DMINT2(2,KSN)
                LT_SMS(NAD_SMS(N2))= -DMINT2(2,KSN)
                NAD_SMS(I) =NAD_SMS(I)+1
                NAD_SMS(N2)=NAD_SMS(N2)+1

                LT_SMS(NAD_SMS(I)) = -DMINT2(3,KSN)
                LT_SMS(NAD_SMS(N3))= -DMINT2(3,KSN)
                NAD_SMS(I) =NAD_SMS(I)+1
                NAD_SMS(N3)=NAD_SMS(N3)+1

                LT_SMS(NAD_SMS(I)) = -DMINT2(4,KSN)
                LT_SMS(NAD_SMS(N4))= -DMINT2(4,KSN)
                NAD_SMS(I) =NAD_SMS(I)+1
                NAD_SMS(N4)=NAD_SMS(N4)+1

              ELSE

                I=-I
                LTIJ = ZERO

                LT_SMS(NAD_SMS(I)) = LTIJ
                LT_SMS(NAD_SMS(N1))= LTIJ
                NAD_SMS(I) =NAD_SMS(I)+1
                NAD_SMS(N1)=NAD_SMS(N1)+1


                LT_SMS(NAD_SMS(I)) = LTIJ
                LT_SMS(NAD_SMS(N2))= LTIJ
                NAD_SMS(I) =NAD_SMS(I)+1
                NAD_SMS(N2)=NAD_SMS(N2)+1

                LT_SMS(NAD_SMS(I)) = LTIJ
                LT_SMS(NAD_SMS(N3))= LTIJ
                NAD_SMS(I) =NAD_SMS(I)+1
                NAD_SMS(N3)=NAD_SMS(N3)+1

                LT_SMS(NAD_SMS(I)) = LTIJ
                LT_SMS(NAD_SMS(N4))= LTIJ
                NAD_SMS(I) =NAD_SMS(I)+1
                NAD_SMS(N4)=NAD_SMS(N4)+1
              END IF
            END DO
C
          ELSEIF(NTY==2.AND.ILAGM==0.AND.(ILEV==27.or.ILEV==28))THEN
C
            KAD=IPARI(1,N)
            NSN=IPARI(5,N)
            DO II=1,NSN
              I=INTBUF_TAB(N)%NSV(II)
              KSN=KSN+1
C
              IF (INTBUF_TAB(N)%IRUPT(II)==0) THEN
C
              L=INTBUF_TAB(N)%IRTLM(II)
              N1 = INTBUF_TAB(N)%IRECTM(4*(L-1)+1)
              N2 = INTBUF_TAB(N)%IRECTM(4*(L-1)+2)
              N3 = INTBUF_TAB(N)%IRECTM(4*(L-1)+3)
              N4 = INTBUF_TAB(N)%IRECTM(4*(L-1)+4)
              FAC_SCAL_I = T2FAC_SMS(I)
    
              IF(NATIV_SMS(I)==0.AND.NATIV_SMS(N1)==0
     .                          .AND.NATIV_SMS(N2)==0
     .                          .AND.NATIV_SMS(N3)==0
     .                          .AND.NATIV_SMS(N4)==0) CYCLE

              IF(I > 0)THEN
                DO KJ=JAD_SMS(I),LAD_SMS(I)
                  J =JDI_SMS(KJ)
                  LTIJ = LT_SMS(KJ)
                  LT_SMS(KJ)=ZERO
                  FAC_SCAL_J = T2FAC_SMS(J)

                  IF (T2MAIN_SMS(1,J) == 1) THEN
C-- No Type2 + AMS on J
                    LTIJ = LTIJ*FAC_SCAL_I
C
                    LT_SMS(NAD_SMS(J)) = LTIJ
                    LT_SMS(NAD_SMS(N1))= LTIJ
                    NAD_SMS(J) =NAD_SMS(J)+1
                    NAD_SMS(N1)=NAD_SMS(N1)+1

                    LT_SMS(NAD_SMS(J)) = LTIJ
                    LT_SMS(NAD_SMS(N2))= LTIJ
                    NAD_SMS(J) =NAD_SMS(J)+1
                    NAD_SMS(N2)=NAD_SMS(N2)+1

                    LT_SMS(NAD_SMS(J)) = LTIJ
                    LT_SMS(NAD_SMS(N3))= LTIJ
                    NAD_SMS(J) =NAD_SMS(J)+1
                    NAD_SMS(N3)=NAD_SMS(N3)+1

                    LT_SMS(NAD_SMS(J)) = LTIJ
                    LT_SMS(NAD_SMS(N4))= LTIJ
                    NAD_SMS(J) =NAD_SMS(J)+1
                    NAD_SMS(N4)=NAD_SMS(N4)+1
C
                  ELSEIF(T2MAIN_SMS(6,J)==0) THEN
C-- Type2 crossed connection between main nodes - failure of main element of j
C
                    LTIJ = LTIJ*MAX(FAC_SCAL_I,FAC_SCAL_J)
C
                    LT_SMS(NAD_SMS(J)) = ZERO
                    LT_SMS(NAD_SMS(N1))= ZERO
                    NAD_SMS(J) =NAD_SMS(J)+1
                    NAD_SMS(N1)=NAD_SMS(N1)+1

                    LT_SMS(NAD_SMS(J)) = ZERO
                    LT_SMS(NAD_SMS(N2))= ZERO
                    NAD_SMS(J) =NAD_SMS(J)+1
                    NAD_SMS(N2)=NAD_SMS(N2)+1

                    LT_SMS(NAD_SMS(J)) = ZERO
                    LT_SMS(NAD_SMS(N3))= ZERO
                    NAD_SMS(J) =NAD_SMS(J)+1
                    NAD_SMS(N3)=NAD_SMS(N3)+1

                    LT_SMS(NAD_SMS(J)) = ZERO
                    LT_SMS(NAD_SMS(N4))= ZERO
                    NAD_SMS(J) =NAD_SMS(J)+1
                    NAD_SMS(N4)=NAD_SMS(N4)+1

                    IF (I>J) THEN
                    DO K =2,5
                      DO KK =2,5
                        IF (T2MAIN_SMS(K,I)/=T2MAIN_SMS(KK,J)) THEN
                          LT_SMS(NAD_SMS(T2MAIN_SMS(K,I))) = HALF*LTIJ
                          LT_SMS(NAD_SMS(T2MAIN_SMS(KK,J)))= HALF*LTIJ
                          NAD_SMS(T2MAIN_SMS(K,I)) =NAD_SMS(T2MAIN_SMS(K,I))+1
                          NAD_SMS(T2MAIN_SMS(KK,J))=NAD_SMS(T2MAIN_SMS(KK,J))+1
                        ENDIF
                      ENDDO
                    ENDDO
                    ENDIF
C
                  ELSE
C-- Type2 crossed connection between main nodes - failure of main element of j
C
                    LT_SMS(NAD_SMS(J)) = LTIJ
                    LT_SMS(NAD_SMS(N1))= LTIJ
                    NAD_SMS(J) =NAD_SMS(J)+1
                    NAD_SMS(N1)=NAD_SMS(N1)+1

                    LT_SMS(NAD_SMS(J)) = LTIJ
                    LT_SMS(NAD_SMS(N2))= LTIJ
                    NAD_SMS(J) =NAD_SMS(J)+1
                    NAD_SMS(N2)=NAD_SMS(N2)+1

                    LT_SMS(NAD_SMS(J)) = LTIJ
                    LT_SMS(NAD_SMS(N3))= LTIJ
                    NAD_SMS(J) =NAD_SMS(J)+1
                    NAD_SMS(N3)=NAD_SMS(N3)+1

                    LT_SMS(NAD_SMS(J)) = LTIJ
                    LT_SMS(NAD_SMS(N4))= LTIJ
                    NAD_SMS(J) =NAD_SMS(J)+1
                    NAD_SMS(N4)=NAD_SMS(N4)+1

                    IF (I>J) THEN
                    DO K =2,5
                      DO KK =2,5
                        IF (T2MAIN_SMS(K,I)/=T2MAIN_SMS(KK,J)) THEN
                          LT_SMS(NAD_SMS(T2MAIN_SMS(K,I))) = ZERO
                          LT_SMS(NAD_SMS(T2MAIN_SMS(KK,J)))= ZERO
                          NAD_SMS(T2MAIN_SMS(K,I)) =NAD_SMS(T2MAIN_SMS(K,I))+1
                          NAD_SMS(T2MAIN_SMS(KK,J))=NAD_SMS(T2MAIN_SMS(KK,J))+1
                        ENDIF
                      ENDDO
                    ENDDO
                    ENDIF
C
                  ENDIF
C
                END DO
              ELSE
                I=-I
                DO KJ=JAD_SMS(I),LAD_SMS(I)
                  J =JDI_SMS(KJ)
                  LTIJ = ZERO

                  IF (T2MAIN_SMS(1,J) == 1) THEN
C-- No Type2 + AMS on J
                    LT_SMS(NAD_SMS(J)) = LTIJ
                    LT_SMS(NAD_SMS(N1))= LTIJ
                    NAD_SMS(J) =NAD_SMS(J)+1
                    NAD_SMS(N1)=NAD_SMS(N1)+1

                    LT_SMS(NAD_SMS(J)) = LTIJ
                    LT_SMS(NAD_SMS(N2))= LTIJ
                    NAD_SMS(J) =NAD_SMS(J)+1
                    NAD_SMS(N2)=NAD_SMS(N2)+1

                    LT_SMS(NAD_SMS(J)) = LTIJ
                    LT_SMS(NAD_SMS(N3))= LTIJ
                    NAD_SMS(J) =NAD_SMS(J)+1
                    NAD_SMS(N3)=NAD_SMS(N3)+1

                    LT_SMS(NAD_SMS(J)) = LTIJ
                    LT_SMS(NAD_SMS(N4))= LTIJ
                    NAD_SMS(J) =NAD_SMS(J)+1
                    NAD_SMS(N4)=NAD_SMS(N4)+1
C
                  ELSE
C-- Type2 crossed connection between main nodes
C
                    LT_SMS(NAD_SMS(J)) = ZERO
                    LT_SMS(NAD_SMS(N1))= ZERO
                    NAD_SMS(J) =NAD_SMS(J)+1
                    NAD_SMS(N1)=NAD_SMS(N1)+1

                    LT_SMS(NAD_SMS(J)) = ZERO
                    LT_SMS(NAD_SMS(N2))= ZERO
                    NAD_SMS(J) =NAD_SMS(J)+1
                    NAD_SMS(N2)=NAD_SMS(N2)+1

                    LT_SMS(NAD_SMS(J)) = ZERO
                    LT_SMS(NAD_SMS(N3))= ZERO
                    NAD_SMS(J) =NAD_SMS(J)+1
                    NAD_SMS(N3)=NAD_SMS(N3)+1

                    LT_SMS(NAD_SMS(J)) = ZERO
                    LT_SMS(NAD_SMS(N4))= ZERO
                    NAD_SMS(J) =NAD_SMS(J)+1
                    NAD_SMS(N4)=NAD_SMS(N4)+1

                    IF (I>J) THEN
                    DO K =2,5
                      DO KK =2,5
                        IF (T2MAIN_SMS(K,I)/=T2MAIN_SMS(KK,J)) THEN
                          LT_SMS(NAD_SMS(T2MAIN_SMS(K,I))) = HALF*LTIJ
                          LT_SMS(NAD_SMS(T2MAIN_SMS(KK,J)))= HALF*LTIJ
                          NAD_SMS(T2MAIN_SMS(K,I)) =NAD_SMS(T2MAIN_SMS(K,I))+1
                          NAD_SMS(T2MAIN_SMS(KK,J))=NAD_SMS(T2MAIN_SMS(KK,J))+1
                        ENDIF
                      ENDDO
                    ENDDO
                    ENDIF
C
                  ENDIF
C
                END DO
              END IF
C
              ELSE
C
C              KSN=KSN+1

              IF(WEIGHT(ABS(I))/=1)CYCLE

              L=INTBUF_TAB(N)%IRTLM(II)
              N1 = INTBUF_TAB(N)%IRECTM(4*(L-1)+1)
              N2 = INTBUF_TAB(N)%IRECTM(4*(L-1)+2)
              N3 = INTBUF_TAB(N)%IRECTM(4*(L-1)+3)
              N4 = INTBUF_TAB(N)%IRECTM(4*(L-1)+4)
    
              IF(NATIV_SMS(I)==0.AND.NATIV_SMS(N1)==0
     .                          .AND.NATIV_SMS(N2)==0
     .                          .AND.NATIV_SMS(N3)==0
     .                          .AND.NATIV_SMS(N4)==0) CYCLE

              IF(I > 0)THEN

                LT_SMS(NAD_SMS(I)) = -DMINT2(1,KSN)
                LT_SMS(NAD_SMS(N1))= -DMINT2(1,KSN)
                NAD_SMS(I) =NAD_SMS(I)+1
                NAD_SMS(N1)=NAD_SMS(N1)+1


                LT_SMS(NAD_SMS(I)) = -DMINT2(2,KSN)
                LT_SMS(NAD_SMS(N2))= -DMINT2(2,KSN)
                NAD_SMS(I) =NAD_SMS(I)+1
                NAD_SMS(N2)=NAD_SMS(N2)+1

                LT_SMS(NAD_SMS(I)) = -DMINT2(3,KSN)
                LT_SMS(NAD_SMS(N3))= -DMINT2(3,KSN)
                NAD_SMS(I) =NAD_SMS(I)+1
                NAD_SMS(N3)=NAD_SMS(N3)+1

                LT_SMS(NAD_SMS(I)) = -DMINT2(4,KSN)
                LT_SMS(NAD_SMS(N4))= -DMINT2(4,KSN)
                NAD_SMS(I) =NAD_SMS(I)+1
                NAD_SMS(N4)=NAD_SMS(N4)+1

              ELSE

                I=-I
                LTIJ = ZERO

                LT_SMS(NAD_SMS(I)) = LTIJ
                LT_SMS(NAD_SMS(N1))= LTIJ
                NAD_SMS(I) =NAD_SMS(I)+1
                NAD_SMS(N1)=NAD_SMS(N1)+1


                LT_SMS(NAD_SMS(I)) = LTIJ
                LT_SMS(NAD_SMS(N2))= LTIJ
                NAD_SMS(I) =NAD_SMS(I)+1
                NAD_SMS(N2)=NAD_SMS(N2)+1

                LT_SMS(NAD_SMS(I)) = LTIJ
                LT_SMS(NAD_SMS(N3))= LTIJ
                NAD_SMS(I) =NAD_SMS(I)+1
                NAD_SMS(N3)=NAD_SMS(N3)+1

                LT_SMS(NAD_SMS(I)) = LTIJ
                LT_SMS(NAD_SMS(N4))= LTIJ
                NAD_SMS(I) =NAD_SMS(I)+1
                NAD_SMS(N4)=NAD_SMS(N4)+1
              END IF              
C
              ENDIF
C
            END DO
C
          END IF
        END DO
C
      END IF
C
      CALL MY_BARRIER()
C 
C------------
C rbodies
C------------
      IF(NRBODY/=0)THEN
C
!$OMP DO SCHEDULE(DYNAMIC,1)
       DO M = 1, NRBODY
C
        IAD=0
        DO K=1,M-1
          NSN = NPBY(2,K)
          IAD = IAD  + NSN
        END DO
C
        MSR=NPBY(1,M)
        IF(MSR < 0) CYCLE
C
        IF(TAGMSR_RBY_SMS(MSR) /= 0) THEN
C
C ce noeud ne sera jamais supprime des rwalls ... probleme ou pas ?
         NODXI_SMS(MSR)=1
C
         NSN=NPBY(2,M)
         DO KI=1,NSN
           I=LPBY(IAD+KI)
           IF(JAD_SMS(I+1) > JAD_SMS(I)) NODXI_SMS(I)=1
           DO KJ=JAD_SMS(I),JAD_SMS(I+1)-1
             J = JDI_SMS(KJ)
             IF(J > 0)THEN
               IF(ITF(KINET(J))/=0) THEN
                 LT_SMS(KJ)=ZERO
                 CYCLE
               END IF
               N = TAGSLV_RBY_SMS(J)
               IF(N==M)THEN
                 LT_SMS(KJ)=ZERO
               END IF
             END IF
           END DO
         END DO
C
        END IF
       END DO
!$OMP  END DO
      END IF
C------------
C symmetrization
C------------
      DO I=NODFT,NODLT
        DO IJ=JAD_SMS(I),JAD_SMS(I+1)-1
          J=JDI_SMS(IJ)
          IF(J > I)THEN
            JI=JSM_SMS(IJ)
            IF(LT_SMS(IJ)==ZERO.OR.LT_SMS(JI)==ZERO)THEN
c IJ or JI ask for resetting connexion to 0
              LT_SMS(IJ)=ZERO
              LT_SMS(JI)=ZERO
            ELSE
              LTIJ=MIN(LT_SMS(IJ),LT_SMS(JI))
              LT_SMS(IJ)=LTIJ
              LT_SMS(JI)=LTIJ
            END IF
          END IF
        END DO
      END DO
C----- 
C     Interfaces
C----- 
 100  CONTINUE
C 
      CALL MY_BARRIER()
C 
C----- 
      LOC_PROC = ISPMD + 1
C 
      DO NN=ITASK+1,NISKY_SMS,NTHREAD
       P  =ISKYI_SMS(NN,3)
       IF(P/=LOC_PROC) CYCLE

       I  =ISKYI_SMS(NN,1)
       J  =ISKYI_SMS(NN,2)
       M = TAGSLV_RBY_SMS(I)
       N = TAGSLV_RBY_SMS(J)
       IF(M/=0.AND.N==M)THEN
         ISKYI_SMS(NN,1)=0
         ISKYI_SMS(NN,2)=0
       END IF
      END DO
C ---
C 
      CALL MY_BARRIER()
C
C non //
      IF(ITASK==0)THEN
 
        DO N=1,NUMNOD
          NADI_SMS(N)=0
        END DO
 
        DO NN=1,NISKY_SMS
          P  =ISKYI_SMS(NN,3)
          IF(P/=LOC_PROC) CYCLE

          I  =ISKYI_SMS(NN,1)
          J  =ISKYI_SMS(NN,2)
          IF(I==0.AND.J==0) CYCLE

          NADI_SMS(I)=NADI_SMS(I)+1
          NADI_SMS(J)=NADI_SMS(J)+1
        END DO
 
        JADI_SMS(1)=1
        KADI_SMS(1)=1
        DO N=2,NUMNOD+1
          JADI_SMS(N)=JADI_SMS(N-1)+NADI_SMS(N-1)
          KADI_SMS(N)=JADI_SMS(N)
        END DO
 
        DO NN=1,NISKY_SMS
          P  =ISKYI_SMS(NN,3)
          IF(P/=LOC_PROC) CYCLE

          I  =ISKYI_SMS(NN,1)
          J  =ISKYI_SMS(NN,2)
          IF(I==0.AND.J==0) CYCLE

          KK =KADI_SMS(I)
          JDII_SMS(KK)=J
          LTI_SMS(KK) =-MSKYI_SMS(NN)
          KADI_SMS(I) = KADI_SMS(I)+1
 
          KK =KADI_SMS(J)
          JDII_SMS(KK)=I
          LTI_SMS(KK) =-MSKYI_SMS(NN)
          KADI_SMS(J) = KADI_SMS(J)+1
        END DO
 
      END IF
C 
      CALL MY_BARRIER()
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
      CALL SMS_BUILD_DIAG(
     1           ITASK    ,NODFT   ,NODLT    ,MS       ,NODII_SMS ,
     2           JAD_SMS ,JDI_SMS  ,LT_SMS   ,DIAG_SMS ,INDX1_SMS   ,
     3           INDX2_SMS,IAD_ELEM,FR_ELEM  ,NPBY     ,LPBY   ,
     4           LAD_SMS  ,KAD_SMS  ,JSM_SMS ,MSKYI_SMS,ISKYI_SMS   ,
     5           JADI_SMS,JDII_SMS  ,LTI_SMS   ,NODXI_SMS ,FR_SMS   ,
     6           FR_RMS  ,LIST_SMS  ,LIST_RMS  ,MSKYI_FI_SMS,ILINK  ,
     7           RLINK   ,NNLINK   ,LNLINK    ,TAG_LNK_SMS,LJOINT,
     8           IADCJ   ,FR_CJ    ,ITAB      ,WEIGHT     ,IMV   ,
     9           MV      ,MV6      ,MW6       ,NPRW       ,LPRW  ,
     A           FR_WALL ,NRWL_SMS ,TAGMSR_RBY_SMS,RBY    ,AWORK ,
     B           X       ,A        ,AR       ,IN          ,V     ,
     C           VR      ,TAGSLV_RBY_SMS,IRBE2,LRBE2      ,IRBE3 ,
     D           LRBE3   ,IAD_RBE3M,FR_RBE3M )
C 

      IF(ITASK==0)DEALLOCATE(LIST_SMS,LIST_RMS,MSKYI_FI_SMS)
      IF(IPARIT/=0)THEN
        DEALLOCATE(IMV, MV, MV6)
        IF(ITASK==0)DEALLOCATE(MW6)
      END IF
c-----------
      RETURN
      END
