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
Chd|  SMS_INIT                      source/ams/sms_init.F         
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        NODNX_SMS_INI                 source/ams/sms_init.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SMS_INIT(
     1  IXS     ,IXQ      ,IXC     ,IXT     ,IXP       ,
     2  IXR     ,IXTG     ,IXTG1   ,IXS10   ,IXS16     ,
     3  IXS20   ,IPARG    ,NODNX_SMS ,
     4  ICODT   ,ICODR    ,KINET     ,
     5                     IPARTS    ,IPARTQ   ,IPARTC  ,
     6  IPARTT  ,IPARTP   ,IPARTR    ,IPARTUR  ,IPARTTG ,
     7  IPARTX  ,TAGPRT_SMS,ITAB    ,IRBE2   ,
     8  IRBE3   ,LRBE2     ,LRBE3     ,NPRW    ,LPRW    ,
     9  IPART   ,IGEO      ,IPM       ,NATIV_SMS,NPBY   ,
     A  LPBY    ,TAGMSR_RBY_SMS,TAGSLV_RBY_SMS,NOM_OPT  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "kincod_c.inc"
#include      "param_c.inc"
#include      "sms_c.inc"
#include      "units_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER 
     .        IXS(NIXS,*),IXS10(6,*) ,IXS16(6,*) ,IXS20(12,*),
     .        IXQ(NIXQ,*),IXC(NIXC,*), IXT(NIXT,*), IXP(NIXP,*),
     .        IXR(NIXR,*), IXTG(NIXTG,*), IXTG1(4,*),
     .        IPARG(NPARG,*), 
     .   NODNX_SMS(*), ICODT(*), ICODR(*), KINET(*),
     .   IPARTS(*),IPARTQ(*),IPARTC(*),IPARTT(*),
     .   IPARTP(*),IPARTR(*),IPARTUR(*),IPARTTG(*),IPARTX(*),
     .   TAGPRT_SMS(*),
     .   ITAB(*),
     .   IRBE2(NRBE2L,*), IRBE3(NRBE3L,*), LRBE2(*), LRBE3(*),
     .   NPRW(*), LPRW(*),
     .   IPART(LIPART1,*), IGEO(NPROPGI,*), IPM(NPROPMI,*), NATIV_SMS(*),
     .   NPBY(NNPBY,*), LPBY(*), TAGMSR_RBY_SMS(*), TAGSLV_RBY_SMS(*)
      INTEGER NOM_OPT(LNOPT1,*)
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, NG, N, JJ, KK, ITY, NEL, NFT, ISOLNOD, 
     .        IAD, IP, NAD_SMS(NUMNOD),ILOC4(4),IWORK(NUMNOD),
     .        TAG8(8), IG, IGTYP, ILW, IRIGID
      INTEGER SIZE, LENR, KSMS1, NM, NS, IMOV, NSN, ILAGM,
     .        N2, N3, N4, N5, N6
      INTEGER M, MSR, KI, NSMS(2), IWSMS, NSNW, NHI
      INTEGER J1, IPERM1(6), IPERM2(6),IPENTA6(6)
      CHARACTER*nchartitle,
     .   TITR
      DATA ILOC4/1,3,6,5/
      DATA IPERM1/1,2,3,1,2,3/
      DATA IPERM2/2,3,1,4,4,4/
      DATA IPENTA6/1,2,3,5,6,7/
C-----------------------------------------------
      IRIGID=0
      DO I=1,NUMMAT
        ILW=IPM(2,I)
        IF(ILW==13)THEN
          IRIGID=1
          EXIT
        END IF
      END DO
      IF(IRIGID/=0)THEN
        CALL ANCMSG(MSGID=1067,MSGTYPE=MSGERROR,ANMODE=ANINFO_BLIND_1)
      END IF

      DO I=1,NUMNOD
        NODNX_SMS(I)=0
      ENDDO
C
C     Construction
      CALL NODNX_SMS_INI(NUMNOD  ,NUMELT ,NIXT ,1 ,2 ,
     1IXT ,IPARTT,TAGPRT_SMS,NATIV_SMS)
      CALL NODNX_SMS_INI(NUMNOD  ,NUMELP ,NIXP ,1 ,2 ,
     1IXP ,IPARTP,TAGPRT_SMS,NATIV_SMS)
      CALL NODNX_SMS_INI(NUMNOD  ,NUMELR ,NIXR ,1 ,2 ,
     1IXR ,IPARTR,TAGPRT_SMS,NATIV_SMS)
C 
C pulleys     
      DO J=1,NUMELR
        IF(TAGPRT_SMS(IPARTR(J))==0) CYCLE
        IG = IPART(2,IPARTR(J))
        IGTYP =  IGEO(11,IG)

        IF(IGTYP==12)THEN
          K=2
          I = IXR(1+K,J) 
          NATIV_SMS(I)=NATIV_SMS(I)+1
          K=3
          I = IXR(1+K,J) 
          NATIV_SMS(I)=NATIV_SMS(I)+1
        END IF                                    
      ENDDO                                      
      CALL NODNX_SMS_INI(NUMNOD  ,NUMELTG,NIXTG,1 ,3 ,
     1IXTG,IPARTTG,TAGPRT_SMS,NATIV_SMS)
      CALL NODNX_SMS_INI(NUMNOD  ,NUMELC ,NIXC ,1 ,4 ,
     1IXC ,IPARTC,TAGPRT_SMS,NATIV_SMS)
      CALL NODNX_SMS_INI(NUMNOD  ,NUMELS ,NIXS ,1 ,8 ,
     1IXS ,IPARTS,TAGPRT_SMS,NATIV_SMS)
      CALL NODNX_SMS_INI(NUMNOD  ,NUMELS10,6 ,0 ,6 ,
     1IXS10 ,IPARTS(NUMELS8+1),TAGPRT_SMS,NATIV_SMS)
      CALL NODNX_SMS_INI(NUMNOD  ,NUMELS16,8 ,0 ,8 ,
     1IXS16 ,IPARTS(NUMELS8+NUMELS10+NUMELS20+1),TAGPRT_SMS,NATIV_SMS)
      CALL NODNX_SMS_INI(NUMNOD  ,NUMELS20,12,0 ,12,
     1IXS20 ,IPARTS(NUMELS8+NUMELS10+1),TAGPRT_SMS,NATIV_SMS)
C-----------------------------------------------
C     Warnings KINEMATIC CONDITIONS
C-----------------------------------------------
C
      KSMS1=0
      DO I=1,NUMNOD
        IF(NATIV_SMS(I)/=0)THEN
          IF(IRV(KINET(I))/=0.OR.
     .       ILMULT(KINET(I))/=0)THEN
            KSMS1=1
            NATIV_SMS(I)=0
          END IF
        END IF
      END DO
C
      IF(KSMS1/=0)THEN
          NG=0
          DO I=1,NUMNOD
            IF (NATIV_SMS(I)/=0.AND.
     .          IRV(KINET(I))/=0) THEN
              NG = NG + 1
              IWORK(NG) = ITAB(I)
            ENDIF
          ENDDO
          IF(NG/=0)THEN
            WRITE(ISTDO,'(A)') 
     .      ' ** WARNING IN ADVANCED MASS SCALING DEFINITION'
            WRITE(IOUT,'(A)')
     .      ' ** WARNING IN ADVANCED MASS SCALING DEFINITION :'
            WRITE(IOUT,'(A,/,A)')
     .' AMS WILL NOT APPLY ON NODES WHERE A RIVET APPLIES',
     .' NODE IDS='
            WRITE(IOUT,'(10I10)')(IWORK(I),I=1,NG)
          END IF

          NG=0
          DO I=1,NUMNOD
            IF (NATIV_SMS(I)/=0.AND.
     .          ILMULT(KINET(I))/=0) THEN
              NG = NG + 1
              IWORK(NG) = ITAB(I)
            ENDIF
          ENDDO
          IF(NG/=0)THEN
            WRITE(ISTDO,'(A)') 
     .      ' ** WARNING IN ADVANCED MASS SCALING DEFINITION'
            WRITE(IOUT,'(A)')
     .      ' ** WARNING IN ADVANCED MASS SCALING DEFINITION :'
              WRITE(IOUT,'(A,/,A)')
     .' AMS WILL NOT APPLY ON NODES WHERE A LAGRANGE OPTION APPLIES',
     .' NODE IDS='
            WRITE(IOUT,'(10I10)')(IWORK(I),I=1,NG)
          END IF

      END IF
C-----
      KSMS1=0
      IF(NRWALL/=0)THEN
        K = 1
        DO N=1,NRWALL
          N2=N +NRWALL
          N3=N2+NRWALL
          N4=N3+NRWALL
          N5=N4+NRWALL
          N6=N5+NRWALL
          NSN  =NPRW(N)
          IMOV =NPRW(N3)
          ITY  =NPRW(N4)
          ILAGM=NPRW(N6)
          IF(ILAGM/=0)THEN
            DO J=1,NSN
              I=LPRW(K+J-1)
              IF(NATIV_SMS(I)/=0)THEN
                NATIV_SMS(I)=0
                KSMS1=1
              END IF
            END DO
          END IF
          K  =K+NSN
        END DO
      END IF
      IF(KSMS1/=0)THEN
        WRITE(ISTDO,'(A)') 
     .  ' ** WARNING IN ADVANCED MASS SCALING DEFINITION'
        WRITE(IOUT,'(A)')
     .  ' ** WARNING IN ADVANCED MASS SCALING DEFINITION :'
        WRITE(IOUT,'(A)')
     .' AMS IS NOT COMPATIBLE WITH LAGRANGE MULTIPLIERS.'
      END IF

C-----------------------------------------------
C rbodies : numbering
C------------
      TAGSLV_RBY_SMS(1:NUMNOD)=0
      TAGMSR_RBY_SMS(1:NUMNOD) =0
C
      IAD=0
      IWSMS=0
      DO M=1,NRBODY
C
        MSR=NPBY(1,M)
        NSN=NPBY(2,M)
        IF(MSR >= 0) THEN
C if msr secnd of moving or lagrange wall => no ams
          IWSMS=0
          K = 1
          DO N=1,NRWALL
            N2=N +NRWALL
            N3=N2+NRWALL
            N4=N3+NRWALL
            N5=N4+NRWALL
            N6=N5+NRWALL
            NSNW =NPRW(N)
            IMOV =NPRW(N3)
            ITY  =NPRW(N4)
            ILAGM=NPRW(N6)
            IF(ILAGM/=0)THEN
              DO J=1,NSNW
                I=LPRW(K+J-1)
                IF(I==MSR)THEN
                  IWSMS=1
                  GOTO 100
                END IF
              END DO
            END IF
            K  =K+NSN
          END DO
 100      CONTINUE
          IF(IWSMS==0.AND.NPBY(7,M).GT.0 .AND. 
     .       (KINET(MSR) <=1 
     .        .OR. IVF(KINET(MSR)) ==1
     .        .OR. IRLK(KINET(MSR))==1 
     .        .OR. IJO(KINET(MSR)) ==1 
     .        .OR. IWL(KINET(MSR)) ==1 )) THEN
C
            TAGMSR_RBY_SMS(MSR)=M
            DO KI=1,NSN
              I=LPBY(IAD+KI)
              TAGSLV_RBY_SMS(I)=M
            END DO
C
          END IF
        END IF
        IAD  = IAD  + NSN
      END DO

      IF(IWSMS/=0)THEN
        WRITE(ISTDO,'(A)') 
     .  ' ** WARNING IN ADVANCED MASS SCALING DEFINITION'
        WRITE(IOUT,'(A)')
     .  ' ** WARNING IN ADVANCED MASS SCALING DEFINITION :'
        WRITE(IOUT,'(A)')
     .  ' AMS IS NOT COMPATIBLE WITH LAGRANGE MULTIPLIERS.'
      END IF

C-----
C  RBODY is it fully SMS (yes <=> its time step will be /dt/ams one)
C-----
C
      IAD=0
      DO M=1,NRBODY
C
        MSR=NPBY(1,M)
        NSN=NPBY(2,M)
        NSMS(1)=0
        NSMS(2)=NSN
C
        IF(MSR >= 0) THEN
          IF(TAGMSR_RBY_SMS(MSR) /= 0) THEN
            DO KI=1,NSN
              I=LPBY(IAD+KI)
              IF(NATIV_SMS(I)/=0)NSMS(1)=NSMS(1)+1
            END DO
          END IF
C
          IF(NSMS(1)==NSMS(2))THEN
            NATIV_SMS(MSR)=1
          ELSEIF(NSMS(1)/=0)THEN
            CALL FRETITL2(TITR,
     .                    NOM_OPT(LNOPT1-LTITR+1,M),LTITR)
            IF(NPBY(10,M)==0)THEN
              CALL ANCMSG(MSGID=1190,MSGTYPE=MSGWARNING,ANMODE=ANINFO_BLIND_1,
     .                    I1=NPBY(6,M),C1=TITR)
            END IF
          END IF
        END IF
C
        IF(NPBY(10,M)/=0.AND.NSMS(1)/=0)THEN
          IF(MSR > 0) NATIV_SMS(MSR)=1
          DO KI=1,NSN
            I=LPBY(IAD+KI)
            NATIV_SMS(I)=1
          END DO
        END IF
C
        IAD  = IAD  + NSN
      END DO

C-----
C-----
      DO NHI=0,NHRBE2
       DO N=1,NRBE2
        IF (IRBE2(9,N).NE.NHI) CYCLE
	IAD = IRBE2(1,N)
        NSN = IRBE2(5,N)
        M   = IRBE2(3,N)

        NSMS(1)=0
        NSMS(2)=NSN
        DO I=1,NSN
         NS = LRBE2(IAD+I)
         IF(NATIV_SMS(NS)/=0) NSMS(1)=NSMS(1)+1
        ENDDO
C
        IF(NSMS(1)/=0)THEN
          NATIV_SMS(M)=1
          DO I=1,NSN
            NS = LRBE2(IAD+I)
            NATIV_SMS(NS)=1
          ENDDO
        END IF
C
       END DO
      END DO

C-----------------------------------------------
      RETURN
      END

Chd|====================================================================
Chd|  SMS_INI_KAD                   source/ams/sms_init.F         
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SMS_INI_KAD(
     1  IXS     ,IXQ      ,IXC     ,IXT     ,IXP       ,
     2  IXR     ,IXTG     ,IXTG1   ,IXS10   ,IXS16     ,
     3  IXS20   ,IPARG    ,MS      ,MS0     ,NODNX_SMS ,
     4  ICODT   ,ICODR    ,KINET   ,
     5  KAD_SMS                    ,IPARTS   ,IPARTQ   ,
     6  IPARTC  ,IPARTT  ,IPARTP   ,IPARTR    ,IPARTUR  ,
     7  IPARTTG ,IPARTX  ,TAGPRT_SMS,TAGREL_SMS,ITAB    ,
     8  IRBE2   ,IRBE3     ,LRBE2    ,LRBE3    ,
     9  NPRW    ,LPRW      ,IPART    ,IGEO     ,NATIV_SMS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "kincod_c.inc"
#include      "param_c.inc"
#include      "sms_c.inc"
#include      "units_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER 
     .        IXS(NIXS,*),IXS10(6,*) ,IXS16(6,*) ,IXS20(12,*),
     .        IXQ(NIXQ,*),IXC(NIXC,*), IXT(NIXT,*), IXP(NIXP,*),
     .        IXR(NIXR,*), IXTG(NIXTG,*), IXTG1(4,*),
     .        IPARG(NPARG,*), 
     .   NODNX_SMS(*), ICODT(*), ICODR(*), KINET(*), 
     .   KAD_SMS(*),
     .   IPARTS(*),IPARTQ(*),IPARTC(*),IPARTT(*),
     .   IPARTP(*),IPARTR(*),IPARTUR(*),IPARTTG(*),IPARTX(*),
     .   TAGPRT_SMS(*), TAGREL_SMS(*),
     .   ITAB(*), 
     .   IRBE2(NRBE2L,*), IRBE3(NRBE3L,*), LRBE2(*), LRBE3(*),
     .   NPRW(*), LPRW(*),
     .   IPART(LIPART1,*), IGEO(NPROPGI,*), NATIV_SMS(*)
C     REAL
      my_real
     .   MS(*), MS0(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, NG, N, JJ, KK, ITY, NEL, NFT, ISOLNOD, 
     .        IAD, IP, NAD_SMS(NUMNOD),ILOC4(4),IWORK(NUMNOD),
     .        TAG8(8), IG, IGTYP
      INTEGER J1, IPERM1(6), IPERM2(6),IPENTA6(6)
      DATA ILOC4/1,3,6,5/
      DATA IPERM1/1,2,3,1,2,3/
      DATA IPERM2/2,3,1,4,4,4/
      DATA IPENTA6/1,2,3,5,6,7/
C
C-----------------------------------------------
      TAGREL_SMS(1:NGROUP)=0
C
      DO I=1,NUMNOD
        NAD_SMS(I)=0
      END DO

      KNZ_SMS = 0

      DO NG=1,NGROUP
        ITY   =IPARG(5,NG)

        NEL     = IPARG(2,NG)
        NFT     = IPARG(3,NG)
        ISOLNOD = IPARG(28,NG)
        IF(ITY==1.AND.ISOLNOD==4)THEN
         DO J=NFT+1,NFT+NEL
           DO K=1,4

             I=IXS(1+ILOC4(K),J)

             DO KK=1,4
               JJ = IXS(1+ILOC4(KK),J)
               IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
                 TAGREL_SMS(NG)=1
                 NAD_SMS(I)=NAD_SMS(I)+1
                 KNZ_SMS   =KNZ_SMS+1
               END IF
             END DO

           END DO
         END DO
        ELSEIF(ITY==1.AND.ISOLNOD==6)THEN
         DO J=NFT+1,NFT+NEL
           DO K=1,6

             I=IXS(1+IPENTA6(K),J)
             DO KK=1,6
               JJ = IXS(1+IPENTA6(KK),J)
               IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
                 TAGREL_SMS(NG)=1
                 NAD_SMS(I)=NAD_SMS(I)+1
                 KNZ_SMS   =KNZ_SMS+1
               END IF
             END DO

           END DO
         END DO
        ELSEIF(ITY==1.AND.ISOLNOD==8)THEN
         DO J=NFT+1,NFT+NEL

           DO K=1,8
             I=IXS(1+K,J)
             IWORK(I)=0
             TAG8(K)=0
           END DO

           DO K=1,8
             I=IXS(1+K,J)
             IF(IWORK(I)/=0)THEN
               TAG8(K)=1
             ELSE
               IWORK(I)=1
             END IF
           END DO

           DO K=1,8

             I=IXS(1+K,J)
             IF(TAG8(K)/=0)CYCLE

             DO KK=1,8
               JJ = IXS(1+KK,J)
               IF(TAG8(KK)/=0) CYCLE

               IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
                 TAGREL_SMS(NG)=1
                 NAD_SMS(I)=NAD_SMS(I)+1
                 KNZ_SMS   =KNZ_SMS+1
               END IF
             END DO

           END DO
         END DO
        ELSEIF(ITY==1.AND.ISOLNOD==10)THEN
          DO J=NFT+1,NFT+NEL
           J1=J-NUMELS8

           DO K=1,4

             I=IXS(1+ILOC4(K),J)
             DO KK=1,4
               JJ = IXS(1+ILOC4(KK),J)
               IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
                 TAGREL_SMS(NG)=1
                 NAD_SMS(I)=NAD_SMS(I)+1
                 KNZ_SMS   =KNZ_SMS+1
               END IF
             END DO

             DO KK=1,6
               JJ=IXS10(KK,J1)
               IF(JJ==0) CYCLE

               IF(.NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
                 TAGREL_SMS(NG)=1
                 NAD_SMS(I)=NAD_SMS(I)+1
                 KNZ_SMS   =KNZ_SMS+1
               END IF
             END DO

           END DO

           DO K=1,6

             I=IXS10(K,J1)
             IF(I==0) CYCLE

             DO KK=1,4
               JJ = IXS(1+ILOC4(KK),J)
               IF(.NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
                 TAGREL_SMS(NG)=1
                 NAD_SMS(I)=NAD_SMS(I)+1
                 KNZ_SMS   =KNZ_SMS+1
               END IF
             END DO

             DO KK=1,6
               JJ=IXS10(KK,J1)
               IF(JJ==0) CYCLE

               IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
                 TAGREL_SMS(NG)=1
                 NAD_SMS(I)=NAD_SMS(I)+1
                 KNZ_SMS   =KNZ_SMS+1
               END IF
             END DO

           END DO

          END DO
        ELSEIF(ITY==3)THEN
         DO J=NFT+1,NFT+NEL
           DO K=1,4

             I=IXC(1+K,J)
             DO KK=1,4
               JJ = IXC(1+KK,J)
               IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
                 TAGREL_SMS(NG)=1
                 NAD_SMS(I)=NAD_SMS(I)+1
                 KNZ_SMS   =KNZ_SMS+1
               END IF
             END DO

           END DO
         END DO
        ELSEIF(ITY==4)THEN
         DO J=NFT+1,NFT+NEL
           DO K=1,2

             I=IXT(1+K,J)
             DO KK=1,2
               JJ = IXT(1+KK,J)
               IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
                 TAGREL_SMS(NG)=1
                 NAD_SMS(I)=NAD_SMS(I)+1
                 KNZ_SMS   =KNZ_SMS+1
               END IF
             END DO

           END DO
         END DO
        ELSEIF(ITY==5)THEN
         DO J=NFT+1,NFT+NEL
           DO K=1,2
             I=IXP(1+K,J)
             DO KK=1,2
               JJ = IXP(1+KK,J)
               IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
                 TAGREL_SMS(NG)=1
                 NAD_SMS(I)=NAD_SMS(I)+1
                 KNZ_SMS   =KNZ_SMS+1
               END IF
             END DO
           END DO
         END DO
        ELSEIF(ITY==6)THEN
         IG = IPART(2,IPARTR(NFT+1))
         IGTYP =  IGEO(11,IG)
         IF(IGTYP/=12)THEN
           DO J=NFT+1,NFT+NEL
             DO K=1,2
               I=IXR(1+K,J)
               DO KK=1,2
         	 JJ = IXR(1+KK,J)
                 IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
                   TAGREL_SMS(NG)=1
         	   NAD_SMS(I)=NAD_SMS(I)+1
         	   KNZ_SMS   =KNZ_SMS+1
         	 END IF
               END DO
             END DO
           END DO
          ELSE
           DO J=NFT+1,NFT+NEL
             K=1

               I=IXR(1+K,J)

               KK=2
         	 JJ = IXR(1+KK,J)
                 IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
                   TAGREL_SMS(NG)=1
         	   NAD_SMS(I)=NAD_SMS(I)+1
         	   KNZ_SMS   =KNZ_SMS+1
         	 END IF

             K=2

               I=IXR(1+K,J)

               KK=1
         	 JJ = IXR(1+KK,J)
                 IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
                   TAGREL_SMS(NG)=1
         	   NAD_SMS(I)=NAD_SMS(I)+1
         	   KNZ_SMS   =KNZ_SMS+1
         	 END IF

               KK=3
         	 JJ = IXR(1+KK,J)
                 IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
                   TAGREL_SMS(NG)=1
         	   NAD_SMS(I)=NAD_SMS(I)+1
         	   KNZ_SMS   =KNZ_SMS+1
         	 END IF

             K=3

               I=IXR(1+K,J)

               KK=2
         	 JJ = IXR(1+KK,J)
                 IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
                   TAGREL_SMS(NG)=1
         	   NAD_SMS(I)=NAD_SMS(I)+1
         	   KNZ_SMS   =KNZ_SMS+1
         	 END IF

           END DO
          END IF
        ELSEIF(ITY==7)THEN
         DO J=NFT+1,NFT+NEL
           DO K=1,3

             I=IXTG(1+K,J)
             DO KK=1,3
               JJ = IXTG(1+KK,J)
               IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
                 TAGREL_SMS(NG)=1
                 NAD_SMS(I)=NAD_SMS(I)+1
                 KNZ_SMS   =KNZ_SMS+1
               END IF
             END DO

           END DO
         END DO
        END IF
      END DO
C
      KAD_SMS(1)=1
      DO I=1,NUMNOD
        KAD_SMS(I+1)=KAD_SMS(I)+NAD_SMS(I)
      END DO
C-----------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  NODNX_SMS_INI                 source/ams/sms_init.F         
Chd|-- called by -----------
Chd|        SMS_INIT                      source/ams/sms_init.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE NODNX_SMS_INI(
     1 NUMNOD  ,NUMEL ,NIX ,MIX ,LIX ,
     2 IX      ,IPARTX,TAGPRT_SMS,NODNX_SMS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NUMNOD   , NUMEL ,NIX  ,MIX, LIX,
     .         IX(NIX,*), IPARTX(*), TAGPRT_SMS(*), NODNX_SMS(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, TAG(NUMNOD)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C      
      DO J=1,NUMEL
        IF(TAGPRT_SMS(IPARTX(J))==0) CYCLE

        DO K=1,LIX                               
          I = IX(MIX+K,J) 
          IF(I/=0) TAG(I)=0
        ENDDO                                    
        DO K=1,LIX                               
          I = IX(MIX+K,J) 
          IF(I/=0)THEN
            IF(TAG(I)==0)THEN
              NODNX_SMS(I)=NODNX_SMS(I)+1
              TAG(I)=1
            END IF		    
          END IF                  
        ENDDO                                    
      ENDDO                                      

      RETURN
      END
Chd|====================================================================
Chd|  SMS_INI_KDI                   source/ams/sms_init.F         
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SMS_INI_KDI(
     2             IXC      ,IPARG   ,IXS      ,IXT      ,IXP     ,
     3             IXR      ,IXTG    ,IXS10    ,NODNX_SMS,KAD_SMS ,
     4             KDI_SMS  ,JADC_SMS,JADS_SMS ,JADS10_SMS,
     5             JADT_SMS ,JADP_SMS,
     6             JADR_SMS,JADTG_SMS,TAGPRT_SMS,IAD_SMS ,
     7             TAGREL_SMS,IPARTS ,IPARTQ   ,IPARTC   ,IPARTT   ,
     8             IPARTP    ,IPARTR ,IPARTUR  ,IPARTTG  ,IPARTX   ,
     9             NPBY      ,LPBY   ,KINET    ,TAGSLV_RBY_SMS,IPARI,
     A             INTBUF_TAB,LAD_SMS,IPART    ,IGEO     ,NATIV_SMS )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
#include "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "kincod_c.inc"
#include      "param_c.inc"
#include      "sms_c.inc"
#include      "units_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER 
     .        IPARG(NPARG,*), IXC(NIXC,*), IXS(NIXS,*), IXT(NIXT,*), 
     .        IXP(NIXP,*), IXR(NIXR,*), IXTG(NIXTG,*), IXS10(6,*),
     .        NODNX_SMS(*), KAD_SMS(*), IAD_SMS(*),
     .        JADC_SMS(4,*),
     .        JADS_SMS(8,*), JADS10_SMS(6,*), 
     .        JADT_SMS(2,*), 
     .        JADP_SMS(2,*),
     .        JADR_SMS(3,*), 
     .        JADTG_SMS(3,*), NATIV_SMS(*),
     .        TAGPRT_SMS(*), TAGREL_SMS(*), 
     .        IPARTS(*), IPARTQ(*), IPARTC(*), IPARTT(*),
     .        IPARTP(*), IPARTR(*), IPARTUR(*), IPARTTG(*), IPARTX(*),
     .        NPBY(NNPBY,*), LPBY(*), KINET(*), TAGSLV_RBY_SMS(*),
     .        IPARI(NPARI,*), 
     .        LAD_SMS(*), KDI_SMS(*), 
     .        IPART(LIPART1,*), IGEO(NPROPGI,*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, JJ, KK, II, IJ, M, N, IERROR, KL
      INTEGER NG, ITY, NEL, NFT, ISOLNOD,ILOC4(4),TAGA(NUMNOD),
     .        TAG8(8), IG, IGTYP
      INTEGER MSR, NSN, KI, KJ, NAD_SMS(NUMNOD), 
     .        NSR
      INTEGER SIZE, LENR, IAD, L, LLT
      INTEGER NTY, ILAGM,JI, N1, N2, N3, N4, LNEW, ILEV
      INTEGER J1, IPERM1(6), IPERM2(6),IPENTA6(6)
      INTEGER TAGK(NUMNOD), IK, NK
      DATA ILOC4/1,3,6,5/
      DATA IPERM1/1,2,3,1,2,3/
      DATA IPERM2/2,3,1,4,4,4/
      DATA IPENTA6/1,2,3,5,6,7/
C-----------------------------------------------
C
C     Construit JDI_SMS, JADS_SMS, etc
C     -----------------
      DO I=1,NUMNOD
        NAD_SMS(I)=KAD_SMS(I)
      END DO
C
      DO NG=1,NGROUP
C
        IF(TAGREL_SMS(NG)==0)CYCLE
        ITY   =IPARG(5,NG)

        NEL     = IPARG(2,NG)
        NFT     = IPARG(3,NG)
        ISOLNOD = IPARG(28,NG)
        IF(ITY==1.AND.ISOLNOD==4)THEN
         DO J=NFT+1,NFT+NEL

           DO K=1,4
             I=IXS(1+ILOC4(K),J)
             JADS_SMS(K,J)=NAD_SMS(I)

             IJ=JADS_SMS(K,J)
             DO KK=1,4
               JJ = IXS(1+ILOC4(KK),J)
               IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
                  NAD_SMS(I)=NAD_SMS(I)+1
                  KDI_SMS(IJ)=JJ
                  IJ=IJ+1
               END IF
             END DO
           END DO
         END DO
        ELSEIF(ITY==1.AND.ISOLNOD==6)THEN
         DO J=NFT+1,NFT+NEL

           DO K=1,6
             I=IXS(1+IPENTA6(K),J)
             JADS_SMS(K,J)=NAD_SMS(I)

             IJ=JADS_SMS(K,J)
             DO KK=1,6
               JJ = IXS(1+IPENTA6(KK),J)
               IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
                  NAD_SMS(I)=NAD_SMS(I)+1
                  KDI_SMS(IJ)=JJ
                  IJ=IJ+1
               END IF
             END DO
           END DO
         END DO
        ELSEIF(ITY==1.AND.ISOLNOD==8)THEN
         DO J=NFT+1,NFT+NEL

           DO K=1,8
             I=IXS(1+K,J)
             TAGA(I)=0
             TAG8(K)=0
           END DO

           DO K=1,8
             I=IXS(1+K,J)
             IF(TAGA(I)/=0)THEN
               TAG8(K)=1
             ELSE
               TAGA(I)=1
             END IF
           END DO

           DO K=1,8
             I=IXS(1+K,J)
             JADS_SMS(K,J)=NAD_SMS(I)
           END DO

           DO K=1,8

             I=IXS(1+K,J)
             IF(TAG8(K)/=0)CYCLE

             IJ=JADS_SMS(K,J)
             DO KK=1,8
               JJ = IXS(1+KK,J)
               IF(TAG8(KK)/=0) CYCLE

               IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
                 NAD_SMS(I)=NAD_SMS(I)+1
                 KDI_SMS(IJ)=JJ
                 IJ=IJ+1
               END IF
             END DO

           END DO

         END DO
        ELSEIF(ITY==1.AND.ISOLNOD==10)THEN
          DO J=NFT+1,NFT+NEL
           J1=J-NUMELS8

           DO K=1,4

             I=IXS(1+ILOC4(K),J)
             JADS_SMS(K,J)=NAD_SMS(I)

             IJ=JADS_SMS(K,J)
             DO KK=1,4
               JJ = IXS(1+ILOC4(KK),J)
               IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
                 NAD_SMS(I)=NAD_SMS(I)+1
                 KDI_SMS(IJ)=JJ
                 IJ=IJ+1
               END IF
             END DO

             DO KK=1,6
               JJ=IXS10(KK,J1)
               IF(JJ==0) CYCLE

               IF(.NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
                 NAD_SMS(I)=NAD_SMS(I)+1
                 KDI_SMS(IJ)=JJ
                 IJ=IJ+1
               END IF
             END DO

           END DO


           DO K=1,6

             I=IXS10(K,J1)
             IF(I==0) CYCLE

             JADS10_SMS(K,J1)=NAD_SMS(I)

             IJ=JADS10_SMS(K,J1)
             DO KK=1,4
               JJ = IXS(1+ILOC4(KK),J)
               IF(.NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
                 NAD_SMS(I)=NAD_SMS(I)+1
                 KDI_SMS(IJ)=JJ
                 IJ=IJ+1
               END IF
             END DO

             DO KK=1,6
               JJ=IXS10(KK,J1)
               IF(JJ==0) CYCLE

               IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
                 NAD_SMS(I)=NAD_SMS(I)+1
                 KDI_SMS(IJ)=JJ
                 IJ=IJ+1
               END IF
             END DO

           END DO

          END DO
        ELSEIF(ITY==3)THEN
         DO J=NFT+1,NFT+NEL

           DO K=1,4
             I=IXC(1+K,J)
             JADC_SMS(K,J)=NAD_SMS(I)

             IJ=JADC_SMS(K,J)
             DO KK=1,4
               JJ = IXC(1+KK,J)
               IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
                  NAD_SMS(I)=NAD_SMS(I)+1
                  KDI_SMS(IJ)=JJ
                  IJ=IJ+1
               END IF
             END DO
           END DO
         END DO
        ELSEIF(ITY==4)THEN
         DO J=NFT+1,NFT+NEL

           DO K=1,2
             I=IXT(1+K,J)
             JADT_SMS(K,J)=NAD_SMS(I)

             IJ=JADT_SMS(K,J)
             DO KK=1,2
               JJ = IXT(1+KK,J)
               IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
                  NAD_SMS(I)=NAD_SMS(I)+1
                  KDI_SMS(IJ)=JJ
                  IJ=IJ+1
               END IF
             END DO
           END DO
         END DO
        ELSEIF(ITY==5)THEN
         DO J=NFT+1,NFT+NEL

           DO K=1,2
             I=IXP(1+K,J)
             JADP_SMS(K,J)=NAD_SMS(I)

             IJ=JADP_SMS(K,J)
             DO KK=1,2
               JJ = IXP(1+KK,J)
               IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
                  NAD_SMS(I)=NAD_SMS(I)+1
                  KDI_SMS(IJ)=JJ
                  IJ=IJ+1
               END IF
             END DO
           END DO
         END DO
        ELSEIF(ITY==6)THEN
         IG = IPART(2,IPARTR(NFT+1))
         IGTYP =  IGEO(11,IG)
         IF(IGTYP/=12)THEN
          DO J=NFT+1,NFT+NEL

           DO K=1,2
             I=IXR(1+K,J)
             JADR_SMS(K,J)=NAD_SMS(I)

             IJ=JADR_SMS(K,J)
             DO KK=1,2
               JJ = IXR(1+KK,J)
               IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
                  NAD_SMS(I)=NAD_SMS(I)+1
                  KDI_SMS(IJ)=JJ
                  IJ=IJ+1
               END IF
             END DO
           END DO
          END DO
         ELSE
          DO J=NFT+1,NFT+NEL
             K=1
             I=IXR(1+K,J)
             JADR_SMS(K,J)=NAD_SMS(I)

             IJ=JADR_SMS(K,J)
             KK=2
               JJ = IXR(1+KK,J)
               IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
                  NAD_SMS(I)=NAD_SMS(I)+1
                  KDI_SMS(IJ)=JJ
                  IJ=IJ+1
               END IF

             K=2
             I=IXR(1+K,J)
             JADR_SMS(K,J)=NAD_SMS(I)

             IJ=JADR_SMS(K,J)
             KK=1
               JJ = IXR(1+KK,J)
               IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
                  NAD_SMS(I)=NAD_SMS(I)+1
                  KDI_SMS(IJ)=JJ
                  IJ=IJ+1
               END IF

             KK=3
               JJ = IXR(1+KK,J)
               IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
                  NAD_SMS(I)=NAD_SMS(I)+1
                  KDI_SMS(IJ)=JJ
                  IJ=IJ+1
               END IF

             K=3
             I=IXR(1+K,J)
             JADR_SMS(K,J)=NAD_SMS(I)

             IJ=JADR_SMS(K,J)
             KK=2
               JJ = IXR(1+KK,J)
               IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
                  NAD_SMS(I)=NAD_SMS(I)+1
                  KDI_SMS(IJ)=JJ
                  IJ=IJ+1
               END IF
          END DO
         END IF
        ELSEIF(ITY==7)THEN
         DO J=NFT+1,NFT+NEL

           DO K=1,3
             I=IXTG(1+K,J)
             JADTG_SMS(K,J)=NAD_SMS(I)

             IJ=JADTG_SMS(K,J)
             DO KK=1,3
               JJ = IXTG(1+KK,J)
               IF(JJ/=I.AND..NOT.(NATIV_SMS(I)==0.AND.NATIV_SMS(JJ)==0))THEN
                  NAD_SMS(I)=NAD_SMS(I)+1
                  KDI_SMS(IJ)=JJ
                  IJ=IJ+1
               END IF
             END DO
           END DO
         END DO
        END IF
      END DO
C-------------------------------------------------------------------------
C     PREPARE KOMPACTION OF ELEMENTARY MATRIX
C     NODNX_SMS(I) devient le nb de nds connectes a I
C-------------------------------------------------------------------------
      TAGK(1:NUMNOD)=0
      DO I=1,NUMNOD
        NODNX_SMS(I)=0
        DO KJ=KAD_SMS(I),KAD_SMS(I+1)-1
          IK =KDI_SMS(KJ)
          IF(TAGK(IK)==0)THEN
            NODNX_SMS(I)=NODNX_SMS(I)+1
            TAGK(IK)=1
          END IF
        END DO
        DO KJ=KAD_SMS(I),KAD_SMS(I+1)-1
          IK =KDI_SMS(KJ)
          TAGK(IK)=0
        END DO
      END DO
C
      IAD_SMS(1)=1
      DO I=1,NUMNOD
        IAD_SMS(I+1)=IAD_SMS(I)+NODNX_SMS(I)
        LAD_SMS(I)  =NODNX_SMS(I)
      END DO
C
      NNZ_SMS = IAD_SMS(NUMNOD+1)
C
      RETURN
      END
Chd|====================================================================
Chd|  SMS_INI_JAD_1                 source/ams/sms_init.F         
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SMS_INI_JAD_1(
     2             IXC      ,IPARG   ,IXS      ,IXT      ,IXP      ,
     3             IXR      ,IXTG    ,IXS10    ,NODNX_SMS,JADC_SMS ,
     4             JADS_SMS ,JADS10_SMS,JADT_SMS,JADP_SMS,JADR_SMS ,
     5             JADTG_SMS,TAGPRT_SMS,KAD_SMS,KDI_SMS  ,PK_SMS   ,
     6             TAGREL_SMS,IPARTS ,IPARTQ   ,IPARTC   ,IPARTT   ,
     7             IPARTP    ,IPARTR ,IPARTUR  ,IPARTTG  ,IPARTX   ,
     8             NPBY      ,LPBY   ,KINET    ,TAGSLV_RBY_SMS,IPARI,
     9             INTBUF_TAB,LAD_SMS,IPART    ,IGEO     ,NATIV_SMS ,
     A             IAD_SMS   ,IDI_SMS,JAD_SMS  ,JDI_SMS  ,T2MAIN_SMS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
#include "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "kincod_c.inc"
#include      "param_c.inc"
#include      "sms_c.inc"
#include      "units_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER 
     .        IPARG(NPARG,*), IXC(NIXC,*), IXS(NIXS,*), IXT(NIXT,*), 
     .        IXP(NIXP,*), IXR(NIXR,*), IXTG(NIXTG,*), IXS10(6,*),
     .        NODNX_SMS(*), KAD_SMS(*), KDI_SMS(*), PK_SMS(*),
     .        IAD_SMS(*), IDI_SMS(*), JAD_SMS(*), JDI_SMS(*),
     .        JADC_SMS(4,*),
     .        JADS_SMS(8,*), JADS10_SMS(6,*), 
     .        JADT_SMS(2,*), 
     .        JADP_SMS(2,*),
     .        JADR_SMS(3,*), 
     .        JADTG_SMS(3,*),NATIV_SMS(*),
     .        TAGPRT_SMS(*), TAGREL_SMS(*), 
     .        IPARTS(*), IPARTQ(*), IPARTC(*), IPARTT(*),
     .        IPARTP(*), IPARTR(*), IPARTUR(*), IPARTTG(*), IPARTX(*),
     .        NPBY(NNPBY,*), LPBY(*), KINET(*), TAGSLV_RBY_SMS(*),
     .        IPARI(NPARI,*), 
     .        LAD_SMS(*), 
     .        IPART(LIPART1,*), IGEO(NPROPGI,*),T2MAIN_SMS(4,*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, JJ, KK, II, IJ, M, N, IERROR, KL
      INTEGER NMN, IUN
      INTEGER MSR, NSN, KI, KJ, NAD_SMS(NUMNOD), 
     .        NSR
      INTEGER SIZE, LENR, IAD, L, LLT
      INTEGER NTY, ILAGM, K10, K11, K12, K13, K14, JI, 
     .         N1, N2, N3, N4, LNEW, ILEV
      INTEGER TAGK(NUMNOD), IK, NK, IKK,PERM,
     .        ITRI(NUMNOD),INDEX(2*NUMNOD),INDEX2(NUMNOD),WORK(70000)
      LOGICAL ITERATE
      DATA IUN/1/
C-------------------------------------------------------------------------
C     PREPARE KOMPACTION OF ELEMENTARY MATRIX
C     construit IDI_SMS et pointeurs KAD_SMS vers JAD_SMS 
C     KJ = KAD_SMS(I),KAD_SMS(I+1)-1 => PK_SMS(KJ) = rang de KDI_SMS(KJ) dans IDI_SMS(I),IDI_SMS(I+1)-1
C-------------------------------------------------------------------------
      TAGK(1:NUMNOD)=0
C
      DO I=1,NUMNOD
        NK=0
        DO KJ=KAD_SMS(I),KAD_SMS(I+1)-1
          IK =KDI_SMS(KJ)
          IF(TAGK(IK)==0)THEN
            IDI_SMS(IAD_SMS(I)+NK)=IK
            NK=NK+1
            TAGK(IK)=NK
          END IF
        END DO
C
C       reordonne IDI_SMS(KJ), KJ=IAD_SMS(I),IAD_SMS(I)+LAD_SMS(I)-1
        DO IK=1,NK
          KJ=IAD_SMS(I)+IK-1
          ITRI(IK) =IDI_SMS(KJ)
          INDEX(IK)=IK
        END DO  

        IF(NK/=0)THEN 

          IF(NK<16)THEN
C When #of connectivities are small
C Bubble sort is more efficient

           ITERATE=.TRUE.
           DO WHILE (ITERATE .EQV. .TRUE.)     
              ITERATE=.FALSE.
              DO J=1,NK-1
                IF(ITRI(J)> ITRI(J+1) )THEN
                  PERM = ITRI(J)
                  ITRI(J) = ITRI(J+1)
                  ITRI(J+1)=PERM

                  PERM = INDEX(J)
                  INDEX(J) = INDEX(J+1)
                  INDEX(J+1) = PERM

                  ITERATE = .TRUE.
                ENDIF
              ENDDO
           ENDDO
           DO IK=1,NK
             KJ=IAD_SMS(I)+IK-1
             IDI_SMS(KJ)=ITRI(IK)
           END DO
                

          ELSE
           CALL MY_ORDERS(0,WORK,ITRI,INDEX,NK,1)

           DO IK=1,NK
             KJ=IAD_SMS(I)+IK-1
             IDI_SMS(KJ)=ITRI(INDEX(IK))
           END DO

          ENDIF
        ENDIF



        DO IK=1,NK
          IKK        =INDEX(IK)
          INDEX2(IKK)=IK
        END DO

        DO KJ=KAD_SMS(I),KAD_SMS(I+1)-1
          IK        = KDI_SMS(KJ)
          PK_SMS(KJ)= INDEX2(TAGK(IK))
        END DO

        DO KJ=KAD_SMS(I),KAD_SMS(I+1)-1
          IK =KDI_SMS(KJ)
          TAGK(IK)=0
        END DO

      END DO
C-------------------------------------------------------------------------
      DO I=1,NUMNOD+1
        JAD_SMS(I)=IAD_SMS(I)
      END DO
      DO I=1,NUMNOD
        DO KJ=IAD_SMS(I),IAD_SMS(I+1)-1
          JDI_SMS(KJ)=IDI_SMS(KJ)
        END DO
      END DO
C-------------------------------------------------------------------------
C inter/type2 : numbering
C------------
      KINET(1:NUMNOD) = 0 
C
C     Tag des mains pour TYPE2 symetrisees
C
      DO N=1,NINTER
        NTY = IPARI(7,N)
        IF (NTY == 2) THEN
          NMN   = IPARI(6,N)
          ILEV  = IPARI(20,N)
c
          DO I=1,NMN                   
            J = INTBUF_TAB(N)%MSR(I)   
            IF (ILEV == 0 .OR. ILEV == 1 .OR. ILEV == 27 .OR. ILEV == 28) THEN
              KINET(J) = KINET(J)+1
            ENDIF  
          ENDDO    
        ENDIF
      ENDDO
C
      DO N=1,NINTER
        NTY = IPARI(7,N)
        IF (NTY == 2) THEN
          NMN   = IPARI(6,N)
          ILEV  = IPARI(20,N)
c
          DO I=1,NMN                   
            J = INTBUF_TAB(N)%MSR(I)   
            IF (ILEV == 0 .OR. ILEV == 1 .OR. ILEV == 27 .OR. ILEV == 28) THEN
              KINET(J) = KINET(J)+1
            ENDIF  
          ENDDO    
        ENDIF
      ENDDO
C
      DO N=1,NUMNOD
        IF(KINET(N)/=0) KINET(N)=MIN(IUN,KINET(N)-1) ! KINET == 1 <=> Incompatible conditions
      END DO
C------------
C
C---- First pass - detection of main nodes for crossed type 2 connection
C
      DO N=1,NINTER
        NTY   = IPARI(7,N)
        ILAGM = IPARI(33,N)
        ILEV  = IPARI(20,N)
        NSN   = IPARI(5,N)
        IF(NTY.EQ.2 .AND. ILAGM.EQ.0 .AND.ILEV/=25 .and. ILEV/=26.AND. ILEV/=27 .and. ILEV/=28)THEN
          DO II=1,NSN
            I=ABS(INTBUF_TAB(N)%NSV(II))
            L=INTBUF_TAB(N)%IRTLM(II)
            N1 = INTBUF_TAB(N)%IRECTM(4*(L-1)+1)
            N2 = INTBUF_TAB(N)%IRECTM(4*(L-1)+2)
            N3 = INTBUF_TAB(N)%IRECTM(4*(L-1)+3)
            N4 = INTBUF_TAB(N)%IRECTM(4*(L-1)+4)
C
            IF(NATIV_SMS(I)==0.AND.NATIV_SMS(N1)==0
     .                        .AND.NATIV_SMS(N2)==0
     .                        .AND.NATIV_SMS(N3)==0
     .                        .AND.NATIV_SMS(N4)==0) CYCLE
            T2MAIN_SMS(1,I)  = N1
            T2MAIN_SMS(2,I)  = N2
            T2MAIN_SMS(3,I)  = N3
            T2MAIN_SMS(4,I)  = N4

          ENDDO  
        ELSEIF(NTY.EQ.2 .AND. ILAGM.EQ.0 .AND.(ILEV==27.or.ILEV==28))THEN
           DO II=1,NSN
            I=ABS(INTBUF_TAB(N)%NSV(II))
            IF (INTBUF_TAB(N)%IRUPT(II).EQ.0) THEN
C           Kinematic node
              L=INTBUF_TAB(N)%IRTLM(II)
              N1 = INTBUF_TAB(N)%IRECTM(4*(L-1)+1)
              N2 = INTBUF_TAB(N)%IRECTM(4*(L-1)+2)
              N3 = INTBUF_TAB(N)%IRECTM(4*(L-1)+3)
              N4 = INTBUF_TAB(N)%IRECTM(4*(L-1)+4)
C
              IF(NATIV_SMS(I)==0.AND.NATIV_SMS(N1)==0
     .                        .AND.NATIV_SMS(N2)==0
     .                        .AND.NATIV_SMS(N3)==0
     .                        .AND.NATIV_SMS(N4)==0) CYCLE
              T2MAIN_SMS(1,I)  = N1
              T2MAIN_SMS(2,I)  = N2
              T2MAIN_SMS(3,I)  = N3
              T2MAIN_SMS(4,I)  = N4

            ENDIF
          ENDDO        
        ENDIF
      ENDDO
C
      DO N=1,NINTER
        NTY   = IPARI(7,N)
        ILAGM = IPARI(33,N)
        ILEV  = IPARI(20,N)
        IF(NTY.EQ.2 .AND. ILAGM.EQ.0 .AND.ILEV/=25 .and. ILEV/=26.AND. ILEV/=27 .and. ILEV/=28)THEN
          NSN=IPARI(5,N)
          DO II=1,NSN
            I=ABS(INTBUF_TAB(N)%NSV(II))
            L=INTBUF_TAB(N)%IRTLM(II)
            N1 = INTBUF_TAB(N)%IRECTM(4*(L-1)+1)
            N2 = INTBUF_TAB(N)%IRECTM(4*(L-1)+2)
            N3 = INTBUF_TAB(N)%IRECTM(4*(L-1)+3)
            N4 = INTBUF_TAB(N)%IRECTM(4*(L-1)+4)
    
            IF(NATIV_SMS(I)==0.AND.NATIV_SMS(N1)==0
     .                        .AND.NATIV_SMS(N2)==0
     .                        .AND.NATIV_SMS(N3)==0
     .                        .AND.NATIV_SMS(N4)==0) CYCLE

            DO KJ=JAD_SMS(I),JAD_SMS(I)+LAD_SMS(I)-1
              J =JDI_SMS(KJ)
              NODNX_SMS(J) =NODNX_SMS(J) +4
              NODNX_SMS(N1)=NODNX_SMS(N1)+1
              NODNX_SMS(N2)=NODNX_SMS(N2)+1
              NODNX_SMS(N3)=NODNX_SMS(N3)+1
              NODNX_SMS(N4)=NODNX_SMS(N4)+1
              NNZ_SMS = NNZ_SMS + 8
C-- Type2 crossed connection between main nodes
              IF ((T2MAIN_SMS(1,J).GT.0).AND.(I.GT.J)) THEN
                DO K =1,4
                  DO KK =1,4
                    IF (T2MAIN_SMS(K,I).NE.T2MAIN_SMS(KK,J)) THEN
                      NODNX_SMS(T2MAIN_SMS(K,I))=NODNX_SMS(T2MAIN_SMS(K,I))+1
                      NODNX_SMS(T2MAIN_SMS(KK,J))=NODNX_SMS(T2MAIN_SMS(KK,J))+1
                      NNZ_SMS = NNZ_SMS + 2
                    ENDIF
                  ENDDO
                ENDDO
              ENDIF
            END DO
          END DO
        ELSEIF(NTY.EQ.2 .AND. ILAGM.EQ.0 .AND.(ILEV==25.or.ILEV==26))THEN
          NSN=IPARI(5,N)
          DO II=1,NSN
            I=ABS(INTBUF_TAB(N)%NSV(II))
            L=INTBUF_TAB(N)%IRTLM(II)
            N1 = INTBUF_TAB(N)%IRECTM(4*(L-1)+1)
            N2 = INTBUF_TAB(N)%IRECTM(4*(L-1)+2)
            N3 = INTBUF_TAB(N)%IRECTM(4*(L-1)+3)
            N4 = INTBUF_TAB(N)%IRECTM(4*(L-1)+4)
    
            IF(NATIV_SMS(I)==0.AND.NATIV_SMS(N1)==0
     .                        .AND.NATIV_SMS(N2)==0
     .                        .AND.NATIV_SMS(N3)==0
     .                        .AND.NATIV_SMS(N4)==0) CYCLE

            NODNX_SMS(I) =NODNX_SMS(I) +4
            NODNX_SMS(N1)=NODNX_SMS(N1)+1
            NODNX_SMS(N2)=NODNX_SMS(N2)+1
            NODNX_SMS(N3)=NODNX_SMS(N3)+1
            NODNX_SMS(N4)=NODNX_SMS(N4)+1
            NNZ_SMS = NNZ_SMS + 8
          END DO
        ELSEIF(NTY.EQ.2 .AND. ILAGM.EQ.0 .AND.(ILEV==27.or.ILEV==28))THEN
          NSN=IPARI(5,N)
          DO II=1,NSN
            I=ABS(INTBUF_TAB(N)%NSV(II))
            IF (KINET(I).EQ.0) THEN
C           Kinematic node
              L=INTBUF_TAB(N)%IRTLM(II)
              N1 = INTBUF_TAB(N)%IRECTM(4*(L-1)+1)
              N2 = INTBUF_TAB(N)%IRECTM(4*(L-1)+2)
              N3 = INTBUF_TAB(N)%IRECTM(4*(L-1)+3)
              N4 = INTBUF_TAB(N)%IRECTM(4*(L-1)+4)
    
              IF(NATIV_SMS(I)==0.AND.NATIV_SMS(N1)==0
     .                          .AND.NATIV_SMS(N2)==0
     .                          .AND.NATIV_SMS(N3)==0
     .                          .AND.NATIV_SMS(N4)==0) CYCLE

              DO KJ=JAD_SMS(I),JAD_SMS(I)+LAD_SMS(I)-1
                J =JDI_SMS(KJ)
                NODNX_SMS(J) =NODNX_SMS(J) +4
                NODNX_SMS(N1)=NODNX_SMS(N1)+1
                NODNX_SMS(N2)=NODNX_SMS(N2)+1
                NODNX_SMS(N3)=NODNX_SMS(N3)+1
                NODNX_SMS(N4)=NODNX_SMS(N4)+1
                NNZ_SMS = NNZ_SMS + 8
C-- Type2 crossed connection between main nodes
                IF ((T2MAIN_SMS(1,J).GT.0).AND.(I.GT.J)) THEN
                  DO K =1,4
                    DO KK =1,4
                      IF (T2MAIN_SMS(K,I).NE.T2MAIN_SMS(KK,J)) THEN
                        NODNX_SMS(T2MAIN_SMS(K,I))=NODNX_SMS(T2MAIN_SMS(K,I))+1
                        NODNX_SMS(T2MAIN_SMS(KK,J))=NODNX_SMS(T2MAIN_SMS(KK,J))+1
                        NNZ_SMS = NNZ_SMS + 2
                      ENDIF
                    ENDDO
                  ENDDO
                ENDIF
              END DO
            ELSE
C           Penalty node
              L=INTBUF_TAB(N)%IRTLM(II)
              N1 = INTBUF_TAB(N)%IRECTM(4*(L-1)+1)
              N2 = INTBUF_TAB(N)%IRECTM(4*(L-1)+2)
              N3 = INTBUF_TAB(N)%IRECTM(4*(L-1)+3)
              N4 = INTBUF_TAB(N)%IRECTM(4*(L-1)+4)
    
              IF(NATIV_SMS(I)==0.AND.NATIV_SMS(N1)==0
     .                        .AND.NATIV_SMS(N2)==0
     .                        .AND.NATIV_SMS(N3)==0
     .                        .AND.NATIV_SMS(N4)==0) CYCLE

              NODNX_SMS(I) =NODNX_SMS(I) +4
              NODNX_SMS(N1)=NODNX_SMS(N1)+1
              NODNX_SMS(N2)=NODNX_SMS(N2)+1
              NODNX_SMS(N3)=NODNX_SMS(N3)+1
              NODNX_SMS(N4)=NODNX_SMS(N4)+1
              NNZ_SMS = NNZ_SMS + 8
            ENDIF
          END DO
        END IF
      END DO
C 
C reconstruit JAD_SMS
      JAD_SMS(1)=1
      DO I=1,NUMNOD
        JAD_SMS(I+1)=JAD_SMS(I)+NODNX_SMS(I)
      END DO
C-----------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  SMS_INI_JAD_2                 source/ams/sms_init.F         
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTSTAMP_MOD                  share/modules1/intstamp_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SMS_INI_JAD_2(
     2             IXC      ,IPARG   ,IXS      ,IXT      ,IXP     ,
     3             IXR      ,IXTG    ,IXS10    ,NODNX_SMS,JADC_SMS,
     4             JADS_SMS ,JADS10_SMS,JADT_SMS ,JADP_SMS,JADR_SMS ,
     5             JADTG_SMS,TAGPRT_SMS,KAD_SMS,KDI_SMS  ,
     6             TAGREL_SMS,IPARTS ,IPARTQ   ,IPARTC   ,IPARTT   ,
     7             IPARTP    ,IPARTR ,IPARTUR  ,IPARTTG  ,IPARTX   ,
     8             NPBY      ,LPBY   ,KINET    ,TAGSLV_RBY_SMS,IPARI,
     9             INTBUF_TAB,LAD_SMS ,NPRW    ,LPRW ,TAGMSR_RBY_SMS,
     A             INTSTAMP ,IPART    ,IGEO    ,NATIV_SMS,IRBE2   ,
     B             LRBE2    ,IAD_SMS  ,IDI_SMS ,JAD_SMS  ,JDI_SMS ,
     C             T2MAIN_SMS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTSTAMP_MOD
      USE INTBUFDEF_MOD 
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
#include "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "intstamp_c.inc"
#include      "kincod_c.inc"
#include      "param_c.inc"
#include      "sms_c.inc"
#include      "units_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER 
     .        IPARG(NPARG,*), IXC(NIXC,*), IXS(NIXS,*), IXT(NIXT,*), 
     .        IXP(NIXP,*), IXR(NIXR,*), IXTG(NIXTG,*), IXS10(6,*),
     .        NODNX_SMS(*), KAD_SMS(*), KDI_SMS(*), 
     .        IAD_SMS(*), IDI_SMS(*), JAD_SMS(*), JDI_SMS(*),
     .        JADC_SMS(4,*),
     .        JADS_SMS(8,*), JADS10_SMS(6,*), 
     .        JADT_SMS(2,*), 
     .        JADP_SMS(2,*),
     .        JADR_SMS(3,*), 
     .        JADTG_SMS(3,*),
     .        TAGPRT_SMS(*), TAGREL_SMS(*), 
     .        IPARTS(*), IPARTQ(*), IPARTC(*), IPARTT(*),
     .        IPARTP(*), IPARTR(*), IPARTUR(*), IPARTTG(*), IPARTX(*),
     .        NPBY(NNPBY,*), LPBY(*), KINET(*), TAGSLV_RBY_SMS(*),
     .        IPARI(NPARI,*), 
     .        LAD_SMS(*), 
     .        NPRW(*), LPRW(*), TAGMSR_RBY_SMS(*),  
     .        IPART(LIPART1,*), IGEO(NPROPGI,*), NATIV_SMS(*),
     .        IRBE2(NRBE2L,*), LRBE2(*), T2MAIN_SMS(4,*)

      TYPE(INTSTAMP_DATA) INTSTAMP(*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, JJ, KK, II, IJ, M, N, IERROR, KL,
     .        NHI, NS
      INTEGER MSR, NSN, KI, KJ, NAD_SMS(NUMNOD),  NAD_SMS_0(NUMNOD), 
     .        NSR, NSMS(2)
      INTEGER NSNW, IMOV
      INTEGER SIZE, LENR, IAD, L, LLT
      INTEGER NTY, ILAGM, JI, 
     .         N1, N2, N3, N4, N5, N6, 
     .         NMN, ILEV
      INTEGER IK
C-------------------------------------------------------------------------
C     PREPARE KOMPACTION OF ELEMENTARY MATRIX
C     KJ = KAD_SMS(I),KAD_SMS(I+1)-1 => PK_SMS(KJ) = rang de KDI_SMS(KJ) dans JDI_SMS(I),JDI_SMS(I+1)-1
C
C     Reconstruit JDI_SMS :: Recopie IDI_SMS (connectivite elementaire compactee et triee)
C-------------------------------------------------------------------------
      DO I=1,NUMNOD
        DO KJ=IAD_SMS(I),IAD_SMS(I+1)-1
          IK=KJ-IAD_SMS(I)
          JDI_SMS(JAD_SMS(I)+IK)=IDI_SMS(KJ)
        END DO
      END DO
C-------------------------------------------------------------------------
C inter/type2 : construction de JDI_SMS
C-------------------------------------------------------------------------
      DO I=1,NUMNOD
        NAD_SMS(I)=JAD_SMS(I)+LAD_SMS(I)
      END DO

C
      DO N=1,NINTER
        NTY   = IPARI(7,N)
        ILAGM = IPARI(33,N)
        ILEV  = IPARI(20,N)
        IF(NTY==2 .AND. ILAGM==0 .AND.ILEV/=25 .and. ILEV/=26 .AND.ILEV/=27 .and. ILEV/=28)THEN
C
          NSN=IPARI(5,N)
          DO II=1,NSN
            I=ABS(INTBUF_TAB(N)%NSV(II))
            L=INTBUF_TAB(N)%IRTLM(II)
            N1 = INTBUF_TAB(N)%IRECTM(4*(L-1)+1)
            N2 = INTBUF_TAB(N)%IRECTM(4*(L-1)+2)
            N3 = INTBUF_TAB(N)%IRECTM(4*(L-1)+3)
            N4 = INTBUF_TAB(N)%IRECTM(4*(L-1)+4)
    
            IF(NATIV_SMS(I)==0.AND.NATIV_SMS(N1)==0
     .                        .AND.NATIV_SMS(N2)==0
     .                        .AND.NATIV_SMS(N3)==0
     .                        .AND.NATIV_SMS(N4)==0) CYCLE

            DO KJ=JAD_SMS(I),JAD_SMS(I)+LAD_SMS(I)-1
              J =JDI_SMS(KJ)
C
              JDI_SMS(NAD_SMS(N1))=J
              NAD_SMS(N1)=NAD_SMS(N1)+1
              JDI_SMS(NAD_SMS(J))=N1
              NAD_SMS(J)=NAD_SMS(J)+1
C
              JDI_SMS(NAD_SMS(N2))=J
              NAD_SMS(N2)=NAD_SMS(N2)+1
              JDI_SMS(NAD_SMS(J))=N2
              NAD_SMS(J)=NAD_SMS(J)+1
C
              JDI_SMS(NAD_SMS(N3))=J
              NAD_SMS(N3)=NAD_SMS(N3)+1
              JDI_SMS(NAD_SMS(J))=N3
              NAD_SMS(J)=NAD_SMS(J)+1
C
              JDI_SMS(NAD_SMS(N4))=J
              NAD_SMS(N4)=NAD_SMS(N4)+1
              JDI_SMS(NAD_SMS(J))=N4
              NAD_SMS(J)=NAD_SMS(J)+1
C
C-- Type2 crossed connection between main nodes
              IF ((T2MAIN_SMS(1,J).GT.0).AND.(I.GT.J)) THEN
                DO K =1,4
                  DO KK =1,4
                    IF (T2MAIN_SMS(K,I).NE.T2MAIN_SMS(KK,J)) THEN
                      JDI_SMS(NAD_SMS(T2MAIN_SMS(K,I)))=T2MAIN_SMS(KK,J)
                      NAD_SMS(T2MAIN_SMS(K,I))=NAD_SMS(T2MAIN_SMS(K,I))+1
                      JDI_SMS(NAD_SMS(T2MAIN_SMS(KK,J)))=T2MAIN_SMS(K,I)
                      NAD_SMS(T2MAIN_SMS(KK,J))=NAD_SMS(T2MAIN_SMS(KK,J))+1
                    ENDIF
                  ENDDO
                ENDDO
              ENDIF
C
            END DO
          END DO
        ELSEIF(NTY==2.AND.ILAGM==0.AND.(ILEV==25.or.ILEV==26))THEN
          NSN=IPARI(5,N)
          DO II=1,NSN
            I=ABS(INTBUF_TAB(N)%NSV(II))
            L=INTBUF_TAB(N)%IRTLM(II)
            N1 = INTBUF_TAB(N)%IRECTM(4*(L-1)+1)
            N2 = INTBUF_TAB(N)%IRECTM(4*(L-1)+2)
            N3 = INTBUF_TAB(N)%IRECTM(4*(L-1)+3)
            N4 = INTBUF_TAB(N)%IRECTM(4*(L-1)+4)
    
            IF(NATIV_SMS(I)==0.AND.NATIV_SMS(N1)==0
     .                        .AND.NATIV_SMS(N2)==0
     .                        .AND.NATIV_SMS(N3)==0
     .                        .AND.NATIV_SMS(N4)==0) CYCLE

            JDI_SMS(NAD_SMS(N1))=I
            NAD_SMS(N1)=NAD_SMS(N1)+1
            JDI_SMS(NAD_SMS(I))=N1
            NAD_SMS(I)=NAD_SMS(I)+1

            JDI_SMS(NAD_SMS(N2))=I
            NAD_SMS(N2)=NAD_SMS(N2)+1
            JDI_SMS(NAD_SMS(I))=N2
            NAD_SMS(I)=NAD_SMS(I)+1

            JDI_SMS(NAD_SMS(N3))=I
            NAD_SMS(N3)=NAD_SMS(N3)+1
            JDI_SMS(NAD_SMS(I))=N3
            NAD_SMS(I)=NAD_SMS(I)+1

            JDI_SMS(NAD_SMS(N4))=I
            NAD_SMS(N4)=NAD_SMS(N4)+1
            JDI_SMS(NAD_SMS(I))=N4
            NAD_SMS(I)=NAD_SMS(I)+1
          END DO
C
        ELSEIF(NTY==2.AND.ILAGM==0.AND.(ILEV==27.or.ILEV==28))THEN
C
          NSN=IPARI(5,N)
          DO II=1,NSN
            I=ABS(INTBUF_TAB(N)%NSV(II))
            IF (KINET(I).EQ.0) THEN
C             Kinematic node
              L=INTBUF_TAB(N)%IRTLM(II)
              N1 = INTBUF_TAB(N)%IRECTM(4*(L-1)+1)
              N2 = INTBUF_TAB(N)%IRECTM(4*(L-1)+2)
              N3 = INTBUF_TAB(N)%IRECTM(4*(L-1)+3)
              N4 = INTBUF_TAB(N)%IRECTM(4*(L-1)+4)
    
              IF(NATIV_SMS(I)==0.AND.NATIV_SMS(N1)==0
     .                        .AND.NATIV_SMS(N2)==0
     .                        .AND.NATIV_SMS(N3)==0
     .                        .AND.NATIV_SMS(N4)==0) CYCLE
  
              DO KJ=JAD_SMS(I),JAD_SMS(I)+LAD_SMS(I)-1
                J =JDI_SMS(KJ)
C
                JDI_SMS(NAD_SMS(N1))=J
                NAD_SMS(N1)=NAD_SMS(N1)+1
                JDI_SMS(NAD_SMS(J))=N1
                NAD_SMS(J)=NAD_SMS(J)+1
C
                JDI_SMS(NAD_SMS(N2))=J
                NAD_SMS(N2)=NAD_SMS(N2)+1
                JDI_SMS(NAD_SMS(J))=N2
                NAD_SMS(J)=NAD_SMS(J)+1
C
                JDI_SMS(NAD_SMS(N3))=J
                NAD_SMS(N3)=NAD_SMS(N3)+1
                JDI_SMS(NAD_SMS(J))=N3
                NAD_SMS(J)=NAD_SMS(J)+1
C
                JDI_SMS(NAD_SMS(N4))=J
                NAD_SMS(N4)=NAD_SMS(N4)+1
                JDI_SMS(NAD_SMS(J))=N4
                NAD_SMS(J)=NAD_SMS(J)+1
C
C-- Type2 crossed connection between main nodes
                IF ((T2MAIN_SMS(1,J).GT.0).AND.(I.GT.J)) THEN
                  DO K =1,4
                    DO KK =1,4
                      IF (T2MAIN_SMS(K,I).NE.T2MAIN_SMS(KK,J)) THEN
                        JDI_SMS(NAD_SMS(T2MAIN_SMS(K,I)))=T2MAIN_SMS(KK,J)
                        NAD_SMS(T2MAIN_SMS(K,I))=NAD_SMS(T2MAIN_SMS(K,I))+1
                        JDI_SMS(NAD_SMS(T2MAIN_SMS(KK,J)))=T2MAIN_SMS(K,I)
                        NAD_SMS(T2MAIN_SMS(KK,J))=NAD_SMS(T2MAIN_SMS(KK,J))+1
                      ENDIF
                    ENDDO
                  ENDDO
C
                ENDIF
              END DO
C
            ELSE
C             Penalty node
              L=INTBUF_TAB(N)%IRTLM(II)
              N1 = INTBUF_TAB(N)%IRECTM(4*(L-1)+1)
              N2 = INTBUF_TAB(N)%IRECTM(4*(L-1)+2)
              N3 = INTBUF_TAB(N)%IRECTM(4*(L-1)+3)
              N4 = INTBUF_TAB(N)%IRECTM(4*(L-1)+4)
    
              IF(NATIV_SMS(I)==0.AND.NATIV_SMS(N1)==0
     .                        .AND.NATIV_SMS(N2)==0
     .                        .AND.NATIV_SMS(N3)==0
     .                        .AND.NATIV_SMS(N4)==0) CYCLE

              JDI_SMS(NAD_SMS(N1))=I
              NAD_SMS(N1)=NAD_SMS(N1)+1
              JDI_SMS(NAD_SMS(I))=N1
              NAD_SMS(I)=NAD_SMS(I)+1

              JDI_SMS(NAD_SMS(N2))=I
              NAD_SMS(N2)=NAD_SMS(N2)+1
              JDI_SMS(NAD_SMS(I))=N2
              NAD_SMS(I)=NAD_SMS(I)+1

              JDI_SMS(NAD_SMS(N3))=I
              NAD_SMS(N3)=NAD_SMS(N3)+1
              JDI_SMS(NAD_SMS(I))=N3
              NAD_SMS(I)=NAD_SMS(I)+1

              JDI_SMS(NAD_SMS(N4))=I
              NAD_SMS(N4)=NAD_SMS(N4)+1
              JDI_SMS(NAD_SMS(I))=N4
              NAD_SMS(I)=NAD_SMS(I)+1
            ENDIF
          END DO
        END IF
      END DO
C------------
C     Recalcule NNZ_SMS de la matrice compactee
C------------
      NNZ_SMS=0
      DO I=1,NUMNOD
        NODNX_SMS(I)=NAD_SMS(I)-JAD_SMS(I)
        NNZ_SMS=NNZ_SMS+NODNX_SMS(I)
      END DO
C------------
C reconstruit JAD_SMS
      JAD_SMS(1)=1
      DO I=1,NUMNOD
        JAD_SMS(I+1)=JAD_SMS(I)+NODNX_SMS(I)
      END DO
C-----------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  SMS_INI_JAD_3                 source/ams/sms_init.F         
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTSTAMP_MOD                  share/modules1/intstamp_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SMS_INI_JAD_3(
     2             IXC      ,IPARG   ,IXS      ,IXT      ,IXP     ,
     3             IXR      ,IXTG    ,IXS10    ,NODNX_SMS,JADC_SMS,
     4             JADS_SMS ,JADS10_SMS,JADT_SMS ,JADP_SMS,JADR_SMS ,
     5             JADTG_SMS,TAGPRT_SMS,KAD_SMS  ,KDI_SMS ,
     6             TAGREL_SMS,IPARTS ,IPARTQ   ,IPARTC   ,IPARTT   ,
     7             IPARTP    ,IPARTR ,IPARTUR  ,IPARTTG  ,IPARTX   ,
     8             NPBY     ,LPBY     ,KINET    ,
     9             TAGSLV_RBY_SMS,IPARI,INTBUF_TAB,
     A             LAD_SMS   ,JSM_SMS  ,INTSTAMP  ,IPART     , 
     B             IGEO     ,TAGMSR_RBY_SMS,NATIV_SMS,
     C             IAD_SMS   ,IDI_SMS,JAD_SMS  ,JDI_SMS  ,T2MAIN_SMS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTSTAMP_MOD
      USE INTBUFDEF_MOD
      USE MESSAGE_MOD  
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
#include "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "intstamp_c.inc"
#include      "kincod_c.inc"
#include      "param_c.inc"
#include      "sms_c.inc"
#include      "units_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER 
     .        IPARG(NPARG,*), IXC(NIXC,*), IXS(NIXS,*), IXT(NIXT,*), 
     .        IXP(NIXP,*), IXR(NIXR,*), IXTG(NIXTG,*), IXS10(6,*),
     .        NODNX_SMS(*), KAD_SMS(*), KDI_SMS(*),
     .        IAD_SMS(*), IDI_SMS(*), JAD_SMS(*), JDI_SMS(*),
     .        JADC_SMS(4,*),
     .        JADS_SMS(8,*), JADS10_SMS(6,*), 
     .        JADT_SMS(2,*), 
     .        JADP_SMS(2,*),
     .        JADR_SMS(3,*), 
     .        JADTG_SMS(3,*),NATIV_SMS(*),
     .        TAGPRT_SMS(*), TAGREL_SMS(*), 
     .        IPARTS(*), IPARTQ(*), IPARTC(*), IPARTT(*),
     .        IPARTP(*), IPARTR(*), IPARTUR(*), IPARTTG(*), IPARTX(*),
     .        NPBY(NNPBY,*), LPBY(*), KINET(*), TAGSLV_RBY_SMS(*),
     .        IPARI(NPARI,*), 
     .        LAD_SMS(*), JSM_SMS(*), 
     .        IPART(LIPART1,*), IGEO(NPROPGI,*), TAGMSR_RBY_SMS(*), T2MAIN_SMS(4,*)
      TYPE(INTSTAMP_DATA) INTSTAMP(*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, JJ, KK, II, IJ, M, N, IERROR, KL
      INTEGER MSR, NSN, KI, KJ, NAD_SMS(NUMNOD), NAD_SMS_0(NUMNOD), 
     .        NSR
      INTEGER SIZE, LENR, IAD, L, LLT
      INTEGER NTY, ILAGM, K10, K11, K12, K13, K14, JI, 
     .         N1, N2, N3, N4,
     .         NMN, ILEV, ERROR
      INTEGER IK, NK, K1, K2, KM
C-------------------------------------------------------------------------
C     PREPARE KOMPACTION OF ELEMENTARY MATRIX
C     KJ = KAD_SMS(I),KAD_SMS(I+1)-1 => PK_SMS(KJ) = rang de KDI_SMS(KJ) dans JDI_SMS(I),JDI_SMS(I+1)-1
C
C     Reconstruit JDI_SMS :: Recopie IDI_SMS (connectivite elementaire compactee et triee)
C-------------------------------------------------------------------------
      DO I=1,NUMNOD
        DO KJ=IAD_SMS(I),IAD_SMS(I+1)-1
          IK=KJ-IAD_SMS(I)
          JDI_SMS(JAD_SMS(I)+IK)=IDI_SMS(KJ)
        END DO
      END DO
C-------------------------------------------------------------------------
C     PREPARE JSM_SMS (connectivite elementaire)
C-------------------------------------------------------------------------
      DO I=1,NUMNOD
        DO KJ=JAD_SMS(I),JAD_SMS(I)+LAD_SMS(I)-1
          J =JDI_SMS(KJ)
cc        IF(I < J)THEN
C
C         dichotomie (recherche parmi les voisins ordonnes de J)
          K1=JAD_SMS(J)
          K2=JAD_SMS(J)+LAD_SMS(J)-1
 100      CONTINUE
          KM=(K1+K2)/2
          IF(JDI_SMS(K1) == I)THEN
            JSM_SMS(KJ)=K1
cc            JSM_SMS(K1)=KJ
            GOTO 200
          ELSEIF(JDI_SMS(K2) == I)THEN
            JSM_SMS(KJ)=K2
cc            JSM_SMS(K2)=KJ
            GOTO 200
          ELSEIF(JDI_SMS(KM) == I)THEN
            JSM_SMS(KJ)=KM
cc            JSM_SMS(KM)=KJ
            GOTO 200
          ELSEIF(JDI_SMS(KM) < I)THEN
            K1=KM
            GOTO 100
          ELSE ! JDI_SMS(KM) > I
            K2=KM
            GOTO 100
          END IF
          WRITE(6,*) ' ** internal error in AMS initialization'
 200      CONTINUE
cc        END IF
        END DO
      END DO
C
      DO I=1,NUMNOD
        NAD_SMS(I)=JAD_SMS(I)+LAD_SMS(I)
      END DO
C
C inter/type2 : reconstruction (jdi et jsm)
C------------
      DO N=1,NINTER
        NTY   = IPARI(7,N)
        ILAGM = IPARI(33,N)
        ILEV  = IPARI(20,N)
        IF(NTY==2 .AND. ILAGM==0 .AND.ILEV/=25 .and. ILEV/=26.AND.ILEV/=27 .and. ILEV/=28)THEN
C
          NSN=IPARI(5,N)
          DO II=1,NSN
            I=ABS(INTBUF_TAB(N)%NSV(II))

            L=INTBUF_TAB(N)%IRTLM(II)
            N1 = INTBUF_TAB(N)%IRECTM(4*(L-1)+1)
            N2 = INTBUF_TAB(N)%IRECTM(4*(L-1)+2)
            N3 = INTBUF_TAB(N)%IRECTM(4*(L-1)+3)
            N4 = INTBUF_TAB(N)%IRECTM(4*(L-1)+4)
    
            IF(NATIV_SMS(I)==0.AND.NATIV_SMS(N1)==0
     .                        .AND.NATIV_SMS(N2)==0
     .                        .AND.NATIV_SMS(N3)==0
     .                        .AND.NATIV_SMS(N4)==0) CYCLE

            DO KJ=JAD_SMS(I),JAD_SMS(I)+LAD_SMS(I)-1
              J =JDI_SMS(KJ)

              JSM_SMS(NAD_SMS(N1))=NAD_SMS(J)
              JSM_SMS(NAD_SMS(J)) =NAD_SMS(N1)
              JDI_SMS(NAD_SMS(N1))=J
              NAD_SMS(N1)=NAD_SMS(N1)+1
              JDI_SMS(NAD_SMS(J))=N1
              NAD_SMS(J)=NAD_SMS(J)+1

              JSM_SMS(NAD_SMS(N2))=NAD_SMS(J)
              JSM_SMS(NAD_SMS(J)) =NAD_SMS(N2)
              JDI_SMS(NAD_SMS(N2))=J
              NAD_SMS(N2)=NAD_SMS(N2)+1
              JDI_SMS(NAD_SMS(J))=N2
              NAD_SMS(J)=NAD_SMS(J)+1

              JSM_SMS(NAD_SMS(N3))=NAD_SMS(J)
              JSM_SMS(NAD_SMS(J)) =NAD_SMS(N3)
              JDI_SMS(NAD_SMS(N3))=J
              NAD_SMS(N3)=NAD_SMS(N3)+1
              JDI_SMS(NAD_SMS(J))=N3
              NAD_SMS(J)=NAD_SMS(J)+1

              JSM_SMS(NAD_SMS(N4))=NAD_SMS(J)
              JSM_SMS(NAD_SMS(J)) =NAD_SMS(N4)
              JDI_SMS(NAD_SMS(N4))=J
              NAD_SMS(N4)=NAD_SMS(N4)+1
              JDI_SMS(NAD_SMS(J))=N4
              NAD_SMS(J)=NAD_SMS(J)+1
C
C-- Type2 crossed connection between main nodes
              IF ((T2MAIN_SMS(1,J).GT.0).AND.(I.GT.J)) THEN
                DO K =1,4
                  DO KK =1,4
                    IF (T2MAIN_SMS(K,I).NE.T2MAIN_SMS(KK,J)) THEN
                      JSM_SMS(NAD_SMS(T2MAIN_SMS(K,I)))=NAD_SMS(T2MAIN_SMS(KK,J))
                      JSM_SMS(NAD_SMS(T2MAIN_SMS(KK,J)))=NAD_SMS(T2MAIN_SMS(K,I))
                      JDI_SMS(NAD_SMS(T2MAIN_SMS(K,I)))=T2MAIN_SMS(KK,J)
                      NAD_SMS(T2MAIN_SMS(K,I))=NAD_SMS(T2MAIN_SMS(K,I))+1
                      JDI_SMS(NAD_SMS(T2MAIN_SMS(KK,J)))=T2MAIN_SMS(K,I)
                      NAD_SMS(T2MAIN_SMS(KK,J))=NAD_SMS(T2MAIN_SMS(KK,J))+1
                    ENDIF
                  ENDDO
                ENDDO
              ENDIF
C
            END DO
          END DO
        ELSEIF(NTY==2.AND.ILAGM==0.AND.(ILEV==25.or.ILEV==26))THEN
          K10=IPARI(1,N)
          K11=K10+4*IPARI(3,N)
          K12=K11+4*IPARI(4,N)
          K13=K12+IPARI(5,N)
          K14=K13+IPARI(6,N)
          NSN=IPARI(5,N)
          DO II=1,NSN
            I=ABS(INTBUF_TAB(N)%NSV(II))
            L=INTBUF_TAB(N)%IRTLM(II)
            N1 = INTBUF_TAB(N)%IRECTM(4*(L-1)+1)
            N2 = INTBUF_TAB(N)%IRECTM(4*(L-1)+2)
            N3 = INTBUF_TAB(N)%IRECTM(4*(L-1)+3)
            N4 = INTBUF_TAB(N)%IRECTM(4*(L-1)+4)
    
            IF(NATIV_SMS(I)==0.AND.NATIV_SMS(N1)==0
     .                        .AND.NATIV_SMS(N2)==0
     .                        .AND.NATIV_SMS(N3)==0
     .                        .AND.NATIV_SMS(N4)==0) CYCLE

            JSM_SMS(NAD_SMS(N1))=NAD_SMS(I)
            JSM_SMS(NAD_SMS(I)) =NAD_SMS(N1)
            JDI_SMS(NAD_SMS(N1))=I
            NAD_SMS(N1)=NAD_SMS(N1)+1
            JDI_SMS(NAD_SMS(I))=N1
            NAD_SMS(I)=NAD_SMS(I)+1

            JSM_SMS(NAD_SMS(N2))=NAD_SMS(I)
            JSM_SMS(NAD_SMS(I)) =NAD_SMS(N2)
            JDI_SMS(NAD_SMS(N2))=I
            NAD_SMS(N2)=NAD_SMS(N2)+1
            JDI_SMS(NAD_SMS(I))=N2
            NAD_SMS(I)=NAD_SMS(I)+1

            JSM_SMS(NAD_SMS(N3))=NAD_SMS(I)
            JSM_SMS(NAD_SMS(I)) =NAD_SMS(N3)
            JDI_SMS(NAD_SMS(N3))=I
            NAD_SMS(N3)=NAD_SMS(N3)+1
            JDI_SMS(NAD_SMS(I))=N3
            NAD_SMS(I)=NAD_SMS(I)+1

            JSM_SMS(NAD_SMS(N4))=NAD_SMS(I)
            JSM_SMS(NAD_SMS(I)) =NAD_SMS(N4)
            JDI_SMS(NAD_SMS(N4))=I
            NAD_SMS(N4)=NAD_SMS(N4)+1
            JDI_SMS(NAD_SMS(I))=N4
            NAD_SMS(I)=NAD_SMS(I)+1
          END DO
        ELSEIF(NTY==2.AND.ILAGM==0.AND.(ILEV==27.or.ILEV==28))THEN
C
          NSN=IPARI(5,N)
          DO II=1,NSN
            I=ABS(INTBUF_TAB(N)%NSV(II))
            IF (KINET(I).EQ.0) THEN
C           Kinematic node
 
              L=INTBUF_TAB(N)%IRTLM(II)
              N1 = INTBUF_TAB(N)%IRECTM(4*(L-1)+1)
              N2 = INTBUF_TAB(N)%IRECTM(4*(L-1)+2)
              N3 = INTBUF_TAB(N)%IRECTM(4*(L-1)+3)
              N4 = INTBUF_TAB(N)%IRECTM(4*(L-1)+4)
      
              IF(NATIV_SMS(I)==0.AND.NATIV_SMS(N1)==0
     .                        .AND.NATIV_SMS(N2)==0
     .                        .AND.NATIV_SMS(N3)==0
     .                        .AND.NATIV_SMS(N4)==0) CYCLE

              DO KJ=JAD_SMS(I),JAD_SMS(I)+LAD_SMS(I)-1
                J =JDI_SMS(KJ)

                JSM_SMS(NAD_SMS(N1))=NAD_SMS(J)
                JSM_SMS(NAD_SMS(J)) =NAD_SMS(N1)
                JDI_SMS(NAD_SMS(N1))=J
                NAD_SMS(N1)=NAD_SMS(N1)+1
                JDI_SMS(NAD_SMS(J))=N1
                NAD_SMS(J)=NAD_SMS(J)+1

                JSM_SMS(NAD_SMS(N2))=NAD_SMS(J)
                JSM_SMS(NAD_SMS(J)) =NAD_SMS(N2)
                JDI_SMS(NAD_SMS(N2))=J
                NAD_SMS(N2)=NAD_SMS(N2)+1
                JDI_SMS(NAD_SMS(J))=N2
                NAD_SMS(J)=NAD_SMS(J)+1

                JSM_SMS(NAD_SMS(N3))=NAD_SMS(J)
                JSM_SMS(NAD_SMS(J)) =NAD_SMS(N3)
                JDI_SMS(NAD_SMS(N3))=J
                NAD_SMS(N3)=NAD_SMS(N3)+1
                JDI_SMS(NAD_SMS(J))=N3
                NAD_SMS(J)=NAD_SMS(J)+1

                JSM_SMS(NAD_SMS(N4))=NAD_SMS(J)
                JSM_SMS(NAD_SMS(J)) =NAD_SMS(N4)
                JDI_SMS(NAD_SMS(N4))=J
                NAD_SMS(N4)=NAD_SMS(N4)+1
                JDI_SMS(NAD_SMS(J))=N4
                NAD_SMS(J)=NAD_SMS(J)+1
C
C-- Type2 crossed connection between main nodes
                IF ((T2MAIN_SMS(1,J).GT.0).AND.(I.GT.J)) THEN
                  DO K =1,4
                    DO KK =1,4
                      IF (T2MAIN_SMS(K,I).NE.T2MAIN_SMS(KK,J)) THEN
                        JSM_SMS(NAD_SMS(T2MAIN_SMS(K,I)))=NAD_SMS(T2MAIN_SMS(KK,J))
                        JSM_SMS(NAD_SMS(T2MAIN_SMS(KK,J)))=NAD_SMS(T2MAIN_SMS(K,I))
                        JDI_SMS(NAD_SMS(T2MAIN_SMS(K,I)))=T2MAIN_SMS(KK,J)
                        NAD_SMS(T2MAIN_SMS(K,I))=NAD_SMS(T2MAIN_SMS(K,I))+1
                        JDI_SMS(NAD_SMS(T2MAIN_SMS(KK,J)))=T2MAIN_SMS(K,I)
                        NAD_SMS(T2MAIN_SMS(KK,J))=NAD_SMS(T2MAIN_SMS(KK,J))+1
                      ENDIF
                    ENDDO
                  ENDDO
                ENDIF
C
              END DO
            ELSE
C           Penalty node
              L=INTBUF_TAB(N)%IRTLM(II)
              N1 = INTBUF_TAB(N)%IRECTM(4*(L-1)+1)
              N2 = INTBUF_TAB(N)%IRECTM(4*(L-1)+2)
              N3 = INTBUF_TAB(N)%IRECTM(4*(L-1)+3)
              N4 = INTBUF_TAB(N)%IRECTM(4*(L-1)+4)
    
              IF(NATIV_SMS(I)==0.AND.NATIV_SMS(N1)==0
     .                        .AND.NATIV_SMS(N2)==0
     .                        .AND.NATIV_SMS(N3)==0
     .                        .AND.NATIV_SMS(N4)==0) CYCLE

              JSM_SMS(NAD_SMS(N1))=NAD_SMS(I)
              JSM_SMS(NAD_SMS(I)) =NAD_SMS(N1)
              JDI_SMS(NAD_SMS(N1))=I
              NAD_SMS(N1)=NAD_SMS(N1)+1
              JDI_SMS(NAD_SMS(I))=N1
              NAD_SMS(I)=NAD_SMS(I)+1

              JSM_SMS(NAD_SMS(N2))=NAD_SMS(I)
              JSM_SMS(NAD_SMS(I)) =NAD_SMS(N2)
              JDI_SMS(NAD_SMS(N2))=I
              NAD_SMS(N2)=NAD_SMS(N2)+1
              JDI_SMS(NAD_SMS(I))=N2
              NAD_SMS(I)=NAD_SMS(I)+1

              JSM_SMS(NAD_SMS(N3))=NAD_SMS(I)
              JSM_SMS(NAD_SMS(I)) =NAD_SMS(N3)
              JDI_SMS(NAD_SMS(N3))=I
              NAD_SMS(N3)=NAD_SMS(N3)+1
              JDI_SMS(NAD_SMS(I))=N3
              NAD_SMS(I)=NAD_SMS(I)+1

              JSM_SMS(NAD_SMS(N4))=NAD_SMS(I)
              JSM_SMS(NAD_SMS(I)) =NAD_SMS(N4)
              JDI_SMS(NAD_SMS(N4))=I
              NAD_SMS(N4)=NAD_SMS(N4)+1
              JDI_SMS(NAD_SMS(I))=N4
              NAD_SMS(I)=NAD_SMS(I)+1
            ENDIF
          END DO
        END IF
      END DO
C------------
      DO I=1,NUMNOD
        NAD_SMS_0(I)=NAD_SMS(I)
      END DO
C------------
      DO I=1,NUMNOD
        LAD_SMS(I)=JAD_SMS(I)  + LAD_SMS(I) - 1
      END DO
c      DO I=1,NUMNOD
c        do kj=JAD_SMS(I),JAD_SMS(I+1)-1
c          print *,i,jdi_sms(kj),jdi_sms(jsm_sms(kj))
c        end do
c      END DO
C-----------------------------------------------
C Check of the symmetrization operator JSM_SMS
C-----------------------------------------------
      ERROR = 0
      DO I=1,NUMNOD
        DO IJ=JAD_SMS(I),JAD_SMS(I+1)-1
          J=JDI_SMS(IJ)
          IF(J > I)THEN
            JI=JSM_SMS(IJ)
            IF (IJ.NE.JSM_SMS(JI)) ERROR = 1
          END IF
        END DO
      END DO
C
      IF (ERROR.EQ.1) THEN
        CALL ANCMSG(MSGID=1242,ANMODE=ANINFO,MSGTYPE=MSGERROR)
        CALL ARRET(2)
      ENDIF
C-----------------------------------------------
      RETURN
      END
