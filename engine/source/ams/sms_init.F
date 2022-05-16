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
Chd|  SMS_INI_PART                  source/ams/sms_init.F         
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SMS_INI_PART(IGRPART ,TAGPRT_SMS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
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
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "sms_c.inc"
#include      "spmd_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER 
     .   TAGPRT_SMS(*)
C-----------------------------------------------
      TYPE (GROUP_) , DIMENSION(NGRPART) :: IGRPART
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IP,IAD,N
C-----------------------------------------------
C
      IF(IDTGRS==0)THEN
        DO IP=1,NPART
          TAGPRT_SMS(IP)=1
        END DO
      ELSE
        DO IP=1,NPART
          TAGPRT_SMS(IP)=0
        END DO
        IF(IDTGRS < 0)THEN
         DO N=1,NGRPART
          IF (IGRPART(N)%ID==-IDTGRS) THEN
            IDTGRS=N
            GO TO 120
          END IF
         END DO
 100     CONTINUE
         CALL ANCMSG(MSGID=21,ANMODE=ANINFO_BLIND,
     .               I1=-IDTGRS)
         CALL ARRET(2)
 120     CONTINUE
        END IF
!
        DO I=1,IGRPART(IDTGRS)%NENTITY
          IP=IGRPART(IDTGRS)%ENTITY(I)
          TAGPRT_SMS(IP)=1
        END DO
      END IF
C
C-----------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  SMS_INI_RBY                   source/ams/sms_init.F         
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SMS_INI_RBY(
     1  KINET   ,NPRW      ,LPRW     ,NPBY     , LPBY    ,
     2  TAGMSR_RBY_SMS,TAGSLV_RBY_SMS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
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
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "sms_c.inc"
#include      "spmd_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER 
     .   KINET(*),NPRW(*), LPRW(*),NPBY(NNPBY,*), LPBY(*),
     .   TAGMSR_RBY_SMS(*), TAGSLV_RBY_SMS(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, NG, N, JJ, KK, ITY, NEL, NFT, ISOLNOD, 
     .        IAD, IP, ILOC4(4)
      INTEGER SIZE, LENR, ISMS, NM, NS, IMOV, NSN, ILAGM,
     .        N2, N3, N4, N5, N6
      INTEGER M, MSR, NSNW, KI, NHI, NSMS(2)
      INTEGER J1, IPERM1(6), IPERM2(6)
      DATA ILOC4/1,3,6,5/
      DATA IPERM1/1,2,3,1,2,3/
      DATA IPERM2/2,3,1,4,4,4/
C
C-----------------------------------------------
C rbodies : numbering
C------------
      TAGMSR_RBY_SMS(1:NUMNOD) =0
      TAGSLV_RBY_SMS(1:NUMNOD) =0
C
      IAD=0
      ISMS=0
      DO M=1,NRBODY
C
        MSR=NPBY(1,M)
        NSN=NPBY(2,M)
        IF(MSR >= 0) THEN
C if msr secnd of lagrange wall => no ams
          ISMS=0
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
                  ISMS=1
                  GOTO 100
                END IF
              END DO
            END IF
            K  =K+NSN
          END DO
 100      CONTINUE
          IF(ISMS==0.AND.NPBY(7,M).GT.0 .AND. 
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

C-----------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  SMS_INI_KAD                   source/ams/sms_init.F         
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SMS_INI_KAD(
     1  IXS     ,IXQ      ,IXC     ,IXT     ,IXP       ,
     2  IXR     ,IXTG     ,IXTG1   ,IXS10   ,IXS16     ,
     3  IXS20   ,IPARG    ,MS      ,MS0     ,NODNX_SMS ,
     4  ICODT   ,ICODR    ,KINET   ,INDX1_SMS,
     5  KAD_SMS ,IPARTS   ,IPARTQ   ,
     6  IPARTC  ,IPARTT  ,IPARTP   ,IPARTR    ,IPARTUR  ,
     7  IPARTTG ,IPARTX  ,TAGPRT_SMS,TAGREL_SMS,ITAB    ,
     8  WEIGHT  ,IRBE2   ,IRBE3     ,LRBE2    ,LRBE3    ,
     9  IAD_ELEM,FR_ELEM ,NPRW      ,LPRW     ,IPART    ,
     A  IGEO    ,NATIV_SMS)
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
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "sms_c.inc"
#include      "spmd_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER 
     .        IXS(NIXS,*),IXS10(6,*) ,IXS16(6,*) ,IXS20(12,*),
     .        IXQ(NIXQ,*),IXC(NIXC,*), IXT(NIXT,*), IXP(NIXP,*),
     .        IXR(NIXR,*), IXTG(NIXTG,*), IXTG1(4,*),
     .        IPARG(NPARG,*), 
     .   NODNX_SMS(*), ICODT(*), ICODR(*), KINET(*), 
     .   INDX1_SMS(*),
     .   KAD_SMS(*), 
     .   IPARTS(*),IPARTQ(*),IPARTC(*),IPARTT(*),
     .   IPARTP(*),IPARTR(*),IPARTUR(*),IPARTTG(*),IPARTX(*),
     .   TAGPRT_SMS(*), TAGREL_SMS(*),
     .   ITAB(*), WEIGHT(*), 
     .   IRBE2(NRBE2L,*), IRBE3(NRBE3L,*), LRBE2(*), LRBE3(*),
     .   IAD_ELEM(2,NSPMD+1) ,FR_ELEM(*), NPRW(*), LPRW(*),
     .   IPART(LIPART1,*), IGEO(NPROPGI,*), NATIV_SMS(*)
C     REAL ou REAL*8
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
C-----------------------------------------------
      DO I=1,NUMNOD
        NAD_SMS(I)=0
      END DO

      KNZ_SMS = 0

      TAGREL_SMS(1:NGROUP)=0
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
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        STARTIMEG                     source/system/timer.F         
Chd|        STOPTIMEG                     source/system/timer.F         
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SMS_INI_KDI(
     2             IXC      ,IPARG   ,IXS      ,IXT      ,IXP     ,
     3             IXR      ,IXTG    ,IXS10    ,NODNX_SMS,KAD_SMS ,
     4             KDI_SMS  ,JADC_SMS,JADS_SMS ,JADS10_SMS,
     5             JADT_SMS ,JADP_SMS,
     6             JADR_SMS,JADTG_SMS,INDX1_SMS,TAGPRT_SMS,IAD_SMS ,
     7             TAGREL_SMS,IPARTS ,IPARTQ   ,IPARTC   ,IPARTT   ,
     8             IPARTP    ,IPARTR ,IPARTUR  ,IPARTTG  ,IPARTX   ,
     9             IAD_ELEM  ,FR_ELEM,NPBY     ,LPBY     ,KINET    ,
     A             TAGSLV_RBY_SMS,IPARI,INTBUF_TAB,IRECT    ,
     B             LAD_SMS   ,IPART  ,IGEO     ,WEIGHT   ,
     C             NATIV_SMS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
#include "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "kincod_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "sms_c.inc"
#include      "task_c.inc"
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
     .        INDX1_SMS(*), TAGPRT_SMS(*), TAGREL_SMS(*), 
     .        IPARTS(*), IPARTQ(*), IPARTC(*), IPARTT(*),
     .        IPARTP(*), IPARTR(*), IPARTUR(*), IPARTTG(*), IPARTX(*),
     .        IAD_ELEM(2,NSPMD+1) ,FR_ELEM(*),
     .        NPBY(NNPBY,*), LPBY(*), KINET(*), TAGSLV_RBY_SMS(*),
     .        IPARI(NPARI,*), IRECT(4,*),
     .        LAD_SMS(*), KDI_SMS(*), 
     .        IPART(LIPART1,*), IGEO(NPROPGI,*), WEIGHT(*)
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
      INTEGER NTY, ILAGM, K10, K11, K12, K13, K14, JI, 
     .         N1, N2, N3, N4, LNEW, ILEV
      INTEGER J1, IPERM1(6), IPERM2(6),IPENTA6(6)
      INTEGER LSMSPCG
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
 250  CONTINUE
#include "lockon.inc"
      IF(NSGDONE.GT.NGROUP) THEN
#include "lockoff.inc"
         GOTO 252
      ENDIF
      NG=NSGDONE
      NSGDONE = NG + 1
#include "lockoff.inc"
C
      IF(TAGREL_SMS(NG)==0)GOTO 250
      ITY   =IPARG(5,NG)
      IF (IDDW.GT.0) CALL STARTIMEG(NG)

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
      IF (IDDW.GT.0) CALL STOPTIMEG(NG)
      GOTO 250
 252  CONTINUE
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
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SMS_INI_JAD_1(
     2             IXC      ,IPARG   ,IXS      ,IXT      ,IXP     ,
     3             IXR      ,IXTG    ,IXS10    ,NODNX_SMS,JADC_SMS,
     4             JADS_SMS ,JADS10_SMS,JADT_SMS ,JADP_SMS,JADR_SMS ,
     5             JADTG_SMS,INDX1_SMS,TAGPRT_SMS,
     6             KAD_SMS,KDI_SMS  ,PK_SMS   ,
     7             TAGREL_SMS,IPARTS ,IPARTQ   ,IPARTC   ,IPARTT   ,
     8             IPARTP    ,IPARTR ,IPARTUR  ,IPARTTG  ,IPARTX   ,
     9             IAD_ELEM  ,FR_ELEM,NPBY     ,LPBY     ,KINET    ,
     A             TAGSLV_RBY_SMS,IPARI,INTBUF_TAB,IRECT ,
     B             LAD_SMS   ,IPART ,IGEO     ,WEIGHT    ,NATIV_SMS,
     C             IAD_SMS   ,IDI_SMS,JAD_SMS  ,JDI_SMS  ,T2MAIN_SMS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
#include "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "kincod_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "sms_c.inc"
#include      "task_c.inc"
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
     .        INDX1_SMS(*), TAGPRT_SMS(*), TAGREL_SMS(*), 
     .        IPARTS(*), IPARTQ(*), IPARTC(*), IPARTT(*),
     .        IPARTP(*), IPARTR(*), IPARTUR(*), IPARTTG(*), IPARTX(*),
     .        IAD_ELEM(2,NSPMD+1) ,FR_ELEM(*),
     .        NPBY(NNPBY,*), LPBY(*), KINET(*), TAGSLV_RBY_SMS(*),
     .        IPARI(NPARI,*), IRECT(4,*),
     .        LAD_SMS(*), 
     .        IPART(LIPART1,*), IGEO(NPROPGI,*), WEIGHT(*),T2MAIN_SMS(6,*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, JJ, KK, II, IJ, M, N, IERROR, KL
      INTEGER MSR, NSN, KI, KJ, NAD_SMS(NUMNOD), 
     .        NSR
      INTEGER SIZE, LENR, IAD, L, LLT
      INTEGER NTY, ILAGM, K10, K11, K12, K13, K14, JI, 
     .         N1, N2, N3, N4, LNEW, ILEV
      INTEGER TAGK(NUMNOD), IK, NK, IKK,
     .        ITRI(NUMNOD),INDEX(2*NUMNOD),INDEX2(NUMNOD), WORK(70000)
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
        IF(NK/=0) CALL MY_ORDERS(0,WORK,ITRI,INDEX,NK,1)
        DO IK=1,NK
          KJ=IAD_SMS(I)+IK-1
          IDI_SMS(KJ)=ITRI(INDEX(IK))
        END DO

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
C
C     T2MAIN_SMS(1) : nb of type2 main nodes (4 or 1)
C     T2MAIN_SMS(2-5) : id of type2 main nodes
C     T2MAIN_SMS(6) : flag for deleted main element
C
      DO I=1,NUMNOD
C---    If node is not secnd of type2 kinematic interface it is its own main --
        T2MAIN_SMS(1,I)  = 1
        T2MAIN_SMS(2,I)  = I
      ENDDO
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
C
            T2MAIN_SMS(1,I)  = 4
            T2MAIN_SMS(2,I)  = N1
            T2MAIN_SMS(3,I)  = N2
            T2MAIN_SMS(4,I)  = N3
            T2MAIN_SMS(5,I)  = N4
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
C
              T2MAIN_SMS(1,I)  = 4
              T2MAIN_SMS(2,I)  = N1
              T2MAIN_SMS(3,I)  = N2
              T2MAIN_SMS(4,I)  = N3
              T2MAIN_SMS(5,I)  = N4
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
              IF ((T2MAIN_SMS(1,J) > 1).AND.(I > J)) THEN
                DO K =2,5
                  DO KK =2,5
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

            IF(WEIGHT(I)/=1)CYCLE

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
            IF (INTBUF_TAB(N)%IRUPT(II).EQ.0) THEN
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
                NODNX_SMS(J) =NODNX_SMS(J) +4
                NODNX_SMS(N1)=NODNX_SMS(N1)+1
                NODNX_SMS(N2)=NODNX_SMS(N2)+1
                NODNX_SMS(N3)=NODNX_SMS(N3)+1
                NODNX_SMS(N4)=NODNX_SMS(N4)+1
                NNZ_SMS = NNZ_SMS + 8
C-- Type2 crossed connection between main nodes
                IF ((T2MAIN_SMS(1,J) > 1).AND.(I > J)) THEN
                  DO K =2,5
                    DO KK =2,5
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
              IF(WEIGHT(I)/=1)CYCLE
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
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTSTAMP_MOD                  share/modules/intstamp_mod.F  
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SMS_INI_JAD_2(
     2             IXC      ,IPARG   ,IXS      ,IXT      ,IXP     ,
     3             IXR      ,IXTG    ,IXS10    ,NODNX_SMS,JADC_SMS,
     4             JADS_SMS ,JADS10_SMS,JADT_SMS ,JADP_SMS,JADR_SMS ,
     5             JADTG_SMS,INDX1_SMS,TAGPRT_SMS,KAD_SMS,KDI_SMS  ,
     6             TAGREL_SMS,IPARTS ,IPARTQ   ,IPARTC   ,IPARTT   ,
     7             IPARTP    ,IPARTR ,IPARTUR  ,IPARTTG  ,IPARTX   ,
     8             IAD_ELEM  ,FR_ELEM,NPBY     ,LPBY     ,KINET    ,
     9             TAGSLV_RBY_SMS,IPARI,INTBUF_TAB,IRECT    ,
     A             LAD_SMS   ,NPRW   ,LPRW,TAGMSR_RBY_SMS,
     B             TAGSLV_I21_SMS ,TAGMSR_I21_SMS,JADI21_SMS,INTSTAMP ,
     .                                                       IPART    ,
     C             IGEO      ,WEIGHT ,NATIV_SMS,IRBE2      ,LRBE2     ,
     B             IAD_SMS  ,IDI_SMS ,JAD_SMS  ,JDI_SMS  ,T2MAIN_SMS)
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
#include      "comlock.inc"
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
#include      "parit_c.inc"
#include      "sms_c.inc"
#include      "task_c.inc"
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
     .        INDX1_SMS(*), TAGPRT_SMS(*), TAGREL_SMS(*), 
     .        IPARTS(*), IPARTQ(*), IPARTC(*), IPARTT(*),
     .        IPARTP(*), IPARTR(*), IPARTUR(*), IPARTTG(*), IPARTX(*),
     .        IAD_ELEM(2,NSPMD+1) ,FR_ELEM(*),
     .        NPBY(NNPBY,*), LPBY(*), KINET(*), TAGSLV_RBY_SMS(*),
     .        IPARI(NPARI,*), IRECT(4,*),
     .        LAD_SMS(*), 
     .        NPRW(*), LPRW(*), TAGMSR_RBY_SMS(*),  
     .        TAGSLV_I21_SMS(*), TAGMSR_I21_SMS(*), JADI21_SMS(*),
     .        IPART(LIPART1,*), IGEO(NPROPGI,*), WEIGHT(*), NATIV_SMS(*),
     .        IRBE2(NRBE2L,*), LRBE2(*), T2MAIN_SMS(6,*)

      TYPE(INTSTAMP_DATA) INTSTAMP(*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, JJ, KK, II, IJ, M, N, IERROR, KL,
     .        NHI, NS
      INTEGER MSR, NSN, KI, KJ, NAD_SMS(NUMNOD),
     .        NSR, NSMS(2)
      INTEGER NSNW, IMOV
      INTEGER SIZE, LENR, IAD, L, LLT
      INTEGER NTY, ILAGM, JI, 
     .         N1, N2, N3, N4, N5, N6, ISMS, 
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
              IF ((T2MAIN_SMS(1,J) > 1).AND.(I > J)) THEN
                DO K =2,5
                  DO KK =2,5
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

            IF(WEIGHT(I)/=1)CYCLE

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
            IF (INTBUF_TAB(N)%IRUPT(II).EQ.0) THEN
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
                IF ((T2MAIN_SMS(1,J) > 1).AND.(I > J)) THEN
                  DO K =2,5
                    DO KK =2,5
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
              IF(WEIGHT(I)/=1)CYCLE
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
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        SPMD_ALLGLOB_ISUM9            source/mpi/generic/spmd_allglob_isum9.F
Chd|        SPMD_EXCH_NODNX               source/mpi/ams/spmd_exch_nodnx.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTSTAMP_MOD                  share/modules/intstamp_mod.F  
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SMS_INI_JAD_3(
     2             IXC      ,IPARG   ,IXS      ,IXT      ,IXP     ,
     3             IXR      ,IXTG    ,IXS10    ,NODNX_SMS,JADC_SMS,
     4             JADS_SMS ,JADS10_SMS,JADT_SMS ,JADP_SMS,JADR_SMS ,
     5             JADTG_SMS ,INDX1_SMS,TAGPRT_SMS,KAD_SMS,KDI_SMS  ,
     6             TAGREL_SMS,IPARTS ,IPARTQ   ,IPARTC   ,IPARTT   ,
     7             IPARTP    ,IPARTR ,IPARTUR  ,IPARTTG  ,IPARTX   ,
     8             IAD_ELEM  ,FR_ELEM,NPBY     ,LPBY     ,KINET    ,
     9             TAGSLV_RBY_SMS,IPARI,INTBUF_TAB,IRECT    ,
     A             LAD_SMS   ,JSM_SMS  ,TAGSLV_I21_SMS ,INTSTAMP  ,
     .                                                  IPART     , 
     B             IGEO     ,TAGMSR_RBY_SMS,WEIGHT,NATIV_SMS,
     C             IAD_SMS  ,IDI_SMS ,JAD_SMS ,JDI_SMS ,T2MAIN_SMS)
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
#include      "comlock.inc"
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
#include      "parit_c.inc"
#include      "sms_c.inc"
#include      "task_c.inc"
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
     .        INDX1_SMS(*), TAGPRT_SMS(*), TAGREL_SMS(*), 
     .        IPARTS(*), IPARTQ(*), IPARTC(*), IPARTT(*),
     .        IPARTP(*), IPARTR(*), IPARTUR(*), IPARTTG(*), IPARTX(*),
     .        IAD_ELEM(2,NSPMD+1) ,FR_ELEM(*),
     .        NPBY(NNPBY,*), LPBY(*), KINET(*), TAGSLV_RBY_SMS(*),
     .        IPARI(NPARI,*), IRECT(4,*),
     .        LAD_SMS(*), JSM_SMS(*), 
     .        TAGSLV_I21_SMS(*),
     .        IPART(LIPART1,*), IGEO(NPROPGI,*), TAGMSR_RBY_SMS(*),
     .        WEIGHT(*),T2MAIN_SMS(6,*)
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
      INTEGER LSMSPCG
      INTEGER IK, K1, K2, KM
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
C     PREPARE JSM_SMS
C-------------------------------------------------------------------------
C
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
C-------------------------------------------------------------------------
      LSMSPCG=0
C------------
C inter/type2 : reconstruction (jsm)
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
            IF(NODNX_SMS(I)/=0) LSMSPCG=LSMSPCG-1

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
              IF ((T2MAIN_SMS(1,J) > 1).AND.(I > J)) THEN
                DO K =2,5
                  DO KK =2,5
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

            IF(WEIGHT(I)/=1)CYCLE

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
            IF (INTBUF_TAB(N)%IRUPT(II).EQ.0) THEN
C           Kinematic node
              IF(NODNX_SMS(I)/=0) LSMSPCG=LSMSPCG-1
 
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
                IF ((T2MAIN_SMS(1,J) > 1).AND.(I > J)) THEN
                  DO K =2,5
                    DO KK =2,5
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
              IF(WEIGHT(I)/=1)CYCLE

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
        CALL ANCMSG(MSGID=273,ANMODE=ANINFO)
        CALL ARRET(2)
      ENDIF
C-----------------------------------------------
C       COMMUNICATION 
C-----------------------------------------------
      IF(NSPMD.GT.1) THEN
        SIZE = 1
        LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
C
C Echange NODNX_SMS
C
        CALL SPMD_EXCH_NODNX(NODNX_SMS,IAD_ELEM ,FR_ELEM,LENR)
      END IF
C-----------------------------------------------
      NINDX1_SMS=0
      DO I=1,NUMNOD
       IF(NODNX_SMS(I)/=0)THEN
         NINDX1_SMS=NINDX1_SMS+1
         INDX1_SMS(NINDX1_SMS)=I
       END IF
      END DO
      LSMSPCG=LSMSPCG+NINDX1_SMS
      IF(NSPMD.GT.1) 
     .  CALL SPMD_ALLGLOB_ISUM9(LSMSPCG,1)	
      NSMSPCG=MIN(NSMSPCG,3*LSMSPCG)
C
C------------
C CHeck of the symmetrization operator JSM_SMS
C------------
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
        CALL ANCMSG(MSGID=273,ANMODE=ANINFO)
        CALL ARRET(2)
      ENDIF
C-----------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  SMS_INI_KIN_1                 source/ams/sms_init.F         
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        SPMD_ALLGLOB_ISUM9            source/mpi/generic/spmd_allglob_isum9.F
Chd|        SPMD_FRWALL_NN                source/mpi/kinematic_conditions/spmd_frwall_nn.F
Chd|        SPMD_GLOB_IMAX9               source/mpi/generic/spmd_glob_imax9.F
Chd|        SPMD_IBCAST                   source/mpi/generic/spmd_ibcast.F
Chd|        SPMD_SD_CJ_2                  source/mpi/kinematic_conditions/spmd_sd_cj_2.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SMS_INI_KIN_1(
     1  NODNX_SMS ,INDX1_SMS ,ILINK     ,RLINK    ,NNLINK    ,
     2  LNLINK    ,TAG_LNK_SMS,FR_LL    ,FR_RL    ,WEIGHT    ,
     3  ITAB      ,LJOINT     ,IADCJ    ,FR_CJ    ,NPRW      ,
     4  LPRW      ,FR_WALL    ,NRWL_SMS ,IAD_ELEM ,FR_ELEM   ,
     5  INTBUF_TAB )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
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
#include      "scr03_c.inc"
#include      "sms_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER 
     .   NODNX_SMS(*), INDX1_SMS(*),
     .   ILINK(*), RLINK(*), NNLINK(10,*), LNLINK(*),
     .   TAG_LNK_SMS(*), FR_LL(NSPMD+2,*), FR_RL(NSPMD+2,*),
     .   WEIGHT(*), ITAB(*), LJOINT(*), FR_CJ(*),IADCJ(NSPMD+1,*)
      INTEGER NPRW(*), LPRW(*), FR_WALL(NSPMD+2,*) ,NRWL_SMS(*),
     .        IAD_ELEM(2,*), FR_ELEM(*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K1, K, I, N, J, IC, NSN, ISMS,
     .        NOD2ADD(NUMNOD), ICSIZE, IMOV, ITYP, ILAGM, ICOUNT,
     .        TAG(NUMNOD)
      INTEGER NTY, ILEV, NMN, NRTS, NRTM,
     .        NLINS, NLINM, II, SIZE, LENR
      my_real
     .        IDMAX,ID
C-----------------------------------------------
C
      NOD2ADD(1:NUMNOD)=0
C
      IF(NRLINK/=0)THEN
        K = 1
        DO I=1,NRLINK

          K1=4*I-3
          IC=ILINK(K1+1)
          IF(IC==0) CYCLE
          NSN = ILINK(K1)


          IDMAX=0
          DO J=1,NSN
           N=RLINK(K+J-1)
           IF(WEIGHT(N)==1)THEN
             ID=ITAB(N)
             IDMAX=MAX(IDMAX,ID)
           END IF
          END DO

          IF(NSPMD > 1) THEN
            CALL SPMD_GLOB_IMAX9(IDMAX,1)
            CALL SPMD_IBCAST(IDMAX,IDMAX,1,1,0,2)
          END IF

          TAG_LNK_SMS(I)=-IDMAX

          ISMS=0
          DO J=1,NSN
            N=RLINK(K+J-1)
            IF(NODNX_SMS(N)/=0)THEN
              ISMS=1
              EXIT
            END IF
          END DO

          IF(NSPMD > 1) CALL SPMD_ALLGLOB_ISUM9(ISMS,1)
          
          IF(ISMS/=0) TAG_LNK_SMS(I) = ABS(TAG_LNK_SMS(I))

          IF(ISMS/=0)THEN
C
C propagate AMS to all nodes of the rlink
            DO J=1,NSN
              N=RLINK(K+J-1)
              IF(NODNX_SMS(N)==0.AND.NOD2ADD(N)==0)THEN
                NINDX1_SMS=NINDX1_SMS+1
                INDX1_SMS(NINDX1_SMS)=N
                NOD2ADD(N)=1
              END IF
            END DO
C
          END IF
          K = K + NSN
        END DO
      END IF
C-----------------------------------------------
      IF(NLINK/=0)THEN
        K = 1
        DO I=1,NLINK
          IC=NNLINK(3,I)
          IF(IC==0) CYCLE
          NSN = NNLINK(1,I)


          IDMAX=ZERO
          DO J=1,NSN
           N=LNLINK(K+J-1)
           IF(WEIGHT(N)==1)THEN
             ID=ITAB(N)
             IDMAX=MAX(IDMAX,ID)
           END IF
          END DO

          IF(NSPMD > 1) THEN
            CALL SPMD_GLOB_IMAX9(IDMAX,1)
            CALL SPMD_IBCAST(IDMAX,IDMAX,1,1,0,2)
          END IF

          TAG_LNK_SMS(NRLINK+I)=-IDMAX

          ISMS=0
          DO J=1,NSN
            N=LNLINK(K+J-1)
            IF(NODNX_SMS(N)/=0)THEN
              ISMS=1
              EXIT
            END IF
          END DO

          IF(NSPMD > 1) CALL SPMD_ALLGLOB_ISUM9(ISMS,1)

          IF(ISMS/=0) TAG_LNK_SMS(NRLINK+I) = ABS(TAG_LNK_SMS(NRLINK+I))

          IF(ISMS/=0)THEN
C
C propagate AMS to all nodes of the rlink
            DO J=1,NSN
              N=LNLINK(K+J-1)
              IF(NODNX_SMS(N)==0.AND.NOD2ADD(N)==0)THEN
                NINDX1_SMS=NINDX1_SMS+1
                INDX1_SMS(NINDX1_SMS)=N
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
                IF(NODNX_SMS(N)/=0)THEN
                  ISMS=1
                  EXIT
                END IF
              END DO

            TAG_LNK_SMS(NRLINK+NLINK+J)=ISMS

            K=K+NSN+1
          END DO
        END IF

        IF(NSPMD > 1) 
     .    CALL SPMD_IBCAST(TAG_LNK_SMS(NRLINK+NLINK+1),
     .                     TAG_LNK_SMS(NRLINK+NLINK+1),NJOINT,1,0,2)

        IF(NSPMD==1)THEN
          K=1
          DO J=1,NJOINT
            ISMS=TAG_LNK_SMS(NRLINK+NLINK+J)
            IF(ISMS/=0)THEN
              NSN=LJOINT(K)
              DO I=1,NSN
                N=LJOINT(K+I)
                 IF(NODNX_SMS(N)==0.AND.NOD2ADD(N)==0)THEN
                  NINDX1_SMS=NINDX1_SMS+1
                  INDX1_SMS(NINDX1_SMS)=N
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
                  IF(NODNX_SMS(N)==0.AND.NOD2ADD(N)==0)THEN
                    NINDX1_SMS=NINDX1_SMS+1
                    INDX1_SMS(NINDX1_SMS)=N
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
     .        ICSIZE=ICSIZE+IADCJ(NSPMD+1,N)-IADCJ(1,N)
          END DO
          CALL SPMD_SD_CJ_2(NOD2ADD,LJOINT,FR_CJ,IADCJ,ICSIZE,
     .                      TAG_LNK_SMS(NRLINK+NLINK+1),NODNX_SMS,
     .                                                  INDX1_SMS)
        END IF
      END IF
C-----------------------------------------------
      DO N=1,NUMNOD
        IF(NOD2ADD(N)/=0)NODNX_SMS(N)=1
      END DO
C-----------------------------------------------
C liste des noeuds sms du mur dans NRWL_SMS (memoire non optimisee).
      IF(NRWALL/=0)THEN
        K = 1
        DO N=1,NRWALL
          NSN=NPRW(N)
          ICOUNT   =K
          IMOV =NPRW(2*NRWALL+N)
          ITYP =NPRW(3*NRWALL+N)
          ILAGM=NPRW(5*NRWALL+N)
          IF(ILAGM==0)THEN
            DO J=1,NSN
              I=LPRW(K+J-1)
              IF(NODNX_SMS(I)/=0)THEN
                NRWL_SMS(ICOUNT)=J
                ICOUNT=ICOUNT+1
              END IF
            END DO
          END IF
C nb de noeuds sms dans le mur.
          NPRW(6*NRWALL+N)=ICOUNT-K
C for sms_fixvel, etc
          IF(IMOV /= 0)THEN
            NOD2ADD(IMOV)=0
            IF(ICOUNT > K.AND.NODNX_SMS(IMOV)==0)NOD2ADD(IMOV)=1
            IF(NSPMD > 1)
     .        CALL SPMD_FRWALL_NN(FR_WALL(1,N),NOD2ADD(IMOV))
            IF(NOD2ADD(IMOV)/=0)THEN
              NINDX1_SMS=NINDX1_SMS+1
              INDX1_SMS(NINDX1_SMS)=IMOV
            END IF
          END IF
          K  =K+NSN
        END DO
      END IF
C-----------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  SMS_INI_KIN_2                 source/ams/sms_init.F         
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        SPMD_GLOB_IMAX9               source/mpi/generic/spmd_glob_imax9.F
Chd|        SPMD_IBCAST                   source/mpi/generic/spmd_ibcast.F
Chd|====================================================================
      SUBROUTINE SMS_INI_KIN_2(
     1  ILINK     ,RLINK    ,NNLINK    ,LNLINK    ,TAG_LNK_SMS,
     2  FR_LL    ,FR_RL     ,WEIGHT    ,ITAB      ,LJOINT     ,
     3  IADCJ    ,FR_CJ     ,NPRW      ,LPRW      ,FR_WALL    ,
     4  NRWL_SMS ,IAD_ELEM  ,FR_ELEM  )
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
#include      "scr03_c.inc"
#include      "sms_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER 
     .   ILINK(*), RLINK(*), NNLINK(10,*), LNLINK(*),
     .   TAG_LNK_SMS(*), FR_LL(NSPMD+2,*), FR_RL(NSPMD+2,*),
     .   WEIGHT(*), ITAB(*), LJOINT(*), FR_CJ(*),IADCJ(NSPMD+1,*)
      INTEGER NPRW(*), LPRW(*), FR_WALL(NSPMD+2,*) ,NRWL_SMS(*),
     .        IAD_ELEM(2,*), FR_ELEM(*)
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K1, K, I, N, J, IC, NSN
      my_real
     .        IDMAX,ID
C-----------------------------------------------
      IF(NRLINK/=0)THEN
        K = 1
        DO I=1,NRLINK

          K1=4*I-3
          IC=ILINK(K1+1)
          IF(IC==0) CYCLE
          NSN = ILINK(K1)

          IDMAX=0
          DO J=1,NSN
           N=RLINK(K+J-1)
           IF(WEIGHT(N)==1)THEN
             ID=ITAB(N)
             IDMAX=MAX(IDMAX,ID)
           END IF
          END DO

          IF(NSPMD > 1) THEN
            CALL SPMD_GLOB_IMAX9(IDMAX,1)
            CALL SPMD_IBCAST(IDMAX,IDMAX,1,1,0,2)
          END IF
          
          TAG_LNK_SMS(I)=-IDMAX

          K = K + NSN
        END DO
      END IF
C-----------------------------------------------
      IF(NLINK/=0)THEN
        K = 1
        DO I=1,NLINK
          IC=NNLINK(3,I)
          IF(IC==0) CYCLE
          NSN = NNLINK(1,I)

          IDMAX=ZERO
          DO J=1,NSN
           N=LNLINK(K+J-1)
           IF(WEIGHT(N)==1)THEN
             ID=ITAB(N)
             IDMAX=MAX(IDMAX,ID)
           END IF
          END DO

          IF(NSPMD > 1) THEN
            CALL SPMD_GLOB_IMAX9(IDMAX,1)
            CALL SPMD_IBCAST(IDMAX,IDMAX,1,1,0,2)
          END IF
          
          TAG_LNK_SMS(NRLINK+I)=-IDMAX

          K = K + NSN
        END DO
      END IF
C-----------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  SMS_INI_INT                   source/ams/sms_init.F         
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        SPMD_EXCH_ICONT               source/mpi/nodes/spmd_exch_icont.F
Chd|        SPMD_EXCH_SMST2               source/mpi/ams/spmd_exch_smst2.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SMS_INI_INT(
     1  IPARI    ,INTBUF_TAB     ,IAD_ELEM   ,FR_ELEM  ,INTLIST,
     2  NBINTC)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
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
#include      "scr03_c.inc"
#include      "sms_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*), IAD_ELEM(2,*), FR_ELEM(*)
      INTEGER INTLIST(*),NBINTC
C     REAL ou REAL*8
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K, I, N, J, TAG(NUMNOD)
      INTEGER NTY, ILEV, NSN, NMN, NRTS, NRTM,
     .        NLINS, NLINM, II, SIZE, LENR
C-----------------------------------------------
C supprime nds d'interf type 2 des interfs a penalite
C /DT/AMS or /DT/INTER/AMS
C
      TAG(1:NUMNOD)=0
      DO N=1,NINTER
     	NTY=IPARI(7,N)
        ILEV  = IPARI(20,N)
        IF(NTY==2 .AND. ILEV/=25 .and. ILEV /= 26)THEN
     	  NRTS  =IPARI(3,N)
     	  NRTM  =IPARI(4,N)
     	  NSN	=IPARI(5,N)
     	  NMN	=IPARI(6,N)
     	  ILEV  =IPARI(20,N)
C
     	  DO II=1,NSN	     
            J=INTBUF_TAB(N)%NSV(II)
            IF ((ILEV==27.OR.ILEV==28).AND.INTBUF_TAB(N)%IRUPT(II).EQ.1) CYCLE
     	    TAG(J)=1
     	  ENDDO
     	ENDIF
      ENDDO
C
      IF(NSPMD > 1) THEN
     	SIZE = 1
     	LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
     	CALL SPMD_EXCH_ICONT(TAG,IAD_ELEM ,FR_ELEM,SIZE,LENR)
        CALL SPMD_EXCH_SMST2(IPARI,TAG,INTLIST,NBINTC,INTBUF_TAB)
      END IF
C
      DO N=1,NINTER
     	 NTY=IPARI(7,N)
     	 NSN   =IPARI(5,N)
     	 NRTS  =IPARI(3,N)
     	 NRTM  =IPARI(4,N)
     	 IF(NTY==7.OR.NTY==10.OR.NTY==20.OR.NTY==25)THEN
     	   DO II=1,NSN
             J=INTBUF_TAB(N)%NSV(II)
     	     IF(TAG(J)/=0) THEN
               INTBUF_TAB(N)%STFNS(II) = ZERO
     	     END IF
     	   END DO
     	   DO II=1,NRTM
     	     J=INTBUF_TAB(N)%IRECTM(4*(II-1)+1)
     	     IF(TAG(J)/=0)THEN
               INTBUF_TAB(N)%STFM(II)=ZERO
     	     END IF
     	     J=INTBUF_TAB(N)%IRECTM(4*(II-1)+2)
     	     IF(TAG(J)/=0)THEN
               INTBUF_TAB(N)%STFM(II)=ZERO
     	     END IF
     	     J=INTBUF_TAB(N)%IRECTM(4*(II-1)+3)
     	     IF(TAG(J)/=0)THEN
               INTBUF_TAB(N)%STFM(II)=ZERO
     	     END IF
     	     J=INTBUF_TAB(N)%IRECTM(4*(II-1)+4)
     	     IF(TAG(J)/=0)THEN
               INTBUF_TAB(N)%STFM(II)=ZERO
     	     END IF
     	   END DO
     	   IF(NTY==20)THEN
     	     NLINS  =IPARI(51,N)
     	     NLINM  =IPARI(52,N)
     	     IF(NLINS+NLINM /= 0)THEN
     	       DO II=1,NLINS
     		 J=INTBUF_TAB(N)%IXLINS(2*(II-1)+1)
     		 IF(TAG(J)/=0)THEN
                   INTBUF_TAB(N)%STFS(II) = ZERO
     		 END IF
     		 J=INTBUF_TAB(N)%IXLINS(2*(II-1)+2)
     		 IF(TAG(J)/=0)THEN
                   INTBUF_TAB(N)%STFS(II) = ZERO
     		 END IF
     	       END DO
     	       DO II=1,NLINM
     		 J=INTBUF_TAB(N)%IXLINM(2*(II-1)+1)
     		 IF(TAG(J)/=0)THEN
                   INTBUF_TAB(N)%STF(II) = ZERO
     		 END IF
     		 J=INTBUF_TAB(N)%IXLINM(2*(II-1)+2)
     		 IF(TAG(J)/=0)THEN
                   INTBUF_TAB(N)%STF(II) = ZERO
     		 END IF
     	       END DO
     	     END IF
     	   END IF
     	 ELSEIF(NTY==11)THEN
     	   DO II=1,NRTS
     	     J=INTBUF_TAB(N)%IRECTS(2*(II-1)+1)
     	     IF(TAG(J)/=0)THEN
               INTBUF_TAB(N)%STFS(II) = ZERO
     	     END IF
     	     J=INTBUF_TAB(N)%IRECTS(2*(II-1)+2)
     	     IF(TAG(J)/=0)THEN
               INTBUF_TAB(N)%STFS(II) = ZERO
     	     END IF
     	   END DO
     	   DO II=1,NRTM
     	     J=INTBUF_TAB(N)%IRECTM(2*(II-1)+1)
     	     IF(TAG(J)/=0)THEN
               INTBUF_TAB(N)%STFM(II) = ZERO
     	     END IF
     	     J=INTBUF_TAB(N)%IRECTM(2*(II-1)+2)
     	     IF(TAG(J)/=0)THEN
               INTBUF_TAB(N)%STFM(II) = ZERO
     	     END IF
     	   END DO
     	 ELSEIF(NTY==21)THEN
     	   DO II=1,NSN
     	     J=INTBUF_TAB(N)%NSV(II)
     	     IF(TAG(J)/=0) THEN
               INTBUF_TAB(N)%STFNS(II) = ZERO
     	     END IF
     	   END DO
     	 END IF
      END DO  
C-----------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  SMS_INI_ERR                   source/ams/sms_init.F         
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        SPMD_ALLGLOB_ISUM9            source/mpi/generic/spmd_allglob_isum9.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SMS_INI_ERR(NPRW    ,LPRW    ,KINET   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
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
#include      "parit_c.inc"
#include      "sms_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER 
     .        KINET(*), NPRW(*), LPRW(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, M, N, NSN,
     .        N1, N2, N3, N4, N5, N6
      INTEGER ITY, IMOV, ILAGM, ISMS, IERR
C-----------------------------------------------
C
      IERR=0
C
C-----
      ISMS=0
      DO I=1,NUMNOD
        IF(IRV(KINET(I))/=0)THEN
          ISMS=1
        END IF
      END DO
      CALL SPMD_ALLGLOB_ISUM9(ISMS,1)
      IF(ISMS/=0)THEN
       IF(ISPMD==0)THEN
         CALL ANCMSG(MSGID=22,ANMODE=ANINFO_BLIND,
     .               C1='RIVETS')
       END IF
       IERR=1
      END IF
C
C-----
      ISMS=0
      DO I=1,NUMNOD
        IF(ILMULT(KINET(I))/=0)THEN
          ISMS=1
        END IF
      END DO
      CALL SPMD_ALLGLOB_ISUM9(ISMS,1)
      IF(ISMS/=0)THEN
       IF(ISPMD==0)THEN
         CALL ANCMSG(MSGID=22,ANMODE=ANINFO_BLIND,
     .               C1='LAGRANGE MULTIPLIERS')
       END IF
       IERR=1
      END IF
C
C-----------------------------------------------
      IF(IERR/=0) CALL ARRET(2)
      RETURN
      END
