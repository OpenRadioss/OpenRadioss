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
Chd|  RBYONF                        source/constraints/general/rbody/rbyonf.F
Chd|-- called by -----------
Chd|        LECTUR                        source/input/lectur.F         
Chd|-- calls ---------------
Chd|        RBYPID                        source/constraints/general/rbody/rbypid.F
Chd|        SPMD_GLOB_ISUM9               source/mpi/interfaces/spmd_th.F
Chd|        SPMD_IBCAST                   source/mpi/generic/spmd_ibcast.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE RBYONF(IPARG   ,IPARI         ,MS     ,IN     ,
     2                  IXS     ,IXQ    ,IXC   ,IXT    ,IXP    ,
     3                  IXR     ,SKEW   ,ITAB  ,ITABM1 ,ISKWN  ,
     4                  NPBY    ,ONOF   ,NRBYNF,ITAG   ,LPBY   ,
     5                  RBY     ,X      ,V     ,VR     ,IXTG   ,
     6                  IGRV    ,IBGR   ,WEIGHT,FR_RBY2,PARTSAV,
     7                  IPART   ,ELBUF_TAB,ICFIELD,LCFIELD,TAGSLV_RBY)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
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
#include      "units_c.inc"
#include      "task_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*), IPARI(*), IXS(NIXS,*), IXQ(NIXQ,*),
     .   IXC(NIXC,*), IXT(NIXT,*), IXP(NIXP,*), IXR(NIXR,*),
     .   ITAB(*), ITABM1(*),IGRV(NIGRV,*),IBGR(*),IPART(*),
     .   ISKWN(LISKN,*), NPBY(NNPBY,*),ITAG(*),LPBY(*), IXTG(NIXTG,*),
     .   WEIGHT(*), FR_RBY2(*), ICFIELD(SIZFIELD,*), LCFIELD(*), TAGSLV_RBY(*)
      INTEGER ONOF,NRBYNF,PRI_OFF
C     REAL
      my_real
     .   SKEW(LSKEW,*),MS(*),IN(*),RBY(NRBY,*),X(3,*),
     .   V(3,*),VR(3,*),PARTSAV(*)
      TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J ,ITEMP(10),K, K0, N,NSL,NN,IAD,ONOF1,ISENS,IACTI,
     .        ONFELT,K1,K2,K3,K4,K5,K6,K7,K8,K9
C======================================================================|
C     MODIFICATION DES RIGID BODY
C-------------------------------------------
      K1=1+LIPART1*NPART+2*9*NPART
      K2=K1+NUMELS
      K3=K2+NUMELQ
      K4=K3+NUMELC
      K5=K4+NUMELT
      K6=K5+NUMELP
      K7=K6+NUMELR
C
      DO I=1,NUMNOD
       ITAG(I)=0
      ENDDO
C
      DO I=1,NUMNOD
       ITAG(I+NUMNOD)=0
      ENDDO
C
      DO N=1,NRBYKIN
        ISENS=NPBY(4,N)
        IACTI=NPBY(7,N)
        IF(ISENS==0.AND.IACTI==1.AND.NPBY(1,N)>0)
     .    ITAG(NPBY(1,N)+NUMNOD)=N
      ENDDO
C
      DO I=1,(NRBYNF+9)/10
          READ(IIN,'(10I10)')(ITEMP(J),J=1,10)
          DO 120 J=1,10
           IF(ITEMP(J)==0) GOTO 120
           K = 1
           DO  N=1,NRBYKIN
              IF(NPBY(1,N)>0) THEN
                IF(ITEMP(J)==ITAB(NPBY(1,N))) GOTO 110
              ENDIF
              K=K+NPBY(2,N)
           ENDDO
C
c           IF(IMACH/=3) THEN
c             GOTO 120
c           ELSE
             N = 0
c           END IF
 110       CONTINUE
C En SPMD, il faut communiquer le RB concerne si N<>0 sur un proc
C si rigid body present sur le proc N = rb trouve
              IF(N/=0) N = N*WEIGHT(NPBY(1,N))
C reduction pour retrouver la valeur de N (N = 0 partout sauf sur le proc main)
              IF(NSPMD > 1) THEN
                CALL SPMD_GLOB_ISUM9(N,1)
C broadcast de N sur tous les procs
                CALL SPMD_IBCAST(N,N,1,1,0,2)
              ENDIF
C si N = 0, alors le rby n avait ete trouve sur aucun proc
              IF(N==0) GOTO 120
C
              IF(ONOF==0)THEN
               IF(ISPMD==0)
     .         WRITE(IOUT,'(/A,I9,A)')' RIGID BODY:',ITEMP(J),' SET OFF'
              ELSE
               IF(ISPMD==0)
     .         WRITE(IOUT,'(/A,I9,A)')' RIGID BODY:',ITEMP(J),' SET ON'
              ENDIF
C
              ISENS=NPBY(4,N)
              IACTI=NPBY(7,N)
              IF(ISENS/=0)THEN
               IF(IACTI>1)THEN
C               body waiting for deactivation, override sensor request.
                IACTI=1
                NPBY(7,N)=IACTI
               ELSEIF(IACTI<0)THEN
C               body waiting for activation, override sensor request.
                IACTI=0
                NPBY(7,N)=IACTI
               ENDIF
              ENDIF
C
              ONFELT= 1-ONOF 
                     ! ONFELT= 0 : deactivation of elements
                     ! ONFELT= 1 : activation of elements
              ONOF1 = ONOF
              PRI_OFF = 0 ! full printout
              IF(ONOF==1.AND.NPBY(7,N)/=0) ONOF1 = -1 
                     ! ONOF1 = -1 nothing against rbody (rbody was already active)
                     !       = 0   ! deactivate rbody
                     !       = 1   ! activate rbody
              CALL RBYPID(
     1             IPARG    ,IPARI            ,MS      ,IN       ,
     2             IXS      ,IXQ    ,IXC      ,IXT     ,IXP      ,
     3             IXR      ,SKEW   ,ITAB     ,ITABM1  ,ISKWN    ,
     4             NPBY(1,N),ONOF1  ,ITAG     ,LPBY(K) ,
     5             X        ,V      ,VR       ,RBY(1,N),
     6             IXTG     ,NPBY   ,RBY      ,LPBY    ,0        ,
     7             FR_RBY2  ,N      ,ONFELT   ,WEIGHT  ,PARTSAV  ,
     8             IPART(K3),NPBY(2,N)        ,ELBUF_TAB,PRI_OFF)
              NPBY(7,N)=ONOF
 120      CONTINUE
      ENDDO
C------------------------------------
C     tag des noeuds secnds rby avec gravite ou load/centri
C     pour calcul du travail des forces externes
C-------------------------------------
      TAGSLV_RBY(1:NUMNOD)=0
C
      K=0
      DO N=1,NRBYKIN
        ONOF1=NPBY(7,N)
        NSL=NPBY(2,N)
        IF(ONOF1>=1)THEN
          DO I=1,NSL
            TAGSLV_RBY(LPBY(I+K))=N
          ENDDO
        ENDIF
        K=K+NSL
      ENDDO
C
      DO K=1,NGRAV
        NN =IGRV(1,K)
        IAD=IGRV(4,K)
        DO I=IAD,IAD+NN-1
          N=IABS(IBGR(I))
          IF(TAGSLV_RBY(N) /= 0)THEN
            IBGR(I) = -N
          ELSE
            IBGR(I) = N
          ENDIF
        ENDDO
      ENDDO
C
      DO K=1,NLOADC
	NN   = ICFIELD(1,K) 
        IAD  = ICFIELD(4,K)
        DO I=1,NN
         N=LCFIELD(IAD+I-1)
         IF(TAGSLV_RBY(N) /= 0)LCFIELD(IAD+I-1) = -N
        END DO
      ENDDO 
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  RBYSENS                       source/constraints/general/rbody/rbyonf.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        RBYPID                        source/constraints/general/rbody/rbypid.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE RBYSENS(IPARG,IPARI      ,MS    ,IN   ,
     2                  IXS  ,IXQ  ,IXC   ,IXT   ,IXP  ,
     3                  IXR  ,SKEW ,ITAB  ,ITABM1,ISKWN,
     4                  NPBY ,ITAG  ,LPBY ,FSKY  ,
     5                  RBY  ,X    ,V     ,VR    ,IXTG ,
     6                  IGRV ,IBGR ,SENSOR_TAB,A     ,AR   ,
     7                  FSAV,STIFN,STIFR  ,FANI  ,WEIGHT,
     8                  DMAST,DINERT,BUFSF,FR_RBY2,PARTSAV,
     9                  IPART  ,ELBUF_TAB,ICFIELD,LCFIELD,TAGSLV_RBY)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*), IPARI(*), IXS(NIXS,*), IXQ(NIXQ,*),
     .   IXC(NIXC,*), IXT(NIXT,*), IXP(NIXP,*), IXR(NIXR,*),
     .   ITAB(*), ITABM1(*),IGRV(NIGRV,*),IBGR(*),
     .   ISKWN(LISKN,*), NPBY(NNPBY,*),ITAG(*),LPBY(*), IXTG(NIXTG,*),
     .   WEIGHT(*), IPART(*), FR_RBY2(3,*), ICFIELD(SIZFIELD,*), LCFIELD(*), TAGSLV_RBY(*)
C     REAL
      my_real
     .   SKEW(LSKEW,*),MS(*),IN(*),RBY(NRBY,*),X(3,*),
     .   V(3,*),VR(3,*),FSKY(*), A(3,*) ,AR(3,*),
     .   FSAV(NTHVKI,*), STIFN(*),STIFR(*),FANI(3,*),
     .   DMAST, DINERT, BUFSF(*),PARTSAV(*)
      TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J ,ITEMP(10),K, K0, N,NSL,NN,IAD,ONOF,ONOF1,ISENS,IACTI,
     .        N2, ISU,ADRSRF,IM, IDEB, IGET,IMAXNSN, IMAXP,
     .        NSN, NSNP, PROC, ND, PP, II, NSLARB_L, P,
     .        ONFELT,K1,K2,K3,K4,K5,K6,K7,K8,K9,IFAIL,ELT_ACTIV,PRI_OFF
      my_real
     .        CRIT
C======================================================================|
C     ACIVATION/DEACTIVATION DES RIGID BODY
C     Deactivation ::
C       Elements are activated first 
C       Rbody is deactivated at the end (after 2 cycles)
C     Activation ::  
C       Rbody is activated at the same time as the elements are deactivated
C-------------------------------------------
      K1=1+LIPART1*NPART+2*9*NPART
      K2=K1+NUMELS
      K3=K2+NUMELQ
      K4=K3+NUMELC
      K5=K4+NUMELT
      K6=K5+NUMELP
      K7=K6+NUMELR
C-------------------------------------------
C     ITAG :: Main node of active rbody w/o sensor => Rbody number
C       is used for initialization of mass & inertia if the rbody is included into the rbody being activated
C       i.e. mass and inertia of the "sub-rbody" secnd nodes must not be counted twice
C-------------------------------------------
      DO I=1,NUMNOD
       ITAG(I)=0
      ENDDO
C
      DO I=1,NUMNOD
        ITAG(I+NUMNOD)=0
      ENDDO
C
      DO N=1,NRBYKIN
        ISENS = NPBY(4,N)
        IACTI = NPBY(7,N)
        IF(ISENS==0 .AND. IACTI==1 .AND. NPBY(1,N)>0)
     .    ITAG(NPBY(1,N)+NUMNOD)=N
      ENDDO
C-------------------------------------------
C     1. Looking for sensor deactivation & rbody activation 
C-------------------------------------------
      K = 1
      ONFELT=1
      ONOF1 =0
      ELT_ACTIV =0
      DO N=1,NRBYKIN
        ISENS = NPBY(4,N)
        IACTI = NPBY(7,N)
        IFAIL = NPBY(18,N)
        CRIT  = RBY(30,N)
        IF(ISENS/=0 .AND. (IFAIL/=1 .OR. CRIT < ONE))THEN
          IF (IACTI==0 .AND. TT <= SENSOR_TAB(ISENS)%TSTART) THEN
C
C           - rbody is activated and elements are deactivated at the same time
C           - unless failure criteria has been reached already
C
            IF (ISPMD==0) THEN
             WRITE(IOUT,'(/A,I9,A)')' RIGID BODY:',
     .         NPBY(6,N),' SET ON'
             WRITE(ISTDO,'(/A,I9,A)')' RIGID BODY:',
     .         NPBY(6,N),' ON'
            ENDIF
C
            ONOF  = 1   ! activate rbody
            ONFELT= 0  ! deactivation of elements
            PRI_OFF = 0 ! full printout
            CALL RBYPID(  IPARG    ,IPARI       ,MS      ,IN       ,
     2                    IXS      ,IXQ   ,IXC  ,IXT     ,IXP      ,
     3                    IXR      ,SKEW  ,ITAB ,ITABM1  ,ISKWN    ,
     4                    NPBY(1,N),ONOF  ,ITAG ,LPBY(K) ,
     5                    X        ,V     ,VR   ,RBY(1,N),
     6                    IXTG     ,NPBY  ,RBY  ,LPBY    ,1        ,
     7                    FR_RBY2  ,N     ,ONFELT,WEIGHT ,PARTSAV  ,
     8                    IPART(K3),NPBY(2,N)   ,ELBUF_TAB,PRI_OFF)
            ONOF1 = 1 ! at least 1 rbody is activated or deactivated
            NPBY(7,N)=1
          ELSEIF (IACTI>1 .AND. TT <= SENSOR_TAB(ISENS)%TSTART) THEN
C
C          - rbody is waiting for deactivation :
C              Sensor status changes again => override previous request unless failure criteria was already reached.
C
            ONOF  = -1 ! nothing against rbody (rbody was not yet deactivated)
            ONFELT= 0  ! deactivation of elements
            PRI_OFF = 0 ! full printout
            CALL RBYPID(  IPARG    ,IPARI       ,MS      ,IN       ,
     2                    IXS      ,IXQ   ,IXC  ,IXT     ,IXP      ,
     3                    IXR      ,SKEW  ,ITAB ,ITABM1  ,ISKWN    ,
     4                    NPBY(1,N),ONOF  ,ITAG ,LPBY(K) ,
     5                    X        ,V     ,VR   ,RBY(1,N),
     6                    IXTG     ,NPBY  ,RBY  ,LPBY    ,1        ,
     7                    FR_RBY2  ,N     ,ONFELT,WEIGHT ,PARTSAV  ,
     8                    IPART(K3),NPBY(2,N)   ,ELBUF_TAB,PRI_OFF)
            NPBY(7,N)=1
          ENDIF
        ENDIF
        K=K+NPBY(2,N)
      ENDDO
      IF(ONFELT==0.AND.IPARIT/=0)THEN ! reset forces of deactivated elements.
        DO I=1,8*LSKY
          FSKY(I)=0.0
        ENDDO
      ENDIF
C-------------------------------------------
C     2. Looking for sensors activation & deactivation of the rby
C        - elements will be activated yet, but rbody will be activated 2 cycles after
C-------------------------------------------
      K = 1
      DO N=1,NRBYKIN
        IACTI=NPBY(7,N)
        ISENS=NPBY(4,N)
        IFAIL = NPBY(18,N)
        CRIT  = RBY(30,N)
        IF(ISENS/=0 .AND. (IFAIL/=1 .OR. CRIT < ONE) )THEN
         IF (IACTI == 1 .AND. TT > SENSOR_TAB(ISENS)%TSTART) THEN
           IF( TT> ZERO)THEN
             IACTI=4
             NPBY(7,N)=IACTI
             IF (ISPMD==0) THEN
               WRITE(IOUT,'(/A,I9,A)')' RIGID BODY:',
     .           NPBY(6,N),' WILL BE SET OFF WITHIN 2 CYCLES'
               WRITE(ISTDO,'(/A,I9,A)')' RIGID BODY:',
     .           NPBY(6,N),' WILL BE SET OFF WITHIN 2 CYCLES'
             ENDIF
C
             ONOF  = -1 ! nothing against rbody
             ONFELT= 1  ! activation of elements
             PRI_OFF = 0 ! full printout
             CALL RBYPID(  IPARG    ,IPARI       ,MS      ,IN       ,
     2                     IXS      ,IXQ   ,IXC  ,IXT     ,IXP      ,
     3                     IXR      ,SKEW  ,ITAB ,ITABM1  ,ISKWN    ,
     4                     NPBY(1,N),ONOF  ,ITAG ,LPBY(K) ,
     5                     X        ,V     ,VR   ,RBY(1,N),
     6                     IXTG     ,NPBY  ,RBY  ,LPBY    ,1        ,
     7                     FR_RBY2  ,N     ,ONFELT,WEIGHT ,PARTSAV  ,
     8                     IPART(K3),NPBY(2,N)   ,ELBUF_TAB, PRI_OFF)
             ELT_ACTIV = 1 ! elts of at least 1 rby are activated
           ELSE ! IF(TT>0.)THEN
             IF (ISPMD==0) THEN
              WRITE(IOUT,'(/A,I9,A)')' RIGID BODY:',
     .          NPBY(6,N),' SET OFF'
              WRITE(ISTDO,'(/A,I9,A)')' RIGID BODY:',
     .          NPBY(6,N),' OFF'
             ENDIF
C
             ONOF  = 0 ! deactivate rbody
             ONFELT= 1 ! activation of elements
             PRI_OFF = 0 ! full printout
             CALL RBYPID(  IPARG    ,IPARI       ,MS      ,IN       ,
     2                     IXS      ,IXQ   ,IXC  ,IXT     ,IXP      ,
     3                     IXR      ,SKEW  ,ITAB ,ITABM1  ,ISKWN    ,
     4                     NPBY(1,N),ONOF  ,ITAG ,LPBY(K) ,
     5                     X        ,V     ,VR   ,RBY(1,N),
     6                     IXTG     ,NPBY  ,RBY  ,LPBY    ,1        ,
     7                     FR_RBY2  ,N     ,ONFELT,WEIGHT ,PARTSAV  ,
     8                     IPART(K3),NPBY(2,N)   ,ELBUF_TAB, PRI_OFF)
             NPBY(7,N)=0
             ONOF1 = 1 ! at least 1 rbody is activated or deactivated
             ELT_ACTIV = 1 ! elts of at least 1 rby are activated
           ENDIF
         ELSEIF(IACTI==2)THEN
C
C          Sensor has activated or Failure criteria has been reached ::
C          Last cycle wrt rbody deactivation <=> the rbody is deactivated (nothing wrt elements)
C
           IF (ISPMD==0) THEN
            WRITE(IOUT,'(/A,I9,A)')' RIGID BODY:',
     .        NPBY(6,N),' SET OFF'
            WRITE(ISTDO,'(/A,I9,A)')' RIGID BODY:',
     .        NPBY(6,N),' OFF'
           ENDIF
C
           ONOF  = 0  ! deactivate rbody
           ONFELT= -1 ! nothing against elements
           PRI_OFF = 0 ! full printout
           CALL RBYPID(  IPARG    ,IPARI       ,MS      ,IN       ,
     2                   IXS      ,IXQ   ,IXC  ,IXT     ,IXP      ,
     3                   IXR      ,SKEW  ,ITAB ,ITABM1  ,ISKWN    ,
     4                   NPBY(1,N),ONOF  ,ITAG ,LPBY(K) ,
     5                   X        ,V     ,VR   ,RBY(1,N),
     6                   IXTG     ,NPBY  ,RBY  ,LPBY    ,1        ,
     7                   FR_RBY2  ,N     ,ONFELT,WEIGHT ,PARTSAV  ,
     8                   IPART(K3),NPBY(2,N)   ,ELBUF_TAB, PRI_OFF)
           NPBY(7,N)=0
           ONOF1 = 1 ! at least 1 rbody is activated or deactivated
         ENDIF
        ENDIF
        K=K+NPBY(2,N)
      ENDDO
C-------------------------------------------
C     3. Looking for failure criteria
C        - failure criteria will deactivate the rby
C        - elements will be activated yet, but rbody will be activated 2 cycles after
C-------------------------------------------
      K = 1
      DO N=1,NRBYKIN
        IACTI=NPBY(7,N)
        ISENS=NPBY(4,N)
        IFAIL = NPBY(18,N)
        CRIT  = RBY(30,N)
        IF(IACTI >= 1.AND.IFAIL == 1.AND.CRIT >= ONE)THEN ! If rbody is active
         IF(IACTI==1)THEN ! and failure is detected 
           IF(TT>0.)THEN
             IACTI=4
             NPBY(7,N)=IACTI
             IF (ISPMD==0) THEN
               WRITE(IOUT,'(/A,I9,A)')' RIGID BODY FAILURE : RIGID BODY:',
     .           NPBY(6,N),' WILL BE SET OFF WITHIN 2 CYCLES'
               WRITE(ISTDO,'(/A,I9,A)')' RIGID BODY FAILURE : RIGID BODY:',
     .           NPBY(6,N),' WILL BE SET OFF WITHIN 2 CYCLES'
             ENDIF
C
             ONOF  = -1 ! nothing against rbody
             ONFELT= 1  ! activation of elements
             PRI_OFF = 0 ! full printout
             CALL RBYPID(  IPARG    ,IPARI       ,MS      ,IN       ,
     2                     IXS      ,IXQ   ,IXC  ,IXT     ,IXP      ,
     3                     IXR      ,SKEW  ,ITAB ,ITABM1  ,ISKWN    ,
     4                     NPBY(1,N),ONOF  ,ITAG ,LPBY(K) ,
     5                     X        ,V     ,VR   ,RBY(1,N),
     6                     IXTG     ,NPBY  ,RBY  ,LPBY    ,1        ,
     7                     FR_RBY2  ,N     ,ONFELT,WEIGHT ,PARTSAV  ,
     8                     IPART(K3),NPBY(2,N)   ,ELBUF_TAB, PRI_OFF)
             ELT_ACTIV = 1 ! elts of at least 1 rby are activated
           ELSE ! IF(TT>0.)THEN (Failure most probably does not occur at time zero)
             IF (ISPMD==0) THEN
              WRITE(IOUT,'(/A,I9,A)')' RIGID BODY FAILURE : RIGID BODY:',
     .          NPBY(6,N),' SET OFF'
              WRITE(ISTDO,'(/A,I9,A)')' RIGID BODY FAILURE : RIGID BODY:',
     .          NPBY(6,N),' OFF'
             ENDIF
C
             ONOF  = 0 ! deactivate rbody
             ONFELT= 1 ! activation of elements
             PRI_OFF = 0 ! full printout
             CALL RBYPID(  IPARG    ,IPARI       ,MS      ,IN       ,
     2                     IXS      ,IXQ   ,IXC  ,IXT     ,IXP      ,
     3                     IXR      ,SKEW  ,ITAB ,ITABM1  ,ISKWN    ,
     4                     NPBY(1,N),ONOF  ,ITAG ,LPBY(K) ,
     5                     X        ,V     ,VR   ,RBY(1,N),
     6                     IXTG     ,NPBY  ,RBY  ,LPBY    ,1        ,
     7                     FR_RBY2  ,N     ,ONFELT,WEIGHT ,PARTSAV  ,
     8                     IPART(K3),NPBY(2,N)   ,ELBUF_TAB, PRI_OFF)
             NPBY(7,N)=0
             ONOF1 = 1 ! at least 1 rbody is activated or deactivated   
           ENDIF
         ELSEIF(IACTI==2)THEN
C
C          Sensor has activated or Failure criteria has been reached ::
C          Last cycle wrt rbody deactivation <=> the rbody is deactivated (nothing wrt elements)
C
           IF (ISPMD==0) THEN
            WRITE(IOUT,'(/A,I9,A)')' RIGID BODY FAILURE : RIGID BODY:',
     .        NPBY(6,N),' SET OFF'
            WRITE(ISTDO,'(/A,I9,A)')' RIGID BODY FAILURE : RIGID BODY:',
     .        NPBY(6,N),' OFF'
           ENDIF
C
           ONOF  = 0  ! deactivate rbody
           ONFELT= -1 ! nothing against elements
           PRI_OFF = 0 ! full printout
           CALL RBYPID(  IPARG    ,IPARI       ,MS      ,IN       ,
     2                   IXS      ,IXQ   ,IXC  ,IXT     ,IXP      ,
     3                   IXR      ,SKEW  ,ITAB ,ITABM1  ,ISKWN    ,
     4                   NPBY(1,N),ONOF  ,ITAG ,LPBY(K) ,
     5                   X        ,V     ,VR   ,RBY(1,N),
     6                   IXTG     ,NPBY  ,RBY  ,LPBY    ,1        ,
     7                   FR_RBY2  ,N     ,ONFELT,WEIGHT ,PARTSAV  ,
     8                   IPART(K3),NPBY(2,N)   ,ELBUF_TAB, PRI_OFF)
           NPBY(7,N)=0
           ONOF1 = 1 ! at least 1 rbody is activated or deactivated
         ENDIF
        ENDIF
        K=K+NPBY(2,N)
      ENDDO
C-------------------------------------------
C     4. Loop over other rby in case of elts activation
C        - in case of hierachy of rby elt is activated only
C        - if all rbys are deactivated
C-------------------------------------------
      IF(ELT_ACTIV == 1)THEN
        K = 1
        DO N=1,NRBYKIN
          IACTI=NPBY(7,N)
          IF(IACTI.EQ.1)THEN
            ONOF  = -1 ! nothing against rbody
            ONFELT= 0  ! deactivation of elements
            PRI_OFF = 1 ! printout for changed elements only
            CALL RBYPID(  IPARG    ,IPARI       ,MS      ,IN       ,
     2                    IXS      ,IXQ   ,IXC  ,IXT     ,IXP      ,
     3                    IXR      ,SKEW  ,ITAB ,ITABM1  ,ISKWN    ,
     4                    NPBY(1,N),ONOF  ,ITAG ,LPBY(K) ,
     5                    X        ,V     ,VR   ,RBY(1,N),
     6                    IXTG     ,NPBY  ,RBY  ,LPBY    ,1        ,
     7                    FR_RBY2  ,N     ,ONFELT,WEIGHT ,PARTSAV  ,
     8                    IPART(K3),NPBY(2,N)   ,ELBUF_TAB,PRI_OFF)
          ENDIF
          K=K+NPBY(2,N)
        ENDDO
      ENDIF
C-------------------------------------------
      DO N=1,NRBYKIN
        IACTI=NPBY(7,N)
        IF(IACTI>1)THEN
         IACTI=IACTI-1
        ENDIF
        NPBY(7,N)=IACTI
      ENDDO
C-------------------------------------
C     tag des noeuds secnds rby avec gravite ou load/centri
C     pour calcul du travail des forces externes
C-------------------------------------
      IF(ONOF1==0) GOTO 200
C
      TAGSLV_RBY(1:NUMNOD)=0
C
      K=0
      DO N=1,NRBYKIN
       ONOF1=NPBY(7,N)
       NSL=NPBY(2,N)
       IF(ONOF1>=1)THEN
         DO I=1,NSL
           TAGSLV_RBY(LPBY(I+K))=N
         ENDDO
       ENDIF
       K=K+NSL
      ENDDO
C
      DO K=1,NGRAV
       NN =IGRV(1,K)
       IAD=IGRV(4,K)
       DO I=IAD,IAD+NN-1
         N=IABS(IBGR(I))
         IF(TAGSLV_RBY(N) /= 0)THEN
           IBGR(I) = -N
         ELSE
           IBGR(I) = N
         ENDIF
       ENDDO
      ENDDO
C
      DO K=1,NLOADC
	NN   = ICFIELD(1,K) 
        IAD  = ICFIELD(4,K)
        DO I=1,NN
         N=LCFIELD(IAD+I-1)
         IF(TAGSLV_RBY(N) /= 0)LCFIELD(IAD+I-1) = -N
        END DO
      ENDDO 
C
 200  CONTINUE
      RETURN
C
      END
