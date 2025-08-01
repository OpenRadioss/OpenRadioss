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
!||    rbyfor                       ../engine/source/constraints/general/rbody/rbyfor.F
!||--- called by ------------------------------------------------------
!||    resol                        ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    damping_vref_rby             ../engine/source/assembly/damping_vref_rby.F90
!||    damping_vref_rby_stiff       ../engine/source/assembly/damping_vref_rby_stiff.F90
!||    rgbodfp                      ../engine/source/constraints/general/rbody/rgbodfp.F
!||    spmd_exch_a_rb6              ../engine/source/mpi/kinematic_conditions/spmd_exch_a_rb6.F
!||    spmd_exch_a_rb6_vrel         ../engine/source/mpi/kinematic_conditions/spmd_exch_a_rb6.F
!||    spmd_exch_a_rb6g             ../engine/source/mpi/kinematic_conditions/spmd_exch_a_rb6g.F
!||    startime                     ../engine/source/system/timer_mod.F90
!||    stoptime                     ../engine/source/system/timer_mod.F90
!||--- uses       -----------------------------------------------------
!||    damping_vref_rby_mod         ../engine/source/assembly/damping_vref_rby.F90
!||    damping_vref_rby_stiff_mod   ../engine/source/assembly/damping_vref_rby_stiff.F90
!||    groupdef_mod                 ../common_source/modules/groupdef_mod.F
!||    timer_mod                    ../engine/source/system/timer_mod.F90
!||====================================================================
      SUBROUTINE RBYFOR(TIMERS,
     1   RBY      ,A      ,AR     ,X      ,VR      ,
     2   FSAV     ,IN     ,STIFN  ,STIFR  ,FANI    ,
     3   LPBY     ,NPBY   ,WEIGHT ,MS     ,V       ,
     4   IGRSURF  ,BUFSF  ,ICODR  ,ISKEW  ,SKEW    ,
     5   KIND     ,IAD_RBY,FR_RBY6,RBY6   ,IRBKIN_L,
     6   NRBYKIN_L,NATIV_SMS,DIMFB,FBSAV6 ,STABSEN ,
     7   TABSENSOR,NODREAC,FTHREAC,CPTREAC,DAMPR   ,
     8   SDAMP    ,DAMP   ,NDAMP_VREL,ID_DAMP_VREL,IGRNOD,
     9   TAGSLV_RBY,IPARIT,WFEXT,NDAMP_VREL_RBYG,SIZE_RBY6_C,
     A   RBY6_C)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TIMER_MOD
      USE GROUPDEF_MOD
      USE damping_vref_rby_mod
      USE damping_vref_rby_stiff_mod
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "timeri_c.inc"
#include      "intstamp_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(TIMER_), INTENT(inout) :: TIMERS
      INTEGER ICODR(*),ISKEW(*),WEIGHT(*),LPBY(*),NPBY(NNPBY,*),
     .        KIND(NRBYKIN),FR_RBY6(*),IAD_RBY(*),IRBKIN_L(*),
     .        NRBYKIN_L, NATIV_SMS(*),DIMFB,STABSEN,TABSENSOR(*),
     .        NODREAC(*),CPTREAC,NDAMP_VREL_RBYG,SIZE_RBY6_C
      my_real RBY(NRBY,*) ,A(3,*) ,AR(3,*) ,X(3,*) ,VR(3,*),FSAV(NTHVKI,*),
     .        IN(*) ,STIFN(*),STIFR(*),FANI(3,*),MS(*),V(3,*) ,
     .        BUFSF(*), SKEW(LSKEW,*),FTHREAC(6,*)
      DOUBLE PRECISION RBY6(8,6,NRBYKIN),RBY6_C(2,6,SIZE_RBY6_C)
      DOUBLE PRECISION FBSAV6(12,6,DIMFB),RBID(12,6),WFEXT
      TYPE (SURF_), DIMENSION(NSURF) :: IGRSURF
      INTEGER ,INTENT(IN) :: NDAMP_VREL,IPARIT,SDAMP
      INTEGER ,INTENT(IN) :: ID_DAMP_VREL(NDAMP_VREL),TAGSLV_RBY(NUMNOD)
      my_real, INTENT(INOUT) :: DAMPR(NRDAMP,NDAMP),DAMP(SDAMP)
      TYPE(GROUP_), INTENT(IN) :: IGRNOD(NGRNOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  J,K,N,KK,N2,ILAGM,
     .         ISU,ADRSRF,IM, L, LSEND, LRECV, NRBDIM,IPARSENS,ISECT,
     .         DIM    
C-------------------------------------
C INIT RBY6 to ZERO for deactivated Rigid Bodies (non multi-thread)
C-------------------------------------
!$OMP SINGLE
      RBID = ZERO
      DO KK=1,NRBYKIN_L
        N=IRBKIN_L(KK)
        IF(NPBY(7,N)<=0) THEN
          DO K = 1, 6
            RBY6(1,K,N) = ZERO
            RBY6(2,K,N) = ZERO
            RBY6(3,K,N) = ZERO
            RBY6(4,K,N) = ZERO
            RBY6(5,K,N) = ZERO
            RBY6(6,K,N) = ZERO
            RBY6(7,K,N) = ZERO
            RBY6(8,K,N) = ZERO 
          ENDDO
        ENDIF
      ENDDO
!$OMP END SINGLE 
C-------------------------------------
C CALCUL FORCE SUPER RIGID BODIES (non multi-thread)
C-------------------------------------

!$OMP SINGLE

      DO KK=1,NRBYKIN_L
        N=IRBKIN_L(KK)
        K  = KIND(N)
        IF (NPBY(4,N)/=0) THEN
          IF(NPBY(7,N)>0) THEN
           N2 = NINTER+NRWALL+N
C          Cumul des Forces / Moments sur la surface :
C          A chaque instant (et a TT=0. dans le Starter),
C          le Rigid Body transmet la position du noeud main
C          a la surface
C          ET
C          Dans l'Interface TYPE 14, Les forces, moments (et rigidites)
C          sont calcules en ce point !!!
           ISU=NPBY(8,N)
           IF(ISPMD==0.AND.ISU/=0) THEN
            IM=NPBY(1,N)
            ADRSRF=IGRSURF(ISU)%IAD_BUFR
            A(1,IM) =A(1,IM) +BUFSF(ADRSRF+25)
            A(2,IM) =A(2,IM) +BUFSF(ADRSRF+26)
            A(3,IM) =A(3,IM) +BUFSF(ADRSRF+27)
            AR(1,IM)=AR(1,IM)+BUFSF(ADRSRF+28)
            AR(2,IM)=AR(2,IM)+BUFSF(ADRSRF+29)
            AR(3,IM)=AR(3,IM)+BUFSF(ADRSRF+30)
            STIFN(IM)=STIFN(IM)+BUFSF(ADRSRF+31)
            STIFR(IM)=STIFR(IM)+BUFSF(ADRSRF+32)
C           Sortie sur TH des forces d'interface.
            FSAV(10,N2)=FSAV(10,N2)+BUFSF(ADRSRF+25)*DT1
            FSAV(11,N2)=FSAV(11,N2)+BUFSF(ADRSRF+26)*DT1
            FSAV(12,N2)=FSAV(12,N2)+BUFSF(ADRSRF+27)*DT1
            FSAV(13,N2)=FSAV(13,N2)+BUFSF(ADRSRF+28)*DT1
            FSAV(14,N2)=FSAV(14,N2)+BUFSF(ADRSRF+29)*DT1
            FSAV(15,N2)=FSAV(15,N2)+BUFSF(ADRSRF+30)*DT1
           END IF
c
           IPARSENS=0
           ISECT=0
           IF(STABSEN/=0) ISECT=TABSENSOR(N+NSECT+NINTSUB+NINTER+NRWALL+1)-
     .           TABSENSOR(N+NSECT+NINTSUB+NINTER+NRWALL)
           IF(ISECT/=0) THEN
             IPARSENS=1
             CALL RGBODFP(
     1         A        ,AR       ,X            ,FSAV(1,N2),RBY(1,N),
     2         LPBY(K)  ,NPBY(1,N),IN           ,VR        ,STIFN   ,
     3         STIFR    ,FANI(1,1+2*(NSECT+N-1)),WEIGHT,MS ,V       ,
     4         1        ,ICODR    ,ISKEW        ,SKEW   ,RBY6(1,1,N),
     5         NPBY(2,N),NATIV_SMS,FBSAV6(1,1,ISECT) ,IPARSENS,
     6         NODREAC,FTHREAC,CPTREAC,NPBY(5,N))
           ELSE
             CALL RGBODFP(
     1         A        ,AR       ,X            ,FSAV(1,N2),RBY(1,N),
     2         LPBY(K)  ,NPBY(1,N),IN           ,VR        ,STIFN   ,
     3         STIFR    ,FANI(1,1+2*(NSECT+N-1)),WEIGHT,MS ,V       ,
     4         1        ,ICODR    ,ISKEW        ,SKEW   ,RBY6(1,1,N),
     5         NPBY(2,N),NATIV_SMS,RBID         , IPARSENS,
     6         NODREAC,FTHREAC,CPTREAC,NPBY(5,N))
           ENDIF
          ENDIF
        ENDIF
      ENDDO

!$OMP END SINGLE

C-------------------------------------
C CALCUL FORCE RIGID BODIES CLASSIQUES (multi-thread)
C-------------------------------------

C-------------------------------------
C Traitement rigid bodies (sans packet), avec scheduling dynamique
C-------------------------------------

!$OMP DO SCHEDULE(DYNAMIC,1)

      DO KK=1,NRBYKIN_L
        N = IRBKIN_L(KK)
        K  = KIND(N)
        IF (NPBY(4,N)==0) THEN
          IF(NPBY(7,N)>0) THEN
           N2 = NINTER+NRWALL+N
C          Cumul des Forces / Moments sur la surface :
C          A chaque instant (et a TT=0. dans le Starter),
C          le Rigid Body transmet la position du noeud main
C          a la surface
C          ET
C          Dans l'Interface TYPE 14, Les forces, moments (et rigidites)
C          sont calcules en ce point !!!
           ISU=NPBY(8,N)
           IF(ISPMD==0.AND.ISU/=0) THEN
            IM=NPBY(1,N)
            ADRSRF=IGRSURF(ISU)%IAD_BUFR
            A(1,IM) =A(1,IM) +BUFSF(ADRSRF+25)
            A(2,IM) =A(2,IM) +BUFSF(ADRSRF+26)
            A(3,IM) =A(3,IM) +BUFSF(ADRSRF+27)
            AR(1,IM)=AR(1,IM)+BUFSF(ADRSRF+28)
            AR(2,IM)=AR(2,IM)+BUFSF(ADRSRF+29)
            AR(3,IM)=AR(3,IM)+BUFSF(ADRSRF+30)
            STIFN(IM)=STIFN(IM)+BUFSF(ADRSRF+31)
            STIFR(IM)=STIFR(IM)+BUFSF(ADRSRF+32)
C           Sortie sur TH des forces d'interface.
            FSAV(10,N2)=FSAV(10,N2)+BUFSF(ADRSRF+25)*DT1
            FSAV(11,N2)=FSAV(11,N2)+BUFSF(ADRSRF+26)*DT1
            FSAV(12,N2)=FSAV(12,N2)+BUFSF(ADRSRF+27)*DT1
            FSAV(13,N2)=FSAV(13,N2)+BUFSF(ADRSRF+28)*DT1
            FSAV(14,N2)=FSAV(14,N2)+BUFSF(ADRSRF+29)*DT1
            FSAV(15,N2)=FSAV(15,N2)+BUFSF(ADRSRF+30)*DT1
           END IF
c
           IPARSENS=0
           ISECT=0
           IF(STABSEN/=0) ISECT=TABSENSOR(N+NSECT+NINTSUB+NINTER+NRWALL+1)-
     .           TABSENSOR(N+NSECT+NINTSUB+NINTER+NRWALL)
           IF(ISECT/=0) THEN
             IPARSENS=1
             CALL RGBODFP(
     1         A        ,AR         ,X          ,FSAV(1,N2),RBY(1,N),
     2         LPBY(K)  ,NPBY(1,N)  ,IN         ,VR        ,STIFN   ,
     3         STIFR    ,FANI(1,1+2*(NSECT+N-1)),WEIGHT,MS ,V       ,
     4         1        ,ICODR      ,ISKEW      ,SKEW   ,RBY6(1,1,N),
     5         NPBY(2,N),NATIV_SMS,FBSAV6(1,1,ISECT) ,IPARSENS,
     6         NODREAC,FTHREAC,CPTREAC,NPBY(5,N))
           ELSE
             CALL RGBODFP(
     1         A        ,AR         ,X          ,FSAV(1,N2),RBY(1,N),
     2         LPBY(K)  ,NPBY(1,N)  ,IN         ,VR        ,STIFN   ,
     3         STIFR    ,FANI(1,1+2*(NSECT+N-1)),WEIGHT,MS ,V       ,
     4         1        ,ICODR      ,ISKEW      ,SKEW   ,RBY6(1,1,N),
     5         NPBY(2,N),NATIV_SMS  ,RBID       ,IPARSENS   ,
     6         NODREAC,FTHREAC,CPTREAC,NPBY(5,N))
           ENDIF
          ENDIF
        ENDIF
      ENDDO

!$OMP END DO
   
C-------------------------------------
C Contribution of damping forces from /DAMP/VREL
C-------------------------------------
      IF ((NDAMP_VREL_RBYG > 0).AND.(NRBYKIN_L > 0)) THEN
!$OMP SINGLE
        DIM = 3+3*IRODDL    
        CALL damping_vref_rby(IGRNOD    ,NGRNOD   ,V        ,VR        ,A         ,
     .                        X         ,MS       ,DAMPR    ,NRDAMP    ,NDAMP     ,
     .                        NDAMP_VREL,IPARIT   ,NUMNOD  ,DT1       ,ID_DAMP_VREL,
     .                        TT        ,NNPBY    ,NRBYKIN  ,NPBY      ,RBY6      ,
     .                        RBY6_C    ,TAGSLV_RBY,WEIGHT  ,LSKEW     ,NUMSKW    ,
     .                        DIM       ,DAMP     ,SKEW     ,WFEXT     ,SIZE_RBY6_C)
!$OMP END SINGLE
      ENDIF 

      IF (NSPMD > 1) THEN

!$OMP SINGLE

        IF (IMON>0) CALL STARTIME(TIMERS,11)
C
C Comm non multi-thread
C
C flag to enable/disable all to all communication for RBY.
          IF(NINTSTAMP == 0) THEN
            IF (NDAMP_VREL_RBYG == 0) THEN
C routine A2A for Parith/ON computation of A AR STIFN STIFR on main nodes of RB
              NRBDIM=8
              CALL SPMD_EXCH_A_RB6(NRBDIM,IAD_RBY,FR_RBY6,IAD_RBY(NSPMD+1),RBY6)
            ELSE
C routine A2A for Parith/ON computation of A AR STIFN STIFR C CR on main nodes of RB for /DAMP/VREL
              NRBDIM=10
              CALL SPMD_EXCH_A_RB6_VREL(NRBDIM,IAD_RBY,FR_RBY6,IAD_RBY(NSPMD+1),RBY6,
     .                                  RBY6_C,SIZE_RBY6_C)
            ENDIF      
C routine G/S de calcul Parith/ON de A AR STIFN STIFR noeuds main RB
          ELSE
            CALL SPMD_EXCH_A_RB6G(NPBY,RBY6)
          END IF

        IF (IMON>0) CALL STOPTIME(TIMERS,11)

!$OMP END SINGLE

      END IF
C
C Traitement fin parith/on multi-thread
C
!$OMP DO SCHEDULE(DYNAMIC,1)

      DO KK=1,NRBYKIN_L
        N = IRBKIN_L(KK)
        K  = KIND(N)
        N2 = NINTER+NRWALL+N
        IF(NPBY(7,N)>0) THEN
c
           IPARSENS=0
           ISECT=0
           IF(STABSEN/=0) ISECT=TABSENSOR(N+NSECT+NINTSUB+NINTER+NRWALL+1)-
     .           TABSENSOR(N+NSECT+NINTSUB+NINTER+NRWALL)
           IF(ISECT/=0) THEN
             IPARSENS=1
             CALL RGBODFP(
     1         A        ,AR       ,X            ,FSAV(1,N2),RBY(1,N),
     2         LPBY(K)  ,NPBY(1,N),IN           ,VR        ,STIFN   ,
     3         STIFR    ,FANI(1,1+2*(NSECT+N-1)),WEIGHT,MS ,V       ,
     4         2        ,ICODR    ,ISKEW        ,SKEW,   RBY6(1,1,N),
     5         NPBY(2,N),NATIV_SMS,FBSAV6(1,1,ISECT) ,IPARSENS,
     6         NODREAC,FTHREAC,CPTREAC,NPBY(5,N))
           ELSE
             CALL RGBODFP(
     1         A        ,AR       ,X            ,FSAV(1,N2),RBY(1,N),
     2         LPBY(K)  ,NPBY(1,N),IN           ,VR        ,STIFN   ,
     3         STIFR    ,FANI(1,1+2*(NSECT+N-1)),WEIGHT,MS ,V       ,
     4         2        ,ICODR    ,ISKEW        ,SKEW,   RBY6(1,1,N),
     5         NPBY(2,N),NATIV_SMS,RBID         ,IPARSENS,
     6         NODREAC,FTHREAC,CPTREAC,NPBY(5,N))
           ENDIF
        ENDIF
      ENDDO

!$OMP END DO
C      
C-------------------------------------
C Computation of stability of main node for DAMP/VREL
C-------------------------------------
      IF (NDAMP_VREL_RBYG > 0) THEN
!$OMP SINGLE               
        call damping_vref_rby_stiff(NUMNOD,NNPBY,NRBYKIN,NRBYKIN_L,NPBY,
     .                              RBY6_C,MS,IN,STIFN,STIFR,SIZE_RBY6_C,  
     .                              IRBKIN_L )
!$OMP END SINGLE
      ENDIF 
C
      RETURN
      END
