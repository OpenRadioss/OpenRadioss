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
Chd|  SPSTABW                       source/elements/sph/spstab.F  
Chd|-- called by -----------
Chd|        FORINTP                       source/elements/forintp.F     
Chd|-- calls ---------------
Chd|        WEIGHT0                       source/elements/sph/weight.F  
Chd|        GET_U_GEO                     source/user_interface/upidmid.F
Chd|        SPHBOX                        share/modules/sphbox.F        
Chd|====================================================================
      SUBROUTINE SPSTABW(
     1    ITASK     ,IPARG     ,NGROUNC   ,IGROUNC   ,KXSP      ,
     2    ISPCOND   ,ISPSYM    ,WASPACT   ,SPH2SOL   ,WA        ,
     3    WASIGSM   ,WAR       ,STAB      ,IXSP      ,NOD2SP    ,
     4    SPBUF     ,X         ,IPART     ,IPARTSP   ,XSPSYM    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SPHBOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "sphcom.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITASK, IPARG(NPARG,*), NGROUNC, IGROUNC(*), KXSP(NISP,*),
     .        ISPCOND(NISPCOND,*),  ISPSYM(NSPCOND,*), WASPACT(*), 
     .        SPH2SOL(*), IXSP(KVOISPH,*), NOD2SP(*), IPART(LIPART1,*),
     .        IPARTSP(*)
      my_real
     .   WA(KWASPH,*), WASIGSM(6,*), WAR(10,*), STAB(7,*), 
     .   SPBUF(NSPBUF,*), X(3,*), XSPSYM(3,*)
C-----------------------------------------------
      my_real
     .         GET_U_GEO
      EXTERNAL GET_U_GEO
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, NS, INOD, JNOD, M, NVOIS, NN, 
     .        NSPACTF, NSPACTL, IPRT, IGEO, ID, IS,
     .        NVOISS, JS, SM, NC, IACT
      my_real
     .       XI, YI, ZI, XJ, YJ, ZJ, DI, DMIN, DD, WGHT,
     .       XS, YS, ZS, ZSTAB,NUL,UNO
C-----------------------------------------------
      NUL=ZERO
      UNO=ONE
      NSPACTF=1+ITASK*NSPHACT/NTHREAD
      NSPACTL=(ITASK+1)*NSPHACT/NTHREAD
      DO NS=NSPACTF,NSPACTL
        N=WASPACT(NS)
        INOD =KXSP(3,N)
        XI=X(1,INOD)
        YI=X(2,INOD)
        ZI=X(3,INOD)
        DI    =SPBUF(1,N)
        NVOIS =KXSP(4,N)
        IPRT  =IPARTSP(N)
        IGEO  =IPART(2,IPRT)
        ZSTAB =GET_U_GEO(7,IGEO)
C
C indique si il faut calculer SI =-MAX(ZERO,SI)
C         <=> ZSTAB/=0 et il existe voisin a prendre en compte dans spforcp.F
        STAB(7,N)=ZERO
        IF(ZSTAB/=ZERO.AND.NVOIS/=0)THEN
          IACT=0
          DO J=1,NVOIS
           JNOD=IXSP(J,N)
           IF(JNOD>0)THEN
            M=NOD2SP(JNOD)
C
C Solids to SPH, no interaction if both particles are inactive
            IF(KXSP(2,N)>0.OR.KXSP(2,M)>0)THEN
              IACT=1
              EXIT
            END IF
           ELSE                           ! cellule remote
            NN = -JNOD
C
C Solidds to SPH, no interaction if both particles are inactive
            IF(KXSP(2,N)>0.OR.XSPHR(13,NN)>0)THEN
              IACT=1
              EXIT
            END IF
           END IF
          END DO
          IF(IACT==1) THEN
C-- W(DP)-- DP is the distance of the PPV in initial configuration
C-- DD = DP/H
            IF (SPBUF(13,N)>EM30) THEN
              DD = SPBUF(13,N)
            ELSE
              DD=TWO/THREE
            ENDIF
            CALL WEIGHT0(NUL,NUL,NUL,DD,NUL,NUL,UNO,WGHT)
            STAB(7,N)=ZSTAB/MAX(EM30,WGHT*WGHT*WGHT*WGHT)
          ENDIF
        END IF
      END DO
C----------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  SPSTABS                       source/elements/sph/spstab.F  
Chd|-- called by -----------
Chd|        FORINTP                       source/elements/forintp.F     
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPH_VALPVEC                   source/elements/sph/spstab.F  
Chd|        STARTIMEG                     source/system/timer.F         
Chd|        STOPTIMEG                     source/system/timer.F         
Chd|        SPHBOX                        share/modules/sphbox.F        
Chd|====================================================================
      SUBROUTINE SPSTABS(
     1    ITASK     ,IPARG     ,NGROUNC   ,IGROUNC   ,KXSP      ,
     2    ISPCOND   ,ISPSYM    ,WASPACT   ,SPH2SOL   ,WA        ,
     3    WASIGSM   ,WAR       ,STAB      ,IXSP      ,NOD2SP    ,
     4    SPBUF     ,X         )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SPHBOX
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
#include      "vect01_c.inc"
#include      "sphcom.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITASK, IPARG(NPARG,*), NGROUNC, IGROUNC(*), KXSP(NISP,*),
     .        ISPCOND(NISPCOND,*),  ISPSYM(NSPCOND,*), WASPACT(*), 
     .        SPH2SOL(*), IXSP(KVOISPH,*), NOD2SP(*)
C     REAL
      my_real
     .   WA(KWASPH,*), WASIGSM(6,*), WAR(10,*), STAB(7,*), 
     .   SPBUF(NSPBUF,*), X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, NINDX, INDEX(MVSIZ), KS, JS, SM, SS, KR, NR,
     .        IG, NELEM, NG, OFFSET, NEL,
     .        NC, IC, IS, ISLIDE, NSPHRFT, NSPHRLT, L, SIZE,
     .        NS, INOD, JNOD, M, NVOIS, 
     .        NSPACTF, NSPACTL
      my_real
     .       SIGPRV(3,MVSIZ), DIRPRV(3,3,MVSIZ),
     .       R1, R2, R3, XI, YI, ZI, XJ, YJ, ZJ, DI, DMIN, DD, WGHT,
     .       XS, YS, ZS, SIG(6,MVSIZ)
C-----------------------------------------------
C Boucle parallele dynamique SMP
C
!$OMP DO SCHEDULE(DYNAMIC,1)
c------------------------
      DO IG = 1, NGROUNC
        NG = IGROUNC(IG)         
C--------
C Solids to SPH, particles must be computed when cloud active
        IF(IPARG(8,NG)==1)GOTO 350
        IF (IDDW>0) CALL STARTIMEG(NG)
        DO NELEM = 1,IPARG(2,NG),NVSIZ
          OFFSET = NELEM - 1
          NEL   =IPARG(2,NG)
          NFT   =IPARG(3,NG) + OFFSET
          IAD   =IPARG(4,NG)
          ITY   =IPARG(5,NG)
          ISPH2SOL=IPARG(69,NG)
          IPARTSPH=0
          LFT=1
          LLT=MIN(NVSIZ,NEL-NELEM+1)
          IF(ITY==51) THEN
C-----------
            NINDX=0
            DO I=LFT,LLT
              N    =NFT+I
              IF(STAB(7,N)/=ZERO)THEN
                NINDX=NINDX+1
                INDEX(NINDX)=I
              END IF
            END DO
C----------------------------------
C           Eigenvalues needed to be calculated in double precision
C           for a simple precision executing
            SIZE=KWASPH
c           IF (IRESP==1) THEN
c             CALL VALPVECDP(AV,SIGPRV,DIRPRV,NEL)
c           ELSE
            CALL SPH_VALPVEC(NINDX,INDEX,SIZE,WA(1,NFT+1),SIGPRV,DIRPRV)
c           ENDIF
            DO J=1,NINDX
              I=INDEX(J)
              N    =NFT+I
              R1=-MAX(ZERO,SIGPRV(1,I))
              R2=-MAX(ZERO,SIGPRV(2,I))
              R3=-MAX(ZERO,SIGPRV(3,I))
              STAB(1,N) = DIRPRV(1,1,I)*DIRPRV(1,1,I)*R1
     &                  + DIRPRV(1,2,I)*DIRPRV(1,2,I)*R2
     &                  + DIRPRV(1,3,I)*DIRPRV(1,3,I)*R3
              STAB(2,N) = DIRPRV(2,2,I)*DIRPRV(2,2,I)*R2
     &                  + DIRPRV(2,3,I)*DIRPRV(2,3,I)*R3
     &                  + DIRPRV(2,1,I)*DIRPRV(2,1,I)*R1
              STAB(3,N) = DIRPRV(3,3,I)*DIRPRV(3,3,I)*R3
     &                  + DIRPRV(3,1,I)*DIRPRV(3,1,I)*R1
     &                  + DIRPRV(3,2,I)*DIRPRV(3,2,I)*R2
              STAB(4,N) = DIRPRV(1,1,I)*DIRPRV(2,1,I)*R1
     &                  + DIRPRV(1,2,I)*DIRPRV(2,2,I)*R2
     &                  + DIRPRV(1,3,I)*DIRPRV(2,3,I)*R3
              STAB(5,N) = DIRPRV(2,2,I)*DIRPRV(3,2,I)*R2
     &                  + DIRPRV(2,3,I)*DIRPRV(3,3,I)*R3
     &                  + DIRPRV(2,1,I)*DIRPRV(3,1,I)*R1
              STAB(6,N) = DIRPRV(3,3,I)*DIRPRV(1,3,I)*R3
     &                  + DIRPRV(3,1,I)*DIRPRV(1,1,I)*R1
     &                  + DIRPRV(3,2,I)*DIRPRV(1,2,I)*R2
c          STAB(1,N)=-MAX(ZERO,THIRD*(WA(1,N)+WA(2,N)+WA(3,N)))
c          STAB(2,N)=-MAX(ZERO,THIRD*(WA(1,N)+WA(2,N)+WA(3,N)))
c          STAB(3,N)=-MAX(ZERO,THIRD*(WA(1,N)+WA(2,N)+WA(3,N)))
c          STAB(4,N)=ZERO
c          STAB(5,N)=ZERO
c          STAB(6,N)=ZERO
            END DO
          ENDIF
          IF (IDDW>0) CALL STOPTIMEG(NG)
        END DO
C--------
 350    CONTINUE
      END DO
!$OMP END DO
C----------------------------------
      NSPHRFT=1+ITASK*NSPHR/NTHREAD
      NSPHRLT=(ITASK+1)*NSPHR/NTHREAD
      DO N=NSPHRFT,NSPHRLT,MVSIZ
        NINDX=0
        DO L=1,MIN(NSPHRLT-N+1,MVSIZ)
          I=N+L-1
          IF(STAB(7,NUMSPH+I)/=ZERO)THEN
            NINDX=NINDX+1
            INDEX(NINDX)=L
          END IF
        ENDDO
C----------------------------------
C       Eigenvalues needed to be calculated in double precision
C       for a simple precision executing
        SIZE=10
c       IF (IRESP==1) THEN
c         CALL VALPVECDP(AV,SIGPRV,DIRPRV,NEL)
c       ELSE
        CALL SPH_VALPVEC(NINDX,INDEX,SIZE,WAR(1,N),SIGPRV,DIRPRV)
c       ENDIF
        DO J=1,NINDX
          I  = INDEX(J)
          NR = N + I - 1
          KR = NUMSPH + NR
          R1=-MAX(ZERO,SIGPRV(1,I))
          R2=-MAX(ZERO,SIGPRV(2,I))
          R3=-MAX(ZERO,SIGPRV(3,I))
          STAB(1,KR) = DIRPRV(1,1,I)*DIRPRV(1,1,I)*R1
     &               + DIRPRV(1,2,I)*DIRPRV(1,2,I)*R2
     &               + DIRPRV(1,3,I)*DIRPRV(1,3,I)*R3
          STAB(2,KR) = DIRPRV(2,2,I)*DIRPRV(2,2,I)*R2
     &               + DIRPRV(2,3,I)*DIRPRV(2,3,I)*R3
     &               + DIRPRV(2,1,I)*DIRPRV(2,1,I)*R1
          STAB(3,KR) = DIRPRV(3,3,I)*DIRPRV(3,3,I)*R3
     &               + DIRPRV(3,1,I)*DIRPRV(3,1,I)*R1
     &               + DIRPRV(3,2,I)*DIRPRV(3,2,I)*R2
          STAB(4,KR) = DIRPRV(1,1,I)*DIRPRV(2,1,I)*R1
     &               + DIRPRV(1,2,I)*DIRPRV(2,2,I)*R2
     &               + DIRPRV(1,3,I)*DIRPRV(2,3,I)*R3
          STAB(5,KR) = DIRPRV(2,2,I)*DIRPRV(3,2,I)*R2
     &               + DIRPRV(2,3,I)*DIRPRV(3,3,I)*R3
     &               + DIRPRV(2,1,I)*DIRPRV(3,1,I)*R1
          STAB(6,KR) = DIRPRV(3,3,I)*DIRPRV(1,3,I)*R3
     &               + DIRPRV(3,1,I)*DIRPRV(1,1,I)*R1
     &               + DIRPRV(3,2,I)*DIRPRV(1,2,I)*R2

c          STAB(1,KR)=-MAX(ZERO,THIRD*(WAR(1,NR)+WAR(2,NR)+WAR(3,NR)))
c          STAB(2,KR)=-MAX(ZERO,THIRD*(WAR(1,NR)+WAR(2,NR)+WAR(3,NR)))
c          STAB(3,KR)=-MAX(ZERO,THIRD*(WAR(1,NR)+WAR(2,NR)+WAR(3,NR)))
c          STAB(4,KR)=ZERO
c          STAB(5,KR)=ZERO
c          STAB(6,KR)=ZERO
        END DO
      END DO
C----------------------------------
C
C Particules symetriques
C
C     /---------------/
       CALL MY_BARRIER
C     /---------------/
      NSPACTF=1+ITASK*NSPHACT/NTHREAD
      NSPACTL=(ITASK+1)*NSPHACT/NTHREAD
      DO NC=1,NSPCOND
        IC=ISPCOND(2,NC)
        IS=ISPCOND(3,NC)
        ISLIDE=ISPCOND(5,NC)
        IF(ISLIDE==0)THEN
          DO SS=NSPACTF,NSPACTL
           SM=WASPACT(SS)
           JS=ISPSYM(NC,SM)
           KS=NUMSPH+NSPHR+JS
           IF(JS>0.AND.STAB(7,SM)/=ZERO)THEN
C nothing changes
             STAB(1,KS)=STAB(1,SM)
             STAB(2,KS)=STAB(2,SM)
             STAB(3,KS)=STAB(3,SM)
             STAB(4,KS)=STAB(4,SM)
             STAB(5,KS)=STAB(5,SM)
             STAB(6,KS)=STAB(6,SM)
           END IF
          END DO
        ELSE !IF(ISLIDE==0)THEN
         DO IS=NSPACTF,NSPACTL,MVSIZ
           NINDX=0
           DO L=1,MIN(NSPACTL-IS+1,MVSIZ)
             SS=IS+L-1
             SM=WASPACT(SS)
             JS=ISPSYM(NC,SM)
             KS=NUMSPH+NSPHR+JS
             IF(JS>0.AND.STAB(7,SM)/=ZERO)THEN
               NINDX=NINDX+1
               INDEX(NINDX)=NINDX
               SIG(1,NINDX)=WASIGSM(1,JS)
               SIG(2,NINDX)=WASIGSM(2,JS)
               SIG(3,NINDX)=WASIGSM(3,JS)
               SIG(4,NINDX)=WASIGSM(4,JS)
               SIG(5,NINDX)=WASIGSM(5,JS)
               SIG(6,NINDX)=WASIGSM(6,JS)
             END IF
           END DO
           SIZE=6
           CALL SPH_VALPVEC(NINDX,INDEX,SIZE,SIG,SIGPRV,DIRPRV)
           NINDX=0
           DO L=1,MIN(NSPACTL-IS+1,MVSIZ)
             SS=IS+L-1
             SM=WASPACT(SS)
             JS=ISPSYM(NC,SM)
             KS=NUMSPH+NSPHR+JS
             IF(JS>0.AND.STAB(7,SM)/=ZERO)THEN
               NINDX=NINDX+1
               R1=-MAX(ZERO,SIGPRV(1,NINDX))
               R2=-MAX(ZERO,SIGPRV(2,NINDX))
               R3=-MAX(ZERO,SIGPRV(3,NINDX))
               STAB(1,KS) = DIRPRV(1,1,NINDX)*DIRPRV(1,1,NINDX)*R1
     &                    + DIRPRV(1,2,NINDX)*DIRPRV(1,2,NINDX)*R2
     &                    + DIRPRV(1,3,NINDX)*DIRPRV(1,3,NINDX)*R3
               STAB(2,KS) = DIRPRV(2,2,NINDX)*DIRPRV(2,2,NINDX)*R2
     &                    + DIRPRV(2,3,NINDX)*DIRPRV(2,3,NINDX)*R3
     &                    + DIRPRV(2,1,NINDX)*DIRPRV(2,1,NINDX)*R1
               STAB(3,KS) = DIRPRV(3,3,NINDX)*DIRPRV(3,3,NINDX)*R3
     &                    + DIRPRV(3,1,NINDX)*DIRPRV(3,1,NINDX)*R1
     &                    + DIRPRV(3,2,NINDX)*DIRPRV(3,2,NINDX)*R2
               STAB(4,KS) = DIRPRV(1,1,NINDX)*DIRPRV(2,1,NINDX)*R1
     &                    + DIRPRV(1,2,NINDX)*DIRPRV(2,2,NINDX)*R2
     &                    + DIRPRV(1,3,NINDX)*DIRPRV(2,3,NINDX)*R3
               STAB(5,KS) = DIRPRV(2,2,NINDX)*DIRPRV(3,2,NINDX)*R2
     &                    + DIRPRV(2,3,NINDX)*DIRPRV(3,3,NINDX)*R3
     &                    + DIRPRV(2,1,NINDX)*DIRPRV(3,1,NINDX)*R1
               STAB(6,KS) = DIRPRV(3,3,NINDX)*DIRPRV(1,3,NINDX)*R3
     &                    + DIRPRV(3,1,NINDX)*DIRPRV(1,1,NINDX)*R1
     &                    + DIRPRV(3,2,NINDX)*DIRPRV(1,2,NINDX)*R2
c          STAB(1,KS)=-MAX(ZERO,THIRD*(SIG(1,NINDX)+SIG(2,NINDX)+SIG(3,NINDX)))
c          STAB(2,KS)=-MAX(ZERO,THIRD*(SIG(1,NINDX)+SIG(2,NINDX)+SIG(3,NINDX)))
c          STAB(3,KS)=-MAX(ZERO,THIRD*(SIG(1,NINDX)+SIG(2,NINDX)+SIG(3,NINDX)))
c          STAB(4,KS)=ZERO
c          STAB(5,KS)=ZERO
c          STAB(6,KS)=ZERO
             END IF
           END DO
         END DO
        END IF
      END DO
C-----
C
C Particules symetriques de particules remotes
C
      DO NC=1,NSPCOND
        IC=ISPCOND(2,NC)
        IS=ISPCOND(3,NC)
        ISLIDE=ISPCOND(5,NC)
        IF(ISLIDE==0)THEN
          DO SS=NSPHRFT,NSPHRLT
           JS=ISPSYMR(NC,SS)
           KS=NUMSPH+NSPHR+JS
           IF(JS>0.AND.STAB(7,NUMSPH+SS)/=ZERO)THEN
C nothing changes
             STAB(1,KS)=STAB(1,NUMSPH+SS)
             STAB(2,KS)=STAB(2,NUMSPH+SS)
             STAB(3,KS)=STAB(3,NUMSPH+SS)
             STAB(4,KS)=STAB(4,NUMSPH+SS)
             STAB(5,KS)=STAB(5,NUMSPH+SS)
             STAB(6,KS)=STAB(6,NUMSPH+SS)
           END IF
          END DO
        ELSE !IF(ISLIDE==0)THEN
         DO IS=NSPHRFT,NSPHRLT,MVSIZ
           NINDX=0
           DO L=1,MIN(NSPHRLT-IS+1,MVSIZ)
             SS=IS+L-1
             JS=ISPSYMR(NC,SS)
             KS=NUMSPH+NSPHR+JS
             IF(JS>0.AND.STAB(7,NUMSPH+SS)/=ZERO)THEN
               NINDX=NINDX+1
               INDEX(NINDX)=NINDX
               SIG(1,NINDX)=WASIGSM(1,JS)
               SIG(2,NINDX)=WASIGSM(2,JS)
               SIG(3,NINDX)=WASIGSM(3,JS)
               SIG(4,NINDX)=WASIGSM(4,JS)
               SIG(5,NINDX)=WASIGSM(5,JS)
               SIG(6,NINDX)=WASIGSM(6,JS)
             END IF
           END DO
           SIZE=6
           CALL SPH_VALPVEC(NINDX,INDEX,SIZE,SIG,SIGPRV,DIRPRV)
           NINDX=0
           DO L=1,MIN(NSPHRLT-IS+1,MVSIZ)
             SS=IS+L-1
             JS=ISPSYMR(NC,SS)
             KS=NUMSPH+NSPHR+JS
             IF(JS>0.AND.STAB(7,NUMSPH+SS)/=ZERO)THEN
               NINDX=NINDX+1
               R1=-MAX(ZERO,SIGPRV(1,NINDX))
               R2=-MAX(ZERO,SIGPRV(2,NINDX))
               R3=-MAX(ZERO,SIGPRV(3,NINDX))
               STAB(1,KS) = DIRPRV(1,1,NINDX)*DIRPRV(1,1,NINDX)*R1
     &                    + DIRPRV(1,2,NINDX)*DIRPRV(1,2,NINDX)*R2
     &                    + DIRPRV(1,3,NINDX)*DIRPRV(1,3,NINDX)*R3
               STAB(2,KS) = DIRPRV(2,2,NINDX)*DIRPRV(2,2,NINDX)*R2
     &                    + DIRPRV(2,3,NINDX)*DIRPRV(2,3,NINDX)*R3
     &                    + DIRPRV(2,1,NINDX)*DIRPRV(2,1,NINDX)*R1
               STAB(3,KS) = DIRPRV(3,3,NINDX)*DIRPRV(3,3,NINDX)*R3
     &                    + DIRPRV(3,1,NINDX)*DIRPRV(3,1,NINDX)*R1
     &                    + DIRPRV(3,2,NINDX)*DIRPRV(3,2,NINDX)*R2
               STAB(4,KS) = DIRPRV(1,1,NINDX)*DIRPRV(2,1,NINDX)*R1
     &                    + DIRPRV(1,2,NINDX)*DIRPRV(2,2,NINDX)*R2
     &                    + DIRPRV(1,3,NINDX)*DIRPRV(2,3,NINDX)*R3
               STAB(5,KS) = DIRPRV(2,2,NINDX)*DIRPRV(3,2,NINDX)*R2
     &                    + DIRPRV(2,3,NINDX)*DIRPRV(3,3,NINDX)*R3
     &                    + DIRPRV(2,1,NINDX)*DIRPRV(3,1,NINDX)*R1
               STAB(6,KS) = DIRPRV(3,3,NINDX)*DIRPRV(1,3,NINDX)*R3
     &                    + DIRPRV(3,1,NINDX)*DIRPRV(1,1,NINDX)*R1
     &                    + DIRPRV(3,2,NINDX)*DIRPRV(1,2,NINDX)*R2
c          STAB(1,KS)=-MAX(ZERO,THIRD*(SIG(1,NINDX)+SIG(2,NINDX)+SIG(3,NINDX)))
c          STAB(2,KS)=-MAX(ZERO,THIRD*(SIG(1,NINDX)+SIG(2,NINDX)+SIG(3,NINDX)))
c          STAB(3,KS)=-MAX(ZERO,THIRD*(SIG(1,NINDX)+SIG(2,NINDX)+SIG(3,NINDX)))
c          STAB(4,KS)=ZERO
c          STAB(5,KS)=ZERO
c          STAB(6,KS)=ZERO
             END IF
           END DO
         END DO
        END IF
      END DO
C----------------------------------
      RETURN
      END
C
Chd|====================================================================
Chd|  SPH_VALPVEC                   source/elements/sph/spstab.F  
Chd|-- called by -----------
Chd|        SPSTABS                       source/elements/sph/spstab.F  
Chd|-- calls ---------------
Chd|        FLOATMIN                      ../common_source/tools/math/precision.c
Chd|====================================================================
      SUBROUTINE SPH_VALPVEC(NINDX,INDEX,SIZE,SIG,VAL,VEC)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NINDX, INDEX(*), SIZE
      my_real
     .   SIG(SIZE,*), VAL(3,*), VEC(9,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, L, N, II, NN, LMAX, LMAXV(MVSIZ),
     .       NINDEX1, NINDEX2, NINDEX3, 
     .       INDEX1(MVSIZ), INDEX2(MVSIZ), INDEX3(MVSIZ)
      my_real
     .   CS(6), STR(3,MVSIZ), A(3,3,MVSIZ), V(3,3,MVSIZ), B(3,3,MVSIZ),
     .   XMAG(3,MVSIZ), PR, AA, BB, AAA(MVSIZ),
     .   CC, ANGP, DD, FTPI, TTPI, STRMAX,
     .   TOL1(MVSIZ), TOL2(MVSIZ), XMAXV(MVSIZ), NORMINF(MVSIZ), 
     .   VMAG, S11,
     .   S21, S31, S12, S22, S32, S13, S23, S33, A11, A12, A13, A21,
     .   A22, A23, A31, A32, A33,
     .   MDEMI, XMAXINV, FLM
      REAL FLMIN
C-----------------------------------------------
C     DATA FTPI,TTPI / 4.188790205, 2.094395102 /
C     FTPI=(4/3)*PI, TTPI=(2/3)*PI
C
C     DEVIATEUR PRINCIPAL DE CONTRAINTE
C . . . . . . . . . . . . . . . . . . .
      MDEMI = -HALF
      TTPI = ACOS(MDEMI)
      FTPI = TWO*TTPI
C precision minimum dependant du type REAL ou DOUBLE
      CALL FLOATMIN(CS(1),CS(2),FLMIN)
      FLM = TWO*SQRT(FLMIN)
      NINDEX3=0  
      DO II = 1, NINDX
        NN=INDEX(II)
        CS(1) = SIG(1,NN)
        CS(2) = SIG(2,NN)
        CS(3) = SIG(3,NN)
        CS(4) = SIG(4,NN)
        CS(5) = SIG(5,NN)
        CS(6) = SIG(6,NN)
        PR = -(CS(1)+CS(2)+CS(3))* THIRD
        CS(1) = CS(1) + PR
        CS(2) = CS(2) + PR
        CS(3) = CS(3) + PR
        AAA(NN)=CS(4)**2 + CS(5)**2 + CS(6)**2 - CS(1)*CS(2)
     &      - CS(2)*CS(3) - CS(1)*CS(3)
        NORMINF(NN) = MAX(ABS(CS(1)),ABS(CS(2)),ABS(CS(3)),
     &      ABS(CS(4)),ABS(CS(5)),ABS(CS(6)))
        NORMINF(NN) = EM10*NORMINF(NN)
C cas racine triple
c        AA = MAX(AAA(NN),NORMINF(NN),EM20)
        AA = MAX(AAA(NN),NORMINF(NN))
C
        BB=CS(1)*CS(5)**2 + CS(2)*CS(6)**2
     &     + CS(3)*CS(4)**2 - CS(1)*CS(2)*CS(3)
     &     - TWO*CS(4)*CS(5)*CS(6)     
C
        CC=-SQRT(TWENTY7/MAX(EM20,AA))*BB*HALF/MAX(EM20,AA)
        CC= MIN(CC,ONE)
        CC= MAX(CC,-ONE)
        ANGP=ACOS(CC) * THIRD
        DD=TWO*SQRT(AA * THIRD)
        STR(1,NN)=DD*COS(ANGP)
        STR(2,NN)=DD*COS(ANGP+FTPI)
        STR(3,NN)=DD*COS(ANGP+TTPI)
C
        VAL(1,NN) = STR(1,NN)-PR
        VAL(2,NN) = STR(2,NN)-PR
        VAL(3,NN) = STR(3,NN)-PR
C renforcement de precision en compression----
        IF(ABS(STR(3,NN))>ABS(STR(1,NN))
     &     .AND.AAA(NN)>NORMINF(NN)) THEN
         AA=STR(1,NN)
         STR(1,NN)=STR(3,NN)
         STR(3,NN)=AA
         NINDEX3 = NINDEX3+1
         INDEX3(NINDEX3) = NN
        ENDIF
C . . . . . . . . . . .
C      VECTEURS PROPRES
C . . . . . . . . . . .
        STRMAX= MAX(ABS(STR(1,NN)),ABS(STR(3,NN)))
        TOL1(NN)= MAX(EM20,FLM*STRMAX**2)
        TOL2(NN)=FLM*STRMAX/3
        A(1,1,NN)=CS(1)-STR(1,NN)
        A(2,2,NN)=CS(2)-STR(1,NN)
        A(3,3,NN)=CS(3)-STR(1,NN)
        A(1,2,NN)=CS(4)
        A(2,1,NN)=CS(4)
        A(2,3,NN)=CS(5)
        A(3,2,NN)=CS(5)
        A(1,3,NN)=CS(6)
        A(3,1,NN)=CS(6)
C
        B(1,1,NN)=A(2,1,NN)*A(3,2,NN)-A(3,1,NN)
     .           *A(2,2,NN)
        B(1,2,NN)=A(2,2,NN)*A(3,3,NN)-A(3,2,NN)
     .           *A(2,3,NN)
        B(1,3,NN)=A(2,3,NN)*A(3,1,NN)-A(3,3,NN)
     .           *A(2,1,NN)
        B(2,1,NN)=A(3,1,NN)*A(1,2,NN)-A(1,1,NN)
     .           *A(3,2,NN)
        B(2,2,NN)=A(3,2,NN)*A(1,3,NN)-A(1,2,NN)
     .           *A(3,3,NN)
        B(2,3,NN)=A(3,3,NN)*A(1,1,NN)-A(1,3,NN)
     .           *A(3,1,NN)
        B(3,1,NN)=A(1,1,NN)*A(2,2,NN)-A(2,1,NN)
     .           *A(1,2,NN)
        B(3,2,NN)=A(1,2,NN)*A(2,3,NN)-A(2,2,NN)
     .           *A(1,3,NN)
        B(3,3,NN)=A(1,3,NN)*A(2,1,NN)-A(2,3,NN)
     .           *A(1,1,NN)
        XMAG(1,NN)=SQRT(B(1,1,NN)**2+B(2,1,NN)**2+B(3,1,NN)**2)
        XMAG(2,NN)=SQRT(B(1,2,NN)**2+B(2,2,NN)**2+B(3,2,NN)**2)
        XMAG(3,NN)=SQRT(B(1,3,NN)**2+B(2,3,NN)**2+B(3,3,NN)**2)

      ENDDO
C 
      NINDEX1 = 0
      NINDEX2 = 0
      DO II = 1, NINDX
        NN=INDEX(II)
        XMAXV(NN)=MAX(XMAG(1,NN),XMAG(2,NN),XMAG(3,NN))
        IF(XMAG(1,NN)==XMAXV(NN)) THEN
          LMAXV(NN) = 1
        ELSEIF(XMAG(2,NN)==XMAXV(NN)) THEN
          LMAXV(NN) = 2
        ELSE
          LMAXV(NN) = 3
        ENDIF
        IF(AAA(NN)<NORMINF(NN)) THEN
          VAL(1,NN) = SIG(1,NN)
          VAL(2,NN) = SIG(2,NN)
          VAL(3,NN) = SIG(3,NN)
          V(1,1,NN) = ONE
          V(2,1,NN) = ZERO
          V(3,1,NN) = ZERO
          V(1,2,NN) = ZERO
          V(2,2,NN) = ONE
          V(3,2,NN) = ZERO

        ELSEIF(XMAXV(NN)>TOL1(NN)) THEN
          NINDEX1 = NINDEX1 + 1
          INDEX1(NINDEX1) = NN
        ELSE
          NINDEX2 = NINDEX2 + 1
          INDEX2(NINDEX2) = NN
        ENDIF
      ENDDO
C
#include      "vectorize.inc"
      DO N = 1, NINDEX1
        NN = INDEX1(N)
        LMAX = LMAXV(NN)
        XMAXINV = ONE/XMAXV(NN)
        V(1,1,NN)=B(1,LMAX,NN)*XMAXINV
        V(2,1,NN)=B(2,LMAX,NN)*XMAXINV
        V(3,1,NN)=B(3,LMAX,NN)*XMAXINV
        A(1,1,NN)=A(1,1,NN)+STR(1,NN)-STR(3,NN)
        A(2,2,NN)=A(2,2,NN)+STR(1,NN)-STR(3,NN)
        A(3,3,NN)=A(3,3,NN)+STR(1,NN)-STR(3,NN)
C
        B(1,1,NN)=A(2,1,NN)*V(3,1,NN)-A(3,1,NN)*V(2,1,NN)
        B(1,2,NN)=A(2,2,NN)*V(3,1,NN)-A(3,2,NN)*V(2,1,NN)
        B(1,3,NN)=A(2,3,NN)*V(3,1,NN)-A(3,3,NN)*V(2,1,NN)
        B(2,1,NN)=A(3,1,NN)*V(1,1,NN)-A(1,1,NN)*V(3,1,NN)
        B(2,2,NN)=A(3,2,NN)*V(1,1,NN)-A(1,2,NN)*V(3,1,NN)
        B(2,3,NN)=A(3,3,NN)*V(1,1,NN)-A(1,3,NN)*V(3,1,NN)
        B(3,1,NN)=A(1,1,NN)*V(2,1,NN)-A(2,1,NN)*V(1,1,NN)
        B(3,2,NN)=A(1,2,NN)*V(2,1,NN)-A(2,2,NN)*V(1,1,NN)
        B(3,3,NN)=A(1,3,NN)*V(2,1,NN)-A(2,3,NN)*V(1,1,NN)
        XMAG(1,NN)=SQRT(B(1,1,NN)**2+B(2,1,NN)**2+B(3,1,NN)**2)
        XMAG(2,NN)=SQRT(B(1,2,NN)**2+B(2,2,NN)**2+B(3,2,NN)**2)
        XMAG(3,NN)=SQRT(B(1,3,NN)**2+B(2,3,NN)**2+B(3,3,NN)**2)
C
        XMAXV(NN)=MAX(XMAG(1,NN),XMAG(2,NN),XMAG(3,NN))
      ENDDO
C
#include      "vectorize.inc"
      DO N = 1, NINDEX1
        NN = INDEX1(N)
        IF(XMAG(1,NN)==XMAXV(NN)) THEN
          LMAXV(NN) = 1
        ELSEIF(XMAG(2,NN)==XMAXV(NN)) THEN
          LMAXV(NN) = 2
        ELSE
          LMAXV(NN) = 3
        ENDIF
C
        VMAG=SQRT(V(1,1,NN)**2+V(2,1,NN)**2)
        LMAX = LMAXV(NN)
        IF(XMAXV(NN)>TOL2(NN))THEN
          XMAXINV = ONE/XMAXV(NN)
          V(1,3,NN)=B(1,LMAX,NN)*XMAXINV
          V(2,3,NN)=B(2,LMAX,NN)*XMAXINV
          V(3,3,NN)=B(3,LMAX,NN)*XMAXINV
          V(1,2,NN)=V(2,3,NN)*V(3,1,NN)-V(2,1,NN)*V(3,3,NN)
          V(2,2,NN)=V(3,3,NN)*V(1,1,NN)-V(3,1,NN)*V(1,3,NN)
          V(3,2,NN)=V(1,3,NN)*V(2,1,NN)-V(1,1,NN)*V(2,3,NN)
          VMAG=ONE/SQRT(V(1,2,NN)**2+V(2,2,NN)**2+V(3,2,NN)**2)
          V(1,2,NN)=V(1,2,NN)*VMAG
          V(2,2,NN)=V(2,2,NN)*VMAG
          V(3,2,NN)=V(3,2,NN)*VMAG
        ELSEIF(VMAG>TOL2(NN))THEN
          V(1,2,NN)=-V(2,1,NN)/VMAG
          V(2,2,NN)=V(1,1,NN)/VMAG
          V(3,2,NN)=ZERO
        ELSE
          V(1,2,NN)=ONE
          V(2,2,NN)=ZERO
          V(3,2,NN)=ZERO  
        ENDIF
      ENDDO
C . . . . . . . . . . . . .
C    SOLUTION DOUBLE
C . . . . . . . . . . . . .
      DO N = 1, NINDEX2
        NN = INDEX2(N)
        XMAG(1,NN)=SQRT(A(1,1,NN)**2+A(2,1,NN)**2)
        XMAG(2,NN)=SQRT(A(1,2,NN)**2+A(2,2,NN)**2)
        XMAG(3,NN)=SQRT(A(1,3,NN)**2+A(2,3,NN)**2)
C
        XMAXV(NN)=MAX(XMAG(1,NN),XMAG(2,NN),XMAG(3,NN))
      ENDDO
C
#include      "vectorize.inc"
      DO N = 1, NINDEX2
        NN = INDEX2(N)
        IF(XMAG(1,NN)==XMAXV(NN)) THEN
          LMAXV(NN) = 1
        ELSEIF(XMAG(2,NN)==XMAXV(NN)) THEN
          LMAXV(NN) = 2
        ELSE
          LMAXV(NN) = 3
        ENDIF
C
        LMAX = LMAXV(NN)
        IF(MAX(ABS(A(3,1,NN)),ABS(A(3,2,NN)),ABS(A(3,3,NN)))
     &       <TOL2(NN))THEN
          XMAXINV = ONE/XMAXV(NN)
          V(1,1,NN)= ZERO
          V(2,1,NN)= ZERO
          V(3,1,NN)= ONE
          V(1,2,NN)=-A(2,LMAX,NN)*XMAXINV
          V(2,2,NN)= A(1,LMAX,NN)*XMAXINV
          V(3,2,NN)= ZERO
C
        ELSEIF(XMAXV(NN)>TOL2(NN))THEN
          XMAXINV = ONE/XMAXV(NN)
          V(1,1,NN)=-A(2,LMAX,NN)*XMAXINV
          V(2,1,NN)= A(1,LMAX,NN)*XMAXINV
          V(3,1,NN)= ZERO
          V(1,2,NN)=-A(3,LMAX,NN)*V(2,1,NN)
          V(2,2,NN)= A(3,LMAX,NN)*V(1,1,NN)
          V(3,2,NN)= A(1,LMAX,NN)*V(2,1,NN)-A(2,LMAX,NN)*V(1,1,NN)
          VMAG=ONE/SQRT(V(1,2,NN)**2+V(2,2,NN)**2+V(3,2,NN)**2)
          V(1,2,NN)=V(1,2,NN)*VMAG
          V(2,2,NN)=V(2,2,NN)*VMAG
          V(3,2,NN)=V(3,2,NN)*VMAG
        ELSE
          V(1,1,NN) = ONE
          V(2,1,NN) = ZERO
          V(3,1,NN) = ZERO
          V(1,2,NN) = ZERO
          V(2,2,NN) = ONE
          V(3,2,NN) = ZERO    
        ENDIF
      ENDDO
C
      DO II = 1, NINDX
        NN=INDEX(II)
        VEC(1,NN)=V(1,1,NN)
        VEC(2,NN)=V(2,1,NN)
        VEC(3,NN)=V(3,1,NN)
        VEC(4,NN)=V(1,2,NN)
        VEC(5,NN)=V(2,2,NN)
        VEC(6,NN)=V(3,2,NN)
        VEC(7,NN)=VEC(2,NN)*VEC(6,NN)-VEC(3,NN)*VEC(5,NN)
        VEC(8,NN)=VEC(3,NN)*VEC(4,NN)-VEC(1,NN)*VEC(6,NN)
        VEC(9,NN)=VEC(1,NN)*VEC(5,NN)-VEC(2,NN)*VEC(4,NN)
      ENDDO
C reecriture pour contourner probleme sur itanium2 comp 9. + latency=16
      DO N = 1, NINDEX3
        NN = INDEX3(N)
C str utilise com tableau temporaire au lieu de scalaires temporaires
        STR(1,NN)=VEC(7,NN)
        STR(2,NN)=VEC(8,NN)
        STR(3,NN)=VEC(9,NN)
      ENDDO
      DO N = 1, NINDEX3
        NN = INDEX3(N)
        VEC(7,NN)=VEC(1,NN)
        VEC(8,NN)=VEC(2,NN)
        VEC(9,NN)=VEC(3,NN)
        VEC(1,NN)=-STR(1,NN)
        VEC(2,NN)=-STR(2,NN)
        VEC(3,NN)=-STR(3,NN)
      ENDDO
C
      RETURN
      END

