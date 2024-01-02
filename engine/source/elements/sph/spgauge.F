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
Chd|  SPGAUGE                       source/elements/sph/spgauge.F 
Chd|-- called by -----------
Chd|        FORINTP                       source/elements/forintp.F     
Chd|-- calls ---------------
Chd|        SPMD_EXCH_FR6                 source/mpi/kinematic_conditions/spmd_exch_fr6.F
Chd|        SUM_6_FLOAT                   source/system/parit.F         
Chd|        WEIGHT0                       source/elements/sph/weight.F  
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        SPHBOX                        share/modules/sphbox.F        
Chd|====================================================================
      SUBROUTINE SPGAUGE(LGAUGE  ,GAUGE ,KXSP   ,IXSP  ,
     1                   SPBUF   ,IPARG ,ELBUF_TAB,ISPSYM ,XSPSYM,
     2                   NOD2SP  ,X       ,ITASK  ,WA     ,WASIGSM,
     3                   WAR     ,SPHG_F6)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE SPHBOX
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
#include      "sphcom.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LGAUGE(3,*), KXSP(NISP,*), IXSP(KVOISPH,*),
     .        IPARG(NPARG,*), ISPSYM(NSPCOND,*), NOD2SP(*), ITASK
C     REAL
      my_real
     .   GAUGE(LLGAUGE,*), SPBUF(NSPBUF,*), X(3,*),XSPSYM(3,*),
     .   WA(KWASPH,*), WASIGSM(6,*), WAR(10,*)
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      DOUBLE PRECISION SPHG_F6(4,6,NBGAUGE)
C-----------------------------------------------
c     GAUGE(3,*)
c 1:  -Isolid           -(NUMELS_G+1) if SPH gauge
c 2:  GaugeId
c 3:  +Node or -Shell
c
c     => GAUGE(LLGAUGE,*), LLGAUGE = 37
c 1:  Dist (distance from Shell)     Dist (distance from Shell)
c 2:  XG           XG
c 3:  YG           YG
c 4:  ZG           ZG
c 5:  Alpha (Solid penetration ratio)     not yet used
c 6:               XSAV (SPH sorting)
c 7:               YSAV (SPH sorting)
c 8:               ZSAV (SPH sorting)
c 9:               FF (sph only)
c 10:              intantaneous Pressure
c 11:              not yet available
c 12:              intantaneous Rho
c 13:              intantaneous E
c 14:              ! Butterworth !
c 15:              ! Butterworth !
c 16:              ! Butterworth !
c 17:              ! Butterworth !
c 18:              ! Butterworth !
c 19:              ! Butterworth !
c 20:                    ! Butterworth !
c 21:                    ! Butterworth !
c 22:                    ! Butterworth !
c 23:                    ! Butterworth !  
c 24:              ! Butterworth !
c 25:              ! Butterworth !
c 26:              ! Butterworth !
c 27:              ! Butterworth !
c 28:              ! Butterworth !
c 29:              ! Butterworth !
c 30:  Pressure                 filtered Pressure
c 31:  PA                     not yet available
c 32:  Rho                    filtered Rho
c 33:  E                      filtered E       
c 34:              ! Butterworth !
c 35:              ! Butterworth !
c 36:              ! Butterworth !
c 37:              ! Butterworth !
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,INOD,JNOD,J,NVOIS,M,
     .        NVOISS,SM,JS,NC,NS,NN,
     .        IG,NEL,KAD,NG,K,KK,SFGAUGE,FR_RL(NSPMD+2)
      my_real
     .       XI,YI,ZI,DI,RHOI,XJ,YJ,ZJ,DJ,RHOJ,VJ,
     .       WGHT,ALPHAI,WCOMPI,
     .       PP,EE,PRESS,ENER,RHO
      TYPE(G_BUFEL_)  ,POINTER :: GBUF
      my_real
     .       , DIMENSION(:), ALLOCATABLE :: FGAUGE1,FGAUGE2,FGAUGE3,FGAUGE4        
C-----------------------------------------------
      FR_RL(:) = 1
!$OMP DO SCHEDULE(DYNAMIC,1)
      DO IG=1,NBGAUGE
        IF(LGAUGE(1,IG) > -(NUMELS+1)) CYCLE
C------
        N=NUMSPH+IG
        XI =GAUGE(2,IG)
        YI =GAUGE(3,IG)
        ZI =GAUGE(4,IG)
        WCOMPI =ZERO
        PRESS  =ZERO
        ENER   =ZERO
        RHO    =ZERO
C------
          NVOIS=KXSP(4,N)
C------
        SFGAUGE = KXSP(4,N) + KXSP(6,N)
        ALLOCATE(FGAUGE1(SFGAUGE),FGAUGE2(SFGAUGE),FGAUGE3(SFGAUGE),FGAUGE4(SFGAUGE))
C------
          DO J=1,NVOIS
            JNOD=IXSP(J,N)
            IF(JNOD>0)THEN
              M=NOD2SP(JNOD)
            IF(KXSP(2,M)<=0) CYCLE
              XJ=X(1,JNOD)
              YJ=X(2,JNOD)
              ZJ=X(3,JNOD)
              DJ  =SPBUF(1,M)
              RHOJ=SPBUF(2,M)
              VJ=SPBUF(12,M)/MAX(EM20,RHOJ)
              CALL WEIGHT0(XI,YI,ZI,XJ,YJ,ZJ,DJ,WGHT)
            WCOMPI=VJ*WGHT
            PP=-THIRD*(WA(1,M)+WA(2,M)+WA(3,M))
            FGAUGE1(J) = WCOMPI
            FGAUGE2(J) = VJ*WGHT*PP
            FGAUGE3(J) = VJ*WGHT*RHOJ
            FGAUGE4(J) = ENER
            END IF
          ENDDO
C------
C       partie symetrique.
        NVOISS=KXSP(6,N)
        K = NVOIS
        DO J=KXSP(5,N)+1,KXSP(5,N)+NVOISS
          JS=IXSP(J,N)
          IF(JS>0)THEN
            SM=JS/(NSPCOND+1)
            NC=MOD(JS,NSPCOND+1)
            JS=ISPSYM(NC,SM)
            XJ =XSPSYM(1,JS)
            YJ =XSPSYM(2,JS)
            ZJ =XSPSYM(3,JS)
            DJ  =SPBUF(1,SM)
            RHOJ=SPBUF(2,SM)
            VJ=SPBUF(12,SM)/MAX(EM20,RHOJ)
            CALL WEIGHT0(XI,YI,ZI,XJ,YJ,ZJ,DJ,WGHT)
            WCOMPI=VJ*WGHT
            PP=-THIRD*(WASIGSM(1,JS)+WASIGSM(2,JS)+WASIGSM(3,JS))
            K = K+1
            FGAUGE1(K) = WCOMPI
            FGAUGE2(K) = VJ*WGHT*PP
            FGAUGE3(K) = VJ*WGHT*RHOJ
            FGAUGE4(K) = ENER
          END IF
        ENDDO
C
C Traitement Parith/ON avant echange
C
        DO K = 1, 6
          SPHG_F6(1,K,IG) = ZERO
          SPHG_F6(2,K,IG) = ZERO
          SPHG_F6(3,K,IG) = ZERO
          SPHG_F6(4,K,IG) = ZERO
        END DO
C
        IF(IPARIT > 0)THEN
          CALL SUM_6_FLOAT(1  ,SFGAUGE  ,FGAUGE1, SPHG_F6(1,1,IG),4)
          CALL SUM_6_FLOAT(1  ,SFGAUGE  ,FGAUGE2, SPHG_F6(2,1,IG),4)
          CALL SUM_6_FLOAT(1  ,SFGAUGE  ,FGAUGE3, SPHG_F6(3,1,IG),4)
          CALL SUM_6_FLOAT(1  ,SFGAUGE  ,FGAUGE4, SPHG_F6(4,1,IG),4)
        ELSE
          DO I=1,SFGAUGE
            K = 1
            SPHG_F6(1,K,IG) = SPHG_F6(1,K,IG) + FGAUGE1(I)
            SPHG_F6(2,K,IG) = SPHG_F6(2,K,IG) + FGAUGE2(I)
            SPHG_F6(3,K,IG) = SPHG_F6(3,K,IG) + FGAUGE3(I)
            SPHG_F6(4,K,IG) = SPHG_F6(4,K,IG) + FGAUGE4(I)
          END DO
        END IF
C------
        DEALLOCATE(FGAUGE1,FGAUGE2,FGAUGE3,FGAUGE4)
C-----------------
      END DO
!$OMP END DO
C
C-------------
      IF(NSPMD > 1) THEN    
!$OMP SINGLE
           CALL SPMD_EXCH_FR6(FR_RL,SPHG_F6,4*NBGAUGE*6)
!$OMP END SINGLE
      ENDIF
C-------------
C
C Traitement Parith/ON apres echange
C
!$OMP DO SCHEDULE(DYNAMIC,1)
      DO IG=1,NBGAUGE
        IF(LGAUGE(1,IG) > -(NUMELS+1)) CYCLE
        WCOMPI =  SPHG_F6(1,1,IG)+SPHG_F6(1,2,IG)+SPHG_F6(1,3,IG)+
     +            SPHG_F6(1,4,IG)+SPHG_F6(1,5,IG)+SPHG_F6(1,6,IG)
        PRESS =   SPHG_F6(2,1,IG)+SPHG_F6(2,2,IG)+SPHG_F6(2,3,IG)+
     +            SPHG_F6(2,4,IG)+SPHG_F6(2,5,IG)+SPHG_F6(2,6,IG)
        RHO  =    SPHG_F6(3,1,IG)+SPHG_F6(3,2,IG)+SPHG_F6(3,3,IG)+
     +            SPHG_F6(3,4,IG)+SPHG_F6(3,5,IG)+SPHG_F6(3,6,IG)      
        ALPHAI=ONE/MAX(EM20,WCOMPI)
        PRESS=PRESS*ALPHAI
        RHO  =RHO*ALPHAI
        GAUGE(10,IG)=PRESS
        GAUGE(12,IG)=RHO
C energy not yet available
        GAUGE(13,IG)=ZERO    
      END DO
!$OMP END DO      
C
      RETURN
      END
Chd|====================================================================
Chd|  SPGAUGE_F                     source/elements/sph/spgauge.F 
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPGAUGE_F(P,FF,P0,P1,P2,N)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N
C     REAL
      my_real
     . P(N),P0(N,2),P1(N,2),P2(N,2),FF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J,IFIRST
      my_real
     . PI1,PI8,PI38,SPI8,SPI38,C0,C1,C2,C3,C4,C5,C6,C7,C8,C9,
     , X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3,D,DD,D2,DP,E,G,F
C
      IF(FF==ZERO)THEN
        DO J=1,N
          P2(J,1) = P(J)
        ENDDO
        RETURN
      END IF
C
      F = MIN(FF,ZEP4/DT2)
C-----------------------------------------------
C INITIALIALISATION DES COEFFICIENTS DU FILTRE
C-----------------------------------------------
      PI1   = TWO*ATAN2(ONE,ZERO)
      PI8  = PI1*ONE_OVER_8
      PI38 = THREE*PI8
      SPI8  = SIN(PI8)
      SPI38 = SIN(PI38)
C
      D  = TAN(PI1*F*DT2)
      DD = D*D
      D2 = TWO*D
      DP = ONE + DD
      E  = D2*SPI8
      G  = E + DP
      G  = ONE/G
C
      C0 = DD * G
      C1 = TWO* C0
      C2 = C0
      C3 = TWO * G - C1
      C4 = (E - DP) * G
C
      E  = D2*SPI38
      G  = E + DP
      G  = ONE/G
C
      C5 = DD * G
      C6 = TWO * C5
      C7 = C5
      C8 = TWO * G - C6
      C9 = (E - DP) * G
C-----------------------------------------------
C FILTRAGE
C-----------------------------------------------
      DO J=1,N
        X1 = P0(J,2)
        X2 = P0(J,1)
        X3 = P(J)
        Y1 = P1(J,2)
        Y2 = P1(J,1)
        Y3 = C0 * X3 + C1 * X2 + C2 * X1
     .     + C3 * Y2 + C4 * Y1
        Z1 = P2(J,2)
        Z2 = P2(J,1)
        Z3 = C5 * Y3 + C6 * Y2 + C7 * Y1
     .     + C8 * Z2 + C9 * Z1
C
        P0(J,2) = X2
        P0(J,1) = X3
        P1(J,2) = Y2
        P1(J,1) = Y3
        P2(J,2) = Z2
        P2(J,1) = Z3
      ENDDO
C
      RETURN
      END
