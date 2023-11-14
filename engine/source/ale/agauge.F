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
Chd|  AGAUGE                        source/ale/agauge.F           
Chd|-- called by -----------
Chd|        ALEMAIN                       source/ale/alemain.F          
Chd|-- calls ---------------
Chd|        AGAUG3                        source/ale/agauge.F           
Chd|        AGAUG30                       source/ale/agauge.F           
Chd|        AGAUG3Q                       source/ale/agaug3q.F          
Chd|        AGAUG3T                       source/ale/agaug3t.F          
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_E1VOIS                   source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_RBCAST                   source/mpi/generic/spmd_rbcast.F
Chd|        SPMD_SD_GAUG                  source/mpi/output/spmd_sd_gaug.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE AGAUGE(
     1  IPARG    ,ELBUF_STR   ,PHI     ,IXS      ,IXQ     ,
     2  X        ,ALE_CONNECT ,ITASK   ,NERCVOIS ,NESDVOIS,
     3  LERCVOIS ,LESDVOIS    ,LENCOM  ,LGAUGE   ,
     4  GAUGE    ,V           ,IGAUP   ,NGAUP    ,IXTG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD            
      USE ALE_CONNECTIVITY_MOD
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
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,NGROUP), ITASK, LENCOM,IXTG(NIXTG,NUMELTG),
     .        NERCVOIS(*),NESDVOIS(*),LERCVOIS(*),LESDVOIS(*),
     .        IXS(NIXS,NUMELS),IXQ(NIXQ,NUMELQ),LGAUGE(3,NBGAUGE),IGAUP(NBGAUGE),NGAUP(NSPMD)   
C     REAL
      my_real PHI(SPHI),GAUGE(LLGAUGE,NBGAUGE),X(3,NUMNOD),V(3,NUMNOD)
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_STR
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NG, JMUL, IADR, I, II,J,JJ(6), K,N,IG,IS,IGAUGE,IG0,
     .        ITAG(NBGAUGE),NEL,NUMEL,NCONNECT
      my_real P,RHO,E,PA,U2,ALPHA(NBGAUGE)
      TYPE(G_BUFEL_) ,POINTER :: GBUF
C-----------------------------------------------
c     LGAUGE(3,*)
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
c 11:              intantaneous PA
c 12:              intantaneous Rho
c 13:              intantaneous E
c 14:              ! Butterworth !
c 15:              ! Butterworth !
c 16:              ! Butterworth !
c 17:              ! Butterworth !
c 18:              ! Butterworth !
c 19:              ! Butterworth !
c 20:                  ! Butterworth !
c 21:            ! Butterworth !
c 22:            ! Butterworth !
c 23:            ! Butterworth !  
c 24:              ! Butterworth !
c 25:              ! Butterworth !
c 26:              ! Butterworth !
c 27:              ! Butterworth !
c 28:              ! Butterworth !
c 29:              ! Butterworth !
c 30:  Pressure                 filtered Pressure
c 31:  PA                     filtered PA
c 32:  Rho                    filtered Rho
c 33:  E                      filtered E       
c 34:              ! Xpoint      !
c 35:              ! Ypoint      !
c 36:              ! Zpoint      !
c 37:              ! Butterworth !
C-----------------------------------------------

C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IGAUGE=0
      DO IG=1,NBGAUGE
        IF(LGAUGE(1,IG) <= 0 .AND. LGAUGE(1,IG) >= -(NUMELS+NUMELQ+NUMELTG))
     .     IGAUGE=1
      END DO
C      
      CALL MY_BARRIER
C      
      IF(IGAUGE == 0)RETURN     
C 
C-----------------------------
C       Recherche des elements T=0
C-----------------------------
      IF(TT==ZERO)THEN
       DO NG=ITASK+1,NGROUP,NTHREAD
        IF( IPARG(5,NG) /= 1 .AND. IPARG(5,NG) /= 2 .AND.
     .     (IPARG(5,NG) /= 7 .AND. N2D == 0) ) CYCLE
        CALL INITBUF(IPARG    ,NG      ,
     2        MTN     ,LLT     ,NFT     ,IADR    ,ITY     ,
     3        NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,
     4        JTHE    ,JLAG    ,JMUL    ,JHBE    ,JIVF    ,
     5        NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,
     6        IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,
     7        ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
        IF (IPARG(8,NG) == 1)       CYCLE
        LFT=1
        IF (IPARG(5,NG) == 1) THEN
          ! solid (8N)
          NUMEL = NUMELS
          NCONNECT = 8
          CALL AGAUG30(LGAUGE,GAUGE,IXS ,X  ,NIXS,NUMEL,NCONNECT)
        ELSEIF (IPARG(5,NG) == 2) THEN
          ! quad 2D
          NUMEL = NUMELQ
          NCONNECT = 4
          CALL AGAUG30(LGAUGE,GAUGE,IXQ ,X  ,NIXQ,NUMEL,NCONNECT)
        ELSEIF (IPARG(5,NG) == 7) THEN
          ! tria 2D
          NUMEL = NUMELTG
          NCONNECT = 3
          CALL AGAUG30(LGAUGE,GAUGE,IXTG ,X  ,NIXTG,NUMEL,NCONNECT)
        ENDIF
       ENDDO
       CALL MY_BARRIER 
      ENDIF
C
      IF(ITASK==0)THEN
        DO I=1,MAX(NUMELS,NUMELQ,NUMELTG)
          PHI(I)=ZERO
        END DO
C
        DO IG=1,NBGAUGE
          IS = -LGAUGE(1,IG)
          IF(IS > 0 .AND. IS <= NUMELS+NUMELQ+NUMELTG)THEN
               PHI(IS)= IG 
C
C              PHI(IS)=I
c penetration ratio within element (initialisation)
            GAUGE(5,IG)=ZERO
          ENDIF
        END DO
      ENDIF
C
      CALL MY_BARRIER
C-----------------------------
C       SPMD EXCHANGE
C-----------------------------
      IF (NSPMD > 1) THEN
!$OMP SINGLE
        CALL SPMD_E1VOIS(PHI,NERCVOIS,NESDVOIS,LERCVOIS,LESDVOIS,LENCOM)
!$OMP END SINGLE
      ENDIF
C-----------------------------
C       Searching for new elements
C-----------------------------
      DO NG=ITASK+1,NGROUP,NTHREAD
        IF( IPARG(5,NG) /= 1 .AND. IPARG(5,NG) /= 2 .AND.
     .     (IPARG(5,NG) /= 7 .AND. N2D == 0) ) CYCLE
        CALL INITBUF(IPARG    ,NG      ,
     2        MTN     ,LLT     ,NFT     ,IADR    ,ITY     ,
     3        NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,
     4        JTHE    ,JLAG    ,JMUL    ,JHBE    ,JIVF    ,
     5        NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,
     6        IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,
     7        ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
        IF(IPARG(8,NG) == 1)       CYCLE
         
        LFT=1    
        IF (IPARG(5,NG) == 1) THEN
          ! solid (8N)
          CALL AGAUG3(LGAUGE,GAUGE,PHI,IXS  ,X  ,ALE_CONNECT )
        ELSEIF (IPARG(5,NG) == 2) THEN
          ! quad 2D
          CALL AGAUG3Q(LGAUGE,GAUGE,PHI,IXQ  ,X  ,ALE_CONNECT )
        ELSEIF (IPARG(5,NG) == 7) THEN
          ! tria 2D
          CALL AGAUG3T(LGAUGE,GAUGE,PHI,IXTG ,X  ,ALE_CONNECT )
        ENDIF
      END DO
C      
       CALL MY_BARRIER
       IF(ITASK==0)THEN
        DO I=1,MAX(NUMELS,NUMELQ,NUMELTG)
          PHI(I)=ZERO
        END DO
C
C  This array is used when several gauges are on the same element
c
        DO IG= 1,NBGAUGE
          ITAG(IG) = 0
        ENDDO
C
        DO IG=1,NBGAUGE
          IS = -LGAUGE(1,IG)
          IF(IS > 0 .AND. IS <= (NUMELS+NUMELQ+NUMELTG))THEN
            IG0 = NINT(PHI(IS))
            IF(IG0 > 0) THEN
              ITAG(IG) = IG0
            ELSE
              PHI(IS) = IG  
            ENDIF
           ENDIF 
        END DO
      ENDIF
C      
      CALL MY_BARRIER
C-----------------------------
C       SPMD EXCHANGE
C-----------------------------
      IF (NSPMD > 1) THEN
!$OMP SINGLE
        CALL SPMD_E1VOIS(PHI,NERCVOIS,NESDVOIS,LERCVOIS,LESDVOIS,LENCOM)
!$OMP END SINGLE
      ENDIF  
C-----------------------------
C     CALCULATION OF GAUGE MEASURES
C-----------------------------
      DO NG=ITASK+1,NGROUP,NTHREAD
C-----------------------------
        IF( IPARG(5,NG) /= 1 .AND. IPARG(5,NG) /= 2 .AND.
     .     (IPARG(5,NG) /= 7 .AND. N2D == 0) ) CYCLE
        CALL INITBUF(IPARG    ,NG      ,
     2        MTN     ,LLT     ,NFT     ,IADR    ,ITY     ,
     3        NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,
     4        JTHE    ,JLAG    ,JMUL    ,JHBE    ,JIVF    ,
     5        NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,
     6        IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,
     7        ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
         IF (IPARG(8,NG) == 1) CYCLE
         GBUF => ELBUF_STR(NG)%GBUF
         LFT=1
!
         NEL = LLT
         DO I=1,6
           JJ(I) = NEL*(I-1)
         ENDDO
!
         DO I=LFT,LLT
          II=I+NFT
          IG = NINT(PHI(II))
          IF (IG <= 0) CYCLE
c         
          P = (GBUF%SIG(JJ(1)+I) + GBUF%SIG(JJ(2)+I) + GBUF%SIG(JJ(3)+I))/THREE
          RHO = GBUF%RHO(I)
          E   = GBUF%EINT(I)
          U2 = ZERO
c
          IF (IPARG(5,NG) == 1) THEN
            ! Solid (8N)
            DO J=2,9
              N = IXS(J,II)
              U2 = U2+ V(1,N)*V(1,N)
              U2 = U2+ V(2,N)*V(2,N)
              U2 = U2+ V(3,N)*V(3,N)
            ENDDO
          ELSEIF (IPARG(5,NG) == 2) THEN
            ! Quad 2D
            DO J=2,5
              N = IXQ(J,II)
              U2 = U2+ V(1,N)*V(1,N)
              U2 = U2+ V(2,N)*V(2,N)
              U2 = U2+ V(3,N)*V(3,N)
            ENDDO
          ELSEIF (IPARG(5,NG) == 7) THEN
            ! Tria 2D
            DO J=2,4
              N = IXTG(J,II)
              U2 = U2+ V(1,N)*V(1,N)
              U2 = U2+ V(2,N)*V(2,N)
              U2 = U2+ V(3,N)*V(3,N)
            ENDDO
          ENDIF
          PA = P - RHO*U2/SIXTEEN
#include "lockon.inc"
          GAUGE(30,IG)= -P
          GAUGE(31,IG)= -PA
          GAUGE(32,IG)= RHO
          GAUGE(33,IG)= E
#include "lockoff.inc"
          END DO ! I 
      ENDDO ! NG=ITASK+1,NGROUP,NTASK

      IF(ITASK == 0) THEN
       DO IG=1,NBGAUGE
        IG0= ITAG(IG)
        IF(IG0 > 0) THEN
#include "lockon.inc"
          GAUGE(30,IG)= GAUGE(30,IG0)
          GAUGE(31,IG)= GAUGE(31,IG0)
          GAUGE(32,IG)= GAUGE(32,IG0)
          GAUGE(33,IG)= GAUGE(33,IG0)
#include "lockoff.inc" 
        ENDIF
       ENDDO 
      ENDIF
C       
      CALL MY_BARRIER 
C-------------
      IF(NSPMD > 1) THEN        
        IF(ITASK == 0) THEN
          DO IG=1,NBGAUGE
           ALPHA(IG) = GAUGE(5,IG)
          ENDDO
          CALL SPMD_SD_GAUG(LGAUGE,GAUGE,IGAUP,NGAUP)
          CALL SPMD_RBCAST(GAUGE,GAUGE,LLGAUGE,NBGAUGE,0,2)
C        
          DO IG=1,NBGAUGE
           IF(GAUGE(5,IG) /= ALPHA(IG))LGAUGE(1,IG) = 0  
          ENDDO
        ENDIF
      ENDIF 
      RETURN
      END
Chd|====================================================================
Chd|  AGAUG30                       source/ale/agauge.F           
Chd|-- called by -----------
Chd|        AGAUGE                        source/ale/agauge.F           
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE AGAUG30(LGAUGE ,GAUGE ,IX  ,X ,NIX, NUMEL,NCONNECT)
C-----------------------------------------------
C   Description
C-----------------------------------------------
c  Searching for element associated to the gauge
c     algorithmic complexity:quadratic (numels*nbgauge) at time 0
c     can be improved
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIX,NUMEL,NCONNECT
      INTEGER IX(NIX,NUMEL),LGAUGE(3,NBGAUGE)
      my_real X(3,NUMNOD),GAUGE(LLGAUGE,NBGAUGE)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,J,N,IG,IGAUGE
      my_real XX1,YY1,ZZ1,XX2,YY2,ZZ2,XG,YG,ZG
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      DO I=1,LLT
          II=I+NFT
          XX1 = EP30
          YY1 = EP30
          ZZ1 = EP30
          XX2 = -EP30
          YY2 = -EP30
          ZZ2 = -EP30
          DO J=2,NCONNECT+1
            XX1 = MIN(XX1,X(1,IX(J,II)))
            YY1 = MIN(YY1,X(2,IX(J,II)))
            ZZ1 = MIN(ZZ1,X(3,IX(J,II)))
            XX2 = MAX(XX2,X(1,IX(J,II)))
            YY2 = MAX(YY2,X(2,IX(J,II)))
            ZZ2 = MAX(ZZ2,X(3,IX(J,II)))
          ENDDO
C          
          IGAUGE = 0 
          DO IG=1,NBGAUGE
            IF(LGAUGE(1,IG) > 0 .OR. 
     .         LGAUGE(1,IG) < -(NUMELS+NUMELQ+NUMELTG)) CYCLE
            XG  = GAUGE(2,IG)
            YG  = GAUGE(3,IG)
            ZG  = GAUGE(4,IG)
            IF (NCONNECT == 8) THEN
              ! solid (8N)
              IF(XG < XX1)CYCLE
              IF(XG > XX2)CYCLE

              IF(YG < YY1)CYCLE
              IF(YG > YY2)CYCLE

              IF(ZG < ZZ1)CYCLE
              IF(ZG > ZZ2)CYCLE
            ELSEIF (NCONNECT == 3 . OR. NCONNECT == 4) THEN
              ! tria 2D + quad 2D
              IF(YG < YY1)CYCLE
              IF(YG > YY2)CYCLE

              IF(ZG < ZZ1)CYCLE
              IF(ZG > ZZ2)CYCLE
            ENDIF

            IGAUGE=IG
            IF(IGAUGE == 0)CYCLE
c element associated to the gauge
c in case of multiple solutions do not need to choose the best one : AGAUG3 will update it.           
#include "lockon.inc"
            LGAUGE(1,IGAUGE)=-II
#include "lockoff.inc"
         ENDDO
      ENDDO

      RETURN
      END
Chd|====================================================================
Chd|  AGAUG3                        source/ale/agauge.F           
Chd|-- called by -----------
Chd|        AGAUGE                        source/ale/agauge.F           
Chd|-- calls ---------------
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|====================================================================
      SUBROUTINE AGAUG3(LGAUGE,GAUGE,PHI,IXS  ,X    ,ALE_CONNECT )
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,NUMELS),LGAUGE(3,NBGAUGE)
      my_real PHI(SPHI),X(3,NUMNOD),GAUGE(LLGAUGE,NBGAUGE)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,J,N,N1,N2,N3,N4,IG,IAD2
      INTEGER IFACE(4,6)
      my_real ALPHA,XX0,YY0,ZZ0,XX1,YY1,ZZ1,XX2,YY2,ZZ2,A1,A2,A3,
     .        VOL,AREAP32,XG,YG,ZG
      DATA IFACE/ 1, 2, 3, 4,
     2            4, 3, 7, 8,
     3            8, 7, 6, 5,
     4            5, 6, 2, 1,
     5            2, 6, 7, 3,
     5            1, 4, 8, 5/
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------     
c--------------------------------------------------------------
c  Searching for the new element associated to the gauge
c--------------------------------------------------------------
      DO I=1,LLT
        II=I+NFT  
        IAD2 = ALE_CONNECT%ee_connect%iad_connect(II)
        DO J=1,6
          N= ALE_CONNECT%ee_connect%connected(IAD2 + J - 1)
          IF(N<=0)CYCLE
          IG=NINT(PHI(N))
          IF(IG==0)CYCLE
C            
            XG     = GAUGE(2,IG)
            YG     = GAUGE(3,IG)
            ZG     = GAUGE(4,IG)
C                    
            N1 = IXS(IFACE(1,J)+1,II)
            N2 = IXS(IFACE(2,J)+1,II)
            N3 = IXS(IFACE(3,J)+1,II)
            N4 = IXS(IFACE(4,J)+1,II)
C            
            XX0 = (X(1,N1)+X(1,N2)+X(1,N3)+X(1,N4))*FOURTH
            YY0 = (X(2,N1)+X(2,N2)+X(2,N3)+X(2,N4))*FOURTH
            ZZ0 = (X(3,N1)+X(3,N2)+X(3,N3)+X(3,N4))*FOURTH
            XX1 = X(1,N3)-X(1,N1)
            YY1 = X(2,N3)-X(2,N1)
            ZZ1 = X(3,N3)-X(3,N1)
            XX2 = X(1,N4)-X(1,N2)
            YY2 = X(2,N4)-X(2,N2)
            ZZ2 = X(3,N4)-X(3,N2)
c incoming vector surface
            A1  = YY1*ZZ2 - YY2*ZZ1
            A2  = XX2*ZZ1 - XX1*ZZ2
            A3  = XX1*YY2 - XX2*YY1
            VOL = A1*(XG-XX0) + A2*(YG-YY0) + A3*(ZG-ZZ0) 
            AREAP32 = (A1*A1+A2*A2+A3*A3)**THREE_OVER_4 
            ALPHA = VOL/MAX(AREAP32,EM20)
#include "lockon.inc"            
            IF(ALPHA >= ZERO .AND. ALPHA >= GAUGE(5,IG))THEN
c the gauge is changing of element
c element associated to the gauge
              LGAUGE(1,IG)=-II
              GAUGE(5,IG)=ALPHA 
            ENDIF
#include "lockoff.inc"
          ENDDO
      ENDDO


      RETURN
      END
Chd|====================================================================
Chd|  AGAUGE0                       source/ale/agauge.F           
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        SPMD_RBCAST                   source/mpi/generic/spmd_rbcast.F
Chd|        SPMD_SD_GAU                   source/mpi/output/spmd_sd_gau.F
Chd|====================================================================
      SUBROUTINE AGAUGE0(LGAUGE ,GAUGE,X ,IXC,IGAUP,NGAUP)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
c     LGAUGE(3,*)
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
c 11:              intantaneous PA
c 12:              intantaneous Rho
c 13:              intantaneous E
c 14:              ! Butterworth !
c 15:              ! Butterworth !
c 16:              ! Butterworth !
c 17:              ! Butterworth !
c 18:              ! Butterworth !
c 19:              ! Butterworth !
c 20:                  ! Butterworth !
c 21:            ! Butterworth !
c 22:            ! Butterworth !
c 23:            ! Butterworth !  
c 24:              ! Butterworth !
c 25:              ! Butterworth !
c 26:              ! Butterworth !
c 27:              ! Butterworth !
c 28:              ! Butterworth !
c 29:              ! Butterworth !
c 30:  Pressure                 filtered Pressure
c 31:  PA                     filtered PA
c 32:  Rho                    filtered Rho
c 33:  E                      filtered E       
c 34:              ! Xpoint      !
c 35:              ! Ypoint      !
c 36:              ! Zpoint      !
c 37:              ! Butterworth !
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXC(NIXC,NUMELC),LGAUGE(3,NBGAUGE),IGAUP(*),NGAUP(*)
      my_real X(3,NUMNOD),GAUGE(LLGAUGE,NBGAUGE)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IG,IS,IGAUGE,N,N1,N2,N3,N4
      my_real XX0,YY0,ZZ0,XX1,YY1,ZZ1,XX2,YY2,ZZ2,A1,A2,A3,
     .        AA,DIST
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IGAUGE=0
      DO IG=1,NBGAUGE
        IF(LGAUGE(1,IG) <= 0) IGAUGE=1
      END DO
      IF(IGAUGE == 0)RETURN      
C      
      DO IG=1,NBGAUGE
        IS = -LGAUGE(1,IG)
C
C all gauges (ale, sph, ..)
        IF(IS >= 0)THEN
          GAUGE(5,IG) = -EP20
          N = LGAUGE(3,IG)
          IF(N > 0)THEN
            GAUGE(2,IG) = X(1,N)
            GAUGE(3,IG) = X(2,N)
            GAUGE(4,IG) = X(3,N)
          ELSEIF(N < 0)THEN
            N=-N
            DIST = GAUGE(1,IG)
            N1 = IXC(2,N)
            N2 = IXC(3,N)
            N3 = IXC(4,N)
            N4 = IXC(5,N)
            XX0 = (X(1,N1)+X(1,N2)+X(1,N3)+X(1,N4))*FOURTH
            YY0 = (X(2,N1)+X(2,N2)+X(2,N3)+X(2,N4))*FOURTH
            ZZ0 = (X(3,N1)+X(3,N2)+X(3,N3)+X(3,N4))*FOURTH
            XX1 = X(1,N3)-X(1,N1)
            YY1 = X(2,N3)-X(2,N1)
            ZZ1 = X(3,N3)-X(3,N1)
            XX2 = X(1,N4)-X(1,N2)
            YY2 = X(2,N4)-X(2,N2)
            ZZ2 = X(3,N4)-X(3,N2)
cc            A1 = YY1*YY2 - ZZ1*ZZ2            
cc            A2 = ZZ1*ZZ2 - XX1*XX2            
cc            A3 = XX1*XX2 - YY1*YY2
            A1  = YY1*ZZ2 - YY2*ZZ1
            A2  = XX2*ZZ1 - XX1*ZZ2
            A3  = XX1*YY2 - XX2*YY1            
            AA = DIST/SQRT(MAX(EM20,A1*A1+A2*A2+A3*A3))
            GAUGE(2,IG) = XX0 + AA*A1
            GAUGE(3,IG) = YY0 + AA*A2
            GAUGE(4,IG) = ZZ0 + AA*A3
          ELSE
            ! point coordinates
            GAUGE(2,IG) = GAUGE(34,IG)
            GAUGE(3,IG) = GAUGE(35,IG)
            GAUGE(4,IG) = GAUGE(36,IG)
          ENDIF
        ENDIF
      END DO
C
       IF(NSPMD > 1 ) THEN 
          CALL SPMD_SD_GAU(GAUGE,IGAUP,NGAUP)
          CALL SPMD_RBCAST(GAUGE,GAUGE,LLGAUGE,NBGAUGE,0,2)
       ENDIF 
C
      RETURN
      END
      
