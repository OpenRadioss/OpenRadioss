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
Chd|  AGAUGE                        source/ale/agauge.F           
Chd|-- called by -----------
Chd|        ALETHE                        source/ale/alethe.F           
Chd|-- calls ---------------
Chd|        AGAUG3                        source/ale/agauge.F           
Chd|        AGAUG30                       source/ale/agauge.F           
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_E1VOIS                   source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_RBCAST                   source/mpi/generic/spmd_rbcast.F
Chd|        SPMD_SD_GAUG                  source/mpi/output/spmd_sd_gaug.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE AGAUGE(
     1  IPARG   ,ELBUF_STR,PHI     ,IXS     ,IXQ     ,
     2  X       ,ALE_CONNECT    ,ITASK   ,NERCVOIS,NESDVOIS,
     3  LERCVOIS,LESDVOIS ,LENCOM  ,LGAUGE  ,
     4  GAUGE   ,V        ,IGAUP   ,NGAUP   )
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
#include      "scr01_c.inc"
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "conv_c.inc"
#include      "spmd_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*), ITASK, LENCOM,
     .        NERCVOIS(*),NESDVOIS(*),LERCVOIS(*),LESDVOIS(*),
     .        IXS(NIXS,*),IXQ(NIXQ,*),LGAUGE(3,*),IGAUP(*),NGAUP(*)   
C     REAL
      my_real
     .   PHI(*),GAUGE(LLGAUGE,*),X(3,*),V(3,*)
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_STR
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NG, JMUL, IADR, I, II,J,JJ(6), K,IG,IS,IGAUGE,N,IG0,GELE,
     .        ITAG(NBGAUGE),NEL,IAD2
      my_real
     .   P,RHO,E,PA,U2,ALPHA(NBGAUGE)
      TYPE(G_BUFEL_) ,POINTER :: GBUF
C-----------------------------------------------
c     GAUGE(3,*)
c 1:  -Isolid				   -(NUMELS_G+1) if SPH gauge
c 2:  GaugeId
c 3:  +Node or -Shell
c
c     => GAUGE(LLGAUGE,*), LLGAUGE = 37
c 1:  Dist (distance from Shell)	   Dist (distance from Shell)
c 2:  XG				   XG
c 3:  YG				   YG
c 4:  ZG				   ZG
c 5:  Alpha (Solid penetration ratio)	   not yet used
c 6:    				   XSAV (SPH sorting)
c 7:    				   YSAV (SPH sorting)
c 8:    				   ZSAV (SPH sorting)
c 9:    				   FF (sph only)
c 10:   				   intantaneous Pressure
c 11:   				   intantaneous PA
c 12:   				   intantaneous Rho
c 13:   				   intantaneous E
c 14:   				   ! Butterworth !
c 15:   				   ! Butterworth !
c 16:   				   ! Butterworth !
c 17:   				   ! Butterworth !
c 18:   				   ! Butterworth !
c 19:   				   ! Butterworth !
c 20:   			           ! Butterworth !
c 21: 				           ! Butterworth !
c 22: 				           ! Butterworth !
c 23: 				           ! Butterworth !  
c 24:   				   ! Butterworth !
c 25:   				   ! Butterworth !
c 26:   				   ! Butterworth !
c 27:   				   ! Butterworth !
c 28:   				   ! Butterworth !
c 29:   				   ! Butterworth !
c 30:  Pressure      		  	   filtered Pressure
c 31:  PA            			   filtered PA
c 32:  Rho           			   filtered Rho
c 33:  E             			   filtered E	     
c 34:   				   ! Butterworth !
c 35:   				   ! Butterworth !
c 36:   				   ! Butterworth !
c 37:   				   ! Butterworth !
C-----------------------------------------------
      IGAUGE=0
      DO IG=1,NBGAUGE
        IF(LGAUGE(1,IG) <= 0 .AND. LGAUGE(1,IG) >= -NUMELS) IGAUGE=1
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
        IF(IPARG(5,NG).NE.1) CYCLE
        CALL INITBUF (IPARG    ,NG      ,
     2        MTN     ,LLT     ,NFT     ,IADR    ,ITY     ,
     3        NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,
     4        JTHE    ,JLAG    ,JMUL    ,JHBE    ,JIVF    ,
     5        NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,
     6        IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,
     7        ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
        IF (IPARG(8,NG).EQ.1)       CYCLE
        LFT=1
        CALL AGAUG30(LGAUGE,GAUGE,IXS ,X  )
       ENDDO
       CALL MY_BARRIER 
      ENDIF
C
      IF(ITASK==0)THEN
        DO I=1,MAX(NUMELS,NUMELQ)
          PHI(I)=ZERO
        END DO
C
        DO IG=1,NBGAUGE
          IS = -LGAUGE(1,IG)
          IF(IS > 0 .AND. IS <= NUMELS)THEN
               PHI(IS)= IG 
C
Csm            PHI(IS)=I
c taux de penetration dans l'element (initialisation)             
            GAUGE(5,IG)=ZERO
          ENDIF
        END DO
      ENDIF
C
      CALL MY_BARRIER
C-----------------------------
C       ECHANGES SPMD IGAUGE SUR ELTS VOISINS
C-----------------------------
      IF (NSPMD.GT.1) THEN
C
C Partie non parallele smt
C
!$OMP SINGLE
        CALL SPMD_E1VOIS(PHI     ,NERCVOIS,NESDVOIS,LERCVOIS,
     2                     LESDVOIS,LENCOM  )
C Fin Partie non parallele smt
!$OMP END SINGLE
      ENDIF
C-----------------------------
C       Recherche des nouveaux elements  
C-----------------------------
      DO NG=ITASK+1,NGROUP,NTHREAD
        IF(IPARG(5,NG).NE.1) CYCLE
        CALL INITBUF (IPARG    ,NG      ,
     2        MTN     ,LLT     ,NFT     ,IADR    ,ITY     ,
     3        NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,
     4        JTHE    ,JLAG    ,JMUL    ,JHBE    ,JIVF    ,
     5        NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,
     6        IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,
     7        ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
        IF(IPARG(8,NG).EQ.1)       CYCLE
         
        LFT=1    
        CALL AGAUG3(LGAUGE,GAUGE,PHI,IXS  ,X  ,ALE_CONNECT )
      END DO
C      
       CALL MY_BARRIER
       IF(ITASK==0)THEN
        DO I=1,MAX(NUMELS,NUMELQ)
          PHI(I)=ZERO
        END DO
C
C  ce tableau est utilis    dans le cas ou plusieurs jauges sont
C       sur le meme elts.         
c
        DO IG= 1,NBGAUGE
          ITAG(IG) = 0
        ENDDO
C
        DO IG=1,NBGAUGE
          IS = -LGAUGE(1,IG)
          IF(IS > 0 .AND. IS <= NUMELS)THEN
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
C       ECHANGES SPMD IGAUGE SUR ELTS VOISINS
C-----------------------------
      IF (NSPMD.GT.1) THEN
C
C Partie non parallele smt
C
!$OMP SINGLE
        CALL SPMD_E1VOIS(PHI     ,NERCVOIS,NESDVOIS,LERCVOIS,
     2                     LESDVOIS,LENCOM  )
C Fin Partie non parallele smt
!$OMP END SINGLE
      ENDIF  
C-----------------------------
C     Calcul des mesures de jauge
C-----------------------------
      DO NG=ITASK+1,NGROUP,NTHREAD
C-----------------------------
        IF(IPARG(5,NG).NE.1) CYCLE
        CALL INITBUF (IPARG    ,NG      ,
     2        MTN     ,LLT     ,NFT     ,IADR    ,ITY     ,
     3        NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,
     4        JTHE    ,JLAG    ,JMUL    ,JHBE    ,JIVF    ,
     5        NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,
     6        IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,
     7        ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
         IF (IPARG(8,NG).EQ.1) CYCLE
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
          RHO = GBUF%RHO(I) /GBUF%VOL(I)
          E   = GBUF%EINT(I)/GBUF%VOL(I)
          U2 = ZERO
          DO J=2,9
            N = IXS(J,II)
            U2 = U2+ V(1,N)*V(1,N)
            U2 = U2+ V(2,N)*V(2,N)
            U2 = U2+ V(3,N)*V(3,N)
          ENDDO
          PA = P - RHO*U2/SIXTEEN
#include "lockon.inc"
          GAUGE(30,IG)= -P
          GAUGE(31,IG)= -PA
          GAUGE(32,IG)= RHO
          GAUGE(33,IG)= E
#include "lockoff.inc"
          END DO ! I 
      ENDDO ! NG=ITASK+1,NGROUP,NTASK

cad     ATTENTION   
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
      SUBROUTINE AGAUG30(LGAUGE,GAUGE,IXS  ,X )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr01_c.inc"
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "conv_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),LGAUGE(3,*)
C     REAL
      my_real
     .   X(3,*),GAUGE(LLGAUGE,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,J,N,IG,IGAUGE
      my_real
     .   XX1,YY1,ZZ1,XX2,YY2,ZZ2,XG,YG,ZG
c--------------------------------------------------------------
c  recherche element associe a la jauge
c     algo quadratique (numels*nbgauge) au temps 0
c     a ameliorer eventuellement
c--------------------------------------------------------------
      DO I=1,LLT
          II=I+NFT
          XX1 = EP30
          YY1 = EP30
          ZZ1 = EP30
          XX2 = -EP30
          YY2 = -EP30
          ZZ2 = -EP30
          DO J=2,9
            XX1 = MIN(XX1,X(1,IXS(J,II)))
            YY1 = MIN(YY1,X(2,IXS(J,II)))
            ZZ1 = MIN(ZZ1,X(3,IXS(J,II)))
            XX2 = MAX(XX2,X(1,IXS(J,II)))
            YY2 = MAX(YY2,X(2,IXS(J,II)))
            ZZ2 = MAX(ZZ2,X(3,IXS(J,II)))
          ENDDO
C          
          IGAUGE = 0 
          DO IG=1,NBGAUGE
           IF(LGAUGE(1,IG) > 0 .OR. LGAUGE(1,IG) < -NUMELS) CYCLE
           XG  = GAUGE(2,IG)
           IF(XG < XX1)CYCLE
           IF(XG > XX2)CYCLE
           YG  = GAUGE(3,IG)
           IF(YG < YY1)CYCLE
           IF(YG > YY2)CYCLE
           ZG  = GAUGE(4,IG)
           IF(ZG < ZZ1)CYCLE
           IF(ZG > ZZ2)CYCLE
            IGAUGE=IG
          IF(IGAUGE == 0)CYCLE
c element associe a la jauge
c en cas de solutions multiples il n'est pas necessaire de choisir 
c la meilleure; AGAUG3 corrigera.           
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
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
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
#include      "scr01_c.inc"
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "conv_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),LGAUGE(3,*)
C     REAL
      my_real
     .   PHI(*),X(3,*),GAUGE(LLGAUGE,*)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,J,N,N1,N2,N3,N4,IG,IGAUGE,IAD2
      INTEGER ICONT,INDX(NBGAUGE),NINDX,IP
      INTEGER IFACE(4,6)
      my_real
     .   ALPHA,XX0,YY0,ZZ0,XX1,YY1,ZZ1,XX2,YY2,ZZ2,A1,A2,A3,
     .   VOL,AREAP32,XG,YG,ZG
      DATA IFACE/ 1, 2, 3, 4,
     2            4, 3, 7, 8,
     3            8, 7, 6, 5,
     4            5, 6, 2, 1,
     5            2, 6, 7, 3,
     5            1, 4, 8, 5/
c--------------------------------------------------------------
c  recherche du nouvel element associe a la jauge
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
c surface vectorielle entrante
cc            A1 = YY1*YY2 - ZZ1*ZZ2            
cc            A2 = ZZ1*ZZ2 - XX1*XX2            
cc            A3 = XX1*XX2 - YY1*YY2
            A1  = YY1*ZZ2 - YY2*ZZ1
            A2  = XX2*ZZ1 - XX1*ZZ2
            A3  = XX1*YY2 - XX2*YY1
            VOL = A1*(XG-XX0) + A2*(YG-YY0) + A3*(ZG-ZZ0) 
            AREAP32 = (A1*A1+A2*A2+A3*A3)**THREE_OVER_4 
            ALPHA = VOL/MAX(AREAP32,EM20)
#include "lockon.inc"            
            IF(ALPHA >= ZERO .AND. ALPHA >= GAUGE(5,IG))THEN
c la jauge change d'element
c element associe a la jauge
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
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr01_c.inc"
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "conv_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXC(NIXC,*),LGAUGE(3,*),IGAUP(*),NGAUP(*)
C     REAL
      my_real
     .   X(3,*),GAUGE(LLGAUGE,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, IG,IS,IGAUGE,N,N1,N2,N3,N4
      my_real
     .   XX0,YY0,ZZ0,XX1,YY1,ZZ1,XX2,YY2,ZZ2,A1,A2,A3,
     .   AA,XG,YG,ZG,DIST
C-----------------------------------------------
c     GAUGE(3,*)
c 1:  -Isolid				   -(NUMELS_G+1) if SPH gauge
c 2:  GaugeId
c 3:  +Node or -Shell
c
c     => GAUGE(LLGAUGE,*), LLGAUGE = 37
c 1:  Dist (distance from Shell)	   Dist (distance from Shell)
c 2:  XG				   XG
c 3:  YG				   YG
c 4:  ZG				   ZG
c 5:  Alpha (Solid penetration ratio)	   not yet used
c 6:    				   XSAV (SPH sorting)
c 7:    				   YSAV (SPH sorting)
c 8:    				   ZSAV (SPH sorting)
c 9:    				   FF (sph only)
c 10:   				   intantaneous Pressure
c 11:   				   intantaneous PA
c 12:   				   intantaneous Rho
c 13:   				   intantaneous E
c 14:   				   ! Butterworth !
c 15:   				   ! Butterworth !
c 16:   				   ! Butterworth !
c 17:   				   ! Butterworth !
c 18:   				   ! Butterworth !
c 19:   				   ! Butterworth !
c 20:   			           ! Butterworth !
c 21: 				           ! Butterworth !
c 22: 				           ! Butterworth !
c 23: 				           ! Butterworth !  
c 24:   				   ! Butterworth !
c 25:   				   ! Butterworth !
c 26:   				   ! Butterworth !
c 27:   				   ! Butterworth !
c 28:   				   ! Butterworth !
c 29:   				   ! Butterworth !
c 30:  Pressure      		  	   filtered Pressure
c 31:  PA            			   filtered PA
c 32:  Rho           			   filtered Rho
c 33:  E             			   filtered E	     
c 34:   				   ! Butterworth !
c 35:   				   ! Butterworth !
c 36:   				   ! Butterworth !
c 37:   				   ! Butterworth !
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
      
