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
Chd|  SPCLASV                       source/elements/sph/spclasv.F 
Chd|-- called by -----------
Chd|        SPHPREP                       source/elements/sph/sphprep.F 
Chd|-- calls ---------------
Chd|        MYQSORT                       ../common_source/tools/sort/myqsort.F
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_ALLGLOB_ISUM9            source/mpi/generic/spmd_allglob_isum9.F
Chd|        SPMD_SPHGETH                  source/mpi/elements/spmd_sph.F
Chd|        SPHBOX                        share/modules/sphbox.F        
Chd|        SPH_STRUCT_MOD                share/modules/sph_struct_mod.F
Chd|====================================================================
      SUBROUTINE SPCLASV(X      ,SPBUF ,KXSP   ,IXSP   ,NOD2SP   ,
     1                   ISPSYM ,XSPSYM,WSP2SORT,ITASK  ,MYSPATRUE,
     2                   IREDUCE,KREDUCE,LGAUGE ,GAUGE ,ISORTSP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SPHBOX
      USE SPH_STRUCT_MOD
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "bucki_c.inc"
#include      "buckr_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "sphcom.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXSP(NISP,*),IXSP(KVOISPH,*),NOD2SP(*),
     .        ISPSYM(NSPCOND,*),WSP2SORT(*), ITASK, IREDUCE, KREDUCE(*),
     .        LGAUGE(3,*),ISORTSP
C     REAL
      my_real
     .   X(3,*),SPBUF(NSPBUF,*),XSPSYM(3,*), MYSPATRUE, GAUGE(LLGAUGE,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER
     .   N,INOD,JNOD,J,NVOIS,M,NCAND,K1,K2,NVOIS1,NVOIS2,
     .   NVOISS,NVOISS1,NVOISS2, IAUX, IERROR,
     .   K, L, JK, NC, JS, NS, NN, NB,JJ1,JJ2, JJ, JJJ,
     .   MWA(2*KVOISPH),JSTOR(KVOISPH), JPERM(KVOISPH),
     .   LVOIRED, IG
      my_real
     .       DMS,DMS2,DK,
     .       XI,YI,ZI,DI,XJ,YJ,ZJ,DJ,DD,DVOIS(KVOISPH),
     .       DWA(NUMSPH)
      SAVE LVOIRED
      LOGICAL :: SORTING_CONDITION
C-----------------------------------------------
        LVOIRED = 0
        IF(IREDUCE==0)GO TO 100
C-------------------------------------------
C       tri voisins / ne garder que LVOISPH voisins effectifs
C
C       /---------------/
        CALL MY_BARRIER
C       /---------------/
C
        DO NS=ITASK+1,NSP2SORT,NTHREAD
         N=WSP2SORT(NS)
         DWA(N)=ONE
         NVOIS1 =KXSP(4,N)
         NVOISS1=KXSP(6,N)
         IF(KREDUCE(N)/=0.OR.NVOIS1+NVOISS1>LVOISPH)THEN
C
           IF(NVOIS1+NVOISS1>LVOISPH)THEN
             KREDUCE(N)=KREDUCE(N)+10
             LVOIRED = 1
           END IF
C
           INOD=KXSP(3,N)
           XI=X(1,INOD)
           YI=X(2,INOD)
           ZI=X(3,INOD)
           DI=SPBUF(1,N)
           NVOIS=KXSP(5,N)
           NCAND=KXSP(5,N)+KXSP(7,N)
           DO K=1,NVOIS
             JNOD = IXSP(K,N)
             IF(JNOD>0)THEN
               M =NOD2SP(JNOD)
               XJ=X(1,JNOD)
               YJ=X(2,JNOD)
               ZJ=X(3,JNOD)
               DJ=SPBUF(1,M)
             ELSE			 ! cellule remote
               NN = -JNOD
               XJ=XSPHR(3,NN)
               YJ=XSPHR(4,NN)
               ZJ=XSPHR(5,NN)
               DJ=XSPHR(2,NN)
             END IF
             DMS =DI+DJ
             DMS2=DMS*DMS
             DVOIS(K)=(XI-XJ)*(XI-XJ)+(YI-YJ)*(YI-YJ)+(ZI-ZJ)*(ZI-ZJ)
             DVOIS(K)=DVOIS(K)/DMS2
           END DO
           DO K=NVOIS+1,NCAND
             JK = IXSP(K,N)
             IF(JK>0)THEN
               NC=MOD(JK,NSPCOND+1)
               M=JK/(NSPCOND+1)
               JS=ISPSYM(NC,M)
               DJ=SPBUF(1,M)
             ELSE		       ! particule symetrique de particule remote
              NC=MOD(-JK,NSPCOND+1)
              M =-JK/(NSPCOND+1)
              JS=ISPSYMR(NC,M)
              DJ =XSPHR(2,M)
             END IF
             XJ =XSPSYM(1,JS)
             YJ =XSPSYM(2,JS)
             ZJ =XSPSYM(3,JS)
             DMS =DI+DJ
             DMS2=DMS*DMS
             DVOIS(K)=(XI-XJ)*(XI-XJ)+(YI-YJ)*(YI-YJ)+(ZI-ZJ)*(ZI-ZJ)
             DVOIS(K)=DVOIS(K)/DMS2
           END DO
C
           SORTING_CONDITION = (.NOT.(BOOL_SPH_SORT(N)).OR.ISORTSP==0.OR.NVOIS/=NCAND)
           IF(SORTING_CONDITION) THEN
               CALL MYQSORT(NCAND,DVOIS,JPERM,IERROR)
           ELSE
                DO K=1,KXSP(4,N)
                    JPERM(K) = K
                ENDDO
                DO K=1,KXSP(5,N)-KXSP(4,N)+1
                    JPERM(KXSP(4,N)+K) = KXSP(5,N)-K+1
                ENDDO                
           ENDIF

           DO K=1,NCAND
             JSTOR(K) = IXSP(K,N)
           END DO
C
           IF(KREDUCE(N) >= 10)DWA(N)=SQRT(DVOIS(LVOISPH))
C
           K1=0
           K2=0
           DO K=1,NCAND
             JK=JSTOR(JPERM(K))
             IF(JPERM(K) <= NVOIS) THEN
               K1=K1+1
               IXSP(K1,N) = JK
             ELSE
               K2=K2+1
               IXSP(NVOIS+K2,N) = JK
             END IF
           END DO
C
         END IF
        END DO
C-------------------------------------------
C adapte diametre (reduction only) 
C
C       /---------------/
        CALL MY_BARRIER
C       /---------------/
C
        IF(LVOIRED /= 0)THEN
C
          DO NS=ITASK+1,NSP2SORT,NTHREAD
           N=WSP2SORT(NS)
           SPBUF(1,N)=MIN(SPBUF(1,N),DWA(N)*SPBUF(1,N))
           SPBUF(8,N)=SPBUF(1,N)
          END DO
        END IF
C
        IF(NSPMD > 1)THEN
C
C         /---------------/
          CALL MY_BARRIER
C         /---------------/
          IF(ITASK==0) THEN
c            CALL SPMD_GLOB_IMAX9(LVOIRED,1)
            CALL  SPMD_ALLGLOB_ISUM9(LVOIRED,1)
C
C il faut encore echanger SPBUF(1,*)
            IF(LVOIRED /= 0) CALL SPMD_SPHGETH(KXSP ,SPBUF)
          END IF
        END IF
C
C       /---------------/
        CALL MY_BARRIER
C       /---------------/
C
        DO NS=ITASK+1,NSP2SORT,NTHREAD
         N=WSP2SORT(NS)
C
         IF(MOD(KREDUCE(N),10)/=0)THEN
C
           NVOIS1 =KXSP(4,N)
           NVOIS  =KXSP(5,N)
           NVOISS1=KXSP(6,N)
           NVOISS =KXSP(7,N)
           INOD=KXSP(3,N)
           XI=X(1,INOD)
           YI=X(2,INOD)
           ZI=X(3,INOD)
           DI=SPBUF(1,N)
C
C          on est forcement plus proche de la particule vraie que de la particule fantome
           JNOD = IXSP(NVOIS,N)
           IF(JNOD>0)THEN
             M =NOD2SP(JNOD)
             XJ=X(1,JNOD)
             YJ=X(2,JNOD)
             ZJ=X(3,JNOD)
             DJ=SPBUF(1,M)
           ELSE
             NN = -JNOD
             XJ=XSPHR(3,NN)
             YJ=XSPHR(4,NN)
             ZJ=XSPHR(5,NN)
             DJ=XSPHR(2,NN)
           END IF
           DD  =(XI-XJ)*(XI-XJ)+(YI-YJ)*(YI-YJ)+(ZI-ZJ)*(ZI-ZJ)
           DMS =DI+DJ
           DMS2=DMS*DMS
           DK=DD/DMS2
           MYSPATRUE=MAX(ZERO,MIN(MYSPATRUE,DK-ONE))
         END IF
C
       END DO
C-------------------------------------------
 100    CONTINUE
        IF(NSPCOND==0) THEN
          DO N = ITASK+1,NSPHR,NTHREAD
C remise a zero du flag de reperage des cellules actives en spmd multiprocesseurs
            ISPHR(N) = 0
          END DO
        ELSE
C si condition de symetrie alors pas d'optimisation sur particules active
C car particule symetrique de particule inactive eventuellement active
          DO N = ITASK+1,NSPHR,NTHREAD
            ISPHR(N) = 1
          END DO
        END IF        
C       /---------------/
        CALL MY_BARRIER
C       /---------------/
C
        IF(IPARIT/=0)THEN
         DO NS=ITASK+1,NSP2SORT,NTHREAD
          N=WSP2SORT(NS)
          INOD=KXSP(3,N)
          XI=X(1,INOD)
          YI=X(2,INOD)
          ZI=X(3,INOD)
          NCAND=KXSP(5,N)
          NVOIS1=0
          NVOIS2=0
          DO J=1,NCAND
            JNOD=IXSP(J,N)
            IF(JNOD>0)THEN
              M=NOD2SP(JNOD)
              XJ=X(1,JNOD)
              YJ=X(2,JNOD)
              ZJ=X(3,JNOD)
              DD  =(XI-XJ)*(XI-XJ)+(YI-YJ)*(YI-YJ)+(ZI-ZJ)*(ZI-ZJ)
              DMS =SPBUF(1,N)+SPBUF(1,M)
              DMS2=DMS*DMS
              IF (KXSP(2,M)/=0.AND.DD<DMS2) THEN
                NVOIS1=NVOIS1+1
                MWA(NVOIS1)=JNOD
              ELSE
                NVOIS2=NVOIS2+1
                MWA(KVOISPH+NVOIS2)=JNOD
              END IF
            ELSE                        ! cellule remote
              NN = -JNOD
              XJ=XSPHR(3,NN)
              YJ=XSPHR(4,NN)
              ZJ=XSPHR(5,NN)
              DD  =(XI-XJ)*(XI-XJ)+(YI-YJ)*(YI-YJ)+(ZI-ZJ)*(ZI-ZJ)
              DMS =SPBUF(1,N)+XSPHR(2,NN)
              DMS2=DMS*DMS
              IF (NINT(XSPHR(13,NN))/=0.AND.DD<DMS2) THEN
                NVOIS1=NVOIS1+1
                MWA(NVOIS1)=JNOD
                ISPHR(NN) = 1          ! flag reperage cellule active          
              ELSE
                NVOIS2=NVOIS2+1
                MWA(KVOISPH+NVOIS2)=JNOD
              ENDIF
            END IF
          ENDDO
C---------
          KXSP(4,N)=NVOIS1
          DO J=1,NVOIS1
            IXSP(J,N)=MWA(J)
          ENDDO
          DO J=1,NVOIS2
            IXSP(NVOIS1+J,N)=MWA(KVOISPH+J)
          ENDDO
C------------------
C Tri des particules effectives suivant no particule pour conservation Parith/ON
          DO K = 1, NVOIS1
            JK = IXSP(K,N)
            IF(JK>0)THEN
              DVOIS(K) = KXSP(8,NOD2SP(JK))           ! ID particule stoke ds DVOIS
            ELSE
              DVOIS(K) = NINT(XSPHR(6,-JK))
            END IF
          END DO
          CALL MYQSORT(NVOIS1,DVOIS,JPERM,IERROR)
          DO K=1,NVOIS1
            JSTOR(K) = IXSP(K,N)
          END DO
          DO K=1,NVOIS1
            IXSP(K,N) = JSTOR(JPERM(K))
          END DO
         ENDDO
C--------------------------------------------
C      Re-ordonne les particules fantomes.
         DO NS=ITASK+1,NSP2SORT,NTHREAD
          N=WSP2SORT(NS)
          INOD=KXSP(3,N)
          XI =X(1,INOD)
          YI =X(2,INOD)
          ZI =X(3,INOD)
          DI =SPBUF(1,N)
          NVOIS2 =KXSP(5,N)
          NVOISS =KXSP(7,N)
          NVOISS1=0
          NVOISS2=0
          DO K=NVOIS2+1,NVOIS2+NVOISS
            JK=IXSP(K,N)
            IF(JK>0)THEN
             NC=MOD(JK,NSPCOND+1)
             M =JK/(NSPCOND+1)
             JS=ISPSYM(NC,M)
             DJ =SPBUF(1,M)
             XJ =XSPSYM(1,JS)
             YJ =XSPSYM(2,JS)
             ZJ =XSPSYM(3,JS)
             DMS =DI+DJ
             DMS2=DMS*DMS
             DD=(XI-XJ)*(XI-XJ)+(YI-YJ)*(YI-YJ)+(ZI-ZJ)*(ZI-ZJ)
             IF (KXSP(2,M)/=0.AND.DD<DMS2) THEN
              NVOISS1=NVOISS1+1
              MWA(NVOISS1)=JK
             ELSE
              NVOISS2=NVOISS2+1
              MWA(KVOISPH+NVOISS2)=JK
             ENDIF
            ELSE                      ! particule symetrique de particule remote
             NC=MOD(-JK,NSPCOND+1)
             M =-JK/(NSPCOND+1)
             JS=ISPSYMR(NC,M)
             DJ =XSPHR(2,M)
             XJ =XSPSYM(1,JS)
             YJ =XSPSYM(2,JS)
             ZJ =XSPSYM(3,JS)
             DMS =DI+DJ
             DMS2=DMS*DMS
             DD=(XI-XJ)*(XI-XJ)+(YI-YJ)*(YI-YJ)+(ZI-ZJ)*(ZI-ZJ)
             IF (NINT(XSPHR(13,M))/=0.AND.DD<DMS2) THEN
              NVOISS1=NVOISS1+1
              MWA(NVOISS1)=JK
             ELSE
              NVOISS2=NVOISS2+1
              MWA(KVOISPH+NVOISS2)=JK
             ENDIF
            END IF
          ENDDO
          KXSP(6,N)=NVOISS1
          DO J=1,NVOISS1
            IXSP(NVOIS2+J,N)=MWA(J)
          ENDDO
          DO J=1,NVOISS2
            IXSP(NVOIS2+NVOISS1+J,N)=MWA(KVOISPH+J)
          ENDDO
C Tri des particules symetriques suivant no particule pour conservation Parith/ON
          DO K = 1, NVOISS1
            JK = IXSP(NVOIS2+K,N)
            IF(JK>0)THEN
              M=JK/(NSPCOND+1)
              NC=MOD(JK,NSPCOND+1)
              DVOIS(K) = KXSP(8,M)            ! ID particule stoke ds DVOIS
cc              DVOIS(K) = KXSP(8,nod2sp(M))          ! ID particule stoke ds DVOIS
              MWA(K) = NC
            ELSE
              M=-JK/(NSPCOND+1)
              NC=MOD(-JK,NSPCOND+1)
              DVOIS(K) = XSPHR(6,M)
              MWA(K) = NC
            END IF
          END DO
          CALL MYQSORT(NVOISS1,DVOIS,JPERM,IERROR)
          DO K=1,NVOISS1
           JSTOR(K) = IXSP(NVOIS2+K,N)
          END DO
          DO K=1,NVOISS1
           IXSP(NVOIS2+K,N) = JSTOR(JPERM(K))
          END DO
          DO K=1,NVOISS1
           JSTOR(K) = MWA(K)
          END DO
          DO K=1,NVOISS1
           MWA(K) = JSTOR(JPERM(K))
          END DO
          IF(NSPCOND>1) THEN
C Tri des particules symetriques suivant NSPCOND pour un meme no particule pour conservation Parith/ON
            M = NINT(DVOIS(1))
            NB = 1
            DO K = 2, NVOISS1
              IF(NINT(DVOIS(K))/=M) THEN
                IF(NB>1)THEN
                  JJ1 = K-NB
                  JJ2 = K-1
C petit tri bulle
                  DO JJ = JJ1, JJ2-1
                    DO JJJ = JJ+1, JJ2
                      IF(MWA(JJ)>MWA(JJJ))THEN
                        IAUX = MWA(JJ)
                        MWA(JJ) = MWA(JJJ)
                        MWA(JJJ) = IAUX
                        IAUX = IXSP(NVOIS2+JJ,N)
                        IXSP(NVOIS2+JJ,N) = IXSP(NVOIS2+JJJ,N)
                        IXSP(NVOIS2+JJJ,N) = IAUX
                      END IF
                    END DO
                  END DO
                END IF
                M = NINT(DVOIS(K))
                NB = 1
              ELSE
                NB = NB + 1
              END IF 
            END DO
C terminaison
            IF(NB>1)THEN
              JJ1 = NVOISS1-NB+1
              JJ2 = NVOISS1
C petit tri bulle
              DO JJ = JJ1, JJ2-1
                DO JJJ = JJ+1, JJ2
                  IF(MWA(JJ)>MWA(JJJ))THEN
                    IAUX = MWA(JJ)
                    MWA(JJ) = MWA(JJJ)
                    MWA(JJJ) = IAUX
                    IAUX = IXSP(NVOIS2+JJ,N)
                    IXSP(NVOIS2+JJ,N) = IXSP(NVOIS2+JJJ,N)
                    IXSP(NVOIS2+JJJ,N) = IAUX
                  END IF
                END DO
              END DO
            END IF          
          END IF
C fin traitement special parith/on en spmd
         ENDDO
C--------------------------------------------
        ELSE ! IF(IPARIT/=0)THEN
         DO NS=ITASK+1,NSP2SORT,NTHREAD
          N=WSP2SORT(NS)
          INOD=KXSP(3,N)
          XI=X(1,INOD)
          YI=X(2,INOD)
          ZI=X(3,INOD)
          NCAND=KXSP(5,N)
          NVOIS1=0
          NVOIS2=0
          DO J=1,NCAND
            JNOD=IXSP(J,N)
            IF(JNOD>0)THEN
              M=NOD2SP(JNOD)
              XJ=X(1,JNOD)
              YJ=X(2,JNOD)
              ZJ=X(3,JNOD)
              DD  =(XI-XJ)*(XI-XJ)+(YI-YJ)*(YI-YJ)+(ZI-ZJ)*(ZI-ZJ)
              DMS =SPBUF(1,N)+SPBUF(1,M)
              DMS2=DMS*DMS
              IF (KXSP(2,M)/=0.AND.DD<DMS2) THEN
                NVOIS1=NVOIS1+1
                MWA(NVOIS1)=JNOD
              ELSE
                NVOIS2=NVOIS2+1
                MWA(KVOISPH+NVOIS2)=JNOD
              END IF
            ELSE                        ! cellule remote
              NN = -JNOD
              XJ=XSPHR(3,NN)
              YJ=XSPHR(4,NN)
              ZJ=XSPHR(5,NN)
              DD  =(XI-XJ)*(XI-XJ)+(YI-YJ)*(YI-YJ)+(ZI-ZJ)*(ZI-ZJ)
              DMS =SPBUF(1,N)+XSPHR(2,NN)
              DMS2=DMS*DMS
              IF (NINT(XSPHR(13,NN))/=0.AND.DD<DMS2) THEN
                NVOIS1=NVOIS1+1
                MWA(NVOIS1)=JNOD
                ISPHR(NN) = 1          ! flag reperage cellule active          
              ELSE
                NVOIS2=NVOIS2+1
                MWA(KVOISPH+NVOIS2)=JNOD
              ENDIF
            END IF
          ENDDO
C---------
          KXSP(4,N)=NVOIS1
          DO J=1,NVOIS1
            IXSP(J,N)=MWA(J)
          ENDDO
          DO J=1,NVOIS2
            IXSP(NVOIS1+J,N)=MWA(KVOISPH+J)
          ENDDO
         ENDDO ! NS=ITASK+1,NSP2SORT,NTHREAD
C--------------------------------------------
C        Re-ordonne les particules fantomes.
         DO NS=ITASK+1,NSP2SORT,NTHREAD
          N=WSP2SORT(NS)
          INOD=KXSP(3,N)
          XI =X(1,INOD)
          YI =X(2,INOD)
          ZI =X(3,INOD)
          DI =SPBUF(1,N)
          NVOIS2 =KXSP(5,N)
          NVOISS =KXSP(7,N)
          NVOISS1=0
          NVOISS2=0
          DO K=NVOIS2+1,NVOIS2+NVOISS
            JK=IXSP(K,N)
            IF(JK>0)THEN
             NC=MOD(JK,NSPCOND+1)
             M =JK/(NSPCOND+1)
             JS=ISPSYM(NC,M)
             DJ =SPBUF(1,M)
             XJ =XSPSYM(1,JS)
             YJ =XSPSYM(2,JS)
             ZJ =XSPSYM(3,JS)
             DMS =DI+DJ
             DMS2=DMS*DMS
             DD=(XI-XJ)*(XI-XJ)+(YI-YJ)*(YI-YJ)+(ZI-ZJ)*(ZI-ZJ)
             IF (KXSP(2,M)/=0.AND.DD<DMS2) THEN
              NVOISS1=NVOISS1+1
              MWA(NVOISS1)=JK
             ELSE
              NVOISS2=NVOISS2+1
              MWA(KVOISPH+NVOISS2)=JK
             ENDIF
            ELSE                      ! particule symetrique de particule remote
             NC=MOD(-JK,NSPCOND+1)
             M =-JK/(NSPCOND+1)
             JS=ISPSYMR(NC,M)
             DJ =XSPHR(2,M)
             XJ =XSPSYM(1,JS)
             YJ =XSPSYM(2,JS)
             ZJ =XSPSYM(3,JS)
             DMS =DI+DJ
             DMS2=DMS*DMS
             DD=(XI-XJ)*(XI-XJ)+(YI-YJ)*(YI-YJ)+(ZI-ZJ)*(ZI-ZJ)
             IF (NINT(XSPHR(13,M))/=0.AND.DD<DMS2) THEN
              NVOISS1=NVOISS1+1
              MWA(NVOISS1)=JK
             ELSE
              NVOISS2=NVOISS2+1
              MWA(KVOISPH+NVOISS2)=JK
             ENDIF
            END IF
          ENDDO
          KXSP(6,N)=NVOISS1
          DO J=1,NVOISS1
            IXSP(NVOIS2+J,N)=MWA(J)
          ENDDO
          DO J=1,NVOISS2
            IXSP(NVOIS2+NVOISS1+J,N)=MWA(KVOISPH+J)
          ENDDO
         ENDDO ! NS=ITASK+1,NSP2SORT,NTHREAD
        END IF
C-----------------------------------------------
!$OMP DO SCHEDULE(DYNAMIC,1)
        DO IG=1,NBGAUGE
         IF(LGAUGE(1,IG) > -(NUMELS+1))CYCLE
         N=NUMSPH+IG
         XI =GAUGE(2,IG)
         YI =GAUGE(3,IG)
         ZI =GAUGE(4,IG)
         NCAND=KXSP(5,N)
         NVOIS1=0
         NVOIS2=0
         DO J=1,NCAND
           JNOD=IXSP(J,N)
           IF(JNOD>0)THEN
             M=NOD2SP(JNOD)
             XJ=X(1,JNOD)
             YJ=X(2,JNOD)
             ZJ=X(3,JNOD)
             DD  =(XI-XJ)*(XI-XJ)+(YI-YJ)*(YI-YJ)+(ZI-ZJ)*(ZI-ZJ)
             DMS =TWO*SPBUF(1,M)
             DMS2=DMS*DMS
             IF (KXSP(2,M)/=0.AND.DD<DMS2) THEN
               NVOIS1=NVOIS1+1
               MWA(NVOIS1)=JNOD
             ELSE
               NVOIS2=NVOIS2+1
               MWA(KVOISPH+NVOIS2)=JNOD
             END IF
           ELSE                        ! cellule remote
             NN = -JNOD
             XJ=XSPHR(3,NN)
             YJ=XSPHR(4,NN)
             ZJ=XSPHR(5,NN)
             DD  =(XI-XJ)*(XI-XJ)+(YI-YJ)*(YI-YJ)+(ZI-ZJ)*(ZI-ZJ)
             DMS =TWO*XSPHR(2,NN)
             DMS2=DMS*DMS
             IF (NINT(XSPHR(13,NN))/=0.AND.DD<DMS2) THEN
               NVOIS1=NVOIS1+1
               MWA(NVOIS1)=JNOD
               ISPHR(NN) = 1          ! flag reperage cellule active          
             ELSE
               NVOIS2=NVOIS2+1
               MWA(KVOISPH+NVOIS2)=JNOD
             ENDIF
           END IF
         ENDDO
C--------
         KXSP(4,N)=NVOIS1
         DO J=1,NVOIS1
           IXSP(J,N)=MWA(J)
         ENDDO
         DO J=1,NVOIS2
           IXSP(NVOIS1+J,N)=MWA(KVOISPH+J)
         ENDDO
C-------------------------------------------
C        Re-ordonne les particules fantomes.
         NVOIS2 =KXSP(5,N)
         NVOISS =KXSP(7,N)
         NVOISS1=0
         NVOISS2=0
         DO K=NVOIS2+1,NVOIS2+NVOISS
           JK=IXSP(K,N)
           IF(JK>0)THEN
            NC=MOD(JK,NSPCOND+1)
            M =JK/(NSPCOND+1)
            JS=ISPSYM(NC,M)
            DJ =SPBUF(1,M)
            XJ =XSPSYM(1,JS)
            YJ =XSPSYM(2,JS)
            ZJ =XSPSYM(3,JS)
            DMS =TWO*DJ
            DMS2=DMS*DMS
            DD=(XI-XJ)*(XI-XJ)+(YI-YJ)*(YI-YJ)+(ZI-ZJ)*(ZI-ZJ)
            IF (KXSP(2,M)/=0.AND.DD<DMS2) THEN
             NVOISS1=NVOISS1+1
             MWA(NVOISS1)=JK
            ELSE
             NVOISS2=NVOISS2+1
             MWA(KVOISPH+NVOISS2)=JK
            ENDIF
           ELSE                      ! particule symetrique de particule remote
            NC=MOD(-JK,NSPCOND+1)
            M =-JK/(NSPCOND+1)
            JS=ISPSYMR(NC,M)
            DJ =XSPHR(2,M)
            XJ =XSPSYM(1,JS)
            YJ =XSPSYM(2,JS)
            ZJ =XSPSYM(3,JS)
            DMS =TWO*DJ
            DMS2=DMS*DMS
            DD=(XI-XJ)*(XI-XJ)+(YI-YJ)*(YI-YJ)+(ZI-ZJ)*(ZI-ZJ)
            IF (NINT(XSPHR(13,M))/=0.AND.DD<DMS2) THEN
             NVOISS1=NVOISS1+1
             MWA(NVOISS1)=JK
            ELSE
             NVOISS2=NVOISS2+1
             MWA(KVOISPH+NVOISS2)=JK
            ENDIF
           END IF
         ENDDO
         KXSP(6,N)=NVOISS1
         DO J=1,NVOISS1
           IXSP(NVOIS2+J,N)=MWA(J)
         ENDDO
         DO J=1,NVOISS2
           IXSP(NVOIS2+NVOISS1+J,N)=MWA(KVOISPH+J)
         ENDDO
        ENDDO
!$OMP END DO
C-----------------------------------------------
      RETURN
      END
