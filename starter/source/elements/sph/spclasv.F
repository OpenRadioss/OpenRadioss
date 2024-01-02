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
Chd|  SPCLASV                       source/elements/sph/spclasv.F 
Chd|-- called by -----------
Chd|        SPTRI                         source/elements/sph/sptri.F   
Chd|-- calls ---------------
Chd|        MYQSORT                       ../common_source/tools/sort/myqsort.F
Chd|====================================================================
      SUBROUTINE SPCLASV(X      ,SPBUF ,KXSP   ,IXSP   ,NOD2SP    ,
     1                   WASPACT,MYSPATRUE,IREDUCE,KREDUCE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "sphcom.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXSP(NISP,*),IXSP(KVOISPH,*),NOD2SP(*),
     .        WASPACT(*), IREDUCE, KREDUCE(*)
      my_real
     .   X(3,*),SPBUF(NSPBUF,*), MYSPATRUE
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
C-----------------------------------------------
C       force sorting for domain decomposition
        DO NS=1,NSPHACT
         N=WASPACT(NS)
         INOD=KXSP(3,N)
         XI=X(1,INOD)
         YI=X(2,INOD)
         ZI=X(3,INOD)
         DI=SPBUF(1,N)
         NVOIS=KXSP(5,N)
         DO K=1,NVOIS
           JNOD = IXSP(K,N)
           M =NOD2SP(JNOD)
           XJ=X(1,JNOD)
           YJ=X(2,JNOD)
           ZJ=X(3,JNOD)
           DJ=SPBUF(1,M)
           DMS =DI+DJ
           DMS2=DMS*DMS
           DVOIS(K)=(XI-XJ)*(XI-XJ)+(YI-YJ)*(YI-YJ)+(ZI-ZJ)*(ZI-ZJ)
           DVOIS(K)=DVOIS(K)/DMS2
         END DO
C
         CALL MYQSORT(NVOIS,DVOIS,JPERM,IERROR)
         DO K=1,NVOIS
           JSTOR(K) = IXSP(K,N)
         END DO
C
         K1=0
         DO K=1,NVOIS
           JK=JSTOR(JPERM(K))
           K1=K1+1
           IXSP(K1,N) = JK
         END DO
C
C--- Closest neighbor is stored for tensile instability treatment --
C
         IF (NSPBUF==13) THEN
           SPBUF(13,N)=TWO*SQRT(DVOIS(1))
         ENDIF
C
        END DO
C-----------------------------------------------
        LVOIRED = 0
        IF(IREDUCE==0)GO TO 100
C-------------------------------------------
C       tri voisins / ne garder que LVOISPH voisins effectifs
C
        DO NS=1,NSPHACT
         N=WASPACT(NS)
         DWA(N)=ONE
         NVOIS1 =KXSP(4,N)
         IF(KREDUCE(N)/=0.OR.NVOIS1>LVOISPH)THEN
C
           IF(NVOIS1>LVOISPH)THEN
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
           DO K=1,NVOIS
             JNOD = IXSP(K,N)
             M =NOD2SP(JNOD)
             XJ=X(1,JNOD)
             YJ=X(2,JNOD)
             ZJ=X(3,JNOD)
             DJ=SPBUF(1,M)
             DMS =DI+DJ
             DMS2=DMS*DMS
             DVOIS(K)=(XI-XJ)*(XI-XJ)+(YI-YJ)*(YI-YJ)+(ZI-ZJ)*(ZI-ZJ)
             DVOIS(K)=DVOIS(K)/DMS2
           END DO
C
           CALL MYQSORT(NVOIS,DVOIS,JPERM,IERROR)
           DO K=1,NVOIS
             JSTOR(K) = IXSP(K,N)
           END DO
C
           IF(KREDUCE(N) >= 10)DWA(N)=SQRT(DVOIS(LVOISPH))
C
           K1=0
           DO K=1,NVOIS
             JK=JSTOR(JPERM(K))
             K1=K1+1
             IXSP(K1,N) = JK
           END DO
C
         END IF
        END DO
C-------------------------------------------
C adapte diametre (reduction only) 
C
        IF(LVOIRED /= 0)THEN
C
          DO NS=1,NSPHACT
           N=WASPACT(NS)
           SPBUF(1,N)=MIN(SPBUF(1,N),DWA(N)*SPBUF(1,N))
           SPBUF(8,N)=SPBUF(1,N)
          END DO
        END IF
C
        DO NS=1,NSPHACT
         N=WASPACT(NS)
C
         IF(MOD(KREDUCE(N),10)/=0)THEN
C
           NVOIS1 =KXSP(4,N)
           NVOIS  =KXSP(5,N)
           INOD=KXSP(3,N)
           XI=X(1,INOD)
           YI=X(2,INOD)
           ZI=X(3,INOD)
           DI=SPBUF(1,N)
C
           JNOD = IXSP(NVOIS,N)
           M =NOD2SP(JNOD)
           XJ=X(1,JNOD)
           YJ=X(2,JNOD)
           ZJ=X(3,JNOD)
           DJ=SPBUF(1,M)
           DD  =(XI-XJ)*(XI-XJ)+(YI-YJ)*(YI-YJ)+(ZI-ZJ)*(ZI-ZJ)
           DMS =DI+DJ
           DMS2=DMS*DMS
           DK=DD/DMS2
           MYSPATRUE=MAX(ZERO,MIN(MYSPATRUE,DK-ONE))
         END IF
C
       END DO
C-------------------------------------------
 100   CONTINUE
C-----------------------------------------------
      RETURN
      END
