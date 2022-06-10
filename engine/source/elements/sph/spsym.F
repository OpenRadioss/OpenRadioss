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
Chd|  SPSYMP                        source/elements/sph/spsym.F   
Chd|-- called by -----------
Chd|        SPHPREP                       source/elements/sph/sphprep.F 
Chd|-- calls ---------------
Chd|        MYQSORT                       ../common_source/tools/sort/myqsort.F
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SPHBOX                        share/modules/sphbox.F        
Chd|====================================================================
      SUBROUTINE SPSYMP(
     1    X       ,V        ,MS      ,SPBUF   ,ITAB    ,
     2    KXSP    ,IXSP     ,NOD2SP  ,ISPCOND ,ISPSYM  ,
     3    XFRAME  ,XSPSYM   ,VSPSYM  ,IREDUCE ,
     4    WSP2SORT ,MYSPATRUE,DMAX    ,ITASK   ,KREDUCE ,
     5    LGAUGE  ,GAUGE   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SPHBOX
      USE MESSAGE_MOD
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "sphcom.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "scr18_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXSP(NISP,*),IXSP(KVOISPH,*),NOD2SP(*),ITAB(*),
     .        ISPCOND(NISPCOND,*),ISPSYM(NSPCOND,*),
     .        IREDUCE, WSP2SORT(*), ITASK, KREDUCE(*),
     .        LGAUGE(3,*)
C     REAL
      my_real
     .   X(3,*) ,V(3,*) ,MS(*) ,SPBUF(NSPBUF,*) ,
     .   XFRAME(NXFRAME,*) ,
     .   XSPSYM(3,*) ,VSPSYM(3,*),
     .   MYSPATRUE, DMAX, MYSPATRUE2, GAUGE(LLGAUGE,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K,N,INOD,JNOD,J,NVOIS,M,SM,JS,I,NN,IG,
     .        NVOIS1,NVOIS2,NVOISS,NVOISS1,NVOISS2,
     .        IS,IC,NC,L,ISLIDE, IAUX, NSPHSYM_L,
     .        JK,JL,IERROR,NS,NB,JJ1,JJ2, JJ, JJJ,
     .        JVOIS(NSPHSYM+KVOISPH+1), JSTOR(NSPHSYM+KVOISPH+1),
     .        JPERM(NSPHSYM+KVOISPH+1), JCOND(KVOISPH)
      my_real
     .       XI,YI,ZI,DI,RHOI,XJ,YJ,ZJ,DJ,RHOJ,DIJ,
     .       VXI,VYI,VZI,VXJ,VYJ,VZJ,
     .       OX,OY,OZ,NX,NY,NZ,
     .       XS,YS,ZS,VXS,VYS,VZS,VN,DD,DM,DK,DL,
     .       XISORT,YISORT,ZISORT,DISORT,
     .       XJSORT,YJSORT,ZJSORT,DJSORT,
     .       SPALINR, DVOIS(NSPHSYM+KVOISPH+1)
C-----------------------------------------------
C     new construction of ghost particles is necessary.
      SPALINR=SQRT(ONE + MYSPATRUE)

c     save MYSPATRUE value
      MYSPATRUE2 = MYSPATRUE
C      NSPHSYM=0 initialise dans sphprep

      DO NC=1,NSPCOND
        IS=ISPCOND(3,NC)
        IC=ISPCOND(2,NC)
        ISLIDE=ISPCOND(5,NC)
        OX=XFRAME(10,IS)
        OY=XFRAME(11,IS)
        OZ=XFRAME(12,IS)
        NX=XFRAME(3*(IC-1)+1,IS)
        NY=XFRAME(3*(IC-1)+2,IS)
        NZ=XFRAME(3*(IC-1)+3,IS)
        DO NS=1+ITASK,NSP2SORT,NTHREAD
         N=WSP2SORT(NS)
         INOD  =KXSP(3,N)
         XI =X(1,INOD)
         YI =X(2,INOD)
         ZI =X(3,INOD)
         VXI=V(1,INOD)
         VYI=V(2,INOD)
         VZI=V(3,INOD)
         DD=(XI-OX)*NX+(YI-OY)*NY+(ZI-OZ)*NZ
         IF (ISPSYM(NC,N).NE.-1) THEN
           NSPHSYM_L = ISPSYM(NC,N)
           XS=XI - TWO*DD*NX
           YS=YI - TWO*DD*NY
           ZS=ZI - TWO*DD*NZ	   
           IF(ISLIDE.EQ.0)THEN
            VXS=-VXI
            VYS=-VYI
            VZS=-VZI
           ELSE
            VN=VXI*NX+VYI*NY+VZI*NZ
            VXS=VXI  - TWO*VN*NX
            VYS=VYI - TWO*VN*NY
            VZS=VZI - TWO*VN*NZ	    
           ENDIF
           XSPSYM(1,NSPHSYM_L)= XS
           XSPSYM(2,NSPHSYM_L)= YS
           XSPSYM(3,NSPHSYM_L)= ZS
           VSPSYM(1,NSPHSYM_L)=VXS
           VSPSYM(2,NSPHSYM_L)=VYS
           VSPSYM(3,NSPHSYM_L)=VZS
         ENDIF
        ENDDO
C
C Particules symetriques de particules remotes
C
        DO NS = ITASK+1,NSPHR,NTHREAD
         XI =XSPHR(3,NS)
         YI =XSPHR(4,NS)
         ZI =XSPHR(5,NS)
         VXI=XSPHR(9,NS)
         VYI=XSPHR(10,NS)
         VZI=XSPHR(11,NS)
         DD=(XI-OX)*NX+(YI-OY)*NY+(ZI-OZ)*NZ
         IF (ISPSYMR(NC,NS).NE.-1) THEN
           NSPHSYM_L = ISPSYMR(NC,NS)
           XS=XI - TWO*DD*NX
           YS=YI - TWO*DD*NY
           ZS=ZI - TWO*DD*NZ	   
           IF(ISLIDE.EQ.0)THEN
            VXS=-VXI
            VYS=-VYI
            VZS=-VZI
           ELSE
            VN=VXI*NX+VYI*NY+VZI*NZ
            VXS=VXI  - TWO*VN*NX
            VYS=VYI - TWO*VN*NY
            VZS=VZI - TWO*VN*NZ	    
           ENDIF
           XSPSYM(1,NSPHSYM_L)= XS
           XSPSYM(2,NSPHSYM_L)= YS
           XSPSYM(3,NSPHSYM_L)= ZS
           VSPSYM(1,NSPHSYM_L)=VXS
           VSPSYM(2,NSPHSYM_L)=VYS
           VSPSYM(3,NSPHSYM_L)=VZS
         END IF
        END DO
      END DO
C
C Synchro sur ISPSYM & ISPSYMR 
C    /---------------/
      CALL MY_BARRIER
C    /---------------/

C-------------------------------------------
C     searching for candidate neighbours among ghost particles.
C-------------------------------------------
      IF (NSPCOND.NE.0)THEN

       DO NS=ITASK+1,NSP2SORT,NTHREAD
        N=WSP2SORT(NS)
        INOD=KXSP(3,N)
        XI =X(1,INOD)
        YI =X(2,INOD)
        ZI =X(3,INOD)
        DI =SPBUF(1,N)
C------
        NVOIS2   =KXSP(5,N)
        KXSP(7,N)=0
C------
        DO NC=1,NSPCOND
         JS=ISPSYM(NC,N)
         IF(JS.GT.0)THEN
          XJ =XSPSYM(1,JS)
          YJ =XSPSYM(2,JS)
          ZJ =XSPSYM(3,JS)
          DIJ=DI+DI
          DIJ=DIJ*DIJ
          DD=(XI-XJ)*(XI-XJ)+(YI-YJ)*(YI-YJ)+(ZI-ZJ)*(ZI-ZJ)
          IF(DD.LE.(ONE+MYSPATRUE2)*DIJ) THEN
           NVOISS=KXSP(7,N)
           NVOISS=NVOISS+1
           JVOIS(NVOIS2+NVOISS)=NC+N*(NSPCOND+1)
           DVOIS(NVOIS2+NVOISS)=DD/DIJ
           KXSP(7,N)=NVOISS
          ENDIF
         ENDIF
        ENDDO
C------
        DO I=1,NVOIS2
         JNOD=IXSP(I,N)
         IF(JNOD.GT.0)THEN          ! particule interne       
          M=NOD2SP(JNOD)
          DJ =SPBUF(1,M)
          DIJ=(DI+DJ)
          DIJ=DIJ*DIJ
          DO NC=1,NSPCOND
           JS=ISPSYM(NC,M)
           IF(JS.GT.0)THEN
            XJ =XSPSYM(1,JS)
            YJ =XSPSYM(2,JS)
            ZJ =XSPSYM(3,JS)
            DD=(XI-XJ)*(XI-XJ)+(YI-YJ)*(YI-YJ)+(ZI-ZJ)*(ZI-ZJ)
            IF(DD.LE.(ONE+MYSPATRUE2)*DIJ) THEN
             NVOISS=KXSP(7,N)
             NVOISS=NVOISS+1
             JVOIS(NVOIS2+NVOISS)=NC+M*(NSPCOND+1)
             DVOIS(NVOIS2+NVOISS)=DD/DIJ
             KXSP(7,N)=NVOISS
            END IF
           END IF
          END DO
         ELSE                    ! particule remote
          NN = -JNOD
          DJ =XSPHR(2,NN)
          DIJ=(DI+DJ)
          DIJ=DIJ*DIJ
          DO NC=1,NSPCOND
           JS=ISPSYMR(NC,NN)
           IF(JS.GT.0)THEN
            XJ =XSPSYM(1,JS)
            YJ =XSPSYM(2,JS)
            ZJ =XSPSYM(3,JS)
            DD=(XI-XJ)*(XI-XJ)+(YI-YJ)*(YI-YJ)+(ZI-ZJ)*(ZI-ZJ)
            IF(DD.LE.(ONE+MYSPATRUE2)*DIJ) THEN
             NVOISS=KXSP(7,N)
             NVOISS=NVOISS+1
             JVOIS(NVOIS2+NVOISS)=-NC-NN*(NSPCOND+1)           ! reperage particule sym remote
C             JVOIS(NVOIS2+NVOISS)=NC+M*(NSPCOND+1)
C 2 choix : NC + (NN+NUMSPH)*(NSPCOND+1) : on retrouve ensuite que NN > NUMSPH
C           -NC-NN*(NSPCOND+1) : on sait que l'on est sur du remote car -
             DVOIS(NVOIS2+NVOISS)=DD/DIJ
             KXSP(7,N)=NVOISS
            END IF
           END IF
          END DO
         END IF
        END DO
        NVOISS=KXSP(7,N)
        IF(NVOIS2+NVOISS.LE.KVOISPH)THEN
C--------------------------------------------------
C        ghost particles are added to real ones.
C        (ordonne les particules fantomes).
         NVOISS1=0
         NVOISS2=NVOISS
         DO K=NVOIS2+1,NVOIS2+NVOISS
          DK=DVOIS(K)
          JK=JVOIS(K)
          IF(DK.LT.ONE)THEN
           NVOISS1=NVOISS1+1
           IXSP(NVOIS2+NVOISS1,N)=JK
          ELSE
           IXSP(NVOIS2+NVOISS2,N)=JK
           NVOISS2=NVOISS2-1
          ENDIF
         ENDDO
         KXSP(6,N)=NVOISS1
        ELSE
C--------------------------------------------------
C        memory space needs to reduce the number of stored neighbours.
         IREDUCE=1
         KREDUCE(N)=1
C-------
C        preparing all real neighbours for sort.
         DO J=1,NVOIS2
          JNOD=IXSP(J,N)
          IF(JNOD.GT.0)THEN               ! particule interne  
            XJ =X(1,JNOD)
            YJ =X(2,JNOD)
            ZJ =X(3,JNOD)
            M =NOD2SP(JNOD)
            DJ=SPBUF(1,M)
            DIJ=DI+DJ
            DIJ=DIJ*DIJ
            DD=(XI-XJ)*(XI-XJ)+(YI-YJ)*(YI-YJ)+(ZI-ZJ)*(ZI-ZJ)
            DVOIS(J)=DD/DIJ
            JVOIS(J)=M*(NSPCOND+1)
          ELSE                           ! particule remote
            NN = -JNOD
            XJ = XSPHR(3,NN)
            YJ = XSPHR(4,NN)
            ZJ = XSPHR(5,NN)
            DJ = XSPHR(2,NN)
            DIJ=DI+DJ
            DIJ=DIJ*DIJ
            DD=(XI-XJ)*(XI-XJ)+(YI-YJ)*(YI-YJ)+(ZI-ZJ)*(ZI-ZJ)
            DVOIS(J)=DD/DIJ
            JVOIS(J)=-NN*(NSPCOND+1)
          END IF
         ENDDO
C-------
         CALL MYQSORT(NVOIS2+NVOISS,DVOIS,JPERM,IERROR)
         DO K=1,NVOIS2+NVOISS
          JSTOR(K)=JVOIS(K)
         ENDDO
         DO K=1,KVOISPH
          JVOIS(K)=JSTOR(JPERM(K))
         ENDDO
C-------
C        adapting bucket sort true security coefficient ; 
C        looking if non-zero weight neighbours were lost.
         DK=DVOIS(KVOISPH)
C Choix des cellules a conserver tq distance < DK pour eviter pb de parith/on
	 NVOIS=0
	 DO K=1,KVOISPH
	   IF(DVOIS(K).LT.DK)THEN
	     NVOIS=NVOIS+1
	   END IF
	 END DO
C-------
C        Re-ordonne les voisins (particules reelles puis particules fantomes).
         NVOIS1=0
c         DO K=1,KVOISPH
         DO K=1,NVOIS
          JK=JVOIS(K)
          DK=DVOIS(K)
          IF(JK.GT.0)THEN            ! particule interne
            NC=MOD(JK,NSPCOND+1)
            IF(NC.EQ.0.AND.DK.LT.ONE)THEN
              NVOIS1=NVOIS1+1
              IXSP(NVOIS1,N)=KXSP(3,JK/(NSPCOND+1))
            ENDIF
          ELSE                      ! particule remote
            JK = -JK
            NC=MOD(JK,NSPCOND+1)
            IF(NC.EQ.0.AND.DK.LT.ONE)THEN
              NVOIS1=NVOIS1+1
              IXSP(NVOIS1,N)=-JK/(NSPCOND+1)
            END IF
          END IF
         END DO
         KXSP(4,N)=NVOIS1
         NVOIS2=NVOIS1
C
         DO K=1,NVOIS
          JK=JVOIS(K)
          DK=DVOIS(K)
          IF(JK.GT.0)THEN            ! particule interne
            NC=MOD(JK,NSPCOND+1)
            IF(NC.EQ.0.AND.DK.GE.ONE)THEN
              NVOIS2=NVOIS2+1
              IXSP(NVOIS2,N)=KXSP(3,JK/(NSPCOND+1))
            END IF
          ELSE                      ! particule remote
            JK = -JK
            NC=MOD(JK,NSPCOND+1)
            IF(NC.EQ.0.AND.DK.GE.ONE)THEN
              NVOIS2=NVOIS2+1
              IXSP(NVOIS2,N)=-JK/(NSPCOND+1)
            END IF
          END IF
         END DO
         KXSP(5,N)=NVOIS2
C--------
C        ordonne les particules fantomes.
         NVOISS1=0
c         DO K=1,KVOISPH
         DO K=1,NVOIS
          JK=JVOIS(K)
          DK=DVOIS(K)
          IF(JK.GT.0)THEN            ! particule interne
            NC=MOD(JK,NSPCOND+1)
            IF(NC.NE.0.AND.DK.LT.ONE)THEN
              NVOISS1=NVOISS1+1
              IXSP(NVOIS2+NVOISS1,N)=JK
            END IF
          ELSE                      ! particule remote
            JK = -JK
            NC=MOD(JK,NSPCOND+1)
            IF(NC.NE.0.AND.DK.LT.ONE)THEN
              NVOISS1=NVOISS1+1
              IXSP(NVOIS2+NVOISS1,N)=-JK
            END IF
          END IF
         END DO
         KXSP(6,N)=NVOISS1
         NVOISS2=NVOISS1
C
         DO K=1,NVOIS
          JK=JVOIS(K)
          DK=DVOIS(K)
          IF(JK.GT.0)THEN            ! particule interne
            NC=MOD(JK,NSPCOND+1)
            IF(NC.NE.0.AND.DK.GE.ONE)THEN
              NVOISS2=NVOISS2+1
              IXSP(NVOIS2+NVOISS2,N)=JK
            END IF
          ELSE                      ! particule remote
            JK = -JK
            NC=MOD(JK,NSPCOND+1)
            IF(NC.NE.0.AND.DK.GE.ONE)THEN
              NVOISS2=NVOISS2+1
              IXSP(NVOIS2+NVOISS2,N)=-JK
            END IF
          END IF
         END DO
         KXSP(7,N)=NVOISS2
C-----------------
        END IF   ! fin suivant NVOIS2+NVOISS.LE.KVOISPH
C
        IF(NVOIS1+NVOISS1 > LVOISPH)IREDUCE=1
C-------------------------------------------
       END DO
C-----------------
C      Gauges
C-----------------
C Boucle parallele dynamique SMP
C
!$OMP DO SCHEDULE(DYNAMIC,1)
       DO IG=1,NBGAUGE
        IF(LGAUGE(1,IG) <= -(NUMELS+1))THEN
         N=NUMSPH+IG
C
         XI =GAUGE(2,IG)
         YI =GAUGE(3,IG)
         ZI =GAUGE(4,IG)
C------
         NVOIS2   =KXSP(5,N)
         KXSP(7,N)=0
C------
         DO I=1,NVOIS2
          JNOD=IXSP(I,N)
          IF(JNOD.GT.0)THEN          ! particule interne       
           M=NOD2SP(JNOD)
           DJ =SPBUF(1,M)
           DIJ=TWO*DJ
           DIJ=DIJ*DIJ
           DO NC=1,NSPCOND
            JS=ISPSYM(NC,M)
            IF(JS.GT.0)THEN
             XJ =XSPSYM(1,JS)
             YJ =XSPSYM(2,JS)
             ZJ =XSPSYM(3,JS)
             DD=(XI-XJ)*(XI-XJ)+(YI-YJ)*(YI-YJ)+(ZI-ZJ)*(ZI-ZJ)
             IF(DD.LE.(ONE+MYSPATRUE2)*DIJ) THEN
              NVOISS=KXSP(7,N)
              NVOISS=NVOISS+1
              JVOIS(NVOIS2+NVOISS)=NC+M*(NSPCOND+1)
              DVOIS(NVOIS2+NVOISS)=DD/DIJ
              KXSP(7,N)=NVOISS
             END IF
            END IF
           END DO
          ELSE                    ! particule remote
           NN = -JNOD
           DJ =XSPHR(2,NN)
           DIJ=TWO*DJ
           DIJ=DIJ*DIJ
           DO NC=1,NSPCOND
            JS=ISPSYMR(NC,NN)
            IF(JS.GT.0)THEN
             XJ =XSPSYM(1,JS)
             YJ =XSPSYM(2,JS)
             ZJ =XSPSYM(3,JS)
             DD=(XI-XJ)*(XI-XJ)+(YI-YJ)*(YI-YJ)+(ZI-ZJ)*(ZI-ZJ)
             IF(DD.LE.(ONE+MYSPATRUE2)*DIJ) THEN
              NVOISS=KXSP(7,N)
              NVOISS=NVOISS+1
              JVOIS(NVOIS2+NVOISS)=-NC-NN*(NSPCOND+1)           ! reperage particule sym remote
C             JVOIS(NVOIS2+NVOISS)=NC+M*(NSPCOND+1)
C 2 choix : NC + (NN+NUMSPH)*(NSPCOND+1) : on retrouve ensuite que NN > NUMSPH
C             -NC-NN*(NSPCOND+1) : on sait que l'on est sur du remote car -
              DVOIS(NVOIS2+NVOISS)=DD/DIJ
              KXSP(7,N)=NVOISS
             END IF
            END IF
           END DO
          END IF
         END DO
         NVOISS=KXSP(7,N)
         IF(NVOIS2+NVOISS.LE.KVOISPH)THEN
C--------------------------------------------------
C         ghost particles are added to real ones.
C         (ordonne les particules fantomes).
          NVOISS1=0
          NVOISS2=NVOISS
          DO K=NVOIS2+1,NVOIS2+NVOISS
           DK=DVOIS(K)
           JK=JVOIS(K)
           IF(DK.LT.ONE)THEN
            NVOISS1=NVOISS1+1
            IXSP(NVOIS2+NVOISS1,N)=JK
           ELSE
            IXSP(NVOIS2+NVOISS2,N)=JK
            NVOISS2=NVOISS2-1
           ENDIF
          ENDDO
          KXSP(6,N)=NVOISS1
         ELSE
C-------
C         preparing all real neighbours for sort.
          DO J=1,NVOIS2
           JNOD=IXSP(J,N)
           IF(JNOD.GT.0)THEN               ! particule interne  
            XJ =X(1,JNOD)
            YJ =X(2,JNOD)
            ZJ =X(3,JNOD)
            M =NOD2SP(JNOD)
            DJ=SPBUF(1,M)
            DIJ=TWO*DJ
            DIJ=DIJ*DIJ
            DD=(XI-XJ)*(XI-XJ)+(YI-YJ)*(YI-YJ)+(ZI-ZJ)*(ZI-ZJ)
            DVOIS(J)=DD/DIJ
            JVOIS(J)=M*(NSPCOND+1)
           ELSE                           ! particule remote
            NN = -JNOD
            XJ = XSPHR(3,NN)
            YJ = XSPHR(4,NN)
            ZJ = XSPHR(5,NN)
            DJ = XSPHR(2,NN)
            DIJ=TWO*DJ
            DIJ=DIJ*DIJ
            DD=(XI-XJ)*(XI-XJ)+(YI-YJ)*(YI-YJ)+(ZI-ZJ)*(ZI-ZJ)
            DVOIS(J)=DD/DIJ
            JVOIS(J)=-NN*(NSPCOND+1)
           END IF
          ENDDO
C-------
          CALL MYQSORT(NVOIS2+NVOISS,DVOIS,JPERM,IERROR)
          DO K=1,NVOIS2+NVOISS
           JSTOR(K)=JVOIS(K)
          ENDDO
          DO K=1,KVOISPH
           JVOIS(K)=JSTOR(JPERM(K))
          ENDDO
C-------
C         adapting bucket sort true security coefficient ; 
C         looking if non-zero weight neighbours were lost.
          DK=DVOIS(KVOISPH)
C Choix des cellules a conserver tq distance < DK pour eviter pb de parith/on
	  NVOIS=0
	  DO K=1,KVOISPH
	   IF(DVOIS(K).LT.DK)THEN
	     NVOIS=NVOIS+1
	   END IF
	  END DO
C-------
C         Re-ordonne les voisins (particules reelles puis particules fantomes).
          NVOIS1=0
          DO K=1,NVOIS
           JK=JVOIS(K)
           DK=DVOIS(K)
           IF(JK.GT.0)THEN            ! particule interne
            NC=MOD(JK,NSPCOND+1)
            IF(NC.EQ.0.AND.DK.LT.ONE)THEN
              NVOIS1=NVOIS1+1
              IXSP(NVOIS1,N)=KXSP(3,JK/(NSPCOND+1))
            ENDIF
           ELSE                      ! particule remote
            JK = -JK
            NC=MOD(JK,NSPCOND+1)
            IF(NC.EQ.0.AND.DK.LT.ONE)THEN
              NVOIS1=NVOIS1+1
              IXSP(NVOIS1,N)=-JK/(NSPCOND+1)
            END IF
           END IF
          END DO
          KXSP(4,N)=NVOIS1
          NVOIS2=NVOIS1
C
          DO K=1,NVOIS
           JK=JVOIS(K)
           DK=DVOIS(K)
           IF(JK.GT.0)THEN            ! particule interne
            NC=MOD(JK,NSPCOND+1)
            IF(NC.EQ.0.AND.DK.GE.ONE)THEN
              NVOIS2=NVOIS2+1
              IXSP(NVOIS2,N)=KXSP(3,JK/(NSPCOND+1))
            END IF
           ELSE                      ! particule remote
            JK = -JK
            NC=MOD(JK,NSPCOND+1)
            IF(NC.EQ.0.AND.DK.GE.ONE)THEN
              NVOIS2=NVOIS2+1
              IXSP(NVOIS2,N)=-JK/(NSPCOND+1)
            END IF
           END IF
          END DO
          KXSP(5,N)=NVOIS2
C--------
C         ordonne les particules fantomes.
          NVOISS1=0
          DO K=1,NVOIS
           JK=JVOIS(K)
           DK=DVOIS(K)
           IF(JK.GT.0)THEN            ! particule interne
            NC=MOD(JK,NSPCOND+1)
            IF(NC.NE.0.AND.DK.LT.ONE)THEN
              NVOISS1=NVOISS1+1
              IXSP(NVOIS2+NVOISS1,N)=JK
            END IF
           ELSE                      ! particule remote
            JK = -JK
            NC=MOD(JK,NSPCOND+1)
            IF(NC.NE.0.AND.DK.LT.ONE)THEN
              NVOISS1=NVOISS1+1
              IXSP(NVOIS2+NVOISS1,N)=-JK
            END IF
           END IF
          END DO
          KXSP(6,N)=NVOISS1
          NVOISS2=NVOISS1
C
          DO K=1,NVOIS
           JK=JVOIS(K)
           DK=DVOIS(K)
           IF(JK.GT.0)THEN            ! particule interne
             NC=MOD(JK,NSPCOND+1)
             IF(NC.NE.0.AND.DK.GE.ONE)THEN
               NVOISS2=NVOISS2+1
               IXSP(NVOIS2+NVOISS2,N)=JK
             END IF
           ELSE                      ! particule remote
             JK = -JK
             NC=MOD(JK,NSPCOND+1)
             IF(NC.NE.0.AND.DK.GE.ONE)THEN
               NVOISS2=NVOISS2+1
               IXSP(NVOIS2+NVOISS2,N)=-JK
             END IF
           END IF
          END DO
          KXSP(7,N)=NVOISS2
C-----------------
         END IF   ! fin suivant NVOIS2+NVOISS.LE.KVOISPH
C-------------------------------------------
         END IF
       END DO
!$OMP END DO
      END IF
C-------------------------------------------
      RETURN
      END
