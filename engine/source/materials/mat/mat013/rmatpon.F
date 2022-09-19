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
Chd|  RMATPON                       source/materials/mat/mat013/rmatpon.F
Chd|-- called by -----------
Chd|        RMATFORP                      source/materials/mat/mat013/rmatforp.F
Chd|-- calls ---------------
Chd|        ROTBMR                        source/tools/skew/rotbmr.F    
Chd|        SUM_6_FLOAT                   source/system/parit.F         
Chd|====================================================================
      SUBROUTINE RMATPON(AF   ,AM   ,X    ,RBY   ,NOD  ,
     2                   NBY  ,STIFN,STIFR,WEIGHT,NSL  ,
     3                   RBF6 ,ICOD ,ARBY ,VRBY  ,ARRBY,
     4                   VRRBY,IFLAG      )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "scr11_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NOD(*), NBY(*), WEIGHT(*), ICOD(2,*),
     .        NSL,  IFLAG
C     REAL
      my_real
     .   AF(3,*), AM(3,*), X(3,*),  RBY(*),
     .   STIFN(*),STIFR(*),
     .   VRRBY(3,*),ARBY(3,*),ARRBY(3,*),VRBY(3,*)
      DOUBLE PRECISION RBF6(6,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER M, I, N, LCOD, ISK, K
C     REAL
      my_real
     .  VI(3),XG,YG,ZG,II1,II2,II3,II4,II5,II6,II7,II8,
     .   II9,WA1,WA2,WA3,DET,IN,MSRBY,RX,RY,RZ,RB(12),
     .   F1(NSL), F2(NSL), F3(NSL), F4(NSL),
     .   F5(NSL), F6(NSL)
C-----------------------------------------------
C premiere passe : calcul de force sur tous les r.b.
      M = NBY(1)
      IF( M < 0) RETURN
C       
      IF (IFLAG == 1) THEN
C
c       NSL   = NBY(2)
       XG    = RBY(2)
       YG    = RBY(3)
       ZG    = RBY(4)
       DO I=1,NSL
         N = NOD(I)
         IF (WEIGHT(N) == 1) THEN
           F1(I) = AF(1,N)
           F2(I) = AF(2,N)
           F3(I) = AF(3,N)
           RX = X(1,N) - XG
           RY = X(2,N) - YG
           RZ = X(3,N) - ZG
           F4(I) = AM(1,N) + RY*AF(3,N) - RZ*AF(2,N)
           F5(I) = AM(2,N) + RZ*AF(1,N) - RX*AF(3,N)
           F6(I) = AM(3,N) + RX*AF(2,N) - RY*AF(1,N)
         ELSE
           F1(I) = ZERO
           F2(I) = ZERO
           F3(I) = ZERO
           F4(I) = ZERO
           F5(I) = ZERO
           F6(I) = ZERO
         END IF
       ENDDO
C
C Traitement Parith/ON avant echange
C
       DO K = 1, 6
         RBF6(1,K) = ZERO
         RBF6(2,K) = ZERO
         RBF6(3,K) = ZERO
         RBF6(4,K) = ZERO
         RBF6(5,K) = ZERO
         RBF6(6,K) = ZERO
       END DO
       CALL SUM_6_FLOAT(1  ,NSL  ,F1, RBF6(1,1), 6)
       CALL SUM_6_FLOAT(1  ,NSL  ,F2, RBF6(2,1), 6)
       CALL SUM_6_FLOAT(1  ,NSL  ,F3, RBF6(3,1), 6)
       CALL SUM_6_FLOAT(1  ,NSL  ,F4, RBF6(4,1), 6)
       CALL SUM_6_FLOAT(1  ,NSL  ,F5, RBF6(5,1), 6)
       CALL SUM_6_FLOAT(1  ,NSL  ,F6, RBF6(6,1), 6)
C
C phase 2 : 
C
      ELSEIF (IFLAG==2) THEN
C     
C Traitement Parith/ON apres echange
C
       ARBY(1,M)  = RBF6(1,1)+RBF6(1,2)+RBF6(1,3)+
     +            RBF6(1,4)+RBF6(1,5)+RBF6(1,6)
       ARBY(2,M)  = RBF6(2,1)+RBF6(2,2)+RBF6(2,3)+
     +            RBF6(2,4)+RBF6(2,5)+RBF6(2,6)
       ARBY(3,M)  = RBF6(3,1)+RBF6(3,2)+RBF6(3,3)+
     +            RBF6(3,4)+RBF6(3,5)+RBF6(3,6)
       ARRBY(1,M) = RBF6(4,1)+RBF6(4,2)+RBF6(4,3)+
     +            RBF6(4,4)+RBF6(4,5)+RBF6(4,6)
       ARRBY(2,M) = RBF6(5,1)+RBF6(5,2)+RBF6(5,3)+
     +            RBF6(5,4)+RBF6(5,5)+RBF6(5,6)
       ARRBY(3,M) = RBF6(6,1)+RBF6(6,2)+RBF6(6,3)+
     +            RBF6(6,4)+RBF6(6,5)+RBF6(6,6)
C         
        LCOD=ICOD(1,M)
        RB(1)= RBY(5)
        RB(2)= RBY(6)
        RB(3)= RBY(7)
        RB(4)= RBY(8)
        RB(5)= RBY(9)
        RB(6)= RBY(10)
        RB(7)= RBY(11)
        RB(8)= RBY(12)
        RB(9)= RBY(13)
        RB(10)= RBY(14)
        RB(11)= RBY(15)
        RB(12)= RBY(16)
        IN    = RBY(17)
        IF(LCOD > 0)THEN
C       rotation de la matrice d'orientation (directions principales)
          VI(1)=RB(1)*VRRBY(1,M) + RB(2)*VRRBY(2,M)
     .                           + RB(3)*VRRBY(3,M)
          VI(2)=RB(4)*VRRBY(1,M) + RB(5)*VRRBY(2,M)
     .                           + RB(6)*VRRBY(3,M)
          VI(3)=RB(7)*VRRBY(1,M) + RB(8)*VRRBY(2,M)
     .                           + RB(9)*VRRBY(3,M)
          CALL ROTBMR(VI,RB,DT1)
C
C           matrice d'inertie en repere global
          II1=RB(10)*RB(1)
          II2=RB(10)*RB(2)
          II3=RB(10)*RB(3)
          II4=RB(11)*RB(4)
          II5=RB(11)*RB(5)
          II6=RB(11)*RB(6)
          II7=RB(12)*RB(7)
          II8=RB(12)*RB(8)
          II9=RB(12)*RB(9)
C
          RBY(18)=RB(1)*II1 + RB(4)*II4 + RB(7)*II7
          RBY(19)=RB(1)*II2 + RB(4)*II5 + RB(7)*II8
          RBY(20)=RB(1)*II3 + RB(4)*II6 + RB(7)*II9
          RBY(21)=RB(2)*II1 + RB(5)*II4 + RB(8)*II7
          RBY(22)=RB(2)*II2 + RB(5)*II5 + RB(8)*II8
          RBY(23)=RB(2)*II3 + RB(5)*II6 + RB(8)*II9
          RBY(24)=RB(3)*II1 + RB(6)*II4 + RB(9)*II7
          RBY(25)=RB(3)*II2 + RB(6)*II5 + RB(9)*II8
          RBY(26)=RB(3)*II3 + RB(6)*II6 + RB(9)*II9
C
C           ajout des termes [Iglobal] vr ^ vr
          WA1=RBY(18)*VRRBY(1,M)+RBY(19)*VRRBY(2,M)+RBY(20)*VRRBY(3,M)
          WA2=RBY(21)*VRRBY(1,M)+RBY(22)*VRRBY(2,M)+RBY(23)*VRRBY(3,M)
          WA3=RBY(24)*VRRBY(1,M)+RBY(25)*VRRBY(2,M)+RBY(26)*VRRBY(3,M)
C
          ARRBY(1,M)=ARRBY(1,M) + (WA2*VRRBY(3,M)-WA3*VRRBY(2,M))
          ARRBY(2,M)=ARRBY(2,M) + (WA3*VRRBY(1,M)-WA1*VRRBY(3,M))
          ARRBY(3,M)=ARRBY(3,M) + (WA1*VRRBY(2,M)-WA2*VRRBY(1,M))
C------------------
C       REPERE GLOBAL : 
C       Resolution [Iglobal] gama = M, compte-tenu des conditions aux limites
C       Ex : gamaz=0
C           | Ixx Ixy Ixz | { gamax }   { Mx }
C           | Iyx Iyy Iyz | { gamay } = { My }
C           | Izx Izy Izz | {   0   }   { Mz + DMz }  DMz inconnue
C       equivaut a 
C           | Ixx Ixy | { gamax }   { Mx }
C           | Iyx Iyy | { gamay } = { My }
C           et gamaz=0
C------------------
          IF(LCOD == 1)THEN
            DET=ONE/(RBY(18)*RBY(22) - RBY(19)*RBY(21))
            WA1=ARRBY(1,M)
            WA2=ARRBY(2,M)
            ARRBY(1,M)=( RBY(22)*WA1 - RBY(21)*WA2)*DET
            ARRBY(2,M)=(-RBY(19)*WA1 + RBY(18)*WA2)*DET
            ARRBY(3,M)=  ZERO
            VRRBY(3,M) = ZERO
          ELSEIF(LCOD == 2)THEN
            DET=ONE/(RBY(18)*RBY(26) - RBY(20)*RBY(24))
            WA1=ARRBY(1,M)
            WA2=ARRBY(3,M)
            ARRBY(1,M)=( RBY(26)*WA1 - RBY(24)*WA2)*DET
            ARRBY(2,M)= ZERO
            ARRBY(3,M)=(-RBY(20)*WA1 + RBY(18)*WA2)*DET
            VRRBY(2,M) = ZERO
          ELSEIF(LCOD == 3)THEN
            ARRBY(1,M)=ARRBY(1,M)/RBY(18)
            ARRBY(2,M)=ZERO
            ARRBY(3,M)=ZERO
            VRRBY(2,M) = ZERO
            VRRBY(3,M) = ZERO
          ELSEIF(LCOD == 4)THEN
            DET=ONE/(RBY(22)*RBY(26) - RBY(23)*RBY(25))
            WA1=ARRBY(2,M)
            WA2=ARRBY(3,M)
            ARRBY(1,M)=ZERO
            ARRBY(2,M)=( RBY(26)*WA1 - RBY(25)*WA2)*DET
            ARRBY(3,M)=(-RBY(23)*WA1 + RBY(22)*WA2)*DET
            VRRBY(1,M) = ZERO
          ELSEIF(LCOD == 5)THEN
            ARRBY(1,M) =ZERO
            ARRBY(2,M) =ARRBY(2,M)/RBY(22)
            ARRBY(3,M) = ZERO
            VRRBY(1,M) = ZERO
            VRRBY(3,M) = ZERO
          ELSEIF(LCOD == 6)THEN
            ARRBY(1,M)=ZERO
            ARRBY(2,M)=ZERO
            ARRBY(3,M)=ARRBY(3,M)/RBY(26)
            VRRBY(1,M) = ZERO
            VRRBY(2,M) = ZERO
          ELSEIF(LCOD == 7)THEN
            ARRBY(1,M) = ZERO
            ARRBY(2,M) = ZERO
            ARRBY(3,M) = ZERO
            VRRBY(1,M) = ZERO
            VRRBY(2,M) = ZERO
            VRRBY(3,M) = ZERO
          ENDIF
C
         ELSE
C------------------------
C  calcul des accelerations dans le r p re globale du main
C----------------------------      
C correction des momemt M = m  - VRRBY X I.VRRBY
C 
          WA1=ARRBY(1,M)
          WA2=ARRBY(2,M)
          WA3=ARRBY(3,M)
C repere globale -> repere d'inertie principale
          ARRBY(1,M) = RB(1)*WA1 + RB(2)*WA2 + RB(3)*WA3
          ARRBY(2,M) = RB(4)*WA1 + RB(5)*WA2 + RB(6)*WA3
          ARRBY(3,M) = RB(7)*WA1 + RB(8)*WA2 + RB(9)*WA3
C les contributions des VRRBY ne sont ajoutees que sur le processeur main
          VI(1) = RB(1)*VRRBY(1,M)+RB(2)*VRRBY(2,M)+RB(3)*VRRBY(3,M)
          VI(2) = RB(4)*VRRBY(1,M)+RB(5)*VRRBY(2,M)+RB(6)*VRRBY(3,M)
          VI(3) = RB(7)*VRRBY(1,M)+RB(8)*VRRBY(2,M)+RB(9)*VRRBY(3,M)
C
          CALL ROTBMR(VI,RB,DT1)
          ARRBY(1,M) = ARRBY(1,M)
     .                   + ((RB(11)-RB(12))*VI(2)*VI(3))
          ARRBY(2,M) = ARRBY(2,M)
     .                   + ((RB(12)-RB(10))*VI(3)*VI(1))
          ARRBY(3,M) = ARRBY(3,M)
     .                   + ((RB(10)-RB(11))*VI(1)*VI(2))
C CALCUL D'ONE PSEUDO MOMENT: 
C EN FAIT L'ACCELERATION DE ROTATION * INERTIE DU NOEUD MAIN (IMIN DU RB)
C
          WA1 = ARRBY(1,M)*IN/RB(10)
          WA2 = ARRBY(2,M)*IN/RB(11)
          WA3 = ARRBY(3,M)*IN/RB(12)
C 
          ARRBY(1,M) = RB(1)*WA1 + RB(4)*WA2 + RB(7)*WA3
          ARRBY(2,M) = RB(2)*WA1 + RB(5)*WA2 + RB(8)*WA3
          ARRBY(3,M) = RB(3)*WA1 + RB(6)*WA2 + RB(9)*WA3
C        
C MATRICE d'inertie -> repere globale
          II1=RB(10)*RB(1)
          II2=RB(10)*RB(2)
          II3=RB(10)*RB(3)
          II4=RB(11)*RB(4)
          II5=RB(11)*RB(5)
          II6=RB(11)*RB(6)
          II7=RB(12)*RB(7)
          II8=RB(12)*RB(8)
          II9=RB(12)*RB(9)
C
          RBY(18)=RB(1)*II1 + RB(4)*II4 + RB(7)*II7
          RBY(19)=RB(1)*II2 + RB(4)*II5 + RB(7)*II8
          RBY(20)=RB(1)*II3 + RB(4)*II6 + RB(7)*II9
          RBY(21)=RB(2)*II1 + RB(5)*II4 + RB(8)*II7
          RBY(22)=RB(2)*II2 + RB(5)*II5 + RB(8)*II8
          RBY(23)=RB(2)*II3 + RB(5)*II6 + RB(8)*II9
          RBY(24)=RB(3)*II1 + RB(6)*II4 + RB(9)*II7
          RBY(25)=RB(3)*II2 + RB(6)*II5 + RB(9)*II8
          RBY(26)=RB(3)*II3 + RB(6)*II6 + RB(9)*II9
        ENDIF
C      
c        NSL =NBY(2)
        MSRBY = RBY(1)
        IN = RBY(17)
        IF(MSRBY > 0) THEN
          ARBY(1,M)  = ARBY(1,M) / MSRBY
          ARBY(2,M)  = ARBY(2,M) / MSRBY
          ARBY(3,M)  = ARBY(3,M) / MSRBY
        ELSE
          ARBY(1,M)  = ZERO
          ARBY(2,M)  = ZERO
          ARBY(3,M)  = ZERO
        ENDIF

        IF(IN > 0) THEN
          ARRBY(1,M)  = ARRBY(1,M) / IN
          ARRBY(2,M)  = ARRBY(2,M) / IN
          ARRBY(3,M)  = ARRBY(3,M) / IN
        ELSE
          ARRBY(1,M)  = ZERO
          ARRBY(2,M)  = ZERO
          ARRBY(3,M)  = ZERO
        ENDIF
C           
        LCOD   = ICOD(2,M)
        IF(LCOD == 1) THEN
           ARBY(3,M)  = ZERO
           VRBY(3,M)  = ZERO
        ELSEIF(LCOD == 2) THEN
          ARBY(2,M)  = ZERO
          VRBY(2,M)  = ZERO
        ELSEIF(LCOD == 3) THEN
           ARBY(2,M)  = ZERO
           ARBY(3,M)  = ZERO
           VRBY(2,M)  = ZERO
           VRBY(3,M)  = ZERO
        ELSEIF(LCOD == 4) THEN
           ARBY(1,M)  = ZERO
           VRBY(1,M)  = ZERO
        ELSEIF(LCOD == 5) THEN
           ARBY(1,M)  = ZERO
           ARBY(3,M)  = ZERO
           VRBY(1,M)  = ZERO
           VRBY(3,M)  = ZERO
        ELSEIF(LCOD == 6) THEN
           ARBY(1,M)  = ZERO
           ARBY(2,M)  = ZERO
           VRBY(1,M)  = ZERO
           VRBY(2,M)  = ZERO
        ELSEIF(LCOD == 7) THEN
           ARBY(1,M)  = ZERO
           ARBY(2,M)  = ZERO
           ARBY(3,M)  = ZERO
           VRBY(1,M)  = ZERO
           VRBY(2,M)  = ZERO
           VRBY(3,M)  = ZERO
        ENDIF
C          
        DO I=1,NSL
          N = NOD(I)
C
          AF(1,N)= ZERO
          AF(2,N)= ZERO
          AF(3,N)= ZERO
C
          AM(1,N)= ZERO
          AM(2,N)= ZERO
          AM(3,N)= ZERO
C
          STIFR(N)= EM20
          STIFN(N)= EM20
         ENDDO
      ENDIF
C
      RETURN
      END
