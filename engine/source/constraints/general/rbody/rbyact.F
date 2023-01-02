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
Chd|  RBYACT                        source/constraints/general/rbody/rbyact.F
Chd|-- called by -----------
Chd|        RBYPID                        source/constraints/general/rbody/rbypid.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        INEPRI                        source/constraints/general/rbody/inepri.F
Chd|        SPMD_EXCH_FR6                 source/mpi/kinematic_conditions/spmd_exch_fr6.F
Chd|        SUM_6_FLOAT                   source/system/parit.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE RBYACT(RBY,M,LSN ,NSL ,MS  ,
     .                  IN ,X,ITAB,SKEW,ISPH,
     .                  IWA,NPBYI,RBYI,LSNI ,
     .                  PMAIN,ICOMM,WEIGHT,ID)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER M, NSL,ISPH, PMAIN, ICOMM(*), WEIGHT(*)
      INTEGER LSN(*), ITAB(*),IWA(*),NPBYI(NNPBY,*) ,LSNI(*),ID
C     REAL
      my_real
     .   RBY(20), MS(*), IN(*), X(3,*), SKEW(LSKEW,*),RBYI(NRBY,*),
     .   F1(NSL), F2(NSL), F3(NSL), F4(NSL),
     .   F5(NSL), F6(NSL),F7(NSL), F8(NSL),F9(NSL)
      DOUBLE PRECISION RBF6(5,6), RBFB6(9,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J, I, N, NONOD, NI, NSLI, K, II
C     REAL
      my_real
     .   XG(3), XMG, XX, XY, XZ, YY, YZ, ZZ, XIIN,MASRB,INERT,
     .   RBY17,RBY18,RBY19,RBY20,RBY21,RBY22,RBY23,RBY24,RBY25,
     .   II1,II2,II3,II4,II5,II6,II7,II8,II9, INMIN, INMAX
C
      RBY(1)=IN(M)
      RBY(2)=ZERO
      RBY(3)=ZERO
      RBY(4)=ZERO
      RBY(5)=IN(M)
      RBY(6)=ZERO
      RBY(7)=ZERO
      RBY(8)=ZERO
      RBY(9)=IN(M)
C
      XMG=MS(M)
      MASRB=MS(M)
      NONOD=ITAB(M)
C
c      IF(NSPMD>1) THEN
c        RBY(1)=RBY(1)*WEIGHT(M)
c        RBY(5)=RBY(5)*WEIGHT(M)
c        RBY(9)=RBY(9)*WEIGHT(M)
c        MASRB=MASRB*WEIGHT(M)
c        XMG=XMG*WEIGHT(M)
c      ENDIF
C---------------------------------
C     RECHERCHE DES NOEUDS SECONDS
C     DE SOUS RBY (NOEUD MAIN PARMI LES SECONDS)
C---------------------------------
      DO I=1,NSL
         N=LSN(I)
         IF(IWA(N)>0)THEN
           K=0
           DO NI=1,IWA(N)-1
             NSLI=NPBYI(2,NI)
             K  = K  + NSLI
           ENDDO
           NSLI=NPBYI(2,IWA(N))
           DO II=1,NSLI
             NI=LSNI(K+II)
             IF(IWA(NI)==0)IWA(NI)=-1
           ENDDO
         ENDIF
      ENDDO
C---------------------------------
C     CORRECTION DE LA MASSE ET DU
C     CENTRE DE GRAVITE DU MAIN
C---------------------------------
      DO J=1,3
         XG(J)=X(J,M)
         X(J,M)=X(J,M)*MS(M)
      ENDDO
      IF(N2D/=0) THEN
C       ANALYSE 2D 
        XG(1)= ZERO
        X(1,M)= ZERO
      ENDIF
C
      DO I=1,NSL
        N=LSN(I)
        IF(IWA(N)>=0.AND.WEIGHT(N)==1)THEN
          F1(I) =  X(1,N)*MS(N)
          F2(I) =  X(2,N)*MS(N)
          F3(I) =  X(3,N)*MS(N)
          F4(I) =  MS(N)
          F5(I) =  IN(N)
        ELSE
          F1(I) = ZERO
          F2(I) = ZERO
          F3(I) = ZERO
          F4(I) = ZERO
C inertie meme si iwa(n)==0
          F5(I) = IN(N)*WEIGHT(N)
        ENDIF
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
      END DO
C
      CALL SUM_6_FLOAT(1  ,NSL  ,F1, RBF6(1,1), 5)
      CALL SUM_6_FLOAT(1  ,NSL  ,F2, RBF6(2,1), 5)
      CALL SUM_6_FLOAT(1  ,NSL  ,F3, RBF6(3,1), 5)
      CALL SUM_6_FLOAT(1  ,NSL  ,F4, RBF6(4,1), 5)
      CALL SUM_6_FLOAT(1  ,NSL  ,F5, RBF6(5,1), 5)

      
      IF(NSPMD>1) THEN
        CALL SPMD_EXCH_FR6(ICOMM,RBF6,5*6)
      ENDIF

      X(1,M) = X(1,M)+
     +         RBF6(1,1)+RBF6(1,2)+RBF6(1,3)+
     +         RBF6(1,4)+RBF6(1,5)+RBF6(1,6)
      X(2,M) = X(2,M)+
     +         RBF6(2,1)+RBF6(2,2)+RBF6(2,3)+
     +         RBF6(2,4)+RBF6(2,5)+RBF6(2,6)
      X(3,M) = X(3,M)+
     +         RBF6(3,1)+RBF6(3,2)+RBF6(3,3)+
     +         RBF6(3,4)+RBF6(3,5)+RBF6(3,6)
      MASRB  = MASRB+
     +         RBF6(4,1)+RBF6(4,2)+RBF6(4,3)+
     +         RBF6(4,4)+RBF6(4,5)+RBF6(4,6)
      INERT   =RBF6(5,1)+RBF6(5,2)+RBF6(5,3)+
     +         RBF6(5,4)+RBF6(5,5)+RBF6(5,6)

      IF(MASRB<=ZERO)THEN
        IF (ISPMD+1==PMAIN) THEN
          CALL ANCMSG(MSGID=109,ANMODE=ANINFO,
     .            I1=NONOD)
        ENDIF
        CALL ARRET(2)
      ENDIF

      DO J=1,3
       X(J,M)=X(J,M)/MASRB
      ENDDO

C--------------------------------------
C     CORRECTION DE L'INERTIE DU MAIN
C--------------------------------------
      RBY(1)=RBY(1)+INERT
      RBY(5)=RBY(5)+INERT
      RBY(9)=RBY(9)+INERT
C
      IF(N2D==0) THEN
C       ANALYSE 3D
        XX=(XG(1)-X(1,M))*(XG(1)-X(1,M))
        XY=(XG(1)-X(1,M))*(XG(2)-X(2,M))
        XZ=(XG(1)-X(1,M))*(XG(3)-X(3,M))
        YY=(XG(2)-X(2,M))*(XG(2)-X(2,M))
        YZ=(XG(2)-X(2,M))*(XG(3)-X(3,M))
        ZZ=(XG(3)-X(3,M))*(XG(3)-X(3,M))
        RBY(1)=RBY(1)+(YY+ZZ)*XMG
        RBY(2)=RBY(2)-XY*XMG
        RBY(3)=RBY(3)-XZ*XMG
        RBY(4)=RBY(4)-XY*XMG
        RBY(5)=RBY(5)+(ZZ+XX)*XMG
        RBY(6)=RBY(6)-YZ*XMG
        RBY(7)=RBY(7)-XZ*XMG
        RBY(8)=RBY(8)-YZ*XMG
        RBY(9)=RBY(9)+(XX+YY)*XMG
C
        DO I=1,NSL
          N=LSN(I)
          NI=IWA(N)
         IF(NI==0.AND.WEIGHT(N)==1)THEN
           XX=(X(1,N)-X(1,M))*(X(1,N)-X(1,M))
           XY=(X(1,N)-X(1,M))*(X(2,N)-X(2,M))
           XZ=(X(1,N)-X(1,M))*(X(3,N)-X(3,M))
           YY=(X(2,N)-X(2,M))*(X(2,N)-X(2,M))
           YZ=(X(2,N)-X(2,M))*(X(3,N)-X(3,M))
           ZZ=(X(3,N)-X(3,M))*(X(3,N)-X(3,M))
           F1(I)=(YY+ZZ)*MS(N)
           F2(I)=-XY*MS(N)
           F3(I)=-XZ*MS(N)
           F4(I)=-XY*MS(N)
           F5(I)=(ZZ+XX)*MS(N)
           F6(I)=-YZ*MS(N)
           F7(I)=-XZ*MS(N)
           F8(I)=-YZ*MS(N)
           F9(I)=(XX+YY)*MS(N)
         ELSEIF(NI>0.AND.WEIGHT(N)==1)THEN
C main DE SOUS RBY
           RBY(1)=RBY(1)-IN(N)
           RBY(5)=RBY(5)-IN(N)
           RBY(9)=RBY(9)-IN(N)
           XX=(X(1,N)-X(1,M))*(X(1,N)-X(1,M))
           XY=(X(1,N)-X(1,M))*(X(2,N)-X(2,M))
           XZ=(X(1,N)-X(1,M))*(X(3,N)-X(3,M))
           YY=(X(2,N)-X(2,M))*(X(2,N)-X(2,M))
           YZ=(X(2,N)-X(2,M))*(X(3,N)-X(3,M))
           ZZ=(X(3,N)-X(3,M))*(X(3,N)-X(3,M))
C MATRICE d'inertie -> repere globale
           II1=RBYI(10,NI)*RBYI(1,NI)
           II2=RBYI(10,NI)*RBYI(2,NI)
           II3=RBYI(10,NI)*RBYI(3,NI)
           II4=RBYI(11,NI)*RBYI(4,NI)
           II5=RBYI(11,NI)*RBYI(5,NI)
           II6=RBYI(11,NI)*RBYI(6,NI)
           II7=RBYI(12,NI)*RBYI(7,NI)
           II8=RBYI(12,NI)*RBYI(8,NI)
           II9=RBYI(12,NI)*RBYI(9,NI)
C
           RBY17=RBYI(1,NI)*II1 + RBYI(4,NI)*II4 + RBYI(7,NI)*II7
           RBY18=RBYI(1,NI)*II2 + RBYI(4,NI)*II5 + RBYI(7,NI)*II8
           RBY19=RBYI(1,NI)*II3 + RBYI(4,NI)*II6 + RBYI(7,NI)*II9
           RBY20=RBYI(2,NI)*II1 + RBYI(5,NI)*II4 + RBYI(8,NI)*II7
           RBY21=RBYI(2,NI)*II2 + RBYI(5,NI)*II5 + RBYI(8,NI)*II8
           RBY22=RBYI(2,NI)*II3 + RBYI(5,NI)*II6 + RBYI(8,NI)*II9
           RBY23=RBYI(3,NI)*II1 + RBYI(6,NI)*II4 + RBYI(9,NI)*II7
           RBY24=RBYI(3,NI)*II2 + RBYI(6,NI)*II5 + RBYI(9,NI)*II8
           RBY25=RBYI(3,NI)*II3 + RBYI(6,NI)*II6 + RBYI(9,NI)*II9
C
           F1(I)=(YY+ZZ)*MS(N)+RBY17
           F2(I)=-XY*MS(N)+RBY18
           F3(I)=-XZ*MS(N)+RBY19
           F4(I)=-XY*MS(N)+RBY20
           F5(I)=(ZZ+XX)*MS(N)+RBY21
           F6(I)=-YZ*MS(N)+RBY22
           F7(I)=-XZ*MS(N)+RBY23
           F8(I)=-YZ*MS(N)+RBY24
           F9(I)=(XX+YY)*MS(N)+RBY25
         ELSE
           F1(I) = ZERO
           F2(I) = ZERO
           F3(I) = ZERO
           F4(I) = ZERO
           F5(I) = ZERO
           F6(I) = ZERO
           F7(I) = ZERO
           F8(I) = ZERO
           F9(I) = ZERO
         ENDIF
        ENDDO
      ELSEIF(N2D==1) THEN
C       ANALYSE 2D  
        YY=(XG(2)-X(2,M))*(XG(2)-X(2,M))
        YZ=(XG(2)-X(2,M))*(XG(3)-X(3,M))
        ZZ=(XG(3)-X(3,M))*(XG(3)-X(3,M))
        RBY(1)=RBY(1)+(YY+ZZ)*XMG
        RBY(2)=ZERO
        RBY(3)=ZERO
        RBY(4)=ZERO
        RBY(5)=RBY(5)+ZZ*XMG
        RBY(6)=RBY(6)-YZ*XMG
        RBY(7)=ZERO
        RBY(8)=RBY(8)-YZ*XMG
        RBY(9)=RBY(9)+YY*XMG
C
        DO I=1,NSL
          N=LSN(I)
          NI=IWA(N)
         IF(NI==0.AND.WEIGHT(N)==1)THEN
           YY=(X(2,N)-X(2,M))*(X(2,N)-X(2,M))
           YZ=(X(2,N)-X(2,M))*(X(3,N)-X(3,M))
           ZZ=(X(3,N)-X(3,M))*(X(3,N)-X(3,M))
           F1(I)=(YY+ZZ)*MS(N)
           F2(I)=ZERO
           F3(I)=ZERO
           F4(I)=ZERO
           F5(I)=ZZ*MS(N)
           F6(I)=-YZ*MS(N)
           F7(I)=ZERO
           F8(I)=-YZ*MS(N)
           F9(I)=YY*MS(N)
         ELSEIF(NI>0.AND.WEIGHT(N)==1)THEN
C main DE SOUS RBY
           RBY(1)=RBY(1)-IN(N)
           RBY(5)=RBY(5)-IN(N)
           RBY(9)=RBY(9)-IN(N)
           YY=(X(2,N)-X(2,M))*(X(2,N)-X(2,M))
           YZ=(X(2,N)-X(2,M))*(X(3,N)-X(3,M))
           ZZ=(X(3,N)-X(3,M))*(X(3,N)-X(3,M))
C MATRICE d'inertie -> repere globale
           II1=RBYI(10,NI)*RBYI(1,NI)
           II5=RBYI(11,NI)*RBYI(5,NI)
           II6=RBYI(11,NI)*RBYI(6,NI)
           II8=RBYI(12,NI)*RBYI(8,NI)
           II9=RBYI(12,NI)*RBYI(9,NI)
C
           RBY17=RBYI(1,NI)*II1 
           RBY18= ZERO
           RBY19= ZERO
           RBY20= ZERO
           RBY21= RBYI(5,NI)*II5 + RBYI(8,NI)*II8
           RBY22= RBYI(5,NI)*II6 + RBYI(8,NI)*II9
           RBY23= ZERO
           RBY24= RBYI(6,NI)*II5 + RBYI(9,NI)*II8
           RBY25= RBYI(6,NI)*II6 + RBYI(9,NI)*II9
C
           F1(I)=(YY+ZZ)*MS(N)+RBY17
           F2(I)=ZERO
           F3(I)=ZERO
           F4(I)=ZERO
           F5(I)=ZZ*MS(N)+RBY21
           F6(I)=-YZ*MS(N)+RBY22
           F7(I)=ZERO
           F8(I)=-YZ*MS(N)+RBY24
           F9(I)=YY*MS(N)+RBY25
         ELSE
           F1(I) = ZERO
           F2(I) = ZERO
           F3(I) = ZERO
           F4(I) = ZERO
           F5(I) = ZERO
           F6(I) = ZERO
           F7(I) = ZERO
           F8(I) = ZERO
           F9(I) = ZERO
         ENDIF
        ENDDO
      ENDIF
C
C Traitement Parith/ON avant echange
C
      DO K = 1, 6
        RBFB6(1,K) = ZERO
        RBFB6(2,K) = ZERO
        RBFB6(3,K) = ZERO
        RBFB6(4,K) = ZERO
        RBFB6(5,K) = ZERO
        RBFB6(6,K) = ZERO
        RBFB6(7,K) = ZERO
        RBFB6(8,K) = ZERO
        RBFB6(9,K) = ZERO
      END DO

      CALL SUM_6_FLOAT(1  ,NSL  ,F1, RBFB6(1,1), 9)
      CALL SUM_6_FLOAT(1  ,NSL  ,F2, RBFB6(2,1), 9)
      CALL SUM_6_FLOAT(1  ,NSL  ,F3, RBFB6(3,1), 9)
      CALL SUM_6_FLOAT(1  ,NSL  ,F4, RBFB6(4,1), 9)
      CALL SUM_6_FLOAT(1  ,NSL  ,F5, RBFB6(5,1), 9)
      CALL SUM_6_FLOAT(1  ,NSL  ,F6, RBFB6(6,1), 9)
      CALL SUM_6_FLOAT(1  ,NSL  ,F7, RBFB6(7,1), 9)
      CALL SUM_6_FLOAT(1  ,NSL  ,F8, RBFB6(8,1), 9)
      CALL SUM_6_FLOAT(1  ,NSL  ,F9, RBFB6(9,1), 9)


      IF(NSPMD>1) THEN
        CALL SPMD_EXCH_FR6(ICOMM,RBFB6,9*6)
      ENDIF

        RBY(1) = RBY(1) + RBFB6(1,1)+RBFB6(1,2)+RBFB6(1,3)+
     +         RBFB6(1,4)+RBFB6(1,5)+RBFB6(1,6)
        RBY(2) = RBY(2) + RBFB6(2,1)+RBFB6(2,2)+RBFB6(2,3)+
     +         RBFB6(2,4)+RBFB6(2,5)+RBFB6(2,6)
        RBY(3) = RBY(3) + RBFB6(3,1)+RBFB6(3,2)+RBFB6(3,3)+
     +         RBFB6(3,4)+RBFB6(3,5)+RBFB6(3,6)
        RBY(4) = RBY(4) + RBFB6(4,1)+RBFB6(4,2)+RBFB6(4,3)+
     +         RBFB6(4,4)+RBFB6(4,5)+RBFB6(4,6)
        RBY(5) = RBY(5) + RBFB6(5,1)+RBFB6(5,2)+RBFB6(5,3)+
     +         RBFB6(5,4)+RBFB6(5,5)+RBFB6(5,6)
        RBY(6) = RBY(6) + RBFB6(6,1)+RBFB6(6,2)+RBFB6(6,3)+
     +         RBFB6(6,4)+RBFB6(6,5)+RBFB6(6,6)
        RBY(7) = RBY(7) + RBFB6(7,1)+RBFB6(7,2)+RBFB6(7,3)+
     +         RBFB6(7,4)+RBFB6(7,5)+RBFB6(7,6)
        RBY(8) = RBY(8) + RBFB6(8,1)+RBFB6(8,2)+RBFB6(8,3)+
     +         RBFB6(8,4)+RBFB6(8,5)+RBFB6(8,6)
        RBY(9) = RBY(9) + RBFB6(9,1)+RBFB6(9,2)+RBFB6(9,3)+
     +          RBFB6(9,4)+RBFB6(9,5)+RBFB6(9,6)
C----------------------------------------------
C     MISE A ZERO DES MASSES ET INERTIES SECNDS voir up
C----------------------------------------------
      IF (ISPMD+1==PMAIN) THEN
        WRITE(IOUT,1000)
        WRITE(IOUT,1100) NONOD,X(1,M),X(2,M),X(3,M),
     .        MASRB,RBY(1),RBY(5),RBY(9),RBY(2),RBY(3),RBY(6)
      ENDIF
C----------------------------------------------------------------
C     CALCUL DU REPERE D'INERTIE PRINCIPALE
C----------------------------------------------------------------
      IF(N2D == 1) THEN
        RBY(10) = RBY(1)
        RBY(11) = RBY(5)
        RBY(12) = RBY(9)
        RBY(1) = ONE
        RBY(5) = ONE
        RBY(9) = ONE
      ELSE
        CALL INEPRI(RBY(10),RBY)
      ENDIF
      IF(ISPH==1)THEN
        XIIN = (RBY(10) + RBY(11) + RBY(12)) * THIRD
        RBY(10) = XIIN
        RBY(11) = XIIN
        RBY(12) = XIIN
      ELSEIF(ISPH==2) THEN
        INMIN = MIN(RBY(10),RBY(11),RBY(12))
        INMAX = MAX(RBY(10),RBY(11),RBY(12))
        IF(INMIN<=1.E-3*INMAX)THEN
          IF(RBY(10)/INMAX<EM03) THEN
            RBY(10)=RBY(10)+EM01*INMAX
          ENDIF
          IF (RBY(11)/INMAX<EM03) THEN
            RBY(11)=RBY(11)+EM01*INMAX
          ENDIF
          IF (RBY(12)/INMAX<EM03) THEN
            RBY(12)=RBY(12)+EM01*INMAX
          ENDIF
          CALL ANCMSG(MSGID=281,
     .                ANMODE=ANINFO,
     .                I1=ID)
        ENDIF
      ENDIF
C
      RBY(13)=IN(M)
      RBY(14)=MASRB
      RBY(15)=MS(M)
      MS(M) = MASRB
      IN(M) = MIN(RBY(10),RBY(11),RBY(12))
C
      RETURN
C
1000  FORMAT(
     . 44H1     RIGID BODY INITIALIZATION             /
     . 44H      -------------------------             /)
1100  FORMAT(/5X,'RIGID BODY ',
     .       /10X,'PRIMARY NODE    ',I8
     .       /10X,'NEW X,Y,Z       ',3G10.3
     .       /10X,'NEW MASS        ',1G10.3
     .       /10X,'NEW INERTIA     ',6G10.3)
      END
