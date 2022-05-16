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
Chd|  RIGID_MAT                     source/materials/mat/mat019/rigid_mat.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        INEPRI                        source/materials/mat/mat019/inepri.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE RIGID_MAT(NRB, GNSL  ,LSN , NSLNRM  ,STIFN, 
     .                   STIFR, X, V     ,MS    ,IN   ,
     .                    RBYM, IRBYM ,LCRBM,NOM_OPT)
      USE MESSAGE_MOD
C===============================================
C    RBY    EN SORTIE DE INIRBY
C a voir 
C     1 -> 9 :  MATRICE ROTATION 
C    10 -> 12:  INERTIES PRINCIPALES RBODY
C          13:  INERTIE MAIN INITIALE SPHERIQUE 
C          14:  MASSE RBODY
C          15:  MASSE MAIN INITIALE
C    17 -> 20:  INERTIES DANS LE REPERE GLOBALE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr17_c.inc"


C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRB,GNSL
      INTEGER LSN(*), NSLNRM(*),IRBYM(NIRBYM,NRB) ,LCRBM(GNSL)
C     REAL ou REAL*8
      my_real
     .   RBYM(NFRBYM,*), MS(*), IN(*), X(3,*),V(3,*), STIFN(*),
     .   STIFR(*)
      INTEGER NOM_OPT(LNOPT1,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "warn_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J, NOSKEW, I, N, NONOD,ICDG,KK,NR,NSL,IR
C     REAL ou REAL*8
      my_real
     .   XG(3), XM0(3), XMG, XX, XY, XZ, YY, YZ, ZZ, XIIN, INMIN,
     .   MASRB,DD,TOL, II1,II2,II3,II4,II5,II6,II7,II8,II9,
     .   RBY(20)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
C
      TOL=UN+EM04
      KK = 0
      DO NR = 1, NRB 
        NSL  = NSLNRM(NR)
        IRBYM(1,NR) = NR
        IRBYM(2,NR) = NSL
        ID=NOM_OPT(1,NRBYKIN+NR)
        CALL FRETITL2(TITR,
     .                NOM_OPT(LNOPT1-LTITR+1,NRBYKIN+NR),LTITR)
C---------------------------------
C     CORRECTION DE LA MASSE ET DU
C     CENTRE DE GRAVITE DU MAIN
C---------------------------------
C-----CDG DES NOEUDS SECONDS
         MASRB=ZERO
         XG(1)=ZERO
         XG(2)= ZERO
         XG(3) = ZERO
         DO I=1,9
           RBY(I) = ZERO
         ENDDO  
         DO I=1,NSL
           N=LSN(I + KK)
           LCRBM(I + KK ) = N
           DO J=1,3
             XG(J)    = XG(J) + X(J,N)*MS(N)
           ENDDO
             MASRB   = MASRB + MS(N)
         ENDDO
        
C
          IF(MASRB.LE.EM30) THEN
c            CALL ANSTCKI(RBYID)
            CALL ANCMSG(MSGID=679,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=ID,
     .                  C1=TITR,
     .                  C2='ON SECONDARY NODES')
           RETURN
          ENDIF
C
           RBYM(1,NR) = MASRB
           DO J=1,3
             XG(J)=XG(J)/MASRB
             RBYM(1 + J,NR) = XG(J) 
           ENDDO   
C--------------------------------------
C      L'INERTIE DU MAIN
C--------------------------------------
          DO I=1,NSL
               N=LSN(I + KK)
               XX=(X(1,N) - XG(1))*(X(1,N) - XG(1))
               XY=(X(1,N) - XG(1))*(X(2,N) - XG(2))
               XZ=(X(1,N) - XG(1))*(X(3,N) - XG(3))
               YY=(X(2,N) - XG(2))*(X(2,N) - XG(2))
               YZ=(X(2,N) - XG(2))*(X(3,N) - XG(3))
               ZZ=(X(3,N) - XG(3))*(X(3,N) - XG(3))
               RBY(1) = RBY(1) + IN(N)+(YY+ZZ)*MS(N)
               RBY(2) = RBY(2) - XY*MS(N)
               RBY(3) = RBY(3) - XZ*MS(N)
               RBY(4) = RBY(4) - XY*MS(N)
               RBY(5) = RBY(5) + IN(N)+(ZZ+XX)*MS(N)
               RBY(6) = RBY(6) - YZ*MS(N)
               RBY(7) = RBY(7) - XZ*MS(N)
               RBY(8) = RBY(8) - YZ*MS(N)
               RBY(9) = RBY(9) + IN(N)+(XX+YY)*MS(N)               
          ENDDO
C
C Rigidite au noeud main pour estimation DT.
C
      DO I=1,NSL
        N = LSN(I + KK)
        RBYM(27,NR)= RBYM(27,NR) + STIFN(I + KK)
        DD = (X(1,N)-XG(1))**2 + (X(2,N)-XG(2))**2 + (X(3,N)-XG(3))**2
        RBYM(28,NR) = RBYM(28,NR) + (STIFR(I + KK) + DD*STIFN(I + KK))
      END DO        
C----------------------------------------------------------------
C     CALCUL DU REPERE D'INERTIE PRINCIPALE
C----------------------------------------------------------------
          CALL INEPRI(RBY(10),RBY)
c          IF(ISPH == 1)THEN
c            XIIN = (RBY(10) + RBY(11) + RBY(12)) * THIRD
c            RBY(10) = XIIN
c            RBY(11) = XIIN
c            RBY(12) = XIIN
            WRITE(IOUT,1100) NR,NSL,XG(1),XG(2),XG(3),
     .           MASRB
            write(IOUT, 1300)
            write(IOUT,1400) (LCRBM(I + KK ), I=1,NSL)
c          ELSE
            INMIN = MIN(RBY(10),RBY(11),RBY(12))
c          ENDIF
cc          WRITE(IOUT,1400) RBY(10),RBY(11),RBY(12)
          IF(RBY(10).GE.RBY(11).AND.RBY(10).GE.RBY(12))THEN
            IF(RBY(10).GT.(RBY(11)+RBY(12))*TOL)THEN
             CALL ANCMSG(MSGID=542,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=ID,
     .                   C1=TITR,
     .                   R1=RBY(10),
     .                   R2=RBY(11),
     .                   R3=RBY(12))
            ENDIF
          ELSEIF(RBY(11).GE.RBY(10).AND.RBY(11).GE.RBY(12))THEN
            IF(RBY(11).GT.(RBY(10)+RBY(12))*TOL)THEN
             CALL ANCMSG(MSGID=542,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=ID,
     .                   C1=TITR,
     .                   R1=RBY(11),
     .                   R2=RBY(10),
     .                   R3=RBY(12))
            ENDIF
          ELSEIF(RBY(12).GE.RBY(10).AND.RBY(12).GE.RBY(11))THEN
            IF(RBY(12).GT.(RBY(10)+RBY(11))*TOL)THEN
             CALL ANCMSG(MSGID=542,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=ID,
     .                   C1=TITR,
     .                   R1=RBY(12),
     .                   R2=RBY(10),
     .                   R3=RBY(11))
            ENDIF
          ENDIF
          IF(INMIN.LE.0.0)THEN
C             CALL ANSTCKI(RBYID)
             CALL ANCMSG(MSGID=274,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=ID,
     .                   C1=TITR)
          ELSEIF(INMIN.LE.1.E-10*MAX(RBY(10),RBY(11),RBY(12)))THEN
c             CALL ANSTCKI(RBYID)
             CALL ANCMSG(MSGID=275,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=ID,
     .                   C1=TITR)
          ENDIF
C

            RBYM(5,NR)  = RBY(1)
            RBYM(6,NR)  = RBY(2)
            RBYM(7,NR)  = RBY(3)
            RBYM(8,NR)  = RBY(4)
            RBYM(9,NR)  = RBY(5)
            RBYM(10,NR) = RBY(6)
            RBYM(11,NR) = RBY(7)
            RBYM(12,NR) = RBY(8)
            RBYM(13,NR) = RBY(9)
            RBYM(14,NR) = RBY(10)
            RBYM(15,NR) = RBY(11)
            RBYM(16,NR) = RBY(12)
            RBYM(17,NR) =  MIN(RBY(10),RBY(11),RBY(12))
C             

C MATRICE d'inertie -> repere global
           II1=RBY(10)*RBY(1)
           II2=RBY(10)*RBY(2)
           II3=RBY(10)*RBY(3)
           II4=RBY(11)*RBY(4)
           II5=RBY(11)*RBY(5)
           II6=RBY(11)*RBY(6)
           II7=RBY(12)*RBY(7)
           II8=RBY(12)*RBY(8)
           II9=RBY(12)*RBY(9)
C
           RBYM(18,NR)=RBY(1)*II1 + RBY(4)*II4 + RBY(7)*II7
           RBYM(19,NR)=RBY(1)*II2 + RBY(4)*II5 + RBY(7)*II8
           RBYM(20,NR)=RBY(1)*II3 + RBY(4)*II6 + RBY(7)*II9
           RBYM(21,NR)=RBY(2)*II1 + RBY(5)*II4 + RBY(8)*II7
           RBYM(22,NR)=RBY(2)*II2 + RBY(5)*II5 + RBY(8)*II8
           RBYM(23,NR)=RBY(2)*II3 + RBY(5)*II6 + RBY(8)*II9
           RBYM(24,NR)=RBY(3)*II1 + RBY(6)*II4 + RBY(9)*II7
           RBYM(25,NR)=RBY(3)*II2 + RBY(6)*II5 + RBY(9)*II8
           RBYM(26,NR)=RBY(3)*II3 + RBY(6)*II6 + RBY(9)*II9  
        KK = KK + NSL
      ENDDO     
C
      RETURN
C
1000  FORMAT(//
     . '      RIGID BODY INITIALIZATION '/
     . '      ------------------------- ')
1100  FORMAT(/5X,'RIGID BODY ID',I10
     .       /5X,'NUMBER OF SECONDARY NODE' ,I10
     .       /10X,'NEW X,Y,Z            ',3G14.7
     .       /10X,'NEW MASS             ',1G14.7)
c     .       /10X,'NEW INERTIA xx yy zz ',3G14.7
c     .       /10X,'NEW INERTIA xy yz zx ',3G14.7)
1200  FORMAT(10X,'PRINCIPAL INERTIE ',1P3G14.7)
1300  FORMAT(10X,'SECONDARY NODES ')
1400  FORMAT(10X,10I10)
      END
