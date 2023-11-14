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
Chd|  INIRBY                        source/constraints/general/rbody/inirby.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        CHBAS                         source/constraints/general/rbody/chbas.F
Chd|        INEPRI                        source/materials/mat/mat019/inepri.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|====================================================================
      SUBROUTINE INIRBY(NRB   ,RBY   ,M     ,LPBY  ,
     .                  MS    ,IN    ,X     ,ITAB  ,SKEW  ,
     .                  B1    ,B2    ,B3    ,B5    ,B6    ,
     .                  B9    ,ISPH  ,TOTMAS,XGT   ,YGT   ,
     .                  ZGT   ,STIFN ,STIFR ,NPBY  ,RBYID ,
     .                  V     ,VR    ,ID    ,TITR  ,ITAGND,
     .                  RBY_INIAXIS)
      USE MESSAGE_MOD
C=======================================================================
C    RBY    EN SORTIE DE INIRBY
C     1 -> 9 :  MATRICE ROTATION
C    10 -> 12:  INERTIES PRINCIPALES RBODY
C          13:  INERTIE MAIN INITIALE SPHERIQUE
C          14:  MASSE RBODY
C          15:  MASSE MAIN INITIALE
C    16 -> 20:  LIBRE
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE R2R_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "r2r_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPBY(NNPBY,*), M, ISPH, NRB
      INTEGER ITAB(*), RBYID,ITAGND(*)
      INTEGER, TARGET :: LPBY(*)
C     REAL
      my_real
     .   RBY(NRBY,*), MS(*), IN(*), X(3,*), SKEW(LSKEW,*),
     .   B1, B2, B3, B5, B6, B9,TOTMAS ,XGT ,YGT ,
     .   ZGT, STIFN(*), STIFR(*), V(3,*), VR(3,*),
     .   II1,II2,II3,II4,II5,II6,II7,II8,II9,
     .   RBY_R2R(9),RBY_INIAXIS(7,*)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NSL, J, NOSKEW, I, N, NONOD,ICDG, FLAG,NSL_XTRA
      my_real
     .   XG(3), XM0(3), XMG, XX, XY, XZ, YY, YZ, ZZ, XIIN, INMIN,
     .   MASRB,DD,TOL,
     .   V1X2, V2X1, V2X3, V3X2, V3X1, V1X3,
     .   INMAX,X_MSN0(3),DIST
      INTEGER M_I, OPT_MERGE, RBLEVEL
      INTEGER, DIMENSION(:), POINTER  :: LSN, LSN_XTRA
      my_real, DIMENSION(:), ALLOCATABLE :: MS_LOC,IN_LOC 
C
      TOL=ONE+EM04
C
      NSL = NPBY(2,NRB) ! Nombre total de noeuds SECONDARY 
      LSN => LPBY(NPBY(11,NRB)+1:NPBY(11,NRB)+NSL) ! Liste des noeuds SECONDARY
      RBLEVEL = NPBY(12,NRB)
      ALLOCATE (MS_LOC(NSL))
      ALLOCATE (IN_LOC(NSL))
C
C------------------------------------------------------------------------------------------------------
      FLAG = 0
      DO I=1,NSL
         N=LSN(I)
         MS_LOC(I)=MS(N)
         IN_LOC(I)=IN(N)
      ENDDO
C-----------Multidomaines : on ne compte l'ajout de masse et d'inertie uniquement d'1 cote-------------
      IF ((NSUBDOM>0).AND.(IPID==0)) THEN
        IF (TAGNO(M+N_PART)==3) THEN
             FLAG = 1
             DO J=1,9
               RBY(J,NRB) = ZERO
             END DO
C-----------Multidomaines : on evite de compter 2 fois la masse et l'inertie des noeuds communs--------
           DO I=1,NSL
             N=LSN(I)
             IF (TAGNO(N+N_PART)>3) MS_LOC(I)= 0
             IF (TAGNO(N+N_PART)>3) IN_LOC(I)= 0
             IF (TAGNO(N+N_PART)==0) MS_LOC(I)= 0
             IF (TAGNO(N+N_PART)==0) IN_LOC(I)= 0
           ENDDO
          ENDIF
      ENDIF
C------------------------------------------------------------------------------------------------------
       IF (NS10E>0) THEN
        DO I=1,NSL
          N=LSN(I)
          IF (ITAGND(N)/=0) THEN
           MS_LOC(I)= ZERO
           IN_LOC(I)= ZERO
          END IF 
        ENDDO
       END IF 
C
      NSL_XTRA = NPBY(14,NRB)+NPBY(15,NRB)+NPBY(16,NRB)
      NSL = NPBY(2,NRB) - NSL_XTRA
      LSN => LPBY(NPBY(11,NRB)+1:NPBY(11,NRB)+NSL) ! Liste des noeuds SECONDARY
C------------------------------------------------------------------------------------------------------

      MS(M)=MS(M)+RBY(1,NRB)
       IF(MS(M)==ZERO) MS(M)=EM20

C NUMERO EXTERNE DU NOEUD (DEPLACEE PAR JBM LE 30/9/96)
      NONOD=ITAB(M)
C
      ICDG = NPBY(3,NRB)
      NOSKEW=NPBY(9,NRB)
      RBY(1,NRB)=RBY(2,NRB)
      RBY(9,NRB)=RBY(4,NRB)
      RBY(8,NRB)=RBY(6,NRB)
      RBY(2,NRB)=RBY(5,NRB)
      RBY(4,NRB)=RBY(5,NRB)
      RBY(5,NRB)=RBY(3,NRB)
      RBY(3,NRB)=RBY(7,NRB)
C CAS DU REPERE SKEW POUR LE RB
      IF(NOSKEW/=0) THEN
        CALL CHBAS(SKEW(1,NOSKEW),RBY(1,NRB))
      ENDIF
C
      RBY(1,NRB)=RBY(1,NRB)+IN(M)
      RBY(5,NRB)=RBY(5,NRB)+IN(M)
      RBY(9,NRB)=RBY(9,NRB)+IN(M)
      IN(M) = (RBY(1,NRB) + RBY(5,NRB) + RBY(9,NRB)) * THIRD
C
C---------------------------------
C     CORRECTION DE LA MASSE ET DU
C     CENTRE DE GRAVITE DU MAIN
C---------------------------------
C
      XMG=MS(M)
C
C-----INITIAL COORDINATES ARE STORED FOR INIVEL/AXIS correction
      IF (RBY_INIAXIS(1,NRB) > 0) THEN
        DO J=1,3
          X_MSN0(J)=X(J,M)
        ENDDO
      ENDIF

C-----CDG DES NOEUDS SECONDS + MAIN
      IF(ICDG==1)THEN
        MASRB=MS(M)
        DO J=1,3
          XG(J)=X(J,M)
          X(J,M)=X(J,M)*MS(M)
        ENDDO
        DO I=1,NSL
         N=LSN(I)
         DO J=1,3
          X(J,M)    = X(J,M)+X(J,N)*MS_LOC(I)
         ENDDO
         MASRB   = MASRB+MS_LOC(I)
        ENDDO
C
        IF(MASRB<=1.E-30) THEN
           CALL ANCMSG(MSGID=679,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR)
           RETURN
        ENDIF
C
        DO J=1,3
          X(J,M)=X(J,M)/MASRB
        ENDDO
C
C-----CDG DES NOEUDS SECONDS
      ELSEIF(ICDG==2)THEN
        MASRB=ZERO
        DO J=1,3
          X(J,M)=ZERO
        ENDDO
        DO I=1,NSL
         N=LSN(I)
         DO J=1,3
          X(J,M)    = X(J,M)+X(J,N)*MS_LOC(I)
         ENDDO
         MASRB   = MASRB+MS_LOC(I)
        ENDDO
          IF (FLAG==1) MASRB = MAX(MASRB,EM20)
C
        IF(MASRB<=EM30) THEN
           CALL ANCMSG(MSGID=679,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 C2='ON SECONDARY NODES')
           RETURN
        ENDIF
C
        DO J=1,3
          X(J,M)=X(J,M)/MASRB
          XG(J)=X(J,M)
        ENDDO
C
        MASRB=MASRB+MS(M)
C
C-----CDG DU NOEUD MAIN
      ELSEIF(ICDG==3)THEN
        DO J=1,3
          XG(J)=X(J,M)
        ENDDO
        MASRB=MS(M)
        DO I=1,NSL
         N=LSN(I)
         MASRB   = MASRB+MS_LOC(I)
        ENDDO
C
        IF(MASRB<=EM30) THEN
           CALL ANCMSG(MSGID=679,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                   I1=ID,
     .                   C1=TITR)
           RETURN
        ENDIF
C
C-----CDG DU NOEUD MAIN (MASSE DES SECONDS IGNOREE)
      ELSEIF(ICDG==4)THEN
        DO J=1,3
          XG(J)=X(J,M)
        ENDDO
        MASRB=MS(M)
C
        IF(MASRB<=EM30) THEN
           CALL ANCMSG(MSGID=679,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                   I1=ID,
     .                   C1=TITR,
     .                 C2='ON MAIN NODE')
          RETURN
        ENDIF
C
      ENDIF

C--------------------------------------
C     ASSEMBLAGE DES XTRA NODES DANS LA MASSE ET COG
C--------------------------------------
      IF(NPBY(15,NRB) > 0) THEN ! MASS/INERTIA ADD, COG ACTUALIZED
        LSN_XTRA => LPBY(NPBY(11,NRB)+NSL+NPBY(14,NRB)+1:
     .                   NPBY(11,NRB)+NSL+NPBY(14,NRB)+NPBY(15,NRB))
        DO J=1,3
          XG(J)=X(J,M)
          X(J,M)=X(J,M)*MASRB
        ENDDO
        XMG=MASRB
C
        DO I=1,NPBY(15,NRB)
          N=LSN_XTRA(I)
          DO J=1,3
            X(J,M)    = X(J,M)+X(J,N)*MS_LOC(NSL+I)
          ENDDO
          MASRB   = MASRB+MS_LOC(NSL+I)
        ENDDO
C    
        DO J=1,3
          X(J,M)=X(J,M)/MASRB
          XG(J)=X(J,M)          
        ENDDO
      ENDIF
C
      IF(NPBY(16,NRB) > 0) THEN ! MASS/INERTIA ADD, COG NOT ACTUALIZED
        LSN_XTRA => LPBY(NPBY(11,NRB)+NSL+NPBY(14,NRB)+NPBY(15,NRB)+1:
     .                   NPBY(11,NRB)+NSL+NPBY(14,NRB)+NPBY(15,NRB)+NPBY(16,NRB))
        DO I=1,NPBY(16,NRB)
          N=LSN_XTRA(I)
          MASRB   = MASRB+MS_LOC(NSL+I)
        ENDDO
      ENDIF
C 
C--------------------------------------
C     CORRECTION DE L'INERTIE DU MAIN
C--------------------------------------
      IF(ICDG<=3)THEN
        IF(N2D==0)THEN
C        ANALYSE 3D
          XX=(XG(1)-X(1,M))*(XG(1)-X(1,M))
          XY=(XG(1)-X(1,M))*(XG(2)-X(2,M))
          XZ=(XG(1)-X(1,M))*(XG(3)-X(3,M))
          YY=(XG(2)-X(2,M))*(XG(2)-X(2,M))
          YZ=(XG(2)-X(2,M))*(XG(3)-X(3,M))
          ZZ=(XG(3)-X(3,M))*(XG(3)-X(3,M))
          RBY(1,NRB)=RBY(1,NRB)+(YY+ZZ)*XMG
          RBY(2,NRB)=RBY(2,NRB)-XY*XMG
          RBY(3,NRB)=RBY(3,NRB)-XZ*XMG
          RBY(4,NRB)=RBY(4,NRB)-XY*XMG
          RBY(5,NRB)=RBY(5,NRB)+(ZZ+XX)*XMG
          RBY(6,NRB)=RBY(6,NRB)-YZ*XMG
          RBY(7,NRB)=RBY(7,NRB)-XZ*XMG
          RBY(8,NRB)=RBY(8,NRB)-YZ*XMG
          RBY(9,NRB)=RBY(9,NRB)+(XX+YY)*XMG

            IF (NSL==1) THEN
            RBY(1,NRB)=RBY(1,NRB)+EM20
            RBY(5,NRB)=RBY(5,NRB)+EM20
            RBY(9,NRB)=RBY(9,NRB)+EM20
            ENDIF
C
          DO I=1,NSL
           N=LSN(I)
           XX=(X(1,N)-X(1,M))*(X(1,N)-X(1,M))
           XY=(X(1,N)-X(1,M))*(X(2,N)-X(2,M))
           XZ=(X(1,N)-X(1,M))*(X(3,N)-X(3,M))
           YY=(X(2,N)-X(2,M))*(X(2,N)-X(2,M))
           YZ=(X(2,N)-X(2,M))*(X(3,N)-X(3,M))
           ZZ=(X(3,N)-X(3,M))*(X(3,N)-X(3,M))
           RBY(1,NRB)=RBY(1,NRB)+IN_LOC(I)+(YY+ZZ)*MS_LOC(I)
           RBY(2,NRB)=RBY(2,NRB)-XY*MS_LOC(I)
           RBY(3,NRB)=RBY(3,NRB)-XZ*MS_LOC(I)
           RBY(4,NRB)=RBY(4,NRB)-XY*MS_LOC(I)
           RBY(5,NRB)=RBY(5,NRB)+IN_LOC(I)+(ZZ+XX)*MS_LOC(I)
           RBY(6,NRB)=RBY(6,NRB)-YZ*MS_LOC(I)
           RBY(7,NRB)=RBY(7,NRB)-XZ*MS_LOC(I)
           RBY(8,NRB)=RBY(8,NRB)-YZ*MS_LOC(I)
           RBY(9,NRB)=RBY(9,NRB)+IN_LOC(I)+(XX+YY)*MS_LOC(I)
          ENDDO

        ELSEIF(N2D==1) THEN
C        ANALYSE 2D : Axisymmetry  
C             I= A 0 0
C                0 A 0
C                0 0 B 
          YY=(XG(2)-X(2,M))*(XG(2)-X(2,M))
          ZZ=(XG(3)-X(3,M))*(XG(3)-X(3,M))
          RBY(1,NRB)=RBY(1,NRB)+(YY+ZZ)*XMG
          RBY(2,NRB)=ZERO
          RBY(3,NRB)=ZERO
C
          RBY(4,NRB)=ZERO
          RBY(5,NRB)=RBY(5,NRB)+ZZ*XMG
          RBY(6,NRB)=ZERO
C
          RBY(7,NRB)=ZERO
          RBY(8,NRB)=ZERO
          RBY(9,NRB)=RBY(9,NRB)+YY*XMG

            IF (NSL==1) THEN
            RBY(1,NRB)=RBY(1,NRB)+EM20
            RBY(5,NRB)=RBY(5,NRB)+EM20
            RBY(9,NRB)=RBY(9,NRB)+EM20
            ENDIF
C
          DO I=1,NSL
           N=LSN(I)
           YY=(X(2,N)-X(2,M))*(X(2,N)-X(2,M))
           ZZ=(X(3,N)-X(3,M))*(X(3,N)-X(3,M))
           RBY(1,NRB)=RBY(1,NRB)+IN_LOC(I)+(YY+ZZ)*MS_LOC(I)
           RBY(5,NRB)=RBY(5,NRB)+IN_LOC(I)+ZZ*MS_LOC(I)
           RBY(9,NRB)=RBY(9,NRB)+IN_LOC(I)+YY*MS_LOC(I)
          ENDDO
         ELSEIF(N2D==2) THEN
C        ANALYSE 2D : Plane strain Inertia matrix 
C             I= A 0 0
C                0 B D
C                0 D C 
          YY=(XG(2)-X(2,M))*(XG(2)-X(2,M))
          YZ=(XG(2)-X(2,M))*(XG(3)-X(3,M))
          ZZ=(XG(3)-X(3,M))*(XG(3)-X(3,M))
          RBY(1,NRB)=RBY(1,NRB)+(YY+ZZ)*XMG
          RBY(2,NRB)=ZERO
          RBY(3,NRB)=ZERO
c
          RBY(4,NRB)=ZERO
          RBY(5,NRB)=RBY(5,NRB)+ZZ*XMG
          RBY(6,NRB)=RBY(6,NRB)-YZ*XMG
c
          RBY(7,NRB)=ZERO
          RBY(8,NRB)=RBY(8,NRB)-YZ*XMG
          RBY(9,NRB)=RBY(9,NRB)+YY*XMG

            IF (NSL==1) THEN
            RBY(1,NRB)=RBY(1,NRB)+EM20
            RBY(5,NRB)=RBY(5,NRB)+EM20
            RBY(9,NRB)=RBY(9,NRB)+EM20
            ENDIF
C
          DO I=1,NSL
           N=LSN(I)
           YY=(X(2,N)-X(2,M))*(X(2,N)-X(2,M))
           YZ=(X(2,N)-X(2,M))*(X(3,N)-X(3,M))
           ZZ=(X(3,N)-X(3,M))*(X(3,N)-X(3,M))
           RBY(1,NRB)=RBY(1,NRB)+IN_LOC(I)+(YY+ZZ)*MS_LOC(I)
c
           RBY(5,NRB)=RBY(5,NRB)+IN_LOC(I)+ZZ*MS_LOC(I)
           RBY(6,NRB)=RBY(6,NRB)-YZ*MS_LOC(I)
c
           RBY(8,NRB)=RBY(8,NRB)-YZ*MS_LOC(I)
           RBY(9,NRB)=RBY(9,NRB)+IN_LOC(I)+YY*MS_LOC(I)
          ENDDO
        ENDIF !N2D
      ENDIF
c
C--------------------------------------
C     AJOUT DE L'INERTIE DES XTRA NODES
C--------------------------------------
      IF((NPBY(15,NRB)+NPBY(16,NRB)) > 0) THEN

        LSN_XTRA => LPBY(NPBY(11,NRB)+NSL+NPBY(14,NRB)+1:
     .                   NPBY(11,NRB)+NSL+NPBY(14,NRB)+NPBY(15,NRB)+NPBY(16,NRB))

        IF(N2D==0)THEN
         DO I=1,NPBY(15,NRB)+NPBY(16,NRB)
          N=LSN_XTRA(I)
          XX=(X(1,N)-X(1,M))*(X(1,N)-X(1,M))
          XY=(X(1,N)-X(1,M))*(X(2,N)-X(2,M))
          XZ=(X(1,N)-X(1,M))*(X(3,N)-X(3,M))
          YY=(X(2,N)-X(2,M))*(X(2,N)-X(2,M))
          YZ=(X(2,N)-X(2,M))*(X(3,N)-X(3,M))
          ZZ=(X(3,N)-X(3,M))*(X(3,N)-X(3,M))
          RBY(1,NRB)=RBY(1,NRB)+IN_LOC(NSL+I)+(YY+ZZ)*MS_LOC(NSL+I)
          RBY(2,NRB)=RBY(2,NRB)-XY*MS_LOC(NSL+I)
          RBY(3,NRB)=RBY(3,NRB)-XZ*MS_LOC(NSL+I)
          RBY(4,NRB)=RBY(4,NRB)-XY*MS_LOC(NSL+I)
          RBY(5,NRB)=RBY(5,NRB)+IN_LOC(NSL+I)+(ZZ+XX)*MS_LOC(NSL+I)
          RBY(6,NRB)=RBY(6,NRB)-YZ*MS_LOC(NSL+I)
          RBY(7,NRB)=RBY(7,NRB)-XZ*MS_LOC(NSL+I)
          RBY(8,NRB)=RBY(8,NRB)-YZ*MS_LOC(NSL+I)
          RBY(9,NRB)=RBY(9,NRB)+IN_LOC(NSL+I)+(XX+YY)*MS_LOC(NSL+I)
         ENDDO
        ELSEIF(N2D==1) THEN
         DO I=1,NPBY(15,NRB)+NPBY(16,NRB)
          N=LSN_XTRA(I) 
          YY=(X(2,N)-X(2,M))*(X(2,N)-X(2,M))
          ZZ=(X(3,N)-X(3,M))*(X(3,N)-X(3,M))
          RBY(1,NRB)=RBY(1,NRB)+IN_LOC(NSL+I)+(YY+ZZ)*MS_LOC(NSL+I)
          RBY(5,NRB)=RBY(5,NRB)+IN_LOC(NSL+I)+ZZ*MS_LOC(NSL+I)
          RBY(9,NRB)=RBY(9,NRB)+IN_LOC(NSL+I)+YY*MS_LOC(NSL+I)
         ENDDO
        ELSEIF(N2D==1) THEN
         DO I=1,NPBY(15,NRB)+NPBY(16,NRB)
          N=LSN_XTRA(I) 
          YY=(X(2,N)-X(2,M))*(X(2,N)-X(2,M))
          YZ=(X(2,N)-X(2,M))*(X(3,N)-X(3,M))
          ZZ=(X(3,N)-X(3,M))*(X(3,N)-X(3,M))
          RBY(1,NRB)=RBY(1,NRB)+IN_LOC(NSL+I)+(YY+ZZ)*MS_LOC(NSL+I)
          RBY(5,NRB)=RBY(5,NRB)+IN_LOC(NSL+I)+ZZ*MS_LOC(NSL+I)
          RBY(6,NRB)=RBY(6,NRB)-YZ*MS_LOC(NSL+I)
          RBY(8,NRB)=RBY(8,NRB)-YZ*MS_LOC(NSL+I)
          RBY(9,NRB)=RBY(9,NRB)+IN_LOC(NSL+I)+YY*MS_LOC(NSL+I)
         ENDDO
        ENDIF !N2D
      ENDIF
C
C--------------------------------------
C     ASSEMBLAGE DES MASSES DES RIGID BODY SECONDARYS ET MAJ POSITION DU COG
C--------------------------------------
      DO I=NRB-1,1,-1
        IF((NPBY(12,I)) == RBLEVEL-1) THEN ! Merge with Rb level n-1 
          DO J=1,3
            XG(J)=X(J,M)  ! centre de gravite du rb main avant modif
          ENDDO
          XMG=MASRB  ! masse du RB main avant modif
c
          OPT_MERGE = NPBY(13,I)
          NPBY(11,NRB) = NPBY(11,I) 
          NPBY(2,NRB) = NPBY(2,NRB)+NPBY(2,I) 
          M_I=NPBY(1,I)
c
          IF(OPT_MERGE == 2) THEN ! MASSE ET INERTIE AJOUTEE, COG ACTUALISE
            DO J=1,3
             X(J,M)=X(J,M)*MASRB+X(J,M_I)*RBY(14,I)
            ENDDO
            MASRB = MASRB + RBY(14,I)
            DO J=1,3
              X(J,M)=X(J,M)/MASRB
            ENDDO
          ELSEIF(OPT_MERGE == 3) THEN ! MASSE ET INERTIE AJOUTEE, COG NON ACTUALISE
            MASRB = MASRB + RBY(14,I)
          ENDIF         
c
C--------------------------------------
C     TRANSFERT DES INERTIES AU COG
C--------------------------------------
          IF(OPT_MERGE == 2) THEN
           IF(N2D==0)THEN
            XX=(XG(1)-X(1,M))*(XG(1)-X(1,M))
            XY=(XG(1)-X(1,M))*(XG(2)-X(2,M))
            XZ=(XG(1)-X(1,M))*(XG(3)-X(3,M))
            YY=(XG(2)-X(2,M))*(XG(2)-X(2,M))
            YZ=(XG(2)-X(2,M))*(XG(3)-X(3,M))
            ZZ=(XG(3)-X(3,M))*(XG(3)-X(3,M))
            RBY(1,NRB)=RBY(1,NRB)+(YY+ZZ)*XMG
            RBY(2,NRB)=RBY(2,NRB)-XY*XMG
            RBY(3,NRB)=RBY(3,NRB)-XZ*XMG
            RBY(4,NRB)=RBY(4,NRB)-XY*XMG
            RBY(5,NRB)=RBY(5,NRB)+(ZZ+XX)*XMG
            RBY(6,NRB)=RBY(6,NRB)-YZ*XMG
            RBY(7,NRB)=RBY(7,NRB)-XZ*XMG
            RBY(8,NRB)=RBY(8,NRB)-YZ*XMG
            RBY(9,NRB)=RBY(9,NRB)+(XX+YY)*XMG
           ELSEIF(N2D==1) THEN
            YY=(XG(2)-X(2,M))*(XG(2)-X(2,M))
            ZZ=(XG(3)-X(3,M))*(XG(3)-X(3,M))
            RBY(1,NRB)=RBY(1,NRB)+(YY+ZZ)*XMG
            RBY(2,NRB)=ZERO
            RBY(3,NRB)=ZERO
            RBY(4,NRB)=ZERO
            RBY(5,NRB)=RBY(5,NRB)+ZZ*XMG
            RBY(6,NRB)=ZERO
            RBY(7,NRB)=ZERO
            RBY(8,NRB)=ZERO
            RBY(9,NRB)=RBY(9,NRB)+YY*XMG
           ELSEIF(N2D==2) THEN  
            YY=(XG(2)-X(2,M))*(XG(2)-X(2,M))
            YZ=(XG(2)-X(2,M))*(XG(3)-X(3,M))
            ZZ=(XG(3)-X(3,M))*(XG(3)-X(3,M))
            RBY(1,NRB)=RBY(1,NRB)+(YY+ZZ)*XMG
            RBY(2,NRB)=ZERO
            RBY(3,NRB)=ZERO
            RBY(4,NRB)=ZERO
            RBY(5,NRB)=RBY(5,NRB)+ZZ*XMG
            RBY(6,NRB)=RBY(6,NRB)-YZ*XMG
            RBY(7,NRB)=ZERO
            RBY(8,NRB)=RBY(8,NRB)-YZ*XMG
            RBY(9,NRB)=RBY(9,NRB)+YY*XMG
           ENDIF
          ENDIF
          IF(OPT_MERGE > 1) THEN
           IF(N2D==0)THEN
            XX=(X(1,M_I)-X(1,M))*(X(1,M_I)-X(1,M))  
            XY=(X(1,M_I)-X(1,M))*(X(2,M_I)-X(2,M))
            XZ=(X(1,M_I)-X(1,M))*(X(3,M_I)-X(3,M))
            YY=(X(2,M_I)-X(2,M))*(X(2,M_I)-X(2,M))
            YZ=(X(2,M_I)-X(2,M))*(X(3,M_I)-X(3,M))
            ZZ=(X(3,M_I)-X(3,M))*(X(3,M_I)-X(3,M))
            RBY(1,NRB)=RBY(1,NRB)+RBY(1,I)+(YY+ZZ)*RBY(14,I)
            RBY(2,NRB)=RBY(2,NRB)+RBY(2,I)-XY*RBY(14,I)
            RBY(3,NRB)=RBY(3,NRB)+RBY(3,I)-XZ*RBY(14,I)
            RBY(4,NRB)=RBY(4,NRB)+RBY(4,I)-XY*RBY(14,I)
            RBY(5,NRB)=RBY(5,NRB)+RBY(5,I)+(ZZ+XX)*RBY(14,I)
            RBY(6,NRB)=RBY(6,NRB)+RBY(6,I)-YZ*RBY(14,I)
            RBY(7,NRB)=RBY(7,NRB)+RBY(7,I)-XZ*RBY(14,I)
            RBY(8,NRB)=RBY(8,NRB)+RBY(8,I)-YZ*RBY(14,I)
            RBY(9,NRB)=RBY(9,NRB)+RBY(9,I)+(XX+YY)*RBY(14,I) 
           ELSEIF(N2D==1) THEN
            YY=(X(2,M_I)-X(2,M))*(X(2,M_I)-X(2,M))
            ZZ=(X(3,M_I)-X(3,M))*(X(3,M_I)-X(3,M))
            RBY(1,NRB)=RBY(1,NRB)+RBY(1,I)+(YY+ZZ)*RBY(14,I)
            RBY(5,NRB)=RBY(5,NRB)+RBY(5,I)+ZZ*RBY(14,I)
            RBY(9,NRB)=RBY(9,NRB)+RBY(9,I)+YY*RBY(14,I)
           ELSEIF(N2D==2) THEN
            YY=(X(2,M_I)-X(2,M))*(X(2,M_I)-X(2,M))
            YZ=(X(2,M_I)-X(2,M))*(X(3,M_I)-X(3,M))
            ZZ=(X(3,M_I)-X(3,M))*(X(3,M_I)-X(3,M))
            RBY(1,NRB)=RBY(1,NRB)+RBY(1,I)+(YY+ZZ)*RBY(14,I)
            RBY(5,NRB)=RBY(5,NRB)+RBY(5,I)+ZZ*RBY(14,I)
            RBY(6,NRB)=RBY(6,NRB)-YZ*RBY(14,I)
            RBY(8,NRB)=RBY(8,NRB)-YZ*RBY(14,I)
            RBY(9,NRB)=RBY(9,NRB)+RBY(9,I)+YY*RBY(14,I)
           ENDIF !N2D
          ENDIF   
c         mise a zero des donnees rb SECONDARY
          RBY(1:15,I)=ZERO           
          IN(M_I)=ZERO 
          MS(M_I)=ZERO    
        ELSEIF(NPBY(12,I) == RBLEVEL) THEN ! End of the RB branch 
          EXIT
        ENDIF
      ENDDO
c
C----------------------------------------------
C    NSL est complete par les noeuds SECONDARY des rigid body SECONDARYs
C    LSN est actualisee
C----------------------------------------------
      IF(RBLEVEL == 0) THEN ! IF THIS RIGID BODY IS THE TOP MAIN
        IF(NRBMERGE > 0) THEN
          DEALLOCATE(MS_LOC)
          DEALLOCATE(IN_LOC)
          NSL = NPBY(2,NRB) ! Liste totale des noeuds SECONDARY
          LSN => LPBY(NPBY(11,NRB)+1:NPBY(11,NRB)+NSL)
          ALLOCATE (MS_LOC(NSL))
          ALLOCATE (IN_LOC(NSL))
C
C-----------Multidomaines : on ne compte l'ajout de masse et d'inertie uniquement d'1 cote-------------
          IF ((NSUBDOM>0).AND.(IPID==0)) THEN
            IF (TAGNO(M+N_PART)==3) THEN
C-----------Multidomaines : on evite de compter 2 fois la masse et l'inertie des noeuds communs--------
               DO I=1,NSL
                 N=LSN(I)
                 IF (TAGNO(N+N_PART)>3) MS_LOC(I)= 0
                 IF (TAGNO(N+N_PART)>3) IN_LOC(I)= 0
                 IF (TAGNO(N+N_PART)==0) MS_LOC(I)= 0
                 IF (TAGNO(N+N_PART)==0) IN_LOC(I)= 0
              ENDDO
              ENDIF
          ENDIF
C
          IF (NS10E>0) THEN
            DO I=1,NSL
              N=LSN(I)
              IF (ITAGND(N)/=0) THEN
               MS_LOC(I)= ZERO
               IN_LOC(I)= ZERO
              ELSE
               MS_LOC(I)=MS(N)
               IN_LOC(I)=IN(N)
              ENDIF
            ENDDO
          ELSE
            DO I=1,NSL
              N=LSN(I)
              MS_LOC(I)=MS(N)
              IN_LOC(I)=IN(N)
            ENDDO
          ENDIF
        ENDIF
C
C-----------Multidomaines : on stoke la matrice assembl   e--------------
        IF(NSUBDOM>0)THEN
          IF(TAGNO(M+N_PART)==3)THEN
            DO J=1,9
                RBY_R2R(J)=RBY(J,NRB)
              END DO
          END IF
        ENDIF

C----------------------------------------------
C     MISE A ZERO DES MASSES ET INERTIES SECONDARYS
C----------------------------------------------
C      DO I=1,NSL
C       N=LSN(I)
C       RBYM(I) = MS(N)
C       RBYI(I) = IN(N)
C       IN(N)=0.
C       MS(N)=0.
C      ENDDO
C
        WRITE(IOUT,1000)
        DO I=1,NSL
         N=LSN(I)
         XX=(X(1,N))*(X(1,N))
         XY=(X(1,N))*(X(2,N))
         XZ=(X(1,N))*(X(3,N))
         YY=(X(2,N))*(X(2,N))
         YZ=(X(2,N))*(X(3,N))
         ZZ=(X(3,N))*(X(3,N))
         B1 = B1 - IN_LOC(I)-(YY+ZZ)*MS_LOC(I)
         B2 = B2 + XY*MS_LOC(I)
         B3 = B3 + XZ*MS_LOC(I)
         B5 = B5 - IN_LOC(I)-(ZZ+XX)*MS_LOC(I)
         B6 = B6 + YZ*MS_LOC(I)
         B9 = B9 - IN_LOC(I)-(XX+YY)*MS_LOC(I)
         XGT = XGT - MS_LOC(I)*X(1,N)
         YGT = YGT - MS_LOC(I)*X(2,N)
         ZGT = ZGT - MS_LOC(I)*X(3,N)
         TOTMAS = TOTMAS - MS_LOC(I)
        ENDDO
        IF(ISPH/=1)THEN
         B1 = B1 + RBY(1,NRB)
         B2 = B2 + RBY(2,NRB)
         B3 = B3 + RBY(3,NRB)
         B5 = B5 + RBY(5,NRB)
         B6 = B6 + RBY(6,NRB)
         B9 = B9 + RBY(9,NRB)
         IF(NSUBDOM>0)THEN
           IF(TAGNO(M+N_PART)==3) THEN
             WRITE(IOUT,1300) RBYID
             WRITE(IOUT,1400)
           ELSE
             WRITE(IOUT,1100) RBYID,NONOD,X(1,M),X(2,M),X(3,M),
     .             MASRB,RBY(1,NRB),RBY(5,NRB),RBY(9,NRB),RBY(2,NRB),RBY(6,NRB),RBY(3,NRB)
           END IF
         ELSE
             WRITE(IOUT,1100) RBYID,NONOD,X(1,M),X(2,M),X(3,M),
     .             MASRB,RBY(1,NRB),RBY(5,NRB),RBY(9,NRB),RBY(2,NRB),RBY(6,NRB),RBY(3,NRB)
          END IF
        ENDIF

C----------------------------------------------------------------
C     CALCUL DU REPERE D'INERTIE PRINCIPALE
C----------------------------------------------------------------
        IF(N2D == 1) THEN
          RBY(10,NRB) = RBY(1,NRB)
          RBY(11,NRB) = RBY(5,NRB)
          RBY(12,NRB) = RBY(9,NRB)
          RBY(1,NRB) = ONE
          RBY(5,NRB) = ONE
          RBY(9,NRB) = ONE
        ELSE
          CALL INEPRI(RBY(10,NRB),RBY(1,NRB))
        ENDIF
C
        IF(ISPH==1)THEN
          XIIN = (RBY(10,NRB) + RBY(11,NRB) + RBY(12,NRB)) * THIRD
          RBY(10,NRB) = XIIN
          RBY(11,NRB) = XIIN
          RBY(12,NRB) = XIIN
          INMIN   = XIIN
          B1 = B1 + XIIN
          B5 = B5 + XIIN
          B9 = B9 + XIIN
          IF(NSUBDOM>0)THEN
            IF(TAGNO(M+N_PART)==3)THEN
              WRITE(IOUT,1300) RBYID
              WRITE(IOUT,1400)
            ELSE
              WRITE(IOUT,1100) RBYID,NONOD,X(1,M),X(2,M),X(3,M),
     .                MASRB,XIIN,XIIN,XIIN,ZERO,ZERO,ZERO
            END IF
          ELSE
            WRITE(IOUT,1100) RBYID,NONOD,X(1,M),X(2,M),X(3,M),
     .              MASRB,XIIN,XIIN,XIIN,ZERO,ZERO,ZERO
          ENDIF
        ELSEIF (ISPH==2) THEN
          INMIN = MIN(RBY(10,NRB),RBY(11,NRB),RBY(12,NRB))
          INMAX = MAX(RBY(10,NRB),RBY(11,NRB),RBY(12,NRB))
          IF(INMIN<=1.E-3*INMAX)THEN
            IF(RBY(10,NRB)/INMAX<EM03) RBY(10,NRB)=RBY(10,NRB)+EM01*INMAX
            IF(RBY(11,NRB)/INMAX<EM03) RBY(11,NRB)=RBY(11,NRB)+EM01*INMAX
            IF(RBY(12,NRB)/INMAX<EM03) RBY(12,NRB)=RBY(12,NRB)+EM01*INMAX
            IF(NSUBDOM>0)THEN          
              IF(TAGNO(M+N_PART) /= 3) THEN
                CALL ANCMSG(MSGID=275,
     .                      MSGTYPE=MSGWARNING,
     .                      ANMODE=ANINFO_BLIND_1,
     .                      I1=ID,
     .                      C1=TITR)
              ENDIF
            ELSE
              CALL ANCMSG(MSGID=275,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=ID,
     .                    C1=TITR)
            ENDIF
          ENDIF
        ENDIF

      
        IF(NSUBDOM>0)THEN
          IF(TAGNO(M+N_PART)==3)GOTO 350
        END IF


        INMIN = MIN(RBY(10,NRB),RBY(11,NRB),RBY(12,NRB))
        WRITE(IOUT,1200) RBY(10,NRB),RBY(11,NRB),RBY(12,NRB)
        WRITE(IOUT,1101)
        WRITE(IOUT,1102) (ITAB(LPBY(I+NPBY(11,NRB))),I=1,NSL)

        IF(RBY(10,NRB)>=RBY(11,NRB).AND.RBY(10,NRB)>=RBY(12,NRB))THEN
          IF(RBY(10,NRB)>(RBY(11,NRB)+RBY(12,NRB))*TOL)THEN
           CALL ANCMSG(MSGID=542,
     .                 MSGTYPE=MSGWARNING,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 R1=RBY(10,NRB),
     .                 R2=RBY(11,NRB),
     .                 R3=RBY(12,NRB))
          ENDIF
        ELSEIF(RBY(11,NRB)>=RBY(10,NRB).AND.RBY(11,NRB)>=RBY(12,NRB))THEN
          IF(RBY(11,NRB)>(RBY(10,NRB)+RBY(12,NRB))*TOL)THEN
           CALL ANCMSG(MSGID=542,
     .                 MSGTYPE=MSGWARNING,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 R1=RBY(11,NRB),
     .                 R2=RBY(10,NRB),
     .                 R3=RBY(12,NRB))
          ENDIF
        ELSEIF(RBY(12,NRB)>=RBY(10,NRB).AND.RBY(12,NRB)>=RBY(11,NRB))THEN
          IF(RBY(12,NRB)>(RBY(10,NRB)+RBY(11,NRB))*TOL)THEN
           CALL ANCMSG(MSGID=542,
     .                 MSGTYPE=MSGWARNING,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 R1=RBY(12,NRB),
     .                 R2=RBY(10,NRB),
     .                 R3=RBY(11,NRB))
          ENDIF
        ENDIF

        IF(INMIN<=0.0)THEN
          CALL ANCMSG(MSGID=274,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID,
     .                C1=TITR)
        ENDIF
      ENDIF ! (RBLEVEL == 0)
C
350   CONTINUE
C
      RBY(13,NRB)=IN(M)
      RBY(14,NRB)=MASRB
      RBY(15,NRB)=MS(M)
      MS(M) = MASRB
      IN(M) = MIN(RBY(10,NRB),RBY(11,NRB),RBY(12,NRB))
C
      DEALLOCATE(MS_LOC)
      DEALLOCATE(IN_LOC)
c
C----------------------------------------------------------------
C     TRAITEMENT DU RIGID BODY QUI N'A PAS DE MAIN
C----------------------------------------------------------------
c
      IF(RBLEVEL == 0) THEN ! IF THIS RIGID BODY IS THE TOP MAIN
C
       IF(N2D == 0) THEN
         XX=(X(1,M))*(X(1,M))
         XY=(X(1,M))*(X(2,M))
         XZ=(X(1,M))*(X(3,M))
         YY=(X(2,M))*(X(2,M))
         YZ=(X(2,M))*(X(3,M))
         ZZ=(X(3,M))*(X(3,M))
         B1 = B1 - IN(M)
         B5 = B5 - IN(M)
         B9 = B9 - IN(M)
         TOTMAS = TOTMAS - MS(M) + MASRB
         XGT = XGT - MS(M)*X(1,M) + MASRB*X(1,M)
         YGT = YGT - MS(M)*X(2,M) + MASRB*X(2,M)
         ZGT = ZGT - MS(M)*X(3,M) + MASRB*X(3,M)
C
C Rigidite au noeud main pour estimation DT.
         IF (NS10E>0) THEN
           DO I=1,NSL
             N = LSN(I)
             IF (ITAGND(N)/=0) CYCLE
             STIFN(M)= STIFN(M)+STIFN(N)
             DD = (X(1,N)-X(1,M))**2+(X(2,N)-X(2,M))**2+(X(3,N)-X(3,M))**2
             STIFR(M)= STIFR(M)+(STIFR(N)+DD*STIFN(N))
             STIFR(N)= EM20
             STIFN(N)= EM20
           END DO
         ELSE
           DO I=1,NSL
             N = LSN(I)
             STIFN(M)= STIFN(M)+STIFN(N)
             DD = (X(1,N)-X(1,M))**2+(X(2,N)-X(2,M))**2+(X(3,N)-X(3,M))**2
             STIFR(M)= STIFR(M)+(STIFR(N)+DD*STIFN(N))
             STIFR(N)= EM20
             STIFN(N)= EM20
            END DO
         END IF !(NS10E>0) THEN

C MATRICE d'inertie -> repere global
         II1=RBY(10,NRB)*RBY(1,NRB)
         II2=RBY(10,NRB)*RBY(2,NRB)
         II3=RBY(10,NRB)*RBY(3,NRB)
         II4=RBY(11,NRB)*RBY(4,NRB)
         II5=RBY(11,NRB)*RBY(5,NRB)
         II6=RBY(11,NRB)*RBY(6,NRB)
         II7=RBY(12,NRB)*RBY(7,NRB)
         II8=RBY(12,NRB)*RBY(8,NRB)
         II9=RBY(12,NRB)*RBY(9,NRB)
C
         RBY(17,NRB)=RBY(1,NRB)*II1 + RBY(4,NRB)*II4 + RBY(7,NRB)*II7
         RBY(18,NRB)=RBY(1,NRB)*II2 + RBY(4,NRB)*II5 + RBY(7,NRB)*II8
         RBY(19,NRB)=RBY(1,NRB)*II3 + RBY(4,NRB)*II6 + RBY(7,NRB)*II9
         RBY(20,NRB)=RBY(2,NRB)*II1 + RBY(5,NRB)*II4 + RBY(8,NRB)*II7
         RBY(21,NRB)=RBY(2,NRB)*II2 + RBY(5,NRB)*II5 + RBY(8,NRB)*II8
         RBY(22,NRB)=RBY(2,NRB)*II3 + RBY(5,NRB)*II6 + RBY(8,NRB)*II9
         RBY(23,NRB)=RBY(3,NRB)*II1 + RBY(6,NRB)*II4 + RBY(9,NRB)*II7
         RBY(24,NRB)=RBY(3,NRB)*II2 + RBY(6,NRB)*II5 + RBY(9,NRB)*II8
         RBY(25,NRB)=RBY(3,NRB)*II3 + RBY(6,NRB)*II6 + RBY(9,NRB)*II9
C
       ELSEIF(N2D == 1) THEN
C
         B1 = B1 - IN(M)
         B5 = B5 - IN(M)
         B9 = B9 - IN(M)
         TOTMAS = TOTMAS - MS(M) + MASRB
         XGT = ZERO
         YGT = YGT - MS(M)*X(2,M) + MASRB*X(2,M)
         ZGT = ZGT - MS(M)*X(3,M) + MASRB*X(3,M)
C
C Rigidite au noeud main pour estimation DT.

         IF (NS10E>0) THEN
           DO I=1,NSL
             N = LSN(I)
             IF (ITAGND(N)/=0) CYCLE
             STIFN(M)= STIFN(M)+STIFN(N)
             DD =(X(1,N)-X(1,M))**2+(X(2,N)-X(2,M))**2+(X(3,N)-X(3,M))**2
             STIFR(M)= STIFR(M)+(STIFR(N)+DD*STIFN(N))
             STIFR(N)= EM20
             STIFN(N)= EM20
           END DO
         ELSE
           DO I=1,NSL
             N = LSN(I)
             STIFN(M)= STIFN(M)+STIFN(N)
             DD = (X(1,N)-X(1,M))**2+(X(2,N)-X(2,M))**2+(X(3,N)-X(3,M))**2
             STIFR(M)= STIFR(M)+(STIFR(N)+DD*STIFN(N))
             STIFR(N)= EM20
             STIFN(N)= EM20
            END DO
         END IF !(NS10E>0) THEN

C MATRICE d'inertie -> repere global
         RBY(17,NRB)=RBY(10,NRB)
         RBY(18,NRB)=ZERO
         RBY(19,NRB)=ZERO
         RBY(20,NRB)=ZERO
         RBY(21,NRB)=RBY(11,NRB)
         RBY(22,NRB)=ZERO
         RBY(23,NRB)=ZERO
         RBY(24,NRB)=ZERO
         RBY(25,NRB)=RBY(12,NRB)

       ELSEIF(N2D == 2) THEN
         B1 = B1 - IN(M)
         B5 = B5 - IN(M)
         B9 = B9 - IN(M)
         TOTMAS = TOTMAS - MS(M) + MASRB
         XGT = ZERO
         YGT = YGT - MS(M)*X(2,M) + MASRB*X(2,M)
         ZGT = ZGT - MS(M)*X(3,M) + MASRB*X(3,M)
C
C Rigidite au noeud main pour estimation DT.
         IF (NS10E>0) THEN
           DO I=1,NSL
             N = LSN(I)
             IF (ITAGND(N)/=0) CYCLE
             STIFN(M)= STIFN(M)+STIFN(N)
             DD = (X(2,N)-X(2,M))**2+(X(3,N)-X(3,M))**2
             STIFR(M)= STIFR(M)+(STIFR(N)+DD*STIFN(N))
             STIFR(N)= EM20
             STIFN(N)= EM20
           END DO
         ELSE
           DO I=1,NSL
             N = LSN(I)
             STIFN(M)= STIFN(M)+STIFN(N)
             DD = (X(2,N)-X(2,M))**2+(X(3,N)-X(3,M))**2
             STIFR(M)= STIFR(M)+(STIFR(N)+DD*STIFN(N))
             STIFR(N)= EM20
             STIFN(N)= EM20
           END DO
         END IF !(NS10E>0) THEN

C MATRICE d'inertie -> repere global
         II1=RBY(10,NRB)*RBY(1,NRB)
         II2=ZERO
         II3=ZERO
         II4=RBY(11,NRB)*RBY(4,NRB)
         II5=RBY(11,NRB)*RBY(5,NRB)
         II6=RBY(11,NRB)*RBY(6,NRB)
         II7=ZERO
         II8=RBY(12,NRB)*RBY(8,NRB)
         II9=RBY(12,NRB)*RBY(9,NRB)
C
         RBY(17,NRB)=RBY(1,NRB)*II1 + RBY(4,NRB)*II4
         RBY(18,NRB)=RBY(4,NRB)*II5 + RBY(7,NRB)*II8
         RBY(19,NRB)=RBY(4,NRB)*II6 + RBY(7,NRB)*II9
         RBY(20,NRB)=RBY(2,NRB)*II1 + RBY(5,NRB)*II4
         RBY(21,NRB)=RBY(5,NRB)*II5 + RBY(8,NRB)*II8
         RBY(22,NRB)=RBY(5,NRB)*II6 + RBY(8,NRB)*II9
         RBY(23,NRB)=RBY(3,NRB)*II1 + RBY(6,NRB)*II4
         RBY(24,NRB)=RBY(6,NRB)*II5 + RBY(9,NRB)*II8
         RBY(25,NRB)=RBY(6,NRB)*II6 + RBY(9,NRB)*II9
       ENDIF
C-----------Multidomaines : on ignore l'erreur generee par le calcul des val propres et le chgt de repere--------------
C------------------> on reinjecte les ancienne valeures de la matrice dans le repere global <--------------------------
       IF(NSUBDOM>0)THEN
         IF(TAGNO(M+N_PART)==3)THEN
           DO J=1,9
             RBY(16+J,NRB)=RBY_R2R(J)
           END DO
         ENDIF
       END IF
C
C-------INIVEL/AXIS must be corrected to take into account move of main node
        IF (RBY_INIAXIS(1,NRB) > 0) THEN
          DIST = SQRT((X(1,M)-X_MSN0(1))**2+(X(2,M)-X_MSN0(2))**2+(X(3,M)-X_MSN0(3))**2)
          IF ((DIST  > ZERO).AND.
     .         V(1,M)==RBY_INIAXIS(2,NRB).AND.
     .         V(2,M)==RBY_INIAXIS(3,NRB).AND.
     .         V(3,M)==RBY_INIAXIS(4,NRB)) THEN
C--         inivel is corrected only if not modified after read of inivel
            V1X2=RBY_INIAXIS(5,NRB)*(X(2,M)-X_MSN0(2))
            V2X1=RBY_INIAXIS(6,NRB)*(X(1,M)-X_MSN0(1))
            V2X3=RBY_INIAXIS(6,NRB)*(X(3,M)-X_MSN0(3))
            V3X2=RBY_INIAXIS(7,NRB)*(X(2,M)-X_MSN0(2))
            V3X1=RBY_INIAXIS(7,NRB)*(X(1,M)-X_MSN0(1))
            V1X3=RBY_INIAXIS(5,NRB)*(X(3,M)-X_MSN0(3))
            V(1,M)= V(1,M)+V2X3-V3X2
            V(2,M)= V(2,M)+V3X1-V1X3
            V(3,M)= V(3,M)+V1X2-V2X1
          ENDIF
        ENDIF
C
       IF(N2D == 0) THEN
         DO I=1,NSL
           N=LSN(I)
           V1X2=VR(1,M)*(X(2,N)-X(2,M))
           V2X1=VR(2,M)*(X(1,N)-X(1,M))
           V2X3=VR(2,M)*(X(3,N)-X(3,M))
           V3X2=VR(3,M)*(X(2,N)-X(2,M))
           V3X1=VR(3,M)*(X(1,N)-X(1,M))
           V1X3=VR(1,M)*(X(3,N)-X(3,M))
           V(1,N)= V(1,M)+V2X3-V3X2
           V(2,N)= V(2,M)+V3X1-V1X3
           V(3,N)= V(3,M)+V1X2-V2X1
           VR(1,N)= VR(1,M)
           VR(2,N)= VR(2,M)
           VR(3,N)= VR(3,M)
         ENDDO
       ELSEIF(N2D == 1) THEN
         V(1,M)= ZERO
         VR(1,M)= ZERO
         VR(2,M)= ZERO
         DO I=1,NSL
           N=LSN(I)
           V3X2=VR(3,M)*(X(2,N)-X(2,M))
           V3X1=VR(3,M)*(X(1,N)-X(1,M))
           V(1,N)= V(1,M)-V3X2
           V(2,N)= V(2,M)+V3X1
           V(3,N)= V(3,M)
           VR(1,N)= VR(1,M)
           VR(2,N)= VR(2,M)
           VR(3,N)= VR(3,M)
         ENDDO
       ELSEIF(N2D == 2) THEN
         V(1,M)= ZERO
         VR(2,M)= ZERO
         VR(3,M)= ZERO
         DO I=1,NSL
           N=LSN(I)
           V1X2=VR(1,M)*(X(2,N)-X(2,M))
           V1X3=VR(1,M)*(X(3,N)-X(3,M))
           V(1,N)= ZERO
           V(2,N)= V(2,M)-V1X3
           V(3,N)= V(3,M)+V1X2
           VR(1,N)= VR(1,M)
           VR(2,N)= ZERO
           VR(3,N)= ZERO
         ENDDO
       ENDIF
C
      ENDIF ! (RBLEVEL == 0)
c
      RETURN
C
1000  FORMAT(//
     . '      RIGID BODY INITIALIZATION '/
     . '      ------------------------- ')

1100  FORMAT(/5X,'RIGID BODY ID',I10
     .       /10X,'PRIMARY NODE         ',I10
     .       /10X,'NEW X,Y,Z            ',3G14.7
     .       /10X,'NEW MASS             ',1G14.7
     .       /10X,'NEW INERTIA xx yy zz ',3G14.7
     .       /10X,'NEW INERTIA xy yz zx ',3G14.7)
1101  FORMAT(10X,'SECONDARY NODES ')
1102  FORMAT( 10X,10I10)

1200  FORMAT(10X,'PRINCIPAL INERTIA',1P3G20.13)
1300  FORMAT(/5X,'RIGID BODY ID',I10)
1400  FORMAT(
     & 5X,40HRIGID BODY ON MULTIDOMAINS INTERFACE    ,/,
     & 5X,55H --> MASS AND INERTIA MATRIX ARE COMPUTED IN THE ENGINE,/)
      END


Chd|====================================================================
Chd|  INIRBYS                       source/constraints/general/rbody/inirby.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        INEPRI                        source/materials/mat/mat019/inepri.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE INIRBYS(NRB ,RBY  ,M     ,LPBY ,
     .                   MS  ,IN   ,X     ,ITAB,SKEW,
     .                   B1  ,B2   ,B3    ,B5  ,B6,
     .                   B9  ,ISPH ,TOTMAS,XGT ,YGT ,
     .                   ZGT ,NPBY ,IWA   ,V   ,VR, 
     .                   RBYID,ID  ,TITR  ,ITAGND ,RBY_INIAXIS)
      USE MESSAGE_MOD
C=======================================================================
C    RBY    EN SORTIE DE INIRBY
C     1 -> 9 :  MATRICE ROTATION
C    10 -> 12:  INERTIES PRINCIPALES RBODY
C          13:  INERTIE MAIN INITIALE SPHERIQUE
C          14:  MASSE RBODY
C          15:  MASSE MAIN INITIALE
C    16 -> 20:  LIBRE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRB, M, ISPH
      INTEGER ITAB(*),NPBY(NNPBY,*) ,IWA(*),
     .        RBYID,ITAGND(*)
      INTEGER, TARGET :: LPBY(*)
C     REAL
      my_real
     .   RBY(NRBY,*), MS(*), IN(*), X(3,*), SKEW(LSKEW,*),
     .   V(3,*), VR(3,*),
     .   B1, B2, B3, B5, B6, B9,TOTMAS ,XGT ,YGT ,
     .   ZGT,RBY_INIAXIS(7,*)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NSL, J, I, N, NONOD,II,NI,K, NSLI,
     .        RBLEVEL, OPT_MERGE, M_I, NSL_XTRA
C     REAL
      my_real
     .   XG(3), XM0(3), XX, XY, XZ, YY, YZ, ZZ, XIIN, INMIN,
     .   V1X2, V2X1, V2X3, V3X2, V3X1, V1X3,
     .   MASRB,DD,INMAX, XMG, X_MSN0(3), DIST
      INTEGER, DIMENSION(:), POINTER  :: LSN, LSN_XTRA 
C
      MS(M)=ZERO
      IN(M) = ZERO
      NONOD=ITAB(M)
C
      NSL_XTRA = NPBY(14,NRB)+NPBY(15,NRB)+NPBY(16,NRB)
      NSL = NPBY(2,NRB) - NSL_XTRA
      LSN => LPBY(NPBY(11,NRB)+1:NPBY(11,NRB)+NSL) ! Liste des noeuds SECONDARY
      RBLEVEL = NPBY(12,NRB)
C
      RBY(1,NRB)=ZERO
      RBY(2,NRB)=ZERO
      RBY(3,NRB)=ZERO
      RBY(4,NRB)=ZERO
      RBY(5,NRB)=ZERO
      RBY(8,NRB)=ZERO
      RBY(9,NRB)=ZERO
C---------------------------------
C     RECHERCHE DES NOEUDS SECONDS
C     DE SOUS RBY (NOEUD MAIN PARMI LES SECONDS)
C---------------------------------
      DO I=1,NSL
         N=LSN(I)
         IF(IWA(N)>0)THEN
           K=0
           DO NI=1,IWA(N)-1
             NSLI=NPBY(2,NI)
             K  = K  + NSLI
           ENDDO
           NSLI=NPBY(2,IWA(N))
           DO II=1,NSLI
             NI=LPBY(K+II)
             IF(IWA(NI)==0)IWA(NI)=-1
           ENDDO
         ENDIF
      ENDDO
C---------------------------------
C     CORRECTION DE LA MASSE ET DU
C     CENTRE DE GRAVITE DU MAIN
C---------------------------------
C
C---INITIAL COORDINATES ARE STORED FOR INIVEL/AXIS correction
      IF (RBY_INIAXIS(1,NRB) > 0) THEN
        DO J=1,3
          X_MSN0(J)=X(J,M)
        ENDDO
      ENDIF

C-----CDG DES NOEUDS SECONDS
      MASRB=ZERO
      DO J=1,3
          X(J,M)=ZERO
      ENDDO
      IF (NS10E>0) THEN
       DO I=1,NSL
        N=LSN(I)
        IF(IWA(N)>=0.AND.ITAGND(N)==0)THEN
          DO J=1,3
           X(J,M)    = X(J,M)+X(J,N)*MS(N)
          ENDDO
          MASRB   = MASRB+MS(N)
        ENDIF
       ENDDO
      ELSE
      DO I=1,NSL
       N=LSN(I)
       IF(IWA(N)>=0)THEN
         DO J=1,3
          X(J,M)    = X(J,M)+X(J,N)*MS(N)
         ENDDO
         MASRB   = MASRB+MS(N)
       ENDIF
      ENDDO
      END IF !(NS10E>0) THEN
C
      IF(MASRB<=1.E-30) THEN
         CALL ANCMSG(MSGID=679,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=ID,
     .               C1=TITR,
     .               C2='ON SECONDARY NODES')
         RETURN
      ENDIF
C
      DO J=1,3
          X(J,M)=X(J,M)/MASRB
          XG(J)=X(J,M)
      ENDDO
C
C--------------------------------------
C     ASSEMBLAGE DES XTRA NODES DANS LA MASSE ET COG
C--------------------------------------
      IF(NPBY(15,NRB) > 0) THEN ! MASS/INERTIA ADD, COG ACTUALIZED
        LSN_XTRA => LPBY(NPBY(11,NRB)+NSL+NPBY(14,NRB)+1:
     .                   NPBY(11,NRB)+NSL+NPBY(14,NRB)+NPBY(15,NRB))
        DO J=1,3
          XG(J)=X(J,M)
          X(J,M)=X(J,M)*MASRB
        ENDDO
        XMG=MASRB
C
        DO I=1,NPBY(15,NRB)
          N=LSN_XTRA(I)
          IF (ITAGND(N)/=0) CYCLE
          DO J=1,3
            X(J,M)    = X(J,M)+X(J,N)*MS(N)
          ENDDO
          MASRB   = MASRB+MS(N)
        ENDDO
C    
        DO J=1,3
          X(J,M)=X(J,M)/MASRB
          XG(J)=X(J,M)          
        ENDDO
      ENDIF
C
      IF(NPBY(16,NRB) > 0) THEN ! MASS/INERTIA ADD, COG NOT ACTUALIZED
        LSN_XTRA => LPBY(NPBY(11,NRB)+NSL+NPBY(14,NRB)+NPBY(15,NRB)+1:
     .                   NPBY(11,NRB)+NSL+NPBY(14,NRB)+NPBY(15,NRB)+NPBY(16,NRB))
        DO I=1,NPBY(16,NRB)
          N=LSN_XTRA(I)
          IF (ITAGND(N)/=0) CYCLE
          MASRB   = MASRB+MS(N)
        ENDDO
      ENDIF
C 
C--------------------------------------
C     CORRECTION DE L'INERTIE DU MAIN
C--------------------------------------
      IF(N2D==0)THEN
C        ANALYSE 3D
         DO I=1,NSL
           N=LSN(I)
           NI=IWA(N)
           IF(NI==0)THEN
            XX=(X(1,N)-X(1,M))*(X(1,N)-X(1,M))
            XY=(X(1,N)-X(1,M))*(X(2,N)-X(2,M))
            XZ=(X(1,N)-X(1,M))*(X(3,N)-X(3,M))
            YY=(X(2,N)-X(2,M))*(X(2,N)-X(2,M))
            YZ=(X(2,N)-X(2,M))*(X(3,N)-X(3,M))
            ZZ=(X(3,N)-X(3,M))*(X(3,N)-X(3,M))
            RBY(1,NRB)=RBY(1,NRB)+IN(N)+(YY+ZZ)*MS(N)
            RBY(2,NRB)=RBY(2,NRB)-XY*MS(N)
            RBY(3,NRB)=RBY(3,NRB)-XZ*MS(N)
            RBY(4,NRB)=RBY(4,NRB)-XY*MS(N)
            RBY(5,NRB)=RBY(5,NRB)+IN(N)+(ZZ+XX)*MS(N)
            RBY(6,NRB)=RBY(6,NRB)-YZ*MS(N)
            RBY(7,NRB)=RBY(7,NRB)-XZ*MS(N)
            RBY(8,NRB)=RBY(8,NRB)-YZ*MS(N)
            RBY(9,NRB)=RBY(9,NRB)+IN(N)+(XX+YY)*MS(N)
           ELSEIF(NI>0)THEN
C MAIN DE SOUS RBY
            XX=(X(1,N)-X(1,M))*(X(1,N)-X(1,M))
            XY=(X(1,N)-X(1,M))*(X(2,N)-X(2,M))
            XZ=(X(1,N)-X(1,M))*(X(3,N)-X(3,M))
            YY=(X(2,N)-X(2,M))*(X(2,N)-X(2,M))
            YZ=(X(2,N)-X(2,M))*(X(3,N)-X(3,M))
            ZZ=(X(3,N)-X(3,M))*(X(3,N)-X(3,M))
            RBY(1,NRB)=RBY(1,NRB)+(YY+ZZ)*MS(N)+RBY(17,NI)
            RBY(2,NRB)=RBY(2,NRB)-XY*MS(N)+RBY(18,NI)
            RBY(3,NRB)=RBY(3,NRB)-XZ*MS(N)+RBY(19,NI)
            RBY(4,NRB)=RBY(4,NRB)-XY*MS(N)+RBY(20,NI)
            RBY(5,NRB)=RBY(5,NRB)+(ZZ+XX)*MS(N)+RBY(21,NI)
            RBY(6,NRB)=RBY(6,NRB)-YZ*MS(N)+RBY(22,NI)
            RBY(7,NRB)=RBY(7,NRB)-XZ*MS(N)+RBY(23,NI)
            RBY(8,NRB)=RBY(8,NRB)-YZ*MS(N)+RBY(24,NI)
            RBY(9,NRB)=RBY(9,NRB)+(XX+YY)*MS(N)+RBY(25,NI)
           ENDIF
         ENDDO
C
      ELSEIF(N2D == 1) THEN
C        ANALYSE 2D : Axisymetrie
         DO I=1,NSL
           N=LSN(I)
           NI=IWA(N)
           IF(NI==0)THEN
            YY=(X(2,N)-X(2,M))*(X(2,N)-X(2,M))
            YZ=(X(2,N)-X(2,M))*(X(3,N)-X(3,M))
            ZZ=(X(3,N)-X(3,M))*(X(3,N)-X(3,M))
            RBY(1,NRB)=RBY(1,NRB)+IN(N)+(YY+ZZ)*MS(N)
            RBY(2,NRB)=ZERO
            RBY(3,NRB)=ZERO
            RBY(4,NRB)=ZERO
            RBY(5,NRB)=RBY(5,NRB)+IN(N)+(ZZ+XX)*MS(N)
            RBY(6,NRB)=ZERO
            RBY(7,NRB)=ZERO
            RBY(8,NRB)=ZERO
            RBY(9,NRB)=RBY(9,NRB)+IN(N)+(XX+YY)*MS(N)
           ELSEIF(NI>0)THEN
C MAIN DE SOUS RBY
            YY=(X(2,N)-X(2,M))*(X(2,N)-X(2,M))
            YZ=(X(2,N)-X(2,M))*(X(3,N)-X(3,M))
            ZZ=(X(3,N)-X(3,M))*(X(3,N)-X(3,M))
            RBY(1,NRB)=RBY(1,NRB)+(YY+ZZ)*MS(N)+RBY(17,NI)
            RBY(2,NRB)=ZERO
            RBY(3,NRB)=ZERO
            RBY(4,NRB)=ZERO
            RBY(5,NRB)=RBY(5,NRB)+(ZZ+XX)*MS(N)+RBY(21,NI)
            RBY(6,NRB)=ZERO
            RBY(7,NRB)=ZERO
            RBY(8,NRB)=ZERO
            RBY(9,NRB)=RBY(9,NRB)+(XX+YY)*MS(N)+RBY(25,NI)
           ENDIF
         ENDDO
C
      ELSEIF(N2D == 2) THEN
C        ANALYSE 2D : Strain Plane
         DO I=1,NSL
           N=LSN(I)
           NI=IWA(N)
           IF(NI==0)THEN
            YY=(X(2,N)-X(2,M))*(X(2,N)-X(2,M))
            YZ=(X(2,N)-X(2,M))*(X(3,N)-X(3,M))
            ZZ=(X(3,N)-X(3,M))*(X(3,N)-X(3,M))
            RBY(1,NRB)=RBY(1,NRB)+IN(N)+(YY+ZZ)*MS(N)
            RBY(2,NRB)=ZERO
            RBY(3,NRB)=ZERO
            RBY(4,NRB)=ZERO
            RBY(5,NRB)=RBY(5,NRB)+IN(N)+ZZ*MS(N)
            RBY(6,NRB)=RBY(6,NRB)-YZ*MS(N)
            RBY(7,NRB)=ZERO
            RBY(8,NRB)=RBY(8,NRB)-YZ*MS(N)
            RBY(9,NRB)=RBY(9,NRB)+IN(N)+YY*MS(N)
           ELSEIF(NI>0)THEN
C MAIN DE SOUS RBY
            YY=(X(2,N)-X(2,M))*(X(2,N)-X(2,M))
            YZ=(X(2,N)-X(2,M))*(X(3,N)-X(3,M))
            ZZ=(X(3,N)-X(3,M))*(X(3,N)-X(3,M))
            RBY(1,NRB)=RBY(1,NRB)+(YY+ZZ)*MS(N)+RBY(17,NI)
            RBY(2,NRB)=ZERO
            RBY(3,NRB)=ZERO
            RBY(4,NRB)=ZERO
            RBY(5,NRB)=RBY(5,NRB)+ZZ*MS(N)+RBY(21,NI)
            RBY(6,NRB)=RBY(6,NRB)-YZ*MS(N)+RBY(22,NI)
            RBY(7,NRB)=ZERO
            RBY(8,NRB)=RBY(8,NRB)-YZ*MS(N)+RBY(24,NI)
            RBY(9,NRB)=RBY(9,NRB)+YY*MS(N)+RBY(25,NI)
           ENDIF
         ENDDO
C
      ENDIF
C
C--------------------------------------
C     AJOUT DE L'INERTIE DES XTRA NODES
C--------------------------------------
      IF((NPBY(15,NRB)+NPBY(16,NRB)) > 0) THEN

        LSN_XTRA => LPBY(NPBY(11,NRB)+NSL+NPBY(14,NRB)+1:
     .                   NPBY(11,NRB)+NSL+NPBY(14,NRB)+NPBY(15,NRB)+NPBY(16,NRB))

        IF(N2D==0)THEN
         DO I=1,NPBY(15,NRB)+NPBY(16,NRB)
          N=LSN_XTRA(I)
          IF (ITAGND(N)/=0) CYCLE
          XX=(X(1,N)-X(1,M))*(X(1,N)-X(1,M))
          XY=(X(1,N)-X(1,M))*(X(2,N)-X(2,M))
          XZ=(X(1,N)-X(1,M))*(X(3,N)-X(3,M))
          YY=(X(2,N)-X(2,M))*(X(2,N)-X(2,M))
          YZ=(X(2,N)-X(2,M))*(X(3,N)-X(3,M))
          ZZ=(X(3,N)-X(3,M))*(X(3,N)-X(3,M))
          RBY(1,NRB)=RBY(1,NRB)+IN(N)+(YY+ZZ)*MS(N)
          RBY(2,NRB)=RBY(2,NRB)-XY*MS(N)
          RBY(3,NRB)=RBY(3,NRB)-XZ*MS(N)
          RBY(4,NRB)=RBY(4,NRB)-XY*MS(N)
          RBY(5,NRB)=RBY(5,NRB)+IN(N)+(ZZ+XX)*MS(N)
          RBY(6,NRB)=RBY(6,NRB)-YZ*MS(N)
          RBY(7,NRB)=RBY(7,NRB)-XZ*MS(N)
          RBY(8,NRB)=RBY(8,NRB)-YZ*MS(N)
          RBY(9,NRB)=RBY(9,NRB)+IN(N)+(XX+YY)*MS(N)
         ENDDO
        ELSEIF(N2D==1) THEN
         DO I=1,NPBY(15,NRB)+NPBY(16,NRB)
          N=LSN_XTRA(I)  
          IF (ITAGND(N)/=0) CYCLE       
          YY=(X(2,N)-X(2,M))*(X(2,N)-X(2,M))
          ZZ=(X(3,N)-X(3,M))*(X(3,N)-X(3,M))
          RBY(1,NRB)=RBY(1,NRB)+IN(N)+(YY+ZZ)*MS(N)
          RBY(5,NRB)=RBY(5,NRB)+IN(N)+ZZ*MS(N)
          RBY(9,NRB)=RBY(9,NRB)+IN(N)+YY*MS(N)
         ENDDO
        ELSEIF(N2D==1) THEN
         DO I=1,NPBY(15,NRB)+NPBY(16,NRB)
          N=LSN_XTRA(I) 
          IF (ITAGND(N)/=0) CYCLE
          YY=(X(2,N)-X(2,M))*(X(2,N)-X(2,M))
          YZ=(X(2,N)-X(2,M))*(X(3,N)-X(3,M))
          ZZ=(X(3,N)-X(3,M))*(X(3,N)-X(3,M))
          RBY(1,NRB)=RBY(1,NRB)+IN(N)+(YY+ZZ)*MS(N)
          RBY(5,NRB)=RBY(5,NRB)+IN(N)+ZZ*MS(N)
          RBY(6,NRB)=RBY(6,NRB)-YZ*MS(N)
          RBY(8,NRB)=RBY(8,NRB)-YZ*MS(N)
          RBY(9,NRB)=RBY(9,NRB)+IN(N)+YY*MS(N)
         ENDDO
        ENDIF !N2D
      ENDIF
C
C--------------------------------------
C     ASSEMBLAGE DES MASSES DES RIGID BODY SECONDARYS ET MAJ POSITION DU COG
C--------------------------------------
      DO I=NRB-1,1,-1
        IF((NPBY(12,I)) == RBLEVEL-1) THEN
          DO J=1,3
            XG(J)=X(J,M)  ! centre de gravite du rb main avant modif
          ENDDO
          XMG=MASRB  ! masse du RB main avant modif
c
          OPT_MERGE = NPBY(13,I)
          NPBY(11,NRB) = NPBY(11,I) 
          NPBY(2,NRB) = NPBY(2,NRB)+NPBY(2,I) 
          M_I=NPBY(1,I)
c
          IF(OPT_MERGE == 2) THEN ! MASSE AJOUTEE, COG ACTUALISE
            DO J=1,3
             X(J,M)=X(J,M)*MASRB+X(J,M_I)*RBY(14,I)
            ENDDO
            MASRB = MASRB + RBY(14,I)
            DO J=1,3
              X(J,M)=X(J,M)/MASRB
            ENDDO    
          ELSEIF(OPT_MERGE == 3) THEN ! MASSE AJOUTEE, COG NON ACTUALISE
            MASRB = MASRB + RBY(14,I)
          ENDIF         
c
C--------------------------------------
C     TRANSFERT DES INERTIES AU COG
C--------------------------------------
          IF(OPT_MERGE == 2) THEN ! COG ACTUALISE, TRANSFERT INERTIE MAIN
           IF(N2D==0)THEN
            XX=(XG(1)-X(1,M))*(XG(1)-X(1,M))
            XY=(XG(1)-X(1,M))*(XG(2)-X(2,M))
            XZ=(XG(1)-X(1,M))*(XG(3)-X(3,M))
            YY=(XG(2)-X(2,M))*(XG(2)-X(2,M))
            YZ=(XG(2)-X(2,M))*(XG(3)-X(3,M))
            ZZ=(XG(3)-X(3,M))*(XG(3)-X(3,M))
            RBY(1,NRB)=RBY(1,NRB)+(YY+ZZ)*XMG
            RBY(2,NRB)=RBY(2,NRB)-XY*XMG
            RBY(3,NRB)=RBY(3,NRB)-XZ*XMG
            RBY(4,NRB)=RBY(4,NRB)-XY*XMG
            RBY(5,NRB)=RBY(5,NRB)+(ZZ+XX)*XMG
            RBY(6,NRB)=RBY(6,NRB)-YZ*XMG
            RBY(7,NRB)=RBY(7,NRB)-XZ*XMG
            RBY(8,NRB)=RBY(8,NRB)-YZ*XMG
            RBY(9,NRB)=RBY(9,NRB)+(XX+YY)*XMG
           ELSEIF(N2D==1) THEN
            YY=(XG(2)-X(2,M))*(XG(2)-X(2,M))
            ZZ=(XG(3)-X(3,M))*(XG(3)-X(3,M))
            RBY(1,NRB)=RBY(1,NRB)+(YY+ZZ)*XMG
            RBY(2,NRB)=ZERO
            RBY(3,NRB)=ZERO
            RBY(4,NRB)=ZERO
            RBY(5,NRB)=RBY(5,NRB)+ZZ*XMG
            RBY(6,NRB)=ZERO
            RBY(7,NRB)=ZERO
            RBY(8,NRB)=ZERO
            RBY(9,NRB)=RBY(9,NRB)+YY*XMG
           ELSEIF(N2D==2) THEN  
            YY=(XG(2)-X(2,M))*(XG(2)-X(2,M))
            YZ=(XG(2)-X(2,M))*(XG(3)-X(3,M))
            ZZ=(XG(3)-X(3,M))*(XG(3)-X(3,M))
            RBY(1,NRB)=RBY(1,NRB)+(YY+ZZ)*XMG
            RBY(2,NRB)=ZERO
            RBY(3,NRB)=ZERO
            RBY(4,NRB)=ZERO
            RBY(5,NRB)=RBY(5,NRB)+ZZ*XMG
            RBY(6,NRB)=RBY(6,NRB)-YZ*XMG
            RBY(7,NRB)=ZERO
            RBY(8,NRB)=RBY(8,NRB)-YZ*XMG
            RBY(9,NRB)=RBY(9,NRB)+YY*XMG
           ENDIF
          ENDIF
          IF(OPT_MERGE > 1) THEN ! TRANSFERT INERTIE SECONDARY 
           IF(N2D==0)THEN
            XX=(X(1,M_I)-X(1,M))*(X(1,M_I)-X(1,M))  
            XY=(X(1,M_I)-X(1,M))*(X(2,M_I)-X(2,M))
            XZ=(X(1,M_I)-X(1,M))*(X(3,M_I)-X(3,M))
            YY=(X(2,M_I)-X(2,M))*(X(2,M_I)-X(2,M))
            YZ=(X(2,M_I)-X(2,M))*(X(3,M_I)-X(3,M))
            ZZ=(X(3,M_I)-X(3,M))*(X(3,M_I)-X(3,M))
            RBY(1,NRB)=RBY(1,NRB)+RBY(1,I)+(YY+ZZ)*RBY(14,I)
            RBY(2,NRB)=RBY(2,NRB)+RBY(2,I)-XY*RBY(14,I)
            RBY(3,NRB)=RBY(3,NRB)+RBY(3,I)-XZ*RBY(14,I)
            RBY(4,NRB)=RBY(4,NRB)+RBY(4,I)-XY*RBY(14,I)
            RBY(5,NRB)=RBY(5,NRB)+RBY(5,I)+(ZZ+XX)*RBY(14,I)
            RBY(6,NRB)=RBY(6,NRB)+RBY(6,I)-YZ*RBY(14,I)
            RBY(7,NRB)=RBY(7,NRB)+RBY(7,I)-XZ*RBY(14,I)
            RBY(8,NRB)=RBY(8,NRB)+RBY(8,I)-YZ*RBY(14,I)
            RBY(9,NRB)=RBY(9,NRB)+RBY(9,I)+(XX+YY)*RBY(14,I) 
           ELSEIF(N2D==1) THEN
            YY=(X(2,M_I)-X(2,M))*(X(2,M_I)-X(2,M))
            ZZ=(X(3,M_I)-X(3,M))*(X(3,M_I)-X(3,M))
            RBY(1,NRB)=RBY(1,NRB)+RBY(1,I)+(YY+ZZ)*RBY(14,I)
            RBY(5,NRB)=RBY(5,NRB)+RBY(5,I)+ZZ*RBY(14,I)
            RBY(9,NRB)=RBY(9,NRB)+RBY(9,I)+YY*RBY(14,I)
           ELSEIF(N2D==2) THEN
            YY=(X(2,M_I)-X(2,M))*(X(2,M_I)-X(2,M))
            YZ=(X(2,M_I)-X(2,M))*(X(3,M_I)-X(3,M))
            ZZ=(X(3,M_I)-X(3,M))*(X(3,M_I)-X(3,M))
            RBY(1,NRB)=RBY(1,NRB)+RBY(1,I)+(YY+ZZ)*RBY(14,I)
            RBY(5,NRB)=RBY(5,NRB)+RBY(5,I)+ZZ*RBY(14,I)
            RBY(6,NRB)=RBY(6,NRB)-YZ*RBY(14,I)
            RBY(8,NRB)=RBY(8,NRB)-YZ*RBY(14,I)
            RBY(9,NRB)=RBY(9,NRB)+RBY(9,I)+YY*RBY(14,I)
           ENDIF !N2D
          ENDIF   
c         mise a zero des donnees rb SECONDARY
          RBY(1:15,I)=ZERO           
          IN(M_I)=ZERO 
          MS(M_I)=ZERO    
        ELSEIF(NPBY(12,I) == RBLEVEL) THEN
          EXIT
        ENDIF
      ENDDO
c
C----------------------------------------------
C    NSL est complete par les noeuds SECONDARY des rigid body SECONDARYs
C    LSN est actualisee
C----------------------------------------------
      IF(RBLEVEL == 0) THEN ! IF THIS RIGID BODY IS THE TOP MAIN
        NSL = NPBY(2,NRB) ! Liste totale des noeuds SECONDARY
        LSN => LPBY(NPBY(11,NRB)+1:NPBY(11,NRB)+NSL)
c
        WRITE(IOUT,1000)

        IF(ISPH==1)THEN
        ELSE

          WRITE(IOUT,1100) RBYID,NONOD,X(1,M),X(2,M),X(3,M),
     .         MASRB,RBY(1,NRB),RBY(5,NRB),RBY(9,NRB),
     .         RBY(2,NRB),RBY(6,NRB),RBY(3,NRB)
        ENDIF
C----------------------------------------------------------------
C     CALCUL DU REPERE D'INERTIE PRINCIPALE
C
        IF(N2D == 1) THEN
          RBY(11,NRB) = RBY(5,NRB)
          RBY(12,NRB) = RBY(9,NRB)
          RBY(1,NRB) = ONE
          RBY(5,NRB) = ONE
          RBY(9,NRB) = ONE
        ELSE
          CALL INEPRI(RBY(10,NRB),RBY(1,NRB))
        ENDIF

        IF(ISPH==1)THEN
          XIIN = (RBY(10,NRB) + RBY(11,NRB) + RBY(12,NRB)) * THIRD
          RBY(10,NRB) = XIIN
          RBY(11,NRB) = XIIN
          RBY(12,NRB) = XIIN
          INMIN   = XIIN

          WRITE(IOUT,1100) RBYID,NONOD,X(1,M),X(2,M),X(3,M),
     .         MASRB,XIIN,XIIN,XIIN,ZERO,ZERO,ZERO
        ELSEIF(ISPH==2) THEN
          INMIN = MIN(RBY(10,NRB),RBY(11,NRB),RBY(12,NRB))
          INMAX = MAX(RBY(10,NRB),RBY(11,NRB),RBY(12,NRB))
          IF(INMIN<=1.E-3*INMAX)THEN
            IF(RBY(10,NRB)/INMAX<EM03) RBY(10,NRB)=RBY(10,NRB)+EM01*INMAX
            IF(RBY(11,NRB)/INMAX<EM03) RBY(11,NRB)=RBY(11,NRB)+EM01*INMAX
            IF(RBY(12,NRB)/INMAX<EM03) RBY(12,NRB)=RBY(12,NRB)+EM01*INMAX
            CALL ANCMSG(MSGID=275,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID,
     .                C1=TITR)
          ENDIF
        ENDIF
C
        INMIN = MIN(RBY(10,NRB),RBY(11,NRB),RBY(12,NRB))      
        WRITE(IOUT,1200) RBY(10,NRB),RBY(11,NRB),RBY(12,NRB)
        WRITE(IOUT,1101)
        WRITE(IOUT,1102) (ITAB(LPBY(I+NPBY(11,NRB))),I=1,NSL)

C
        IF(INMIN<=0.0)THEN
          CALL ANCMSG(MSGID=274,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID,
     .                C1=TITR)
        ENDIF
C
        RBY(13,NRB)=ZERO
        RBY(14,NRB)=MASRB
C
        RBY(15,NRB)=ZERO
        MS(M) = ZERO
        IN(M) = ZERO
C
C-------INIVEL/AXIS must be corrected to take into account move of main node
        IF (RBY_INIAXIS(1,NRB) > 0) THEN
          DIST = SQRT((X(1,M)-X_MSN0(1))**2+(X(2,M)-X_MSN0(2))**2+(X(3,M)-X_MSN0(3))**2)
          IF ((DIST  > ZERO).AND.
     .         V(1,M)==RBY_INIAXIS(2,NRB).AND.
     .         V(2,M)==RBY_INIAXIS(3,NRB).AND.
     .         V(3,M)==RBY_INIAXIS(4,NRB)) THEN
C--         inivel is corrected only if not modified after read of inivel
            V1X2=RBY_INIAXIS(5,NRB)*(X(2,M)-X_MSN0(2))
            V2X1=RBY_INIAXIS(6,NRB)*(X(1,M)-X_MSN0(1))
            V2X3=RBY_INIAXIS(6,NRB)*(X(3,M)-X_MSN0(3))
            V3X2=RBY_INIAXIS(7,NRB)*(X(2,M)-X_MSN0(2))
            V3X1=RBY_INIAXIS(7,NRB)*(X(1,M)-X_MSN0(1))
            V1X3=RBY_INIAXIS(5,NRB)*(X(3,M)-X_MSN0(3))
            V(1,M)= V(1,M)+V2X3-V3X2
            V(2,M)= V(2,M)+V3X1-V1X3
            V(3,M)= V(3,M)+V1X2-V2X1
          ENDIF
        ENDIF      
C
        IF(N2D == 0) THEN
          DO I=1,NSL
           N=LSN(I)
           IWA(N)=MAX(IWA(N),0)
           V1X2=VR(1,M)*(X(2,N)-X(2,M))
           V2X1=VR(2,M)*(X(1,N)-X(1,M))
           V2X3=VR(2,M)*(X(3,N)-X(3,M))
           V3X2=VR(3,M)*(X(2,N)-X(2,M))
           V3X1=VR(3,M)*(X(1,N)-X(1,M))
           V1X3=VR(1,M)*(X(3,N)-X(3,M))
           V(1,N)= V(1,M)+V2X3-V3X2
           V(2,N)= V(2,M)+V3X1-V1X3
           V(3,N)= V(3,M)+V1X2-V2X1
           VR(1,N)= VR(1,M)
           VR(2,N)= VR(2,M)
           VR(3,N)= VR(3,M)
          ENDDO
        ELSEIF(N2D == 1) THEN
          DO I=1,NSL
           N=LSN(I)
           IWA(N)=MAX(IWA(N),0)
           V(1,M)= ZERO
c           V(2,M)= ZERO
           VR(1,M)= ZERO
           VR(2,M)= ZERO
           V3X2=VR(3,M)*(X(2,N)-X(2,M))
           V(1,N)= V(1,M)-V3X2
           V(2,N)= V(2,M)
           V(3,N)= V(3,M)
           VR(1,N)= VR(1,M)
           VR(2,N)= VR(2,M)
           VR(3,N)= VR(3,M)
          ENDDO
        ELSEIF(N2D == 2) THEN
          DO I=1,NSL
           N=LSN(I)
           IWA(N)=MAX(IWA(N),0)
           V(1,M)= ZERO
           VR(2,M)= ZERO
           VR(3,M)= ZERO
           V1X2=VR(1,M)*(X(2,N)-X(2,M))
           V1X3=VR(1,M)*(X(3,N)-X(3,M))
           V(1,N)= V(1,M)
           V(2,N)= V(2,M)-V1X3
           V(3,N)= V(3,M)+V1X2
           VR(1,N)= VR(1,M)
           VR(2,N)= VR(2,M)
           VR(3,N)= VR(3,M)
          ENDDO
        ENDIF
C
      ENDIF ! (RBLEVEL == 0)
C
      RETURN
C
1000  FORMAT(/
     . '      RIGID BODY INITIALIZATION (SENSOR ACTIVATED) '/
     . '      ------------------------- ')

1100  FORMAT(/5X,'RIGID BODY ID',I10
     .       /10X,'PRIMARY NODE    ',I10
     .       /10X,'NEW X,Y,Z       ',1P3G14.7
     .       /10X,'NEW MASS        ',1G14.7
     .       /10X,'NEW INERTIA xx yy zz ',3G14.7
     .       /10X,'NEW INERTIA xy yz zx ',3G14.7)
1200  FORMAT(10X,'PRINCIPAL INERTIA',1P3G20.13)
1101  FORMAT(10X,'SECONDARY NODES ')
1102  FORMAT( 10X,10I10)
      END
