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
Chd|  PROJECIG3D                    source/elements/ige3d/projecig3d.F
Chd|-- called by -----------
Chd|        ANIMIG3D                      source/output/anim/generate/animig3d.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        IG3DONEBASIS                  source/elements/ige3d/ig3donebasis.F
Chd|        IG3DONEDERIV                  source/elements/ige3d/ig3donederiv.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE PROJECIG3D(ELBUF_TAB, IPARG  ,X , D, V, A, WIGE ,KXIG3D   ,IXIG3D   ,
     .               IG3DSOLID,NANIM3D_L, X_TEMP, D_TEMP, V_TEMP, A_TEMP, TABSTRESL, 
     .               IGEO,KNOT, NG, NBG, NISOELCUT, NCTRL, NEL_P, ITAB, CONT, CONT_TEMP,
     .               FINT, FINT_TEMP, FEXT, FEXT_TEMP, FREAC, FREACT_TEMP, 
     .               PX, PY, PZ,KNOTLOCPC,KNOTLOCEL)  
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD  
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
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
#include      "param_c.inc"
#include      "tabsiz_c.inc"
#include      "scr14_c.inc"
#include      "ige3d_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXIG3D(NIXIG3D,*),IXIG3D(*),IGEO(NPROPGI,*),
     .        NANIM3D_L, IPARG(NPARG,*), NISOELCUT, NG, NBG, NCTRL, NEL_P, ITAB(*),  ! DISCUTER DE COMMENT RENVOYER NANIM3D_L
     .        PX,PY,PZ
      INTEGER IG3DSOLID(8,27,*)
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      my_real
     .   X(3,*),D(3,*),V(3,*),A(3,*),WIGE(*),KNOT(*),
     .   X_TEMP(3,*),D_TEMP(3,*),V_TEMP(3,*),A_TEMP(3,*),
     .   CONT(3,*),CONT_TEMP(3,*),
     .   FINT(3,*),FINT_TEMP(3,*),FEXT(3,*),FEXT_TEMP(3,*),
     .   FREAC(6,*),FREACT_TEMP(3,*),KNOTLOCPC(DEG_MAX,3,*),KNOTLOCEL(2,3,*)
      my_real,
     .  DIMENSION(6,*) :: TABSTRESL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IDX(MVSIZ),IDY(MVSIZ),IDZ(MVSIZ),
     .        IDX2(MVSIZ),IDY2(MVSIZ),IDZ2(MVSIZ),
     .        N1, N2, N3,NKNOT1,NKNOT2,NKNOT3, I, J,K, JJ, L, M, N,
     .        O, P, Q, BID, COUNT, PROJEC,
     .        ITENS, IDBRICK, INCTRL,IERROR,
     .        IPROP, NELIG3D,IAD_KNOT,
     .        IR, IS, IT, ING, DECAL, COUNT2, COUNT3, NEL,
     .        IDFRSTLOCKNT,IDPC
      my_real
     .   XX(NCTRL,MVSIZ),YY(NCTRL,MVSIZ),ZZ(NCTRL,MVSIZ),
     .   DX(NCTRL,MVSIZ),DY(NCTRL,MVSIZ),DZ(NCTRL,MVSIZ), 
     .   UX(NCTRL,MVSIZ),UY(NCTRL,MVSIZ),UZ(NCTRL,MVSIZ),
     .   VX(NCTRL,MVSIZ),VY(NCTRL,MVSIZ),VZ(NCTRL,MVSIZ),
     .   AX(NCTRL,MVSIZ),AY(NCTRL,MVSIZ),AZ(NCTRL,MVSIZ),
     .   CONTX(NCTRL,MVSIZ),CONTY(NCTRL,MVSIZ),CONTZ(NCTRL,MVSIZ),
     .   FINTX(NCTRL,MVSIZ),FINTY(NCTRL,MVSIZ),FINTZ(NCTRL,MVSIZ),
     .   FEXTX(NCTRL,MVSIZ),FEXTY(NCTRL,MVSIZ),FEXTZ(NCTRL,MVSIZ),
     .   FREACTX(NCTRL,MVSIZ),FREACTY(NCTRL,MVSIZ),FREACTZ(NCTRL,MVSIZ),
     .   WW(NCTRL,MVSIZ),PASX, PASY, PASZ, R(NCTRL),
     .   XI(3), XXI(3), DI(3), VI(3), AI(3),CONTI(3),
     .   FINTI(3),FEXTI(3),FREACTI(3),
     .   S(6), ZR, ZS, ZT, DETJAC, PGAUSS,KNOTLOCX(PX+1,NCTRL,MVSIZ),
     .   KNOTLOCY(PY+1,NCTRL,MVSIZ),KNOTLOCZ(PZ+1,NCTRL,MVSIZ),
     .   KNOTLOCELX(2,MVSIZ),
     .   KNOTLOCELY(2,MVSIZ),KNOTLOCELZ(2,MVSIZ)
      TYPE(L_BUFEL_) ,POINTER :: LBUF 
      my_real,
     .  ALLOCATABLE, DIMENSION(:) :: MAB 
      my_real,
     .  ALLOCATABLE, DIMENSION(:,:) :: RI,FI
      my_real
     .   RBID,TBID(NCTRL,3)
C----------------------------------------------------------
      DOUBLE PRECISION
     .  W_GAUSS(9,9),A_GAUSS(9,9)
      DATA W_GAUSS / 
     1 2.D0               ,0.D0               ,0.D0               ,
     1 0.D0               ,0.D0               ,0.D0               ,
     1 0.D0               ,0.D0               ,0.D0               ,
     2 1.D0               ,1.D0               ,0.D0               ,
     2 0.D0               ,0.D0               ,0.D0               ,
     2 0.D0               ,0.D0               ,0.D0               ,
     3 0.555555555555556D0,0.888888888888889D0,0.555555555555556D0,
     3 0.D0               ,0.D0               ,0.D0               ,
     3 0.D0               ,0.D0               ,0.D0               ,
     4 0.347854845137454D0,0.652145154862546D0,0.652145154862546D0,
     4 0.347854845137454D0,0.D0               ,0.D0               ,
     4 0.D0               ,0.D0               ,0.D0               ,
     5 0.236926885056189D0,0.478628670499366D0,0.568888888888889D0,
     5 0.478628670499366D0,0.236926885056189D0,0.D0               ,
     5 0.D0               ,0.D0               ,0.D0               ,
     6 0.171324492379170D0,0.360761573048139D0,0.467913934572691D0,
     6 0.467913934572691D0,0.360761573048139D0,0.171324492379170D0,
     6 0.D0               ,0.D0               ,0.D0               ,
     7 0.129484966168870D0,0.279705391489277D0,0.381830050505119D0,
     7 0.417959183673469D0,0.381830050505119D0,0.279705391489277D0,
     7 0.129484966168870D0,0.D0               ,0.D0               ,
     8 0.101228536290376D0,0.222381034453374D0,0.313706645877887D0,
     8 0.362683783378362D0,0.362683783378362D0,0.313706645877887D0,
     8 0.222381034453374D0,0.101228536290376D0,0.D0               ,
     9 0.081274388361574D0,0.180648160694857D0,0.260610696402935D0,
     9 0.312347077040003D0,0.330239355001260D0,0.312347077040003D0,
     9 0.260610696402935D0,0.180648160694857D0,0.081274388361574D0/
      DATA A_GAUSS / 
     1 0.D0               ,0.D0               ,0.D0               ,
     1 0.D0               ,0.D0               ,0.D0               ,
     1 0.D0               ,0.D0               ,0.D0               ,
     2 -.577350269189625D0,0.577350269189625D0,0.D0               ,
     2 0.D0               ,0.D0               ,0.D0               ,
     2 0.D0               ,0.D0               ,0.D0               , 
     3 -.774596669241483D0,0.D0               ,0.774596669241483D0,
     3 0.D0               ,0.D0               ,0.D0               ,
     3 0.D0               ,0.D0               ,0.D0               ,
     4 -.861136311594053D0,-.339981043584856D0,0.339981043584856D0,
     4 0.861136311594053D0,0.D0               ,0.D0               ,
     4 0.D0               ,0.D0               ,0.D0               ,
     5 -.906179845938664D0,-.538469310105683D0,0.D0               ,
     5 0.538469310105683D0,0.906179845938664D0,0.D0               ,
     5 0.D0               ,0.D0               ,0.D0               ,
     6 -.932469514203152D0,-.661209386466265D0,-.238619186083197D0,
     6 0.238619186083197D0,0.661209386466265D0,0.932469514203152D0,
     6 0.D0               ,0.D0               ,0.D0               ,
     7 -.949107912342759D0,-.741531185599394D0,-.405845151377397D0,
     7 0.D0               ,0.405845151377397D0,0.741531185599394D0,
     7 0.949107912342759D0,0.D0               ,0.D0               ,
     8 -.960289856497536D0,-.796666477413627D0,-.525532409916329D0,
     8 -.183434642495650D0,0.183434642495650D0,0.525532409916329D0,
     8 0.796666477413627D0,0.960289856497536D0,0.D0               ,
     9 -.968160239507626D0,-.836031107326636D0,-.613371432700590D0,
     9 -.324253423403809D0,0.D0               ,0.324253423403809D0,
     9 0.613371432700590D0,0.836031107326636D0,0.968160239507626D0/
C
C-----------------------------------------------

      IERROR = 0
      IPROP=IPARG(62,NG)
      IAD_KNOT = IGEO(40,IPROP)
      N1 = IGEO(44,IPROP)
      N2 = IGEO(45,IPROP)
      N3 = IGEO(46,IPROP)
      IDFRSTLOCKNT = IGEO(47,IPROP)
      NKNOT1 = N1+PX
      NKNOT2 = N2+PY
      NKNOT3 = N3+PZ
      KNOTLOCX = ZERO
      KNOTLOCY = ZERO
      KNOTLOCZ = ZERO
      KNOTLOCELX = ZERO
      KNOTLOCELY = ZERO
      KNOTLOCELZ = ZERO

      ALLOCATE(MAB(SIXIG3D),RI(6,SIXIG3D),
     .         FI(6,SIXIG3D), STAT=IERROR)
      
      IF(IERROR/=0)THEN
       CALL ANCMSG(MSGID=249,ANMODE=ANINFO)
       CALL ARRET(2)
      END IF

      MAB(:) = ZERO
      RI(:,:) = ZERO
      DECAL = NISOELCUT

      COUNT3 = 0 

      DO ING=NG,NG+NBG-1

       NFT=IPARG(3,ING)
       NEL=IPARG(2,ING)
       LLT=MIN(NVSIZ,NEL)

       DO I=LFT,LLT
         DO J=1,NCTRL
           IF( J <= KXIG3D(3,I+NFT) ) THEN
            XX(J,I)=X(1,IXIG3D(KXIG3D(4,I+NFT)+J-1))
            YY(J,I)=X(2,IXIG3D(KXIG3D(4,I+NFT)+J-1))
            ZZ(J,I)=X(3,IXIG3D(KXIG3D(4,I+NFT)+J-1))
            IF(ANIM_V(1)==1) THEN
            VX(J,I)=V(1,IXIG3D(KXIG3D(4,I+NFT)+J-1))
            VY(J,I)=V(2,IXIG3D(KXIG3D(4,I+NFT)+J-1))
            VZ(J,I)=V(3,IXIG3D(KXIG3D(4,I+NFT)+J-1))
            ENDIF
            IF(ANIM_V(2)==1) THEN
            DX(J,I)=D(1,IXIG3D(KXIG3D(4,I+NFT)+J-1))
            DY(J,I)=D(2,IXIG3D(KXIG3D(4,I+NFT)+J-1))
            DZ(J,I)=D(3,IXIG3D(KXIG3D(4,I+NFT)+J-1))
            ENDIF
            IF(ANIM_V(3)==1) THEN
            AX(J,I)=A(1,IXIG3D(KXIG3D(4,I+NFT)+J-1))
            AY(J,I)=A(2,IXIG3D(KXIG3D(4,I+NFT)+J-1))
            AZ(J,I)=A(3,IXIG3D(KXIG3D(4,I+NFT)+J-1))
            ENDIF
            IF(ANIM_V(4)>0)THEN
            CONTX(J,I)=CONT(1,IXIG3D(KXIG3D(4,I+NFT)+J-1))
            CONTY(J,I)=CONT(2,IXIG3D(KXIG3D(4,I+NFT)+J-1))
            CONTZ(J,I)=CONT(3,IXIG3D(KXIG3D(4,I+NFT)+J-1))            
            ENDIF
            IF(ANIM_V(5)==1) THEN
            FINTX(J,I)=FINT(1,IXIG3D(KXIG3D(4,I+NFT)+J-1))
            FINTY(J,I)=FINT(2,IXIG3D(KXIG3D(4,I+NFT)+J-1))
            FINTZ(J,I)=FINT(3,IXIG3D(KXIG3D(4,I+NFT)+J-1))    
            ENDIF
            IF(ANIM_V(6)==1) THEN
            FEXTX(J,I)=FEXT(1,IXIG3D(KXIG3D(4,I+NFT)+J-1))
            FEXTY(J,I)=FEXT(2,IXIG3D(KXIG3D(4,I+NFT)+J-1))
            FEXTZ(J,I)=FEXT(3,IXIG3D(KXIG3D(4,I+NFT)+J-1)) 
            ENDIF
            IF(ANIM_V(17)==1) THEN
            FREACTX(J,I)=FREAC(1,IXIG3D(KXIG3D(4,I+NFT)+J-1))
            FREACTY(J,I)=FREAC(2,IXIG3D(KXIG3D(4,I+NFT)+J-1))
            FREACTZ(J,I)=FREAC(3,IXIG3D(KXIG3D(4,I+NFT)+J-1)) 
            ENDIF
            WW(J,I)=1!WIGE(IXIG3D(KXIG3D(4,I+NFT)+J-1))
            DO K=1,PX+1
              KNOTLOCX(K,J,I)=KNOTLOCPC(K,1,(KXIG3D(2,I+NFT)-1)*NUMNOD+IXIG3D(KXIG3D(4,I+NFT)+J-1))
            ENDDO
            DO K=1,PY+1
              KNOTLOCY(K,J,I)=KNOTLOCPC(K,2,(KXIG3D(2,I+NFT)-1)*NUMNOD+IXIG3D(KXIG3D(4,I+NFT)+J-1))
            ENDDO
            DO K=1,PZ+1
              KNOTLOCZ(K,J,I)=KNOTLOCPC(K,3,(KXIG3D(2,I+NFT)-1)*NUMNOD+IXIG3D(KXIG3D(4,I+NFT)+J-1))
            ENDDO
           ENDIF
         ENDDO
         IDX(I) = KXIG3D(6,I+NFT)
         IDY(I) = KXIG3D(7,I+NFT)
         IDZ(I) = KXIG3D(8,I+NFT)
         IDX2(I) = KXIG3D(9,I+NFT)
         IDY2(I) = KXIG3D(10,I+NFT)
         IDZ2(I) = KXIG3D(11,I+NFT)
         KNOTLOCELX(1,I) = KNOTLOCEL(1,1,I+NFT)
         KNOTLOCELY(1,I) = KNOTLOCEL(1,2,I+NFT)
         KNOTLOCELZ(1,I) = KNOTLOCEL(1,3,I+NFT)
         KNOTLOCELX(2,I) = KNOTLOCEL(2,1,I+NFT)
         KNOTLOCELY(2,I) = KNOTLOCEL(2,2,I+NFT)
         KNOTLOCELZ(2,I) = KNOTLOCEL(2,3,I+NFT)
       ENDDO

       COUNT = 0 

       DO I=LFT,LLT
  
        PASX = (KNOTLOCELX(2,I) - KNOTLOCELX(1,I)) / THREE
        PASY = (KNOTLOCELY(2,I) - KNOTLOCELY(1,I)) / THREE
        PASZ = (KNOTLOCELZ(2,I) - KNOTLOCELZ(1,I)) / THREE
c        PASX = (KNOT(IAD_KNOT+IDX2(I)) - KNOT(IAD_KNOT+IDX(I))) / THREE
c        PASY = (KNOT(IAD_KNOT+NKNOT1+IDY2(I)) - KNOT(IAD_KNOT+NKNOT1+IDY(I))) / THREE
c        PASZ = (KNOT(IAD_KNOT+NKNOT1+NKNOT2+IDZ2(I)) - KNOT(IAD_KNOT+NKNOT1+NKNOT2+IDZ(I))) / THREE
c        PASX = (KNOT(IAD_KNOT+IDX(I)+1) - KNOT(IAD_KNOT+IDX(I))) / THREE
c        PASY = (KNOT(IAD_KNOT+NKNOT1+1+IDY(I)) - KNOT(IAD_KNOT+NKNOT1+IDY(I))) / THREE
c        PASZ = (KNOT(IAD_KNOT+NKNOT1+NKNOT2+1+IDZ(I)) - KNOT(IAD_KNOT+NKNOT1+NKNOT2+IDZ(I))) / THREE
 
        DO N=1,4
         DO M=1,4
          DO L=1,4  

           COUNT = COUNT+1
           COUNT3 = COUNT3+1 
      
           DO ITENS=1,3
            XXI(ITENS) = ZERO
            IF(ANIM_V(1)==1) THEN
             VI(ITENS) = ZERO
            ENDIF
            IF(ANIM_V(2)==1) THEN
             DI(ITENS) = ZERO
            ENDIF
            IF(ANIM_V(3)==1) THEN
             AI(ITENS) = ZERO
            ENDIF
            IF(ANIM_V(4)>0)THEN
             CONTI(ITENS) = ZERO
            ENDIF
            IF(ANIM_V(5)>0)THEN
             FINTI(ITENS) = ZERO
            ENDIF
            IF(ANIM_V(6)>0)THEN
             FEXTI(ITENS) = ZERO
            ENDIF
            IF(ANIM_V(17)>0)THEN
             FREACTI(ITENS) = ZERO
            ENDIF
           ENDDO 

           XI(1) = KNOTLOCELX(1,I) + (L-1)*PASX
           XI(2) = KNOTLOCELY(1,I) + (M-1)*PASY
           XI(3) = KNOTLOCELZ(1,I) + (N-1)*PASZ  
c           XI(1) = KNOT(IAD_KNOT+IDX(I)) + (L-1)*PASX
c           XI(2) = KNOT(IAD_KNOT+NKNOT1+IDY(I)) + (M-1)*PASY
c           XI(3) = KNOT(IAD_KNOT+NKNOT1+NKNOT2+IDZ(I)) + (N-1)*PASZ  
 
c           CALL IGE3DBASIS(
c     .       I      ,BID    ,XX(:,I) ,YY(:,I) ,ZZ(:,I) ,WW(:,I) ,
c     .       IDX(I) ,IDY(I) ,IDZ(I)  ,R       ,
c     .       NCTRL  ,XI(1)  ,XI(2)   ,XI(3)   ,KNOT(IAD_KNOT+1), KNOT(IAD_KNOT+NKNOT1+1),
c     .       KNOT(IAD_KNOT+NKNOT1+NKNOT2+1)

          CALL IG3DONEBASIS(
     1      I      ,BID        ,XX(:,I)  ,YY(:,I),
     2      ZZ(:,I),WW(:,I)    ,IDX(I)   ,IDY(I) ,
     3      IDZ(I) ,KNOTLOCX(:,:,I) ,KNOTLOCY(:,:,I),KNOTLOCZ(:,:,I),
     4      R          ,NCTRL  ,
     5      XI(1)  ,XI(2)      ,XI(3)    ,KNOT(IAD_KNOT+1),
     6      KNOT(IAD_KNOT+NKNOT1+1),KNOT(IAD_KNOT+NKNOT1+NKNOT2+1),PX-1, 
     7      PY-1   ,PZ-1       ,0        ,
     8      IDX2(I),IDY2(I)    ,IDZ2(I)   ,
     9      KNOTLOCELX(:,I),KNOTLOCELY(:,I),KNOTLOCELZ(:,I))

           DO J=1,NCTRL
             XXI(1) = XXI(1) + R(J)*XX(J,I)
             XXI(2) = XXI(2) + R(J)*YY(J,I)
             XXI(3) = XXI(3) + R(J)*ZZ(J,I)
             IF(ANIM_V(1)==1) THEN
             VI(1) = VI(1) + R(J)*VX(J,I)
             VI(2) = VI(2) + R(J)*VY(J,I)
             VI(3) = VI(3) + R(J)*VZ(J,I)
             ENDIF
             IF(ANIM_V(2)==1) THEN
             DI(1) = DI(1) + R(J)*DX(J,I)
             DI(2) = DI(2) + R(J)*DY(J,I)
             DI(3) = DI(3) + R(J)*DZ(J,I)
             ENDIF
             IF(ANIM_V(3)==1) THEN
             AI(1) = AI(1) + R(J)*AX(J,I)
             AI(2) = AI(2) + R(J)*AY(J,I)
             AI(3) = AI(3) + R(J)*AZ(J,I)
             ENDIF
             IF(ANIM_V(4)>0)THEN
             CONTI(1) = CONTI(1) + R(J)*CONTX(J,I)
             CONTI(2) = CONTI(2) + R(J)*CONTY(J,I)
             CONTI(3) = CONTI(3) + R(J)*CONTZ(J,I)
             ENDIF
             IF(ANIM_V(5)>0)THEN
             FINTI(1) = FINTI(1) + R(J)*FINTX(J,I)
             FINTI(2) = FINTI(2) + R(J)*FINTY(J,I)
             FINTI(3) = FINTI(3) + R(J)*FINTZ(J,I)
             ENDIF
             IF(ANIM_V(6)>0)THEN
             FEXTI(1) = FEXTI(1) + R(J)*FEXTX(J,I)
             FEXTI(2) = FEXTI(2) + R(J)*FEXTY(J,I)
             FEXTI(3) = FEXTI(3) + R(J)*FEXTZ(J,I)
             ENDIF
             IF(ANIM_V(17)>0)THEN
             FREACTI(1) = FREACTI(1) + R(J)*FREACTX(J,I)
             FREACTI(2) = FREACTI(2) + R(J)*FREACTY(J,I)
             FREACTI(3) = FREACTI(3) + R(J)*FREACTZ(J,I)
             ENDIF
           ENDDO

           DO ITENS=1,3
             X_TEMP(ITENS,COUNT + DECAL*64) = XXI(ITENS)
             IF(ANIM_V(1)==1) THEN
             V_TEMP(ITENS,COUNT + DECAL*64) = VI(ITENS)
             ENDIF
             IF(ANIM_V(2)==1) THEN
             D_TEMP(ITENS,COUNT + DECAL*64) = DI(ITENS)
             ENDIF
             IF(ANIM_V(3)==1) THEN
             A_TEMP(ITENS,COUNT + DECAL*64) = AI(ITENS)
             ENDIF
             IF(ANIM_V(4)>0)THEN
             CONT_TEMP(ITENS,COUNT + DECAL*64) = CONTI(ITENS)
             ENDIF
             IF(ANIM_V(5)>0)THEN
             FINT_TEMP(ITENS,COUNT + DECAL*64) = FINTI(ITENS)
             ENDIF
             IF(ANIM_V(6)>0)THEN
             FEXT_TEMP(ITENS,COUNT + DECAL*64) = FEXTI(ITENS)
             ENDIF
             IF(ANIM_V(17)>0)THEN
             FREACT_TEMP(ITENS,COUNT + DECAL*64) = FREACTI(ITENS)
             ENDIF
           ENDDO
 
          ENDDO
         ENDDO
        ENDDO

        IDBRICK=0
        DO L=1,2+1
         DO M=0,2
          DO N=0,2
           IDBRICK = IDBRICK+1
           IG3DSOLID(1,IDBRICK,I+NFT) = NUMNOD + 64*(I-1) + DECAL*64 - 1 + L    +M    *(2+2)+N    *((2+2)*(2+2))
           IG3DSOLID(2,IDBRICK,I+NFT) = NUMNOD + 64*(I-1) + DECAL*64 - 1 + (L+1)+M    *(2+2)+N    *((2+2)*(2+2))
           IG3DSOLID(3,IDBRICK,I+NFT) = NUMNOD + 64*(I-1) + DECAL*64 - 1 + (L+1)+(M+1)*(2+2)+N    *((2+2)*(2+2))
           IG3DSOLID(4,IDBRICK,I+NFT) = NUMNOD + 64*(I-1) + DECAL*64 - 1 + L    +(M+1)*(2+2)+N    *((2+2)*(2+2))
           IG3DSOLID(5,IDBRICK,I+NFT) = NUMNOD + 64*(I-1) + DECAL*64 - 1 + L    +M    *(2+2)+(N+1)*((2+2)*(2+2))
           IG3DSOLID(6,IDBRICK,I+NFT) = NUMNOD + 64*(I-1) + DECAL*64 - 1 + (L+1)+M    *(2+2)+(N+1)*((2+2)*(2+2))
           IG3DSOLID(7,IDBRICK,I+NFT) = NUMNOD + 64*(I-1) + DECAL*64 - 1 + (L+1)+(M+1)*(2+2)+(N+1)*((2+2)*(2+2))
           IG3DSOLID(8,IDBRICK,I+NFT) = NUMNOD + 64*(I-1) + DECAL*64 - 1 + L    +(M+1)*(2+2)+(N+1)*((2+2)*(2+2))
          ENDDO
         ENDDO
        ENDDO

       ENDDO

c     METHODE DE PROJECTION PAR LES MOINDRES CARRES SUR LE PATCH 

       COUNT2 = 0

       DO IR=1,PX
        DO IS=1,PY
         DO IT=1,PZ 
         
          COUNT2 = COUNT2+1 
          LBUF => ELBUF_TAB(ING)%BUFLY(1)%LBUF(IR,IS,IT)

          ZR = A_GAUSS(IR,PX)
          ZS = A_GAUSS(IS,PY)
          ZT = A_GAUSS(IT,PZ)
          PGAUSS = W_GAUSS(IR,PX)*W_GAUSS(IS,PY)*W_GAUSS(IT,PZ)

          DO I=LFT,LLT
          JJ = 6*(I-1)

c           CALL IGE3DDERIV(
c     .       I    ,BID   ,XX(:,I),YY(:,I),ZZ(:,I),WW(:,I),  
c     .       IDX(I), IDY(I), IDZ(I), TBID, R, DETJAC, 
c     .       NCTRL, ZR, ZS, ZT, KNOT(IAD_KNOT+1), KNOT(IAD_KNOT+NKNOT1+1),
c     .       KNOT(IAD_KNOT+NKNOT1+NKNOT2+1), PX-1, PY-1, PZ-1, 1)

          CALL IG3DONEDERIV(
     1      I      ,BID    ,XX(:,I),YY(:,I),
     2      ZZ(:,I),WW(:,I),IDX(I) ,IDY(I) ,
     3      IDZ(I) ,KNOTLOCX(:,:,I) ,KNOTLOCY(:,:,I),KNOTLOCZ(:,:,I) ,
     4      TBID      ,R             ,DETJAC      ,NCTRL    ,
     5      ZR        ,ZS            ,ZT          ,KNOT(IAD_KNOT+1),
     6      KNOT(IAD_KNOT+NKNOT1+1),KNOT(IAD_KNOT+NKNOT1+NKNOT2+1),PX-1, 
     7      PY-1      ,PZ-1          ,1           ,
     8      IDX2(I),IDY2(I)    ,IDZ2(I)   ,
     9      KNOTLOCELX(:,I),KNOTLOCELY(:,I),KNOTLOCELZ(:,I))

           DO J=1,NCTRL
            INCTRL = IXIG3D(KXIG3D(4,I+NFT)+J-1) 
             MAB(INCTRL) = MAB(INCTRL) + R(J)*DETJAC*PGAUSS
             DO ITENS=1,6
              RI(ITENS,INCTRL) = RI(ITENS,INCTRL) + R(J)*LBUF%SIG((ITENS-1)*NEL+I)*DETJAC*PGAUSS
             ENDDO
           ENDDO  
          ENDDO 

         ENDDO
        ENDDO
       ENDDO

       DECAL = DECAL + NEL

      ENDDO 

      DO ING=NG,NG+NBG-1

       NFT=IPARG(3,ING)
       NEL=IPARG(2,ING)
       LLT=MIN(NVSIZ,NEL)

       DO I=LFT,LLT
        DO J=1,NCTRL   
         INCTRL = IXIG3D(KXIG3D(4,I+NFT)+J-1) 
         DO ITENS=1,6
          FI(ITENS,INCTRL) = RI(ITENS,INCTRL)/MAB(INCTRL)
         ENDDO
        ENDDO
       ENDDO

      ENDDO

      COUNT3 = 0 

      DO ING=NG,NG+NBG-1

       NFT=IPARG(3,ING)
       NEL=IPARG(2,ING)
       LLT=MIN(NVSIZ,NEL)

       DO I=LFT,LLT

        PASX = (KNOTLOCELX(2,I) - KNOTLOCELX(1,I)) / THREE
        PASY = (KNOTLOCELY(2,I) - KNOTLOCELY(1,I)) / THREE
        PASZ = (KNOTLOCELZ(2,I) - KNOTLOCELZ(1,I)) / THREE
c        PASX = (KNOT(IAD_KNOT+IDX2(I)) - KNOT(IAD_KNOT+IDX(I))) / THREE
c        PASY = (KNOT(IAD_KNOT+NKNOT1+IDY2(I)) - KNOT(IAD_KNOT+NKNOT1+IDY(I))) / THREE
c        PASZ = (KNOT(IAD_KNOT+NKNOT1+NKNOT2+IDZ2(I)) - KNOT(IAD_KNOT+NKNOT1+NKNOT2+IDZ(I))) / THREE
c        PASX = (KNOT(IAD_KNOT+IDX(I)+1) - KNOT(IAD_KNOT+IDX(I))) / THREE
c        PASY = (KNOT(IAD_KNOT+NKNOT1+1+IDY(I)) - KNOT(IAD_KNOT+NKNOT1+IDY(I))) / THREE
c        PASZ = (KNOT(IAD_KNOT+NKNOT1+NKNOT2+1+IDZ(I)) - KNOT(IAD_KNOT+NKNOT1+NKNOT2+IDZ(I))) / THREE

        DO N=1,4
         DO M=1,4
          DO L=1,4   

           XI(1) = KNOTLOCELX(1,I) + (L-1)*PASX
           XI(2) = KNOTLOCELY(1,I) + (M-1)*PASY
           XI(3) = KNOTLOCELZ(1,I) + (N-1)*PASZ  
c           XI(1) = KNOT(IAD_KNOT+IDX(I)) + (L-1)*PASX
c           XI(2) = KNOT(IAD_KNOT+NKNOT1+IDY(I)) + (M-1)*PASY
c           XI(3) = KNOT(IAD_KNOT+NKNOT1+NKNOT2+IDZ(I)) + (N-1)*PASZ

c           CALL IGE3DBASIS(
c     .       I      ,BID    ,XX(:,I) ,YY(:,I) ,ZZ(:,I) ,WW(:,I) ,
c     .       IDX(I) ,IDY(I) ,IDZ(I)  ,R       ,
c     .       NCTRL  ,XI(1)  ,XI(2)   ,XI(3)   ,KNOT(IAD_KNOT+1), KNOT(IAD_KNOT+NKNOT1+1),
c     .       KNOT(IAD_KNOT+NKNOT1+NKNOT2+1)   ,PX-1    ,PY-1    ,PZ-1    ,0 )

          CALL IG3DONEBASIS(
     1      I         ,BID           ,XX(:,I)  ,YY(:,I),
     2      ZZ(:,I),WW(:,I)    ,IDX(I)   ,IDY(I) ,
     3      IDZ(I) ,KNOTLOCX(:,:,I) ,KNOTLOCY(:,:,I),KNOTLOCZ(:,:,I) ,
     4      R             ,NCTRL    ,
     5      XI(1)  ,XI(2)   ,XI(3)         ,KNOT(IAD_KNOT+1),
     6      KNOT(IAD_KNOT+NKNOT1+1),KNOT(IAD_KNOT+NKNOT1+NKNOT2+1),PX-1, 
     7      PY-1      ,PZ-1        ,0           ,
     8      IDX2(I),IDY2(I)    ,IDZ2(I)   ,
     9      KNOTLOCELX(:,I),KNOTLOCELY(:,I),KNOTLOCELZ(:,I))   
           COUNT3 = COUNT3+1 

           DO ITENS=1,6
            S(ITENS) = ZERO  
           ENDDO

           DO J=1,NCTRL
            INCTRL = IXIG3D(KXIG3D(4,I+NFT)+J-1) 
             DO ITENS=1,6
              S(ITENS) = S(ITENS) + R(J)*FI(ITENS,INCTRL)
             ENDDO
           ENDDO
 
           DO ITENS=1,6
            TABSTRESL(ITENS,COUNT3 + NISOELCUT*64) = S(ITENS)
           ENDDO

          ENDDO
         ENDDO
        ENDDO
       ENDDO

      ENDDO

      DEALLOCATE(MAB,RI,FI)

      NISOELCUT = DECAL 
C-----------
      RETURN
      END

