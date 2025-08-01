Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    i10lagm   ../engine/source/interfaces/int16/i10lagm.F
!||--- called by ------------------------------------------------------
!||    i16main   ../engine/source/interfaces/int16/i16main.F
!||--- calls      -----------------------------------------------------
!||    i10lll    ../engine/source/interfaces/int16/i10lagm.F
!||====================================================================
      SUBROUTINE I10LAGM(X    ,V      ,LLL     ,JLL   ,SLL   ,
     2                  XLL   ,CANDN  ,CANDE   ,I_STOK,IXS   ,
     3                  IXS10 ,IADLL  ,EMINX   ,NSV   ,NELEM ,
     4                  N_MUL_MX,ITASK  ,A     ,ITIED ,
     5                  NINT  ,NKMAX  ,COMNTAG )
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
#include      "task_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I_STOK,N_MUL_MX,ITASK,ITIED,NINT,NKMAX ,
     .  LLL(*),JLL(*),SLL(*),CANDN(*),CANDE(*),COMNTAG(*),
     .  IXS(NIXS,*),IXS10(6,*),IADLL(*),NSV(*)  ,NELEM(*) 
C     REAL
      my_real
     .  X(3,*),V(3,*),XLL(*),
     .  EMINX(6,*),A(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,IK,IE,IS,IC,NK,III(MVSIZ,7),LLT,NFT,LE,FIRST,LAST,
     .        I10
      my_real
     .   XX(MVSIZ,7),YY(MVSIZ,7),ZZ(MVSIZ,7),
     .   AA,XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,DIST
C-----------------------------------------------
C
C
C       | M | Lt| | a    | M ao
C       |---+---| |    = |
C       | L | 0 | | la   | bo
C
C        [M] a + [L]t la = [M] ao
C        [L] a = bo
C
C         a = -[M]-1[L]t la + ao
C        [L][M]-1[L]t la  = [L] ao - bo
C
C on pose:
C        [H] = [L][M]-1[L]t
C        b  = [L] ao - bo
C
C        [H] la = b
C
C        a = ao - [M]-1[L]t la
C-----------------------------------------------
C
C        la              : LAMBDA(NC)
C        ao              : A(NUMNOD)
C        L               : XLL(NK,NC)
C        M               : MAS(NUMNOD)
C        [L][M]-1[L]t la : HLA(NC)
C        [L] ao - b      : B(NC)
C        [M]-1[L]t la    : LTLA(NUMNOD)
C
C        NC : nombre de contact 
C        NK : nombre de noeud pour un contact (8+1,16+1,8+8,16+16)
C
C        IC : numero du contact (1,NC)
C        IK : numero de noeud local a un contact (1,NK)
C        I  : numero global du noeud (1,NUMNOD)
C
C        IADLL(NC)        : IAD = IADLL(IC)
C        LLL(NC*(7,21))  : I  = LLL(IAD+1,2...IADNEXT-1)
C-----------------------------------------------
C  evaluation de b:
C
C         Vs = Somme(Ni Vi)
C         Vs_ + dt As = Somme(Ni Vi_) + Somme(dt Ni Ai)
C         Somme(dt Ni Ai) - dt As =  Vs_ -Somme(Ni Vi_)
C         [L] = dt {N1,N2,..,N15,-1}
C         bo = [L] a = -[L]/dt v_
C         b  = [L] ao - bo
C         b  = [L] ao + [L]/dt v_ = [L] (v_ + ao dt)/dt
C-----------------------------------------------
C                 b  = [L] vo+/dt   +   vout
C-----------------------------------------------
C-----------------------------------------------------------------------
C     boucle sur les candidats au contact   
C-----------------------------------------------------------------------
      FIRST = 1 + I_STOK * ITASK / NTHREAD
      LAST = I_STOK*(ITASK+1) / NTHREAD
      LLT = 0
      NFT=LLT+1
      DO IC=FIRST,LAST
       LE = CANDE(IC)
       IE = NELEM(LE)
       I10 = IE - NUMELS8
C-----------------------------------------------------------------------
C      test si shell 16  
C-----------------------------------------------------------------------
       IF(I10.GE .1.AND.I10.LE .NUMELS10)THEN
        IS = NSV(CANDN(IC))
        DIST = -1.E30
        DIST = MAX(EMINX(1,LE)-X(1,IS)-DT2*(V(1,IS)+DT12*A(1,IS)),
     .             X(1,IS)+DT2*(V(1,IS)+DT12*A(1,IS))-EMINX(4,LE),DIST)
        DIST = MAX(EMINX(2,LE)-X(2,IS)-DT2*(V(2,IS)+DT12*A(2,IS)),
     .             X(2,IS)+DT2*(V(2,IS)+DT12*A(2,IS))-EMINX(5,LE),DIST)
        DIST = MAX(EMINX(3,LE)-X(3,IS)-DT2*(V(3,IS)+DT12*A(3,IS)),
     .             X(3,IS)+DT2*(V(3,IS)+DT12*A(3,IS))-EMINX(6,LE),DIST)
c        IF (DIST<0.) CANDN(I) = -CANDN(I)
C-----------------------------------------------------------------------
C       test si dans la boite   
C-----------------------------------------------------------------------
        IF(DIST.LE .ZERO)THEN
c
c      print *, "dans la boite",XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX
c
          LLT = LLT+1
          III(LLT,7)=IS
          XX(LLT,7)=X(1,IS)
          YY(LLT,7)=X(2,IS)
          ZZ(LLT,7)=X(3,IS)
          III(LLT,1)=IXS(2,IE)
          III(LLT,2)=IXS(4,IE)
          III(LLT,3)=IXS(6,IE)
          III(LLT,4)=IXS(7,IE)
          DO K=1,6
            III(LLT,K+8)=IXS10(K,I10)
          ENDDO
          DO K=1,7
            I = III(LLT,K)
            XX(LLT,K)=X(1,I)
            YY(LLT,K)=X(2,I)
            ZZ(LLT,K)=X(3,I)
          ENDDO
c
C-----------------------------------------------------------------------
C     calcul de  [L] par paquet de mvsiz   
C-----------------------------------------------------------------------
          IF(LLT==MVSIZ-1)THEN
            CALL I10LLL(
     1       LLT ,LLL       ,JLL       ,SLL       ,XLL     ,V       ,
     2       XX  ,YY        ,ZZ        ,III       ,IADLL   ,
     3       N_MUL_MX ,A    ,X         ,ITIED     ,NINT    ,NKMAX   ,
     4       COMNTAG )
            NFT=LLT+1
            LLT = 0
          ENDIF
        ELSE
c debug
          k=0
        ENDIF
       ENDIF
      ENDDO
C-----------------------------------------------------------------------
C     calcul de  [L] pour dernier paquet   
C-----------------------------------------------------------------------
      IF(LLT/=0) CALL I10LLL(
     1       LLT ,LLL       ,JLL       ,SLL       ,XLL     ,V       ,
     2       XX  ,YY        ,ZZ        ,III       ,IADLL   ,
     3       N_MUL_MX ,A    ,X         ,ITIED     ,NINT    ,NKMAX   ,
     4       COMNTAG )
C
C-----------------------------------------------
      RETURN
      END
!||====================================================================
!||    i10lll        ../engine/source/interfaces/int16/i10lagm.F
!||--- called by ------------------------------------------------------
!||    i10lagm       ../engine/source/interfaces/int16/i10lagm.F
!||--- calls      -----------------------------------------------------
!||    ancmsg        ../engine/source/output/message/message.F
!||    arret         ../engine/source/system/arret.F
!||    i10rst        ../engine/source/interfaces/int16/i10lagm.F
!||--- uses       -----------------------------------------------------
!||    message_mod   ../engine/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE I10LLL(LLT  ,LLL       ,JLL  ,SLL  ,XLL  ,V     ,
     2                  XX   ,YY        ,ZZ   ,III  ,IADLL ,
     3                  N_MUL_MX,A      ,X    ,ITIED,NINT ,NKMAX ,
     4                  COMNTAG )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
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
#include      "com08_c.inc"
      COMMON /LAGGLOB/N_MULT
      INTEGER N_MULT
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LLT,N_MUL_MX,ITIED,NINT ,NKMAX   
      INTEGER LLL(*),JLL(*),SLL(*),COMNTAG(*),
     .        III(MVSIZ,7),IADLL(*)
C     REAL
      my_real
     .  XLL(*),V(3,*),A(3,*)
      my_real
     .   XX(MVSIZ,7),YY(MVSIZ,7),ZZ(MVSIZ,7),X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IK,NK,I1,I2,I3,I4,IAD,NN
      my_real
     .   VX,VY,VZ,VN,AA
      my_real
     .   R(MVSIZ),S(MVSIZ),T(MVSIZ),
     .   NX(MVSIZ), NY(MVSIZ), NZ(MVSIZ),
     .   NI(MVSIZ,7) 
C-----------------------------------------------
C      calcul de r,s,t
C-----------------------------------------------
c
c      print *, "XX(1,1),XX(1,9)",XX(1,1),XX(1,9)
c
      CALL I10RST(LLT   ,R     ,S     ,T     ,NI    ,
     2            NX    ,NY    ,NZ    ,XX    ,YY    ,ZZ    )
C-----------------------------------------------
C      calcul de [L]
C-----------------------------------------------
      IF(ITIED==0)THEN
       DO I=1,LLT
C-----------------------------------------------
C       test si contact
C-----------------------------------------------
        IF(R(I)>=-ONE.AND.S(I)>=-ONE.AND.T(I)>=-ONE.AND.
     .     R(I)<= ONE.AND.S(I)<= ONE.AND.T(I)<= ONE)THEN
C
          NK = 7
          VX = ZERO
          VY = ZERO
          VZ = ZERO
          DO IK=1,NK
            VX = VX - (V(1,III(I,IK))+DT12*A(1,III(I,IK)))*NI(I,IK)
            VY = VY - (V(2,III(I,IK))+DT12*A(2,III(I,IK)))*NI(I,IK)
            VZ = VZ - (V(3,III(I,IK))+DT12*A(3,III(I,IK)))*NI(I,IK)
          ENDDO
c
c
          VN = NX(I)*VX + NY(I)*VY + NZ(I)*VZ
C-----------------------------------------------
C         test si vitesse entrante en s
C-----------------------------------------------
          IF(S(I)*VN<=ZERO)THEN
c
c      print *, "vitesse entrante",vn
c      print *, "s = ",S(I)
c
           AA = ONE/SQRT(NX(I)*NX(I)+NY(I)*NY(I)+NZ(I)*NZ(I))
           NX(I) = NX(I)*AA
           NY(I) = NY(I)*AA
           NZ(I) = NZ(I)*AA
#include "lockon.inc"
            N_MULT=N_MULT+1
            IF(N_MULT>N_MUL_MX)THEN
#include "lockoff.inc"
              CALL ANCMSG(MSGID=84,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
            IADLL(N_MULT+1)=IADLL(N_MULT) + 21
            IF(IADLL(N_MULT+1)-1>NKMAX)THEN
#include "lockoff.inc"
              CALL ANCMSG(MSGID=84,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
            IAD = IADLL(N_MULT) - 1
            DO IK=1,7
                LLL(IAD+IK)    = III(I,IK)
                JLL(IAD+IK)    = 1
                SLL(IAD+IK)    = 0
                XLL(IAD+IK)    = NX(I)*NI(I,IK)
                LLL(IAD+IK+7)  = III(I,IK)
                JLL(IAD+IK+7)  = 2
                SLL(IAD+IK+7)  = 0
                XLL(IAD+IK+7)  = NY(I)*NI(I,IK)
                LLL(IAD+IK+14) = III(I,IK)
                JLL(IAD+IK+14) = 3
                SLL(IAD+IK+14) = 0
                XLL(IAD+IK+14) = NZ(I)*NI(I,IK)
                NN = LLL(IAD+IK)
                COMNTAG(NN) = COMNTAG(NN) + 1
            ENDDO
            SLL(IAD+7) = NINT
            SLL(IAD+14) = NINT
            SLL(IAD+21) = NINT
#include "lockoff.inc"
          ENDIF
        ENDIF
       ENDDO
      ELSEIF(ITIED==1)THEN
C-----------------------------------------------
C      ITIED = 1
C-----------------------------------------------
       DO I=1,LLT
C-----------------------------------------------
C       test si contact
C-----------------------------------------------
        IF(R(I)>=-ONE.AND.S(I)>=-ONE.AND.T(I)>=-ONE.AND.
     .     R(I)<= ONE.AND.S(I)<= ONE.AND.T(I)<= ONE)THEN
C
          NK = 7
          VX = ZERO
          VY = ZERO
          VZ = ZERO
          DO IK=1,NK
            VX = VX - (V(1,III(I,IK))+DT12*A(1,III(I,IK)))*NI(I,IK)
            VY = VY - (V(2,III(I,IK))+DT12*A(2,III(I,IK)))*NI(I,IK)
            VZ = VZ - (V(3,III(I,IK))+DT12*A(3,III(I,IK)))*NI(I,IK)
          ENDDO
c
c      print *, "vx,vy,vz s-m",vx,vy,vz
c      print *, "nx,ny,nz ", NX(I),NY(I),NZ(I)
c
          VN = NX(I)*VX + NY(I)*VY + NZ(I)*VZ
C-----------------------------------------------
C         test si vitesse entrante en s
C-----------------------------------------------
          IF(S(I)*VN<=ZERO)THEN
c          
c      print *, "vitesse entrante",vn
c      print *, "s = ",S(I)
c
#include "lockon.inc"
            IF(N_MULT+3>N_MUL_MX)THEN
#include "lockoff.inc"
              CALL ANCMSG(MSGID=84,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
            IF(IADLL(N_MULT+1)-1+7*3>NKMAX)THEN
#include "lockoff.inc"
              CALL ANCMSG(MSGID=84,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
C
            N_MULT=N_MULT+1
            IADLL(N_MULT+1)=IADLL(N_MULT) + 7
            IAD = IADLL(N_MULT) - 1
            DO IK=1,7
                LLL(IAD+IK) = III(I,IK)
                JLL(IAD+IK) = 1
                SLL(IAD+IK) = 0
                XLL(IAD+IK) = NI(I,IK)
                NN = LLL(IAD+IK)
                COMNTAG(NN) = COMNTAG(NN) + 1
            ENDDO
            SLL(IAD+7) = NINT
C
            N_MULT=N_MULT+1
            IADLL(N_MULT+1)=IADLL(N_MULT) + 7
            IAD = IADLL(N_MULT) - 1
            DO IK=1,7
                LLL(IAD+IK) = III(I,IK)
                JLL(IAD+IK) = 2
                SLL(IAD+IK) = 0
                XLL(IAD+IK) = NI(I,IK)
                NN = LLL(IAD+IK)
                COMNTAG(NN) = COMNTAG(NN) + 1
            ENDDO
            SLL(IAD+7) = NINT
C
            N_MULT=N_MULT+1
            IADLL(N_MULT+1)=IADLL(N_MULT) + 7
            IAD = IADLL(N_MULT) - 1
            DO IK=1,7
                LLL(IAD+IK) = III(I,IK)
                JLL(IAD+IK) = 3
                SLL(IAD+IK) = 0
                XLL(IAD+IK) = NI(I,IK)
                NN = LLL(IAD+IK)
                COMNTAG(NN) = COMNTAG(NN) + 1
            ENDDO
            SLL(IAD+7) = NINT
#include "lockoff.inc"
          ENDIF
        ENDIF
       ENDDO
      ELSE
C-----------------------------------------------
C      ITIED = 2
C-----------------------------------------------
       DO I=1,LLT
C-----------------------------------------------
C       test si contact
C-----------------------------------------------
        IF(R(I)>=-ONE.AND.S(I)>=-ONE.AND.T(I)>=-ONE.AND.
     .     R(I)<= ONE.AND.S(I)<= ONE.AND.T(I)<= ONE)THEN
C
           NK = 7
C-----------------------------------------------
c      print *, "s = ",S(I)
c
#include "lockon.inc"
            IF(N_MULT+3>N_MUL_MX)THEN
#include "lockoff.inc"
              CALL ANCMSG(MSGID=84,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
            IF(IADLL(N_MULT+1)-1+7*3>NKMAX)THEN
#include "lockoff.inc"
              CALL ANCMSG(MSGID=84,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
            N_MULT=N_MULT+1
            IADLL(N_MULT+1)=IADLL(N_MULT) + 7
            IAD = IADLL(N_MULT) - 1
            DO IK=1,7
                LLL(IAD+IK) = III(I,IK)
                JLL(IAD+IK) = 1
                SLL(IAD+IK) = 0
                XLL(IAD+IK) = NI(I,IK)
                NN = LLL(IAD+IK)
                COMNTAG(NN) = COMNTAG(NN) + 1
            ENDDO
            SLL(IAD+7) = NINT
C
            N_MULT=N_MULT+1
            IADLL(N_MULT+1)=IADLL(N_MULT) + 7
            IAD = IADLL(N_MULT) - 1
            DO IK=1,7
                LLL(IAD+IK) = III(I,IK)
                JLL(IAD+IK) = 2
                SLL(IAD+IK) = 0
                XLL(IAD+IK) = NI(I,IK)
                NN = LLL(IAD+IK)
                COMNTAG(NN) = COMNTAG(NN) + 1
            ENDDO
            SLL(IAD+7) = NINT
C
            N_MULT=N_MULT+1
            IADLL(N_MULT+1)=IADLL(N_MULT) + 7
            IAD = IADLL(N_MULT) - 1
            DO IK=1,7
                LLL(IAD+IK) = III(I,IK)
                JLL(IAD+IK) = 3
                SLL(IAD+IK) = 0
                XLL(IAD+IK) = NI(I,IK)
                NN = LLL(IAD+IK)
                COMNTAG(NN) = COMNTAG(NN) + 1
            ENDDO
            SLL(IAD+7) = NINT
C
#include "lockoff.inc"
        ENDIF
       ENDDO
      ENDIF
c
c      print *, "r,s,t",r(1),s(1),t(1)
C
      RETURN
      END
C
!||====================================================================
!||    i10rst    ../engine/source/interfaces/int16/i10lagm.F
!||--- called by ------------------------------------------------------
!||    i10lll    ../engine/source/interfaces/int16/i10lagm.F
!||--- calls      -----------------------------------------------------
!||    i10deri   ../engine/source/interfaces/int16/i10lagm.F
!||    i10ni     ../engine/source/interfaces/int16/i10lagm.F
!||    i10rstn   ../engine/source/interfaces/int16/i10lagm.F
!||====================================================================
      SUBROUTINE I10RST(LLT,R  ,S     ,T     ,NI    ,
     2            NX  ,NY  ,NZ  ,XX    ,YY    ,ZZ    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LLT
C     REAL
      my_real
     .   XX(MVSIZ,7),YY(MVSIZ,7),ZZ(MVSIZ,7)
      my_real
     .   R(MVSIZ),S(MVSIZ),T(MVSIZ),NI(MVSIZ,7) ,
     .   NX(MVSIZ),NY(MVSIZ),NZ(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IK,NK,ITER,NITERMAX,JTER,NJTERMAX,CONV
      my_real
     .   VX,VY,VZ,VN
      my_real
     .   DRDX(MVSIZ),DRDY(MVSIZ),DRDZ(MVSIZ),
     .   DSDX(MVSIZ),DSDY(MVSIZ),DSDZ(MVSIZ),     
     .   DTDX(MVSIZ),DTDY(MVSIZ),DTDZ(MVSIZ),
     .   DXDR(MVSIZ),DYDR(MVSIZ),DZDR(MVSIZ),
     .   DXDT(MVSIZ),DYDT(MVSIZ),DZDT(MVSIZ),
     .   RR(MVSIZ),SS(MVSIZ),TT(MVSIZ)     
C-----------------------------------------------
C
C      r=s=t=0
C
C  +---> iter 
C  |                  
C  |     Ni(r,s,t) =  
C  |     dNi/dr    =  
C  |     ...         _ 
C  |                \
C  |     dx/dr    = /_ (xi * dNi/dr)
C  |     ...        
C  | 
C  |            [dx/dr dy/dr dz/dr]            
C  |      [J] = |dx/ds dy/ds dz/ds|                
C  |            [dx/dt dy/dt dz/dt]               
C  | 
C  | +--> jter                                 
C  | |                  _ 
C  | |                 \
C  | |      x(r,s,t) = /_ (xi * Ni(r,s,t))
C  | |      ...        
C  | |
C  | |      |r|   |r|      -1  |xs-x(r,s,t)|
C  | |      {s} = {s} + [J]    {ys-y(r,s,t)}
C  | |      |t|   |t|          |zs-z(r,s,t)|
C  | |                   
C  | |      Ni(r,s,t) =  
C  +-+---
C-----------------------------------------------
       NITERMAX = 3
       NJTERMAX = 3
       CONV = 0
C
       DO I=1,LLT
         RR(I) = ZERO
         SS(I) = ZERO
         TT(I) = ZERO
       ENDDO
C-----------------------------------------------
C      calcul de r,s,t   et   Ni(r,s,t)
C-----------------------------------------------
       DO ITER=1,NITERMAX
c
c      print *, "iter",iter
c
C-----------------------------------------------
C          calcul de Ni(r,s,t); [J]; [J]-1
C-----------------------------------------------
           CALL I10DERI(LLT,RR ,SS    ,TT    ,NI    ,
     2            DRDX  ,DRDY  ,DRDZ  ,DSDX  ,DSDY  ,DSDZ  ,
     3            DTDX  ,DTDY  ,DTDZ  ,DXDR  ,DYDR  ,DZDR  ,
     4            DXDT  ,DYDT  ,DZDT  ,XX    ,YY    ,ZZ    )
C
           DO JTER=1,NJTERMAX
c
c      print *, "jter",jter
c
C-----------------------------------------------
C            calcul de r,s,t new
C-----------------------------------------------
             CALL I10RSTN(LLT,RR,SS   ,TT    ,NI    ,CONV  ,
     2            DRDX  ,DRDY  ,DRDZ  ,DSDX  ,DSDY  ,DSDZ  ,
     3            DTDX  ,DTDY  ,DTDZ  ,XX    ,YY    ,ZZ    ,
     4            R     ,S     ,T     )
c
c      print *, "r,s,t",r(1),s(1),t(1)
c      print *, "rr,ss,tt",rr(1),ss(1),tt(1)
c
C-----------------------------------------------
C            calcul de Ni(-1<r<1 , -1<s<1 , -1<t<1)
C-----------------------------------------------
             CALL I10NI(LLT,RR ,SS ,TT ,NI  )
C  pb de parith on si conv fonction de mvsiz !!!!!!!
C             IF(CONV/=0)RETURN
C
           ENDDO
      ENDDO
C
       DO I=1,LLT
         NX(I) = DYDT(I)*DZDR(I) - DZDT(I)*DYDR(I)
         NY(I) = DZDT(I)*DXDR(I) - DXDT(I)*DZDR(I)
         NZ(I) = DXDT(I)*DYDR(I) - DYDT(I)*DXDR(I)
       ENDDO
C
      RETURN
      END
!||====================================================================
!||    i10ni    ../engine/source/interfaces/int16/i10lagm.F
!||--- called by ------------------------------------------------------
!||    i10rst   ../engine/source/interfaces/int16/i10lagm.F
!||====================================================================
      SUBROUTINE I10NI(LLT,RR ,SS ,TT ,NI    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LLT
      my_real
     .   RR(MVSIZ),SS(MVSIZ),TT(MVSIZ),NI(MVSIZ,7)  
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .  U_M_R,U_P_R,U_M_S,U_P_S,U_M_T,U_P_T,
     .  UMS_UMT,UMS_UPT,UPS_UMT,UPS_UPT,
     .  UMR_UMS,UMR_UPS,UPR_UMS,UPR_UPS,
     .  UMT_UMR,UMT_UPR,UPT_UMR,UPT_UPR,
     .  A,R05,S05,T05
C-----------------------------------------------------------------------
C     calcul de Ni
C-----------------------------------------------------------------------
      DO I=1,LLT
C
        R05 = HALF*RR(I)
        S05 = HALF*SS(I)
        T05 = HALF*TT(I)
C
        U_M_R = HALF - R05
        U_P_R = HALF + R05
C
        U_M_S = HALF - S05
        U_P_S = HALF + S05
C
        U_M_T = HALF - T05
        U_P_T = HALF + T05
C
        UMS_UMT = U_M_S * U_M_T
        UMS_UPT = U_M_S * U_P_T
        UPS_UMT = U_P_S * U_M_T
        UPS_UPT = U_P_S * U_P_T
C
        UMR_UMS = U_M_R * U_M_S
        UMR_UPS = U_M_R * U_P_S
        UPR_UMS = U_P_R * U_M_S
        UPR_UPS = U_P_R * U_P_S
C
        UMT_UMR = U_M_T * U_M_R
        UMT_UPR = U_M_T * U_P_R
        UPT_UMR = U_P_T * U_M_R
        UPT_UPR = U_P_T * U_P_R
C
        A = -RR(I)-TT(I)-ONE
        NI(I,1) = U_M_R * UMS_UMT * A
        NI(I,2) = U_M_R * UMS_UPT * A
        NI(I,3) = U_P_R * UMS_UPT * A
        NI(I,4) = U_P_R * UMS_UMT * A
        NI(I,5) = U_M_R * UPS_UMT * A
        NI(I,6) = U_M_R * UPS_UPT * A
C------------------------------------
        NI(I,7) = -1.
C------------------------------------
      ENDDO
C-----------------------------------------------
      RETURN
      END
!||====================================================================
!||    i10rstn   ../engine/source/interfaces/int16/i10lagm.F
!||--- called by ------------------------------------------------------
!||    i10rst    ../engine/source/interfaces/int16/i10lagm.F
!||====================================================================
      SUBROUTINE I10RSTN(LLT,RR,SS    ,TT    ,NI    ,CONV  ,
     2            DRDX  ,DRDY  ,DRDZ  ,DSDX  ,DSDY  ,DSDZ  ,
     3            DTDX  ,DTDY  ,DTDZ  ,XX    ,YY    ,ZZ    ,
     4            R     ,S     ,T     )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
c#include      "implicit_f.inc"
      implicit none
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "constant.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LLT,CONV
      my_real
     .   R(MVSIZ),S(MVSIZ),T(MVSIZ),NI(MVSIZ,7) ,
     .   RR(MVSIZ),SS(MVSIZ),TT(MVSIZ),
     .   XX(MVSIZ,7) ,YY(MVSIZ,7) ,ZZ(MVSIZ,7) ,
     .   DRDX(MVSIZ),DRDY(MVSIZ),DRDZ(MVSIZ),
     .   DSDX(MVSIZ),DSDY(MVSIZ),DSDZ(MVSIZ),     
     .   DTDX(MVSIZ),DTDY(MVSIZ),DTDZ(MVSIZ)     
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   DX ,DY,DZ,DR ,DS,DT,ERR
C
      ERR=ZERO
C-----------------------------------------------
      DO I=1,LLT
C
        DX = XX(I,7)
     +     - NI(I, 1)*XX(I, 1) - NI(I, 2)*XX(I, 2) - NI(I, 3)*XX(I, 3)
     +     - NI(I, 4)*XX(I, 4) - NI(I, 5)*XX(I, 5) - NI(I, 6)*XX(I, 6)
        DY = YY(I,7)
     +     - NI(I, 1)*YY(I, 1) - NI(I, 2)*YY(I, 2) - NI(I, 3)*YY(I, 3)
     +     - NI(I, 4)*YY(I, 4) - NI(I, 5)*YY(I, 5) - NI(I, 6)*YY(I, 6)
        DZ = ZZ(I,7)
     +     - NI(I, 1)*ZZ(I, 1) - NI(I, 2)*ZZ(I, 2) - NI(I, 3)*ZZ(I, 3)
     +     - NI(I, 4)*ZZ(I, 4) - NI(I, 5)*ZZ(I, 5) - NI(I, 6)*ZZ(I, 6)
C
        DR = DRDX(I)*DX + DRDY(I)*DY + DRDZ(I)*DZ
        DS = DSDX(I)*DX + DSDY(I)*DY + DSDZ(I)*DZ 
        DT = DTDX(I)*DX + DTDY(I)*DY + DTDZ(I)*DZ
C
        RR(I) = RR(I) + DR
        SS(I) = SS(I) + DS
        TT(I) = TT(I) + DT
C
        R(I) = RR(I)
        S(I) = SS(I)
        T(I) = TT(I)
C
        IF(R(I)>=-ONE.AND.S(I)>=-ONE.AND.T(I)>=-ONE.AND.
     .     R(I)<= ONE.AND.S(I)<= ONE.AND.T(I)<= ONE)THEN
          ERR = MAX(ERR,ABS(DR),ABS(DS),ABS(DT))
        ELSE
          RR(I) = MAX(MIN(RR(I),ONE),-ONE)
          SS(I) = MAX(MIN(SS(I),ONE),-ONE)
          TT(I) = MAX(MIN(TT(I),ONE),-ONE)
        ENDIF
c
C
      ENDDO
C
      IF(ERR<EM4) CONV = 1
C-----------------------------------------------
      RETURN
      END
!||====================================================================
!||    i10deri   ../engine/source/interfaces/int16/i10lagm.F
!||--- called by ------------------------------------------------------
!||    i10rst    ../engine/source/interfaces/int16/i10lagm.F
!||====================================================================
      SUBROUTINE I10DERI(LLT,RR,SS ,TT    ,NI    ,
     2   DRDX  ,DRDY  ,DRDZ  ,DSDX  ,DSDY  ,DSDZ  ,
     3   DTDX  ,DTDY  ,DTDZ  ,DXDR  ,DYDR  ,DZDR  ,
     4   DXDT  ,DYDT  ,DZDT  ,XX    ,YY    ,ZZ    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LLT
      my_real
     .   DXDR(MVSIZ), DYDR(MVSIZ), DZDR(MVSIZ),
     .   DXDT(MVSIZ), DYDT(MVSIZ), DZDT(MVSIZ),
     .   DRDX(MVSIZ), DSDX(MVSIZ), DTDX(MVSIZ),
     .   DRDY(MVSIZ), DSDY(MVSIZ), DTDY(MVSIZ),
     .   DRDZ(MVSIZ), DSDZ(MVSIZ), DTDZ(MVSIZ),
     .   XX(MVSIZ,7) ,YY(MVSIZ,7),ZZ(MVSIZ,7),
     .   NI(MVSIZ,7) ,RR(MVSIZ) ,SS(MVSIZ) ,TT(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N
      my_real
     .   DXDS(MVSIZ), DYDS(MVSIZ), DZDS(MVSIZ),
     .   DNIDR(10),DNIDS(10),DNIDT(10),
     .   D, AA, BB, DET(MVSIZ),R9 ,R13 ,S9 ,S10 ,S11 ,S12 ,T10 ,T14
      my_real
     .  U_M_R,U_P_R,U_M_S,U_P_S,U_M_T,U_P_T,
     .  UMS_UMT,UMS_UPT,UPS_UMT,UPS_UPT,
     .  UMR_UMS,UMR_UPS,UPR_UMS,UPR_UPS,
     .  UMT_UMR,UMT_UPR,UPT_UMR,UPT_UPR,
     .  A,R05,S05,T05
C-----------------------------------------------
C/*
C
C
C*/
C-----------------------------------------------
C
C-----------------------------------------------
C                _ 
C               \
C    x(r,s,t) = /_ (xi * Ni(r,s,t))
C                _ 
C               \
C    y(r,s,t) = /_ (yi * Ni(r,s,t))
C                _ 
C               \
C    z(r,s,t) = /_ zi * Ni(r,s,t))
C        
C                _ 
C               \
C    dx/dr    = /_ (xi * dNi/dr)
C    ...        
C
C          [dx/dr dy/dr dz/dr]            
C    [J] = |dx/ds dy/ds dz/ds|                
C          [dx/dt dy/dt dz/dt]               
C                                     
C    |r|   |r|      -1  |xs-x|
C    {s} = {s} + [J]    {ys-y}
C    |t|   |t|          |zs-z|
C
C-----------------------------------------------------------------------
C     Ni; dNi/dr; dNi/ds; dNi/dt
C-----------------------------------------------------------------------
      DO I=1,LLT
        R05 = HALF*RR(I)
        S05 = HALF*SS(I)
        T05 = HALF*TT(I)
C
        U_M_R = HALF - R05
        U_P_R = HALF + R05
C
        U_M_S = HALF - S05
        U_P_S = HALF + S05
C
        U_M_T = HALF - T05
        U_P_T = HALF + T05
C
        UMS_UMT = U_M_S * U_M_T
        UMS_UPT = U_M_S * U_P_T
        UPS_UMT = U_P_S * U_M_T
        UPS_UPT = U_P_S * U_P_T
C
        UMR_UMS = U_M_R * U_M_S
        UMR_UPS = U_M_R * U_P_S
        UPR_UMS = U_P_R * U_M_S
        UPR_UPS = U_P_R * U_P_S
C
        UMT_UMR = U_M_T * U_M_R
        UMT_UPR = U_M_T * U_P_R
        UPT_UMR = U_P_T * U_M_R
        UPT_UPR = U_P_T * U_P_R
C
        A = -RR(I)-TT(I)-ONE
        NI(I,1) = U_M_R * UMS_UMT * A
        NI(I,2) = U_M_R * UMS_UPT * A
        NI(I,3) = U_P_R * UMS_UPT * A
        NI(I,4) = U_P_R * UMS_UMT * A
        NI(I,5) = U_M_R * UPS_UMT * A
        NI(I,6) = U_M_R * UPS_UPT * A
C
        A = -T05 - RR(I)
        DNIDR(1) = -UMS_UMT * A 
        DNIDR(5) = -UPS_UMT * A
        DNIDR(2) = -UMS_UPT * A
        DNIDR(6) = -UPS_UPT * A
        DNIDR(3) =  UMS_UPT * A
        DNIDR(4) =  UMS_UMT * A
C
        DNIDS(1) = -UMT_UMR * A
        DNIDS(5) =  UMT_UMR * A
        DNIDS(2) = -UPT_UMR * A
        DNIDS(6) =  UPT_UMR * A
        DNIDS(3) = -UPT_UPR * A
        DNIDS(4) = -UMT_UPR * A
C
        DNIDT(1) = -UMR_UMS * A
        DNIDT(5) = -UMR_UPS * A
        DNIDT(2) =  UMR_UMS * A
        DNIDT(6) =  UMR_UPS * A
        DNIDT(3) =  UPR_UMS * A
        DNIDT(4) = -UPR_UMS * A
C------------------------------------
        NI(I,7) = -1.
C-----------------------------------------------------------------------
C     dx/dr; dx/ds; dx/dt
C-----------------------------------------------------------------------
        DXDR(I) = DNIDR(1)*XX(I,1) + DNIDR(2)*XX(I,2) + DNIDR(3)*XX(I,3)
     +          + DNIDR(4)*XX(I,4) + DNIDR(5)*XX(I,5) + DNIDR(6)*XX(I,6)
C  
        DXDS(I) = DNIDS(1)*XX(I,1) + DNIDS(2)*XX(I,2) + DNIDS(3)*XX(I,3)
     +          + DNIDS(4)*XX(I,4) + DNIDS(5)*XX(I,5) + DNIDS(6)*XX(I,6)
C 
        DXDT(I) = DNIDT(1)*XX(I,1) + DNIDT(2)*XX(I,2) + DNIDT(3)*XX(I,3)
     +          + DNIDT(4)*XX(I,4) + DNIDT(5)*XX(I,5) + DNIDT(6)*XX(I,6)
C-----------------------------------------------------------------------
C     dy/dr; dy/ds; dy/dt
C-----------------------------------------------------------------------
        DYDR(I) = DNIDR(1)*YY(I,1) + DNIDR(2)*YY(I,2) + DNIDR(3)*YY(I,3)
     +          + DNIDR(4)*YY(I,4) + DNIDR(5)*YY(I,5) + DNIDR(6)*YY(I,6)
C  
        DYDS(I) = DNIDS(1)*YY(I,1) + DNIDS(2)*YY(I,2) + DNIDS(3)*YY(I,3)
     +          + DNIDS(4)*YY(I,4) + DNIDS(5)*YY(I,5) + DNIDS(6)*YY(I,6)
C 
        DYDT(I) = DNIDT(1)*YY(I,1) + DNIDT(2)*YY(I,2) + DNIDT(3)*YY(I,3)
     +          + DNIDT(4)*YY(I,4) + DNIDT(5)*YY(I,5) + DNIDT(6)*YY(I,6)
C-----------------------------------------------------------------------
C     dz/dr; dz/ds; dz/dt
C-----------------------------------------------------------------------
        DZDR(I) = DNIDR(1)*ZZ(I,1) + DNIDR(2)*ZZ(I,2) + DNIDR(3)*ZZ(I,3)
     +          + DNIDR(4)*ZZ(I,4) + DNIDR(5)*ZZ(I,5) + DNIDR(6)*ZZ(I,6)
C  
        DZDS(I) = DNIDS(1)*ZZ(I,1) + DNIDS(2)*ZZ(I,2) + DNIDS(3)*ZZ(I,3)
     +          + DNIDS(4)*ZZ(I,4) + DNIDS(5)*ZZ(I,5) + DNIDS(6)*ZZ(I,6)
C 
        DZDT(I) = DNIDT(1)*ZZ(I,1) + DNIDT(2)*ZZ(I,2) + DNIDT(3)*ZZ(I,3)
     +          + DNIDT(4)*ZZ(I,4) + DNIDT(5)*ZZ(I,5) + DNIDT(6)*ZZ(I,6)
C-----------------------------------------------------------------------
C          -1
C       [J]          Inversion du jacobien
C-----------------------------------------------------------------------
        DRDX(I)=DYDS(I)*DZDT(I)-DZDS(I)*DYDT(I)
        DRDY(I)=DZDS(I)*DXDT(I)-DXDS(I)*DZDT(I)
        DRDZ(I)=DXDS(I)*DYDT(I)-DYDS(I)*DXDT(I)
C
        DSDZ(I)=DXDT(I)*DYDR(I)-DYDT(I)*DXDR(I)
        DSDY(I)=DZDT(I)*DXDR(I)-DXDT(I)*DZDR(I)
        DSDX(I)=DYDT(I)*DZDR(I)-DZDT(I)*DYDR(I)
C
        DTDX(I)=DYDR(I)*DZDS(I)-DZDR(I)*DYDS(I)
        DTDY(I)=DZDR(I)*DXDS(I)-DXDR(I)*DZDS(I)
        DTDZ(I)=DXDR(I)*DYDS(I)-DYDR(I)*DXDS(I)
C
        DET(I) = DXDR(I) * DRDX(I)
     .         + DYDR(I) * DRDY(I)
     .         + DZDR(I) * DRDZ(I)
C
c
c
      ENDDO
C
      DO I=1,LLT
C-----------------------------------------------------------------------
C          -1            
C       [J]              Inversion du jacobien suite
C-----------------------------------------------------------------------
        D = ONE/DET(I)
        DRDX(I)=D*DRDX(I)
        DSDX(I)=D*DSDX(I)
        DTDX(I)=D*DTDX(I)
C
        DRDY(I)=D*DRDY(I)
        DSDY(I)=D*DSDY(I)
        DTDY(I)=D*DTDY(I)
C
        DRDZ(I)=D*DRDZ(I)
        DSDZ(I)=D*DSDZ(I)
        DTDZ(I)=D*DTDZ(I)
C
c
c      print *, "DRDX(I),DRDY(I),DRDZ(I)",DRDX(I),DRDY(I),DRDZ(I)
c      print *, "DSDX(I),DSDY(I),DSDZ(I)",DSDX(I),DSDY(I),DSDZ(I)
c      print *, "DTDX(I),DTDY(I),DTDZ(I)",DTDX(I),DTDY(I),DTDZ(I)
c
      ENDDO
C-----------------------------------------------------------------------
      RETURN
      END
