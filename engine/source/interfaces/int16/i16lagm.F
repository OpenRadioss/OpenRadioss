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
Chd|  I16LAGM                       source/interfaces/int16/i16lagm.F
Chd|-- called by -----------
Chd|        I16MAIN                       source/interfaces/int16/i16main.F
Chd|-- calls ---------------
Chd|        I16LLL                        source/interfaces/int16/i16lagm.F
Chd|====================================================================
      SUBROUTINE I16LAGM(X    ,V     ,LLL     ,JLL   ,SLL   ,
     2                  XLL   ,CANDN ,CANDE   ,I_STOK,IXS   ,
     3                  IXS16 ,IADLL ,EMINX   ,NSV   ,NELEM ,
     4                  N_MUL_MX,ITASK ,A     ,ITIED ,
     5                  NINT  ,NKMAX  ,COMNTAG)
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
     .  IXS(NIXS,*),IXS16(8,*),IADLL(*),NSV(*)  ,NELEM(*) 
C     REAL
      my_real
     .  X(3,*),V(3,*),XLL(*),
     .  EMINX(6,*),A(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,IK,IE,IS,IC,NK,III(MVSIZ,17),LLT,NFT,LE,FIRST,LAST,
     .        I16
      my_real
     .   XX(MVSIZ,17),YY(MVSIZ,17),ZZ(MVSIZ,17),
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
C        LLL(NC*(17,51))  : I  = LLL(IAD+1,2...IADNEXT-1)
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
       I16 = IE - NUMELS8 - NUMELS10 - NUMELS20
C-----------------------------------------------------------------------
C      test si shell 16  
C-----------------------------------------------------------------------
       IF(I16.GE .1.AND.I16.LE .NUMELS16)THEN
        IS = NSV(CANDN(IC))
        DIST = -EP30
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
          III(LLT,17)=IS
c          XX(LLT,17)=X(1,IS)+DT2*(V(1,IS)+DT12*A(1,IS))
c          YY(LLT,17)=X(2,IS)+DT2*(V(2,IS)+DT12*A(2,IS))
c          ZZ(LLT,17)=X(3,IS)+DT2*(V(3,IS)+DT12*A(3,IS))
          XX(LLT,17)=X(1,IS)
          YY(LLT,17)=X(2,IS)
          ZZ(LLT,17)=X(3,IS)
          DO K=1,8
            III(LLT,K)=IXS(K+1,IE)
            III(LLT,K+8)=IXS16(K,I16)
          ENDDO
          DO K=1,16
            I = III(LLT,K)
c            XX(LLT,K)=X(1,I)+DT2*(V(1,I)+DT12*A(1,I))
c            YY(LLT,K)=X(2,I)+DT2*(V(2,I)+DT12*A(2,I))
c            ZZ(LLT,K)=X(3,I)+DT2*(V(3,I)+DT12*A(3,I))
            XX(LLT,K)=X(1,I)
            YY(LLT,K)=X(2,I)
            ZZ(LLT,K)=X(3,I)
          ENDDO
c
c      print *, "XX(1,1),XX(1,9)",XX(1,1),XX(1,9)
c
C-----------------------------------------------------------------------
C     calcul de  [L] par paquet de mvsiz   
C-----------------------------------------------------------------------
          IF(LLT==MVSIZ-1)THEN
            CALL I16LLL(
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
      IF(LLT/=0) CALL I16LLL(
     1       LLT ,LLL       ,JLL       ,SLL       ,XLL     ,V       ,
     2       XX  ,YY        ,ZZ        ,III       ,IADLL   ,
     3       N_MUL_MX ,A    ,X         ,ITIED     ,NINT    ,NKMAX   ,
     4       COMNTAG )
C
C-----------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  I16LLL                        source/interfaces/int16/i16lagm.F
Chd|-- called by -----------
Chd|        I16LAGM                       source/interfaces/int16/i16lagm.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        I16RST                        source/interfaces/int16/i16lagm.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I16LLL(LLT  ,LLL       ,JLL  ,SLL  ,XLL  ,V     ,
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
     .        III(MVSIZ,17),IADLL(*)
C     REAL
      my_real
     .  XLL(*),V(3,*),A(3,*)
      my_real
     .   XX(MVSIZ,17),YY(MVSIZ,17),ZZ(MVSIZ,17),X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IK,NK,I1,I2,I3,I4,IAD,NN
      my_real
     .   VX,VY,VZ,VN,AA
      my_real
     .   R(MVSIZ),S(MVSIZ),T(MVSIZ),
     .   NX(MVSIZ), NY(MVSIZ), NZ(MVSIZ),
     .   NI(MVSIZ,17) 
C-----------------------------------------------
C      calcul de r,s,t
C-----------------------------------------------
c
c      print *, "XX(1,1),XX(1,9)",XX(1,1),XX(1,9)
c
      CALL I16RST(LLT   ,R     ,S     ,T     ,NI    ,
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
          NK = 17
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
c           AA = DT12/SQRT(NX(I)*NX(I)+NY(I)*NY(I)+NZ(I)*NZ(I))
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
            IADLL(N_MULT+1)=IADLL(N_MULT) + 51
            IF(IADLL(N_MULT+1)-1>NKMAX)THEN
#include "lockoff.inc"
              CALL ANCMSG(MSGID=84,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
            IAD = IADLL(N_MULT) - 1
            DO IK=1,17
                LLL(IAD+IK)    = III(I,IK)
                JLL(IAD+IK)    = 1
                SLL(IAD+IK)    = 0
                XLL(IAD+IK)    = NX(I)*NI(I,IK)
                LLL(IAD+IK+17) = III(I,IK)
                JLL(IAD+IK+17) = 2
                SLL(IAD+IK+17) = 0
                XLL(IAD+IK+17) = NY(I)*NI(I,IK)
                LLL(IAD+IK+34) = III(I,IK)
                JLL(IAD+IK+34) = 3
                SLL(IAD+IK+34) = 0
                XLL(IAD+IK+34) = NZ(I)*NI(I,IK)
                NN = LLL(IAD+IK)
                COMNTAG(NN) = COMNTAG(NN) + 1
            ENDDO
            SLL(IAD+17) = NINT
            SLL(IAD+34) = NINT
            SLL(IAD+51) = NINT
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
          NK = 17
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
            IF(IADLL(N_MULT+1)-1+17*3>NKMAX)THEN
#include "lockoff.inc"
              CALL ANCMSG(MSGID=84,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
C
            N_MULT=N_MULT+1
            IADLL(N_MULT+1)=IADLL(N_MULT) + 17
            IAD = IADLL(N_MULT) - 1
            DO IK=1,17
                LLL(IAD+IK) = III(I,IK)
                JLL(IAD+IK) = 1
                SLL(IAD+IK) = 0
                XLL(IAD+IK) = NI(I,IK)
                NN = LLL(IAD+IK)
                COMNTAG(NN) = COMNTAG(NN) + 1
            ENDDO
            SLL(IAD+17) = NINT
C
            N_MULT=N_MULT+1
            IADLL(N_MULT+1)=IADLL(N_MULT) + 17
            IAD = IADLL(N_MULT) - 1
            DO IK=1,17
                LLL(IAD+IK) = III(I,IK)
                JLL(IAD+IK) = 2
                SLL(IAD+IK) = 0
                XLL(IAD+IK) = NI(I,IK)
                NN = LLL(IAD+IK)
                COMNTAG(NN) = COMNTAG(NN) + 1
            ENDDO
            SLL(IAD+17) = NINT
C
            N_MULT=N_MULT+1
            IADLL(N_MULT+1)=IADLL(N_MULT) + 17
            IAD = IADLL(N_MULT) - 1
            DO IK=1,17
                LLL(IAD+IK) = III(I,IK)
                JLL(IAD+IK) = 3
                SLL(IAD+IK) = 0
                XLL(IAD+IK) = NI(I,IK)
                NN = LLL(IAD+IK)
                COMNTAG(NN) = COMNTAG(NN) + 1
            ENDDO
            SLL(IAD+17) = NINT
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
           NK = 17
C-----------------------------------------------
c      print *, "s = ",S(I)
c
#include "lockon.inc"
            IF(N_MULT+3>N_MUL_MX)THEN
#include "lockoff.inc"
              CALL ANCMSG(MSGID=84,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
            IF(IADLL(N_MULT+1)-1+17*3>NKMAX)THEN
#include "lockoff.inc"
              CALL ANCMSG(MSGID=84,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
            N_MULT=N_MULT+1
            IADLL(N_MULT+1)=IADLL(N_MULT) + 17
            IAD = IADLL(N_MULT) - 1
            DO IK=1,17
                LLL(IAD+IK) = III(I,IK)
                JLL(IAD+IK) = 1
                SLL(IAD+IK) = 0
                XLL(IAD+IK) = NI(I,IK)
                NN = LLL(IAD+IK)
                COMNTAG(NN) = COMNTAG(NN) + 1
            ENDDO
            SLL(IAD+17) = NINT
C
            N_MULT=N_MULT+1
            IADLL(N_MULT+1)=IADLL(N_MULT) + 17
            IAD = IADLL(N_MULT) - 1
            DO IK=1,17
                LLL(IAD+IK) = III(I,IK)
                JLL(IAD+IK) = 2
                SLL(IAD+IK) = 0
                XLL(IAD+IK) = NI(I,IK)
                NN = LLL(IAD+IK)
                COMNTAG(NN) = COMNTAG(NN) + 1
            ENDDO
            SLL(IAD+17) = NINT
C
            N_MULT=N_MULT+1
            IADLL(N_MULT+1)=IADLL(N_MULT) + 17
            IAD = IADLL(N_MULT) - 1
            DO IK=1,17
                LLL(IAD+IK) = III(I,IK)
                JLL(IAD+IK) = 3
                SLL(IAD+IK) = 0
                XLL(IAD+IK) = NI(I,IK)
                NN = LLL(IAD+IK)
                COMNTAG(NN) = COMNTAG(NN) + 1
            ENDDO
            SLL(IAD+17) = NINT
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
Chd|====================================================================
Chd|  I16RST                        source/interfaces/int16/i16lagm.F
Chd|-- called by -----------
Chd|        I16LLL                        source/interfaces/int16/i16lagm.F
Chd|-- calls ---------------
Chd|        I16DERI                       source/interfaces/int16/i16lagm.F
Chd|        I16NI                         source/interfaces/int16/i16lagm.F
Chd|        I16RSTN                       source/interfaces/int16/i16lagm.F
Chd|====================================================================
      SUBROUTINE I16RST(LLT,R  ,S     ,T     ,NI    ,
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
     .   XX(MVSIZ,17),YY(MVSIZ,17),ZZ(MVSIZ,17)
      my_real
     .   R(MVSIZ),S(MVSIZ),T(MVSIZ),NI(MVSIZ,17) ,
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
           CALL I16DERI(LLT,RR ,SS    ,TT    ,NI    ,
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
             CALL I16RSTN(LLT,RR,SS   ,TT    ,NI    ,CONV  ,
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
             CALL I16NI(LLT,RR ,SS ,TT ,NI  )
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
C
Chd|====================================================================
Chd|  I16NI                         source/interfaces/int16/i16lagm.F
Chd|-- called by -----------
Chd|        I16RST                        source/interfaces/int16/i16lagm.F
Chd|        I17RST                        source/interfaces/int17/i17lagm.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I16NI(LLT,RR ,SS ,TT ,NI    )
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
     .   RR(MVSIZ),SS(MVSIZ),TT(MVSIZ),NI(MVSIZ,17)  
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
        A = -RR(I)-TT(I) - ONE
        NI(I,1) = U_M_R * UMS_UMT * A
        NI(I,5) = U_M_R * UPS_UMT * A
        A = -RR(I)+TT(I) - ONE
        NI(I,2) = U_M_R * UMS_UPT * A
        NI(I,6) = U_M_R * UPS_UPT * A
        A = RR(I)+TT(I) - ONE
        NI(I,3) = U_P_R * UMS_UPT * A
        NI(I,7) = U_P_R * UPS_UPT * A
        A = RR(I)-TT(I) - ONE
        NI(I,4) = U_P_R * UMS_UMT * A
        NI(I,8) = U_P_R * UPS_UMT * A
C------------------------------------
        A = (ONE - RR(I)*RR(I))
        NI(I,10) = A * UMS_UPT
        NI(I,12) = A * UMS_UMT
        NI(I,14) = A * UPS_UPT
        NI(I,16) = A * UPS_UMT
C------------------------------------
        A = (ONE - TT(I)*TT(I))
        NI(I,9)  = A * UMR_UMS
        NI(I,11) = A * UPR_UMS
        NI(I,13) = A * UMR_UPS
        NI(I,15) = A * UPR_UPS
C------------------------------------
        NI(I,17) = -ONE
C------------------------------------
      ENDDO
C-----------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  I16RSTN                       source/interfaces/int16/i16lagm.F
Chd|-- called by -----------
Chd|        I16RST                        source/interfaces/int16/i16lagm.F
Chd|        I17RST                        source/interfaces/int17/i17lagm.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I16RSTN(LLT,RR,SS    ,TT    ,NI    ,CONV  ,
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
     .   R(MVSIZ),S(MVSIZ),T(MVSIZ),NI(MVSIZ,17) ,
     .   RR(MVSIZ),SS(MVSIZ),TT(MVSIZ),
     .   XX(MVSIZ,17) ,YY(MVSIZ,17) ,ZZ(MVSIZ,17) ,
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
        DX = XX(I,17)
     +     - NI(I, 1)*XX(I, 1) - NI(I, 2)*XX(I, 2) - NI(I, 3)*XX(I, 3)
     +     - NI(I, 4)*XX(I, 4) - NI(I, 5)*XX(I, 5) - NI(I, 6)*XX(I, 6)
     +     - NI(I, 7)*XX(I, 7) - NI(I, 8)*XX(I, 8) - NI(I, 9)*XX(I, 9)
     +     - NI(I,10)*XX(I,10) - NI(I,11)*XX(I,11) - NI(I,12)*XX(I,12)
     +     - NI(I,13)*XX(I,13) - NI(I,14)*XX(I,14) - NI(I,15)*XX(I,15)
     +     - NI(I,16)*XX(I,16)
        DY = YY(I,17)
     +     - NI(I, 1)*YY(I, 1) - NI(I, 2)*YY(I, 2) - NI(I, 3)*YY(I, 3)
     +     - NI(I, 4)*YY(I, 4) - NI(I, 5)*YY(I, 5) - NI(I, 6)*YY(I, 6)
     +     - NI(I, 7)*YY(I, 7) - NI(I, 8)*YY(I, 8) - NI(I, 9)*YY(I, 9)
     +     - NI(I,10)*YY(I,10) - NI(I,11)*YY(I,11) - NI(I,12)*YY(I,12)
     +     - NI(I,13)*YY(I,13) - NI(I,14)*YY(I,14) - NI(I,15)*YY(I,15)
     +     - NI(I,16)*YY(I,16)
        DZ = ZZ(I,17)
     +     - NI(I, 1)*ZZ(I, 1) - NI(I, 2)*ZZ(I, 2) - NI(I, 3)*ZZ(I, 3)
     +     - NI(I, 4)*ZZ(I, 4) - NI(I, 5)*ZZ(I, 5) - NI(I, 6)*ZZ(I, 6)
     +     - NI(I, 7)*ZZ(I, 7) - NI(I, 8)*ZZ(I, 8) - NI(I, 9)*ZZ(I, 9)
     +     - NI(I,10)*ZZ(I,10) - NI(I,11)*ZZ(I,11) - NI(I,12)*ZZ(I,12)
     +     - NI(I,13)*ZZ(I,13) - NI(I,14)*ZZ(I,14) - NI(I,15)*ZZ(I,15)
     +     - NI(I,16)*ZZ(I,16)
C
        DR = DRDX(I)*DX + DRDY(I)*DY + DRDZ(I)*DZ
        DS = DSDX(I)*DX + DSDY(I)*DY + DSDZ(I)*DZ 
        DT = DTDX(I)*DX + DTDY(I)*DY + DTDZ(I)*DZ
C
c
c      print *, "DRDX(I),DRDY(I),DRDZ(I)",DRDX(I),DRDY(I),DRDZ(I)
c      print *, "DSDX(I),DSDY(I),DSDZ(I)",DSDX(I),DSDY(I),DSDZ(I)
c      print *, "DTDX(I),DTDY(I),DTDZ(I)",DTDX(I),DTDY(I),DTDZ(I)
c      print *, "Ni",ni(1,1),ni(1,2),ni(1,3),ni(1,4),ni(1,5),ni(1,9)
c      print *, "dx,dy,dz",dx ,dy ,dz
c
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
c      print *, "3r,s,t",r(1),s(1),t(1)
c      print *, "3rr,ss,tt",rr(1),ss(1),tt(1)
c      print *, "dr,ds,dt",dr ,ds ,dt 
c      print *, "r,s,t",r(1),s(1),t(1)
c      print *, "ERR",ERR
c
C
      ENDDO
C
      IF(ERR<EM4) CONV = 1
C-----------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  I16DERI                       source/interfaces/int16/i16lagm.F
Chd|-- called by -----------
Chd|        I16RST                        source/interfaces/int16/i16lagm.F
Chd|        I17RST                        source/interfaces/int17/i17lagm.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I16DERI(LLT,RR,SS ,TT    ,NI    ,
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
C     REAL
      my_real
     .   DXDR(MVSIZ), DYDR(MVSIZ), DZDR(MVSIZ),
     .   DXDT(MVSIZ), DYDT(MVSIZ), DZDT(MVSIZ),
     .   DRDX(MVSIZ), DSDX(MVSIZ), DTDX(MVSIZ),
     .   DRDY(MVSIZ), DSDY(MVSIZ), DTDY(MVSIZ),
     .   DRDZ(MVSIZ), DSDZ(MVSIZ), DTDZ(MVSIZ),
     .   XX(MVSIZ,17) ,YY(MVSIZ,17),ZZ(MVSIZ,17),
     .   NI(MVSIZ,17) ,RR(MVSIZ) ,SS(MVSIZ) ,TT(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N
      my_real
     .   DXDS(MVSIZ), DYDS(MVSIZ), DZDS(MVSIZ),
     .   DNIDR(16),DNIDS(16),DNIDT(16),
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
C
C                                       ^ S          _ T
C                                       |            /|
C                                       |           /
C                                       |          /
C                        ( 7)===========|===(14)==============( 6)
C                        //|            |        /            //|
C                       // |            |       /            //||
C                      //  |            |      /            // ||
C                     //   |    (Is)    |     +            //  ||
C                    //    |            |    /            //   ||
C                  (15)    |            +   /           (13)   ||
C                  //      |            |  /            //     ||
C                 //     ( 3)-----------|---(10)-------//-----( 2)
C                //       /             |/            //      //
C     R <-------//- -+ - - - - - - - - -# - - - - - -//- -+  //
C              //       /              /            //      //
C            ( 8)===============(16)==============( 5)     //
C             ||      /              /             ||     //
C             ||   (11)             /              ||   ( 9)    
C             ||    /              /               ||   //
C             ||   /              +                ||  //
C             ||  /                                || //
C             || /                                 ||//
C             ||/                                  ||/
C            ( 4)===============(12)==============( 1)    
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
        NI(I,5) = U_M_R * UPS_UMT * A
        A = -RR(I)+TT(I)-ONE
        NI(I,2) = U_M_R * UMS_UPT * A
        NI(I,6) = U_M_R * UPS_UPT * A
        A = RR(I)+TT(I)-ONE
        NI(I,3) = U_P_R * UMS_UPT * A
        NI(I,7) = U_P_R * UPS_UPT * A
        A = RR(I)-TT(I)-ONE
        NI(I,4) = U_P_R * UMS_UMT * A
        NI(I,8) = U_P_R * UPS_UMT * A
C
        A = -T05 - RR(I)
        DNIDR(1) = -UMS_UMT * A 
        DNIDR(5) = -UPS_UMT * A
        A =  T05 - RR(I)
        DNIDR(2) = -UMS_UPT * A
        DNIDR(6) = -UPS_UPT * A
        A =  T05 + RR(I)
        DNIDR(3) =  UMS_UPT * A
        DNIDR(7) =  UPS_UPT * A
        A = -T05 + RR(I)
        DNIDR(4) =  UMS_UMT * A
        DNIDR(8) =  UPS_UMT * A
C
        A = -R05 - U_P_T
        DNIDS(1) = -UMT_UMR * A
        DNIDS(5) =  UMT_UMR * A
        A = -R05 - U_M_T
        DNIDS(2) = -UPT_UMR * A
        DNIDS(6) =  UPT_UMR * A
        A =  R05 - U_M_T
        DNIDS(3) = -UPT_UPR * A
        DNIDS(7) =  UPT_UPR * A
        A =  R05 - U_P_T
        DNIDS(4) = -UMT_UPR * A
        DNIDS(8) =  UMT_UPR * A
C
        A = -R05 - TT(I)
        DNIDT(1) = -UMR_UMS * A
        DNIDT(5) = -UMR_UPS * A
        A = -R05 + TT(I)
        DNIDT(2) =  UMR_UMS * A
        DNIDT(6) =  UMR_UPS * A
        A = +R05 + TT(I)
        DNIDT(3) =  UPR_UMS * A
        DNIDT(7) =  UPR_UPS * A
        A = +R05 - TT(I)
        DNIDT(4) = -UPR_UMS * A
        DNIDT(8) = -UPR_UPS * A
C------------------------------------
        A = (ONE-RR(I)*RR(I))
        NI(I,10) = A * UMS_UPT
        NI(I,12) = A * UMS_UMT
        NI(I,14) = A * UPS_UPT
        NI(I,16) = A * UPS_UMT
C    
        A = HALF*A
        DNIDT(10) =  A * U_M_S
        DNIDT(14) =  A * U_P_S
        DNIDS(10) = -A * U_P_T
        DNIDS(12) = -A * U_M_T
C    
        A = -TWO*RR(I)
        DNIDR(10) = A * UMS_UPT
        DNIDR(12) = A * UMS_UMT
        DNIDR(14) = A * UPS_UPT
        DNIDR(16) = A * UPS_UMT
C------------------------------------
        A = (ONE-TT(I)*TT(I))
        NI(I,9)  = A * UMR_UMS
        NI(I,11) = A * UPR_UMS
        NI(I,13) = A * UMR_UPS
        NI(I,15) = A * UPR_UPS
C
        NI(I,17) = -ONE
C
        A = HALF*A
        DNIDR(9)  = -A * U_M_S
        DNIDR(13) = -A * U_P_S
C
        DNIDS(9)  = -A * U_M_R
        DNIDS(11) = -A * U_P_R
C
        A = -TWO*TT(I)
        DNIDT(9)  = A * UMR_UMS
        DNIDT(11) = A * UPR_UMS
        DNIDT(13) = A * UMR_UPS
        DNIDT(15) = A * UPR_UPS
c
c      print *, "DNIDr(1),DNIDr(9)",DNIDr(1),DNIDr(9)
c      print *, "DNIDs(1),DNIDs(9)",DNIDs(1),DNIDs(9)
c      print *, "DNIDT(1),DNIDT(9)",DNIDT(1),DNIDT(9)
c      print *, "XX(I,1),XX(I,9)",XX(I,1),XX(I,9)
c
C-----------------------------------------------------------------------
C     dx/dr; dx/ds; dx/dt
C-----------------------------------------------------------------------
        DXDR(I) = DNIDR(1)*XX(I,1) + DNIDR(2)*XX(I,2) + DNIDR(3)*XX(I,3)
     +          + DNIDR(4)*XX(I,4) + DNIDR(5)*XX(I,5) + DNIDR(6)*XX(I,6)
     +          + DNIDR(7)*XX(I,7) + DNIDR(8)*XX(I,8)  
     +    + DNIDR(9)*(XX(I,9) - XX(I,11)) + DNIDR(10)*XX(I,10)
     +    + DNIDR(12)*XX(I,12) + DNIDR(13)*(XX(I,13) - XX(I,15))
     +    + DNIDR(14)*XX(I,14) + DNIDR(16)*XX(I,16)
C  
        DXDS(I) = DNIDS(1)*XX(I,1) + DNIDS(2)*XX(I,2) + DNIDS(3)*XX(I,3)
     +          + DNIDS(4)*XX(I,4) + DNIDS(5)*XX(I,5) + DNIDS(6)*XX(I,6)
     +          + DNIDS(7)*XX(I,7) + DNIDS(8)*XX(I,8)  
     +    + DNIDS(9)* (XX(I,9)  - XX(I,13))
     +    + DNIDS(10)*(XX(I,10) - XX(I,14)) 
     +    + DNIDS(11)*(XX(I,11) - XX(I,15))
     +    + DNIDS(12)*(XX(I,12) - XX(I,16)) 
C 
        DXDT(I) = DNIDT(1)*XX(I,1) + DNIDT(2)*XX(I,2) + DNIDT(3)*XX(I,3)
     +          + DNIDT(4)*XX(I,4) + DNIDT(5)*XX(I,5) + DNIDT(6)*XX(I,6)
     +          + DNIDT(7)*XX(I,7) + DNIDT(8)*XX(I,8)  
     +    + DNIDT(9)*XX(I,9)   + DNIDT(10)*(XX(I,10) - XX(I,12))
     +    + DNIDT(11)*XX(I,11) + DNIDT(13)*XX(I,13)
     +    + DNIDT(14)*(XX(I,14) - XX(I,16)) + DNIDT(15)*XX(I,15) 
C-----------------------------------------------------------------------
C     dy/dr; dy/ds; dy/dt
C-----------------------------------------------------------------------
        DYDR(I) = DNIDR(1)*YY(I,1) + DNIDR(2)*YY(I,2) + DNIDR(3)*YY(I,3)
     +          + DNIDR(4)*YY(I,4) + DNIDR(5)*YY(I,5) + DNIDR(6)*YY(I,6)
     +          + DNIDR(7)*YY(I,7) + DNIDR(8)*YY(I,8)  
     +    + DNIDR(9)*(YY(I,9) - YY(I,11))   + DNIDR(10)*YY(I,10)
     +    + DNIDR(12)*YY(I,12) + DNIDR(13)*(YY(I,13) - YY(I,15))
     +    + DNIDR(14)*YY(I,14) + DNIDR(16)*YY(I,16)
C  
        DYDS(I) = DNIDS(1)*YY(I,1) + DNIDS(2)*YY(I,2) + DNIDS(3)*YY(I,3)
     +          + DNIDS(4)*YY(I,4) + DNIDS(5)*YY(I,5) + DNIDS(6)*YY(I,6)
     +          + DNIDS(7)*YY(I,7) + DNIDS(8)*YY(I,8)  
     +    + DNIDS(9)* (YY(I,9)  - YY(I,13))   
     +    + DNIDS(10)*(YY(I,10) - YY(I,14)) 
     +    + DNIDS(11)*(YY(I,11) - YY(I,15))
     +    + DNIDS(12)*(YY(I,12) - YY(I,16))
C 
        DYDT(I) = DNIDT(1)*YY(I,1) + DNIDT(2)*YY(I,2) + DNIDT(3)*YY(I,3)
     +          + DNIDT(4)*YY(I,4) + DNIDT(5)*YY(I,5) + DNIDT(6)*YY(I,6)
     +          + DNIDT(7)*YY(I,7) + DNIDT(8)*YY(I,8)  
     +    + DNIDT(9)*YY(I,9)   + DNIDT(10)*(YY(I,10) - YY(I,12))
     +    + DNIDT(11)*YY(I,11) + DNIDT(13)*YY(I,13)
     +    + DNIDT(14)*(YY(I,14) - YY(I,16)) + DNIDT(15)*YY(I,15) 
C-----------------------------------------------------------------------
C     dz/dr; dz/ds; dz/dt
C-----------------------------------------------------------------------
        DZDR(I) = DNIDR(1)*ZZ(I,1) + DNIDR(2)*ZZ(I,2) + DNIDR(3)*ZZ(I,3)
     +          + DNIDR(4)*ZZ(I,4) + DNIDR(5)*ZZ(I,5) + DNIDR(6)*ZZ(I,6)
     +          + DNIDR(7)*ZZ(I,7) + DNIDR(8)*ZZ(I,8)  
     +    + DNIDR(9)*(ZZ(I,9) - ZZ(I,11))   + DNIDR(10)*ZZ(I,10)
     +    + DNIDR(12)*ZZ(I,12) + DNIDR(13)*(ZZ(I,13) - ZZ(I,15))
     +    + DNIDR(14)*ZZ(I,14) + DNIDR(16)*ZZ(I,16)
C  
        DZDS(I) = DNIDS(1)*ZZ(I,1) + DNIDS(2)*ZZ(I,2) + DNIDS(3)*ZZ(I,3)
     +          + DNIDS(4)*ZZ(I,4) + DNIDS(5)*ZZ(I,5) + DNIDS(6)*ZZ(I,6)
     +          + DNIDS(7)*ZZ(I,7) + DNIDS(8)*ZZ(I,8)  
     +    + DNIDS(9)* (ZZ(I,9)  - ZZ(I,13))
     +    + DNIDS(10)*(ZZ(I,10) - ZZ(I,14))
     +    + DNIDS(11)*(ZZ(I,11) - ZZ(I,15))
     +    + DNIDS(12)*(ZZ(I,12) - ZZ(I,16)) 
C 
        DZDT(I) = DNIDT(1)*ZZ(I,1) + DNIDT(2)*ZZ(I,2) + DNIDT(3)*ZZ(I,3)
     +          + DNIDT(4)*ZZ(I,4) + DNIDT(5)*ZZ(I,5) + DNIDT(6)*ZZ(I,6)
     +          + DNIDT(7)*ZZ(I,7) + DNIDT(8)*ZZ(I,8)  
     +    + DNIDT(9)*ZZ(I,9)   + DNIDT(10)*(ZZ(I,10) - ZZ(I,12))
     +    + DNIDT(11)*ZZ(I,11) + DNIDT(13)*ZZ(I,13)
     +    + DNIDT(14)*(ZZ(I,14) - ZZ(I,16)) + DNIDT(15)*ZZ(I,15) 
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
c      print *, "Det",DET(I)
c      print *, "DXDR(I),DYDR(I),DZDR(I)",DXDR(I),DYDR(I),DZDR(I)
c      print *, "DXDs(I),DYDs(I),DZDs(I)",DXDs(I),DYDs(I),DZDs(I)
c      print *, "DXDt(I),DYDt(I),DZDt(I)",DXDt(I),DYDt(I),DZDt(I)
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
