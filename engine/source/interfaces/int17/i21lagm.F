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
Chd|  I21LAGM                       source/interfaces/int17/i21lagm.F
Chd|-- called by -----------
Chd|        I17MAIN                       source/interfaces/int17/i17main.F
Chd|-- calls ---------------
Chd|        I21LLL                        source/interfaces/int17/i21lagm.F
Chd|====================================================================
      SUBROUTINE I21LAGM(X    ,V     ,LLL     ,JLL   ,SLL   ,
     2                  XLL   ,CANDN ,CANDE   ,I_STOK,IXS   ,
     3                  IXS20 ,IADLL ,EMINX   ,NSV   ,NELEM ,
     4                  NC    ,N_MUL_MX,ITASK ,A     ,ITIED ,
     5                  NINT  ,NKMAX ,EMINXS  ,COMNTAG)
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
#include      "warn_c.inc"
#include      "scr05_c.inc"
#include      "scr07_c.inc"
#include      "scr18_c.inc"
#include      "units_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "couple_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC,I_STOK,N_MUL_MX,ITASK,ITIED,NINT,NKMAX ,
     .  LLL(*),JLL(*),SLL(*),CANDN(*),CANDE(*),COMNTAG(*),
     .  IXS(NIXS,*),IXS20(12,*),IADLL(*),NSV(*)  ,NELEM(*) 
C     REAL
      my_real
     .  X(3,*),V(3,*),XLL(*),
     .  EMINX(6,*),EMINXS(6,*),A(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,IK,IE,IS,IC,NK,III(MVSIZ,21),LLT,NFT,LE,FIRST,LAST,
     .        I20
      my_real
     .   XX(MVSIZ,21),YY(MVSIZ,21),ZZ(MVSIZ,21),
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
C        LLL(NC*(21,63))  : I  = LLL(IAD+1,2...IADNEXT-1)
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
       I20 = IE - NUMELS8 - NUMELS10
C-----------------------------------------------------------------------
C      test si brick 20  
C-----------------------------------------------------------------------
       IF(I20.GE .1.AND.I20.LE .NUMELS20)THEN
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
        IF(DIST.LE .0.)THEN
c
c      print *, "dans la boite",XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX
c
          LLT = LLT+1
          III(LLT,21)=IS
          XX(LLT,21)=X(1,IS)
          YY(LLT,21)=X(2,IS)
          ZZ(LLT,21)=X(3,IS)
          DO K=1,8
            III(LLT,K)=IXS(K+1,IE)
          ENDDO
          DO K=1,12
            III(LLT,K+8)=IXS20(K,I20)
          ENDDO
          DO K=1,20
            I = III(LLT,K)
            XX(LLT,K)=X(1,I)
            YY(LLT,K)=X(2,I)
            ZZ(LLT,K)=X(3,I)
          ENDDO
C-----------------------------------------------------------------------
C     calcul de  [L] par paquet de mvsiz   
C-----------------------------------------------------------------------
          IF(LLT==MVSIZ-1)THEN
            CALL I21LLL(
     1       LLT ,LLL       ,JLL       ,SLL       ,XLL     ,V       ,
     2       XX  ,YY        ,ZZ        ,III       ,NC      ,IADLL   ,
     3       N_MUL_MX ,A    ,X         ,ITIED     ,NINT    ,NKMAX   ,
     4       COMNTAG)
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
      IF(LLT/=0) CALL I21LLL(
     1       LLT ,LLL       ,JLL       ,SLL       ,XLL     ,V       ,
     2       XX  ,YY        ,ZZ        ,III       ,NC      ,IADLL   ,
     3       N_MUL_MX ,A    ,X         ,ITIED     ,NINT    ,NKMAX   ,
     4       COMNTAG)
C
C-----------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  I21LLL                        source/interfaces/int17/i21lagm.F
Chd|-- called by -----------
Chd|        I21LAGM                       source/interfaces/int17/i21lagm.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        I20RST                        source/interfaces/int16/i20lagm.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I21LLL(LLT  ,LLL       ,JLL  ,SLL  ,XLL  ,V     ,
     2                  XX   ,YY        ,ZZ   ,III  ,NC   ,IADLL ,
     3                  N_MUL_MX,A      ,X    ,ITIED,NINT ,NKMAX ,
     4                  COMNTAG)
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
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LLT,NC,N_MUL_MX,ITIED,NINT ,NKMAX   
      INTEGER LLL(*),JLL(*),SLL(*),COMNTAG(*),
     .        III(MVSIZ,21),IADLL(*)
C     REAL
      my_real
     .  XLL(*),V(3,*),A(3,*)
      my_real
     .   XX(MVSIZ,21),YY(MVSIZ,21),ZZ(MVSIZ,21),X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IK,NK,I1,I2,I3,I4,IAD,NN
      my_real
     .   VX,VY,VZ,VN,AA
      my_real
     .   R(MVSIZ),S(MVSIZ),T(MVSIZ),
     .   NSX(MVSIZ), NSY(MVSIZ), NSZ(MVSIZ),
     .   NX(MVSIZ), NY(MVSIZ), NZ(MVSIZ),
     .   NI(MVSIZ,21) 
C-----------------------------------------------
C      calcul de r,s,t
C-----------------------------------------------
c
c      print *, "XX(1,1),XX(1,9)",XX(1,1),XX(1,9)
c
      CALL I20RST(LLT   ,R     ,S     ,T     ,NI    ,
     2            NSX   ,NSY   ,NSZ   ,NX    ,NY    ,NZ    ,
     3            XX    ,YY    ,ZZ    )
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
          NK = 21
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
          VN = NSX(I)*VX + NSY(I)*VY + NSZ(I)*VZ
C-----------------------------------------------
C         test si vitesse entrante en s
C-----------------------------------------------
          IF(S(I)*VN<=ZERO)THEN
c
c      print *, "vitesse entrante",vn
      print *, "s = ",S(I)
c
c           AA = DT12/SQRT(NX(I)*NX(I)+NY(I)*NY(I)+NZ(I)*NZ(I))
           AA = ONE/SQRT(NSX(I)*NSX(I)+NSY(I)*NSY(I)+NSZ(I)*NSZ(I))
           NSX(I) = NSX(I)*AA
           NSY(I) = NSY(I)*AA
           NSZ(I) = NSZ(I)*AA
#include "lockon.inc"
            NC=NC+1
            IF(NC>N_MUL_MX)THEN
#include "lockoff.inc"
              CALL ANCMSG(MSGID=84,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
            IADLL(NC+1)=IADLL(NC) + 63
            IF(IADLL(NC+1)-1>NKMAX)THEN
#include "lockoff.inc"
              CALL ANCMSG(MSGID=84,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
            IAD = IADLL(NC) - 1
            DO IK=1,21
                LLL(IAD+IK)    = III(I,IK)
                JLL(IAD+IK)    = 1
                SLL(IAD+IK)    = 0
                XLL(IAD+IK)    = NSX(I)*NI(I,IK)
                LLL(IAD+IK+21) = III(I,IK)
                JLL(IAD+IK+21) = 2
                SLL(IAD+IK+21) = 0
                XLL(IAD+IK+21) = NSY(I)*NI(I,IK)
                LLL(IAD+IK+42) = III(I,IK)
                JLL(IAD+IK+42) = 3
                SLL(IAD+IK+42) = 0
                XLL(IAD+IK+42) = NSZ(I)*NI(I,IK)
                NN = LLL(IAD+IK)
                COMNTAG(NN) = COMNTAG(NN) + 1
            ENDDO
            SLL(IAD+21) = NINT
            SLL(IAD+42) = NINT
            SLL(IAD+63) = NINT
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
          NK = 21
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
C         test si vitesse entrante en r,s ou t
C-----------------------------------------------
          IF(VN<=ZERO)THEN
c
c      print *, "vitesse entrante",vn
      print *, "s = ",S(I)
c
#include "lockon.inc"
            IF(NC+3>N_MUL_MX)THEN
#include "lockoff.inc"
              CALL ANCMSG(MSGID=84,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
            IF(IADLL(NC+1)-1+21*3>NKMAX)THEN
#include "lockoff.inc"
              CALL ANCMSG(MSGID=84,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
C
            NC=NC+1
            IADLL(NC+1)=IADLL(NC) + 21
            IAD = IADLL(NC) - 1
            DO IK=1,21
                LLL(IAD+IK) = III(I,IK)
                JLL(IAD+IK) = 1
                SLL(IAD+IK) = 0
                XLL(IAD+IK) = NI(I,IK)
                NN = LLL(IAD+IK)
                COMNTAG(NN) = COMNTAG(NN) + 1
            ENDDO
            SLL(IAD+21) = NINT
C
            NC=NC+1
            IADLL(NC+1)=IADLL(NC) + 21
            IAD = IADLL(NC) - 1
            DO IK=1,21
                LLL(IAD+IK) = III(I,IK)
                JLL(IAD+IK) = 2
                SLL(IAD+IK) = 0
                XLL(IAD+IK) = NI(I,IK)
                NN = LLL(IAD+IK)
                COMNTAG(NN) = COMNTAG(NN) + 1
            ENDDO
            SLL(IAD+21) = NINT
C
            NC=NC+1
            IADLL(NC+1)=IADLL(NC) + 21
            IAD = IADLL(NC) - 1
            DO IK=1,21
                LLL(IAD+IK) = III(I,IK)
                JLL(IAD+IK) = 3
                SLL(IAD+IK) = 0
                XLL(IAD+IK) = NI(I,IK)
                NN = LLL(IAD+IK)
                COMNTAG(NN) = COMNTAG(NN) + 1
            ENDDO
            SLL(IAD+21) = NINT
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
           NK = 21
C-----------------------------------------------
      print *, "s = ",S(I)
c
#include "lockon.inc"
            IF(NC+3>N_MUL_MX)THEN
#include "lockoff.inc"
              CALL ANCMSG(MSGID=84,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
            IF(IADLL(NC+1)-1+21*3>NKMAX)THEN
#include "lockoff.inc"
              CALL ANCMSG(MSGID=84,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
            NC=NC+1
            IADLL(NC+1)=IADLL(NC) + 21
            IAD = IADLL(NC) - 1
            DO IK=1,21
                LLL(IAD+IK) = III(I,IK)
                JLL(IAD+IK) = 1
                SLL(IAD+IK) = 0
                XLL(IAD+IK) = NI(I,IK)
                NN = LLL(IAD+IK)
                COMNTAG(NN) = COMNTAG(NN) + 1
            ENDDO
            SLL(IAD+21) = NINT
C
            NC=NC+1
            IADLL(NC+1)=IADLL(NC) + 21
            IAD = IADLL(NC) - 1
            DO IK=1,21
                LLL(IAD+IK) = III(I,IK)
                JLL(IAD+IK) = 2
                SLL(IAD+IK) = 0
                XLL(IAD+IK) = NI(I,IK)
                NN = LLL(IAD+IK)
                COMNTAG(NN) = COMNTAG(NN) + 1
            ENDDO
            SLL(IAD+21) = NINT
C
            NC=NC+1
            IADLL(NC+1)=IADLL(NC) + 21
            IAD = IADLL(NC) - 1
            DO IK=1,21
                LLL(IAD+IK) = III(I,IK)
                JLL(IAD+IK) = 3
                SLL(IAD+IK) = 0
                XLL(IAD+IK) = NI(I,IK)
                NN = LLL(IAD+IK)
                COMNTAG(NN) = COMNTAG(NN) + 1
            ENDDO
            SLL(IAD+21) = NINT
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
