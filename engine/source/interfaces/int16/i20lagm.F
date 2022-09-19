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
Chd|  I20LAGM                       source/interfaces/int16/i20lagm.F
Chd|-- called by -----------
Chd|        I16MAIN                       source/interfaces/int16/i16main.F
Chd|-- calls ---------------
Chd|        I20LLL                        source/interfaces/int16/i20lagm.F
Chd|====================================================================
      SUBROUTINE I20LAGM(X    ,V     ,LLL     ,JLL   ,SLL   ,
     2                  XLL   ,CANDN ,CANDE   ,I_STOK,IXS   ,
     3                  IXS20 ,IADLL ,EMINX   ,NSV   ,NELEM ,
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
      INTEGER I_STOK,N_MUL_MX,ITASK,ITIED,NINT,NKMAX ,
     .  LLL(*),JLL(*),SLL(*),CANDN(*),CANDE(*),COMNTAG(*),
     .  IXS(NIXS,*),IXS20(12,*),IADLL(*),NSV(*)  ,NELEM(*)
C     REAL
      my_real
     .  X(3,*),V(3,*),XLL(*),
     .  EMINX(6,*),A(3,*)
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
            IF(I/=0)THEN	  	
              XX(LLT,K)=X(1,I)
              YY(LLT,K)=X(2,I)
              ZZ(LLT,K)=X(3,I)
	    ELSE
	      IF(K==9)THEN
	        XX(LLT,K)=0.500*(X(1,III(LLT,1))+X(1,III(LLT,2)))
                YY(LLT,K)=0.500*(X(2,III(LLT,1))+X(2,III(LLT,2)))
                ZZ(LLT,K)=0.500*(X(3,III(LLT,1))+X(3,III(LLT,2)))
	      ENDIF
	      IF(K==10)THEN
	        XX(LLT,K)=0.500*(X(1,III(LLT,2))+X(1,III(LLT,3)))
                YY(LLT,K)=0.500*(X(2,III(LLT,2))+X(2,III(LLT,3)))
                ZZ(LLT,K)=0.500*(X(3,III(LLT,2))+X(3,III(LLT,3)))
	      ENDIF
	      IF(K==11)THEN
	        XX(LLT,K)=0.500*(X(1,III(LLT,3))+X(1,III(LLT,4)))
                YY(LLT,K)=0.500*(X(2,III(LLT,3))+X(2,III(LLT,4)))
                ZZ(LLT,K)=0.500*(X(3,III(LLT,3))+X(3,III(LLT,4)))
	      ENDIF
	      IF(K==12)THEN
	        XX(LLT,K)=0.500*(X(1,III(LLT,4))+X(1,III(LLT,1)))
                YY(LLT,K)=0.500*(X(2,III(LLT,4))+X(2,III(LLT,1)))
                ZZ(LLT,K)=0.500*(X(3,III(LLT,4))+X(3,III(LLT,1)))
	      ENDIF
	      IF(K==13)THEN
	        XX(LLT,K)=0.500*(X(1,III(LLT,1))+X(1,III(LLT,5)))
                YY(LLT,K)=0.500*(X(2,III(LLT,1))+X(2,III(LLT,5)))
                ZZ(LLT,K)=0.500*(X(3,III(LLT,1))+X(3,III(LLT,5)))
	      ENDIF
	      IF(K==14)THEN
	        XX(LLT,K)=0.500*(X(1,III(LLT,2))+X(1,III(LLT,6)))
                YY(LLT,K)=0.500*(X(2,III(LLT,2))+X(2,III(LLT,6)))
                ZZ(LLT,K)=0.500*(X(3,III(LLT,2))+X(3,III(LLT,6)))
	      ENDIF
	      IF(K==15)THEN
	        XX(LLT,K)=0.500*(X(1,III(LLT,3))+X(1,III(LLT,7)))
                YY(LLT,K)=0.500*(X(2,III(LLT,3))+X(2,III(LLT,7)))
                ZZ(LLT,K)=0.500*(X(3,III(LLT,3))+X(3,III(LLT,7)))
	      ENDIF
	      IF(K==16)THEN
	        XX(LLT,K)=0.500*(X(1,III(LLT,4))+X(1,III(LLT,8)))
                YY(LLT,K)=0.500*(X(2,III(LLT,4))+X(2,III(LLT,8)))
                ZZ(LLT,K)=0.500*(X(3,III(LLT,4))+X(3,III(LLT,8)))
	      ENDIF
	      IF(K==17)THEN
	        XX(LLT,K)=0.500*(X(1,III(LLT,5))+X(1,III(LLT,6)))
                YY(LLT,K)=0.500*(X(2,III(LLT,5))+X(2,III(LLT,6)))
                ZZ(LLT,K)=0.500*(X(3,III(LLT,5))+X(3,III(LLT,6)))
	      ENDIF
	      IF(K==18)THEN
	        XX(LLT,K)=0.500*(X(1,III(LLT,6))+X(1,III(LLT,7)))
                YY(LLT,K)=0.500*(X(2,III(LLT,6))+X(2,III(LLT,7)))
                ZZ(LLT,K)=0.500*(X(3,III(LLT,6))+X(3,III(LLT,7)))
	      ENDIF
	      IF(K==19)THEN
	        XX(LLT,K)=0.500*(X(1,III(LLT,7))+X(1,III(LLT,8)))
                YY(LLT,K)=0.500*(X(2,III(LLT,7))+X(2,III(LLT,8)))
                ZZ(LLT,K)=0.500*(X(3,III(LLT,7))+X(3,III(LLT,8)))
	      ENDIF
	      IF(K==20)THEN
	        XX(LLT,K)=0.500*(X(1,III(LLT,5))+X(1,III(LLT,8)))
                YY(LLT,K)=0.500*(X(2,III(LLT,5))+X(2,III(LLT,8)))
                ZZ(LLT,K)=0.500*(X(3,III(LLT,5))+X(3,III(LLT,8)))
	      ENDIF
	    ENDIF
          ENDDO
C-----------------------------------------------------------------------
C     calcul de  [L] par paquet de mvsiz   
C-----------------------------------------------------------------------
          IF(LLT==MVSIZ-1)THEN
            CALL I20LLL(
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
      IF(LLT/=0) CALL I20LLL(
     1       LLT ,LLL       ,JLL       ,SLL       ,XLL     ,V       ,
     2       XX  ,YY        ,ZZ        ,III       ,IADLL   ,
     3       N_MUL_MX ,A    ,X         ,ITIED     ,NINT    ,NKMAX   ,
     4       COMNTAG )
C
C-----------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  I20LLL                        source/interfaces/int16/i20lagm.F
Chd|-- called by -----------
Chd|        I20LAGM                       source/interfaces/int16/i20lagm.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        I20RST                        source/interfaces/int16/i20lagm.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I20LLL(LLT  ,LLL       ,JLL  ,SLL  ,XLL  ,V     ,
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
#include      "units_c.inc"
      COMMON /LAGGLOB/N_MULT
      INTEGER N_MULT
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LLT,N_MUL_MX,ITIED,NINT ,NKMAX
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
      INTEGER I,J,IK,NK,I1,I2,I3,I4,IAD,NN,J1,J2,IIK,
     .        IPERM1(20),IPERM2(20),COMPTIK
      my_real
     .   VX,VY,VZ,VN,AA
      my_real
     .   R(MVSIZ),S(MVSIZ),T(MVSIZ),
     .   NSX(MVSIZ), NSY(MVSIZ), NSZ(MVSIZ),
     .   NX(MVSIZ), NY(MVSIZ), NZ(MVSIZ),
     .   NI(MVSIZ,21)
      DATA IPERM1 /0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,
     .             5,6,7,8/
      DATA IPERM2 /0,0,0,0,0,0,0,0,2,3,4,1,1,2,3,4,
     .             6,7,8,5/	
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
          DO IK=1,8
            VX = VX - (V(1,III(I,IK))+DT12*A(1,III(I,IK)))*NI(I,IK)
            VY = VY - (V(2,III(I,IK))+DT12*A(2,III(I,IK)))*NI(I,IK)
            VZ = VZ - (V(3,III(I,IK))+DT12*A(3,III(I,IK)))*NI(I,IK)
          ENDDO
	  DO IK=9,21
	    IF(III(I,IK)/=0)THEN
	      VX = VX - (V(1,III(I,IK))+DT12*A(1,III(I,IK)))*NI(I,IK)
              VY = VY - (V(2,III(I,IK))+DT12*A(2,III(I,IK)))*NI(I,IK)
              VZ = VZ - (V(3,III(I,IK))+DT12*A(3,III(I,IK)))*NI(I,IK)
            ELSE
	       VX=VX
	       VY=VY
	       VZ=VZ
	    ENDIF
	   ENDDO
c
c      print *, "vx,vy,vz s-m",vx,vy,vz
c      print *, "nx,ny,nz ", NX(I),NY(I),NZ(I)
c
          VN = NSX(I)*VX + NSY(I)*VY + NSZ(I)*VZ
C-----------------------------------------------
C         test si vitesse entrante en s
C-----------------------------------------------
          IF(S(I)*VN<=0.0)THEN
c
c      print *, "vitesse entrante",vn
c
c           AA = DT12/SQRT(NX(I)*NX(I)+NY(I)*NY(I)+NZ(I)*NZ(I))
           AA = ONE/SQRT(NSX(I)*NSX(I)+NSY(I)*NSY(I)+NSZ(I)*NSZ(I))
           NSX(I) = NSX(I)*AA
           NSY(I) = NSY(I)*AA
           NSZ(I) = NSZ(I)*AA
#include "lockon.inc"
            N_MULT=N_MULT+1
            IF(N_MULT>N_MUL_MX)THEN
#include "lockoff.inc"
              CALL ANCMSG(MSGID=84,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
            IAD = IADLL(N_MULT) - 1
            IIK=0
	    COMPTIK=0
	    DO IK=1,21
              IF(III(I,IK)/=0)THEN	
	       COMPTIK=COMPTIK+1
	      ENDIF
	    ENDDO
            DO IK=1,21
	      IF(III(I,IK)/=0)THEN	
	        IIK=IIK+1
		LLL(IAD+IIK)    = III(I,IK)
                JLL(IAD+IIK)    = 1
                SLL(IAD+IIK)    = 0
                XLL(IAD+IIK)    = NSX(I)*NI(I,IK)
                LLL(IAD+COMPTIK+IIK) = III(I,IK)
                JLL(IAD+COMPTIK+IIK) = 2
                SLL(IAD+COMPTIK+IIK) = 0
                XLL(IAD+COMPTIK+IIK) = NSY(I)*NI(I,IK)
                LLL(IAD+(2*COMPTIK)+IIK) = III(I,IK)
                JLL(IAD+(2*COMPTIK)+IIK) = 3
                SLL(IAD+(2*COMPTIK)+IIK) = 0
                XLL(IAD+(2*COMPTIK)+IIK) = NSZ(I)*NI(I,IK)
		NN = LLL(IAD+IIK)
		COMNTAG(NN) = COMNTAG(NN) + 1
              ELSE
	        J1=IPERM1(IK)
	        J2=IPERM2(IK)
	        XLL(IAD+J1)=XLL(IAD+J1)+0.5*(NSX(I)*NI(I,IK))
		XLL(IAD+J2)=XLL(IAD+J2)+0.5*(NSX(I)*NI(I,IK))
		XLL(IAD+COMPTIK+J1)=XLL(IAD+COMPTIK+J1)+
     .		0.5*(NSY(I)*NI(I,IK))
		XLL(IAD+COMPTIK+J2)=XLL(IAD+COMPTIK+J2)+
     .		0.5*(NSY(I)*NI(I,IK))
		XLL(IAD+(2*COMPTIK)+J1)=XLL(IAD+(2*COMPTIK)+J1)+
     .		0.5*(NSZ(I)*NI(I,IK))
		XLL(IAD+(2*COMPTIK)+J2)=XLL(IAD+(2*COMPTIK)+J2)+
     .		0.5*(NSZ(I)*NI(I,IK))
	      ENDIF	  	
            ENDDO
            IADLL(N_MULT+1)=IADLL(N_MULT)+(3*COMPTIK)
	    IF(IADLL(N_MULT+1)-1>NKMAX)THEN
#include "lockoff.inc"
              CALL ANCMSG(MSGID=84,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
	    SLL(IAD+COMPTIK) = NINT
            SLL(IAD+(2*COMPTIK)) = NINT
            SLL(IAD+(3*COMPTIK)) = NINT
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
          DO IK=1,8
            VX = VX - (V(1,III(I,IK))+DT12*A(1,III(I,IK)))*NI(I,IK)
            VY = VY - (V(2,III(I,IK))+DT12*A(2,III(I,IK)))*NI(I,IK)
            VZ = VZ - (V(3,III(I,IK))+DT12*A(3,III(I,IK)))*NI(I,IK)
          ENDDO
	  DO IK=9,21
	    IF(III(I,IK)/=0)THEN
	      VX = VX - (V(1,III(I,IK))+DT12*A(1,III(I,IK)))*NI(I,IK)
              VY = VY - (V(2,III(I,IK))+DT12*A(2,III(I,IK)))*NI(I,IK)
              VZ = VZ - (V(3,III(I,IK))+DT12*A(3,III(I,IK)))*NI(I,IK)
            ELSE
	      VX=VX
	      VY=VY
	      VZ=VZ
	    ENDIF
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
c
#include "lockon.inc"
            IF(N_MULT+3>N_MUL_MX)THEN
#include "lockoff.inc"
              CALL ANCMSG(MSGID=84,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
            IF(IADLL(N_MULT+1)-1+21*3>NKMAX)THEN
#include "lockoff.inc"
              CALL ANCMSG(MSGID=84,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
C
            N_MULT=N_MULT+1
            IAD = IADLL(N_MULT) - 1
            IIK=0	
            DO IK=1,21
	      IF(III(I,IK)/=0)THEN
	         IIK=IIK+1
		 LLL(IAD+IIK) = III(I,IK)
                 JLL(IAD+IIK) = 1
                 SLL(IAD+IIK) = 0
                 XLL(IAD+IIK) = NI(I,IK)
		 NN = LLL(IAD+IIK)
                 COMNTAG(NN) = COMNTAG(NN) + 1
	      ELSE
	        J1=IPERM1(IK)
	        J2=IPERM2(IK)
	        XLL(IAD+J1)=XLL(IAD+J1)+0.5*NI(I,IK)
		XLL(IAD+J2)=XLL(IAD+J2)+0.5*NI(I,IK)
	      ENDIF
            ENDDO
            SLL(IAD+IIK) = NINT
	    IADLL(N_MULT+1)=IADLL(N_MULT) + IIK
C
            N_MULT=N_MULT+1
            IAD = IADLL(N_MULT) - 1
            IIK=0
            DO IK=1,21
	       IF(III(I,IK)/=0)THEN
	         IIK=IIK+1
		 LLL(IAD+IIK) = III(I,IK)
                 JLL(IAD+IIK) = 2
                 SLL(IAD+IIK) = 0
                 XLL(IAD+IIK) = NI(I,IK)
		 NN = LLL(IAD+IIK)
                 COMNTAG(NN) = COMNTAG(NN) + 1
               ELSE
	         J1=IPERM1(IK)
	         J2=IPERM2(IK)
	         XLL(IAD+J1)=XLL(IAD+J1)+0.5*NI(I,IK)
		 XLL(IAD+J2)=XLL(IAD+J2)+0.5*NI(I,IK)
	       ENDIF
            ENDDO
	    IADLL(N_MULT+1)=IADLL(N_MULT) + IIK
	    SLL(IAD+IIK) = NINT
C
            N_MULT=N_MULT+1
            IAD = IADLL(N_MULT) - 1
	    IIK=0	
            DO IK=1,21
	      IF(III(I,IK)/=0)THEN
	        IIK=IIK+1
                LLL(IAD+IIK) = III(I,IK)
                JLL(IAD+IIK) = 3
                SLL(IAD+IIK) = 0
                XLL(IAD+IIK) = NI(I,IK)
		NN = LLL(IAD+IIK)
                COMNTAG(NN) = COMNTAG(NN) + 1
	      ELSE
	      	J1=IPERM1(IK)
	        J2=IPERM2(IK)
	        XLL(IAD+J1)=XLL(IAD+J1)+0.5*NI(I,IK)
	        XLL(IAD+J2)=XLL(IAD+J2)+0.5*NI(I,IK)
	      ENDIF
            ENDDO
            IADLL(N_MULT+1)=IADLL(N_MULT) + IIK	
	    SLL(IAD+IIK) = NINT
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
c
#include "lockon.inc"
            IF(N_MULT+3>N_MUL_MX)THEN
#include "lockoff.inc"
              CALL ANCMSG(MSGID=84,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
            IF(IADLL(N_MULT+1)-1+21*3>NKMAX)THEN
#include "lockoff.inc"
              CALL ANCMSG(MSGID=84,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
            N_MULT=N_MULT+1
            IAD = IADLL(N_MULT) - 1
            IIK=0	
            DO IK=1,21
	     IF(III(I,IK)/=0)THEN
	         IIK=IIK+1
		 LLL(IAD+IIK) = III(I,IK)
                 JLL(IAD+IIK) = 1
                 SLL(IAD+IIK) = 0
                 XLL(IAD+IIK) = NI(I,IK)
                 NN = LLL(IAD+IIK)
                 COMNTAG(NN) = COMNTAG(NN) + 1
              ELSE
	        J1=IPERM1(IK)
	        J2=IPERM2(IK)
	        XLL(IAD+J1)=XLL(IAD+J1)+0.5*NI(I,IK)
		XLL(IAD+J2)=XLL(IAD+J2)+0.5*NI(I,IK)
	      ENDIF
            ENDDO
	    IADLL(N_MULT+1)=IADLL(N_MULT) + IIK	
	    SLL(IAD+IIK) = NINT
C
            N_MULT=N_MULT+1
            IAD = IADLL(N_MULT) - 1
            IIK=0	
            DO IK=1,21
	     IF(III(I,IK)/=0)THEN
	         IIK=IIK+1
		 LLL(IAD+IIK) = III(I,IK)
                 JLL(IAD+IIK) = 2
                 SLL(IAD+IIK) = 0
                 XLL(IAD+IIK) = NI(I,IK)
		 NN = LLL(IAD+IIK)
                 COMNTAG(NN) = COMNTAG(NN) + 1
              ELSE
	        J1=IPERM1(IK)
	        J2=IPERM2(IK)
	        XLL(IAD+J1)=XLL(IAD+J1)+0.5*NI(I,IK)
		XLL(IAD+J2)=XLL(IAD+J2)+0.5*NI(I,IK)
	      ENDIF
            ENDDO
	    IADLL(N_MULT+1)=IADLL(N_MULT) + IIK	
	    SLL(IAD+IIK) = NINT
C
            N_MULT=N_MULT+1
            IAD = IADLL(N_MULT) - 1
            IIK=0	
            DO IK=1,21
	     IF(III(I,IK)/=0)THEN
	         IIK=IIK+1
		 LLL(IAD+IIK) = III(I,IK)
                 JLL(IAD+IIK) = 3
                 SLL(IAD+IIK) = 0
                 XLL(IAD+IIK) = NI(I,IK)
	         NN = LLL(IAD+IIK)
                 COMNTAG(NN) = COMNTAG(NN) + 1
              ELSE
	        J1=IPERM1(IK)
	        J2=IPERM2(IK)
	        XLL(IAD+J1)=XLL(IAD+J1)+0.5*NI(I,IK)
		XLL(IAD+J2)=XLL(IAD+J2)+0.5*NI(I,IK)
	      ENDIF
            ENDDO
	    IADLL(N_MULT+1)=IADLL(N_MULT) + IIK	
	    SLL(IAD+IIK) = NINT
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
Chd|  I20RST                        source/interfaces/int16/i20lagm.F
Chd|-- called by -----------
Chd|        I20LLL                        source/interfaces/int16/i20lagm.F
Chd|        I21LLL                        source/interfaces/int17/i21lagm.F
Chd|-- calls ---------------
Chd|        I20DERI                       source/interfaces/int16/i20lagm.F
Chd|        I20NI                         source/interfaces/int16/i20lagm.F
Chd|        I20RSTN                       source/interfaces/int16/i20lagm.F
Chd|====================================================================
      SUBROUTINE I20RST(LLT,R  ,S     ,T     ,NI    ,
     2            NSX   ,NSY   ,NSZ   ,NX    ,NY    ,NZ  ,
     3            XX    ,YY    ,ZZ    )
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
     .   XX(MVSIZ,21),YY(MVSIZ,21),ZZ(MVSIZ,21)
      my_real
     .   R(MVSIZ),S(MVSIZ),T(MVSIZ),NI(MVSIZ,21) ,
     .   NSX(MVSIZ),NSY(MVSIZ),NSZ(MVSIZ),
     .   NX(MVSIZ),NY(MVSIZ),NZ(MVSIZ)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IK,NK,ITER,NITERMAX,JTER,NJTERMAX,CONV
      my_real
     .   VX,VY,VZ,VN
      my_real
     .   SN, RN, TN,
     .   DRDX(MVSIZ),DRDY(MVSIZ),DRDZ(MVSIZ),
     .   DSDX(MVSIZ),DSDY(MVSIZ),DSDZ(MVSIZ),
     .   DTDX(MVSIZ),DTDY(MVSIZ),DTDZ(MVSIZ),
     .   DXDR(MVSIZ),DYDR(MVSIZ),DZDR(MVSIZ),
     .   DXDS(MVSIZ),DYDS(MVSIZ),DZDS(MVSIZ),
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
           CALL I20DERI(LLT,RR ,SS    ,TT    ,NI    ,
     2            DRDX  ,DRDY  ,DRDZ  ,DSDX  ,DSDY  ,DSDZ  ,
     3            DTDX  ,DTDY  ,DTDZ  ,DXDR  ,DYDR  ,DZDR  ,
     4            DXDS  ,DYDS  ,DZDS  ,DXDT  ,DYDT  ,DZDT  ,
     5            XX    ,YY    ,ZZ    )
C
           DO JTER=1,NJTERMAX
c
c      print *, "jter",jter
c
C-----------------------------------------------
C            calcul de r,s,t new
C-----------------------------------------------
             CALL I20RSTN(LLT,RR,SS   ,TT    ,NI    ,CONV  ,
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
             CALL I20NI(LLT,RR ,SS ,TT ,NI  )
C  pb de parith on si conv fonction de mvsiz !!!!!!!
C             IF(CONV/=0)RETURN
C
           ENDDO
      ENDDO
C
       DO I=1,LLT
         NSX(I) = DYDT(I)*DZDR(I) - DZDT(I)*DYDR(I)
         NSY(I) = DZDT(I)*DXDR(I) - DXDT(I)*DZDR(I)
         NSZ(I) = DXDT(I)*DYDR(I) - DYDT(I)*DXDR(I)
C
         SN = SS(I) * SS(I)
         SN = SN * SN
         SN = SN * SN
         SN = SN * SN
         SN = SN * SN
         SN = SN * SS(I)
         NX(I) = SN * NSX(I)
         NY(I) = SN * NSY(I)
         NZ(I) = SN * NSZ(I)
C
         RN = RR(I) * RR(I)
         RN = RN * RN
         RN = RN * RN
         RN = RN * RN
         RN = RN * RN
         RN = RN * RR(I)
         NX(I) = NX(I) + RN * (DYDS(I)*DZDT(I) - DZDS(I)*DYDT(I))
         NY(I) = NY(I) + RN * (DZDS(I)*DXDT(I) - DXDS(I)*DZDT(I))
         NZ(I) = NZ(I) + RN * (DXDS(I)*DYDT(I) - DYDS(I)*DXDT(I))
C
         TN = TT(I) * TT(I)
         TN = TN * TN
         TN = TN * TN
         TN = TN * TN
         TN = TN * TN
         TN = TN * TT(I)
         NX(I) = NX(I) + TN * (DYDR(I)*DZDS(I) - DZDR(I)*DYDS(I))
         NY(I) = NY(I) + TN * (DZDR(I)*DXDS(I) - DXDR(I)*DZDS(I))
         NZ(I) = NZ(I) + TN * (DXDR(I)*DYDS(I) - DYDR(I)*DXDS(I))
C
       ENDDO
C
      RETURN
      END
C
Chd|====================================================================
Chd|  I20NI                         source/interfaces/int16/i20lagm.F
Chd|-- called by -----------
Chd|        I20RST                        source/interfaces/int16/i20lagm.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I20NI(LLT,RR ,SS ,TT ,NI    )
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
     .   RR(MVSIZ),SS(MVSIZ),TT(MVSIZ),NI(MVSIZ,21)
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
        NI(I,1) = U_M_R * UMS_UMT * (-RR(I)-SS(I)-TT(I)-TWO)
        NI(I,2) = U_M_R * UMS_UPT * (-RR(I)-SS(I)+TT(I)-TWO)
        NI(I,3) = U_P_R * UMS_UPT * ( RR(I)-SS(I)+TT(I)-TWO)
        NI(I,4) = U_P_R * UMS_UMT * ( RR(I)-SS(I)-TT(I)-TWO)
        NI(I,5) = U_M_R * UPS_UMT * (-RR(I)+SS(I)-TT(I)-TWO)
        NI(I,6) = U_M_R * UPS_UPT * (-RR(I)+SS(I)+TT(I)-TWO)
        NI(I,7) = U_P_R * UPS_UPT * ( RR(I)+SS(I)+TT(I)-TWO)
        NI(I,8) = U_P_R * UPS_UMT * ( RR(I)+SS(I)-TT(I)-TWO)
C------------------------------------
        A = (ONE-RR(I)*RR(I))
        NI(I,10) = A * UMS_UPT
        NI(I,12) = A * UMS_UMT
        NI(I,18) = A * UPS_UPT
        NI(I,20) = A * UPS_UMT
C------------------------------------
        A = (ONE-SS(I)*SS(I))
        NI(I,13) = A * UMT_UMR
        NI(I,14) = A * UPT_UMR
        NI(I,15) = A * UPT_UPR
        NI(I,16) = A * UMT_UPR
C------------------------------------
        A = (ONE-TT(I)*TT(I))
        NI(I,9)  = A * UMR_UMS
        NI(I,11) = A * UPR_UMS
        NI(I,17) = A * UMR_UPS
        NI(I,19) = A * UPR_UPS
C------------------------------------
        NI(I,21) = -ONE
C------------------------------------
      ENDDO
C-----------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  I20RSTN                       source/interfaces/int16/i20lagm.F
Chd|-- called by -----------
Chd|        I20RST                        source/interfaces/int16/i20lagm.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I20RSTN(LLT,RR,SS    ,TT    ,NI    ,CONV  ,
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
     .   R(MVSIZ),S(MVSIZ),T(MVSIZ),NI(MVSIZ,21) ,
     .   RR(MVSIZ),SS(MVSIZ),TT(MVSIZ),
     .   XX(MVSIZ,21) ,YY(MVSIZ,21) ,ZZ(MVSIZ,21) ,
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
        DX = XX(I,21)
     +     - NI(I, 1)*XX(I, 1) - NI(I, 2)*XX(I, 2) - NI(I, 3)*XX(I, 3)
     +     - NI(I, 4)*XX(I, 4) - NI(I, 5)*XX(I, 5) - NI(I, 6)*XX(I, 6)
     +     - NI(I, 7)*XX(I, 7) - NI(I, 8)*XX(I, 8) - NI(I, 9)*XX(I, 9)
     +     - NI(I,10)*XX(I,10) - NI(I,11)*XX(I,11) - NI(I,12)*XX(I,12)
     +     - NI(I,13)*XX(I,13) - NI(I,14)*XX(I,14) - NI(I,15)*XX(I,15)
     +     - NI(I,16)*XX(I,16) - NI(I,17)*XX(I,17) - NI(I,18)*XX(I,18)
     +     - NI(I,19)*XX(I,19) - NI(I,20)*XX(I,20)
        DY = YY(I,21)
     +     - NI(I, 1)*YY(I, 1) - NI(I, 2)*YY(I, 2) - NI(I, 3)*YY(I, 3)
     +     - NI(I, 4)*YY(I, 4) - NI(I, 5)*YY(I, 5) - NI(I, 6)*YY(I, 6)
     +     - NI(I, 7)*YY(I, 7) - NI(I, 8)*YY(I, 8) - NI(I, 9)*YY(I, 9)
     +     - NI(I,10)*YY(I,10) - NI(I,11)*YY(I,11) - NI(I,12)*YY(I,12)
     +     - NI(I,13)*YY(I,13) - NI(I,14)*YY(I,14) - NI(I,15)*YY(I,15)
     +     - NI(I,16)*YY(I,16) - NI(I,17)*YY(I,17) - NI(I,18)*YY(I,18)
     +     - NI(I,19)*YY(I,19) - NI(I,20)*YY(I,20)
        DZ = ZZ(I,21)
     +     - NI(I, 1)*ZZ(I, 1) - NI(I, 2)*ZZ(I, 2) - NI(I, 3)*ZZ(I, 3)
     +     - NI(I, 4)*ZZ(I, 4) - NI(I, 5)*ZZ(I, 5) - NI(I, 6)*ZZ(I, 6)
     +     - NI(I, 7)*ZZ(I, 7) - NI(I, 8)*ZZ(I, 8) - NI(I, 9)*ZZ(I, 9)
     +     - NI(I,10)*ZZ(I,10) - NI(I,11)*ZZ(I,11) - NI(I,12)*ZZ(I,12)
     +     - NI(I,13)*ZZ(I,13) - NI(I,14)*ZZ(I,14) - NI(I,15)*ZZ(I,15)
     +     - NI(I,16)*ZZ(I,16) - NI(I,17)*ZZ(I,17) - NI(I,18)*ZZ(I,18)
     +     - NI(I,19)*ZZ(I,19) - NI(I,20)*ZZ(I,20)
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
Chd|  I20DERI                       source/interfaces/int16/i20lagm.F
Chd|-- called by -----------
Chd|        I20RST                        source/interfaces/int16/i20lagm.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I20DERI(LLT,RR,SS ,TT    ,NI    ,
     2   DRDX  ,DRDY  ,DRDZ  ,DSDX  ,DSDY  ,DSDZ  ,
     3   DTDX  ,DTDY  ,DTDZ  ,DXDR  ,DYDR  ,DZDR  ,
     4   DXDS  ,DYDS  ,DZDS  ,DXDT  ,DYDT  ,DZDT  ,
     5   XX    ,YY    ,ZZ    )
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
     .   DXDS(MVSIZ), DYDS(MVSIZ), DZDS(MVSIZ),
     .   DXDT(MVSIZ), DYDT(MVSIZ), DZDT(MVSIZ),
     .   DRDX(MVSIZ), DSDX(MVSIZ), DTDX(MVSIZ),
     .   DRDY(MVSIZ), DSDY(MVSIZ), DTDY(MVSIZ),
     .   DRDZ(MVSIZ), DSDZ(MVSIZ), DTDZ(MVSIZ),
     .   XX(MVSIZ,21) ,YY(MVSIZ,21),ZZ(MVSIZ,21),
     .   NI(MVSIZ,21) ,RR(MVSIZ) ,SS(MVSIZ) ,TT(MVSIZ)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N
      my_real
     .   DNIDR(20),DNIDS(20),DNIDT(20),
     .   D, AA, BB, DET(MVSIZ),R9 ,R13 ,S9 ,S10 ,S11 ,S12 ,T10 ,T14
      my_real
     .  U_M_R,U_P_R,U_M_S,U_P_S,U_M_T,U_P_T,
     .  UMS_UMT,UMS_UPT,UPS_UMT,UPS_UPT,
     .  UMR_UMS,UMR_UPS,UPR_UMS,UPR_UPS,
     .  UMT_UMR,UMT_UPR,UPT_UMR,UPT_UPR,
     .  A,R05,S05,T05
C-----------------------------------------------
C
C
C                                      ^ S               _ T
C                                      |                 /|
C                                      |                /
C                                      |               /
C                        ( 7)==========|===(18)===============( 6)
C                        //|           |             /        //|
C                       // |           |            /        //||
C                      //  |           |                    // ||
C                     //   |           |          /        //  ||
C                    //    |           |         /        //   ||
C                  (19)    |    *- - - + - * - - - - -* (17)   ||
C                  //      |   /|      |  /|   /     /| //     ||
C                 //       |                  /        //      ||
C                //      (15)/  |      |/  | +     /  //      (14)
C               //         |#- - - - - # - -/- - -#  //        ||
C              //          |    * - - /|- -*- - -/ -//*        ||
C            ( 8)===============(20)==============( 5)         ||
C             ||         / || / |   /  | / |   /  ||| |        ||
C             ||        @- | - - - @ - - - - -@    ||          ||
C     R <-----||- - -+ -|- -# - - -| - # - - -|- -#|| - -+     ||
C             ||           |    * - - /| - *- - -/-|| *        ||
C             ||        |  ||  /   |      /   |   |||/         ||
C             ||         ( 3)-------/--|---(10)----||---------( 2)
C             ||        @ /- / - - @ - -/ - - @    ||         //
C             ||        |/  #- - -/| - # - - -|- -#||        //
C            (16)       /  /     +    /|         /(13)      // 
C             ||       /|          |          |    ||      //    
C             ||      /  /          /  |       /   ||     //    
C             ||   (11) @- - - - - @ - + - - -@    ||   ( 9)    
C             ||    /                              ||   //     
C             ||   /                               ||  //    
C             ||  /                                || //    
C             || /                                 ||//    
C             ||/                                  ||/    
C            ( 4)===============(12)==============( 1)    
C                                                
C
C
C
C
C*/
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
C
C
C
        NI(I,1) = U_M_R * UMS_UMT * (-RR(I)-SS(I)-TT(I)-TWO)
        NI(I,2) = U_M_R * UMS_UPT * (-RR(I)-SS(I)+TT(I)-TWO)
        NI(I,3) = U_P_R * UMS_UPT * ( RR(I)-SS(I)+TT(I)-TWO)
        NI(I,4) = U_P_R * UMS_UMT * ( RR(I)-SS(I)-TT(I)-TWO)
        NI(I,5) = U_M_R * UPS_UMT * (-RR(I)+SS(I)-TT(I)-TWO)
        NI(I,6) = U_M_R * UPS_UPT * (-RR(I)+SS(I)+TT(I)-TWO)
        NI(I,7) = U_P_R * UPS_UPT * ( RR(I)+SS(I)+TT(I)-TWO)
        NI(I,8) = U_P_R * UPS_UMT * ( RR(I)+SS(I)-TT(I)-TWO)
C
        DNIDR(1) = -UMS_UMT * (U_M_S + U_M_T - RR(I) -THREE_HALF)
        DNIDR(2) = -UMS_UPT * (U_M_S + U_P_T - RR(I) -THREE_HALF)
        DNIDR(3) =  UMS_UPT * (U_M_S + U_P_T + RR(I) -THREE_HALF)
        DNIDR(4) =  UMS_UMT * (U_M_S + U_M_T + RR(I) -THREE_HALF)
        DNIDR(5) = -UPS_UMT * (U_P_S + U_M_T - RR(I) -THREE_HALF)
        DNIDR(6) = -UPS_UPT * (U_P_S + U_P_T - RR(I) -THREE_HALF)
        DNIDR(7) =  UPS_UPT * (U_P_S + U_P_T + RR(I) -THREE_HALF)
        DNIDR(8) =  UPS_UMT * (U_P_S + U_M_T + RR(I) -THREE_HALF)
C
        DNIDS(1) = -UMT_UMR * (U_M_R + U_M_T - SS(I) -THREE_HALF)
        DNIDS(2) = -UPT_UMR * (U_M_R + U_P_T - SS(I) -THREE_HALF)
        DNIDS(3) = -UPT_UPR * (U_P_R + U_P_T - SS(I) -THREE_HALF)
        DNIDS(4) = -UMT_UPR * (U_P_R + U_M_T - SS(I) -THREE_HALF)
        DNIDS(5) =  UMT_UMR * (U_M_R + U_M_T + SS(I) -THREE_HALF)
        DNIDS(6) =  UPT_UMR * (U_M_R + U_P_T + SS(I) -THREE_HALF)
        DNIDS(7) =  UPT_UPR * (U_P_R + U_P_T + SS(I) -THREE_HALF)
        DNIDS(8) =  UMT_UPR * (U_P_R + U_M_T + SS(I) -THREE_HALF)
C
        DNIDT(1) = -UMR_UMS * (U_M_R + U_M_S - TT(I) -THREE_HALF)
        DNIDT(2) =  UMR_UMS * (U_M_R + U_M_S + TT(I) -THREE_HALF)
        DNIDT(3) =  UPR_UMS * (U_P_R + U_M_S + TT(I) -THREE_HALF)
        DNIDT(4) = -UPR_UMS * (U_P_R + U_M_S - TT(I) -THREE_HALF)
        DNIDT(5) = -UMR_UPS * (U_M_R + U_P_S - TT(I) -THREE_HALF)
        DNIDT(6) =  UMR_UPS * (U_M_R + U_P_S + TT(I) -THREE_HALF)
        DNIDT(7) =  UPR_UPS * (U_P_R + U_P_S + TT(I) -THREE_HALF)
        DNIDT(8) = -UPR_UPS * (U_P_R + U_P_S - TT(I) -THREE_HALF)
C------------------------------------
        A = (ONE-RR(I)*RR(I))
        NI(I,10) = A * UMS_UPT
        NI(I,12) = A * UMS_UMT
        NI(I,18) = A * UPS_UPT
        NI(I,20) = A * UPS_UMT
C    
        A = HALF*A
        DNIDT(10) =  A * U_M_S
        DNIDT(18) =  A * U_P_S
        DNIDT(12) = -DNIDT(10)
        DNIDT(20) = -DNIDT(18)
C    
        DNIDS(18) =  A * U_P_T
        DNIDS(20) =  A * U_M_T
        DNIDS(10) = -DNIDS(18)
        DNIDS(12) = -DNIDS(20)
C    
        A = -TWO*RR(I)
        DNIDR(10) = A * UMS_UPT
        DNIDR(12) = A * UMS_UMT
        DNIDR(18) = A * UPS_UPT
        DNIDR(20) = A * UPS_UMT
C------------------------------------
        A = (ONE-SS(I)*SS(I))
        NI(I,13) = A * UMT_UMR
        NI(I,14) = A * UPT_UMR
        NI(I,15) = A * UPT_UPR
        NI(I,16) = A * UMT_UPR
C
        A = HALF*A
        DNIDR(15) =  A * U_P_T
        DNIDR(16) =  A * U_M_T
        DNIDR(13) = -DNIDR(16)
        DNIDR(14) = -DNIDR(15)
C
        DNIDT(14) =  A * U_M_R
        DNIDT(15) =  A * U_P_R
        DNIDT(13) = -DNIDT(14)
        DNIDT(16) = -DNIDT(15)
C    
        A = -TWO*SS(I)
        DNIDS(13) = A * UMT_UMR
        DNIDS(14) = A * UPT_UMR
        DNIDS(15) = A * UPT_UPR
        DNIDS(16) = A * UMT_UPR
C------------------------------------
        A = (ONE-TT(I)*TT(I))
        NI(I,9)  = A * UMR_UMS
        NI(I,11) = A * UPR_UMS
        NI(I,17) = A * UMR_UPS
        NI(I,19) = A * UPR_UPS
C
        NI(I,21) = -ONE
C
        A = HALF*A
        DNIDR(11) =  A * U_M_S
        DNIDR(19) =  A * U_P_S
        DNIDR(9)  = -DNIDR(11)
        DNIDR(17) = -DNIDR(19)
C
        DNIDS(17) =  A * U_M_R
        DNIDS(19) =  A * U_P_R
        DNIDS(9)  = -DNIDS(17)
        DNIDS(11) = -DNIDS(19)
C
        A = -TWO*TT(I)
        DNIDT(9)  = A * UMR_UMS
        DNIDT(11) = A * UPR_UMS
        DNIDT(17) = A * UMR_UPS
        DNIDT(19) = A * UPR_UPS
C
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
     +    + DNIDR(9)*XX(I,9)   + DNIDR(10)*XX(I,10) + DNIDR(11)*XX(I,11)
     +    + DNIDR(12)*XX(I,12) + DNIDR(13)*XX(I,13) + DNIDR(14)*XX(I,14)
     +    + DNIDR(15)*XX(I,15) + DNIDR(16)*XX(I,16) + DNIDR(17)*XX(I,17)
     +    + DNIDR(18)*XX(I,18) + DNIDR(19)*XX(I,19) + DNIDR(20)*XX(I,20)
C  
        DXDS(I) = DNIDS(1)*XX(I,1) + DNIDS(2)*XX(I,2) + DNIDS(3)*XX(I,3)
     +          + DNIDS(4)*XX(I,4) + DNIDS(5)*XX(I,5) + DNIDS(6)*XX(I,6)
     +          + DNIDS(7)*XX(I,7) + DNIDS(8)*XX(I,8)
     +    + DNIDS(9)*XX(I,9)   + DNIDS(10)*XX(I,10) + DNIDS(11)*XX(I,11)
     +    + DNIDS(12)*XX(I,12) + DNIDS(13)*XX(I,13) + DNIDS(14)*XX(I,14)
     +    + DNIDS(15)*XX(I,15) + DNIDS(16)*XX(I,16) + DNIDS(17)*XX(I,17)
     +    + DNIDS(18)*XX(I,18) + DNIDS(19)*XX(I,19) + DNIDS(20)*XX(I,20)
C 
        DXDT(I) = DNIDT(1)*XX(I,1) + DNIDT(2)*XX(I,2) + DNIDT(3)*XX(I,3)
     +          + DNIDT(4)*XX(I,4) + DNIDT(5)*XX(I,5) + DNIDT(6)*XX(I,6)
     +          + DNIDT(7)*XX(I,7) + DNIDT(8)*XX(I,8)
     +    + DNIDT(9)*XX(I,9)   + DNIDT(10)*XX(I,10) + DNIDT(11)*XX(I,11)
     +    + DNIDT(12)*XX(I,12) + DNIDT(13)*XX(I,13) + DNIDT(14)*XX(I,14)
     +    + DNIDT(15)*XX(I,15) + DNIDT(16)*XX(I,16) + DNIDT(17)*XX(I,17)
     +    + DNIDT(18)*XX(I,18) + DNIDT(19)*XX(I,19) + DNIDT(20)*XX(I,20)
C-----------------------------------------------------------------------
C     dy/dr; dy/ds; dy/dt
C-----------------------------------------------------------------------
        DYDR(I) = DNIDR(1)*YY(I,1) + DNIDR(2)*YY(I,2) + DNIDR(3)*YY(I,3)
     +          + DNIDR(4)*YY(I,4) + DNIDR(5)*YY(I,5) + DNIDR(6)*YY(I,6)
     +          + DNIDR(7)*YY(I,7) + DNIDR(8)*YY(I,8)
     +    + DNIDR(9)*YY(I,9)   + DNIDR(10)*YY(I,10) + DNIDR(11)*YY(I,11)
     +    + DNIDR(12)*YY(I,12) + DNIDR(13)*YY(I,13) + DNIDR(14)*YY(I,14)
     +    + DNIDR(15)*YY(I,15) + DNIDR(16)*YY(I,16) + DNIDR(17)*YY(I,17)
     +    + DNIDR(18)*YY(I,18) + DNIDR(19)*YY(I,19) + DNIDR(20)*YY(I,20)
C  
        DYDS(I) = DNIDS(1)*YY(I,1) + DNIDS(2)*YY(I,2) + DNIDS(3)*YY(I,3)
     +          + DNIDS(4)*YY(I,4) + DNIDS(5)*YY(I,5) + DNIDS(6)*YY(I,6)
     +          + DNIDS(7)*YY(I,7) + DNIDS(8)*YY(I,8)
     +    + DNIDS(9)*YY(I,9)   + DNIDS(10)*YY(I,10) + DNIDS(11)*YY(I,11)
     +    + DNIDS(12)*YY(I,12) + DNIDS(13)*YY(I,13) + DNIDS(14)*YY(I,14)
     +    + DNIDS(15)*YY(I,15) + DNIDS(16)*YY(I,16) + DNIDS(17)*YY(I,17)
     +    + DNIDS(18)*YY(I,18) + DNIDS(19)*YY(I,19) + DNIDS(20)*YY(I,20)
C 
        DYDT(I) = DNIDT(1)*YY(I,1) + DNIDT(2)*YY(I,2) + DNIDT(3)*YY(I,3)
     +          + DNIDT(4)*YY(I,4) + DNIDT(5)*YY(I,5) + DNIDT(6)*YY(I,6)
     +          + DNIDT(7)*YY(I,7) + DNIDT(8)*YY(I,8)
     +    + DNIDT(9)*YY(I,9)   + DNIDT(10)*YY(I,10) + DNIDT(11)*YY(I,11)
     +    + DNIDT(12)*YY(I,12) + DNIDT(13)*YY(I,13) + DNIDT(14)*YY(I,14)
     +    + DNIDT(15)*YY(I,15) + DNIDT(16)*YY(I,16) + DNIDT(17)*YY(I,17)
     +    + DNIDT(18)*YY(I,18) + DNIDT(19)*YY(I,19) + DNIDT(20)*YY(I,20)
C-----------------------------------------------------------------------
C     dz/dr; dz/ds; dz/dt
C-----------------------------------------------------------------------
        DZDR(I) = DNIDR(1)*ZZ(I,1) + DNIDR(2)*ZZ(I,2) + DNIDR(3)*ZZ(I,3)
     +          + DNIDR(4)*ZZ(I,4) + DNIDR(5)*ZZ(I,5) + DNIDR(6)*ZZ(I,6)
     +          + DNIDR(7)*ZZ(I,7) + DNIDR(8)*ZZ(I,8)
     +    + DNIDR(9)*ZZ(I,9)   + DNIDR(10)*ZZ(I,10) + DNIDR(11)*ZZ(I,11)
     +    + DNIDR(12)*ZZ(I,12) + DNIDR(13)*ZZ(I,13) + DNIDR(14)*ZZ(I,14)
     +    + DNIDR(15)*ZZ(I,15) + DNIDR(16)*ZZ(I,16) + DNIDR(17)*ZZ(I,17)
     +    + DNIDR(18)*ZZ(I,18) + DNIDR(19)*ZZ(I,19) + DNIDR(20)*ZZ(I,20)
C  
        DZDS(I) = DNIDS(1)*ZZ(I,1) + DNIDS(2)*ZZ(I,2) + DNIDS(3)*ZZ(I,3)
     +          + DNIDS(4)*ZZ(I,4) + DNIDS(5)*ZZ(I,5) + DNIDS(6)*ZZ(I,6)
     +          + DNIDS(7)*ZZ(I,7) + DNIDS(8)*ZZ(I,8)
     +    + DNIDS(9)*ZZ(I,9)   + DNIDS(10)*ZZ(I,10) + DNIDS(11)*ZZ(I,11)
     +    + DNIDS(12)*ZZ(I,12) + DNIDS(13)*ZZ(I,13) + DNIDS(14)*ZZ(I,14)
     +    + DNIDS(15)*ZZ(I,15) + DNIDS(16)*ZZ(I,16) + DNIDS(17)*ZZ(I,17)
     +    + DNIDS(18)*ZZ(I,18) + DNIDS(19)*ZZ(I,19) + DNIDS(20)*ZZ(I,20)
C 
        DZDT(I) = DNIDT(1)*ZZ(I,1) + DNIDT(2)*ZZ(I,2) + DNIDT(3)*ZZ(I,3)
     +          + DNIDT(4)*ZZ(I,4) + DNIDT(5)*ZZ(I,5) + DNIDT(6)*ZZ(I,6)
     +          + DNIDT(7)*ZZ(I,7) + DNIDT(8)*ZZ(I,8)
     +    + DNIDT(9)*ZZ(I,9)   + DNIDT(10)*ZZ(I,10) + DNIDT(11)*ZZ(I,11)
     +    + DNIDT(12)*ZZ(I,12) + DNIDT(13)*ZZ(I,13) + DNIDT(14)*ZZ(I,14)
     +    + DNIDT(15)*ZZ(I,15) + DNIDT(16)*ZZ(I,16) + DNIDT(17)*ZZ(I,17)
     +    + DNIDT(18)*ZZ(I,18) + DNIDT(19)*ZZ(I,19) + DNIDT(20)*ZZ(I,20)
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
