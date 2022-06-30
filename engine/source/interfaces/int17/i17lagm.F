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
Chd|  I17LAGM                       source/interfaces/int17/i17lagm.F
Chd|-- called by -----------
Chd|        I17MAIN                       source/interfaces/int17/i17main.F
Chd|-- calls ---------------
Chd|        I17LLL                        source/interfaces/int17/i17lagm.F
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|====================================================================
      SUBROUTINE I17LAGM(X    ,V     ,LLL     ,JLL   ,SLL   ,
     2                  XLL   ,CANDN ,CANDE   ,I_STOK,IXS   ,
     3                  IXS16 ,IADLL ,EMINX   ,NELES ,NELEM ,
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
      COMMON /I17GLOBI/IE_MIN,IES_MIN
      COMMON /I17GLOBR/VIT_MIN
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC,I_STOK,N_MUL_MX,ITASK,ITIED,NINT,NKMAX ,
     .  LLL(*),JLL(*),SLL(*),CANDN(*),CANDE(*),COMNTAG(*),
     .  IXS(NIXS,*),IXS16(8,*),IADLL(*),NELES(*)  ,NELEM(*) 
C     REAL
      my_real
     .  X(3,*),V(3,*),XLL(*),
     .  EMINX(6,*),EMINXS(6,*),A(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,IK,IE,IS,IC,NK,III(MVSIZ,17),LLT,NFT,LE,FIRST,LAST,
     .        I16,LES,IES,IIIS(MVSIZ,16),NC_SAV,IEL(MVSIZ),IESL(MVSIZ),
     .        IE_MIN,IES_MIN
      my_real
     .   XX(MVSIZ,17),YY(MVSIZ,17),ZZ(MVSIZ,17),
     .   XXS(MVSIZ,16),YYS(MVSIZ,16),ZZS(MVSIZ,16),
     .   AA,XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,DIST,VIT_MIN
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
      NC_SAV = NC
      VIT_MIN = ZERO
      IE_MIN  = 99999999
      IES_MIN = 99999999
      CALL MY_BARRIER
      FIRST = 1 + I_STOK * ITASK / NTHREAD
      LAST = I_STOK*(ITASK+1) / NTHREAD
      LLT = 0
      NFT=LLT+1
      DO IC=FIRST,LAST
        LE  = CANDE(IC)
        LES = CANDN(IC)
        IE  = NELEM(LE)
        IES = NELES(LES)
C-----------------------------------------------------------------------
C       test si dans la boite   
C-----------------------------------------------------------------------
        IF(LE .GT.0.AND.LES.GT.0.AND.
     .           EMINXS(4,LES).GT.EMINX(1,LE).AND.
     .           EMINXS(5,LES).GT.EMINX(2,LE).AND.
     .           EMINXS(6,LES).GT.EMINX(3,LE).AND.
     .           EMINXS(1,LES).LT.EMINX(4,LE).AND.
     .           EMINXS(2,LES).LT.EMINX(5,LE).AND.
     .           EMINXS(3,LES).LT.EMINX(6,LE))THEN
c
c      print *, "dans la boite",XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX
c
          LLT = LLT+1
	  IEL(LLT)=IE
	  IESL(LLT)=IES
          DO K=1,4
C III 1,2,3,4,5,6,7,8, 9,10,11,12,13,14,15,16
            III(LLT,K)   =IXS(K+1,IE)
            III(LLT,K+4) =IXS(K+5,IE)
            III(LLT,K+8) =IXS16(K,IE-NUMELS8-NUMELS10-NUMELS20)
            III(LLT,K+12)=IXS16(K+4,IE-NUMELS8-NUMELS10-NUMELS20)
C IIIS 1,2,3,4,9,10,11,12, 5,6,7,8,13,14,15,16
            IIIS(LLT,K)  =IXS(K+1,IES)
            IIIS(LLT,K+8) =IXS(K+5,IES)
            IIIS(LLT,K+4)=IXS16(K,IES-NUMELS8-NUMELS10-NUMELS20)
            IIIS(LLT,K+12)=IXS16(K+4,IES-NUMELS8-NUMELS10-NUMELS20)
          ENDDO
          DO K=1,16
            I = III(LLT,K)
c            XX(LLT,K)=X(1,I)+DT2*(V(1,I)+DT12*A(1,I))
c            YY(LLT,K)=X(2,I)+DT2*(V(2,I)+DT12*A(2,I))
c            ZZ(LLT,K)=X(3,I)+DT2*(V(3,I)+DT12*A(3,I))
c            XX(LLT,K)=X(1,I)
c            YY(LLT,K)=X(2,I)
c            ZZ(LLT,K)=X(3,I)
            XX(LLT,K)=X(1,I)+HALF*DT2*(V(1,I)+DT12*A(1,I))
            YY(LLT,K)=X(2,I)+HALF*DT2*(V(2,I)+DT12*A(2,I))
            ZZ(LLT,K)=X(3,I)+HALF*DT2*(V(3,I)+DT12*A(3,I))
            I = IIIS(LLT,K)
            XXS(LLT,K)=X(1,I)+HALF*DT2*(V(1,I)+DT12*A(1,I))
            YYS(LLT,K)=X(2,I)+HALF*DT2*(V(2,I)+DT12*A(2,I))
            ZZS(LLT,K)=X(3,I)+HALF*DT2*(V(3,I)+DT12*A(3,I))
          ENDDO
c
c      print *, "XX(1,1),XX(1,9)",XX(1,1),XX(1,9)
c
C-----------------------------------------------------------------------
C     calcul de  [L] par paquet de mvsiz   
C-----------------------------------------------------------------------
          IF(LLT.EQ.MVSIZ-1)THEN
            CALL I17LLL(
     1       LLT ,LLL       ,JLL       ,SLL       ,XLL     ,V       ,
     2       XX  ,YY        ,ZZ        ,III       ,NC      ,IADLL   ,
     3       N_MUL_MX ,A    ,X         ,ITIED     ,NINT    ,NKMAX   ,
     4       XXS ,YYS       ,ZZS       ,IIIS      ,NC_SAV  ,VIT_MIN ,
     5       IEL  ,IESL     ,IE_MIN    ,IES_MIN   ,ITASK   ,COMNTAG )
            NFT=LLT+1
            LLT = 0
          ENDIF
        ELSE
c debug
          k=0
        ENDIF
      ENDDO
C-----------------------------------------------------------------------
C     calcul de  [L] pour dernier paquet   
C-----------------------------------------------------------------------	
      IF(LLT.NE.0) CALL I17LLL(
     1       LLT ,LLL       ,JLL       ,SLL       ,XLL     ,V       ,
     2       XX  ,YY        ,ZZ        ,III       ,NC      ,IADLL   ,
     3       N_MUL_MX ,A    ,X         ,ITIED     ,NINT    ,NKMAX   ,
     4       XXS ,YYS       ,ZZS       ,IIIS      ,NC_SAV  ,VIT_MIN ,
     5       IEL  ,IESL     ,IE_MIN    ,IES_MIN   ,ITASK   ,COMNTAG )
C
C-----------------------------------------------
      CALL MY_BARRIER
      RETURN
      END
Chd|====================================================================
Chd|  I17LLL                        source/interfaces/int17/i17lagm.F
Chd|-- called by -----------
Chd|        I17LAGM                       source/interfaces/int17/i17lagm.F
Chd|-- calls ---------------
Chd|        I17LLL4                       source/interfaces/int17/i17lagm.F
Chd|        I17MINI                       source/interfaces/int17/i17lagm.F
Chd|        I17RST                        source/interfaces/int17/i17lagm.F
Chd|        I17VIT4                       source/interfaces/int17/i17lagm.F
Chd|====================================================================
      SUBROUTINE I17LLL(LLT  ,LLL    ,JLL   ,SLL    ,XLL   ,V      ,
     2                  XX   ,YY     ,ZZ    ,III    ,NC    ,IADLL  ,
     3                  N_MUL_MX,A   ,X     ,ITIED  ,NINT  ,NKMAX  ,
     4                  XXS  ,YYS    ,ZZS   ,IIIS   ,NC_SAV,VIT_MIN,
     5                  IE   ,IES    ,IE_MIN,IES_MIN,ITASK ,COMNTAG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
Cbbdebug +1
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
      INTEGER LLT,NC,N_MUL_MX,ITIED,NINT ,NKMAX ,NC_SAV ,IE_MIN,IES_MIN 
      INTEGER LLL(*),JLL(*),SLL(*),ITASK,COMNTAG(*),
     .        III(MVSIZ,17),IADLL(*) ,IIIS(MVSIZ,16),IE(*) ,IES(*)
C     REAL
      my_real
     .  XLL(*),V(3,*),A(3,*)
      my_real
     .   XX(MVSIZ,17),YY(MVSIZ,17),ZZ(MVSIZ,17),X(3,*),
     .   XXS(MVSIZ,16) ,YYS(MVSIZ,16) ,ZZS(MVSIZ,16) ,VIT_MIN       
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IK,NK,I1,I2,I3,I4,IAD,ICON,
     +        ICONT(MVSIZ,4)
      my_real
     .   VX,VY,VZ,VN,AA,VV,PENE
      my_real
     .   R_CM(MVSIZ),T_CM(MVSIZ),S_CM(MVSIZ),SI_S(MVSIZ,8),
     .   RI_S(MVSIZ,8),TI_S(MVSIZ,8),
     .   NX(MVSIZ), NY(MVSIZ), NZ(MVSIZ),
     .   NI_M(MVSIZ,17),NI_S(MVSIZ,8) ,
     .   R_1S(MVSIZ) ,R_2S(MVSIZ) ,T_1S(MVSIZ) ,T_2S(MVSIZ),
     .   R_1M(MVSIZ) ,R_2M(MVSIZ) ,T_1M(MVSIZ) ,T_2M(MVSIZ),
     .   R_3M(MVSIZ) ,R_4M(MVSIZ) ,T_3M(MVSIZ) ,T_4M(MVSIZ),
     .   R_CS(MVSIZ) ,S_CS(MVSIZ) ,T_CS(MVSIZ) ,VIT(MVSIZ),
     .   R_S(MVSIZ) ,T_S(MVSIZ) 
C-----------------------------------------------
C      calcul de r,s,t
C-----------------------------------------------
c
c      print *, "XX(1,1),XX(1,9)",XX(1,1),XX(1,9)
c
C-----------------------------------------------
C      noeud centrale face 1 2 3 4
C-----------------------------------------------
      DO I=1,LLT
        XX(I,17) = HALF *(XXS(I,5) +XXS(I,6) +XXS(I,7) +XXS(I,8))
     .           - FOURTH*(XXS(I,1) +XXS(I,2) +XXS(I,3) +XXS(I,4))
        YY(I,17) = HALF *(YYS(I,5) +YYS(I,6) +YYS(I,7) +YYS(I,8))
     .           - FOURTH*(YYS(I,1) +YYS(I,2) +YYS(I,3) +YYS(I,4))
        ZZ(I,17) = HALF *(ZZS(I,5) +ZZS(I,6) +ZZS(I,7) +ZZS(I,8))
     .           - FOURTH*(ZZS(I,1) +ZZS(I,2) +ZZS(I,3) +ZZS(I,4))     
      ENDDO
      CALL I17RST(LLT   ,RI_S(1,1),SI_S(1,1),TI_S(1,1),NI_M  ,
     2            XX    ,YY       ,ZZ       )
C-----------------------------------------------
C      noeud centrale face 5 6 7 8
C-----------------------------------------------
      DO I=1,LLT
        XX(I,17) = HALF *(XXS(I,13)+XXS(I,14)+XXS(I,15)+XXS(I,16))
     .           - FOURTH*(XXS(I,9) +XXS(I,10)+XXS(I,11)+XXS(I,12))
        YY(I,17) = HALF *(YYS(I,13)+YYS(I,14)+YYS(I,15)+YYS(I,16))
     .           - FOURTH*(YYS(I,9) +YYS(I,10)+YYS(I,11)+YYS(I,12))
        ZZ(I,17) = HALF *(ZZS(I,13)+ZZS(I,14)+ZZS(I,15)+ZZS(I,16))
     .           - FOURTH*(ZZS(I,9) +ZZS(I,10)+ZZS(I,11)+ZZS(I,12))     
      ENDDO
      CALL I17RST(LLT   ,RI_S(1,2),SI_S(1,2),TI_S(1,2),NI_M  ,
     2            XX    ,YY       ,ZZ       )
C-----------------------------------------------
C      choix face 1 2 3 4 face 5 6 7 8 cote second
C-----------------------------------------------
      DO I=1,LLT
        IF(ABS(SI_S(I,1)).LE.ABS(SI_S(I,2)))THEN
C         face 1 2 3 4
          S_CS(I) = -ONE
        ELSE
C         face 5 6 7 8
          S_CS(I) =  ONE
          IIIS(I,1) = IIIS(I,9)
          IIIS(I,2) = IIIS(I,10)
          IIIS(I,3) = IIIS(I,11)
          IIIS(I,4) = IIIS(I,12)
          IIIS(I,5) = IIIS(I,13)
          IIIS(I,6) = IIIS(I,14)
          IIIS(I,7) = IIIS(I,15)
          IIIS(I,8) = IIIS(I,16)
C
          XXS(I,1) = XXS(I,9)
          XXS(I,2) = XXS(I,10)
          XXS(I,3) = XXS(I,11)
          XXS(I,4) = XXS(I,12)
          XXS(I,5) = XXS(I,13)
          XXS(I,6) = XXS(I,14)
          XXS(I,7) = XXS(I,15)
          XXS(I,8) = XXS(I,16)
C
          YYS(I,1) = YYS(I,9)
          YYS(I,2) = YYS(I,10)
          YYS(I,3) = YYS(I,11)
          YYS(I,4) = YYS(I,12)
          YYS(I,5) = YYS(I,13)
          YYS(I,6) = YYS(I,14)
          YYS(I,7) = YYS(I,15)
          YYS(I,8) = YYS(I,16)
C
          ZZS(I,1) = ZZS(I,9)
          ZZS(I,2) = ZZS(I,10)
          ZZS(I,3) = ZZS(I,11)
          ZZS(I,4) = ZZS(I,12)
          ZZS(I,5) = ZZS(I,13)
          ZZS(I,6) = ZZS(I,14)
          ZZS(I,7) = ZZS(I,15)
          ZZS(I,8) = ZZS(I,16)
C
        ENDIF
      ENDDO
C-----------------------------------------------
C      calcul de SI_S=s(relatif element main) 
c      des 8 noeuds de la face de l'element second
C-----------------------------------------------
      DO J=1,8
        DO I=1,LLT
          XX(I,17) = XXS(I,J)
          YY(I,17) = YYS(I,J)
          ZZ(I,17) = ZZS(I,J)
        ENDDO
        CALL I17RST(LLT   ,RI_S(1,J),SI_S(1,J),TI_S(1,J),NI_M  ,
     2              XX    ,YY    ,ZZ    )
      ENDDO
C-----------------------------------------------
C      calcul du point de distance min
C      dSI_S/dr = 0 dSI_S/dt = 0     (r,t de l'element second) 
C-----------------------------------------------
      CALL I17MINI(LLT   ,R_CS  ,S_CS  ,T_CS  ,RI_S  ,SI_S   ,
     2             TI_S  ,NI_S  ,XXS   ,YYS   ,ZZS   ,XX     ,
     3             YY    ,ZZ    ,R_CM  ,S_CM  ,T_CM  ,NX     ,
     4             NY    ,NZ    ,R_1S  ,R_2S  ,T_1S  ,T_2S   ,
     5             R_1M  ,R_2M  ,R_3M  ,R_4M  ,T_1M  ,T_2M   ,
     6             T_3M  ,T_4M  ,ICONT )
C-----------------------------------------------
C      choix face 1 2 3 4 face 5 6 7 8 cote main
C-----------------------------------------------
      DO I=1,LLT
        IF(S_CM(I).LE.0.)THEN
C         face 1 2 3 4
          III(I,5) = III(I,9)
          III(I,6) = III(I,10)
          III(I,7) = III(I,11)
          III(I,8) = III(I,12)
        ELSE
C         face 5 6 7 8
          III(I,1) = III(I,5)
          III(I,2) = III(I,6)
          III(I,3) = III(I,7)
          III(I,4) = III(I,8)
          III(I,5) = III(I,13)
          III(I,6) = III(I,14)
          III(I,7) = III(I,15)
          III(I,8) = III(I,16)
        ENDIF
      ENDDO
C-----------------------------------------------------------------------
C     calcul des vitesses de penetration
C-----------------------------------------------------------------------
      DO I=1,LLT
        VIT(I) = EM20
      ENDDO
      CALL I17VIT4(LLT       ,NINT   ,V      ,A      ,III    ,IIIS   ,
     2             NI_M      ,NI_S   ,NX     ,NY     ,NZ     ,VIT    ,
     3             ICONT(1,1),R_1M   ,T_1M   ,R_1S   ,T_CS   ,S_CS   ,
     4             R_CM      ,T_CM   ,R_S    ,T_S    ,ICONT(1,1))
      CALL I17VIT4(LLT       ,NINT   ,V      ,A      ,III    ,IIIS   ,
     2             NI_M      ,NI_S   ,NX     ,NY     ,NZ     ,VIT    ,
     3             ICONT(1,2),R_2M   ,T_2M   ,R_2S   ,T_CS   ,S_CS   ,
     4             R_CM      ,T_CM   ,R_S    ,T_S    ,ICONT(1,1))
      CALL I17VIT4(LLT       ,NINT   ,V      ,A      ,III    ,IIIS   ,
     2             NI_M      ,NI_S   ,NX     ,NY     ,NZ     ,VIT    ,
     3             ICONT(1,3),R_3M   ,T_3M   ,R_CS   ,T_1S   ,S_CS   ,
     4             R_CM      ,T_CM   ,R_S    ,T_S    ,ICONT(1,1))
      CALL I17VIT4(LLT       ,NINT   ,V      ,A      ,III    ,IIIS   ,
     2             NI_M      ,NI_S   ,NX     ,NY     ,NZ     ,VIT    ,
     3             ICONT(1,4),R_4M   ,T_4M   ,R_CS   ,T_2S   ,S_CS   ,
     4             R_CM      ,T_CM   ,R_S    ,T_S    ,ICONT(1,1))
c tmp !!!!!!!!!!!!!!!!!!!!!!!!!!! un seul contact
c      VV = 0.
c      DO I=1,LLT
c        VV = MIN(VV,VIT(I))
c      ENDDO
c      DO I=1,LLT
c        IF(VV.EQ.VIT(I))THEN
c          VV=1.
c        ELSE
c          ICONT(I,1)=0
c        ENDIF
c      ENDDO
c tmp !!!!!!!!!!!!!!!!!!!!!!!!!!!
c      DO I=1,LLT
c        PENE = 1. - ABS(S_CM(I))
c        IF(ICONT(I,1).EQ.1.AND.PENE.GT.PENE_MAX)THEN
c          PENE_MAX = PENE
c          NC = NC_SAV
c          J  = I
c        ENDIF
c        ICONT(I,1) = 0
c      ENDDO
c      ICONT(J,1) = 1
#include "lockon.inc"
      J=0
      DO I=1,LLT
        IF(VIT(I).LT.VIT_MIN.OR.
     .    (VIT(I).EQ.VIT_MIN.AND.
     .          (IE(I).LT.IE_MIN.OR.
     .          (IE(I).EQ.IE_MIN.AND.IES(I).LT.IES_MIN))))THEN
          VIT_MIN = VIT(I)
          IE_MIN  = IE(I)
          IES_MIN = IES(I)
          NC = NC_SAV
          J  = I
          ICON=ICONT(I,1)
        ENDIF
        ICONT(I,1) = 0
      ENDDO
      IF(J.NE.0)THEN
        ICONT(J,1) = ICON
cc	print *,ITASK,':',VIT(J),IE(J),IES(J),NC,J,ICON
      ENDIF
C-----------------------------------------------------------------------
C     calcul du contact
C-----------------------------------------------------------------------
      CALL I17LLL4(LLT       ,LLL    ,JLL    ,SLL    ,XLL    ,N_MUL_MX,
     2             ITIED     ,NINT   ,NKMAX  ,NC     ,V      ,A       ,
     3             IADLL     ,III    ,IIIS   ,NI_M   ,NI_S   ,
     4             NX        ,NY     ,NZ     ,VIT    ,COMNTAG,
     5             ICONT(1,1),R_CM   ,T_CM   ,R_S    ,T_S    )
c      IF(J.NE.0)THEN
c        do i=1,nc*16
c	print *,ITASK,':',i,':',LLL(i),JLL(i),XLL(i)
c	enddo
c      ENDIF
C
#include "lockoff.inc"
      RETURN
      END
Chd|====================================================================
Chd|  I17VIT4                       source/interfaces/int17/i17lagm.F
Chd|-- called by -----------
Chd|        I17LLL                        source/interfaces/int17/i17lagm.F
Chd|-- calls ---------------
Chd|        I17NI                         source/interfaces/int17/i17lagm.F
Chd|====================================================================
      SUBROUTINE I17VIT4(LLT,NINT ,V      ,A      ,III    ,IIIS   ,
     2             NI_M   ,NI_S   ,NX     ,NY     ,NZ     ,VIT    ,
     3             ICONT  ,RM     ,TM     ,RS     ,TS     ,SM     ,
     4             R_M    ,T_M    ,R_S    ,T_S    ,ICONTN )
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
#include      "com08_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LLT,NINT   
      INTEGER III(MVSIZ,17),IIIS(MVSIZ,16),
     +        ICONT(MVSIZ),ICONTN(MVSIZ)
C     REAL
      my_real
     .  V(3,*),A(3,*),VIT(*)
      my_real
     .   RM(MVSIZ) ,RS(MVSIZ) ,TM(MVSIZ) ,TS(MVSIZ) ,SM(MVSIZ),
     .   R_M(MVSIZ) ,R_S(MVSIZ) ,T_M(MVSIZ) ,T_S(MVSIZ) ,
     .   NI_M(MVSIZ,*) ,NI_S(MVSIZ,*),NX(MVSIZ) ,NY(MVSIZ) ,NZ(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IK,NK,I1,I2,I3,I4
      my_real
     .   VX,VY,VZ,VN,AA
      my_real
     .   NI_ML(MVSIZ,8) ,NI_SL(MVSIZ,8)
C-----------------------------------------------------------------------
C     calcul de Ni sur face  1 2 3 4 ou 5 6 7 8 du main (s=+-1)
C-----------------------------------------------------------------------
      CALL I17NI(LLT,RM ,TM ,NI_ML )
C-----------------------------------------------------------------------
C     calcul de Ni sur face  1 2 3 4 ou 5 6 7 8 de l'second (s=+-1)
C-----------------------------------------------------------------------
      CALL I17NI(LLT,RS ,TS ,NI_SL )
C-----------------------------------------------
C      calcul de [L]
C-----------------------------------------------
      DO I=1,LLT
C-----------------------------------------------
C       test si contact
C-----------------------------------------------
        IF(ICONT(I).NE.0)THEN
C
          VX = ZERO
          VY = ZERO
          VZ = ZERO
          DO IK=1,8
c            VX = VX - (V(1,III(I,IK))+DT12*A(1,III(I,IK)))*NI_M(I,IK)
c            VY = VY - (V(2,III(I,IK))+DT12*A(2,III(I,IK)))*NI_M(I,IK)
c            VZ = VZ - (V(3,III(I,IK))+DT12*A(3,III(I,IK)))*NI_M(I,IK)
c            VX = VX + (V(1,IIIS(I,IK))+DT12*A(1,IIIS(I,IK)))*NI_S(I,IK)
c            VY = VY + (V(2,IIIS(I,IK))+DT12*A(2,IIIS(I,IK)))*NI_S(I,IK)
c            VZ = VZ + (V(3,IIIS(I,IK))+DT12*A(3,IIIS(I,IK)))*NI_S(I,IK)
            VX = VX - (V(1,III(I,IK)))*NI_ML(I,IK)
            VY = VY - (V(2,III(I,IK)))*NI_ML(I,IK)
            VZ = VZ - (V(3,III(I,IK)))*NI_ML(I,IK)
            VX = VX + (V(1,IIIS(I,IK)))*NI_SL(I,IK)
            VY = VY + (V(2,IIIS(I,IK)))*NI_SL(I,IK)
            VZ = VZ + (V(3,IIIS(I,IK)))*NI_SL(I,IK)
          ENDDO
c
          VN = NX(I)*VX + NY(I)*VY + NZ(I)*VZ
C-----------------------------------------------
C         test si vitesse entrante en s
C-----------------------------------------------
          IF(SM(I)*VN.LE.VIT(I))THEN
c
c      print *, "vitesse entrante",vn
c      print *, "s = ",S(I)
c
            VIT(I)=SM(I)*VN
            R_S(I)=RS(I)
            R_M(I)=RM(I)
            T_S(I)=TS(I)
            T_M(I)=TM(I)
            DO IK=1,8
              NI_M(I,IK)=NI_ML(I,IK)
              NI_S(I,IK)=NI_SL(I,IK)
            ENDDO
            ICONTN(I)=1
          ENDIF
        ENDIF
      ENDDO
c
c      print *, "r,s,t",r(1),s(1),t(1)
C
      RETURN
      END
C
Chd|====================================================================
Chd|  I17LLL4                       source/interfaces/int17/i17lagm.F
Chd|-- called by -----------
Chd|        I17LLL                        source/interfaces/int17/i17lagm.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I17LLL4(LLT,LLL  ,JLL    ,SLL    ,XLL    ,N_MUL_MX,
     2             ITIED  ,NINT   ,NKMAX  ,NC     ,V      ,A      ,
     3             IADLL  ,III    ,IIIS   ,NI_M   ,NI_S   ,
     4             NX     ,NY     ,NZ     ,VIT    ,COMNTAG,
     5             ICONT  ,RM     ,TM     ,RS     ,TS     )
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
     .        III(MVSIZ,17),IADLL(*) ,IIIS(MVSIZ,16),
     +        ICONT(MVSIZ)
C     REAL
      my_real
     .  XLL(*),V(3,*),A(3,*),VIT(*)
      my_real
     .   RM(MVSIZ) ,RS(MVSIZ) ,TM(MVSIZ) ,TS(MVSIZ) ,
     .   NI_M(MVSIZ,*) ,NI_S(MVSIZ,*),NX(MVSIZ) ,NY(MVSIZ) ,NZ(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IK,NK,I1,I2,I3,I4,IAD,NN
      my_real
     .   VX,VY,VZ,VN,AA
C-----------------------------------------------
      IF(ITIED.EQ.0)THEN
       DO I=1,LLT
C-----------------------------------------------
C       test si contact
C-----------------------------------------------
        IF(ICONT(I).NE.0)THEN
C
           NK = 16
            NC=NC+1
            IF(NC.GT.N_MUL_MX)THEN
              CALL ANCMSG(MSGID=89,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
            IADLL(NC+1)=IADLL(NC) + 48
            IF(IADLL(NC+1)-1.GT.NKMAX)THEN
              CALL ANCMSG(MSGID=89,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
            IAD = IADLL(NC) - 1
            DO IK=1,8
                LLL(IAD+IK)    = III(I,IK)
                JLL(IAD+IK)    = 1
                SLL(IAD+IK)    = 0
                XLL(IAD+IK)    = NX(I)*NI_M(I,IK)
                LLL(IAD+IK+16) = III(I,IK)
                JLL(IAD+IK+16) = 2
                SLL(IAD+IK+16) = 0
                XLL(IAD+IK+16) = NY(I)*NI_M(I,IK)
                LLL(IAD+IK+32) = III(I,IK)
                JLL(IAD+IK+32) = 3
                SLL(IAD+IK+32) = 0
                XLL(IAD+IK+32) = NZ(I)*NI_M(I,IK)
C
                LLL(IAD+IK+8)  = IIIS(I,IK)
                JLL(IAD+IK+8)  = 1
                SLL(IAD+IK+8)  = NINT
                XLL(IAD+IK+8)  = -NX(I)*NI_S(I,IK)
                LLL(IAD+IK+24) = IIIS(I,IK)
                JLL(IAD+IK+24) = 2
                SLL(IAD+IK+24) = NINT
                XLL(IAD+IK+24) = -NY(I)*NI_S(I,IK)
                LLL(IAD+IK+40) = IIIS(I,IK)
                JLL(IAD+IK+40) = 3
                SLL(IAD+IK+40) = NINT
                XLL(IAD+IK+40) = -NZ(I)*NI_S(I,IK)
                NN = LLL(IAD+IK)
                COMNTAG(NN) = COMNTAG(NN) + 1
            ENDDO
c#include "lockoff.inc"
        ENDIF
       ENDDO
      ELSEIF(ITIED.EQ.1)THEN
C-----------------------------------------------
C      ITIED = 1
C-----------------------------------------------
       DO I=1,LLT
C-----------------------------------------------
C       test si contact
C-----------------------------------------------
        IF(ICONT(I).NE.0)THEN
C
           NK = 16
            IF(NC+3.GT.N_MUL_MX)THEN
              CALL ANCMSG(MSGID=89,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
            IF(IADLL(NC+1)-1+16*3.GT.NKMAX)THEN
              CALL ANCMSG(MSGID=89,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
C
            NC=NC+1
            IADLL(NC+1)=IADLL(NC) + 16
            IAD = IADLL(NC) - 1
            DO IK=1,8
                LLL(IAD+IK)   = III(I,IK)
                JLL(IAD+IK)   = 1
                SLL(IAD+IK)   = 0
                XLL(IAD+IK)   = NI_M(I,IK)
                LLL(IAD+IK+8) = IIIS(I,IK)
                JLL(IAD+IK+8) = 1
                SLL(IAD+IK+8) = NINT
                XLL(IAD+IK+8) = -NI_S(I,IK)
                NN = LLL(IAD+IK)
                COMNTAG(NN) = COMNTAG(NN) + 1
            ENDDO
C
            NC=NC+1
            IADLL(NC+1)=IADLL(NC) + 16
            IAD = IADLL(NC) - 1
            DO IK=1,8
                LLL(IAD+IK)   = III(I,IK)
                JLL(IAD+IK)   = 2
                SLL(IAD+IK)   = 0
                XLL(IAD+IK)   = NI_M(I,IK)
                LLL(IAD+IK+8) = IIIS(I,IK)
                JLL(IAD+IK+8) = 2
                SLL(IAD+IK+8) = NINT
                XLL(IAD+IK+8) = -NI_S(I,IK)
                NN = LLL(IAD+IK)
                COMNTAG(NN) = COMNTAG(NN) + 1
            ENDDO
C
            NC=NC+1
            IADLL(NC+1)=IADLL(NC) + 16
            IAD = IADLL(NC) - 1
            DO IK=1,8
                LLL(IAD+IK)   = III(I,IK)
                JLL(IAD+IK)   = 3
                SLL(IAD+IK)   = 0
                XLL(IAD+IK)   = NI_M(I,IK)
                LLL(IAD+IK+8) = IIIS(I,IK)
                JLL(IAD+IK+8) = 3
                SLL(IAD+IK+8) = NINT
                XLL(IAD+IK+8) = -NI_S(I,IK)
                NN = LLL(IAD+IK)
                COMNTAG(NN) = COMNTAG(NN) + 1
           ENDDO
c#include "lockoff.inc"
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
Chd|  I17RST                        source/interfaces/int17/i17lagm.F
Chd|-- called by -----------
Chd|        I17LLL                        source/interfaces/int17/i17lagm.F
Chd|        I17LLL_PENA                   source/interfaces/int17/i17for3.F
Chd|-- calls ---------------
Chd|        I16DERI                       source/interfaces/int16/i16lagm.F
Chd|        I16NI                         source/interfaces/int16/i16lagm.F
Chd|        I16RSTN                       source/interfaces/int16/i16lagm.F
Chd|====================================================================
      SUBROUTINE I17RST(LLT,R  ,S     ,T     ,NI    ,
     2            XX    ,YY    ,ZZ    )
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
     .   R(MVSIZ),S(MVSIZ),T(MVSIZ),NI(MVSIZ,17) 
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
         RR(I) = 0.
         SS(I) = 0.
         TT(I) = 0.
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
C
           ENDDO
      ENDDO
C
      RETURN
      END
C
Chd|====================================================================
Chd|  I17MINI                       source/interfaces/int17/i17lagm.F
Chd|-- called by -----------
Chd|        I17LLL                        source/interfaces/int17/i17lagm.F
Chd|-- calls ---------------
Chd|        I17ABC                        source/interfaces/int17/i17lagm.F
Chd|        I17BORNE                      source/interfaces/int17/i17lagm.F
Chd|        I17NORM                       source/interfaces/int17/i17lagm.F
Chd|        I17RACINE                     source/interfaces/int17/i17lagm.F
Chd|====================================================================
      SUBROUTINE I17MINI(LLT   ,R_CS  ,S_CS  ,T_CS  ,RI_S  ,SI_S  ,
     2                   TI_S  ,NI_S  ,XXS   ,YYS   ,ZZS   ,XX    ,
     3                   YY    ,ZZ    ,R_CM  ,S_CM  ,T_CM  ,NX    ,
     4                   NY    ,NZ    ,R_1S  ,R_2S  ,T_1S  ,T_2S  ,
     5                   R_1M  ,R_2M  ,R_3M  ,R_4M  ,T_1M  ,T_2M  ,
     6                   T_3M  ,T_4M  ,ICONT )
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
      INTEGER LLT,
     +        ICONT(MVSIZ,4)
C     REAL
      my_real
     + SI_S(MVSIZ,*),NI_S(MVSIZ,*),RI_S,TI_S ,
     + XXS(MVSIZ,*) ,YYS(MVSIZ,*) ,ZZS(MVSIZ,*) ,
     + XX(MVSIZ,*)  ,YY(MVSIZ,*)  ,ZZ(MVSIZ,*) ,
     + R_CM(MVSIZ) ,S_CM(MVSIZ) ,T_CM(MVSIZ),
     + R_CS(MVSIZ) ,S_CS(MVSIZ) ,T_CS(MVSIZ),
     + R_1S(MVSIZ) ,R_2S(MVSIZ) ,T_1S(MVSIZ) ,T_2S(MVSIZ),
     + R_1M(MVSIZ) ,R_2M(MVSIZ) ,T_1M(MVSIZ) ,T_2M(MVSIZ),
     + R_3M(MVSIZ) ,R_4M(MVSIZ) ,T_3M(MVSIZ) ,T_4M(MVSIZ),
     + NX(MVSIZ) ,NY(MVSIZ) ,NZ(MVSIZ)   
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ITER,NITERMAX
      my_real
     +        A1(MVSIZ),A2(MVSIZ),A3(MVSIZ),A4(MVSIZ),A5(MVSIZ),
     +        B1(MVSIZ),B2(MVSIZ),B3(MVSIZ),B4(MVSIZ),B5(MVSIZ),
     +        C1(MVSIZ),C2(MVSIZ),C3(MVSIZ),
     +        F1,F2,F3,F4,F5,F6,F7,F8,
     +        CC1,CC2,CC3,DD1,DD2,DD3,DD,D,
     +        A0,AB,BA,A4R,B4T,A5T,B5R,EPS
C-----------------------------------------------
C-----------------------------------------------
C ro = r ri    to = t ti
C
C i=1,4  
C    ri=+-1  ti=+-1
C    Ni = 1/4 (1+ro)(1+to)(ro+to-1)
C    Ni = 1/4 (r^2+r^2to+t^2+roto+rot^2-1)
C    Ni = 1/4 (r^2(1+to)+t^2(1+ro)+roto-1)
C    dNi/dr = ri/4 (1+to)(2ro+to)
C    dNi/dt = ti/4 (1+ro)(2to+ro)
C    dNi/dr = 1/4 (2 r + ri ti t + 2ti rt + ri t^2)
C    dNi/dt = 1/4 (2 t + ri ti r + 2ri rt + ti r^2)
C
C i=6;8
C    ri=0    ti=+-1
C    Ni = 1/2 (1-r^2)(1+to)
C    dNi/dr = -r (1+to)
C    dNi/dt = ti/2 (1-r^2)
C    dNi/dr = 1/4 (-4 r - 4ti r t)
C    dNi/dt = 1/4 (2ti - 2ti r^2 )
C
C
C i=5;7
C    ri=+-1  ti=0
C    Ni = 1/2 (1-t^2)(1+ro)
C    dNi/dr = ri/2 (1-t^2)
C    dNi/dt = -t (1+ro)
C    dNi/dr = 1/4 (2ri  - 2ri t^2 )
C    dNi/dt = 1/4 (-4 t - 4ri rt  )
C-----------------------------------------------
C  df/dr = Somme( fi dNi/dr )
C  df/dt = Somme( fi dNi/dt )
C
C  df/dr = A1 + A2 r + A3 t + A4 rt + A5 t^2
C  df/dt = B1 + B2 r + B3 t + B4 rt + B5 r^2
C 
C  A1 = (                      -f5          +   f7        )/2
C  A2 = ( f1 + f2 + f3 + f4         - 2 f6         - 2 f8)/2
C  A3 = ( f1 - f2 + f3 - f4                                )/4
C  A4 = (-f1 + f2 + f3 - f4         - 2 f6         + 2 f8)/2
C  A5 = (-f1 - f2 + f3 + f4 + 2 f5          - 2 f7        )/4
C
C  B1 = (                               f6         -   f8)/2
C  B2 = ( f1 - f2 + f3 - f4                                )/4
C  B3 = ( f1 + f2 + f3 + f4 - 2 f5          - 2 f7        )/2
C  B4 = (-f1 - f2 + f3 + f4 + 2 f5          - 2 f7        )/2
C  B5 = (-f1 + f2 + f3 - f4         - 2 f6         + 2 f8)/4
C
C  df/dr = A1 + A2 r + A3 t + A4 rt + A5 t^2 = 0
C  df/dt = B1 + B2 r + B3 t + B4 rt + B5 r^2 = 0
C  r = -(A1 + A3 t + A5 t^2 ) / (A2 + A4 t)
C  t = -(B1 + B2 r + B5 r^2 ) / (B3 + B4 r)
c
c
c                                r
C                                ^
c                                |
C                          .     |7    
C            4 O-----------------O-----------------O 3        
C              |           .     |                 |          
C              |           .     |                 |          
C              |                 |      C          |
C              |           .    r+------+df/dt=0   |  
C              |           .     |      |df/dr=0   |    
C              |                 |      |          |
C              |                 |      |          |   
C              |                 |      |          |6       
C            8 0                 +------+----------0----> t  
C              |                        t          |       
C              |                                   |      
C              |                                   |        
C              |                                   |      
C              |                                   |    
C              |                                   |    
C              |                                   |    
C              |'                                  |    
C              O-----------------O-----------------O    
C             1                  5                  2
C
C-----------------------------------------------
C
c
c
      NITERMAX = 5
C
      DO I=1,LLT
        D = SI_S(I,1)*SI_S(I,1)+SI_S(I,2)*SI_S(I,2)
     +    + SI_S(I,3)*SI_S(I,3)+SI_S(I,4)*SI_S(I,4)
     +    + SI_S(I,5)*SI_S(I,5)+SI_S(I,6)*SI_S(I,6)
     +    + SI_S(I,7)*SI_S(I,7)+SI_S(I,8)*SI_S(I,8)
        D = 1./MAX(EM20,SQRT(D))
        F1 = D * SI_S(I,1)
        F2 = D * SI_S(I,2)
        F3 = D * SI_S(I,3)
        F4 = D * SI_S(I,4)
        F5 = D * SI_S(I,5)
        F6 = D * SI_S(I,6)
        F7 = D * SI_S(I,7)
        F8 = D * SI_S(I,8)
        A0 = ( F1 + F2 + F3 + F4 )*HALF
        AB = F5 - F7
        BA = F6 - F8
C
        A1(I) = -AB * HALF
        A2(I) = A0 - F6 - F8
        A3(I) = ( F1 - F2 + F3 - F4 )*FOURTH
        A4(I) = (-F1 + F2 + F3 - F4 )*HALF  - BA
        A5(I) = (-F1 - F2 + F3 + F4 )*FOURTH - A1(I)
C
        B1(I) = BA*HALF
        B2(I) = A3(I)
        B3(I) = A0 - F5  - F7
        B4(I) = (-F1 - F2 + F3 + F4 )*HALF  + AB
        B5(I) = (-F1 + F2 + F3 - F4 )*FOURTH - B1(I)
c
        R_CS(I) = ZERO
        T_CS(I) = ZERO
      ENDDO
c------------------------------------------------
c   newton iter : lineaRI_Sation en r et t
c------------------------------------------------
C  fr = df/dr = A1 + A2 r + A3 t + A4 t r + A5 t t = 0
C  ft = df/dt = B1 + B2 r + B3 t + B4 r t + B5 r r = 0 
c
C  fr = fr_ + dfr/dr dr + dfr/dt dt
C  ft = ft_ + dft/dr dr + dft/dt dt
c
C  fr = fr_ + dfr/dr (r-r_) + dfr/dt (t-t_) = 0
C  ft = ft_ + dft/dr (r-r_) + dft/dt (t-t_) = 0
c
C  dfr/dr = A2 + A4 t_ = C2
C  dfr/dt = A3 + A4 r_ + 2 A5 t_ = C3
C  dft/dr = B2 + B4 t_ + 2 B5 r_ = D2
C  dft/dt = B3 + B4 r_ = D3
c
c  C1 = A1 - A4 r_ t_ - A5 t_^2
c  C2 = A2 + A4 t_
c  C3 = A3 + A4 r_ + 2 A5 t_
c
c  D1 = B1 - B4 r_ t_ - B5 r_^2
c  D2 = B2 + B4 t_ + 2 B5 r_
c  D3 = B3 + B4 r_ 
c
C  fr = C1 + C2 r + C3 t = 0
C  ft = D1 + D2 r + D3 t = 0
c
C  r = (C3 D1 - D3 C1) / (D3 C2 - C3 D2) 
C  t = (D2 C1 - C2 D1) / (D3 C2 - C3 D2)
c------------------------------------------------
      DO ITER=1,NITERMAX
        DO I=1,LLT
          A4R = A4(I) * R_CS(I)
          A5T = A5(I) * T_CS(I)
          B4T = B4(I) * T_CS(I)
          B5R = B5(I) * R_CS(I)
          CC1  = A1(I) -(A4R + A5T) * T_CS(I)
          CC2  = A2(I) + A4(I) * T_CS(I)
          CC3  = A3(I) + A4R + A5T + A5T
          DD1  = B1(I) -(B4T + B5R )* R_CS(I)
          DD2  = B2(I) + B4T + B5R + B5R
          DD3  = B3(I) + B4(I) * R_CS(I)
          D    = DD3 * CC2 - CC3 * DD2 
          IF(ABS(D).LT.EM20)THEN
            CC2 = CC2 + EM10
            DD3 = DD3 + EM10
            D   = DD3 * CC2 - CC3 * DD2 
          ENDIF
          R_CS(I) = (CC3 * DD1 - DD3 * CC1) / D
          T_CS(I) = (DD2 * CC1 - CC2 * DD1) / D
          R_CS(I) = MAX(-ONE,MIN(ONE,R_CS(I)))
          T_CS(I) = MAX(-ONE,MIN(ONE,T_CS(I)))
c          dfdr = A1 + A2 * r + A3 * t + A4 * t * r + A5 * t * t 
c          dfdt = B1 + B2 * r + B3 * t + B4 * r * t + B5 * r * r 
c          print *,'it=',iter,' r=',r,' t=',t,' fr=',dfdr,' ft=',dfdt
        ENDDO
      ENDDO
C-----------------------------------------------------------------------
C     calcul de 4 points de l'iso S_M = +- 1
C 
C          second.           main
C     1: <R_1S , T_CS>    <R_1M , T_1M>
C     2: <R_2S , T_CS>    <R_2M , T_2M>
C     3: <R_CS , T_1S>    <R_3M , T_3M>
C     4: <R_CS , T_2S>    <R_4M , T_4M> 
C
C     on borne r et t a +- 1 sur l'element second. :
C                -1 < R_1S < 1  et -1 < R_2S < 1 
C                -1 < T_1S < 1  et -1 < T_2S < 1 
c
C     on borne r et t a +- 1 sur l'element main :
C                -1 < R_M < 1   et -1 < T_M < 1 
C-----------------------------------------------------------------------
      CALL  I17ABC(LLT ,SI_S,R_CS,T_CS,
     +             B1  ,B2  ,B3  ,C1  ,C2  ,C3  )
C
      DO I=1,LLT
        S_CM(I) = B1(I) + B2(I)*R_CS(I) + B3(I)*R_CS(I)*R_CS(I)
      ENDDO
c
      DO I=1,LLT
          IF(S_CM(I).GT.ZERO)THEN
            B1(I) = B1(I) - ONE
            C1(I) = C1(I) - ONE
          ELSE
            B1(I) = B1(I) + ONE
            C1(I) = C1(I) + ONE
          ENDIF
      ENDDO
c
C  f = B1 + B2 r + B3 r^2
c
      CALL I17RACINE(LLT,B3,B2,B1,R_1S,R_2S)

C  f = C1 + C2 t + C3 t^2
C
      CALL I17RACINE(LLT,C3,C2,C1,T_1S,T_2S)
c-----------------------------------------------------------------------
c      borne r a +-1 sur le main
c-----------------------------------------------------------------------
C
      CALL  I17ABC(LLT ,RI_S,R_CS,T_CS,
     +             B1  ,B2  ,B3  ,C1  ,C2  ,C3  )
C
      DO I=1,LLT
        R_CM(I) = B1(I) + (B2(I) + B3(I)*R_CS(I))*R_CS(I)
        ICONT(I,1) = 1
        ICONT(I,2) = 1
        ICONT(I,3) = 1
        ICONT(I,4) = 1
      ENDDO
C
c      CALL I17BORNE(LLT ,R_CS,B3  ,B2  ,B1  ,ICONT(1,1),R_CM)
c      CALL I17BORNE(LLT ,T_CS,C3  ,C2  ,C1  ,ICONT(1,1),R_CM)
      CALL I17BORNE(LLT ,R_1S,B3  ,B2  ,B1  ,ICONT(1,1),R_1M)
      CALL I17BORNE(LLT ,R_2S,B3  ,B2  ,B1  ,ICONT(1,2),R_2M)
      CALL I17BORNE(LLT ,T_1S,C3  ,C2  ,C1  ,ICONT(1,3),R_3M)
      CALL I17BORNE(LLT ,T_2S,C3  ,C2  ,C1  ,ICONT(1,4),R_4M)
c-----------------------------------------------------------------------
c  2: borne t a +-1 sur le main 
c-----------------------------------------------------------------------
      CALL  I17ABC(LLT ,TI_S,R_CS,T_CS,
     +             B1  ,B2  ,B3  ,C1  ,C2  ,C3  )
C
      DO I=1,LLT
        T_CM(I) = B1(I) + (B2(I) + B3(I)*R_CS(I))*R_CS(I)
      ENDDO
C
c      CALL I17BORNE(LLT ,R_CS,B3  ,B2  ,B1  ,ICONT(1,1),T_CM)
c      CALL I17BORNE(LLT ,T_CS,C3  ,C2  ,C1  ,ICONT(1,1),T_CM)
      CALL I17BORNE(LLT ,R_1S,B3  ,B2  ,B1  ,ICONT(1,1),T_1M)
      CALL I17BORNE(LLT ,R_2S,B3  ,B2  ,B1  ,ICONT(1,2),T_2M)
      CALL I17BORNE(LLT ,T_1S,C3  ,C2  ,C1  ,ICONT(1,3),T_3M)
      CALL I17BORNE(LLT ,T_2S,C3  ,C2  ,C1  ,ICONT(1,4),T_4M)
c-----------------------------------------------------------------------
c  test si contact
c-----------------------------------------------------------------------
      CALL  I17ABC(LLT ,SI_S,R_CS,T_CS,
     +             B1  ,B2  ,B3  ,C1  ,C2  ,C3  )
      EPS = EM3
      DO I=1,LLT
        D = B1(I) + (B2(I) + B3(I)*R_1S(I)) * R_1S(I)
        IF(D.LT.-ONE-EPS.OR.D.GT.ONE+EPS) ICONT(I,1)=0
        D = B1(I) + (B2(I) + B3(I)*R_2S(I)) * R_2S(I)
        IF(D.LT.-ONE-EPS.OR.D.GT.ONE+EPS) ICONT(I,2)=0
        D = C1(I) + (C2(I) + C3(I)*T_1S(I)) * T_1S(I)
        IF(D.LT.-ONE-EPS.OR.D.GT.ONE+EPS) ICONT(I,3)=0
        D = C1(I) + (C2(I) + C3(I)*T_2S(I)) * T_2S(I)
        IF(D.LT.-ONE-EPS.OR.D.GT.ONE+EPS) ICONT(I,4)=0
c        R_CS(I) = ICONT(I,1)*R_1S(I) + ICONT(I,2)*R_2S(I) +  
c     +            ICONT(I,3)*R_CS(I) + ICONT(I,4)*R_CS(I)
c        T_CS(I) = ICONT(I,1)*T_CS(I) + ICONT(I,2)*T_CS(I) +  
c     +            ICONT(I,3)*T_1S(I) + ICONT(I,4)*T_2S(I)
cc        R_CM(I) = ICONT(I,1)*R_1M(I) + ICONT(I,2)*R_2M(I) +  
cc     +            ICONT(I,3)*R_3M(I) + ICONT(I,4)*R_4M(I)
cc        T_CM(I) = ICONT(I,1)*T_1M(I) + ICONT(I,2)*T_2M(I) +  
cc     +            ICONT(I,3)*T_3S(I) + ICONT(I,4)*T_4S(I)
c        ICONT(I,1) = ICONT(I,1) + ICONT(I,2) +  
c     +               ICONT(I,3) + ICONT(I,4)
c        IF(ICONT(I,1).NE.0)THEN
c          R_CS(I) = R_CS(I) / ICONT(I,1) 
c          T_CS(I) = T_CS(I) / ICONT(I,1) 
c          ICONT(I,1) = 1 
c        ENDIF
      ENDDO
C
      CALL  I17ABC(LLT ,SI_S,R_CS,T_CS,
     +             B1  ,B2  ,B3  ,C1  ,C2  ,C3  )
      DO I=1,LLT
        S_CM(I) = B1(I) + (B2(I) + B3(I)*R_CS(I))*R_CS(I)
      ENDDO
C
      CALL  I17ABC(LLT ,RI_S,R_CS,T_CS,
     +             B1  ,B2  ,B3  ,C1  ,C2  ,C3  )
      DO I=1,LLT
        R_CM(I) = B1(I) + (B2(I) + B3(I)*R_CS(I))*R_CS(I)
      ENDDO
C
      CALL  I17ABC(LLT ,TI_S,R_CS,T_CS,
     +             B1  ,B2  ,B3  ,C1  ,C2  ,C3  )
      DO I=1,LLT
        T_CM(I) = B1(I) + (B2(I) + B3(I)*R_CS(I))*R_CS(I)
      ENDDO
C-----------------------------------------------
C          calcul de Ni(r,s,t); dNi/dr; dNi/dt; normale second(-n)
C-----------------------------------------------
      CALL I17NORM(LLT  ,R_CS  ,S_CS  ,T_CS  ,
     2             NX   ,NY    ,NZ    ,XXS   ,YYS   ,ZZS   )
c-----------------------------------------------------------------------
c  
c-----------------------------------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  I17NI                         source/interfaces/int17/i17lagm.F
Chd|-- called by -----------
Chd|        I17VIT4                       source/interfaces/int17/i17lagm.F
Chd|        I17VIT_PENA                   source/interfaces/int17/i17for3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I17NI(LLT,R ,T ,NI    )
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
     .   R(MVSIZ),T(MVSIZ),NI(MVSIZ,*)  
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
C
        R05 = HALF*R(I)
        T05 = HALF*T(I)
C
        U_M_R = HALF - R05
        U_P_R = HALF + R05
C
        U_M_T = HALF - T05
        U_P_T = HALF + T05
C
        NI(I,1) = U_M_T * U_M_R * (-R(I)-T(I)-ONE)
        NI(I,2) = U_P_T * U_M_R * (-R(I)+T(I)-ONE)
        NI(I,3) = U_P_T * U_P_R * ( R(I)+T(I)-ONE)
        NI(I,4) = U_M_T * U_P_R * ( R(I)-T(I)-ONE)	
C------------------------------------
        A = (ONE-R(I)*R(I))
        NI(I,6) = A * U_P_T
        NI(I,8) = A * U_M_T
C------------------------------------
        A = (ONE-T(I)*T(I))
        NI(I,5) = A * U_M_R
        NI(I,7) = A * U_P_R
C------------------------------------
      ENDDO
C-----------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  I17RACINE                     source/interfaces/int17/i17lagm.F
Chd|-- called by -----------
Chd|        I17BORNE                      source/interfaces/int17/i17lagm.F
Chd|        I17BORNE_PENA                 source/interfaces/int17/i17for3.F
Chd|        I17MINI                       source/interfaces/int17/i17lagm.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I17RACINE(LLT,A,B,C,R1,R2)
C-----------------------------------------------
C
C    calcul des racines r1,r2 bornees a +- 1
C
C    de  a x^2 + b x + c = 0
C 
C    la routine retourne -1,+1 s'il n'y a pas de racines
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
     .   C(MVSIZ),B(MVSIZ),A(MVSIZ),R1(MVSIZ),R2(MVSIZ) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .  D      
c
C
      DO I=1,LLT
        D =  B(I)*B(I) - FOUR*A(I)*C(I) 
        IF(ABS(A(I)).LT.EM20)THEN
          IF(ABS(B(I)).LT.EM20)THEN
            R1(I) = -ONE
            R2(I) =  ONE
          ELSE
            R1(I) = -C(I) / B(I)
            R2(I) = B(I) / ABS(B(I))
          ENDIF
        ELSEIF(D.LT.ZERO)THEN
            R1(I) = -ONE
            R2(I) =  ONE	    
        ELSE
            D = SQRT( D )
            R1(I) = (-B(I) - D) / (TWO*A(I))
            R2(I) = (-B(I) + D) / (TWO*A(I))
        ENDIF
      ENDDO
c-----------------------------------------------------------------------
c      borne r (ou t) a +-1 sur l'second
c-----------------------------------------------------------------------
      DO I=1,LLT
          R1(I) = MAX(-ONE,MIN(ONE,R1(I)))
          R2(I) = MAX(-ONE,MIN(ONE,R2(I)))
      ENDDO
c
      RETURN
      END
Chd|====================================================================
Chd|  I17BORNE                      source/interfaces/int17/i17lagm.F
Chd|-- called by -----------
Chd|        I17MINI                       source/interfaces/int17/i17lagm.F
Chd|-- calls ---------------
Chd|        I17RACINE                     source/interfaces/int17/i17lagm.F
Chd|====================================================================
      SUBROUTINE I17BORNE(LLT ,R_S ,A   ,B  ,C  ,ICONT,RS)
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
      INTEGER LLT,ICONT(MVSIZ)
      my_real
     +        R_S(MVSIZ),C(MVSIZ),B(MVSIZ),A(MVSIZ),RS(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     +   CC(MVSIZ),R1(MVSIZ),R2(MVSIZ),RS1,RS2
c-----------------------------------------------------------------------
c  1: borne r(ou s au 2eme appel) a +-1 sur le main
c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
c  1.1: borne <r1,t> et <r,t1> a +-1 sur le main
c-----------------------------------------------------------------------
        DO I=1,LLT
          RS(I) = C(I) + ( B(I) + A(I)*R_S(I) )*R_S(I)
          IF(RS(I).GT.ONE)THEN
            CC(I) = C(I) - ONE
          ELSEIF(RS(I).LT.-ONE)THEN
            CC(I) = C(I) + ONE
          ELSE 
            CC(I) = ONE
          ENDIF
        ENDDO
c
C  f = C + B r + A r^2
c
        CALL I17RACINE(LLT,A,B,CC,R1,R2)
c
        DO I=1,LLT
          IF(RS(I).GT.ONE.OR.RS(I).LT.-ONE)THEN
            RS1 = C(I) + ( B(I) + A(I)*R1(I) )*R1(I)
            RS2 = C(I) + ( B(I) + A(I)*R2(I) )*R2(I)
            IF(RS1.GE.-ONE.AND.RS1.LE.ONE.AND.
     +         RS2.GE.-ONE.AND.RS2.LE.ONE)THEN
              IF(ABS(RS(I)-R1(I)).LT.ABS(RS(I)-R2(I)))THEN
                R_S(I) = R1(I)
                RS(I)  = RS1
              ELSE
                R_S(I) = R2(I)
                RS(I)  = RS2
              ENDIF
            ELSEIF(RS1.GE.-ONE.AND.RS1.LE.ONE)THEN
                R_S(I) = R1(I)
                RS(I)  = RS1
            ELSEIF(RS2.GE.-ONE.AND.RS2.LE.ONE)THEN
                R_S(I) = R2(I)
                RS(I)  = RS2
            ELSE
C pas de recouvrement main/second
                ICONT(I)=0
            ENDIF
          ENDIF
        ENDDO
c
      RETURN
      END
Chd|====================================================================
Chd|  I17ABC                        source/interfaces/int17/i17lagm.F
Chd|-- called by -----------
Chd|        I17MINI                       source/interfaces/int17/i17lagm.F
Chd|        I17MINI_PENA                  source/interfaces/int17/i17for3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I17ABC(LLT ,F   ,R   ,T   ,
     +                  B1  ,B2  ,B3  ,C1  ,C2  ,C3  )
C-----------------------------------------------
C i=1,4  
C    ri=+-1  ti=+-1
C    Ni = 1/4 (1+ro)(1+to)(ro+to-1)
C    Ni = 1/4 (r^2 + ti r^2 t + t^2 + ri ti r t + ri r t^2 - 1)
C
C i=6;8
C    ri=0    ti=+-1
C    Ni = 1/2 ( 1 + ti t - r^2 - ti t r^2)
C
C
C i=5;7
C    ri=+-1  ti=0
C    Ni = 1/2 (1 + ri r - t^2 - ri r t^2)
C-----------------------------------------------
C  f = Somme( fi Ni )
C
C  f = A1 + A2 r + A3 t + A4 rt + A5 r^2 + A6 t^2 + A7 tr^2 + A8 rt^2
C  f = (A1 + A3 t + A6 t^2) + (A2 + A4 t + A8 t^2)r + (A5 + A7 t)r^2
C  f = B1 + B2 r + B3 r^2
C  f = (A1 + A2 r + A5 r^2) + (A3 + A4 r + A7 r^2)t + (A6 + A8 r)t^2
C  f = C1 + C2 t + C3 t^2
C 
C  A1 = (-f1 - f2 - f3 - f4 + 2 f5  + 2 f6  + 2 f7 + 2 f8)/4
C  A2 = (                   -   f5          +   f7       )/2
C  A3 = (                           +   f6         -   f8)/2
C  A4 = (+f1 - f2 + f3 - f4                              )/4
C  A5 = (+f1 + f2 + f3 + f4         - 2 f6         - 2 f8)/4                     
C  A6 = (+f1 + f2 + f3 + f4 - 2 f5         - 2 f7        )/4   
C  A7 = (-f1 + f2 + f3 - f4         - 2 f6         + 2 f8)/4   
C  A8 = (-f1 - f2 + f3 + f4 + 2 f5         - 2 f7        )/4  
C-----------------------------------------------
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
     +        F(MVSIZ,*),R(MVSIZ),T(MVSIZ),
     +        B1(MVSIZ),B2(MVSIZ),B3(MVSIZ),
     +        C1(MVSIZ),C2(MVSIZ),C3(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     +        A1,A2,A3,A4,A5,A6,A7,A8,R2,T2,FF5,FF6,FF7,FF8
C
      DO I=1,LLT
        FF5 = F(I,5) + F(I,5)
        FF6 = F(I,6) + F(I,6)
        FF7 = F(I,7) + F(I,7)
        FF8 = F(I,8) + F(I,8)
c
c        A1 = (-F(I,1)-F(I,2)-F(I,3)-F(I,4)+FF5+FF6+FF7+FF8)*0.25 ...
c        B1(I) = A1 + ( A3 + A6 * T(I) ) *T(I) ...
c
        A1 = (-F(I,1)-F(I,2)-F(I,3)-F(I,4)+FF5+FF6+FF7+FF8)
        A2 = (                            -FF5    +FF7    )
        A3 = (                                +FF6    -FF8)
        A4 = (+F(I,1)-F(I,2)+F(I,3)-F(I,4)                )
        A5 = (+F(I,1)+F(I,2)+F(I,3)+F(I,4)    -FF6    -FF8)
        A6 = (+F(I,1)+F(I,2)+F(I,3)+F(I,4)-FF5    -FF7    ) 
        A7 = (-F(I,1)+F(I,2)+F(I,3)-F(I,4)    -FF6    +FF8) 
        A8 = (-F(I,1)-F(I,2)+F(I,3)+F(I,4)+FF5    -FF7    )
c
c
        B1(I) = ( A1 + ( A3 + A6 * T(I) ) *T(I) )*FOURTH
        B2(I) = ( A2 + ( A4 + A8 * T(I) ) *T(I) )*FOURTH
        B3(I) = ( A5 + A7 * T(I)                )*FOURTH
c
        C1(I) = ( A1 + ( A2 + A5 * R(I) ) *R(I) )*FOURTH
        C2(I) = ( A3 + ( A4 + A7 * R(I) ) *R(I) )*FOURTH
        C3(I) = ( A6 + A8 * R(I)                )*FOURTH
      ENDDO
c
      RETURN
      END
Chd|====================================================================
Chd|  I17NORM                       source/interfaces/int17/i17lagm.F
Chd|-- called by -----------
Chd|        I17MINI                       source/interfaces/int17/i17lagm.F
Chd|        I17MINI_PENA                  source/interfaces/int17/i17for3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I17NORM(LLT ,RR  ,SS  ,TT    ,
     2                   NX  ,NY  ,NZ  ,XX    ,YY    ,ZZ    )
C-----------------------------------------------
C          calcul de Ni(r,t); dNi/dr; dNi/dt; normale
C-----------------------------------------------
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
     .   XX(MVSIZ,*) ,YY(MVSIZ,*),ZZ(MVSIZ,*),
     .   NX(MVSIZ) ,NY(MVSIZ) ,NZ(MVSIZ),
     .   RR(MVSIZ) ,SS(MVSIZ) ,TT(MVSIZ)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N
      my_real
     .   DXDR(MVSIZ), DYDR(MVSIZ), DZDR(MVSIZ),
     .   DXDT(MVSIZ), DYDT(MVSIZ), DZDT(MVSIZ),
     .   DNIDR(8),DNIDS(8),DNIDT(8)
      my_real
     .  U_M_R,U_P_R,U_M_S,U_P_S,U_M_T,U_P_T,
     .  UMS_UMT,UMS_UPT,UPS_UMT,UPS_UPT,
     .  UMR_UMS,UMR_UPS,UPR_UMS,UPR_UPS,
     .  UMT_UMR,UMT_UPR,UPT_UMR,UPT_UPR,
     .  A,R05,S05,T05,AA
C-----------------------------------------------------------------------
C     Ni; dNi/dr; dNi/ds; dNi/dt  s=+-1
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
        A = -T05 - RR(I)
        DNIDR(1) = (-UMS_UMT - UPS_UMT) * A
        A =  T05 - RR(I)
        DNIDR(2) = (-UMS_UPT - UPS_UPT) * A
        A =  T05 + RR(I)
        DNIDR(3) =  (UMS_UPT + UPS_UPT) * A
        A = -T05 + RR(I)
        DNIDR(4) =  (UMS_UMT + UPS_UMT) * A
C
        A = -R05 - TT(I)
        DNIDT(1) = (-UMR_UMS - UMR_UPS) * A
        A = -R05 + TT(I)
        DNIDT(2) =  (UMR_UMS + UMR_UPS) * A
        A = +R05 + TT(I)
        DNIDT(3) =  (UPR_UMS + UPR_UPS) * A
        A = +R05 - TT(I)
        DNIDT(4) = (-UPR_UMS - UPR_UPS) * A
C------------------------------------
       A = HALF*(ONE-RR(I)*RR(I))
        DNIDT(6) =  A * (U_M_S + U_P_S)
C    
        A = -TWO*RR(I)
        DNIDR(6) = A * (UMS_UPT + UPS_UPT)
        DNIDR(8) = A * (UMS_UMT + UPS_UMT)
C------------------------------------
        A = HALF*(ONE-TT(I)*TT(I))
        DNIDR(5)  = -A * (U_M_S + U_P_S)
C
        A = -TWO*TT(I)
        DNIDT(5) = A * (UMR_UMS + UMR_UPS)
        DNIDT(7) = A * (UPR_UMS + UPR_UPS)
C-----------------------------------------------------------------------
C     dx/dr; dx/dt
C-----------------------------------------------------------------------
        DXDR(I) = DNIDR(1)*XX(I,1) + DNIDR(2)*XX(I,2) + DNIDR(3)*XX(I,3)
     +          + DNIDR(4)*XX(I,4)   
     +    + DNIDR(5)*(XX(I,5) - XX(I,7)) + DNIDR(6)*XX(I,6)
     +    + DNIDR(8)*XX(I,8) 
C  
        DXDT(I) = DNIDT(1)*XX(I,1) + DNIDT(2)*XX(I,2) + DNIDT(3)*XX(I,3)
     +          + DNIDT(4)*XX(I,4)   
     +    + DNIDT(5)*XX(I,5)   + DNIDT(6)*(XX(I,6) - XX(I,8))
     +    + DNIDT(7)*XX(I,7)
C-----------------------------------------------------------------------
C     dy/dr; dy/dt
C-----------------------------------------------------------------------
        DYDR(I) = DNIDR(1)*YY(I,1) + DNIDR(2)*YY(I,2) + DNIDR(3)*YY(I,3)
     +          + DNIDR(4)*YY(I,4)   
     +    + DNIDR(5)*(YY(I,5) - YY(I,7))   + DNIDR(6)*YY(I,6)
     +    + DNIDR(8)*YY(I,8) 
C  
        DYDT(I) = DNIDT(1)*YY(I,1) + DNIDT(2)*YY(I,2) + DNIDT(3)*YY(I,3)
     +          + DNIDT(4)*YY(I,4)   
     +    + DNIDT(5)*YY(I,5)   + DNIDT(6)*(YY(I,6) - YY(I,8))
     +    + DNIDT(7)*YY(I,7)
C-----------------------------------------------------------------------
C     dz/dr; dz/dt
C-----------------------------------------------------------------------
        DZDR(I) = DNIDR(1)*ZZ(I,1) + DNIDR(2)*ZZ(I,2) + DNIDR(3)*ZZ(I,3)
     +          + DNIDR(4)*ZZ(I,4)   
     +    + DNIDR(5)*(ZZ(I,5) - ZZ(I,7))   + DNIDR(6)*ZZ(I,6)
     +    + DNIDR(8)*ZZ(I,8) 
C  
        DZDT(I) = DNIDT(1)*ZZ(I,1) + DNIDT(2)*ZZ(I,2) + DNIDT(3)*ZZ(I,3)
     +          + DNIDT(4)*ZZ(I,4)   
     +    + DNIDT(5)*ZZ(I,5)   + DNIDT(6)*(ZZ(I,6) - ZZ(I,8))
     +    + DNIDT(7)*ZZ(I,7)
C-----------------------------------------------------------------------
C     calcul de la normale -n
C-----------------------------------------------------------------------
        NX(I) = -DYDT(I)*DZDR(I) + DZDT(I)*DYDR(I)
        NY(I) = -DZDT(I)*DXDR(I) + DXDT(I)*DZDR(I)
        NZ(I) = -DXDT(I)*DYDR(I) + DYDT(I)*DXDR(I)
C
        AA = ONE/SQRT(NX(I)*NX(I)+NY(I)*NY(I)+NZ(I)*NZ(I))
        NX(I) = NX(I)*AA
        NY(I) = NY(I)*AA
        NZ(I) = NZ(I)*AA
      ENDDO
c
      RETURN
      END
