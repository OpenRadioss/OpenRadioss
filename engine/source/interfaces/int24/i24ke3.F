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
Chd|  I24KE3                        source/interfaces/int24/i24ke3.F
Chd|-- called by -----------
Chd|        IMP_INT_K                     source/implicit/imp_int_k.F   
Chd|-- calls ---------------
Chd|        ASSEM_INT                     source/implicit/assem_int.F   
Chd|        ASS_SPMD                      source/implicit/assem_int.F   
Chd|        FFIZERO                       source/interfaces/int07/i7ke3.F
Chd|        I24CORKM                      source/interfaces/int24/i24cork3.F
Chd|        I24KEG3                       source/interfaces/int24/i24ke3.F
Chd|        I24KGEO3                      source/interfaces/int24/i24ke3.F
Chd|        IMP_INTBUFDEF                 share/modules/imp_mod_def.F   
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I24KE3( A     ,V      ,MS   ,
     1                   IPARI ,INTBUF_TAB,X    ,NIN   ,
c     2                   NUM_IMP,CAND_N,CAND_E     ,IND_SUBT,
     3                   IDDL  ,K_DIAG ,K_LT ,IADK ,JDIK  ,
     4                   GAP_IMP,LREM  ,INTBUF_TAB_IMP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MOD_NODNORM
      USE MOD_RCURV
      USE INTBUFDEF_MOD
      USE IMP_INTBUFDEF
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "scr04_c.inc"
#include      "impl2_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,NINTER)
      INTEGER NIN,IDDL(*),IADK(*)     ,JDIK(*),LREM
C     REAL
      my_real 
     .  A(3,*), MS(*), V(3,*),X(*),K_DIAG(*),K_LT(*) 
      my_real 
     .        GAP_IMP
C     REAL

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
      TYPE(IMP_INTBUF_STRUCT_) INTBUF_TAB_IMP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,I_STOK, JLT_NEW, JLT , NFT, IVIS2,
     .        IBC, NOINT, NSEG, ISECIN, IBAG, IADM,
     .        IGAP, INACTI, IFQ, MFROT, IGSTI, NISUB,
     .        NB_LOC, I_STOK_LOC,DEBUT,
     .        ILAGM, LENR, LENT, MAXCC,INTTH,IFORM,INTKG,
     .        IDNJ,IDHJ
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        NSVG(MVSIZ),KINI(MVSIZ),IXX(MVSIZ,13),ITRIV(4,MVSIZ) 
C     REAL
      my_real
     .   STARTT, FRIC, GAP, STOPT,
     .   VISC,VISCF,STIGLO,GAPMIN,
     .   KMIN, KMAX, GAPMAX,RSTIF,FHEAT,TINT,RHOH,EPS
C-----------------------------------------------
C     REAL
      my_real
     .     NX1(MVSIZ), NX2(MVSIZ), NX3(MVSIZ), NX4(MVSIZ),
     .     NY1(MVSIZ), NY2(MVSIZ), NY3(MVSIZ), NY4(MVSIZ),
     .     NZ1(MVSIZ), NZ2(MVSIZ), NZ3(MVSIZ), NZ4(MVSIZ),
     .     SUBTRIA(MVSIZ), 
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ),
     .   VX1(MVSIZ), VX2(MVSIZ), VX3(MVSIZ), VX4(MVSIZ),
     .   VY1(MVSIZ), VY2(MVSIZ), VY3(MVSIZ), VY4(MVSIZ),
     .   VZ1(MVSIZ), VZ2(MVSIZ), VZ3(MVSIZ), VZ4(MVSIZ),
     .   VXI(MVSIZ), VYI(MVSIZ), VZI(MVSIZ), 
     .   N1(MVSIZ), N2(MVSIZ), N3(MVSIZ), PENE(MVSIZ),
     .   H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ),
     .   NM1(MVSIZ), NM2(MVSIZ), NM3(MVSIZ), 
     .   GAPV(MVSIZ),MSI(MVSIZ),GAPS(MVSIZ),
c     .   TEMPI(MVSIZ),PHI(MVSIZ),AREASI(MVSIZ),
c     .     XX0(MVSIZ,17),YY0(MVSIZ,17),ZZ0(MVSIZ,17),
c     .     VX(MVSIZ,17),VY(MVSIZ,17),VZ(MVSIZ,17),
     .    KI11(9,MVSIZ),KJ11(9,MVSIZ),OFF(MVSIZ),
     .    KK11(9,MVSIZ),KL11(9,MVSIZ),KI12(9,MVSIZ),
     .    KJ12(9,MVSIZ),KK12(9,MVSIZ),KL12(9,MVSIZ),
     .    LL_SL(MVSIZ),LL_ML(MVSIZ)
      INTEGER NRTMFT, NRTMLT, NMNFT, NMNLT, NRADM
      INTEGER ICURV,INTKG1
      INTEGER, DIMENSION(:),ALLOCATABLE :: TAG_S,TAG_M
C---------------------------------------------------- 
C	Calcul des adresses des buffers d'interfaces. 
C	Les adresses des buffers J10-JFI et K10-KFI 
C	sont remplaces systematiquement par des tableaux
C	JD(i) et KD(i), en gardant les memes numeros d'indexes.
C	Les anciens adresses directs Jn, Kn sont modifies
C	dans la routine sans commentaires additionnels habituels
C----------------------------------------------------
C
      NSN   =IPARI(5,NIN)
      NTY   =IPARI(7,NIN)
C      IBC   =IPARI(11,NIN)
C      IVIS2 =IPARI(14,NIN)
      IF(IPARI(33,NIN)==1) RETURN
      NOINT =IPARI(15,NIN)
      IGAP  =IPARI(21,NIN)
C      INACTI=IPARI(22,NIN)
C      ISECIN=IPARI(28,NIN)
      MFROT =IPARI(30,NIN)
      IFQ =IPARI(31,NIN) 
      IBAG =IPARI(32,NIN) 
      IGSTI=IPARI(34,NIN)
      NISUB =IPARI(36,NIN)
      ICURV =IPARI(39,NIN)
      INTKG =IPARI(65,NIN)
C adaptive meshing
C      IADM =IPARI(44,NIN) 
C      NRADM=IPARI(49,NIN)
C      PADM =INTBUF_TAB%VARIABLES(24)
C      ANGLT=INTBUF_TAB%VARIABLES(25)
C heat interface
      INTTH = IPARI(47,NIN)
      IFORM = IPARI(48,NIN)
C      
      STIGLO=-INTBUF_TAB%STFAC(1)
      STARTT=INTBUF_TAB%VARIABLES(3)
      STOPT =INTBUF_TAB%VARIABLES(11)
      IF(STARTT>TT) RETURN
      IF(TT>STOPT)  RETURN
C  
      FRIC  =INTBUF_TAB%VARIABLES(1)
      GAP   =INTBUF_TAB%VARIABLES(2)
      GAPMIN=INTBUF_TAB%VARIABLES(13)
      VISC  =INTBUF_TAB%VARIABLES(14)
      VISCF =INTBUF_TAB%VARIABLES(15)
C
      GAPMAX=INTBUF_TAB%VARIABLES(16)
      KMIN  =INTBUF_TAB%VARIABLES(17)
      KMAX  =INTBUF_TAB%VARIABLES(18)
C
      RSTIF   = INTBUF_TAB%VARIABLES(20)
      FHEAT   = INTBUF_TAB%VARIABLES(21)
      TINT    = INTBUF_TAB%VARIABLES(22)
      EPS     = INTBUF_TAB%VARIABLES(39)
C  
c----------------------------------------------------
c   Courbure quadratique calcul des normales nodales
c----------------------------------------------------
      IF(ICURV==3)THEN
      ENDIF!(ICURV==3)
c----------------------------------------------------
c   Rayon de courbure : calcul des normales nodales (normees)
C   IADM!=0 + Icurv!=0 non available (starter error).
c----------------------------------------------------
c      IF(IADM/=0)THEN
c      END IF!(IADM/=0)
C----------------------------------------------------
c------------------------------------------------
        I_STOK = INTBUF_TAB_IMP%I_STOK(1)
      IF(I_STOK== 0) RETURN
C-----------in SPMD, should do the comm or simplifying the values--  
      INTKG1 = 0
      IF (INTKG>0.AND.IIKGOFF/=1) INTKG1=1    
      IF(INTKG1 > 0) THEN
       ALLOCATE(TAG_S(NUMNOD),TAG_M(NUMNOD))
        TAG_S =0
        TAG_M =0
       DO I=1,NSN
        J=INTBUF_TAB%NSV(I)
        TAG_S(J) =I
       END DO
       DO I=1,NMN
        J=INTBUF_TAB%MSR(I)
        TAG_M(J) =I
       END DO
      END IF !(INTKG1 > 0) THEN
C------------multi-contact spmd      
      CALL FFIZERO(I_STOK    ,NIN   ,NSN     ,INTBUF_TAB_IMP%CAND_N     )
C
        DO NFT = 0 , I_STOK - 1 , NVSIZ
          JLT = MIN( NVSIZ, I_STOK - NFT )
          IDNJ = 3*NFT + 1
          IDHJ = 4*NFT + 1
C preparation 
c          CALL I24CORK3(
c     1  JLT       ,X       ,INTBUF_TAB%IRECTM,INTBUF_TAB%NSV,
c     2  CAND_E(NFT+1),CAND_N(NFT+1) ,INTBUF_TAB%STFM,
c     2  INTBUF_TAB%STFNS,STIF  ,
c     3  XX0       ,YY0        ,ZZ0       ,VX           ,VY           ,   
c     5  VZ        ,XI         ,YI        ,ZI           ,VXI          ,
c     7  VYI       ,VZI        ,IXX       ,NSVG         ,INTBUF_TAB%NVOISIN,
c     9  MS,MSI    ,NSN        ,V         ,
c     A  KINI      ,NTY        ,NIN       ,IGSTI        ,KMIN         ,  
c     B  KMAX      ,INTBUF_TAB%GAP_S,GAPS    ,ITRIV     )

c          CALL I24DSTK3(
c     1                  JLT    ,CAND_E(NFT+1)  ,CAND_N(NFT+1) ,
c     2                  X1     ,X2     ,X3     ,X4     ,Y1     ,
c     3                  Y2     ,Y3     ,Y4     ,Z1     ,Z2     ,
c     4                  Z3     ,Z4     ,XI     ,YI     ,ZI     ,
c     5                  VX1    ,VX2    ,VX3    ,VX4    ,VXI    ,
c     6                  VY1    ,VY2    ,VY3    ,VY4    ,VYI    ,
c     7                  VZ1    ,VZ2    ,VZ3    ,VZ4    ,VZI    ,
c     8                  N1     ,N2     ,N3     ,H1     ,H2     ,
c     9                  H3     ,H4     ,NIN    ,NSN    ,IX1    ,
c     A                  IX2    ,IX3    ,IX4    ,NSVG   ,STIF   ,
c     B                  INTBUF_TAB%MSEGLO  ,GAPS   ,INTBUF_TAB%GAP_NM  ,
c     B                  INTBUF_TAB%IRECTM,INTBUF_TAB%IRTLM,SUBTRIA,
c     C                  PENE   ,XX0    ,YY0    ,ZZ0    ,
c     E                  VX     ,VY     ,VZ     ,IXX    ,INTBUF_TAB%MVOISIN,
c     F                  INTBUF_TAB%GAP_M,IND_SUBT(NFT+1)  ,ITRIV  ,
c     G                  INTBUF_TAB%PENE_OLD,EPS     ,NM1    ,NM2    ,
c     I                  NM3   ,INTBUF_TAB%ICONT_I)
C     
            CALL I24CORKM(
     1               JLT     ,X      ,INTBUF_TAB%IRECTM,INTBUF_TAB%NSV,
     +               INTBUF_TAB_IMP%CAND_E(NFT+1) ,INTBUF_TAB_IMP%CAND_N(NFT+1),
     2               STIF   ,INTBUF_TAB_IMP%STIF(NFT+1),
     +               XI     ,YI      ,ZI      ,
     3               VXI    ,VYI     ,VZI    ,IX1   ,
     4               IX2     ,IX3    ,IX4     ,NSVG  ,INTBUF_TAB%NVOISIN,
     5               MS      ,MSI    ,NSN     ,V     ,NIN    , 
     6               N1      ,N2     ,N3     ,H1    ,H2      ,
     7               H3      ,H4     ,INTBUF_TAB_IMP%NJ(IDNJ),INTBUF_TAB_IMP%HJ(IDHJ),
     8               INTBUF_TAB_IMP%INDSUBT(NFT+1))
            CALL I24KEG3(JLT    ,A      ,V      ,MS    ,FRIC   ,
     1                  IX1    ,IX2    ,IX3    ,IX4   ,NSVG   ,
     2                  STIF   ,VXI    ,VYI    ,VZI   ,MSI    ,
     5                  N1     ,N2     ,N3     ,H1    ,H2     ,
     6                  H3     ,H4     ,PENE   ,STIGLO,X      ,
     3                  KI11   ,KI12   ,KJ11  ,KJ12   ,KK11   ,
     4                  KK12   ,KL11   ,KL12  ,OFF    ,SK_INT,
     5                  NIN    ,LREM   ,INTBUF_TAB%STIF_OLD ,
     +                  INTBUF_TAB_IMP%CAND_N(NFT+1),
     6                  IGSTI  ,INTBUF_TAB%PENE_OLD,NM1    ,NM2    ,
     7                  NM3    )
       IF(INTKG1 > 0) THEN
            CALL I24KGEO3(JLT    ,IX1    ,IX2    ,IX3    ,IX4   ,
     1                    NSVG   ,STIF   ,H1     ,H2     ,H3    ,
     2                    H4     ,PENE   ,STIGLO ,KI11   ,KI12  ,
     3                    KJ11   ,KJ12   ,KK11   ,KK12   ,KL11  ,
     4                    KL12   ,SK_INT ,INTBUF_TAB%NOD_2RY_LGTH,
     .                    INTBUF_TAB%NOD_MAS_LGTH,
     5                    TAG_S  ,TAG_M  ,NSN    )
        DEALLOCATE(TAG_S,TAG_M)
       END IF
     
           IF (NSPMD > 1) THEN
            LREM = LREM + JLT
            CALL ASS_SPMD(3        ,NSVG     ,IX1    ,IX2    ,IX3    ,       
     1         IX4      ,JLT       ,IDDL     ,K_DIAG    ,K_LT     ,
     2         IADK     ,JDIK      ,KI11      ,KI12      ,KJ11     ,
     3         KJ12     ,KK11      ,KK12      ,KL11      ,KL12     ,
     4         OFF      ,NIN       )
            LREM = LREM - JLT
           ENDIF 
C
            CALL ASSEM_INT(3       ,NSVG     ,IX1    ,IX2    ,IX3    ,
     1         IX4      ,JLT       ,IDDL      ,K_DIAG    ,K_LT     ,
     2         IADK     ,JDIK      ,KI11      ,KI12      ,KJ11     ,
     3         KJ12     ,KK11      ,KK12      ,KL11      ,KL12     ,
     4         OFF      )
      ENDDO

      INTBUF_TAB_IMP%I_STOK(1) = 0 
C
      RETURN
      END
Chd|====================================================================
Chd|  I24KEG3                       source/interfaces/int24/i24ke3.F
Chd|-- called by -----------
Chd|        I24KE3                        source/interfaces/int24/i24ke3.F
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I24KEG3(JLT    ,A      ,V      ,MS    ,FRIC   ,
     1                  IX1    ,IX2    ,IX3    ,IX4   ,NSVG   ,
     2                  STIF   ,VXI    ,VYI    ,VZI   ,MSI    ,
     5                  N1     ,N2     ,N3     ,H1    ,H2     ,
     6                  H3     ,H4     ,PENE   ,STIGLO,X      ,
     3                  KI11   ,KI12   ,KJ11  ,KJ12   ,KK11   ,
     4                  KK12   ,KL11   ,KL12  ,OFF    ,SCALK  ,
     5                  NIN    ,LREM   ,STIF_OLD,CAND_N,IGSTI ,
     6                  PENE_OLD,NM1   ,NM2    ,NM3    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
      USE TRI7BOX
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
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, LREM,NIN,CAND_N(*),IGSTI
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        NSVG(MVSIZ)
      my_real
     .   A(3,*), MS(*), V(3,*),X(3,*),
     .   STIGLO,FRIC,OFF(*),SCALK,
     .   VXI(MVSIZ),VYI(MVSIZ),VZI(MVSIZ),MSI(MVSIZ)
      my_real
     .   N1(MVSIZ), N2(MVSIZ), N3(MVSIZ), PENE(MVSIZ),
     .   H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ),
     .   NM1(MVSIZ), NM2(MVSIZ), NM3(MVSIZ), 
     .   STIF(MVSIZ),PENE_OLD(5,*),STIF_OLD(2,*),
     .    KI11(3,3,MVSIZ),KJ11(3,3,MVSIZ),
     .    KK11(3,3,MVSIZ),KL11(3,3,MVSIZ),KI12(3,3,MVSIZ),
     .    KJ12(3,3,MVSIZ),KK12(3,3,MVSIZ),KL12(3,3,MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J1, J, K,IG,ISF,NN,NS,JLTF,NE,JG,N,ip
      my_real
     .   VX(MVSIZ), VY(MVSIZ), VZ(MVSIZ), VN(MVSIZ), 
     .   S2,FAC,FACF, H0, LA1, LA2, LA3, LA4,FACT(MVSIZ),
     .   D1,D2,D3,D4,A1,A2,A3,A4,KN(4,MVSIZ),Q(3,3,MVSIZ)
      my_real
     .   PREC,Q11,Q12,Q13,Q22,Q23,Q33,H00,VTX,VTY,VTZ,VT,
     .   KT1,KT2,KT3,KT4,Q1,Q2,DPENE(MVSIZ),VNM(MVSIZ)
      INTEGER NA1,NA2
      my_real
     .   A0X,A0Y,A0Z,RX,RY,RZ,
     .   ANX,ANY,ANZ,AAN,AAX,AAY,AAZ ,RR,RS,AAA ,TM,TS
C-----------------------------------------------
      IF (IRESP==1) THEN
           PREC = FIVEEM4
      ELSE
           PREC = EM10
      ENDIF
C---------------------
C     COURBURE FIXE
C---------------------
C      IF(ICURV(1)==1)THEN
C      ELSEIF(ICURV(1)==2)THEN
C      ELSEIF(ICURV(1) == 3)THEN
C      ENDIF
      DO I=1,JLT
        VX(I) = VXI(I) - H1(I)*V(1,IX1(I)) - H2(I)*V(1,IX2(I))
     .                 - H3(I)*V(1,IX3(I)) - H4(I)*V(1,IX4(I))
        VY(I) = VYI(I) - H1(I)*V(2,IX1(I)) - H2(I)*V(2,IX2(I))
     .                 - H3(I)*V(2,IX3(I)) - H4(I)*V(2,IX4(I))
        VZ(I) = VZI(I) - H1(I)*V(3,IX1(I)) - H2(I)*V(3,IX2(I))
     .                 - H3(I)*V(3,IX3(I)) - H4(I)*V(3,IX4(I))
        VN(I) = N1(I)*VX(I) + N2(I)*VY(I) + N3(I)*VZ(I)
c        VNM(I) = NM1(I)*VX(I) + NM2(I)*VY(I) + NM3(I)*VZ(I)
      ENDDO
C---------------------
C      PENE INITIALE
C---------------------
       ip=0
       IF (IGSTI==6) THEN
        IF (INCONV < 0) THEN
         DO I=1,JLT
          JG = NSVG(I)
          N  = CAND_N(I)
          IF(JG > 0)THEN
           STIF(I) = STIF_OLD(1,N) 
          ELSE
           STIF(I) = STIF_OLDFI(NIN)%P(1,-JG) 
          END IF
         END DO 
c        ELSE
c         DO I=1,JLT
c          JG = NSVG(I)
c          N  = CAND_N(I)
c          IF(JG > 0)THEN
c           STIF(I) = STIF_OLD(2,N) 
c          ELSE
c           STIF(I) = STIF_OLDFI(NIN)%P(2,-JG) 
c          END IF
c         END DO 
        END IF !(INCONV < 0 THEN
c       ELSE
C  ------(IGSTI/=6)     
c       DO I=1,JLT
c       IF(PENE(I) == ZERO)CYCLE
c        DPENE(I) = MAX(ZERO,-VNM(I)*DT1)
c        JG = NSVG(I)
c        N  = CAND_N(I)
c        IF(JG > 0)THEN
c          IF(PENE_OLD(2,N) /= ZERO.and.TT /= ZERO)THEN
c            IF(PENE(I) > PENE_OLD(2,N) .and. 
c     .            DPENE(I) < PENE(I)-PENE_OLD(2,N))THEN
c              STIF(I) = (STIF_OLD(2,N)*PENE_OLD(2,N)+ STIF(I)*DPENE(I))
c     .                                /PENE(I)
c            ELSE
c              STIF(I) = STIF_OLD(2,N) 
c            ENDIF
c          ENDIF
c        ELSE
c          JG = -JG
c          IF(PENE_OLDFI(NIN)%P(2,JG) /= ZERO.and.TT /= ZERO)THEN
c            IF(PENE(I) > PENE_OLDFI(NIN)%P(2,JG) .and. 
c     .       DPENE(I) < PENE(I)-PENE_OLDFI(NIN)%P(2,JG))THEN
c              STIF(I) = (STIF_OLDFI(NIN)%P(2,JG)*PENE_OLDFI(NIN)%P(2,JG)
c     .                  + STIF(I)*DPENE(I)) / PENE(I)
c            ELSE
c              STIF(I) = STIF_OLDFI(NIN)%P(2,JG) 
c            ENDIF
c          ENDIF
c        ENDIF
c       ENDDO
       END IF !(IGSTI==6) THEN
C-----
c      DO I=1,JLT
c       IF(STIGLO<=ZERO)THEN
c        STIF(I) = HALF*STIF(I) 
c       ELSEIF(STIF(I)/=ZERO)THEN
c        STIF(I) = STIGLO 
c       ENDIF
c      ENDDO
C---------------------------------
C    ----sans frottement d'abord--- 
      DO I=1,JLT
        VTX = VX(I) -VN(I)*N1(I)
        VTY = VY(I) -VN(I)*N2(I)
        VTZ = VZ(I) -VN(I)*N3(I)
        VT  = VTX*VTX+VTY*VTY+VTZ*VTZ
        IF (VT>EM20) THEN
         S2=ONE/SQRT(VT)
         Q(1,1,I)=VTX*S2
         Q(1,2,I)=VTY*S2
         Q(1,3,I)=VTZ*S2
         Q(3,1,I)=N1(I)
         Q(3,2,I)=N2(I)
         Q(3,3,I)=N3(I)
         Q(2,1,I)=Q(3,2,I)*Q(1,3,I)-Q(3,3,I)*Q(1,2,I)
         Q(2,2,I)=Q(3,3,I)*Q(1,1,I)-Q(3,1,I)*Q(1,3,I)
         Q(2,3,I)=Q(3,1,I)*Q(1,2,I)-Q(3,2,I)*Q(1,1,I)
         FACT(I)=FRIC
        ELSE
         FACT(I)=ZERO
        ENDIF
      ENDDO
      IF (SCALK<0) THEN
       ISF=1
      ELSE
       ISF=0
      ENDIF
      FACF=ABS(SCALK)
      IF (ISF==1) THEN
       DO I=1,JLT
        IF (VN(I)>ZERO) THEN
         FAC=STIF(I)/FACF
        ELSEIF (VN(I)<ZERO) THEN
         FAC=STIF(I)*FACF
        ELSE
         FAC=STIF(I)
        ENDIF
        KN(1,I)=FAC*H1(I)
        KN(2,I)=FAC*H2(I)
        KN(3,I)=FAC*H3(I)
        KN(4,I)=FAC*H4(I)
        FACT(I)=FAC*FACT(I)
       ENDDO
      ELSE
       DO I=1,JLT
        FAC=STIF(I)*FACF
        KN(1,I)=FAC*H1(I)
        KN(2,I)=FAC*H2(I)
        KN(3,I)=FAC*H3(I)
        KN(4,I)=FAC*H4(I)
        FACT(I)=FAC*FACT(I)
       ENDDO
      ENDIF
      DO I=1,JLT
       Q11=N1(I)*N1(I)
       Q12=N1(I)*N2(I)
       Q13=N1(I)*N3(I)
       Q22=N2(I)*N2(I)
       Q23=N2(I)*N3(I)
       Q33=N3(I)*N3(I)
       KI11(1,1,I)=KN(1,I)*Q11
       KI11(1,2,I)=KN(1,I)*Q12
       KI11(1,3,I)=KN(1,I)*Q13
       KI11(2,2,I)=KN(1,I)*Q22
       KI11(2,3,I)=KN(1,I)*Q23
       KI11(3,3,I)=KN(1,I)*Q33
       KJ11(1,1,I)=KN(2,I)*Q11
       KJ11(1,2,I)=KN(2,I)*Q12
       KJ11(1,3,I)=KN(2,I)*Q13
       KJ11(2,2,I)=KN(2,I)*Q22
       KJ11(2,3,I)=KN(2,I)*Q23
       KJ11(3,3,I)=KN(2,I)*Q33
       KK11(1,1,I)=KN(3,I)*Q11
       KK11(1,2,I)=KN(3,I)*Q12
       KK11(1,3,I)=KN(3,I)*Q13
       KK11(2,2,I)=KN(3,I)*Q22
       KK11(2,3,I)=KN(3,I)*Q23
       KK11(3,3,I)=KN(3,I)*Q33
       KL11(1,1,I)=KN(4,I)*Q11
       KL11(1,2,I)=KN(4,I)*Q12
       KL11(1,3,I)=KN(4,I)*Q13
       KL11(2,2,I)=KN(4,I)*Q22
       KL11(2,3,I)=KN(4,I)*Q23
       KL11(3,3,I)=KN(4,I)*Q33
      ENDDO
C    ----avec frottement --- 
       DO J=1,3 
        DO K=J,3 
         DO I=1,JLT
          IF (FACT(I)>ZERO) THEN
           Q1 =Q(1,J,I)*Q(1,K,I)
           Q2 =Q(2,J,I)*Q(2,K,I)
           FAC=FACT(I)*(Q1+Q2)
           KT1=FAC*H1(I)
           KI11(J,K,I)=KI11(J,K,I)+KT1
           KT2=FAC*H2(I)
           KJ11(J,K,I)=KJ11(J,K,I)+KT2
           KT3=FAC*H3(I)
           KK11(J,K,I)=KK11(J,K,I)+KT3
           KT4=FAC*H4(I)
           KL11(J,K,I)=KL11(J,K,I)+KT4
          ENDIF 
         ENDDO
        ENDDO
       ENDDO
C
       DO J=1,3 
        DO K=J,3 
         DO I=1,JLT
          KI12(J,K,I)=-KI11(J,K,I)
          KJ12(J,K,I)=-KJ11(J,K,I)
          KK12(J,K,I)=-KK11(J,K,I)
          KL12(J,K,I)=-KL11(J,K,I)
         ENDDO
        ENDDO
       ENDDO
       DO J=1,3 
        DO K=J+1,3 
         DO I=1,JLT
          KI12(K,J,I)=-KI11(J,K,I)
          KJ12(K,J,I)=-KJ11(J,K,I)
          KK12(K,J,I)=-KK11(J,K,I)
          KL12(K,J,I)=-KL11(J,K,I)
         ENDDO
        ENDDO
       ENDDO
C
       DO I=1,JLT
        OFF(I)=ONE
       ENDDO
C
      IF (NSPMD > 1) THEN
C
       IF (INTP_D>0) THEN
        DO I=1,JLT
         IF(NSVG(I)<0) THEN
	      NN=-NSVG(I)
	      NS=IND_INT(NIN)%P(NN)
C---------pour diag_ss---
	      FFI(1,NS)=ZERO
	      FFI(2,NS)=ZERO
	      FFI(3,NS)=ZERO
	      DFI(1,NS)=ZERO
	      DFI(2,NS)=ZERO
	      DFI(3,NS)=ZERO
	     ENDIF
        ENDDO
       ELSE
C---  general case----
        JLTF = 0
        DO I=1,JLT
         IF(NSVG(I)<0) THEN
          NN=-NSVG(I)
          JLTF = JLTF + 1
          NE=SHF_INT(NIN) + JLTF +LREM
          NS=IND_INT(NIN)%P(NN)
          STIFS(NE)=STIF(I)
          H_E(1,NE)=H1(I)
          H_E(2,NE)=H2(I)
          H_E(3,NE)=H3(I)
          H_E(4,NE)=H4(I)
          N_E(1,NE)=N1(I)
          N_E(2,NE)=N2(I)
          N_E(3,NE)=N3(I)
C----pour temporairement diag_ss---
          FFI(1,NS)=ZERO
          FFI(2,NS)=ZERO
          FFI(3,NS)=ZERO
          DFI(1,NS)=ZERO
          DFI(2,NS)=ZERO
          DFI(3,NS)=ZERO
	     ENDIF
        ENDDO
C
       END IF !IF (INTP_D>0)
      END IF 
C
       RETURN
      END
Chd|====================================================================
Chd|  I24KGEO3                      source/interfaces/int24/i24ke3.F
Chd|-- called by -----------
Chd|        I24KE3                        source/interfaces/int24/i24ke3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I24KGEO3(JLT    ,IX1    ,IX2    ,IX3    ,IX4   ,
     1                    NSVG   ,STIF   ,H1     ,H2     ,H3    ,
     2                    H4     ,PENE   ,STIGLO ,KI11   ,KI12  ,
     3                    KJ11   ,KJ12   ,KK11   ,KK12   ,KL11  ,
     4                    KL12   ,SCALK  ,LL_S   ,LL_M   ,TAG_S ,
     5                    TAG_M  ,NSN    )
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
      INTEGER JLT
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        NSVG(MVSIZ),TAG_S(*),TAG_M(*),NSN
      my_real
     .   STIGLO,SCALK
      my_real
     .   PENE(MVSIZ),
     .   H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ),
     .   STIF(MVSIZ),LL_S(*)   ,LL_M(*),
     .    KI11(3,3,MVSIZ),KJ11(3,3,MVSIZ),
     .    KK11(3,3,MVSIZ),KL11(3,3,MVSIZ),KI12(3,3,MVSIZ),
     .    KJ12(3,3,MVSIZ),KK12(3,3,MVSIZ),KL12(3,3,MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J1, J, K,IG,ISF,NN,NS,JLTF,NE,NM
      my_real
     .   S2,FAC,FACF, H0, LA1, LA2, LA3, LA4,
     .   D1,D2,D3,D4,A1,A2,A3,A4,KN(4,MVSIZ),LNS,LNS1
      my_real
     .   LMAX,FNI(MVSIZ),AL(4,MVSIZ)
C-----------------------------------------------
      IF (NSN == 0) RETURN
      DO I=1,JLT
       IF(STIGLO<=ZERO)THEN
        STIF(I) = HALF*STIF(I) 
       ELSEIF(STIF(I)/=ZERO)THEN
        STIF(I) = STIGLO 
       ENDIF
        FNI(I)= -STIF(I) * PENE(I)
      ENDDO
C---------------------------------
       LNS1=HALF*(LL_S(1)+LL_S(NSN))
       DO I=1,JLT
        NS=NSVG(I)
C-------in spmd LNS is simplified        
        IF (NS < 0) THEN
         LNS=LNS1
        ELSE
         LNS=LL_S(TAG_S(NS))
        END IF
        NM=IX1(I)
        AL(1,I)=LNS+LL_M(TAG_M(NM))
        NM=IX2(I)
        AL(2,I)=LNS+LL_M(TAG_M(NM))
        NM=IX3(I)
        AL(3,I)=LNS+LL_M(TAG_M(NM))
        NM=IX4(I)
        AL(4,I)=LNS+LL_M(TAG_M(NM))
        LMAX=ONEP01*PENE(I)
        DO J=1,4
         AL(J,I)=MAX(LMAX,AL(J,I))
        END DO
       ENDDO
       FACF=ABS(SCALK)
       DO I=1,JLT
        FAC = FACF*FNI(I)
        KN(1,I)=FAC*H1(I)/AL(1,I)
        KN(2,I)=FAC*H2(I)/AL(2,I)
        KN(3,I)=FAC*H3(I)/AL(3,I)
        KN(4,I)=FAC*H4(I)/AL(4,I)
c        print *,'FACF,FNI(I),KN(1,I)=',FACF,FNI(I),KN(1,I)
       ENDDO
C
       DO J=1,3
        DO I=1,JLT
         KI11(J,J,I) = KI11(J,J,I)+KN(1,I)
         KI12(J,J,I) = KI12(J,J,I)-KN(1,I)
         KJ11(J,J,I) = KJ11(J,J,I)+KN(2,I)
         KJ12(J,J,I) = KJ12(J,J,I)-KN(2,I)
         KK11(J,J,I) = KK11(J,J,I)+KN(3,I)
         KK12(J,J,I) = KK12(J,J,I)-KN(3,I)
         KL11(J,J,I) = KL11(J,J,I)+KN(4,I)
         KL12(J,J,I) = KL12(J,J,I)-KN(4,I)
        ENDDO
       ENDDO
C
       RETURN
      END
C-----
