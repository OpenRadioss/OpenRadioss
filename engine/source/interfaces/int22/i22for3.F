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
Chd|  I22FOR3                       source/interfaces/int22/i22for3.F
Chd|-- called by -----------
Chd|        I22MAINF                      source/interfaces/int22/i22mainf.F
Chd|-- calls ---------------
Chd|        I22ASS0                       source/interfaces/int22/i22assembly.F
Chd|        I22ASS2                       source/interfaces/int22/i22assembly.F
Chd|        I7ASS3                        source/interfaces/int07/i7ass3.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        I22BUFBRIC_MOD                ../common_source/modules/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/cut-cell-search_mod.F
Chd|====================================================================
      SUBROUTINE I22FOR3(JLT       ,A        ,V         ,IBC      ,ICODT  ,
     2                   FSAV      ,GAP      ,FRIC      ,MS       ,VISC   ,
     3                   VISCF     ,NOINT    ,STFN      ,ITAB     ,CB_LOC ,
     4                   STIGLO    ,STIFN    ,STIF      ,FSKYI    ,ISKY   ,
     6                   NX1       ,NX2      ,NX3       ,NX4      ,NY1    ,
     7                   NY2       ,NY3      ,NY4       ,NZ1      ,NZ2    ,
     8                   NZ3       ,NZ4      ,LB1       ,LB2      ,LB3    ,
     9                   LB4       ,LC1      ,LC2       ,LC3      ,LC4    ,
     A                   P1        ,P2       ,P3        ,P4       ,FCONT  ,
     B                   IX1       ,IX2      ,IX3       ,IX4      ,NSVG   ,
     C                   IVIS2     ,NELTST   ,ITYPTST   ,DT2T     ,INTTH  ,
     D                   GAPV      ,INACTI   ,CAND_P    ,INDEX    ,NISKYFI,
     E                   KINET     ,NEWFRONT ,ISECIN    ,NSTRF    ,SECFCUM,
     F                   X         ,IRECT    ,CE_LOC    ,MFROT    ,IFQ    ,
     G                   FROT_P    ,CAND_FX  ,CAND_FY   ,CAND_FZ  ,ALPHA0,
     H                   IFPEN     ,IBAG     ,ICONTACT ,
     J                   VISCN     ,VXI      ,VYI       ,VZI      ,MSI    ,
     K                   KINI      ,NIN      ,NISUB     ,LISUB    ,ADDSUBS,
     L                   ADDSUBM   ,LISUBS   ,LISUBM    ,FSAVSUB  ,CAND_N ,
     M                   ILAGM     ,ICURV    ,PRES      ,FNCONT   ,MS0    ,
     N                   N_SCUT    ,N_SURF   ,CoG       ,CAND_E   ,Swet   ,
     O                   ELBUF_TAB ,X1       ,X2        ,X3       ,X4     ,
     3                   Y1        ,Y2       ,Y3        ,Y4       ,Z1     ,
     4                   Z2        ,Z3       ,Z4        ,IXS      ,NV46   ,
     5                   Delta     ,ISENSINT ,FSAVPARIT ,IPARG    ,H3D_DATA)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C Interface Type22 (/INTER/TYPE22) is an FSI coupling method based on cut cell method. 
C   This experimental cut cell method is not completed, abandonned, and is not an official option.
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE I22TRI_MOD
      USE I22BUFBRIC_MOD
      USE H3D_MOD
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr02_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr07_c.inc"
#include      "scr11_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr18_c.inc"
#include      "units_c.inc"
#include      "parit_c.inc"
#include      "warn_c.inc"
#include      "couple_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB

      INTEGER NELTST,ITYPTST,JLT,IBC,IBCM,IBCS,IVIS2,INACTI,IBAG,NIN,
     .        ICODT(*), ITAB(*), ISKY(*), KINET(*),
     .        MFROT, IFQ, NOINT,NEWFRONT,ISECIN, NSTRF(*),
     .        IRECT(4,*),IFPEN(*) ,ICONTACT(*), CAND_N(*),
     .        KINI(*),
     .        ISET, NISKYFI,INTTH,IFORM,NV46
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        CB_LOC(MVSIZ),CE_LOC(MVSIZ),INDEX(MVSIZ),NSVG(MVSIZ),
     .        NISUB, LISUB(*), ADDSUBS(*), ADDSUBM(*), LISUBS(*),
     .        LISUBM(*),ILAGM,ICURV(3),CAND_E(*),ixs(nixs,*),
     .        ISENSINT(*), IPARG(NPARG,*)
      my_real
     .   STIGLO,CAND_P(*),FROT_P(*), X(3,*),MS0(*),
     .   A(3,*), MS(*), V(3,*), FSAV(*),FCONT(3,*),
     .   CAND_FX(*),CAND_FY(*),CAND_FZ(*),ALPHA0,
     .   GAP, FRIC,VISC,VISCF,VIS,DT2T,STFN(*),STIFN(*),
     .   FSKYI(LSKYI,NFSKYI),FSAVSUB(NTHVKI,*),FNCONT(3,*)
      my_real
     .     NX1(MVSIZ), NX2(MVSIZ), NX3(MVSIZ), NX4(MVSIZ),
     .     NY1(MVSIZ), NY2(MVSIZ), NY3(MVSIZ), NY4(MVSIZ),
     .     NZ1(MVSIZ), NZ2(MVSIZ), NZ3(MVSIZ), NZ4(MVSIZ),
     .     LB1(MVSIZ), LB2(MVSIZ), LB3(MVSIZ), LB4(MVSIZ),
     .     LC1(MVSIZ), LC2(MVSIZ), LC3(MVSIZ), LC4(MVSIZ),
     .     P1(MVSIZ), P2(MVSIZ), P3(MVSIZ), P4(MVSIZ), STIF(MVSIZ),
     .     GAPV(MVSIZ),
     .     SECFCUM(7,NUMNOD,NSECT),TMP(MVSIZ),
     .     STIFSAV(MVSIZ), VISCN(*),
     .     VXI(MVSIZ),VYI(MVSIZ),VZI(MVSIZ),MSI(MVSIZ),
     .     PRES(*), RSTIF ,
     .     CoG(1:3,NBCUT_MAX,MVSIZ), N_SCUT(3,NBCUT_MAX, MVSIZ), Swet(NBCUT_MAX,MVSIZ)
      my_real ::
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   delta(4,NBCUT_MAX,MVSIZ),FSAVPARIT(NISUB+1,11,*),FACE
      TYPE(H3D_DATABASE) :: H3D_DATA

C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      TYPE(G_BUFEL_)  ,POINTER            :: GBUF1, GBUF2
      INTEGER I, J1, J2, IG, J, JG , K0,NBINTER,K1S,K,IL,IE, NN, NI,NA1,NA2
      INTEGER IBv, Jv, NBCUTv, IEv, LLT1, LLT2
      my_real
     .   FXI(MVSIZ), FYI(MVSIZ), FZI(MVSIZ), FNI(MVSIZ),
     .   FXT(MVSIZ),FYT(MVSIZ),FZT(MVSIZ),
     .   FX1(MVSIZ), FX2(MVSIZ), FX3(MVSIZ), FX4(MVSIZ),
     .   FY1(MVSIZ), FY2(MVSIZ), FY3(MVSIZ), FY4(MVSIZ),
     .   FZ1(MVSIZ), FZ2(MVSIZ), FZ3(MVSIZ), FZ4(MVSIZ),
     .   N1(MVSIZ), N2(MVSIZ), N3(MVSIZ), PENE(MVSIZ),
     .   VIS2(MVSIZ), DTMI(MVSIZ), XMU(MVSIZ),STIF0(MVSIZ),
     .   H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ),
     .   VX(MVSIZ), VY(MVSIZ), VZ(MVSIZ), VN(MVSIZ),DIST(MVSIZ),
     .   VNX, VNY, VNZ, AA, CRIT,S2,RDIST,
     .   V2, FM2, VISCA, FAC,FF(3),ALPHI,ALPHA,BETA,
     .   FX, FY, FZ, F2, MAS2, M2SK, DTMI0,DTI,FT,FN,FMAX,FTN,
     .   FACM1, ECONTT, ECONVT, H0, LA1, LA2, LA3, LA4,
     .   D1,D2,D3,D4,A1,A2,A3,A4,
     .   FSAV1, FSAV2, FSAV3, FSAV4, FSAV5, FSAV6, FSAV7, FSAV8,
     .   FSAV9, FSAV10, FSAV11, FSAV12, FSAV13, FSAV14, FSAV15, FFO,
     .   E10, H0D, S2D,
     .   LA1D,LA2D,LA3D,LA4D,T1,T1D,T2,T2D,FFD,VISD,FACD,D1D,
     .   P1S(MVSIZ),P2S(MVSIZ),P3S(MVSIZ),P4S(MVSIZ),
     .   D2D,D3D,D4D,VNXD,VNYD,VNZD,V2D,FM2D,F2D,AAD,FXD,FYD,FZD,
     .   A1D,A2D,A3D,A4D,VV,AX1,AX2,AY1,AY2,AZ1,AZ2,AX,AY,AZ,
     .   AREA,P,VV1,VV2,V21,DMU, DTI2,H00 ,A0X,A0Y,A0Z,RX,RY,RZ,
     .   ANX,ANY,ANZ,AAN,AAX,AAY,AAZ ,RR,RS,AAA ,TM,TS
      my_real
     .   SURFX,SURFY,SURFZ,N_SURF(3,*),M1,M2,MF,THETA
      my_real
     .   ST1(MVSIZ),ST2(MVSIZ),ST3(MVSIZ),ST4(MVSIZ),STV(MVSIZ),
     .   KT(MVSIZ),C(MVSIZ),CF(MVSIZ),
     .   KS(MVSIZ),K1(MVSIZ),K2(MVSIZ),K3(MVSIZ),K4(MVSIZ),
     .   CS(MVSIZ),C1(MVSIZ),C2(MVSIZ),C3(MVSIZ),C4(MVSIZ),
     .   CX,CY,CFI,AUX,PHI1(MVSIZ), PHI2(MVSIZ), PHI3(MVSIZ),
     .   PHI4(MVSIZ), N_SCUT_(3)

      INTEGER JSUB,KSUB,JJ,KK,IN,NSUB,IBID, NG, IB,IV, NBCUT,ICUT, NP_RECT(MVSIZ),
     .         IEM1,IEM2, IBM1, IBM2, JM1,JM2
      INTEGER NG1,NG2,IDLOC1,IDLOC2,NP

      my_real
     .   FSAVSUB1(15,NISUB),IMPX,IMPY,IMPZ,PP1,PP2,PP3,PP4,BID
      my_real :: DP(MVSIZ), W(4,MVSIZ), Q, Slag, TMP2(3), Pt1(3),Pt2(3), Pt3(3), Pt4(3),distance(4),Dsum,UN1,UN2,ZC1,ZC2

      INTEGER :: ICELL1, ICELL2, ICELLv, MCELL1, MCELL2, IB1, IB2
C-----------------------------------------------
      INTERFACE
        FUNCTION I22AERA(NPTS,P, C)
        INTEGER :: NPTS
        my_real :: P(3,NPTS), I22AERA(3), C(3)
        END FUNCTION
      END INTERFACE
C-----------------------------------------------
#include "sync_dir.inc"
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------  
      ECONTT = ZERO
      ECONVT = ZERO
      DO I=1,JLT
        STIF0(I) = STIF(I)
      ENDDO


      BID = ZERO
      IBID = 0

!      !---------------------------------!
!      !       PRESSURE CORRECTION       !
!      !---------------------------------!
!      IF (IBAG/=0)THEN
!#include "lockon.inc"
!        DO I=1,JLT
!          PRES(CE_LOC(I)) = PRES(CE_LOC(I)) + FNI(I)
!        ENDDO
!#include "lockoff.inc"
!      END IF

C---------------------------------
#include "lockon.inc"
        ECONTV = ECONTV + ECONVT
        ECONT  = ECONT + ECONTT
#include "lockoff.inc"
C---------------------------------
C
      !---------------------------------!
      !       PARAMETER INIT.           !
      !---------------------------------!
      DO I=1,JLT
        IF(IX3(I)/=IX4(I))THEN
          NP_RECT(I) = 4
          W(1:4,I) = FOURTH
         ELSE
          NP_RECT(I) = 3
          W(1:4,I) = THIRD
         ENDIF
      ENDDO

      !---------------------------------!
      !       CONTACT FORCES.           !
      !---------------------------------!

      DO I=1,JLT

c        print *, "       I===== :      ", I
c        print *, "       CELL   :      ", IXS(11,BRICK_LIST(NIN,CB_LOC(I))%ID)
c        print *, "       NBCUT  :      ", BRICK_LIST(NIN,CB_LOC(I))%NBCUT
c        print *, "       SHELL  :      ", ITAB(IRECT(1:4,iabs(CE_LOC(I))))
c        print *, "       NCYCLE :      ", NCYCLE


        IB     = CB_LOC(I)
        IE     = BRICK_LIST(NIN,IB)%ID
        NBCUT  = BRICK_LIST(NIN,IB)%NBCUT

        FX1(I) = ZERO
        FY1(I) = ZERO
        FZ1(I) = ZERO
        FX2(I) = ZERO
        FY2(I) = ZERO
        FZ2(I) = ZERO
        FX3(I) = ZERO
        FY3(I) = ZERO
        FZ3(I) = ZERO
        FX4(I) = ZERO
        FY4(I) = ZERO
        FZ4(I) = ZERO
        FXI(I) = ZERO
        FYI(I) = ZERO
        FZI(I) = ZERO
        DP(I)  = ZERO
        FNI(I) = ZERO

c        IF(NBCUT==0) print *, "     C Y C L E"
        IF(NBCUT==0) CYCLE


        Pt1(1:3) = (/X1(I),Y1(I),Z1(I)/)
        Pt2(1:3) = (/X2(I),Y2(I),Z2(I)/)
        Pt3(1:3) = (/X3(I),Y3(I),Z3(I)/)
        Pt4(1:3) = (/X4(I),Y4(I),Z4(I)/)
        TMP(1:3) = I22AERA(  NP_RECT(I), (/Pt1,Pt2,Pt3,Pt4/) ,TMP2)  !to be optimized, almoste calculated in i22cor or i22dst3
        Slag=  SQRT(SUM(TMP(1:3)*TMP(1:3)))

        DO ICUT=1,NBCUT

          IF(CAND_E(I)<0) CYCLE   !ICUT=1,8 do not project shell which do not intersect
          !
          ! +------------+------------+
          ! +        |   +       |    +      20 with facette 1 + ( + : intersection)
          ! +        |   +       |    +      20 with facette 2 - ( - : couple sans intersection)
          ! +    20  |   +    30 |    +      30 with facette 1 -
          ! +        |   +       |    +      30 with facette 2 +
          ! +  IX    | I +   I   | IX +
          ! +------------+------------+
          !
          ! +--------\----+
          ! +         \I  +
          ! +          ---+-\
          ! +    20       +  | <---non intersecting edges but near virtual face (closure hypothesis)
          ! +           __+_/
          ! +  IX      /II+
          ! +---------/---+
          !
          ! +--------\----+-------|----+
          ! +         \I  +       |    +
          ! +          ---+-\     |    +
          ! +    20       +  | 30 |    +
          ! +           __+_/     |    +
          ! +  IX      /II+   I   | IX +
          ! +---------/---+-------|----+
          !                       |
          !                       | <------ face of this edge must not be projected on vitual cut surface from cell 20
          !                                 this case is not treated (lagrangian face too complex or multiple surface, self-impact, etc...)
          !                                 Use a simple surface : smooth and without holes

          !-------------------------------!
          !     GET RELATIVE PRESSURE.    !
          ! FROM BOTH SIDE OF CUT SURFACE !
          !-------------------------------!
          ! %P is pressure in cut cell buffer.
          !Bijection : Scut=Icut <-> (icell1,icell2)
          ICELL1 = ICUT
          ICELL2 = 9
          !Surjection (icell1,icell2) ->> ( (mcell1,ibv1) , (mcell2,ibv2) )
          IEM1   = BRICK_LIST(NIN,IB)%POLY(ICELL1)%WhereIsMain(3)
          IEM2   = BRICK_LIST(NIN,IB)%POLY(ICELL2)%WhereIsMain(3)
          JM1    = BRICK_LIST(NIN,IB)%POLY(ICELL1)%WhereIsMain(1)
          JM2    = BRICK_LIST(NIN,IB)%POLY(ICELL2)%WhereIsMain(1)
          IF(IEM1==IE)THEN
            IBM1 = IB
          ELSE
            IF(JM1<=NV46)THEN
              IBM1 = BRICK_LIST(NIN,IB)%Adjacent_Brick(JM1,4)
            ELSE
              J1   = JM1/10
              J2   = MOD(JM1,10)
              IBM1 = BRICK_LIST(NIN,IB  )%Adjacent_Brick(J1,4)
              IBM1 = BRICK_LIST(NIN,IBM1)%Adjacent_Brick(J2,4)
            ENDIF
          ENDIF
          IF(IEM2==IE)THEN
            IBM2 = IB
          ELSE
            IF(JM2<=NV46)THEN
              IBM2 = BRICK_LIST(NIN,IB)%Adjacent_Brick(JM2,4)
            ELSE
              J1   = JM2/10
              J2   = MOD(JM2,10)
              IBM2 = BRICK_LIST(NIN,IB  )%Adjacent_Brick(J1,4)
              IBM2 = BRICK_LIST(NIN,IBM2)%Adjacent_Brick(J2,4)
            ENDIF
          ENDIF
          NG1    =  BRICK_LIST(NIN,IBM1)%NG
          NG2    =  BRICK_LIST(NIN,IBM2)%NG
          IDLOC1 =  BRICK_LIST(NIN,IBM1)%IDLOC
          IDLOC2 =  BRICK_LIST(NIN,IBM2)%IDLOC
          GBUF1  => ELBUF_TAB(NG1)%GBUF
          GBUF2  => ELBUF_TAB(NG2)%GBUF
          LLT1   =  IPARG(2,NG1)
          LLT2   =  IPARG(2,NG2)
          P1(I)  =  -THIRD * (GBUF1%SIG(IDLOC1        )+
     .                        GBUF1%SIG(IDLOC1 +LLT1  )+
     .                        GBUF1%SIG(IDLOC1 +LLT1*2) )
          M1     = BRICK_LIST(NIN,IBM1)%MACH
          UN1    = BRICK_LIST(NIN,IB  )%POLY(ICELL1)%FACE0%U_N(1)  !for poly_id=ICELL1 in [1,NBCUT], there is a single cut section
          ZC1    = BRICK_LIST(NIN,IBM1)%RHOC

          P2(I)  =  -THIRD * (GBUF2%SIG(IDLOC2        )+
     .                        GBUF2%SIG(IDLOC2 +LLT2  )+
     .                        GBUF2%SIG(IDLOC2 +LLT2*2) )
          UN2    = BRICK_LIST(NIN,IB  )%POLY(ICELL2)%FACE0%U_N(ICELL1)   !icell2=9 => cut surface sharing icell1=icut and icell2=9, is Scut_id=icut=icell1
          ZC2    = BRICK_LIST(NIN,IBM2)%RHOC
          M2     = BRICK_LIST(NIN,IBM2)%MACH

          MF     = HALF*(M1+M2) ! with grid velocity  Mi must be calculated consequently in alefvm_stress*
          THETA  = MIN(ONE,MF)

          P1(I)  = P1(I) + ZC1*UN1*THETA
          P2(I)  = P2(I) + ZC2*UN2*THETA

          DP(I)  =  P1(I) - P2(I)

          !print *, i, dp(i), dp(i)+ZC1*UN1-ZC2*UN2
          !-------------------------------!
          Q     = SUM( N_SURF(1:3,iabs(CAND_E(I))) * N_SCUT(1:3,ICUT,I) ) !N_SCUT(1:3,ICUT,I)= S*n(1:3)
          FFO   = DP(I) * Swet(ICUT,I)
          IF(Q<ZERO) FFO = -FFO   !sign change to adjust normal direction (non oriented surface)

          !force au CoG
          FF(:) = FFO * N_SURF(:,iabs(CAND_E(I)))
          !rint *, "F_cog=", FF(:)

          !distributed forces using weigthing coefficients
          FX1(I)= FX1(I) + delta(1,ICUT,I) * FF(1)
          FY1(I)= FY1(I) + delta(1,ICUT,I) * FF(2)
          FZ1(I)= FZ1(I) + delta(1,ICUT,I) * FF(3)

          FX2(I)= FX2(I) + delta(2,ICUT,I) * FF(1)
          FY2(I)= FY2(I) + delta(2,ICUT,I) * FF(2)
          FZ2(I)= FZ2(I) + delta(2,ICUT,I) * FF(3)

          FX3(I)= FX3(I) + delta(3,ICUT,I) * FF(1)
          FY3(I)= FY3(I) + delta(3,ICUT,I) * FF(2)
          FZ3(I)= FZ3(I) + delta(3,ICUT,I) * FF(3)

          IF(NP_RECT(I)==4)THEN
          FX4(I)= FX4(I) + delta(4,ICUT,I) * FF(1)
          FY4(I)= FY4(I) + delta(4,ICUT,I) * FF(2)
          FZ4(I)= FZ4(I) + delta(4,ICUT,I) * FF(3)
          ENDIF

C          print *, "FF=", FF(1), ITAB(IRECT(1:4,CE_LOC(I)))

          FXI(I)= FXI(I) + FF(1)
          FYI(I)= FYI(I) + FF(2)
          FZI(I)= FZI(I) + FF(3)

          if(ibug22_fcont==-1)then
            print *, "######################################################"
            print *, "##I22FOR ; candidat I=", I, "  ICUT=", ICUT
            print *, "######################################################"
            print *, "   JLT    :      ", I,"/",JLT
            print *, "   CELL   :      ", IXS(11,BRICK_LIST(NIN,CB_LOC(I))%ID)
            print *, "   SHELL  :      ", ITAB(IRECT(1:4,iabs(CE_LOC(I))))
            print *, "   ICUT   :      ", ICUT
            print *, "   NCYCLE :      ", NCYCLE
            print *, "   P1           =", P1(I)
            print *, "   P2           =", P2(I)
            print *, "   DP           =", DP(I)
            print *, "   Swet(ICUT,I) =", Swet(ICUT,I)
            print *, "   F=DP*Swet    =", DP(I) * Swet(ICUT,I)
            print *, "   Slag         =", Slag
            print *, "   N_SURF()     =", N_SURF(:,iabs(CAND_E(I)))
            print *, "-----------------"
            write (*,FMT='(A,4E20.12,A,E20.12)') "   DELTA  :      ", delta(1:4,ICUT,I), "   | SUM=", SUM(delta(1:4,ICUT,I))
            write(*,FMT='(A,I8,A,3E30.16)')"FX(",itab(IRECT(1,iabs(CE_LOC(I)))),") = ", FX1(I),FY1(I),FZ1(I)
            write(*,FMT='(A,I8,A,3E30.16)')"FX(",itab(IRECT(2,iabs(CE_LOC(I)))),") = ", FX2(I),FY2(I),FZ2(I)
            write(*,FMT='(A,I8,A,3E30.16)')"FX(",itab(IRECT(3,iabs(CE_LOC(I)))),") = ", FX3(I),FY3(I),FZ3(I)
            write(*,FMT='(A,I8,A,3E30.16)')"FX(",itab(IRECT(4,iabs(CE_LOC(I)))),") = ", FX4(I),FY4(I),FZ4(I)
            print *, "-----------------"
            print *, "   FXI(I)=",FXI(I)
            print *, "   FYI(I)=",FYI(I)
            print *, "   FZI(I)=",FZI(I)
            print *, ""
          endif
        ENDDO!next ICUT

        !previous treatment was for face0
        !now treating other faces

        DO J=1,6
          NP = BRICK_LIST(NIN,IB)%PCUT(8+J)%NP
          IF(NP<=0)CYCLE
          ICELL1        = 9
          FACE          = BRICK_LIST(NIN,IB)%POLY(9)%FACE(J)%Surf
          IF(FACE<=ZERO) CYCLE
          !-------------------------------!
          !     GET RELATIVE PRESSURE.    !
          ! FROM BOTH SIDE OF CUT SURFACE !
          !-------------------------------!
          !Bijection : Scut=Icut <-> (icell1,icell2)
          !Surjection (icell1,icell2) ->> ( (mcell1,ibv1) , (mcell2,ibv2) )
          IEM1          = BRICK_LIST(NIN,IB)%POLY(ICELL1)%WhereIsMain(3)
          JM1           = BRICK_LIST(NIN,IB)%POLY(ICELL1)%WhereIsMain(1)
          IBv           = BRICK_LIST(NIN,IB)%Adjacent_Brick(J,4)
          IEv           = BRICK_LIST(NIN,IB)%Adjacent_Brick(J,1)
          Jv            = BRICK_LIST(NIN,IB)%Adjacent_Brick(J,5)
          IF(IEv == 0 .OR. IBv == 0) CYCLE    !closed surface is outside the domain
          NBCUTv        = BRICK_LIST(NIN,IBv)%NBCUT
            IF(NBCUTv == 0)THEN
              ICELL2        = 1
            ELSE
              ICELL2        = 9
            ENDIF
          IF(ICELL2 == 0) ICELL2 = 1          !closed surface hypothesis
          IEM2          = BRICK_LIST(NIN,IBv)%POLY(ICELL2)%WhereIsMain(3)
          JM2           = BRICK_LIST(NIN,IBv)%POLY(ICELL2)%WhereIsMain(1)
          IF(IEM1==IE)THEN
            IBM1 = IB
          ELSE
            IF(JM1<=NV46)THEN !1<= direct <=6   ;  indirect >= 10
C              if (jm1==0)then
C                print *, "i22for3.F:issue"
C                pause
C              endif
              IBM1     = BRICK_LIST(NIN,IB)%Adjacent_Brick(JM1,4)
            ELSE
              J1       = JM1/10
              J2       = MOD(JM1,10)
              IBM1     = BRICK_LIST(NIN,IB  )%Adjacent_Brick(J1,4)
              IBM1     = BRICK_LIST(NIN,IBM1)%Adjacent_Brick(J2,4)
            ENDIF
          ENDIF
          IF(IEM2 == IEv)THEN
            IBM2       = IBv
          ELSE
            IF(JM2<=NV46)THEN
              IBM2     = BRICK_LIST(NIN,IBv)%Adjacent_Brick(JM2,4)
            ELSE
              J1       = JM2/10
              J2       = MOD(JM2,10)
              IBM2     = BRICK_LIST(NIN,IB  )%Adjacent_Brick(J1,4)
              IBM2     = BRICK_LIST(NIN,IBM2)%Adjacent_Brick(J2,4)
            ENDIF
          ENDIF
          NG1          =  BRICK_LIST(NIN,IBM1)%NG
          NG2          =  BRICK_LIST(NIN,IBM2)%NG
          IDLOC1       =  BRICK_LIST(NIN,IBM1)%IDLOC
          IDLOC2       =  BRICK_LIST(NIN,IBM2)%IDLOC
          M1           =  BRICK_LIST(NIN,IBM1)%MACH
          M2           =  BRICK_LIST(NIN,IBM2)%MACH
          GBUF1        => ELBUF_TAB(NG1)%GBUF
          GBUF2        => ELBUF_TAB(NG2)%GBUF
          LLT1         =  IPARG(2,NG1)
          LLT2         =  IPARG(2,NG2)
          P1(I)        =  -THIRD * (GBUF1%SIG(IDLOC1        )+
     .                              GBUF1%SIG(IDLOC1 +LLT1  )+
     .                              GBUF1%SIG(IDLOC1 +LLT1*2) )
          P2(I)        =  -THIRD * (GBUF2%SIG(IDLOC2        )+
     .                              GBUF2%SIG(IDLOC2 +LLT2  )+
     .                              GBUF2%SIG(IDLOC2 +LLT2*2) )

          UN1          = BRICK_LIST(NIN,IB  )%POLY(ICELL1)%FACE(J)%U_N
          UN2          = BRICK_LIST(NIN,IBv )%POLY(ICELL2)%FACE(Jv)%U_N

          ZC1          = BRICK_LIST(NIN,IBM1)%RHOC
          ZC2          = BRICK_LIST(NIN,IBM1)%RHOC

          MF           = HALF*(M1+M2)
          THETA        = MIN(ONE,MF)

          P1(I)        = P1(I) + ZC1*UN1*THETA
          P2(I)        = P2(I) + ZC2*UN2*THETA
          DP(I)        = P1(I) - P2(I)
          !-------------------------------!
          N_SCUT_(1:3) = BRICK_LIST(NIN,IB)%N(J,1:3)*BRICK_LIST(NIN,IB)%POLY(9)%FACE(J)%Surf
          Q            = SUM( N_SURF(1:3,iabs(CAND_E(I))) * N_SCUT_(1:3)  )
          FFO          = DP(I) * Swet(8+J,I)
          IF(Q<ZERO) FFO = -FFO   !sign change to adjust normal direction (non oriented surface)

          !force au CoG
          FF(:)        = FFO * N_SURF(:,iabs(CAND_E(I)))
          !rint *, "F_cog=", FF(:)

          !distributed forces using weigthing coefficients
          FX1(I)       = FX1(I) + delta(1,8+J,I) * FF(1)
          FY1(I)       = FY1(I) + delta(1,8+J,I) * FF(2)
          FZ1(I)       = FZ1(I) + delta(1,8+J,I) * FF(3)

          FX2(I)       = FX2(I) + delta(2,8+J,I) * FF(1)
          FY2(I)       = FY2(I) + delta(2,8+J,I) * FF(2)
          FZ2(I)       = FZ2(I) + delta(2,8+J,I) * FF(3)

          FX3(I)       = FX3(I) + delta(3,8+J,I) * FF(1)
          FY3(I)       = FY3(I) + delta(3,8+J,I) * FF(2)
          FZ3(I)       = FZ3(I) + delta(3,8+J,I) * FF(3)

          IF(NP_RECT(I)==4)THEN
          FX4(I)       = FX4(I) + delta(4,8+J,I) * FF(1)
          FY4(I)       = FY4(I) + delta(4,8+J,I) * FF(2)
          FZ4(I)       = FZ4(I) + delta(4,8+J,I) * FF(3)
          ENDIF

C           print *, "FF=", FF(1), ITAB(IRECT(1:4,CE_LOC(I)))

          FXI(I)       = FXI(I) + FF(1)
          FYI(I)       = FYI(I) + FF(2)
          FZI(I)       = FZI(I) + FF(3)

          if(ibug22_fcont==-1)then
            print *, "#################################"
            print *, "##I22FOR ; facette I=", I
            print *, "#################################"
            print *, "   CELL :      ", IXS(11,BRICK_LIST(NIN,CB_LOC(I))%ID)
            print *, "   SHELL  :      ", ITAB(IRECT(1:4,iabs(CE_LOC(I))))
            print *, "   NCYCLE :      ", NCYCLE
            print *, "   P1           =", P1(I)
            print *, "   P2           =", P2(I)
            print *, "   DP           =", DP(I)
            print *, "   Swet(8+J,I)  =", Swet(8+J,I)
            print *, "   F=DP*Swet    =", DP(I) * Swet(8+J,I)
            print *, "   Slag         =", Slag
            print *, "   N_SURF()     =", N_SURF(:,iabs(CAND_E(I)))
            print *, "   FN           =", FFO
            print *, "-----------------"
            write (*,FMT='(A,4E20.12,A,E20.12)') "   DELTA  :      ", delta(1:4,8+J,I), "   | SUM=", SUM(delta(1:4,8+J,I))
            write(*,FMT='(A,I8,A,3E30.16)')"FX(",itab(IRECT(1,iabs(CE_LOC(I)))),") = ", FX1(I),FY1(I),FZ1(I)
            write(*,FMT='(A,I8,A,3E30.16)')"FX(",itab(IRECT(2,iabs(CE_LOC(I)))),") = ", FX2(I),FY2(I),FZ2(I)
            write(*,FMT='(A,I8,A,3E30.16)')"FX(",itab(IRECT(3,iabs(CE_LOC(I)))),") = ", FX3(I),FY3(I),FZ3(I)
            write(*,FMT='(A,I8,A,3E30.16)')"FX(",itab(IRECT(4,iabs(CE_LOC(I)))),") = ", FX4(I),FY4(I),FZ4(I)
            print *, "-----------------"
            print *, "   FXI(I)=",FXI(I)
            print *, "   FYI(I)=",FYI(I)
            print *, "   FZI(I)=",FZI(I)
            print *, ""
          endif
        ENDDO!next J





















      ENDDO!next I

C---------------------------------
C     SAUVEGARDE DE L'IMPULSION NORMALE
C---------------------------------
      FSAV1     = ZERO
      FSAV2     = ZERO
      FSAV3     = ZERO
      FSAV8     = ZERO
      FSAV9     = ZERO
      FSAV10    = ZERO
      FSAV11    = ZERO
      DO I=1,JLT
       IMPX     = FXI(I)*DT12
       IMPY     = FYI(I)*DT12
       IMPZ     = FZI(I)*DT12
       FNI(I)   = SQRT(FXI(I)*FXI(I) + FYI(I)*FYI(I) + FZI(I)*FZI(I))
       !FNX,FNY,FNZ
       FSAV1    = FSAV1 +IMPX
       FSAV2    = FSAV2 +IMPY
       FSAV3    = FSAV3 +IMPZ
       !|FNX|,|FNY|,|FNZ|
       FSAV8    = FSAV8 +ABS(IMPX)
       FSAV9    = FSAV9 +ABS(IMPY)
       FSAV10   = FSAV10+ABS(IMPZ)

       FSAV11   = FSAV11+FNI(I)*DT12
      ENDDO
#include "lockon.inc"
        FSAV(1) = FSAV(1) + FSAV1
        FSAV(2) = FSAV(2) + FSAV2
        FSAV(3) = FSAV(3) + FSAV3
        FSAV(8) = FSAV(8) + FSAV8
        FSAV(9) = FSAV(9) + FSAV9
        FSAV(10)= FSAV(10)+ FSAV10
        FSAV(11)= FSAV(11)+ FSAV11
#include "lockoff.inc"
C
      IF(ISENSINT(1)/=0) THEN
        DO I=1,JLT
          FSAVPARIT(1,1,I) =  FXI(I)
          FSAVPARIT(1,2,I) =  FYI(I)
          FSAVPARIT(1,3,I) =  FZI(I)
        ENDDO
      ENDIF

      !---------------------------------!
      !       ASSEMBLY PARITH ON/OFF    !
      !---------------------------------!
      !these array could be removed from inter22 later during source cleaing
      H1 = ZERO
      H2 = ZERO
      H3 = ZERO
      H4 = ZERO

      SELECT CASE (IPARIT)
        CASE (0)
c          print *, "PARITH=OFF (IPARIT=0)"
          CALL I22ASS0(
     1             JLT   ,IX1   ,IX2   ,IX3  ,IX4  ,
     2             NSVG  ,H1    ,H2    ,H3   ,H4   ,STIF   ,
     3             FX1   ,FY1   ,FZ1   ,FX2  ,FY2  ,FZ2    ,
     4             FX3   ,FY3   ,FZ3   ,FX4  ,FY4  ,FZ4    ,
     5             FXI   ,FYI   ,FZI   ,A    ,STIFN,NIN    ,
     6             INTTH ,BID   ,BID   ,BID  ,BID  ,BID    ,
     7             BID )
         CASE (3)
c          print *, "PARITH=ON (new) (IPARIT=3)"
          CALL I7ASS3(
     1             JLT   ,IX1   ,IX2   ,IX3  ,IX4  ,
     2             NSVG  ,H1    ,H2    ,H3   ,H4   ,STIF   ,
     3             FX1   ,FY1   ,FZ1   ,FX2  ,FY2  ,FZ2    ,
     4             FX3   ,FY3   ,FZ3   ,FX4  ,FY4  ,FZ4    ,
     5             FXI   ,FYI   ,FZI   ,A    ,STIFN)
         CASE DEFAULT
          CALL I22ASS2(
     1             JLT   ,IX1   ,IX2   ,IX3    ,IX4    ,ITAB   ,
     2             NSVG  ,H1    ,H2    ,H3     ,H4     ,STIF   ,
     3             FX1   ,FY1   ,FZ1   ,FX2    ,FY2    ,FZ2    ,
     4             FX3   ,FY3   ,FZ3   ,FX4    ,FY4    ,FZ4    ,
     5             FXI   ,FYI   ,FZI   ,FSKYI  ,ISKY   ,NISKYFI,
     6             NIN   ,NOINT ,INTTH ,BID    ,BID    ,BID    ,
     7             BID   ,BID   ,BID   ,CB_LOC ,CE_LOC ,IRECT  ,
     8             IXS)

      END SELECT

C
      !---------------------------------!
      !       /ANIM/VECT/CONT           !
      !---------------------------------!
      IF(ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT>0)THEN
#include "lockon.inc"
           DO I=1,JLT
             IB    = CB_LOC(I)
             NBCUT = BRICK_LIST(NIN,IB)%NBCUT
             IF(NBCUT == 0) CYCLE
             FCONT(1,IX1(I)) =FCONT(1,IX1(I)) + FX1(I)
             FCONT(2,IX1(I)) =FCONT(2,IX1(I)) + FY1(I)
             FCONT(3,IX1(I)) =FCONT(3,IX1(I)) + FZ1(I)
             FCONT(1,IX2(I)) =FCONT(1,IX2(I)) + FX2(I)
             FCONT(2,IX2(I)) =FCONT(2,IX2(I)) + FY2(I)
             FCONT(3,IX2(I)) =FCONT(3,IX2(I)) + FZ2(I)
             FCONT(1,IX3(I)) =FCONT(1,IX3(I)) + FX3(I)
             FCONT(2,IX3(I)) =FCONT(2,IX3(I)) + FY3(I)
             FCONT(3,IX3(I)) =FCONT(3,IX3(I)) + FZ3(I)
             FCONT(1,IX4(I)) =FCONT(1,IX4(I)) + FX4(I)
             FCONT(2,IX4(I)) =FCONT(2,IX4(I)) + FY4(I)
             FCONT(3,IX4(I)) =FCONT(3,IX4(I)) + FZ4(I)
             if(ibug22_fcont==-1)then
               print *, "#################################"
               print *, "##I22FOR ; facette I=", I
               print *, "##    FCONT /ANIM/VECT/CONT     #"
               print *, "#################################"
               print *, "   CELL   :       ", IXS(11,BRICK_LIST(NIN,CB_LOC(I))%ID)
               print *, "   SHELL  :       ", ITAB(IRECT(1:4,iabs(CE_LOC(I))))
               print *, "   NCYCLE :       ", NCYCLE
               print *, "------------------"
               write(*,FMT='(A,I8,A,3E30.16)')"FCONT(",itab(IRECT(1,iabs(CE_LOC(I)))),") = ", FCONT(1:3,IX1(I))
               write(*,FMT='(A,I8,A,3E30.16)')"FCONT(",itab(IRECT(2,iabs(CE_LOC(I)))),") = ", FCONT(1:3,IX2(I))
               write(*,FMT='(A,I8,A,3E30.16)')"FCONT(",itab(IRECT(3,iabs(CE_LOC(I)))),") = ", FCONT(1:3,IX3(I))
               write(*,FMT='(A,I8,A,3E30.16)')"FCONT(",itab(IRECT(4,iabs(CE_LOC(I)))),") = ", FCONT(1:3,IX4(I))
               print *, ""
             endif
           ENDDO
#include "lockoff.inc"
      ENDIF


      ! A TRAITER PLUS TARD

      !---------------------------------!
      !       /ANIM/VECT/PCONT          !
      !---------------------------------!
      IF(ANIM_V(12)+OUTP_V(12)+H3D_DATA%N_VECT_PCONT>0.AND.
     .          (TT>=TANIM.OR.TT>=TOUTP.OR.TT>=H3D_DATA%TH3D.OR.
     .              (MANIM>=4.AND.MANIM<=15).OR.H3D_DATA%MH3D/=0))THEN
#include "lockon.inc"
           DO I=1,JLT
             FNCONT(1,IX1(I)) =FNCONT(1,IX1(I)) + FXI(I)
             FNCONT(2,IX1(I)) =FNCONT(2,IX1(I)) + FYI(I)
             FNCONT(3,IX1(I)) =FNCONT(3,IX1(I)) + FZI(I)
             FNCONT(1,IX2(I)) =FNCONT(1,IX2(I)) + FXI(I)
             FNCONT(2,IX2(I)) =FNCONT(2,IX2(I)) + FYI(I)
             FNCONT(3,IX2(I)) =FNCONT(3,IX2(I)) + FZI(I)
             FNCONT(1,IX3(I)) =FNCONT(1,IX3(I)) + FXI(I)
             FNCONT(2,IX3(I)) =FNCONT(2,IX3(I)) + FYI(I)
             FNCONT(3,IX3(I)) =FNCONT(3,IX3(I)) + FZI(I)
             FNCONT(1,IX4(I)) =FNCONT(1,IX4(I)) + FXI(I)
             FNCONT(2,IX4(I)) =FNCONT(2,IX4(I)) + FYI(I)
             FNCONT(3,IX4(I)) =FNCONT(3,IX4(I)) + FZI(I)
             if(ibug22_fcont==-1)then
               print *, "#################################"
               print *, "##I22FOR ; facette I=", I
               print *, "##    FCONT /ANIM/VECT/CONT     #"
               print *, "#################################"
               print *, "   CELL   :        ", IXS(11,BRICK_LIST(NIN,CB_LOC(I))%ID)
               print *, "   SHELL  :        ", ITAB(IRECT(1:4,iabs(CE_LOC(I))))
               print *, "   NCYCLE :        ", NCYCLE
               print *, "-------------------"
               write(*,FMT='(A,I8,A,3E30.16)')"FNCONT(",itab(IRECT(1,iabs(CE_LOC(I)))),") = ", FNCONT(1:3,IX1(I))
               write(*,FMT='(A,I8,A,3E30.16)')"FNCONT(",itab(IRECT(2,iabs(CE_LOC(I)))),") = ", FNCONT(1:3,IX2(I))
               write(*,FMT='(A,I8,A,3E30.16)')"FNCONT(",itab(IRECT(3,iabs(CE_LOC(I)))),") = ", FNCONT(1:3,IX3(I))
               write(*,FMT='(A,I8,A,3E30.16)')"FNCONT(",itab(IRECT(4,iabs(CE_LOC(I)))),") = ", FNCONT(1:3,IX4(I))

               print *, ""
             endif
           ENDDO
#include "lockoff.inc"
      ENDIF
C-----------------------------------------------------



C-----------------------------------------------------
C
      if(ibug22_fcont==-1)then
        print *, "================================================="
        print *, ""
        print *, ""
        print *, ""
      endif

      RETURN
      END
