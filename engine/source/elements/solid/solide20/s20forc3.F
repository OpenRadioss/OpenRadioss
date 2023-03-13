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
Chd|  S20FORC3                      source/elements/solid/solide20/s20forc3.F
Chd|-- called by -----------
Chd|        FORINT                        source/elements/forint.F      
Chd|-- calls ---------------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        S20BILAN                      source/elements/solid/solide20/s20bilan.F
Chd|        S20COOR3                      source/elements/solid/solide20/s20coor3.F
Chd|        S20CUMU3                      source/elements/solid/solide20/s20cumu3.F
Chd|        S20CUMU3P                     source/elements/solid/solide20/s20cumu3.F
Chd|        S20DEFO3                      source/elements/solid/solide20/s20defo3.F
Chd|        S20DERI3                      source/elements/solid/solide20/s20deri3.F
Chd|        S20FINT3                      source/elements/solid/solide20/s20fint3.F
Chd|        S20RST                        source/elements/solid/solide20/s20deri3.F
Chd|        S20TEMPCG                     source/elements/solid/solide20/s20tempcg.F
Chd|        S20THERM                      source/elements/solid/solide20/s20therm.F
Chd|        SMALLA3                       source/elements/solid/solide/small3.F
Chd|        SMALLB3                       source/elements/solid/solide/small3.F
Chd|        SRHO3                         source/elements/solid/solide/srho3.F
Chd|        SROTA3                        source/elements/solid/solide/srota3.F
Chd|        SSTRA3                        source/elements/solid/solide/sstra3.F
Chd|        SXFILLOPT                     source/elements/solid/solide10/sxfillopt.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MMAIN_MOD                     source/materials/mat_share/mmain.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE S20FORC3(ELBUF_TAB,NG     ,
     1                    PM       ,GEO    ,IXS    ,X      ,
     2                    A        ,V      ,MS     ,W      ,FLUX   ,
     3                    FLU1     ,VEUL    ,FV     ,ALE_CONNECT  ,IPARG   ,
     4                    TF       ,NPF     ,BUFMAT ,PARTSAV,NLOC_DMG,
     5                    DT2T     ,NELTST ,ITYPTST,STIFN  ,FSKY   ,
     6                    IADS     ,OFFSET ,EANI   ,IPARTS ,
     7                    IXS20    ,IADS20 ,NEL    ,FX     ,
     8                    FY       ,FZ     ,VOLNP  ,RX     ,RY     , 
     9                    RZ       ,SX     ,SY     ,SZ     ,TX     ,
     A                    TY       ,TZ     ,STIG   ,STIN   ,UL     ,
     B                    XX       ,YY     ,ZZ     ,VX     ,VY     , 
     C                    VZ       ,VDXX   ,VDYY   ,VDZZ   ,DNIDR  ,
     D                    DNIDS    ,DNIDT  ,PX     ,PY     ,PZ     ,
     G                    IPM      ,ISTRAIN,
     H                    TEMP     ,FTHE   ,FTHESKY,IEXPAN ,GRESAV ,
     I                    GRTH     ,IGRTH  ,TABLE  ,IGEO   ,VOLN   ,
     J                    CONDN    ,CONDNSKY,
     K                    ITASK    ,IOUTPRT ,MATPARAM_TAB,H3D_STRAIN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MMAIN_MOD
      USE TABLE_MOD
      USE ELBUFDEF_MOD            
      USE NLOCAL_REG_MOD
      USE ALE_CONNECTIVITY_MOD
      USE MATPARAM_DEF_MOD
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
#include      "vect01_c.inc"
#include      "parit_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   L o c a l   P a r a m e t e r s
C-----------------------------------------------
      INTEGER NIPMAX,NPE
      PARAMETER (NIPMAX=81)
      PARAMETER (NPE=20)
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*), IPARG(NPARG,NGROUP),NPF(*),
     .        IADS(8,*),IPARTS(*),IXS20(12,*),IADS20(12,*),IPM(*),
     .        GRTH(*),IGRTH(*),IGEO(*),ITASK,IOUTPRT
      INTEGER NELTST,ITYPTST,OFFSET,NEL,ISTRAIN,
     .        IEXPAN,NG,H3D_STRAIN
      my_real
     .   DT2T
      my_real
     .   PM(*), GEO(*), X(*), A(*), V(*), MS(*), W(*), FLUX(6,*),FLU1(*), 
     .   VEUL(*), FV(*), TF(*),TEMP(*), FTHE(*),FTHESKY(*),GRESAV(*), 
     .   BUFMAT(*),PARTSAV(*),STIFN(*), FSKY(*),EANI(*),VOLN(MVSIZ)
      my_real
     .   FX(MVSIZ,NPE),FY(MVSIZ,NPE),FZ(MVSIZ,NPE),VOLNP(MVSIZ,NIPMAX),
     .   RX(MVSIZ,NIPMAX) , RY(MVSIZ,NIPMAX) , RZ(MVSIZ,NIPMAX) ,
     .   SX(MVSIZ,NIPMAX) , SY(MVSIZ,NIPMAX) , SZ(MVSIZ,NIPMAX) ,
     .   TX(MVSIZ,NIPMAX),TY(MVSIZ,NIPMAX),TZ(MVSIZ,NIPMAX),
     .   STIG(MVSIZ,NPE),STIN(MVSIZ,NPE),UL(MVSIZ,NPE),
     .   XX(MVSIZ,NPE), YY(MVSIZ,NPE), ZZ(MVSIZ,NPE),
     .   VX(MVSIZ,NPE),VY(MVSIZ,NPE),VZ(MVSIZ,NPE),
     .   VDXX(MVSIZ,NPE), VDYY(MVSIZ,NPE), VDZZ(MVSIZ,NPE),
     .   DNIDR(MVSIZ,NPE),DNIDS(MVSIZ,NPE),DNIDT(MVSIZ,NPE),
     .   PX(MVSIZ,NPE,NIPMAX),PY(MVSIZ,NPE,NIPMAX),PZ(MVSIZ,NPE,NIPMAX),
     .   CONDN(*),CONDNSKY(*)
      TYPE (TTABLE) TABLE(*)
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (NLOCAL_STR_)  , TARGET :: NLOC_DMG 
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
      TYPE(MATPARAM_STRUCT_)  , DIMENSION(NUMMAT) :: MATPARAM_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,NF1,NF2,ILAY,IP,IR,IS,IT,NPTT,NPTS,NPTR,IFLAG,IBID
      INTEGER IBIDON(1)

      INTEGER MXT(MVSIZ),NGL(MVSIZ),NGEO(MVSIZ), IPERM1(NPE),
     .IPERM2(NPE)
      my_real
     . VD2(MVSIZ) , DVOL(MVSIZ),DELTAX(MVSIZ),
     . VIS(MVSIZ) , QVIS(MVSIZ), CXX(MVSIZ) ,
     . S1(MVSIZ)  , S2(MVSIZ)  , S3(MVSIZ)  ,
     . S4(MVSIZ)  , S5(MVSIZ)  , S6(MVSIZ)  ,
     . DXX(MVSIZ) , DYY(MVSIZ) , DZZ(MVSIZ) ,
     . D4(MVSIZ)  , D5(MVSIZ)  , D6(MVSIZ)  , 
     . VDX(MVSIZ),VDY(MVSIZ),VDZ(MVSIZ),SSP_EQ(MVSIZ),
     . AIRE(MVSIZ),CONDE(MVSIZ),CONDEG(MVSIZ,NPE),AMU(MVSIZ),DIVDE(MVSIZ)

      ! Variables utilisees en argument par les materiaux.
      my_real
     .   STI(MVSIZ), WXX(MVSIZ) , WYY(MVSIZ) , WZZ(MVSIZ)

      ! Variables utilisees en argument par les materiaux si SPH uniquement.
      my_real
     .   MUVOID(MVSIZ)

      ! Variables void MMAIN 
      my_real
     . SIGY(MVSIZ),ET(MVSIZ),GAMA(MVSIZ,6),
     . R1_FREE(MVSIZ),R3_FREE(MVSIZ),R4_FREE(MVSIZ),
     . TEMPEL(MVSIZ),DIE(MVSIZ),THEM(MVSIZ,NPE)
     
      ! Variables utilisees dans les routines solides uniquement (en arguments).
      INTEGER NC(MVSIZ,NPE)
      my_real
     .  OFF(MVSIZ) , RHOO(MVSIZ),NI(NPE,NIPMAX),
     .  DXY(MVSIZ),DYX(MVSIZ),
     .  DYZ(MVSIZ),DZY(MVSIZ),VOLG(MVSIZ),
     .  DZX(MVSIZ),DXZ(MVSIZ),BID(MVSIZ),AA,WI,BB, MBID(1)
      my_real VARNL(NEL)
      DOUBLE PRECISION
     .  VOLDP(MVSIZ,NIPMAX)
C-----------------------------------------------
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
      TYPE(BUF_MAT_) ,POINTER :: MBUF     
c------------------------------------------------------------
      my_real
     .  W_GAUSS(9,9),A_GAUSS(9,9)
      DATA W_GAUSS / 
c---
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
c------------------------------------------------------------
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
c------------------------------------------------------------
C     Variables utilisees en argument par les materiaux.
      DATA IPERM1/0,0,0,0,0,0,0,0,1,2,3,4,1,2,3,4,5,6,7,8/
      DATA IPERM2/0,0,0,0,0,0,0,0,2,3,4,1,5,6,7,8,6,7,8,5/
C-----------------------------------------------
C   S o u r c e  L i n e s
C=======================================================================
      GBUF  => ELBUF_TAB(NG)%GBUF
c
      BID    = ZERO
      IBID   = 0
      IBIDON = 0
      NF1  = NFT+1
      NF2  = NF1-(NUMELS8+NUMELS10)
C     
      CALL S20COOR3(
     1   X,IXS(1,NF1),IXS20(1,NF2),V,W, IPERM1,IPERM2,NPE,
     2   XX, YY, ZZ, VX, VY, VZ,VDXX, VDYY, VDZZ, 
     3   VDX,VDY,VDZ,VD2,VIS,GBUF%OFF,OFF,GBUF%SMSTR,
     4   NC,NGL,MXT,NGEO,   FX,FY,FZ,STIG,
     5   GBUF%SIG,GBUF%EINT,GBUF%RHO,GBUF%QVIS,GBUF%PLA,
     6   GBUF%EPSD,GBUF%G_PLA,GBUF%G_EPSD,NEL,CONDEG)
c
      DO N=1,NPE
        DO I=LFT,LLT
          UL(I,N) = ZERO
        ENDDO
      ENDDO
      DO I=LFT,LLT
        VOLG(I) = ZERO
      ENDDO
      IF(JTHE < 0) THEM(LFT:LLT,1:NPE) = ZERO
C
C-----------------------------
C     POINTS D' INTEGRATION 
C-----------------------------
      NPTR = ELBUF_TAB(NG)%NPTR
      NPTS = ELBUF_TAB(NG)%NPTS
      NPTT = ELBUF_TAB(NG)%NPTT
C-------------
      ILAY = 1
      DO IT=1,NPTT
       DO IS=1,NPTS
        DO IR=1,NPTR
         LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,IT)


         IP = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
         WI = W_GAUSS(IR,NPTR)*W_GAUSS(IS,NPTS)*W_GAUSS(IT,NPTT)
c
         CALL S20RST(
     1      A_GAUSS(IR,NPTR),A_GAUSS(IS,NPTS),A_GAUSS(IT,NPTT),NI(1,IP),
     2      DNIDR         ,DNIDS         ,DNIDT         )
C
         CALL S20DERI3(
     1    NGL,OFF,A_GAUSS(IR,NPTR),A_GAUSS(IS,NPTS),A_GAUSS(IT,NPTT),WI,
     2    DNIDR    ,DNIDS    ,DNIDT   ,RX(1,IP)  ,RY(1,IP)  ,RZ(1,IP)  ,
     3    SX(1,IP) ,SY(1,IP) ,SZ(1,IP),TX(1,IP)  ,TY(1,IP)  ,TZ(1,IP)  ,
     4    XX       ,YY       ,ZZ      ,PX(1,1,IP),PY(1,1,IP),PZ(1,1,IP),
     5    VOLNP(1,IP),DELTAX ,STIN    ,NI(1,IP)  ,VOLG      ,UL        ,
     6    IR       ,IS       ,IT      ,VOLDP(1,IP))
C
        ENDDO
       ENDDO
      ENDDO
C-------------
C
      DO I=LFT,LLT
        AA = MAX(UL(I,1),UL(I,2),UL(I,3),UL(I,4),
     .           UL(I,5),UL(I,6),UL(I,7),UL(I,8))
        BB = MAX(UL(I,9) ,UL(I,10),UL(I,11),UL(I,12),UL(I,13),UL(I,14),
     .           UL(I,15),UL(I,16),UL(I,17),UL(I,18),UL(I,19),UL(I,20))
        AA = AA*SIXTY4
        BB = BB*EIGHTY16/SEVEN
        DELTAX(I) = SQRT(TWO*VOLG(I)/MAX(AA,BB))
      ENDDO
C
C-------------
      ILAY = 1
      DO IT=1,NPTT
       DO IS=1,NPTS
        DO IR=1,NPTR

         LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,IT)
         IP = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
         WI = W_GAUSS(IR,NPTR)*W_GAUSS(IS,NPTS)*W_GAUSS(IT,NPTT)
C--------
         CALL S20DEFO3(
     .    NPE,  PX(1,1,IP), PY(1,1,IP), PZ(1,1,IP), VX, VY, VZ,
     .    DXX, DXY, DXZ, DYX, DYY, DYZ, DZX, DZY, DZZ, D4, D5, D6,
     .    WXX, WYY, WZZ,LBUF%RHO,RHOO,VOLNP(1,IP),VOLN)
C                                                                     
         DIVDE(1:NEL) = DT1*(DXX(1:NEL)+ DYY(1:NEL)+ DZZ(1:NEL))   
         CALL SRHO3( PM         , LBUF%VOL  , LBUF%RHO, LBUF%EINT, DIVDE,
     .               FLUX(1,NF1), FLU1(NF1) , VOLN    , DVOL     , NGL, MXT, OFF,
     .               0          , GBUF%TAG22 ,VOLDP(1,IP),LBUF%VOL0DP,AMU  ,GBUF%OFF)
     
         CALL SROTA3(LBUF%SIG,S1   ,S2   ,S3   ,S4 ,
     .               S5      ,S6   ,WXX  ,WYY  ,WZZ,
     .               NEL     )
C-----------------------------
C        SMALL STRAIN
C-----------------------------
         CALL SMALLA3(GBUF%SMSTR,GBUF%OFF,OFF,WXX,WYY,WZZ,NEL)
C
C         for heat transfert
C
          IF(JTHE < 0 ) THEN
             CALL S20TEMPCG(LFT,LLT,NPE, NC,NI(1,IP),TEMP,TEMPEL)
          ENDIF             
C------------------------------------------------------
C         CALCUL DES CONTRAINTES SUIVANT LOIS CONSTITUTIVES
C------------------------------------------------------
          CALL MMAIN(
     1    ELBUF_TAB,NG       ,PM       ,GEO           ,FV	     ,ALE_CONNECT    ,
     2    IXS      ,IPARG    ,V        ,TF       ,NPF      ,BUFMAT   ,
     3    STI      ,X        ,DT2T     ,NELTST   ,ITYPTST  ,OFFSET   ,
     4    NEL      ,W        ,OFF      ,NGEO          ,
     6    MXT      ,NGL      ,VOLN     ,VD2      ,DVOL     ,DELTAX   ,
     7    VIS      ,QVIS     ,CXX      ,S1       ,S2       ,S3       ,
     8    S4       ,S5       ,S6       ,DXX      ,DYY      ,DZZ      ,
     9    D4       ,D5       ,D6       ,WXX      ,WYY      ,WZZ      ,
     A    RX(1,IP) ,RY(1,IP) ,RZ(1,IP) ,SX(1,IP) ,SY(1,IP) ,SZ(1,IP) ,
     B    VDX      ,VDY      ,VDZ      ,MUVOID   ,SSP_EQ   ,AIRE     ,
     C    SIGY     ,ET       ,R1_FREE  ,LBUF%PLA,     R3_FREE	     ,AMU      ,
     D    BID      ,BID      ,BID      ,BID      ,BID      ,BID      , 
     E    BID      ,BID      ,BID      ,IPM      ,GAMA     ,BID      ,
     F    BID      ,BID      ,BID      ,BID      ,BID      ,BID      ,
     G    ISTRAIN  ,TEMPEL   ,DIE      ,IEXPAN   ,ILAY     ,MBID     ,
     H    MBID     ,IR       ,IS       ,IT       ,TABLE    ,BID      ,
     I    BID      ,BID      ,BID      ,IPARG(1,NG)   ,IGEO     ,CONDE    ,
     J    ITASK    ,NLOC_DMG ,VARNL    ,MATPARAM_TAB  ,H3D_STRAIN)
c     
          IF (ISTRAIN == 1) THEN 
            CALL SSTRA3(DXX,DYY,DZZ,D4,D5, 
     .                  D6,LBUF%STRA, WXX, WYY, WZZ,
     .                  OFF,NEL)
          ENDIF

C-----------------------------
C         SMALL STRAIN 
C-----------------------------
          CALL SMALLB3(GBUF%OFF,OFF)
C----------------------------
C         INTERNAL FORCES
C----------------------------
c
          CALL S20FINT3(
     1    NPE,LBUF%SIG,PX(1,1,IP), PY(1,1,IP), PZ(1,1,IP),SSP_EQ,
     2    FX,FY,FZ,VOLN,QVIS,STIG,STIN,
     3    LBUF%EINT,LBUF%RHO,LBUF%QVIS,LBUF%PLA,LBUF%EPSD,GBUF%EPSD,
     4    GBUF%SIG,GBUF%EINT,GBUF%RHO,GBUF%QVIS,GBUF%PLA,WI,VOLG,
     5    LBUF%VOL,GBUF%VOL,GBUF%G_PLA,NEL,CONDE,DELTAX,CONDEG)
C
C-------------------------
c    finite element heat transfert  
C--------------------------
          IF(JTHE < 0) THEN
            CALL S20THERM(NPE,PM     ,MXT    ,NC,VOLN, 
     .               PX(1,1,IP), PY(1,1,IP), PZ(1,1,IP),NI(1,IP),
     .               DT1 ,TEMP  ,TEMPEL ,DIE  , THEM ,
     .               GBUF%OFF,LBUF%OFF) 
          ENDIF  
        ENDDO
       ENDDO
      ENDDO
C--------------------------
C     BILANS PAR MATERIAU
C--------------------------
      IFLAG=MOD(NCYCLE,NCPRI)
      IF (IOUTPRT>0)THEN
           CALL S20BILAN(PARTSAV,GBUF%EINT,GBUF%RHO,VOLG,
     .                   VX, VY, VZ,IPARTS,GBUF%VOL,
     .                   GRESAV,GRTH,IGRTH,IEXPAN,GBUF%EINTTH,
     .                   GBUF%FILL, XX, YY, ZZ,ITASK,IPARG(1,NG),
     .                   GBUF%OFF)
      ENDIF
C
c-----------------------------
      IF(NFILSOL/=0) CALL SXFILLOPT(NPE,GBUF%FILL,STIG,FX,FY,FZ)
c-----------------------------
      IF (IPARIT == 0)THEN
        CALL S20CUMU3(GBUF%OFF,A,NC,STIFN,STIG,FX,FY,
     2                FZ, IPERM1,IPERM2,NPE, THEM,FTHE,CONDN,CONDEG)
      ELSE
        CALL S20CUMU3P(GBUF%OFF,STIG,FSKY,FSKY,IADS(1,NF1),FX,FY,
     2           FZ,IADS20(1,NF2),NC,IPERM1,IPERM2,NPE,THEM, FTHESKY,
     3           CONDNSKY,CONDEG)
      ENDIF
C-----------
      RETURN
      END
