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
Chd|  S16FORC3                      source/elements/thickshell/solide16/s16forc3.F
Chd|-- called by -----------
Chd|        FORINT                        source/elements/forint.F      
Chd|-- calls ---------------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        S16BILAN                      source/elements/thickshell/solide16/s16bilan.F
Chd|        S16DERI3                      source/elements/thickshell/solide16/s16deri3.F
Chd|        S16RST                        source/elements/thickshell/solide16/s16rst.F
Chd|        S16SIGP3                      source/elements/thickshell/solide16/s16sigp3.F
Chd|        S16SIGS3                      source/elements/thickshell/solide16/s16sigs3.F
Chd|        S20COOR3                      source/elements/solid/solide20/s20coor3.F
Chd|        S20CUMU3                      source/elements/solid/solide20/s20cumu3.F
Chd|        S20CUMU3P                     source/elements/solid/solide20/s20cumu3p.F
Chd|        S20DEFO3                      source/elements/solid/solide20/s20defo3.F
Chd|        S20FINT3                      source/elements/solid/solide20/s20fint3.F
Chd|        S20TEMPCG                     source/elements/solid/solide20/s20tempcg.F
Chd|        S20THERM                      source/elements/solid/solide20/s20therm.F
Chd|        SDLENSH2                      source/elements/thickshell/solidec/sdlensh2.F
Chd|        SMALLA3                       source/elements/solid/solide/smalla3.F
Chd|        SMALLB3                       source/elements/solid/solide/smallb3.F
Chd|        SRHO3                         source/elements/solid/solide/srho3.F
Chd|        SROTA3                        source/elements/solid/solide/srota3.F
Chd|        SSTRA3                        source/elements/solid/solide/sstra3.F
Chd|        SXFILLOPT                     source/elements/solid/solide10/sxfillopt.F
Chd|        TSHGEODEL3                    source/elements/thickshell/solidec/tshgeodel3.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        DT_MOD                        share/modules/dt_mod.F        
Chd|        MAT_ELEM_MOD                  ../common_source/modules/mat_elem/mat_elem_mod.F
Chd|        MMAIN_MOD                     source/materials/mat_share/mmain.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE S16FORC3(ELBUF_TAB,NG      ,
     1                    PM       ,GEO     ,IXS    ,X       ,
     2                    A        ,V       ,MS     ,W       ,FLUX    ,
     3                    FLU1     ,VEUL    ,FV     ,ALE_CONNECT   ,IPARG   ,
     4                    TF       ,NPF     ,BUFMAT ,PARTSAV ,NLOC_DMG,
     5                    DT2T     ,NELTST  ,ITYPTST,STIFN   ,FSKY    ,
     6                    IADS     ,OFFSET  ,EANI   ,IPARTS  ,
     7                    IXS16    ,IADS16  ,NEL    ,FX      ,
     8                    FY       ,FZ      ,VOLNP  ,SIGS    ,RX      ,
     9                    RY       ,RZ      ,SX     ,SY      ,SZ      ,
     A                    TX       ,TY      ,TZ     ,STIG    ,STIN    ,
     B                    UL       ,XX      ,YY     ,ZZ      ,VX      ,
     C                    VY       ,VZ      ,VDXX   ,VDYY    ,VDZZ    ,
     D                    DNIDR    ,DNIDS   ,DNIDT  ,PX      ,PY      ,
     E                    PZ       ,ICP     ,ICSIG  ,
     F                    IPM      ,ISTRAIN ,TEMP   ,FTHE    ,FTHESKY ,
     G                    IEXPAN   ,GRESAV  ,GRTH   ,IGRTH   ,TABLE   ,
     H                    IGEO     ,VOLN    ,CONDN  ,CONDNSKY,ITASK  ,
     I                    IOUTPRT  ,MAT_ELEM,H3D_STRAIN  ,DT    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MMAIN_MOD
      USE TABLE_MOD
      USE MAT_ELEM_MOD            
      USE NLOCAL_REG_MOD
      USE ALE_CONNECTIVITY_MOD
      USE DT_MOD
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
C   l o c a l   P a r a m e t e r s
C-----------------------------------------------
      INTEGER NIPMAX,NPE
      PARAMETER (NIPMAX=81)
      PARAMETER (NPE=16)
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*), IPARG(NPARG,NGROUP),NPF(*),
     .        IPARTS(*),IXS16(8,*),IADS16(8,*),IPM(*),GRTH(*),IGRTH(*),
     .        IGEO(*),IADS(8,*),ITASK,IOUTPRT
      INTEGER NELTST,ITYPTST,OFFSET,NEL,ICP,ICSIG,ISTRAIN,IEXPAN,NG,H3D_STRAIN
      my_real
     .   DT2T
      my_real
     .   PM(NPROPM,*), GEO(NPROPG,*), X(*), A(*), V(*), MS(*), W(*), 
     .   FLUX(6,*),FLU1(*), VEUL(*), FV(*), TF(*), 
     .   BUFMAT(*),PARTSAV(*),STIFN(*), FSKY(*),EANI(*)
      my_real
     .  FX(MVSIZ,NPE),FY(MVSIZ,NPE),FZ(MVSIZ,NPE),
     .  VOLNP(MVSIZ,NIPMAX),SIGS(MVSIZ,NIPMAX),
     .  RX(MVSIZ,NIPMAX) , RY(MVSIZ,NIPMAX) , RZ(MVSIZ,NIPMAX) ,
     .  SX(MVSIZ,NIPMAX) , SY(MVSIZ,NIPMAX) , SZ(MVSIZ,NIPMAX) ,
     .  TX(MVSIZ,NIPMAX) , TY(MVSIZ,NIPMAX) , TZ(MVSIZ,NIPMAX) ,
     .  STIG(MVSIZ,NPE),STIN(MVSIZ,NPE),UL(MVSIZ,NPE),
     .  XX(MVSIZ,NPE), YY(MVSIZ,NPE), ZZ(MVSIZ,NPE),
     .  VX(MVSIZ,NPE), VY(MVSIZ,NPE), VZ(MVSIZ,NPE),
     .  VDXX(MVSIZ,NPE), VDYY(MVSIZ,NPE), VDZZ(MVSIZ,NPE),
     .  DNIDR(MVSIZ,NPE),DNIDS(MVSIZ,NPE),DNIDT(MVSIZ,NPE),
     .  PX(MVSIZ,NPE,NIPMAX),PY(MVSIZ,NPE,NIPMAX),PZ(MVSIZ,NPE,NIPMAX),
     .  TEMP(*), FTHE(*), FTHESKY(*), GRESAV(*),VOLN(MVSIZ),CONDN(*),
     .  CONDNSKY(*)
      TYPE (TTABLE) TABLE(*)
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (NLOCAL_STR_)  , TARGET :: NLOC_DMG 
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
      TYPE (MAT_ELEM_) ,INTENT(INOUT) :: MAT_ELEM
      TYPE(DT_), INTENT(INOUT) :: DT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,LCO, NF1, IFLAG, IL, IR, IS, IT, NF2,L_PLA,
     .        IP,NLAY,NPTT,NPTS,NPTR,ICO,IBID,IBIDON(1),MX,II(6),PID

      INTEGER MXT(MVSIZ),NGL(MVSIZ),NGEO(MVSIZ),IPERM1(NPE),IPERM2(NPE)
      my_real
     . VD2(MVSIZ) , DVOL(MVSIZ),DELTAX(MVSIZ),
     . VIS(MVSIZ) , QVIS(MVSIZ), CXX(MVSIZ) ,
     . S1(MVSIZ)  , S2(MVSIZ)  , S3(MVSIZ)  ,
     . S4(MVSIZ)  , S5(MVSIZ)  , S6(MVSIZ)  ,
     . DXX(MVSIZ) , DYY(MVSIZ) , DZZ(MVSIZ) ,
     . D4(MVSIZ)  , D5(MVSIZ)  , D6(MVSIZ)  , 
     . VDX(MVSIZ),VDY(MVSIZ),VDZ(MVSIZ),SSP_EQ(MVSIZ),
     . AIRE(MVSIZ),CONDE(MVSIZ),CONDEG(MVSIZ,NPE),DIVDE(MVSIZ)

      ! Variables utilisees en argument par les materiaux.
      my_real
     .   STI(MVSIZ), WXX(MVSIZ) , WYY(MVSIZ) , WZZ(MVSIZ)
      ! Variables utilisees en argument par les materiaux si SPH uniquement.
      my_real
     .   MUVOID(MVSIZ)

      !  Variables void MMAIN 
      my_real
     . SIGY(MVSIZ),ET(MVSIZ),GAMA(MVSIZ,6),
     . R1_FREE(MVSIZ),R3_FREE(MVSIZ),R4_FREE(MVSIZ),
     . TEMPEL(MVSIZ),DIE(MVSIZ),THEM(MVSIZ,NPE),AREA(MVSIZ),LLSH(MVSIZ)
     
      ! Variables utilisees dans les routines solides uniquement (en arguments).
      INTEGER NC(MVSIZ,NPE),NN_DEL,IPRES
      my_real
     .  OFF(MVSIZ) , RHOO(MVSIZ),NI(NPE,NIPMAX),
     .  SIGSM(MVSIZ),VOLSM(MVSIZ),
     .  DXY(MVSIZ),DYX(MVSIZ),
     .  DYZ(MVSIZ),DZY(MVSIZ),VOLG(MVSIZ),
     .  DZX(MVSIZ),DXZ(MVSIZ),  WI,BID(MVSIZ),AA,BB, MBID(1),AMU(MVSIZ)
      my_real VARNL(NEL)
      DOUBLE PRECISION 
     .   VOLDP(MVSIZ,NIPMAX)
C-----------------------------------------------
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
C-----------------------------------------------
      my_real
     .  W_GAUSS(9,9),A_GAUSS(9,9),W_LOBATTO(9,9),A_LOBATTO(9,9),
     .  W_NEWTON(9,9),A_NEWTON(9,9)
C-----------------------------------------------
      DATA W_GAUSS / 
     1 2.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     2 1.               ,1.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     3 0.555555555555556,0.888888888888889,0.555555555555556,
     3 0.               ,0.               ,0.               ,
     3 0.               ,0.               ,0.               ,
     4 0.347854845137454,0.652145154862546,0.652145154862546,
     4 0.347854845137454,0.               ,0.               ,
     4 0.               ,0.               ,0.               ,
     5 0.236926885056189,0.478628670499366,0.568888888888889,
     5 0.478628670499366,0.236926885056189,0.               ,
     5 0.               ,0.               ,0.               ,
     6 0.171324492379170,0.360761573048139,0.467913934572691,
     6 0.467913934572691,0.360761573048139,0.171324492379170,
     6 0.               ,0.               ,0.               ,
     7 0.129484966168870,0.279705391489277,0.381830050505119,
     7 0.417959183673469,0.381830050505119,0.279705391489277,
     7 0.129484966168870,0.               ,0.               ,
     8 0.101228536290376,0.222381034453374,0.313706645877887,
     8 0.362683783378362,0.362683783378362,0.313706645877887,
     8 0.222381034453374,0.101228536290376,0.               ,
     9 0.081274388361574,0.180648160694857,0.260610696402935,
     9 0.312347077040003,0.330239355001260,0.312347077040003,
     9 0.260610696402935,0.180648160694857,0.081274388361574/
      DATA A_GAUSS / 
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     2 -.577350269189626,0.577350269189626,0.               ,
     2 0.               ,0.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     3 -.774596669241483,0.               ,0.774596669241483,
     3 0.               ,0.               ,0.               ,
     3 0.               ,0.               ,0.               ,
     4 -.861136311594053,-.339981043584856,0.339981043584856,
     4 0.861136311594053,0.               ,0.               ,
     4 0.               ,0.               ,0.               ,
     5 -.906179845938664,-.538469310105683,0.               ,
     5 0.538469310105683,0.906179845938664,0.               ,
     5 0.               ,0.               ,0.               ,
     6 -.932469514203152,-.661209386466265,-.238619186083197,
     6 0.238619186083197,0.661209386466265,0.932469514203152,
     6 0.               ,0.               ,0.               ,
     7 -.949107912342759,-.741531185599394,-.405845151377397,
     7 0.               ,0.405845151377397,0.741531185599394,
     7 0.949107912342759,0.               ,0.               ,
     8 -.960289856497536,-.796666477413627,-.525532409916329,
     8 -.183434642495650,0.183434642495650,0.525532409916329,
     8 0.796666477413627,0.960289856497536,0.               ,
     9 -.968160239507626,-.836031107326636,-.613371432700590,
     9 -.324253423403809,0.               ,0.324253423403809,
     9 0.613371432700590,0.836031107326636,0.968160239507626/
C-----
      DATA W_LOBATTO / 
     1 2.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     2 1.               ,1.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     3 0.333333333333333,1.333333333333333,0.333333333333333,
     3 0.               ,0.               ,0.               ,
     3 0.               ,0.               ,0.               ,
     4 0.166666666666667,0.833333333333333,0.833333333333333,
     4 0.166666666666667,0.               ,0.               ,
     4 0.               ,0.               ,0.               ,
     5 0.1              ,0.544444444444444,0.711111111111111,
     5 0.544444444444444,0.1              ,0.               ,
     5 0.               ,0.               ,0.               ,
     6 0.066666666666667,0.37847496       ,0.55485838       ,
     6 0.55485838       ,0.37847496       ,0.066666666666667,
     6 0.               ,0.               ,0.               ,
     7 0.04761904       ,0.27682604       ,0.43174538       ,
     7 0.48761904       ,0.43174538       ,0.27682604       ,
     7 0.04761904       ,0.               ,0.               ,
     8 0.03571428       ,0.21070422       ,0.34112270       ,
     8 0.41245880       ,0.41245880       ,0.34112270       ,
     8 0.21070422       ,0.03571428       ,0.               ,
     9 0.027777777777778,0.1654953616     ,0.2745387126     ,
     9 0.3464285110     ,0.3715192744     ,0.3464285110     ,
     9 0.2745387126     ,0.1654953616     ,0.027777777777778/
      DATA A_LOBATTO / 
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     2 -1.              ,1.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     3 -1.              ,0.               ,1.               ,
     3 0.               ,0.               ,0.               ,
     3 0.               ,0.               ,0.               ,
     4 -1.              ,-.44721360       ,0.44721360       ,
     4  1.              ,0.               ,0.               ,
     4 0.               ,0.               ,0.               ,
     5 -1.              ,-.65465367       ,0.               ,
     5 0.65465367       , 1.              ,0.               ,
     5 0.               ,0.               ,0.               ,
     6 -1.              ,-.76505532       ,-.28523152       ,
     6 0.28523152       ,0.76505532       , 1.              ,
     6 0.               ,0.               ,0.               ,
     7 -1.              ,-.83022390       ,-.46884879       ,
     7 0.               ,0.46884879       ,0.83022390       ,
     7  1.              ,0.               ,0.               ,
     8 -1.              ,-.87174015       ,-.59170018       ,
     8 -.20929922       ,0.20929922       ,0.59170018       ,
     8 0.87174015       , 1.              ,0.               ,
     9 -1.              ,-.8997579954     ,-.6771862795     ,
     9 -.3631174638     ,0.               ,0.3631174638     ,
     9 0.6771862795     ,0.8997579954     , 1.              /
C-----
C-----
      DATA W_NEWTON / 
     1 2.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     2 1.               ,1.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     3 0.5              ,1.               ,0.5              ,
     3 0.               ,0.               ,0.               ,
     3 0.               ,0.               ,0.               ,
     4 0.166666666666667,0.833333333333333,0.833333333333333,
     4 0.166666666666667,0.               ,0.               ,
     4 0.               ,0.               ,0.               ,
     5 0.25             ,0.5              ,0.5              ,
     5 0.5              ,0.25             ,0.               ,
     5 0.               ,0.               ,0.               ,
     6 0.066666666666667,0.37847496       ,0.55485838       ,
     6 0.55485838       ,0.37847496       ,0.066666666666667,
     6 0.               ,0.               ,0.               ,
     7 0.04761904       ,0.27682604       ,0.43174538       ,
     7 0.48761904       ,0.43174538       ,0.27682604       ,
     7 0.04761904       ,0.               ,0.               ,
     8 0.03571428       ,0.21070422       ,0.34112270       ,
     8 0.41245880       ,0.41245880       ,0.34112270       ,
     8 0.21070422       ,0.03571428       ,0.               ,
     9 0.027777777777778,0.1654953616     ,0.2745387126     ,
     9 0.3464285110     ,0.3715192744     ,0.3464285110     ,
     9 0.2745387126     ,0.1654953616     ,0.027777777777778/
      DATA A_NEWTON / 
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     2 -1.              ,1.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     3 -1.              ,0.               ,1.               ,
     3 0.               ,0.               ,0.               ,
     3 0.               ,0.               ,0.               ,
     4 -1.              ,-.44721360       ,0.44721360       ,
     4  1.              ,0.               ,0.               ,
     4 0.               ,0.               ,0.               ,
     5 -1.              ,-.5              ,0.               ,
     5 0.5              , 1.              ,0.               ,
     5 0.               ,0.               ,0.               ,
     6 -1.              ,-.76505532       ,-.28523152       ,
     6 0.28523152       ,0.76505532       , 1.              ,
     6 0.               ,0.               ,0.               ,
     7 -1.              ,-.83022390       ,-.46884879       ,
     7 0.               ,0.46884879       ,0.83022390       ,
     7  1.              ,0.               ,0.               ,
     8 -1.              ,-.87174015       ,-.59170018       ,
     8 -.20929922       ,0.20929922       ,0.59170018       ,
     8 0.87174015       , 1.              ,0.               ,
     9 -1.              ,-.8997579954     ,-.6771862795     ,
     9 -.3631174638     ,0.               ,0.3631174638     ,
     9 0.6771862795     ,0.8997579954     , 1.              /
C-----
      DATA IPERM1/0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8/
      DATA IPERM2/0,0,0,0,0,0,0,0,2,3,4,1,6,7,8,5/
C-----------------------------------------------
C   S o u r c e  L i n e s
C=======================================================================
      GBUF    =>ELBUF_TAB(NG)%GBUF
      NPTR     = ELBUF_TAB(NG)%NPTR
      NPTS     = ELBUF_TAB(NG)%NPTS
      NPTT     = ELBUF_TAB(NG)%NPTT
      NLAY     = ELBUF_TAB(NG)%NLAY
      IS       = 1
      ISORTH   = 0
      TEMPEL(1:MVSIZ) = ZERO
      NF1      = NFT+1
      NF2      = NF1-(NUMELS8+NUMELS10+NUMELS20)
      IF (NPTR*NPTS*NPTT*NLAY > NIPMAX) STOP 933
      IBID     = 0
      IBIDON(1)= 0
!
      DO I=1,6
        II(I) = NEL*(I-1)
      ENDDO
!
      CALL S20COOR3(
     1   X,           IXS(1,NF1),  IXS16(1,NF2),V,
     2   W,           IPERM1,      IPERM2,      NPE,
     3   XX,          YY,          ZZ,          VX,
     4   VY,          VZ,          VDXX,        VDYY,
     5   VDZZ,        VDX,         VDY,         VDZ,
     6   VD2,         VIS,         GBUF%OFF,    OFF,
     7   GBUF%SMSTR,  NC,          NGL,         MXT,
     8   NGEO,        FX,          FY,          FZ,
     9   STIG,        GBUF%SIG,    GBUF%EINT,   GBUF%RHO,
     A   GBUF%QVIS,   GBUF%PLA,    GBUF%EPSD,   GBUF%G_PLA,
     B   GBUF%G_EPSD, NEL,         CONDEG,      JALE,
     C   ISMSTR,      JEUL,        JLAG)

      NN_DEL = 0
      PID = NGEO(1)
      IF (GEO(190,PID)+GEO(191,PID)+GEO(192,PID)+GEO(192,PID)>ZERO)
     .        NN_DEL=8
      IF (NN_DEL ==0 .AND. DT%IDEL_BRICK>0) NN_DEL=16
      IPRES = MAT_ELEM%MAT_PARAM(MXT(1))%IPRES
C
      DO N=1,NPE
        DO I=1,NEL
          UL(I,N) = ZERO
        ENDDO
      ENDDO
      IF (JTHE < 0) THEM(1:NEL,1:NPE) = ZERO
      DO I=1,NEL
        VOLG(I)  = ZERO
      ENDDO
C-----------------------------
C     POINTS D' INTEGRATION - SIGS
C-----------------------------
      DO IT=1,NPTT
        DO IR=1,NPTR
          DO I=1,NEL
            SIGSM(I) = ZERO
            VOLSM(I) = ZERO
          ENDDO
C-----------
          DO IL=1,NLAY
            LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)
            IP = IR + ( (IL-1) + (IT-1)*NLAY )*NPTR
            IF (IINT == 1) THEN
              !---GAUSS
              WI = W_GAUSS(IR,NPTR)*W_GAUSS(IL,NLAY)*W_GAUSS(IT,NPTT)
              CALL S16RST(
     1         A_GAUSS(IR,NPTR),A_GAUSS(IL,NLAY),A_GAUSS(IT,NPTT),NI(1,IP),
     2         DNIDR         ,DNIDS         ,DNIDT         )

             CALL S16DERI3(
     1   NGL,             OFF,             A_GAUSS(IR,NPTR),A_GAUSS(IL,NLAY),
     2   A_GAUSS(IT,NPTT),WI,              DNIDR,           DNIDS,
     3   DNIDT,           RX(1,IP),        RY(1,IP),        RZ(1,IP),
     4   SX(1,IP),        SY(1,IP),        SZ(1,IP),        TX(1,IP),
     5   TY(1,IP),        TZ(1,IP),        XX,              YY,
     6   ZZ,              PX(1,1,IP),      PY(1,1,IP),      PZ(1,1,IP),
     7   VOLNP(1,IP),     DELTAX,          STIN,            NI(1,IP),
     8   VOLG,            UL,              IR,              IL,
     9   IT,              VOLDP(1,IP),     NEL)
            ELSEIF (IINT == 2) THEN
              !---LOBATTO
              WI = W_GAUSS(IR,NPTR)*W_LOBATTO(IL,NLAY)*W_GAUSS(IT,NPTT)
              CALL S16RST(
     1       A_GAUSS(IR,NPTR),A_LOBATTO(IL,NLAY),A_GAUSS(IT,NPTT),NI(1,IP),
     2         DNIDR         ,DNIDS             ,DNIDT         )

             CALL S16DERI3(
     1   NGL,               OFF,               A_GAUSS(IR,NPTR),  A_LOBATTO(IL,NLAY),
     2   A_GAUSS(IT,NPTT),  WI,                DNIDR,             DNIDS,
     3   DNIDT,             RX(1,IP),          RY(1,IP),          RZ(1,IP),
     4   SX(1,IP),          SY(1,IP),          SZ(1,IP),          TX(1,IP),
     5   TY(1,IP),          TZ(1,IP),          XX,                YY,
     6   ZZ,                PX(1,1,IP),        PY(1,1,IP),        PZ(1,1,IP),
     7   VOLNP(1,IP),       DELTAX,            STIN,              NI(1,IP),
     8   VOLG,              UL,                IR,                IL,
     9   IT,                VOLDP(1,IP),       NEL)
            ENDIF
            !--------------------------
            ! SOUS INTEGRATION DE SIGS
            !-------------------------
            DO I=1,NEL
              AA         = ONE / SQRT(SX(I,IP)*SX(I,IP) + SY(I,IP)*SY(I,IP) +SZ(I,IP)*SZ(I,IP))
              SX(I,IP)   = SX(I,IP) * AA
              SY(I,IP)   = SY(I,IP) * AA
              SZ(I,IP)   = SZ(I,IP) * AA
              RX(I,IP)   = SY(I,IP) * TZ(I,IP) - SZ(I,IP) * TY(I,IP)
              RY(I,IP)   = SZ(I,IP) * TX(I,IP) - SX(I,IP) * TZ(I,IP)
              RZ(I,IP)   = SX(I,IP) * TY(I,IP) - SY(I,IP) * TX(I,IP)
              AA         = ONE / SQRT(RX(I,IP)*RX(I,IP)+RY(I,IP)*RY(I,IP)+RZ(I,IP)*RZ(I,IP))
              RX(I,IP)   = RX(I,IP) * AA
              RY(I,IP)   = RY(I,IP) * AA
              RZ(I,IP)   = RZ(I,IP) * AA
              TX(I,IP)   = RY(I,IP) * SZ(I,IP) - RZ(I,IP) * SY(I,IP)
              TY(I,IP)   = RZ(I,IP) * SX(I,IP) - RX(I,IP) * SZ(I,IP)
              TZ(I,IP)   = RX(I,IP) * SY(I,IP) - RY(I,IP) * SX(I,IP)
              SIGS(I,IP) = SX(I,IP)*SX(I,IP)*LBUF%SIG(II(1)+I)
     .                    +SY(I,IP)*SY(I,IP)*LBUF%SIG(II(2)+I)
     .                    +SZ(I,IP)*SZ(I,IP)*LBUF%SIG(II(3)+I)
     .                    +SX(I,IP)*SY(I,IP)*LBUF%SIG(II(4)+I)*TWO
     .                    +SY(I,IP)*SZ(I,IP)*LBUF%SIG(II(5)+I)*TWO
     .                    +SZ(I,IP)*SX(I,IP)*LBUF%SIG(II(6)+I)*TWO
             SIGSM(I)    = SIGSM(I) + SIGS(I,IP)*LBUF%VOL(I)
             VOLSM(I)    = VOLSM(I) + LBUF%VOL(I)
            ENDDO!next I
          ENDDO!next ILAY 
          DO I=1,NEL
            SIGSM(I) = SIGSM(I) / VOLSM(I)
          ENDDO
          DO IL=1,NLAY
            IP = IR + ( (IL-1) + (IT-1)*NLAY ) * NPTR
            DO I = 1,NEL
              SIGS(I,IP) = SIGS(I,IP)-SIGSM(I)
            ENDDO
          ENDDO!IL=1,NLAY
       ENDDO!IR=1,NPTR
      ENDDO!IT=1,NPTT
C-------------------------
      DO I=1,NEL
        AA = MAX(UL(I,1),UL(I,2),UL(I,3),UL(I,4),
     .           UL(I,5),UL(I,6),UL(I,7),UL(I,8))
        BB = MAX(UL(I,9) ,UL(I,10),UL(I,11),UL(I,12),UL(I,13),UL(I,14),
     .           UL(I,15),UL(I,16))
        AA = AA*THIRTY2
        BB = BB*THIRTY2*THIRD
        DELTAX(I) = SQRT(TWO*VOLG(I)/MAX(AA,BB))
      ENDDO
      IF (ICP == 1 .OR. ICP == 2 .AND. IPRES==1 ) THEN
        CALL S16SIGP3(1,NEL,NEL,NPTR,NLAY,
     .                NPTT,ICP,MTN,NPE,NIPMAX, 
     .                PX, PY, PZ, VX, VY, VZ, 
     .                W_GAUSS,GBUF%PLA,PM,MXT,GBUF%SIG,DT1,
     .                ELBUF_TAB(NG))
      ENDIF
      !-----------------------------
      !     POINTS D' INTEGRATION - MMAIN
      !-----------------------------
      DO IT=1,NPTT
       DO IR=1,NPTR
        DO IL=1,NLAY
C
         LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)
         IP = IR + ( (IL-1) + (IT-1)*NLAY )*NPTR
c
         CALL S20DEFO3(
     1   NPE,        PX(1,1,IP), PY(1,1,IP), PZ(1,1,IP),
     2   VX,         VY,         VZ,         DXX,
     3   DXY,        DXZ,        DYX,        DYY,
     4   DYZ,        DZX,        DZY,        DZZ,
     5   D4,         D5,         D6,         WXX,
     6   WYY,        WZZ,        LBUF%RHO,   RHOO,
     7   VOLNP(1,IP),VOLN,       NEL)
C           
         IF(DT1/=ZERO .AND. IPRES==1 )CALL S16SIGS3(
     1   DXX,        DYY,        DZZ,        D4,
     2   D5,         D6,         RX(1,IP),   RY(1,IP),
     3   RZ(1,IP),   SX(1,IP),   SY(1,IP),   SZ(1,IP),
     4   TX(1,IP),   TY(1,IP),   TZ(1,IP),   GBUF%EINT,
     5   LBUF%VOL,   SIGS(1,IP), MXT,        PM,
     6   DT1,        LBUF%VOL0DP,NEL)
C
         DIVDE(1:NEL) = DT1*(DXX(1:NEL)+ DYY(1:NEL)+ DZZ(1:NEL))   
         CALL SRHO3(
     1   PM,         LBUF%VOL,   LBUF%RHO,   LBUF%EINT,
     2   DIVDE,      FLUX(1,NF1),FLU1(NF1),  VOLN,
     3   DVOL,       NGL,        MXT,        OFF,
     4   0,          GBUF%TAG22, VOLDP(1,IP),LBUF%VOL0DP,
     5   AMU,        GBUF%OFF,   NEL,        MTN,
     6   JALE,       ISMSTR,     JEUL,       JLAG)
     
         CALL SROTA3(
     1   LBUF%SIG,S1,      S2,      S3,
     2   S4,      S5,      S6,      WXX,
     3   WYY,     WZZ,     NEL,     MTN,
     4   ISMSTR)
      !-----------------------------
      !     SMALL STRAIN
      !-----------------------------
         CALL SMALLA3(
     1   GBUF%SMSTR,GBUF%OFF,  OFF,       WXX,
     2   WYY,       WZZ,       NEL,       ISMSTR,
     3   JLAG)
C
C    for heat transfert
C
          IF(JTHE < 0 ) THEN
             CALL S20TEMPCG(1,NEL,NPE, NC,NI(1,IP), TEMP,TEMPEL)
          ENDIF
      !------------------------------------------------------
      !     CALCUL DES CONTRAINTES SUIVANT LOIS CONSTITUTIVES
      !------------------------------------------------------
         CALL MMAIN(
     1   ELBUF_TAB,   NG,          PM,          GEO,
     2   FV,          ALE_CONNECT, IXS,         IPARG,
     3   V,           TF,          NPF,         BUFMAT,
     4   STI,         X,           DT2T,        NELTST,
     5   ITYPTST,     OFFSET,      NEL,         W,
     6   OFF,         NGEO,        MXT,         NGL,
     7   VOLN,        VD2,         DVOL,        DELTAX,
     8   VIS,         QVIS,        CXX,         S1,
     9   S2,          S3,          S4,          S5,
     A   S6,          DXX,         DYY,         DZZ,
     B   D4,          D5,          D6,          WXX,
     C   WYY,         WZZ,         RX(1,IP),    RY(1,IP),
     D   RZ(1,IP),    SX(1,IP),    SY(1,IP),    SZ(1,IP),
     E   VDX,         VDY,         VDZ,         MUVOID,
     F   SSP_EQ,      AIRE,        SIGY,        ET,
     G   R1_FREE,     LBUF%PLA,    R3_FREE,     AMU,
     H   DXX,         DXY,         DXZ,         DYX,
     I   DYY,         DYZ,         DZX,         DZY,
     J   DZZ,         IPM,         GAMA,        BID,
     K   BID,         BID,         BID,         BID,
     L   BID,         BID,         ISTRAIN,     TEMPEL,
     M   DIE,         IEXPAN,      IL,          MBID,
     N   MBID,        IR,          IS,          IT,
     O   TABLE,       BID,         BID,         BID,
     P   BID,         IPARG(1,NG), IGEO,        CONDE,
     Q   ITASK,       NLOC_DMG,    VARNL,       MAT_ELEM,
     R   H3D_STRAIN,  JPLASOL,     JSPH,        OPT_MTN=MTN,
     S   OPT_JCVT=JCVT,OPT_ISORTH=ISORTH,       OPT_ISORTHG=ISORTHG)

c
      IF (ISTRAIN == 1)THEN 
        CALL SSTRA3(
     1   DXX,      DYY,      DZZ,      D4,
     2   D5,       D6,       LBUF%STRA,WXX,
     3   WYY,      WZZ,      OFF,      NEL,
     4   JCVT)
      ENDIF
      !-----------------------------
      !       SMALL STRAIN 
      !-----------------------------
         CALL SMALLB3(
     1   GBUF%OFF,OFF,     NEL,     ISMSTR)
      !-------------------------
      !       ASSEMBLE
      !-------------------------
      !----------------------------
      !       INTERNAL FORCES
      !----------------------------
         IF (IINT == 1) THEN
          WI = W_GAUSS(IR,NPTR)*W_GAUSS(IL,NLAY)*W_GAUSS(IT,NPTT)
         ELSEIF (IINT == 2) THEN
          WI = W_GAUSS(IR,NPTR)*W_LOBATTO(IL,NLAY)*W_GAUSS(IT,NPTT)
         ENDIF
C
         L_PLA  = ELBUF_TAB(NG)%BUFLY(IL)%L_PLA
         CALL S20FINT3(
     1   NPE,       LBUF%SIG,  PX(1,1,IP),PY(1,1,IP),
     2   PZ(1,1,IP),SSP_EQ,    FX,        FY,
     3   FZ,        VOLN,      QVIS,      STIG,
     4   STIN,      LBUF%EINT, LBUF%RHO,  LBUF%QVIS,
     5   LBUF%PLA,  LBUF%EPSD, GBUF%EPSD, GBUF%SIG,
     6   GBUF%EINT, GBUF%RHO,  GBUF%QVIS, GBUF%PLA,
     7   WI,        VOLG,      LBUF%VOL,  GBUF%VOL,
     8   L_PLA,     NEL,       CONDE,     DELTAX,
     9   CONDEG,    ISRAT)
      !-------------------------
      !    finite element heat transfert  
      !--------------------------

         IF (JTHE < 0) THEN
           CALL S20THERM(
     1   NPE,       PM,        MXT,       NC,
     2   VOLN,      PX(1,1,IP),PY(1,1,IP),PZ(1,1,IP),
     3   NI(1,IP),  DT1,       TEMP,      TEMPEL,
     4   DIE,       THEM,      GBUF%OFF,  LBUF%OFF,
     5   NEL)
          ENDIF
        ENDDO   ! IL=1,NLAY
       ENDDO    ! IR=1,NPTR
      ENDDO     ! IT=1,NPTT
C      
      IF ( NN_DEL> 0) THEN
        CALL SDLENSH2(VOLG,LLSH,AREA ,
     .          XX(1,1), XX(1,2), XX(1,3), XX(1,4),
     .          XX(1,5), XX(1,6), XX(1,7), XX(1,8),
     .          YY(1,1), YY(1,2), YY(1,3), YY(1,4),
     .          YY(1,5), YY(1,6), YY(1,7), YY(1,8),
     .          ZZ(1,1), ZZ(1,2), ZZ(1,3), ZZ(1,4),
     .          ZZ(1,5), ZZ(1,6), ZZ(1,7), ZZ(1,8), NEL)
        CALL TSHGEODEL3(NGL,GBUF%OFF,VOLG,AREA,GBUF%VOL,
     .                  LLSH,GEO(1,PID),NN_DEL,DT ,NEL )
      ENDIF
      !--------------------------
      !     BILANS PAR MATERIAU
      !--------------------------
      IFLAG=MOD(NCYCLE,NCPRI)
      IF(IOUTPRT>0)THEN
           CALL S16BILAN(PARTSAV,GBUF%EINT,GBUF%RHO,VOLG  ,GBUF%VOL,
     .             VX     ,VY       ,VZ      ,IPARTS     ,GRESAV    ,
     .             GRTH   ,IGRTH    ,IEXPAN  ,GBUF%EINTTH, GBUF%FILL,
     .             XX     ,YY       ,ZZ      ,ITASK      ,IPARG(1,NG),
     .             GBUF%OFF)
      ENDIF
C
c-----------------------------
      IF(NFILSOL/=0) CALL SXFILLOPT(
     1   NPE,      GBUF%FILL,STIG,     FX,
     2   FY,       FZ,       NEL)
c-----------------------------
      IF (IPARIT == 0)THEN
        CALL S20CUMU3(
     1   GBUF%OFF,A,       NC,      STIFN,
     2   STIG,    FX,      FY,      FZ,
     3   IPERM1,  IPERM2,  NPE,     THEM,
     4   FTHE,    CONDN,   CONDEG,  NEL,
     5   JTHE)
      ELSE
        CALL S20CUMU3P(
     1   GBUF%OFF,     STIG,         FSKY,         FSKY,
     2   IADS(1,NF1),  FX,           FY,           FZ,
     3   IADS16(1,NF2),NC,           IPERM1,       IPERM2,
     4   NPE,          THEM,         FTHESKY,      CONDNSKY,
     5   CONDEG,       NEL,          NFT,          JTHE)
      ENDIF
C-----------
      RETURN
      END


C                                                                     12
