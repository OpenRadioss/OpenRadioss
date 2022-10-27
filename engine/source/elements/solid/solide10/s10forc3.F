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
Chd|  S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|-- called by -----------
Chd|        FORINT                        source/elements/forint.F      
Chd|-- calls ---------------
Chd|        BOLTST                        source/elements/solid/solide/boltst.F
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        S10BILAN                      source/elements/solid/solide10/s10bilan.F
Chd|        S10COOR3                      source/elements/solid/solide10/s10coor3.F
Chd|        S10CUMU3                      source/elements/solid/solide10/s10cumu3.F
Chd|        S10CUMU3P                     source/elements/solid/solide10/s10cumu3.F
Chd|        S10DEFO3                      source/elements/solid/solide10/s10defo3.F
Chd|        S10DEFOT3                     source/elements/solid/solide10/s10defo3.F
Chd|        S10DERI3                      source/elements/solid/solide10/s10deri3.F
Chd|        S10DERIT3                     source/elements/solid/solide10/s10deri3.F
Chd|        S10FINT3                      source/elements/solid/solide10/s10fint3.F
Chd|        S10LEN3                       source/elements/solid/solide10/s10len3.F
Chd|        S10MALLA3                     source/elements/solid/solide10/s10mall3.F
Chd|        S10MALLB3                     source/elements/solid/solide10/s10mall3.F
Chd|        S10MALLGEO3                   source/elements/solid/solide10/s10mall3.F
Chd|        S10NX3                        source/elements/solid/solide10/s10nx3.F
Chd|        S10NXT4                       source/elements/solid/solide10/s10nxt4.F
Chd|        S10PIJTO3                     source/elements/solid/solide10/s10pijto3.F
Chd|        S10RCOOR12                    source/elements/solid/solide10/s10coor3.F
Chd|        S10SAV12                      source/elements/solid/solide10/s10coor3.F
Chd|        S10SAV3                       source/elements/solid/solide10/s10coor3.F
Chd|        S10SIGP3                      source/elements/solid/solide4_sfem/s10sigp3.F
Chd|        S10THERM                      source/elements/solid/solide10/s10therm.F
Chd|        S10UPD11T12                   source/elements/solid/solide10/s10upd11t12.F
Chd|        S10_ICP                       source/elements/solid/solide4_sfem/s10_icp.F
Chd|        S4THERM_ITET1                 source/elements/solid/solide4/s4therm-itet1.F
Chd|        SGCOOR10                      source/elements/solid/solide10/s10coor3.F
Chd|        SMALLB3                       source/elements/solid/solide/small3.F
Chd|        SORDEF12                      source/elements/solid/solidez/szforc3.F
Chd|        SORDEFT12                     source/elements/solid/solidez/szforc3.F
Chd|        SORTHDIR3                     source/elements/solid/solide/sorthdir3.F
Chd|        SREPLOC3                      source/elements/solid/solide/sreploc3.F
Chd|        SRHO3                         source/elements/solid/solide/srho3.F
Chd|        SROTA3                        source/elements/solid/solide/srota3.F
Chd|        SROTO12_SIG                   source/elements/solid/solidez/szforc3.F
Chd|        SSTRA3                        source/elements/solid/solide/sstra3.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        SXFILLOPT                     source/elements/solid/solide10/sxfillopt.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MMAIN_MOD                     source/materials/mat_share/mmain.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE S10FORC3(ELBUF_TAB,NG     ,
     1                   PM       ,GEO     ,IXS     ,X       ,
     2                   A        ,V       ,MS      ,W       ,FLUX    ,
     3                   FLU1     ,VEUL    ,FV      ,ALE_CONNECT   ,IPARG   ,
     4                   TF       ,NPF     ,BUFMAT  ,PARTSAV ,NLOC_DMG,
     5                   DT2T     ,NELTST  ,ITYPTST ,STIFN   ,FSKY    ,
     6                   IADS     ,OFFSET  ,EANI    ,IPARTS  ,
     7                   IXS10    ,IADS10  ,NEL     ,FX      ,
     8                   FY       ,FZ      ,AR      ,VR      ,DR      ,
     9                   IPM      ,ISTRAIN ,ISOLNOD ,
     A                   ITASK    ,TEMP    ,FTHE    ,FTHESKY ,IEXPAN  ,
     B                   STIFR    ,D       ,GRESAV  ,GRTH    ,IGRTH   ,
     C                   TABLE    ,MSSA    ,DMELS   ,IGEO    ,XDP     ,
     D                   VOLN     ,CONDN   ,CONDNSKY,VARNOD  ,
     E                   ITAGDN  ,SENSOR_TAB,IOUTPRT ,
     F                   MATPARAM_TAB,H3D_STRAIN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MMAIN_MOD
      USE TABLE_MOD
      USE ELBUFDEF_MOD            
      USE NLOCAL_REG_MOD
      USE ALE_CONNECTIVITY_MOD
      USE MATPARAM_DEF_MOD
      USE SENSOR_MOD
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
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr03_c.inc"
#include      "scr07_c.inc"
#include      "vect01_c.inc"
#include      "scr01_c.inc"
#include      "scr06_c.inc"
#include      "parit_c.inc"
#include      "param_c.inc"
#include      "timeri_c.inc"
#include      "warn_c.inc"
#include      "scr18_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   l o c a l   P a r a m e t e r s
C-----------------------------------------------
      INTEGER NPE
      PARAMETER (NPE=10)
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),IPARG(NPARG,NGROUP),NPF(*),IADS(8,*),
     .        IPARTS(*),IXS10(6,*),IADS10(6,*),IPM(*),ITASK,GRTH(*),
     .        IGRTH(*),IGEO(*),IOUTPRT,H3D_STRAIN
      INTEGER NELTST,ITYPTST,OFFSET,NEL,NG,ISTRAIN,ISOLNOD,IEXPAN,ITAGDN(*)      
C     REAL

      DOUBLE PRECISION 
     .   XDP(3,*)
      
      my_real
     .   DT2T
      my_real
     .   PM(NPROPM,*), GEO(*), X(*), A(*), V(3,*), MS(*), W(*),
     .   FLUX(6,*),FLU1(*), VEUL(*), FV(*), TF(*), 
     .   BUFMAT(*),PARTSAV(*),STIFN(*), FSKY(*),EANI(*),
     .   AR(*),VR(*) ,DR(*) ,STIFR(*),D(*), MSSA(*)  ,DMELS(*)
      my_real
     .   FX(MVSIZ,10),FY(MVSIZ,10),FZ(MVSIZ,10),
     .   TEMP(*), FTHE(*), FTHESKY(*),GRESAV(*),VOLN(MVSIZ),CONDN(*),
     .   CONDNSKY(*),VARNOD(SVARNOD)
      TYPE(TTABLE) TABLE(*)
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (NLOCAL_STR_)  , TARGET :: NLOC_DMG 
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
      TYPE(MATPARAM_STRUCT_)  , DIMENSION(NUMMAT) :: MATPARAM_TAB
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IP,LCO,NF1,NF2,IFLAG,IOFFS,IPTR,IPTS,IPTT,ILAY
      INTEGER IBID,IBIDON(1),NNEGA,INDEX(MVSIZ),ITET,iel,ISM12_11
C-----
C Variables utilisees en equivalence par les materiaux
C transmises en argument a mmain.
      INTEGER MXT(MVSIZ),NGL(MVSIZ),NGEO(MVSIZ)
      my_real
     . VD2(MVSIZ) , DVOL(MVSIZ),DELTAX(MVSIZ),
     . VIS(MVSIZ) , QVIS(MVSIZ), CXX(MVSIZ) ,DELTAX2(MVSIZ),
     . S1(MVSIZ)  , S2(MVSIZ)  , S3(MVSIZ)  ,
     . S4(MVSIZ)  , S5(MVSIZ)  , S6(MVSIZ)  ,
     . DXX(MVSIZ) , DYY(MVSIZ) , DZZ(MVSIZ) ,
     . D4(MVSIZ)  , D5(MVSIZ)  , D6(MVSIZ)  , 
     . RX(MVSIZ) , RY(MVSIZ) , RZ(MVSIZ) ,
     . SX(MVSIZ) , SY(MVSIZ) , SZ(MVSIZ) ,
     . VDX(MVSIZ), VDY(MVSIZ), VDZ(MVSIZ),SSP_EQ(MVSIZ),AIRE(MVSIZ),
     . CONDE(MVSIZ),CONDEG(MVSIZ), VOLG(MVSIZ), JACGM(MVSIZ)
C-----
C Variables utilisees en argument par les materiaux.
      my_real
     .   STI(MVSIZ),
     .   WXX(MVSIZ) , WYY(MVSIZ) , WZZ(MVSIZ),
     .   WXXG(MVSIZ) , WYYG(MVSIZ) , WZZG(MVSIZ)
C Variables utilisees en argument par les materiaux si SPH uniquement.
      my_real
     .   MUVOID(MVSIZ)
C-----
C     Variables void MMAIN 
      my_real
     . SIGY(MVSIZ),ET(MVSIZ),GAMA(MVSIZ,6),
     . R1_FREE(MVSIZ),R3_FREE(MVSIZ),R4_FREE(MVSIZ)
C Variables utilisees dans les routines solides uniquement (en arguments).
      INTEGER NC(MVSIZ,10),ICP,MX

      DOUBLE PRECISION
     .  XX(MVSIZ,10), YY(MVSIZ,10), ZZ(MVSIZ,10),
     .  WXX0(MVSIZ) , WYY0(MVSIZ) , WZZ0(MVSIZ),
     .  XX0(MVSIZ,10), YY0(MVSIZ,10), ZZ0(MVSIZ,10),VOLDP(MVSIZ,5)

      my_real
     .  TX(MVSIZ),TY(MVSIZ),TZ(MVSIZ),OFF(MVSIZ),VOLP(MVSIZ,5),
     .  RHOO(MVSIZ),OFFS(MVSIZ),THEM(MVSIZ,10),TEMPEL(MVSIZ),
     .  VX(MVSIZ,10),VY(MVSIZ,10),VZ(MVSIZ,10),
     .  PX(MVSIZ,10,5),PY(MVSIZ,10,5),PZ(MVSIZ,10,5),
     .  NX(MVSIZ,10,5),VDXX(MVSIZ,10),VDYY(MVSIZ,10),VDZZ(MVSIZ,10),
     .  DXY(MVSIZ),DYX(MVSIZ),DYZ(MVSIZ),DZY(MVSIZ),
     .  DZX(MVSIZ),DXZ(MVSIZ),
     .  STIG(MVSIZ),  WIP(5,5), ALPH(5,5), BETA(5,5),BID(MVSIZ),
     .  DIE(MVSIZ), MBID(1),OFFG0(MVSIZ),AMU(MVSIZ),SUM
      my_real NXT4(MVSIZ,4,4)
      my_real, DIMENSION(MVSIZ) ::
     .   E1X, E1Y, E1Z, E2X, E2Y, E2Z, E3X, E3Y, E3Z
C
      my_real
     .    VX0(MVSIZ,10),VY0(MVSIZ,10),VZ0(MVSIZ,10),
     .    MFXX(MVSIZ,5),MFXY(MVSIZ,5),MFYX(MVSIZ,5),
     .    MFYY(MVSIZ,5),MFYZ(MVSIZ,5),MFZY(MVSIZ,5),
     .    MFZZ(MVSIZ,5),MFZX(MVSIZ,5),MFXZ(MVSIZ,5),DIVDE(MVSIZ),
     .    NU(MVSIZ),FAC(MVSIZ),FACP(MVSIZ),E0(MVSIZ),C1
c
      my_real VARNL(NEL)
c     Flag Bolt Preloading
      INTEGER IBOLTP,NBPRELD,II(6)
      my_real, 
     .  DIMENSION(:), POINTER :: BPRELD
C-----
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
C--------------------------------------------------------
      DATA WIP / 1.  ,0.  ,0.  ,0.  ,0.  ,
     2           0.  ,0.  ,0.  ,0.  ,0.  ,
     3           0.  ,0.  ,0.  ,0.  ,0.  ,
     4           0.25,0.25,0.25,0.25,0.  ,
     5           0.45,0.45,0.45,0.45,-0.8/
C-----------------------------------------------
C   S o u r c e  L i n e s
C=======================================================================
      GBUF  => ELBUF_TAB(NG)%GBUF
      IBOLTP = IPARG(72,NG)
      NBPRELD = GBUF%G_BPRELD
      BPRELD =>GBUF%BPRELD(1:NBPRELD*NEL)
      ISM12_11 = ELBUF_TAB(NG)%BUFLY(1)%L_SIGL
C-----
      NF1=NFT+1
      NF2=NF1-NUMELS8
      IBID = 0
      IBIDON  = 0
      IOFFS=0
       IPTS = 1
       IPTT = 1
       ILAY = 1
      IF(ISROT == 1) THEN
        IISROT=1
        NF2=1
      END IF
      ICP = IPARG(10,NG)
      DO I=LFT,LLT
        OFFS(I)=EP20
      END DO
C      
      DO IP=1,3
        DO J=1,5
          ALPH(J,IP)=ZERO
          BETA(J,IP)=ZERO
        END DO
      END DO
c
      ALPH(1,4)=ZEP5854102
      ALPH(2,4)=ZEP5854102
      ALPH(3,4)=ZEP5854102
      ALPH(4,4)=ZEP5854102
      ALPH(5,4)=ZERO
      ALPH(1,5)=HALF
      ALPH(2,5)=HALF
      ALPH(3,5)=HALF
      ALPH(4,5)=HALF
      ALPH(5,5)=FOURTH
      BETA(1,4)=ZEP1381966
      BETA(2,4)=ZEP1381966
      BETA(3,4)=ZEP1381966
      BETA(4,4)=ZEP1381966
      BETA(5,4)=ZERO
      BETA(1,5)=ONE_OVER_6
      BETA(2,5)=ONE_OVER_6
      BETA(3,5)=ONE_OVER_6
      BETA(4,5)=ONE_OVER_6
      BETA(5,5)=FOURTH
C   
      DO I=LFT,LLT
        TEMPEL(I) = ZERO
      END DO
      IF (JTHE < 0) THEM(LFT:LLT,1:10) = ZERO
C      
      IF (ICP==1) THEN
        MX = IXS(1,NF1)
        NU(LFT:LLT)=MIN(HALF,PM(21,MX))
        FACP(LFT:LLT)=ONE
      ELSEIF (ICP==2) THEN
        MX = IXS(1,NF1)
        NU(LFT:LLT)=MIN(HALF,PM(21,MX))
        C1 =PM(32,MX)
        E0(LFT:LLT) =THREE*(ONE-TWO*NU(LFT:LLT))*C1
        CALL S10SIGP3(GBUF%SIG,E0 ,GBUF%PLA,FACP ,GBUF%G_PLA,NEL )
      END IF
C-----------
      CALL S10COOR3(
     1   X        ,IXS(1,NF1),IXS10(1,NF2),V        ,W        ,
     2   XX       ,YY        ,ZZ          ,VX       ,VY       , 
     3   VZ       ,VDXX      ,VDYY        ,VDZZ     ,VDX      , 
     4   VDY      ,VDZ       ,VD2         ,VIS      ,GBUF%OFF ,
     5   OFF      ,GBUF%SMSTR,NC          ,NGL      ,MXT      ,
     6   NGEO     ,FX        ,FY          ,FZ       ,STIG     ,
     7   GBUF%SIG ,GBUF%EINT ,GBUF%RHO    ,GBUF%QVIS,GBUF%PLA ,
     8   GBUF%EPSD,VR        ,DR          ,D        ,WXXG     ,
     9   WYYG     ,WZZG      ,GBUF%G_PLA  ,XDP      ,NEL      ,
     A   CONDEG   )
C     
      CALL S10NX3(NX)
      IF(JTHE < 0 .AND. ISOLNOD == 4) CALL S10NXT4(NXT4,NEL)
C-----------
C GATHER NODAL VARIABLES FOR TOTAL STRAIN CASE.
C-----------
      IF (ISMSTR >= 10.AND.ISMSTR <= 12) THEN
       CALL SGCOOR10(XX,YY,ZZ,X ,XDP ,
     1   XX0, YY0, ZZ0, VX0,VY0, VZ0, 
     2   GBUF%SMSTR,NC,D ,GBUF%OFF,OFFG0,NEL)
C----------------------
       IF (ISMSTR ==  11) THEN
        CALL S10DERIT3(VOLP,DELTAX,DELTAX2,
     .   XX0, YY0, ZZ0, PX,PY,PZ, NX,
     .   RX,  RY,  RZ,  SX,  SY,  SZ , TX, TY, TZ,
     .   WIP(1,NPT),ALPH(1,NPT),BETA(1,NPT),VOLN,VOLG,VOLDP)
        CALL S10LEN3(VOLP,NGL,DELTAX,DELTAX2,
     .   PX ,PY ,PZ ,VOLG,GBUF%VOL ,
     .   RX,  RY,  RZ,  SX,  SY,  SZ , TX, TY, TZ,
     .   NC ,NEL,MXT,PM  ,GBUF%ISMS,
     .   GBUF%DT_PITER)
       ELSE
C!!!!!!calcul local rep for ISMSTR 10 to 11 (offg>un) 
        IF (ISMSTR == 12.AND.ISM12_11==0.AND.IDTMIN(1)==3) THEN
         CALL S10RCOOR12(GBUF%OFF,X ,XDP ,NC ,
     .    E1X   ,E2X   ,E3X   ,E1Y   ,E2Y   ,E3Y   ,E1Z  ,E2Z  ,E3Z   ) 
        END IF !(ISMSTR == 10.AND.ISORTH/=0.AND.IDTMIN(1)==3)
c        CALL S10DERITO3(VOLP,
c     .   XX0, YY0, ZZ0, PX,PY,PZ, NX,
c     .   RX,  RY,  RZ,  SX,  SY,  SZ , TX, TY, TZ,
c     .   WIP(1,NPT),ALPH(1,NPT),BETA(1,NPT),VOLN,ELBUF_TAB(NG))
        IBID = 1
        DO IP=1,NPT
         LBUF => ELBUF_TAB(NG)%BUFLY(IBID)%LBUF(IP,IBID,IBID)
         CALL S10PIJTO3(PX(1,1,IP),PY(1,1,IP),PZ(1,1,IP),LBUF%PIJ,LLT)
        ENDDO
       END IF !(ISMSTR ==  11) THEN
       DO IP=1,NPT
        CALL S10DEFOT3(
     .   PX(1,1,IP), PY(1,1,IP), PZ(1,1,IP), VX0, VY0, VZ0,
     .   MFXX(1,IP), MFXY(1,IP), MFXZ(1,IP), MFYX(1,IP), MFYY(1,IP), 
     .   MFYZ(1,IP), MFZX(1,IP), MFZY(1,IP), MFZZ(1,IP) )
       END DO 
       IF (ISMSTR == 12.AND.ISM12_11==0.AND.IDTMIN(1)==3) THEN
         DO IP=1,NPT
          CALL SORDEFT12(LFT,LLT,MFXX(1,IP), MFXY(1,IP), MFXZ(1,IP),
     .         MFYX(1,IP), MFYY(1,IP), MFYZ(1,IP),
     .         MFZX(1,IP), MFZY(1,IP), MFZZ(1,IP),
     .         E1X, E1Y, E1Z, E2X, E2Y, E2Z, E3X, E3Y, E3Z,GBUF%OFF)
         END DO 
       ENDIF
      ENDIF     ! ISMSTR == 10
C-----------
      IF (ISMSTR /= 11) THEN
      CALL S10DERI3(OFF,VOLP,NGL,DELTAX,DELTAX2,
     .   XX, YY, ZZ, PX,PY,PZ, NX,
     .   RX,  RY,  RZ,  SX,  SY,  SZ , TX, TY, TZ,
     .   WIP(1,NPT),ALPH(1,NPT),BETA(1,NPT),VOLN,VOLG,VOLDP,
     .   NC ,GBUF%SMSTR,GBUF%OFF ,NEL)
C
        CALL S10LEN3(VOLP,NGL,DELTAX,DELTAX2,
     .   PX ,PY ,PZ ,VOLG,GBUF%VOL ,
     .   RX,  RY,  RZ,  SX,  SY,  SZ , TX, TY, TZ,
     .   NC ,NEL,MXT,PM  ,GBUF%ISMS,
     .   GBUF%DT_PITER)

C--- for /CST avoid multi-printout----	
       IF (ISMSTR == 2 .OR.ISMSTR ==12) THEN
        DO IP=1,NPT
         IPTR = IP
         LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IPTR,IPTS,IPTT)
         DO I=LFT,LLT
          IF (GBUF%OFF(I)==TWO) LBUF%OFF(I)=GBUF%OFF(I)
         ENDDO
        ENDDO
       END IF 
      END IF 
C --------------------------
C  --- UPDATE REF. CONFIGURATION 
C --------------------------
      IF (ISMSTR <= 3.OR.(ISMSTR==4.AND.JLAG>0)) THEN
        CALL S10SAV3(GBUF%OFF ,GBUF%SMSTR,NC   ,
     +                   XX   ,YY   ,ZZ  ,NEL)
      END IF !(ISMSTR == 2) THEN
C-----------------------------
      IF (ISORTH == 0) THEN            
        DO I=LFT,LLT                                            
          GAMA(I,1) = ONE                               
          GAMA(I,2) = ZERO                                
          GAMA(I,3) = ZERO             
          GAMA(I,4) = ZERO                                
          GAMA(I,5) = ONE                                
          GAMA(I,6) = ZERO             
        ENDDO                          
      ELSE                             
        CALL SREPLOC3(
     .         RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .         E1X  ,E2X  ,E3X  ,E1Y  ,E2Y  ,E3Y  ,E1Z  ,E2Z  ,E3Z  )
        CALL SORTHDIR3(
     .       RX , RY , RZ , SX , SY , SZ , TX , TY , TZ  ,
     .       E1X, E2X, E3X, E1Y, E2Y, E3Y, E1Z, E2Z, E3Z ,
     .       GBUF%GAMA,GAMA )
      ENDIF
      IF(ICP >0 .AND. ISMSTR/=10) THEN
         DO I=LFT,LLT
          IF(GBUF%OFF(I) == ZERO) CYCLE
          SUM=VARNOD(NC(I,1))+VARNOD(NC(I,2))+VARNOD(NC(I,3))+VARNOD(NC(I,4))
          JACGM(I)=FOURTH*SUM
         ENDDO
      ENDIF
C-----------------------------
C     POINTS D' INTEGRATION 
C-----------------------------
      DO IP=1,NPT
        IPTR = IP
        LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IPTR,IPTS,IPTT)
       IF (IOFFS == 1)THEN
        DO I=LFT,LLT
         IF (OFFS(I)<=TWO) LBUF%OFF(I)=OFFS(I)
        ENDDO
       END IF
C
        CALL S10DEFO3(
     .   PX(1,1,IP), PY(1,1,IP), PZ(1,1,IP), VX, VY, VZ,
     .   DXX, DXY, DXZ, DYX, DYY, DYZ, DZX, DZY, DZZ, D4, D5, D6,
     .       WXX, WYY, WZZ,VOLP(1,IP),VOLN,LBUF%RHO,RHOO)

         IF (ISMSTR == 12.AND.ISM12_11==0.AND.IDTMIN(1)==3) THEN
          CALL SORDEF12(LFT,LLT,DXX, DYY, DZZ,
     .         D4, D5, D6,
     .         E1X, E1Y, E1Z, E2X, E2Y, E2Z, E3X, E3Y, E3Z,OFFG0)
         ENDIF
         IF (ICP>0) THEN
           IF (ISMSTR==10) THEN
             DO I=LFT,LLT
               IF(GBUF%OFF(I) == ZERO) CYCLE
               JACGM(I)=VARNOD(NC(I,IP))
             ENDDO
           END IF
           CALL S10_ICP(GBUF%OFF,JACGM,FACP,NU,
     .        MFXX(1,IP), MFXY(1,IP), MFXZ(1,IP), MFYX(1,IP), MFYY(1,IP), 
     .        MFYZ(1,IP), MFZX(1,IP), MFZY(1,IP), MFZZ(1,IP) ,
     .                  LBUF%VOL,VOLN,LBUF%VOL0DP,VOLDP(1,IP),NEL)
         ENDIF
C   
        DIVDE(1:NEL) = DT1*(DXX(1:NEL)+ DYY(1:NEL)+ DZZ(1:NEL))  
        CALL SRHO3(PM         ,LBUF%VOL  , LBUF%RHO, LBUF%EINT, DIVDE,
     .             FLUX(1,NF1),FLU1(NF1) , VOLN    , DVOL     , NGL, MXT, OFF,
     .             0          ,GBUF%TAG22, VOLDP(1,IP),LBUF%VOL0DP ,AMU ,GBUF%OFF)
c
       IF (ISMSTR == 12.AND.ISM12_11==0.AND.IDTMIN(1)==3) THEN
          CALL SROTO12_SIG(LFT,LLT,LBUF%SIG,NEL,
     .                     E1X, E1Y, E1Z, E2X, E2Y, E2Z, E3X, E3Y, E3Z,OFFG0)
!! temporary replaced by (the same) SROTO12_SIG() in order to do not affect
!! the other multidimensional buffer ARRAYS which are still not modified
!!          CALL SROTO12(LFT,LLT,LBUF%SIG,
!!     .                 E1X, E1Y, E1Z, E2X, E2Y, E2Z, E3X, E3Y, E3Z,OFFG0)
       ENDIF
        CALL SROTA3(LBUF%SIG,S1   ,S2   ,S3   ,S4 ,
     .              S5      ,S6   ,WXX  ,WYY  ,WZZ,
     .              NEL     )
C-----------------------------
C       SMALL STRAIN
C-----------------------------
        CALL S10MALLA3(GBUF%OFF,OFF,WXX,WYY,WZZ,
     .                 WXXG,WYYG,WZZG,WIP(IP,NPT))
           
        IF(JTHE < 0 ) THEN                                        
          DO I=LFT,LLT                                            
            TEMPEL(I)= ZERO                                       
          ENDDO                                                   
          IF(ISOLNOD == 10) THEN		  
          DO J = 1,10                                             
            DO I=LFT,LLT                                          
              TEMPEL(I)= TEMPEL(I) +  NX(I,J,IP)*TEMP(NC(I,J))       
            ENDDO                                                 
          ENDDO                                                   
          ELSEIF(ISOLNOD == 4) THEN
            DO J = 1,4                                             
              DO I=LFT,LLT                                          
                TEMPEL(I)= TEMPEL(I) +  NXT4(I,J,IP)*TEMP(NC(I,J))
              ENDDO 
            ENDDO 
        ENDIF                                                     
        ENDIF                                                     
C------------------------------------------------------
C     CALCUL DES CONTRAINTES SUIVANT LOIS CONSTITUTIVES
C------------------------------------------------------
      IF ((ITASK==0).AND.(IMON_MAT==1)) CALL STARTIME(35,1)
C
      IF(IBOLTP /= 0) CALL BOLTST(IBOLTP,IP,BPRELD,LBUF%SIG,TT,NEL,SENSOR_TAB)
C
        CALL MMAIN(ELBUF_TAB,NG     ,PM     ,GEO        ,FV	    ,ALE_CONNECT  ,
     2             IXS      ,IPARG  ,V      ,TF      ,NPF     ,BUFMAT ,
     3             STI      ,X      ,DT2T   ,NELTST  ,ITYPTST ,OFFSET ,
     4             NEL      ,W      ,OFF    ,NGEO       ,
     6             MXT      ,NGL    ,VOLN   ,VD2     ,DVOL    ,DELTAX ,
     7             VIS      ,QVIS   ,CXX    ,S1      ,S2      ,S3     ,
     8             S4       ,S5     ,S6     ,DXX     ,DYY     ,DZZ    ,
     9             D4       ,D5     ,D6     ,WXX     ,WYY     ,WZZ    ,
     A             RX       ,RY     ,RZ     ,SX      ,SY      ,SZ     ,
     B             VDX      ,VDY    ,VDZ    ,MUVOID  ,SSP_EQ  ,AIRE   ,
     C             SIGY     ,ET     ,R1_FREE,LBUF%PLA   ,R3_FREE    ,AMU    ,
     D             MFXX(1,IP),MFXY(1,IP),MFXZ(1,IP),MFYX(1,IP),MFYY(1,IP)     ,MFYZ(1,IP), 
     E             MFZX(1,IP),MFZY(1,IP),MFZZ(1,IP)  ,IPM     ,GAMA	      ,BID    ,
     F             BID      ,BID    ,BID    ,BID     ,BID     ,BID    ,
     G             ISTRAIN  ,TEMPEL, DIE    ,IEXPAN  ,ILAY    ,MSSA   ,
     H             DMELS    ,IPTR   ,IPTS   ,IPTT    ,TABLE   ,BID    ,
     I             BID      ,BID    ,BID    ,IPARG(1,NG),IGEO  ,CONDE    ,
     J             ITASK    ,NLOC_DMG,VARNL ,MATPARAM_TAB,H3D_STRAIN)
C
      IF ((ITASK==0).AND.(IMON_MAT==1)) CALL STOPTIME(35,1)
C-----------  return to global system    
       IF (ISMSTR == 12.AND.ISM12_11==0.AND.IDTMIN(1)==3) THEN
         CALL SROTO12_SIG(LFT,LLT,LBUF%SIG,NEL,
     .                 E1X,E2X,E3X,E1Y,E2Y,E3Y,E1Z,E2Z,E3Z,OFFG0)
!! temporary replaced by (the same) SROTO12_SIG() in order to do not affect
!! the other multidimensional buffer ARRAYS which are still not modified
!!         CALL SROTO12(LFT,LLT,LBUF%SIG,
!!     .                 E1X,E2X,E3X,E1Y,E2Y,E3Y,E1Z,E2Z,E3Z,OFFG0)
         IF (ISTRAIN == 1) THEN 
          CALL SORDEF12(LFT,LLT,DXX, DXY, DXZ,
     .         D4, D5, D6,
     .         E1X,E2X,E3X,E1Y,E2Y,E3Y,E1Z,E2Z,E3Z,OFFG0)
         ENDIF
       ENDIF
        IF (ISTRAIN == 1) CALL SSTRA3(DXX ,DYY      ,DZZ ,D4  ,D5 ,
     .                                D6  ,LBUF%STRA,WXX ,WYY ,WZZ,
     .                                OFF ,NEL      )
C--------------------------
C     BILANS PAR MATERIAU
C--------------------------
        IFLAG=MOD(NCYCLE,NCPRI)
        IF(IOUTPRT>0)THEN
           CALL S10BILAN(PARTSAV,LBUF%EINT,LBUF%RHO,LBUF%RK,LBUF%VOL,
     .                   VX, VY, VZ,NX(1,1,IP),VOLN,IPARTS,
     .                   GRESAV,GRTH,IGRTH,IEXPAN,LBUF%EINTTH,
     .                   GBUF%FILL,XX,YY,ZZ,ITASK,IPARG(1,NG))
        ENDIF
C-------------------------
C       ASSEMBLE
C-------------------------
C----------------------------
C       INTERNAL FORCES
C----------------------------
        CALL S10FINT3(LBUF%SIG,PX(1,1,IP), PY(1,1,IP), PZ(1,1,IP),
     .   FX,FY,FZ,VOLN,QVIS,STI,STIG,
     .   LBUF%EINT,LBUF%RHO,LBUF%QVIS,LBUF%PLA,LBUF%EPSD,GBUF%EPSD,
     .   GBUF%SIG,GBUF%EINT,GBUF%RHO,GBUF%QVIS,GBUF%PLA,
     .   WIP(IP,NPT),GBUF%G_PLA,NEL,CONDE,CONDEG)
C
        DO I=LFT,LLT
         IF (LBUF%OFF(I) > ONE .AND. GBUF%OFF(I) == ONE) THEN
C          switched to small strain
           OFFS(I)=MIN(LBUF%OFF(I),OFFS(I))
           IOFFS  =1
         END IF
        ENDDO
C
        IF (JTHE < 0 .AND. ISOLNOD == 10) THEN
          CALL S10THERM(PM     ,MXT    ,NC ,VOLN, 
     .    	    PX(1,1,IP), PY(1,1,IP), PZ(1,1,IP),NX(1,1,IP),
     .    	    DT1 ,TEMP  ,TEMPEL ,DIE  , THEM,GBUF%OFF,LBUF%OFF) 
        ENDIF
c----------
      ENDDO   ! end integration point loop

      IF (JTHE < 0 .AND. ISOLNOD == 4) THEN
          CALL S4THERM_ITET1(PM   ,MXT    ,NC   ,NEL  ,
     .    	                 XX   ,YY     ,ZZ   ,DT1  ,DIE ,
     .    	                 TEMP ,THEM   ,GBUF%OFF   ,LBUF%OFF) 
      ENDIF
c-----------------------------
      IF (JLAG+JALE+JEUL /= 0) THEN
c
C-----  small strain
C------ correction of GBUF%SMSTR will be done only from next cycle	
        CALL S10MALLB3(GBUF%SMSTR,GBUF%OFF,WXXG,WYYG,WZZG,NEL)
        IF (IOFFS == 1)THEN
          DO I=LFT,LLT
C           switch to small strain
            IF (OFFS(I)<=TWO) GBUF%OFF(I) = OFFS(I)
          END DO
C
        IPTS = 1
        IPTT = 1
        ILAY = 1
          DO IP=1,NPT
           IPTR = IP
           LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IPTR,IPTS,IPTT)
            DO I=LFT,LLT
              IF (GBUF%OFF(I) > ONE) LBUF%OFF(I)=GBUF%OFF(I)
            END DO
          END DO
        END IF
c
        CALL SMALLB3(GBUF%OFF,OFF)
        CALL S10MALLGEO3(NGL,GBUF%OFF ,VOLN ,DELTAX, GBUF%VOL ,
     .                       RX , RY , RZ , 
     .                       SX , SY , SZ ,
     .                       TX , TY , TZ )
C-----------!!!-seperate LBUF%SIGL=LBUF%SIG,w/ npt>0 ISM12_11
       IF (ISMSTR == 12.AND.IDTMIN(1)==3) THEN
        IOFFS =0
         DO I=LFT,LLT
          IF(GBUF%OFF(I)/=OFFG0(I).AND.ABS(GBUF%OFF(I)) > ONE ) IOFFS=1
         ENDDO
         IF (IOFFS == 1) THEN
           CALL S10SAV12(GBUF%OFF,OFFG0,GBUF%SMSTR,NC   ,
     +                   XX   ,YY   ,ZZ   ,NEL )
           IF (ISM12_11>0 .AND. ISORTH == 0) THEN
             CALL S10UPD11T12(ELBUF_TAB(NG) ,GBUF%OFF   ,OFFG0  ,NC   ,
     +                        XX   ,YY   ,ZZ   ,NEL )
           END IF 
          IPTS = 1
          IPTT = 1
          ILAY = 1
          DO IP=1,NPT
           IPTR = IP
           LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IPTR,IPTS,IPTT)
            DO I=LFT,LLT
              IF (ABS(GBUF%OFF(I)) > ONE) LBUF%OFF(I)=GBUF%OFF(I)
            END DO
          END DO
         END IF !(IOFFS == 1) THEN
       END IF 
c-----------------------------
      IF(NFILSOL/=0) CALL SXFILLOPT(NPE,GBUF%FILL,STIG,FX,FY,FZ)
C-----
        IF (IPARIT == 0) THEN
          CALL S10CUMU3(GBUF%OFF,A,NC,STIFN,STIG,FX,FY,FZ,DELTAX2,
     .                 THEM    ,FTHE,AR,X,STIFR,GBUF%SMSTR,CONDN,CONDEG,
     .                 ITAGDN  ,NEL)
          ELSE
          CALL S10CUMU3P(GBUF%OFF,STIG,FSKY   ,FSKY   ,IADS,FX,FY,
     .                 FZ  ,DELTAX2,IADS10 ,NC,THEM, 
     .                 FTHESKY ,AR,X,GBUF%SMSTR,CONDNSKY,CONDEG,
     .                 ITAGDN  ,NEL)
        ENDIF
c-----
      ENDIF
c-----------
      RETURN
      END
Chd|====================================================================
Chd|  S10PIJ12                      source/elements/solid/solide10/s10forc3.F
Chd|-- called by -----------
Chd|        S10UPD11T12                   source/elements/solid/solide10/s10upd11t12.F
Chd|-- calls ---------------
Chd|        S10JACOBNEL                   source/elements/solid/solide10/s10jacob.F
Chd|====================================================================
      SUBROUTINE S10PIJ12(NPT,XX, YY, ZZ, PX,PY,PZ ,NEL)
C-----------------------------------------------
C   M o d u l e s
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
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER NEL,NPT
      DOUBLE PRECISION
     .   XX(MVSIZ,10), YY(MVSIZ,10), ZZ(MVSIZ,10)
      my_real
     .   PX(MVSIZ,10,5),PY(MVSIZ,10,5),PZ(MVSIZ,10,5)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IP,N,K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,
     .        M,IPERM(10,4)
      DOUBLE PRECISION
     .   XA(MVSIZ,10),YA(MVSIZ,10),ZA(MVSIZ,10),
     .   XB(MVSIZ,10),YB(MVSIZ,10),ZB(MVSIZ,10),
     .   A4,B4,A4M1,B4M1,AA,VOLDP(MVSIZ)
      my_real
     .   ALPH,BETA,W,VOL(MVSIZ)
      DATA IPERM/
     .            2, 4, 3, 1, 9,10, 6, 5, 8, 7,
     .            4, 1, 3, 2, 8, 7,10, 9, 5, 6,
     .            1, 4, 2, 3, 8, 9, 5, 7,10, 6,
     .            1, 2, 3, 4, 5, 6, 7, 8, 9,10/
C-----------------------------------------------
      IF(NPT==4)THEN
        ALPH = ZEP5854102
        BETA  = ZEP1381966
        W = FOURTH
      ELSEIF(NPT==5)THEN
        ALPH = HALF
        BETA  = ONE_OVER_6
        W = NINE_OVER_20
      ENDIF
      A4 = FOUR * ALPH
      B4 = FOUR * BETA
      A4M1  = A4- ONE
      B4M1  = B4- ONE
C
      DO N=1,4
        DO I=1,NEL
          XA(I,N) = A4M1*XX(I,N)
          YA(I,N) = A4M1*YY(I,N)
          ZA(I,N) = A4M1*ZZ(I,N)
C
          XB(I,N) = B4M1*XX(I,N)
          YB(I,N) = B4M1*YY(I,N)
          ZB(I,N) = B4M1*ZZ(I,N)
        ENDDO
      ENDDO
C
      DO N=5,10
        DO I=1,NEL
          XA(I,N) = A4*XX(I,N)
          YA(I,N) = A4*YY(I,N)
          ZA(I,N) = A4*ZZ(I,N)
C
          XB(I,N) = B4*XX(I,N)
          YB(I,N) = B4*YY(I,N)
          ZB(I,N) = B4*ZZ(I,N)
        ENDDO
      ENDDO
C
      DO IP=1,4
        K1 = IPERM(1,IP)
        K2 = IPERM(2,IP)
        K3 = IPERM(3,IP)
        K4 = IPERM(4,IP)
        K5 = IPERM(5,IP)
        K6 = IPERM(6,IP)
        K7 = IPERM(7,IP)
        K8 = IPERM(8,IP)
        K9 = IPERM(9,IP)
        K10= IPERM(10,IP)
        CALL S10JACOBNEL(ALPH,BETA,W,
     .   XB(1,K1),XB(1,K2),XB(1,K3),XA(1,K4),XB(1,K5),
     .   XB(1,K6),XB(1,K7),XB(1,K8),XB(1,K9),XB(1,K10),
     .   XA(1,K8),XA(1,K9),XA(1,K10),
     .   YB(1,K1),YB(1,K2),YB(1,K3),YA(1,K4),YB(1,K5),
     .   YB(1,K6),YB(1,K7),YB(1,K8),YB(1,K9),YB(1,K10),
     .   YA(1,K8),YA(1,K9),YA(1,K10),
     .   ZB(1,K1),ZB(1,K2),ZB(1,K3),ZA(1,K4),ZB(1,K5),
     .   ZB(1,K6),ZB(1,K7),ZB(1,K8),ZB(1,K9),ZB(1,K10),
     .   ZA(1,K8),ZA(1,K9),ZA(1,K10),
     .   PX(1,K1,IP) ,PX(1,K2,IP),PX(1,K3,IP),PX(1,K4,IP),PX(1,K5,IP),
     .   PX(1,K6,IP) ,PX(1,K7,IP),PX(1,K8,IP),PX(1,K9,IP),PX(1,K10,IP),
     .   PY(1,K1,IP) ,PY(1,K2,IP),PY(1,K3,IP),PY(1,K4,IP),PY(1,K5,IP),
     .   PY(1,K6,IP) ,PY(1,K7,IP),PY(1,K8,IP),PY(1,K9,IP),PY(1,K10,IP),
     .   PZ(1,K1,IP) ,PZ(1,K2,IP),PZ(1,K3,IP),PZ(1,K4,IP),PZ(1,K5,IP),
     .   PZ(1,K6,IP) ,PZ(1,K7,IP),PZ(1,K8,IP),PZ(1,K9,IP),PZ(1,K10,IP),
     .   VOL ,VOLDP,NEL)
c
      ENDDO
C
C
      IF(NPT==5)THEN
        ALPH = FOURTH
        BETA  = FOURTH
        A4 = ONE
        B4 = ONE
        A4M1  = ZERO
        B4M1  = ZERO
        W = - FOUR_OVER_5
        IP = 5
C
        DO N=1,4
         DO I=1,NEL
          XA(I,N) = ZERO
          YA(I,N) = ZERO
          ZA(I,N) = ZERO
         ENDDO
        ENDDO
CC
        CALL S10JACOBNEL(ALPH,BETA,W,
     .   XA(1,K1),XA(1,K2),XA(1,K3),XA(1,K4),XX(1,K5),
     .   XX(1,K6),XX(1,K7),XX(1,K8),XX(1,K9),XX(1,K10),
     .   XX(1,K8),XX(1,K9),XX(1,K10),
     .   YA(1,K1),YA(1,K2),YA(1,K3),YA(1,K4),YY(1,K5),
     .   YY(1,K6),YY(1,K7),YY(1,K8),YY(1,K9),YY(1,K10),
     .   YY(1,K8),YY(1,K9),YY(1,K10),
     .   ZA(1,K1),ZA(1,K2),ZA(1,K3),ZA(1,K4),ZZ(1,K5),
     .   ZZ(1,K6),ZZ(1,K7),ZZ(1,K8),ZZ(1,K9),ZZ(1,K10),
     .   ZZ(1,K8),ZZ(1,K9),ZZ(1,K10),
     .   PX(1,K1,IP) ,PX(1,K2,IP),PX(1,K3,IP),PX(1,K4,IP),PX(1,K5,IP),
     .   PX(1,K6,IP) ,PX(1,K7,IP),PX(1,K8,IP),PX(1,K9,IP),PX(1,K10,IP),
     .   PY(1,K1,IP) ,PY(1,K2,IP),PY(1,K3,IP),PY(1,K4,IP),PY(1,K5,IP),
     .   PY(1,K6,IP) ,PY(1,K7,IP),PY(1,K8,IP),PY(1,K9,IP),PY(1,K10,IP),
     .   PZ(1,K1,IP) ,PZ(1,K2,IP),PZ(1,K3,IP),PZ(1,K4,IP),PZ(1,K5,IP),
     .   PZ(1,K6,IP) ,PZ(1,K7,IP),PZ(1,K8,IP),PZ(1,K9,IP),PZ(1,K10,IP),
     .   VOL ,VOLDP,NEL)
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  S10PIJSAV12                   source/elements/solid/solide10/s10forc3.F
Chd|-- called by -----------
Chd|        S10UPD11T12                   source/elements/solid/solide10/s10upd11t12.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S10PIJSAV12(PX,PY,PZ, PIJ,SIG,SIGL,INDEX,NB,NEL)
C-----------------------------------------------
C   M o d u l e s
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
      INTEGER NEL,NB,INDEX(*)
C     REAL
      my_real
     .   PX(MVSIZ,10),PY(MVSIZ,10),PZ(MVSIZ,10),
     .   PIJ(NEL,3,10),SIG(NEL,6),SIGL(NEL,6)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K

C-----------------------------------------------
#include "vectorize.inc"
      DO J=1,NB
        I = INDEX(J)
        PIJ(I,1,1:10) = PX(J,1:10) 
        PIJ(I,2,1:10) = PY(J,1:10) 
        PIJ(I,3,1:10) = PZ(J,1:10) 
        SIGL(I,1:6) = SIG(I,1:6)
      END DO
C-----------
      RETURN
      END

