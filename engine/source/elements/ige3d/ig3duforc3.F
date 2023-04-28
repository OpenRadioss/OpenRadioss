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
Chd|  IG3DUFORC3                    source/elements/ige3d/ig3duforc3.F
Chd|-- called by -----------
Chd|        FORINT                        source/elements/forint.F      
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        IG3DAVERAGE                   source/elements/ige3d/ig3daverage.F
Chd|        IG3DCUMU3                     source/elements/ige3d/ig3dcumu3.F
Chd|        IG3DDERISHAP                  source/elements/ige3d/ig3dderishap.F
Chd|        IG3DFINT                      source/elements/ige3d/ig3dfint.F
Chd|        IG3DONEDERIV                  source/elements/ige3d/ig3donederiv.F
Chd|        IGE3DAIRE                     source/elements/ige3d/ig3daire.F
Chd|        IGE3DBILAN                    source/elements/ige3d/ige3dbilan.F
Chd|        IGE3DDEFO                     source/elements/ige3d/ige3ddefo.F
Chd|        IGE3DZERO                     source/elements/ige3d/ige3dzero.F
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        SMALLB3                       source/elements/solid/solide/smallb3.F
Chd|        SRHO3                         source/elements/solid/solide/srho3.F
Chd|        SROTA3                        source/elements/solid/solide/srota3.F
Chd|        SSTRA3                        source/elements/solid/solide/sstra3.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MMAIN_MOD                     source/materials/mat_share/mmain.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE IG3DUFORC3(
     1   ELBUF_TAB,   NG,          LFT,         LLT,
     2   NFT,         NEL,         IXS,         PM,
     3   GEO,         IPM,         IGEO,        X,
     4   A,           AR,          V,           VR,
     5   W,           D,           MS,          IN,
     6   TF,          NPF,         BUFMAT,      IPARG,
     7   IPARTS,      PARTSAV,     NLOC_DMG,    FSKY,
     8   FR_WAVE,     IADS,        EANI,        STIFN,
     9   STIFR,       FX,          FY,          FZ,
     A   IFAILURE,    MTN,         IGTYP,       NPT,
     B   JSMS,        MSSA,        DMELS,       KXIG3D,
     C   IXIG3D,      KNOT,        NCTRL,       WIGE,
     D   FLUX,        FLU1,        DT2T,        NELTST,
     E   ITYPTST,     OFFSET,      TABLE,       IEXPAN,
     F   ALE_CONNECT, FV,          ITASK,       IOUTPRT,
     G   PX,          PY,          PZ,          KNOTLOCPC,
     H   KNOTLOCEL,   GRESAV,      GRTH,        IGRTH,
     I   MAT_ELEM,    H3D_STRAIN,  ISMSTR,      JALE,
     J   JEUL,        JLAG,        JCVT,        JPLASOL,
     K   JSPH)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MMAIN_MOD
      USE TABLE_MOD
      USE MAT_ELEM_MOD            
      USE MESSAGE_MOD
      USE NLOCAL_REG_MOD
      USE ALE_CONNECTIVITY_MOD
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
#include      "com08_c.inc"
#include      "scr19_c.inc"
#include      "param_c.inc"
#include      "timeri_c.inc"
#include      "scr18_c.inc"
#include      "ige3d_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(INOUT) :: JPLASOL
      INTEGER, INTENT(INOUT) :: JSPH
      INTEGER, INTENT(IN) :: JCVT
      INTEGER, INTENT(IN) :: ISMSTR
      INTEGER, INTENT(IN) :: JALE
      INTEGER, INTENT(IN) :: JEUL
      INTEGER, INTENT(IN) :: JLAG
      INTEGER LFT,LLT,NEL,NFT,MTN,IGTYP,IFAILURE,NPT,JSMS,
     .        NCTRL,NG,NELTST,ITYPTST,OFFSET,IEXPAN,ITASK,H3D_STRAIN
      INTEGER IXS(NIXS,*), IPARG(NPARG,*), NPF(*),IADS(8,*),
     .        IPARTS(*), IGEO(NPROPGI,*), IPM(NPROPMI,*),
     .        KXIG3D(NIXIG3D,*),IXIG3D(*),FLUX(6,*),FLU1(*),
     .        IOUTPRT,PX,PY,PZ,GRTH(*),IGRTH(*)
      my_real
     .   PM(NPROPM,*), GEO(NPROPG,*),X(3,*),A(3,*),V(3,*),MS(*),W(*),
     .   AR(3,*), VR(3,*), IN(3,*),D(3,*),TF(*), BUFMAT(*),FR_WAVE(*),
     .   PARTSAV(*),STIFN(*), STIFR(*), FSKY(*),EANI(*),
     .   FX(MVSIZ,*),FY(MVSIZ,*),FZ(MVSIZ,*),
     .   MSSA(*), DMELS(*),KNOT(*),WIGE(*),DT2T, FV(*),KNOTLOCPC(DEG_MAX,3,*),
     .   KNOTLOCEL(2,3,*),GRESAV(*)
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE(TTABLE) TABLE(*)
      TYPE (NLOCAL_STR_)  , TARGET :: NLOC_DMG 
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
      TYPE (MAT_ELEM_) ,INTENT(INOUT) :: MAT_ELEM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NF1,IFLAG,IG,IGT,NUPARAM,
     .   NUVAR,NUVARP,IMAT,IPID,N1,N2,N3,NKNOT1,NKNOT2,NKNOT3,
     .   IDX(MVSIZ),IDY(MVSIZ),IDZ(MVSIZ),IFUNC(MAXFUNC),NFUNC,IADBUF,
     .   IBID,ISTRAIN,IBIDV(1),IP,ILAY,IERROR,IAD_KNOT,IDFRSTLOCKNT, IDPC,
     .   IDX2(MVSIZ),IDY2(MVSIZ),IDZ2(MVSIZ)
C-----
      INTEGER SID(MVSIZ),IPROP,NC(MVSIZ,8),
     .   MAT(MVSIZ)
      my_real
     . MX(NEL,NCTRL),MY(NEL,NCTRL) , MZ(NEL,NCTRL),
     .   STI(MVSIZ),STIN(MVSIZ),STIR(MVSIZ), VISCM(MVSIZ) ,VISCR(MVSIZ),RHO0(MVSIZ)
      my_real
     .   OFF(MVSIZ) , RHOO(MVSIZ),FR_W_E(MVSIZ),
     .   XX(NCTRL,NEL),YY(NCTRL,NEL),ZZ(NCTRL,NEL), 
     .   DX(NCTRL,NEL),DY(NCTRL,NEL),DZ(NCTRL,NEL), 
     .   UX(NCTRL,NEL),UY(NCTRL,NEL),UZ(NCTRL,NEL),
     .   VX(NCTRL,NEL),VY(NCTRL,NEL),VZ(NCTRL,NEL),
     .   VRX(NCTRL,NEL),VRY(NCTRL,NEL),VRZ(NCTRL,NEL),
     .   DTE(MVSIZ)  ,WW(NCTRL,NEL),RBID, ZR, ZS, ZT

      TYPE(G_BUFEL_) ,POINTER :: GBUF 
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      TYPE(BUF_MAT_) ,POINTER :: MBUF    
      my_real,
     .   DIMENSION(:),POINTER :: UVAR
      INTEGER MXT(MVSIZ),NGL(MVSIZ),NGEO(MVSIZ)
      my_real
     . VOLN(MVSIZ), VD2(MVSIZ) , DVOL(MVSIZ),DELTAX(MVSIZ),
     . VIS(MVSIZ) , QVIS(MVSIZ), CXX(MVSIZ) ,
     . S1(MVSIZ)  , S2(MVSIZ)  , S3(MVSIZ)  ,
     . S4(MVSIZ)  , S5(MVSIZ)  , S6(MVSIZ)  ,
     . D4(MVSIZ)  , D5(MVSIZ)  , D6(MVSIZ)  , 
     . AJC1(MVSIZ) , AJC2(MVSIZ) , AJC3(MVSIZ) ,
     . AJC4(MVSIZ) , AJC5(MVSIZ) , AJC6(MVSIZ) ,
     . AJC7(MVSIZ) , AJC8(MVSIZ) , AJC9(MVSIZ) ,
     . AJ1(MVSIZ) , AJ2(MVSIZ) , AJ3(MVSIZ) ,
     . AJ4(MVSIZ) , AJ5(MVSIZ) , AJ6(MVSIZ),
     . WXX(MVSIZ) , WYY(MVSIZ) , WZZ(MVSIZ),
     . VDX(MVSIZ) , VDY(MVSIZ) , VDZ(MVSIZ),
     . MUVOID(MVSIZ),SSP_EQ(MVSIZ),AIRE(MVSIZ),
     . SIGY(MVSIZ),ET(MVSIZ),R1_FREE(MVSIZ),
     . R3_FREE(MVSIZ),R4_FREE(MVSIZ),DEFP(MVSIZ),
     . MFXX(1,1),MFXY(1,1),MFYX(1,1),
     . MFYY(1,1),MFYZ(1,1),MFZY(1,1),
     . MFZZ(1,1),MFZX(1,1),MFXZ(1,1),
     . GAMA(MVSIZ,6),BID(MVSIZ),TEMPEL(MVSIZ),DIE(MVSIZ),
     . STIG(MVSIZ,NCTRL)
C 
      my_real 
     .   DXX(MVSIZ), DYY(MVSIZ), DZZ(MVSIZ),
     .   DXY(MVSIZ), DXZ(MVSIZ), DYX(MVSIZ), 
     .   DYZ(MVSIZ), DZX(MVSIZ), DZY(MVSIZ),DIVDE(MVSIZ)

      INTEGER ITEL, ITNCTRL, K, N, JJ, INCTRL, L, IFACE
      my_real,
     .  DIMENSION(NCTRL) :: R 
      my_real,
     .  DIMENSION(NCTRL,3) :: DRDXI
      my_real,
     .  DIMENSION(NCTRL,MVSIZ) :: MATN
      my_real,
     .  DIMENSION(3*NCTRL,MVSIZ) :: MATB
      my_real,
     .  DIMENSION(MVSIZ) :: MATDET
      my_real
     . DETJAC, PGAUSS, VOLG(MVSIZ)
      my_real
     .   DD,BtDBAloc(3*NCTRL,MVSIZ),
     .   BA(6,MVSIZ),DBA(6,MVSIZ), Aloc(3*NCTRL,MVSIZ),
     .   MASS(NCTRL,MVSIZ),MMUNK(MVSIZ),KNOTLOCX(PX+1,NCTRL,MVSIZ),
     .   KNOTLOCY(PY+1,NCTRL,MVSIZ),KNOTLOCZ(PZ+1,NCTRL,MVSIZ),
     .   KNOTLOCELX(2,MVSIZ),
     .   KNOTLOCELY(2,MVSIZ),KNOTLOCELZ(2,MVSIZ)
      my_real
     .   AIRENURBS(3), AFACE(6,MVSIZ), TC, LC, 
     .   VMIN(MVSIZ), SMAX(MVSIZ), SUMV,AMU(MVSIZ)
      my_real VARNL(NEL)
      my_real,
     .   ALLOCATABLE, DIMENSION(:,:) :: VGAUSS
C----------------------------------------------------------
      DOUBLE PRECISION
     .  W_GAUSS(9,9),A_GAUSS(9,9),VOLDP(MVSIZ)
      DATA W_GAUSS / 
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
C-----------------------------------------------
C   S o u r c e  L i n e s
C=======================================================================
      IBID = 0
      IBIDV = 0
      ISTRAIN = 1
      BID = ZERO
      RBID = ZERO
      GBUF => ELBUF_TAB(NG)%GBUF
      UVAR  => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR
      IPROP = IPARG(62,NG)
      ILAY   = 1
      NF1=NFT+1
      KNOTLOCX = ZERO
      KNOTLOCY = ZERO
      KNOTLOCZ = ZERO
      KNOTLOCELX = ZERO
      KNOTLOCELY = ZERO
      KNOTLOCELZ = ZERO
c a modifier
      OFF = ONE
      DO I=LFT,LLT
        IMAT = KXIG3D(1,I+NFT)
        NGEO(I)=KXIG3D(2,I+NFT)
        MXT(I)=IMAT
        VIS(I)=ZERO
        QVIS(I)=ZERO
        VDX(I)=ZERO
        VDY(I)=ZERO
        VDZ(I)=ZERO
        VD2(I)=ZERO
c
        DO J=1,NCTRL
          IF( J <= KXIG3D(3,I+NFT) ) THEN
            XX(J,I)=X(1,IXIG3D(KXIG3D(4,I+NFT)+J-1))
            YY(J,I)=X(2,IXIG3D(KXIG3D(4,I+NFT)+J-1))
            ZZ(J,I)=X(3,IXIG3D(KXIG3D(4,I+NFT)+J-1))
            DX(J,I)=D(1,IXIG3D(KXIG3D(4,I+NFT)+J-1))
            DY(J,I)=D(2,IXIG3D(KXIG3D(4,I+NFT)+J-1))
            DZ(J,I)=D(3,IXIG3D(KXIG3D(4,I+NFT)+J-1))
            VX(J,I)=V(1,IXIG3D(KXIG3D(4,I+NFT)+J-1))
            VY(J,I)=V(2,IXIG3D(KXIG3D(4,I+NFT)+J-1))
            VZ(J,I)=V(3,IXIG3D(KXIG3D(4,I+NFT)+J-1))
            WW(J,I)=1!WIGE(IXIG3D(KXIG3D(4,I+NFT)+J-1))
            DO K=1,PX+1
              KNOTLOCX(K,J,I)=KNOTLOCPC(K,1,(NGEO(I)-1)*NUMNOD+IXIG3D(KXIG3D(4,I+NFT)+J-1))
            ENDDO
            DO K=1,PY+1
              KNOTLOCY(K,J,I)=KNOTLOCPC(K,2,(NGEO(I)-1)*NUMNOD+IXIG3D(KXIG3D(4,I+NFT)+J-1))
            ENDDO
            DO K=1,PZ+1
              KNOTLOCZ(K,J,I)=KNOTLOCPC(K,3,(NGEO(I)-1)*NUMNOD+IXIG3D(KXIG3D(4,I+NFT)+J-1))
            ENDDO
          ENDIF
        ENDDO
        NGL(I) = KXIG3D(5,I+NFT)
        IDX(I) = KXIG3D(6,I+NFT)
        IDY(I) = KXIG3D(7,I+NFT)
        IDZ(I) = KXIG3D(8,I+NFT)
        IDX2(I) = KXIG3D(9,I+NFT)
        IDY2(I) = KXIG3D(10,I+NFT)
        IDZ2(I) = KXIG3D(11,I+NFT)
        KNOTLOCELX(1,I) = KNOTLOCEL(1,1,I+NFT)
        KNOTLOCELY(1,I) = KNOTLOCEL(1,2,I+NFT)
        KNOTLOCELZ(1,I) = KNOTLOCEL(1,3,I+NFT)
        KNOTLOCELX(2,I) = KNOTLOCEL(2,1,I+NFT)
        KNOTLOCELY(2,I) = KNOTLOCEL(2,2,I+NFT)
        KNOTLOCELZ(2,I) = KNOTLOCEL(2,3,I+NFT)
        RHO0(I)= PM(1,IMAT)
      ENDDO
      IAD_KNOT = IGEO(40,IPROP)
      N1 = IGEO(44,IPROP)
      N2 = IGEO(45,IPROP)
      N3 = IGEO(46,IPROP)
      IDFRSTLOCKNT = IGEO(47,IPROP)
      NKNOT1 = N1+PX
      NKNOT2 = N2+PY
      NKNOT3 = N3+PZ
C-----------
      IADBUF =  IPM(7,IMAT)
      NUVAR   = IPM(8,IMAT)
      NUPARAM = IPM(9,IMAT)
      NFUNC  = IPM(10,IMAT)
      DO I=1,NFUNC
        IFUNC(I) = IPM(10+I,IMAT)
      ENDDO

C-----------AMU for SP is not really implemented
      IF (TT==ZERO) THEN
       DO I=1,PX  
       DO J=1,PY
        DO K=1,PZ 
         LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(I,J,K)
         LBUF%VOL0DP(LFT:LLT) = LBUF%VOL(LFT:LLT)
        ENDDO
       ENDDO
       ENDDO ! FIN BOUCLE SUR LES POINTS D'INTEGRATION
      END IF
C------------------------------------------------------
C     INITIALISATION DES VARIABLES
C------------------------------------------------------

      DELTAX=EP20
      BID = ZERO
      IBID = 0
      TC = EP10
      SMAX(:)=ZERO

      ALLOCATE(VGAUSS(PX*PY*PZ,MVSIZ),STAT=IERROR)
      IF(IERROR/=0)THEN
       CALL ANCMSG(MSGID=246,ANMODE=ANINFO)
       CALL ARRET(2)
      END IF
      VGAUSS(:,:)=ZERO

      CALL IGE3DZERO(
     1   NCTRL,      VOLG,       GBUF%SIG,   GBUF%EINT,
     2   GBUF%RHO,   GBUF%QVIS,  FX,         FY,
     3   FZ,         BtDBAloc,   STIG,       MASS,
     4   MMUNK,      AFACE,      VMIN,       GBUF%PLA,
     5   GBUF%EPSD,  GBUF%G_PLA, GBUF%G_EPSD,NEL)

      N=0
      DO I=1,PX  
       DO J=1,PY
        DO K=1,PZ 

         N=N+1
         ZR = A_GAUSS(I,PX)
         ZS = A_GAUSS(J,PY)
         ZT = A_GAUSS(K,PZ)
         PGAUSS = W_GAUSS(I,PX)*W_GAUSS(J,PY)*W_GAUSS(K,PZ)

         LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(I,J,K)

C------------------------------------------------------
C     CALCUL DES FONCTIONS DE FORME ET DES DERIVEES
C------------------------------------------------------

         DO ITEL=LFT,LLT

c          CALL IGE3DDERIV(
c     .       ITEL     ,N   ,XX(:,ITEL),YY(:,ITEL),ZZ(:,ITEL),WW(:,ITEL),  
c     .       IDX(ITEL), IDY(ITEL), IDZ(ITEL), DRDXI, R, DETJAC, 
c     .       NCTRL, ZR, ZS, ZT, KNOT(IAD_KNOT+1), KNOT(IAD_KNOT+NKNOT1+1),
c     .       KNOT(IAD_KNOT+NKNOT1+NKNOT2+1), PX-1, PY-1, PZ-1, 1)

          CALL IG3DONEDERIV(
     1      ITEL      ,N             ,XX(:,ITEL)  ,YY(:,ITEL),
     2      ZZ(:,ITEL),WW(:,ITEL)    ,IDX(ITEL)   ,IDY(ITEL) ,
     3      IDZ(ITEL) ,KNOTLOCX(:,:,ITEL) ,KNOTLOCY(:,:,ITEL),KNOTLOCZ(:,:,ITEL) ,
     4      DRDXI     ,R             ,DETJAC      ,NCTRL    ,
     5      ZR        ,ZS            ,ZT          ,KNOT(IAD_KNOT+1),
     6      KNOT(IAD_KNOT+NKNOT1+1),KNOT(IAD_KNOT+NKNOT1+NKNOT2+1),PX-1, 
     7      PY-1      ,PZ-1          ,1           ,
     8      IDX2(ITEL),IDY2(ITEL)    ,IDZ2(ITEL)  ,
     9      KNOTLOCELX(:,ITEL),KNOTLOCELY(:,ITEL),KNOTLOCELZ(:,ITEL))
       
          VOLN(ITEL) = PGAUSS*DETJAC 
          VGAUSS(N,ITEL) = PGAUSS*DETJAC
          VOLG(ITEL) = VOLG(ITEL) + VOLN(ITEL)

          IF(IDTMIN(101)==1)THEN
           DO ITNCTRL=1,NCTRL
            MASS(ITNCTRL,ITEL)=MASS(ITNCTRL,ITEL)+PM(89,MXT(ITEL))*R(ITNCTRL)*LBUF%VOL(ITEL)
           ENDDO
          ENDIF

C------------------------------------------------------
C     ASSEMBLAGE DE LA MATRICE MATB, MATN, MATDET
C------------------------------------------------------
         
          CALL IG3DDERISHAP(
     .      ITEL  ,NCTRL ,R     ,DRDXI ,
     .      DETJAC,MATN  ,MATB  ,MATDET) 

         ENDDO 

C------------------------------------------------------
C     STRAIN RATE
C------------------------------------------------------

         CALL IGE3DDEFO(
     1   VX,      VY,      VZ,      MATB,
     2   NCTRL,   WXX,     WYY,     WZZ,
     3   DXX,     DYY,     DZZ,     DXY,
     4   DYX,     DYZ,     DZY,     DXZ,
     5   DZX,     D4,      D5,      D6,
     6   BA,      Aloc,    NEL)
 
C------------------------------------------------------
C     ROTATION DE CORPS RIGIDE DES CONTRAINTES PASSEES 
C------------------------------------------------------

         CALL SROTA3(
     1   LBUF%SIG,S1,      S2,      S3,
     2   S4,      S5,      S6,      WXX,
     3   WYY,     WZZ,     NEL,     MTN,
     4   IPARG(9,NG))

C------------------------------------------------------
C     CALCUL DE LA MASSE VOLUMIQUE COURANTE
C------------------------------------------------------
         VOLDP(LFT:LLT) = VOLN(LFT:LLT)
         DIVDE(1:NEL) = DT1*(DXX(1:NEL)+ DYY(1:NEL)+ DZZ(1:NEL))   

         CALL SRHO3(
     1   PM,          LBUF%VOL,    LBUF%RHO,    LBUF%EINT,
     2   DIVDE,       FLUX(1,NF1), FLU1(NF1),   VOLN,
     3   DVOL,        NGL,         MXT,         OFF,
     4   IPARG(64,NG),GBUF%TAG22,  VOLDP,       LBUF%VOL0DP,
     5   AMU,         GBUF%OFF,    NEL,         MTN,
     6   JALE,        ISMSTR,      JEUL,        JLAG)

c      CALL SMALL3(GBUF%SMSTR,GBUF%OFF,OFF,WXX,WYY,
c     .            WZZ,DXX,DYY,DZZ,GBUF%RHO,RHO0,
c     .            DVOL,VOLN)

C------------------------------------------------------
C     CALCUL DES CONTRAINTES 
C------------------------------------------------------

      IF ((ITASK==0).AND.(IMON_MAT==1)) CALL STARTIME(35,1)
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
     C   WYY,         WZZ,         AJ1,         AJ2,
     D   AJ3,         AJ4,         AJ5,         AJ6,
     E   VDX,         VDY,         VDZ,         MUVOID,
     F   SSP_EQ,      AIRE,        SIGY,        ET,
     G   R1_FREE,     DEFP,        R3_FREE,     AMU,
     H   MFXX,        MFXY,        MFXZ,        MFYX,
     I   MFYY,        MFYZ,        MFZX,        MFZY,
     J   MFZZ,        IPM,         GAMA,        BID,
     K   DXY,         DYX,         DYZ,         DZY,
     L   DZX,         DXZ,         ISTRAIN,     TEMPEL,
     M   DIE,         IEXPAN,      ILAY,        MSSA,
     N   DMELS,       I,           J,           K,
     O   TABLE,       BID,         BID,         BID,
     P   BID,         IPARG(1,NG), IGEO,        BID,
     Q   ITASK,       NLOC_DMG,    VARNL,       MAT_ELEM,
     R   H3D_STRAIN,  JPLASOL,     JSPH)

       CALL SSTRA3(
     1   DXX,      DYY,      DZZ,      D4,
     2   D5,       D6,       LBUF%STRA,WXX,
     3   WYY,      WZZ,      OFF,      NEL,
     4   JCVT)

      IF ((ITASK==0).AND.(IMON_MAT==1)) CALL STOPTIME(35,1)

C------------------------------------------------------
C     FORCES INTERNES
C------------------------------------------------------

      CALL IG3DFINT(
     1   PM,      MXT,     KXIG3D,  LBUF%SIG,
     2   NCTRL,   MATB,    FX,      FY,
     3   FZ,      VOLN,    BtDBAloc,DBA,
     4   SSP_EQ,  STIG,    NEL,     NFT)

           ENDDO
         ENDDO
       ENDDO ! FIN BOUCLE SUR LES POINTS D'INTEGRATION

C-----------------------------
C     SMALL STRAIN 
C-----------------------------

      CALL SMALLB3(
     1   GBUF%OFF,OFF,     NEL,     ISMSTR)

C------------------------------------------------------
c
      N=0
      DO I=1,PX  
       DO J=1,PY
        DO K=1,PZ 

         N=N+1
         LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(I,J,K)

         CALL IG3DAVERAGE(
     1   LBUF%SIG,   GBUF%SIG,   LBUF%VOL,   GBUF%VOL,
     2   LBUF%RHO,   LBUF%EINT,  GBUF%EINT,  GBUF%RHO,
     3   VGAUSS(N,:),VOLG,       LBUF%PLA,   GBUF%PLA,
     4   GBUF%G_PLA, LBUF%EPSD,  GBUF%EPSD,  NEL,
     5   IPARG(40,NG))

        ENDDO
       ENDDO
      ENDDO ! FIN BOUCLE SUR LES POINTS D'INTEGRATION
c
C------------------------------------------------------
C     BILANS PAR MATERIAU
C------------------------------------------------------
      IFLAG=MOD(NCYCLE,NCPRI)
      IF (IOUTPRT>0)THEN
           CALL IGE3DBILAN(PARTSAV,GBUF%EINT,GBUF%RHO,VOLG,
     .                     VX, VY, VZ,IPARTS,GBUF%VOL,
     .                     GRESAV,GRTH,IGRTH,
     .                     XX, YY, ZZ, NCTRL,ITASK,IPARG(1,NG))
      ENDIF

C--------------------------------------------
C     CUMUL
C--------------------------------------------

      CALL IG3DCUMU3(
     1   IXIG3D,  KXIG3D,  NCTRL,   GBUF%OFF,
     2   A,       FX,      FY,      FZ,
     3   BtDBAloc,STIG,    STIFN,   NEL,
     4   NFT)



C------------------------------------------------------
C     PAS DE TEMPS ELEMENTAIRE : PULSATION PROPRE PAR ELEMENT
C------------------------------------------------------

      IF(IDTMIN(101)==1)THEN
       DO I=LFT,LLT
        DO J=1,NCTRL
         IF( J <= KXIG3D(3,I+NFT) ) THEN
          MMUNK(I) = MIN(MMUNK(I),MASS(J,I)/STIG(I,J))
         ENDIF
        ENDDO
        TC = SQRT(2*MMUNK(I))
        TC = DTFAC1(101)*TC
        IF(TC<DT2T)THEN
         DT2T   =TC
         ITYPTST=101
         NELTST =NGL(I)
        ENDIF
       ENDDO

C------------------------------------------------------
C     CALCUL DES AIRES DES FACES DES ELEMENTS
C------------------------------------------------------

      ELSEIF(IDTMIN(101)==2)THEN

       N=0
       DO I=1,PX
        DO J=1,PY
         N=N+1
         ZR = A_GAUSS(I,PX)
         ZS = A_GAUSS(J,PY)
         ZT = -ONE
         PGAUSS = W_GAUSS(I,PX)*W_GAUSS(J,PY) 

         DO ITEL=LFT,LLT

          CALL IGE3DAIRE(
     .      ITEL     ,N   ,XX(:,ITEL),YY(:,ITEL),ZZ(:,ITEL),WW(:,ITEL),  
     .      IDX(ITEL), IDY(ITEL), IDZ(ITEL), AIRENURBS,
     .      NCTRL, ZR, ZS, ZT, KNOT(IAD_KNOT+1), KNOT(IAD_KNOT+NKNOT1+1),
     .      KNOT(IAD_KNOT+NKNOT1+NKNOT2+1), PX-1, PY-1, PZ-1)
                      
          AFACE(1,ITEL) = AFACE(1,ITEL) + AIRENURBS(1)*PGAUSS
         
         ENDDO

         ZT = ONE

         DO ITEL=LFT,LLT
          CALL IGE3DAIRE(
     .      ITEL     ,N   ,XX(:,ITEL),YY(:,ITEL),ZZ(:,ITEL),WW(:,ITEL),  
     .      IDX(ITEL), IDY(ITEL), IDZ(ITEL), AIRENURBS,
     .      NCTRL, ZR, ZS, ZT, KNOT(IAD_KNOT+1), KNOT(IAD_KNOT+NKNOT1+1),
     .      KNOT(IAD_KNOT+NKNOT1+NKNOT2+1), PX-1, PY-1, PZ-1)
                      
          AFACE(2,ITEL) = AFACE(2,ITEL) + AIRENURBS(1)*PGAUSS
         ENDDO
        ENDDO
       ENDDO

       N=0
       DO I=1,PX
        DO K=1,PZ 
         N=N+1
         ZS = -ONE
         ZR = A_GAUSS(I,PX)
         ZT = A_GAUSS(K,PZ)
         PGAUSS = W_GAUSS(I,PX)*W_GAUSS(K,PZ)

         DO ITEL=LFT,LLT
          CALL IGE3DAIRE(
     .      ITEL     ,N   ,XX(:,ITEL),YY(:,ITEL),ZZ(:,ITEL),WW(:,ITEL),  
     .      IDX(ITEL), IDY(ITEL), IDZ(ITEL), AIRENURBS,
     .      NCTRL, ZR, ZS, ZT, KNOT(IAD_KNOT+1), KNOT(IAD_KNOT+NKNOT1+1),
     .      KNOT(IAD_KNOT+NKNOT1+NKNOT2+1), PX-1, PY-1, PZ-1)
         
          AFACE(3,ITEL) = AFACE(3,ITEL) + AIRENURBS(2)*PGAUSS
         ENDDO

         ZS = ONE 

         DO ITEL=LFT,LLT
          CALL IGE3DAIRE(
     .      ITEL     ,N   ,XX(:,ITEL),YY(:,ITEL),ZZ(:,ITEL),WW(:,ITEL),  
     .      IDX(ITEL), IDY(ITEL), IDZ(ITEL), AIRENURBS,
     .      NCTRL, ZR, ZS, ZT, KNOT(IAD_KNOT+1), KNOT(IAD_KNOT+NKNOT1+1),
     .      KNOT(IAD_KNOT+NKNOT1+NKNOT2+1), PX-1, PY-1, PZ-1)
                      
          AFACE(4,ITEL) = AFACE(4,ITEL) + AIRENURBS(2)*PGAUSS
         ENDDO
        ENDDO
       ENDDO

       N=0
       DO J=1,PY
        DO K=1,PZ 
         N=N+1
         ZR = -ONE
         ZS = A_GAUSS(J,PY)
         ZT = A_GAUSS(K,PZ)
         PGAUSS = W_GAUSS(J,PY)*W_GAUSS(K,PZ)

         DO ITEL=LFT,LLT
          CALL IGE3DAIRE(
     .      ITEL     ,N   ,XX(:,ITEL),YY(:,ITEL),ZZ(:,ITEL),WW(:,ITEL),  
     .      IDX(ITEL), IDY(ITEL), IDZ(ITEL), AIRENURBS,
     .      NCTRL, ZR, ZS, ZT, KNOT(IAD_KNOT+1), KNOT(IAD_KNOT+NKNOT1+1),
     .      KNOT(IAD_KNOT+NKNOT1+NKNOT2+1), PX-1, PY-1, PZ-1)
                      
          AFACE(5,ITEL) = AFACE(5,ITEL) + AIRENURBS(3)*PGAUSS
         ENDDO

         ZR = ONE 

         DO ITEL=LFT,LLT
          CALL IGE3DAIRE(
     .      ITEL     ,N   ,XX(:,ITEL),YY(:,ITEL),ZZ(:,ITEL),WW(:,ITEL),  
     .      IDX(ITEL), IDY(ITEL), IDZ(ITEL), AIRENURBS,
     .      NCTRL, ZR, ZS, ZT, KNOT(IAD_KNOT+1), KNOT(IAD_KNOT+NKNOT1+1),
     .      KNOT(IAD_KNOT+NKNOT1+NKNOT2+1), PX-1, PY-1, PZ-1)
                     
          AFACE(6,ITEL) = AFACE(6,ITEL) + AIRENURBS(3)*PGAUSS
         ENDDO
        ENDDO
       ENDDO

C------------------------------------------------------
C     PAS DE TEMPS ELEMENTAIRE : LONGUEUR CARACTERISTIQUE
C------------------------------------------------------

       DO ITEL=LFT,LLT
        DO I=1,PX
         DO J=1,PY
          SUMV=ZERO 
          DO K=1,PZ
           SUMV=SUMV+VGAUSS((J-1)*PZ+(I-1)*PZ*PY+K,ITEL)
          ENDDO
          VMIN(ITEL)=MIN(VMIN(ITEL),SUMV)
         ENDDO
        ENDDO

        DELTAX(ITEL)=MIN(DELTAX(ITEL),PX*PY*VMIN(ITEL)/MAX(AFACE(1,ITEL),AFACE(2,ITEL)))
        VMIN(ITEL)=EP10
        SUMV=ZERO

        DO I=1,PX
         DO J=1,PZ
          SUMV=ZERO
          DO K=1,PY
           SUMV=SUMV+VGAUSS(J+(I-1)*PY*PZ+(K-1)*PZ,ITEL)
          ENDDO
          VMIN(ITEL)=MIN(VMIN(ITEL),SUMV)
         ENDDO
        ENDDO

        DELTAX(ITEL)=MIN(DELTAX(ITEL),PX*PZ*VMIN(ITEL)/MAX(AFACE(3,ITEL),AFACE(4,ITEL)))
        VMIN(ITEL)=EP10

        DO I=1,PY
         DO J=1,PZ
          SUMV=ZERO
          DO K=1,PX
           SUMV=SUMV+VGAUSS(J+(I-1)*PZ+(K-1)*PY*PZ,ITEL)
          ENDDO
          VMIN(ITEL)=MIN(VMIN(ITEL),SUMV)
         ENDDO
        ENDDO

        DELTAX(ITEL)=MIN(DELTAX(ITEL),PZ*PY*VMIN(ITEL)/MAX(AFACE(5,ITEL),AFACE(6,ITEL)))
        VMIN(ITEL)=EP10
        SUMV=ZERO

       ENDDO

      DEALLOCATE(VGAUSS)
      ENDIF

C----------------------------

C-----------
      RETURN
      END
