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
Chd|  MULTIFLUID_INIT3              source/multifluid/multifluid_init3.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ATHERI                        source/ale/atheri.F           
Chd|        ATURI3                        source/ale/ale3d/aturi3.F     
Chd|        DTMAIN                        source/materials/time_step/dtmain.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        M5IN3                         source/initial_conditions/detonation/m5in3.F
Chd|        MATINI                        source/materials/mat_share/matini.F
Chd|        SCOOR3                        source/elements/solid/solide/scoor3.F
Chd|        SDERI3                        source/elements/solid/solide/sderi3.F
Chd|        SDLEN3                        source/elements/solid/solide/sdlen3.F
Chd|        SJAC_I                        source/elements/solid/solide/sderi3.F
Chd|        SMASS3                        source/elements/solid/solide/smass3.F
Chd|        SMORTH3                       source/elements/solid/solide/smorth3.F
Chd|        SRCOOR3                       source/elements/solid/solide/srcoor3.F
Chd|        SVEOK3                        source/elements/solid/solide/sveok3.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        DETONATORS_MOD                share/modules1/detonators_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE MULTIFLUID_INIT3(ELBUF_STR,MAS      ,IXS    ,PM      ,X       ,
     .     GEO      ,ALE_CONNECTIVITY   ,IPARG_GR,
     .     DTELEM    ,SIGI     ,NEL    ,SKEW    ,IGEO    ,
     .     STIFN     ,PARTSAV  ,V      ,IPARTS  ,MSS     ,
     .     IPART     ,SIGSP    ,NG     ,IPARG   ,
     .     NSIGI     ,MSNF     ,NVC    ,MSSF    ,IPM     ,
     .     IUSER     ,NSIGS    ,VOLNOD ,BVOLNOD ,VNS     ,
     .     BNS       ,IN       ,VR     ,INS     ,WMA     ,
     .     PTSOL     ,BUFMAT   ,MCP    ,MCPS    ,TEMP    ,
     .     XREFS     ,NPF      ,TF     ,MSSA    ,STRSGLOB,
     .     STRAGLOB  ,FAIL_INI ,SPBUF  ,KXSP    ,IPARTSP ,
     .     NOD2SP    ,SOL2SPH  ,IRST   ,ILOADP  ,FACLOAD,
     .     MULTI_FVM, ERROR_THROWN,DETONATORS)
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
      USE MESSAGE_MOD
      USE MULTI_FVM_MOD
      USE DETONATORS_MOD
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C     I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C     A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C     G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C     C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr01_c.inc"
#include      "scr03_c.inc"
#include      "scr12_c.inc"
#include      "scr17_c.inc"
#include      "scry_c.inc"
#include      "sphcom.inc"
#include      "units_c.inc"
#include      "vect01_c.inc"
#include      "warn_c.inc"
#include      "alefvm.inc"
C-----------------------------------------------
C     D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),IPARG(NPARG,NGROUP),
     .     IPARG_GR(NPARG),IPARTS(*),IGEO(NPROPGI,*),
     .     IPM(NPROPMI,*),IPART(LIPART1,*),PTSOL(*),
     .     NG,NDDIM, NSIGI ,NVC,NEL,IUSER, NSIGS, NPF(*),
     .     STRSGLOB(*),STRAGLOB(*),FAIL_INI(*), 
     .     KXSP(NISP,*), IPARTSP(*), NOD2SP(*), SOL2SPH(2,*), IRST(3,*)
      my_real
     .     MAS(*), PM(NPROPM,*), X(3,*), GEO(NPROPG,*),
     .     DTELEM(*),SIGI(NSIGS,*),SKEW(LSKEW,*),STIFN(*),
     .     PARTSAV(20,*), V(3, *), MSS(8,*), 
     .     SIGSP(NSIGI,*),MSNF(*), MSSF(8,*), WMA(*),
     .     VOLNOD(*), BVOLNOD(*), VNS(8,*), BNS(8,*),
     .     IN(*),VR(*), INS(8,*),BUFMAT(*),
     .     MCP(*), MCPS(8,*), TEMP(*),
     .     XREFS(8,3,*), TF(*), MSSA(*),
     .     SPBUF(NSPBUF,*)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
      INTEGER,INTENT(IN) :: ILOADP(SIZLOADP,*)
      my_real,INTENT(IN) :: FACLOAD(LFACLOAD,*)
      TYPE(MULTI_FVM_STRUCT) :: MULTI_FVM
      LOGICAL :: ERROR_THROWN
      TYPE(DETONATOR_STRUCT_) :: DETONATORS
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
C-----------------------------------------------
C     L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,J, NF1, NCC, IBID, JHBE, IREP,IGTYP, NUVAR,NUVARR,IDEF,
     .     IPT,LVLOC,IPID1,NPTR,NPTS,NPTT,NLAY,
     .     NSPHDIR, NCELF, NCELL,L_PLA
      INTEGER MAT(MVSIZ), PID(MVSIZ), NGL(MVSIZ),
     .     IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ),IX4(MVSIZ),
     .     IX5(MVSIZ),IX6(MVSIZ),IX7(MVSIZ),IX8(MVSIZ)
      my_real
     .     V8LOC(51,MVSIZ),VOLU(MVSIZ),DTX(MVSIZ),
     .     X1(MVSIZ),X2(MVSIZ),X3(MVSIZ),X4(MVSIZ),X5(MVSIZ),X6(MVSIZ),
     .     X7(MVSIZ),X8(MVSIZ),Y1(MVSIZ),Y2(MVSIZ),Y3(MVSIZ),Y4(MVSIZ),
     .     Y5(MVSIZ),Y6(MVSIZ),Y7(MVSIZ),Y8(MVSIZ),Z1(MVSIZ),Z2(MVSIZ),
     .     Z3(MVSIZ),Z4(MVSIZ),Z5(MVSIZ),Z6(MVSIZ),Z7(MVSIZ),Z8(MVSIZ),
     .     RX(MVSIZ) ,RY(MVSIZ) ,RZ(MVSIZ) ,SX(MVSIZ) ,
     .     SY(MVSIZ) ,SZ(MVSIZ) ,TX(MVSIZ) ,TY(MVSIZ) ,TZ(MVSIZ) ,
     .     E1X(MVSIZ),E1Y(MVSIZ),E1Z(MVSIZ),
     .     E2X(MVSIZ),E2Y(MVSIZ),E2Z(MVSIZ),
     .     E3X(MVSIZ),E3Y(MVSIZ),E3Z(MVSIZ),
     .     F1X(MVSIZ) ,F1Y(MVSIZ) ,F1Z(MVSIZ) ,
     .     F2X(MVSIZ) ,F2Y(MVSIZ) ,F2Z(MVSIZ),RHOCP(MVSIZ),TEMP0(MVSIZ),
     .     PX1(MVSIZ),PX2(MVSIZ),PX3(MVSIZ),PX4(MVSIZ),
     .     PY1(MVSIZ),PY2(MVSIZ),PY3(MVSIZ),PY4(MVSIZ),
     .     PZ1(MVSIZ),PZ2(MVSIZ),PZ3(MVSIZ),PZ4(MVSIZ),
     .     RHOF(MVSIZ),ALPHA(MVSIZ), DELTAX(MVSIZ), AIRE(MVSIZ), DUMMY, PRES, VFRAC
      my_real
     .     BID, FV, STI
      DOUBLE PRECISION
     .   XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ),
     .   XD5(MVSIZ), XD6(MVSIZ), XD7(MVSIZ), XD8(MVSIZ),
     .   YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ),
     .   YD5(MVSIZ), YD6(MVSIZ), YD7(MVSIZ), YD8(MVSIZ),
     .   ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ),
     .   ZD5(MVSIZ), ZD6(MVSIZ), ZD7(MVSIZ), ZD8(MVSIZ),VOLDP(MVSIZ)
      INTEGER :: ILAY, MATLAW
C-----------------------------------------------
      CHARACTER*nchartitle,
     .     TITR1
      PARAMETER (LVLOC = 51)
C-----------------------------------------------
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
      TYPE(BUF_MAT_) ,POINTER :: MBUF
      TYPE(BUF_LAY_) ,POINTER :: BUFLY
      my_real,
     .     DIMENSION(:), POINTER  :: UVARF   
      INTEGER :: NODE1, NODE2, NODE3, NODE4, NODE5, NODE6, NODE7, NODE8
C-----------------------------------------------
C     S o u r c e  L i n e s
C=======================================================================
C     Global buffer
      GBUF  => ELBUF_STR%GBUF
C     Number of layer
      NLAY  =  ELBUF_STR%NLAY
C     Current group starts at NF1
      NF1=NFT+1
C     ???
      JHBE  = IPARG_GR(23)
      IREP  = IPARG_GR(35)
      JCVT  = IPARG_GR(37)
      IGTYP = IPARG_GR(38)
      IF (JCVT==1.AND.ISORTH/=0) JCVT=2
      IDEF  = 0                 ! initialization flag for the total strain
      BID   = ZERO
      IBID = 0
      NPTR  =  ELBUF_STR%NPTR
      NPTS  =  ELBUF_STR%NPTS
      NPTT  =  ELBUF_STR%NPTT

      DO I=LFT,LLT
         RHOCP(I) =  PM(69,IXS(1,NFT+I))
         TEMP0(I) =  PM(79,IXS(1,NFT+I))
C     for air + foam
         RHOF(I) =  PM(192,IXS(1,NFT+I))
         ALPHA(I) = PM(193,IXS(1,NFT+I))
      ENDDO
C-----JAC_I [J]^-1 is calculated in global system
      IF (ISMSTR==10.OR.ISMSTR==12) THEN
         CALL SCOOR3(X,XREFS(1,1,NF1),IXS(1,NF1), GEO  ,MAT  ,PID  ,NGL  ,
     .        IX1  ,IX2  ,IX3  ,IX4  ,IX5  ,IX6  ,IX7  ,IX8  ,
     .        X1   ,X2   ,X3   ,X4   ,X5   ,X6   ,X7   ,X8   ,
     .        Y1   ,Y2   ,Y3   ,Y4   ,Y5   ,Y6   ,Y7   ,Y8   ,
     .        Z1   ,Z2   ,Z3   ,Z4   ,Z5   ,Z6   ,Z7   ,Z8   ,
     .        RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .        E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  ,
     .        F1X  ,F1Y  ,F1Z  ,F2X  ,F2Y  ,F2Z  ,TEMP0,TEMP ,
     .        XD1  ,XD2  ,XD3  ,XD4  ,XD5  ,XD6  ,XD7  ,XD8   ,
     .        YD1  ,YD2  ,YD3  ,YD4  ,YD5  ,YD6  ,YD7  ,YD8   ,
     .        ZD1  ,ZD2  ,ZD3  ,ZD4  ,ZD5  ,ZD6  ,ZD7  ,ZD8   )
         CALL SJAC_I(
     .        X1   ,X2   ,X3   ,X4   ,X5   ,X6   ,X7   ,X8   ,
     .        Y1   ,Y2   ,Y3   ,Y4   ,Y5   ,Y6   ,Y7   ,Y8   ,
     .        Z1   ,Z2   ,Z3   ,Z4   ,Z5   ,Z6   ,Z7   ,Z8   ,
     .        GBUF%JAC_I ,NEL)
      END IF
C Orthotropy wrt reference geometry
      IF (JCVT == 0) THEN
         CALL SCOOR3(X,XREFS(1,1,NF1),IXS(1,NF1),GEO  ,MAT  ,PID  ,NGL  ,
     .        IX1  ,IX2  ,IX3  ,IX4  ,IX5  ,IX6  ,IX7  ,IX8  ,
     .        X1   ,X2   ,X3   ,X4   ,X5   ,X6   ,X7   ,X8   ,
     .        Y1   ,Y2   ,Y3   ,Y4   ,Y5   ,Y6   ,Y7   ,Y8   ,
     .        Z1   ,Z2   ,Z3   ,Z4   ,Z5   ,Z6   ,Z7   ,Z8   ,
     .        RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .        E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  ,
     .        F1X  ,F1Y  ,F1Z  ,F2X  ,F2Y  ,F2Z  ,TEMP0,TEMP ,
     .        XD1  ,XD2  ,XD3  ,XD4  ,XD5  ,XD6  ,XD7  ,XD8   ,
     .        YD1  ,YD2  ,YD3  ,YD4  ,YD5  ,YD6  ,YD7  ,YD8   ,
     .        ZD1  ,ZD2  ,ZD3  ,ZD4  ,ZD5  ,ZD6  ,ZD7  ,ZD8   )
      ELSE
         CALL SRCOOR3(X,XREFS(1,1,NF1),IXS(1,NF1),GEO  ,MAT  ,PID  ,NGL  ,JHBE ,
     .        IX1  ,IX2  ,IX3  ,IX4  ,IX5  ,IX6  ,IX7  ,IX8  ,
     .        X1   ,X2   ,X3   ,X4   ,X5   ,X6   ,X7   ,X8   ,
     .        Y1   ,Y2   ,Y3   ,Y4   ,Y5   ,Y6   ,Y7   ,Y8   ,
     .        Z1   ,Z2   ,Z3   ,Z4   ,Z5   ,Z6   ,Z7   ,Z8   ,
     .        RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .        E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  ,
     .        F1X  ,F1Y  ,F1Z  ,F2X  ,F2Y  ,F2Z  ,TEMP0,TEMP ,
     .        XD1  ,XD2  ,XD3  ,XD4  ,XD5  ,XD6  ,XD7  ,XD8   ,
     .        YD1  ,YD2  ,YD3  ,YD4  ,YD5  ,YD6  ,YD7  ,YD8   ,
     .        ZD1  ,ZD2  ,ZD3  ,ZD4  ,ZD5  ,ZD6  ,ZD7  ,ZD8   )

      ENDIF
C     
C     Orthotropy
      IF (ISORTH == 1)
     .     CALL SMORTH3(PID  ,GEO  ,IGEO ,SKEW ,IREP ,GBUF%GAMA  , 
     .     RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .     E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  ,
     .     F1X  ,F1Y  ,F1Z  ,F2X  ,F2Y  ,F2Z  ,NSIGI,SIGSP,NSIGS,
     .     SIGI ,IXS  ,X    ,JHBE ,PTSOL,NEL  ,IPARG_GR(28))
C     
C-----------
      CALL SVEOK3(NVC,8, IX1, IX2, IX3, IX4, IX5, IX6, IX7, IX8)

      CALL SDERI3(
     .     GBUF%VOL ,DUMMY  ,GEO  ,IGEO ,
     .     XD1  ,XD2  ,XD3  ,XD4  ,XD5  ,XD6  ,XD7  ,XD8  ,
     .     YD1  ,YD2  ,YD3  ,YD4  ,YD5  ,YD6  ,YD7  ,YD8  ,
     .     ZD1  ,ZD2  ,ZD3  ,ZD4  ,ZD5  ,ZD6  ,ZD7  ,ZD8  ,
     .     RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,NGL  ,PID  ,
     .     PX1  ,PX2  ,PX3  ,PX4  ,PY1  ,PY2  ,PY3  ,PY4  ,
     .     PZ1  ,PZ2  ,PZ3  ,PZ4  ,VOLU ,VOLDP,NEL  ,JEUL ,
     .     NXREF,IMULTI_FVM )
      CALL SDLEN3(
     .     X1   ,X2   ,X3   ,X4   ,X5   ,X6   ,X7   ,X8   ,
     .     Y1   ,Y2   ,Y3   ,Y4   ,Y5   ,Y6   ,Y7   ,Y8   ,
     .     Z1   ,Z2   ,Z3   ,Z4   ,Z5   ,Z6   ,Z7   ,Z8, 
     .     DELTAX, VOLU)

      GBUF%RHO(:) = ZERO
      PM(104,IXS(1, 1 + NFT)) = ZERO  !global pressure
      
      DO ILAY = 1, NLAY
         LBUF  => ELBUF_STR%BUFLY(ILAY)%LBUF(1,1,1)
         MBUF  => ELBUF_STR%BUFLY(ILAY)%MAT(1,1,1)
         BUFLY => ELBUF_STR%BUFLY(ILAY)
         NUVAR =  ELBUF_STR%BUFLY(ILAY)%NVAR_MAT
         L_PLA =  ELBUF_STR%BUFLY(ILAY)%L_PLA
         DO I = LFT, LLT
            MAT(I) = IPM(20 + ILAY, IXS(1, I + NFT))
C     Fill partial volumes
            LBUF%VOL(I) = PM(20 + ILAY, IXS(1, I + NFT)) * GBUF%VOL(I)
         ENDDO

!     PT=1 => material initializations in matini are not fully compatible with Isolid=12 (lm)
         IPT=1
         CALL MATINI(PM ,IXS   ,NIXS      ,X         ,
     2        GEO      ,ALE_CONNECTIVITY  ,DETONATORS,IPARG_GR  ,
     3        SIGI     ,NEL    ,SKEW      ,IGEO      ,
     4        IPART    ,IPARTS ,
     5        MAT      ,IPM    ,NSIGS     ,NUMSOL    ,PTSOL  ,
     6        IPT      ,NGL    ,NPF       ,TF        ,BUFMAT ,
     7        GBUF     ,LBUF   ,MBUF      ,ELBUF_STR ,ILOADP ,
     8        FACLOAD, DELTAX)
     
         VFRAC = PM(20+ILAY,IXS(1, 1 + NFT))
         PRES  = PM(104, IPM(20+ILAY,IXS(1, 1 + NFT)))
         PM(104,IXS(1, 1 + NFT)) = PM(104,IXS(1, 1 + NFT)) + VFRAC * PRES !global pressure

         MATLAW = IPM(2, MAT(1))
         IF (MATLAW == 5) THEN
! JWL material
            IF (.NOT. ERROR_THROWN) THEN
               IF (PM(44, MAT(1)) == ZERO) THEN
                  CALL ANCMSG(MSGID = 1623, MSGTYPE = MSGERROR, ANMODE = ANINFO, 
     .                 I1 = IPM(1, IXS(1, 1 + NFT)), I2 = IPM(1, MAT(1)))
               ENDIF
               ERROR_THROWN = .TRUE.
            ENDIF
            CALL M5IN3(PM, MAT,  IPM(1, IXS(1,LFT+NFT)), DETONATORS, LBUF%TB, NGL, IPARG, X, IXS, NIXS)
         ENDIF
         IF (MATLAW == 6) THEN
            IF (PM(24, MAT(1)) > ZERO) THEN
               MULTI_FVM%NS_DIFF = .TRUE.
            ENDIF
         ENDIF

      ENDDO                     ! ILAY = 1, NLAY

      IF (NLAY > 1) THEN

C      Mass globalization                 
         DO ILAY = 1, NLAY
            LBUF  => ELBUF_STR%BUFLY(ILAY)%LBUF(1,1,1)
            DO I = LFT, LLT
               GBUF%RHO(I) = GBUF%RHO(I) + LBUF%RHO(I) * PM(20 + ILAY, IXS(1, I + NFT))
            ENDDO
         ENDDO

C      Temperature globalization. We must solve later T such as e+p/rho=integral(Cp_global(T),dT)
         GBUF%TEMP(LFT:LLT)=ZERO
         DO ILAY = 1, NLAY
            LBUF  => ELBUF_STR%BUFLY(ILAY)%LBUF(1,1,1)
            DO I = LFT, LLT
               GBUF%TEMP(I) = GBUF%TEMP(I) + LBUF%TEMP(I) * PM(20 + ILAY, IXS(1, I + NFT))*LBUF%RHO(I)/GBUF%RHO(I)   !volfrac*densfrac=massfrac
            ENDDO
         ENDDO           

      ENDIF
C----------------------------------------
C Thermal and turbulence initialization 
C----------------------------------------
      IF(JTHE /=0) CALL ATHERI(MAT,PM  ,GBUF%TEMP)
      IF(JTUR /=0) CALL ATURI3(IPARG   ,GBUF%RHO,PM,IXS,X,
     .     GBUF%RK ,GBUF%RE,VOLU)
C----------------------------------------
C Masses initialization
C----------------------------------------
      IF(JLAG+JALE+JEUL/=0) THEN
         CALL SMASS3(
     .        GBUF%RHO   ,MAS        ,PARTSAV    ,X          ,V        ,
     .        IPARTS(NF1),MSS(1,NF1) ,VOLU       ,
     .        MSNF       ,MSSF(1,NF1),IN         ,
     .        VR         ,INS(1,NF1) ,WMA        ,RHOCP      ,MCP      ,
     .        MCPS(1,NF1),MSSA       ,RHOF       ,ALPHA      ,GBUF%FILL, 
     .        IX1, IX2, IX3, IX4, IX5, IX6, IX7, IX8)
      ENDIF
C------------------------------------------
C Element time steps
C------------------------------------------
      AIRE(:) = ZERO
      CALL DTMAIN(GEO      , PM       , IPM        , PID    , MAT    , FV    ,
     .     GBUF%EINT, GBUF%TEMP, GBUF%DELTAX, GBUF%RK, GBUF%RE, BUFMAT, DELTAX, AIRE, VOLU, DTX, IGEO)
C     
      DO I=LFT,LLT
         IF(IXS(10,I+NFT)/=0) THEN
            IF(     IGTYP/=0 .AND.IGTYP/=6 .AND. IGTYP/=14
     .           .AND.IGTYP/=15.AND. IGTYP/=29) THEN
               IPID1=IXS(NIXS-1,I+NFT)
               CALL FRETITL2(TITR1,IGEO(NPROPGI-LTITR+1,IPID1),LTITR)
               CALL ANCMSG(MSGID=226,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=IGEO(1,IPID1),
     .              C1=TITR1,
     .              I2=IGTYP)
            ENDIF
         ENDIF
         DTELEM(NFT+I)=DTX(I)
C     
C     STI = 0.25 * RHO * VOL / (DT*DT)
         STI = FOURTH * GBUF%FILL(I) * GBUF%RHO(I) * VOLU(I) /
     .        MAX(EM20,DTX(I)*DTX(I))
         STIFN(IXS(2,I+NFT))=STIFN(IXS(2,I+NFT))+STI
         STIFN(IXS(3,I+NFT))=STIFN(IXS(3,I+NFT))+STI
         STIFN(IXS(4,I+NFT))=STIFN(IXS(4,I+NFT))+STI
         STIFN(IXS(5,I+NFT))=STIFN(IXS(5,I+NFT))+STI
         STIFN(IXS(6,I+NFT))=STIFN(IXS(6,I+NFT))+STI
         STIFN(IXS(7,I+NFT))=STIFN(IXS(7,I+NFT))+STI
         STIFN(IXS(8,I+NFT))=STIFN(IXS(8,I+NFT))+STI
         STIFN(IXS(9,I+NFT))=STIFN(IXS(9,I+NFT))+STI
      ENDDO
      RETURN
      END SUBROUTINE MULTIFLUID_INIT3
