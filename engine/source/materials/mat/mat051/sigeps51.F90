!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
!Copyright>
!Copyright>        This program is free software: you can redistribute it and/or modify
!Copyright>        it under the terms of the GNU Affero General Public License as published by
!Copyright>        the Free Software Foundation, either version 3 of the License, or
!Copyright>        (at your option) any later version.
!Copyright>
!Copyright>        This program is distributed in the hope that it will be useful,
!Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
!Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!Copyright>        GNU Affero General Public License for more details.
!Copyright>
!Copyright>        You should have received a copy of the GNU Affero General Public License
!Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
!Copyright>
!Copyright>
!Copyright>        Commercial Alternative: Altair Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
!Copyright>        software under a commercial license.  Contact Altair to discuss further if the
!Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
      !||====================================================================
      !||    sigeps51_mod   ../engine/source/materials/mat/mat051/sigeps51.F90
      !||--- called by ------------------------------------------------------
      !||    mulaw          ../engine/source/materials/mat_share/mulaw.F90
      !||====================================================================
      module sigeps51_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
      !||====================================================================
      !||    sigeps51                         ../engine/source/materials/mat/mat051/sigeps51.F90
      !||--- called by ------------------------------------------------------
      !||    mulaw                            ../engine/source/materials/mat_share/mulaw.F90
      !||--- calls      -----------------------------------------------------
      !||    compute_bfrac                    ../engine/source/materials/mat/mat051/compute_bfrac.F
      !||    dprag51                          ../engine/source/materials/mat/mat051/dprag51.F
      !||    finter                           ../engine/source/tools/curve/finter.F
      !||    jcook51                          ../engine/source/materials/mat/mat051/jcook51.F
      !||    jwl51                            ../engine/source/materials/mat/mat051/jwl51.F
      !||    jwlun51                          ../engine/source/materials/mat/mat051/jwl51.F
      !||    poly51                           ../engine/source/materials/mat/mat051/polynomial51.F
      !||    polyun51                         ../engine/source/materials/mat/mat051/polynomial51.F
      !||    sigeps51_boundary_material       ../engine/source/materials/mat/mat051/sigeps51_boundary_material.F90
      !||--- uses       -----------------------------------------------------
      !||    ale_connectivity_mod             ../common_source/modules/ale/ale_connectivity_mod.F
      !||    constant_mod                     ../common_source/modules/constant_mod.F
      !||    elbufdef_mod                     ../common_source/modules/mat_elem/elbufdef_mod.F90
      !||    i22bufbric_mod                   ../common_source/modules/interfaces/cut-cell-search_mod.F
      !||    i22tri_mod                       ../common_source/modules/interfaces/cut-cell-search_mod.F
      !||    multimat_param_mod               ../common_source/modules/multimat_param_mod.F90
      !||    prop_param_mod                   ../common_source/modules/mat_elem/prop_param_mod.F90
      !||    sigeps51_boundary_material_mod   ../engine/source/materials/mat/mat051/sigeps51_boundary_material.F90
      !||====================================================================
      subroutine sigeps51 ( &
           nel    ,nuparam     ,nuvar   ,nfunc   ,ifunc    , &
           npf    ,tf          ,time    ,timestep,uparam   ,numel  , &
           rho    ,volume       ,eint   ,vel_o   ,wfext    , &
           epspxx ,epspyy      ,epspzz  ,epspxy  ,epspyz   ,epspzx , &
           depsxx ,depsyy      ,depszz  ,depsxy  ,depsyz   ,depszx , &
           sigoxx ,sigoyy      ,sigozz  ,sigoxy  ,sigoyz   ,sigozx , &
           signxx ,signyy      ,signzz  ,signxy  ,signyz   ,signzx , &
           sigvxx ,sigvyy      ,sigvzz  ,sigvxy  ,sigvyz   ,sigvzx , &
           soundsp,viscmax     ,uvar    ,off     ,nft      ,v      , &
           w      ,x           ,ix      ,n48     ,nix      ,jthe   , &
           geo    ,pid         ,ilay    ,ng      ,elbuf_tab,pm     , &
           iparg  ,ale_connect ,bufvois ,ipm     ,bufmat   ,stifn  , &
           vd2    ,vdx         ,vdy     ,vdz     , &
           qvis   ,ddvol       ,qqold   ,nv46    ,numgeo   ,n2d    ,&
           numnod ,ngroup      ,nummat)
! ======================================================================================================================
!                                                   Modules
! ======================================================================================================================
      use elbufdef_mod  , only : elbuf_struct_, g_bufel_, l_bufel_, buf_lay_
      use i22bufbric_mod
      use i22tri_mod
      use ale_connectivity_mod , only : t_ale_connectivity
      use multimat_param_mod , only : m51_n0phas, m51_nvphas
      use sigeps51_boundary_material_mod , only : sigeps51_boundary_material
      use constant_mod , only : zero,one,em03,em12,em13,em14,ep10,ep20,em06,em10,em20,em4,half,third,three,two, three100
      use prop_param_mod , only : n_var_ipm, n_var_pm, n_var_geo, n_var_iparg
      use i22bufbric_mod , only : ninter22
!---------+---------+---+---+--------------------------------------------
! VAR     | SIZE    |TYP| RW| DEFINITION
!---------+---------+---+---+--------------------------------------------
! NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL
! NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
! NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
!---------+---------+---+---+--------------------------------------------
! NFUNC   |  1      | I | R | NUMBER FUNCTION USED FOR THIS USER LAW
! IFUNC   | NFUNC   | I | R | FUNCTION INDEX
! NPF     |  *      | I | R | FUNCTION ARRAY
! TF      |  *      | F | R | FUNCTION ARRAY
!---------+---------+---+---+--------------------------------------------
! TIME    |  1      | F | R | CURRENT TIME
! TIMESTEP|  1      | F | R | CURRENT TIME STEP
! UVAR    | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
! RHO     | NEL     | F | R | DENSITY
! VOLUME  | NEL     | F | R | VOLUME
! EINT    | NEL     | F | R | TOTAL INTERNAL ENERGY
! EPSPXX  | NEL     | F | R | STRAIN RATE XX
! EPSPYY  | NEL     | F | R | STRAIN RATE YY
! ...     |         |   |   |
! DEPSXX  | NEL     | F | R | STRAIN INCREMENT XX
! DEPSYY  | NEL     | F | R | STRAIN INCREMENT YY
! ...     |         |   |   |
! SIGOXX  | NEL     | F | R | OLD ELASTO PLASTIC STRESS XX
! SIGOYY  | NEL     | F | R | OLD ELASTO PLASTIC STRESS YY
! ...     |         |   |   |
!---------+---------+---+---+--------------------------------------------
! SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
! SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
! ...     |         |   |   |
! SIGVXX  | NEL     | F | W | VISCOUS STRESS XX
! SIGVYY  | NEL     | F | W | VISCOUS STRESS YY
! ...     |         |   |   |
! SOUNDSP | NEL     | F | W | SOUND SPEED (NEEDED FOR TIME STEP)
! VISCMAX | NEL     | F | W | MAXIMUM DAMPING MODULUS(NEEDED FOR TIME STEP)
!---------+---------+---+---+--------------------------------------------
! UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY (see below)
! OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
!---------+---------+---+---+--------------------------------------------
! phase 0  dim=M51_N0PHAS
!          UVAR(1) = TEMP  !post-treatment only
!          UVAR(2) = PLAS  !post-treatment only
!          UVAR(3) = BFRAC !post-treatment only
! phase k  dim=M51_NVPHAS
!          IAD = M51_N0PHAS+(k-1)*M51_NVPHAS
!          UVAR(1 + IAD) = AVk
!          UVAR(2 + IAD) = SIGxk
!          UVAR(3 + IAD) = SIGyk
!          UVAR(4 + IAD) = SIGzk
!          UVAR(5 + IAD) = SIGxyk
!          UVAR(6 + IAD) = SIGyzk
!          UVAR(7 + IAD) = SIGzxk
!          UVAR(8 + IAD) = EINTk              ! energie IN rho e OUT
!          UVAR(9 + IAD) = RHOk0 * VOLUMk     ! masse IN rho OUT
!          UVAR(10+ IAD) = Qk
!          UVAR(11+ IAD) = VOLUMEk
!          UVAR(12+ IAD) = RHO
!          UVAR(13+ IAD) = DDVOL
!          UVAR(14+ IAD) = SSP
!          UVAR(15+ IAD) = PLAS
!          UVAR(16+ IAD) = Temp
!          UVAR(17+ IAD) = Edif/V
!          UVAR(18+ IAD) = P
!          UVAR(19+ IAD) = EPSEQ_k
!          UVAR(20+ IAD) = rho0 ( /= rho(t=0) in UVAR(12) ) due to inigrav
!          UVAR(21+ IAD) = E0 = rho0.e0 (/= rho.e(t) at t=0 in UVAR 8) due to inigrav
!          UVAR(22+ IAD) = SSP0
!          UVAR(23+ IAD) = AV0
! ======================================================================================================================
!                                                   Implicit none
! ======================================================================================================================
       implicit none
! ======================================================================================================================
!                                                   Included Files
! ======================================================================================================================
#include "my_real.inc"
! ======================================================================================================================
!                                                   Arguments
! ======================================================================================================================
      double precision,intent(inout) :: wfext
      integer,intent(in) :: nummat,numnod,ngroup !< array size
      integer,intent(in) :: numgeo !< array size
      integer,intent(in) :: n2d !< flag for 2d / 3d analysis
      integer,intent(in) :: nel, nuparam, nuvar,nft,n48,nix,jthe,numel, &
                            ix(nix,numel), pid(nel), ilay, ng,iparg(n_var_iparg,ngroup), &
                            ipm(n_var_ipm,nummat),nv46
      my_real,intent(in) :: time,timestep,uparam(nuparam),pm(n_var_pm,nummat), &
                            volume(nel),bufvois(*),ddvol(nel),qqold(nel), &
                            epspxx(nel),epspyy(nel),epspzz(nel), &
                            epspxy(nel),epspyz(nel),epspzx(nel), &
                            depsxx(nel),depsyy(nel),depszz(nel), &
                            depsxy(nel),depsyz(nel),depszx(nel), &
                            w(3,numnod),x(3,numnod),geo(n_var_geo,numgeo), bufmat(*), &
                            vd2(nel)

      my_real,intent(inout) :: sigoxx(nel),sigoyy(nel),sigozz(nel),sigoxy(nel),sigoyz(nel),sigozx(nel)
      my_real,intent(inout) :: rho(nel),eint(nel), soundsp(nel)

      type (elbuf_struct_), target, dimension(ngroup) :: elbuf_tab
      type(t_ale_connectivity), intent(in) :: ale_connect
      my_real,intent(inout) :: &
          signxx(nel),signyy(nel),signzz(nel), &
          signxy(nel),signyz(nel),signzx(nel), &
          sigvxx(nel),sigvyy(nel),sigvzz(nel), &
          sigvxy(nel),sigvyz(nel),sigvzx(nel), &
          viscmax(nel)
      my_real,intent(inout) :: uvar(nel,nuvar),qvis(nel),stifn(nel),vdx(nel),vdy(nel),vdz(nel),v(3,numnod)
      my_real,intent(in) :: off(nel)
! ======================================================================================================================
!                                                   External
! ======================================================================================================================
      integer npf(*), nfunc, ifunc(nfunc)
      my_real finter ,tf(*)
      external finter
!        Y = FINTER(IFUNC(J),X,NPF,TF,DYDX)
!        Y       : y = f(x)
!        X       : x
!        DYDX    : f'(x) = dy/dx
!        IFUNC(J): FUNCTION INDEX
!              J : FIRST(J=1), SECOND(J=2) .. FUNCTION USED FOR THIS LAW
!        NPF,TF  : FUNCTION PARAMETER
! ======================================================================================================================
!                                                   Local Variables
! ======================================================================================================================
      INTEGER, PARAMETER :: IDBG = 0

      INTEGER SUBMAT_CODE
      INTEGER I,J,K,KK,ITER,NITER
      my_real P,PEXT,WFEXTT, &
              GG1,GG2,GG3, &
              C11,C12,C13,C21,C22,C23,C31,C32,C33,C41,C42,C43,C51,C52,C53, &
              AV1(nel),AV2(nel),AV3(nel),AV4(nel),RHO10,RHO20,RHO30,RHO40,RHO1,RHO2,RHO3,RHO4, &
              RHO0E1,RHO0E2,RHO0E3,RHO1OLD,RHO2OLD,RHO3OLD,RHO4OLD, &
              MAS1,MAS2,MAS3,MAS4,EINT1,EINT2,EINT3,EINT4, &
              E1_INF, E2_INF, E3_INF, E4_INF, &
              DPDV1,DPDV2,DPDV3,DPDV4, &
              P1,P2,P3,P4,P1I,P2I,P3I,P4I,VQ0,VQ1,VQ2,VQ3,VQ4, &
              Q0, Q1,Q2,Q3,Q4,Q1OLD,Q2OLD,Q3OLD,Q4OLD,SSP1,SSP2,SSP3,SSP4, &
              MU1,MU2,MU3,MU4, &
              MU1P1, MU2P1, MU3P1, MU4P1, &
              DVDP1,DVDP2,DVDP3,DVDP4,DDVOL1,DDVOL2,DDVOL3,DDVOL4, &
              V10,V20,V30,V40,V1,V2,V3,V4,V1OLD,V2OLD,V3OLD,V4OLD, &
              V1I,V2I,V3I,V4I,E01,E02,E03,E04,PM1,PM2,PM3,PM4, &
              ECOLD1,ECOLD2,ECOLD3,ECOLD4,SPH1,SPH2,SPH3,SPH4, &
              T10,T20,T30,T40,H1,H2,H3,H4,TEMP1,TEMP2,TEMP3,TEMP4, &
              XL,QAL,QBL,Q,POLD,QOLD,MASS, &
              VISA1,VISB1,AA,DD,QA,QB,UNDT, &
              VOLD,DVOL, &
              C01,C02,C03,C04,EDIF1,EDIF2,EDIF3,EDIF4, &
              DE(NEL), &
              AAA, &
              TBURN,ECOLD,T
      my_real DEPS(6,nel),EPD(nel), &
              P1OLD(nel),P2OLD(nel),P3OLD(nel),P4OLD(nel), &
              SIGD(6,nel), EINT0(nel),PLAS1(nel),PLAS2(nel),PLAS3(nel), &
              VOL(nel), TEMP(nel), &
              EPSEQ1(nel), EPSEQ2(nel), EPSEQ3(nel), &
              PM5, BFRAC(nel), &
              VEL_O(nel), &
              alpha, PP(nel), dbVOLD(4), dbVOLD_f(4), Tol51, &
              XL1,XL2,XL3,XL4,QAL1,QAL2,QAL3,QAL4,QBL1,QBL2,QBL3,QBL4,DD2, &
              TOL, TOL2, ERROR, ERROR2, COEF1, COEF2, COEF3, COEF4, &
              STEP, DV1, DV2, DV3, DV4, &
              EINT1_INI, EINT2_INI, EINT3_INI, EINT4_INI, &
              ALPHA1OLD, ALPHA2OLD, ALPHA3OLD, ALPHA4OLD, &
              RHOOLD, SSP1_INI, SSP2_INI, SSP3_INI, SSP4_INI, VFRAC(nel)
      my_real :: VISC1, VISC2, VISC3,VISC4
      INTEGER :: CONT
      INTEGER IFLG,IEXP, &
              IOPT, IPLA, IPLA1, IPLA2, IPLA3,&
              K1,K2,K3,K4,ML,IFORM
      INTEGER :: IX1,IX2,IX3,IX4
      INTEGER IBUG_ID(2)
      TYPE(G_BUFEL_)  ,POINTER :: GBUF
      TYPE(L_BUFEL_)  ,POINTER :: LBUF
      TYPE(BUF_LAY_)  ,POINTER :: BUFLY

! ======================================================================================================================
!                                                   Body
! ======================================================================================================================
      IFORM = HUGE(IFORM)
      IX1 = HUGE(IX1) 
      IX2 = HUGE(IX2)
      IX3 = HUGE(IX3)
      IX4 = HUGE(IX4)
      ML = HUGE(ML)
      IF(TIMESTEP > ZERO)THEN
        UNDT = ONE/TIMESTEP
      ELSE
        UNDT = ZERO
      ENDIF

      IBUG_ID(1:2) = (/0, 0/)

      TOL51 = EM10

      WFEXTT = ZERO
      MU1P1 = ONE
      MU2P1 = ONE
      MU3P1 = ONE
      MU4P1 = ONE

      GBUF  => ELBUF_TAB(NG)%GBUF
      LBUF  => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(1,1,1)
      BUFLY => ELBUF_TAB(NG)%BUFLY(ILAY)

      IFLG = NINT(UPARAM(31))

      SELECT CASE (IFLG)

      CASE (2,3,4,5,6)
        ! obsolete boundary material formulation
        CALL SIGEPS51_BOUNDARY_MATERIAL( &
            nel    ,nuparam     ,nuvar    ,nfunc   ,ifunc    ,&
            npf    ,tf          ,time     ,timestep,uparam   ,numel  ,&
            rho    ,volume       ,eint    ,vel_o   ,WFEXT    ,&
            epspxx ,epspyy      ,epspzz   ,&
            sigoxx ,sigoyy      ,sigozz   ,sigoxy  ,sigoyz   ,sigozx ,&
            signxx ,signyy      ,signzz   ,signxy  ,signyz   ,signzx ,&
            soundsp,viscmax     ,uvar     ,nft     ,v        ,&
            w      ,x           ,ix       ,n48     ,nix      ,&
            ilay   ,ng          ,elbuf_tab,pm      ,&
            iparg  ,ale_connect ,bufvois  ,ipm     ,bufmat   ,stifn  ,&
            vd2    ,vdx         ,vdy      ,vdz     ,&
            nv46   ,n2d         ,numnod   ,ngroup  ,nummat)
        RETURN

      CASE DEFAULT
        ! normal material formulation
        CONTINUE
      END SELECT



      !===========================================!
      ! Reading Material Flags Through UPARAM()   !
      !===========================================!
      VISA1  = UPARAM(1)  !1st global Viscosity coefficient : mu
      VISB1  = UPARAM(3)  !2nd vglobal iscosity coefficient : lambda    !VISB1 =(VISV1-2.VISA1)/3.

      VISC1 = UPARAM(81) !law 6 parameter when law51 is defined from law6 (iform=12)
      VISC2 = UPARAM(82)
      VISC3 = UPARAM(83)
      VISC4 = UPARAM(84)

      !INITIAL volume fraction (in case of inivol, it was updated in Starter)
      K=M51_N0PHAS
      DO I=1,NEL
        AV1(I)=UVAR(I,K+23)
      ENDDO
      K=M51_N0PHAS+M51_NVPHAS
      DO I=1,NEL
        AV2(I)=UVAR(I,K+23)
      ENDDO
      K=M51_N0PHAS+2*M51_NVPHAS
      DO I=1,NEL
        AV3(I)=UVAR(I,K+23)
      ENDDO
       K=M51_N0PHAS+3*M51_NVPHAS
      DO I=1,NEL
        AV4(I)=UVAR(I,K+23)
      ENDDO

      PEXT   = UPARAM(8)

      RHO10  = UPARAM(9)
      RHO20  = UPARAM(10)
      RHO30  = UPARAM(11)
      RHO40  = UPARAM(47)


      !This avoid to divide by zero for empty phase
      IF(RHO10 == ZERO)RHO10=EM20
      IF(RHO20 == ZERO)RHO20=EM20
      IF(RHO30 == ZERO)RHO30=EM20
      IF(RHO40 == ZERO)RHO40=EM20

      RHO1   = RHO10
      RHO2   = RHO20
      RHO3   = RHO30
      RHO4   = RHO40

      C11    = UPARAM(12)
      C12    = UPARAM(13)
      C13    = UPARAM(14)

      C21    = UPARAM(15)
      C22    = UPARAM(16)
      C23    = UPARAM(17)

      C31    = UPARAM(18)
      C32    = UPARAM(20)
      C33    = UPARAM(21)

      C41    = UPARAM(22)
      C42    = UPARAM(23)
      C43    = UPARAM(24)

      C51    = UPARAM(25)
      C52    = UPARAM(26)
      C53    = UPARAM(27)

      GG1    = UPARAM(28)
      GG2    = UPARAM(29)
      GG3    = UPARAM(30)

      IOPT   = NINT(UPARAM(61))
      IEXP   = NINT(UPARAM(55))
      ALPHA  = UPARAM(62)

      IPLA   = NINT(UPARAM(63))
      IPLA1  = NINT(UPARAM(64))
      IPLA2  = NINT(UPARAM(65))
      IPLA3  = NINT(UPARAM(66))

      E01    = UPARAM(32)
      E02    = UPARAM(33)
      E03    = UPARAM(34)
      E04    = UPARAM(48)

      C01    = UPARAM(35)
      C02    = UPARAM(36)
      C03    = UPARAM(37)
      C04    = UPARAM(49)

      PM1    = UPARAM(39)
      PM2    = UPARAM(40)
      PM3    = UPARAM(41)
      PM4    = UPARAM(56)

      E1_INF = UPARAM(57) !E1_INF (t=0) Eint1_INF (t>0)
      E2_INF = UPARAM(58) !E2_INF (t=0) Eint2_INF (t>0)
      E3_INF = UPARAM(59) !E3_INF (t=0) Eint3_INF (t>0)
      E4_INF = UPARAM(60) !E4_INF (t=0) Eint4_INF (t>0)

      SPH1   = UPARAM(112)
      SPH2   = UPARAM(162)
      SPH3   = UPARAM(212)
      SPH4   = UPARAM(262)

      T10    = UPARAM(113)
      T20    = UPARAM(163)
      T30    = UPARAM(213)
      T40    = UPARAM(263)

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC!
!===========================================================================!
!     LAW51 : MULTI MATERIAL LAW                                            !
!     (IFLG == 0.or.IFLG == 1)                                              !
!     (IEXP == 0.or.IEXP == 1)                                              !
!===========================================================================!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC!

      IF (TIME  ==  ZERO) THEN
         DO I = 1, NEL
            KK = M51_N0PHAS + 0 * M51_NVPHAS
            V1OLD = AV1(I) * VOLUME(I)
            UVAR(I, 11 + KK) = V1OLD
            UVAR(I, 9 + KK) = V1OLD * UVAR(I, 20 + KK)
            UVAR(I, 8 + KK) = V1OLD * UVAR(I, 21 + KK)

            KK = M51_N0PHAS + 1 * M51_NVPHAS
            V2OLD = AV2(I) * VOLUME(I)
            UVAR(I, 11 + KK) = V2OLD
            UVAR(I, 9 + KK) = V2OLD * UVAR(I, 20 + KK)
            UVAR(I, 8 + KK) = V2OLD * UVAR(I, 21 + KK)

            KK = M51_N0PHAS + 2 * M51_NVPHAS
            V3OLD = AV3(I) * VOLUME(I)
            UVAR(I, 11 + KK) = V3OLD
            UVAR(I, 9 + KK) = V3OLD * UVAR(I, 20 + KK)
            UVAR(I, 8 + KK) = V3OLD * UVAR(I, 21 + KK)

            KK = M51_N0PHAS + 3 * M51_NVPHAS
            V4OLD = AV4(I) * VOLUME(I)
            UVAR(I, 11 + KK) = V4OLD
            UVAR(I, 9 + KK) = V4OLD * UVAR(I, 20 + KK)
            UVAR(I, 8 + KK) = V4OLD * UVAR(I, 21 + KK)
         ENDDO
      ENDIF

      !=======================================================================
      !     TEMPERATURES
      !     T > 0
      !=======================================================================
      DO I=1,NEL
           !---submaterial_1---!
           KK = M51_N0PHAS + 0*M51_NVPHAS
           EINT1   = UVAR(I,8 +KK)
           V1OLD   = UVAR(I,11+KK)
           RHO1OLD = UVAR(I,12+KK)
           ECOLD = -T10 * SPH1
           MU1 = (RHO1OLD/RHO10 - ONE)
           H1 = SPH1*V1OLD*(ONE+MU1)
           IF(MU1 > ZERO) ECOLD = ECOLD * (ONE+C51*MU1*(ONE-MU1)) + HALF*C21*MU1*MU1
           ECOLD1 = ECOLD*V1OLD*(ONE+MU1)
           TEMP1 = (EINT1 - ECOLD1)/MAX(EM20,H1)
           UVAR(I,16+KK) = TEMP1
           P1OLD(I) = UVAR(I,18+KK)

           !---submaterial_2---!
           KK = M51_N0PHAS + 1*M51_NVPHAS
           EINT2   = UVAR(I,8 +KK)
           V2OLD   = UVAR(I,11+KK)
           RHO2OLD = UVAR(I,12+KK)
           ECOLD = -T20 * SPH2
           MU2 = (RHO2OLD/RHO20 - ONE)
           H2 = SPH2*V2OLD*(ONE+MU2)
           IF(MU2 > ZERO) ECOLD = ECOLD * (ONE+C52*MU2*(ONE-MU2)) + HALF*C22*MU2*MU2
           ECOLD2 = ECOLD*V2OLD*(ONE+MU2)
           TEMP2 = (EINT2 - ECOLD2)/MAX(EM20,H2)
           UVAR(I,16+KK) = TEMP2
           P2OLD(I) = UVAR(I,18+KK)

           !---submaterial_3---!
           KK = M51_N0PHAS + 2*M51_NVPHAS
           EINT3   = UVAR(I,8 +KK)
           V3OLD   = UVAR(I,11+KK)
           RHO3OLD = UVAR(I,12+KK)
           ECOLD = -T30 * SPH3
           MU3 = (RHO3OLD/RHO30 - ONE)
           H3 = SPH3*V3OLD*(ONE+MU3)
           IF(MU3 > ZERO) ECOLD = ECOLD * (ONE+C53*MU3*(ONE-MU3)) + HALF*C23*MU3*MU3
           ECOLD3 = ECOLD*V3OLD*(ONE+MU3)
           TEMP3 = (EINT3 - ECOLD3)/MAX(EM20,H3)
           UVAR(I,16+KK) = TEMP3
           P3OLD(I) = UVAR(I,18+KK)

           !---submaterial_4---!
           IF(IEXP == 1)THEN
             KK = M51_N0PHAS + 3*M51_NVPHAS
             EINT4   = UVAR(I,8 +KK)
             V4OLD   = UVAR(I,11+KK)
             RHO4OLD = UVAR(I,12+KK)
             ECOLD = -T40 * SPH4
             MU4 = (RHO4OLD/RHO40 - ONE)
             H4 = SPH4*V4OLD*(ONE+MU4)
             ECOLD4 = ECOLD*V4OLD*(ONE+MU4)
             TEMP4 = (EINT4 - ECOLD4)/MAX(EM20,H4)
             P4OLD(I) = UVAR(I,18+KK)
           ELSE
             V4OLD    = ZERO
             EINT4    = ZERO
             RHO4OLD  = EM20
             MU4      = ZERO
             TEMP4    = ZERO
             P4OLD(I) = ZERO
             ECOLD4   = ZERO
             H4       = ZERO
           ENDIF
      ENDDO

      !=======================================================================
      !     PLASTICITY
      !     T > 0
      !=======================================================================
      ! If at least, one material has plasticity data
      IF(IPLA > ZERO)THEN
        DO I=1,NEL
          !inter22
          IF(VOLUME(I) == ZERO)CYCLE
          !
          DE(I) = THIRD*(DEPSXX(I)+DEPSYY(I)+DEPSZZ(I))
          DEPS(1,I) = DEPSXX(I) !- DE
          DEPS(2,I) = DEPSYY(I) !- DE
          DEPS(3,I) = DEPSZZ(I) !- DE
          DEPS(4,I) = DEPSXY(I)
          DEPS(5,I) = DEPSYZ(I)
          DEPS(6,I) = DEPSZX(I)
          EPD(I)=OFF(I)*MAX(   ABS(EPSPXX(I)),   ABS(EPSPYY(I)),   ABS(EPSPZZ(I)), &
               HALF*ABS(EPSPXY(I)),HALF*ABS(EPSPYZ(I)),HALF*ABS(EPSPZX(I)))
        ENDDO
        IF(BUFLY%L_EPSD > 0)THEN
          DO I=1,NEL
           LBUF%EPSD(I) = EPD(I)
          ENDDO
        ENDIF
      ENDIF

      !===================================
      !       material 1 : PLASTICITY
      !===================================
      IF(IPLA1 /= ZERO)THEN
        KK =  M51_N0PHAS
        DO I=1,NEL
          !inter22
          IF(VOLUME(I) == ZERO)CYCLE
           DO J=1,6
             SIGD(J,I)  = UVAR(I,KK + J + 1)
           ENDDO

           VFRAC(I)     = UVAR(I,1+KK)
           PP(I)        = UVAR(I,18+KK)
           PLAS1(I)     = UVAR(I,15+KK)
           TEMP(I)      = UVAR(I,16+KK)
           VOL(I)       = UVAR(I,11+KK)
           EINT0(I)     = UVAR(I,8 +KK)
           EPSEQ1(I)    = UVAR(I,19+KK)   ! Dprag

        ENDDO
        SELECT CASE (IPLA1)
           CASE (1)
             CALL JCOOK51 (NEL  ,SIGD     ,PLAS1       ,TEMP   ,VOL  , &
                           DEPS ,EPD      ,UPARAM(101) ,VOLUME ,EINT0, &
                           DE   ,TIMESTEP ,OFF         , &
                           VFRAC)
           CASE (2)
              CALL DPRAG51 &
                  (NEL    ,SIGD       ,VOL        ,EPSEQ1  , &
                   DEPS   ,UPARAM(101),VOLUME     ,EINT0   , PLAS1, &
                   UVAR   ,NUVAR      ,KK         ,RHO10   , &
                   C01    ,C11        ,C21        ,C31     ,PM1   ,PP    , &
                   OFF    ,PEXT       ,TIMESTEP   ,DE)
        END SELECT
        DO I=1,NEL

          !inter22
          IF(VOLUME(I) == ZERO)CYCLE

             UVAR(I,KK + 1 + 1)= SIGD(1,I)
             UVAR(I,KK + 2 + 1)= SIGD(2,I)
             UVAR(I,KK + 3 + 1)= SIGD(3,I)
             UVAR(I,KK + 4 + 1)= SIGD(4,I)
             UVAR(I,KK + 5 + 1)= SIGD(5,I)
             UVAR(I,KK + 6 + 1)= SIGD(6,I)

             UVAR(I,15+KK) = PLAS1(I)
             UVAR(I,16+KK) = TEMP(I)
             UVAR(I,11+KK) = VOL(I)
             UVAR(I,8 +KK) = EINT0(I)
             UVAR(I,19+KK) = EPSEQ1(I)   ! Dprag

             !total stress is set before  returning from sigeps51.F

        ENDDO
      ENDIF

      !===================================
      !        material 2 : PLASTICITY
      !===================================
      IF(IPLA2 /= ZERO)THEN
        KK = M51_N0PHAS + M51_NVPHAS
        DO I=1,NEL
          !inter22
          IF(VOLUME(I) == ZERO)CYCLE
          DO J=1,6
            SIGD(J,I)   = UVAR(I,KK + J + 1)
          ENDDO
           VFRAC(I)     = UVAR(I,1+KK)
           PP(I)        = UVAR(I,18+KK)
           PLAS2(I)     = UVAR(I,15+KK)
           TEMP(I)      = UVAR(I,16+KK)
           VOL(I)       = UVAR(I,11+KK)
           EINT0(I)     = UVAR(I,8 +KK)
           EPSEQ2(I)    = UVAR(I,19+KK)   ! Dprag
        ENDDO
        SELECT CASE (IPLA2)
           CASE (1)
             CALL JCOOK51 (NEL  ,SIGD     ,PLAS2       ,TEMP   ,VOL  , &
                           DEPS ,EPD      ,UPARAM(151) ,VOLUME ,EINT0, &
                           DE   ,TIMESTEP ,OFF         , &
                           VFRAC)
           CASE (2)
             CALL DPRAG51 &
                  (NEL    ,SIGD       ,VOL        ,EPSEQ2  , &
                   DEPS   ,UPARAM(151),VOLUME     ,EINT0   , PLAS2, &
                   UVAR   ,NUVAR      ,KK         ,RHO20   , &
                   C02    ,C12        ,C22        ,C32     ,PM2     ,PP    , &
                   OFF    ,PEXT       ,TIMESTEP   ,DE)
        END SELECT
        DO I=1,NEL
         !inter22
         IF(VOLUME(I) == ZERO)CYCLE

           UVAR(I,KK + 1 + 1) = SIGD(1,I)
           UVAR(I,KK + 2 + 1) = SIGD(2,I)
           UVAR(I,KK + 3 + 1) = SIGD(3,I)
           UVAR(I,KK + 4 + 1) = SIGD(4,I)
           UVAR(I,KK + 5 + 1) = SIGD(5,I)
           UVAR(I,KK + 6 + 1) = SIGD(6,I)

           UVAR(I,15+KK)      = PLAS2(I)
           UVAR(I,16+KK)      = TEMP(I)
           UVAR(I,11+KK)      = VOL(I)
           UVAR(I,8+KK)       = EINT0(I)
           UVAR(I,19+KK)      = EPSEQ2(I)   ! Dprag

        ENDDO
      ENDIF

      !===================================
      !        material 3 : PLASTICITY
      !===================================
      IF(IPLA3 /= ZERO)THEN
        KK = M51_N0PHAS + 2*M51_NVPHAS
        DO I=1,NEL
          !inter22
          IF(VOLUME(I) == ZERO)CYCLE
           DO J=1,6
             SIGD(J,I)  = UVAR(I,KK + J + 1)
           ENDDO
           VFRAC(I)     = UVAR(I,1+KK)
           PP(I)        = UVAR(I,18+KK)
           PLAS3(I)     = UVAR(I,15+KK)
           TEMP(I)      = UVAR(I,16+KK)
           VOL(I)       = UVAR(I,11+KK)
           EINT0(I)     = UVAR(I,8 +KK)
           EPSEQ3(I)    = UVAR(I,19+KK)   ! Dprag
        ENDDO
        SELECT CASE (IPLA3)
           CASE (1)
             CALL JCOOK51 (NEL  ,SIGD    ,PLAS3       ,TEMP   ,VOL  , &
                           DEPS ,EPD     ,UPARAM(201) ,VOLUME ,EINT0, &
                           DE   ,TIMESTEP,OFF         , &
                           VFRAC)
           CASE (2)
             CALL DPRAG51 &
                  (NEL    ,SIGD        ,VOL        ,EPSEQ3  , &
                   DEPS   ,UPARAM(201) ,VOLUME     ,EINT0   , PLAS3 , &
                   UVAR   ,NUVAR       ,KK         ,RHO30   , &
                   C03    ,C13         ,C23        ,C33     ,PM3   ,PP    , &
                   OFF    ,PEXT        ,TIMESTEP   ,DE)
        END SELECT
        DO I=1,NEL
         !inter22
         IF(VOLUME(I) == ZERO)CYCLE

            UVAR(I,KK + 1 + 1)= SIGD(1,I)
            UVAR(I,KK + 2 + 1)= SIGD(2,I)
            UVAR(I,KK + 3 + 1)= SIGD(3,I)
            UVAR(I,KK + 4 + 1)= SIGD(4,I)
            UVAR(I,KK + 5 + 1)= SIGD(5,I)
            UVAR(I,KK + 6 + 1)= SIGD(6,I)

            UVAR(I,15+KK) = PLAS3(I)
            UVAR(I,16+KK) = TEMP(I)
            UVAR(I,11+KK) = VOL(I)
            UVAR(I,8+KK)  = EINT0(I)
            UVAR(I,19+KK) = EPSEQ3(I)   ! Dprag
            !total stress is set before  returning from sigeps51.F

        ENDDO
      ENDIF



      !=======================================================================
      !     THERMODYNAMICAL STATE
      !=======================================================================
      DO I=1,NEL

          !inter22
          IF(VOLUME(I) == ZERO)CYCLE

        !===================================
        ! material 1 : THERMODYNAMICAL STATE
        !===================================
        KK = M51_N0PHAS
        EINT1   = UVAR(I,8+KK)    ! energie IN rho e OUT
        MAS1    = UVAR(I,9+KK)    ! masse IN rho OUT
        Q1OLD   = UVAR(I,10+KK)
        V1OLD   = UVAR(I,11+KK)
        RHO1OLD = UVAR(I,12+KK)   ! rho_old IN rho OUT
        DDVOL1  = UVAR(I,13+KK)
        SSP1    = UVAR(I,14+KK)   ! sound speed
        PLAS1(I)= UVAR(I,15+KK)   ! Eps plastique
        TEMP1   = UVAR(I,16+KK)   ! temperature
        EDIF1   = UVAR(I,17+KK)   ! chaleur diffusee
        P1OLD(I)= UVAR(I,18+KK)   ! Pressure
!        EPSEQ1(I)= UVAR(I,19+KK)   ! Dprag
        ECOLD = -T10 * SPH1
        MU1 = (RHO1OLD/RHO10 - ONE)
        H1 = SPH1*V1OLD*(ONE+MU1)
          IF(MU1 > ZERO) ECOLD = ECOLD * (ONE+C51*MU1*(ONE-MU1)) + HALF*C21*MU1*MU1
        ECOLD1 = ECOLD*V1OLD*(ONE+MU1)
        TEMP1 = (EINT1 - ECOLD1)/MAX(EM20,H1)


        !===================================
        ! material 2 : THERMODYNAMICAL STATE
        !===================================
        KK = M51_N0PHAS + M51_NVPHAS
        EINT2   = UVAR(I,8+KK)    ! energie IN rho e OUT
        MAS2    = UVAR(I,9+KK)    ! masse IN rho OUT
        Q2OLD   = UVAR(I,10+KK)
        V2OLD   = UVAR(I,11+KK)
        RHO2OLD = UVAR(I,12+KK)   ! rho_old IN rho OUT
        DDVOL2  = UVAR(I,13+KK)
        SSP2    = UVAR(I,14+KK)   ! sound speed
        PLAS2(I)= UVAR(I,15+KK)   ! Eps plastique
        TEMP2   = UVAR(I,16+KK)   ! temperature
        EDIF2   = UVAR(I,17+KK)   ! chaleur diffusee
        P2OLD(I)= UVAR(I,18+KK)   ! Pressure
!        EPSEQ2(I)= UVAR(I,19+KK)   ! Dprag
        ECOLD = -T20 * SPH2
        MU2 = (RHO2OLD/RHO20 - ONE)
        H2 = SPH2*V2OLD*(ONE+MU2)
           IF(MU2 > ZERO) ECOLD = ECOLD * (ONE+C52*MU2*(ONE-MU2)) + HALF*C22*MU2*MU2
        ECOLD2 = ECOLD*V2OLD*(ONE+MU2)
        TEMP2 = (EINT2 - ECOLD2)/MAX(H2,EM20)


        !===================================
        ! material 3 : THERMODYNAMICAL STATE
        !===================================
        KK = M51_N0PHAS + 2*M51_NVPHAS
        EINT3   = UVAR(I,8+KK)    ! energie IN rho e OUT
        MAS3    = UVAR(I,9+KK)    ! masse IN rho OUT
        Q3OLD   = UVAR(I,10+KK)
        V3OLD   = UVAR(I,11+KK)
        RHO3OLD = UVAR(I,12+KK)   ! rho_old IN rho OUT
        DDVOL3  = UVAR(I,13+KK)
        SSP3    = UVAR(I,14+KK)   ! sound speed
        PLAS3(I)= UVAR(I,15+KK)   ! Eps plastique
        TEMP3   = UVAR(I,16+KK)   ! temperature
        EDIF3   = UVAR(I,17+KK)   ! chaleur diffusee
        P3OLD(I)= UVAR(I,18+KK)   ! Pressure
!        EPSEQ3(I)= UVAR(I,19+KK)   ! Dprag
        ECOLD   = -T30 * SPH3
        MU3     = (RHO3OLD/RHO30 - ONE)
        H3      = SPH3*V3OLD*(ONE+MU3)
          IF(MU3 > ZERO) ECOLD = ECOLD * (ONE+C53*MU3*(ONE-MU3)) + HALF*C23*MU3*MU3
        ECOLD3  = ECOLD*V3OLD*(ONE+MU3)
        TEMP3   = (EINT3 - ECOLD3)/MAX(H3,EM20)

        !===================================
        ! H.Explosive : THERMODYNAMICAL STATE
        !===================================
        IF(IEXP == 0)THEN
          EINT4   = ZERO
          MAS4    = ZERO
          Q4OLD   = ZERO
          V4OLD   = ZERO
          RHO4OLD = ZERO
          DDVOL4  = ZERO
          SSP4    = ZERO
          TEMP4   = ZERO
          EDIF4   = ZERO
          MU4     = ZERO
          H4      = ZERO
          ECOLD4  = ZERO
          TEMP4   = ZERO
          P4OLD(I)= ZERO
        ELSE
          KK = M51_N0PHAS + 3*M51_NVPHAS
          EINT4   = UVAR(I,8+KK)
          MAS4    = UVAR(I,9+KK)
          Q4OLD   = UVAR(I,10+KK)
          V4OLD   = UVAR(I,11+KK)
          RHO4OLD = UVAR(I,12+KK)
          DDVOL4  = UVAR(I,13+KK)
          SSP4    = UVAR(I,14+KK)   ! sound speed
          TEMP4   = UVAR(I,16+KK)
          TBURN =  UVAR(I, 15+KK)
          EDIF4   = UVAR(I,17+KK)   ! chaleur diffusee
          P4OLD(I)= UVAR(I,KK + 18) ! Pressure
          ECOLD = -T40 * SPH4
          MU4 = (RHO4OLD/RHO40 - ONE)
          H4 = SPH4*V4OLD*(ONE+MU4)
          ECOLD4 = ECOLD*V4OLD*(ONE+MU4)
          TEMP4 = (EINT4 - ECOLD4)/MAX(H4,EM20)
          BFRAC(I) = GBUF%BFRAC(I)

        ENDIF

        !===================================
        ! GLOBAL PRESSURE IN ELEMENT
        !===================================
        POLD = P1OLD(I)*V1OLD + P2OLD(I)*V2OLD+ P3OLD(I)*V3OLD + P4OLD(I)*V4OLD
        POLD = POLD / (V1OLD + V2OLD + V3OLD + V4OLD)


        !=======================================================================
        !  THERMAL CONDUCTION BETWEEN THE MATERIALS
        !     --> Energy should here corrected with TdH
        !                    (dE = dW+dQ = -PdV+TdH)
        !=======================================================================
        IF(JTHE == 1)THEN
         IF(V1OLD+V2OLD+V3OLD > EM03)THEN
         ! heat in
          T = ( EINT1 - ECOLD1 + EINT2 - ECOLD2 + EINT3 - ECOLD3 + UVAR(I,2)) / (H1 + H2 + H3)
          !--------material_1---------!
          AAA   = (T-TEMP1)*H1        ! 'TdH'
          EDIF1   = EDIF1 + AAA       ! -> Heat 'Q'
          EINT1   = EINT1 + AAA       ! -> E(k)=E(k)+ TdH
          !--------material_2---------!
          AAA   = (T-TEMP2)*H2
          EDIF2   = EDIF2 + AAA
          EINT2   = EINT2 + AAA
          !--------material_3---------!
          AAA   = (T-TEMP3)*H3
          EDIF3   = EDIF3 + AAA
          EINT3   = EINT3 + AAA
          !--------output-------------!
          !================================================!
          !Internal Energy is here bounded (Inferior limit)!
          !for stability reasons                           !
          !================================================!
          EINT1=max(EINT1,MAS1/MAX(RHO10,EM20)*E1_INF)
          EINT2=max(EINT2,MAS2/MAX(RHO20,EM20)*E2_INF)
          EINT3=max(EINT3,MAS3/MAX(RHO30,EM20)*E3_INF)
          EINT4=max(EINT4,MAS4/MAX(RHO40,EM20)*E4_INF)
         END IF
        ECOLD4 = ZERO
        END IF
        IF (TIME  ==  ZERO) THEN
           EINT(I)= EINT1 + EINT2 + EINT3 + EINT4
        ENDIF
        !=======================================================================
        !  NUMERICAL VISCOSITY
        !=======================================================================
        QOLD = Q1OLD*V1OLD + Q2OLD*V2OLD + Q3OLD*V3OLD + Q4OLD*V4OLD
        QOLD = QOLD /(V1OLD + V2OLD + V3OLD + V4OLD)

        DD   = -EPSPXX(I)-EPSPYY(I)-EPSPZZ(I)

        dbVOLD(1) = V1OLD
        dbVOLD(2) = V2OLD
        dbVOLD(3) = V3OLD
        dbVOLD(4) = V4OLD

        IF(ninter22 > 0) DD=ZERO !int22 > 0

        QA   = GEO(14,PID(I)) !1.225D00
        QB   = GEO(15,PID(I)) !0.06D00
        XL   = VOLUME(I)**THIRD
        QAL  = (QA*XL)**2
        QBL  = QB*XL

        VQ0  = RHO(I)*QAL*MAX(ZERO,DD)
        VQ0  = VQ0 + (RHO1OLD*SSP1*V1OLD+RHO2OLD*SSP2*V2OLD+RHO3OLD*SSP3*V3OLD+RHO4OLD*SSP4*V4OLD)*QBL/(V1OLD+V2OLD+V3OLD+V4OLD)
        VQ1  = VQ0
        VQ2  = VQ0
        VQ3  = VQ0
        VQ4  = VQ0

        Q0   = VQ0*MAX(ZERO,DD)
        Q1   = Q0
        Q2   = Q0
        Q3   = Q0
        Q4   = Q0

        XL1  = V1OLD**THIRD
        XL2  = V2OLD**THIRD
        XL3  = V3OLD**THIRD
        XL4  = V4OLD**THIRD

        QAL1 = (QA*XL1)**2
        QAL2 = (QA*XL2)**2
        QAL3 = (QA*XL3)**2
        QAL4 = (QA*XL4)**2

        QBL1 = QB*XL1
        QBL2 = QB*XL2
        QBL3 = QB*XL3
        QBL4 = QB*XL4

        IF(DD > ZERO)THEN
          DD2  = DD * DD
          VQ1  = RHO1OLD*(QAL1*DD2 + SSP1*QBL1*DD)
          VQ2  = RHO2OLD*(QAL2*DD2 + SSP2*QBL2*DD)
          VQ3  = RHO3OLD*(QAL3*DD2 + SSP3*QBL3*DD)
          VQ4  = RHO4OLD*(QAL4*DD2 + SSP4*QBL4*DD)
        ELSE
          VQ1  = ZERO
          VQ2  = ZERO
          VQ3  = ZERO
          VQ4  = ZERO
        ENDIF

        Q1   = VQ1
        Q2   = VQ2
        Q3   = VQ3
        Q4   = VQ4

        SSP1 = ZERO
        SSP2 = ZERO
        SSP3 = ZERO
        SSP4 = ZERO

!------------------------------------------------------------!
!                       'SUBMAT_CODE'                        !
!                      ---------------                       !
!                                                            !
!    ___Description_____________________________________     !
!   Describe the presence/absence of differtent material     !
!   possible values 1,2,3,...,15                             !
!   this integer can be described with 4 bits                !
!                                                            !
!               mat1   mat2  mat3  mat4                      !
!              +-----+-----+-----+-----+                     !
!              |  0  |  1  |  1  |  0  |                     !
!              +-----+-----+-----+-----+                     !
!                 0     1     2     3                        !
!                2     2     2     2                         !
!                                                            !
!------------------------------------------------------------!
        SUBMAT_CODE = 0
        MASS = MAS1 + MAS2 + MAS3 + MAS4
        VOLD = V1OLD + V2OLD + V3OLD + V4OLD

        IF (MAS1 / MASS  >  TOL51 .AND. MAS1/RHO10/VOLD > Tol51) THEN
           SUBMAT_CODE = SUBMAT_CODE + 1
           V10 = MAS1 / RHO10
           RHO0E1 = EINT1/V10
           ALPHA1OLD = V1OLD / VOLD
           !   PRINT*, "ALPHA1OLD",ALPHA1OLD
           !ENDIF
           V1 = V1OLD - TIMESTEP*DDVOL1 + ALPHA1OLD * DDVOL(I)
           V1 = MAX(ZERO,V1)
        ELSE
           MAS1  = ZERO
           V1 = ZERO
           V10   = ZERO
           ALPHA1OLD = ZERO
        ENDIF
        IF (MAS2 / MASS  >  TOL51 .AND. MAS2/RHO20/VOLD > Tol51) THEN
           SUBMAT_CODE = SUBMAT_CODE + 2
           V20 = MAS2 / RHO20
           RHO0E2 = EINT2/V20
           ALPHA2OLD = V2OLD / VOLD
           !   PRINT*, "ALPHA2OLD",ALPHA2OLD
           !ENDIF
           V2 = V2OLD - TIMESTEP*DDVOL2 + ALPHA2OLD * DDVOL(I)
           V2 = MAX(ZERO,V2)
        ELSE
           MAS2  = ZERO
           V2 = ZERO
           V20   = ZERO
           ALPHA2OLD = ZERO
        ENDIF

        IF (MAS3 / MASS  >  TOL51 .AND. MAS3/RHO30/VOLD > Tol51) THEN
           SUBMAT_CODE = SUBMAT_CODE + 4
           V30 = MAS3 / RHO30
           RHO0E3 = EINT3/V30
           ALPHA3OLD = V3OLD / VOLD
           !   PRINT*, "ALPHA3OLD",ALPHA3OLD
           !ENDIF
           V3 = V3OLD - TIMESTEP*DDVOL3 + ALPHA3OLD * DDVOL(I)
           V3 = MAX(ZERO,V3)
        ELSE
          MAS3  = ZERO
          V3 = ZERO
          V30   = ZERO
          ALPHA3OLD = ZERO
        ENDIF

        IF (MAS4 / MASS  >  TOL51 .AND. MAS4/RHO40/VOLD > Tol51) THEN
           SUBMAT_CODE = SUBMAT_CODE + 8
           V40 = MAS4 / RHO40
           ALPHA4OLD = V4OLD / VOLD
           !   PRINT*, "ALPHA4OLD",ALPHA4OLD
           !ENDIF
           V4 = V4OLD - TIMESTEP*DDVOL4 + ALPHA4OLD * DDVOL(I)
           V4 = MAX(ZERO,V4)
        ELSE
           MAS4  = ZERO
           V4 = ZERO
           V40   = ZERO
           ALPHA4OLD = ZERO
        ENDIF
        ! SANITY CHECK
        AA = (V1 + V2 + V3 + V4) / VOLUME(I)
        IF(AA > EM06)THEN
         V1 = V1 / AA
         V2 = V2 / AA
         V3 = V3 / AA
         V4 = V4 / AA
        ELSE
         V1 = ZERO
         V2 = ZERO
         V3 = ZERO
         V4 = ZERO
        ENDIF
        !Mass may have changed and then needs to be update.
        MASS = MAS1 + MAS2 + MAS3 + MAS4
        RHO(I) = MASS / VOLUME(I)

        RHOOLD = ALPHA1OLD * RHO1OLD + ALPHA2OLD * RHO2OLD + ALPHA3OLD * RHO3OLD + ALPHA4OLD * RHO4OLD

        dbVOLD_f(1) = V1OLD
        dbVOLD_f(2) = V2OLD
        dbVOLD_f(3) = V3OLD
        dbVOLD_f(4) = V4OLD

        !=======================================================================
        ! IF ONLY ONE MATERIAL IN ELEMENT
        !      MAT1 : (SUBMAT_CODE=1) => Polynomial EOS
        !   or MAT2 : (SUBMAT_CODE=2) => Polynomial EOS
        !   or MAT3 : (SUBMAT_CODE=4) => Polynomial EOS
        !   or MAT4 : (SUBMAT_CODE=8) => JWL EOS
        !=======================================================================
        IF(SUBMAT_CODE == 1 .OR. SUBMAT_CODE == 2 .OR. SUBMAT_CODE == 4 .OR. SUBMAT_CODE == 8) THEN

          P1    = ZERO
          P2    = ZERO
          P3    = ZERO
          P4    = ZERO
          Q1    = ZERO
          Q2    = ZERO
          Q3    = ZERO
          Q4    = ZERO
          V1    = ZERO
          V2    = ZERO
          V3    = ZERO
          V4    = ZERO

         !==========================================
         ! The only material is MAT1 (sol, liq, gas)
         !==========================================
          IF(SUBMAT_CODE == 1)THEN
             EINT1 = EINT1  - (PEXT + POLD + QQOLD(I)) * DDVOL(I)
             EINT(I) = EINT1
             V1 = VOLUME(I)
             RHO1 = MASS / V1
             CALL POLYUN51 ( &
                  C01,C11,C21,C31,C41,C51,GG1, &
                  VOLUME(I),DVOL,V1OLD, &
                  RHO(I),MAS1 ,RHO10,DD,MU1,MU1P1, &
                  POLD,PEXT,P1,PM1,Q1, &
                  RHO0E1,EINT1 ,VISCMAX(I),XL ,SSP1, &
                  QA,QB,UPARAM(101))
     
                  SOUNDSP(I) = SSP1
                  P = P1
                  Q = Q1
                  V1 = VOLUME(I)
                  RHO1 = RHO(I)


         !==========================================
         ! The only material is MAT2 (sol, liq, gas)
         !==========================================
         ELSEIF(SUBMAT_CODE == 2)THEN
            EINT2 = EINT2  - (PEXT + POLD + QQOLD(I)) * DDVOL(I)
            EINT(I) = EINT2
            V2 = VOLUME(I)
            RHO2 = MASS / V2
            CALL POLYUN51 ( &
                 C02,C12,C22,C32,C42,C52,GG2, &
                 VOLUME(I),DVOL,V2OLD, &
                 RHO(I),MAS2 ,RHO20,DD,MU2,MU2P1, &
                 POLD,PEXT,P2,PM2,Q2, &
                 RHO0E2,EINT2 ,VISCMAX(I),XL ,SSP2, &
                 QA,QB,UPARAM(151))
     
                  SOUNDSP(I) = SSP2
                  P = P2
                  Q = Q2
                  V2 = VOLUME(I)
                  RHO2 = RHO(I)
                       
         !==========================================
         ! The only material is MAT3 (sol, liq, gas)
         !==========================================
         ELSEIF(SUBMAT_CODE == 4)THEN
            EINT3 = EINT3  - (PEXT + POLD + QQOLD(I)) * DDVOL(I)
            EINT(I) = EINT3
            V3 = VOLUME(I)
            RHO3 = MASS / V3
            CALL POLYUN51 ( &
                   C03,C13,C23,C33,C43,C53,GG3,&
                   VOLUME(I),DVOL,V3OLD,&
                   RHO(I),MAS3 ,RHO30,DD,MU3,MU3P1,&
                   POLD,PEXT,P3,PM3,Q3,&
                   RHO0E3,EINT3,VISCMAX(I),XL ,SSP3,&
                   QA,QB,UPARAM(201))
     
                 SOUNDSP(I) = SSP3
                 P = P3
                 Q = Q3
                 V3 = VOLUME(I)
                 RHO3 = RHO(I)
                       
         !===========================================
         ! The only material is MAT4 (High Explosive)
         !===========================================
         ELSEIF(SUBMAT_CODE == 8)THEN
            EINT4 = EINT4  - (PEXT + POLD + QQOLD(I)) * DDVOL(I)
            EINT(I) = EINT4
            V4 = VOLUME(I)
            RHO4 = MASS / V4

            !---------------------------------------------------
            !     Computation of burnt fraction
            !---------------------------------------------------
            CALL JWLUN51 (TIME,XL,TBURN,UPARAM,DD,MU4,MU4P1, &
                 VOLUME(I),DVOL, V4OLD,EINT4,VISCMAX(I),&
                 Q4,PEXT,P4,PM4,&
                 RHO(I),RHO40,MAS4,&
                 SSP4,&
                 QA,QB,BFRAC(I))

                 SOUNDSP(I) = SSP4
                 P = P4
                 Q = Q4
                 V4 = VOLUME(I)
                 RHO4 = RHO(I)
                      
           ENDIF

        ! MORE THAN ONE MATERIAL IN ELEMENT
        !    SUBMAT_CODE /= 1,2,4,8
        !    SUBMAT_CODE /= 0
        ELSEIF(SUBMAT_CODE /= 0) THEN


!=======================================================================
!=======================================================================
!     Iterative solver
!=======================================================================
!=======================================================================

          ! Pseudo viscosity pondarated by old massic fractions
          Q1OLD = RHO1OLD / RHOOLD * QQOLD(I)
          Q2OLD = RHO2OLD / RHOOLD * QQOLD(I)
          Q3OLD = RHO3OLD / RHOOLD * QQOLD(I)
          Q4OLD = RHO4OLD / RHOOLD * QQOLD(I)

          !---------------------------------------------------
          !     Submaterial energies evolve according to
          !     dEi / dt + div(Ei u) + alpha_i p_i div(u) = 0
          !     with a fraction of the global pseudo-viscosity
          !     coefficient
          !---------------------------------------------------
          EINT1 = EINT1 - ALPHA1OLD * (PEXT + P1OLD(I) + Q1OLD) * DDVOL(I)
          EINT2 = EINT2 - ALPHA2OLD * (PEXT + P2OLD(I) + Q2OLD) * DDVOL(I)
          EINT3 = EINT3 - ALPHA3OLD * (PEXT + P3OLD(I) + Q3OLD) * DDVOL(I)
          EINT4 = EINT4 - ALPHA4OLD * (PEXT + P4OLD(I) + Q4OLD) * DDVOL(I)

          !---------------------------------------------------
          !     Global energy evolution
          !     d(E) / dt + div(Eu) +  P div(u) = 0
          !     with contribution of pseudo viscosity and
          !     pressure at time t^n.
          !     However, since EINT1 ... EINT4 have received
          !     plasticity contributions, we directly set
          !     EINT(I) = EINT1 + EINT2 + EINT3 + EINT4
          !---------------------------------------------------
          EINT(I) = EINT1 + EINT2 + EINT3 + EINT4

          !---------------------------------------------------
          !     COMPUTE MASS DENSITIES FOR EACH FLUID
          !---------------------------------------------------
          P1    = ZERO
          P2    = ZERO
          P3    = ZERO
          P4    = ZERO
          DVDP1 = ZERO
          DVDP2 = ZERO
          DVDP3 = ZERO
          DVDP4 = ZERO
          V1I   = V1
          V2I   = V2
          V3I   = V3
          V4I   = V4
          IF (V1  >  ZERO) RHO1 = MAS1 / V1
          IF (V2  >  ZERO) RHO2 = MAS2 / V2
          IF (V3  >  ZERO) RHO3 = MAS3 / V3
          IF (V4  >  ZERO) RHO4 = MAS4 / V4

          !---------------------------------------------------
          !     COMPUTE PRESSURE FOR EACH SUBMATERIAL
          !---------------------------------------------------

          ! MAT1
          IF (V1  >  ZERO) THEN
             CALL POLY51 (C01,C11,C21,C31,C41,C51,GG1, &
                  V10,V1,V1I,MU1,MU1P1,EINT1, &
                  PEXT,P1,PM1,P1I, &
                  RHO1,RHO10,MAS1,SSP1,DVDP1,DPDV1, E1_INF, &
                  UPARAM(101), 0)
          ENDIF

          ! MAT2
          IF (V2  >  ZERO) THEN
             CALL POLY51 (C02,C12,C22,C32,C42,C52,GG2, &
                  V20,V2,V2I,MU2,MU2P1,EINT2, &
                  PEXT,P2,PM2,P2I, &
                  RHO2,RHO20,MAS2,SSP2,DVDP2,DPDV2, E2_INF, &
                  UPARAM(151), 0)
          ENDIF

          ! MAT3
          IF (V3  >  ZERO) THEN
             CALL POLY51 (C03,C13,C23,C33,C43,C53,GG3, &
                  V30,V3,V3I,MU3,MU3P1,EINT3, &
                  PEXT,P3,PM3,P3I, &
                  RHO3,RHO30,MAS3,SSP3,DVDP3,DPDV3, E3_INF, &
                  UPARAM(201), 0)
          ENDIF

          ! MAT4 : High Explosive
          IF (V4  >  ZERO) THEN
             !---------------------------------------------------
             !     Computation of burnt fraction
             !---------------------------------------------------
             CALL COMPUTE_BFRAC(TIME, XL, TBURN, UPARAM, V4, RHO4, RHO40, BFRAC(I))

             CALL JWL51 (UPARAM, &
                  V4,V4I,MU4,MU4P1,EINT4, &
                  P4OLD(I),PEXT,P4,PM4, &
                  RHO4,RHO40,MAS4,SSP4,DVDP4,DPDV4, &
                  BFRAC(I),V40, 0)
          ENDIF

          !---------------------------------------------------
          !     Save initial state in case of convergence failure
          !---------------------------------------------------
          V1I = V1
          V2I = V2
          V3I = V3
          V4I = V4
          P1I = P1
          P2I = P2
          P3I = P3
          P4I = P4
          EINT1_INI = EINT1
          EINT2_INI = EINT2
          EINT3_INI = EINT3
          EINT4_INI = EINT4
          SSP1_INI = SSP1
          SSP2_INI = SSP2
          SSP3_INI = SSP3
          SSP4_INI = SSP4

          !---------------------------------------------------
          !     ITERATIVE SOLVER PARAMETERS
          !---------------------------------------------------
          ITER = 0
          NITER = 50
          TOL = EM4
          TOL2 = EM12
          ERROR = EP10
          ERROR2 = ERROR
          STEP = ONE
          CONT = 1
          DO WHILE(ITER <= NITER .AND. CONT  ==  1)
             ITER = ITER + 1

             COEF1 = ZERO
             IF (V1  >  ZERO .AND. SSP1  >  ZERO) THEN
                COEF1 = V1 / RHO1 / SSP1 / SSP1
             ENDIF
             COEF2 = ZERO
             IF (V2  >  ZERO .AND. SSP2  >  ZERO) THEN
                COEF2 = V2 / RHO2 / SSP2 / SSP2
             ENDIF
             COEF3 = ZERO
             IF (V3  >  ZERO .AND. SSP3  >  ZERO) THEN
                COEF3 = V3 / RHO3 / SSP3 / SSP3
             ENDIF
             COEF4 = ZERO
             IF (V4  >  ZERO .AND. SSP4  >  ZERO) THEN
                COEF4 = V4 / RHO4 / SSP4 / SSP4
             ENDIF
             !!! Relaxation pressure
             P = (COEF1 * P1 + COEF2 * P2 + COEF3 * P3 + COEF4 * P4)
             P = P / (COEF1 + COEF2 + COEF3 + COEF4)

             !!! Compute corresponding volume variations
             DV1 = COEF1 * (P1 - P)
             DV2 = COEF2 * (P2 - P)
             DV3 = COEF3 * (P3 - P)
             DV4 = COEF4 * (P4 - P)

             !!! Compute a step less or equal that one to ensure
             !!! that volume does not decrease of more than half
             !!! of its previous value

             STEP = ONE
             IF (DV1 < ZERO) THEN
                STEP = MIN(STEP, -HALF * V1 / DV1)
             ENDIF
             IF (DV2 < ZERO) THEN
                STEP = MIN(STEP, -HALF * V2 / DV2)
             ENDIF
             IF (DV3 < ZERO) THEN
                STEP = MIN(STEP, -HALF * V3 / DV3)
             ENDIF
             IF (DV4 < ZERO) THEN
                STEP = MIN(STEP, -HALF * V4 / DV4)
             ENDIF

             DV1 = STEP * DV1
             DV2 = STEP * DV2
             DV3 = STEP * DV3
             DV4 = STEP * DV4

             !!! Note that SUM (DV_i) = ZERO so that volume conservation is achieved

             !!! Update submaterial internal energies in a thermodynamically consistent way
             !!! only if this has a sense (GRUN /= 0)
             !!! Gruneisen coefficient : GRUN = 1 / RHO * Dp / De (cst rho)
             
             IF (V1  >  ZERO) THEN
                EINT1 = EINT1 - (P + PEXT) * DV1
             ENDIF
             IF (V2  >  ZERO) THEN
                EINT2 = EINT2 - (P + PEXT) * DV2
             ENDIF
             IF (V3  >  ZERO) THEN
                EINT3 = EINT3 - (P + PEXT) * DV3
             ENDIF
             IF (V4  >  ZERO) THEN
                EINT4 = EINT4 - (P + PEXT) * DV4
             ENDIF

             !!! Update volumes
             V1 = V1 + DV1
             V2 = V2 + DV2
             V3 = V3 + DV3
             V4 = V4 + DV4

             AA = (V1 + V2 + V3 + V4) / VOLUME(I)
             V1 = V1 / AA
             V2 = V2 / AA
             V3 = V3 / AA
             V4 = V4 / AA

             !!! Update densities
             IF (V1  >  ZERO) RHO1 = MAS1 / V1
             IF (V2  >  ZERO) RHO2 = MAS2 / V2
             IF (V3  >  ZERO) RHO3 = MAS3 / V3
             IF (V4  >  ZERO) RHO4 = MAS4 / V4

             !!! Update submaterial pressures
             PM5 = -EP20
             IF (V1  >  ZERO) PM5 = max(PM5,PM1)
             IF (V2  >  ZERO) PM5 = max(PM5,PM2)
             IF (V3  >  ZERO) PM5 = max(PM5,PM3)
             IF (V4  >  ZERO) PM5 = max(PM5,PM4)

            !===========================================
            !       material 1 - Polynomial EOS
            !===========================================
            IF (V1  >  ZERO) THEN
                CALL POLY51 (C01,C11,C21,C31,C41,C51,GG1, &
                              V10,V1,V1I,MU1,MU1P1,EINT1, &
                              PEXT,P1,PM5,P1I, &
                              RHO1,RHO10,MAS1,SSP1,DVDP1,DPDV1, E1_INF, &
                              UPARAM(101), 0)
            ENDIF
            !===========================================
            !       material 2 - Polynomial EOS
            !===========================================
            IF (V2  >  ZERO) THEN
                CALL POLY51 (C02,C12,C22,C32,C42,C52,GG2, &
                              V20,V2,V2I,MU2,MU2P1,EINT2, &
                              PEXT,P2,PM5,P2I, &
                              RHO2,RHO20,MAS2,SSP2,DVDP2,DPDV2, E2_INF, &
                              UPARAM(151), 0)
            ENDIF
            !===========================================
            !       material 3 - Polynomial EOS
            !===========================================
            IF (V3  >  ZERO) THEN
                CALL POLY51 (C03,C13,C23,C33,C43,C53,GG3, &
                              V30,V3,V3I,MU3,MU3P1,EINT3, &
                              PEXT,P3,PM5,P3I, &
                              RHO3,RHO30,MAS3,SSP3,DVDP3,DPDV3, E3_INF, &
                              UPARAM(201), 0)
            ENDIF
            !===========================================
            !       material 4 - Polynomial EOS
            !===========================================
            IF (V4  >  ZERO)THEN
                CALL JWL51 (UPARAM, &
                            V4,V4I,MU4,MU4P1,EINT4,&
                            P4OLD(I),PEXT,P4,PM5,&
                            RHO4,RHO40,MAS4,SSP4,DVDP4,DPDV4,&
                            BFRAC(I),V40, 0)
            ENDIF

            !!! Check convergence
            ERROR = ZERO
            IF (V1  >  ZERO) ERROR = ERROR + ABS(DV1) / V1
            IF (V2  >  ZERO) ERROR = ERROR + ABS(DV2) / V2
            IF (V3  >  ZERO) ERROR = ERROR + ABS(DV3) / V3
            IF (V4  >  ZERO) ERROR = ERROR + ABS(DV4) / V4

            IF (ERROR  <  TOL) THEN
               CONT = 0
            ENDIF

          ENDDO

          !---------------------------------------------------
          !     CONVERGENCE TEST
          !---------------------------------------------------
          IF (CONT  ==  1) THEN
             IF (IDBG  ==  1) THEN
                PRINT*, "*** Non convergence of LAW51 EOS in elem", IX(NIX, I + NFT), ERROR, ERROR2
             ENDIF
          ENDIF
          AA = (V1 + V2 + V3 + V4) / VOLUME(I)
          IF (IDBG==1 .AND. ABS(AA - ONE) > EM14) THEN
             PRINT*, "**WARNING : CONVERGENCE ISSUE", AA
          ENDIF
          ! Sanity check
          ERROR2 = ABS(EINT(I) - EINT1 - EINT2 - EINT3 - EINT4)
          IF (IDBG==1 .AND. ERROR2 > EM13 * (ONE + ABS(EINT(I)))) THEN
             PRINT*, "**Warning : Conversgence Issue", ERROR2
          ENDIF
          Q1 = Q1OLD
          Q2 = Q2OLD
          Q3 = Q3OLD
          Q4 = Q4OLD
          P = P1 * V1 + P2 * V2 + P3 * V3 + P4 * V4
          P = P / VOLUME(I)
!=======================================================================
!=======================================================================
!     END New solver
!=======================================================================
!=======================================================================


        ELSEIF(SUBMAT_CODE == 0) THEN
          !! This case should not occur.It means that MASS<=ZERO
          !print *, "Warning, empty element"
           SOUNDSP(I)=EM20

        ENDIF

        RHO(I) = MASS / VOLUME(I)

        SOUNDSP(I) = ZERO
        VISA1 = ZERO

        IF (MAS1 / MASS  >  TOL51) THEN
           SOUNDSP(I) = SOUNDSP(I) + MAS1 / MASS * SSP1 * SSP1
           VISA1 = VISA1 + V1*RHO1 * VISC1
        ENDIF
        IF (MAS2 / MASS  >  TOL51) THEN
           SOUNDSP(I) = SOUNDSP(I) + MAS2 / MASS * SSP2 * SSP2
           VISA1 = VISA1 + V2*RHO2 * VISC2
        ENDIF
        IF (MAS3 / MASS  >  TOL51) THEN
           SOUNDSP(I) = SOUNDSP(I) + MAS3 / MASS * SSP3 * SSP3
           VISA1 = VISA1 + V3*RHO3 * VISC3
        ENDIF
        IF (MAS4 / MASS  >  TOL51) THEN
           SOUNDSP(I) = SOUNDSP(I) + MAS4 / MASS * SSP4 * SSP4
           VISA1 = VISA1 + V4*RHO4 * VISC4
        ENDIF

        VISA1 = VISA1 / VOLUME(I) / RHO(I)

        SOUNDSP(I) = SQRT(SOUNDSP(I))
        VISCMAX(I)=VQ0
        VISCMAX(I) = VISCMAX(I)/TWO
        VISCMAX(I) = VISCMAX(I) + RHO(I)*(TWO*VISA1 + VISB1)/THREE

        !---------------------------------------------------------------------------------------------!
        !ENERGY INTEGRATION WITH PRESSURE CONTRIBUTION IS DONE AT ENT OF MULAW SUBROUTINE             !
        !---------------------------------------------------------------------------------------------!
        !EINT(I) = EINT(I) - (PEXT+PEXT+QOLD+Q)*DVOL*HALF !Attention : Q est retranche ici car stocke dans Svis pour mulaw et sfint

        !---------------------------!
        !   TOTAL STRESS TENSOR     !
        !---------------------------!
        K1 = M51_N0PHAS
        K2 = M51_N0PHAS+M51_NVPHAS
        K3 = M51_N0PHAS+2*M51_NVPHAS
        K4 = M51_N0PHAS+3*M51_NVPHAS

        SIGNXX(I) = UVAR(I,2+K1) * V1 + UVAR(I,2+K2) * V2 + UVAR(I,2+K3) * V3
        SIGNYY(I) = UVAR(I,3+K1) * V1 + UVAR(I,3+K2) * V2 + UVAR(I,3+K3) * V3
        SIGNZZ(I) = UVAR(I,4+K1) * V1 + UVAR(I,4+K2) * V2 + UVAR(I,4+K3) * V3
        SIGNXY(I) = UVAR(I,5+K1) * V1 + UVAR(I,5+K2) * V2 + UVAR(I,5+K3) * V3
        SIGNYZ(I) = UVAR(I,6+K1) * V1 + UVAR(I,6+K2) * V2 + UVAR(I,6+K3) * V3
        SIGNZX(I) = UVAR(I,7+K1) * V1 + UVAR(I,7+K2) * V2 + UVAR(I,7+K3) * V3
        IF(IEXP /= 0)THEN
          SIGNXX(I) = SIGNXX(I) + UVAR(I,2+K4) * V4
          SIGNYY(I) = SIGNYY(I) + UVAR(I,3+K4) * V4
          SIGNZZ(I) = SIGNZZ(I) + UVAR(I,4+K4) * V4
          SIGNXY(I) = SIGNXY(I) + UVAR(I,5+K4) * V4
          SIGNYZ(I) = SIGNYZ(I) + UVAR(I,6+K4) * V4
          SIGNZX(I) = SIGNZX(I) + UVAR(I,7+K4) * V4
        ENDIF

        SIGNXX(I) = SIGNXX(I) / VOLUME(I)
        SIGNYY(I) = SIGNYY(I) / VOLUME(I)
        SIGNZZ(I) = SIGNZZ(I) / VOLUME(I)
        SIGNXY(I) = SIGNXY(I) / VOLUME(I)
        SIGNYZ(I) = SIGNYZ(I) / VOLUME(I)
        SIGNZX(I) = SIGNZX(I) / VOLUME(I)

        SIGNXX(I) = SIGNXX(I) - P
        SIGNYY(I) = SIGNYY(I) - P
        SIGNZZ(I) = SIGNZZ(I) - P

        !---------------------------!
        !   VISCOUS STRESS TENSOR   !
        !---------------------------!
        SIGVXX(I) = TWO*RHO(I)*VISA1*EPSPXX(I) + RHO(I)*VISB1*(EPSPXX(I)+EPSPYY(I)+EPSPZZ(I))
        SIGVYY(I) = TWO*RHO(I)*VISA1*EPSPYY(I) + RHO(I)*VISB1*(EPSPXX(I)+EPSPYY(I)+EPSPZZ(I))
        SIGVZZ(I) = TWO*RHO(I)*VISA1*EPSPZZ(I) + RHO(I)*VISB1*(EPSPXX(I)+EPSPYY(I)+EPSPZZ(I))
        SIGVXY(I) = RHO(I)* VISA1 * EPSPXY(I)
        SIGVYZ(I) = RHO(I)* VISA1 * EPSPYZ(I)
        SIGVZX(I) = RHO(I)* VISA1 * EPSPZX(I)

      !===========================================================C
      !  OUTPUT : UVAR(,) SUBMATERIAL OUTPUTS                     C
      !===========================================================C

        !===================================================C
        !        material 1 : THERMODYNAMICAL STATE OUTPUT  C
        !===================================================C
        KK = M51_N0PHAS
        IF(V1  >  ZERO)THEN
            UVAR(I,1+KK)  = V1 / VOLUME(I)
            !deviator stress already updated in plasticity subroutines
            UVAR(I,18+KK) =   P1
            UVAR(I,14+KK) = SSP1
            UVAR(I,15+KK) = PLAS1(I)     ! Eps plastique
            ECOLD         = -T10 * SPH1
            IF(MU1 > ZERO) ECOLD = ECOLD * (ONE+C51*MU1*(ONE-MU1)) + HALF*C21*MU1*MU1
            TEMP1         = (EINT1/V1/MU1P1 - ECOLD) / SPH1
            UVAR(I,16+KK) = TEMP1
            UVAR(I,8+KK)  = EINT1 / V1 ! energie IN rho e OUT
            UVAR(I,9+KK)  = RHO1       ! masse IN rho OUT
            UVAR(I,10+KK) = Q1
            UVAR(I,11+KK) = V1
            UVAR(I,12+KK) = RHO1       ! rho_old IN rho OUT
            UVAR(I,17+KK) = EDIF1 / V1 ! energie diffusee IN spec OUT
!           UVAR(I,19+KK) = EPSEQ1(I)
        ELSE
            UVAR(I,1+KK)  = ZERO
            UVAR(I,2+KK)  = ZERO
            UVAR(I,3+KK)  = ZERO
            UVAR(I,4+KK)  = ZERO
            UVAR(I,5+KK)  = ZERO
            UVAR(I,6+KK)  = ZERO
            UVAR(I,7+KK)  = ZERO
            UVAR(I,8+KK)  = ZERO
            UVAR(I,9+KK)  = ZERO
            UVAR(I,10+KK) = ZERO
            UVAR(I,11+KK) = ZERO
            UVAR(I,12+KK) = RHO10
            UVAR(I,14+KK) = ZERO
            UVAR(I,15+KK) = ZERO
            UVAR(I,16+KK) = T10
            TEMP1         = T10
            UVAR(I,17+KK) = ZERO ! energie diffusee IN spec OUT
            UVAR(I,18+KK) =  P
            UVAR(I,19+KK) = ZERO
            EPSEQ1(I)     = ZERO
        ENDIF

        !===================================================C
        !        material 2 : THERMODYNAMICAL STATE OUTPUT  C
        !===================================================C
        KK = M51_N0PHAS + M51_NVPHAS
        IF(V2  >  ZERO)THEN
            UVAR(I,1+KK)  = V2  / VOLUME(I)
            !deviator stress already updated in plasticity subroutines
            UVAR(I,18+KK) = P2
            UVAR(I,14+KK) = SSP2
            UVAR(I,15+KK) = PLAS2(I)   ! Eps plastique
            ECOLD         = -T20 * SPH2
            IF(MU2 > ZERO) ECOLD = ECOLD * (ONE+C52*MU2*(ONE-MU2)) + HALF*C22*MU2*MU2
            TEMP2         = (EINT2/V2/(MU2P1) - ECOLD) / SPH2
            UVAR(I,16+KK) = TEMP2
            UVAR(I,8+KK)  = EINT2 / V2
            UVAR(I,9+KK)  = RHO2
            UVAR(I,10+KK) = Q2
            UVAR(I,11+KK) = V2
            UVAR(I,12+KK) = RHO2
            UVAR(I,17+KK) = EDIF2 / V2 ! energie diffusee IN spec OUT
!           UVAR(I,19+KK) = EPSEQ2(I)
        ELSE
            UVAR(I,1+KK)  = ZERO
            UVAR(I,2+KK)  = ZERO
            UVAR(I,3+KK)  = ZERO
            UVAR(I,4+KK)  = ZERO
            UVAR(I,5+KK)  = ZERO
            UVAR(I,6+KK)  = ZERO
            UVAR(I,7+KK)  = ZERO
            UVAR(I,8+KK)  = ZERO
            UVAR(I,9+KK)  = ZERO
            UVAR(I,10+KK) = ZERO
            UVAR(I,11+KK) = ZERO
            UVAR(I,12+KK) = RHO20
            UVAR(I,14+KK) = ZERO
            UVAR(I,15+KK) = ZERO
            UVAR(I,16+KK) = T20
            TEMP2         = T20
            UVAR(I,17+KK) = ZERO ! energie diffusee IN spec OUT
            UVAR(I,18+KK) =   P
            UVAR(I,19+KK) = ZERO
            EPSEQ2(I)     = ZERO
        ENDIF

        !===================================================C
        !        material 3 : THERMODYNAMICAL STATE OUTPUT  C
        !===================================================C
        KK = M51_N0PHAS + 2*M51_NVPHAS
        IF(V3  >  ZERO)THEN
            UVAR(I,1+KK)  = V3  / VOLUME(I)
            !deviator stress already updated in plasticity subroutines
            UVAR(I,18+KK) =   P3
            UVAR(I,14+KK) = SSP3
            UVAR(I,15+KK) = PLAS3(I)   ! Eps plastique
            ECOLD         = -T30 * SPH3
            IF(MU3 > ZERO) ECOLD = ECOLD * (ONE+C53*MU3*(ONE-MU3)) + HALF*C23*MU3*MU3
            TEMP3         = (EINT3/V3/MU3P1 - ECOLD) / SPH3
            UVAR(I,16+KK) = TEMP3
            UVAR(I,8+KK)  = EINT3 / V3
            UVAR(I,9+KK)  = RHO3
            UVAR(I,10+KK) = Q3
            UVAR(I,11+KK) = V3
            UVAR(I,12+KK) = RHO3
            UVAR(I,17+KK) = EDIF3 / V3 ! energie diffusee IN spec OUT
!           UVAR(I,19+KK) = EPSEQ3(I)
        ELSE
            UVAR(I,1+KK)  = ZERO
            UVAR(I,2+KK)  = ZERO
            UVAR(I,3+KK)  = ZERO
            UVAR(I,4+KK)  = ZERO
            UVAR(I,5+KK)  = ZERO
            UVAR(I,6+KK)  = ZERO
            UVAR(I,7+KK)  = ZERO
            UVAR(I,8+KK)  = ZERO
            UVAR(I,9+KK)  = ZERO
            UVAR(I,10+KK) = ZERO
            UVAR(I,11+KK) = ZERO
            UVAR(I,12+KK) = RHO30
            UVAR(I,14+KK) = ZERO
            UVAR(I,15+KK) = ZERO
            UVAR(I,16+KK) = T30
            UVAR(I,17+KK) = ZERO ! energie diffusee IN spec OUT
            UVAR(I,18+KK) =   P
            TEMP3         = T30
            UVAR(I,19+KK) = ZERO
            EPSEQ3(I)     = ZERO
        ENDIF

        !===================================================C
        !        material 4 : THERMODYNAMICAL STATE OUTPUT  C
        !===================================================C
        IF(IEXP/=0)THEN
        KK = M51_N0PHAS + 3*M51_NVPHAS
         IF(V4  >  ZERO)THEN
            UVAR(I,1+KK)  = V4 / VOLUME(I)
            UVAR(I,18+KK) = P4
            UVAR(I,14+KK) = SSP4
            UVAR(I,15+KK) = ZERO      ! Eps plastique
            UVAR(I,8+KK)  = EINT4 / V4 ! energie IN rho e OUT
            UVAR(I,9+KK)  = RHO4       ! masse IN rho OUT
            UVAR(I,10+KK) = Q4
            UVAR(I,11+KK) = V4
            UVAR(I,12+KK) = RHO4       ! rho_old IN rho OUT
            UVAR(I,15+KK) = TBURN   ! -burn time ou burn fraction
            UVAR(I,17+KK) = EDIF4 / V4 ! energie diffusee IN spec OUT
            TEMP4 = T40
            UVAR(I,19+KK) = ZERO
         ELSE
            UVAR(I,1+KK)  = ZERO
            UVAR(I,2+KK)  = ZERO
            UVAR(I,3+KK)  = ZERO
            UVAR(I,4+KK)  = ZERO
            UVAR(I,18+KK) = P
            UVAR(I,5+KK)  = ZERO
            UVAR(I,6+KK)  = ZERO
            UVAR(I,7+KK)  = ZERO
            UVAR(I,8+KK)  = ZERO
            UVAR(I,9+KK)  = ZERO
            UVAR(I,10+KK) = ZERO
            UVAR(I,11+KK) = ZERO
            UVAR(I,12+KK) = RHO40
            UVAR(I,14+KK) = ZERO
            UVAR(I,16+KK) = T40
            UVAR(I,17+KK) = ZERO ! energie diffusee IN spec OUT
            TEMP4 = T40
            UVAR(I,19+KK) = ZERO
         ENDIF
        END IF
        !===================================================
        !               UVAR(I,1) : PLAS
        !===================================================
        IF(IPLA1 == 2)PLAS1(I)=EPSEQ1(I)
        IF(IPLA2 == 2)PLAS2(I)=EPSEQ2(I)
        IF(IPLA3 == 2)PLAS3(I)=EPSEQ3(I)
        UVAR(I,1) = (PLAS1(I) * V1 + PLAS2(I) * V2 + PLAS3(I) * V3) / VOLUME(I)
        IF(BUFLY%L_PLA>0)LBUF%PLA(I) = UVAR(I,1)

        !===================================================
        !               UVAR(I,2)    : TEMP
        !===================================================
        H1 = SPH1*V1*(MU1P1)
        H2 = SPH2*V2*(MU2P1)
        H3 = SPH3*V3*(MU3P1)
        H4 = SPH4*V4*(MU4P1)
        ! temperature out ! chaleur     in
        UVAR(I,2) = (TEMP1*H1 + TEMP2*H2 + TEMP3*H3 + TEMP4*H4)  / (H1 + H2 + H3 + H4)
        IF(BUFLY%L_TEMP>0)LBUF%TEMP(I) = UVAR(I,2)
        !===================================================
        !               UVAR(I,3) : BFRAC
        !===================================================
        IF(V4 > ZERO)THEN
          UVAR(I,3) = BFRAC(I)
        ELSE
          UVAR(I,3) = ZERO
        END IF
        IF(BUFLY%L_BFRAC>0)LBUF%BFRAC(I) = UVAR(I,3)
        !===================================================
        !               EPSEQ (DPrag)
        !===================================================
        IF(BUFLY%L_EPSQ>0)THEN
          LBUF%EPSQ(I) = ZERO
          IF(IPLA1==2)LBUF%EPSQ(I) = LBUF%EPSQ(I) + V1*EPSEQ1(I)
          IF(IPLA2==2)LBUF%EPSQ(I) = LBUF%EPSQ(I) + V2*EPSEQ2(I)
          IF(IPLA3==2)LBUF%EPSQ(I) = LBUF%EPSQ(I) + V3*EPSEQ3(I)
          LBUF%EPSQ(I) = LBUF%EPSQ(I) / VOLUME(I)
        ENDIF

       IF(JTHE == 1 ) THEN
         GBUF%TEMP(I) = THREE100 + EM03
       ELSE
         GBUF%TEMP(I) = UVAR(I,2)
       ENDIF

        QVIS(I) = QQOLD(I)

        !===================================================
        !               POST-TREATMENT (DEBUG)
        !     must be commented in official releases
        !===================================================
!          IF(IX(11,I+NFT)==ibug_ID(1) .OR. IX(11,I+NFT)==ibug_ID(2)) THEN
!            CALL WRITE_BUF_LAW51(IX    , NFT        , NUVAR    , NEL      , UVAR     ,
!       .                         I     , SUBMAT_CODE, DD       , dbVOLD   , dbVOLD_f ,
!       .                         VOLUME, VOLD       , EPSPXX   , EPSPYY   , EPSPZZ   ,
!       .                         TAG22 ,BFRAC(I)    , RHO10    , RHO20    , RHO30    ,
!       .                         RHO40)
!          ENDIF

      ENDDO  !DO I=1,NEL

      RETURN

      END SUBROUTINE SIGEPS51
      END MODULE SIGEPS51_MOD