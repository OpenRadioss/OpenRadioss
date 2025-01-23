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
      !||    sigeps51_boundary_material_mod   ../engine/source/materials/mat/mat051/sigeps51_boundary_material.F90
      !||--- called by ------------------------------------------------------
      !||    sigeps51                         ../engine/source/materials/mat/mat051/sigeps51.F90
      !||====================================================================
      module sigeps51_boundary_material_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief Here is a small description of the routine, [after the header]
!! \details if needed, more details can be added here
      !||====================================================================
      !||    sigeps51_boundary_material   ../engine/source/materials/mat/mat051/sigeps51_boundary_material.F90
      !||--- called by ------------------------------------------------------
      !||    sigeps51                     ../engine/source/materials/mat/mat051/sigeps51.F90
      !||--- calls      -----------------------------------------------------
      !||    finter                       ../engine/source/tools/curve/finter.F
      !||    m51vois2                     ../engine/source/materials/mat/mat051/m51vois2.F
      !||    m51vois3                     ../engine/source/materials/mat/mat051/m51vois3.F
      !||--- uses       -----------------------------------------------------
      !||    ale_connectivity_mod         ../common_source/modules/ale/ale_connectivity_mod.F
      !||    alefvm_mod                   ../common_source/modules/ale/alefvm_mod.F
      !||    constant_mod                 ../common_source/modules/constant_mod.F
      !||    elbufdef_mod                 ../common_source/modules/mat_elem/elbufdef_mod.F90
      !||    multimat_param_mod           ../common_source/modules/multimat_param_mod.F90
      !||    prop_param_mod               ../common_source/modules/mat_elem/prop_param_mod.F90
      !||====================================================================
      subroutine sigeps51_boundary_material ( &
            nel    ,nuparam     ,nuvar   ,nfunc   ,ifunc    ,&
            npf    ,tf          ,time    ,timestep,uparam   ,numel  ,&
            rho    ,volume       ,eint   ,vel_o   ,WFEXT    ,&
            epspxx ,epspyy      ,epspzz  ,&
            sigoxx ,sigoyy      ,sigozz  ,sigoxy  ,sigoyz   ,sigozx ,&
            signxx ,signyy      ,signzz  ,signxy  ,signyz   ,signzx ,&
            soundsp,viscmax     ,uvar    ,nft     ,v      ,&
            w      ,x           ,ix      ,n48     ,nix      ,&
            ilay   ,ng          ,elbuf_tab,pm     ,&
            iparg  ,ale_connect ,bufvois ,ipm     ,bufmat   ,stifn  ,&
            vd2    ,vdx         ,vdy     ,vdz     ,&
            nv46   ,n2d         ,numnod  ,ngroup  ,nummat)
! ======================================================================================================================
!                                                   Modules
! ======================================================================================================================
       use elbufdef_mod ! , only : elbuf_struct_, g_bufel_, l_bufel_, buf_lay_
       !use i22bufbric_mod
       !use i22tri_mod
       use ale_connectivity_mod , only : t_ale_connectivity
       use alefvm_mod , only:alefvm_param
       use multimat_param_mod , only : m51_n0phas, m51_nvphas
       use constant_mod , only : half, zero, one, em20, ep20, two, third, em4,em10, three,em06
       use prop_param_mod , only : n_var_ipm, n_var_pm, n_var_geo, n_var_iparg
! ======================================================================================================================
! phase 0  dim=M51_N0PHAS
!          UVAR(1) = TEMP  !post
!          UVAR(2) = PLAS  !post
!          UVAR(3) = BFRAC !post
! phase k  dim=M51_NVPHAS
!          IAD = M51_N0PHAS+(k-1)*M51_NVPHAS
!          UVAR(1 + IAD) = AVk                 (EV(NB10) +12k)
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
!                                                   Implicit None
! ======================================================================================================================
       implicit none
! ======================================================================================================================
!                                                   Included Files
! ======================================================================================================================
#include "my_real.inc"
! ======================================================================================================================
!                                                   Local Variables
! ======================================================================================================================
      double precision,intent(inout) :: WFEXT
      integer,intent(in) :: nummat,numnod,ngroup !< array size
      integer,intent(in) :: n2d !< flag for 2d / 3d analysis

      integer,intent(in) :: nel, nuparam, nuvar,nft,n48,nix,numel,&
              ix(nix,numel), ilay, ng,iparg(n_var_iparg, ngroup),&
              ipm(n_var_ipm,nummat),nv46

      my_real,intent(in) :: time,timestep,uparam(nuparam),pm(n_var_pm,nummat),&
                            volume(nel),bufvois(*),&
                            epspxx(nel),epspyy(nel),epspzz(nel),&
                            w(3,numnod),x(3,numnod), bufmat(*),&
                            vd2(nel)

       my_real,intent(inout) :: eint(nel),&
                                sigoxx(nel),sigoyy(nel),sigozz(nel),&
                                sigoxy(nel),sigoyz(nel),sigozx(nel),&
                                vdx(nel),vdy(nel),vdz(nel),&
                                V(3,numnod),&
                                rho(nel),stifn(nel)

      type(elbuf_struct_), target, dimension(ngroup) :: elbuf_tab
      type(t_ale_connectivity), intent(in) :: ale_connect

      my_real, intent(inout) :: signxx(nel),signyy(nel),signzz(nel),&
                                signxy(nel),signyz(nel),signzx(nel),&
                                soundsp(nel),viscmax(nel)

      my_real,INTENT(INOUT) :: UVAR(NEL,NUVAR)
! ======================================================================================================================
!                                                   External
! ======================================================================================================================
      INTEGER NPF(*), NFUNC, IFUNC(NFUNC)
      my_real FINTER ,TF(*)
      EXTERNAL FINTER
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

      INTEGER I,J,K,KK,II

      my_real P,PEXT,WFEXTT,P0_NRF,P0_NRFv(nel),DP0,&
              C11,C12,C13,C21,C22,C23,C31,C32,C33,C41,C42,C43,C51,C52,C53,&
              AV1(nel),AV2(nel),AV3(nel),AV4(nel),RHO10,RHO20,RHO30,RHO40,RHO1,RHO2,RHO3,RHO4,&
              P1,P2,P3,&
              SSP1,SSP2,SSP3,SSP4,BETA,&
              MU1,MU2,MU3,&
              MU1P1, MU2P1, MU3P1, MU4P1,&
              E01,E02,E03,E04,PM1,PM2,PM3,PM4,&
              POLD,&
              DD,&
              C01,C02,C03,C04,ABCS,DYDX,&
              AA1,AA2,AA3,&
              EEE,AAA,BBB,CC1,CC2,CC3,U2,&
              VCRT1,VCRT2,VCRT3,U21,U22,U23,GV1,GV2,GV3,RV1,RV2,RV3,&
              VN,X0,Y0,Z0,VX,VY,VZ,NX,NY,NZ,RHO1A,P1A,E01A,RHO2A,P2A,E02A,&
              RHO3A,P3A,E03A,FAC

      my_real VEL_N(nel),VEL_O(nel), VEL(nel),&
              EIV(0:4,nel), RHOV(0:4,nel), PV(0:4,nel), TV(0:4,nel),RHO0V(0:4,nel),&
              AVV(0:4,nel), SSPv(0:4,nel),EPSPv(0:4,nel),&
              RHOC2(4),RHOC20,ROC,&
              alpha, myVAR, PP(nel), PP0(nel), PFAR,&
              MACH,&
              E01f,E02f,E03f,RHO1f,RHO2f,RHO3f
        
      my_real :: VEL_IN, MOM
      INTEGER :: IVEL
      INTEGER IFLG,IAV1,IAV2,IAV3,IRHO1,IRHO2,IRHO3,IE1,IE2,IE3,IEXP,&
              IOPT, IPLA, IPLA1, IPLA2, IPLA3,&
              K1,K2,K3,K4, ISUPERSONIC, IVOI,ML,IFORM,IADBUF
      INTEGER :: IX1,IX2,IX3,IX4
      my_real :: X13, Y13, Z13, X24, Y24, Z24, XN, YN, ZN
      INTEGER ICF3D(4,6), ICF2D(2,4), IAD2
      TYPE(G_BUFEL_)  ,POINTER :: GBUF
      TYPE(L_BUFEL_)  ,POINTER :: LBUF
      TYPE(BUF_LAY_)  ,POINTER :: BUFLY

      DATA ICF3D/1,4,3,2,3,4,8,7,5,6,7,8,1,2,6,5,2,3,7,6,1,5,8,4/
      DATA ICF2D/1,2,2,3,3,4,4,1/

! ======================================================================================================================
!                                                   Body
! ======================================================================================================================
      IFORM = HUGE(IFORM)
      IX1 = HUGE(IX1) 
      IX2 = HUGE(IX2)
      IX3 = HUGE(IX3)
      IX4 = HUGE(IX4)
      ML = HUGE(ML)
      WFEXTT=ZERO
      MU1P1  = ONE
      MU2P1  = ONE
      MU3P1  = ONE
      MU4P1  = ONE

      GBUF   => ELBUF_TAB(NG)%GBUF
      LBUF   => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(1,1,1)
      BUFLY  => ELBUF_TAB(NG)%BUFLY(ILAY)

      IFLG   = NINT(UPARAM(31))

      !===========================================!
      ! Reading Material Flags Through UPARAM()   !
      !===========================================!
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

      PFAR   = UPARAM(7)
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

      VEL_IN = UPARAM(75)


      !===========================================!
      ! INLET (IFLG = 2)                          !
      !===========================================!

      IF(IFLG == 2)THEN
        ABCS = TIME / UPARAM(38)

        IAV1  = IFUNC(1)
        IRHO1 = IFUNC(2)
        IE1   = IFUNC(3)
        IAV2  = IFUNC(4)
        IRHO2 = IFUNC(5)
        IE2   = IFUNC(6)
        IAV3  = IFUNC(7)
        IRHO3 = IFUNC(8)
        IE3   = IFUNC(9)
        IVEL  = IFUNC(10)

        IF(IAV1 /= 0)THEN
          myVAR = FINTER(IAV1,ABCS,NPF,TF,DYDX)
          AV1(1:NEL) = myVAR * AV1(1:NEL)
        ENDIF
        IF(IAV2 /= 0)THEN
          myVAR = FINTER(IAV2,ABCS,NPF,TF,DYDX)
          AV2(1:NEL) = myVAR * AV2(1:NEL)
        ENDIF
        IF(IAV3 /= 0)THEN
          myVAR = FINTER(IAV3,ABCS,NPF,TF,DYDX)
          AV3(1:NEL) = myVAR * AV3(1:NEL)
        ENDIF

        !rho0,E0,P0,SSP0 in UVAR(20,21,22,24)

        !scale factors
        RHO1 = ONE
        RHO2 = ONE
        RHO3 = ONE
        RHO4 = ONE
        E01  = ONE
        E02  = ONE
        E03  = ONE
        E04  = ONE
        IF(IRHO1/=0)RHO1 = FINTER(IRHO1,ABCS,NPF,TF,DYDX)
        IF(IRHO2/=0)RHO2 = FINTER(IRHO2,ABCS,NPF,TF,DYDX)
        IF(IRHO3/=0)RHO3 = FINTER(IRHO3,ABCS,NPF,TF,DYDX)
        IF(IE1/=0)E01 = FINTER(IE1,ABCS,NPF,TF,DYDX)
        IF(IE2/=0)E02 = FINTER(IE2,ABCS,NPF,TF,DYDX)
        IF(IE3/=0)E03 = FINTER(IE3,ABCS,NPF,TF,DYDX)
        IF(IVEL/=0)VEL_IN = VEL_IN*FINTER(IVEL,ABCS,NPF,TF,DYDX)

        K1 = M51_N0PHAS
        K2 = M51_N0PHAS+M51_NVPHAS
        K3 = M51_N0PHAS+2*M51_NVPHAS
        K4 = M51_N0PHAS+3*M51_NVPHAS

        DO I=1,NEL
          P1=UVAR(I,4)
          P2=UVAR(I,4)
          P3=UVAR(I,4)
          MU1=UVAR(I,K1+20)*RHO1/RHO10 - ONE
          IF(UVAR(I,K1+20)/=ZERO)THEN
            P1 = ( C01 + C11*MU1&
                 + MAX(MU1,ZERO)*(C21*MU1 + C31*MU1*MU1)&
                 + (C41 + C51*MU1)*E01*UVAR(I,K1+21)*RHO10/RHO1/UVAR(I,K1+20) )
          ENDIF
          P1 = MAX(P1,PM1)
          MU2=UVAR(I,K2+20)*RHO2/RHO20 - ONE
          IF(UVAR(I,K2+20)/=ZERO)THEN
            P2 = ( C02 + C12*MU2&
               + MAX(MU2,ZERO)*(C22*MU2 + C32*MU2*MU2)&
               + (C42 + C52*MU2)*E02*UVAR(I,K2+21)*RHO20/RHO2/UVAR(I,K2+20) )
          ENDIF
          P2 = MAX(P2,PM2)
          MU3=UVAR(I,K3+20)*RHO3/RHO30 - ONE
          IF(UVAR(I,K3+20)/=ZERO)THEN
            P3 = ( C03 + C13*MU3&
               + MAX(MU3,ZERO)*(C23*MU3 + C33*MU3*MU3)&
               + (C43 + C53*MU3)*E03*UVAR(I,K3+21)*RHO30/RHO3/UVAR(I,K3+20) )
          ENDIF
          P3 = MAX(P3,PM3)
          PP(I) = P1 * AV1(I) + P2 * AV2(I) + P3 * AV3(I)
        ENDDO

        DO I=1,NEL

          UVAR(I,1)  =  ZERO
          UVAR(I,2)  =  ZERO
          UVAR(I,3)  =  ZERO !BFRAC

          !====================!
          !     material 1     !
          !====================!
          KK = M51_N0PHAS
          UVAR(I,1+KK)  = AV1(I)
          UVAR(I,2+KK)  = ZERO
          UVAR(I,3+KK)  = ZERO
          UVAR(I,4+KK)  = ZERO
          UVAR(I,5+KK)  = ZERO
          UVAR(I,6+KK)  = ZERO
          UVAR(I,7+KK)  = ZERO
          UVAR(I,8+KK)  = E01*UVAR(I,K1+21)  !E0*AV1
          UVAR(I,9+KK)  = RHO1*UVAR(I,K1+20) !RHO1*V1
          UVAR(I,10+KK) = ZERO
          UVAR(I,11+KK) = VOLUME(I) * AV1(I)
          UVAR(I,12+KK) = RHO1*UVAR(I,K1+20)
          !------------------!
          UVAR(I,13+KK) = ZERO
          UVAR(I,14+KK) = ZERO
          UVAR(I,15+KK) = ZERO
          UVAR(I,16+KK) = ZERO
          UVAR(I,17+KK) = ZERO
          UVAR(I,18+KK) = ZERO
          UVAR(I,19+KK) = ZERO

          !====================!
          !     material 2     !
          !====================!
          KK = M51_N0PHAS + M51_NVPHAS
          UVAR(I,1+KK)  = AV2(I)
          UVAR(I,2+KK)  = ZERO
          UVAR(I,3+KK)  = ZERO
          UVAR(I,4+KK)  = ZERO
          UVAR(I,5+KK)  = ZERO
          UVAR(I,6+KK)  = ZERO
          UVAR(I,7+KK)  = ZERO
          UVAR(I,8+KK)  = E02*UVAR(I,K2+21)
          UVAR(I,9+KK)  = RHO2*UVAR(I,K2+20)
          UVAR(I,10+KK) = ZERO
          UVAR(I,11+KK) = VOLUME(I) * AV2(I)
          UVAR(I,12+KK) = RHO2*UVAR(I,K2+20)
          !------------------!
          UVAR(I,13+KK) = ZERO
          UVAR(I,14+KK) = ZERO
          UVAR(I,15+KK) = ZERO
          UVAR(I,16+KK) = ZERO
          UVAR(I,17+KK) = ZERO
          UVAR(I,18+KK) = ZERO
          UVAR(I,19+KK) = ZERO

          !====================!
          !     material 3     !
          !====================!
          KK = M51_N0PHAS + 2*M51_NVPHAS
          UVAR(I,1+KK)  = AV3(I)
          UVAR(I,2+KK)  = ZERO
          UVAR(I,3+KK)  = ZERO
          UVAR(I,4+KK)  = ZERO
          UVAR(I,5+KK)  = ZERO
          UVAR(I,6+KK)  = ZERO
          UVAR(I,7+KK)  = ZERO
          UVAR(I,8+KK)  = E03*UVAR(I,K3+21)
          UVAR(I,9+KK)  = RHO3*UVAR(I,K3+20)
          UVAR(I,10+KK) = ZERO
          UVAR(I,11+KK) = VOLUME(I) * AV3(I)
          UVAR(I,12+KK) = RHO3*UVAR(I,K3+20)
          !------------------!
          UVAR(I,13+KK) = ZERO
          UVAR(I,14+KK) = ZERO
          UVAR(I,15+KK) = ZERO
          UVAR(I,16+KK) = ZERO
          UVAR(I,17+KK) = ZERO
          UVAR(I,18+KK) = ZERO
          UVAR(I,19+KK) = ZERO

          !====================!
          !     material 4     !
          !====================!
          KK = M51_N0PHAS + 3*M51_NVPHAS
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
          UVAR(I,12+KK) = ZERO
          !------------------!
          UVAR(I,13+KK) = ZERO
          UVAR(I,14+KK) = ZERO
          UVAR(I,15+KK) = ZERO
          UVAR(I,16+KK) = ZERO
          UVAR(I,17+KK) = ZERO
          UVAR(I,18+KK) = ZERO
          UVAR(I,19+KK) = ZERO

          !==================================!
          !     ELEMENT STATE                !
          !==================================!
          RHO(I) = RHO1*UVAR(I,K1+20) * AV1(I) + RHO2*UVAR(I,K2+20) * AV2(I) + RHO3*UVAR(I,K3+20) * AV3(I)
          SOUNDSP(I) = EM20
          VISCMAX(I) = ZERO
          EEE        = VOLUME(I) * (E01*UVAR(I,K1+21) * AV1(I) + E02*UVAR(I,K2+21) * AV2(I) + E03*UVAR(I,K3+21) * AV3(I))
          WFEXTT      = ZERO !WFEXTT + EEE - EINT(I)
          EINT(I)    = EEE
          DD = -EPSPXX(I)-EPSPYY(I)-EPSPZZ(I)
          SIGNXX(I) = -PP(I)
          SIGNYY(I) = -PP(I)
          SIGNZZ(I) = -PP(I)

          IF(VEL_IN /= ZERO .OR. IVEL /= 0)THEN
            !normal face
            IF(N2D == 0)THEN
               II     = I+NFT
               IAD2 = ALE_CONNECT%ee_connect%iad_connect(II)
               DO  J=1,NV46
                IVOI  = ALE_CONNECT%ee_connect%connected(IAD2 + J - 1)
                ML    = 51
                IFORM = 1000
                IF(IVOI /= 0) ML = NINT(PM(19,IX(1,IVOI)))
                IF(IVOI /= 0) IADBUF = IPM(7,IX(1,IVOI))
                IF(IVOI /= 0 .AND. ML == 51) IFORM = NINT(BUFMAT(IADBUF+31-1))
                IF(ML == 51 .AND. IFORM <= 1)  EXIT
               ENDDO
               XN = ZERO
               YN = ZERO
               ZN = ZERO
               IF(ML == 51 .AND. IFORM <= 1)THEN
                IX1 = IX(ICF3D(1,J)+1,II)
                IX2 = IX(ICF3D(2,J)+1,II)
                IX3 = IX(ICF3D(3,J)+1,II)
                IX4 = IX(ICF3D(4,J)+1,II)
                X13 = X(1,IX3)-X(1,IX1)
                Y13 = X(2,IX3)-X(2,IX1)
                Z13 = X(3,IX3)-X(3,IX1)
                X24 = X(1,IX4)-X(1,IX2)
                Y24 = X(2,IX4)-X(2,IX2)
                Z24 = X(3,IX4)-X(3,IX2)
                XN  = -Y13*Z24+Z13*Y24
                YN  = -Z13*X24+X13*Z24
                ZN  = -X13*Y24+Y13*X24
                FAC = ONE/SQRT(XN**2+YN**2+ZN**2)
                XN  = XN*FAC
                YN  = YN*FAC
                ZN  = ZN*FAC
              ENDIF
            ELSE !IF(N2D==0)
              II     = I+NFT
              IAD2 = ALE_CONNECT%ee_connect%iad_connect(II)
              DO  J=1,NV46
               IVOI  =  ALE_CONNECT%ee_connect%connected(IAD2 + J - 1)
               ML    = 51
               IFORM = 1000
               IF(IVOI /= 0) ML = NINT(PM(19,IX(1,IVOI)))
               IF(IVOI /= 0) IADBUF = IPM(7,IX(1,IVOI))
               IF(IVOI /= 0 .AND. ML == 51) IFORM = NINT(BUFMAT(IADBUF+31-1))     !if adjacent is mat51 then retrieving UPARAM(31)=IFLG to check it is not a boundary material
               IF(ML == 51 .AND. IFORM <= 1)  EXIT
              ENDDO
              XN = ZERO
              YN = ZERO
              ZN = ZERO
              IF(ML == 51 .AND. IFORM <= 1)THEN
                IX1 = IX(ICF2D(1,J)+1,II)
                IX2 = IX(ICF2D(2,J)+1,II)
                XN   = ZERO
                YN   = (X(2,IX2)-X(2,IX1))
                ZN   = (X(3,IX2)-X(3,IX1))
                FAC  = ONE/SQRT(YN**2+ZN**2)
                YN  = YN*FAC
                ZN  = ZN*FAC
              ENDIF
            ENDIF
            ! sauvegarde de la quantite de mouvement imposee
            IF(ALEFVM_Param%IEnabled /= 0)THEN
              MOM = RHO(I) * VEL_IN * VOLUME(I)
              GBUF%MOM(NEL*(1-1)+I) = -MOM * XN
              GBUF%MOM(NEL*(2-1)+I) = -MOM * YN
              GBUF%MOM(NEL*(3-1)+I) = -MOM * ZN
            ELSE
              V(1,IX1) = VEL_IN * XN ; V(2,IX1) = VEL_IN * YN ; V(3,IX1) = VEL_IN * ZN
              V(1:3,IX2) =  V(1:3,IX1)
              IF(N2D == 0)THEN
                V(1:3,IX3) =  V(1:3,IX1)
                V(1:3,IX4) =  V(1:3,IX1)
              ENDIF
            ENDIF
          ENDIF

          VDX(I) = ZERO
          VDY(I) = ZERO
          VDZ(I) = ZERO

        ENDDO

#if defined(_OPENMP)
!$OMP ATOMIC
#endif
        WFEXT = WFEXT + WFEXTT

        RETURN

      !===========================================!
      ! LEGACY OUTLET - OLD VERSIONS (IFLG = 3)   !
      !===========================================!

      ELSEIF(IFLG == 3)THEN

        DO I=1,NEL
          PP0(I) = C01 * AV1(I) + C02 * AV2(I) + C03 * AV3(I)
        ENDDO

        IF(TIME == ZERO)THEN
          DO I=1,NEL
            SIGOXX(I) = -PP0(I)
            SIGOYY(I) = -PP0(I)
            SIGOZZ(I) = -PP0(I)
            SIGOXY(I) = ZERO
            SIGOYZ(I) = ZERO
            SIGOZX(I) = ZERO
            RHO(I) = RHO1 * AV1(I) + RHO2 * AV2(I) + RHO3 * AV3(I)
          ENDDO
        ENDIF


        DO I=1,NEL
          DD = -EPSPXX(I)-EPSPYY(I)-EPSPZZ(I)
          BBB = ONE - ALPHA
          IF(DD > ZERO)THEN
            SIGNXX(I) = SIGOXX(I)
            SIGNYY(I) = SIGOYY(I)
            SIGNZZ(I) = SIGOZZ(I)
            SIGNXY(I) = SIGOXY(I)
            SIGNYZ(I) = SIGOYZ(I)
            SIGNZX(I) = SIGOZX(I)
            IF(IOPT == 1)RHO(I) = ALPHA*(RHO1 * AV1(I) + RHO2 * AV2(I) + RHO3 * AV3(I))+ BBB*RHO(I)
          ELSE
            AAA = -ALPHA * PP0(I)
            SIGNXX(I) = BBB * SIGOXX(I) + AAA
            SIGNYY(I) = BBB * SIGOYY(I) + AAA
            SIGNZZ(I) = BBB * SIGOZZ(I) + AAA
            !IF(IOPT==0)BBB=ZERO
            SIGNXY(I) = BBB * SIGOXY(I)
            SIGNYZ(I) = BBB * SIGOYZ(I)
            SIGNZX(I) = BBB * SIGOZX(I)
          ENDIF
          UVAR(I,1)  =  ZERO
          UVAR(I,2)  =  ZERO
          UVAR(I,3)  =  ZERO !BFRAC

          !====================!
          !     material 1     !
          !====================!
          KK = M51_N0PHAS
          UVAR(I,1+KK)  = AV1(I)
          UVAR(I,8+KK)  = E01
          UVAR(I,9+KK)  = RHO1
          UVAR(I,10+KK) = ZERO
          UVAR(I,11+KK) = VOLUME(I) * AV1(I)
          UVAR(I,12+KK) = RHO1
          !------------------!
          UVAR(I,13+KK) = ZERO
          UVAR(I,14+KK) = ZERO
          UVAR(I,15+KK) = ZERO
          UVAR(I,16+KK) = ZERO
          UVAR(I,17+KK) = ZERO
          UVAR(I,18+KK) = ZERO
          UVAR(I,19+KK) = ZERO

          !====================!
          !     material 2     !
          !====================!
          KK = M51_N0PHAS + M51_NVPHAS
          UVAR(I,1+KK)  = AV2(I)
          UVAR(I,8+KK)  = E02
          UVAR(I,9+KK)  = RHO2
          UVAR(I,10+KK) = ZERO
          UVAR(I,11+KK) = VOLUME(I) * AV2(I)
          UVAR(I,12+KK) = RHO2
          !------------------!
          UVAR(I,13+KK) = ZERO
          UVAR(I,14+KK) = ZERO
          UVAR(I,15+KK) = ZERO
          UVAR(I,16+KK) = ZERO
          UVAR(I,17+KK) = ZERO
          UVAR(I,18+KK) = ZERO
          UVAR(I,19+KK) = ZERO

          !====================!
          !     material 3     !
          !====================!
          KK = M51_N0PHAS + 2*M51_NVPHAS
          UVAR(I,1+KK)  = AV3(I)
          UVAR(I,8+KK)  = E03
          UVAR(I,9+KK)  = RHO3
          UVAR(I,10+KK) = ZERO
          UVAR(I,11+KK) = VOLUME(I) * AV3(I)
          UVAR(I,12+KK) = RHO3
          !------------------!
          UVAR(I,13+KK) = ZERO
          UVAR(I,14+KK) = ZERO
          UVAR(I,15+KK) = ZERO
          UVAR(I,16+KK) = ZERO
          UVAR(I,17+KK) = ZERO
          UVAR(I,18+KK) = ZERO
          UVAR(I,19+KK) = ZERO

          !====================!
          !     material 4     !
          !====================!
          KK = M51_N0PHAS + 3*M51_NVPHAS
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
          UVAR(I,12+KK) = ZERO
          !------------------!
          UVAR(I,13+KK) = ZERO
          UVAR(I,14+KK) = ZERO
          UVAR(I,15+KK) = ZERO
          UVAR(I,16+KK) = ZERO
          UVAR(I,17+KK) = ZERO
          UVAR(I,18+KK) = ZERO
          UVAR(I,19+KK) = ZERO

          !==================================!
          !     ELEMENT  OUTPUT  (IFLG == 3) !
          !==================================!
          IF(IOPT == 0)RHO(I) = RHO1 * AV1(I) + RHO2 * AV2(I) + RHO3 * AV3(I)
          EEE        = VOLUME(I) * (E01 * AV1(I) + E02 * AV2(I) + E03 * AV3(I))
          WFEXTT     = ZERO ! WFEXTT + EEE - EINT(I)
          EINT(I)    = EEE
          SOUNDSP(I) = EM20
          VISCMAX(I) = ZERO

          VDX(I) = ZERO
          VDY(I) = ZERO
          VDZ(I) = ZERO

        ENDDO

#if defined(_OPENMP)
!$OMP ATOMIC
#endif
        WFEXT = WFEXT + WFEXTT

        RETURN

      !===========================================!
      ! INLET + STAGNATION                        !
      !   GAS    (IFLG = 4)                       !
      !   LIQUID (IFLG = 5)                       !
      !===========================================!
      ELSEIF(IFLG == 4 .OR. IFLG == 5)THEN
        ! Conditions d'arret pour gaz parfait ou liquide selon la phase
        ABCS  = TIME / UPARAM(38)
        IAV1  = IFUNC(1)
        IRHO1 = IFUNC(2)
        IE1   = IFUNC(3)
        IAV2  = IFUNC(4)
        IRHO2 = IFUNC(5)
        IE2   = IFUNC(6)
        IAV3  = IFUNC(7)
        IRHO3 = IFUNC(8)
        IE3   = IFUNC(9)
        IF(IAV1 /= 0)THEN
          myVAR = FINTER(IAV1,ABCS,NPF,TF,DYDX)
          AV1(1:NEL) = myVAR * AV1(1:NEL)
        ENDIF
        IF(IAV2 /= 0)THEN
          myVAR = FINTER(IAV2,ABCS,NPF,TF,DYDX)
          AV2(1:NEL) = myVAR * AV2(1:NEL)
        ENDIF
        IF(IAV3 /= 0)THEN
          myVAR = FINTER(IAV3,ABCS,NPF,TF,DYDX)
          AV3(1:NEL) = myVAR * AV3(1:NEL)
        ENDIF
        K1 = M51_N0PHAS
        K2 = M51_N0PHAS+M51_NVPHAS
        K3 = M51_N0PHAS+2*M51_NVPHAS
        K4 = M51_N0PHAS+3*M51_NVPHAS
        RHO1f = ONE
        RHO2f = ONE
        RHO3f = ONE
          E01f  = ONE
          E02f  = ONE
          E03f  = ONE
        IF(IRHO1 /= 0)RHO1f = FINTER(IRHO1,ABCS,NPF,TF,DYDX)
        IF(IRHO2 /= 0)RHO2f = FINTER(IRHO2,ABCS,NPF,TF,DYDX)
        IF(IRHO3 /= 0)RHO3f = FINTER(IRHO3,ABCS,NPF,TF,DYDX)
        IF(IE1 /= 0)E01f = FINTER(IE1,ABCS,NPF,TF,DYDX)
        IF(IE2 /= 0)E02f = FINTER(IE2,ABCS,NPF,TF,DYDX)
        IF(IE3 /= 0)E03f = FINTER(IE3,ABCS,NPF,TF,DYDX)
        DO I=1,NEL
          E01=E01f
          E02=E02f
          E03=E03f
          RHO1=RHO1f
          RHO2=RHO2f
          RHO3=RHO3f
          MU1=RHO1*UVAR(I,20+K1)/RHO10 - ONE
          P1 = ( C01 + C11*MU1+ MAX(MU1,ZERO)*(C21*MU1 + C31*MU1*MU1) + (C41 + C51*MU1)*E01*UVAR(I,21+K1)*RHO10/RHO1/UVAR(I,20+K1) )
          P1 = MAX(P1,PM1)
          MU2=RHO2*UVAR(I,20+K2)/RHO20 - ONE
          P2 = ( C02 + C12*MU2+ MAX(MU2,ZERO)*(C22*MU2 + C32*MU2*MU2) + (C42 + C52*MU2)*E02*UVAR(I,21+K2)*RHO20/RHO2/UVAR(I,20+K2) )
          P2 = MAX(P2,PM2)
          MU3=RHO3*UVAR(I,20+K3)/RHO30 - ONE
          P3 = ( C03 + C13*MU3 + MAX(MU3,ZERO)*(C23*MU3 + C33*MU3*MU3)+ (C43 + C53*MU3)*E03*UVAR(I,21+K3)*RHO30/RHO3/UVAR(I,20+K3) )
          P3 = MAX(P3,PM3)
          !switching to total pressure formulation since following lines are based on it.
          P1 = PEXT + P1
          P2 = PEXT + P2
          P3 = PEXT + P3
          !stagnation point
          RHO1A = RHO1*UVAR(I,20+K1)
          P1A = P1
          E01A = E01*UVAR(I,21+K1)
          RHO2A = RHO2*UVAR(I,20+K2)
          P2A = P2
          E02A = E02*UVAR(I,21+K2)
          RHO3A = RHO3*UVAR(I,20+K3)
          P3A = P3
          E03A = E03*UVAR(I,21+K3)
          CC1 = (C41+ONE)*P1/RHO1/UVAR(I,20+K1)
          CC2 = (C42+ONE)*P2/RHO2/UVAR(I,20+K2)
          CC3 = (C43+ONE)*P3/RHO3/UVAR(I,20+K3)
          AA1 = TWO / (C41+TWO)
          AA2 = TWO / (C42+TWO)
          AA3 = TWO / (C43+TWO)
          VCRT1 = AA1*CC1
          VCRT2 = AA2*CC2
          VCRT3 = AA3*CC3
          !------------
          UVAR(I,1:M51_N0PHAS)  =  ZERO
          II = I + NFT
          VN = ZERO
          U2 = ZERO
          X0 = ZERO
          Y0 = ZERO
          Z0 = ZERO
          DO KK=1,N48
            K  = IX(1+KK,II)
            X0 = X0 + X(1,K)
            Y0 = Y0 + X(2,K)
            Z0 = Z0 + X(3,K)
          ENDDO
          X0 = X0 / N48
          Y0 = Y0 / N48
          Z0 = Z0 / N48
          DO KK=1,N48
            K  = IX(1+KK,II)
            VX = V(1,K)-W(1,K)
            VY = V(2,K)-W(2,K)
            VZ = V(3,K)-W(3,K)
            U2 = U2 + VX*VX + VY*VY + VZ*VZ
            NX = X(1,K)-X0
            NY = X(2,K)-Y0
            NZ = X(3,K)-Z0
            VN = VN + VX*NX + VY*NY + VZ*NZ
          ENDDO
          U2 = U2 / (N48/2)
          IF(VN <= ZERO) U2 = ZERO

          !===============================!
          !     material 1                !
          !===============================!
          IF(AV1(I) > ZERO)THEN
            !---GAZ
            IF((IFLG == 4.and.C41 /= ZERO).or. (IFLG == 5 .AND. C41 /= ZERO .AND. C11 == ZERO))THEN
              U21 = MIN(U2,VCRT1)
              GV1 = C41/2/(C41+1)*RHO1A*U21/P1A
              IF(GV1 > EM4)THEN
                AAA  = ONE - GV1
                RHO1 = RHO1A * AAA**(ONE/C41)
                BBB  = AAA**((C41+ONE)/C41)
                E01  = E01A * BBB
                P1   = P1A * BBB
                !yann     test sur 0.0001 pour cas incompressible (Bernouilli classique)
              ELSE
                RV1  = HALF*RHO1A*U21
                RHO1 = RHO1A *(ONE- RV1/(C41+ONE)/P1A)
                P1   = P1A - RV1
                E01  = P1/C41
              ENDIF
            !---LIQUID
            ELSE
              FAC = (C11 + HALF*RHO10*U2)
              IF(FAC /= ZERO)THEN
                RHO1 = RHO1A*C11/FAC
                P1   = P1A - HALF*RHO1*U2
                E01  = E01A + (ONE - RHO1/RHO1A)*P1
              ENDIF
            ENDIF
          ELSE
           !unused submaterial
           RHO1 = RHO10
          ENDIF! (AV1(I) > ZERO)
          !================!
          !     output     !
          !================!
          KK = M51_N0PHAS
          UVAR(I,1+KK)  = AV1(I)
          UVAR(I,2+KK)  = ZERO
          UVAR(I,3+KK)  = ZERO
          UVAR(I,4+KK)  = ZERO
          UVAR(I,5+KK)  = ZERO
          UVAR(I,6+KK)  = ZERO
          UVAR(I,7+KK)  = ZERO
          UVAR(I,8+KK)  = E01
          UVAR(I,9+KK)  = RHO1
          UVAR(I,10+KK) = ZERO
          UVAR(I,11+KK) = VOLUME(I) * AV1(I)
          UVAR(I,12+KK) = RHO1
          !------------------!
          UVAR(I,13+KK) = ZERO
          UVAR(I,14+KK) = ZERO
          UVAR(I,15+KK) = ZERO
          UVAR(I,16+KK) = ZERO
          UVAR(I,17+KK) = ZERO
          UVAR(I,18+KK) = ZERO
          UVAR(I,19+KK) = ZERO

          !===============================!
          !     material 2                !
          !===============================!
          IF(AV2(I) > ZERO)THEN
            IF((IFLG == 4.and.C42 /= ZERO).or. (IFLG == 5 .AND. C42 /= ZERO .AND. C12 == ZERO))THEN
              U22 = MIN(U2,VCRT2)
              GV2 = C42/2/(C42+1)*RHO2A*U22/P2A
              IF(GV2 > EM4)THEN
                AAA  = ONE - GV2
                RHO2 = RHO2A * AAA**(ONE/C42)
                BBB  = AAA**((C42+ONE)/C42)
                E02  = E02A * BBB
                P2   = P2A * BBB
              ELSE
                RV2  = HALF*RHO2A*U22
                RHO2 = RHO2A *(ONE- RV2/(C42+ONE)/P2A)
                P2   = P2A - RV2
                E02  = P2/C42
              ENDIF
            ELSE
              FAC = (C12 + HALF*RHO20*U2)
              IF(FAC /= ZERO)THEN
                RHO2 = RHO2A*C12/FAC
                P2   = P2A - HALF*RHO2*U2
                E02  = E02A + (ONE - RHO2/RHO2A)*P2
              ENDIF
            ENDIF
          ELSE
           !unused submaterial
           RHO2 = RHO20
          ENDIF! (AV2(I) > ZERO)
          !================!
          !     output     !
          !================!
          KK = M51_N0PHAS + M51_NVPHAS
          UVAR(I,1+KK)  = AV2(I)
          UVAR(I,2+KK)  = ZERO
          UVAR(I,3+KK)  = ZERO
          UVAR(I,4+KK)  = ZERO
          UVAR(I,5+KK)  = ZERO
          UVAR(I,6+KK)  = ZERO
          UVAR(I,7+KK)  = ZERO
          UVAR(I,8+KK)  = E02
          UVAR(I,9+KK)  = RHO2
          UVAR(I,10+KK) = ZERO
          UVAR(I,11+KK) = VOLUME(I) * AV2(I)
          UVAR(I,12+KK) = RHO2
          !------------------!
          UVAR(I,13+KK) = ZERO
          UVAR(I,14+KK) = ZERO
          UVAR(I,15+KK) = ZERO
          UVAR(I,16+KK) = ZERO
          UVAR(I,17+KK) = ZERO
          UVAR(I,18+KK) = ZERO
          UVAR(I,19+KK) = ZERO

          !===============================!
          !     material 3                !
          !===============================!
          IF(AV3(I) > ZERO)THEN
            IF((IFLG == 4 .AND. C43 /= ZERO) .OR. (IFLG == 5 .AND. C43 /= ZERO .AND. C13 == ZERO))THEN
              U23 = MIN(U2,VCRT3)
              GV3 = C43/2/(C43+1)*RHO3A*U23/P3A
              IF(GV3 > EM4)THEN
                AAA  = ONE - GV3
                RHO3 = RHO3A * AAA**(ONE/C43)
                BBB  = AAA**((C43+ONE)/C43)
                E03  = E03A * BBB
                P3   = P3A * BBB
              ELSE
                RV3  = HALF*RHO3A*U23
                RHO3 = RHO3A *(ONE- RV3/(C43+ONE)/P3A)
                P3   = P3A - RV3
                E03  = P3/C43
              ENDIF
            ELSE
              FAC = (C13 + HALF*RHO30*U2)
              IF(FAC /= ZERO)THEN
                RHO3 = RHO3A*C13/FAC
                P3   = P3A - HALF*RHO3*U2
                E03  = E03A + (ONE - RHO3/RHO3A)*P3
              ENDIF
            ENDIF
          ELSE
            !unused submaterial
            RHO3 = RHO30
          ENDIF! (AV3(I) > ZERO)
          !================!
          !     output     !
          !================!
          KK = M51_N0PHAS + 2*M51_NVPHAS
          UVAR(I,1+KK)  = AV3(I)
          UVAR(I,2+KK)  = ZERO
          UVAR(I,3+KK)  = ZERO
          UVAR(I,4+KK)  = ZERO
          UVAR(I,5+KK)  = ZERO
          UVAR(I,6+KK)  = ZERO
          UVAR(I,7+KK)  = ZERO
          UVAR(I,8+KK)  = E03
          UVAR(I,9+KK)  = RHO3
          UVAR(I,10+KK) = ZERO
          UVAR(I,11+KK) = VOLUME(I) * AV3(I)
          UVAR(I,12+KK) = RHO3

          UVAR(I,13+KK) = ZERO
          UVAR(I,14+KK) = ZERO
          UVAR(I,15+KK) = ZERO
          UVAR(I,16+KK) = ZERO
          UVAR(I,17+KK) = ZERO
          UVAR(I,18+KK) = ZERO
          UVAR(I,19+KK) = ZERO
          !===============================!
          !     material 4                !
          !===============================!
          KK = M51_N0PHAS + 3*M51_NVPHAS
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
          UVAR(I,12+KK) = ZERO
          !------------------!
          UVAR(I,13+KK) = ZERO
          UVAR(I,14+KK) = ZERO
          UVAR(I,15+KK) = ZERO
          UVAR(I,16+KK) = ZERO
          UVAR(I,17+KK) = ZERO
          UVAR(I,18+KK) = ZERO
          UVAR(I,19+KK) = ZERO

          !================================================!
          !     ELEMENT  OUTPUT  (IFLG == 4.or.IFLG == 5)  !
          !================================================!
          RHO(I) = RHO1 * AV1(I) + RHO2 * AV2(I) + RHO3 * AV3(I)
          SOUNDSP(I) = EM20
          VISCMAX(I) = ZERO
          EEE        = VOLUME(I) * (E01 * AV1(I) + E02 * AV2(I) + E03 * AV3(I))
          WFEXTT     = ZERO !WFEXTT + EEE - EINT(I)
          EINT(I)    = EEE
          P1 = P1 - PEXT
          P2 = P2 - PEXT
          P3 = P3 - PEXT
          P  = P1 * AV1(I) + P2 * AV2(I) + P3 * AV3(I)
          SIGNXX(I) = -P
          SIGNYY(I) = -P
          SIGNZZ(I) = -P

          VDX(I) = ZERO
          VDY(I) = ZERO
          VDZ(I) = ZERO

        ENDDO  !DO I=1,NEL

#if defined(_OPENMP)
!$OMP ATOMIC
#endif
        WFEXT = WFEXT + WFEXTT

        RETURN

      !===========================================!
      ! OUTLET NRF (IFLG = 6)                     !
      !===========================================!

      ELSEIF(IFLG == 6)THEN

        IF(TIME == ZERO) THEN
          DO I=1,NEL
            P0_NRF = UVAR(I,4)
            !
            KK     = M51_N0PHAS + 0*M51_NVPHAS
            RHO1   = UVAR(I,KK+20)
            E01    = UVAR(I,KK+21)
            SSP1   = UVAR(I,KK+22)
            !
            KK     = M51_N0PHAS + 1*M51_NVPHAS
            RHO2   = UVAR(I,KK+20)
            E02    = UVAR(I,KK+21)
            SSP2   = UVAR(I,KK+22)
            !
            KK     = M51_N0PHAS + 2*M51_NVPHAS
            RHO3   = UVAR(I,KK+20)
            E03    = UVAR(I,KK+21)
            SSP3   = UVAR(I,KK+22)
            !
            KK     = M51_N0PHAS + 3*M51_NVPHAS
            RHO4   = UVAR(I,KK+20)
            E04    = UVAR(I,KK+21)
            SSP4   = UVAR(I,KK+22)
            !----------------------!
            SIGNXX(I)     = -P0_NRF
            SIGNYY(I)     = -P0_NRF
            SIGNZZ(I)     = -P0_NRF
            SIGNXY(I)     = ZERO
            SIGNYZ(I)     = ZERO
            SIGNZX(I)     = ZERO
            !----------------------!
            SIGOXX(I)     = -P0_NRF
            SIGOYY(I)     = -P0_NRF
            SIGOZZ(I)     = -P0_NRF
            SIGOXY(I)     = ZERO
            SIGOYZ(I)     = ZERO
            SIGOZX(I)     = ZERO
            !----------------------!
            RHO(I)        = AV1(I)*RHO1+ AV2(I)*RHO2+ AV3(I)*RHO3                     !not relevant to add explosive here
            EINT(I)       = VOLUME(I) * (E01 * AV1(I) + E02 * AV2(I) + E03 * AV3(I))  !not relevant to add explosive here

            KK = M51_N0PHAS
            UVAR(I,1+KK)  = AV1(I)
            UVAR(I,8+KK)  = E01
            UVAR(I,9+KK)  = RHO1
            UVAR(I,10+KK) = ZERO
            UVAR(I,11+KK) = VOLUME(I) * AV1(I)
            UVAR(I,12+KK) = RHO1
            UVAR(I,13+KK) = ZERO
            UVAR(I,14+KK) = SSP1
            UVAR(I,15+KK) = ZERO
            UVAR(I,16+KK) = ZERO
            UVAR(I,17+KK) = ZERO
            UVAR(I,18+KK) = P0_NRF
            UVAR(I,19+KK) = ZERO

            KK = M51_N0PHAS + M51_NVPHAS
            UVAR(I,1+KK)  = AV2(I)
            UVAR(I,8+KK)  = E02
            UVAR(I,9+KK)  = RHO2
            UVAR(I,10+KK) = ZERO
            UVAR(I,11+KK) = VOLUME(I) * AV2(I)
            UVAR(I,12+KK) = RHO2
            UVAR(I,13+KK) = ZERO
            UVAR(I,14+KK) = SSP2
            UVAR(I,15+KK) = ZERO
            UVAR(I,16+KK) = ZERO
            UVAR(I,17+KK) = ZERO
            UVAR(I,18+KK) = P0_NRF
            UVAR(I,19+KK) = ZERO

            KK = M51_N0PHAS + 2*M51_NVPHAS
            UVAR(I,1+KK)  = AV3(I)
            UVAR(I,8+KK)  = E03
            UVAR(I,9+KK)  = RHO3
            UVAR(I,10+KK) = ZERO
            UVAR(I,11+KK) = VOLUME(I) *  AV3(I)
            UVAR(I,12+KK) = RHO3
            !------------------!
            UVAR(I,13+KK) = ZERO
            UVAR(I,14+KK) = SSP3
            UVAR(I,15+KK) = ZERO
            UVAR(I,16+KK) = ZERO
            UVAR(I,17+KK) = ZERO
            UVAR(I,18+KK) = P0_NRF
            UVAR(I,19+KK) = ZERO

            KK = M51_N0PHAS + 3*M51_NVPHAS
            UVAR(I,1+KK)  = AV4(I)
            UVAR(I,8+KK)  = E04
            UVAR(I,9+KK)  = RHO4
            UVAR(I,10+KK) = ZERO
            UVAR(I,11+KK) = VOLUME(I) *  AV4(I)
            UVAR(I,12+KK) = RHO4
            !------------------!
            UVAR(I,13+KK) = ZERO
            UVAR(I,14+KK) = SSP4
            UVAR(I,15+KK) = ZERO
            UVAR(I,16+KK) = ZERO
            UVAR(I,17+KK) = ZERO
            UVAR(I,18+KK) = P0_NRF
            UVAR(I,19+KK) = ZERO

          ENDDO
        ENDIF

        IF(TIMESTEP > EM20)THEN
          BETA = TIMESTEP/MAX(UPARAM(71),TIMESTEP)
          IF(UPARAM(71) >= EP20)BETA=ZERO  ! default : Tcalpha=EP20 => BETA = 0
          ALPHA = TIMESTEP/UPARAM(70)
        ELSE
          BETA = ONE
          ALPHA = ONE
        ENDIF

        !RHOC2(1)  = MAX(RHO1*SSP1*SSP1,EM20)
        !RHOC2(2)  = MAX(RHO2*SSP2*SSP2,EM20)
        !RHOC2(3)  = MAX(RHO3*SSP3*SSP3,EM20)
        !RHOC2(4)  = MAX(RHO4*SSP4*SSP4,EM20)

        STIFN(1:NEL) = ZERO

        !     Search adjacent quantities
        !     and comput enormal velocity at boundary face
        IF(N2D == 0)THEN
          CALL M51VOIS3(PM   ,  IPARG  ,IX      ,ALE_CONNECT   ,ELBUF_TAB ,V    , &
                        X    ,  VEL_N  ,W       ,VEL     ,VD2  , &
                        RHOV ,  PV     ,VDX     ,VDY     ,VDZ  , &
                        EIV  ,  TV     ,BUFVOIS ,AVV     ,RHO0V, &
                        IPM    ,BUFMAT ,NEL     , &
                        NV46 ,  SSPv   ,EPSPv   ,P0_NRFv)
        ELSE
          CALL M51VOIS2(PM   ,  IPARG  ,IX      ,ALE_CONNECT   ,ELBUF_TAB ,V    , &
                        X    ,  VEL_N  ,W       ,VEL           ,VD2  , &
                        RHOV ,  PV     ,VDX     ,VDY           ,VDZ  , &
                        EIV  ,  TV     ,BUFVOIS ,AVV           ,RHO0V, &
                        IPM  ,  BUFMAT ,NEL     , &
                        NV46 ,  SSPv   ,EPSPv   ,P0_NRFv )
        ENDIF

        IF(TIME == ZERO) THEN
         DO I=1,NEL
           VEL_O(I)=VEL_N(I)
           P0_NRF = UVAR(I,4)
           KK           = M51_N0PHAS
           UVAR(I,1+KK) = AVV(1,I)
           AV1(I)       = AVV(1,I)
           KK           = M51_N0PHAS + M51_NVPHAS
           UVAR(I,1+KK) = AVV(2,I)
           AV2(I)       = AVV(2,I)
           KK           = M51_N0PHAS + 2*M51_NVPHAS
           UVAR(I,1+KK) = AVV(3,I)
           AV3(I)       = AVV(3,I)
           KK           = M51_N0PHAS + 3*M51_NVPHAS
           UVAR(I,1+KK) = AVV(4,I)
           AV4(I)       = AVV(4,I)
           SIGOXX(I)    = -P0_NRF
           SIGOYY(I)    = -P0_NRF
           SIGOZZ(I)    = -P0_NRF
           SIGOXY(I)    = ZERO
           SIGOYZ(I)    = ZERO
           SIGOZX(I)    = ZERO
         ENDDO
        ENDIF
        DO I=1,NEL
          P0_NRF = UVAR(I,4)
          !
          KK     = M51_N0PHAS + 0*M51_NVPHAS
          RHO1   = UVAR(I,KK+20)
          E01    = UVAR(I,KK+21)
          SSP1   = UVAR(I,KK+22)
          !
          KK     = M51_N0PHAS + 1*M51_NVPHAS
          RHO2   = UVAR(I,KK+20)
          E02    = UVAR(I,KK+21)
          SSP2   = UVAR(I,KK+22)
          !
          KK     = M51_N0PHAS + 2*M51_NVPHAS
          RHO3   = UVAR(I,KK+20)
          E03    = UVAR(I,KK+21)
          SSP3   = UVAR(I,KK+22)
          !
          KK     = M51_N0PHAS + 3*M51_NVPHAS
          RHO4   = UVAR(I,KK+20)
          E04    = UVAR(I,KK+21)
          SSP4   = UVAR(I,KK+22)

          IF( SUM(AVV(1:4,I)) == ZERO) THEN !corner element
            RHO(I)     = EM20
            SOUNDSP(I) = EM20
            EINT(I)    = ZERO
            P          = ZERO
            EPSPv(0,I) = ZERO
            EEE        = ZERO
          ELSE
            RHOC2(1)  = MAX(EM20,RHOV(1,I)*SSPV(1,I)*SSPV(1,I))
            RHOC2(2)  = MAX(EM20,RHOV(2,I)*SSPV(2,I)*SSPV(2,I))
            RHOC2(3)  = MAX(EM20,RHOV(3,I)*SSPV(3,I)*SSPV(3,I))
            RHOC2(4)  = MAX(EM20,RHOV(4,I)*SSPV(4,I)*SSPV(4,I))
            RHOC20    = ZERO
            IF(AVV(1,I)>EM06)RHOC20=RHOC20 + AVV(1,I)/RHOC2(1)
            IF(AVV(2,I)>EM06)RHOC20=RHOC20 + AVV(2,I)/RHOC2(2)
            IF(AVV(3,I)>EM06)RHOC20=RHOC20 + AVV(3,I)/RHOC2(3)
            IF(AVV(4,I)>EM06)RHOC20=RHOC20 + AVV(4,I)/RHOC2(4)
            RHOC20=ONE/RHOC20 !average stiffness
            RHO(I) = AVV(1,I)*RHOV(1,I)+ AVV(2,I)*RHOV(2,I)+ AVV(3,I)*RHOV(3,I) + AVV(4,I)*RHOV(4,I)
            SOUNDSP(I)  = SSPV(0,I)
            MACH        = VEL_N(I) / SOUNDSP(I)
            ISUPERSONIC = 0
            IF(MACH>=ONE .AND. VEL_N(I)>ZERO)THEN
             !outgoing supersonic velocity : state = adjacent state
              ISUPERSONIC = 1
              P           = PV(0,I)
              EINT(I)     = EIV(0,I)
              RHO(I)      = RHOV(0,I)
            ELSE
              ROC   =SQRT(RHO(I)*RHOC20)
              POLD  = -THIRD*(SIGOXX(I)+SIGOYY(I)+SIGOZZ(I))
              DP0   = (P0_NRF-P0_NRFv(I)) ! assuming rho0*g(t)*(z-zvois) = cst ; otherwise complicated: get g(t) and compute z-zvois
              P     = ONE/(ONE+ALPHA)*(POLD+ROC*(VEL_N(I)-VEL_O(I)))+ALPHA*(PV(0,I)+DP0)/(ALPHA + ONE)
            ENDIF

            IF(VEL_N(I)<ZERO)THEN     !incoming flux. relaxation of volume fraction
               KK = M51_N0PHAS
               AVV(1,I)  = (ONE-BETA)*UVAR(I,1+KK) + BETA* AV1(I)
               KK = M51_N0PHAS + M51_NVPHAS
               AVV(2,I)  = (ONE-BETA)*UVAR(I,1+KK) + BETA* AV2(I)
               KK = M51_N0PHAS + 2*M51_NVPHAS
               AVV(3,I)  = (ONE-BETA)*UVAR(I,1+KK) + BETA* AV3(I)
               KK = M51_N0PHAS + 3*M51_NVPHAS
               AVV(4,I)  = (ONE-BETA)*UVAR(I,1+KK) + BETA* AV4(I)
            ELSE

            ENDIF
          ENDIF
          EEE       = EIV(0,I) * VOLUME(I)
          RHO(I)    = RHOV(0,I)
          SIGNXX(I) = -P
          SIGNYY(I) = SIGNXX(I)
          SIGNZZ(I) = SIGNXX(I)
          SIGNXY(I) = ZERO
          SIGNYZ(I) = ZERO
          SIGNZX(I) = ZERO
          VEL_O(I)  = VEL_N(I)
          SIGOXX(I) = SIGNXX(I)
          SIGOYY(I) = SIGNYY(I)
          SIGOZZ(I) = SIGNZZ(I)
          SIGOXY(I) = SIGNXY(I)
          SIGOYZ(I) = SIGNYZ(I)
          SIGOZX(I) = SIGNZX(I)

          IF(BUFLY%L_PLA>0)THEN
            LBUF%PLA(I) = EPSPv(0,I)
            UVAR(I,1)  =  EPSPv(0,I)
          ENDIF
          UVAR(I,2) = ZERO
          UVAR(I,3) = ZERO

          !====================!
          !     material 1     !
          !====================!
          KK = M51_N0PHAS
          UVAR(I,1+KK)  = AVV(1,I)
          UVAR(I,8+KK)  = EIV(1,I)
          UVAR(I,9+KK)  = RHOV(1,I)
          UVAR(I,10+KK) = ZERO
          UVAR(I,11+KK) = VOLUME(I) * AVV(1,I)
          UVAR(I,12+KK) = RHOV(1,I)
          !------------------!
          UVAR(I,13+KK) = ZERO
          UVAR(I,14+KK) = SSPV(1,I)
          UVAR(I,15+KK) = EPSPv(1,I)
          UVAR(I,16+KK) = TV(1,I)
          UVAR(I,17+KK) = ZERO
          UVAR(I,18+KK) = P0_NRF
          UVAR(I,19+KK) = ZERO

          !====================!
          !     material 2     !
          !====================!
          KK = M51_N0PHAS + M51_NVPHAS
          UVAR(I,1+KK)  = AVV(2,I)
          UVAR(I,8+KK)  = EIV(2,I)
          UVAR(I,9+KK)  = RHOV(2,I)
          UVAR(I,10+KK) = ZERO
          UVAR(I,11+KK) = VOLUME(I) * AVV(2,I)
          UVAR(I,12+KK) = RHOV(2,I)
          !------------------!
          UVAR(I,13+KK) = ZERO
          UVAR(I,14+KK) = SSPV(2,I)
          UVAR(I,15+KK) = EPSPv(2,I)
          UVAR(I,16+KK) = TV(2,I)
          UVAR(I,17+KK) = ZERO
          UVAR(I,18+KK) = P0_NRF
          UVAR(I,19+KK) = ZERO

          !====================!
          !     material 3     !
          !====================!
          KK = M51_N0PHAS + 2*M51_NVPHAS
          UVAR(I,1+KK)  = AVV(3,I)
          UVAR(I,8+KK)  = EIV(3,I)
          UVAR(I,9+KK)  = RHOV(3,I)
          UVAR(I,10+KK) = ZERO
          UVAR(I,11+KK) = VOLUME(I) * AVV(3,I)
          UVAR(I,12+KK) = RHOV(3,I)
          !------------------!
          UVAR(I,13+KK) = ZERO
          UVAR(I,14+KK) = SSPV(3,I)
          UVAR(I,15+KK) = EPSPv(3,I)
          UVAR(I,16+KK) = TV(3,I)
          UVAR(I,17+KK) = ZERO
          UVAR(I,18+KK) = P0_NRF
          UVAR(I,19+KK) = ZERO

          !====================!
          !     material 4     !
          !====================!
          KK = M51_N0PHAS + 3*M51_NVPHAS
          UVAR(I,1+KK)  = AVV(4,I)
          UVAR(I,8+KK)  = EIV(4,I)
          UVAR(I,9+KK)  = RHOV(4,I)
          UVAR(I,10+KK) = ZERO
          UVAR(I,11+KK) = VOLUME(I) * AVV(4,I)
          UVAR(I,12+KK) = RHOV(4,I)
          !------------------!
          UVAR(I,13+KK) = ZERO
          UVAR(I,14+KK) = SSPV(4,I)
          UVAR(I,15+KK) = ZERO          !no plasticity with detonation products
          UVAR(I,16+KK) = TV(4,I)
          UVAR(I,17+KK) = ZERO
          UVAR(I,18+KK) = P0_NRF
          UVAR(I,19+KK) = ZERO

          WFEXTT = ZERO !WFEXTT + EEE - EINT(I)
          EINT(I) = EEE
          VISCMAX(I) = ZERO
        ENDDO

#if defined(_OPENMP)
!$OMP ATOMIC
#endif
        WFEXT = WFEXT + WFEXTT

        RETURN

      ENDIF ! IFLG

      end subroutine sigeps51_boundary_material

      end module sigeps51_boundary_material_mod
