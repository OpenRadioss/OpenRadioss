Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    eosmain_mod        ../common_source/eos/eosmain.F
!||--- called by ------------------------------------------------------
!||    ebcs8_inlet        ../engine/source/boundary_conditions/ebcs/ebcs8_inlet.F90
!||    inigrav_eos        ../starter/source/initial_conditions/inigrav/inigrav_eos.F
!||    m12law             ../engine/source/materials/mat/mat012/m12law.F
!||    m51init            ../starter/source/materials/mat/mat051/m51init.F
!||    mmain              ../engine/source/materials/mat_share/mmain.F90
!||    multi_solve_eint   ../engine/source/multifluid/multi_solve_eint.F90
!||    multi_submatlaw    ../engine/source/multifluid/multi_submatlaw.F
!||    sigeps75           ../engine/source/materials/mat/mat075/sigeps75.F
!||====================================================================
      MODULE EOSMAIN_MOD
      contains
!||====================================================================
!||    eosmain              ../common_source/eos/eosmain.F
!||--- called by ------------------------------------------------------
!||    ebcs8_inlet          ../engine/source/boundary_conditions/ebcs/ebcs8_inlet.F90
!||    inigrav_eos          ../starter/source/initial_conditions/inigrav/inigrav_eos.F
!||    m12law               ../engine/source/materials/mat/mat012/m12law.F
!||    m51init              ../starter/source/materials/mat/mat051/m51init.F
!||    mmain                ../engine/source/materials/mat_share/mmain.F90
!||    multi_solve_eint     ../engine/source/multifluid/multi_solve_eint.F90
!||    multi_submatlaw      ../engine/source/multifluid/multi_submatlaw.F
!||    sigeps75             ../engine/source/materials/mat/mat075/sigeps75.F
!||--- calls      -----------------------------------------------------
!||    compaction           ../common_source/eos/compaction.F90
!||    compaction2          ../common_source/eos/compaction2.F90
!||    compaction_tab       ../common_source/eos/compaction_tab.F90
!||    eosexponential       ../common_source/eos/eosexponential.F90
!||    eoslinear            ../common_source/eos/eoslinear.F
!||    eospolyno            ../common_source/eos/eospolyno.F
!||    gruneisen            ../common_source/eos/gruneisen.F
!||    idealgas             ../common_source/eos/idealgas.F
!||    idealgas_vt          ../common_source/eos/idealgas_vt.F
!||    jwl                  ../common_source/eos/jwl.F
!||    lszk                 ../common_source/eos/lszk.F
!||    murnaghan            ../common_source/eos/murnaghan.F
!||    nasg                 ../common_source/eos/nasg.F
!||    noble_abel           ../common_source/eos/noble_abel.F
!||    osborne              ../common_source/eos/osborne.F
!||    powder_burn          ../common_source/eos/powder_burn.F
!||    puff                 ../common_source/eos/puff.F
!||    sesame               ../common_source/eos/sesame.F
!||    stiffgas             ../common_source/eos/stiffgas.F
!||    tabulated            ../common_source/eos/tabulated.F
!||    tillotson            ../common_source/eos/tillotson.F
!||--- uses       -----------------------------------------------------
!||    compaction2_mod      ../common_source/eos/compaction2.F90
!||    compaction_mod       ../common_source/eos/compaction.F90
!||    compaction_tab_mod   ../common_source/eos/compaction_tab.F90
!||    eos_param_mod        ../common_source/modules/mat_elem/eos_param_mod.F90
!||    eosexponential_mod   ../common_source/eos/eosexponential.F90
!||    eoslinear_mod        ../common_source/eos/eoslinear.F
!||    eospolyno_mod        ../common_source/eos/eospolyno.F
!||    gruneisen_mod        ../common_source/eos/gruneisen.F
!||    idealgas_mod         ../common_source/eos/idealgas.F
!||    idealgas_vt_mod      ../common_source/eos/idealgas_vt.F
!||    jwl_mod              ../common_source/eos/jwl.F
!||    lszk_mod             ../common_source/eos/lszk.F
!||    matparam_def_mod     ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    murnaghan_mod        ../common_source/eos/murnaghan.F
!||    nasg_mod             ../common_source/eos/nasg.F
!||    noble_abel_mod       ../common_source/eos/noble_abel.F
!||    osborne_mod          ../common_source/eos/osborne.F
!||    powder_burn_mod      ../common_source/eos/powder_burn.F
!||    puff_mod             ../common_source/eos/puff.F
!||    sesame_mod           ../common_source/eos/sesame.F
!||    stiffgas_mod         ../common_source/eos/stiffgas.F
!||    tabulated_mod        ../common_source/eos/tabulated.F
!||    tillotson_mod        ../common_source/eos/tillotson.F
!||====================================================================
        SUBROUTINE EOSMAIN(IFLAG ,NEL    ,EOSTYP ,PM      ,OFF      ,EINT  ,
     2                     RHO   ,RHO0   ,MU     ,MU2     ,ESPE     ,
     3                     DVOL  ,DF     ,VNEW   ,MAT     ,PSH      ,
     4                     PNEW  ,DPDM   ,DPDE   ,THETA   ,
     5                     BUFMAT,SIG    ,MU_BAK ,MLW     ,
     6                     NPF   ,TF     ,VAREOS ,NVAREOS ,MAT_PARAM,
     7                     BFRAC ,NVARTMP,VARTMP)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutines call the expected EOS solver
C depending on EOSTYP(INTEGER) value.
C IFLAG:0 staggered scheme, Cranck-Nicolson scheme / semi-implicit step 0
C IFLAG:1 sstaggered scheme, Cranck-Nicolson scheme / semi-implicit step 1
C IFLAG:2 collocated scheme and others, Euler scheme
      !--------------------------------------------------!--------------------------!-----------!
      !   MODULE                                         !  IEOS  ! EOS             !  VERSION  !
      !--------------------------------------------------!--------!-----------------!-----------!
          USE EOSPOLYNO_MOD      , ONLY : EOSPOLYNO      !   1    !  POLYNOMIAL     ! 12.0      !
          USE GRUNEISEN_MOD      , ONLY : GRUNEISEN      !   2    !  GRUNEISEN      ! 12.0      !
          USE TILLOTSON_MOD      , ONLY : TILLOTSON      !   3    !  TILLOTSON      ! 12.0      !
          USE PUFF_MOD           , ONLY : PUFF           !   4    !  PUFF           ! 12.0      !
          USE SESAME_MOD         , ONLY : SESAME         !   5    !  SESAME         ! 12.0      !
          USE NOBLE_ABEL_MOD     , ONLY : NOBLE_ABEL     !   6    !  NOBLE-ABEL     ! 2017.0    !
          USE IDEALGAS_MOD       , ONLY : IDEALGAS       !   7    !  IDEAL GAS      ! 2018.0    !
          USE MURNAGHAN_MOD      , ONLY : MURNAGHAN      !   8    !  MUNAGHAN       ! 2018.0    !
          USE OSBORNE_MOD        , ONLY : OSBORNE        !   9    !  OSBORNE        ! 2018.0    !
          USE STIFFGAS_MOD       , ONLY : STIFFGAS       !  10    !  STIFFENED GAS  ! 2018.0    !
          USE LSZK_MOD           , ONLY : LSZK           !  11    !  LSZK           ! 2018.0    !
          USE POWDER_BURN_MOD    , ONLY : POWDER_BURN    !  12    !  POWDER-BURN    ! 2019.1    !
          USE COMPACTION_MOD     , ONLY : COMPACTION     !  13    !  COMPACTION     ! 2019.1    !
          USE NASG_MOD           , ONLY : NASG           !  14    !  NASG           ! 2020.0    !
          USE JWL_MOD            , ONLY : JWL            !  15    !  JWL            ! internal use : INIMAP
          USE IDEALGAS_VT_MOD    , ONLY : IDEALGAS_VT    !  16    !  IDEALGAS_VT    ! 2022.0    !
          USE TABULATED_MOD      , ONLY : TABULATED      !  17    !  TABULATED      ! 2022.2    !
          USE EOSLINEAR_MOD      , ONLY : EOSLINEAR      !  18    !  LINEAR         ! 2019.0    !
          USE EOSEXPONENTIAL_MOD , ONLY : EOSEXPONENTIAL !  19    !  EXPONENTIAL    ! 2024.1    !
          USE COMPACTION2_MOD    , ONLY : COMPACTION2    !  20    !  COMPACTION2    ! 2025.1    !
          USE COMPACTION_TAB_MOD , ONLY : COMPACTION_TAB !  21    !  COMPACTION_TAB ! 2026.0    !
      !--------------------------------------------------!--------!-----------------!-----------!
!
!  input   EINT(:) :  rho.e   (law151    , unit J/m**3)
!                     rho.e.V (other laws, unit J)
!
! output   EPSE(:) :  rho0.e (=E)   (all laws, used such as P_eos:=P(mu,E) )
!
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
          USE EOS_PARAM_MOD ,ONLY : ANALY_TEMP
          USE MATPARAM_DEF_MOD   , ONLY : MATPARAM_STRUCT_
          USE COMPACTION_MOD     , ONLY : COMPACTION
          USE COMPACTION2_MOD    , ONLY : COMPACTION2
          USE COMPACTION_TAB_MOD , ONLY : COMPACTION_TAB
          USE EOSEXPONENTIAL_MOD , ONLY : EOSEXPONENTIAL
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
          INTEGER, INTENT(IN) :: IFLAG  !< IFLAG 0,1,2 : 0=explicit (or semi-implicit pass 1), 1=semi-implicit (pass2), 2=collocated
          INTEGER, INTENT(IN) :: NEL    !< number of elements
          INTEGER, INTENT(IN) :: EOSTYP !< EoS identifier  (1:polynomial, 2:gruneisen, 3:tillotson, 4:puff, 5:sesame, etc...)
          my_real, INTENT(IN) :: PM(NPROPM,NUMMAT) !< material properties array
          my_real, INTENT(IN) :: OFF(NEL) !< Element failure factor (usually 1.0)
          my_real, INTENT(INOUT) :: EINT(NEL) !< Internal Energy (SI:J)
          my_real, INTENT(IN) :: RHO(NEL)  !< Mass density
          my_real, INTENT(IN) :: RHO0(NEL) !< Reference mass density
          my_real, INTENT(IN) :: MU(NEL)    !< volumetric strain
          my_real, INTENT(INOUT) :: MU2(NEL) !< mu*mu
          my_real, INTENT(INOUT) :: ESPE(NEL) !< rho0.e (used such as P_eos:=P(mu,E) )
          my_real, INTENT(IN) :: DVOL(NEL) !< voluem change (including advected flow)
          my_real, INTENT(IN) :: DF(NEL) !< rho0/rho
          my_real, INTENT(IN) :: VNEW(NEL) !< current volume
          INTEGER, INTENT(IN) :: MAT(NEL) !< material internal identifier
          my_real, INTENT(INOUT) :: PSH(NEL) !< pressure shift
          my_real, INTENT(INOUT) :: PNEW(NEL) !< curent pressure
          my_real, INTENT(INOUT) :: DPDM(NEL) !< total derivative dP/d(mu)
          my_real, INTENT(INOUT) :: DPDE(NEL) !< partial derivative dP/dE  where E=Eint/V0 and V0 such as rho0.V0 = rho.V
          my_real, INTENT(INOUT) :: THETA(NEL) !< current temperature
          my_real, INTENT(IN) :: BUFMAT(SBUFMAT) !< buffer array for EOS (used by SESAME)
          my_real, INTENT(IN) :: SIG(NEL,6) !< stress tensor (used by ideal gas VT)
          my_real, INTENT(INOUT) :: MU_BAK(NEL) !< backup for history (used by COMPACTION)
          INTEGER, INTENT(IN) :: MLW !< current material law type
          INTEGER, INTENT(IN) :: NPF(SNPC) !< buffer for functions
          my_real, INTENT(IN) :: TF(STF) !< buffer for functions
          my_real, INTENT(INOUT) :: BFRAC(NEL) !< Burn fractions
          INTEGER, INTENT(IN) :: NVAREOS !< number of additional variables for EOS
          my_real, INTENT(INOUT) :: VAREOS(NVAREOS*NEL) !< additional variables for EOS
          TYPE(MATPARAM_STRUCT_), INTENT(IN) :: MAT_PARAM !< material parameters structure
          INTEGER ,INTENT(IN) :: NVARTMP                              !< NUMBER OF TEMPORARY INTERNAL VARIABLES
          INTEGER ,DIMENSION(NEL,NVARTMP) ,INTENT(INOUT) :: VARTMP    !< TEMPORARY INTERNAL VARIABLES
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
          INTEGER I,ISFLUID
          my_real :: DTEMP(NEL)    !< temperature increment
          my_real :: CV            !< specific heat at constant volume
          my_real :: CP            !< specific heat at constant pressure
          my_real :: TAB_TEMP(NEL) !< temporary array,tabulated temperatures
          my_real :: PMIN          !< minimum pressure allowed (material parameter)
          my_real :: POLD(NEL)     !< previous pressure (used by IDEALGAS_VT)
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
          IF(IFLAG == 0) THEN
            DO I=1,NEL
              MU2(I)=MAX(ZERO,MU(I))**2
              ESPE(I)=DF(I)*EINT(I)/ MAX(EM15,VNEW(I))         !ESPE=rho0.e   since EINT=rho.e.V  and DF=rho0/rho   =>  DF*EINT/V = E
            ENDDO

          ELSEIF (IFLAG == 2) THEN
            !COLLOCATED SCHEME (LAW151 ONLY)
            DO I = 1, NEL
              IF (VNEW(I) > ZERO) THEN
                MU2(I)=MAX(ZERO,MU(I))**2
              ENDIF
              ESPE(I) =  DF(I) * EINT(I)                   !law151 : EINT=rho.e
            ENDDO

          ENDIF

          PMIN = PM(37,MAT(1)) ! minimum pressure allowed (material parameter)

          SELECT CASE (EOSTYP)

       CASE (1)
         !--------------------------------------------------!
         !         POLYNOMIAL EOS                           !
         !--------------------------------------------------! 
         CALL EOSPOLYNO(
     1                  IFLAG  ,NEL    ,PMIN   ,OFF    ,EINT  ,MU  ,MU2,
     2                  ESPE   ,DVOL   ,DF     ,VNEW   ,PSH   ,
     3                  PNEW   ,DPDM   ,DPDE   ,MAT_PARAM%EOS)

       CASE (2)
         !--------------------------------------------------!
         !         GRUNEISEN EOS                            !
         !--------------------------------------------------! 
         CALL GRUNEISEN(
     1                  IFLAG  ,NEL    ,PMIN   ,OFF    ,EINT  ,MU  ,MU2,
     2                  ESPE   ,DVOL   ,DF     ,VNEW   ,RHO0,
     3                  PNEW   ,DPDM   ,DPDE   ,PSH ,
     4                  MAT_PARAM%EOS)

       CASE (3)
         !--------------------------------------------------!
         !         TILLOTSON EOS                            !
         !--------------------------------------------------! 
         CALL TILLOTSON(
     1                  IFLAG  ,NEL    ,PMIN   ,OFF    ,EINT   ,MU  ,MU2,
     2                  ESPE   ,DVOL   ,DF     ,VNEW   ,PSH    ,
     3                  PNEW   ,DPDM   ,DPDE   ,VAREOS ,NVAREOS,MAT_PARAM%EOS)

       CASE (4) 
         !--------------------------------------------------!
         !         PUFF EOS                                 !
         !--------------------------------------------------! 
         CALL PUFF(
     1                  IFLAG  ,NEL    ,PMIN   ,OFF    ,EINT  ,MU  ,MU2,
     2                  ESPE   ,DVOL   ,DF     ,VNEW   ,
     3                  PNEW   ,DPDM   ,DPDE   ,MAT_PARAM%EOS)

           CASE (5)
            !--------------------------------------------------!
            !      SESAME EOS                                  !
            !--------------------------------------------------!
            TAB_TEMP(1:NEL) = THETA(1:NEL)
            CALL SESAME(
     1                     IFLAG    ,NEL    ,PMIN   ,OFF    ,EINT  ,RHO ,RHO0 ,
     2                     ESPE     ,DVOL   ,PNEW   ,DPDM   ,DPDE,
     3                     TAB_TEMP ,BUFMAT ,MAT_PARAM%EOS)

       CASE(6)
         !--------------------------------------------------!
         !         NOBLE-ABEL EOS (COVOLUME EOS)            !
         !--------------------------------------------------!          
          CALL NOBLE_ABEL(
     1                    IFLAG , NEL  ,OFF  ,EINT ,MU  ,
     2                    ESPE  , DVOL ,DF   ,VNEW ,PSH ,
     3                    PNEW  , DPDM ,DPDE ,MAT_PARAM%EOS)

       CASE(7)
         !--------------------------------------------------!
         !         IDEAL GAS EOS                            !
         !--------------------------------------------------!          
          CALL IDEALGAS(
     1                  IFLAG ,NEL  ,OFF  ,EINT ,MU  ,
     2                  ESPE  ,DVOL ,DF   ,VNEW ,PSH ,
     3                  PNEW  ,DPDM ,DPDE ,MAT_PARAM%EOS )

       CASE(8)
         !--------------------------------------------------!
         !         MURNAGHAN EOS                            !
         !--------------------------------------------------!          
          CALL MURNAGHAN(
     1                  IFLAG  ,NEL    ,PMIN,OFF  ,EINT ,MU  ,
     2                  DVOL   ,VNEW   ,PSH ,
     3                  PNEW   ,DPDM   ,DPDE ,MAT_PARAM%EOS)

       CASE(9)
         !--------------------------------------------------!
         !         OSBORNE EOS                              !
         !--------------------------------------------------!
          CALL OSBORNE(
     1                  IFLAG  ,NEL   ,PMIN ,OFF  ,EINT ,MU  ,
     2                  ESPE   ,DVOL  ,DF   ,VNEW ,RHO0 ,PSH ,
     3                  PNEW   ,DPDM  ,DPDE ,MAT_PARAM%EOS)

       CASE(10)
         !--------------------------------------------------!
         !         STIFFENED GAS EOS                        !
         !--------------------------------------------------!          
          CALL STIFFGAS(
     1                  IFLAG , NEL  ,PMIN ,OFF  ,EINT ,MU  ,MU2,
     2                  ESPE  , DVOL ,DF   ,VNEW ,PSH ,
     3                  PNEW  , DPDM ,DPDE ,MAT_PARAM%EOS)

       CASE(11)
         !--------------------------------------------------!
         !         LSZK EOS                                 !
         !--------------------------------------------------!          
          CALL LSZK(
     1                  IFLAG , NEL   ,PMIN ,OFF  ,EINT ,MU  ,
     2                  ESPE  , DVOL  ,DF   ,VNEW ,PSH  ,
     3                  PNEW  , DPDM  ,DPDE ,MAT_PARAM%EOS)

       CASE(12)
         !--------------------------------------------------!
         !         POWDER BURN EOS                          !
         !--------------------------------------------------!          
          CALL POWDER_BURN(NPROPM, NUMMAT,
     1                  IFLAG  ,NEL     ,PM    ,OFF      ,EINT  ,MU       ,MU2,
     2                  ESPE   ,DVOL    ,DF    ,VNEW     ,MAT   ,PSH      ,
     3                  PNEW   ,DPDM    ,DPDE  ,MAT_PARAM,
     4                  VAREOS ,NVAREOS ,DT1   ,RHO0 ,BFRAC)

       CASE(13)
         !--------------------------------------------------!
         !         COMPACTION EOS                           !
         !--------------------------------------------------!          
          CALL COMPACTION(
     1                    IFLAG ,NEL   ,PMIN ,OFF   ,EINT  ,MU  ,MU2,
     2                    DVOL  ,PSH  ,
     3                    PNEW  ,DPDM  ,DPDE ,MU_BAK,
     4                    MAT_PARAM%EOS)

       CASE(14)
         !--------------------------------------------------!
         !         NASG EOS                                 !
         !--------------------------------------------------!          
          CALL NASG(
     1                  IFLAG , NEL   ,PMIN ,OFF  ,EINT ,MU  ,
     2                  ESPE  , DVOL  ,VNEW ,PSH ,
     3                  PNEW  , DPDM  ,DPDE ,MAT_PARAM%EOS)
       CASE(15)
         !--------------------------------------------------!
         !         JWL EOS (internal use law151)            !
         !--------------------------------------------------!
          CALL JWL(
     1                  IFLAG ,NEL  ,PM   ,OFF  , EINT ,MU  ,MU2,
     2                  ESPE  ,DVOL ,DF   ,VNEW , MAT  ,PSH ,
     3                  PNEW  ,DPDM ,DPDE )

       CASE(16)
         !--------------------------------------------------!
         !         IDEAL GAS VT EOS                         !
         !--------------------------------------------------!
          DO I=1,NEL
            POLD(I) = -THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3))
          ENDDO
          CALL IDEALGAS_VT(
     1                     IFLAG ,NEL  ,PMIN ,OFF     ,EINT ,MU  ,MU2,
     2                     ESPE  ,DVOL ,DF   ,VNEW    ,PSH  ,RHO0,RHO,
     3                     PNEW  ,DPDM ,DPDE ,TAB_TEMP,POLD  ,MAT_PARAM%EOS)

       CASE(17)
         !--------------------------------------------------!
         !         TABULATED EOS                            !
         !--------------------------------------------------!          
          CALL TABULATED(
     1                  IFLAG  , NEL   ,PMIN ,OFF  ,EINT ,MU  ,
     2                  ESPE   , DVOL  ,DF   ,VNEW ,PSH ,
     3                  PNEW   , DPDM  ,DPDE ,
     4                  NPF    , TF    ,SNPC ,STF  ,MAT_PARAM%EOS)

       CASE (18)
         !--------------------------------------------------!
         !         LINEAR EOS                               !
         !--------------------------------------------------!
         CALL EOSLINEAR(
     1                  IFLAG  ,NEL    ,PMIN   ,OFF    ,EINT  ,MU  ,
     2                  DVOL   ,VNEW   ,PSH    ,PNEW  ,DPDM,
     3                  DPDE   ,MAT_PARAM%EOS, PM(104,MAT(1)), PM(32,MAT(1)))

       CASE (19)
         !--------------------------------------------------!
         !         EXPONENTIAL EOS                          !
         !--------------------------------------------------!
         CALL EOSEXPONENTIAL(
     1                       IFLAG  ,NEL    ,OFF    ,EINT  ,
     2                       DVOL   ,VNEW   ,PSH   ,
     3                       PNEW   ,DPDM   ,DPDE   ,TT    ,
     4                       MAT_PARAM%EOS)

       CASE(20)
         !--------------------------------------------------!
         !         COMPACTION EOS                           !
         !--------------------------------------------------!
          CALL COMPACTION2(
     1                  IFLAG ,NEL  ,PMIN   ,OFF   ,EINT  ,MU   ,
     2                  DVOL  ,PSH  ,
     3                  PNEW  ,DPDM ,DPDE ,NVAREOS, VAREOS ,
     4                  NPF   ,TF   ,SNPC ,STF   ,
     5                  MAT_PARAM%EOS)

       CASE(21)
         !--------------------------------------------------!
         !         COMPACTION_TAB EOS                       !
         !--------------------------------------------------!
          CALL COMPACTION_TAB(
     1                        IFLAG  ,NEL   ,PMIN   ,OFF   ,EINT  ,
     2                        DVOL   ,PSH   ,DT1    ,RHO   ,RHO0 ,
     3                        PNEW   ,DPDM  ,DPDE   ,
     4                        NVAREOS,VAREOS,NVARTMP,VARTMP,
     5                        MAT_PARAM%EOS)

          END SELECT

          !--------------------------------------------------!
          !         TEMPERATURE FROM EOS                     !
          !--------------------------------------------------!
          IF(IFLAG == 0) RETURN

          DTEMP(1:NEL) = ZERO

          SELECT CASE (EOSTYP)

           CASE (5,16)
            !tabulated EoS or formal expression
            !SESAME(5), IDEAL-GAS-VT(16)
            DO I=1,NEL
              IF(OFF(I) == ONE .AND. VNEW(I) > ZERO) THEN
                THETA(I) = TAB_TEMP(I)
                DTEMP(I) = ZERO
              ENDIF
            ENDDO

           CASE(8,13,18,20)
            !no depedency on internal energy (we may deduce the EoS from strain tensor if Hooke's law is used)
            CP = PM(69,MAT(1)) / RHO0(1)
            CV = CP !suitable approximation since these EoS are modeling solids
            IF(CV > ZERO)THEN
              DO I=1,NEL
                IF(OFF(I) == ONE .AND. VNEW(I) > ZERO) THEN
                  DTEMP(I) = (PNEW(I)+PSH(I))*DVOL(I)/CV
                ENDIF
              ENDDO
            ENDIF

           CASE DEFAULT
            !gruneisen parameter is strictly positive (most of EoS)
            DO I=1,NEL
              IF(PNEW(I) > PMIN .AND. VNEW(I) > ZERO) THEN
                DTEMP(I) = -OFF(I)*(THETA(I)*DPDE(I)*DF(I))*DVOL(I)/VNEW(I) ! no conduction => Cv does not play any role
              ENDIF
            ENDDO

          END SELECT


          isfluid = mat_param%eos%isfluid
          if(isfluid == 0 .AND. ANALY_TEMP == 0)then
            !solid material modeling - cutoff in expansion
            do i=1,nel
              IF(MU(I) > ZERO)THEN
                THETA(I) = THETA(I) + DTEMP(I)
              endif
              THETA(I) = MAX(ZERO,THETA(I))
            end do
          else
            !fluid material modeling
            do i=1,nel
              THETA(I) = THETA(I) + DTEMP(I)
              THETA(I) = MAX(ZERO,THETA(I))
            end do
          end if


          RETURN
        END SUBROUTINE EOSMAIN
      END MODULE EOSMAIN_MOD
