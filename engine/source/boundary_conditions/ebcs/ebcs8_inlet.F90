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
!||    ebcs8_inlet_mod   ../engine/source/boundary_conditions/ebcs/ebcs8_inlet.F90
!||--- called by ------------------------------------------------------
!||    ebcs_main         ../engine/source/boundary_conditions/ebcs/ebcs_main.F
!||====================================================================
      module ebcs8_inlet_mod
        implicit none
        contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief Here is a small description of the routine, [after the header]
!! \details if needed, more details can be added here
!||====================================================================
!||    ebcs8_inlet           ../engine/source/boundary_conditions/ebcs/ebcs8_inlet.F90
!||--- called by ------------------------------------------------------
!||    ebcs_main             ../engine/source/boundary_conditions/ebcs/ebcs_main.F
!||--- calls      -----------------------------------------------------
!||    ebcs_get_group_info   ../engine/source/boundary_conditions/ebcs/ebcs8_inlet.F90
!||    eosmain               ../common_source/eos/eosmain.F
!||--- uses       -----------------------------------------------------
!||    constant_mod          ../common_source/modules/constant_mod.F
!||    debug_mod             ../engine/share/modules/debug_mod.F
!||    ebcs_mod              ../common_source/modules/boundary_conditions/ebcs_mod.F90
!||    elbufdef_mod          ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    eosmain_mod           ../common_source/eos/eosmain.F
!||    matparam_def_mod      ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    multimat_param_mod    ../common_source/modules/multimat_param_mod.F90
!||    output_mod            ../common_source/modules/output/output_mod.F90
!||    precision_mod         ../common_source/modules/precision_mod.F90
!||    segvar_mod            ../engine/share/modules/segvar_mod.F
!||    th_surf_mod           ../common_source/modules/interfaces/th_surf_mod.F
!||====================================================================
      SUBROUTINE EBCS8_INLET( &
                       NUMNOD, NSEG,ISEG,SEGVAR, &
                       A,V,W,X, &
                       LISTE,NOD,IRECT, &
                       LA,FUNC_VALUE,EBCS, OUTPUT, DT1, TIME, &
                       NGROUP, ELBUF_TAB, NPARG, IPARG, N2D,&
                       SURF_ID,NSURF,FSAVSURF,NUMMAT,MATPARAM,SNPC,NPC,STF,TF,&
                       NPROPM, PM,  BUFMAT, NCYCLE, IALE)
!
!This subroutine is applying inlet State + velocity.
!  two possibilities :   vel + rho,e  => p(rho,e) deduced
!                    :   vel + rho,P  => e deduced such as P(rho,r)=P
!  state is stored in SEGVAR% for the ghost cell data
!  compatible staggered scheme : mono & multi-multimaterial ALE
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
        use ebcs_mod , only : t_ebcs_inlet
        use segvar_mod , only : t_segvar
        use precision_mod , only : wp
        use output_mod , only : output_
        use constant_mod , only : zero,two,half, third, fourth, em06, em12, em20, one
        use elbufdef_mod , only : elbuf_struct_, g_bufel_, l_bufel_, buf_mat_
        use th_surf_mod , only : th_surf_num_channel
        use multimat_param_mod , only : m51_n0phas, m51_nvphas
        use matparam_def_mod, only : matparam_struct_
        use eosmain_mod , only : eosmain

         USE DEBUG_MOD
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      INTEGER,INTENT(IN) :: NGROUP, NPARG, NUMNOD,NUMMAT
      INTEGER,INTENT(IN) :: IALE !< to detect ALE or EULER framework
      INTEGER,INTENT(IN) :: N2D !< 0:3D, 1:axi, 2:plane strain
      INTEGER,INTENT(IN) :: IPARG(NPARG,NGROUP)
      INTEGER :: NSEG,NOD,ISEG(NSEG),LISTE(NOD),IRECT(4,NSEG)
      REAL(KIND=WP) :: A(3,NUMNOD), V(3,NUMNOD), W(3,NUMNOD), X(3,NUMNOD), LA(3,NOD), FUNC_VALUE(*)
      TYPE(t_ebcs_inlet), INTENT(INOUT) :: EBCS
      TYPE(t_segvar) :: SEGVAR
      TYPE(OUTPUT_), INTENT(INOUT) :: OUTPUT !< output structure
      REAL(KIND=WP),INTENT(IN) :: DT1 !< time step
      REAL(KIND=WP),INTENT(IN) :: TIME !< current time
      TYPE(ELBUF_STRUCT_), INTENT(IN), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      INTEGER,INTENT(IN) :: NSURF
      INTEGER,INTENT(IN) :: SURF_ID
      real(kind=WP), intent(inout) :: fsavsurf(th_surf_num_channel,nsurf)
      TYPE(MATPARAM_STRUCT_),DIMENSION(NUMMAT),INTENT(IN) :: MATPARAM !< material buffer
      INTEGER,INTENT(IN) :: SNPC, STF
      INTEGER,INTENT(IN) :: NPC(SNPC)
      REAL(KIND=WP) :: tf(STF)
      INTEGER,INTENT(IN) :: NPROPM
      REAL(KIND=WP),INTENT(IN) :: PM(NPROPM, NUMMAT)
      real(kind=WP), intent(in) :: BUFMAT(*)
      INTEGER,INTENT(IN) :: NCYCLE
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
      INTEGER :: I,IS,KSEG,N1,N2,N3,N4,NG1,NG2,NG3,NG4,N
      REAL(KIND=WP) :: ORIENT,FAC,X13,Y13,Z13,X24,Y24,Z24,NX,NY,NZ,S !< tmp variables (normal + mean velocities)
      REAL(KIND=WP) :: DVNF,VNF,WNF !< normal velocity at face : relative(dv), material(v) & grid(w).
      REAL(KIND=WP) :: WMX,WMY,WMZ !< mean grid velocity at face
      REAL(KIND=WP) :: FLUXI,FLUXO !< volumetric fluxes (in/out)
      REAL(KIND=WP) :: M_IN, M_OUT, E_IN, E_OUT !< inlet/outlet post-treatment
      REAL(KIND=WP) :: PHASE_RHOII(21), PHASE_RHOJJ(21)    !< submaterial densities (cell:ii, and ghost cell:jj)
      REAL(KIND=WP) :: PHASE_ALPHAII(21),PHASE_ALPHAJJ(21)
      REAL(KIND=WP) :: PHASE_EINTII(21),PHASE_EINTJJ(21)
      REAL(KIND=WP) :: PHASE_TEMPII(21),PHASE_TEMPJJ(21)
      REAL(KIND=WP) :: PHASE_SSPII(21),PHASE_SSPJJ(21)
      REAL(KIND=WP) :: PHASE_PII(21),PHASE_PJJ(21)
      REAL(KIND=WP) :: RHOII,RHOJJ    !< mass density, cell (ii) and ghost cell (jj)
      REAL(KIND=WP) :: EINTII,EINTJJ  !< internal energy density, cell (ii) and ghost cell (jj)
      REAL(KIND=WP) :: SSPII,SSPJJ    !< sound speed, cell (ii) and ghost cell (jj)
      REAL(KIND=WP) :: TEMPII,TEMPJJ  !< temperature, cell (ii) and ghost cell (jj)
      REAL(KIND=WP) :: PII,PJJ        !< pressure, cell (ii) and ghost cell (jj)
      REAL(KIND=WP) :: VOLII          !< volume
      REAL(KIND=WP) :: VN             !< user imposed normal velocity
      REAL(KIND=WP) :: VX,VY,VZ
      REAL(KIND=WP) :: AREA           !< face data
      REAL(KIND=WP) :: NORM           !< norm of normal vector
      REAL(KIND=WP) :: PFACE
      REAL(KIND=WP) :: WFEXTT
      REAL(KIND=WP) :: YM
      type(g_bufel_), pointer :: gbuf
      type(l_bufel_), pointer :: lbuf
      type(buf_mat_), pointer :: mbuf
      INTEGER :: VEL_FLAG
      INTEGER :: SUBMAT_mid
      INTEGER :: klt, iloc, imat, ipos, isubmat, kk, mtn, ngrp !< temp variables
      INTEGER :: idx(6)
      INTEGER :: IFORM ! formulation flag : VP or VE
      INTEGER :: NBMAT ! number of submaterials (multimaterial case)
      INTEGER :: IFUNC
      
      !specific to call EOSMAIN
      integer :: mat(1)
      real(kind=wp),dimension(1) :: off, ener, rho, rho0, amu, amu2, espe, dvol
      real(kind=wp),dimension(1) :: psh, df, vnew, dpde, dpdm, temp, muold, bid, pnew
      real(kind=wp),dimension(1,6) :: sig
      integer :: nvareos, nvartmp_eos
      integer :: eostyp, ilaw
      real(kind=wp), dimension(16) :: vareos
      integer,dimension(16) ::  vartmp_eos

      !Newton's iterations
      REAL(KIND=WP) :: func, error, grun, dfunc, incr, tol
      integer :: MAX_ITER,  ITER
      logical :: CONT
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ---------------------------------------------------------------------------------------------------------------------- 
      VN = -HUGE(VN) ! GCC15 warning about uninitialized variable for those 4 variables
      VX = -HUGE(VN)
      VY = -HUGE(VN)
      VZ = -HUGE(VN)
      nbmat = ebcs%nbmat
      iform = ebcs%fvm_inlet_data%formulation  ! 1:VP  2:VE
      vel_flag = ebcs%fvm_inlet_data%vector_velocity  ! 0:Vn  1:Vxyz
      !   VEL_FLAG = 0 : normal velocity    : vx.'n'
      !              1 : vectorial velocity : vx.'x' + vy.'y' + vz.'z'
      !
      !      iVx   =0  : Vx = fscale_Vx
      !      iVx   >0  : Vx = fscale_Vx * fvel(iVx)
      !      iVx = -1  : nothing to do : managed with momentum advection
      ! ------------------------------------------------------------------------!
      !                        0. RETRIVE USER VELOCITY                         !
      ! ------------------------------------------------------------------------!
      IF(VEL_FLAG == 0)THEN
        VN = -EBCS%FVM_INLET_DATA%VAL_VEL(1)
        IFUNC = EBCS%FVM_INLET_DATA%FUNC_VEL(1)
        IF (IFUNC  >  0) THEN
           VN = VN * FUNC_VALUE(IFUNC)
           ! IFUNC == 0  : do nothing
           ! IFUNC == -1 : do nothing
        ENDIF
      ELSE
        VX = EBCS%FVM_INLET_DATA%VAL_VEL(1)
        VY = EBCS%FVM_INLET_DATA%VAL_VEL(2)
        VZ = EBCS%FVM_INLET_DATA%VAL_VEL(3)
        IFUNC = EBCS%FVM_INLET_DATA%FUNC_VEL(1)
        IF (IFUNC  >  0) VX = VX * FUNC_VALUE(IFUNC)
        IFUNC = EBCS%FVM_INLET_DATA%FUNC_VEL(2)
        IF (IFUNC  >  0) VY = VY * FUNC_VALUE(IFUNC)
        IFUNC = EBCS%FVM_INLET_DATA%FUNC_VEL(3)
        IF (IFUNC  >  0) VZ = VZ * FUNC_VALUE(IFUNC)
      ENDIF

      ! ------------------------------------------------------------------------!
      !                        1. INITIALIZATION                                !
      ! ------------------------------------------------------------------------!
      M_IN = ZERO
      M_OUT = ZERO
      E_IN = ZERO
      E_OUT = ZERO
      WFEXTT = ZERO
      YM = ONE
      WNF = ZERO
      ! ISEG -> ELEM_ID -> (NG, ILOC)
      IF(NCYCLE == 0) CALL EBCS_GET_GROUP_INFO(NSEG, EBCS%IELEM, EBCS%NG, EBCS%ILOC,  NGROUP, NPARG, IPARG, N2D)
      ! NORMAL VECTORS AT NODES (zeoring)
      DO I=1,NOD
         LA(1,I)=ZERO
         LA(2,I)=ZERO
         LA(3,I)=ZERO
      ENDDO

      ! ------------------------------------------------------------------------!
      !                     3. NODAL AVERAGES                                   !
      !                            (normal vectors & impedence)                 !
      ! ------------------------------------------------------------------------!
      IF(N2D == 0)THEN
        DO IS=1,NSEG
          KSEG=ABS(ISEG(IS))
          ORIENT=FLOAT(ISEG(IS)/KSEG)
          N1=IRECT(1,IS)
          N2=IRECT(2,IS)
          N3=IRECT(3,IS)
          N4=IRECT(4,IS)
          IF(N4 == 0 .OR. N4 == N3) THEN
            FAC=THIRD*ORIENT
            N4=N3
          ELSE
            FAC=FOURTH*ORIENT
          ENDIF
          NG1=LISTE(N1)
          NG2=LISTE(N2)
          NG3=LISTE(N3)
          NG4=LISTE(N4)
          X13=X(1,NG3)-X(1,NG1)
          Y13=X(2,NG3)-X(2,NG1)
          Z13=X(3,NG3)-X(3,NG1)
          X24=X(1,NG4)-X(1,NG2)
          Y24=X(2,NG4)-X(2,NG2)
          Z24=X(3,NG4)-X(3,NG2)
          NX=(Y13*Z24-Z13*Y24)*FAC
          NY=(Z13*X24-X13*Z24)*FAC
          NZ=(X13*Y24-Y13*X24)*FAC
          LA(1,N1)=LA(1,N1)+NX
          LA(2,N1)=LA(2,N1)+NY
          LA(3,N1)=LA(3,N1)+NZ
          LA(1,N2)=LA(1,N2)+NX
          LA(2,N2)=LA(2,N2)+NY
          LA(3,N2)=LA(3,N2)+NZ
          LA(1,N3)=LA(1,N3)+NX
          LA(2,N3)=LA(2,N3)+NY
          LA(3,N3)=LA(3,N3)+NZ
          IF(N4 /= N3) THEN
             LA(1,N4)=LA(1,N4)+NX
             LA(2,N4)=LA(2,N4)+NY
             LA(3,N4)=LA(3,N4)+NZ
          ENDIF

          !CUMULATIVE AREA FOR MEAN IMPEDENCE (NUMERATOR)
          NORM = SQRT(NX*NX + NY*NY + NZ*NZ)
          AREA = HALF*NORM/FAC
          EBCS%AREA(IS) = AREA

          ! RELATIVE VELOCITY AT FACE
          VNF = VN
          IF(VEL_FLAG == 1)VNF = (VX*NX + VY*NY + VZ*NZ)/NORM
          WNF = ZERO
          IF(IALE == 1)THEN
            WMX=W(1,NG1)+W(1,NG2)+W(1,NG3)
            WMY=W(2,NG1)+W(2,NG2)+W(2,NG3)
            WMZ=W(3,NG1)+W(3,NG2)+W(3,NG3)
            FAC = THIRD
            IF(N4 /= N3) THEN
               WMX = WMX+W(1,NG4)
               WMY = WMY+W(2,NG4)
               WMZ = WMZ+W(3,NG4)
               FAC = FOURTH
            ENDIF
            WNF = WMX*NX + WMY*NY + WMZ*NZ
            WNF = FAC * WNF / NORM
          END IF
          EBCS%DVNF(IS) = VNF-WNF !relative normal velocity at face

        ENDDO !next IS

      ELSE

       DO IS=1,NSEG
          KSEG=ABS(ISEG(IS))
          ORIENT=FLOAT(ISEG(IS)/KSEG)
          N1=IRECT(1,IS)
          N2=IRECT(2,IS)
          NG1=LISTE(N1)
          NG2=LISTE(N2)
          NY= (X(3,NG2)-X(3,NG1))*ORIENT
          NZ=-(X(2,NG2)-X(2,NG1))*ORIENT
          LA(2,N1)=LA(2,N1)+NY
          LA(3,N1)=LA(3,N1)+NZ
          LA(2,N2)=LA(2,N2)+NY
          LA(3,N2)=LA(3,N2)+NZ
          YM = ONE
          IF(N2D==1)YM=HALF*(X(2,NG1)+X(2,NG2))

          !CUMULATIVE AREA FOR MEAN IMPEDENCE (NUMERATOR)
          NORM = SQRT(NY*NY + NZ*NZ)
          AREA = NORM*YM
          EBCS%AREA(IS) = AREA

          ! RELATIVE VELOCITY AT FACE
          VNF = VN
          IF(VEL_FLAG == 1)VNF = (VY*NY+VZ*NZ)/NORM
          WNF = ZERO
          IF(IALE == 1)THEN
            WMY=W(2,NG1)+W(2,NG2)
            WMZ=W(3,NG1)+W(3,NG2)
            WNF = WMY*NY + WMZ*NZ
            WNF = HALF * WNF / NORM
          END IF
          EBCS%DVNF(IS) = VNF-WNF

        ENDDO !next IS

      END IF
      ! ------------------------------------------------------------------------!
      !                        2. INITIAL CONDITION                             !
      ! ------------------------------------------------------------------------!
      IF(TIME == ZERO)THEN
        IF(VEL_FLAG == 0)THEN
          ! user normal velocity
          DO I=1,NOD
            N=LISTE(I)
            NORM = ONE/SQRT(LA(1,I)**2+LA(2,I)**2+LA(3,I)**2)
            FAC=VN*NORM
            V(1,N) = FAC*LA(1,I)
            V(2,N) = FAC*LA(2,I)
            V(3,N) = FAC*LA(3,I)
          ENDDO
        ELSE
          ! user vectorial velocity
          DO I=1,NOD
            N=LISTE(I)
            V(1,N) = VX
            V(2,N) = VY
            V(3,N) = VZ
          ENDDO
        END IF
      ENDIF


      ! ------------------------------------------------------------------------!
      !                 4. INIT GHOST CELL FOR ADVECTION STEP                   !
      ! ------------------------------------------------------------------------!
      DO IS=1,NSEG

        KSEG=ABS(ISEG(IS))

        !ADJACENT CELL - STATE
        ngrp = ebcs%ng(is)
        iloc = ebcs%iloc(is)-1
        gbuf => elbuf_tab(ngrp)%gbuf                                                                                                              
        lbuf => elbuf_tab(ngrp)%bufly(1)%lbuf(1,1,1)
        mtn = iparg(1,ngrp)
        klt = iparg(2,ngrp)
        !adjacent pressure                                                                                                                        
        do kk=1,6
          idx(kk) = klt*(kk-1)                                                                                                                    
        enddo                                                                                                                                     
        pii = -third*(gbuf%sig(idx(1)+iloc+1) + gbuf%sig(idx(2)+iloc+1) + gbuf%sig(idx(3)+iloc+1))
        eintii = gbuf%eint(iloc+1)
        tempii= gbuf%temp(iloc+1)
        !density                                                                                                                                  
        rhoii = gbuf%rho(iloc+1)
        volii = gbuf%vol(iloc+1)
        !adjacent sound speed                                                                                                                     
        sspii = lbuf%ssp(iloc+1)

        !ADJACENT CELL - MULTIMATERIAL STATE
        if(mtn == 51)then                                                                                                                         
          mbuf => elbuf_tab(ngrp)%bufly(1)%mat(1,1,1)
          do isubmat=1,nbmat
            !volume fraction
            ipos = 1
            kk = (m51_n0phas + (isubmat-1)*m51_nvphas +ipos-1) * klt  +  iloc+1
            phase_alphaii(isubmat) = mbuf%var(kk)
            !energy density
            ipos = 8
            kk = (m51_n0phas + (isubmat-1)*m51_nvphas +ipos-1) * klt  +  iloc+1
            phase_eintii(isubmat) = mbuf%var(kk)
            !mass density
            ipos = 9
            kk = (m51_n0phas + (isubmat-1)*m51_nvphas +ipos-1) * klt  +  iloc+1
            phase_rhoii(isubmat) = mbuf%var(kk)
            !sound speed
            ipos = 14
            kk = (m51_n0phas + (isubmat-1)*m51_nvphas +ipos-1) * klt  +  iloc+1
            phase_sspii(isubmat) = mbuf%var(kk)
            !temperature
            ipos = 16
            kk = (m51_n0phas + (isubmat-1)*m51_nvphas +ipos-1) * klt  +  iloc+1
            phase_tempii(isubmat) = mbuf%var(kk)
            !pressure
            ipos = 18
            kk = (m51_n0phas + (isubmat-1)*m51_nvphas +ipos-1) * klt  +  iloc+1                                                                           
            phase_pii(isubmat) = mbuf%var(kk)
          enddo                                                                                                                                   
        else
           PHASE_ALPHAII = ONE
           PHASE_EINTII = EINTII
           PHASE_RHOII = RHOII
           PHASE_SSPII = SSPII
           PHASE_TEMPII = TEMPII
           PHASE_PII = PII
        endif
        
        ! DENSITY / VFRAC - GHOST CELL
        RHOJJ  = ZERO
        PJJ    = ZERO
        EINTJJ = ZERO
        SSPJJ  = ZERO
        TEMPJJ = ZERO
        PHASE_ALPHAJJ(1:4) = ZERO  !for potential unused submat
        PHASE_RHOJJ(1:4) = ONE
        PHASE_EINTJJ(1:4) = ONE
        IF(nbmat >= 1)THEN

         DO IMAT = 1, nbmat
         
            PHASE_RHOJJ(IMAT) = EBCS%FVM_INLET_DATA%VAL_RHO(IMAT)
            IFUNC = EBCS%FVM_INLET_DATA%FUNC_RHO(IMAT)
            IF (IFUNC  >  0)THEN
              PHASE_RHOJJ(IMAT) = PHASE_RHOJJ(IMAT) * FUNC_VALUE(IFUNC)
            ELSEIF (IFUNC  == -1) THEN
               PHASE_RHOJJ(IMAT) = PHASE_RHOII(IMAT)
            ENDIF
            
            PHASE_ALPHAJJ(IMAT) = EBCS%FVM_INLET_DATA%VAL_ALPHA(IMAT)
            IFUNC = EBCS%FVM_INLET_DATA%FUNC_ALPHA(IMAT)
            IF (IFUNC  >  0) THEN
              PHASE_ALPHAJJ(IMAT) = PHASE_ALPHAJJ(IMAT) * FUNC_VALUE(IFUNC)
            ELSEIF (IFUNC  == -1) THEN
               PHASE_ALPHAJJ(IMAT) = PHASE_ALPHAII(IMAT)
            ENDIF
            
            !global density
            RHOJJ = RHOJJ + PHASE_RHOJJ(IMAT) * PHASE_ALPHAJJ(IMAT)

            !temperature
            phase_tempjj(imat) = phase_tempii(imat)

            
         ENDDO        

         IF(IFORM == 2)THEN
         ! VE formulation

             !NBMAT is 1 for mono-material law
             DO IMAT = 1, nbmat
                PHASE_EINTJJ(IMAT) = EBCS%FVM_INLET_DATA%VAL_PRES(IMAT)
                IFUNC = EBCS%FVM_INLET_DATA%FUNC_PRES(IMAT)
                IF (IFUNC  >  0) THEN
                   PHASE_EINTJJ(IMAT) = PHASE_EINTJJ(IMAT) * FUNC_VALUE(IFUNC)
                ELSEIF (IFUNC == -1) THEN
                   PHASE_EINTJJ(IMAT) = PHASE_EINTII(IMAT)
                ENDIF

                 ! CALL EOS
                 submat_mid = iparg(18,ngrp)
                 if(mtn == 51) submat_mid = matparam(iparg(18,ngrp))%multimat%mid(imat)
                 if(submat_mid == 0)cycle  ! means /EBCS/INLET has more sibmat than Law51 definition : ignore additional submat in inlet
                 eostyp = matparam(submat_mid)%ieos
                 ilaw = matparam(submat_mid)%ilaw
                 mat(1) = submat_mid
                 OFF(1)=one
                 ener(1)=phase_eintjj(imat)
                 rho(1)=phase_rhojj(imat)
                 rho0(1)=PM(1,submat_mid)
                 amu(1)=phase_rhojj(imat)/rho0(1)-one
                 amu2(1)=max(zero,amu(1))*amu(1)
                 espe(1)=phase_eintjj(imat)
                 dvol(1)=zero
                 df(1)=rho0(1)/rho(1)
                 vnew(1)=one
                 DPDE(1)=zero
                 DPDM(1)=zero
                 TEMP(1)=phase_tempjj(imat)
                 sig(1,1:3) = -phase_pjj(imat)
                 muold(1)=zero
                 bid(1)=zero
                 psh(1)=pm(88,submat_mid)
                 vareos(1:16)=zero
                 vartmp_eos(1:16)=1
                 nvareos=16
                 nvartmp_eos=16

                 CALL EOSMAIN(2       ,1       ,EOSTYP ,PM    ,OFF  , ENER , &
                              RHO     ,RHO0    ,AMU    ,AMU2  ,ESPE , &
                              DVOL    ,DF      ,VNEW   ,MAT   ,PSH  , &
                              PNEW    ,DPDM    ,DPDE   ,TEMP  , &
                              BUFMAT  ,SIG     ,MUOLD  ,ILAW  , &
                              NPC     ,TF      ,VAREOS ,NVAREOS, MATPARAM(SUBMAT_MID), BID, NVARTMP_EOS,VARTMP_EOS)

                 PHASE_PJJ(IMAT) = PNEW(1)
                 PHASE_SSPJJ(IMAT) = SQRT(DPDM(1)/RHO0(1))

                 SSPJJ = SSPJJ + PHASE_ALPHAJJ(IMAT) * PHASE_RHOJJ(IMAT) * MAX(EM20, PHASE_SSPJJ(IMAT))
                 PJJ = PJJ + PHASE_PJJ(IMAT) * PHASE_ALPHAJJ(IMAT)
                 EINTJJ = EINTJJ + PHASE_ALPHAJJ(IMAT) * PHASE_EINTJJ(IMAT)
             ENDDO

         ELSE
           !IFORM = 1 : VP formulation
           !   NBMAT is 1 for mono-material law
             DO IMAT = 1, nbmat
                PHASE_PJJ(IMAT) = EBCS%FVM_INLET_DATA%VAL_PRES(IMAT)
                IFUNC = EBCS%FVM_INLET_DATA%FUNC_PRES(IMAT)
                IF (IFUNC  >  0) THEN
                   PHASE_PJJ(IMAT) = PHASE_PJJ(IMAT) * FUNC_VALUE(IFUNC)
                ELSEIF (IFUNC == -1) THEN
                   PHASE_PJJ(IMAT) = PHASE_PII(IMAT)
                ENDIF

                 ! CALL EOS
                 submat_mid = iparg(18,ngrp)
                 if(mtn == 51)submat_mid = matparam(iparg(18,ngrp))%multimat%mid(imat)
                 if(submat_mid == 0)cycle  ! means /EBCS/INLET has more sibmat than Law51 definition : ignore additional submat in inlet
                 eostyp = matparam(submat_mid)%ieos
                 ilaw = matparam(submat_mid)%ilaw
                 mat(1) = submat_mid
                 OFF(1)=one
                 rho(1)=phase_rhojj(imat)
                 rho0(1)=PM(1,submat_mid)
                 amu(1)=phase_rhojj(imat)/rho0(1)-one
                 amu2(1)=max(zero,amu(1))*amu(1)
                 dvol(1)=zero
                 df(1)=rho0(1)/rho(1)
                 vnew(1)=one
                 DPDE(1)=zero
                 DPDM(1)=zero
                 TEMP(1)=phase_tempjj(imat)
                 sig(1,1:3) = -phase_pjj(imat)
                 muold(1)=zero
                 bid(1)=zero
                 psh(1)=pm(88,submat_mid)
                 vareos(1:16)=zero
                 vartmp_eos(1:16)=1
                 nvareos=16
                 nvartmp_eos=16

                 ener(1)=zero
                 espe(1)=zero

                 ! ---SOLVE EINT
                 MAX_ITER = 50
                 TOL = EM12
                 ERROR = ONE
                 ITER = 0
                 CONT = .TRUE.
                 DO WHILE (CONT .AND. ITER  <  MAX_ITER)
                   ITER = ITER + 1

                       CALL EOSMAIN(2       ,1       ,EOSTYP ,PM    ,OFF  , ENER , &
                                    RHO     ,RHO0    ,AMU    ,AMU2  ,ESPE , &
                                    DVOL    ,DF      ,VNEW   ,MAT   ,PSH  , &
                                    PNEW    ,DPDM    ,DPDE   ,TEMP  , &
                                    BUFMAT  ,SIG     ,MUOLD  ,ILAW  , &
                                    NPC     ,TF      ,VAREOS ,NVAREOS, MATPARAM(SUBMAT_MID), BID, NVARTMP_EOS,VARTMP_EOS)

                   FUNC = PNEW(1) - PHASE_PJJ(IMAT)
                   ERROR  = ABS(FUNC)
                   IF (ERROR  <  TOL * (ABS(PHASE_PJJ(IMAT)) + ONE)) THEN
                     CONT = .FALSE.
                   END IF
                   DFUNC = DPDE(1)
                   GRUN = DPDE(1)/(one+AMU(1))  ! /rho0 > 0.
                   IF (GRUN  >  ZERO) THEN
                     INCR = -FUNC / DFUNC
                     ENER(1) = ENER(1) + INCR
                   ELSE
                     CONT = .FALSE.
                     ENER(1) = ZERO
                   END IF

                 END DO

                 PHASE_EINTJJ(IMAT) = ESPE(1)
                 PHASE_SSPJJ(IMAT) = SQRT(DPDM(1)/RHO0(1))

                 SSPJJ = SSPJJ + PHASE_ALPHAJJ(IMAT) * PHASE_RHOJJ(IMAT) * MAX(EM20, PHASE_SSPJJ(IMAT))
                 PJJ = PJJ + PHASE_PJJ(IMAT) * PHASE_ALPHAJJ(IMAT)
                 EINTJJ = EINTJJ + PHASE_ALPHAJJ(IMAT) * PHASE_EINTJJ(IMAT)
             ENDDO

         END IF

       END IF
            

       ! ghost cell state for advection step
       SEGVAR%RHO(KSEG)=RHOJJ     !ghost cell advection
       SEGVAR%EINT(KSEG)=EINTJJ   !ghost cell advection

       ! ghost cell submaterials
       IF(SEGVAR%has_phase_alpha)SEGVAR%PHASE_ALPHA(1:4,KSEG)=PHASE_ALPHAJJ(1:4)
       IF(SEGVAR%has_phase_rho)SEGVAR%PHASE_RHO(1:4,KSEG)=PHASE_RHOJJ(1:4)
       IF(SEGVAR%has_phase_eint)SEGVAR%PHASE_EINT(1:4,KSEG)=PHASE_EINTJJ(1:4)

        !inlet/outlet mass & energy
        AREA = EBCS%AREA(IS)
        DVNF = EBCS%DVNF(IS)
        FLUXO=AREA*DVNF*DT1*YM  !normal vector is outward
        !2d case : check Normal norm (2S or S)
        FLUXI=MIN(FLUXO,ZERO) !in  (<0 , normal is outgoing)
        FLUXO=MAX(FLUXO,ZERO) !out (>0 , normal is outgoing)
        M_in  = M_in-FLUXI*RHOJJ
        M_out = M_out-FLUXO*RHOII
        E_in  = E_in-FLUXI*EINTJJ
        E_out = E_out-FLUXO*EINTII
        
        !/TH/SURF (massflow, velocity, pressure)
        PFACE=PJJ
        IF(DVNF > ZERO)PFACE=PII
        FSAVSURF(2,SURF_ID) = FSAVSURF(2,SURF_ID) + RHOII*AREA*DVNF     ! rho.S.u = dm/dt
        FSAVSURF(3,SURF_ID) = FSAVSURF(3,SURF_ID) + AREA*DVNF           ! S.u
        FSAVSURF(4,SURF_ID) = FSAVSURF(4,SURF_ID) + AREA*PFACE          ! S.P
        FSAVSURF(6,SURF_ID) = FSAVSURF(6,SURF_ID) + RHOII*AREA*DVNF*DT1 !  m<-m+dm (cumulative)

        WFEXTT = WFEXTT - DT1*AREA*PII*DVNF

      ENDDO

!$OMP CRITICAL
      OUTPUT%DATA%INOUT%DM_IN  = OUTPUT%DATA%INOUT%DM_IN + M_IN
      OUTPUT%DATA%INOUT%DM_OUT = OUTPUT%DATA%INOUT%DM_OUT + M_OUT
      OUTPUT%DATA%INOUT%DE_IN = OUTPUT%DATA%INOUT%DE_IN + E_IN
      OUTPUT%DATA%INOUT%DE_OUT = OUTPUT%DATA%INOUT%DE_OUT + E_OUT
      !external force work
      OUTPUT%TH%WFEXT = OUTPUT%TH%WFEXT + WFEXTT
!$OMP END CRITICAL

      ! ------------------------------------------------------------------------!
      !                 5. IMPOSED VELOCITY  (PENALITY)                         !
      !                       (alternative for subsonic flows)                  !
      ! ------------------------------------------------------------------------!
      !IF(VEL_FLAG == 0)THEN
      !  DO I=1,NOD
      !    N=LISTE(I)
      !    S=ONE/SQRT(LA(1,I)**2+LA(2,I)**2+LA(3,I)**2)
      !    DVX=V(1,N)-VN*LA(1,I)/S
      !    DVY=V(2,N)-VN*LA(2,I)/S
      !    DVZ=V(3,N)-VN*LA(3,I)/S
      !    ROC = NODAL_AREA(I) / NODAL_ROC(I) !averaged impedence at node I
      !    P=ROC*(DVX*LA(1,I)+DVY*LA(2,I)+DVZ*LA(3,I))/S
      !    A(1,N)=A(1,N)-P*LA(1,I)
      !    A(2,N)=A(2,N)-P*LA(2,I)
      !    A(3,N)=A(3,N)-P*LA(3,I)
      !    STIFN(N)=STIFN(N)+(TWO*(S*ROC)**2)/MS(N)
      !  ENDDO
      !ELSE
      !  DO I=1,NOD
      !    N=LISTE(I)
      !    DVX=V(1,N)-VX
      !    DVY=V(2,N)-VY
      !    DVZ=V(3,N)-VZ
      !    S=ONE/SQRT(LA(1,I)**2+LA(2,I)**2+LA(3,I)**2)
      !    ROC = NODAL_AREA(I) / NODAL_ROC(I) !averaged impedence at node I
      !    P=ROC*(DVX*LA(1,I)+DVY*LA(2,I)+DVZ*LA(3,I))/S
      !    A(1,N)=A(1,N)-P*LA(1,I)
      !    A(2,N)=A(2,N)-P*LA(2,I)
      !    A(3,N)=A(3,N)-P*LA(3,I)
      !    STIFN(N)=STIFN(N)+(TWO*(S*ROC)**2)/MS(N)
      !  ENDDO
      !END IF

      ! ------------------------------------------------------------------------!
      !                 5. IMPOSED VELOCITY  (DIRECT)                           !
      ! ------------------------------------------------------------------------!
      IF(VEL_FLAG == 0)THEN
        DO I=1,NOD
          N=LISTE(I)
          S=ONE/SQRT(LA(1,I)**2+LA(2,I)**2+LA(3,I)**2)
          V(1,N)=VN*LA(1,I)*S
          V(2,N)=VN*LA(2,I)*S
          V(3,N)=VN*LA(3,I)*S
          A(1,N)=zero
          A(2,N)=zero
          A(3,N)=zero
        ENDDO
      ELSE
        DO I=1,NOD
          N=LISTE(I)
          V(1,N)=VX
          V(2,N)=VY
          V(3,N)=VZ
          A(1,N)=zero
          A(2,N)=zero
          A(3,N)=zero
        ENDDO
      END IF

      RETURN
      END SUBROUTINE EBCS8_INLET

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Procedure
! ----------------------------------------------------------------------------------------------------------------------
!||====================================================================
!||    ebcs_get_group_info   ../engine/source/boundary_conditions/ebcs/ebcs8_inlet.F90
!||--- called by ------------------------------------------------------
!||    ebcs8_inlet           ../engine/source/boundary_conditions/ebcs/ebcs8_inlet.F90
!||====================================================================
      SUBROUTINE EBCS_GET_GROUP_INFO(NSEG, IELEM, NG, ILOC, NGROUP, NPARG, IPARG, N2D)
        IMPLICIT NONE
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
        INTEGER,INTENT(IN) :: NGROUP,  NPARG  , NSEG
        INTEGER,INTENT(IN) :: N2D
        INTEGER,INTENT(IN) :: IPARG(NPARG,NGROUP)
        INTEGER,DIMENSION(NSEG),INTENT(IN) :: IELEM    !< element ids
        INTEGER,DIMENSION(:),ALLOCATABLE,INTENT(INOUT) :: NG      !< group number
        INTEGER,DIMENSION(:),ALLOCATABLE,INTENT(INOUT) :: ILOC    !< local number in group
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
        INTEGER :: ELEM_ID
        INTEGER :: NGRP, KTY, KLT, MFT, ISOLNOD, IS
        LOGICAL :: BFOUND
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
        IF(.NOT.ALLOCATED(NG))ALLOCATE(NG(NSEG))
        IF(.NOT.ALLOCATED(ILOC))ALLOCATE(ILOC(NSEG))
        !LOCATING ADJACENT CELL (ELEM-ID) IN THE GROUPS
        DO IS=1,NSEG
          bfound = .false.
          elem_id = ielem(is)
          do ngrp=1,ngroup
            kty = iparg(5,ngrp)
            klt = iparg(2,ngrp)
            mft = iparg(3,ngrp)
            isolnod = iparg(28,ngrp)
            if(n2d == 0)then
              if(kty /= 1)cycle
            else
              if(kty == 2)then
                isolnod=4
              elseif(kty == 7)then
                isolnod=3
              else
                cycle
              endif
            endif
            if (elem_id <= klt+mft)then
              bfound = .true.
              exit
            endif
          enddo
          if(.not.bfound)cycle !next i
          ng(is) = ngrp
          iloc(is) = elem_id - mft
        ENDDO !next IS
      END SUBROUTINE EBCS_GET_GROUP_INFO

      END MODULE EBCS8_INLET_MOD
