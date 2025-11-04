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
!||    anim_monvol_mod    ../engine/source/output/anim/generate/monvol_anim.F90
!||--- called by ------------------------------------------------------
!||    genani             ../engine/source/output/anim/generate/genani.F
!||    h3d_nodal_scalar   ../engine/source/output/h3d/h3d_results/h3d_nodal_scalar.F
!||====================================================================
      MODULE ANIM_MONVOL_MOD
      implicit none
        integer, parameter, private :: NIXS=11
      CONTAINS
!||====================================================================
!||    xyz16               ../engine/source/output/anim/generate/monvol_anim.F90
!||--- called by ------------------------------------------------------
!||    genani              ../engine/source/output/anim/generate/genani.F
!||--- calls      -----------------------------------------------------
!||    spmd_gather_xyz16   ../engine/source/mpi/anim/spmd_gather_xyz16.F
!||    write_r_c           ../common_source/tools/input_output/write_routtines.c
!||--- uses       -----------------------------------------------------
!||    constant_mod        ../common_source/modules/constant_mod.F
!||    precision_mod       ../common_source/modules/precision_mod.F90
!||====================================================================
        SUBROUTINE XYZ16(IXS,IXS16,X,ISPMD,NSPMD,NUMELS16,NUMELS8,NUMELS10,&
        &NUMELS20,NUMELS16G)
          use precision_mod, only: WP
          use constant_mod
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
          real(kind=WP)&
          &:: X(3,*)
          INTEGER :: IXS(NIXS,*),&
          &IXS16(8,*)
          INTEGER, intent(in) :: ISPMD,NSPMD,NUMELS16,NUMELS8,NUMELS10
          INTEGER, intent(in) :: NUMELS20,NUMELS16G
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          real(kind=WP)&
          &:: XX,YY,ZZ
          REAL :: R4,R4NP(6*NUMELS16)
          INTEGER :: I, J, N1,N2,N3,N4,N5,N6,N7,N8,&
          &JJ,BUF
! ----------------------------------------------------------------------------------------------------------------------
          JJ = 0
          DO J=1,NUMELS16
            I = J+NUMELS8+NUMELS10+NUMELS20
            N1 = IXS(2,I)
            N2 = IXS(3,I)
            N3 = IXS(4,I)
            N4 = IXS(5,I)
            N5 = IXS16(1,J)
            N6 = IXS16(2,J)
            N7 = IXS16(3,J)
            N8 = IXS16(4,J)
            IF(N5==0)N5=N1
            IF(N6==0)N6=N2
            IF(N7==0)N7=N3
            IF(N8==0)N8=N4
            XX = HALF *(X(1,N5)+X(1,N6)+X(1,N7)+X(1,N8))&
            &-FOURTH*(X(1,N1)+X(1,N2)+X(1,N3)+X(1,N4))
            YY = HALF *(X(2,N5)+X(2,N6)+X(2,N7)+X(2,N8))&
            &-FOURTH*(X(2,N1)+X(2,N2)+X(2,N3)+X(2,N4))
            ZZ = HALF *(X(3,N5)+X(3,N6)+X(3,N7)+X(3,N8))&
            &-FOURTH*(X(3,N1)+X(3,N2)+X(3,N3)+X(3,N4))
            IF (NSPMD == 1) THEN
              R4 = XX
              CALL WRITE_R_C(R4,1)
              R4 = YY
              CALL WRITE_R_C(R4,1)
              R4 = ZZ
              CALL WRITE_R_C(R4,1)
            ELSE
              R4NP(JJ+1) = XX
              R4NP(JJ+2) = YY
              R4NP(JJ+3) = ZZ
            END IF
            N1 = IXS(6,I)
            N2 = IXS(7,I)
            N3 = IXS(8,I)
            N4 = IXS(9,I)
            N5 = IXS16(5,J)
            N6 = IXS16(6,J)
            N7 = IXS16(7,J)
            N8 = IXS16(8,J)
            IF(N5==0)N5=N1
            IF(N6==0)N6=N2
            IF(N7==0)N7=N3
            IF(N8==0)N8=N4
            XX = HALF *(X(1,N5)+X(1,N6)+X(1,N7)+X(1,N8))&
            &-FOURTH*(X(1,N1)+X(1,N2)+X(1,N3)+X(1,N4))
            YY = HALF *(X(2,N5)+X(2,N6)+X(2,N7)+X(2,N8))&
            &-FOURTH*(X(2,N1)+X(2,N2)+X(2,N3)+X(2,N4))
            ZZ = HALF *(X(3,N5)+X(3,N6)+X(3,N7)+X(3,N8))&
            &-FOURTH*(X(3,N1)+X(3,N2)+X(3,N3)+X(3,N4))
            IF (NSPMD == 1) THEN
              R4 = XX
              CALL WRITE_R_C(R4,1)
              R4 = YY
              CALL WRITE_R_C(R4,1)
              R4 = ZZ
              CALL WRITE_R_C(R4,1)
            ELSE
              R4NP(JJ+4) = XX
              R4NP(JJ+5) = YY
              R4NP(JJ+6) = ZZ
              JJ = JJ + 6
            END IF
          END DO
          IF (NSPMD > 1) THEN
            IF (ISPMD==0) THEN
              BUF = 6*NUMELS16G
            ELSE
              BUF=1
            END IF
!
            CALL SPMD_GATHER_XYZ16(R4NP,BUF)
!
          END IF
!
          RETURN
        end subroutine XYZ16
!||====================================================================
!||    xyznor16       ../engine/source/output/anim/generate/monvol_anim.F90
!||--- called by ------------------------------------------------------
!||    genani         ../engine/source/output/anim/generate/genani.F
!||--- calls      -----------------------------------------------------
!||    write_s_c      ../common_source/tools/input_output/write_routtines.c
!||--- uses       -----------------------------------------------------
!||    constant_mod   ../common_source/modules/constant_mod.F
!||====================================================================
        SUBROUTINE XYZNOR16(NUMELS16G)
! ----------------------------------------------------------------------------------------------------------------------
!                                                 implicit none
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
          INTEGER, intent(in) :: NUMELS16G
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          INTEGER :: J, I3000,SIZ
! ----------------------------------------------------------------------------------------------------------------------
          I3000 = 3000
          SIZ = NUMELS16G
          DO J=1,SIZ
            CALL WRITE_S_C(I3000,1)
            CALL WRITE_S_C(I3000,1)
            CALL WRITE_S_C(I3000,1)
            CALL WRITE_S_C(I3000,1)
            CALL WRITE_S_C(I3000,1)
            CALL WRITE_S_C(I3000,1)
          END DO
!
          RETURN
        end subroutine XYZNOR16
!||====================================================================
!||    animbale                 ../engine/source/output/anim/generate/monvol_anim.F90
!||--- called by ------------------------------------------------------
!||    genani                   ../engine/source/output/anim/generate/genani.F
!||    h3d_nodal_scalar         ../engine/source/output/h3d/h3d_results/h3d_nodal_scalar.F
!||--- calls      -----------------------------------------------------
!||    spmd_fvb_scat_num_noda   ../engine/source/mpi/anim/spmd_fvb_scat_num_noda.F
!||--- uses       -----------------------------------------------------
!||    constant_mod             ../common_source/modules/constant_mod.F
!||    fvbag_mod                ../engine/share/modules/fvbag_mod.F
!||    precision_mod            ../common_source/modules/precision_mod.F90
!||====================================================================
        SUBROUTINE ANIMBALE(IAN ,WA4, IS_WRITTEN_NODE,MONVOL,VOLMON, IOPT,&
        &NUMNOD, NIMV, NVOLU, NRVOLU, LICBAG, LIBAGJET,&
        &LIBAGHOL, LRCBAG, LRBAGJET, LRBAGHOL, NSPMD)
! ----------------------------------------------------------------------------------------------------------------------
!                                                        Modules
! ----------------------------------------------------------------------------------------------------------------------
          USE FVBAG_MOD
          use precision_mod, only: WP
          use constant_mod
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
          INTEGER :: MONVOL(*),IAN
          real(kind=WP)&
          &:: VOLMON(*)
          REAL :: WA4(*)
          INTEGER, INTENT(IN) :: IOPT
          ! OPT=1 : write internal nodes (1:NUMNOD)            : calls for /H3D
          ! OPT=2 : write also additional nodes (1:NUMNOD+NNA) : calls for /ANIM
          INTEGER, INTENT(IN) :: NUMNOD, NIMV, NVOLU, NRVOLU, LICBAG, LIBAGJET
          INTEGER, INTENT(IN) :: LIBAGHOL, LRCBAG, LRBAGJET, LRBAGHOL, NSPMD
          INTEGER , INTENT(INOUT) :: IS_WRITTEN_NODE(NUMNOD)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          INTEGER :: I,ITYP,K1,KK1,N,N1,N2
          INTEGER :: KR1,KR2,KR3,KR9,KRA1,KRA2,KRA3,KRA8
          INTEGER :: KIBHOL,KRBHOL

          INTEGER :: NTG, KIBALE, KRBALE

          INTEGER :: NNS, NBA, NNA, NNI, NTGI,NNT
          INTEGER :: IFV
!
          ! IS_WRITTEN_NODE(1:NUMNOD) = 0   !if uncommented all other nodal values which were previously defined will be not displayed
          K1 = 1
          KK1 = 1
          KIBHOL =1 + NIMV*NVOLU  + LICBAG + LIBAGJET
          KRBHOL =1 + NRVOLU * NVOLU + LRCBAG + LRBAGJET
          KIBALE=KIBHOL + LIBAGHOL
          KRBALE=KRBHOL + LRBAGHOL
          IFV=0
          DO I=1,NVOLU
            ITYP=MONVOL(K1+1)
            IF(ITYP==6.OR.ITYP==8)THEN
              IFV=MONVOL(K1-1+45)
              NNS=MONVOL(K1-1+32)
              NTG=MONVOL(K1-1+33)
              NBA=MONVOL(K1-1+62)
              NNA=MONVOL(K1-1+64)
              NNI =MONVOL(K1-1+68)
              NTGI=MONVOL(K1-1+69)
              NNT = NNS+NNI
              KR1=KRBALE+MONVOL(K1-1+34)
              KR2=KR1+NNT
              KR3=KR2+NNT
              KR9=KR1+6*NNT+4*(NTG+NTGI)
              KRA1=KR9+NNT
              KRA2=KRA1+NNA
              KRA3=KRA2+NNA
              KRA8=KRA1+12*NNA+NTGI
              IF (IAN==3) THEN
! pressure
                IF (NSPMD > 1)THEN
                  CALL SPMD_FVB_SCAT_NUM_NODA(IFV,VOLMON(KRA1),NNA)
                  CALL SPMD_FVB_SCAT_NUM_NODA(IFV,VOLMON(KR1),NNT)
                END IF
                IF(IOPT==2)THEN
                  DO N=1,FVSPMD(IFV)%NNA_L
                    N1=FVSPMD(IFV)%IBUFA_L(1,N)
                    N2=FVSPMD(IFV)%IBUFA_L(2,N)
                    WA4(N2)=VOLMON(KRA1-1+N1)
                  END DO
                END IF
!
                DO N=1,FVSPMD(IFV)%NN_L+FVSPMD(IFV)%NNI_L
                  N1=FVSPMD(IFV)%IBUF_L(1,N)
                  N2=FVSPMD(IFV)%IBUF_L(2,N)
                  WA4(N2)=VOLMON(KR1-1+N1)
                  IS_WRITTEN_NODE(N2)=1
                END DO
              ELSE IF (IAN==4) THEN
! densite
                IF (NSPMD > 1)THEN
                  CALL SPMD_FVB_SCAT_NUM_NODA(IFV,VOLMON(KRA2),NNA)
                  CALL SPMD_FVB_SCAT_NUM_NODA(IFV,VOLMON(KR2),NNT)
                END IF
                IF(IOPT==2)THEN
                  DO N=1,FVSPMD(IFV)%NNA_L
                    N1=FVSPMD(IFV)%IBUFA_L(1,N)
                    N2=FVSPMD(IFV)%IBUFA_L(2,N)
                    WA4(N2)=VOLMON(KRA2-1+N1)
                  END DO
                END IF
!
                DO N=1,FVSPMD(IFV)%NN_L+FVSPMD(IFV)%NNI_L
                  N1=FVSPMD(IFV)%IBUF_L(1,N)
                  N2=FVSPMD(IFV)%IBUF_L(2,N)
                  WA4(N2)=VOLMON(KR2-1+N1)
                  IS_WRITTEN_NODE(N2)=1
                END DO
              ELSE IF (IAN==6) THEN
! temperature
                IF (NSPMD > 1)THEN
                  CALL SPMD_FVB_SCAT_NUM_NODA(IFV,VOLMON(KRA3),NNA)
                  CALL SPMD_FVB_SCAT_NUM_NODA(IFV,VOLMON(KR3),NNT)
                END IF
                IF(IOPT==2)THEN
                  DO N=1,FVSPMD(IFV)%NNA_L
                    N1=FVSPMD(IFV)%IBUFA_L(1,N)
                    N2=FVSPMD(IFV)%IBUFA_L(2,N)
                    WA4(N2)=VOLMON(KRA3-1+N1)
                  END DO
                END IF
!
                DO N=1,FVSPMD(IFV)%NN_L+FVSPMD(IFV)%NNI_L
                  N1=FVSPMD(IFV)%IBUF_L(1,N)
                  N2=FVSPMD(IFV)%IBUF_L(2,N)
                  WA4(N2)=VOLMON(KR3-1+N1)
                  IS_WRITTEN_NODE(N2)=1
                END DO
              ELSE IF (IAN==30) THEN
! ssp:sound speed
                IF (NSPMD > 1)THEN
                  CALL SPMD_FVB_SCAT_NUM_NODA(IFV,VOLMON(KRA8),NNA)
                  CALL SPMD_FVB_SCAT_NUM_NODA(IFV,VOLMON(KR9),NNT)
                END IF
                IF(IOPT==2)THEN
                  DO N=1,FVSPMD(IFV)%NNA_L
                    N1=FVSPMD(IFV)%IBUFA_L(1,N)
                    N2=FVSPMD(IFV)%IBUFA_L(2,N)
                    WA4(N2)=VOLMON(KRA8-1+N1)
                  END DO
                END IF
!
                DO N=1,FVSPMD(IFV)%NN_L+FVSPMD(IFV)%NNI_L
                  N1=FVSPMD(IFV)%IBUF_L(1,N)
                  N2=FVSPMD(IFV)%IBUF_L(2,N)
                  WA4(N2)=VOLMON(KR9-1+N1)
                  IS_WRITTEN_NODE(N2)=1
                END DO
              END IF
            END IF
            K1 = K1 + NIMV
            KK1 = KK1 + NRVOLU
          END DO
!
          RETURN
        end subroutine ANIMBALE
!
!||====================================================================
!||    alevflu                  ../engine/source/output/anim/generate/monvol_anim.F90
!||--- called by ------------------------------------------------------
!||    genani                   ../engine/source/output/anim/generate/genani.F
!||--- calls      -----------------------------------------------------
!||    spmd_fvb_scat_num_noda   ../engine/source/mpi/anim/spmd_fvb_scat_num_noda.F
!||--- uses       -----------------------------------------------------
!||    constant_mod             ../common_source/modules/constant_mod.F
!||    fvbag_mod                ../engine/share/modules/fvbag_mod.F
!||    fvmbag_meshcontrol_mod   ../common_source/modules/airbag/fvmbag_meshcontrol_mod.F
!||    precision_mod            ../common_source/modules/precision_mod.F90
!||====================================================================
        SUBROUTINE ALEVFLU(VFLU , NNT, U, NNA,&
        &UA,  IFV, NSPMD)
! ----------------------------------------------------------------------------------------------------------------------
!                                                        Modules
! ----------------------------------------------------------------------------------------------------------------------
          USE FVBAG_MOD
          USE FVMBAG_MESHCONTROL_MOD
          use precision_mod, only: WP
          use constant_mod
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
          INTEGER, INTENT(IN) :: NSPMD
          INTEGER :: NNT, NNA, IFV
          real(kind=WP)&
          &:: VFLU(3,*), U(3,*), UA(3,*)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          INTEGER :: N,N1,N2
!
          IF (NSPMD > 1)THEN
            CALL SPMD_FVB_SCAT_NUM_NODA(IFV,UA,NNA*3)
            CALL SPMD_FVB_SCAT_NUM_NODA(IFV,U, NNT*3)
          END IF


          IF (KMESH(IFV) < 2) THEN
            DO N=1,FVSPMD(IFV)%NNA_L
              N1=FVSPMD(IFV)%IBUFA_L(1,N)
              N2=FVSPMD(IFV)%IBUFA_L(2,N)
              VFLU(1,N2)=UA(1,N1)
              VFLU(2,N2)=UA(2,N1)
              VFLU(3,N2)=UA(3,N1)
            END DO
          END IF
!
          DO N=1,FVSPMD(IFV)%NN_L+FVSPMD(IFV)%NNI_L
            N1=FVSPMD(IFV)%IBUF_L(1,N)
            N2=FVSPMD(IFV)%IBUF_L(2,N)
            VFLU(1,N2)=U(1,N1)
            VFLU(2,N2)=U(2,N1)
            VFLU(3,N2)=U(3,N1)
          END DO
!
          RETURN
        end subroutine ALEVFLU
!||====================================================================
!||    anivflow        ../engine/source/output/anim/generate/monvol_anim.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod    ../common_source/modules/constant_mod.F
!||    precision_mod   ../common_source/modules/precision_mod.F90
!||====================================================================
        SUBROUTINE ANIVFLOW(VFLU, NNO, NNI, IBUF, IBUFI,&
        &U   )
! ----------------------------------------------------------------------------------------------------------------------
!                                                 implicit none
! ----------------------------------------------------------------------------------------------------------------------
          use precision_mod, only: WP
          use constant_mod
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
          INTEGER :: NNO, NNI, IBUF(*), IBUFI(*)
          real(kind=WP)&
          &:: VFLU(3,*), U(3,*)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          INTEGER :: I, II
!
          DO I=1,NNO
            II=IBUF(I)
            VFLU(1,II)=U(1,I)
            VFLU(2,II)=U(2,I)
            VFLU(3,II)=U(3,I)
          END DO
          DO I=1,NNI
            II=IBUFI(I)
            VFLU(1,II)=U(1,NNO+I)
            VFLU(2,II)=U(2,NNO+I)
            VFLU(3,II)=U(3,NNO+I)
          END DO
!
          RETURN
        end subroutine ANIVFLOW
!||====================================================================
!||    anivflowp       ../engine/source/output/anim/generate/monvol_anim.F90
!||--- called by ------------------------------------------------------
!||    genani          ../engine/source/output/anim/generate/genani.F
!||--- uses       -----------------------------------------------------
!||    constant_mod    ../common_source/modules/constant_mod.F
!||    precision_mod   ../common_source/modules/precision_mod.F90
!||====================================================================
        SUBROUTINE ANIVFLOWP(VFLU, NNO  ,  NNO_L , NNI_L,&
        &IBUF, IBUFI, IBUFL, IBUFIL, U    )
! ----------------------------------------------------------------------------------------------------------------------
!                                                 implicit none
! ----------------------------------------------------------------------------------------------------------------------
          use precision_mod, only: WP
          use constant_mod
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
          INTEGER :: NNO,  NNO_L, NNI_L, IBUF(*), IBUFI(*),&
          &IBUFL(*), IBUFIL(*)
          real(kind=WP)&
          &:: VFLU(3,*), U(3,*)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          INTEGER :: I, II, III
!
          DO I=1,NNO_L
            II=IBUFL(I)
            III=IBUF(II)
            VFLU(1,III)=U(1,II)
            VFLU(2,III)=U(2,II)
            VFLU(3,III)=U(3,II)
          END DO
          DO I=1,NNI_L
            II=IBUFIL(I)
            III=IBUFI(II)
            VFLU(1,III)=U(1,NNO+II)
            VFLU(2,III)=U(2,NNO+II)
            VFLU(3,III)=U(3,NNO+II)
          END DO
!
          RETURN
        end subroutine ANIVFLOWP
!||====================================================================
!||    animcale           ../engine/source/output/anim/generate/monvol_anim.F90
!||--- called by ------------------------------------------------------
!||    genani             ../engine/source/output/anim/generate/genani.F
!||--- calls      -----------------------------------------------------
!||    spmd_r4get_partn   ../engine/source/mpi/anim/spmd_r4get_partn.F
!||    write_r_c          ../common_source/tools/input_output/write_routtines.c
!||--- uses       -----------------------------------------------------
!||    constant_mod       ../common_source/modules/constant_mod.F
!||    precision_mod      ../common_source/modules/precision_mod.F90
!||====================================================================
        SUBROUTINE ANIMCALE(IANIM , MONVOL, VOLMON, NBF, EL2FA,&
        &NBPART, IADG  , NBF_L,&
        &ISPMD, NSPMD, NIMV, NRVOLU, NVOLU,&
        &LICBAG, LRCBAG,&
        &LIBAGHOL, LRBAGHOL,LRBAGJET, LIBAGJET,&
        &NUMELQG, NUMELCG, NUMELTGG)
          use precision_mod, only: WP
          use constant_mod
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
          INTEGER :: IANIM, MONVOL(*), NBF, EL2FA(*), NBPART, IADG(NSPMD,*),&
          &NBF_L
          real(kind=WP)&
          &:: VOLMON(*)
          INTEGER, INTENT(IN) :: ISPMD, NSPMD, NIMV, NRVOLU, NVOLU
          INTEGER, INTENT(IN) :: LICBAG, LRCBAG,&
          &LIBAGHOL, LRBAGHOL, LRBAGJET, LIBAGJET
          INTEGER, INTENT(IN) :: NUMELQG, NUMELCG, NUMELTGG
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          INTEGER :: I, K1, KK1, KIBHOL, KRBHOL, KIBALE, KRBALE, ITYP, NNS,&
          &NTG, KI1, KR1, KR2, J, JJ, BUF, NNI, NTGI, NNT, NTGT
          real(kind=WP)&
          &:: WA(NBF)
          REAL    :: R4
!
          DO I=1,NBF
            WA(I)=ZERO
          END DO
!
          K1 = 1
          KK1 = 1
          KIBHOL =1 + NIMV*NVOLU  + LICBAG + LIBAGJET
          KRBHOL =1 + NRVOLU * NVOLU + LRCBAG + LRBAGJET
          KIBALE=KIBHOL + LIBAGHOL
          KRBALE=KRBHOL + LRBAGHOL
          DO I=1,NVOLU
            ITYP=MONVOL(K1-1+2)
            IF (ITYP==8) THEN
              NNS=MONVOL(K1-1+32)
              NTG=MONVOL(K1-1+33)
              NNI =MONVOL(K1-1+68)
              NTGI=MONVOL(K1-1+69)
              NNT=NNS+NNI
              NTGT=NTG+NTGI
              KI1=KIBALE+MONVOL(K1-1+31)
              KI1=KI1+NNT+5*NTGT
              KR1=KRBALE+MONVOL(K1-1+34)
              KR1=KR1+6*NNT+NTGT
              KR2=KR1+NTGT
              IF (IANIM==2143) THEN
                DO J=1,NTGT
                  JJ=MONVOL(KI1-1+J)
                  IF (JJ==0) CYCLE
                  JJ=EL2FA(1+JJ)
                  WA(JJ)=VOLMON(KR1-1+J)
                END DO
              ELSE IF (IANIM==2144) THEN
                DO J=1,NTGT
                  JJ=MONVOL(KI1-1+J)
                  IF (JJ==0) CYCLE
                  JJ=EL2FA(1+JJ)
                  WA(JJ)=VOLMON(KR2-1+J)
                END DO
              END IF
            END IF
          END DO
          IF (NSPMD == 1) THEN
            DO I=1,NBF
              R4 = WA(I)
              CALL WRITE_R_C(R4,1)
            END DO
          ELSE
            IF (ISPMD==0) THEN
              BUF = (NUMELQG+NUMELCG+NUMELTGG)*4
            ELSE
              BUF=1
            END IF
            CALL SPMD_R4GET_PARTN(1,NBF_L,NBPART,IADG,WA,BUF)
          END IF

!
          RETURN
        end subroutine ANIMCALE
!||====================================================================
!||    alevec          ../engine/source/output/anim/generate/monvol_anim.F90
!||--- called by ------------------------------------------------------
!||    genani          ../engine/source/output/anim/generate/genani.F
!||--- calls      -----------------------------------------------------
!||    write_r_c       ../common_source/tools/input_output/write_routtines.c
!||--- uses       -----------------------------------------------------
!||    constant_mod    ../common_source/modules/constant_mod.F
!||    fvbag_mod       ../engine/share/modules/fvbag_mod.F
!||    precision_mod   ../common_source/modules/precision_mod.F90
!||====================================================================
        SUBROUTINE ALEVEC()
! ----------------------------------------------------------------------------------------------------------------------
!                                                        Modules
! ----------------------------------------------------------------------------------------------------------------------
          USE FVBAG_MOD
          use precision_mod, only: WP
          use constant_mod
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          INTEGER :: I, NNS_ANIM, NNTR, J, K, KK, L, LL, N1, N2, N3
          real(kind=WP)&
          &:: VVT(3)
          REAL :: R4
!
          INTEGER, DIMENSION(:), ALLOCATABLE :: NPTR, NPN
          real(kind=WP)&
          &, DIMENSION(:,:), ALLOCATABLE :: VTR, VV
!
          DO I=1,NFVBAG
            NNS_ANIM=FVDATA(I)%NNS_ANIM
            NNTR=FVDATA(I)%NNTR
            ALLOCATE(VTR(3,NNTR), VV(3,NNS_ANIM), NPTR(NNTR),&
            &NPN(NNS_ANIM))
!
            DO J=1,NNTR
              NPTR(J)=0
              VTR(1,J)=ZERO
              VTR(2,J)=ZERO
              VTR(3,J)=ZERO
            END DO
            DO J=1,NNS_ANIM
              NPN(J)=0
              VV(1,J)=ZERO
              VV(2,J)=ZERO
              VV(3,J)=ZERO
            END DO
            DO J=1,FVDATA(I)%NPOLH
              IF (FVDATA(I)%MPOLH(J)==ZERO) CYCLE
              DO K=FVDATA(I)%IFVPADR(J),FVDATA(I)%IFVPADR(J+1)-1
                KK=FVDATA(I)%IFVPOLH(K)
                DO L=FVDATA(I)%IFVTADR(KK),FVDATA(I)%IFVTADR(KK+1)-1
                  LL=FVDATA(I)%IFVPOLY(L)
                  NPTR(LL)=NPTR(LL)+1
                  VTR(1,LL)=VTR(1,LL)+FVDATA(I)%QPOLH(1,J)/&
                  &FVDATA(I)%MPOLH(J)
                  VTR(2,LL)=VTR(2,LL)+FVDATA(I)%QPOLH(2,J)/&
                  &FVDATA(I)%MPOLH(J)
                  VTR(3,LL)=VTR(3,LL)+FVDATA(I)%QPOLH(3,J)/&
                  &FVDATA(I)%MPOLH(J)
                END DO
              END DO
            END DO
            DO J=1,NNTR
              N1=FVDATA(I)%IFVTRI_ANIM(1,J)
              N2=FVDATA(I)%IFVTRI_ANIM(2,J)
              N3=FVDATA(I)%IFVTRI_ANIM(3,J)
              NPN(N1)=NPN(N1)+1
              NPN(N2)=NPN(N2)+1
              NPN(N3)=NPN(N3)+1
              IF (NPTR(J)/=0) THEN
                VVT(1)=VTR(1,J)/NPTR(J)
                VVT(2)=VTR(2,J)/NPTR(J)
                VVT(3)=VTR(3,J)/NPTR(J)
              ELSE
                VVT(1)=ZERO
                VVT(2)=ZERO
                VVT(3)=ZERO
              END IF
              VV(1,N1)=VV(1,N1)+VVT(1)
              VV(2,N1)=VV(2,N1)+VVT(2)
              VV(3,N1)=VV(3,N1)+VVT(3)
              VV(1,N2)=VV(1,N2)+VVT(1)
              VV(2,N2)=VV(2,N2)+VVT(2)
              VV(3,N2)=VV(3,N2)+VVT(3)
              VV(1,N3)=VV(1,N3)+VVT(1)
              VV(2,N3)=VV(2,N3)+VVT(2)
              VV(3,N3)=VV(3,N3)+VVT(3)
            END DO
!
            DO J=1,NNS_ANIM
              R4 = VV(1,J)/NPN(J)
              CALL WRITE_R_C(R4,1)
              R4 = VV(2,J)/NPN(J)
              CALL WRITE_R_C(R4,1)
              R4 = VV(3,J)/NPN(J)
              CALL WRITE_R_C(R4,1)
            END DO
!
            DEALLOCATE(VTR, VV, NPTR, NPN)
          END DO
!
          R4=ZERO
          DO I=1,3
            CALL WRITE_R_C(R4,1)
            CALL WRITE_R_C(R4,1)
            CALL WRITE_R_C(R4,1)
          END DO
!
          RETURN
        end subroutine ALEVEC
      END MODULE ANIM_MONVOL_MOD
