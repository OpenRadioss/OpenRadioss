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
!||    inivol_set           ../starter/source/initial_conditions/inivol/inivol_set.F
!||--- called by ------------------------------------------------------
!||    init_inivol          ../starter/source/initial_conditions/inivol/init_inivol.F90
!||--- uses       -----------------------------------------------------
!||    message_mod          ../starter/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE INIVOL_SET(UVAR     , NUVAR , NEL     , KVOL         , MLW                         ,
     .                      ELBUF_TAB, NG    , NBSUBMAT, MULTI_FVM    ,
     .                      IDP      , IPART , NFT     , IMID,
     .                      MAT_PARAM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE MULTI_FVM_MOD
      USE ELBUFDEF_MOD
      USE MESSAGE_MOD
      USE CONSTANT_MOD, ONLY : NINE_OVER_10
      USE MULTIMAT_PARAM_MOD , ONLY : M51_N0PHAS, M51_NVPHAS
      USE MATPARAM_DEF_MOD  , ONLY : MATPARAM_STRUCT_
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN)                 :: IMID
      INTEGER,INTENT(IN)                 :: NEL, NUVAR, MLW, NG, NBSUBMAT,IDP,IPART(*),NFT
      my_real,INTENT(INOUT)              :: KVOL(NBSUBMAT,NEL)
      my_real,INTENT(INOUT)              :: UVAR(NEL,NUVAR)
      TYPE(MULTI_FVM_STRUCT),INTENT(IN)  :: MULTI_FVM
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP),INTENT(INOUT) :: ELBUF_TAB
      TYPE (MATPARAM_STRUCT_) ,DIMENSION(NUMMAT) ,INTENT(IN) :: MAT_PARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER                 :: I, IMAT, KK
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(L_BUFEL_) ,POINTER :: LBUF
      my_real                 :: P, P1,P2,P3,P4,SUMVF,RATIO
      my_real                 :: vfrac0(nbsubmat)
      INTEGER                 :: default_SUBMAT_id(NEL)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------

      ! DETERMINE DEFAULT SUBMAT ID
      vfrac0(1:nbsubmat) = MAT_PARAM(IMID)%MULTIMAT%VFRAC(1:NBSUBMAT)
      default_SUBMAT_id(1:NEL) = MAX(1, MAXLOC(vfrac0,1))

      !---CHECK UNOCCUPIPED SUBVOLUME AND FILL WITH DEFAULT PHASE IF SUM(vfrac>1) display an error message.
      DO I=1,NEL
       IF(IPART(I+NFT) /= IDP)CYCLE
        SUMVF = SUM(KVOL(1:NBSUBMAT,I))
        IMAT = default_SUBMAT_id(I)
        IF(SUMVF > ONE+EM06)THEN
          !not expected, user input must be checked : error message
          SUMVF = SUM(KVOL(1:NBSUBMAT,I))
          SUMVF = SUMVF - KVOL(IMAT,I)
          IF(SUMVF <= ONE .AND. SUMVF > ZERO)THEN  ! sumvf is here calculated in range 2:nbsumat
            KVOL(IMAT,I)=ONE-SUMVF
          ELSE
            SUMVF = SUM(KVOL(1:NBSUBMAT,I))
            RATIO=ONE/SUMVF
            KVOL(1:NBSUBMAT,I)=RATIO*KVOL(1:NBSUBMAT,I)
          ENDIF
        ELSEIF(SUMVF < ONE-EM06)THEN
          !fill unoccupied subvolume with phase-1
          KVOL(IMAT,I) = KVOL(IMAT,I) + ONE-SUMVF
        ELSEIF(SUMVF >= ONE-EM06 .AND. SUMVF <= ONE+EM06)THEN
          !get rid of precision issue so that sumvf is exactly 1.000000
          RATIO=ONE/SUMVF
          KVOL(1:NBSUBMAT,I)=RATIO*KVOL(1:NBSUBMAT,I)
        ENDIF
      ENDDO

      !---set initial volumetric fraction result (KVOL) in material buffer (+ posteriori checks)
      IF(MLW == 51)THEN
        DO IMAT=1,4
          KK = M51_N0PHAS + (IMAT-1)*M51_NVPHAS
          DO I=1,NEL
            IF(IPART(I+NFT) /= IDP)CYCLE
            UVAR(I,1+KK)   = KVOL(IMAT,I)
            UVAR(I,23+KK)  = KVOL(IMAT,I)
          ENDDO
        ENDDO
        DO I=1,NEL
          IF(IPART(I+NFT) /= IDP)CYCLE
          KK  = M51_N0PHAS + (1-1)*M51_NVPHAS
          P1  = UVAR(I,18+KK)
          KK  = M51_N0PHAS + (2-1)*M51_NVPHAS
          P2  = UVAR(I,18+KK)
          KK  = M51_N0PHAS + (3-1)*M51_NVPHAS
          P3  = UVAR(I,18+KK)
          KK  = M51_N0PHAS + (4-1)*M51_NVPHAS
          P4  = UVAR(I,18+KK)
          SUMVF=SUM(KVOL(1:NBSUBMAT,I))
          P   = KVOL(1,I)*P1 + KVOL(2,I)*P2 + KVOL(3,I)*P3 + KVOL(4,I)*P4
          UVAR(I,4) = P
        ENDDO
      ELSEIF(MLW == 37)THEN
        DO I=1,NEL
          IF(IPART(I+NFT) /= IDP)CYCLE
          UVAR(I,4)  = KVOL(1,I)
          UVAR(I,5)  = KVOL(2,I)
        ENDDO
      ELSEIF(MLW == 151)THEN
        GBUF => ELBUF_TAB(NG)%GBUF
        DO IMAT=1,MULTI_FVM%NBMAT
          LBUF => ELBUF_TAB(NG)%BUFLY(IMAT)%LBUF(1,1,1)
          DO I=1,NEL
            IF(IPART(I+NFT) /= IDP)CYCLE
            LBUF%VOL(I) = KVOL(IMAT,I) * GBUF%VOL(I)
          ENDDO
        ENDDO
      ENDIF


      RETURN
      END
