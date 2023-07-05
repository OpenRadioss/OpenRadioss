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
Chd|  INIGRAV_M51                   source/initial_conditions/inigrav/inigrav_m51.F
Chd|-- called by -----------
Chd|        INIGRAV_LOAD                  source/initial_conditions/inigrav/inigrav_load.F
Chd|-- calls ---------------
Chd|        POLYSOLVE                     source/initial_conditions/inigrav/inigrav_m51.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE INIGRAV_M51(NELG, NEL , NG   , MATID, IPM, GRAV0, DEPTH, PM,     BUFMAT, ELBUF_TAB,
     .                       PSURF,LIST, ALE_CONNECTIVITY, NV46 , IX , NIX  , NFT  , BUFMATG, IPARG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
!     NPROPMI, NPROPM
#include      "param_c.inc"
!     NGROUP
#include      "com01_c.inc"
#include      "mmale51_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL, NG, MATID, IPM(NPROPMI, *),LIST(NEL),NELG,IX(NIX,*),NFT,NIX,
     .                       IPARG(NPARG,NGROUP)
      my_real, INTENT(IN) :: GRAV0, DEPTH(*), PM(NPROPM, *), BUFMAT(*),PSURF,BUFMATG(*)
      TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET, INTENT(IN) :: ELBUF_TAB
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I, K1, K2, K3, K4, NUVAR,K,J,IFORM,IFORMv,NV46,IV,IADBUF,ML,NGv,KTY,KLT,N,MFT,IS,NELGv
      my_real ::  PGRAV, RHO, GAM, RHO0, 
     .     ALPHA1, ALPHA2, ALPHA3, ALPHA4, 
     .     C01, C11, C21, C31, C41, C51, 
     .     C02, C12, C22, C32, C42, C52, 
     .     C03, C13, C23, C33, C43, C53, 
     .     C04, PEXT,
     .     RHO10, RHO20, RHO30, RHO40, RHO1, RHO2, RHO3, RHO4, MU, 
     .     EINT1, EINT2, EINT3, EINT4, VOL, VOL1, VOL2, VOL3,
     .     EINT10, EINT20, EINT30, EINT40
      TYPE(G_BUFEL_), POINTER :: GBUF
      TYPE(BUF_MAT_) ,POINTER :: MBUF  , MBUFv
      INTEGER :: IAD
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

C LIST IS SUBGROUP TO TREAT : ONLY ELEM WITH RELEVANT PARTS ARE KEPT
C NEL IS ISEZ OF LIST
C NELG IS SIZE OF ORIGINAL GROUP : needed to shift indexes in GBUF%SIG & MBUF%VAR

      !Global buffer
      GBUF => ELBUF_TAB(NG)%GBUF
      !Material buffer
      MBUF  => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)

      IFORM = NINT(BUFMAT(31))
      K1 = N0PHAS + (1 - 1) * NVPHAS
      K2 = N0PHAS + (2 - 1) * NVPHAS
      K3 = N0PHAS + (3 - 1) * NVPHAS
      K4 = N0PHAS + (4 - 1) * NVPHAS
      NUVAR = IPM(8, MATID)
      PEXT  = BUFMAT(8)
      IF(IFORM/=6)THEN
      !all value are in UPARAM=>BUFMAT()
        !Initial densities
        RHO10 = BUFMAT(9)
        RHO20 = BUFMAT(10)
        RHO30 = BUFMAT(11)
        RHO40 = BUFMAT(12)
        !Initial energies
        EINT10 = BUFMAT(32)
        EINT20 = BUFMAT(33)
        EINT30 = BUFMAT(34)
        EINT40 = BUFMAT(48)
      !Internal energies
        ! EINT1 = MBUF%VAR(I + (K1 + 8 - 1) * NELG)
        ! EINT2 = MBUF%VAR(I + (K2 + 8 - 1) * NELG)
        ! EINT3 = MBUF%VAR(I + (K3 + 8 - 1) * NELG)
        ! EINT4 = MBUF%VAR(I + (K4 + 8 - 1) * NELG)
        !Material 1
        C01 = BUFMAT(35)
        C11 = BUFMAT(12)
        C21 = BUFMAT(15)
        C31 = BUFMAT(18)
        C41 = BUFMAT(22)
        C51 = BUFMAT(25)
        !Material 2
        C02 = BUFMAT(36)
        C12 = BUFMAT(13)
        C22 = BUFMAT(16)
        C32 = BUFMAT(20)
        C42 = BUFMAT(23)  
        C52 = BUFMAT(26)
        !Material 3
        C03 = BUFMAT(37)
        C13 = BUFMAT(14)
        C23 = BUFMAT(17)
        C33 = BUFMAT(21)
        C43 = BUFMAT(24)  
        C53 = BUFMAT(27)
        !Material 4
        C04 = BUFMAT(49)
      ENDIF
      DO K = 1, NEL
         I = LIST(K)
         EINT1 = EINT10
         EINT2 = EINT20
         EINT3 = EINT30
         EINT4 = EINT40
         IF(IFORM==6)THEN
         !value are not in buffer UPARAM=>BUFMAT but are cell dependent
           ML     = 0
           IFORMv = 0
           IAD = ALE_CONNECTIVITY%ee_connect%iad_connect(I + NFT)
           DO J=1,ALE_CONNECTIVITY%ee_connect%iad_connect(I + NFT + 1) - ALE_CONNECTIVITY%ee_connect%iad_connect(I + NFT)
             IV = ALE_CONNECTIVITY%ee_connect%connected(IAD + J - 1)
             IFORMv = 1000
             IF(IV/=0)            ML     = NINT(PM(19,IX(1,IV)))  !ix is here local I and not IV
             IF(IV/=0)            IADBUF = IPM(7,IX(1,IV))      
             IF(IV/=0.AND.ML==51) IFORMv  = BUFMATG(IADBUF+31-1)     
             IF(ML==51.AND.IFORMv<=1)EXIT                             
           ENDDO
           DO N=1,NGROUP                                    
              KTY = IPARG(5,N)                              
              KLT = IPARG(2,N)                              
              MFT = IPARG(3,N)                              
              IF (KTY==1 .AND. IV<=KLT+MFT) EXIT      
           ENDDO                                            
           IF (KTY/=1 .OR. IV>KLT+MFT) CYCLE 
           NGv   = N
           MBUFv => ELBUF_TAB(NGv)%BUFLY(1)%MAT(1,1,1)
           NELGv = KLT
           IS    = IV-MFT
           RHO10 = BUFMATG(IADBUF-1+09)
           RHO20 = BUFMATG(IADBUF-1+10)
           RHO30 = BUFMATG(IADBUF-1+11)
           RHO40 = BUFMATG(IADBUF-1+12)
           !energies
           EINT1 = BUFMATG(IADBUF-1+32)
           EINT2 = BUFMATG(IADBUF-1+33)
           EINT3 = BUFMATG(IADBUF-1+34)
           EINT4 = BUFMATG(IADBUF-1+48)
           !Material 1
           C01 = BUFMATG(IADBUF-1+35)
           C11 = BUFMATG(IADBUF-1+12)
           C21 = BUFMATG(IADBUF-1+15)
           C31 = BUFMATG(IADBUF-1+18)
           C41 = BUFMATG(IADBUF-1+22)
           C51 = BUFMATG(IADBUF-1+25)
           !Material 2
           C02 = BUFMATG(IADBUF-1+36)
           C12 = BUFMATG(IADBUF-1+13)
           C22 = BUFMATG(IADBUF-1+16)
           C32 = BUFMATG(IADBUF-1+20)
           C42 = BUFMATG(IADBUF-1+23)  
           C52 = BUFMATG(IADBUF-1+26)
           !Material 3
           C03 = BUFMATG(IADBUF-1+37)
           C13 = BUFMATG(IADBUF-1+14)
           C23 = BUFMATG(IADBUF-1+17)
           C33 = BUFMATG(IADBUF-1+21)
           C43 = BUFMATG(IADBUF-1+24)  
           C53 = BUFMATG(IADBUF-1+27)
           !Material 4
           C04 = BUFMATG(IADBUF-1+49)
           !vol frac
           ALPHA1 = MBUFv%VAR(IS + (K1 + 23 - 1) * NELGv)
           ALPHA2 = MBUFv%VAR(IS + (K2 + 23 - 1) * NELGv)
           ALPHA3 = MBUFv%VAR(IS + (K3 + 23 - 1) * NELGv)
           ALPHA4 = MBUFv%VAR(IS + (K4 + 23 - 1) * NELGv)
         ELSE
           !Volumic fractions
           ALPHA1 = MBUF%VAR(I + (K1 + 23 - 1) * NELG)
           ALPHA2 = MBUF%VAR(I + (K2 + 23 - 1) * NELG)
           ALPHA3 = MBUF%VAR(I + (K3 + 23 - 1) * NELG)
           ALPHA4 = MBUF%VAR(I + (K4 + 23 - 1) * NELG)
         ENDIF

      !Mean initial density
         RHO0 = ALPHA1 * RHO10 + ALPHA2 * RHO20 + ALPHA3 * RHO30 + ALPHA4 * RHO40        
      !Hydrostatic pressure
         PGRAV = PSURF - RHO0  * GRAV0 * DEPTH(K)
      !Solve for partial densities
         VOL  = GBUF%VOL(I)
         VOL1 = ALPHA1*VOL
         VOL2 = ALPHA2*VOL
         VOL3 = ALPHA3*VOL
C
         CALL POLYSOLVE(C01, C11, C21, C31, C41, C51, PGRAV, EINT1, MU, RHO10, VOL1,PEXT,IX(11,I+NFT))
         RHO1 = RHO10 * (MU + ONE)
         CALL POLYSOLVE(C02, C12, C22, C32, C42, C52, PGRAV, EINT2, MU, RHO20, VOL2,PEXT,IX(11,I+NFT))
         RHO2 = RHO20 * (MU + ONE)
         CALL POLYSOLVE(C03, C13, C23, C33, C43, C53, PGRAV, EINT3, MU, RHO30, VOL3,PEXT,IX(11,I+NFT))
         RHO3 = RHO30 * (MU + ONE)
      !Store partial densities
         MBUF%VAR(I + (K1 + 09 - 1) * NELG) = RHO1  !initialize rho(t=0)
         MBUF%VAR(I + (K2 + 09 - 1) * NELG) = RHO2
         MBUF%VAR(I + (K3 + 09 - 1) * NELG) = RHO3
         MBUF%VAR(I + (K1 + 12 - 1) * NELG) = RHO1  !initialize rhoOLD(t=0)
         MBUF%VAR(I + (K2 + 12 - 1) * NELG) = RHO2
         MBUF%VAR(I + (K3 + 12 - 1) * NELG) = RHO3
         MBUF%VAR(I + (K1 + 20 - 1) * NELG) = RHO1  !keep rho0 (initial state is element dependent)
         MBUF%VAR(I + (K2 + 20 - 1) * NELG) = RHO2
         MBUF%VAR(I + (K3 + 20 - 1) * NELG) = RHO3
      !Store pressures
         MBUF%VAR(I + (K1 + 18 - 1) * NELG) = PGRAV !P(t=0)
         MBUF%VAR(I + (K2 + 18 - 1) * NELG) = PGRAV
         MBUF%VAR(I + (K3 + 18 - 1) * NELG) = PGRAV
         MBUF%VAR(I + (K4 + 18 - 1) * NELG) = PGRAV
      !Internal energies
         IF (RHO10 /= ZERO) THEN
            MBUF%VAR(I + (K1 + 08 - 1) * NELG) = EINT1 * RHO1 / RHO10 !rho.e(t)
            MBUF%VAR(I + (K1 + 21 - 1) * NELG) = EINT1 * RHO1 / RHO10 !rho0.e0
         ELSE
            MBUF%VAR(I + (K1 + 08 - 1) * NELG) = ZERO
            MBUF%VAR(I + (K1 + 21 - 1) * NELG) = ZERO
         ENDIF
         IF (RHO20 /= ZERO) THEN
            MBUF%VAR(I + (K2 + 08 - 1) * NELG) = EINT2 * RHO2 / RHO20 !rho.e(t)
            MBUF%VAR(I + (K2 + 21 - 1) * NELG) = EINT2 * RHO2 / RHO20 !rho0.e0
         ELSE
            MBUF%VAR(I + (K2 + 08 - 1) * NELG) = ZERO 
            MBUF%VAR(I + (K2 + 21 - 1) * NELG) = ZERO
         ENDIF
         IF (RHO30 /= ZERO) THEN
            MBUF%VAR(I + (K3 + 08 - 1) * NELG) = EINT3 * RHO3 / RHO30 !rho.e(t)
            MBUF%VAR(I + (K3 + 21 - 1) * NELG) = EINT3 * RHO3 / RHO30 !rho0.e0
         ELSE
            MBUF%VAR(I + (K3 + 08 - 1) * NELG) = ZERO
            MBUF%VAR(I + (K3 + 21 - 1) * NELG) = ZERO
         ENDIF

         GBUF%RHO(I)            = ALPHA1 * RHO1 + ALPHA2 * RHO2 + ALPHA3 * RHO3 + ALPHA4 * RHO40
         GBUF%EINT(I)           = ALPHA1 * EINT1+ ALPHA2 * EINT2+ ALPHA3 * EINT3+ ALPHA4 * EINT4
        
         GBUF%SIG(I)            = - PGRAV
         GBUF%SIG(I + NELG)     = - PGRAV
         GBUF%SIG(I + 2 * NELG) = - PGRAV
         
         MBUF%VAR(I + 3 * NELG) = PGRAV

      ENDDO
      
      END SUBROUTINE INIGRAV_M51

Chd|====================================================================
Chd|  POLYSOLVE                     source/initial_conditions/inigrav/inigrav_m51.F
Chd|-- called by -----------
Chd|        INIGRAV_M51                   source/initial_conditions/inigrav/inigrav_m51.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE POLYSOLVE(C0, C1, C2, C3, C4, C5, PRES, EINT, MU, RHO0, VOL0, PEXT, UID)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real, INTENT(IN)    :: C0, C1, C2, C3, C4, C5, PRES, PEXT
      my_real, INTENT(OUT)   :: MU
      my_real, INTENT(INOUT) :: RHO0, EINT, VOL0
      INTEGER, INTENT(IN)    :: UID  !for debug purpose
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real :: TOL, ERROR, FUNC, DFUNC, INCR, VOL, VOL_prev,PP
      INTEGER :: ITER, MAX_ITER
      LOGICAL :: CONT
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      ITER        = 0
      MAX_ITER    = 30
      TOL         = EM4
      CONT        = .TRUE.
      VOL         = VOL0
      VOL_prev    = VOL0
      IF(VOL0==ZERO)VOL0=ONE
      !Initial guess for MU
      MU          = ZERO
      DO WHILE (CONT .AND. ITER < MAX_ITER)
         !pressure difference
         FUNC     = (C0 + C1 * MU + C2 * MAX(ZERO, MU)*MU + C3 * MAX(ZERO, MU)*MU*MU + (C4 + C5 * MU) * EINT) - PRES
         !total derivative
         DFUNC    = C1 + TWO * C2 * MAX(ZERO, MU) + THREE * C3 * MU*MAX(MU,ZERO) + C5 * EINT 
         DFUNC   = DFUNC + (PEXT+FUNC+PRES)/(ONE+MU)/(ONE+MU)*(C4+C5*MU)
         IF(DFUNC==ZERO)EXIT   !iform=6 cell with no adj elem (not automatically initialized)
         ! MU INCREMENT
         INCR     = - FUNC / DFUNC
         If(INCR==ZERO)EXIT  !P(mu=0)=PRES nothing to do
         ERROR    = ABS(FUNC)
         IF(ABS(MU+INCR)>EM20) ERROR = MAX(ERROR, ABS(INCR / (MU + INCR)))
         MU       = MU + INCR
         !ENERGY INCREMENT
         VOL      = VOL0/(ONE+MU)
         EINT     = EINT - (PEXT+FUNC+PRES)*(VOL-VOL_prev)/VOL0 
         VOL_prev = VOL
         ITER     = ITER + 1
         IF (ERROR < TOL) THEN
           CONT   = .FALSE.
         ENDIF
      ENDDO
      !IF(UID==16866)print *,"16866,  P=", (C0 + C1 * MU + C2 * MAX(ZERO, MU) ** 2 + C3 * MU ** 3 + (C4 + C5 * MU) * EINT) 
      !IF(UID==16866)print *,"16866,  E=",  EINT
      !IF(UID==16866)print *,"16866, MU=", MU
      !IF(UID==16866)print *, "ITER=", ITER
      END SUBROUTINE POLYSOLVE
