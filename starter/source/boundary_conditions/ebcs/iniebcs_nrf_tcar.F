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
Chd|  INIEBCS_NRF_TCAR              source/boundary_conditions/ebcs/iniebcs_nrf_tcar.F
Chd|-- called by -----------
Chd|        INIEBCSP0                     source/boundary_conditions/ebcs/iniebcsp0.F
Chd|-- calls ---------------
Chd|        EBCS_SET_TCARP                source/boundary_conditions/ebcs/iniebcs_nrf_tcar.F
Chd|        EBCS_MOD                      ../common_source/modules/ebcs_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INIGRAV                       share/modules1/inigrav_mod.F  
Chd|====================================================================
      SUBROUTINE INIEBCS_NRF_TCAR(EBCS_TAB,
     .                    IPARG,X, IXS,IXQ,IXTG,
     .                    IPARTS,IPARTQ,IPARTTG,
     .                    PM, IPM)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE INIGRAV
      USE EBCS_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "com01_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(t_ebcs_tab), INTENT(INOUT) :: EBCS_TAB
      INTEGER                       :: IPARG(NPARG,NGROUP)
      INTEGER,INTENT(IN),TARGET     :: IXS(NIXS,NUMELS),IXQ(NIXQ,NUMELQ), IXTG(NIXTG,NUMELTG)
      INTEGER, INTENT(IN)           :: IPARTS(NUMELS), IPARTQ(NUMELQ), IPARTTG(NUMELTG)
      my_real, INTENT(IN)           :: X(3,NUMNOD)
      my_real,INTENT(IN)            :: PM(NPROPM,NUMMAT)
      INTEGER,INTENT(IN)            :: IPM(NPROPMI,NUMMAT)
      TYPE (ELBUF_STRUCT_), TARGET  :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: IMAT,MLN,IADBUF,IMAT_SUB,NIX,NELS,NBPHASE
      INTEGER :: TAGMAT(NUMMAT+1),II, IFORM, NBMAT, KK
      my_real :: TCP_REF,Xmin,Ymin,Zmin,Xmax,Ymax,Zmax,TMP,SSP0,SSP0MAX,LC,LC0MAX
      INTEGER, DIMENSION(:, :), POINTER  :: IX
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

       SSP0 = ZERO
       TAGMAT(1:NUMMAT) = 0

       IF(NUMELS>0)THEN
         NELS = NUMELS
         NIX=NIXS
         IX => IXS(1:NIX, 1:NELS)
       ELSEIF(NUMELQ >0)THEN
         NELS = NUMELQ
         NIX=NIXQ
         IX => IXQ(1:NIX, 1:NELS)
       ELSEIF(N2D /=0 .AND. NUMELTG >0)THEN
         NELS = NUMELTG
         NIX=NIXTG
         IX => IXTG(1:NIX, 1:NELS)
       ENDIF

       !-----------------------------!
       ! CALCUL DE SSP0MAX           !
       !-----------------------------!
       NBPHASE = 0
       DO II=1,NELS
         IMAT   = IX(1,II)
         IF(IMAT == 0)CYCLE
         IF(TAGMAT(IMAT)==1)CYCLE
         TAGMAT(IMAT)=1
         MLN    = IPM(2,IMAT)
         IADBUF = IPM(7,IMAT)
         IF(MLN == 51)THEN
           !UPRM => BUFMAT(IADBUF:IADBUF+275)
           !TMP  = UPRM(31)
           !IFORM = NINT(TMP)
           !IF(IFORM>1)CYCLE
           !SSP0 = UPRM(124)
           !SSP0 = MAX(SSP0,UPRM(174))
           !SSP0 = MAX(SSP0,UPRM(224))
           !SSP0 = MAX(SSP0,UPRM(273))
           SSP0=PM(27,IMAT)
           NBPHASE=MAX(NBPHASE,4)
         ELSEIF(MLN == 151)THEN
           NBMAT = IPM(20,IMAT)
           NBPHASE = MAX(NBPHASE,NBMAT)
           IMAT_SUB = IPM(20+1,IMAT)
           SSP0 = IPM(27,IMAT_SUB)
           DO KK=2,NBMAT
             IMAT_SUB = IPM(20+KK,IMAT)
             SSP0=MAX(SSP0,PM(27,IMAT_SUB))
           ENDDO
         ELSE
           SSP0=PM(27,IMAT)
         ENDIF
       ENDDO
       SSP0MAX = SSP0

       !-----------------------------!
       ! ESTIMATEUR DE LC            !
       !-----------------------------!
        Xmin   = X(1,1)
        Ymin   = X(2,1)
        Zmin   = X(3,1)
        Xmax   = X(1,1)
        Ymax   = X(2,1)
        Zmax   = X(3,1)
        DO II=1,NUMNOD
          Xmin = MIN(Xmin,X(1,II))
          Ymin = MIN(Ymin,X(2,II))
          Zmin = MIN(Zmin,X(3,II))
          Xmax = MAX(Xmax,X(1,II))
          Ymax = MAX(Ymax,X(2,II))
          Zmax = MAX(Zmax,X(3,II))
        ENDDO
        LC     = Xmax-Xmin
        LC     = MAX(LC,Ymax-Ymin)
        LC     = MAX(LC,Zmax-Zmin)
        LC0MAX = LC

       !-----------------------------!
       ! CALCUL ET AFFECTATION : Tcp !
       !-----------------------------!
       IF(SSP0MAX == ZERO)THEN
         TCP_REF = EP20
       ELSE
         TCP_REF    = LC0MAX/TWO/SSP0MAX/LOG(TWO)
       ENDIF
       DO KK=1,EBCS_TAB%NEBCS
          select type (twf => EBCS_TAB%tab(KK)%poly)
            type is (t_ebcs_nrf)
             CALL EBCS_SET_TCARP(twf,TCP_REF)
         end select
       ENDDO

       !-----------------------------!
       ! ALLOCATION BUFFER VOL FRAC  !
       !-----------------------------!
        !EBCS%NBMAT= NBPHASE
        !IF(NBPHASE > 0)THEN
        !  ALLOCATE(EBCS%PHASE_ALPHA(NBPHASE,EBCS%NB_ELEM))
        !  EBCS%PHASE_ALPHA(1:NBPHASE,1:EBCS%NB_ELEM) = ZERO
        !ENDIF

       !-----------------------------!
       ! OUTPUT                      !
       !-----------------------------!
        WRITE (IOUT,1001)LC0MAX,SSP0MAX,TCP_REF


 1001 FORMAT(
     .//
     .'     NON REFLECTING FRONTIERS (EBCS)    '/
     .'     -------------------------------    '/
     & 5X,'INITIALIZATION OF GLOBAL PARAMETERS      ',/
     & 5X,'CHARACTERISTIC LENGTH. . . . . . . . . .=',E12.4/
     & 5X,'REFERENCE SOUND SPEED. . . . . . . . .  =',E12.4/
     & 5X,'CHARACTERISTIC TIME (TCP). . . . . . . .=',E12.4//)


      END SUBROUTINE INIEBCS_NRF_TCAR


C-----------------------------------------------


Chd|====================================================================
Chd|  EBCS_SET_TCARP                source/boundary_conditions/ebcs/iniebcs_nrf_tcar.F
Chd|-- called by -----------
Chd|        INIEBCS_NRF_TCAR              source/boundary_conditions/ebcs/iniebcs_nrf_tcar.F
Chd|-- calls ---------------
Chd|        EBCS_MOD                      ../common_source/modules/ebcs_mod.F
Chd|====================================================================
      SUBROUTINE EBCS_SET_TCARP(EBCS,TCAR_P)
      USE EBCS_MOD
#include      "implicit_f.inc"

      TYPE(t_ebcs_nrf) :: EBCS
      my_real :: TCAR_P

           IF(EBCS%TCAR_P == ZERO) EBCS%TCAR_P = TCAR_P

      END SUBROUTINE
