Copyright>        OpenRadioss
Copyright>        Copyright (C) 2026 Siemens
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
Copyright>        Commercial Alternative: Simcenter Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
Copyright>        commercial version may interest you: 
Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    iniebcs_nrf_tcar   ../starter/source/boundary_conditions/ebcs/iniebcs_nrf_tcar.F
!||--- called by ------------------------------------------------------
!||    iniebcsp0          ../starter/source/boundary_conditions/ebcs/iniebcsp0.F
!||--- calls      -----------------------------------------------------
!||    ebcs_set_tcarp     ../starter/source/boundary_conditions/ebcs/iniebcs_nrf_tcar.F
!||--- uses       -----------------------------------------------------
!||    inigrav            ../starter/share/modules1/inigrav_mod.F
!||====================================================================
      SUBROUTINE INIEBCS_NRF_TCAR(EBCS_TAB, 
     .                    X, IXS,IXQ,IXTG,
     .                    PM, IPM,MAT_PARAM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD    
      USE INIGRAV 
      USE EBCS_MOD
      USE MATPARAM_DEF_MOD, ONLY : MATPARAM_STRUCT_
      use element_mod , only : nixs,nixq,nixtg
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
      INTEGER,INTENT(IN),TARGET     :: IXS(NIXS,NUMELS),IXQ(NIXQ,NUMELQ), IXTG(NIXTG,NUMELTG)
      my_real, INTENT(IN)           :: X(3,NUMNOD)
      my_real,INTENT(IN)            :: PM(NPROPM,NUMMAT)
      INTEGER,INTENT(IN)            :: IPM(NPROPMI,NUMMAT)
      TYPE(MATPARAM_STRUCT_) ,DIMENSION(NUMMAT) ,INTENT(IN) :: MAT_PARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: IMAT,MLN,IMAT_SUB,NIX,NELS
      INTEGER :: TAGMAT(NUMMAT+1),II, NBMAT, KK
      my_real :: TCP_REF,Xmin,Ymin,Zmin,Xmax,Ymax,Zmax,SSP0,SSP0MAX,LC,LC0MAX,USED_TCARP
      INTEGER, DIMENSION(:, :), POINTER  :: IX
      LOGICAL :: TO_BE_DISPLAYED
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

       SSP0 = ZERO
       SSP0MAX = ZERO
       TAGMAT(1:NUMMAT) = 0
       NELS = -HUGE(NELS)
       NIX = -HUGE(NIX)        
       IX => NULL()
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
       ! CALCULATION OF SSP0MAX           !
       !-----------------------------!
       DO II=1,NELS
         IMAT   = IX(1,II)
         IF(IMAT == 0)CYCLE
         IF(TAGMAT(IMAT)==1)CYCLE
         TAGMAT(IMAT)=1                                                                                            
         MLN    = IPM(2,IMAT)
         IF(MLN == 51 .OR. MLN == 151)THEN
           IF(MLN == 51)THEN
             NBMAT = IPM(5,IMAT)
           ELSE
             NBMAT = MAT_PARAM(IMAT)%MULTIMAT%NB
           ENDIF
           SSP0 = ZERO
           DO KK=1,NBMAT
             IMAT_SUB = MAT_PARAM(IMAT)%MULTIMAT%MID(KK)
             IF(IMAT_SUB > 0) SSP0 = MAX(SSP0, PM(27,IMAT_SUB))
           ENDDO
         ELSE
           SSP0=PM(27,IMAT)
         ENDIF
         SSP0MAX = MAX(SSP0MAX, SSP0)
       ENDDO
 
       !-----------------------------!
       ! LC ESTIMATOR                !
       !-----------------------------!
        ! NB: LC0MAX is the largest extent of the GLOBAL model bounding box
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
       ! CALCULATION AND ASSIGNMENT : Tcp !
       !-----------------------------!  
       IF(SSP0MAX == ZERO)THEN
         TCP_REF = EP20
       ELSE     
         ! Default pressure relaxation time TCAR_P.
         ! the HALF-LIFE of that decay is chosen equals the time for an acoustic wave to travel half the domain,
         !   tau * ln(2) = LC0MAX / (2 * SSP0MAX)   ==>   tau = LC0MAX / (2*SSP0MAX*ln2)
         TCP_REF = LC0MAX/TWO/SSP0MAX/LOG(TWO)
       ENDIF
       DO KK=1,EBCS_TAB%NEBCS
          select type (twf => EBCS_TAB%tab(KK)%poly)
            type is (t_ebcs_nrf)
             TO_BE_DISPLAYED = .FALSE.
             CALL EBCS_SET_TCARP(twf,TCP_REF, TO_BE_DISPLAYED, USED_TCARP)
         end select
       ENDDO

       !-----------------------------!
       ! OUTPUT                      !
       !-----------------------------!          
        IF(TO_BE_DISPLAYED) THEN
          WRITE (IOUT,1001)LC0MAX,SSP0MAX,TCP_REF,USED_TCARP
        ELSE

        ENDIF

 1001 FORMAT(
     .//
     .'     NON REFLECTING FRONTIERS (EBCS)    '/
     .'     -------------------------------    '/
     & 5X,'INITIALIZATION OF GLOBAL PARAMETERS      ',/
     & 5X,'CHARACTERISTIC LENGTH. . . . . . . . . .=',E12.4/
     & 5X,'REFERENCE SOUND SPEED. . . . . . . . . .=',E12.4/
     & 5X,'COMPUTED DEFAULT TCP PARAMETER . . . . .=',E12.4/
     & 5X,'TCP PARAMETER ACTUALLY USED. . . . . . .=',E12.4//)
                           
                                                               
      END SUBROUTINE INIEBCS_NRF_TCAR


C-----------------------------------------------

      
!||====================================================================
!||    ebcs_set_tcarp     ../starter/source/boundary_conditions/ebcs/iniebcs_nrf_tcar.F
!||--- called by ------------------------------------------------------
!||    iniebcs_nrf_tcar   ../starter/source/boundary_conditions/ebcs/iniebcs_nrf_tcar.F
!||--- uses       -----------------------------------------------------
!||====================================================================
      SUBROUTINE EBCS_SET_TCARP(EBCS,TCAR_P,TO_BE_DISPLAYED,USED_TCARP)
      USE EBCS_MOD
#include      "implicit_f.inc"
      
      TYPE(t_ebcs_nrf) :: EBCS
      my_real :: TCAR_P,USED_TCARP
      LOGICAL :: TO_BE_DISPLAYED

           USED_TCARP = EBCS%TCAR_P
           IF(EBCS%TCAR_P == ZERO) THEN
             EBCS%TCAR_P = TCAR_P
             TO_BE_DISPLAYED = .TRUE.
             USED_TCARP = TCAR_P
           ENDIF
      
      END SUBROUTINE
