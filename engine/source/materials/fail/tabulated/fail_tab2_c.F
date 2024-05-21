Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  FAIL_TAB2_C                   source/materials/fail/tabulated/fail_tab2_c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|-- calls ---------------
Chd|        TABLE_VINTERP                 source/tools/curve/table_tools.F
Chd|        VINTER2                       source/tools/curve/vinter.F   
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        INTERFACE_TABLE_MOD           share/modules/table_mod.F     
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE FAIL_TAB2_C (
     1     NEL      ,NUPARAM  ,NUVAR    ,NFUNC    ,IFUNC    ,
     2     NPF      ,TABLE    ,TF       ,TIME     ,UPARAM   , 
     3     NGL      ,ALDT     ,DPLA     ,EPSP     ,UVAR     ,     
     4     SIGNXX   ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,            
     5     TEMP     ,FOFF     ,DFMAX    ,TDELE    ,IPT      ,
     6     IPG      ,DMG_FLAG ,DMG_SCALE,NTABLF   ,ITABLF   )   
C---------+---------+---+---+--------------------------------------------
      USE TABLE_MOD
      USE INTERFACE_TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "comlock.inc"
#include      "com04_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: 
     .        NEL,NUPARAM,NUVAR,NTABLF,NGL(NEL),IPT,IPG
      INTEGER, DIMENSION(NTABLF) ,INTENT(IN) :: ITABLF
      INTEGER, INTENT(INOUT) :: 
     .        DMG_FLAG,FOFF(NEL)
      my_real, INTENT(IN) :: 
     .        TIME,UPARAM(NUPARAM),ALDT(NEL),
     .        DPLA(NEL),EPSP(NEL),TEMP(NEL),
     .        SIGNXX(NEL),SIGNYY(NEL),SIGNXY(NEL),
     .        SIGNYZ(NEL),SIGNZX(NEL)
      my_real, INTENT(INOUT) :: 
     .        UVAR(NEL,NUVAR),DFMAX(NEL),TDELE(NEL),
     .        DMG_SCALE(NEL)
      TYPE (TTABLE), INTENT(IN), DIMENSION(NTABLE) :: TABLE 
C!-----------------------------------------------
C!   VARIABLES FOR FUNCTION INTERPOLATION 
C!-----------------------------------------------
      INTEGER, INTENT(IN) :: NPF(SNPC),NFUNC,IFUNC(NFUNC)
      my_real, INTENT(IN) :: TF(STF)
      my_real
     .         FINTER
      EXTERNAL FINTER
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,INDX(NEL),NINDX,ITAB_EPSF,
     .        ITAB_INST,ITAB_SIZE,IREG,
     .        IPOS(NEL,3),NDIM,IPOS2(NEL),
     .        IAD(NEL),ILEN(NEL),LOG_SCALE1,
     .        LOG_SCALE2
      my_real 
     .        FCRIT  ,DN,DCRIT,ECRIT,EXP_REF,EXPO,EL_REF,
     .        SR_REF1,FSCALE_EL,SHRF,BIAXF  ,SR_REF2,
     .        FSCALE_SR,CJC,FSCALE_DLIM,TEMP_REF, FSCALE_TEMP
      my_real
     .        LAMBDA,FAC,DF,INST(NEL)  ,DC(NEL)     ,L0(NEL)     ,
     .        TRIAX(NEL)  ,XI(NEL)     ,EPSF(NEL)   ,EPSL(NEL)   ,
     .        DEPSF(NEL)  ,DEPSL(NEL)  ,XVEC(NEL,3) ,DPL_DEF     ,
     .        COS3THETA   ,P           ,SVM         ,SIZEFAC(NEL),
     .        RATEFAC(NEL),DSIZE(NEL)  ,SOFTEXP(NEL),DLIM(NEL)   ,
     .        TEMPFAC(NEL),TEMPFAC2(NEL),DFT(NEL)   ,VAR(NEL)
C!--------------------------------------------------------------
      !=======================================================================
      ! - INITIALISATION OF COMPUTATION ON TIME STEP
      !=======================================================================
      ! Recovering failure criterion parameters
      FCRIT         = UPARAM(1)
      DN            = UPARAM(4)
      DCRIT         = UPARAM(5) 
      ECRIT         = UPARAM(6)
      EXP_REF       = UPARAM(7)
      EXPO          = UPARAM(8)
      IREG          = NINT(UPARAM(9))
      EL_REF        = UPARAM(10)
      SR_REF1       = UPARAM(11)
      FSCALE_EL     = UPARAM(12)
      SHRF          = UPARAM(13)
      BIAXF         = UPARAM(14)
      SR_REF2       = UPARAM(15)
      FSCALE_SR     = UPARAM(16)
      CJC           = UPARAM(17)
      FSCALE_DLIM   = UPARAM(18)
      TEMP_REF      = UPARAM(19)
      FSCALE_TEMP   = UPARAM(20)
      LOG_SCALE1    = NINT(UPARAM(21))
      LOG_SCALE2    = NINT(UPARAM(22))
c
      ITAB_EPSF = ITABLF(1)
      ITAB_INST = ITABLF(2)
      ITAB_SIZE = ITABLF(3)
c
      ! Set flag for stress softening
      DMG_FLAG = 1
c
      ! Checking element failure and recovering user variable
      DO I=1,NEL
        ! If necking control is activated
        IF ((ITAB_INST > 0).OR.(ECRIT > ZERO)) THEN 
          ! Instability damage
          INST(I) = UVAR(I,1)
          ! Necking critical damage 
          IF (UVAR(I,2) == ZERO) UVAR(I,2) = ONE
          DC(I)   = UVAR(I,2)
        ENDIF   
      END DO
c      
      !====================================================================
      ! - LOOP OVER THE ELEMENT TO COMPUTE THE STRESS STATE QUANTITIES
      !====================================================================       
      DO I=1,NEL
c
        ! Computation of hydrostatic stress, Von Mises stress, and stress triaxiality
        P = THIRD*(SIGNXX(I) + SIGNYY(I))
        ! Von Mises equivalent stress
        SVM = SIGNXX(I)**2 + SIGNYY(I)**2 - SIGNXX(I)*SIGNYY(I) +
     .           THREE*SIGNXY(I)**2
        SVM = SQRT(MAX(SVM,ZERO))
        TRIAX(I) = P/MAX(EM20,SVM)
        IF (TRIAX(I) < -TWO_THIRD) TRIAX(I) = -TWO_THIRD
        IF (TRIAX(I) >  TWO_THIRD) TRIAX(I) =  TWO_THIRD
c
        ! Computation of Lode parameter
        COS3THETA = -HALF*TWENTY7*TRIAX(I)*(TRIAX(I)**2 - THIRD)
        IF (COS3THETA < -ONE ) COS3THETA = -ONE
        IF (COS3THETA > ONE  ) COS3THETA =  ONE
        XI(I) = ONE - TWO*ACOS(COS3THETA)/PI
c
      ENDDO
c      
      !====================================================================
      ! - COMPUTE FACTORS FOR ELEMENT SIZE, STRAIN RATE AND TEMPERATURE
      !====================================================================
      ! At initial time, save the element size 
      IF (UVAR(1,3) == ZERO) UVAR(1:NEL,3) = ALDT(1:NEL)
      L0(1:NEL) = UVAR(1:NEL,3)
c
      ! Compute the softening exponent
      IF (IFUNC(1) > 0) THEN 
        DO I=1,NEL   
          LAMBDA     = L0(I)/EXP_REF
          SOFTEXP(I) = FINTER(IFUNC(1),LAMBDA,NPF,TF,DF) 
          SOFTEXP(I) = EXPO*SOFTEXP(I)
        ENDDO
      ELSE
        SOFTEXP(1:NEL) = EXPO
      ENDIF
c
      ! Compute the temperature dependency factor
      IF (IFUNC(4) > 0) THEN 
        VAR(1:NEL)   = TEMP(1:NEL)/TEMP_REF
        IPOS2(1:NEL) = 1
        IAD(1:NEL)   = NPF(IFUNC(4)) / 2 + 1
        ILEN(1:NEL)  = NPF(IFUNC(4)+1) / 2 - IAD(1:NEL) - IPOS2(1:NEL)
        CALL VINTER2(TF,IAD,IPOS2,ILEN,NEL,VAR,DFT,TEMPFAC)
        TEMPFAC(1:NEL) = FSCALE_TEMP*TEMPFAC(1:NEL)
        TEMPFAC2(1:NEL) = TEMPFAC(1:NEL)
      ELSE
        TEMPFAC(1:NEL)  = ONE
        TEMPFAC2(1:NEL) = ONE
      ENDIF
c
      ! Compute the element size regularization factor 
      IF (ITAB_SIZE > 0) THEN 
        ! Element size scaling dependency
        NDIM = TABLE(ITAB_SIZE)%NDIM
        IF (IREG == 1) THEN 
          SELECT CASE (NDIM)
            ! Scale factor vs element size
            CASE(1)
              XVEC(1:NEL,1)   = L0(1:NEL)/EL_REF
              XVEC(1:NEL,2:3) = ZERO
              IPOS(1:NEL,1:3) = 1
            ! Scale factor vs element size vs strain rate
            CASE(2)
              XVEC(1:NEL,1)   = L0(1:NEL)/EL_REF
              IF (LOG_SCALE1 > 0) THEN 
                DO I = 1,NEL
                  XVEC(I,2) = LOG(MAX(EPSP(I),EM20)/SR_REF1)
                ENDDO 
              ELSE
                XVEC(1:NEL,2) = EPSP(1:NEL)/SR_REF1
              ENDIF
              XVEC(1:NEL,3)   = ZERO
              IPOS(1:NEL,1:3) = 1
          END SELECT
        ELSEIF (IREG == 2) THEN 
          SELECT CASE (NDIM)
            ! Scale factor vs element size
            CASE(1)
              XVEC(1:NEL,1)   = L0(1:NEL)/EL_REF
              XVEC(1:NEL,2:3) = ZERO
              IPOS(1:NEL,1:3) = 1
            ! Scale factor vs element size vs triaxiality
            CASE(2)
              XVEC(1:NEL,1)   = L0(1:NEL)/EL_REF
              XVEC(1:NEL,2)   = TRIAX(1:NEL)
              XVEC(1:NEL,3)   = ZERO
              IPOS(1:NEL,1:3) = 1
              ! Scale factor vs element size vs triaxiality vs Lode parameter
            CASE(3)
              XVEC(1:NEL,1)   = L0(1:NEL)/EL_REF
              XVEC(1:NEL,2)   = TRIAX(1:NEL)
              XVEC(1:NEL,3)   = XI(1:NEL)
              IPOS(1:NEL,1:3) = 1
          END SELECT            
        ENDIF
        CALL TABLE_VINTERP(TABLE(ITAB_SIZE),NEL,NEL,IPOS,XVEC,SIZEFAC,DSIZE)
        SIZEFAC(1:NEL) = SIZEFAC(1:NEL)*FSCALE_EL
        IF (IREG == 1) THEN 
          DO I = 1,NEL
            IF (TRIAX(I) < SHRF) THEN 
              SIZEFAC(I) = ONE
            ELSEIF (TRIAX(I) > BIAXF) THEN 
              SIZEFAC(I) = ONE
            ENDIF
          ENDDO
        ENDIF
      ELSE
        SIZEFAC(1:NEL) = ONE
      ENDIF
c
      ! Compute the strain rate dependency factor
      IF (IFUNC(2) > 0) THEN 
        IF (LOG_SCALE2 > 0) THEN
          DO I = 1,NEL 
            VAR(I) = LOG(MAX(EPSP(I),EM20)/SR_REF2)
          ENDDO 
        ELSE
          VAR(1:NEL) = EPSP(1:NEL)/SR_REF2
        ENDIF
        IPOS2(1:NEL) = 1
        IAD (1:NEL) = NPF(IFUNC(2)) / 2 + 1
        ILEN(1:NEL) = NPF(IFUNC(2)+1) / 2 - IAD(1:NEL) - IPOS2(1:NEL)
        CALL VINTER2(TF,IAD,IPOS2,ILEN,NEL,VAR,DFT,RATEFAC)
        RATEFAC(1:NEL) = FSCALE_SR*RATEFAC(1:NEL)
      ELSEIF (CJC > ZERO) THEN 
        DO I=1,NEL
          IF (EPSP(I) > SR_REF2) THEN 
            RATEFAC(I) = ONE + CJC*LOG(EPSP(I)/SR_REF2)
          ELSE
            RATEFAC(I) = ONE
          ENDIF
        ENDDO
      ELSE
        RATEFAC(1:NEL) = ONE
      ENDIF
c
      ! Compute the damage limit value
      IF (IFUNC(3) > 0) THEN 
        DO I = 1,NEL 
          LAMBDA  = TRIAX(I)
          DLIM(I) = FINTER(IFUNC(3),LAMBDA,NPF,TF,DF) 
          DLIM(I) = FSCALE_DLIM*DLIM(I)
          DLIM(I) = MIN(DLIM(I),ONE)
          DLIM(I) = MAX(DLIM(I),ZERO)
        ENDDO
      ELSE
        DLIM(1:NEL) = ONE
      ENDIF
c
      !====================================================================
      ! - COMPUTATION OF PLASTIC STRAIN AT FAILURE
      !====================================================================   
      IF (ITAB_EPSF > 0) THEN 
        ! Failure plastic strain map dependency
        NDIM = TABLE(ITAB_EPSF)%NDIM
        SELECT CASE (NDIM)
          ! Failure plastic strain vs triaxiality
          CASE (1)
            XVEC(1:NEL,1)   = TRIAX(1:NEL)
            XVEC(1:NEL,2:3) = ZERO
            IPOS(1:NEL,1:3) = 1
          ! Failure plastic strain vs triaxiality vs Lode parameter
          CASE (2)
            XVEC(1:NEL,1)   = TRIAX(1:NEL)
            XVEC(1:NEL,2)   = XI(1:NEL)
            XVEC(1:NEL,3)   = ZERO
            IPOS(1:NEL,1:3) = 1
          ! Failure plastic strain vs triaxiality vs Lode parameter vs temperature
          CASE (3)
            XVEC(1:NEL,1)   = TRIAX(1:NEL)
            XVEC(1:NEL,2)   = XI(1:NEL)
            XVEC(1:NEL,3)   = TEMP(1:NEL)/TEMP_REF
            IPOS(1:NEL,1:3) = 1
            TEMPFAC(1:NEL)  = ONE
        END SELECT
        CALL TABLE_VINTERP(TABLE(ITAB_EPSF),NEL,NEL,IPOS,XVEC,EPSF,DEPSF)
        EPSF(1:NEL) = EPSF(1:NEL)*FCRIT
      ELSE 
        EPSF(1:NEL) = FCRIT
      ENDIF
c
      !====================================================================
      ! - COMPUTATION OF PLASTIC STRAIN AT NECKING
      !====================================================================   
      IF (ITAB_INST > 0) THEN 
        ! Instability plastic strain map dependency
        NDIM = TABLE(ITAB_INST)%NDIM
        SELECT CASE (NDIM)
          ! Instability plastic strain vs triaxiality
          CASE(1)
            XVEC(1:NEL,1)   = TRIAX(1:NEL)
            XVEC(1:NEL,2:3) = ZERO
            IPOS(1:NEL,1:3) = 1
          ! Instability plastic strain vs triaxiality vs Lode 
          CASE(2)
            XVEC(1:NEL,1)   = TRIAX(1:NEL)
            XVEC(1:NEL,2)   = XI(1:NEL)
            XVEC(1:NEL,3)   = ZERO
            IPOS(1:NEL,1:3) = 1
          ! Instability plastic strain vs triaxiality vs Lode vs temperature
          CASE(3)
            XVEC(1:NEL,1)   = TRIAX(1:NEL)
            XVEC(1:NEL,2)   = XI(1:NEL)
            XVEC(1:NEL,3)   = TEMP(1:NEL)/TEMP_REF
            IPOS(1:NEL,1:3) = 1  
            TEMPFAC2(1:NEL) = ONE         
        END SELECT
        CALL TABLE_VINTERP(TABLE(ITAB_INST),NEL,NEL,IPOS,XVEC,EPSL,DEPSL)
        EPSL(1:NEL) = EPSL(1:NEL)*ECRIT
      ELSEIF (ECRIT > ZERO) THEN 
        EPSL(1:NEL) = ECRIT
      ENDIF
c
      !====================================================================
      ! - COMPUTATION OF THE DAMAGE VARIABLE EVOLUTION
      !==================================================================== 
      ! Initialization of element failure index
      NINDX = 0  
      INDX(1:NEL) = 0
c
      ! Loop over the elements 
      DO I=1,NEL
c
        ! If the element is not broken
        IF (FOFF(I) /= 0 .AND. DPLA(I) > ZERO) THEN
c
          ! Needs to initialize damage at a very small value the first time
          IF (DFMAX(I) == ZERO) DFMAX(I) = EM20
          IF (INST(I)  == ZERO) INST(I)  = EM20
c
          ! Compute failure strain damage variable
          DPL_DEF  = DPLA(I)/MAX(EPSF(I)*RATEFAC(I)*SIZEFAC(I)*TEMPFAC(I),EM20)
          DFMAX(I) = DFMAX(I) + DPL_DEF*DN*(DFMAX(I)**(ONE-(ONE/DN)))
          DFMAX(I) = MIN(DFMAX(I),DLIM(I))
          IF (DFMAX(I) >= ONE) THEN 
            NINDX       = NINDX + 1
            INDX(NINDX) = I
            FOFF(I)     = 0
            TDELE(I)    = TIME
          ENDIF
c
          ! Compute the controle necking instability damage
          IF ((ITAB_INST > 0).OR.(ECRIT > ZERO)) THEN 
            DPL_DEF = DPLA(I)/MAX(EPSL(I)*RATEFAC(I)*SIZEFAC(I)*TEMPFAC2(I),EM20)
            INST(I) = INST(I) + DPL_DEF*DN*(INST(I)**(ONE-(ONE/DN)))
            INST(I) = MIN(INST(I),ONE)
            IF ((INST(I) >= ONE).AND.(DC(I) == ONE)) THEN 
              DC(I) = DFMAX(I)
            ENDIF
          ENDIF
c
        ENDIF
      ENDDO
c
      !====================================================================
      ! - UPDATE UVAR AND THE STRESS TENSOR
      !====================================================================
      DO I = 1,NEL 
        IF ((ITAB_INST > 0).OR.(ECRIT > ZERO)) THEN 
          UVAR(I,1) = INST(I)
          UVAR(I,2) = DC(I)
          IF (DFMAX(I) >= DC(I)) THEN 
            IF (DC(I) < ONE) THEN 
              DMG_SCALE(I) = ONE - ((DFMAX(I)-DC(I))/MAX(ONE-DC(I),EM20))**SOFTEXP(I)
            ELSE
              DMG_SCALE(I) = ZERO
            ENDIF
          ELSE
            DMG_SCALE(I) = ONE
          ENDIF
        ELSE
          IF (DFMAX(I) >= DCRIT) THEN 
            IF (DCRIT < ONE) THEN 
              DMG_SCALE(I) = ONE - ((DFMAX(I)-DCRIT)/MAX(ONE-DCRIT,EM20))**SOFTEXP(I)
            ELSE
              DMG_SCALE(I) = ZERO
            ENDIF
          ELSE
            DMG_SCALE(I) = ONE
          ENDIF
        ENDIF
      ENDDO
c
      !====================================================================
      ! - PRINTOUT DATA ABOUT FAILED ELEMENTS
      !====================================================================
      IF (NINDX > 0) THEN        
        DO J=1,NINDX             
          I = INDX(J)         
#include "lockon.inc"         
          WRITE(IOUT, 1000) NGL(I),IPG,IPT,TIME
          WRITE(ISTDO,1000) NGL(I),IPG,IPT,TIME
#include "lockoff.inc" 
        END DO                   
      END IF       
c------------------------
 1000 FORMAT(1X,'FOR SHELL ELEMENT NUMBER el#',I10,
     .          ' FAILURE (TAB2) AT GAUSS POINT ',I3,' LAYER ',I3,
     .          ' AT TIME :',1PE12.4)
c
      END
