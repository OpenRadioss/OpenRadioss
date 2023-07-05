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
Chd|  SIGEPS121C                    source/materials/mat/mat121/sigeps121c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|        MAT121C_NEWTON                source/materials/mat/mat121/mat121c_newton.F
Chd|        MAT121C_NICE                  source/materials/mat/mat121/mat121c_nice.F
Chd|        VINTER2                       source/tools/curve/vinter.F   
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SIGEPS121C(
     1     NEL     ,NGL     ,NUPARAM ,NUVAR   ,NFUNC   ,IFUNC   ,NPF     ,
     2     TF      ,TIMESTEP,TIME    ,UPARAM  ,UVAR    ,RHO     ,PLA     ,
     3     DPLA    ,SOUNDSP ,EPSD    ,GS      ,THK     ,THKLY   ,OFF     ,
     4     DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5     EPSPXX  ,EPSPYY  ,EPSPXY  ,EPSPYZ  ,EPSPZX  ,EPSP    ,EPSPL   ,
     6     SIGOXX  ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     7     SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     8     SIGY    ,ET      ,VARNL   ,INLOC   ,DT      ,
     9     IPG     ,IPT     ,NPTR    ,NPTS    ,NPTT    ,
     A     BUFLY   ,OFFL    )
C
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C O M M O N
C-----------------------------------------------
#include      "com08_c.inc"
#include      "units_c.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NUPARAM,NUVAR,NFUNC,INLOC,
     .        IPG,IPT,NPF(*),NGL(NEL),
     .        IFUNC(NFUNC),NPTR,NPTS,NPTT
      my_real 
     .   TIMESTEP,TIME,TF(*),UPARAM(NUPARAM)
      my_real,DIMENSION(NEL), INTENT(IN) :: 
     .   RHO,THKLY,GS,DT,
     .   DEPSXX,DEPSYY,DEPSXY,DEPSYZ,DEPSZX,
     .   EPSPXX,EPSPYY,EPSPXY,EPSPYZ,EPSPZX,
     .   SIGOXX,SIGOYY,SIGOXY,SIGOYZ,SIGOZX
      my_real,DIMENSION(NEL), INTENT(OUT)   :: 
     .   SOUNDSP,SIGNXX,SIGNYY,SIGNXY,SIGNYZ,SIGNZX
      my_real,DIMENSION(NEL) :: 
     .   SIGY,ET,EPSP,EPSPL
      my_real,DIMENSION(NEL), INTENT(INOUT) :: 
     .   PLA,EPSD,THK,OFF,VARNL,DPLA,OFFL
      my_real,DIMENSION(NEL,NUVAR), INTENT(INOUT) :: 
     .   UVAR
      TYPE(BUF_LAY_) :: BUFLY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IRES,Ifail,NINDX,NINDX2,INDX(NEL),INDX2(NEL),
     .        IPOS(NEL),IAD(NEL),ILEN(NEL),IR,IS,IT
      my_real DTMIN,Xscale_FAIL,Yscale_FAIL,S1,S2,Q,S11(NEL),
     .        S22(NEL),R_INTER,DFDEPSD(NEL),FAIL(NEL),SEQ(NEL)
C=======================================================================
c  
      IRES        = NINT(UPARAM(11)) ! Plastic projection method
                                     !  = 1 => Nice method
                                     !  = 2 => Newton-iteration method
      Ifail       = NINT(UPARAM(13)) ! Failure criterion flag
                                     !  = 0 => Von Mises stress
                                     !  = 1 => Plastic strain
                                     !  = 2 => Maximum princ. stress + 
                                     !         abs(Minimum princ. stress)
                                     !  = 3 => Maximum princ. stress
      DTMIN       = UPARAM(15)       ! Minimal timestep for element deletion
      Xscale_FAIL = UPARAM(22)       ! Strain-rate scale factor for failure criterion function
      Yscale_FAIL = UPARAM(23)       ! Ordinate scale factor for failure criterion function
     
c--------------------------                        
      SELECT CASE (IRES)
c      
        CASE(1)
c
          CALL MAT121C_NICE(
     1         NEL     ,NGL     ,NUPARAM ,NUVAR   ,NFUNC   ,IFUNC   ,NPF     ,
     2         TF      ,TIMESTEP,TIME    ,UPARAM  ,UVAR    ,RHO     ,PLA     ,
     3         DPLA    ,SOUNDSP ,EPSD    ,GS      ,THK     ,THKLY   ,OFF     ,
     4         DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5         EPSPXX  ,EPSPYY  ,EPSPXY  ,EPSPYZ  ,EPSPZX  ,
     6         SIGOXX  ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     7         SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     8         SIGY    ,ET      ,VARNL   ,SEQ     ,INLOC   )
c        
        CASE(2)
c
          CALL MAT121C_NEWTON(
     1         NEL     ,NGL     ,NUPARAM ,NUVAR   ,NFUNC   ,IFUNC   ,NPF     ,
     2         TF      ,TIMESTEP,TIME    ,UPARAM  ,UVAR    ,RHO     ,PLA     ,
     3         DPLA    ,SOUNDSP ,EPSD    ,GS      ,THK     ,THKLY   ,OFF     ,
     4         DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5         EPSPXX  ,EPSPYY  ,EPSPXY  ,EPSPYZ  ,EPSPZX  ,
     6         SIGOXX  ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     7         SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     8         SIGY    ,ET      ,VARNL   ,SEQ     ,INLOC   )
c      
      END SELECT    
c
      ! Store strain-rate for output
      EPSP(1:NEL)  = EPSD(1:NEL)    
      ! Store strain-rate for failure criteria
      EPSPL(1:NEL) = EPSD(1:NEL)
c
      !--------------------------------------------------------------------
      ! Failure
      !--------------------------------------------------------------------
c
      ! Compute failure criterion value
      IF (IFUNC(4) > 0) THEN 
        IPOS(1:NEL) = 1
        IAD (1:NEL) = NPF(IFUNC(4)) / 2 + 1
        ILEN(1:NEL) = NPF(IFUNC(4)+1) / 2 - IAD(1:NEL) - IPOS(1:NEL)
        CALL VINTER2(TF,IAD,IPOS,ILEN,NEL,EPSD/Xscale_FAIL,DFDEPSD,FAIL)
        FAIL(1:NEL) = Yscale_FAIL*FAIL(1:NEL)
      ENDIF
c
      ! Checking elements deletion criteria
      NINDX  = 0
      NINDX2 = 0
      IF (DTMIN > ZERO .OR. IFUNC(4) > 0) THEN
        DO I = 1,NEL  
c
          ! Minimum timestep
          IF ((DT(I) > EM20).AND.(DT(I) < DTMIN).AND.(OFF(I) == ONE)) THEN 
            OFF(I)      = ZERO
            NINDX       = NINDX+1
            INDX(NINDX) = I
          ENDIF   
c     
          ! Failure criterion
          IF ((IFUNC(4) > 0).AND.(OFF(I) == ONE)) THEN
            ! Failure indicator evolution
            IF (OFFL(I) < EM01) OFFL(I) = ZERO
            IF (OFFL(I) < ONE)   OFFL(I) = OFFL(I)*FOUR_OVER_5
            ! Case of under-integrated shells
            IF ((NPTR == 1).AND.(NPTS == 1)) THEN
              !Initialization for checking complete failure of the shell (all integration points)
              IF (IPT == 1) THEN
                OFF(I) = ZERO
              ENDIF
              !If one integration points is not fully broken, the shell remains
              IF (OFFL(I)>ZERO) OFF(I) = ONE
            ! Case of fully integrated shells
            ELSE
              IF ((IPG == 1).AND.(IPT == 1)) THEN 
                !Initialization for checking complete failure of the shell (all integration points)
                OFF(I) = ZERO
                ! Loop over all Gauss points (thickness + surface)
                DO IR = 1,NPTR
                  DO IS = 1,NPTS
                    DO IT = 1,NPTT
                      !If one integration points is not fully broken, the shell remains
                      IF (BUFLY%LBUF(IR,IS,IT)%OFF(I)>ZERO) OFF(I) = ONE
                    ENDDO
                  ENDDO
                ENDDO
              ENDIF
            ENDIF
            ! Check integration point failure
            IF (OFFL(I) == ONE) THEN 
              ! -> Von Mises stress
              IF (Ifail == 0) THEN
                IF (SEQ(I) >= FAIL(I)) OFFL(I) = FOUR_OVER_5
              ! -> Plastic strain
              ELSEIF (Ifail == 1) THEN
                IF (PLA(I) >= FAIL(I)) OFFL(I) = FOUR_OVER_5
              ! -> Maximum principal stress and absolute value of minimum principal stress
              ELSEIF (Ifail == 2) THEN
                S1 = HALF*(SIGNXX(I) + SIGNYY(I))
                S2 = HALF*(SIGNXX(I) - SIGNYY(I))
                Q  = SQRT(S2**2 + SIGNXY(I)**2)
                S11(I) = S1 + Q
                S22(I) = S1 - Q 
                IF (S22(I) >= S11(I)) THEN
                  R_INTER = S22(I)
                  S22(I)  = S11(I)
                  S11(I)  = R_INTER
                ENDIF
                IF ((S11(I)>=FAIL(I)).OR.(ABS(S22(I))>=FAIL(I))) OFFL(I) = FOUR_OVER_5
              ! -> Maximum principal stress
              ELSEIF (Ifail == 3) THEN 
                S1 = HALF*(SIGNXX(I) + SIGNYY(I))
                S2 = HALF*(SIGNXX(I) - SIGNYY(I))
                Q  = SQRT(S2**2 + SIGNXY(I)**2)
                S11(I) = S1 + Q
                S22(I) = S1 - Q 
                IF (S22(I) >= S11(I)) THEN
                  R_INTER = S22(I)
                  S22(I)  = S11(I)
                  S11(I)  = R_INTER
                ENDIF
                IF (S11(I)>=FAIL(I)) OFFL(I) = FOUR_OVER_5
              ENDIF                
              !Integration point failure
              IF (OFFL(I) == FOUR_OVER_5) THEN
                NINDX2      = NINDX2+1
                INDX2(NINDX2) = I
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDIF
c
      ! Printout timestep element deletion
      IF (NINDX>0) THEN
        DO J=1,NINDX
#include "lockon.inc"
          WRITE(IOUT, 1000) NGL(INDX(J))
          WRITE(ISTDO,1100) NGL(INDX(J)),TT
#include "lockoff.inc"
        ENDDO
      ENDIF
c
      ! Printout failure
      IF (NINDX2>0) THEN
        DO J=1,NINDX2
#include "lockon.inc"
          WRITE(IOUT, 2000) NGL(INDX2(J)),IPG,IPT
          WRITE(ISTDO,2100) NGL(INDX2(J)),IPG,IPT,TT
#include "lockoff.inc"
        ENDDO
      ENDIF
c
 1000 FORMAT(1X,'MINIMUM TIMESTEP (PLAS_RATE) REACHED, DELETED SHELL ELEMENT ',I10)
 1100 FORMAT(1X,'MINIMUM TIMESTEP (PLAS_RATE) REACHED, DELETED SHELL ELEMENT ',I10,1X,'AT TIME :',1PE12.4)
 2000 FORMAT(1X,'FAILURE (PLAS_RATE) IN SHELL ELEMENT ',I10,1X,',GAUSS PT',I2,1X,',THICKNESS INTG. PT',I3)
 2100 FORMAT(1X,'FAILURE (PLAS_RATE) IN SHELL ELEMENT ',I10,1X,',GAUSS PT',I2,1X,',THICKNESS INTG. PT',I3,
     .       1X,'AT TIME :',1PE12.4)
c  
c-----------
      END
