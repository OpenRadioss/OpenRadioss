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
Chd|  SIGEPS121                     source/materials/mat/mat121/sigeps121.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|        MAT121_NEWTON                 source/materials/mat/mat121/mat121_newton.F
Chd|        MAT121_NICE                   source/materials/mat/mat121/mat121_nice.F
Chd|        VINTER2                       source/tools/curve/vinter.F   
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SIGEPS121(
     1     NEL     ,NGL     ,NUPARAM ,NUVAR   ,NFUNC   ,IFUNC   ,NPF      ,
     2     TF      ,TIMESTEP,TIME    ,UPARAM  ,UVAR    ,RHO     ,PLA      ,
     3     DPLA    ,SOUNDSP ,EPSD    ,OFF     ,LOFF    ,
     4     DEPSXX  ,DEPSYY  ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5     EPSPXX  ,EPSPYY  ,EPSPZZ  ,EPSPXY  ,EPSPYZ  ,EPSPZX  ,
     6     SIGOXX  ,SIGOYY  ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     7     SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     8     SIGY    ,ET      ,VARNL   ,INLOC   ,DT      ,
     9     IPG     ,NPG     ,ELBUF_TAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD        
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C O M M O N
C-----------------------------------------------
#include      "param_c.inc" 
#include      "com01_c.inc"
#include      "scr17_c.inc"
#include      "mvsiz_p.inc"
#include      "parit_c.inc"
#include      "scr05_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "units_c.inc"
#include      "impl1_c.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NUPARAM,NUVAR,NFUNC,INLOC,
     .        IPG,NPG,NPF(*),NGL(NEL),
     .        IFUNC(NFUNC)
      my_real 
     .   TIME,TIMESTEP,TF(*),UPARAM(NUPARAM)
      my_real,DIMENSION(NEL), INTENT(IN)     :: 
     .   RHO,DT,
     .   DEPSXX,DEPSYY,DEPSZZ,DEPSXY,DEPSYZ,DEPSZX,
     .   EPSPXX,EPSPYY,EPSPZZ,EPSPXY,EPSPYZ,EPSPZX,
     .   SIGOXX,SIGOYY,SIGOZZ,SIGOXY,SIGOYZ,SIGOZX
      my_real ,DIMENSION(NEL), INTENT(OUT)   :: 
     .   SOUNDSP,SIGNXX,SIGNYY,SIGNZZ,SIGNXY,SIGNYZ,SIGNZX
      my_real,DIMENSION(NEL) ::
     .   SIGY,ET
      my_real ,DIMENSION(NEL), INTENT(INOUT)       :: 
     .   PLA,DPLA,EPSD,VARNL,LOFF,OFF
      my_real ,DIMENSION(NEL,NUVAR), INTENT(INOUT) :: 
     .   UVAR
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IRES,Ifail,NINDX,NINDX2,INDX(NEL),INDX2(NEL),
     .        IPOS(NEL),IAD(NEL),ILEN(NEL),IR,IS,IT
      my_real DTMIN,Xscale_FAIL,Yscale_FAIL,S1,S2,Q,S11,
     .        S22,R_INTER,DFDEPSD(NEL),FAIL(NEL),SEQ(NEL),
     .        I1,I2,I3,R,PSI,S33
c=======================================================================
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
          CALL MAT121_NICE(
     1         NEL     ,NGL     ,NUPARAM ,NUVAR   ,NFUNC   ,IFUNC   ,NPF      ,
     2         TF      ,TIMESTEP,TIME    ,UPARAM  ,UVAR    ,RHO     ,PLA      ,
     3         DPLA    ,SOUNDSP ,EPSD    ,OFF     ,
     4         DEPSXX  ,DEPSYY  ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5         EPSPXX  ,EPSPYY  ,EPSPZZ  ,EPSPXY  ,EPSPYZ  ,EPSPZX  ,
     6         SIGOXX  ,SIGOYY  ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     7         SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     8         SIGY    ,ET      ,SEQ     )
c        
        CASE(2)
c
          CALL MAT121_NEWTON(
     1         NEL     ,NGL     ,NUPARAM ,NUVAR   ,NFUNC   ,IFUNC   ,NPF      ,
     2         TF      ,TIMESTEP,TIME    ,UPARAM  ,UVAR    ,RHO     ,PLA      ,
     3         DPLA    ,SOUNDSP ,EPSD    ,OFF     ,
     4         DEPSXX  ,DEPSYY  ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5         EPSPXX  ,EPSPYY  ,EPSPZZ  ,EPSPXY  ,EPSPYZ  ,EPSPZX  ,
     6         SIGOXX  ,SIGOYY  ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     7         SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     8         SIGY    ,ET      ,SEQ     )
c      
      END SELECT   
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
            IF (LOFF(I) < EM01) LOFF(I) = ZERO
            IF (LOFF(I) < ONE)   LOFF(I) = LOFF(I)*FOUR_OVER_5
c
            ! Fully integrated solid elements
            IF (NPG > 1) THEN
              ! Checking full failure of the element
              IF (IPG == NPG) THEN 
                ! Initialization of OFFG
                OFF(I) = ZERO
                ! Loop over integration points
                DO IR = 1, ELBUF_TAB%NPTR
                  DO IS = 1, ELBUF_TAB%NPTS
                    DO IT = 1, ELBUF_TAB%NPTT
                      !If one integration points is not fully broken, the brick remains
                      IF (ELBUF_TAB%BUFLY(1)%LBUF(IR,IS,IT)%OFF(I)>ZERO) OFF(I) = ONE
                    ENDDO
                  ENDDO
                ENDDO
              ENDIF
            ! Under-integrated solid element
            ELSE
              !Initialization for checking complete failure of the shell (all integration points)
              IF (IPG == 1) THEN
                OFF(I) = ZERO
              ENDIF
              !If one integration points is not fully broken, the brick remains
              IF (LOFF(I)>ZERO) OFF(I) = ONE
            ENDIF
            ! Check integration point failure
            IF (LOFF(I) == ONE) THEN 
              ! -> Von Mises stress
              IF (Ifail == 0) THEN
                IF (SEQ(I) >= FAIL(I)) LOFF(I) = FOUR_OVER_5
              ! -> Plastic strain
              ELSEIF (Ifail == 1) THEN
                IF (PLA(I) >= FAIL(I)) LOFF(I) = FOUR_OVER_5
              ! -> Maximum principal stress and absolute value of minimum principal stress
              ELSEIF (Ifail == 2) THEN
                ! Computing the principal stresses
                I1 = SIGNXX(I)+SIGNYY(I)+SIGNZZ(I)
                I2 = SIGNXX(I)*SIGNYY(I)+SIGNYY(I)*SIGNZZ(I)+SIGNZZ(I)*SIGNXX(I)-
     .               SIGNXY(I)*SIGNXY(I)-SIGNZX(I)*SIGNZX(I)-SIGNYZ(I)*SIGNYZ(I)
                I3 = SIGNXX(I)*SIGNYY(I)*SIGNZZ(I)-SIGNXX(I)*SIGNYZ(I)*SIGNYZ(I)-
     .               SIGNYY(I)*SIGNZX(I)*SIGNZX(I)-SIGNZZ(I)*SIGNXY(I)*SIGNXY(I)+
     .               TWO*SIGNXY(I)*SIGNZX(I)*SIGNYZ(I)
                Q  = (THREE*I2 - I1*I1)/NINE
                R  = (TWO*I1*I1*I1-NINE*I1*I2+TWENTY7*I3)/CINQUANTE4     ! (2*I3^3-9*I1*I2+27*I3)/54
                R_INTER = MIN(R/SQRT(MAX(EM20,(-Q**3))),ONE)
                PSI = ACOS(MAX(R_INTER,-ONE))
                S11 = TWO*SQRT(-Q)*COS(PSI/THREE)+THIRD*I1
                S22 = TWO*SQRT(-Q)*COS((PSI+TWO*PI)/THREE)+THIRD*I1
                S33 = TWO*SQRT(-Q)*COS((PSI+FOUR*PI)/THREE)+THIRD*I1
                ! Sorting principal strains
                IF (S11 < S22) THEN 
                  R_INTER = S11
                  S11     = S22
                  S22     = R_INTER
                ENDIF 
                IF (S22 < S33)THEN
                  R_INTER = S22
                  S22     = S33
                  S33     = R_INTER
                ENDIF
                IF (S11 < S22)THEN
                  R_INTER = S11
                  S11     = S22
                  S22     = R_INTER
                ENDIF     
                IF ((S11>=FAIL(I)).OR.(ABS(S33)>=FAIL(I))) LOFF(I) = FOUR_OVER_5
              ! -> Maximum principal stress
              ELSEIF (Ifail == 3) THEN 
                ! Computing the principal stresses
                I1 = SIGNXX(I)+SIGNYY(I)+SIGNZZ(I)
                I2 = SIGNXX(I)*SIGNYY(I)+SIGNYY(I)*SIGNZZ(I)+SIGNZZ(I)*SIGNXX(I)-
     .               SIGNXY(I)*SIGNXY(I)-SIGNZX(I)*SIGNZX(I)-SIGNYZ(I)*SIGNYZ(I)
                I3 = SIGNXX(I)*SIGNYY(I)*SIGNZZ(I)-SIGNXX(I)*SIGNYZ(I)*SIGNYZ(I)-
     .               SIGNYY(I)*SIGNZX(I)*SIGNZX(I)-SIGNZZ(I)*SIGNXY(I)*SIGNXY(I)+
     .               TWO*SIGNXY(I)*SIGNZX(I)*SIGNYZ(I)
                Q  = (THREE*I2 - I1*I1)/NINE
                R  = (TWO*I1*I1*I1-NINE*I1*I2+TWENTY7*I3)/CINQUANTE4     ! (2*I3^3-9*I1*I2+27*I3)/54
                R_INTER = MIN(R/SQRT(MAX(EM20,(-Q**3))),ONE)
                PSI = ACOS(MAX(R_INTER,-ONE))
                S11 = TWO*SQRT(-Q)*COS(PSI/THREE)+THIRD*I1
                S22 = TWO*SQRT(-Q)*COS((PSI+TWO*PI)/THREE)+THIRD*I1
                S33 = TWO*SQRT(-Q)*COS((PSI+FOUR*PI)/THREE)+THIRD*I1
                ! Sorting principal strains
                IF (S11 < S22) THEN 
                  R_INTER = S11
                  S11     = S22
                  S22     = R_INTER
                ENDIF 
                IF (S22 < S33)THEN
                  R_INTER = S22
                  S22     = S33
                  S33     = R_INTER
                ENDIF
                IF (S11 < S22)THEN
                  R_INTER = S11
                  S11     = S22
                  S22     = R_INTER
                ENDIF     
                IF (S11>=FAIL(I)) LOFF(I) = FOUR_OVER_5
              ENDIF  
              !Integration point failure
              IF (LOFF(I) == FOUR_OVER_5) THEN
                IDEL7NOK      = 1
                NINDX2        = NINDX2+1
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
          WRITE(IOUT, 2000) NGL(INDX2(J)),IPG
          WRITE(ISTDO,2100) NGL(INDX2(J)),IPG,TT
#include "lockoff.inc"
        ENDDO
      ENDIF
c
 1000 FORMAT(1X,'MINIMUM TIMESTEP (PLAS_RATE) REACHED, DELETED SOLID ELEMENT ',I10)
 1100 FORMAT(1X,'MINIMUM TIMESTEP (PLAS_RATE) REACHED, DELETED SOLID ELEMENT ',I10,1X,'AT TIME :',1PE12.4)
 2000 FORMAT(1X,'FAILURE (PLAS_RATE) IN SOLID ELEMENT ',I10,1X,',GAUSS PT',I2)
 2100 FORMAT(1X,'FAILURE (PLAS_RATE) IN SOLID ELEMENT ',I10,1X,',GAUSS PT',I2,1X,'AT TIME :',1PE12.4)
c  
c-----------
      RETURN
      END
