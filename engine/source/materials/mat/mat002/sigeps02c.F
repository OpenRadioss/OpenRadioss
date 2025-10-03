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
!||    sigeps02c          ../engine/source/materials/mat/mat002/sigeps02c.F
!||--- called by ------------------------------------------------------
!||    mulawc             ../engine/source/materials/mat_share/mulawc.F90
!||--- calls      -----------------------------------------------------
!||    m2cplr             ../engine/source/materials/mat/mat002/m2cplr.F
!||--- uses       -----------------------------------------------------
!||    matparam_def_mod   ../common_source/modules/mat_elem/matparam_def_mod.F90
!||====================================================================
      SUBROUTINE SIGEPS02C(mat_param ,
     .            NEL      ,EINT     ,THK      ,TEMPEL   ,FHEAT    ,            
     .            OFF      ,SIGY     ,DT1      ,IPLA     ,SIGKSI   ,        
     .            VOL      ,GS       ,THKLYL   ,ETSE     ,G_IMP    ,                    
     .            DPLA     ,TSTAR    ,JTHE     ,HARDM    ,EPCHK    ,          
     .            NPTT     ,PLA      ,OFF_OLD  ,LOFF     ,IOFF_DUCT,          
     .            SIGOXX   ,SIGOYY   ,SIGOXY   ,SIGOYZ   ,SIGOZX   ,          
     .            SIGNXX   ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,          
     .            DEPSXX   ,DEPSYY   ,DEPSXY   ,DEPSYZ   ,DEPSZX   ,          
     .            EPSPXX   ,EPSPYY   ,EPSPXY   ,EPSD_PG  ,EPSD     ,          
     .            ASRATE   ,SIGBAKXX ,SIGBAKYY ,SIGBAKXY ,
     .            INLOC    ,DPLANL   )        
!-----------------------------------------------
!   m o d u l e s
!-----------------------------------------------
      use matparam_def_mod
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NEL,IPLA,JTHE,NPTT,INLOC
      INTEGER IOFF_DUCT(*)
      my_real ,INTENT(IN) :: ASRATE
      my_real ,INTENT(IN) :: DT1
      my_real ,DIMENSION(NEL) ,INTENT(IN) :: LOFF
      my_real ,DIMENSION(NEL) ,INTENT(IN) :: EPSD_PG
      my_real ,DIMENSION(NEL) ,INTENT(IN) :: EPSPXX,EPSPYY,EPSPXY
      my_real ,DIMENSION(NEL) :: OFF,SIGY,VOL,GS,THK,TSTAR,DPLA,PLA,THKLYL,DPLANL
      my_real :: EINT(NEL,2),SIGKSI(MVSIZ,5)
      my_real, DIMENSION(MVSIZ) ,INTENT(IN)    :: DEPSXX,DEPSYY,DEPSXY,DEPSYZ,DEPSZX
      my_real, DIMENSION(MVSIZ) ,INTENT(INOUT) :: HARDM,G_IMP,EPCHK,OFF_OLD,ETSE
      my_real, DIMENSION(NEL)   ,INTENT(IN)    :: SIGOXX,SIGOYY,SIGOXY,SIGOYZ,SIGOZX
      my_real, DIMENSION(NEL)   ,INTENT(INOUT) :: SIGBAKXX,SIGBAKYY,SIGBAKXY
      my_real, DIMENSION(NEL)   ,INTENT(INOUT) :: SIGNXX,SIGNYY,SIGNXY,SIGNYZ,SIGNZX
      my_real, DIMENSION(NEL)   ,INTENT(INOUT) :: TEMPEL
      my_real, DIMENSION(NEL)   ,INTENT(INOUT) :: FHEAT
      my_real, DIMENSION(NEL)   ,INTENT(INOUT) :: EPSD
      type (matparam_struct_)   ,intent(in)    :: mat_param
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,ICC,IFORM,ISRATE,VP
      my_real :: EPMX,CA,CB,CN,CC,M_EXP,YOUNG,G,A11,A12,NU,RHOCP,TREF,TMELT
      my_real :: DAV,DEVE1,DEVE2,DEVE3,DEVE4,Z3,Z4,FISOKIN,EPDR,YMAX
      my_real :: YLD(MVSIZ),EPSDOT(MVSIZ),EZZ(MVSIZ)
!=======================================================================
      ! Variable initialization
      EZZ(1:MVSIZ) = ZERO
!---
      ! Recover material law parameters
!---
      iform  = mat_param%iparam(1)
      icc    = mat_param%iparam(2)
      vp     = mat_param%iparam(3)
      israte = mat_param%iparam(4)
!
      young  = mat_param%young
      g      = mat_param%shear
      nu     = mat_param%nu   
      a11    = young / (one - nu**2) 
      a12    = a11 *nu
!
      ca     = mat_param%uparam(1)          ! pm(38)
      cb     = mat_param%uparam(2)          ! pm(39)
      cn     = mat_param%uparam(3)          ! pm(40)
      epmx   = mat_param%uparam(4)          ! pm(41)
      ymax   = mat_param%uparam(5)          ! pm(41)
      cc     = mat_param%uparam(6)          ! pm(43)
      epdr   = mat_param%uparam(7)          ! pm(44)
      epdr   = max(epdr*dt1, em20)   
      fisokin= mat_param%uparam(8)          ! pm(55)

      tref   = mat_param%therm%tref 
      tmelt  = mat_param%therm%tmelt
      rhocp  = mat_param%therm%rhocp
!
      IF (IFORM == 1) THEN       ! Zerilli
        Z3    = mat_param%uparam(10)
        Z4    = mat_param%uparam(11)                    
        m_exp = one
      ELSE                       ! Johnson-Cook
        z3 = zero
        z4 = zero                 
        m_exp = mat_param%uparam(10)
        DO I=1,NEL
          TSTAR(I) = MAX( ZERO, (TEMPEL(I)-TREF)/(TMELT-TREF) )
        ENDDO
      END IF             
!
      ! Specific strain rate computation
      ! -> Plastic strain rate
      ! Old filtered value already stored in EPSD
!
      IF (VP == 1) THEN 
        DO I=1,NEL
          EPSDOT(I) = EPSD(I)*DT1 
        ENDDO
      ! -> Total strain rate
      ELSEIF (VP == 2) THEN
        DO I=1,NEL  
          EPSD(I)   = ASRATE*EPSD_PG(I) + (ONE-ASRATE)*EPSD(I)
          EPSDOT(I) = EPSD(I) * DT1
!          EPSDOT(I) = EPSPDT(I) 
!          EPSD(I) = EPSPDT(I)/MAX(DT1,EM20)
        ENDDO   
      ! -> Deviatoric strain rate    
      ELSEIF (VP == 3) THEN
        DO I=1,NEL
          DAV   = (EPSPXX(I)+EPSPYY(I))*THIRD
          DEVE1 = EPSPXX(I) - DAV
          DEVE2 = EPSPYY(I) - DAV
          DEVE3 = - DAV
          DEVE4 = HALF*EPSPXY(I)
          EPSDOT(I) = HALF*(DEVE1**2 + DEVE2**2 + DEVE3**2) + DEVE4**2
          EPSDOT(I) = SQRT(THREE*EPSDOT(I))/THREE_HALF             
          IF (ISRATE > 0) THEN 
            EPSDOT(I) = ASRATE*EPSDOT(I) + (ONE - ASRATE)*EPSD(I)
          ENDIF
          EPSD(I) = EPSDOT(I)
          EPSDOT(I) = EPSDOT(I)*DT1
        ENDDO 
      ENDIF
!----------------------------
!         CONTRAINTES PLASTIQUEMENT ADMISSIBLES
!----------------------------
      CALL M2CPLR(NEL     ,EZZ     ,OFF_OLD ,PLA     ,
     2            IPLA    ,TEMPEL  ,Z3      ,Z4      ,M_EXP   ,
     3            IFORM   ,ETSE    ,GS      ,EPSDOT  ,VP      ,
     4            ISRATE  ,YLD     ,G       ,A11     ,A12     ,
     5            NU      ,CA      ,CB      ,CN      ,YMAX    ,
     6            EPCHK   ,YOUNG   ,CC      ,EPDR    ,ICC     ,
     7            DPLA    ,TSTAR   ,FISOKIN ,G_IMP   ,SIGKSI  ,
     8            HARDM   ,DEPSXX  ,DEPSYY  ,DEPSXY  ,
     9            DEPSYZ  ,DEPSZX  ,SIGNXX  ,SIGNYY  ,SIGNXY  ,
     A            SIGNYZ  ,SIGNZX  ,SIGBAKXX,SIGBAKYY,SIGBAKXY,
     B            SIGOXX  ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX  )
!
!--------------------------------------------
!     UPDATE AND FILTER PLASTIC STRAIN RATE
!--------------------------------------------
      IF (VP == 1) THEN
        DO I=1,NEL
          EPSDOT(I) = DPLA(I)/MAX(EM20,DT1)
          EPSD(I)   = ASRATE*EPSDOT(I) + (ONE - ASRATE)*EPSD(I)
        ENDDO
      ENDIF
!--------------------------------------------
      DO I=1,NEL
        SIGY(I) = SIGY(I) + YLD(I)/NPTT
      ENDDO
!----------------------------
! Ductile rupture test
!----------------------------
        DO I=1,NEL
          IF (OFF(I) == OFF_OLD(I) .and. OFF(I) > ZERO) THEN
            IF (OFF(I) == ONE .and. EPCHK(I) >= EPMX) THEN
              OFF(I)= FOUR_OVER_5
              IOFF_DUCT(I) = 1    ! start of progressive rupture
            ELSE IF (OFF(I) < ONE ) THEN
              OFF(I) = OFF(I)*FOUR_OVER_5
            ENDIF
          ENDIF
        ENDDO
c------------------------------------            
!     thickness update:
c------------------------------------            
      DO I=1,NEL
        IF (INLOC > 0) THEN
          IF (LOFF(I) == ONE) THEN 
            EZZ(I) = -NU*(SIGNXX(I)-SIGOXX(I)+SIGNYY(I)-SIGOYY(I))/YOUNG
            EZZ(I) = EZZ(I) - MAX(DPLANL(I),ZERO)*HALF*(SIGNXX(I)+SIGNYY(I))/YLD(I)
          ENDIF
        ELSE
          EZZ(I) = -(DEPSXX(I)+DEPSYY(I))*NU-(ONE - TWO*NU)*EZZ(I)
          EZZ(I) = EZZ(I)/(ONE-NU)
        ENDIF
        THK(I) = THK(I) + EZZ(I) * THKLYL(I)*OFF(I)
      ENDDO
C-----------------
C     TEMPERATURE
C-----------------
      IF (JTHE /= 0) THEN
        ! heat increment due to plastic work for /heat/mat
        DO I=1,NEL
          FHEAT(I) = FHEAT(I) + SIGY(I)*DPLA(I)*VOL(I) * MAT_PARAM%THERM%EFRAC
        ENDDO
      ELSEIF (RHOCP > ZERO) THEN   ! adiabatic conditions => temp is updated locally
        DO I=1,NEL
          TEMPEL(I) = TEMPEL(I) + SIGY(I)*DPLA(I) / RHOCP
          ! internal energy incremented later in parent subroutine (mmain)
          ! with total energy deformation which already includes plastic work
        ENDDO
      END IF
c-----------
      RETURN
      END
