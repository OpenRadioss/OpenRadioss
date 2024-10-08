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
      !||====================================================================
      !||    sigeps02c   ../engine/source/materials/mat/mat002/sigeps02c.F
      !||--- called by ------------------------------------------------------
      !||    mulawc      ../engine/source/materials/mat_share/mulawc.F
      !||--- calls      -----------------------------------------------------
      !||    m2cplr      ../engine/source/materials/mat/mat002/m2cplr.F
      !||====================================================================
      SUBROUTINE SIGEPS02C(MAT_PARAM,NEL      ,EINT     ,THK      ,
     1                     OFF      ,SIGY     ,DT1      ,IPLA     ,
     2                     VOL      ,GS       ,ISRATE   ,THKLYL   ,ETSE     ,
     3                     NGL      ,EPSP     ,G_IMP    ,SIGKSI   ,IOFF_DUCT,
     4                     DPLA     ,TSTAR    ,JTHE     ,HARDM    ,EPCHK    ,
     5                     NPTT     ,PLA      ,OFF_OLD  ,
     6                     SIGOXX   ,SIGOYY   ,SIGOXY   ,SIGOYZ   ,SIGOZX   ,
     7                     SIGNXX   ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     8                     DEPSXX   ,DEPSYY   ,DEPSXY   ,DEPSYZ   ,DEPSZX   ,
     9                     EPSPXX   ,EPSPYY   ,EPSPXY   ,EPSPYZ   ,EPSPZX   ,
     A                     SIGBAKXX ,SIGBAKYY ,SIGBAKXY ,INLOC    ,DPLANL   ,
     B                     ASRATE   ,LOFF     ,EPSD     ,TEMPEL   ,FHEAT    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MATPARAM_DEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL,IPLA,JTHE,NPTT,ISRATE,INLOC
      INTEGER, DIMENSION(NEL), INTENT(IN) :: NGL
      INTEGER, DIMENSION(NEL), INTENT(INOUT) :: IOFF_DUCT
      my_real, DIMENSION(NEL), INTENT(IN) :: EPSPXX,EPSPYY,EPSPXY,EPSPYZ,
     .   EPSPZX,EPSP,DEPSXX,DEPSYY,DEPSXY,DEPSYZ,DEPSZX,SIGOXX,
     .   SIGOYY,SIGOXY,SIGOYZ,SIGOZX,OFF_OLD,DPLANL,GS,THKLYL
      my_real, INTENT(IN) :: ASRATE,DT1
      my_real, DIMENSION(NEL)  , INTENT(INOUT) :: SIGNXX,SIGNYY,SIGNXY,SIGNYZ,SIGNZX
      my_real, DIMENSION(NEL)  , INTENT(INOUT) :: SIGBAKXX,SIGBAKYY,SIGBAKXY
      my_real, DIMENSION(NEL)  , INTENT(INOUT) :: PLA,DPLA,ETSE,THK,SIGY,OFF,VOL,HARDM
      my_real, DIMENSION(NEL)  , INTENT(INOUT) :: EPCHK,G_IMP,EPSD
      my_real, DIMENSION(NEL)  , INTENT(IN)    :: TSTAR
      my_real, DIMENSION(NEL,5), INTENT(INOUT) :: SIGKSI
      my_real, DIMENSION(NEL,2), INTENT(IN)    :: EINT
      my_real, DIMENSION(NEL)  , INTENT(IN)    :: LOFF
      my_real, DIMENSION(NEL)  , INTENT(INOUT) :: TEMPEL
      my_real, DIMENSION(NEL)  , INTENT(INOUT) :: FHEAT
      TYPE(MATPARAM_STRUCT_)   , INTENT(IN)    :: MAT_PARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,ICC,IFORM,VP
      my_real :: EPMX,CA,CB,CN,RHOCPI,CC,EPDR,M_EXP,
     .   DAV,DEVE1,DEVE2,DEVE3,DEVE4,C3,C4,FISOKIN,
     .   YOUNG,TREF,TMELT,G,A11,A12,NU,YMAX
      my_real, DIMENSION(NEL) :: YLD,EPSDOT,T,EZZ
!=======================================================================
      ! Recover material law parameters
!---
      iform  = mat_param%iparam(1)
      icc    = mat_param%iparam(2)        
      vp     = mat_param%iparam(3)        
      
      young  = mat_param%young
      nu     = mat_param%nu 
      g      = mat_param%shear
      a11    = young / (one - nu**2) 
      a12    = a11 *nu
!
      ca     = mat_param%uparam(1)         ! pm(38)
      cb     = mat_param%uparam(2)         ! pm(39)
      cn     = mat_param%uparam(3)         ! pm(40)
      epmx   = mat_param%uparam(4)         ! pm(41)
      ymax   = mat_param%uparam(5)         ! pm(41)
      cc     = mat_param%uparam(6)         ! pm(43)
      epdr   = mat_param%uparam(7)      
      fisokin= mat_param%uparam(8)         ! pm(55)
!
      if (iform == 1) then        ! zerilli
        c3     = mat_param%uparam(10)        ! pm(51)
        c4     = mat_param%uparam(11)        ! pm(52)
        rhocpi = mat_param%uparam(12)           
        tref   = mat_param%uparam(13)
      else                        ! johnson-cook
        rhocpi = mat_param%uparam(10)
        tref   = mat_param%uparam(11)               
        tmelt  = mat_param%uparam(12)               
        m_exp  = mat_param%uparam(13)
        if (rhocpi > zero) rhocpi = one / rhocpi
        c3    = m_exp
        
        ! tstar is still calculated in cmain using values in PM       
!        do i = 1,nel
!          tstar(i) = max(zero, (tempel(i)-tref)/(tmelt-tref) )
!        end do
      end if
!
      ! Specific strain rate computation
      ! -> Plastic strain rate
      ! Old filtered value already stored in EPSD
!
      IF (VP == 1) THEN 
<<<<<<< HEAD
        DO I=JFT,JLT
          EPSDOT(I) = EPSD(I)*DT1C(I) 
        ENDDO
      ! -> Total strain rate
      ELSEIF (VP == 2) THEN
        DO I=JFT,JLT  
          EPSDOT(I) = EPSPDT(I) 
          EPSD(I) = EPSPDT(I)/MAX(DT1C(I),EM20)
=======
        DO I=1,NEL
          EPSDOT(I) = EPSD(I) 
        ENDDO
      ! -> Total strain rate
      ELSEIF (VP == 2) THEN
        DO I=1,NEL  
          EPSDOT(I) = EPSP(I)  
          EPSD(I)   = EPSP(I)    
>>>>>>> a32dfb8d0 (update vraiment sigeps02c)
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
          EPSDOT(I) = EPSDOT(I)*DT1C(I)
        ENDDO 
      ENDIF
      ! Variable initialization
      EZZ(1:NEL) = ZERO
!----------------------------
!         CONTRAINTES PLASTIQUEMENT ADMISSIBLES
!----------------------------
      CALL M2CPLR(NEL     ,EZZ     ,OFF_OLD  ,PLA     ,
     2            IPLA    ,TEMPEL  ,C3       ,C4       ,
     3            IFORM   ,ETSE    ,GS       ,EPSDOT   ,
     4            ISRATE  ,YLD     ,G        ,A11      ,A12     ,
     5            NU      ,CA      ,CB       ,CN       ,YMAX    ,
     6            EPCHK   ,YOUNG   ,CC       ,EPDR     ,ICC     ,
     7            DPLA    ,TSTAR   ,FISOKIN  ,G_IMP    ,SIGKSI  ,
     8            HARDM   ,DEPSXX  ,DEPSYY   ,DEPSXY   ,
     9            DEPSYZ  ,DEPSZX  ,SIGNXX   ,SIGNYY   ,SIGNXY  ,
     A            SIGNYZ  ,SIGNZX  ,SIGBAKXX ,SIGBAKYY ,SIGBAKXY,
     B            SIGOXX  ,SIGOYY  ,SIGOXY   ,SIGOYZ   ,SIGOZX  ,
     C            VP      )
!
!--------------------------------------------
!     UPDATE AND FILTER PLASTIC STRAIN RATE
!--------------------------------------------
      IF (VP == 1) THEN
<<<<<<< HEAD
        DO I=JFT,JLT
          EPSDOT(I) = DPLA(I)/MAX(EM20,DT1C(I))
=======
        DO I=1,NEL
          EPSDOT(I) = DPLA(I)/MAX(EM20,DT1)
>>>>>>> a32dfb8d0 (update vraiment sigeps02c)
          EPSD(I)   = ASRATE*EPSDOT(I) + (ONE - ASRATE)*EPSD(I)
        ENDDO
      ENDIF
!--------------------------------------------
      DO I=1,NEL
        SIGY(I) = SIGY(I) + YLD(I)/NPTT
      ENDDO
!----------------------------
!     TEST DE DUCTILE RUPTURE
!----------------------------
        DO I=1,NEL
          IF (OFF(I) == OFF_OLD(I) .and. OFF(I) > ZERO) THEN
            IF (OFF(I) == ONE .and. EPCHK(I) >= EPMX) THEN
              OFF(I)= FOUR_OVER_5
              IOFF_DUCT(I) = 1    ! debut de la rupture progressive
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
!---------------------------------------------------------
!     calculate temperature or thermal load due to plastic work
!---------------------------------------------------------
      IF (JTHE == 0 .and. RHOCPI > ZERO) THEN  
        ! update temperature in adiabatic conditions
        DO I=1,NEL       
          TEMPEL(I) = TEMPEL(I) + SIGY(I)*DPLA(I) * RHOCPI
        ENDDO
      ELSE IF (JTHE /= 0) THEN
        ! cumulate thermal load due to plastic work for /heat/mat
        DO I=1,NEL       
          FHEAT(I) = FHEAT(I) + SIGY(I)*DPLA(I)*VOL(I)
        ENDDO
      END IF
c-----------
      RETURN
      END
