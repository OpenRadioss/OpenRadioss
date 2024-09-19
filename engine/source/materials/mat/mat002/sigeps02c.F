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
      SUBROUTINE SIGEPS02C(
     1                  JFT      ,JLT      ,PM       ,EINT     ,THK      ,
     2                  OFF      ,SIGY     ,DT1      ,IPLA     ,NEL      ,
     3                  VOL      ,GS       ,ISRATE   ,THKLYL   ,ETSE     ,
     4                  NGL      ,EPSP     ,G_IMP    ,SIGKSI   ,IOFF_DUCT,
     5                  DPLA     ,TSTAR    ,JTHE     ,HARDM    ,EPCHK    ,
     6                  IMAT     ,IPT      ,NPTT     ,PLA      ,OFF_OLD  ,
     7                  SIGOXX   ,SIGOYY   ,SIGOXY   ,SIGOYZ   ,SIGOZX   ,
     8                  SIGNXX   ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     9                  DEPSXX   ,DEPSYY   ,DEPSXY   ,DEPSYZ   ,DEPSZX   ,
     A                  EPSPXX   ,EPSPYY   ,EPSPXY   ,EPSPYZ   ,EPSPZX   ,
     B                  SIGBAKXX ,SIGBAKYY ,SIGBAKXY ,INLOC    ,DPLANL   ,
     C                  VP       ,ASRATE   ,LOFF     ,EPSD     ,
     D                  TEMPEL   ,FHEAT    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: JFT,JLT,IPLA,NEL,JTHE,IPT,NPTT,IMAT,ISRATE,
     .   INLOC,VP
      INTEGER, DIMENSION(NEL), INTENT(IN) :: NGL
      INTEGER, DIMENSION(NEL), INTENT(INOUT) :: IOFF_DUCT
      my_real, DIMENSION(NEL), INTENT(IN) :: EPSPXX,EPSPYY,EPSPXY,EPSPYZ,
     .   EPSPZX,EPSP,DEPSXX,DEPSYY,DEPSXY,DEPSYZ,DEPSZX,SIGOXX,
     .   SIGOYY,SIGOXY,SIGOYZ,SIGOZX,OFF_OLD,DPLANL,GS,THKLYL
      my_real, INTENT(IN) :: ASRATE,DT1
      my_real, DIMENSION(NEL), INTENT(INOUT) :: SIGNXX,SIGNYY,SIGNXY,SIGNYZ,
     .   SIGNZX,SIGBAKXX,SIGBAKYY,SIGBAKXY,PLA,DPLA,TSTAR,ETSE,THK, 
     .   SIGY,OFF,VOL,HARDM,EPCHK,G_IMP,EPSD
      my_real, DIMENSION(NPROPM,*), INTENT(IN) :: PM
      my_real, DIMENSION(NEL,5), INTENT(INOUT) :: SIGKSI
      my_real, DIMENSION(NEL,2), INTENT(IN)    :: EINT
      my_real, DIMENSION(NEL)  , INTENT(IN)    :: LOFF
      my_real, DIMENSION(NEL)  , INTENT(INOUT) :: TEMPEL
      my_real, DIMENSION(NEL)  , INTENT(INOUT) :: FHEAT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,ICC,IRTY
      my_real :: EPMX,CA,CB,CN,RHOCP,CC,
     .   DAV,DEVE1,DEVE2,DEVE3,DEVE4,Z3,Z4,FISOKIN,
     .   YOUNG,TREF,TMELT,G,A11,A12,NU,YMAX
      my_real, DIMENSION(NEL) :: EPDR,YLD,EPSDOT,T,EZZ
!=======================================================================
      ! Variable initialization
      EZZ(1:NEL) = ZERO
!---
      ! Recover material law parameters
      YOUNG  = PM(20,IMAT)
      NU     = PM(21,IMAT)
      G      = PM(22,IMAT)
      A11    = PM(24,IMAT)
      A12    = PM(25,IMAT)
      CA     = PM(38,IMAT)
      CB     = PM(39,IMAT)
      CN     = PM(40,IMAT)
      EPMX   = PM(41,IMAT)
      YMAX   = PM(42,IMAT)
      CC     = PM(43,IMAT)
      ICC    = NINT(PM(49,IMAT))
      IRTY   = NINT(PM(50,IMAT))
      Z3     = PM(51,IMAT)
      FISOKIN= PM(55,IMAT)
!
      ! Recover internal variables
      DO I=JFT,JLT              
        EPDR(I) = MAX(EM20, PM(44,IMAT))      
      ENDDO
!
      IF (IRTY == 1) THEN       ! Zerilli
        RHOCP = PM(53,IMAT)     ! = 1 /rhocp             
        Z4    = PM(52,IMAT)                    
        TREF  = PM(54,IMAT)                    
        IF (JTHE /= 0) THEN      
          DO I=JFT,JLT
            TEMPEL(I) = TREF + RHOCP*(EINT(I,1)+EINT(I,2))/VOL(I)
          ENDDO
        ENDIF
      ELSE                       ! Johnson-Cook
        RHOCP = PM(69,IMAT)                  
        TREF  = PM(79,IMAT)                    
        TMELT = PM(80,IMAT)                    
        DO I=JFT,JLT
          TSTAR(I) = MAX( ZERO, (TEMPEL(I)-TREF)/(TMELT-TREF) )
        ENDDO
      END IF             
!
      ! Specific strain rate computation
      ! -> Plastic strain rate
      ! Old filtered value already stored in EPSD
!
      IF (VP == 1) THEN 
        DO I=JFT,JLT
          EPSDOT(I) = EPSD(I) 
        ENDDO
      ! -> Total strain rate
      ELSEIF (VP == 2) THEN
        DO I=JFT,JLT  
          EPSDOT(I) = EPSP(I)  
          EPSD(I)   = EPSP(I)    
        ENDDO   
      ! -> Deviatoric strain rate    
      ELSEIF (VP == 3) THEN
        DO I=JFT,JLT
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
        ENDDO 
      ENDIF
!----------------------------
!         CONTRAINTES PLASTIQUEMENT ADMISSIBLES
!----------------------------
      CALL M2CPLR(JFT     ,JLT     ,EZZ      ,OFF_OLD  ,PLA     ,
     2            IPLA    ,TEMPEL  ,Z3       ,Z4       ,
     3            IRTY    ,ETSE    ,GS       ,EPSDOT   ,
     4            ISRATE  ,YLD     ,G        ,A11      ,A12     ,
     5            NU      ,CA      ,CB       ,CN       ,YMAX    ,
     6            EPCHK   ,YOUNG   ,CC       ,EPDR     ,ICC     ,
     7            DPLA    ,TSTAR   ,FISOKIN  ,G_IMP    ,SIGKSI  ,
     8            HARDM   ,NEL     ,DEPSXX   ,DEPSYY   ,DEPSXY  ,
     9            DEPSYZ  ,DEPSZX  ,SIGNXX   ,SIGNYY   ,SIGNXY  ,
     A            SIGNYZ  ,SIGNZX  ,SIGBAKXX ,SIGBAKYY ,SIGBAKXY,
     B            SIGOXX  ,SIGOYY  ,SIGOXY   ,SIGOYZ   ,SIGOZX  ,
     C            VP      ,EPSPXX  ,EPSPYY   ,EPSPXY  )
!
!--------------------------------------------
!     UPDATE AND FILTER PLASTIC STRAIN RATE
!--------------------------------------------
      IF (VP == 1) THEN
        DO I=JFT,JLT
          EPSDOT(I) = DPLA(I)/MAX(EM20,DT1)
          EPSD(I)   = ASRATE*EPSDOT(I) + (ONE - ASRATE)*EPSD(I)
        ENDDO
      ENDIF
!--------------------------------------------
      DO I=JFT,JLT
        SIGY(I) = SIGY(I) + YLD(I)/NPTT
      ENDDO
!----------------------------
!     TEST DE DUCTILE RUPTURE
!----------------------------
        DO I=JFT,JLT
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
      DO I=JFT,JLT
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
      IF (JTHE == 0 .and. RHOCP > ZERO) THEN  ! update temperature in adiabatic conditions
        IF (IRTY /= 1) THEN                   ! Johnson-Cook only
          DO I=1,NEL       
            TEMPEL(I) = TEMPEL(I) + SIGY(I)*DPLA(I) / RHOCP
          ENDDO
        END IF
      ELSE                                    ! cumulate thermal load due to plastic work for /heat/mat
        DO I=1,NEL       
          FHEAT(I) = FHEAT(I) + SIGY(I)*DPLA(I)*VOL(I)
        ENDDO
      END IF
c-----------
      RETURN
      END
