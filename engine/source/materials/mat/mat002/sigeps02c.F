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
Chd|  SIGEPS02C                     source/materials/mat/mat002/sigeps02c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|        M2CPLR                        source/materials/mat/mat002/m2cplr.F
Chd|====================================================================
      SUBROUTINE SIGEPS02C(
     1                  JFT      ,JLT      ,PM       ,EINT     ,THK      ,
     2                  OFF      ,SIGY     ,DT1C     ,IPLA     ,NEL      ,
     3                  VOL0     ,GS       ,ISRATE   ,THKLYL   ,ETSE     ,
     4                  NGL      ,EPSPDT   ,G_IMP    ,SIGKSI   ,IOFF_DUCT,
     5                  DPLA     ,TSTAR    ,JTHE     ,HARDM    ,EPCHK    ,
     6                  IMAT     ,IPT      ,NPTT     ,PLA      ,OFF_OLD  ,
     7                  SIGOXX   ,SIGOYY   ,SIGOXY   ,SIGOYZ   ,SIGOZX   ,
     8                  SIGNXX   ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     9                  DEPSXX   ,DEPSYY   ,DEPSXY   ,DEPSYZ   ,DEPSZX   ,
     A                  SIGBAKXX ,SIGBAKYY ,SIGBAKXY ,INLOC    ,DPLANL   )
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
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT,IPLA,NEL,IOFC,JTHE,IPT,NPTT,IMAT,ISRATE
      INTEGER NGL(MVSIZ),IOFF_DUCT(*),INLOC
!---
C     REAL ou REAL*8
      my_real
     .   PM(NPROPM,*),EINT(NEL,2),
     .   OFF(*),SIGY(*),DT1C(*),VOL0(*),GS(*),THK(*),
     .   TSTAR(*),DPLA(*),SIGKSI(MVSIZ,5),ETSE(MVSIZ),
     .   DEPSXX(MVSIZ),DEPSYY(MVSIZ),DEPSXY(MVSIZ),DEPSYZ(MVSIZ),
     .   DEPSZX(MVSIZ),PLA(NEL),
     .   SIGBAKXX(NEL),SIGBAKYY(NEL),SIGBAKXY(NEL),
     .   SIGOXX(NEL),SIGOYY(NEL),SIGOXY(NEL),SIGOYZ(NEL),SIGOZX(NEL),
     .   OFF_OLD(MVSIZ),THKLYL(NEL),DPLANL(NEL)
!
      my_real
     .   SIGNXX(NEL),SIGNYY(NEL),SIGNXY(NEL),
     .   SIGNYZ(NEL),SIGNZX(NEL),EPCHK(MVSIZ),G_IMP(MVSIZ)
!
      my_real
     .   HARDM(MVSIZ),EPSPDT(MVSIZ),EZZ(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ICC,IRTY,I,J,NPIF
C     REAL ou REAL*8
      my_real
     .   EPMX,CA,CB,CN,YMAX(MVSIZ),CP,CC,EPDR(MVSIZ),YLD(MVSIZ),
     .   T(MVSIZ),Z3,Z4,FISOKIN,EPIF,YOUNG,TI,TM,G,A11,A12,NU
C-----------------------------------------------
      EPIF = ZERO
      NPIF = 0
      EZZ(1:MVSIZ) = ZERO
!---
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
      Z4     = PM(52,IMAT)
      CP     = PM(53,IMAT)
      TI     = PM(54,IMAT)
      FISOKIN= PM(55,IMAT)
!
      EPIF   = MAX( EPIF, CC )
!
      DO I=JFT,JLT          
        EPDR(I) = MAX( EM20, PM(44,IMAT)*DT1C(I) )
        NPIF	= NPIF + IRTY
        T(I)	= TI
      ENDDO
!
      IF ( JTHE <= 0 ) THEN      
        TM=Z4  
        DO I=JFT,JLT
          T(I) = T(I)+CP*(EINT(I,1)+EINT(I,2))/VOL0(I)
          TSTAR(I) = MAX( ZERO, (T(I)-TI)/(TM-TI) )
        ENDDO
      ENDIF
!----------------------------
!         CONTRAINTES PLASTIQUEMENT ADMISSIBLES
!----------------------------
      CALL M2CPLR(JFT     ,JLT     ,EZZ      ,OFF_OLD  ,PLA     ,
     2            IPLA    ,T       ,Z3       ,Z4       ,EPIF    ,
     3            NPIF    ,IRTY    ,ETSE     ,GS       ,EPSPDT  ,
     4            ISRATE  ,YLD     ,G        ,A11      ,A12     ,
     5            NU      ,CA      ,CB       ,CN       ,YMAX    ,
     6            EPCHK   ,YOUNG   ,CC       ,EPDR     ,ICC     ,
     7            DPLA    ,TSTAR   ,FISOKIN  ,G_IMP    ,SIGKSI  ,
     8            HARDM   ,NEL     ,DEPSXX   ,DEPSYY   ,DEPSXY  ,
     9            DEPSYZ  ,DEPSZX  ,SIGNXX   ,SIGNYY   ,SIGNXY  ,
     A            SIGNYZ  ,SIGNZX  ,SIGBAKXX ,SIGBAKYY ,SIGBAKXY,
     B            SIGOXX  ,SIGOYY  ,SIGOXY   ,SIGOYZ   ,SIGOZX  )
!
      DO I=JFT,JLT
        SIGY(I) = SIGY(I) + YLD(I)/NPTT
      ENDDO
!----------------------------
!     TEST DE DUCTILE RUPTURE
!----------------------------
        DO I=JFT,JLT
          IF (OFF(I) == OFF_OLD(I) .and. OFF(I) > ZERO) THEN
            IF (OFF(I) == UN .and. EPCHK(I) >= EPMX) THEN
              OFF(I)= QCINQ
              IOFF_DUCT(I) = 1    ! debut de la rupture progressive
            ELSE IF (OFF(I) < UN ) THEN
              OFF(I) = OFF(I)*QCINQ
            ENDIF
          ENDIF
        ENDDO
c------------------------------------            
!     thickness update:
c------------------------------------            
      DO I=JFT,JLT
        IF (INLOC > 0) THEN
          EZZ(I) = -NU*(SIGNXX(I)-SIGOXX(I)+SIGNYY(I)-SIGOYY(I))/YOUNG
          EZZ(I) = EZZ(I) - MAX(DPLANL(I),ZERO)*UNDEMI*(SIGNXX(I)+SIGNYY(I))/YLD(I)
        ELSE
          EZZ(I) = -(DEPSXX(I)+DEPSYY(I))*NU-(UN - DEUX*NU)*EZZ(I)
          EZZ(I) = EZZ(I)/(UN-NU)
        ENDIF
        THK(I) = THK(I) + EZZ(I) * THKLYL(I)*OFF(I)
      ENDDO
c-----------
      RETURN
      END
