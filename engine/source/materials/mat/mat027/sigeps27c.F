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
!||    sigeps27c   ../engine/source/materials/mat/mat027/sigeps27c.F
!||--- called by ------------------------------------------------------
!||    mulawc      ../engine/source/materials/mat_share/mulawc.F90
!||--- calls      -----------------------------------------------------
!||    m27crak     ../engine/source/materials/mat/mat027/m27crak.F
!||    m27elas     ../engine/source/materials/mat/mat027/m27elas.F
!||    m27plas     ../engine/source/materials/mat/mat027/m27plas.F
!||    roto_sig    ../engine/source/airbag/roto.F
!||    uroto_sig   ../engine/source/airbag/uroto.F
!||====================================================================
      SUBROUTINE SIGEPS27C(
     1                   JFT    ,JLT    ,PM     ,THK    ,OFF   ,
     2                   GSTR   ,IMAT   ,DT1    ,IPLA   ,SHF   ,
     3                   NGL    ,THK0   ,THKLYL ,CRAK   ,DAM   ,
     4                   SIGY   ,ZCFAC  ,DPLA   ,ILAYER ,IPT   ,
     5                   ISRATE ,NEL    ,ZZ     ,NPTTOT ,EPSD_PG,
     6                   DEPSXX ,DEPSYY ,DEPSXY ,DEPSYZ ,DEPSZX,
     7                   SIGOXX ,SIGOYY ,SIGOXY ,SIGOYZ ,SIGOZX,
     8                   SIGNXX ,SIGNYY ,SIGNXY ,SIGNYZ ,SIGNZX,
     9                   DIRDMG ,PLA    ,INLOC  ,DPLANL ,LOFF  ,
     .                   EPSD   ,ASRATE )
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
      INTEGER NGL(MVSIZ),IMAT,ILAYER,IPT,ISRATE,NPTTOT,
     .   JFT,JLT,IPLA,NEL,INLOC
      my_real, INTENT(IN) :: DT1
      my_real, INTENT(IN) :: ASRATE
      my_real PM(NPROPM,*),THK(*),OFF(*),GSTR(NEL,8),
     .   SHF(*),SIGY(*),ZCFAC(MVSIZ,2),DPLA(*),
     .   THK0(MVSIZ),THKLYL(NEL),DPLANL(NEL)
      my_real
     .   DEPSXX(MVSIZ),DEPSYY(MVSIZ),DEPSXY(MVSIZ),DEPSYZ(MVSIZ),
     .   DEPSZX(MVSIZ),SIGOXX(NEL),SIGOYY(NEL),SIGOXY(NEL),
     .   SIGOYZ(NEL),SIGOZX(NEL),SIGNXX(NEL),SIGNYY(NEL),SIGNXY(NEL),
     .   SIGNYZ(NEL),SIGNZX(NEL),DIRDMG(NEL,2),
     .   PLA(NEL),CRAK(NEL),DAM(NEL),ZZ(MVSIZ)
      my_real, DIMENSION(NEL), INTENT(IN) :: LOFF
      my_real ,DIMENSION(NEL), INTENT(IN) :: EPSD_PG
      my_real, DIMENSION(NEL) ,INTENT(INOUT) :: EPSD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   SIG(NEL,5),NU,STRN1(MVSIZ),STRN2(MVSIZ),STRN3(MVSIZ),YLD(MVSIZ),
     .   ETSE(MVSIZ),ZT,EZZ(MVSIZ),YOUNG
C=======================================================================
!
      YOUNG = PM(20,IMAT)
      NU    = PM(21,IMAT)
!
      EZZ(1:MVSIZ) = ZERO
C-----------------------------------------------------------
C     PLASTICITE AUX POINTS D'INTEGRATION
C-----------------------------------------------------------
C----------------------------
C      CONTRAINTES ELASTIQUES
C----------------------------
      DO I=JFT,JLT
        SIG(I,1) = SIGOXX(I)
        SIG(I,2) = SIGOYY(I)
        SIG(I,3) = SIGOXY(I)
        SIG(I,4) = SIGOYZ(I)
        SIG(I,5) = SIGOZX(I)
      ENDDO
!
      CALL ROTO_SIG(JFT,JLT,SIG,DIRDMG,NEL)
!
      CALL M27ELAS(JFT    ,JLT    ,PM     ,SIG    ,IMAT  ,
     2             DEPSXX ,DEPSYY ,DEPSXY ,DEPSYZ ,DEPSZX,
     3             DAM    ,CRAK   ,SHF    ,NEL    )
C------------------------------------------
C      CONTRAINTES PLASTIQUEMENT ADMISSIBLES
C------------------------------------------
      CALL M27PLAS(JFT    ,JLT    ,PM     ,OFF    ,SIG   ,
     2             PLA    ,IMAT   ,DT1    ,IPLA   ,EZZ   ,
     3             EPSD_PG,ISRATE ,YLD    ,ETSE   ,DPLA  ,
     4             DEPSXX ,DEPSYY ,DEPSXY ,DEPSYZ ,DEPSZX,
     5             NEL    ,EPSD   ,ASRATE )
C---------------------------------
C      RETOUR DANS LE REPERE LOCAL
C---------------------------------
      CALL UROTO_SIG(JFT,JLT,SIG,DIRDMG,NEL)
!
      DO I=JFT,JLT
        SIGNXX(I) = SIG(I,1)
        SIGNYY(I) = SIG(I,2)
        SIGNXY(I) = SIG(I,3)
        SIGNYZ(I) = SIG(I,4)
        SIGNZX(I) = SIG(I,5)
      ENDDO
C-----------------------
C     TENSILE RUPTURE
C-----------------------
      DO I=JFT,JLT
        ZT = ZZ(I)*THK0(I)
        STRN1(I) = GSTR(I,1)+ZT*GSTR(I,6)
        STRN2(I) = GSTR(I,2)+ZT*GSTR(I,7)
        STRN3(I) =(GSTR(I,3)+ZT*GSTR(I,8))*HALF
      ENDDO
C-----------------------
      CALL M27CRAK(JFT    ,JLT    ,PM     ,DAM    ,DIRDMG ,
     2             IMAT   ,ILAYER ,OFF    ,NGL    ,NEL    ,
     3             STRN1  ,STRN2  ,STRN3  )
C-----------------------
      DO I=JFT,JLT
        ZCFAC(I,1) = ZCFAC(I,1) + ETSE(I) / NPTTOT
        ZCFAC(I,2) = MIN(ETSE(I),ZCFAC(I,2))
        SIGY(I) = MIN(SIGY(I),YLD(I))
      ENDDO
!---
!     thickness update:
!---
      DO I=JFT,JLT
        IF (INLOC > 0) THEN
          IF (LOFF(I) == ONE) THEN 
            EZZ(I) = -NU*(SIGNXX(I)-SIGOXX(I)+SIGNYY(I)-SIGOYY(I))/YOUNG
            EZZ(I) = EZZ(I) - MAX(DPLANL(I),ZERO)*HALF*(SIGNXX(I)+SIGNYY(I))/YLD(I)
          ENDIF
        ELSE
          EZZ(I) =-(DEPSXX(I)+DEPSYY(I))*NU-(ONE - TWO*NU)*EZZ(I)
          EZZ(I) = EZZ(I)/(ONE-NU)
        ENDIF
        THK(I) = THK(I) + EZZ(I) * THKLYL(I)*OFF(I)
      ENDDO
C-----------
      RETURN
      END
