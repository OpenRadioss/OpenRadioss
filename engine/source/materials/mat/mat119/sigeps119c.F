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
Chd|  SIGEPS19C                     source/materials/mat/mat019/sigeps19c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|====================================================================
      SUBROUTINE SIGEPS119C(
     .            NEL    ,NUPARAM,FLAG_ZCFAC,ZCFAC  ,SHF    ,
     .            UPARAM ,NPTTOT ,SSP    ,
     .            EPSXX  ,EPSYY  ,EPSXY  ,EPSYZ  ,EPSZX     ,
     .            SIGOXX ,SIGOYY ,SIGOXY ,SIGOYZ ,SIGOZX    ,
     .            SIGNXX ,SIGNYY ,SIGNXY ,SIGNYZ ,SIGNZX    ,
     .            SIGI   ,LSENSOR,SENSOR ,FACTOR)
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NEL,NPTTOT,NUPARAM
      INTEGER ,DIMENSION(NSENSI,NSENSOR) ,INTENT(IN) :: LSENSOR
      LOGICAL ,INTENT(IN) :: FLAG_ZCFAC
      my_real ,DIMENSION(MVSIZ,2) ,INTENT(OUT) :: ZCFAC
      my_real ,DIMENSION(NUPARAM) ,INTENT(IN)  :: UPARAM
      my_real ,DIMENSION(NEL) ,INTENT(IN) ::  SHF,
     .   EPSXX,EPSYY,EPSXY,EPSYZ,EPSZX,SIGOXX,SIGOYY,SIGOXY,SIGOYZ,SIGOZX,FACTOR
      my_real ,DIMENSION(NEL) ,INTENT(OUT) :: SSP,
     .   SIGNXX,SIGNYY,SIGNXY,SIGNYZ,SIGNZX
      my_real ,DIMENSION(NEL,3) ,INTENT(INOUT) :: SIGI
      INTEGER ,DIMENSION(NSENSR,NSENSOR) ,INTENT(IN) :: SENSOR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,II,ISENS
      my_real :: E11,E22,NU12,NU21,G12,G23,G31,A11,A12,A22,RCOMP,
     .           S,D,T,P1,P2,R,BETA,ZEROSTRESS,TSTART,DSIG
      my_real ,DIMENSION(NEL) :: ET
C=======================================================================
      E11   = UPARAM(1)
      E22   = UPARAM(2)
      NU12  = UPARAM(3)
      NU21  = UPARAM(4)
      G12   = UPARAM(5)
      G23   = UPARAM(6)
      G31   = UPARAM(7)
      RCOMP = UPARAM(8)
      A11   = UPARAM(10)
      A22   = UPARAM(11)
      A12   = UPARAM(12)
      ISENS = NINT(UPARAM(13))
      ZEROSTRESS = UPARAM(14)
      SSP(1:NEL) = UPARAM(9)
c-------------------------
      DO I=1,NEL
        SIGNXX(I) = A11*EPSXX(I) + A12*EPSYY(I)
        SIGNYY(I) = A12*EPSXX(I) + A22*EPSYY(I)
        SIGNXY(I) = G12*EPSXY(I)
        SIGNYZ(I) = G23*EPSYZ(I)*SHF(I)
        SIGNZX(I) = G31*EPSZX(I)*SHF(I)
      ENDDO
C-----------------------------
C     CONTRAINTES reduites en compression
C-----------------------------
      DO  I=1,NEL
       S  = HALF*(SIGNXX(I) + SIGNYY(I))
       D  = HALF*(SIGNXX(I) - SIGNYY(I))
       R  = SQRT(SIGNXY(I)**2 + D*D)
       P1 = S - R 
       ET(I) = ONE       
       IF (P1 < ZERO) THEN
         P2 = S + R 
         IF (P2 > ZERO) THEN
           BETA = HALF*((ONE-RCOMP)*S/R + ONE+RCOMP) 
           SIGNXX(I) = BETA*(SIGNXX(I)-P2) + P2 
           SIGNYY(I) = BETA*(SIGNYY(I)-P2) + P2 
           SIGNXY(I) = BETA*SIGNXY(I)
         ELSE
           BETA = RCOMP
           SIGNXX(I) = BETA*SIGNXX(I)
           SIGNYY(I) = BETA*SIGNYY(I)
           SIGNXY(I) = BETA*SIGNXY(I)
         ENDIF
         ET(I) = BETA
       ENDIF
      ENDDO
c-----------------------------------------------------------
c     REF-STATE ZEROSTRESS OPTION
c-----------------------------------------------------------
      IF (ZEROSTRESS /= ZERO)THEN
        TSTART = ZERO
        DO II=1,NSENSOR
          IF (ISENS == LSENSOR(1,II)) THEN
            TSTART  = SENSOR(2,II)
            EXIT
          ENDIF
        ENDDO
c          
        DO I=1,NEL
          IF (TT <= TSTART) THEN
            SIGI(I,1) = SIGNXX(I)
            SIGI(I,2) = SIGNYY(I)
            SIGI(I,3) = SIGNXY(I)
            SIGNXX(I) = ZERO
            SIGNYY(I) = ZERO
            SIGNXY(I) = ZERO
          ELSE
            DSIG = SIGNXX(I) - SIGOXX(I) - SIGI(I,1)
            IF (SIGI(I,1) > ZERO .AND. DSIG < ZERO) THEN
              SIGI(I,1) = MAX(ZERO,SIGI(I,1)+ZEROSTRESS*DSIG)
            ELSEIF (SIGI(I,1) < ZERO .AND. DSIG > ZERO) THEN
              SIGI(I,1) = MIN(ZERO,SIGI(I,1)+ZEROSTRESS*DSIG)
            ENDIF
            DSIG = SIGNYY(I) - SIGOYY(I) - SIGI(I,2)
            IF (SIGI(I,2) > ZERO .AND. DSIG < ZERO) THEN
              SIGI(I,2) = MAX(ZERO,SIGI(I,2)+ZEROSTRESS*DSIG)
            ELSEIF (SIGI(I,2) < ZERO .AND. DSIG > ZERO) THEN
              SIGI(I,2) = MIN(ZERO,SIGI(I,2)+ZEROSTRESS*DSIG)
            ENDIF
            DSIG = SIGNXY(I) - SIGOXY(I) - SIGI(I,3)
            IF (SIGI(I,3) > ZERO .AND. DSIG < ZERO) THEN
              SIGI(I,3) = MAX(ZERO,SIGI(I,3)+ZEROSTRESS*DSIG)
            ELSEIF (SIGI(I,3) < ZERO .AND. DSIG > ZERO) THEN
              SIGI(I,3) = MIN(ZERO,SIGI(I,3)+ZEROSTRESS*DSIG)
            ENDIF
            SIGNXX(I) = SIGNXX(I) - SIGI(I,1)
            SIGNYY(I) = SIGNYY(I) - SIGI(I,2)
            SIGNXY(I) = SIGNXY(I) - SIGI(I,3)
          ENDIF
        ENDDO
      ENDIF
c
C-----Scale of stress when close of slipring
      DO I=1,NEL
        SIGNXX(I) = SIGNXX(I)*FACTOR(I)
        SIGNYY(I) = SIGNYY(I)*FACTOR(I)
        SIGNXY(I) = SIGNXY(I)*FACTOR(I)
        SIGNYZ(I) = SIGNYZ(I)*FACTOR(I)
        SIGNZX(I) = SIGNZX(I)*FACTOR(I)
      ENDDO
c
C------for QEPH-------       
       IF (FLAG_ZCFAC) THEN
         DO I=1,NEL
           ZCFAC(I,1) = ZCFAC(I,1) + ET(I) / NPTTOT
           ZCFAC(I,2) = MIN(ET(I),ZCFAC(I,2))
         ENDDO
       ENDIF
c--------------------------------------------------------
      RETURN
      END
