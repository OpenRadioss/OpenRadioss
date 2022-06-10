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
Chd|  DAMA24                        source/materials/mat/mat024/dama24.F
Chd|-- called by -----------
Chd|        CONC24                        source/materials/mat/mat024/conc24.F
Chd|-- calls ---------------
Chd|        PRI224                        source/materials/mat/mat024/pri224.F
Chd|        PRI324                        source/materials/mat/mat024/pri324.F
Chd|====================================================================
      SUBROUTINE DAMA24(NEL    ,NINDX  ,INDX   ,NGL    ,PM     ,SCLE2  ,
     .                  SIG    ,DAM    ,ANG    ,EPS_F  ,CRAK   ,CDAM   ,
     .                  S01    ,S02    ,S03    ,S04    ,S05    ,S06    ,
     .                  DEPS1  ,DEPS2  ,DEPS3  ,DEPS4  ,DEPS5  ,DEPS6  ,
     .                  DE1    ,DE2    ,DE3    ,SCAL1  ,SCAL2  ,SCAL3  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "units_c.inc"
#include      "vect01_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NINDX
      INTEGER INDX(NINDX),NGL(NEL)
      my_real, DIMENSION(NPROPM) :: PM
      my_real, DIMENSION(NEL) ,INTENT(IN) :: S01,S02,S03,S04,S05,S06,
     .  DEPS1,DEPS2,DEPS3,DEPS4,DEPS5,DEPS6,DE1,DE2,DE3,
     .  SCAL1,SCAL2,SCAL3,SCLE2
      my_real ,DIMENSION(NEL,3,3) ,INTENT(OUT) :: CDAM
      my_real, DIMENSION(NEL,6) ,INTENT(INOUT) :: SIG,ANG
      my_real, DIMENSION(NEL,3) ,INTENT(INOUT) :: EPS_F,DAM,CRAK
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,IDEB,IDIR
      my_real EPS(6), EPSTOT(6), DE(3), SCAL(3), SIGO(6), DIR(6),ANG0(6)
      my_real YOUNG, NU, G, DSUP, VMAX, QQ, AA, AC, EPST, SFTRY, DEN, DEPSF
C=======================================================================
      IDEB  = 0
c
      YOUNG = PM(20)
      NU    = PM(21)
      G     = PM(22)
      DSUP  = PM(26)
      VMAX  = PM(27)
      QQ    = PM(28)
      AA    = PM(38)
      AC    = PM(41)
      EPST  = PM(42)
c-------------------
      DO J = 1,NINDX
        I = INDX(J)
        EPS(1)   = DEPS1(I)
        EPS(2)   = DEPS2(I)
        EPS(3)   = DEPS3(I)
        EPS(4)   = DEPS4(I)
        EPS(5)   = DEPS5(I)
        EPS(6)   = DEPS6(I)
        DE(1)    = DE1(I)
        DE(2)    = DE2(I)
        DE(3)    = DE3(I)
        SCAL(1)  = SCAL1(I)
        SCAL(2)  = SCAL2(I)
        SCAL(3)  = SCAL3(I)
        EPSTOT(1)= CRAK(I,1)
        EPSTOT(2)= CRAK(I,2)
        EPSTOT(3)= CRAK(I,3)
        EPSTOT(4)= S04(I)/G
        EPSTOT(5)= S05(I)/G
        EPSTOT(6)= S06(I)/G
        ANG0(1) = ANG(I,1)
        ANG0(2) = ANG(I,2)
        ANG0(3) = ANG(I,3)
        ANG0(4) = ANG(I,4)
        ANG0(5) = ANG(I,5)
        ANG0(6) = ANG(I,6)
c---------------------
        SIGO(1) = S01(I)
        SIGO(2) = S02(I)
        SIGO(3) = S03(I)
        SIGO(4) = S04(I)
        SIGO(5) = S05(I)
        SIGO(6) = S06(I)
c---------------------
        IF (DAM(I,1) > ZERO) THEN
          IDIR = 3
          IF (DAM(I,2) == ZERO) THEN
            IDIR = 2
            CALL PRI224(SIGO,EPSTOT,EPS,DIR(4),ANG0)
          ENDIF
        ELSE
          IDIR = 1
          CALL PRI324(SIGO,EPSTOT,EPS,DIR)
        ENDIF
c
c       SAUVE LA DEFORMATION A RUPTURE
c
        SFTRY = MIN(EPSTOT(IDIR),EPSTOT(IDIR)-SCLE2(I)*EPS(IDIR),EPST)
        SFTRY = MAX(SFTRY,FOURTH*EPST)
c
        IF (EPSTOT(IDIR) < SFTRY) THEN
           IF (IDEB==1) THEN
           WRITE(IOUT, '(A,I1,A,I10,A,5X,A,E10.3,A,E10.3,A,E10.3)')
     .                 ' FAILURE-',IDIR,' ELEMENT #',NGL(I),' FUNNY!',
     .                 ' EPS_F ',SFTRY,'EPSTOT',EPSTOT(IDIR),' EPST ',EPST
           WRITE(ISTDO,'(A,I1,A,I10,A,5X,A,E10.3,A,E10.3,A,E10.3)')
     .                 ' FAILURE-',IDIR,' ELEMENT #',NGL(I),' FUNNY!',
     .                 ' EPS_F ',SFTRY,'EPSTOT',EPSTOT(IDIR),' EPST ',EPST
           ENDIF
           SIG(I,1) = S01(I)
           SIG(I,2) = S02(I)
           SIG(I,3) = S03(I)
           SIG(I,4) = S04(I)
           SIG(I,5) = S05(I)
           SIG(I,6) = S06(I)
c
           CYCLE
        ENDIF
c
c       SAUVE LES DIRECTIONS DE DOMMAGE
c
        IF (IDIR == 1) THEN
          ANG(I,1:6) = DIR(1:6)
        ELSEIF (IDIR == 2) THEN
          ANG(I,4:6) = DIR(4:6)
        ENDIF
c
c       CALCULE LE DOMMAGE
c
        DO K=1,3
          CRAK(I,K) = EPSTOT(K)
          DEPSF = EPSTOT(K) - SFTRY
          IF (DEPSF >= ZERO .AND. EPS_F(I,K) < ZERO) THEN  ! EPS_F initialized to -1
            IF (K >= 2) THEN
              IF (DAM(I,K-1) == ZERO) CYCLE
            ENDIF
            IF (IDEB==1) THEN
              WRITE(IOUT, '(A,I1,A,I10,A,3F6.3,A,3F6.3,A,E10.3,A,E10.3)')
     .                       ' FAILURE-',K,' ELEMENT #',NGL(I),
     .                       ' VEC-1 ',ANG(I,1),ANG(I,2),ANG(I,3),
     .                       ' VEC-2 ',ANG(I,4),ANG(I,5),ANG(I,6),
     .                       ' EPS_F ',SFTRY,' EPSTOT ',EPSTOT(K)
              WRITE(ISTDO,'(A,I1,A,I10,A,3F6.3,A,3F6.3,A,E10.3,A,E10.3)')
     .                       ' FAILURE-',K,' ELEMENT #',NGL(I),
     .                       ' VEC-1 ',ANG(I,1),ANG(I,2),ANG(I,3),
     .                       ' VEC-2 ',ANG(I,4),ANG(I,5),ANG(I,6),
     .                       ' EPS_F ',SFTRY,' EPSTOT ',EPSTOT(K)
            ENDIF

            EPS_F(I,K) = SFTRY
            DAM(I,K) = QQ * (ONE - EPS_F(I,K) / MAX(EPSTOT(K),EM20) )
            DAM(I,K) = MAX(DAM(I,K),EM20)
            DAM(I,K) = MIN(DAM(I,K),DSUP)
            DE(K)    = ONE - DAM(I,K)
            SCAL(K)  = ZERO
c
c
c            EPS_F(I,K) = SFTRY
c            DAM(I,K) = QQ * DEPSF / EPSTOT(K)
c            DAM(I,K) = MIN(DAM(I,K),DSUP)
c            DE(K)  = ONE - DAM(I,K)
c            SCAL(K)= ZERO
          ENDIF
        ENDDO   ! next K
c
c       CALCULE LA MATRICE DE HOOKE ET LES CONTRAINTES (rotation repere !)
c
        DEN = ONE - NU**2
     .      * (SCAL(1)*SCAL(2) + SCAL(2)*SCAL(3) + SCAL(3)*SCAL(1)
     .      + TWO*NU*SCAL(1)*SCAL(2)*SCAL(3))
C
        CDAM(I,1,1) = YOUNG*DE(1)*(ONE-NU**2*SCAL(2)*SCAL(3))/DEN
        CDAM(I,2,2) = YOUNG*DE(2)*(ONE-NU**2*SCAL(1)*SCAL(3))/DEN
        CDAM(I,3,3) = YOUNG*DE(3)*(ONE-NU**2*SCAL(2)*SCAL(1))/DEN
        CDAM(I,1,2) = NU*YOUNG*SCAL(1)*SCAL(2)*(ONE+NU*SCAL(3))/DEN
        CDAM(I,2,3) = NU*YOUNG*SCAL(2)*SCAL(3)*(ONE+NU*SCAL(1))/DEN
        CDAM(I,1,3) = NU*YOUNG*SCAL(1)*SCAL(3)*(ONE+NU*SCAL(2))/DEN
        CDAM(I,2,1) = CDAM(I,1,2)
        CDAM(I,3,1) = CDAM(I,1,3)
        CDAM(I,3,2) = CDAM(I,2,3)
c
        SIG(I,1) = CDAM(I,1,1)*EPSTOT(1)
     .           + CDAM(I,1,2)*EPSTOT(2)
     .           + CDAM(I,1,3)*EPSTOT(3)
        SIG(I,2) = CDAM(I,2,1)*EPSTOT(1)
     .           + CDAM(I,2,2)*EPSTOT(2)
     .           + CDAM(I,2,3)*EPSTOT(3)
        SIG(I,3) = CDAM(I,3,1)*EPSTOT(1)
     .           + CDAM(I,3,2)*EPSTOT(2)
     .           + CDAM(I,3,3)*EPSTOT(3)
        SIG(I,4) = SCAL(1)*SCAL(2)*SIGO(4)
        SIG(I,5) = SCAL(2)*SCAL(3)*SIGO(5)
        SIG(I,6) = SCAL(3)*SCAL(1)*SIGO(6)
c
      ENDDO
c-----------
      RETURN
      END
