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
Chd|  SET_FAILWAVE_NOD3             source/materials/fail/failwave/set_failwave_nod3.F
Chd|-- called by -----------
Chd|        C3FORC3                       source/elements/sh3n/coque3n/c3forc3.F
Chd|        CDKFORC3                      source/elements/sh3n/coquedk/cdkforc3.F
Chd|-- calls ---------------
Chd|        SEG_INTERSECT                 source/materials/fail/failwave/seg_intersect.F
Chd|        FAILWAVE_MOD                  ../common_source/modules/failwave_mod.F
Chd|====================================================================
      SUBROUTINE SET_FAILWAVE_NOD3(FAILWAVE   ,FWAVE_EL ,NGL      ,
     .           NEL      ,IXTG     ,ITAB     ,CRKDIR   ,DIR_A    ,
     .           NROT     ,XL2      ,XL3      ,YL2      ,YL3      )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FAILWAVE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "scr17_c.inc"
#include "com04_c.inc"
#include "units_c.inc"
#include "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NROT
      INTEGER ,DIMENSION(NIXTG,NEL),INTENT(IN)    :: IXTG
      INTEGER ,DIMENSION(NUMNOD)   ,INTENT(IN)    :: ITAB 
      INTEGER ,DIMENSION(NEL)      ,INTENT(IN)    :: NGL,FWAVE_EL
C
      my_real ,DIMENSION(NEL,NROT) ,INTENT(IN)    :: DIR_A
      my_real ,DIMENSION(NEL,2)    ,INTENT(IN)    :: CRKDIR
      my_real ,DIMENSION(NEL)      ,INTENT(IN)    :: XL2,XL3,YL2,YL3
      TYPE (FAILWAVE_STR_)  :: FAILWAVE 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,K,N1,N2,N3,INTERSECTION,INT1,INT2,INT3,INT4,INT5,INT6,
     .   LEVEL,NEWCRK1,NEWCRK2,
     .        NCURR,MAXLEV,FOUND_EXISTING,IDEBUG
      INTEGER ,DIMENSION(NEL) :: INDX1,INDX2
      INTEGER ,DIMENSION(3)   :: IDF1,IDF2,NOD_ID,NOD_NN
C
      my_real :: DX1,DX2,DY1,DY2,DIR11,DIR22,COSX,SINX,COSY,SINY,
     .   LMAX,XM,YM,X1,Y1,X2,Y2,X3,Y3,X4,Y4,X5,Y5,X6,Y6,XINT,YINT,
     .   RX,RY,RAT1,RAT2
      INTEGER  SEG_INTERSECT
      EXTERNAL SEG_INTERSECT
c-----------------------------------------------
c  damaged elements will set nodal frontwave values to propagate crack info
C=======================================================================
      IDEBUG = 0
c
c---------------
      SELECT CASE (FAILWAVE%WAVE_MOD)
c---------------
        CASE (1)   ! isotropic propagation
c---------------
          DO I=1,NEL
            IF (FWAVE_EL(I) < 0) THEN
              N1 = FAILWAVE%IDXI(IXTG(2,I))
              N2 = FAILWAVE%IDXI(IXTG(3,I))
              N3 = FAILWAVE%IDXI(IXTG(4,I))
              FAILWAVE%FWAVE_NOD_STACK(1,N1,1) = 1
              FAILWAVE%FWAVE_NOD_STACK(1,N2,1) = 1
              FAILWAVE%FWAVE_NOD_STACK(1,N3,1) = 1
              FAILWAVE%MAXLEV_STACK(N1) = 1
              FAILWAVE%MAXLEV_STACK(N2) = 1
              FAILWAVE%MAXLEV_STACK(N3) = 1
            ENDIF
          ENDDO
c---------------
        CASE (2,3)   ! directional propagation
c---------------
          NEWCRK1 = 0
          NEWCRK2 = 0
          DO I=1,NEL
            IF (FWAVE_EL(I) == -1) THEN        ! DIR 1 vient de cracker
              NEWCRK1 = NEWCRK1 + 1
              INDX1(NEWCRK1) = I
            ELSEIF (FWAVE_EL(I) == -2) THEN    ! DIR 2 vient de cracker
              NEWCRK2 = NEWCRK2 + 1
              INDX2(NEWCRK2) = I
            ELSEIF (FWAVE_EL(I) == -3) THEN    ! deux directions viennent de cracker
              NEWCRK1 = NEWCRK1 + 1
              NEWCRK2 = NEWCRK2 + 1
              INDX1(NEWCRK1) = I
              INDX2(NEWCRK2) = I
            ENDIF
          ENDDO
c
          IF (NEWCRK1 + NEWCRK2 > 0) THEN
c
            RAT1 = HALF * TAN(PI/SIX)
            RAT2 = ONE - RAT1
c------------------------------------------------
c             Propagation in first direction    
c------------------------------------------------
            DO II=1,NEWCRK1
              I = INDX1(II)
              N1 = IXTG(2,I)
              N2 = IXTG(3,I)
              N3 = IXTG(4,I)
              NOD_NN(1) = FAILWAVE%IDXI(N1)  
              NOD_NN(2) = FAILWAVE%IDXI(N2)  
              NOD_NN(3) = FAILWAVE%IDXI(N3)  
              NOD_ID(1) = ITAB(N1)   
              NOD_ID(2) = ITAB(N2)   
              NOD_ID(3) = ITAB(N3)   
              IDF1(:) = 0
              IDF2(:) = 0
c
              IDEBUG = 0
c              if (NGL(I) == 15607559) IDEBUG= 1
              
              IF (NROT == 0) THEN
                DIR11 = -CRKDIR(I,2)
                DIR22 =  CRKDIR(I,1)
              ELSE
                COSX  = DIR_A(I,1)
                SINX  = DIR_A(I,2)
                COSY  =-CRKDIR(I,2)                  
                SINY  = CRKDIR(I,1)                   
                DIR11 = COSX*COSY - SINX*SINY
                DIR22 = COSX*SINY + SINX*COSY
              ENDIF
c
              XM = (XL2(I) + XL3(I)) * THIRD
              YM = (YL2(I) + YL3(I)) * THIRD
              LMAX = MAX(XL2(I)*2 + YL2(I)**2, XL3(I)**2 + YL3(I))
              LMAX = SQRT(LMAX) * TWO
c
              DX1 = XM - DIR11 * LMAX
              DY1 = YM - DIR22 * LMAX
              DX2 = XM + DIR11 * LMAX
              DY2 = YM + DIR22 * LMAX        
c
              X1 = XL2(I)*RAT1
              Y1 = YL2(I)*RAT1
              X2 = XL2(I)*RAT2
              Y2 = YL2(I)*RAT2
              RX = XL3(I) - XL2(I)
              RY = YL3(I) - YL2(I)
              X3 = XL2(I) + RX * RAT1
              Y3 = YL2(I) + RY * RAT1
              X4 = XL2(I) + RX * RAT2
              Y4 = YL2(I) + RY * RAT2
              RX =-XL3(I)
              RY =-YL3(I)
              X5 = XL3(I) + RX * RAT1
              Y5 = YL3(I) + RY * RAT1
              X6 = XL3(I) + RX * RAT2
              Y6 = YL3(I) + RY * RAT2
c
              INT1 = SEG_INTERSECT(X6,Y6,X1,Y1,DX1,DY1,DX2,DY2,XINT,YINT,IDEBUG)
              INT2 = SEG_INTERSECT(X2,Y2,X3,Y3,DX1,DY1,DX2,DY2,XINT,YINT,IDEBUG)
              INT3 = SEG_INTERSECT(X4,Y4,X5,Y5,DX1,DY1,DX2,DY2,XINT,YINT,IDEBUG)
              INT4 = SEG_INTERSECT(X1,Y1,X2,Y2,DX1,DY1,DX2,DY2,XINT,YINT,IDEBUG)
              INT5 = SEG_INTERSECT(X3,Y3,X4,Y4,DX1,DY1,DX2,DY2,XINT,YINT,IDEBUG)
              INT6 = SEG_INTERSECT(X5,Y5,X6,Y6,DX1,DY1,DX2,DY2,XINT,YINT,IDEBUG)
c              
              INTERSECTION = 0
              IF (INT1 == 1) THEN        ! N1 + N2-N3
                IDF1(1) = NOD_ID(3)
                IDF2(1) = NOD_ID(2)
                IDF1(2) = NOD_ID(3)
                IDF1(3) = NOD_ID(2)
                INTERSECTION = 1
              ELSE IF (INT2 == 1) THEN   ! N2 + N3-N1
                IDF1(1) = NOD_ID(3)
                IDF1(3) = NOD_ID(1)
                IDF1(2) = NOD_ID(1)
                IDF2(2) = NOD_ID(3)
                INTERSECTION = 1
              ELSE IF (INT3 == 1) THEN   ! N3 + N1-N2
                IDF1(1) = NOD_ID(2)
                IDF1(2) = NOD_ID(1)
                IDF1(3) = NOD_ID(2)
                IDF2(3) = NOD_ID(1)
                INTERSECTION = 1
              ELSE IF (INT4 == 1) THEN   ! N3 + N1-N2
                IDF1(1) = NOD_ID(2)
                IDF1(2) = NOD_ID(1)
                IDF1(3) = NOD_ID(2)
                IDF2(3) = NOD_ID(1)
                INTERSECTION = 1
              ELSE IF (INT5 == 1) THEN   ! N1 + N2-N3
                IDF1(1) = NOD_ID(3)
                IDF2(1) = NOD_ID(2)
                IDF1(2) = NOD_ID(3)
                IDF1(3) = NOD_ID(2)
                INTERSECTION = 1
              ELSE IF (INT6 == 1) THEN   ! N2 + N3-N1
                IDF1(1) = NOD_ID(3)
                IDF1(3) = NOD_ID(1)
                IDF1(2) = NOD_ID(1)
                IDF2(2) = NOD_ID(3)
                INTERSECTION = 1
              END IF
c
              IF (INTERSECTION == 1) THEN
                DO K=1,3
                  NCURR  = NOD_NN(K)    
c--------------------------------------------------------------------
!$OMP ATOMIC CAPTURE 
                  FAILWAVE%MAXLEV_STACK(NCURR) = FAILWAVE%MAXLEV_STACK(NCURR) + 1 
                  MAXLEV = FAILWAVE%MAXLEV_STACK(NCURR)
!$OMP END ATOMIC
c--------------------------------------------------------------------
                  IF (MAXLEV > FAILWAVE%SIZE) THEN
#include "lockon.inc"
                    WRITE(IOUT,*) 'ERROR IN FAILWAVE PROPGATION: ELEMENT =',NGL(I),
     .                            'LEVEL=',MAXLEV
#include "lockoff.inc"
                    MAXLEV = FAILWAVE%SIZE
                    FAILWAVE%MAXLEV_STACK(NCURR) = MAXLEV
                  ENDIF
                  FAILWAVE%FWAVE_NOD_STACK(1,NCURR,MAXLEV) = IDF1(K)
                  FAILWAVE%FWAVE_NOD_STACK(2,NCURR,MAXLEV) = IDF2(K)
                END DO ! K=1,3

              ELSE    ! NO intersection found       
c
#include "lockon.inc"
                  WRITE(IOUT,*) 'ERROR ADVANCING CRACK IN ELEMENT, 1st DIR ',NGL(I)     
#include "lockoff.inc"
              ENDIF
c
            ENDDO ! NEWCRK1 
c--------------------------------------
c           Propagation in second direction    
c--------------------------------------
            DO II=1,NEWCRK2
              I = INDX2(II)
              N1 = IXTG(2,I)
              N2 = IXTG(3,I)
              N3 = IXTG(4,I)
              NOD_NN(1) = FAILWAVE%IDXI(N1)  
              NOD_NN(2) = FAILWAVE%IDXI(N2)  
              NOD_NN(3) = FAILWAVE%IDXI(N3)  
              NOD_ID(1) = ITAB(N1)   
              NOD_ID(2) = ITAB(N2)   
              NOD_ID(3) = ITAB(N3)   
              IDF1(:) = 0
              IDF2(:) = 0
c
              IF (NROT == 0) THEN
                DIR11 = CRKDIR(I,1)
                DIR22 = CRKDIR(I,2)
              ELSE
                COSX  = DIR_A(I,1)
                SINX  = DIR_A(I,2)
                COSY  = CRKDIR(I,1)                  
                SINY  = CRKDIR(I,2)                    
                DIR11 = COSX*COSY - SINX*SINY
                DIR22 = COSX*SINY + SINX*COSY
              ENDIF
c
              XM = (XL2(I) + XL3(I)) * THIRD
              YM = (YL2(I) + YL3(I)) * THIRD
              LMAX = MAX(XL2(I)*2 + YL2(I)**2, XL3(I)**2 + YL3(I))
              LMAX = SQRT(LMAX) * TWO
c
              DX1 = XM - DIR11 * LMAX
              DY1 = YM - DIR22 * LMAX
              DX2 = XM + DIR11 * LMAX
              DY2 = YM + DIR22 * LMAX        
c
              X1 = XL2(I)*RAT1
              Y1 = YL2(I)*RAT1
              X2 = XL2(I)*RAT2
              Y2 = YL2(I)*RAT2
              RX = XL3(I) - XL2(I)
              RY = YL3(I) - YL2(I)
              X3 = XL2(I) + RX * RAT1
              Y3 = YL2(I) + RY * RAT1
              X4 = XL2(I) + RX * RAT2
              Y4 = YL2(I) + RY * RAT2
              RX =-XL3(I)
              RY =-YL3(I)
              X5 = XL3(I) + RX * RAT1
              Y5 = YL3(I) + RY * RAT1
              X6 = XL3(I) + RX * RAT2
              Y6 = YL3(I) + RY * RAT2
c              
              INT1 = SEG_INTERSECT(X6,Y6,X1,Y1,DX1,DY1,DX2,DY2,XINT,YINT,IDEBUG)
              INT2 = SEG_INTERSECT(X2,Y2,X3,Y3,DX1,DY1,DX2,DY2,XINT,YINT,IDEBUG)
              INT3 = SEG_INTERSECT(X4,Y4,X5,Y5,DX1,DY1,DX2,DY2,XINT,YINT,IDEBUG)
              INT4 = SEG_INTERSECT(X1,Y1,X2,Y2,DX1,DY1,DX2,DY2,XINT,YINT,IDEBUG)
              INT5 = SEG_INTERSECT(X3,Y3,X4,Y4,DX1,DY1,DX2,DY2,XINT,YINT,IDEBUG)
              INT6 = SEG_INTERSECT(X5,Y5,X6,Y6,DX1,DY1,DX2,DY2,XINT,YINT,IDEBUG)
c              
              INTERSECTION = 0
              IF (INT1 == 1) THEN        ! N1 + N2-N3
                IDF1(1) = NOD_ID(3)
                IDF2(1) = NOD_ID(2)
                IDF1(2) = NOD_ID(3)
                IDF1(3) = NOD_ID(2)
                INTERSECTION = 1
              ELSE IF (INT2 == 1) THEN   ! N2 + N3-N1
                IDF1(1) = NOD_ID(3)
                IDF1(3) = NOD_ID(1)
                IDF1(2) = NOD_ID(1)
                IDF2(2) = NOD_ID(3)
                INTERSECTION = 1
              ELSE IF (INT3 == 1) THEN   ! N3 + N1-N2
                IDF1(1) = NOD_ID(2)
                IDF1(2) = NOD_ID(1)
                IDF1(3) = NOD_ID(2)
                IDF2(3) = NOD_ID(1)
                INTERSECTION = 1
              ELSE IF (INT4 == 1) THEN   ! N3 + N1-N2
                IDF1(1) = NOD_ID(2)
                IDF1(2) = NOD_ID(1)
                IDF1(3) = NOD_ID(2)
                IDF2(3) = NOD_ID(1)
                INTERSECTION = 1
              ELSE IF (INT5 == 1) THEN   ! N1 + N2-N3
                IDF1(1) = NOD_ID(3)
                IDF2(1) = NOD_ID(2)
                IDF1(2) = NOD_ID(3)
                IDF1(3) = NOD_ID(2)
                INTERSECTION = 1
              ELSE IF (INT6 == 1) THEN   ! N2 + N3-N1
                IDF1(1) = NOD_ID(3)
                IDF1(3) = NOD_ID(1)
                IDF1(2) = NOD_ID(1)
                IDF2(2) = NOD_ID(3)
                INTERSECTION = 1
              END IF
c
              IF (INTERSECTION == 1) THEN
                DO K=1,3
                  NCURR  = NOD_NN(K)    
c--------------------------------------------------------------------
!$OMP ATOMIC CAPTURE 
                  FAILWAVE%MAXLEV_STACK(NCURR) = FAILWAVE%MAXLEV_STACK(NCURR) + 1 
                  MAXLEV = FAILWAVE%MAXLEV_STACK(NCURR)
!$OMP END ATOMIC
c--------------------------------------------------------------------
                  IF (MAXLEV > FAILWAVE%SIZE) THEN
#include "lockon.inc"
                    WRITE(IOUT,*) 'ERROR IN FAILWAVE PROPGATION: ELEMENT =',NGL(I),
     .                            'LEVEL=',MAXLEV
#include "lockoff.inc"
                    MAXLEV = FAILWAVE%SIZE
                    FAILWAVE%MAXLEV_STACK(NCURR) = MAXLEV
                  ENDIF
                  FAILWAVE%FWAVE_NOD_STACK(1,NCURR,MAXLEV) = IDF1(K)
                  FAILWAVE%FWAVE_NOD_STACK(2,NCURR,MAXLEV) = IDF2(K)
                END DO ! K=1,3

              ELSE    !  NO intersection found    
c
#include "lockon.inc"
                  WRITE(IOUT,*) 'ERROR ADVANCING CRACK IN ELEMENT, 2nd DIR ',NGL(I)     
#include "lockoff.inc"
              ENDIF
c
            ENDDO ! NEWCRK2 
          END IF  ! NEWCRK1 + NEWCRK2 > 0
c
c---------------
      END SELECT
c---------------
      RETURN
      END
