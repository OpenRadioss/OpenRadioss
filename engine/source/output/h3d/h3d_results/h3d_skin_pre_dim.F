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
Chd|  H3D_SKIN_PRE_DIM              source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|-- called by -----------
Chd|        H3D_SKIN_DIM                  source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|-- calls ---------------
Chd|        PINCHTYPE_MOD                 ../common_source/modules/pinchtype_mod.F
Chd|====================================================================
      SUBROUTINE H3D_SKIN_PRE_DIM(IB     ,ILOADP    ,LLOADP , NSKIN0,NSKIN)
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE PINCHTYPE_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   E x t e r n a l  F u n c t i o n s
C-----------------------------------------------
C-----------------------------------------------,
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LLOADP(SLLOADP),NSKIN,NSKIN0
      INTEGER ILOADP(SIZLOADP,*),IB(NIBCLD,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NL, N1, ISK, N2, N3, N4, N5, J,
     .        IAD ,NP ,IFUNC ,NPRES ,N,NSKIN_I,NSKINP0,IERR,IXST
      INTEGER, DIMENSION(:), ALLOCATABLE  :: ITAGN
      INTEGER, DIMENSION(:,:), ALLOCATABLE  :: IRECT
C=======================================================================
C-----Force (pressure) first 
       NSKIN_I = NSKIN
       DO NL=1,NCONLD-NPLOADPINCH
         N1      = IB(1,NL)
         N2      = IB(2,NL)
         N3      = IB(3,NL)
         N4      = IB(4,NL)
C        
		 IF (N1>0.AND.N2>0.AND.N3>0.AND.N4/=-1) NSKIN = NSKIN + 1
       END DO 
C----------load_pressure
       DO  NL=1,NLOADP_HYD
           IAD = ILOADP(4,NL)
           DO N=1, ILOADP(1,NL)/4
              N1 = LLOADP(IAD+4*(N-1))
              N2 = LLOADP(IAD+4*(N-1)+1)
              N3 = LLOADP(IAD+4*(N-1)+2)
              N4 = LLOADP(IAD+4*(N-1)+3)
		     IF (N1>0.AND.N2>0.AND.N3>0) NSKIN = NSKIN + 1
           ENDDO
       END DO 
C---------pfluid+pblast	   
       DO NL=1,NLOADP_F+NLOADP_B
         IAD = ILOADP(4,NL)
           DO N=1, ILOADP(1,NL)/4
              N1 = LLOADP(IAD+4*(N-1))
              N2 = LLOADP(IAD+4*(N-1)+1)
              N3 = LLOADP(IAD+4*(N-1)+2)
              N4 = LLOADP(IAD+4*(N-1)+3)
		     IF (N1>0.AND.N2>0.AND.N3>0) NSKIN = NSKIN + 1
           ENDDO
       END DO 
       NSKIN0 = NSKIN
       NSKINP0 = NSKIN0 - NSKIN_I
       IF (NSKINP0 > (NCONLD-NPLOADPINCH)) THEN
         ALLOCATE(IRECT(4,NSKINP0),ITAGN(NUMNOD),STAT=IERR)
C
         NP = 0 
         ITAGN = 0         
         DO NL=1,NCONLD-NPLOADPINCH
           N1      = IB(1,NL)
           N2      = IB(2,NL)
           N3      = IB(3,NL)
           N4      = IB(4,NL)
C          
		   IF (N1==0.OR.N2==0.OR.N3==0.OR.N4==-1) CYCLE
           IF (N4==0) N4 = N3
           NP = NP +1
           ITAGN(N1) = 1
           ITAGN(N2) = 1
           ITAGN(N3) = 1
           ITAGN(N4) = 1
           IRECT(1:4,NP) = IB(1:4,NL)
         END DO 
C----add only not existing           
           DO  NL=1,NLOADP_HYD
              IAD = ILOADP(4,NL)
              DO N=1, ILOADP(1,NL)/4
                N1 = LLOADP(IAD+4*(N-1))
                N2 = LLOADP(IAD+4*(N-1)+1)
                N3 = LLOADP(IAD+4*(N-1)+2)
                N4 = LLOADP(IAD+4*(N-1)+3)
		        IF (N1==0.OR.N2==0.OR.N3==0) CYCLE
                IF (N4==0) N4 = N3
                IF(ITAGN(N1)+ITAGN(N2)+ITAGN(N3)+ITAGN(N4)<4) THEN
                   NP = NP +1
                   ITAGN(N1) = 1
                   ITAGN(N2) = 1
                   ITAGN(N3) = 1
                   ITAGN(N4) = 1
                   IRECT(1:4,NP) = IB(1:4,NL)
                ELSE
                  IXST = 0
                  J = 0
                  DO WHILE (IXST==0 .AND. J < NP)
                    J = J + 1
                    IF (N1 /= IRECT(1,J)) CYCLE
                    IF (N2 /= IRECT(2,J)) CYCLE
                    IF (N3 /= IRECT(3,J)) CYCLE
                    IF (N4 /= IRECT(4,J)) CYCLE
                    IXST = 1
                  END DO
                  IF (IXST == 0 )THEN
                    NP = NP +1
                    ITAGN(N1) = 1
                    ITAGN(N2) = 1
                    ITAGN(N3) = 1
                    ITAGN(N4) = 1
                    IRECT(1:4,NP) = IB(1:4,NL)
                  END IF
                END IF 
              ENDDO
           END DO 
           DO  NL=1,NLOADP_F + NLOADP_B
              IAD = ILOADP(4,NL)
              DO N=1, ILOADP(1,NL)/4
                N1 = LLOADP(IAD+4*(N-1))
                N2 = LLOADP(IAD+4*(N-1)+1)
                N3 = LLOADP(IAD+4*(N-1)+2)
                N4 = LLOADP(IAD+4*(N-1)+3)
		        IF (N1==0.OR.N2==0.OR.N3==0) CYCLE
                IF (N4==0) N4 = N3
                IF(ITAGN(N1)+ITAGN(N2)+ITAGN(N3)+ITAGN(N4)<4) THEN
                   NP = NP +1
                   ITAGN(N1) = 1
                   ITAGN(N2) = 1
                   ITAGN(N3) = 1
                   ITAGN(N4) = 1
                   IRECT(1,NP) = N1
                   IRECT(2,NP) = N2
                   IRECT(3,NP) = N3
                   IRECT(4,NP) = N4
                ELSE
                  IXST = 0
                  J = 0
                  DO WHILE (IXST==0 .AND. J < NP)
                    J = J + 1
                    IF (N1 /= IRECT(1,J)) CYCLE
                    IF (N2 /= IRECT(2,J)) CYCLE
                    IF (N3 /= IRECT(3,J)) CYCLE
                    IF (N4 /= IRECT(4,J)) CYCLE
                    IXST = 1
                  END DO
                  IF (IXST == 0 )THEN
                    NP = NP +1
                    ITAGN(N1) = 1
                    ITAGN(N2) = 1
                    ITAGN(N3) = 1
                    ITAGN(N4) = 1
                    IRECT(1,NP) = N1
                    IRECT(2,NP) = N2
                    IRECT(3,NP) = N3
                    IRECT(4,NP) = N4
                  END IF
                END IF 
              ENDDO
           END DO 
          NSKIN = NSKIN_I + NP
          DEALLOCATE(IRECT,ITAGN)
       END IF
C-----some segs are used > 1 times       
C
      RETURN
      END
