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
Chd|  H3D_SKIN_PRE_MAP              source/output/h3d/h3d_results/h3d_skin_pre_map.F
Chd|-- called by -----------
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|-- calls ---------------
Chd|        H3D_INC_MOD                   share/modules/h3d_inc_mod.F   
Chd|        LOADS_MOD                     ../common_source/modules/loads/loads_mod.F
Chd|        PINCHTYPE_MOD                 ../common_source/modules/pinchtype_mod.F
Chd|====================================================================
      SUBROUTINE H3D_SKIN_PRE_MAP(IB     ,ILOADP    ,LLOADP ,IMAPSKP,LOADS)
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE PINCHTYPE_MOD 
      USE H3D_INC_MOD
      USE LOADS_MOD
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
      INTEGER, DIMENSION(SIZLOADP,NLOADP), INTENT(IN)  :: ILOADP
      INTEGER, DIMENSION(SLLOADP), INTENT(IN)  :: LLOADP
      INTEGER, DIMENSION(NIBCLD,NCONLD), INTENT(IN)  :: IB
      INTEGER, DIMENSION(NUMSKINP0), INTENT(OUT)  :: IMAPSKP
      TYPE (LOADS_)   , INTENT(IN) :: LOADS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NL, N1, ISK, N2, N3, N4, N5, J,IXST,
     .        IAD ,NP ,NP0 ,NPRES ,N,NSKIN_I,NSKINP0,IERR,SHIFT
      INTEGER, DIMENSION(:,:), ALLOCATABLE  :: IRECT
      INTEGER, DIMENSION(:), ALLOCATABLE  :: ITAGN
C=======================================================================
       ALLOCATE(IRECT(4,NUMSKINP),ITAGN(NUMNOD),STAT=IERR)
C
         ITAGN = 0         
         NP = 0
         DO NL=1,NCONLD-NPLOADPINCH
           N1      = IB(1,NL)
           N2      = IB(2,NL)
           N3      = IB(3,NL)
           N4      = IB(4,NL)
C          
		   IF (N1==0.OR.N2==0.OR.N3==0.OR.N4==-1) CYCLE
           NP = NP +1
           IRECT(1:4,NP) = IB(1:4,NL)
           IF (IRECT(4,NP)==0) IRECT(4,NP)=IRECT(3,NP)
           IF (N4==0) N4 = N3
           ITAGN(N1) = 1
           ITAGN(N2) = 1
           ITAGN(N3) = 1
           ITAGN(N4) = 1
           IMAPSKP(NP) = NP         
         END DO
         NP0 = NP         
C----add only not existing           
           SHIFT = 	NLOADP_F+NLOADP_B
           DO NL=1+SHIFT,NLOADP_HYD+SHIFT
              IAD = ILOADP(4,NL)
              DO N=1, ILOADP(1,NL)/4
                N1 = LLOADP(IAD+4*(N-1))
                N2 = LLOADP(IAD+4*(N-1)+1)
                N3 = LLOADP(IAD+4*(N-1)+2)
                N4 = LLOADP(IAD+4*(N-1)+3)
		        IF (N1==0.OR.N2==0.OR.N3==0) CYCLE
                IF (N4==0) N4 = N3
                NP0 = NP0 +1
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
                   IMAPSKP(NP0) = NP         
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
                    IMAPSKP(NP0) = J         
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
                    IMAPSKP(NP0) = NP         
                  END IF
                END IF 
              ENDDO
           END DO 
C           
           DO  NL=1,NLOADP_F+NLOADP_B
              IAD = ILOADP(4,NL)
              DO N=1, ILOADP(1,NL)/4
                N1 = LLOADP(IAD+4*(N-1))
                N2 = LLOADP(IAD+4*(N-1)+1)
                N3 = LLOADP(IAD+4*(N-1)+2)
                N4 = LLOADP(IAD+4*(N-1)+3)
		        IF (N1==0.OR.N2==0.OR.N3==0) CYCLE
                IF (N4==0) N4 = N3
                NP0 = NP0 +1
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
                   IMAPSKP(NP0) = NP         
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
                    IMAPSKP(NP0) = J         
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
                    IMAPSKP(NP0) = NP         
                  END IF
                END IF 
              ENDDO
           END DO 
C           
           DO  NL=1,LOADS%NLOAD_CYL
              DO N=1, LOADS%LOAD_CYL(NL)%NSEG
                N1 = LOADS%LOAD_CYL(NL)%SEGNOD(N,1)
                N2 = LOADS%LOAD_CYL(NL)%SEGNOD(N,2)
                N3 = LOADS%LOAD_CYL(NL)%SEGNOD(N,3)
                N4 = LOADS%LOAD_CYL(NL)%SEGNOD(N,4)
                IF (N4==0) N4 = N3
                NP0 = NP0 +1
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
                   IMAPSKP(NP0) = NP         
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
                    IMAPSKP(NP0) = J         
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
                    IMAPSKP(NP0) = NP         
                  END IF
                END IF 
              ENDDO
           END DO 
        DEALLOCATE(IRECT,ITAGN)
C
      RETURN
      END SUBROUTINE H3D_SKIN_PRE_MAP
