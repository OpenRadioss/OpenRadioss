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
Chd|  MULTI_COMPUTEVOLUME           source/multifluid/multi_computevolume.F
Chd|-- called by -----------
Chd|        ALEMAIN                       source/ale/alemain.F          
Chd|        MULTI_TIMEEVOLUTION           source/multifluid/multi_timeevolution.F
Chd|-- calls ---------------
Chd|        QVOLU2                        source/elements/solid_2d/quad/qvolu2.F
Chd|        SDERI3                        source/elements/solid/solide/sderi3.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE MULTI_COMPUTEVOLUME(NEL, NG, IPARG, SYM, 
     .     ELBUF_TAB, IXS, IXQ, IXTG, VOLNEW, XGRID)
C-----------------------------------------------
C     D e s c r i p t i o n
C-----------------------------------------------
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
C-----------------------------------------------
C     I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C     C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"      
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "mvsiz_p.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C     D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NG, NEL, IPARG(NPARG, NGROUP) , SYM
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      INTEGER, INTENT(IN), TARGET :: IXS(NIXS, NUMELS), IXQ(NIXQ, NUMELQ), IXTG(NIXTG, NUMELTG)
      my_real, INTENT(INOUT) :: VOLNEW(NEL)
      my_real, INTENT(IN) :: XGRID(3, NUMNOD)
C-----------------------------------------------
C     L o c a l   V a r i a b l e s
C-----------------------------------------------
      TYPE(G_BUFEL_), POINTER :: GBUF
      INTEGER :: II, NGL(MVSIZ), ISOLNOD
      my_real :: X1(MVSIZ), Y1(MVSIZ), Z1(MVSIZ)
      my_real :: X2(MVSIZ), Y2(MVSIZ), Z2(MVSIZ)
      my_real :: X3(MVSIZ), Y3(MVSIZ), Z3(MVSIZ)
      my_real :: X4(MVSIZ), Y4(MVSIZ), Z4(MVSIZ)
      my_real :: X5(MVSIZ), Y5(MVSIZ), Z5(MVSIZ)
      my_real :: X6(MVSIZ), Y6(MVSIZ), Z6(MVSIZ)
      my_real :: X7(MVSIZ), Y7(MVSIZ), Z7(MVSIZ)
      my_real :: X8(MVSIZ), Y8(MVSIZ), Z8(MVSIZ)
      my_real :: JAC1(MVSIZ), JAC2(MVSIZ), JAC3(MVSIZ) 
      my_real :: JAC4(MVSIZ), JAC5(MVSIZ), JAC6(MVSIZ) 
      my_real :: B1, B2, B3, B4
      my_real :: C1, C2, C3, C4
      my_real :: D1, D2, D3, D4
      my_real :: X43, X41, X42, Y43, Y41, Y42, Z43, Z41, Z42
      my_real :: DUMMY(MVSIZ)
      INTEGER, DIMENSION(:,:), POINTER :: IX
      DOUBLE PRECISION :: VOLDP(MVSIZ)
C-----------------------------------------------------------     
      GBUF => ELBUF_TAB(NG)%GBUF
      JEUL = IPARG(11, NG)
      ISOLNOD = IPARG(28, NG)
      NFT = IPARG(3, NG)
      ITY = IPARG(5, NG)
      JHBE = IPARG(23,NG)
      ISMSTR = IPARG(9,NG)
      LFT = 1
      LLT = NEL
C
      IF (JEUL /= 0) THEN
C     Euler : volume does not change
         DO II = 1, NEL
            VOLNEW(II) = GBUF%VOL(II)
         ENDDO
      ELSE
C     ALE : compute new volumes
         IF (SYM == 0) THEN
C     =======
C     3D case
C     =======
            IX => IXS(1:NIXS, 1 + NFT:NEL + NFT)
            IF (ISOLNOD /= 4) THEN
               DO II = 1, NEL
C     Node 1
                  X1(II) = XGRID(1, IX(2, II))
                  Y1(II) = XGRID(2, IX(2, II))
                  Z1(II) = XGRID(3, IX(2, II))
C     Node 2
                  X2(II) = XGRID(1, IX(3, II))
                  Y2(II) = XGRID(2, IX(3, II))
                  Z2(II) = XGRID(3, IX(3, II))
C     Node 3
                  X3(II) = XGRID(1, IX(4, II))
                  Y3(II) = XGRID(2, IX(4, II))
                  Z3(II) = XGRID(3, IX(4, II))
C     Node 4
                  X4(II) = XGRID(1, IX(5, II))
                  Y4(II) = XGRID(2, IX(5, II))
                  Z4(II) = XGRID(3, IX(5, II))
C     Node 5
                  X5(II) = XGRID(1, IX(6, II))
                  Y5(II) = XGRID(2, IX(6, II))
                  Z5(II) = XGRID(3, IX(6, II))
C     Node 6
                  X6(II) = XGRID(1, IX(7, II))
                  Y6(II) = XGRID(2, IX(7, II))
                  Z6(II) = XGRID(3, IX(7, II))
C     Node 7
                  X7(II) = XGRID(1, IX(8, II))
                  Y7(II) = XGRID(2, IX(8, II))
                  Z7(II) = XGRID(3, IX(8, II))
C     Node 8
                  X8(II) = XGRID(1, IX(9, II))
                  Y8(II) = XGRID(2, IX(9, II))
                  Z8(II) = XGRID(3, IX(9, II))
                  NGL(II) = IX(NIXS, II)
               ENDDO
               CALL SDERI3(GBUF%OFF, VOLNEW, NGL,
     .              X1, X2, X3, X4, X5, X6, X7, X8,
     .              Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .              Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .              DUMMY, DUMMY, DUMMY, DUMMY,
     .              DUMMY, DUMMY, DUMMY, DUMMY,
     .              DUMMY, DUMMY, DUMMY, DUMMY,
     .              DUMMY, DUMMY, DUMMY, 
     .              DUMMY, DUMMY, DUMMY, 
     .              DUMMY, DUMMY, DUMMY, 
     .              DUMMY, DUMMY, DUMMY, 
     .              JAC1, JAC2, JAC3, 
     .              JAC4, JAC5, JAC6, 
     .              GBUF%SMSTR, GBUF%OFF, NEL, VOLDP, JHBE, ISMSTR)
            ELSE
               DO II = 1, NEL
C     Node 1
                  X1(II) = XGRID(1, IX(2, II))
                  Y1(II) = XGRID(2, IX(2, II))
                  Z1(II) = XGRID(3, IX(2, II))
C     Node 2
                  X2(II) = XGRID(1, IX(4, II))
                  Y2(II) = XGRID(2, IX(4, II))
                  Z2(II) = XGRID(3, IX(4, II))
C     Node 3
                  X3(II) = XGRID(1, IX(7, II))
                  Y3(II) = XGRID(2, IX(7, II))
                  Z3(II) = XGRID(3, IX(7, II))
C     Node 4
                  X4(II) = XGRID(1, IX(6, II))
                  Y4(II) = XGRID(2, IX(6, II))
                  Z4(II) = XGRID(3, IX(6, II))
                  NGL(II) = IX(NIXS, II)
               ENDDO
               DO II=1,NEL
                  X43 = X4(II) - X3(II)
                  Y43 = Y4(II) - Y3(II)
                  Z43 = Z4(II) - Z3(II)
                  X41 = X4(II) - X1(II)
                  Y41 = Y4(II) - Y1(II)
                  Z41 = Z4(II) - Z1(II)
                  X42 = X4(II) - X2(II)
                  Y42 = Y4(II) - Y2(II)
                  Z42 = Z4(II) - Z2(II)
C     
                  B1 =  Y43*Z42 - Y42*Z43
                  B2 =  Y41*Z43 - Y43*Z41
                  B3 =  Y42*Z41 - Y41*Z42
                  B4 =  -(B1 + B2 + B3)
C     
                  C1 =  Z43*X42 - Z42*X43
                  C2 =  Z41*X43 - Z43*X41
                  C3 =  Z42*X41 - Z41*X42
                  C4 =  -(C1 + C2 + C3)
C     
                  D1 =  X43*Y42 - X42*Y43
                  D2 =  X41*Y43 - X43*Y41
                  D3 =  X42*Y41 - X41*Y42
                  D4 =  -(D1 + D2 + D3)
C     
                  VOLNEW(II) = (X41*B1 + Y41*C1 + Z41*D1)*ONE_OVER_6
      ENDDO
            ENDIF
         ELSE
C     =======
C     2D case
C     =======
            IF (ITY == 2) THEN
C     QUADS
               IX => IXQ(1:NIXQ, 1 + NFT:NEL + NFT)
               DO II = 1, NEL
C     Node 1
                  Y1(II) = XGRID(2, IX(2, II))
                  Z1(II) = XGRID(3, IX(2, II))
C     Node 2
                  Y2(II) = XGRID(2, IX(3, II))
                  Z2(II) = XGRID(3, IX(3, II))
C     Node 3
                  Y3(II) = XGRID(2, IX(4, II))
                  Z3(II) = XGRID(3, IX(4, II))
C     Node 4
                  Y4(II) = XGRID(2, IX(5, II))
                  Z4(II) = XGRID(3, IX(5, II))
                  NGL(II) = IX(NIXS, II)
               ENDDO
               CALL QVOLU2(GBUF%OFF, GBUF%AREA, VOLNEW, NGL, 
     .              Y1, Y2, Y3, Y4, Z1, Z2, Z3, Z4, 
     .              DUMMY, DUMMY) 
            ELSEIF (ITY == 7) THEN
C     TRIANGLES
               IX => IXTG(1:NIXTG, 1 + NFT:NEL + NFT)
               IF (SYM == 2) THEN
                  DO II = 1, NEL
                     Y1(II) = XGRID(2, IX(1 + 1, II))
                     Z1(II) = XGRID(3, IX(1 + 1, II))
                     Y2(II) = XGRID(2, IX(1 + 2, II))
                     Z2(II) = XGRID(3, IX(1 + 2, II))
                     Y3(II) = XGRID(2, IX(1 + 3, II))
                     Z3(II) = XGRID(3, IX(1 + 3, II))
                     GBUF%AREA(II) = ABS(HALF * ((Y2(II) - Y1(II)) * (Z3(II) - Z1(II)) - 
     .                    (Z2(II) - Z1(II)) * (Y3(II) - Y1(II))))
                     VOLNEW(II) = GBUF%AREA(II)
                  ENDDO            
               ELSE IF (SYM == 1) THEN
C     Axisymmetric case
                  DO II = 1, NEL
                     Y1(II) = XGRID(2, IX(1 + 1, II))
                     Z1(II) = XGRID(3, IX(1 + 1, II))
                     Y2(II) = XGRID(2, IX(1 + 2, II))
                     Z2(II) = XGRID(3, IX(1 + 2, II))
                     Y3(II) = XGRID(2, IX(1 + 3, II))
                     Z3(II) = XGRID(3, IX(1 + 3, II))
                     GBUF%AREA(II) = ABS(HALF * ((Y2(II) - Y1(II)) * (Z3(II) - Z1(II)) - 
     .                    (Z2(II) - Z1(II)) * (Y3(II) - Y1(II))))
                     VOLNEW(II) = (Y1(II) + Y2(II) + Y3(II)) * (
     .                    Y1(II) * (Z2(II) - Z3(II)) + 
     .                    Y2(II) * (Z3(II) - Z1(II)) + 
     .                    Y3(II) * (Z1(II) - Z2(II))) * ONE_OVER_6
                  ENDDO            
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      
      END SUBROUTINE MULTI_COMPUTEVOLUME
