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
Chd|  ALEW1                         source/ale/alew1.F            
Chd|-- called by -----------
Chd|        ALEWDX                        source/ale/alewdx.F           
Chd|-- calls ---------------
Chd|        SPMD_XVOIS                    source/mpi/fluid/spmd_cfd.F   
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|====================================================================
      SUBROUTINE ALEW1( 
     1   X         ,D       ,V       ,W       ,
     2   ALE_NN_CONNECT     ,NALE    ,NODFT   ,NODLT   ,ITASK   ,
     3   NBRCVOIS  ,NBSDVOIS,LNRCVOIS,LNSDVOIS,ITAB         )
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C Compute Grid for /ALE/GRID/DISP
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NALE(*), NODFT, NODLT, ITASK,
     .        NBRCVOIS(*),NBSDVOIS(*),
     .        LNRCVOIS(*),LNSDVOIS(*),ITAB(*)
      my_real
     .        X(3,*), D(3,*), V(3,*), W(3,*)     
      TYPE(t_connectivity), INTENT(IN) :: ALE_NN_CONNECT
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com07_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, IA1, IA2, IA3, IA4, IA5, IA6, L, LENCOM
      my_real
     .     DX, DY, DZ,FAC
      INTEGER :: IAD, IAD1, IAD2, NODE_ID
      my_real :: DX_MAX, DX_MIN
      my_real :: DY_MAX, DY_MIN
      my_real :: DZ_MAX, DZ_MIN
C-----------------------------------------------
C-----------------------------------------------
C  SPMD : COMM D
C-----------------------------------------------
      IF(NSPMD.GT.1)THEN
C
C Partie non parallele smt
C
!$OMP SINGLE

         LENCOM = NBRCVOIS(NSPMD+1)+NBSDVOIS(NSPMD+1)
         CALL SPMD_XVOIS(
     1        D     ,NBRCVOIS,NBSDVOIS,LNRCVOIS,LNSDVOIS,
     2        LENCOM)

C Fin Partie non parallele smt
!$OMP END SINGLE

      END IF
      FAC=UN/DT2/TROIS
      DO I = NODFT, NODLT
         IF(IABS(NALE(I)).EQ.1) THEN
            IAD1 = ALE_NN_CONNECT%IAD_CONNECT(I)
            IAD2 = ALE_NN_CONNECT%IAD_CONNECT(I + 1) - 1
            DX_MAX = -HUGE(ZERO)
            DX_MIN = HUGE(ZERO)
            DY_MAX = -HUGE(ZERO)
            DY_MIN = HUGE(ZERO)
            DZ_MAX = -HUGE(ZERO)
            DZ_MIN = HUGE(ZERO)
            DO IAD = IAD1, IAD2
               NODE_ID = ALE_NN_CONNECT%CONNECTED(IAD)
               DX_MAX = MAX(DX_MAX, D(1, NODE_ID))
               DX_MIN = MIN(DX_MIN, D(1, NODE_ID))
               DY_MAX = MAX(DY_MAX, D(2, NODE_ID))
               DY_MIN = MIN(DY_MIN, D(2, NODE_ID))
               DZ_MAX = MAX(DZ_MAX, D(3, NODE_ID))
               DZ_MIN = MIN(DZ_MIN, D(3, NODE_ID))
            ENDDO
            DX = FAC * (DX_MAX + DX_MIN - DEUX * D(1, I))
            DY = FAC * (DY_MAX + DY_MIN - DEUX * D(2, I))
            DZ = FAC * (DZ_MAX + DZ_MIN - DEUX * D(3, I))

            DX=VGX*DX
            DY=VGY*DY
            DZ=VGZ*DZ
            W(1,I)=SIGN(UN,DX)*MIN(ABS(DX),ALPHA)
            W(2,I)=SIGN(UN,DY)*MIN(ABS(DY),ALPHA)
            W(3,I)=SIGN(UN,DZ)*MIN(ABS(DZ),ALPHA)
         ELSEIF(NALE(I).EQ.0)THEN
            W(1,I)=V(1,I)
            W(2,I)=V(2,I)
            W(3,I)=V(3,I)
         ELSE
            W(1,I)=ZERO
            W(2,I)=ZERO
            W(3,I)=ZERO     
         ENDIF
C     
      ENDDO
C     
      RETURN
      END
