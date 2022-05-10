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
Chd|  EFLUX2                        source/ale/euler2d/eflux2.F   
Chd|-- called by -----------
Chd|        AFLUX0                        source/ale/aflux0.F           
Chd|-- calls ---------------
Chd|        AMULF2                        source/ale/bimat/amulf2.F     
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|====================================================================
      SUBROUTINE EFLUX2(PM,IXQ,V,X,
     .     FLUX,FLU1,FILL,DFILL,VOL,ALE_CONNECT,ALPH)
      USE ALE_CONNECTIVITY_MOD
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXQ(NIXQ,NUMELQ)
      my_real
     .   PM(NPROPM,NUMMAT), V(3,NUMNOD), X(3,NUMNOD), FLUX(4,*), FLU1(*), FILL(*),
     .   DFILL(*), VOL(*), ALPH(*)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER MAT(MVSIZ), NC1(MVSIZ), NC2(MVSIZ), NC3(MVSIZ), NC4(MVSIZ), I,II,J,M,IAD2
      my_real
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ), Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ), N1X(MVSIZ), N2X(MVSIZ), N3X(MVSIZ), N4X(MVSIZ),
     .   N1Y(MVSIZ), N2Y(MVSIZ), N3Y(MVSIZ), N4Y(MVSIZ), N1Z(MVSIZ), N2Z(MVSIZ), N3Z(MVSIZ), N4Z(MVSIZ),
     .   FLUX1(MVSIZ), FLUX2(MVSIZ), FLUX3(MVSIZ), FLUX4(MVSIZ), VX1(MVSIZ), VX2(MVSIZ), VX3(MVSIZ),
     .   VX4(MVSIZ), VY1(MVSIZ), VY2(MVSIZ), VY3(MVSIZ), VY4(MVSIZ), VZ1(MVSIZ), VZ2(MVSIZ), VZ3(MVSIZ),
     .   VZ4(MVSIZ), VDX1(MVSIZ), VDX2(MVSIZ), VDX3(MVSIZ), VDX4(MVSIZ), VDY1(MVSIZ), VDY2(MVSIZ),
     .   VDY3(MVSIZ), VDY4(MVSIZ), VDZ1(MVSIZ), VDZ2(MVSIZ), VDZ3(MVSIZ), VDZ4(MVSIZ), UPW(MVSIZ),
     .   REDUC,UPWL(4,MVSIZ)
C-----------------------------------------------      
      DO I=LFT,LLT
        II=I+NFT
        MAT(I)=IXQ(1,II)
        NC1(I)=IXQ(2,II)
        NC2(I)=IXQ(3,II)
        NC3(I)=IXQ(4,II)
        NC4(I)=IXQ(5,II)

        Y1(I)=X(2,NC1(I))
        Z1(I)=X(3,NC1(I))

        Y2(I)=X(2,NC2(I))
        Z2(I)=X(3,NC2(I))

        Y3(I)=X(2,NC3(I))
        Z3(I)=X(3,NC3(I))

        Y4(I)=X(2,NC4(I))
        Z4(I)=X(3,NC4(I))

        VDY1(I)=V(2,NC1(I))
        VDZ1(I)=V(3,NC1(I))

        VDY2(I)=V(2,NC2(I))
        VDZ2(I)=V(3,NC2(I))

        VDY3(I)=V(2,NC3(I))
        VDZ3(I)=V(3,NC3(I))

        VDY4(I)=V(2,NC4(I))
        VDZ4(I)=V(3,NC4(I))
      ENDDO
C-----------------------------------------------
C     CALCUL PAR FACE DE V MATIERE - V MAILLAGE
C-----------------------------------------------
      DO I=LFT,LLT
        VY1(I)=UNDEMI*(VDY1(I)+VDY2(I))
        VY2(I)=UNDEMI*(VDY2(I)+VDY3(I))
        VY3(I)=UNDEMI*(VDY3(I)+VDY4(I))
        VY4(I)=UNDEMI*(VDY4(I)+VDY1(I))      
        VZ1(I)=UNDEMI*(VDZ1(I)+VDZ2(I))
        VZ2(I)=UNDEMI*(VDZ2(I)+VDZ3(I))
        VZ3(I)=UNDEMI*(VDZ3(I)+VDZ4(I))
        VZ4(I)=UNDEMI*(VDZ4(I)+VDZ1(I))      
      ENDDO
C------------------------------------------
C     CALCUL DE LA NORMALE A CHAQUE FACE
C------------------------------------------
      DO I=LFT,LLT
        N1Y(I)= (Z2(I)-Z1(I))
        N1Z(I)=-(Y2(I)-Y1(I))

        N2Y(I)= (Z3(I)-Z2(I))
        N2Z(I)=-(Y3(I)-Y2(I))

        N3Y(I)= (Z4(I)-Z3(I))
        N3Z(I)=-(Y4(I)-Y3(I))

        N4Y(I)= (Z1(I)-Z4(I))
        N4Z(I)=-(Y1(I)-Y4(I))
      ENDDO
      IF(N2D.EQ.1)THEN
       DO I=LFT,LLT
         N1Y(I) = N1Y(I)*(Y1(I)+Y2(I))*UNDEMI
         N1Z(I) = N1Z(I)*(Y1(I)+Y2(I))*UNDEMI
         N2Y(I) = N2Y(I)*(Y2(I)+Y3(I))*UNDEMI
         N2Z(I) = N2Z(I)*(Y2(I)+Y3(I))*UNDEMI
         N3Y(I) = N3Y(I)*(Y3(I)+Y4(I))*UNDEMI
         N3Z(I) = N3Z(I)*(Y3(I)+Y4(I))*UNDEMI
         N4Y(I) = N4Y(I)*(Y1(I)+Y4(I))*UNDEMI
         N4Z(I) = N4Z(I)*(Y1(I)+Y4(I))*UNDEMI       
       ENDDO
      ENDIF
C--------------------
C     CALCUL DES FLUX
C--------------------
      DO I=LFT,LLT
        FLUX1(I)=(VY1(I)*N1Y(I)+VZ1(I)*N1Z(I))
        FLUX2(I)=(VY2(I)*N2Y(I)+VZ2(I)*N2Z(I))
        FLUX3(I)=(VY3(I)*N3Y(I)+VZ3(I)*N3Z(I))
        FLUX4(I)=(VY4(I)*N4Y(I)+VZ4(I)*N4Z(I))
      ENDDO
C-----------------------------------------------
C     TRIMATERIAU
C-----------------------------------------------
      IF(NINT(PM(19,MAT(1))).EQ.51)THEN
        DO I=LFT,LLT
          FLUX(1,I)=FLUX1(I)
          FLUX(2,I)=FLUX2(I)
          FLUX(3,I)=FLUX3(I)
          FLUX(4,I)=FLUX4(I)
        ENDDO
        RETURN
      ENDIF
C-----------------------------------------------
C     UPWIND
C-----------------------------------------------
      DO J=1,4
        DO I=LFT,LLT
          UPWL(J,I) = PM(16,MAT(I))
        ENDDO
      ENDDO
      
      !======================================================!
      !  BOUNDARY FACE : no volume flux by default           !
      !    slip wall bc                                      !
      !======================================================!
      DO I=LFT,LLT
       IAD2 = ALE_CONNECT%ee_connect%iad_connect(I + NFT)
       REDUC=PM(92,MAT(I))
       
       II=ALE_CONNECT%ee_connect%connected(IAD2 + 1 - 1)
       IF(II == 0)THEN
        FLUX1(I)=FLUX1(I)*REDUC
       ENDIF

       II=ALE_CONNECT%ee_connect%connected(IAD2 + 2 - 1)
       IF(II == 0)THEN
        FLUX2(I)=FLUX2(I)*REDUC
       ENDIF

       II=ALE_CONNECT%ee_connect%connected(IAD2 + 3 - 1)
       IF(II == 0)THEN
        FLUX3(I)=FLUX3(I)*REDUC
       ENDIF

       II=ALE_CONNECT%ee_connect%connected(IAD2 + 4 - 1)
       IF(II == 0)THEN
        FLUX4(I)=FLUX4(I)*REDUC
       ENDIF

      ENDDO
C-----------------------------------------------
C     MULTIMATERIAUX SURFACE MOUILLEE
C-----------------------------------------------
      IF(JMULT.NE.0)THEN
        DO I=LFT,LLT
          UPW(I) =PM(16,MAT(I))
        ENDDO
        CALL AMULF2(FILL,DFILL,FLUX,FLU1,VOL,ALE_CONNECT,ALPH,
     .     FLUX1, FLUX2, FLUX3, FLUX4, UPW, 
     .     NC1, NC2, NC3, NC4)
      ELSE
       DO I=LFT,LLT
         FLUX(1,I)= FLUX1(I) - UPWL(1,I) * ABS(FLUX1(I))
         FLUX(2,I)= FLUX2(I) - UPWL(2,I) * ABS(FLUX2(I))
         FLUX(3,I)= FLUX3(I) - UPWL(3,I) * ABS(FLUX3(I))
         FLUX(4,I)= FLUX4(I) - UPWL(4,I) * ABS(FLUX4(I))
         FLU1(I)  = FLUX1(I) + UPWL(1,I) * ABS(FLUX1(I))
     .            + FLUX2(I) + UPWL(2,I) * ABS(FLUX2(I))
     .            + FLUX3(I) + UPWL(3,I) * ABS(FLUX3(I))
     .            + FLUX4(I) + UPWL(4,I) * ABS(FLUX4(I))
       ENDDO
      ENDIF

      RETURN
      END
