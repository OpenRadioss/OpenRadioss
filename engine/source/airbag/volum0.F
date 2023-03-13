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
Chd|  VOLUM0                        source/airbag/volum0.F        
Chd|-- called by -----------
Chd|        MONVOL0                       source/airbag/monvol0.F       
Chd|-- calls ---------------
Chd|        SPMD_EXCH_FR6                 source/mpi/kinematic_conditions/spmd_exch_fr6.F
Chd|        SUM_6_FLOAT                   source/system/parit.F         
Chd|        MONVOL_STRUCT_MOD             share/modules/monvol_struct_mod.F
Chd|====================================================================
      SUBROUTINE VOLUM0(IVOLU   ,RVOLU ,VOL  ,X  ,SURF_NODES,
     2                  N       ,NN    ,SURF_ELTYP,SURF_ELEM,
     3                  ICONTACT,PORO  ,FR_MV, T_MONVOLN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MONVOL_STRUCT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IVOLU(*), ICONTACT(*), NN, FR_MV(*),
     . SURF_NODES(NN,4),SURF_ELTYP(NN),SURF_ELEM(NN)
C     REAL
      my_real
     .   X(3,*), N(3,*),RVOLU(*),PORO(*),VOL
      TYPE(MONVOL_STRUCT_), INTENT(IN) :: T_MONVOLN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,IS,K,NOD1,NOD2,NOD3,NOD4
      my_real
     .   AREA,XX,YY,ZZ,X12,Y12,Z12,X13,Y13,Z13,X24,Y24,Z24,DIR,
     .     NX, NY, NZ
      my_real, DIMENSION(:), ALLOCATABLE :: F1, F2
      DOUBLE PRECISION 
     .   FRMV6(2,6)
C--------------------------------------------------------------
C CALCUL DU VOLUME (variable locale dans MONVOL0)
C CALCUL DE LA SURFACE (stockee dans RVOLU(18))
C PORO(*) : PORO=0 aucun noeud en contact
C           PORO=1 tous les noeuds en contact ...
C--------------------------------------------------------------

      ALLOCATE(F1(NN + T_MONVOLN%NB_FILL_TRI), F2(NN + T_MONVOLN%NB_FILL_TRI))
      IF(INTBAG==0)THEN
        DO I=1,NN
          II=SURF_ELEM(I)
          IF(SURF_ELTYP(I)==7)THEN
            II=II+NUMELC
          ELSEIF(SURF_ELTYP(I)/=3)THEN
            II=I+NUMELC+NUMELTG
          ENDIF
          NOD1 = SURF_NODES(I,1)
          NOD2 = SURF_NODES(I,2)
          NOD3 = SURF_NODES(I,3)
          NOD4 = SURF_NODES(I,4)
          XX=HALF*(X(1,NOD1)+X(1,NOD2))
          YY=HALF*(X(2,NOD1)+X(2,NOD2))
          ZZ=HALF*(X(3,NOD1)+X(3,NOD2)) 
          X13=X(1,NOD3)-X(1,NOD1)
          Y13=X(2,NOD3)-X(2,NOD1)
          Z13=X(3,NOD3)-X(3,NOD1)
          X24=X(1,NOD4)-X(1,NOD2)
          Y24=X(2,NOD4)-X(2,NOD2)
          Z24=X(3,NOD4)-X(3,NOD2)
          N(1,II)=HALF*(Y13*Z24-Y24*Z13)
          N(2,II)=HALF*(Z13*X24-Z24*X13)
          N(3,II)=HALF*(X13*Y24-X24*Y13)
          F1(I) = SQRT( N(1,II)**2+N(2,II)**2+N(3,II)**2 )
          F2(I) = THIRD*( N(1,II)*XX+N(2,II)*YY+N(3,II)*ZZ )
        ENDDO
      ELSE
        DO I=1,NN
          II=SURF_ELEM(I)
          NOD1 = SURF_NODES(I,1)
          NOD2 = SURF_NODES(I,2)
          NOD3 = SURF_NODES(I,3)
          NOD4 = SURF_NODES(I,4)
          IF(SURF_ELTYP(I)==3)THEN
            PORO(II)=ZERO
            IF(ICONTACT(NOD1)/=0)PORO(II)=PORO(II)+FOURTH
            IF(ICONTACT(NOD2)/=0)PORO(II)=PORO(II)+FOURTH
            IF(ICONTACT(NOD3)/=0)PORO(II)=PORO(II)+FOURTH
            IF(ICONTACT(NOD4)/=0)PORO(II)=PORO(II)+FOURTH
          ELSEIF(SURF_ELTYP(I)==7)THEN
            II=II+NUMELC
            PORO(II)=ZERO
            IF(ICONTACT(NOD1)/=0)PORO(II)=PORO(II)+THIRD
            IF(ICONTACT(NOD2)/=0)PORO(II)=PORO(II)+THIRD
            IF(ICONTACT(NOD3)/=0)PORO(II)=PORO(II)+THIRD
          ELSE
            II=I+NUMELC+NUMELTG
            PORO(II)=ZERO
            IF(ICONTACT(NOD1)/=0)PORO(II)=PORO(II)+FOURTH
            IF(ICONTACT(NOD2)/=0)PORO(II)=PORO(II)+FOURTH
            IF(ICONTACT(NOD3)/=0)PORO(II)=PORO(II)+FOURTH
            IF(ICONTACT(NOD4)/=0)PORO(II)=PORO(II)+FOURTH    
          ENDIF
          XX=HALF*(X(1,NOD1)+X(1,NOD2))
          YY=HALF*(X(2,NOD1)+X(2,NOD2))
          ZZ=HALF*(X(3,NOD1)+X(3,NOD2))
          X13=X(1,NOD3)-X(1,NOD1)
          Y13=X(2,NOD3)-X(2,NOD1)
          Z13=X(3,NOD3)-X(3,NOD1)
          X24=X(1,NOD4)-X(1,NOD2)
          Y24=X(2,NOD4)-X(2,NOD2)
          Z24=X(3,NOD4)-X(3,NOD2)
          N(1,II)=HALF*(Y13*Z24-Y24*Z13)
          N(2,II)=HALF*(Z13*X24-Z24*X13)
          N(3,II)=HALF*(X13*Y24-X24*Y13)
          F1(I) = SQRT( N(1,II)**2+N(2,II)**2+N(3,II)**2 )
          F2(I) = THIRD*( N(1,II)*XX+N(2,II)*YY+N(3,II)*ZZ )
        ENDDO
      ENDIF
      IF (ISPMD + 1 == FR_MV(NSPMD + 2)) THEN
         DO II = 1, T_MONVOLN%NB_FILL_TRI
            NOD1 = T_MONVOLN%FILL_TRI(3 * (II - 1) + 1)
            NOD2 = T_MONVOLN%FILL_TRI(3 * (II - 1) + 2)
            NOD3 = T_MONVOLN%FILL_TRI(3 * (II - 1) + 3)
            NOD4 = NOD3
            XX=HALF*(X(1,NOD1)+X(1,NOD2))
            YY=HALF*(X(2,NOD1)+X(2,NOD2))
            ZZ=HALF*(X(3,NOD1)+X(3,NOD2)) 
            X13=X(1,NOD3)-X(1,NOD1)
            Y13=X(2,NOD3)-X(2,NOD1)
            Z13=X(3,NOD3)-X(3,NOD1)
            X24=X(1,NOD4)-X(1,NOD2)
            Y24=X(2,NOD4)-X(2,NOD2)
            Z24=X(3,NOD4)-X(3,NOD2)
            NX=HALF*(Y13*Z24-Y24*Z13)
            NY=HALF*(Z13*X24-Z24*X13)
            NZ=HALF*(X13*Y24-X24*Y13)
            F1(NN + II) = SQRT( NX**2+NY**2+NZ**2 )
            F2(NN + II) = THIRD*( NX*XX+NY*YY+NZ*ZZ )
         ENDDO
      ELSE
         DO II = 1, T_MONVOLN%NB_FILL_TRI
            F1(NN + II) = ZERO                         
            F2(NN + II) = ZERO                        
         ENDDO
      ENDIF
      DO K = 1, 6
        FRMV6(1,K) = ZERO
        FRMV6(2,K) = ZERO
      END DO
      CALL SUM_6_FLOAT(1, NN + T_MONVOLN%NB_FILL_TRI, F1, FRMV6(1,1), 2)
      CALL SUM_6_FLOAT(1, NN + T_MONVOLN%NB_FILL_TRI, F2, FRMV6(2,1), 2)
C comm si necessaire
      IF(NSPMD > 1) THEN
        CALL SPMD_EXCH_FR6(FR_MV,FRMV6,2*6)
      ENDIF
C
      AREA = FRMV6(1,1)+FRMV6(1,2)+FRMV6(1,3)+
     .       FRMV6(1,4)+FRMV6(1,5)+FRMV6(1,6)
      VOL  = FRMV6(2,1)+FRMV6(2,2)+FRMV6(2,3)+
     .       FRMV6(2,4)+FRMV6(2,5)+FRMV6(2,6)
C
      RVOLU(18) = AREA
C
      RETURN
      END
