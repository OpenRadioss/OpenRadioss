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
Chd|  IGE3DAIRE                     source/elements/ige3d/ig3daire.F
Chd|-- called by -----------
Chd|        IG3DUFORC3                    source/elements/ige3d/ig3duforc3.F
Chd|-- calls ---------------
Chd|        DERSBASISFUNS                 source/elements/ige3d/dersbasisfuns.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE IGE3DAIRE(ITEL , N     , XXI   , YYI   , ZZI ,
     1                     WWI  , IDX   , IDY   , IDZ   , AIRE, 
     2                     NCTRL, GAUSSX, GAUSSY, GAUSSZ, KX  ,
     3                     KY   , KZ    , PX    , PY    , PZ  ) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C--------------------------------------------------------------------------------------------------------
C Implicite Types
C--------------------------------------------------------------------------------------------------------
#include     "implicit_f.inc"
C--------------------------------------------------------------------------------------------------------
C Dummy Arguments 
C--------------------------------------------------------------------------------------------------------
      INTEGER IDX, IDY, IDZ, NCTRL, PX, PY, PZ, ITEL, N
      my_real
     .  GAUSSX, GAUSSY, GAUSSZ
      my_real, 
     .  DIMENSION(*)  :: AIRE
      my_real,
     .  DIMENSION(*) :: XXI,YYI,ZZI,WWI
      my_real,
     .  DIMENSION(*) :: KX, KY, KZ
C--------------------------------------------------------------------------------------------------------
C Local Variables
C--------------------------------------------------------------------------------------------------------
      INTEGER NUMLOC, I, J, K, NA, NB, NC
      my_real,
     .  DIMENSION(NCTRL) :: R
      my_real,
     .  DIMENSION(NCTRL,3) :: DRDXI
      my_real
     .  SUMTOT
      my_real, 
     .  DIMENSION(PX+1) :: FN, DNDXI
      my_real, 
     .  DIMENSION(PY+1) :: FM, DMDXI
      my_real, 
     .  DIMENSION(PZ+1) :: FL, DLDXI
      my_real, 
     .  DIMENSION(3) :: XI, SUMXI
      my_real, 
     .  DIMENSION(3,3)  :: DXDXI, AJMAT, DXIDTILDEXI
C--------------------------------------------------------------------------------------------------------

C INITIALISATION DES DIFFERENTES MATRICES LOCALES


      DXDXI(:,:)=ZERO
      DXIDTILDEXI(:,:)=ZERO
      AJMAT(:,:)=ZERO


      XI(1) = ((KX(IDX+1)-KX(IDX))*GAUSSX + (KX(IDX+1)+(KX(IDX))))/DEUX
      XI(2) = ((KY(IDY+1)-KY(IDY))*GAUSSY + (KY(IDY+1)+(KY(IDY))))/DEUX
      XI(3) = ((KZ(IDZ+1)-KZ(IDZ))*GAUSSZ + (KZ(IDZ+1)+(KZ(IDZ))))/DEUX

C CALCULATE UNIVARIATE B-SPLINE FUNCTION AT XI POINT

      CALL DERSBASISFUNS(IDX, PX, XI(1), KX, FN, DNDXI)
      CALL DERSBASISFUNS(IDY, PY, XI(2), KY, FM, DMDXI)
      CALL DERSBASISFUNS(IDZ, PZ, XI(3), KZ, FL, DLDXI)

C BUILD NUMERATORS AND DENOMINATORS

      NUMLOC=0
      SUMTOT=ZERO
      SUMXI(1)=ZERO
      SUMXI(2)=ZERO
      SUMXI(3)=ZERO

C BOUCLE SUR TOUT LES POINTS DE CONTROLE 

      DO K = 0,PZ
        DO J = 0,PY
          DO I = 0,PX
            NUMLOC = NUMLOC+1
            R(NUMLOC)=FN(PX+1-I)*FM(PY+1-J)*FL(PZ+1-K)*WWI(NUMLOC)
            SUMTOT=SUMTOT+R(NUMLOC)
            DRDXI(NUMLOC,1)=DNDXI(PX+1-I)*FM(PY+1-J)*FL(PZ+1-K)*
     .                       WWI(NUMLOC)
            SUMXI(1)=SUMXI(1)+DRDXI(NUMLOC,1)
            DRDXI(NUMLOC,2)=FN(PX+1-I)*DMDXI(PY+1-J)*FL(PZ+1-K)*
     .                       WWI(NUMLOC)
            SUMXI(2)=SUMXI(2)+DRDXI(NUMLOC,2)
            DRDXI(NUMLOC,3)=FN(PX+1-I)*FM(PY+1-J)*DLDXI(PZ+1-K)*
     .                       WWI(NUMLOC)
            SUMXI(3)=SUMXI(3)+DRDXI(NUMLOC,3)
          ENDDO
        ENDDO
      ENDDO

C DIVIDE BY DENOMINATOR TO COMPLETE DEFINITION OF FUNCTION AND DERIVATIVES

      DO NUMLOC=1,NCTRL
        R(NUMLOC)=R(NUMLOC)/SUMTOT
      ENDDO

      DO I=1,3
        DO NUMLOC=1,NCTRL
          DRDXI(NUMLOC,I)=(DRDXI(NUMLOC,I)-R(NUMLOC)*SUMXI(I))/SUMTOT
        ENDDO
      ENDDO

C GRADIENT OF MAPPING FROM PARAMETER SPACE TO PHYSICAL SPACE 

      DO NUMLOC=1,NCTRL
        DO NB=1,3
          DXDXI(1,NB)=DXDXI(1,NB)+XXI(NUMLOC)*DRDXI(NUMLOC,NB)
          DXDXI(2,NB)=DXDXI(2,NB)+YYI(NUMLOC)*DRDXI(NUMLOC,NB)
          DXDXI(3,NB)=DXDXI(3,NB)+ZZI(NUMLOC)*DRDXI(NUMLOC,NB)
        ENDDO                             
      ENDDO

C GRADIENT OF MAPPING FROM PARENT ELEMENT TO PARAMETER SPACE

      DXIDTILDEXI(1,1)=(KX(IDX+1)-KX(IDX))/DEUX
      DXIDTILDEXI(2,2)=(KY(IDY+1)-KY(IDY))/DEUX
      DXIDTILDEXI(3,3)=(KZ(IDZ+1)-KZ(IDZ))/DEUX

      DO NA=1,3
        DO NB=1,3
          DO NC=1,3
            AJMAT(NA,NB)=AJMAT(NA,NB)+DXDXI(NA,NC)*DXIDTILDEXI(NC,NB)
          ENDDO
        ENDDO
      ENDDO

      AIRE(1) = SQRT(AJMAT(1,1)*AJMAT(1,1)+AJMAT(2,1)*AJMAT(2,1)
     .              +AJMAT(3,1)*AJMAT(3,1))
     .         *SQRT(AJMAT(1,2)*AJMAT(1,2)+AJMAT(2,2)*AJMAT(2,2)
     .              +AJMAT(3,2)*AJMAT(3,2)) 
      AIRE(2) = SQRT(AJMAT(1,1)*AJMAT(1,1)+AJMAT(2,1)*AJMAT(2,1)
     .              +AJMAT(3,1)*AJMAT(3,1))
     .         *SQRT(AJMAT(1,3)*AJMAT(1,3)+AJMAT(2,3)*AJMAT(2,3)
     .              +AJMAT(3,3)*AJMAT(3,3))
      AIRE(3) = SQRT(AJMAT(1,2)*AJMAT(1,2)+AJMAT(2,2)*AJMAT(2,2)
     .              +AJMAT(3,2)*AJMAT(3,2))
     .         *SQRT(AJMAT(1,3)*AJMAT(1,3)+AJMAT(2,3)*AJMAT(2,3)
     .              +AJMAT(3,3)*AJMAT(3,3)) 
    
      RETURN 
      END
