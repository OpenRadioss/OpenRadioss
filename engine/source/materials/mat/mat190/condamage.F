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
c-----------------------------------------------
Chd|====================================================================
Chd|  CONDAMAGE_MOD                 source/materials/mat/mat190/condamage.F
Chd|-- called by -----------
Chd|        SIGEPS190                     source/materials/mat/mat190/sigeps190.F
Chd|-- calls ---------------
Chd|====================================================================
       MODULE CONDAMAGE_MOD
       CONTAINS 
Chd|====================================================================
Chd|  CONDAMAGE                     source/materials/mat/mat190/condamage.F
Chd|-- called by -----------
Chd|        SIGEPS190                     source/materials/mat/mat190/sigeps190.F
Chd|-- calls ---------------
Chd|        TABLE_MAT_VINTERP             source/materials/tools/table_mat_vinterp.F
Chd|        TABLE4D_MOD                   ../common_source/modules/table4d_mod.F
Chd|        TABLE_MAT_VINTERP_MOD         source/materials/tools/table_mat_vinterp.F
Chd|====================================================================
        SUBROUTINE CONDAMAGE 
     .         (ZXX,ZYY,ZZZ,ZXY,ZZX,ZYZ,
     .         DAMAGE,NEL,HU,SHAPE,WHYSMAX,
     .         NUMTABL ,TABLE,  NVARTMP, VARTMP)
c-----------------------------------------------
c     computation of damage for hsyteretic unloading
c      computes the damage for unloading at t(n+1)
c      ( new GL strains are used )
c-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
       USE TABLE4D_MOD
      USE TABLE_MAT_VINTERP_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "impl1_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C      A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(IN) :: NEL ,NUMTABL,NVARTMP
        my_real ,INTENT(IN) ::  HU, SHAPE  
        my_real, INTENT(IN) ,DIMENSION(NEL):: ZXX ,ZYY ,ZZZ ,ZXY ,ZZX ,ZYZ 
        TYPE (TABLE_4D_), DIMENSION(NUMTABL) ,TARGET  ::  TABLE
        my_real, INTENT(INOUT) ,DIMENSION(NEL) :: WHYSMAX
        my_real, INTENT(OUT)   ,DIMENSION(NEL) :: DAMAGE
        INTEGER ,DIMENSION(NEL,NVARTMP) ,INTENT(INOUT) :: VARTMP
c-----------------------------------------------
        INTEGER I,L
        my_real,DIMENSION(NEL) ::  ALAMBDA1,ALAMBDA2,ALAMBDA3,Z1,Z2,Z3,
     .                           FENER1,DENER1,FENER2,DENER2,FENER3,DENER3
        my_real
     .            RATIO, WHYS(NEL),EPSILON(NEL,3)
          
        my_real 
     .     AH1(NEL),AH2(NEL),AH3(NEL),
     .     AT2(NEL),AT3(NEL),AA3(NEL),AA2(NEL)
        INTEGER  IPOS(NEL,1)
        my_real, DIMENSION(NEL,1)   :: XVEC
        TYPE(TABLE_4D_), POINTER :: FUNC_ENER , FUNC_SIG
c-----------------------------------------------
c-----------------------------------------------
      FUNC_SIG   => TABLE(1)
      FUNC_ENER  => TABLE(2)

      DO I=1,NEL      
c       principal strains
c       note that these are not the same principal strains
c       that were computed in subroutine conversion since
c       we are now 1/2 timestep further
c
c       compute H1, H2 and H3
c
        AH1(I)=ZXX(I)+ZYY(I)+ZZZ(I)
        AH2(I)=ZYY(I)*ZZZ(I)+ZZZ(I)*ZXX(I)+ZXX(I)*ZYY(I)
     .        -ZYZ(I)**2-ZZX(I)**2-ZXY(I)**2
        AH3(I)=ZXX(I)*ZYY(I)*ZZZ(I)+TWO*ZXY(I)*ZYZ(I)*ZZX(I)
     .        -ZXX(I)*ZYZ(I)**2-ZYY(I)*ZZX(I)**2-ZZZ(I)*ZXY(I)**2
C
C       COMPUTE T2 AND T3
C
        AT3(I)=TWO*AH1(I)**3/TWENTY7 - AH1(I)*AH2(I)/THREE + AH3(I)
        AT2(I)=-AH2(I)+AH1(I)**2/THREE
        AT2(I)=MAX(AT2(I),1.E-16)
C
C       COMPUTE COS(A)
C
        AA3(I)=AT3(I)/TWO/SQRT(AT2(I)**3/TWENTY7)
        AA3(I)=MIN(ONE,AA3(I)) 
        AA3(I)=MAX(-ONE,AA3(I)) 
        AA2(I)=SQRT(AT2(I)/THREE)
C
C       COMPUTE PRINCIPAL STRAINS
C        
        Z1(I)=TWO*AA2(I)*COS(THIRD*ACOS(AA3(I))) 
        Z2(I)=TWO*AA2(I)*COS(THIRD*ACOS(AA3(I)) - TWO*PI/THREE) 
        Z3(I)=TWO*AA2(I)*COS(THIRD*ACOS(AA3(I)) - FOUR*PI/THREE) 
C
C       ADD HYDROSTATIC PART 
C
        Z1(I)=Z1(I)+AH1(I)/THREE 
        Z2(I)=Z2(I)+AH1(I)/THREE
        Z3(I)=Z3(I)+AH1(I)/THREE  
C
      ENDDO
C
C     UPDATE IN THE PRINCIPAL SYSTEM AT T(N+1)
C
      DO I=1,NEL
C
C       PRINCIPAL STRETCHES FROM PRINCIPAL GL STRAINS
C
        ALAMBDA1(I)=SQRT(TWO*Z1(I) + ONE)
        ALAMBDA2(I)=SQRT(TWO*Z2(I) + ONE)
        ALAMBDA3(I)=SQRT(TWO*Z3(I) + ONE)
C
C       PRINCIPAL ENGINEERING STRAIN POSITIVE IN COMPRESSION
C
        EPSILON(I,1) = ONE - ALAMBDA1(I)
        EPSILON(I,2) = ONE - ALAMBDA2(I)
        EPSILON(I,3) = ONE - ALAMBDA3(I)
      ENDDO !I=1,NEL
C
C     INTERPOLATE ENERGIES FOR HYSTERESIS MODEL
C
      XVEC(1:NEL,1)   = EPSILON(1:NEL,1)
      IPOS(1:NEL,1)   = VARTMP(1:NEL,16)
      CALL TABLE_MAT_VINTERP(FUNC_ENER,NEL,NEL,IPOS,XVEC,FENER1,DENER1)
      VARTMP(1:NEL,16) = IPOS(1:NEL,1)     

      XVEC(1:NEL,1)   = EPSILON(1:NEL,2)
      IPOS(1:NEL,1)   = VARTMP(1:NEL,17)
      CALL TABLE_MAT_VINTERP(FUNC_ENER,NEL,NEL,IPOS,XVEC,FENER2,DENER2)
      VARTMP(1:NEL,17) = IPOS(1:NEL,1)     

      XVEC(1:NEL,1)   = EPSILON(1:NEL,3)
      IPOS(1:NEL,1)   = VARTMP(1:NEL,18)
      CALL TABLE_MAT_VINTERP(FUNC_ENER,NEL,NEL,IPOS,XVEC,FENER3,DENER3)
      VARTMP(1:NEL,18) = IPOS(1:NEL,1)
C
C     COMPUTE DAMAGE FOR UNLOADING
C
      DO I=1,NEL
        WHYS(I)   = FENER1(I) + FENER2(I) + FENER3(I)
        WHYSMAX(I)= MAX(WHYSMAX(I),WHYS(I))
        RATIO     = WHYS(I) / WHYSMAX(I)
        DAMAGE(I) =(ONE-HU)*(ONE - RATIO**SHAPE)
      ENDDO
C      
      RETURN
      END
      END MODULE
