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
c===========================================================================
Chd|====================================================================
Chd|  CONVERSION_MOD                source/materials/mat/mat190/conversion.F
Chd|-- called by -----------
Chd|        SIGEPS190                     source/materials/mat/mat190/sigeps190.F
Chd|-- calls ---------------
Chd|====================================================================
       MODULE CONVERSION_MOD
       CONTAINS 
Chd|====================================================================
Chd|  CONVERSION                    source/materials/mat/mat190/conversion.F
Chd|-- called by -----------
Chd|        SIGEPS190                     source/materials/mat/mat190/sigeps190.F
Chd|-- calls ---------------
Chd|        TABLE_MAT_VINTERP             source/materials/tools/table_mat_vinterp.F
Chd|        TABLE4D_MOD                   ../common_source/modules/table4d_mod.F
Chd|        TABLE_MAT_VINTERP_MOD         source/materials/tools/table_mat_vinterp.F
Chd|====================================================================
       SUBROUTINE CONVERSION (ZXX,  ZYY,  ZZZ,   ZXY,  ZYZ,  ZZX,   
     .                      CIJKL,DFIRST,DRATE,  SCAL, NEL,  JAC, 
     .                      SLOPE ,NUMTABL ,TABLE,NVARTMP,VARTMP,
     .                      YLD,NDIM_TABLE,TOL1)
c           computation of coefficients to convert stiffness
c           matrix from local to global reference system
c===========================================================================
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
#include      "com01_c.inc"
c===========================================================================
C-----------------------------------------------
C   I N T P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL,  NUMTABL,NVARTMP,NDIM_TABLE
      my_real, INTENT(IN) :: TOL1,SCAL,ZXX(NEL),ZYY(NEL),ZZZ(NEL),ZXY(NEL),ZZX(NEL),ZYZ(NEL), JAC(NEL)
      INTEGER ,DIMENSION(NEL,NVARTMP) ,INTENT(INOUT) :: VARTMP
C-----------------------------------------------
C    I N T P U T   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real, INTENT(OUT) :: DFIRST(NEL,3,6),CIJKL(NEL,6,6), SLOPE(NEL,3),YLD(NEL)
      my_real, INTENT(INOUT) :: DRATE(NEL,3)
      my_real, EXTERNAL :: FINTER
      TYPE (TABLE_4D_), DIMENSION(NUMTABL) ,TARGET  ::  TABLE
c===========================================================================
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J,K,L
 
      my_real, DIMENSION(NEL)   :: DYDX1,VOLSTR,  
     .                             FSIG1,DSIG1,FSIG2,DSIG2,
     .                             SIGENER,DSIGENER,FSIG3,DSIG3
      my_real, DIMENSION(NEL,3) :: ALAMBDA,EPSILON,ENGIRATE, SIGMA, DYNSIGMA,
     .                              STRAINRATE 
      my_real   
     .   DIK(NEL,3), SIK(NEL,3), 
     .   AC,AS,Z1(NEL),Z2(NEL),Z3(NEL),DSECOND (NEL,3,6,6),
     .   AH1(NEL),AH2(NEL),AH3(NEL),J2(NEL),J3(NEL),AA3(NEL),AA2(NEL),AA4(NEL),
     .   DZERO(NEL,3),DAT2(NEL,6),DAT3(NEL,6),DA(NEL,6),DA2(NEL,6,6),DAT22(NEL,6,6),
     .   DAT32(NEL,6,6),DX2(NEL,6,6),
     .   DH1ZXX,DH1ZYY,DH1ZXY,DH1ZYZ,DH1ZZX,DH1ZZZ,
     .   DH2ZXX,DH2ZYY,DH2ZXY,DH2ZYZ,DH2ZZX,DH2ZZZ,
     .   DH3ZXX,DH3ZYY,DH3ZXY,DH3ZYZ,DH3ZZX,DH3ZZZ,
     .   DT2ZXX,DT2ZYY,DT2ZXY,DT2ZYZ,DT2ZZX,DT2ZZZ,
     .   DT3ZXX,DT3ZYY,DT3ZXY,DT3ZYZ,DT3ZZX,DT3ZZZ,
     .   TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,TEMP11,TEMP12,
     .   DZ1ZXX,DZ1ZYY,DZ1ZXY,DZ1ZYZ,DZ1ZZX,DZ1ZZZ,
     .   DZ2ZXX,DZ2ZYY,DZ2ZXY,DZ2ZYZ,DZ2ZZX,DZ2ZZZ,
     .   DZ3ZXX,DZ3ZYY,DZ3ZXY,DZ3ZYZ,DZ3ZZX,DZ3ZZZ,
     .   DA3ZXX,DA3ZYY,DA3ZXY,DA3ZYZ,DA3ZZX,DA3ZZZ,
     .   DAH2ZXXZXX,DAH2ZXXZYY,DAH2ZXXZZZ,DAH2ZXXZXY,DAH2ZXXZYZ,DAH2ZXXZZX,
     .   DAH2ZYYZXX,DAH2ZYYZYY,DAH2ZYYZZZ,DAH2ZYYZXY,DAH2ZYYZYZ,DAH2ZYYZZX,
     .   DAH2ZZZZXX,DAH2ZZZZYY,DAH2ZZZZZZ,DAH2ZZZZXY,DAH2ZZZZYZ,DAH2ZZZZZX,
     .   DAH2ZXYZXX,DAH2ZXYZYY,DAH2ZXYZZZ,DAH2ZXYZXY,DAH2ZXYZYZ,DAH2ZXYZZX,
     .   DAH2ZYZZXX,DAH2ZYZZYY,DAH2ZYZZZZ,DAH2ZYZZXY,DAH2ZYZZYZ,DAH2ZYZZZX,
     .   DAH2ZZXZXX,DAH2ZZXZYY,DAH2ZZXZZZ,DAH2ZZXZXY,DAH2ZZXZYZ,DAH2ZZXZZX,
     .   DAH3ZXXZXX,DAH3ZXXZYY,DAH3ZXXZZZ,DAH3ZXXZXY,DAH3ZXXZYZ,DAH3ZXXZZX,
     .   DAH3ZYYZXX,DAH3ZYYZYY,DAH3ZYYZZZ,DAH3ZYYZXY,DAH3ZYYZYZ,DAH3ZYYZZX,
     .   DAH3ZZZZXX,DAH3ZZZZYY,DAH3ZZZZZZ,DAH3ZZZZXY,DAH3ZZZZYZ,DAH3ZZZZZX,
     .   DAH3ZXYZXX,DAH3ZXYZYY,DAH3ZXYZZZ,DAH3ZXYZXY,DAH3ZXYZYZ,DAH3ZXYZZX,
     .   DAH3ZYZZXX,DAH3ZYZZYY,DAH3ZYZZZZ,DAH3ZYZZXY,DAH3ZYZZYZ,DAH3ZYZZZX,
     .   DAH3ZZXZXX,DAH3ZZXZYY,DAH3ZZXZZZ,DAH3ZZXZXY,DAH3ZZXZYZ,DAH3ZZXZZX,
     .   X1,X2,X22,X3,X4,X5,X6,
     .   T1,T2,T3,T4,T5,T6,
     .   AFAC1,AFAC2,AFAC3,AFAC4,AFAC33,AFAC5,
     .   AHELP3,AHELP33,AHELP,AHELP4,TETA,
     .   XCAP,DXCAP,THREE_TETA,TERMDAT32,TERM_SING,TERM_DERI_A,
     .   FAC1_DA2, FAC2_DA2,FAC3_DA2,T1DA2 , T2DA2 , T3DA2 ,  T4DA2, T5DA2 , T6DA2
      ! TABLE
      INTEGER  IPOS(NEL,3),IPOS2(NEL,2),IPOS1(NEL,1)
      my_real, DIMENSION(NEL,3)   :: XVEC
      my_real, DIMENSION(NEL,2)   :: XVEC2
      my_real, DIMENSION(NEL,1)   :: XVEC1
      TYPE(TABLE_4D_), POINTER ::  FUNC_SIG
C===========================================================================
C===========================================================================
      FUNC_SIG   => TABLE(1)
      

      DO I=1,NEL
C      
C       PART 1 : PRINCIPAL STRAINS
C  
C       COMPUTE H1, H2 AND H3
C
        AH1(I)=ZXX(I)+ZYY(I)+ZZZ(I)
        AH2(I)=ZYY(I)*ZZZ(I)+ZZZ(I)*ZXX(I)+ZXX(I)*ZYY(I)
     .          -ZYZ(I)**2  -ZZX(I)**2    -ZXY(I)**2
        AH3(I)=ZXX(I)*ZYY(I)*ZZZ(I)+TWO*ZXY(I)*ZYZ(I)*ZZX(I)
     .          -ZXX(I)*ZYZ(I)**2-ZYY(I)*ZZX(I)**2-ZZZ(I)*ZXY(I)**2
C
C       DERIVATIVES OF H1
C
        DH1ZXX=ONE
        DH1ZYY=ONE
        DH1ZZZ=ONE
        DH1ZXY=ZERO
        DH1ZYZ=ZERO
        DH1ZZX=ZERO
C
C       DERIVATIVES OF H2
C
        DH2ZXX=ZYY(I)+ZZZ(I)
        DH2ZYY=ZXX(I)+ZZZ(I)
        DH2ZZZ=ZYY(I)+ZXX(I)
        DH2ZXY=-TWO*ZXY(I)
        DH2ZYZ=-TWO*ZYZ(I)
        DH2ZZX=-TWO*ZZX(I)
C
C       DERIVATIVES OF H3
C
        DH3ZXX = ZYY(I)*ZZZ(I)-ZYZ(I)**2
        DH3ZYY = ZXX(I)*ZZZ(I)-ZZX(I)**2
        DH3ZZZ = ZYY(I)*ZXX(I)-ZXY(I)**2
        DH3ZXY = -TWO*ZXY(I)*ZZZ(I)+TWO*ZYZ(I)*ZZX(I)
        DH3ZYZ = -TWO*ZYZ(I)*ZXX(I)+TWO*ZXY(I)*ZZX(I)
        DH3ZZX = -TWO*ZZX(I)*ZYY(I)+TWO*ZYZ(I)*ZXY(I)
C
C       COMPUTE T2 AND T3
C
        J3(I) = TWO*AH1(I)**3/TWENTY7 - AH1(I)*AH2(I)*THIRD +  AH3(I)
        J2(I) = -AH2(I) + THIRD*AH1(I)**2 
        TERM_SING = FOUR * J2(I)**3 - TWENTY7 *J3(I) **2
        J2(I) = MAX(J2(I),1.E-16)
        ! J2 = -3*Q
        ! J3 = 2*R
        ! D   = Q**3 + R**2 must be negative or nul to have real solutions and define teta
C
C       COMPUTE COS(A)
C
        AA3(I)=J3(I)/TWO/SQRT(J2(I)**3/TWENTY7)
    
        AA3(I)=MIN( ONE,AA3(I)) 
        AA3(I)=MAX(-ONE,AA3(I)) 
        AA2(I)=SQRT(J2(I)*THIRD)
        THREE_TETA = ACOS(AA3(I))
C
C       COMPUTE PRINCIPAL STRAINS
C
        Z1(I)=TWO*AA2(I)*COS( THIRD * THREE_TETA  ) 
        Z2(I)=TWO*AA2(I)*COS( THIRD * THREE_TETA -TWO *PI*THIRD)  
        Z3(I)=TWO*AA2(I)*COS( THIRD * THREE_TETA -FOUR*PI*THIRD) 
C
C       ADD HYDROSTATIC PART
C
        Z1(I)=Z1(I)+ AH1(I)*THIRD
        Z2(I)=Z2(I)+ AH1(I)*THIRD
        Z3(I)=Z3(I)+ AH1(I)*THIRD  
C       PART 2
C       FIRST DERIVATIVES OF PRINCIPAL STRAINS
C
C       DERIVATIVES  OF  T2
C
        DT2ZXX = TWO*AH1(I)*THIRD-(ZYY(I)+ZZZ(I))
        DT2ZYY = TWO*AH1(I)*THIRD-(ZXX(I)+ZZZ(I))
        DT2ZZZ = TWO*AH1(I)*THIRD-(ZYY(I)+ZXX(I))
        DT2ZXY = TWO*ZXY(I)
        DT2ZYZ = TWO*ZYZ(I)
        DT2ZZX = TWO*ZZX(I)
C
C       STORE DERIVATIVES OF T2
C
        DAT2(I,1)=DT2ZXX
        DAT2(I,2)=DT2ZYY
        DAT2(I,3)=DT2ZZZ
        DAT2(I,4)=DT2ZXY
        DAT2(I,5)=DT2ZYZ
        DAT2(I,6)=DT2ZZX
C
C       DERIVATIVES OF T3
C    
        DT3ZXX = SIX * AH1(I)**2/TWENTY7 - AH2(I)*THIRD
     .                       - AH1(I)*(ZYY(I)+ZZZ(I))*THIRD
     .                       +(ZYY(I)*ZZZ(I)-ZYZ(I)**2)
        DT3ZYY = SIX * AH1(I)**2/TWENTY7-AH2(I)*THIRD
     .                       - AH1(I)*(ZXX(I)+ZZZ(I))*THIRD
     .                       +(ZXX(I)*ZZZ(I)-ZZX(I)**2)
        DT3ZZZ = SIX * AH1(I)**2/TWENTY7-AH2(I)*THIRD
     .                       - AH1(I)*(ZYY(I)+ZXX(I))*THIRD
     .                       +(ZYY(I)*ZXX(I)-ZXY(I)**2)
        DT3ZXY = TWO*AH1(I)*ZXY(I)*THIRD
     .           +TWO*(ZYZ(I)*ZZX(I)-ZXY(I)*ZZZ(I))
        DT3ZYZ = TWO*AH1(I)*ZYZ(I)*THIRD
     .           +TWO*(ZXY(I)*ZZX(I)-ZYZ(I)*ZXX(I))
        DT3ZZX = TWO*AH1(I)*ZZX(I)*THIRD
     .           +TWO*(ZYZ(I)*ZXY(I)-ZZX(I)*ZYY(I))
C
C       STORE DERIVATIVES OF T3
C
        DAT3(I,1)=DT3ZXX
        DAT3(I,2)=DT3ZYY
        DAT3(I,3)=DT3ZZZ
        DAT3(I,4)=DT3ZXY
        DAT3(I,5)=DT3ZYZ
        DAT3(I,6)=DT3ZZX
C
C       AUXILLARY COEFFICIENTS
C

C
C       'IF' TEST NEEDED TO GET CORRECT LIMIT VALUE
C       IN THE UNIAXIAL CASES
C       BIAXIAL CASES SEEM FINE WITHOUT THE TEST
C

C
C       DERIVATIVES OF A
C
        TERM_SING   = FOUR * J2(I)**3 - TWENTY7 *J3(I) **2        
        IF (TERM_SING<=EM20)THEN 
         TERM_DERI_A = ZERO
        ELSE
         TERM_DERI_A = -THREE* SQRT(THREE) / SQRT (TERM_SING )
        ENDIF

        DA3ZXX = TERM_DERI_A*(DT3ZXX- THREE_HALF *(J3(I)/ J2(I))*DT2ZXX) 
        DA3ZYY = TERM_DERI_A*(DT3ZYY- THREE_HALF *(J3(I)/ J2(I))*DT2ZYY) 
        DA3ZZZ = TERM_DERI_A*(DT3ZZZ- THREE_HALF *(J3(I)/ J2(I))*DT2ZZZ) 
        DA3ZXY = TERM_DERI_A*(DT3ZXY- THREE_HALF *(J3(I)/ J2(I))*DT2ZXY) 
        DA3ZYZ = TERM_DERI_A*(DT3ZYZ- THREE_HALF *(J3(I)/ J2(I))*DT2ZYZ) 
        DA3ZZX = TERM_DERI_A*(DT3ZZX- THREE_HALF *(J3(I)/ J2(I))*DT2ZZX) 

C
C       STORE DERIVATIVES OF A
C
        DA(I,1)=DA3ZXX
        DA(I,2)=DA3ZYY
        DA(I,3)=DA3ZZZ
        DA(I,4)=DA3ZXY
        DA(I,5)=DA3ZYZ
        DA(I,6)=DA3ZZX
C
        TEMP4 = ONE*THIRD/SQRT(J2(I)*THIRD)
        TEMP5 = TWO*SQRT(J2(I)*THIRD)*THIRD
        AA4(I)= ACOS(AA3(I))
        TETA  = THREE_TETA * THIRD
        AA4(I)= ACOS(AA3(I))
C
C       DERIVATIVES OF PRINCIPAL STRAINS
C
        DZ1ZXX =TEMP4*DT2ZXX*COS(-TETA)               +TEMP5*SIN(-TETA)*DA3ZXX                        
        DZ2ZXX =TEMP4*DT2ZXX*COS(-TETA+TWO*PI*THIRD)  +TEMP5*SIN(-TETA+TWO*PI*THIRD)*DA3ZXX                       
        DZ3ZXX =TEMP4*DT2ZXX*COS(-TETA+FOUR*PI*THIRD) +TEMP5*SIN(-TETA+FOUR*PI*THIRD)*DA3ZXX    

        DZ1ZYY =TEMP4*DT2ZYY*COS(-TETA)               +TEMP5*SIN(-TETA)*DA3ZYY                        
        DZ2ZYY =TEMP4*DT2ZYY*COS(-TETA+TWO*PI*THIRD)  +TEMP5*SIN(-TETA+TWO*PI*THIRD)*DA3ZYY                        
        DZ3ZYY =TEMP4*DT2ZYY*COS(-TETA+FOUR*PI*THIRD) +TEMP5*SIN(-TETA+FOUR*PI*THIRD)*DA3ZYY   

        DZ1ZZZ =TEMP4*DT2ZZZ*COS(-TETA)               +TEMP5*SIN(-TETA)*DA3ZZZ                        
        DZ2ZZZ =TEMP4*DT2ZZZ*COS(-TETA+TWO*PI*THIRD)  +TEMP5*SIN(-TETA+TWO*PI*THIRD)*DA3ZZZ                       
        DZ3ZZZ =TEMP4*DT2ZZZ*COS(-TETA+FOUR*PI*THIRD) +TEMP5*SIN(-TETA+FOUR*PI*THIRD)*DA3ZZZ    

        DZ1ZXY =TEMP4*DT2ZXY*COS(-TETA)               +TEMP5*SIN(-TETA)*DA3ZXY                        
        DZ2ZXY =TEMP4*DT2ZXY*COS(-TETA+TWO*PI*THIRD)  +TEMP5*SIN(-TETA+TWO*PI*THIRD)*DA3ZXY                        
        DZ3ZXY =TEMP4*DT2ZXY*COS(-TETA+FOUR*PI*THIRD) +TEMP5*SIN(-TETA+FOUR*PI*THIRD)*DA3ZXY   

        DZ1ZYZ =TEMP4*DT2ZYZ*COS(-TETA)               +TEMP5*SIN(-TETA)*DA3ZYZ                        
        DZ2ZYZ =TEMP4*DT2ZYZ*COS(-TETA+TWO*PI*THIRD)  +TEMP5*SIN(-TETA+TWO*PI*THIRD)*DA3ZYZ                        
        DZ3ZYZ =TEMP4*DT2ZYZ*COS(-TETA+FOUR*PI*THIRD) +TEMP5*SIN(-TETA+FOUR*PI*THIRD)*DA3ZYZ   

        DZ1ZZX =TEMP4*DT2ZZX*COS(-TETA)               +TEMP5*SIN(-TETA)*DA3ZZX                        
        DZ2ZZX =TEMP4*DT2ZZX*COS(-TETA+TWO*PI*THIRD)  +TEMP5*SIN(-TETA+TWO*PI*THIRD)*DA3ZZX                        
        DZ3ZZX =TEMP4*DT2ZZX*COS(-TETA+FOUR*PI*THIRD) +TEMP5*SIN(-TETA+FOUR*PI*THIRD)*DA3ZZX
c
c       store first derivatives
c       store zero'd derivatives
c
        DZERO(I,1) =Z1(I)
        DZERO(I,2) =Z2(I)
        DZERO(I,3) =Z3(I)
        DFIRST(I,1,1)=DZ1ZXX + THIRD
        DFIRST(I,1,2)=DZ1ZYY + THIRD
        DFIRST(I,1,3)=DZ1ZZZ + THIRD
        DFIRST(I,1,4)=DZ1ZXY
        DFIRST(I,1,5)=DZ1ZYZ
        DFIRST(I,1,6)=DZ1ZZX
        DFIRST(I,2,1)=DZ2ZXX + THIRD
        DFIRST(I,2,2)=DZ2ZYY + THIRD
        DFIRST(I,2,3)=DZ2ZZZ + THIRD
        DFIRST(I,2,4)=DZ2ZXY
        DFIRST(I,2,5)=DZ2ZYZ
        DFIRST(I,2,6)=DZ2ZZX
        DFIRST(I,3,1)=DZ3ZXX + THIRD
        DFIRST(I,3,2)=DZ3ZYY + THIRD
        DFIRST(I,3,3)=DZ3ZZZ + THIRD
        DFIRST(I,3,4)=DZ3ZXY
        DFIRST(I,3,5)=DZ3ZYZ
        DFIRST(I,3,6)=DZ3ZZX

C
C       PART 3
C       SECOND DERIVATIVES OF PRINCIPAL STRAINS
C       SECOND DERIVATIVES OF H1 ARE ALL ZERO
C       SECOND DERIVATIVES OF H2
C
        DAH2ZXXZXX = ZERO
        DAH2ZXXZYY = ONE
        DAH2ZXXZZZ = ONE
        DAH2ZXXZXY = ZERO
        DAH2ZXXZYZ = ZERO
        DAH2ZXXZZX = ZERO
C
        DAH2ZYYZXX = ONE
        DAH2ZYYZYY = ZERO
        DAH2ZYYZZZ = ONE
        DAH2ZYYZXY = ZERO
        DAH2ZYYZYZ = ZERO
        DAH2ZYYZZX = ZERO
C
        DAH2ZZZZXX = ONE
        DAH2ZZZZYY = ONE
        DAH2ZZZZZZ = ZERO
        DAH2ZZZZXY = ZERO
        DAH2ZZZZYZ = ZERO
        DAH2ZZZZZX = ZERO
C
        DAH2ZXYZXX = ZERO
        DAH2ZXYZYY = ZERO
        DAH2ZXYZZZ = ZERO
        DAH2ZXYZXY = -TWO
        DAH2ZXYZYZ = ZERO
        DAH2ZXYZZX = ZERO
C
        DAH2ZYZZXX = ZERO
        DAH2ZYZZYY = ZERO
        DAH2ZYZZZZ = ZERO
        DAH2ZYZZXY = ZERO
        DAH2ZYZZYZ = -TWO
        DAH2ZYZZZX = ZERO
C
        DAH2ZZXZXX = ZERO
        DAH2ZZXZYY = ZERO
        DAH2ZZXZZZ = ZERO
        DAH2ZZXZXY = ZERO
        DAH2ZZXZYZ = ZERO
        DAH2ZZXZZX = -TWO
C
C       SECOND DERIVATIVES OF H3
C
        DAH3ZXXZXX=ZERO
        DAH3ZXXZYY=ZZZ(I)
        DAH3ZXXZZZ=ZYY(I)
        DAH3ZXXZXY=ZERO
        DAH3ZXXZYZ=-TWO*ZYZ(I)
        DAH3ZXXZZX=ZERO
C
        DAH3ZYYZXX=ZZZ(I)
        DAH3ZYYZYY=ZERO
        DAH3ZYYZZZ=ZXX(I)
        DAH3ZYYZXY=ZERO
        DAH3ZYYZYZ=ZERO
        DAH3ZYYZZX=-TWO*ZZX(I)
C
        DAH3ZZZZXX=ZYY(I)
        DAH3ZZZZYY=ZXX(I)
        DAH3ZZZZZZ=ZERO
        DAH3ZZZZXY=-TWO*ZXY(I)
        DAH3ZZZZYZ=ZERO
        DAH3ZZZZZX=ZERO
C
        DAH3ZXYZXX=ZERO
        DAH3ZXYZYY=ZERO
        DAH3ZXYZZZ=-TWO*ZXY(I)
        DAH3ZXYZXY=-TWO*ZZZ(I)
        DAH3ZXYZYZ=TWO*ZZX(I)
        DAH3ZXYZZX=TWO*ZYZ(I)
C
        DAH3ZYZZXX=-TWO*ZYZ(I)
        DAH3ZYZZYY=ZERO
        DAH3ZYZZZZ=ZERO
        DAH3ZYZZXY=TWO*ZZX(I)
        DAH3ZYZZYZ=-TWO*ZXX(I)
        DAH3ZYZZZX=TWO*ZXY(I)
C
        DAH3ZZXZXX=ZERO
        DAH3ZZXZYY=-TWO*ZZX(I)
        DAH3ZZXZZZ=ZERO
        DAH3ZZXZXY=TWO*ZYZ(I)
        DAH3ZZXZYZ=TWO*ZXY(I)
        DAH3ZZXZZX=-TWO*ZYY(I)
C
C       SECOND DERIVATIVES OF T2
C
        DAT22(I,1,1)=-DAH2ZXXZXX+(TWO_THIRD)
        DAT22(I,1,2)=-DAH2ZXXZYY+(TWO_THIRD)
        DAT22(I,1,3)=-DAH2ZXXZZZ+(TWO_THIRD)
        DAT22(I,1,4)=-DAH2ZXXZXY
        DAT22(I,1,5)=-DAH2ZXXZYZ
        DAT22(I,1,6)=-DAH2ZXXZZX
C
        DAT22(I,2,1)=-DAH2ZYYZXX+(TWO_THIRD)
        DAT22(I,2,2)=-DAH2ZYYZYY+(TWO_THIRD)
        DAT22(I,2,3)=-DAH2ZYYZZZ+(TWO_THIRD)
        DAT22(I,2,4)=-DAH2ZYYZXY
        DAT22(I,2,5)=-DAH2ZYYZYZ
        DAT22(I,2,6)=-DAH2ZYYZZX
C
        DAT22(I,3,1)=-DAH2ZZZZXX+(TWO_THIRD)
        DAT22(I,3,2)=-DAH2ZZZZYY+(TWO_THIRD)
        DAT22(I,3,3)=-DAH2ZZZZZZ+(TWO_THIRD)
        DAT22(I,3,4)=-DAH2ZZZZXY
        DAT22(I,3,5)=-DAH2ZZZZYZ
        DAT22(I,3,6)=-DAH2ZZZZZX
C
        DAT22(I,4,1)=-DAH2ZXYZXX
        DAT22(I,4,2)=-DAH2ZXYZYY
        DAT22(I,4,3)=-DAH2ZXYZZZ
        DAT22(I,4,4)=-DAH2ZXYZXY
        DAT22(I,4,5)=-DAH2ZXYZYZ
        DAT22(I,4,6)=-DAH2ZXYZZX
C
        DAT22(I,5,1)=-DAH2ZYZZXX
        DAT22(I,5,2)=-DAH2ZYZZYY
        DAT22(I,5,3)=-DAH2ZYZZZZ
        DAT22(I,5,4)=-DAH2ZYZZXY
        DAT22(I,5,5)=-DAH2ZYZZYZ
        DAT22(I,5,6)=-DAH2ZYZZZX
C
        DAT22(I,6,1)=-DAH2ZZXZXX
        DAT22(I,6,2)=-DAH2ZZXZYY
        DAT22(I,6,3)=-DAH2ZZXZZZ
        DAT22(I,6,4)=-DAH2ZZXZXY
        DAT22(I,6,5)=-DAH2ZZXZYZ
        DAT22(I,6,6)=-DAH2ZZXZZX
C
C       SECOND DERIVATIVES OF T3
C
        DAT32(I,1,1) = DAH3ZXXZXX - (AH1(I)*THIRD)*DAH2ZXXZXX
        DAT32(I,1,2) = DAH3ZXXZYY - (AH1(I)*THIRD)*DAH2ZXXZYY
        DAT32(I,1,3) = DAH3ZXXZZZ - (AH1(I)*THIRD)*DAH2ZXXZZZ
        DAT32(I,1,4) = DAH3ZXXZXY - (AH1(I)*THIRD)*DAH2ZXXZXY
        DAT32(I,1,5) = DAH3ZXXZYZ - (AH1(I)*THIRD)*DAH2ZXXZYZ
        DAT32(I,1,6) = DAH3ZXXZZX - (AH1(I)*THIRD)*DAH2ZXXZZX
C
        DAT32(I,2,1) = DAH3ZYYZXX - (AH1(I)*THIRD)*DAH2ZYYZXX
        DAT32(I,2,2) = DAH3ZYYZYY - (AH1(I)*THIRD)*DAH2ZYYZYY
        DAT32(I,2,3) = DAH3ZYYZZZ - (AH1(I)*THIRD)*DAH2ZYYZZZ
        DAT32(I,2,4) = DAH3ZYYZXY - (AH1(I)*THIRD)*DAH2ZYYZXY
        DAT32(I,2,5) = DAH3ZYYZYZ - (AH1(I)*THIRD)*DAH2ZYYZYZ
        DAT32(I,2,6) = DAH3ZYYZZX - (AH1(I)*THIRD)*DAH2ZYYZZX
C
        DAT32(I,3,1) = DAH3ZZZZXX - (AH1(I)*THIRD)*DAH2ZZZZXX
        DAT32(I,3,2) = DAH3ZZZZYY - (AH1(I)*THIRD)*DAH2ZZZZYY
        DAT32(I,3,3) = DAH3ZZZZZZ - (AH1(I)*THIRD)*DAH2ZZZZZZ
        DAT32(I,3,4) = DAH3ZZZZXY - (AH1(I)*THIRD)*DAH2ZZZZXY
        DAT32(I,3,5) = DAH3ZZZZYZ - (AH1(I)*THIRD)*DAH2ZZZZYZ
        DAT32(I,3,6) = DAH3ZZZZZX - (AH1(I)*THIRD)*DAH2ZZZZZX
C
        DAT32(I,4,1) = DAH3ZXYZXX - (AH1(I)*THIRD)*DAH2ZXYZXX
        DAT32(I,4,2) = DAH3ZXYZYY - (AH1(I)*THIRD)*DAH2ZXYZYY
        DAT32(I,4,3) = DAH3ZXYZZZ - (AH1(I)*THIRD)*DAH2ZXYZZZ
        DAT32(I,4,4) = DAH3ZXYZXY - (AH1(I)*THIRD)*DAH2ZXYZXY
        DAT32(I,4,5) = DAH3ZXYZYZ - (AH1(I)*THIRD)*DAH2ZXYZYZ
        DAT32(I,4,6) = DAH3ZXYZZX - (AH1(I)*THIRD)*DAH2ZXYZZX
C
        DAT32(I,5,1) = DAH3ZYZZXX - (AH1(I)*THIRD)*DAH2ZYZZXX
        DAT32(I,5,2) = DAH3ZYZZYY - (AH1(I)*THIRD)*DAH2ZYZZYY
        DAT32(I,5,3) = DAH3ZYZZZZ - (AH1(I)*THIRD)*DAH2ZYZZZZ
        DAT32(I,5,4) = DAH3ZYZZXY - (AH1(I)*THIRD)*DAH2ZYZZXY
        DAT32(I,5,5) = DAH3ZYZZYZ - (AH1(I)*THIRD)*DAH2ZYZZYZ
        DAT32(I,5,6) = DAH3ZYZZZX - (AH1(I)*THIRD)*DAH2ZYZZZX

        DAT32(I,6,1) = DAH3ZZXZXX - (AH1(I)*THIRD)*DAH2ZZXZXX
        DAT32(I,6,2) = DAH3ZZXZYY - (AH1(I)*THIRD)*DAH2ZZXZYY
        DAT32(I,6,3) = DAH3ZZXZZZ - (AH1(I)*THIRD)*DAH2ZZXZZZ
        DAT32(I,6,4) = DAH3ZZXZXY - (AH1(I)*THIRD)*DAH2ZZXZXY
        DAT32(I,6,5) = DAH3ZZXZYZ - (AH1(I)*THIRD)*DAH2ZZXZYZ
        DAT32(I,6,6) = DAH3ZZXZZX - (AH1(I)*THIRD)*DAH2ZZXZZX
C
C       ADD FIRST DERIVATIVE TERMS
C
        TERMDAT32 = (TEN+TWO) * AH1(I)/TWENTY7
        DAT32(I,1,1)=DAT32(I,1,1)+TERMDAT32*DH1ZXX*DH1ZXX
     .                           -(THIRD)*DH1ZXX*DH2ZXX-(THIRD)*DH1ZXX*DH2ZXX
        DAT32(I,1,2)=DAT32(I,1,2)+TERMDAT32*DH1ZYY*DH1ZXX
     .                           -(THIRD)*DH1ZXX*DH2ZYY-(THIRD)*DH1ZYY*DH2ZXX
        DAT32(I,1,3)=DAT32(I,1,3)+TERMDAT32*DH1ZZZ*DH1ZXX
     .                           -(THIRD)*DH1ZXX*DH2ZZZ-(THIRD)*DH1ZZZ*DH2ZXX
        DAT32(I,1,4)=DAT32(I,1,4)+TERMDAT32*DH1ZXY*DH1ZXX
     .                           -(THIRD)*DH1ZXX*DH2ZXY-(THIRD)*DH1ZXY*DH2ZXX
        DAT32(I,1,5)=DAT32(I,1,5)+TERMDAT32*DH1ZYZ*DH1ZXX
     .                           -(THIRD)*DH1ZXX*DH2ZYZ-(THIRD)*DH1ZYZ*DH2ZXX
        DAT32(I,1,6)=DAT32(I,1,6)+TERMDAT32*DH1ZZX*DH1ZXX
     .                           -(THIRD)*DH1ZXX*DH2ZZX-(THIRD)*DH1ZZX*DH2ZXX
C
        DAT32(I,2,1)=DAT32(I,2,1)+TERMDAT32*DH1ZXX*DH1ZYY
     .                           -(THIRD)*DH1ZYY*DH2ZXX-(THIRD)*DH1ZXX*DH2ZYY
        DAT32(I,2,2)=DAT32(I,2,2)+TERMDAT32*DH1ZYY*DH1ZYY
     .                           -(THIRD)*DH1ZYY*DH2ZYY-(THIRD)*DH1ZYY*DH2ZYY
        DAT32(I,2,3)=DAT32(I,2,3)+TERMDAT32*DH1ZZZ*DH1ZYY
     .                           -(THIRD)*DH1ZYY*DH2ZZZ-(THIRD)*DH1ZZZ*DH2ZYY
        DAT32(I,2,4)=DAT32(I,2,4)+TERMDAT32*DH1ZXY*DH1ZYY
     .                           -(THIRD)*DH1ZYY*DH2ZXY-(THIRD)*DH1ZXY*DH2ZYY
        DAT32(I,2,5)=DAT32(I,2,5)+TERMDAT32*DH1ZYZ*DH1ZYY
     .                           -(THIRD)*DH1ZYY*DH2ZYZ-(THIRD)*DH1ZYZ*DH2ZYY
        DAT32(I,2,6)=DAT32(I,2,6)+TERMDAT32*DH1ZZX*DH1ZYY
     .                           -(THIRD)*DH1ZYY*DH2ZZX-(THIRD)*DH1ZZX*DH2ZYY
C
        DAT32(I,3,1)=DAT32(I,3,1)+TERMDAT32*DH1ZXX*DH1ZZZ
     .                           -(THIRD)*DH1ZZZ*DH2ZXX-(THIRD)*DH1ZXX*DH2ZZZ
        DAT32(I,3,2)=DAT32(I,3,2)+TERMDAT32*DH1ZYY*DH1ZZZ
     .                           -(THIRD)*DH1ZZZ*DH2ZYY-(THIRD)*DH1ZYY*DH2ZZZ
        DAT32(I,3,3)=DAT32(I,3,3)+TERMDAT32*DH1ZZZ*DH1ZZZ
     .                           -(THIRD)*DH1ZZZ*DH2ZZZ-(THIRD)*DH1ZZZ*DH2ZZZ
        DAT32(I,3,4)=DAT32(I,3,4)+TERMDAT32*DH1ZXY*DH1ZZZ
     .                           -(THIRD)*DH1ZZZ*DH2ZXY-(THIRD)*DH1ZXY*DH2ZZZ
        DAT32(I,3,5)=DAT32(I,3,5)+TERMDAT32*DH1ZYZ*DH1ZZZ
     .                           -(THIRD)*DH1ZZZ*DH2ZYZ-(THIRD)*DH1ZYZ*DH2ZZZ
        DAT32(I,3,6)=DAT32(I,3,6)+TERMDAT32*DH1ZZX*DH1ZZZ
     .                           -(THIRD)*DH1ZZZ*DH2ZZX-(THIRD)*DH1ZZX*DH2ZZZ
C
        DAT32(I,4,1)=DAT32(I,4,1)+TERMDAT32*DH1ZXX*DH1ZXY
     .                           -(THIRD)*DH1ZXY*DH2ZXX-(THIRD)*DH1ZXX*DH2ZXY
        DAT32(I,4,2)=DAT32(I,4,2)+TERMDAT32*DH1ZYY*DH1ZXY
     .                           -(THIRD)*DH1ZXY*DH2ZYY-(THIRD)*DH1ZYY*DH2ZXY
        DAT32(I,4,3)=DAT32(I,4,3)+TERMDAT32*DH1ZZZ*DH1ZXY
     .                           -(THIRD)*DH1ZXY*DH2ZZZ-(THIRD)*DH1ZZZ*DH2ZXY
        DAT32(I,4,4)=DAT32(I,4,4)+TERMDAT32*DH1ZXY*DH1ZXY
     .                           -(THIRD)*DH1ZXY*DH2ZXY-(THIRD)*DH1ZXY*DH2ZXY
        DAT32(I,4,5)=DAT32(I,4,5)+TERMDAT32*DH1ZYZ*DH1ZXY
     .                           -(THIRD)*DH1ZXY*DH2ZYZ-(THIRD)*DH1ZYZ*DH2ZXY
        DAT32(I,4,6)=DAT32(I,4,6)+TERMDAT32*DH1ZZX*DH1ZXY
     .                           -(THIRD)*DH1ZXY*DH2ZZX-(THIRD)*DH1ZZX*DH2ZXY
C
        DAT32(I,5,1)=DAT32(I,5,1)+TERMDAT32*DH1ZXX*DH1ZYZ
     .                           -(THIRD)*DH1ZYZ*DH2ZXX-(THIRD)*DH1ZXX*DH2ZYZ
        DAT32(I,5,2)=DAT32(I,5,2)+TERMDAT32*DH1ZYY*DH1ZYZ
     .                           -(THIRD)*DH1ZYZ*DH2ZYY-(THIRD)*DH1ZYY*DH2ZYZ
        DAT32(I,5,3)=DAT32(I,5,3)+TERMDAT32*DH1ZZZ*DH1ZYZ
     .                           -(THIRD)*DH1ZYZ*DH2ZZZ-(THIRD)*DH1ZZZ*DH2ZYZ
        DAT32(I,5,4)=DAT32(I,5,4)+TERMDAT32*DH1ZXY*DH1ZYZ
     .                           -(THIRD)*DH1ZYZ*DH2ZXY-(THIRD)*DH1ZXY*DH2ZYZ
        DAT32(I,5,5)=DAT32(I,5,5)+TERMDAT32*DH1ZYZ*DH1ZYZ
     .                           -(THIRD)*DH1ZYZ*DH2ZYZ-(THIRD)*DH1ZYZ*DH2ZYZ
        DAT32(I,5,6)=DAT32(I,5,6)+TERMDAT32*DH1ZZX*DH1ZYZ
     .                           -(THIRD)*DH1ZYZ*DH2ZZX-(THIRD)*DH1ZZX*DH2ZYZ
C
        DAT32(I,6,1)=DAT32(I,6,1)+TERMDAT32*DH1ZXX*DH1ZZX
     .                           -(THIRD)*DH1ZZX*DH2ZXX-(THIRD)*DH1ZXX*DH2ZZX
        DAT32(I,6,2)=DAT32(I,6,2)+TERMDAT32*DH1ZYY*DH1ZZX
     .                           -(THIRD)*DH1ZZX*DH2ZYY-(THIRD)*DH1ZYY*DH2ZZX
        DAT32(I,6,3)=DAT32(I,6,3)+TERMDAT32*DH1ZZZ*DH1ZZX
     .                           -(THIRD)*DH1ZZX*DH2ZZZ-(THIRD)*DH1ZZZ*DH2ZZX
        DAT32(I,6,4)=DAT32(I,6,4)+TERMDAT32*DH1ZXY*DH1ZZX
     .                           -(THIRD)*DH1ZZX*DH2ZXY-(THIRD)*DH1ZXY*DH2ZZX
        DAT32(I,6,5)=DAT32(I,6,5)+TERMDAT32*DH1ZYZ*DH1ZZX
     .                           -(THIRD)*DH1ZZX*DH2ZYZ-(THIRD)*DH1ZYZ*DH2ZZX
        DAT32(I,6,6)=DAT32(I,6,6)+TERMDAT32*DH1ZZX*DH1ZZX
     .                           -(THIRD)*DH1ZZX*DH2ZZX-(THIRD)*DH1ZZX*DH2ZZX
C
C       SECOND DERIVATIVES OF A
C
         IF (J2(I) <= TOL1 .OR.TERM_SING <= TOL1 )THEN    
            FAC1_DA2 = ZERO
            FAC2_DA2 = ZERO
            FAC3_DA2 = ZERO
         ELSE 
            FAC1_DA2 =  SQRT(TWENTY7) / (J2(I)**2 * SQRT(TERM_SING))
            FAC2_DA2 =  SQRT(TWENTY7) / (TWO  * (TERM_SING)**(3/2) )
            FAC3_DA2 =  FAC2_DA2 / J2(I) 
            
         ENDIF 
         DO J=1,6
           DO K=1,6
             
                T1DA2  =  -FAC1_DA2 *( J2(I)**2 * DAT32(I,J,K) -THREE_HALF *J2(I) * J3(I) * DAT22(I,J,K)  )

                T2DA2  =  -FAC2_DA2 * THREE*THREE * J2(I) * J3(I) * DAT2(I,K)*DAT2(I,J)
   
                T3DA2  =  -FAC1_DA2 *THREE_HALF*J3(I) *DAT2(I,K)*DAT2(I,J)
                
                T4DA2  =  -FAC2_DA2 * TWO * TWENTY7 * J3(I) * DAT3(I,J) *DAT3(I,K)
   
                T5DA2  =   FAC3_DA2 * THREE * TWENTY7 * J3(I)**2 * DAT3(I,K)*  DAT2(I,J) 
     .                  +FAC1_DA2 * THREE_HALF * J2(I) * DAT3(I,K)*  DAT2(I,J) 
   
                T6DA2  =   FAC2_DA2 *THREE * TWO * J2(I)**2 * DAT3(I,J) * DAT2(I,K)
   

             DA2(I,J,K) =  ( T1DA2 + T2DA2 + T3DA2 +  T4DA2 + T5DA2 + T6DA2)
            ENDDO
         ENDDO


C
C       SIMILAR ZEROING OUT AS IN THE FIRST DERIVATIVES
C       UNLIKE WITH THE FIRST DERIVATIVES, HERE IT MAKES A DIFFERENCE

C       SECOND DERIVATIVES OF PRINCIPAL STRAINS
C       MAKES USE OF FIRST DERIVATIVES OF T2 : DAT2
C       MAKES USE OF FIRST DERIVATIVES OF A : DA
C       MAKES USE OF SECOND DERIVATIVES OF T2 : DAT22
C       MAKES USE OF SECOND DERIVATIVES OF A : DA2
C

        DO  L=1,3
         AC    = COS(-TETA   +(L-1)*TWO*PI*THIRD)
         AS    = SIN(-TETA   +(L-1)*TWO*PI*THIRD) 
         AA2(I)= SQRT(J2(I)*THIRD)
         DO J=1,6
           DO K=1,6
             T1=(-ONE/SIX/AA2(I)**3*THIRD)* AC * DAT2(I,J)*DAT2(I,K)  
             T2=( THIRD/AA2(I))           * AC *     DAT22(I,J,K)
             T3=(ONE/NINE/AA2(I))         * AS * DAT2(I,K) * DA(I,J)
             T4=(ONE/NINE/AA2(I))         * AS * DAT2(I,J) * DA(I,K)
             T5=(-TWO/NINE) *AA2(I)       * AC * DA(I,J)   * DA(I,K) 
             T6=(TWO*THIRD) *AA2(I)       * AS * DA2(I,J,K) ! IF J2==ZERO T6=ZERO
             DSECOND(I,L,J,K) = T1 + T2 + T3 + T4 + T5 + T6
           ENDDO
         ENDDO
       ENDDO   
      ENDDO
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     END OF CALCULATION OF
C     PRINCIPAL STRAIN DERIVATIVES
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     COMPUTE STATIC PRINCIPAL 2PK STRESSES
C     COMPUTE CURRENT TANGENT TO STRESS-STRAIN CURVE
C     IN GENERAL : USE THE LOAD CURVE WITH LOWEST STRAIN RATE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   
C
      STRAINRATE(1:NEL,1)= ZERO
      STRAINRATE(1:NEL,2)= ZERO
      STRAINRATE(1:NEL,3)= ZERO
C
      DO I=1,NEL
C
c      VOLUMETRIC STRAIN FROM JACOBIAN
c      POSITIVE IN COMPRESSION, FOR INSTANCE 0.8 MEANS 80% COMPRESSION
c      TO BE USED AS ABCISSA FOR THE TABLE3D
       VOLSTR(I) = ONE-JAC(I)
       
c      PRINCIPAL STRETCHES FROM PRINCIPAL GL STRAINS
       ALAMBDA(I,1)=SQRT(TWO*Z1(I)+ONE)
       ALAMBDA(I,2)=SQRT(TWO*Z2(I)+ONE)
       ALAMBDA(I,3)=SQRT(TWO*Z3(I)+ONE)
c
c      PRINCIPAL ENGINEERING STRAIN POSITIVE IN COMPRESSION
c      TO BE USED AS ABCISSA FOR THE LOAD CURVES
C
       EPSILON(I,1) = ONE - ALAMBDA(I,1)  
       EPSILON(I,2) = ONE - ALAMBDA(I,2)  
       EPSILON(I,3) = ONE - ALAMBDA(I,3)  
C
C      ENGINEERING STRAIN RATE, COMPUTED FROM TRUE STRAIN RATE
C      AND WE TAKE THE ABSOLUTE VALUE ( SO LINEAR STRAIN RATE DEFINITION )
C      TO BE USED AS ABCISSA FOR THE TABLE3D (SECOND ABCISSA)
C
       STRAINRATE(I,1)= ABS(DRATE(I,1))
       STRAINRATE(I,2)= ABS(DRATE(I,2))
       STRAINRATE(I,3)= ABS(DRATE(I,3))
C
      ENDDO !I=1,NEL
C
C     2 STRESS LOOKUPS ARE DONE NOW, STATIC AND DYNAMIC
C     BOTH USE THE TABLE2D THAT CORRESPONDS TO THE CURRENT VALUE OF VOLSTR
C  

C     PRINCIPAL ENGINEERING STRESS POSITIVE IN COMPRESSION
C     THIS IS FROM THE QUASISTATIC LOAD CURVE ( LOWEST STRAIN RATE )


      !check table dimension
      IF (NDIM_TABLE == 1) THEN
         XVEC1(1:NEL,1)   = EPSILON(1:NEL,1)
         IPOS1(1:NEL,1)   = VARTMP(1:NEL,1)
         CALL TABLE_MAT_VINTERP(FUNC_SIG,NEL,NEL,IPOS1,XVEC1,FSIG1,DSIG1) 
         VARTMP(1:NEL,1)  = IPOS1(1:NEL,1)

         XVEC1(1:NEL,1)   = EPSILON(1:NEL,2)
         IPOS1(1:NEL,1)   = VARTMP(1:NEL,3)
         CALL TABLE_MAT_VINTERP(FUNC_SIG,NEL,NEL,IPOS1,XVEC1,FSIG2,DSIG2)
         VARTMP(1:NEL,3) = IPOS1(1:NEL,1)

         XVEC1(1:NEL,1)   = EPSILON(1:NEL,3)
         IPOS1(1:NEL,1) = VARTMP(1:NEL,5)
         CALL TABLE_MAT_VINTERP(FUNC_SIG,NEL,NEL,IPOS1,XVEC1,FSIG3,DSIG3)
         VARTMP(1:NEL,5) = IPOS1(1:NEL,1)
c
         !if 2D table is used   
      ELSEIF (NDIM_TABLE == 2) THEN
         XVEC2(1:NEL,1)   = EPSILON(1:NEL,1)
         XVEC2(1:NEL,2)   = ZERO           ! QUASISTATIC
         IPOS2(1:NEL,1)   = VARTMP(1:NEL,1)
         IPOS2(1:NEL,2)   = 1          
         CALL TABLE_MAT_VINTERP(FUNC_SIG,NEL,NEL,IPOS2,XVEC2,FSIG1,DSIG1)
         VARTMP(1:NEL,1) = IPOS2(1:NEL,1)

         XVEC2(1:NEL,1)   = EPSILON(1:NEL,2)
         XVEC2(1:NEL,2)   = ZERO           ! QUASISTATIC
         IPOS2(1:NEL,1)   = VARTMP(1:NEL,3)
         IPOS2(1:NEL,2)   = 1
         CALL TABLE_MAT_VINTERP(FUNC_SIG,NEL,NEL,IPOS2,XVEC2,FSIG2,DSIG2)
         VARTMP(1:NEL,3) = IPOS2(1:NEL,1)

         XVEC2(1:NEL,1)   = EPSILON(1:NEL,3)
         XVEC2(1:NEL,2)   = ZERO           ! QUASISTATIC
         IPOS2(1:NEL,1) = VARTMP(1:NEL,5)
         IPOS2(1:NEL,2) = 1         
         CALL TABLE_MAT_VINTERP(FUNC_SIG,NEL,NEL,IPOS2,XVEC2,FSIG3,DSIG3)
         VARTMP(1:NEL,5) = IPOS2(1:NEL,1)
   
      !if 3D table is used   
      ELSEIF (NDIM_TABLE == 3) THEN
     
         XVEC(1:NEL,1)   = EPSILON(1:NEL,1)
         XVEC(1:NEL,2)   = ZERO           ! QUASISTATIC
         XVEC(1:NEL,3)   = VOLSTR(1:NEL)
         IPOS(1:NEL,1) = VARTMP(1:NEL,1)
         IPOS(1:NEL,2) = 1
         IPOS(1:NEL,3) = VARTMP(1:NEL,2)  
         CALL TABLE_MAT_VINTERP(FUNC_SIG,NEL,NEL,IPOS,XVEC,FSIG1,DSIG1)
         VARTMP(1:NEL,1) = IPOS(1:NEL,1)
         VARTMP(1:NEL,2) = IPOS(1:NEL,3)
c
         XVEC(1:NEL,1)   = EPSILON(1:NEL,2)
         XVEC(1:NEL,2)   = ZERO           ! QUASISTATIC
         XVEC(1:NEL,3)   = VOLSTR(1:NEL)
         IPOS(1:NEL,1) = VARTMP(1:NEL,3)
         IPOS(1:NEL,2) = 1
         IPOS(1:NEL,3) = VARTMP(1:NEL,4)             
         CALL TABLE_MAT_VINTERP(FUNC_SIG,NEL,NEL,IPOS,XVEC,FSIG2,DSIG2)
         VARTMP(1:NEL,3) = IPOS(1:NEL,1)
         VARTMP(1:NEL,4) = IPOS(1:NEL,3)
c
         XVEC(1:NEL,1)   = EPSILON(1:NEL,3)
         XVEC(1:NEL,2)   = ZERO           ! QUASISTATIC
         XVEC(1:NEL,3)   = VOLSTR(1:NEL)
         IPOS(1:NEL,1) = VARTMP(1:NEL,5)
         IPOS(1:NEL,2) = 1
         IPOS(1:NEL,3) = VARTMP(1:NEL,6)             
         CALL TABLE_MAT_VINTERP(FUNC_SIG,NEL,NEL,IPOS,XVEC,FSIG3,DSIG3)
         VARTMP(1:NEL,5) = IPOS(1:NEL,1)
         VARTMP(1:NEL,6) = IPOS(1:NEL,3)
      ENDIF
c
      DO I=1,NEL
C
        SIGMA(I,1) = FSIG1(I) 
        SLOPE(I,1) = DSIG1(I) 

        SIGMA(I,2) = FSIG2(I) 
        SLOPE(I,2) = DSIG2(I) 

        SIGMA(I,3) = FSIG3(I) 
        SLOPE(I,3) = DSIG3(I) 
      ENDDO !I=1,NEL
C
C     PRINCIPAL ENGINEERING STRESS POSITIVE IN COMPRESSION
C     THIS IS FROM THE COMPLETE RATE DEPENDENT TABLE3D
C     SO 'ENGIRATE' HAS TO COME IN AS AN ADDITIONAL ARGUMENT
      !check table dimension
      IF (NDIM_TABLE == 1) THEN
C       DIRECTION 1
        XVEC1(1:NEL,1)   = EPSILON(1:NEL,1)       
        IPOS1(1:NEL,1) = VARTMP(1:NEL,7)
        CALL TABLE_MAT_VINTERP(FUNC_SIG,NEL,NEL,IPOS1,XVEC1,FSIG1,DSIG1)
        VARTMP(1:NEL,7) = IPOS1(1:NEL,1)

C       DIRECTION 2

        XVEC1(1:NEL,1)   = EPSILON(1:NEL,2)
        IPOS1(1:NEL,1) = VARTMP(1:NEL,10)
        CALL TABLE_MAT_VINTERP(FUNC_SIG,NEL,NEL,IPOS1,XVEC1,FSIG2,DSIG2)
        VARTMP(1:NEL,10) = IPOS1(1:NEL,1) 
      
C       DIRECTION 3

        XVEC1(1:NEL,1)   = EPSILON(1:NEL,3)
        IPOS1(1:NEL,1) = VARTMP(1:NEL,13)
        CALL TABLE_MAT_VINTERP(FUNC_SIG,NEL,NEL,IPOS1,XVEC1,FSIG3,DSIG3)
        VARTMP(1:NEL,13) = IPOS1(1:NEL,1) 

         !if 2D table is used   
      ELSEIF (NDIM_TABLE == 2) THEN
C       DIRECTION 1
        XVEC2(1:NEL,1)   = EPSILON(1:NEL,1)
        XVEC2(1:NEL,2)   = STRAINRATE(1:NEL,1) * SCAL       
        IPOS2(1:NEL,1) = VARTMP(1:NEL,7)
        IPOS2(1:NEL,2) = VARTMP(1:NEL,8)             
        CALL TABLE_MAT_VINTERP(FUNC_SIG,NEL,NEL,IPOS2,XVEC2,FSIG1,DSIG1)
        VARTMP(1:NEL,7) = IPOS2(1:NEL,1)
        VARTMP(1:NEL,8) = IPOS2(1:NEL,2)
C       DIRECTION 2
        XVEC2(1:NEL,1)   = EPSILON(1:NEL,2)
        XVEC2(1:NEL,2)   = STRAINRATE(1:NEL,2) * SCAL      
        IPOS2(1:NEL,1) = VARTMP(1:NEL,10)
        IPOS2(1:NEL,2) = VARTMP(1:NEL,11)           
        CALL TABLE_MAT_VINTERP(FUNC_SIG,NEL,NEL,IPOS2,XVEC2,FSIG2,DSIG2)
        VARTMP(1:NEL,10) = IPOS2(1:NEL,1) 
        VARTMP(1:NEL,11) = IPOS2(1:NEL,2) 
C       DIRECTION 3
        XVEC2(1:NEL,1)   = EPSILON(1:NEL,3)
        XVEC2(1:NEL,2)   = STRAINRATE(1:NEL,3) * SCAL      
        IPOS2(1:NEL,1) = VARTMP(1:NEL,13)
        IPOS2(1:NEL,2) = VARTMP(1:NEL,14)         
        CALL TABLE_MAT_VINTERP(FUNC_SIG,NEL,NEL,IPOS2,XVEC2,FSIG3,DSIG3)
        VARTMP(1:NEL,13) = IPOS2(1:NEL,1) 
        VARTMP(1:NEL,14) = IPOS2(1:NEL,2) 
c        
      !if 3D table is used   
      ELSEIF (NDIM_TABLE == 3) THEN
C       DIRECTION 1
        XVEC(1:NEL,1)   = EPSILON(1:NEL,1)
        XVEC(1:NEL,2)   = STRAINRATE(1:NEL,1) * SCAL
        XVEC(1:NEL,3)   = VOLSTR(1:NEL)
!       
        IPOS(1:NEL,1) = VARTMP(1:NEL,7)
        IPOS(1:NEL,2) = VARTMP(1:NEL,8)
        IPOS(1:NEL,3) = VARTMP(1:NEL,9)             
        CALL TABLE_MAT_VINTERP(FUNC_SIG,NEL,NEL,IPOS,XVEC,FSIG1,DSIG1)
        VARTMP(1:NEL,7) = IPOS(1:NEL,1)
        VARTMP(1:NEL,8) = IPOS(1:NEL,2)
        VARTMP(1:NEL,9) = IPOS(1:NEL,3)

C       DIRECTION 2

        XVEC(1:NEL,1)   = EPSILON(1:NEL,2)
        XVEC(1:NEL,2)   = STRAINRATE(1:NEL,2) * SCAL
        XVEC(1:NEL,3)   = VOLSTR(1:NEL)       
        IPOS(1:NEL,1) = VARTMP(1:NEL,10)
        IPOS(1:NEL,2) = VARTMP(1:NEL,11)
        IPOS(1:NEL,3) = VARTMP(1:NEL,12)            
        CALL TABLE_MAT_VINTERP(FUNC_SIG,NEL,NEL,IPOS,XVEC,FSIG2,DSIG2)
        VARTMP(1:NEL,10) = IPOS(1:NEL,1) 
        VARTMP(1:NEL,11) = IPOS(1:NEL,2) 
        VARTMP(1:NEL,12) = IPOS(1:NEL,3)
      
C       DIRECTION 3

        XVEC(1:NEL,1)   = EPSILON(1:NEL,3)
        XVEC(1:NEL,2)   = STRAINRATE(1:NEL,3) * SCAL
        XVEC(1:NEL,3)   = VOLSTR(1:NEL)       
        IPOS(1:NEL,1) = VARTMP(1:NEL,13)
        IPOS(1:NEL,2) = VARTMP(1:NEL,14)
        IPOS(1:NEL,3) = VARTMP(1:NEL,15)            
        CALL TABLE_MAT_VINTERP(FUNC_SIG,NEL,NEL,IPOS,XVEC,FSIG3,DSIG3)
        VARTMP(1:NEL,13) = IPOS(1:NEL,1) 
        VARTMP(1:NEL,14) = IPOS(1:NEL,2) 
        VARTMP(1:NEL,15) = IPOS(1:NEL,3)

      ENDIF  !NDIM_TABLE



      DO I=1,NEL

        DYNSIGMA(I,1) = FSIG1(I) 
        DYNSIGMA(I,2) = FSIG2(I) 
        DYNSIGMA(I,3) = FSIG3(I) 
        YLD(I) = SQRT(DYNSIGMA(I,1)**2 + DYNSIGMA(I,2)**2 + DYNSIGMA(I,3)**2 )
C
C       PRINCIPAL 2PK STRESS POSITIVE IN TENSION
C
        SIK(I,1)=-SIGMA(I,1)/ALAMBDA(I,1)
        SIK(I,2)=-SIGMA(I,2)/ALAMBDA(I,3)
        SIK(I,3)=-SIGMA(I,3)/ALAMBDA(I,3)
C
C       INCREMENTAL PRINCIPAL STIFFNESSES
C
        DIK(I,1) = SLOPE(I,1)/ALAMBDA(I,1)**2+SIGMA(I,1)/ALAMBDA(I,1)**3
        DIK(I,2) = SLOPE(I,2)/ALAMBDA(I,2)**2+SIGMA(I,2)/ALAMBDA(I,2)**3
        DIK(I,3) = SLOPE(I,3)/ALAMBDA(I,3)**2+SIGMA(I,3)/ALAMBDA(I,3)**3

C
C       VISCOUS OVERSTRESS IN TERMS OF 2PK
C       POSITIVE IN TENSION
C
        DRATE(I,1)=-(DYNSIGMA(I,1)-SIGMA(I,1))/ALAMBDA(I,1)
        DRATE(I,2)=-(DYNSIGMA(I,2)-SIGMA(I,2))/ALAMBDA(I,2)
        DRATE(I,3)=-(DYNSIGMA(I,3)-SIGMA(I,3))/ALAMBDA(I,3)
      ENDDO
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     COMPUTE INCREMENTAL STIFFNESS MATRIX
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DO L=1,3
        DO K=1,6
          DO J=1,6
            DO I=1,NEL
              CIJKL(I,J,K)=ZERO
            ENDDO
          ENDDO
        ENDDO
      ENDDO
C
      DO K=1,6
        DO J=1,6
          DO I=1,NEL
            DO L=1,3
              CIJKL(I,J,K)=CIJKL(I,J,K)
     .          +  DFIRST(I,L,J)*DFIRST(I,L,K)*DIK(I,L)
     .          + DSECOND(I,L,J,K)* SIK(I,L)
  
            ENDDO
          ENDDO
        ENDDO
      ENDDO
C
C     FACTOR 1/4 FOR VOIGT NOTATION WITH GAMMAS ( 2 GAMMAS)
C     FACTOR 1/2 FOR VOIGT NOTATION WITH GAMMAS ( 1 GAMMA)
C
      DO K=4,6
        DO J=4,6
          DO I=1,NEL
            CIJKL(I,J,K)=CIJKL(I,J,K)/FOUR
          ENDDO
        ENDDO
      ENDDO
C
      DO K=1,3
        DO J=4,6
          DO I=1,NEL
            CIJKL(I,J,K)=CIJKL(I,J,K)*HALF
          ENDDO
        ENDDO
      ENDDO
      DO K=4,6
        DO J=1,3
          DO I=1,NEL
            CIJKL(I,J,K)=CIJKL(I,J,K)*HALF
          ENDDO
        ENDDO
      ENDDO
C-------------------             
      RETURN
      END
      END MODULE
