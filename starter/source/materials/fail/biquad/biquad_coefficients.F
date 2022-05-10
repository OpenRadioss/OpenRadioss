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
Chd|  BIQUAD_COEFFICIENTS           source/materials/fail/biquad/biquad_coefficients.F
Chd|-- called by -----------
Chd|        CFAILINI                      source/elements/shell/coque/cfailini.F
Chd|        CFAILINI4                     source/elements/shell/coque/cfailini.F
Chd|        FAILINI                       source/elements/solid/solide/failini.F
Chd|        HM_READ_FAIL_BIQUAD           source/materials/fail/biquad/hm_read_fail_biquad.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE BIQUAD_COEFFICIENTS(
     .           C1    ,C2    ,C3    ,C4    ,C5    ,PTHK  ,L     ,
     .           X_1   ,X_2   ,E1    ,E2    ,E3    ,E4    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER L
      my_real  C1,C2,C3,C4,C5,PTHK
      my_real  E1,E2,E3,E4
      my_real  X_1(2),X_2(3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, J, K, IPIV2(2), IPIV3(3), INFO
      my_real TRIAX_1_LIN, TRIAX_2_LIN, TRIAX_3_LIN,
     .        TRIAX_4_LIN, TRIAX_5_LIN
      my_real TRIAX_1_QUAD, TRIAX_2_QUAD, 
     .        TRIAX_3_QUAD, TRIAX_4_QUAD, TRIAX_5_QUAD
      my_real A_1(2,2), B_1(2)
      my_real A_2(3,3), B_2(3)
      DATA TRIAX_1_LIN, TRIAX_2_LIN, TRIAX_3_LIN, TRIAX_4_LIN,
     .        TRIAX_5_LIN 
     .     / -0.33333333, 0.0, 0.33333333, 0.57735, 0.66666667 /
      DATA TRIAX_1_QUAD, TRIAX_2_QUAD, TRIAX_3_QUAD, 
     .     TRIAX_4_QUAD, TRIAX_5_QUAD 
     .     / 0.111111, 0.0, 0.111111, 0.333333, 0.444444 /
#ifndef WITHOUT_LINALG
C=============================================
C
C pre definition for user-input data when only 
C tension test data are provided
C
C=============================================  
       IF (L/=0)THEN
         IF     (L == 1) THEN      ! Mild Seel (c3 = 0.6)
           c1 =  3.5 * c3
           c2 =  1.6 * c3         
           c4 =  0.6 * c3
           c5 =  1.5 * c3
         ELSEIF (L == 2) THEN      ! DP600 (c3 = 0.5)
           c1 =  4.3 * c3
           c2 =  1.4 * c3          
           c4 =  0.6 * c3
           c5 =  1.6 * c3
         ELSEIF (L == 3) THEN      ! Boron (c3 = 0.12)
           c1 =   5.2 * c3
           c2 =   3.1 * c3         
           c4 =   0.8 * c3
           c5 =   3.5 * c3
         ELSEIF (L == 4) THEN      ! Aluminium AA5182 (c3 = 0.3)
           c1 =   5.0 * c3
           c2 =   1.0 * c3         
           c4 =   0.4 * c3
           c5 =   0.8 * c3
         ELSEIF (L == 5) THEN      ! Aluminium AA6082-T6 (c3 = 0.17)
           c1 =   7.8 * c3
           c2 =   3.5 * c3         
           c4 =   0.6 * c3
           c5 =   2.8 * c3
         ELSEIF (L == 6) THEN      ! Plastic light_eBody PA6GF30 (c3 = 0.1)
           c1 =   3.6 * c3
           c2 =   0.6 * c3         
           c4 =   0.5 * c3
           c5 =   0.6 * c3
         ELSEIF (L == 7) THEN      ! Plastic light_eBody PP T40 ( c3=0.11 )
           c1 =  10.0 * c3
           c2 =   2.7 * c3         
           c4 =   0.6 * c3
           c5 =   0.7 * c3
         ELSEIF (L == 99) THEN     ! user scalling factors
           c1 =    e1 * c3
           c2 =    e2 * c3        
           c4 =    e3 * c3
           c5 =    e4 * c3
         ELSE                      ! ELSE --> Mild Seel
           c1 =  3.5 * c3
           c2 =  1.6 * c3          
           c4 =  0.6 * c3
           c5 =  1.5 * c3
         ENDIF        
       ELSEIF(c1 == ZERO .AND. c2 == ZERO .AND. c4 == ZERO .AND. c5 == ZERO) THEN
         c1 =  3.5 * c3
         c2 =  1.6 * c3    
         c4 =  0.6 * c3
         c5 =  1.5 * c3
       ENDIF
C=============================================
C
C determine coefficient matrix for parable_1
C
C=============================================
        A_1(1,1) = TRIAX_1_LIN
        A_1(1,2) = TRIAX_1_QUAD
        A_1(2,1) = TRIAX_3_LIN
        A_1(2,2) = TRIAX_3_QUAD
        B_1(1)   =   c1 - c2
        B_1(2)   =   c3 - c2
C
C!      fitting the first quadratic function
        IF (IRESP == 0) THEN 
          CALL DGESV(2, 1, A_1, 2, IPIV2, B_1, 2, INFO)
        ELSE
          CALL SGESV(2, 1, A_1, 2, IPIV2, B_1, 2, INFO)
        ENDIF
        X_1(1:2) = B_1(1:2)
C
C
C=============================================
C
C determine coefficient matrix for parable_2
C
C=============================================
        A_2(1,1) = 1.0
        A_2(1,2) = TRIAX_3_LIN
        A_2(1,3) = TRIAX_3_QUAD
        A_2(2,1) = 1.0
        A_2(2,2) = TRIAX_4_LIN
        A_2(2,3) = TRIAX_4_QUAD
        A_2(3,1) = 1.0
        A_2(3,2) = TRIAX_5_LIN
        A_2(3,3) = TRIAX_5_QUAD
        B_2(1)   =   c3
        B_2(2)   =   c4
        B_2(3)   =   c5
C
C!      fitting the second quadratic function
        IF (IRESP == 0) THEN 
          CALL DGESV(3, 1, A_2, 3, IPIV3, B_2, 3, INFO)
        ELSE
          CALL SGESV(3, 1, A_2, 3, IPIV3, B_2, 3, INFO)
        ENDIF
        X_2(1:3) = B_2(1:3)
#else 
        WRITE(6,*) "Error: Blas/Lapack required for /FAIL/BIQUAD"
#endif
C
c------------
       RETURN
       END
