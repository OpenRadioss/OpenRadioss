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
Chd|  XFEM_CRK_DIR                  source/elements/xfem/xfem_crk_dir.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE XFEM_CRK_DIR(NEL   ,ILAY  ,IXFEM   ,ELCRKINI,
     .                        DIR_A ,TENS  ,DIR1_CRK,DIR2_CRK,IROT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include   "implicit_f.inc"
#include   "scr17_c.inc"
#include   "param_c.inc"
#include   "com_xfem1.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,ILAY,IXFEM,IROT
      INTEGER ELCRKINI(NXLAYMAX,*)
      my_real DIR_A(NEL,2),TENS(NEL,5),DIR1_CRK(NXLAYMAX,NEL),
     .   DIR2_CRK(NXLAYMAX,NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JCT(NEL)
      INTEGER I,IR,NEWCRK
      my_real CC,BB,CR,ORM,SS1,SS2,S1,S2,S3,S4,S5
C=======================================================================
      NEWCRK = 0
      DO I=1,NEL
        JCT(I) = 0
        IF (ABS(ELCRKINI(ILAY,I)) == 1) THEN  ! new crack (init or adv)
          NEWCRK = NEWCRK + 1
          JCT(NEWCRK) = I
        ENDIF
      ENDDO
      IF (NEWCRK == 0) RETURN
c------------------------------------------
c     crack directions dans le repere coque
c------------------------------------------
      IF (IROT > 0) THEN
        DO IR=1,NEWCRK
          I  = JCT(IR)            
          S1 = DIR_A(I,1)*DIR_A(I,1)*TENS(I,1)
     .       + DIR_A(I,2)*DIR_A(I,2)*TENS(I,2)
     .       - TWO*DIR_A(I,1)*DIR_A(I,2)*TENS(I,3)
          S2 = DIR_A(I,2)*DIR_A(I,2)*TENS(I,1)
     .       + DIR_A(I,1)*DIR_A(I,1)*TENS(I,2)
     .       + TWO*DIR_A(I,2)*DIR_A(I,1)*TENS(I,3)
          S3 = DIR_A(I,1)*DIR_A(I,2)*TENS(I,1)
     .       - DIR_A(I,2)*DIR_A(I,1)*TENS(I,2)
     .       +(DIR_A(I,1)*DIR_A(I,1)-DIR_A(I,2)*DIR_A(I,2))*TENS(I,3)
          S4 = DIR_A(I,2)*TENS(I,5)+DIR_A(I,1)*TENS(I,4)
          S5 = DIR_A(I,1)*TENS(I,5)-DIR_A(I,2)*TENS(I,4)
          TENS(I,1) = S1
          TENS(I,2) = S2
          TENS(I,3) = S3
          TENS(I,4) = S4
          TENS(I,5) = S5
        ENDDO
      ENDIF                               
c
      DO IR=1,NEWCRK
        I   = JCT(IR)              
        CC  = (TENS(I,1)+TENS(I,2))*HALF      
        BB  = (TENS(I,1)-TENS(I,2))*HALF      
        CR  = SQRT(BB*BB+TENS(I,3)*TENS(I,3))   
        SS1 = CC+CR                             
        SS2 = CC-CR                             
        DIR1_CRK(ILAY,I) = TENS(I,3)              
        DIR2_CRK(ILAY,I) = SS1-TENS(I,1)          
        ORM = DIR1_CRK(ILAY,I)*DIR1_CRK(ILAY,I)+   
     .        DIR2_CRK(ILAY,I)*DIR2_CRK(ILAY,I)    
        ORM = SQRT(ORM)                          
        IF (ORM < EM8) THEN                      
          DIR1_CRK(ILAY,I)=ONE                   
          DIR2_CRK(ILAY,I)=ZERO                 
        ELSE                                   
          DIR1_CRK(ILAY,I)=DIR1_CRK(ILAY,I)/ORM  
          DIR2_CRK(ILAY,I)=DIR2_CRK(ILAY,I)/ORM  
        ENDIF                                  
      ENDDO                                    
C-----------
      RETURN
      END
