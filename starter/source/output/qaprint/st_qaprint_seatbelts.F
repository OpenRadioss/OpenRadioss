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
Chd|  ST_QAPRINT_SEATBELTS          source/output/qaprint/st_qaprint_seatbelts.F
Chd|-- called by -----------
Chd|        ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|-- calls ---------------
Chd|        SEATBELT_MOD                  ../common_source/modules/seatbelt_mod.F
Chd|====================================================================
      SUBROUTINE ST_QAPRINT_SEATBELTS(ITAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE QA_OUT_MOD
        USE SEATBELT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "scr10_c.inc"
#include      "tabsiz_c.inc"
#include      "units_c.inc"
#include      "nchar_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: ITAB(*)
C--------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: OK_QA
      CHARACTER (LEN=255) :: VARNAME
      INTEGER :: I,J
      DOUBLE PRECISION :: TMPVAL
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IF(NSLIPRING + NRETRACTOR == 0)RETURN
C
      OK_QA = MYQAKEY('/SLIPRING')  
C                                                         
      IF (OK_QA) THEN
C   
         DO I = 1, NSLIPRING
C
            CALL QAPRINT('SLIPRING ID ',SLIPRING(I)%ID,0.0_8)
            CALL QAPRINT('SLIPRING NFRAM  ',SLIPRING(I)%NFRAM,0.0_8)
            CALL QAPRINT('SLIPRING FL_FLAG',SLIPRING(I)%FL_FLAG,0.0_8)
            CALL QAPRINT('SLIPRING SENSID ',SLIPRING(I)%SENSID,0.0_8)
            CALL QAPRINT('SLIPRING FUNC1 ',SLIPRING(I)%IFUNC(1),0.0_8)
            CALL QAPRINT('SLIPRING FUNC2 ',SLIPRING(I)%IFUNC(2),0.0_8)
            CALL QAPRINT('SLIPRING FUNC3 ',SLIPRING(I)%IFUNC(3),0.0_8)
            CALL QAPRINT('SLIPRING FUNC4 ',SLIPRING(I)%IFUNC(4),0.0_8)
C
            TMPVAL = SLIPRING(I)%DC
            CALL QAPRINT('SLIPRING DC FACTOR ',0,TMPVAL)
            TMPVAL = SLIPRING(I)%A
            CALL QAPRINT('SLIPRING A FACTOR ',0,TMPVAL)
C
            TMPVAL = SLIPRING(I)%FRIC
            CALL QAPRINT('SLIPRING FRICD ',0,TMPVAL)
            TMPVAL = SLIPRING(I)%FAC_D(1)
            CALL QAPRINT('SLIPRING FACD1 ',0,TMPVAL)
            TMPVAL = SLIPRING(I)%FAC_D(2)
            CALL QAPRINT('SLIPRING FACD2 ',0,TMPVAL)
            TMPVAL = SLIPRING(I)%FAC_D(3)
            CALL QAPRINT('SLIPRING FACD3 ',0,TMPVAL)
C
            TMPVAL = SLIPRING(I)%FRICS
            CALL QAPRINT('SLIPRING FRICS ',0,TMPVAL)
            TMPVAL = SLIPRING(I)%FAC_S(1)
            CALL QAPRINT('SLIPRING FACS1 ',0,TMPVAL)
            TMPVAL = SLIPRING(I)%FAC_S(2)
            CALL QAPRINT('SLIPRING FACS2 ',0,TMPVAL)
            TMPVAL = SLIPRING(I)%FAC_S(3)
            CALL QAPRINT('SLIPRING FACS3 ',0,TMPVAL)
C
            DO J=1,SLIPRING(I)%NFRAM
C
              CALL QAPRINT('SLIPRING N1 ',ITAB(SLIPRING(I)%FRAM(J)%NODE(1)),0.0_8)
              CALL QAPRINT('SLIPRING N2 ',ITAB(SLIPRING(I)%FRAM(J)%NODE(2)),0.0_8)
              CALL QAPRINT('SLIPRING N3 ',ITAB(SLIPRING(I)%FRAM(J)%NODE(3)),0.0_8)
              CALL QAPRINT('SLIPRING ANCHOR ',ITAB(SLIPRING(I)%FRAM(J)%ANCHOR_NODE),0.0_8)
              IF (SLIPRING(I)%FRAM(J)%ORIENTATION_NODE > 0) THEN
                CALL QAPRINT('SLIPRING ORIENTATION NODE ',ITAB(SLIPRING(I)%FRAM(J)%ORIENTATION_NODE),0.0_8)
              ENDIF
              CALL QAPRINT('SLIPRING DIRECTION1 ',SLIPRING(I)%FRAM(J)%STRAND_DIRECTION(1),0.0_8)
              CALL QAPRINT('SLIPRING DIRECTION2 ',SLIPRING(I)%FRAM(J)%STRAND_DIRECTION(2),0.0_8)
C
              TMPVAL = SLIPRING(I)%FRAM(J)%ORIENTATION_ANGLE
              CALL QAPRINT('SLIPRING ANGLE ',0,TMPVAL)
C
              TMPVAL = SLIPRING(I)%FRAM(J)%VECTOR(1)
              CALL QAPRINT('SLIPRING VECTOR1 ',0,TMPVAL)
              TMPVAL = SLIPRING(I)%FRAM(J)%VECTOR(2)
              CALL QAPRINT('SLIPRING VECTOR2 ',0,TMPVAL)
              TMPVAL = SLIPRING(I)%FRAM(J)%VECTOR(3)
              CALL QAPRINT('SLIPRING VECTOR3 ',0,TMPVAL)
              TMPVAL = SLIPRING(I)%FRAM(J)%VECTOR(4)
              CALL QAPRINT('SLIPRING VECTOR4 ',0,TMPVAL)
              TMPVAL = SLIPRING(I)%FRAM(J)%VECTOR(5)
              CALL QAPRINT('SLIPRING VECTOR5 ',0,TMPVAL)
              TMPVAL = SLIPRING(I)%FRAM(J)%VECTOR(6)
              CALL QAPRINT('SLIPRING VECTOR6 ',0,TMPVAL)
C
            ENDDO
C
         ENDDO 
      ENDIF
C        
      OK_QA = MYQAKEY('/RETRACTOR')  
C                                                         
      IF (OK_QA) THEN   
         DO I = 1, NRETRACTOR
C
            CALL QAPRINT('RETRACTOR ID ',RETRACTOR(I)%ID,0.0_8)
            CALL QAPRINT('RETRACTOR N1 ',ITAB(RETRACTOR(I)%NODE(1)),0.0_8)
            CALL QAPRINT('RETRACTOR N2 ',ITAB(RETRACTOR(I)%NODE(2)),0.0_8)
            CALL QAPRINT('RETRACTOR ANCHOR ',ITAB(RETRACTOR(I)%ANCHOR_NODE),0.0_8)
            CALL QAPRINT('RETRACTOR DIRECTION ',RETRACTOR(I)%STRAND_DIRECTION,0.0_8)
            CALL QAPRINT('RETRACTOR FUNC1 ',RETRACTOR(I)%IFUNC(1),0.0_8)
            CALL QAPRINT('RETRACTOR FUNC2 ',RETRACTOR(I)%IFUNC(2),0.0_8)
            CALL QAPRINT('RETRACTOR FUNC3 ',RETRACTOR(I)%IFUNC(3),0.0_8)
            CALL QAPRINT('RETRACTOR SENSID1 ',RETRACTOR(I)%ISENS(1),0.0_8)
            CALL QAPRINT('RETRACTOR SENSID2 ',RETRACTOR(I)%ISENS(2),0.0_8)
            CALL QAPRINT('RETRACTOR TENSTYP ',RETRACTOR(I)%TENS_TYP,0.0_8)
            CALL QAPRINT('RETRACTOR INACTIVE NODES ',RETRACTOR(I)%INACTI_NNOD,0.0_8)
C
            DO J=1,RETRACTOR(I)%INACTI_NNOD
              CALL QAPRINT('RETRACTOR INACTI NODE ',ITAB(RETRACTOR(I)%INACTI_NODE(J)),0.0_8)
            ENDDO
C
            TMPVAL = RETRACTOR(I)%VECTOR(1)
            CALL QAPRINT('RETRACTOR VECTOR1 ',0,TMPVAL)
            TMPVAL = RETRACTOR(I)%VECTOR(2)
            CALL QAPRINT('RETRACTOR VECTOR2 ',0,TMPVAL)
            TMPVAL = RETRACTOR(I)%VECTOR(3)
            CALL QAPRINT('RETRACTOR VECTOR3 ',0,TMPVAL)
C
            TMPVAL = RETRACTOR(I)%ELEMENT_SIZE
            CALL QAPRINT('RETRACTOR ELEM SIZE ',0,TMPVAL)
            TMPVAL = RETRACTOR(I)%FORCE
            CALL QAPRINT('RETRACTOR FORCE ',0,TMPVAL)
C
            TMPVAL = RETRACTOR(I)%FAC(1)
            CALL QAPRINT('RETRACTOR FAC1 ',0,TMPVAL)
            TMPVAL = RETRACTOR(I)%FAC(2)
            CALL QAPRINT('RETRACTOR FAC2 ',0,TMPVAL)
            TMPVAL = RETRACTOR(I)%FAC(3)
            CALL QAPRINT('RETRACTOR FAC3 ',0,TMPVAL)
            TMPVAL = RETRACTOR(I)%FAC(4)
            CALL QAPRINT('RETRACTOR FAC4 ',0,TMPVAL)
C
            TMPVAL = RETRACTOR(I)%PULLOUT
            CALL QAPRINT('RETRACTOR PULLOUT ',0,TMPVAL)
C
         ENDDO 
      ENDIF                                                                        
C-----------------------------------------------
      RETURN
      END

