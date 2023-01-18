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
Chd|  HM_READ_MOVE_FUNCT            source/tools/curve/hm_read_move_funct.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE HM_READ_MOVE_FUNCT(NPC,PLD,NFUNCT,TABLE,NTABLE,FUNCRYPT,UNITAB, LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE TABLE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NFUNCT, NTABLE
      INTEGER NPC(*),FUNCRYPT(*)
C     REAL
      my_real
     .   PLD(*)
      TYPE(TTABLE) TABLE(*)
      TYPE(UNIT_TYPE_), INTENT(IN) :: UNITAB
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
C     NSUBMOD
#include      "submod_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,ID,IFIX_TMP,CPT
      my_real
     .   SCX,SCY,SHX,SHY
      my_real, DIMENSION(:),ALLOCATABLE :: PLD_TMP
      INTEGER :: NMOVE_FUNCT
      LOGICAL :: IS_AVAILABLE
      CHARACTER TITR*nchartitle
!     ***************************    !
!     * Count /MOVE_FUNCT cards *    !
!     ***************************    !
      CALL HM_OPTION_COUNT('/MOVE_FUNCT', NMOVE_FUNCT)
      
      IF (NMOVE_FUNCT > 0) THEN
         WRITE (IOUT, 2000)
         CALL HM_OPTION_START('/MOVE_FUNCT')         
         DO I = 1, NMOVE_FUNCT
            CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID = ID, OPTION_TITR = TITR)
            CALL HM_GET_FLOATV('Ascalex', SCX, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOATV('Fscaley', SCY, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOATV('Ashiftx', SHX, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOATV('Fshifty', SHY, IS_AVAILABLE, LSUBMODEL, UNITAB)
            
            IF (SCX == ZERO) SCX = ONE          
            IF (SCY == ZERO) SCY = ONE
            DO N = 1,NFUNCT
               IF (NPC(NFUNCT+1+N) == ID) THEN
                  IF (FUNCRYPT(N) == 0) THEN
                     WRITE (IOUT,2100) ID,SCX,SCY,SHX,SHY
                  ELSE
                     WRITE (IOUT,2200) ID,SCX,SCY,SHX,SHY
                  ENDIF
                  IF(SCX >= ZERO) THEN
                     DO J = NPC(N),NPC(N+1)-1,2
                        PLD(J)   = PLD(J)  *SCX
                        PLD(J+1) = PLD(J+1)*SCY
                        PLD(J)   = PLD(J)  +SHX
                        PLD(J+1) = PLD(J+1)+SHY
                        IF (FUNCRYPT(N) == 0) WRITE(IOUT,'(8X,1PG20.13,2X,1G20.13)')PLD(J),PLD(J+1)   
                     ENDDO
                  ELSEIF(SCX < ZERO) THEN
                     ALLOCATE(PLD_TMP(NPC(N+1)))
                     CPT = 1
                     DO J = NPC(N),NPC(N+1)-1,2
                        PLD_TMP(J)   = PLD(NPC(N+1)-1-CPT)  *SCX
                        PLD_TMP(J+1) = PLD(NPC(N+1)-CPT)*SCY
                        PLD_TMP(J)   = PLD_TMP(J)  +SHX
                        PLD_TMP(J+1) = PLD_TMP(J+1)+SHY  
                        CPT = CPT + 2
                     ENDDO
                     DO J = NPC(N),NPC(N+1)-1,2
                        PLD(J)   = PLD_TMP(J)
                        PLD(J+1) = PLD_TMP(J+1)
                        IF (FUNCRYPT(N) == 0) WRITE(IOUT,'(8X,1PG20.13,2X,1G20.13)')PLD(J),PLD(J+1)   
                     ENDDO
                     IF(ALLOCATED(PLD_TMP)) DEALLOCATE(PLD_TMP)
                  ENDIF
                  EXIT
               ENDIF
            ENDDO
            DO N = 1,NTABLE
               IF (TABLE(N)%NOTABLE == ID) THEN
                  DO J = 1,SIZE(TABLE(N)%X(1)%VALUES)
                     TABLE(N)%X(1)%VALUES(J)= TABLE(N)%X(1)%VALUES(J)  *SCX
                     TABLE(N)%Y%VALUES(J)   = TABLE(N)%Y%VALUES(J)     *SCY
                     TABLE(N)%X(1)%VALUES(J)= TABLE(N)%X(1)%VALUES(J)+SHX
                     TABLE(N)%Y%VALUES(J)   = TABLE(N)%Y%VALUES(J)   +SHY
                  ENDDO
                  EXIT
               ENDIF
            ENDDO
         ENDDO 
      ENDIF
C
      RETURN
C-----------------------------------------------------------------
2000  FORMAT(//
     .       '    FUNCTION  TRANSFORMATION '/)
2100  FORMAT(/
     . '    LOAD FUNCTION NUMBER. . . . . =',I10/
     . '      X SCALE                    = ',E20.13/,                        
     . '      Y SCALE                    = ',E20.13/,                        
     . '      X SHIFT                    = ',E20.13/,                        
     . '      Y SHIFT                    = ',E20.13/,                        
     . '      TRANSFORMED VALUES :'/,
     . '          X                     Y    ')
2200  FORMAT(/
     . '    LOAD FUNCTION NUMBER. . . . . =',I10/
     . '      X SCALE                    = ',E20.13/,                        
     . '      Y SCALE                    = ',E20.13/,                        
     . '      X SHIFT                    = ',E20.13/,                        
     . '      Y SHIFT                    = ',E20.13/,                        
     . '      TRANSFORMED VALUES : CONFIDENTIAL DATA')
      END
