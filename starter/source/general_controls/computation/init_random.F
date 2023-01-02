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
Chd|  INIT_RANDOM                   source/general_controls/computation/init_random.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        RANDOM_MOD                    share/modules1/random_mod.F   
Chd|====================================================================
        SUBROUTINE INIT_RANDOM( )
!$COMMENT
!       INIT_RANDOM description
!       initialization of RAND_STRUCT structure     
!       
!       INIT_RANDOM organization :
!       - check -rxalea or -rseed cmd line is used
!       - check the value or -rxalea / -rseed and print
!         a warning if the value is not good
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE RANDOM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        IF(RAND_STRUCT%CMD) THEN

            WRITE(IOUT,1000)

            IF(RAND_STRUCT%ALEA.AND.(.NOT.RAND_STRUCT%SEED) ) THEN
                !   apply default value to seed
                RAND_STRUCT%SEED=.TRUE.
                RAND_STRUCT%SEED_NBR=0.d+00
                WRITE(IOUT,'(A)') ' '
                WRITE(IOUT,'(A)') '** WARNING : -RSEED OPTIONAL OPTION IS NOT USED'
                WRITE(IOUT,'(A)') '              DEFAULT VALUE WILL BE APPLIED'
                WRITE(IOUT,'(A)') ' '
            ENDIF
            IF(RAND_STRUCT%SEED) THEN
                IF(RAND_STRUCT%SEED_NBR<0.d+00.OR.RAND_STRUCT%SEED_NBR>1.d+00) THEN
                    !   error, 0 <= seed < 1
                    WRITE(IOUT,'(A)') ' '
                    WRITE(IOUT,'(A)') '** WARNING : -RSEED VALUE MUST BE DEFINED BETWEEN'
                    WRITE(IOUT,'(A)') '              0. AND 1.'     
                    WRITE(IOUT,'(A)') ' '
                ENDIF
            ENDIF
            IF(RAND_STRUCT%ALEA) THEN
                IF(RAND_STRUCT%ALEA_NBR<0.d+00.OR.RAND_STRUCT%ALEA_NBR>10.d+30) THEN
                    !   error, 0 <= alea < +infinity
                    WRITE(IOUT,'(A)') ' '
                    WRITE(IOUT,'(A)') '** WARNING : -RALEA VALUE MUST BE DEFINED BETWEEN'
                    WRITE(IOUT,'(A)') '              0 AND +INFINITY'       
                    WRITE(IOUT,'(A)') ' '
                ENDIF
            ELSE
                RAND_STRUCT%CMD=.FALSE.
                WRITE(IOUT,'(A)') ' '
                WRITE(IOUT,'(A)') '** WARNING : -RSEED OPTIONAL OPTION MUST BE USED WITH -RXALEA OPTION'
                WRITE(IOUT,'(A)') '             -RSEED OPTION WILL BE IGNORED'
                WRITE(IOUT,'(A)') ' '
            ENDIF
        ENDIF

 1000 FORMAT(//
     .'     RANDOM NOISE       '/
     .'     ------------       ')

        RETURN
        END SUBROUTINE INIT_RANDOM
