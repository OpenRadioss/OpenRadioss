Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  ST_QAPRINT_INIVOL             source/output/qaprint/st_qaprint_inivol.F
Chd|-- called by -----------
Chd|        ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INIVOL_DEF_MOD                share/modules1/inivol_mod.F   
Chd|====================================================================
      SUBROUTINE ST_QAPRINT_INIVOL()
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE QA_OUT_MOD
        USE GROUPDEF_MOD
        USE INIVOL_DEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "nchar_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C
C--------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: OK_QA
      CHARACTER (LEN=255) :: VARNAME
      INTEGER KK,JJ
      INTEGER ID,IPARTFILL,NBCONTY
      INTEGER IPHASE,ICUMU,IREVERSED
      DOUBLE PRECISION VFRAC 
      CHARACTER*nchartitle TITLE
      DOUBLE PRECISION TEMP_DOUBLE      
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      OK_QA = MYQAKEY('/INIVOL')  
                                                         
      IF (OK_QA) THEN

          DO KK=1,NUM_INIVOL

            ID = INIVOL(KK)%ID
            TITLE = INIVOL(KK)%TITLE
            NBCONTY = INIVOL(KK)%NUM_CONTAINER
            IPARTFILL = INIVOL(KK)%PART_ID

            WRITE(VARNAME,'(A,I0,A)')   'INIVOL(',KK ,')%ID            ='
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ID,0.0_8)             
            WRITE(VARNAME,'(A,I0,A,A)') 'INIVOL(',KK ,')%TITLE         =',TITLE(1:LEN_TRIM(TITLE))
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,0.0_8)             
            WRITE(VARNAME,'(A,I0,A)')   'INIVOL(',KK ,')%NUM_CONTAINER ='
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NBCONTY,0.0_8)             
            WRITE(VARNAME,'(A,I0,A)')   'INIVOL(',KK ,')%PART_ID       ='
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IPARTFILL,0.0_8)

            DO JJ=1,NBCONTY

              IPHASE = INIVOL(KK)%CONTAINER(JJ)%SUBMAT_ID
              ICUMU = INIVOL(KK)%CONTAINER(JJ)%ICUMU
              IREVERSED = INIVOL(KK)%CONTAINER(JJ)%IREVERSED
              VFRAC = INIVOL(KK)%CONTAINER(JJ)%VFRAC

              WRITE(VARNAME,'(A,I0,A,I0,A)') 'INIVOL(',KK ,')%CONTY(',JJ,')%SUBMAT_ID     ='
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IPHASE,0.0_8)             
              WRITE(VARNAME,'(A,I0,A,I0,A)') 'INIVOL(',KK ,')%CONTY(',JJ,')%ICUMU         ='
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ICUMU,0.0_8)             
              WRITE(VARNAME,'(A,I0,A,I0,A)') 'INIVOL(',KK ,')%CONTY(',JJ,')%IREVERSED     ='
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IREVERSED,0.0_8)
              WRITE(VARNAME,'(A,I0,A,I0,A)') 'INIVOL(',KK ,')%CONTY(',JJ,')%VFRAC         ='
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,VFRAC)                             

            ENDDO! next JJ (next line)            

          ENDDO!next KK (next option)

      ENDIF                                                                             
C-----------------------------------------------
      RETURN
      END

