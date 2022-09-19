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
Chd|  ST_QAPRINT_MULTIDOMAINS       source/output/qaprint/st_qaprint_multidomains.F
Chd|-- called by -----------
Chd|        ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|====================================================================
      SUBROUTINE ST_QAPRINT_MULTIDOMAINS(IPART,IEXLNK,IGRNOD)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE QA_OUT_MOD
        USE R2R_MOD
        USE GROUPDEF_MOD
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
#include      "r2r_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: IPART(LIPART1,*),IEXLNK(NR2R,*)
      TYPE (GROUP_)  , INTENT(IN), DIMENSION(NGRNOD)  :: IGRNOD
C--------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: OK_QA
      CHARACTER (LEN=255) :: VARNAME
      INTEGER :: I,J,ID,RLEN
      DOUBLE PRECISION :: TMPVAL
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IF(NR2RLNK + NSUBDOM == 0) RETURN
C
      OK_QA = MYQAKEY('/SUBDOMAIN')
C                                                         
      IF (OK_QA) THEN
         DO I = 1, NSUBDOM
C
            CALL QAPRINT('NB_SUBDOMAIN',NSUBDOM,0.0_8)
            CALL QAPRINT('NB_PART_SUB',NB_PART_SUB,0.0_8)
C
	    RLEN = ISUBDOM(7,I)
            WRITE(VARNAME,'(A)') DOM_NAME(ISUBDOM(8,I):ISUBDOM(8,I)+RLEN-1)
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ISUBDOM(2,I), 0.0_8)
C
            CALL QAPRINT('SUBDOMAIN NB PART',ISUBDOM(1,I),0.0_8)
            DO J=1,ISUBDOM(1,I)
              ID = IPART(4,ISUBDOM_PART(ISUBDOM(3,I)+J))
              CALL QAPRINT('SUBDOMAIN -> PART ID',ID,0.0_8)
            ENDDO
         ENDDO
      ENDIF
C        
      OK_QA = MYQAKEY('/EXT/LINK')
C                                                         
      IF (OK_QA) THEN
         DO I = 1, NR2RLNK
           CALL QAPRINT('EXTLNK ID ',IEXLNK(2,I),0.0_8)
           CALL QAPRINT('EXTLNK GROUP ID ',IGRNOD(IEXLNK(1,I))%ID,0.0_8)
           CALL QAPRINT('EXTLNK ID DOM1 ',IEXLNK(2,I),0.0_8)
           CALL QAPRINT('EXTLNK ID DOM2 ',IEXLNK(3,I),0.0_8)
           CALL QAPRINT('EXTLNK TYPE '   ,IEXLNK(4,I),0.0_8)
         ENDDO
      ENDIF
C-----------------------------------------------
      RETURN
      END

