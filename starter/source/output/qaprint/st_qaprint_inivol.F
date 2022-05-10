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
Chd|  ST_QAPRINT_INIVOL             source/output/qaprint/st_qaprint_inivol.F
Chd|-- called by -----------
Chd|        ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INIVOL_ARRAY_MOD              share/modules1/inivol_mod.F   
Chd|        INIVOL_DEF_MOD                share/modules1/inivol_mod.F   
Chd|====================================================================
      SUBROUTINE ST_QAPRINT_INIVOL()
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE QA_OUT_MOD
        USE GROUPDEF_MOD
        USE INIVOL_DEF_MOD
        USE INIVOL_ARRAY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr10_c.inc"
#include      "tabsiz_c.inc"
#include      "units_c.inc"
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
      INTEGER IPHASE,ICUMU ,IFILL
      DOUBLE PRECISION VFRAC 
      CHARACTER*nchartitle TITLE
      DOUBLE PRECISION TEMP_DOUBLE      
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      OK_QA = MYQAKEY('/INIVOL')  
                                                         
      IF (OK_QA) THEN   
                                                                      
          DO KK=1,NINIVOL
          
            ID = USER_INIVOL(KK)%ID
            TITLE = USER_INIVOL(KK)%TITLE
            NBCONTY = USER_INIVOL(KK)%NBCONTY
            IPARTFILL = USER_INIVOL(KK)%IPARTFILL
            
            WRITE(VARNAME,'(A,I0,A)') 'USER_INIVOL(',KK ,')%ID                  ='              
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ID,0.0_8)             
            WRITE(VARNAME,'(A,I0,A,A)') 'USER_INIVOL(',KK ,')%TITLE               =',TITLE(1:LEN_TRIM(TITLE))
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,0.0_8)             
            WRITE(VARNAME,'(A,I0,A)') 'USER_INIVOL(',KK ,')%NBCONTY             ='              
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NBCONTY,0.0_8)             
            WRITE(VARNAME,'(A,I0,A)') 'USER_INIVOL(',KK ,')%IPARTFILLE          ='              
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IPARTFILL,0.0_8)
            
            DO JJ=1,NBCONTY

              IPHASE = USER_INIVOL(KK)%CONTY(JJ)%IPHASE
              ICUMU = USER_INIVOL(KK)%CONTY(JJ)%ICUMU
              IFILL = USER_INIVOL(KK)%CONTY(JJ)%IFILL 
              VFRAC = USER_INIVOL(KK)%CONTY(JJ)%FILL_RATIO 

              WRITE(VARNAME,'(A,I0,A,I0,A)') 'USER_INIVOL(',KK ,')%CONTY(',JJ,')%IPHASE     ='              
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IPHASE,0.0_8)             
              WRITE(VARNAME,'(A,I0,A,I0,A)') 'USER_INIVOL(',KK ,')%CONTY(',JJ,')%ICUMU      ='              
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ICUMU,0.0_8)             
              WRITE(VARNAME,'(A,I0,A,I0,A)') 'USER_INIVOL(',KK ,')%CONTY(',JJ,')%IFILL      ='              
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IFILL,0.0_8)             
              WRITE(VARNAME,'(A,I0,A,I0,A)') 'USER_INIVOL(',KK ,')%CONTY(',JJ,')%RATIO_FILL ='              
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,VFRAC)                             
            
            ENDDO! next JJ (next line)            
            
          ENDDO!next KK (next option)
                
      ENDIF                                                                             
C-----------------------------------------------
      RETURN
      END

