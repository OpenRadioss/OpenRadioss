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
Chd|  ST_QAPRINT_MADYMO             source/output/qaprint/st_qaprint_madymo.F
Chd|-- called by -----------
Chd|        ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE ST_QAPRINT_MADYMO(IEXMAD,ITAB,IPART,IXS,IXC,IXTG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE QA_OUT_MOD
        USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr10_c.inc"
#include      "scr17_c.inc"
#include      "tabsiz_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: IEXMAD(*)
      INTEGER, INTENT(IN) :: ITAB(*)
      INTEGER, INTENT(IN) :: IPART(LIPART1,*)
      INTEGER, INTENT(IN) :: IXS(NIXS,*)
      INTEGER, INTENT(IN) :: IXC(NIXC,*)
      INTEGER, INTENT(IN) :: IXTG(NIXTG,*)
C--------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: OK_QA
      CHARACTER (LEN=255) :: VARNAME
      INTEGER I
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IF (MYQAKEY('/MADYMO') ) THEN 
        DO I=1,NMADPRT           
          WRITE(VARNAME,'(A,I0)') 'MADYMO_PART_',I                  
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IPART(4,IEXMAD(I)),0.0_8)                                                          
        ENDDO              
C            
        DO I=NMADPRT+1,NMADPRT+NMADSH4
          WRITE(VARNAME,'(A,I0)') 'MADYMO_SH4_',I                  
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IXC(NIXC,IEXMAD(I)),0.0_8)                                                          
        ENDDO      
C                
        DO I=NMADPRT+NMADSH4+1,NMADPRT+NMADSH4+NMADSH3
          WRITE(VARNAME,'(A,I0)') 'MADYMO_SH3_',I                  
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IXTG(NIXTG,IEXMAD(I)),0.0_8)                                                          
        ENDDO     
C                 
        DO I=NMADPRT+NMADSH4+NMADSH3+1,NMADPRT+NMADSH4+NMADSH3+NMADSOL
          WRITE(VARNAME,'(A,I0)') 'MADYMO_SOL_',I                  
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IXS(NIXS,IEXMAD(I)),0.0_8)                                                         
        ENDDO     
C                            
        DO I=NMADPRT+NMADSH4+NMADSH3+NMADSOL+1,NMADPRT+NMADSH4+NMADSH3+NMADSOL+NMADNOD
          WRITE(VARNAME,'(A,I0)') 'MADYMO_NOD_',I                  
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ITAB(IEXMAD(I)),0.0_8)                                                        
        ENDDO                                                               
      ENDIF                
C-----------------------------------------------
      RETURN
      END

