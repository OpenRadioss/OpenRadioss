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
Chd|  ST_QAPRINT_DFS_LASERS         source/output/qaprint/st_qaprint_dfs_lasers.F
Chd|-- called by -----------
Chd|        ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE ST_QAPRINT_DFS_LASERS(XLAS,ILAS)
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
#include      "tabsiz_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real, INTENT(IN) :: XLAS(*)
      INTEGER, INTENT(IN) :: ILAS(*)    
C--------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: OK_QA
      CHARACTER (LEN=255) :: VARNAME
      INTEGER K,J,DET_IID,I1,I2,I3,I4,I5,I6
      DOUBLE PRECISION TEMP_DOUBLE      
      INTEGER :: NL,K1,K2,IFUNC,ITYP, IFK, MFK, IAFUNC, NCELL
      DOUBLE PRECISION :: CHALEUR, FSI, XK0, HNUK, DAR_HNUK_2,UN_TIMESCAL, FI, ALPHA
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

      IF (MYQAKEY('LASERS') ) THEN 

        K1 = 1
        K2 = 1
      
        DO K=1,NLASER

          NL            = ILAS(K1)  
          IFUNC         = ILAS(K1+3) 
          IAFUNC        = ILAS(K1+4) 
          ITYP          = ILAS(K1+5) 

          CHALEUR       = XLAS(K2)   
          FI            = XLAS(K2+1) 
          ALPHA         = XLAS(K2+2) 

          XK0           = XLAS(K2+3) 
          HNUK          = XLAS(K2+4) 
          DAR_HNUK_2    = XLAS(K2+5) 
          UN_TIMESCAL   = XLAS(K2+6) 

                           
          WRITE(VARNAME,'(A,I0,A,I0,A)') 'LASER(',K ,')%ILAS(',K1,')=NL :'                  
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ILAS(K1),0.0_8)                        
          WRITE(VARNAME,'(A,I0,A,I0,A)') 'LASER(',K ,')%ILAS(',K1+3,')=IFUNC :'               
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ILAS(K1+3),0.0_8)                      
          WRITE(VARNAME,'(A,I0,A,I0,A)') 'LASER(',K ,')%ILAS(',K1+4,')=IAFUNC :'              
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ILAS(K1+4),0.0_8)                      
          WRITE(VARNAME,'(A,I0,A,I0,A)') 'LASER(',K ,')%ILAS(',K1+5,')=ITYP :'                
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ILAS(K1+5),0.0_8)                      

          NCELL=0
          DO J=1,2*NL,2   
            NCELL=NCELL+1                                                                 
            WRITE(VARNAME,'(A,I0,A,I0,A,I0,A)') 'LASER(',K ,')%ILAS(',K1+5+J,')=ELEM(',NCELL,') :'              
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ILAS(K1+5+J),0.0_8)                  
            WRITE(VARNAME,'(A,I0,A,I0,A)') 'LASER(',K ,')%ILAS(',K1+5+J+1,')=NG :'                
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ILAS(K1+5+J+1),0.0_8)                
          ENDDO                                                                            

          WRITE(VARNAME,'(A,I0,A,I0,A,I0,A)') 'LASER(',K ,')%XLAS(',K2,')=CHALEUR :'       
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,CHALEUR)                             
          WRITE(VARNAME,'(A,I0,A,I0,A,I0,A)') 'LASER(',K ,')%XLAS(',K2+1,')=FI :'            
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,FI)                                  
          WRITE(VARNAME,'(A,I0,A,I0,A,I0,A)') 'LASER(',K ,')%XLAS(',K2+2,')=ALPHA :'         
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,ALPHA)                               

          WRITE(VARNAME,'(A,I0,A,I0,A,I0,A)') 'LASER(',K ,')%XLAS(',K2+3,')=XK0 :'           
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,XK0)                                 
          WRITE(VARNAME,'(A,I0,A,I0,A,I0,A)') 'LASER(',K ,')%XLAS(',K2+4,')=HNUK :'          
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,HNUK)                                
          WRITE(VARNAME,'(A,I0,A,I0,A,I0,A)') 'LASER(',K ,')%XLAS(',K2+5,')=DAR_HNUK_2 :'    
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,DAR_HNUK_2)                          
          WRITE(VARNAME,'(A,I0,A,I0,A,I0,A)') 'LASER(',K ,')%XLAS(',K2+6,')=UN_TIMESCAL :'   
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,UN_TIMESCAL)                         

          IFK = 6 + 2*(NL+1)  
          MFK = 7             
          K1 = K1 + IFK       
          K2 = K2 + MFK       
                                                                         
        ENDDO !next K                                                                   
      ENDIF                
C-----------------------------------------------
      RETURN
      END

