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
Chd|  DPARRBY                       source/output/anim/dparrby.F  
Chd|-- called by -----------
Chd|        GENANI1                       source/output/anim/genani1.F  
Chd|-- calls ---------------
Chd|        WRITE_I_C                     ../common_source/tools/input_output/write_routtines.c
Chd|====================================================================
      SUBROUTINE DPARRBY(LPBY ,NPBY)                                    
C-----------------------------------------------                        
C   I m p l i c i t   T y p e s                                         
C-----------------------------------------------                        
#include      "implicit_f.inc"                                          
C-----------------------------------------------                        
C   C o m m o n   B l o c k s                                           
C-----------------------------------------------                        
#include      "com04_c.inc"                                             
#include      "param_c.inc"                                             
C-----------------------------------------------                        
C   D u m m y   A r g u m e n t s                                       
C-----------------------------------------------                        
      INTEGER NPBY(NNPBY,*),LPBY(*)                                     
C-----------------------------------------------                        
C   L o c a l   V a r i a b l e s                                       
C-----------------------------------------------                        
      INTEGER I, K, N,                                                  
     .   NSN, II(2)                                                     
C-----------------------------------------------                        
      K=0                                                               
      DO I=1,NRBYKIN                                                    
        NSN = NPBY(2,I)                                                 
        II(1) = NPBY(1,I) - 1                                           
        DO N=1,NSN                                                      
           II(2) = LPBY(N+K)-1                                          
           CALL WRITE_I_C(II,2)                                         
        ENDDO                                                           
        K  = K  + NSN                                                   
      ENDDO                                                             
      DO I=NRBYKIN+1,NRBODY                                             
        NSN = NPBY(2,I)                                                 
        II(1) = NPBY(1,I) - 1                                           
        DO N=1,NSN                                                      
           II(2) = LPBY(N+K)-1                                          
           CALL WRITE_I_C(II,2)                                         
        ENDDO                                                           
        K  = K  + NSN*3                                                 
      ENDDO                                                             
C                                                                       
      RETURN                                                            
      END                                                               
