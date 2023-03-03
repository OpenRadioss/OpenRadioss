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
Chd|  SORTHO31                      source/output/anim/sortho31.F 
Chd|-- called by -----------
Chd|        SROTA6                        source/output/anim/srota6.F   
Chd|        SROTA6_M1                     source/output/anim/srota6_M1.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SORTHO31(                                              
     .   X1, X2, X3, X4, X5, X6, X7, X8,                                
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,                                
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,                                
     .   E1X, E2X, E3X, E1Y, E2Y, E3Y, E1Z, E2Z, E3Z )                  
C-----------------------------------------------                        
C   I m p l i c i t   T y p e s                                         
C-----------------------------------------------                        
#include      "implicit_f.inc"                                          
C-----------------------------------------------                        
C   D u m m y   A r g u m e n t s                                       
C-----------------------------------------------                        
      my_real                                                           
     .   X1, X2, X3, X4, X5, X6, X7, X8,                                
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,                                
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,                                
     .   E1X, E1Y, E1Z,                                                 
     .   E2X, E2Y, E2Z,                                                 
     .   E3X, E3Y, E3Z                                                  
C-----------------------------------------------                        
C   L o c a l   V a r i a b l e s                                       
C-----------------------------------------------                        
      INTEGER N,NITER                                                   
      my_real                                                           
     .   X17 , X28 , X35 , X46,                                         
     .   Y17 , Y28 , Y35 , Y46,                                         
     .   Z17 , Z28 , Z35 , Z46,                                         
     .   A17 , A28 ,                                                    
     .   B17 , B28 ,                                                    
     .   C17 , C28 ,                                                    
     .   rx , ry , rz ,                                                 
     .   sx , sy , sz ,                                                 
     .   tx , ty , tz                                                   
      my_real                                                           
     .   aa,bb                                                          
      DATA NITER/3/                                                     
C-----------------------------------------------                        
      X17=X7-X1                                                         
      X28=X8-X2                                                         
      X35=X5-X3                                                         
      X46=X6-X4                                                         
      Y17=Y7-Y1                                                         
      Y28=Y8-Y2                                                         
      Y35=Y5-Y3                                                         
      Y46=Y6-Y4                                                         
      Z17=Z7-Z1                                                         
      Z28=Z8-Z2                                                         
      Z35=Z5-Z3                                                         
      Z46=Z6-Z4                                                         
      RX=X17+X28-X35-X46                                                
      RY=Y17+Y28-Y35-Y46                                                
      RZ=Z17+Z28-Z35-Z46                                                
      A17=X17+X46                                                       
      A28=X28+X35                                                       
      B17=Y17+Y46                                                       
      B28=Y28+Y35                                                       
      C17=Z17+Z46                                                       
      C28=Z28+Z35                                                       
      SX=A17+A28                                                        
      SY=B17+B28                                                        
      SZ=C17+C28                                                        
      TX=A17-A28                                                        
      TY=B17-B28                                                        
      TZ=C17-C28                                                        
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                       
c     norme r s t                                                       
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                       
      aa = sqrt(rx*rx + ry*ry + rz*rz)                                  
      if ( aa/=ZERO) aa = ONE / aa                                     
      rx = rx * aa                                                      
      ry = ry * aa                                                      
      rz = rz * aa                                                      
      aa = sqrt(sx*sx + sy*sy + sz*sz)                                  
      if ( aa/=ZERO) aa = ONE / aa                                     
      sx = sx * aa                                                      
      sy = sy * aa                                                      
      sz = sz * aa                                                      
      aa = sqrt(tx*tx + ty*ty + tz*tz)                                  
      if ( aa/=ZERO) aa = ONE / aa                                     
      tx = tx * aa                                                      
      ty = ty * aa                                                      
      tz = tz * aa                                                      
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                       
c     iterations                                                        
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                       
      N=0                                                               
111   CONTINUE                                                          
      N=N+1                                                             
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                       
       e1x = sy * tz - sz * ty + rx                                     
       e1y = sz * tx - sx * tz + ry                                     
       e1z = sx * ty - sy * tx + rz                                     
c                                                                       
       e2x = ty * rz - tz * ry + sx                                     
       e2y = tz * rx - tx * rz + sy                                     
       e2z = tx * ry - ty * rx + sz                                     
c                                                                       
       e3x = ry * sz - rz * sy + tx                                     
       e3y = rz * sx - rx * sz + ty                                     
       e3z = rx * sy - ry * sx + tz                                     
c                                                                       
       bb = sqrt(e1x*e1x + e1y*e1y + e1z*e1z)                           
       if ( bb/=ZERO) bb = ONE / bb                                    
       rx = e1x * bb                                                    
       ry = e1y * bb                                                    
       rz = e1z * bb                                                    
c                                                                       
       bb = sqrt(e2x*e2x + e2y*e2y + e2z*e2z)                           
       if ( bb/=ZERO) bb = ONE / bb                                    
       sx = e2x * bb                                                    
       sy = e2y * bb                                                    
       sz = e2z * bb                                                    
c                                                                       
       bb = sqrt(e3x*e3x + e3y*e3y + e3z*e3z)                           
       if ( bb/=ZERO) bb = ONE / bb                                    
       tx = e3x * bb                                                    
       ty = e3y * bb                                                    
       tz = e3z * bb                                                    
c                                                                       
      IF (N<NITER) GOTO 111                                          
c     norme et orthogonalisation                                        
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                       
      e1x = rx                                                          
      e1y = ry                                                          
      e1z = rz                                                          
c                                                                       
      e3x = e1y * sz - e1z * sy                                         
      e3y = e1z * sx - e1x * sz                                         
      e3z = e1x * sy - e1y * sx                                         
c                                                                       
      aa = sqrt(e3x*e3x + e3y*e3y + e3z*e3z)                            
      if ( aa/=ZERO) aa = ONE / aa                                     
      e3x = e3x * aa                                                    
      e3y = e3y * aa                                                    
      e3z = e3z * aa                                                    
c                                                                       
      e2x = e3y * e1z - e3z * e1y                                       
      e2y = e3z * e1x - e3x * e1z                                       
      e2z = e3x * e1y - e3y * e1x                                       
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                       
      RETURN                                                            
      END                                                               
