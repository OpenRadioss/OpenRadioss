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
C.../MMALE51/

      INTEGER N0PHAS
      PARAMETER (N0PHAS = 04)
      
      INTEGER NVPHAS
      PARAMETER (NVPHAS = 23) 
           
      INTEGER M51_IFLG6_SIZE
      PARAMETER (M51_IFLG6_SIZE = 37)
      
      COMMON /MMALE51/ SSP0MAX, LC0MAX, TCP_REF
      DOUBLE PRECISION SSP0MAX, LC0MAX, TCP_REF

      COMMON /MMALE51/ M51_IFLG6,lSET_51_IFLG6, ILOOP_NRF51
      INTEGER M51_IFLG6, lSET_51_IFLG6, ILOOP_NRF51
