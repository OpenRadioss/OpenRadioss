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
C
Chd|====================================================================
Chd|  BASIS8                        source/elements/solid/solide8/basis8.F
Chd|-- called by -----------
Chd|        BASISF                        source/elements/solid/solide8/basisf.F
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE BASIS8 (R,S,T,H,PR,PS,PT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL ou REAL*8
      my_real
     .   R, S, T
C     REAL ou REAL*8
      my_real
     .   H(*), PR(*), PS(*), PT(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL ou REAL*8
      my_real
     .   RP, SP, TP, RM, SM, TM
      RP=ONE+R
      SP=ONE+S
      TP=ONE+T
      RM=ONE-R
      SM=ONE-S
      TM=ONE-T
      H(1)=ONE_OVER_8*RM*SM*TM
      H(2)=ONE_OVER_8*RP*SM*TM
      H(3)=ONE_OVER_8*RP*SP*TM
      H(4)=ONE_OVER_8*RM*SP*TM
      H(5)=ONE_OVER_8*RM*SM*TP
      H(6)=ONE_OVER_8*RP*SM*TP
      H(7)=ONE_OVER_8*RP*SP*TP
      H(8)=ONE_OVER_8*RM*SP*TP
      PR(1)=-ONE_OVER_8*SM*TM      
      PR(2)=-PR(1)
      PR(3)= ONE_OVER_8*SP*TM
      PR(4)=-PR(3)
      PR(5)=-ONE_OVER_8*SM*TP
      PR(6)=-PR(5)
      PR(7)= ONE_OVER_8*SP*TP
      PR(8)=-PR(7)
      PS(1)=-ONE_OVER_8*RM*TM
      PS(2)=-ONE_OVER_8*RP*TM
      PS(3)=-PS(2)
      PS(4)=-PS(1)
      PS(5)=-ONE_OVER_8*RM*TP
      PS(6)=-ONE_OVER_8*RP*TP
      PS(7)=-PS(6)
      PS(8)=-PS(5)
      PT(1)=-ONE_OVER_8*RM*SM
      PT(2)=-ONE_OVER_8*RP*SM
      PT(3)=-ONE_OVER_8*RP*SP
      PT(4)=-ONE_OVER_8*RM*SP      
      PT(5)=-PT(1)
      PT(6)=-PT(2)
      PT(7)=-PT(3)
      PT(8)=-PT(4)
C
      RETURN
      END
