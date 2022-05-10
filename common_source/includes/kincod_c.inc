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
Cmw41j8.../KINCOD/
Cmw41p2      COMMON /KINCOD/ IBC,ITF,IWL,IRB,IRB2,IVF,IRV,IJO,IRBM
Cmw41p2      INTEGER IBC(0:511), ITF(0:511), IWL(0:511),IRB(0:511),IRB2(0:511),  
Cmw41p2     .        IVF(0:511), IRV(0:511), IJO(0:511),IRBM(0:511)
Cpm41q06      COMMON /KINCOD/ IBC,ITF,IWL,IRB,IRB2,IVF,IRV,IJO,IRBM,ILMULT
Cpm41q06      INTEGER IBC(0:1023), ITF(0:1023), IWL(0:1023),IRB(0:1023),  
Cpm41q06     .        IRB2(0:1023),IVF(0:1023), IRV(0:1023), IJO(0:1023),
Cpm41q06     .        IRBM(0:1023),ILMULT(0:1023)
      COMMON /KINCOD/ IBC,ITF,IWL,IRB,IRB2,IVF,IRV,IJO,IRBM,ILMULT,
     .                IRLK,IKRBE2,IKRBE3
      INTEGER IBC(0:8191), ITF(0:8191), IWL(0:8191),IRB(0:8191),  
     .        IRB2(0:8191),IVF(0:8191), IRV(0:8191), IJO(0:8191),
     .        IRBM(0:8191),ILMULT(0:8191), IRLK(0:8191), IKRBE2(0:8191),
     .        IKRBE3(0:8191)
