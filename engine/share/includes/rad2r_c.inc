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
C.../RAD2R/
Cmwdev004      
      COMMON /RAD2R/IRAD2R,R2R_ACTIV,FLG_RST,NBK,TMAN_R2R,
     .        R2R_MFILR,R2R_CTR,R2R_MSTOP,SOCK0,R2R_SIU,
     .        IDDOM,NUMMAT0,NUMGEO0,SEEK0,SEEKC,SEEK_FLAG,
     .        R2R_TH_FLAG,R2R_TH_MASTER,FLAG_KINE,MASTER_SIDE             
      INTEGER IRAD2R,R2R_ACTIV,FLG_RST,NBK,SOCK0
      INTEGER R2R_MFILR,R2R_MSTOP,R2R_CTR(3),R2R_SIU
      INTEGER IDDOM,NUMMAT0,NUMGEO0,SEEK0(10),SEEKC(10)
      INTEGER SEEK_FLAG(10),R2R_TH_FLAG(10),R2R_TH_MASTER(10)
      INTEGER FLAG_KINE,MASTER_SIDE
      my_real
     .       TMAN_R2R
                
Cmw51e2      INTEGER IRAD2R
