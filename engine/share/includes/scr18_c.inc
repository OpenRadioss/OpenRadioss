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
C.../SCR18/
C...recopie par defaut en spmd
      COMMON /SCR18R/ DTFAC1(102),DTMIN1(102),DT_THERM,MIN_ASPECT,MIN_DEFV,PERCENT_ADDMASS,
     .    DT_STOP_PERCENT_ADDMASS,MASS0_START,PERCENT_ADDMASS_OLD
      my_real
     .    DTFAC1,DTMIN1,DT_THERM,MIN_ASPECT,MIN_DEFV,PERCENT_ADDMASS,DT_STOP_PERCENT_ADDMASS,MASS0_START,
     .    PERCENT_ADDMASS_OLD
      COMMON /SCR18/ IDTMIN(102),IDTGR(102),KDTINT,KDTSMSTR,IDT_THERM,NODADT_THERM,IDT_PERCENT_ADDMASS
      INTEGER IDTMIN,IDTGR,KDTINT,KDTSMSTR,IDT_THERM,NODADT_THERM,IDT_PERCENT_ADDMASS
