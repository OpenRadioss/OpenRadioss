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
C.../SCR_USENS_C/
Cmw41j4
Cmw41k6+1
      COMMON /SCR_CURSEN/KSENS_CUR
      COMMON /SCR_SECTOR/ISECTOR
      INTEGER ISECTOR, LSENBUF, NSENPARI, NSENPARR, 
     .        ISENBUF, ISENPARI, ISENPARR, KSENS_CUR
      PARAMETER (ISENBUF=2)
      PARAMETER (LSENBUF=101)
      PARAMETER (NSENPARI=12)
      PARAMETER (NSENPARR=20)
      PARAMETER (ISENPARI=3)
      PARAMETER (ISENPARR=203)

