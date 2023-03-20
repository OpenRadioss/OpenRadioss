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
C.../DDSPMDV5/

      COMMON /DDSPMDV5/ TPSREF,TPSENG,
     .  SOL1TNL(0:MAXLAW,3),SOL8TNL(0:MAXLAW,3),
     .  SHTNL(0:MAXLAW,0:3,3),
     .  SOLTELT(20),SHTELT(30),TRITNL(0:MAXLAW,0:3,3),TRITELT(10),
     .  TET4TNL(0:MAXLAW,3),TET10TNL(0:MAXLAW,3),TET4TELT(10),
     .  TET10TELT(10),
     .  RUPTURE_SHELL(MAXFAIL,6),
     .  RUPTURE_SOL(MAXFAIL,3),RUPTURE_SOL12(MAXFAIL,3),
     .  RUPTURE_TET4(MAXFAIL,3),RUPTURE_TET10(MAXFAIL,3),
     .  SHTNL_OPTION(10,5),SOL_OPTION(10,5),NLOCAL_OPTION(10)

      my_real
     .   TPSREF,TPSENG,SOL1TNL,SOL8TNL,
     .   SHTNL,
     .   TRITNL,SOLTELT,SHTELT,TRITELT,
     .   TET4TNL,TET10TNL,TET4TELT,
     .   TET10TELT,
     .   RUPTURE_SHELL,
     .   RUPTURE_SOL,RUPTURE_SOL12,
     .   RUPTURE_TET4,RUPTURE_TET10,
     .   SHTNL_OPTION,SOL_OPTION,NLOCAL_OPTION

