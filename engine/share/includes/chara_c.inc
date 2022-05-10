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
#include "nchara_c.inc"
C.../CHARA/
      COMMON /CHARA/ ROOTNAM, CHRUN, CHRUN0,INPUT ,ROOTN,PATH,RVERS,IMVW_REFPATH
      COMMON /CHARI/ ROOTLEN,MULTIREST
      INTEGER ROOTLEN,MULTIREST
      CHARACTER ROOTNAM*80, CHRUN*4, CHRUN0*2,INPUT*256 ,ROOTN*80,
     *          PATH*2048,RVERS*32, IMVW_REFPATH*ncharline

      COMMON /LENTIT/ LENINPUT,LENROOTN,GOT_INPUT,RUNN,LENPATH,GOT_PATH
      INTEGER LENINPUT,LENROOTN,GOT_INPUT,RUNN,LENPATH,GOT_PATH
