Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
      COMMON /USERLIB/
     .   USERL_AVAIL,GOT_DUSERL,DLIBFILE_SIZE,DLIBTKVERS,
     *   LEN_RAD_INPUTNAME,GOT_USERL_ALTNAME,LEN_USERL_ALTNAME,GETUNOD_NOCOM,MDS_AVAIL,MDS_VER,
     *   MDS_PATH_LEN

      INTEGER USERL_AVAIL,GOT_DUSERL,DLIBFILE_SIZE,
     .         DLIBTKVERS, LEN_RAD_INPUTNAME,GOT_USERL_ALTNAME,LEN_USERL_ALTNAME,GETUNOD_NOCOM,MDS_AVAIL,MDS_VER,
     .         MDS_PATH_LEN

      COMMON /USERLIB_NAM/
     . DLIBFILE,RAD_INPUTNAME,USERL_ALTNAME,MDS_PATH
       CHARACTER DLIBFILE*2048,RAD_INPUTNAME*2048,USERL_ALTNAME*2096
       CHARACTER(len=4096) MDS_PATH

