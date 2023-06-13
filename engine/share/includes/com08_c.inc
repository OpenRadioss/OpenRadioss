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
C.../COM08/
      INTEGER LCOM08
      PARAMETER (LCOM08 = 38) 
C
      COMMON /COM08/ TT, DT1, DT2, DT12, DT2OLD, TSTOP, 
     .               TABFIS(10),TABFWR(10),XMIN_GLOB,XMAX_GLOB,
     .    YMIN_GLOB,YMAX_GLOB,
     .    ZMIN_GLOB,ZMAX_GLOB
      COMMON /COM08DP/TT_DP
C     REAL ou REAL*8
      my_real
     .    TT,DT1, DT2, DT12, DT2OLD, TSTOP, 
     .    TABFIS,TABFWR,XMIN_GLOB,XMAX_GLOB,YMIN_GLOB,YMAX_GLOB,
     .    ZMIN_GLOB,ZMAX_GLOB
      DOUBLE PRECISION TT_DP
