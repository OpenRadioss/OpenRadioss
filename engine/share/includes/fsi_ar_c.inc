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
C.../FSI/
C Harold for FSI
      COMMON /IFSI/ IACUSLV,
     .              FSI_EL_SET,FSI_SURF_SET,FSI_PORT,FSI_MINEX,
     .              FSI_MAXEX,FSI_USE_TAG,FSI_DATA,FSI_MSGLEVEL,
     .              FSI_WAIT_TIME,FSI_INIT_TSTEP
      INTEGER IACUSLV,
     .        FSI_EL_SET,FSI_SURF_SET,FSI_PORT,FSI_MINEX,
     .        FSI_MAXEX,FSI_USE_TAG,FSI_DATA,FSI_MSGLEVEL,
     .        FSI_WAIT_TIME,FSI_INIT_TSTEP
      INTEGER LFSI
      PARAMETER(LFSI = 10)
C     REAL ou REAL*8
      COMMON /RFSI/ FSI_DISP_TOL,FSI_FORCE_TOL,FSI_TEMP_DOL,FSI_FLUX_TOL
      my_real
     .        FSI_DISP_TOL,FSI_FORCE_TOL,FSI_TEMP_DOL,FSI_FLUX_TOL
