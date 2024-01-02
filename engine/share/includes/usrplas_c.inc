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
C
C.../UPLAS/
#include      "task_common.inc"
      task_common /UPLAS/ U_YELD(MVSIZ),U_ETSE(MVSIZ),U_DEFP(MVSIZ)
c$dir thread_private(/UPLAS/)
!IBM* THREADLOCAL /UPLAS/
!$OMP THREADPRIVATE(/UPLAS/)
C     REAL ou REAL*8
      my_real
     .   U_YELD,U_ETSE,U_DEFP
C
C.../UTAG/
      task_common /UTAG/ U_TAGPLAS(MVSIZ)
c$dir thread_private(/UTAG/)
!IBM* THREADLOCAL /UTAG/
!$OMP THREADPRIVATE(/UTAG/)
C     
      INTEGER U_TAGPLAS
C
C
C.../UVAR/
      task_common /UVAR/ UUVAR(MVSIZ,5000)
c$dir thread_private(/UVAR/)
!IBM* THREADLOCAL /UVAR/
!$OMP THREADPRIVATE(/UVAR/)
      my_real
     .   UUVAR
C     
