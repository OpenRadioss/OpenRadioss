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
C.../VECPX/
C$PAR INSTANCE TASK /VECPX/
#include      "task_common.inc"
      task_common /VECPX/ PX1(MVSIZ),PX2(MVSIZ),PX3(MVSIZ),PX4(MVSIZ),
     .   PX5(MVSIZ), PX6(MVSIZ), PX7(MVSIZ), PX8(MVSIZ), PY1(MVSIZ),
     .   PY2(MVSIZ), PY3(MVSIZ), PY4(MVSIZ), PY5(MVSIZ), PY6(MVSIZ),
     .   PY7(MVSIZ), PY8(MVSIZ), PZ1(MVSIZ), PZ2(MVSIZ), PZ3(MVSIZ),
     .   PZ4(MVSIZ), PZ5(MVSIZ), PZ6(MVSIZ), PZ7(MVSIZ), PZ8(MVSIZ)
c$dir thread_private(/VECPX/)
!IBM* THREADLOCAL /VECPX/
!$OMP THREADPRIVATE(/VECPX/)
C     REAL ou REAL*8
      my_real
     .   PX1, PX2, PX3, PX4, PX5, PX6, PX7, PX8, PY1, PY2, PY3, PY4,
     .   PY5, PY6, PY7, PY8, PZ1, PZ2, PZ3, PZ4, PZ5, PZ6, PZ7, PZ8
