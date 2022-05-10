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
C.../VECFX/
C$PAR INSTANCE TASK /VECFX/
#include      "task_common.inc"
      task_common /VECFX/ F11(MVSIZ),F12(MVSIZ),F13(MVSIZ),F14(MVSIZ),
     .   F15(MVSIZ), F16(MVSIZ), F17(MVSIZ), F18(MVSIZ), F21(MVSIZ),
     .   F22(MVSIZ), F23(MVSIZ), F24(MVSIZ), F25(MVSIZ), F26(MVSIZ),
     .   F27(MVSIZ), F28(MVSIZ), F31(MVSIZ), F32(MVSIZ), F33(MVSIZ),
     .   F34(MVSIZ), F35(MVSIZ), F36(MVSIZ), F37(MVSIZ), F38(MVSIZ)
c$dir thread_private(/VECFX/)
!IBM* THREADLOCAL /VECFX/
!$OMP THREADPRIVATE(/VECFX/)
C     REAL ou REAL*8
      my_real
     .   F11, F12, F13, F14, F15, F16, F17, F18, F21, F22, F23, F24,
     .   F25, F26, F27, F28, F31, F32, F33, F34, F35, F36, F37, F38
