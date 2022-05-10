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
C.../SCR01/
C$PAR INSTANCE TASK /SCR01/
#include      "task_common.inc"
      task_common /SCR01/ NB1,NB2,NB3,NB4,NB5,NB6,NB7,NB8,NB9,NB10,
     .   NB11, NB12, NB13, NB14, NB15, NB16, NB17, NB18, NB19, NB20,
     .   NB21,NB22,NBFI, ITASKP1, NBFAIL, NBIPFAIL
Cpm51e01     .   NB21,NB22,NBFI, ITASKP1
c$dir thread_private(/SCR01/)
!IBM* THREADLOCAL /SCR01/
!$OMP THREADPRIVATE(/SCR01/)
      INTEGER NB1, NB2, NB3, NB4, NB5, NB6, NB7, NB8, NB9, NB10, NB11,
     .   NB12, NB13,NB14,NB15,NB16,NB17,NB18,NB19,NB20,NB21,NB22,
     .   NBFI, NBFAIL, NBIPFAIL
Cpm51e01     .   NBFI
Cmw50c13     .   NB12, NB13, NB14, NB15, NB16, NB17, NB18, NB19, NB20,NBFI
Cgw41p8+1
      INTEGER ITASKP1
