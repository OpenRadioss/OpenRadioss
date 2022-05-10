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
C.../COM10/
      INTEGER LCOM10
Csm41p3 PARAMETER (LCOM10 = 13)      
cow41p17      PARAMETER (LCOM10 = 32)
Cvf50c2      PARAMETER (LCOM10 = 33)
Cvf51e1      PARAMETER (LCOM10 = 34)
      PARAMETER (LCOM10 = 35)
C
      COMMON /COM10/ NSNOD, NSELQ, NSELS, NSELC, NSELT, NSELP, NSELR,
     .   NSRBY, NSMAT, NSELTG, NTHGRP, NTHGRP0, NSELU,  NTHGRP01(10),
     .   NTHGRP1(10), NSFXBY, NSMOD
Cvf51e1     .   NTHGRP1(10), NSFXBY
Cvf50c2     .   NTHGRP1(10)
Csm41p3     .   NSRBY, NSMAT, NSELTG, NTHGRP, NTHGRP0, NSELU
      INTEGER NSNOD, NSELQ, NSELS, NSELC, NSELT, NSELP, NSELR, NSRBY,
     .   NSMAT, NSELTG, NTHGRP, NTHGRP0, NSELU, NTHGRP01,NTHGRP1,
     .   NSFXBY, NSMOD
Cvf51e1     .   NSFXBY
Cvf50c2     .   NSMAT, NSELTG, NTHGRP, NTHGRP0, NSELU, NTHGRP01,NTHGRP1
Csm41p3.   NSMAT, NSELTG, NTHGRP, NTHGRP0, NSELU
