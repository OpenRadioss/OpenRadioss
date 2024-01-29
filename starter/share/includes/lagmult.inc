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
C.../LGMULTI/
      COMMON /LGMULTI/ LAG_NCF  ,LAG_NKF  ,LAG_NHF  ,LAG_NCL  ,LAG_NKL ,
     .                 LAG_NHL  ,NBCSLAG  ,NRBYLAG  ,LMPC     ,NFVLAG  ,
     .                 NRWLAG   ,NHMAX    ,LAGMOD   ,LAGOPT
      INTEGER          LAG_NCF  ,LAG_NKF  ,LAG_NHF  ,LAG_NCL  ,LAG_NKL ,
     .                 LAG_NHL  ,NBCSLAG  ,NRBYLAG  ,LMPC     ,NFVLAG  ,
     .                 NRWLAG   ,NHMAX    ,LAGMOD   ,LAGOPT
Cmw41p11     .                 NRWLAG
Cmw41p4     .                 LAG_NHL  ,NBCSLAG  ,NRBYLAG  ,LMPC     ,NFVLAG  
Cmw41p3      .                 LAG_NHL  ,NBCSLAG  ,NRBYLAG  ,LMPC
Cmw41p1      .                 LAG_NHL  ,NBCSLAG  ,LMPC
Cmw41n11     .                 LAG_NHL  ,NBCSLAG
Cmw41p11++
C.../LGMULTR/
      COMMON /LGMULTR/ LAGM_TOL ,LAG_ALPH ,LAG_ALPHS
      my_real
     .                 LAGM_TOL ,LAG_ALPH ,LAG_ALPHS 
