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
C.../ALEFVM/
      COMMON /ALEFVM/   IALEFVM             , I_ALE_SOLVER       ,
     .                  IALEFVM_OUTP        , IALEFVM_OUTP_GRAV  , IALEFVM_OUTP_STRESS, IALEFVM_OUTP_FINT, 
     .                  IALEFVM_OUTP_FLUX   , IALEFVM_OUTP_SCHEME, IALEFVM_OUTP_CONV  , IALEFVM_IFORM    ,
     .                  IALEFVM_OUTP_EPSDOT , IALEFVM_OUTP_BCS   , IALEFVM_OUTP_TFEXT,  IALEFVM_WFEXT

      INTEGER,TARGET :: IALEFVM             , I_ALE_SOLVER       ,
     .                  IALEFVM_OUTP        , IALEFVM_OUTP_GRAV  , IALEFVM_OUTP_STRESS, IALEFVM_OUTP_FINT, 
     .                  IALEFVM_OUTP_FLUX   , IALEFVM_OUTP_SCHEME, IALEFVM_OUTP_CONV  , IALEFVM_IFORM    ,
     .                  IALEFVM_OUTP_EPSDOT , IALEFVM_OUTP_BCS   , IALEFVM_OUTP_TFEXT , IALEFVM_WFEXT
