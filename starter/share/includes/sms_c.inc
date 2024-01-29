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
C-------------------------------------------------------------------------------
C.../SMS/
      INTEGER NDDL_SMS,KNZ_SMS,NNZ_SMS,NINDX1_SMS,NRB_SMS,
     .        IDTMINS,IDTGRS,NSMSPCG,IFRICW,ISMS,
     .        NCPRISMS,IDTMINS_INT,ISMS_SELEC
      COMMON /SMS/
     .    NDDL_SMS,KNZ_SMS,NNZ_SMS,NINDX1_SMS,NRB_SMS,
     .    IDTMINS,IDTGRS,NSMSPCG,IFRICW,ISMS,
     .    NCPRISMS,IDTMINS_INT,ISMS_SELEC
C.../SMSR/
      my_real
     .  DTFACS, DTMINS, DTFACS_INT, DTMINS_INT, TOL_SMS,DT_SMS_SWITCH
      COMMON /SMSR/ DTFACS, DTMINS, DTFACS_INT, DTMINS_INT, TOL_SMS, DT_SMS_SWITCH
