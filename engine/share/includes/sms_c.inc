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
C-------------------------------------------------------------------------------
C.../SMS/
      INTEGER NDDL_SMS,KNZ_SMS,NNZ_SMS,NINDX1_SMS,NINDX2_SMS,NRB_SMS,
     .        IDTMINS,IDTGRS,IDTMINS_OLD,IDTGRS_OLD,NSMSPCG,IFRICW,
     .        ISMSNOK,LSKYI_SMS,NCPRISMS,IDTMINS_INT,IDTMINS_INT_OLD,
     .        M_VS_SMS, NCG_RUN_SMS, NUPDTL_SMS, I2NSN25, KFORSMS, ISMS_SELEC
      COMMON /SMS/
     .    NDDL_SMS,KNZ_SMS,NNZ_SMS,NINDX1_SMS,NINDX2_SMS,NRB_SMS,
     .    IDTMINS,IDTGRS,IDTMINS_OLD,IDTGRS_OLD,NSMSPCG,IFRICW,
     .    ISMSNOK,LSKYI_SMS,NCPRISMS,IDTMINS_INT,IDTMINS_INT_OLD,
     .    M_VS_SMS, NCG_RUN_SMS, NUPDTL_SMS, I2NSN25, KFORSMS, ISMS_SELEC
C.../SMSR/
      my_real
     .  RES1_SMS,RES0_SMS, G0_SMS, G1_SMS, S_SMS, TOL_SMS,
     .  DTFACS, DTMINS, DTFACS_OLD, DTMINS_OLD,
     .  DTFACS_INT, DTMINS_INT, DTFACS_INT_OLD, DTMINS_INT_OLD,
     .  P_MACH_SMS, R_N2_SMS, R_N2_SMS1, R_N2_SMS2, R_N2_SMS3
      COMMON /SMSR/ 
     .  RES1_SMS,RES0_SMS, G0_SMS, G1_SMS, S_SMS, TOL_SMS, 
     .  DTFACS, DTMINS, DTFACS_OLD, DTMINS_OLD,
     .  DTFACS_INT, DTMINS_INT, DTFACS_INT_OLD, DTMINS_INT_OLD,
     .  P_MACH_SMS, R_N2_SMS, R_N2_SMS1, R_N2_SMS2, R_N2_SMS3
C.../SMSDP/
      DOUBLE PRECISION
     .  R6SMS(6),G6SMS(6),S6SMS(6),X6SMS(3,6)
      COMMON /SMSDP/ R6SMS, G6SMS, S6SMS, X6SMS
