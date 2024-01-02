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
Chd|====================================================================
Chd|  SMS_MOD                       share/modules1/sms_mod.F      
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|        NOD_SMS_DT_SOL                source/ams/sms_auto_dt.F      
Chd|-- calls ---------------
Chd|====================================================================
      MODULE SMS_MOD
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
C-----------------------------------------------
      INTEGER, DIMENSION(:),ALLOCATABLE :: TAGPRT_SMS, TAGREL_SMS,
     .                      TAGSLV_RBY_SMS, TAG_LNK_SMS, NRWL_SMS,
     .                      TAGMSR_RBY_SMS, NATIV_SMS
      INTEGER, DIMENSION(:),ALLOCATABLE :: 
     .                      JAD_SMS, JDI_SMS, JSM_SMS, LAD_SMS, 
     .                      IAD_SMS, IDI_SMS,
     .                      KAD_SMS, KDI_SMS, PK_SMS,
     .                      JADC_SMS, JADS_SMS, JADS10_SMS, JADT_SMS,
     .                      JADP_SMS, JADR_SMS, JADTG_SMS, JADRB_SMS
C-----------------------------------------------
      END MODULE SMS_MOD
