Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
      !||====================================================================
      !||    sms_mod   ../engine/share/modules/sms_mod.F
      !||--- called by ------------------------------------------------------
      !||    resol     ../engine/source/engine/resol.F
      !||    wrrestp   ../engine/source/output/restart/wrrestp.F
      !||====================================================================
      MODULE SMS_MOD
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
C-----------------------------------------------
      my_real,
     .         DIMENSION(:), ALLOCATABLE :: LT_SMS, LTK_SMS,
     .                                      MSKYI_SMS, LTI_SMS,
     .                                      PREC_SMS
      INTEGER, DIMENSION(:), ALLOCATABLE :: 
     .                 INDX1_SMS, INDX2_SMS
      INTEGER, TARGET, DIMENSION(:), ALLOCATABLE :: 
     .                 NATIV_SMS, TAGNOD_SMS, NODXI_SMS, NODII_SMS
      INTEGER, DIMENSION(:),ALLOCATABLE :: TAGPRT_SMS, TAGREL_SMS,
     .                      TAGSLV_RBY_SMS, TAGMSR_RBY_SMS,
     .                      TAG_LNK_SMS, NRWL_SMS,
     .                      TAGMSR_I21_SMS,TAGSLV_I21_SMS,JADI21_SMS
      INTEGER, DIMENSION(:),ALLOCATABLE :: JAD_SMS, JDI_SMS, JSM_SMS,
     .                      IAD_SMS, IDI_SMS,
     .                      JADI_SMS, JDII_SMS,
     .                      LAD_SMS, KAD_SMS, KDI_SMS, PK_SMS,
     .                      JADC_SMS, JADS_SMS, JADS10_SMS, JADT_SMS,
     .                      JADP_SMS, JADR_SMS, JADTG_SMS
      my_real,
     .        DIMENSION(:,:), ALLOCATABLE :: X_SMS ,P_SMS ,Z_SMS ,Y_SMS,
     .                                       XMOM_SMS
      my_real,
     .        DIMENSION(:,:), ALLOCATABLE :: DIAG_SMS3, PREC_SMS3
      INTEGER, DIMENSION(:,:),ALLOCATABLE :: ISKYI_SMS
C-----------------------------------------------
      END MODULE SMS_MOD
      !||====================================================================
      !||    sms_pcg_proj   ../engine/share/modules/sms_mod.F
      !||--- called by ------------------------------------------------------
      !||    resol          ../engine/source/engine/resol.F
      !||    sms_inisi      ../engine/source/ams/sms_proj.F
      !||    sms_inist      ../engine/source/ams/sms_proj.F
      !||    sms_inix       ../engine/source/ams/sms_proj.F
      !||    sms_pcg        ../engine/source/ams/sms_pcg.F
      !||    sms_pro_p      ../engine/source/ams/sms_proj.F
      !||    sms_updst      ../engine/source/ams/sms_proj.F
      !||====================================================================
      MODULE SMS_PCG_PROJ
#include      "my_real.inc"
      my_real,
     .         DIMENSION(:,:),ALLOCATABLE :: PROJ_S,PROJ_T
      my_real,
     .         DIMENSION(:,:),ALLOCATABLE :: PROJ_K
      my_real,
     .         DIMENSION(:),ALLOCATABLE :: PROJ_LA_1,PROJ_W
 
      END MODULE SMS_PCG_PROJ
