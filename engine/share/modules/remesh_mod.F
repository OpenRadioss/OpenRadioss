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
Chd|====================================================================
Chd|  REMESH_MOD                    share/modules/remesh_mod.F    
Chd|-- called by -----------
Chd|        ADMDIV                        source/model/remesh/admdiv.F  
Chd|        ADMFOR0                       source/model/remesh/admfor0.F 
Chd|        ADMGVID                       source/model/remesh/admgvid.F 
Chd|        ADMINI                        source/model/remesh/admini.F  
Chd|        ADMMAP3                       source/model/remesh/admmap3.F 
Chd|        ADMMAP4                       source/model/remesh/admmap4.F 
Chd|        ADMNORM3                      source/model/remesh/admnorm.F 
Chd|        ADMNORM4                      source/model/remesh/admnorm.F 
Chd|        ADMORDR                       source/model/remesh/admordr.F 
Chd|        ADMREGUL                      source/model/remesh/admregul.F
Chd|        ADMTHKE                       source/model/remesh/admthke.F 
Chd|        ADMVIT                        source/model/remesh/admvit.F  
Chd|        CNDINT                        source/model/remesh/cndint.F  
Chd|        CNDORDR                       source/model/remesh/cndordr.F 
Chd|        RESOL                         source/engine/resol.F         
Chd|        RMDIM_IMP                     source/model/remesh/rm_imp0.F 
Chd|        RMIND_IMP                     source/model/remesh/rm_imp0.F 
Chd|        RM_IMP0                       source/model/remesh/rm_imp0.F 
Chd|        RM_IMP2                       source/model/remesh/rm_imp0.F 
Chd|        SMS_ADMESH_0                  source/ams/sms_admesh.F       
Chd|        SMS_ADMESH_1                  source/ams/sms_admesh.F       
Chd|        SMS_ADMESH_2                  source/ams/sms_admesh.F       
Chd|        TAGOFF3N                      source/interfaces/interf/chkstfn3.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE REMESH_MOD
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
C-----------------------------------------------
      INTEGER
     .       , DIMENSION(:), ALLOCATABLE :: MSH4SKY,MSH3SKY
C-----------------------------------------------
C     Liste des coques actives dans les parts 
C          ou l'on fait de l'adaptive meshing.
C-----------------------------------------------
      INTEGER, DIMENSION(:),ALLOCATABLE :: LSH4ACT,LSH4KIN,
     .                                     PSH4ACT,PSH4KIN,
     .                                     LSH3ACT,LSH3KIN,
     .                                     PSH3ACT,PSH3KIN
      INTEGER NSH4ACT,NSH4OLD,NSH3ACT,NSH3OLD
C-----------------------------------------------
C     Liste des coques aux niveaux superieurs dans les parts 
C          ou l'on fait de l'adaptive meshing.
C-----------------------------------------------
      INTEGER, DIMENSION(:),ALLOCATABLE :: LSH4UPL,LSH3UPL,
     .                                     PSH4UPL,PSH3UPL
C-----------------------------------------------
C     Work arrays
C-----------------------------------------------
      INTEGER, DIMENSION(:),ALLOCATABLE :: ILEVNOD
      INTEGER, DIMENSION(:),ALLOCATABLE :: TAGNOD
      my_real, 
     .         DIMENSION(:,:),ALLOCATABLE :: NODNORM
      my_real, 
     .         DIMENSION(:,:),ALLOCATABLE :: ACND,ARCND
      my_real, 
     .         DIMENSION(:),ALLOCATABLE :: STCND,STRCND
C-----------------------------------------------
C     Liste des neouds kinematiques pour implicite 
C     ISH_NS(NSH_KIN),ISH_MS(2,NSH_KIN),IAD_NJ(NSH_KIN+1),JDI_NJ(IAD_NJ(NSH_KIN+1)-1)
C-----------------------------------------------
      INTEGER, DIMENSION(:),ALLOCATABLE :: ISH_NS,
     .                                     IAD_NJ,JDI_NJ
      INTEGER, DIMENSION(:,:),ALLOCATABLE :: ISH_MS
      INTEGER NSH_KIN

      END MODULE REMESH_MOD
