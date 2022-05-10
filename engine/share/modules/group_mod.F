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
Chd|  GROUP_MOD                     share/modules/group_mod.F     
Chd|-- called by -----------
Chd|        GROUP_INI                     source/groups/group_ini.F     
Chd|        LINE_INI                      source/groups/line_ini.F      
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|        SUBSET_INI                    source/groups/subset_ini.F    
Chd|        SURF_INI                      source/groups/surf_ini.F      
Chd|        WRRESTP                       source/output/restart/wrrestp.F
Chd|        W_GROUP_STR                   source/output/restart/w_group_str.F
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      MODULE GROUP_MOD
!
        USE GROUPDEF_MOD      
! GROUPS
        TYPE(GROUP_)  ,TARGET,DIMENSION(:),ALLOCATABLE :: IGRNOD    !  NGRNOD
        TYPE(GROUP_)  ,TARGET,DIMENSION(:),ALLOCATABLE :: IGRBRIC   !  NGRBRIC
        TYPE(GROUP_)  ,TARGET,DIMENSION(:),ALLOCATABLE :: IGRQUAD   !  NGRQUAD
        TYPE(GROUP_)  ,TARGET,DIMENSION(:),ALLOCATABLE :: IGRSH4N   !  NGRSHEL
        TYPE(GROUP_)  ,TARGET,DIMENSION(:),ALLOCATABLE :: IGRSH3N   !  NGRSH3N
        TYPE(GROUP_)  ,TARGET,DIMENSION(:),ALLOCATABLE :: IGRTRUSS  !  NGRTRUS
        TYPE(GROUP_)  ,TARGET,DIMENSION(:),ALLOCATABLE :: IGRBEAM   !  NGRBEAM
        TYPE(GROUP_)  ,TARGET,DIMENSION(:),ALLOCATABLE :: IGRSPRING !  NGRSPRI
        TYPE(GROUP_)  ,TARGET,DIMENSION(:),ALLOCATABLE :: IGRPART   !  NGRPART
!
! OTHER ENTITIES 
!
        TYPE(SUBSET_) ,TARGET,DIMENSION(:),ALLOCATABLE :: SUBSETS
        TYPE(SURF_)   ,TARGET,DIMENSION(:),ALLOCATABLE :: IGRSURF   !  NSURF  ---> old ISURF
        TYPE(SURF_)   ,TARGET,DIMENSION(:),ALLOCATABLE :: IGRSLIN   !  NSLIN  --->     ISLIN
!
      END MODULE GROUP_MOD
