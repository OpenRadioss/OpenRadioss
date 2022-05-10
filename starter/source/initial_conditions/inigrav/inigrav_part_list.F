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
Chd|  INIGRAV_PART_LIST             source/initial_conditions/inigrav/inigrav_part_list.F
Chd|-- called by -----------
Chd|        INIGRAV_LOAD                  source/initial_conditions/inigrav/inigrav_load.F
Chd|-- calls ---------------
Chd|        EBCS_MOD                      ../common_source/modules/ebcs_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INIGRAV                       share/modules1/inigrav_mod.F  
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE INIGRAV_PART_LIST(IPART, IGRPART, EBCS_TAB)
C-----------------------------------------------
C   Description
C-----------------------------------------------
C we need to enumerate all PARTS related to INIGRAV and record gravity vectors 
C this will be used to apply pressure gradient due to hydrostatic pressure with /EBCS/NRF
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INIGRAV
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE EBCS_MOD      
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "inigrav_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN),TARGET          :: IPART(LIPART1,*)    
      TYPE (GROUP_)  , DIMENSION(NGRPART) :: IGRPART
      TYPE(T_EBCS_TAB), INTENT(INOUT)     :: EBCS_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: lFOUND_EBCS_NRF
      INTEGER :: KK,JJ,ID_PART,TYP,NSEG,NP
      INTEGER :: IGRP
      my_real :: GRAV0,NGX,NGY,NGZ
C---------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

      !-- pre-condition to run this subroutine (this also avoid to access array with 0 as index)
      IF(.NOT. EBCS_TAB%is_created)RETURN
      IF(NPART == 0)RETURN

      !-- checking if user defined /EBCS/NRF
      lFOUND_EBCS_NRF = .FALSE.
      DO KK=1,EBCS_TAB%nebcs
        TYP  = EBCS_TAB%tab(kk)%poly%type
        NSEG = EBCS_TAB%tab(kk)%poly%nb_elem
        IF(TYP == 10 .AND. NSEG > 0) THEN
          lFOUND_EBCS_NRF = .TRUE.
          EXIT
        ENDIF
      ENDDO
      IF(.NOT.lFOUND_EBCS_NRF)RETURN

      !-- allocations
      INIGRAV_PARTS%IS_ALLOCATED = .FALSE.
      ALLOCATE(INIGRAV_PARTS%TAGPART(NPART))
      ALLOCATE(INIGRAV_PARTS%GRAV0(NPART))
      ALLOCATE(INIGRAV_PARTS%NG(3,NPART))      
      INIGRAV_PARTS%IS_ALLOCATED = .TRUE.
                  
      !-- list of PARTs
      INIGRAV_PARTS%TAGPART(1:NPART) = 0
      INIGRAV_PARTS%GRAV0(1:NPART) = ZERO      
      DO KK=1,NINIGRAV 
        IGRP    = INIGRV(1,KK)           
        GRAV0   = LINIGRAV(07,KK)  
        NGX     = LINIGRAV(08,KK)
        NGY     = LINIGRAV(09,KK)
        NGZ     = LINIGRAV(10,KK) 
        IF(IGRP == 0)CYCLE
        NP      = IGRPART(IGRP)%NENTITY
        DO JJ=1,NP
          ID_PART = IGRPART(IGRP)%ENTITY(JJ)   !  UID : IPART(4,IGRPART(IGRP)%ENTITY(JJ))
          INIGRAV_PARTS%TAGPART(ID_PART) = 1
          INIGRAV_PARTS%GRAV0(ID_PART) = GRAV0       
          INIGRAV_PARTS%NG(1,ID_PART) = NGX
          INIGRAV_PARTS%NG(2,ID_PART) = NGY
          INIGRAV_PARTS%NG(3,ID_PART) = NGZ
        ENDDO
      ENDDO    !next K

      !DEALLOCATE INIGRAV_PARTS WHEN NO LONGER USED BY /EBCS/NRF   (WHEN HYDROSTATIC PRESSURE WAS INITIALIZED)
      
      RETURN
      END



