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
      INTEGER MAXLAY,ANIM_EPSP,ANIM_STRESS,ANIM_DAMA,ANIM_STRAIN,ANIM_EPSDOT,ANIM_WPLA
      PARAMETER(MAXLAY = 100)
      COMMON /ANIVAR1/ ANIM_EPSP(MAXLAY)
      COMMON /ANIVAR2/ ANIM_STRESS(MAXLAY)
      COMMON /ANIVAR3/ ANIM_DAMA(MAXLAY)
      COMMON /ANIVAR4/ ANIM_STRAIN(MAXLAY)
      COMMON /ANIVAR5/ ANIM_EPSDOT(MAXLAY)
      COMMON /ANIVAR6/ ANIM_WPLA(MAXLAY)
c
      INTEGER MX_PLY_ANIM
      PARAMETER (MX_PLY_ANIM = 8000)      
      COMMON /SCR25/ IEPSDOTALL_PLY,IDAMAALL_PLY,
     .              PLY_ANIM(MX_PLY_ANIM),PLY_ANIM_STRESS(MX_PLY_ANIM),
     .              PLY_ANIM_PHI(MX_PLY_ANIM),PLY_ANIM_EPSP(MX_PLY_ANIM),
     .              PLY_ANIM_STRAIN(MX_PLY_ANIM),PLY_ANIM_EPSDOT(MX_PLY_ANIM),
     .              PLY_ANIM_DAMA(MX_PLY_ANIM),IBRICK_STRESSALL,
     .              IBRICK_STRAINALL,IBRICK_EPSPALL
      INTEGER        IEPSDOTALL_PLY,IDAMAALL_PLY,
     .              PLY_ANIM,PLY_ANIM_STRESS,PLY_ANIM_PHI,PLY_ANIM_EPSP,
     .              PLY_ANIM_STRAIN,PLY_ANIM_EPSDOT,PLY_ANIM_DAMA,
     .              IBRICK_STRESSALL,IBRICK_STRAINALL,IBRICK_EPSPALL
