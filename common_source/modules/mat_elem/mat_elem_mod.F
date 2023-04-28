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
Chd|====================================================================
Chd|  MATERIAL & ELEMENTS                      
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      MODULE MAT_ELEM_MOD
        USE ELBUFDEF_MOD
        USE MATPARAM_DEF_MOD
        USE PROP_PARAM_MOD
        USE GROUP_PARAM_MOD

        TYPE MAT_ELEM_
           INTEGER :: NGROUP       ! Number of element groups
           INTEGER :: NUMMAT       ! Number of /MAT  cards
           INTEGER :: NUMGEO       ! Number of /PROP cards
           INTEGER :: NUMSUBSTACK  ! Number of SUBSTACK created from /PROP/TYPE17, TYPE51 or TYPE52 => NS_STACK (see stackgroup.F)
           INTEGER :: NUMSTACK     ! Number of /STACK  used with /PCOMPP
           INTEGER :: NUMPLY       ! Number of /PLY    used with /PCOMPP

           TYPE (ELBUF_STRUCT_)    ,DIMENSION(:)   ,ALLOCATABLE :: ELBUF          ! NGROUP
           TYPE (ELBUF_STRUCT_)    ,DIMENSION(:,:) ,ALLOCATABLE :: XFEM_TAB       
           TYPE (GROUP_PARAM_)     ,DIMENSION(:)   ,ALLOCATABLE :: GROUP_PARAM    ! NGROUP
        
           TYPE (MATPARAM_STRUCT_) ,DIMENSION(:)   ,POINTER     :: MAT_PARAM      ! NUMMAT
        
           TYPE (PROP_PARAM_)      ,DIMENSION(:)   ,ALLOCATABLE :: PROP_PARAM     ! NUMGEO
           TYPE (PROP_PARAM_)      ,DIMENSION(:)   ,ALLOCATABLE :: PROP_STACK     ! NUMSTACK
           TYPE (PROP_PARAM_)      ,DIMENSION(:)   ,ALLOCATABLE :: PROP_PLY       ! NUMPLY
           TYPE (PROP_PARAM_)      ,DIMENSION(:)   ,ALLOCATABLE :: PROP_SUBSTACK  ! NUMSUBSTACK      
     
        END TYPE MAT_ELEM_

      END MODULE MAT_ELEM_MOD
