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
Chd|  SURFACE_BUFFER                source/model/sets/surface_buffer.F
Chd|-- called by -----------
Chd|        CREATE_SURFACE_FROM_ELEMENT   source/model/sets/create_surface_from_element.F
Chd|-- calls ---------------
Chd|        QUAD_SURFACE_BUFFER           source/model/sets/quad_surface_buffer.F
Chd|        SHELL_SURFACE_BUFFER          source/model/sets/shell_surface_buffer.F
Chd|        SHELL_SURFACE_BUFFER_REMESH   source/model/sets/shell_surface_buffer_remesh.F
Chd|        SOLID_SURFACE_BUFFER          source/model/sets/solid_surface_buffer.F
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|====================================================================
      SUBROUTINE SURFACE_BUFFER(
     .                          IXS       ,IXS10    ,IXC       ,SH4TREE   ,SH3TREE   ,
     .                          IXTG      ,KNOD2ELS ,NOD2ELS   ,KNOD2ELC  ,NOD2ELC   ,
     .                          KNOD2ELTG ,NOD2ELTG ,NSEG      ,IEXT      ,IPARTC    ,
     .                          IPARTS    ,IPARTG   ,CLAUSE    ,BUFTMPSURF,IAD_SURF  ,
     .                          OPT_O     ,IPART    ,KNOD2ELQ  ,NOD2ELQ   ,X         ,
     .                          IXQ       ,KEYSET)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SETDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "remesh_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),KNOD2ELS(*),NOD2ELS(*),IXS10(6,*),
     .        KNOD2ELC(*),NOD2ELC(*),KNOD2ELTG(*),NOD2ELTG(*),
     .        IXC(NIXC,*),IXTG(NIXTG,*),IPARTC(*),IPARTG(*),
     .        BUFTMPSURF(*),IPARTS(*),SH4TREE(*),SH3TREE(*),
     .        IPART(LIPART1,*),KNOD2ELQ(*),NOD2ELQ(*),IXQ(NIXQ,*)
      INTEGER IEXT,NSEG,IAD_SURF,OPT_O
      CHARACTER KEYSET*ncharfield
      my_real
     .   X(3,*)
!
      TYPE (SET_) ::  CLAUSE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C=======================================================================
!------------------
!             counting / buffering CLAUSE surface segments from elems ( EXT / ALL )
!---
!
!     ---  solid ---
      IF ( CLAUSE%NB_SOLID > 0 ) CALL SOLID_SURFACE_BUFFER(
     .                            IXS      ,IXS10    ,IXC      ,IXTG      ,CLAUSE   ,
     .                            KNOD2ELS ,NOD2ELS  ,KNOD2ELC ,NOD2ELC   ,KNOD2ELTG,
     .                            NOD2ELTG ,NSEG     ,IEXT     ,BUFTMPSURF,IPARTS   ,
     .                            IPARTC   ,IPARTG   ,IAD_SURF ,KEYSET)
!
!     ---  shell + sh3n ---
!
      IF (NADMESH == 0) THEN
        IF ( CLAUSE%NB_SH4N > 0 ) CALL SHELL_SURFACE_BUFFER(
     .                             IXC     ,NIXC      ,2         ,5         ,3     ,
     .                             NSEG    ,IAD_SURF  ,OPT_O     ,BUFTMPSURF,CLAUSE)
        IF ( CLAUSE%NB_SH3N > 0 .AND. NUMELTRIA == 0)
     .                            CALL SHELL_SURFACE_BUFFER(
     .                             IXTG     ,NIXTG     ,2        ,4         ,7     ,
     .                             NSEG     ,IAD_SURF  ,OPT_O    ,BUFTMPSURF,CLAUSE)
      ELSE
        IF ( CLAUSE%NB_SH4N > 0 ) CALL SHELL_SURFACE_BUFFER_REMESH(
     .                             IXC     ,NIXC      ,2	 ,5	    ,3     ,
     .                             NSEG    ,IAD_SURF  ,BUFTMPSURF,CLAUSE    ,IPARTC ,
     .                             IPART   ,KSH4TREE  ,SH4TREE   )
        IF ( CLAUSE%NB_SH3N > 0 .AND. NUMELTRIA == 0)
     .                            CALL SHELL_SURFACE_BUFFER_REMESH(
     .                             IXTG     ,NIXTG     ,2         ,4         ,7     ,
     .                             NSEG     ,IAD_SURF  ,BUFTMPSURF,CLAUSE    ,IPARTG,
     .                             IPART    ,KSH3TREE  ,SH3TREE   )
      ENDIF ! IF (NADMESH == 0)
!
!     ---  quad --- external surface only ( EXT )
!
      IF ( CLAUSE%NB_QUAD > 0 ) CALL QUAD_SURFACE_BUFFER(
     .                           IXQ     ,IAD_SURF ,BUFTMPSURF ,NSEG  ,KNOD2ELQ ,
     .                           NOD2ELQ ,IEXT     ,X          ,CLAUSE)
!---
      RETURN
      END
