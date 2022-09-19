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
Chd|  CREATE_BOX_CLAUSE             source/model/sets/create_box_clause.F
Chd|-- called by -----------
Chd|        HM_SET                        source/model/sets/hm_set.F    
Chd|-- calls ---------------
Chd|        CREATE_ELT_BOX                source/model/sets/create_elt_box.F
Chd|        CREATE_LINE_FROM_ELEMENT      source/model/sets/create_line_from_element.F
Chd|        CREATE_LINE_FROM_SURFACE      source/model/sets/create_line_from_surface.F
Chd|        CREATE_NODE_BOX               source/model/sets/create_node_box.F
Chd|        CREATE_RBODY_BOX              source/model/sets/create_rbody_box.F
Chd|        CREATE_SURFACE_FROM_ELEMENT   source/model/sets/create_surface_from_element.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|        SET_SCRATCH_MOD               ../common_source/modules/setdef_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE CREATE_BOX_CLAUSE(
     *                  CLAUSE   ,JCLAUSE  ,IS_AVAILABLE,LSUBMODEL ,KEYSET   ,
     *                  ITABM1   ,IBOX     ,X           ,SKEW      ,IXS10    ,
     *                  SET_TITLE,IPART    ,SH4TREE     ,SH3TREE   ,IPARTS   ,
     *                  IPARTQ   ,IPARTC   ,IPARTG      ,IPARTT    ,IPARTP   ,
     *                  IPARTR   ,IXS      ,IXQ         ,IXC       ,IXTG     ,
     *                  IXT      ,IXP      ,IXR         ,KNOD2ELS  ,NOD2ELS  ,
     *                  KNOD2ELC ,NOD2ELC  ,KNOD2ELTG   ,NOD2ELTG  ,KNOD2ELQ ,
     *                  NOD2ELQ  ,OPT_A    ,OPT_O       ,OPT_E     ,DELBUF   ,
     *                  RBY_MSN  ,IRBODYM  )
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   Treat the Elmeent Clause, read Elements from HM_READER & fill clause
C------------------------------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME          DESCRIPTION                         
C
C     CLAUSE        (SET structure) Clause to be treated
C     IPARTM1       MAP Table UID -> LocalID
C     JCLAUSE       parameter with HM_READER (current clause read)
C     IS_AVAILABLE  Bool / Result of HM_interface
C     LSUBMODEL     SUBMODEL Structure.
C============================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SETDEF_MOD
      USE SUBMODEL_MOD
      USE MESSAGE_MOD
      USE HM_OPTION_READ_MOD
      USE OPTIONDEF_MOD
      USE SET_SCRATCH_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "radioss_maptable.inc"
#include      "nchar_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: IPART(LIPART1,*),SH4TREE(*),
     .  SH3TREE(*),IXS(NIXS,*),IXC(NIXC,*),IXTG(NIXTG,*),IXS10(6,*),
     .  IXQ(NIXQ,*),IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*),IPARTS(*),
     .  IPARTQ(*),IPARTC(*),IPARTG(*),IPARTT(*),IPARTP(*),IPARTR(*),
     .  KNOD2ELS(*),KNOD2ELC(*),KNOD2ELTG(*),NOD2ELS(*),NOD2ELC(*),
     .  NOD2ELTG(*),NOD2ELQ(*),KNOD2ELQ(*)
      INTEGER  ELTYP,JCLAUSE,OPT_A,OPT_O,OPT_E
!
      INTEGER, INTENT(IN), DIMENSION(NUMNOD,2) :: ITABM1
      INTEGER, INTENT(IN), DIMENSION(NRBODY,2) :: IRBODYM
      INTEGER, INTENT(IN), DIMENSION(2,NRBODY) :: RBY_MSN

!      INTEGER, INTENT(IN)           ::  MAPSIZE
!      INTEGER, DIMENSION(MAPSIZE,2) :: MAP
      LOGICAL :: IS_AVAILABLE
      my_real
     .        X(3,*),SKEW(LSKEW,*)
      CHARACTER KEYSET*ncharfield,SET_TITLE*nchartitle
      TYPE (SET_SCRATCH) ::  DELBUF
C-----------------------------------------------
      TYPE (SET_) ::  CLAUSE
      TYPE(SUBMODEL_DATA),INTENT(IN) :: LSUBMODEL(*)
      TYPE (BOX_)  , DIMENSION(NBBOX) :: IBOX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER BOXTYPE,SURF_LIST,LINE_LIST_1D,ELTYP_ALL
!
      INTEGER ADMBID
      DATA ADMBID/0/
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
!
      IF (KEYSET == 'BOX')  BOXTYPE = 1
      IF (KEYSET == 'BOX2') BOXTYPE = 2
!
!
      ! ---------------------
      ! Fill Boxes
      ! ---------------------
!
!
      ! NODE box
      CALL CREATE_NODE_BOX(
     .            CLAUSE  ,ITABM1  ,JCLAUSE ,IS_AVAILABLE ,LSUBMODEL,
     .            IBOX    ,X       ,SKEW    ,SET_TITLE    ,KEYSET   )


      ! Solid's in box
      IF (NUMELS > 0)
     *   CALL CREATE_ELT_BOX(
     *            CLAUSE  ,IPARTS  ,JCLAUSE ,IS_AVAILABLE ,LSUBMODEL,
     *            IBOX    ,X       ,SKEW    ,SET_TITLE    ,KEYSET   ,
     *            BOXTYPE ,NUMELS  ,NIXS    ,IXS          ,8        ,
     *            IPART   ,ADMBID  ,ADMBID  ,ADMBID       ,ELT_SOLID)

      ! Quad's in box
      IF (NUMELQ > 0)
     *   CALL CREATE_ELT_BOX(
     *            CLAUSE  ,IPARTQ  ,JCLAUSE ,IS_AVAILABLE ,LSUBMODEL,
     *            IBOX    ,X       ,SKEW    ,SET_TITLE    ,KEYSET   ,
     *            BOXTYPE ,NUMELQ  ,NIXQ    ,IXQ          ,4        ,
     *            IPART   ,ADMBID  ,ADMBID  ,ADMBID       ,ELT_QUAD )

      ! Shell's in box
      IF (NUMELC > 0)
     *   CALL CREATE_ELT_BOX(
     *            CLAUSE  ,IPARTC  ,JCLAUSE ,IS_AVAILABLE ,LSUBMODEL,
     *            IBOX    ,X       ,SKEW    ,SET_TITLE    ,KEYSET   ,
     *            BOXTYPE ,NUMELC  ,NIXC    ,IXC          ,4        ,
     *            IPART   ,SH4TREE ,3       ,KSH4TREE     ,ELT_SH4N )

      ! She3n's in box
      IF (NUMELTG > 0 .AND. NUMELTRIA == 0)
     *   CALL CREATE_ELT_BOX(
     *            CLAUSE  ,IPARTG  ,JCLAUSE ,IS_AVAILABLE ,LSUBMODEL,
     *            IBOX    ,X       ,SKEW    ,SET_TITLE    ,KEYSET   ,
     *            BOXTYPE ,NUMELTG ,NIXTG   ,IXTG         ,3        ,
     *            IPART   ,SH3TREE ,3       ,KSH3TREE     ,ELT_SH3N )

      ! Tria's in box
      IF (NUMELTRIA > 0)
     *   CALL CREATE_ELT_BOX(
     *            CLAUSE  ,IPARTG   ,JCLAUSE ,IS_AVAILABLE ,LSUBMODEL,
     *            IBOX    ,X        ,SKEW    ,SET_TITLE    ,KEYSET   ,
     *            BOXTYPE ,NUMELTRIA,NIXTG   ,IXTG         ,3        ,
     *            IPART   ,ADMBID   ,ADMBID  ,ADMBID       ,ELT_TRIA )

      ! Truss's in box
      IF (NUMELT > 0)
     *   CALL CREATE_ELT_BOX(
     *            CLAUSE  ,IPARTT  ,JCLAUSE ,IS_AVAILABLE ,LSUBMODEL,
     *            IBOX    ,X       ,SKEW    ,SET_TITLE    ,KEYSET   ,
     *            BOXTYPE ,NUMELT  ,NIXT    ,IXT          ,2        ,
     *            IPART   ,ADMBID  ,ADMBID  ,ADMBID       ,ELT_TRUSS)

      ! Beam's in box
      IF (NUMELP > 0)
     *   CALL CREATE_ELT_BOX(
     *            CLAUSE  ,IPARTP  ,JCLAUSE ,IS_AVAILABLE ,LSUBMODEL,
     *            IBOX    ,X       ,SKEW    ,SET_TITLE    ,KEYSET   ,
     *            BOXTYPE ,NUMELP  ,NIXP    ,IXP          ,2        ,
     *            IPART   ,ADMBID  ,ADMBID  ,ADMBID       ,ELT_BEAM )

      ! Spring's in box
      IF (NUMELR > 0)
     *   CALL CREATE_ELT_BOX(
     *            CLAUSE  ,IPARTR  ,JCLAUSE ,IS_AVAILABLE ,LSUBMODEL,
     *            IBOX    ,X       ,SKEW    ,SET_TITLE    ,KEYSET   ,
     *            BOXTYPE ,NUMELR  ,NIXR    ,IXR          ,2        ,
     *            IPART   ,ADMBID  ,ADMBID  ,ADMBID       ,ELT_SPRING)
!------------------------------------------------------
!------------------------------------------------------
      ! Line's --- 1D --- in box
      LINE_LIST_1D = CLAUSE%NB_TRUSS + CLAUSE%NB_BEAM + CLAUSE%NB_SPRING
      IF (LINE_LIST_1D > 0)
          ! Line from 1D_ELEMENT
          !-------------------
     *    CALL CREATE_LINE_FROM_ELEMENT(IXT        ,IXP  ,IXR  ,CLAUSE,DELBUF,
     *                                  .FALSE.    )
!------------------------------------------------------
!------------------------------------------------------
      ! Surface's in box
      SURF_LIST = CLAUSE%NB_SOLID + CLAUSE%NB_QUAD +
     +            CLAUSE%NB_SH4N  + CLAUSE%NB_SH3N
      IF (SURF_LIST > 0) THEN
          ! Surface from ELEMENT
          !-------------------
          CALL CREATE_SURFACE_FROM_ELEMENT(
     *                IXS       ,IXS10    ,SH4TREE   ,SH3TREE   ,IXC       ,
     *                IXTG      ,KNOD2ELS ,NOD2ELS   ,KNOD2ELC  ,NOD2ELC   ,
     *                KNOD2ELTG ,NOD2ELTG ,IPARTC    ,IPARTG    ,IPARTS    ,
     *                IPART     ,CLAUSE   ,OPT_A     ,OPT_O     ,IXQ       ,
     *                KNOD2ELQ  ,NOD2ELQ  ,X         ,KEYSET    ,DELBUF    ,
     *                .FALSE.   )
!
          ! Line from SURFACE
          CALL CREATE_LINE_FROM_SURFACE(CLAUSE ,KEYSET,OPT_A,OPT_E,DELBUF,
     .                                  .FALSE.)
      ENDIF ! IF (SURF_LIST > 0)
!------------------------------------------------------
!------------------------------------------------------
      ! Rbodys in box
      IF (CLAUSE%NB_RBODY > 0) THEN
             CALL CREATE_RBODY_BOX(CLAUSE  ,IRBODYM  ,JCLAUSE ,IS_AVAILABLE ,LSUBMODEL,
     .                             IBOX    ,X        ,SKEW    ,SET_TITLE    ,KEYSET   ,
     .                             RBY_MSN )
      ENDIF ! IF (CLAUSE%NB_RBODY > 0)
C-----------------------------------------------
      END
