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
Chd|  FILL_CLAUSE_ELT_BOX           source/model/sets/fill_clause_elt_box.F
Chd|-- called by -----------
Chd|        CREATE_ELT_BOX                source/model/sets/create_elt_box.F
Chd|-- calls ---------------
Chd|        SET_MERGE_SIMPLE              source/model/sets/set_merge_simple.F
Chd|        SIMPLE_ELT_BOX                source/model/sets/simpl_elt_box.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|====================================================================
      SUBROUTINE FILL_CLAUSE_ELT_BOX(
     *                IBOX    ,X           ,SKEW     ,SET_TITLE ,KEYSET ,
     *                BOXLIST ,BOXLIST_SIZE,BOXELTS  ,SZ_BOXELTS,BOXTYPE,
     *                NIX     ,IX           ,NIX1    ,IPARTE    ,IPART  ,
     *                ELTREE  ,KLEVTREE     ,KELTREE ,NUMEL     )

#include      "set_def.inc"
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE OPTIONDEF_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "nchar_c.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (BOX_)    , DIMENSION(NBBOX)  :: IBOX

      INTEGER  BOXELTS(*),BOXLIST(*),IPARTE(*),IPART(LIPART1,*),
     .         IX(NIX,*),ELTREE(KELTREE,*)
      INTEGER  SZ_BOXELTS, BOXLIST_SIZE,BOXTYPE,NIX,NIX1,KLEVTREE,
     .         KELTREE,NUMEL

      my_real
     .        X(3,*),SKEW(LSKEW,*)

      CHARACTER KEYSET*ncharfield
      CHARACTER SET_TITLE*nchartitle
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,B_ELTS_SIZE,NB_RESULT,
     .        ID,IB,NB_BOX_OF_BOX,J,CLAUSE_OPERATOR 
      LOGICAL BOOL
      INTEGER, DIMENSION(:) , ALLOCATABLE :: B_ELTS,RESULT      
C-----------------------------------------------
      ALLOCATE(B_ELTS(NUMEL))
      ALLOCATE(RESULT(NUMEL))

!
      ! Tag nodes from boxes
      ! ---------------------
      CLAUSE_OPERATOR = SET_ADD 
      SZ_BOXELTS = 0
      DO I=1,BOXLIST_SIZE
        IB = BOXLIST(I)
        B_ELTS_SIZE = 0
        
        CALL ELT_BOX(IB  ,
     *           IBOX    ,X          ,SKEW   ,SET_TITLE ,KEYSET,
     *           B_ELTS  ,B_ELTS_SIZE,NIX    ,IX        ,NIX1   ,
     *           IPARTE  ,IPART      ,ELTREE ,KLEVTREE  ,KELTREE,
     *           NUMEL   ,BOXTYPE    )
        

        CALL SET_MERGE_SIMPLE( BOXELTS,  SZ_BOXELTS  ,
     *                          B_ELTS,   B_ELTS_SIZE ,
     *                          RESULT,  NB_RESULT  ,
     *                          CLAUSE_OPERATOR)

         BOXELTS(1:NB_RESULT) = RESULT(1:NB_RESULT)
         SZ_BOXELTS = NB_RESULT        
      ENDDO ! DO I=1,BOXLIST_SIZE

C-------
      RETURN
      END


      RECURSIVE SUBROUTINE ELT_BOX( IB  ,
     *                     IBOX    ,X         ,SKEW   ,SET_TITLE ,KEYSET,
     *                     BOXELTS ,SZ_BOXELTS,NIX    ,IX        ,NIX1   ,
     *                     IPARTE  ,IPART     ,ELTREE ,KLEVTREE  ,KELTREE,
     *                     NUMEL   ,BOXTYPE   )
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   Recursive routine - Go through tree and fill the Node Box array
C
C------------------------------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME           DESCRIPTION                         
C
C     IB             Recursive indice / Current Box to treat
C     IBOX           IBOX Structure
C     SKEW           Skew Structure
C     SET_TITLE      Title for Error message
C     KEYSET         KEYSET for  Error message
C     BOXELTS         merged node array
C     SZ_BOXELTS      number of stacked nodes in BOXELTS
C============================================================================
#include      "set_def.inc"
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE OPTIONDEF_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "nchar_c.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (BOX_)    , DIMENSION(NBBOX)  :: IBOX

      INTEGER  BOXELTS(*),IX(NIX,*),IPARTE(*),IPART(LIPART1,*),
     .         ELTREE(KELTREE,*)
      INTEGER  IB,SZ_BOXELTS,NIX,NIX1,KLEVTREE,KELTREE,NUMEL,BOXTYPE

      my_real
     .        X(3,*),SKEW(LSKEW,*)

      CHARACTER KEYSET*ncharfield
      CHARACTER SET_TITLE*nchartitle
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,B_ELTS_SIZE,NB_RESULT,
     .        NB_BOX_OF_BOX,J,NEW_BOX,CLAUSE_OPERATOR
      LOGICAL BOOL
      INTEGER, DIMENSION(:) , ALLOCATABLE :: B_ELTS,RESULT
      
C-----------------------------------------------
        

      NB_BOX_OF_BOX   = IBOX(IB)%NBOXBOX

      IF ( NB_BOX_OF_BOX == 0 ) THEN

          ! Fill SIMPLE Boxes
          ! ---------------------
          SZ_BOXELTS = 0
          CALL SIMPLE_ELT_BOX(
     .                IBOX       ,X      ,SKEW     ,IB      ,BOXELTS ,
     .                SZ_BOXELTS ,NIX    ,IX       ,NIX1    ,IPARTE  ,
     .                IPART      ,ELTREE ,KLEVTREE ,KELTREE ,NUMEL   ,
     .                BOXTYPE    )

      ELSE
          ALLOCATE(B_ELTS(NUMEL))
          ALLOCATE(RESULT(NUMEL))


          DO I=1,NB_BOX_OF_BOX

            J    = IBOX(IB)%IBOXBOX(I)  !  could be negative
            NEW_BOX = ABS(J)

            B_ELTS_SIZE=0

            CALL ELT_BOX (NEW_BOX ,
     *                    IBOX    ,X          ,SKEW   ,SET_TITLE ,KEYSET,
     *                    B_ELTS  ,B_ELTS_SIZE,NIX    ,IX        ,NIX1   ,
     *                    IPARTE  ,IPART      ,ELTREE ,KLEVTREE  ,KELTREE,
     *                    NUMEL   ,BOXTYPE    )
            
            IF (J < 0)THEN
                CLAUSE_OPERATOR = SET_DELETE
            ELSE
                CLAUSE_OPERATOR = SET_ADD 
            ENDIF

            CALL SET_MERGE_SIMPLE( BOXELTS,  SZ_BOXELTS  ,
     *                             B_ELTS,   B_ELTS_SIZE ,
     *                             RESULT,  NB_RESULT  ,
     *                             CLAUSE_OPERATOR)

            BOXELTS(1:NB_RESULT) = RESULT(1:NB_RESULT)
            SZ_BOXELTS = NB_RESULT
          ENDDO

       ENDIF

C-------
      RETURN
      END







