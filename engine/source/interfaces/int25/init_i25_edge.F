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
Chd|  SPMD_I25_PREPARE              source/interfaces/int25/i25_prepare.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
        SUBROUTINE INIT_I25_EDGE(NLEDGE,NINTER,NPARI,IPARI,INTBUF_TAB)
!$COMMENT
!       INIT_I25_EDGE description :
!       for interface type 25, find the solid edge & non-main edge
!
!       INIT_I25_EDGE organization :
!           loop over the interface
!               * if NiN is an interface type 25 with edge to edge --> find the solid edge
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "i25edge_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(in) :: NLEDGE !< dim of ledge array
        INTEGER, INTENT(in) :: NINTER !< number of interface
        INTEGER, INTENT(in) :: NPARI !< dim of IPARI array
        INTEGER, DIMENSION(NPARI,NINTER), INTENT(in) :: IPARI !< interface data
        TYPE(INTBUF_STRUCT_), DIMENSION(NINTER), INTENT(inout) :: INTBUF_TAB !< interface data
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
        INTEGER :: I,J
        INTEGER :: INTERFACE_TYPE
        INTEGER :: SOL_EDGE,SH_EDGE,IEDGE,NEDGE
        INTEGER, DIMENSION(:), ALLOCATABLE :: TMP_ARRAY
C-----------------------------------------------
    
        ! ------------------------
        ! loop over the interface
        DO I=1,NINTER
          INTBUF_TAB(I)%NUMBER_EDGE_TYPE1 = 0
          INTBUF_TAB(I)%NUMBER_EDGE_TYPE1_0 = 0
          INTERFACE_TYPE = IPARI(7,I)
          IEDGE = IPARI(58,I)
          ! -----------------
          ! check if the interface is a type 25 with edge to edge
          IF(INTERFACE_TYPE==25.AND.IEDGE/=0) THEN
            NEDGE = IPARI(68,I)
            ALLOCATE (TMP_ARRAY(NEDGE) )
            ! ----------
            ! loop over the edge to find the edge solid
            DO J=1,NEDGE
              IF(INTBUF_TAB(I)%LEDGE((J-1)*NLEDGE+LEDGE_TYPE)==1) THEN
                INTBUF_TAB(I)%NUMBER_EDGE_TYPE1 = INTBUF_TAB(I)%NUMBER_EDGE_TYPE1 + 1
                TMP_ARRAY(INTBUF_TAB(I)%NUMBER_EDGE_TYPE1) = J
              ENDIF
            ENDDO
            ALLOCATE( INTBUF_TAB(I)%EDGE_TYPE1(INTBUF_TAB(I)%NUMBER_EDGE_TYPE1) )
            INTBUF_TAB(I)%EDGE_TYPE1(1:INTBUF_TAB(I)%NUMBER_EDGE_TYPE1) = TMP_ARRAY(1:INTBUF_TAB(I)%NUMBER_EDGE_TYPE1)
            ! ----------

            ! ----------
            ! loop over the edge to find the edge solid & the edge S
            SOL_EDGE = IEDGE/10 ! solids
            SH_EDGE = IEDGE-10*SOL_EDGE ! shells
            DO J=1,NEDGE
              IF(INTBUF_TAB(I)%LEDGE((J-1)*NLEDGE+LEDGE_TYPE)>=0.AND.
     .           (INTBUF_TAB(I)%LEDGE((J-1)*NLEDGE+LEDGE_TYPE)==1.OR.SH_EDGE==0) ) THEN
                INTBUF_TAB(I)%NUMBER_EDGE_TYPE1_0 = INTBUF_TAB(I)%NUMBER_EDGE_TYPE1_0 + 1
                TMP_ARRAY(INTBUF_TAB(I)%NUMBER_EDGE_TYPE1_0) = J
              ENDIF
            ENDDO
            ALLOCATE( INTBUF_TAB(I)%EDGE_TYPE1_0(INTBUF_TAB(I)%NUMBER_EDGE_TYPE1_0) )
            INTBUF_TAB(I)%EDGE_TYPE1_0(1:INTBUF_TAB(I)%NUMBER_EDGE_TYPE1_0) = TMP_ARRAY(1:INTBUF_TAB(I)%NUMBER_EDGE_TYPE1_0)
            ! ----------

            DEALLOCATE( TMP_ARRAY )
          ELSE
            ALLOCATE( INTBUF_TAB(I)%EDGE_TYPE1(INTBUF_TAB(I)%NUMBER_EDGE_TYPE1) )
            ALLOCATE( INTBUF_TAB(I)%EDGE_TYPE1_0(INTBUF_TAB(I)%NUMBER_EDGE_TYPE1_0) )
          ENDIF
          ! -----------------
        ENDDO
        ! ------------------------

        RETURN
        END SUBROUTINE INIT_I25_EDGE
    
