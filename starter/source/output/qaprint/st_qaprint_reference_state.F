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
Chd|  ST_QAPRINT_REFERENCE_STATE    source/output/qaprint/st_qaprint_reference_state.F
Chd|-- called by -----------
Chd|        ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE ST_QAPRINT_REFERENCE_STATE(XREFC     ,XREFTG    ,XREFS     ,TAGXREF,   
     .                                      IXS       ,IXC       ,IXTG      )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE QA_OUT_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "tabsiz_c.inc"
#include      "thermal_c.inc"
#include      "units_c.inc"
#include      "my_allocate.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: TAGXREF(NUMNOD),IXC(NIXC,*),IXTG(NIXTG,*),IXS(NIXS,*)
      my_real, INTENT(IN) ::
     .                       XREFC(4,3,NUMELC),XREFTG(3,3,NUMELTG),XREFS(8,3,NUMELS8)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IE,IN,NN,TEMP_INT,NC,ELEM_ID,WORK(70000)
      CHARACTER (LEN=255) :: VARNAME
      DOUBLE PRECISION TEMP_DOUBLE
      INTEGER, ALLOCATABLE, DIMENSION(:) :: INDEX,ITR1
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
C
C-----------------------------------------------
C     XREF
C-----------------------------------------------

      IF (MYQAKEY('/XREF')) THEN
C      
        WRITE(VARNAME,'(A)') 'NXREF'
        TEMP_INT = NXREF
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)  
C        
        IF (NXREF > 0) THEN
C  
          DO IE = 1,NUMELC
            DO IN = 1,4
              NN = IXC(IN+1,IE)
              IF (TAGXREF(NN) == 1) THEN
                ! Number of the node
                WRITE(VARNAME,'(A)') 'XREFC_NODE'
                TEMP_INT = NN
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8) 
                ! Coordinate X of the node
                WRITE(VARNAME,'(A)') 'XREFC_X'
                TEMP_DOUBLE =  XREFC(IN,1,IE)
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)    
                ! Coordinate Y of the node
                WRITE(VARNAME,'(A)') 'XREFC_Y'
                TEMP_DOUBLE =  XREFC(IN,2,IE)
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)   
                ! Coordinate Z of the node
                WRITE(VARNAME,'(A)') 'XREFC_Z'
                TEMP_DOUBLE =  XREFC(IN,3,IE)
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
              ENDIF
            ENDDO
          ENDDO
C         
          DO IE = 1,NUMELTG
            DO IN = 1,3
              NN = IXTG(IN+1,IE)
              IF (TAGXREF(NN) == 1) THEN
                ! Number of the node
                WRITE(VARNAME,'(A)') 'XREFTG_NODE'
                TEMP_INT = NN
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8) 
                ! Coordinate X of the node
                WRITE(VARNAME,'(A)') 'XREFTG_X'
                TEMP_DOUBLE =  XREFTG(IN,1,IE)
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)    
                ! Coordinate Y of the node
                WRITE(VARNAME,'(A)') 'XREFTG_Y'
                TEMP_DOUBLE =  XREFTG(IN,2,IE)
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)   
                ! Coordinate Z of the node
                WRITE(VARNAME,'(A)') 'XREFTG_Z'
                TEMP_DOUBLE =  XREFTG(IN,3,IE)
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
              ENDIF
            ENDDO
          ENDDO
C         
          DO IE = 1,NUMELS8
            DO IN = 1,8
              NN = IXS(IN+1,IE)
              IF (TAGXREF(NN) == 1) THEN
                ! Number of the node
                WRITE(VARNAME,'(A)') 'XREFS_NODE'
                TEMP_INT = NN
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8) 
                ! Coordinate X of the node
                WRITE(VARNAME,'(A)') 'XREFS_X'
                TEMP_DOUBLE =  XREFS(IN,1,IE)
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)    
                ! Coordinate Y of the node
                WRITE(VARNAME,'(A)') 'XREFS_Y'
                TEMP_DOUBLE =  XREFS(IN,2,IE)
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)   
                ! Coordinate Z of the node
                WRITE(VARNAME,'(A)') 'XREFS_Z'
                TEMP_DOUBLE =  XREFS(IN,3,IE)
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
              ENDIF
            ENDDO
          ENDDO
C
        ENDIF
C
      ENDIF

C-----------------------------------------------
C     EREF
C-----------------------------------------------
      IF (MYQAKEY('/EREF')) THEN
C      
        WRITE(VARNAME,'(A)') 'NEREF'
        TEMP_INT = NEREF
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)  
C        
        IF (NEREF > 0) THEN
C
          MY_ALLOCATE(INDEX,2*NUMELC)
          MY_ALLOCATE(ITR1,NUMELC)
C
          DO IE=1,NUMELC
            ITR1(IE)=IXC(NIXC,IE)
          ENDDO 
          CALL MY_ORDERS(0,WORK,ITR1,INDEX,NUMELC,1)
C  
          DO IE = 1,NUMELC
            NC=INDEX(IE)
            ELEM_ID = IXC(NIXC,NC)
C
            DO IN = 1,4
              NN = IXC(IN+1,NC)
              IF (TAGXREF(NN) /= 1) THEN
                ! Id of the element
                WRITE(VARNAME,'(A,I0)') 'EREF_SHELL_ELEMENT_NODE ',IN
                TEMP_INT = ELEM_ID
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
                IF (XREFC(IN,1,NC) > 0) THEN
                  ! Coordinate X of the node
                  WRITE(VARNAME,'(A)') 'EREFC_X'
                  TEMP_DOUBLE =  XREFC(IN,1,NC)
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
                ENDIF
                IF (XREFC(IN,2,NC) > 0) THEN    
                  ! Coordinate Y of the node
                  WRITE(VARNAME,'(A)') 'EREFC_Y'
                  TEMP_DOUBLE =  XREFC(IN,2,NC)
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
                ENDIF
                IF (XREFC(IN,3,NC) > 0) THEN    
                  ! Coordinate Z of the node
                  WRITE(VARNAME,'(A)') 'EREFC_Z'
                  TEMP_DOUBLE =  XREFC(IN,3,NC)
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
                ENDIF
              ENDIF
            ENDDO
C
          ENDDO
          DEALLOCATE(INDEX,ITR1)
C
          MY_ALLOCATE(INDEX,2*NUMELTG)
          MY_ALLOCATE(ITR1,NUMELTG)
C
          DO IE=1,NUMELTG
            ITR1(IE)=IXTG(NIXTG,IE)
          ENDDO
          CALL MY_ORDERS(0,WORK,ITR1,INDEX,NUMELTG,1) 
C  
          DO IE = 1,NUMELTG
            NC=INDEX(IE)
            ELEM_ID = IXTG(NIXTG,NC)
C
            DO IN = 1,3
              NN = IXTG(IN+1,NC)
              IF (TAGXREF(NN) /= 1) THEN
                ! Id of the element
                WRITE(VARNAME,'(A,I0)') 'EREF_SH3N_ELEMENT_NODE ',IN
                TEMP_INT = ELEM_ID
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
                IF (XREFTG(IN,1,NC) > 0) THEN
                  ! Coordinate X of the node
                  WRITE(VARNAME,'(A)') 'EREFTG_X'
                  TEMP_DOUBLE =  XREFTG(IN,1,NC)
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
                ENDIF
                IF (XREFTG(IN,2,NC) > 0) THEN    
                  ! Coordinate Y of the node
                  WRITE(VARNAME,'(A)') 'EREFTG_Y'
                  TEMP_DOUBLE =  XREFTG(IN,2,NC)
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
                ENDIF
                IF (XREFTG(IN,3,NC) > 0) THEN    
                  ! Coordinate Z of the node
                  WRITE(VARNAME,'(A)') 'EREFTG_Z'
                  TEMP_DOUBLE =  XREFTG(IN,3,NC)
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
                ENDIF
              ENDIF
            ENDDO
C
          ENDDO
          DEALLOCATE(INDEX,ITR1)
C
          MY_ALLOCATE(INDEX,2*NUMELS8)
          MY_ALLOCATE(ITR1,NUMELS8)
C
          DO IE=1,NUMELS8
            ITR1(IE)=IXS(NIXS,IE)
          ENDDO
          CALL MY_ORDERS(0,WORK,ITR1,INDEX,NUMELS8,1) 
C  
          DO IE = 1,NUMELS8
            NC=INDEX(IE)
            ELEM_ID = IXS(NIXS,NC)
C
            DO IN = 1,8
              NN = IXS(IN+1,NC)
              IF (TAGXREF(NN) /= 1) THEN
                ! Id of the element
                WRITE(VARNAME,'(A,I0)') 'EREF_SOLID_ELEMENT_NODE ',IN
                TEMP_INT = ELEM_ID
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
                IF (XREFS(IN,1,NC) > 0) THEN
                  ! Coordinate X of the node
                  WRITE(VARNAME,'(A)') 'EREFS_X'
                  TEMP_DOUBLE =  XREFS(IN,1,NC)
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
                ENDIF
                IF (XREFS(IN,2,NC) > 0) THEN    
                  ! Coordinate Y of the node
                  WRITE(VARNAME,'(A)') 'EREFS_Y'
                  TEMP_DOUBLE =  XREFS(IN,2,NC)
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
                ENDIF
                IF (XREFS(IN,3,NC) > 0) THEN    
                  ! Coordinate Z of the node
                  WRITE(VARNAME,'(A)') 'EREFS_Z'
                  TEMP_DOUBLE =  XREFS(IN,3,NC)
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
                ENDIF
              ENDIF
            ENDDO
C
          ENDDO
          DEALLOCATE(INDEX,ITR1)
C
        ENDIF

      ENDIF

      
      END SUBROUTINE