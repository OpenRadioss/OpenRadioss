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
Chd|  I25MAIND_2                    source/interfaces/int25/i25maind_2.F
Chd|-- called by -----------
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|-- calls ---------------
Chd|        I25COMP_2                     source/interfaces/int25/i25comp_2.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTBUFMOD                     share/modules/restart_mod.F   
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MPI_COMMOD                    share/modules/mpi_comm_mod.F  
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25MAIND_2(
     1       IPARI    ,ITAB    ,SENSOR_TAB,INTLIST25,INTBUF_TAB ,
     2       X        ,V       ,KINET   ,JTASK    ,NB_DST2    ,
     3       ICODT    ,ISKEW   ,NSENSOR )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFMOD
      USE MESSAGE_MOD
      USE TRI7BOX
      USE INTBUFDEF_MOD  
      USE MPI_COMMOD
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NSENSOR
      INTEGER IPARI(NPARI,*), ITAB(*), INTLIST25(*), JTASK,
     .        KINET(*), NB_DST2(PARASIZ), ICODT(*), ISKEW(*)
      my_real :: X(3,*), V(3,*)
      TYPE(INTBUF_STRUCT_),DIMENSION(NINTER) :: INTBUF_TAB
      TYPE (SENSOR_STR_)  ,DIMENSION(NSENSOR) ,INTENT(IN) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIN, NI25, ISENS, LINDMAX
      my_real
     .       TS, STARTT, STOPT
C-----------------------------------------------
      DO NI25=1,NINTER25
C
        NIN = INTLIST25(NI25)
C
        LINDMAX = INTBUF_TAB(NIN)%I_STOK(2)
C
        STARTT=INTBUF_TAB(NIN)%VARIABLES(3)
        STOPT =INTBUF_TAB(NIN)%VARIABLES(11)
        IF(STARTT>TT) CYCLE
        IF(TT>STOPT)  CYCLE
C
        ISENS = IPARI(64,NIN) 
        IF (ISENS > 0) THEN    ! Interface activated by sensor
           TS = SENSOR_TAB(ISENS)%TSTART
        ELSE
           TS = TT
        ENDIF
        IF(TT<TS) CYCLE

        CALL I25COMP_2(
     1    IPARI   ,INTBUF_TAB(NIN),X   ,ITAB      ,NIN          ,
     2    LINDMAX ,KINET     ,JTASK    ,NB_DST2(JTASK),V        ,
     3    ICODT   ,ISKEW     )
C
      ENDDO
C-----------------------------------------------------------------------
      RETURN
      END
C
