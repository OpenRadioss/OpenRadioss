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
Chd|  I25MAIN_FREE                  source/interfaces/intsort/i25main_free.F
Chd|-- called by -----------
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|-- calls ---------------
Chd|        I25EDGE_SWITCH                source/interfaces/int25/i25edge_switch.F
Chd|        I25FREE_BOUND                 source/interfaces/int25/i25free_bound.F
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_GET_STIF25_EDG           source/mpi/interfaces/spmd_getstif25_edg.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25MAIN_FREE(
     1                  ITASK, IPARI  ,INTBUF_TAB, INTLIST25, ISENDTO,
     2                  IRECVFROM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
#include      "macro.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "timeri_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,NINTER),ITASK,INTLIST25(*)
      INTEGER ISENDTO(NSPMD+1,*),IRECVFROM(NSPMD+1,*)
C     REAL
      TYPE(INTBUF_STRUCT_),DIMENSION(NINTER) :: INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, NIN, NRTM, IEDGE, NEDGE
C     REAL
      my_real
     .   STARTT, STOPT
C-----------------------------------------------
      DO N = 1,NINTER25
        NIN = INTLIST25(N)

        NRTM   = IPARI(4,NIN)
        IEDGE  = IPARI(58,NIN)
        NEDGE  = IPARI(68,NIN)
C
        IF(IEDGE/=0)THEN
          CALL  I25EDGE_SWITCH(
     .      ITASK+1,INTBUF_TAB(NIN)%STFM,INTBUF_TAB(NIN)%STFE,INTBUF_TAB(NIN)%MVOISIN,IEDGE,
     .      NEDGE,INTBUF_TAB(NIN)%LEDGE)
C
          CALL MY_BARRIER
C
        END IF
C
!$OMP SINGLE
        CALL I25FREE_BOUND(
     1    NRTM,INTBUF_TAB(NIN)%MVOISIN,INTBUF_TAB(NIN)%IRECTM,INTBUF_TAB(NIN)%STFM,
     .                                                    INTBUF_TAB(NIN)%NRTM_FREE,
     2    INTBUF_TAB(NIN)%FREE_IRECT_ID)
!$OMP END SINGLE
      END DO
C-----------------------------------------------------------
      IF (IMON>0 .AND. ITASK==0) THEN
        CALL STARTIME(MACRO_TIMER_T25STFE,1)
      ENDIF
C       
!$OMP SINGLE
      DO N = 1,NINTER25
       NIN = INTLIST25(N)
       IF(IPARI(MACRO_IEDGE,NIN) > 0) THEN
C
C        Also needed in SMP for resetting STFE !
         CALL SPMD_GET_STIF25_EDG(
     .     INTBUF_TAB(NIN)%STFE,    IPARI(MACRO_NEDGE,NIN), INTBUF_TAB(NIN)%LEDGE,
     .     NIN ,  ISENDTO, IRECVFROM, INTBUF_TAB(NIN)%MPI_COMM, INTBUF_TAB(NIN)%RANK,
     .     INTBUF_TAB(NIN)%NSPMD)
       ENDIF
      ENDDO
!$OMP END SINGLE
C
      IF (IMON>0 .AND. ITASK==0) THEN
        CALL STOPTIME(MACRO_TIMER_T25STFE,1)
      ENDIF
C-----------------------------------------------------------
      RETURN
      END
