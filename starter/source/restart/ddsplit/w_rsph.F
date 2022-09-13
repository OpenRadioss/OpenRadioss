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
C
Chd|====================================================================
Chd|  W_RSPH                        source/restart/ddsplit/w_rsph.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE W_RSPH(SPBUF ,NUMSPH_L,CEPSP,PROC,
     2                  VSPHIO,LEN_AM,SSPHVELN_L)
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "sphcom.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUMSPH_L, PROC, LEN_AM,
     .        CEPSP(*),SSPHVELN_L, STAT
      my_real
     .        SPBUF(NSPBUF,*),VSPHIO(*)

      my_real, DIMENSION(:), ALLOCATABLE :: SPHVELN_L
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, IE_L
      my_real, DIMENSION(:,:), ALLOCATABLE :: SPBUF_L
C-----------------------------------------------
C
! ------------------------------------
!     allocate 2d array
      ALLOCATE( SPBUF_L(NSPBUF,NUMSPH_L) )
! ------------------------------------
      IE_L = 0
C
      DO I = 1, NUMSPH
        IF(CEPSP(I)==PROC) THEN
          IE_L = IE_L + 1
          DO J = 1, NSPBUF
            SPBUF_L(J,IE_L) = SPBUF(J,I)
          END DO
        END IF
      END DO
C
      CALL WRITE_DB(SPBUF_L,NUMSPH_L*NSPBUF)
      LEN_AM = LEN_AM + NUMSPH_L*NSPBUF
C
      IF(NSPHIO > 0) THEN
        ALLOCATE(SPHVELN_L(SSPHVELN_L)    ,STAT=stat)
        IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR)       
        SPHVELN_L(1:SSPHVELN_L)=ZERO
C ecriture faite sur p0 en monodomaine pour inlet/outlet
        CALL WRITE_DB(VSPHIO,SVSPHIO)
        CALL WRITE_DB(SPHVELN_L,SSPHVELN_L)
        LEN_AM = LEN_AM + SVSPHIO + SSPHVELN_L
        DEALLOCATE(SPHVELN_L)
      END IF
C
! ------------------------------------
!     deallocate 2d array
      DEALLOCATE( SPBUF_L )
! ------------------------------------
      RETURN
      END
