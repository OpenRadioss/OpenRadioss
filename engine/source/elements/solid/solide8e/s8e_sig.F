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
Chd|====================================================================
Chd|  S8E_SIGP                      source/elements/solid/solide8e/s8e_sig.F
Chd|-- called by -----------
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|-- calls ---------------
Chd|        S8ESIGP3I                     source/elements/solid/solide8e/s8esigp3i.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE S8E_SIGP(
     1   ELBUF_TAB,SIGP,NEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
C     REAL
      my_real
     .   SIGP(NEL,6)
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ILAY,IP,IR,IS,IT,NPTR,NPTS,NPTT,I,J
      INTEGER EPSIP(NEL)
c-----------------------------------------------------
      my_real 
     .   EPMIN(NEL)
C-----
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
C-----------------------------------------------
      GBUF   => ELBUF_TAB%GBUF
      NPTR   = ELBUF_TAB%NPTR
      NPTS   = ELBUF_TAB%NPTS
      NPTT   = ELBUF_TAB%NPTT
        EPMIN(1:NEL)=GBUF%PLA(1:NEL) 
        EPSIP(1:NEL)=1 
        ILAY = 1
       DO IR=1,NPTR
        DO IS=1,NPTS
        DO IT=1,NPTT
C-----------
         IP = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
         LBUF => ELBUF_TAB%BUFLY(ILAY)%LBUF(IR,IS,IT)
         
         DO I=1,NEL
          IF (LBUF%PLA(I)<=EPMIN(I).AND.LBUF%PLA(I)>ZERO) THEN
           EPSIP(I) = IP
           EPMIN(I) = LBUF%PLA(I)
          END IF
         END DO    
C
        ENDDO    !  IT=1,NPTT
        ENDDO     !  IS=1,NPTS
       ENDDO      !  IR=1,NPTR
C       
       DO IR=1,NPTR
        DO IS=1,NPTS
        DO IT=1,NPTT
C-----------
         IP = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
         LBUF => ELBUF_TAB%BUFLY(ILAY)%LBUF(IR,IS,IT)
         
         CALL S8ESIGP3I(1  ,NEL  ,SIGP ,LBUF%SIG,EPSIP,IP,NEL  )
C
        ENDDO    !  IT=1,NPTT
        ENDDO     !  IS=1,NPTS
       ENDDO      !  IR=1,NPTR
C       
      RETURN
      END SUBROUTINE S8E_SIGP
