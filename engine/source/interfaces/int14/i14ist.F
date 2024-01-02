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
Chd|  I14IST                        source/interfaces/int14/i14ist.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I14IST(IPARI,INTBUF_TAB,IGRSURF,BUFSF)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE INTBUFDEF_MOD
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*)
      my_real 
     .  BUFSF(*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, NRTM, NTY, NRTS
      INTEGER ISRF, ADRBUF
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      DO N=1,NINTER
        NTY=IPARI(7,N)
        IF (NTY==14 .OR. NTY==15) THEN
          !Surfaces mains (une seule surface ...).
          NRTM   = IPARI(4,N)
          ISRF   = INTBUF_TAB(N)%KSURF(1)
          ADRBUF = IGRSURF(ISRF)%IAD_BUFR
          !Forces exercees sur l'ellipsoide (somme des ... / interfaces).
          BUFSF(ADRBUF+25)=ZERO
          BUFSF(ADRBUF+26)=ZERO
          BUFSF(ADRBUF+27)=ZERO   
          !Moments exerces sur l'ellipsoide (somme des ... / interfaces).
          BUFSF(ADRBUF+28)=ZERO
          BUFSF(ADRBUF+29)=ZERO
          BUFSF(ADRBUF+30)=ZERO   
          !Rigidites d'interface en translation (somme des ... / interfaces).
          BUFSF(ADRBUF+31)=ZERO
          !Rigidites d'interface en rotation (somme des ... / interfaces).
          BUFSF(ADRBUF+32)=ZERO
        ENDIF
      ENDDO !next N
C-----------------------------------------------
      RETURN
      END
