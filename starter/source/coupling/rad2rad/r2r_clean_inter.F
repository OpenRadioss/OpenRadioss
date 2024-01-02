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
Chd|  R2R_CLEAN_INTER               source/coupling/rad2rad/r2r_clean_inter.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        R2R_CHECK_SEG                 source/coupling/rad2rad/routines_r2r.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        RESTMOD                       share/modules1/restart_mod.F  
Chd|====================================================================
      SUBROUTINE R2R_CLEAN_INTER(IPARI2,INTBUF_TAB,IPARTC,IPARTG,IPARTS,ISOLNOD)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE RESTMOD
        USE R2R_MOD
        USE INTBUFDEF_MOD
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
        INTEGER IPARI2(NPARI,*),IPARTC(*),IPARTG(*),IPARTS(*),ISOLNOD(*)
        TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER NI,I,J,K,L,NTYP,CUR_ID,IAD,NSN,NRTM,NRTS
        INTEGER ELTAG,FACE(4)
C-----------------------------------------------

C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
        INTEGER USR2SYS
        INTEGER NLOCAL
C-----------------------------------------------

        DO NI=1,NINTER
          NTYP = IPARI2(7,NI)
          IF ((NTYP==7).OR.(NTYP==18).OR.(NTYP==10).OR.(NTYP==24)) THEN
            NSN   =IPARI2(5,NI)
            NRTS  =IPARI2(3,NI)
            NRTM  =IPARI2(4,NI)
C------------------------>   check of secondary nodes  <----------C
            DO J=1,NSN
              CUR_ID = INTBUF_TAB(NI)%NSV(J)
              IF ((TAGNO(CUR_ID+NPART)==2).AND.(TAGNO(CUR_ID+NPART+NUMNOD)==-1)) THEN
                INTBUF_TAB(NI)%STFNS(J) = ZERO
              ENDIF
            ENDDO

C------------------------>   check of main elements <--------------C
            DO J=1,NRTM
              FACE(1) = INTBUF_TAB(NI)%IRECTM(4*(J-1)+1)
              FACE(2) = INTBUF_TAB(NI)%IRECTM(4*(J-1)+2)
              FACE(3) = INTBUF_TAB(NI)%IRECTM(4*(J-1)+3)
              FACE(4) = INTBUF_TAB(NI)%IRECTM(4*(J-1)+4)
              IF (FACE(4)==0) FACE(4)=FACE(3)
              CALL R2R_CHECK_SEG(ELTAG,FACE,IPARTC,IPARTG,IPARTS,ISOLNOD)
              IF (ELTAG==0) INTBUF_TAB(NI)%STFM(J) = ZERO
            ENDDO

          ENDIF
        END DO

C-----------

        RETURN
      END SUBROUTINE R2R_CLEAN_INTER
