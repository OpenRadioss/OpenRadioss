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
Chd|  SPMD_I24_PREPARE              source/interfaces/int24/i24_prepare.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F
Chd|====================================================================
      SUBROUTINE SPMD_I24_PREPARE(MODE,IPARI, INTBUF_TAB,
     *                         IAD_ELEM, FR_ELEM,INTLIST,NBINTC,
     *                         IAD_I24 , SFR_I24, FR_I24,I24MAXNSNE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE TRI7BOX
        USE MESSAGE_MOD
        USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER MODE
        INTEGER IPARI(NPARI,*),IAD_ELEM(2,*),FR_ELEM(*),
     *          INTLIST(*),NBINTC,IAD_I24(NBINTC+1,NSPMD), SFR_I24,
     *          FR_I24(*),I24MAXNSNE
        TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
        INTEGER ITAG(NUMNOD),SZFRI24,IAFR24
        INTEGER P,I,J,NI,NIN,NOD,NTY,NSN,SN,NSNE
C-----------------------------------------------
C Partie 1, calcul des tailles de FR_I24
C-----------------------------------------------
        ITAG(1:NUMNOD)=0
        IF (MODE==1)THEN

          SZFRI24 = 0

          DO P = 1, NSPMD
            IF(IAD_ELEM(1,P+1)-IAD_ELEM(1,P)>0) THEN
              DO J=IAD_ELEM(1,P),IAD_ELEM(1,P+1)-1
                NOD = FR_ELEM(J)
                ITAG(NOD)=1
              ENDDO
              DO NI=1,NBINTC
                NIN = INTLIST(NI)
                NTY   = IPARI(7,NIN)
                NSN   = IPARI(5,NIN)

                IF(NTY==24) THEN
                  DO I=1,NSN
                    SN = INTBUF_TAB(NIN)%NSV(I)
C Fictives T24 E2E Nodes hav NSV over Numnod
                    IF(SN<=NUMNOD)THEN
                      IF(ITAG(SN)==1)THEN
                        SZFRI24 = SZFRI24+1
                      ENDIF
                    ENDIF
                  ENDDO
                ENDIF
              ENDDO
C Flush ITAG to zero again for next CPU
              DO J=IAD_ELEM(1,P),IAD_ELEM(1,P+1)-1
                NOD = FR_ELEM(J)
                ITAG(NOD)=0
              ENDDO
            ENDIF
          ENDDO
          SFR_I24=SZFRI24
C-----------------------------------------------
C Partie 2, on remplis FR_I24 et IAD_I24
C-----------------------------------------------
        ELSEIF(MODE==2)THEN
          IAD_I24(1,1)=1
          IAFR24=0
          DO P = 1, NSPMD
            IF(IAD_ELEM(1,P+1)-IAD_ELEM(1,P)>0) THEN
              DO J=IAD_ELEM(1,P),IAD_ELEM(1,P+1)-1
                NOD = FR_ELEM(J)
                ITAG(NOD)=1
              ENDDO

              DO NI=1,NBINTC
                IAD_I24(NI+1,P)=IAD_I24(NI,P)
                NIN = INTLIST(NI)
                NTY   = IPARI(7,NIN)
                NSN   = IPARI(5,NIN)

                IF(NTY==24) THEN
                  DO I=1,NSN
                    SN = INTBUF_TAB(NIN)%NSV(I)
C Fictives T24 E2E Nodes hav NSV over Numnod
                    IF(SN<=NUMNOD)THEN
                      IF(ITAG(SN)==1)THEN
                        IAD_I24(NI+1,P)=IAD_I24(NI+1,P)+1
                        IAFR24 = IAFR24 + 1
                        FR_I24(IAFR24)=I
                      ENDIF
                    ENDIF
                  ENDDO
                ENDIF
              ENDDO
C Flush ITAG to zero again for next CPU
              DO J=IAD_ELEM(1,P),IAD_ELEM(1,P+1)-1
                NOD = FR_ELEM(J)
                ITAG(NOD)=0
              ENDDO
            ELSE
              DO I=2,NBINTC+1
                IAD_I24(I,P)=IAD_I24(1,P)
              ENDDO
            ENDIF    ! IF(IAD_ELEM(1,P+1)-IAD_ELEM(1,P)>0)
            IF(P<NSPMD)IAD_I24(1,P+1)=IAD_I24(NBINTC+1,P)
          ENDDO
        ENDIF

C E2E SPMD
C Compute Max. number of Fictive E2E Nodes in Type24
C This is needed for Tags over nodes
C
        IF(MODE==2)THEN
          I24MAXNSNE=0
          DO NI=1,NBINTC
            NIN = INTLIST(NI)
            NTY   = IPARI(7,NIN)
            IF(NTY==24)THEN
              NSNE=IPARI(55,NIN)
              I24MAXNSNE=MAX(I24MAXNSNE,NSNE)
            ENDIF
          ENDDO
        ENDIF

        I24COM3 = 0
        I24COM4 = 0
C-----------------------------------------------
      END

