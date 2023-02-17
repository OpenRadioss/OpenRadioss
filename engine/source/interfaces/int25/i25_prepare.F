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
      SUBROUTINE SPMD_I25_PREPARE(MODE,IPARI, INTBUF_TAB,
     *                         IAD_ELEM, FR_ELEM,INTLIST,NBINTC,
     *                         IAD_I25 , SFR_I25, FR_I25)
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
#include      "r4r8_p.inc"
#include      "spmd_c.inc"
#include      "assert.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "scr07_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MODE
      INTEGER IPARI(NPARI,*),IAD_ELEM(2,*),FR_ELEM(*),
     *        INTLIST(*),NBINTC,IAD_I25(NBINTC+1,NSPMD), SFR_I25,
     *        FR_I25(*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER SZFRI25,IAFR25
      INTEGER P,I,J,NI,NIN,NOD,NTY,NSN,SN
      INTEGER, DIMENSION(:), ALLOCATABLE :: ITAG
C-----------------------------------------------
C Partie 1, calcul des tailles de FR_I25
C-----------------------------------------------
      ALLOCATE(ITAG(NUMNOD))
      ITAG(1:NUMNOD)=0
      IF (MODE==1)THEN

        SZFRI25 = 0
      
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

              IF(NTY==25) THEN
                DO I=1,NSN
                  SN = INTBUF_TAB(NIN)%NSV(I)
                  IF(ITAG(SN)==1)THEN
                    SZFRI25 = SZFRI25+1
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
        SFR_I25=SZFRI25
      
C-----------------------------------------------
C Partie 2, on remplis FR_I25 et IAD_I25
C-----------------------------------------------
      ELSEIF(MODE==2)THEN
        IAD_I25(1,1)=1
        IAFR25=0
        DO P = 1, NSPMD
          IF(IAD_ELEM(1,P+1)-IAD_ELEM(1,P)>0) THEN
            DO J=IAD_ELEM(1,P),IAD_ELEM(1,P+1)-1
              NOD = FR_ELEM(J)
              ITAG(NOD)=1
            ENDDO

            DO NI=1,NBINTC
	      IAD_I25(NI+1,P)=IAD_I25(NI,P)
	      NIN = INTLIST(NI)
              NTY   = IPARI(7,NIN)
              NSN   = IPARI(5,NIN)

              IF(NTY==25) THEN
                DO I=1,NSN
                  SN = INTBUF_TAB(NIN)%NSV(I)
                  ASSERT(SN > 0)
                  ASSERT(SN <= NUMNOD)
                  IF(ITAG(SN)==1)THEN		 
                    IAD_I25(NI+1,P)=IAD_I25(NI+1,P)+1
		    IAFR25 = IAFR25 + 1
                    FR_I25(IAFR25)=I
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
	      IAD_I25(I,P)=IAD_I25(1,P)
	    ENDDO	  
          ENDIF    ! IF(IAD_ELEM(1,P+1)-IAD_ELEM(1,P)>0)
	  IF(P<NSPMD)IAD_I25(1,P+1)=IAD_I25(NBINTC+1,P)
        ENDDO
      ENDIF
      

      DEALLOCATE(ITAG)
      RETURN
C-----------------------------------------------
      END
    
