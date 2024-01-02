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
Chd|  TAGNOD                        source/groups/tagnod.F        
Chd|-- called by -----------
Chd|        HM_LECGRN                     source/groups/hm_lecgrn.F     
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TAGNOD(IX,NIX,NIX1,NIX2,NUMEL,IPARTE,TAGBUF,NPART)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIX,IX(NIX,*),NIX1,NIX2,NUMEL,IPARTE(*),
     1      TAGBUF(*),NPART
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J,L
C=======================================================================
      DO J=1,NUMEL
        IF (TAGBUF(IPARTE(J)) == 1)THEN
          DO L=NIX1,NIX2
            TAGBUF(IX(L,J)+NPART)=1
          ENDDO
        ENDIF
      ENDDO
      DO J=1,NUMEL
        IF (TAGBUF(IPARTE(J)) == -1)THEN
          DO L=NIX1,NIX2
            TAGBUF(IX(L,J)+NPART)=0
          ENDDO
        ENDIF
      ENDDO
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  TAGNODS                       source/groups/tagnod.F        
Chd|-- called by -----------
Chd|        HM_LECGRN                     source/groups/hm_lecgrn.F     
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE TAGNODS(IXS,IXS10,IXS20,IXS16,IPARTS,TAGBUF,IDGRN,TITR)
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),IPARTS(*),IXS10(6,*),IXS20(12,*),IXS16(8,*),
     1      TAGBUF(*),IDGRN
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,L,ITETRA10
C=======================================================================
      DO J=1,NUMELS8
        IF (TAGBUF(IPARTS(J)) == 1)THEN
          DO L=2,9
            TAGBUF(IXS(L,J)+NPART)=1
          ENDDO
        ENDIF
      ENDDO
      DO J=1,NUMELS8
        IF (TAGBUF(IPARTS(J)) == -1)THEN
          DO L=2,9
            TAGBUF(IXS(L,J)+NPART)=0
          ENDDO
        ENDIF
      ENDDO
      ITETRA10=0
C-----------
      DO I=1,NUMELS10
        J = I + NUMELS8
        IF (TAGBUF(IPARTS(J)) == 1)THEN
          ITETRA10=ITETRA10+1
          DO L=2,9
            TAGBUF(IXS(L,J)+NPART)=1
          ENDDO
          DO L=1,6
            IF (IXS10(L,I) /= 0) TAGBUF(IXS10(L,I)+NPART)=1
          ENDDO
        ENDIF
      ENDDO
      DO I=1,NUMELS10
        J = I + NUMELS8
        IF (TAGBUF(IPARTS(J)) == -1)THEN
          ITETRA10=ITETRA10+1
          DO L=2,9
            TAGBUF(IXS(L,J)+NPART)=0
          ENDDO
          DO L=1,6
            IF (IXS10(L,I) /= 0) TAGBUF(IXS10(L,I)+NPART)=1
          ENDDO
        ENDIF
      ENDDO
C-----------
      DO I=1,NUMELS20
        J = I + NUMELS8 + NUMELS10
        IF (TAGBUF(IPARTS(J)) == 1)THEN
          DO L=2,9
            TAGBUF(IXS(L,J)+NPART)=1
          ENDDO
          DO L=1,12
            IF (IXS20(L,I) /= 0) TAGBUF(IXS20(L,I)+NPART)=1
          ENDDO
        ENDIF
      ENDDO
      DO I=1,NUMELS20
        J = I + NUMELS8 + NUMELS10
        IF (TAGBUF(IPARTS(J)) == -1)THEN
          DO L=2,9
            TAGBUF(IXS(L,J)+NPART)=0
          ENDDO
          DO L=1,12
            IF (IXS20(L,I) /= 0) TAGBUF(IXS20(L,I)+NPART)=1
          ENDDO
        ENDIF
      ENDDO
C-----------
      DO I=1,NUMELS16
        J = I + NUMELS8 + NUMELS10 + NUMELS20
        IF (TAGBUF(IPARTS(J)) == 1)THEN
          DO L=2,9
            TAGBUF(IXS(L,J)+NPART)=1
          ENDDO
          DO L=1,8
            IF (IXS16(L,I) /= 0) TAGBUF(IXS16(L,I)+NPART)=1
          ENDDO
        ENDIF
      ENDDO
      DO I=1,NUMELS16
        J = I + NUMELS8 + NUMELS10 + NUMELS20
        IF (TAGBUF(IPARTS(J)) == -1)THEN
          DO L=2,9
            TAGBUF(IXS(L,J)+NPART)=0
          ENDDO
          DO L=1,8
            IF (IXS16(L,I) /= 0) TAGBUF(IXS16(L,I)+NPART)=1
          ENDDO
        ENDIF
      ENDDO
C-----------
      RETURN
      END
