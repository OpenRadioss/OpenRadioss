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
Chd|  LECREFSTA                     source/loads/reference_state/refsta/lecrefsta.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        R2R_SYS                       source/coupling/rad2rad/routines_r2r.F
Chd|        USR2SYS                       source/system/sysfus.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE LECREFSTA(ITABM1  ,UNITAB,IXC   ,IXTG  ,IXS   ,
     .                     XYZREF  ,XREFC ,XREFTG,XREFS ,TAGNOD,
     .                     IDDLEVEL,TAGREF )
        USE UNITAB_MOD
        USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB
        INTEGER ITABM1(*),IXC(NIXC,*),IXTG(NIXTG,*),IXS(NIXS,*),TAGNOD(*)
        INTEGER IDDLEVEL,TAGREF(*)
        my_real
     .    XREFC(4,3,*),XREFTG(3,3,*),XREFS(8,3,*),
     .    XYZREF(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER  I,J,IE,IN,ID,NN,IFLAGUNIT,NNOD
C     REAL
        my_real
     .    XX,YY,ZZ,FAC_L
        CHARACTER MESS*40
        DATA MESS/'REFSTA'/
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
        INTEGER USR2SYS,R2R_SYS
C=======================================================================
C--------------------------------------
C  LECTURE DES NOEUDS ETAT DE REFERENCE
C--------------------------------------
        IF(IDDLEVEL == 0) THEN
          WRITE(IOUT,1000)
          IF(IPRI >= 5) WRITE(IOUT,'(8X,A7,3(18X,A2))') 'NODE-ID',' X',' Y',' Z'
        ENDIF
        NNOD=0
        DO
          READ(IIN6,FMT='(A)',END=799,ERR=798)LINE
          IF (LINE(1:1) == '#') CYCLE
          IF (RS0_FMT == 1)THEN
            READ(LINE,'(I8,3F16.0)', ERR=797) ID,XX,YY,ZZ
          ELSE
            READ(LINE,'(I10,3F20.0)',ERR=797) ID,XX,YY,ZZ
          ENDIF
          IF (ID <= 0) CYCLE
          IF (NSUBDOM == 0) NN = USR2SYS(ID,ITABM1,MESS,0)
          IF (NSUBDOM  > 0) THEN
            NN = R2R_SYS(ID,ITABM1,MESS)
            IF (NN == 0) CYCLE
          ENDIF
          TAGREF(NN) = 1
          IF (TAGNOD(NN) == 0) THEN
            NNOD=NNOD+1
            IF(IDDLEVEL == 0.AND.IPRI >= 5) WRITE(IOUT,'(5X,I10,5X,1P3G20.13)') ID,XX,YY,ZZ
            XYZREF(1,NN) = XX
            XYZREF(2,NN) = YY
            XYZREF(3,NN) = ZZ
          ELSEIF(IDDLEVEL == 0) THEN
C   ERROR : THIS NODE IS ALSO DEFINED IN XREF
            CALL ANCMSG(MSGID=1034,
     .                  MSGTYPE=MSGERROR,ANMODE=ANINFO,
     .                  I1=ID)
          ENDIF
        ENDDO
C-------------
 797    CONTINUE
        CALL ANCMSG(MSGID=733,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              C1=LINE)
 798    CONTINUE
        CALL ANCMSG(MSGID=734,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO)
 799    CONTINUE
        IF(IDDLEVEL == 0.AND.IPRI < 5) WRITE(IOUT,1010) NNOD
C-------------
        DO IE=1,NUMELC
          DO IN=1,4
            NN = IXC(IN+1,IE)
            IF (TAGNOD(NN) == 0)THEN
              XREFC(IN,1,IE) = XYZREF(1,NN)
              XREFC(IN,2,IE) = XYZREF(2,NN)
              XREFC(IN,3,IE) = XYZREF(3,NN)
            ENDIF
          ENDDO
        ENDDO
        DO IE=1,NUMELTG
          DO IN=1,3
            NN = IXTG(IN+1,IE)
            IF (TAGNOD(NN) == 0)THEN
              XREFTG(IN,1,IE) = XYZREF(1,NN)
              XREFTG(IN,2,IE) = XYZREF(2,NN)
              XREFTG(IN,3,IE) = XYZREF(3,NN)
            ENDIF
          ENDDO
        ENDDO
        DO IE=1,NUMELS8
          DO IN=1,8
            NN = IXS(IN+1,IE)
            IF (TAGNOD(NN) == 0)THEN
              XREFS(IN,1,IE) = XYZREF(1,NN)
              XREFS(IN,2,IE) = XYZREF(2,NN)
              XREFS(IN,3,IE) = XYZREF(3,NN)
            ENDIF
          ENDDO
        ENDDO
C-----------
        RETURN
 1000   FORMAT(//
     &   5X,'    REFERENCE STATE (REFSTA)  ',/
     &   5X,'    ------------------------  ',/)
 1010   FORMAT(
     &   5X,'NUMBER OF NODES . . . . . . . . =',I10)
      END SUBROUTINE LECREFSTA
