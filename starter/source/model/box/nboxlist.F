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
Chd|  NBOXLST                       source/model/box/nboxlist.F   
Chd|-- called by -----------
Chd|        HM_READ_BOX                   source/model/box/hm_read_box.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      INTEGER FUNCTION NBOXLST(LIST  ,NLIST  ,IBOXTMP,NBBOX  ,
     .                         IX1   ,IX2    ,INDEX  ,KK     ,ID   ,TITR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C      FONCTION DONNE N0 SYSTEME D'UNE LISTE DE BOXES USER
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NLIST,KK,NBBOX,ID
      INTEGER LIST(NLIST),INDEX(NBBOX*3),IBOXTMP(NBBOX),IX1(NBBOX),IX2(NBBOX)
      CHARACTER(nchartitle) ,INTENT(IN) :: TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NBOX,NOLD,K,K0,K1,KALL,ID0,
     .        IWORK(70000),ISIGN(NLIST),ISIGN1(NLIST),
     .        SIGNOLD,FAC(NLIST+1),INDXOLD,FAC1(NLIST+1),
     .        FACX,LIST1(NLIST),IDEL(NLIST),IDBL(NLIST)
      CHARACTER BOX*3
C-----------------------
C TRI DE LIST EN ORDRE CROISSANT
C-----------------------
        DO I=1,NLIST
          ISIGN(I) = SIGN(1,LIST(I))
          LIST(I) = ABS(LIST(I))
        ENDDO
C---
        CALL MY_ORDERS(0,IWORK,LIST,INDEX,NLIST,1)
        DO I=1,NLIST
          INDEX(NLIST+I) = LIST(INDEX(I))
          ISIGN1(I)= ISIGN(INDEX(I))
        ENDDO
C---
        DO I=1,NLIST
          LIST(I)  = INDEX(NLIST+I)
          ISIGN(I) = ISIGN1(I)
        ENDDO
C---
        NBOX = NLIST
C-----------------------
C TRI DE IBOX() EN ORDRE CROISSANT si KK = 0
C-----------------------
        IF (KK == 0) THEN
          DO I=1,NBBOX
            IX2(I) = IBOXTMP(I)
          ENDDO
          CALL MY_ORDERS(0,IWORK,IX2,INDEX,NBBOX,1)
          DO I=1,NBBOX
            IX1(I) = IX2(INDEX(I))
          ENDDO
          DO I=1,NBBOX
            IX2(I) = INDEX(I)
          ENDDO
        ENDIF
C-----------------------
C RECHERCHE DES ELEMENTS DE LIST() DANS IBOX()
C  ALGO < NLIST+NBBOX
C-----------------------
        I=1
        J=1
        DO I=1,NBOX
          DO WHILE(ABS(LIST(I)) > IX1(J).AND. J < NBBOX)
            J=J+1
          ENDDO
          IF (ABS(LIST(I)) == IX1(J))THEN
            LIST(I) = IX2(J)*ISIGN(I)
          ELSE
            CALL ANCMSG(MSGID=795,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=LIST(I))
            NBOXLST=I-1
            RETURN
          ENDIF
        ENDDO
C---
        NBOXLST = NBOX
C---
        RETURN
        END
