Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
      !||====================================================================
      !||    lgmini_i7              ../starter/source/tools/lagmul/lgmini_i7.F
      !||--- called by ------------------------------------------------------
      !||    lectur                 ../starter/source/starter/lectur.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                 ../starter/source/output/message/message.F
      !||    fretitl2               ../starter/source/starter/freform.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod            ../starter/share/message_module/message_mod.F
      !||====================================================================
      SUBROUTINE LGMINI_I7( IPARI, INTBUF_TAB, MASS, ITAB, IGRNOD, NOM_OPT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD
      USE GROUPDEF_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  IPARI(NPARI,NINTER),ITAB(*)   
      my_real MASS(*)
      INTEGER NOM_OPT(LNOPT1,*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, NTY , NN0, NSN, ILAGM, NRTS,NRTM, NMN, NAD, IGN
      INTEGER ID
      CHARACTER(LEN=NCHARTITLE) :: TITR
C======================================================================|
      DO N=1,NINTER
        ID=NOM_OPT(1,N)
        CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,N),LTITR)
         NTY=IPARI(7,N)
         ILAGM=0
         IF(CODVERS>=44) ILAGM=IPARI(33,N)
           IF(NTY==7.AND.ILAGM==1) THEN
             NSN   =IPARI(5,N)
             NMN   =IPARI(6,N)
             DO I=1,NSN
               NN0=INTBUF_TAB(N)%NSV(I)
               IF(MASS(NN0)==0)THEN
                 CALL ANCMSG(MSGID=535,
     .                       MSGTYPE=MSGERROR,
     .                       ANMODE=ANINFO_BLIND_1,
     .                       C1='INTERFACE TYPE7',
     .                       I1=ID,
     .                       C2='INTERFACE TYPE7',
     .                       C3=TITR,C4='SECONDARY',
     .                       I2=ITAB(NN0))
               ENDIF  
             ENDDO
           ELSE
            IF(NTY==16.AND.ILAGM==1) THEN
             IGN  =IPARI(36,N)
             NRTM  =IPARI(4,N)
             NSN   =IPARI(5,N)
             NMN   =IPARI(6,N)
             DO I=1,NSN
               NN0=IGRNOD(IGN)%ENTITY(I)
               IF(MASS(NN0)==0)THEN
                 CALL ANCMSG(MSGID=535,
     .                       MSGTYPE=MSGERROR,
     .                       ANMODE=ANINFO_BLIND_1,
     .                       C1='INTERFACE TYPE7',
     .                       I1=ID,
     .                       C2='INTERFACE TYPE7',
     .                       C3=TITR,C4='SECONDARY',
     .                       I2=ITAB(NN0))
               ENDIF  
             ENDDO
            ENDIF
           ENDIF 
      ENDDO
      RETURN
      END
