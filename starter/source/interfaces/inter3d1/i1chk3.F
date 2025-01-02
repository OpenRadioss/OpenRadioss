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
      !||    i1chk3                 ../starter/source/interfaces/inter3d1/i1chk3.F
      !||--- called by ------------------------------------------------------
      !||    inint3                 ../starter/source/interfaces/inter3d1/inint3.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                 ../starter/source/output/message/message.F
      !||    incoq3                 ../starter/source/interfaces/inter3d1/incoq3.F
      !||    ineltc                 ../starter/source/interfaces/inter3d1/inelt.F
      !||    inelts                 ../starter/source/interfaces/inter3d1/inelt.F
      !||    insol3                 ../starter/source/interfaces/inter3d1/insol3.F
      !||    local_index            ../starter/source/interfaces/interf1/local_index.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod            ../starter/share/message_module/message_mod.F
      !||====================================================================
      SUBROUTINE I1CHK3(X        ,IRECT   ,IXS      ,NRT      ,IXC       ,
     .                  NINT     ,NSN     ,NSV      ,NOINT    ,IXTG      ,
     .                  GEO      ,PM      ,KNOD2ELS ,KNOD2ELC ,KNOD2ELTG ,
     .                  NOD2ELS  ,NOD2ELC ,NOD2ELTG ,IGRSURF  ,
     .                  THK      ,NTY     ,IXS10    ,
     .                  IXS16    ,IXS20   ,ID,TITR  ,IGEO     ,
     .                  PM_STACK ,IWORKSH )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C     CHECK ELEMENT FOR TYPE 1 AND 2 (ISEARCH=1)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
      USE MESSAGE_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRT, NINT, NSN, NOINT, NTY
      INTEGER IRECT(4,*), IXS(NIXS,*), IXC(NIXC,*),
     .   NSV(*), IXTG(NIXTG,*),
     .   KNOD2ELS(*), KNOD2ELC(*), KNOD2ELTG(*), NOD2ELS(*), NOD2ELC(*), 
     .   NOD2ELTG(*), IXS10(*), IXS16(*), IXS20(*),
     .   IGEO(*), IWORKSH(*)
      my_real
     .   X(3,*),GEO(NPROPG,*), PM(NPROPM,*),THK(*),PM_STACK(*)
      INTEGER ID
      CHARACTER(LEN=NCHARTITLE) :: TITR
      TYPE (SURF_) :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, INRT, NELS, NELC, NELTG, J, IG, IL
      my_real AREA
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      DO I=1,NRT
        INRT=I
        CALL INELTS(X           ,IRECT,IXS  ,NINT,NELS         ,
     .              INRT        ,AREA ,NOINT,0   ,IGRSURF%ELTYP,
     .              IGRSURF%ELEM)
        CALL INELTC(NELC ,NELTG ,INRT ,IGRSURF%ELTYP, IGRSURF%ELEM)
        IF(NELS+NELC+NELTG/=0) CYCLE
        CALL INSOL3(X,IRECT,IXS,NINT,NELS,INRT,
     .              AREA,NOINT,KNOD2ELS ,NOD2ELS ,0 ,IXS10,
     .              IXS16,IXS20)
        CALL INCOQ3(IRECT,IXC ,IXTG ,NINT ,NELC     ,
     .              NELTG,INRT,GEO  ,PM   ,KNOD2ELC ,
     .              KNOD2ELTG ,NOD2ELC ,NOD2ELTG,THK,NTY,IGEO,
     .              PM_STACK , IWORKSH )
        IF(NELS+NELC+NELTG==0)THEN
           IF(NINT>0) THEN
              CALL ANCMSG(MSGID=92,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=I)
           ENDIF
           IF(NINT<0) THEN
              CALL ANCMSG(MSGID=93,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=I)
           ENDIF
        ENDIF
      ENDDO!next I
      !LE NOEUD GLOBAL EST REMPLACE PAR LE NOEUD LOCAL DANS IRECT
      DO I=1,NRT
        DO J=1,4
          IG=IRECT(J,I)
          CALL LOCAL_INDEX(IL,IG,NSV,NSN)
          IRECT(J,I)=IL
        ENDDO
      ENDDO
C-----------------------------------------------
      RETURN
      END
