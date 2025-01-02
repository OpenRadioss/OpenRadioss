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
      !||    i9sti3                 ../starter/source/interfaces/int09/i9sti3.F
      !||--- called by ------------------------------------------------------
      !||    inint3                 ../starter/source/interfaces/inter3d1/inint3.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                 ../starter/source/output/message/message.F
      !||    inelts                 ../starter/source/interfaces/inter3d1/inelt.F
      !||    insol3                 ../starter/source/interfaces/inter3d1/insol3.F
      !||    local_index            ../starter/source/interfaces/interf1/local_index.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod            ../starter/share/message_module/message_mod.F
      !||====================================================================
      SUBROUTINE I9STI3(
     1 X        ,IRECT    ,STF      ,IXS      ,
     2 NRT      ,
     3 NINT     ,NSN      ,NSV      ,
     4 NOINT    ,IELE     ,
     5 KNOD2ELS ,NOD2ELS  ,IGRSURF  ,ISU      ,
     6 IXS10    ,IXS16    ,IXS20    ,ID       ,TITR)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C     NODE AND SEGMENT STIFFNESS
C       specific case : segment belongs to both solid and shell elem ; shell stiffness is used (if > 0)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
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
      INTEGER NRT, NINT, NSN, NOINT,ISU
      INTEGER IRECT(4,*), IXS(NIXS,*),
     .   NSV(*),  IELE(*),
     .   KNOD2ELS(*), NOD2ELS(*), 
     .   IXS10(*), IXS16(*), IXS20(*)
      my_real X(3,*), STF(*)
      INTEGER ID
      CHARACTER(LEN=NCHARTITLE) :: TITR
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, INRT, NELS, J, IG, IL
      my_real AREA
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

      DO I=1,NRT
        STF(I)=ZERO
        INRT=I
C
        IF (ISU > 0) THEN
          CALL INELTS(X       ,IRECT  ,IXS     ,NINT    ,NELS              ,
     .                INRT    ,AREA   ,NOINT   ,0       ,IGRSURF(ISU)%ELTYP,
     .                IGRSURF(ISU)%ELEM)
          IF(NELS/=0) THEN
            IELE(I) = NELS
            CYCLE
          END IF
        ENDIF
C----------------------
C     SOLID ELEMS
C----------------------
        CALL INSOL3(X,IRECT,IXS,NINT,NELS,INRT,
     .              AREA,NOINT,KNOD2ELS ,NOD2ELS ,0 ,IXS10,
     .              IXS16,IXS20)
        IF(NELS/=0) THEN
         IELE(I) = NELS
        ELSE
           IF(NINT>0) THEN
              CALL ANCMSG(MSGID=92,MSGTYPE=MSGWARNING,ANMODE=ANINFO_BLIND_2,I1=ID,C1=TITR,I2=I)
           ENDIF            
           IF(NINT<0) THEN
              CALL ANCMSG(MSGID=93,MSGTYPE=MSGWARNING,ANMODE=ANINFO_BLIND_2,I1=ID,C1=TITR,I2=I)
           ENDIF
        ENDIF
C
      ENDDO
C---------------------------------------------
C
      DO I=1,NRT
        DO J=1,4
          IG=IRECT(J,I)
          CALL LOCAL_INDEX(IL,IG,NSV,NSN)
          IRECT(J,I)=IL
        ENDDO
      ENDDO
C
      RETURN
      END
