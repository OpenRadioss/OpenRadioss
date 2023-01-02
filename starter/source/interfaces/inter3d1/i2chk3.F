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
Chd|  I2CHK3                        source/interfaces/inter3d1/i2chk3.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        INCOQ3                        source/interfaces/inter3d1/incoq3.F
Chd|        INELTC                        source/interfaces/inter3d1/inelt.F
Chd|        INELTS                        source/interfaces/inter3d1/inelt.F
Chd|        INSOL3                        source/interfaces/inter3d1/insol3.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I2CHK3(X   ,IRECT    ,IXS      ,NRT       ,IXC     ,
     1                  NINT,NSN      ,NSV      ,NOINT     ,
     2                  IXTG,IRTL     ,ST       ,DMIN      ,GEO     ,
     3                  PM  ,KNOD2ELS ,KNOD2ELC ,KNOD2ELTG ,NOD2ELS ,
     4              NOD2ELC ,NOD2ELTG ,IGRSURF  ,THK       ,
     5                  NTY ,IXS10    ,IXS16    ,IXS20     ,IGEO    ,
     6                  PM_STACK    , IWORKSH )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
C---------------------------------------------------------------------
C     CHECK ELEMENT FOR TYPE 2 (ISEARCH=2)
C     LE NOEUD GLOBAL N'EST PAS REMPLACE PAR LE NOEUD LOCAL DANS IRECT
C---------------------------------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRT, NINT, NSN, NOINT,NTY
      INTEGER IRECT(4,*), IXS(NIXS,*), IXC(NIXC,*),
     .   NSV(*), IXTG(NIXTG,*),IRTL(*),
     .   KNOD2ELS(*), KNOD2ELC(*), KNOD2ELTG(*), NOD2ELS(*), NOD2ELC(*), 
     .   NOD2ELTG(*),IXS10(*), IXS16(*), IXS20(*),
     .   IGEO(*),IWORKSH(*)
C
      my_real
     .   X(3,*),ST(2,*),DMIN(*),GEO(NPROPG,*), PM(NPROPM,*),THK(*), 
     .   PM_STACK(*)
      TYPE (SURF_) :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, INRT, NELS, NELC, NELTG, L, JJ, NN
      my_real AREA
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C
      DO I=1,NRT
        INRT=I
C
        CALL INELTS(X           ,IRECT,IXS  ,NINT,NELS         ,
     .              INRT        ,AREA ,NOINT,0   ,IGRSURF%ELTYP,
     .              IGRSURF%ELEM)
        CALL INELTC(NELC ,NELTG ,INRT ,IGRSURF%ELTYP, IGRSURF%ELEM)
        IF(NELS+NELC+NELTG/=0) CYCLE
C      
        CALL INSOL3(X,IRECT,IXS,NINT,NELS,INRT,
     .              AREA,NOINT,KNOD2ELS ,NOD2ELS ,0 ,IXS10,
     .              IXS16,IXS20)
        CALL INCOQ3(IRECT,IXC ,IXTG ,NINT   ,NELC     ,
     .              NELTG,INRT,GEO  ,PM     ,KNOD2ELC ,
     .              KNOD2ELTG ,NOD2ELC ,NOD2ELTG,THK,NTY,IGEO,
     .              PM_STACK  , IWORKSH )
C
        IF(NELS+NELC+NELTG==0)THEN
           CALL ANCMSG(MSGID=93,
     .                 MSGTYPE=MSGWARNING,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 I2=NOINT,
     .                 I1=I)
        ENDIF
      ENDDO
C
      DO I=1,NSN
        IRTL(I)=0
        ST(1,I)=NINE
        ST(2,I)=NINE
        DMIN(I)=EP20
      ENDDO
C
      RETURN
      END
