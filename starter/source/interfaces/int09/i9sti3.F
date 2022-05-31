Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  I9STI3                        source/interfaces/int09/i9sti3.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        INELTS                        source/interfaces/inter3d1/inelt.F
Chd|        INSOL3                        source/interfaces/inter3d1/insol3.F
Chd|        LOCAL_INDEX                   source/interfaces/interf1/local_index.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I9STI3(
     1 X        ,IRECT    ,STF      ,IXS      ,PM       ,
     2 GEO      ,NRT      ,IXC      ,STFN     ,NSEG     ,
     3 LNSV     ,NINT     ,NSN      ,NSV      ,SLSFAC   ,
     4 NTY      ,GAP      ,NOINT    ,IXTG     ,IELE     ,
     5 KNOD2ELS ,NOD2ELS  ,IGRSURF  ,ISU      ,
     6 IXS10    ,IXS16    ,IXS20    ,ID       ,TITR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
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
#include      "com04_c.inc"
#include      "nchar_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRT, NINT, NSN, NTY, NOINT,ISU
C     REAL ou REAL*8
      my_real
     .   SLSFAC, GAP
      INTEGER IRECT(4,*), IXS(NIXS,*), IXC(NIXC,*),
     .   NSV(*), IXTG(NIXTG,*), NSEG(*), LNSV(*), IELE(*),
     .   KNOD2ELS(*), NOD2ELS(*), 
     .   IXS10(*), IXS16(*), IXS20(*)
C     REAL ou REAL*8
      my_real
     .   X(3,*), STF(*), PM(NPROPM,*), GEO(NPROPG,*), STFN(*)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, INRT, NELS, J, IG, IL
C
      my_real AREA
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C
C--------------------------------------------------------------
C     CALCUL DES RIGIDITES DES SEGMENTS ET DES NOEUDS
C     V16 : DANS LE CAS OU ONE SEGMENT APPARTIENT A LA FOIS
C           A UNE BRIQUE ET A UNE COQUE ON CHOISIT LA RIGIDITE
C           DE LA COQUE SAUF SI LE MATERIAU COQUE EST NUL.
C---------------------------------------------------------------
C
      DO I=1,NRT
        STF(I)=ZERO
        INRT=I
C
        IF (ISU > 0) THEN
          CALL INELTS(X       ,IRECT  ,IXS     ,NINT    ,NELS              ,
     .                INRT    ,AREA   ,NOINT   ,0       ,IGRSURF(ISU)%ELTYP,
     .                IGRSURF(ISU)%ELEM)
          IF(NELS.NE.0) THEN
            IELE(I) = NELS
            CYCLE
          END IF
        ENDIF
C----------------------
C     ELEMENTS SOLIDES
C----------------------
        CALL INSOL3(X,IRECT,IXS,NINT,NELS,INRT,
     .              AREA,NOINT,KNOD2ELS ,NOD2ELS ,0 ,IXS10,
     .              IXS16,IXS20)
        IF(NELS.NE.0) THEN
         IELE(I) = NELS
        ELSE
           IF(NINT.GT.0) THEN
              CALL ANCMSG(MSGID=92,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=I)
           ENDIF            
           IF(NINT.LT.0) THEN
              CALL ANCMSG(MSGID=93,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=I)
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
