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
Chd|  I12CHK3                       source/interfaces/inter3d1/i12chk3.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        I12SOL3                       source/interfaces/inter3d1/insol3.F
Chd|        ICINV3                        source/interfaces/inter3d1/icinv3.F
Chd|        LOCAL_INDEX                   source/interfaces/interf1/local_index.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I12CHK3(X   ,IRECT,IXS,NRT   ,IXC,
     .                  NINT,NSN  ,NSV,NOINT ,MWA,
     .                  IXTG,GEO,PM,ISEG,ALE_CONNECTIVITY,
     .                  VARCONVINT,FCOUNT,ITIED,ITAB,KNOD2ELS,
     .                  NOD2ELS,NTY,ID,TITR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------
C     CHECK ELEMENT FOR TYPE 12
C------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRT, NINT, NSN, NOINT, NTY, ID, ITIED
      INTEGER IRECT(4,*), IXS(NIXS,*), IXC(NIXC,*), MWA(*),
     .   NSV(*), IXTG(NIXTG,*), ISEG(*), VARCONVINT,
     .   ITAB(*), FCOUNT(*), KNOD2ELS(*), NOD2ELS(*)
      my_real
     .   X(3,*),GEO(NPROPG,*), PM(NPROPM,*)
      CHARACTER*nchartitle,
     .        TITR
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, INRT, NELS, J, NN, NF, M, IG, IL, VARCONV,IAD
      my_real
     .   AREA
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------

      IF(ITIED/=3)THEN
        CALL ICINV3(IXS,IXC,IXTG,MWA,MWA(1+(2+NUMNOD)))
        IF(IPRI>=1 .AND. NINT<0)WRITE(IOUT,1000)
        IF(IPRI>=1 .AND. NINT>0)WRITE(IOUT,1001)
        DO I=1,NRT
          INRT=I
          CALL I12SOL3(X,IRECT,IXS,NINT,NELS,INRT,
     .                 AREA,NOINT,MWA ,MWA(1+(2+NUMNOD)),NF,ITAB,
     .                 KNOD2ELS,NOD2ELS,NTY,ID,TITR)
          IF(NELS/=0) THEN
            M=IXS(1,NELS)
            VARCONV=INT(PM(10,M))
            IF(VARCONVINT==-1)VARCONVINT=VARCONV
            IF(VARCONV==VARCONVINT)THEN
              SEGINDX = SEGINDX+1
              ISEG(I) = SEGINDX
              IAD = ALE_CONNECTIVITY%ee_connect%iad_connect(NELS)
              ALE_CONNECTIVITY%ee_connect%connected(IAD + NF - 1) = -SEGINDX
              IF(IPRI>=1)
     .        WRITE(IOUT,FMT=FMW_10I)I,IXS(11,NELS),NF,SEGINDX,NELS
            ELSE
              IF(NINT>0) WRITE (IOUT,1101) I, NOINT
              IF(NINT<0) WRITE (IOUT,1201) I, NOINT
              IERR=IERR+1
            ENDIF
          ELSE
            IF(NINT>0) WRITE (IOUT,1100) I, NOINT
            IF(NINT<0) WRITE (IOUT,1200) I, NOINT
            IERR=IERR+1
          ENDIF !(NELS/=0)
        END DO !I=1,NRT 
        CALL ANCMSG(MSGID=1245,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND,
     .                  I1=ID,
     .                  C1=TITR,
     .                  PRMOD=MSG_PRINT)
        CALL ANCMSG(MSGID=1246,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND,
     .                  I1=ID,
     .                  C1=TITR,
     .                  PRMOD=MSG_PRINT) 
      END IF !(ITIED/=3)
      
      !------------------------------------------------------------------------!
      !     LE NOEUD GLOBAL EST REMPLACE PAR LE NOEUD LOCAL DANS IRECT         !
      !------------------------------------------------------------------------!
      DO I=1,NRT
        DO J=1,4
          IG=IRECT(J,I)
          CALL LOCAL_INDEX(IL,IG,NSV,NSN)
          IRECT(J,I)=IL
        ENDDO
      ENDDO
      !------------------------------------------------------------------------!
      !     COMPTAGE DU NOMBRE DE FACETTES CONNECTEES PAR NOEUD (SECONDARY ONLY!)  !
      !------------------------------------------------------------------------!
      IF(NINT>0)THEN
         DO  I=1,NSN
           FCOUNT(I)=0
         ENDDO
         DO  I=1,NRT
           DO J=1,4
             NN=IRECT(J,I)
             FCOUNT(NN)=FCOUNT(NN)+1
           ENDDO
         ENDDO
      ENDIF

      RETURN
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----A----+----B----+----C
 1000 FORMAT(//'  MAIN     ELT    FACE        ')
 1001 FORMAT(//'  SECONDARY      ELT    FACE        ')
 1100 FORMAT(2X,'** ERROR ** CANNOT FIND FLUID ELT OF MAIN  SEGMENT',I10,' OF INTERFACE',I10)
 1200 FORMAT(2X,'** ERROR ** CANNOT FIND FLUID ELT OF SECONDARY   SEGMENT',I10,' OF INTERFACE',I10)
 1101 FORMAT(2X,'** ERROR ** VARIABLES NOT CONSISTENT IN SECONDARY  ELEMENT',I10,' FOR INTERFACE',I10)
 1201 FORMAT(2X,'** ERROR ** VARIABLES NOT CONSISTENT IN MAIN ELEMENT',I10,' FOR INTERFACE',I10)
       END
