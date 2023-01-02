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
C
Chd|====================================================================
Chd|  I3STI3                        source/interfaces/inter3d1/i3sti3.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        INCOQ3                        source/interfaces/inter3d1/incoq3.F
Chd|        INELTC                        source/interfaces/inter3d1/inelt.F
Chd|        INELTS                        source/interfaces/inter3d1/inelt.F
Chd|        INSOL3                        source/interfaces/inter3d1/insol3.F
Chd|        LOCAL_INDEX                   source/interfaces/interf1/local_index.F
Chd|        VOLINT                        source/interfaces/inter3d1/volint.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I3STI3(
     1 X     ,IRECT ,STF   ,IXS   ,PM    ,
     2 GEO   ,NRT   ,IXC   ,STFN  ,NSEG  ,
     3 LNSV  ,NINT  ,NSN   ,NSV   ,SLSFAC,
     4 NTY   ,GAP   ,NOINT ,IXTG  ,IR    ,
     5 KNOD2ELS ,KNOD2ELC ,KNOD2ELTG ,NOD2ELS ,NOD2ELC ,
     6 NOD2ELTG ,IGRSURF  ,THK     ,IXS10   ,
     7 IXS16    ,IXS20    ,ID,TITR   ,GAPN    ,STF8    ,
     8 DEPTH    ,FMAX     ,IGEO      ,FILLSOL ,PM_STACK,
     9 IWORKSH )
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
#include      "scr08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRT, NINT, NSN, NTY, NOINT, IR
C     REAL
      my_real
     .   SLSFAC, GAP
      INTEGER IRECT(4,*), IXS(NIXS,*), IXC(NIXC,*),
     .   NSV(*), IXTG(NIXTG,*), NSEG(*), LNSV(*),
     .   KNOD2ELS(*), KNOD2ELC(*), KNOD2ELTG(*), NOD2ELS(*), NOD2ELC(*), 
     .   NOD2ELTG(*),IXS10(*), IXS16(*), IXS20(*),
     .   IGEO(NPROPGI,*),IWORKSH(3,*)
C     REAL
      my_real
     .   X(3,*), STF(*), PM(NPROPM,*), GEO(NPROPG,*), STFN(*),THK(*),
     .   GAPN(*),STF8(*) ,FMAX, DEPTH, FILLSOL(*),Pm_STACk(20,*)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
      TYPE (SURF_) :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NDX, I, J, INRT, NELS, MT, JJ, JJJ, NELC,
     .   MG, NUM, NPT, LL, L, NELTG,IGTYP,IPGMAT,IGMAT,
     .   ISUBSTACK, IG, IL
C     REAL
      my_real
     .   DXM, AREA, VOL, DX, GAPTMP,SLOPE,STFMIN
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------

C--------------------------------------------------------------
C     CALCUL DES RIGIDITES DES SEGMENTS ET DES NOEUDS
C     V16 : DANS LE CAS OU ONE SEGMENT APPARTIENT A LA FOIS
C           A UNE BRIQUE ET A UNE COQUE ON CHOISIT LA RIGIDITE
C           DE LA COQUE SAUF SI LE MATERIAU COQUE EST NUL.
C---------------------------------------------------------------
      DXM=ZERO
      NDX=0
      IPGMAT = 700
C
      IF (NTY==8) THEN
          GAPN(1:NRT) = ZERO
          STF8(1:NRT) = ZERO
      ENDIF
      STFMIN = EP20
C
      DO I=1,NRT
      STF(I)=ZERO
      INRT=I
C----------------------
      CALL INELTS(X           ,IRECT,IXS  ,NINT,NELS         ,
     .            INRT        ,AREA ,NOINT,IR  ,IGRSURF%ELTYP,
     .            IGRSURF%ELEM)
      IF(NELS/=0)THEN
        MT=IXS(1,NELS)
        IF(MT>0)THEN
          DO JJ=1,8
            JJJ=IXS(JJ+1,NELS)
            XC(JJ)=X(1,JJJ)
            YC(JJ)=X(2,JJJ)
            ZC(JJ)=X(3,JJJ)
          END DO
          CALL VOLINT(VOL)
          STF(I)=SLSFAC*FILLSOL(NELS)*AREA*AREA*PM(32,MT)/VOL
          STFMIN = MIN(STFMIN,STF(I))
        ELSE
          IF(NINT>=0) THEN
             CALL ANCMSG(MSGID=95,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO_BLIND_2,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=IXS(NIXS,NELS),
     .                   C2='SOLID',
     .                   I3=I)
          ENDIF
          IF(NINT<0) THEN 
             CALL ANCMSG(MSGID=96,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO_BLIND_2,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=IXS(NIXS,NELS),
     .                   C2='SOLID',
     .                   I3=I)
          ENDIF
        ENDIF
        GO TO 500
      ELSE
        CALL INELTC(NELC ,NELTG ,INRT ,IGRSURF%ELTYP, IGRSURF%ELEM)

        IF(NELTG/=0) THEN
          MT=IXTG(1,NELTG)
          MG=IXTG(5,NELTG)
          IGTYP = IGEO(11,MG)
          IGMAT = IGEO(98,MG)
          DX=GEO(1,MG)
          IF(IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP ==52) DX = THK(NUMELC + NELTG)
          IF (NTY==8) GAPN(I) = DX/TWO
          DXM=DXM+DX
          NDX=NDX+1
          IF(MT>0)THEN
           IF( IGTYP == 11 .AND. IGMAT > 0) THEN
            STF(I)=SLSFAC*DX*GEO(IPGMAT + 2 ,MG)
            STFMIN = MIN(STFMIN,STF(I))
           ELSEIF(IGTYP == 52 .OR. 
     .           ((IGTYP == 17 .OR. IGTYP == 51) .AND. IGMAT > 0))THEN
              ISUBSTACK = IWORKSH(3,NUMELC+NELTG)
              STF(I)=SLSFAC*DX*PM_STACK(2,ISUBSTACK)
              STFMIN = MIN(STFMIN,STF(I))
           ELSE
            STF(I)=SLSFAC*DX*PM(20,MT)
            STFMIN = MIN(STFMIN,STF(I))
           ENDIF
          ELSE
            IF(NINT>=0) THEN
               CALL ANCMSG(MSGID=95,
     .                     MSGTYPE=MSGWARNING,
     .                     ANMODE=ANINFO_BLIND_2,
     .                     I1=ID,
     .                     C1=TITR,
     .                     I2=IXTG(NIXTG,NELTG),
     .                     C2='SHELL',
     .                     I3=I)
            END IF
            IF(NINT<0) THEN
               CALL ANCMSG(MSGID=96,
     .                     MSGTYPE=MSGWARNING,
     .                     ANMODE=ANINFO_BLIND_2,
     .                     I1=ID,
     .                     C1=TITR,
     .                     I2=IXTG(NIXTG,NELTG),
     .                     C2='SHELL',
     .                     I3=I)
            END IF
          END IF
          GO TO 500
        ELSEIF(NELC/=0) THEN
          MT=IXC(1,NELC)
          MG=IXC(6,NELC)
          IGTYP = IGEO(11,MG)
          IGMAT = IGEO(98,MG)
          DX=GEO(1,MG)
          IF(IGTYP == 17 .OR. IGTYP == 51) DX = THK(NELC)
          IF (NTY==8) GAPN(I) = DX/TWO
          DXM=DXM+DX
          NDX=NDX+1
          IF(MT>0)THEN
            IF(IGTYP  == 11 .AND. IGMAT > 0) THEN
             STF(I)=SLSFAC*DX*GEO(IPGMAT + 2 ,MG)
             STFMIN = MIN(STFMIN,STF(I))
            ELSEIF(IGTYP == 52 .OR. 
     .           ((IGTYP == 17 .OR. IGTYP == 51) .AND. IGMAT > 0))THEN
              ISUBSTACK = IWORKSH(3,NELC)
              STF(I)=SLSFAC*DX*PM_STACK(2 ,ISUBSTACK)
              STFMIN = MIN(STFMIN,STF(I))
            ELSE
             STF(I)=SLSFAC*DX*PM(20,MT)
             STFMIN = MIN(STFMIN,STF(I))
            ENDIF 
          ELSE
            IF(NINT>=0) THEN
               CALL ANCMSG(MSGID=95,
     .                     MSGTYPE=MSGWARNING,
     .                     ANMODE=ANINFO_BLIND_2,
     .                     I1=ID,
     .                     C1=TITR,
     .                     I2=IXC(NIXC,NELC),
     .                     C2='SHELL',
     .                     I3=I)
            END IF
            IF(NINT<0) THEN
               CALL ANCMSG(MSGID=96,
     .                     MSGTYPE=MSGWARNING,
     .                     ANMODE=ANINFO_BLIND_2,
     .                     I1=ID,
     .                     C1=TITR,
     .                     I2=IXC(NIXC,NELC),
     .                     C2='SHELL',
     .                     I3=I)
            END IF
          END IF
          GO TO 500
        END IF
      END IF
C----------------------
C     ELEMENTS SOLIDES
C----------------------
      CALL INSOL3(X,IRECT,IXS,NINT,NELS,INRT,
     .            AREA,NOINT,KNOD2ELS ,NOD2ELS ,IR ,IXS10,
     .            IXS16,IXS20)
      IF(NELS/=0) THEN
       MT=IXS(1,NELS)
       IF(MT>0)THEN
        DO JJ=1,8
          JJJ=IXS(JJ+1,NELS)
          XC(JJ)=X(1,JJJ)
          YC(JJ)=X(2,JJJ)
          ZC(JJ)=X(3,JJJ)
        ENDDO
        CALL VOLINT(VOL)
        STF(I)=SLSFAC*FILLSOL(NELS)*AREA*AREA*PM(32,MT)/VOL
        STFMIN = MIN(STFMIN,STF(I))
       ELSE
          IF(NINT>=0) THEN
             CALL ANCMSG(MSGID=95,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO_BLIND_2,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=IXS(NIXS,NELS),
     .                   C2='SOLID',
     .                   I3=I)
          ENDIF
          IF(NINT<0) THEN
             CALL ANCMSG(MSGID=96,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO_BLIND_2,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=IXS(NIXS,NELS),
     .                   C2='SOLID',
     .                   I3=I)
          ENDIF
       ENDIF
      ENDIF
C---------------------
C     ELEMENTS COQUES
C---------------------
      CALL INCOQ3(IRECT,IXC ,IXTG ,NINT ,NELC     ,
     .            NELTG,INRT,GEO  ,PM   ,KNOD2ELC ,
     .            KNOD2ELTG ,NOD2ELC ,NOD2ELTG,THK,NTY,IGEO,
     .            PM_STACK , IWORKSH)
      IF(NELTG/=0) THEN
        MT=IXTG(1,NELTG)
        MG=IXTG(5,NELTG)
        IGTYP = IGEO(11,MG)
        IGMAT = IGEO(98,MG)
        DX=GEO(1,MG)
        IF(IGTYP == 17 .OR. IGTYP == 51) DX = THK(NELC)
        IF (NTY==8) GAPN(I) = DX/TWO
        DXM=DXM+DX
        NDX=NDX+1
        IF(MT>0)THEN
         IF(IGTYP  == 11 .AND. IGMAT > 0) THEN
           STF(I)=SLSFAC*DX*GEO(IPGMAT + 2 ,MG)
           STFMIN = MIN(STFMIN,STF(I))
         ELSEIF(IGTYP == 52 .OR. 
     .           ((IGTYP == 17 .OR. IGTYP == 51) .AND. IGMAT > 0))THEN
            ISUBSTACK = IWORKSH(3,NELC)
            STF(I)=SLSFAC*DX*PM_STACK(2 ,ISUBSTACK)
            STFMIN = MIN(STFMIN,STF(I)) 
         ELSE
           STF(I)=SLSFAC*DX*PM(20,MT)
           STFMIN = MIN(STFMIN,STF(I))
         ENDIF
        ELSE
           IF(NINT>=0) THEN
              CALL ANCMSG(MSGID=95,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXTG(NIXTG,NELTG),
     .                    C2='SHELL',
     .                    I3=I)
           ENDIF
           IF(NINT<0) THEN
              CALL ANCMSG(MSGID=95,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXTG(NIXTG,NELTG),
     .                    C2='SHELL',
     .                    I3=I)
           ENDIF
        ENDIF
      ELSEIF(NELC/=0) THEN
        MT=IXC(1,NELC)
        MG=IXC(6,NELC)
        IGTYP = IGEO(11,MG)
        IGMAT = IGEO(98,MG)
        DX=GEO(1,MG)
        IF(IGTYP == 17 .OR. IGTYP == 51) DX = THK(NELC)
        IF (NTY==8) GAPN(I) = DX/TWO
        DXM=DXM+DX
        NDX=NDX+1
        IF(MT>0)THEN
         IF(IGTYP  == 11 .AND. IGMAT > 0) THEN
           STF(I)=SLSFAC*DX*GEO(IPGMAT + 2 ,MG)
           STFMIN = MIN(STFMIN,STF(I))
         ELSEIF(IGTYP == 52 .OR. 
     .           ((IGTYP == 17 .OR. IGTYP == 51) .AND. IGMAT > 0))THEN
            ISUBSTACK = IWORKSH(3,NELC)
            STF(I)=SLSFAC*DX*PM_STACK(2 ,ISUBSTACK)
            STFMIN = MIN(STFMIN,STF(I))  
         ELSE
           STF(I)=SLSFAC*DX*PM(20,MT)
           STFMIN = MIN(STFMIN,STF(I))
         ENDIF  
        ELSE
           IF(NINT>=0) THEN
              CALL ANCMSG(MSGID=95,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXC(NIXC,NELC),
     .                    C2='SHELL',
     .                    I3=I)
           ENDIF
           IF(NINT<0) THEN
              CALL ANCMSG(MSGID=95,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXC(NIXC,NELC),
     .                    C2='SHELL',
     .                    I3=I)
           ENDIF
        ENDIF
      ENDIF
C
      IF(NELS+NELC+NELTG==0)THEN
         IF(NINT>0) THEN
            CALL ANCMSG(MSGID=92,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=I)
         ENDIF
         IF(NINT<0) THEN
            CALL ANCMSG(MSGID=93,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=I)
         ENDIF
      ENDIF
  500 CONTINUE
      ENDDO   !I=1,NRT
C---------------------------
C     Stiffness INTERFACES TYPE 8
C---------------------------
      IF(NTY==8)THEN
        IF(FMAX/=ZERO) THEN
          IF(DEPTH<=EM20) THEN
            DO I=1,NRT   
             STF8(I) = STF(I)
            ENDDO
           CALL ANCMSG(MSGID=1043,
     .                 MSGTYPE=MSGWARNING,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 I1=ID,
     .                 C1=TITR,
     .                 R1=DEPTH)
          ELSE                    
           SLOPE = FMAX/DEPTH
           IF(SLOPE>STFMIN.AND.STFMIN/=ZERO)THEN
              DO I=1,NRT
                STF8(I) = STF(I)
              ENDDO
              CALL ANCMSG(MSGID=1040,
     .                 MSGTYPE=MSGWARNING,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 I1=ID,
     .                 C1=TITR,
     .                 R1=DEPTH,
     .                 R2=FMAX,
     .                 R3=SLOPE)
             ELSE
              DO I=1,NRT
                STF8(I) = SLOPE   
              ENDDO         
             ENDIF
          ENDIF
        ENDIF
      ENDIF
C---------------------------------------------
C     CALCUL DES RIGIDITES NODALES
C---------------------------------------------
      DO J=1,NSN
        NUM=NSEG(J+1)-NSEG(J)
        NPT=NSEG(J)-1
        DO JJ=1,NUM
          LL=LNSV(NPT+JJ)
          STFN(J)=STFN(J)+FOURTH*STF(LL)
        ENDDO
      ENDDO
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
