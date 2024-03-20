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
Chd|  TINIT3                        source/elements/truss/tinit3.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        DT1LAWT                       source/elements/truss/dt1lawt.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        TCOORI                        source/elements/truss/tcoori.F
Chd|        TIBUF3                        source/elements/truss/tibuf3.F
Chd|        TMASS                         source/elements/truss/tmass.F 
Chd|        TSIGINI                       source/elements/truss/tsigini.F
Chd|        BPRELOAD_MOD                  share/modules1/bpreload_mod.F 
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE TINIT3(ELBUF_STR,
     1                  IC     ,PM         ,X        ,GEO    ,XMAS   ,
     2                  DTELEM ,NFT        ,NEL      ,STIFN  ,PARTSAV,
     3                  V      ,IPART      ,MST      ,STIFINT,STT    ,
     4                  IGEO   ,NSIGTRUSS  ,SIGTRUSS ,PTTRUSS,
     5                  PRELOAD_A,IPRELD )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE MESSAGE_MOD
      USE BPRELOAD_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IC(NIXT,*),IPART(*),IGEO(NPROPGI,*),PTTRUSS(*)
      INTEGER NFT,NEL,NSIGTRUSS
      INTEGER , INTENT (IN   )  :: IPRELD
      my_real
     .   PM(*),X(*), GEO(NPROPG,*),XMAS(*),DTELEM(*),
     .   STIFN(*),PARTSAV(20,*),V(*),MST(*),STIFINT(*),STT(*),
     .   SIGTRUSS(NSIGTRUSS,*)
C
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
      TYPE(PREL1D_) ,      TARGET :: PRELOAD_A
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IGTYP,NDEPAR,IPID1
      INTEGER MAT(MVSIZ), MXG(MVSIZ), NC1(MVSIZ), NC2(MVSIZ)
      my_real
     .     X1(MVSIZ), X2(MVSIZ),
     .     Y1(MVSIZ), Y2(MVSIZ),
     .     Z1(MVSIZ), Z2(MVSIZ)
      my_real
     .   DELTAX(MVSIZ),XX,YY,ZZ, DTX(MVSIZ)
      INTEGER IDMIN,IDMAX
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
      DATA IDMIN /-1/, IDMAX /-1/
      my_real
     .     LGTHMIN, LGTHMAX,XNOR,UNDAMP,CC1
      DATA LGTHMIN /-1/, LGTHMAX /-1/
C
      TYPE(G_BUFEL_),POINTER :: GBUF
C=======================================================================
      GBUF => ELBUF_STR%GBUF
C
      IPID1=IC(NIXT-1,NFT+1)
      ID=IGEO(1,IPID1)
      CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1,IPID1),LTITR)
C
      CALL TCOORI(X,IC(1,NFT+1),MAT, MXG, NC1, NC2, 
     .     X1, X2, Y1, Y2, Z1, Z2)
C-----------------------------
C     Check here (change TCOORI for called by /MAT/VOID)
C-----------------------------
      CALL TIBUF3(GBUF%OFF,GBUF%AREA,GEO, MXG)
      DO I=1,NEL
        XNOR=(X1(I)-X2(I))**2+(Y1(I)-Y2(I))**2+(Z1(I)-Z2(I))**2
        IF (XNOR <= 1.E-20) THEN
           CALL ANCMSG(MSGID=269,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 I1=ID,
     .                 C1=TITR,
     .                 I2=IC(5,I+NFT))
        ENDIF
        GBUF%LENGTH(I) = SQRT(XNOR)
      END DO
!------------------------------------------
! Truss initialization
!------------------------------------------
      IF (ISIGI /= 0)
     .  CALL TSIGINI(NSIGTRUSS  ,SIGTRUSS  ,PTTRUSS   , GBUF%EINT ,GBUF%FOR,
     .               GBUF%G_PLA ,GBUF%PLA  ,GBUF%AREA )
      CALL TMASS(X         ,IC      ,GEO      ,PM          ,XMAS      ,
     .     STIFN     ,PARTSAV ,V        ,IPART(NFT+1),MST(NFT+1),
     .     STIFINT,STT(NFT+1) ,GBUF%AREA, MAT, NC1, NC2,
     .     X1, X2, Y1, Y2, Z1, Z2)
!------------------------------------------
C------------------------------------------
C Compute truss time step 22/4/90 
C------------------------------------------
      DO I=1,NEL
        IGTYP=GEO(12,IC(4,I+NFT))
        IF (IGTYP /= 2 .AND. INVERS > 14) THEN
          CALL ANCMSG(MSGID=270,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID,
     .                C1=TITR,
     .                I2=IC(NIXT,I),I3=IGTYP)
        ENDIF
        XX = (X1(I) - X2(I))*(X1(I) - X2(I))
        YY = (Y1(I) - Y2(I))*(Y1(I) - Y2(I))
        ZZ = (Z1(I) - Z2(I))*(Z1(I) - Z2(I))
        DELTAX(I)=SQRT(XX+YY+ZZ)
        IF (LGTHMIN == -1 .OR. DELTAX(I) < LGTHMIN) THEN
          LGTHMIN = DELTAX(I)
          IDMIN = IC(5,I+NFT)
        ENDIF
        IF (LGTHMAX == -1 .OR. DELTAX(I) > LGTHMAX) THEN
          LGTHMAX = DELTAX(I)
          IDMAX = IC(5,I+NFT)
        ENDIF         
      ENDDO ! I=1,NEL
C
      CALL DT1LAWT(PM, DELTAX, MAT, MXG, DTX)
      NDEPAR=NUMELS+NUMELC+NFT
      DO I=1,NEL
        DTELEM(NDEPAR + I) = DTX(I)
      ENDDO
!---  /PRELOAD/AXIAL     
      IF (IPRELD>0) THEN
          CC1 =TWO*SQRT(TWO)
          DO I=1,NEL
            UNDAMP = CC1*MST(NFT+I)*GBUF%LENGTH(I)/DTX(I)
            GBUF%BPRELD(I)     = PRELOAD_A%preload
            GBUF%BPRELD(I+NEL) = UNDAMP*PRELOAD_A%damp
          ENDDO
      END IF
C---
      RETURN
      END
