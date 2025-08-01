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
!||    pinit3           ../starter/source/elements/beam/pinit3.F
!||--- called by ------------------------------------------------------
!||    initia           ../starter/source/elements/initia/initia.F
!||--- calls      -----------------------------------------------------
!||    ancmsg           ../starter/source/output/message/message.F
!||    bsigini          ../starter/source/elements/beam/bsigini.F
!||    buserini         ../starter/source/elements/beam/buserini.F
!||    dt1lawp          ../starter/source/elements/beam/dt1lawp.F
!||    pcoori           ../starter/source/elements/beam/pcoori.F
!||    peveci           ../starter/source/elements/beam/peveci.F
!||    pibuf3           ../starter/source/elements/beam/pibuf3.F
!||    pmass            ../starter/source/elements/beam/pmass.F
!||--- uses       -----------------------------------------------------
!||    bpreload_mod     ../starter/share/modules1/bpreload_mod.F
!||    message_mod      ../starter/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE PINIT3(ELBUF_STR,
     1                  STP      ,IC      ,PM      ,X       ,GEO   ,
     2                  DTELEM   ,NFT     ,NEL     ,
     3                  STIFN    ,STIFR   ,PARTSAV ,V       ,IPART ,
     4                  MSP      ,INP     ,IGEO    ,STRP  ,
     5                  NSIGBEAM ,SIGBEAM ,PTBEAM  ,IUSER   ,
     6                  MCPP     ,TEMP    ,PRELOAD_A,IPRELD,NPRELOAD_A,
     7                  GLOB_THERM,IBEAM_VECTOR,RBEAM_VECTOR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE MESSAGE_MOD
      USE BPRELOAD_MOD
      use glob_therm_mod
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: NEL,NSIGBEAM,IUSER,NFT
      INTEGER :: IC(NIXP,*),IPART(*),IGEO(NPROPGI,*),PTBEAM(*)
      INTEGER , INTENT (IN   )  :: IPRELD,NPRELOAD_A
      INTEGER , INTENT (IN   )  :: IBEAM_VECTOR(NUMELP)
      my_real
     .   PM(*),X(*),GEO(NPROPG,*),
     .   DTELEM(*),STIFN(*),STIFR(*),PARTSAV(20,*),V(*),MSP(*),INP(*),
     .   STP(*),STRP(*),SIGBEAM(NSIGBEAM,*),MCPP(*),
     .   TEMP(*)
      my_real , INTENT (IN   )  :: RBEAM_VECTOR(3,NUMELP)     
C
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
      TYPE(PREL1D_) ,DIMENSION(NPRELOAD_A),    TARGET :: PRELOAD_A
      type (glob_therm_) ,intent(in)      :: glob_therm
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IPT,IGTYP,NDEPAR,IPID,IMAT,NIP
      INTEGER NC1(MVSIZ),NC2(MVSIZ),NC3(MVSIZ),MXT(MVSIZ),MXG(MVSIZ), 
     .   IREL(6,MVSIZ),IVECT(MVSIZ)
      my_real :: TEMP0
      my_real
     .   X1(MVSIZ),X2(MVSIZ),X3(MVSIZ),
     .   Y1(MVSIZ),Y2(MVSIZ),Y3(MVSIZ), 
     .   Z1(MVSIZ),Z2(MVSIZ),Z3(MVSIZ),
     .   E1X(MVSIZ),E1Y(MVSIZ),E1Z(MVSIZ), 
     .   E2X(MVSIZ),E2Y(MVSIZ),E2Z(MVSIZ),AREA(MVSIZ) , 
     .   E3X(MVSIZ),E3Y(MVSIZ),E3Z(MVSIZ),DELTAX(MVSIZ),DTX(MVSIZ),
     .   VECT(3,MVSIZ) 
      INTEGER IDMIN,IDMAX
      DATA IDMIN /-1/,IDMAX /-1/
      my_real :: LGTHMIN,LGTHMAX,CC1,UNDAMP
      DATA LGTHMIN /-1/,LGTHMAX /-1/
C
      TYPE(G_BUFEL_),POINTER :: GBUF
C-----------------------------------------------
      GBUF => ELBUF_STR%GBUF
      IPID = IC(5,1+NFT)
      IGTYP = IGEO(11,IPID)

C
      CALL PCOORI(X,IC(1,NFT+1), 
     .            MXT,MXG ,NC1,NC2,NC3,DELTAX,
     .            X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3,
     .            IBEAM_VECTOR(NFT+1),RBEAM_VECTOR(1,NFT+1),IVECT,VECT)
     
      IMAT = MXT(1)
c
      CALL PEVECI(GBUF%SKEW,X1,X3,Y1,Y3,Z1,Z3,X2,Y2,Z2,NC2,NC3,
     .            IVECT,VECT)
C      
      IF (GLOB_THERM%NINTEMP > 0) THEN
        IF (IGTYP == 18) THEN
          IF (ELBUF_STR%BUFLY(1)%L_TEMP > 0) THEN
            NIP  = IGEO(3,MXG(1))
            DO I = 1,NEL                                       
              TEMP0 = HALF * (TEMP(NC1(I)) + TEMP(NC2(I))) 
              DO IPT=1,NIP                                            
                ELBUF_STR%BUFLY(1)%LBUF(1,1,IPT)%TEMP(I) = TEMP0
              ENDDO                                               
            ENDDO
          END IF
        ELSE IF (IGTYP == 3 .and. ELBUF_STR%GBUF%G_TEMP > 0) THEN                                             
          DO I = 1,NEL                                       
            ELBUF_STR%GBUF%TEMP(I) = HALF * (TEMP(NC1(I)) + TEMP(NC2(I)))
          ENDDO
        END IF
      END IF                                               
!
      CALL PMASS(GEO,PM,
     .           STIFN,STIFR,PARTSAV,V,IPART(NFT+1),
     .           MSP(NFT+1),INP(NFT+1),IGEO , STP(NFT+1),
     .           X1,X2, Y1,Y2, Z1,Z2,
     .           NC1,NC2,IMAT,MXG,AREA,DELTAX,STRP(NFT+1),
     .           MCPP(NFT+1) , TEMP ,GLOB_THERM%NINTEMP)
      CALL PIBUF3(GEO,GBUF%OFF,GBUF%LENGTH,DELTAX,MXG,IREL)
C------------------------------------------
C Beam initialization
C------------------------------------------
       IPID = IC(5,1+NFT)
       IGTYP = IGEO(11,IPID)
C
        IF (ISIGI /= 0)
     .    CALL BSIGINI(ELBUF_STR,
     .                 IGTYP      ,NEL    ,NSIGBEAM  ,SIGBEAM   ,PTBEAM,
     .                 IC(1,NFT+1),IGEO   )
        IF (IUSER /= 0)
     .    CALL BUSERINI(ELBUF_STR,
     .                  IC(1,NFT+1),SIGBEAM  ,NSIGBEAM ,PTBEAM ,IGEO  ,
     .                  NEL        )
C------------------------------------------
C Compute beam element time step
C------------------------------------------
      DO I=1,NEL
        IF (IGTYP /= 3 .AND. IGTYP /= 18) THEN
          CALL ANCMSG(MSGID=225,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=IGTYP)
        ENDIF
        IF (LGTHMIN == -1 .OR. DELTAX(I) < LGTHMIN) THEN
          LGTHMIN = DELTAX(I)
          IDMIN = IC(5,I+NFT)
        ENDIF
        IF (LGTHMAX == -1 .OR. DELTAX(I) > LGTHMAX) THEN
           LGTHMAX = DELTAX(I)
           IDMAX = IC(5,I+NFT)
        ENDIF
      ENDDO
C-----------
      CALL DT1LAWP(PM,GEO,MXT,MXG,DELTAX,DTX,IGTYP)
      NDEPAR=NUMELS+NUMELC+NUMELT+NFT
C-----------
      DO I=1,NEL
        DTELEM(NDEPAR+I)=DTX(I)
      ENDDO
!---  /PRELOAD/AXIAL     
      IF (IPRELD>0) THEN
          CC1 =TWO*SQRT(TWO)
          DO I=1,NEL
            UNDAMP = CC1*MSP(NFT+I)*GBUF%LENGTH(I)/DTX(I)
            GBUF%BPRELD(I)     = PRELOAD_A(IPRELD)%preload
            GBUF%BPRELD(I+NEL) = UNDAMP*PRELOAD_A(IPRELD)%damp
          ENDDO
      END IF
C-----------
      RETURN
      END
