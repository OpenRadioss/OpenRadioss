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
Chd|  ANIM_BUILD_INDEX_ALL          source/output/anim/reader/anim_build_index_all.F
Chd|-- called by -----------
Chd|        LECTUR                        source/input/lectur.F         
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE ANIM_BUILD_INDEX_ALL(ISPMD   ,MCHECK ,LSENSOR ,IGEO    ,GEO    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE STACK_MOD    
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr06_c.inc"
#include      "scr14_c.inc"
#include      "scr25_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISPMD, MCHECK, LSENSOR(NSENSI,*), IGEO(NPROPGI,*)
      my_real
     .        GEO(NPROPG,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,  J, K, IOK, IANIS0, NIPMAX, IHBE,
     .   IUS,IPT,IGTYP,NPT,NPTS,NPTR,NPTT,NPTT_PLY,CPtPLY,NMAX,IGTYP15,
     .   IDX,NIPMAX_BEAM, NIPMAX_SOL,ICSTR,N14R,N14S,N14T
C-------------------------------------------
      IF(ISPMD == 0.AND.MCHECK == 0) THEN
C-----------------------
C      ANIM/LSENSOR Initializations
C-----------------------       
        IANIS0=IANISENS
        IF(IANIS0 /= 0)THEN
          DO I=1,NSENSOR
            IF(IANIS0 == LSENSOR(1,I))IANISENS=I
          ENDDO
        ENDIF
        DO K=1,NLSENS
          IOK = 0
          IANIS0=LANISENS(K)
          IF(IANIS0 /= 0)THEN
            DO I=1,NSENSOR
              IF(IANIS0 == LSENSOR(1,I))THEN
                LANISENS(K)=I
                IOK = 1
                EXIT
              ENDIF
            ENDDO
          ENDIF
          IF(IOK == 0) THEN
            CALL ANCMSG(MSGID=228,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=IANIS0)
            CALL ARRET(2)
          ENDIF
        ENDDO
C-----------------------       
      ELSEIF(ISPMD == 0 .AND. MCHECK /= 0) THEN
C-----------------------
C     ANIM/LSENSOR Iniializations
C-----------------------   
         IANISENS = IANISENS_OLD
         DO K=1,IANISENS_OLD
           LANISENS(K) = LANISENS_OLD(K)
         ENDDO
      END IF                    !(ISPMD.EQ.0.AND.MCHECK.EQ.0)
C-------------------------------------------
      IF(MCHECK == 0 ) THEN
       IF( ISTRESALL == 1 .OR. ISTRAIALL  == 1      .OR.
     .    IEPSDOALL == 1 .OR. IEPSPALL   == 1      .OR.
     .                        IPHIALL    == 1      .OR.
     .                        IDAMAALL   == 1      .OR.
     .                        IEPSPFULL  >  0      .OR.
     .                        ISTRESFULL >  0      .OR.
     .                        IPLYALL    >  0      .OR.
     .                        ISTRESALL_PLY>0      .OR.
     .                        ISTRAINALL_PLY>0     .OR.
     .                        IEPSDOTALL_PLY>0     .OR.
     .                        IEPSPALL_PLY>0       .OR.
     .                        IDAMAALL_PLY>0       .OR.
     .                        IPHIALL_PLY > 0      .OR.
     .                        IBRICK_STRESSALL > 0 .OR.
     .                        IBRICK_STRAINALL > 0 .OR.
     .                        IBRICK_EPSPALL > 0   .OR.
     .                        IDAMAFULL > 0        .OR.
     .                        ISTRAINFULL > 0      .OR.
     .                        IEPSDOFULL > 0       .OR.
     .                        IWPLAALL > 0         .OR.
     .                        IWPLAFULL > 0        .OR.
     .                        IBEAM_EPSPALL > 0       ) THEN
         NIPMAX = 0
         DO I=1,NUMGEO
           NIPMAX = MAX(NIPMAX,NINT(GEO(6,I)))
         ENDDO
         IF(NIPMAX ==0) NIPMAX=1
!
         NIPMAX_BEAM = 0
         DO I=1,NUMGEO
           IGTYP = IGEO(11,I)
           IF (IGTYP == 18) THEN ! integrated beam
              NIPMAX_BEAM = MAX(NIPMAX_BEAM,IGEO(3,I))
           ENDIF
         ENDDO
         IF(NIPMAX_BEAM == 0) NIPMAX_BEAM=1
!
         IF (ISTRESALL == 1)THEN
           IF (ANIM_CT(3) == 0) THEN
             ANIM_CT(3)    = 1
             NCT_ANI = NCT_ANI + 1
           ENDIF
           IF (ANIM_CT(4) == 0) THEN
             ANIM_CT(4)    = 1
             NCT_ANI = NCT_ANI + 1
           ENDIF
           IF(NIPMAX.LE.100)THEN
             DO I=1,NIPMAX
               ANIM_CT(100+I) = 1
               NCT_ANI = NCT_ANI + 1
             ENDDO
           ENDIF
         ENDIF
         IF (ISTRAIALL == 1)THEN
           IF (ANIM_CT(7) == 0) THEN
             ANIM_CT(7)    = 1
             NCT_ANI = NCT_ANI + 1
           ENDIF
           IF (ANIM_CT(8) == 0) THEN
             ANIM_CT(8)    = 1
             NCT_ANI = NCT_ANI + 1
           ENDIF
           IF(NIPMAX.LE.100)THEN
             DO I=1,NIPMAX
               ANIM_CT(200+I) = 1
               NCT_ANI = NCT_ANI + 1
             ENDDO
           ENDIF
         ENDIF
         IF (IEPSDOALL == 1)THEN
           IF (ANIM_CT(93) == 0) THEN
             ANIM_CT(93)    = 1
             NCT_ANI = NCT_ANI + 1
           ENDIF
           IF (ANIM_CT(94) == 0) THEN
             ANIM_CT(94)    = 1
             NCT_ANI = NCT_ANI + 1
           ENDIF
           IF(NIPMAX.LE.100)THEN
             DO I=1,NIPMAX
               ANIM_CT(300+I) = 1
               NCT_ANI = NCT_ANI + 1
             ENDDO
           ENDIF
         ENDIF
         IF (IEPSPALL == 1)THEN
           IF (ANIM_CE(2040) == 0) THEN
             ANIM_CE(2040)    = 1
             NCE_ANI = NCE_ANI + 1
           ENDIF
           IF (ANIM_CE(2041) == 0) THEN
             ANIM_CE(2041)    = 1
             NCE_ANI = NCE_ANI + 1
           ENDIF
           IF(NIPMAX.LE.100)THEN
             DO I=1,NIPMAX
               IF (ANIM_CE(2041+I) == 0) THEN
                 ANIM_CE(2041+I) = 1
                 NCE_ANI = NCE_ANI + 1
               ENDIF
             ENDDO
           ENDIF
         ENDIF
!---
         IF (IEPSPFULL == 1) THEN
!     ...EPSP/N1/ALL
           IF (NIPMAX <= 100) THEN
             DO I=1,NIPMAX
               DO J=1,10
                 IUS = 10*I+J
                 IF (ANIM_CE(10877 + IUS) == 0 .AND. ANIM_EPSP(I) > 0) THEN
                   ANIM_CE(10877 + IUS) = 1
                   NCE_ANI = NCE_ANI + 1
                 ENDIF
               ENDDO
             ENDDO
           ENDIF
         ELSEIF (IEPSPFULL == 2) THEN
!     ...EPSP/ALL/ALL
           IF (NIPMAX <= 100) THEN
             DO I=1,NIPMAX
               DO J=1,10
                 IUS = 10*I+J
                 IF (ANIM_CE(10877 + IUS) == 0) THEN
                   ANIM_CE(10877 + IUS) = 1
                   NCE_ANI = NCE_ANI + 1
                 ENDIF
               ENDDO
             ENDDO
           ENDIF
         ENDIF ! IF (IEPSPFULL == 1)
!---
         IF (ISTRESFULL == 1) THEN
!     ...STRESS/N1/ALL
           IF (NIPMAX <= 100) THEN
             DO I=1,NIPMAX
               DO J=1,10
                 IUS = 10*I+J
                 IF (ANIM_CT(600 + IUS) == 0 .AND. ANIM_STRESS(I) > 0) THEN
                   ANIM_CT(600 + IUS) = 1
                   NCT_ANI = NCT_ANI + 1
                 ENDIF
               ENDDO
             ENDDO
           ENDIF
         ELSEIF (ISTRESFULL == 2) THEN
!     ...STRESS/ALL/ALL
           IF (NIPMAX <= 100) THEN
             DO I=1,NIPMAX
               DO J=1,10
                 IUS = 10*I+J
                 IF (ANIM_CT(600 + IUS) == 0) THEN
                   ANIM_CT(600 + IUS) = 1
                   NCT_ANI = NCT_ANI + 1
                 ENDIF
               ENDDO
             ENDDO
           ENDIF
         ENDIF ! IF (ISTRESFULL == 1)
C-------------
         IF (IPHIALL == 1)THEN
           IF(NIPMAX.LE.100)THEN
             DO I=1,NIPMAX
               ANIM_CE(10139+I) = 1
               NCE_ANI = NCE_ANI + 1
             ENDDO
           ENDIF
         ENDIF
       ENDIF !        IF( ISTRESALL == 1 .OR. ISTRAIALL  == 1      .OR.
C
       IF( IORTHDALL == 1 )THEN
         NIPMAX_SOL = 1
         DO I=1,NUMGEO
           NIPMAX_SOL = MAX(NIPMAX_SOL,IGEO(30,I))
         ENDDO
         DO I=1,NIPMAX_SOL
           ANIM_SE(286+3*(I-1)+1) = 1
           ANIM_SE(286+3*(I-1)+2) = 1
           ANIM_SE(286+3*(I-1)+3) = 1
           NSE_ANI = NSE_ANI + 3
         ENDDO
       ENDIF !
C
       IF (IDAMAALL == 1)THEN
         IF(NIPMAX.LE.100)THEN
           DO I=1,NIPMAX
             ANIM_CE(10259+I) = 1
             NCE_ANI = NCE_ANI + 1
           ENDDO
         ENDIF
       ENDIF
C
       IF (INXTFALL == 1)THEN
         IF(NIPMAX.LE.100)THEN
           DO I=1,NIPMAX
             ANIM_CE(10362+I) = 1
             NCE_ANI = NCE_ANI + 1
           ENDDO
         ENDIF
       ENDIF
C
       IF (SIGH1ALL == 1)THEN
         IF(NIPMAX.LE.100)THEN
           DO I=1,NIPMAX
             ANIM_CE(10465+I) = 1
             NCE_ANI = NCE_ANI + 1
           ENDDO
         ENDIF
       ENDIF
C
       IF (SIGH2ALL == 1)THEN
         IF(NIPMAX.LE.100)THEN
           DO I=1,NIPMAX
             ANIM_CE(10568+I) = 1
             NCE_ANI = NCE_ANI + 1
           ENDDO
         ENDIF
       ENDIF
C
       IF( IPLYALL == 1 )THEN
         CPTPLY = 0
         DO I=1,NUMGEO
           IF(IGEO(11,I) == 19) THEN
             CPTPLY = CPTPLY + 1
             PLY_ANIM( 3 * (CPTPLY - 1) + 1) = IGEO(1,I)
             PLY_ANIM( 3 * (CPTPLY - 1) + 2) = 1
             IF(CPTPLY < MX_PLY_ANIM) THEN
               ANIM_CE(11925 + CPTPLY) = 1
               NCE_ANI = NCE_ANI + 1 
             ELSE
               CALL ANCMSG(MSGID=268,MSGTYPE=MSGWARNING,I1=MX_PLY_ANIM,ANMODE=ANINFO)
             ENDIF
           ENDIF
         ENDDO
c
         DO I=1,NUMPLY
           CPTPLY = CPTPLY + 1
           PLY_ANIM( 3 * (CPTPLY - 1) + 1) = PLY_INFO(1,I)
           PLY_ANIM( 3 * (CPTPLY - 1) + 2) = 1
           IF(CPTPLY < MX_PLY_ANIM) THEN
             ANIM_CE(11925 + CPTPLY) = 1
             NCE_ANI = NCE_ANI + 1 
           ELSE
             CALL ANCMSG(MSGID=268,MSGTYPE=MSGWARNING,I1=MX_PLY_ANIM,ANMODE=ANINFO)
           ENDIF
         ENDDO
       ENDIF !
C
       IF( ISTRESALL_PLY == 1 )THEN
         CPTPLY = 0
         DO I=1,NUMGEO
           IF(IGEO(11,I) == 19) THEN
             DO IPT=1,IGEO(4,I)
               CPTPLY = CPTPLY + 1
               PLY_ANIM_STRESS( 3 * (CPTPLY - 1) + 1) = IGEO(1,I)
               PLY_ANIM_STRESS( 3 * (CPTPLY - 1) + 2) = 2
               PLY_ANIM_STRESS( 3 * (CPTPLY - 1) + 3) = IPT
               IF( CPTPLY < MX_PLY_ANIM) THEN
                 ANIM_CT(1610 + CPTPLY) = 1
                 NCT_ANI = NCT_ANI + 1 
               ELSE
                 CALL ANCMSG(MSGID=268,MSGTYPE=MSGWARNING,I1=MX_PLY_ANIM,ANMODE=ANINFO)
               ENDIF
             ENDDO
           ENDIF
         ENDDO
c
         DO I=1,NUMPLY
           DO IPT=1,PLY_INFO(2,I)
             CPTPLY = CPTPLY + 1
             PLY_ANIM_STRESS( 3 * (CPTPLY - 1) + 1) = PLY_INFO(1,I)
             PLY_ANIM_STRESS( 3 * (CPTPLY - 1) + 2) = 2
             PLY_ANIM_STRESS( 3 * (CPTPLY - 1) + 3) = IPT
             IF(CPTPLY < MX_PLY_ANIM) THEN
               ANIM_CT(1610 + CPTPLY) = 1
               NCT_ANI = NCT_ANI + 1 
             ELSE
               CALL ANCMSG(MSGID=268,MSGTYPE=MSGWARNING,I1=MX_PLY_ANIM,ANMODE=ANINFO)
             ENDIF
           ENDDO
         ENDDO
       ENDIF !
C
       IF( ISTRAINALL_PLY == 1 )THEN
         CPTPLY = 0
         DO I=1,NUMGEO
           IF(IGEO(11,I) == 19) THEN
             DO IPT=1,IGEO(4,I)
               CPTPLY = CPTPLY + 1
               PLY_ANIM_STRAIN( 3 * (CPTPLY - 1) + 1) = IGEO(1,I)
               PLY_ANIM_STRAIN( 3 * (CPTPLY - 1) + 2) = 3
               PLY_ANIM_STRAIN( 3 * (CPTPLY - 1) + 3) = IPT
               IF(CPTPLY < MX_PLY_ANIM) THEN
                 ANIM_CT( (1610+MX_PLY_ANIM) + CPTPLY) = 1
                 NCT_ANI = NCT_ANI + 1 
               ELSE
                 CALL ANCMSG(MSGID=268,MSGTYPE=MSGWARNING,I1=MX_PLY_ANIM,ANMODE=ANINFO)
               ENDIF
             ENDDO
           ENDIF
         ENDDO
c
         DO I=1,NUMPLY
           DO IPT=1,PLY_INFO(2,I)
             CPTPLY = CPTPLY + 1
             PLY_ANIM_STRAIN( 3 * (CPTPLY - 1) + 1) = PLY_INFO(1,I)
             PLY_ANIM_STRAIN( 3 * (CPTPLY - 1) + 2) = 3
             PLY_ANIM_STRAIN( 3 * (CPTPLY - 1) + 3) = IPT
             IF(CPTPLY < MX_PLY_ANIM) THEN
               ANIM_CT( (1610 + MX_PLY_ANIM) + CPTPLY) = 1
               NCT_ANI = NCT_ANI + 1 
             ELSE
               CALL ANCMSG(MSGID=268,MSGTYPE=MSGWARNING,I1=MX_PLY_ANIM,ANMODE=ANINFO)
             ENDIF
           ENDDO
         ENDDO
       ENDIF !
C
       IF( IEPSDOTALL_PLY == 1 )THEN
         CPTPLY = 0
         DO I=1,NUMGEO
           IF(IGEO(11,I) == 19) THEN
             DO IPT=1,IGEO(4,I)
               CPTPLY = CPTPLY + 1
               PLY_ANIM_EPSDOT( 3 * (CPTPLY - 1) + 1) = IGEO(1,I)
               PLY_ANIM_EPSDOT( 3 * (CPTPLY - 1) + 2) = 6
               PLY_ANIM_EPSDOT( 3 * (CPTPLY - 1) + 3) = IPT
               IF(CPTPLY < MX_PLY_ANIM) THEN
                 ANIM_CT( (1610+ 2*MX_PLY_ANIM) + CPTPLY) = 1
                 NCT_ANI = NCT_ANI + 1 
               ELSE
                 CALL ANCMSG(MSGID=268,MSGTYPE=MSGWARNING,I1=MX_PLY_ANIM,ANMODE=ANINFO)
               ENDIF
             ENDDO
           ENDIF
         ENDDO
c
         DO I=1,NUMPLY
           DO IPT=1,PLY_INFO(2,I)
             CPTPLY = CPTPLY + 1
             PLY_ANIM_EPSDOT( 3 * (CPTPLY - 1) + 1) = PLY_INFO(1,I)
             PLY_ANIM_EPSDOT( 3 * (CPTPLY - 1) + 2) = 6
             PLY_ANIM_EPSDOT( 3 * (CPTPLY - 1) + 3) = IPT
             IF(CPTPLY < MX_PLY_ANIM) THEN
               ANIM_CT( (1610 + 2*MX_PLY_ANIM) + CPTPLY) = 1
               NCT_ANI = NCT_ANI + 1 
             ELSE
               CALL ANCMSG(MSGID=268,MSGTYPE=MSGWARNING,I1=MX_PLY_ANIM,ANMODE=ANINFO)
             ENDIF
           ENDDO
         ENDDO
       ENDIF !
!---
       IF (ISTRAINFULL == 1) THEN
         IDX = 1810 + 3*MX_PLY_ANIM
!   ...STRAIN/N1/ALL
         IF (NIPMAX <= 100) THEN
           DO I=1,NIPMAX
             DO J=1,10
               IUS = 10*I+J
               IF (ANIM_CT(IDX + IUS) == 0 .AND. ANIM_STRAIN(I) > 0) THEN
                 ANIM_CT(IDX + IUS) = 1
                 NCT_ANI = NCT_ANI + 1
               ENDIF
             ENDDO
           ENDDO
         ENDIF
       ELSEIF (ISTRAINFULL == 2) THEN
         IDX = 1810 + 3*MX_PLY_ANIM
!   ...STRAIN/ALL/ALL
         IF (NIPMAX <= 100) THEN
           DO I=1,NIPMAX
             DO J=1,10
               IUS = 10*I+J
               IF (ANIM_CT(IDX + IUS) == 0) THEN
                 ANIM_CT(IDX + IUS) = 1
                 NCT_ANI = NCT_ANI + 1
               ENDIF
             ENDDO
           ENDDO
         ENDIF
       ENDIF ! IF (ISTRAINFULL == 1)
!---
       IF (IEPSDOFULL == 1) THEN
         IDX = 2820 + 3*MX_PLY_ANIM
!   ...EPSPDOT/N1/ALL
         IF (NIPMAX <= 100) THEN
           DO I=1,NIPMAX
             DO J=1,10
               IUS = 10*I+J
               IF (ANIM_CT(IDX + IUS) == 0 .AND. ANIM_EPSDOT(I) > 0) THEN
                 ANIM_CT(IDX + IUS) = 1
                 NCT_ANI = NCT_ANI + 1
               ENDIF
             ENDDO
           ENDDO
         ENDIF
       ELSEIF (IEPSDOFULL == 2) THEN
         IDX = 2820 + 3*MX_PLY_ANIM
!   ...EPSPDOT/ALL/ALL
         IF (NIPMAX <= 100) THEN
           DO I=1,NIPMAX
             DO J=1,10
               IUS = 10*I+J
               IF (ANIM_CT(IDX + IUS) == 0) THEN
                 ANIM_CT(IDX + IUS) = 1
                 NCT_ANI = NCT_ANI + 1
               ENDIF
             ENDDO
           ENDDO
         ENDIF
       ENDIF ! IF (IEPSDOFULL == 1)
!---
c
       IF( IPHIALL_PLY == 1 )THEN
         CPTPLY = 0
         DO I=1,NUMGEO
           IF(IGEO(11,I) == 19) THEN
             CPTPLY = CPTPLY + 1
             PLY_ANIM_PHI( 3 * (CPTPLY - 1) + 1) = IGEO(1,I)
             PLY_ANIM_PHI( 3 * (CPTPLY - 1) + 2) = 4
             IF( CPTPLY < MX_PLY_ANIM) THEN
               ANIM_CE( (11925 + MX_PLY_ANIM) + CPTPLY) = 1
               NCE_ANI = NCE_ANI + 1 
             ELSE
               CALL ANCMSG(MSGID=268,MSGTYPE=MSGWARNING,I1=MX_PLY_ANIM,ANMODE=ANINFO)
             ENDIF
           ENDIF
         ENDDO
c
         DO I=1,NUMPLY
           CPTPLY = CPTPLY + 1
           PLY_ANIM_PHI( 3 * (CPTPLY - 1) + 1) = PLY_INFO(1,I)
           PLY_ANIM_PHI( 3 * (CPTPLY - 1) + 2) = 4
           IF( CPTPLY < 11925) THEN
             ANIM_CE( (11925 + MX_PLY_ANIM) + CPTPLY) = 1
             NCE_ANI = NCE_ANI + 1 
           ELSE
             CALL ANCMSG(MSGID=268,MSGTYPE=MSGWARNING,I1=MX_PLY_ANIM,ANMODE=ANINFO)
           ENDIF
         ENDDO
       ENDIF !
C
       IF( IEPSPALL_PLY == 1 )THEN
         CPTPLY = 0
         DO I=1,NUMGEO
           IF(IGEO(11,I) == 19) THEN
             DO IPT=1,IGEO(4,I)
               CPTPLY = CPTPLY + 1
               PLY_ANIM_EPSP( 3 * (CPTPLY - 1) + 1) = IGEO(1,I)
               PLY_ANIM_EPSP( 3 * (CPTPLY - 1) + 2) = 5
               PLY_ANIM_EPSP( 3 * (CPTPLY - 1) + 3) = IPT
               IF( CPTPLY < MX_PLY_ANIM) THEN
                 ANIM_CE( (11925 + ( 2*MX_PLY_ANIM ) ) + CPTPLY) = 1
                 NCE_ANI = NCE_ANI + 1 
               ELSE
                 CALL ANCMSG(MSGID=268,MSGTYPE=MSGWARNING,I1=MX_PLY_ANIM,ANMODE=ANINFO)
               ENDIF
             ENDDO
           ENDIF
         ENDDO
c
         DO I=1,NUMPLY
           DO IPT=1,PLY_INFO(2,I)
             CPTPLY = CPTPLY + 1
             PLY_ANIM_EPSP( 3 * (CPTPLY - 1) + 1) = PLY_INFO(1,I)
             PLY_ANIM_EPSP( 3 * (CPTPLY - 1) + 2) = 5
             PLY_ANIM_EPSP( 3 * (CPTPLY - 1) + 3) = IPT
             IF( CPTPLY < MX_PLY_ANIM) THEN
               ANIM_CE( (11925 + (2*MX_PLY_ANIM)) + CPTPLY) = 1
               NCE_ANI = NCE_ANI + 1 
             ELSE
               CALL ANCMSG(MSGID=268,MSGTYPE=MSGWARNING,I1=MX_PLY_ANIM,ANMODE=ANINFO)
             ENDIF
           ENDDO
         ENDDO
       ENDIF !
C
       IF( IDAMAALL_PLY == 1 )THEN
         CPTPLY = 0
         DO I=1,NUMGEO
           IF(IGEO(11,I) == 19) THEN
             DO IPT=1,IGEO(4,I)
               CPTPLY = CPTPLY + 1
               PLY_ANIM_DAMA( 3 * (CPTPLY - 1) + 1) = IGEO(1,I)
               PLY_ANIM_DAMA( 3 * (CPTPLY - 1) + 2) = 5
               PLY_ANIM_DAMA( 3 * (CPTPLY - 1) + 3) = IPT
               IF( CPTPLY < MX_PLY_ANIM) THEN
                 ANIM_CE( (11925 + ( 3*MX_PLY_ANIM ) ) + CPTPLY) = 1
                 NCE_ANI = NCE_ANI + 1 
               ELSE
                 CALL ANCMSG(MSGID=268,MSGTYPE=MSGWARNING,I1=MX_PLY_ANIM,ANMODE=ANINFO)
               ENDIF
             ENDDO
           ENDIF
         ENDDO
c
         DO I=1,NUMPLY
           DO IPT=1,PLY_INFO(2,I)
             CPTPLY = CPTPLY + 1
             PLY_ANIM_DAMA( 3 * (CPTPLY - 1) + 1) = PLY_INFO(1,I)
             PLY_ANIM_DAMA( 3 * (CPTPLY - 1) + 2) = 7
             PLY_ANIM_DAMA( 3 * (CPTPLY - 1) + 3) = IPT
             IF( CPTPLY < MX_PLY_ANIM) THEN
               ANIM_CE( (11925 + (3*MX_PLY_ANIM)) + CPTPLY) = 1
               NCE_ANI = NCE_ANI + 1 
             ELSE
               CALL ANCMSG(MSGID=268,MSGTYPE=MSGWARNING,I1=MX_PLY_ANIM,ANMODE=ANINFO)
             ENDIF
           ENDDO
         ENDDO
       ENDIF !
!---
       IF (IDAMAFULL == 1) THEN
         IDX = 11931 + 4*MX_PLY_ANIM
!   ...DAMA/N1/ALL
         IF (NIPMAX <= 100) THEN
           DO I=1,NIPMAX
             DO J=1,10
               IUS = 10*I+J
               IF (ANIM_CE(IDX + 300 + IUS) == 0 .AND. ANIM_DAMA(I) > 0) THEN
                 ANIM_CE(IDX + 300 + IUS) = 1
                 NCE_ANI = NCE_ANI + 1
               ENDIF
             ENDDO
           ENDDO
         ENDIF
       ELSEIF (IDAMAFULL == 2) THEN
         IDX = 11931 + 4*MX_PLY_ANIM
!   ...DAMS/ALL/ALL
         IF (NIPMAX <= 100) THEN
           DO I=1,NIPMAX
             DO J=1,10
               IUS = 10*I+J
               IF (ANIM_CE(IDX + 300 + IUS) == 0) THEN
                 ANIM_CE(IDX + 300 + IUS) = 1
                 NCE_ANI = NCE_ANI + 1
               ENDIF
             ENDDO
           ENDDO
         ENDIF
       ENDIF ! IF (IDAMAFULL == 1)
!---
      IF (IBRICK_STRESSALL == 1 .OR. IBRICK_STRAINALL == 1 . OR.
     .      IBRICK_EPSPALL == 1) THEN 
        NPTR= 1
        NPTS= 1
        NPTT= 1
        IGTYP15 = 0
        DO I=1,NUMGEO
          NPT = IGEO(4,I)
          IHBE = IGEO(10,I)
          IGTYP = IGEO(11,I)
          ICSTR= IGEO(14,I)
          IF(IGTYP == 6 .OR. IGTYP == 14 ) THEN
            SELECT CASE (IHBE)
            CASE(14,16,222)                                            
              IF (NPT == 0) NPT = 222                              
              NPTR= MAX(NPTR,NPT/100)
              NPTS= MAX(NPTS,MOD(NPT/10,10))
              NPTT= MAX(NPTT,MOD(NPT,10))
            CASE(1,2,101,102,24)                                   
              NPTR= MAX(NPTR,1)
              NPTS= MAX(NPTS,1)
              NPTT= MAX(NPTT,1)
            CASE(12,112,13,17,18)                                  
              NPTR= MAX(NPTR,2)
              NPTS= MAX(NPTS,2)
              NPTT= MAX(NPTT,2) 
            END SELECT
          ELSE IF(IGTYP == 20 .OR. IGTYP == 21 .OR. IGTYP == 22) THEN            
            SELECT CASE (IHBE)
C--------due to existing Anim designs: fix thickness dir to J and limite other dirs to 2            
            CASE(14)                                            
              N14R=NPT/100
              N14S=MOD(NPT/10,10)
              N14T=MOD(NPT,10)
                SELECT CASE (ICSTR)
                  CASE(100)
                    IF (N14T==0) N14T= IGEO(15,I)
                  CASE(10)
                    IF (N14S==0) N14S= IGEO(15,I)
                  CASE(1)
                    IF (N14R==0) N14R= IGEO(15,I)
                END SELECT
c              NPTR= MAX(NPTR,N14R)
c              NPTS= MAX(NPTS,N14S)
c              NPTT= MAX(NPTT,N14T)
              N14S=MAX(N14R,N14S,N14T)
              NPTR= MAX(NPTR,2)
              NPTS= MAX(NPTS,IGEO(15,I),N14S)
              NPTT= MAX(NPTT,2)
            CASE(15)
C ---------- Isolid=15 will be treated as the same than Isolid=14:
C----------- /1j1/ or /101/j instead of /0j0/ and consisting to H3d output           
C              IGTYP15 = 1
              NPTS= MAX(NPTS,NPT)
            CASE(16)                                            
              IF (NPT == 0) NPT = 222                              
              NPTR= MAX(NPTR,NPT/100)
              NPTS= MAX(NPTS,MOD(NPT/10,10))
              NPTT= MAX(NPTT,MOD(NPT,10))
            END SELECT
          ENDIF
        ENDDO
      ENDIF !---
C       
       NPTT_PLY = 0
       DO I=1,NUMGEO
           NPT = IGEO(4,I)
           IGTYP = IGEO(11,I)
           IF(IGTYP == 19 .OR. IGTYP == 52)  NPTT_PLY = MAX (NPTT_PLY, NPT)
       ENDDO
C       
       IF (IWPLAALL == 1)THEN
         IDX =   13247 + 4*MX_PLY_ANIM
           NMAX = MIN(100,NIPMAX)
           DO I=1,NMAX
               IF (ANIM_CE(IDX + I) == 0) THEN
                 ANIM_CE(IDX + I) = 1
                 NCE_ANI = NCE_ANI + 1
               ENDIF
           ENDDO
       ENDIF
       IF (IWPLAFULL == 1) THEN
!   ...WPLA/N1/ALL
          IDX =   13547 + 4*MX_PLY_ANIM 
           NMAX = MIN(100,NIPMAX)
           DO I=1,NMAX
             DO J=1,NPTT_PLY
               IUS = 10*(I-1) + J
               IF (ANIM_CE(IDX + IUS) == 0 .AND. ANIM_WPLA(I) > 0) THEN
                 ANIM_CE(IDX + IUS) = 1
                 NCE_ANI = NCE_ANI + 1
               ENDIF
             ENDDO
           ENDDO
       ELSEIF (IWPLAFULL == 2) THEN
!   ...WPLA/ALL/ALL
           IDX =   13547 + 4*MX_PLY_ANIM
           NMAX = MIN(100,NIPMAX)
           DO I=1,NMAX
             DO J=1,NPTT_PLY
               IUS = 10*(I-1) + J
               IF (ANIM_CE(IDX + IUS) == 0) THEN
                 ANIM_CE(IDX + IUS) = 1
                 NCE_ANI = NCE_ANI + 1
               ENDIF
             ENDDO
           ENDDO
       ENDIF ! IF (IWPLAFULL == 1)  
C to see case Isolid14, iint<=100
       IF( IBRICK_STRESSALL == 1 )THEN
         IF( NPTS < 10 )THEN
C------121:1009
           DO I=1,NPTR
             DO J=1,NPTS
               DO K=1,NPTT
                 ANIM_ST(10+ (I*100) + (J*10) + K)=1
                 NST_ANI = NST_ANI + 1 
               ENDDO
             ENDDO
           ENDDO
         ELSE
C------(2010+2021):(2010+9*2010+)
           DO I=1,NPTR
             DO J=1,NPTS
               DO K=1,NPTT
                 ANIM_ST(2010+ K + (J*10) + (I*2010) ) = 1
                 NST_ANI = NST_ANI + 1 
               ENDDO
             ENDDO
           ENDDO
         ENDIF !
c--- 2020-4010
         IF( IGTYP15 == 1 )THEN
             DO J=1,NPTS
                 ANIM_ST(2010+ (J*10) ) = 1
                 NST_ANI = NST_ANI + 1 
             ENDDO
         ENDIF !
       ENDIF !
C
       IF( IBRICK_STRAINALL == 1 )THEN
         IF( NPTS < 10 )THEN
C------(1010+111):(1010+999)
           DO I=1,NPTR
             DO J=1,NPTS
               DO K=1,NPTT
                 ANIM_ST(1010+ (I*100) + (J*10) + K)=1
                 NST_ANI = NST_ANI + 1 
               ENDDO
             ENDDO
           ENDDO
         ELSE
C------(22110+2021):(22110+9*2010+1009)
           DO I=1,NPTR
             DO J=1,NPTS
               DO K=1,NPTT
                 ANIM_ST(22110+ K + (J*10) + (I*2010) ) = 1
                 NST_ANI = NST_ANI + 1 
               ENDDO
             ENDDO
           ENDDO
         ENDIF !
c
         IF( IGTYP15 == 1 )THEN
             DO J=1,NPTS
                 ANIM_ST(22110+ (J*10) ) = 1
                 NST_ANI = NST_ANI + 1 
             ENDDO
         ENDIF !
       ENDIF !
!---
       IF( IBRICK_EPSPALL == 1) THEN
         IF (NPTS < 10) THEN
           DO I=1,NPTR
             DO J=1,NPTS
               DO K=1,NPTT
                 ANIM_ST(42210 + (I*100) + (J*10) + K) = 1
                 NST_ANI = NST_ANI + 1 
               ENDDO
             ENDDO
           ENDDO
         ELSE
           DO I=1,NPTR
             DO J=1,NPTS
               DO K=1,NPTT
                 ANIM_ST(43210 + K + (J*10) + (I*2010)) = 1
                 NST_ANI = NST_ANI + 1 
               ENDDO
             ENDDO
           ENDDO
         ENDIF ! IF (NPTS < 10)
c
         IF (IGTYP15 == 1) THEN
             DO J=1,NPTS
                 ANIM_ST(43210 + (J*10)) = 1
                 NST_ANI = NST_ANI + 1 
             ENDDO
         ENDIF ! IF (IGTYP15 == 1)
       ENDIF ! IF( IBRICK_EPSPALL == 1)
!---
       IF ( IBEAM_EPSPALL > 0 ) THEN
         IF (NIPMAX_BEAM <= 100) THEN
           DO I=1,NIPMAX_BEAM
             ANIM_FE(22+I) = 1
             NFE_ANI = NFE_ANI + 1
           ENDDO
         ENDIF
       ENDIF
C
      ELSE ! IF(MCHECK == 0 ) THEN
       DO I = 1,MX_ANI
        NV_ANI = NV_ANI + ANIM_V(I)
        NT_ANI = NT_ANI + ANIM_T(I)
        NE_ANI = NE_ANI + ANIM_E(I)
        NN_ANI = NN_ANI + ANIM_N(I)
        NST_ANI = NST_ANI + ANIM_ST(I)
        NSE_ANI = NSE_ANI + ANIM_SE(I)
        NCT_ANI = NCT_ANI + ANIM_CT(I)
        NCE_ANI = NCE_ANI + ANIM_CE(I)
        NFT_ANI = NFT_ANI + ANIM_FT(I)
        NFE_ANI = NFE_ANI + ANIM_FE(I)
       ENDDO
       IF(ANIM_V(12) == 1)NV_ANI=NV_ANI+1
       IF(ANIM_V(4)==1.AND.ANIMCONT == 0) NV_ANI=NV_ANI-1
       IF(ANIM_V(27).EQ.1)NV_ANI=NV_ANI+1
      ENDIF
C-----
      RETURN
      END
