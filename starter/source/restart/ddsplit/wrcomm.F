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
Chd|  WRCOMR                        source/restart/ddsplit/wrcomm.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        ANIM_MOD                      ../common_source/modules/anim_mod.F
Chd|====================================================================
      SUBROUTINE WRCOMR(LMXVREA,LVARREA,TABVREA,MS_ND,P)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ANIM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LMXVREA,LVARREA, I
      my_real TABVREA(*),MS_ND
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com06_c.inc"
#include      "com07_c.inc"
#include      "com08_c.inc"
#include      "lagmult.inc"
#include      "remesh_c.inc"
#include      "scr18_c.inc"
#include      "sms_c.inc"
#include      "sphcom.inc"
#include      "sysunit.inc"
#include      "inter22.inc"
#include      "mmale51_c.inc"
#include      "thermal_c.inc"
C-----------------------------------------------
#include      "tablen_c.inc"
#include      "ddspmd_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IVAR,J,RVARINT,P
C--------------------------------------
C     PREPARATION DES VARIABLES REELLES POUR ECRITURE SUR RFILE.
C--------------------------------------
C-----
      DO IVAR=1,LMXVREA
       TABVREA(IVAR)=ZERO
      ENDDO
C-----
      TABVREA(1)=TT
      TABVREA(2)=DT1
      TABVREA(3)=DT2
      TABVREA(4)=DT12
      TABVREA(5)=DT2OLD
      TABVREA(6)=TSTOP
      TABVREA(7)=TDES
      TABVREA(8)=THIS
C-----
      TABVREA(9)=DTDES
      TABVREA(10)=DTHIS
      TABVREA(11)=DTFAC
      TABVREA(12)=DTMIN
      TABVREA(13)=VOLMIN
          TABVREA(14)=TFEXT
      TABVREA(15)=REINT
      TABVREA(16)=DTPAT
      TABVREA(17)=TPAT
      TABVREA(18)=DTANIM
      TABVREA(19)=TANIM
      TABVREA(20)=UREINT
      TABVREA(21)=ECONTV
      TABVREA(22)=EHOUR
      TABVREA(23)=DTOUTP
      TABVREA(24)=TOUTP
      TABVREA(25)=T1S
      TABVREA(26)=DT2S
      TABVREA(27)=DT12S
      TABVREA(28)=USREINT
      TABVREA(29)=R2RFX1
      TABVREA(30)=R2RFX2
      TABVREA(31)=DAMPA
      TABVREA(32)=DAMPB
      TABVREA(33)=DAMPW
C-----
      TABVREA(34)=ALPHA
      TABVREA(35)=GAMMA
      TABVREA(36)=VGX
      TABVREA(37)=VGY
      TABVREA(38)=VGZ
C-----
      TABVREA(39)=SPASORT
      TABVREA(40)=SPATRUE
       DO I=1,9
         TABVREA(40+I) = DTHIS1(I)
         TABVREA(49+I) = THIS1(I)
       ENDDO
         TABVREA(59) = ZERO
         TABVREA(60) = ZERO
      TABVREA(61)=LAGM_TOL
      TABVREA(62)=LAG_ALPH
      TABVREA(63)=LAG_ALPHS
      LVARREA=63
C 
C---- debut check point restart
C
      DO I = 1, 42
       TABVREA(63+I)=ZERO
      ENDDO
C 
C---- fin check point restart
C
      LVARREA= 105
      TABVREA(106)=DWMAD
      LVARREA=106
      TABVREA(107)=FAC_MASS
      TABVREA(108)=FAC_LENGTH
      TABVREA(109)=FAC_TIME
      TABVREA(110)=T1SH
      TABVREA(111) =  UPWSM
      RVARINT = 111
      RVARINT = RVARINT + 1
      TABVREA(RVARINT) = TPSREF
C THERMAL ANALYSIS
      RVARINT = RVARINT + 1
      TABVREA(RVARINT) = HEAT_STORED
      RVARINT = RVARINT + 1
      TABVREA(RVARINT) = HEAT_MECA
      RVARINT = RVARINT + 1
      TABVREA(RVARINT) = HEAT_CONV
      RVARINT = RVARINT + 1
      TABVREA(RVARINT) = HEAT_RADIA
      RVARINT = RVARINT + 1
      TABVREA(RVARINT) = HEAT_FFLUX
C TABLEAU SOL1TNL
      DO I=0, MAXLAW
        RVARINT = RVARINT + 1
        TABVREA(RVARINT) = SOL1TNL(I,1)
        RVARINT = RVARINT + 1
        TABVREA(RVARINT) = SOL1TNL(I,2)
      ENDDO
C TABLEAU SOL8TNL
      DO I=0, MAXLAW
        RVARINT = RVARINT + 1
        TABVREA(RVARINT) = SOL8TNL(I,1)
        RVARINT = RVARINT + 1
        TABVREA(RVARINT) = SOL8TNL(I,2)
      ENDDO
C TABLEAU TET4TNL
      DO I=0, MAXLAW
        RVARINT = RVARINT + 1
        TABVREA(RVARINT) = TET4TNL(I,1)
      ENDDO
C TABLEAU TET10TNL
      DO I=0, MAXLAW
        RVARINT = RVARINT + 1
        TABVREA(RVARINT) = TET10TNL(I,1)
      ENDDO
C TABLEAU SHTNL
      DO I=0, MAXLAW
        DO J=0,3
          RVARINT = RVARINT + 1
          TABVREA(RVARINT) = SHTNL(I,J,1)
          RVARINT = RVARINT + 1
          TABVREA(RVARINT) = SHTNL(I,J,2)
          RVARINT = RVARINT + 1
          TABVREA(RVARINT) = SHTNL(I,J,3)
        ENDDO
      ENDDO
C  TABLEAU TRITNL
      DO I=0, MAXLAW
        DO J=0,3
          RVARINT = RVARINT + 1
          TABVREA(RVARINT) = TRITNL(I,J,1)
          RVARINT = RVARINT + 1
          TABVREA(RVARINT) = TRITNL(I,J,2)
          RVARINT = RVARINT + 1
          TABVREA(RVARINT) = TRITNL(I,J,3)
        ENDDO
      ENDDO
C  TABLEAU SOLTELT
      DO I=1,8
        RVARINT = RVARINT + 1
        TABVREA(RVARINT) = SOLTELT(I)
      ENDDO
        RVARINT = RVARINT + 1
        TABVREA(RVARINT) = TET4TELT(1)
        RVARINT = RVARINT + 1
        TABVREA(RVARINT) = TET10TELT(1)
C  TABLEAU SHTELT
      DO I=1,10
        RVARINT = RVARINT + 1
        TABVREA(RVARINT) = SHTELT(I)
      ENDDO
C  TABLEAU TRITELT
      DO I=1,10
        RVARINT = RVARINT + 1
        TABVREA(RVARINT) = TRITELT(I)
      ENDDO
C
C .sta files
      RVARINT = RVARINT + 1
      TABVREA(RVARINT) = DTSTAT
      RVARINT = RVARINT + 1
      TABVREA(RVARINT) = TSTAT
C .dynain files
      RVARINT = RVARINT + 1
      TABVREA(RVARINT) = DTDYNAIN
      RVARINT = RVARINT + 1
      TABVREA(RVARINT) = TDYNAIN
C
      RVARINT = RVARINT + 1
      TABVREA(RVARINT) = TADMESH
      RVARINT = RVARINT + 1
      TABVREA(RVARINT) = DTADMESH
C
      RVARINT = RVARINT + 1
      TABVREA(RVARINT) = DTFACS
      RVARINT = RVARINT + 1
      TABVREA(RVARINT) = DTMINS
      RVARINT = RVARINT + 1
      TABVREA(RVARINT) = TOL_SMS
C abf files
      DO I=1,10
        RVARINT = RVARINT + 1
        TABVREA(RVARINT) = TABFIS(I)
        RVARINT = RVARINT + 1
        TABVREA(RVARINT) = DTABF(I)
        RVARINT = RVARINT + 1
        TABVREA(RVARINT) = DTABFWR(I)
        RVARINT = RVARINT + 1
        TABVREA(RVARINT) = TABFWR(I)
      ENDDO
C
C
      RVARINT = RVARINT + 1
      TABVREA(RVARINT) = RATIO22
C
      RVARINT = RVARINT + 1
      TABVREA(RVARINT) = M51_IFLG6
C      
      RVARINT = RVARINT + 1
      TABVREA(RVARINT) = MS_ND ! MASS Middle nodes Itet=2
C      
      RVARINT = RVARINT + 1
      TABVREA(RVARINT) = ZERO ! DMSI2 Itet=2
      RVARINT = RVARINT + 1
      TABVREA(RVARINT) = ZERO ! MSI20 Itet=2
C      
C---- debut check point restart      
C
C     DTFAC1(51) and DTMIN1(51)
      RVARINT = RVARINT + 1
      TABVREA(RVARINT) = ZERO
      RVARINT = RVARINT + 1
      TABVREA(RVARINT) = ZERO
C     DTFAC1(52) and DTMIN1(52)
      RVARINT = RVARINT + 1
      TABVREA(RVARINT) = ZERO
      RVARINT = RVARINT + 1
      TABVREA(RVARINT) = ZERO
C
      !DTFAC1(102) - DTMIN1(102) - /DT/ALE
      RVARINT = RVARINT + 1
      TABVREA(RVARINT) = ZERO
      RVARINT = RVARINT + 1
      TABVREA(RVARINT) = ZERO
C      
      RVARINT = RVARINT + 1
      TABVREA(RVARINT) = ZERO ! DTFACS_INT
      RVARINT = RVARINT + 1
      TABVREA(RVARINT) = ZERO ! DTMINS_INT
C      
C     BETATE_0, EINT_0,ENCIN_0,PIMAX ,PCMAX,PINT ,PCIN ,FILtr+8,NFIRST 
      DO I = 1, 16
      RVARINT = RVARINT + 1
      TABVREA(RVARINT) = ZERO 
      END DO
C
      RVARINT = RVARINT + 2
C  ENCONTD , ECONT_CUMU 
      RVARINT = RVARINT + 1
      TABVREA(RVARINT) = ECONTD 
      RVARINT = RVARINT + 1
      TABVREA(RVARINT) = ECONT_CUMU
C  INITIAL MASS FROM STARTER
      RVARINT = RVARINT + 1
      TABVREA(RVARINT) = MASS0_START
      RVARINT = RVARINT + 1
      TABVREA(RVARINT) = ZERO    
C ANIM FILES STOP TIME
      RVARINT = RVARINT + 1
      TABVREA(RVARINT) = TANIM_STOP      
C
C---- fin check point restart      
C      
      LVARREA=RVARINT
C----------------------------------------------------------------
C    warning to radioss developers : Check LVARREA < LMXVREA
C----------------------------------------------------------------
      IF(LVARREA>LMXVREA)STOP 123
C
      RETURN
      END
