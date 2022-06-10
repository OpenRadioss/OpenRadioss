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
Chd|  ADDMASPART                    source/tools/admas/addmaspart.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|====================================================================
      SUBROUTINE ADDMASPART(IPART,IPMAS,PARTSAV,
     .                      PART_AREA,PM,ADDEDMS,NOM_OPT,PARTSAV_PON)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE OPTIONDEF_MOD
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
#include      "com01_c.inc"
#include      "scr17_c.inc"
#include      "scr03_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPART(LIPART1,*)
C     REAL
      my_real
     .   PARTSAV(20,*),PARTSAV_PON(*),PART_AREA(*),
     .   PM(NPROPM,*),ADDEDMS(*)
      INTEGER NOM_OPT(LNOPT1,*)
!
      TYPE (ADMAS_)  , DIMENSION(NODMAS)  :: IPMAS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ID_MASS,IPA,IP,NMAS,KAD,II,IMID,ITYPE,IFLAG
C     REAL
      my_real
     .   AMAS,MSP,MASST(NODMAS),RATIO,ADDMAS
      CHARACTER*nchartitle,
     .   TITR
C=======================================================================
!---
      MASST(1:NODMAS) = ZERO
!
      DO 10 I=1,NODMAS
        ID_MASS  = IPMAS(I)%ID
        NMAS = IPMAS(I)%NPART
        ITYPE= IPMAS(I)%TYPE
        IFLAG= IPMAS(I)%WEIGHT_FLAG
        IF (ITYPE /= 3 .and. ITYPE /= 4 .and. 
     .      ITYPE /= 6 .and. ITYPE /= 7) GOTO 10
!
        CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,I),LTITR)
!
! ckeck for type 4 and 7 :
! if AMAS (replacing mass) < MSP (structured mass of concerned parts or list of parts)
        IF (ITYPE == 4) THEN
          MSP = ZERO ! structured mass of liste of parts
          AMAS = ZERO
          DO II = 1,NMAS
            IP = IPMAS(I)%PARTID(II)
            MSP = MSP + PARTSAV_PON(IP) ! structured mass (of list of parts)
            AMAS = IPMAS(I)%PART(II)%RPMAS
          ENDDO ! DO II = 1,NMAS
          IF (AMAS > MSP) THEN
            ADDMAS = AMAS - MSP
            DO II =1,NMAS
              IP = IPMAS(I)%PARTID(II)
              IPMAS(I)%PART(II)%RPMAS =  ADDMAS ! non structural mass to be added (distributed)
            ENDDO ! DO II = KAD
          ELSE
            CALL ANCMSG(MSGID=1576,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANSTOP,
     .                  I1=ID_MASS,
     .                  C1=IPMAS(I)%TITLE,
     .                  R1=MSP-AMAS)
          ENDIF ! IF (AMAS > MSP)
        ELSEIF (ITYPE == 7) THEN
          DO II = 1,NMAS
            IP = IPMAS(I)%PARTID(II)
            MSP = PARTSAV_PON(IP) ! structured mass of part
            AMAS = IPMAS(I)%PART(II)%RPMAS ! non structural mass to be added (distributed)
            IF (AMAS > MSP) THEN
              ADDMAS = AMAS - MSP
              IPMAS(I)%PART(II)%RPMAS = ADDMAS ! non structural mass to be added (distributed)
            ELSE
              IPA = IPART(4,IP)
              CALL ANCMSG(MSGID=877,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANSTOP,
     .                    I1=ID_MASS,
     .                    C1=IPMAS(I)%TITLE,
     .                    R1=MSP-AMAS,
     .                    I2=IPA)
            ENDIF ! IF (AMAS > MSP)
          ENDDO ! DO II = 1,NMAS
        ENDIF ! IF (ITYPE)
!
!  PART MASS DISTRIBUTION :
!
        DO II = 1,NMAS
          IP = IPMAS(I)%PARTID(II)
          MSP = PARTSAV_PON(IP)        ! ---> mass tag of PART
          MASST(I) = MASST(I) + MSP
        ENDDO
!---
        DO II = 1,NMAS
          IP = IPMAS(I)%PARTID(II)
          MSP  = PARTSAV_PON(IP) ! structured mass part
          AMAS = IPMAS(I)%PART(II)%RPMAS     ! added mass part
!---
          IF (ITYPE == 3 .OR. ITYPE == 4) THEN
            RATIO = MSP / MASST(I)
            AMAS = AMAS * RATIO
            IPMAS(I)%PART(II)%RPMAS = AMAS
          ELSEIF (ITYPE == 6 .OR. ITYPE == 7) THEN
            IPMAS(I)%PART(II)%RPMAS = AMAS
          ENDIF ! IF (ITYPE)
!---
          ADDEDMS(IP) = ADDEDMS(IP) + IPMAS(I)%PART(II)%RPMAS
!---
        ENDDO ! DO II = 1,NMAS
!---
 10   CONTINUE
!---
      RETURN
      END
