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
Chd|  INTER_DCOD_FRICTION           source/interfaces/reader/inter_dcod_friction.F
Chd|-- called by -----------
Chd|        HM_READ_INTER_TYPE07          source/interfaces/int07/hm_read_inter_type07.F
Chd|        HM_READ_INTER_TYPE11          source/interfaces/int11/hm_read_inter_type11.F
Chd|        HM_READ_INTER_TYPE24          source/interfaces/int24/hm_read_inter_type24.F
Chd|        HM_READ_INTER_TYPE25          source/interfaces/int25/hm_read_inter_type25.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        INTBUF_FRIC_MOD               share/modules1/intbuf_fric_mod.F
Chd|        INTSTAMP_MOD                  share/modules1/intstamp_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE INTER_DCOD_FRICTION(NTYP,NI,IPARI,NOM_OPT,NOM_OPTFRIC,
     .           INTBUF_FRIC_TAB)
C-----------------------------------------------
C     DECODE USER NUMBERS  
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INTSTAMP_MOD
      USE INTBUF_FRIC_MOD        
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
#include      "lagmult.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "com01_c.inc"
#include      "com10_c.inc"
#include      "warn_c.inc"
#include      "units_c.inc"
#include      "sphcom.inc"
#include      "scr10_c.inc"
#include      "scr17_c.inc"
#include      "intstamp_c.inc"
#include      "thermal_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NTYP,NI,IPARI(*),NOM_OPT(LNOPT1,*),
     .    NOM_OPTFRIC(LNOPT1,*)
      TYPE(INTBUF_FRIC_STRUCT_) INTBUF_FRIC_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ID,IERR1,IERR2,OK,IDF ,INTFRIC,IFQ
      CHARACTER*nchartitle,
     .   TITR
C     DATA IUN/1/
C
C=========================================================
C
      ID  = NOM_OPT(1,NI)
      CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,NI),LTITR)
C
C------------------------------------------------
C     Friction model for interface
C-------------------------------------------
C
      OK = 0
      IF(NTYP.EQ.7.OR.NTYP.EQ.11.OR.NTYP.EQ.19.OR.NTYP.EQ.24.OR.NTYP.EQ.25) THEN
          IF (IPARI(72) > 0) THEN   
            OK = 0
            DO J=1,NINTERFRIC  
              IDF  = NOM_OPTFRIC(1,J)
              IF(IPARI(72) == IDF) THEN 
                 IPARI(72)=J                
                 OK = 1               
                 EXIT
              ENDIF                           
            ENDDO    
            IF (OK == 0) THEN                         
               CALL ANCMSG(MSGID=1592,            
     .                 MSGTYPE=MSGERROR,        
     .                 ANMODE=ANINFO_BLIND_1,   
     .                 I1=ID,                   
     .                 C1=TITR,                 
     .                I2=IPARI(72))          
            ENDIF
          ENDIF
          IF (NTYP.EQ.11.AND.IPARI(72) > 0) THEN  
              INTFRIC = IPARI(72) 
              IF(INTBUF_FRIC_TAB(INTFRIC)%FRICMOD > 0 ) THEN
                CALL ANCMSG(MSGID=1595,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_1,   
     .                  I1=ID,                   
     .                  C1=TITR,                 
     .                  I2=NOM_OPTFRIC(1,INTFRIC))  
              ENDIF
             IPARI(30) = INTBUF_FRIC_TAB(INTFRIC)%FRICFORM  ! we put the iform  flag to the value set in friction interface
          ENDIF

          IF ((NTYP.EQ.24..OR.NTYP.EQ.25).AND.IPARI(72) > 0) THEN  ! correction of filtering parameter if Iform = 0 in friction interface
             INTFRIC = IPARI(72) 
             IFQ = INTBUF_FRIC_TAB(INTFRIC)%IFFILTER
             IF (IFQ.LT.10) IFQ = IFQ + 10
             INTBUF_FRIC_TAB(INTFRIC)%IFFILTER = IFQ
             IF (IFQ.EQ.10) INTBUF_FRIC_TAB(INTFRIC)%XFILTR_FRIC = ONE
          ENDIF

          IF ((NTYP.EQ.7.OR.NTYP.EQ.24..OR.NTYP.EQ.25).AND.IPARI(72) > 0) THEN ! we put the MFROT ifq flag to the value set in friction interface 
              INTFRIC = IPARI(72) 
              IPARI(30) = INTBUF_FRIC_TAB(INTFRIC)%FRICMOD 
              IPARI(31) = INTBUF_FRIC_TAB(INTFRIC)%IFFILTER
          ENDIF

       ENDIF
      RETURN
C-----
      END
C
