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
Chd|  CM27IN3                       source/materials/mat/mat027/cm27in3.F
Chd|-- called by -----------
Chd|        C3INIT3                       source/elements/sh3n/coque3n/c3init3.F
Chd|        CINIT3                        source/elements/shell/coque/cinit3.F
Chd|        CMAINI3                       source/elements/sh3n/coquedk/cmaini3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE CM27IN3(ELBUF_STR,
     .                   GEO ,IGEO,PM ,IPM ,IX ,NIX,
     .                   NLAY,IR  ,IS ,IMAT )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
      USE MESSAGE_MOD
C-----------------------------------------------
C INITIALISE LES DIRECTIONS DE FISSURES
C INITIALISE LES EPAISSEURS ET LES MATERIAUX DES COUCHES
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "warn_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IGEO(NPROPGI,*),IX(NIX,*),NIX,IPM(NPROPMI,*),NLAY,IR,IS,IMAT
      my_real
     .   GEO(NPROPG,*),PM(NPROPM,*)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IPTHK,IPMAT,IPPOS,MID,MLN,IMID,IPID,PID
      INTEGER I,N,I1,I2,I3,J,IGTYP,II,L_DMG,ILAYER,IT,NPTT
      my_real THKL,POS0,DP
      CHARACTER*nchartitle, TITR,TITR1
      my_real, DIMENSION(:), POINTER :: DIR_DMG
C
      TYPE(BUF_LAY_) ,POINTER :: BUFLY
      TYPE(L_BUFEL_) ,POINTER :: LBUF
C======================================================================|
      IF(NPT==0)THEN
        IMID=IX(1,1)
        IPID = IX(NIX-1,1)
        PID = IGEO(1,IPID)
        MID = IPM(1,IMID)
        MLN = NINT(PM(19,IMID))
        CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1,PID),LTITR)
        CALL FRETITL2(TITR1,IPM(NPROPMI-LTITR+1,IMID),LTITR)
        CALL ANCMSG(MSGID=23,
     .              ANMODE=ANINFO,
     .              MSGTYPE=MSGERROR,
     .              I1=PID,
     .              C1=TITR,
     .              I2=MID,
     .              C2=TITR1,
     .              I3=27)
      ENDIF
C
      IGTYP=NINT(GEO(12,IMAT))
      IF (IGTYP /= 51 .AND. IGTYP /= 52) THEN
        DO N=1,NPT
          ILAYER = N
          IF (NLAY > 1) THEN
            LBUF => ELBUF_STR%BUFLY(ILAYER)%LBUF(IR,IS,1)
          ELSE
            LBUF => ELBUF_STR%BUFLY(1)%LBUF(IR,IS,ILAYER)
          ENDIF
C
          L_DMG = ELBUF_STR%BUFLY(1)%L_DMG
          DIR_DMG => LBUF%DMG(1:L_DMG*LLT)
C
          DO I=LFT,LLT
            DIR_DMG(I) = ONE
            DIR_DMG(I+LLT) = ZERO
          ENDDO
        ENDDO
      ELSEIF (IGTYP == 51) THEN
        DO ILAYER=1,NLAY
          NPTT = ELBUF_STR%BUFLY(ILAYER)%NPTT
          DO IT=1,NPTT
            LBUF => ELBUF_STR%BUFLY(ILAYER)%LBUF(IR,IS,IT)
C
            L_DMG = ELBUF_STR%BUFLY(ILAYER)%L_DMG
            DIR_DMG => LBUF%DMG(1:L_DMG*LLT)
C
            DO I=LFT,LLT
              DIR_DMG(I) = ONE
              DIR_DMG(I+LLT) = ZERO
            ENDDO
          ENDDO
        ENDDO
      ENDIF
C
      IPTHK = 300
      IPPOS = 400
      IPMAT = 100
      IF(IGTYP/=11 .AND. IGTYP/=17 . AND. IGTYP/=51) THEN
        THKL = ONE / NPT
        POS0 =-HALF*(ONE + THKL)
        DO N=1,NPT                                                
          I1=IPPOS+N                                              
          I2=IPTHK+N                                              
          I3=IPMAT+N                                              
          DP =  N*THKL
          DO I=LFT,LLT                                            
            GEO(I1,IMAT) = POS0 + DP 
            GEO(I2,IMAT) = THKL                              
          ENDDO                                                   
        ENDDO                                                     
      ENDIF
C-----------
      RETURN
      END
