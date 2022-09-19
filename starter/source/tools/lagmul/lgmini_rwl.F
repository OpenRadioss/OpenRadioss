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
Chd|  LGMINI_RWL                    source/tools/lagmul/lgmini_rwl.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE LGMINI_RWL( NPRW, LPRW, MASS, ITAB,NOM_OPT)
      USE MESSAGE_MOD
C----------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "lagmult.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NPRW(*),LPRW(*),ITAB(*)
      my_real
     .         MASS(*)
      INTEGER NOM_OPT(LNOPT1,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, NSL , NNO, CONT, MSN
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
C======================================================================|
      CONT=0
      DO N=1,NRWALL
         NSL= NPRW(N)
         ID=NOM_OPT(1,N)
         CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,N),LTITR)
         IF (NPRW(N+5*NRWALL) /= 0) THEN
           DO I=1,NSL
            NNO = IABS(LPRW(CONT+I))
              IF (MASS(NNO) == ZERO) THEN
                CALL ANCMSG(MSGID=535,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO_BLIND_1,
     .                      C1='RIGID WALL SECONDARY',
     .                      I1=ID,
     .                      C2='RIGID WALL SECONDARY',
     .                      C3=TITR,C4='SECONDARY',
     .                      I2=ITAB(IABS(LPRW(CONT+I))))
              ENDIF
           ENDDO
           MSN = NPRW(N+2*NRWALL)
           IF (MSN /= 0) THEN
             IF (MASS(MSN) == ZERO) THEN
               CALL ANCMSG(MSGID=535,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   C1='RIGID WALL MAIN',
     .                   I1=ID,
     .                   C2='RIGID WALL MAIN',
     .                   C3=TITR,C4='MAIN',
     .                   I2=ITAB(NPRW(N+2*NRWALL)))
             ENDIF
           ENDIF
         ENDIF
         CONT=CONT+NSL
      ENDDO
      RETURN
      END
