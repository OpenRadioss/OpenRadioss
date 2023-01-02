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
Chd|  LGMINI_RBY                    source/tools/lagmul/lgmini_rby.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE LGMINI_RBY(NPBYL  ,LPBYL  ,RBYL   ,MASS   ,INER   ,
     .                      X      ,V      ,VR     ,ITAB   ,NOM_OPT)
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
#include      "lagmult.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPBYL(NNPBY,*), LPBYL(*), ITAB(*)
      my_real
     .        RBYL(NRBY,*),MASS(*),INER(*),X(3,*), V(3,*), VR(3,*)
      INTEGER NOM_OPT(LNOPT1,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,NN,M,IK,IC,MSL,NSL,TNSL,ICDG
      my_real
     .        RX,RY,RZ,MASRB
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
C======================================================================|
      TNSL = 0
      DO N=1,NRBYLAG
        M    = NPBYL(1,N)
        MSL  = NPBYL(2,N)
        ICDG = NPBYL(3,N)
        NSL  = MSL - 1
        MASS(M) = MASS(M) + RBYL(1,N)
        INER(M) = INER(M) + RBYL(2,N)
        ID=NOM_OPT(1,NRBYKIN+N)
        CALL FRETITL2(TITR,
     .                NOM_OPT(LNOPT1-LTITR+1,NRBYKIN+N),LTITR)
        IF(MASS(M)==ZERO.OR.INER(M)==ZERO) THEN
           CALL ANCMSG(MSGID=273,
     .                 MSGTYPE=MSGWARNING,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR)
        ENDIF
        IF(MASS(M)==ZERO) MASS(M)= EM15
        IF(INER(M)==ZERO) INER(M)= EM15
C-----  CORRECTION DE LA CENTRE DE GRAVITE DU MAIN
        IF(ICDG==1)THEN
C-----    CDG TOTAL
          MASRB = MASS(M)
          DO J=1,3
            X(J,M)=X(J,M)*MASS(M)
          ENDDO
          DO I=1,NSL
            NN = LPBYL(TNSL+I)
            DO J=1,3
             X(J,M) = X(J,M)+X(J,NN)*MASS(NN)
            ENDDO
            MASRB   = MASRB+MASS(NN)
          ENDDO
          IF(MASRB<=EM30) THEN
            CALL ANCMSG(MSGID=273,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=ID,
     .                  C1=TITR)
            RETURN
          ENDIF
          DO J=1,3
            X(J,M)=X(J,M)/MASRB
          ENDDO
        ELSEIF(ICDG==2)THEN
C-----    CDG DES NOEUDS SECONDS
          MASRB=ZERO
          DO J=1,3
            X(J,M)=ZERO
          ENDDO
          DO I=1,NSL
            NN = LPBYL(TNSL+I)
            DO J=1,3
             X(J,M) = X(J,M)+X(J,NN)*MASS(NN)
            ENDDO
            MASRB   = MASRB+MASS(NN)
          ENDDO
C
          IF(MASRB<=EM30) THEN
            CALL ANCMSG(MSGID=273,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=ID,
     .                  C1=TITR)
            RETURN
          ENDIF
          DO J=1,3
            X(J,M)=X(J,M)/MASRB
          ENDDO
          MASRB=MASRB+MASS(M)
        ENDIF
        IF(MASS(M)==ZERO.OR.INER(M)==ZERO) THEN
           CALL ANCMSG(MSGID=679,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 C2='ON MAIN NODE')
        ENDIF
        TNSL = TNSL + 3*MSL
      ENDDO
C-----------
      RETURN
      END
