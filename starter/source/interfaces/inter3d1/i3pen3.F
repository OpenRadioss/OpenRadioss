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
C
Chd|====================================================================
Chd|  I3PEN3                        source/interfaces/inter3d1/i3pen3.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        INIST3                        source/interfaces/inter3d1/inist3.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I3PEN3(X    ,IRECT ,MSR  ,NSV  ,ILOC ,
     1                  IRTL ,CST   ,IRTL0,GAP  ,NSN  ,
     2                  ITAB ,IWPENE,ID,TITR)
      USE MESSAGE_MOD
C
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
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "scr03_c.inc"
#include      "scr07_c.2.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN,IWPENE
C     REAL ou REAL*8
      my_real
     .   GAP
      INTEGER IRECT(4,*), MSR(*), NSV(*), ILOC(*), IRTL(*), IRTL0(*),
     .   ITAB(*)
C     REAL ou REAL*8
      my_real
     .   X(3,*), CST(2,*)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II, I, J, K, L, JJ, NN, IER
C     REAL ou REAL*8
      my_real
     .   N1, N2, N3, PEN, ALP
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C
      ALP = TWOEM2
      DO II=1,NSN
        I=NSV(II)
        J=ILOC(II)
        K=MSR(J)
        L=IRTL(II)
        DO JJ=1,4
          NN=MSR(IRECT(JJ,L))
          XX1(JJ)=X(1,NN)
          XX2(JJ)=X(2,NN)
          XX3(JJ)=X(3,NN)
        ENDDO
        XS1=X(1,I)
        YS1=X(2,I)
        ZS1=X(3,I)
        CALL INIST3(N1,N2,N3,CST(1,II),CST(2,II),IER,ALP)
        IF(IER.EQ.-1)THEN
C         ** ERROR INTERFACE SEGMENT DEFINITION
C         ** ERROR NULL SEGMENT AREA
          CALL ANCMSG(MSGID=85,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=ID,
     .               C1=TITR,
     .               I2=ITAB(I),
     .               I3=ITAB(K),
     .               I4=L,
     .               I5=ITAB(MSR(IRECT(1,L))),
     .               I6=ITAB(MSR(IRECT(2,L))),
     .               I7=ITAB(MSR(IRECT(3,L))),
     .               I8=ITAB(MSR(IRECT(4,L))))
C
        ELSE IF(IER.EQ.1 .AND. IPRI.GE.1)THEN
          WRITE(IOUT,FMT=FMW_7I) ITAB(I),ITAB(K),L,(ITAB(MSR(IRECT(JJ,L))),JJ=1,4)
        ELSE
          PEN=N1*(XS1-XC)+N2*(YS1-YC)+N3*(ZS1-ZC)-GAP
          IF(PEN.LE.ZERO) IRTL0(II)=L
          IF(PEN.LT.ZERO) THEN
C           ** WARNING ** INITIAL PENETRATION 
            CALL ANCMSG(MSGID=346,
     .                 MSGTYPE=MSGWARNING,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 I1=ID,I2=ITAB(I),
     .                 C1=TITR,
     .                 R1=PEN)
C
            IWPENE=IWPENE+1
          ENDIF
          IF(IPRI.GE.1) THEN
            WRITE(IOUT,FMT=FMW_7I_2F) ITAB(I),ITAB(K),L,(ITAB(MSR(IRECT(JJ,L))),JJ=1,4),CST(1,II),CST(2,II)
          ENDIF
        ENDIF
      ENDDO
      RETURN
      END
Chd|====================================================================
Chd|  I5PWR3                        source/interfaces/inter3d1/i3pen3.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        INIST3                        source/interfaces/inter3d1/inist3.F
Chd|====================================================================
      SUBROUTINE I5PWR3(X    ,IRECT ,MSR  ,NSV  ,ILOC ,
     1                  IRTL ,CST   ,IRTL0,GAP  ,NSN  ,
     2                  ITAB ,INACTI)
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN,INACTI
C     REAL ou REAL*8
      my_real
     .   GAP
      INTEGER IRECT(4,*), MSR(*), NSV(*), ILOC(*), IRTL(*), IRTL0(*),
     .   ITAB(*)
C     REAL ou REAL*8
      my_real
     .   X(3,*), CST(2,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "scr03_c.inc"
#include      "scr07_c.2.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II, I, J, K, L, JJ, NN, IER
C     REAL ou REAL*8
      my_real
     .   N1, N2, N3, PEN, ALP
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C
      ALP = TWOEM2
      DO II=1,NSN
        I=NSV(II)
        J=ILOC(II)
        K=MSR(J)
        L=IRTL(II)
        DO JJ=1,4
          NN=MSR(IRECT(JJ,L))
          XX1(JJ)=X(1,NN)
          XX2(JJ)=X(2,NN)
          XX3(JJ)=X(3,NN)
        ENDDO
        XS1=X(1,I)
        YS1=X(2,I)
        ZS1=X(3,I)
        CALL INIST3(N1,N2,N3,CST(1,II),CST(2,II),IER,ALP)
        IF(IER.EQ.0)THEN
         PEN=N1*(XS1-XC)+N2*(YS1-YC)+N3*(ZS1-ZC)-GAP
         IF(PEN.LT.ZERO) THEN
           PEN = PEN + EM8*PEN
           IF(INACTI.EQ.3) THEN
             WRITE(IOUT,1000)PEN
             X(1,I) = XS1 - PEN*N1
             X(2,I) = YS1 - PEN*N2
             X(3,I) = ZS1 - PEN*N3
             WRITE(IOUT,FMT=FMW_I_3F)ITAB(I),X(1,I),X(2,I),X(3,I)
           ELSE IF(INACTI.EQ.4) THEN
             DO JJ=1,4
              NN=MSR(IRECT(JJ,L))
              WRITE(IOUT,1100)PEN
              X(1,NN) = XX1(JJ) + PEN*N1
              X(2,NN) = XX2(JJ) + PEN*N2
              X(3,NN) = XX3(JJ) + PEN*N3
              WRITE(IOUT,FMT=FMW_I_3F)ITAB(NN),X(1,NN),X(2,NN),X(3,NN)
            END DO
           END IF
          END IF !IF(PEN.LT.ZERO)
        END IF
      ENDDO
      RETURN
 1000 FORMAT(2X,'** INITIAL PENETRATION =',1PG20.13,
     . ' CHANGE COORDINATES OF SECONDARY NODE TO:')
 1100 FORMAT(2X,'** INITIAL PENETRATION =',1PG20.13,
     . ' CHANGE COORDINATES OF MAIN NODE TO:')
      END
