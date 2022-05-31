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
Chd|  I3STI2                        source/interfaces/inter2d1/i3sti2.F
Chd|-- called by -----------
Chd|        ININT2                        source/interfaces/inter2d1/inint2.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        INARE2                        source/interfaces/inter2d1/inare2.F
Chd|        INORI2                        source/interfaces/inter2d1/inori2.F
Chd|        INRCH2                        source/interfaces/inter2d1/inrch2.F
Chd|        LOCAL_INDEX                   source/interfaces/interf1/local_index.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I3STI2(
     1 X      ,IRECT   ,STF   ,IXQ     ,PM     ,
     2 NRT    ,STFN    ,NSEG  ,LNSV    ,NINT   ,
     3 NSN    ,NSV    ,SLSFAC ,NOINT   ,IPM    ,
     4 ID     ,TITR   ,AREAS  ,KNOD2ELQ,NOD2ELQ,
     5 NTY    ,NSNS   ,NSVS   ,SEGQUADFR)
      USE MESSAGE_MOD
C-------------------------------------------------------
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
#include      "com04_c.inc"
#include      "nchar_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRT, NINT, NSN, NOINT, NTY, NSNS, IPM(NPROPMI,*)
C     REAL ou REAL*8
      my_real
     .   SLSFAC
      INTEGER IRECT(4,*), IXQ(7,*), NSEG(*), LNSV(*), NSV(*),
     .        KNOD2ELQ(*),NOD2ELQ(*), NSVS(*), SEGQUADFR(2,*)
C     REAL ou REAL*8
      my_real
     .   X(3,*), STF(*), PM(NPROPM,*), STFN(*),AREAS(*)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, NEL, MT, J, NUM, NPT, JJ, LL, IG, IL, IE, INRT,
     .   N1 ,N2 ,STAT, ILINE, LIN, L, N, K
      INTEGER LINES(2,4)
      INTEGER, DIMENSION(:),ALLOCATABLE ::INRTIE
C     REAL ou REAL*8
      my_real
     .   AREA, XL2, YM1, YM2, ZM1, ZM2,YE(4) ,ZE(4),
     .   Y1 ,Y2 ,Z1 ,Z2
      DATA LINES/1,2,
     .           2,3,
     .           3,4,
     .           4,1/
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C
       ALLOCATE(INRTIE(NUMELQ),STAT=stat)
       IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='INRTIE')
        INRTIE(1:NUMELQ)=0
C
      DO I=1,NRT
        II=I
        CALL INRCH2(X,IRECT,IXQ,II,NEL,
     .              NINT,NOINT, YM1, YM2, ZM1,
     .              ZM2 ,YE   ,ZE,ID,TITR)
        IF(NEL.NE.0) THEN
         INRTIE(NEL) = II
         CALL INARE2(AREA,YE   ,ZE)
         XL2=(YM2-YM1)**2+(ZM2-ZM1)**2
         MT=IXQ(1,NEL)
         IF(MT.GT.0)THEN
          STF(I)=SLSFAC*XL2*PM(32,MT)/AREA
         ELSE
          STF(I)=ZERO
          CALL ANCMSG(MSGID=347,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_2,
     .                I1=ID,
     .                C1=TITR,
     .                I2=IPM(1,MT),
     .                I3=NEL,
     .                I4=I)
C
         ENDIF
        ELSE
          STF(I)=ZERO
        ENDIF
C
        CALL INORI2(X,IRECT,II,NEL,NINT,
     .              NOINT, YM1, YM2, ZM1,ZM2 ,
     .              YE   ,ZE)
      ENDDO
C---------------------------------------------
C     CALCUL DES RIGIDITES NODALES
C---------------------------------------------
      DO J=1,NSN
        NUM=NSEG(J+1)-NSEG(J)
        NPT=NSEG(J)-1
        DO JJ=1,NUM
          LL=LNSV(NPT+JJ)
          STFN(J)=STFN(J) + HALF*STF(LL)
        ENDDO
      ENDDO	  

C---------------------------------------------
C     SECND NODAL SURFACE COMPUTATION
C---------------------------------------------
C
      IF(NTY == 3) THEN
       DO I = 1,NSN    
         AREAS(I) = ZERO
         DO J= KNOD2ELQ(NSV(I))+1,KNOD2ELQ(NSV(I)+1)
            IE = NOD2ELQ(J)
            INRT = INRTIE(IE)
            IF(INRT.NE.0)THEN
               N1=IRECT(1,INRT)
               N2=IRECT(2,INRT)
               Y1=X(2,N1)
               Z1=X(3,N1)
               Y2=X(2,N2)
               Z2=X(3,N2)
c
               AREA = SQRT((Y2-Y1)*(Y2-Y1)+(Z2-Z1)*(Z2-Z1))
               AREA = AREA*HALF
c
              AREAS(I) = AREAS(I) + AREA
           ENDIF
          ENDDO
       ENDDO
      ELSEIF(NTY == 5) THEN
       DO I = 1,NSNS   
         AREAS(I) = ZERO
          DO J= KNOD2ELQ(NSVS(I))+1,KNOD2ELQ(NSVS(I)+1)
            IE = NOD2ELQ(J)
            DO L=1,4 
              IF(IXQ(LINES(1,L)+1,IE) ==NSVS(I)) THEN
                 LIN = L
                 EXIT
              ENDIF   
            ENDDO

           DO K=1,NSEGQUADFR
              N  =SEGQUADFR(1,K)
              ILINE=SEGQUADFR(2,K)
              
              IF(N==IE.AND.ILINE==LIN) THEN

                N1=IXQ(LINES(1,ILINE)+1,N)
                N2=IXQ(LINES(2,ILINE)+1,N)

               Y1=X(2,N1)
               Z1=X(3,N1)
               Y2=X(2,N2)
               Z2=X(3,N2)

              AREA = SQRT((Y2-Y1)*(Y2-Y1)+(Z2-Z1)*(Z2-Z1))
              AREA = AREA*HALF

              AREAS(I) = AREAS(I) + AREA
             ENDIF
           ENDDO
          ENDDO
        ENDDO
      ENDIF
      DEALLOCATE(INRTIE)

C-----------------------------------------------------
C     MISE DANS IRECT DU NUMERO LOCAL DU NOEUD
C-----------------------------------------------------
      DO I=1,NRT
        IG=IRECT(1,I)
        CALL LOCAL_INDEX(IL,IG,NSV,NSN)
        IRECT(1,I)=IL
        IG=IRECT(2,I)
        CALL LOCAL_INDEX(IL,IG,NSV,NSN)
        IRECT(2,I)=IL
      ENDDO
C
      RETURN
      END
