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
Chd|  I21ELS3                       source/interfaces/inter3d1/i21els3.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        INCOQ3                        source/interfaces/inter3d1/incoq3.F
Chd|        INELTC                        source/interfaces/inter3d1/inelt.F
Chd|        INELTS                        source/interfaces/inter3d1/inelt.F
Chd|        INSOL3                        source/interfaces/inter3d1/insol3.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I21ELS3(
     1 X        ,IRECTS   ,IRECTM    ,NRTS    ,NRTM  ,
     2 GEO      ,IXS      ,PM        ,IXC     ,IXTG  ,
     3 NINT     ,NTY      ,NOINT     ,NSN     ,NSV   ,
     4 IELES    ,INTTH    ,AREAS     ,NMN     ,MSR   ,
     5 KNOD2ELS ,KNOD2ELC ,KNOD2ELTG ,NOD2ELS ,NOD2ELC  ,
     6 NOD2ELTG ,IGRSURFS ,IGRSURFM  ,IELEM21 ,
     7 THK      ,AS        ,BS       ,IXS10   ,IXS16    ,
     8 IXS20    ,ID        ,TITR     ,IGEO    ,SH4TREE  ,
     9 SH3TREE  ,IPART    ,IPARTC    ,IPARTTG ,PM_STACK , 
     A IWORKSH )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
      USE MESSAGE_MOD
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
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr08_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "remesh_c.inc"
#include      "rnur_c.inc"

C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRTS, NRTM, NINT, NTY, NOINT, NSN, NMN
      INTEGER IRECTS(4,*), IRECTM(4,*), IXS(NIXS,*), IXC(NIXC,*),
     .   NSV(*), IXTG(NIXTG,*), 
     .   KNOD2ELS(*), KNOD2ELC(*), KNOD2ELTG(*), NOD2ELS(*), NOD2ELC(*), 
     .   NOD2ELTG(*),
     .   INTTH, IELES(*), MSR(*), IELEM21(*), IXS10(*),
     .   IXS16(*), IXS20(*),IGEO(*),SH3TREE(KSH3TREE,*), SH4TREE(KSH4TREE,*),
     .   IPART(LIPART1,*),IPARTC(*),IPARTTG(*),IWORKSH(*)
C     REAL ou REAL*8
      my_real
     .   X(3,*), PM(NPROPM,*), GEO(NPROPG,*), AREAS(*),THK(*),
     .   AS(*), BS(*),PM_STACK(*)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
      TYPE (SURF_) :: IGRSURFS
      TYPE (SURF_) :: IGRSURFM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, INRT, NELS, NELC, NELTG, IE, II, MAT,N,LLT,L,N1,N2,N3,N4
      INTEGER ITMP(NUMNOD),NLEV, MYLEV,IP,NELEM,STAT
      INTEGER, DIMENSION(:),ALLOCATABLE ::INRTIE
C     REAL ou REAL*8
      my_real
     .   AREA,SX1,SY1,SZ1,SX2,SY2,SZ2,SX3,SY3,SZ3

C-----
      IF(INTTH > 0)THEN
        NELEM = NUMELC+NUMELTG+NUMELS+NUMELR
     +      + NUMELP+NUMELT+NUMELQ+NUMELUR+NUMELX+NUMELIG3D
        ALLOCATE(INRTIE(NELEM),STAT=stat)
        IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='INRTIE')
        INRTIE=0
      END IF
C
C------------------------------------
C     GAP FACES SECONDS
C------------------------------------
      DO 250 I=1,NRTS
      INRT=I
C----------------------
      CALL INELTS(X            ,IRECTS,IXS  ,NINT,NELS          ,
     .            INRT         ,AREA  ,NOINT,0   ,IGRSURFS%ELTYP,
     .            IGRSURFS%ELEM)
      IF(NELS.NE.0)THEN
        IELES(I)=NELS
        IF(INTTH > 0) INRTIE(NELS) = INRT
        GO TO 250
      ELSE
        CALL INELTC(NELC ,NELTG ,INRT ,IGRSURFS%ELTYP, IGRSURFS%ELEM)
        IF(NELTG.NE.0) THEN
          IELES(I)=NELTG+NUMELS+NUMELC 
          GO TO 250
        ELSEIF(NELC.NE.0) THEN
          IELES(I)=NELC+NUMELS
          GO TO 250
        END IF
      END IF
C----------------------
C     ELEMENTS SOLIDES
C----------------------
      CALL INSOL3(X,IRECTS,IXS,NINT,NELS,INRT,
     .            AREA,NOINT,KNOD2ELS ,NOD2ELS ,0 ,IXS10,
     .            IXS16,IXS20)
      IF(NELS/=0) IELES(I)=NELS
C---------------------
C     ELEMENTS COQUES
C---------------------
      CALL INCOQ3(IRECTS,IXC ,IXTG ,NINT ,NELC     ,
     .            NELTG,INRT,GEO  ,PM   ,KNOD2ELC ,
     .            KNOD2ELTG ,NOD2ELC ,NOD2ELTG,THK,NTY,IGEO ,
     .            PM_STACK , IWORKSH )
      IF(NELTG.NE.0) THEN
        IELES(I)=NELTG
      ELSEIF(NELC.NE.0) THEN
        IELES(I)=NELC
      ENDIF
C
      IF(NELS+NELC+NELTG.EQ.0)THEN
       IF (IMACH.NE.3) THEN
         IF(NINT.GT.0) THEN
            CALL ANCMSG(MSGID=92,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=I)
         ENDIF
         IF(NINT.LT.0) THEN
            CALL ANCMSG(MSGID=93,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=I)
         ENDIF
       ELSE
Ca verifier (second)      en SPMD il faut un element associe a l'arrete sinon erreur
         IF(NINT.GT.0) THEN
            CALL ANCMSG(MSGID=481,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=I)
         ENDIF
         IF(NINT.LT.0) THEN
            CALL ANCMSG(MSGID=482,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=I)
         ENDIF
       ENDIF
      ENDIF
  250 CONTINUE
C------------------------------------
C     TRAITEMENTS FACES MAINS (Cherche elt pour SPMD)
C------------------------------------
      DO 500 I=1,NRTM
      INRT=I
C----------------------
      CALL INELTS(X            ,IRECTM,IXS  ,NINT,NELS          ,
     .            INRT         ,AREA  ,NOINT,0   ,IGRSURFM%ELTYP,
     .            IGRSURFM%ELEM)
      IF(NELS.NE.0)THEN
        IELEM21(NELS)=1
        GO TO 500
      ELSE
        CALL INELTC(NELC ,NELTG ,INRT ,IGRSURFM%ELTYP, IGRSURFM%ELEM)
        IF(NELTG.NE.0) THEN
          IELEM21(NUMELS+NUMELQ+NUMELC+NUMELT
     .                     +NUMELP+NUMELR+NELTG)=1
          GO TO 500
        ELSEIF(NELC.NE.0) THEN
          IELEM21(NUMELS+NUMELQ+NELC)=1
          GO TO 500
        END IF
      END IF
C----------------------
C     ELEMENTS SOLIDES
C----------------------
      CALL INSOL3(X,IRECTM,IXS,NINT,NELS,INRT,
     .            AREA,NOINT,KNOD2ELS ,NOD2ELS ,0 ,IXS10,
     .            IXS16,IXS20)
C---------------------
C     ELEMENTS COQUES
C---------------------
      CALL INCOQ3(IRECTM,IXC ,IXTG ,NINT ,NELC     ,
     .            NELTG,INRT,GEO  ,PM   ,KNOD2ELC ,
     .            KNOD2ELTG ,NOD2ELC ,NOD2ELTG,THK,NTY,IGEO,
     .            PM_STACK , IWORKSH )
C
      IF(NELS+NELC+NELTG.EQ.0)THEN
       IF (IMACH.NE.3) THEN
         IF(NINT.GT.0) THEN
            CALL ANCMSG(MSGID=92,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=I)
         ENDIF
         IF(NINT.LT.0) THEN
            CALL ANCMSG(MSGID=93,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=I)
         ENDIF
       ELSE
C        en SPMD il faut un element associe a l'arrete sinon erreur
         IF(NINT.GT.0) THEN
            CALL ANCMSG(MSGID=481,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=I)
         ENDIF
         IF(NINT.LT.0) THEN
            CALL ANCMSG(MSGID=482,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=I)
         ENDIF
       ENDIF
      ELSE
        IF(NELS.NE.0)  IELEM21(NELS)=1
        IF(NELTG.NE.0) IELEM21(NUMELS+NUMELQ+NUMELC+NUMELT
     .                     +NUMELP+NUMELR+NELTG)=1
        IF(NELC.NE.0)  IELEM21(NUMELS+NUMELQ+NELC)=1
      END IF
  500 CONTINUE
C---------------------------
C     remplace dans IRECTM les n°s de noeuds par leur n° dans MSR.
C---------------------------
      ITMP=0
      DO I=1,NMN
        II=MSR(I)
        ITMP(II)=I
      END DO
C
      DO I=1,NRTM
        DO J=1,4
          IRECTM(J,I)=ITMP(IRECTM(J,I))
        END DO
      END DO

C--------------------------------------------------------------
C
C calcul de l'aire second. ---
C calcul du surface second. ---
      IF(INTTH > 0 ) THEN
C
       IF(NUMELC.NE.0) THEN
C
        IF(NADMESH==0)THEN
          DO I = 1,NSN    
             AREAS(I) = ZERO
             DO J= KNOD2ELC(NSV(I))+1,KNOD2ELC(NSV(I)+1)
               IE = NOD2ELC(J)
               SX1 = X(1,IXC(4,IE)) - X(1,IXC(2,IE))
               SY1 = X(2,IXC(4,IE)) - X(2,IXC(2,IE))
               SZ1 = X(3,IXC(4,IE)) - X(3,IXC(2,IE))
               SX2 = X(1,IXC(5,IE)) - X(1,IXC(3,IE))
               SY2 = X(2,IXC(5,IE)) - X(2,IXC(3,IE))
               SZ2 = X(3,IXC(5,IE)) - X(3,IXc(3,IE))
               SX3  = SY1*SZ2 - SZ1*SY2
               SY3  = SZ1*SX2 - SX1*SZ2
               SZ3  = SX1*SY2 - SY1*SX2
               AREA = ONE_OVER_8*SQRT(SX3*SX3+SY3*SY3+SZ3*SZ3)
               AREAS(I) = AREAS(I) + AREA
C overwrite
               MAT  = IXC(1,IE)
               AS(I)= AS(I)+PM(75,MAT)*AREA
               BS(I)= BS(I)+PM(76,MAT)*AREA
             END DO
C
             DO J= KNOD2ELTG(NSV(I))+1,KNOD2ELTG(NSV(I)+1)
               IE = NOD2ELTG(J)
               SX1 = X(1,IXTG(3,IE)) - X(1,IXTG(2,IE))
               SY1 = X(2,IXTG(3,IE)) - X(2,IXTG(2,IE))
               SZ1 = X(3,IXTG(3,IE)) - X(3,IXTG(2,IE))
               SX2 = X(1,IXTG(4,IE)) - X(1,IXTG(2,IE))
               SY2 = X(2,IXTG(4,IE)) - X(2,IXTG(2,IE))
               SZ2 = X(3,IXTG(4,IE)) - X(3,IXTG(2,IE))
               SX3  = SY1*SZ2 - SZ1*SY2
               SY3  = SZ1*SX2 - SX1*SZ2
               SZ3  = SX1*SY2 - SY1*SX2
               AREA = ONE_OVER_6*SQRT(SX3*SX3+SY3*SY3+SZ3*SZ3)
               AREAS(I) = AREAS(I)+AREA
C overwrite
               MAT  = IXTG(1,IE)
               AS(I)= AS(I)+PM(75,MAT)*AREA
               BS(I)= BS(I)+PM(76,MAT)*AREA
             END DO
             AS(I)=AS(I)/MAX(EM20,AREAS(I))
             BS(I)=BS(I)/MAX(EM20,AREAS(I))
          END DO
        ELSE
          DO I = 1,NSN    
             AREAS(I) = ZERO
             DO J= KNOD2ELC(NSV(I))+1,KNOD2ELC(NSV(I)+1)
               IE = NOD2ELC(J)

               IP = IPARTC(IE)
               NLEV =IPART(10,IP)
               MYLEV=SH4TREE(3,IE)
               IF(MYLEV < 0) MYLEV=-(MYLEV+1)

               IF(MYLEV==NLEV)THEN                 
                 SX1 = X(1,IXC(4,IE)) - X(1,IXC(2,IE))
                 SY1 = X(2,IXC(4,IE)) - X(2,IXC(2,IE))
                 SZ1 = X(3,IXC(4,IE)) - X(3,IXC(2,IE))
                 SX2 = X(1,IXC(5,IE)) - X(1,IXC(3,IE))
                 SY2 = X(2,IXC(5,IE)) - X(2,IXC(3,IE))
                 SZ2 = X(3,IXC(5,IE)) - X(3,IXC(3,IE))
                 SX3  = SY1*SZ2 - SZ1*SY2
                 SY3  = SZ1*SX2 - SX1*SZ2
                 SZ3  = SX1*SY2 - SY1*SX2
                 AREA = ONE_OVER_8*SQRT(SX3*SX3+SY3*SY3+SZ3*SZ3)
                 AREAS(I) = AREAS(I) + AREA
C overwrite
C
                 MAT  = IXC(1,IE)
                 AS(I)= AS(I)+PM(75,MAT)*AREA
                 BS(I)= BS(I)+PM(76,MAT)*AREA
               ENDIF
             END DO
C
            DO J= KNOD2ELTG(NSV(I))+1,KNOD2ELTG(NSV(I)+1)
               IE = NOD2ELTG(J)

               IP = IPARTTG(IE)
               NLEV =IPART(10,IP)
               MYLEV=SH3TREE(3,IE)
               IF(MYLEV < 0) MYLEV=-(MYLEV+1)

               IF(MYLEV==NLEV)THEN                 
                 SX1 = X(1,IXTG(3,IE)) - X(1,IXTG(2,IE))
                 SY1 = X(2,IXTG(3,IE)) - X(2,IXTG(2,IE))
                 SZ1 = X(3,IXTG(3,IE)) - X(3,IXTG(2,IE))
                 SX2 = X(1,IXTG(4,IE)) - X(1,IXTG(2,IE))
                 SY2 = X(2,IXTG(4,IE)) - X(2,IXTG(2,IE))
                 SZ2 = X(3,IXTG(4,IE)) - X(3,IXTG(2,IE))
                 SX3  = SY1*SZ2 - SZ1*SY2
                 SY3  = SZ1*SX2 - SX1*SZ2
                 SZ3  = SX1*SY2 - SY1*SX2
                 AREA = ONE_OVER_6*SQRT(SX3*SX3+SY3*SY3+SZ3*SZ3)
                 AREAS(I) = AREAS(I)+AREA
C overwrite
C
                  MAT  = IXTG(1,IE)
                  AS(I)= AS(I)+PM(75,MAT)*AREA
                  BS(I)= BS(I)+PM(76,MAT)*AREA
               END IF

             END DO
             AS(I)=AS(I)/MAX(EM20,AREAS(I))
             BS(I)=BS(I)/MAX(EM20,AREAS(I))

          END DO
        END IF
      ENDIF
C     
      IF(NUMELS.NE.0)THEN
          DO I = 1,NSN    
             AREAS(I) = ZERO
             DO J= KNOD2ELS(NSV(I))+1,KNOD2ELS(NSV(I)+1)
               IE = NOD2ELS(J)
               INRT = INRTIE(IE)
               IF(INRT.NE.0)THEN
                 N1=IRECTS(1,INRT)
                 N2=IRECTS(2,INRT)
                 N3=IRECTS(3,INRT)
                 N4=IRECTS(4,INRT)

                 SX1 = X(1,N3) - X(1,N1)
                 SY1 = X(2,N3) - X(2,N1)
                 SZ1 = X(3,N3) - X(3,N1)
                 SX2 = X(1,N4) - X(1,N2)
                 SY2 = X(2,N4) - X(2,N2)
                 SZ2 = X(3,N4) - X(3,N2)
                 SX3  = SY1*SZ2 - SZ1*SY2
                 SY3  = SZ1*SX2 - SX1*SZ2
                 SZ3  = SX1*SY2 - SY1*SX2
                 AREA = ONE_OVER_8*SQRT(SX3*SX3+SY3*SY3+SZ3*SZ3)
                 AREAS(I) = AREAS(I) + AREA
C overwrite
                 MAT  = IXS(1,IE)
                 AS(I)= AS(I)+PM(75,MAT)*AREA
                 BS(I)= BS(I)+PM(76,MAT)*AREA
              ENDIF
            END DO
             AS(I)=AS(I)/MAX(EM20,AREAS(I))
             BS(I)=BS(I)/MAX(EM20,AREAS(I))
          ENDDO
        ENDIF

      ENDIF

C---------------------------
      RETURN
      END
