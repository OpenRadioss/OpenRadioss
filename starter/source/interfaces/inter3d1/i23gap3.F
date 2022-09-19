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
Chd|  I23GAP3                       source/interfaces/inter3d1/i23gap3.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        I4GMX3                        source/interfaces/inter3d1/i4gmx3.F
Chd|        INCOQ3                        source/interfaces/inter3d1/incoq3.F
Chd|        INSOL3                        source/interfaces/inter3d1/insol3.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I23GAP3(
     1 X     ,IRECTS ,IRECTM ,NRTS  ,NRTM  ,
     2 GEO      ,IXS      ,PM        ,IXC     ,IXTG  ,
     3 NINT  ,NTY   ,NOINT   ,NSN   ,NSV   ,
     4 INTTH    ,NMN       ,MSR     ,WA    ,
     5 KNOD2ELS ,KNOD2ELC ,KNOD2ELTG ,NOD2ELS ,NOD2ELC  ,
     6 NOD2ELTG ,THK      ,IXS10     ,IXS16   ,IXS20    ,
     7 IPARTC   ,IPARTTG  ,GAP       ,IGAP    ,GAP_S    ,
     8 GAPMIN   ,GAPINF   ,GAPMAX    ,GAPSCALE,BGAPSMX  ,
     9 STFN     ,STF      ,ID,TITR   ,GAP_M   ,IGEO     ,
     A PM_STACK,IWORKSH )
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRTS, NRTM, NINT, NTY, NOINT, NSN, NMN, IGAP
      INTEGER IRECTS(4,*), IRECTM(4,*), IXS(NIXS,*), IXC(NIXC,*),
     .   NSV(*), IXTG(NIXTG,*),
     .   KNOD2ELS(*), KNOD2ELC(*), KNOD2ELTG(*), NOD2ELS(*), NOD2ELC(*),
     .   NOD2ELTG(*),
     .   INTTH, MSR(*), IXS10(*),
     .   IXS16(*), IXS20(*), IPARTC(*), IPARTTG(*),IGEO(NPROPGI,*),
     .   IWORKSH(*)
C     REAL
      my_real
     .   GAP, GAPMIN, GAPINF, GAPMAX, GAPSCALE, BGAPSMX,
     .   X(3,*), PM(NPROPM,*), GEO(NPROPG,*), THK(*), WA(*),
     .   GAP_S(*), STFN(*), STF(*), GAP_M(*),PM_STACK(*)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, INRT, NELS, NELC, NELTG, IE, II, MAT, IP, MG, NDX,
     .        IGTYP
C     REAL
      my_real
     .   DXM, GAPMX, GAPMN, AREA, DX, GAPM
C--------------------------------------------------------------
      DXM=ZERO
      NDX=0
      GAPMX=EP30
      GAPMN=EP30
C------------------------------------
C     GAP VARIABLE NOEUDS SECONDS
C------------------------------------
      IF(IGAP>=1)THEN
        DO I=1,NUMNOD
         WA(I)=ZERO
        ENDDO
      END IF
C-----
      DO 250 I=1,NRTS
      INRT=I
C----------------------
C     ELEMENTS SOLIDES
C----------------------
      CALL INSOL3(X,IRECTS,IXS,NINT,NELS,INRT,
     .            AREA,NOINT,KNOD2ELS ,NOD2ELS ,0 ,IXS10,
     .            IXS16,IXS20)
C---------------------
C     ELEMENTS COQUES
C---------------------
      CALL INCOQ3(IRECTS,IXC ,IXTG ,NINT ,NELC     ,
     .            NELTG,INRT,GEO  ,PM   ,KNOD2ELC ,
     .            KNOD2ELTG ,NOD2ELC ,NOD2ELTG,THK,NTY,IGEO,
     .            PM_STACK  , IWORKSH )
      IF(NELTG/=0) THEN
        IF(IGAP>=1)THEN
          MG=IXTG(5,NELTG)
          IGTYP = IGEO(11,MG)
	  IP = IPARTTG(NELTG)
          DX=HALF*GEO(1,MG)
          IF(IGTYP == 17) DX = HALF*THK(NUMELC + NELTG)
          WA(IXTG(2,NELTG))=MAX(WA(IXTG(2,NELTG)),DX)
          WA(IXTG(3,NELTG))=MAX(WA(IXTG(3,NELTG)),DX)
          WA(IXTG(4,NELTG))=MAX(WA(IXTG(4,NELTG)),DX)
        END IF
      ELSEIF(NELC/=0) THEN
        IF(IGAP>=1)THEN
          MG=IXC(6,NELC)
          IGTYP = IGEO(11,MG)
          IP = IPARTC(NELC)
          DX=HALF*GEO(1,MG)
          IF(IGTYP == 17) DX = HALF*THK(NELC)
          WA(IXC(2,NELC))=MAX(WA(IXC(2,NELC)),DX)
          WA(IXC(3,NELC))=MAX(WA(IXC(3,NELC)),DX)
          WA(IXC(4,NELC))=MAX(WA(IXC(4,NELC)),DX)
          WA(IXC(5,NELC))=MAX(WA(IXC(5,NELC)),DX)
        END IF
      ENDIF
C
      IF(NELS+NELC+NELTG==0)THEN
Ca verifier (second)      en SPMD il faut un element associe a l'arrete sinon erreur
         IF(NINT>0) THEN
            CALL ANCMSG(MSGID=481,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_2,
     .               I1=ID,
     .               C1=TITR,
     .               I2=I)
         ENDIF
         IF(NINT<0) THEN
            CALL ANCMSG(MSGID=482,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_2,
     .               I1=ID,
     .               C1=TITR,
     .               I2=I)
         ENDIF
      ENDIF
  250 CONTINUE
C-----
      IF(IGAP>=1)THEN
       DO I=1,NSN
        GAPM=GAPSCALE * WA(NSV(I))
        GAP_S(I)= GAPM
       ENDDO
      ENDIF
C------------------------------------
C     GAP FACES MAINS
C------------------------------------
      DO 350 I=1,NRTM
      INRT=I
      GAPM=ZERO
      CALL I4GMX3(X,IRECTM,INRT,GAPMX)
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
     .            PM_STACK  , IWORKSH )
      IF(NELTG/=0) THEN
c        IF(IGAP>=1)THEN
          MG=IXTG(5,NELTG)
          IGTYP =IGEO(11,MG)
          IP = IPARTTG(NELTG)
          DX =GEO(1,MG)*GAPSCALE
          IF(IGTYP == 17) DX =THK(NUMELC+NELTG)*GAPSCALE
c        END IF
      ELSEIF(NELC/=0) THEN
c        IF(IGAP>=1)THEN
          MG=IXC(6,NELC)
          IGTYP =IGEO(11,MG)
          IP = IPARTC(NELC)
          DX =GEO(1,MG)*GAPSCALE
          IF(IGTYP == 17) DX =THK(NELC)*GAPSCALE
c        END IF
      ENDIF
      GAPM=HALF*DX
      GAPMN = MIN(GAPMN,HALF*DX)
      DXM=DXM+DX
      NDX=NDX+1
      IF(IGAP/=0) GAP_M(I)=GAPM
C
      IF(NELS+NELC+NELTG==0)THEN
Ca verifier (second)      en SPMD il faut un element associe a l'arrete sinon erreur
         IF(NINT>0) THEN
            CALL ANCMSG(MSGID=481,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_2,
     .               I1=ID,
     .               C1=TITR,
     .               I2=I)
         ENDIF
         IF(NINT<0) THEN
            CALL ANCMSG(MSGID=482,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_2,
     .               I1=ID,
     .               C1=TITR,
     .               I2=I)
         ENDIF
      ENDIF
  350 CONTINUE
C------------------------------------
C     GAP 
C------------------------------------
       GAPMX=SQRT(GAPMX)
       IF(IGAP==0)THEN
C GAP FIXE
         IF(GAP<=ZERO)THEN
           IF(NDX/=0)THEN
             GAP = DXM/NDX
             GAP = MIN(HALF*GAPMX,GAP)
           ELSE
             GAP = EM01 * GAPMX
           ENDIF
           WRITE(IOUT,1000)GAP
         ENDIF
         GAPMIN = GAP
         GAPMAX = GAP
       ELSE
C GAP VARIABLE :
C    - GAPMIN CONTIENT ONE GAP MINIMUM UTILISE SI GAP_S(I)+GAP_M(J) < GAPMIN
C    - GAP CONTIENT LE SUP DE (GAP_S(I)+GAP_M(J),GAPMIN) 
         IF(GAP<=ZERO)THEN
           IF(NDX/=0)THEN
             GAPMIN = GAPMN
             GAPMIN = MIN(HALF*GAPMX,GAPMIN)
           ELSE
             GAPMIN = EM01 * GAPMX
           ENDIF
         ELSE
           GAPMIN=GAP
         END IF
         WRITE(IOUT,1000)GAPMIN
C
C        GAP n'est pas utilise pour Igap > 0 ; Gapmin peut etre egal a 0.
         IF(GAPMAX==ZERO)GAPMAX=EP30
         WRITE(IOUT,1500)GAPMAX
         GAP = MIN(GAP,GAPMAX)
       ENDIF
C------------------------------------
C
C Calcul du gap reel a utiliser lors du critere de retri
C
      BGAPSMX = ZERO
      IF (IGAP==0) THEN
        GAPINF=GAP
      ELSE
        GAPINF=EP30
        DO I = 1, NSN
          GAPINF = MIN(GAPINF,GAP_S(I))
          BGAPSMX = MAX(BGAPSMX,GAP_S(I))
        ENDDO
        DO I = 1, NRTM
          GAPINF = MIN(GAPINF,GAP_M(I))
        ENDDO
        GAPINF=MAX(GAPINF,GAPMIN)
      ENDIF
C---------------------------------------------
C     STiff cote main (1: active ; 0: inactive)
C------------------------------------
      DO I=1,NRTM
        STF(I)=ONE
      END DO
C---------------------------------------------
C     MISE A ONE DU MULTIPLICATEUR NODALE DES RIGIDITES 
C---------------------------------------------
      DO I=1,NSN
        STFN(I) = ONE
      END DO
C---------------------------
      RETURN
 1000 FORMAT(2X,'GAP MIN = ',1PG20.13)
 1500 FORMAT(2X,'GAP MAX = ',1PG20.13)
      END
