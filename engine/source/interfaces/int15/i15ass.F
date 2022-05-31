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
Chd|  I15ASS                        source/interfaces/int15/i15ass.F
Chd|-- called by -----------
Chd|        I15CMP                        source/interfaces/int15/i15cmp.F
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE I15ASS(AF    ,X     ,V    ,KSURF ,IGRSURF ,
     2                  BUFSF ,STIFN ,FS   ,FCONT ,FSKYI ,
     3                  ISKY  ,DE    ,WNF  ,WTF   ,WNS   ,
     4                  FNORMX ,FNORMY ,FNORMZ ,FTANGX ,FTANGY ,
     5                  FTANGZ ,NNC    ,KNC    ,H3D_DATA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE H3D_MOD
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr07_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "parit_c.inc"
#include      "param_c.inc"
#include      "scr18_c.inc"
#include      "scr05_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KSURF,ISKY(*),NNC,KNC(*)
C     REAL ou REAL*8
      my_real
     .  AF(*) , X(3,*), V(3,*),BUFSF(*),
     .  STIFN(*), FS(NTHVKI), 
     .  FCONT(3,*),FSKYI(LSKYI,NFSKYI), DE,
     .  WNF(3,*) ,WTF(3,*) ,WNS(*) ,
     .  FNORMX,FNORMY,FNORMZ,FTANGX,FTANGY,FTANGZ
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ADRBUF, I, IN, I3, I2, I1
      INTEGER NFORC , NISKYL
      INTEGER NDEB, NREST
      my_real
     .   ROT(9), XK , YK, ZK, FX, FY, FZ, AM1, AM2, AM3,
     .   FN1, FN2, FN3, FT1, FT2, FT3,
     .   STF, DD
C-----------------------------------------------
      ADRBUF=IGRSURF(KSURF)%IAD_BUFR
      DO I=1,9
       ROT(I)=BUFSF(ADRBUF+7+I-1)
      END DO
C---------------------------------
C     SORTIES dans le rep. global.
C---------------------------------
      FN1=ROT(1)*FNORMX+ROT(4)*FNORMY+ROT(7)*FNORMZ
      FN2=ROT(2)*FNORMX+ROT(5)*FNORMY+ROT(8)*FNORMZ
      FN3=ROT(3)*FNORMX+ROT(6)*FNORMY+ROT(9)*FNORMZ
      FS(1)=FS(1)-FN1*DT1
      FS(2)=FS(2)-FN2*DT1
      FS(3)=FS(3)-FN3*DT1
      FT1=ROT(1)*FTANGX+ROT(4)*FTANGY+ROT(7)*FTANGZ
      FT2=ROT(2)*FTANGX+ROT(5)*FTANGY+ROT(8)*FTANGZ
      FT3=ROT(3)*FTANGX+ROT(6)*FTANGY+ROT(9)*FTANGZ
      FS(4)=FS(4)-FT1*DT1
      FS(5)=FS(5)-FT2*DT1
      FS(6)=FS(6)-FT3*DT1
C------------------------------------------------------------
C     RETOUR EN GLOBAL.
C------------------------------------------------------------
#include "vectorize.inc"
      DO I=1,NNC
      IN=KNC(I)
      FX=WNF(1,IN)+WTF(1,IN)
      FY=WNF(2,IN)+WTF(2,IN)
      FZ=WNF(3,IN)+WTF(3,IN)
      WNF(1,IN)=ROT(1)*FX+ROT(4)*FY+ROT(7)*FZ
      WNF(2,IN)=ROT(2)*FX+ROT(5)*FY+ROT(8)*FZ
      WNF(3,IN)=ROT(3)*FX+ROT(6)*FY+ROT(9)*FZ
      ENDDO
C------------------------------------------------------------
C     ASSEMBLAGE AU NOEUD MAIN DE LA SURFACE.
C------------------------------------------------------------
      DO I=1,NNC
      IN=KNC(I)
      XK=X(1,IN)-BUFSF(ADRBUF+16)
      YK=X(2,IN)-BUFSF(ADRBUF+17)
      ZK=X(3,IN)-BUFSF(ADRBUF+18)
      FX =WNF(1,IN)
      FY =WNF(2,IN)
      FZ =WNF(3,IN)
      AM1=YK*FZ-ZK*FY
      AM2=ZK*FX-XK*FZ
      AM3=XK*FY-YK*FX
C-----
      BUFSF(ADRBUF+25)=BUFSF(ADRBUF+25)-FX
      BUFSF(ADRBUF+26)=BUFSF(ADRBUF+26)-FY
      BUFSF(ADRBUF+27)=BUFSF(ADRBUF+27)-FZ
      BUFSF(ADRBUF+28)=BUFSF(ADRBUF+28)-AM1
      BUFSF(ADRBUF+29)=BUFSF(ADRBUF+29)-AM2
      BUFSF(ADRBUF+30)=BUFSF(ADRBUF+30)-AM3
C-----
      STF=WNS(IN)
      BUFSF(ADRBUF+31)=BUFSF(ADRBUF+31)+STF
      DD = XK**2+YK**2+ZK**2
      BUFSF(ADRBUF+32)=BUFSF(ADRBUF+32)+DD*STF
      ENDDO
C---------------------------------
C     ASSEMBLAGE des FORCES aux NOEUDS SECONDS.
C---------------------------------
      IF (IPARIT.NE.0) THEN
#include "lockon.inc"
        NISKYL = NISKY
        NISKY  = NISKY+NNC
#include "lockoff.inc"
      END IF
      IF (IPARIT.EQ.0) THEN
#include "vectorize.inc"
       DO 300 I=1,NNC
        IN=KNC(I)
        FX=WNF(1,IN)
        FY=WNF(2,IN)
        FZ=WNF(3,IN)
        I3=3*IN
        I2=I3-1
        I1=I2-1
        AF(I1)=AF(I1)+FX
        AF(I2)=AF(I2)+FY
        AF(I3)=AF(I3)+FZ
        STIFN(IN)=STIFN(IN)+WNS(IN)
 300   CONTINUE
      ELSE
       IF(KDTINT.EQ.0)THEN
        DO 350 I=1,NNC
         IN=KNC(I)
         FX=WNF(1,IN)
         FY=WNF(2,IN)
         FZ=WNF(3,IN)
         NISKYL = NISKYL + 1
         FSKYI(NISKYL,1)=FX
         FSKYI(NISKYL,2)=FY
         FSKYI(NISKYL,3)=FZ
         FSKYI(NISKYL,4)=WNS(IN)
         ISKY(NISKYL)   =IN
 350    CONTINUE
       ELSE
        DO I=1,NNC
         IN=KNC(I)
         FX=WNF(1,IN)
         FY=WNF(2,IN)
         FZ=WNF(3,IN)
         NISKYL = NISKYL + 1
         FSKYI(NISKYL,1)=FX
         FSKYI(NISKYL,2)=FY
         FSKYI(NISKYL,3)=FZ
         FSKYI(NISKYL,4)=WNS(IN)
         FSKYI(NISKYL,5)=ZERO
         ISKY(NISKYL)   =IN
        ENDDO
       ENDIF
      ENDIF
C------------------------------------------------------------
C     ANIM (FORCES DE CONTACT).
C------------------------------------------------------------
      IF(ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT.GT.0.AND.
     .    (TT.GE.TANIM.OR.TT.GE.TOUTP.OR.TT.GE.H3D_DATA%TH3D.OR.
     .   (MANIM.GE.4.AND.MANIM.LE.15).OR. H3D_DATA%MH3D .NE. 0))THEN
#include "lockon.inc"
#include "vectorize.inc"
        DO 400 I=1,NNC
         IN=KNC(I)
         FCONT(1,IN) =FCONT(1,IN) + WNF(1,IN)
         FCONT(2,IN) =FCONT(2,IN) + WNF(2,IN)
         FCONT(3,IN) =FCONT(3,IN) + WNF(3,IN)
 400    CONTINUE
#include "lockoff.inc"
      ENDIF
C---------------------------------
C     Pour Travail des forces sur noeuds seconds
C     1ere partie : ici
C     2eme partie : apres calcul de DT2.
C---------------------------------
       DO 450 I=1,NNC
       IN=KNC(I)
         FX=WNF(1,IN)
         FY=WNF(2,IN)
         FZ=WNF(3,IN)
         DE=DE+FX*V(1,IN)+FY*V(2,IN)+FZ*V(3,IN)
 450  CONTINUE
C---------------------------------
C     Travail des forces d'interface avec Madymo.
C---------------------------------
      FS(7)=FS(7)+DE*DT1*HALF
      IF (IGRSURF(KSURF)%TYPE.EQ.100) THEN
C     Sorties Ellipdoides Madymo.
#include "atomic.inc"
          TFEXT=TFEXT+DE*DT1*HALF
#include "atomend.inc"
      ENDIF
C----------------------------------
      RETURN
      END
