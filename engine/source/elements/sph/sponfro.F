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
Chd|  SPONFRO                       source/elements/sph/sponfro.F 
Chd|-- called by -----------
Chd|        FORINTP                       source/elements/forintp.F     
Chd|-- calls ---------------
Chd|        SPHBOX                        share/modules/sphbox.F        
Chd|====================================================================
      SUBROUTINE SPONFRO(X ,V        ,A       ,MS      ,
     2             SPBUF   ,ITAB    ,KXSP    ,IXSP    ,NOD2SP  ,
     3             ISPHIO  ,IPART   ,IPARTSP ,WASPACT ,WA_EPSD ,
     4             VNORMAL, WAR2 )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SPHBOX
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "sphcom.inc"
#include      "scr01_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "task_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXSP(NISP,*),IXSP(KVOISPH,*),NOD2SP(*),ITAB(*),
     .        ISPHIO(NISPHIO,*),IPART(LIPART1,*),IPARTSP(*),WASPACT(*)
      my_real
     .   X(3,*) ,V(3,*) ,A(3,*) ,MS(*) ,SPBUF(NSPBUF,*) ,
     .   WA_EPSD(KWASPH,*),VNORMAL(3,*),WAR2(9,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NS ,N ,IMPOSE ,INOD ,M ,JNOD ,JMPOSE ,
     .        IPPV ,NP, K, NN, JMPOSE2
      my_real
     .       XI,YI,ZI,XJ,YJ,ZJ,DMIN,DD,
     .       DXX,DXY,DXZ,DYX,DYY,DYZ,DZX,DZY,DZZ,
     .       DIVV
      LOGICAL lBOOL
C-----------------------------------------------
C     general outlets & silent boundary.
C-------------------------------------------
      DO NS=1,NSPHACT
       N=WASPACT(NS)
       IMPOSE=KXSP(2,N)/(NGROUP+1)
       lBOOL=.FALSE.
       IF(IMPOSE /= 0)THEN
         IF(ISPHIO(1,IMPOSE).EQ.2.OR.ISPHIO(1,IMPOSE).EQ.3)lBOOL=.TRUE.
       ENDIF
       IF(lBOOL)THEN
          INOD=KXSP(3,N)
          XI=X(1,INOD)
          YI=X(2,INOD)
          ZI=X(3,INOD)
C-------
C         plus proche voisin en amont de l'outlet.
          IPPV=0
          DMIN=1.E+20

          DO  K=1,KXSP(4,N)

           JNOD=IXSP(K,N)
           IF(JNOD.GT.0)THEN          ! particule locale
             M=NOD2SP(JNOD)
             JMPOSE=KXSP(2,M)/(NGROUP+1)
             lBOOL=.FALSE.
             IF(JMPOSE == 0)THEN
               lBOOL=.TRUE.
             ELSE
               IF(ISPHIO(1,JMPOSE) == 1)lBOOL=.TRUE.
             ENDIF
             IF(lBOOL)THEN
               XJ  =X(1,JNOD)
               YJ  =X(2,JNOD)
               ZJ  =X(3,JNOD)
               DD  =(XI-XJ)*(XI-XJ)+(YI-YJ)*(YI-YJ)+(ZI-ZJ)*(ZI-ZJ)
               IF(DD.LT.DMIN)THEN
                IPPV=JNOD
                DMIN=DD
               ENDIF
             ENDIF
           ELSE
             NN = -JNOD
             JMPOSE = NINT(XSPHR(12,NN))
             IF(JMPOSE.GT.0)THEN
               JMPOSE2=ISPHIO(1,JMPOSE)
             ELSE
               JMPOSE2=0
             ENDIF
             IF(JMPOSE2.EQ.0.OR.JMPOSE2.EQ.1)THEN
               XJ  =XSPHR(3,NN)
               YJ  =XSPHR(4,NN)
               ZJ  =XSPHR(5,NN)
               DD  =(XI-XJ)*(XI-XJ)+(YI-YJ)*(YI-YJ)+(ZI-ZJ)*(ZI-ZJ)
               IF(DD.LT.DMIN)THEN
                IPPV=JNOD
                DMIN=DD
               ENDIF
             ENDIF  
           ENDIF
          ENDDO
C-------
C         d(rho)/dt=- rho * div(V) utilise div(V) calcule en IPPV.
          IF(IPPV.GT.0)THEN
            NP=NOD2SP(IPPV)
            DXX =WA_EPSD(1,NP)
            DYY =WA_EPSD(2,NP)
            DZZ =WA_EPSD(3,NP)
            DXY =WA_EPSD(4,NP)
            DYZ =WA_EPSD(5,NP)
            DXZ =WA_EPSD(6,NP)
            DYX =WA_EPSD(7,NP)
            DZY =WA_EPSD(8,NP)
            DZX =WA_EPSD(9,NP)
          ELSE
c remote neigbourg, get values from SPMD_SPHGETWA
            DXX =WAR2(1,-IPPV)
            DYY =WAR2(2,-IPPV)
            DZZ =WAR2(3,-IPPV)
            DXY =WAR2(4,-IPPV)
            DYZ =WAR2(5,-IPPV)
            DXZ =WAR2(6,-IPPV)
            DYX =WAR2(7,-IPPV)
            DZY =WAR2(8,-IPPV)
            DZX =WAR2(9,-IPPV)
          ENDIF

          DIVV=DXX+DYY+DZZ
          SPBUF(2,N)=WA_EPSD(10,N)*(ONE - DIVV*DT1)
          WA_EPSD(1,N) =DXX
          WA_EPSD(2,N) =DYY
          WA_EPSD(3,N) =DZZ
          WA_EPSD(4,N) =DXY
          WA_EPSD(5,N) =DYZ
          WA_EPSD(6,N) =DXZ
          WA_EPSD(7,N) =DYX
          WA_EPSD(8,N) =DZY
          WA_EPSD(9,N) =DZX
          WA_EPSD(13,N)=DIVV
       ENDIF
      ENDDO
C-------------------------------------------
      RETURN
      END
