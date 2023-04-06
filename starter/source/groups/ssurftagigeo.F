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
Chd|  SSURFTAGIGEO                  source/groups/ssurftagigeo.F  
Chd|-- called by -----------
Chd|        HM_READ_SURF                  source/groups/hm_read_surf.F  
Chd|-- calls ---------------
Chd|        SSURFIGEO                     source/groups/ssurftagigeo.F  
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE SSURFTAGIGEO(IXIG3D,IPARTIG3D,KXIG3D, 
     .                        TAGBUF,NSEG   ,
     .                        IEXT  ,FLAG     ,IFRE  ,KEY    ,
     .                        NSEGIGE,KNOT    ,IGEO  ,WIGE   ,
     .                        X,V,KNOD2ELIG3D,NOD2ELIG3D       ,
     .                        NIGE,RIGE,XIGE,VIGE,IADTABIGE,DECALIGEO,
     .                        IGRSURF,KNOTLOCPC,KNOTLOCEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER TAGBUF(*),
     .        KXIG3D(NIXIG3D,*),IPARTIG3D(*),IXIG3D(*),
     .        KNOD2ELIG3D(*),NOD2ELIG3D(*),NIGE(*)
      INTEGER IEXT,NSEG,FLAG,IFRE,NSEGIGE,IGEO(NPROPGI,*),IADTABIGE,DECALIGEO
      CHARACTER KEY*ncharkey
      my_real
     .   X(3,*),V(3,*),WIGE(*),KNOT(*),RIGE(3,*),XIGE(3,*),VIGE(3,*),
     .   KNOTLOCPC(*),KNOTLOCEL(2,3,*)
!
      TYPE (SURF_) :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,J,K,JS,KS,II,JJ,K1,K2,LL,NN,KK,I1,
     .        NI(4),NS(4),MI(4),MI2(4),MS(4),NMIN,MMIN,
     .        MF,NF,IPERM,N1,N2,I,ISEG
      INTEGER NODTAG(NUMNOD),FACEIGEO(4,6),PWR(7),CROSSFACEIGEO(4,6),
     .        NNS,NCTRL,IPID,PX,PY,PZ,
     .        FASTAGIGEO(NUMELIG3D) 
      my_real
     .   KNOTLOCELX(2),KNOTLOCELY(2),KNOTLOCELZ(2)
      DATA PWR/1,2,4,8,16,32,64/
C=======================================================================
      FASTAGIGEO=0
C
      IF(IEXT==1)THEN

        MF=4
        NF=4
C
C       External surface only.
        DO JS=1,NUMELIG3D

          IF(KEY(1:6)=='GRBRIC')THEN
            IF (TAGBUF(JS)==0) CYCLE          !case of tagged elems
          ELSE
            IF (TAGBUF(IPARTIG3D(JS))==0) CYCLE  !case of tagged parts
          END IF

          IPID = KXIG3D(2,JS)
          PX = IGEO(41,IPID)-1
          PY = IGEO(42,IPID)-1
          PZ = IGEO(43,IPID)-1

          FACEIGEO(1,1) = (PX+1)*(PY+1)*PZ+PX+1
          FACEIGEO(2,1) = (PX+1)*(PY+1)*PZ+1
          FACEIGEO(3,1) = (PX+1)*(PY+1)*PZ+(PX+1)*PY+1
          FACEIGEO(4,1) = (PX+1)*(PY+1)*(PZ+1)

          FACEIGEO(1,2) = (PX+1)*(PY+1)
          FACEIGEO(2,2) = (PX+1)*PY+1
          FACEIGEO(3,2) = 1
          FACEIGEO(4,2) = PX+1

          FACEIGEO(1,3) = (PX+1)*(PY+1)*(PZ+1)
          FACEIGEO(2,3) = (PX+1)*(PY+1)*PZ+(PX+1)*PY+1
          FACEIGEO(3,3) = (PX+1)*PY+1
          FACEIGEO(4,3) = (PX+1)*(PY+1)

          FACEIGEO(1,4) = (PX+1)*(PY+1)*PZ+1
          FACEIGEO(2,4) = (PX+1)*(PY+1)*PZ+PX+1
          FACEIGEO(3,4) = PX+1
          FACEIGEO(4,4) = 1

          FACEIGEO(1,5) = (PX+1)*(PY+1)*PZ+(PX+1)*PY+1
          FACEIGEO(2,5) = (PX+1)*(PY+1)*PZ+1
          FACEIGEO(3,5) = 1
          FACEIGEO(4,5) = (PX+1)*PY+1

          FACEIGEO(1,6) = (PX+1)*(PY+1)*(PZ+1)
          FACEIGEO(2,6) = (PX+1)*(PY+1)
          FACEIGEO(3,6) = PX+1
          FACEIGEO(4,6) = (PX+1)*(PY+1)*PZ+PX+1

          CROSSFACEIGEO=0
C
          IF(PX==2) THEN
            CROSSFACEIGEO(1,5) = (PX+1)*(PY+1)*PZ+(PX+1)*PY+PX   ! milieu - devant
            CROSSFACEIGEO(2,5) = (PX+1)*(PY+1)*PZ+PX
            CROSSFACEIGEO(3,5) = PX
            CROSSFACEIGEO(4,5) = (PX+1)*PY+PX

            CROSSFACEIGEO(1,6) = (PX+1)*(PY+1)*PZ+(PX+1)*PY+PX   ! milieu - derriere
            CROSSFACEIGEO(2,6) = (PX+1)*PY+PX
            CROSSFACEIGEO(3,6) = PX
            CROSSFACEIGEO(4,6) = (PX+1)*(PY+1)*PZ+PX
          ENDIF

          IF(PY==2) THEN
            CROSSFACEIGEO(1,3) = (PX+1)*(PY+1)*PZ+(PX+1)*PY    ! milieu - gauche
            CROSSFACEIGEO(2,3) = (PX+1)*(PY+1)*PZ+(PX+1)+1 
            CROSSFACEIGEO(3,3) = (PX+1)+1
            CROSSFACEIGEO(4,3) = (PX+1)*PY

            CROSSFACEIGEO(1,4) = (PX+1)*(PY+1)*PZ+(PX+1)+1     ! milieu - droit
            CROSSFACEIGEO(2,4) = (PX+1)*(PY+1)*PZ+(PX+1)*PY
            CROSSFACEIGEO(3,4) = (PX+1)*PY
            CROSSFACEIGEO(4,4) = (PX+1)+1
          ENDIF

          IF(PZ==2) THEN
            CROSSFACEIGEO(1,1) = (PX+1)*(PY+1)+PX+1    ! milieu - bas
            CROSSFACEIGEO(2,1) = (PX+1)*(PY+1)+1
            CROSSFACEIGEO(3,1) = (PX+1)*(PY+1)+(PX+1)*PY+1 
            CROSSFACEIGEO(4,1) = (PX+1)*(PY+1)*PZ

            CROSSFACEIGEO(1,2) = (PX+1)*(PY+1)*PZ    ! milieu - haut
            CROSSFACEIGEO(2,2) = (PX+1)*(PY+1)+(PX+1)*PY+1 
            CROSSFACEIGEO(3,2) = (PX+1)*(PY+1)+1
            CROSSFACEIGEO(4,2) = (PX+1)*(PY+1)+PX+1
          ENDIF

CC positionnement des faces crees ici verifiees : ok 

          DO JJ=1,6
            DO II=1,4 ! on va chercher les 4 coins de chaque face
              NS(II)=IXIG3D(KXIG3D(4,JS)+FACEIGEO(II,JJ)-1) 
            END DO
C
C           permute
            NMIN=NS(1)
            DO II=2,NF
              NMIN=MIN(NMIN,NS(II))
            END DO
            DO IPERM=1,NF
              IF(NMIN==NS(IPERM).AND.
     .           NS(MOD(IPERM,NF)+1)/=NS(IPERM))THEN
                DO II=1,NF
                  NI(II)=NS(MOD(II+IPERM-2,NF)+1)
                END DO
                EXIT
              END IF
            END DO
C
C           looks for an elt sharing the face.
            DO K=KNOD2ELIG3D(NI(1))+1,KNOD2ELIG3D(NI(1)+1)
              KS=NOD2ELIG3D(K)
              IF(KS==JS)CYCLE
              IF (KEY(1:6)=='GRBRIC'.AND.TAGBUF(KS)==0.AND.IFRE==0)CYCLE ! if IFRE=0 on cherche la connectivite uniquement avec les elements du marques du groupe (cycle), sinon si IFRE=1 on cherche la connectivite avec tout le monde
              IF (KEY(1:6)/='GRBRIC'.AND.TAGBUF(IPARTIG3D(KS))==0)CYCLE     
              DO II=1,NF
                NODTAG(NI(II))=0
              END DO
              NCTRL = KXIG3D(3,JS)
              DO II=1,NCTRL
                NODTAG(IXIG3D(KXIG3D(4,KS)+II-1))=1
              END DO
              NN=0
              DO II=1,NF
                NN=NN+NODTAG(NI(II))
              END DO
              IF(NN==NF)THEN ! il faut que la face ait 4 points en commun avec uen autre pour eventuellement etre exclue
                DO KK=1,6
            	  DO II=1,4
            	    MS(II)=IXIG3D(KXIG3D(4,KS)+FACEIGEO(II,KK)-1) 
            	  END DO
C
C                 permute
           	  MMIN=MS(1)
           	  DO II=2,MF
           	    MMIN=MIN(MMIN,MS(II))
           	  END DO
            	  DO IPERM=1,MF
            	    IF(MMIN==MS(IPERM).AND.
     .                 MS(MOD(IPERM,MF)+1)/=MS(IPERM))THEN
            	      DO II=1,MF
            	  	    MI(II)=MS(MOD(II+IPERM-2,MF)+1)
            	      END DO
            	      EXIT
            	    END IF
            	  END DO
C
                  IF(CROSSFACEIGEO(1,KK)/=0) THEN
              	    DO II=1,4
              	      MS(II)=IXIG3D(KXIG3D(4,KS)+CROSSFACEIGEO(II,KK)-1) 
            	    END DO
C
C                   permute
           	    MMIN=MS(1)
           	    DO II=2,MF
           	      MMIN=MIN(MMIN,MS(II))
           	    END DO
            	    DO IPERM=1,MF
            	      IF(MMIN==MS(IPERM).AND.
     .                   MS(MOD(IPERM,MF)+1)/=MS(IPERM))THEN
            	        DO II=1,MF
            	          MI2(II)=MS(MOD(II+IPERM-2,MF)+1)
            	        END DO
            	        EXIT
            	      END IF
            	    END DO
                  ENDIF
C
                  IF((MI(1)==NI(1).AND.MI(NF)==NI(2)).OR.
     .               (MI2(1)==NI(1).AND.MI2(NF)==NI(2)))THEN
C                    FACTAGIGEO(JS) moins face jj
                     FASTAGIGEO(JS)=FASTAGIGEO(JS)+PWR(JJ) ! lorsqu'on ajoute PWR(JJ), on exclue la face JJ
                     GO TO 400
                  END IF
                END DO
              END IF
            END DO
 400        CONTINUE
          END DO
        END DO

      END IF

C-----------
      DO J=1,NUMELIG3D
        IF (IABS(TAGBUF(IPARTIG3D(J))) == 1) THEN
          LL=FASTAGIGEO(J)
          IPID = KXIG3D(2,J)
          KNOTLOCELX(1) = KNOTLOCEL(1,1,J)
          KNOTLOCELY(1) = KNOTLOCEL(1,2,J)
          KNOTLOCELZ(1) = KNOTLOCEL(1,3,J)
          KNOTLOCELX(2) = KNOTLOCEL(2,1,J)
          KNOTLOCELY(2) = KNOTLOCEL(2,2,J)
          KNOTLOCELZ(2) = KNOTLOCEL(2,3,J)
          DO JJ=1,6
            IF(MOD(LL,PWR(JJ+1))/PWR(JJ) /= 0)CYCLE
            NCTRL = KXIG3D(3,J)
            PX = IGEO(41,IPID)
            PY = IGEO(42,IPID)
            PZ = IGEO(43,IPID)
!!            IF(FLAG == 0) THEN 
!!            ELSEIF(FLAG == 1)THEN
            IF(FLAG == 1)THEN
              CALL SSURFIGEO(J,101,
     .              KXIG3D,IXIG3D,NCTRL,X,V,WIGE,
     .              KNOT,IGEO,DECALIGEO,JJ,NIGE,RIGE,XIGE,VIGE,IADTABIGE,
     .              IGRSURF,NSEGIGE,PX,PY,PZ,KNOTLOCPC,KNOTLOCELX,
     .              KNOTLOCELY,KNOTLOCELZ)
              DECALIGEO=DECALIGEO+16
            ELSE
              NSEGIGE = NSEGIGE + 9 ! 9 = Anciennement NIGEOCUT
            ENDIF
          ENDDO
        ENDIF
      ENDDO
C-----------
      RETURN
      END

Chd|====================================================================
Chd|  SSURFIGEO                     source/groups/ssurftagigeo.F  
Chd|-- called by -----------
Chd|        SSURFTAGIGEO                  source/groups/ssurftagigeo.F  
Chd|-- calls ---------------
Chd|        IG3DONEBASIS                  source/elements/ige3d/ig3donebasis.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE SSURFIGEO(JS,IELTYP,
     .               KXIG3D,IXIG3D,NCTRL,X,V,WIGE,
     .               KNOT,IGEO,DECALIGEO,JJ,NIGE,RIGE,XIGE,VIGE,IADTABIGE,
     .               IGRSURF,NSEGIGE,PX,PY,PZ,KNOTLOCPC,KNOTLOCELX,
     .               KNOTLOCELY,KNOTLOCELZ)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "ige3d_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JJ,IELTYP,NCTRL,DECALIGEO,
     1      IADPART,IADTABIGE,
     2      KXIG3D(NIXIG3D,*),JS,IXIG3D(*),IGEO(NPROPGI,*),NIGE(*),
     3      NSEGIGE,PX,PY,PZ
      my_real
     .   X(3,*),V(3,*),WIGE(*),KNOT(*),RIGE(3,*),XIGE(3,*),VIGE(3,*),
     .   KNOTLOCPC(DEG_MAX,3,*),KNOTLOCELX(2),KNOTLOCELY(2),KNOTLOCELZ(2)
!
      TYPE (SURF_) :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J,K,L,M,N,IAD_KNOT,N1,N2,N3,
     .        NKNOT1,NKNOT2,NKNOT3,IDX,IDY,IDZ,IPID,ISEG,ITNCTRL,
     .        IDFRSTLOCKNT, IDPC,IDX2,IDY2,IDZ2
      my_real
     .   X_IGEO(NCTRL),Y_IGEO(NCTRL),Z_IGEO(NCTRL),
     .   VX_IGEO(NCTRL),VY_IGEO(NCTRL),VZ_IGEO(NCTRL),
     .   W_IGEO(NCTRL),XI,YI,ZI,ZR,ZS,ZT,
     .   PASX,PASY,PASZ,R(NCTRL),VXI,VYI,VZI,
     .   KNOTLOCX(PX+1,NCTRL),
     .   KNOTLOCY(PY+1,NCTRL),KNOTLOCZ(PZ+1,NCTRL)
C=======================================================================

      DO N=1,3
       DO M=1,3                             
!---
        NSEGIGE = NSEGIGE + 1
        ISEG = NSEGIGE
        IGRSURF%NODES_IGE(ISEG,1) = NUMNOD + DECALIGEO + 1 +(M-1)+4*(N-1)
        IGRSURF%NODES_IGE(ISEG,2) = NUMNOD + DECALIGEO + 1 + M   +4*(N-1)
        IGRSURF%NODES_IGE(ISEG,3) = NUMNOD + DECALIGEO + 1 + M   +4*N
        IGRSURF%NODES_IGE(ISEG,4) = NUMNOD + DECALIGEO + 1 +(M-1)+4*N
!
        IGRSURF%ELTYP_IGE(ISEG) = IELTYP
        IGRSURF%ELEM_IGE(ISEG)  = JS
!---
       ENDDO
      ENDDO          

      DO J=1,NCTRL
        X_IGEO(J)=X(1,IXIG3D(KXIG3D(4,JS)+J-1))
        Y_IGEO(J)=X(2,IXIG3D(KXIG3D(4,JS)+J-1))
        Z_IGEO(J)=X(3,IXIG3D(KXIG3D(4,JS)+J-1))
        VX_IGEO(J)=V(1,IXIG3D(KXIG3D(4,JS)+J-1))
        VY_IGEO(J)=V(2,IXIG3D(KXIG3D(4,JS)+J-1))
        VZ_IGEO(J)=V(3,IXIG3D(KXIG3D(4,JS)+J-1))
        W_IGEO(J)=1!WIGE(IXIG3D(KXIG3D(4,JS)+J-1))
      ENDDO

      IPID=KXIG3D(2,JS)
      IAD_KNOT = IGEO(40,IPID)
      N1 = IGEO(44,IPID)
      N2 = IGEO(45,IPID)
      N3 = IGEO(46,IPID)
      IDFRSTLOCKNT = IGEO(47,IPID)
      NKNOT1 = N1+PX
      NKNOT2 = N2+PY
      NKNOT3 = N3+PZ
      IDX = KXIG3D(6,JS)
      IDY = KXIG3D(7,JS)
      IDZ = KXIG3D(8,JS)
      IDX2 = KXIG3D(9,JS)
      IDY2 = KXIG3D(10,JS)
      IDZ2 = KXIG3D(11,JS)

       DO J=1,NCTRL 
        DO K=1,PX+1
          KNOTLOCX(K,J)=KNOTLOCPC(K,1,(IPID-1)*NUMNOD+IXIG3D(KXIG3D(4,JS)+J-1))
        ENDDO
        DO K=1,PY+1
          KNOTLOCY(K,J)=KNOTLOCPC(K,2,(IPID-1)*NUMNOD+IXIG3D(KXIG3D(4,JS)+J-1))
        ENDDO
        DO K=1,PZ+1
          KNOTLOCZ(K,J)=KNOTLOCPC(K,3,(IPID-1)*NUMNOD+IXIG3D(KXIG3D(4,JS)+J-1))
        ENDDO
       ENDDO

cc      PASX = (KNOT(IAD_KNOT+IDX+1) - KNOT(IAD_KNOT+IDX)) / THREE
cc      PASY = (KNOT(IAD_KNOT+NKNOT1+1+IDY) - KNOT(IAD_KNOT+NKNOT1+IDY)) / THREE
cc      PASZ = (KNOT(IAD_KNOT+NKNOT1+NKNOT2+1+IDZ) - KNOT(IAD_KNOT+NKNOT1+NKNOT2+IDZ)) / THREE
c      PASX = (KNOT(IAD_KNOT+IDX2) - KNOT(IAD_KNOT+IDX)) / THREE
c      PASY = (KNOT(IAD_KNOT+NKNOT1+IDY2) - KNOT(IAD_KNOT+NKNOT1+IDY)) / THREE
c      PASZ = (KNOT(IAD_KNOT+NKNOT1+NKNOT2+IDZ2) - KNOT(IAD_KNOT+NKNOT1+NKNOT2+IDZ)) / THREE
      PASX = (KNOTLOCELX(2) - KNOTLOCELX(1)) / THREE
      PASY = (KNOTLOCELY(2) - KNOTLOCELY(1)) / THREE
      PASZ = (KNOTLOCELZ(2) - KNOTLOCELZ(1)) / THREE

      IF (JJ==1) THEN

      DO M=1,4
       DO L=1,4

         XI = ZERO
         YI = ZERO
         ZI = ZERO
         VXI = ZERO
         VYI = ZERO
         VZI = ZERO

         ZR = KNOTLOCELX(1) + (L-1)*PASX
         ZS = KNOTLOCELY(1) + (M-1)*PASY
         ZT = KNOTLOCELZ(1)

c         CALL IG3DBASIS(
c     1        JS        ,N         ,X_IGEO    ,Y_IGEO  ,  !! N NE VEUT RIEN DIRE ICI
c     2        Z_IGEO    ,W_IGEO    ,IDX       ,IDY     ,
c     3        IDZ       ,R         ,
c     4        NCTRL     ,ZR        ,ZS        ,ZT      ,
c     5        KNOT(IAD_KNOT+1) ,KNOT(IAD_KNOT+NKNOT1+1),
c     6        KNOT(IAD_KNOT+NKNOT1+NKNOT2+1)  ,PX-1    , 
c     7        PY-1      ,PZ-1      ,0)

          CALL IG3DONEBASIS(
     1      JS     ,N        ,X_IGEO  ,Y_IGEO,
     2      Z_IGEO,W_IGEO    ,IDX   ,IDY ,
     3      IDZ ,KNOTLOCX ,KNOTLOCY,KNOTLOCZ,
     4      R          ,NCTRL  ,
     5      ZR  ,ZS      ,ZT   ,KNOT(IAD_KNOT+1),
     6      KNOT(IAD_KNOT+NKNOT1+1),KNOT(IAD_KNOT+NKNOT1+NKNOT2+1),PX-1, 
     7      PY-1   ,PZ-1       ,0        ,
     8      IDX2,IDY2    ,IDZ2   ,
     9      KNOTLOCELX,KNOTLOCELY,KNOTLOCELZ)

         DO J=1,NCTRL
           XI = XI + R(J)*X_IGEO(J)
           YI = YI + R(J)*Y_IGEO(J)
           ZI = ZI + R(J)*Z_IGEO(J)
           VXI = VXI + R(J)*VX_IGEO(J)
           VYI = VYI + R(J)*VY_IGEO(J)
           VZI = VZI + R(J)*VZ_IGEO(J)
         ENDDO

       IADTABIGE=IADTABIGE+1

       NIGE(IADTABIGE)=JS

       RIGE(1,IADTABIGE)=ZR
       RIGE(2,IADTABIGE)=ZS
       RIGE(3,IADTABIGE)=ZT

       XIGE(1,IADTABIGE)=XI
       XIGE(2,IADTABIGE)=YI
       XIGE(3,IADTABIGE)=ZI

       VIGE(1,IADTABIGE)=VXI
       VIGE(2,IADTABIGE)=VYI
       VIGE(3,IADTABIGE)=VZI

       ENDDO
      ENDDO    

      ELSEIF (JJ==2) THEN

      DO M=1,4
       DO L=1,4

         XI = ZERO
         YI = ZERO
         ZI = ZERO
         VXI = ZERO
         VYI = ZERO
         VZI = ZERO

         ZR = KNOTLOCELX(1) + (L-1)*PASX
         ZS = KNOTLOCELY(1) + (M-1)*PASY
         ZT = KNOTLOCELZ(2)

c         CALL IG3DBASIS(
c     1        JS        ,N         ,X_IGEO    ,Y_IGEO  ,
c     2        Z_IGEO    ,W_IGEO    ,IDX       ,IDY     ,
c     3        IDZ       ,R         ,
c     4        NCTRL     ,ZR        ,ZS        ,ZT      ,
c     5        KNOT(IAD_KNOT+1) ,KNOT(IAD_KNOT+NKNOT1+1),
c     6        KNOT(IAD_KNOT+NKNOT1+NKNOT2+1)  ,PX-1    , 
c     7        PY-1      ,PZ-1      ,0)

          CALL IG3DONEBASIS(
     1      JS     ,N        ,X_IGEO  ,Y_IGEO,
     2      Z_IGEO,W_IGEO    ,IDX   ,IDY ,
     3      IDZ ,KNOTLOCX ,KNOTLOCY,KNOTLOCZ,
     4      R          ,NCTRL  ,
     5      ZR  ,ZS      ,ZT   ,KNOT(IAD_KNOT+1),
     6      KNOT(IAD_KNOT+NKNOT1+1),KNOT(IAD_KNOT+NKNOT1+NKNOT2+1),PX-1, 
     7      PY-1   ,PZ-1       ,0        ,
     8      IDX2,IDY2    ,IDZ2   ,
     9      KNOTLOCELX,KNOTLOCELY,KNOTLOCELZ)

         DO J=1,NCTRL
           XI = XI + R(J)*X_IGEO(J)
           YI = YI + R(J)*Y_IGEO(J)
           ZI = ZI + R(J)*Z_IGEO(J)
           VXI = VXI + R(J)*VX_IGEO(J)
           VYI = VYI + R(J)*VY_IGEO(J)
           VZI = VZI + R(J)*VZ_IGEO(J)
         ENDDO
 
       IADTABIGE=IADTABIGE+1

       NIGE(IADTABIGE)=JS

       RIGE(1,IADTABIGE)=ZR
       RIGE(2,IADTABIGE)=ZS
       RIGE(3,IADTABIGE)=ZT

       XIGE(1,IADTABIGE)=XI
       XIGE(2,IADTABIGE)=YI
       XIGE(3,IADTABIGE)=ZI

       VIGE(1,IADTABIGE)=VXI
       VIGE(2,IADTABIGE)=VYI
       VIGE(3,IADTABIGE)=VZI

       ENDDO
      ENDDO   

      ELSEIF (JJ==3) THEN 

      DO N=1,4
       DO L=1,4

         XI = ZERO
         YI = ZERO
         ZI = ZERO
         VXI = ZERO
         VYI = ZERO
         VZI = ZERO

         ZR = KNOTLOCELX(1) + (L-1)*PASX
         ZS = KNOTLOCELY(1)
         ZT = KNOTLOCELZ(1) + (N-1)*PASZ

c         CALL IG3DBASIS(
c     1        JS        ,N         ,X_IGEO    ,Y_IGEO  ,
c     2        Z_IGEO    ,W_IGEO    ,IDX       ,IDY     ,
c     3        IDZ       ,R         ,
c     4        NCTRL     ,ZR        ,ZS        ,ZT      ,
c     5        KNOT(IAD_KNOT+1) ,KNOT(IAD_KNOT+NKNOT1+1),
c     6        KNOT(IAD_KNOT+NKNOT1+NKNOT2+1)  ,PX-1    , 
c     7        PY-1      ,PZ-1      ,0)

          CALL IG3DONEBASIS(
     1      JS     ,N        ,X_IGEO  ,Y_IGEO,
     2      Z_IGEO,W_IGEO    ,IDX   ,IDY ,
     3      IDZ ,KNOTLOCX ,KNOTLOCY,KNOTLOCZ,
     4      R          ,NCTRL  ,
     5      ZR  ,ZS      ,ZT   ,KNOT(IAD_KNOT+1),
     6      KNOT(IAD_KNOT+NKNOT1+1),KNOT(IAD_KNOT+NKNOT1+NKNOT2+1),PX-1, 
     7      PY-1   ,PZ-1       ,0        ,
     8      IDX2,IDY2    ,IDZ2   ,
     9      KNOTLOCELX,KNOTLOCELY,KNOTLOCELZ)

         DO J=1,NCTRL
           XI = XI + R(J)*X_IGEO(J)
           YI = YI + R(J)*Y_IGEO(J)
           ZI = ZI + R(J)*Z_IGEO(J)
           VXI = VXI + R(J)*VX_IGEO(J)
           VYI = VYI + R(J)*VY_IGEO(J)
           VZI = VZI + R(J)*VZ_IGEO(J)
         ENDDO
  
       IADTABIGE=IADTABIGE+1

       NIGE(IADTABIGE)=JS

       RIGE(1,IADTABIGE)=ZR
       RIGE(2,IADTABIGE)=ZS
       RIGE(3,IADTABIGE)=ZT

       XIGE(1,IADTABIGE)=XI
       XIGE(2,IADTABIGE)=YI
       XIGE(3,IADTABIGE)=ZI

       VIGE(1,IADTABIGE)=VXI
       VIGE(2,IADTABIGE)=VYI
       VIGE(3,IADTABIGE)=VZI

       ENDDO
      ENDDO    

      ELSEIF (JJ==4) THEN 

      DO N=1,4
       DO L=1,4

         XI = ZERO
         YI = ZERO
         ZI = ZERO
         VXI = ZERO
         VYI = ZERO
         VZI = ZERO

         ZR = KNOTLOCELX(1) + (L-1)*PASX
         ZS = KNOTLOCELY(2)
         ZT = KNOTLOCELZ(1) + (N-1)*PASZ  

c         CALL IG3DBASIS(
c     1        JS        ,N         ,X_IGEO    ,Y_IGEO  ,
c     2        Z_IGEO    ,W_IGEO    ,IDX       ,IDY     ,
c     3        IDZ       ,R         ,
c     4        NCTRL     ,ZR        ,ZS        ,ZT      ,
c     5        KNOT(IAD_KNOT+1) ,KNOT(IAD_KNOT+NKNOT1+1),
c     6        KNOT(IAD_KNOT+NKNOT1+NKNOT2+1)  ,PX-1    , 
c     7        PY-1      ,PZ-1      ,0)

          CALL IG3DONEBASIS(
     1      JS     ,N        ,X_IGEO  ,Y_IGEO,
     2      Z_IGEO,W_IGEO    ,IDX   ,IDY ,
     3      IDZ ,KNOTLOCX ,KNOTLOCY,KNOTLOCZ,
     4      R          ,NCTRL  ,
     5      ZR  ,ZS      ,ZT   ,KNOT(IAD_KNOT+1),
     6      KNOT(IAD_KNOT+NKNOT1+1),KNOT(IAD_KNOT+NKNOT1+NKNOT2+1),PX-1, 
     7      PY-1   ,PZ-1       ,0        ,
     8      IDX2,IDY2    ,IDZ2   ,
     9      KNOTLOCELX,KNOTLOCELY,KNOTLOCELZ)

         DO J=1,NCTRL
           XI = XI + R(J)*X_IGEO(J)
           YI = YI + R(J)*Y_IGEO(J)
           ZI = ZI + R(J)*Z_IGEO(J)
           VXI = VXI + R(J)*VX_IGEO(J)
           VYI = VYI + R(J)*VY_IGEO(J)
           VZI = VZI + R(J)*VZ_IGEO(J)
         ENDDO
  
       IADTABIGE=IADTABIGE+1

       NIGE(IADTABIGE)=JS

       RIGE(1,IADTABIGE)=ZR
       RIGE(2,IADTABIGE)=ZS
       RIGE(3,IADTABIGE)=ZT

       XIGE(1,IADTABIGE)=XI
       XIGE(2,IADTABIGE)=YI
       XIGE(3,IADTABIGE)=ZI

       VIGE(1,IADTABIGE)=VXI
       VIGE(2,IADTABIGE)=VYI
       VIGE(3,IADTABIGE)=VZI

       ENDDO
      ENDDO    

      ELSEIF (JJ==5) THEN

      DO N=1,4
       DO M=1,4 

         XI = ZERO
         YI = ZERO
         ZI = ZERO
         VXI = ZERO
         VYI = ZERO
         VZI = ZERO

         ZR = KNOTLOCELX(2)
         ZS = KNOTLOCELY(1) + (M-1)*PASY
         ZT = KNOTLOCELZ(1) + (N-1)*PASZ  

c         CALL IG3DBASIS(
c     1        JS        ,N         ,X_IGEO    ,Y_IGEO  ,
c     2        Z_IGEO    ,W_IGEO    ,IDX       ,IDY     ,
c     3        IDZ       ,R         ,
c     4        NCTRL     ,ZR        ,ZS        ,ZT      ,
c     5        KNOT(IAD_KNOT+1) ,KNOT(IAD_KNOT+NKNOT1+1),
c     6        KNOT(IAD_KNOT+NKNOT1+NKNOT2+1)  ,PX-1    , 
c     7        PY-1      ,PZ-1      ,0)

          CALL IG3DONEBASIS(
     1      JS     ,N        ,X_IGEO  ,Y_IGEO,
     2      Z_IGEO,W_IGEO    ,IDX   ,IDY ,
     3      IDZ ,KNOTLOCX ,KNOTLOCY,KNOTLOCZ,
     4      R          ,NCTRL  ,
     5      ZR  ,ZS      ,ZT   ,KNOT(IAD_KNOT+1),
     6      KNOT(IAD_KNOT+NKNOT1+1),KNOT(IAD_KNOT+NKNOT1+NKNOT2+1),PX-1, 
     7      PY-1   ,PZ-1       ,0        ,
     8      IDX2,IDY2    ,IDZ2   ,
     9      KNOTLOCELX,KNOTLOCELY,KNOTLOCELZ)

         DO J=1,NCTRL
           XI = XI + R(J)*X_IGEO(J)
           YI = YI + R(J)*Y_IGEO(J)
           ZI = ZI + R(J)*Z_IGEO(J)
           VXI = VXI + R(J)*VX_IGEO(J)
           VYI = VYI + R(J)*VY_IGEO(J)
           VZI = VZI + R(J)*VZ_IGEO(J)
         ENDDO
  
       IADTABIGE=IADTABIGE+1

       NIGE(IADTABIGE)=JS

       RIGE(1,IADTABIGE)=ZR
       RIGE(2,IADTABIGE)=ZS
       RIGE(3,IADTABIGE)=ZT

       XIGE(1,IADTABIGE)=XI
       XIGE(2,IADTABIGE)=YI
       XIGE(3,IADTABIGE)=ZI

       VIGE(1,IADTABIGE)=VXI
       VIGE(2,IADTABIGE)=VYI
       VIGE(3,IADTABIGE)=VZI

       ENDDO
      ENDDO    

      ELSEIF (JJ==6) THEN 

      DO N=1,4
       DO M=1,4

         XI = ZERO
         YI = ZERO
         ZI = ZERO
         VXI = ZERO
         VYI = ZERO
         VZI = ZERO

         ZR = KNOTLOCELX(1)
         ZS = KNOTLOCELY(1) + (M-1)*PASY
         ZT = KNOTLOCELZ(1) + (N-1)*PASZ  

c         CALL IG3DBASIS(
c     1        JS        ,N         ,X_IGEO    ,Y_IGEO  ,
c     2        Z_IGEO    ,W_IGEO    ,IDX       ,IDY     ,
c     3        IDZ       ,R         ,
c     4        NCTRL     ,ZR        ,ZS        ,ZT      ,
c     5        KNOT(IAD_KNOT+1) ,KNOT(IAD_KNOT+NKNOT1+1),
c     6        KNOT(IAD_KNOT+NKNOT1+NKNOT2+1)  ,PX-1    , 
c     7        PY-1      ,PZ-1      ,0)

          CALL IG3DONEBASIS(
     1      JS     ,N        ,X_IGEO  ,Y_IGEO,
     2      Z_IGEO,W_IGEO    ,IDX   ,IDY ,
     3      IDZ ,KNOTLOCX ,KNOTLOCY,KNOTLOCZ,
     4      R          ,NCTRL  ,
     5      ZR  ,ZS      ,ZT   ,KNOT(IAD_KNOT+1),
     6      KNOT(IAD_KNOT+NKNOT1+1),KNOT(IAD_KNOT+NKNOT1+NKNOT2+1),PX-1, 
     7      PY-1   ,PZ-1       ,0        ,
     8      IDX2,IDY2    ,IDZ2   ,
     9      KNOTLOCELX,KNOTLOCELY,KNOTLOCELZ)

         DO J=1,NCTRL
           XI = XI + R(J)*X_IGEO(J)
           YI = YI + R(J)*Y_IGEO(J)
           ZI = ZI + R(J)*Z_IGEO(J)
           VXI = VXI + R(J)*VX_IGEO(J)
           VYI = VYI + R(J)*VY_IGEO(J)
           VZI = VZI + R(J)*VZ_IGEO(J)
         ENDDO
  
       IADTABIGE=IADTABIGE+1

       NIGE(IADTABIGE)=JS

       RIGE(1,IADTABIGE)=ZR
       RIGE(2,IADTABIGE)=ZS
       RIGE(3,IADTABIGE)=ZT

       XIGE(1,IADTABIGE)=XI
       XIGE(2,IADTABIGE)=YI
       XIGE(3,IADTABIGE)=ZI

       VIGE(1,IADTABIGE)=VXI
       VIGE(2,IADTABIGE)=VYI
       VIGE(3,IADTABIGE)=VZI

       ENDDO
      ENDDO   
 
      ENDIF      
C-----------
      RETURN
      END
C
Chd|====================================================================
Chd|  FICTIVMASSIGEO                source/groups/ssurftagigeo.F  
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE FICTIVMASSIGEO(INTBUF_TAB,NCTRLMAX,MSIG3D,KXIG3D)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  KXIG3D(NIXIG3D,*),NCTRLMAX
      my_real
     .   MSIG3D(NUMELIG3D,*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,I,J,NUMEL,NUMPT
      my_real
     .   MASSELIG3D(NUMELIG3D)
C=======================================================================
C
      DO N=1,NINTER
        INTBUF_TAB(N)%MASSIGE(:)=0
      ENDDO
      MASSELIG3D(:)=0
C
      DO I=1,NUMELIG3D
        DO J=1,NCTRLMAX
          MASSELIG3D(I)=MASSELIG3D(I)+MSIG3D(I,J) !! num locale ici et pas globale I a la place de kxigi3D(I)
        ENDDO
      ENDDO
C
      DO N=1,NINTER  !!! regarder la repartition de la masse sur les points fictifs (masse + importante sur les coins et les arretes)
       DO I=1,INTBUF_TAB(N)%S_NIGE
         NUMEL = INTBUF_TAB(N)%NIGE(I)
         INTBUF_TAB(N)%MASSIGE(I)=INTBUF_TAB(N)%MASSIGE(I)+
     +                                       MASSELIG3D(NUMEL)/(27*4)
       ENDDO
      ENDDO

c      TEST=0
c      DO N=1,NINTER
c        DO I=1,INTBUF_TAB(N)%S_NIGE
c          TEST = TEST + INTBUF_TAB(N)%MASSIGE(I)
c        ENDDO
c      ENDDO
c      PRINT*, 'masse totale', TEST

C-----------
      RETURN
      END
