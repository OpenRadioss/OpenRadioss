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
Chd|  LECINV                        source/input/lecinv.F         
Chd|-- called by -----------
Chd|        LECTUR                        source/input/lectur.F         
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        NGR2USR                       source/input/freform.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE LECINV(NINIV ,X, V ,VR  ,ITAB  ,IFRAME ,XFRAME, IGRNOD )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
#include      "sphcom.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C----------------------------------------------- 
      INTEGER NINIV,ITAB(*),IFRAME(LISKN,*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD) :: IGRNOD
C     REAL
      my_real
     .   X(3,*),V(3,*), VR(3,*), XFRAME(NXFRAME,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,JJ,K,N,N1,N2,K1,K2,K3,NGR2USR,II
      INTEGER IDIR,CPT0,CPT1,IFM,IFRA,IGN,CPT20,CPT21
C     REAL
      my_real
     .   VV  ,VTX ,VTY ,VTZ , NIXJ(6),
     .   VX  ,VY  ,VZ
      EXTERNAL NGR2USR
C-----------------------------------------------
C
      IF(IMACH/=3.OR.ISPMD==0) THEN
        WRITE(IOUT,'(//,A,/)')' VELOCITY REINITIALISATION'
C
      ENDIF
C
      CPT0=0
      CPT1=0
      CPT20=0
      CPT21=0
C
      DO I = 1,NINIV
       READ (IIN,'(3I10,F20.0)') N1,N2,IDIR,VV
       IF (IDIR < 0) THEN
         READ (IIN,'(3F20.0,I10)') VTX,VTY,VTZ,IFRA
       ENDIF
C--------case /INIV/ * /2
       IF (N1 < 0) THEN
         IGN=NGR2USR(-N1,IGRNOD,NGRNOD)
C-----   
         IF (IGN==0) THEN         
            CALL ANCMSG(MSGID=292,ANMODE=ANINFO,I1=-N1)
            CALL ARRET(2)
         END IF
       ENDIF
C
       IF (IDIR < 0) THEN
         IF(IMACH/=3.OR.ISPMD==0) THEN
           IF (N1 < 0) THEN
             IF (CPT20==0) WRITE(IOUT,3000)
             WRITE(IOUT,3100) -N1,VV,VTX,VTY,VTZ,IFRA
             CPT20=CPT20+1
             CPT21=0
           ELSE
             IF (CPT0==0) WRITE(IOUT,1000)
             WRITE(IOUT,1100) N1,N2,VV,VTX,VTY,VTZ,IFRA
             CPT0=CPT0+1
             CPT1=0
           END IF
         ENDIF
C
         K1=-3*IDIR-2
         K2=-3*IDIR-1
         K3=-3*IDIR
         IF (N1 < 0) THEN
          DO II=1,IGRNOD(IGN)%NENTITY
           N=IGRNOD(IGN)%ENTITY(II)
C
             NIXJ = ZERO
             IF (IFRA > 0) THEN
               DO K=1,NUMFRAM
                 J=K+1
                 IF(IFRA==IFRAME(4,K+1)) THEN
                 VX = XFRAME(1,J)*VTX+XFRAME(4,J)*VTY+XFRAME(7,J)*VTZ
                 VY = XFRAME(2,J)*VTX+XFRAME(5,J)*VTY+XFRAME(8,J)*VTZ
                 VZ = XFRAME(3,J)*VTX+XFRAME(6,J)*VTY+XFRAME(9,J)*VTZ
                 GO TO 200
                 ENDIF
               ENDDO
               CALL ANCMSG(MSGID=222,ANMODE=ANINFO)
               CALL ARRET(2)
200            CONTINUE
               NIXJ(1)=XFRAME(K1,J)*(X(2,N)-XFRAME(11,J))
               NIXJ(2)=XFRAME(K2,J)*(X(1,N)-XFRAME(10,J))
               NIXJ(3)=XFRAME(K2,J)*(X(3,N)-XFRAME(12,J))
               NIXJ(4)=XFRAME(K3,J)*(X(2,N)-XFRAME(11,J))
               NIXJ(5)=XFRAME(K3,J)*(X(1,N)-XFRAME(10,J))
               NIXJ(6)=XFRAME(K1,J)*(X(3,N)-XFRAME(12,J))
               IF (IRODDL>0) THEN
                 VR(1,N)= VV*XFRAME(K1,J)
                 VR(2,N)= VV*XFRAME(K2,J)
                 VR(3,N)= VV*XFRAME(K3,J)
               END IF
             ELSE
               IF(-IDIR==1) THEN
                 NIXJ(1)=X(2,N)
                 NIXJ(6)=X(3,N)
               ELSEIF(-IDIR==2) THEN
                 NIXJ(2)=X(1,N)
                 NIXJ(3)=X(3,N)
               ELSEIF(-IDIR==3) THEN
                 NIXJ(4)=X(2,N)
                 NIXJ(5)=X(1,N)
               ENDIF
               VX=VTX
               VY=VTY
               VZ=VTZ
               IF (IRODDL>0) THEN
                 IF (IDIR==-1) VR(1,N)= VV
                 IF (IDIR==-2) VR(2,N)= VV
                 IF (IDIR==-3) VR(3,N)= VV
               END IF
             ENDIF
             V(1,N)= VX+VV*(NIXJ(3)-NIXJ(4))
             V(2,N)= VY+VV*(NIXJ(5)-NIXJ(6))
             V(3,N)= VZ+VV*(NIXJ(1)-NIXJ(2))
          ENDDO
         ELSE
          DO N = 1,NUMNOD
           IF(ITAB(N)>=N1.AND.ITAB(N)<=N2) THEN
C
             NIXJ = ZERO
             IF (IFRA > 0) THEN
               DO K=1,NUMFRAM
                 J=K+1
                 IF(IFRA==IFRAME(4,K+1)) THEN
                 VX = XFRAME(1,J)*VTX+XFRAME(4,J)*VTY+XFRAME(7,J)*VTZ
                 VY = XFRAME(2,J)*VTX+XFRAME(5,J)*VTY+XFRAME(8,J)*VTZ
                 VZ = XFRAME(3,J)*VTX+XFRAME(6,J)*VTY+XFRAME(9,J)*VTZ
                 GO TO 100
                 ENDIF
               ENDDO
               CALL ANCMSG(MSGID=222,ANMODE=ANINFO)
               CALL ARRET(2)
100            CONTINUE
               NIXJ(1)=XFRAME(K1,J)*(X(2,N)-XFRAME(11,J))
               NIXJ(2)=XFRAME(K2,J)*(X(1,N)-XFRAME(10,J))
               NIXJ(3)=XFRAME(K2,J)*(X(3,N)-XFRAME(12,J))
               NIXJ(4)=XFRAME(K3,J)*(X(2,N)-XFRAME(11,J))
               NIXJ(5)=XFRAME(K3,J)*(X(1,N)-XFRAME(10,J))
               NIXJ(6)=XFRAME(K1,J)*(X(3,N)-XFRAME(12,J))
               IF (IRODDL>0) THEN
                 VR(1,N)= VV*XFRAME(K1,J)
                 VR(2,N)= VV*XFRAME(K2,J)
                 VR(3,N)= VV*XFRAME(K3,J)
               END IF
             ELSE
               IF(-IDIR==1) THEN
                 NIXJ(1)=X(2,N)
                 NIXJ(6)=X(3,N)
               ELSEIF(-IDIR==2) THEN
                 NIXJ(2)=X(1,N)
                 NIXJ(3)=X(3,N)
               ELSEIF(-IDIR==3) THEN
                 NIXJ(4)=X(2,N)
                 NIXJ(5)=X(1,N)
               ENDIF
               VX=VTX
               VY=VTY
               VZ=VTZ
               IF (IRODDL>0) THEN
                 IF (IDIR==-1) VR(1,N)= VV
                 IF (IDIR==-2) VR(2,N)= VV
                 IF (IDIR==-3) VR(3,N)= VV
               END IF
             ENDIF
             V(1,N)= VX+VV*(NIXJ(3)-NIXJ(4))
             V(2,N)= VY+VV*(NIXJ(5)-NIXJ(6))
             V(3,N)= VZ+VV*(NIXJ(1)-NIXJ(2))
           ENDIF
          ENDDO
         END IF !(N1 < 0) THEN
       ELSE
C
        IF (N1 < 0) THEN
         IF(ISPMD==0) THEN
           IF (CPT21==0) WRITE(IOUT,4000)
           WRITE(IOUT,4100)-N1,IDIR,VV
           CPT21=CPT21+1
           CPT20=0
         ENDIF
C
         IF(IDIR<=3)THEN
           DO II=1,IGRNOD(IGN)%NENTITY
             N=IGRNOD(IGN)%ENTITY(II)
             V(IDIR,N) = VV
           ENDDO
         ELSE
           DO II=1,IGRNOD(IGN)%NENTITY
             N=IGRNOD(IGN)%ENTITY(II)
             VR(IDIR-3,N) = VV
           ENDDO
         ENDIF
        ELSE
         IF(IMACH/=3.OR.ISPMD==0) THEN
           IF (CPT1==0) WRITE(IOUT,2000)
           WRITE(IOUT,2100)N1,N2,IDIR,VV
           CPT1=CPT1+1
           CPT0=0
         ENDIF
C
         IF(IDIR<=3)THEN
           DO N = 1,NUMNOD
            IF(ITAB(N)>=N1.AND.ITAB(N)<=N2)V(IDIR,N) = VV
           ENDDO
         ELSE
           DO N = 1,NUMNOD
            IF(ITAB(N)>=N1.AND.ITAB(N)<=N2)VR(IDIR-3,N) = VV
           ENDDO
         ENDIF
        END IF !(N1 < 0) THEN
       ENDIF
      ENDDO
C----------------
C     FORMATS
C----------------
 1000   FORMAT(3X,'FIRST-N',4X,'LAST-N',10X,'ROTATION',8X,
     +       'TRANSL X',8X,'TRANSL Y',8X,'TRANSL Z',3X,'FRAME_ID')
 1100   FORMAT(I10,I10,2X,1PE16.9,1PE16.9,1PE16.9,1PE16.9,I10)

 2000   FORMAT(3X,'FIRST-N',4X,'LAST-N',3X,'DIRECT.',10X,'VELOCITY')
 2100   FORMAT(I10,I10,I10,2X,1PE16.9)
 3000   FORMAT(3X,'GRN_id ',20X,'ROTATION',8X,
     +       'TRANSL X',8X,'TRANSL Y',8X,'TRANSL Z',3X,'FRAME_ID')
 3100   FORMAT(I10,12X,1PE16.9,1PE16.9,1PE16.9,1PE16.9,I10)

 4000   FORMAT(3X,'GRN_id',13X,'DIRECT.',10X,'VELOCITY')
 4100   FORMAT(I10,I10,12X,1PE16.9)
C
      RETURN
      END
