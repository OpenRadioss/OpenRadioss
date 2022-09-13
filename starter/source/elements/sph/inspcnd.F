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
C  093 + conditions aux limites SPH, modifie les masses.
Chd|====================================================================
Chd|  INSPCND                       source/elements/sph/inspcnd.F 
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        GET_U_GEO                     source/user_interface/uaccess.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE INSPCND(ISPCOND ,IGRNOD  ,KXSP    ,IXSP    ,
     .                   NOD2SP  ,ITAB    ,ICODE   ,ISKEW   ,ISKN    ,
     .                   SKEW    ,XFRAME  ,X       ,ISPSYM  ,ISPTAG  ,
     .                   PM      ,GEO     ,IPART   ,IPARTSP )
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
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "sphcom.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "scr03_c.inc"
#include      "titr_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISPCOND(NISPCOND,*),
     .        KXSP(NISP,*),IXSP(KVOISPH,*),NOD2SP(*),
     .        ITAB(*),ICODE(*),ISKEW(*),ISKN(LISKN,*),
     .        ISPSYM(NSPCOND,*),ISPTAG(*),IPART(LIPART1,*),IPARTSP(*)
C     REAL
      my_real
     .        SKEW(LSKEW,*),XFRAME(NXFRAME,*),X(3,*),
     .        PM(NPROPM,*),GEO(NPROPG,*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IGRS,J,INOD,N,
     .        K,K2,K3,IFR,NC,ICSP,ISK,IC1,IC2,IC,J6(6),
     .        ISLIDE,IMAT,IPROP,IPRT
C     REAL
      my_real
     .        TX,TY,TZ,UX,UY,UZ,VX,VY,VZ,WX,WY,WZ,NW,NT,PS,
     .        DD,OX,OY,OZ,XI,YI,ZI,DI,MP,RHO,VOL,
     .        UNTIERS
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER MY_OR
      my_real
     .         GET_U_GEO
      EXTERNAL GET_U_GEO
C-----------------------------------------------
        DO I=1,NUMSPH
         ISPTAG(I)=1
        ENDDO
C-----------------------------------------------
C       creation des skews (prepare l'addition des conditions cinematiques).
        DO I=1,NUMSPH
         J =NUMSKW+1+I
         INOD=KXSP(3,I)
C        SKEWS were built when LECSKW.
C        ISKN(4,J)=-1
         IF (ICODE(INOD)/=0) THEN
             ISK=ISKEW(INOD)
             DO K=1,LSKEW
              SKEW(K,J)=SKEW(K,ISK)
             ENDDO
             ISKN(1,J)=ISKN(1,ISK)
             ISKN(2,J)=ISKN(2,ISK)
             ISKN(3,J)=ISKN(3,ISK)
             ISKEW(INOD)=J
         ELSE
C             DO K=1,9
C              SKEW(K,J)=0.
C             ENDDO
C             SKEW(1,J)=1.
C             SKEW(5,J)=1.
C             SKEW(9,J)=1.
C             ISKN(1,J)=0
C             ISKN(2,J)=0
C             ISKN(3,J)=0
             ISKEW(INOD)=J
             ICODE(INOD)=0
         ENDIF
        ENDDO
C-----------------------------------------------
        WRITE(IOUT,*) ' SPH SYMMETRY CONDITIONS INITIALIZATION :'
        WRITE(IOUT,*) ' --------------------------------------  '
        UNTIERS=1./3.
        DO I=1,NSPCOND
          IGRS=ISPCOND(4,I)
          IFR =ISPCOND(3,I)
          ICSP=ISPCOND(2,I)
          ISLIDE=ISPCOND(5,I)
        
          UX=XFRAME(3*(ICSP-1)+1,IFR)
          UY=XFRAME(3*(ICSP-1)+2,IFR)
          UZ=XFRAME(3*(ICSP-1)+3,IFR)
C---------
C         prepares particles to be symetrized with respect to the condition.
          OX=XFRAME(10,IFR)
          OY=XFRAME(11,IFR)
          OZ=XFRAME(12,IFR)
          DO N=1,NUMSPH
             INOD =KXSP(3,N)
             XI =X(1,INOD)
             YI =X(2,INOD)
             ZI =X(3,INOD)
             DD=(XI-OX)*UX+(YI-OY)*UY+(ZI-OZ)*UZ
             IPRT  =IPARTSP(N)
             IMAT  =IPART(1,IPRT)
             RHO   =PM(1,IMAT)
             IPROP =IPART(2,IPRT)
             MP    =GET_U_GEO(1,IPROP)
             VOL   =MP/RHO
             DI    =GET_U_GEO(6,IPROP)
C            Default=Characteristic length of hexagonal compact net:
             IF(DI==0.) DI=(SQRT(2.)*VOL)**UNTIERS
             IF (DD<EM3*DI) THEN
                ISPTAG(N)=ISPTAG(N)+1
             ENDIF
          ENDDO
C---------
C        boundary conditions addition on nodes group.
          IF(IGRS/=0)THEN
            DO 888 J=1,IGRNOD(IGRS)%NENTITY
                INOD=IGRNOD(IGRS)%ENTITY(J)
                IC =ICODE(INOD)
                ISK=ISKEW(INOD)
                IF(IC/=0.AND.ISKN(1,ISK)/=0) THEN
C                  WRITE(IOUT,*)
C     .' ** ERROR INCOMPATIBLE KINEMATIC CONDITIONS :',
C     .' SPH KINEMATIC CONDITION + BOUNDARY CONDITION',
C     .' INTO A MOVING SKEW IS NOT ALLOWED (NODE ID=',ITAB(INOD),').'
C                  IERR=IERR+1
                   CALL ANCMSG(MSGID=394,
     .                         MSGTYPE=MSGERROR,
     .                         ANMODE=ANINFO_BLIND_1,
     .                         I1=ITAB(INOD))
                  GOTO 888
                ENDIF
C---------------
                IC1=IC/512
                IC2=(IC-512*IC1)/64
                J6(1)=IC1/4
                J6(2)=(IC1-4*J6(1))/2
                J6(3)=(IC1-4*J6(1)-2*J6(2))
                J6(4)=IC2/4
                J6(5)=(IC2-4*J6(4))/2
                J6(6)=(IC2-4*J6(4)-2*J6(5))
                NC=0
                DO K=1,3
                 IF(J6(K)/=0) NC=NC+1
                ENDDO
                IF(NC==3) THEN
C---------------
                   CALL ANCMSG(MSGID=395,
     .                         MSGTYPE=MSGWARNING,
     .                         ANMODE=ANINFO_BLIND_2,
     .                         I1=ITAB(INOD))
                ELSEIF(NC==2)THEN
C---------------
C               2 conditions deja realisees + 1 condition :
                 IF(J6(1)==1)THEN
                  K=1
                  IF(J6(2)==1)THEN
                   K2=4
                   K3=7
                  ELSE
                   K2=7
                   K3=4
                  ENDIF
                 ELSEIF(J6(2)==1)THEN
                  K =4
                  K2=7
                  K3=1
                 ENDIF
                 WX=SKEW(K3,ISK)
                 WY=SKEW(K3+1,ISK)
                 WZ=SKEW(K3+2,ISK)
                 PS=UX*WX+UY*WY+UZ*WZ
                 IF(ABS(PS)<EM20) THEN
                    CALL ANCMSG(MSGID=396,
     .                          MSGTYPE=MSGWARNING,
     .                          ANMODE=ANINFO_BLIND_2,
     .                          I1=ITAB(INOD))
                  GOTO 888
                 ENDIF
                 J6(1)=0
                 J6(2)=0
                 J6(3)=0
                 IF(K3==1)J6(1)=1
                 IF(K3==4)J6(2)=1
                 IF(K3==7)J6(3)=1
                 IC1=J6(1)*4+J6(2)*2+J6(3)
                 IC2=0
                 IC=IC1*512+IC2*64
                 ICODE(INOD)=MY_OR(IC,ICODE(INOD))
               ELSEIF(NC==1)THEN
C---------------
                 K=7*J6(3)+4*J6(2)+J6(1)
                 VX=SKEW(K,ISK)
                 VY=SKEW(K+1,ISK)
                 VZ=SKEW(K+2,ISK)
                 WX=VY*UZ-VZ*UY
                 WY=VZ*UX-VX*UZ
                 WZ=VX*UY-VY*UX
                 NW=SQRT(WX*WX+WY*WY+WZ*WZ)
                 IF(NW<EM20) THEN
                    CALL ANCMSG(MSGID=397,
     .                          MSGTYPE=MSGWARNING,
     .                          ANMODE=ANINFO_BLIND_2)
                  GOTO 888
                 ENDIF
                 WX=WX/MAX(NW,EM20)
                 WY=WY/MAX(NW,EM20)
                 WZ=WZ/MAX(NW,EM20)
                 TX=WY*VZ-WZ*VY
                 TY=WZ*VX-WX*VZ
                 TZ=WX*VY-WY*VX
                 NT=SQRT(TX*TX+TY*TY+TZ*TZ)
                 TX=TX/MAX(NT,EM20)
                 TY=TY/MAX(NT,EM20)
                 TZ=TZ/MAX(NT,EM20)
                 K2=K+3
                 IF(K2>9)K2=1
                 K3=K2+3
                 IF(K3>9)K3=1
                 SKEW(K2,ISK)  =TX
                 SKEW(K2+1,ISK)=TY
                 SKEW(K2+2,ISK)=TZ
                 SKEW(K3,ISK)  =WX
                 SKEW(K3+1,ISK)=WY
                 SKEW(K3+2,ISK)=WZ
                 J6(1)=0
                 J6(2)=0
                 J6(3)=0
                 IF(K2==1)J6(1)=1
                 IF(K2==4)J6(2)=1
                 IF(K2==7)J6(3)=1
                 IC1=J6(1)*4+J6(2)*2+J6(3)
                 IC2=0
                 IC=IC1*512+IC2*64
                 ICODE(INOD)=MY_OR(IC,ICODE(INOD))
               ELSEIF(NC==0)THEN
                 DO K=1,9
                  SKEW(K,ISK)=XFRAME(K,IFR)
                 ENDDO
                 IF(ISLIDE==0)THEN
                  IC1=7
                 ELSE
                  IC1=2**(3-ICSP)
                 ENDIF
                 IC2=0
                 IC=IC1*512+IC2*64
                 ICODE(INOD)=MY_OR(IC,ICODE(INOD))
               ENDIF
 888        CONTINUE
          ENDIF
        ENDDO
C-----------------------------------------------
      RETURN
 999  CALL FREERR(1)
C-------------------------------------

      RETURN
      END
