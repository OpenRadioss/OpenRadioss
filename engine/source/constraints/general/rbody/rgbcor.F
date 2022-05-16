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
Chd|  RGBCOR                        source/constraints/general/rbody/rgbcor.F
Chd|-- called by -----------
Chd|        RBYCOR                        source/constraints/general/rbody/rbycor.F
Chd|-- calls ---------------
Chd|        IMP_DYNA                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE RGBCOR(V  ,VR  ,X    ,RBY,NOD ,
     2                  NBY,SKEW,ISKEW,FS ,ITAB,
     3                  WEIGHT,A,AR   ,MS ,IN  ,
     3                  ENROT_T,ENCIN_T,XMASS_T,
     4                  XMOMT_T,YMOMT_T,ZMOMT_T,ISENS,
     4                  WEIGHT_MD,ENCIN2_T,ENROT2_T,
     5                  MS_2D    )
C-----------------------------------------------
      USE IMP_DYNA
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NOD(*), NBY(*), ISKEW(*),ITAB(*), WEIGHT(*),
     .   WEIGHT_MD(*)
C     REAL ou REAL*8
      my_real
     .   V(3,*), VR(3,*), X(3,*), RBY(*), SKEW(LSKEW,*), FS(*),
     .   A(3,*),AR(3,*),IN(*),MS(*),ENROT_T,ENCIN_T,XMASS_T,
     .   XMOMT_T,YMOMT_T,ZMOMT_T,ENCIN2_T,ENROT2_T,MS_2D(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "units_c.inc"
#include      "parit_c.inc"
#include      "impl1_c.inc"
#include      "param_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER M, NSN, ICODR, ISK, I, N,ISENS
C     REAL ou REAL*8
      my_real
     .   VI(3),VG(3),DT05,MAS,WEWE2
C to keep enough acuracy in single precision
      DOUBLE PRECISION
     .   ENROTT,ENCINT,XMASST,XMOMTT,YMOMTT,ZMOMTT,
     .   ENCIN2T,ENROT2T
C-----------------------------------------------
      M    =NBY(1)

      ENROTT=ZERO
      ENCINT=ZERO
      XMASST=ZERO
      XMOMTT=ZERO
      YMOMTT=ZERO
      ZMOMTT=ZERO
      ENCIN2T=ZERO
      ENROT2T=ZERO


C optimisation spmd
      IF (M.LT.0) RETURN
      NSN  =NBY(2)
C------------------------------------------------------------
C      CORRECTION DE L'ENERGIE CINETIQUE time = t
C------------------------------------------------------------
Cqz
      IF(IMPL_S.GT.0.AND.IDYNA.EQ.0)THEN
       IF(ISENS.EQ.0)THEN
        VG(1)=VR(1,M)
        VG(2)=VR(2,M)
        VG(3)=VR(3,M)
        VI(1)=RBY(1)*VG(1)+RBY(2)*VG(2)+RBY(3)*VG(3)
        VI(2)=RBY(4)*VG(1)+RBY(5)*VG(2)+RBY(6)*VG(3)
        VI(3)=RBY(7)*VG(1)+RBY(8)*VG(2)+RBY(9)*VG(3)
C
        ENROTT= - ( VG(1)*VG(1)
     .           + VG(2)*VG(2)
     .           + VG(3)*VG(3))*IN(M)*WEIGHT_MD(M)
     .         + ( RBY(10)*VI(1)*VI(1)
     .           + RBY(11)*VI(2)*VI(2)
     .           + RBY(12)*VI(3)*VI(3))*WEIGHT_MD(M)
        ENCINT=ZERO
        XMASST=ZERO
        XMOMTT=ZERO
        YMOMTT=ZERO
        ZMOMTT=ZERO
        ENCIN2T=ZERO
        ENROT2T=ZERO
C
        IF(N2D==0) THEN
         IF (NSN.GE.10.OR.IPARIT.GT.0) THEN
           DO I=1,NSN
            N = NOD(I)
            MAS=MS(N)*WEIGHT_MD(N)
            WEWE2 = (1-WEIGHT_MD(N))*WEIGHT(N)           
            VG(1)=V(1,N)
            VG(2)=V(2,N)
            VG(3)=V(3,N)
            ENCINT=ENCINT - ( VG(1)*VG(1)
     .                   + VG(2)*VG(2)
     .                   + VG(3)*VG(3))*MAS
            ENCIN2T=ENCIN2T - ( VG(1)*VG(1)
     .                  + VG(2)*VG(2)
     .                  + VG(3)*VG(3))*MS(N)*WEWE2     
            XMOMTT=XMOMTT-VG(1)*MAS
            YMOMTT=YMOMTT-VG(2)*MAS
            ZMOMTT=ZMOMTT-VG(3)*MAS
            VG(1)=VR(1,N)
            VG(2)=VR(2,N)
            VG(3)=VR(3,N)
            ENROTT=ENROTT - ( VG(1)*VG(1)
     .                   + VG(2)*VG(2)
     .                   + VG(3)*VG(3))*IN(N)*WEIGHT_MD(N)
            ENROT2T=ENROT2T - ( VG(1)*VG(1)
     .                   + VG(2)*VG(2)
     .                   + VG(3)*VG(3))*IN(N)*WEWE2    
            XMASST=XMASST-MAS
           ENDDO
         ELSE
#include "scalar.inc"
          DO I=1,NSN
           N = NOD(I)
           MAS=MS(N)*WEIGHT_MD(N)
           WEWE2 = (1-WEIGHT_MD(N))*WEIGHT(N)           
           VG(1)=V(1,N)
           VG(2)=V(2,N)
           VG(3)=V(3,N)
           ENCINT=ENCINT - ( VG(1)*VG(1)
     .                   + VG(2)*VG(2)
     .                  + VG(3)*VG(3))*MAS
           ENCIN2T=ENCIN2T - ( VG(1)*VG(1)
     .                  + VG(2)*VG(2)
     .                  + VG(3)*VG(3))*MS(N)*WEWE2      
           XMOMTT=XMOMTT-VG(1)*MAS
           YMOMTT=YMOMTT-VG(2)*MAS
           ZMOMTT=ZMOMTT-VG(3)*MAS
           VG(1)=VR(1,N)
           VG(2)=VR(2,N)
           VG(3)=VR(3,N)
           ENROTT=ENROTT - ( VG(1)*VG(1)
     .                   + VG(2)*VG(2)
     .                   + VG(3)*VG(3))*IN(N)*WEIGHT_MD(N)
           ENROT2T=ENROT2T - ( VG(1)*VG(1)
     .                   + VG(2)*VG(2)
     .                   + VG(3)*VG(3))*IN(N)*WEWE2       
           XMASST=XMASST-MAS
C
          ENDDO
         ENDIF
C
        ELSE  ! N2D =/ 0
C
         IF (NSN.GE.10.OR.IPARIT.GT.0) THEN
           DO I=1,NSN
            N = NOD(I)
            MAS=MS_2D(N)*WEIGHT_MD(N)
            WEWE2 = (1-WEIGHT_MD(N))*WEIGHT(N)           
            VG(1)=V(1,N)
            VG(2)=V(2,N)
            VG(3)=V(3,N)
            ENCINT=ENCINT - ( VG(1)*VG(1)
     .                   + VG(2)*VG(2)
     .                   + VG(3)*VG(3))*MAS
            ENCIN2T=ENCIN2T - ( VG(1)*VG(1)
     .                  + VG(2)*VG(2)
     .                  + VG(3)*VG(3))*MS_2D(N)*WEWE2     
            XMOMTT=XMOMTT-VG(1)*MAS
            YMOMTT=YMOMTT-VG(2)*MAS
            ZMOMTT=ZMOMTT-VG(3)*MAS
            VG(1)=VR(1,N)
            VG(2)=VR(2,N)
            VG(3)=VR(3,N)
            ENROTT=ENROTT - ( VG(1)*VG(1)
     .                   + VG(2)*VG(2)
     .                   + VG(3)*VG(3))*IN(N)*WEIGHT_MD(N)
            ENROT2T=ENROT2T - ( VG(1)*VG(1)
     .                   + VG(2)*VG(2)
     .                   + VG(3)*VG(3))*IN(N)*WEWE2    
            XMASST=XMASST-MAS
           ENDDO
         ELSE
#include "scalar.inc"
          DO I=1,NSN
           N = NOD(I)
           MAS=MS_2D(N)*WEIGHT_MD(N)
           WEWE2 = (1-WEIGHT_MD(N))*WEIGHT(N)           
           VG(1)=V(1,N)
           VG(2)=V(2,N)
           VG(3)=V(3,N)
           ENCINT=ENCINT - ( VG(1)*VG(1)
     .                   + VG(2)*VG(2)
     .                  + VG(3)*VG(3))*MAS
           ENCIN2T=ENCIN2T - ( VG(1)*VG(1)
     .                  + VG(2)*VG(2)
     .                  + VG(3)*VG(3))*MS_2D(N)*WEWE2      
           XMOMTT=XMOMTT-VG(1)*MAS
           YMOMTT=YMOMTT-VG(2)*MAS
           ZMOMTT=ZMOMTT-VG(3)*MAS
           VG(1)=VR(1,N)
           VG(2)=VR(2,N)
           VG(3)=VR(3,N)
           ENROTT=ENROTT - ( VG(1)*VG(1)
     .                   + VG(2)*VG(2)
     .                   + VG(3)*VG(3))*IN(N)*WEIGHT_MD(N)
           ENROT2T=ENROT2T - ( VG(1)*VG(1)
     .                   + VG(2)*VG(2)
     .                   + VG(3)*VG(3))*IN(N)*WEWE2       
           XMASST=XMASST-MAS
C
          ENDDO
         ENDIF
C
        ENDIF
       ELSE
C       RBY AVEC SENSOR : PAS DE MASSES OU INERTIE AJOUTEE
        IF(N2D==0) THEN
          MAS=MS(M)*WEIGHT_MD(M)
        ELSE
          MAS=MS_2D(M)*WEIGHT_MD(M)
        ENDIF
          VG(1)=VR(1,M)
          VG(2)=VR(2,M)
          VG(3)=VR(3,M)
C
          ENROTT= - ( VG(1)*VG(1)
     .              + VG(2)*VG(2)
     .              + VG(3)*VG(3))*IN(M)*WEIGHT_MD(M)
C

          VG(1)=V(1,M)
          VG(2)=V(2,M)
          VG(3)=V(3,M)
          ENCINT= - ( VG(1)*VG(1)
     .              + VG(2)*VG(2)
     .              + VG(3)*VG(3))*MAS
          XMASST=-MAS
          XMOMTT=-VG(1)*MAS
          YMOMTT=-VG(2)*MAS
          ZMOMTT=-VG(3)*MAS
C
       ENDIF
      ELSE
Cqz
      IF(ISENS.EQ.0)THEN
        DT05 = UNDEMI*DT1
Cqz
        IF(IDYNA.GT.0) DT05=(DY_G-UN)*DT1
        VG(1)=VR(1,M)+AR(1,M)*DT05
        VG(2)=VR(2,M)+AR(2,M)*DT05
        VG(3)=VR(3,M)+AR(3,M)*DT05
        VI(1)=RBY(1)*VG(1)+RBY(2)*VG(2)+RBY(3)*VG(3)
        VI(2)=RBY(4)*VG(1)+RBY(5)*VG(2)+RBY(6)*VG(3)
        VI(3)=RBY(7)*VG(1)+RBY(8)*VG(2)+RBY(9)*VG(3)
C
        ENROTT= - ( VG(1)*VG(1)
     .           + VG(2)*VG(2)
     .           + VG(3)*VG(3))*IN(M)*WEIGHT_MD(M)
     .         + ( RBY(10)*VI(1)*VI(1)
     .           + RBY(11)*VI(2)*VI(2)
     .           + RBY(12)*VI(3)*VI(3))*WEIGHT_MD(M)
        ENCINT=ZERO
        XMASST=ZERO
        XMOMTT=ZERO
        YMOMTT=ZERO
        ZMOMTT=ZERO
        ENCIN2T=ZERO        
        ENROT2T=ZERO
C
Cel vectorisation si nsn.ge.10 ou si imach = 3 et p/on active
        IF(N2D==0) THEN
         IF (NSN.GE.10.OR.IPARIT.GT.0) THEN
           DO I=1,NSN
C
            N = NOD(I)
C
            MAS=MS(N)*WEIGHT_MD(N)
            WEWE2 = (1-WEIGHT_MD(N))*WEIGHT(N)
            VG(1)=V(1,N)+A(1,N)*DT05
            VG(2)=V(2,N)+A(2,N)*DT05
            VG(3)=V(3,N)+A(3,N)*DT05
            ENCINT=ENCINT - ( VG(1)*VG(1)
     .                   + VG(2)*VG(2)
     .                   + VG(3)*VG(3))*MAS
            ENCIN2T=ENCIN2T - ( VG(1)*VG(1)
     .                   + VG(2)*VG(2)
     .                   + VG(3)*VG(3))*MS(N)*WEWE2      
            XMOMTT=XMOMTT-VG(1)*MAS
            YMOMTT=YMOMTT-VG(2)*MAS
            ZMOMTT=ZMOMTT-VG(3)*MAS
            VG(1)=VR(1,N)+AR(1,N)*DT05
            VG(2)=VR(2,N)+AR(2,N)*DT05
            VG(3)=VR(3,N)+AR(3,N)*DT05
            ENROTT=ENROTT - ( VG(1)*VG(1)
     .                   + VG(2)*VG(2)
     .                   + VG(3)*VG(3))*IN(N)*WEIGHT_MD(N)
            ENROT2T=ENROT2T - ( VG(1)*VG(1)
     .                   + VG(2)*VG(2)
     .                   + VG(3)*VG(3))*IN(N)*WEWE2      
            XMASST=XMASST-MAS
C
           ENDDO
         ELSE
#include "scalar.inc"
          DO I=1,NSN
C
           N = NOD(I)
C
           MAS=MS(N)*WEIGHT_MD(N)
           WEWE2 = (1-WEIGHT_MD(N))*WEIGHT(N)          
           VG(1)=V(1,N)+A(1,N)*DT05
           VG(2)=V(2,N)+A(2,N)*DT05
           VG(3)=V(3,N)+A(3,N)*DT05
           ENCINT=ENCINT - ( VG(1)*VG(1)
     .                   + VG(2)*VG(2)
     .                   + VG(3)*VG(3))*MAS
           ENCIN2T=ENCIN2T - ( VG(1)*VG(1)
     .                   + VG(2)*VG(2)
     .                   + VG(3)*VG(3))*MS(N)*WEWE2      
           XMOMTT=XMOMTT-VG(1)*MAS
           YMOMTT=YMOMTT-VG(2)*MAS
           ZMOMTT=ZMOMTT-VG(3)*MAS
           VG(1)=VR(1,N)+AR(1,N)*DT05
           VG(2)=VR(2,N)+AR(2,N)*DT05
           VG(3)=VR(3,N)+AR(3,N)*DT05
           ENROTT=ENROTT - ( VG(1)*VG(1)
     .                   + VG(2)*VG(2)
     .                   + VG(3)*VG(3))*IN(N)*WEIGHT_MD(N)
           ENROT2T=ENROT2T - ( VG(1)*VG(1)
     .                   + VG(2)*VG(2)
     .                   + VG(3)*VG(3))*IN(N)*WEWE2       
           XMASST=XMASST-MAS
C
          ENDDO
        ENDIF
C
       ELSE ! N2D =/0
C
         IF (NSN.GE.10.OR.IPARIT.GT.0) THEN
           DO I=1,NSN
C
            N = NOD(I)
C
            MAS=MS_2D(N)*WEIGHT_MD(N)
            WEWE2 = (1-WEIGHT_MD(N))*WEIGHT(N)
            VG(1)=V(1,N)+A(1,N)*DT05
            VG(2)=V(2,N)+A(2,N)*DT05
            VG(3)=V(3,N)+A(3,N)*DT05
            ENCINT=ENCINT - ( VG(1)*VG(1)
     .                   + VG(2)*VG(2)
     .                   + VG(3)*VG(3))*MAS
            ENCIN2T=ENCIN2T - ( VG(1)*VG(1)
     .                   + VG(2)*VG(2)
     .                   + VG(3)*VG(3))*MS_2D(N)*WEWE2      
            XMOMTT=XMOMTT-VG(1)*MAS
            YMOMTT=YMOMTT-VG(2)*MAS
            ZMOMTT=ZMOMTT-VG(3)*MAS
            VG(1)=VR(1,N)+AR(1,N)*DT05
            VG(2)=VR(2,N)+AR(2,N)*DT05
            VG(3)=VR(3,N)+AR(3,N)*DT05
            ENROTT=ENROTT - ( VG(1)*VG(1)
     .                   + VG(2)*VG(2)
     .                   + VG(3)*VG(3))*IN(N)*WEIGHT_MD(N)
            ENROT2T=ENROT2T - ( VG(1)*VG(1)
     .                   + VG(2)*VG(2)
     .                   + VG(3)*VG(3))*IN(N)*WEWE2      
            XMASST=XMASST-MAS
C
           ENDDO
         ELSE
#include "scalar.inc"
          DO I=1,NSN
C
           N = NOD(I)
C
           MAS=MS_2D(N)*WEIGHT_MD(N)
           WEWE2 = (1-WEIGHT_MD(N))*WEIGHT(N)          
           VG(1)=V(1,N)+A(1,N)*DT05
           VG(2)=V(2,N)+A(2,N)*DT05
           VG(3)=V(3,N)+A(3,N)*DT05
           ENCINT=ENCINT - ( VG(1)*VG(1)
     .                   + VG(2)*VG(2)
     .                   + VG(3)*VG(3))*MAS
           ENCIN2T=ENCIN2T - ( VG(1)*VG(1)
     .                   + VG(2)*VG(2)
     .                   + VG(3)*VG(3))*MS_2D(N)*WEWE2      
           XMOMTT=XMOMTT-VG(1)*MAS
           YMOMTT=YMOMTT-VG(2)*MAS
           ZMOMTT=ZMOMTT-VG(3)*MAS
           VG(1)=VR(1,N)+AR(1,N)*DT05
           VG(2)=VR(2,N)+AR(2,N)*DT05
           VG(3)=VR(3,N)+AR(3,N)*DT05
           ENROTT=ENROTT - ( VG(1)*VG(1)
     .                   + VG(2)*VG(2)
     .                   + VG(3)*VG(3))*IN(N)*WEIGHT_MD(N)
           ENROT2T=ENROT2T - ( VG(1)*VG(1)
     .                   + VG(2)*VG(2)
     .                   + VG(3)*VG(3))*IN(N)*WEWE2       
           XMASST=XMASST-MAS
C
          ENDDO
        ENDIF
C
       ENDIF
      ELSE
C       RBY AVEC SENSOR : PAS DE MASSES OU INERTIE AJOUTEE
        DT05 = UNDEMI*DT1
Cqz
        IF(IDYNA.GT.0) DT05=(DY_G-UN)*DT1
        VG(1)=VR(1,M)+AR(1,M)*DT05
        VG(2)=VR(2,M)+AR(2,M)*DT05
        VG(3)=VR(3,M)+AR(3,M)*DT05
C
        ENROTT= - ( VG(1)*VG(1)
     .            + VG(2)*VG(2)
     .            + VG(3)*VG(3))*IN(M)*WEIGHT_MD(M)
C
        IF(N2D==0) THEN
          MAS=MS(M)*WEIGHT_MD(M)
        ELSE
          MAS=MS_2D(M)*WEIGHT_MD(M)
        ENDIF
        VG(1)=V(1,M)+A(1,M)*DT05
        VG(2)=V(2,M)+A(2,M)*DT05
        VG(3)=V(3,M)+A(3,M)*DT05
        ENCINT= - ( VG(1)*VG(1)
     .            + VG(2)*VG(2)
     .            + VG(3)*VG(3))*MAS
        XMASST=-MAS
        XMOMTT=-VG(1)*MAS
        YMOMTT=-VG(2)*MAS
        ZMOMTT=-VG(3)*MAS
      ENDIF
      ENDIF
C
      ENROT_T=ENROT_T + ENROTT*UNDEMI
      ENCIN_T=ENCIN_T + ENCINT*UNDEMI
      ENROT2_T=ENROT2_T + ENROT2T*UNDEMI
      ENCIN2_T=ENCIN2_T + ENCIN2T*UNDEMI   
      XMASS_T=XMASS_T + XMASST
      XMOMT_T=XMOMT_T + XMOMTT
      YMOMT_T=YMOMT_T + YMOMTT
      ZMOMT_T=ZMOMT_T + ZMOMTT
C
      RETURN
      END
Chd|====================================================================
Chd|  RBE2COR                       source/constraints/general/rbody/rgbcor.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        PRERBE2                       source/constraints/general/rbe2/rbe2f.F
Chd|        RBE2COR0                      source/constraints/general/rbody/rgbcor.F
Chd|        IMP_DYNA                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE RBE2COR(IRBE2 ,LRBE2 ,X    ,V     ,VR    ,
     2                   SKEW  ,ISKEW ,ITAB ,WEIGHT,A     ,
     3                   AR    ,MS    ,IN   ,WEIGHT_MD)
C-----------------------------------------------
      USE IMP_DYNA
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr11_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE2(NRBE2L,*),LRBE2(*)
      INTEGER WEIGHT(*),ISKEW(*),ITAB(*),WEIGHT_MD(*)
C     REAL ou REAL*8
      my_real
     .   X(3,*) ,V(3,*) ,VR(3,*),SKEW(*),
     .   A(3,*),AR(3,*),IN(*),MS(*) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  J,K,N,KK,IAD,M,
     .        JT(3,NRBE2),JR(3,NRBE2),NM,NN,ISK,NSN,IRAD
C     REAL ou REAL*8
      my_real
     .   ENROT_T,ENCIN_T,XMASS_T,
     .   XMOMT_T,YMOMT_T,ZMOMT_T,ENCIN2_T,ENROT2_T
C-----------------------------------------------
      CALL PRERBE2(IRBE2 ,JT  ,JR   )
      ENROT_T =ZERO
      ENCIN_T =ZERO    
      XMASS_T =ZERO
      XMOMT_T =ZERO
      YMOMT_T =ZERO
      ZMOMT_T =ZERO
      ENCIN2_T=ZERO
      ENROT2_T=ZERO             
!$OMP DO SCHEDULE(DYNAMIC,1)
      DO N=1,NRBE2
        IAD = IRBE2(1,N)
        M  = IRBE2(3,N)
        NSN = IRBE2(5,N)
        ISK = IRBE2(7,N)
        IRAD = IRBE2(11,N)
        CALL RBE2COR0(V  ,VR  ,X    ,NSN   ,LRBE2(IAD+1),
     2                JT(1,N),JR(1,N),M    ,WEIGHT,A     ,
     3                AR ,MS  ,IN   ,ITAB  ,IRAD  ,
     4                ENROT_T,ENCIN_T,XMASS_T,XMOMT_T,YMOMT_T,
     5                ZMOMT_T,WEIGHT_MD,ENCIN2_T,ENROT2_T)
      ENDDO
!$OMP END DO NOWAIT
#include "lockon.inc"
      ENROT=ENROT + ENROT_T
      ENCIN=ENCIN + ENCIN_T     
      XMASS=XMASS + XMASS_T
      XMOMT=XMOMT + XMOMT_T
      YMOMT=YMOMT + YMOMT_T
      ZMOMT=ZMOMT + ZMOMT_T
      ENCIN2=ENCIN2 + ENCIN2_T
      ENROT2=ENROT2 + ENROT2_T              
#include "lockoff.inc"
C
      RETURN
      END
Chd|====================================================================
Chd|  RBE2COR0                      source/constraints/general/rbody/rgbcor.F
Chd|-- called by -----------
Chd|        RBE2COR                       source/constraints/general/rbody/rgbcor.F
Chd|-- calls ---------------
Chd|        IMP_DYNA                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE RBE2COR0(V  ,VR  ,X    ,NSL   ,ISL   ,
     2                    JT ,JR  ,M    ,WEIGHT,A     ,
     3                    AR ,MS  ,IN   ,ITAB ,IRAD  ,
     4                   ENROT_T,ENCIN_T,XMASS_T,XMOMT_T,YMOMT_T,
     5                   ZMOMT_T,WEIGHT_MD,ENCIN2_T,ENROT2_T)
C-----------------------------------------------
      USE IMP_DYNA
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSL,ISL(*), ITAB(*), WEIGHT(*),
     .         WEIGHT_MD(*),JT(3),JR(3),M,IRAD
C     REAL ou REAL*8
      my_real
     .   V(3,*), VR(3,*), X(3,*), 
     .   A(3,*),AR(3,*),IN(*),MS(*),ENROT_T,ENCIN_T,XMASS_T,
     .   XMOMT_T,YMOMT_T,ZMOMT_T,ENCIN2_T,ENROT2_T
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "sms_c.inc"
#include      "units_c.inc"
#include      "parit_c.inc"
#include      "impl1_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, NS,ITRA,IROT
C     REAL ou REAL*8
      my_real
     .   VI(3),VG(3),DT05,MAS,WEWE2,WROT,WROT2,WTRA2
C to keep enough acuracy in single precision
      DOUBLE PRECISION
     .   ENROTT,ENCINT,XMASST,XMOMTT,YMOMTT,ZMOMTT,
     .   ENCIN2T,ENROT2T
C======================================================================|
C-----for the moment RBE2 est spheric ---
      IF ((JT(1)+JT(2)+JT(3)) > 0) THEN
       ITRA = 1
      ELSE
       ITRA = 0
      END IF 
C-----in case of JR=0 and IRAD=0, Inertia0 of Main is suppposed << In_s      
      IF ((JR(1)+JR(2)+JR(3)) >0 .OR. IRAD==0) THEN
       IROT = 1
      ELSE
       IROT = 0
      END IF   
      
        ENCINT=ZERO
        XMASST=ZERO
        XMOMTT=ZERO
        YMOMTT=ZERO
        ZMOMTT=ZERO
        ENCIN2T=ZERO
        ENROT2T=ZERO
        ENROTT =ZERO
      IF(IMPL_S.GT.0.AND.IDYNA.EQ.0)THEN
C
         IF (IRODDL/=0) THEN
          VG(1)=VR(1,M)
          VG(2)=VR(2,M)
          VG(3)=VR(3,M)
C
          WROT = WEIGHT_MD(M)*IROT
          ENROTT= - ( VG(1)*VG(1)
     .           + VG(2)*VG(2)
     .           + VG(3)*VG(3))*IN(M)*WROT
         END IF !(IRODDL/=0) THEN
        IF (NSL.GE.10.OR.IPARIT.GT.0) THEN
          DO I=1,NSL
           N = ISL(I)
           MAS=MS(N)*WEIGHT_MD(N)*ITRA
           WEWE2 = (1-WEIGHT_MD(N))*WEIGHT(N)          
           WTRA2 = WEWE2*ITRA          
           VG(1)=V(1,N)
           VG(2)=V(2,N)
           VG(3)=V(3,N)
           ENCINT=ENCINT - ( VG(1)*VG(1)
     .                  + VG(2)*VG(2)
     .                  + VG(3)*VG(3))*MAS
           ENCIN2T=ENCIN2T - ( VG(1)*VG(1)
     .                  + VG(2)*VG(2)
     .                  + VG(3)*VG(3))*MS(N)*WTRA2     
           XMOMTT=XMOMTT-VG(1)*MAS
           YMOMTT=YMOMTT-VG(2)*MAS
           ZMOMTT=ZMOMTT-VG(3)*MAS
           XMASST=XMASST-MAS
          ENDDO
         IF (IRODDL/=0) THEN
          DO I=1,NSL
           N = ISL(I)
           WEWE2 = (1-WEIGHT_MD(N))*WEIGHT(N)          
           VG(1)=VR(1,N)
           VG(2)=VR(2,N)
           VG(3)=VR(3,N)
           WROT = WEIGHT_MD(N)*IROT
           WROT2 =WEWE2*IROT          
           ENROTT=ENROTT - ( VG(1)*VG(1)
     .                  + VG(2)*VG(2)
     .                  + VG(3)*VG(3))*IN(N)*WROT
           ENROT2T=ENROT2T - ( VG(1)*VG(1)
     .                  + VG(2)*VG(2)
     .                  + VG(3)*VG(3))*IN(N)*WROT2    
          ENDDO
         END IF !(IRODDL/=0) THEN
        ELSE
#include "scalar.inc"
         DO I=1,NSL
          N = ISL(I)
          MAS=MS(N)*WEIGHT_MD(N)*ITRA
          WEWE2 = (1-WEIGHT_MD(N))*WEIGHT(N)           
          WTRA2 = WEWE2*ITRA          
          VG(1)=V(1,N)
          VG(2)=V(2,N)
          VG(3)=V(3,N)
          ENCINT=ENCINT - ( VG(1)*VG(1)
     .                  + VG(2)*VG(2)
     .                  + VG(3)*VG(3))*MAS
          ENCIN2T=ENCIN2T - ( VG(1)*VG(1)
     .                  + VG(2)*VG(2)
     .                  + VG(3)*VG(3))*MS(N)*WTRA2      
          XMOMTT=XMOMTT-VG(1)*MAS
          YMOMTT=YMOMTT-VG(2)*MAS
          ZMOMTT=ZMOMTT-VG(3)*MAS
          XMASST=XMASST-MAS
C
         ENDDO
         IF (IRODDL/=0) THEN
          DO I=1,NSL
           N = ISL(I)
           WEWE2 = (1-WEIGHT_MD(N))*WEIGHT(N)           
           VG(1)=VR(1,N)
           VG(2)=VR(2,N)
           VG(3)=VR(3,N)
           WROT = WEIGHT_MD(N)*IROT
           WROT2 =WEWE2*IROT          
           ENROTT=ENROTT - ( VG(1)*VG(1)
     .                  + VG(2)*VG(2)
     .                  + VG(3)*VG(3))*IN(N)*WROT
           ENROT2T=ENROT2T - ( VG(1)*VG(1)
     .                  + VG(2)*VG(2)
     .                  + VG(3)*VG(3))*IN(N)*WROT2       
          ENDDO
         END IF !(IRODDL/=0) THEN
        ENDIF
      ELSE
        DT05 = UNDEMI*DT1
C
        IF(IDYNA.GT.0) DT05=(DY_G-UN)*DT1
C
         IF (IRODDL/=0) THEN
          VG(1)=VR(1,M)+AR(1,M)*DT05
          VG(2)=VR(2,M)+AR(2,M)*DT05
          VG(3)=VR(3,M)+AR(3,M)*DT05
C
          ENROTT= - ( VG(1)*VG(1)
     .           + VG(2)*VG(2)
     .           + VG(3)*VG(3))*IN(M)*WEIGHT_MD(M)*IROT
         END IF !(IRODDL/=0) THEN
Cel vectorisation si nsn.ge.10 ou si imach = 3 et p/on active
        IF (NSL.GE.10.OR.IPARIT.GT.0) THEN
          DO I=1,NSL
C
           N = ISL(I)
C
           MAS=MS(N)*WEIGHT_MD(N)*ITRA
           WEWE2 = (1-WEIGHT_MD(N))*WEIGHT(N)
           WTRA2 = WEWE2*ITRA          
           VG(1)=V(1,N)+A(1,N)*DT05
           VG(2)=V(2,N)+A(2,N)*DT05
           VG(3)=V(3,N)+A(3,N)*DT05
           ENCINT=ENCINT - ( VG(1)*VG(1)
     .                     + VG(2)*VG(2)
     .                     + VG(3)*VG(3))*MAS
           ENCIN2T=ENCIN2T - ( VG(1)*VG(1)
     .                        + VG(2)*VG(2)
     .                        + VG(3)*VG(3))*MS(N)*WTRA2  
           XMOMTT=XMOMTT-VG(1)*MAS
           YMOMTT=YMOMTT-VG(2)*MAS
           ZMOMTT=ZMOMTT-VG(3)*MAS
           XMASST=XMASST-MAS
C
          ENDDO
         IF (IRODDL/=0) THEN
          DO I=1,NSL
C
           N = ISL(I)
           WEWE2 = (1-WEIGHT_MD(N))*WEIGHT(N)
           VG(1)=VR(1,N)+AR(1,N)*DT05
           VG(2)=VR(2,N)+AR(2,N)*DT05
           VG(3)=VR(3,N)+AR(3,N)*DT05
           WROT = WEIGHT_MD(N)*IROT
           WROT2 =WEWE2*IROT          
           ENROTT=ENROTT - ( VG(1)*VG(1)
     .                  + VG(2)*VG(2)
     .                  + VG(3)*VG(3))*IN(N)*WROT
           ENROT2T=ENROT2T - ( VG(1)*VG(1)
     .                  + VG(2)*VG(2)
     .                  + VG(3)*VG(3))*IN(N)*WROT2      
          ENDDO
         ENDIF !(IRODDL/=0) THEN
        ELSE
#include "scalar.inc"
         DO I=1,NSL
C
          N = ISL(I)
C
          MAS=MS(N)*WEIGHT_MD(N)*ITRA
          WEWE2 = (1-WEIGHT_MD(N))*WEIGHT(N)          
          WTRA2 = WEWE2*ITRA          
          VG(1)=V(1,N)+A(1,N)*DT05
          VG(2)=V(2,N)+A(2,N)*DT05
          VG(3)=V(3,N)+A(3,N)*DT05
          ENCINT=ENCINT - ( VG(1)*VG(1)
     .                    + VG(2)*VG(2)
     .                    + VG(3)*VG(3))*MAS
          ENCIN2T=ENCIN2T - ( VG(1)*VG(1)
     .                       + VG(2)*VG(2)
     .                       + VG(3)*VG(3))*MS(N)*WTRA2  
          XMOMTT=XMOMTT-VG(1)*MAS
          YMOMTT=YMOMTT-VG(2)*MAS
          ZMOMTT=ZMOMTT-VG(3)*MAS
          XMASST=XMASST-MAS
C
         ENDDO
         IF (IRODDL/=0) THEN
         DO I=1,NSL
C
          N = ISL(I)
          WEWE2 = (1-WEIGHT_MD(N))*WEIGHT(N)          
          VG(1)=VR(1,N)+AR(1,N)*DT05
          VG(2)=VR(2,N)+AR(2,N)*DT05
          VG(3)=VR(3,N)+AR(3,N)*DT05
           WROT = WEIGHT_MD(N)*IROT
           WROT2 =WEWE2*IROT          
          ENROTT=ENROTT - ( VG(1)*VG(1)
     .                  + VG(2)*VG(2)
     .                  + VG(3)*VG(3))*IN(N)*WROT
          ENROT2T=ENROT2T - ( VG(1)*VG(1)
     .                  + VG(2)*VG(2)
     .                  + VG(3)*VG(3))*IN(N)*WROT2       
         ENDDO
         END IF !(IRODDL/=0) THEN
        ENDIF
      ENDIF
C
      ENROT_T=ENROT_T + ENROTT*UNDEMI
      ENCIN_T=ENCIN_T + ENCINT*UNDEMI
      ENROT2_T=ENROT2_T + ENROT2T*UNDEMI
      ENCIN2_T=ENCIN2_T + ENCIN2T*UNDEMI      
      XMASS_T=XMASS_T + XMASST
      XMOMT_T=XMOMT_T + XMOMTT
      YMOMT_T=YMOMT_T + YMOMTT
      ZMOMT_T=ZMOMT_T + ZMOMTT
C
      RETURN
      END
 
