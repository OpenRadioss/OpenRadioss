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
Chd|  SWITCH_TO_DTNODA              source/time_step/switch_to_dtnoda.F
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SWITCH_TO_DTNODA(
     .                  IXR      ,GEO      ,PM       ,IPARG    ,ELBUF_TAB,
     .                  MS       ,IN       ,ITAB     ,IGEO     ,IPM    ,
     .                  UPARAM   ,IPART    ,IGRNOD   ,IGRPART)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
      USE GROUPDEF_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr02_c.inc"
#include      "rnur_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "sms_c.inc"
#include      "sphcom.inc"
#include      "tabsiz_c.inc"
#include      "task_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXR(NIXR,*), ITAB(*),
     .        IGEO(NPROPGI,*),IPM(NPROPMI,*),IPARG(NPARG,*)
      INTEGER, DIMENSION(SIPART), TARGET :: IPART
C     REAL ou REAL*8
      my_real
     .   GEO(NPROPG,*),PM(NPROPM,*),UPARAM(*),MS(*),IN(*)
C-----------------------------------------------
      TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (GROUP_)   ,DIMENSION(NGRPART) :: IGRPART
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,N1,N2,IPID,IMAT,IADBUF,IEQUI,IP,IERR,IERROR,
     .        K1,K11,K12,K13,K14,
     .        IOK,IDTGRX,NG
      INTEGER I15ATH,I15A,I15B,I15C,I15D,I15E,I15F,I15G,I15H,I15I,I15J,I15K
      INTEGER TAGN(NUMNOD), TAGR(NUMELR),TAGPRT_SMS(NPART)
C     REAL ou REAL*8
      my_real
     .   XKM, XCM, XKR, XCR, XIN(MVSIZ)
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      INTEGER, DIMENSION(:), POINTER :: IPARTR
C-----------------------------------------------------
C     Check for springs with stiffness but no mass
C     ... Switch for cohesive elements should also be done here ...
C-----------------------------------------------------
      I15ATH=1+LIPART1*(NPART+NTHPART)
      I15A=I15ATH+2*9*(NPART+NTHPART)
      I15B=I15A+NUMELS
      I15C=I15B+NUMELQ
      I15D=I15C+NUMELC
      I15E=I15D+NUMELT
      I15F=I15E+NUMELP
      I15G=I15F+NUMELR
      I15H=I15G+NUMELUR
      I15I=I15H+NUMELTG
      I15J=I15I+NUMELX
      I15K=I15J+NUMSPH
C
      IPARTR => IPART(I15F:I15G-1)
C-----------------------------------------------------
      IF(NODADT/=0)THEN
        IF(IDTGR(11).EQ.0)THEN
          TAGN(1:NUMNOD)=1                                          
        ELSE
          TAGN(1:NUMNOD)=0
          IOK = 0
          DO N=1,NGRNOD
            IF (IABS(IDTGR(11)).EQ.IGRNOD(N)%ID) THEN
              IDTGRX= N
              IOK   = 1
            ENDIF
          ENDDO
          IF (IOK == 0) THEN
            CALL ANCMSG(MSGID=237,ANMODE=ANINFO,
     .                  I1=IABS(IDTGR(11)))
            CALL ARRET(2)
          ENDIF        
          DO N=1,IGRNOD(IDTGRX)%NENTITY
            TAGN(IGRNOD(IDTGRX)%ENTITY(N)) = 1
          ENDDO                                                        
        ENDIF
      ELSE
        TAGN(1:NUMNOD)=0
      ENDIF
C-----------------------------------------------------
      TAGR(1:NUMELR)=0
      IF(IDTMINS==2)THEN
C
        IF(IDTGRS==0)THEN
          DO IP=1,NPART
            TAGPRT_SMS(IP)=1
          END DO
        ELSE
          DO IP=1,NPART
            TAGPRT_SMS(IP)=0
          END DO
          IF(IDTGRS < 0)THEN
           DO N=1,NGRPART
            IF (IGRPART(N)%ID==-IDTGRS) THEN
              IDTGRX=N
              GO TO 120
            END IF
           END DO
 100       CONTINUE
           CALL ANCMSG(MSGID=21,ANMODE=ANINFO_BLIND,
     .                 I1=-IDTGRS)
           CALL ARRET(2)
 120       CONTINUE
          END IF
C
          DO I=1,IGRPART(IDTGRX)%NENTITY
            IP=IGRPART(IDTGRX)%ENTITY(I)
            TAGPRT_SMS(IP)=1
          END DO
        END IF
C-----------------------------------------------------
        IF (ISMS_SELEC.EQ.1) THEN
C-- Full AMS
          DO I=1,NUMELR
            TAGR(I)=1
          END DO
        ELSEIF (ISMS_SELEC.EQ.2) THEN
C-- AMS by parts    
          DO I=1,NUMELR
            IF(TAGPRT_SMS(IPARTR(I))==0)THEN
              TAGR(I)=0
            ELSE
              TAGR(I)=1
            END IF
          END DO
        ELSEIF (ISMS_SELEC.EQ.3) THEN
C-- AMS auto - defined by elements
          DO NG = 1, NGROUP
            ITY   =IPARG(5,NG)
            IF(ITY==6)THEN
              NFT   =IPARG(3,NG)
              LFT   =1
              LLT   =IPARG(2,NG)    
              GBUF => ELBUF_TAB(NG)%GBUF 
              DO I=LFT,LLT
                IF(GBUF%ISMS(I).EQ.0)THEN
                  TAGR(NFT+I)=0
                ELSE
                  TAGR(NFT+I)=1
                END IF
              END DO
            END IF
          END DO
        ELSEIF (ISMS_SELEC.EQ.4) THEN
C-- AMS auto + parts
          DO NG = 1, NGROUP
            ITY   =IPARG(5,NG)
            IF(ITY==6)THEN
              NFT   =IPARG(3,NG)
              LFT   =1
              LLT   =IPARG(2,NG)    
              GBUF => ELBUF_TAB(NG)%GBUF 
              DO I=LFT,LLT
                IF(GBUF%ISMS(I).EQ.0.AND.TAGPRT_SMS(IPARTR(NFT+I)).EQ.0)THEN
                  TAGR(NFT+I)=0
                ELSE
                  TAGR(NFT+I)=1
                END IF
              END DO
            END IF
          END DO
        END IF
      END IF
C-----------------------------------------------------
      IERR=0
      DO NG = 1, NGROUP
        ITY   =IPARG(5,NG)
        IF(ITY==6)THEN
          NFT   =IPARG(3,NG)
          LFT   =1
          LLT   =IPARG(2,NG)    
          GBUF => ELBUF_TAB(NG)%GBUF 
C
          IPID = IXR(1,NFT+1)
          IGTYP= IGEO(11,IPID)
C
          IF(IGTYP==23)THEN
C
            IMAT = IXR(5,NFT+1)
            IADBUF = IPM(7,IMAT) - 1
            MTN    = IPM(2,IMAT)
C
            K1 = 4
            K11 = 64
            K12 = K11 + 6
            K13 = K12 + 6
            K14 = K13 + 6
C
            IF(MTN == 108) THEN
              IEQUI = UPARAM(IADBUF+4)
              XKM= MAX(UPARAM(IADBUF + K11 + 1)*UPARAM(IADBUF + K1 + 1),
     .                 UPARAM(IADBUF + K11 + 2)*UPARAM(IADBUF + K1 + 2),
     .                 UPARAM(IADBUF + K11 + 3)*UPARAM(IADBUF + K1 + 3))   ! /XL(I)
              XCM= MAX(UPARAM(IADBUF + K12 + 1),UPARAM(IADBUF + K12 + 2),UPARAM(IADBUF + K12 + 3))
              XKR= MAX(UPARAM(IADBUF + K11 + 4)*UPARAM(IADBUF + K1 + 4),
     .                 UPARAM(IADBUF + K11 + 5)*UPARAM(IADBUF + K1 + 5),
     .                 UPARAM(IADBUF + K11 + 6)*UPARAM(IADBUF + K1 + 6))   ! /XL(I)
              XCR= MAX(UPARAM(IADBUF + K12 + 4),UPARAM(IADBUF + K12 + 5),UPARAM(IADBUF + K12 + 6))
              DO I=LFT,LLT
                N1  =IXR(2,NFT+I)
                N2  =IXR(3,NFT+I)
                IF(GBUF%MASS(I)==ZERO)THEN
                  IF(XKM/=ZERO.OR.XCM/=ZERO)THEN
                    IF(NODADT==0.AND.IDTMINS/=2)THEN
                      IERR=1
                    ELSEIF(.NOT.((NODADT/=0 .AND.TAGN(N1)/=0 .AND. TAGN(N2)/=0).OR.
     .                           (IDTMINS==2.AND.TAGR(I)/=0)))THEN
                      IERR=1
                    END IF
                  END IF
                END IF
                XIN(I)= GEO(2,IPID)
                IF(XIN(I)==ZERO)THEN
                  IF(XKR/=ZERO.OR.XCR/=ZERO.OR.(IEQUI/=0.AND.(XKM/=ZERO.OR.XCM/=ZERO)))THEN
                    IF(NODADT==0.AND.IDTMINS/=2)THEN
                      IERR=1
                    ELSEIF(.NOT.((NODADT/=0 .AND.TAGN(N1)/=0 .AND. TAGN(N2)/=0).OR.
     .                           (IDTMINS==2.AND.TAGR(I)/=0)))THEN
                      IERR=1
                    END IF
                  END IF
                END IF
              END DO
            END IF
          END IF 
        END IF        
      END DO
C------------------------------------------
      IF(NSPMD > 0)THEN
#ifdef MPI
      CALL MPI_ALLREDUCE(MPI_IN_PLACE,IERR,1,MPI_INTEGER,MPI_MAX,MPI_COMM_WORLD,IERROR)
#endif
      END IF
      IF(IERR/=0)THEN
        NODADT   =1
        IDTGR(11)=0
        IF(ISPMD==0)THEN
          CALL ANCMSG(MSGID=286,ANMODE=ANINFO_BLIND_1)
        END IF
      END IF
C------------------------------------------
      RETURN
      END      
