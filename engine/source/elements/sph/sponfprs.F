Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    sponfprs       ../engine/source/elements/sph/sponfprs.F
!||--- called by ------------------------------------------------------
!||    forintp        ../engine/source/elements/forintp.F
!||--- calls      -----------------------------------------------------
!||    initbuf        ../engine/share/resol/initbuf.F
!||--- uses       -----------------------------------------------------
!||    elbufdef_mod   ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    initbuf_mod    ../engine/share/resol/initbuf.F
!||    sphbox         ../engine/share/modules/sphbox.F
!||====================================================================
      SUBROUTINE SPONFPRS(X ,V        ,A       ,MS      ,
     2              SPBUF   ,ITAB     ,KXSP    ,IXSP    ,NOD2SP  ,
     3              ISPHIO  ,VSPHIO   ,NPC     ,PLD     ,PM      ,
     4              IPARG   ,ELBUF_TAB,IPART   ,IPARTSP ,WASPACT ,
     5              VNORMAL ,WA       ,SPHVELN ,WAR     ,WFEXT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE SPHBOX
      USE ELBUFDEF_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com01_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "sphcom.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXSP(NISP,*),IXSP(KVOISPH,*),NOD2SP(*),ITAB(*),
     .        ISPHIO(NISPHIO,*),IPART(LIPART1,*),IPARTSP(*),WASPACT(*),
     .        NPC(*),IPARG(NPARG,*)
      my_real
     .   X(3,*) ,V(3,*) ,A(3,*) ,MS(*) ,SPBUF(NSPBUF,*) ,VSPHIO(*) ,
     .   PLD(*) ,PM(NPROPM,*),VNORMAL(3,*),WA(KWASPH,*), SPHVELN(2,*),
     .   WAR(10,*)
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      DOUBLE PRECISION,INTENT(INOUT) :: WFEXT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, ITYPE, IVAD, 
     .        NEL,KAD,NG,K,
     .        II,IPT,JJ,NPF,
     .        NS,N,INOD,
     .        IPRT,IPPV,J,M,JNOD,IMPOSE,JMPOSE,
     .        IMAT,IFPRES,NP,JMPOSE2,NN,KK(6)
      my_real
     .       PENTP,PN,PSHFT,PX,
     .       XI,YI,ZI,XJ,YJ,ZJ,DMIN,DD,VX,VY,VZ,
     .       PMIN,VN,VO,PO,RHOI,SSPI,PINFINI,LC,ALP,
     .       WFEXTT,DVOL
      TYPE(G_BUFEL_)  ,POINTER :: GBUF 
      LOGICAL :: lBOOL    
C=======================================================================
      WFEXTT= ZERO
      PINFINI = ZERO
      PN = ZERO
C-----------------------------------------------
C     1.  GENERAL OUTLET : REIMPOSE P=f(t) ou Pvoisin.
C-----------------------------------------------       
      DO NS=1,NSPHACT
       N=WASPACT(NS)
       IMPOSE=KXSP(2,N)/(NGROUP+1)
       IF(IMPOSE/=0)THEN
        ITYPE=ISPHIO(1,IMPOSE)
        IF(ITYPE==2)THEN
C------
C        general outlet.
         IPRT=ISPHIO(2,IMPOSE)
         IVAD=ISPHIO(4,IMPOSE)
C
         IFPRES=ISPHIO(6,IMPOSE)
         IF(IFPRES/=0)THEN
C------
C         P=f(t)
          NPF = (NPC(IFPRES+1)-NPC(IFPRES))/2
          II  = NPC(IFPRES)
          IF (TT<=PLD(II)) THEN
           PENTP=(PLD(II+3)-PLD(II+1))/(PLD(II+2)-PLD(II))
           PN   =PLD(II+1)+PENTP*(TT-PLD(II))
          ELSEIF (TT>=PLD(II+2*(NPF-1))) THEN
           JJ=II+2*(NPF-1)
           PENTP=(PLD(JJ+1)-PLD(JJ-1))/(PLD(JJ)-PLD(JJ-2))
           PN   =PLD(JJ+1)+MAX(-PLD(JJ+1),PENTP*(TT-PLD(JJ)))
          ELSE
           DO IPT=1,NPF-1
            IF (PLD(II)<=TT.AND.TT<=PLD(II+2)) THEN
             PENTP=(PLD(II+3)-PLD(II+1))/(PLD(II+2)-PLD(II))
             PN   =PLD(II+1)+PENTP*(TT-PLD(II))
             GOTO 140
            ENDIF
            II=II+2
           ENDDO
          ENDIF
 140      CONTINUE
          PN=PN*VSPHIO(IVAD+1)
          IPRT =IPARTSP(N)
          IMAT =IPART(1,IPRT)
          PMIN= PM(37,IMAT)
          PN=MAX(PN,PMIN)
         ELSE
C------
C         continuite.
          INOD=KXSP(3,N)
          XI=X(1,INOD)
          YI=X(2,INOD)
          ZI=X(3,INOD)
C
          IPPV=0
          DMIN=1.E+20
          DO J=1,KXSP(4,N)
           JNOD=IXSP(J,N)
           IF(JNOD>0)THEN
             M=NOD2SP(JNOD)
             IF(KXSP(2,M)>=0)THEN
              JMPOSE=KXSP(2,M)/(NGROUP+1)
              lBOOL=.FALSE.
              IF(JMPOSE==0)THEN
                lBOOL=.TRUE.
              ELSE
                IF(ISPHIO(1,JMPOSE)==1)lBOOL=.TRUE.
              ENDIF
              IF(lBOOL)THEN
               XJ  =X(1,JNOD)
               YJ  =X(2,JNOD)
               ZJ  =X(3,JNOD)
               DD  =(XI-XJ)*(XI-XJ)+(YI-YJ)*(YI-YJ)+(ZI-ZJ)*(ZI-ZJ)
               IF(DD<DMIN)THEN
                IPPV=JNOD
                DMIN=DD
               ENDIF
              ENDIF
             ENDIF
           ELSE
             NN = -JNOD
             JMPOSE = NINT(XSPHR(12,NN))
             IF(JMPOSE>0)THEN
               JMPOSE2=ISPHIO(1,JMPOSE)
             ELSE
                JMPOSE2=0
             ENDIF
             IF(JMPOSE2==0.OR.JMPOSE2==1)THEN
               XJ  =XSPHR(3,NN)
               YJ  =XSPHR(4,NN)
               ZJ  =XSPHR(5,NN)
               DD  =(XI-XJ)*(XI-XJ)+(YI-YJ)*(YI-YJ)+(ZI-ZJ)*(ZI-ZJ)
               IF(DD<DMIN)THEN
                IPPV=JNOD
                DMIN=DD
               ENDIF
             ENDIF
           ENDIF
          ENDDO
C
          IF(IPPV>0)THEN
            NP=NOD2SP(IPPV)
            NG=MOD(KXSP(2,NP),NGROUP+1)
            CALL INITBUF(IPARG    ,NG      ,                   
     2       MTN     ,NEL     ,NFT     ,KAD     ,ITY     ,    
     3       NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,    
     4       JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,    
     5       NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,    
     6       IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,    
     7       ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
!
            DO I=1,6
              KK(I) = NEL*(I-1)
            ENDDO
!
            GBUF => ELBUF_TAB(NG)%GBUF            
            K=NP-NFT
!
            PN=-( GBUF%SIG(KK(1)+K)
     .           +GBUF%SIG(KK(2)+K)
     .           +GBUF%SIG(KK(3)+K))*THIRD

          ELSEIF(IPPV<0)THEN !cas IPPV negatif on se sert des infos recuperes ds la routine de com

            PN=-( WAR(1,-IPPV)
     .           +WAR(2,-IPPV)
     .           +WAR(3,-IPPV))*THIRD
          ENDIF
         ENDIF
C
         NG=MOD(KXSP(2,N),NGROUP+1)
         CALL INITBUF(IPARG    ,NG      ,                   
     2       MTN     ,NEL     ,NFT     ,KAD     ,ITY     ,    
     3       NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,    
     4       JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,    
     5       NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,    
     6       IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,    
     7       ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
!
         DO I=1,6
           KK(I) = NEL*(I-1)
         ENDDO
!
         GBUF => ELBUF_TAB(NG)%GBUF            
         K=N-NFT
!
         PX=-( GBUF%SIG(KK(1)+K)          
     .        +GBUF%SIG(KK(2)+K)    
     .        +GBUF%SIG(KK(3)+K))*THIRD   
         GBUF%SIG(KK(1)+K)=GBUF%SIG(KK(1)+K)+PX-PN
         GBUF%SIG(KK(2)+K)=GBUF%SIG(KK(2)+K)+PX-PN
         GBUF%SIG(KK(3)+K)=GBUF%SIG(KK(3)+K)+PX-PN
         WA(1,N)=GBUF%SIG(KK(1)+K)
         WA(2,N)=GBUF%SIG(KK(2)+K)
         WA(3,N)=GBUF%SIG(KK(3)+K)
C
         INOD=KXSP(3,N)
         DVOL=SPBUF(12,N)/MAX(EM20,SPBUF(2,N))
     .       -SPBUF(12,N)/MAX(EM20,WA(10,N))
         WFEXTT=WFEXTT+(PN-PX)*DVOL
        ENDIF
       ENDIF
      ENDDO
C-----------------------------------------------
C     2.  SILENT BOUNDARY.
C-----------------------------------------------
      DO NS=1,NSPHACT
       N=WASPACT(NS)
       IMPOSE=KXSP(2,N)/(NGROUP+1)
       IF(IMPOSE/=0)THEN
        ITYPE=ISPHIO(1,IMPOSE)
        IF(ITYPE==3)THEN
         IVAD=ISPHIO(4,IMPOSE)
C-------
         INOD=KXSP(3,N)
         XI=X(1,INOD)
         YI=X(2,INOD)
         ZI=X(3,INOD)
C-------
         IPPV=0
         DMIN=1.E+20
         DO J=1,KXSP(4,N)
          JNOD=IXSP(J,N)
          IF(JNOD>0)THEN
            M=NOD2SP(JNOD)
            IF(KXSP(2,M)>=0)THEN
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
              IF(DD<DMIN)THEN
               IPPV=JNOD
               DMIN=DD
              ENDIF
             ENDIF
            ENDIF
          ELSE
             NN = -JNOD
             JMPOSE = NINT(XSPHR(12,NN))
             IF(JMPOSE>0)THEN
               JMPOSE2=ISPHIO(1,JMPOSE)
             ELSE
               JMPOSE2=0
             ENDIF
             IF(JMPOSE2==0.OR.JMPOSE2==1)THEN
               XJ  =XSPHR(3,NN)
               YJ  =XSPHR(4,NN)
               ZJ  =XSPHR(5,NN)
               DD  =(XI-XJ)*(XI-XJ)+(YI-YJ)*(YI-YJ)+(ZI-ZJ)*(ZI-ZJ)
               IF(DD<DMIN)THEN
                IPPV=JNOD
                DMIN=DD
               ENDIF
             ENDIF
          ENDIF
         ENDDO
C-------
         IFPRES=ISPHIO(6,IMPOSE)
         IF(IFPRES/=0)THEN
          NPF = (NPC(IFPRES+1)-NPC(IFPRES))/2
          II  = NPC(IFPRES)
          IF (TT<=PLD(II)) THEN
           PENTP=(PLD(II+3)-PLD(II+1))/(PLD(II+2)-PLD(II))
           PINFINI   =PLD(II+1)+PENTP*(TT-PLD(II))
          ELSEIF (TT>=PLD(II+2*(NPF-1))) THEN
           JJ=II+2*(NPF-1)
           PENTP=(PLD(JJ+1)-PLD(JJ-1))/(PLD(JJ)-PLD(JJ-2))
           PINFINI   =PLD(JJ+1)+MAX(-PLD(JJ+1),PENTP*(TT-PLD(JJ)))
          ELSE
           DO IPT=1,NPF-1
            IF (PLD(II)<=TT.AND.TT<=PLD(II+2)) THEN
             PENTP=(PLD(II+3)-PLD(II+1))/(PLD(II+2)-PLD(II))
             PINFINI   =PLD(II+1)+PENTP*(TT-PLD(II))
             GOTO 240
            ENDIF
            II=II+2
           ENDDO
 240       CONTINUE
          ENDIF
          PINFINI=PINFINI*VSPHIO(IVAD+1)
         ELSE
          PINFINI=VSPHIO(IVAD+1)
         ENDIF
C------
         VX=V(1,INOD)
         VY=V(2,INOD)
         VZ=V(3,INOD)
         VN=VNORMAL(1,N)*VX+VNORMAL(2,N)*VY+VNORMAL(3,N)*VZ

         IF(VN>=ZERO)THEN
          RHOI=SPBUF(2,N)
          SSPI=WA(8,N)
C
          IPRT =IPARTSP(N)
          IMAT =IPART(1,IPRT)
          PMIN= PM(37,IMAT)
C
          VO=SPHVELN(1,N)
          PO=SPHVELN(2,N)
          PINFINI=PINFINI-HALF*RHOI*VN*VN
          LC     =VSPHIO(IVAD+2)
          ALP    =HALF*SSPI/MAX(EM30,LC)*DT1
          PN=PO+(ONE - ALP)*RHOI*SSPI*(VN-VO)+ALP*(PINFINI-PO)
          PN=MAX(PN,PMIN)
C
          NG=MOD(KXSP(2,N),NGROUP+1)
          CALL INITBUF(IPARG    ,NG      ,                   
     2       MTN     ,NEL     ,NFT     ,KAD     ,ITY     ,    
     3       NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,    
     4       JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,    
     5       NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,    
     6       IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,    
     7       ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
!
          DO I=1,6
            KK(I) = NEL*(I-1)
          ENDDO
!
          GBUF => ELBUF_TAB(NG)%GBUF            
          K=N-NFT
!
          PX=-( GBUF%SIG(KK(1)+K)
     .         +GBUF%SIG(KK(2)+K)
     .         +GBUF%SIG(KK(3)+K))*THIRD

          GBUF%SIG(KK(1)+K)=GBUF%SIG(KK(1)+K)+PX-PN
          GBUF%SIG(KK(2)+K)=GBUF%SIG(KK(2)+K)+PX-PN
          GBUF%SIG(KK(3)+K)=GBUF%SIG(KK(3)+K)+PX-PN
          WA(1,N)=GBUF%SIG(KK(1)+K)
          WA(2,N)=GBUF%SIG(KK(2)+K)
          WA(3,N)=GBUF%SIG(KK(3)+K)
C
          DVOL=SPBUF(12,N)/MAX(EM20,SPBUF(2,N))
     .        -SPBUF(12,N)/MAX(EM20,WA(10,N))
          WFEXTT=WFEXTT+(PN-PX)*DVOL
         ELSE
          NG=MOD(KXSP(2,N),NGROUP+1)
          CALL INITBUF(IPARG    ,NG      ,                   
     2       MTN     ,NEL     ,NFT     ,KAD     ,ITY     ,    
     3       NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,    
     4       JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,    
     5       NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,    
     6       IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,    
     7       ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
!
          DO I=1,6
            KK(I) = NEL*(I-1)
          ENDDO
!
          GBUF => ELBUF_TAB(NG)%GBUF            
          K=N-NFT
!
          PN=-( GBUF%SIG(KK(1)+K)
     .         +GBUF%SIG(KK(2)+K)
     .         +GBUF%SIG(KK(3)+K))*THIRD
         ENDIF
         SPHVELN(1,N)= VN
         SPHVELN(2,N)= PN
        ENDIF
       ENDIF
      ENDDO
C-------------------------------------------
!$OMP ATOMIC
      WFEXT=WFEXT+WFEXTT
C-------------------------------------------
      RETURN
      END
