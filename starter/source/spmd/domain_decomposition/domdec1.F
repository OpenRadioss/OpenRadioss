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
Chd|  DOMDEC1                       source/spmd/domain_decomposition/domdec1.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        DD_ANI                        source/output/anim/dd_ani.F   
Chd|        DD_FR                         source/spmd/domain_decomposition/domdec1.F
Chd|        DD_FRX                        source/spmd/domain_decomposition/domdec1.F
Chd|        DD_FR_2                       source/spmd/domain_decomposition/domdec1.F
Chd|====================================================================
      SUBROUTINE DOMDEC1(
     1     IPARG ,IXS   ,IXQ        ,IXC   ,IXT    ,
     2     IXP   ,IXR   ,IXUR       ,IXTG  ,DD_IAD ,
     3     X     ,DD_IAD_PREV,IXS10 ,IXS20  ,
     4     IXS16 ,KXX   ,IXX        ,KXSP  ,IXSP   ,
     5     CEPSP ,IXTG6 )
C 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "vect01_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr12_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "com10_c.inc"
#include      "scr23_c.inc"
#include      "sphcom.inc"
#include      "scr14_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*), IXS(NIXS,*), IXQ(NIXQ,*) ,
     .        IXC(NIXC,*) , IXT(NIXT,*), IXTG(NIXTG,*) ,
     .        IXP(NIXP,*) , IXR(NIXR,*) , IXUR(NIXUR,*),
     .        DD_IAD(NSPMD+1,*),
     .        DD_IAD_PREV(NSPMD+1,*),
     .        IXS10(6,*),IXS20(12,*),IXS16(8,*), IXTG6(4,*),
     .        KXX(NIXX,*), IXX(*), KXSP(NISP,*), IXSP(*), CEPSP(*)
      my_real
     .        X(3,NUMNOD)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
       INTEGER  NLOCAL
       EXTERNAL NLOCAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NEL, DIM, NG, I,ICON, P, ISOLNOD, ICNOD
C-----------------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------------
C
C phase 1 : dd_iad
C
      DO I = 1, NSPGROUP
        DO P = 1, NSPMD+1
          DD_IAD(P,I) = DD_IAD_PREV(P,I)
        ENDDO
      ENDDO
C                
C phase 2 : frontiere 
C
      DO NG=1,NGROUP
        ITY = IPARG(5,NG)
        NEL = IPARG(2,NG)
        NFT = IPARG(3,NG)
        P   = IPARG(32,NG)+1
        IF(ITY==1) THEN
          ICON = 8
          DIM  = NIXS
          CALL DD_FR(IXS(1,NFT+1),P,NEL,ICON,DIM)
          IF(CODVERS>=42) THEN
            ISOLNOD = IPARG(28,NG)
            IF (ISOLNOD==10) THEN
              ICON = 6
              DIM = 6
C
              CALL DD_FR_2(
     .          IXS10(1,1+NFT-NUMELS8),P,NEL,
     .          ICON,DIM)
            ELSEIF(ISOLNOD==20) THEN
              ICON = 12
              DIM = 12
C
              CALL DD_FR_2(
     .          IXS20(1,1+NFT-NUMELS8-NUMELS10),P,NEL,
     .          ICON,DIM)
            ELSEIF(ISOLNOD==16) THEN
              ICON = 8
              DIM = 8
C
              CALL DD_FR_2(
     .          IXS16(1,1+NFT-NUMELS8-NUMELS10-NUMELS20),P,NEL,
     .          ICON,DIM)
            ENDIF
          ENDIF
        ELSE IF(ITY==2) THEN
          ICON = 4
          DIM  = NIXQ
          CALL DD_FR(IXQ(1,NFT+1),P,NEL,ICON,DIM)
        ELSE IF(ITY==3) THEN
          ICON = 4
          DIM  = NIXC
          CALL DD_FR(IXC(1,NFT+1),P,NEL,ICON,DIM)
        ELSE IF(ITY==4) THEN
          ICON = 2
          DIM  = NIXT
          CALL DD_FR(IXT(1,NFT+1),P,NEL,ICON,DIM)
        ELSE IF(ITY==5) THEN
          ICON = 3
          DIM  = NIXP
          CALL DD_FR(IXP(1,NFT+1),P,NEL,ICON,DIM)
        ELSE IF(ITY==6) THEN
          ICON = 3
          DIM  = NIXR
          CALL DD_FR(IXR(1,NFT+1),P,NEL,ICON,DIM)
        ELSE IF(ITY==7) THEN
          ICON = 3
          DIM  = NIXTG
          CALL DD_FR(IXTG(1,NFT+1),P,NEL,ICON,DIM)
          ICNOD = IPARG(11,NG)
          ICON = 3
          DIM = 4
          IF(ICNOD==6)THEN
C shift de 1 sur ixtg6
            CALL DD_FR(
     .          IXTG6(4,1+NFT-NUMELTG+NUMELTG6),P,NEL,
     .          ICON,DIM)
          END IF
        ELSE IF(ITY==50) THEN
          ICON = 4
          DIM  = NIXUR
          CALL DD_FR(IXUR(1,NFT+1),P,NEL,ICON,DIM)
        ELSE IF(ITY==51) THEN
          ICON = 1
          DIM  = NISP
C kxsp decale pour retrouver KXSP(3) = no noeud
          CALL DD_FR(KXSP(2,NFT+1),P,NEL,ICON,DIM)
        ELSE IF(ITY==100) THEN
          DIM = NIXX
          CALL DD_FRX(KXX(1,NFT+1),IXX,P,NEL,DIM)
        ENDIF
      END DO
c      IF(NUMSPH>0)THEN
c        DO PP = 1, NSPMD
c          DO P = 1, NSPMD
c            DOMAINE(PP,P) = 0
c          END DO
c        END DO
c        DO I = 1, NUMSPH
c          N = KXSP(3,I)
c          PP = CEPSP(I)+1
c          DO P = 1, NSPMD
c            IF(MOD(FRONT(N,P),10)==1) THEN
c              DOMAINE(PP,P) = DOMAINE(PP,P) + 1
c            END IF
c          END DO
c       END DO
c       DO P = 1, NSPMD
c         print *,P,'value :',(DOMAINE(P,PP),PP=1,NSPMD)
c       END DO
c      END IF

C              3) Ecrire le fichier Anim de la DD
C  a condition que imach==3 !
      IF (IMACH==3.AND.DECANI==1.AND.ANIM_VERS<40) THEN
        CALL DD_ANI(X     ,IXS   ,IXQ  ,IXC  ,
     .              IXT   ,IXP   ,IXR  ,IXTG ,
     .              IPARG ,KXSP  ,IXSP )
      ENDIF

      RETURN
      END

Chd|====================================================================
Chd|  DD_FR                         source/spmd/domain_decomposition/domdec1.F
Chd|-- called by -----------
Chd|        DOMDEC1                       source/spmd/domain_decomposition/domdec1.F
Chd|-- calls ---------------
Chd|        IFRONTPLUS                    source/spmd/node/frontplus.F  
Chd|====================================================================
      SUBROUTINE DD_FR(IX,P,NEL,ICON,DIM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICON,NEL,DIM,P,K,CPT
      INTEGER IX(DIM,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N
C-----------------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------------
C         Phase de comptage des noeuds frontieres
C         determiner les noeuds utilises
C         mettre a 1 la FRONTIERE(sd) correspondante
      DO I=1,ICON
        DO J=1,NEL
          N = IX(1+I,J)
          IF(N/=0) THEN
	    CALL IFRONTPLUS(N,P)
          ENDIF
        END DO
      END DO

      RETURN
      END

Chd|====================================================================
Chd|  DD_FRX                        source/spmd/domain_decomposition/domdec1.F
Chd|-- called by -----------
Chd|        DOMDEC1                       source/spmd/domain_decomposition/domdec1.F
Chd|-- calls ---------------
Chd|        IFRONTPLUS                    source/spmd/node/frontplus.F  
Chd|====================================================================
      SUBROUTINE DD_FRX(KXX,IXX,P,NEL,DIM)
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICON,NEL,DIM,P
      INTEGER KXX(DIM,*),IXX(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,ADDX
C-----------------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------------
C         Phase de comptage des noeuds frontieres
C         determiner les noeuds utilises
C         mettre a 1 la FRONTIERE(sd) correspondante
      DO I=1,NEL
          ICON = KXX(3,I)
          ADDX = KXX(4,I)
        DO J=1,ICON
          N = IXX(ADDX-1+J)
          IF(N/=0) THEN
	    CALL IFRONTPLUS(N,P)
          ENDIF
        END DO
      END DO

      RETURN
      END
Chd|====================================================================
Chd|  DD_FR_2                       source/spmd/domain_decomposition/domdec1.F
Chd|-- called by -----------
Chd|        DOMDEC1                       source/spmd/domain_decomposition/domdec1.F
Chd|-- calls ---------------
Chd|        IFRONTPLUS                    source/spmd/node/frontplus.F  
Chd|====================================================================
      SUBROUTINE DD_FR_2(IX,P,NEL,ICON,DIM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICON,NEL,DIM,P
      INTEGER IX(DIM,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N
C-----------------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------------
C         Phase de comptage des noeuds frontieres
C         determiner les noeuds utilises
C         mettre a 1 la FRONTIERE(sd) correspondante
      DO I=1,ICON
        DO J=1,NEL
          N = IX(I,J)
          IF(N/=0) THEN
            CALL IFRONTPLUS(N,P)
          ENDIF
        END DO
      END DO

      RETURN
      END



