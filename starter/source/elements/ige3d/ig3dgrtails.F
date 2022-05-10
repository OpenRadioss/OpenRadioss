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
Chd|  IG3DGRTAILS                   source/elements/ige3d/ig3dgrtails.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        ZEROIN                        source/system/zeroin.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE IG3DGRTAILS(
     1    KXIG3D    ,IPARG  ,GEO     ,EADD,
     2    ND     ,DD_IAD ,IDX     ,LB_MAX, INUM,
     3    INDEX  ,CEP    ,IPARTIG3D  ,ITR1,   IGRSURF,
     4    IXIG3D ,
     5    IGEO ,PM   ,NIGE,   KNOTLOCEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
C-----------------------------------------------
C            A R G U M E N T S
C-----------------------------------------------
C     KXIG3D(NIXIG3D,NUMELIG3D)  TABLEAU CONECS+PID+NOS RESSORTS    E
C     IPARG(NPARG,NGROUP)TABLEAU DES CARACTERISTIQUES DES GROUPES   E/S
C     GEO(NPROPG,NUMGEO) TABLEAU DES CARACS DES PID                 E
C     EADD(NUMELIG3D)       TABLEAU DES ADRESEES DANS IDAM CHGT DAMIER E 
C     DD_IAD             TABLEAU DE LA DD EN SUPER GROUPES          S
C     INDEX(NUMELIG3D)      TABLEAU DE TRAVAIL                         E/S
C     INUM (9*NUMELIG3D)      TABLEAU DE TRAVAIL                         E/S
C     CEP(NUMELIG3D)        TABLEAU DE TRAVAIL                         E/S
C     ITR1(NUMELIG3D)        TABLEAU DE TRAVAIL                         E/S
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "analyse_name.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com10_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "vect01_c.inc"
#include      "scr17_c.inc"
#include      "tabsiz_c.inc"
#include      "ige3d_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXIG3D(NIXIG3D,*),IPARG(NPARG,*),EADD(*),
     .        ND, DD_IAD(NSPMD+1,*),IDX,IGEO(NPROPGI,*),
     .        LB_MAX, INUM(NIXIG3D+1,*), INDEX(*),CEP(*),
     .        IPARTIG3D(*), ITR1(*),NIGE(*)
      my_real
     .   GEO(NPROPG,*),PM(NPROPM,*),KNOTLOCEL(*)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGR1, NG, ISSN, MTNN, I, NE1, N, NFIX,
     .        PID, NEL_PREC, LB_L, P, NEL,NB,
     .        MODE, WORK(70000),NN,IAD1,NGROU, J,MID,IETYP,
     .        MT,IXIG3D(*),NUVAR,NUVARN,NXVIE,NXVIN,INND,II,inno,
     .        NGP(NSPMD+1)
      my_real
     .    KNOTLOCELINDX(SKNOTLOCEL)
C
      DATA NXVIE/3/, NXVIN/0/
C----------------------------------------------------------
      NPT = 1
C----------------------------------------------------------
      NGR1 = NGROUP + 1
C
C phase 1 : decompostition canonique
C
      IDX=IDX+ND*(NSPMD+1)
      CALL ZEROIN(1,ND*(NSPMD+1),DD_IAD(1,NSPGROUP+1))
      NFT = 0
C initialisation dd_iad
      DO N=1,ND
       DO P=1,NSPMD+1
         DD_IAD(P,NSPGROUP+N) = 0
       END DO
      ENDDO

      DO N=1,ND
        NEL = EADD(N+1)-EADD(N)
C
        DO I = 1, NEL 
          INDEX(I) = I
          INUM(1,I)=IPARTIG3D(NFT+I)
          DO J=1,NIXIG3D
            INUM(J+1,I)=KXIG3D(J,NFT+I)
          ENDDO
          DO J=1,6
            KNOTLOCELINDX((I-1)*6+J)=KNOTLOCEL((NFT+I-1)*6+J)
          ENDDO
        ENDDO
      
        MODE=0
        CALL MY_ORDERS( MODE, WORK, CEP(NFT+1), INDEX, NEL , 1)
        DO I = 1, NEL
          IPARTIG3D(I+NFT)=INUM(1,INDEX(I))
          DO J=1,6
            KNOTLOCEL((I+NFT-1)*6+J)=KNOTLOCELINDX((INDEX(I)-1)*6+J)
          ENDDO 
          DO J=1,NIXIG3D
            KXIG3D(J,I+NFT)=INUM(J+1,INDEX(I))
          ENDDO

          ITR1(NFT+INDEX(I)) = NFT+I
        ENDDO
C dd-iad
        P = CEP(NFT+INDEX(1))
        NB = 1
        DO I = 2, NEL
          IF (CEP(NFT+INDEX(I)).NE.P) THEN
            DD_IAD(P+1,NSPGROUP+N) = NB
            NB = 1
            P = CEP(NFT+INDEX(I))
          ELSE
            NB = NB + 1
          ENDIF
        ENDDO
        DD_IAD(P+1,NSPGROUP+N) = NB
        DO P = 2, NSPMD
          DD_IAD(P,NSPGROUP+N) = DD_IAD(P,NSPGROUP+N)
     .                         + DD_IAD(P-1,NSPGROUP+N)
        ENDDO
        DO P = NSPMD+1,2,-1
          DD_IAD(P,NSPGROUP+N) = DD_IAD(P-1,NSPGROUP+N)+1
        ENDDO
        DD_IAD(1,NSPGROUP+N) = 1
C          
C maj CEP
C
        DO I = 1, NEL
          INDEX(I) = CEP(NFT+INDEX(I))          
        ENDDO
        DO I = 1, NEL
          CEP(NFT+I) = INDEX(I)          
        ENDDO
        NFT = NFT + NEL
      ENDDO  
C phase 2 : bornage en groupe de mvsiz
C ngroup est global, iparg est global mais organise en fonction de dd
C
      DO 300 N=1,ND
       NFT = 0
       LB_L = LBUFEL
       DO P = 1, NSPMD
        NGP(P)=0
        NEL = DD_IAD(P+1,NSPGROUP+N)-DD_IAD(P,NSPGROUP+N)
        IF (NEL.GT.0) THEN
         NEL_PREC = DD_IAD(P,NSPGROUP+N)-DD_IAD(1,NSPGROUP+N)
         NGP(P)=NGROUP
         NG  = (NEL-1)/NVSIZ + 1
         DO 220 I=1,NG
C xgroup global
          NGROUP=NGROUP+1
          II = EADD(N)+NFT
          MID = KXIG3D(1,II)
          PID =  KXIG3D(2,II)
          innd = KXIG3D(3,II)
          MTNN= NINT(PM(19,ABS(KXIG3D(1,II))))
          IETYP = 101
          GEO(8,PID)=IETYP + EM01
          JALE=NINT(PM(72,MID))
          JLAG=0
          IF(JALE == 0.AND.MTNN.NE.18)JLAG=1
          JEUL=0
          IF(JALE == 2)THEN
            JALE=0
            JEUL=1
C foam + air
          ELSEIF(JALE == 3 .AND. MTNN == 77) THEN
            JLAG=1          
          ENDIF
          IF(MTNN.NE.50)JTUR=NINT(PM(70,MID)) 
          JTHE=NINT(PM(71,MID))

C
          CALL ZEROIN(1,NPARG,IPARG(1,NGROUP))
C
          NE1 = MIN( NVSIZ, NEL + NEL_PREC - NFT) 
          NUVAR =NINT( GEO(25,PID))
          NUVARN=NINT( GEO(35,PID))

          IPARG(1,NGROUP) = MTNN
          IPARG(2,NGROUP) = NE1
          IPARG(3,NGROUP) = II-1
          IPARG(4,NGROUP) = 1
          IPARG(5,NGROUP) = IETYP
          IPARG(6,NGROUP) = NPT
          IPARG(7,NGROUP) = JALE
          IPARG(11,NGROUP)= JEUL
          IPARG(12,NGROUP)= JTUR
          IPARG(13,NGROUP)= JTHE
          IPARG(14,NGROUP)= JLAG
          IPARG(75,NGROUP) = innd
          IPARG(62,NGROUP) = PID
          IPARG(38,NGROUP) = IGEO(11,PID)
          IPARG(56,NGROUP) = IGEO(41,PID)
          IPARG(57,NGROUP) = IGEO(42,PID)
          IPARG(58,NGROUP) = IGEO(43,PID) 
C
c          LBUFEL= IPARG(4,NGROUP)+NE1*
c     .            (NXVIE+NUVAR+INND*(NXVIN+NUVARN))-1
C reperage groupe/processeur
          IPARG(32,NGROUP)= P-1
          NFT = NFT + NE1
  220    CONTINUE
         NGP(P)=NGROUP-NGP(P)
        ENDIF
       ENDDO
       LB_L = LBUFEL - LB_L
       LB_MAX = MAX(LB_MAX,LB_L)
C DD_IAD => nb groupes par sous domaine
       NGP(NSPMD+1)=0
       DO P = 1, NSPMD
         NGP(NSPMD+1)=NGP(NSPMD+1)+NGP(P)
         DD_IAD(P,NSPGROUP+N)=NGP(P)
       END DO
       DD_IAD(NSPMD+1,NSPGROUP+N)=NGP(NSPMD+1)
C
  300 CONTINUE
C
      NSPGROUP = NSPGROUP + ND
C
C RENUMEROTATION POUR SURFACES
C
      DO I=1,NSURF ! mettre un if pour ne pas changer les faces EF classiques
        NN=IGRSURF(I)%NSEG_IGE
        DO J=1,NN
          IF(IGRSURF(I)%ELTYP_IGE(J) == 101)
     .       IGRSURF(I)%ELEM_IGE(J) = ITR1(IGRSURF(I)%ELEM_IGE(J))
        ENDDO
      ENDDO
C
      DO I=1,NUMFAKENODIGEO
        NIGE(I)=ITR1(NIGE(I))
      ENDDO
C 
      WRITE(IOUT,1000)
      WRITE(IOUT,1001)(N,IPARG(1,N),IPARG(2,N),IPARG(3,N)+1,
     +                IPARG(4,N),IPARG(5,N),
     +              N=NGR1,NGROUP)
c      WRITE(IOUT,1002) LBUFEL
C
 1000 FORMAT(10X,' 3D - ISO-GEOMETRIC ELEMENT GROUPS '/
     +       10X,' ----------------------------------'/
     +' GROUP   ELEMENT    ELEMENT   FIRST    BUFFER   ELEMENT  '/
     +'         MATERIAL   NUMBER    ELEMENT  ADDRESS  TYPE     '/)
 1001 FORMAT(6(1X,I7,1X))
 1002 FORMAT(' BUFFER LENGTH : ',I10 )
C

      RETURN
      END
