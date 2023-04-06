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
Chd|  BINIT2                        source/ale/bimat/binit2.F     
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ATHERI                        source/ale/atheri.F           
Chd|        ATURI2                        source/ale/ale2d/aturi2.F     
Chd|        BIMAT2                        source/ale/bimat/bimat2.F     
Chd|        EDLEN2                        source/ale/ale2d/edlen2.F     
Chd|        MATINI                        source/materials/mat_share/matini.F
Chd|        QCOOR2                        source/elements/solid_2d/quad/qcoor2.F
Chd|        QDLEN2                        source/elements/solid_2d/quad/qdlen2.F
Chd|        QMASI2                        source/elements/solid_2d/quad/qmasi2.F
Chd|        QMASI2B                       source/elements/solid_2d/quad/qmasi2b.F
Chd|        QVOLI2                        source/elements/solid_2d/quad/qvoli2.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        DETONATORS_MOD                share/modules1/detonators_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE BINIT2(ELBUF_STR,MS      ,IXQ     ,PM      ,X       ,
     .                  DETONATORS,VEUL   ,ALE_CONNECTIVITY   ,IPARG   ,FILL    ,
     .                  SIGI    ,BUFMAT  ,NEL     ,
     .                  SKEW    ,MSQ      ,IPART   ,IPARTQ  ,
     .                  GEO     ,IGEO     ,IPM     ,
     .                  NSIGS   ,WMA      ,PTQUAD  ,NPF     ,TF      ,
     .                  IPARGG  ,ILOADP   ,FACLOAD ,PARTSAV ,V)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD  
      USE DETONATORS_MOD          
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "scry_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXQ(NIXQ,*), IPARG(*),
     .        NEL,IPART(LIPART1,*),IPARTQ(*),
     .        IPM(NPROPMI,*), PTQUAD(*), NSIGS, IGEO(*), NPF(*),
     .        IPARGG(*)
      my_real
     .   MS(*), PM(NPROPM,*), X(*), VEUL(10,*),
     .   FILL(NUMNOD,*), SIGI(NSIGS,*),SKEW(LSKEW,*),
     .   MSQ(*), GEO(*), WMA(*), BUFMAT(*), TF(*),
     .   PARTSAV(20,*) ,V(*)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
      INTEGER,INTENT(IN) :: ILOADP(SIZLOADP,*)
      my_real,INTENT(IN) :: FACLOAD(LFACLOAD,*)
      TYPE(DETONATOR_STRUCT_)::DETONATORS
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NF1, IMULT, MLW, IGTYP, IP,IBID
      INTEGER MAT(MVSIZ), NGL(MVSIZ), GAMA(6,MVSIZ)
      my_real
     .     X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .     Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .     Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .     AIRE(MVSIZ), DELTAX(MVSIZ),
     .     SY(MVSIZ), SZ(MVSIZ), TY(MVSIZ), TZ(MVSIZ)
      INTEGER PID(MVSIZ), IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ)
C-----------------------------------------------
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
      TYPE(BUF_MAT_) ,POINTER :: MBUF
C-----------------------------------------------
C=======================================================================
      GBUF => ELBUF_STR%GBUF
      LBUF => ELBUF_STR%BUFLY(1)%LBUF(1,1,1)
      IGTYP= IPARG(38)
      NF1  = NFT+1
      IBID = 0
c
      CALL QCOOR2(X,IXQ(1,NF1),NGL,MAT, 
     .     PID, IX1, IX2, IX3, IX4, 
     .     Y1, Y2, Y3, Y4, 
     .     Z1, Z2, Z3, Z4,
     .     SY, SZ, TY, TZ)
      CALL QVOLI2(GBUF%VOL,IXQ(1,NF1),
     .     NGL, AIRE, 
     .     Y1, Y2, Y3, Y4, 
     .     Z1, Z2, Z3, Z4)
      IF (JEUL/=0) THEN
        CALL QDLEN2(IPARG(63), 
     .        AIRE, DELTAX, 
     .        Y1, Y2, Y3, Y4, 
     .        Z1, Z2, Z3, Z4)
        CALL EDLEN2(VEUL(1,NF1), AIRE, DELTAX)
      ENDIF

C---------------------------------------
C     MULTIMATRIAUX 1
C---------------------------------------
      IMULT=JMULT
      JMULT=1
      LBUF => ELBUF_STR%BUFLY(JMULT)%LBUF(1,1,1)
      MBUF => ELBUF_STR%BUFLY(JMULT)%MAT(1,1,1)
      MTN  =  IPARG(25)

      DO I=LFT,LLT
        MAT(I)=NINT(PM(20+JMULT,IXQ(1,NFT+I)))
      ENDDO
      
      !------------------------------------------
      !     MULTIMATERIAUX POUCENTAGE DE REMPLISSAGE
      !------------------------------------------
      CALL BIMAT2(PM     ,MAT    ,GBUF%VOL  ,LBUF%FRAC ,FILL(1,1),
     .            LBUF%VOL,LBUF%OFF,IXQ(1,NF1))
      !----------------------------------------
      !     INITIALISATION GENERALE
      !----------------------------------------
      IP=1
      CALL MATINI(PM     ,IXQ    ,NIXQ       ,X      ,
     .            GEO    ,ALE_CONNECTIVITY  ,DETONATORS ,IPARG  ,
     .            SIGI   ,NEL    ,SKEW       ,IGEO   ,
     .            IPART  ,IPARTQ ,
     .            MAT    ,IPM    ,NSIGS  ,NUMQUAD   ,PTQUAD ,
     .            IP     ,NGL    ,NPF    ,TF        ,BUFMAT ,
     .            GBUF   ,LBUF   ,MBUF   ,ELBUF_STR ,ILOADP ,
     .            FACLOAD, DELTAX)
      !----------------------------------------
      !     INITIALISATION DE LA THERMIQUE ET TURBULENCE
      !----------------------------------------
      IF(JTHE/=0) CALL ATHERI(MAT,PM,LBUF%TEMP)
      IF(JTUR/=0) CALL ATURI2(IPARGG ,LBUF%RHO,PM,IXQ,X,
     .                          LBUF%RK,LBUF%RE, AIRE)
      !------------------------------------------
      !     INITIALISATION DE LA MATRICE DE MASSE
      !------------------------------------------
      IF(JLAG+JALE+JEUL/=0)
     .  CALL QMASI2(PM,MAT,MS,LBUF%VOL,MSQ(NF1),WMA,IPARTQ(NFT+1),PARTSAV, 
     .     IX1, IX2, IX3, IX4, X ,V)
      !---------------------------------------
      !     MULTIMATRIAUX 2
      !---------------------------------------
      IF(IMULT==1)RETURN
      JMULT=2
      LBUF => ELBUF_STR%BUFLY(JMULT)%LBUF(1,1,1)
      MBUF => ELBUF_STR%BUFLY(JMULT)%MAT(1,1,1)
      MTN  =  IPARG(26)

      NF1=NFT+1
      DO I=LFT,LLT
        MAT(I)=NINT(PM(20+JMULT,IXQ(1,NFT+I)))
      ENDDO
      !------------------------------------------
      !     MULTIMATERIAUX POUCENTAGE DE REMPLISSAGE
      !------------------------------------------
      CALL BIMAT2(PM     ,MAT    ,GBUF%VOL,LBUF%FRAC,FILL(1,2),
     .            LBUF%VOL,LBUF%OFF,IXQ(1,NF1))
      !----------------------------------------
      !     INITIALISATION GENERALE
      !----------------------------------------
      IP=1
      
      CALL MATINI(PM     ,IXQ    ,NIXQ      ,X      ,
     .            GEO    ,ALE_CONNECTIVITY  ,DETONATORS,IPARG     ,
     .            SIGI   ,NEL    ,SKEW      ,IGEO   ,
     .            IPART  ,IPARTQ ,
     .            MAT    ,IPM    ,NSIGS     ,NUMQUAD   ,PTQUAD ,
     .            IP     ,NGL    ,NPF       ,TF        ,BUFMAT ,
     .            GBUF   ,LBUF   ,MBUF      ,ELBUF_STR ,ILOADP ,
     .            FACLOAD, DELTAX)
      !----------------------------------------
      !     INITIALISATION DE LA THERMIQUE ET TURBULENCE
      !----------------------------------------
      IF (JTHE/=0) CALL ATHERI(MAT,PM, LBUF%TEMP)
      IF (JTUR/=0) CALL ATURI2(IPARGG ,LBUF%RHO,PM,IXQ,X,
     .                          LBUF%RK,LBUF%RE, AIRE)
      !------------------------------------------
      !     INITIALISATION DE LA MATRICE DE MASSE
      !------------------------------------------
      IF(JLAG+JALE+JEUL/=0)
     .  CALL QMASI2B(PM,MAT,MS,LBUF%VOL,MSQ(NF1),WMA,IPARTQ(NFT+1),PARTSAV,
     .     IX1, IX2, IX3, IX4 ,X ,V)
C-----------
      RETURN
      END
