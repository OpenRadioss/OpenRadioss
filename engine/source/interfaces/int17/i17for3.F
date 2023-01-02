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
Chd|  I17FOR3                       source/interfaces/int17/i17for3.F
Chd|-- called by -----------
Chd|        I17MAIN_PENA                  source/interfaces/int17/i17main_pena.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        I17LLL_ASS                    source/interfaces/int17/i17for3.F
Chd|        I17LLL_PENA                   source/interfaces/int17/i17for3.F
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_I17FROTS_PON             source/mpi/interfaces/spmd_i17frots_pon.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        ICONTACT_MOD                  share/modules/icontact_mod.F  
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I17FOR3(
     1   X      ,V       ,CANDN    ,CANDE        ,I_STOK  ,
     2   IXS    ,IXS16   ,EMINXM   ,NELES        ,NELEM   ,
     3   ITASK  ,A       ,ITIED    ,NINT         ,EMINXS  ,
     4   STIFN  ,FSKYI   ,ISKY     ,NME          ,NSE     ,
     5   FROTM  ,FROTS   ,KM       ,KS           ,FRIC    ,
     6   FSAV   ,FCONT   ,MS       ,NISKYFI      ,LSKYI17 ,
     7   NOINT  ,H3D_DATA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE ICONTACT_MOD
      USE MESSAGE_MOD
      USE H3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "warn_c.inc"
#include      "scr05_c.inc"
#include      "scr07_c.inc"
#include      "scr18_c.inc"
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "couple_c.inc"
#include      "parit_c.inc"
      COMMON /I17GLOBI/IE_MIN,IES_MIN
      COMMON /I17GLOBR/VIT_MIN
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I_STOK,ITASK,ITIED,NINT,NME,NSE,NISKYFI,LSKYI17,NOINT,
     .  CANDN(*),CANDE(*), ISKY(*),
     .  IXS(NIXS,*),IXS16(8,*),NELES(*)  ,NELEM(*) 
C     REAL
      my_real
     .  X(3,*),V(3,*),EMINXM(6,*),EMINXS(6,*),A(3,*),STIFN(*),
     .  FSKYI   ,FROTM(7,*),FROTS(7,*),KM(*),KS(*),FRIC(*), FSAV(*),
     .  FCONT(3,*), MS(*)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,IK,IE,IS,IC,NK,III(MVSIZ,17),LLT,NFT,LE,FIRST,LAST,
     .        I16,LES,IES,IIIS(MVSIZ,16),LEL(MVSIZ),LESL(MVSIZ),
     .        IE_MIN,IES_MIN, IERR1, IERR2
      my_real
     .   XX(MVSIZ,17),YY(MVSIZ,17),ZZ(MVSIZ,17),
     .   XXS(MVSIZ,16),YYS(MVSIZ,16),ZZS(MVSIZ,16),
     .   VIT_MIN
C-----------------------------------------------------------------------
C     boucle sur les candidats au contact   
C-----------------------------------------------------------------------
      VIT_MIN = ZERO
      IE_MIN  = 99999999
      IES_MIN = 99999999
      NSKYI17 = 0
      NRSKYI17 = 0
      IF(IPARIT/=0.AND.ITASK==0)THEN
        IERR1=0
        IERR2=0
        ALLOCATE(ISKYI17(LSKYI17),STAT=IERR2)
        IERR1 = IERR1+IERR2
        ALLOCATE(FSKYI17(LSKYI17,4),STAT=IERR2)
        IERR1 = IERR1+IERR2
        IF(NSPMD>1)THEN
          ALLOCATE(IRSKYI17(LSKYI17),STAT=IERR2)
          IERR1 = IERR1+IERR2
          ALLOCATE(FRSKYI17(4,LSKYI17),STAT=IERR2)
          IERR1 = IERR1+IERR2
        END IF
        IF(IERR1/=0)THEN
          CALL ANCMSG(MSGID=88,ANMODE=ANINFO,
     .                I1=NOINT)
          CALL ARRET(2)
        END IF
      END IF
C-----------------------------------------------------------------------
      CALL MY_BARRIER
C-----------------------------------------------------------------------
      FIRST = 1 + I_STOK * ITASK / NTHREAD
      LAST = I_STOK*(ITASK+1) / NTHREAD
      LLT = 0
      NFT=LLT+1
      DO IC=FIRST,LAST
        LE  = CANDE(IC)
        LES = CANDN(IC)
        IE  = NELEM(LE)
        IF(LES<=NSE)THEN      ! candidat interne en SPMD
         IES = NELES(LES)
C-----------------------------------------------------------------------
C       test si dans la boite   
C-----------------------------------------------------------------------
         IF(LE >0.AND.LES>0.AND.
     .           EMINXS(4,LES)>EMINXM(1,LE).AND.
     .           EMINXS(5,LES)>EMINXM(2,LE).AND.
     .           EMINXS(6,LES)>EMINXM(3,LE).AND.
     .           EMINXS(1,LES)<EMINXM(4,LE).AND.
     .           EMINXS(2,LES)<EMINXM(5,LE).AND.
     .           EMINXS(3,LES)<EMINXM(6,LE))THEN
          LLT = LLT+1
          LEL(LLT)=LE
          LESL(LLT)=LES
          DO K=1,4
C III 1,2,3,4,5,6,7,8, 9,10,11,12,13,14,15,16
            III(LLT,K)   =IXS(K+1,IE)
            III(LLT,K+4) =IXS(K+5,IE)
            III(LLT,K+8) =IXS16(K,IE-NUMELS8-NUMELS10-NUMELS20)
            III(LLT,K+12)=IXS16(K+4,IE-NUMELS8-NUMELS10-NUMELS20)
C IIIS 1,2,3,4,9,10,11,12, 5,6,7,8,13,14,15,16
            IIIS(LLT,K)  =IXS(K+1,IES)
            IIIS(LLT,K+8) =IXS(K+5,IES)
            IIIS(LLT,K+4)=IXS16(K,IES-NUMELS8-NUMELS10-NUMELS20)
            IIIS(LLT,K+12)=IXS16(K+4,IES-NUMELS8-NUMELS10-NUMELS20)
          ENDDO
          DO K=1,16
            I = III(LLT,K)
            XX(LLT,K)=X(1,I)
            YY(LLT,K)=X(2,I)
            ZZ(LLT,K)=X(3,I)
            I = IIIS(LLT,K)
            XXS(LLT,K)=X(1,I)
            YYS(LLT,K)=X(2,I)
            ZZS(LLT,K)=X(3,I)
          ENDDO
         END IF
        ELSE        !    complement frontiere SPMD
         IES = LES-NSE
C-----------------------------------------------------------------------
C       test si dans la boite   
C-----------------------------------------------------------------------
         IF(LE >0.AND.LES>0.AND.
     .           EMINXFI(NINT)%P(4,IES)>EMINXM(1,LE).AND.
     .           EMINXFI(NINT)%P(5,IES)>EMINXM(2,LE).AND.
     .           EMINXFI(NINT)%P(6,IES)>EMINXM(3,LE).AND.
     .           EMINXFI(NINT)%P(1,IES)<EMINXM(4,LE).AND.
     .           EMINXFI(NINT)%P(2,IES)<EMINXM(5,LE).AND.
     .           EMINXFI(NINT)%P(3,IES)<EMINXM(6,LE))THEN
          LLT = LLT+1
          LEL(LLT)=LE
          LESL(LLT)=-IES        ! reperage en negatif des candidats remote SPMD
#include "mic_lockon.inc"
          NSVFI(NINT)%P(IES) = -ABS(NSVFI(NINT)%P(IES))
#include "mic_lockoff.inc"
          DO K=1,4
C III 1,2,3,4,5,6,7,8, 9,10,11,12,13,14,15,16
            III(LLT,K)   =IXS(K+1,IE)
            III(LLT,K+4) =IXS(K+5,IE)
            III(LLT,K+8) =IXS16(K,IE-NUMELS8-NUMELS10-NUMELS20)
            III(LLT,K+12)=IXS16(K+4,IE-NUMELS8-NUMELS10-NUMELS20)
C IIIS 1,2,3,4,9,10,11,12, 5,6,7,8,13,14,15,16
            IIIS(LLT,K)   =K
            IIIS(LLT,K+8) =K+4
            IIIS(LLT,K+4) =K+8
            IIIS(LLT,K+12)=K+12
          ENDDO
          DO K=1,16
            I = III(LLT,K)
            XX(LLT,K)=X(1,I)
            YY(LLT,K)=X(2,I)
            ZZ(LLT,K)=X(3,I)
            I = IIIS(LLT,K)                ! I indique le no de noeud de 1 a 16
            XXS(LLT,K)=XFI17(NINT)%P(1,I,IES)
            YYS(LLT,K)=XFI17(NINT)%P(2,I,IES)
            ZZS(LLT,K)=XFI17(NINT)%P(3,I,IES)
          ENDDO
         END IF
        END IF
C-----------------------------------------------------------------------
C     calcul de  F par paquet de mvsiz   
C-----------------------------------------------------------------------
          IF(LLT==MVSIZ-1)THEN
            CALL I17LLL_PENA(
     1       LLT     ,V         ,STIFN     ,XX        ,FRIC    ,
     2       YY      ,ZZ        ,III       ,FSKYI     ,ISKY    ,
     3       A       ,X         ,ITIED     ,NINT      ,
     4       XXS     ,YYS       ,ZZS       ,IIIS      ,VIT_MIN ,
     5       LEL     ,LESL      ,IE_MIN    ,IES_MIN   ,ITASK   ,
     6       FROTM   ,FROTS     ,KM        ,KS        ,FSAV    ,
     7       FCONT   ,MS        ,NISKYFI   ,NOINT     ,H3D_DATA )
            NFT=LLT+1
            LLT = 0
          ENDIF
      ENDDO
C-----------------------------------------------------------------------
C     calcul de  F pour dernier paquet   
C-----------------------------------------------------------------------        
      IF(LLT/=0) CALL I17LLL_PENA(
     1       LLT     ,V         ,STIFN     ,XX        ,FRIC    ,
     2       YY      ,ZZ        ,III       ,FSKYI     ,ISKY    ,
     3       A       ,X         ,ITIED     ,NINT      ,
     4       XXS     ,YYS       ,ZZS       ,IIIS      ,VIT_MIN ,
     5       LEL     ,LESL      ,IE_MIN    ,IES_MIN   ,ITASK   ,
     6       FROTM   ,FROTS     ,KM        ,KS        ,FSAV    ,
     7       FCONT   ,MS        ,NISKYFI   ,NOINT     ,H3D_DATA )
C-----------------------------------------------------------------------
C     parallel Arithmetic for assembly (frots)
C-----------------------------------------------------------------------
      IF(IPARIT/=0) THEN
        CALL MY_BARRIER
        IF(ITASK==0)THEN
          IF(NSPMD>1)THEN
            CALL SPMD_I17FROTS_PON(
     1        NSKYI17 ,ISKYI17,FSKYI17,NRSKYI17,IRSKYI17,
     2        FRSKYI17,NINT   ,LSKYI17,NOINT)
            DEALLOCATE(IRSKYI17)
            DEALLOCATE(FRSKYI17)
          END IF
C 
          IF(NSKYI17>0)THEN
            CALL I17LLL_ASS(NSKYI17 ,ISKYI17 ,FSKYI17, FROTS, NSE,
     2                    LSKYI17 )
          END IF
          DEALLOCATE(ISKYI17)
          DEALLOCATE(FSKYI17)
        END IF
      END IF
C
      RETURN
      END
Chd|====================================================================
Chd|  I17LLL_PENA                   source/interfaces/int17/i17for3.F
Chd|-- called by -----------
Chd|        I17FOR3                       source/interfaces/int17/i17for3.F
Chd|-- calls ---------------
Chd|        I17LLL4_PENA                  source/interfaces/int17/i17for3.F
Chd|        I17MINI_PENA                  source/interfaces/int17/i17for3.F
Chd|        I17RST                        source/interfaces/int17/i17lagm.F
Chd|        I17VIT_PENA                   source/interfaces/int17/i17for3.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE I17LLL_PENA(
     .         LLT     ,V      ,STIFN   ,XX        ,FRIC   ,
     2         YY      ,ZZ     ,III     ,FSKYI     ,ISKY   ,
     3         A       ,X      ,ITIED   ,NINT      ,
     4         XXS     ,YYS    ,ZZS     ,IIIS      ,VIT_MIN,
     5         LE      ,LES    ,IE_MIN  ,IES_MIN   ,ITASK  ,
     6         FROTM   ,FROTS  ,KM      ,KS        ,FSAV   ,
     7         FCONT   ,MS     ,NISKYFI ,NOINT     ,H3D_DATA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE H3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LLT,ITIED,NINT ,IE_MIN,IES_MIN,NISKYFI,NOINT 
      INTEGER ITASK,
     .        III(MVSIZ,17),IIIS(MVSIZ,16),LE(*) ,LES(*), ISKY(*)
C     REAL
      my_real
     .  V(3,*),A(3,*),KM(*),KS(*),FRIC(*), MS(*)
      my_real
     .   XX(MVSIZ,17),YY(MVSIZ,17),ZZ(MVSIZ,17),X(3,*),
     .   XXS(MVSIZ,16) ,YYS(MVSIZ,16) ,ZZS(MVSIZ,16) ,VIT_MIN,
     .   STIFN(*) ,FSKYI ,FROTM(7,*),FROTS(7,*),FSAV(*) ,FCONT(3,*)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IK,NK,I1,I2,I3,I4,IAD,ICON,
     +        ICONT(MVSIZ,4)
      my_real
     .   VX,VY,VZ,VN,AA,VV,PENE
      my_real
     .   R_CM(MVSIZ),T_CM(MVSIZ),S_CM(MVSIZ),SI_S(MVSIZ,8),
     .   RI_S(MVSIZ,8),TI_S(MVSIZ,8),
     .   NX(MVSIZ), NY(MVSIZ), NZ(MVSIZ),
     .   NI_M(MVSIZ,17),NI_S(MVSIZ,8) ,
     .   R_1S(MVSIZ) ,R_2S(MVSIZ) ,T_1S(MVSIZ) ,T_2S(MVSIZ),
     .   R_1M(MVSIZ) ,R_2M(MVSIZ) ,T_1M(MVSIZ) ,T_2M(MVSIZ),
     .   R_3M(MVSIZ) ,R_4M(MVSIZ) ,T_3M(MVSIZ) ,T_4M(MVSIZ),
     .   R_CS(MVSIZ) ,S_CS(MVSIZ) ,T_CS(MVSIZ) ,VIT(MVSIZ),
     .   R_S(MVSIZ) ,T_S(MVSIZ)   ,AREA(MVSIZ) ,AREA_TOT(MVSIZ),
     .   AREA_EL(MVSIZ)        
C-----------------------------------------------
C      calcul de r,s,t
C-----------------------------------------------
C-----------------------------------------------
C      noeud centrale face 1 2 3 4
C-----------------------------------------------
      DO I=1,LLT
        XX(I,17) = HALF *(XXS(I,5) +XXS(I,6) +XXS(I,7) +XXS(I,8))
     .           - FOURTH*(XXS(I,1) +XXS(I,2) +XXS(I,3) +XXS(I,4))
        YY(I,17) = HALF *(YYS(I,5) +YYS(I,6) +YYS(I,7) +YYS(I,8))
     .           - FOURTH*(YYS(I,1) +YYS(I,2) +YYS(I,3) +YYS(I,4))
        ZZ(I,17) = HALF *(ZZS(I,5) +ZZS(I,6) +ZZS(I,7) +ZZS(I,8))
     .           - FOURTH*(ZZS(I,1) +ZZS(I,2) +ZZS(I,3) +ZZS(I,4))     
      ENDDO
      CALL I17RST(LLT   ,RI_S(1,1),SI_S(1,1),TI_S(1,1),NI_M  ,
     2            XX    ,YY       ,ZZ       )
C-----------------------------------------------
C      noeud centrale face 5 6 7 8
C-----------------------------------------------
      DO I=1,LLT
        XX(I,17) = HALF *(XXS(I,13)+XXS(I,14)+XXS(I,15)+XXS(I,16))
     .           - FOURTH*(XXS(I,9) +XXS(I,10)+XXS(I,11)+XXS(I,12))
        YY(I,17) = HALF *(YYS(I,13)+YYS(I,14)+YYS(I,15)+YYS(I,16))
     .           - FOURTH*(YYS(I,9) +YYS(I,10)+YYS(I,11)+YYS(I,12))
        ZZ(I,17) = HALF *(ZZS(I,13)+ZZS(I,14)+ZZS(I,15)+ZZS(I,16))
     .           - FOURTH*(ZZS(I,9) +ZZS(I,10)+ZZS(I,11)+ZZS(I,12))     
      ENDDO
      CALL I17RST(LLT   ,RI_S(1,2),SI_S(1,2),TI_S(1,2),NI_M  ,
     2            XX    ,YY       ,ZZ       )
C-----------------------------------------------
C      choix face 1 2 3 4 face 5 6 7 8 cote second
C-----------------------------------------------
      DO I=1,LLT
        IF(ABS(SI_S(I,1))<=ABS(SI_S(I,2)))THEN
C         face 1 2 3 4
          S_CS(I) = -ONE
        ELSE
C         face 5 6 7 8
          S_CS(I) =  ONE
          IIIS(I,1) = IIIS(I,12)
          IIIS(I,2) = IIIS(I,11)
          IIIS(I,3) = IIIS(I,10)
          IIIS(I,4) = IIIS(I, 9)
          IIIS(I,5) = IIIS(I,15)
          IIIS(I,6) = IIIS(I,14)
          IIIS(I,7) = IIIS(I,13)
          IIIS(I,8) = IIIS(I,16)
C
          XXS(I,1) = XXS(I,12)
          XXS(I,2) = XXS(I,11)
          XXS(I,3) = XXS(I,10)
          XXS(I,4) = XXS(I, 9)
          XXS(I,5) = XXS(I,15)
          XXS(I,6) = XXS(I,14)
          XXS(I,7) = XXS(I,13)
          XXS(I,8) = XXS(I,16)
C
          YYS(I,1) = YYS(I,12)
          YYS(I,2) = YYS(I,11)
          YYS(I,3) = YYS(I,10)
          YYS(I,4) = YYS(I, 9)
          YYS(I,5) = YYS(I,15)
          YYS(I,6) = YYS(I,14)
          YYS(I,7) = YYS(I,13)
          YYS(I,8) = YYS(I,16)
C
          ZZS(I,1) = ZZS(I,12)
          ZZS(I,2) = ZZS(I,11)
          ZZS(I,3) = ZZS(I,10)
          ZZS(I,4) = ZZS(I, 9)
          ZZS(I,5) = ZZS(I,15)
          ZZS(I,6) = ZZS(I,14)
          ZZS(I,7) = ZZS(I,13)
          ZZS(I,8) = ZZS(I,16)
C
        ENDIF
      ENDDO
C-----------------------------------------------
C      calcul de SI_S=s(relatif element main) 
c      des 8 noeuds de la face de l'element second
C-----------------------------------------------
      DO J=1,8
        DO I=1,LLT
          XX(I,17) = XXS(I,J)
          YY(I,17) = YYS(I,J)
          ZZ(I,17) = ZZS(I,J)
        ENDDO
        CALL I17RST(LLT   ,RI_S(1,J),SI_S(1,J),TI_S(1,J),NI_M  ,
     2              XX    ,YY    ,ZZ    )
      ENDDO
C-----------------------------------------------
C      calcul du point de distance min
C      dSI_S/dr = 0 dSI_S/dt = 0     (r,t de l'element second) 
C-----------------------------------------------
c                                                                                                                                                                           
c   il est possible de calculer la surface effective 
c   de contact pour ponderer la force
c   => pas de discontinuit    si contact sur 2 elements
c
c   a faire dans I17MINI
c
c   possible aussi de calculer la pression de Hertz
c                                                                                                                                                                           
      CALL I17MINI_PENA(
     1             LLT   ,R_CS  ,S_CS  ,T_CS  ,RI_S  ,SI_S   ,
     2             TI_S  ,NI_S  ,XXS   ,YYS   ,ZZS   ,XX     ,
     3             YY    ,ZZ    ,R_CM  ,S_CM  ,T_CM  ,NX     ,
     4             NY    ,NZ    ,R_1S  ,R_2S  ,T_1S  ,T_2S   ,
     5             R_1M  ,R_2M  ,R_3M  ,R_4M  ,T_1M  ,T_2M   ,
     6             T_3M  ,T_4M  ,ICONT ,AREA  ,AREA_TOT,AREA_EL)
C-----------------------------------------------
C      choix face 1 2 3 4 face 5 6 7 8 cote main
C-----------------------------------------------
      DO I=1,LLT
        IF(S_CM(I)<=0.)THEN
C         face 1 2 3 4
          III(I,5) = III(I,9)
          III(I,6) = III(I,10)
          III(I,7) = III(I,11)
          III(I,8) = III(I,12)
        ELSE
C         face 5 6 7 8
          III(I,1) = III(I,5)
          III(I,2) = III(I,6)
          III(I,3) = III(I,7)
          III(I,4) = III(I,8)
          III(I,5) = III(I,13)
          III(I,6) = III(I,14)
          III(I,7) = III(I,15)
          III(I,8) = III(I,16)
        ENDIF
      ENDDO
      CALL I17VIT_PENA(
     1             LLT       ,NINT   ,V      ,A      ,III    ,IIIS   ,
     2             NI_M      ,NI_S   ,NX     ,NY     ,NZ     ,VIT    ,
     3             ICONT(1,1),R_CM   ,T_CM   ,R_CS   ,T_CS   ,S_CM   ,
     4             LES       )
C-----------------------------------------------------------------------
C     calcul du contact
C-----------------------------------------------------------------------
      CALL I17LLL4_PENA(
     1      LLT       ,ITIED     ,NINT    ,V       ,A       ,FRIC    ,
     3      III       ,IIIS      ,NI_M    ,NI_S    ,S_CM    ,S_CS    ,
     4      NX        ,NY        ,NZ      ,VIT     ,LE      ,LES     ,
     5      ICONT(1,1),R_CM      ,T_CM    ,R_CS    ,T_CS    ,XX      ,
     6      YY        ,ZZ        ,STIFN   ,FSKYI   ,ISKY    ,
     7      FROTM     ,FROTS     ,AREA    ,AREA_TOT,KM      ,KS      ,
     8      FSAV      ,FCONT     ,MS      ,AREA_EL ,NISKYFI ,NOINT   ,
     9      H3D_DATA  )
      RETURN
      END
Chd|====================================================================
Chd|  I17MINI_PENA                  source/interfaces/int17/i17for3.F
Chd|-- called by -----------
Chd|        I17LLL_PENA                   source/interfaces/int17/i17for3.F
Chd|-- calls ---------------
Chd|        I17ABC                        source/interfaces/int17/i17lagm.F
Chd|        I17ABC_PENA                   source/interfaces/int17/i17for3.F
Chd|        I17NORM                       source/interfaces/int17/i17lagm.F
Chd|        I17RACINE_PENA                source/interfaces/int17/i17for3.F
Chd|        I17SURF                       source/interfaces/int17/i17for3.F
Chd|====================================================================
      SUBROUTINE I17MINI_PENA(
     1                   LLT   ,R_CS  ,S_CS  ,T_CS  ,RI_S  ,SI_S  ,
     2                   TI_S  ,NI_S  ,XXS   ,YYS   ,ZZS   ,XX    ,
     3                   YY    ,ZZ    ,R_CM  ,S_CM  ,T_CM  ,NX    ,
     4                   NY    ,NZ    ,R_1S  ,R_2S  ,T_1S  ,T_2S  ,
     5                   R_1M  ,R_2M  ,R_3M  ,R_4M  ,T_1M  ,T_2M  ,
     6                   T_3M  ,T_4M  ,ICONT ,AREA,AREA_TOT,AREA_EL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LLT,
     +        ICONT(MVSIZ,4)
C     REAL
      my_real
     + SI_S(MVSIZ,*),NI_S(MVSIZ,*),RI_S(MVSIZ,*),TI_S(MVSIZ,*),
     + XXS(MVSIZ,*) ,YYS(MVSIZ,*) ,ZZS(MVSIZ,*) ,
     + XX(MVSIZ,*)  ,YY(MVSIZ,*)  ,ZZ(MVSIZ,*) ,
     + R_CM(MVSIZ) ,S_CM(MVSIZ) ,T_CM(MVSIZ),
     + R_CS(MVSIZ) ,S_CS(MVSIZ) ,T_CS(MVSIZ),
     + R_1S(MVSIZ) ,R_2S(MVSIZ) ,T_1S(MVSIZ) ,T_2S(MVSIZ),
     + R_1M(MVSIZ) ,R_2M(MVSIZ) ,T_1M(MVSIZ) ,T_2M(MVSIZ),
     + R_3M(MVSIZ) ,R_4M(MVSIZ) ,T_3M(MVSIZ) ,T_4M(MVSIZ),
     + NX(MVSIZ) ,NY(MVSIZ) ,NZ(MVSIZ) ,AREA(MVSIZ) ,AREA_TOT(MVSIZ),
     + AREA_EL(MVSIZ)  
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ITER,NITERMAX,IR,IT
      my_real
     +        A1(MVSIZ),A2(MVSIZ),A3(MVSIZ),A4(MVSIZ),A5(MVSIZ),
     +        B1(MVSIZ),B2(MVSIZ),B3(MVSIZ),B4(MVSIZ),B5(MVSIZ),
     +        C1(MVSIZ),C2(MVSIZ),C3(MVSIZ),AZERO(MVSIZ),
     +        F1,F2,F3,F4,F5,F6,F7,F8,
     +        BB1,BB2,BB3,
     +        CC1,CC2,CC3,DD1,DD2,DD3,DD,D,
     +        A0,AB,BA,A4R,B4T,A5T,B5R,EPS,
     +        XA,YA,ZA,XB,YB,ZB,XC,YC,ZC,AAA,UNPEPS,
     +        P,RM,TM,SM,PP,RR,TT,AA,RT(9),AS(9)
      DATA RT/-1.,-0.75,-0.5,-0.25,0.0,0.25,0.5,0.75,1./
      DATA AS/0.0625,0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.0625/
C-----------------------------------------------
C-----------------------------------------------
C ro = r ri    to = t ti
C
C i=1,4  
C    ri=+-1  ti=+-1
C    Ni = 1/4 (1+ro)(1+to)(ro+to-1)
C    Ni = 1/4 (r^2+r^2to+t^2+roto+rot^2-1)
C    Ni = 1/4 (r^2(1+to)+t^2(1+ro)+roto-1)
C    dNi/dr = ri/4 (1+to)(2ro+to)
C    dNi/dt = ti/4 (1+ro)(2to+ro)
C    dNi/dr = 1/4 (2 r + ri ti t + 2ti rt + ri t^2)
C    dNi/dt = 1/4 (2 t + ri ti r + 2ri rt + ti r^2)
C
C i=6;8
C    ri=0    ti=+-1
C    Ni = 1/2 (1-r^2)(1+to)
C    dNi/dr = -r (1+to)
C    dNi/dt = ti/2 (1-r^2)
C    dNi/dr = 1/4 (-4 r - 4ti r t)
C    dNi/dt = 1/4 (2ti - 2ti r^2 )
C
C
C i=5;7
C    ri=+-1  ti=0
C    Ni = 1/2 (1-t^2)(1+ro)
C    dNi/dr = ri/2 (1-t^2)
C    dNi/dt = -t (1+ro)
C    dNi/dr = 1/4 (2ri  - 2ri t^2 )
C    dNi/dt = 1/4 (-4 t - 4ri rt  )
C-----------------------------------------------
C  df/dr = Somme( fi dNi/dr )
C  df/dt = Somme( fi dNi/dt )
C
C  df/dr = A1 + A2 r + A3 t + A4 rt + A5 t^2
C  df/dt = B1 + B2 r + B3 t + B4 rt + B5 r^2
C 
C  A1 = (                      -f5          +   f7        )/2
C  A2 = ( f1 + f2 + f3 + f4         - 2 f6         - 2 f8)/2
C  A3 = ( f1 - f2 + f3 - f4                                )/4
C  A4 = (-f1 + f2 + f3 - f4         - 2 f6         + 2 f8)/2
C  A5 = (-f1 - f2 + f3 + f4 + 2 f5          - 2 f7        )/4
C
C  B1 = (                               f6         -   f8)/2
C  B2 = ( f1 - f2 + f3 - f4                                )/4
C  B3 = ( f1 + f2 + f3 + f4 - 2 f5          - 2 f7        )/2
C  B4 = (-f1 - f2 + f3 + f4 + 2 f5          - 2 f7        )/2
C  B5 = (-f1 + f2 + f3 - f4         - 2 f6         + 2 f8)/4
C
C  df/dr = A1 + A2 r + A3 t + A4 rt + A5 t^2 = 0
C  df/dt = B1 + B2 r + B3 t + B4 rt + B5 r^2 = 0
C  r = -(A1 + A3 t + A5 t^2 ) / (A2 + A4 t)
C  t = -(B1 + B2 r + B5 r^2 ) / (B3 + B4 r)
c
c
c                                r
C                                ^
c                                |
C                          .     |7    
C            4 O-----------------O-----------------O 3        
C              |           .     |                 |          
C              |           .     |                 |          
C              |                 |      C          |
C              |           .    r+------+df/dt=0   |  
C              |           .     |      |df/dr=0   |    
C              |                 |      |          |
C              |                 |      |          |   
C              |                 |      |          |6       
C            8 0                 +------+----------0----> t  
C              |                        t          |       
C              |                                   |      
C              |                                   |        
C              |                                   |      
C              |                                   |    
C              |                                   |    
C              |                                   |    
C              |'                                  |    
C              O-----------------O-----------------O    
C             1                  5                  2
C
C-----------------------------------------------
C
C              second.        main
C     (1):   <R1S , TCS>    <R1M , T1M>
C     (2):   <R2S , TCS>    <R2M , T2M>
C     (3):   <RCS , T1S>    <R3M , T3M>
C     (4):   <RCS , T2S>    <R4M , T4M> 
C
C
c                                 O
c                                / \
c                               /   _____
c                              /   / \   \
c                             /   /   \   +r2s
c                            /   /     \ / \
c                           /   /   r2s'+   \
c                          /   /       /|\  |<-----Area_tot
c                      rm O   /       / | \ | 
C                      ^ /   /       /  |  \|   rs
c                      |/t1s/       /   |   /   ^  
C                      /   +       / (2)|  / \ / 
C    O----------------/O---o------------oO/   O     
C    |               / | /(3)\:::/:::::::|   / \
C    |              /  |/:::::\:/:::::::/|  /   \
C    |             /rcm+-------+rcs,tcs/ | /     \
C    |            /   /|::::::/|\:::::/  |/       \
C    |           /   /:|:::::/:|:\:::/   /         \
C    |          O   /::|::::/::|::\:/   /|          \
C    |           \ /:::|:::/:::|:::o(4)  |           \
C    |            \::::|::/::::|::/t2s   |            \
C    O            |\:::+-/-----+-/-------O----> tm     \
C    |            | \:::/::::tcm/       \|              \
C    |            |  \:/:::::::/         \               O      
C    |            \(1)o:::::::/          |\             /        
C    |             \ / \:::::/           | \           /      
C    |              +   \:::/            |  \         /    
C    |            r1s\___\_/             |   \       /    
C    |                    \              |    \     /    
C    |main               \             |     \   /    
C    O-----------------O----O------------O      \ /
C                            \                   O    
C                             \                 / \
C                              \               /   \
C      :::: Area                \             /     v  
C      :::: commune              \           /      ts
C      :::: main   Sec            \         /    
C                                  \second/
C                                   \     /
C                                    \   /
C                                     \ /
C                                      O
c
c    Area_tot = pi/4 | (r1s,r2s) x (t1s,t2s) |
c    Area    ~= pi/4 | ((1),(2)) x ((3),(4)) |
C-----------------------------------------------
C
c
      NITERMAX = 5
C
      DO I=1,LLT
        AZERO(I) = ZERO
        D = SI_S(I,1)*SI_S(I,1)+SI_S(I,2)*SI_S(I,2)
     +    + SI_S(I,3)*SI_S(I,3)+SI_S(I,4)*SI_S(I,4)
     +    + SI_S(I,5)*SI_S(I,5)+SI_S(I,6)*SI_S(I,6)
     +    + SI_S(I,7)*SI_S(I,7)+SI_S(I,8)*SI_S(I,8)
        D = 1./MAX(EM20,SQRT(D))
        F1 = D * SI_S(I,1)
        F2 = D * SI_S(I,2)
        F3 = D * SI_S(I,3)
        F4 = D * SI_S(I,4)
        F5 = D * SI_S(I,5)
        F6 = D * SI_S(I,6)
        F7 = D * SI_S(I,7)
        F8 = D * SI_S(I,8)
        A0 = ( F1 + F2 + F3 + F4 )*HALF
        AB = F5 - F7
        BA = F6 - F8
C
        A1(I) = -AB * HALF
        A2(I) = A0 - F6 - F8
        A3(I) = ( F1 - F2 + F3 - F4 )*FOURTH
        A4(I) = (-F1 + F2 + F3 - F4 )*HALF  - BA
        A5(I) = (-F1 - F2 + F3 + F4 )*FOURTH - A1(I)
C
        B1(I) = BA*HALF
        B2(I) = A3(I)
        B3(I) = A0 - F5  - F7
        B4(I) = (-F1 - F2 + F3 + F4 )*HALF  + AB
        B5(I) = (-F1 + F2 + F3 - F4 )*FOURTH - B1(I)
c
        R_CS(I) = ZERO
        T_CS(I) = ZERO
      ENDDO
c------------------------------------------------
c   newton iter : lineari_sation en r et t
c------------------------------------------------
C  fr = df/dr = A1 + A2 r + A3 t + A4 t r + A5 t t = 0
C  ft = df/dt = B1 + B2 r + B3 t + B4 r t + B5 r r = 0 
c
C  fr = fr_ + dfr/dr dr + dfr/dt dt
C  ft = ft_ + dft/dr dr + dft/dt dt
c
C  fr = fr_ + dfr/dr (r-r_) + dfr/dt (t-t_) = 0
C  ft = ft_ + dft/dr (r-r_) + dft/dt (t-t_) = 0
c
C  dfr/dr = A2 + A4 t_ = C2
C  dfr/dt = A3 + A4 r_ + 2 A5 t_ = C3
C  dft/dr = B2 + B4 t_ + 2 B5 r_ = D2
C  dft/dt = B3 + B4 r_ = D3
c
c  C1 = A1 - A4 r_ t_ - A5 t_^2
c  C2 = A2 + A4 t_
c  C3 = A3 + A4 r_ + 2 A5 t_
c
c  D1 = B1 - B4 r_ t_ - B5 r_^2
c  D2 = B2 + B4 t_ + 2 B5 r_
c  D3 = B3 + B4 r_ 
c
C  fr = C1 + C2 r + C3 t = 0
C  ft = D1 + D2 r + D3 t = 0
c
C  r = (C3 D1 - D3 C1) / (D3 C2 - C3 D2) 
C  t = (D2 C1 - C2 D1) / (D3 C2 - C3 D2)
c------------------------------------------------
      DO ITER=1,NITERMAX
        DO I=1,LLT
          A4R = A4(I) * R_CS(I)
          A5T = A5(I) * T_CS(I)
          B4T = B4(I) * T_CS(I)
          B5R = B5(I) * R_CS(I)
          CC1  = A1(I) -(A4R + A5T) * T_CS(I)
          CC2  = A2(I) + A4(I) * T_CS(I)
          CC3  = A3(I) + A4R + A5T + A5T
          DD1  = B1(I) -(B4T + B5R )* R_CS(I)
          DD2  = B2(I) + B4T + B5R + B5R
          DD3  = B3(I) + B4(I) * R_CS(I)
          D    = DD3 * CC2 - CC3 * DD2 
          IF(ABS(D)<EM20)THEN
            CC2 = CC2 + EM10
            DD3 = DD3 + EM10
            D   = DD3 * CC2 - CC3 * DD2 
          ENDIF
          R_CS(I) = (CC3 * DD1 - DD3 * CC1) / D
          T_CS(I) = (DD2 * CC1 - CC2 * DD1) / D
          R_CS(I) = MAX(-ONE,MIN(ONE,R_CS(I)))
          T_CS(I) = MAX(-ONE,MIN(ONE,T_CS(I)))
c          dfdr = A1 + A2 * r + A3 * t + A4 * t * r + A5 * t * t 
c          dfdt = B1 + B2 * r + B3 * t + B4 * r * t + B5 * r * r 
        ENDDO
      ENDDO
C-----------------------------------------------------------------------
C     calcul de 4 points de l'iso S_M = +- 1
C 
C          second.           main
C     1: <R_1S , T_CS>    <R_1M , T_1M>
C     2: <R_2S , T_CS>    <R_2M , T_2M>
C     3: <R_CS , T_1S>    <R_3M , T_3M>
C     4: <R_CS , T_2S>    <R_4M , T_4M> 
C
C     on borne r et t a +- 1 sur l'element second. :
C                -1 < R_1S < 1  et -1 < R_2S < 1 
C                -1 < T_1S < 1  et -1 < T_2S < 1 
c
C     on borne r et t a +- 1 sur l'element main :
C                -1 < R_M < 1   et -1 < T_M < 1 
C-----------------------------------------------------------------------
      CALL  I17ABC(LLT ,SI_S,R_CS,T_CS,
     +             B1  ,B2  ,B3  ,C1  ,C2  ,C3  )
C
      DO I=1,LLT
        S_CM(I) = C1(I) + (C2(I) + C3(I)*T_CS(I))*T_CS(I)
        S_CM(I) = B1(I) + (B2(I) + B3(I)*R_CS(I))*R_CS(I)
      ENDDO
c
      DO I=1,LLT
          IF(S_CM(I)>ZERO)THEN
            B1(I) = B1(I) - ONE
            C1(I) = C1(I) - ONE
          ELSE
            B1(I) = B1(I) + ONE
            C1(I) = C1(I) + ONE
          ENDIF
      ENDDO
c
C  f = B1 + B2 r + B3 r^2
c
      CALL I17RACINE_PENA(LLT,B3,B2,B1,R_1S,R_2S)
C  f = C1 + C2 t + C3 t^2
C
      CALL I17RACINE_PENA(LLT,C3,C2,C1,T_1S,T_2S)
C-----------------------------------------------
C     calcul de la surface totale second
C-----------------------------------------------
      CALL I17SURF(LLT   ,R_1S  ,R_2S  ,R_CS  ,R_CS    ,
     2             T_CS  ,T_CS  ,T_1S  ,T_2S  ,AREA_TOT,
     3             XXS   ,YYS   ,ZZS   ,AZERO )
c-----------------------------------------------------------------------
c      borne surface totale     la surface de l'second
c-----------------------------------------------------------------------
      DO I=1,LLT
          XA = XXS(I,7)-XXS(I,5)
          YA = YYS(I,7)-YYS(I,5)
          ZA = ZZS(I,7)-ZZS(I,5)
          XB = XXS(I,8)-XXS(I,6)
          YB = YYS(I,8)-YYS(I,6)
          ZB = ZZS(I,8)-ZZS(I,6)
          XC = YA*ZB - ZA*YB
          YC = ZA*XB - XA*ZB
          ZC = XA*YB - YA*XB
c          AAA = PI*FOURTH*ABS(XC*NX(I) + YC*NY(I) + ZC*NZ(I))
          AAA = PI*FOURTH*SQRT(XC*XC+YC*YC+ZC*ZC)
          AREA_EL(I) = AAA
          AREA_TOT(I) = MIN(AREA_TOT(I),AAA)
c          IF(AREA_TOT(I) == ZERO)AREA_TOT(I) = AAA
      ENDDO
c-----------------------------------------------------------------------
c      decoupe de l'second 9x9
c      calcul surface commune
c-----------------------------------------------------------------------
      UNPEPS = ONE+EM02
      DO I=1,LLT
        AREA(I) = ZERO
        PP = ZERO
        RR = ZERO
        TT = ZERO
        AA = ZERO
        DO IR=1,9
          DO IT=1,9
            CALL  I17ABC_PENA(I,RI_S,RT(IR),RT(IT),
     +                   BB1 ,BB2 ,BB3 ,CC1 ,CC2 ,CC3 )
            RM = BB1 + (BB2 + BB3*RT(IR))*RT(IR)
            IF(RM >= -UNPEPS.and.RM <= UNPEPS)THEN
              CALL  I17ABC_PENA(I,TI_S,RT(IR),RT(IT),
     +                    BB1 ,BB2 ,BB3 ,CC1 ,CC2 ,CC3 )
              TM = CC1 + (CC2 + CC3*RT(IT))*RT(IT)
              IF(TM >= -UNPEPS.and.TM <= UNPEPS)THEN
                CALL  I17ABC_PENA(I,SI_S,RT(IR),RT(IT),
     +                      BB1 ,BB2 ,BB3 ,CC1 ,CC2 ,CC3 )
                SM = CC1 + (CC2 + CC3*RT(IT))*RT(IT)
                P = ONE - ABS(SM)
                IF(P > ZERO)THEN
                  AA = AA + AS(IR)*AS(IT)
                  P  = P * AS(IR) * AS(IT)
                  PP = PP + P
                  RR = RR + P*RT(IR)
                  TT = TT + P*RT(IT)
                ENDIF
              ENDIF
            ENDIF
          ENDDO
        ENDDO
        ICONT(I,1) = 0
        IF(AA > ZERO)THEN
          R_CS(I) = RR / PP
          T_CS(I) = TT / PP
          CALL  I17ABC_PENA(I,RI_S,R_CS(I),T_CS(I),
     +                   BB1 ,BB2 ,BB3 ,CC1 ,CC2 ,CC3 )
          R_CM(I) = BB1 + (BB2 + BB3*RT(IR))*RT(IR)
          CALL  I17ABC_PENA(I,TI_S,R_CS(I),T_CS(I),
     +                   BB1 ,BB2 ,BB3 ,CC1 ,CC2 ,CC3 )
          T_CM(I) = CC1 + (CC2 + CC3*RT(IT))*RT(IT)
          CALL  I17ABC_PENA(I,SI_S,R_CS(I),T_CS(I),
     +                   BB1 ,BB2 ,BB3 ,CC1 ,CC2 ,CC3 )
          S_CM(I) = CC1 + (CC2 + CC3*T_CS(I))*T_CS(I)
          IF(ABS(S_CM(I)) < ONE)THEN
            ICONT(I,1) = 1
            AREA(I) = MIN(AREA_EL(I) * AA,AREA_TOT(I))
          ENDIF
        ENDIF
      ENDDO
c-----------------------------------------------------------------------
c      borne surface totale     la surface du main
c-----------------------------------------------------------------------
      DO I=1,LLT
        IF(S_CM(I) < ZERO)THEN
c         main 1234
          XA = XX(I,11)-XX(I,9)
          YA = YY(I,11)-YY(I,9)
          ZA = ZZ(I,11)-ZZ(I,9)
          XB = XX(I,12)-XX(I,10)
          YB = YY(I,12)-YY(I,10)
          ZB = ZZ(I,12)-ZZ(I,10)
        ELSE
c         main 5678
          XA = XX(I,15)-XX(I,13)
          YA = YY(I,15)-YY(I,13)
          ZA = ZZ(I,15)-ZZ(I,13)
          XB = XX(I,16)-XX(I,14)
          YB = YY(I,16)-YY(I,14)
          ZB = ZZ(I,16)-ZZ(I,14)
        ENDIF
        XC = YA*ZB - ZA*YB
        YC = ZA*XB - XA*ZB
        ZC = XA*YB - YA*XB
        AAA = PI*FOURTH*SQRT(XC*XC+YC*YC+ZC*ZC)
        AREA_EL(I) = MIN(AREA_EL(I),AAA)
        AREA_TOT(I) = MIN(AREA_TOT(I),AAA)
        IF(AREA_TOT(I) == ZERO)AREA_TOT(I) = AREA_EL(I)
      ENDDO
C-----------------------------------------------
C          calcul de Ni(r,s,t); dNi/dr; dNi/dt; normale second(-n)
C-----------------------------------------------
      CALL I17NORM(LLT  ,R_CS  ,S_CS  ,T_CS  ,
     2             NX   ,NY    ,NZ    ,XXS   ,YYS   ,ZZS   )
c-----------------------------------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  I17ABC_PENA                   source/interfaces/int17/i17for3.F
Chd|-- called by -----------
Chd|        I17MINI_PENA                  source/interfaces/int17/i17for3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I17ABC_PENA(I,F   ,R   ,T   ,
     +                  B1  ,B2  ,B3  ,C1  ,C2  ,C3  )
C-----------------------------------------------
C i=1,4  
C    ri=+-1  ti=+-1
C    Ni = 1/4 (1+ro)(1+to)(ro+to-1)
C    Ni = 1/4 (r^2 + ti r^2 t + t^2 + ri ti r t + ri r t^2 - 1)
C
C i=6;8
C    ri=0    ti=+-1
C    Ni = 1/2 ( 1 + ti t - r^2 - ti t r^2)
C
C
C i=5;7
C    ri=+-1  ti=0
C    Ni = 1/2 (1 + ri r - t^2 - ri r t^2)
C-----------------------------------------------
C  f = Somme( fi Ni )
C
C  f = A1 + A2 r + A3 t + A4 rt + A5 r^2 + A6 t^2 + A7 tr^2 + A8 rt^2
C  f = (A1 + A3 t + A6 t^2) + (A2 + A4 t + A8 t^2)r + (A5 + A7 t)r^2
C  f = B1 + B2 r + B3 r^2
C  f = (A1 + A2 r + A5 r^2) + (A3 + A4 r + A7 r^2)t + (A6 + A8 r)t^2
C  f = C1 + C2 t + C3 t^2
C 
C  A1 = (-f1 - f2 - f3 - f4 + 2 f5  + 2 f6  + 2 f7 + 2 f8)/4
C  A2 = (                   -   f5          +   f7       )/2
C  A3 = (                           +   f6         -   f8)/2
C  A4 = (+f1 - f2 + f3 - f4                              )/4
C  A5 = (+f1 + f2 + f3 + f4         - 2 f6         - 2 f8)/4                     
C  A6 = (+f1 + f2 + f3 + f4 - 2 f5         - 2 f7        )/4   
C  A7 = (-f1 + f2 + f3 - f4         - 2 f6         + 2 f8)/4   
C  A8 = (-f1 - f2 + f3 + f4 + 2 f5         - 2 f7        )/4  
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I
      my_real
     +        F(MVSIZ,*),R,T,
     +        B1,B2,B3,
     +        C1,C2,C3
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     +        A1,A2,A3,A4,A5,A6,A7,A8,R2,T2,FF5,FF6,FF7,FF8
C
        FF5 = F(I,5) + F(I,5)
        FF6 = F(I,6) + F(I,6)
        FF7 = F(I,7) + F(I,7)
        FF8 = F(I,8) + F(I,8)
c
c        A1 = (-F(I,1)-F(I,2)-F(I,3)-F(I,4)+FF5+FF6+FF7+FF8)*0.25 ...
c        B1(I) = A1 + ( A3 + A6 * T(I) ) *T(I) ...
c
        A1 = (-F(I,1)-F(I,2)-F(I,3)-F(I,4)+FF5+FF6+FF7+FF8)
        A2 = (                            -FF5    +FF7    )
        A3 = (                              +FF6    -FF8)
        A4 = (+F(I,1)-F(I,2)+F(I,3)-F(I,4)                )
        A5 = (+F(I,1)+F(I,2)+F(I,3)+F(I,4)    -FF6    -FF8)
        A6 = (+F(I,1)+F(I,2)+F(I,3)+F(I,4)-FF5    -FF7    ) 
        A7 = (-F(I,1)+F(I,2)+F(I,3)-F(I,4)    -FF6    +FF8) 
        A8 = (-F(I,1)-F(I,2)+F(I,3)+F(I,4)+FF5    -FF7    )
c
        B1 = ( A1 + ( A3 + A6 * T ) *T )*FOURTH
        B2 = ( A2 + ( A4 + A8 * T ) *T )*FOURTH
        B3 = ( A5 + A7 * T             )*FOURTH
c
        C1 = ( A1 + ( A2 + A5 * R ) *R )*FOURTH
        C2 = ( A3 + ( A4 + A7 * R ) *R )*FOURTH
        C3 = ( A6 + A8 * R             )*FOURTH
c
      RETURN
      END
Chd|====================================================================
Chd|  I17BORNE_PENA                 source/interfaces/int17/i17for3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        I17RACINE                     source/interfaces/int17/i17lagm.F
Chd|====================================================================
      SUBROUTINE I17BORNE_PENA(LLT ,R_S ,A   ,B  ,C  ,ICONT,RS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LLT,ICONT(MVSIZ)
      my_real
     +        R_S(MVSIZ),C(MVSIZ),B(MVSIZ),A(MVSIZ),RS(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     +   CC(MVSIZ),R1(MVSIZ),R2(MVSIZ),RS1,RS2,UNPEPS
c-----------------------------------------------------------------------
c  1: borne r(ou s au 2eme appel) a +-1 sur le main
c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
c  1.1: borne <r1,t> et <r,t1> a +-1 sur le main
c-----------------------------------------------------------------------
        UNPEPS = ONE + EM4
        DO I=1,LLT
          RS(I) = C(I) + ( B(I) + A(I)*R_S(I) )*R_S(I)
          IF(RS(I)>ONE)THEN
            CC(I) = C(I) - ONE
          ELSEIF(RS(I)<-ONE)THEN
            CC(I) = C(I) + ONE
          ELSE 
            CC(I) = ONE
          ENDIF
        ENDDO
c
C  f = C + B r + A r^2
c
        CALL I17RACINE(LLT,A,B,CC,R1,R2)
c
        DO I=1,LLT
          IF(RS(I)>UNPEPS.OR.RS(I)<-UNPEPS)THEN
            RS1 = C(I) + ( B(I) + A(I)*R1(I) )*R1(I)
            RS2 = C(I) + ( B(I) + A(I)*R2(I) )*R2(I)
            IF(RS1>=-UNPEPS.AND.RS1<=UNPEPS.AND.
     +         RS2>=-UNPEPS.AND.RS2<=UNPEPS)THEN
              IF(ABS(RS(I)-R1(I))<ABS(RS(I)-R2(I)))THEN
                R_S(I) = R1(I)
                RS(I)  = RS1
              ELSE
                R_S(I) = R2(I)
                RS(I)  = RS2
              ENDIF
            ELSEIF(RS1>=-UNPEPS.AND.RS1<=UNPEPS)THEN
                R_S(I) = R1(I)
                RS(I)  = RS1
            ELSEIF(RS2>=-UNPEPS.AND.RS2<=UNPEPS)THEN
                R_S(I) = R2(I)
                RS(I)  = RS2
            ELSE
C pas de recouvrement main/second
                ICONT(I)=0
            ENDIF
          ENDIF
        ENDDO
c
      RETURN
      END
Chd|====================================================================
Chd|  I17RACINE_PENA                source/interfaces/int17/i17for3.F
Chd|-- called by -----------
Chd|        I17MINI_PENA                  source/interfaces/int17/i17for3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I17RACINE_PENA(LLT,A,B,C,R1,R2)
C-----------------------------------------------
C
C    calcul des racines r1,r2 bornees a +- 1
C
C    de  a x^2 + b x + c = 0
C 
C    la routine retourne -1,+1 s'il n'y a pas de racines
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LLT
      my_real
     .   C(MVSIZ),B(MVSIZ),A(MVSIZ),R1(MVSIZ),R2(MVSIZ) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .  D      
C
      DO I=1,LLT
        D =  B(I)*B(I) - FOUR*A(I)*C(I) 
        IF(ABS(A(I))<EM20)THEN
          IF(ABS(B(I))<EM20)THEN
            R1(I) = -ONE
            R2(I) =  ONE
          ELSE
            R1(I) = -C(I) / B(I)
            R2(I) = B(I) / ABS(B(I))
          ENDIF
        ELSEIF(D<ZERO)THEN
            R1(I) = -ONE
            R2(I) =  ONE            
        ELSE
            D = SQRT( D )
            R1(I) = (-B(I) - D) / (TWO*A(I))
            R2(I) = (-B(I) + D) / (TWO*A(I))
        ENDIF
      ENDDO
c-----------------------------------------------------------------------
c      borne r (ou t) a +-1 sur l'second
c-----------------------------------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  I17SURF                       source/interfaces/int17/i17for3.F
Chd|-- called by -----------
Chd|        I17MINI_PENA                  source/interfaces/int17/i17for3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I17SURF(
     1             LLT   ,R1    ,R2    ,R3    ,R4    ,
     2             T1    ,T2    ,T3    ,T4    ,AREA  ,
     3             XX    ,YY    ,ZZ    ,SM   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LLT
      my_real
     .   R1(MVSIZ),R2(MVSIZ),R3(MVSIZ),R4(MVSIZ),
     .   T1(MVSIZ),T2(MVSIZ),T3(MVSIZ),T4(MVSIZ),
     .   AREA(MVSIZ),XX(MVSIZ,*) ,YY(MVSIZ,*),ZZ(MVSIZ,*),SM(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K
      my_real
     .  U_M_R,U_P_R,U_M_S,U_P_S,U_M_T,U_P_T,
     .  UMS_UMT,UMS_UPT,UPS_UMT,UPS_UPT,
     .  UMR_UMS,UMR_UPS,UPR_UMS,UPR_UPS,
     .  UMT_UMR,UMT_UPR,UPT_UMR,UPT_UPR,
     .  A,B,R05,S05,T05,R,T,NI(8),
     .  X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3,PIS4
C-----------------------------------------------------------------------
C     calcul de la surface
C-----------------------------------------------------------------------
      DO I=1,LLT
C
C-----------------------------------------------------------------------
        R = R1(I)
        T = T1(I)
C-----------------------------------------------------------------------
        R05 = HALF*R
        T05 = HALF*T
C
        U_M_R = HALF - R05
        U_P_R = HALF + R05
        U_M_T = HALF - T05
        U_P_T = HALF + T05
C
        NI(1) = U_M_T * U_M_R * (-R-T-ONE)
        NI(2) = U_P_T * U_M_R * (-R+T-ONE)
        NI(3) = U_P_T * U_P_R * ( R+T-ONE)
        NI(4) = U_M_T * U_P_R * ( R-T-ONE)       
        A = (ONE-R*R)
        NI(6) = A * U_P_T
        NI(8) = A * U_M_T
        B = (ONE-T*T)
        NI(5) = B * U_M_R
        NI(7) = B * U_P_R
        X1 = ZERO
        Y1 = ZERO
        Z1 = ZERO
        X2 = ZERO
        Y2 = ZERO
        Z2 = ZERO
C------------------------------------
        IF(SM(I) == ZERO)THEN
c         secondary 1234 ou 5678
          DO K=1,8
            X1 = X1-NI(K)*XX(I,K)
            Y1 = Y1-NI(K)*YY(I,K)
            Z1 = Z1-NI(K)*ZZ(I,K)
          ENDDO
        ELSEIF(SM(I) < ZERO)THEN
c         main 1234
          DO K=1,4
            X1 = X1-NI(K)*XX(I,K)-NI(K+4)*XX(I,K+8)
            Y1 = Y1-NI(K)*YY(I,K)-NI(K+4)*YY(I,K+8)
            Z1 = Z1-NI(K)*ZZ(I,K)-NI(K+4)*ZZ(I,K+8)
          ENDDO
        ELSEIF(SM(I) > ZERO)THEN
c         main 5678
          DO K=1,4
            X1 = X1-NI(K)*XX(I,K+4)-NI(K+4)*XX(I,K+12)
            Y1 = Y1-NI(K)*YY(I,K+4)-NI(K+4)*YY(I,K+12)
            Z1 = Z1-NI(K)*ZZ(I,K+4)-NI(K+4)*ZZ(I,K+12)
          ENDDO
        ENDIF
C------------------------------------
C-----------------------------------------------------------------------
        R = R2(I)
        T = T2(I)
C-----------------------------------------------------------------------
        R05 = HALF*R
        T05 = HALF*T
C
        U_M_R = HALF - R05
        U_P_R = HALF + R05
        U_M_T = HALF - T05
        U_P_T = HALF + T05
C
        NI(1) = U_M_T * U_M_R * (-R-T-ONE)
        NI(2) = U_P_T * U_M_R * (-R+T-ONE)
        NI(3) = U_P_T * U_P_R * ( R+T-ONE)
        NI(4) = U_M_T * U_P_R * ( R-T-ONE)       
        A = (ONE-R*R)
        NI(6) = A * U_P_T
        NI(8) = A * U_M_T
        B = (ONE-T*T)
        NI(5) = B * U_M_R
        NI(7) = B * U_P_R
C------------------------------------
        IF(SM(I) == ZERO)THEN
c         secondary 1234 ou 5678
          DO K=1,8
            X1 = X1+NI(K)*XX(I,K)
            Y1 = Y1+NI(K)*YY(I,K)
            Z1 = Z1+NI(K)*ZZ(I,K)
          ENDDO
        ELSEIF(SM(I) < ZERO)THEN
c         main 1234
          DO K=1,4
            X1 = X1+NI(K)*XX(I,K)+NI(K+4)*XX(I,K+8)
            Y1 = Y1+NI(K)*YY(I,K)+NI(K+4)*YY(I,K+8)
            Z1 = Z1+NI(K)*ZZ(I,K)+NI(K+4)*ZZ(I,K+8)
          ENDDO
        ELSEIF(SM(I) > ZERO)THEN
c         main 5678
          DO K=1,4
            X1 = X1+NI(K)*XX(I,K+4)+NI(K+4)*XX(I,K+12)
            Y1 = Y1+NI(K)*YY(I,K+4)+NI(K+4)*YY(I,K+12)
            Z1 = Z1+NI(K)*ZZ(I,K+4)+NI(K+4)*ZZ(I,K+12)
          ENDDO
        ENDIF
C------------------------------------
C-----------------------------------------------------------------------
        R = R3(I)
        T = T3(I)
C-----------------------------------------------------------------------
        R05 = HALF*R
        T05 = HALF*T
C
        U_M_R = HALF - R05
        U_P_R = HALF + R05
        U_M_T = HALF - T05
        U_P_T = HALF + T05
C
        NI(1) = U_M_T * U_M_R * (-R-T-ONE)
        NI(2) = U_P_T * U_M_R * (-R+T-ONE)
        NI(3) = U_P_T * U_P_R * ( R+T-ONE)
        NI(4) = U_M_T * U_P_R * ( R-T-ONE)       
        A = (ONE-R*R)
        NI(6) = A * U_P_T
        NI(8) = A * U_M_T
        B = (ONE-T*T)
        NI(5) = B * U_M_R
        NI(7) = B * U_P_R
C------------------------------------
        IF(SM(I) == ZERO)THEN
c         secondary 1234 ou 5678
          DO K=1,8
            X2 = X2-NI(K)*XX(I,K)
            Y2 = Y2-NI(K)*YY(I,K)
            Z2 = Z2-NI(K)*ZZ(I,K)
          ENDDO
        ELSEIF(SM(I) < ZERO)THEN
c         main 1234
          DO K=1,4
            X2 = X2-NI(K)*XX(I,K)-NI(K+4)*XX(I,K+8)
            Y2 = Y2-NI(K)*YY(I,K)-NI(K+4)*YY(I,K+8)
            Z2 = Z2-NI(K)*ZZ(I,K)-NI(K+4)*ZZ(I,K+8)
          ENDDO
        ELSEIF(SM(I) > ZERO)THEN
c         main 5678
          DO K=1,4
            X2 = X2-NI(K)*XX(I,K+4)-NI(K+4)*XX(I,K+12)
            Y2 = Y2-NI(K)*YY(I,K+4)-NI(K+4)*YY(I,K+12)
            Z2 = Z2-NI(K)*ZZ(I,K+4)-NI(K+4)*ZZ(I,K+12)
          ENDDO
        ENDIF
C------------------------------------
C-----------------------------------------------------------------------
        R = R4(I)
        T = T4(I)
C-----------------------------------------------------------------------
        R05 = HALF*R
        T05 = HALF*T
C
        U_M_R = HALF - R05
        U_P_R = HALF + R05
        U_M_T = HALF - T05
        U_P_T = HALF + T05
C
        NI(1) = U_M_T * U_M_R * (-R-T-ONE)
        NI(2) = U_P_T * U_M_R * (-R+T-ONE)
        NI(3) = U_P_T * U_P_R * ( R+T-ONE)
        NI(4) = U_M_T * U_P_R * ( R-T-ONE)       
        A = (ONE-R*R)
        NI(6) = A * U_P_T
        NI(8) = A * U_M_T
        B = (ONE-T*T)
        NI(5) = B * U_M_R
        NI(7) = B * U_P_R
C------------------------------------
        IF(SM(I) == ZERO)THEN
c         secondary 1234 ou 5678
          DO K=1,8
            X2 = X2+NI(K)*XX(I,K)
            Y2 = Y2+NI(K)*YY(I,K)
            Z2 = Z2+NI(K)*ZZ(I,K)
          ENDDO
        ELSEIF(SM(I) < ZERO)THEN
c         main 1234
          DO K=1,4
            X2 = X2+NI(K)*XX(I,K)+NI(K+4)*XX(I,K+8)
            Y2 = Y2+NI(K)*YY(I,K)+NI(K+4)*YY(I,K+8)
            Z2 = Z2+NI(K)*ZZ(I,K)+NI(K+4)*ZZ(I,K+8)
          ENDDO
        ELSEIF(SM(I) > ZERO)THEN
c         main 5678
          DO K=1,4
            X2 = X2+NI(K)*XX(I,K+4)+NI(K+4)*XX(I,K+12)
            Y2 = Y2+NI(K)*YY(I,K+4)+NI(K+4)*YY(I,K+12)
            Z2 = Z2+NI(K)*ZZ(I,K+4)+NI(K+4)*ZZ(I,K+12)
          ENDDO
        ENDIF
C------------------------------------
C       Surface de l'ellipse
C------------------------------------
        X3 = Y1*Z2 - Z1*Y2
        Y3 = Z1*X2 - X1*Z2
        Z3 = X1*Y2 - Y1*X2
        AREA(I) = PI*FOURTH*SQRT(X3*X3+Y3*Y3+Z3*Z3)
      ENDDO
C-----------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  I17VIT_PENA                   source/interfaces/int17/i17for3.F
Chd|-- called by -----------
Chd|        I17LLL_PENA                   source/interfaces/int17/i17for3.F
Chd|-- calls ---------------
Chd|        I17NI                         source/interfaces/int17/i17lagm.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I17VIT_PENA(
     1             LLT    ,NINT   ,V      ,A      ,III    ,IIIS   ,
     2             NI_M   ,NI_S   ,NX     ,NY     ,NZ     ,VIT    ,
     3             ICONT  ,RM     ,TM     ,RS     ,TS     ,SM     ,
     4             LES    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
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
#include      "com08_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LLT,NINT   
      INTEGER III(MVSIZ,17),IIIS(MVSIZ,16),
     +        ICONT(MVSIZ,4), LES(MVSIZ)
C     REAL
      my_real
     .  V(3,*),A(3,*),VIT(*)
      my_real
     .   RM(MVSIZ) ,RS(MVSIZ) ,TM(MVSIZ) ,TS(MVSIZ) ,SM(MVSIZ),
     .   NI_M(MVSIZ,*) ,NI_S(MVSIZ,*),NX(MVSIZ) ,NY(MVSIZ) ,NZ(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IK,NK,I1,I2,I3,I4,IES,IIS
      my_real
     .   VX,VY,VZ,VN,AA
C-----------------------------------------------------------------------
C     calcul de Ni sur face  1 2 3 4 ou 5 6 7 8 du main (s=+-1)
C-----------------------------------------------------------------------
      CALL I17NI(LLT,RM ,TM ,NI_M )
C-----------------------------------------------------------------------
C     calcul de Ni sur face  1 2 3 4 ou 5 6 7 8 de l'second (s=+-1)
C-----------------------------------------------------------------------
      CALL I17NI(LLT,RS ,TS ,NI_S )
C-----------------------------------------------
C      calcul de [L]
C-----------------------------------------------
      DO I=1,LLT
C-----------------------------------------------
C       test si contact
C-----------------------------------------------
        IF(ICONT(I,1) /= 0)THEN
          VX = ZERO
          VY = ZERO
          VZ = ZERO
          IF(LES(I)>0)THEN      ! test pour SPMD
           DO IK=1,8
            VX = VX - (V(1,III(I,IK)))*NI_M(I,IK)
            VY = VY - (V(2,III(I,IK)))*NI_M(I,IK)
            VZ = VZ - (V(3,III(I,IK)))*NI_M(I,IK)
            VX = VX + (V(1,IIIS(I,IK)))*NI_S(I,IK)
            VY = VY + (V(2,IIIS(I,IK)))*NI_S(I,IK)
            VZ = VZ + (V(3,IIIS(I,IK)))*NI_S(I,IK)
           ENDDO
          ELSE  ! partie complementaire SPMD
           IES = ABS(LES(I))   ! no local
           DO IK=1,8
            VX = VX - V(1,III(I,IK))*NI_M(I,IK)
            VY = VY - V(2,III(I,IK))*NI_M(I,IK)
            VZ = VZ - V(3,III(I,IK))*NI_M(I,IK)
            IIS = IIIS(I,IK)
            VX = VX + VFI17(NINT)%P(1,IIS,IES)*NI_S(I,IK)
            VY = VY + VFI17(NINT)%P(2,IIS,IES)*NI_S(I,IK)
            VZ = VZ + VFI17(NINT)%P(3,IIS,IES)*NI_S(I,IK)
           ENDDO
          END IF
C
          VN = NX(I)*VX + NY(I)*VY + NZ(I)*VZ
          VIT(I)=SM(I)*VN
        ENDIF
      ENDDO
C
      RETURN
      END
C
Chd|====================================================================
Chd|  I17LLL4_PENA                  source/interfaces/int17/i17for3.F
Chd|-- called by -----------
Chd|        I17LLL_PENA                   source/interfaces/int17/i17for3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        ICONTACT_MOD                  share/modules/icontact_mod.F  
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I17LLL4_PENA(
     1      LLT       ,ITIED     ,NINT    ,V       ,A       ,FRIC    ,
     3      III       ,IIIS      ,NI_M    ,NI_S    ,S_CM    ,S_CS    ,
     4      NX        ,NY        ,NZ      ,VIT     ,LE      ,LES     ,
     5      ICONT     ,RM        ,TM      ,RS      ,TS      ,XX      ,
     6      YY        ,ZZ        ,STIFN   ,FSKYI   ,ISKY    ,
     7      FROTM     ,FROTS     ,AREA    ,AREA_TOT,KM      ,KS      ,
     8      FSAV      ,FCONT     ,MS      ,AREA_EL ,NISKYFI ,NOINT   ,
     9      H3D_DATA  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE ICONTACT_MOD
      USE MESSAGE_MOD
      USE H3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr07_c.inc"
#include      "scr11_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "units_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LLT,ITIED,NINT,NISKYFI,NOINT   
      INTEGER III(MVSIZ,17),IIIS(MVSIZ,16),ICONT(MVSIZ), ISKY(*),
     .        LE(*) ,LES(*)
C     REAL
      my_real
     .  V(3,*),A(3,*),VIT(*), MS(*)
      my_real
     .   RM(MVSIZ) ,RS(MVSIZ) ,TM(MVSIZ) ,TS(MVSIZ) ,
     .   XX(MVSIZ,17),YY(MVSIZ,17),ZZ(MVSIZ,17),S_CM(MVSIZ),
     .   S_CS(MVSIZ), FSAV(*),FCONT(3,*),
     .   NI_M(MVSIZ,*) ,NI_S(MVSIZ,*),NX(MVSIZ) ,NY(MVSIZ) ,NZ(MVSIZ),
     .   STIFN(*),FSKYI(LSKYI,NFSKYI),FROTM(7,*),FROTS(7,*),
     .   AREA(MVSIZ),AREA_TOT(MVSIZ),AREA_EL(MVSIZ),KM(2,*),KS(2,*),
     .   FRIC(*)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IK,NK,I1,I2,I3,I4,IAD,NN,LEM,J1,J2,NISKYL,IES,IIS,
     .        NISKYFIL
      my_real
     .   VX,VY,VZ,VN,AA,SURF,EP2,XE,YE,ZE,XA,YA,ZA,XB,YB,ZB,XC,YC,ZC,
     .   F,STIF,PENE,SIGTMX,SIGTMY,SIGTMZ,SIGTSX,SIGTSY,SIGTSZ,BB,
     .   FX,FY,FZ,FFX,FFY,FFZ,FF,FF2,MUF2,HTSQRTPI,MAS4,EP,VIS,
     .   RHOM,RHOS,STIFV,DT1INV,FFVX,FFVY,FFVZ,FFTX,FFTY,FFTZ,MUF,
     .   FSAV1,FSAV2,FSAV3,FSAV4,FSAV5,FSAV6,ECONVT,ECONTT,BETA,
     .   KS1,KS2
c-----------------------------------------------------------------------
cfrottement
c-----------------------------------------------------------------------
c0- formulation classique
c
c Ki = 8/3 E* (pi/A)^1/2  Ai/pi
c
c Fti = min[(Fti + Ki * Vti * dt) , mu*Fni]
c
c => limitation en cas de roulement glissement ou roulement adh   rence:
c    si Fti est m   moris   e sur l'   l   ment main ou second, Fti est perdue
c    quand on passe d'un    l   ment sur son voisin
c
c1- on consid   re une facette main contact   e par plusieurs seconds
c
c
c Fti = min[(Sigt*Ai + Ki * Vti * dt) , mu*Fni]
c
c Sigt = Somme(Fti)/Somme(Ai)
c 
c ou (estimation de A en i):  Sigt = Somme(Fti/A)
c
c    * Sigt vecteur contrainte de frottement
c
c
c2- sym   trie main second
c
c Fti = min[((SigtM+SigtS)*Ai/2 + Ki * Vti * dt) , mu*Fni]
c
c SigtM = SommeM(Fti)/SommeM(Ai)
c SigtS = SommeS(Fti)/SommeS(Ai)
C-----------------------------------------------
      HTSQRTPI = EIGHT / (THREE*SQRT(PI))

      IF(DT1>ZERO)THEN
        DT1INV = ONE/DT1
      ELSE
        DT1INV =ZERO
      ENDIF

C-----------------------------------------------
      FSAV1 = ZERO
      FSAV2 = ZERO
      FSAV3 = ZERO
      FSAV4 = ZERO
      FSAV5 = ZERO
      FSAV6 = ZERO
      ECONVT = ZERO
      ECONTT = ZERO
      DO I=1,LLT
        LEM = LE(I)
        IF(ICONT(I)/=0 )THEN
c---------------------------------------------
c       contact de Hertz
c---------------------------------------------
c
c ellipse de contact 2a x 2b   => surface A = a b pi
c r: rayon    quivalent de l'ellipse    r^2 = a b
c rayon de courbure  1/R = 1/R1 + 1/R2 + 1/R3 + 1/R4 
c k1 = (1-v1^2)/E1       k2 = (1-v2^2)/E2     k = k1+k2 = 1/E*
c kk1 = (1-v1^2)/piE1    kk2 = (1-v2^2)/piE2    kk = kk1+kk2 
c E* = 1/(k1+k2)
c F: force
c pm: pression max au centre de l'ellipse)
c d: penetration max
c------------------------------------------
cd'apres http://fr.wikibooks.org/wiki/Tribologie_-_Contacts_localis%C3%A9s
c
c pm = 3/2  F/A   (1)
c
c p(x,y) = pm [1 - x^2/a^2 - y^2/b^2]^1/3   (2)
c
c       sur la fronti   re p=0 => x^2/a^2 + y^2/b^2 = 1
c
cd'apres http://barreau.matthieu.free.fr/cours/Liaison-pivot/pages/roulements-1.html
c
c
c F = pm^3 pi^3 R^2 k^2 / 6   (4)
c
c   (1)+(4)
c F = 4 A / (3 pi R k)    [A/pi]^1/2
c
c-----------------------
c F = 4 A r / (3 pi R k)     (5)    
c F = 4 r^3 / (3 R k)        (6)    
c-----------------------
c--------------------------------------------------------------
cp   n   tration, force, rigidit   
c--------------------------------------------------------------
c
c   d = R - (R^2 - r^2)^1/2
c   d = R[1 - (1 - r^2/R^2)^1/2]
c-----------------------
c   d ~= 1/2 r^2/R              (8) 
c-----------------------
c   F = 8 r/ (3 k)  d           (9)    
c-----------------------
c   F = K  d                    (10)    
c-----------------------
c   K = 8 r/ (3 k)              (11)    
c-------------------------------------------
c---------------------------------------------
c       contact multiple (i=1,n)
c       si on connait la courbure
c---------------------------------------------
c   pour chaque contact on evalue ri^2 , Ri et di
c
c   Rmax = ri^2 / 2 di                    
c   Ki = 4/3 E* (2/ (Ri di))^1/2 ri^2     
c   Kmin = 8/3 E* ri
c   Fi = Ki di
c
c   Rmax = Ai / 2pi di
c   Ki   = 4/3 E* (2/(Ri di))^1/2  Ai/pi
c   Kmin = 8/3 E* (Ai/pi)^1/2
c   Fi   = max(Ki,Kmin) di
c---------------------------------------------
c       contact multiple (i=1,n)
c       si on connait la surface totale
c---------------------------------------------
c   A: surface totale du contact (estimee) 
c
c   Ri = A / 2pi di
c   Ki = 8/3 E* (pi/A)^1/2  Ai/pi
c   Fi = Ki di
C-----------------------------------------------
C          epaisseur main
C-----------------------------------------------
          XE = ZERO
          YE = ZERO
          ZE = ZERO
          DO IK=1,4
              XE = XE + XX(I,IK)   *NI_M(I,IK)
     +                + XX(I,IK+8) *NI_M(I,IK+4)
     +                - XX(I,IK+4) *NI_M(I,IK)
     +                - XX(I,IK+12)*NI_M(I,IK+4)
              YE = YE + YY(I,IK)   *NI_M(I,IK)
     +                + YY(I,IK+8) *NI_M(I,IK+4)
     +                - YY(I,IK+4) *NI_M(I,IK)
     +                - YY(I,IK+12)*NI_M(I,IK+4)
              ZE = ZE + ZZ(I,IK)   *NI_M(I,IK)
     +                + ZZ(I,IK+8) *NI_M(I,IK+4)
     +                - ZZ(I,IK+4) *NI_M(I,IK)
     +                - ZZ(I,IK+12)*NI_M(I,IK+4)
          ENDDO
          EP = ABS(XE*NX(I) + YE*NY(I) + ZE*NZ(I))
          PENE = HALF*(ONE-ABS(S_CM(I))) * EP
          if(pene<zero)then
            stop 111
          endif
C-----------------------------------------------
C          vitesse relative second/main
C-----------------------------------------------
          VX = ZERO
          VY = ZERO
          VZ = ZERO
          IF(LES(I)>0)THEN
           DO IK=1,8
            VX = VX + (V(1,IIIS(I,IK)))*NI_S(I,IK)
     .              - (V(1,III(I,IK))) *NI_M(I,IK)
            VY = VY + (V(2,IIIS(I,IK)))*NI_S(I,IK)
     .              - (V(2,III(I,IK))) *NI_M(I,IK)
            VZ = VZ + (V(3,IIIS(I,IK)))*NI_S(I,IK)
     .              - (V(3,III(I,IK))) *NI_M(I,IK)
           ENDDO
          KS1 = KS(1,LES(I))
           KS2 = KS(2,LES(I))
C           
           SIGTMX = FROTM(5,LEM)
           SIGTMY = FROTM(6,LEM)
           SIGTMZ = FROTM(7,LEM)
           SIGTSX = FROTS(5,LES(I))
           SIGTSY = FROTS(6,LES(I))
           SIGTSZ = FROTS(7,LES(I))
          ELSE  ! partie complementaire SPMD
           IES = ABS(LES(I))   ! no local
           DO IK=1,8
            IIS = IIIS(I,IK)
            VX = VX + VFI17(NINT)%P(1,IIS,IES)*NI_S(I,IK)
     .              - V(1,III(I,IK)) *NI_M(I,IK)
            VY = VY + VFI17(NINT)%P(2,IIS,IES)*NI_S(I,IK)
     .              - V(2,III(I,IK)) *NI_M(I,IK)
            VZ = VZ + VFI17(NINT)%P(3,IIS,IES)*NI_S(I,IK)
     .              - V(3,III(I,IK)) *NI_M(I,IK)
           ENDDO
           KS1 = KSFI(NINT)%P(1,IES)
           KS2 = KSFI(NINT)%P(2,IES)           
C           
           SIGTMX = FROTM(5,LEM)
           SIGTMY = FROTM(6,LEM)
           SIGTMZ = FROTM(7,LEM)
           SIGTSX = FROTSFI(NINT)%P(5,IES)
           SIGTSY = FROTSFI(NINT)%P(6,IES)
           SIGTSZ = FROTSFI(NINT)%P(7,IES)
          END IF
          VN = NX(I)*VX + NY(I)*VY + NZ(I)*VZ
C-----------------------------------------------
C          rigidite Hertz
C-----------------------------------------------
c   Ki = 8 E*  Ai / 3(A*pi)^1/2 
c         HTSQRTPI = EIGHT / (THREE*SQRT(PI))
          STIF = HTSQRTPI*AREA(I) / 
     .         ((KM(1,LEM)+KS1)*SQRT(AREA_TOT(I)))
          F  = STIF*PENE
C-----------------------------------------------
C          Viscosite critique
C-----------------------------------------------
          RHOM = KM(2,LEM)
          RHOS = KS2
          MAS4 = (RHOM+RHOS) * AREA(I) * MIN(EP,SQRT(AREA(I)))
          BETA=0.5
          VIS  = BETA*SQRT(STIF*MAS4)
          F    = F + VIS * VN
          STIFV = VIS*DT1INV
C-----------------------------------------------
C          force normale
C-----------------------------------------------
          FX = NX(I) * F
          FY = NY(I) * F
          FZ = NZ(I) * F
C-----------------------------------------------
C          vitesse tangente 
C-----------------------------------------------
c          VX = VX - VN*NX(I)
c          VY = VY - VN*NY(I)
c          VZ = VZ - VN*NZ(I)
C-----------------------------------------------
C       frottement
C         calcul avec vitesse totale 
C         et projection de la force 
C-----------------------------------------------          
          AA = HALF*AREA(I)
          BB = STIF*DT1
          FFX = AA*(SIGTMX+SIGTSX)+BB*VX
          FFY = AA*(SIGTMY+SIGTSY)+BB*VY
          FFZ = AA*(SIGTMZ+SIGTSZ)+BB*VZ
          FFVX = VIS*VX
          FFVY = VIS*VY
          FFVZ = VIS*VZ
C-----------------------------------------------
C         force tangente
C-----------------------------------------------
          FF = NX(I)*FFX + NY(I)*FFY + NZ(I)*FFZ
          FFX = FFX - FF*NX(I)
          FFY = FFY - FF*NY(I)
          FFZ = FFZ - FF*NZ(I)
          FF = NX(I)*FFVX + NY(I)*FFVY + NZ(I)*FFVZ
          FFVX = FFVX - FF*NX(I)
          FFVY = FFVY - FF*NY(I)
          FFVZ = FFVZ - FF*NZ(I)
          FFTX = FFX + FFVX
          FFTY = FFY + FFVY
          FFTZ = FFZ + FFVZ
C-----------------------------------------------
C         frottement de Coulomb (penalite viscoelastique)
C-----------------------------------------------
          FF2 = FFTX*FFTX
     .        + FFTY*FFTY
     .        + FFTZ*FFTZ
          MUF  = FRIC(1)*MAX(ZERO,F)
          MUF2 = MUF*MUF
          IF(FF2 > MUF2)THEN
c reduction de la contrainte visqueuse 
c puis de la contrainte elastique (si necessaire)
            AA = MUF/SQRT(FF2)
            FFTX = FFTX*AA
            FFTY = FFTY*AA
            FFTZ = FFTZ*AA
            FF2  = FFX*FFX + FFY*FFY + FFZ*FFZ
            IF(FF2 > MUF2)THEN
              AA  = MUF / SQRT(FF2)
              FFX = FFX*AA
              FFY = FFY*AA
              FFZ = FFZ*AA
            ENDIF
          ENDIF

#include "lockon.inc"
          FROTM(1,LEM) = FROTM(1,LEM) + FFX
          FROTM(2,LEM) = FROTM(2,LEM) + FFY
          FROTM(3,LEM) = FROTM(3,LEM) + FFZ
          FROTM(4,LEM) = FROTM(4,LEM) + AREA(I)
          IF(LES(I)>0)THEN
            IF(IPARIT == 0)THEN
              FROTS(1,LES(I)) = FROTS(1,LES(I)) + FFX
              FROTS(2,LES(I)) = FROTS(2,LES(I)) + FFY
              FROTS(3,LES(I)) = FROTS(3,LES(I)) + FFZ
              FROTS(4,LES(I)) = FROTS(4,LES(I)) + AREA(I)
            ELSE
              NSKYI17 = NSKYI17+1
              ISKYI17(NSKYI17)   = LES(I)
              FSKYI17(NSKYI17,1) = FFX
              FSKYI17(NSKYI17,2) = FFY
              FSKYI17(NSKYI17,3) = FFZ
              FSKYI17(NSKYI17,4) = AREA(I)
            END IF
          ELSE      ! partie complementaire SPMD
            IF(IPARIT == 0)THEN
              FROTSFI(NINT)%P(1,IES) = FROTSFI(NINT)%P(1,IES) + FFX
              FROTSFI(NINT)%P(2,IES) = FROTSFI(NINT)%P(2,IES) + FFY
              FROTSFI(NINT)%P(3,IES) = FROTSFI(NINT)%P(3,IES) + FFZ
              FROTSFI(NINT)%P(4,IES) = FROTSFI(NINT)%P(4,IES) + AREA(I)
            ELSE
              NRSKYI17=NRSKYI17+1
              IRSKYI17(NRSKYI17) = IES
              FRSKYI17(1,NRSKYI17) = FFX
              FRSKYI17(2,NRSKYI17) = FFY
              FRSKYI17(3,NRSKYI17) = FFZ
              FRSKYI17(4,NRSKYI17) = AREA(I)
            END IF
          END IF
#include "lockoff.inc"
C-----------------------------------------------
C         TH
C-----------------------------------------------
          FSAV1 =FSAV1 + FX
          FSAV2 =FSAV2 + FY
          FSAV3 =FSAV3 + FZ
          FSAV4 =FSAV4 + FFTX
          FSAV5 =FSAV5 + FFTY
          FSAV6 =FSAV6 + FFTZ
C-----------------------------------------------
C         force totale
C-----------------------------------------------
          FX = FX + FFTX
          FY = FY + FFTY
          FZ = FZ + FFTZ
          STIF = STIF + STIFV
          ECONT = ECONT 
     .          + VX*FX+VY*FY+VZ*FZ

          IF(LES(I)>0)THEN
           IF(IPARIT == 0)THEN
#include "lockon.inc"
            DO IK=1,8
              J1 = III(I,IK)
              A(1,J1) = A(1,J1) + NI_M(I,IK)*FX
              A(2,J1) = A(2,J1) + NI_M(I,IK)*FY
              A(3,J1) = A(3,J1) + NI_M(I,IK)*FZ
              STIFN(J1) = STIFN(J1) + ABS(NI_M(I,IK)*STIF)
          
              J2 = IIIS(I,IK)
              A(1,J2) = A(1,J2) - NI_S(I,IK)*FX
              A(2,J2) = A(2,J2) - NI_S(I,IK)*FY
              A(3,J2) = A(3,J2) - NI_S(I,IK)*FZ
              STIFN(J2) = STIFN(J2) + ABS(NI_S(I,IK)*STIF)
            ENDDO
#include "lockoff.inc"
           ELSE
#include "lockon.inc"
            NISKYL = NISKY
            NISKY  = NISKY + 16
#include "lockoff.inc"
            IF (NISKYL > LSKYI) THEN
             CALL ANCMSG(MSGID=26,ANMODE=ANINFO,
     .                   I1=NOINT)
             CALL ARRET(2)
            ENDIF
            DO IK=1,8
              NISKYL = NISKYL + 1
              FSKYI(NISKYL,1)=NI_M(I,IK)*FX
              FSKYI(NISKYL,2)=NI_M(I,IK)*FY
              FSKYI(NISKYL,3)=NI_M(I,IK)*FZ
              FSKYI(NISKYL,4)=ABS(NI_M(I,IK)*STIF)
              ISKY(NISKYL)   = III(I,IK)
              NISKYL = NISKYL + 1
              FSKYI(NISKYL,1)= - NI_S(I,IK)*FX
              FSKYI(NISKYL,2)= - NI_S(I,IK)*FY
              FSKYI(NISKYL,3)= - NI_S(I,IK)*FZ
              FSKYI(NISKYL,4)= ABS(NI_S(I,IK)*STIF)
              ISKY(NISKYL)   = IIIS(I,IK)
            ENDDO
           ENDIF
C
           IF(ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT >0.AND.
     .          (TT>=TANIM.OR.TT>=TOUTP.OR.TT>=H3D_DATA%TH3D.OR.
     .              (MANIM>=4.AND.MANIM<=15).OR. H3D_DATA%MH3D /= 0))THEN
#include "lockon.inc"
            DO IK=1,8
              J1 = III(I,IK)
              FCONT(1,J1) =FCONT(1,J1) + NI_M(I,IK)*FX
              FCONT(2,J1) =FCONT(2,J1) + NI_M(I,IK)*FY
              FCONT(3,J1) =FCONT(3,J1) + NI_M(I,IK)*FZ
          
              J2 = IIIS(I,IK)
              FCONT(1,J2) =FCONT(1,J2) - NI_S(I,IK)*FX
              FCONT(2,J2) =FCONT(2,J2) - NI_S(I,IK)*FY
              FCONT(3,J2) =FCONT(3,J2) - NI_S(I,IK)*FZ
            ENDDO
#include "lockoff.inc"
           ENDIF
          ELSE     ! cas complementaire uniquement en SPMD
           IF(IPARIT == 0)THEN
#include "lockon.inc"
            DO IK=1,8
              J1 = III(I,IK)
              A(1,J1) = A(1,J1) + NI_M(I,IK)*FX
              A(2,J1) = A(2,J1) + NI_M(I,IK)*FY
              A(3,J1) = A(3,J1) + NI_M(I,IK)*FZ
              STIFN(J1) = STIFN(J1) + ABS(NI_M(I,IK)*STIF)
C          
              J2 = IIIS(I,IK)
              AFI17(NINT)%P(1,J2,IES) = AFI17(NINT)%P(1,J2,IES)
     -                                - NI_S(I,IK)*FX
              AFI17(NINT)%P(2,J2,IES) = AFI17(NINT)%P(2,J2,IES)
     -                                - NI_S(I,IK)*FY
              AFI17(NINT)%P(3,J2,IES) = AFI17(NINT)%P(3,J2,IES)
     -                                - NI_S(I,IK)*FZ
              STNFI17(NINT)%P(J2,IES) = STNFI17(NINT)%P(J2,IES)
     +                                + ABS(NI_S(I,IK)*STIF)
            ENDDO
#include "lockoff.inc"
           ELSE
#include "lockon.inc"
            NISKYL = NISKY
            NISKY  = NISKY + 8
            NISKYFI=NISKYFI+1
            NISKYFIL=NISKYFI
#include "lockoff.inc"
            ISKYFI(NINT)%P(NISKYFIL) = IES
C
            IF (NISKYL > LSKYI) THEN
             CALL ANCMSG(MSGID=26,ANMODE=ANINFO,
     .                   I1=NOINT)
             CALL ARRET(2)
            ENDIF
            IF (NISKYFIL > NLSKYFI(NINT)) THEN
             CALL ANCMSG(MSGID=26,ANMODE=ANINFO,
     .                   I1=NOINT)
             CALL ARRET(2)
            ENDIF
C
            DO IK=1,8
              NISKYL = NISKYL + 1
              FSKYI(NISKYL,1)=NI_M(I,IK)*FX
              FSKYI(NISKYL,2)=NI_M(I,IK)*FY
              FSKYI(NISKYL,3)=NI_M(I,IK)*FZ
              FSKYI(NISKYL,4)=ABS(NI_M(I,IK)*STIF)
              ISKY(NISKYL)   = III(I,IK)
C
              FSKYFI(NINT)%P(1+(IK-1)*5,NISKYFIL)= - NI_S(I,IK)*FX
              FSKYFI(NINT)%P(2+(IK-1)*5,NISKYFIL)= - NI_S(I,IK)*FY
              FSKYFI(NINT)%P(3+(IK-1)*5,NISKYFIL)= - NI_S(I,IK)*FZ
              FSKYFI(NINT)%P(4+(IK-1)*5,NISKYFIL)= ABS(NI_S(I,IK)*STIF)
              FSKYFI(NINT)%P(5+(IK-1)*5,NISKYFIL)= IIIS(I,IK)
            ENDDO
           ENDIF
C
           IF(ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT >0.AND.
     .          (TT>=TANIM.OR.TT>=TOUTP.OR.TT>=H3D_DATA%TH3D.OR.
     .              (MANIM>=4.AND.MANIM<=15).OR. H3D_DATA%MH3D /= 0))THEN
#include "lockon.inc"
            DO IK=1,8
              J1 = III(I,IK)
              FCONT(1,J1) =FCONT(1,J1) + NI_M(I,IK)*FX
              FCONT(2,J1) =FCONT(2,J1) + NI_M(I,IK)*FY
              FCONT(3,J1) =FCONT(3,J1) + NI_M(I,IK)*FZ
            ENDDO
#include "lockoff.inc"
           ENDIF
          END IF
        ENDIF
      ENDDO
C
C-----------------------------------------------
C         TH
C-----------------------------------------------
#include "lockon.inc"
        FSAV(1) = FSAV(1) + FSAV1*DT12
        FSAV(2) = FSAV(2) + FSAV2*DT12
        FSAV(3) = FSAV(3) + FSAV3*DT12
        FSAV(4) = FSAV(4) + FSAV4*DT12
        FSAV(5) = FSAV(5) + FSAV5*DT12
        FSAV(6) = FSAV(6) + FSAV6*DT12
        ECONTV = ECONTV + DT1*ECONVT
        ECONT  = ECONT + DT1*ECONTT
#include "lockoff.inc"
C
      RETURN
      END

Chd|====================================================================
Chd|  I17LLL_ASS                    source/interfaces/int17/i17for3.F
Chd|-- called by -----------
Chd|        I17FOR3                       source/interfaces/int17/i17for3.F
Chd|-- calls ---------------
Chd|        ASS2SORT                      source/assembly/ass2sort.F    
Chd|====================================================================
      SUBROUTINE I17LLL_ASS(NSKYI17 ,ISKYI17 ,FSKYI17, FROTS, NMES,
     2                      LSKYI17 )
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSKYI17, NMES, LSKYI17, ISKYI17(*)
C     REAL
      my_real
     .        FSKYI17(LSKYI17,4),FROTS(7,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, JJ1, JJ2, K, KK, L, N, NN, IDIFF,
     .        ADSKYI(0:NMES+1)
      my_real
     .        FF, FSKYIT(NSKYI17) , BID  
C-----------------------------------------------
      BID = ZERO
      DO N = 1, NMES+1
        ADSKYI(N) = 0
      END DO
      DO I=1,NSKYI17
        N = ISKYI17(I)+1
        ADSKYI(N) = ADSKYI(N)+1
      END DO
      ADSKYI(0) = 1
      ADSKYI(1) = 1
      DO N = 1, NMES
        NN = N+1
        ADSKYI(NN) = ADSKYI(NN) + ADSKYI(N)
      END DO
C
      DO I=1,NSKYI17
        N = ISKYI17(I)
        J  = ADSKYI(N)
        ISKYI17(I)  = J
        ADSKYI(N) = ADSKYI(N) + 1
      END DO
C
      DO L = 1, 4
        DO I=1,NSKYI17
          J = ISKYI17(I)
          FSKYIT(J) = FSKYI17(I,L)
        END DO
        DO I=1,NSKYI17
          FSKYI17(I,L) = FSKYIT(I)
        END DO
      ENDDO
C
      DO I=1,NSKYI17
        ISKYI17(I) = 0
      END DO
C
      DO N = 1, NMES
        NN = N-1
        IDIFF = ADSKYI(N)-ADSKYI(NN)
        IF(IDIFF==1)THEN
          K = ADSKYI(NN)
          FROTS(1,N) = FROTS(1,N) + FSKYI17(K,1)
          FROTS(2,N) = FROTS(2,N) + FSKYI17(K,2)
          FROTS(3,N) = FROTS(3,N) + FSKYI17(K,3)
          FROTS(4,N) = FROTS(4,N) + FSKYI17(K,4)
        ELSEIF(IDIFF>1.AND.IDIFF<20)THEN
          JJ1 = ADSKYI(NN)
          JJ2 = ADSKYI(N)-1
          DO K=JJ1,JJ2-1
            DO KK=K+1,JJ2
              DO L=1,4
                IF(FSKYI17(KK,L)>FSKYI17(K,L))THEN
                  FF = FSKYI17(KK,L)
                  FSKYI17(KK,L) = FSKYI17(K,L)
                  FSKYI17(K,L)  = FF
                END IF
              END DO
            END DO
          END DO
          DO K=JJ1,JJ2
            FROTS(1,N)= FROTS(1,N)+ FSKYI17(K,1)
            FROTS(2,N)= FROTS(2,N)+ FSKYI17(K,2)
            FROTS(3,N)= FROTS(3,N)+ FSKYI17(K,3)
            FROTS(4,N)= FROTS(4,N)+ FSKYI17(K,4)
          ENDDO
        ELSEIF(IDIFF>=20)THEN
C qsort necessaire si > 20
          JJ1 = ADSKYI(NN)
          JJ2 = ADSKYI(N)-1
          CALL ASS2SORT(FSKYI17,JJ1,JJ2,FSKYIT,4)
          DO K=JJ1,JJ2
            FROTS(1,N)= FROTS(1,N)+ FSKYI17(K,1)
            FROTS(2,N)= FROTS(2,N)+ FSKYI17(K,2)
            FROTS(3,N)= FROTS(3,N)+ FSKYI17(K,3)
            FROTS(4,N)= FROTS(4,N)+ FSKYI17(K,4)
          ENDDO
        END IF
      END DO
C
      RETURN
      END
