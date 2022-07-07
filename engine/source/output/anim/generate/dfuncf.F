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
Chd|  DFUNCF                        source/output/anim/generate/dfuncf.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        SPMD_R4GET_PARTN              source/mpi/anim/spmd_r4get_partn.F
Chd|        WRITE_R_C                     source/output/tools/sortie_c.c
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE DFUNCF(ELBUF_TAB ,FUNC  ,IFUNC  ,IPARG    ,GEO  ,
     .                  IXT       ,IXP   ,IXR    ,MASS     ,PM   ,
     .                  EL2FA     ,NBF   ,IADP   ,NBPART   ,EHOUR,
     .                  ANIM      ,IADG  ,XFUNC1 ,NANIM1D_L,IGEO )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "scr23_c.inc"
#include      "param_c.inc"
#include      "rnur_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   FUNC(*), MASS(*) ,PM(NPROPM,*), GEO(NPROPG,*),
     .   EHOUR(*),ANIM(*), XFUNC1(10,*)
      INTEGER IPARG(NPARG,*),EL2FA(*),
     .        IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*),IFUNC,NBF,
     .        IADP(*),NBPART,IADG(NSPMD,*),NANIM1D_L,NBF2,
     .        IGEO(NPROPGI,*)
      INTEGER BUF
      REAL WAL(NBF+NANIM1D_L)
C
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   EVAR(MVSIZ),
     .   OFF, P, VONM2, VONM, S1, S2, S12, S3, VALUE,
     .   A1,B1,B2,B3,YEQ,F1,M1,M2,M3, XM,     
     .   FOR, AREA, FEQ, EPLAS    
      INTEGER I, II, NG, NEL, NFT, IAD, ITY, LFT, NPT, ISS, ISC,
     .        IADD, N, J, LLT, MLW, NB1, NB2, NB3, NB4, NB5, 
     .        NB6, NB7, NB8, NB9, NB10, NB11, NB12, NB13, NB14, NB15,
     .        NB16, LLL,NUVAR,IGTYP,
     .        ISTRAIN,NN, K1, K2,JTURB,MT,JALE, IMID, IALEL,IPID,
     .        NN1,NN2,NN3,NN4,NN5,NN6,NN7,NN8,NN9,NN10,NF,
     .        OFFSET,K,INC,KK,IHBE,ISROT,ILAYER,IR,IS,JJ(6),IPT
      REAL R4
C
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(L_BUFEL_),POINTER :: LBUF
C-----------------------------------------------
C
      NN1 = 1
      NN3 = 1
      NN4 = NN3 
      NN5 = NN4 
      NN6 = NN5 
      NN7 = NN6 + NUMELT
      NN8 = NN7 + NUMELP
      NN9 = NN8 + NUMELR
      NN10= NN9 
C
      DO NG=1,NGROUP
        MLW   =IPARG(1,NG)
        NEL   =IPARG(2,NG)
        ITY   =IPARG(5,NG)
        IGTYP =IPARG(38,NG)
C---
        GBUF => ELBUF_TAB(NG)%GBUF
C---
        DO OFFSET = 0,NEL-1,NVSIZ
          NFT   =IPARG(3,NG) + OFFSET
          LFT=1
          LLT=MIN(NVSIZ,NEL-OFFSET)
!
          DO I=1,6
            JJ(I) = NEL*(I-1)
          ENDDO
!
C-----------------------------------------------
C       TRUSS
C-----------------------------------------------
        IF(ITY.EQ.4)THEN
          IF(IFUNC.EQ.1)THEN
           IF(MLW.NE.1)THEN
            DO  I=LFT,LLT
              N = I + NFT
              OFF = GBUF%OFF(I)
              FUNC(EL2FA(NN6+N)) = GBUF%PLA(I)
            ENDDO
           ELSE
            DO  I=LFT,LLT
              N = I + NFT 
              FUNC(EL2FA(NN6+N)) = ZERO
            ENDDO
           ENDIF
          ELSEIF(IFUNC.EQ.3)THEN
            DO I=LFT,LLT
             N = I + NFT
             FUNC(EL2FA(NN6+N))=GBUF%EINT(I)/
     .            MAX(EM30,MASS(EL2FA(NN6+N)))
            ENDDO
          ELSEIF(IFUNC.EQ.7)THEN
            DO I=LFT,LLT
             N = I + NFT
             FOR = GBUF%FOR(I)
             AREA = GBUF%AREA(I)
             FEQ = FOR*FOR
             FUNC(EL2FA(NN6+N)) = SQRT(FEQ)/AREA
            ENDDO
          ELSEIF(IFUNC.EQ.14)THEN  
            DO I=LFT,LLT
             N = I + NFT                   
             FUNC(EL2FA(NN6+N)) = GBUF%FOR(I) / GBUF%AREA(I)
            ENDDO
          ELSEIF(IFUNC.EQ.20)THEN 
            IF(GBUF%G_DT>0)THEN                      
              DO I=LFT,LLT
                N = I + NFT
                FUNC(EL2FA(NN6+N)) = GBUF%DT(I)
             ENDDO 
            ENDIF
          ELSEIF ((IFUNC.EQ.21).AND.(GBUF%G_ISMS>0)) THEN                       
              DO I=LFT,LLT
                N = I + NFT
                FUNC(EL2FA(NN6+N)) = GBUF%ISMS(I)
              ENDDO              
          ELSEIF (IFUNC == 22) THEN 
            DO I=LFT,LLT
              N = I + NFT
              IF (GBUF%G_OFF > 0) THEN
                IF(GBUF%OFF(I) > ONE) THEN
                  FUNC(EL2FA(NN6+N)) = GBUF%OFF(I) - ONE
                ELSEIF((GBUF%OFF(I) >= ZERO .AND. GBUF%OFF(I) <= ONE)) THEN
                  FUNC(EL2FA(NN6+N)) = GBUF%OFF(I)
                ELSE
                  FUNC(EL2FA(NN6+N)) = -ONE
                ENDIF
              ENDIF
            ENDDO   
          ELSEIF (IFUNC == 123) THEN 
            DO I=LFT,LLT
              N = I + NFT
              FUNC(EL2FA(NN6+N)) = GBUF%STRA(I)
            ENDDO   
          ELSE
            DO I=LFT,LLT
             N = I + NFT
             FUNC(EL2FA(NN6+N)) = ZERO
            ENDDO
          ENDIF
C-----------------------------------------------
C       POUTRES
C-----------------------------------------------
        ELSEIF(ITY.EQ.5)THEN
          IF (IFUNC == 1) THEN
           IF (MLW /= 1) THEN
             IF (IGTYP == 18) THEN
               NPT  = IPARG(6,NG)
               DO  I=LFT,LLT
                 N = I + NFT
                 EPLAS = ZERO
                 DO K = 1,NPT
                   ILAYER=1
                   IR = 1
                   IS = 1
                   LBUF => ELBUF_TAB(NG)%BUFLY(ILAYER)%LBUF(IR,IS,K)
                   EPLAS = EPLAS + LBUF%PLA(I)
                 ENDDO
                 FUNC(EL2FA(NN7+N)) = EPLAS/NPT
               ENDDO
             ELSE
            DO  I=LFT,LLT
              N = I + NFT
              OFF = GBUF%OFF(I)
              FUNC(EL2FA(NN7+N)) = GBUF%PLA(I)
            ENDDO
             ENDIF
           ELSE
            DO  I=LFT,LLT
              N = I + NFT
              FUNC(EL2FA(NN7+N)) = ZERO
            ENDDO
           ENDIF
          ELSEIF(IFUNC.EQ.3)THEN
            DO I=LFT,LLT
             N = I + NFT
             FUNC(EL2FA(NN7+N)) = (GBUF%EINT(I) + GBUF%EINT(I+LLT)) / MAX(EM30,MASS(EL2FA(NN7+N)))
            ENDDO
          ELSEIF(IFUNC.EQ.7)THEN
            DO I=LFT,LLT
             N = I + NFT
             A1 = GEO(1,IXP(5,N))
             B1 = GEO(2,IXP(5,N))
             B2 = GEO(18,IXP(5,N))
             B3 = GEO(4,IXP(5,N))
             F1 = GBUF%FOR(JJ(1)+I)
             M1 = GBUF%MOM(JJ(1) + I)
             M2 = GBUF%MOM(JJ(2) + I)
             M3 = GBUF%MOM(JJ(3) + I)
             YEQ= F1*F1 + THREE* A1 *
     +                  ( M1*M1 / MAX(B3,EM30)
     +                  + M2*M2 / MAX(B1,EM30)
     +                  + M3*M3 / MAX(B2,EM30) )
             FUNC(EL2FA(NN7+N)) = SQRT(YEQ)/A1
            ENDDO
          ELSEIF(IFUNC.EQ.14)THEN
            DO I=LFT,LLT
             N = I + NFT
             FUNC(EL2FA(NN7+N)) = GBUF%FOR(JJ(1)+I) / GEO(1,IXP(5,N))
            ENDDO
          ELSEIF(IFUNC.EQ.17)THEN
            DO I=LFT,LLT
             N = I + NFT
             FUNC(EL2FA(NN7+N)) = GBUF%FOR(JJ(2)+I) / GEO(1,IXP(5,N))
            ENDDO
          ELSEIF(IFUNC.EQ.19)THEN
            DO I=LFT,LLT
             N = I + NFT
             FUNC(EL2FA(NN7+N)) = GBUF%FOR(JJ(3)+I) / GEO(1,IXP(5,N))
            ENDDO
          ELSEIF(IFUNC.EQ.20)THEN                       
            DO I=LFT,LLT
              N = I + NFT
              FUNC(EL2FA(NN7+N)) = GBUF%DT(I)
            ENDDO
          ELSEIF ((IFUNC.EQ.21).AND.(GBUF%G_ISMS>0)) THEN                       
              DO I=LFT,LLT
                N = I + NFT
                FUNC(EL2FA(NN7+N)) = GBUF%ISMS(I)
              ENDDO        
          ELSEIF (IFUNC == 22) THEN 
            DO I=LFT,LLT
              N = I + NFT
              IF (GBUF%G_OFF > 0) THEN
                IF(GBUF%OFF(I) > ONE) THEN
                  FUNC(EL2FA(NN7+N)) = GBUF%OFF(I) - ONE
                ELSEIF((GBUF%OFF(I) >= ZERO .AND. GBUF%OFF(I) <= ONE)) THEN
                  FUNC(EL2FA(NN7+N)) = GBUF%OFF(I)
                ELSE
                  FUNC(EL2FA(NN7+N)) = -ONE
                ENDIF
              ENDIF
            ENDDO              
          ELSEIF (IFUNC >= 23 .AND. IFUNC <= 122) THEN   
            IPT = MOD((IFUNC - 22), 100)
            IF (IPT == 0) IPT = 100
            IF (MLW /= 1) THEN
              IF (IGTYP == 18) THEN
                NPT  = IPARG(6,NG)
                ILAYER=1
                IR = 1
                IS = 1
                IF (IPT <= NPT) THEN
                  LBUF => ELBUF_TAB(NG)%BUFLY(ILAYER)%LBUF(IR,IS,IPT)
                  DO  I=LFT,LLT
                    N = I + NFT
                    FUNC(EL2FA(NN7+N)) = LBUF%PLA(I)
                  ENDDO
                ELSE
                  DO I=LFT,LLT
                    N = I + NFT
                    FUNC(EL2FA(NN7+N)) = ZERO
                  ENDDO
                ENDIF ! IF (IPT <= NPT)
              ENDIF ! IF (IGTYP == 18)
            ENDIF ! IF (MLW /= 1)
          ELSE
            DO I=LFT,LLT
             N = I + NFT
             FUNC(EL2FA(NN7+N)) = ZERO
            ENDDO
          ENDIF
C-----------------------------------------------
C       RESSORTS
C-----------------------------------------------
        ELSEIF(ITY.EQ.6)THEN
          IF(IFUNC.EQ.3)THEN
           IF (MLW.EQ.1) THEN
            XM = ONE/GEO(1,IXR(1,1+NFT))
            DO  I=LFT,LLT
              N = I + NFT
C             XM cannot be zero (was checked in starter).
              FUNC(EL2FA(NN8+N)) = GBUF%EINT(I)*XM
            ENDDO
           ELSEIF (MLW.EQ.2) THEN
            XM = ONE/GEO(1,IXR(1,1+NFT))
            DO  I=LFT,LLT
              N = I + NFT
C             XM cannot be zero (was checked in starter).
              FUNC(EL2FA(NN8+N)) = GBUF%EINT(I)*XM
            ENDDO
           ELSEIF (MLW.EQ.3) THEN
            XM = ONE/GEO(1,IXR(1,1+NFT))
            DO  I=LFT,LLT
              N = I + NFT
C             XM cannot be zero (was checked in starter).
              FUNC(EL2FA(NN8+N)) = GBUF%EINT(I)*XM
            ENDDO
           ELSEIF (MLW.EQ.4) THEN
            XM = ONE/GEO(1,IXR(1,1+NFT))
            DO  I=LFT,LLT
              N = I + NFT
C             XM cannot be zero (was checked in starter).
              FUNC(EL2FA(NN8+N)) = GBUF%EINT(I)*XM
            ENDDO
           ELSEIF (MLW.EQ.5) THEN
C           user springs.
            DO  I=LFT,LLT
              N = I + NFT
              FUNC(EL2FA(NN8+N)) = GBUF%EINT(I)/MAX(EM30,GBUF%MASS(I))
            ENDDO
           ELSEIF (MLW.EQ.6) THEN
            XM = ONE/GEO(1,IXR(1,1+NFT))
            DO  I=LFT,LLT
              N = I + NFT
C             XM cannot be zero (was checked in starter).
              FUNC(EL2FA(NN8+N)) = GBUF%EINT(I)*XM
            ENDDO
           ELSEIF (MLW.EQ.7) THEN
            XM = ONE/GEO(1,IXR(1,1+NFT))
            DO  I=LFT,LLT
              N = I + NFT
              FUNC(EL2FA(NN8+N)) = GBUF%EINT(I)*XM
            ENDDO
           ENDIF
          ELSEIF(IFUNC.EQ.11)THEN
            DO  I=LFT,LLT
              N = I + NFT
              FUNC(EL2FA(NN8+N)) = ANIM(N)
            ENDDO
          ELSEIF(IFUNC.EQ.12)THEN
            KK = NUMELR * ANIM_FE(11)
            DO  I=LFT,LLT
              N = I + NFT
              FUNC(EL2FA(NN8+N)) = ANIM(N+KK)
            ENDDO
          ELSEIF(IFUNC.EQ.13)THEN
            KK = NUMELR * (ANIM_FE(11)+ANIM_FE(12))
            DO  I=LFT,LLT
              N = I + NFT
              FUNC(EL2FA(NN8+N)) = ANIM(N+KK)
            ENDDO
          ELSEIF(IFUNC.EQ.20 .AND. GBUF%G_DT.NE.0)THEN                       
            DO I=LFT,LLT
              N = I + NFT
              FUNC(EL2FA(NN8+N)) = GBUF%DT(I)
            ENDDO
          ELSEIF ((IFUNC.EQ.21).AND.(GBUF%G_ISMS>0)) THEN                       
              DO I=LFT,LLT
                N = I + NFT
                FUNC(EL2FA(NN8+N)) = GBUF%ISMS(I)
              ENDDO       
          ELSEIF (IFUNC == 22) THEN 
            DO I=LFT,LLT
              N = I + NFT
              IF (GBUF%G_OFF > 0) THEN
                IF(GBUF%OFF(I) > ONE) THEN
                  FUNC(EL2FA(NN8+N)) = GBUF%OFF(I) - ONE
                ELSEIF((GBUF%OFF(I) >= ZERO .AND. GBUF%OFF(I) <= ONE)) THEN
                  FUNC(EL2FA(NN8+N)) = GBUF%OFF(I)
                ELSE
                  FUNC(EL2FA(NN8+N)) = -ONE
                ENDIF
              ENDIF
            ENDDO                       
          ELSE
            DO I=LFT,LLT
             N = I + NFT
             FUNC(EL2FA(NN8+N)) = 0.
            ENDDO
          ENDIF
          IF(MLW.EQ.3)THEN
            DO I=LFT,LLT
             N = I + NFT
             FUNC(EL2FA(NN8+N)+1) = FUNC(EL2FA(NN8+N))
            ENDDO
          ENDIF
C
        ENDIF
C-----------------------------------------------
C       FIN DE BOUCLE SUR LES OFFSET
C-----------------------------------------------
       END DO
      ENDDO ! DO NG=1,NGROUP
C-----------------------------------------------
      IF (NSPMD == 1) THEN
        DO N=1,NBF
           R4 = FUNC(N)
           CALL WRITE_R_C(R4,1)
        ENDDO
C + X-ELEMENTS
        IF (IFUNC.EQ.3) THEN
         DO N=1,NANIM1D
           VALUE  = XFUNC1(1,N)
           R4 = VALUE
           CALL WRITE_R_C(R4,1)
         ENDDO
        ELSE
         DO N=1,NANIM1D
           R4 = ZERO
           CALL WRITE_R_C(R4,1)
         ENDDO
        ENDIF
      ELSE
        DO N = 1, NBF
          WAL(N) = FUNC(N)
        ENDDO
        IF (IFUNC.EQ.3) THEN
         DO N=1,NANIM1D_L
           VALUE  = XFUNC1(1,N)
           WAL(NBF+N)=VALUE
         ENDDO
        ELSE
         DO N=1,NANIM1D_L
           WAL(NBF+N)=0.
         ENDDO
        ENDIF
        NBF2=NBF+NANIM1D_L
        IF (ISPMD.EQ.0) THEN
          BUF = NB1DG+NANIM1D
        ELSE
          BUF=1
        ENDIF
          CALL SPMD_R4GET_PARTN(1,NBF2,NBPART,IADG,WAL,BUF)
      ENDIF
 
      RETURN
      END
Chd|====================================================================
Chd|  DFUNGPS1                      source/output/anim/generate/dfuncf.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE DFUNGPS1(ELBUF_TAB ,FUNC     ,IFUNC   ,IPARG   ,GEO     ,
     .                    IXS       ,IXS10    ,IXS16   ,IXS20   ,IXQ     ,
     .                    IXC       ,IXTG     ,IXT     ,IXP     ,IXR     ,
     .                    ITAGPS    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr01_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   FUNC(*),GEO(NPROPG,*)
      INTEGER IPARG(NPARG,*),IFUNC,
     .        IXS(NIXS,*),IXQ(NIXQ,*),IXC(NIXC,*),IXTG(NIXTG,*),
     .        IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*),
     .        IXS10(6,*) ,IXS16(8,*) ,IXS20(12,*) ,ITAGPS(*) 
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   EVAR(MVSIZ),
     .   OFF, P, VONM2, VONM, S1, S2, S12, S3, VALUE,
     .   A1,B1,B2,B3,YEQ,F1,M1,M2,M3,FOR,AREA
      INTEGER I,J,K,II,N,NN, NG, NEL, ISC,MLW,NN1,  
     .        INOD, ISOLNOD, IPRT, LIAD, NPTR, NPTS, NPTT, IPT,
     .        IS, IR, IT, NPTG,NC(20,MVSIZ),NNOD,IHBE,MPT,JJ(6)
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
C=======================================================================
      DO 900 NG=1,NGROUP
          CALL INITBUF (IPARG    ,NG      ,                    
     2          MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,  
     3          NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,  
     4          JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,  
     5          NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,  
     6          IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,  
     7          ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
        ISOLNOD = IPARG(28,NG)
        LFT=1
        LLT=NEL
        NNOD = 0
!
        DO I=1,6
          JJ(I) = NEL*(I-1)
        ENDDO
!
C-----------------------------------------------
C       SOLID 8N
C-----------------------------------------------
          IF (ITY.EQ.1) THEN
            NNOD = ISOLNOD
            GBUF => ELBUF_TAB(NG)%GBUF
            DO I=LFT,LLT
                N  = I + NFT
                P = - (GBUF%SIG(JJ(1) + I)
     .              +  GBUF%SIG(JJ(2) + I)
     .              +  GBUF%SIG(JJ(3) + I)) * THIRD
                VALUE = P
                IF (IFUNC.EQ.2) THEN
                  S1 = GBUF%SIG(JJ(1) + I)+P
                  S2 = GBUF%SIG(JJ(2) + I)+P
                  S3 = GBUF%SIG(JJ(3) + I)+P
                  VONM2= THREE*(GBUF%SIG(JJ(4) + I)**2 +
     .                          GBUF%SIG(JJ(5) + I)**2 +
     .                          GBUF%SIG(JJ(6) + I)**2 +
     .                   HALF*(S1*S1+S2*S2+S3*S3))
                  VONM= SQRT(VONM2)
                  VALUE = VONM
                ENDIF
                EVAR(I) = VALUE
                IF(ISOLNOD.EQ.8)THEN
	                 DO J = 1,ISOLNOD
	                   NC(J,I) = IXS(J+1,N)
                  ENDDO
                ELSEIF(ISOLNOD.EQ.4)THEN
                  NC(1,I)=IXS(2,N)
                  NC(2,I)=IXS(4,N)
                  NC(3,I)=IXS(7,N)
                  NC(4,I)=IXS(6,N)
                ELSEIF(ISOLNOD.EQ.6)THEN
                  NC(1,I)=IXS(2,N)
                  NC(2,I)=IXS(3,N)
                  NC(3,I)=IXS(4,N)
                  NC(4,I)=IXS(6,N)
                  NC(5,I)=IXS(7,N)
                  NC(6,I)=IXS(8,N)
                ELSEIF(ISOLNOD.EQ.10)THEN
                  NC(1,I)=IXS(2,N)
                  NC(2,I)=IXS(4,N)
                  NC(3,I)=IXS(7,N)
                  NC(4,I)=IXS(6,N)
		                NN1 = N - NUMELS8
                  DO J=1,6
C                   IF (IXS10(J,NN1).GT.0) THEN
                    NC(J+4,I) = IXS10(J,NN1)
C                   ENDIF
                  ENDDO
                ELSEIF(ISOLNOD.EQ.16)THEN
	                 DO J = 1,8
	                   NC(J,I) = IXS(J+1,N)
                  ENDDO
		                NN1 = N - (NUMELS8+NUMELS10+NUMELS20)
                  DO J=1,8
                   NC(J+8,I) = IXS16(J,NN1)
                  ENDDO
                ELSEIF(ISOLNOD.EQ.20)THEN
	                 DO J = 1,8
	                  NC(J,I) = IXS(J+1,N)
                  ENDDO
		                NN1 = N - (NUMELS8+NUMELS10)
                  DO J=1,12
                    NC(J+8,I) = IXS20(J,NN1)
                  ENDDO
                ENDIF
              ENDDO 
C
C-----------------------------------------------
C             QUAD
C-----------------------------------------------
          ELSEIF(ITY.EQ.2)THEN
            GBUF => ELBUF_TAB(NG)%GBUF
            NNOD = 4
            DO I=LFT,LLT
              N  = I + NFT
              P = - (GBUF%SIG(JJ(1) + I)
     .             + GBUF%SIG(JJ(2) + I)
     .             + GBUF%SIG(JJ(3) + I) ) * THIRD
              VALUE = P
              IF (IFUNC.EQ.2) THEN
                S1 = GBUF%SIG(JJ(1) + I) + P
                S2 = GBUF%SIG(JJ(2) + I) + P
                S3 = GBUF%SIG(JJ(3) + I) + P
                VONM2= THREE*(GBUF%SIG(JJ(4) + I)**2 +
     .                        GBUF%SIG(JJ(5) + I)**2 +
     .                        GBUF%SIG(JJ(6) + I)**2 +
     .                 HALF*(S1*S1+S2*S2+S3*S3))
                VONM= SQRT(VONM2)
                VALUE = VONM
              ENDIF
              EVAR(I) = VALUE
              DO J = 1,NNOD
                NC(J,I) = IXQ(J+1,N)
              ENDDO
            ENDDO 
C-----------------------------------------------
C         COQUES 3 N 4 N
C-----------------------------------------------
          ELSEIF(ITY.EQ.3.OR.ITY.EQ.7)THEN
            GBUF => ELBUF_TAB(NG)%GBUF
c
            DO I=LFT,LLT
              P = - (GBUF%FOR(JJ(1)+I) + GBUF%FOR(JJ(2)+I))*THIRD  
              VALUE = P
              IF (IFUNC.EQ.2) THEN
                S1 = GBUF%FOR(JJ(1)+I)                             
                S2 = GBUF%FOR(JJ(2)+I)                             
                S12= GBUF%FOR(JJ(3)+I)                             
                VONM2= S1*S1 + S2*S2 - S1*S2 + THREE*S12*S12
                VALUE = SQRT(VONM2)
              ENDIF
              EVAR(I) = VALUE
            ENDDO 
c
            IF (ITY.EQ.7) THEN
              NNOD=3
              DO I=LFT,LLT
                N = I + NFT
                DO J = 1,NNOD
                  NC(J,I) = IXTG(J+1,N)
                ENDDO
              ENDDO 
            ELSEIF(ITY.EQ.3)THEN
              NNOD=4
              DO I=LFT,LLT
                N = I + NFT
                DO J = 1,NNOD
                  NC(J,I) = IXC(J+1,N)
                ENDDO
              ENDDO 
            ENDIF
C-----------------------------------------------
C       TRUSS
C-----------------------------------------------
        ELSEIF (ITY == 4) THEN
          NNOD=2
          DO I=LFT,LLT
            N = I + NFT
            VALUE = ZERO
            NC(1,I) = 0
            NC(2,I) = 0
            IF (IFUNC == 2) THEN
              FOR = GBUF%FOR(I)
              AREA = GBUF%AREA(I)
              VALUE = SQRT(FOR*FOR)/AREA
              NC(1,I) = IXT(2,N)
              NC(2,I) = IXT(3,N)
            ENDIF
            EVAR(I) = VALUE
          ENDDO
C-----------------------
C     5. ELEMENTS POUTRES
C-----------------------
        ELSEIF (ITY == 5) THEN
          NNOD=2
          DO I=LFT,LLT
            N = I + NFT
            VALUE = ZERO
            NC(1,I) = 0
            NC(2,I) = 0
            IF (IFUNC == 2) THEN
              A1 = GEO(1,IXP(5,N))
              B1 = GEO(2,IXP(5,N))
              B2 = GEO(18,IXP(5,N))
              B3 = GEO(4,IXP(5,N))
              F1 = GBUF%FOR(JJ(1) + I)
              M1 = GBUF%MOM(JJ(1) + I)
              M2 = GBUF%MOM(JJ(2) + I)
              M3 = GBUF%MOM(JJ(3) + I)
              YEQ= F1*F1 + THREE* A1 *
     +                   ( M1*M1 / MAX(B3,EM30)
     +                   + M2*M2 / MAX(B1,EM30)
     +                   + M3*M3 / MAX(B2,EM30) )
              VALUE = SQRT(YEQ)/A1
              NC(1,I) = IXP(2,N)
              NC(2,I) = IXP(3,N)
            ENDIF
            EVAR(I) = VALUE
          ENDDO
        ENDIF ! IF (ITY)
C	
        DO I=LFT,LLT
          DO J=1,NNOD
            N = NC(J,I)
            IF (N > 0) THEN
              FUNC(N) = FUNC(N)+EVAR(I)
              ITAGPS(N) = ITAGPS(N)+1
            ENDIF
          ENDDO
        ENDDO
C
 900  CONTINUE
C-----------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  DFUNGPS2                      source/output/anim/generate/dfuncf.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE DFUNGPS2(ELBUF_TAB ,FUNC    ,IFUNC   ,IPARG   ,GEO     ,
     .                    IXS       ,IXS10   ,IXS16   ,IXS20   ,IXQ     ,
     .                    IXC       ,IXTG    ,IXT     ,IXP     ,IXR     ,
     .                    X         ,VGPS    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr01_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   FUNC(*),GEO(NPROPG,*),X(3,*),VGPS(*)
      INTEGER IPARG(NPARG,*),IFUNC,
     .        IXS(NIXS,*),IXQ(NIXQ,*),IXC(NIXC,*),IXTG(NIXTG,*),
     .        IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*),
     .        IXS10(6,*) ,IXS16(8,*) ,IXS20(12,*)  
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   EVAR(MVSIZ),VOL(MVSIZ),
     .   OFF, P, VONM2, VONM, S1, S2, S12, S3, VALUE,
     .   A1,B1,B2,B3,YEQ,F1,M1,M2,M3,FOR,AREA,
     .   XX1,XX2,XX3,YY1,YY2,YY3,ZZ1,ZZ2,ZZ3,THK0,AL0
      INTEGER I,II, NG, NEL, ISC,
     .        IADD, N, J,K, MLW,  
     .        NN, MT, IMID, IALEL,IPID,
     .        NN1,NN2,NN3,NN4,NN5,NN6,NN7,NN8,NN9,NN10,NF,
     .        OFFSET,INC,KK, IUS, NUVAR,
     .        INOD, ISOLNOD, IPRT, LIAD, NPTR, NPTS, NPTT, IPT,
     .        IS, IR, IT, NPTG,NC(20,MVSIZ),NNOD,IEXPAN,IHBE,MPT,
     .        N1,N2,N3,N4,JJ(6)
      INTEGER MLW2                
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
C=======================================================================
      DO 900 NG=1,NGROUP
          CALL INITBUF (IPARG    ,NG      ,                    
     2          MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,  
     3          NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,  
     4          JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,  
     5          NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,  
     6          IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,  
     7          ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
        ISOLNOD = IPARG(28,NG)
        LFT=1
        LLT=NEL
        NNOD = 0
!
        DO I=1,6
          JJ(I) = NEL*(I-1)
        ENDDO
!
C-----------------------------------------------
C       SOLID 8N
C-----------------------------------------------
          IF(ITY.EQ.1)THEN
            NNOD = ISOLNOD
            GBUF => ELBUF_TAB(NG)%GBUF
            DO I=LFT,LLT
                N = I + NFT
                P = - (GBUF%SIG(JJ(1) + I)
     .               + GBUF%SIG(JJ(2) + I)
     .               + GBUF%SIG(JJ(3) + I)) * THIRD
                VALUE = P
                IF (IFUNC.EQ.2) THEN
                  S1 = GBUF%SIG(JJ(1) + I)+P
                  S2 = GBUF%SIG(JJ(2) + I)+P
                  S3 = GBUF%SIG(JJ(3) + I)+P
                  VONM2= THREE*(GBUF%SIG(JJ(4) + I)**2 +
     .                          GBUF%SIG(JJ(5) + I)**2 +
     .                          GBUF%SIG(JJ(6) + I)**2 +
     .                   HALF*(S1*S1+S2*S2+S3*S3))
                  VONM= SQRT(VONM2)
                  VALUE = VONM
                ENDIF
                EVAR(I) = VALUE
                OFF    = MIN(GBUF%OFF(I),ONE)
                VOL(I) = GBUF%VOL(I)*OFF
                IF(ISOLNOD.EQ.8)THEN
	                DO J = 1,ISOLNOD
	                  NC(J,I) = IXS(J+1,N)
                 ENDDO
                ELSEIF(ISOLNOD.EQ.4)THEN
                  NC(1,I)=IXS(2,N)
                  NC(2,I)=IXS(4,N)
                  NC(3,I)=IXS(7,N)
                  NC(4,I)=IXS(6,N)
                ELSEIF(ISOLNOD.EQ.6)THEN
                  NC(1,I)=IXS(2,N)
                  NC(2,I)=IXS(3,N)
                  NC(3,I)=IXS(4,N)
                  NC(4,I)=IXS(6,N)
                  NC(5,I)=IXS(7,N)
                  NC(6,I)=IXS(8,N)
                ELSEIF(ISOLNOD.EQ.10)THEN
                  NC(1,I)=IXS(2,N)
                  NC(2,I)=IXS(4,N)
                  NC(3,I)=IXS(7,N)
                  NC(4,I)=IXS(6,N)
		                NN1 = N - NUMELS8
                  DO J=1,6
C                   IF (IXS10(J,NN1).GT.0) THEN
                    NC(J+4,I) = IXS10(J,NN1)
C                   ENDIF
                  ENDDO
                ELSEIF(ISOLNOD.EQ.16)THEN
                 DO J = 1,8
                  NC(J,I) = IXS(J+1,N)
                 ENDDO
                 NN1 = N - (NUMELS8+NUMELS10+NUMELS20)
                  DO J=1,8
                   NC(J+8,I) = IXS16(J,NN1)
                  ENDDO
                ELSEIF(ISOLNOD.EQ.20)THEN
                  DO J = 1,8
                    NC(J,I) = IXS(J+1,N)
                  ENDDO
                  NN1 = N - (NUMELS8+NUMELS10)
                  DO J=1,12
                    NC(J+8,I) = IXS20(J,NN1)
                  ENDDO
                ENDIF
            ENDDO 
C
C-----------------------------------------------
C         QUAD
C-----------------------------------------------
          ELSEIF(ITY.EQ.2)THEN
            NNOD = 4
            GBUF => ELBUF_TAB(NG)%GBUF
            DO I=LFT,LLT
                N = I + NFT
                P = - (GBUF%SIG(JJ(1) + I)
     .               + GBUF%SIG(JJ(2) + I)
     .               + GBUF%SIG(JJ(3) + I)) * THIRD
                VALUE = P
                IF (IFUNC.EQ.2) THEN
                  S1 = GBUF%SIG(JJ(1) + I) + P
                  S2 = GBUF%SIG(JJ(2) + I) + P
                  S3 = GBUF%SIG(JJ(3) + I) + P
                  VONM2= THREE*(GBUF%SIG(JJ(4) + I)**2 +
     .                          GBUF%SIG(JJ(5) + I)**2 +
     .                          GBUF%SIG(JJ(6) + I)**2 +
     .                       HALF*(S1*S1+S2*S2+S3*S3))
                  VONM= SQRT(VONM2)
                  VALUE = VONM
                ENDIF
                EVAR(I) = VALUE
                OFF = MIN(GBUF%OFF(I),ONE)
                VOL(I) = GBUF%VOL(I)*OFF
	               DO J = 1,NNOD
	                 NC(J,I) = IXQ(J+1,N)
                ENDDO
            ENDDO 
C-----------------------------------------------
C       COQUES 3 N 4 N
C-----------------------------------------------
        ELSEIF(ITY.EQ.3.OR.ITY.EQ.7)THEN
           GBUF => ELBUF_TAB(NG)%GBUF
            DO I=LFT,LLT
               P = -(GBUF%FOR(JJ(1)+I)+ GBUF%FOR(JJ(2)+I)) * THIRD      
                VALUE = P
                IF(IFUNC.EQ.2) THEN
                 S1 = GBUF%FOR(JJ(1)+I)               
                 S2 = GBUF%FOR(JJ(2)+I)               
                 S12= GBUF%FOR(JJ(3)+I)               
                  VONM2= S1*S1 + S2*S2 - S1*S2 + THREE*S12*S12
                  VALUE = SQRT(VONM2)
                ENDIF
                EVAR(I) = VALUE
            ENDDO 
c
           IF(ITY.EQ.7)THEN
	           NNOD=3
            DO I=LFT,LLT
                N = I + NFT
                DO J = 1,NNOD
                  NC(J,I) = IXTG(J+1,N)
                ENDDO
                THK0 = GEO(1,IXTG(5,N))
                N1 = IXTG(2,N)
                N2 = IXTG(3,N)
                N3 = IXTG(4,N)
                XX1 = X(1,N2)-X(1,N1)
                YY1 = X(2,N2)-X(2,N1)
                ZZ1 = X(3,N2)-X(3,N1)
                XX2 = X(1,N3)-X(1,N1)
                YY2 = X(2,N3)-X(2,N1)
                ZZ2 = X(3,N3)-X(3,N1)
                XX3 = YY1*ZZ2 - ZZ1*YY2
                YY3 = ZZ1*XX2 - XX1*ZZ2
                ZZ3 = XX1*YY2 - YY1*XX2
                AREA = HALF*SQRT(XX3*XX3 + YY3*YY3 + ZZ3*ZZ3)
                OFF = MIN(GBUF%OFF(I),ONE)
                VOL(I) = THK0*AREA*OFF
            ENDDO 
           ELSEIF(ITY.EQ.3)THEN
            NNOD=4
            DO I=LFT,LLT
                N = I + NFT
	               DO J = 1,NNOD
	                 NC(J,I) = IXC(J+1,N)
                ENDDO
                THK0 = GEO(1,IXC(6,N))
                N1 = IXC(2,N)
                N2 = IXC(3,N)
                N3 = IXC(4,N)
                N4 = IXC(5,N)
                XX1 = X(1,N3)-X(1,N1)
                YY1 = X(2,N3)-X(2,N1)
                ZZ1 = X(3,N3)-X(3,N1)
                XX2 = X(1,N4)-X(1,N2)
                YY2 = X(2,N4)-X(2,N2)
                ZZ2 = X(3,N4)-X(3,N2)
                XX3 = YY1*ZZ2 - ZZ1*YY2
                YY3 = ZZ1*XX2 - XX1*ZZ2
                ZZ3 = XX1*YY2 - YY1*XX2
                AREA = HALF*SQRT(XX3*XX3 + YY3*YY3 + ZZ3*ZZ3)
                OFF = MIN(GBUF%OFF(I),ONE)
                VOL(I) = THK0*AREA*OFF
            ENDDO 
           ENDIF
C-----------------------------------------------
C       TRUSS
C-----------------------------------------------
        ELSEIF (ITY == 4) THEN
          NNOD=2
          DO I=LFT,LLT
            N = I + NFT
            VALUE = ZERO
            NC(1,I) = 0
            NC(2,I) = 0
            VOL(I) = ZERO
            IF (IFUNC == 2) THEN
              FOR = GBUF%FOR(I)
              AREA = GBUF%AREA(I)
              VALUE = SQRT(FOR*FOR)/AREA
              NC(1,I) = IXT(2,N)
              NC(2,I) = IXT(3,N)
              N1 = IXT(2,N)
              N2 = IXT(3,N)
              XX1 = X(1,N2)-X(1,N1)
              YY1 = X(2,N2)-X(2,N1)
              ZZ1 = X(3,N2)-X(3,N1)
              AL0  = HALF*SQRT(XX1*XX1 + YY1*YY1 + ZZ1*ZZ1)
              OFF = MIN(GBUF%OFF(I),ONE)
              VOL(I) = AL0*AREA*OFF
            ENDIF
            EVAR(I) = VALUE
          ENDDO
C-----------------------
C     5. ELEMENTS POUTRES
C-----------------------
        ELSEIF (ITY == 5) THEN
          NNOD=2
          DO I=LFT,LLT
            N = I + NFT
            VALUE = ZERO
            NC(1,I) = 0
            NC(2,I) = 0
            IF (IFUNC == 2) THEN
              A1 = GEO(1,IXP(5,N))
              B1 = GEO(2,IXP(5,N))
              B2 = GEO(18,IXP(5,N))
              B3 = GEO(4,IXP(5,N))
              F1 = GBUF%FOR(JJ(1) + I)
              M1 = GBUF%MOM(JJ(1) + I)
              M2 = GBUF%MOM(JJ(2) + I)
              M3 = GBUF%MOM(JJ(3) + I)
              YEQ= F1*F1 + THREE* A1 *
     +                   ( M1*M1 / MAX(B3,EM30)
     +                   + M2*M2 / MAX(B1,EM30)
     +                   + M3*M3 / MAX(B2,EM30) )
              VALUE = SQRT(YEQ)/A1
              NC(1,I) = IXP(2,N)
              NC(2,I) = IXP(3,N)
              N1 = IXP(2,N)
              N2 = IXP(3,N)
              XX1 = X(1,N2)-X(1,N1)
              YY1 = X(2,N2)-X(2,N1)
              ZZ1 = X(3,N2)-X(3,N1)
              AL0  = HALF*SQRT(XX1*XX1 + YY1*YY1 + ZZ1*ZZ1)
              OFF = MIN(GBUF%OFF(I),ONE)
              VOL(I) = AL0*A1*OFF
            ENDIF
            EVAR(I) = VALUE
          ENDDO
        ENDIF ! IF (ITY)
C	
        DO I=LFT,LLT
          DO J=1,NNOD
            N = NC(J,I)
            IF (N > 0 .AND. VOL(I) > ZERO) THEN
              FUNC(N) = FUNC(N)+EVAR(I)*VOL(I)
              VGPS(N) = VGPS(N)+VOL(I)
            ENDIF
          ENDDO
        ENDDO
C
 900  CONTINUE
C-----------------------------------------------
      RETURN
      END
