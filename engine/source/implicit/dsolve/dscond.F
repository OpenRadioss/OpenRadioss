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
Chd|  DSCOND                        source/implicit/dsolve/dscond.F
Chd|-- called by -----------
Chd|        IMP_BUCK                      source/implicit/imp_buck.F    
Chd|        IMP_DSOLV                     source/implicit/imp_dsolv.F   
Chd|        IMP_DSOLV_ITER                source/implicit/imp_dsolv.F   
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        DSDYN                         source/implicit/dsolve/dsdyn.F
Chd|        DSEXTR1                       source/implicit/dsolve/dsextr.F
Chd|        DSEXTR2                       source/implicit/dsolve/dsextr.F
Chd|        DSEXTR3                       source/implicit/dsolve/dsextr.F
Chd|        DSKCOND                       source/implicit/dsolve/dscond.F
Chd|        DSMCOND                       source/implicit/dsolve/dscond.F
Chd|        DSSTAT                        source/implicit/dsolve/dsstat.F
Chd|        DSWRKLI                       source/implicit/dsolve/dscond.F
Chd|        MY_ETIME                      source/system/machine.F       
Chd|        PROGCONDP_C                   source/system/progcond_c.c    
Chd|        PROGCOND_C                    source/system/progcond_c.c    
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE DSCOND(GRAPHE, K_DIAG, K_LT , IADK , JDIK   ,
     .                  MASS  , NDDL  , NSCM , B    , IOPT   ,
     .                  NM    , MAXITR, TOL  , NBLF , ITAB   ,
     .                  IWKLI , NDDLI7, IADI7, JDII7, I7_DIAG,
     .                  I7_LT , I7TOK )
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE DSGRAPH_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "units_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IADK(*), JDIK(*), NDDL, NSCM, IOPT, NM, MAXITR, NBLF,
     .        ITAB(*), IWKLI, NDDLI7, IADI7(*), JDII7(*), I7TOK(*)
      my_real
     .        K_DIAG(*), K_LT(*), MASS(*), B(NSCM,*), TOL, I7_DIAG(*),
     .        I7_LT(*)
      TYPE(PRGRAPH) :: GRAPHE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, DSLEN, NBSD, OFFSD, J, ISD, NDDL_I, NDDL_F, NSDMAX,
     .        NVD, LEN1, LEN2, LEN3, IMDIAG, ILVL, NDDL_I2, IC,
     .        ISC, IFILE, IREC, IAD, K, NR, II, JJ, NDDL_M, NSLEVEL,
     .        NSDEC, NSVMAX, IPRIM, ISUPEL, ISTEP, ISING, 
     .        LDMF, LDMI, IPRIB, IPRW
      my_real
     .        CUTFRQ
      REAL    DSTIME(20), TEXTR(2), TDYN(2), TSTAT(2),
     .        TKCOND(2), TKWRI(2), TMCOND(2), TMWRI(2), TMDWRI(2),
     .        TFCOND(2), TTOT(2), TTOTD(2)
*
      CHARACTER NAME*4
*
      INTEGER, DIMENSION(:), ALLOCATABLE :: KLIPIV
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE :: KL, KLB, KLC, KLI,
     .                                        ML, MLB, MLC, MLI,
     .                                        VD, VS, BL
      my_real
     .       , DIMENSION(:), ALLOCATABLE :: FREQ
C
      DO I=1,20
         DSTIME(I)=ZERO
      ENDDO
C
      DO I=1,NSPROC
         IPRIM=GRAPHE(I)%IPRI
         IF (IPRIM.LT.0) THEN
            IPRIM=ABS(IPRIM)-1
            IPRIB=1
         ELSE
            IPRIB=0
         ENDIF
C
         IF (IMACH.NE.3.AND.IPRIM.GT.0) THEN
            WRITE(IOUT,'(A)') '   ##########################'
            WRITE(IOUT,'(A14,I8)') '   SUBTREE: ',I
            WRITE(IOUT,'(A)') '   ##########################'
         ELSEIF (IMACH.EQ.3.AND.IPRIM.GT.0) THEN
            WRITE(IOUT,*)
            WRITE(IOUT,'(A)') 
     .      '   ##############################################'
            IF (IOPT.EQ.1) WRITE(IOUT,'(A36,I8)')
     .      '   STATIC CONDENSATION INFO - PROC: ',ISPMD
            IF (IOPT.EQ.2) WRITE(IOUT,'(A37,I8)')
     .      '   DYNAMIC CONDENSATION INFO - PROC: ',ISPMD
            WRITE(IOUT,'(A)') 
     .      '   ##############################################'
         ENDIF
C
         IREC=1
         IFILE=IFICDS+I-1
         GRAPHE(I)%IFILE=IFILE
         DSLEN=GRAPHE(I)%NSUPEL
         OFFSD=DSLEN
         NSLEVEL=GRAPHE(I)%NSLEVEL
         NSDEC=GRAPHE(I)%NSDEC
         NSVMAX=GRAPHE(I)%NSVMAX
         CUTFRQ=GRAPHE(I)%CUTFREQ
         ISUPEL=0
C
         IPRW=I
         IF (IMACH.EQ.3) IPRW=ISPMD
C  
         DO ILVL=1,NSLEVEL
C
            IF (IPRIM.GT.0) THEN
               WRITE(IOUT,'(A12,I8)') '     LEVEL: ',ILVL
               WRITE(IOUT,'(A)') '     ---------------'
            ENDIF
C  
            NBSD=NSDEC**(NSLEVEL-ILVL)
            OFFSD=OFFSD-NBSD
            DO J=1,NBSD
C Timer
               CALL MY_ETIME(DSTIME(1))
C
               ISD=OFFSD+J
C
               IF (IPRIM.GT.0) 
     .            WRITE(IOUT,'(A21,I10)') '       SUPERELEMENT: ',ISD
C
               NDDL_I=GRAPHE(I)%DGRAPH(ISD)%NDDL_I
               NDDL_F=GRAPHE(I)%DGRAPH(ISD)%NDDL_F
               NSDMAX=GRAPHE(I)%DGRAPH(ISD)%NSDMAX
C
               IF (IOPT.EQ.1) THEN
                  DO II=1,NSCM
                     DO JJ=1,NDDL_F
                        GRAPHE(I)%DGRAPH(ISD)%B(II,JJ)=ZERO
                     ENDDO
                  ENDDO
               ENDIF 
C
               ISUPEL=ISUPEL+1
               ISTEP=1
               IF (IMACH.NE.3.AND.IPRIB.EQ.1) THEN
                  CALL PROGCOND_C(ISUPEL, DSLEN, I, 
     .                            NSLEVEL-ILVL+1, ISD, ISTEP)
               ELSEIF (IPRIB.EQ.1) THEN
C On fait un affichage interactif de l'avancement des procs si NSPMD
C est inferieur a 16 
C On affiche rien sinon
                  IF (NSPMD.LE.16) 
     .               CALL PROGCONDP_C(ISUPEL, DSLEN, ISPMD+1, ISD)
               ENDIF
               IF (ILVL.EQ.1) THEN
C----------------------------------
C Condensation au niveau le plus bas : extraction de la rigidite 
C et de la masse
C----------------------------------
                  LEN1=NDDL_I+NDDL_F
                  LDMF=NDDL_F
                  LDMI=NDDL_I
                  ALLOCATE(KL(LEN1,LEN1))
                  IF (IOPT.EQ.1) THEN
                     ALLOCATE(BL(NSCM,NDDL_I), ML(0,0))
                  ELSEIF (IOPT.EQ.2) THEN
                     ALLOCATE(BL(0,0), ML(LEN1,1))
                  ENDIF
                  CALL DSEXTR1(K_DIAG, K_LT, IADK, JDIK, NDDL_I,
     .                         NDDL_F, NDDL, KL,   MASS, ML,
     .                         GRAPHE(I)%DGRAPH(ISD)%DDLS,
     .                         GRAPHE(I)%DGRAPH(ISD)%IFAC,
     .                         NSDMAX, NSPROC*DSLEN, NSCM,
     .                         BL,     B,            IOPT,
     .                         GRAPHE(I)%DGRAPH(ISD)%IFACM, NDDLI7,
     .                         IADI7, JDII7, I7_DIAG, I7_LT, I7TOK)
                  IMDIAG=1
                  NDDL_I2=NDDL_I
               ELSE
C----------------------------------
C Assemblage de la rigidite et de la masse a partir des enfants 
C----------------------------------
                  IMDIAG=0
C Nombre de ddls internes
                  NDDL_I2=NDDL_I
                  DO IC=1,NSDEC
                     ISC=GRAPHE(I)%DGRAPH(ISD)%CHILD(IC)
                     NDDL_I2=NDDL_I2+GRAPHE(I)%DGRAPH(ISC)%NMDYN
                  ENDDO
C
                  GRAPHE(I)%DGRAPH(ISD)%NDDL_M=NDDL_I2-NDDL_I
                  LEN1=NDDL_I2+NDDL_F
                  LDMF=NDDL_F
                  LDMI=NDDL_I2
                  NDDL_M=NDDL_I2-NDDL_I
                  ALLOCATE(KL(LEN1,LEN1))
                  IF (IOPT.EQ.1) THEN
                     ALLOCATE(BL(NSCM,NDDL_I2), ML(0,0))
                  ELSEIF (IOPT.EQ.2) THEN
                     ALLOCATE(BL(0,0), ML(LEN1,LEN1))
                  ENDIF
                  CALL DSEXTR2(GRAPHE, I,      ISD,    KL,   ML,
     .                         NDDL_F, NDDL_I, NDDL_M, NDDL, NSCM,
     .                         BL,     B     , IOPT  )
               ENDIF
C
               IF (IOPT.EQ.2) THEN
                  IF (IMDIAG.EQ.0) THEN
                     ALLOCATE(MLB(NDDL_F,NDDL_F), MLC(NDDL_F,NDDL_I2),
     .                        MLI(NDDL_I2, NDDL_I2))
                     CALL DSEXTR3(ML,     MLB,     MLC, MLI, NDDL_F, 
     .                            NDDL_I2)
                  ELSEIF (IMDIAG.EQ.1) THEN
                     ALLOCATE(MLB(NDDL_F,1), MLC(0,0), MLI(NDDL_I2,1))
                     DO II=1,NDDL_F
                        MLB(II,1)=ML(II,1)
                     ENDDO
                     DO II=1,NDDL_I2
                        MLI(II,1)=ML(NDDL_F+II,1)
                     ENDDO
                  ENDIF
               ENDIF
               DEALLOCATE(ML)
               ALLOCATE(KLB(NDDL_F,NDDL_F), KLC(NDDL_F,NDDL_I2),
     .                  KLI(NDDL_I2, NDDL_I2), KLIPIV(NDDL_I2))
               CALL DSEXTR3(KL,     KLB,     KLC, KLI, NDDL_F, 
     .                      NDDL_I2)
               DEALLOCATE(KL)
C
               CALL MY_ETIME(DSTIME(3))
               ISTEP=2
               IF (IMACH.NE.3.AND.IPRIB.EQ.1) 
     .            CALL PROGCOND_C(ISUPEL, DSLEN, I, 
     .                            NSLEVEL-ILVL+1, ISD, ISTEP)
C
               NVD=0
               IF (IOPT.EQ.2) THEN
C Modes dynamiques encastres
                  ALLOCATE(VD(NDDL_I2,NSVMAX), FREQ(NSVMAX))
                  IF (NDDL_I2.GT.0)
     .               CALL DSDYN(KLI,    MLI,  NDDL_F, NDDL_I2, NM,
     .                          MAXITR, TOL,  VD,     NSVMAX,  NVD,
     .                          CUTFRQ, NBLF, IPRIM,  IMDIAG,  IPRW,
     .                          ISD,    ILVL, FREQ  , LDMI   )
               ENDIF
               GRAPHE(I)%DGRAPH(ISD)%NMDYN=NVD
C
               CALL MY_ETIME(DSTIME(5))
C Modes statiques + equilibre forces sur ddls internes
               ALLOCATE(VS(NDDL_I2,NDDL_F))
               CALL DSSTAT(KLC,   KLI, NDDL_I2, NDDL_F, VS,
     .                     IPRIM, IOPT, NSCM, BL, 
     .                     GRAPHE(I)%DGRAPH(ISD)%IFI,
     .                     ISING ,KLIPIV)
C
               IF (ISING.GT.0) THEN
                  II=GRAPHE(I)%DGRAPH(ISD)%DDLS(NDDL_F+ISING)
                  CALL ANCMSG(MSGID=38,ANMODE=ANINFO_BLIND,
     .            I1=ISING,
     .            I2=ITAB(GRAPHE(I)%LSDDL(1,II)),
     .            I3=GRAPHE(I)%LSDDL(2,II))
                  CALL ARRET(2)
               ENDIF
C
               ISTEP=3
               IF (IMACH.NE.3.AND.IPRIB.EQ.1) 
     .            CALL PROGCOND_C(ISUPEL, DSLEN, I, 
     .                            NSLEVEL-ILVL+1, ISD, ISTEP)
C Ecriture de l'inverse de la matrice de rigidite interne
C sur fichier acces direct pour utilisation dans DSCDF
               IF (IWKLI.EQ.1) THEN
                  GRAPHE(I)%DGRAPH(ISD)%KIREC=IREC
                  CALL DSWRKLI(KLI, KLIPIV, NDDL_I2, IFILE, IREC)
               ENDIF
C Condensations des matrices et ecriture sur fichier acces direct
               LEN1=(NDDL_F+NVD)*(NDDL_F+NVD+1)/2
               GRAPHE(I)%DGRAPH(ISD)%KREC=IREC
               CALL MY_ETIME(DSTIME(7))
               CALL DSKCOND(KLB,  KLC,    NDDL_I2, NDDL_F, VS,
     .                      VD,   IFILE,  IREC,    NVD,    LEN1,
     .                      FREQ, DSTIME)
C
               CALL MY_ETIME(DSTIME(11))
               DEALLOCATE(KLB,KLC,KLI,KLIPIV)
C
               IF (IOPT.EQ.2) THEN
                  GRAPHE(I)%DGRAPH(ISD)%MREC=IREC
                  CALL DSMCOND(MLB,  MLC,    MLI,    NDDL_I2, NDDL_F, 
     .                         VS,   VD,     IFILE,  IREC,    NVD,
     .                         LEN1, IMDIAG, DSTIME, LDMF,    LDMI  )
                  DEALLOCATE(MLB,MLC,MLI)
               ENDIF
               IF (IOPT.EQ.1) THEN
                  DSTIME(13)=DSTIME(11)
                  DSTIME(14)=DSTIME(12)
               ENDIF
C Ecriture des modes sur fichier acces direct
               CALL MY_ETIME(DSTIME(15))
C
               GRAPHE(I)%DGRAPH(ISD)%MDREC=IREC
               DO II=1,NDDL_F
                  IAD=0
                  DO JJ=1,NDDL_I2/10
                     WRITE(IFILE,REC=IREC) (VS(IAD+K,II),K=1,10)
                     IAD=IAD+10
                     IREC=IREC+1
                  ENDDO
                  NR=NDDL_I2-(NDDL_I2/10)*10
                  IF (NR.GT.0) THEN
                     WRITE(IFILE,REC=IREC) (VS(IAD+K,II),K=1,NR)
                     IREC=IREC+1
                  ENDIF
               ENDDO
               DO II=1,NVD           
                  IAD=0
                  DO JJ=1,NDDL_I2/10
                     WRITE(IFILE,REC=IREC) (VD(IAD+K,II),K=1,10)
                     IAD=IAD+10
                     IREC=IREC+1
                  ENDDO
                  NR=NDDL_I2-(NDDL_I2/10)*10
                  IF (NR.GT.0) THEN
                     WRITE(IFILE,REC=IREC) (VD(IAD+K,II),K=1,NR)
                     IREC=IREC+1
                  ENDIF
               ENDDO
C
               CALL MY_ETIME(DSTIME(17))
C
               IF (IOPT.EQ.2) THEN
                  DEALLOCATE(VD, VS, FREQ, BL)
                  GOTO 100
               ENDIF
C Condensation des forces portant sur les ddls internes
               IF (NDDL_I2.GT.0.AND.NSCM.GT.0) 
     .            CALL DGEMM(
     .           'N', 'N',                     NSCM,   NDDL_F, NDDL_I2,
     .           UN,  BL,                      NSCM,   VS,     NDDL_I2,
     .           UN,  GRAPHE(I)%DGRAPH(ISD)%B, NSBMAX)
               DEALLOCATE(VS, BL)
C
 100           CONTINUE
               CALL MY_ETIME(DSTIME(19))
               TEXTR(1)  = DSTIME(3)  - DSTIME(1)
               TEXTR(2)  = DSTIME(4)  - DSTIME(2)
               TDYN(1)   = DSTIME(5)  - DSTIME(3)
               TDYN(2)   = DSTIME(6)  - DSTIME(4)
               TSTAT(1)  = DSTIME(7)  - DSTIME(5)
               TSTAT(2)  = DSTIME(8)  - DSTIME(6)
               TKCOND(1) = DSTIME(9)  - DSTIME(7)
               TKCOND(2) = DSTIME(10) - DSTIME(8)
               TKWRI(1)  = DSTIME(11) - DSTIME(9)
               TKWRI(2)  = DSTIME(12) - DSTIME(10)
               TMCOND(1) = DSTIME(13) - DSTIME(11)
               TMCOND(2) = DSTIME(14) - DSTIME(12)
               TMWRI(1)  = DSTIME(15) - DSTIME(13)
               TMWRI(2)  = DSTIME(16) - DSTIME(14)
               TMDWRI(1) = DSTIME(17) - DSTIME(15)
               TMDWRI(2) = DSTIME(18) - DSTIME(16)
               TFCOND(1) = DSTIME(19) - DSTIME(17)
               TFCOND(2) = DSTIME(20) - DSTIME(18)
               TTOT(1)   = DSTIME(19) - DSTIME(1)
               TTOT(2)   = DSTIME(20) - DSTIME(2)
               TTOTD(1)  = TTOT(1)
               TTOTD(2)  = TTOT(2)
               IF (TTOTD(1).EQ.ZERO) TTOTD(1)=EP30
               IF (TTOTD(2).EQ.ZERO) TTOTD(2)=EP30
               IF (IPRIM.GT.1) WRITE(IOUT,1000) NDDL_F, NDDL_I2,
     .                               NDDL_F, NVD,
     .                               TTOT(1),
     .                               NINT(TEXTR(1) /TTOTD(1)*100),
     .                               NINT(TDYN(1)  /TTOTD(1)*100),
     .                               NINT(TSTAT(1) /TTOTD(1)*100),
     .                               NINT(TKCOND(1)/TTOTD(1)*100),
     .                               NINT(TKWRI(1) /TTOTD(1)*100),
     .                               NINT(TMCOND(1)/TTOTD(1)*100),
     .                               NINT(TMWRI(1) /TTOTD(1)*100),
     .                               NINT(TMDWRI(1)/TTOTD(1)*100),
     .                               NINT(TFCOND(1)/TTOTD(1)*100)
C
            ENDDO
         ENDDO
      ENDDO
C
      RETURN
1000  FORMAT(' CONDENSATION SUMMARY'
     .       /' --------------------'
     .       /' NB BOUNDARY DOFS ',I6,' NB INTERNAL DOFS ',I6
     .       /' NB STATIC MODES  ',I6,' NB DYNAMIC MODES ',I6
     .       //' TIMING INFORMATION'
     .       /' ------------------'
     .       /' TOTAL TIME ',1PG11.4,' sec (cpu)'
     .       /' ---'
     .       /' EXTRACTION             ',I3,'%'
     .       /' DYNAMIC SOLUTION       ',I3,'%'
     .       /' STATIC SOLUTION        ',I3,'%'
     .       /' STIFFNESS CONDENSATION ',I3,'%'
     .       /' STIFFNESS WRITING      ',I3,'%'
     .       /' MASS CONDENSATION      ',I3,'%'
     .       /' MASS WRITING           ',I3,'%'
     .       /' MODES WRITING          ',I3,'%'
     .       /' FORCES CONDENSATION    ',I3,'%'/)
      END
Chd|====================================================================
Chd|  DSKCOND                       source/implicit/dsolve/dscond.F
Chd|-- called by -----------
Chd|        DSCOND                        source/implicit/dsolve/dscond.F
Chd|-- calls ---------------
Chd|        MY_ETIME                      source/system/machine.F       
Chd|====================================================================
      SUBROUTINE DSKCOND(KLB , KLC   , NDI , NDF, VS , 
     .                   VD  , IFILE , IREC, NVD, LEN,
     .                   FREQ, DSTIME)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDI, NDF, IFILE, IREC, NVD, LEN
      my_real
     .        KLB(NDF,*), KLC(NDF,*), VS(NDI,*), VD(NDI,*), FREQ(*)
      REAL    DSTIME(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, IAD, NR, II
      my_real
*     .        KPS(NDF,NDF), KPI(NVD), KP(LEN)
     .        KPI(NVD)
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE :: KPS
      my_real
     .       , DIMENSION(:), ALLOCATABLE :: KP
C
      ALLOCATE(KPS(NDF,NDF), KP(LEN))
C
      DO I=1,NDF
         DO J=1,NDF
            KPS(I,J)=KLB(I,J)
         ENDDO
      ENDDO
C
      IF (NDI.GT.0.AND.NDF.GT.0)
     .   CALL DGEMM('N', 'N', NDF, NDF, NDI,
     .              UN,  KLC, NDF, VS,  NDI,
     .              UN,  KPS, NDF) 
C
      DO I=1,NVD
         KPI(I)=(DEUX*PI*FREQ(I))**2
      ENDDO
C
C Ecriture sur fichier acces direct
      CALL MY_ETIME(DSTIME(9))
C
      DO I=1,LEN
         KP(I)=ZERO
      ENDDO
      II=0
      DO I=1,NDF
         DO J=1,I
            II=II+1
            KP(II)=KPS(J,I)
         ENDDO
      ENDDO
      DO I=1,NVD
         II=II+NDF+I
         KP(II)=KPI(I)
      ENDDO
C
      IAD=0
      DO I=1,LEN/10
         WRITE(IFILE,REC=IREC) (KP(IAD+J),J=1,10)
         IAD=IAD+10
         IREC=IREC+1
      ENDDO
      NR=LEN-(LEN/10)*10
      IF (NR.GT.0) THEN
         WRITE(IFILE,REC=IREC) (KP(IAD+J),J=1,NR)
         IREC=IREC+1
      ENDIF
C
      DEALLOCATE(KPS, KP)
C
      RETURN
      END
Chd|====================================================================
Chd|  DSMCOND                       source/implicit/dsolve/dscond.F
Chd|-- called by -----------
Chd|        DSCOND                        source/implicit/dsolve/dscond.F
Chd|-- calls ---------------
Chd|        MY_ETIME                      source/system/machine.F       
Chd|====================================================================
      SUBROUTINE DSMCOND(MLB, MLC   , MLI   , NDI , NDF , 
     .                   VS , VD    , IFILE , IREC, NVD , 
     .                   LEN, IMDIAG, DSTIME, LDMF, LDMI)           
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
* debug
#include      "units_c.inc"
*
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDI, NDF, IFILE, IREC, NVD, LEN, IMDIAG, LDMF, LDMI
      my_real
     .        MLB(LDMF,*), MLC(NDF,*), MLI(LDMI,*), VS(NDI,*), VD(NDI,*)
      REAL    DSTIME(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, I, J, NR, IAD, II, NRT, NLT, IOP
      my_real
*     .        MPS(NDF,NDF), MPC(NDF,NVD),
*     .        MPII(NVD), MP(LEN)
     .        MPII(NVD)
      my_real,
     .         DIMENSION(:,:), ALLOCATABLE :: MPS, MPC, MTMP
      my_real
     .       , DIMENSION(:), ALLOCATABLE :: MP
* debug
*      REAL  TMCOND(20)
C
      NRT=MAX(NDF,NDI)
      NLT=MAX(NDF,NVD)
      ALLOCATE(MPS(NDF,NDF), MPC(NDF,NVD), MTMP(NRT,NLT), MP(LEN))
C
      IF (IMDIAG.EQ.1) THEN
         DO I=1,NDF
            DO J=1,NDF
               MPS(I,J)=ZERO
            ENDDO
            MPS(I,I)=MLB(I,1)
         ENDDO
         DO I=1,NDI
            DO J=1,NDF
               MTMP(I,J)=VS(I,J)*MLI(I,1)
            ENDDO
         ENDDO
         IF (NDI.GT.0.AND.NDF.GT.0) 
     .      CALL DGEMM('T', 'N', NDF, NDF,  NDI,
     .                 UN,  VS,  NDI, MTMP, NRT,
     .                 UN,  MPS, NDF)
C
         DO I=1,NDI
            DO J=1,NVD
               MTMP(I,J)=VD(I,J)*MLI(I,1)
            ENDDO
         ENDDO
         IF (NDI.GT.0.AND.NDF.GT.0) 
     .      CALL DGEMM('T',  'N', NDF, NVD,  NDI,
     .                 UN,   VS,  NDI, MTMP, NRT,
     .                 ZERO, MPC, NDF)
C
         DO I=1,NVD
            MPII(I)=UN
         ENDDO
C
      ELSEIF (IMDIAG.EQ.0) THEN
         DO I=1,NDF
            DO J=1,NDF
               MPS(I,J)=MLB(I,J)
            ENDDO
         ENDDO
C
         IF (NDI.GT.0.AND.NDF.GT.0) THEN
*            CALL MY_ETIME(TMCOND(1))
            CALL DGEMM('N',  'N',  NDI, NDF, NDI,
     .                 UN,   MLI,  NDI, VS,  NDI,
     .                 ZERO, MTMP, NRT)
*            CALL MY_ETIME(TMCOND(3))
            CALL DGEMM('T', 'N', NDF, NDF,  NDI,
     .                 UN,  VS,  NDI, MTMP, NRT,
     .                 UN,  MPS, NDF)
*            CALL MY_ETIME(TMCOND(5))
            CALL DGEMM('N',  'N',  NDF, NDF, NDI,
     .                 UN,   MLC,  NDF, VS,  NDI,
     .                 ZERO, MTMP, NRT)
            DO I=1,NDF
               DO J=1,NDF
                  MPS(I,J)=MPS(I,J)+MTMP(I,J)+MTMP(J,I)
               ENDDO
            ENDDO
*            CALL MY_ETIME(TMCOND(7))
C
*            CALL MY_ETIME(TMCOND(9))
            CALL DGEMM('N',  'N',  NDI, NVD, NDI,
     .                 UN,   MLI,  NDI, VD,  NDI,
     .                 ZERO, MTMP, NRT)
*            CALL MY_ETIME(TMCOND(11))
            CALL DGEMM('T',   'N', NDF, NVD,  NDI,
     .                 UN,    VS,  NDI, MTMP, NRT,
     .                 ZERO,  MPC, NDF)
*            CALL MY_ETIME(TMCOND(13))
            CALL DGEMM('N', 'N', NDF, NVD, NDI,
     .                 UN,  MLC, NDF, VD,  NDI,
     .                 UN,  MPC, NDF)
*            CALL MY_ETIME(TMCOND(15))
         ENDIF
*
*         WRITE(IOUT,'(A)') '****** '
*         WRITE(IOUT,'(A)') 'TIMING DSMCOND '
*         WRITE(IOUT,'(A)') 'MATRIX DIMENSIONS '
*         WRITE(IOUT,'(A9,3I6)') 'OPER 1 : ',NDI,NDI,NDF
*         WRITE(IOUT,'(A9,3I6)') 'OPER 2 : ',NDF,NDI,NDF
*         WRITE(IOUT,'(A9,3I6)') 'OPER 3 : ',NDF,NDI,NDF
*         WRITE(IOUT,'(A9,3I6)') 'OPER 4 : ',NDF,NDI,NDF
*         WRITE(IOUT,'(A9,3I6)') 'OPER 5 : ',NDI,NDI,NVD
*         WRITE(IOUT,'(A9,3I6)') 'OPER 6 : ',NDF,NDI,NVD
*         WRITE(IOUT,'(A9,3I6)') 'OPER 7 : ',NDF,NDI,NVD
*         WRITE(IOUT,'(A)') 'TIMES '
*         WRITE(IOUT,'(A9,1PE11.4)') 'OPER 1 : ',TMCOND(3)-TMCOND(1)
*         WRITE(IOUT,'(A9,1PE11.4)') 'OPER 2 : ',TMCOND(5)-TMCOND(3)
*         WRITE(IOUT,'(A9,1PE11.4)') 'OPER 3 : ',TMCOND(7)-TMCOND(5)
*         WRITE(IOUT,'(A9,1PE11.4)') 'OPER 4 : ',TMCOND(9)-TMCOND(7)
*         WRITE(IOUT,'(A9,1PE11.4)') 'OPER 5 : ',TMCOND(11)-TMCOND(9)
*         WRITE(IOUT,'(A9,1PE11.4)') 'OPER 6 : ',TMCOND(13)-TMCOND(11)
*         WRITE(IOUT,'(A9,1PE11.4)') 'OPER 7 : ',TMCOND(15)-TMCOND(13)
*         WRITE(IOUT,'(A)') '****** '
C
         DO I=1,NVD
            MPII(I)=UN
         ENDDO
C
      ENDIF
C
C Ecriture sur fichier acces direct
      CALL MY_ETIME(DSTIME(13))
C
      DO I=1,LEN
         MP(I)=ZERO
      ENDDO
      II=0
      DO I=1,NDF
         DO J=1,I
            II=II+1
            MP(II)=MPS(J,I)
         ENDDO
      ENDDO
      DO I=1,NVD
         DO J=1,NDF
            II=II+1
            MP(II)=MPC(J,I)
         ENDDO
         II=II+I
         MP(II)=MPII(I)
      ENDDO
C
      IAD=0
      DO I=1,LEN/10
         WRITE(IFILE,REC=IREC) (MP(IAD+J),J=1,10)
         IAD=IAD+10
         IREC=IREC+1
      ENDDO
      NR=LEN-(LEN/10)*10
      IF (NR.GT.0) THEN
         WRITE(IFILE,REC=IREC) (MP(IAD+J),J=1,NR)
         IREC=IREC+1
      ENDIF
C
      DEALLOCATE(MPS, MPC, MTMP, MP)
C
      RETURN
      END
Chd|====================================================================
Chd|  DSWRKLI                       source/implicit/dsolve/dscond.F
Chd|-- called by -----------
Chd|        DSCOND                        source/implicit/dsolve/dscond.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DSWRKLI(KLI, IPIV, NDI, IFILE, IREC)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPIV(*), NDI, IFILE, IREC
      my_real
     .        KLI(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LEN, IAD, I, J, NR
      my_real
     .        RPIV(NDI)
C
      LEN=NDI*NDI
      IAD=0
      DO I=1,LEN/10
         WRITE(IFILE,REC=IREC) (KLI(IAD+J),J=1,10)
         IAD=IAD+10
         IREC=IREC+1
      ENDDO
      NR=LEN-(LEN/10)*10
      IF (NR.GT.0) THEN
         WRITE(IFILE,REC=IREC) (KLI(IAD+J),J=1,NR)
         IREC=IREC+1
      ENDIF
C
      DO I=1,NDI
         RPIV(I)=IPIV(I)
      ENDDO
      LEN=NDI
      IAD=0
      DO I=1,LEN/10
         WRITE(IFILE,REC=IREC) (RPIV(IAD+J),J=1,10)
         IAD=IAD+10
         IREC=IREC+1
      ENDDO
      NR=LEN-(LEN/10)*10
      IF (NR.GT.0) THEN
         WRITE(IFILE,REC=IREC) (RPIV(IAD+J),J=1,NR)
         IREC=IREC+1
      ENDIF
C
      RETURN
      END
            
      
