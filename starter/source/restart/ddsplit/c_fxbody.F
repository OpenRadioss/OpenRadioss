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
Chd|  C_FXBODY1                     source/restart/ddsplit/c_fxbody.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE C_FXBODY1(
     .   FXBIPM   , FXBNOD  , NODLOCAL, IPARG   , FXBELM,
     .   LENNOD_L , LENMOD_L, LENELM_L, LENSIG_L, PROC  ,
     .   LENGRVI_L, FXBGRVI )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "fxbcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER FXBIPM(NBIPM,*), FXBNOD(*), NODLOCAL(*), IPARG(NPARG,*),
     .        FXBELM(*), LENNOD_L, LENMOD_L, LENELM_L, LENSIG_L, PROC,
     .        LENGRVI_L, FXBGRVI(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NFX, NSN, NSNI, IFILE, ANOD, I, II, AELM, NELS, NELC,
     .        NELT, NELP, NELTG, NG, P, NSN_L, NSNI_L, NMOD, NME,
     .        NLG, AGRVI, IADG, IG, NNO, NNO_L
C
      LENNOD_L=0
      LENMOD_L=0
      LENELM_L=0
      LENSIG_L=0
      LENGRVI_L=0
      DO NFX=1,NFXBODY
         NSN=FXBIPM(3,NFX)
         NSNI=FXBIPM(18,NFX)
         NMOD=FXBIPM(4,NFX)
         NME=FXBIPM(17,NFX)
         IFILE=FXBIPM(29,NFX)
C Modes
         ANOD=FXBIPM(6,NFX)
         NSN_L=0
         NSNI_L=0
         DO I=1,NSNI
            II=FXBNOD(ANOD+I-1)
            IF (NODLOCAL(II)/=0) THEN
               NSN_L=NSN_L+1
               NSNI_L=NSNI_L+1
            ENDIF
         ENDDO
         DO I=NSNI+1,NSN
            II=FXBNOD(ANOD+I-1)
            IF (NODLOCAL(II)/=0) NSN_L=NSN_L+1
         ENDDO
         LENNOD_L=LENNOD_L+NSN_L
         IF (IFILE==0) THEN
            LENMOD_L=LENMOD_L+NSN_L*(NME+NMOD)
         ELSEIF (IFILE==1) THEN
            LENMOD_L=LENMOD_L+NSNI_L*(NME+NMOD)
         ENDIF
C Champs de contraintes
         AELM=FXBIPM(19,NFX)
         NELS=FXBIPM(21,NFX)
         NELC=FXBIPM(22,NFX)
         NELT=FXBIPM(34,NFX)
         NELP=FXBIPM(35,NFX)
         NELTG=FXBIPM(23,NFX)
         DO I=1,NELS
            NG=FXBELM(AELM)
            P=IPARG(32,NG)
            IF (P==PROC) THEN
               LENELM_L=LENELM_L+13
               IF (IFILE==0) LENSIG_L=LENSIG_L+7*NMOD
            ENDIF
            AELM=AELM+13
         ENDDO
         DO I=1,NELC
            NG=FXBELM(AELM)
            P=IPARG(32,NG)
            IF (P==PROC) THEN
               LENELM_L=LENELM_L+10
               IF (IFILE==0) LENSIG_L=LENSIG_L+10*NMOD
            ENDIF
            AELM=AELM+10
         ENDDO
         DO I=1,NELT
            NG=FXBELM(AELM)
            P=IPARG(32,NG)
            IF (P==PROC) THEN
               LENELM_L=LENELM_L+7
               IF (IFILE==0) LENSIG_L=LENSIG_L+2*NMOD
            ENDIF
            AELM=AELM+7
         ENDDO
         DO I=1,NELP
            NG=FXBELM(AELM)
            P=IPARG(32,NG)
            IF (P==PROC) THEN
               LENELM_L=LENELM_L+9
               IF (IFILE==0) LENSIG_L=LENSIG_L+8*NMOD
            ENDIF
            AELM=AELM+9
         ENDDO
         DO I=1,NELTG
            NG=FXBELM(AELM)
            P=IPARG(32,NG)
            IF (P==PROC) THEN
               LENELM_L=LENELM_L+9
               IF (IFILE==0) LENSIG_L=LENSIG_L+10*NMOD
            ENDIF
            AELM=AELM+9
         ENDDO
C Gravite
         NLG=FXBIPM(25,NFX)
         AGRVI=FXBIPM(26,NFX)
         IADG=0
         DO IG=1,NLG
            NNO=FXBGRVI(AGRVI+IADG+1)
            NNO_L=0
            DO I=1,NNO
               II=FXBGRVI(AGRVI+IADG+I+1)
               IF (NODLOCAL(II)/=0) NNO_L=NNO_L+1
            ENDDO
            IADG=IADG+2+NNO
            LENGRVI_L=LENGRVI_L+2+NNO_L
         ENDDO
      ENDDO      
C
      RETURN
      END
Chd|====================================================================
Chd|  C_FXBODY2                     source/restart/ddsplit/c_fxbody.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|====================================================================
      SUBROUTINE C_FXBODY2(
     .   FXBIPM   , FXBNOD  , NODLOCAL, IPARG   , FXBELM ,
     .   FXBNOD_L , FXBMOD_L, FXBELM_L, FXBSIG_L, PROC   ,
     .   FXBMOD   , FXBSIG  , FXBIPM_L, FXBGRVI,
     .   FXBGRVI_L, LENNOD_L,ITASK)
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
#include      "scr15_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "units_fxbody_c.inc"
#include      "fxbcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER FXBIPM(NBIPM,*), FXBNOD(*), NODLOCAL(*), IPARG(NPARG,*),
     .        FXBELM(*), FXBNOD_L(*), FXBELM_L(*), PROC, 
     .        FXBIPM_L(NBIPM,*), FXBGRVI(*),
     .        FXBGRVI_L(*), LENNOD_L
      my_real
     .        FXBMOD_L(*), FXBSIG_L(*), FXBMOD(*), FXBSIG(*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL       
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER RCLEN, LB_L, NG, P, IAD_L(NGROUP), LBUFELI, ANOD_L,
     .        AMOD_L, IRCM_L, AELM_L, ASIG_L, IRCS_L, NFX, I, NSN,
     .        NSNI, IFILE, NMOD, NME, IRCM, ANOD, AMOD, NSN_L, NSNI_L,
     .        II, ASIG, IRCS, NELS, NELC, NELT, NELP, NELTG, AELM,
     .        NELS_L, NELC_L, NELT_L, NELP_L, NELTG_L, J, K, ELM(13),
     .        LVSIG, IAD, JJ, ASIG2, ASIG_L2, LVSIG2, NMAX, PMAIN, PP,
     .        AGRVI_L, NLG, AGRVI, IADG, IADG_L, IG, NNO, NNO_L,
     .        NG_L(NGROUP), IADFXB(LENNOD_L),ITASK
      my_real
     .        FLREC6(6), VAR(6)
      my_real
     .       , DIMENSION(:), ALLOCATABLE :: VSIG, VSIG2
      CHARACTER(LEN=4) :: CIT
      CHARACTER(LEN=256) :: SCR_FILE_NAME,SCR_FILE_NAME2
C
      INQUIRE(IOLENGTH=RCLEN) FLREC6

c initially was set in starter0, but re-set here for multi threading compatibility
      IFXM_L = 27000
      IFXS_L = 28000 

      WRITE(CIT,'(I4.4)')ITASK
      SCR_FILE_NAME ='SCR_FXM_'//ROOTNAM(1:ROOTLEN)//'_'//CIT(1:4)//'.scr'
      SCR_FILE_NAME2='SCR_FXS_'//ROOTNAM(1:ROOTLEN)//'_'//CIT(1:4)//'.scr'

      OPEN(UNIT=IFXM_L+ITASK,FILE=TRIM(SCR_FILE_NAME),ACCESS='DIRECT',RECL=RCLEN)
      OPEN(UNIT=IFXS_L+ITASK,FILE=TRIM(SCR_FILE_NAME2),ACCESS='DIRECT',RECL=RCLEN)
C
      LB_L=1
      II=0
      DO NG=1,NGROUP
         P=IPARG(32,NG)
         IF (P==PROC) THEN
            II=II+1
            NG_L(NG)=II
            IAD_L(NG)=LB_L
            IF (NG<NGROUP) THEN
               LBUFELI=IPARG(4,NG+1)-IPARG(4,NG)
            ELSE
               LBUFELI=LBUFEL+1-IPARG(4,NG)
            ENDIF
            LB_L=LB_L+LBUFELI
         ENDIF
      ENDDO
C
      ANOD_L=0
      AMOD_L=0
      IRCM_L=0
      AELM_L=0
      ASIG_L=0
      IRCS_L=0
      AGRVI_L=1
      DO NFX=1,NFXBODY
         DO I=1,NBIPM
            FXBIPM_L(I,NFX)=FXBIPM(I,NFX)
         ENDDO
C Noeud Main
         FXBIPM_L(2,NFX)=NODLOCAL(FXBIPM(2,NFX))
C
         NSN=FXBIPM(3,NFX)
         NSNI=FXBIPM(18,NFX)
         IFILE=FXBIPM(29,NFX)
         NMOD=FXBIPM(4,NFX)
         NME=FXBIPM(17,NFX)
         IRCM=FXBIPM(30,NFX)
C Modes
         ANOD=FXBIPM(6,NFX)
         AMOD=FXBIPM(7,NFX)
         FXBIPM_L(6,NFX)=ANOD_L+1
         FXBIPM_L(7,NFX)=AMOD_L+1
         NSN_L=0
         NSNI_L=0
         DO I=1,NSNI
            II=FXBNOD(ANOD+I-1)
            IF (NODLOCAL(II)/=0) THEN
               NSN_L=NSN_L+1
               NSNI_L=NSNI_L+1
               ANOD_L=ANOD_L+1
               FXBNOD_L(ANOD_L)=NODLOCAL(II)
               IADFXB(NSN_L)=I
            ENDIF
         ENDDO
         DO I=NSNI+1,NSN
            II=FXBNOD(ANOD+I-1)
            IF (NODLOCAL(II)/=0) THEN
               NSN_L=NSN_L+1
               ANOD_L=ANOD_L+1
               FXBNOD_L(ANOD_L)=NODLOCAL(II)
               IADFXB(NSN_L)=I
            ENDIF
         ENDDO
         FXBIPM_L(3,NFX)=NSN_L
         FXBIPM_L(18,NFX)=NSNI_L
         DO I=1,NSN_L
            ANOD_L=ANOD_L+1
            FXBNOD_L(ANOD_L)=IADFXB(I)
         ENDDO
         FXBIPM_L(30,NFX)=IRCM_L
         IF (IFILE==0) THEN
            DO I=1,NME+NMOD
               DO J=1,NSN
                  JJ=FXBNOD(ANOD+J-1)
                  IF (NODLOCAL(JJ)/=0) THEN
                     DO K=1,6
                        FXBMOD_L(AMOD_L+K)=FXBMOD(AMOD-1+6*(J-1)+K)
                     ENDDO
                     AMOD_L=AMOD_L+6
                  ENDIF
               ENDDO
               AMOD=AMOD+6*NSN
            ENDDO
         ELSEIF (IFILE==1) THEN
            DO I=1,NME+NMOD
               DO J=1,NSNI
                  JJ=FXBNOD(ANOD+J-1)
                  IF (NODLOCAL(JJ)/=0) THEN
                     DO K=1,6
                        FXBMOD_L(AMOD_L+K)=FXBMOD(AMOD-1+6*(J-1)+K)
                     ENDDO
                     AMOD_L=AMOD_L+6
                  ENDIF
               ENDDO
               AMOD=AMOD+6*NSNI
               DO J=NSNI+1,NSN
                  IRCM=IRCM+1
                  JJ=FXBNOD(ANOD+J-1)
                  IF (NODLOCAL(JJ)/=0) THEN
!$OMP CRITICAL(read_IXFM_critic)
                     READ(IFXM,REC=IRCM) (VAR(K),K=1,6)
!$OMP END CRITICAL(read_IXFM_critic)
                     IRCM_L=IRCM_L+1
                     WRITE(IFXM_L+ITASK,REC=IRCM_L) (VAR(K),K=1,6)
                  ENDIF
               ENDDO
            ENDDO
         ENDIF
         FXBIPM_L(32,NFX)=IRCM_L
C
Champs de contrainte modaux
         ASIG=FXBIPM(20,NFX)
         IRCS=FXBIPM(31,NFX)
         NELS=FXBIPM(21,NFX)
         NELC=FXBIPM(22,NFX)
         NELT=FXBIPM(34,NFX)
         NELP=FXBIPM(35,NFX)
         NELTG=FXBIPM(23,NFX)
C
         FXBIPM_L(19,NFX)=AELM_L+1
         FXBIPM_L(20,NFX)=ASIG_L+1
C
         DO I=1,NMOD
            AELM=FXBIPM(19,NFX)
            NELS_L=0
            NELC_L=0
            NELT_L=0
            NELP_L=0
            NELTG_L=0
            IF (IFILE==0) THEN
               DO J=1,NELS
                  NG=FXBELM(AELM)
                  P=IPARG(32,NG)
                  IAD=IPARG(4,NG)
                  IF (P==PROC) THEN
                     NELS_L=NELS_L+1
                     IF (I==1) THEN
                        DO K=1,13
                           ELM(K)=FXBELM(AELM+K-1)
                        ENDDO
                        ELM(1)=NG_L(NG)
                        ELM(11)=ELM(11)-IAD+IAD_L(NG)
                        ELM(12)=ELM(12)-IAD+IAD_L(NG)
                        DO K=1,13
                           FXBELM_L(AELM_L+K)=ELM(K)
                        ENDDO
                        AELM_L=AELM_L+13
                     ENDIF
                     DO K=1,7
                        FXBSIG_L(ASIG_L+K)=FXBSIG(ASIG+K-1)
                     ENDDO
                     ASIG_L=ASIG_L+7
                  ENDIF
                  AELM=AELM+13
                  ASIG=ASIG+7
               ENDDO
               DO J=1,NELC
                  NG=FXBELM(AELM)
                  P=IPARG(32,NG)
                  IAD=IPARG(4,NG)
                  IF (P==PROC) THEN
                     NELC_L=NELC_L+1
                     IF (I==1) THEN
                        DO K=1,10
                           ELM(K)=FXBELM(AELM+K-1)
                        ENDDO
                        ELM(1)=NG_L(NG)
                        ELM(7)=ELM(7)-IAD+IAD_L(NG)
                        ELM(8)=ELM(8)-IAD+IAD_L(NG)
                        ELM(9)=ELM(9)-IAD+IAD_L(NG)
                        DO K=1,10
                           FXBELM_L(AELM_L+K)=ELM(K)
                        ENDDO
                        AELM_L=AELM_L+10
                     ENDIF
                     DO K=1,10
                        FXBSIG_L(ASIG_L+K)=FXBSIG(ASIG+K-1)
                     ENDDO
                     ASIG_L=ASIG_L+10
                  ENDIF
                  AELM=AELM+10
                  ASIG=ASIG+10
               ENDDO
               DO J=1,NELT
                  NG=FXBELM(AELM)
                  P=IPARG(32,NG)
                  IAD=IPARG(4,NG)
                  IF (P==PROC) THEN
                     NELT_L=NELT_L+1
                     IF (I==1) THEN
                        DO K=1,7
                           ELM(K)=FXBELM(AELM+K-1)
                        ENDDO
                        ELM(1)=NG_L(NG)
                        ELM(5)=ELM(5)-IAD+IAD_L(NG)
                        ELM(6)=ELM(6)-IAD+IAD_L(NG)
                        DO K=1,7
                           FXBELM_L(AELM_L+K)=ELM(K)
                        ENDDO
                        AELM_L=AELM_L+7
                     ENDIF
                     DO K=1,2
                        FXBSIG_L(ASIG_L+K)=FXBSIG(ASIG+K-1)
                     ENDDO
                     ASIG_L=ASIG_L+2
                  ENDIF
                  AELM=AELM+7
                  ASIG=ASIG+2
               ENDDO
               DO J=1,NELP
                  NG=FXBELM(AELM)
                  P=IPARG(32,NG)
                  IAD=IPARG(4,NG)
                  IF (P==PROC) THEN
                     NELP_L=NELP_L+1
                     IF (I==1) THEN
                        DO K=1,9
                           ELM(K)=FXBELM(AELM+K-1)
                        ENDDO
                        ELM(1)=NG_L(NG)
                        ELM(6)=ELM(6)-IAD+IAD_L(NG)
                        ELM(7)=ELM(7)-IAD+IAD_L(NG)
                        ELM(8)=ELM(8)-IAD+IAD_L(NG)
                        DO K=1,9
                           FXBELM_L(AELM_L+K)=ELM(K)
                        ENDDO
                        AELM_L=AELM_L+9
                     ENDIF
                     DO K=1,8
                        FXBSIG_L(ASIG_L+K)=FXBSIG(ASIG+K-1)
                     ENDDO
                     ASIG_L=ASIG_L+8
                  ENDIF
                  AELM=AELM+9
                  ASIG=ASIG+8
               ENDDO
               DO J=1,NELTG
                  NG=FXBELM(AELM)
                  P=IPARG(32,NG)
                  IAD=IPARG(4,NG)
                  IF (P==PROC) THEN
                     NELTG_L=NELTG_L+1
                     IF (I==1) THEN
                        DO K=1,9
                           ELM(K)=FXBELM(AELM+K-1)
                        ENDDO
                        ELM(1)=NG_L(NG)
                        ELM(6)=ELM(6)-IAD+IAD_L(NG)
                        ELM(7)=ELM(7)-IAD+IAD_L(NG)
                        ELM(8)=ELM(8)-IAD+IAD_L(NG)
                        DO K=1,9
                           FXBELM_L(AELM_L+K)=ELM(K)
                        ENDDO
                        AELM_L=AELM_L+9
                     ENDIF
                     DO K=1,10
                        FXBSIG_L(ASIG_L+K)=FXBSIG(ASIG+K-1)
                     ENDDO
                     ASIG_L=ASIG_L+10
                  ENDIF
                  AELM=AELM+9
                  ASIG=ASIG+10
               ENDDO
            ELSEIF (IFILE==1) THEN
               LVSIG=NELS*7+NELC*10+NELT*2+NELP*8+NELTG*10
               ALLOCATE(VSIG(LVSIG), VSIG2(LVSIG))
               IAD=0
               DO J=1,LVSIG/6
                  IRCS=IRCS+1
!$OMP CRITICAL(read_IXFS_critic)
                  READ(IFXS,REC=IRCS) (VSIG(IAD+K),K=1,6)
!$OMP END CRITICAL(read_IXFS_critic)
                  IAD=IAD+6
               ENDDO
               JJ=LVSIG-(LVSIG/6)*6
               IF (JJ>0) THEN
                  IRCS=IRCS+1
!$OMP CRITICAL(read_IXFS_critic)
                  READ(IFXS,REC=IRCS) (VSIG(IAD+K),K=1,JJ)
!$OMP END CRITICAL(read_IXFS_critic)
               ENDIF
C
               ASIG2=0
               ASIG_L2=0
               DO J=1,NELS
                  NG=FXBELM(AELM)
                  P=IPARG(32,NG)
                  IAD=IPARG(4,NG)
                  IF (P==PROC) THEN
                     NELS_L=NELS_L+1
                     IF (I==1) THEN
                        DO K=1,13
                           ELM(K)=FXBELM(AELM+K-1)
                        ENDDO
                        ELM(1)=NG_L(NG)
                        ELM(11)=ELM(11)-IAD+IAD_L(NG)
                        ELM(12)=ELM(12)-IAD+IAD_L(NG)
                        DO K=1,13
                           FXBELM_L(AELM_L+K)=ELM(K)
                        ENDDO
                        AELM_L=AELM_L+13
                     ENDIF
                     DO K=1,7
                        VSIG2(ASIG_L2+K)=VSIG(ASIG2+K)
                     ENDDO
                     ASIG_L2=ASIG_L2+7
                  ENDIF
                  AELM=AELM+13
                  ASIG2=ASIG2+7
               ENDDO
               DO J=1,NELC
                  NG=FXBELM(AELM)
                  P=IPARG(32,NG)
                  IAD=IPARG(4,NG)
                  IF (P==PROC) THEN
                     NELC_L=NELC_L+1
                     IF (I==1) THEN
                        DO K=1,10
                           ELM(K)=FXBELM(AELM+K-1)
                        ENDDO
                        ELM(1)=NG_L(NG)
                        ELM(7)=ELM(7)-IAD+IAD_L(NG)
                        ELM(8)=ELM(8)-IAD+IAD_L(NG)
                        ELM(9)=ELM(9)-IAD+IAD_L(NG)
                        DO K=1,10
                           FXBELM_L(AELM_L+K)=ELM(K)
                        ENDDO
                        AELM_L=AELM_L+10
                     ENDIF
                     DO K=1,10
                        VSIG2(ASIG_L2+K)=VSIG(ASIG2+K)
                     ENDDO
                     ASIG_L2=ASIG_L2+10
                  ENDIF
                  AELM=AELM+10
                  ASIG2=ASIG2+10
               ENDDO
               DO J=1,NELT
                  NG=FXBELM(AELM)
                  P=IPARG(32,NG)
                  IAD=IPARG(4,NG)
                  IF (P==PROC) THEN
                     NELT_L=NELT_L+1
                     IF (I==1) THEN
                        DO K=1,7
                           ELM(K)=FXBELM(AELM+K-1)
                        ENDDO
                        ELM(1)=NG_L(NG)
                        ELM(5)=ELM(5)-IAD+IAD_L(NG)
                        ELM(6)=ELM(6)-IAD+IAD_L(NG)
                        DO K=1,7
                           FXBELM_L(AELM_L+K)=ELM(K)
                        ENDDO
                        AELM_L=AELM_L+7
                     ENDIF
                     DO K=1,2
                        VSIG2(ASIG_L2+K)=VSIG(ASIG2+K)
                     ENDDO
                     ASIG_L2=ASIG_L2+2
                  ENDIF
                  AELM=AELM+7
                  ASIG2=ASIG2+2
               ENDDO
               DO J=1,NELP
                  NG=FXBELM(AELM)
                  P=IPARG(32,NG)
                  IAD=IPARG(4,NG)
                  IF (P==PROC) THEN
                     NELP_L=NELP_L+1
                     IF (I==1) THEN
                        DO K=1,9
                           ELM(K)=FXBELM(AELM+K-1)
                        ENDDO
                        ELM(1)=NG_L(NG)
                        ELM(6)=ELM(6)-IAD+IAD_L(NG)
                        ELM(7)=ELM(7)-IAD+IAD_L(NG)
                        ELM(8)=ELM(8)-IAD+IAD_L(NG)
                        DO K=1,9
                           FXBELM_L(AELM_L+K)=ELM(K)
                        ENDDO
                        AELM_L=AELM_L+9
                     ENDIF
                     DO K=1,8
                        VSIG2(ASIG_L2+K)=VSIG(ASIG2+K)
                     ENDDO
                     ASIG_L2=ASIG_L2+8
                  ENDIF
                  AELM=AELM+9
                  ASIG2=ASIG2+8
               ENDDO
               DO J=1,NELTG
                  NG=FXBELM(AELM)
                  P=IPARG(32,NG)
                  IAD=IPARG(4,NG)
                  IF (P==PROC) THEN
                     NELTG_L=NELTG_L+1
                     IF (I==1) THEN
                        DO K=1,9                     
                           ELM(K)=FXBELM(AELM+K-1)   
                        ENDDO                        
                        ELM(1)=NG_L(NG)
                        ELM(6)=ELM(6)-IAD+IAD_L(NG)  
                        ELM(7)=ELM(7)-IAD+IAD_L(NG)  
                        ELM(8)=ELM(8)-IAD+IAD_L(NG)  
                        DO K=1,9                     
                           FXBELM_L(AELM_L+K)=ELM(K) 
                        ENDDO                        
                        AELM_L=AELM_L+9
                     ENDIF            
                     DO K=1,10
                        VSIG2(ASIG_L2+K)=VSIG(ASIG2+K)
                     ENDDO
                     ASIG_L2=ASIG_L2+10
                  ENDIF
                  AELM=AELM+9
                  ASIG2=ASIG2+10
               ENDDO
C
               LVSIG2=NELS_L*7+NELC_L*10+NELT_L*2+NELP_L*8+NELTG_L*10
               IAD=0
               DO J=1,LVSIG2/6
                  IRCS_L=IRCS_L+1
                  WRITE(IFXS_L+ITASK,REC=IRCS_L) (VSIG2(IAD+K),K=1,6)
                  IAD=IAD+6
               ENDDO
               JJ=LVSIG2-(LVSIG2/6)*6
               IF (JJ/=0) THEN
                  IRCS_L=IRCS_L+1
                  WRITE(IFXS_L+ITASK,REC=IRCS_L) (VSIG2(IAD+K),K=1,JJ)
               ENDIF
               DEALLOCATE(VSIG, VSIG2)
            ENDIF
         ENDDO
         FXBIPM_L(21,NFX)=NELS_L
         FXBIPM_L(22,NFX)=NELC_L
         FXBIPM_L(34,NFX)=NELT_L
         FXBIPM_L(35,NFX)=NELP_L
         FXBIPM_L(23,NFX)=NELTG_L
         FXBIPM_L(33,NFX)=IRCS_L
C Gravite
         NLG=FXBIPM(25,NFX)
         AGRVI=FXBIPM(26,NFX)
         FXBIPM_L(26,NFX)=AGRVI_L
         IADG=0
         IADG_L=0
         DO IG=1,NLG
            FXBGRVI_L(AGRVI_L+IADG_L)=FXBGRVI(AGRVI+IADG)
            NNO=FXBGRVI(AGRVI+IADG+1)
            NNO_L=0
            DO I=1,NNO
               II=FXBGRVI(AGRVI+IADG+I+1)
               IF (NODLOCAL(II)/=0) THEN
                  NNO_L=NNO_L+1
                  FXBGRVI_L(AGRVI_L+IADG_L+NNO_L+1)=NODLOCAL(II)
               ENDIF
            ENDDO
            IADG=IADG+2+NNO
            FXBGRVI_L(AGRVI_L+IADG_L+1)=NNO_L
            IADG_L=IADG_L+2+NNO_L
         ENDDO
         AGRVI_L=AGRVI_L+IADG_L
C Identification du PMAIN
         NMAX=0
         PMAIN=1
         DO P=1,NSPMD
            NSN_L=0
            DO I=1,NSN
               II=FXBNOD(ANOD+I-1)
               IF(NLOCAL(II,P)==1)THEN
                  DO PP = 1, P-1
                     IF(NLOCAL(II,PP)==1) THEN
                       GOTO 100
                     ENDIF
                  ENDDO
                  NSN_L=NSN_L+1
 100              CONTINUE
               ENDIF
            ENDDO
            IF(NSN_L>NMAX)THEN
               PMAIN=P
               NMAX=NSN_L
            ENDIF
         ENDDO
         FXBIPM_L(39,NFX)=PMAIN-1
         FXBIPM_L(40,NFX)=FXBIPM(18,NFX)
      ENDDO
      LENNOD_L=LENNOD_L*2
C
      RETURN
      END
Chd|====================================================================
Chd|  C_FXBODY3                     source/restart/ddsplit/c_fxbody.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE C_FXBODY3(
     .     LENGLM_L, LENCP_L  , LENLM_L , LENFLS_L, LENDLS_L,
     .     LENMCD_L, LENGRVR_L, FXBIPM_L, PROC    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "fxbcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LENGLM_L, LENCP_L, LENLM_L, LENFLS_L, LENDLS_L,
     .        LENMCD_L, LENGRVR_L, FXBIPM_L(NBIPM,*),
     .        PROC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NTR, NFX, NMOD, NMST, NME, NLG, PMAIN
C
      LENGLM_L=0
      LENCP_L=0
      LENLM_L=0
      LENFLS_L=0
      LENDLS_L=0
      LENMCD_L=0
      LENGRVR_L=0
C
      NTR=9
      DO NFX=1,NFXBODY
         NMOD=FXBIPM_L(4,NFX)
         NMST=FXBIPM_L(5,NFX)
         NME=FXBIPM_L(17,NFX)
         NLG=FXBIPM_L(25,NFX)
         PMAIN=FXBIPM_L(39,NFX)
C
         FXBIPM_L(8 ,NFX)=LENGLM_L+1
         FXBIPM_L(9 ,NFX)=LENCP_L+1
         FXBIPM_L(10,NFX)=LENLM_L+1
         FXBIPM_L(11,NFX)=LENFLS_L+1
         FXBIPM_L(12,NFX)=LENDLS_L+1
         FXBIPM_L(15,NFX)=LENMCD_L+1
         FXBIPM_L(27,NFX)=LENGRVR_L+1
C
         IF (PMAIN==PROC) THEN
            LENGLM_L =LENGLM_L +NME*(NME+1)/2
            LENCP_L  =LENCP_L  +NTR*NMOD*NME
            LENLM_L  =LENLM_L  +NMOD
            LENFLS_L =LENFLS_L +NMST*(2*NMOD-NMST+1)/2
            LENDLS_L =LENDLS_L +NMOD-NMST
            LENMCD_L =LENMCD_L +NME*NME
            LENGRVR_L=LENGRVR_L+NME*NLG+NMOD*9*NLG
         ENDIF
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  C_FXBODY4                     source/restart/ddsplit/c_fxbody.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE C_FXBODY4(
     .     FXBGLM   , FXBCPM  , FXBCPS  , FXBLM   , FXBFLS  ,
     .     FXBDLS   , FXBGRVR , FXBIPM  , PROC    , FXBGLM_L,
     .     FXBCPM_L , FXBCPS_L, FXBLM_L , FXBFLS_L, FXBDLS_L,
     .     FXBGRVR_L, FXBIPM_L)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "fxbcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER FXBIPM(NBIPM,*), FXBIPM_L(NBIPM,*), PROC
      my_real
     .        FXBGLM(*), FXBCPM(*), FXBCPS(*), FXBLM(*), FXBFLS(*),
     .        FXBDLS(*), FXBGRVR(*), FXBGLM_L(*), FXBCPM_L(*),
     .        FXBCPS_L(*), FXBLM_L(*), FXBFLS_L(*), FXBDLS_L(*),
     .        FXBGRVR_L(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NTR, NFX, NMOD, NMST, NME, NLG, PMAIN, AGLM, ACP, ALM,
     .        AFLS, ADLS, AGRVR, AGLM_L, ACP_L, ALM_L, AFLS_L, ADLS_L,
     .        AGRVR_L, I
C
      NTR=9
      DO NFX=1,NFXBODY
         NMOD=FXBIPM_L(4,NFX)
         NMST=FXBIPM_L(5,NFX)
         NME=FXBIPM_L(17,NFX)
         NLG=FXBIPM_L(25,NFX)
         PMAIN=FXBIPM_L(39,NFX)
C
         AGLM =FXBIPM(8,NFX)
         ACP  =FXBIPM(9,NFX)
         ALM  =FXBIPM(10,NFX)
         AFLS =FXBIPM(11,NFX)
         ADLS =FXBIPM(12,NFX)
         AGRVR=FXBIPM(27,NFX)
         AGLM_L =FXBIPM_L(8,NFX)
         ACP_L  =FXBIPM_L(9,NFX)
         ALM_L  =FXBIPM_L(10,NFX)
         AFLS_L =FXBIPM_L(11,NFX)
         ADLS_L =FXBIPM_L(12,NFX)
         AGRVR_L=FXBIPM_L(27,NFX)
C
         IF (PMAIN==PROC) THEN
            DO I=1,NME*(NME+1)/2
               FXBGLM_L(AGLM_L+I-1)=FXBGLM(AGLM+I-1)
            ENDDO
            DO I=1,NTR*NMOD*NME
               FXBCPM_L(ACP_L+I-1)=FXBCPM(ACP+I-1)
               FXBCPS_L(ACP_L+I-1)=FXBCPS(ACP+I-1)
            ENDDO
            DO I=1,NMOD
               FXBLM_L(ALM_L+I-1)=FXBLM(ALM+I-1)
            ENDDO
            DO I=1,NMST*(2*NMOD-NMST+1)/2
               FXBFLS_L(AFLS_L+I-1)=FXBFLS(AFLS+I-1)
            ENDDO
            DO I=1,NMOD-NMST
               FXBDLS_L(ADLS_L+I-1)=FXBDLS(ADLS+I-1)
            ENDDO
            DO I=1,NME*NLG+NMOD*9*NLG
               FXBGRVR_L(AGRVR_L+I-1)=FXBGRVR(AGRVR+I-1)
            ENDDO
         ENDIF
      ENDDO       
C
      RETURN
      END            
