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
Chd|  NOISE                         source/general_controls/computation/noise.F
Chd|-- called by -----------
Chd|        SORTIE_MAIN                   source/output/sortie_main.F   
Chd|-- calls ---------------
Chd|        CUR_FIL_C                     source/output/tools/sortie_c.c
Chd|        INITNOISE                     source/general_controls/computation/initnoise.F
Chd|        INITNOISE2                    source/general_controls/computation/initnoise.F
Chd|        PNOISE                        source/general_controls/computation/pnoise.F
Chd|        SPMD_GLOB_DSUM9               source/mpi/interfaces/spmd_th.F
Chd|        WRTDES                        source/output/th/wrtdes.F     
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE NOISE(
     1   DT2R  ,IN,J     ,BUF  ,V     ,
     2   A     ,IXS,ELBUF_TAB,IPARG,WEIGHT,
     3   IXQ   )
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
#include      "r4r8_p.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "units_c.inc"
#include      "scrnoi_c.inc"
#include      "scr05_c.inc"
#include      "scr13_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IN(*),J(*),IXS(*),IPARG(*), WEIGHT(*),IXQ(*)
      my_real
     . BUF(NCNOIS,*),V(3,*),DT2R ,A(3,*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER , DIMENSION(:), ALLOCATABLE :: ELNOI,ELG,NOIADD
      my_real
     &  ,DIMENSION(:),ALLOCATABLE ::  C
      INTEGER IPELNOI,IPNOIADD,IPTC
      INTEGER IFIRST,I,K,L,NE,NC,KK,IAD, LEN
      my_real
     . PI1,PI2,TOL,TE,TC,FNE,FAC,DEC,TO,    
     . TRANS,XI,W,CC,TTN
      INTEGER LENGTH, ITAG(0:NUMNOD)
      SAVE IFIRST,TE,IPTC,C,NC,TO,IPELNOI,ELNOI,ELG,IPNOIADD,NOIADD
      DATA IFIRST/0/
      my_real
     .     WA(NNOISE)
C-----------------------------------------------
C INITIALIALISATION DES COEFFICIENTS DU FILTRE
C-----------------------------------------------
      TOL=EM01
      IF(IMACH.NE.3.OR.ISPMD.EQ.0) THEN
        IUNIT=IUNOI
        IF(ITFORM.EQ.3) CALL CUR_FIL_C(IUNIT)
      ENDIF
      IF(IFIRST.EQ.0)THEN
        IFIRST=1
        IF(RNOI.EQ.0)THEN
C NE : NOMBRE D'ECHANTILLON ENTRE CHAQUE SORTIE
        TE=DT2
         DT2R=TE
        NE=MAX(1,INT(DTNOISE/TE))
         J(7)=NE
        ELSEIF(RNOI.EQ.1)THEN
         TE=DT2R
         NE=J(7)
        ENDIF
C
C INITIALISATION DES TABLEAUX D'ELEMENTS PAR NOEUDS SAUVES
C
        IF(NOISEP.NE.0)THEN
          ALLOCATE (NOIADD(NNOISE+1))
          CALL INITNOISE(IN,NOIADD,IXS,ITAG(0),LENGTH,IXQ)
          ALLOCATE(ELNOI(LENGTH))        
          ALLOCATE(ELG(LENGTH))
          CALL INITNOISE2(IN,ELNOI,ELG,NOIADD,IXS,ITAG(1),IPARG,IXQ)
        ENDIF
C TC PERIODE DE COUPURE
        DTNOISE=FLOAT(NE)*TE
        TC=2.*DTNOISE
C NOMBRE DE COEFFICIENTS ET ALLOCATION ; TO DEPHASAGE
        NC=6*NE
        FAC=UN/FLOAT(NE)
        DEC=FLOAT(3*NE) - UNDEMI
        TO=FLOAT(3*NE)*TE
        IF(NC.GT.0)THEN
          ALLOCATE(C(NC))
        ENDIF
C
C COEFF. DU FILTRE PARFAIT AVEC FENETRE DE HAMMING 
C
        PI2 = DEUX*PI
        TRANS=0
        DO 10 I=1,NC
          XI=FLOAT(I-1)-DEC
          W=0.54+0.46*COS(PI2*XI/FLOAT(NC))
          IF(ABS(PI*FAC*XI).LT.TOL)THEN
            C(I)=FAC
          ELSE
            C(I)=W*SIN(PI*XI*FAC)/PI/XI
          ENDIF
          TRANS=TRANS+C(I)
 10     CONTINUE
C
        IF(IMACH.NE.3.OR.ISPMD.EQ.0) THEN
          WRITE(IOUT,1000)UN/DTNOISE,UN/TC,NC,TRANS
          WRITE(IOUT,'(1P8E10.3)')(C(I),I=1,NC)
          WRITE(IOUT,'(//)')
        ENDIF
C           
        IF(RNOI.EQ.0)THEN
         DO 20 K=1,6
 20       J(K)=(1-K)*NE-1
         DO I=1,6*NNOISE
          DO L=1,NCNOIS
            BUF(L,I)=ZERO
          ENDDO
         ENDDO
        ENDIF  
      ENDIF
      IF(IMACH.NE.3.OR.ISPMD.EQ.0) THEN
        IF(ABS(1.-DT2/TE).GT.TOL)THEN
          WRITE(IOUT,1100)
          WRITE(ISTDO,1100)
        ENDIF
      ENDIF
      IF(RNOI.EQ.0 .AND. TT-TO.LT.TNOISE-NC*TE)RETURN
C-----------------------------------------------
C PRESSION
C-----------------------------------------------
      IF(NOISEP.NE.0)
     .  CALL PNOISE(ELNOI,ELG,NOIADD,ELBUF_TAB,WA,IPARG)
C-----------------------------------------------
C FILTRAGE
C-----------------------------------------------
C
C Anullation buffer noise sur pi, i<>0 en SPMD
C
      IF(IMACH.EQ.3.AND.ISPMD.NE.0) THEN
        DO I=1,6*NNOISE
          DO L=1,NCNOIS
            BUF(L,I)=ZERO	 
          ENDDO
        ENDDO
      ENDIF
C
      DO 100 K=1,6
      J(K)=J(K)+1
      IF(J(K).GT.0)THEN
       CC=C(J(K))
       KK=(K-1)*NNOISE
       DO 110 I=1,NNOISE
          KK=KK+1
          IF(IN(I).NE.0)THEN          ! test preliminaire necessaire en spmd (presence noeud sur proc)
           IF(WEIGHT(IN(I)).EQ.1) THEN
            IF(NOISEV.NE.0)THEN
             BUF(1,KK)=BUF(1,KK)+CC *V(1,IN(I))
             BUF(2,KK)=BUF(2,KK)+CC *V(2,IN(I))
	            BUF(3,KK)=BUF(3,KK)+CC *V(3,IN(I))
            ENDIF
            IF(NOISEA.NE.0)THEN
             IAD=3*NOISEV
             BUF(IAD+1,KK)=BUF(IAD+1,KK)+CC *A(1,IN(I))
             BUF(IAD+2,KK)=BUF(IAD+2,KK)+CC *A(2,IN(I))
	            BUF(IAD+3,KK)=BUF(IAD+3,KK)+CC *A(3,IN(I))
            ENDIF
           ENDIF
          END IF
          IF(NOISEP.NE.0.AND.ISPMD.EQ.0)THEN
            IAD=3*NOISEV+3*NOISEA+1
            BUF(IAD,KK)=BUF(IAD,KK)-CC *WA(I)
          ENDIF
 110    CONTINUE
      ENDIF
 100  CONTINUE
C
C GATHER SPMD BUF
C
      IF(IMACH.EQ.3.AND.NSPMD.GT.1) THEN
        LEN = 6*NNOISE*NCNOIS
        CALL SPMD_GLOB_DSUM9(BUF,LEN)
      ENDIF
C-----------------------------------------------------
C INCREMENTATION DES INDICES ET ECRITURE DES RESULTATS
C-----------------------------------------------------
      DO 200 K=1,6
        IF(J(K).EQ.NC)THEN
         J(K)=0
         KK=(K-1)*NNOISE+1
         TTN=TT-TO
         IF(IMACH.NE.3.OR.ISPMD.EQ.0) THEN
           CALL WRTDES(TTN,TTN,1,ITFORM,1)
           CALL WRTDES(TTN,TTN,1,ITFORM,1)
           CALL WRTDES(BUF(1,KK),BUF(1,KK),NCNOIS*NNOISE,ITFORM,1)
         ENDIF
         DO I=(K-1)*NNOISE+1,K*NNOISE
          DO L=1,NCNOIS
            BUF(L,I)=ZERO
          ENDDO
         ENDDO
        ENDIF
 200  CONTINUE
c
C-----------
      RETURN
 1000 FORMAT(///
     .  ' OUTPUT OF SAMPLED VELOCITIES FOR NOISE AND VIBRATION  ',//
     .  ' EFFECTIVE SAMPLING FREQUENCY . . . . . . . . . . ',1PE10.4/
     .  ' HIGH FREQUENCY CUTOFF . . .  . . . . . . . . . . ',1PE10.4/
     .  ' NUMBER OF COEFFICIENT USED FOR FILTERING . . . . ',I10/
     .  ' STATIC TRANSMITANCE OF FILTER  . . . . . . . . . ',1PE10.4/
     .  ' LIST OF COEFFICIENTS USED FOR FILTERING  . . . . ')
 1100 FORMAT('*** WARNING STRUCTURAL TIME STEP MUST BE CONSTANT ***')
      END
