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
!||    noise             ../engine/source/general_controls/computation/noise.F
!||--- called by ------------------------------------------------------
!||    sortie_main       ../engine/source/output/sortie_main.F
!||--- calls      -----------------------------------------------------
!||    cur_fil_c         ../common_source/tools/input_output/write_routtines.c
!||    initnoise         ../engine/source/general_controls/computation/initnoise.F
!||    initnoise2        ../engine/source/general_controls/computation/initnoise.F
!||    pnoise            ../engine/source/general_controls/computation/pnoise.F
!||    spmd_glob_dsum9   ../engine/source/mpi/interfaces/spmd_th.F
!||    wrtdes            ../engine/source/output/th/wrtdes.F
!||--- uses       -----------------------------------------------------
!||    elbufdef_mod      ../common_source/modules/mat_elem/elbufdef_mod.F90
!||====================================================================
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
      my_real,DIMENSION(:),ALLOCATABLE ::  C
      INTEGER IPELNOI,IPNOIADD,IPTC
      INTEGER IFIRST,I,K,L,NE,NC,KK,IAD, LEN
      my_real PI1,PI2,TOL,TE,TC,FNE,FAC,DEC,TO,TRANS,XI,W,CC,TTN
      INTEGER LENGTH, ITAG(0:NUMNOD)
      SAVE IFIRST,TE,IPTC,C,NC,TO,IPELNOI,ELNOI,ELG,IPNOIADD,NOIADD
      DATA IFIRST/0/
      my_real WA(NNOISE)
C-----------------------------------------------
C INITIALIALISATION DES COEFFICIENTS DU FILTRE
C-----------------------------------------------
      NE = 0 
      TOL=EM01
      IF(ISPMD==0) THEN
        IUNIT=IUNOI
        IF(ITFORM==3) CALL CUR_FIL_C(IUNIT)
      ENDIF
      IF(IFIRST==0)THEN
        IFIRST=1
        IF(RNOI==0)THEN
C NE : NOMBRE D'ECHANTILLON ENTRE CHAQUE SORTIE
        TE=DT2
         DT2R=TE
        NE=MAX(1,INT(DTNOISE/TE))
         J(7)=NE
        ELSEIF(RNOI==1)THEN
         TE=DT2R
         NE=J(7)
        ENDIF
C
C INITIALISATION DES TABLEAUX D'ELEMENTS PAR NOEUDS SAUVES
C
        IF(NOISEP/=0)THEN
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
        FAC=ONE/FLOAT(NE)
        DEC=FLOAT(3*NE) - HALF
        TO=FLOAT(3*NE)*TE
        IF(NC>0)THEN
          ALLOCATE(C(NC))
        ENDIF
C
C COEFF. DU FILTRE PARFAIT AVEC FENETRE DE HAMMING 
C
        PI2 = TWO*PI
        TRANS=0
        DO I=1,NC
          XI=FLOAT(I-1)-DEC
          W=0.54+0.46*COS(PI2*XI/FLOAT(NC))
          IF(ABS(PI*FAC*XI)<TOL)THEN
            C(I)=FAC
          ELSE
            C(I)=W*SIN(PI*XI*FAC)/PI/XI
          ENDIF
          TRANS=TRANS+C(I)
        ENDDO
C
        IF(ISPMD==0) THEN
          WRITE(IOUT,1000)ONE/DTNOISE,ONE/TC,NC,TRANS
          WRITE(IOUT,'(1P8E10.3)')(C(I),I=1,NC)
          WRITE(IOUT,'(//)')
        ENDIF
C           
        IF(RNOI==0)THEN
         DO K=1,6
          J(K)=(1-K)*NE-1
         ENDDO
         DO I=1,6*NNOISE
          DO L=1,NCNOIS
            BUF(L,I)=ZERO
          ENDDO
         ENDDO
        ENDIF  
      ENDIF
      IF(ISPMD==0) THEN
        IF(ABS(1.-DT2/TE)>TOL)THEN
          WRITE(IOUT,1100)
          WRITE(ISTDO,1100)
        ENDIF
      ENDIF
      IF(RNOI==0 .AND. TT-TO<TNOISE-NC*TE)RETURN
C-----------------------------------------------
C PRESSION
C-----------------------------------------------
      IF(NOISEP/=0)CALL PNOISE(ELNOI,ELG,NOIADD,ELBUF_TAB,WA,IPARG)
C-----------------------------------------------
C FILTRAGE
C-----------------------------------------------
C
C Anullation buffer noise sur pi, i<>0 en SPMD
C
      IF(ISPMD/=0) THEN
        DO I=1,6*NNOISE
          DO L=1,NCNOIS
            BUF(L,I)=ZERO   
          ENDDO
        ENDDO
      ENDIF
C
      DO K=1,6
        J(K)=J(K)+1
        IF(J(K)>0)THEN
         CC=C(J(K))
         KK=(K-1)*NNOISE
         DO I=1,NNOISE
            KK=KK+1
            IF(IN(I) /= 0)THEN      ! test if present on current domain (spmd)
             IF(WEIGHT(IN(I))==1) THEN
              IF(NOISEV/=0)THEN
               BUF(1,KK)=BUF(1,KK)+CC *V(1,IN(I))
               BUF(2,KK)=BUF(2,KK)+CC *V(2,IN(I))
               BUF(3,KK)=BUF(3,KK)+CC *V(3,IN(I))
              ENDIF
              IF(NOISEA/=0)THEN
               IAD=3*NOISEV
               BUF(IAD+1,KK)=BUF(IAD+1,KK)+CC *A(1,IN(I))
               BUF(IAD+2,KK)=BUF(IAD+2,KK)+CC *A(2,IN(I))
               BUF(IAD+3,KK)=BUF(IAD+3,KK)+CC *A(3,IN(I))
              ENDIF
             ENDIF
            END IF
            IF(NOISEP/=0.AND.ISPMD==0)THEN
              IAD=3*NOISEV+3*NOISEA+1
              BUF(IAD,KK)=BUF(IAD,KK)-CC *WA(I)
            ENDIF
          ENDDO ! I
        ENDIF
      ENDDO ! K
C
C GATHER SPMD BUF
C
      IF(NSPMD>1) THEN
        LEN = 6*NNOISE*NCNOIS
        CALL SPMD_GLOB_DSUM9(BUF,LEN)
      ENDIF
C-----------------------------------------------------
C INCREMENTATION DES INDICES ET ECRITURE DES RESULTATS
C-----------------------------------------------------
      DO K=1,6
        IF(J(K)==NC)THEN
         J(K)=0
         KK=(K-1)*NNOISE+1
         TTN=TT-TO
         IF(ISPMD==0) THEN
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
      ENDDO
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
