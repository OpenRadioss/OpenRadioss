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
Chd|  RBE3T1                        source/constraints/general/rbe3/rbe3f.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ASP2_RBE3                     source/constraints/general/rbe3/rbe3f.F
Chd|        ASS_RBE3                      source/constraints/general/rbe3/rbe3f.F
Chd|        DMI_RBE3                      source/constraints/general/rbe3/rbe3f.F
Chd|        FOAT_TO_6_FLOAT               source/system/parit.F         
Chd|        PRERBE3                       source/constraints/general/rbe3/rbe3f.F
Chd|        PRERBE3P                      source/constraints/general/rbe3/rbe3f.F
Chd|        RBE3F                         source/constraints/general/rbe3/rbe3f.F
Chd|        RBE3POFF                      source/constraints/general/rbe3/rbe3f.F
Chd|        SPMD_EXCH_RBE3                source/mpi/kinematic_conditions/spmd_exch_rbe3.F
Chd|        SPMD_EXCH_RBE3_PON            source/mpi/kinematic_conditions/spmd_exch_rbe3_pon.F
Chd|        SPMD_MAX_II                   source/mpi/implicit/imp_spmd.F
Chd|        ZERO1                         source/system/zero.F          
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE RBE3T1(IRBE3 ,LRBE3 ,X     ,A      ,AR     ,
     1                  MS    ,IN    ,FRBE3 ,SKEW   ,WEIGHT ,
     2                  STIFN ,STIFR ,IAD_M ,FR_M   ,ISIZE  ,
     3                  FR_MPON,DMAST ,ADM   ,DINERT,ADI    ,
     4                  RSUM  ,RSUM_PON,H3D_DATA )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE H3D_MOD
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
#include      "parit_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE3(NRBE3L,*),LRBE3(*),WEIGHT(*),IAD_M(*) ,FR_M(*),
     .        ISIZE,FR_MPON(*)
C     REAL
      my_real
     .   X(3,*), A(3,*), AR(3,*), MS(*), IN(*), FRBE3(*),SKEW(*),
     .   STIFN(*) ,STIFR(*),DMAST,ADM(*),DINERT,ADI(*) ,RSUM(*)
      DOUBLE PRECISION
     .   RSUM_PON(*)
      TYPE (H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, MAX_M,IROTG,JT(3,NRBE3),JR(3,NRBE3),IERR,NMT,
     .        IADA,IADMS,IADFN,IADAR,IADIN,IADFR,IADM0,IADI0,IADL,
     .        IPA,IPMS,IPFN,IPAR,IPIN,IPFR,NMP,IADLP,NS,NML,ICOM,
     .        IADLP1,IADM1,IADI1,NMT0,IADMP(SLRBE3/2),IML(SLRBE3/2)
C     REAL
C------------allacation will be removed to ini_ uniforming smp spmd in v11
C      my_real
C     .      , DIMENSION(:), ALLOCATABLE :: RSUM
C      DOUBLE PRECISION
C     .      , DIMENSION(:), ALLOCATABLE :: RSUM_PON
C======================================================================|
      I7KGLO = 1
      NMT0 = SLRBE3/2
      CALL PRERBE3(IRBE3 ,MAX_M , IROTG,JT  ,JR   )
      ICOM = IAD_M(NSPMD+1)-IAD_M(1)
      IF (NSPMD>1)CALL SPMD_MAX_II(IROTG,IAD_M,ICOM)
      IF (NMT0>0) THEN
       CALL PRERBE3P(IRBE3 ,LRBE3 ,IADMP ,IML   , NMT   )
       IADA=1
       IADMS=IADA+3*NMT
       IADFN=IADMS+NMT
       IF (IROTG>0) THEN
        IADAR=IADFN+NMT
        IADIN=IADAR+3*NMT
        IADFR=IADIN+NMT
       ELSE
        IADAR=IADFN
        IADIN=IADAR
        IADFR=IADIN
       ENDIF
       IADL=IADFR+NMT
C       ALLOCATE(RSUM(IADL),STAT=IERR)
       CALL ZERO1(RSUM,IADL)
       CALL RBE3F(IRBE3 ,LRBE3 ,X    ,A     ,AR    ,
     1           MS    ,IN    ,FRBE3,SKEW  ,WEIGHT,
     2           STIFN ,STIFR ,JT   ,JR    ,IROTG ,
     3           MAX_M ,RSUM(IADA),RSUM(IADAR) ,RSUM(IADMS),
     4           RSUM(IADIN),RSUM(IADFN),RSUM(IADFR),NMT0  ,
     5           IADMP )
C
       IF (NSPMD>1.AND.IPARIT==0) THEN
            CALL RBE3POFF(IRBE3 ,LRBE3 ,A    ,MS    ,WEIGHT,
     1                    AR    ,IN    ,STIFN,STIFR )
       END IF
       IF (IPARIT>0.AND.IMACH==3) THEN
        NMP = 6*NMT
        IPA=1
        IPMS=IPA+3*NMP
        IPFN=IPMS+NMP
        IF (IROTG>0) THEN
         IPAR=IPFN+NMP
         IPIN=IPAR+3*NMP
         IPFR=IPIN+NMP
        ELSE
         IPAR=IPFN
         IPIN=IPAR
         IPFR=IPIN
        ENDIF
        IADLP=IPFR+NMP
C         version spmd p/on
C        ALLOCATE(RSUM_PON(IADLP),STAT=IERR)
C	 RSUM_PON=ZERO
        CALL FOAT_TO_6_FLOAT(1  ,NMT*3  ,RSUM(IADA) ,RSUM_PON(IPA) )
        CALL FOAT_TO_6_FLOAT(1  ,NMT    ,RSUM(IADMS),RSUM_PON(IPMS))
        CALL FOAT_TO_6_FLOAT(1  ,NMT    ,RSUM(IADFN),RSUM_PON(IPFN))
        IF (IROTG>0) THEN
         CALL FOAT_TO_6_FLOAT(1  ,NMT*3  ,RSUM(IADAR) ,RSUM_PON(IPAR))
         CALL FOAT_TO_6_FLOAT(1  ,NMT    ,RSUM(IADIN),RSUM_PON(IPIN))
         CALL FOAT_TO_6_FLOAT(1  ,NMT    ,RSUM(IADFR),RSUM_PON(IPFR))
        ENDIF
        IF (ICOM>0) THEN
            CALL SPMD_EXCH_RBE3_PON(
     .       RSUM_PON(IPA),RSUM_PON(IPAR),RSUM_PON(IPMS),RSUM_PON(IPIN),
     .       RSUM_PON(IPFN),RSUM_PON(IPFR),FR_MPON,IAD_M  ,
     .       IAD_M(NSPMD+1),ISIZE,IROTG)
        ENDIF
C
C Routine assemblage parith/ON
C
        CALL ASP2_RBE3(IRBE3 ,LRBE3 ,A     ,AR    ,MS    ,
     1                IN     ,WEIGHT,STIFN ,STIFR ,RSUM_PON(IPA),
     2                RSUM_PON(IPAR),RSUM_PON(IPMS),RSUM_PON(IPIN),
     3                RSUM_PON(IPFN),RSUM_PON(IPFR),NMT  ,IML  ,IROTG)
C        DEALLOCATE(RSUM_PON)
       ELSE
C-----------------A-=A*W+DA-----
        CALL ASS_RBE3(IRBE3 ,LRBE3 ,A     ,AR    ,MS    ,
     1                IN    ,WEIGHT,STIFN ,STIFR ,RSUM(IADA),
     2                RSUM(IADAR) ,RSUM(IADMS), RSUM(IADIN),
     3                RSUM(IADFN) ,RSUM(IADFR),NMT   ,IML  ,IROTG)
        IF (IPARIT==0.AND.ICOM>0) THEN
            CALL SPMD_EXCH_RBE3(
     .       A    ,AR   ,MS      ,IN     ,STIFN,
     .       STIFR,FR_M ,IAD_M   ,IAD_M(NSPMD+1),ISIZE,
     .       IROTG)
        ENDIF
       END IF
C
c--------------calcul dms,diner---
       IADM0=  NMT0*6 + 1
       IADI0=  IADM0 + NMT0
       CALL DMI_RBE3(NMT   ,LRBE3 ,FRBE3(IADM0),FRBE3(IADI0),
     1               RSUM(IADMS)  ,RSUM(IADIN) ,DMAST ,ADM  ,
     2               DINERT,ADI   ,IROTG   ,IRBE3  ,MS      ,
     3               IN    ,WEIGHT,IADMP   ,H3D_DATA)
C       DEALLOCATE(RSUM)
      END IF ! IF (NMT>0)
C
C---  reset of secnd nodes forces is necessary w/AMS
      DO N=1,NRBE3
        NS  = IRBE3(3,N)
        IF(NS/=0) THEN
          IF (WEIGHT(NS)/=0) THEN
            DO J = 1,3
              IF(JT(J,N)/=0)A(J,NS)=ZERO
            END DO
          ENDIF
        END IF
      END DO
C---
      RETURN
      END
Chd|====================================================================
Chd|  RBE3POFF                      source/constraints/general/rbe3/rbe3f.F
Chd|-- called by -----------
Chd|        RBE3T1                        source/constraints/general/rbe3/rbe3f.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE RBE3POFF(IRBE3 ,LRBE3 ,A    ,MS    ,WEIGHT,
     1                    AR    ,IN    ,STIFN,STIFR )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE3(NRBE3L,*),LRBE3(*),WEIGHT(*)
C     REAL
      my_real
     .   A(3,*),AR(3,*),MS(*), IN(*),STIFN(*),STIFR(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, NML, IAD,IROT,M,NS
C-----------------------------------------------
      DO N=1,NRBE3
        IAD = IRBE3(1,N)
        NML = IRBE3(5,N)
        NS  = IRBE3(3,N)
	IROT =IRBE3(6,N)
#include    "vectorize.inc"
        DO I=IAD+1,IAD+NML
         M = LRBE3(I)
	 A(1,M) = A(1,M)*WEIGHT(M)
	 A(2,M) = A(2,M)*WEIGHT(M)
	 A(3,M) = A(3,M)*WEIGHT(M)
	 MS(M)  = MS(M)*WEIGHT(M)
	 STIFN(M)= STIFN(M)*WEIGHT(M)
        ENDDO
        IF (IROT>0) THEN
#include    "vectorize.inc"
         DO I=IAD+1,IAD+NML
          M = LRBE3(I)
	  AR(1,M) = AR(1,M)*WEIGHT(M)
	  AR(2,M) = AR(2,M)*WEIGHT(M)
	  AR(3,M) = AR(3,M)*WEIGHT(M)
	  IN(M) =  IN(M)*WEIGHT(M)
	  STIFR(M) = STIFR(M)*WEIGHT(M)
         ENDDO
        ENDIF
      ENDDO
C-----------
      RETURN
      END

Chd|====================================================================
Chd|  RBE3F                         source/constraints/general/rbe3/rbe3f.F
Chd|-- called by -----------
Chd|        RBE3T1                        source/constraints/general/rbe3/rbe3f.F
Chd|-- calls ---------------
Chd|        MFAC_RBE3                     source/constraints/general/rbe3/rbe3f.F
Chd|        RBE3CL                        source/constraints/general/rbe3/rbe3f.F
Chd|====================================================================
      SUBROUTINE RBE3F(IRBE3 ,LRBE3 ,X    ,A     ,AR    ,
     1                 MS    ,IN    ,FRBE3,SKEW  ,WEIGHT,
     2                 STIFN ,STIFR ,JT   ,JR    ,IROTG ,
     3                 MAX_M ,AM    ,ARM  ,MSM   ,INM   ,
     4                 STIFNM,STIFRM,NMT0 ,IADMP )
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE3(NRBE3L,*),LRBE3(*),WEIGHT(*)
      INTEGER MAX_M,IROTG,JT(3,*),JR(3,*),NMT0,IADMP(*)
C     REAL
      my_real
     .   X(3,*), A(3,*), AR(3,*), MS(*), IN(*), FRBE3(*),SKEW(*),
     .   STIFN(*) ,STIFR(*), AM(3,*), ARM(3,*), MSM(*), INM(*),
     .   STIFNM(*) ,STIFRM(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, NS ,NML, IAD,JJ,IROT,IADS,NM,NN,K,IMOD,IADF
C     REAL
      my_real
     .     FNS(3),MNS(3),MSS(3),INS(3),STN(3),STR(3),FSUM,MSUM,
     .     FMAX,SMAX,MMAX,SFD,SMD
      my_real,
     .         DIMENSION(:,:,:),ALLOCATABLE :: FDSTNB ,MDSTNB

C======================================================================|
      IADS = NMT0
      ALLOCATE(FDSTNB(3,6,MAX_M))
      IF (IROTG>0) ALLOCATE(MDSTNB(3,6,MAX_M))
      DO N=1,NRBE3
        IAD = IRBE3(1,N)
        NS  = IRBE3(3,N)
        NML = IRBE3(5,N)
        IROT =IRBE3(6,N)
        IMOD =IRBE3(8,N)
       IF (NS>0) THEN
       IF (WEIGHT(NS)==1) THEN
        CALL RBE3CL(LRBE3(IAD+1),LRBE3(IADS+IAD+1),NS     ,X      ,
     .              FRBE3(6*IAD+1),SKEW    ,NML     ,IROT   ,FDSTNB ,
     .              MDSTNB  ,IRBE3(2,N))
          DO J = 1,3
           NN = JT(J,N)*WEIGHT(NS)
           FNS(J) = A(J,NS)*NN
           MSS(J) = MS(NS)*NN/3
           STN(J) = STIFN(NS)*NN
          ENDDO
C---not to add supplementary mass globaly
         IF (IMOD <=3) THEN
           CALL MFAC_RBE3(FDSTNB,MDSTNB,NML  ,IROTG,SFD ,SMD)
           DO I=1,NML
             K = IADMP(IAD+I)
             DO J = 1,3
              FSUM = FDSTNB(J,1,I)+FDSTNB(J,2,I)+FDSTNB(J,3,I)
              MSM(K) = MSM(K)+ABS(FSUM)*MSS(J)*SFD
            ENDDO
           ENDDO
         ELSEIF (IMOD ==4) THEN
           DO I=1,NML
             K = IADMP(IAD+I)
             IADF =6*(IAD+I-1)
             DO J = 1,3
              MSM(K) = MSM(K)+FRBE3(IADF+J)*MSS(J)
             ENDDO
           ENDDO
         END IF
        DO I=1,NML
          K = IADMP(IAD+I)
          DO J = 1,3
            AM(1,K) = AM(1,K)+FDSTNB(1,J,I)*FNS(J)
            AM(2,K) = AM(2,K)+FDSTNB(2,J,I)*FNS(J)
            AM(3,K) = AM(3,K)+FDSTNB(3,J,I)*FNS(J)
          ENDDO
C-----IMOD=4  STIFNM might be over_estimated but safe         
           SMAX = ZERO
          DO J = 1,3
           FMAX=ABS(FDSTNB(J,1,I))+ABS(FDSTNB(J,2,I))+ABS(FDSTNB(J,3,I))
           SMAX = MAX(SMAX,FMAX*STN(J))
          ENDDO
           STIFNM(K) = STIFNM(K)+SMAX
        ENDDO
        IF ((JR(1,N)+JR(2,N)+JR(3,N))>0) THEN
         DO J = 1,3
	     NN = JR(J,N)*WEIGHT(NS)
	     MNS(J) = AR(J,NS)*NN
	     INS(J) = IN(NS)*NN/3
	     STR(J) = STIFR(NS)*NN
         ENDDO
         DO I=1,NML
	      K = IADMP(IAD+I)
          DO J = 1,3
	      AM(1,K) = AM(1,K)+FDSTNB(1,J+3,I)*MNS(J)
	      AM(2,K) = AM(2,K)+FDSTNB(2,J+3,I)*MNS(J)
	      AM(3,K) = AM(3,K)+FDSTNB(3,J+3,I)*MNS(J)
          ENDDO
           SMAX = ZERO
          DO J = 1,3
C	   FSUM = FDSTNB(J,J+3,I)
            FSUM = FDSTNB(J,4,I)+FDSTNB(J,5,I)+FDSTNB(J,6,I)
            MSM(K) =MSM(K)+ABS(FSUM)*INS(J)
            FMAX=ABS(FDSTNB(J,4,I))+ABS(FDSTNB(J,5,I))+ABS(FDSTNB(J,6,I))
            SMAX = MAX(SMAX,FMAX*STR(J))
          ENDDO
	      STIFNM(K) = STIFNM(K)+SMAX
         ENDDO
        ENDIF
        IF (IROT>0) THEN
         DO I=1,NML
          K = IADMP(IAD+I)
          DO J = 1,3
	       ARM(1,K) = ARM(1,K)+MDSTNB(1,J,I)*FNS(J)
	       ARM(2,K) = ARM(2,K)+MDSTNB(2,J,I)*FNS(J)
	       ARM(3,K) = ARM(3,K)+MDSTNB(3,J,I)*FNS(J)
          ENDDO
          SMAX = ZERO
          DO J = 1,3
C	   MSUM = MDSTNB(J,J,I)
	      MSUM = MDSTNB(J,1,I)+MDSTNB(J,2,I)+MDSTNB(J,3,I)
 	      IF (IMOD /=4) INM(K) = INM(K)+ABS(MSUM)*MSS(J)
	      MMAX=ABS(MDSTNB(J,1,I))+ABS(MDSTNB(J,2,I))+ABS(MDSTNB(J,3,I))
              SMAX = MAX(SMAX,MMAX*STN(J))
          ENDDO
	      STIFRM(K) = STIFRM(K)+SMAX
         ENDDO
         IF ((JR(1,N)+JR(2,N)+JR(3,N))>0) THEN
          IF (IMOD <=3) THEN
            DO I=1,NML
              K = IADMP(IAD+I)
              DO J = 1,3
               MSUM = MDSTNB(J,4,I)+MDSTNB(J,5,I)+MDSTNB(J,6,I)
               INM(K) = INM(K)+ABS(MSUM)*INS(J)*SMD
             ENDDO
            ENDDO
          ELSEIF (IMOD ==4) THEN
            DO I=1,NML
              K = IADMP(IAD+I)
              IADF =6*(IAD+I-1)
              DO J = 1,3
               INM(K) = INM(K)+FRBE3(IADF+J+3)*INS(J)
              ENDDO
            ENDDO
          END IF
          DO I=1,NML
           K = IADMP(IAD+I)
           DO J = 1,3
	       ARM(1,K) = ARM(1,K)+MDSTNB(1,J+3,I)*MNS(J)
	       ARM(2,K) = ARM(2,K)+MDSTNB(2,J+3,I)*MNS(J)
	       ARM(3,K) = ARM(3,K)+MDSTNB(3,J+3,I)*MNS(J)
           ENDDO
           SMAX = ZERO
           DO J = 1,3
	      MMAX=ABS(MDSTNB(J,4,I))+ABS(MDSTNB(J,5,I))+ABS(MDSTNB(J,6,I))
              SMAX = MAX(SMAX,MMAX*STR(J))
           ENDDO
	      STIFRM(K) = STIFRM(K)+SMAX
          ENDDO
         ENDIF
        ENDIF
C	MS(NS) = ZERO
	STIFN(NS) = EM20
        IF ((JR(1,N)+JR(2,N)+JR(3,N))>0) STIFR(NS) = EM20
       ENDIF
       END IF ! IF (NS>0) THEN
      ENDDO
C
      DEALLOCATE(FDSTNB)
      IF (IROTG>0) DEALLOCATE(MDSTNB)
C---
      RETURN
      END
Chd|====================================================================
Chd|  ASS_RBE3                      source/constraints/general/rbe3/rbe3f.F
Chd|-- called by -----------
Chd|        RBE3T1                        source/constraints/general/rbe3/rbe3f.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ASS_RBE3(IRBE3 ,LRBE3 ,A     ,AR    ,MS    ,
     1                    IN    ,WEIGHT,STIFN ,STIFR ,DA    ,
     2                    DAR   ,DMS   ,DIN   ,DSTIFN,DSTIFR,
     3                    NMT   ,IML   ,IROTG)
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE3(NRBE3L,*),LRBE3(*),WEIGHT(*),NMT ,IML(*),IROTG
C     REAL
      my_real
     .   A(3,*), AR(3,*), MS(*), IN(*),
     .   STIFN(*) ,STIFR(*), DA(3,*), DAR(3,*), DMS(*), DIN(*),
     .   DSTIFN(*) ,DSTIFR(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, NS ,NML, IAD,JJ,IROT,M
C     REAL
C======================================================================|
#include    "vectorize.inc"
      DO I=1,NMT
         M = IML(I)
	 A(1,M) = A(1,M) + DA(1,I)
	 A(2,M) = A(2,M) + DA(2,I)
	 A(3,M) = A(3,M) + DA(3,I)
	 MS(M)  = MS(M)  + DMS(I)
	 STIFN(M)= STIFN(M) + DSTIFN(I)
      ENDDO
      IF (IROTG>0) THEN
       DO I=1,NMT
          M = IML(I)
	  AR(1,M) = AR(1,M) + DAR(1,I)
	  AR(2,M) = AR(2,M) + DAR(2,I)
	  AR(3,M) = AR(3,M) + DAR(3,I)
	  IN(M) =  IN(M)  +  DIN(I)
	  STIFR(M) = STIFR(M) + DSTIFR(I)
       ENDDO
      ENDIF
      RETURN
C
      DO N=1,NRBE3
        IAD = IRBE3(1,N)
        NS  = IRBE3(3,N)
        NML = IRBE3(5,N)
	IROT =IRBE3(6,N)
        IF (NS==0.OR.WEIGHT(NS)==0) CYCLE
        DO I=IAD+1,IAD+NML
         M = LRBE3(I)
	 A(1,M) = A(1,M) + DA(1,I)
	 A(2,M) = A(2,M) + DA(2,I)
	 A(3,M) = A(3,M) + DA(3,I)
	 MS(M)  = MS(M)  + DMS(I)
	 STIFN(M)= STIFN(M) + DSTIFN(I)
        ENDDO
        IF (IROT>0) THEN
         DO I=IAD+1,IAD+NML
          M = LRBE3(I)
	  AR(1,M) = AR(1,M) + DAR(1,I)
	  AR(2,M) = AR(2,M) + DAR(2,I)
	  AR(3,M) = AR(3,M) + DAR(3,I)
	  IN(M) =  IN(M)  +  DIN(I)
	  STIFR(M) = STIFR(M) + DSTIFR(I)
         ENDDO
        ENDIF
      ENDDO
C---
      RETURN
      END
Chd|====================================================================
Chd|  ASP0_RBE3                     source/constraints/general/rbe3/rbe3f.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ASP0_RBE3(IRBE3 ,LRBE3 ,A     ,AR    ,MS    ,
     1                     IN    ,WEIGHT,STIFN ,STIFR ,DA    ,
     2                     DAR   ,DMS   ,DIN   ,DSTIFN,DSTIFR)
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE3(NRBE3L,*),LRBE3(*),WEIGHT(*)
C     REAL
      my_real
     .   A(3,*), AR(3,*), MS(*), IN(*),
     .   STIFN(*) ,STIFR(*), DA(3,*), DAR(3,*), DMS(*), DIN(*),
     .   DSTIFN(*) ,DSTIFR(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, NS ,NML, IAD,JJ,IROT,M,ITAG(NUMNOD)
C     REAL
C======================================================================|
      DO N =1,NUMNOD
       ITAG(N) = 0
      END DO
      DO N=1,NRBE3
        IAD = IRBE3(1,N)
        NS  = IRBE3(3,N)
        NML = IRBE3(5,N)
	IROT =IRBE3(6,N)
#include    "vectorize.inc"
        DO I=IAD+1,IAD+NML
         M = LRBE3(I)
	 IF (ITAG(M)==0) THEN
	  WEIGHT(I) = 1
	 ELSE
	  WEIGHT(I) = 0
	 END IF
	 ITAG(M)=1
        ENDDO
      ENDDO
C
      DO N=1,NRBE3
        IAD = IRBE3(1,N)
        NS  = IRBE3(3,N)
        NML = IRBE3(5,N)
	IROT =IRBE3(6,N)
#include    "vectorize.inc"
        DO I=IAD+1,IAD+NML
         M = LRBE3(I)
	 DA(1,I) = A(1,M)*WEIGHT(I) + DA(1,I)
	 DA(2,I) = A(2,M)*WEIGHT(I) + DA(2,I)
	 DA(3,I) = A(3,M)*WEIGHT(I) + DA(3,I)
	 DMS(I) = MS(M)*WEIGHT(I)+DMS(I)
	 DSTIFN(I) = STIFN(M)*WEIGHT(I)+DSTIFN(I)
        ENDDO
        IF (IROT>0) THEN
#include    "vectorize.inc"
         DO I=IAD+1,IAD+NML
          M = LRBE3(I)
	  DAR(1,I) = AR(1,M)*WEIGHT(I) + DAR(1,I)
	  DAR(2,I) = AR(2,M)*WEIGHT(I) + DAR(2,I)
	  DAR(3,I) = AR(3,M)*WEIGHT(I) + DAR(3,I)
	  DIN(I) = IN(M)*WEIGHT(I)+DIN(I)
	  DSTIFR(I) = STIFR(M)*WEIGHT(I)+DSTIFR(I)
         ENDDO
        ENDIF
      ENDDO
C---
      RETURN
      END
Chd|====================================================================
Chd|  ASP1_RBE3                     source/constraints/general/rbe3/rbe3f.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ASP1_RBE3(IRBE3 ,LRBE3 ,A     ,AR    ,MS    ,
     1                     IN    ,WEIGHT,STIFN ,STIFR ,DA    ,
     2                     DAR   ,DMS   ,DIN   ,DSTIFN,DSTIFR)
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE3(NRBE3L,*),LRBE3(*),WEIGHT(*)
C     REAL
      my_real
     .   A(3,*), AR(3,*), MS(*), IN(*),STIFN(*) ,STIFR(*)
      DOUBLE PRECISION
     .   DA(6,3,*), DAR(6,3,*), DMS(6,*),
     .   DIN(6,*),DSTIFN(6,*) ,DSTIFR(6,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, NS ,NML, IAD,JJ,IROT,M
C     REAL
C======================================================================|
      DO N=1,NRBE3
        IAD = IRBE3(1,N)
        NS  = IRBE3(3,N)
        NML = IRBE3(5,N)
	IROT =IRBE3(6,N)
#include    "vectorize.inc"
        DO I=IAD+1,IAD+NML
         M = LRBE3(I)
	  A(1,M) = ZERO
	  A(2,M) = ZERO
	  A(3,M) = ZERO
	  MS(M) =  ZERO
	  STIFN(M) = ZERO
        ENDDO
        IF (IROT>0) THEN
#include    "vectorize.inc"
         DO I=IAD+1,IAD+NML
          M = LRBE3(I)
	   AR(1,M) = ZERO
	   AR(2,M) = ZERO
	   AR(3,M) = ZERO
	   IN(M) =  ZERO
	   STIFR(M) = ZERO
         ENDDO
        ENDIF
      ENDDO
C
      DO N=1,NRBE3
        IAD = IRBE3(1,N)
        NS  = IRBE3(3,N)
        NML = IRBE3(5,N)
	IROT =IRBE3(6,N)
#include    "vectorize.inc"
        DO I=IAD+1,IAD+NML
         M = LRBE3(I)
         DO J=1,6
	  A(1,M) = A(1,M)+ DA(J,1,I)
	  A(2,M) = A(2,M)+ DA(J,2,I)
	  A(3,M) = A(3,M)+ DA(J,3,I)
	  MS(M) =  MS(M)+DMS(J,I)
	  STIFN(M) = STIFN(M)+DSTIFN(J,I)
         ENDDO
        ENDDO
        IF (IROT>0) THEN
#include    "vectorize.inc"
         DO I=IAD+1,IAD+NML
          M = LRBE3(I)
          DO J=1,6
	   AR(1,M) = AR(1,M)+ DAR(J,1,I)
	   AR(2,M) = AR(2,M)+ DAR(J,2,I)
	   AR(3,M) = AR(3,M)+ DAR(J,3,I)
	   IN(M) =  IN(M)+DIN(J,I)
	   STIFR(M) = STIFR(M)+DSTIFR(J,I)
          ENDDO
         ENDDO
        ENDIF
      ENDDO
C---
      RETURN
      END
Chd|====================================================================
Chd|  ASP2_RBE3                     source/constraints/general/rbe3/rbe3f.F
Chd|-- called by -----------
Chd|        RBE3T1                        source/constraints/general/rbe3/rbe3f.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ASP2_RBE3(IRBE3 ,LRBE3 ,A     ,AR    ,MS    ,
     1                     IN    ,WEIGHT,STIFN ,STIFR ,DA    ,
     2                     DAR   ,DMS   ,DIN   ,DSTIFN,DSTIFR,
     3                     NMT   ,IML   ,IROTG )
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE3(NRBE3L,*),LRBE3(*),WEIGHT(*),IML(*) ,NMT ,IROTG
C     REAL
      my_real
     .   A(3,*), AR(3,*), MS(*), IN(*),STIFN(*) ,STIFR(*)
      DOUBLE PRECISION
     .   DA(6,3,*), DAR(6,3,*), DMS(6,*),
     .   DIN(6,*),DSTIFN(6,*) ,DSTIFR(6,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, NS ,NML, IAD,JJ,M
C     REAL
      my_real
     .   MS1,AX,AY,AZ
C======================================================================|
#include    "vectorize.inc"
       DO I=1,NMT
         M = IML(I)
	 AX = ZERO
	 AY = ZERO
	 AZ = ZERO
         DO J=1,6
	  AX = AX + DA(J,1,I)
	  AY = AY + DA(J,2,I)
	  AZ = AZ + DA(J,3,I)
	  MS(M) =  MS(M)+DMS(J,I)
C	  MS1 = MS1 + DMS(J,I)
 	  STIFN(M) = STIFN(M)+DSTIFN(J,I)
         ENDDO
	  A(1,M) = A(1,M)+ AX
	  A(2,M) = A(2,M)+ AY
	  A(3,M) = A(3,M)+ AZ
       ENDDO
      IF (IROTG>0) THEN
       DO I=1,NMT
         M = IML(I)
	 AX = ZERO
	 AY = ZERO
	 AZ = ZERO
          DO J=1,6
	   AX = AX + DAR(J,1,I)
	   AY = AY + DAR(J,2,I)
	   AZ = AZ + DAR(J,3,I)
	   IN(M) =  IN(M)+DIN(J,I)
	   STIFR(M) = STIFR(M)+DSTIFR(J,I)
          ENDDO
	  AR(1,M) = AR(1,M)+ AX
	  AR(2,M) = AR(2,M)+ AY
	  AR(3,M) = AR(3,M)+ AZ
       ENDDO
      ENDIF
C---
      RETURN
      END
Chd|====================================================================
Chd|  DMI_RBE3                      source/constraints/general/rbe3/rbe3f.F
Chd|-- called by -----------
Chd|        RBE3T1                        source/constraints/general/rbe3/rbe3f.F
Chd|-- calls ---------------
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE DMI_RBE3(NMT   ,LRBE3 ,MS0   ,IN0   ,DMS   ,
     1                    DIN   ,DMAST ,ADM   ,DINERT,ADI   ,
     2                    IROTG ,IRBE3 ,MS    ,IN    ,WEIGHT,
     3                    IADMP ,H3D_DATA )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE H3D_MOD
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NMT,IRBE3(NRBE3L,*),LRBE3(*),IROTG,WEIGHT(*),IADMP(*)
C     REAL
      my_real
     .   MS0(*), IN0(*),DMS(*), DIN(*),
     .   DMAST ,ADM(*)   ,DINERT,ADI(*),MS(*),IN(*)
      TYPE (H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J ,M,N,NS,NML,IAD,K,IROT
C     REAL
      my_real
     .   MNS,DMAS0
C======================================================================|
C----accumulated secnd masses---
      MNS= ZERO
#include    "vectorize.inc"
      DO N=1,NRBE3
        IAD = IRBE3(1,N)
        NS  = IRBE3(3,N)
        NML = IRBE3(5,N)
        IF (NS>0) THEN
          IF (WEIGHT(NS)==1) THEN
            IF (NML>0) MNS= MNS+MS(NS)*WEIGHT(NS)
c         DO J = 1,NML
c	  K = IADMP(IAD+J)
c          DMS(K) = DMS(K) -MNS
c	 ENDDO
            MS(NS) = ZERO
	    IF (IRODDL/=0) IN(NS) = ZERO
          ENDIF
        ENDIF
      ENDDO
C
       DMAS0 = -MNS
       DO J=1,NMT
C         DMS(J)= MAX(ZERO,DMS(J))
        DMAS0 = DMAS0 + DMS(J)
       ENDDO
       DMAST = DMAST + MAX(ZERO,DMAS0)
C       
      IF(IROTG>0) THEN
       DO J=1,NMT
         DINERT = DINERT + DIN(J)
       ENDDO
      ENDIF
C-------attention now for ANIM, ADM contains MS(NS), 
C-------output only when real added mass hapened: debug usage 
      IF (DMAS0>EM10) THEN
      IF(ANIM_N(2)+OUTP_N(2)+H3D_DATA%N_SCAL_DMAS >0) THEN
#include    "vectorize.inc"
       DO N=1,NRBE3
        IAD = IRBE3(1,N)
        NS  = IRBE3(3,N)
        NML = IRBE3(5,N)
        IF (NS>0.AND.WEIGHT(NS)==1) THEN
         DO J = 1,NML
          M = LRBE3(IAD+J)
          K = IADMP(IAD+J)
          ADM(M) = ADM(M)+DMS(K)/MAX(EM20,MS0(IAD+J))
         ENDDO
        ENDIF
       ENDDO
      ENDIF
      IF(ANIM_N(12)+OUTP_N(3)>0+H3D_DATA%N_SCAL_DINER .AND.IROTG>0) THEN
#include    "vectorize.inc"
       DO N=1,NRBE3
        IAD = IRBE3(1,N)
        NS  = IRBE3(3,N)
        NML = IRBE3(5,N)
	IROT =IRBE3(6,N)
        IF (NS>0.AND.IROT>0.AND.WEIGHT(NS)==1) THEN
         DO J = 1,NML
          M = LRBE3(IAD+J)
          K = IADMP(IAD+J)
          ADI(M) = ADI(M)+DIN(K)/MAX(EM20,IN0(IAD+J))
         ENDDO
        ENDIF
       ENDDO
      ENDIF
      END IF !(DMAS0>EM10) THEN
C---
      RETURN
      END
Chd|====================================================================
Chd|  PRERBE3                       source/constraints/general/rbe3/rbe3f.F
Chd|-- called by -----------
Chd|        RBE3T1                        source/constraints/general/rbe3/rbe3f.F
Chd|        RBE3V                         source/constraints/general/rbe3/rbe3v.F
Chd|        RBE3_IMP0                     source/constraints/general/rbe3/rbe3_imp0.F
Chd|        RBE3_IMPD                     source/constraints/general/rbe3/rbe3v.F
Chd|        RBE3_IMPI                     source/constraints/general/rbe3/rbe3_imp0.F
Chd|        RBE3_IMPR1                    source/constraints/general/rbe3/rbe3_imp0.F
Chd|        RBE3_IMPR2                    source/constraints/general/rbe3/rbe3_imp0.F
Chd|        SMS_RBE3T1                    source/ams/sms_rbe3.F         
Chd|        SMS_RBE3T2                    source/ams/sms_rbe3.F         
Chd|        SMS_RBE3_NODXI                source/ams/sms_rbe3.F         
Chd|        SMS_RBE3_PREC                 source/ams/sms_rbe3.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PRERBE3(IRBE3 ,MAX_M , IROTG,JT  ,JR   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE3(NRBE3L,*),MAX_M , IROTG,JT(3,*)  ,JR(3,*)
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N,NML,IC,ICT,ICR,IROT
C======================================================================|
      MAX_M=0
      IROTG=0
      DO N=1,NRBE3
	    NML = IRBE3(5,N)
	    IROT =IRBE3(6,N)
        MAX_M=MAX(MAX_M,NML)
        IROTG=MAX(IROTG,IROT)
	    IC=IRBE3(4,N)
        ICT=IC/512
        ICR=(IC-512*(ICT))/64
	   DO J =1,3
           JT(J,N)=0
           JR(J,N)=0
	   ENDDO
        SELECT CASE (ICT)
          CASE(1)
           JT(3,N)=1
          CASE(2)
           JT(2,N)=1
          CASE(3)
           JT(2,N)=1
           JT(3,N)=1
          CASE(4)
           JT(1,N)=1
          CASE(5)
           JT(1,N)=1
           JT(3,N)=1
          CASE(6)
           JT(1,N)=1
           JT(2,N)=1
          CASE(7)
           JT(1,N)=1
           JT(2,N)=1
           JT(3,N)=1
       END SELECT
       SELECT CASE (ICR)
          CASE(1)
           JR(3,N)=1
          CASE(2)
           JR(2,N)=1
          CASE(3)
           JR(2,N)=1
           JR(3,N)=1
          CASE(4)
           JR(1,N)=1
          CASE(5)
           JR(1,N)=1
           JR(3,N)=1
          CASE(6)
           JR(1,N)=1
           JR(2,N)=1
          CASE(7)
           JR(1,N)=1
           JR(2,N)=1
           JR(3,N)=1
       END SELECT
      ENDDO
C      IF (IMACH==3.AND.MAXPROC>1) CALL SPMD_MAX_I(IROTG)
C---
      RETURN
      END
Chd|====================================================================
Chd|  PRERBE3FR                     source/constraints/general/rbe3/rbe3f.F
Chd|-- called by -----------
Chd|        DIAG_INT                      source/mpi/implicit/imp_fri.F 
Chd|        FR_U2DD                       source/mpi/implicit/imp_fri.F 
Chd|        IMP3_A2B                      source/airbag/monv_imp0.F     
Chd|        IMP3_U2X                      source/airbag/monv_imp0.F     
Chd|        UPDK_MV                       source/airbag/monv_imp0.F     
Chd|        UPD_FR                        source/mpi/implicit/imp_fri.F 
Chd|        UPD_KML                       source/mpi/implicit/imp_fri.F 
Chd|        UPD_KSL                       source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PRERBE3FR(IRBE3 ,N    ,JT  ,JR   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE3(NRBE3L,*),JT(3)  ,JR(3),N
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, NML,IC,ICT,ICR
C======================================================================|
	IC=IRBE3(4,N)
        ICT=IC/512
        ICR=(IC-512*(ICT))/64
	    DO J =1,3
           JT(J)=0
           JR(J)=0
	    ENDDO
        SELECT CASE (ICT)
          CASE(1)
           JT(3)=1
          CASE(2)
           JT(2)=1
          CASE(3)
           JT(2)=1
           JT(3)=1
          CASE(4)
           JT(1)=1
          CASE(5)
           JT(1)=1
           JT(3)=1
          CASE(6)
           JT(1)=1
           JT(2)=1
          CASE(7)
           JT(1)=1
           JT(2)=1
           JT(3)=1
       END SELECT
       SELECT CASE (ICR)
          CASE(1)
           JR(3)=1
          CASE(2)
           JR(2)=1
          CASE(3)
           JR(2)=1
           JR(3)=1
          CASE(4)
           JR(1)=1
          CASE(5)
           JR(1)=1
           JR(3)=1
          CASE(6)
           JR(1)=1
           JR(2)=1
          CASE(7)
           JR(1)=1
           JR(2)=1
           JR(3)=1
       END SELECT
C---
      RETURN
      END
Chd|====================================================================
Chd|  RBE3CL                        source/constraints/general/rbe3/rbe3f.F
Chd|-- called by -----------
Chd|        IDDL_INT                      source/mpi/implicit/imp_fri.F 
Chd|        ID_MVINI                      source/airbag/monv_imp0.F     
Chd|        RBE3F                         source/constraints/general/rbe3/rbe3f.F
Chd|        RBE3V                         source/constraints/general/rbe3/rbe3v.F
Chd|        RBE3_FR0                      source/constraints/general/rbe3/rbe3_imp0.F
Chd|        RBE3_IMP0                     source/constraints/general/rbe3/rbe3_imp0.F
Chd|        RBE3_IMPD                     source/constraints/general/rbe3/rbe3v.F
Chd|        RBE3_IMPI                     source/constraints/general/rbe3/rbe3_imp0.F
Chd|        RBE3_IMPR1                    source/constraints/general/rbe3/rbe3_imp0.F
Chd|        RBE3_IMPR2                    source/constraints/general/rbe3/rbe3_imp0.F
Chd|        RBE3_MINT                     source/implicit/imp_int_k.F   
Chd|        SMS_RBE3T2                    source/ams/sms_rbe3.F         
Chd|        SMS_RBE3_1                    source/ams/sms_rbe3.F         
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        INVERT                        source/constraints/general/rbe3/rbe3f.F
Chd|        RBE3UF                        source/constraints/general/rbe3/rbe3f.F
Chd|        RBE3UM                        source/constraints/general/rbe3/rbe3f.F
Chd|        ZERO1                         source/system/zero.F          
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE RBE3CL(INRBE3  ,ILRBE3  ,NS     ,XYZ    ,FRBE3   ,
     .                  SKEW    ,NG      ,IROT   ,FDSTNB ,MDSTNB  ,ID )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "impl1_c.inc"
#include      "param_c.inc"
#include      "scr07_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER INRBE3(*),ILRBE3(*),NG, NS,IROT,ID
C     REAL
      my_real
     .   XYZ(3,*), FRBE3(6,*), SKEW(LSKEW,*),FDSTNB(3,6,*), MDSTNB(3,6,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K,N, M ,NML, IAD,JJ,KG,NSNGLR,IELSUB,IERR,ng1
C     REAL
      my_real
     *        TW(3,NG), RW(3,NG),
     *        FUFXLC(3,NG), FUFYLC(3,NG), FUFZLC(3,NG),
     *        FUMXLC(3,NG), FUMYLC(3,NG), FUMZLC(3,NG),
     *        MXLC(3,NG), MYLC(3,NG), MZLC(3,NG),
     *        FUFX(3,NG), FUFY(3,NG), FUFZ(3,NG),
     *        MUFX(3,NG), MUFY(3,NG), MUFZ(3,NG),
     *        FUMX(3,NG), FUMY(3,NG), FUMZ(3,NG),
     *        MX(3,NG), MY(3,NG), MZ(3,NG),
     *        MUMX(3,NG), MUMY(3,NG), MUMZ(3,NG),
     *        EL(3,3,NG)
      my_real
     *                 DENFX, DENFY, DENFZ, DENMX, DENMY, DENMZ,
     *                 REFPT(3), CGMX(3), CGMY(3), CGMZ(3), AVEREF,
     *                 TFUFX(3), TFUFY(3), TFUFZ(3),
     *                 TMUFX(3), TMUFY(3), TMUFZ(3),
     *                 TFUMX(3), TFUMY(3), TFUMZ(3),
     *                 TMUMX(3), TMUMY(3), TMUMZ(3),
     *                 A(6,6), C(6,6), T(3,3)
C
C     INITIALIZATION
C
      IF (NG==0) RETURN
      CALL ZERO1(FDSTNB,3*NG*6)
      IF (IROT>0) CALL ZERO1(MDSTNB,3*NG*6)
      CALL ZERO1(A,36)
      CALL ZERO1(C,36)
      CALL ZERO1(CGMX,3)
      CALL ZERO1(CGMY,3)
      CALL ZERO1(CGMZ,3)
      IERR = 0
C
      REFPT(1) = XYZ(1,NS)
      REFPT(2) = XYZ(2,NS)
      REFPT(3) = XYZ(3,NS)
      DO K = 1, NG
            DO I = 1, 3
               TW(I,K) = FRBE3(I,K)
               RW(I,K) = FRBE3(I+3,K)
            ENDDO
      ENDDO
C
C     ERROR OUT IF RBE3 ELEMENT HAS TWO INDEPENDENT NODES WITH
C     NO ROTATIONAL WEIGHTS SET (THIS MEANS THE ELEMENT CANNOT
C     SUPPORT A MOMENT ALONG ITS AXIS)
C
      IF (NG == 2.AND.IROT==0) THEN
            IERR = 322
            GOTO 999
      ENDIF
C
C     CALCULATE DIRECTION COSINES OF LOCAL COORDINATE SYSTEMS, IF ANY
C
        DO K = 1, NG
            IELSUB = ILRBE3(K)
            IF (IELSUB > 0) THEN
               DO I = 1, 3
                     EL(I,1,K) = SKEW(I,IELSUB)
                     EL(I,2,K) = SKEW(I+3,IELSUB)
                     EL(I,3,K) = SKEW(I+6,IELSUB)
               ENDDO
            ENDIF
        ENDDO
C
C     DENOMINATORS FOR DISTRIBUTING FORCES (DENFX, DENFY AND DENFZ)
C
      DENFX = ZERO
      DENFY = ZERO
      DENFZ = ZERO
      AVEREF = ZERO
C
      DO 70 K = 1, NG
         KG = INRBE3(K)
         IELSUB = ILRBE3(K)
         IF (IELSUB > 0) THEN
C
C           IF GRID POINT HAS A LOCAL COORDINATE SYSTEM
C
            DO 60 I = 1, 3
               DENFX = DENFX + TW(I,K)*EL(I,1,K)**2
               DENFY = DENFY + TW(I,K)*EL(I,2,K)**2
               DENFZ = DENFZ + TW(I,K)*EL(I,3,K)**2
 60         CONTINUE
         ELSE
            DENFX = DENFX + TW(1,K)
            DENFY = DENFY + TW(2,K)
            DENFZ = DENFZ + TW(3,K)
         END IF
C
            AVEREF = AVEREF + SQRT( (XYZ(1,KG) - REFPT(1))**2 +
     *                              (XYZ(2,KG) - REFPT(2))**2 +
     *                              (XYZ(3,KG) - REFPT(3))**2 )
 70   CONTINUE
C
      IF (ABS(DENFX) <= EM20) THEN
         IERR = 326
      ENDIF
C
      IF (ABS(DENFY) <= EM20) THEN
         IERR = 327
      ENDIF
C
      IF (ABS(DENFZ) <= EM20) THEN
         IERR = 328
      ENDIF
      IF (IERR /= 0) GOTO 999
         AVEREF = AVEREF/NG
         IF (AVEREF == ZERO) AVEREF = ONE
C
C     CALCULATE 3 CENTERS OF GRAVITY (CGMX, CGMY AND CGMZ) AND
C     DENOMINATORS FOR DISTRIBUTING MOMENTS (DENMX, DENMY AND DENMZ)
C
      DO 40 K = 1, NG
         KG = INRBE3(K)
         IELSUB = ILRBE3(K)
         IF (IELSUB > 0) THEN
C
C           IF THERE IS A LOCAL COORDINATE SYSTEM AT THE GRID POINT
C
            DO 10 I = 1, 3
               CGMX(2) = CGMX(2) + TW(I,K)*EL(I,3,K)**2*XYZ(2,KG)
               CGMX(3) = CGMX(3) + TW(I,K)*EL(I,2,K)**2*XYZ(3,KG)
 10         CONTINUE
C
            DO 20 I = 1, 3
               CGMY(3) = CGMY(3) + TW(I,K)*EL(I,1,K)**2*XYZ(3,KG)
               CGMY(1) = CGMY(1) + TW(I,K)*EL(I,3,K)**2*XYZ(1,KG)
 20         CONTINUE
C
            DO 30 I = 1, 3
               CGMZ(1) = CGMZ(1) + TW(I,K)*EL(I,2,K)**2*XYZ(1,KG)
               CGMZ(2) = CGMZ(2) + TW(I,K)*EL(I,1,K)**2*XYZ(2,KG)
 30         CONTINUE
C
         ELSE
            CGMX(2) = CGMX(2) + TW(3,K)*XYZ(2,KG)
            CGMX(3) = CGMX(3) + TW(2,K)*XYZ(3,KG)
C
            CGMY(3) = CGMY(3) + TW(1,K)*XYZ(3,KG)
            CGMY(1) = CGMY(1) + TW(3,K)*XYZ(1,KG)
C
            CGMZ(1) = CGMZ(1) + TW(2,K)*XYZ(1,KG)
            CGMZ(2) = CGMZ(2) + TW(1,K)*XYZ(2,KG)
         END IF
 40   CONTINUE
         CGMX(2) = CGMX(2)/DENFZ
         CGMX(3) = CGMX(3)/DENFY
C
         CGMY(3) = CGMY(3)/DENFX
         CGMY(1) = CGMY(1)/DENFZ
C
         CGMZ(1) = CGMZ(1)/DENFY
         CGMZ(2) = CGMZ(2)/DENFX
C
      DENMX = ZERO
      DENMY = ZERO
      DENMZ = ZERO
C
      DO 90 K = 1, NG
         KG = INRBE3(K)
         IELSUB = ILRBE3(K)
C
C        NOTE: AS IMPLEMENTED IN NASTRAN 70.7, WE SCALE THE ROTATIONAL
C              WEIGHTS WITH THE SQUARE OF THE AVERAGE DISTANCE OF THE
C              INDEPENDENT GRID POINTS FROM THE REFERENCE POINT TO
C              RENDER THE RBE3 CALCULATIONS UNIT INDEPENDENT
C
         IF (IELSUB > 0) THEN
C
C           IF GRID POINT HAS A LOCAL COORDINATE SYSTEM
C
            DO 80 I = 1, 3
               DENMX = DENMX + RW(I,K)*EL(I,1,K)**2*AVEREF**2 +
     *                 TW(I,K)*( EL(I,3,K)*(XYZ(2,KG) - CGMX(2)) -
     *                           EL(I,2,K)*(XYZ(3,KG) - CGMX(3))
     *                         ) **2
               DENMY = DENMY + RW(I,K)*EL(I,2,K)**2*AVEREF**2 +
     *                 TW(I,K)*( EL(I,1,K)*(XYZ(3,KG) - CGMY(3)) -
     *                           EL(I,3,K)*(XYZ(1,KG) - CGMY(1))
     *                         ) **2
               DENMZ = DENMZ + RW(I,K)*EL(I,3,K)**2*AVEREF**2 +
     *                 TW(I,K)*( EL(I,2,K)*(XYZ(1,KG) - CGMZ(1)) -
     *                           EL(I,1,K)*(XYZ(2,KG) - CGMZ(2))
     *                         ) **2
 80         CONTINUE
         ELSE
            DENMX = DENMX + RW(1,K)*AVEREF**2 +
     *                      TW(2,K)*(XYZ(3,KG) - CGMX(3))**2 +
     *                      TW(3,K)*(XYZ(2,KG) - CGMX(2))**2
            DENMY = DENMY + RW(2,K)*AVEREF**2 +
     *                      TW(1,K)*(XYZ(3,KG) - CGMY(3))**2 +
     *                      TW(3,K)*(XYZ(1,KG) - CGMY(1))**2
            DENMZ = DENMZ + RW(3,K)*AVEREF**2 +
     *                      TW(2,K)*(XYZ(1,KG) - CGMZ(1))**2 +
     *                      TW(1,K)*(XYZ(2,KG) - CGMZ(2))**2
         END IF
 90   CONTINUE
C
C     PERFORM SOME CHECKS ON WEIGHTS, TO MAKE SURE THAT THE RBE3
C     ELEMENT HAS NO UNCONSTRAINED DEGREES OF FREEDOM
C
C
      IF (ABS(DENMX) <= EM20) THEN
         IERR = 329
      ENDIF
C
      IF (ABS(DENMY) <= EM20) THEN
         IERR = 330
      ENDIF
C
      IF (ABS(DENMZ) <= EM20) THEN
         IERR = 331
      ENDIF
C
      IF (IERR /= 0) GOTO 999
C
C     CALCULATE 3 FORCE DISTRIBUTIONS THAT CREATE NET X, Y AND Z FORCES
C     OF 1 (BESIDES OTHER NONZERO FORCES/MOMENTS IN ALL THE DIRECTIONS)
C
      CALL RBE3UF(INRBE3,ILRBE3,EL,TW,XYZ,REFPT,
     *            FUFXLC,FUFYLC,FUFZLC,FUFX,FUFY,FUFZ,MUFX,MUFY,MUFZ,
     *            TFUFX,TFUFY,TFUFZ,TMUFX,TMUFY,TMUFZ,
     *            DENFX,DENFY,DENFZ,NG)
C
C     CALCULATE 3 MOMENT/FORCE DISTRIBUTIONS THAT CREATE NET X, Y AND Z
C     MOMENTS OF 1 (BESIDES OTHER NONZERO FORCES/MOMENTS IN ALL THE
C     DIRECTIONS) AT CGMX, CGMY AND CGMZ RESPECTIVELY
C
      CALL RBE3UM(INRBE3,ILRBE3,EL,TW,RW,XYZ,REFPT,CGMX,CGMY,CGMZ,
     *            FUMXLC,FUMYLC,FUMZLC,MXLC,MYLC,MZLC,
     *            FUMX,FUMY,FUMZ,MX,MY,MZ,MUMX,MUMY,MUMZ,
     *            TFUMX,TFUMY,TFUMZ,TMUMX,TMUMY,TMUMZ,
     *            AVEREF,DENMX,DENMY,DENMZ,NG,IROT )
C
C     DETERMINE COMBINATORY COEFFICIENTS FOR THESE 6 DISTRIBUTIONS
C     (6 COEFFICIENTS FOR EACH OF 6 CASES)
C
C     CASE 1 - RESULTANT OF THE LINEAR COMBINATION OF THESE FORCE/MOMENT
C              DISTRIBUTIONS IS A UNIT X-FORCE AT REFERENCE POINT
C     CASE 2 - RESULTANT OF THE LINEAR COMBINATION OF THESE FORCE/MOMENT
C              DISTRIBUTIONS IS A UNIT Y-FORCE AT REFERENCE POINT
C     CASE 3 - RESULTANT OF THE LINEAR COMBINATION OF THESE FORCE/MOMENT
C              DISTRIBUTIONS IS A UNIT Z-FORCE AT REFERENCE POINT
C     CASE 4 - RESULTANT OF THE LINEAR COMBINATION OF THESE FORCE/MOMENT
C              DISTRIBUTIONS IS A UNIT X-MOMENT AT REFERENCE POINT
C     CASE 5 - RESULTANT OF THE LINEAR COMBINATION OF THESE FORCE/MOMENT
C              DISTRIBUTIONS IS A UNIT Y-MOMENT AT REFERENCE POINT
C     CASE 6 - RESULTANT OF THE LINEAR COMBINATION OF THESE FORCE/MOMENT
C              DISTRIBUTIONS IS A UNIT Z-MOMENT AT REFERENCE POINT
C
C     IN ORDER TO DETERMINE THESE COEFFICIENTS, FIRST SET UP A 6X6
C     MATRIX.  THE 6 COLUMNS OF THE INVERSE OF THIS MATRIX ARE THE
C     DESIRED 6 SETS OF COEFFICIENTS.
C
      DO 120 I = 1, 3
         K = I + 3
         A(I,1) = TFUFX(I)
         A(K,1) = TMUFX(I)
         A(I,2) = TFUFY(I)
         A(K,2) = TMUFY(I)
         A(I,3) = TFUFZ(I)
         A(K,3) = TMUFZ(I)
         A(I,4) = TFUMX(I)
         A(K,4) = TMUMX(I)
         A(I,5) = TFUMY(I)
         A(K,5) = TMUMY(I)
         A(I,6) = TFUMZ(I)
         A(K,6) = TMUMZ(I)
 120  CONTINUE
C
C     INVERT THE 6X6 MATRIX
C
      NSNGLR  = 0
      CALL INVERT(A,C,6,NSNGLR)
      IF (NSNGLR /= 0) THEN
        IERR = 332
         GOTO 999
      ENDIF
C
      DO I = 1, 3
       DO J = 1, 6
         DO K = 1, NG
               FDSTNB(I,J,K) = C(1,J)*FUFX(I,K) + C(2,J)*FUFY(I,K) +
     *                         C(3,J)*FUFZ(I,K) + C(4,J)*FUMX(I,K) +
     *                         C(5,J)*FUMY(I,K) + C(6,J)*FUMZ(I,K)
         ENDDO
       ENDDO
      ENDDO
      IF (IROT>0) THEN
       DO I = 1, 3
        DO J = 1, 6
         DO K = 1, NG
               MDSTNB(I,J,K) = C(4,J)*MX(I,K) + C(5,J)*MY(I,K) +
     *                         C(6,J)*MZ(I,K)
         ENDDO
        ENDDO
       ENDDO
      END IF
C
 999  CONTINUE
      IF (IERR>0) THEN
           IF(ISPMD==0)THEN
             CALL ANCMSG(MSGID=108,ANMODE=ANINFO,
     .            I1=ID)
           ENDIF
           MSTOP = 1
      ENDIF
C
C     DIAGNOSTIC INFORMATION
C
      RETURN
      END
C----------------------------
Chd|====================================================================
Chd|  WRRINF                        source/constraints/general/rbe3/rbe3f.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE WRRINF(TITLE,R,N)
#include      "implicit_f.inc"
c	!DECLARATIONS
      INTEGER N
      my_real
     .        R(N)
      CHARACTER TITLE*(*)
C----------------------------
      INTEGER I
      print *, TITLE,(R(I),I=1,N)
      RETURN
      END
Chd|====================================================================
Chd|  RBE3UF                        source/constraints/general/rbe3/rbe3f.F
Chd|-- called by -----------
Chd|        RBE3CL                        source/constraints/general/rbe3/rbe3f.F
Chd|-- calls ---------------
Chd|        ZERO1                         source/system/zero.F          
Chd|====================================================================
      SUBROUTINE RBE3UF(INRBE3,ILRBE3,EL,TW,XYZ,REFPT,
     *                  FUFXLC,FUFYLC,FUFZLC,
     *                  FUFX,FUFY,FUFZ,MUFX,MUFY,MUFZ,
     *                  TFUFX,TFUFY,TFUFZ,TMUFX,TMUFY,TMUFZ,
     *                  DENFX,DENFY,DENFZ,NG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      INTEGER NG
      INTEGER INRBE3(NG), ILRBE3(NG)
      my_real
     *                 EL(3,3,*),TW(3,NG), XYZ(3,*), REFPT(3),
     *                 FUFXLC(3,NG), FUFYLC(3,NG), FUFZLC(3,NG),
     *                 FUFX(3,NG), FUFY(3,NG), FUFZ(3,NG),
     *                 MUFX(3,NG), MUFY(3,NG), MUFZ(3,NG),
     *                 TFUFX(3), TFUFY(3), TFUFZ(3),
     *                 TMUFX(3), TMUFY(3), TMUFZ(3)
      my_real
     *    DENFX, DENFY, DENFZ,XARM, YARM, ZARM
      INTEGER I, J, K, KG, IELSUB
C
C     INITIALIZE FORCE AND MOMENT DISTRIBUTIONS TO ZERO
C
      CALL ZERO1(FUFX,3*NG)
      CALL ZERO1(FUFY,3*NG)
      CALL ZERO1(FUFZ,3*NG)
      CALL ZERO1(TFUFX,3)
      CALL ZERO1(TFUFY,3)
      CALL ZERO1(TFUFZ,3)
      CALL ZERO1(TMUFX,3)
      CALL ZERO1(TMUFY,3)
      CALL ZERO1(TMUFZ,3)
C
C     FORCE DISTRIBUTIONS AT RBE3 GRID POINTS CORRESPONDING TO UNIT
C     APPLIED FORCES AT RBE3 REFERENCE POINT ALONG (BASIC COORDINATE)
C     X, Y AND Z DIRECTIONS
C
      DO 50 K = 1, NG
         KG = INRBE3(K)
         IELSUB = ILRBE3(K)
         IF (IELSUB > 0) THEN
C
C           FORCES AT GRID POINT ALONG GRID POINT'S LOCAL (OUTPUT)
C           COORDINATE AXES
C
            DO 10 I = 1, 3
               FUFXLC(I,K) = TW(I,K)*EL(I,1,K)/DENFX
               FUFYLC(I,K) = TW(I,K)*EL(I,2,K)/DENFY
               FUFZLC(I,K) = TW(I,K)*EL(I,3,K)/DENFZ
 10         CONTINUE
C
C           FORCES AT GRID POINT ALONG BASIC COORDINATE AXES
C
            DO 30 I = 1, 3
               DO 20 J = 1, 3
                  FUFX(J,K) = FUFX(J,K) + FUFXLC(I,K)*EL(I,J,K)
                  FUFY(J,K) = FUFY(J,K) + FUFYLC(I,K)*EL(I,J,K)
                  FUFZ(J,K) = FUFZ(J,K) + FUFZLC(I,K)*EL(I,J,K)
 20            CONTINUE
 30         CONTINUE
C
         ELSE
            FUFXLC(1,K) = TW(1,K)/DENFX
            FUFYLC(2,K) = TW(2,K)/DENFY
            FUFZLC(3,K) = TW(3,K)/DENFZ
            FUFX(1,K) = FUFXLC(1,K)
            FUFY(2,K) = FUFYLC(2,K)
            FUFZ(3,K) = FUFZLC(3,K)
         ENDIF
C
C        MOMENTS AT REFERENCE POINT DUE TO THESE FORCE DISTRIBUTIONS
C
         XARM = XYZ(1,KG) - REFPT(1)
         YARM = XYZ(2,KG) - REFPT(2)
         ZARM = XYZ(3,KG) - REFPT(3)
C
C        MOMENTS AT REFERENCE POINT DUE TO FUFX
C
         MUFX(1,K) = YARM*FUFX(3,K) - ZARM*FUFX(2,K)
         MUFX(2,K) = ZARM*FUFX(1,K) - XARM*FUFX(3,K)
         MUFX(3,K) = XARM*FUFX(2,K) - YARM*FUFX(1,K)
C
C        MOMENTS AT REFERENCE POINT DUE TO FUFY
C
         MUFY(1,K) = YARM*FUFY(3,K) - ZARM*FUFY(2,K)
         MUFY(2,K) = ZARM*FUFY(1,K) - XARM*FUFY(3,K)
         MUFY(3,K) = XARM*FUFY(2,K) - YARM*FUFY(1,K)
C
C        MOMENTS AT REFERENCE POINT DUE TO FUFZ
C
         MUFZ(1,K) = YARM*FUFZ(3,K) - ZARM*FUFZ(2,K)
         MUFZ(2,K) = ZARM*FUFZ(1,K) - XARM*FUFZ(3,K)
         MUFZ(3,K) = XARM*FUFZ(2,K) - YARM*FUFZ(1,K)
C
C        TOTAL FORCES AND MOMENTS
C
         DO 40 J = 1, 3
            TFUFX(J) = TFUFX(J) + FUFX(J,K)
            TFUFY(J) = TFUFY(J) + FUFY(J,K)
            TFUFZ(J) = TFUFZ(J) + FUFZ(J,K)
            TMUFX(J) = TMUFX(J) + MUFX(J,K)
            TMUFY(J) = TMUFY(J) + MUFY(J,K)
            TMUFZ(J) = TMUFZ(J) + MUFZ(J,K)
 40      CONTINUE
C
 50   CONTINUE
C
      RETURN
      END
C
Chd|====================================================================
Chd|  RBE3UM                        source/constraints/general/rbe3/rbe3f.F
Chd|-- called by -----------
Chd|        RBE3CL                        source/constraints/general/rbe3/rbe3f.F
Chd|-- calls ---------------
Chd|        ZERO1                         source/system/zero.F          
Chd|====================================================================
      SUBROUTINE RBE3UM(INRBE3,ILRBE3,EL,TW,RW,XYZ,REFPT,CGMX,CGMY,CGMZ,
     *                  FUMXLC,FUMYLC,FUMZLC,MXLC,MYLC,MZLC,
     *                  FUMX,FUMY,FUMZ,MX,MY,MZ,MUMX,MUMY,MUMZ,
     *                  TFUMX,TFUMY,TFUMZ,TMUMX,TMUMY,TMUMZ,
     *                  AVEREF,DENMX,DENMY,DENMZ,NG ,IROT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      INTEGER NG,IROT
      INTEGER INRBE3(NG), ILRBE3(NG)
      my_real
     *                 EL(3,3,*),TW(3,NG), RW(3,NG), XYZ(3,*),
     *                 REFPT(3), CGMX(3), CGMY(3), CGMZ(3),
     *                 FUMXLC(3,NG), FUMYLC(3,NG), FUMZLC(3,NG),
     *                 MXLC(3,NG), MYLC(3,NG), MZLC(3,NG),
     *                 FUMX(3,NG), FUMY(3,NG), FUMZ(3,NG),
     *                 MX(3,NG), MY(3,NG), MZ(3,NG),
     *                 MUMX(3,NG), MUMY(3,NG), MUMZ(3,NG),
     *                 TFUMX(3), TFUMY(3), TFUMZ(3),
     *                 TMUMX(3), TMUMY(3), TMUMZ(3)
      my_real
     *         AVEREF, DENMX, DENMY, DENMZ,XARM, YARM, ZARM
      INTEGER I, J, K, KG, IELSUB
C
C     INITIALIZE FORCE AND MOMENT DISTRIBUTIONS TO ZERO
C
      CALL ZERO1(FUMX,3*NG)
      CALL ZERO1(FUMY,3*NG)
      CALL ZERO1(FUMZ,3*NG)
      CALL ZERO1(MX,3*NG)
      CALL ZERO1(MY,3*NG)
      CALL ZERO1(MZ,3*NG)
      CALL ZERO1(TFUMX,3)
      CALL ZERO1(TFUMY,3)
      CALL ZERO1(TFUMZ,3)
      CALL ZERO1(TMUMX,3)
      CALL ZERO1(TMUMY,3)
      CALL ZERO1(TMUMZ,3)
C
C     FORCE AND MOMENT DISTRIBUTIONS AT RBE3 GRID POINTS CORRESPONDING
C     TO UNIT APPLIED MOMENTS AT RBE3 REFERENCE POINT ALONG (BASIC
C     COORDINATE) X, Y AND Z DIRECTIONS
C
      DO 50 K = 1, NG
         KG = INRBE3(K)
         IELSUB = ILRBE3(K)
         IF (IELSUB > 0) THEN
C
C           FORCES  AT GRID POINT ALONG GRID POINT'S LOCAL
C           (OUTPUT) COORDINATE AXES
C
            DO 10 I = 1, 3
               FUMXLC(I,K) = TW(I,K)*
     *                       ( EL(I,3,K)*(XYZ(2,KG) - CGMX(2)) -
     *                         EL(I,2,K)*(XYZ(3,KG) - CGMX(3))
     *                       )/DENMX
               FUMYLC(I,K) = TW(I,K)*
     *                       ( EL(I,1,K)*(XYZ(3,KG) - CGMY(3)) -
     *                         EL(I,3,K)*(XYZ(1,KG) - CGMY(1))
     *                       )/DENMY
               FUMZLC(I,K) = TW(I,K)*
     *                       ( EL(I,2,K)*(XYZ(1,KG) - CGMZ(1)) -
     *                         EL(I,1,K)*(XYZ(2,KG) - CGMZ(2))
     *                       )/DENMZ
 10         CONTINUE
C
C           FORCES AND MOMENTS AT GRID POINT ALONG BASIC COORDINATE AXES
C
            DO 30 I = 1, 3
               DO 20 J = 1, 3
                  FUMX(J,K) = FUMX(J,K) + FUMXLC(I,K)*EL(I,J,K)
                  FUMY(J,K) = FUMY(J,K) + FUMYLC(I,K)*EL(I,J,K)
                  FUMZ(J,K) = FUMZ(J,K) + FUMZLC(I,K)*EL(I,J,K)
 20            CONTINUE
 30         CONTINUE
C
         ELSE
            FUMXLC(2,K) = -TW(2,K)*(XYZ(3,KG) - CGMX(3))/DENMX
            FUMXLC(3,K) = TW(3,K)*(XYZ(2,KG) - CGMX(2))/DENMX
            FUMYLC(1,K) = TW(1,K)*(XYZ(3,KG) - CGMY(3))/DENMY
            FUMYLC(3,K) = -TW(3,K)*(XYZ(1,KG) - CGMY(1))/DENMY
            FUMZLC(1,K) = -TW(1,K)*(XYZ(2,KG) - CGMZ(2))/DENMZ
            FUMZLC(2,K) = TW(2,K)*(XYZ(1,KG) - CGMZ(1))/DENMZ
C
            FUMX(2,K) = FUMXLC(2,K)
            FUMX(3,K) = FUMXLC(3,K)
            FUMY(1,K) = FUMYLC(1,K)
            FUMY(3,K) = FUMYLC(3,K)
            FUMZ(1,K) = FUMZLC(1,K)
            FUMZ(2,K) = FUMZLC(2,K)
         ENDIF
C
C        MOMENTS AT REFERENCE POINT DUE TO FUMX
C
         XARM = XYZ(1,KG) - REFPT(1)
         YARM = XYZ(2,KG) - REFPT(2)
         ZARM = XYZ(3,KG) - REFPT(3)
C
         MUMX(1,K) = YARM*FUMX(3,K) - ZARM*FUMX(2,K)
         MUMX(2,K) = ZARM*FUMX(1,K) - XARM*FUMX(3,K)
         MUMX(3,K) = XARM*FUMX(2,K) - YARM*FUMX(1,K)
C
C        MOMENTS AT REFERENCE POINT DUE TO FUMY
C
         MUMY(1,K) = YARM*FUMY(3,K) - ZARM*FUMY(2,K)
         MUMY(2,K) = ZARM*FUMY(1,K) - XARM*FUMY(3,K)
         MUMY(3,K) = XARM*FUMY(2,K) - YARM*FUMY(1,K)
C
C        MOMENTS AT REFERENCE POINT DUE TO FUMZ
C
         MUMZ(1,K) = YARM*FUMZ(3,K) - ZARM*FUMZ(2,K)
         MUMZ(2,K) = ZARM*FUMZ(1,K) - XARM*FUMZ(3,K)
         MUMZ(3,K) = XARM*FUMZ(2,K) - YARM*FUMZ(1,K)
C
 50   CONTINUE
C
      IF (IROT>0) THEN
       DO K = 1, NG
         KG = INRBE3(K)
         IELSUB = ILRBE3(K)
         IF (IELSUB > 0) THEN
C
C           MOMENTS AT GRID POINT ALONG GRID POINT'S LOCAL
C           (OUTPUT) COORDINATE AXES
C
            DO I = 1, 3
               MXLC(I,K) = AVEREF**2*RW(I,K)*EL(I,1,K)/DENMX
               MYLC(I,K) = AVEREF**2*RW(I,K)*EL(I,2,K)/DENMY
               MZLC(I,K) = AVEREF**2*RW(I,K)*EL(I,3,K)/DENMZ
            END DO
C
C           MOMENTS AT GRID POINT ALONG BASIC COORDINATE AXES
C
            DO I = 1, 3
               DO J = 1, 3
                  MX(J,K) = MX(J,K) + MXLC(I,K)*EL(I,J,K)
                  MY(J,K) = MY(J,K) + MYLC(I,K)*EL(I,J,K)
                  MZ(J,K) = MZ(J,K) + MZLC(I,K)*EL(I,J,K)
               END DO
            END DO
C
         ELSE
            MXLC(1,K) = AVEREF**2*RW(1,K)/DENMX
            MYLC(2,K) = AVEREF**2*RW(2,K)/DENMY
            MZLC(3,K) = AVEREF**2*RW(3,K)/DENMZ
C
            MX(1,K) = MXLC(1,K)
            MY(2,K) = MYLC(2,K)
            MZ(3,K) = MZLC(3,K)
         ENDIF
C
         DO J = 1, 3
          MUMX(J,K) = MUMX(J,K) + MX(J,K)
          MUMY(J,K) = MUMY(J,K) + MY(J,K)
          MUMZ(J,K) = MUMZ(J,K) + MZ(J,K)
         END DO
       END DO
      END IF
C
C
C        TOTAL FORCES AND MOMENTS
C
C
      DO K = 1, NG
         DO J = 1, 3
            TFUMX(J) = TFUMX(J) + FUMX(J,K)
            TFUMY(J) = TFUMY(J) + FUMY(J,K)
            TFUMZ(J) = TFUMZ(J) + FUMZ(J,K)
            TMUMX(J) = TMUMX(J) + MUMX(J,K)
            TMUMY(J) = TMUMY(J) + MUMY(J,K)
            TMUMZ(J) = TMUMZ(J) + MUMZ(J,K)
         END DO
      END DO
C
      RETURN
      END
Chd|====================================================================
Chd|  INVERT                        source/constraints/general/rbe3/rbe3f.F
Chd|-- called by -----------
Chd|        RBE3CL                        source/constraints/general/rbe3/rbe3f.F
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE INVERT(MATRIX, INVERSE, N, ERRORFLAG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
c	!DECLARATIONS
	INTEGER, INTENT(IN) :: N
	INTEGER, INTENT(OUT) :: ERRORFLAG  !RETURN ERROR STATUS. -1 FOR ERROR, 0 FOR NORMAL
      my_real
     *     , INTENT(IN), DIMENSION(N,N) :: MATRIX  !INPUT MATRIX
      my_real
     *     , INTENT(OUT), DIMENSION(N,N) :: INVERSE !INVERTED MATRIX

	LOGICAL :: FLAG = .TRUE.
	INTEGER :: I, J, K, L
      my_real
     *     :: M
      my_real
     *       , DIMENSION(N,2*N) :: AUGMATRIX !AUGMENTED MATRIX

c	!AUGMENT INPUT MATRIX WITH AN IDENTITY MATRIX
	DO I = 1, N
		DO J = 1, 2*N
			IF (J <= N ) THEN
				AUGMATRIX(I,J) = MATRIX(I,J)
			ELSE IF ((I+N) == J) THEN
				AUGMATRIX(I,J) = ONE
			ELSE
				AUGMATRIX(I,J) = ZERO
			ENDIF
		END DO
	END DO

c	!REDUCE AUGMENTED MATRIX TO UPPER TRAINGULAR FORM
	DO K =1, N-1
		IF (AUGMATRIX(K,K) == 0) THEN
		 FLAG = .FALSE.
		 DO I = K+1, N
		  IF (AUGMATRIX(I,K) /= 0) THEN
		    DO J = 1,2*N
			AUGMATRIX(K,J) = AUGMATRIX(K,J)+AUGMATRIX(I,J)
		    END DO
		    FLAG = .TRUE.
		    EXIT
		  ENDIF
		  IF (FLAG .EQV. .FALSE.) THEN
			INVERSE = 0
			ERRORFLAG = -1
			RETURN
		  ENDIF
		 END DO
		ENDIF
		DO J = K+1, N
		 M = AUGMATRIX(J,K)/AUGMATRIX(K,K)
		 DO I = K, 2*N
		  AUGMATRIX(J,I) = AUGMATRIX(J,I) - M*AUGMATRIX(K,I)
		 END DO
		END DO
	END DO

c	!TEST FOR INVERTIBILITY
	DO I = 1, N
		IF (AUGMATRIX(I,I) == 0) THEN
c			PRINT*, "MATRIX IS NON - INVERTIBLE"
			INVERSE = 0
			ERRORFLAG = -1
			RETURN
		ENDIF
	END DO

c	!MAKE DIAGONAL ELEMENTS AS 1
	DO I = 1 , N
		M = AUGMATRIX(I,I)
		DO J = I , (2 * N)
			AUGMATRIX(I,J) = (AUGMATRIX(I,J) / M)
		END DO
	END DO

c	!REDUCED RIGHT SIDE HALF OF AUGMENTED MATRIX TO IDENTITY MATRIX
	DO K = N-1, 1, -1
		DO I =1, K
		M = AUGMATRIX(I,K+1)
		 DO J = K, (2*N)
		  AUGMATRIX(I,J) = AUGMATRIX(I,J) -AUGMATRIX(K+1,J) * M
		 END DO
		END DO
	END DO

c	!STORE ANSWER
	DO I =1, N
		DO J = 1, N
			INVERSE(I,J) = AUGMATRIX(I,J+N)
		END DO
	END DO
	ERRORFLAG = 0
        RETURN
        END SUBROUTINE INVERT
Chd|====================================================================
Chd|  RBE3FRF                       source/constraints/general/rbe3/rbe3f.F
Chd|-- called by -----------
Chd|        IMP3_A2B                      source/airbag/monv_imp0.F     
Chd|        UPD_FR                        source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE RBE3FRF(NML   ,IML   ,NS    ,A     ,AR    ,
     *                   FDSTNB,MDSTNB,JT    ,JR    ,IROT  )
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NML   ,IML(*)   ,NS,JT(*),JR(*),IROT
C     REAL
      my_real
     .   A(3,*), AR(3,*), FDSTNB(3,6,*) ,MDSTNB(3,6,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N,M
C     REAL
      my_real
     .        FNS(3),MNS(3)
C======================================================================|
        DO J = 1,3
	 FNS(J) = A(J,NS)*JT(J)
	ENDDO
        DO I=1,NML
	 M = IML(I)
         DO J = 1,3
	  A(1,M) = A(1,M)+FDSTNB(1,J,I)*FNS(J)
	  A(2,M) = A(2,M)+FDSTNB(2,J,I)*FNS(J)
	  A(3,M) = A(3,M)+FDSTNB(3,J,I)*FNS(J)
	 ENDDO
        ENDDO
        IF ((JR(1)+JR(2)+JR(3))>0) THEN
         DO J = 1,3
	  MNS(J) = AR(J,NS)*JR(J)
	 ENDDO
         DO I=1,NML
	  M = IML(I)
          DO J = 1,3
	   A(1,M) = A(1,M)+FDSTNB(1,J+3,I)*MNS(J)
	   A(2,M) = A(2,M)+FDSTNB(2,J+3,I)*MNS(J)
	   A(3,M) = A(3,M)+FDSTNB(3,J+3,I)*MNS(J)
	  ENDDO
         ENDDO
        ENDIF
        IF (IROT>0) THEN
         DO I=1,NML
	  M = IML(I)
          DO J = 1,3
	   AR(1,M) = AR(1,M)+MDSTNB(1,J,I)*FNS(J)
	   AR(2,M) = AR(2,M)+MDSTNB(2,J,I)*FNS(J)
	   AR(3,M) = AR(3,M)+MDSTNB(3,J,I)*FNS(J)
	  ENDDO
         ENDDO
         IF ((JR(1)+JR(2)+JR(3))>0) THEN
          DO I=1,NML
	   M = IML(I)
           DO J = 1,3
	    AR(1,M) = AR(1,M)+MDSTNB(1,J+3,I)*MNS(J)
	    AR(2,M) = AR(2,M)+MDSTNB(2,J+3,I)*MNS(J)
	    AR(3,M) = AR(3,M)+MDSTNB(3,J+3,I)*MNS(J)
	   ENDDO
          ENDDO
         ENDIF
        ENDIF
C---
      RETURN
      END
Chd|====================================================================
Chd|  PRERBE3P                      source/constraints/general/rbe3/rbe3f.F
Chd|-- called by -----------
Chd|        RBE3T1                        source/constraints/general/rbe3/rbe3f.F
Chd|        SMS_RBE3T1                    source/ams/sms_rbe3.F         
Chd|        SMS_RBE3T2                    source/ams/sms_rbe3.F         
Chd|        SMS_RBE3_PREC                 source/ams/sms_rbe3.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PRERBE3P(IRBE3 ,LRBE3 ,AD_M , IML  ,NMT   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE3(NRBE3L,*),LRBE3(*),AD_M(*) ,IML(*) , NMT
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N,M,IAD,NS,NML,ITAG(NUMNOD)
C NMT: number (no doublon) of main nodes,AD(M): index from NMT0->NMT
C======================================================================|
      DO N =1,NUMNOD
       ITAG(N) = 0
      END DO
      NMT = 0
      DO N=1,NRBE3
        IAD = IRBE3(1,N)
        NS  = IRBE3(3,N)
        NML = IRBE3(5,N)
        DO I=1,NML
         M = LRBE3(IAD+I)
         IF (ITAG(M)==0) THEN
          NMT = NMT + 1
          AD_M(IAD+I) = NMT
          ITAG(M) = NMT
          IML(NMT) = M
         ELSE
          AD_M(IAD+I) = ITAG(M)
         ENDIF
        ENDDO
      ENDDO
C---
      RETURN
      END
Chd|====================================================================
Chd|  PRERBE3P0                     source/constraints/general/rbe3/rbe3f.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PRERBE3P0(IRBE3 ,LRBE3 ,NMT   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE3(NRBE3L,*),LRBE3(*),NMT
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N,M,IAD,NS,NML,ITAG(NUMNOD)
C NMT: number (no doublon) of main nodes,AD(M): index from NMT0->NMT
C======================================================================|
      DO N =1,NUMNOD
       ITAG(N) = 0
      END DO
      NMT = 0
      DO N=1,NRBE3
        IAD = IRBE3(1,N)
        NS  = IRBE3(3,N)
        NML = IRBE3(5,N)
        DO I=1,NML
         M = LRBE3(IAD+I)
         IF (ITAG(M)==0) THEN
          NMT = NMT + 1
          ITAG(M) = NMT
         ENDIF
        ENDDO
      ENDDO
C---
      RETURN
      END
Chd|====================================================================
Chd|  MFAC_RBE3                     source/constraints/general/rbe3/rbe3f.F
Chd|-- called by -----------
Chd|        RBE3F                         source/constraints/general/rbe3/rbe3f.F
Chd|        SMS_RBE3_1                    source/ams/sms_rbe3.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MFAC_RBE3(FDSTNB,MDSTNB,NML ,IROT,SF,SM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NML,IROT
      my_real
     .   FDSTNB(3,6,*) ,MDSTNB(3,6,*),SF,SM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IFD,IMD
      my_real
     .   FSUM,MSUM,SUM
C======================================================================|
	IFD=0
	SF=ONE
	SM=ONE
        DO I=1,NML
         DO J = 1,3
          IF (IFD==0) THEN
           FSUM = FDSTNB(J,1,I)+FDSTNB(J,2,I)+FDSTNB(J,3,I)
           IF (FSUM <0) IFD=1
          END IF
         ENDDO
        ENDDO
C--renormalizing to avoid mass adding
        IF (IFD >0) THEN         
         SUM =ZERO
         DO I=1,NML
          DO J = 1,3
           FSUM = FDSTNB(J,1,I)+FDSTNB(J,2,I)+FDSTNB(J,3,I)
           SUM = SUM +ABS(FSUM)
          ENDDO
         ENDDO
         SF = THREE/SUM
        END IF
C 
       IF (IROT==0) RETURN
C       
        IMD=0
        DO I=1,NML
         DO J = 1,3
          IF (IMD==0) THEN
           MSUM = MDSTNB(J,1,I)+MDSTNB(J,2,I)+MDSTNB(J,3,I)
           IF (MSUM <0) IMD=1
          END IF
         ENDDO
        ENDDO
        IF (IMD >0) THEN         
         SUM =ZERO
         DO I=1,NML
          DO J = 1,3
           MSUM = MDSTNB(J,1,I)+MDSTNB(J,2,I)+MDSTNB(J,3,I)
           SUM = SUM +ABS(MSUM)
          ENDDO
         ENDDO
         SM = THREE/SUM
        END IF
C---
      RETURN
      END
