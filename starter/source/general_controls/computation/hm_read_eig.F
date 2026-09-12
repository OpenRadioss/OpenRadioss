Copyright>        OpenRadioss
Copyright>        Copyright (C) 2026 Siemens
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
Copyright>        Commercial Alternative: Simcenter Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
Copyright>        commercial version may interest you: 
Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    hm_preread_eig         ../starter/source/general_controls/computation/hm_read_eig.F
!||--- called by ------------------------------------------------------
!||    lectur                 ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                 ../starter/source/output/message/message.F
!||    hm_get_intv            ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_read_key     ../starter/source/devtools/hm_reader/hm_option_read_key.F
!||    hm_option_start        ../starter/source/devtools/hm_reader/hm_option_start.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod     ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod            ../starter/share/message_module/message_mod.F
!||    random_mod             ../starter/share/modules1/random_mod.F
!||    submodel_mod           ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_PREREAD_EIG(IGRNOD   ,NNT   ,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE UNITAB_MOD
      USE RANDOM_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE, NCHARKEY
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNT
C-----------------------------------------------
      TYPE (GROUP_), DIMENSION(NGRNOD) :: IGRNOD
      TYPE (SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IE, IG, IGU1, IGU2, NNI, ID, SUB_INDEX
      CHARACTER(LEN=NCHARTITLE)::TITR
      CHARACTER :: MESS*40
      LOGICAL IS_AVAILABLE
C
      DATA MESS/'EIGEN AND STATIC MODES COMPUTATION      '/
C=====================================================================  
C
      NNT=0
      CALL HM_OPTION_START('/EIG')
      DO IE=1,NEIG
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = ID,
     .                       SUBMODEL_INDEX = SUB_INDEX,
     .                       OPTION_TITR = TITR)
        CALL HM_GET_INTV('grnd_ID',IGU1,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('grnd_bc',IGU2,IS_AVAILABLE,LSUBMODEL)
C
         IF (IGU1==0) THEN
            NNT=NNT+NUMNOD
         ELSE
            IG=0
            DO I=1,NGRNOD
              IF(IGRNOD(I)%ID==IGU1)THEN
                IG=I
              ENDIF
            ENDDO
C
            IF(IG==0)THEN
               CALL ANCMSG(MSGID=53,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO,
     .                     C1= MESS,
     .                     I1=IGU1)
               RETURN
            ENDIF
C
            NNT=NNT+IGRNOD(IG)%NENTITY
         ENDIF
C
         NNI=0
         IF (IGU2/=0) THEN
            IG=0
            DO I=1,NGRNOD
              IF(IGRNOD(I)%ID==IGU2)THEN
                IG=I
              ENDIF
            ENDDO
C
            IF(IG==0)THEN
               CALL ANCMSG(MSGID=53,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO,
     .                     C1= MESS,
     .                     I1=IGU2)
               RETURN
            ENDIF
C
            NNI=IGRNOD(IG)%NENTITY
C
         ENDIF
         NNT=NNT+NNI  
      ENDDO      
C-----------------------
      RETURN
      END SUBROUTINE HM_PREREAD_EIG
!||====================================================================
!||    hm_read_eig            ../starter/source/general_controls/computation/hm_read_eig.F
!||--- called by ------------------------------------------------------
!||    lectur                 ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                 ../starter/source/output/message/message.F
!||    hm_get_floatv          ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_intv            ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_read_key     ../starter/source/devtools/hm_reader/hm_option_read_key.F
!||    hm_option_start        ../starter/source/devtools/hm_reader/hm_option_start.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod     ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod            ../starter/share/message_module/message_mod.F
!||    random_mod             ../starter/share/modules1/random_mod.F
!||    submodel_mod           ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_EIG(EIGIPM, EIGIBUF, EIGRPM, IGRNOD   ,
     .                       UNITAB, LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE UNITAB_MOD
      USE RANDOM_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "eigcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER EIGIPM(*), EIGIBUF(*)
      my_real
     .        EIGRPM(*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ID,SUB_INDEX,UID
      LOGICAL IS_AVAILABLE
      INTEGER I, ITAG(NUMNOD), IAD, IADF, IE, IGU1, IGU2, NN, IG, NNI,
     .        ICT, ICR, NEV, NBLFR, INCV, NITER, IPRI, NDOF, NMS, IFILE,
     .        J6(6), ITYP, IADB, J, IPRSP, IMLS, IFLAGUNIT,
     .        IRIGID_OUT
      my_real
     .        CUTFREQ, SHIFTINI, TOL,
     .        TOLRIGID,TOLPIVU,TOLPIVP
      CHARACTER(LEN=NCHARTITLE)::TITR
      CHARACTER :: CTYPE*33, CPRE*17, CIMLS*3, CPRSP*4
C=====================================================================  
      IAD=1
      IADB=1
      IADF=1
      CALL HM_OPTION_START('/EIG')
      DO IE=1,NEIG
        ITAG(1:NUMNOD)=0
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = ID,
     .                       UNIT_ID = UID,
     .                       SUBMODEL_INDEX = SUB_INDEX,
     .                       OPTION_TITR = TITR)
        CALL HM_GET_INTV('grnd_ID',IGU1,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('grnd_bc',IGU2,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('Tx',J6(1),IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('Ty',J6(2),IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('Tz',J6(3),IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('OmegaX',J6(4),IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('OmegaY',J6(5),IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('OmegaZ',J6(6),IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('Ifile',IFILE,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('Nmod',NEV,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('Inorm',IPRSP,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_FLOATV('Cutfreq',CUTFREQ,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('Freqmin',SHIFTINI,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_INTV('Nbloc',NBLFR,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('Incv',INCV,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('Niter',NITER,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('Ipri',IPRI,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_FLOATV('Tol',TOL,IS_AVAILABLE,LSUBMODEL,UNITAB)
        IRIGID_OUT=1
        CALL HM_GET_INTV('Irigid',IRIGID_OUT,IS_AVAILABLE,LSUBMODEL)
        IF (.NOT.IS_AVAILABLE) IRIGID_OUT=1
        IF (IRIGID_OUT.NE.0.AND.IRIGID_OUT.NE.2) IRIGID_OUT=1
        TOLRIGID=1.0E-12
        CALL HM_GET_FLOATV('TolRigid',TOLRIGID,IS_AVAILABLE,
     .                     LSUBMODEL,UNITAB)
        IF (.NOT.IS_AVAILABLE.OR.TOLRIGID.LE.ZERO) TOLRIGID=1.0E-12
        TOLPIVU=1.0E-12
        CALL HM_GET_FLOATV('TolPivU',TOLPIVU,IS_AVAILABLE,
     .                     LSUBMODEL,UNITAB)
        IF (.NOT.IS_AVAILABLE.OR.TOLPIVU.LE.ZERO) TOLPIVU=1.0E-12
        TOLPIVP=1.0E-12
        CALL HM_GET_FLOATV('TolPivP',TOLPIVP,IS_AVAILABLE,
     .                     LSUBMODEL,UNITAB)
        IF (.NOT.IS_AVAILABLE.OR.TOLPIVP.LE.ZERO) TOLPIVP=1.0E-12
C
         IFLAGUNIT = 0                                           
         DO J=1,UNITAB%NUNITS                          
           IF (UNITAB%UNIT_ID(J) == UID) THEN
             IFLAGUNIT = 1                      
             EXIT                               
           ENDIF                                
         ENDDO                                           
         IF (UID/=0.AND.IFLAGUNIT==0) THEN
           CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                 I2=UID,I1=ID,C1='EIG',
     .                 C2='EIG',
     .                 C3=TITR)                              
         ENDIF  
         EIGIPM(IAD)=ID
C
         IF (IFILE.NE.0) THEN
            WRITE(ISTDO,'(A)')
     .       ' ** ERROR: /EIG external mode import is not supported'
            WRITE(ISTDO,'(A)')
     .       ' ** by the public /EIG backend; set Ifile=0'
            WRITE(IOUT,'(A)')
     .       ' ** ERROR: /EIG external mode import is not supported'
            WRITE(IOUT,'(A)')
     .       ' ** by the public /EIG backend; set Ifile=0'
            CALL ARRET(2)
         ENDIF
C
         IMLS=0
         IF (IGU1==0) THEN
            DO I=1,NUMNOD
               EIGIBUF(IADB+I-1)=I
            ENDDO
            NN=NUMNOD
         ELSE
            IG=0
            DO I=1,NGRNOD
              IF(IGRNOD(I)%ID==IGU1)THEN
                IG=I
              ENDIF
            ENDDO
C
            NN=IGRNOD(IG)%NENTITY
            DO I=1,NN
               EIGIBUF(IADB+I-1)=IGRNOD(IG)%ENTITY(I)
               ITAG(IGRNOD(IG)%ENTITY(I))=1
            ENDDO
         ENDIF
         EIGIPM(IAD+9)=NN
         IF (NSPMD>1.AND.NN/=NUMNOD) THEN
            CALL ANCMSG(MSGID=629,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=NN,
     .                  I3=NUMNOD)
         ENDIF
         NNI=0
         ICT=0
         ICR=0
         IF (IGU2==0) THEN
            ITYP=1
            EIGIPM(IAD+1)=ITYP
         ELSE
            ITYP=2
            EIGIPM(IAD+1)=ITYP
            ICT=J6(1)*4 +J6(2)*2 +J6(3)
            ICR=J6(4)*4 +J6(5)*2 +J6(6)
C
            IG=0
            DO I=1,NGRNOD
              IF(IGRNOD(I)%ID==IGU2)THEN
                IG=I
              ENDIF
            ENDDO
C
            DO I=1,IGRNOD(IG)%NENTITY
               IF (ITAG(IGRNOD(IG)%ENTITY(I))==1) THEN
                  NNI=NNI+1
                  EIGIBUF(IADB+NN+NNI-1)=IGRNOD(IG)%ENTITY(I)
               ENDIF
            ENDDO
         ENDIF
         EIGIPM(IAD+10)=NNI
         EIGIPM(IAD+2)=ICT
         EIGIPM(IAD+3)=ICR
C
         EIGIPM(IAD+16)=IMLS
         EIGIPM(IAD+17)=IRIGID_OUT
C
         IF (IPRSP>1) IPRSP=0
         IF (NEV==0) NEV=100
         EIGIPM(IAD+4)=NEV
         EIGRPM(IADF+1)=ZERO
         EIGRPM(IADF+2)=(SHIFTINI*TWO*PI)**2
         EIGRPM(IADF+3)=CUTFREQ
         EIGRPM(IADF+4)=TOLRIGID
         EIGRPM(IADF+5)=ZERO
         EIGRPM(IADF+6)=TOLPIVU
         EIGRPM(IADF+7)=TOLPIVP
C
         IF (NBLFR==0) NBLFR=NEV      
         IF (INCV==0) INCV=2
         IF (NITER==0) NITER=300
C
         EIGIPM(IAD+8)=NBLFR
         EIGIPM(IAD+5)=INCV
         EIGIPM(IAD+6)=NITER
         EIGIPM(IAD+7)=IPRI
C
         EIGIPM(IAD+14)=IPRSP
C
         EIGIPM(IAD+15)=0
C
         EIGRPM(IADF)=TOL
C The public backend does not consume legacy additional-mode records.
         EIGIPM(IAD+12)=0
         EIGIPM(IAD+13)=0
C PRINTOUTS
         NDOF=0
         IF(ICT==1)THEN
           NDOF=1
         ELSEIF(ICT==2)THEN
           NDOF=1
         ELSEIF(ICT==3)THEN
           NDOF=2
         ELSEIF(ICT==4)THEN
           NDOF=1
         ELSEIF(ICT==5)THEN
           NDOF=2
         ELSEIF(ICT==6)THEN
           NDOF=2
         ELSEIF(ICT==7)THEN
           NDOF=3
         ENDIF
         IF(ICR==1)THEN
           NDOF=NDOF+1
         ELSEIF(ICR==2)THEN
           NDOF=NDOF+1
         ELSEIF(ICR==3)THEN
           NDOF=NDOF+2
         ELSEIF(ICR==4)THEN
           NDOF=NDOF+1
         ELSEIF(ICR==5)THEN
           NDOF=NDOF+2
         ELSEIF(ICR==6)THEN
           NDOF=NDOF+2
         ELSEIF(ICR==7)THEN
           NDOF=NDOF+3
         ENDIF
         NMS=NNI*NDOF
         IF (ITYP==1) THEN
            CTYPE='                  FREE EIGENMODES'
         ELSEIF (ITYP==2) THEN
            CTYPE='CLAMPED EIGENMODES + STATIC MODES'
         ENDIF
         IF (TOL>ZERO) THEN
            WRITE(CPRE,FMT='(7X,1PE10.3)') TOL
         ELSE
            CPRE='MACHINE PRECISION'
         ENDIF
C
         CIMLS=' NO'
         IF (IMLS==1) CIMLS='YES'
         CPRSP='MASS'
         IF (IPRSP==1) CPRSP='MAX '
         WRITE(IOUT,2100) ID,TRIM(TITR),CTYPE, NEV, NBLFR, INCV, NITER, CPRE,CIMLS,CPRSP 
         IF (ITYP==2) WRITE(IOUT,2105) NMS 
         WRITE(IOUT,2109)
         WRITE(IOUT,2110) (EIGIBUF(IADB+I-1),I=1,NN)
         IF (ITYP==2) THEN
            WRITE(IOUT,2111)
            WRITE(IOUT,2110) (EIGIBUF(IADB+NN+I-1),I=1,NNI)
         ENDIF
C
         EIGIPM(IAD+11)=IADB
         IAD=IAD+NEIPM
         IADB=IADB+NN+NNI
         IADF=IADF+NERPM
      ENDDO      
C-----------------------
      RETURN
C      
2100  FORMAT( /5X,'EIGENPROBLEM ID ',I10,1X,A
     .       /10X,'TYPE             ',A33
     .       /10X,'NUMBER OF EIGENMODES                    ',I10
     .       /10X,'NUMBER OF FREQUENCIES PER BLOCK         ',I10
     .       /10X,'FACTOR FOR NUMBER OF LANCZOS VECTORS    ',I10
     .       /10X,'MAX NUMBER OF ARNOLDI ITERATIONS        ',I10
     .       /10X,'PRECISION                        ',A17
     .       /10X,'USE OF MULTI-LEVEL CONDENSATION         ',7X,A3,
     .       /10X,'OUTPUT EIGENVECTORS NORMALIZATION       ',6X,A4)
2105  FORMAT( 10X,'NUMBER OF STATIC MODES                  ',I10)
2109  FORMAT( 10X,'NODES ')
2110  FORMAT(  9X,10I9)
2111  FORMAT( 10X,'INTERFACE NODES ')
      END SUBROUTINE HM_READ_EIG
