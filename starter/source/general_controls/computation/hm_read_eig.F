Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  HM_PREREAD_EIG                source/general_controls/computation/hm_read_eig.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        RANDOM_MOD                    share/modules1/random_mod.F   
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
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
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNT
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IE,N, IG, IGU1, IGU2, NNI, ID,  NM,M, SUB_INDEX
      CHARACTER TITR*nchartitle,KEY*ncharkey,MESS*40
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
Chd|====================================================================
Chd|  HM_READ_EIG                   source/general_controls/computation/hm_read_eig.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        EIGRLINE                      source/general_controls/computation/hm_read_eig.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_STRING                 source/devtools/hm_reader/hm_get_string.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        RANDOM_MOD                    share/modules1/random_mod.F   
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_EIG(EIGIPM, EIGIBUF, EIGRPM, IGRNOD   ,ITABM1   ,
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
      USE INOUTFILE_MOD
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
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER EIGIPM(*), EIGIBUF(*),ITABM1(*)
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
     .        J6(6), ITYP, IADB, IRCM, J, JJ, K, NLIG, NRES, IADN,
     .        NBNO, NBMO, II, IPRSP, IMLS, IFLAGUNIT,LEN_MODFILE,MAXL
      my_real
     .        CUTFREQ, SHIFTINI, TOL, FAC_T,FAC_FRE
      CHARACTER TITR*nchartitle, STRING*ncharfield, CODE*7,
     .          CTYPE*33, CPRE*17, NWLINE*100, MESS*40, MODFILE*2148,
     .          CIMLS*3 ,CPRSP*4,MODFILE_TMP*2148
      INTEGER, ALLOCATABLE :: NODES(:)
      my_real,
     .         ALLOCATABLE :: MODES(:,:,:)
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
      DATA MESS/'ADDITIONAL MODES FILE : NODES           '/
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
C
         IFLAGUNIT = 0                                           
         DO J=1,NUNITS                          
           IF (UNITAB%UNIT_ID(J) == UID) THEN
             FAC_T = UNITAB%FAC_T(J)
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
         IF (IFILE==2) THEN
C--- add generic message         
            WRITE(ISTDO,'(A)') ' ** WARNING - EIG OPTION:'
            WRITE(ISTDO,'(A)') 
     .           ' ** USE OF NASTRAN OUTPUT FILE NOT YET IMPLEMENTED'
            WRITE(IOUT,'(A)') ' ** WARNING - EIG OPTION:'
            WRITE(IOUT,'(A)') 
     .           ' ** USE OF NASTRAN OUTPUT FILE NOT YET IMPLEMENTED'
            IFILE=0
         ENDIF
C
         IF (IFILE==1.AND.IGU2/=0) THEN
            CALL ANCMSG(MSGID=588,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=IGU2)
            IGU2=0
         ENDIF
C
         IF (NSPMD>1.AND.IFILE==1) THEN
            CALL ANCMSG(MSGID=628,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO,
     .                  I1=ID,
     .                  C1=TITR)
            IFILE=0
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
C
         IF (IPRSP>1) IPRSP=0
         CALL HM_GET_FLOATV_DIM('Freqmin',FAC_FRE,IS_AVAILABLE,LSUBMODEL,UNITAB)
         IF (SHIFTINI==ZERO) SHIFTINI=EM03*FAC_FRE
C         CUTFREQ = CUTFREQ / FAC_T
C         SHIFTINI = SHIFTINI / FAC_T
C
         IF (NEV==0) NEV=100
         EIGIPM(IAD+4)=NEV
         EIGRPM(IADF+1)=ZERO
         EIGRPM(IADF+2)=(SHIFTINI*TWO*PI)**2
         EIGRPM(IADF+3)=CUTFREQ
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
C
         EIGIPM(IAD+13)=0
         IF (IFILE==1) THEN
            MAXL = 100
            CALL HM_GET_STRING('Filename', MODFILE, MAXL, IS_AVAILABLE)
            LEN_MODFILE = LEN_TRIM(MODFILE)
            MODFILE_TMP = INFILE_NAME(1:INFILE_NAME_LEN)//MODFILE(1:LEN_MODFILE)
            LEN_MODFILE = LEN_MODFILE + INFILE_NAME_LEN
            OPEN(UNIT=IFICM,FILE=MODFILE_TMP(1:LEN_MODFILE),
     .           ACCESS='SEQUENTIAL',FORM='FORMATTED',
     .           STATUS='OLD',ERR=999)
            CALL EIGRLINE(IFICM,NWLINE,ID,TITR)
            READ(NWLINE,FMT='(2I8)',ERR=9999) NBNO, NBMO
C
            IF (NEV>NBMO) THEN
               CALL ANCMSG(MSGID=589,
     .                     MSGTYPE=MSGWARNING,
     .                     ANMODE=ANINFO_BLIND_1,
     .                     I1=ID,
     .                     C1=TITR,
     .                     I3=NEV,
     .                     I2=NBMO)
               NEV=NBMO
               EIGIPM(IAD+4)=NEV
            ENDIF
            NBLFR=MIN(NBMO-2,NBLFR)
            EIGIPM(IAD+8)=NBLFR
C
            ALLOCATE(NODES(NBNO), MODES(NBMO,6,NN))
            NLIG=NBNO/10
            NRES=NBNO-NLIG*10
            IADN=0
            DO I=1,NLIG
               CALL EIGRLINE(IFICM,NWLINE,ID,TITR)
               READ(NWLINE,FMT='(10I8)',ERR=9999) (NODES(IADN+J),J=1,10)
               DO J=1,10
                  NODES(IADN+J)=USR2SYS(NODES(IADN+J),ITABM1,MESS,ID)
               ENDDO
               IADN=IADN+10
            ENDDO
            IF (NRES>0) THEN
               CALL EIGRLINE(IFICM,NWLINE,ID,TITR)
               READ(NWLINE,FMT='(10I8)',ERR=9999) 
     .                            (NODES(IADN+J),J=1,NRES)
               DO J=1,NRES
                  NODES(IADN+J)=USR2SYS(NODES(IADN+J),ITABM1,MESS,ID)
               ENDDO
            ENDIF
C
            DO I=1,NUMNOD
               ITAG(I)=0
            ENDDO
            DO I=1,NN
               II=EIGIBUF(IADB+I-1)
               ITAG(II)=I
            ENDDO
            DO I=1,NBNO
               II=NODES(I)
               NODES(I)=ITAG(II)
               ITAG(II)=-1
            ENDDO
            DO I=1,NN
               II=EIGIBUF(IADB+I-1)
               IF (ITAG(II)/=-1) THEN
                  J=ITABM1(II)
                  CALL ANCMSG(MSGID=586,
     .                        MSGTYPE=MSGERROR,
     .                        ANMODE=ANINFO_BLIND_1,
     .                        I1=ID,
     .                        C1=TITR,
     .                        I2=J)
               ENDIF
            ENDDO
C
            DO I=1,NBMO
               DO J=1,NBNO
                  JJ=NODES(J)
                  CALL EIGRLINE(IFICM,NWLINE,ID,TITR)
                  READ(NWLINE,FMT='(5F16.0)',ERR=9999) 
     .                                  (MODES(I,K,JJ),K=1,5)
                  CALL EIGRLINE(IFICM,NWLINE,ID,TITR)
                  READ(NWLINE,FMT='(F16.0)',ERR=9999) MODES(I,6,JJ)
               ENDDO
            ENDDO
C
            EIGIPM(IAD+12)=IRCM
            EIGIPM(IAD+13)=NBMO
            DO I=1,NBMO
               DO J=1,NBNO
                  IRCM=IRCM+1
                  WRITE(IEIGM,REC=IRCM) (MODES(I,K,J),K=1,6)
               ENDDO
            ENDDO
C
            DEALLOCATE(NODES, MODES)
            CLOSE(IFICM)
         ENDIF   
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
 999  CALL FREERR(3)
      RETURN
9999  CALL ANCMSG(MSGID=587,
     .            MSGTYPE=MSGERROR,
     .            ANMODE=ANINFO,
     .            I1=ID,
     .            C1=TITR,
     .            C2=NWLINE)
      RETURN
C      
2000  FORMAT(/
     .       /
     . '      EIGEN AND STATIC MODES COMPUTATION '/
     . '      ---------------------------------- '/)
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
Chd|====================================================================
Chd|  EIGRLINE                      source/general_controls/computation/hm_read_eig.F
Chd|-- called by -----------
Chd|        HM_READ_EIG                   source/general_controls/computation/hm_read_eig.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE EIGRLINE(IFIC, NWLINE, ID,TITR)                     
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IFIC, ID
      CHARACTER NWLINE*100
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ISTOP     
C
      ISTOP=0
      DO WHILE (ISTOP==0)
         READ(IFIC,'(A)',END=999) NWLINE
         IF (NWLINE(1:1)/='#') ISTOP=1
      ENDDO
C
      RETURN
  999 CALL ANCMSG(MSGID=585,
     .            MSGTYPE=MSGERROR,
     .            ANMODE=ANINFO,
     .            I1=ID,
     .            C1=TITR)
      RETURN
C
      END
      
