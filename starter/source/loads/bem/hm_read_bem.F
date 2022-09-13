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
Chd|  HM_PREREAD_BEM                source/loads/bem/hm_read_bem.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_PREREAD_BEM(IGRSURF,IGRNOD, NNFT ,
     .     UNITAB, NOM_OPT, LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "flowcom.inc"
#include      "sysunit.inc"
C     NSUBMOD
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER NNFT
      INTEGER NOM_OPT(LNOPT1,*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, ID, ITYP, II, NINOUT, J, ISU, NN, IAD, ITAG(NUMNOD),
     .        IAD1, NEL, J1, J2, J3, J4, ISH34, NNO, L, IBID,
     .        III, IGR, NNN, UID, IFLAGUNIT, IUNIT
      INTEGER JFORM, FREESURF, NELMAX, HG
      my_real
     .        RBID
      CHARACTER TITR*nchartitle
      LOGICAL :: IS_AVAILABLE
      INTEGER :: HM_NDAA, HM_NFLOW
      
      HM_NDAA = 0
      HM_NFLOW = 0
      CALL HM_OPTION_COUNT('/BEM/DAA', HM_NDAA)
      CALL HM_OPTION_COUNT('/BEM/FLOW', HM_NFLOW)
C
      LIFLOW=0
      LRFLOW=0
      NNFT=0
      CALL HM_OPTION_START('/BEM/FLOW')
      DO I = 1, HM_NFLOW
         CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_TITR = TITR, OPTION_ID = ID)         
                                
         ITYP = 1
C     
         CALL HM_GET_INTV('surf_IDex', II, IS_AVAILABLE, LSUBMODEL)
         CALL HM_GET_INTV('Nio', NINOUT, IS_AVAILABLE, LSUBMODEL)
         ISU=0
         DO J=1,NSURF
            IF (II==IGRSURF(J)%ID) ISU=J
         ENDDO
         IF (ISU==0) THEN
            CALL ANCMSG(MSGID=621,MSGTYPE=MSGERROR,ANMODE=ANINFO,
     .           I1=ID,C1=TITR,C2='SURFACE',I2=II)
         ENDIF
C     
         NN=IGRSURF(ISU)%NSEG
         DO J=1,NUMNOD
            ITAG(J)=0
         ENDDO
         NEL=0
         DO J=1,NN
            J1=IGRSURF(ISU)%NODES(J,1)
            J2=IGRSURF(ISU)%NODES(J,2)
            J3=IGRSURF(ISU)%NODES(J,3)
            J4=IGRSURF(ISU)%NODES(J,4)
            ISH34=IGRSURF(ISU)%ELTYP(J)
            ITAG(J1)=1
            ITAG(J2)=1
            ITAG(J3)=1
            IF (ISH34==3) THEN
               NEL=NEL+2
               ITAG(J4)=1
            ELSEIF (ISH34==7) THEN
               NEL=NEL+1
            ENDIF
         ENDDO
         NNO=0
         DO J=1,NUMNOD
            IF (ITAG(J)==1) NNO=NNO+1
         ENDDO
         NNFT=NNFT+NNO
C     
         CALL HM_GET_INTV('grn_IDaux', III, IS_AVAILABLE, LSUBMODEL)
         
         NNN=0
         IF (III/=0) THEN
            IGR=0
            DO J=1,NGRNOD
               IF (IGRNOD(J)%ID==III) IGR=J
            ENDDO
            IF (IGR==0) THEN
               CALL ANCMSG(MSGID=621,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=ID,
     .              C1=TITR,
     .              C2='NODE GROUP',
     .              I2=III)
            ENDIF
            NNN=IGRNOD(IGR)%NENTITY
         ENDIF
C     
         IF (NSPMD == 1) THEN
            LIFLOW=LIFLOW+NIFLOW+NNO+3*NEL+NINOUT*NIIOFLOW+NNN+NEL+NNO+NNN
         ELSE
            LIFLOW=LIFLOW+NIFLOW+NNO+3*NEL+NINOUT*NIIOFLOW+NNN+NEL+4*NNO+2*NNN+2*NEL
         ENDIF   
         LRFLOW=LRFLOW+NRFLOW+5*(NNO+NNN)+NINOUT*NRIOFLOW

      ENDDO
C     
      

      CALL HM_OPTION_START('/BEM/DAA')
      DO I = 1, HM_NDAA
         CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_TITR = TITR, OPTION_ID = ID)
         
         ITYP = 3
C     DAA SURFACES
         NELMAX=0
         CALL HM_GET_INTV('surf_ID', II, IS_AVAILABLE, LSUBMODEL)
         CALL HM_GET_INTV('Freesurf', FREESURF, IS_AVAILABLE, LSUBMODEL)
         JFORM=2
         IF(FREESURF == 0) FREESURF=1
         ISU=0
         DO J=1,NSURF
            IF (II==IGRSURF(J)%ID) ISU=J
         ENDDO
         IF (ISU==0) THEN
            CALL ANCMSG(MSGID=1603,MSGTYPE=MSGERROR,ANMODE=ANINFO,
     .           I1=ID,C1=TITR,C2='SURFACE NUMBER NOT FOUND')
         ENDIF
C     
         NN= IGRSURF(ISU)%NSEG
         DO J=1,NUMNOD
            ITAG(J)=0
         ENDDO
         NEL=0
         DO J=1,NN
            J1=IGRSURF(ISU)%NODES(J,1)
            J2=IGRSURF(ISU)%NODES(J,2)
            J3=IGRSURF(ISU)%NODES(J,3)
            J4=IGRSURF(ISU)%NODES(J,4)
            ISH34=IGRSURF(ISU)%ELTYP(J)
            ITAG(J1)=1
            ITAG(J2)=1
            ITAG(J3)=1
            IF (ISH34==3) THEN
               ITAG(J4)=1
               IF(JFORM == 1) THEN
                  NEL=NEL+2
               ELSEIF(JFORM == 2) THEN
                  NEL=NEL+1
               ENDIF
            ELSEIF (ISH34==7) THEN
               NEL=NEL+1
            ENDIF
         ENDDO
         NNO=0
         DO J=1,NUMNOD
            IF (ITAG(J)==1) NNO=NNO+1
         ENDDO
         IF(NSPMD == 1) THEN
            IF(JFORM == 1) THEN
               LIFLOW=LIFLOW+NIFLOW+NNO+3*NEL+NNO
            ELSEIF(JFORM == 2) THEN
               LIFLOW=LIFLOW+NIFLOW+NNO+5*NEL+NNO+NBGAUGE
            ENDIF
         ELSE
            IF(JFORM == 1) THEN
               LIFLOW=LIFLOW+NIFLOW+NNO+3*NEL+NNO+NNO+2*NEL
            ELSEIF(JFORM == 2) THEN
               LIFLOW=LIFLOW+NIFLOW+NNO+5*NEL+NNO+NBGAUGE+NNO+2*NEL
            ENDIF
         ENDIF
         HG = HUGE(NEL)
         IF (NEL > INT(SQRT(REAL(HG)))) THEN
            CALL ANCMSG(MSGID = 1711, ANMODE=ANINFO, MSGTYPE = MSGERROR, 
     .           I1 = INT(SQRT(REAL(HG))))
            CALL ARRET(2)
         ENDIF			
         IF(NEL < NELMAX) THEN
            LRFLOW=LRFLOW+NRFLOW+7*NEL+NEL*NEL+3*NEL
         ELSE
            LIFLOW=LIFLOW+NEL
            LRFLOW=LRFLOW+NRFLOW+7*NEL+2*NEL*NEL+3*NEL
         ENDIF
         IF(FREESURF == 2) LRFLOW=LRFLOW+3*NEL
      ENDDO
C
      RETURN
 999  CALL FREERR(1)
      RETURN
      END
Chd|====================================================================
Chd|  HM_READ_BEM                   source/loads/bem/hm_read_bem.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX_DIM  source/devtools/hm_reader/hm_get_float_array_index_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        INIT_QD                       source/fluid/init_qd.F        
Chd|        INIT_TG                       source/fluid/init_tg.F        
Chd|        MASS_FLUID_QD                 source/fluid/mass-fluid_qd.F  
Chd|        MASS_FLUID_TG                 source/fluid/mass-fluid_tg.F  
Chd|        ID                            source/boundary_conditions/ebcs/hm_read_ebcs_inlet.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_BEM(IGRSURF, IFLOW   , RFLOW  ,
     .                    NPC    , IGRNOD  , MEMFLOW,UNITAB,
     .                    X      , NOM_OPT , LGAUGE ,IGRV, LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "scr05_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "flowcom.inc"
#include      "sysunit.inc"
C     NSUBMOD
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER IFLOW(*),
     .        NPC(*)
      INTEGER NOM_OPT(LNOPT1,*), LGAUGE(3,*), IGRV(NIGRV,*)
      INTEGER(KIND=8) MEMFLOW(*)
      my_real
     .        RFLOW(*), X(3,*)
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IADI, IADR, I, ID, ITYP, II, NINOUT, ISU, J, IPIMP,
     .     NN, IAD, ITAG(NUMNOD), IAD1, IAD2, J1, J2, J3, J4, ISH34,
     .     NNO, ITABINV(NUMNOD), NEL, IVFREE, IFVEL, IFPRES,
     .     K, IFUNC, L, IFPA, IADMATI, IADMATR, III, IGR, NNN,
     .     NBLOC, IFORM, ILVOUT, ITAGIO(NUMNOD), NG1, NG2, NG3, NG4,
     .     N1, N2, N3, N4, PROD, NPROW, NPCOL, ITEST, IVINI, IFVINI,
     .     IINSIDE, NRMAX, NR, ISUIO,NFLOW0, NBLOCMAX, UID,
     .     IFLAGUNIT,IUNIT
      INTEGER JFORM, KFORM, II1, II2, II3, II4, II5, II6
      INTEGER IR1, IR2, IR3,IR4, IR5, IR6, IR7, IR8, IR9, IR10, IR11
      INTEGER IPRES, IWAVE, INTEGR, FREESURF, AFTERFLOW, GRAV_ID, NELMAX, IBID, HG
      INTEGER, DIMENSION(:), ALLOCATABLE :: N_SHELL
      my_real
     .     SFPA, SFVEL, SFPRES, SCALT, DTSUB, RHO, TOLE, SCALT_PA,
     .     SFVINI, DIRX, DIRY, DIRZ, NORM, RNSPMD, SCALT_VI
      my_real XC, YC, ZC, XS, YS, ZS, SSP, PMAX, THETA
      my_real APMAX, ATHETA, PMIN
      my_real XA, YA, ZA, XD, YD, ZD, TT
      my_real, DIMENSION(:,:), ALLOCATABLE :: CBEM
      my_real :: FAC_GEN
      CHARACTER KEY*ncharkey,TITR*nchartitle
      LOGICAL :: IS_AVAILABLE
      INTEGER :: HM_NDAA, HM_NFLOW
      
      HM_NDAA = 0
      HM_NFLOW = 0
      CALL HM_OPTION_COUNT('/BEM/DAA', HM_NDAA)
      CALL HM_OPTION_COUNT('/BEM/FLOW', HM_NFLOW)
C     
      IADI=0
      IADR=0
      IADMATI=1
      IADMATR=1
      NFLOW0=NFLOW
      IFUNC  = 0

      CALL HM_OPTION_START('/BEM/FLOW')

      DO I=1,HM_NFLOW
         CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_TITR = TITR, OPTION_ID = ID)
                                  
         ITYP = 1
         CALL HM_GET_INTV('surf_IDex', II, IS_AVAILABLE, LSUBMODEL)
         CALL HM_GET_INTV('Nio', NINOUT, IS_AVAILABLE, LSUBMODEL)
         CALL HM_GET_INTV('Iinside', IINSIDE, IS_AVAILABLE, LSUBMODEL)
         CALL HM_GET_INTV('Ifsp', IFPA, IS_AVAILABLE, LSUBMODEL)
         CALL HM_GET_FLOATV('Fscalesp', SFPA, IS_AVAILABLE, LSUBMODEL, UNITAB)
         CALL HM_GET_FLOATV('Ascalesp', SCALT, IS_AVAILABLE, LSUBMODEL, UNITAB)
         
         ISU=0
         DO J=1,NSURF
            IF (II==IGRSURF(J)%ID) ISU=J
         ENDDO
         SCALT_PA=SCALT
C     
         NN=IGRSURF(ISU)%NSEG
         DO J=1,NUMNOD
            ITAG(J)=0
         ENDDO
         DO J=1,NN
            J1=IGRSURF(ISU)%NODES(J,1)
            J2=IGRSURF(ISU)%NODES(J,2)
            J3=IGRSURF(ISU)%NODES(J,3)
            J4=IGRSURF(ISU)%NODES(J,4)
            ISH34=IGRSURF(ISU)%ELTYP(J)
            ITAG(J1)=1
            ITAG(J2)=1
            ITAG(J3)=1
            IF (ISH34==3) ITAG(J4)=1
         ENDDO
         NNO=0
         DO J=1,NUMNOD
            IF (ITAG(J)==1) THEN
               NNO=NNO+1
               IFLOW(IADI+NIFLOW+NNO)=J
               ITABINV(J)=NNO
            ENDIF
         ENDDO
         NEL=0
         DO J=1,NN
            J1=IGRSURF(ISU)%NODES(J,1)
            J2=IGRSURF(ISU)%NODES(J,2)
            J3=IGRSURF(ISU)%NODES(J,3)
            J4=IGRSURF(ISU)%NODES(J,4)
            ISH34=IGRSURF(ISU)%ELTYP(J)
            IF (ISH34==7) THEN
               NEL=NEL+1
               IFLOW(IADI+NIFLOW+NNO+3*(NEL-1)+1)=ITABINV(J1)
               IFLOW(IADI+NIFLOW+NNO+3*(NEL-1)+2)=ITABINV(J2)
               IFLOW(IADI+NIFLOW+NNO+3*(NEL-1)+3)=ITABINV(J3)
            ELSEIF (ISH34==3) THEN
               NEL=NEL+1
               IFLOW(IADI+NIFLOW+NNO+3*(NEL-1)+1)=ITABINV(J1)
               IFLOW(IADI+NIFLOW+NNO+3*(NEL-1)+2)=ITABINV(J2)
               IFLOW(IADI+NIFLOW+NNO+3*(NEL-1)+3)=ITABINV(J4)
               NEL=NEL+1
               IFLOW(IADI+NIFLOW+NNO+3*(NEL-1)+1)=ITABINV(J2)
               IFLOW(IADI+NIFLOW+NNO+3*(NEL-1)+2)=ITABINV(J3)
               IFLOW(IADI+NIFLOW+NNO+3*(NEL-1)+3)=ITABINV(J4)
            ENDIF
         ENDDO
C     
         IF (IINSIDE/=2) IINSIDE=1
C     
         IPIMP=0
         IF (IFPA/=0) THEN
            IFUNC=0
            DO J=1,NFUNCT
               IF (IFPA==NPC(J)) IFUNC=J
            ENDDO
            IF (IFUNC==0) THEN
               CALL ANCMSG(MSGID=621,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=ID,
     .              C1=TITR,
     .              C2='FUNCTION',
     .              I2=IFPA)
            ENDIF
            IFPA=IFUNC
            IPIMP=IPIMP+1
         ENDIF 
C     
         CALL HM_GET_INTV('grn_IDaux', III, IS_AVAILABLE, LSUBMODEL)
         CALL HM_GET_INTV('Itest', ITEST, IS_AVAILABLE, LSUBMODEL)
         CALL HM_GET_FLOATV('Tole', TOLE, IS_AVAILABLE, LSUBMODEL, UNITAB)
         
         IF (IINSIDE==2.AND.ITEST==1) ITEST=2
         IF (TOLE==ZERO) TOLE=EM5
C     
         CALL HM_GET_FLOATV('Rho', RHO, IS_AVAILABLE, LSUBMODEL, UNITAB)
         CALL HM_GET_INTV('Ivinf', IVINI, IS_AVAILABLE, LSUBMODEL)
C     
         NNN=0
         IF (III/=0) THEN
            DO J=1,NGRNOD
               IF (IGRNOD(J)%ID==III) IGR=J
            ENDDO
            NNN=IGRNOD(IGR)%NENTITY
            DO J=1,NNN
               IFLOW(IADI+NIFLOW+NNO+3*NEL+NINOUT*NIIOFLOW+J)=
     .              IGRNOD(IGR)%ENTITY(J)
            ENDDO
         ENDIF 
C     
         IVFREE=0
         DO J = 1, NINOUT
            CALL HM_GET_INT_ARRAY_INDEX('surf_IDio', II, J, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INT_ARRAY_INDEX('fct_IDvel', IFVEL, J, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INT_ARRAY_INDEX('fct_IDpr', IFPRES, J, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_FLOAT_ARRAY_INDEX('Fscalenv', SFVEL, J, IS_AVAILABLE, LSUBMODEL, UNITAB)               
            CALL HM_GET_FLOAT_ARRAY_INDEX('Fscalepres', SFPRES, J, IS_AVAILABLE, LSUBMODEL, UNITAB)               
            CALL HM_GET_FLOAT_ARRAY_INDEX('Ascalet', SCALT, J, IS_AVAILABLE, LSUBMODEL, UNITAB)               

            
            IF(SFVEL  == ZERO) THEN
               CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('Sfvel', FAC_GEN, J, IS_AVAILABLE, LSUBMODEL, UNITAB)
               SFVEL  = ONE * FAC_GEN
            ENDIF
            IF(SFPRES == ZERO) THEN
               CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('Sfpres', FAC_GEN, J, IS_AVAILABLE, LSUBMODEL, UNITAB)
               SFPRES = ONE * FAC_GEN
            ENDIF
            IF(SCALT  == ZERO) THEN
               CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('Scal_t', FAC_GEN, J, IS_AVAILABLE, LSUBMODEL, UNITAB)
               SCALT  = ONE * FAC_GEN
            ENDIF
C     
            ISUIO=0
            DO K=1,NSURF
               IF (II==IGRSURF(K)%ID) ISUIO=K
            ENDDO
            IF (ISUIO==0) THEN
               CALL ANCMSG(MSGID=621,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=ID,
     .              C1=TITR,
     .              C2='SURFACE',
     .              I2=II)
            ENDIF
            IFLOW(IADI+NIFLOW+NNO+3*NEL+NIIOFLOW*(J-1)+1)=ISUIO
C     
            IFLOW(IADI+NIFLOW+NNO+3*NEL+NIIOFLOW*(J-1)+2)=0
C     
            IF (IFVEL/=0) THEN
               IFUNC=0
               DO K=1,NFUNCT
                  IF (IFVEL==NPC(K)) IFUNC=K
               ENDDO
               IF (IFUNC==0) THEN
                  CALL ANCMSG(MSGID=621,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 I1=ID,
     .                 C1=TITR,
     .                 C2='FUNCTION',
     .                 I2=IFVEL)
               ENDIF
               IFVEL=IFUNC
            ELSE
               IVFREE=IVFREE+1
            ENDIF
            IFLOW(IADI+NIFLOW+NNO+3*NEL+NIIOFLOW*(J-1)+3)=IFVEL
            RFLOW(IADR+NRFLOW+5*(NNO+NNN)+NRIOFLOW*(J-1)+1)=SFVEL
C     
            IF (IFPRES/=0) THEN
               IPIMP=IPIMP+1
               IFUNC=0
               DO K=1,NFUNCT
                  IF (IFPRES==NPC(K)) IFUNC=K
               ENDDO
               IF (IFUNC==0) THEN
                  CALL ANCMSG(MSGID=621,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 I1=ID,
     .                 C1=TITR,
     .                 C2='FUNCTION',
     .                 I2=IFPRES)
               ENDIF
               IFPRES=IFUNC
            ENDIF
            IFLOW(IADI+NIFLOW+NNO+3*NEL+NIIOFLOW*(J-1)+4)=IFPRES
            RFLOW(IADR+NRFLOW+5*(NNO+NNN)+NRIOFLOW*(J-1)+2)=SFPRES
C     
            RFLOW(IADR+NRFLOW+5*(NNO+NNN)+NRIOFLOW*(J-1)+3)=SCALT
         ENDDO
         IF (NINOUT>=1) THEN
            IF (IPIMP/=1) THEN
               CALL ANCMSG(MSGID=622,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=ID,
     .              C1=TITR,
     .              C2='ONE AND ONLY ONE PRESSURE MUST BE IMPOSED')
            ELSEIF (IVFREE/=1.AND.(IINSIDE==0.OR.
     .              (IINSIDE==1.AND.IVINI==0))) THEN
               CALL ANCMSG(MSGID=622,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=ID,
     .              C1=TITR,
     .              C2='ONE AND ONLY ONE VELOCITY MUST BE LEFT FREE')
            ELSEIF (IVFREE/=0.AND.IINSIDE==1.AND.IVINI==1) THEN
               CALL ANCMSG(MSGID=622,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=ID,
     .              C1=TITR,
     .              C2='NO FREE VELOCITY ALLOWED')
            ENDIF
         ELSE
            IF (IFPA==0) THEN
               CALL ANCMSG(MSGID=622,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=ID,
     .              C1=TITR,
     .              C2='NO IMPOSED PRESSURE')
            ENDIF
         ENDIF
C     Tag des elements d'entrees et de sorties
         DO J=1,NNO
            ITAGIO(J)=0
         ENDDO
         DO J=1,NINOUT
            ISUIO=IFLOW(IADI+NIFLOW+NNO+3*NEL+NIIOFLOW*(J-1)+1)
            NN=IGRSURF(ISUIO)%NSEG
            DO K=1,NN
               NG1=IGRSURF(ISUIO)%NODES(K,1)
               NG2=IGRSURF(ISUIO)%NODES(K,2)
               NG3=IGRSURF(ISUIO)%NODES(K,3)
               NG4=IGRSURF(ISUIO)%NODES(K,4)
               ISH34=IGRSURF(ISUIO)%ELTYP(K)
               N1=ITABINV(NG1)
               N2=ITABINV(NG2)
               N3=ITABINV(NG3)
               ITAGIO(N1)=J
               ITAGIO(N2)=J
               ITAGIO(N3)=J
               IF (ISH34==3) THEN
                  N4=ITABINV(NG4)
                  ITAGIO(N4)=J
               ENDIF
            ENDDO
         ENDDO
C     
         DO J=1,NEL
            N1=IFLOW(IADI+NIFLOW+NNO+3*(J-1)+1)
            N2=IFLOW(IADI+NIFLOW+NNO+3*(J-1)+2)
            N3=IFLOW(IADI+NIFLOW+NNO+3*(J-1)+3)
            PROD=ITAGIO(N1)*ITAGIO(N2)*ITAGIO(N3)
            IFLOW(IADI+NIFLOW+NNO+3*NEL+NINOUT*NIIOFLOW+NNN+J)=0
            IF (PROD/=0) 
     .           IFLOW(IADI+NIFLOW+NNO+3*NEL+NINOUT*NIIOFLOW+NNN+J)=
     .           MAX(ITAGIO(N1),ITAGIO(N2),ITAGIO(N3))
         ENDDO 
C     
         CALL HM_GET_INTV('Iform', IFORM, IS_AVAILABLE, LSUBMODEL)
         CALL HM_GET_INTV('Ipri', ILVOUT, IS_AVAILABLE, LSUBMODEL)
         CALL HM_GET_FLOATV('Dtflow', DTSUB, IS_AVAILABLE, LSUBMODEL, UNITAB)

         
         IF (IFORM==0) IFORM=1
C     
         NBLOC=0
         NPROW=1
         NPCOL=1
         IF (NSPMD > 1) THEN
            RNSPMD=NSPMD
            NRMAX=INT(SQRT(RNSPMD))
            IF (IFORM==1) THEN
               DO NR=1,NRMAX
                  IF (MOD(NSPMD,NR)==0) THEN
                     NPROW=NR
                     NPCOL=NSPMD/NR
                  ENDIF
               ENDDO
            ELSEIF (IFORM==2) THEN
               NPROW=NSPMD
               NPCOL=1
            ENDIF
            IF (NNO<1000) THEN
               NBLOCMAX=32
            ELSE
               NBLOCMAX=64
            ENDIF
            NBLOC=MIN(NNO/NPROW, NNO/NPCOL)
            NBLOC=MIN(NBLOCMAX,NBLOC)
            NBLOC=MAX(1,NBLOC)
         ENDIF
C     
         IFVINI=0
         SFVINI=ZERO
         SCALT_VI=ZERO
         DIRX=ZERO
         DIRY=ZERO
         DIRZ=ZERO
         IF (IVINI==1) THEN
            CALL HM_GET_INTV('Ifvinf', IFVINI, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_FLOATV('Fscalevel', SFVINI, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOATV('Ascalevel', SCALT_VI, IS_AVAILABLE, LSUBMODEL, UNITAB)
            
            CALL HM_GET_FLOATV('Dirx', DIRX, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOATV('Diry', DIRY, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOATV('Dirz', DIRZ, IS_AVAILABLE, LSUBMODEL, UNITAB)
            
            IFUNC=0
            DO J=1,NFUNCT
               IF (IFVINI==NPC(J)) IFUNC=J
            ENDDO
            IF (IFUNC==0) THEN
               CALL ANCMSG(MSGID=621,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=ID,
     .              C1=TITR,
     .              C2='FUNCTION',
     .              I2=IFVINI)
            ENDIF
            IFVINI=IFUNC
            NORM=SQRT(DIRX**2+DIRY**2+DIRZ**2)
            IF (NORM==ZERO) THEN
               CALL ANCMSG(MSGID=622,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=ID,
     .              C1=TITR,
     .              C2='NULL VELOCITY DIRECTION VECTOR')
            ENDIF
            DIRX=DIRX/NORM
            DIRY=DIRY/NORM
            DIRZ=DIRZ/NORM
         ENDIF
C     
         IFLOW(IADI+1)=ID
         IFLOW(IADI+2)=ITYP
         IFLOW(IADI+3)=ISU
         IFLOW(IADI+4)=NINOUT
         IFLOW(IADI+5)=NNO
         IFLOW(IADI+6)=NEL
         IFLOW(IADI+7)=NNN           
         IFLOW(IADI+8)=NNO
         IFLOW(IADI+9)=NNO*NNO+NNO*(NEL+1)
         IFLOW(IADI+10)=IADMATI
         IFLOW(IADI+11)=IADMATR
         IFLOW(IADI+12)=NBLOC
         IFLOW(IADI+13)=IFORM
         IF (NSPMD == 1) THEN
            IFLOW(IADI+14)=NIFLOW+NNO+3*NEL+NINOUT*NIIOFLOW+NNN+NEL+NNO+NNN
         ELSE
            IFLOW(IADI+14)=NIFLOW+NNO+3*NEL+NINOUT*NIIOFLOW+NNN+NEL+4*NNO+2*NNN+2*NEL
         ENDIF
         IFLOW(IADI+15)=NRFLOW+5*(NNO+NNN)+NINOUT*NRIOFLOW
         IFLOW(IADI+17)=ILVOUT
         IFLOW(IADI+18)=NPROW
         IFLOW(IADI+19)=NPCOL
         IFLOW(IADI+20)=IADMATR+NNO*NNO
         IFLOW(IADI+21)=ITEST
         IFLOW(IADI+23)=IFPA
         IFLOW(IADI+24)=IFVINI
         RFLOW(IADR+1)=SFPA
         RFLOW(IADR+2)=SCALT_PA
         RFLOW(IADR+3)=DTSUB
         RFLOW(IADR+4)=ZERO
         RFLOW(IADR+5)=RHO
         RFLOW(IADR+6)=TOLE
         RFLOW(IADR+7)=SFVINI
         RFLOW(IADR+8)=SCALT_VI
         RFLOW(IADR+9)=DIRX
         RFLOW(IADR+10)=DIRY
         RFLOW(IADR+11)=DIRZ
         RFLOW(IADR+12)=ZERO
C     
         MEMFLOW(1)=MEMFLOW(1)+IFLOW(IADI+8)
         MEMFLOW(2)=MEMFLOW(2)+IFLOW(IADI+9)
C     PRINTOUTS
         WRITE(IOUT,1000)
         WRITE(IOUT,1100) I, ID, TRIM(TITR),IGRSURF(ISU)%ID, NNO, NEL
         IF (IINSIDE==1) THEN
            WRITE(IOUT,1110)
         ELSEIF (IINSIDE==2) THEN
            WRITE(IOUT,1120)
         ENDIF
         WRITE(IOUT,1200) IFPA, SFPA, SCALT_PA, III, NNN
         IF (ITEST==1) THEN
            WRITE(IOUT,1210)
            WRITE(IOUT,1225) TOLE
         ELSEIF (ITEST==2) THEN
            WRITE(IOUT,1220)
            WRITE(IOUT,1225) TOLE
         ELSEIF (ITEST==0) THEN
            WRITE(IOUT,1230)
         ENDIF
         WRITE(IOUT,1300) RHO
         WRITE(IOUT,1400) NINOUT
         DO J=1,NINOUT
            ISUIO=IFLOW(IADI+NIFLOW+NNO+3*NEL+NIIOFLOW*(J-1)+1)
            IFVEL=IFLOW(IADI+NIFLOW+NNO+3*NEL+NIIOFLOW*(J-1)+3)
            SFVEL=RFLOW(IADR+NRFLOW+5*(NNO+NNN)+NRIOFLOW*(J-1)+1)
            IFPRES=IFLOW(IADI+NIFLOW+NNO+3*NEL+NIIOFLOW*(J-1)+4)
            SFPRES=RFLOW(IADR+NRFLOW+5*(NNO+NNN)+NRIOFLOW*(J-1)+2)
            SCALT=RFLOW(IADR+NRFLOW+5*(NNO+NNN)+NRIOFLOW*(J-1)+3)
            WRITE(IOUT,1410) J, IGRSURF(ISUIO)%ID
            IF (IFVEL>0) WRITE(IOUT,1420) IFVEL, SFVEL
            IF (IFPRES>0) WRITE(IOUT,1430) IFPRES, SFPRES
            WRITE(IOUT,1440) SCALT
         ENDDO
         WRITE(IOUT,1500) IFORM, ILVOUT, DTSUB
         IF (IVINI==1) WRITE(IOUT,1600) IFVINI, SFVINI, SCALT_VI,
     .        DIRX, DIRY, DIRZ
         IF (NSPMD > 1) WRITE(IOUT,1700) NPROW, NPCOL, NBLOC
C     
         IADR=IADR+IFLOW(IADI+15)
         IADI=IADI+IFLOW(IADI+14)
         IADMATI=IADMATI+NNO
         IADMATR=IADMATR+NNO*NNO+NNO*(NEL+1)

      ENDDO

      CALL HM_OPTION_START('/BEM/DAA')

      DO I=1,HM_NDAA
         CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_TITR = TITR, OPTION_ID = ID)
C     
         
         CALL HM_GET_INTV('surf_ID', II, IS_AVAILABLE, LSUBMODEL)
         CALL HM_GET_INTV('grav_ID', GRAV_ID, IS_AVAILABLE, LSUBMODEL)
         NELMAX=0
         CALL HM_GET_FLOATV('Rho', RHO, IS_AVAILABLE, LSUBMODEL, UNITAB)
         CALL HM_GET_FLOATV('C', SSP, IS_AVAILABLE, LSUBMODEL, UNITAB)
         CALL HM_GET_FLOATV('Pmin', PMIN, IS_AVAILABLE, LSUBMODEL, UNITAB)

         IF(PMIN == ZERO) PMIN=-EP30

         CALL HM_GET_FLOATV('Xs', XS, IS_AVAILABLE, LSUBMODEL, UNITAB)
         CALL HM_GET_FLOATV('Ys', YS, IS_AVAILABLE, LSUBMODEL, UNITAB)
         CALL HM_GET_FLOATV('Zs', ZS, IS_AVAILABLE, LSUBMODEL, UNITAB)
         
         CALL HM_GET_INTV('Iform', IFORM, IS_AVAILABLE, LSUBMODEL)
         CALL HM_GET_INTV('Ipri', ILVOUT, IS_AVAILABLE, LSUBMODEL)
         CALL HM_GET_INTV('Ipres', IPRES, IS_AVAILABLE, LSUBMODEL)
         CALL HM_GET_INTV('Kform', KFORM, IS_AVAILABLE, LSUBMODEL)
         CALL HM_GET_INTV('Freesurf', FREESURF, IS_AVAILABLE, LSUBMODEL)
         CALL HM_GET_INTV('Afterflow', AFTERFLOW, IS_AVAILABLE, LSUBMODEL)
         CALL HM_GET_INTV('Integr', INTEGR, IS_AVAILABLE, LSUBMODEL)
         ITYP = 3
         IF(IFORM == 0)  IFORM=1
         IF(ILVOUT == 0) ILVOUT=1
         IWAVE=1
         JFORM=2
         IF(KFORM == 0)  KFORM=1
         IF(INTEGR == 0) INTEGR=2
         IF(FREESURF  == 0) FREESURF=1
         IF(AFTERFLOW == 0) AFTERFLOW=2
         IF (IWAVE ==2 .AND. FREESURF == 2) THEN
            CALL ANCMSG(MSGID=1603,
     .           MSGTYPE=MSGERROR,
     .           ANMODE=ANINFO,
     .           I1=ID,
     .           C1=TITR,
     .           C2='FREE SURFACE IS NOT COMPATIBLE WITH PLANE WAVE')
         ENDIF
         IF(KFORM ==2) INTEGR=1
C     
         IF(NBGAUGE > 0 .AND. JFORM == 2) ALLOCATE(N_SHELL(NUMELC))
C     
         ISU=0
         DO J=1,NSURF
            IF (II==IGRSURF(J)%ID) ISU=J
         ENDDO
C     
         NN =IGRSURF(ISU)%NSEG
         DO J=1,NUMNOD
            ITAG(J)=0
         ENDDO
         DO J=1,NN
            J1=IGRSURF(ISU)%NODES(J,1)
            J2=IGRSURF(ISU)%NODES(J,2)
            J3=IGRSURF(ISU)%NODES(J,3)
            J4=IGRSURF(ISU)%NODES(J,4)
            ISH34=IGRSURF(ISU)%ELTYP(J)
            ITAG(J1)=1
            ITAG(J2)=1
            ITAG(J3)=1
            IF (ISH34==3) ITAG(J4)=1
         ENDDO
         NNO=0
         DO J=1,NUMNOD
            IF (ITAG(J)==1) THEN
               NNO=NNO+1
               IFLOW(IADI+NIFLOW+NNO)=J
               ITABINV(J)=NNO
            ENDIF
         ENDDO
         NEL=0
         DO J=1,NN
            J1=IGRSURF(ISU)%NODES(J,1)
            J2=IGRSURF(ISU)%NODES(J,2)
            J3=IGRSURF(ISU)%NODES(J,3)
            J4=IGRSURF(ISU)%NODES(J,4)
            ISH34=IGRSURF(ISU)%ELTYP(J)
            IF(JFORM == 1) THEN
               IF (ISH34==7) THEN
                  NEL=NEL+1
                  IAD2=IADI+NIFLOW+NNO+3*(NEL-1)
                  IFLOW(IAD2+1)=ITABINV(J1)
                  IFLOW(IAD2+2)=ITABINV(J2)
                  IFLOW(IAD2+3)=ITABINV(J3)
               ELSEIF (ISH34==3) THEN
                  NEL=NEL+1
                  IAD2=IADI+NIFLOW+NNO+3*(NEL-1)
                  IFLOW(IAD2+1)=ITABINV(J1)
                  IFLOW(IAD2+2)=ITABINV(J2)
                  IFLOW(IAD2+3)=ITABINV(J4)
                  NEL=NEL+1
                  IAD2=IADI+NIFLOW+NNO+3*(NEL-1)
                  IFLOW(IAD2+1)=ITABINV(J2)
                  IFLOW(IAD2+2)=ITABINV(J3)
                  IFLOW(IAD2+3)=ITABINV(J4)
               ENDIF
            ELSEIF(JFORM == 2) THEN
               IF (ISH34==7) THEN
                  NEL =NEL+1
                  IAD2=IADI+NIFLOW+NNO+5*(NEL-1)
                  IFLOW(IAD2+1)=ITABINV(J1)
                  IFLOW(IAD2+2)=ITABINV(J2)
                  IFLOW(IAD2+3)=ITABINV(J3)
                  IFLOW(IAD2+4)=ITABINV(J3)
                  IFLOW(IAD2+5)=2
                  IF(NBGAUGE > 0) N_SHELL(NEL)=0
               ELSEIF (ISH34==3) THEN
                  NEL =NEL+1
                  IAD2=IADI+NIFLOW+NNO+5*(NEL-1)
                  IFLOW(IAD2+1)=ITABINV(J1)
                  IFLOW(IAD2+2)=ITABINV(J2)
                  IFLOW(IAD2+3)=ITABINV(J3)
                  IFLOW(IAD2+4)=ITABINV(J4)
                  IFLOW(IAD2+5)=1
                  IF(NBGAUGE > 0) N_SHELL(NEL)=IGRSURF(ISU)%ELEM(J)
               ENDIF
            ENDIF
         ENDDO
C     
         PMAX = ZERO
         THETA = ZERO
         APMAX = ZERO
         ATHETA = ZERO
         IFPRES = 0
         SFPRES = ZERO
         IF(IPRES == 1)THEN
            CALL HM_GET_FLOATV('Pm', PMAX, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOATV('Theta', THETA, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOATV('a', APMAX, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOATV('aTheta', ATHETA, IS_AVAILABLE, LSUBMODEL, UNITAB)

            IF(APMAX == ZERO) APMAX = ONE
         ELSEIF(IPRES == 2) THEN
C     Initialization 
            PMAX = EP30
            THETA = EP30
            CALL HM_GET_INTV('fct_IDP', IFPRES, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_FLOATV('FscaleP', SFPRES, IS_AVAILABLE, LSUBMODEL, UNITAB)
            IF(SFPRES == ZERO) THEN
               CALL HM_GET_FLOATV_DIM('FscaleP', FAC_GEN, IS_AVAILABLE, LSUBMODEL, UNITAB)
               SFPRES = ONE * FAC_GEN
            ENDIF
            IF (IFPRES/=0) THEN
               IFUNC=0
               DO K=1,NFUNCT
                  IF (IFPRES==NPC(K)) IFUNC=K
               ENDDO
               IF (IFUNC==0) THEN
                  CALL ANCMSG(MSGID=621,MSGTYPE=MSGERROR,ANMODE=ANINFO,
     .                 I1=ID,C1=TITR,C2='FUNCTION',I2=IFPRES)
               ENDIF
            ENDIF
         ELSEIF(IPRES == 3) THEN

         ELSEIF(IPRES == 4) THEN
         ENDIF
         
         CALL HM_GET_FLOATV('Xc', XC, IS_AVAILABLE, LSUBMODEL, UNITAB)
         CALL HM_GET_FLOATV('Yc', YC, IS_AVAILABLE, LSUBMODEL, UNITAB)
         CALL HM_GET_FLOATV('Zc', ZC, IS_AVAILABLE, LSUBMODEL, UNITAB)

         
         

         XD=ZERO
         YD=ZERO
         ZD=ZERO         
         XA=ZERO
         YA=ZERO
         ZA=ZERO 
         DIRX=ZERO			
         DIRY=ZERO			
         DIRZ=ZERO   
         IF(FREESURF == 2 .OR. GRAV_ID > 0)THEN
            CALL HM_GET_FLOATV('XA', XA, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOATV('YA', YA, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOATV('ZA', ZA, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOATV('Dir-X', DIRX, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOATV('Dir-Y', DIRY, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOATV('Dir-Z', DIRZ, IS_AVAILABLE, LSUBMODEL, UNITAB)              			
            NORM=SQRT(DIRX**2+DIRY**2+DIRZ**2)
            IF (NORM==ZERO) THEN
               CALL ANCMSG(MSGID=622,
     .              MSGTYPE=MSGERROR,ANMODE=ANINFO,
     .              I1=ID,C1=TITR,C2='NULL FREE SURFACE NORMAL')
            ENDIF
            DIRX=DIRX/NORM
            DIRY=DIRY/NORM
            DIRZ=DIRZ/NORM
C     Compute Image D of Charge C
            IF(FREESURF == 2) THEN
               TT = DIRX*(XC-XA)+DIRY*(YC-YA)+DIRZ*(ZC-ZA)
               XD = XC - TWO*TT*DIRX
               YD = YC - TWO*TT*DIRY
               ZD = ZC - TWO*TT*DIRZ
            ENDIF
         ENDIF

         NBLOC=0
         NPROW=1
         NPCOL=1
         IF (NSPMD > 1) THEN
            RNSPMD=NSPMD
            NRMAX=INT(SQRT(RNSPMD))
            DO NR=1,NRMAX
               IF (MOD(NSPMD,NR)==0) THEN
                  NPROW=NR
                  NPCOL=NSPMD/NR
               ENDIF
            ENDDO
            IF (NEL<1000) THEN
               NBLOCMAX=32
            ELSE
               NBLOCMAX=64
            ENDIF
            NBLOC=MIN(NEL/NPROW, NEL/NPCOL)
            NBLOC=MIN(NBLOCMAX,NBLOC)
            NBLOC=MAX(1,NBLOC)
         ENDIF
C     
         IFLOW(IADI+1)=ID
         IFLOW(IADI+2)=ITYP
         IFLOW(IADI+3)=ISU
         IFLOW(IADI+4)=JFORM
         IFLOW(IADI+5)=NNO
         IFLOW(IADI+6)=NEL
         IFLOW(IADI+7)=IFUNC
         IFLOW(IADI+8)=0
         IFLOW(IADI+9)=0
         IFLOW(IADI+10)=IADMATI
         IFLOW(IADI+11)=IADMATR
         IFLOW(IADI+12)=NBLOC
         IFLOW(IADI+13)=IFORM
         IF(NSPMD == 1) THEN
            IF(JFORM == 1) THEN
               IFLOW(IADI+14)=NIFLOW+NNO+3*NEL+NNO
            ELSEIF(JFORM == 2) THEN
               IFLOW(IADI+14)=NIFLOW+NNO+5*NEL+NNO+NBGAUGE
            ENDIF
         ELSE
            IF(JFORM == 1) THEN
               IFLOW(IADI+14)=NIFLOW+NNO+3*NEL+NNO+NNO+NEL
            ELSEIF(JFORM == 2) THEN
               IFLOW(IADI+14)=NIFLOW+NNO+5*NEL+NNO+NBGAUGE+NNO+NEL
            ENDIF
         ENDIF
         HG = HUGE(NEL)
         IF (NEL > INT(SQRT(REAL(HG)))) THEN
            CALL ANCMSG(MSGID = 1711, ANMODE=ANINFO, MSGTYPE = MSGERROR, 
     .           I1 = INT(SQRT(REAL(HG))))
            CALL ARRET(2)
         ENDIF
         IF(FREESURF == 1)IFLOW(IADI+15)=NRFLOW+ 7*NEL+NEL*NEL+3*NEL
         IF(FREESURF == 2)IFLOW(IADI+15)=NRFLOW+10*NEL+NEL*NEL+3*NEL
         IFLOW(IADI+17)=ILVOUT
         IFLOW(IADI+18)=NPROW
         IFLOW(IADI+19)=NPCOL
         IFLOW(IADI+20)=0
         IFLOW(IADI+21)=IPRES
         IFLOW(IADI+22)=IWAVE
         IFLOW(IADI+23)=KFORM
         IFLOW(IADI+24)=INTEGR
         IFLOW(IADI+25)=FREESURF
         IFLOW(IADI+26)=AFTERFLOW
         K=0
         IF(GRAV_ID > 0) THEN
            DO J=1,NGRAV
               IF(IGRV(5,J) == GRAV_ID) K=J
            ENDDO
         ENDIF
         IFLOW(IADI+27)=K
         IFLOW(IADI+28)=NELMAX

         RFLOW(IADR+1)=RHO*SSP
         RFLOW(IADR+2)=SSP
         RFLOW(IADR+3)=SFPRES
         RFLOW(IADR+5)=RHO
         RFLOW(IADR+6)=PMAX
         RFLOW(IADR+7)=THETA
         RFLOW(IADR+8)=-ONE
         RFLOW(IADR+9) =XC
         RFLOW(IADR+10)=YC
         RFLOW(IADR+11)=ZC
         RFLOW(IADR+12)=SQRT((XS-XC)**2+(YS-YC)**2+(ZS-ZC)**2)
         RFLOW(IADR+13)=XD
         RFLOW(IADR+14)=YD
         RFLOW(IADR+15)=ZD
         
         RFLOW(IADR+16)=XA
         RFLOW(IADR+17)=YA
         RFLOW(IADR+18)=ZA
         RFLOW(IADR+19)=DIRX
         RFLOW(IADR+20)=DIRY
         RFLOW(IADR+21)=DIRZ
         RFLOW(IADR+22)=PMIN
         RFLOW(IADR+23)=APMAX
         RFLOW(IADR+24)=ATHETA

         MEMFLOW(1)=MEMFLOW(1)+IFLOW(IADI+8)
         MEMFLOW(2)=MEMFLOW(2)+IFLOW(IADI+9)
C     
         WRITE(IOUT,2000)
         IF(JFORM == 1) WRITE(IOUT,2100) I, ID, TRIM(TITR), IGRSURF(ISU)%ID, NNO, NEL, GRAV_ID
         IF(JFORM == 2) WRITE(IOUT,2200) I, ID, TRIM(TITR), IGRSURF(ISU)%ID, NNO, NEL, GRAV_ID
         WRITE(IOUT,2300) RHO, SSP, PMIN
         WRITE(IOUT,2400) XS, YS, ZS
         WRITE(IOUT,2500) IFORM, ILVOUT, IPRES, KFORM, FREESURF, AFTERFLOW, INTEGR
         IF(IPRES == 1)THEN
            WRITE(IOUT,2600) PMAX, THETA, APMAX, ATHETA
         ELSEIF(IPRES == 2) THEN
            WRITE(IOUT,2700) IFPRES,SFPRES
         ENDIF           
         IF(IWAVE == 1) WRITE(IOUT,3000) XC, YC, ZC
         IF(GRAV_ID > 0 .OR. FREESURF == 2) WRITE(IOUT,3500) XA,YA,ZA,DIRX,DIRY,DIRZ
         IF(FREESURF == 2) WRITE(IOUT,3600) XD,YD,ZD

C-----------------------------------------------------------------------------------
C     Compute arrival time of incident wave, area, direction cosine, distance to charge
C     Compute fluid mass matrix
C-----------------------------------------------------------------------------------
C     Description des tableaux
C     II1  -> IFLOW    NIFLOW     : parametres entiers
C     II2  -> IBUF     NNO        : correspondances local-global noeuds surface
C     II3  -> ELEM     3 ou 5*NEL : connectivite locale des triangles/quadrangles+flag
C     II4  -> IBUF_L   NNO        : correspondances local-global noeuds surface
C     II5  -> SHELL_GA NBGAUGE    : correspondances local-gauge 4-node shell
C     II6  -> CNP      NNO        : nombre de processeur pour chaque noeud nspmd > 1
C     engine
C     II7  -> IPIV     NEL        : lapack resolution nel > nelmax
C     II8  -> IBUFELR  NEL        : SPMD ligne   des elements dans la process grid (0 si absent du processeur courant)
C     II9  -> IBUFELC  NEL        : SPMD colonne des elements dans la process grid (0 si absent du processeur courant)
C     
C     IR1  -> RFLOW  : parametres reels
C     IR2  -> NORMAL : normale
C     IR3  -> TA     : arrival time
C     IR4  -> AREA   : element area
C     IR5  -> COSG   : direction cosine
C     IR6  -> DIST   : distance charge
C     IR7  -> MFLE   : fluide mass matrix (inverse) if nel < nelmax
C     IR7  -> MFLE   : C**t B + B**t C if nel >= nelmax
C     engine
C     IR8  -> ACCF   : acceleration point fluide
C     IR9  -> PS     : scattered pressure
C     IR10 -> PTI    : incident pressure time integral
C     IR11 -> CBEM   : C matrix if nel >= nelmax
C-----------------------------------------------------------------------------------
         II1=IADI+1
         II2=II1+NIFLOW
         II3=II2+NNO
         IF(JFORM==1) THEN
            II4=II3+3*NEL
            II5=II4+NNO
            II6=II5
         ELSEIF(JFORM==2) THEN
            II4=II3+5*NEL
            II5=II4+NNO
            II6=II5+NBGAUGE
         ENDIF
         IR1=IADR+1
         IR2=IR1+NRFLOW
         IR3=IR2+NEL*3
         IR4=IR3+NEL*FREESURF
         IR5=IR4+NEL
         IR6=IR5+NEL*FREESURF
         IR7=IR6+NEL*FREESURF
         HG = HUGE(NEL)
         IF (NEL > INT(SQRT(REAL(HG)))) THEN
            CALL ANCMSG(MSGID = 1711, ANMODE=ANINFO, MSGTYPE = MSGERROR, 
     .           I1 = INT(SQRT(REAL(HG))))
            CALL ARRET(2)
         ENDIF
         IR8=IR7+NEL*NEL
         IR9=IR8+NEL
         IR10=IR9+NEL
         IR11=IR10+NEL
         IF(JFORM == 1) THEN
            CALL INIT_TG(IFLOW(II1), IFLOW(II2), IFLOW(II3), X, XS, YS, ZS, XD, YD, ZD,
     .           RFLOW(IR1), RFLOW(IR2), RFLOW(IR3), RFLOW(IR4), RFLOW(IR5), RFLOW(IR6))
            CALL MASS_FLUID_TG(IFORM, ILVOUT, NNO, NEL, IFLOW(II2), IFLOW(II3), X, 
     .           RFLOW(IR2), RFLOW(IR4), RFLOW(IR7), RHO)
         ELSEIF(JFORM == 2) THEN
            CALL INIT_QD(IFLOW(II1), IFLOW(II2), IFLOW(II3), X, XS, YS, ZS, XD, YD, ZD,
     .           RFLOW(IR1), RFLOW(IR2), RFLOW(IR3), RFLOW(IR4), RFLOW(IR5), RFLOW(IR6))
            IF(NEL < NELMAX) THEN
               ALLOCATE(CBEM(NEL,NEL))
               CALL MASS_FLUID_QD(NNO, NEL, IFLOW(II1), IFLOW(II2), IFLOW(II3), X,
     .              RFLOW(IR2), RFLOW(IR4), RFLOW(IR7), CBEM, RHO)
               DEALLOCATE(CBEM)
            ELSE
               CALL MASS_FLUID_QD(NNO, NEL, IFLOW(II1), IFLOW(II2), IFLOW(II3),  X,
     .              RFLOW(IR2), RFLOW(IR4), RFLOW(IR7), RFLOW(IR11), RHO)
            ENDIF
            IF(NBGAUGE > 0) THEN
               WRITE (IOUT,'(/5X,A)') 'GAUGE   ELEMENT   ELEMENT'
               DO J=1,NBGAUGE
                  IFLOW(II5+J-1)=0                    
                  IF(LGAUGE(1,J) /= 0) CYCLE
C     Node or Shell input
                  N1=LGAUGE(3,J)
                  IF(N1 < 0) THEN
                     J1=-N1
                     DO K=1,NEL
                        IF(J1 /= N_SHELL(K)) CYCLE
                        IFLOW(II5+J-1)=K
                        GO TO 100               
                     ENDDO
                  ENDIF
 100              CONTINUE
                  WRITE(IOUT,'(3I10)') J,-LGAUGE(3,J),IFLOW(II5+J-1)
                  ENDDO
                  DEALLOCATE(N_SHELL)
               ENDIF
            ENDIF
C
            IADR=IADR+IFLOW(IADI+15)
            IADI=IADI+IFLOW(IADI+14)
C
         ENDDO
C
         RETURN
 999  CALL FREERR(1)
      RETURN
C
 1000 FORMAT(/
     .       /
     . '       INCOMPRESSIBLE FLOW (BOUNDARY ELEMENTS METHOD)'/
     . '       ----------------------------------------------'/)
 1100 FORMAT(  5X,'BEM PROBLEM NUMBER ',I10
     .       /10X,'FLOW ID ',I10,1X,A,
     .       /10X,'EXTERNAL SURFACE ID                     ',I10
     .       /10X,'NUMBER OF SURFACE NODES                 ',I10
     .       /10X,'NUMBER OF TRIANGULAR BOUNDARY ELEMENTS  ',I10)
 1110 FORMAT( 10X,'FLOW INSIDE THE SURFACE')
 1120 FORMAT( 10X,'FLOW OUTSIDE THE SURFACE')
 1200 FORMAT( 10X,'STAGNATION PRESSURE CURVE               ',I10
     .       /10X,'STAGNATION PRESSURE SCALE FACTOR        ',1PE10.3
     .       /10X,'TIME SCALE FACTOR FOR STAG. PRES. CURVE ',1PE10.3
     .       /10X,'AUXILIARY NODE GROUP ID                 ',I10
     .       /10X,'NUMBER OF AUXILIARY NODES               ',I10)
 1210 FORMAT( 10X,'POINT-INSIDE-SURFACE TEST FOR AUX. NODES')
 1220 FORMAT( 10X,'POINT-OUTSIDE-SURFACE TEST FOR AUX. NODES')
 1225 FORMAT( 10X,'ADIMENSIONAL TOLERANCE FOR TESTING      ',1PE10.3)
 1230 FORMAT( 10X,'NO TEST FOR AUX. NODES')
 1300 FORMAT( 10X,'FLUID DENSITY                           ',1PE10.3)
 1400 FORMAT(/10X,'INFLOW-OUTFLOW'
     .       /10X,'--------------'
     .       /10X,'NUMBER OF INFLOW-OUTFLOW SURFACES       ',I10)
 1410 FORMAT(/10X,'SURFACE NUMBER                          ',I10
     .       /10X,'SURFACE ID                              ',I10)
 1420 FORMAT( 10X,'IMPOSED VELOCITY CURVE                  ',I10
     .       /10X,'IMPOSED VELOCITY SCALE FACTOR           ',1PE10.3)
 1430 FORMAT( 10X,'IMPOSED PRESSURE CURVE                  ',I10
     .       /10X,'IMPOSED PRESSURE SCALE FACTOR           ',1PE10.3)
 1440 FORMAT( 10X,'TIME SCALE FACTOR FOR CURVES            ',1PE10.3)
 1500 FORMAT(/10X,'BEM PARAMETERS'
     .       /10X,'--------------'
     .       /10X,'BEM FORMULATION FLAG                    ',I10
     .       /10X,'BEM SOLVER OUTPUT LEVEL                 ',I10
     .       /10X,'TIME STEP FOR MATRICES ASSEMBLY         ',1PE10.3)
 1600 FORMAT(/10X,'VELOCITY FIELD AT INFINITY'
     .       /10X,'--------------------------'
     .       /10X,'VELOCITY CURVE                          ',I10
     .       /10X,'VELOCITY SCALE FACTOR                   ',1PE10.3
     .       /10X,'TIME SCALE FOR VELOCITY CURVE           ',1PE10.3
     .       /10X,'X COMPONENT OF VELOCITY VECTOR          ',1PE10.3
     .       /10X,'Y COMPONENT OF VELOCITY VECTOR          ',1PE10.3
     .       /10X,'Z COMPONENT OF VELOCITY VECTOR          ',1PE10.3)
 1700 FORMAT(/10X,'PARALLEL SOLVER PARAMETERS (SCALAPACK)'
     .       /10X,'--------------------------------------'
     .       /10X,'NUMBER OF ROW OF PROCESS GRID           ',I10
     .       /10X,'NUMBER OF COLUMNS OF PROCESS GRID       ',I10
     .       /10X,'2D-CYCLIC DECOMPOSITION BLOCK-SIZE      ',I10)

 2000 FORMAT(//
     . '     DAA SURFACE (BOUNDARY ELEMENT METHOD)   '/
     . '     --------------------------------------  '/)
 2100 FORMAT(  5X,'DAA SURFACE NUMBER ',I10
     .       /10X,'DAA ID ',I10,1X,A,
     .       /10X,'WET SURFACE ID                          ',I10
     .       /10X,'NUMBER OF SURFACE NODES                 ',I10
     .       /10X,'NUMBER OF TRIANGULAR ELEMENTS           ',I10
     .       /10X,'GRAVITY ID (/GRAV)                      ',I10)
 2200 FORMAT(  5X,'DAA SURFACE NUMBER ',I10
     .       /10X,'DAA ID ',I10,1X,A,
     .       /10X,'WET SURFACE ID                          ',I10
     .       /10X,'NUMBER OF SURFACE NODES                 ',I10
     .       /10X,'NUMBER OF SHELL ELEMENTS                ',I10
     .       /10X,'GRAVITY ID (/GRAV)                      ',I10)
 2300 FORMAT( 10X,'FLUID DENSITY                           ',1PE13.6
     .       /10X,'FLUID SOUND SPEED                       ',1PE13.6
     .       /10X,'MINIMUM PRESSURE                        ',1PE13.6)
 2400 FORMAT( 10X,'X-COORDINATE OF STANDOFF POINT          ',1PE13.6
     .       /10X,'Y-COORDINATE OF STANDOFF POINT          ',1PE13.6
     .       /10X,'Z-COORDINATE OF STANDOFF POINT          ',1PE13.6)
 2500 FORMAT(/10X,'BEM FORMULATION FLAG IFORM              ',I10
     .       /10X,'DAA SOLVER OUTPUT LEVEL                 ',I10
     .       /10X,'INCIDENT PRESSURE INPUT FLAG            ',I10
     .       /10X,'DAA FORMULATION FLAG KFORM              ',I10
     .       /10X,'FREE SURFACE FLAG                       ',I10
     .       /10X,'AFTERFLOW VELOCITY FLAG                 ',I10
     .       /10X,'INTEGRATION FLAG                        ',I10)
 2600 FORMAT(/10X,'MAXIMUM PRESSURE AT STANDOFF POINT      ',1PE13.6
     .       /10X,'DECAY TIME AT STANDOFF POINT            ',1PE13.6
     .       /10X,'EXPONENT FOR PMAX (APMAX)               ',1PE13.6
     .       /10X,'EXPONENT FOR DECAY TIME (ATHETA)        ',1PE13.6)
 2700 FORMAT(/10X,'INCIDENT PRESSURE FUNCTION              ',I10
     .       /10X,'PRESSURE SCALE FACTOR                   ',1PE13.6)
 3000 FORMAT( 10X,'X-COORDINATE OF EXPLOSIVE CHARGE        ',1PE13.6
     .       /10X,'Y-COORDINATE OF EXPLOSIVE CHARGE        ',1PE13.6
     .       /10X,'Z-COORDINATE OF EXPLOSIVE CHARGE        ',1PE13.6)
 3100 FORMAT(/10X,'PLANE WAVE DIRECTION                    '
     .       /10X,'X-DIRECTION                             ',1PE13.6
     .       /10X,'Y-DIRECTION                             ',1PE13.6
     .       /10X,'Z-DIRECTION                             ',1PE13.6)
 3500 FORMAT(/10X,'FREE SURFACE                    '
     .       /10X,'X-COORDINATE OF SURFACE POINT A         ',1PE13.6
     .       /10X,'Y-COORDINATE OF SURFACE POINT A         ',1PE13.6
     .       /10X,'Z-COORDINATE OF SURFACE POINT A         ',1PE13.6
     .       /10X,'SURFACE NORMAL X-COMPONENT              ',1PE13.6
     .       /10X,'SURFACE NORMAL Y-COMPONENT              ',1PE13.6
     .       /10X,'SURFACE NORMAL Z-COMPONENT              ',1PE13.6)
 3600 FORMAT(/10X,'X-COORDINATE OF CHARGE IMAGE            ',1PE13.6
     .       /10X,'Y-COORDINATE OF CHARGE IMAGE            ',1PE13.6
     .       /10X,'Z-COORDINATE OF CHARGE IMAGE            ',1PE13.6)
      END
            
