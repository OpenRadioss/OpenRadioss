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
Chd|  HM_READ_SPHIO                 source/loads/sph/hm_read_sphio.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        UDOUBLE                       source/system/sysfus.F        
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_SPHIO(ISPHIO   ,VSPHIO   ,IPART    ,IGRSURF  ,
     .                         NOD2SP   ,IPARTSP  ,ITAB     ,X        ,
     .                         MFI      ,LWASPIO  ,ITABM1   ,UNITAB   ,
     .                         LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE R2R_MOD
      USE GROUPDEF_MOD
      USE HM_OPTION_READ_MOD
      USE SUBMODEL_MOD    
      USE UNITAB_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "sphcom.inc"
#include      "units_c.inc"
#include      "scr17_c.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISPHIO(NISPHIO,*), IPART(LIPART1,*),
     .        NOD2SP(*),IPARTSP(*),
     .        ITAB(*),MFI,LWASPIO,ITABM1(*)
      my_real
     .   VSPHIO(*),X(3,*)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,  N, ID, J, IDS, IDPRT, IPRT, INOD, ICEL,
     .  IPROP, ISU, NSEG,
     .  MFITMP, IVAD, LVAD, ITYPE, IDSURF, IFTEMP,
     .  IFVITS,IFDENS, IFPRES, IFENER,SKIP,
     .  IAD,IN1,IN2,IN3,IN4, IUN,NBOX,NBOY,NBOZ,NBAND
      CHARACTER MESS*40,TITR*nchartitle,KEY*ncharkey
      my_real
     .   BID,RHOIN,PIN,EIN,DIST,
     .   X1,X2,X3,X4,Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,
     .   X12,Y12,Z12,X13,Y13,Z13,NN,NX,NY,NZ,
     .   DBUCS,XBMIN,YBMIN,ZBMIN,XBMAX,YBMAX,ZBMAX,
     .   PINFINI,CARL,XX(9),FCUT,
     .   RHOIN_UNIT,PIN_UNIT,EIN_UNIT
      LOGICAL IS_AVAILABLE,FOUND
C-----------------------------------------------
      DATA MESS/'SPH INLET/OUTLET DEFINITION             '/
      DATA IUN/1/
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS   
      my_real
     .         GET_U_GEO
      EXTERNAL GET_U_GEO
C-----------------------------------------------
C                                                      TYPE=1    TYPE=2   TYPE=3   TYPE=4
C   ISPHIO(1,N) = CONDITION TYPE                          x         x        x        x
C      1: INLET  2:GENERAL OUTLET  3:CONTINUE OUTLET
C   ISPHIO(2,N) = NODES GROUP RELATED TO THE CONDITION    x         x        x        x
C   ISPHIO(3,N) = SURFACE                                 x         x        x
C   ISPHIO(4,N) =                                         x         x        x
C                 IVAD : INDEX OF REAL CORRESPONDING ARRAY INTO VSPHIO
C   ISPHIO(5,N) = DENSITY FUNCTION NUMBER                 x         x                   
C   ISPHIO(6,N) = PRESSURE FUNCTION NUMBER                          x        x          
C   ISPHIO(7,N) = ENERGY FUNCTION NUMBER                  x         x                   
C   ISPHIO(8,N) = NORMAL VELOCITY FUNCTION NUMBER         x
C   ISPHIO(9,N) = CONDITION IDENTIFIER                    x         x        x        x
C   ISPHIO(10,N) = NB SEG IO                              x         x        x
C   ISPHIO(11,N) = IAD for // outlet                      x         x        x        x
C   ISPHIO(12,N) = type of input                                    x        x        x
C   ISPHIO(13,N) = id of nod1                                       x        x        x
C   ISPHIO(14,N) = id of nod2                                       x        x        x
C   ISPHIO(15,N) = id of nod3                                       x        x        x
C-----------------------------------------------
C                                                      TYPE=1    TYPE=2   TYPE=3   TYPE=4
C   VSPHIO(IVAD  ) = RHOIN                                 x         
C   VSPHIO(IVAD+1) = PIN                                            x        x        x
C   VSPHIO(IVAD+2) = EIN                                   x         
C   VSPHIO(IVAD+3) = WITHIN/WITHOUT DISTANCE               x        x        x        x      
C
Case TYPE=1
C   VSPHIO(IVAD+4+2*J,J=0,NSEG-1) =                        x
C          MASS FLOW THROUGH SEGMENT J SINCE LAST ENTRY
C   VSPHIO(IVAD+5+2*J,J=0,NSEG-1) = SEGMENTS WIDTH         x                       
Case TYPE=2,3,4 control section + new outlet / silent boundary (if surface defined by coordinates)                          
C   VSPHIO(IVAD+4->6) = X1                                          x        x        x                           
C   VSPHIO(IVAD+7->9) = X2                                          x        x        x                          
C   VSPHIO(IVAD+10->12) = X3                                        x        x        x
Case TYPE=2,3,4 control section + new outlet / silent boundary
C   VSPHIO(IVAD+13) = Total mass that cross section                 x        x        x
C   VSPHIO(IVAD+14) = DT12                                          x        x        x
C   VSPHIO(IVAD+15) = Fcut Butterworth filter                       x        x        x
C   VSPHIO(IVAD+16) = Filtered mass that cross section              x        x        x
C   VSPHIO(IVAD+17->21) = Butterworth data                          x        x        x                            
C-----------------------------------------------
      LWASPIO = 0
      NSEG_IO = 0
      IS_AVAILABLE = .FALSE.
C
      MFITMP = MFI
      IVAD = 1
C
      ! Start reading the option
      CALL HM_OPTION_START('/SPH/INOUT') 
C
      ! Loop over /SPH/INOUT
      I = 0
      DO N = 1,NSPHIO
        SKIP = 0
CC----------Multidomaines --> on ignore les inlets non tages------------
        IF (NSUBDOM > 0) THEN
          IF (TAGSPHIO(N) == 0) SKIP = 1
        ENDIF
C----------------------------------------------------------------------
        IF (SKIP == 0) THEN
C
          ! Title and ID
          TITR = ''   
          CALL HM_OPTION_READ_KEY(LSUBMODEL, 
     .                            OPTION_ID   = ID, 
     .                            OPTION_TITR = TITR) 
C        
          I = I+1
          ISPHIO(NISPHIO,I)=ID
C
          ! Read card 1
          CALL HM_GET_INTV('Itype'   ,ITYPE  ,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('pid'     ,IDPRT  ,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('SURF_ID' ,IDSURF ,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_FLOATV('DIST'  ,DIST   ,IS_AVAILABLE,LSUBMODEL, UNITAB)
          
          CALL HM_GET_INTV('node_ID1',IN1 ,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('node_ID2',IN2 ,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('node_ID3',IN3 ,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_FLOATV('Fcut'  ,FCUT   ,IS_AVAILABLE,LSUBMODEL, UNITAB)      
          ISPHIO(1,I) =ITYPE
C
          IDS = 0
          FOUND = .FALSE.
          DO J = 1,NPART
            IF (IPART(4,J) == IDPRT) THEN
              IDS = J
              FOUND = .TRUE.
              EXIT
            ENDIF
          ENDDO
          IF (.NOT.FOUND) THEN
            CALL ANCMSG(MSGID=437,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=IDPRT)
          ENDIF
          ISPHIO(2,I) = IDS
          ISPHIO(4,I) = IVAD
C
          IF (IDSURF > 0) THEN
C         input by surfid
            IDS=0
            FOUND = .FALSE.
            DO J=1,NSURF
              IF (IGRSURF(J)%ID == IDSURF) THEN
                IDS=J
                FOUND = .TRUE.
                EXIT
              ENDIF
            ENDDO
            IF (.NOT.FOUND) THEN
              CALL ANCMSG(MSGID=438,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=ID,
     .                C1=TITR,
     .                I2=IDSURF)
            ENDIF
            ISPHIO(3,I) =IDS
          ELSEIF ((IN1 == 0).AND.(IN2 == 0).AND.(IN3 == 0)) THEN
C         input by coordinates ->  ISPHIO(12,I) = 1
            ISPHIO(12,I) =1
            CALL HM_GET_FLOATV('XM'  ,XX(1)   ,IS_AVAILABLE,LSUBMODEL, UNITAB)   
            CALL HM_GET_FLOATV('YM'  ,XX(2)   ,IS_AVAILABLE,LSUBMODEL, UNITAB)  
            CALL HM_GET_FLOATV('ZM'  ,XX(3)   ,IS_AVAILABLE,LSUBMODEL, UNITAB)  
            CALL HM_GET_FLOATV('XM1' ,XX(4)   ,IS_AVAILABLE,LSUBMODEL, UNITAB)   
            CALL HM_GET_FLOATV('YM1' ,XX(5)   ,IS_AVAILABLE,LSUBMODEL, UNITAB)  
            CALL HM_GET_FLOATV('ZM1' ,XX(6)   ,IS_AVAILABLE,LSUBMODEL, UNITAB)              
            CALL HM_GET_FLOATV('XM2' ,XX(7)   ,IS_AVAILABLE,LSUBMODEL, UNITAB)   
            CALL HM_GET_FLOATV('YM2' ,XX(8)   ,IS_AVAILABLE,LSUBMODEL, UNITAB)  
            CALL HM_GET_FLOATV('ZM2' ,XX(9)   ,IS_AVAILABLE,LSUBMODEL, UNITAB)              
            DO J = 1,9
              VSPHIO(IVAD+3+J) = XX(J)
            ENDDO 
          ELSE
C         input by node ID ->  ISPHIO(12,I) = 2
            ISPHIO(12,I) = 2
            ISPHIO(13,I) = USR2SYS(IN1,ITABM1,MESS,ID)
            ISPHIO(14,I) = USR2SYS(IN2,ITABM1,MESS,ID)
            ISPHIO(15,I) = USR2SYS(IN3,ITABM1,MESS,ID)  
          ENDIF
C
          IF (ITYPE == 1) THEN
            NSEG=IGRSURF(IDS)%NSEG
            LVAD=4+2*NSEG
          ELSE
            LVAD=22
          ENDIF
C
          IF (ITYPE == 1)THEN
            CALL HM_GET_INTV('FUN_A1'  ,IFDENS ,IS_AVAILABLE,LSUBMODEL)         
            CALL HM_GET_FLOATV('R0k0'  ,RHOIN  ,IS_AVAILABLE,LSUBMODEL,UNITAB) 
            IF ((RHOIN == ZERO).AND.(IFDENS>0)) THEN
              CALL HM_GET_FLOATV_DIM('R0k0' ,RHOIN_UNIT ,IS_AVAILABLE, LSUBMODEL, UNITAB)
              RHOIN = ONE * RHOIN_UNIT
            ENDIF
            CALL HM_GET_INTV('FUN_A6'  ,IFENER ,IS_AVAILABLE,LSUBMODEL)         
            CALL HM_GET_FLOATV('MAT_E0',EIN    ,IS_AVAILABLE,LSUBMODEL,UNITAB)  
            IF ((EIN == ZERO).AND.(IFENER>0)) THEN
              CALL HM_GET_FLOATV_DIM('MAT_E0'  ,EIN_UNIT ,IS_AVAILABLE, LSUBMODEL, UNITAB)
              EIN = ONE * EIN_UNIT
            ENDIF
            CALL HM_GET_INTV('FUN_A3'  ,IFVITS ,IS_AVAILABLE,LSUBMODEL)    
          ELSEIF (ITYPE == 2) THEN
            CALL HM_GET_INTV('FUN_A2'   ,IFPRES ,IS_AVAILABLE,LSUBMODEL)         
            CALL HM_GET_FLOATV('MAT_P0' ,PIN    ,IS_AVAILABLE,LSUBMODEL,UNITAB) 
            IF ((PIN == ZERO).AND.(IFPRES>0)) THEN
              CALL HM_GET_FLOATV_DIM('MAT_P0'  ,PIN_UNIT ,IS_AVAILABLE, LSUBMODEL, UNITAB)
              PIN = ONE * PIN_UNIT
            ENDIF
          ELSEIF (ITYPE == 3) THEN
            CALL HM_GET_INTV('FUN_A4'   ,IFPRES ,IS_AVAILABLE,LSUBMODEL)         
            CALL HM_GET_FLOATV('MAT_PScale',PIN ,IS_AVAILABLE,LSUBMODEL, UNITAB) 
            IF ((PIN == ZERO).AND.(IFPRES>0)) THEN
              CALL HM_GET_FLOATV_DIM('MAT_PScale' ,PIN_UNIT ,IS_AVAILABLE, LSUBMODEL, UNITAB)
              PIN = ONE * PIN_UNIT
            ENDIF
            CALL HM_GET_FLOATV('Lc'     ,CARL   ,IS_AVAILABLE,LSUBMODEL, UNITAB)     
          ENDIF
C
          IF (ITYPE == 1) THEN
           ISPHIO(5,I) = IFDENS
           ISPHIO(7,I) = IFENER
           ISPHIO(8,I) = IFVITS
          ELSEIF (ITYPE == 2) THEN
           ISPHIO(6,I) = IFPRES
          ELSEIF (ITYPE == 3) THEN
           ISPHIO(6,I) = IFPRES
          ENDIF
!
          IF (ITYPE == 1) THEN
            IF ((IFDENS /= 0).AND.(RHOIN == ZERO)) RHOIN = ONE
            IF ((IFENER /= 0).AND.(EIN == ZERO))   EIN   = ONE
            VSPHIO(IVAD  )  = RHOIN
            VSPHIO(IVAD+2)  = EIN
            VSPHIO(IVAD+3)  = DIST
          ELSEIF (ITYPE == 2) THEN
            VSPHIO(IVAD+1)  = PIN
            VSPHIO(IVAD+3)  = DIST
            VSPHIO(IVAD+15) = FCUT
          ELSEIF (ITYPE == 3) THEN
            VSPHIO(IVAD+1)  = PIN
            VSPHIO(IVAD+2)  = CARL
            VSPHIO(IVAD+3)  = DIST
            VSPHIO(IVAD+15) = FCUT
          ELSEIF (ITYPE == 4) THEN
            VSPHIO(IVAD+15) = FCUT
          ENDIF
          MFITMP = MFITMP+LVAD
          IVAD   = IVAD+LVAD
C----------------------------------------------------------------------
        ENDIF
      ENDDO
      MFI = MFITMP
C-------------------------------------
C Recherche des ID doubles
C-------------------------------------
      CALL UDOUBLE(ISPHIO(NISPHIO,1),NISPHIO,NSPHIO,MESS,0,BID)
C-------------------------------------
C inlets, pre-computes surface of segments.
C-------------------------------------
      DO N=1,NSPHIO
       IF(ISPHIO(1,N)==1)THEN
        IVAD=ISPHIO(4,N)+5
        ISU =ISPHIO(3,N)
        NSEG=IGRSURF(ISU)%NSEG
        DO J=1,NSEG
          IN1=IGRSURF(ISU)%NODES(J,1)
          IN2=IGRSURF(ISU)%NODES(J,2)
          IN3=IGRSURF(ISU)%NODES(J,3)
          IN4=IGRSURF(ISU)%NODES(J,4)
          X1=X(1,IN1)
          Y1=X(2,IN1)
          Z1=X(3,IN1)
          X2=X(1,IN2)
          Y2=X(2,IN2)
          Z2=X(3,IN2)
          X3=X(1,IN3)
          Y3=X(2,IN3)
          Z3=X(3,IN3)
          X12=X1-X2
          Y12=Y1-Y2
          Z12=Z1-Z2
          X13=X3-X2
          Y13=Y3-Y2
          Z13=Z3-Z2
          NX=Y12*Z13-Z12*Y13
          NY=Z12*X13-X12*Z13
          NZ=X12*Y13-Y12*X13
          NN =SQRT(NX*NX+NY*NY+NZ*NZ)
          IF(IN4/=IN3)THEN
           X4=X(1,IN4)
           Y4=X(2,IN4)
           Z4=X(3,IN4)
           X12=X1-X4
           Y12=Y1-Y4
           Z12=Z1-Z4
           X13=X3-X4
           Y13=Y3-Y4
           Z13=Z3-Z4
           NX=Y12*Z13-Z12*Y13
           NY=Z12*X13-X12*Z13
           NZ=X12*Y13-Y12*X13
           NN =NN+SQRT(NX*NX+NY*NY+NZ*NZ)
          ENDIF
          VSPHIO(IVAD)=HALF*NN
          IVAD=IVAD+2
        ENDDO
       ENDIF
      ENDDO
C-------------------------------------
C inlets, outlets : necessary work space (cf engine).
C-------------------------------------
      DO N=1,NSPHIO
        IF(ISPHIO(12,N)==0)THEN
        IVAD=ISPHIO(4,N)
        ISU =ISPHIO(3,N)
        NSEG=IGRSURF(ISU)%NSEG
        NSEG_IO = NSEG_IO + NSEG
        DIST =VSPHIO(IVAD+3)
        DBUCS=DIST
        XBMIN =1.E+20
        YBMIN =1.E+20
        ZBMIN =1.E+20
        XBMAX=-1.E+20
        YBMAX=-1.E+20
        ZBMAX=-1.E+20
        DO J=1,NSEG
          IN1=IGRSURF(ISU)%NODES(J,1)
          IN2=IGRSURF(ISU)%NODES(J,2)
          IN3=IGRSURF(ISU)%NODES(J,3)
          IN4=IGRSURF(ISU)%NODES(J,4)
          X1=X(1,IN1)
          Y1=X(2,IN1)
          Z1=X(3,IN1)
          X2=X(1,IN2)
          Y2=X(2,IN2)
          Z2=X(3,IN2)
          X3=X(1,IN3)
          Y3=X(2,IN3)
          Z3=X(3,IN3)
          XBMIN=MIN(XBMIN,X1)
          YBMIN=MIN(YBMIN,Y1)
          ZBMIN=MIN(ZBMIN,Z1)
          XBMAX=MAX(XBMAX,X1)
          YBMAX=MAX(YBMAX,Y1)
          ZBMAX=MAX(ZBMAX,Z1)
          XBMIN=MIN(XBMIN,X2)
          YBMIN=MIN(YBMIN,Y2)
          ZBMIN=MIN(ZBMIN,Z2)
          XBMAX=MAX(XBMAX,X2)
          YBMAX=MAX(YBMAX,Y2)
          ZBMAX=MAX(ZBMAX,Z2)
          XBMIN=MIN(XBMIN,X3)
          YBMIN=MIN(YBMIN,Y3)
          ZBMIN=MIN(ZBMIN,Z3)
          XBMAX=MAX(XBMAX,X3)
          YBMAX=MAX(YBMAX,Y3)
          ZBMAX=MAX(ZBMAX,Z3)
          DBUCS=MAX(DBUCS,ABS(X1-X2))
          DBUCS=MAX(DBUCS,ABS(Y1-Y2))
          DBUCS=MAX(DBUCS,ABS(Z1-Z2))
          DBUCS=MAX(DBUCS,ABS(X2-X3))
          DBUCS=MAX(DBUCS,ABS(Y2-Y3))
          DBUCS=MAX(DBUCS,ABS(Z2-Z3))
          DBUCS=MAX(DBUCS,ABS(X3-X1))
          DBUCS=MAX(DBUCS,ABS(Y3-Y1))
          DBUCS=MAX(DBUCS,ABS(Z3-Z1))
          IN4=IGRSURF(ISU)%NODES(J,4)
          IF(IN4/=IN3)THEN
           X4=X(1,IN4)
           Y4=X(2,IN4)
           Z4=X(3,IN4)
           XBMIN=MIN(XBMIN,X4)
           YBMIN=MIN(YBMIN,Y4)
           ZBMIN=MIN(ZBMIN,Z4)
           XBMAX=MAX(XBMAX,X4)
           YBMAX=MAX(YBMAX,Y4)
           ZBMAX=MAX(ZBMAX,Z4)
           DBUCS=MAX(DBUCS,ABS(X1-X4))
           DBUCS=MAX(DBUCS,ABS(Y1-Y4))
           DBUCS=MAX(DBUCS,ABS(Z1-Z4))
           DBUCS=MAX(DBUCS,ABS(X2-X4))
           DBUCS=MAX(DBUCS,ABS(Y2-Y4))
           DBUCS=MAX(DBUCS,ABS(Z2-Z4))
           DBUCS=MAX(DBUCS,ABS(X3-X4))
           DBUCS=MAX(DBUCS,ABS(Y3-Y4))
           DBUCS=MAX(DBUCS,ABS(Z3-Z4))
          ENDIF
        ENDDO
        XBMIN=XBMIN-DIST
        YBMIN=YBMIN-DIST
        ZBMIN=ZBMIN-DIST
        XBMAX=XBMAX+DIST
        YBMAX=YBMAX+DIST
        ZBMAX=ZBMAX+DIST
        NBOX =MAX(IUN,INT((XBMAX-XBMIN)/DBUCS))
        NBOY =MAX(IUN,INT((YBMAX-YBMIN)/DBUCS))
        NBOZ =MAX(IUN,INT((ZBMAX-ZBMIN)/DBUCS))
        NBAND=MAX(NBOX,NBOY,NBOZ)+1
        ELSE
          NBAND = 2
        ENDIF
        LWASPIO=MAX(LWASPIO,15*NUMSPH+6*(NBAND+1)+12*NSEG)
      ENDDO
      LWASPIO=MAX(LWASPIO,3*NSPHIO)
C-------------------------------------
C print
C-------------------------------------
      WRITE(IOUT,1000)
      DO N=1,NSPHIO
         IVAD=ISPHIO(4,N)
         IF(ISPHIO(1,N)==1)THEN
           WRITE(IOUT,1100) ISPHIO(NISPHIO,N),
     .     IPART(4,ISPHIO(2,N)),IGRSURF(ISPHIO(3,N))%ID,
     .     ISPHIO(5,N),VSPHIO(IVAD),ISPHIO(7,N),
     .     VSPHIO(IVAD+2),ISPHIO(8,N),VSPHIO(IVAD+3)
         ELSE
           IF(ISPHIO(1,N)==2)THEN
             IF (ISPHIO(12,N)==0) THEN
               WRITE(IOUT,1200) ISPHIO(NISPHIO,N),
     .         IPART(4,ISPHIO(2,N)),IGRSURF(ISPHIO(3,N))%ID,
     .         ISPHIO(6,N),VSPHIO(IVAD+1),VSPHIO(IVAD+3)
             ELSE
               WRITE(IOUT,1400) ISPHIO(NISPHIO,N),
     .         IPART(4,ISPHIO(2,N)),ISPHIO(6,N),VSPHIO(IVAD+1),VSPHIO(IVAD+3)
             ENDIF
           ELSEIF(ISPHIO(1,N)==3)THEN
             IF (ISPHIO(12,N)==0) THEN
               WRITE(IOUT,1300) ISPHIO(NISPHIO,N),
     .         IPART(4,ISPHIO(2,N)),IGRSURF(ISPHIO(3,N))%ID,VSPHIO(IVAD+3),
     .         ISPHIO(6,N),VSPHIO(IVAD+1),VSPHIO(IVAD+2)
             ELSEIF (ISPHIO(12,N)==1) THEN
               WRITE(IOUT,1500) ISPHIO(NISPHIO,N),IPART(4,ISPHIO(2,N)),
     .         ISPHIO(6,N),VSPHIO(IVAD+1),VSPHIO(IVAD+2)
             ENDIF
           ELSEIF(ISPHIO(1,N)==4)THEN
             IF (ISPHIO(12,N)==0) THEN
               WRITE(IOUT,1600) ISPHIO(NISPHIO,N),IPART(4,ISPHIO(2,N)),
     .         IGRSURF(ISPHIO(3,N))%ID
             ELSE
               WRITE(IOUT,1700) ISPHIO(NISPHIO,N),IPART(4,ISPHIO(2,N))
             ENDIF
           ENDIF
           IF (ISPHIO(12,N)==1) THEN
C            input by coordinates
             IVAD=ISPHIO(4,N)
             WRITE(IOUT,2100) VSPHIO(IVAD+4),VSPHIO(IVAD+5),VSPHIO(IVAD+6),
     .       VSPHIO(IVAD+7),VSPHIO(IVAD+8),VSPHIO(IVAD+9),VSPHIO(IVAD+10),
     .       VSPHIO(IVAD+11),VSPHIO(IVAD+12) 
           ELSEIF (ISPHIO(12,N)==2) THEN
C            input by node ID
             WRITE(IOUT,2200) ITAB(ISPHIO(13,N)),ITAB(ISPHIO(14,N)),ITAB(ISPHIO(15,N))
           ENDIF
           IF (VSPHIO(IVAD+15)>EM20) WRITE(IOUT,2300) VSPHIO(IVAD+15)
         ENDIF
      ENDDO
C-------------------------------------
C
      RETURN
C
 1000 FORMAT(//
     .'     SPH INLET/OUTLET CONDITIONS       '/
     .'     ---------------------------       ')
 1100 FORMAT(/5X ,'SPH INLET/OUTLET CONDITION ID           ',I10
     .       /5X ,'TYPE INLET                              ',
     .       /10X,'PART RELATED TO CONDITION               ',I10
     .       /10X,'INLET SURFACE                           ',I10
     .       /10X,'DENSITY FUNCTION                        ',I10
     .       /10X,'SCALE FACTOR ON DENSITY FUNCTION        ',1PG20.13
     .       /10X,'ENERGY FUNCTION                         ',I10
     .       /10X,'SCALE FACTOR ON ENERGY FUNCTION         ',1PG20.13
     .       /10X,'NORMAL VELOCITY FUNCTION                ',I10
     .       /10X,'WITHIN DISTANCE FOR PARTICLES SETTING   ',1PG20.13)
 1200 FORMAT(/5X ,'SPH INLET/OUTLET CONDITION ID           ',I10
     .       /5X ,'TYPE GENERAL OUTLET                     ',
     .       /10X,'PART RELATED TO CONDITION               ',I10
     .       /10X,'OUTLET SURFACE                          ',I10
     .       /10X,'PRESSURE FUNCTION                       ',I10
     .       /10X,'SCALE FACTOR ON PRESSURE FUNCTION       ',1PG20.13
     .       /10X,'WITHOUT DISTANCE FOR PARTICLES SETTING  ',1PG20.13)
 1300 FORMAT(/5X ,'SPH INLET/OUTLET CONDITION ID           ',I10
     .       /5X ,'TYPE SILENT BOUNDARY                    ',
     .       /10X,'PART RELATED TO CONDITION               ',I10
     .       /10X,'OUTLET SURFACE                          ',I10
     .       /10X,'WITHOUT DISTANCE FOR PARTICLES SETTING  ',1PG20.13
     .       /10X,'PRESSURE FUNCTION                       ',I10
     .       /10X,'SCALE FACTOR ON PRESSURE FUNCTION       ',1PG20.13
     .       /10X,'CHARACTERISTIC LENGTH                   ',1PG20.13)
 1400 FORMAT(/5X ,'SPH INLET/OUTLET CONDITION ID           ',I10
     .       /5X ,'TYPE GENERAL OUTLET                     ',
     .       /10X,'PART RELATED TO CONDITION               ',I10
     .       /10X,'PRESSURE FUNCTION                       ',I10
     .       /10X,'SCALE FACTOR ON PRESSURE FUNCTION       ',1PG20.13
     .       /10X,'WITHOUT DISTANCE FOR PARTICLES SETTING  ',1PG20.13)
 1500 FORMAT(/5X ,'SPH INLET/OUTLET CONDITION ID           ',I10
     .       /5X ,'TYPE SILENT BOUNDARY                    ',
     .       /10X,'PART RELATED TO CONDITION               ',I10
     .       /10X,'PRESSURE FUNCTION                       ',I10
     .       /10X,'SCALE FACTOR ON PRESSURE FUNCTION       ',1PG20.13
     .       /10X,'WITHOUT DISTANCE FOR PARTICLES SETTING  ',1PG20.13
     .       /10X,'CHARACTERISTIC LENGTH                   ',1PG20.13)
 1600 FORMAT(/5X ,'SPH INLET/OUTLET CONDITION ID           ',I10
     .       /5X ,'TYPE SPH CONTROL SECTION                ',
     .       /10X,'PART RELATED TO CONTROL SECTION         ',I10
     .       /10X,'SURFACE                                 ',I10)
 1700 FORMAT(/5X ,'SPH INLET/OUTLET CONDITION ID           ',I10
     .       /5X ,'TYPE SPH CONTROL SECTION                ',
     .       /10X,'PART RELATED TO CONTROL SECTION         ',I10)
C
 2100 FORMAT(10X,'SURFACE DEFINED BY COORDINATES          ',
     .       /10X,' --> COORDINATES OF NODE1               ',1PG20.13,1PG20.13,1PG20.13,
     .       /10X,' --> COORDINATES OF NODE2               ',1PG20.13,1PG20.13,1PG20.13,
     .       /10X,' --> COORDINATES OF NODE3               ',1PG20.13,1PG20.13,1PG20.13)
C
 2200 FORMAT(10X,'SURFACE DEFINED BY NODES                ',I10,I10,I10)
C
 2300 FORMAT(10X,'4-POLE BUTTERWORTH CORNER FREQUENCY     ',1PG20.13)
C
      RETURN
      END


