Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  HM_READ_RWALL_THERM           source/constraints/general/rwall/hm_read_rwall_therm.F
Chd|-- called by -----------
Chd|        READ_RWALL                    source/constraints/general/rwall/read_rwall.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL                       source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        KINSET                        source/constraints/general/kinset.F
Chd|        REMOVE_ND                     source/elements/solid/solide10/dim_s10edg.F
Chd|        SUBROTPOINT                   source/model/submodel/subrot.F
Chd|        NGR2USR                       source/system/nintrr.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_RWALL_THERM(RWL      ,NPRW    ,LPRW    ,IFI     ,MS      ,
     .                                V       ,ITAB    ,ITABM1  ,X       ,IKINE   ,
     .                                IGRNOD  ,MFI     ,IMERGE  ,UNITAB  ,IDDLEVEL,
     .                               LSUBMODEL,RTRANS  ,NOM_OPT ,ITAGND  ,NCHTHERM, 
     .                                K       ,OFFS    ,IKINE1  ,IXS     ,IXQ     ,
     .                                NPC     ) 
C-------------------------------------
C     LECTURE MUR RIGIDE
C-----------------------------------------------
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE SUBMODEL_MOD
      USE MESSAGE_MOD
      USE R2R_MOD
      USE GROUPDEF_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "analyse_name.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "lagmult.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "sysunit.inc"
#include      "submod_c.inc"
#include      "tabsiz_c.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER IFI,MFI,IDDLEVEL,NCHTHERM,K,OFFS
      INTEGER NPRW(*), LPRW(*), ITAB(*), ITABM1(*),
     .        IKINE(*), IMERGE(*),ITAGND(*),IKINE1(3*NUMNOD),
     .        IXS(NIXS,*),IXQ(NIXQ,*),NPC(*)
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
C     REAL
      my_real
     .   RWL(NRWLP,*), MS(*), V(3,*), X(3,*),
     .   RTRANS(NTRANSF,*)
      INTEGER NOM_OPT(LNOPT1,*)
C-----------------------------------------------
      TYPE (GROUP_)  ,TARGET, DIMENSION(NGRNOD)  :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, ITYP, ITIED, NSL, NUSER, MSR, J, I,
     .   L, IGU,IGU2, IGRS, NOSYS, IFQ, JC, UID,
     .   IFLAGUNIT,SUB_ID, SUB_INDEX,IFUNC,NE,KK,ND,K0,NN,NF
      my_real
     .   DIST, FRIC, DIAM, XMAS, VX, VY, VZ, XM1, YM1, ZM1,
     .   XN, X1, Y1, Z1, DISN, X2, Y2, Z2, X3, FREQ, ALPHA, FAC_M_R2R,
     .   TEMP,TSTIF,FHEAT,FACX,FAC_TSTIF
      CHARACTER MESS*40,OPT*ncharkey,TITR*nchartitle
      LOGICAL :: IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS, NGR2USR
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
      DATA MESS/'STANDARD RIGID WALL DEFINITION          '/
C=======================================================================
C-----------------------------------------------      
!     **************************    !
!     RWALL/PLANE read with hm reader     !
!     **************************    !
C-----------------------------------------------
      IS_AVAILABLE = .FALSE.
      CALL HM_OPTION_START('/RWALL/THERM')
      ! Flag for RWALL type PLANE
      ITYP = 1
      !----------------------------------------------------------------------
      ! Loop over HM_RWALLs
      !----------------------------------------------------------------------
      DO N = 1+OFFS, NCHTHERM+OFFS
C      
        ! Reading the option
        ! /RWALL/type/rwall_ID/node_ID
        !   rwall_title
        TITR = ''   
        CALL HM_OPTION_READ_KEY(LSUBMODEL, 
     .                          OPTION_ID      = NUSER, 
     .                          UNIT_ID        = UID,
     .                          SUBMODEL_INDEX = SUB_INDEX,
     .                          SUBMODEL_ID    = SUB_ID,
     .                          OPTION_TITR    = TITR)   
C
        NOM_OPT(1,N)=NUSER
        CALL FRETITL(TITR,NOM_OPT(LNOPT1-LTITR+1,N),LTITR)
C
        ! Checking flag unit
        IFLAGUNIT = 0        
        DO J=1,NUNITS                           
          IF (UNITAB%UNIT_ID(J) == UID) THEN          
            IFLAGUNIT = 1                         
            EXIT                                
          ENDIF                                 
        ENDDO                                 
        IF (UID /= 0 .AND. IFLAGUNIT == 0) THEN
          CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I2=UID,I1=NUSER,C1='RIGID WALL',
     .                C2='RIGID WALL',
     .                C3=TITR)
        ENDIF   
C
        !  node_ID     Slide  grnd_ID1  grnd_ID2
        CALL HM_GET_INTV('slidingflag',ITIED,IS_AVAILABLE,LSUBMODEL)  
        CALL HM_GET_INTV('NodeSet_ID',IGU,IS_AVAILABLE,LSUBMODEL)  
        CALL HM_GET_INTV('excludeNodeSet_ID',IGU2,IS_AVAILABLE,LSUBMODEL)  
C
        ! Fixed rigid wall
        MSR = 0
C
        ! 2nd card
        !                  d                fric            Diameter                ffac       ifq
        CALL HM_GET_FLOATV('offset'         ,DIST       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('fric'           ,FRIC       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('Diameter'       ,DIAM       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IFQ   = 0
        FREQ  = ONE
        ALPHA = FREQ
        RWL(13,N) = FRIC
        RWL(14,N) = ALPHA
        RWL(15,N) = IFQ
C
        ! 3rd card
        !                 XM                  YM                  ZM
        CALL HM_GET_FLOATV('x'         ,X1       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('y'         ,X2       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('z'         ,X3       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF(SUB_ID /= 0) CALL SUBROTPOINT(X1,X2,X3,RTRANS,SUB_ID,LSUBMODEL)
        RWL(4,N) = X1
        RWL(5,N) = X2
        RWL(6,N) = X3 
C
        ! 4th card
        !                 XM1                 YM1                 ZM1
        CALL HM_GET_FLOATV('XH'        ,XM1       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('YH'        ,YM1       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('ZH'        ,ZM1       ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
        IF (SUB_ID /= 0) CALL SUBROTPOINT(XM1,YM1,ZM1,RTRANS,SUB_ID,LSUBMODEL)    
C
        ! 5th card (only for THERM)
        CALL HM_GET_INTV('fct_IDt'            ,IFUNC,IS_AVAILABLE, LSUBMODEL)       
        CALL HM_GET_FLOATV('Fscale_T'         ,TEMP ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('Thermalresistance',TSTIF,IS_AVAILABLE, LSUBMODEL, UNITAB)
C
        ! Initialization depending on the type of interface
        ! PLANE
C       M MUR ET MM1 NORMALE
        RWL(1,N) = XM1-RWL(4,N)
        RWL(2,N) = YM1-RWL(5,N)
        RWL(3,N) = ZM1-RWL(6,N)
        XN = SQRT(RWL(1,N)**2+RWL(2,N)**2+RWL(3,N)**2)
        IF (XN <= EM10) THEN
           CALL ANCMSG(MSGID=167,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                 I1=NUSER,C2='PLANE',C1=TITR)
        ELSE
          RWL(1,N) = RWL(1,N)/XN
          RWL(2,N) = RWL(2,N)/XN
          RWL(3,N) = RWL(3,N)/XN
        ENDIF
C
        ! Looking for SECONDARY nodes
        DO I = 1,NUMNOD
          LPRW(K+I) = 0
        ENDDO
C
        ! SECONDARY nodes at DIST from the RWALL
        IF (DIST /= ZERO) THEN
          DO I = 1,NUMNOD
            X1 = (X(1,I)-RWL(4,N))*RWL(1,N)
            Y1 = (X(2,I)-RWL(5,N))*RWL(2,N)
            Z1 = (X(3,I)-RWL(6,N))*RWL(3,N)
            DISN = X1+Y1+Z1
            IF (DISN >= ZERO .AND. DISN <= DIST .AND. I /= MSR) LPRW(K+I)=1
          ENDDO
        ENDIF
C
        ! Node group +
        INGR2USR => IGRNOD(1:NGRNOD)%ID
        IGRS = NGR2USR(IGU,INGR2USR,NGRNOD)
        IF (IGRS /= 0) THEN
          DO J = 1,IGRNOD(IGRS)%NENTITY
            NOSYS = IGRNOD(IGRS)%ENTITY(J)
            LPRW(K+NOSYS) = 1
            IF (ITAB(NOSYS) == NUSER) THEN
              CALL ANCMSG(MSGID=637,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=NUSER,
     .                    C1=TITR,
     .                    I2=NUSER)
            ENDIF
          ENDDO
        ENDIF
C
        ! Node group -
        INGR2USR => IGRNOD(1:NGRNOD)%ID
        IGRS = NGR2USR(IGU2,INGR2USR,NGRNOD)
        IF (IGRS /= 0) THEN
          DO J = 1,IGRNOD(IGRS)%NENTITY
            NOSYS = IGRNOD(IGRS)%ENTITY(J)
            LPRW(K+NOSYS) = 0
          ENDDO
        ENDIF
C
        ! Compaction
        NSL = 0
        DO I = 1,NUMNOD
          IF (LPRW(K+I) > 0) THEN
            IF (NS10E > 0 .AND. ITAGND(I) /= 0) CYCLE
            NSL = NSL+1
            LPRW(K+NSL) = I
            IF (IDDLEVEL == 0) THEN
              CALL KINSET(4,ITAB(I),IKINE(I),1,N+NUMSKW+1,IKINE1(I))
            ENDIF
          ENDIF
        ENDDO
        ! Itet=2 of S10      
        IF (NS10E > 0 ) CALL REMOVE_ND(NSL,LPRW(K+1),ITAGND)
        IFI=IFI+NSL
        IF (IFQ > 0) THEN
          MFI=MFI+3*NSL
          SRWSAV = SRWSAV + 3 * NSL
        ENDIF
C
        ! Printing
        WRITE(IOUT,1100) N,ITYP,ITIED,NSL
        WRITE(IOUT,2001)(RWL(L,N),L=4,6),(RWL(L,N),L=1,3)
        IF (ITIED == 2) WRITE(IOUT,2101)FRIC,IFQ,FREQ
        IF (IPRI >= 1) THEN
          WRITE(IOUT,1200)
          WRITE(IOUT,1201) (ITAB(LPRW(I+K)),I=1,NSL)
        ENDIF
C
        NPRW(N)          = NSL
        NPRW(N+NRWALL)   = ITIED
        NPRW(N+2*NRWALL) = MSR
        NPRW(N+3*NRWALL) = ITYP
        NPRW(N+4*NRWALL) = 0
        NPRW(N+5*NRWALL) = 0              
        K0 = K
        K  = K+NSL
C
        NE = 0
        KK = K+2*NSL
        DO ND = KK+1,KK+NUMNOD
          LPRW(ND) = 0
        ENDDO
        DO J = 1+K0,NSL+K0
          ND = LPRW(J) + KK
          LPRW(ND) = 1
        ENDDO
        IF (N2D == 0) THEN
          DO I = 1,NUMELS
            NN = 0
            DO J = 2,9
              ND = IXS(J,I) + KK
              NN = NN + LPRW(ND)
            ENDDO
            IF (NN >= 4) THEN
              NE = NE+1
              NN = LPRW(IXS(2,I) + KK) 
     .           + LPRW(IXS(3,I) + KK)
     .           + LPRW(IXS(4,I) + KK)
     .           + LPRW(IXS(5,I) + KK)
              IF (NN == 4) THEN
                LPRW(K+NE) = I*10 + 1
              ELSE
                NN = LPRW(IXS(4,I) + KK) 
     .             + LPRW(IXS(5,I) + KK)
     .             + LPRW(IXS(8,I) + KK)
     .             + LPRW(IXS(9,I) + KK)
                IF (NN == 4) THEN
                  LPRW(K+NE) = I*10 + 2
                ELSE
                  NN = LPRW(IXS(6,I) + KK) 
     .               + LPRW(IXS(7,I) + KK)
     .               + LPRW(IXS(8,I) + KK)
     .               + LPRW(IXS(9,I) + KK)
                  IF (NN == 4) THEN
                    LPRW(K+NE) = I*10 + 3
                  ELSE
                    NN = LPRW(IXS(2,I) + KK) 
     .                 + LPRW(IXS(3,I) + KK)
     .                 + LPRW(IXS(6,I) + KK)
     .                 + LPRW(IXS(7,I) + KK)
                    IF (NN == 4) THEN
                      LPRW(K+NE) = I*10 + 4
                    ELSE
                      NN = LPRW(IXS(3,I) + KK) 
     .                   + LPRW(IXS(4,I) + KK)
     .                   + LPRW(IXS(7,I) + KK)
     .                   + LPRW(IXS(8,I) + KK)
                      IF (NN == 4) THEN
                        LPRW(K+NE) = I*10 + 5
                      ELSE
                        NN = LPRW(IXS(2,I) + KK) 
     .                     + LPRW(IXS(5,I) + KK)
     .                     + LPRW(IXS(6,I) + KK)
     .                     + LPRW(IXS(9,I) + KK)
                        IF (NN == 4)THEN
                          LPRW(K+NE) = I*10 + 6
                        ELSE
                          NE = NE-1
                        ENDIF
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDDO
        ELSE
          DO I = 1,NUMELQ
            NN = 0
            DO J = 2,5
              ND = IXQ(J,I) + KK
              NN = NN + LPRW(ND)
            ENDDO
            IF (NN >= 2) THEN
              NE = NE+1
              NN = LPRW(IXQ(2,I) + KK) + LPRW(IXQ(3,I) + KK)
              IF (NN == 2) THEN
                LPRW(K+NE) = I*10 + 1
              ELSE
                NN = LPRW(IXQ(3,I) + KK) + LPRW(IXQ(4,I) + KK)
                IF (NN == 2) THEN
                  LPRW(K+NE) = I*10 + 2
                ELSE
                  NN = LPRW(IXQ(4,I) + KK) + LPRW(IXQ(5,I) + KK)
                  IF (NN == 2) THEN
                    LPRW(K+NE) = I*10 + 3
                  ELSE
                    NN = LPRW(IXQ(5,I) + KK) + LPRW(IXQ(2,I) + KK)
                    IF (NN == 2) THEN
                      LPRW(K+NE) = I*10 + 4
                    ELSE
                      NE = NE-1
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDDO
        ENDIF
C
        ! These two parameters are not set in the input deck
        ! but remain in the buffer table
        FHEAT = ONE
        FACX  = ONE/FAC_T_WORK
C
        IF (TSTIF == ZERO) THEN 
          CALL HM_GET_FLOATV_DIM('Thermalresistance',FAC_TSTIF,IS_AVAILABLE,LSUBMODEL,UNITAB)
          TSTIF = ONE*FAC_TSTIF
        ENDIF     
C       
        WRITE(IOUT,2100) IFUNC,TEMP,TSTIF
C        
        TSTIF = ONE/TSTIF
C
        NPRW(N+3*NRWALL)=-ITYP
C
        NF = 0
        IF (IFUNC > 0) THEN 
          DO I=1,NFUNCT
            IF (IFUNC == NPC(I)) THEN 
              NF = I
              EXIT
            ENDIF
          ENDDO
          IF (NF == 0) THEN 
            CALL ANCMSG(MSGID=120,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=IFUNC,
     .                  C1=TITR)
          ENDIF
        ENDIF
C
        RWL(8,N)  = NE
        RWL(9,N)  = NF
        RWL(10,N) = TEMP
        RWL(11,N) = TSTIF
        RWL(12,N) = FHEAT
        RWL(16,N) = FACX
C
        IFI = IFI + NE
C
        K = K + NE
C
      ENDDO
C
      ! Updating the OFFSET
      OFFS = OFFS + NCHTHERM
C
      RETURN
C
 1100 FORMAT(/5X,'RIGID WALL NUMBER. . . . .',I10
     .       /10X,'RIGID WALL TYPE . . . . .',I10
     .       /10X,'TYPE SLIDE/TIED/FRICTION.',I10
     .       /10X,'NUMBER OF NODES . . . . .',I10)
 1200 FORMAT(/10X,'SECONDARY NODES :   ')
 1201 FORMAT(/10X,10I10)
 2001 FORMAT(/5X,'INFINITE WALL CHARACTERISTICS',
     .       /10X,'POINT M . . . . . . . . .',1P3G20.13
     .       /10X,'NORMAL VECTOR . . . . . .',1P3G20.13)
 2100 FORMAT(/5X,'THERMAL CHARACTERISTICS',
     .       /10X,'TEMPERATURE FUNCTION. . .',I10
     .       /10X,'TEMPERATURE SCALE FACTOR.',1PG14.4
     .       /10X,'THERMAL RESISTANCE. . . .',1PG14.4)
 2101 FORMAT(/5X,'COULOMB FRICTION CHARACTERISTICS',
     .       /10X,'FRICTION COEFFICIENT . . .',1PG14.4
     .       /10X,'FILTRATION FLAG. . . . . .',I10
     .       /10X,'FILTRATION FACTOR. . . . .',1PG14.4)
      END
