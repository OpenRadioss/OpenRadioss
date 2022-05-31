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
Chd|  HM_READ_INTER_LAGMUL_TYPE07   source/interfaces/int07/hm_read_inter_lagmul_type07.F
Chd|-- called by -----------
Chd|        HM_READ_INTER_LAGMUL          source/interfaces/reader/hm_read_inter_lagmul.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        NGR2USR                       source/system/nintrr.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_INTER_LAGMUL_TYPE07(
     1        IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,NI         ,    
     2        IGRNOD     ,IGRSURF    ,TITR       ,UNITAB     ,LSUBMODEL  )
C============================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE UNITAB_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr06_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISU1,ISU2,NOINT,NI
      INTEGER IPARI(*)
      my_real
     .   FRIGAP(*), STFAC
      CHARACTER TITR*nchartitle
C-----------------------------------------------
      TYPE (GROUP_)  ,TARGET, DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (SURF_)   ,TARGET , DIMENSION(NSURF)   :: IGRSURF
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,L, NTYP,MULTIMP,FLAGREMNOD,
     .    IREM7I2,IS1,IS2
      my_real
     .   STARTT,BUMULT,STOPT,GAPMAX,GAP
      CHARACTER MESS*40
!
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NGR2USR
C=======================================================================
C    /INTER/LAGMUL/TYPE2 reading 
C=======================================================================
      IS1=0
      IS2=0
      MULTIMP = 0
      IREM7I2=0
C
      NTYP = 7 
      IPARI(15)=NOINT
      IPARI(7)=NTYP
C
      IS_AVAILABLE = .FALSE.
C------------------------------------------------------------
C  Card1
C------------------------------------------------------------
      CALL HM_GET_INTV('secondaryentityids',ISU1,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('mainentityids',ISU2,IS_AVAILABLE,LSUBMODEL)
C
       IF(ISU2.EQ.0) THEN
          CALL ANCMSG(MSGID=119,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=NOINT,
     .                C1=TITR)
       ENDIF
C
       IS2=1
       INGR2USR => IGRSURF(1:NSURF)%ID
       ISU2=NGR2USR(ISU2,INGR2USR,NSURF)
       IF(ISU1.NE.0)THEN
          INGR2USR => IGRNOD(1:NGRNOD)%ID
          ISU1=NGR2USR(ISU1,INGR2USR,NGRNOD)
          IS1 =2
       ENDIF
       IF(ISU1.EQ.0)THEN
         ISU1=ISU2
         IS1 =1
       ENDIF
C------------------------------------------------------------
       IF(FRIGAP(16).EQ.ZERO)THEN
           GAPMAX=EP30
           FRIGAP(16)=GAPMAX
       END IF
C------------------------------------------------------------
       FLAGREMNOD = 0
       IF (FLAGREMNOD == 0) FLAGREMNOD = 1
       IPARI(63) = FLAGREMNOD
C
       IF (IREM7I2==0) THEN
          IF (IIMPLICIT>0) IREM7I2=1
       END IF
       IPARI(54) = IREM7I2
C------------------------------------------------------------
C  Card4
C------------------------------------------------------------
        CALL HM_GET_FLOATV('Gapmin',GAP,IS_AVAILABLE,LSUBMODEL,UNITAB)
        FRIGAP(2)=GAP
C
C       STARTT & STOPT are actually working but not documented
C       IF (STOPT == ZERO) STOPT = EP30
C
        STARTT = ZERO
        STOPT = EP30
        FRIGAP(3)=STARTT
        FRIGAP(11)=STOPT
C
        IF(STFAC.EQ.ZERO) THEN
           STFAC=ONE
        ENDIF 
C
C------------------------------------------------------------
C  Card5
C------------------------------------------------------------
        CALL HM_GET_FLOATV('BUMULT',BUMULT,IS_AVAILABLE,LSUBMODEL,UNITAB)

        IF(BUMULT.EQ.ZERO) THEN
          BUMULT = BMUL0
C bmult augmente pour les tres gros modeles et interface 7 ou 20
           IF(NTYP.EQ.7)THEN
             IF(NUMNOD > 2500000) THEN
               BUMULT = BMUL0*TWO
             ELSEIF(NUMNOD > 1500000) THEN
               BUMULT = BMUL0*THREE/TWO
             END IF
           END IF
        END IF
        FRIGAP(4)=BUMULT

C FRIGAP(10) is initialized but used only in engine for storing number of couples candidates  
        FRIGAP(10)=FLOAT(0)
        MULTIMP = 4
        IPARI(23)=MULTIMP

        IPARI(13)=IS1*10+IS2

        IPARI(15)=NOINT

        IPARI(45)=ISU1
        IPARI(46)=ISU2
C
C------------------------------------------------------------
C     PRINTOUT
C------------------------------------------------------------
C
      WRITE(IOUT,1527)       
C
      IF(IS1.EQ.0)THEN
        WRITE(IOUT,'(6X,A)')'NO SECONDARY SURFACE INPUT'
      ELSEIF(IS1.EQ.1)THEN
        WRITE(IOUT,'(6X,A)')'SECONDARY SURFACE INPUT BY SEGMENTS'
      ELSEIF(IS1.EQ.2)THEN
        WRITE(IOUT,'(6X,A)')'SECONDARY SURFACE INPUT BY NODES'
      ELSEIF(IS1.EQ.3)THEN
        WRITE(IOUT,'(6X,A)')'SECONDARY SURFACE INPUT BY SEGMENTS'
      ELSEIF(IS1.EQ.4 )THEN
        WRITE(IOUT,'(6X,A)')'SECONDARY SIDE INPUT BY BRICKS'
      ELSEIF(IS1.EQ.5 )THEN
        WRITE(IOUT,'(6X,A)')'SECONDARY SIDE INPUT BY SOLID ELEMENTS'        
      ENDIF
      IF(IS2.EQ.0)THEN
        WRITE(IOUT,'(6X,A)')'NO MAIN SURFACE INPUT'
      ELSEIF(IS2.EQ.1)THEN
        WRITE(IOUT,'(6X,A)')'MAIN SURFACE INPUT BY SEGMENTS'
      ELSEIF(IS2.EQ.2)THEN
        WRITE(IOUT,'(6X,A)')'MAIN SURFACE INPUT BY NODES'
      ELSEIF(IS2.EQ.3)THEN
        WRITE(IOUT,'(6X,A)')'MAIN SURFACE INPUT BY SEGMENTS'
      ELSEIF(IS2.EQ.4)THEN
        WRITE(IOUT,'(6X,A)')'MAIN SURFACE REFERS ',
     .                      'TO HYPER-ELLIPSOIDAL SURFACE'
      ENDIF
C  
C--------------------------------------------------------------
      RETURN
 1527 FORMAT(//
     .    '    TYPE.EQ.7   PARALLEL/AUTO IMPACTING           ' /,
     .    '    LAGRANGE MULTIPLIER FORMULATION               ' //)  
      END
