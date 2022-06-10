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
Chd|  GENOUTP                       source/output/sty/genoutp.F   
Chd|-- called by -----------
Chd|        IMP_BUCK                      source/implicit/imp_buck.F    
Chd|        SORTIE_MAIN                   source/output/sortie_main.F   
Chd|-- calls ---------------
Chd|        COUNT_ARSZ_CS                 source/output/sty/outp_c_s.F  
Chd|        COUNT_ARSZ_CT                 source/output/sty/outp_c_t.F  
Chd|        COUNT_ARSZ_RS                 source/output/sty/outp_r_s.F  
Chd|        COUNT_ARSZ_RT                 source/output/sty/outp_r_t.F  
Chd|        COUNT_ARSZ_SPS                source/output/sty/outp_sp_s.F 
Chd|        COUNT_ARSZ_SPT                source/output/sty/outp_sp_t.F 
Chd|        COUNT_ARSZ_SPTT               source/output/sty/outp_sp_t.F 
Chd|        COUNT_ARSZ_SS                 source/output/sty/outp_s_s.F  
Chd|        COUNT_ARSZ_ST                 source/output/sty/outp_s_t.F  
Chd|        FILELEN_SYS                   source/output/tools/sortie_c.c
Chd|        OUTP_ARSZ_CS                  source/mpi/interfaces/spmd_outp.F
Chd|        OUTP_ARSZ_CT                  source/mpi/interfaces/spmd_outp.F
Chd|        OUTP_ARSZ_RS                  source/mpi/interfaces/spmd_outp.F
Chd|        OUTP_ARSZ_RT                  source/mpi/interfaces/spmd_outp.F
Chd|        OUTP_ARSZ_SPS                 source/mpi/interfaces/spmd_outp.F
Chd|        OUTP_ARSZ_SPT                 source/mpi/interfaces/spmd_outp.F
Chd|        OUTP_ARSZ_SPTT                source/mpi/interfaces/spmd_outp.F
Chd|        OUTP_ARSZ_SS                  source/mpi/interfaces/spmd_outp.F
Chd|        OUTP_ARSZ_ST                  source/mpi/interfaces/spmd_outp.F
Chd|        OUTP_C_S                      source/output/sty/outp_c_s.F  
Chd|        OUTP_C_T                      source/output/sty/outp_c_t.F  
Chd|        OUTP_C_TF                     source/output/sty/outp_c_t.F  
Chd|        OUTP_MT                       source/output/sty/outp_mt.F   
Chd|        OUTP_NO                       source/output/sty/outp_no.F   
Chd|        OUTP_N_V                      source/output/sty/outp_n_v.F  
Chd|        OUTP_N_V2                     source/output/sty/outp_n_v.F  
Chd|        OUTP_N_VC                     source/output/sty/outp_n_v.F  
Chd|        OUTP_R_S                      source/output/sty/outp_r_s.F  
Chd|        OUTP_R_T                      source/output/sty/outp_r_t.F  
Chd|        OUTP_SP_S                     source/output/sty/outp_sp_s.F 
Chd|        OUTP_SP_T                     source/output/sty/outp_sp_t.F 
Chd|        OUTP_SP_TT                    source/output/sty/outp_sp_t.F 
Chd|        OUTP_S_S                      source/output/sty/outp_s_s.F  
Chd|        OUTP_S_T                      source/output/sty/outp_s_t.F  
Chd|        OUTP_S_TT                     source/output/sty/outp_s_t.F  
Chd|        SPMD_OUTPITAB                 source/mpi/interfaces/spmd_outp.F
Chd|        DRAPE_MOD                     share/modules/drape_mod.F     
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE GENOUTP(X    ,D         ,V      ,A     ,
     2                  IXS   ,IXQ       ,IXC    ,IXT   ,IXP   ,
     3                  IXR   ,IXTG      ,IPARG  ,PM    ,IGEO  ,
     4                  MS    ,CONT      ,ITAB   ,PARTSAV,FINT ,
     5                  FEXT  ,TANI      ,EANI   ,ANIN  ,IPART ,
     6                  VR    ,ELBUF_TAB ,DD_IAD,WEIGHT,
     7                  IPM   ,KXSP      ,SPBUF  ,NODGLOB,LENG ,
     8                  FOPT  ,NOM_OPT   ,NPBY   ,FNCONT   ,FTCONT,
     9                  GEO   ,THKE      ,STACK  ,DRAPE_Q4, DRAPE_T3)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD         
      USE STACK_MOD
      USE INOUTFILE_MOD
      USE DRAPE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com05_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "com09_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr11_c.inc"
#include      "scr13_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "chara_c.inc"
#include      "task_c.inc"
#include      "sphcom.inc"
#include      "spmd_c.inc"
#include      "filescount_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(*),IPART(LIPART1,*),
     .   IXS(NIXS,*),IXQ(NIXQ,*),IXC(NIXC,*),IXTG(NIXTG,*),
     .   IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*),ITAB(*),
     .   WEIGHT(*), DD_IAD(*),IPM(*),IGEO(*),KXSP(NISP,*),
     .   LENG,NODGLOB(*), NOM_OPT(*), NPBY(*)
C     REAL
      my_real
     .   X(*), D(*), V(*), VR(*), A(*), 
     .   PM(NPROPM,*), MS(*),ANIN(*),
     .   CONT(*), PARTSAV(*),FINT(*),FEXT(*),TANI(6,*),EANI(*),
     .   SPBUF(*), FOPT(*), FNCONT(3,*), FTCONT(3,*),GEO(*),THKE(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (STACK_PLY) :: STACK
      TYPE(DRAPE_) :: DRAPE_Q4, DRAPE_T3
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER CHOUTP*4,FILNAM*100,T10*10,MES*40,VAR_CHAR*11,CHAR_JJ*2,
     .          TITRE*40
      INTEGER FILEN,I,INNODA,J,JJ
      INTEGER LENR,SIZLOC,SIZP0,SIZP0_95_96
      INTEGER I161,I16A,I16B,I16C,I16D,I16E,I16F,I16G,I16H,I16I
      INTEGER SIZWR_CT(3),SIZP0_CT(3),SIZLOC_CT(3),
     .        SIZWR_CS(2),SIZP0_CS(2),SIZLOC_CS(2),
     .        SIZWR_SS(2),SIZP0_SS(2),SIZLOC_SS(2),
     .        SIZWR_ST(3),SIZP0_ST(3),SIZLOC_ST(3)
      INTEGER SIZW_P0
      INTEGER, DIMENSION(:), ALLOCATABLE :: SIZLOC_WRITE
      INTEGER OUTPSIZE
      INTEGER, DIMENSION(:), ALLOCATABLE :: ITABG

      INTEGER :: LEN_TMP_NAME
      CHARACTER(len=2148) :: TMP_NAME
C-----------------------------------------------
      ALLOCATE(ITABG(LENG))
      I161=1
      I16A=I161+LNOPT1*NRBODY0
      I16B=I16A+LNOPT1*NACCELM
      I16C=I16B+LNOPT1*NVOLU
      I16D=I16C+LNOPT1*(NINTER+NINTSUB)
      I16E=I16D+LNOPT1*NRWALL
      I16F=I16E !obsolete option removed
      I16G=I16F+LNOPT1*NJOINT
      I16H=I16G+LNOPT1*NSECT
      I16I=I16H+LNOPT1*NLINK
C-----------------------------------------------
C   OPEN FILE
C-----------------------------------------------
      IF(ISPMD == 0) THEN
       IF(IROOTYY == 2)THEN
        IF(IOUTP.GE.1000)IOUTP=1
        WRITE(CHOUTP,'(I3.3)')IOUTP
        FILNAM=ROOTNAM(1:ROOTLEN)//'Y'//CHOUTP
        FILEN = ROOTLEN + 4 
       ELSE       
        IF(IOUTP.GE.10000)IOUTP=1
        WRITE(CHOUTP,'(I4.4)')IOUTP
        FILNAM=ROOTNAM(1:ROOTLEN)//'_'//CHOUTP//'.sty'
        FILEN = ROOTLEN + 9
        ENDIF
        LEN_TMP_NAME = OUTFILE_NAME_LEN + FILEN
        TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:FILEN)   
       OPEN(UNIT=IUGEO,FILE=TMP_NAME(1:LEN_TMP_NAME),
     .     ACCESS='SEQUENTIAL',
     .     FORM='FORMATTED',STATUS='UNKNOWN')
C-----------------------------------------------
C   WRITE CONTROL
C-----------------------------------------------
       WRITE(IUGEO,'(2A)')'#RADIOSS OUTPUT FILE V21 ',
     .      FILNAM(1:FILEN)
C-----------------------------------------------
C   GLOBAL SCALARS
C-----------------------------------------------
       WRITE(IUGEO,'(A)')'/GLOBAL    '
       WRITE(IUGEO,'(A)')' '
       WRITE(IUGEO,'(A)')'#FORMAT: (1P5E16.9)'
       WRITE(IUGEO,'(2A)')'#           TIME INTERNAL_ENERGY',
     .   '  KINETIC_ENERGY ROT_KINE_ENERGY EXTE_FORCE_WORK'
       WRITE(IUGEO,'(1P5E16.9)') TT,ENINT,ENCIN,ENROT,TFEXT
      ENDIF
C-----------------------------------------------
C   MATER SCALARS 
C-----------------------------------------------
      CALL OUTP_MT(PM,NPART,PARTSAV,IPART,IPM)
C-----------------------------------------------
C   NODAL VECT 
C-----------------------------------------------
      IF (NSPMD > 1)
     .    CALL SPMD_OUTPITAB(ITAB,WEIGHT,NODGLOB,ITABG)
      CALL OUTP_N_V('COORDINATE',
     .   ' Coordinates                            ',
     .   20,X,NUMNOD,ITAB,
     .   ITABG,LENG,NODGLOB,WEIGHT)

      IF(OUTP_V(1) == 1) CALL OUTP_N_V('VELOCITY  ',
     .   ' Velocity                               ',
     .   20,V,NUMNOD,ITAB,
     .   ITABG,LENG,NODGLOB,WEIGHT)

      IF(OUTP_V(2) == 1) CALL OUTP_N_V('DISPLACEME',
     .   ' Displacement                           ',
     .   20,D,NUMNOD,ITAB,
     .   ITABG,LENG,NODGLOB,WEIGHT)

      IF(OUTP_V(3) == 1) CALL OUTP_N_V('ACCELERATI',
     .   ' Acceleration                           ',
     .   20,A,NUMNOD,ITAB,
     .   ITABG,LENG,NODGLOB,WEIGHT)

      IF(OUTP_V(4) == 1) CALL OUTP_N_VC('CONT_FORCE',
     .   ' Contact Forces                         ',
     .   20,CONT,NUMNOD,ITAB,
     .   ITABG,LENG,NODGLOB,WEIGHT)

      IF(OUTP_V(5) == 1) CALL OUTP_N_V('INTE_FORCE',
     .   ' Internal Forces                        ',
     .   20,FINT,NUMNOD,ITAB,
     .   ITABG,LENG,NODGLOB,WEIGHT)

      IF(OUTP_V(6) == 1) CALL OUTP_N_V('EXTE_FORCE',
     .   ' External Forces                        ',
     .   20,FEXT,NUMNOD,ITAB,
     .   ITABG,LENG,NODGLOB,WEIGHT)

      IF(OUTP_V(9) == 1 .AND. (IDROT==1 .OR. ISECUT>0 .OR. ISUB>0 .OR.
     .                 MSHSUB>0 .OR. IISROT>0 .OR. IMPOSE_DR>0 ) .AND.
     .                 IRODDL/=0) 
     .                   CALL OUTP_N_V('VELO_ROT  ',
     .   ' Rotational Velocity                               ',
     .   20,VR,NUMNOD,ITAB,
     .   ITABG,LENG,NODGLOB,WEIGHT)

      IF(OUTP_V(12) == 1) THEN
        CALL OUTP_N_VC('PCONT_NORM',
     .   ' Contact Pressure from Normal Contact Forces      ',
     .   50,FNCONT,NUMNOD,ITAB,
     .   ITABG,LENG,NODGLOB,WEIGHT)
        CALL OUTP_N_VC('PCONT_TANG',
     .   ' Contact Pressure from Tangential Contact Forces  ',
     .   50,FTCONT,NUMNOD,ITAB,
     .   ITABG,LENG,NODGLOB,WEIGHT)
      END IF
C-----------------------------------------------
C   NODAL SCALAR
C   JBM LE 23/9/96
C   
C-----------------------------------------------
      INNODA=0
      IF(OUTP_N(1) == 1) CALL OUTP_NO('NODAL_DT  ',
     .   ' Nodal Time Step                        ',
     .   20,ANIN,NUMNOD,ITAB,INNODA,
     .   ITABG,LENG,NODGLOB,WEIGHT)

      IF(OUTP_N(1)+ANIM_N(1).GE.1)
     .   INNODA=INNODA+NUMNOD

      IF(OUTP_N(2) == 1) CALL OUTP_NO('NOD_ADMAS ',
     .   ' Added Mass                             ',
     .   20,ANIN,NUMNOD,ITAB,INNODA,
     .   ITABG,LENG,NODGLOB,WEIGHT)

      IF(OUTP_N(2)+ANIM_N(2).GE.1)
     .   INNODA=INNODA+NUMNOD

      IF(OUTP_N(3) == 1) CALL OUTP_NO('NOD_ADINER',
     .   ' Added Inertia                          ',
     .   20,ANIN,NUMNOD,ITAB,INNODA,
     .   ITABG,LENG,NODGLOB,WEIGHT)

C     IF(OUTP_N(3)+ANIM_N(12).GE.1)
C    .   INNODA=INNODA+NUMNOD

C-----------------------------------------------
C   SOLID SCALAR
C-----------------------------------------------
      IF (NSPMD > 1) THEN
        CALL OUTP_ARSZ_SS(IPARG,DD_IAD,IPM,IXS,SIZP0_SS,SIZLOC_SS,SIZWR_SS)
      ELSE
        ALLOCATE(SIZLOC_WRITE(2*NSPGROUP+2))
        CALL COUNT_ARSZ_SS(IPARG,DD_IAD,IPM,IXS,SIZLOC_SS,SIZLOC_WRITE)
        SIZP0_SS(:)=SIZLOC_SS(:)
        SIZWR_SS(1:2) = -1
        DO I=1,NSPGROUP
           SIZWR_SS(1) = MAX(SIZWR_SS(1),SIZLOC_WRITE(I))
           SIZWR_SS(2) = MAX(SIZWR_SS(2),SIZLOC_WRITE(NSPGROUP+I))
        ENDDO
        SIZWR_SS(1) = SIZWR_SS(1)+6
        SIZWR_SS(2) = SIZWR_SS(2)+6
        DEALLOCATE(SIZLOC_WRITE)
      ENDIF
      IF(OUTP_SS(1) == 1) CALL OUTP_S_S( 1,'OFF       ',
     .   ' Delete flag (1.0:On 0.0:Off)           ' 
     .   ,ELBUF_TAB,IPARG,EANI,IXS, IPM,DD_IAD,SIZLOC_SS(1),SIZP0_SS(1),SIZWR_SS(1))

      IF(OUTP_SS(2) == 1) CALL OUTP_S_S(10,'EPSP      ',
     .   ' Plastic Strain                         '
     .   ,ELBUF_TAB,IPARG,EANI,IXS, IPM,DD_IAD,SIZLOC_SS(1),SIZP0_SS(1),SIZWR_SS(1))

      IF(OUTP_SS(3) == 1) CALL OUTP_S_S( 3,'ENER      ',
     .   ' Internal Specific Energy               '
     .   ,ELBUF_TAB,IPARG,EANI,IXS, IPM,DD_IAD,SIZLOC_SS(1),SIZP0_SS(1),SIZWR_SS(1))

      IF(OUTP_SS(4) == 1) CALL OUTP_S_S( 4,'DENS      ',
     .   ' Density                                '
     .   ,ELBUF_TAB,IPARG,EANI,IXS, IPM,DD_IAD,SIZLOC_SS(1),SIZP0_SS(1),SIZWR_SS(1))

      IF(OUTP_SS(5) == 1) CALL OUTP_S_S(11,'TEMP      ',
     .   ' Temperature                            ' 
     .   ,ELBUF_TAB,IPARG,EANI,IXS, IPM,DD_IAD,SIZLOC_SS(1),SIZP0_SS(1),SIZWR_SS(1))

      IF(OUTP_SS(6) == 1) CALL OUTP_S_S( 2,'PRES      ',
     .   ' Pressure                               ' 
     .   ,ELBUF_TAB,IPARG,EANI,IXS, IPM,DD_IAD,SIZLOC_SS(1),SIZP0_SS(1),SIZWR_SS(1))

      IF(OUTP_SS(7) == 1) CALL OUTP_S_S(-2,'VONM      ',
     .   ' Von Mises Stress                       '
     .   ,ELBUF_TAB,IPARG,EANI,IXS, IPM,DD_IAD,SIZLOC_SS(1),SIZP0_SS(1),SIZWR_SS(1))

      IF(OUTP_SS(25) == 1) CALL OUTP_S_S(25,'HOUR      ',
     .   ' Hourglass Specific Energy              ' 
     .   ,ELBUF_TAB,IPARG,EANI,IXS, IPM,DD_IAD,SIZLOC_SS(1),SIZP0_SS(1),SIZWR_SS(1))

      IF(OUTP_SS(20) == 1) CALL OUTP_S_S(20,'USER1     ',
     .   ' User Variable 1                        '
     .   ,ELBUF_TAB,IPARG,EANI,IXS, IPM,DD_IAD,SIZLOC_SS(1),SIZP0_SS(1),SIZWR_SS(1))

      IF(OUTP_SS(21) == 1) CALL OUTP_S_S(21,'USER2     ',
     .   ' User Variable 2                        '
     .   ,ELBUF_TAB,IPARG,EANI,IXS, IPM,DD_IAD,SIZLOC_SS(1),SIZP0_SS(1),SIZWR_SS(1))

      IF(OUTP_SS(22) == 1) CALL OUTP_S_S(22,'USER3     ',
     .   ' User Variable 3                        '
     .   ,ELBUF_TAB,IPARG,EANI,IXS, IPM,DD_IAD,SIZLOC_SS(1),SIZP0_SS(1),SIZWR_SS(1))

      IF(OUTP_SS(23) == 1) CALL OUTP_S_S(23,'USER4     ',
     .   ' User Variable 4                        '
     .   ,ELBUF_TAB,IPARG,EANI,IXS, IPM,DD_IAD,SIZLOC_SS(1),SIZP0_SS(1),SIZWR_SS(1))

      IF(OUTP_SS(24) == 1) CALL OUTP_S_S(24,'USER5     ',
     .   ' User Variable 5                        '
     .   ,ELBUF_TAB,IPARG,EANI,IXS, IPM,DD_IAD,SIZLOC_SS(1),SIZP0_SS(1),SIZWR_SS(1))

      IF(OUTP_SS(26) == 1)CALL OUTP_S_S(26,'USERS     ',
     .   ' All User Variable                      '
     .   ,ELBUF_TAB,IPARG,EANI,IXS, IPM,DD_IAD,SIZLOC_SS(2),SIZP0_SS(2),SIZWR_SS(2))
C-----------------------------------------------
C   element equivalent stress criteria
C-----------------------------------------------
      IF(OUTP_SS(27) == 1)CALL OUTP_S_S(26,'EQ stress  ',
     .   ' Equivalent Stress                      '
     .   ,ELBUF_TAB,IPARG,EANI,IXS, IPM,DD_IAD,SIZLOC_SS(2),SIZP0_SS(2),SIZWR_SS(2))
C-----------------------------------------------
C   SHELL SCALAR
C-----------------------------------------------

        IF (NSPMD > 1) THEN
          CALL OUTP_ARSZ_CS(IPARG,IXC,IXTG,IGEO,IPM,DD_IAD,
     .                       SIZP0_CS,SIZLOC_CS,SIZWR_CS)
        ELSE
          ALLOCATE(SIZLOC_WRITE(2*NSPGROUP+2))
          CALL COUNT_ARSZ_CS(IPARG,IXC,IXTG,IGEO,IPM,DD_IAD,
     .                       SIZLOC_CS,SIZLOC_WRITE)
          SIZP0_CS(1:2)=SIZLOC_CS(1:2)+6
          SIZWR_CS(1:2) = -1
          DO I=1,NSPGROUP
           SIZWR_CS(1) = MAX(SIZWR_CS(1),SIZLOC_WRITE(I))
           SIZWR_CS(2) = MAX(SIZWR_CS(2),SIZLOC_WRITE(NSPGROUP+I))
          ENDDO
          SIZWR_CS(1) = SIZWR_CS(1)+6
          SIZWR_CS(2) = SIZWR_CS(2)+6
          DEALLOCATE(SIZLOC_WRITE)
        ENDIF

      IF (OUTP_CS(1) == 1) CALL OUTP_C_S( 6,'OFF        ',
     .   ' Delete flag (1.0:On 0.0:Off)           '
     .   ,ELBUF_TAB,IPARG,EANI  ,IPM ,IGEO,
     .    IXC  ,IXTG ,DD_IAD,SIZLOC_CS(1),SIZP0_CS(1),THKE,SIZWR_CS(1))

      IF (OUTP_CS(2) == 1) THEN
        CALL OUTP_C_S(15,'EPSP       ',
     .   ' Plastic Strain                         ' 
     .   ,ELBUF_TAB,IPARG,EANI  ,IPM ,IGEO,IXC  ,IXTG ,DD_IAD,SIZLOC_CS(1),
     .    SIZP0_CS(1),THKE,SIZWR_CS(1) )

        CALL OUTP_C_S(-15,'EPSP_MAX   ',
     .   ' Maximum Plastic Strain                 '
     .   ,ELBUF_TAB,IPARG,EANI  ,IPM ,IGEO,IXC  ,IXTG ,DD_IAD,SIZLOC_CS(1),
     .    SIZP0_CS(1),THKE,SIZWR_CS(1) )

      ENDIF

      IF (OUTP_CS(3) == 1) CALL OUTP_C_S( 5,'ENER       ',
     .   ' Internal Energy                        '
     .   ,ELBUF_TAB,IPARG,EANI  ,IPM ,IGEO,
     .    IXC  ,IXTG ,DD_IAD,SIZLOC_CS(1),SIZP0_CS(1),THKE,SIZWR_CS(1) )

      IF (OUTP_CS(4) == 1) CALL OUTP_C_S( 3,'THICK      ',
     .   ' Thickness                               '
     .   ,ELBUF_TAB,IPARG,EANI  ,IPM ,IGEO,
     .    IXC  ,IXTG ,DD_IAD,SIZLOC_CS(1),SIZP0_CS(1),THKE,SIZWR_CS(1) )

      IF (OUTP_CS(7) == 1) CALL OUTP_C_S( 1,'VONM       ',
     .   ' Membrane Von Mises Stress              ' 
     .   ,ELBUF_TAB,IPARG,EANI  ,IPM ,IGEO,
     .    IXC  ,IXTG ,DD_IAD,SIZLOC_CS(1),SIZP0_CS(1),THKE,SIZWR_CS(1))

      IF (OUTP_CS(25) == 1) CALL OUTP_C_S(25,'HOUR       ',
     .   ' Hourglass Energy                       ' 
     .   ,ELBUF_TAB,IPARG,EANI  ,IPM ,IGEO,
     .    IXC  ,IXTG ,DD_IAD,SIZLOC_CS(1),SIZP0_CS(1),THKE,SIZWR_CS(1))

      IF (OUTP_CS(20) == 1) THEN
        CALL OUTP_C_S(20,'USER1      ',
     .   ' User Variable 1                        '
     .   ,ELBUF_TAB,IPARG,EANI  ,IPM ,IGEO,IXC  ,IXTG ,DD_IAD,
     .    SIZLOC_CS(1),SIZP0_CS(1),THKE,SIZWR_CS(1) )

        CALL OUTP_C_S(-20,'USER1_MAX  ',
     .   ' Maximum User Variable 1                '
     .   ,ELBUF_TAB,IPARG,EANI  ,IPM ,IGEO,IXC  ,IXTG ,DD_IAD,
     .    SIZLOC_CS(1),SIZP0_CS(1),THKE,SIZWR_CS(1))

      ENDIF

      IF (OUTP_CS(21) == 1) THEN
        CALL OUTP_C_S(21,'USER2      ',
     .   ' User Variable 2                        '
     .   ,ELBUF_TAB,IPARG,EANI  ,IPM ,IGEO,IXC  ,IXTG ,DD_IAD,
     .    SIZLOC_CS(1),SIZP0_CS(1),THKE,SIZWR_CS(1) )

        CALL OUTP_C_S(-21,'USER2_MAX  ',
     .   ' Maximum User Variable 2                '
     .   ,ELBUF_TAB,IPARG,EANI  ,IPM ,IGEO,IXC  ,IXTG ,DD_IAD,
     .    SIZLOC_CS(1),SIZP0_CS(1),THKE,SIZWR_CS(1) )

      ENDIF

      IF (OUTP_CS(22) == 1) THEN
        CALL OUTP_C_S(22,'USER3      ',
     .   ' User Variable 3                        '
     .   ,ELBUF_TAB,IPARG,EANI  ,IPM ,IGEO,IXC  ,IXTG ,DD_IAD,
     .    SIZLOC_CS(1),SIZP0_CS(1),THKE,SIZWR_CS(1))

        CALL OUTP_C_S(-22,'USER3_MAX  ',
     .   ' Maximum User Variable 3                '
     .   ,ELBUF_TAB,IPARG,EANI  ,IPM ,IGEO,IXC  ,IXTG ,DD_IAD,
     .    SIZLOC_CS(1),SIZP0_CS(1),THKE,SIZWR_CS(1) )

      ENDIF

      IF (OUTP_CS(23) == 1) THEN
        CALL OUTP_C_S(23,'USER4      ',
     .   ' User Variable 4                        '
     .   ,ELBUF_TAB,IPARG,EANI  ,IPM ,IGEO,IXC  ,IXTG ,DD_IAD,
     .    SIZLOC_CS(1),SIZP0_CS(1),THKE,SIZWR_CS(1) )

        CALL OUTP_C_S(-23,'USER4_MAX  ',
     .   ' Maximum User Variable 4                '
     .   ,ELBUF_TAB,IPARG,EANI  ,IPM ,IGEO,IXC  ,IXTG ,DD_IAD,
     .    SIZLOC_CS(1),SIZP0_CS(1),THKE,SIZWR_CS(1) )

      ENDIF

      IF (OUTP_CS(24) == 1) THEN
        CALL OUTP_C_S(24,'USER5      ',
     .   ' User Variable 5                        '
     .   ,ELBUF_TAB,IPARG,EANI  ,IPM ,IGEO,IXC  ,IXTG ,DD_IAD,
     .    SIZLOC_CS(1),SIZP0_CS(1),THKE,SIZWR_CS(1) )

        CALL OUTP_C_S(-24,'USER5_MAX  ',
     .   ' Maximum User Variable 5                '
     .   ,ELBUF_TAB,IPARG,EANI  ,IPM ,IGEO,IXC  ,IXTG ,DD_IAD,
     .    SIZLOC_CS(1),SIZP0_CS(1),THKE,SIZWR_CS(1) )

      ENDIF
      
      IF (OUTP_CS(26) == 1)CALL OUTP_C_S(26,'USERS      ',
     .   ' All User Variables                         '
     .   ,ELBUF_TAB,IPARG,EANI  ,IPM ,IGEO,IXC  ,IXTG ,DD_IAD,
     .    SIZLOC_CS(2),SIZP0_CS(2),THKE,SIZWR_CS(2) ) 
       DO J = 27, 86
         JJ = J - 26
        write(CHAR_JJ,'(I2.2)')JJ
        VAR_CHAR = 'USER'//CHAR_JJ//'_FULL'
        TITRE = ' User Variable'//CHAR_JJ//'             '
        IF(OUTP_CS(J) == 1)CALL OUTP_C_S(J,VAR_CHAR,
     .    TITRE ,
     .    ELBUF_TAB,IPARG,EANI  ,IPM ,IGEO,IXC  ,IXTG ,DD_IAD,
     .    SIZLOC_CS(2),SIZP0_CS(2),THKE,SIZWR_CS(2) ) 
       ENDDO 
C-----------------------------------------------
C   element equivalent stress criteria
C-----------------------------------------------
      IF (OUTP_CS(87) == 1) CALL OUTP_C_S(87,'Eq stress  ',
     .   ' Membrane Equivalent Stress             '
     .   ,ELBUF_TAB,IPARG,EANI  ,IPM ,IGEO,
     .    IXC  ,IXTG ,DD_IAD,SIZLOC_CS(2),SIZP0_CS(2),THKE,SIZWR_CS(2) )
C-----------------------------------------------
C   SPH SCALAR
C-----------------------------------------------
       IF (NSPMD > 1) THEN
         CALL OUTP_ARSZ_SPS(IPARG,DD_IAD,SIZLOC,SIZP0,SIZW_P0)
       ELSE
         ALLOCATE(SIZLOC_WRITE(NSPGROUP+1))
         CALL COUNT_ARSZ_SPS(IPARG,DD_IAD,SIZLOC,SIZLOC_WRITE)
         SIZP0 = SIZLOC
         SIZW_P0 = -1
         DO I=1,NSPGROUP
          SIZW_P0 = MAX(SIZW_P0,SIZLOC_WRITE(I))
         ENDDO
         SIZW_P0 = SIZW_P0 + 6    
         DEALLOCATE(SIZLOC_WRITE)
       ENDIF

      IF(OUTP_SPS(1) == 1) CALL OUTP_SP_S( 1,'OFF       ',
     .   ' Delete flag (1.0:On 0.0:Off)           ' 
     .   ,ELBUF_TAB,IPARG,EANI,DD_IAD,KXSP,IPM,SPBUF,SIZLOC,SIZP0,SIZW_P0)

      IF(OUTP_SPS(2) == 1) CALL OUTP_SP_S(10,'EPSP      ',
     .   ' Plastic Strain                         '
     .   ,ELBUF_TAB,IPARG,EANI,DD_IAD,KXSP,IPM,SPBUF,SIZLOC,SIZP0,SIZW_P0)

      IF(OUTP_SPS(3) == 1) CALL OUTP_SP_S( 3,'ENER      ',
     .   ' Internal Specific Energy               '
     .   ,ELBUF_TAB,IPARG,EANI,DD_IAD,KXSP,IPM,SPBUF,SIZLOC,SIZP0,SIZW_P0)

      IF(OUTP_SPS(4) == 1) CALL OUTP_SP_S( 4,'DENS      ',
     .   ' Density                                '
     .   ,ELBUF_TAB,IPARG,EANI,DD_IAD,KXSP,IPM,SPBUF,SIZLOC,SIZP0,SIZW_P0) 

      IF(OUTP_SPS(5) == 1) CALL OUTP_SP_S(11,'TEMP      ',
     .   ' Temperature                            ' 
     .   ,ELBUF_TAB,IPARG,EANI,DD_IAD,KXSP,IPM,SPBUF,SIZLOC,SIZP0,SIZW_P0)

      IF(OUTP_SPS(6) == 1) CALL OUTP_SP_S( 2,'PRES      ',
     .   ' Pressure                               ' 
     .   ,ELBUF_TAB,IPARG,EANI,DD_IAD,KXSP,IPM,SPBUF,SIZLOC,SIZP0,SIZW_P0)

      IF(OUTP_SPS(7) == 1) CALL OUTP_SP_S(-2,'VONM      ',
     .   ' Von Mises Stress                       '
     .   ,ELBUF_TAB,IPARG,EANI,DD_IAD,KXSP,IPM,SPBUF,SIZLOC,SIZP0,SIZW_P0)

      IF(OUTP_SPS(25) == 1) CALL OUTP_SP_S(25,'SLEN      ',
     .   ' Smoothing length                       ' 
     .   ,ELBUF_TAB,IPARG,EANI,DD_IAD,KXSP,IPM,SPBUF,SIZLOC,SIZP0,SIZW_P0)

      IF(OUTP_SPS(20) == 1) CALL OUTP_SP_S(20,'USER1     ',
     .   ' User Variable 1                        '
     .   ,ELBUF_TAB,IPARG,EANI,DD_IAD,KXSP,IPM,SPBUF,SIZLOC,SIZP0,SIZW_P0)

      IF(OUTP_SPS(21) == 1) CALL OUTP_SP_S(21,'USER2     ',
     .   ' User Variable 2                        '
     .   ,ELBUF_TAB,IPARG,EANI,DD_IAD,KXSP,IPM,SPBUF,SIZLOC,SIZP0,SIZW_P0)

      IF(OUTP_SPS(22) == 1) CALL OUTP_SP_S(22,'USER3     ',
     .   ' User Variable 3                        '
     .   ,ELBUF_TAB,IPARG,EANI,DD_IAD,KXSP,IPM,SPBUF,SIZLOC,SIZP0,SIZW_P0)

      IF(OUTP_SPS(23) == 1) CALL OUTP_SP_S(23,'USER4     ',
     .   ' User Variable 4                        '
     .   ,ELBUF_TAB,IPARG,EANI,DD_IAD,KXSP,IPM,SPBUF,SIZLOC,SIZP0,SIZW_P0)

      IF(OUTP_SPS(24) == 1) CALL OUTP_SP_S(24,'USER5     ',
     .   ' User Variable 5                        '
     .   ,ELBUF_TAB,IPARG,EANI,DD_IAD,KXSP,IPM,SPBUF,SIZLOC,SIZP0,SIZW_P0)
C-----------------------------------------------
C   element equivalent stress criteria
C-----------------------------------------------
      IF(OUTP_SPS(26) == 1) CALL OUTP_SP_S(24,'Eq stress ',
     .   ' Equivalent Stress                      '
     .   ,ELBUF_TAB,IPARG,EANI,DD_IAD,KXSP,IPM,SPBUF,SIZLOC,SIZP0,SIZW_P0)
C-----------------------------------------------
C   SOLID TENSOR(3D)
C-----------------------------------------------
       IF (NSPMD > 1) THEN
         CALL OUTP_ARSZ_ST(IPARG,DD_IAD,SIZLOC_ST,SIZWR_ST,SIZP0_ST)
       ELSE
         SIZP0_ST = -1
         SIZLOC_ST = -1
         ALLOCATE( SIZLOC_WRITE(3*NSPGROUP+3) )
         CALL COUNT_ARSZ_ST(IPARG,DD_IAD,SIZLOC_ST,SIZLOC_WRITE)
         SIZP0_ST(1:3) = SIZLOC_WRITE(3*NSPGROUP+1:3*NSPGROUP+3)+8
         SIZWR_ST(1:3) = -1
         DO I=1,NSPGROUP
          SIZWR_ST(1) = MAX(SIZWR_ST(1),SIZLOC_WRITE(I)) 
          SIZWR_ST(2) = MAX(SIZWR_ST(2),SIZLOC_WRITE(NSPGROUP+I)) 
          SIZWR_ST(3) = MAX(SIZWR_ST(3),SIZLOC_WRITE(2*NSPGROUP+I))
         ENDDO
         SIZWR_ST(1:3)= SIZWR_ST(1:3)+8
         DEALLOCATE( SIZLOC_WRITE )
       ENDIF

      IF(OUTP_ST(1) == 1) CALL OUTP_S_T( 2,'STRESS    ',
     .   ' Stress                                 ',
     .   ELBUF_TAB,IPARG,DD_IAD,SIZLOC_ST(1),SIZP0_ST(1),SIZWR_ST(1))

       IF(OUTP_ST(2) == 1) CALL OUTP_S_TT( 2,'STR_FUL    ',
     .   ' Full stress tensor + plastic strain    ',
     .   ELBUF_TAB,IPARG,DD_IAD,IPM, IXS,
     .   SIZLOC_ST(2),SIZP0_ST(2),SIZWR_ST(2))

       IF(OUTP_ST(3) == 1) CALL OUTP_S_TT( 3,'STRAIN_FUL ',
     .   ' Full strain tensor                     ',
     .   ELBUF_TAB,IPARG,DD_IAD,IPM, IXS,
     .   SIZLOC_ST(3),SIZP0_ST(3),SIZWR_ST(3))
C-----------------------------------------------
C   SHELL TENSOR(2D)
C-----------------------------------------------
      IF (NSPMD > 1) THEN
        CALL OUTP_ARSZ_CT(IPARG,DD_IAD,SIZLOC_CT,SIZP0_CT,SIZWR_CT,ELBUF_TAB)
      ELSE
        ALLOCATE(SIZLOC_WRITE(3*NSPGROUP+3))
        CALL COUNT_ARSZ_CT(IPARG,DD_IAD,SIZLOC_CT,SIZLOC_WRITE,ELBUF_TAB)
        SIZP0_CT(1)=SIZLOC_CT(1)
        SIZP0_CT(2:3)=SIZLOC_CT(2:3)
        SIZWR_CT(1:3)=-1
        DO I=1,NSPGROUP
         SIZWR_CT(1) = MAX(SIZWR_CT(1),SIZLOC_WRITE(I))
         SIZWR_CT(2) = MAX(SIZWR_CT(2),SIZLOC_WRITE(NSPGROUP+I))
         SIZWR_CT(3) = MAX(SIZWR_CT(3),SIZLOC_WRITE(2*NSPGROUP+I))
        ENDDO
        SIZWR_CT(1) = SIZWR_CT(1) + 6
        DEALLOCATE(SIZLOC_WRITE)
      ENDIF

      IF(OUTP_CT(1) == 1) CALL OUTP_C_T( 1,'STRESS_MEM',
     .   ' Membrane Stress                        ',
     .      ELBUF_TAB,IPARG,TANI,DD_IAD,SIZLOC_CT(1),SIZP0_CT(1),SIZWR_CT(1)  )

      IF(OUTP_CT(2) == 1) CALL OUTP_C_T( 2,'STRESS_BEN',
     .   ' Bending Stress                         ',
     .      ELBUF_TAB,IPARG,TANI,DD_IAD,SIZLOC_CT(1),SIZP0_CT(1),SIZWR_CT(1)  )

      IF(OUTP_CT(3) == 1) CALL OUTP_C_T( 3,'STRESS_UPP',
     .   ' Upper Surface Stress                   ',
     .      ELBUF_TAB,IPARG,TANI,DD_IAD,SIZLOC_CT(1),SIZP0_CT(1),SIZWR_CT(1)  )

      IF(OUTP_CT(4) == 1) CALL OUTP_C_T( 4,'STRESS_LOW',
     .   ' Lower Surface Stress                   ',
     .      ELBUF_TAB,IPARG,TANI,DD_IAD,SIZLOC_CT(1),SIZP0_CT(1),SIZWR_CT(1)  )

      IF(OUTP_CT(5) == 1) CALL OUTP_C_T( 5,'STRAIN_MEM',
     .   ' Membrane Strain                        ',
     .      ELBUF_TAB,IPARG,TANI,DD_IAD,SIZLOC_CT(1),SIZP0_CT(1),SIZWR_CT(1)  )

      IF(OUTP_CT(6) == 1) CALL OUTP_C_T( 6,'STRAIN_BEN',
     .   ' Bending Strain                         ',
     .      ELBUF_TAB,IPARG,TANI,DD_IAD,SIZLOC_CT(1),SIZP0_CT(1),SIZWR_CT(1)  )

      IF(OUTP_CT(7) == 1) CALL OUTP_C_T( 7,'STRAIN_UPP',
     .   ' Upper Surface Strain                   ',
     .      ELBUF_TAB,IPARG,TANI,DD_IAD,SIZLOC_CT(1),SIZP0_CT(1),SIZWR_CT(1)  )

      IF(OUTP_CT(8) == 1) CALL OUTP_C_T( 8,'STRAIN_LOW',
     .   ' Lower Surface Strain                   ',
     .      ELBUF_TAB,IPARG,TANI,DD_IAD,SIZLOC_CT(1),SIZP0_CT(1),SIZWR_CT(1)  )

      IF(OUTP_CT(91) == 1) CALL OUTP_C_T(91,'EPSDOT_MEM',
     .   ' Membrane Strain Rate                   ',
     .      ELBUF_TAB,IPARG,TANI,DD_IAD,SIZLOC_CT(1),SIZP0_CT(1),SIZWR_CT(1)  )

      IF(OUTP_CT(92) == 1) CALL OUTP_C_T(92,'EPSDOT_BEN',
     .   ' Bending Strain Rate                    ',
     .   ELBUF_TAB,IPARG,TANI,DD_IAD,SIZLOC_CT(1),SIZP0_CT(1),SIZWR_CT(1))

      IF(OUTP_CT(93) == 1) CALL OUTP_C_T(93,'EPSDOT_UPP',
     .   ' Upper Surface Strain Rate              ',
     .   ELBUF_TAB,IPARG,TANI,DD_IAD,SIZLOC_CT(1),SIZP0_CT(1),SIZWR_CT(1))

      IF(OUTP_CT(94) == 1) CALL OUTP_C_T(94,'EPSDOT_LOW',
     .   ' Lower Surface Strain Rate              ',
     .      ELBUF_TAB,IPARG,TANI,DD_IAD,SIZLOC_CT(1),SIZP0_CT(1),SIZWR_CT(1)  )

      DO I=1,30
       IF(OUTP_CT(10+I) == 1)THEN
          WRITE(MES,'(A,I2,A)')
     .   ' Stress (layer',I,')                       ' 
          WRITE(T10,'(A,I2)')'STRESS_L',I
          CALL OUTP_C_T( 10+I,T10   ,MES,
     .         ELBUF_TAB,IPARG,TANI,DD_IAD,SIZLOC_CT(1),SIZP0_CT(1),SIZWR_CT(1)  )
       ENDIF

       IF(OUTP_CT(50+I) == 1)THEN
          WRITE(MES,'(A,I2,A)') 
     .   ' Strain (layer',I,')                       ' 
          WRITE(T10,'(A,I2)')'STRAIN_L',I
          CALL OUTP_C_T( 50+I,T10   ,MES,
     .         ELBUF_TAB,IPARG,TANI,DD_IAD,SIZLOC_CT(1),SIZP0_CT(1),SIZWR_CT(1)  )
       ENDIF

       IF(OUTP_CT(100+I) == 1)THEN
          WRITE(MES,'(A,I2,A)') 
     .   ' Epsdot (layer',I,')                       ' 
          WRITE(T10,'(A,I2)')'EPSDOT_L',I
          CALL OUTP_C_T( 100+I,T10   ,MES,
     .         ELBUF_TAB,IPARG,TANI,DD_IAD,SIZLOC_CT(1),SIZP0_CT(1),SIZWR_CT(1)  )
       ENDIF
      ENDDO

      IF(OUTP_CT(95) == 1) CALL OUTP_C_TF(95,'STRESS_FUL',
     .   ' Full stress tensor + plastic strain    ',
     .     ELBUF_TAB,IPARG ,DD_IAD,SIZLOC_CT(2),SIZP0_CT(2),THKE,GEO,
     .     IGEO     ,STACK ,DRAPE_Q4, DRAPE_T3,IXC  ,IXTG,SIZWR_CT(2))


      IF(OUTP_CT(96) == 1) CALL OUTP_C_TF(96,'STRAIN_FUL',
     .   ' Full strain tensor + plastic strain    ',
     .     ELBUF_TAB,IPARG ,DD_IAD,SIZLOC_CT(3),SIZP0_CT(3),THKE,GEO,
     .     IGEO     ,STACK ,DRAPE_Q4, DRAPE_T3,IXC  ,IXTG,SIZWR_CT(3))

C-----------------------------------------------
C   SPH TENSOR(3D)
C-----------------------------------------------
       IF (NSPMD > 1) THEN
         CALL OUTP_ARSZ_SPT(IPARG,DD_IAD,SIZLOC,SIZP0,SIZW_P0)
       ELSE
         ALLOCATE(SIZLOC_WRITE(NSPGROUP+1))
         CALL COUNT_ARSZ_SPT(IPARG,DD_IAD,SIZLOC,SIZLOC_WRITE)
         SIZP0 = SIZLOC
         SIZW_P0 = -1
         DO I=1,NSPGROUP
          SIZW_P0 = MAX(SIZW_P0,SIZLOC_WRITE(I))
         ENDDO 
         SIZW_P0 = SIZW_P0 + 6
         DEALLOCATE(SIZLOC_WRITE)
       ENDIF

      IF(OUTP_SPT(1) == 1) CALL OUTP_SP_T('STRESS    ',
     .   ' Stress                                 ',
     .   ELBUF_TAB,IPARG,DD_IAD,SIZLOC,SIZP0,SIZW_P0)

       IF (NSPMD > 1) THEN
         CALL OUTP_ARSZ_SPTT(IPARG,DD_IAD,SIZLOC,SIZP0,SIZW_P0)
       ELSE
         ALLOCATE(SIZLOC_WRITE(NSPGROUP+1))
         CALL COUNT_ARSZ_SPTT(IPARG,DD_IAD,SIZLOC,SIZLOC_WRITE)
         SIZP0 = SIZLOC
         SIZW_P0 = -1
         DO I=1,NSPGROUP
          SIZW_P0 = MAX(SIZW_P0,SIZLOC_WRITE(I))
         ENDDO  
         DEALLOCATE(SIZLOC_WRITE)
       ENDIF

      IF(OUTP_SPT(2) == 1) CALL OUTP_SP_TT('STR_FUL    ',
     .   ' Full stress tensor + plastic strain    ',
     .   ELBUF_TAB,IPARG,DD_IAD,IPM, KXSP,SPBUF,SIZLOC,SIZP0,SIZW_P0)
C-----------------------------------------------
C   SPRING SCALAR
C-----------------------------------------------
      IF (NSPMD > 1) THEN
        CALL OUTP_ARSZ_RS(IPARG,DD_IAD,SIZLOC,SIZP0,SIZW_P0)
      ELSE
        ALLOCATE(SIZLOC_WRITE(NSPGROUP+1))
        CALL COUNT_ARSZ_RS(IPARG,DD_IAD,SIZLOC,SIZLOC_WRITE)
        SIZP0 = SIZLOC_WRITE(NSPGROUP+1)
        SIZW_P0 = -1
        DO I=1,NSPGROUP
         SIZW_P0 = MAX(SIZW_P0,SIZLOC_WRITE(I))
        ENDDO  
        SIZW_P0 = SIZW_P0 + 6
        DEALLOCATE(SIZLOC_WRITE)
      ENDIF

      IF(OUTP_RS(1) == 1)CALL OUTP_R_S(1,'OFF       ', 
     .   ' Delete flag (1.0:On 0.0:Off)           ' 
     .   ,ELBUF_TAB,IPARG,DD_IAD,SIZLOC,SIZP0,SIZW_P0)  

C-----------------------------------------------
C   springs forces and moments
C-----------------------------------------------
      IF (NSPMD > 1) THEN
       CALL OUTP_ARSZ_RT(IPARG,IGEO,GEO,IXR,DD_IAD,
     .                   SIZLOC,SIZP0,SIZW_P0)
      ELSE
        ALLOCATE(SIZLOC_WRITE(NSPGROUP+1))
        CALL COUNT_ARSZ_RT(IPARG,IGEO,GEO,IXR,DD_IAD,
     .                     SIZLOC,SIZLOC_WRITE)
        SIZP0 = SIZLOC
        SIZW_P0 = -1
        DO I=1,NSPGROUP
         SIZW_P0 = MAX(SIZW_P0,SIZLOC_WRITE(I))
        ENDDO   
        SIZW_P0 = SIZW_P0 + 6 
        DEALLOCATE(SIZLOC_WRITE)
      ENDIF
C
      IF(OUTP_RS(2) == 1)CALL OUTP_R_T('FULL    ',
     .   ' forces and moments                     ',
     .   ELBUF_TAB,IPARG,DD_IAD,IXR,IGEO,GEO,SIZLOC,SIZP0,SIZW_P0)     

C-----------------------------------------------
C   RBODIES+SECTIONS+RWALLS FORCES (FOPT)
C-----------------------------------------------
      IF(OUTP_V(8) == 1) 
     .  CALL OUTP_N_V2(FOPT ,NPBY ,NOM_OPT(I16G),NOM_OPT(I16D))
C-----------------------------------------------
C   END
C-----------------------------------------------

      IF(ISPMD == 0) THEN
        WRITE(IUGEO,'(A)')'/ENDDATA   '
        CLOSE(UNIT=IUGEO)
        CALL FILELEN_SYS(FILNAM,FILEN, OUTPSIZE)
        OUTPFILESIZE=OUTPFILESIZE + OUTPSIZE/1024
        WRITE (IOUT,1000)  FILNAM(1:FILEN)
        WRITE (ISTDO,1000) FILNAM(1:FILEN)
      ENDIF
 1000 FORMAT (4X,' OUTPUT FILE:',1X,A,' WRITTEN')
C-----------
      RETURN
      END
