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
Chd|  INIT_MONVOL                   source/airbag/init_monvol.F   
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ALENO                         source/airbag/fvmbag1.F       
Chd|        ANCMSG                        source/output/message/message.F
Chd|        CONSTIT                       source/elements/nodes/constit.F
Chd|        FVBRIC0                       source/airbag/fvbric0.F       
Chd|        FVBRIC01                      source/airbag/fvbric01.F      
Chd|        FVBRIC1                       source/airbag/fvbric1.F       
Chd|        FVBRIC2                       source/airbag/fvbric2.F       
Chd|        FVBRIC3                       source/airbag/fvbric3.F       
Chd|        FVELPRINT                     source/airbag/fvelprint.F     
Chd|        FVELSURF                      source/airbag/fvelsurf.F      
Chd|        FVINJECT                      source/airbag/fvinject.F      
Chd|        FVNODI                        source/airbag/fvmbag1.F       
Chd|        FVVENTHOLE                    source/airbag/fvventhole.F    
Chd|        HYPERMESH_TETRA               stub/fvmbags_stub.F           
Chd|        LOAD_MESHGEMS_LIBRARY         stub/fvmbags_stub.F           
Chd|        MESHGEMS_TETRA                stub/fvmbags_stub.F           
Chd|        MONVOL_TRIANGULATE_SURFACE    source/airbag/monvol_triangulate_surface.F
Chd|        WRITEMESH                     source/airbag/writeMesh.F     
Chd|        FVMBAG_MESHCONTROL_MOD        ../common_source/modules/fvmbag_meshcontrol_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MONVOL_STRUCT_MOD             share/modules1/monvol_struct_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|====================================================================
      SUBROUTINE INIT_MONVOL(T_MONVOL, T_MONVOL_METADATA,
     2     GEO     ,PM      ,
     3     IXC     ,IXTG    ,X       ,NPC     ,
     4     ITABM1  ,ITAB    ,IGRSURF ,
     5     LSENSOR ,IGRBRIC ,MFI     ,
     6     NAIRWA  ,IXS     ,V       ,LIBAGALE,
     7     NPT     ,PLD     ,LRBAGALE,IPM     ,IGEO    ,
     8     UNITAB  ,SENSOR)
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE R2R_MOD
      USE MESSAGE_MOD
      USE FVMBAG_MESHCONTROL_MOD
      USE GROUPDEF_MOD
      USE MONVOL_STRUCT_MOD
C-----------------------------------------------
C     I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C     A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C     C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "sysunit.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C     D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER  IXC(NIXC,*),IXTG(NIXTG,*),NPC(*),
     .     ITABM1(*),ITAB(*),
     .     LSENSOR(NSENSI,*),
     .     MFI,NAIRWA ,IXS(NIXS,*),
     .     LIBAGALE,NPT(*),LRBAGALE,IPM(NPROPMI,*),IGEO(NPROPGI,*)
C     REAL ou REAL*8
      my_real
     .     GEO(NPROPG,*), PM(NPROPM,*), X(3,*), V(3,*),
     .     PLD(2,*), MEMBAG, 
     .     SENSOR(NSENSR,*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
      TYPE(MONVOL_STRUCT_), DIMENSION(NVOLU), INTENT(INOUT) :: T_MONVOL
      TYPE(MONVOL_METADATA_), INTENT(INOUT) :: T_MONVOL_METADATA
C-----------------------------------------------
C     L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: NN, MM, LCA, LIBJET, LRBJET, LIBHOL, LRBHOL
      INTEGER I, J, J1, ID, UID, ITYPE, K, NC, NCA, N, IEQUI,
     .     IMASS, ITEMP, IFLU, IFUNC, ISENS, IS, NUM,
     .     IPVENT, IJET,NJ1,NJ2,NJ3,IPT,IPA,IPZ,ISH34,
     .     NN1,NNI,IAD,IAD1,NTGI,I1,I2,I3,I4,P,
     .     IPORT,IPORP,IPORA,IPORT1,IPORP1,IPORA1,
     .     NJET,NVENT,
     .     NGROOT,IDEF,
     .     DECREASE, IP, NP, IOK, IVEL, ITVENT, NBX, NBY, NBZ,
     .     IBRIC, IGMERG, ILVOUT, NLAYER, NFACMAX, NPPMAX, IADALE,
     .     RADALE, NNS, IADALE2, NNO, NTG, IADALE3, IADALE4, NBRIC,
     .     IADALE5, IADALE6, IADALE7, IADALE8, NTGA, NNA, IADALE9,
     .     IADALE10, IADALE11,MID_INI,I_INJ,I_INJSYS,MID,NGASES,
     .     IFVENT,ICURJETMAX,ICURJETMAX_OK,MID_INISYS, IADALE12,
     .     ITTF,IDTPDEF,NVB,KFRAME,IREF,IFOUND,JI,JI1,ITY,ITY1,
     .     NEL,CHKSURF,ITYPFUN,NVENTHOLES,NPORSURF,IBLOCKAGE,IVENTYP,
     .     IE, NE, 
     .     IERR1, IERR2, IERR3, IADBRIC, IADALE13, NTGT, II, IBID(1), JJ, ITMP
      INTEGER IVDP,IFLAGUNIT,ID1,ID2, NSWITCH, AUTO_UP_SWITCH
      INTEGER TITREVENT(20), MAX_ID
      INTEGER IFBULK, IFMIN, IFMOUTT, IFMOUTP, IFP0, IFPMAX, IFUNCT_ID
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAGE, ITABINV, TAGVENT,
     .     ITABINV1, ITABINV2, INDEX
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAGNODBR
      INTEGER, DIMENSION(:), ALLOCATABLE :: ITAB_SAVE
      INTEGER :: RET_VAL,NFVMBAGS
      my_real :: PARAM, TMP
      INTEGER :: IMESH_ALL, ICOPY, NJET_NEW(NVOLU)
      
C     REAL ou REAL*8
      my_real
     .     AMU, PEXT, PINI, TI,  RHOI, EI,RHOEI,
     .     GAMAI, CPAI, CPBI, CPCI, CPDI, CPEI, CPFI,
     .     RMWI, MI, CVI, CPI,MWI,
     .     GAMA, CPA, CPB, CPC, CPD, CPE, CPF,
     .     RMWG, CPG, CVG,
     .     FMASS, FTEMP, FVDP,FAC_M,FAC_L,FAC_T,FAC_C,
     .     DPDEF, DTPDEF, AVENT, BVENT, TVENT, TSTOPE, CLEAK,
     .     CP, CV, NX, NY, NZ, VOL, ROT, DIR,
     .     XX, YY, ZZ, X13, Y13, Z13, X24, Y24, Z24,
     .     X12,Y12,Z12,DS,BID,
     .     SV, SX, SY, SZ, SA, VINC, PMAX,VEPS,VMIN,
     .     FFUN,FPORT,FPORP,FPORA,FPORT1,FPORP1,FPORA1,FPT,FPA,FPZ,
     .     SCAL_T,SCAL_P,SCAL_S,SCAL_A,SCAL_D,SJET,SHOL,FVEL,DIRX,DIRY,
     .     DIRZ,DIR2X,DIR2Y,DIR2Z,ORIGX,ORIGY,ORIGZ,LX,LY,LZ,CGMERG,
     .     CNMERG,PTOLE,QA,QB,HMIN,MW_TMP,R_IGC1,HCONV,ESPECI,TI2,
     .     TTFIRE,TRELAX,TSWITCH,CFL_COEF, DTMIN
      my_real
     .     SFBULK, SFMIN, SFMOUTT, SFMOUTP, SFP0, SFPMAX
C     
      CHARACTER MESS*40,KEY*ncharkey,TITR*nchartitle,
     .     TITRSURF*nchartitle,TITR1*nchartitle,
     .     TITR2*nchartitle,TITR3*nchartitle
      CHARACTER*10 MONVOLTYP(11)
      CHARACTER*20 VENTTITLE
      INTEGER, DIMENSION(:), ALLOCATABLE :: FVBAG_ELEMID
      INTEGER :: NB_TETRA, NB_VERTICES, NB_EXTRA_VERTICES
      INTEGER :: EXT_SURFID, INT_SURFID
      LOGICAL :: FOUND, MGM_USED
      INTEGER :: SIZE1, SIZE2
      INTEGER :: COMM_TAG(NVOLU), NCOMMU
      INTEGER, DIMENSION(:, :), ALLOCATABLE :: IBAGJET_SAVE
      my_real, DIMENSION(:, :), ALLOCATABLE :: RBAGJET_SAVE
C-----------------------------------------------
C     E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
      EXTERNAL USR2SYS
C-----------------------------------------------
C     D a t a
C-----------------------------------------------
      DATA MESS/'MONITORED VOLUME DEFINITION             '/
      DATA MONVOLTYP/'      AREA','      PRES','       GAS',
     .     '    AIRBAG','     COMMU','    FVMBAG',
     .     '   AIRBAG1','   FVMBAG1','    COMMU1',
     .     '    LFLUID','   FVMBAG2'/
C---  +----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----A----+----B
      K   = 0
      MFI = 0
      NVB = 0
!     
      IBID = 0

C-------------------------------------
C     Allocate KMESH (for type 8 airbags)
C-------------------------------------
      IF(.NOT.(ALLOCATED(KMESH))) ALLOCATE(KMESH(NVOLU))
      TETRAMESHER_USED = .FALSE.
      NB_TOTAL_NODE = NUMNOD
      IF(.NOT.(ALLOCATED(NODE_COORD))) ALLOCATE(NODE_COORD(3, NUMNOD))
      DO I = 1, NUMNOD
         NODE_COORD(1, I) = X(1, I)
         NODE_COORD(2, I) = X(2, I)
         NODE_COORD(3, I) = X(3, I)
      ENDDO
      NB_TOTAL_SOLIDS = NUMELS
      IF(.NOT.(ALLOCATED(IXS_TEMP))) ALLOCATE(IXS_TEMP(NIXS * NUMELS))
      DO I = 1, NUMELS
         DO II = 1, NIXS
            IXS_TEMP(NIXS * (I - 1) + II) = IXS(II, I)
         ENDDO
      ENDDO
      IF(.NOT.(ALLOCATED(ITAB_TMP))) ALLOCATE(ITAB_TMP(NUMNOD), ITABM1_TMP(2 * NUMNOD))
      DO I = 1, NUMNOD
         ITAB_TMP(I) = ITAB(I)
      ENDDO
!!!   Completion of ITAB
      MAX_ID = MAXVAL(ITAB(1:NUMNOD))
      
      DO I = NUMNOD + 1, NB_TOTAL_NODE
         MAX_ID = MAX_ID + 1
         ITAB_TMP(I) = MAX_ID    
      ENDDO
      
      CALL CONSTIT(ITAB_TMP,ITABM1_TMP,NB_TOTAL_NODE)
      
      IADALE=1
      RADALE=1

      TETRAMESHER_USED = .FALSE.
      DO N = 1, NVOLU
         ITYPE = T_MONVOL(N)%TYPE
         IF (ITYPE == 11) THEN
C     FVMABG2 are in fact FVMBAG1 with simplified input
            T_MONVOL(N)%TYPE = 8
            T_MONVOL(N)%IVOLU(2) = T_MONVOL(N)%TYPE
         ENDIF
         KMESH(N) = 0
         IF (ITYPE == 6) THEN
            KMESH(N) = 1
         ENDIF
         IF (ITYPE == 8 .OR. ITYPE == 11) THEN
            KMESH(N) = T_MONVOL(N)%IVOLU(30)
         ENDIF
         IF (KMESH(N) >= 2) TETRAMESHER_USED = .TRUE.
      ENDDO
      MGM_USED = .FALSE.
      DO N = 1, NVOLU
         IF (KMESH(N) == 2 .OR. KMESH(N) == 4) THEN
            MGM_USED = .TRUE.
         ENDIF
      ENDDO
C------------------------------------------
C     If meshgems is to be used, then
C     replace bufalei by bufalei_mgm
C------------------------------------------
      IF (MGM_USED) THEN
C     MESHGEMS TETRAMESHER
         RET_VAL=0
         CALL LOAD_MESHGEMS_LIBRARY(RET_VAL)
      ENDIF

      DO N=1,NVOLU
         ITYPE = T_MONVOL(N)%TYPE
         ID = T_MONVOL(N)%ID
         ILVOUT = T_MONVOL(N)%IVOLU(44)
         TITR = T_MONVOL(N)%TITLE
         NN =IGRSURF(T_MONVOL(N)%EXT_SURFID)%NSEG
         IF (ITYPE == 6.OR.ITYPE == 8) THEN
C------------------------------------------
C     Elements de l'airbag et elements internes
C------------------------------------------
            T_MONVOL(N)%IVOLU(31)=IADALE-1
            T_MONVOL(N)%IVOLU(34)=RADALE-1
C     Tag les noeuds des briques additionnelles
            ALLOCATE(TAGNODBR(NUMNOD))
            TAGNODBR(1:NUMNOD)=0
            IBRIC=T_MONVOL(N)%IVOLU(61)
            IF (IBRIC/=0) THEN
               NBRIC=IGRBRIC(IBRIC)%NENTITY
               T_MONVOL(N)%NBRIC = NBRIC
               ALLOCATE(T_MONVOL(N)%TBRIC(2, NBRIC))
               ALLOCATE(T_MONVOL(N)%TFAC(12, NBRIC))
               CALL FVBRIC0(
     .              IBID      ,IXS                ,1      ,NBRIC  ,
     .              T_MONVOL(N)%ID,T_MONVOL(N)%IVOLU(44)        ,TITR   ,TAGNODBR,
     .              T_MONVOL(N)%TBRIC  ,T_MONVOL(N)%TFAC,NUMNOD ,IGRBRIC,IBRIC,ITYPE)
            ENDIF
C     
            ALLOCATE(TAGE(NN),ITABINV(NUMNOD),TAGVENT(NUMNOD))
C     
            
            CALL ALENO(NN  ,IGRSURF(T_MONVOL(N)%EXT_SURFID)%NODES,  NNS,ITABINV,
     .           TAGE, TAGVENT,  TAGNODBR, T_MONVOL(N))
            
            DEALLOCATE(TAGNODBR)
C     
            NNI=0
            
            IF (T_MONVOL(N)%INT_SURFID /= 0) THEN
               ALLOCATE(ITABINV1(NUMNOD))
               NP = IGRSURF(T_MONVOL(N)%INT_SURFID)%NSEG
               CALL FVNODI(NP ,IGRSURF(T_MONVOL(N)%INT_SURFID)%NODES, NNI,ITABINV1 ,
     .              T_MONVOL(N))
               IF (KMESH(N) >= 12) THEN
!     Renumber nodes of internal surface.
!     Needed for HM quad split algorithm
                  ALLOCATE(ITABINV2(NUMNOD), INDEX(NUMNOD))
                  ITABINV2(1:NUMNOD) = 0
                  INDEX(1:NUMNOD) = 0
                  DO II = 1, NNI
                     FOUND = .FALSE.
                     DO JJ = 1, NNS
                        IF (T_MONVOL(N)%NODES(JJ) == T_MONVOL(N)%NODES(II + NNS)) THEN
                           ITABINV2(T_MONVOL(N)%NODES(II + NNS)) = ITABINV(T_MONVOL(N)%NODES(JJ))
                           FOUND = .TRUE.
                           EXIT
                        ENDIF
                     ENDDO
                     IF (.NOT. FOUND) THEN
                        ITABINV2(T_MONVOL(N)%NODES(II + NNS)) = NNS + II
                     ENDIF
                     INDEX(T_MONVOL(N)%NODES(II + NNS)) = NNS + II
                  ENDDO
               ENDIF
            ENDIF
!     Number of nodes on external surface
            T_MONVOL(N)%IVOLU(32)=NNS
!     Number of nodes on internel surface
            T_MONVOL(N)%IVOLU(68)=NNI

            IADALE2=IADALE+NNS+NNI
            NNO=3


!     -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-    !
!     *****************************************************    !
!     ** Triangulation of external and internal surfaces **    !
!     *****************************************************    !
!     -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-    !
            NTG = 0
            NTGI = 0
            SIZE1 = 0
            SIZE2 = 0
            IF (T_MONVOL(N)%EXT_SURFID > 0) THEN
               SIZE1 = IGRSURF(T_MONVOL(N)%EXT_SURFID)%NSEG
            ENDIF
            IF (T_MONVOL(N)%INT_SURFID > 0) THEN
               SIZE2 = IGRSURF(T_MONVOL(N)%INT_SURFID)%NSEG
            ENDIF
!     Keep track of original elements ids
            ALLOCATE(FVBAG_ELEMID(2 * (SIZE1 + SIZE2 + T_MONVOL(N)%NB_FILL_TRI)))

            IF (KMESH(N) >= 12) THEN
               CALL MONVOL_TRIANGULATE_SURFACE(T_MONVOL(N), IGRSURF, ITABINV, ITABINV2, TAGE, 
     .              X, KMESH(N), NNS, NNI, NTG, NTGI, SIZE1, SIZE2, FVBAG_ELEMID)
!     Get back to initial numbering for nodes of the internal surface once the 
!     quad have been split
               DO II = NTG + 1, NTG + NTGI
                  T_MONVOL(N)%ELEM(1:3, II) = INDEX(T_MONVOL(N)%NODES(T_MONVOL(N)%ELEM(1:3, II)))
               ENDDO
            ELSE
               CALL MONVOL_TRIANGULATE_SURFACE(T_MONVOL(N), IGRSURF, ITABINV, ITABINV1, TAGE, 
     .              X, KMESH(N), NNS, NNI, NTG, NTGI, SIZE1, SIZE2, FVBAG_ELEMID)
            ENDIF

            IF (ALLOCATED(ITABINV1)) DEALLOCATE(ITABINV1)
            IF (ALLOCATED(ITABINV)) DEALLOCATE(ITABINV)
            IF (ALLOCATED(ITABINV2)) DEALLOCATE(ITABINV2)
            IF (ALLOCATED(INDEX)) DEALLOCATE(INDEX)
            IF (ALLOCATED(TAGE)) DEALLOCATE(TAGE)

            T_MONVOL(N)%IVOLU(33)=NTG
            T_MONVOL(N)%IVOLU(69)=NTGI
            IADALE3=IADALE2+NNO*NTG

            IF (NTGI > 0) THEN
               ALLOCATE(T_MONVOL(N)%POROSITY(NTGI))
               ALLOCATE(T_MONVOL(N)%THSURF_TAG(NSURF, NTGI + 1))
            ENDIF


            IF (T_MONVOL(N)%INT_SURFID /= 0) THEN
               IF (.NOT. TETRAMESHER_USED) THEN
                  CALL FVBRIC01(T_MONVOL(N)%NODES, T_MONVOL(N)%ELEM(1, NTG + 1), NTGI, NUMNOD, NBRIC, IXS,
     .                 T_MONVOL(N)%TBRIC, T_MONVOL(N)%TFAC)
               ENDIF
            ENDIF
            ALLOCATE(T_MONVOL(N)%ELAREA(NTG + NTGI))
            IADALE3=IADALE3+NNO*NTGI
  
            IF (KMESH(N) >= 2) THEN
!     Automatic tetrameshing
               SELECT CASE (KMESH(N))
               CASE (2, 4)
!     Meshgems (2), Meshgems + writing of mesh as an include file (4)
                  CALL MESHGEMS_TETRA(T_MONVOL(N), NIXS, N, KMESH(N),
     .                 NB_EXTRA_VERTICES, NB_TETRA, NB_VERTICES)
               CASE (12, 14) 
!     Hypermesh (12), Hypermesh + writing of mesh as an include file (14)
                  CALL HYPERMESH_TETRA(T_MONVOL(N), NIXS, N, KMESH(N),
     .                 NB_EXTRA_VERTICES, NB_TETRA, NB_VERTICES)
               CASE DEFAULT
                  
               END SELECT
               WRITE(IOUT,1703) ID,NB_TETRA, NB_EXTRA_VERTICES
               IF (ALLOCATED(ITAB_SAVE)) DEALLOCATE(ITAB_SAVE)
               ALLOCATE(ITAB_SAVE(NB_TOTAL_NODE_OLD))
               ITAB_SAVE(1:NB_TOTAL_NODE_OLD) = ITAB_TMP(1:NB_TOTAL_NODE_OLD)

               DEALLOCATE(ITAB_TMP, ITABM1_TMP)
!!!   First, determine max user ID (external ID)
               ALLOCATE(ITAB_TMP(NB_TOTAL_NODE), ITABM1_TMP(2 * NB_TOTAL_NODE))
               DO I = 1, NB_TOTAL_NODE_OLD
                  ITAB_TMP(I) = ITAB_SAVE(I)
               ENDDO
!!!   Completion of ITAB
               MAX_ID = MAXVAL(ITAB_TMP(1:NB_TOTAL_NODE_OLD))

               DO I = NB_TOTAL_NODE_OLD + 1, NB_TOTAL_NODE
                  MAX_ID = MAX_ID + 1
                  ITAB_TMP(I) = MAX_ID    
               ENDDO

               CALL CONSTIT(ITAB_TMP,ITABM1_TMP,NB_TOTAL_NODE)
               
!!!   Output .inc file
               IF (KMESH(N) == 4 .OR. KMESH(N) == 14) THEN
                  CALL WRITEMESH(N)
               ENDIF
               
               T_MONVOL(N)%NBRIC = NB_TETRA
               ALLOCATE(T_MONVOL(N)%TBRIC(2, NB_TETRA))
               ALLOCATE(T_MONVOL(N)%TFAC(12, NB_TETRA))

               IF (ALLOCATED(IBUFSSG_TEMP)) DEALLOCATE(IBUFSSG_TEMP)
               ALLOCATE(IBUFSSG_TEMP(NB_TETRA))              
               DO I = 1, NB_TETRA
                  IBUFSSG_TEMP(I) = I + NB_TOTAL_SOLIDS_OLD
               ENDDO

               IF (ALLOCATED(TAGNODBR)) DEALLOCATE(TAGNODBR)
               ALLOCATE(TAGNODBR(NB_TOTAL_NODE))
               TAGNODBR(1:NB_TOTAL_NODE)=0
               
               CALL FVBRIC0(
     .              IBUFSSG_TEMP,IXS_TEMP              ,2    ,NB_TETRA,
     .              T_MONVOL(N)%ID  ,T_MONVOL(N)%IVOLU(44)           ,TITR ,TAGNODBR,
     .              T_MONVOL(N)%TBRIC    ,T_MONVOL(N)%TFAC,NB_TOTAL_NODE ,IBID,IBRIC,ITYPE)
               IF (T_MONVOL(N)%INT_SURFID > 0) THEN
                  CALL FVBRIC01(T_MONVOL(N)%NODES, T_MONVOL(N)%ELEM(1, NTG + 1), NTGI, NB_TOTAL_NODE, 
     .                 NB_TETRA, IXS_TEMP, T_MONVOL(N)%TBRIC, T_MONVOL(N)%TFAC)
               ENDIF
               
               DEALLOCATE(TAGNODBR)
            ENDIF

C--------------------------------------
C     Elements d'injecteur et de vent holes
C--------------------------------------
            NJET=T_MONVOL(N)%IVOLU(8)
            ALLOCATE(T_MONVOL(N)%ITAGEL(T_MONVOL(N)%NTG +T_MONVOL(N)%NTGI))
            T_MONVOL(N)%ITAGEL(:) = 0
            ALLOCATE(T_MONVOL(N)%ELTG(T_MONVOL(N)%NTG +T_MONVOL(N)%NTGI))
            T_MONVOL(N)%ELTG(:) = 0
            ALLOCATE(T_MONVOL(N)%MATTG(T_MONVOL(N)%NTG +T_MONVOL(N)%NTGI))
            T_MONVOL(N)%MATTG(:) = 0

            CALL FVINJECT(
     .           T_MONVOL(N)%NODES , T_MONVOL(N)%ELEM, T_MONVOL(N)%IBAGJET,
     .           NJET,            IGRSURF,
     .           T_MONVOL(N)%ITAGEL,NNS+NNI,             NTG,
     .           T_MONVOL(N)%ID      ,TAGVENT,            TITR,
     .           ITAB )
            
            NVENT=T_MONVOL(N)%IVOLU(11)
            NTGT=NTG+NTGI
            IADALE4=IADALE3+NTGT
            IADALE5=IADALE4+NTGT
            IF (TETRAMESHER_USED) THEN
               CALL FVELSURF(
     .              T_MONVOL(N)%NODES,  T_MONVOL(N)%ELEM, FVBAG_ELEMID(1:NTGT), IXC, IXTG, NTGT,
     .              T_MONVOL(N)%ELTG, T_MONVOL(N)%MATTG, NB_TOTAL_NODE, .TRUE.)
               CALL FVVENTHOLE(
     .              T_MONVOL(N)%NODES ,T_MONVOL(N)%ELEM,T_MONVOL(N)%IBAGHOL,
     .              NVENT,           IGRSURF,
     .              T_MONVOL(N)%ITAGEL,NNS+NNI,             NTG,
     .              T_MONVOL(N)%ID      ,TAGVENT,            TITR,
     .              T_MONVOL(N)%ELTG, NB_TOTAL_NODE)
            ELSE
               CALL FVELSURF(
     .              T_MONVOL(N)%NODES,  T_MONVOL(N)%ELEM, FVBAG_ELEMID(1:NTGT), IXC, IXTG, NTGT,
     .              T_MONVOL(N)%ELTG, T_MONVOL(N)%MATTG, NUMNOD, .TRUE.)
               CALL FVVENTHOLE(
     .              T_MONVOL(N)%NODES ,T_MONVOL(N)%ELEM,T_MONVOL(N)%IBAGHOL,
     .              NVENT,           IGRSURF,
     .              T_MONVOL(N)%ITAGEL,NNS+NNI,             NTG,
     .              T_MONVOL(N)%ID      ,TAGVENT,            TITR,
     .              T_MONVOL(N)%ELTG, NUMNOD)
            ENDIF
            DEALLOCATE(TAGVENT)
            DEALLOCATE(FVBAG_ELEMID)
        
C     
            IF (TETRAMESHER_USED) THEN
               CALL FVELPRINT(
     .              IXC,  IXTG, NTGT, T_MONVOL(N)%ITAGEL, T_MONVOL(N)%ELTG,
     .              T_MONVOL(N)%IBAGHOL, ILVOUT, 0 )
            ELSE
               CALL FVELPRINT(
     .              IXC,  IXTG, NTGT, T_MONVOL(N)%ITAGEL, T_MONVOL(N)%ELTG,
     .              T_MONVOL(N)%IBAGHOL, ILVOUT, 0 )
            ENDIF
C----------------------------------
C     Elements volumiques additionnels
C----------------------------------
            IBRIC=T_MONVOL(N)%IVOLU(61)
            IF (IBRIC/=0 .OR. (IBRIC==0 .AND. KMESH(N)>=2)) THEN

               IF (KMESH(N) >= 2) THEN
                  NBRIC = NB_TETRA
                  IADALE5=IADALE4+2*NTGT
                  IADALE6=IADALE5+2*NBRIC
                  IADALE7=IADALE6+12*NBRIC
                  IADALE8=IADALE7+2*NTGT
                  T_MONVOL(N)%IVOLU(19)=IADALE5
                  T_MONVOL(N)%IVOLU(20)=IADALE8

!!!   Now we're done et ready to go on filling bufalei
               ELSE
                  NBRIC=IGRBRIC(IBRIC)%NENTITY
                  IADALE5=IADALE4+2*NTGT
                  IADALE6=IADALE5+2*NBRIC
                  IADALE7=IADALE6+12*NBRIC
                  IADALE8=IADALE7+2*NTGT
                  T_MONVOL(N)%IVOLU(19)=IADALE5
                  T_MONVOL(N)%IVOLU(20)=IADALE8
                  
               ENDIF
               
               ALLOCATE(T_MONVOL(N)%TAGELS(T_MONVOL(N)%NTG + 2 * T_MONVOL(N)%NTGI))
               T_MONVOL(N)%TAGELS(:) = 0
               IF (TETRAMESHER_USED) THEN
                  CALL FVBRIC1(T_MONVOL(N),
     .                 T_MONVOL(N)%NODES, T_MONVOL(N)%ELEM, IXS_TEMP,     IBRIC,
     .                 T_MONVOL(N)%TBRIC,NTG,              NTGA,    NBRIC,
     .                 T_MONVOL(N)%TFAC,T_MONVOL(N)%TAGELS, T_MONVOL(N)%ID, NTGI,
     .                 NNA,              T_MONVOL(N)%IVOLU(44),
     .                 T_MONVOL(N)%ELTG,NODE_COORD,                TITR, 
     .                 NB_TOTAL_NODE)
               ELSE
                  CALL FVBRIC1(T_MONVOL(N),
     .                 T_MONVOL(N)%NODES, T_MONVOL(N)%ELEM, IXS,     IBRIC,
     .                 T_MONVOL(N)%TBRIC,NTG,              NTGA,    NBRIC,
     .                 T_MONVOL(N)%TFAC,T_MONVOL(N)%TAGELS, T_MONVOL(N)%ID, NTGI,
     .                 NNA,              T_MONVOL(N)%IVOLU(44),
     .                 T_MONVOL(N)%ELTG,X,                TITR, NUMNOD)
               ENDIF
C     
               IADALE9 =IADALE8+NNA
               IADALE10=IADALE9+3*NTGA
               IADALE11=IADALE10+NTGA
               IF (TETRAMESHER_USED) THEN
                  CALL FVBRIC2(
     .                 T_MONVOL(N)%NODES,   T_MONVOL(N)%ELEM, IXS_TEMP,  NTG+NTGI, NBRIC,
     .                 T_MONVOL(N)%TBRIC,  T_MONVOL(N)%TFAC, T_MONVOL(N)%TAGELS, NTGA,
     .                 T_MONVOL(N)%IBUFA,  NNA,              T_MONVOL(N)%ELEMA,
     .                 T_MONVOL(N)%TAGELA, T_MONVOL(N)%BRNA, 
     .                 NB_TOTAL_NODE)
               ELSE
                  CALL FVBRIC2(
     .                 T_MONVOL(N)%NODES,   T_MONVOL(N)%ELEM, IXS,  NTG+NTGI, NBRIC,
     .                 T_MONVOL(N)%TBRIC,  T_MONVOL(N)%TFAC, T_MONVOL(N)%TAGELS, NTGA,
     .                 T_MONVOL(N)%IBUFA,  NNA,              T_MONVOL(N)%ELEMA,
     .                 T_MONVOL(N)%TAGELA, T_MONVOL(N)%BRNA, NUMNOD)
               ENDIF
C     
               IADALE12=IADALE11+8*NBRIC
               IF (TETRAMESHER_USED) THEN
                  CALL FVBRIC3(
     .                 T_MONVOL(N)%NODES,  T_MONVOL(N)%IBUFA,   NNS+NNI,    NBRIC, NNA,
     .                 T_MONVOL(N)%TBRIC, T_MONVOL(N)%BRNA,  T_MONVOL(N)%NCONA, ITAB_TMP,
     .                 ILVOUT, NB_TOTAL_NODE,T_MONVOL(N)%IVOLU(38),T_MONVOL(N)%RVOLU(67),KMESH(N), V )
                  
               ELSE
                  CALL FVBRIC3(
     .                 T_MONVOL(N)%NODES,  T_MONVOL(N)%IBUFA,  NNS+NNI,    NBRIC, NNA,
     .                 T_MONVOL(N)%TBRIC, T_MONVOL(N)%BRNA, T_MONVOL(N)%NCONA, ITAB,
     .                 ILVOUT, NUMNOD,   T_MONVOL(N)%IVOLU(38),T_MONVOL(N)%RVOLU(67),KMESH(N), V )
               ENDIF            
            ELSE
               NBRIC=0
               NTGA=NTG+NTGI
               NNA=0
               IADALE5=IADALE4+2*NTGA
               IADALE6=IADALE5
               IADALE7=IADALE6
               IADALE8=IADALE7+2*NTGA
               IADALE9=IADALE8
               IADALE10=IADALE9+3*NTGA
               IADALE11=IADALE10+NTGA
               IADALE12=IADALE11
               T_MONVOL(N)%IVOLU(19)=IADALE5
               T_MONVOL(N)%IVOLU(20)=IADALE8
               
               T_MONVOL(N)%NTGA = NTGA
               ALLOCATE(T_MONVOL(N)%TAGELS(NTG + 2 * NTGI))
               DO I = 1, NTG + 2 * NTGI
                  T_MONVOL(N)%TAGELS(I) = 0
               ENDDO
               ALLOCATE(T_MONVOL(N)%ELEMA(3, NTGA))
               DO I = 1, NTGA
                  T_MONVOL(N)%ELEMA(1, I) = T_MONVOL(N)%ELEM(1, I)
                  T_MONVOL(N)%ELEMA(2, I) = T_MONVOL(N)%ELEM(2, I)
                  T_MONVOL(N)%ELEMA(3, I) = T_MONVOL(N)%ELEM(3, I)
               ENDDO
               ALLOCATE(T_MONVOL(N)%TAGELA(NTGA))
               DO I = 1, NTGA
                  T_MONVOL(N)%TAGELA(I) = I
               ENDDO
            ENDIF
            T_MONVOL(N)%IVOLU(62) = NBRIC
            T_MONVOL(N)%IVOLU(63) = NTGA
            T_MONVOL(N)%IVOLU(64) = NNA
            T_MONVOL(N)%NNA = NNA
            IF (NNA > 0) THEN
               ALLOCATE(T_MONVOL(N)%VELOCITY(3, NNA), T_MONVOL(N)%NODE_COORD(3, NNA))
            ENDIF
C     
            IADALE13=IADALE12+16*NNA
            T_MONVOL(N)%IVOLU(18)=IADALE13-1
!     Copy adresses to structure
            T_MONVOL(N)%IADALE = IADALE
            T_MONVOL(N)%IADALE2 = IADALE2
            T_MONVOL(N)%IADALE3 = IADALE3
            T_MONVOL(N)%IADALE4 = IADALE4
            T_MONVOL(N)%IADALE5 = IADALE5
            T_MONVOL(N)%IADALE6 = IADALE6
            T_MONVOL(N)%IADALE7 = IADALE7
            T_MONVOL(N)%IADALE8 = IADALE8
            T_MONVOL(N)%IADALE9 = IADALE9
            T_MONVOL(N)%IADALE10 = IADALE10
            T_MONVOL(N)%IADALE11 = IADALE11
            T_MONVOL(N)%IADALE12 = IADALE12
            T_MONVOL(N)%IADALE12 = IADALE13
            IADALE=IADALE13+NSURF*(NTGI+1)
            RADALE=RADALE+7*(NNS+NNI)+4*NTG+13*NNA+5*NTGI
            MFI=MFI+      7*(NNS+NNI)+4*NTG+13*NNA+5*NTGI
C     
            T_MONVOL(N)%IVOLU(56)=0
            LIBAGALE=IADALE-1
         ENDIF     
      ENDDO
C     
      LIBAGALE=IADALE-1
      LRBAGALE=RADALE-1

!     *********************    !
!     Communicating airbags    !
!     *********************    !
!     Change to local monvol ID
      LCA = 0
      NCOMMU = 0
      COMM_TAG(1:NVOLU) = 0
      DO NN = 1, NVOLU
         ITYPE = T_MONVOL(NN)%TYPE
         IF (ITYPE == 5 .OR. ITYPE == 9) THEN
            NCOMMU = NCOMMU + 1
            NCA = T_MONVOL(NN)%NCA
            DO II = 1, NCA
               ID = T_MONVOL_METADATA%ICBAG(1, II + LCA)
               FOUND = .FALSE.
               DO JJ = 1, NVOLU
                  IF (ID == T_MONVOL(JJ)%ID) THEN
                     T_MONVOL_METADATA%ICBAG(1, II + LCA) = JJ
                     FOUND = .TRUE.
                     EXIT
                  ENDIF
               ENDDO
               IF (.NOT. FOUND) THEN
                  CALL ANCMSG(MSGID = 15, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .              I2 = ID, I1 = T_MONVOL(NN)%ID, C1 = T_MONVOL(NN)%TITLE)
               ELSE
!     Check compatibility : type 5 with type 5 only, and type 9 with type 9 only
                  IF (ITYPE == 5 .AND. T_MONVOL(JJ)%TYPE /= 5 .OR.
     .                 ITYPE == 9 .AND. T_MONVOL(JJ)%TYPE /= 9) THEN
                     CALL ANCMSG(MSGID = 16, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .                    I1 = T_MONVOL(NN)%ID, C1 = T_MONVOL(NN)%TITLE,
     .                    I2 = T_MONVOL(NN)%TYPE, C2 = T_MONVOL(JJ)%TITLE)
                  ENDIF
               ENDIF
            ENDDO
            LCA = LCA + NCA
         ENDIF
      ENDDO
      
!     Tag communicting airbags
      COMM_TAG(1:NVOLU) = 0
      LCA = 0
      DO NN = 1, NVOLU
         ITYPE = T_MONVOL(NN)%TYPE
         IF (ITYPE == 5 .OR. ITYPE == 9) THEN
            NCA = T_MONVOL(NN)%NCA
            IF (COMM_TAG(NN) == 0) THEN
               COMM_TAG(NN) = NN
               DO II = 1, NCA
                  ID1 = T_MONVOL_METADATA%ICBAG(1, II + LCA)
                  IF (COMM_TAG(ID1) /= 0) THEN
                     COMM_TAG(NN) = COMM_TAG(ID1)
                     DO JJ = 1, NCA
                        ID2 = T_MONVOL_METADATA%ICBAG(1, JJ + LCA)
                        COMM_TAG(ID2) = COMM_TAG(ID1)
                     ENDDO
                  ENDIF
               ENDDO
            ENDIF
            LCA = LCA + NCA
         ENDIF
      ENDDO
!     ***************    !
!     Venthole adress    !
!     ***************    !
      NVENTTOT = 0
      DO NN = 1, NVOLU
         T_MONVOL(NN)%IVOLU(16) = NVENTTOT
         NVENT = T_MONVOL(NN)%NVENT
         NVENTTOT = NVENTTOT + NVENT
      ENDDO
      LIBHOL = 0
      LRBHOL = 0
      DO NN = 1, NVOLU
         ITYPE = T_MONVOL(NN)%TYPE
         IF (ITYPE >= 3.AND.(ITYPE <= 9 .OR. ITYPE == 11))THEN
            T_MONVOL(NN)%IVOLU(12) = LIBHOL
            T_MONVOL(NN)%IVOLU(13) = LRBHOL
            NVENT = T_MONVOL(NN)%NVENT
            LIBHOL = LIBHOL + NVENT * NIBHOL          
            LRBHOL = LRBHOL + NVENT * NRBHOL
         ENDIF
      ENDDO
C----------------------------------------------------------------------------
C     airbags communiquants, verifie les caracteristiques du gaz a l'initial.
C----------------------------------------------------------------------------
      DO N=1,NVOLU
         ITYPE=T_MONVOL(N)%IVOLU(2)
         IF(ITYPE==5.OR.ITYPE==9.AND.N/=COMM_TAG(N))THEN
            GAMAI=T_MONVOL(COMM_TAG(N))%RVOLU(1)
            CPAI =T_MONVOL(COMM_TAG(N))%RVOLU(7)
            CPBI =T_MONVOL(COMM_TAG(N))%RVOLU(8)
            CPCI =T_MONVOL(COMM_TAG(N))%RVOLU(9)
            CPDI =T_MONVOL(COMM_TAG(N))%RVOLU(56)
            CPEI =T_MONVOL(COMM_TAG(N))%RVOLU(57)
            CPFI =T_MONVOL(COMM_TAG(N))%RVOLU(58)
            IF(T_MONVOL(N)%RVOLU(1) /= GAMAI.OR.
     .           T_MONVOL(N)%RVOLU(7)/=CPAI.OR.
     .           T_MONVOL(N)%RVOLU(8)/=CPBI.OR.
     .           T_MONVOL(N)%RVOLU(9)/=CPCI.OR.
     .           T_MONVOL(N)%RVOLU(56)/=CPDI.OR.
     .           T_MONVOL(N)%RVOLU(57)/=CPEI.OR.
     .           T_MONVOL(N)%RVOLU(58)/=CPFI)THEN
               ID = T_MONVOL(N)%ID
               TITR = T_MONVOL(N)%TITLE
               ID1 = T_MONVOL(COMM_TAG(N))%ID
               TITR1 = T_MONVOL(COMM_TAG(N))%TITLE
               CALL ANCMSG(MSGID=474,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .              I1=ID,
     .              C1=TITR,
     .              I2=ID1,
     .              C2=TITR1)
            ENDIF
         ENDIF
      ENDDO

      DO NN = 1, NVOLU
         NJET_NEW(NN) = T_MONVOL(NN)%NJET
      ENDDO
      DO NN = 1, NVOLU
         ITYPE = T_MONVOL(NN)%TYPE
         IF (ITYPE == 5 .OR. ITYPE == 9) THEN
            IF (COMM_TAG(NN) /= NN) THEN
               NJET_NEW(COMM_TAG(NN)) = NJET_NEW(COMM_TAG(NN)) + NJET_NEW(NN)
            ENDIF
         ENDIF
      ENDDO
      DO NN = 1, NVOLU
         ITYPE = T_MONVOL(NN)%TYPE
         IF (ITYPE == 5 .OR. ITYPE == 9) THEN
            IF (COMM_TAG(NN) /= NN) THEN
               NJET_NEW(NN) = NJET_NEW(COMM_TAG(NN))
            ENDIF
         ENDIF
      ENDDO

      DO NN = 1, NVOLU
         IF (COMM_TAG(NN) == NN) THEN
!     Root communicating airbag
            NJET = T_MONVOL(NN)%NJET
            IF (NJET > 0) THEN
               ALLOCATE(IBAGJET_SAVE(NIBJET, NJET))
               ALLOCATE(RBAGJET_SAVE(NRBJET, NJET))
               DO II = 1, NJET
                  DO JJ = 1, NIBJET
                     IBAGJET_SAVE(JJ, II) = T_MONVOL(NN)%IBAGJET(JJ, II)
                  ENDDO
                  DO JJ = 1, NRBJET
                     RBAGJET_SAVE(JJ, II) = T_MONVOL(NN)%RBAGJET(JJ, II)
                  ENDDO
               ENDDO
            ENDIF
            IF (ALLOCATED(T_MONVOL(NN)%RBAGJET)) DEALLOCATE(T_MONVOL(NN)%RBAGJET)
            IF (ALLOCATED(T_MONVOL(NN)%IBAGJET)) DEALLOCATE(T_MONVOL(NN)%IBAGJET)
            IF (NJET_NEW(NN) > 0) THEN
               ALLOCATE(T_MONVOL(NN)%RBAGJET(NRBJET, NJET_NEW(NN)), 
     .              T_MONVOL(NN)%IBAGJET(NIBJET, NJET_NEW(NN)))
               T_MONVOL(NN)%RBAGJET(1:NRBJET, 1:NJET_NEW(NN)) = ZERO
               T_MONVOL(NN)%IBAGJET(1:NIBJET, 1:NJET_NEW(NN)) = 0
               DO II = 1, NJET
                   DO JJ = 1, NIBJET
                      T_MONVOL(NN)%IBAGJET(JJ, II) = IBAGJET_SAVE(JJ, II)
                  ENDDO
                  DO JJ = 1, NRBJET
                     T_MONVOL(NN)%RBAGJET(JJ, II) = RBAGJET_SAVE(JJ, II) 
                  ENDDO
               ENDDO
            ENDIF
            IF (ALLOCATED(IBAGJET_SAVE)) DEALLOCATE(IBAGJET_SAVE)
            IF (ALLOCATED(RBAGJET_SAVE)) DEALLOCATE(RBAGJET_SAVE)
         ENDIF
      ENDDO
      DO NN = 1, NVOLU
         IF (COMM_TAG(NN) /= NN) THEN
!     Non root communicating airbag
            NJET = T_MONVOL(NN)%NJET
            IF (NJET > 0) THEN
               ALLOCATE(IBAGJET_SAVE(NIBJET, NJET))
               ALLOCATE(RBAGJET_SAVE(NRBJET, NJET))
               DO II = 1, NJET
                  DO JJ = 1, NIBJET
                     IBAGJET_SAVE(JJ, II) = T_MONVOL(NN)%IBAGJET(JJ, II)
                  ENDDO
                  DO JJ = 1, NRBJET
                     RBAGJET_SAVE(JJ, II) = T_MONVOL(NN)%RBAGJET(JJ, II)
                  ENDDO
               ENDDO
            ENDIF
            IF (ALLOCATED(T_MONVOL(NN)%RBAGJET)) DEALLOCATE(T_MONVOL(NN)%RBAGJET)
            IF (ALLOCATED(T_MONVOL(NN)%IBAGJET)) DEALLOCATE(T_MONVOL(NN)%IBAGJET)
            IF (NJET_NEW(NN) > 0) THEN
               ALLOCATE(T_MONVOL(NN)%RBAGJET(NRBJET, NJET_NEW(NN)), 
     .              T_MONVOL(NN)%IBAGJET(NIBJET, NJET_NEW(NN)))
               T_MONVOL(NN)%RBAGJET(1:NRBJET, 1:NJET_NEW(NN)) = ZERO
               T_MONVOL(NN)%IBAGJET(1:NIBJET, 1:NJET_NEW(NN)) = 0
               DO II = 1, NJET
                   DO JJ = 1, NIBJET
                      T_MONVOL(NN)%IBAGJET(JJ, II) = IBAGJET_SAVE(JJ, II)
                  ENDDO
                  DO JJ = 1, NRBJET
                     T_MONVOL(NN)%RBAGJET(JJ, II) = RBAGJET_SAVE(JJ, II) 
                  ENDDO
               ENDDO
            ENDIF
            IF (ALLOCATED(IBAGJET_SAVE)) DEALLOCATE(IBAGJET_SAVE)
            IF (ALLOCATED(RBAGJET_SAVE)) DEALLOCATE(RBAGJET_SAVE)
         ENDIF
      ENDDO
     

C-------------------------------------
C     airbags communiquants, echange des caracteristiques des gaz.
C-------------------------------------
      DO N=1,NVOLU
         ITYPE=T_MONVOL(N)%TYPE
         IF((ITYPE==5.OR.ITYPE==9).AND.N/=COMM_TAG(N))THEN
            NGROOT =T_MONVOL(COMM_TAG(N))%IVOLU(8)
            NJET   =T_MONVOL(N)%IVOLU(8)
            DO J=1,NJET
C---------------------------------------------------------------------------------
C     informe le volume pere des caracteristiques du gaz injecte dans un fils
C---------------------------------------------------------------------------------
               T_MONVOL(COMM_TAG(N))%RBAGJET(1, NGROOT + J) = T_MONVOL(N)%RBAGJET(1, J)
               T_MONVOL(COMM_TAG(N))%RBAGJET(2, NGROOT + J) = T_MONVOL(N)%RBAGJET(2, J)
               T_MONVOL(COMM_TAG(N))%RBAGJET(3, NGROOT + J) = T_MONVOL(N)%RBAGJET(3, J)
               T_MONVOL(COMM_TAG(N))%RBAGJET(4, NGROOT + J) = T_MONVOL(N)%RBAGJET(4, J)
               T_MONVOL(COMM_TAG(N))%RBAGJET(16, NGROOT + J) = T_MONVOL(N)%RBAGJET(16, J)
               T_MONVOL(COMM_TAG(N))%RBAGJET(17, NGROOT + J) = T_MONVOL(N)%RBAGJET(17, J)
               T_MONVOL(COMM_TAG(N))%RBAGJET(18, NGROOT + J) = T_MONVOL(N)%RBAGJET(18, J)
               T_MONVOL(COMM_TAG(N))%IBAGJET(4, NGROOT + J) = T_MONVOL(N)%IBAGJET(4, J)
               IF(ITYPE==9) THEN
                  T_MONVOL(COMM_TAG(N))%IBAGJET(13, NGROOT + J) = T_MONVOL(N)%IBAGJET(13, J)
               ENDIF
C----------------------------------------------------
C     deplace les infos relatives a l'injection.
C----------------------------------------------------
               IF(J-1/=NGROOT)THEN
                  DO K=1,NIBJET
                     T_MONVOL(N)%IBAGJET(K, NGROOT + 1) = T_MONVOL(N)%IBAGJET(K, J)
                     T_MONVOL(N)%IBAGJET(K, J) = 0
                  ENDDO
                  DO K=1,NRBJET
                     T_MONVOL(N)%RBAGJET(K, NGROOT + 1) = T_MONVOL(N)%RBAGJET(K, J)
                     T_MONVOL(N)%RBAGJET(K, J) = ZERO
                  ENDDO
               ENDIF
               NGROOT = NGROOT + 1
            ENDDO
            T_MONVOL(COMM_TAG(N))%IVOLU(8)=NGROOT
            T_MONVOL(COMM_TAG(N))%NJET = NGROOT
         ENDIF
      ENDDO
C----------------------------------------------------------
C     informe tous les volumes des caracteristiques des gaz.
C----------------------------------------------------------
      DO N=1,NVOLU
         ITYPE=T_MONVOL(N)%TYPE
         IF(ITYPE==5.OR.ITYPE==9.AND.N/=COMM_TAG(N))THEN
            NJET = T_MONVOL(COMM_TAG(N))%NJET
            DO I=1,NJET
               T_MONVOL(N)%IBAGJET(4, I) = T_MONVOL(COMM_TAG(N))%IBAGJET(4, I) ! sensor
               IF(ITYPE==9 .AND. T_MONVOL(N)%IBAGJET(13, I)==0) THEN ! property
                  IF(T_MONVOL(COMM_TAG(N))%IBAGJET(13, I) > 0) THEN
                     T_MONVOL(N)%IBAGJET(13, I) = -T_MONVOL(COMM_TAG(N))%IBAGJET(13, I)
                  ELSE
                     T_MONVOL(N)%IBAGJET(13, I) = T_MONVOL(COMM_TAG(N))%IBAGJET(13, I)
                  ENDIF
               ENDIF
C     
               T_MONVOL(N)%RBAGJET(1, I) = T_MONVOL(COMM_TAG(N))%RBAGJET(1, I)
               T_MONVOL(N)%RBAGJET(2, I) = T_MONVOL(COMM_TAG(N))%RBAGJET(2, I)
               T_MONVOL(N)%RBAGJET(3, I) = T_MONVOL(COMM_TAG(N))%RBAGJET(3, I)
               T_MONVOL(N)%RBAGJET(4, I) = T_MONVOL(COMM_TAG(N))%RBAGJET(4, I)
               T_MONVOL(N)%RBAGJET(16, I) = T_MONVOL(COMM_TAG(N))%RBAGJET(16, I)
               T_MONVOL(N)%RBAGJET(17, I) = T_MONVOL(COMM_TAG(N))%RBAGJET(17, I)
               T_MONVOL(N)%RBAGJET(18, I) = T_MONVOL(COMM_TAG(N))%RBAGJET(18, I)
            ENDDO
            T_MONVOL(N)%IVOLU(8)=T_MONVOL(COMM_TAG(N))%IVOLU(8)
            T_MONVOL(N)%NJET = T_MONVOL(COMM_TAG(N))%NJET
         ENDIF
      ENDDO

!     ***************    !
!     Injector adress    !
!     ***************    !
      LIBJET = 0
      LRBJET = 0
      DO NN = 1, NVOLU
         ITYPE = T_MONVOL(NN)%TYPE
         IF (ITYPE >= 4 .AND. (ITYPE <= 9 .OR. ITYPE == 11)) THEN
            T_MONVOL(NN)%IVOLU(9) = LIBJET
            T_MONVOL(NN)%IVOLU(10) = LRBJET
            LIBJET = LIBJET + T_MONVOL(NN)%NJET * NIBJET            
            LRBJET = LRBJET + T_MONVOL(NN)%NJET * NRBJET
         ENDIF
      ENDDO
C-------------------------------------
      IF(IPRI>=7)THEN
         WRITE(IOUT,3000)
         DO N=1,NVOLU
            ITYPE=T_MONVOL(N)%TYPE
            IF(ITYPE==5.OR.ITYPE==9)THEN
               WRITE(IOUT,1001)T_MONVOL(N)%ID,TRIM(TITR),MONVOLTYP(ITYPE)
C     
               TI   =T_MONVOL(N)%RVOLU(13)
               GAMAI=T_MONVOL(N)%RVOLU(1)
               CPAI =T_MONVOL(N)%RVOLU(7)
               CPBI =T_MONVOL(N)%RVOLU(8)
               CPCI =T_MONVOL(N)%RVOLU(9)
               WRITE(IOUT,1410)GAMAI,CPAI,CPBI,CPCI
C     
               WRITE(IOUT,1420)
               NJET=T_MONVOL(N)%IVOLU(8)
               WRITE(IOUT,1421)NJET
               DO I=1,NJET
                  IMASS=T_MONVOL(N)%IBAGJET(1, I)
                  IF(IMASS/=0)IMASS=NPC(IMASS)
                  IFLU =T_MONVOL(N)%IBAGJET(2, I)
                  FMASS=T_MONVOL(N)%RBAGJET(5, I)
                  ITEMP=T_MONVOL(N)%IBAGJET(3, I)
                  IF(ITEMP/=0)ITEMP=NPC(ITEMP)
                  FTEMP=T_MONVOL(N)%RBAGJET(6, I)
                  ISENS=T_MONVOL(N)%IBAGJET(4, I)
                  IF(ISENS/=0)ISENS=LSENSOR(1,ISENS)
                  WRITE(IOUT,1430)I,
     .                 IMASS,IFLU,FMASS,ITEMP,FTEMP,ISENS
C     
                  RMWG=T_MONVOL(N)%RBAGJET(1, I)
                  CPA =T_MONVOL(N)%RBAGJET(2, I)
                  CPB =T_MONVOL(N)%RBAGJET(3, I)
                  CPC =T_MONVOL(N)%RBAGJET(4, I)
C     
                  CPG   =CPA+CPB*TI+CPC*TI*TI
                  CVG   =CPG-RMWG
                  GAMA  =CPG/CVG
                  WRITE(IOUT,1440)GAMA,CPA,CPB,CPC
C     
                  NJ1=T_MONVOL(N)%IBAGJET(5, I)
                  NJ2=T_MONVOL(N)%IBAGJET(6, I)
                  NJ3=T_MONVOL(N)%IBAGJET(7, I)
                  IPT=T_MONVOL(N)%IBAGJET(8, I)
                  IPA=T_MONVOL(N)%IBAGJET(9, I)
                  IPZ=T_MONVOL(N)%IBAGJET(10, I)
                  IF(NJ1==0)THEN
                     IJET=0
                  ELSE
                     IJET=1
                  ENDIF
                  WRITE(IOUT,1450)IJET
                  IF(IJET==1)THEN
                     IF(IPT/=0)IPT=NPC(IPT)
                     IF(IPA/=0)IPA=NPC(IPA)
                     IF(IPZ/=0)IPZ=NPC(IPZ)
                     WRITE(IOUT,1461)ITAB(NJ1),ITAB(NJ2),ITAB(NJ3),IPT,IPA,IPZ
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
      ENDIF
      WRITE(IOUT,2000)
C-------------------------------------

      NFVMBAGS = 0
      DO N = 1, NVOLU
         ITYPE=T_MONVOL(N)%TYPE
         IF(ITYPE == 6 .OR. ITYPE == 8) THEN
            NFVMBAGS = NFVMBAGS + 1
!     Local numbering of FVMBAGS
!     This numbering does not change when FVMBAGS are switched to UP
            T_MONVOL(N)%IVOLU(45) = NFVMBAGS
         ENDIF
      ENDDO
      DO N = 1, NVOLU
         ITYPE=T_MONVOL(N)%TYPE
         IF(ITYPE == 6 .OR. ITYPE == 8) THEN
!     Total number of FVMBAGS (including FVMBAGs switched to UP)
            T_MONVOL(N)%IVOLU(29) = NFVMBAGS
         ENDIF
      ENDDO

      ! --------------------
      ! allocation of NUMBER_TRI_PER_PROC : number of tri element per processor 
      ! for each N nvolu
      DO N = 1, NVOLU
            ALLOCATE( T_MONVOL(N)%NUMBER_TRI_PER_PROC(NSPMD) )
            T_MONVOL(N)%NUMBER_TRI_PER_PROC(1:NSPMD) = 0
      ENDDO
      ! --------------------
C     
      RETURN
C     
 1001 FORMAT(//5X,'VOLUME NUMBER ',I10,
     .     / 5X,'------------------------',
     .     / 5X,'TITLE: ',A,
     .     / 5X,'TYPE OF MONITORED VOLUME. . . . . . . .=',A10)
 1410 FORMAT(
     .     5X,'CHARACTERISTICS OF INITIAL GAZ          ',
     .     /5X,'------------------------------          ',
     .     /5X,'GAMMA AT INITIAL TEMPERATURE. . . . . .=',1PG20.13,
     .     /5X,'COEFFICIENT CPA . . . . . . . . . . . .=',1PG20.13,
     .     /5X,'COEFFICIENT CPB . . . . . . . . . . . .=',1PG20.13,
     .     /5X,'COEFFICIENT CPC . . . . . . . . . . . .=',1PG20.13/)
 1420 FORMAT(
     .     5X,'INFLATORS                               ',
     .     /5X,'---------                               ')
 1421 FORMAT(
     .     5X,'NUMBER OF INFLATORS . . . . . . . . . .=',I10/)
 1430 FORMAT(
     .     5X,'INFLATOR NUMBER . . . . . . . . . . . .=',I10,
     .     /15X,'TIME FUNCTION FOR INCOMING TOTAL MASS .=',I10,
     .     /15X,'   or MASS FLUX if IFLU=1 . . . . IFLU =',I10,
     .     /15X,'SCALE FACTOR FOR INCOMING TOTAL MASS  .=',1PG20.13,
     .     /15X,'TIME FUNCTION FOR INCOMIMG GAS TEMP . .=',I10,
     .     /15X,'SCALE FACTOR FOR INCOMIMG GAS TEMP  . .=',1PG20.13,
     .     /15X,'SENSOR NUMBER . . . . . . . . . . . . .=',I10)
 1440 FORMAT(
     .     /15X,'GAZ CHARACTERISTICS                     ',
     .     /15X,'-------------------                     ',
     .     /15X,'GAMMA AT INITIAL TEMPERATURE. . . . . .=',1PG20.13,
     .     /15X,'COEFFICIENT CPA . . . . . . . . . . . .=',1PG20.13,
     .     /15X,'COEFFICIENT CPB . . . . . . . . . . . .=',1PG20.13,
     .     /15X,'COEFFICIENT CPC . . . . . . . . . . . .=',1PG20.13)
 1450 FORMAT(
     .     /15X,'JETTING OPTION. . . . . . . . . . . . .=',I10,
     .     /15X,'----------------------------------------')
 1461 FORMAT(
     .     15X,'DIHEDRAL JET. . . . . . . . . . . . . .',
     .     /15X,'NODE NUMBER DEFINING INJECTION CENTER .=',I10,
     .     /15X,'NODE NUMBER DEFINING INJECTION AXIS . .=',I10,
     .     /15X,'NODE NUMBER DEFINING BASE LINE. . . . .=',I10,
     .     /15X,'JETTING PRESSURE TIME CURVE NUMBER. . .=',I10,
     .     /15X,'JETTING PRESSURE THETA CURVE NUMBER . .=',I10,
     .     /15X,'JETTING PRESSURE DIST. CURVE NUMBER . .=',I10,
     .     /15X,'TIME FUNCTION SCALE FACTOR          . .=',1PG20.13,
     .     /15X,'THETA FUNCTION SCALE FACTOR         . .=',1PG20.13,
     .     /15X,'DIST FUNCTION SCALE FACTOR          . .=',1PG20.13)
 1703 FORMAT(5X,'FVMBAG MESHING, VOLUME NUMBER',I10,
     .     /5X,'-----------------                       ',
     .     /15X,'TETRAMESH CREATED AUTOMATICALLY',
     .     /15X,'         NUMBER OF TETRAHEDRA',I10,
     .     /15X,'         NUMBER OF ADDED INTERNAL NODES',I10) 
 2000 FORMAT(/)
 3000 FORMAT(
     .     //,'      MONITORED VOLUMES TYPE COMMU : GAS ORGANIZATION'/
     .     '      -----------------------------------------------')
      END
