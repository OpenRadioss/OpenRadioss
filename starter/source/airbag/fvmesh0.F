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
C ATTENTION : toute modification dans ces routines doit etre reportee
C             dans les routines identiques dans l'engine
Chd|====================================================================
Chd|  FVMESH0                       source/airbag/fvmesh0.F       
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        FVBRIC                        source/airbag/fvbric.F        
Chd|        FVELAREA                      source/airbag/fvelarea.F      
Chd|        FVELINTE                      source/airbag/fvelinte.F      
Chd|        FVELPRINT                     source/airbag/fvelprint.F     
Chd|        FVELSURF                      source/airbag/fvelsurf.F      
Chd|        FVINJECTINT                   source/airbag/fvinjectint.F   
Chd|        FVINJNORMAL                   source/airbag/fvinjnormal.F   
Chd|        FVLENGTH                      source/airbag/fvlength.F      
Chd|        FVMESH1                       source/airbag/fvmesh.F        
Chd|        FVNODBR                       source/airbag/fvmbag1.F       
Chd|        FVTHSURF                      source/airbag/fvthsurf.F      
Chd|        FVVENTHOLEINT                 source/airbag/fvventholeint.F 
Chd|        FVVERIF                       source/airbag/fvmbag1.F       
Chd|        FVVOLU                        source/airbag/fvvolu.F        
Chd|        FVBAG_MOD                     share/modules1/fvbag_mod.F    
Chd|        FVMBAG_MESHCONTROL_MOD        ../common_source/modules/airbag/fvmbag_meshcontrol_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MONVOL_STRUCT_MOD             share/modules1/monvol_struct_mod.F
Chd|        REORDER_MOD                   share/modules1/reorder_mod.F  
Chd|====================================================================
      SUBROUTINE FVMESH0(T_MONVOL, XYZINI, IXS, IXC, IXTG, PM,
     1                   IPM,  ITAB, IGRSURF, XYZREF,
     2                   NOM_OPT, NB_NODE)
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE FVBAG_MOD
      USE FVMBAG_MESHCONTROL_MOD
      USE REORDER_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE MONVOL_STRUCT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(MONVOL_STRUCT_), DIMENSION(NVOLU), INTENT(INOUT) :: T_MONVOL
      INTEGER IXS(NIXS,*), IXC(NIXC,*), IXTG(NIXTG,*)
      INTEGER IPM(NPROPMI,*), ITAB(*)
      INTEGER NOM_OPT(LNOPT1,*), NB_NODE
     
      my_real
     .        XYZINI(3,NB_NODE), PM(NPROPM,*), 
     .        XYZREF(3,NB_NODE)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IFV,
     .        N, ITYP, NNS, NTG, NBRIC, NBX, NBY, NNB, IMESH,
     .        NBA, NTGA, NNA, 
     .        PFLAG, NNI, NTGI, ILVOUT, NNFV, NNT, NSURFI, 
     .        NSEG, IREF, NTRFV,  NPOLH, ID,
     .        NTGT, INODE
      INTEGER I
      INTEGER J, IEL, IBID
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAGVENT,TMP
      CHARACTER*nchartitle,
     .        TITR
      INTEGER, DIMENSION(:), ALLOCATABLE :: MINUS_SIGN_REVERSE
C
      my_real
     .        DIRX,DIRY,DIRZ,DIR2X,DIR2Y,DIR2Z,ORIGX,ORIGY,ORIGZ,
     .        LX,LY,LZ
      my_real
     .        X(3,NB_NODE)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      PFLAG = 0
      IFV=0
      DO N = 1, NVOLU
         ITYP = T_MONVOL(N)%TYPE
         ID = T_MONVOL(N)%ID
         TITR = T_MONVOL(N)%TITLE
         IF (ITYP == 6 .OR. ITYP == 8) THEN
            IF (PFLAG == 0) THEN
              WRITE(IOUT,1000)
              PFLAG = 1
            ENDIF
C
            IREF = T_MONVOL(N)%IVOLU(59)
            IF(IREF==0) THEN
               X=XYZINI
            ELSE
               X=XYZREF
            ENDIF
C
            IFV=IFV+1
            NNS = T_MONVOL(N)%NNS
            NTG = T_MONVOL(N)%NTG
            NNI = T_MONVOL(N)%NNI
            NTGI = T_MONVOL(N)%NTGI
            NBA = T_MONVOL(N)%NBRIC
            NTGA = T_MONVOL(N)%NTGA
            NNA = T_MONVOL(N)%NNA
            NNT = NNS + NNI
            NTGT = NTG + NTGI

            T_MONVOL(N)%KR5 = 1+NRVOLU*NVOLU+LRCBAG+LRBAGJET+LRBAGHOL+T_MONVOL(N)%IVOLU(34) + NNT*6
C----------------------------------------------
C Parametres de decoupage automatique du volume
C----------------------------------------------
            DIRX = T_MONVOL(N)%RVOLU(35)
            DIRY = T_MONVOL(N)%RVOLU(36)
            DIRZ = T_MONVOL(N)%RVOLU(37)
            DIR2X = T_MONVOL(N)%RVOLU(38)
            DIR2Y = T_MONVOL(N)%RVOLU(39)
            DIR2Z = T_MONVOL(N)%RVOLU(40)
            ORIGX = T_MONVOL(N)%RVOLU(41)
            ORIGY = T_MONVOL(N)%RVOLU(42)
            ORIGZ = T_MONVOL(N)%RVOLU(43)
            LX = T_MONVOL(N)%RVOLU(44)
            LY = T_MONVOL(N)%RVOLU(45)
            LZ = T_MONVOL(N)%RVOLU(53)
            FVDATA(IFV)%L_TYPE = 0
            FVDATA(IFV)%ID_DT_OPTION = T_MONVOL(N)%IVOLU(27)
            FVDATA(IFV)%LAMBDA = ZERO
            FVDATA(IFV)%DTOLD = ZERO
            FVDATA(IFV)%CFL_COEF = T_MONVOL(N)%RVOLU(71)
            FVDATA(IFV)%DTMIN = T_MONVOL(N)%RVOLU(72)
            FVDATA(IFV)%PDISP = ZERO
            FVDATA(IFV)%PDISP_OLD = ZERO
C
            CALL FVVERIF(
     .           NTGA, T_MONVOL(N)%ELEMA, X, ID,
     .           DIRX, DIRY, DIRZ, DIR2X, DIR2Y,
     .           DIR2Z, ORIGX, ORIGY, ORIGZ, 
     .           LX, LY, LZ, T_MONVOL(N)%NODES, T_MONVOL(N)%IBUFA, T_MONVOL(N)%TAGELA, 
     .           TITR)
C    
            T_MONVOL(N)%RVOLU(44) = LX
            T_MONVOL(N)%RVOLU(45) = LY
            T_MONVOL(N)%RVOLU(53) = LZ
C
            NBX = T_MONVOL(N)%IVOLU(54)
            NBY = T_MONVOL(N)%IVOLU(55)          
            NBRIC = NBX * NBY          
            NNB = (NBX + 1) * (NBY + 1) * 2
            ALLOCATE(FVDATA(IFV)%BRIC(8,NBRIC),
     .           FVDATA(IFV)%TBRIC(13,NBRIC),
     .           FVDATA(IFV)%XB(3,NNB),
     .           FVDATA(IFV)%SFAC(6,4,NBRIC))
C
            CALL FVBRIC(T_MONVOL(N)%IVOLU, T_MONVOL(N)%RVOLU, T_MONVOL(N)%NODES, X, NNS)
C
            T_MONVOL(N)%IVOLU(50) = T_MONVOL(N)%IVOLU(46)
            T_MONVOL(N)%IVOLU(51) = T_MONVOL(N)%IVOLU(47)
            T_MONVOL(N)%IVOLU(52) = T_MONVOL(N)%IVOLU(48)
            T_MONVOL(N)%IVOLU(53) = T_MONVOL(N)%IVOLU(49)
C
            CALL FVMESH1(
     .           T_MONVOL(N)%NODES, T_MONVOL(N)%ELEM, X, T_MONVOL(N)%IVOLU, FVDATA(IFV)%BRIC,
     .           FVDATA(IFV)%XB, T_MONVOL(N)%RVOLU, NTG, NTGI, NBRIC, FVDATA(IFV)%TBRIC,
     .           FVDATA(IFV)%SFAC, FVDATA(IFV)%DLH, NBA, NTGA, NNA,
     .           T_MONVOL(N)%TBRIC, T_MONVOL(N)%TFAC, T_MONVOL(N)%TAGELS, T_MONVOL(N)%IBUFA,
     .           T_MONVOL(N)%ELEMA, T_MONVOL(N)%TAGELA, IXS, ID ,TITR, NB_NODE, ITYP)

            IF (KMESH(N) >= 2) THEN
               T_MONVOL(N)%KRA5 = 1 + NRVOLU * NVOLU + LRCBAG + LRBAGJET + LRBAGHOL +
     .              T_MONVOL(N)%IVOLU(34) + 7*NNT+4*NTGT+6*NNA
               T_MONVOL(N)%VELOCITY(1:3, 1:NNA) = ZERO
               DO I = 1, NNA
                  INODE = T_MONVOL(N)%IBUFA(I)
                  T_MONVOL(N)%NODE_COORD(1, I) = NODE_COORD(1, INODE)
                  T_MONVOL(N)%NODE_COORD(2, I) = NODE_COORD(2, INODE)
                  T_MONVOL(N)%NODE_COORD(3, I) = NODE_COORD(3, INODE)
               ENDDO
            ENDIF
C
            IF (NTGI > 0) THEN
              ILVOUT = T_MONVOL(N)%IVOLU(44)
              NSURFI=T_MONVOL(N)%INT_SURFID
              NSEG=IGRSURF(NSURFI)%NSEG
C-----------------------------
C  Elements de surface interne
C-----------------------------
              T_MONVOL(N)%POROSITY(1:NTGI) = ZERO
              CALL FVELINTE(T_MONVOL(N)%NODES, T_MONVOL(N)%ELEM(1, NTG + 1), IXC, IXTG,
     .                     PM, IPM, ILVOUT, IFV, NNT, NTG, T_MONVOL(N)%POROSITY,
     .                     ITAB, NSEG,IGRSURF(NSURFI)%ELTYP, NTGI, T_MONVOL(N)%ELTG,
     .                     NB_NODE,IGRSURF(NSURFI)%ELEM)
C--------------------------------
C  Injecteurs sur surface interne
C--------------------------------
              ALLOCATE(MINUS_SIGN_REVERSE(NTGI))
              MINUS_SIGN_REVERSE(:) = 0
              CALL FVINJECTINT(
     .               T_MONVOL(N)%NODES, T_MONVOL(N)%ELEM(1, NTG + 1), T_MONVOL(N)%IBAGJET,
     .               T_MONVOL(N)%NJET , IGRSURF          ,
     .               T_MONVOL(N)%ITAGEL(NTG + 1), NNS+NNI          , NTGI,NB_NODE,
     .               MINUS_SIGN_REVERSE)
C--------------------------------
C  Porous surface sur surface interne
C--------------------------------
              CALL FVVENTHOLEINT(
     .               T_MONVOL(N)%NODES, T_MONVOL(N)%ELEM(1, NTG + 1), T_MONVOL(N)%IBAGHOL,
     .               T_MONVOL(N)%NVENT, IGRSURF          ,
     .               T_MONVOL(N)%ITAGEL(NTG + 1), NNS+NNI          , NTGI, NB_NODE)
C
              CALL FVELSURF(
     .               T_MONVOL(N)%NODES, T_MONVOL(N)%ELEM(1, NTG + 1), IBID, IXC, IXTG, NTGI,
     .               T_MONVOL(N)%ELTG(NTG + 1), T_MONVOL(N)%MATTG(NTG + 1), NB_NODE, .FALSE.)
C-----------------------------------------------------------------------
C        REDEFINE INTERNAL TRIANGLE FOR NORMAL CONSISTENCY FOR INJECTORS
C-----------------------------------------------------------------------
              CALL FVINJNORMAL(
     .             T_MONVOL(N)%NODES, T_MONVOL(N)%ELEM(1, NTG + 1), IXC, IXTG, 
     .             T_MONVOL(N)%ITAGEL(NTG + 1), T_MONVOL(N)%ELTG(NTG + 1), NTGI, ILVOUT, 
     .             MINUS_SIGN_REVERSE)
              DEALLOCATE(MINUS_SIGN_REVERSE)
              CALL FVELPRINT(
     .               IXC, IXTG, NTGI, T_MONVOL(N)%ITAGEL(NTG + 1), T_MONVOL(N)%ELTG(NTG + 1),
     .               T_MONVOL(N)%IBAGHOL,  ILVOUT , 1 )
C------------------------------------------------------
C  Tag element interne pour l'option /TH/SURF mass flow
C------------------------------------------------------
              DO I=1,NSURF
                NSEG=IGRSURF(I)%NSEG
                CALL FVTHSURF(NSEG, NTGI,IGRSURF(I)%ELTYP, T_MONVOL(N)%ELTG(NTG + 1),
     .                        T_MONVOL(N)%THSURF_TAG(I, 1:NTGI + 1), IGRSURF(I)%ELEM)
              ENDDO
            ENDIF
C---------------------------------
C  Noeuds de briques additionnelles
C---------------------------------  
            IF (NNA > 0) THEN
               NNFV = T_MONVOL(N)%IVOLU(46)
               CALL FVNODBR(T_MONVOL(N)%IBUFA, NNA, NNFV, IFV, NB_NODE)
            ENDIF
C
            NNFV = T_MONVOL(N)%IVOLU(46)
            NTRFV = T_MONVOL(N)%IVOLU(47) 
            NPOLH = T_MONVOL(N)%IVOLU(49)
C------------------------------------
C    REFERENCE METRICS : VOLUME CHECK
C------------------------------------
            IF (IREF /= 0) THEN
               CALL FVVOLU(IFV,     ITYP, NNFV,  NTRFV, NPOLH,
     1              T_MONVOL(N)%NODES, T_MONVOL(N)%IBUFA, T_MONVOL(N)%ELEMA, T_MONVOL(N)%TAGELA,
     2              XYZINI,      T_MONVOL(N)%IVOLU,   T_MONVOL(N)%RVOLU,
     3              FVDATA(IFV)%IFVNOD, FVDATA(IFV)%RFVNOD, FVDATA(IFV)%IFVTRI,
     4              FVDATA(IFV)%IFVPOLY,FVDATA(IFV)%IFVTADR,FVDATA(IFV)%IFVPOLH,
     5              FVDATA(IFV)%IFVPADR,FVDATA(IFV)%IDPOLH, FVDATA(IFV)%MPOLH, 
     6              FVDATA(IFV)%EPOLH,  FVDATA(IFV)%VPOLH_INI )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--

            ENDIF
C----------------------------------------
C    COMPUTE MINIMUM LENGTH FOR TIME STEP
C----------------------------------------
            CALL FVLENGTH(IFV,    NNFV,         NTRFV,        NPOLH,
     1           T_MONVOL(N)%NODES, T_MONVOL(N)%IBUFA, T_MONVOL(N)%ELEMA, T_MONVOL(N)%TAGELA,
     2           XYZINI,     T_MONVOL(N)%IVOLU,   T_MONVOL(N)%RVOLU,
     3           FVDATA(IFV)%IFVNOD, FVDATA(IFV)%RFVNOD, FVDATA(IFV)%IFVTRI,
     4           FVDATA(IFV)%IFVPOLY,FVDATA(IFV)%IFVTADR,FVDATA(IFV)%IFVPOLH,
     5           FVDATA(IFV)%IFVPADR,FVDATA(IFV)%IBPOLH, FVDATA(IFV)%DLH    ) 
C----------------------------
C     COMPUTE INITIAL SURFACE
C----------------------------
            CALL FVELAREA(T_MONVOL(N)%NODES, T_MONVOL(N)%ELEM, XYZREF,   NTGT,
     1                    T_MONVOL(N)%ELAREA)
C
         ENDIF
      ENDDO
C
      RETURN
C
1000  FORMAT(
     . //,'     FVMBAG: FINITE VOLUME MESH     '/
     .    '     --------------------------     ')
C
      END



Chd|====================================================================
Chd|  APPLYSORT2FVM                 source/airbag/fvmesh0.F       
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        FVBAG_MOD                     share/modules1/fvbag_mod.F    
Chd|        FVMBAG_MESHCONTROL_MOD        ../common_source/modules/airbag/fvmbag_meshcontrol_mod.F
Chd|        MONVOL_STRUCT_MOD             share/modules1/monvol_struct_mod.F
Chd|        REORDER_MOD                   share/modules1/reorder_mod.F  
Chd|====================================================================
      SUBROUTINE APPLYSORT2FVM(T_MONVOL, IXS    , IXC,   IXTG,
     1                         ITAB  , NOM_OPT, NB_NODE)
C Description: Apply renumbering to FVMBAGS (ELTG et TBA) 
C after S*HEAD S*TAILS routines 
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE FVBAG_MOD
      USE FVMBAG_MESHCONTROL_MOD
      USE REORDER_MOD
      USE MONVOL_STRUCT_MOD
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
      INTEGER IXS(NIXS,*), IXC(NIXC,*), IXTG(NIXTG,*)
      INTEGER ITAB(*)                        
      INTEGER NOM_OPT(LNOPT1,*), NB_NODE
      TYPE(MONVOL_STRUCT_), DIMENSION(NVOLU), INTENT(INOUT) :: T_MONVOL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: IFV, N, ITYP, NNS, NTG, NBA, NTGA, NNA, 
     .     NNI, NTGI,  NNT,  IREF, ID, NTGT
      INTEGER I, J, IEL, NJET, NODE_ID
      INTEGER NJ1, NJ2, NJ3, P, NJ
      INTEGER, DIMENSION(:), ALLOCATABLE :: TMP
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      IFV=0
      DO N = 1, NVOLU
         ITYP = T_MONVOL(N)%TYPE
         ID = T_MONVOL(N)%ID
         IF (ITYP == 6 .OR. ITYP == 8 .OR. ITYP == 11) THEN
            IREF = T_MONVOL(N)%IVOLU(59)
            IFV = IFV + 1
            NNS = T_MONVOL(N)%NNS
            NTG = T_MONVOL(N)%NTG
            NNI = T_MONVOL(N)%NNI
            NTGI = T_MONVOL(N)%NTGI
            NBA = T_MONVOL(N)%NBRIC
            NTGA = T_MONVOL(N)%NTGA
            NNA = T_MONVOL(N)%NNA
            NNT = NNS + NNI
            NTGT = NTG + NTGI

C----------------------------------------------
C Parametres de decoupage automatique du volume
C----------------------------------------------
C           PERMUTATION ELTG
            IF (NTG > 0) THEN
              ALLOCATE(TMP(NTG))
              DO J = 1, NTG
                IEL = T_MONVOL(N)%ELTG(J)
                IF (IEL <= NUMELC) THEN
                  TMP(J) = PERMUTATION%SHELL(IEL + NUMELC)
                ELSE IF (IEL > NUMELC) THEN
                  TMP(J) = NUMELC + PERMUTATION%TRIANGLE(IEL - NUMELC + NUMELTG)
                ENDIF
              ENDDO 
              T_MONVOL(N)%ELTG(1:NTG) = TMP(1:NTG)
              DEALLOCATE(TMP)
            ENDIF
     
            IF (NTGI > 0) THEN
              ALLOCATE(TMP(NTGI))
              DO J = 1, NTGI
                IEL = T_MONVOL(N)%ELTG(NTG + J) 
                IF (IEL <= NUMELC) THEN
                  TMP(J) = PERMUTATION%SHELL(IEL + NUMELC)
                ELSE IF (IEL > NUMELC) THEN
                  TMP(J) = NUMELC + PERMUTATION%TRIANGLE(IEL - NUMELC + NUMELTG)
                ENDIF
              ENDDO 
              T_MONVOL(N)%ELTG(NTG + 1: NTG + NTGI) = TMP(1:NTGI)
              DEALLOCATE(TMP)
            ENDIF

C      PERMUTATION TBA 
            IF (NTG > 0 .AND. (.NOT. TETRAMESHER_USED)) THEN
              DO J = 1, NBA
                IEL = T_MONVOL(N)%TBRIC(1, J) 
                T_MONVOL(N)%TBRIC(1, J) = PERMUTATION%SOLID(IEL + NUMELS)
              ENDDO 
            ELSE IF(NTG > 0 .AND. TETRAMESHER_USED) THEN
C     Not possible
            ENDIF
         ENDIF
      ENDDO
C
      RETURN
C
      END
