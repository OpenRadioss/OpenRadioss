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
Chd|  INI_INIMAP1D                  source/initial_conditions/inimap/ini_inimap1d.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        SOLVE_EINT                    source/initial_conditions/inimap/ini_inimap1d.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INIMAP1D_MOD                  share/modules1/inimap1d_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE INI_INIMAP1D(INIMAP1D ,ELBUF_TAB ,IPART  ,IPARG   ,IPARTS   ,
     .                        IPARTQ   ,XGRID     ,VEL    ,IXS     ,IXQ      ,
     .                        IXTG     ,PM        ,IPM    ,BUFMAT  ,MULTI_FVM, 
     .                        PLD      ,NPC       ,IGRBRIC,IGRQUAD ,IGRSH3N  ,
     .                        NPTS)
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
      USE INIMAP1D_MOD  
      USE MULTI_FVM_MOD
      USE GROUPDEF_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C     I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc" 
C-----------------------------------------------
C     C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr17_c.inc"
!     NGROUP
#include      "com01_c.inc"
!     NFUNCT
#include      "com04_c.inc"
!     MVSIZ
#include      "mvsiz_p.inc"
!     IOUT
#include      "units_c.inc"
!     IALEFVM
#include      "alefvm.inc"
!     N0PHAS, NVPHAS
#include      "mmale51_c.inc"
C-----------------------------------------------
C     D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(INIMAP1D_STRUCT), TARGET, DIMENSION(NINIMAP1D), INTENT(INOUT) :: INIMAP1D
      TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP), INTENT(INOUT), TARGET :: ELBUF_TAB
      INTEGER, INTENT(IN) :: IPART(LIPART1, *), NPC(*)
      INTEGER, INTENT(IN) :: IPARTS(*), IPARTQ(*),IPM(NPROPMI, *),IPARG(NPARG, NGROUP),
     .  IXS(NIXS, NUMELS), IXQ(NIXQ, NUMELQ), IXTG(NIXTG, NUMELTG),NPTS
      my_real, INTENT(IN) :: XGRID(3, *), PM(NPROPM, NUMMAT), BUFMAT(*)
      my_real, INTENT(IN), TARGET :: PLD(2, NPTS/2)
      my_real, INTENT(INOUT) :: VEL(3, *)
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE (GROUP_)  , DIMENSION(NGRQUAD) :: IGRQUAD
      TYPE (GROUP_)  , DIMENSION(NGRSH3N) :: IGRSH3N
C-----------------------------------------------
C     L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real,  TARGET :: PLD_INV(NPTS/2,2)
      INTEGER :: JJ, KK, IAD, NELEM, NG, NFT, NEL, IFIRST, ILAST, IAD1, IAD2, IMID, FUNC,
     .     FUNC1, FUNC2, MATID, MLW
      LOGICAL :: CONT
      INTEGER :: II, IFUNC, NPT, FIRST, LAST, ICOOR, INODE, NODEID, IND, SHIFT,
     .     NODE1, NODE2, NODE3, NODE4, NODE5, NODE6, NODE7, NODE8,
     .     ELEMID
      my_real :: X0, Y0, Z0, RADIUS, XC, YC, ZC, VALUE1, VALUE2, NX, NY, NZ, VALUE, 
     .     XNODE, YNODE, ZNODE, RAD1, RAD2, X1, Y1, Z1, XP, YP, ZP
      INTEGER, DIMENSION(:), ALLOCATABLE :: ELEM_LIST
      INTEGER :: LOCAL_ELEM_LIST(MVSIZ)
      my_real, DIMENSION(:, :), ALLOCATABLE :: PRES, EINT
      my_real ::  TT, RES, RHO
      TYPE(G_BUFEL_), POINTER :: GBUF
      INTEGER :: NBNODE, NODELIST(8), ITY, ISOLNOD,
     .     GRBRICID,GRQUADID,GRSH3NID, NBMAT, NBMAT_TARGET, IMAT, KPHASE, NUVAR, IADBUF, NPAR, IFORM, 
     .     M51_SUBMAT_ID(4),TAG_MAT(NUMMAT)
      my_real, POINTER, DIMENSION(:) :: PTRx, PTRy 

      TYPE PSEUDO_POINTER_ARRAY
        my_real, POINTER, DIMENSION(:) :: TEMP   
      END TYPE
      TYPE (PSEUDO_POINTER_ARRAY) :: SUBMAT(21)

      CHARACTER*2 :: Str1, Str2      
      my_real :: VFRAC(MVSIZ,21),DENSFRAC 
C-----------------------------------------------
C     S o u r c e   L i n e s
C-----------------------------------------------
      
      DO KK=1,NPTS/2
        PLD_INV(KK,1)=PLD(1,KK)
        PLD_INV(KK,2)=PLD(2,KK)          
      ENDDO

      IF (N2D  ==  0) THEN
         ALLOCATE(ELEM_LIST(NUMELS))
         ELEM_LIST(1:NUMELS)=0
      ELSE
         ALLOCATE(ELEM_LIST(NUMELQ + NUMELTG))
         ELEM_LIST(1:NUMELQ+NUMELTG)=0
      ENDIF
      
      TAG_MAT(1:NUMMAT)=0

      GRBRICID = 0
      GRQUADID = 0
      GRSH3NID = 0

      DO KK = 1, NINIMAP1D
         IF(.NOT.INIMAP1D(KK)%CORRECTLY_READ)CYCLE
         NBMAT = INIMAP1D(KK)%NBMAT
         IF(NBMAT==0)CYCLE !something went wrong with reader
         ALLOCATE(PRES(MVSIZ, NBMAT), EINT(MVSIZ, NBMAT))
         IF (INIMAP1D(KK)%GRBRICID  /=  0) THEN
            GRBRICID = INIMAP1D(KK)%GRBRICID
         ELSEIF (INIMAP1D(KK)%GRQUADID  /=  0) THEN
            GRQUADID = INIMAP1D(KK)%GRQUADID
         ELSEIF (INIMAP1D(KK)%GRSH3NID  /=  0) THEN
            GRSH3NID = INIMAP1D(KK)%GRSH3NID
         ENDIF
         X0 = XGRID(1, INIMAP1D(KK)%NODEID1)
         Y0 = XGRID(2, INIMAP1D(KK)%NODEID1)
         Z0 = XGRID(3, INIMAP1D(KK)%NODEID1)
         IF (INIMAP1D(KK)%NODEID2  >  0) THEN
            X1 = XGRID(1, INIMAP1D(KK)%NODEID2)
            Y1 = XGRID(2, INIMAP1D(KK)%NODEID2)
            Z1 = XGRID(3, INIMAP1D(KK)%NODEID2)
         ENDIF
         IF (GRBRICID > 0) THEN
           NELEM = IGRBRIC(GRBRICID)%NENTITY
           DO II = 1, NELEM
              ELEM_LIST(II) = IGRBRIC(GRBRICID)%ENTITY(II)
           ENDDO
         ELSEIF (GRQUADID > 0) THEN
           NELEM = IGRQUAD(GRQUADID)%NENTITY
           DO II = 1, NELEM
              ELEM_LIST(II) = IGRQUAD(GRQUADID)%ENTITY(II)
           ENDDO
         ELSEIF (GRSH3NID > 0) THEN
           NELEM = IGRSH3N(GRSH3NID)%NENTITY
           DO II = 1, NELEM
              ELEM_LIST(II) = IGRSH3N(GRSH3NID)%ENTITY(II)
           ENDDO
         ENDIF
C     If NSPMD != 1 the ids are not ordered
         CALL QUICKSORT_I(ELEM_LIST(1:NELEM), 1, NELEM)
         DO NG = 1, NGROUP
            MLW = IPARG(1, NG)
            NEL = IPARG(2, NG)
            NFT = IPARG(3, NG)
            ITY = IPARG(5, NG)
            SELECT CASE(MLW)
            CASE (3, 4, 6, 49, 51, 151)
               CONTINUE
            CASE DEFAULT
               CYCLE
            END SELECT  
            MATID=0        
            IF (ITY == 1) THEN
               MATID = IXS(1, NFT + 1)
            ELSE IF (ITY == 2) THEN
               MATID = IXQ(1, NFT + 1)
            ELSE IF (ITY == 7) THEN
               MATID = IXTG(1, NFT + 1)
            ELSE
               CYCLE ! not compatible elem type (beam, truss, spring, shell ...)
            ENDIF
            M51_SUBMAT_ID(1:4) = 0
            IF (MLW == 51) THEN
              IFORM = IPM(62, MATID) 
              IF(IFORM>=2.AND.IFORM<=6)CYCLE             
              M51_SUBMAT_ID(1:4) = IPM(51:54, MATID)
               JJ = MINLOC(M51_SUBMAT_ID,1)
               NBMAT_TARGET = 4 
               IF(M51_SUBMAT_ID(JJ)==0)NBMAT_TARGET=MAX(1,JJ-1)
            ELSEIF (MLW == 151)THEN
              NBMAT_TARGET = MULTI_FVM%NBMAT
            ELSE
              NBMAT_TARGET = 1              
            ENDIF
c                        
            NUVAR = IPM(8, MATID)
            IADBUF = IPM(7, MATID)
            ISOLNOD = IPARG(28, NG)
            GBUF => ELBUF_TAB(NG)%GBUF
C     Find first index of elem_list such as corresponding elem lies in the group
            IFIRST = 1
            SHIFT = 0
            DO WHILE(ELEM_LIST(IFIRST + SHIFT)  <  1 + NFT .AND. IFIRST + SHIFT  <=  NELEM)
               SHIFT = SHIFT + 1
            ENDDO
            IF (SHIFT  >  NELEM) THEN
               CYCLE
            ELSE
               IFIRST = IFIRST + SHIFT
            ENDIF
            
            ILAST = IFIRST
            SHIFT = 0
            DO WHILE(ELEM_LIST(ILAST + SHIFT)  <=  NEL + NFT .AND. ILAST + SHIFT  <  NELEM)
                 SHIFT = SHIFT + 1
                 IF( SHIFT  >=  NEL)EXIT
            ENDDO
C     IFIRST + SHIFT is the first element which ID is greater than NFT + NEL
            IF (ILAST + SHIFT  <  NELEM) THEN
               SHIFT = SHIFT - 1
               ILAST = ILAST + SHIFT
            ELSE
               ILAST = NELEM
            ENDIF

            !IF some elems are targeted in this current group NG, check material law
            IF(IFIRST>0 .AND. IFIRST<=ILAST )THEN
              SELECT CASE(MLW)
              CASE (3, 4, 6, 49, 51, 151)
                 CONTINUE
              CASE DEFAULT
                 !WRITE(IOUT, '(A, I10)') "**WARNING : /INIMAP1D OPTION NOT COMPATIBLE WITH MATERIAL LAW", MLW
                 CYCLE
              END SELECT              
              IF (MLW == 51) THEN
                 IFORM = IPM(62, MATID)
                 IF (IFORM /= 12 .AND. INIMAP1D(KK)%FORMULATION == 1) THEN
                    CALL ANCMSG(MSGID = 1732, MSGTYPE=MSGERROR, ANMODE=ANINFO,I1 = IPM(1, MATID), I2 = INIMAP1D(KK)%ID)
                 ENDIF
              ENDIF            
            ENDIF

C Check submaterial consistency
            IF(TAG_MAT(MATID)==0)THEN                                                                                                                                                                            
              IF(NBMAT /= NBMAT_TARGET)THEN                                                                                            
                WRITE(Str1,'(i2)')NBMAT                                                                                                
                WRITE(Str2,'(i2)')NBMAT_TARGET                                                                                         
                CALL ANCMSG(MSGID = 2048, MSGTYPE=MSGERROR, ANMODE=ANINFO,                                                             
     .                C1="INIMAP1D",                                                                                                   
     .                C2="NUMBER OF SUBMATERIAL(S) FROM TARGET MATERIAL LAW:"//Str1,
     .                C3=" IS DIFFERENT FROM SUBMATERIAL(S) NUMBER IN TARGET DOMAIN:"//Str2,                                       
     .                C4="INIMAP1D",                                                                                                   
     .                I1 = IPM(1, MATID),                                                                                              
     .                I2 = INIMAP1D(KK)%ID)                                                                                            
                TAG_MAT(MATID) = 1 !display this error material once per material law. Not for each cell using this material law.      
                NBMAT=MIN(NBMAT,NBMAT_TARGET) !in order to finish starter without memory corruption                    
              ENDIF                                                                                                                    
            ENDIF                                                                                                                      
            
            DO II = IFIRST, ILAST
               ELEMID = ELEM_LIST(II)
               IF (ELEMID  <  1 + NFT .OR. ELEMID  >  NEL + NFT) THEN
               ENDIF
               IF (ITY  ==  1 .AND. ISOLNOD  ==  8) THEN
C     HEXA
                  NODE1 = IXS(2, ELEMID)
                  NODE2 = IXS(3, ELEMID)
                  NODE3 = IXS(4, ELEMID)
                  NODE4 = IXS(5, ELEMID)
                  NODE5 = IXS(6, ELEMID)
                  NODE6 = IXS(7, ELEMID)
                  NODE7 = IXS(8, ELEMID)
                  NODE8 = IXS(9, ELEMID)
                  XC = ONE_OVER_8 * (XGRID(1, NODE1) + XGRID(1, NODE2) + XGRID(1, NODE3) + XGRID(1, NODE4)
     .                 + XGRID(1, NODE5) + XGRID(1, NODE6) + XGRID(1, NODE7) + XGRID(1, NODE8))
                  YC = ONE_OVER_8 * (XGRID(2, NODE1) + XGRID(2, NODE2) + XGRID(2, NODE3) + XGRID(2, NODE4)
     .                 + XGRID(2, NODE5) + XGRID(2, NODE6) + XGRID(2, NODE7) + XGRID(2, NODE8))
                  ZC = ONE_OVER_8 * (XGRID(3, NODE1) + XGRID(3, NODE2) + XGRID(3, NODE3) + XGRID(3, NODE4)
     .                 + XGRID(3, NODE5) + XGRID(3, NODE6) + XGRID(3, NODE7) + XGRID(3, NODE8))
                  NBNODE = 8
                  NODELIST(1) = 2
                  NODELIST(2) = 3
                  NODELIST(3) = 4
                  NODELIST(4) = 5
                  NODELIST(5) = 6
                  NODELIST(6) = 7
                  NODELIST(7) = 8
                  NODELIST(8) = 9
               ELSEIF (ITY  ==  1 .AND. ISOLNOD  ==  4) THEN
C     TETRA
                  NODE1 = IXS(2, ELEMID)
                  NODE2 = IXS(4, ELEMID)
                  NODE3 = IXS(7, ELEMID)
                  NODE4 = IXS(6, ELEMID)
                  XC = FOURTH * (XGRID(1, NODE1) + XGRID(1, NODE2) + XGRID(1, NODE3) + XGRID(1, NODE4))
                  YC = FOURTH * (XGRID(2, NODE1) + XGRID(2, NODE2) + XGRID(2, NODE3) + XGRID(2, NODE4))
                  ZC = FOURTH * (XGRID(3, NODE1) + XGRID(3, NODE2) + XGRID(3, NODE3) + XGRID(3, NODE4))
                  NBNODE = 4
                  NODELIST(1) = 2
                  NODELIST(2) = 4
                  NODELIST(3) = 7
                  NODELIST(4) = 6
               ELSEIF (ITY  ==  2) THEN
C     QUAD
                  NODE1 = IXQ(2, ELEMID)
                  NODE2 = IXQ(3, ELEMID)
                  NODE3 = IXQ(4, ELEMID)
                  NODE4 = IXQ(5, ELEMID)
                  XC = FOURTH * (XGRID(1, NODE1) + XGRID(1, NODE2) + XGRID(1, NODE3) + XGRID(1, NODE4))
                  YC = FOURTH * (XGRID(2, NODE1) + XGRID(2, NODE2) + XGRID(2, NODE3) + XGRID(2, NODE4))
                  ZC = FOURTH * (XGRID(3, NODE1) + XGRID(3, NODE2) + XGRID(3, NODE3) + XGRID(3, NODE4))
                  NBNODE = 4
                  NODELIST(1) = 2
                  NODELIST(2) = 3
                  NODELIST(3) = 4
                  NODELIST(4) = 5
               ELSEIF (ITY  ==  7) THEN
C     TRIANGLES
                  NODE1 = IXTG(2, ELEMID)
                  NODE2 = IXTG(3, ELEMID)
                  NODE3 = IXTG(4, ELEMID)
                  XC = THIRD * (XGRID(1, NODE1) + XGRID(1, NODE2) + XGRID(1, NODE3))
                  YC = THIRD * (XGRID(2, NODE1) + XGRID(2, NODE2) + XGRID(2, NODE3))
                  ZC = THIRD * (XGRID(3, NODE1) + XGRID(3, NODE2) + XGRID(3, NODE3))
                  NBNODE = 3
                  NODELIST(1) = 2
                  NODELIST(2) = 3
                  NODELIST(3) = 4
               ENDIF
               IF (INIMAP1D(KK)%PROJ  ==  3) THEN
C     SPHERICAL
                  RADIUS = SQRT((XC - X0) * (XC - X0) + (YC - Y0) * (YC - Y0) + (ZC - Z0) * (ZC - Z0))
               ELSEIF (INIMAP1D(KK)%PROJ  ==  1) THEN
C     PLANAR
                  RADIUS = (XC - X0) * INIMAP1D(KK)%NX + 
     .                 (YC - Y0) * INIMAP1D(KK)%NY + 
     .                 (ZC - Z0) * INIMAP1D(KK)%NZ 
               ELSE
C     CYLINDRICAL PROJ
                  TT = (XC - X0) * (X1 - X0) + (YC - Y0) * (Y1 - Y0) + (ZC - Z0) * (Z1 - Z0)
                  TT = TT / ((X1 - X0) * (X1 - X0) + 
     .                 (Y1 - Y0) * (Y1 - Y0) + 
     .                 (Z1 - Z0) * (Z1 - Z0))
                  XP = X0 + TT * (X1 - X0)
                  YP = Y0 + TT * (Y1 - Y0)
                  ZP = Z0 + TT * (Z1 - Z0)
                  RADIUS = SQRT((XC - XP) * (XC - XP) + (YC - YP) * (YC - YP) + (ZC - ZP) * (ZC - ZP))
               ENDIF
               
               DO IMAT = 1, NBMAT
C     Alpha
                  IFUNC = INIMAP1D(KK)%FUNC_ALPHA(IMAT)
                  IF (IFUNC /= 0) THEN
                    IF(IFUNC>0)THEN
                      !--function_identifier
                      NPT = (NPC(IFUNC + 1) - NPC(IFUNC)) / 2
                      FIRST = 1 + (NPC(IFUNC) - 1) / 2                      
                      PTRx => PLD_INV(1:NPTS/2,1)  !NPTS : 2*nb of all function points     NPT:for current function
                      PTRy => PLD_INV(1:NPTS/2,2)
                    ELSE
                      !--data from file
                      NPT = INIMAP1D(KK)%NUM_CENTROIDS
                      FIRST = 1
                      PTRx => INIMAP1D(KK)%X(1:NPT)
                      PTRy => INIMAP1D(KK)%SUBMAT(IMAT)%VFRAC(1:NPT)                      
                    ENDIF
                    SHIFT = 0                                                                                            
                    DO WHILE (PTRx(FIRST + SHIFT)  <  RADIUS ) 
                         SHIFT = SHIFT + 1 
                         IF(SHIFT  >=  NPT)EXIT
                    ENDDO                                                                                                
                    IND = FIRST + SHIFT                                                                                  
                    IF (SHIFT  ==  0) THEN                                                                               
                       RES = PTRy(FIRST)                                                                                 
                       IF (MLW == 51) THEN                                                                               
                          KPHASE = N0PHAS + (M51_SUBMAT_ID(IMAT) - 1) * NVPHAS                                           
                          ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(ELEMID - NFT + NEL * (1 + KPHASE - 1)) = RES             
                          ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(ELEMID - NFT + NEL * (23 + KPHASE - 1)) = RES            
                       ELSE                                                                                              
                          ELBUF_TAB(NG)%BUFLY(IMAT)%LBUF(1,1,1)%VOL(ELEMID - NFT) = RES * GBUF%VOL(ELEMID - NFT)         
                       ENDIF                                                                                            
                    ELSEIF (SHIFT  <  NPT) THEN                                                                         
                       RAD1 = PTRx(IND - 1)                                                                              
                       RAD2 = PTRx(IND)                                                                                  
                       VALUE1 = PTRy(IND - 1)                                                                            
                       VALUE2 = PTRy(IND)                                                                                
                       RES = (VALUE1 + (VALUE2 - VALUE1) / (RAD2 - RAD1) * (RADIUS - RAD1))                              
                       IF (MLW == 51) THEN                                                                               
                          KPHASE = N0PHAS + (M51_SUBMAT_ID(IMAT) - 1) * NVPHAS                                           
                          ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(ELEMID - NFT + NEL * (1 + KPHASE - 1)) = RES             
                          ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(ELEMID - NFT + NEL * (23 + KPHASE - 1)) = RES            
                       ELSE                                                                                              
                          ELBUF_TAB(NG)%BUFLY(IMAT)%LBUF(1,1,1)%VOL(ELEMID - NFT) = RES * GBUF%VOL(ELEMID - NFT)         
                       ENDIF                                                                                           
                    ELSE                                                                                                 
                       RES = PTRy(FIRST + NPT - 1)                                                                       
                       IF (MLW == 51) THEN                                                                               
                          KPHASE = N0PHAS + (M51_SUBMAT_ID(IMAT) - 1) * NVPHAS                                           
                          ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(ELEMID - NFT + NEL * (1 + KPHASE - 1)) = RES             
                          ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(ELEMID - NFT + NEL * (23 + KPHASE - 1)) = RES            
                       ELSE                                                                                              
                          ELBUF_TAB(NG)%BUFLY(IMAT)%LBUF(1,1,1)%VOL(ELEMID - NFT) = RES * GBUF%VOL(ELEMID - NFT)         
                       ENDIF                                                                                            
                    ENDIF                                                                                               
                  ELSE
                    RES = ONE                                                                                  
                    IF (MLW == 51) THEN                                                                       
                       KPHASE = N0PHAS + (M51_SUBMAT_ID(IMAT) - 1) * NVPHAS                                   
                       ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(ELEMID - NFT + NEL * (1 + KPHASE - 1)) = RES     
                       ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(ELEMID - NFT + NEL * (23 + KPHASE - 1)) = RES    
                    ELSE                                                                                      
                       ELBUF_TAB(NG)%BUFLY(IMAT)%LBUF(1,1,1)%VOL(ELEMID - NFT) = RES * GBUF%VOL(ELEMID - NFT) 
                    ENDIF 
                  ENDIF
                  VFRAC(ELEMID-NFT,IMAT) = RES  
C     RHO
                  IFUNC = INIMAP1D(KK)%FUNC_RHO(IMAT)
                  IF(IFUNC /= 0)THEN
                    IF(IFUNC > 0)THEN
                      !--function_identifier
                      NPT = (NPC(IFUNC + 1) - NPC(IFUNC)) / 2
                      FIRST = 1 + (NPC(IFUNC) - 1) / 2                      
                      PTRx => PLD_INV(1:NPTS/2,1)  !NPTS : 2*nb of all function points     NPT:for current function
                      PTRy => PLD_INV(1:NPTS/2,2)
                    ELSE
                      !--data from file
                      NPT = INIMAP1D(KK)%NUM_CENTROIDS
                      FIRST = 1
                      PTRx => INIMAP1D(KK)%X(1:NPT)
                      PTRy => INIMAP1D(KK)%SUBMAT(IMAT)%RHO(1:NPT)                      
                    ENDIF
                    SHIFT = 0
                     DO WHILE (PTRx(FIRST + SHIFT)  <  RADIUS ) 
                        SHIFT = SHIFT + 1
                        IF( SHIFT  >=  NPT)EXIT
                     ENDDO
                     IND = FIRST + SHIFT
                     IF (SHIFT  ==  0) THEN
                        RES = PTRy(FIRST) * INIMAP1D(KK)%FAC_RHO(IMAT)  
                        IF (MLW == 51) THEN
                           KPHASE = N0PHAS + (M51_SUBMAT_ID(IMAT) - 1) * NVPHAS
                           ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(ELEMID - NFT + NEL * (9 + KPHASE - 1)) = RES
                           ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(ELEMID - NFT + NEL * (12 + KPHASE - 1)) = RES
                           ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(ELEMID - NFT + NEL * (20 + KPHASE - 1)) = RES
                        ELSE
                           ELBUF_TAB(NG)%BUFLY(IMAT)%LBUF(1,1,1)%RHO(ELEMID - NFT) = RES  
                        ENDIF
                     ELSEIF (SHIFT  <  NPT) THEN
                        RAD1 = PTRx(IND - 1)
                        RAD2 = PTRx(IND)
                        VALUE1 = PTRy(IND - 1)
                        VALUE2 = PTRy(IND)
                        RES = (VALUE1 + (VALUE2 - VALUE1) / (RAD2 - RAD1) * (RADIUS - RAD1)) * INIMAP1D(KK)%FAC_RHO(IMAT)    
                        IF (MLW == 51) THEN
                           KPHASE = N0PHAS + (M51_SUBMAT_ID(IMAT) - 1) * NVPHAS
                           ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(ELEMID - NFT + NEL * (9 + KPHASE - 1)) = RES
                           ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(ELEMID - NFT + NEL * (12 + KPHASE - 1)) = RES
                           ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(ELEMID - NFT + NEL * (20 + KPHASE - 1)) = RES
                        ELSE
                           ELBUF_TAB(NG)%BUFLY(IMAT)%LBUF(1,1,1)%RHO(ELEMID - NFT) = RES
                        ENDIF
                     ELSE
                        RES = PTRy(FIRST + NPT - 1) * INIMAP1D(KK)%FAC_RHO(IMAT)  
                        IF (MLW == 51) THEN
                           KPHASE = N0PHAS + (M51_SUBMAT_ID(IMAT) - 1) * NVPHAS
                           ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(ELEMID - NFT + NEL * (9 + KPHASE - 1)) = RES
                           ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(ELEMID - NFT + NEL * (12 + KPHASE - 1)) = RES
                           ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(ELEMID - NFT + NEL * (20 + KPHASE - 1)) = RES
                        ELSE
                           ELBUF_TAB(NG)%BUFLY(IMAT)%LBUF(1,1,1)%RHO(ELEMID - NFT) = RES                   
                        ENDIF
                     ENDIF
                  ELSE
                     RES = INIMAP1D(KK)%FAC_RHO(IMAT)   
                     IF (MLW == 51) THEN
                        KPHASE = N0PHAS + (M51_SUBMAT_ID(IMAT) - 1) * NVPHAS
                        ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(ELEMID - NFT + NEL * (9 + KPHASE - 1)) = RES
                        ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(ELEMID - NFT + NEL * (12 + KPHASE - 1)) = RES
                        ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(ELEMID - NFT + NEL * (20 + KPHASE - 1)) = RES
                     ELSE
                        ELBUF_TAB(NG)%BUFLY(IMAT)%LBUF(1,1,1)%RHO(ELEMID - NFT) = RES
                     ENDIF      
                  ENDIF
                  
                  IF (INIMAP1D(KK)%FORMULATION  ==  2) THEN
C     EINT
                     !not managed when reading from file (FORMULATION=1)
                     IFUNC = INIMAP1D(KK)%FUNC_ENER(IMAT)
                     IF (IFUNC > 0) THEN
                        NPT = (NPC(IFUNC + 1) - NPC(IFUNC)) / 2
                        FIRST = 1 + (NPC(IFUNC) - 1) / 2
                        SHIFT = 0
                        DO WHILE (PTRx(FIRST + SHIFT)  <  RADIUS) 
                           SHIFT = SHIFT + 1
                           IF( SHIFT  >=  NPT)EXIT
                        ENDDO
                        IND = FIRST + SHIFT
                        IF (SHIFT  ==  0) THEN
                           RES = PTRy(FIRST) * INIMAP1D(KK)%FAC_PRES_ENER(IMAT)
                           IF (MLW == 51) THEN
                              KPHASE = N0PHAS + (M51_SUBMAT_ID(IMAT) - 1) * NVPHAS
                              ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(ELEMID - NFT + NEL * (8 + KPHASE - 1)) = RES
                              ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(ELEMID - NFT + NEL * (21 + KPHASE - 1)) = RES
                           ELSE
                              ELBUF_TAB(NG)%BUFLY(IMAT)%LBUF(1,1,1)%EINT(ELEMID - NFT) = RES
                           ENDIF
                        ELSEIF (SHIFT  <  NPT) THEN
                           RAD1 = PTRx(IND - 1)
                           RAD2 = PTRx(IND)
                           VALUE1 = PTRy(IND - 1)
                           VALUE2 = PTRy(IND)
                           RES = (VALUE1 + (VALUE2 - VALUE1) / (RAD2 - RAD1) * (RADIUS - RAD1)) * INIMAP1D(KK)%FAC_PRES_ENER(IMAT)
                           IF (MLW == 51) THEN
                              KPHASE = N0PHAS + (M51_SUBMAT_ID(IMAT) - 1) * NVPHAS
                              ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(ELEMID - NFT + NEL * (8 + KPHASE - 1)) = RES
                              ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(ELEMID - NFT + NEL * (21 + KPHASE - 1)) = RES
                           ELSE
                              ELBUF_TAB(NG)%BUFLY(IMAT)%LBUF(1,1,1)%EINT(ELEMID - NFT) = RES
                           ENDIF
                        ELSE
                           RES = PTRy(FIRST + NPT - 1) * INIMAP1D(KK)%FAC_PRES_ENER(IMAT)
                           IF (MLW == 51) THEN
                              KPHASE = N0PHAS + (M51_SUBMAT_ID(IMAT) - 1) * NVPHAS
                              ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(ELEMID - NFT + NEL * (8 + KPHASE - 1)) = RES
                              ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(ELEMID - NFT + NEL * (21 + KPHASE - 1)) = RES
                           ELSE
                              ELBUF_TAB(NG)%BUFLY(IMAT)%LBUF(1,1,1)%EINT(ELEMID - NFT) = RES
                           ENDIF
                        ENDIF
                     ELSE
                        RES = INIMAP1D(KK)%FAC_PRES_ENER(IMAT)
                        IF (MLW == 51) THEN
                           KPHASE = N0PHAS + (M51_SUBMAT_ID(IMAT) - 1) * NVPHAS
                           ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(ELEMID - NFT + NEL * (8 + KPHASE - 1)) = RES
                           ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(ELEMID - NFT + NEL * (21 + KPHASE - 1)) = RES
                        ELSE
                           ELBUF_TAB(NG)%BUFLY(IMAT)%LBUF(1,1,1)%EINT(ELEMID - NFT) = RES
                        ENDIF
                     ENDIF
                     EINT(ELEMID-NFT,IMAT) = RES                     
                  ELSE IF (INIMAP1D(KK)%FORMULATION  ==  1) THEN
C     PRES
                     IFUNC = INIMAP1D(KK)%FUNC_PRES(IMAT)
                     IF(IFUNC /= 0)THEN
                       IF(IFUNC > 0)THEN
                         !--function_identifier
                         NPT = (NPC(IFUNC + 1) - NPC(IFUNC)) / 2
                         FIRST = 1 + (NPC(IFUNC) - 1) / 2                      
                         PTRx => PLD_INV(1:NPTS/2,1)  !NPTS : 2*nb of all function points     NPT:for current function
                         PTRy => PLD_INV(1:NPTS/2,2)
                       ELSE
                         !--data from file
                         NPT = INIMAP1D(KK)%NUM_CENTROIDS
                         FIRST = 1
                         PTRx => INIMAP1D(KK)%X(1:NPT)
                         PTRy => INIMAP1D(KK)%SUBMAT(IMAT)%PRES(1:NPT)                      
                       ENDIF
                       SHIFT = 0
                       DO WHILE (PTRx(FIRST + SHIFT)  <  RADIUS)                                 
                          SHIFT = SHIFT + 1 
                          IF( SHIFT  >=  NPT)EXIT                                                                           
                       ENDDO                                                                                           
                       IND = FIRST + SHIFT                                                                             
                       IF (SHIFT  ==  0) THEN                                                                          
                          PRES(ELEMID - NFT, IMAT) = PTRy(FIRST) * INIMAP1D(KK)%FAC_PRES_ENER(IMAT)                    
                       ELSEIF (SHIFT  <  NPT) THEN                                                                    
                          RAD1 = PTRx(IND - 1)                                                                         
                          RAD2 = PTRx(IND)                                                                             
                          VALUE1 = PTRy(IND - 1)                                                                       
                          VALUE2 = PTRy(IND)                                                                           
                          PRES(ELEMID - NFT, IMAT) = (VALUE1 + (VALUE2 - VALUE1) / (RAD2 - RAD1) * (RADIUS - RAD1)) *  
     .                         * INIMAP1D(KK)%FAC_PRES_ENER(IMAT)                                                      
                       ELSE                                                                                            
                          VALUE1 = PTRy(FIRST + NPT - 1)                                                               
                          PRES(ELEMID - NFT, IMAT) = VALUE1 * INIMAP1D(KK)%FAC_PRES_ENER(IMAT)                         
                       ENDIF                                                                                           
                     ELSE
                        PRES(ELEMID - NFT, IMAT) = INIMAP1D(KK)%FAC_PRES_ENER(IMAT)
                     ENDIF
                     
                     
                     IF(MULTI_FVM%IS_USED)THEN
                      ELBUF_TAB(NG)%BUFLY(IMAT)%LBUF(1,1,1)%SIG(NEL*(1-1)+ELEMID-NFT)=-PRES(ELEMID - NFT, IMAT)
                      ELBUF_TAB(NG)%BUFLY(IMAT)%LBUF(1,1,1)%SIG(NEL*(2-1)+ELEMID-NFT)=-PRES(ELEMID - NFT, IMAT)
                      ELBUF_TAB(NG)%BUFLY(IMAT)%LBUF(1,1,1)%SIG(NEL*(3-1)+ELEMID-NFT)=-PRES(ELEMID - NFT, IMAT)
                      ELBUF_TAB(NG)%GBUF%SIG(NEL*(1-1)+ELEMID-NFT)=-PRES(ELEMID - NFT, IMAT) !same pressure for all phases
                      ELBUF_TAB(NG)%GBUF%SIG(NEL*(2-1)+ELEMID-NFT)=-PRES(ELEMID - NFT, IMAT)
                      ELBUF_TAB(NG)%GBUF%SIG(NEL*(3-1)+ELEMID-NFT)=-PRES(ELEMID - NFT, IMAT)                      
                     ELSE
                      ELBUF_TAB(NG)%GBUF%SIG(NEL*(1-1)+ELEMID-NFT)=-PRES(ELEMID - NFT, 1)
                      ELBUF_TAB(NG)%GBUF%SIG(NEL*(2-1)+ELEMID-NFT)=-PRES(ELEMID - NFT, 1)
                      ELBUF_TAB(NG)%GBUF%SIG(NEL*(3-1)+ELEMID-NFT)=-PRES(ELEMID - NFT, 1)
                       IF(MLW==51)THEN
                         KPHASE = N0PHAS + (M51_SUBMAT_ID(IMAT) - 1) * NVPHAS
                         !ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(ELEMID - NFT + NEL * (2 + KPHASE - 1)) = -PRES(ELEMID - NFT, 1)
                         !ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(ELEMID - NFT + NEL * (3 + KPHASE - 1)) = -PRES(ELEMID - NFT, 1)
                         !ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(ELEMID - NFT + NEL * (4 + KPHASE - 1)) = -PRES(ELEMID - NFT, 1)                                            
                         ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(ELEMID - NFT + NEL * (18+ KPHASE - 1)) = PRES(ELEMID - NFT, 1)                                            
                       ENDIF
                     ENDIF
                                                                 
                  ENDIF
                                    
               ENDDO
               
C     Burn Fraction / Detonation Times
               IF(ELBUF_TAB(NG)%GBUF%G_TB > 0)ELBUF_TAB(NG)%GBUF%TB(ELEMID - NFT) = 1E20
               IF (MLW == 51) THEN                                                                                                        
                  KPHASE = N0PHAS - 1
                  ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(ELEMID - NFT + NEL * (KPHASE - 1)) = ONE                                           
                  IF(ELBUF_TAB(NG)%GBUF%G_BFRAC > 0)ELBUF_TAB(NG)%GBUF%BFRAC(ELEMID - NFT) = ONE    
                  KPHASE = N0PHAS + (4 - 1) * NVPHAS ! JWL is submat 4
                  ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(1 + NEL * (15 + KPHASE - 1)) = 1E20                                                 
               ELSE                                                                                                                       
                  DO IMAT=1,NBMAT                                                                                                         
                   IF(ELBUF_TAB(NG)%BUFLY(IMAT)%L_BFRAC > 0)ELBUF_TAB(NG)%BUFLY(IMAT)%LBUF(1,1,1)%BFRAC(ELEMID - NFT)=ONE   
                  ENDDO                                                                                                                   
                  IF(ELBUF_TAB(NG)%GBUF%G_BFRAC > 0)ELBUF_TAB(NG)%GBUF%BFRAC(ELEMID - NFT) = ONE                                           
               ENDIF  
                                                                                                                                   

               IF (.NOT. MULTI_FVM%IS_USED .AND. IALEFVM  ==  0) THEN
C     VEL
                  DO INODE = 1, NBNODE
                     IF (ITY  ==  1) THEN
                        NODEID = IXS(NODELIST(INODE), ELEMID)
                     ELSEIF (ITY  ==  2) THEN
                        NODEID = IXQ(NODELIST(INODE), ELEMID)
                     ELSEIF (ITY  ==  7) THEN
                        NODEID = IXTG(NODELIST(INODE), ELEMID)
                     ENDIF
                     IF (INIMAP1D(KK)%TAGNODE(NODEID)  ==  0) THEN

                        XNODE = XGRID(1, NODEID)
                        YNODE = XGRID(2, NODEID)
                        ZNODE = XGRID(3, NODEID)
                        IF (INIMAP1D(KK)%PROJ  ==  3) THEN
C     SPHERICAL PROJ
                           RADIUS = SQRT((XNODE - X0) * (XNODE - X0) + 
     .                          (YNODE - Y0) * (YNODE - Y0) + 
     .                          (ZNODE - Z0) * (ZNODE - Z0))
                        ELSE IF (INIMAP1D(KK)%PROJ  ==  1) THEN
C     PLANAR PROJ
                           RADIUS = (XNODE - X0) * INIMAP1D(KK)%NX + 
     .                          (YNODE - Y0) * INIMAP1D(KK)%NY + 
     .                          (ZNODE - Z0) * INIMAP1D(KK)%NZ
                        ELSE
C     CYLINDRICAL PROJ
                           TT = (XNODE - X0) * (X1 - X0) + 
     .                          (YNODE - Y0) * (Y1 - Y0) + 
     .                          (ZNODE - Z0) * (Z1 - Z0)
                           TT = TT / ((X1 - X0) * (X1 - X0) + 
     .                          (Y1 - Y0) * (Y1 - Y0) + 
     .                          (Z1 - Z0) * (Z1 - Z0))
                           XP = X0 + TT * (X1 - X0)
                           YP = Y0 + TT * (Y1 - Y0)
                           ZP = Z0 + TT * (Z1 - Z0)
                           RADIUS = SQRT((XNODE - XP) * (XNODE - XP) + 
     .                          (YNODE - YP) * (YNODE - YP) + 
     .                          (ZNODE - ZP) * (ZNODE - ZP))
                        ENDIF
                        IF (ABS(RADIUS)  <  EM10
     .                       .AND. INIMAP1D(KK)%PROJ  /=  1) THEN
                           VEL(1:3, NODEID) = ZERO
                        ELSE
                           IF (INIMAP1D(KK)%PROJ  ==  3) THEN
C     SPHERICAL
                              NX = (XNODE - X0) / RADIUS
                              NY = (YNODE - Y0) / RADIUS
                              NZ = (ZNODE - Z0) / RADIUS
                           ELSE IF (INIMAP1D(KK)%PROJ  ==  1) THEN
C     PLANAR
                              NX = INIMAP1D(KK)%NX
                              NY = INIMAP1D(KK)%NY
                              NZ = INIMAP1D(KK)%NZ
                           ELSE
C     CYLINDRICAL
                              NX = (XNODE - XP) / RADIUS
                              NY = (YNODE - YP) / RADIUS
                              NZ = (ZNODE - ZP) / RADIUS
                           ENDIF
                           IFUNC = INIMAP1D(KK)%FUNC_VEL
                           IF(IFUNC /= 0)THEN
                             IF(IFUNC > 0)THEN                                                                           
                               !--function_identifier                                                                    
                               NPT = (NPC(IFUNC + 1) - NPC(IFUNC)) / 2                                                   
                               FIRST = 1 + (NPC(IFUNC) - 1) / 2                                                          
                               PTRx => PLD_INV(1:NPTS/2,1)  !NPTS : 2*nb of all function points     NPT:for current function 
                               PTRy => PLD_INV(1:NPTS/2,2)                                                                   
                             ELSE                                                                                        
                               !--data from file                                                                         
                               NPT = INIMAP1D(KK)%NUM_NODE_VEL                                                         
                               FIRST = 1                                                                                 
                               PTRx => INIMAP1D(KK)%X_VEL(1:NPT)                                                             
                               PTRy => INIMAP1D(KK)%VEL(1:NPT)                                             
                             ENDIF         
                             SHIFT = 0
                             DO WHILE (PTRx(FIRST + SHIFT)  <  RADIUS ) 
                                SHIFT = SHIFT + 1
                                IF( SHIFT  >=  NPT)EXIT
                             ENDDO
                             IND = FIRST + SHIFT
                             IF (SHIFT  ==  0) THEN
                                VALUE = PTRy(FIRST)
                             ELSEIF (SHIFT  <  NPT) THEN
                                RAD1 = PTRx(IND - 1)
                                RAD2 = PTRx(IND)
                                VALUE1 = PTRy(IND - 1)
                                VALUE2 = PTRy(IND)
                                VALUE = VALUE1 + (VALUE2 - VALUE1) / (RAD2 - RAD1) * (RADIUS - RAD1)
                             ELSE
                                VALUE1 = PTRy(FIRST + NPT - 1)
                                VALUE = VALUE1
                             ENDIF
                             VALUE = VALUE * INIMAP1D(KK)%FAC_VEL
                           ELSE
                             VALUE = ZERO
                           ENDIF
                           VEL(1, NODEID) = VALUE * NX
                           VEL(2, NODEID) = VALUE * NY
                           VEL(3, NODEID) = VALUE * NZ
                        ENDIF
                        INIMAP1D(KK)%TAGNODE(NODEID) = 1
                     ENDIF
                  ENDDO         ! INODE
               ELSE
               
C     Initialize cell centred velocities
                  IFUNC = INIMAP1D(KK)%FUNC_VEL
                  IF(IFUNC /= 0)THEN                                                                            
                    IF(IFUNC > 0)THEN                                                                           
                      !--function_identifier                                                                    
                      NPT = (NPC(IFUNC + 1) - NPC(IFUNC)) / 2                                                   
                      FIRST = 1 + (NPC(IFUNC) - 1) / 2                                                          
                      PTRx => PLD_INV(1:NPTS/2,1)  !NPTS : 2*nb of all function points     NPT:for current function 
                      PTRy => PLD_INV(1:NPTS/2,2)                                                                   
                    ELSE                                                                                        
                      !--data from file                                                                         
                      NPT = INIMAP1D(KK)%NUM_NODE_VEL                                                         
                      FIRST = 1                                                                                 
                      PTRx => INIMAP1D(KK)%X_VEL(1:NPT)                                                             
                      PTRy => INIMAP1D(KK)%VEL(1:NPT)                                             
                    ENDIF 
                    SHIFT = 0                                                                                   
                    DO WHILE (PTRx(FIRST + SHIFT)  <  RADIUS ) 
                       SHIFT = SHIFT + 1
                       IF( SHIFT  >=  NPT)EXIT                       
                    ENDDO
                    IND = FIRST + SHIFT
                  
                    IF (SHIFT  ==  0) THEN
                       VALUE = PTRy(FIRST)
                    ELSEIF (SHIFT  <  NPT) THEN
                       RAD1 = PTRx(IND - 1)
                       RAD2 = PTRx(IND)
                       VALUE1 = PTRy(IND - 1)
                       VALUE2 = PTRy(IND)
                       VALUE = VALUE1 + (VALUE2 - VALUE1) / (RAD2 - RAD1) * (RADIUS - RAD1)
                    ELSE
                       VALUE1 = PTRy(FIRST + NPT - 1)
                       VALUE = VALUE1
                    ENDIF
                    NX = ZERO
                    NY = ZERO
                    NZ = ZERO
                    IF (INIMAP1D(KK)%PROJ  ==  3) THEN
C     SPHERICAL
                       IF (ABS(RADIUS)  >  EM10) THEN
                          NX = (XC - X0) / RADIUS
                          NY = (YC - Y0) / RADIUS
                          NZ = (ZC - Z0) / RADIUS
                       ENDIF
                    ELSE IF (INIMAP1D(KK)%PROJ  ==  1) THEN
C     PLANAR
                       NX = INIMAP1D(KK)%NX
                       NY = INIMAP1D(KK)%NY
                       NZ = INIMAP1D(KK)%NZ
                    ELSE
C     CYLINDRICAL
                       IF (ABS(RADIUS)  >  EM10) THEN
                          NX = (XC - XP) / RADIUS
                          NY = (YC - YP) / RADIUS
                          NZ = (ZC - ZP) / RADIUS
                       ENDIF
                    ENDIF
                    VALUE = VALUE * INIMAP1D(KK)%FAC_VEL
                  ELSE
                    VALUE = ZERO
                  ENDIF
                  IF (MULTI_FVM%IS_USED) THEN                                                
                     MULTI_FVM%VEL(1, ELEMID) = VALUE * NX                                   
                     MULTI_FVM%VEL(2, ELEMID) = VALUE * NY                                   
                     MULTI_FVM%VEL(3, ELEMID) = VALUE * NZ                                   
                  ELSE IF (IALEFVM  >  0) THEN                                              
                     GBUF%MOM(ELEMID - NFT + 0 * NEL) = VALUE * NX * GBUF%RHO(ELEMID - NFT)  
                     GBUF%MOM(ELEMID - NFT + 1 * NEL) = VALUE * NY * GBUF%RHO(ELEMID - NFT)  
                     GBUF%MOM(ELEMID - NFT + 2 * NEL) = VALUE * NZ * GBUF%RHO(ELEMID - NFT)  
                  ENDIF                                                                      
               ENDIF            ! MULTI_FVM
            ENDDO               ! II = IFIRST, ILAST
            
            IF (INIMAP1D(KK)%FORMULATION  ==  2) THEN
               DO II = IFIRST, ILAST
                  ELEMID = ELEM_LIST(II)
                  GBUF%EINT(ELEMID - NFT) = GBUF%EINT(ELEMID - NFT) * GBUF%RHO(ELEMID - NFT)
               ENDDO
               
               IF (MLW /= 51 .AND. MLW /= 151) THEN
                 SUBMAT(1)%TEMP(1:NEL) => ELBUF_TAB(NG)%GBUF%TEMP(1:NEL) 
               ELSE IF (MLW == 151) THEN
                 DO IMAT=1,NBMAT
                   SUBMAT(IMAT)%TEMP(1:NEL) => ELBUF_TAB(NG)%BUFLY(IMAT)%LBUF(1,1,1)%TEMP(1:NEL)               
                 ENDDO               
               ELSE
                 DO IMAT=1,NBMAT               
                   KPHASE = N0PHAS + (M51_SUBMAT_ID(IMAT) - 1) * NVPHAS
                   SUBMAT(IMAT)%TEMP(1:NEL) => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(1+NEL*(16+KPHASE-1) : NEL*(16+KPHASE )) 
                 ENDDO
               ENDIF     
               
            ELSE IF (INIMAP1D(KK)%FORMULATION  ==  1) THEN
C     Compute EINT from pressure
               IF (MLW /= 151 .AND. MLW /= 51) THEN
                  IF (ITY  ==  1) THEN
                     MATID = IXS(1, 1 + NFT)
                  ELSEIF (ITY  ==  2) THEN
                     MATID = IXQ(1, 1 + NFT)
                  ELSEIF (ITY  ==  7) THEN
                     MATID = IXTG(1, 1 + NFT)
                  ENDIF
                  SUBMAT(1)%TEMP(1:NEL) => ELBUF_TAB(NG)%GBUF%TEMP(1:NEL)
                  CALL SOLVE_EINT(MATID, NFT, NEL, PRES(:, 1), GBUF%EINT, GBUF%RHO, IFIRST, ILAST, ELEM_LIST, 
     .                 IPM, PM, BUFMAT, MLW,NG,0,SUBMAT(1)%TEMP,NPC,PLD)
               ELSE IF (MLW == 151) THEN
                  DO IMAT = 1, NBMAT
                     IF (ITY  ==  1) THEN
                        MATID = IPM(20 + IMAT, IXS(1, 1 + NFT))
                     ELSEIF (ITY  ==  2) THEN
                        MATID = IPM(20 + IMAT, IXQ(1, 1 + NFT))
                     ELSEIF (ITY  ==  7) THEN
                        MATID = IPM(20 + IMAT, IXTG(1, 1 + NFT))
                     ENDIF
                     SUBMAT(IMAT)%TEMP(1:NEL) => ELBUF_TAB(NG)%BUFLY(IMAT)%LBUF(1,1,1)%TEMP(1:NEL)
                     CALL SOLVE_EINT(MATID, NFT, NEL, PRES(:, IMAT), ELBUF_TAB(NG)%BUFLY(IMAT)%LBUF(1,1,1)%EINT, 
     .                    ELBUF_TAB(NG)%BUFLY(IMAT)%LBUF(1,1,1)%RHO, IFIRST, ILAST, ELEM_LIST, 
     .                    IPM, PM, BUFMAT, MLW, NG,IMAT,SUBMAT(IMAT)%TEMP,NPC,PLD)
                  ENDDO
               ELSE
                  DO IMAT = 1, NBMAT
                     IF (ITY  ==  1) THEN
                        MATID = IPM(20 + IMAT, IXS(1, 1 + NFT))
                     ELSEIF (ITY  ==  2) THEN
                        MATID = IPM(20 + IMAT, IXQ(1, 1 + NFT))
                     ELSEIF (ITY  ==  7) THEN
                        MATID = IPM(20 + IMAT, IXTG(1, 1 + NFT))
                     ENDIF
                     KPHASE = N0PHAS + (M51_SUBMAT_ID(IMAT) - 1) * NVPHAS
                     SUBMAT(IMAT)%TEMP(1:NEL) => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(1+NEL*(16+KPHASE-1) : NEL*(16+KPHASE )) 
                     CALL SOLVE_EINT(MATID, NFT, NEL, PRES(:, IMAT), EINT(:, IMAT), 
     .                    ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(1 + NEL * (9 + KPHASE - 1):NEL + NEL * (9 + KPHASE - 1)), 
     .                    IFIRST, ILAST, ELEM_LIST, 
     .                    IPM, PM, BUFMAT, MLW, NG,IMAT,SUBMAT(IMAT)%TEMP,NPC,PLD)
                     DO II = IFIRST, ILAST
                        ELEMID = ELEM_LIST(II)
                        RES = EINT(ELEMID - NFT, IMAT)
                        KPHASE = N0PHAS + (M51_SUBMAT_ID(IMAT) - 1) * NVPHAS
                        ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(ELEMID - NFT + NEL * (8 + KPHASE - 1)) = RES
                        ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(ELEMID - NFT + NEL * (21 + KPHASE - 1)) = RES
                     ENDDO
                  ENDDO  
               ENDIF
               
               IF(MLW == 151)THEN 
                 IF(NBMAT > 1)THEN
                   DO II = 1,NEL                                                                                
                     ELBUF_TAB(NG)%GBUF%TEMP(II) = ZERO                                                        
                       DO IMAT=1,NBMAT 
                         DENSFRAC = ELBUF_TAB(NG)%BUFLY(IMAT)%LBUF(1,1,1)%RHO(II)  / ELBUF_TAB(NG)%GBUF%RHO(II)                                                                                                                                                                                                
                         ELBUF_TAB(NG)%GBUF%TEMP(II) = ELBUF_TAB(NG)%GBUF%TEMP(II) + VFRAC(II,IMAT)*DENSFRAC * SUBMAT(IMAT)%TEMP(II)
                       ENDDO                                                                                      
                   ENDDO  
                 ENDIF                                                                                      
               ELSEIF(MLW==51)THEN
                 DO II = 1,NEL   
                   ELBUF_TAB(NG)%GBUF%TEMP(II) = ZERO  
                   DO IMAT=1,NBMAT 
                     KPHASE = N0PHAS + (M51_SUBMAT_ID(IMAT) - 1) * NVPHAS                                                                                                                  
                     DENSFRAC = ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(II + NEL * (12 + KPHASE - 1)) / ELBUF_TAB(NG)%GBUF%RHO(II)                                                                                                  
                     ELBUF_TAB(NG)%GBUF%TEMP(II) = ELBUF_TAB(NG)%GBUF%TEMP(II) + VFRAC(II,IMAT)*DENSFRAC * SUBMAT(IMAT)%TEMP(II)     
                   ENDDO 
                   ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(1:NEL)= ELBUF_TAB(NG)%GBUF%TEMP(1:NEL) !global temperature law51 buffer
                 ENDDO                                                                                        
               ENDIF
              
            ENDIF
         ENDDO                  ! NG = 1, NGROUP
         DEALLOCATE(PRES, EINT)
      ENDDO
C     ===================
C     Memory deallocation
C     ===================
      DEALLOCATE(ELEM_LIST)

      END SUBROUTINE INI_INIMAP1D

Chd|====================================================================
Chd|  SOLVE_EINT                    source/initial_conditions/inimap/ini_inimap1d.F
Chd|-- called by -----------
Chd|        INI_INIMAP1D                  source/initial_conditions/inimap/ini_inimap1d.F
Chd|-- calls ---------------
Chd|        EOSMAIN                       ../common_source/eos/eosmain.F
Chd|====================================================================
      SUBROUTINE SOLVE_EINT(MATID, NFT,    NEL, PRESIN, EINT, RHO,   IFIRST, ILAST, ELEM_LIST, IPM, 
     .                      PM   , BUFMAT, MLW, NG,     IMAT, THETA, NPC,    TF)
C-----------------------------------------------
C     I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc" 
C-----------------------------------------------
C     C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
!     MVSIZ
#include      "mvsiz_p.inc"
#include      "com04_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C     D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: MATID, NFT, NEL, IFIRST, ILAST, ELEM_LIST(*), IPM(NPROPMI, NUMMAT), MLW, NG
      my_real, INTENT(IN) :: PM(NPROPM, NUMMAT), BUFMAT(*), RHO(NEL), PRESIN(NEL)
      my_real, INTENT(OUT) :: EINT(NEL)
      my_real, INTENT(INOUT) :: THETA(NEL)
      INTEGER,INTENT(IN) :: IMAT !debug purpose
      INTEGER,INTENT(IN)::NPC(SNPC)
      my_real,INTENT(IN)::TF(STF)
C-----------------------------------------------
C     L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: II, ELEMID, LOCALID, REMAINING_ELTS, ITER, MAX_ITER
      INTEGER :: EOSTYPE, MAT(NEL)
      my_real :: VOL(NEL), ERROR(NEL), TOL, OFF(NEL), RHOZERO(NEL), RHO0
      my_real :: MU(NEL), DF(NEL), MU2(NEL), ESPE(NEL), DVOL(NEL), PSH(NEL),
     .     SSP(NEL), DPDE(NEL), ECOLD(NEL), PRES(NEL), POLD(NEL)
      my_real :: FUNC, DFUNC, INCR,SIG(NEL,6),MUOLD(NEL)
      LOGICAL :: CONVERGED(NEL)

C-----------------------------------------------
C     P r e C o n d i t i o n
C-----------------------------------------------
      IF(IFIRST>ILAST .OR. IFIRST<1)RETURN !DO NO INIT TO 0.
      
C-----------------------------------------------
C     S o u r c e   L i n e s
C-----------------------------------------------

      EOSTYPE = IPM(4, MATID)
      RHO0 = PM(1, MATID)

      VOL(1:NEL) = ZERO
      OFF(1:NEL) = ONE
      ERROR(1:NEL) = ZERO
      CONVERGED(1:NEL) = .TRUE.
      REMAINING_ELTS = 0
      RHOZERO(1:NEL) = RHO0
      MAT(1:NEL) = MATID
      PSH(1:NEL) = PM(88, MATID)
      SIG(1:NEL,1)=-PM(31,MATID)
      SIG(1:NEL,2)=-PM(31,MATID)
      SIG(1:NEL,3)=-PM(31,MATID)
      SIG(1:NEL,4)= ZERO
      SIG(1:NEL,5)= ZERO
      SIG(1:NEL,6)= ZERO
      MUOLD(1:NEL)   = ZERO
      THETA(1:NEL) = ZERO

      DO II = IFIRST, ILAST
         ELEMID = ELEM_LIST(II)
         LOCALID = ELEMID - NFT
         VOL(LOCALID) = ONE
         ERROR(LOCALID) = EP30
         CONVERGED(LOCALID) = .FALSE.
         REMAINING_ELTS = REMAINING_ELTS + 1
      ENDDO

      MAX_ITER = 30
      ITER = 0
      TOL = EM06
      POLD(1:NEL)=PRESIN(1:NEL)
      DO II=1,NEL
        SIG(II,1:3)=-POLD(II)
      ENDDO
      DO WHILE(REMAINING_ELTS  /=  0 .AND. ITER  <  MAX_ITER) 
         ITER = ITER + 1
         DO II = IFIRST, ILAST
            ELEMID = ELEM_LIST(II)
            LOCALID = ELEMID - NFT
            MU(LOCALID) = RHO(LOCALID) / RHOZERO(LOCALID) - ONE
            DF(LOCALID) = RHOZERO(LOCALID) / RHO(LOCALID)
         ENDDO
         
         CALL EOSMAIN(2, NEL, EOSTYPE, PM, OFF, EINT(1:NEL), 
     .        RHO(1:NEL), RHOZERO, MU, MU2, ESPE, 
     .        DVOL, DF, VOL, MAT, PSH, 
     .        PRES, SSP, DPDE, THETA, ECOLD, BUFMAT, SIG, MUOLD, MLW, POLD, NPC, TF)
         
         DO II = IFIRST, ILAST
            ELEMID = ELEM_LIST(II)
            LOCALID = ELEMID - NFT
            IF (.NOT. CONVERGED(LOCALID)) THEN
               FUNC  = PRES(LOCALID) - PRESIN(LOCALID)
               DFUNC = DPDE(LOCALID) / (ONE + MU(LOCALID))      !variable is EINT=rho.e, but EoS solves P(mu,E:=rho0.e) and returns dPdE.  variable change : d(rho.e)=(1+mu)d(rho0.e)=(1+mu)dE  => dP/d(rho.e) = dPdE/(1+mu)
               IF (DFUNC  ==  ZERO) THEN
                  REMAINING_ELTS = REMAINING_ELTS - 1
                  CONVERGED(LOCALID) = .TRUE.
                  VOL(LOCALID) = ZERO
               ELSE
                  INCR = - FUNC / DFUNC
                  ERROR(LOCALID) = ABS(INCR) / ABS(INCR + EINT(LOCALID))
                  EINT(LOCALID) = EINT(LOCALID) + INCR
                  IF (ERROR(LOCALID)  <  TOL) THEN
                     REMAINING_ELTS = REMAINING_ELTS - 1
                     CONVERGED(LOCALID) = .TRUE.
                     VOL(LOCALID) = ZERO
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
      ENDDO
      
      IF (REMAINING_ELTS  /=  0) THEN
         PRINT*, "*** Convergence issue in /INIMAP1D or /INIMAP2D option"
      ENDIF

      END SUBROUTINE SOLVE_EINT
