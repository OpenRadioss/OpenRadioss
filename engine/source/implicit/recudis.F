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
C   ----  nouvelle routine---------
Chd|====================================================================
Chd|  RECUDIS                       source/implicit/recudis.F     
Chd|-- called by -----------
Chd|        IMP_BUCK                      source/implicit/imp_buck.F    
Chd|        LIN_SOLV                      source/implicit/lin_solv.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE RECUDIS(NDDL  ,IDDL  ,NDOF  ,IKC   ,LX    ,
     1                   D     ,DR    ,INLOC )
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
      INTEGER NDDL,NDOF(*),IDDL(*),IKC(*),INLOC(*)
      my_real
     .  D(3,*),DR(3,*),LX(*)
C----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,NKC,ID,ND,M,N
C----No ddl actuelle -> IDDL(NUMNOD)+J(1-6)-NKC-----
      NKC=0
      DO N = 1,NUMNOD
       I=INLOC(N)
       DO J=1,NDOF(I)
        ND = IDDL(I)+J
        ID = ND-NKC
        IF (J<=3) THEN
         IF (IKC(ND)<1) THEN
          D(J,I)=LX(ID)
         ELSE
          NKC=NKC+1
         ENDIF
        ELSE
         K=J-3
         IF (IKC(ND)<1) THEN
          DR(K,I)=LX(ID)
         ELSE
          NKC=NKC+1
         ENDIF
        ENDIF
       ENDDO
      ENDDO
C
      RETURN
      END
C   ----  nouvelle routine---------
Chd|====================================================================
Chd|  RECUKIN                       source/implicit/recudis.F     
Chd|-- called by -----------
Chd|        IMP_BUCK                      source/implicit/imp_buck.F    
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|        RER02                         source/implicit/upd_glob_k.F  
Chd|-- calls ---------------
Chd|        BC_IMP2                       source/constraints/general/bcs/bc_imp0.F
Chd|        FVBC_IMPD                     source/constraints/general/impvel/fv_imp0.F
Chd|        FV_IMPD                       source/constraints/general/impvel/fv_imp0.F
Chd|        I2_IMPD                       source/interfaces/interf/i2_impd.F
Chd|        RBE2_IMPD                     source/constraints/general/rbe2/rbe2v.F
Chd|        RBE3_IMPD                     source/constraints/general/rbe3/rbe3v.F
Chd|        RBY_IMPD                      source/constraints/general/rbody/rby_impd.F
Chd|        RM_IMP2                       source/model/remesh/rm_imp0.F 
Chd|        RWL_IMPD                      source/constraints/general/rwall/srw_imp.F
Chd|        IMP_FVBCL                     share/modules/impbufdef_mod.F 
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE RECUKIN(RBY   ,LPBY  ,NPBY  ,SKEW  ,ISKEW ,
     1                   ITAB  ,WEIGHT,MS    ,IN    ,SENSOR,
     2                   IBFV  ,VEL   ,LSENSOR,ICODT,ICODR ,
     3                   NRBYAC,IRBYAC,NINT2 ,IINT2 ,IPARI ,
     4                   INTBUF_TAB   ,NDOF  ,D     ,DR    ,
     5                   X     ,XFRAME,LJ    ,IXR   ,IXC   ,
     6                   IXTG  ,SH4TREE,SH3TREE,IRBE3 ,LRBE3,
     7                   FRBE3 ,IRBE2 ,LRBE2 )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_FVBCL
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "com09_c.inc"
#include      "param_c.inc"
#include      "remesh_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBFV(NIFV,*),LSENSOR(NSENSI,*),LJ(*)
      INTEGER WEIGHT(*),LPBY(*),NPBY(NNPBY,*),ITAB(*),
     .        IPARI(NPARI,*), ISKEW(*),
     .        NRBYAC,IRBYAC(*),NINT2 ,IINT2(*),IXR(*)
      INTEGER NDOF(*),ICODT(*) ,ICODR(*),IXC(*),IXTG(*),
     .         SH4TREE(*), SH3TREE(*),IRBE3(NRBE3L,*),LRBE3(*),
     .         IRBE2(*),LRBE2(*)
      my_real
     .   RBY(NRBY,*) ,SKEW(*),IN(*),MS(*),SENSOR(NSENSR,*),
     .   VEL(LFXVELR,*), XFRAME(NXFRAME,*),FRBE3(*)
      my_real
     .  X(3,*),D(3,*),DR(3,*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
C-------pendant iteration dU_d=0------------
C-------local FV-BCS coupling might modify global dirs due to unconsisting
      IF(NFVBCL > 0 ) THEN
       CALL FVBC_IMPD(IBFV   ,SKEW  ,XFRAME ,LJ   ,NDOF  ,
     1                D      ,DR   ,ICODT  ,ICODR ,ISKEW ,
     2                ICT_1  ,ICR_1)
      END IF
      IF(NFXVEL>0)THEN
       CALL FV_IMPD(IBFV  ,LJ    ,SKEW  ,XFRAME ,D     ,
     1              DR    )
      ENDIF
      CALL RWL_IMPD(D)
      IF(NFVBCL > 0 ) THEN
       CALL BC_IMP2(ICT_1 ,ICR_1,ISKEW ,SKEW  ,NDOF   ,
     1                D     ,DR    )
      ELSE
       CALL BC_IMP2(ICODT ,ICODR ,ISKEW ,SKEW  ,NDOF   ,
     1             D     ,DR    )
      END IF
      IF(NADMESH/=0)THEN
            CALL RM_IMP2(IXC   ,IXTG  ,D    ,DR    ,SH4TREE,SH3TREE)
      ENDIF
      IF(NRBYAC>0)THEN
       CALL RBY_IMPD(NRBYAC,IRBYAC,X    ,RBY,LPBY,NPBY,SKEW,
     1              ISKEW,ITAB,WEIGHT,MS ,IN  ,SENSOR,
     2              NDOF  ,D    ,DR   ,IXR  )
      ENDIF
      IF(NRBE3>0)THEN
       CALL RBE3_IMPD(IRBE3 ,LRBE3 ,X    ,D     ,DR    ,
     1                FRBE3  ,SKEW  )
      ENDIF
      IF(NRBE2>0)THEN
       CALL RBE2_IMPD(IRBE2 ,LRBE2 ,X    ,D     ,DR    ,
     1                SKEW  )
      ENDIF
       CALL I2_IMPD(NINT2 ,IINT2,IPARI,INTBUF_TAB   ,
     1              X     ,MS   ,IN   ,WEIGHT,NDOF  ,
     2              D     ,DR   )
C
      RETURN
      END
Chd|====================================================================
Chd|  IDDL2NOD                      source/implicit/recudis.F     
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE IDDL2NOD(NDDL  ,IDDL  ,NDOF  ,IKC   ,INLOC ,
     1                    IID   ,NN    )
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
      INTEGER NDDL,NDOF(*),IDDL(*),IKC(*),INLOC(*),IID ,NN
C----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,NKC,ID,ND,N
C----No ddl actuelle -> IDDL(NUMNOD)+J(1-6)-NKC-----
      NN=0
      NKC=0
      DO N = 1,NUMNOD
       I=INLOC(N)
       DO J=1,NDOF(I)
        ND = IDDL(I)+J
        ID = ND-NKC
        IF (IKC(ND)<1) THEN
         IF (ID==IID) THEN
          NN=I
          RETURN
         ENDIF
        ELSE
         NKC=NKC+1
        ENDIF
       ENDDO
      ENDDO
C
      RETURN
      END
C   ----  nouvelle routine---------
Chd|====================================================================
Chd|  WEIGHTDDL                     source/implicit/recudis.F     
Chd|-- called by -----------
Chd|        IMP_CHKM                      source/implicit/imp_solv.F    
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE WEIGHTDDL(IDDL  ,NDOF  ,IKC   ,WEIGHT,W_IMP ,
     1                     INLOC )
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
      INTEGER NDOF(*),IDDL(*),IKC(*),INLOC(*),WEIGHT(*),W_IMP(*)
C----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,NKC,ID,ND,M,N
C----No ddl actuelle -> IDDL(NUMNOD)+J(1-6)-NKC-----
      NKC=0
      DO N = 1,NUMNOD
       I=INLOC(N)
       DO J=1,NDOF(I)
        ND = IDDL(I)+J
        ID = ND-NKC
        IF (IKC(ND)<1) THEN
         W_IMP(ID)=WEIGHT(I)
        ELSE
         NKC=NKC+1
        ENDIF
       ENDDO
      ENDDO
C
      RETURN
      END
