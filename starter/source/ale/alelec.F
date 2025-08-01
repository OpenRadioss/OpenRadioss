Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    alelec                 ../starter/source/ale/alelec.F
!||--- called by ------------------------------------------------------
!||    lectur                 ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||    ale_check_lag          ../starter/source/ale/ale_check_lag.F
!||    aleso2                 ../starter/source/ale/ale2d/aleso2.F
!||    aleso2t                ../starter/source/ale/ale2d/aleso2t.F
!||    aleso3                 ../starter/source/ale/ale3d/aleso3.F
!||    alesop                 ../starter/source/ale/alesop.F
!||--- uses       -----------------------------------------------------
!||    message_mod            ../starter/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE ALELEC(
     1      ICODE    ,IXS     , IXQ      , IXC             ,IXT    ,
     2      IXTG     ,PM      , IGEO     , ITAB            ,GEO    ,
     3      NALE_R2R ,FLAG_R2R, MULTI_FVM, ALE_CONNECTIVITY,ITHERM , ISHADOW)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine is building ALE connectivities
C    'NN' stands for Node/Node
C    'NE' stands for Node/Elem
C    'EE' stands for Elem/Elem
C  In addition to that the subroutine is also marking ALE nodes 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE MULTI_FVM_MOD
      USE ALE_CONNECTIVITY_MOD
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: ITHERM
      LOGICAL,INTENT(IN) :: ISHADOW !< shadowing option for detonators (Eikonal equation solver)
      INTEGER,INTENT(IN) :: ICODE(*),IXS(3,NUMNOD),IXQ(NIXQ,NUMELQ),IXC(NIXC,NUMELC),IXT(NIXT,NUMELT),IXTG(NIXTG,NUMELTG)
      INTEGER,INTENT(IN) ::  ITAB(NUMNOD), NALE_R2R(*),FLAG_R2R,IGEO(NPROPGI,NUMGEO)
      my_real,INTENT(IN) ::  PM(NPROPM,NUMMAT), GEO(NPROPG,NUMGEO)
      TYPE(MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      CALL ALE_CONNECTIVITY%ALE_COMPUTE_EE_CONNECTIVITY(
     .     PM     , IGEO,
     .     NPROPGI, NUMGEO, NPROPM, NUMMAT , NUMNOD , NUMELQ, NUMELTG, NUMELS, N2D, 
     .     IALE   , IEULER, ITHERM, IALELAG, ISHADOW,
     .     NIXQ   , NIXTG , NIXS  , 
     .     IXQ    , IXTG  , IXS   )
      
      IF (ALE_CONNECTIVITY%has_ne_connect .OR. ALE_CONNECTIVITY%has_nn_connect) THEN
         CALL ALE_CONNECTIVITY%ALE_COMPUTE_CONNECTIVITY(
     .        NUMNOD, NUMELQ, NUMELTG, NUMELS, 
     .        NIXQ  , NIXTG , NIXS   ,
     .        IXQ   , IXTG  , IXS)
      ENDIF
      
      CALL ALE_CHECK_LAG(ALE_CONNECTIVITY%NALE,IXS,IXQ,IXC,IXT,IXTG,PM,ITAB,NALE_R2R,FLAG_R2R,IGEO)   
      
      IF (ALE_CONNECTIVITY%has_nn_connect) THEN 
         CALL ALESOP(ALE_CONNECTIVITY%NALE, ALE_CONNECTIVITY%NN_CONNECT, ICODE, ITAB)        
      ENDIF


      !ALE NODES ARE HERE MARKED
      IF (N2D == 0)THEN
         CALL ALESO3(ALE_CONNECTIVITY,PM,IXS,GEO,IGEO)
      ELSEIF( .NOT. MULTI_FVM%IS_USED .OR. NUMELQ /= 0) THEN
         CALL ALESO2(ALE_CONNECTIVITY,PM,IXQ,IGEO)   
      ELSE
         CALL ALESO2T(ALE_CONNECTIVITY, PM, IXTG,IGEO)
      ENDIF

      !SPECIFIC MARKER FOR COLOCATED SCHEME (LAW51)
      IF(MULTI_FVM%IS_USED)THEN
        DO I=1,NUMNOD
          IF(    ALE_CONNECTIVITY%NALE(I)==151)THEN
           ALE_CONNECTIVITY%NALE(I)=-1
          ELSEIF(ALE_CONNECTIVITY%NALE(I)==152)THEN
           ALE_CONNECTIVITY%NALE(I)=-2
          ENDIF
        ENDDO
      ENDIF
C-----------------------------------------------
      RETURN
      END
