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
Chd|  CJOINT                        source/constraints/general/cyl_joint/cjoint.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        TELESC                        source/constraints/general/cyl_joint/telesc.F
Chd|        JOINT_MOD                     share/modules/joint_mod.F     
Chd|====================================================================
        SUBROUTINE CJOINT(A    ,AR    ,V ,VR,X    ,
     2                  FSAV ,LJOINT,MS,IN,IADCJ,
     3                  FR_CJ,TAG_LNK_SMS,ITASK)
        USE JOINT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER LJOINT(*), FR_CJ(*), IADCJ(NSPMD+1,*), TAG_LNK_SMS(*),
     .        ITASK
        my_real
     .   A(3,NUMNOD), AR(3,NUMNOD), V(3,NUMNOD), VR(3,NUMNOD), X(3,NUMNOD), FSAV(NTHVKI,*),
     .   MS(*), IN(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "sms_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER K, N, NN, KIND(NJOINT), ICSIZE
        LOGICAL :: CONDITION
C-----------------------------------------------
C
        K=1
        DO N=1,NJOINT
            KIND(N) = K
            K=K+LJOINT(K)+1
        END DO

        IF(IDTMINS==0.AND.IDTMINS_INT==0)THEN
            DO N=1,NJOINT
                NN=NINTER+NRWALL+NRBODY+NSECT+N
                K = KIND(N)
                CONDITION = ( (CYL_JOIN(N)%NUMBER_NODE>0).OR.(CYL_JOIN(N)%NUMBER_MAIN_NODE>0) )
                IF(CONDITION) CALL TELESC(N,A,AR,V,VR,X,FSAV(1,NN),MS,IN,ITASK)
            END DO
        ELSE
            DO N=1,NJOINT
                IF(TAG_LNK_SMS(N)/=0)CYCLE
                NN=NINTER+NRWALL+NRBODY+NSECT+N
                K = KIND(N)
                CONDITION = ( (CYL_JOIN(N)%NUMBER_NODE>0).OR.(CYL_JOIN(N)%NUMBER_MAIN_NODE>0) )
                IF(CONDITION) CALL TELESC(N,A,AR,V,VR,X,FSAV(1,NN),MS,IN,ITASK)
            END DO
        END IF

        RETURN
        END SUBROUTINE CJOINT
